(*
 * Kevin Angstadt
 * Represents a Micron Automaton
 *)
 
type start =
    | NotStart
    | Start
    | AllStart

type at_target =
    | Pulse
    | Latch
    | Roll

type combinatorial =
    | Inverter
    | OR
    | AND
    | NAND
    | NOR
    | SOP
    | POS
    | NSOP
    | NPOS

type element =
    | STE of string * string * start * bool * element_connections list * bool
        (* id, symbol set, start, latch, activate on match, report on match *)
    | Counter of string * int * at_target * bool * element_connections list
        (* id, target, at_target, report on target, activate on target *)
    | Combinatorial of string * bool * bool * element_connections list
        (* id, eod, report on high, activate on high *)
and element_connections = element * string option
    
type network = {
    states : (string, element) Hashtbl.t;
    start : (string * element) list;
    report : (string * element) list;
    last : element option (*TODO will probably have to be some sort of list*)
}

type macro = {
    id : string;
    head : element option;
    tail : element option;
    states : (string, element) Hashtbl.t;
    input_ids : string list;
    output_ids : string list;
    input : (string * element_connections) list;
    output : (string * element) list;
}

exception Duplicate_ID
exception Element_not_found of string
exception Malformed_connection

let create () = ref {
    states = Hashtbl.create 255;
    start = [];
    report = [];
    last = None;
}

let make_ste id set strt latch connect report =
    STE(id,set,strt,latch,connect,report)
    
let get_id e =
    match e with
        | STE(id,_,_,_,_,_)
        | Counter(id,_,_,_,_)
        | Combinatorial(id,_,_,_) -> id

let get_connections e =
    match e with
        | STE(_,_,_,_,connect,_)
        | Counter(_,_,_,_,connect)
        | Combinatorial(_,_,_,connect) -> connect

let contains (net:network ref) e =
    let id = get_id e in
    Hashtbl.mem (!net).states id

let add_element net e =
let add_helper (net: network ref) id e start report = begin
let start = match start with
    | NotStart -> false
    | Start
    | AllStart -> true
    in
    if not (Hashtbl.mem (!net).states id) then begin
        Hashtbl.add (!net).states id e ;
        if start then net := {!net with start = (id,e) :: (!net).start; } ;
        if report then net := {!net with report = (id,e) :: (!net).report; }
        end
    else
        raise Duplicate_ID
end in match e with
    | STE(id,set,strt,latch,connect,report) -> add_helper net id e strt report
    | Counter(id,target,behavior,report,connect) -> add_helper net id e NotStart report
    | Combinatorial(id,eod,report,connect) -> add_helper net id e NotStart report

let connect (net:network ref) e1_id e2_id (terminal : string option) =
let e1 = begin try Hashtbl.find (!net).states e1_id
    with Not_found -> raise (Element_not_found e1_id)
    end in
let e2 = begin try Hashtbl.find (!net).states e2_id
    with Not_found -> raise (Element_not_found e2_id)
    end in
let e2_conn : element_connections = begin match terminal with 
    | None -> (e2,None)
    | Some x -> (e2,Some x)
    end in
    match e1 with
        | STE(id,set,strt,latch,connect,report) ->
            Hashtbl.replace (!net).states e1_id (STE(id,set,strt,latch,e2_conn::connect,report))
        | Counter(id,target,behavior,report,connect) ->
            Hashtbl.replace (!net).states e1_id (Counter(id,target,behavior,report,e2_conn::connect))
        | Combinatorial(id,eod,report,connect) ->
            Hashtbl.replace (!net).states e1_id (Combinatorial(id,eod,report,e2_conn::connect))

let set_report (net:network ref) e_id r =
let e = begin try Hashtbl.find (!net).states e_id
    with Not_found -> raise (Element_not_found e_id)
    end in begin
    match e with 
        | STE(id,set,strt,latch,connect,report) ->
            Hashtbl.replace (!net).states e_id (STE(id,set,strt,latch,connect,r))
        | Counter(id,target,behavior,report,connect) ->
            Hashtbl.replace (!net).states e_id (Counter(id,target,behavior,r,connect))
        | Combinatorial(id,eod,report,connect) ->
            Hashtbl.replace (!net).states e_id (Combinatorial(id,eod,r,connect))
    end ;
    net := {!net with report = (List.filter (fun (a,b) -> a <> e_id) (!net).report)}
    
(*Operations on Macros*)
let m_create id input output = {
    id = id;
    head = None;
    tail = None;
    states = Hashtbl.create 255;
    input_ids = input;
    output_ids = output;
    input = [];
    output = [];
}

let m_add_element macro e =
    match e with
        | STE(id,_,_,_,_,_)
        | Counter(id,_,_,_,_)
        | Combinatorial(id,_,_,_) -> begin
            if not (Hashtbl.mem macro.states id) then Hashtbl.add macro.states id e
            else
                raise Duplicate_ID
        end

let m_connect macro e1_id e2_id ?terminal : macro =
if (Hashtbl.mem macro.states e1_id) then
    let e1 = Hashtbl.find macro.states e1_id in    
    if (Hashtbl.mem macro.states e2_id) then begin
        let e2 = Hashtbl.find macro.states e2_id in
        let e2_conn : element_connections = begin match terminal with
            | None -> (e2,None)
            | Some x -> (e2,x)
            end in begin
            match e1 with
                | STE(id,set,strt,latch,connect,report) ->
                    Hashtbl.replace macro.states e1_id (STE(id,set,strt,latch,e2_conn::connect,report))
                | Counter(id,target,behavior,report,connect) ->
                    Hashtbl.replace macro.states e1_id (Counter(id,target,behavior,report,e2_conn::connect))
                | Combinatorial(id,eod,report,connect) ->
                    Hashtbl.replace macro.states e1_id (Combinatorial(id,eod,report,e2_conn::connect))
            end ; macro
    end
    else
        (*try e2 as an end terminal*)
        begin try
            let term = List.find (fun a -> e2_id = a) macro.output_ids in
                {macro with output = (e2_id,e1) :: macro.output}
            
            with Not_found -> raise (Element_not_found e1_id)
        end
else
    (*Try e1 as an input terminal*)
    begin try
        let term = List.find (fun a -> e1_id = a) macro.input_ids in
        let e2 = begin try Hashtbl.find macro.states e2_id
            with Not_found -> raise (Element_not_found e2_id)
            end in
            let e2_conn = match terminal with
                | None -> (e2,None)
                | Some x -> (e2,x) in
            {macro with input = (e1_id,e2_conn) :: macro.input}
        with Not_found -> raise (Element_not_found e1_id)
    end
    
let m_set_report macro e_id r =
let e = begin try Hashtbl.find macro.states e_id
    with Not_found -> raise (Element_not_found e_id)
    end in begin
    match e with 
        | STE(id,set,strt,latch,connect,report) ->
            Hashtbl.replace macro.states e_id (STE(id,set,strt,latch,connect,r))
        | Counter(id,target,behavior,report,connect) ->
            Hashtbl.replace macro.states e_id (Counter(id,target,behavior,r,connect))
        | Combinatorial(id,eod,report,connect) ->
            Hashtbl.replace macro.states e_id (Combinatorial(id,eod,r,connect))
    end ;
    macro

(*Output Functions*)  
let element_to_str e =
    let start_to_str strt =
        match strt with
            | NotStart -> "none"
            | Start -> "start-of-data"
            | AllStart -> "all-input" 
    in let connection_id_to_str ((e,c) : element_connections) =
        let term = match c with
            | None -> begin
                match e with
                    | STE(_,_,_,_,_,_) -> ""
                    | _ -> raise Malformed_connection
                end 
            | Some x -> x in
        match e with (*TODO Add lots of error-checking here*)
            | STE(id,set,strt,latch,connect,report) -> id
            | Counter(id,target,behavior,report,connect) -> id ^ ":" ^ term
            | Combinatorial(id,eod,report,connect) -> id ^ ":" ^ term
        
    in match e with
        | STE(id,set,strt,latch,connect,report) -> begin
            let rep_line =
                if report then "<report-on-match/>\n" else "" in
            let latch_str = if latch then "latch=\"true\"" else "" in
            Printf.sprintf "<state-transition-element id=\"%s\" symbol-set=\"%s\" start=\"%s\" %s>\n" id set (start_to_str strt) latch_str ^
            rep_line ^
            List.fold_left (fun prev b -> prev ^ Printf.sprintf "<activate-on-match element = \"%s\" />\n" (connection_id_to_str b) ) "" connect ^
            "</state-transition-element>\n"
            end
        | Counter(id,target,behavior,report,connect) -> ""
        | Combinatorial(id,eod,report,connect) -> ""

let network_to_str (net:network ref) =
    let internal = ref "" in
    Hashtbl.iter (fun k e -> internal := !internal ^ element_to_str e) (!net).states ;
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ^
    "<automata-network name=\"Bob\" id=\"Bob\">\n<description></description>\n" ^ (*TODO figure out how to get an actual name here*)
    !internal ^
    "</automata-network>"
    
let rec print_rec (e:element) =
    let _ = print_endline (element_to_str e) in
    match e with
        | STE(_,_,_,_,connect,_)
        | Counter(_,_,_,_,connect)
        | Combinatorial(_,_,_,connect) -> List.iter (fun (a,c) -> print_rec a) connect
(*
match e with
    | STE(id,set,strt,latch,connect,report) ->
    | Counter(id,target,behavior,report,connect) ->
    | Combinatorial(id,eod,report,connect) ->
*)