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

type network = (string, element) Hashtbl.t

exception Duplicate_ID
exception Element_not_found of string
exception Malformed_connection

let add_element net e =
let add_helper net id e = begin
    if not (Hashtbl.mem net id) then
        Hashtbl.add net id e
    else
        raise Duplicate_ID
end in match e with
    | STE(id,set,strt,latch,connect,report) -> add_helper net id e
    | Counter(id,target,behavior,report,connect) -> add_helper net id e
    | Combinatorial(id,eod,report,connect) -> add_helper net id e

let connect net e1_id e2_id ?terminal =
let e1 = begin try Hashtbl.find net e1_id
    with Not_found -> raise (Element_not_found e1_id)
    end in
let e2 = begin try Hashtbl.find net e2_id
    with Not_found -> raise (Element_not_found e2_id)
    end in
let e2_conn : element_connections = begin match terminal with 
    | None -> (e2,None)
    | Some x -> (e2,x)
    end in
    match e1 with
        | STE(id,set,strt,latch,connect,report) ->
            Hashtbl.replace net e1_id (STE(id,set,strt,latch,e2_conn::connect,report))
        | Counter(id,target,behavior,report,connect) ->
            Hashtbl.replace net e1_id (Counter(id,target,behavior,report,e2_conn::connect))
        | Combinatorial(id,eod,report,connect) ->
            Hashtbl.replace net e1_id (Combinatorial(id,eod,report,e2_conn::connect))

let set_report net e_id r =
let e = begin try Hashtbl.find net e_id
    with Not_found -> raise (Element_not_found e_id)
    end in
    match e with
        | STE(id,set,strt,latch,connect,report) ->
            Hashtbl.replace net e_id (STE(id,set,strt,latch,connect,r))
        | Counter(id,target,behavior,report,connect) ->
            Hashtbl.replace net e_id (Counter(id,target,behavior,r,connect))
        | Combinatorial(id,eod,report,connect) ->
            Hashtbl.replace net e_id (Combinatorial(id,eod,r,connect))

let element_to_str e =
    let start_to_str strt =
        match strt with
            | NotStart -> "none"
            | Start -> "start-of-data"
            | AllStart -> "all-input" 
    in let connection_id_to_str ((e,c) : element_connections) =
        let term = match c with
            | None -> raise Malformed_connection
            | Some x -> x in
        match e with (*TODO Add lots of error-checking here*)
            | STE(id,set,strt,latch,connect,report) -> id
            | Counter(id,target,behavior,report,connect) -> id ^ ":" ^ term
            | Combinatorial(id,eod,report,connect) -> id ^ ":" ^ term
        
    in match e with
        | STE(id,set,strt,latch,connect,report) -> begin
            let rep_line =
                if report then "<report-on-match/>\n" else "" in
            Printf.sprintf "<state-transition-element id=\"%s\" symbol-set=\"%s\" start=\"%s\" latch=\"%B\">\n" id set (start_to_str strt) latch ^
            rep_line ^
            List.fold_left (fun prev b -> prev ^ Printf.sprintf "<activate-on-match element = \"%s\" />\n" (connection_id_to_str b) ) "" connect ^
            "</state-transition-element>\n"
            end
        | Counter(id,target,behavior,report,connect) -> ""
        | Combinatorial(id,eod,report,connect) -> ""

let network_to_str net =
    let internal = ref "" in
    Hashtbl.iter (fun k e -> internal := !internal ^ element_to_str e) net ;
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ^
    "<automata-network name=\"Bob\" id=\"Bob\">\n<description></description>\n" ^ (*TODO figure out how to get an actual name here*)
    !internal ^
    "</automata-network>"
(*
match e with
    | STE(id,set,strt,latch,connect,report) ->
    | Counter(id,target,behavior,report,connect) ->
    | Combinatorial(id,eod,report,connect) ->
*)