(*
 * Represents a Micron Automaton
 * i.e. a homogeneous automaton that also has boolean elements and counters
 *)
 
let all_in = '*'

let start_of_input = '\255'

(*TODO this only supports checking all counters at the same time*)
let counter_trigger_char = '\254'

let start_in = start_of_input
 
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

type 'a ast =
      AST of int * (string, 'a) Hashtbl.t
    | PortAST of int * string * (string, 'a) Hashtbl.t

type 'a element =
    | STE of string * string * bool * start * bool * 'a connections * bool * 'a ast list
        (* id, symbol set, negate, start, latch, activate on match, report on match, ast id *)
    | Counter of string * int * at_target * bool * 'a connections * 'a ast list
        (* id, target, at_target, report on target, activate on target, ast id *)
    | Combinatorial of combinatorial * string * bool * bool * 'a connections * 'a ast list
        (* type, id, eod, report on high, activate on high, ast id *)
and 'a element_connection = 'a element * string option
and 'a connections = {
    mutable children : 'a element_connection list;
    mutable parents : string list;
}
    
type 'a network = {
    states : (string, 'a element) Hashtbl.t;
    start : (string * 'a element) list;
    report : (string * 'a element) list;
    last : 'a element option; (*TODO will probably have to be some sort of list*)
    id : string;
    description : string;
}

(*type macro = {
    id : string;
    head : element option;
    tail : element option;
    states : (string, element) Hashtbl.t;
    input_ids : string list;
    output_ids : string list;
    input : (string * element_connection) list;
    output : (string * element) list;
}*)

exception Duplicate_ID
exception Element_not_found of string
exception Malformed_connection
exception Malformed_start

let create name desc = ref {
    states = Hashtbl.create 1000000;
    start = [];
    report = [];
    last = None;
    id = name;
    description = desc;
}

let clone (net:'a network ref) = ref {!net with states = Hashtbl.copy (!net).states }

let set_name (net:'a network ref) name = net := {!net with id=name;}

let make_ste id set neg strt latch connect report ast_id_list=
    STE(id,set,neg,strt,latch,connect,report, ast_id_list)
    
let get_id e =
    match e with
        | STE(id,_,_,_,_,_,_,_)
        | Counter(id,_,_,_,_,_)
        | Combinatorial(_,id,_,_,_,_) -> id

let get_connections e =
    match e with
        | STE(_,_,_,_,_,connect,_,_)
        | Counter(_,_,_,_,connect,_)
        | Combinatorial(_,_,_,_,connect,_) -> connect

let generate_connections conn_list =
    {
        children = conn_list;
        parents = [];
    }

let contains (net:'a network ref) e =
    let id = get_id e in
    Hashtbl.mem (!net).states id

let get_element (net: 'a network ref) e_id =
    try Hashtbl.find (!net).states e_id
    with Not_found -> raise (Element_not_found e_id)

let add_element net e =
let add_helper (net: 'a network ref) id e start report = begin
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
    | STE(id,set,neg,strt,latch,connect,report,ast_id) -> add_helper net id e strt report
    | Counter(id,target,behavior,report,connect,ast_id) -> add_helper net id e NotStart report
    | Combinatorial(_,id,eod,report,connect,ast_id) -> add_helper net id e NotStart report

let remove_element (net:'a network ref) e_id =
    Hashtbl.remove (!net).states e_id 

let connect (net:'a network ref) e1_id e2_id (terminal : string option) =
let e1 = begin try Hashtbl.find (!net).states e1_id
    with Not_found -> raise (Element_not_found e1_id)
    end in
let e2 = begin try Hashtbl.find (!net).states e2_id
    with Not_found -> raise (Element_not_found e2_id)
    end in
let e2_conn : 'a element_connection = begin match terminal with 
    | None -> (e2,None)
    | Some x -> (e2,Some x)
    end in
    match e1 with
        | STE(id,set,neg,strt,latch,connect,report,ast_id) ->
            if not (List.mem e2_conn connect.children) then
                Hashtbl.replace (!net).states e1_id (STE(id,set,neg,strt,latch,{connect with children = e2_conn::connect.children},report,ast_id))
        | Counter(id,target,behavior,report,connect,ast_id) ->
            if not (List.mem e2_conn connect.children) then
                Hashtbl.replace (!net).states e1_id (Counter(id,target,behavior,report,{connect with children = e2_conn::connect.children},ast_id))
        | Combinatorial(typ,id,eod,report,connect,ast_id) ->
            if not (List.mem e2_conn connect.children) then
                Hashtbl.replace (!net).states e1_id (Combinatorial(typ,id,eod,report,{connect with children = e2_conn::connect.children},ast_id))

let set_count (net:'a network ref) e_id cnt =
let e = begin try Hashtbl.find (!net).states e_id
    with Not_found -> raise (Element_not_found e_id)
    end in begin
    match e with
        | Counter(id,target,behavior,report,connect,ast_id) ->
            Hashtbl.replace (!net).states e_id (Counter(id,cnt,behavior,report,connect,ast_id))
        | _ -> raise (Element_not_found e_id)
    end

let set_latch (net:'a network ref) e_id latch =
let e = begin try Hashtbl.find (!net).states e_id
    with Not_found -> raise (Element_not_found e_id)
    end in begin
    match e with
        | Counter(id,target,behavior,report,connect,ast_id) ->
            Hashtbl.replace (!net).states e_id (Counter(id,target,latch,report,connect,ast_id))
        | _ -> raise (Element_not_found e_id)
    end

let is_start_empty (net:'a network ref) =
    (!net).start = []

let set_start (net:'a network ref) e_id strt =
let e = begin try Hashtbl.find (!net).states e_id
    with Not_found -> raise (Element_not_found e_id)
    end in begin
    match e with
        | STE(id,set,neg,old_strt,latch,connect,report,ast_id) ->
            Hashtbl.replace (!net).states e_id (STE(id,set,neg,strt,latch,connect,report,ast_id))
        | _ -> raise Malformed_start
    end ;
    net := {!net with start = (List.filter (fun (a,b) ->
        match strt with
            | NotStart -> a <> e_id
            | _ -> true
        ) ((e_id,e)::(!net).start))}

let set_report (net:'a network ref) e_id r =
let e = begin try Hashtbl.find (!net).states e_id
    with Not_found -> raise (Element_not_found e_id)
    end in begin
    match e with 
        | STE(id,set,neg,strt,latch,connect,report,ast_id) ->
            Hashtbl.replace (!net).states e_id (STE(id,set,neg,strt,latch,connect,r,ast_id))
        | Counter(id,target,behavior,report,connect,ast_id) ->
            Hashtbl.replace (!net).states e_id (Counter(id,target,behavior,r,connect,ast_id))
        | Combinatorial(typ,id,eod,report,connect,ast_id) ->
            Hashtbl.replace (!net).states e_id (Combinatorial(typ,id,eod,r,connect,ast_id))
    end ;
    net := {!net with report = (List.filter (fun (a,b) -> a <> e_id) ((e_id,e)::(!net).report))}

let add_ast (net:'a network ref) e_id ast_info =
    let e = begin try Hashtbl.find (!net).states e_id
    with Not_found -> raise (Element_not_found e_id)
    end in begin
    match e with
        | STE(id,set,neg,strt,latch,connect,report,ast_id) ->
            Hashtbl.replace (!net).states e_id (STE(id,set,neg,strt,latch,connect,report,ast_info::ast_id))
        | Counter(id,target,behavior,report,connect,ast_id) ->
            Hashtbl.replace (!net).states e_id (Counter(id,target,behavior,report,connect,ast_info::ast_id))
        | Combinatorial(typ,id,eod,report,connect,ast_id) ->
            Hashtbl.replace (!net).states e_id (Combinatorial(typ,id,eod,report,connect,ast_info::ast_id))
    end
(*Operations on Macros*)
(*let m_create id input output = {
    id = id;
    head = None;
    tail = None;
    states = Hashtbl.create 255;
    input_ids = input;
    output_ids = output;
    input = [];
    output = [];
}*)

(* We do not currently use macros, so this code is outdated.
let m_add_element macro e =
    match e with
        | STE(id,_,_,_,_,_)
        | Counter(id,_,_,_,_)
        | Combinatorial(_,id,_,_,_) -> begin
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
                | Combinatorial(typ,id,eod,report,connect) ->
                    Hashtbl.replace macro.states e1_id (Combinatorial(typ,id,eod,report,e2_conn::connect))
            end ; macro
    end
    else
        (*try e2 as an end terminal*)
        begin try
            let _ = List.find (fun a -> e2_id = a) macro.output_ids in
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
        | Combinatorial(typ,id,eod,report,connect) ->
            Hashtbl.replace macro.states e_id (Combinatorial(typ,id,eod,r,connect))
    end ;
    macro
*)

(*lineage generation*)
let generate_parents (net:'a network ref) =
    Hashtbl.iter ( fun k e ->
        match e with
            | STE(_,_,_,_,_,conn,_,_)
            | Combinatorial(_,_,_,_,conn,_)
            | Counter(_,_,_,_,conn,_) ->
                begin
                    List.iter (fun (e2,_) ->
                        let id = get_id e2 in
                        let child = Hashtbl.find (!net.states) id in
                        match child with
                            | STE(_,_,_,_,_,conn,_,_)
                            | Combinatorial(_,_,_,_,conn,_)
                            | Counter(_,_,_,_,conn,_) ->
                                conn.parents <- (get_id e) :: conn.parents
                    ) conn.children
                end
    ) (!net).states

let clear_parents (net:'a network ref) =
    Hashtbl.iter ( fun k e ->
        match e with
            | STE(_,_,_,_,_,conn,_,_)
            | Combinatorial(_,_,_,_,conn,_)
            | Counter(_,_,_,_,conn,_) ->
                conn.parents <- []
    ) (!net).states

(*Output Functions*)
let element_to_ast_id e =
    match e with
    | STE(_,_,_,_,_,_,_,ast_id) -> List.map (fun a -> (a,"ste")) ast_id
    | Combinatorial(_,_,_,_,_,ast_id) -> List.map (fun a -> (a,"boolean")) ast_id
    | Counter(_,_,_,_,_,ast_id) -> List.map (fun a -> (a,"counter")) ast_id

let mnrl_element_to_str e =
    let behavior_to_str behavior =
        match behavior with
            | Pulse -> "trigger"
            | Latch -> "high"
            | Roll -> "rollover"
    in let start_to_str strt =
        match strt with
            | NotStart -> "onActivateIn"
            | Start -> "onStartAndActivateIn"
            | AllStart -> "always"
    in let connection_id_to_str ((e,c) : 'a element_connection) =
        let term = match c with
            | None -> begin
                match e with
                    | STE(_,_,_,_,_,_,_,_) -> "i"
                    | Combinatorial(typ,_,_,_,_,_) ->
                        begin
                        match typ with
                            | Inverter
                            | OR
                            | AND
                            | NAND
                            | NOR -> "b0"
                            | _ -> raise Malformed_connection
                        end
                    | _ -> raise Malformed_connection
                end 
            | Some x -> x in
        match e with (*TODO Add lots of error-checking here*)
            | STE(id,_,_,_,_,_,_,_) 
            | Counter(id,_,_,_,_,_)
            | Combinatorial(_,id,_,_,_,_) -> (id, term)
    in let con_to_string conn =
        let id,port = connection_id_to_str conn in
        Printf.sprintf "                    {\n                        \"id\": \"%s\",\n                        \"portId\": \"%s\"\n                    }" id port
    in let con_formatter children =
        match children with
            | [] -> ""
            | hd :: tl -> List.fold_left (fun prev b -> prev ^ Printf.sprintf ",\n%s" (con_to_string b)) (con_to_string hd) tl 
    in match e with
        | STE(id,set,neg,strt,latch,connect,report,ast_id) -> begin
            let set_to_str set =
                if set = "*" then
                    set
                else
                let c_list = Util.explode set in
                let hex_escaped = List.fold_left (fun str x -> Printf.sprintf "%s\\\x%02x" str (Char.code x)) "" c_list in
                if neg then
                    Printf.sprintf "[^%s\\\x%x]" hex_escaped (Char.code start_of_input) (*(Char.code counter_trigger_char)*)
                else "[" ^ hex_escaped ^ "]"
            in
            let rep_line =
                if report then "            \"report\": true,\n" else "            \"report\": false,\n" in
            let set_line = Printf.sprintf "                \"symbolSet\": \"%s\"\n" (set_to_str set) in
            let latch_str = if latch then "                \"latched\": true,\n" else "                \"latched\": false,\n" in
            Printf.sprintf "        {\n            \"id\": \"%s\",\n            \"enable\": \"%s\",\n%s            \"type\": \"hState\",\n" id (start_to_str strt) rep_line ^
            Printf.sprintf "            \"inputDefs\": [ { \"portId\": \"i\", \"width\": 1 }],\n            \"outputDefs\": [\n                {\n                    \"portId\": \"o\",\n                    \"width\": 1,\n            \"activate\": [\n%s\n                    ]\n                }\n            ],\n"
                (con_formatter connect.children) ^
            Printf.sprintf "            \"attributes\": {\n%s%s            }\n        }" latch_str set_line 
            end
        | Counter(id,target,behavior,report,connect,ast_id) -> begin
            let mode_line = Printf.sprintf "                \"mode\": \"%s\"\n" (behavior_to_str behavior) in
            let threshold_line = Printf.sprintf "                \"threshold\": %d,\n" target in
            let rep_line =
                if report then "            \"report\": true,\n" else "            \"report\": false,\n" in
            Printf.sprintf "        {\n            \"id\": \"%s\",\n            \"enable\": \"onActivateIn\",\n%s            \"type\": \"upCounter\",\n" id rep_line ^
            Printf.sprintf "            \"inputDefs\": [ { \"portId\": \"cnt\", \"width\": 1 }, { \"portId\": \"rst\", \"width\": 1 } ],\n            \"outputDefs\": [\n                {\n                    \"portId\": \"o\",\n                    \"width\": 1,\n            \"activate\": [\n%s\n                    ]\n                }\n            ],\n"
               (con_formatter connect.children) ^
            Printf.sprintf "            \"attributes\": {\n%s%s            }\n        }" threshold_line mode_line  
            end
        | Combinatorial(typ,id,eod,report,connect,ast_id) ->
            let comb_typ = match typ with
                | Inverter -> "not"
                | OR -> "or"
                | AND -> "and"
                | NAND -> "nand"
                | NOR -> "nor"
                | SOP -> "sum-of-products"
                | POS -> "product-of-sums"
                | NSOP -> "nsum-of-products"
                | NPOS -> "nproduct-of-sums"
            in
            let mode_line = Printf.sprintf "                \"gateType\": \"%s\"\n" comb_typ in
            let rep_line =
                if report then "            \"report\": true,\n" else "            \"report\": false,\n" in
            Printf.sprintf "        {\n            \"id\": \"%s\",\n            \"enable\": \"%s\",\n%s            \"type\": \"boolean\",\n" id (if eod then "onLast" else "onStartAndActivateIn") rep_line ^
            Printf.sprintf "            \"inputDefs\": [ { \"portId\": \"b0\", \"width\": 1 } ],\n            \"outputDefs\": [\n                {\n                    \"portId\": \"o\",\n                    \"width\": 1,\n            \"activate\": [\n%s\n                    ]\n                }\n            ],\n"
                (con_formatter connect.children) ^
            Printf.sprintf "            \"attributes\": {\n%s            }\n        }" mode_line
            

let element_to_str e =
    let behavior_to_str behavior =
        match behavior with
            | Pulse -> "pulse"
            | Latch -> "latch"
            | Roll -> "roll"
    in let start_to_str strt =
        match strt with
            | NotStart -> "none"
            | Start -> "start-of-data"
            | AllStart -> "all-input" 
    in let connection_id_to_str ((e,c) : 'a element_connection) =
        let term = match c with
            | None -> begin
                match e with
                    | STE(_,_,_,_,_,_,_,_) -> ""
                    | Combinatorial(typ,_,_,_,_,_) ->
                        begin
                        match typ with
                            | Inverter
                            | OR
                            | AND
                            | NAND
                            | NOR -> ""
                            | _ -> raise Malformed_connection
                        end
                    | _ -> raise Malformed_connection
                end 
            | Some x -> x in
        match e with (*TODO Add lots of error-checking here*)
            | STE(id,set,neg,strt,latch,connect,report,ast_id) -> id
            | Counter(id,target,behavior,report,connect,ast_id) -> id ^ ":" ^ term
            | Combinatorial(typ,id,_,_,_,_) ->
                match typ with
                    | Inverter
                    | OR
                    | AND
                    | NAND
                    | NOR -> id
                    | _ -> id ^ ":" ^ term
        
    in match e with
        | STE(id,set,neg,strt,latch,connect,report,ast_id) -> begin
            let set_to_str set =
                if set = "*" then
                    set
                else
                let c_list = Util.explode set in
                let hex_escaped = List.fold_left (fun str x -> Printf.sprintf "%s\\x%02x" str (Char.code x)) "" c_list in
                if neg then
                    Printf.sprintf "[^%s\\x%x]" hex_escaped (Char.code start_of_input) (*(Char.code counter_trigger_char)*)
                else "[" ^ hex_escaped ^ "]"
            in
            let rep_line =
                if report then "<report-on-match/>\n" else "" in
            let latch_str = if latch then "latch=\"true\"" else "" in
            Printf.sprintf "<state-transition-element id=\"%s\" symbol-set=\"%s\" start=\"%s\" %s>\n" id (set_to_str set) (start_to_str strt) latch_str ^
            rep_line ^
            List.fold_left (fun prev b -> prev ^ Printf.sprintf "<activate-on-match element = \"%s\" />\n" (connection_id_to_str b) ) "" connect.children ^
            "</state-transition-element>\n"
            end
        | Counter(id,target,behavior,report,connect,ast_id) -> begin
            let rep_line =
                if report then "<report-on-target/>\n" else "" in
            Printf.sprintf "<counter id=\"%s\" target=\"%d\" at-target=\"%s\">\n" id target (behavior_to_str behavior) ^
            rep_line ^
            List.fold_left (fun prev b -> prev ^ Printf.sprintf "<activate-on-target element = \"%s\" />\n" (connection_id_to_str b) ) "" connect.children ^
            "</counter>\n"
            end
        | Combinatorial(typ,id,eod,report,connect,ast_id) ->
            let comb_typ = match typ with
                | Inverter -> "inverter"
                | OR -> "or"
                | AND -> "and"
                | NAND -> "nand"
                | NOR -> "nor"
                | SOP -> "sum-of-products"
                | POS -> "product-of-sums"
                | NSOP -> "nsum-of-products"
                | NPOS -> "nproduct-of-sums"
            in
            let rep_line =
                if report then "<report-on-match/>\n" else "" in
            Printf.sprintf "<%s id=\"%s\" high-only-on-eod=\"%b\">" comb_typ id eod ^
            rep_line ^
            List.fold_left (fun prev b -> prev ^ Printf.sprintf "<activate-on-high element = \"%s\" />\n" (connection_id_to_str b) ) "" connect.children ^
            Printf.sprintf "</%s>\n" comb_typ

let network_to_str (net:'a network ref) =
    let internal = ref "" in
    Hashtbl.iter (fun k e -> internal := !internal ^ element_to_str e) (!net).states ;
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ^
    (Printf.sprintf "<automata-network name=\"%s\" id=\"%s\">\n<description>%s</description>\n" (!net).id (!net).id (!net).description )^ (*TODO figure out how to get an actual name here*)
    !internal ^
    "</automata-network>"

let network_to_file (net:'a network ref) (channel:out_channel) =
    Printf.printf "Automaton size: %d\n%!" (Hashtbl.length (!net).states);
    Printf.fprintf channel "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ;
    Printf.fprintf channel "<automata-network name=\"%s\" id=\"%s\">\n<description>%s</description>\n" (!net).id (!net).id (!net).description ;
    Hashtbl.iter (fun k e -> Printf.fprintf channel "%s" (element_to_str e)) (!net).states ;
    Printf.fprintf channel "</automata-network>"

let mnrl_network_to_file (net:'a network ref) (channel:out_channel) =
    Printf.printf "Automaton size: %d\n%!" (Hashtbl.length (!net).states);
    Printf.fprintf channel "{\n    \"id\":\"%s\",\n" (!net).id  ;
    Printf.fprintf channel "    \"nodes\": [\n";
    let outs = ref [] in
    Hashtbl.iter (fun k e -> outs := (mnrl_element_to_str e) :: !outs) (!net).states ;
    begin match (List.rev !outs) with
    | [] -> Printf.fprintf channel ""
    | hd :: tl -> Printf.fprintf channel "%s" hd ; List.iter (fun e -> Printf.fprintf channel ",\n%s" e) tl
    end ;
    Printf.fprintf channel "\n    ]\n}"

let networks_to_file (nets:('a network ref) list) (channel:out_channel) =
    assert (nets <> []);
    Printf.fprintf channel "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ;
    Printf.fprintf channel "<automata-network name=\"%s\" id=\"%s\">\n<description>%s</description>\n" !(List.hd nets).id !(List.hd nets).id !(List.hd nets).description ;
    let size = List.fold_left (fun size (net:'a network ref) ->
        Hashtbl.iter (fun k e -> Printf.fprintf channel "%s" (element_to_str e)) (!net).states ;
        size + (Hashtbl.length (!net).states);
    ) 0 nets in
    Printf.printf "Automaton size: %d\n%!" size;
    Printf.fprintf channel "</automata-network>"

let mnrl_networks_to_file (nets:('a network ref) list) (channel:out_channel) =
    assert (nets <> []);
    Printf.fprintf channel "{\n    \"id\":\"%s\",\n" !(List.hd nets).id  ;
    Printf.fprintf channel "    \"nodes\": [\n";
    let size = List.fold_left (fun size (net:'a network ref) ->
        let outs = ref [] in
        Hashtbl.iter (fun k e -> outs := (mnrl_element_to_str e) :: !outs) (!net).states ;
        begin match (List.rev !outs) with
        | [] -> Printf.fprintf channel ""
        | hd :: tl -> Printf.fprintf channel "%s" hd ; List.iter (fun e -> Printf.fprintf channel ",\n%s" e) tl
        end ;
        size + (Hashtbl.length (!net).states);
    ) 0 nets in
    Printf.printf "Automaton size: %d\n%!" size;
    Printf.fprintf channel "\n    ]\n}"

let rec print_rec (e:'a element) =
    let _ = print_endline (element_to_str e) in
    match e with
        | STE(_,_,_,_,_,connect,_,_)
        | Counter(_,_,_,_,connect,_)
        | Combinatorial(_,_,_,_,connect,_) -> List.iter (fun (a,c) -> print_rec a) connect.children


(*
match e with
    | STE(id,set,strt,latch,connect,report) ->
    | Counter(id,target,behavior,report,connect) ->
    | Combinatorial(id,eod,report,connect) ->
*)