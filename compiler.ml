(*
 * Compiler for RAPID
 *)
 
open Language (* Contains all the types for RAPID *)
open Config (* Contains types for the configuration language *)
open Id (*For counting on IDs*)
open Util

exception Config_error of string

(*Helper function if needed for debugging*)
let val_to_string value =
    match value with
        | StringValue(s) -> s
        | IntValue(i) -> Printf.sprintf "%d" i
        | CharValue(c) -> Printf.sprintf "%c" c
        | BooleanValue(b) -> Printf.sprintf "%b" b
        | CounterList(_) -> "CounterList"
        | AbstractValue(_,s,_)
        | AbstractChar(s,_) -> s

let do_tiling = ref false

let symbol_table : symbol = Hashtbl.create 255

let abstract_mapping : ((string,string) Hashtbl.t) ref = ref (Hashtbl.create 255)

let return_list = ref StringSet.empty

let symbol_scope = ref StringSet.empty
let counter_rename : (string,string) Hashtbl.t = Hashtbl.create 1000000
let net = Automata.create "" ""

let symbol_variable_lookup (v : string) =
    begin try
    let var = Hashtbl.find symbol_table v in
        begin
        match var with
            | Variable(_,_,_) as return -> return
            | _ -> raise (Syntax_error "")
        end
        with Not_found -> raise (Syntax_error "")
        end

let if_seed = new_seed ()
let c_seed = new_seed ()
let while_seed = new_seed ()
let e_seed = new_seed ()
let m_seed = new_seed ()

(*TODO This is stupid, just doing so that the code works for paper*)
let track_start = ref false

(*Makes a random String of Length *)
let make_string length =
    String.init length (fun _ -> Char.chr ((Random.int 26) + 97))

let evaluate_report last=
    Automata.set_report net last true

let is_counter exp =
    exp.expr_type = Counter
        
let rec add_all net (e:'a Automata.element) =
    let connect = Automata.get_connections e in
    if not (Automata.contains net e) then
        Automata.add_element net e ;
        List.iter (fun (a,c) -> add_all net a) connect.children

let rec find_last elements =
    if List.for_all (fun e -> (Automata.get_connections e).children = []) elements then
        elements
    else
        let new_es = List.fold_left (fun list e ->
            (List.map (fun (c,_) -> c) (Automata.get_connections e).children) @ list
        ) [] elements in
        find_last new_es
(* return (outputs, breaks) *)
let rec cgen_statement ?(start_automaton=false) (stmt : statement) ((last, break) : string list * string list) (label : string) : string list * string list =
    match stmt.stmt with
        | Debug(s) ->
            let Variable(_,_,(Some v)) = symbol_variable_lookup s in
            Printf.printf "%s => %s \n" s (val_to_string v) ;
            if start_automaton then track_start := true ;
            (last, break)
        | Report ->
            List.iter (fun s->return_list := StringSet.add s !return_list; evaluate_report s) last ;
            track_start := false ;
            (last, break)
        | Break ->
            let brk = List.fold_left (fun brk s->
                    let inv = Automata.Combinatorial(Automata.Inverter, "break_"^s, false, false, (Automata.generate_connections []), [AST(stmt.id, Hashtbl.copy symbol_table)]) in
                    Automata.add_element net inv;
                    Automata.connect net s (Automata.get_id inv) None ;
                    (Automata.get_id inv) :: brk
                ) [] last in
            (last,brk)
        | Block(b) ->
            let start_of_automaton = ref ((Automata.is_start_empty net) || start_automaton || !track_start) in
            track_start := false ;
            List.fold_left (fun (last,b) a ->
                let (return,brk) = cgen_statement a (last, b) label ~start_automaton:!start_of_automaton in
                start_of_automaton := false ;
                (return,brk)
            ) (last,break) b
        | If(exp,then_clause,_)
        | While(exp,then_clause) ->
            begin
            let id = Printf.sprintf "%s_if_%d" label (get_num if_seed) in
            let states : (string,'a Automata.element) Hashtbl.t = Hashtbl.create 255 in
            let tb = ref StringSet.empty in
            let fb = ref StringSet.empty in
            let rec flip_symbol (Automata.STE(id,set,neg,strt,latch,connect,report,ast_id) as ste) =
                begin
                let _ = Hashtbl.add states id ste in
                let connection_list : 'a Automata.element_connection list = match connect.children with
                    | hd :: tl -> List.map (fun (a,conn) -> ((flip_symbol a),conn)) connect.children
                    | [] -> tb := StringSet.add id !tb ; [] in
                let new_ste = Automata.STE("n_"^id,set,(not neg),strt,latch,(Automata.generate_connections connection_list),report,ast_id) in
                let _ = Hashtbl.add states ("n_"^id) new_ste in
                begin try
                let mapped = Hashtbl.find !abstract_mapping id in
                    Hashtbl.add !abstract_mapping ("n_"^id) mapped
                with Not_found -> ()
                end
                ;
                new_ste
                end in
            let rec add_conn (Automata.STE(id,set,neg,strt,latch,connect,report,ast_id) as ste) : 'a Automata.element =
                begin
                let new_cons = List.map (fun ((Automata.STE(a_id,a_set,a_neg,a_strt,a_latch,a_connect,a_report,a_ast_id) as a),conn) ->
                                        ((Hashtbl.find states ("n_"^a_id)),None)) connect.children in
                let old_cons = match connect.children with
                    | hd :: tl -> List.map (fun (a,con) -> ((add_conn a),con)) connect.children
                    | [] -> [] in
                Automata.STE(id,set,neg,strt,latch,(Automata.generate_connections (new_cons@old_cons)),report, ast_id)
                end in
            let rec rename (Automata.STE(id,set,neg,strt,latch,connect,report,ast_id) as ste) : 'a Automata.element =
                begin
                let connection_list : 'a Automata.element_connection list = match connect.children with
                    | hd :: tl -> List.map (fun (a,conn) -> ((rename a),conn)) connect.children
                    | [] -> [] in
                let new_ste = Automata.STE("nn_"^id,set,neg,strt,latch,(Automata.generate_connections connection_list),report,ast_id) in
                let _ = Hashtbl.add states ("nn_"^id) new_ste in
                begin try
                let mapped = Hashtbl.find !abstract_mapping id in
                    Hashtbl.add !abstract_mapping ("n_"^id) mapped
                with Not_found -> ()
                end
                ;
                new_ste
                end in
            let rec add_conn_neg (Automata.STE(id,set,neg,strt,latch,connect,report,ast_id) as ste) =
                begin
                match connect.children with
                    | hd :: tl -> List.iter (fun (a,con) ->
                        Automata.connect net id ("n"^(Automata.get_id a)) None ;
                        Automata.connect net ("n"^id) (Automata.get_id a) None ;
                        (add_conn_neg a)
                        ) connect.children
                    | [] -> fb := StringSet.add ("n"^id) (StringSet.add id !fb) ; ()
                end in
            
            let else_clause = match stmt.stmt with
                | If(_,_,e) -> e
                | While(_,_) -> {stmt=Block([]); loc=(-1,-1); id=(-1)}
            in
            match cgen_expression exp None None id (new_seed ()) with
            | CounterExp(c_list,n_list,yes,no) ->
            (*if this is a counter experession*)
                begin
                (*Add in traps!*)
                let yes_trigger = Automata.STE(Printf.sprintf "%s_t" yes,String.make 1 Automata.counter_trigger_char,false,Automata.NotStart,false,(Automata.generate_connections []),false,[AST(exp.id, Hashtbl.copy symbol_table)]) in
                let no_trigger = Automata.STE(Printf.sprintf "%s_f" no,String.make 1 Automata.counter_trigger_char,false,Automata.NotStart,false,(Automata.generate_connections []),false,[AST(exp.id, Hashtbl.copy symbol_table)]) in
                Automata.add_element net yes_trigger ;
                Automata.add_element net no_trigger ;
                Automata.connect net yes (Automata.get_id yes_trigger) None;
                Automata.connect net no (Automata.get_id no_trigger) None;
                List.iter2 (fun c n -> Automata.set_count net c n ) c_list n_list ;
                let (true_last,b1) = cgen_statement then_clause ([(Automata.get_id yes_trigger)], break) label in
                let (false_last,b2) = cgen_statement else_clause ([(Automata.get_id no_trigger)], break) label in
                (true_last @ false_last, b1@b2)
                end
            (*this is a regular if*)
            | AutomataExp(if_exp) ->
                begin
                let start_of_automaton = (Automata.is_start_empty net) || start_automaton || !track_start in
                track_start := false ;
                List.iter (fun if_exp -> 
                    let neg_exp = flip_symbol if_exp in
                    let if_exp_mod = add_conn if_exp in
                    let neg_neg_exp = rename if_exp in
                    add_all net if_exp_mod ;
                    add_all net neg_exp ;
                    add_all net neg_neg_exp ;
                    add_conn_neg neg_exp ;
                    (*we don't use the first element in neg_neg_exp at all
                      so remove it if it's in our false brance list*)
                    fb := StringSet.remove (Automata.get_id neg_neg_exp) !fb ;
                    Automata.remove_element net (Automata.get_id neg_neg_exp) ;
                    List.iter (fun s ->
                        Automata.connect net s (Automata.get_id if_exp_mod) None ;
                        Automata.connect net s (Automata.get_id neg_exp) None
                    ) last ;
                    if start_of_automaton then
                        Automata.set_start net (Automata.get_id if_exp_mod) Automata.Start
                ) if_exp ;
                let (true_last,b1) = cgen_statement then_clause ((StringSet.elements !tb),break) label in
                let (false_last,b2) = cgen_statement else_clause ((StringSet.elements !fb), break) label in
                    match stmt.stmt with
                    | While(_,_) -> List.iter (fun e ->
                                    List.iter (fun s -> Automata.connect net s (Automata.get_id e) None ;
                                                        Automata.connect net s ("n_"^(Automata.get_id e)) None
                                                        ) true_last
                                    ) if_exp ; (*true_last @*) (false_last, b1@b2)
                    | If(_,_,_) -> (true_last @ false_last, b1@b2)
                end
            | BooleanExp(b) ->
                if b then cgen_statement then_clause (last, break) label
                else cgen_statement else_clause (last, break) label
            end
        | Whenever(exp,stmt) ->
            begin
            let id = Printf.sprintf "%s_if_%d" label (get_num if_seed) in
            let start_of_automaton = (Automata.is_start_empty net) || start_automaton || !track_start in
            track_start := false ;
            Printf.printf "start_of_automaton = %b\n" start_of_automaton ;
            let exp_return = cgen_expression exp None None id (new_seed ()) in
            match exp_return with
                | AutomataExp(e_list) -> 
                    let end_of_exp = find_last e_list in
                    let spin = Automata.STE("spin_"^id, "*", false, Automata.NotStart, false, (Automata.generate_connections []), false, [AST(exp.id, Hashtbl.copy symbol_table)]) in
                    let spin_and = Automata.Combinatorial(Automata.AND, "spin_and_"^id, false, false, (Automata.generate_connections []), [AST(exp.id, Hashtbl.copy symbol_table)]) in
                    let exp_and = Automata.Combinatorial(Automata.AND, "exp_and_"^id, false, false, (Automata.generate_connections []), [AST(exp.id, Hashtbl.copy symbol_table)]) in
                    Automata.add_element net spin ;
                    Automata.add_element net spin_and;
                    Automata.add_element net exp_and;
                    (*Connect spin to itself...so that it spins!*)
                    Automata.connect net (Automata.get_id spin) (Automata.get_id spin_and) None ;
                    Automata.connect net (Automata.get_id spin_and) (Automata.get_id spin) None ;
                    Automata.connect net (Automata.get_id spin) (Automata.get_id exp_and) None ;
                    List.iter (fun e ->
                        add_all net e ;
                        (*Add connections from the spin guard to the exp guard*)
                        Automata.connect net (Automata.get_id exp_and) (Automata.get_id e) None ;
                        if start_of_automaton then
                            Automata.set_start net (Automata.get_id e) Automata.Start
                    ) e_list ;
                    List.iter (fun id2 ->
                        (*Add connect from last to the spin guard*)
                        Automata.connect net id2 (Automata.get_id spin) None ;
                        (*Add all of the connections from last to the exp guard*)
                        List.iter (fun e ->
                            Automata.connect net id2 (Automata.get_id e) None
                        ) e_list;
                    ) last ;
                    if start_of_automaton then
                        Automata.set_start net (Automata.get_id spin) Automata.Start;
                    (* Add in body of whenever *)
                    (* This returns back our last set; remove the breaks *)
                    let out,brk = cgen_statement stmt ((List.map Automata.get_id end_of_exp), break) label in
                        List.iter (fun id ->
                            Automata.connect net id (Automata.get_id exp_and) None;
                            Automata.connect net id (Automata.get_id spin_and) None;
                        ) brk ;
                        (out, [])
                | CounterExp(c_list,n_list,yes,no) ->
                    List.iter2 (fun c n -> Automata.set_count net c n ) c_list n_list ;
                    cgen_statement stmt ([yes],break) label
            end
        | Either(statement_blocks) ->
            begin
            let start_of_automaton = (Automata.is_start_empty net) || start_automaton || !track_start in
            track_start := false;
            List.fold_left( fun (complete,b) stmt ->
                let terminals,brk = cgen_statement stmt (last,break) label ~start_automaton:start_of_automaton in
                (complete @ terminals, b@brk)
            ) ([],[]) statement_blocks
            end
        | SomeStmt((Param(name,t)),source,f) ->
            begin
            let start_of_automaton = (Automata.is_start_empty net) || start_automaton || !track_start in
            track_start := false ;
            let obj = cgen_expression source None None label (new_seed ()) in
            match obj with
            | StringExp(s) ->
                let s = explode s in
                    List.fold_left (fun (new_last,b) c ->
                        let c = CharValue(c) in
                        (*set binding to 'name'*)
                        Hashtbl.add symbol_table name (Variable(name,Char,Some c)) ;
                        let return,brk = cgen_statement f (last,break) label ~start_automaton:start_of_automaton in
                        (*remove binding*)
                        Hashtbl.remove symbol_table name ; (new_last @ return, b @ brk)
                    ) ([],[]) s
            | ArrayExp(a) ->
                Array.fold_left (fun (new_last, b) (Some c) ->
                    Hashtbl.add symbol_table name (Variable(name,Char,Some c)) ;
                    let return,brk = cgen_statement f (last,break) label ~start_automaton:start_of_automaton in
                    (*remove binding*)
                    Hashtbl.remove symbol_table name ; (new_last @ return, b @ brk )
                ) ([],[]) a
            | AbstractExp((n,abstract_typ),pre,t,fake) ->
                let n_array = Array.init n (fun n -> n) in
                Array.fold_left (fun (new_last,b) n ->
                    let new_pre = Printf.sprintf "%s_%d_" pre n in
                    begin
                    match t with
                        | String ->
                            let c = match fake with
                            | AbstractString(s) -> String.get s n
                            | _ -> failwith "should be unreachable"
                            in
                            Hashtbl.add symbol_table name (Variable(name,Char,Some (AbstractChar(new_pre,c))))
                        | Array(x) ->
                            let (Config.ArrayInfo(new_size)) = abstract_typ in
                            Hashtbl.add symbol_table name (Variable(name,x,Some (AbstractValue(new_size,new_pre,fake))))
                    end ;
                    let return,brk = cgen_statement f (last,break) label ~start_automaton:start_of_automaton in
                    (*remove binding*)
                    Hashtbl.remove symbol_table name ; (new_last @ return, b @ brk)
                                       
                ) ([],[]) n_array
                
            (* TODO add some sort of array exp*)
            
            end
        | ForEach((Param(name,t)),source,f) ->
            begin
            let start_of_automaton = ref ((Automata.is_start_empty net) || start_automaton || !track_start) in
            track_start := false ;
            let obj = cgen_expression source None None label (new_seed ()) in
            match obj with
            | StringExp(s) ->
                let s = explode s in
                    List.fold_left (fun (last, b) c ->
                        let c = CharValue(c) in
                        (*set binding to 'name'*)
                        Hashtbl.add symbol_table name (Variable(name,Char,Some c)) ;
                        let return,brk = cgen_statement f (last, b) label ~start_automaton:!start_of_automaton in
                        (*remove binding*)
                        Hashtbl.remove symbol_table name ; start_of_automaton := false ; (return, brk)
                    ) (last,break) s
            | ArrayExp(a) ->
                Array.fold_left (fun (last, b) (Some c) ->
                    Hashtbl.add symbol_table name (Variable(name,Char,Some c)) ;
                    let return,brk = cgen_statement f (last, b) label ~start_automaton:!start_of_automaton in
                    (*remove binding*)
                    Hashtbl.remove symbol_table name ; start_of_automaton := false ; (return,brk)
                ) (last,break) a
            | AbstractExp((n,abstract_typ),pre,t,fake) ->
                let n_array = Array.init n (fun n -> n) in
                Array.fold_left (fun (last,b) n ->
                    let new_pre = Printf.sprintf "%s[%d]" pre n in
                    begin
                    match t with
                        | String ->
                            let c = match fake with
                                | AbstractString(s) -> String.get s n
                                | _ -> failwith "should be unreachable"
                            in
                            Hashtbl.add symbol_table name (Variable(name,Char,Some (AbstractChar(new_pre,c))))
                        | Array(x) ->
                            let (Config.ArrayInfo(new_size)) = abstract_typ in
                            Hashtbl.add symbol_table name (Variable(name,x,Some (AbstractValue(new_size,new_pre,NoValue))))
                    end ;
                    let return,brk = cgen_statement f (last,b) label ~start_automaton:!start_of_automaton in
                    (*remove binding*)
                    Hashtbl.remove symbol_table name ; start_of_automaton := false ; (return, brk)
                                       
                ) (last,break) n_array
            (* TODO add some sort of array exp*)
            end
        | Assign((n,o),exp) ->
            begin
            match o with
            | NoOffset ->
                let v = cgen_expression exp None None label (new_seed ()) in
                let value =
                    begin
                    match v with
                        | BooleanExp(b) -> Some (BooleanValue(b))
                        | IntExp(b) -> Some (IntValue(b))
                        | StringExp(s) -> Some (StringValue(s))
                        | AbstractExp(s,p,_,fake) -> Some (AbstractValue(s,p,fake))
                    end
                in
                (* TODO We need some notion of scope to remove these after the fact! *)
                let Variable(id,typ,_) = symbol_variable_lookup n in
                Hashtbl.add symbol_table id (Variable(id,typ,value)) ;
                symbol_scope := StringSet.add id !symbol_scope ;
            | _ -> failwith "array assignment not supported atm"
            end
            ;
            if start_automaton then track_start := true ;
            (last, break)
        | VarDec(var) ->
            let rec gen_value init =
                match init with
                    | PrimitiveInit(e) ->
                        let v = cgen_expression e None None label (new_seed ()) in
                        begin
                        match v with
                            | BooleanExp(b) -> Some (BooleanValue(b))
                            | IntExp(b) -> Some (IntValue(b))
                            | StringExp(s) -> Some (StringValue(s))
                            | AbstractExp(s,p,_,fake) -> Some (AbstractValue(s,p,fake))
                        end
                    | ArrayInit(e) ->
                        let i_list = List.map gen_value e in
                        let a = Array.of_list i_list in
                            Some (ArrayValue(a))
            in
            begin
            List.iter (fun dec ->
                (*TODO support list*)
                let (id,value) =
                    match dec.typ with
                    | Counter ->
                        let num = get_num c_seed in
                        let id = Printf.sprintf "%s_%s_%d" label dec.var num in
                        let i_id = Printf.sprintf "%s_i" id in
                        let counter = Automata.Counter(id,100,Automata.Latch,false,(Automata.generate_connections []), [AST(stmt.id, Hashtbl.copy symbol_table)]) in
                        let inverter = Automata.Combinatorial(Inverter,i_id, false, false, (Automata.generate_connections []), [AST(stmt.id, Hashtbl.copy symbol_table)]) in
                        (*Add to symbol table*)
                        Hashtbl.add counter_rename dec.var id ;
                        (*Hashtbl.add symbol_table dec.var (Variable(id,dec.typ,None)) ;*)
                        (*Add to net*)
                        Automata.add_element net counter ;
                        Automata.add_element net inverter ;
                        Automata.connect net id i_id None ;
                        (*Reset as soon as in scope*)
                        List.iter (fun e_id ->
                            Automata.connect net e_id id (Some "rst")
                        ) last ;
                        (*Add to scope*)
                        symbol_scope := StringSet.add dec.var !symbol_scope ;
                        (dec.var, Some (CounterList([id])))
                    | DoubleCounter(is_eq) ->
                        let num1 = get_num c_seed in
                        let num2 = get_num c_seed in
                        let id1 = Printf.sprintf "%s_%s_%d" label dec.var num1 in
                        let id2 = Printf.sprintf "%s_%s_%d" label dec.var num2 in
                        let i_id1 = Printf.sprintf "%s_i" id1 in
                        let i_id2 = Printf.sprintf "%s_i" id2 in
                        let id_true = Printf.sprintf "%s_true" id1 in
                        let id_false = Printf.sprintf "%s_false" id1 in
                        let counter1 = Automata.Counter(id1,100,Automata.Latch,false,(Automata.generate_connections []), [AST(stmt.id, Hashtbl.copy symbol_table)]) in
                        let counter2 = Automata.Counter(id2,100,Automata.Latch,false,(Automata.generate_connections []), [AST(stmt.id, Hashtbl.copy symbol_table)]) in
                        let inverter1 = Automata.Combinatorial(Inverter,i_id1, false, false, (Automata.generate_connections []), [AST(stmt.id, Hashtbl.copy symbol_table)]) in
                        let inverter2 = Automata.Combinatorial(Inverter,i_id2, false, false, (Automata.generate_connections []), [AST(stmt.id, Hashtbl.copy symbol_table)]) in
                        let out_true = if is_eq then
                            Automata.Combinatorial(AND,id_true,false,false,(Automata.generate_connections []), [AST(stmt.id, Hashtbl.copy symbol_table)])
                        else
                            Automata.Combinatorial(OR,id_true,false,false,(Automata.generate_connections []), [AST(stmt.id, Hashtbl.copy symbol_table)])
                        in
                        let out_false = if is_eq then
                            Automata.Combinatorial(AND,id_false,false,false,(Automata.generate_connections []), [AST(stmt.id, Hashtbl.copy symbol_table)])
                        else
                            Automata.Combinatorial(OR,id_false,false,false,(Automata.generate_connections []), [AST(stmt.id, Hashtbl.copy symbol_table)])
                        in
                        (*Add to symbol table*)
                        Hashtbl.add counter_rename dec.var id1 ;
                        Hashtbl.add counter_rename dec.var id2 ;
                        (*Hashtbl.add symbol_table dec.var (Variable(id,dec.typ,None)) ;*)
                        (*Add to net*)
                        Automata.add_element net counter1 ;
                        Automata.add_element net counter2 ;
                        Automata.add_element net inverter1 ;
                        Automata.add_element net inverter2 ;
                        Automata.add_element net out_true ;
                        Automata.add_element net out_false ;
                        Automata.connect net id1 i_id1 None ;
                        Automata.connect net id2 i_id2 None ;
                        Automata.connect net i_id1 id_true None ;
                        Automata.connect net id2 id_true None ;
                        Automata.connect net id1 id_false None ;
                        Automata.connect net i_id2 id_false None ;
                        (*Reset as soon as in scope*)
                        List.iter (fun e_id ->
                            Automata.connect net e_id id1 (Some "rst");
                            Automata.connect net e_id id2 (Some "rst")
                        ) last ;
                        (*Add to scope*)
                        symbol_scope := StringSet.add dec.var !symbol_scope ;
                        (dec.var, Some (CounterList([id1;id2])))
                    | _ ->
                        let x = match dec.init with
                            | None -> None
                            | Some i -> gen_value i
                        in (dec.var,x) in
                (* TODO We need some notion of scope to remove these after the fact! *)
                Hashtbl.add symbol_table id (Variable(id,dec.typ,value)) ;
                symbol_scope := StringSet.add id !symbol_scope ;
            ) var ;
            if start_automaton then track_start := true ;
            (last, break) (*TODO OMG this will not work correctly.  So much error checking needed*)
            end
        (*| ExpStmt(e,scope) -> match e with None -> () | Some x -> Automata.add_element net (cgen_expression x None)*) (*TODO check to see if the expression is allowed as a statement*)
        | MacroCall(a,Arguments(b)) -> begin
            (* Look up macro in the symbol table, make sure it is there *)
            (* TODO does this need to be error-checked? *)
            let start_of_automaton = (Automata.is_start_empty net) || start_automaton || !track_start in
            track_start := false ;
            let (MacroContainer(Macro(name,Parameters(params),stmts) as m)) = Hashtbl.find symbol_table a in
                (*evaluate macro*)
                (*FIXME break can't go over macro bound*)
                (cgen_macro m b last ~start_automaton:start_of_automaton, [])
            end
        | ExpStmt(e) ->
            begin
            let start_of_automaton = (Automata.is_start_empty net) || start_automaton || !track_start in
            track_start := false ;
            match e.expr_type with
                | Automata ->
                    
                    let (AutomataExp(e_list)) = cgen_expression e (Some (List.map (fun l -> Automata.get_element net l) last)) None (Printf.sprintf "%s_exp_%d" label (get_num e_seed)) (new_seed ()) in
                    let new_last = List.fold_left (fun ss e ->
                        StringSet.add (Automata.get_id e) ss
                    ) StringSet.empty (find_last e_list) in
                    List.iter (fun e ->
                        add_all net e ;
                        if start_of_automaton then
                            Automata.set_start net (Automata.get_id e) Automata.Start
                    ) e_list;
                    List.iter (fun e1 ->
                        List.iter (fun e2 -> Automata.connect net e1 (Automata.get_id e2) None ) e_list
                    ) last ;
                    if not (StringSet.is_empty new_last) then (StringSet.elements new_last, break) else (last, break)
                | _ ->
                    begin
                    let return = cgen_expression e (Some (List.map (fun l -> Automata.get_element net l) last)) None label (new_seed ()) in
                    match return with
                        | BooleanExp(false) -> ([], break)
                        | _ -> (last, break)
                    end
            end
        | _ -> Printf.printf "Oh goodness! %s" (statement_to_str stmt) ; raise (Syntax_error "unimplemented method")
and cgen_expression (exp : expression) (before : 'a Automata.element list option) (s : 'a Automata.element list option) (prefix : string) (seed : id_seed) =
    (*get the type of an expression...needed to determine how to eval the exp*)
    match exp.expr_type with
        | Boolean -> BooleanExp(evaluate_boolean_expression exp)
        | DoubleCounter(_)
        | Counter ->
         CounterExp(evaluate_counter_expression exp)
        | Automata -> AutomataExp(cgen_expression_aut exp before s prefix seed)
        | Int -> IntExp(evaluate_int_expression exp)
        | Char -> CharExp(evaluate_char_expression exp)
        | String -> evaluate_string_expression exp
        | Array(_) -> evaluate_array_expression exp
and cgen_expression_aut (exp : expression) (before : 'a Automata.element list option) (s: 'a Automata.element list option) (prefix:string) (seed:id_seed) : 'a Automata.element list =
    (*Helper...requires type checking first or will result in a syntax error*)
    let id = Printf.sprintf "%s_%d" prefix (get_num seed) in
    let get_value v =
        begin
        match v.exp with
            | Lval((n,o)) ->
                let (Variable(name,typ,Some v)) = Hashtbl.find symbol_table n in
                begin
                match o with
                | NoOffset ->
                    begin match v with
                    | CharValue(s) -> s
                    | AbstractChar(s,c) ->
                        Hashtbl.add !abstract_mapping id s ;
                        c
                    end
                end
            | _ -> evaluate_char_expression v
        end in
    match exp.exp with
        | EQ(a,b) -> begin
            let helper (Automata.STE(id,set,neg,strt,latch,connect,report,ast_id) as new_element) = begin
                match s with
                    | None -> [new_element]
                    | Some x ->
                        let cons = List.map (fun a -> (a,None)) x in
                        [Automata.STE(id,set,neg,strt,latch,(Automata.generate_connections (cons@connect.children)),report,ast_id)]
            end in
            if a.exp = Input then
                (*FIXME getting rid of Char.escaped...not sure of the impacts of this*)
                let new_element = Automata.STE(id,Printf.sprintf "%c" (get_value b),false,Automata.NotStart,false,(Automata.generate_connections []),false,[AST(exp.id, Hashtbl.copy symbol_table)]) in
                helper new_element
            else if b.exp = Input then
                let new_element = Automata.STE(id,Printf.sprintf "%c" (get_value a),false,Automata.NotStart,false,(Automata.generate_connections []),false,[AST(exp.id, Hashtbl.copy symbol_table)]) in
                helper new_element

            else raise (Syntax_error "Something with Input")
            end
        | NEQ(a,b) -> begin (* TODO This is just copied and pasted from above; need to fix this *)
            let helper (Automata.STE(id,set,neg,strt,latch,connect,report,ast_id) as new_element) = begin
                match s with
                    | None -> [new_element]
                    | Some x -> 
                        let cons = List.map (fun a -> (a,None)) x in
                        [Automata.STE(id,set,neg,strt,latch,(Automata.generate_connections (cons@connect.children)),report,ast_id)]
            end in
            if a.exp = Input then
                let new_element = Automata.STE(id,Printf.sprintf "%c" (get_value b),true,Automata.NotStart,false,(Automata.generate_connections []),false,[AST(exp.id, Hashtbl.copy symbol_table)]) in
                helper new_element
            else if b.exp = Input then
                let new_element = Automata.STE(id,Printf.sprintf "%c"(get_value a),true,Automata.NotStart,false,(Automata.generate_connections []),false,[AST(exp.id, Hashtbl.copy symbol_table)]) in
                helper new_element
            else raise (Syntax_error "Something with Input")
            end
        (*| LEQ(a,b)
        | LT(a,b)
        | GT(a,b)*)
        | Not(a) -> (* TODO This is a copied and modified from the if-statement.  Need to combine/consolidate *)
            begin
            let temp_net = Automata.create "tmp" "" in
            let states : (string,'a Automata.element) Hashtbl.t = Hashtbl.create 255 in
            let rec flip_symbol (Automata.STE(id,set,neg,strt,latch,connect,report,ast_id) as ste) =
                begin
                let _ = Hashtbl.add states id ste in
                let connection_list : 'a Automata.element_connection list = match connect.children with
                    | hd :: tl -> List.map (fun (a,conn) -> ((flip_symbol a),conn)) connect.children
                    | [] -> [] in
                let new_ste = Automata.STE("n_"^id,set,(not neg),strt,latch,(Automata.generate_connections connection_list),report,ast_id) in
                let _ = Hashtbl.add states ("n_"^id) new_ste in
                begin try
                let mapped = Hashtbl.find !abstract_mapping id in
                    Hashtbl.add !abstract_mapping ("n_"^id) mapped
                with Not_found -> ()
                end
                ;
                new_ste
                end in
            let rec add_conn (Automata.STE(id,set,neg,strt,latch,connect,report,ast_id) as ste) : 'a Automata.element =
                begin
                let new_cons = List.map (fun ((Automata.STE(a_id,a_set,a_neg,a_strt,a_latch,a_connect,a_report,a_ast) as a),conn) ->
                                        ((Hashtbl.find states ("n_"^a_id)),None)) connect.children in
                let old_cons = match connect.children with
                    | hd :: tl -> List.map (fun (a,con) -> ((add_conn a),con)) connect.children
                    | [] -> [] in
                Automata.STE(id,set,neg,strt,latch,(Automata.generate_connections (new_cons@old_cons)),report,ast_id)
                end in
            let rec rename (Automata.STE(id,set,neg,strt,latch,connect,report,ast_id) as ste) : 'a Automata.element =
                begin
                let connection_list : 'a Automata.element_connection list = match connect.children with
                    | hd :: tl -> List.map (fun (a,conn) -> ((rename a),conn)) connect.children
                    | [] -> [] in
                let new_ste = Automata.STE("nn_"^id,set,neg,strt,latch,(Automata.generate_connections connection_list),report,ast_id) in
                let _ = Hashtbl.add states ("nn_"^id) new_ste in
                begin try
                let mapped = Hashtbl.find !abstract_mapping id in
                    Hashtbl.add !abstract_mapping ("n_"^id) mapped
                with Not_found -> ()
                end
                ;
                new_ste
                end in
            let rec add_conn_neg (Automata.STE(id,set,neg,strt,latch,connect,report,ast_id) as ste) =
                begin
                match connect.children with
                    | hd :: tl -> List.iter (fun (a,con) ->
                        Automata.connect temp_net id ("n"^(Automata.get_id a)) None ;
                        Automata.connect temp_net ("n"^id) (Automata.get_id a) None ;
                        (add_conn_neg a)
                        ) connect.children
                    | [] -> ()
                end in
            (* TODO This None might actually bad...will have to consider further
                It should be a "last" element.  Probably have to add it in later
                (after the fold)
            *)
            let if_exp = cgen_expression_aut a None None prefix seed in
            List.fold_left (fun rest if_exp ->
                let neg_exp = flip_symbol if_exp in
                let if_exp_mod = add_conn if_exp in
                let neg_neg_exp = rename if_exp in
                add_all temp_net neg_exp ;
                add_all temp_net neg_neg_exp ;
                add_conn_neg neg_exp ;
                Automata.remove_element temp_net (Automata.get_id neg_neg_exp) ;
                (Automata.get_element temp_net (Automata.get_id neg_exp)) :: rest
            ) [] if_exp
            end
        (*| Negative(a)*)
        | And(a,b) ->
            let rec add_to_last start x =
                match start with
                    | Automata.STE(id,set,neg,strt,latch,connect,report,ast_id) ->
                        begin
                        match connect.children with
                        | hd :: tl -> Automata.STE(id,set,neg,strt,latch, (Automata.generate_connections (List.map (fun (a,s) -> ((add_to_last a x),s)) connect.children)), report,ast_id)
                        | [] ->
                            let temp = List.map (fun a -> (a,None)) x in
                            Automata.STE(id,set,neg,strt,latch,(Automata.generate_connections temp),report,ast_id)
                        end
                    | Automata.Combinatorial(typ,id,eod,report,connect,ast_id) ->
                        begin
                        match connect.children with
                        | hd :: tl -> Automata.Combinatorial(typ, id, eod, report,(Automata.generate_connections (List.map (fun (a,s) -> ((add_to_last a x),s)) connect.children)),ast_id)
                        | [] ->
                            let temp = List.map (fun a -> (a,None)) x in
                            Automata.Combinatorial(typ,id,eod,report,(Automata.generate_connections temp),ast_id)
                        end
                        
            in
            let b_eval = cgen_expression_aut b None None prefix seed in
            let a_eval = cgen_expression_aut a None (Some b_eval) prefix seed in
            begin
            match s with
                | None -> a_eval
                | Some x -> List.map (fun a -> add_to_last a x) a_eval
            end
        | PAnd(a,b) ->
            (*Evaluate separately and then combine with an AND gate*)
            let connect = match s with
                        | None -> []
                        | Some x -> List.map (fun a -> (a,None)) x
            in
            let join = Automata.Combinatorial(Automata.AND,Printf.sprintf "%s_and_%d" prefix (get_num seed), false, false, (Automata.generate_connections connect),[AST(exp.id, Hashtbl.copy symbol_table)]) in
            let a_eval = cgen_expression_aut a None (Some [join]) prefix seed in
            let b_eval = cgen_expression_aut b None (Some [join]) prefix seed in
                a_eval@b_eval
        | Or(a,b) ->
            (*TODO Do we do this here, or at a later optimization stage?*)
            let rec build_charset c =
                match c.exp with
                    | EQ(a,b)
                    | NEQ(a,b) ->
                        if a.exp = Input then Printf.sprintf "%c" (get_value b)
                        else if b.exp = Input then Printf.sprintf "%c" (get_value a)
                        else raise (Syntax_error "Input is messed up")
                    | Or(a,b) -> (build_charset a) ^ (build_charset b)
                    | _ -> ""
                in
            let rec can_condense c =
                match c.exp with
                    | EQ(_,_)
                    | NEQ(_,_) -> true
                    | Or(a,b) -> (can_condense a) && (can_condense b)
                    | And(_,_)
                    | PAnd(_,_) -> false
                in
            if can_condense a then
                if can_condense b then
                    let connect = match s with
                        | None -> []
                        | Some x -> List.map (fun a -> (a,None)) x
                    in [Automata.STE(id,Printf.sprintf "%s" ((build_charset a)^(build_charset b)),false, Automata.NotStart, false, (Automata.generate_connections connect), false,[AST(a.id,Hashtbl.copy symbol_table);AST(b.id, Hashtbl.copy symbol_table)])]
                else
                let b_eval = cgen_expression_aut b None s prefix seed in
                    let connect = match s with
                        | None -> []
                        | Some x -> List.map (fun a -> (a,None)) x
                    in Automata.STE(id,Printf.sprintf "%s" ((build_charset a)),false, Automata.NotStart, false, (Automata.generate_connections connect), false, [AST(a.id, Hashtbl.copy symbol_table)]) :: b_eval
            else
                if can_condense b then
                let a_eval = cgen_expression_aut a None s prefix seed in
                    let connect = match s with
                        | None -> []
                        | Some x -> List.map (fun a -> (a,None)) x
                    in Automata.STE(id,Printf.sprintf "%s" ((build_charset b)), false, Automata.NotStart, false, (Automata.generate_connections connect), false, [AST(b.id, Hashtbl.copy symbol_table)]) :: a_eval
                else
                    let a_eval = cgen_expression_aut a None s prefix seed in
                    let b_eval = cgen_expression_aut b None s prefix seed in
                    a_eval @ b_eval
        | Fun((a,_),b,c) ->
            begin match b with
                | "count" ->
                    begin
                    (*let id = Hashtbl.find counter_rename a in*)
                    let counter = Hashtbl.find symbol_table a in
                    let last = match before with Some x -> x | _ -> [] in
                    
                    match counter with
                        | Variable(s,t,v) ->
                            let c_list = match v with
                                | Some CounterList(x) -> x in
                            begin
                            List.iter ( fun  c ->
                                (* Update so that we have debugging info for counts *)
                                let (Automata.Counter(id,b,c,d,e,ast_list) as old_c) = Automata.get_element net c in
                                let new_c = Automata.Counter(id,b,c,d,e,PortAST(exp.id, "cnt", Hashtbl.copy symbol_table)::ast_list) in
                                Automata.remove_element net id ;
                                Automata.add_element net new_c ;
                                List.iter (fun l -> Automata.connect net (Automata.get_id l) id (Some "cnt")) last
                            ) c_list ; []
                            
                            end
                            
                    end
                | "reset" ->
                    begin
                    (*let id = Hashtbl.find counter_rename a in*)
                    let counter = Hashtbl.find symbol_table a in
                    let last = match before with Some x -> x | _ -> [] in
                    match counter with
                        | Variable(s,t,v) ->
                            let c_list = match v with
                                | Some CounterList(x) -> x in
                            (*Check that we have a counter*)
                            begin
                            List.iter ( fun c ->
                                List.iter (fun l -> Automata.connect net (Automata.get_id l) c (Some "rst")) last
                            ) c_list ; []
                            end
                    end

            end
        (*| Var(a)
        | Lit(a)
        | Input*)
        
and evaluate_counter_expression (exp : expression) =
    let get_counter v =
        begin
        match v.exp with
            | Lval((a,_)) ->
                let id = Hashtbl.find counter_rename a in
                let (Variable(name,typ, Some (CounterList(e)))) = Hashtbl.find symbol_table a in
                    e
                
        end in
    let helper num c_list =
        begin
        let c = List.hd c_list in
        let number = match exp.exp with
            | LEQ(_,_)
            | GT(_,_) -> [(num+1)]
            | LT(_,_)
            | GEQ(_,_) -> [num]
            | _ -> [num;(num+1)]
        in match exp.exp with
            | LT(_,_)
            | LEQ(_,_) ->
                (*The inverter has the same id as c + "_i" *)
                (c_list, number, c^"_i", c)
            | GT(_,_)
            | GEQ(_,_) ->
                (c_list, number, c, c^"_i")
            | EQ(_,_) ->
                (c_list, List.rev number, c^"_true",c^"_false")
            | NEQ(_,_) ->
                (c_list, number, c^"_true",c^"_false")
                
        end in
    (*if counter is not listed first, flip it!*)
    
    match exp.exp with
        | EQ(a,b) -> if is_counter b then evaluate_counter_expression ({ exp with exp = EQ(b,a); })
                     else
                        helper (evaluate_int_expression b) (get_counter a)
        | GT(a,b) -> if is_counter b then evaluate_counter_expression ({ exp with exp = LT(b,a); })
                     else
                        helper (evaluate_int_expression b) (get_counter a)
        | GEQ(a,b) -> if is_counter b then evaluate_counter_expression ({ exp with exp = LEQ(b,a); })
                      else
                        helper (evaluate_int_expression b) (get_counter a)
        | LEQ(a,b) -> if is_counter b then evaluate_counter_expression ({ exp with exp = GEQ(b,a); })
                      else
                        helper (evaluate_int_expression b) (get_counter a)
        | LT(a,b) -> if is_counter b then evaluate_counter_expression ({ exp with exp = GT(b,a); })
                     else
                        helper (evaluate_int_expression b) (get_counter a)
        | Not(e) ->
            let c_list,n_list,no,yes = evaluate_counter_expression e in
            (c_list,n_list,yes,no)
        | And(a,b)
        | PAnd(a,b) ->
            let a_c_list,a_n_list,a_yes,a_no = evaluate_counter_expression a in
            let b_c_list,b_n_list,b_yes,b_no = evaluate_counter_expression b in
            let yes = List.fold_left (fun last c -> Printf.sprintf "%s_%s" last c) "yes" (a_c_list@b_c_list) in
            let no = List.fold_left (fun last c -> Printf.sprintf "%s_%s" last c) "no" (a_c_list@b_c_list) in
            let yes_and = Automata.Combinatorial(Automata.AND,yes,false,false,(Automata.generate_connections []), [AST(exp.id, Hashtbl.copy symbol_table)]) in
            let no_or = Automata.Combinatorial(Automata.OR,no,false,false,(Automata.generate_connections []), [AST(exp.id, Hashtbl.copy symbol_table)]) in
                Automata.add_element net yes_and ;
                Automata.add_element net no_or ;
                Automata.connect net a_yes yes None ;
                Automata.connect net b_yes yes None ;
                Automata.connect net a_no no None ;
                Automata.connect net b_no no None ;
                (a_c_list@b_c_list,a_n_list@b_n_list,yes,no)
        | Or(a,b) ->
            let a_c_list,a_n_list,a_yes,a_no = evaluate_counter_expression a in
            let b_c_list,b_n_list,b_yes,b_no = evaluate_counter_expression b in
            let yes = List.fold_left (fun last c -> Printf.sprintf "%s_%s" last c) "yes" (a_c_list@b_c_list) in
            let no = List.fold_left (fun last c -> Printf.sprintf "%s_%s" last c) "no" (a_c_list@b_c_list) in
            let yes_or = Automata.Combinatorial(Automata.OR,yes,false,false,(Automata.generate_connections []),[AST(exp.id, Hashtbl.copy symbol_table)]) in
            let no_and = Automata.Combinatorial(Automata.AND,no,false,false,(Automata.generate_connections []),[AST(exp.id, Hashtbl.copy symbol_table)]) in
                Automata.add_element net yes_or ;
                Automata.add_element net no_and ;
                Automata.connect net a_yes yes None ;
                Automata.connect net b_yes yes None ;
                Automata.connect net a_no no None ;
                Automata.connect net b_no no None ;
                (a_c_list@b_c_list,a_n_list@b_n_list,yes,no)
        | _ -> failwith (exp_to_str exp) ; (["foo"],[1],"bar","baz")

and evaluate_boolean_expression exp =
    match exp.exp with
        | EQ(a,b) ->
            begin match a.expr_type with
                | Boolean -> (evaluate_boolean_expression a) = (evaluate_boolean_expression b)
                | Int -> (evaluate_int_expression a) = (evaluate_int_expression b)
                | Char -> (evaluate_char_expression a) = (evaluate_char_expression b)
            end
        | NEQ(a,b) ->
            begin match a.expr_type with
                | Boolean -> (evaluate_boolean_expression a) <> (evaluate_boolean_expression b)
                | Int -> (evaluate_int_expression a) <> (evaluate_int_expression b)
                | Char -> (evaluate_char_expression a) <> (evaluate_char_expression b)
            end
        | LEQ(a,b) -> (evaluate_int_expression a) <= (evaluate_int_expression b)
        | GEQ(a,b) -> (evaluate_int_expression a) >= (evaluate_int_expression b)
        | LT(a,b) -> (evaluate_int_expression a) < (evaluate_int_expression b)
        | GT(a,b) -> (evaluate_int_expression a) > (evaluate_int_expression b)
        | Not(a) -> not (evaluate_boolean_expression a)
        | And(a,b) -> (evaluate_boolean_expression a) && (evaluate_boolean_expression b)
        | Or(a,b) -> (evaluate_boolean_expression a) || (evaluate_boolean_expression b)
        | Lval((a,o)) ->
            let Variable(n,t,(Some (BooleanValue(b)))) = symbol_variable_lookup a in
                b
        | Lit(a) -> begin match a with
                      | True -> true
                      | False -> false
                    end

and evaluate_int_expression exp =
    match exp.exp with
        | Plus(a,b)
        | Minus(a,b)
        | Times(a,b)
        | Mod(a,b) ->
            let a_value = evaluate_int_expression a in
            let b_value = evaluate_int_expression b in
                begin
                match exp.exp with
                | Plus(_,_) -> a_value + b_value
                | Minus(_,_) -> a_value - b_value
                | Times(_,_) -> a_value * b_value
                | Mod(_,_) -> a_value mod b_value
                end
        | Negative(a) -> - (evaluate_int_expression a)
        | Lval((a,o)) ->
            let Variable(n,t,(Some (IntValue(i)))) = symbol_variable_lookup a in
                i
        | Lit(a) -> begin match a with
                      | IntLit(x,_) -> x
                    end
        | Fun((a,o),b,c) ->
            let (Variable(n,t,(Some value))) = symbol_variable_lookup a in
                begin
                match value with
                | StringValue(v) ->
                    begin match b with
                        | "length" -> String.length v
                    end
                | AbstractValue((s,_),_,_) ->
                    begin match b with
                        | "length" ->
                            (*TODO THIS DOESN'T WORK WELL BUT WILL DO FOR NOW*)
                            s
                    end
                end
and evaluate_char_expression exp =
    match exp.exp with
        | Lval((a,o)) ->
            let Variable(n,t,(Some (CharValue(c)))) = symbol_variable_lookup a in
                c
        | Lit(a) -> begin match a with
                      | CharLit(x,_) -> x
                      | AllIn -> Automata.all_in
                      | StartIn -> Automata.start_in
                    end
        | Fun((a,o),b,Arguments(args)) ->
            (*TODO ALLOW OFFSETS!*)
            let Variable(n,t,(Some v)) = symbol_variable_lookup a in
                begin
                match b with
                    | "charAt" -> begin
                        match v with
                            | StringValue(s) ->
                                let loc = evaluate_int_expression (List.hd args) in
                                    String.get s loc
                            | AbstractValue(s,pre,fake) ->
                                    failwith "currently Abstract value charats not supported"
                            (*TODO Add support for abstract values with substring*)
                    end
                end 

and evaluate_string_expression exp =
    match exp.exp with
        | Lval((a,o)) ->
            let Variable(n,t,(Some v)) = symbol_variable_lookup a in
                begin
                match v with
                    | StringValue(s) -> StringExp(s)
                    | AbstractValue(s,pre,fake) -> AbstractExp((s,pre,t,fake))
                end
        | Lit(a) -> begin match a with
                      | StringLit(x,_) -> StringExp(x)
                    end
        | Fun((a,o),b,Arguments(args)) ->
            (*TODO: Allow for offsets*)
            let Variable(n,t,(Some v)) = symbol_variable_lookup a in
                begin
                match b with
                    | "sub" -> begin
                        match v with
                            | StringValue(s) ->
                                let start = evaluate_int_expression (List.hd args) in
                                let length = (evaluate_int_expression (List.nth args 1)) - start in
                                let new_s = String.sub s start length in
                                StringExp(new_s)
                            | AbstractValue(s,pre,fake) ->
                                let start = evaluate_int_expression (List.hd args) in
                                let last = (evaluate_int_expression (List.nth args 1)) in
                                let length = last  - start in
                                let pre = Printf.sprintf "%s.sub(%d,%d)" pre start last in
                                let new_val = match fake with
                                    | NoValue -> NoValue
                                    | AbstractString(s) -> AbstractString((String.sub s start length))
                                in
                                AbstractExp((last-start,StringInfo),pre,String,new_val)
                            (*TODO Add support for abstract values with substring*)
                    end
                end

(*TODO Offsets*)
and evaluate_array_expression exp =
    match exp.exp with
        | Lval((a,o)) ->
            let Variable(n,t,(Some v)) = symbol_variable_lookup a in
                begin
                match v with
                    | ArrayValue(s) -> ArrayExp(s)
                    | AbstractValue(s,pre,fake) -> AbstractExp((s,pre,t,fake))
                end
        | Fun((a,_),b,Arguments(args)) ->
            begin
            match a with
                | "Integer" ->
                    begin
                    match b with
                        | "range" ->
                            let low = evaluate_int_expression (List.hd args) in
                            let high = evaluate_int_expression (List.nth args 1) in
                                ArrayExp(Array.init (high - low) (fun i ->
                                    Some (IntValue(i+low))
                                ) )
                    end
            end
                    
and cgen_macro ?(start_automaton=false) (Macro(name,Parameters(params),stmt)) (args:expression list) (last:string list) =
    (* add bindings for arguments to state *)
    let s = Printf.sprintf "%s_%d" name (get_num m_seed) in
    (*back up symbol scope and create a new one*)
    let scope_backup = !symbol_scope in
    let return_backup = !return_list in
    return_list := StringSet.empty ;
    symbol_scope := StringSet.empty ;
    List.iter2 (fun (Param(p,t)) arg ->
        let value =
            begin
            (*TODO allow for offsets*)
            (*TODO ALLOW ARBITRARY EXPRESSIONS*)
            match arg.exp with
                | Lval((s,o2)) ->
                    let variable = Hashtbl.find symbol_table s in
                    begin
                    match variable with
                        | Variable(name,t,value) ->
                            begin
                            (*keep track of counter rename*)
                            match t with
                                | Counter -> Hashtbl.add counter_rename p name
                                | _ -> ()
                            end
                            ;
                            begin
                            match o2 with
                                | NoOffset -> value
                                | _ -> failwith "need to handle offsets"
                            end
                        | _ -> raise (Syntax_error "")
                    end
                | Lit(l) ->
                    begin
                    match l with
                        | StringLit(s,_) -> Some (StringValue(s))
                        | IntLit(s,_) -> Some (IntValue(s))
                        | CharLit(s,_) -> Some (CharValue(s))
                        | AllIn -> Some (CharValue(Automata.all_in))
                        | StartIn -> Some (CharValue(Automata.start_in))
                    end
                | _ -> failwith (exp_to_str arg)
            end in
        let v = match value with
            | Some(v) -> v
            | _ -> StringValue("")
        in
        (*s := Printf.sprintf "%s_%s" !s (String.map (fun c ->
            match c with
                | '['
                | ']'
                | '+' -> '_'
                | _ -> c
        ) (val_to_string v)) ;*) 
        Hashtbl.add symbol_table p (Variable(p,t,value))
    ) params args ;
    (* verify that we have a block; evalutate it *)
    
    let last = match stmt.stmt with
        | Block(b) -> let (l,_) = cgen_statement stmt (last,[]) s ~start_automaton:start_automaton in
                        l
    in
    (* remove those bindings again *)
    List.iter(fun (Param(p,t)) ->
        begin
        match t with
            | Counter -> Hashtbl.remove counter_rename p
            | _ -> ()
        end;
        Hashtbl.remove symbol_table p) params ;
    StringSet.iter(fun s -> Hashtbl.remove symbol_table s) !symbol_scope ;
    symbol_scope := scope_backup ;
    let return : string list =
        if StringSet.is_empty !return_list then last
        else
            StringSet.elements !return_list
    in
        return_list := return_backup ;
         return

let compile (Program(macros,network)) config name =
    
        
    (* Add the macros to the symbol table *)
    List.iter (fun ((Macro(name,params,stmts)) as m) ->
                    Hashtbl.add symbol_table name (MacroContainer(m))) macros
    ;
    match network with
        | Network((Parameters(params)),({stmt=(Block(b)); id=ast_id})) -> begin
            (*Add params to symbol_table*)
            List.iter (fun (Param(n,t)) ->
                try
                    let ((size,_) as a_val) : size = List.assoc n config in
                    let fake = match t with
                        | String -> AbstractString(make_string size)
                        | _ -> NoValue
                    in
                    Hashtbl.add symbol_table n (Variable(n,t,Some (AbstractValue(a_val,n,fake))))
                with Not_found ->
                    raise (Config_error(Printf.sprintf "No configuration provided for variable \"%s\".\n" n))
            ) params ;
            let seed = new_seed () in
            let return = List.mapi (fun i s ->
                    net := !(Automata.create (Printf.sprintf "%s_%d" name i) "") ;
                    (*let start_str = Printf.sprintf "start_%d" (get_num seed) in
                    let start = Automata.STE(start_str,"@",false, Automata.AllStart,false,[],false) in
                    let _ = Automata.add_element net start in*)
                    match s.stmt with
                        | SomeStmt((Param(name,t)),source,f) ->
                            begin
                            let obj = cgen_expression source None None "" (new_seed ()) in
                            match obj with
                            | AbstractExp((range_list,abstract_typ),pre,t,fake) ->
                                let add_to_net n =
                                    begin
                                    let new_pre = Printf.sprintf "%s[dynamic_array_index+%d]" pre n in
                                    begin
                                    match t with
                                        | String ->
                                            let c = match fake with
                                            | AbstractString(s) -> String.get s n
                                            | _ -> failwith "should be unreachable"
                                            in
                                            Hashtbl.add symbol_table name (Variable(name,Char,Some (AbstractChar(new_pre,c))))
                                        | Array(x) ->
                                            let (Config.ArrayInfo((new_size_int,_) as new_size)) = abstract_typ in
                                            let fake = match x with
                                                | String -> AbstractString(make_string new_size_int)
                                                | _ -> fake
                                            in
                                            Hashtbl.add symbol_table name (Variable(name,x,Some (AbstractValue(new_size,new_pre,fake))))
                                    end ;
                                    (* FIXME is throwing away break correct? *)
                                    let (return,_) = cgen_statement f ([],[]) "" ~start_automaton:true in
                                    (*remove binding*)
                                    Hashtbl.remove symbol_table name
                                    end
                                in
                                let rec tiling_optimizer num_blocks n =
                                    let net_back = Automata.clone net in
                                    let mapping_back = Hashtbl.copy !abstract_mapping in
                                    add_to_net n ;
                                    (*Minimize*)
                                    Automata.clear_parents net ;
                                    Automata.generate_parents net ;
                                    Opt.remove_dead_states net !abstract_mapping ;
                                    let new_blocks = Opt.get_blocks net in
                                    Printf.printf "Number of blocks needed now: %d\n" new_blocks;
                                    if new_blocks > num_blocks then
                                        begin
                                        abstract_mapping := mapping_back ;
                                        net_back
                                        end
                                    else
                                        tiling_optimizer num_blocks (n+1);
                                in
                                add_to_net 0 ;
                                if !do_tiling then
                                    let num_blocks = Opt.get_blocks net in
                                    Printf.printf "Number of initial blocks: %d\n" num_blocks;
                                    tiling_optimizer (num_blocks) 1
                                else
                                    Automata.clone net
                            | _ -> cgen_statement s ([],[]) "" ; Automata.clone net
                            end
                        | _ -> cgen_statement s ([],[]) "" ; Automata.clone net
                    ) b in
                Hashtbl.iter (fun k v ->
                    Printf.printf "%s -> %s\n" k v
                ) !abstract_mapping ;
                (*Minimize the automata*)
                List.iter Automata.clear_parents return ;
                List.iter Automata.generate_parents return ;
                let return2 = List.map (fun anml -> Opt.remove_dead_states anml !abstract_mapping; anml) return in
                (return2,!abstract_mapping)
                end




