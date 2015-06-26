(*
 * Kevin Angstadt
 * Compiler for AP Language
 *)
 
open Language (* Contains all the types for the AP language *)
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
        | AbstractValue(_,s)
        | AbstractChar(s) -> s

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


let evaluate_report last=
    Automata.set_report net last true

let is_counter exp =
    exp.expr_type = Counter
            
(*let is_counter_expression exp =
    match exp with
        | EQ(a,b)
        | NEQ(a,b)
        | LEQ(a,b)
        | GEQ(a,b)
        | LT(a,b)
        | GT(a,b) -> is_counter a || is_counter b
        | _ -> false*)
        
let rec add_all net (e:Automata.element) =
    let connect = Automata.get_connections e in
    if not (Automata.contains net e) then
        Automata.add_element net e ;
        List.iter (fun (a,c) -> add_all net a) connect

let rec find_last elements =
    if List.for_all (fun e -> (Automata.get_connections e) = []) elements then
        elements
    else
        let new_es = List.fold_left (fun list e ->
            (List.map (fun (c,_) -> c) (Automata.get_connections e)) @ list
        ) [] elements in
        find_last new_es

let rec evaluate_statement ?(start_automaton=false) (stmt : statement) (last : string list) (label : string) : string list =
    match stmt with
        | Report -> List.iter (fun s->return_list := StringSet.add s !return_list; evaluate_report s) last ; last
        | Block(b) ->
            let start_of_automaton = ref ((Automata.is_start_empty net) || start_automaton) in
            List.fold_left (fun last a ->
                let return = evaluate_statement a last label ~start_automaton:!start_of_automaton in
                start_of_automaton := false ;
                return
            ) last b
        | If(exp,then_clause,_)
        | While(exp,then_clause) ->
            begin
            let id = Printf.sprintf "%s_if_%d" label (get_num if_seed) in
            let states : (string,Automata.element) Hashtbl.t = Hashtbl.create 255 in
            let tb = ref StringSet.empty in
            let fb = ref StringSet.empty in
            let rec flip_symbol (Automata.STE(id,set,neg,strt,latch,connect,report) as ste) =
                begin
                let _ = Hashtbl.add states id ste in
                let connection_list : Automata.element_connections list = match connect with
                    | hd :: tl -> List.map (fun (a,conn) -> ((flip_symbol a),conn)) connect
                    | [] -> tb := StringSet.add id !tb ; [] in
                let new_ste = Automata.STE("n_"^id,set,(not neg),strt,latch,connection_list,report) in
                let _ = Hashtbl.add states ("n_"^id) new_ste in
                begin try
                let mapped = Hashtbl.find !abstract_mapping id in
                    Hashtbl.add !abstract_mapping ("n_"^id) mapped
                with Not_found -> ()
                end
                ;
                new_ste
                end in
            let rec add_conn (Automata.STE(id,set,neg,strt,latch,connect,report) as ste) : Automata.element =
                begin
                let new_cons = List.map (fun ((Automata.STE(a_id,a_set,a_neg,a_strt,a_latch,a_connect,a_report) as a),conn) ->
                                        ((Hashtbl.find states ("n_"^a_id)),None)) connect in
                let old_cons = match connect with
                    | hd :: tl -> List.map (fun (a,con) -> ((add_conn a),con)) connect
                    | [] -> [] in
                Automata.STE(id,set,neg,strt,latch,new_cons@old_cons,report)
                end in
            let rec rename (Automata.STE(id,set,neg,strt,latch,connect,report) as ste) : Automata.element =
                begin
                let connection_list : Automata.element_connections list = match connect with
                    | hd :: tl -> List.map (fun (a,conn) -> ((rename a),conn)) connect
                    | [] -> [] in
                let new_ste = Automata.STE("nn_"^id,set,neg,strt,latch,connection_list,report) in
                let _ = Hashtbl.add states ("nn_"^id) new_ste in
                begin try
                let mapped = Hashtbl.find !abstract_mapping id in
                    Hashtbl.add !abstract_mapping ("n_"^id) mapped
                with Not_found -> ()
                end
                ;
                new_ste
                end in
            let rec add_conn_neg (Automata.STE(id,set,neg,strt,latch,connect,report) as ste) =
                begin
                match connect with
                    | hd :: tl -> List.iter (fun (a,con) ->
                        Automata.connect net id ("n"^(Automata.get_id a)) None ;
                        Automata.connect net ("n"^id) (Automata.get_id a) None ;
                        (add_conn_neg a)
                        ) connect
                    | [] -> fb := StringSet.add ("n"^id) (StringSet.add id !fb) ; ()
                end in
            
            let else_clause = match stmt with
                | If(_,_,e) -> e
                | While(_,_) -> Block([])
            in
            match evaluate_expression exp None None id (new_seed ()) with
            | CounterExp(c_list,n_list,yes,no) ->
            (*if this is a counter experession*)
                begin
                (*Add in traps!*)
                let yes_trigger = Automata.STE(Printf.sprintf "%s_t" yes,"\\x26",false,Automata.NotStart,false,[],false) in
                let no_trigger = Automata.STE(Printf.sprintf "%s_f" no,"\\x26",false,Automata.NotStart,false,[],false) in
                Automata.add_element net yes_trigger ;
                Automata.add_element net no_trigger ;
                Automata.connect net yes (Automata.get_id yes_trigger) None;
                Automata.connect net no (Automata.get_id no_trigger) None;
                List.iter2 (fun c n -> Automata.set_count net c n ) c_list n_list ;
                let true_last = evaluate_statement then_clause [(Automata.get_id yes_trigger)] label in
                let false_last = evaluate_statement else_clause [(Automata.get_id no_trigger)] label in
                true_last @ false_last
                end
            (*this is a regular if*)
            | AutomataExp(if_exp) ->
                begin
                let start_of_automaton = (Automata.is_start_empty net) || start_automaton in
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
                let true_last = evaluate_statement then_clause (StringSet.elements !tb) label in
                let false_last = evaluate_statement else_clause (StringSet.elements !fb) label in
                    match stmt with
                    | While(_,_) -> List.iter (fun e ->
                                    List.iter (fun s -> Automata.connect net s (Automata.get_id e) None ;
                                                        Automata.connect net s ("n_"^(Automata.get_id e)) None
                                                        ) true_last
                                    ) if_exp ; (*true_last @*) false_last
                    | If(_,_,_) -> true_last @ false_last
                end
            | BooleanExp(b) ->
                if b then evaluate_statement then_clause last label
                else evaluate_statement else_clause last label
            end
        | Whenever(exp,stmt) ->
            begin
            let id = Printf.sprintf "%s_if_%d" label (get_num if_seed) in
            let start_of_automaton = (Automata.is_start_empty net) || start_automaton in
            Printf.printf "start_of_automaton = %b\n" start_of_automaton ;
            let exp_return = evaluate_expression exp None None id (new_seed ()) in
            match exp_return with
                | AutomataExp(e_list) -> 
                    let end_of_exp = find_last e_list in
                    let spin = Automata.STE("spin_"^id, "*", false, Automata.NotStart, false, [], false) in
                    Automata.add_element net spin ;
                    (*Connect spin to itself...so that it spins!*)
                    Automata.connect net (Automata.get_id spin) (Automata.get_id spin) None ;
                    List.iter (fun e ->
                        add_all net e ;
                        (*Add connections from the spin guard to the exp guard*)
                        Automata.connect net (Automata.get_id spin) (Automata.get_id e) None ;
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
                    evaluate_statement stmt (List.map Automata.get_id end_of_exp) label
                | CounterExp(c_list,n_list,yes,no) ->
                    List.iter2 (fun c n -> Automata.set_count net c n ) c_list n_list ;
                    evaluate_statement stmt [yes] label
            end
        | Either(statement_blocks) ->
            begin
            let start_of_automaton = (Automata.is_start_empty net) || start_automaton in
            List.fold_left( fun complete stmt ->
                let terminals = evaluate_statement stmt last label ~start_automaton:start_of_automaton in
                complete @ terminals
            ) [] statement_blocks
            end
        | SomeStmt((Param(name,t)),source,f) ->
            begin
            let start_of_automaton = (Automata.is_start_empty net) || start_automaton in
            let obj = evaluate_expression source None None label (new_seed ()) in
            match obj with
            | StringExp(s) ->
                let s = explode s in
                    List.fold_left (fun new_last c ->
                        let c = CharValue(c) in
                        (*set binding to 'name'*)
                        Hashtbl.add symbol_table name (Variable(name,Char,Some c)) ;
                        let return = evaluate_statement f last label ~start_automaton:start_of_automaton in
                        (*remove binding*)
                        Hashtbl.remove symbol_table name ; new_last @ return
                    ) last s
            | ArrayExp(a) ->
                Array.fold_left (fun new_last (Some c) ->
                    Hashtbl.add symbol_table name (Variable(name,Char,Some c)) ;
                    let return = evaluate_statement f last label ~start_automaton:start_of_automaton in
                    (*remove binding*)
                    Hashtbl.remove symbol_table name ; new_last @ return
                ) last a
            | AbstractExp((range_list,abstract_typ),pre,t) ->
                let n = match List.hd range_list with
                    | SingleValue(n) -> n
                    | _ -> raise (Config_error("Ranges not supported at this time!"))
                in
                let n_array = Array.init n (fun n -> n) in
                Array.fold_left (fun new_last n ->
                    let new_pre = Printf.sprintf "%s_%d_" pre n in
                    begin
                    match t with
                        | String ->
                            Hashtbl.add symbol_table name (Variable(name,Char,Some (AbstractChar(new_pre))))
                        | Array(x) ->
                            let (Config.ArrayInfo(new_size)) = abstract_typ in
                            Hashtbl.add symbol_table name (Variable(name,x,Some (AbstractValue(new_size,new_pre))))
                    end ;
                    let return = evaluate_statement f last label ~start_automaton:start_of_automaton in
                    (*remove binding*)
                    Hashtbl.remove symbol_table name ; new_last @ return
                                       
                ) last n_array
                
            (* TODO add some sort of array exp*)
            
            end
        | ForEach((Param(name,t)),source,f) ->
            begin
            let start_of_automaton = ref ((Automata.is_start_empty net) || start_automaton) in
            let obj = evaluate_expression source None None label (new_seed ()) in
            match obj with
            | StringExp(s) ->
                let s = explode s in
                    List.fold_left (fun last c ->
                        let c = CharValue(c) in
                        (*set binding to 'name'*)
                        Hashtbl.add symbol_table name (Variable(name,Char,Some c)) ;
                        let return = evaluate_statement f last label ~start_automaton:!start_of_automaton in
                        (*remove binding*)
                        Hashtbl.remove symbol_table name ; start_of_automaton := false ; return
                    ) last s
            | ArrayExp(a) ->
                Array.fold_left (fun last (Some c) ->
                    Hashtbl.add symbol_table name (Variable(name,Char,Some c)) ;
                    let return = evaluate_statement f last label ~start_automaton:!start_of_automaton in
                    (*remove binding*)
                    Hashtbl.remove symbol_table name ; start_of_automaton := false ; return
                ) last a
            | AbstractExp((range_list,abstract_typ),pre,t) ->
                let n = match List.hd range_list with
                    | SingleValue(n) -> n
                    | _ -> raise (Config_error("Ranges not supported at this time!"))
                in
                let n_array = Array.init n (fun n -> n) in
                Array.fold_left (fun last n ->
                    let new_pre = Printf.sprintf "%s[%d]" pre n in
                    begin
                    match t with
                        | String ->
                            Hashtbl.add symbol_table name (Variable(name,Char,Some (AbstractChar(new_pre))))
                        | Array(x) ->
                            let (Config.ArrayInfo(new_size)) = abstract_typ in
                            Hashtbl.add symbol_table name (Variable(name,x,Some (AbstractValue(new_size,new_pre))))
                    end ;
                    let return = evaluate_statement f last label ~start_automaton:!start_of_automaton in
                    (*remove binding*)
                    Hashtbl.remove symbol_table name ; start_of_automaton := false ; return
                                       
                ) last n_array
            (* TODO add some sort of array exp*)
            end
        | VarDec(var) ->
            let rec gen_value init =
                match init with
                    | PrimitiveInit(e) ->
                        let v = evaluate_expression e None None label (new_seed ()) in
                        begin
                        match v with
                            | BooleanExp(b) -> Some (BooleanValue(b))
                            | IntExp(b) -> Some (IntValue(b))
                            | StringExp(s) -> Some (StringValue(s))
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
                        let counter = Automata.Counter(id,100,Automata.Latch,false,[]) in
                        let inverter = Automata.Combinatorial(Inverter,i_id, false, false, []) in
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
                        let counter1 = Automata.Counter(id1,100,Automata.Latch,false,[]) in
                        let counter2 = Automata.Counter(id2,100,Automata.Latch,false,[]) in
                        let inverter1 = Automata.Combinatorial(Inverter,i_id1, false, false, []) in
                        let inverter2 = Automata.Combinatorial(Inverter,i_id2, false, false, []) in
                        let out_true = if is_eq then
                            Automata.Combinatorial(AND,id_true,false,false,[])
                        else
                            Automata.Combinatorial(OR,id_true,false,false,[])
                        in
                        let out_false = if is_eq then
                            Automata.Combinatorial(AND,id_false,false,false,[])
                        else
                            Automata.Combinatorial(OR,id_false,false,false,[])
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
            
            last (*TODO OMG this will not work correctly.  So much error checking needed*)
            end
        (*| ExpStmt(e,scope) -> match e with None -> () | Some x -> Automata.add_element net (evaluate_expression x None)*) (*TODO check to see if the expression is allowed as a statement*)
        | MacroCall(a,Arguments(b)) -> begin
            (* Look up macro in the symbol table, make sure it is there *)
            (* TODO does this need to be error-checked? *)
            let start_of_automaton = (Automata.is_start_empty net) || start_automaton in
            let (MacroContainer(Macro(name,Parameters(params),stmts) as m)) = Hashtbl.find symbol_table a in
                (*evaluate macro*)
                evaluate_macro m b last ~start_automaton:start_of_automaton
            end
        | ExpStmt(e) ->
            begin
            let start_of_automaton = (Automata.is_start_empty net) || start_automaton in
            match e.expr_type with
                | Automata ->
                    
                    let (AutomataExp(e_list)) = evaluate_expression e (Some (List.map (fun l -> Automata.get_element net l) last)) None (Printf.sprintf "%s_exp_%d" label (get_num e_seed)) (new_seed ()) in
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
                    if not (StringSet.is_empty new_last) then StringSet.elements new_last else last 
                | _ ->
                    begin
                    let return = evaluate_expression e (Some (List.map (fun l -> Automata.get_element net l) last)) None label (new_seed ()) in
                    match return with
                        | BooleanExp(false) -> []
                        | _ -> last
                    end
            end
        | _ -> Printf.printf "Oh goodness! %s" (statement_to_str stmt) ; raise (Syntax_error "unimplemented method")
and evaluate_expression (exp : expression) (before : Automata.element list option) (s : Automata.element list option) (prefix : string) (seed : id_seed) =
    (*get the type of an expression...needed to determine how to eval the exp*)
    match exp.expr_type with
        | Boolean -> BooleanExp(evaluate_boolean_expression exp)
        | DoubleCounter(_)
        | Counter ->
         CounterExp(evaluate_counter_expression exp)
        | Automata -> AutomataExp(evaluate_expression_aut exp before s prefix seed)
        | Int -> IntExp(evaluate_int_expression exp)
        | Char -> CharExp(evaluate_char_expression exp)
        | String -> evaluate_string_expression exp
        | Array(_) -> evaluate_array_expression exp
and evaluate_expression_aut (exp : expression) (before : Automata.element list option) (s: Automata.element list option) (prefix:string) (seed:id_seed) : Automata.element list =
    (*Helper...requires type checking first or will result in a syntax error*)
    let id = Printf.sprintf "%s_%d" prefix (get_num seed) in
    let get_value v =
        begin
        match v.exp with
            | Lit(CharLit(a,_)) -> a
            | Lit(AllIn) -> Automata.all_in
            | Lit(StartIn) -> Automata.start_in
            | Lval((n,o)) ->
                let (Variable(name,typ,Some v)) = Hashtbl.find symbol_table n in
                match o with
                | NoOffset ->
                    begin match v with
                    | CharValue(s) -> s
                    | AbstractChar(s) ->
                        Hashtbl.add !abstract_mapping id s ;
                        Char.chr ((Random.int 26) + 97)
                    end
        end in
    match exp.exp with
        | EQ(a,b) -> begin
            let helper (Automata.STE(id,set,neg,strt,latch,connect,report) as new_element) = begin
                match s with
                    | None -> [new_element]
                    | Some x ->
                        let cons = List.map (fun a -> (a,None)) x in
                        [Automata.STE(id,set,neg,strt,latch,cons@connect,report)]
            end in
            if a.exp = Input then
                let new_element = Automata.STE(id,Char.escaped (get_value b),false,Automata.NotStart,false,[],false) in
                helper new_element
            else if b.exp = Input then
                let new_element = Automata.STE(id,Char.escaped (get_value a),false,Automata.NotStart,false,[],false) in
                helper new_element

            else raise (Syntax_error "Something with Input")
            end
        | NEQ(a,b) -> begin (* TODO This is just copied and pasted from above; need to fix this *)
            let helper (Automata.STE(id,set,neg,strt,latch,connect,report) as new_element) = begin
                match s with
                    | None -> [new_element]
                    | Some x -> 
                        let cons = List.map (fun a -> (a,None)) x in
                        [Automata.STE(id,set,neg,strt,latch,cons@connect,report)]
            end in
            if a.exp = Input then
                let new_element = Automata.STE(id,Char.escaped (get_value b),true,Automata.NotStart,false,[],false) in
                helper new_element
            else if b.exp = Input then
                let new_element = Automata.STE(id,Char.escaped (get_value a),true,Automata.NotStart,false,[],false) in
                helper new_element
            else raise (Syntax_error "Something with Input")
            end
        (*| LEQ(a,b)
        | LT(a,b)
        | GT(a,b)*)
        | Not(a) -> (* TODO This is a copied and modified from the if-statement.  Need to combine/consolidate *)
            begin
            let temp_net = Automata.create "tmp" "" in
            let states : (string,Automata.element) Hashtbl.t = Hashtbl.create 255 in
            let rec flip_symbol (Automata.STE(id,set,neg,strt,latch,connect,report) as ste) =
                begin
                let _ = Hashtbl.add states id ste in
                let connection_list : Automata.element_connections list = match connect with
                    | hd :: tl -> List.map (fun (a,conn) -> ((flip_symbol a),conn)) connect
                    | [] -> [] in
                let new_ste = Automata.STE("n_"^id,set,(not neg),strt,latch,connection_list,report) in
                let _ = Hashtbl.add states ("n_"^id) new_ste in
                begin try
                let mapped = Hashtbl.find !abstract_mapping id in
                    Hashtbl.add !abstract_mapping ("n_"^id) mapped
                with Not_found -> ()
                end
                ;
                new_ste
                end in
            let rec add_conn (Automata.STE(id,set,neg,strt,latch,connect,report) as ste) : Automata.element =
                begin
                let new_cons = List.map (fun ((Automata.STE(a_id,a_set,a_neg,a_strt,a_latch,a_connect,a_report) as a),conn) ->
                                        ((Hashtbl.find states ("n_"^a_id)),None)) connect in
                let old_cons = match connect with
                    | hd :: tl -> List.map (fun (a,con) -> ((add_conn a),con)) connect
                    | [] -> [] in
                Automata.STE(id,set,neg,strt,latch,new_cons@old_cons,report)
                end in
            let rec rename (Automata.STE(id,set,neg,strt,latch,connect,report) as ste) : Automata.element =
                begin
                let connection_list : Automata.element_connections list = match connect with
                    | hd :: tl -> List.map (fun (a,conn) -> ((rename a),conn)) connect
                    | [] -> [] in
                let new_ste = Automata.STE("nn_"^id,set,neg,strt,latch,connection_list,report) in
                let _ = Hashtbl.add states ("nn_"^id) new_ste in
                begin try
                let mapped = Hashtbl.find !abstract_mapping id in
                    Hashtbl.add !abstract_mapping ("n_"^id) mapped
                with Not_found -> ()
                end
                ;
                new_ste
                end in
            let rec add_conn_neg (Automata.STE(id,set,neg,strt,latch,connect,report) as ste) =
                begin
                match connect with
                    | hd :: tl -> List.iter (fun (a,con) ->
                        Automata.connect temp_net id ("n"^(Automata.get_id a)) None ;
                        Automata.connect temp_net ("n"^id) (Automata.get_id a) None ;
                        (add_conn_neg a)
                        ) connect
                    | [] -> ()
                end in
            (* TODO This None might actually bad...will have to consider further
                It should be a "last" element.  Probably have to add it in later
                (after the fold)
            *)
            let if_exp = evaluate_expression_aut a None None prefix seed in
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
                    | Automata.STE(id,set,neg,strt,latch,connect,report) ->
                        begin
                        match connect with
                        | hd :: tl -> Automata.STE(id,set,neg,strt,latch, List.map (fun (a,s) -> ((add_to_last a x),s)) connect, report)
                        | [] ->
                            let temp = List.map (fun a -> (a,None)) x in
                            Automata.STE(id,set,neg,strt,latch,temp,report)
                        end
                    | Automata.Combinatorial(typ,id,eod,report,connect) ->
                        begin
                        match connect with
                        | hd :: tl -> Automata.Combinatorial(typ, id, eod, report, List.map (fun (a,s) -> ((add_to_last a x),s)) connect)
                        | [] ->
                            let temp = List.map (fun a -> (a,None)) x in
                            Automata.Combinatorial(typ,id,eod,report,temp)
                        end
                        
            in
            let b_eval = evaluate_expression_aut b None None prefix seed in
            let a_eval = evaluate_expression_aut a None (Some b_eval) prefix seed in
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
            let join = Automata.Combinatorial(Automata.AND,Printf.sprintf "%s_and_%d" prefix (get_num seed), false, false, connect) in
            let a_eval = evaluate_expression_aut a None (Some [join]) prefix seed in
            let b_eval = evaluate_expression_aut b None (Some [join]) prefix seed in
                a_eval@b_eval
        | Or(a,b) ->
            (*TODO Do we do this here, or at a later optimization stage?*)
            let rec build_charset c =
                match c.exp with
                    | EQ(a,b)
                    | NEQ(a,b) ->
                        if a.exp = Input then Char.escaped (get_value b)
                        else if b.exp = Input then Char.escaped (get_value a)
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
                    in [Automata.STE(id,Printf.sprintf "%s" ((build_charset a)^(build_charset b)),false, Automata.NotStart, false, connect, false)]
                else
                let b_eval = evaluate_expression_aut b None s prefix seed in
                    let connect = match s with
                        | None -> []
                        | Some x -> List.map (fun a -> (a,None)) x
                    in Automata.STE(id,Printf.sprintf "%s" ((build_charset a)),false, Automata.NotStart, false, connect, false) :: b_eval
            else
                if can_condense b then
                let a_eval = evaluate_expression_aut a None s prefix seed in
                    let connect = match s with
                        | None -> []
                        | Some x -> List.map (fun a -> (a,None)) x
                    in Automata.STE(id,Printf.sprintf "%s" ((build_charset b)), false, Automata.NotStart, false, connect, false) :: a_eval
                else
                    let a_eval = evaluate_expression_aut a None s prefix seed in
                    let b_eval = evaluate_expression_aut b None s prefix seed in
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
                            List.iter ( fun c -> 
                                List.iter (fun l -> Automata.connect net (Automata.get_id l) c (Some "cnt")) last
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
            let yes_and = Automata.Combinatorial(Automata.AND,yes,false,false,[]) in
            let no_or = Automata.Combinatorial(Automata.OR,no,false,false,[]) in
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
            let yes_or = Automata.Combinatorial(Automata.OR,yes,false,false,[]) in
            let no_and = Automata.Combinatorial(Automata.AND,no,false,false,[]) in
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
            let (Variable(n,t,(Some (StringValue(v))))) = symbol_variable_lookup a in
                begin match b with
                    | "length" -> String.length v
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

and evaluate_string_expression exp =
    match exp.exp with
        | Lval((a,o)) ->
            let Variable(n,t,(Some v)) = symbol_variable_lookup a in
                begin
                match v with
                    | StringValue(s) -> StringExp(s)
                    | AbstractValue(s,pre) -> AbstractExp((s,pre,t))
                end
        | Lit(a) -> begin match a with
                      | StringLit(x,_) -> StringExp(x)
                    end

(*TODO Offsets*)
and evaluate_array_expression exp =
    match exp.exp with
        | Lval((a,o)) ->
            let Variable(n,t,(Some v)) = symbol_variable_lookup a in
                match v with
                    | ArrayValue(s) -> ArrayExp(s)
                    | AbstractValue(s,pre) -> AbstractExp((s,pre,t))
                    
and evaluate_macro ?(start_automaton=false) (Macro(name,Parameters(params),stmt)) (args:expression list) (last:string list) =
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
    
    let last = match stmt with
        | Block(b) -> evaluate_statement stmt last s ~start_automaton:start_automaton
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
        | Network((Parameters(params)),(Block(b))) -> begin
            (*Add params to symbol_table*)
            List.iter (fun (Param(n,t)) ->
                try
                    let a_val = List.assoc n config in
                    Hashtbl.add symbol_table n (Variable(n,t,Some (AbstractValue(a_val,n))))
                with Not_found ->
                    raise (Config_error(Printf.sprintf "No configuration provided for variable \"%s\".\n" n))
            ) params ;
            let seed = new_seed () in
            let return = List.mapi (fun i s ->
                    net := !(Automata.create (Printf.sprintf "%s_%d" name i) "") ;
                    (*let start_str = Printf.sprintf "start_%d" (get_num seed) in
                    let start = Automata.STE(start_str,"@",false, Automata.AllStart,false,[],false) in
                    let _ = Automata.add_element net start in*)
                    match s with
                        | SomeStmt((Param(name,t)),source,f) ->
                            begin
                            let obj = evaluate_expression source None None "" (new_seed ()) in
                            match obj with
                            | AbstractExp((range_list,abstract_typ),pre,t) ->
                                let add_to_net n =
                                    begin
                                    let new_pre = Printf.sprintf "%s[dynamic_array_index+%d]" pre n in
                                    begin
                                    match t with
                                        | String ->
                                            Hashtbl.add symbol_table name (Variable(name,Char,Some (AbstractChar(new_pre))))
                                        | Array(x) ->
                                            let (Config.ArrayInfo(new_size)) = abstract_typ in
                                            Hashtbl.add symbol_table name (Variable(name,x,Some (AbstractValue(new_size,new_pre))))
                                    end ;
                                    let return = evaluate_statement f [] "" ~start_automaton:true in
                                    (*remove binding*)
                                    Hashtbl.remove symbol_table name
                                    end
                                in
                                let rec tiling_optimizer num_blocks n =
                                    let net_back = Automata.clone net in
                                    let mapping_back = Hashtbl.copy !abstract_mapping in
                                    add_to_net n ;
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
                            | _ -> evaluate_statement s [] "" ; Automata.clone net
                            end
                        | _ -> evaluate_statement s [] "" ; Automata.clone net
                    ) b in
                Hashtbl.iter (fun k v ->
                    Printf.printf "%s -> %s\n" k v
                ) !abstract_mapping ; return
                end
            
    (*;
    Automata.set_name net name ; net*) (*;
    let return = (Automata.network_to_str net) in
        Printf.printf "pong\n%!" ; return*)

