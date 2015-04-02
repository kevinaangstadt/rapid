(*
 * Kevin Angstadt
 * Compiler for AP Language
 *)
 
open Language (* Contains all the types for the AP language *)
open Id (*For counting on IDs*)
open Util


let symbol_table : symbol = Hashtbl.create 255


let symbol_scope = ref StringSet.empty
let counter_rename : (string,string) Hashtbl.t = Hashtbl.create 10
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

let rec evaluate_statement (stmt : statement) (last : string list) : string list =
    match stmt with
        | Report -> List.iter (fun s->evaluate_report s) last ; last
        | Block(b) -> List.fold_left (fun last a -> evaluate_statement a last) last b
        | If(exp,then_clause,_)
        | While(exp,then_clause) ->
            begin
            let id = Printf.sprintf "if_%d" (get_num if_seed) in
            let states : (string,Automata.element) Hashtbl.t = Hashtbl.create 255 in
            let tb = ref StringSet.empty in
            let fb = ref StringSet.empty in
            let rec flip_symbol (Automata.STE(id,set,strt,latch,connect,report) as ste) =
                begin
                let _ = Hashtbl.add states id ste in
                let connection_list : Automata.element_connections list = match connect with
                    | hd :: tl -> List.map (fun (a,conn) -> ((flip_symbol a),conn)) connect
                    | [] -> tb := StringSet.add id !tb ; [] in
                let new_ste = if (String.get set 0) = '^' then
                        Automata.STE("n_"^id,String.sub set 1 ((String.length set) - 1 ),strt,latch,connection_list,report)
                    else Automata.STE("n_"^id,"^"^set,strt,latch,connection_list,report) in
                let _ = Hashtbl.add states ("n_"^id) new_ste in
                new_ste
                end in
            let rec add_conn (Automata.STE(id,set,strt,latch,connect,report) as ste) : Automata.element =
                begin
                let new_cons = List.map (fun ((Automata.STE(a_id,a_set,a_strt,a_latch,a_connect,a_report) as a),conn) ->
                                        ((Hashtbl.find states ("n_"^a_id)),None)) connect in
                let old_cons = match connect with
                    | hd :: tl -> List.map (fun (a,con) -> ((add_conn a),con)) connect
                    | [] -> [] in
                Automata.STE(id,set,strt,latch,new_cons@old_cons,report)
                end in
            let rec rename (Automata.STE(id,set,strt,latch,connect,report) as ste) : Automata.element =
                begin
                let connection_list : Automata.element_connections list = match connect with
                    | hd :: tl -> List.map (fun (a,conn) -> ((rename a),conn)) connect
                    | [] -> [] in
                let new_ste = Automata.STE("nn_"^id,set,strt,latch,connection_list,report) in
                let _ = Hashtbl.add states ("nn_"^id) new_ste in
                new_ste
                end in
            let rec add_conn_neg (Automata.STE(id,set,strt,latch,connect,report) as ste) =
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
            | CounterExp(c,n,if_exp,yes,no,loop) ->
            (*if this is a counter experession*)
                begin
                (*(c, (create (num+1) None), StringSet.of_list tb, StringSet.of_list fb)*)
                Automata.add_element net (Automata.STE(c^"_trap","$",Automata.NotStart,false,[],false)) ;
                Automata.add_element net (Automata.STE("n_"^c^"_trap","^$",Automata.NotStart,false,[],false)) ;
                Automata.add_element net (Automata.STE(c^"_trap_t","\\x26",Automata.NotStart,false,[],false)) ;
                add_all net if_exp ;
                List.iter (fun a -> Automata.connect net a (c^"_trap") None ; Automata.connect net a ("n_"^c^"_trap") None) last ;
                Automata.connect net loop loop None ;
                Automata.connect net c (Automata.get_id if_exp) None ;
                Automata.connect net (c^"_trap") (c^"_trap")  None ;
                Automata.connect net ("n_"^c^"_trap") ("n_"^c^"_trap") None ;
                Automata.connect net (c^"_trap") (c^"_trap_t") None ;
                Automata.connect net ("n_"^c^"_trap") (c^"_trap") None ;
                Automata.connect net (c^"_trap") c (Some "cnt") ;
                (*Automata.connect net ("n_"^c^"_trap") c (Some "cnt") ;*)
                tb := yes ;
                fb := StringSet.add (c^"_trap_t") no ;
                Automata.set_count net c n ;
                let true_last = evaluate_statement then_clause (StringSet.elements !tb) in
                let false_last = evaluate_statement else_clause (StringSet.elements !fb) in
                true_last @ false_last
                end
            (*this is a regular if*)
            | AutomataExp(if_exp) ->
                begin
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
                    ) last
                ) if_exp ;
                let true_last = evaluate_statement then_clause (StringSet.elements !tb) in
                let false_last = evaluate_statement else_clause (StringSet.elements !fb) in
                    match stmt with
                    | While(_,_) -> List.iter (fun e ->
                                    List.iter (fun s -> Automata.connect net s (Automata.get_id e) None ;
                                                        Automata.connect net s ("n_"^(Automata.get_id e)) None
                                                        ) true_last
                                    ) if_exp ; (*true_last @*) false_last
                    | If(_,_,_) -> true_last @ false_last
                end
            | BooleanExp(b) ->
                if b then evaluate_statement then_clause last
                else evaluate_statement else_clause last
            end
        | Either(statement_blocks) ->
            begin
            List.fold_left( fun complete stmt ->
                let terminals = evaluate_statement stmt last in
                complete @ terminals
            ) [] statement_blocks
            end
        | ForEach((Param((name,o),t)),source,f) ->
            begin
            let obj = evaluate_expression source None None "" (new_seed ()) in
            match obj with
            | StringExp(s) ->
                let s = explode s in
                    List.fold_left (fun last c ->
                        let c = CharValue(c) in
                        (*set binding to 'name'*)
                        Hashtbl.add symbol_table name (Variable(name,Char,Some c)) ;
                        let return = evaluate_statement f last in
                        (*remove binding*)
                        Hashtbl.remove symbol_table name ; return
                    ) last s
            (* TODO add some sort of array exp*)
            end
        | VarDec(var) ->
            begin
            List.iter (fun ((s,o),t,init) ->
                (*TODO support list*)
                let (id,value) =
                    match t with
                    | Counter ->
                        let num = get_num c_seed in
                        let id = Printf.sprintf "%s_%d" s num in
                        let _ = Hashtbl.add counter_rename s id in
                        let _ = Hashtbl.add symbol_table s (Variable(id,t,None)) in
                        symbol_scope := StringSet.add s !symbol_scope ;
                        (id, Some (AutomataElement(Automata.Counter(id,100,Automata.Pulse,false,[]))))
                    | _ ->
                        let x = match init with
                            | None -> None
                            | Some i -> begin match i with
                                | PrimitiveInit(e) ->
                                    let v = evaluate_expression e None None "" (new_seed ()) in
                                    match v with
                                        | BooleanExp(b) -> Some (BooleanValue(b))
                                        | IntExp(b) -> Some (IntValue(b))
                                        | StringExp(s) -> Some (StringValue(s))
                                end
                        in (s,x) in
                (* TODO We need some notion of scope to remove these after the fact! *)
                Hashtbl.add symbol_table id (Variable(id,t,value)) ;
                symbol_scope := StringSet.add id !symbol_scope ;
            ) var ;
            
            last (*TODO OMG this will not work correctly.  So much error checking needed*)
            end
        (*| ExpStmt(e,scope) -> match e with None -> () | Some x -> Automata.add_element net (evaluate_expression x None)*) (*TODO check to see if the expression is allowed as a statement*)
        | MacroCall(a,Arguments(b)) -> begin
            (* Look up macro in the symbol table, make sure it is there *)
            (* TODO does this need to be error-checked? *)
            let (MacroContainer(Macro(name,Parameters(params),stmts) as m)) = Hashtbl.find symbol_table a in
                (*evaluate macro*)
                evaluate_macro m b last
            end
        | ExpStmt(e) ->
            begin
            match e.expr_type with
                | Automata ->
                    let rec find_last elements =
                        if List.for_all (fun e -> (Automata.get_connections e) = []) elements then
                            elements
                        else
                            let new_es = List.fold_left (fun list e ->
                                (List.map (fun (c,_) -> c) (Automata.get_connections e)) @ list
                            ) [] elements in
                            find_last new_es
                    in
                    let (AutomataExp(e_list)) = evaluate_expression e (Some (List.map (fun l -> Automata.get_element net l) last)) None (Printf.sprintf "exp_%d" (get_num e_seed)) (new_seed ()) in
                    let new_last = List.fold_left (fun ss e ->
                        StringSet.add (Automata.get_id e) ss
                    ) StringSet.empty (find_last e_list) in
                    List.iter (fun e -> add_all net e) e_list;
                    List.iter (fun e1 ->
                        List.iter (fun e2 -> Automata.connect net e1 (Automata.get_id e2) None ) e_list
                    ) last ;
                    if not (StringSet.is_empty new_last) then StringSet.elements new_last else last
                | _ ->
                    begin
                    let return = evaluate_expression e (Some (List.map (fun l -> Automata.get_element net l) last)) None "" (new_seed ()) in
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
        | Counter -> CounterExp(evaluate_counter_expression exp)
        | Automata -> AutomataExp(evaluate_expression_aut exp before s prefix seed)
        | Int -> IntExp(evaluate_int_expression exp)
        | Char -> CharExp(evaluate_char_expression exp)
        | String -> StringExp(evaluate_string_expression exp)
and evaluate_expression_aut (exp : expression) (before : Automata.element list option) (s: Automata.element list option) (prefix:string) (seed:id_seed) : Automata.element list =
    (*Helper...requires type checking first or will result in a syntax error*)
    let get_value v =
        begin
        match v.exp with
            | Lit(CharLit(a,_)) -> a
            | Lval((n,o)) ->
                let (Variable(name,typ,Some v)) = Hashtbl.find symbol_table n in
                match o with
                | NoOffset ->
                    begin match v with
                    | CharValue(s) -> s
                    end
        end in
    match exp.exp with
        | EQ(a,b) -> begin
            let helper (Automata.STE(id,set,strt,latch,connect,report) as new_element) = begin
                match s with
                    | None -> [new_element]
                    | Some x ->
                        let cons = List.map (fun a -> (a,None)) x in
                        [Automata.STE(id,set,strt,latch,cons@connect,report)]
            end in
            if a.exp = Input then
                let new_element = Automata.STE((Printf.sprintf "%s_%d" prefix (get_num seed)),Char.escaped (get_value b),Automata.NotStart,false,[],false) in
                helper new_element
            else if b.exp = Input then
                let new_element = Automata.STE((Printf.sprintf "%s_%d" prefix (get_num seed)),Char.escaped (get_value a),Automata.NotStart,false,[],false) in
                helper new_element
            else raise (Syntax_error "Something with Input")
            end
        | NEQ(a,b) -> begin (* TODO This is just copied and pasted from above; need to fix this *)
            let helper (Automata.STE(id,set,strt,latch,connect,report) as new_element) = begin
                match s with
                    | None -> [new_element]
                    | Some x -> 
                        let cons = List.map (fun a -> (a,None)) x in
                        [Automata.STE(id,set,strt,latch,cons@connect,report)]
            end in
            if a.exp = Input then
                let new_element = Automata.STE((Printf.sprintf "%s_%d" prefix (get_num seed)),Printf.sprintf "^%s" (Char.escaped (get_value b)),Automata.NotStart,false,[],false) in
                helper new_element
            else if b.exp = Input then
                let new_element = Automata.STE((Printf.sprintf "%s_%d" prefix (get_num seed)),Printf.sprintf "^%s" (Char.escaped (get_value a)),Automata.NotStart,false,[],false) in
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
            let rec flip_symbol (Automata.STE(id,set,strt,latch,connect,report) as ste) =
                begin
                let _ = Hashtbl.add states id ste in
                let connection_list : Automata.element_connections list = match connect with
                    | hd :: tl -> List.map (fun (a,conn) -> ((flip_symbol a),conn)) connect
                    | [] -> [] in
                let new_ste = if (String.get set 0) = '^' then
                        Automata.STE("n_"^id,String.sub set 1 ((String.length set) - 1 ),strt,latch,connection_list,report)
                    else Automata.STE("n_"^id,"^"^set,strt,latch,connection_list,report) in
                let _ = Hashtbl.add states ("n_"^id) new_ste in
                new_ste
                end in
            let rec add_conn (Automata.STE(id,set,strt,latch,connect,report) as ste) : Automata.element =
                begin
                let new_cons = List.map (fun ((Automata.STE(a_id,a_set,a_strt,a_latch,a_connect,a_report) as a),conn) ->
                                        ((Hashtbl.find states ("n_"^a_id)),None)) connect in
                let old_cons = match connect with
                    | hd :: tl -> List.map (fun (a,con) -> ((add_conn a),con)) connect
                    | [] -> [] in
                Automata.STE(id,set,strt,latch,new_cons@old_cons,report)
                end in
            let rec rename (Automata.STE(id,set,strt,latch,connect,report) as ste) : Automata.element =
                begin
                let connection_list : Automata.element_connections list = match connect with
                    | hd :: tl -> List.map (fun (a,conn) -> ((rename a),conn)) connect
                    | [] -> [] in
                let new_ste = Automata.STE("nn_"^id,set,strt,latch,connection_list,report) in
                let _ = Hashtbl.add states ("nn_"^id) new_ste in
                new_ste
                end in
            let rec add_conn_neg (Automata.STE(id,set,strt,latch,connect,report) as ste) =
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
            let rec add_to_last (Automata.STE(id,set,strt,latch,connect,report) as start) x =
                match connect with
                | hd :: tl -> Automata.STE(id,set,strt,latch, List.map (fun (a,s) -> ((add_to_last a x),s)) connect, report)
                | [] ->
                    let temp = List.map (fun a -> (a,None)) x in
                    Automata.STE(id,set,strt,latch,temp,report)
            in
            let b_eval = evaluate_expression_aut b None None prefix seed in
            let a_eval = evaluate_expression_aut a None (Some b_eval) prefix seed in
            begin
            match s with
                | None -> a_eval
                | Some x -> List.map (fun a -> add_to_last a x) a_eval
            end
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
                    | And(_,_) -> false
                in
            if can_condense a then
                if can_condense b then
                    let connect = match s with
                        | None -> []
                        | Some x -> List.map (fun a -> (a,None)) x
                    in [Automata.STE(Printf.sprintf "%s_%d" prefix (get_num seed),Printf.sprintf "[%s]" ((build_charset a)^(build_charset b)), Automata.NotStart, false, connect, false)]
                else
                let b_eval = evaluate_expression_aut b None s prefix seed in
                    let connect = match s with
                        | None -> []
                        | Some x -> List.map (fun a -> (a,None)) x
                    in Automata.STE(Printf.sprintf "%s_%d" prefix (get_num seed),Printf.sprintf "[%s]" ((build_charset a)), Automata.NotStart, false, connect, false) :: b_eval
            else
                if can_condense b then
                let a_eval = evaluate_expression_aut a None s prefix seed in
                    let connect = match s with
                        | None -> []
                        | Some x -> List.map (fun a -> (a,None)) x
                    in Automata.STE(Printf.sprintf "%s_%d" prefix (get_num seed),Printf.sprintf "[%s]" ((build_charset b)), Automata.NotStart, false, connect, false) :: a_eval
                else
                    let a_eval = evaluate_expression_aut a None s prefix seed in
                    let b_eval = evaluate_expression_aut b None s prefix seed in
                    a_eval @ b_eval
        | Fun((a,_),b,c) ->
            begin match b with
                | "count" ->
                    begin
                    let id = Hashtbl.find counter_rename a in
                    let counter = Hashtbl.find symbol_table id in
                    let last = match before with Some x -> x | _ -> [] in
                    match counter with
                        | Variable(s,t,v) ->
                            let c = match v with
                                | Some AutomataElement(x) -> x in
                            begin
                            if not (Automata.contains net c) then Automata.add_element net c ;
                            List.iter (fun l -> Automata.connect net (Automata.get_id l) s (Some "cnt")) last ; []
                            end
                            
                    end
                | "reset" ->
                    begin
                    let id = Hashtbl.find counter_rename a in
                    let counter = Hashtbl.find symbol_table id in
                    let last = match before with Some x -> x | _ -> [] in
                    match counter with
                        | Variable(s,t,v) ->
                            let c = match v with
                                | Some AutomataElement(x) -> x in
                            (*Check that we have a counter*)
                            begin
                            if not (Automata.contains net c) then Automata.add_element net c ;
                            List.iter (fun l -> Automata.connect net (Automata.get_id l) s (Some "rst")) last ; []
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
                let (Variable(name,typ, Some (AutomataElement(e)))) = Hashtbl.find symbol_table id in
                    Automata.get_id e
                
        end in
    let helper num c =
        begin
        let rec create i (e : Automata.element option) =
            begin
            if i < 0 then
                match e with
                    | Some x -> x
                    | None -> raise Negative_count
            else
                let id = Printf.sprintf "%s_%d" c i in
                let trigger = Automata.STE(id^"_t","\\x26",Automata.NotStart,false,[],false) in
                let connect = match e with
                    | Some x -> [(trigger,None);(x,None)]
                    | None -> [(trigger,None)] in
                    let ctr = Automata.STE(id,"$",Automata.NotStart,false,connect,false) in
                    create (i - 1) (Some ctr)
            end in
        let state_list low high = List.map (fun a -> Printf.sprintf "%s_%d_t" c a) (range low high) in
        let (tb,fb) = match exp.exp with
            | EQ(_,_) -> ( state_list num (num + 1) , state_list 0 num @ state_list (num + 1) (num + 2) )
            | GT(_,_) -> ( state_list (num + 1) (num + 2) , state_list 0 (num + 1) )
            | GEQ(_,_) -> ( state_list num (num + 2) , state_list 0 num )
            | LEQ(_,_) -> ( state_list 0 (num+1) , state_list (num + 1) (num + 2) )
            | LT(_,_) ->( state_list 0 num , state_list num (num + 2) ) in
        
        (c, (num+2), (create (num+1) None), StringSet.of_list tb, StringSet.of_list fb, Printf.sprintf "%s_%d" c (num+1))
        end in
    (*if counter is not listed first, flip it!*)
    match exp.exp with
        | EQ(a,b) -> if is_counter b then evaluate_counter_expression ({ exp with exp = EQ(b,a); })
                     else
                        helper (evaluate_int_expression b) (get_counter a)
        | GT(a,b) -> if is_counter b then evaluate_counter_expression ({ exp with exp = LEQ(b,a); })
                     else
                        helper (evaluate_int_expression b) (get_counter a)
        | GEQ(a,b) -> if is_counter b then evaluate_counter_expression ({ exp with exp = LT(b,a); })
                      else
                        helper (evaluate_int_expression b) (get_counter a)
        | LEQ(a,b) -> if is_counter b then evaluate_counter_expression ({ exp with exp = GT(b,a); })
                      else
                        helper (evaluate_int_expression b) (get_counter a)
        | LT(a,b) -> if is_counter b then evaluate_counter_expression ({ exp with exp = GEQ(b,a); })
                     else
                        helper (evaluate_int_expression b) (get_counter a)

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
                    end

and evaluate_string_expression exp =
    match exp.exp with
        | Lval((a,o)) ->
            let Variable(n,t,(Some (StringValue(s)))) = symbol_variable_lookup a in
                s
        | Lit(a) -> begin match a with
                      | StringLit(x,_) -> x
                    end
                    
and evaluate_macro (Macro(name,Parameters(params),stmt)) (args:expression list) (last:string list) =
    (* add bindings for arguments to state *)
    List.iter2 (fun (Param((p,o),t)) arg ->
        let value =
            begin
            (*TODO allow for offsets*)
            match arg.exp with
                | Lval((s,o2)) ->
                    let variable = Hashtbl.find symbol_table s in
                    begin
                    match variable with
                        | Variable(name,t,value) -> value
                        | _ -> raise (Syntax_error "")
                    end
                | Lit(l) ->
                    begin
                    match l with
                        | StringLit(s,_) -> Some (StringValue(s))
                        | IntLit(s,_) -> Some (IntValue(s))
                        | CharLit(s,_) -> Some (CharValue(s))
                    end
            end in
        Hashtbl.add symbol_table p (Variable(p,t,value))
    ) params args ;
    (* verify that we have a block; evalutate it *)
    let return = (match stmt with
        | Block(b) -> evaluate_statement stmt last
    ) in  begin
    (* remove those bindings again *)
    List.iter(fun (Param((p,o),t)) -> Hashtbl.remove symbol_table p) params ;
    StringSet.iter(fun s -> Hashtbl.remove symbol_table s) !symbol_scope ;
    symbol_scope := StringSet.empty ;
    end ; return

let compile (Program(macros,network)) name =
    
        
    (* Add the macros to the symbol table *)
    List.iter (fun ((Macro(name,params,stmts)) as m) ->
                    Hashtbl.add symbol_table name (MacroContainer(m))) macros
    ;
    match network with
        | Network(params,(Block(b))) -> begin
            let seed = new_seed () in
                List.iter (fun s ->
                    let start_str = Printf.sprintf "start_%d" (get_num seed) in
                    let start = Automata.STE(start_str,"@", Automata.AllStart,false,[],false) in
                    let _ = Automata.add_element net start in
                    evaluate_statement s [start_str] ; () ) b
                end
    ;
    Automata.set_name net name ;
    (Automata.network_to_str net)

