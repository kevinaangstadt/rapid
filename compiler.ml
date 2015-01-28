(*
 * Kevin Angstadt
 * Compiler for AP Language
 *)
 
open Language (* Contains all the types for the AP language *)
open Id (*For counting on IDs*)
open Util

exception Syntax_error
exception Type_mismatch
exception Uninitialized_variable
exception Negative_count

module StringSet = Set.Make(String)

let explode s =
    let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
type symbol = (string, container) Hashtbl.t

let symbol_table : symbol = Hashtbl.create 255
let symbol_scope = ref StringSet.empty
let counter_rename : (string,string) Hashtbl.t = Hashtbl.create 10
let net = Automata.create "" ""

let if_seed = new_seed ()
let c_seed = new_seed ()

let evaluate_report last=
    Automata.set_report net last true

let is_counter exp =
    match exp with
        | Var(a) -> begin try
            let _ = Hashtbl.find counter_rename a in
                true
            with Not_found -> false
            end
        | _ -> false
            
let is_counter_expression exp =
    match exp with
        | EQ(a,b)
        | NEQ(a,b)
        | LEQ(a,b)
        | GEQ(a,b)
        | LT(a,b)
        | GT(a,b) -> is_counter a || is_counter b
        | _ -> false
        
let rec add_all net (e:Automata.element) =
    let connect = Automata.get_connections e in
    if not (Automata.contains net e) then
        Automata.add_element net e ;
        List.iter (fun (a,c) -> add_all net a) connect

let rec evaluate_statement (stmt : statement) (last : string list) : string list =
    match stmt with
        | Report(scope) -> List.iter (fun s->evaluate_report s) last ; last
        | Block(b,scope) -> List.fold_left (fun last a -> evaluate_statement a last) last b
        | IF(exp,then_clause,else_clause,scope) ->
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
            
            (*if this is a counter experession*)
            if is_counter_expression exp then
                begin
                (*(c, (create (num+1) None), StringSet.of_list tb, StringSet.of_list fb)*)
                let (c,n,if_exp,yes,no) = evaluate_counter_expression exp in
                    Automata.add_element net (Automata.STE(c^"_trap","$",Automata.NotStart,false,[],false)) ;
                    Automata.add_element net (Automata.STE("n_"^c^"_trap","^$",Automata.NotStart,false,[],false)) ;
                    Automata.add_element net (Automata.STE(c^"_trap_t","\\x26",Automata.NotStart,false,[],false)) ;
                    add_all net if_exp ;
                    List.iter (fun a -> Automata.connect net a (c^"_trap") None ; Automata.connect net a ("n_"^c^"_trap") None) last ;
                    Automata.connect net c (Automata.get_id if_exp) None ;
                    Automata.connect net (c^"_trap") (c^"_trap")  None ;
                    Automata.connect net ("n_"^c^"_trap") ("n_"^c^"_trap") None ;
                    Automata.connect net (c^"_trap") (c^"_trap_t") None ;
                    Automata.connect net ("n_"^c^"_trap") (c^"_trap") None ;
                    Automata.connect net (c^"_trap") c (Some "cnt") ;
                    Automata.connect net ("n_"^c^"_trap") c (Some "cnt") ;
                    tb := yes ;
                    fb := StringSet.add (c^"_trap_t") no ;
                    Automata.set_count net c n
                end
            (*this is a regular if*)
            else
                begin
                let if_exp = evaluate_expression exp None id (new_seed ()) in
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
                ) if_exp
                end ;
            evaluate_statement then_clause (StringSet.elements !tb) @ evaluate_statement else_clause (StringSet.elements !fb)
            end
        | ForEach((Param(name,t)),source,f,scope) ->
            begin
            match source with
                | Var(v) -> (*TODO error handling*)
                    let Variable(_,_,value) = Hashtbl.find symbol_table v in
                    match value with
                        | None -> raise Uninitialized_variable
                        | Some value ->
                            begin
                            match value with
                                | StringValue(s) ->
                                    if t = Char then
                                        let s = explode s in
                                        List.fold_left (fun last c ->
                                            let c = CharValue(c) in
                                            (*set binding to 'name'*)
                                            Hashtbl.add symbol_table name (Variable(name,Char,Some c)) ;
                                            let return = evaluate_statement f last in
                                            (*remove binding*)
                                            Hashtbl.remove symbol_table name ; return
                                        ) last s
                                    else
                                        raise Syntax_error
                                | _ -> raise Syntax_error
                            end
            end
        | VarDec(s,t,scope) ->
            begin
            let (id,value) =
                match t with
                | Counter ->
                    let num = get_num c_seed in
                    let id = Printf.sprintf "%s_%d" s num in
                    let _ = Hashtbl.add counter_rename s id in
                    (id, Some (AutomataElement(Automata.Counter(id,100,Automata.Pulse,false,[]))))
                | _ -> (s,None) in
            (* TODO We need some notion of scope to remove these after the fact! *)
            Hashtbl.add symbol_table id (Variable(id,t,value)) ;
            symbol_scope := StringSet.add id !symbol_scope ;
            last (*TODO OMG this will not work correctly.  So much error checking needed*)
            end
        (*| ExpStmt(e,scope) -> match e with None -> () | Some x -> Automata.add_element net (evaluate_expression x None)*) (*TODO check to see if the expression is allowed as a statement*)
        | Fun(Var(a),Arguments(b),scope) -> begin
            (* Look up macro in the symbol table, make sure it is there *)
            (* TODO does this need to be error-checked? *)
            let called_function = Hashtbl.find symbol_table a in
            match called_function with
                | MacroContainer(Macro(name,Parameters(params),stmts) as m) ->
                    begin
                    (*make sure the arguments are correct*)
                    
                    List.iter2 (fun (arg:expression) (p:param) -> match arg with
                        | Var(s) ->
                            let Variable(ident,t,value) = Hashtbl.find symbol_table s in
                                begin
                                match p with Param(e,t2) ->
                                    if not (t = t2) then raise Type_mismatch
                                end
                        | Lit(l) ->
                            begin
                            match l with
                                | StringLit(_,t)
                                | IntLit(_,t)
                                | CharLit(_,t) ->
                                    begin
                                    match p with Param(e,t2) ->
                                        if not (t = t2) then raise Type_mismatch
                                    end
                                | _ -> raise Syntax_error
                            end
                        | _ -> raise Syntax_error
                    ) b params
                    ;
                    (*evaluate macro*)
                    evaluate_macro m b last
                       
                    end
                | _ -> raise Syntax_error
            end
        | Count(Var(a),scope) -> begin try
            let id = Hashtbl.find counter_rename a in
            let counter = Hashtbl.find symbol_table id in
            begin
            match counter with
                | Variable(s,t,v) ->
                    let c = match v with
                        | Some AutomataElement(x) -> x in
                    (*Check that we have a counter*)
                    if t = Counter then
                        begin
                        if not (Automata.contains net c) then Automata.add_element net c ;
                        List.iter (fun l -> Automata.connect net l s (Some "cnt")) last ; last
                        end
                    else raise Type_mismatch
                | _ -> raise Type_mismatch
            end
            with Not_found -> raise Syntax_error
            end
        | Reset(Var(a),scope) -> begin try
            let id = Hashtbl.find counter_rename a in
            let counter = Hashtbl.find symbol_table id in
            begin
            match counter with
                | Variable(s,t,v) ->
                    let c = match v with
                        | Some AutomataElement(x) -> x in
                    (*Check that we have a counter*)
                    if t = Counter then
                        begin
                        if not (Automata.contains net c) then Automata.add_element net c ;
                        List.iter (fun l -> Automata.connect net l s (Some "rst")) last ; last
                        end
                    else raise Type_mismatch
                | _ -> raise Type_mismatch
            end
            with Not_found -> raise Syntax_error
            end
        | _ -> Printf.printf "Oh goodness! %s" (statement_to_str stmt) ; raise Syntax_error
        
and evaluate_expression (exp : expression) (s: Automata.element list option) (prefix:string) (seed:id_seed) : Automata.element list =
    (*Helper...requires type checking first or will result in a syntax error*)
    let get_value v =
        begin
        match v with
            | Lit(CharLit(a,_)) -> a
            | Var(a) -> begin try
                let var = Hashtbl.find symbol_table a in
                begin
                match var with
                    | Variable(name,typ,Some CharValue(s)) ->
                        if typ = Char then s else raise Syntax_error
                    | _ -> raise Syntax_error
                end
                with Not_found -> raise Syntax_error
                end
            | _ -> raise Syntax_error
        end in
    match exp with
        | EQ(a,b) -> begin
            let helper (Automata.STE(id,set,strt,latch,connect,report) as new_element) = begin
                match s with
                    | None -> [new_element]
                    | Some x ->
                        let cons = List.map (fun a -> (a,None)) x in
                        [Automata.STE(id,set,strt,latch,cons@connect,report)]
            end in
            if a = Input then
                let new_element = Automata.STE((Printf.sprintf "%s_%d" prefix (get_num seed)),Char.escaped (get_value b),Automata.NotStart,false,[],false) in
                helper new_element
            else if b = Input then
                let new_element = Automata.STE((Printf.sprintf "%s_%d" prefix (get_num seed)),Char.escaped (get_value a),Automata.NotStart,false,[],false) in
                helper new_element
            else raise Syntax_error 
            end
        | NEQ(a,b) -> begin (* TODO This is just copied and pasted from above; need to fix this *)
            let helper (Automata.STE(id,set,strt,latch,connect,report) as new_element) = begin
                match s with
                    | None -> [new_element]
                    | Some x -> 
                        let cons = List.map (fun a -> (a,None)) x in
                        [Automata.STE(id,set,strt,latch,cons@connect,report)]
            end in
            if a = Input then
                let new_element = Automata.STE((Printf.sprintf "%s_%d" prefix (get_num seed)),Printf.sprintf "^%s" (Char.escaped (get_value b)),Automata.NotStart,false,[],false) in
                helper new_element
            else if b = Input then
                let new_element = Automata.STE((Printf.sprintf "%s_%d" prefix (get_num seed)),Printf.sprintf "^%s" (Char.escaped (get_value a)),Automata.NotStart,false,[],false) in
                helper new_element
            else raise Syntax_error 
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
            let if_exp = evaluate_expression a None prefix seed in
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
            let b_eval = evaluate_expression b None prefix seed in
            let a_eval = evaluate_expression a (Some b_eval) prefix seed in
            begin
            match s with
                | None -> a_eval
                | Some x -> List.map (fun a -> add_to_last a x) a_eval
            end
        | Or(a,b) ->
            let rec build_charset c =
                match c with
                    | EQ(a,b)
                    | NEQ(a,b) ->
                        if a = Input then Char.escaped (get_value b)
                        else if b = Input then Char.escaped (get_value a)
                        else raise Syntax_error
                    | Or(a,b) -> (build_charset a) ^ (build_charset b)
                    | _ -> ""
                in
            let rec can_condense c =
                match c with
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
                let b_eval = evaluate_expression b s prefix seed in
                    let connect = match s with
                        | None -> []
                        | Some x -> List.map (fun a -> (a,None)) x
                    in Automata.STE(Printf.sprintf "%s_%d" prefix (get_num seed),Printf.sprintf "[%s]" ((build_charset a)), Automata.NotStart, false, connect, false) :: b_eval
            else
                if can_condense b then
                let a_eval = evaluate_expression a s prefix seed in
                    let connect = match s with
                        | None -> []
                        | Some x -> List.map (fun a -> (a,None)) x
                    in Automata.STE(Printf.sprintf "%s_%d" prefix (get_num seed),Printf.sprintf "[%s]" ((build_charset b)), Automata.NotStart, false, connect, false) :: a_eval
                else
                    let a_eval = evaluate_expression a s prefix seed in
                    let b_eval = evaluate_expression b s prefix seed in
                    a_eval @ b_eval
        (*| Var(a)
        | Lit(a)
        | Input*)
        
and evaluate_counter_expression (exp : expression) =
    (*Helper...requires type checking first or will result in a syntax error*)
    let get_value v =
        begin
        match v with
            | Lit(IntLit(a,_)) -> a
            | Var(a) -> begin try
                let var = Hashtbl.find symbol_table a in
                begin
                match var with
                    | Variable(name,typ,Some IntValue(s)) ->
                        if typ = Int then s else raise Syntax_error
                    | _ -> raise Syntax_error
                end
                with Not_found -> raise Syntax_error
                end
            | _ -> raise Syntax_error
        end in
    let get_counter v =
        begin
        match v with
            | Var(a) -> try
                let id = Hashtbl.find counter_rename a in
                let var = Hashtbl.find symbol_table id in
                begin
                match var with
                    | Variable(name,typ, Some (AutomataElement(e))) ->
                        if typ = Counter then (Automata.get_id e) else raise Syntax_error
                    | _ -> raise Syntax_error
                end
                with Not_found -> raise Syntax_error
            | _ -> raise Syntax_error
        end in
    (*TODO determine if we need to do this*)
    let kill_counter (Var(a)) =
        begin
            let id = Hashtbl.find counter_rename a in
            Hashtbl.remove symbol_table id ;
            Hashtbl.remove counter_rename a
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
        let (tb,fb) = match exp with
            | EQ(_,_) -> ( state_list num (num + 1) , state_list 0 num @ state_list (num + 1) (num + 2) )
            | LEQ(_,_) -> ( state_list 0 (num+1) , state_list (num + 1) (num + 2) )
            | LT(_,_) ->( state_list 0 num , state_list num (num + 2) ) in
        
        (c, (num+2), (create (num+1) None), StringSet.of_list tb, StringSet.of_list fb)
        end in
    (*if counter is not listed first, flip it!*)
    match exp with
        | EQ(a,b) -> if is_counter b then evaluate_counter_expression (EQ(b,a))
                     else
                        helper (get_value b) (get_counter a)
        | GT(a,b) -> evaluate_counter_expression (LEQ(b,a))
        | GEQ(a,b) -> evaluate_counter_expression (LT(b,a))
        | LEQ(a,b)
        | LT(a,b) ->
            let value = if is_counter a then get_value b else raise Syntax_error in
            helper value (get_counter a)
        


and evaluate_macro (Macro(name,Parameters(params),stmt)) (args:expression list) (last:string list) =
    (* add bindings for arguments to state *)
    List.iter2 (fun (Param(p,t)) arg ->
        let value =
            begin
            match arg with
                | Var(s) ->
                    let variable = Hashtbl.find symbol_table s in
                    begin
                    match variable with
                        | Variable(name,t,value) -> value
                        | _ -> raise Syntax_error
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
        | Block(b,scope) -> evaluate_statement stmt last
        | _ -> raise Syntax_error
    ) in  begin
    (* remove those bindings again *)
    List.iter(fun (Param(p,t)) -> Hashtbl.remove symbol_table p) params ;
    StringSet.iter(fun s -> Hashtbl.remove symbol_table s) !symbol_scope ;
    symbol_scope := StringSet.empty ;
    end ; return

let compile (Program(macros,network)) name =
    let verify_network_params (Parameters(p)) =
        List.iter (fun param -> match param with
                                | Param(exp,typ) -> begin
                                    match typ with
                                    | Counter -> (Printf.printf "param error 2!" ; exit 1)
                                    | _ -> ()
                                end
        ) p in
        
    (* Add the macros to the symbol table *)
    List.iter (fun ((Macro(name,params,stmts)) as m) ->
                    Hashtbl.add symbol_table name (MacroContainer(m))) macros
    ;
    match network with
        | Network(params,stmt) -> begin
            verify_network_params params ;
            let seed = new_seed () in
            match stmt with
                | Block(b,s) -> begin
                    List.iter (fun s ->
                        let start_str = Printf.sprintf "start_%d" (get_num seed) in
                        let start = Automata.STE(start_str,"@", Automata.AllStart,false,[],false) in
                        let _ = Automata.add_element net start in
                        evaluate_statement s [start_str] ; () ) b
                    end
                | _ -> (Printf.printf "network error!" ; exit 1)
            end
    ;
    Automata.set_name net name ;
    (Automata.network_to_str net)

