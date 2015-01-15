(*
 * Kevin Angstadt
 * Compiler for AP Language
 *)
 
open Language (* Contains all the types for the AP language *)
open Id (*For counting on IDs*)

exception Syntax_error
exception Type_mismatch
exception Uninitialized_variable

let explode s =
    let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []
type symbol = (string, container) Hashtbl.t

let symbol_table : symbol = Hashtbl.create 255
let net = Automata.create ()

let if_seed = new_seed ()
let for_seed = new_seed ()

let evaluate_report last=
    Automata.set_report net last true

let rec add_all (e:Automata.element) =
    let connect = Automata.get_connections e in
    if not (Automata.contains net e) then
        Automata.add_element net e ;
        List.iter (fun (a,c) -> add_all a) connect

let rec evaluate_statement (stmt : statement) (last : string list) : string list =
    match stmt with
        | Report(scope) -> List.iter (fun s->evaluate_report s) last ; last
        | Block(b,scope) -> List.fold_left (fun last a -> evaluate_statement a last) last b
        | IF(exp,then_clause,else_clause,scope) ->
            begin
            let id = Printf.sprintf "if_%d" (get_num if_seed) in
            let states : (string,Automata.element) Hashtbl.t = Hashtbl.create 255 in
            let tb = Automata.STE(id^"_tb","$",Automata.NotStart,false,[],false) in
            let fb = Automata.STE(id^"_fb","$",Automata.NotStart,false,[],false) in
            let trap = Automata.STE(id^"trap","^$",Automata.NotStart,false,[(fb,None)],false) in
            let rec flip_symbol (Automata.STE(id,set,strt,latch,connect,report) as ste) =
                begin
                let _ = Hashtbl.add states id ste in
                let connection_list : Automata.element_connections list = match connect with
                    | hd :: tl -> List.map (fun (a,conn) -> ((flip_symbol a),conn)) connect
                    | [] -> [(fb,None)] in
                let new_ste = Automata.STE("n_"^id,"^"^set,strt,latch,(trap,None)::connection_list,report) in
                let _ = Hashtbl.add states ("n_"^id) new_ste in
                new_ste
                end in
            let rec add_conn (Automata.STE(id,set,strt,latch,connect,report) as ste) : Automata.element =
                begin
                let new_cons = List.map (fun ((Automata.STE(a_id,a_set,a_strt,a_latch,a_connect,a_report) as a),conn) ->
                                        ((Hashtbl.find states ("n_"^a_id)),None)) connect in
                let _ = List.iter (fun (a,c) -> print_endline ( Automata.element_to_str a) ) new_cons in
                let old_cons = match connect with
                    | hd :: tl -> List.map (fun (a,con) -> ((add_conn a),con)) connect
                    | [] -> [(tb,None)] in
                Automata.STE(id,set,strt,latch,new_cons@old_cons,report)
                end in
            let if_exp = evaluate_expression exp None id (new_seed ()) in
            let neg_exp = flip_symbol if_exp in
            let if_exp_mod = add_conn if_exp in
            add_all if_exp_mod ;
            add_all neg_exp ;
            Automata.connect net (id^"trap") (id^"trap") None;
            List.iter (fun s ->
                Automata.connect net s (Automata.get_id if_exp_mod) None ;
                Automata.connect net s (Automata.get_id neg_exp) None
            ) last ;
            evaluate_statement then_clause [(id^"_tb")] @ evaluate_statement else_clause [(id^"_fb")]
            end
        | ForEach((Param(Var(name),t)),source,f,scope) ->
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
        | VarDec(s,t,scope) -> begin
            match scope with
                | NetworkScope -> Hashtbl.add symbol_table s (Variable(s,t,None)) ; last (*TODO OMG this will not work correctly.  So much error checking needed*)
            end
        (*| ExpStmt(e,scope) -> match e with None -> () | Some x -> Automata.add_element net (evaluate_expression x None)*) (*TODO check to see if the expression is allowed as a statement*)
        | Fun(Var(a),Arguments(b),scope) -> begin
            (* Look up macro in the symbol table, make sure it is there *)
            let called_function = Hashtbl.find symbol_table a in
            match called_function with
                | MacroContainer(Macro(name,Parameters(params),stmts) as m) ->
                    begin
                    (*make sure the arguments are correct*)
                    
                    List.iter2 (fun (arg:expression) (p:param) -> match arg with
                        | Var(s) ->
                            let var = Hashtbl.find symbol_table s in
                                begin
                                match var with
                                    | Variable(ident,t,value) ->
                                        match p with Param(e,t2) ->
                                            if not (t = t2) then raise Type_mismatch
                                    | _ -> raise Syntax_error
                                end
                        | Lit(l) ->
                            begin
                            match l with
                                | StringLit(_,t)
                                | IntLit(_,t)
                                | CharLit(_,t) ->
                                    match p with Param(e,t2) ->
                                        if not (t = t2) then raise Type_mismatch
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
        | _ -> Printf.printf "Oh goodness! %s" (statement_to_str stmt) ; raise Syntax_error
        
and evaluate_expression (exp : expression) (s: Automata.element option) (prefix:string) (seed:id_seed) =
    match exp with
        | EQ(a,b) -> begin
            let helper (Automata.STE(id,set,strt,latch,connect,report) as new_element) = begin
                match s with
                    | None -> new_element
                    | Some x -> Automata.STE(id,set,strt,latch,(x,None)::connect,report)
            end in
            let get_value v =
                begin
                match v with
                    | Lit(CharLit(a,_)) -> a
                    | Var(a) -> try
                        let var = Hashtbl.find symbol_table a in
                        begin
                        match var with
                            | Variable(name,typ,Some CharValue(s)) ->
                                if typ = Char then s else raise Syntax_error
                            | _ -> raise Syntax_error
                        end
                        with Not_found -> raise Syntax_error
                    | _ -> raise Syntax_error
                end
            in
            if a = Input then
                let new_element = Automata.STE((Printf.sprintf "%s_%d" prefix (get_num seed)),Char.escaped (get_value b),NotStart,false,[],false) in
                helper new_element
            else if b = Input then
                let new_element = Automata.STE((Printf.sprintf "%s_%d" prefix (get_num seed)),Char.escaped (get_value a),NotStart,false,[],false) in
                helper new_element
            else raise Syntax_error 
            end
        (*| NEQ(a,b)
        | LEQ(a,b)
        | LT(a,b)
        | GT(a,b)
        | Not(a)
        | Negative(a)
        | And(a,b)
        | Or(a,b)
        | Var(a)
        | Lit(a)
        | Input*)
        
  
and evaluate_macro (Macro(name,Parameters(params),stmt)) (args:expression list) (last:string list) =
    (* add bindings for arguments to state *)
    List.iter2 (fun (Param((Var(p)),t)) arg ->
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
    List.iter(fun (Param((Var(p)),t)) -> Hashtbl.remove symbol_table p) params
    end ; return

let compile (Program(macros,network)) =
    let verify_network_params (Parameters(p)) =
        List.iter (fun param -> match param with
                                | Param(exp,typ) -> begin
                                    match exp with
                                    | Var(str) -> ()
                                    | _ -> (Printf.printf "param error 1!" ; exit 1)
                                    ;
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
            match stmt with
                | Block(b,s) -> begin
                    let start = Automata.STE("start","@", Automata.AllStart,false,[],false) in
                    let _ = Automata.add_element net start in
                    evaluate_statement stmt ["start"]
                    end
                | _ -> (Printf.printf "network error!" ; exit 1)
            end
    ;
    Printf.printf "%s" (Automata.network_to_str net)

