(*
 * Kevin Angstadt
 * Compiler for AP Language
 *)
 
open Language (* Contains all the types for the AP language *)

exception Syntax_error
exception Type_mismatch
exception Uninitialized_variable

type symbol = (string, container) Hashtbl.t

let symbol_table : symbol = Hashtbl.create 255
let net : Automata.network = Hashtbl.create 255


let last : string option ref = ref (None : string option)
let num = ref 1

let evaluate_report last=
    match last with
    | None ->
        Automata.add_element net (Automata.STE((Printf.sprintf "_%d" !num),"*",Automata.Start,false,[],true)) ;
        num := (!num + 1)
    | Some x -> Automata.set_report net x true


let rec evaluate_statement (stmt : statement) =
    match stmt with
        | Report -> evaluate_report !last
        | Block(b) -> List.iter (fun a -> evaluate_statement a) b
        | IF(exp,then_clause,else_clause) -> ()
        | ForEach((Param(Var(name),t)),source,f) ->
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
                                        String.iter (fun c ->
                                            let c = CharValue(c) in
                                            (*set binding to 'name'*)
                                            Hashtbl.add symbol_table name (Variable(name,Char,Some c)) ;
                                            evaluate_statement f;
                                            (*remove binding*)
                                            Hashtbl.remove symbol_table name
                                        ) s
                                    else
                                        raise Syntax_error
                                | _ -> raise Syntax_error
                            end
            end
        | VarDec(s,t) -> Hashtbl.add symbol_table s (Statement(stmt)) (*TODO OMG this will not work correctly.  So much error checking needed*)
        | ExpStmt(e) -> match e with None -> () | Some x -> evaluate_expression x (*TODO check to see if the expression is allowed as a statement*)
        
and evaluate_expression (exp : expression) =
    match exp with
        (*| EQ(a,b)
        | NEQ(a,b)
        | LEQ(a,b)
        | LT(a,b)
        | GT(a,b)
        | Not(a)
        | Negative(a)
        | And(a,b)
        | Or(a,b)
        | Var(a)
        | Lit(a)*)
        | Fun(Var(a),Arguments(b)) -> begin
            (* Look up macro in the symbol table, make sure it is there *)
            let called_function = Hashtbl.find symbol_table a in
            match called_function with
                | MacroContainer(m) ->
                    begin
                    (*make sure the arguments are correct*)
                    match m with Macro(name,Parameters(params),stmts) ->
                        begin
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
                        evaluate_macro m b
                        end
                    end
                | _ -> raise Syntax_error
            end
  
and evaluate_macro (Macro(name,Parameters(params),stmt)) (args:expression list) =
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
    (match stmt with
        | Block(b) -> evaluate_statement stmt
        | _ -> raise Syntax_error
    );   
    (* remove those bindings again *)
    List.iter(fun (Param((Var(p)),t)) -> Hashtbl.remove symbol_table p) params

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
    List.iter (fun m -> match (m : macro) with
                                | Macro(name,params,stmts) -> Hashtbl.add symbol_table name (MacroContainer(m))
                                ) macros
    ;
    match network with
        | Network(params,stmt) -> begin
            verify_network_params params ;
            match stmt with
                | Block(b) -> evaluate_statement stmt
                | _ -> (Printf.printf "network error!" ; exit 1)
            end
    ;
    Printf.printf "%s" (Automata.network_to_str net)

