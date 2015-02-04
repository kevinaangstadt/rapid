(*
 * Kevin Angstadt
 * Type Checking functions
 *)
 
open Language

let symbol_variable_lookup (v : string) symbol_table =
    begin try
    let var = Hashtbl.find symbol_table v in
        begin
        match var with
            | Variable(_,_,_) as return -> return
            | _ -> raise Syntax_error
        end
        with Not_found -> raise Syntax_error
        end
        
let rec get_type exp symbol_table  =
    match exp with
        | EQ(a,b)
        | NEQ(a,b) ->
            let at = get_type a symbol_table in
            let bt = get_type b symbol_table in
            begin
            match at,bt with
                | Boolean,Boolean
                | Char,Char
                | Int,Int -> Boolean
                | Counter,Int
                | Int,Counter -> Counter
                | Automata, Char
                | Char, Automata -> Automata
            end
        | LEQ(a,b)
        | GEQ(a,b)
        | LT(a,b)
        | GT(a,b) ->
            let at = get_type a symbol_table in
            let bt = get_type b symbol_table in
            begin
            match at,bt with
                | Int,Int -> Boolean
                | Counter,Int
                | Int,Counter -> Counter
            end
        | And(a,b)
        | Or(a,b) ->
            let at = get_type a symbol_table in
            let bt = get_type b symbol_table in
            begin
            match at,bt with
                | Boolean,Boolean -> Boolean
                | Automata,Automata -> Automata
            end
        | Not(a)
        | Negative(a) -> get_type a symbol_table  
        | Var(a) -> let Variable(n,t,v) = symbol_variable_lookup a symbol_table in t
        | Lit(a) -> begin match a with
            | StringLit(_,t)
            | IntLit(_,t)
            | CharLit(_,t) -> t
            | True
            | False -> Boolean
            end
        | Fun(a,b,c) ->
            let Variable(n,t,v) = symbol_variable_lookup a symbol_table in
            begin match t with
                | String ->
                    begin match b with
                        | "length" -> Int
                    end
                | Counter ->
                    begin match b with
                        | "count" -> Automata
                        | "reset" -> Automata
                    end
            end
        | Input -> Automata