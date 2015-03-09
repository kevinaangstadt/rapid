(*
 * Kevin Angstadt
 * Language Simulator
 *)

open Util
open Language

type state = {
    input : char list ;
    index : int ;
    memory : symbol ;
    report : (int,int) Hashtbl.t ;
}



(* Cloning does a "deep" copy of memory and input (not entirely preceise)
 * and a shallow copy of the reports, since these are cumulative
 *)
let clone_state sigma = {
    input = sigma.input ;
    index = sigma.index ;
    memory = Hashtbl.copy sigma.memory ;
    report = sigma.report
}

let report sigma =
    let idx = sigma.index - 1 in
    let current =
        try
            Hashtbl.find sigma.report idx
        with Not_found -> 0
    in Hashtbl.replace sigma.report idx (current + 1)

let consume sigma : value * state =
    let value = CharValue(List.hd sigma.input) in
    let tail = if List.length sigma.input = 1 then [] else List.tl sigma.input in
    let sigma_prime = { sigma with
        input = tail ;
        index = sigma.index + 1 ;
    } in
        value,sigma_prime
        
let print_reports sigma =
    Hashtbl.iter (fun k v -> Printf.printf "%d -> %d\n" k v) sigma.report

let rec evaluate_statement (stmt :statement) (sigma : state) : state =
    match stmt with
        | Report -> report sigma ; sigma
        | Block(b) ->
            List.fold_left (fun sigma_prime s -> evaluate_statement s sigma_prime) sigma b
        | If(exp,then_clause,else_clause) ->
            let (BooleanValue(value)),sigma_prime = evaluate_expression exp sigma in
                if value then evaluate_statement then_clause sigma_prime
                else evaluate_statement else_clause sigma_prime
        | While(exp,body) ->
            let (BooleanValue(value)),sigma_prime = evaluate_expression exp sigma in
                if value then evaluate_statement (Block([body;stmt])) sigma_prime
                else sigma_prime
        (*| Either(statement_blocks) ->*)
        | ForEach((Param((name,o),t)),source,f) ->
            let value,sigma_prime = evaluate_expression source sigma in
                begin
                match value with
                | StringValue(s) ->
                    let char_list = explode s in
                        List.fold_left (fun sigma_prime c ->
                            (* add binding *)
                            let c = CharValue(c) in
                            Hashtbl.add sigma.memory name (Variable(name,Char,Some c)) ;
                            let sigma_prime_prime = evaluate_statement f sigma_prime in
                            (* remove binding *)
                            Hashtbl.remove sigma.memory name ; sigma_prime_prime
                        ) sigma_prime char_list
                (*TODO Add Array iteration here*)
                end
        | VarDec(var) ->
            List.fold_left (fun sigma_prime ((s,o),t,init) ->
                let value,sigma_prime_prime = match t with
                    | Counter -> Some(IntValue(0)), sigma_prime
                    | _ ->
                        match init with
                            | None -> None, sigma_prime
                            (*TODO ARRAY INITS!*)
                            | Some (PrimitiveInit(e)) ->
                                let tmp,sig_prime = evaluate_expression e sigma_prime in
                                    (Some tmp),sig_prime
                in
                    Hashtbl.add sigma_prime_prime.memory s (Variable(s,t,value)) ;
                    sigma_prime_prime
            ) sigma var
        | MacroCall(a,Arguments(b)) ->
            let (MacroContainer(m)) = Hashtbl.find sigma.memory a in
                evaluate_macro m b sigma
        | ExpStmt(exp) ->
            begin
            match exp with
                | None -> sigma
                | Some exp ->
                    let value,sigma_prime = evaluate_expression exp sigma in
                        sigma_prime
            end
        | Assign((name,o),exp) ->
            let value,sigma_prime = evaluate_expression exp sigma in
                (*TODO Arrays!*)
                let (Variable(s,t,_)) = Hashtbl.find sigma_prime.memory name in
                Hashtbl.replace sigma_prime.memory name (Variable(s,t,(Some value))) ;
                sigma_prime

and evaluate_expression (exp : expression) (sigma : state) : value * state =
    match exp.exp with
        | EQ(a,b)
        | NEQ(a,b)
        | LEQ(a,b)
        | GEQ(a,b)
        | LT(a,b)
        | GT(a,b) ->
            let value_a,sigma_prime = evaluate_expression a sigma in
            let value_b,sigma_prime_prime = evaluate_expression b sigma_prime in
                let value = match value_a,value_b with
                    | IntValue(a),IntValue(b) ->
                        begin
                        match exp.exp with
                            | EQ(_,_) -> a = b
                            | NEQ(_,_) -> a <> b
                            | LEQ(_,_) -> a <= b
                            | GEQ(_,_) -> a >= b
                            | LT(_,_) -> a < b
                            | GT(_,_) -> a > b
                        end
                    | CharValue(a),CharValue(b) ->
                        begin
                        match exp.exp with
                            | EQ(_,_) -> a = b
                            | NEQ(_,_) -> a <> b
                            | LEQ(_,_) -> a <= b
                            | GEQ(_,_) -> a >= b
                            | LT(_,_) -> a < b
                            | GT(_,_) -> a > b
                        end
                    | BooleanValue(a),BooleanValue(b) ->
                        begin
                        match exp.exp with
                            | EQ(_,_) -> a = b
                            | NEQ(_,_) -> a <> b
                            | LEQ(_,_) -> a <= b
                            | GEQ(_,_) -> a >= b
                            | LT(_,_) -> a < b
                            | GT(_,_) -> a > b
                        end
                in
                    BooleanValue(value),sigma_prime_prime
        | Not(a) ->
            let BooleanValue(b),sigma_prime = evaluate_expression a sigma in
            (BooleanValue(not b)),sigma_prime
        | Negative(a) ->
            let IntValue(i),sigma_prime = evaluate_expression a sigma in
            (IntValue(- i)),sigma_prime
        | And(a,b) ->
            (*TODO Make this actually fail early*)
            let (BooleanValue(b1)),sigma_prime = evaluate_expression a sigma in
            let (BooleanValue(b2)),sigma_prime_prime = evaluate_expression b sigma_prime in
                (BooleanValue(b1 && b2)),sigma_prime_prime
        | Or(a,b) ->
            (*TODO make this actually work like or*)
            let (BooleanValue(b1)),sigma_prime = evaluate_expression a sigma in
            let (BooleanValue(b2)),sigma_prime_prime = evaluate_expression b sigma_prime in
                (BooleanValue(b1 || b2)),sigma_prime_prime
        (*| Plus(a,b)
        | Minus(a,b)
        | Times(a,b)
        | Mod(a,b)*)
        | Lval((name,o)) ->
            (*TODO ARRAYS!*)
            let (Variable(_,_,Some v)) = Hashtbl.find sigma.memory name in
            v,sigma
        | Lit(l) ->
            let value =
                match l with
                    | StringLit(s,_) -> StringValue(s)
                    | CharLit(c,_) -> CharValue(c)
                    | IntLit(i,_) -> IntValue(i)
                    | True -> BooleanValue(true)
                    | False -> BooleanValue(false)
            in
                value,sigma
        | Fun((name,o),s,a) ->
            let (Variable(_,t,(Some value))) = Hashtbl.find sigma.memory name  in
                let value =
                    match value with
                        | StringValue(v) ->
                            begin
                            match s with
                                | "length" -> IntValue(String.length v)
                            end
                        | IntValue(i) ->
                            begin
                            match s with
                                | "count" -> Hashtbl.replace sigma.memory name (Variable(name,t,(Some (IntValue(i+1))))) ;
                                             IntValue(i+1)
                                | "reset" -> Hashtbl.replace sigma.memory name (Variable(name,t,(Some (IntValue(0))))) ;
                                             IntValue(0)
                            end
                in
                    value,sigma
        | Input -> consume sigma
    
and evaluate_macro ((Macro(name,Parameters(params),stmt)) : macro) (args:expression list) (sigma : state) : state =
    (* add bindings for arguments to state *)
    List.iter2 (fun (Param((p,o),t)) arg ->
        let value =
            begin
            (*TODO allow for offsets*)
            match arg.exp with
                | Lval((s,o2)) ->
                    let variable = Hashtbl.find sigma.memory s in
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
        Hashtbl.add sigma.memory p (Variable(p,t,value))
    ) params args ;
    (* verify that we have a block; evalutate it *)
    let sigma_prime = evaluate_statement stmt sigma in
    begin
    (* remove those bindings again *)
    List.iter(fun (Param((p,o),t)) -> Hashtbl.remove sigma_prime.memory p) params ;
    sigma_prime
    end
    
let simulate (Program(macros,(Network(params,(Block(b)))))) sigma =
    begin
    try
    (* Add the macros to the symbol table*)
    List.iter (fun ((Macro(name,params,stmts)) as m) ->
                    Hashtbl.add sigma.memory name (MacroContainer(m))) macros
    ;
    List.iter (fun s ->
        let start_sigma = clone_state sigma in
        (evaluate_statement s start_sigma) ; ()
    ) b
    ;
    print_reports sigma
    with Failure _ -> print_reports sigma
    end;;

let process program sigma =
    let program_t = Tc.check program in
        simulate program_t sigma
        
let read_program (name : string) =
    try
        let channel = open_in name in
        let lexbuf = Lexing.from_channel channel in
            flush stdout;
            let return = Parse.program Lex.initial lexbuf in
            close_in_noerr channel ; return
    with Sys_error _ -> begin
        Printf.printf "Failed to open %s\n" name ; exit (-1)
        end

let read_input (name : string) =
    let rec build_list l channel : char list =
        try
            let c = input_char channel in
            build_list (c::l) channel
        with End_of_file -> l
        in
    try
        let channel = open_in name in
        List.rev (build_list [] channel)
    with Sys_error _ -> begin
        Printf.printf "Failed to open %s\n" name ; exit (-1)
        end ;;

let file = ref "" in
let input = ref "" in
let set_in filename = input := filename in
let argspec = [
        ("-input", Arg.String (set_in), "Name of input file")
    ] in
let argspec = Arg.align argspec in
    Arg.parse argspec (fun x -> file := x) "Usage: rapidsim -input [input file] [rapid program]" ;
    if(String.length !file = 0) then
        begin
            Printf.printf "You must provide a RAPID program to simulate!\n"; exit (-1)
        end
    else if (String.length !input = 0) then
        begin
            Printf.printf "You must provide an input file to simulate!\n"; exit (-1)
        end
    else
        let program = read_program !file in
        let program_input = read_input !input in
        let sigma = {
            input = program_input ;
            index = 1;
            memory = Hashtbl.create 255 ;
            report = Hashtbl.create (String.length !input)
        } in
            process program sigma
        