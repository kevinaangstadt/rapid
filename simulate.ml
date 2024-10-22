(*
 * Language Simulator
 *)
 
(*
 * For printing memory:
 * Hashtbl.iter (fun k v -> Printf.printf "%s -> %s\n" k ((fun v -> match v with Variable(_,_,Some v) -> (val_to_string v) | _ -> "") v)) sigma_prime.memory ;
 *)

open Util
open Language
open Intermediate

exception Fail

(*Helper function if needed for debugging*)
let val_to_string value =
    match value with
        | StringValue(s) -> s
        | IntValue(i) -> Printf.sprintf "%d" i
        | CharValue(c) -> Printf.sprintf "%c" c
        | BooleanValue(b) -> Printf.sprintf "%b" b
        | CounterList(_) -> "CounterList"

type state = {
    input : char list ;
    index : int ;
    memory : symbol ;
    report : (int,int) Hashtbl.t ;
}

let original_sigma = ref {
    input = [] ;
    index = 0;
    memory = Hashtbl.create 0 ;
    report = Hashtbl.create 0 ;
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

(*Let's set up the job queue and related functions*)
type job_location =
    | EvaluationDone
    | Converge
    | EvaluatingStatement of statement
    | EvaluatingNetwork
    | RemoveVariables of string list
    
let jobs = Queue.create ()
let job_history : (int,job_location) Hashtbl.t = Hashtbl.create 1000

let in_queue idx stmt =
    begin try
    let statements_seen = Hashtbl.find_all job_history idx in
        List.mem stmt statements_seen
    with Not_found -> false
    end

let add_job (next : job_location list) (sigma : state) =
    (*TODO Still need to figure out the best way to filter job queue*)
    match next with
        | [] -> Queue.add (EvaluationDone,[],sigma) jobs
        | hd :: tl -> Hashtbl.add job_history sigma.index hd ; Queue.add (hd,tl,sigma) jobs

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
        (*begin try
            let head = List.hd tail in
            if head = '@' && not (in_queue (sigma_prime.index + 1) EvaluatingNetwork) then
                begin
                add_job [EvaluatingNetwork] {!original_sigma with input = List.tl tail; index = sigma_prime.index + 1}
                end
        with Failure _-> ()
        end ;*)
        value,sigma_prime
        
let print_reports sigma =
    let output = Hashtbl.fold (fun k v l -> (k,v) :: l ) sigma.report [] in
    List.iter (fun (k,v) -> Printf.printf "%d -> %d\n" k v) (List.sort (fun (k1,_) (k2,_) -> k1-k2)  output)

let rec evaluate_statement (stmt :statement) (sigma : state) (next : job_location list) =
    (*Printf.printf "evaluating: %s\n" (Language.statement_to_str stmt);*)
    match stmt.stmt with
        | Report -> report sigma ; add_job next sigma
        | Break -> add_job (List.tl (List.tl next)) sigma
        | Block(b) ->
            begin
            match b with
                | [] -> add_job next sigma
                | hd::tl -> add_job (EvaluatingStatement(hd)::EvaluatingStatement({stmt=Block(tl);loc=(List.hd tl).loc; id=stmt.id})::next) sigma
            end
        | If(exp,then_clause,else_clause) ->
            let result_list = evaluate_expression exp sigma in
                List.iter (fun ((BooleanValue(value)),sigma_prime) ->
                    if value then
                        add_job (EvaluatingStatement(then_clause)::next) sigma_prime
                    else add_job (EvaluatingStatement(else_clause)::next) sigma_prime
                ) result_list
        | Either(statement_blocks) ->
            List.iter (fun stmt ->
                let sigma_prime = clone_state sigma in
                    add_job (EvaluatingStatement(stmt)::Converge::next) sigma_prime
            ) statement_blocks
        | SomeStmt(iterVar, iter, body) -> Printf.printf "bob2" ; failwith "some not implements"
        | ForEach((Param(name,t)),source,f) ->
            (*TODO THIS COULD CAUSE A MASSIVE ERROR*)
            let [(value,sigma_prime)] = evaluate_expression source sigma in
                begin
                match value with
                | StringValue(s) ->
                    let char_list = explode s in
                    let new_stmts = RemoveVariables([name]) :: List.fold_left (fun last c ->
                            (* add binding *)
                            let new_assign = ({stmt=Assign((name,NoOffset),{source with exp = Lit(CharLit(c,Char))}); loc=stmt.loc; id=stmt.id}) in
                                EvaluatingStatement(f)::EvaluatingStatement(new_assign)::last
                        ) [EvaluatingStatement({stmt=VarDec([{var=name;typ=t;init=None;loc=stmt.loc}]); loc=stmt.loc; id=stmt.id})] char_list in
                        add_job ((List.rev new_stmts) @ next) sigma
                (*TODO Add Array iteration here*)
                end
        | While(exp,body) ->
            let result_list = evaluate_expression exp sigma in
                List.iter (fun ((BooleanValue(value)),sigma_prime) ->
                    if value then
                        add_job ((EvaluatingStatement(body))::(EvaluatingStatement(stmt))::next) sigma_prime
                    else add_job next sigma_prime
                ) result_list
        | Whenever(exp,body) ->
            let result_list = evaluate_expression exp sigma in
                List.iter (fun ((BooleanValue(value)),sigma_prime) ->
                    add_job next sigma_prime ;
                    let sigma_prime_prime = clone_state sigma_prime in
                    if value then
                    begin
                        (*execute the body*)
                        Printf.printf "whenever was true\n";
                        Printf.printf "adding job: %s\n" (Language.statement_to_str body);
                        add_job ((EvaluatingStatement(body))::(EvaluatingStatement(stmt))::[EvaluationDone]) sigma_prime_prime
                    end
                    else
                        (*get ready to execute the check again*)
                        
                        add_job ((EvaluatingStatement(stmt)) :: [EvaluationDone]) sigma_prime_prime
                ) result_list
        | VarDec(var) ->
            let sigma_prime = List.fold_left (fun sigma_prime dec ->
                let value,sigma_prime_prime = match dec.typ with
                    | Counter -> Some(IntValue(0)), sigma_prime
                    | _ ->
                        match dec.init with
                            | None -> None, sigma_prime
                            (*TODO ARRAY INITS!*)
                            | Some (PrimitiveInit(e)) ->
                                (*TODO THIS COULD CAUSE A MASSIVE ERROR*)
                                let [(tmp,sig_prime)] = evaluate_expression e sigma_prime in
                                    (Some tmp),sig_prime
                in
                    Hashtbl.add sigma_prime_prime.memory dec.var (Variable(dec.var,dec.typ,value)) ;
                    sigma_prime_prime
            ) sigma var in
                add_job next sigma_prime
        | Assign((name,o),exp) ->
            (*TODO THIS COULD BE VERY BAD*)
            let [(value,sigma_prime)] = evaluate_expression exp sigma in
                (*TODO Arrays!*)
                let (Variable(s,t,_)) = Hashtbl.find sigma_prime.memory name in
                Hashtbl.replace sigma_prime.memory name (Variable(s,t,(Some value))) ;
                add_job next sigma_prime
        | ExpStmt(exp) ->
            begin
            let result_list = evaluate_expression exp sigma in
                List.iter(fun (value,sigma_prime) ->
                    match value with
                        | BooleanValue(b) -> if not b then raise Fail else add_job next sigma_prime
                        | _ -> add_job next sigma_prime
                ) result_list
            end
        | MacroCall(a,Arguments(args)) ->
            let names = ref [] in
            let (MacroContainer((Macro(name,Parameters(params),stmt)))) = Hashtbl.find sigma.memory a in
            (*evaluate and assign variables*)
            let sigma_prime = List.fold_left2 (fun sigma_prime (Param(p,t)) arg ->
                (*TODO THIS COULD CAUSE A MASSIVE ERROR*)
                let [(value,sigma_prime_prime)] = evaluate_expression arg sigma_prime in
                    names := p :: !names ;
                    Hashtbl.add sigma_prime_prime.memory p (Variable(p,t,Some value)) ;
                    sigma_prime_prime
            ) sigma params args in
                add_job (EvaluatingStatement(stmt) :: RemoveVariables(!names) :: next) sigma_prime

        
        
        | _ -> Printf.printf "Failing on:%s\n" (Language.statement_to_str stmt); failwith "didn't match"

and evaluate_expression (exp : expression) (sigma : state) : (value * state) list =
    (*Printf.printf "%s\n" (Language.exp_to_str exp) ;*)
    match exp.exp with
        | EQ(a,b)
        | NEQ(a,b)
        | LEQ(a,b)
        | GEQ(a,b)
        | LT(a,b)
        | GT(a,b) ->
            let counter_consume sigma n =
                let rec helper sigma n =
                    if n = 0 then
                        sigma
                    else
                        let v,sigma_prime = consume sigma in
                            helper sigma_prime (n-1)
                in
                let rec consume_first sigma =
                    let (CharValue(v)),sigma_prime = consume sigma in
                    if v = '$' then sigma else consume_first sigma_prime
                in
                helper (consume_first sigma) n
            in
            let [(value_a,sigma_prime)] = evaluate_expression a sigma in
            let [(value_b,sigma_prime_prime)] = evaluate_expression b sigma_prime in
                let sigma_prime_prime_prime = match a.expr_type,b.expr_type with
                    (* we need to consume n+4 input*)
                    | Counter,Int ->
                        let (IntValue(v)) = value_b in
                        counter_consume sigma_prime_prime (v+5)
                    | Int,Counter ->
                        let (IntValue(v)) = value_a in
                        counter_consume sigma_prime_prime (v+5)
                    | _ -> sigma_prime_prime
                in
                let value = match exp.exp with
                    | EQ(_,_) ->
                        begin
                        match value_a,value_b with
                        (*| CharValue(_),AllInValue
                        | AllInValue,CharValue(_) -> Printf.printf "bob" ; true*)
                        | _ -> value_a = value_b
                        end
                    | NEQ(_,_) ->
                        begin
                        match value_a,value_b with
                        (*| CharValue(_),AllInValue
                        | AllInValue,CharValue(_) -> false*)
                        | _ -> value_a <> value_b
                        end
                    | LEQ(_,_) -> value_a <= value_b
                    | GEQ(_,_) -> value_a >= value_b
                    | LT(_,_) -> value_a < value_b
                    | GT(_,_) -> value_a > value_b
                    
                in
                    [(BooleanValue(value),sigma_prime_prime_prime)]
        | Not(a) ->
            let result_list = evaluate_expression a sigma in
                List.map (fun (BooleanValue(b),sigma_prime) -> (BooleanValue(not b)),sigma_prime) result_list
            
        | Negative(a) ->
            let [(IntValue(i),sigma_prime)] = evaluate_expression a sigma in
            [((IntValue(- i)),sigma_prime)]
        | And(a,b) ->
            (*TODO Make this actually fail early?*)
            let result_list = evaluate_expression a sigma in
                List.flatten (List.map (fun ((BooleanValue(b1)),sigma_prime) ->
                    let result_list2 = evaluate_expression b sigma_prime in
                    List.map (fun ((BooleanValue(b2)),sigma_prime_prime) ->
                        (BooleanValue(b1 && b2)),sigma_prime_prime
                    ) result_list2
                ) result_list)
                    
        | Or(a,b) ->
            (*TODO make this actually work like or*)
            (evaluate_expression a sigma) @ (evaluate_expression b sigma)
            (*let (BooleanValue(b1)),sigma_prime = evaluate_expression a sigma in
            let (BooleanValue(b2)),sigma_prime_prime = evaluate_expression b sigma in
                Printf.printf "%s : %b ; %b \n" (Language.exp_to_str exp) (b1) (b2) ;
                (BooleanValue(b1 || b2)),sigma_prime_prime*)
        | Plus(a,b)
        | Minus(a,b)
        | Times(a,b)
        | Mod(a,b) ->
            
            let [((IntValue(value_a)),sigma_prime)] = evaluate_expression a sigma in
            let [((IntValue(value_b)),sigma_prime_prime)] = evaluate_expression b sigma_prime in
                let value = match exp.exp with
                    | Plus(_,_) -> value_a + value_b
                    | Minus(_,_) -> value_a - value_b
                    | Times(_,_) -> value_a * value_b
                    | Mod(_,_) -> value_a mod value_b
                
                in
                    [(IntValue(value),sigma_prime_prime)]
        | Lval((name,o)) ->
            (*TODO ARRAYS!*)
            let (Variable(_,_,Some v)) = Hashtbl.find sigma.memory name in
            (*Printf.printf "VAL: %s\n" (val_to_string v) ;*)
            [(v,sigma)]
        | Lit(l) ->
            let value =
                match l with
                    | StringLit(s,_) -> StringValue(s)
                    | CharLit(c,_) -> CharValue(c)
                    | IntLit(i,_) -> IntValue(i)
                    | True -> BooleanValue(true)
                    | False -> BooleanValue(false)
                    (*| AllIn -> AllInValue*)
                    | _ -> Printf.printf "failed with value: %s\n" (exp_to_str exp) ; failwith "failure"
            in
                [(value,sigma)]
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
                    [(value,sigma)]
        | Input -> [(consume sigma)]
    
(*and evaluate_macro ((Macro(name,Parameters(params),stmt)) : macro) (args:expression list) (sigma : state) : state =
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
    end*)
    
and evaluate_network (Network(params,({stmt=(Block(b)); id=ast_id}))) sigma next =
    List.iter (fun s ->
        let start_sigma = clone_state sigma in
        add_job (EvaluatingStatement(s) :: next) start_sigma
    ) b
    
let simulate (Program(macros,net)) =
    begin
    try
    (* Add the macros to the state*)
    List.iter (fun ((Macro(name,params,stmts)) as m) ->
                    Hashtbl.add !original_sigma.memory name (MacroContainer(m))) macros
    ;
    List.iteri (fun i c ->
        (*indexing starts at 1, not 0*)
        let i = i + 1 in
        if c = Automata.start_of_input then
            add_job [EvaluatingNetwork]
                { !original_sigma with
                    index=i+1;
                    input = sublist i !original_sigma.input
                }
        ) !original_sigma.input ;
    while not (Queue.is_empty jobs) do
        (*Printf.printf "size:%d\n" (Queue.length jobs) ;*)
        let where,next,sigma = Queue.take jobs in
        try
        match where with
            | EvaluationDone -> ()
            | EvaluatingStatement(s) -> evaluate_statement s sigma next
            | EvaluatingNetwork -> evaluate_network net sigma next
            | RemoveVariables(l) ->
                begin
                List.iter (fun s -> Hashtbl.remove sigma.memory s) l ;
                add_job next sigma
                end
            | Converge ->
                begin
                match next with
                    | [] -> ()
                    | hd :: tl ->
                        (* TODO Add an assertiomn here about state convergence and memory *)
                        if not (in_queue sigma.index (List.hd next)) then
                            add_job next sigma
                        else
                            ()
                end
        with Fail -> add_job [EvaluationDone] sigma
    done
    
    ;
    print_reports !original_sigma
    with Failure _ -> print_reports !original_sigma
    end;;

let process program =
    (*print_endline (Language.program_to_str program) ;
    print_endline "~~~~~~~~~~~~~~~~~~~~~~~~" ;
    let lowered_program = Intermediate.intermediate program in
        print_endline (Language.program_to_str lowered_program)*)
    (*let program = Intermediate.intermediate program in*)
    let program_t = Tc.check program in
        simulate program_t
        
let read_program (name : string) =
    try
        let channel = open_in name in
        let lexbuf = Lexing.from_channel channel in
            flush stdout;
            let return =
                try
                Parse.program Lex.initial lexbuf
                with exn ->
                    begin
                      let curr = lexbuf.Lexing.lex_curr_p in
                      let line = curr.Lexing.pos_lnum in
                      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
                      let tok = Lexing.lexeme lexbuf in
                      Printf.printf "Parsing Error: \n";
                      Printf.printf "line: %d\n" line ;
                      Printf.printf "col: %d\n" cnum ;
                      Printf.printf "tok: %s\n" tok;
                      exit(-1)
                    end
            in
            close_in_noerr channel ; return
    with Sys_error _ -> begin
        Printf.printf "Failed to open %s\n" name ; exit (-1)
        end

let read_input (name : string) =
    let rec build_list l channel : char list =
        try
            let c = input_char channel in
            build_list (c::l) channel
        with End_of_file ->  l 
        in
    try
        let channel = open_in name in
        (*Cheat to get the queue working nicely*)
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
        original_sigma := {
            input = program_input ;
            index = 1;
            memory = Hashtbl.create 255 ;
            report = Hashtbl.create (String.length !input)
        }; process program
        