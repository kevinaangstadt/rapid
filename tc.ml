(*
 * Kevin Angstadt
 * Type Checking functions
 *)
 
open Language

exception Type_error of string
exception Type_mismatch
exception Macro_error of string
exception Var_error of string

module StringMap = Map.Make(String)

let macro_map : (string, param list) Hashtbl.t = Hashtbl.create 255

let macro_map_lookup m =
    try
        Hashtbl.find macro_map m
    with Not_found -> raise (Macro_error (Printf.sprintf "Macro %s not found." m))

type environment = (typ*vardec option) StringMap.t

let var_map : environment = StringMap.empty

let var_map_lookup v map : typ*vardec option =
    try
        StringMap.find v map
    with Not_found -> raise (Var_error (Printf.sprintf "Variable %s is not in scope." v))

let get_lval_type (l,o) gamma =
    let rec modify_type t o =
        match o with
            | NoOffset -> t
            | Index(_,o2) -> match t with
                | Array(t2) -> modify_type t2 o2
    in
    let t,_ = var_map_lookup l gamma in
        modify_type t o

(* Update a VarDec to be an internal DoubleCounter type *)
let set_double_counter exp t gamma =
    match exp.exp with
        | Lval(s,_) ->
            let _,(Some dec) = var_map_lookup s gamma in
                match t with
                    | EQ(_,_) -> dec.typ <- DoubleCounter(true)
                    | NEQ(_,_) -> dec.typ <- DoubleCounter(false)
            
        | _ -> failwith "unreachable: set_double_counter"

(* TODO Does this really make sense to approach this way?  I suppose
 * since this is only for convenience, but more consideration would
 * be desirable.
 *)
(*let get_type exp =
    match exp with
        | EQ(_,_,Some t)
        | NEQ(_,_,Some t)
        | LEQ(_,_,Some t)
        | GEQ(_,_,Some t)
        | LT(_,_,Some t)
        | GT(_,_,Some t)
        | And(_,_,Some t)
        | Or(_,_,Some t)
        | Plus(_,_,Some t)
        | Minus(_,_,Some t)
        | Times(_,_,Some t)
        | Mod(_,_,Some t)
        | Fun(_,_,_,Some t)
        | Not(_,Some t)
        | Fun(_,_,_,Some t)
        | Lval(_,Some t) -> t
        | Negative(_) -> Int
        | Lit(l) ->
            begin match l with
            | StringLit(_,_) -> String
            | IntLit(_,_) -> Int
            | CharLit(_,_) -> Char
            | True | False -> Boolean
            end
        | Input -> Automata*)
            
(* Call this function to check the types of a program.  Will return an
 * annotated AST with types for all of the expressions *)
let check (Program(macros,network) as p) : program =    
    (* Recursively check the type of an expression
     * The returned value is an expression with the appropriate
     * type annotation
     *)
    let rec check_exp exp gamma : environment =
        match exp.exp with
            | EQ(a,b)
            | NEQ(a,b) ->
                let at = check_exp a gamma in
                let bt = check_exp b at in
                begin
                let typ = match a.expr_type,b.expr_type with
                    | Boolean,Boolean
                    | Char,Char
                    | Int,Int -> Boolean
                    | Counter,Int -> set_double_counter a exp.exp bt; Counter
                    | Int,Counter -> set_double_counter b exp.exp bt; Counter
                    | Automata, Char
                    | Char, Automata -> Automata
                in
                    exp.expr_type <- typ ; bt
                end
            | LEQ(a,b)
            | GEQ(a,b)
            | LT(a,b)
            | GT(a,b) ->
                let at = check_exp a gamma in
                let bt = check_exp b at in
                begin
                let typ = match a.expr_type,b.expr_type with
                    | Int,Int -> Boolean
                    | Counter,Int
                    | Int,Counter -> Counter
                in
                    exp.expr_type <- typ ; bt
                end
            | Plus(a,b)
            | Minus(a,b)
            | Times(a,b)
            | Mod(a,b) ->
                let at = check_exp a gamma in
                let bt = check_exp b at in
                begin
                let typ = match a.expr_type,b.expr_type with
                    | Int,Int -> Int
                in
                    exp.expr_type <- typ ; bt
                end
            | And(a,b)
            | Or(a,b)
            | PAnd(a,b) ->
                let at = check_exp a gamma in
                let bt = check_exp b at in
                begin
                let typ = match a.expr_type,b.expr_type with
                    | Boolean,Boolean -> Boolean
                    | Automata,Automata -> Automata
                    | Counter,Counter -> Counter
                    | _ -> raise (Type_error "unsupported combination of expression types")
                in
                    exp.expr_type <- typ ; bt
                end
            | Not(a) ->
                let at = check_exp a gamma in
                begin
                let typ = match a.expr_type with
                    | Boolean -> Boolean
                    | Automata -> Automata
                    | Counter -> Counter
                    | _ -> failwith (Printf.sprintf "failed: %s\n" (Language.exp_to_str exp))
                in exp.expr_type <- typ ; at
                end
            | Negative(a) ->
                let at = check_exp a gamma in
                begin
                let typ = match a.expr_type with
                    | Int -> Int
                in exp.expr_type <- typ ; at
                end
            | Lval(var) ->
                let gamma_prime = check_lval var gamma in
                exp.expr_type <- get_lval_type var gamma_prime ;
                gamma_prime
            | Lit(a) ->
                let typ = match a with
                    | StringLit(_,t)
                    | IntLit(_,t)
                    | CharLit(_,t) -> t
                    | True | False -> Boolean
                    | AllIn | StartIn -> Char
                in exp.expr_type <- typ ; gamma
            (* TODO handle checking arguments at some point *)
            | Fun(a,b,c) ->
                let gamma_prime = check_lval a gamma in
                let t = get_lval_type a gamma_prime in
                let typ = begin match t with
                    | String ->
                        begin match b with
                            | "length" -> Int
                        end
                    | Counter ->
                        begin match b with
                            | "count"
                            | "reset" -> Automata
                        end
                end
                in exp.expr_type <- typ ; gamma_prime
            | Input -> exp.expr_type <- Automata ; gamma
    and check_lval ((l,o) as var) gamma : environment =
        match (get_lval_type var gamma) with
            | String
            | Int
            | Char
            | Counter
            | DoubleCounter(_)
            | Automata
            | Boolean ->
                begin match o with
                | Index(_,_) -> raise (Type_error "It is not possible to index a simple type")
                | _ -> gamma
                end
            | Array(t) ->
                (* TODO Handle recursive lvals ie x[][]..*)
                begin match o with
                | Index(e,o2) ->
                    let gamma_prime = check_exp e gamma in
                    begin
                    match e.expr_type with 
                    | Int -> gamma_prime
                    | _ -> raise (Type_error "Array index must be of int type.")
                    end
                | NoOffset -> gamma
                end
    in
    
    (* Recursive checker for statements...returns new Statement with type annotations*)
    let rec check_statement stmt gamma : environment =
        match stmt with
            | Report -> gamma
            | Break -> gamma
            | Block(stmts) ->
                let _ = List.fold_left (fun gamma_prime s -> check_statement s gamma_prime) gamma stmts in
                gamma
            | If(exp,s1,_)
            | While(exp,s1)
            | Whenever(exp,s1)->
                let exp_t = check_exp exp gamma in
                let s1_t = check_statement s1 exp_t  in
                begin match stmt with
                    | If(_,_,s2) -> check_statement s2 s1_t
                    | Whenever(_,_) ->
                        begin
                        (*EXP can only be Automata*)
                        match exp.expr_type with
                            | Automata -> s1_t
                            | Counter -> s1_t
                            | _ -> raise (Type_error "Whenever statements can only have Automata or Counter expressions")
                        end
                    | _ -> s1_t
                end
            | Either(stmts) ->
                List.iter(fun block ->
                    match block with
                     | Block(_) -> check_statement block gamma ; ()
                     | _ -> raise (Syntax_error "An either or orelse token must be followed by a block")
                ) stmts ; gamma
            | ForEach((Param(l,t) as p),e,s)
            | SomeStmt((Param(l,t) as p),e,s) ->
                begin
                let e_t = check_exp e gamma in
                let _ = match e.expr_type with
                    | String -> if t <> Char then raise (Type_error "Iterating over a string requires a char parameter.")
                    | Array(t2) -> if t <> t2 then raise (Type_error "ForEach/Some loop type error.")
                in
                let gamma_prime = StringMap.add l (t,None) e_t in
                let _ = check_statement s gamma_prime in
                gamma
                end
            | VarDec(vars) ->
                (* TODO Don't allow Counters inside of arrays *)
                (* n -> .var
                   typ -> .typ
                   i -> .init *)
                List.fold_left (fun gamma_prime dec ->
                    match dec.init with
                    | None -> StringMap.add dec.var (dec.typ,Some dec) gamma_prime
                    | Some x -> match x with
                        | PrimitiveInit(e) ->
                            let gamma_prime = check_exp e gamma_prime in
                            if dec.typ = e.expr_type then
                                StringMap.add dec.var (dec.typ,Some dec) gamma_prime
                            else raise (Type_error "Initialized value does not match type.")
                        | ArrayInit(e) ->
                            let rec check_array a =
                                match a with
                                    | PrimitiveInit(e) -> check_exp e gamma_prime ; ()
                                    | ArrayInit(e) -> List.iter (fun a -> check_array a ; ()) e
                            in
                            print_endline "TC Warning: Array init not checked";
                            check_array x ;
                            StringMap.add dec.var (dec.typ,Some dec) gamma_prime
                        (*TODO handle type checking for array initializers!*)
                ) gamma (List.rev vars)
            | Assign(n,e) ->
                let gamma_prime = check_exp e gamma in
                let t = get_lval_type n gamma in
                if t <> e.expr_type then raise (Type_error "The expression being assigned must match the type of the variable.")
                else gamma_prime
            | ExpStmt(e) -> check_exp e gamma
            | MacroCall(a,Arguments(b)) ->
                let params = macro_map_lookup a in
                (*make sure the arguments are correct*)  
                begin try
                List.fold_left2 (fun gamma_prime (arg:expression) (Param(l,t)) ->
                    let gamma_prime = check_exp arg gamma_prime in
                    if t <> arg.expr_type then raise Type_mismatch
                    else
                    gamma_prime
                ) gamma b params
                with Invalid_argument(_) -> raise (Syntax_error (Printf.sprintf "invalid number of arguments in call to %s" a))
                end
                
    in
    
    let check_macro (Macro(name,(Parameters(params) as p),stmt) as m) =
        begin
        (* Check to see if macro is a duplicate*)
        try
            macro_map_lookup name ; raise (Macro_error (Printf.sprintf "Macro %s has already been declared." name))
        with Macro_error(_) -> 
            begin
            (* Add macro to the macro table*)
            Hashtbl.add macro_map name params
            ;
            (* verify that there are not duplicate params
               and add the params to the var_map *)
            let gamma = List.fold_left (fun gamma_prime (Param(s,t)) ->
                    if StringMap.mem s gamma_prime then
                        raise (Var_error (Printf.sprintf "Parameter %s has already been declared." s))
                    else
                        StringMap.add s (t,None) gamma_prime
            ) var_map params
            in
            match stmt with
                | Block(_) -> check_statement stmt gamma
                | _ -> raise (Syntax_error "A block must follow a macro declaration")
            end
            ;
            ()
        end

        
    in
    
    let check_network (Network(params,stmt)) =
        let verify_network_params (Parameters(p)) gamma =
            List.fold_left (fun gamma_prime (Param(n,typ)) ->
                match typ with
                    | Counter -> raise (Type_error "Counters cannot be passed to a network")
                    | _ -> StringMap.add n (typ,None) gamma_prime
            ) gamma p in
        match stmt with
            | Block(_) -> check_statement stmt (verify_network_params params var_map)
            | _ -> raise (Syntax_error "A block must follow the network declaration.")
    in
    
    (* First, let's type check the macros *)
    List.iter check_macro macros ;
    (* Next, let's type check the network *)
    check_network network ;
    (*just return the program ast*)
    p