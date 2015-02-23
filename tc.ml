(*
 * Kevin Angstadt
 * Type Checking functions
 *)
 
open Language

exception Type_error of string
exception Type_mismatch
exception Macro_error of string
exception Var_error of string

let macro_map : (string, param list) Hashtbl.t = Hashtbl.create 255

let macro_map_lookup m =
    try
        Hashtbl.find macro_map m
    with Not_found -> raise (Macro_error (Printf.sprintf "Macro %s not found." m))

let var_map : (string, typ) Hashtbl.t = Hashtbl.create 255

let var_map_lookup v =
    try
        Hashtbl.find var_map v
    with Not_found -> raise (Var_error (Printf.sprintf "Variable %s is not in scope." v))

let get_lval_type (l,o) =
    let rec modify_type t o =
        match o with
            | NoOffset -> t
            | Index(_,o2) -> match t with
                | Array(t2) -> modify_type t2 o2
    in
    let t = var_map_lookup l in
        modify_type t o

(* TODO Does this really make sense to approach this way?  I suppose
 * since this is only for convenience, but more consideration would
 * be desirable.
 *)
let get_type exp =
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
        | Input -> Automata
            
(* Call this function to check the types of a program.  Will return an
 * annotated AST with types for all of the expressions *)
let check (Program(macros,network)) : program =    
    (* Recursively check the type of an expression
     * The returned value is an expression with the appropriate
     * type annotation
     *)
    let rec check_exp exp : expression =
        match exp with
            | EQ(a,b,_)
            | NEQ(a,b,_) ->
                let at = check_exp a in
                let bt = check_exp b in
                begin
                let typ = match (get_type at),(get_type bt) with
                    | Boolean,Boolean
                    | Char,Char
                    | Int,Int -> Boolean
                    | Counter,Int
                    | Int,Counter -> Counter
                    | Automata, Char
                    | Char, Automata -> Automata
                in match exp with
                    | EQ(_,_,_) -> EQ(at,bt,Some typ)
                    | NEQ(_,_,_) -> NEQ(at,bt,Some typ)
                end
            | LEQ(a,b,_)
            | GEQ(a,b,_)
            | LT(a,b,_)
            | GT(a,b,_) ->
                let at = check_exp a in
                let bt = check_exp b in
                begin
                let typ = match (get_type at),(get_type bt) with
                    | Int,Int -> Boolean
                    | Counter,Int
                    | Int,Counter -> Counter
                in match exp with
                    | LEQ(_,_,_) -> LEQ(at,bt,Some typ)
                    | GEQ(_,_,_) -> GEQ(at,bt,Some typ)
                    | LT(_,_,_) -> LT(at,bt,Some typ)
                    | GT(_,_,_) -> GT(at,bt,Some typ)
                end
            | And(a,b,_)
            | Or(a,b,_) ->
                let at = check_exp a in
                let bt = check_exp b  in
                begin
                let typ = match (get_type at),(get_type bt) with
                    | Boolean,Boolean -> Boolean
                    | Automata,Automata -> Automata
                in match exp with
                    | And(_,_,_) -> And(at,bt,Some typ)
                    | Or(_,_,_) -> Or(at,bt,Some typ)
                end
            | Not(a,_) ->
                let at = check_exp a in
                begin
                let typ = match (get_type at) with
                    | Boolean -> Boolean
                    | Automata -> Automata
                in Not(at,Some typ)
                end
            | Negative(a) ->
                let at = check_exp a in
                begin
                let typ = match (get_type at) with
                    | Int -> Int
                in Negative(at)
                end
            | Lval(var,_) -> Lval(check_lval var, Some (get_lval_type var))
            | Lit(a) -> exp
            (* TODO handle checking arguments at some point *)
            | Fun(a,b,c,_) ->
                let var = check_lval a in
                let t = get_lval_type var in
                begin match t with
                    | String ->
                        begin match b with
                            | "length" -> Fun(var,b,c,Some Int)
                        end
                    | Counter ->
                        begin match b with
                            | "count" -> Fun(var,b,c,Some Automata)
                            | "reset" -> Fun(var,b,c,Some Automata)
                        end
                end
            | Input -> exp
    and check_lval ((l,o) as var) =
        match (get_lval_type var) with
            | String
            | Int
            | Char
            | Counter
            | Automata
            | Boolean ->
                begin match o with
                | Index(_,_) -> raise (Type_error "It is not possible to index a simple type")
                | _ -> var
                end
            | Array(t) ->
                (* TODO Handle recursive lvals ie x[][]..*)
                begin match o with
                | Index(e,o2) ->
                    let exp = check_exp e in
                    let typ = get_type exp in
                    begin
                    match typ with 
                    | Int -> (l,Index(exp,o2))
                    | _ -> raise (Type_error "Array index must be of int type.")
                    end
                | NoOffset -> var
                end
    in
    
    (* Recursive checker for statements...returns new Statement with type annotations*)
    let rec check_statement stmt =
        match stmt with
            | Report -> stmt
            | Block(stmts) ->
                let rev = List.fold_left (fun list s -> (check_statement s) :: list) [] stmts in
                Block(List.rev rev)
            | If(exp,s1,_)
            | While(exp,s1) ->
                let exp_t = check_exp exp in
                let s1_t = check_statement s1  in
                begin match stmt with
                    | If(_,_,s2) ->
                        let s2_t = check_statement s2 in
                        If(exp_t,s1_t,s2_t)
                    | _ -> While(exp_t, s1_t)
                end
            | ForEach((Param((l,o),t) as p),e,s) ->
                begin
                match o with
                | NoOffset -> 
                    let e_t = check_exp e in
                    let _ = match (get_type e_t) with
                        | String -> if t <> Char then raise (Type_error "Iterating over a string requires a char parameter.")
                        | Array(t2) -> if t <> t2 then raise (Type_error "ForEach loop type error.")
                    in Hashtbl.add var_map l t ;
                    let return = check_statement s in
                    Hashtbl.remove var_map l ;
                    return
                | _ -> raise (Type_error "Variable cannot be indexed.")
                end
            | VarDec(vars) ->
                (* TODO Don't allow Counters inside of arrays *)
                let vars = List.map (fun (((n,o),typ,i) as dec) ->
                    Hashtbl.add var_map n typ ;
                    match i with
                    | None -> dec
                    | Some x -> match x with
                        | PrimitiveInit(e) ->
                            let e_t = check_exp e in
                            let t = get_type e_t in
                            if typ = t then
                                ((n,o),typ,Some (PrimitiveInit(e_t)))
                            else raise (Type_error "Initialized value does not match type.")
                        (*TODO handle type checking for array initializers!*)
                ) vars in
                VarDec(vars)
            | Assign(n,e) ->
                let e_t = check_exp e in
                let t = get_lval_type n in
                let t2 = get_type e_t in
                if t <> t2 then raise (Type_error "The expression being assigned must match the type of the variable.")
                else Assign(n,e_t)
            | ExpStmt(e) ->
                begin match e with
                    | None -> stmt
                    | Some exp -> ExpStmt(Some (check_exp exp))
                end
            | MacroCall(a,Arguments(b)) ->
                let params = macro_map_lookup a in
                (*make sure the arguments are correct*)  
                let args =
                    begin try
                    List.map2 (fun (arg:expression) (Param(l,t2)) ->
                        let arg_t = check_exp arg in
                        let t = get_type arg_t in
                        if t <> t2 then raise Type_mismatch
                        else
                        arg_t
                    ) b params
                    with Invalid_argument(_) -> raise (Syntax_error "invalid number of arguments")
                    end
                in
                MacroCall(a,Arguments(args))
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
            (* verify that there are nod duplicate params
               and add the params to the var_map *)
            List.iter (fun (Param((s,o),t)) ->
                try
                    var_map_lookup s ;
                    raise (Var_error (Printf.sprintf "Parameter %s has already been declared." s))
                with Var_error(_) -> begin
                    Hashtbl.add var_map s t
                end
            ) params
            ;
            let stmt_t = match stmt with
                | Block(_) -> check_statement stmt
                | _ -> raise (Syntax_error "A block must follow a macro declaration")
            in
            (*reset the var_map*)
            (Hashtbl.reset var_map
            ;
            Macro(name,p,stmt_t))
            end
        end

        
    in
    
    let check_network (Network(params,stmt)) =
        let verify_network_params (Parameters(p)) =
            List.iter (fun param ->
                match param with
                    | Param((n,_),typ) ->
                        begin
                        match typ with
                            | Counter -> raise (Type_error "Counters cannot be passed to a network")
                            | _ -> Hashtbl.add var_map n typ ; ()
                        end
            ) p in
        match stmt with
            | Block(_) -> Network(params,check_statement stmt)
            | _ -> raise (Syntax_error "A block must follow the network declaration.")
    in
    
    (* First, let's type check the macros *)
    let macros_t = List.map check_macro macros in
    (* Next, let's type check the network *)
    let network_t = check_network network in
    Program(macros_t,network_t)