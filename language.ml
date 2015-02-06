(*
 * Kevin Angstadt
 * Abstract Syntax for AP Language
 *)
 
module StringSet = Set.Make(String)

exception Syntax_error
exception Type_mismatch
exception Uninitialized_variable
exception Negative_count

type typ =
    | String
    | Int
    | Char
    | Counter
    | List
    | Automata
    | Boolean

type literal =
    | StringLit of string * typ
    | IntLit of int * typ
    | CharLit of char * typ
    | True
    | False

type value =
    | StringValue of string
    | IntValue of int
    | CharValue of char
    | BooleanValue of bool
    | AutomataElement of Automata.element

type expression =
    | EQ of expression * expression                     (* a0 == a1 *)
    | NEQ of expression * expression                    (* a0 != a1 *)
    | LEQ of expression * expression                    (* a0 <= a2 *)
    | GEQ of expression * expression                    (* a0 >= a2 *)
    | LT of expression * expression                     (* a0 < a2 *)
    | GT of expression * expression                     (* a0 > a2 *) 
    | Not of expression                                 (* !b *)
    | Negative of expression                            (* -a0 *)
    | And of expression * expression                    (* b0 && b1 *) 
    | Or of expression * expression                     (* b0 || b1 *)
    | Plus of expression * expression
    | Minus of expression * expression
    | Times of expression * expression
    | Mod of expression * expression
    | Var of string
    | ListVar of expression * expression option
    | Lit of literal
    | Fun of string * string * arguments                (* var.fun(args)*)
    | Input

and arguments = Arguments of expression list

type param = Param of string * typ
    
type parameters = Parameters of param list

type scope = MacroScope | NetworkScope

type initialize =
    | PrimitiveInit of  expression
    | ArrayInit of initialize list

type statement =
    | Report of scope
    | Block of statement list * scope 
    | IfWhile of expression * statement * statement * bool * scope
    | ForEach of param * expression * statement * scope
    | While of expression * statement * scope
    | VarDec of (expression * typ * initialize option) list * scope
    | Assign of string * expression * scope
    | ExpStmt of expression option * scope
    | MacroCall of string * arguments * scope

type macro = Macro of string * parameters * statement

type network = Network of parameters * statement

type program = Program of macro list * network

(* Used in the Symbol Table *)
type container =
    | MacroContainer of macro
    | Variable of string * typ * value option
type exp_return =
    | AutomataExp of Automata.element list
    | CounterExp of (string * int * Automata.element * StringSet.t * StringSet.t * string)
    | BooleanExp of bool
    | IntExp of int
type symbol = (string, container) Hashtbl.t


(* Printing functions *)

open Printf

let rec program_to_str (Program(macros,network)) = sprintf "%s %s" (List.fold_left (fun prev a -> (prev) ^ (sprintf "%s" (macro_to_str a) )) "" macros) (network_to_str network)

and network_to_str (Network(params,stmts)) = sprintf "network ( %s ) \n %s "  (params_to_str params) (statement_to_str stmts)

and macro_to_str (Macro(name,params,stmts)) = sprintf "macro %s ( %s ) \n %s " name (params_to_str params) (statement_to_str stmts)

and typ_to_str t = match t with
    | String -> "String"
    | Int ->    "int"
    | Char ->   "char"
    | Counter ->"Counter"
    | List ->   "List"

and init_to_str i = match i with
    | PrimitiveInit(e) -> exp_to_str e
    | ArrayInit(a) -> sprintf "{ %s }" (List.fold_left (fun prev e -> sprintf "%s, %s" prev (init_to_str e)) "" a)
    
and param_to_str (Param(a,t)) = sprintf "%s %s" (typ_to_str t) a 

and params_to_str (Parameters(a)) = List.fold_left (fun prev v -> (prev) ^ (sprintf "%s, " (param_to_str v))) "" a

and args_to_str (Arguments(a)) = List.fold_left (fun prev v -> (prev) ^(sprintf "%s, " (exp_to_str v))) "" a

and statement_to_str (a : statement) = match a with
    | Report(_) -> "report;\n"
    | Block(b,_) -> sprintf "{ \n %s }\n" (List.fold_left (fun prev s -> (sprintf "%s %s" prev (statement_to_str s))) "" b)
    | IfWhile(exp,t,e,w,_) -> if not w then sprintf "if ( %s ) \n %s else \n %s" (exp_to_str exp) (statement_to_str t) (statement_to_str e)
                              else sprintf "while( %s ) \n %s" (exp_to_str exp) (statement_to_str t)
    | ForEach(var,exp,s,_) -> sprintf "foreach( %s : %s )\n %s" (param_to_str var) (exp_to_str exp) (statement_to_str s)
    | VarDec(var,_) -> List.fold_left (fun prev (s,t,i) ->
                                    let s = match s with
                                        | Var(a) -> a
                                        | ListVar(a,None) -> sprintf "%s[]" (exp_to_str a)
                                        | ListVar(a,Some b) -> sprintf "%s[%s]" (exp_to_str a) (exp_to_str b)
                                    in
                                    let new_var = match i with
                                        | None -> sprintf "%s %s;\n" (typ_to_str t) s
                                        | Some x -> sprintf "%s %s = %s;\n" (typ_to_str t) s (init_to_str x)
                                    in prev ^ new_var
                                 ) "" var
    | MacroCall(a,b,_)  -> sprintf "%s(%s);"   a (args_to_str b)
    | ExpStmt(exp,_) ->
        match exp with
        | Some(e) -> sprintf "%s;" (exp_to_str e)
        | None -> "NOP"

and exp_to_str exp = match exp with
    | EQ(a,b)       -> sprintf "(%s == %s)" (exp_to_str a) (exp_to_str b)
    | NEQ(a,b)      -> sprintf "(%s != %s)" (exp_to_str a) (exp_to_str b)
    | LEQ(a,b)      -> sprintf "(%s <= %s)" (exp_to_str a) (exp_to_str b)
    | GEQ(a,b)      -> sprintf "(%s >= %s)" (exp_to_str a) (exp_to_str b)
    | LT(a,b)       -> sprintf "(%s < %s)"  (exp_to_str a) (exp_to_str b)
    | GT(a,b)       -> sprintf "(%s > %s)"  (exp_to_str a) (exp_to_str b)
    | Not(a)        -> sprintf "(!%s)"      (exp_to_str a)
    | Negative(a)   -> sprintf "-(%s)"    (exp_to_str a)
    | And(a,b)      -> sprintf "(%s && %s)" (exp_to_str a) (exp_to_str b)
    | Or(a,b)       -> sprintf "(%s || %s)" (exp_to_str a) (exp_to_str b)
    | Plus(a,b)     -> sprintf "(%s + %s)"  (exp_to_str a) (exp_to_str b)
    | Minus(a,b)    -> sprintf "(%s - %s)"  (exp_to_str a) (exp_to_str b)
    | Times(a,b)    -> sprintf "(%s * %s)"  (exp_to_str a) (exp_to_str b)
    | Mod(a,b)      -> sprintf "(%s %% %s)" (exp_to_str a) (exp_to_str b)
    | Var(a)        -> a
    | Lit(a)        -> begin match a with
                        | StringLit(x,typ) -> sprintf "\"%s\"" x
                        | IntLit(x,typ)    -> sprintf "%d" x
                        | CharLit(x,typ)   -> sprintf "%c" x
                        | True             -> "true"
                        | False            -> "false"
                        end
    | Fun(a,b,c)    -> sprintf "%s.%s(%s)" a b (args_to_str c)
    | Input         -> "input"
    


