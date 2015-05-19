(*
 * Kevin Angstadt
 * Abstract Syntax for AP Language
 *)

open Util

module StringSet = Set.Make(String)

exception Syntax_error of string
exception Uninitialized_variable
exception Negative_count

type typ =
    | String
    | Int
    | Char
    | Counter
    | Array of typ
    | Automata
    | Boolean
    | NoType (*Internal*)

type literal =
    | StringLit of string * typ
    | IntLit of int * typ
    | CharLit of char * typ
    | True
    | False

type expression = {
    exp : expression_kind ;
    mutable expr_type : typ ;
    loc : loc ;
}

and expression_kind =
    | EQ of expression * expression                 (* a0 == a1 *)
    | NEQ of expression * expression                (* a0 != a1 *)
    | LEQ of expression * expression                (* a0 <= a2 *)
    | GEQ of expression * expression                (* a0 >= a2 *)
    | LT of expression * expression                 (* a0 < a2 *)
    | GT of expression * expression                 (* a0 > a2 *) 
    | Not of expression                             (* !b *)
    | Negative of expression                        (* -a0 *)
    | And of expression * expression                (* b0 && b1 *)
    | PAnd of expression * expression               (* b0 & b1 *)
    | Or of expression * expression                 (* b0 || b1 *)
    | Plus of expression * expression
    | Minus of expression * expression
    | Times of expression * expression
    | Mod of expression * expression
    | Lval of lval
    | Lit of literal
    | Fun of lval * string * arguments              (* var.fun(args)*)
    | Input

and arguments = Arguments of expression list

and offset =
    | NoOffset
    | Index of expression * offset

and lval = string * offset

type param = Param of string * typ
    
type parameters = Parameters of param list

type initialize =
    | PrimitiveInit of  expression
    | ArrayInit of initialize list

type statement =
    | Report
    | Break
    | Block of statement list 
    | If of expression * statement * statement
    | Either of statement list
    | SomeStmt of param * expression * statement
    | Allof of statement list
    | ForEach of param * expression * statement
    | While of expression * statement
    | VarDec of (string * typ * initialize option) list
    | Assign of lval * expression
    | ExpStmt of expression 
    | MacroCall of string * arguments

type macro = Macro of string * parameters * statement

type network = Network of parameters * statement

type program = Program of macro list * network

(* Used in the Symbol Table *)
type array_type =
    | IntArray of int array
    | StringArray of string array
    | CharArray of char array
    | BooleanArray of bool array

type value =
    | StringValue of string
    | IntValue of int
    | CharValue of char
    | BooleanValue of bool
    | AutomataElement of Automata.element
    | ArrayValue of value option array
    | AbstractValue of Config.size
    
type container =
    | MacroContainer of macro
    | Variable of string * typ * value option
type exp_return =
    | AutomataExp of Automata.element list
    | CounterExp of (string * int * Automata.element * StringSet.t * StringSet.t * string * Automata.at_target)
    | BooleanExp of bool
    | IntExp of int
    | CharExp of char
    | StringExp of string
    | ArrayExp of value option array
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
    | Boolean ->"bool"
    | NoType -> "ERROR!"
    | Automata->"Automata(internal)"
    | Array(t)->typ_to_str t ^ "[]"

and init_to_str i = match i with
    | PrimitiveInit(e) -> exp_to_str e
    | ArrayInit(a) -> sprintf "{ %s }" (List.fold_left (fun prev e -> sprintf "%s, %s" prev (init_to_str e)) "" a)

and lval_to_str (n,o) =
    let rec offset_to_str o = match o with
        | NoOffset -> ""
        | Index(e,o2) -> sprintf "[%s]%s" (exp_to_str e) (offset_to_str o2)
    in sprintf "%s%s" n (offset_to_str o)
    
and param_to_str (Param(a,t)) = sprintf "%s %s" (typ_to_str t) (a) 

and params_to_str (Parameters(a)) = List.fold_left (fun prev v -> (prev) ^ (sprintf "%s, " (param_to_str v))) "" a

and args_to_str (Arguments(a)) = List.fold_left (fun prev v -> (prev) ^(sprintf "%s, " (exp_to_str v))) "" a

and statement_to_str (a : statement) = match a with
    | Break -> "break;\n"
    | Report -> "report;\n"
    | Block(b) -> sprintf "{ \n %s }\n" (List.fold_left (fun prev s -> (sprintf "%s     %s" prev (statement_to_str s))) "" b)
    | Either(e) -> begin
        match e with
            | [] -> ""
            | hd :: tl -> sprintf "either %s \n %s" (statement_to_str hd) (List.fold_left (fun prev s -> (sprintf "%s orelse %s" prev (statement_to_str s))) "" tl)
        end
    | SomeStmt(var,exp,s) -> sprintf "some( %s : %s )\n %s" (param_to_str var) (exp_to_str exp) (statement_to_str s)
    | Allof(e) -> begin
        match e with
            | [] -> ""
            | hd :: tl -> sprintf "allof %s \n %s" (statement_to_str hd) (List.fold_left (fun prev s -> (sprintf "%s andalso %s" prev (statement_to_str s))) "" tl)
        end
    | If(exp,t,e) -> sprintf "if ( %s ) \n %s else \n %s" (exp_to_str exp) (statement_to_str t) (statement_to_str e)
    | While(exp,t) -> sprintf "while( %s ) \n %s" (exp_to_str exp) (statement_to_str t)
    | ForEach(var,exp,s) -> sprintf "foreach( %s : %s )\n %s" (param_to_str var) (exp_to_str exp) (statement_to_str s)
    | VarDec(var) -> List.fold_left (fun prev (s,t,i) ->
                                    let new_var = match i with
                                        | None -> sprintf "%s %s;\n" (typ_to_str t) s
                                        | Some x -> sprintf "%s %s = %s;\n" (typ_to_str t) s (init_to_str x)
                                    in prev ^ new_var
                                 ) "" var
    | Assign(l,e) -> sprintf "%s = %s;\n" (lval_to_str l) (exp_to_str e)
    | MacroCall(a,b)  -> sprintf "%s(%s);\n"   a (args_to_str b)
    | ExpStmt(exp) -> sprintf "%s;\n" (exp_to_str exp)

and exp_to_str exp = match exp.exp with
    | EQ(a,b)       -> sprintf "(%s == %s)" (exp_to_str a) (exp_to_str b)
    | NEQ(a,b)      -> sprintf "(%s != %s)" (exp_to_str a) (exp_to_str b)
    | LEQ(a,b)      -> sprintf "(%s <= %s)" (exp_to_str a) (exp_to_str b)
    | GEQ(a,b)      -> sprintf "(%s >= %s)" (exp_to_str a) (exp_to_str b)
    | LT(a,b)       -> sprintf "(%s < %s)"  (exp_to_str a) (exp_to_str b)
    | GT(a,b)       -> sprintf "(%s > %s)"  (exp_to_str a) (exp_to_str b)
    | Not(a)        -> sprintf "(!%s)"      (exp_to_str a)
    | Negative(a)   -> sprintf "-(%s)"    (exp_to_str a)
    | And(a,b)      -> sprintf "(%s && %s)" (exp_to_str a) (exp_to_str b)
    | PAnd(a,b)     -> sprintf "(%s & %s)" (exp_to_str a) (exp_to_str b)
    | Or(a,b)       -> sprintf "(%s || %s)" (exp_to_str a) (exp_to_str b)
    | Plus(a,b)     -> sprintf "(%s + %s)"  (exp_to_str a) (exp_to_str b)
    | Minus(a,b)    -> sprintf "(%s - %s)"  (exp_to_str a) (exp_to_str b)
    | Times(a,b)    -> sprintf "(%s * %s)"  (exp_to_str a) (exp_to_str b)
    | Mod(a,b)      -> sprintf "(%s %% %s)" (exp_to_str a) (exp_to_str b)
    | Lval(a)        -> lval_to_str a
    | Lit(a)        -> begin match a with
                        | StringLit(x,typ) -> sprintf "\"%s\"" x
                        | IntLit(x,typ)    -> sprintf "%d" x
                        | CharLit(x,typ)   -> sprintf "'%c'" x
                        | True             -> "true"
                        | False            -> "false"
                        end
    | Fun(a,b,c)    -> sprintf "%s.%s(%s)" (lval_to_str a) b (args_to_str c)
    | Input         -> "input()"
    


