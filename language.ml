(*
 * Kevin Angstadt
 * Abstract Syntax for AP Language
 *)
 
type typ =
    | String
    | Int
    | Char
    | Counter
    | List

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
    | Var of string
    | Lit of literal
    | Input

and arguments = Arguments of expression list

type param = Param of string * typ
    
type parameters = Parameters of param list

type scope = MacroScope | NetworkScope

type statement =
    | Report of scope
    | Block of statement list * scope 
    | IF of expression * statement * statement * scope
    | ForEach of param * expression * statement * scope
    | VarDec of string * typ * scope
    | ExpStmt of expression option * scope
    | Fun of expression * arguments * scope
    | Count of expression * scope
    | Reset of expression * scope

type macro = Macro of string * parameters * statement

type network = Network of parameters * statement

type program = Program of macro list * network

type container =
    | Literal of literal
    | Expression of expression
    | Statement of statement
    | MacroContainer of macro
    | Variable of string * typ * value option



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
    
and param_to_str (Param(a,t)) = sprintf "%s %s" (typ_to_str t) a 

and params_to_str (Parameters(a)) = List.fold_left (fun prev v -> (prev) ^ (sprintf "%s, " (param_to_str v))) "" a

and args_to_str (Arguments(a)) = List.fold_left (fun prev v -> (prev) ^(sprintf "%s, " (exp_to_str v))) "" a

and statement_to_str (a : statement) = match a with
    | Report(_) -> "report;\n"
    | Block(b,_) -> sprintf "{ \n %s }\n" (List.fold_left (fun prev s -> (sprintf "%s %s" prev (statement_to_str s))) "" b)
    | IF(exp,t,e,_) -> sprintf "if ( %s ) \n %s else \n %s" (exp_to_str exp) (statement_to_str t) (statement_to_str e)
    | ForEach(var,exp,s,_) -> sprintf "foreach( %s : %s )\n %s" (param_to_str var) (exp_to_str exp) (statement_to_str s)
    | VarDec(var,t,_) -> sprintf "%s %s;" (typ_to_str t) var
    | Fun(a,b,_)  -> sprintf "%s(%s);"   (exp_to_str a) (args_to_str b)
    | Count(a,_)  -> sprintf "%s.count();" (exp_to_str a)
    | Reset(a,_)  -> sprintf "%s.reset();" (exp_to_str a)
    | ExpStmt(exp,_) ->
        match exp with
        | Some(e) -> sprintf "%s;" (exp_to_str e)
        | None -> "bob"

and exp_to_str exp = match exp with
    | EQ(a,b)       -> sprintf "%s == %s" (exp_to_str a) (exp_to_str b)
    | NEQ(a,b)      -> sprintf "%s != %s" (exp_to_str a) (exp_to_str b)
    | LEQ(a,b)      -> sprintf "%s <= %s" (exp_to_str a) (exp_to_str b)
    | GEQ(a,b)      -> sprintf "%s >= %s" (exp_to_str a) (exp_to_str b)
    | LT(a,b)       -> sprintf "%s < %s"  (exp_to_str a) (exp_to_str b)
    | GT(a,b)       -> sprintf "%s > %s"  (exp_to_str a) (exp_to_str b)
    | Not(a)        -> sprintf "!%s"      (exp_to_str a)
    | Negative(a)   -> sprintf "-(%s)"    (exp_to_str a)
    | And(a,b)      -> sprintf "%s && %s" (exp_to_str a) (exp_to_str b)
    | Or(a,b)       -> sprintf "%s || %s" (exp_to_str a) (exp_to_str b)
    | Var(a)        -> a
    | Lit(a)        -> begin match a with
                        | StringLit(x,typ) -> sprintf "\"%s\"" x
                        | IntLit(x,typ)    -> sprintf "%d" x
                        | CharLit(x,typ)   -> sprintf "%c" x
                        | True             -> "true"
                        | False            -> "false"
                        end
    | Input         -> "input()"
    


