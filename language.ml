(*
 * Kevin Angstadt
 * Abstract Syntax for AP Language
 *)
 
type typ =
    | String
    | Int
    | Char

type literal =
    | StringLit of string * typ
    | IntLit of int * typ
    | CharLit of char * typ
    | True
    | False


type variable_dec = VarDec of string * typ
    
type args = Args of variable_dec list

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

type statement =
    | Report
    | Block of statement list
    | IF of expression * statement * statement

type macro = Macro of string * args * statement

(* Printing functions *)

open Printf

let rec macro_to_str (Macro(name,args,stmts)) = sprintf "macro %s ( %s ) \n %s " name (args_to_str args) (statement_to_str stmts)

and typ_to_str t = match t with
    | String -> "String"
    | Int ->    "Int"
    | Char ->   "Char"
    
and  vardec_to_str (VarDec(a,t)) = sprintf "%s %s" (typ_to_str t) a 

and args_to_str (Args(a)) = List.fold_left (fun prev v -> (prev) ^ (sprintf "%s, " (vardec_to_str v))) "" a

and statement_to_str (a : statement) = match a with
    | Report -> "report;\n"
    | Block(b) -> sprintf "{ \n %s }\n" (List.fold_left (fun prev s -> (sprintf "%s %s" (statement_to_str s) prev)) "" b)
    | IF(exp,t,e) -> sprintf "if ( %s ) \n %s else \n %s" (exp_to_str exp) (statement_to_str t) (statement_to_str e)

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
                        | StringLit(x,typ) -> x
                        | IntLit(x,typ)    -> sprintf "%d" x
                        | CharLit(x,typ)   -> sprintf "%c" x
                        | True             -> "true"
                        | False            -> "false"
                        end


