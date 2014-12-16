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

type variable = Var of string * typ
    
type args = Args of variable list

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

type statement_list = Statements of statement list
and statement =
    | Report
    | Block of statement_list
    | IF of expression * statement * statement

type macro = Macro of string * args * statement_list

