(*
 * Kevin Angstadt
 * Abstract Syntax for AP Language
 *)

type literal =
    | String of string
    | Int of int
    | Char of char
    | True
    | False

type variable = Var of string * literal
    
type args = Args of input_variable list

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

type statement =
    | Report
    | Block of statement_list
    | IF of expression * statement * statement
    
type statement_list = Statements of statement list

type macro = Macro of string * args * block

