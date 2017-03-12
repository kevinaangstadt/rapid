(*
 * Abstract Syntax for RAPID
 *)

open Util

exception Syntax_error of string
exception Uninitialized_variable
exception Negative_count

type typ =
    | String
    | Int
    | Char
    | Counter
    | DoubleCounter of bool (*Internal; true means EQ, else NEQ*)
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
    | StartIn
    | AllIn

type expression = {
    exp : expression_kind ;
    mutable expr_type : typ ;
    loc : loc ;
    id : int ;
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
    
type vardec = {
    var          : string ;
    mutable typ  : typ ;
    init         : initialize option;
    loc          : loc
}

type statement = {
    stmt : statement_kind;
    loc : loc;
    id : int
}

and statement_kind =
    | Report
    | Break
    | Block of statement list 
    | If of expression * statement * statement
    | Either of statement list
    | SomeStmt of param * expression * statement
    (*| Allof of statement list*)
    | ForEach of param * expression * statement
    | While of expression * statement
    | Whenever of expression * statement
    | VarDec of vardec list
    | Assign of lval * expression
    | ExpStmt of expression 
    | MacroCall of string * arguments
    | Debug of string

type macro = Macro of string * parameters * statement

type network = Network of parameters * statement

type program = Program of macro list * network

(* Used in the Symbol Table *)
type array_type =
    | IntArray of int array
    | StringArray of string array
    | CharArray of char array
    | BooleanArray of bool array

type fake_value =
    | AbstractString of string
    | NoValue

type value =
    | StringValue of string
    | IntValue of int
    | CharValue of char
    | BooleanValue of bool
    | CounterList of string list
    | ArrayValue of value option array
    | AbstractValue of Config.size * string * fake_value
    | AbstractChar of string * char
    
type container =
    | MacroContainer of macro
    | Variable of string * typ * value option
type 'a exp_return =
    | AutomataExp of 'a Automata.element list
    | CounterExp of (string list * int list * string * string)
    | BooleanExp of bool
    | IntExp of int
    | CharExp of char
    | StringExp of string
    | ArrayExp of value option array
    | AbstractExp of Config.size * string * typ * fake_value
type symbol = (string, container) Hashtbl.t




(* Printing functions *)

open Printf

let rec value_to_string (value:value option) =
    match value with
    | Some(v) -> begin
        match v with
        | StringValue(s) -> s
        | IntValue(i) -> string_of_int i
        | CharValue(c) -> Char.escaped c
        | BooleanValue(b) -> if b then "true" else "false"
        | ArrayValue(a) ->
            let l = Array.to_list a in
                if l = [] then
                    "[]"
                else 
                    "[" ^ List.fold_left (fun acc v -> ", " ^ (value_to_string v) ) (value_to_string (List.hd l)) (List.tl l) ^ "]"
        | _ -> "Abstract Value"
    end
    | _ -> "No Value"

let rec program_to_str (Program(macros,network)) = sprintf "%s %s" (List.fold_left (fun prev a -> (prev) ^ (sprintf "%s" (macro_to_str a) )) "" macros) (network_to_str network)

and network_to_str (Network(params,stmts)) = sprintf "network ( %s ) \n %s "  (params_to_str params) (statement_to_str stmts)

and macro_to_str (Macro(name,params,stmts)) = sprintf "macro %s ( %s ) \n %s " name (params_to_str params) (statement_to_str stmts)

and typ_to_str t = match t with
    | String -> "String"
    | Int ->    "int"
    | Char ->   "char"
    | Counter ->"Counter"
    | DoubleCounter(_) -> "DoubleCounter(internal)"
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

and statement_to_str (a : statement) = match a.stmt with
    | Break -> "break;\n"
    | Report -> "report;\n"
    | Block(b) -> sprintf "{ \n %s }\n" (List.fold_left (fun prev s -> (sprintf "%s     %s" prev (statement_to_str s))) "" b)
    | Either(e) -> begin
        match e with
            | [] -> ""
            | hd :: tl -> sprintf "either %s \n %s" (statement_to_str hd) (List.fold_left (fun prev s -> (sprintf "%s orelse %s" prev (statement_to_str s))) "" tl)
        end
    | SomeStmt(var,exp,s) -> sprintf "some( %s : %s )\n %s" (param_to_str var) (exp_to_str exp) (statement_to_str s)
    (*| Allof(e) -> begin
        match e with
            | [] -> ""
            | hd :: tl -> sprintf "allof %s \n %s" (statement_to_str hd) (List.fold_left (fun prev s -> (sprintf "%s andalso %s" prev (statement_to_str s))) "" tl)
        end*)
    | If(exp,t,e) -> sprintf "if ( %s ) \n %s else \n %s" (exp_to_str exp) (statement_to_str t) (statement_to_str e)
    | While(exp,t) -> sprintf "while( %s ) \n %s" (exp_to_str exp) (statement_to_str t)
    | Whenever(exp,t) -> sprintf "whenever( %s ) \n %s" (exp_to_str exp) (statement_to_str t)
    | ForEach(var,exp,s) -> sprintf "foreach( %s : %s )\n %s" (param_to_str var) (exp_to_str exp) (statement_to_str s)
    | VarDec(var) -> List.fold_left (fun prev vardec ->
                                    let new_var = match vardec.init with
                                        | None -> sprintf "%s %s;\n" (typ_to_str vardec.typ) vardec.var
                                        | Some x -> sprintf "%s %s = %s;\n" (typ_to_str vardec.typ) vardec.var (init_to_str x)
                                    in prev ^ new_var
                                 ) "" var
    | Assign(l,e) -> sprintf "%s = %s;\n" (lval_to_str l) (exp_to_str e)
    | MacroCall(a,b)  -> sprintf "%s(%s);\n"   a (args_to_str b)
    | ExpStmt(exp) -> sprintf "%s;\n" (exp_to_str exp)
    | Debug(s) -> sprintf "debug(%s);\n" s

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
                        | StartIn          -> "START_OF_INPUT"
                        | AllIn            -> "ALL_INPUT"
                        end
    | Fun(a,b,c)    -> sprintf "%s.%s(%s)" (lval_to_str a) b (args_to_str c)
    | Input         -> "input()"
    
let rec program_to_line (Program(macros,network)) channel =
    List.iter (fun a ->
                macro_to_line a channel) macros ;
    network_to_line network channel
    
and network_to_line (Network(params,stmts)) channel =
    statement_to_line stmts channel
    
and macro_to_line (Macro(name,params,stmts)) channel =
   statement_to_line stmts channel
   
and statement_to_line (a : statement) channel =
    let line,_ = a.loc in
    fprintf channel "%d\t%d\n" a.id line ;
    match a.stmt with
    | Break -> ()
    | Report -> ()
    | Block(b) -> List.iter (fun a -> statement_to_line a channel) b
    | Either(e) -> List.iter (fun a -> statement_to_line a channel) e
    | SomeStmt(var,exp,s) -> begin
        exp_to_line exp channel;
        statement_to_line s channel
    end
    (*| Allof(e) -> begin
        match e with
            | [] -> ""
            | hd :: tl -> sprintf "allof %s \n %s" (statement_to_str hd) (List.fold_left (fun prev s -> (sprintf "%s andalso %s" prev (statement_to_str s))) "" tl)
        end*)
    | If(exp,t,e) -> begin
        exp_to_line exp channel;
        statement_to_line t channel;
        statement_to_line e channel
    end
    | While(exp,t) ->begin
        exp_to_line exp channel;
        statement_to_line t channel
    end
    | Whenever(exp,t) -> begin
        exp_to_line exp channel;
        statement_to_line t channel
    end
    | ForEach(var,exp,s) -> begin
        exp_to_line exp channel;
        statement_to_line s channel
    end
    | VarDec(var) -> List.iter (fun var -> var_to_line var channel) var
    | Assign(l,e) -> begin
        exp_to_line e channel
    end
    | MacroCall(a,b) -> ()
    | ExpStmt(exp) -> exp_to_line exp channel
    | Debug(s) -> ()
    
and var_to_line (a:vardec) channel =
    match a.init with
    | Some a -> init_to_line a channel
    | None -> ()

and init_to_line a channel =
    match a with
    | PrimitiveInit(a) -> exp_to_line a channel
    | ArrayInit(a) -> List.iter (fun a -> init_to_line a channel) a

and exp_to_line exp channel =
    let line,_ = exp.loc in
    fprintf channel "%d\t%d\n" exp.id line;
    match exp.exp with
    | And(a,b)      
    | PAnd(a,b)    
    | Or(a,b)       
    | Plus(a,b)     
    | Minus(a,b)   
    | Times(a,b)    
    | Mod(a,b)
    | EQ(a,b)       
    | NEQ(a,b)      
    | LEQ(a,b)      
    | GEQ(a,b)      
    | LT(a,b)       
    | GT(a,b)       -> exp_to_line a channel ; exp_to_line b channel
    | Not(a)       
    | Negative(a)   -> exp_to_line a channel
    | Lval(a)        -> ()
    | Lit(a)        -> ()
    | Fun(a,b,c)    ->  ()
    | Input         -> ()