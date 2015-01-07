(*
 * Kevin Angstadt
 * Compiler for AP Language
 *)
 
open Language (* Contains all the types for the AP language *)

type symbol = (string, container) Hashtbl.t

let symbol_table : symbol = Hashtbl.create 255

let verify_network_params (Parameters(p)) =
    List.iter (fun param -> match param with
                            | Param(exp,typ) -> begin
                                match exp with
                                | Var(str) -> ()
                                | _ -> (Printf.printf "param error 1!" ; exit 1)
                                ;
                                match typ with
                                | Counter -> (Printf.printf "param error 2!" ; exit 1)
                                | _ -> ()
                            end
    ) p
    
let rec evaluate_statement (stmt : statement) =
    match stmt with
        | Report ->
        | Block(b) -> List.iter (fun a -> evaluate_statment a) b
        | IF(exp,then_clause,else_clause) ->
        | ForEach(var,source,f) ->
        | VarDec(s,t) ->
        | ExpStmt(e) ->

let compile (Program(macros,network)) =
    (* Add the macros to the symbol table *)
    List.iter (fun m -> match (m : macro) with
                                | Macro(name,params,stmts) -> Hashtbl.add symbol_table name (MacroContainer(m))
                                ) macros
    ;
    match network with
        | Network(params,stmt) -> verify_network_params params ;
            match stmt with
                | Block(b) -> evaluate_statment stmt
                | _ -> (Printf.printf "network error!" ; exit 1)


