(*
 * Kevin Angstadt
 * Compiler for AP Language
 *)
 
 open Language (* Contains all the types for the AP language *)
 
 type symbol = (string, container) Hashtbl.t
 
 let symbol_table : symbol = Hashtbl.create 255
 
 let compile (Program(macros,network)) =
    List.iter (fun m -> match (m : macro) with
                                | Macro(name,params,stmts) -> Hashtbl.add symbol_table name (MacroContainer(m))
                                ) macros