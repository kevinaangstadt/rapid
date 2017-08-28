(* Print debug mapping *)
open Util
open Automata
open Language

let get_all_keys ht =
    Hashtbl.fold ( fun k _ acc -> StringSet.add k acc) ht (StringSet.empty)

let helper (net:'a Automata.network ref) (channel:out_channel) =
    let final = Hashtbl.fold (fun k e acc ->
        let arr = List.fold_left (fun acc (ast_id, el_type) ->
                    let printer_fun = fun k e ->
                                    match e with
                                    | Variable(name,typ,value) -> Printf.sprintf  "{\"id\":\"%s\",\"kind\":\"variable\",\"type\":\"%s\",\"value\":\"%s\"}" name (typ_to_str typ) (value_to_string value)
                                    | MacroContainer(_) -> Printf.sprintf "{\"id\":\"%s\",\"kind\":\"macro\"}" k
                                    | DebugContainer(c) ->
                                        let dbg_val = value_to_string (Some(c)) in
                                        Printf.sprintf "{\"id\":\"%s\",\"kind\":\"debug\",\"value\":\"%s\"}" k dbg_val
                    in
                    let str,id,prt = match ast_id with
                        | AST(ast_id, st) -> StringSet.fold ( fun k acc ->
                                    let v = Hashtbl.find st k in
                                    Printf.sprintf "%s,%s" (printer_fun k v) acc
                                  ) (get_all_keys st) "", ast_id, "ste"
                        | PortAST(ast_id, port, st) -> StringSet.fold ( fun k acc ->
                                    let v = Hashtbl.find st k in
                                    Printf.sprintf "%s,%s" (printer_fun k v) acc
                                  ) (get_all_keys st) "", ast_id, port
                    in
                    let str = if str = "" then "" else (String.sub str 0 ((String.length str) - 1)) in
                    Printf.sprintf "{\"ast_id\":%d,\"el_type\":\"%s\",\"port\":\"%s\",\"state\":[%s]}, %s" id el_type prt str acc
                   ) "" (element_to_ast_id e) in
        let arr = if arr = "" then "" else (String.sub arr 0 ((String.length arr) - 2)) in
        Printf.sprintf"\"%s\":[%s],%s" (get_id e) arr acc
      ) (!net).states "" in
    if final = "" then "" else (String.sub final 0 ((String.length final) - 1))

let network_to_ast_id (net:'a Automata.network ref) (channel:out_channel) =
    Printf.fprintf channel "{" ;
    let final = helper net channel in
    Printf.fprintf channel "%s}" final

let networks_to_ast_id (nets: 'a Automata.network ref list) (channel:out_channel) =
    Printf.fprintf channel "{" ;
    let final = List.fold_left (fun acc a ->
        let s = (helper a channel) in
        if s = "" then
            acc 
        else
            Printf.sprintf "%s,%s" s acc
    ) "" nets in
    let final = if final = "" then "" else (String.sub final 0 ((String.length final) - 1)) in
    Printf.fprintf channel "%s}" final