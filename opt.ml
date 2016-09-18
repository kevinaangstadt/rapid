(*
 * RAPID Optimizations
 *)

(*
 * general usage of apcompile
 * apcompile -v -Ablah.map -f blah.fsm blah.anml
 *)

open Util 
open Automata

(*TODO LOTS MORE ERROR CHECKING*)
let temp_compile net =
    let name,channel = Filename.open_temp_file "rapid" "tempcompile" in
    let command = Printf.sprintf "apcompile -v -A/dev/null -f /dev/null %s" name in
    Automata.network_to_file net channel ;
    close_out channel ;
    let ic,oc = Unix.open_process command in
    let buff = Buffer.create 16 in
        begin try
        while true do
            Buffer.add_channel buff ic 1
        done
        with End_of_file -> ()
        end ;
        Unix.close_process (ic,oc) ;
        (* Remove the temp file *)
        Sys.remove name ;
        (Buffer.contents buff)

let get_blocks net =
    let compile_results = temp_compile net in
    let search = Str.regexp "Total blocks used[ ]*=[ ]*\([0-9]+\)" in
    Str.search_forward search compile_results 0 ;
    let blocks = Str.matched_group 1 compile_results in
    int_of_string blocks


(*minimization of automata*)
let remove_dead_states (net:Automata.network ref) abstract_mapping =
    begin
    let visited = ref StringSet.empty in
    let rec remove_state e =
        match e with
            | Automata.STE(_,_,_,_,_,conn,rpt)
            | Automata.Combinatorial(_,_,_,rpt,conn)
            | Automata.Counter(_,_,_,rpt,conn) ->
                begin
                    match conn.children with
                        | [] when not rpt ->
                            let id_to_remove = Automata.get_id e in
                            List.iter (fun id ->
                                let parent = Hashtbl.find (!net).states id in
                                match parent with
                                    | Automata.STE(_,_,_,_,_,conn,_)
                                    | Automata.Combinatorial(_,_,_,_,conn)
                                    | Automata.Counter(_,_,_,_,conn) ->
                                        conn.children <- List.filter (fun (e2,_) -> (Automata.get_id e2) <> id_to_remove) conn.children
                            ) conn.parents ;
                            Hashtbl.remove (!net).states id_to_remove;
                        | hd :: tl ->
                            if (not (StringSet.mem (Automata.get_id e) !visited)) then
                                begin
                                visited := StringSet.add (Automata.get_id e) !visited ;
                                List.iter (fun (child,_) -> remove_state (Hashtbl.find (!net).states (Automata.get_id child))) conn.children ;
                                remove_state e
                                end
                        | _ -> ()
                end
    in
    List.iter (fun (k,e) ->
        begin try
        let start = Hashtbl.find (!net).states k in
        remove_state start
        with Not_found -> ()
        end
    ) (!net).start
    end
