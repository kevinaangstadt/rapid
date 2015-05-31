(*
 * Kevin Angstadt
 * RAPID Optimizations
 *)

(*
 * general usage of apcompile
 * apcompile -v -Ablah.map -f blah.fsm blah.anml
 *)

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
        (Buffer.contents buff)

let get_blocks net =
    let compile_results = temp_compile net in
    let search = Str.regexp "Total blocks used[Ê]*=[ ]*\([0-9]+\)" in
    Str.search_forward search compile_results 0 ;
    let blocks = Str.matched_group 1 compile_results in
    int_of_string blocks
    