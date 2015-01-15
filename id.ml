(*
 * Kevin Angstadt
 * University of Virginia
 * Generate ascending integers
 *)

type id_seed = int ref

let new_seed () : id_seed = ref 0

let get_num (seed : id_seed) = seed := (!seed + 1) ; !seed