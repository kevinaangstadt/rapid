(*
 * Kevin Angstadt
 * University of Virginia
 *)
 
let rec range i j = if i == j then [] else i :: (range (i+1) j)