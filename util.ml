(*
 * Kevin Angstadt
 * University of Virginia
 *)

type loc = int*int

let rec range i j = if i == j then [] else i :: (range (i+1) j)

let where lexbuf : loc =
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        (line,cnum)