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
        
let explode s =
    let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

let rec sublist b l =
    match l with
        | [] -> []
        | hd :: tl ->
            let tail = sublist (b-1) tl in
                if b>0 then tail else hd :: tail
                
