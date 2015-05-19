{
(* Kevin Angstadt
 * ocamllex definition for config files
 *)
open Config_parse
open Util

} 

let blank = [' ' '\t']
let newline = ['\012' '\r' '\n']

rule initial = parse
      "/*"      { let _ = comment lexbuf in initial lexbuf }
    | "//"      { endline lexbuf }
    | blank     { initial lexbuf }
    | newline   { Lexing.new_line lexbuf ; initial lexbuf }
    
    | ","       { TCOMMA(where lexbuf) }
    | ":"       { TCOLON(where lexbuf) }
    | '{'       { TLBRACE(where lexbuf) }
    | '}'       { TRBRACE(where lexbuf) }
    | '['       { TLBRACKET(where lexbuf) }
    | ']'       { TRBRACKET(where lexbuf) }
    
    | ("0x")?['0'-'9']+ {
        let str = Lexing.lexeme lexbuf in
            INT((int_of_string str),(where lexbuf))
    }
    
    | ['A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''_']* {
        let str = Lexing.lexeme lexbuf in
        match str with
            | "name"   -> TNAME(where lexbuf)
            | "data"   -> TDATA(where lexbuf)
            | "length" -> TLENGTH(where lexbuf)
            | "type"   -> TTYPE(where lexbuf)
            | "low"    -> TLOW(where lexbuf)
            | "high"   -> THIGH(where lexbuf)
            | "String" -> TSTRING(where lexbuf)
            | "Array"  -> TARRAY(where lexbuf)
            | "int"    -> TINT(where lexbuf)
            | _        -> begin
                let (lnum,cnum) = where lexbuf in
                let tok = Lexing.lexeme lexbuf in
                Printf.printf "CONFIG: Invalid token at line %d, col %d: %s.\n" lnum cnum tok ;
                exit 1
            end
    }
    
    | '"' (([^'\000''"''\n''\\'] | ('\\'_))* as str) '"' {
        STRINGLIT(str,(where lexbuf))
    }
    
    | eof       { EOF }
    | _         {
        Printf.printf "invalid character '%s'\n" (Lexing.lexeme lexbuf) ;
        exit 1 (*FIXME Nice error handling needed here*)
    }

and comment = parse
      "*/"      { () }
    | '\n'      { Lexing.new_line lexbuf ; comment lexbuf }
    | eof       { Printf.printf "unterminated /* comment\n" ; exit 1 }
    | _         { comment lexbuf }

and endline = parse
      '\n'      { Lexing.new_line lexbuf ; initial lexbuf }
    | _         { endline lexbuf }
    | eof       { EOF }