{
(* Kevin Angstadt
 * ocamllex definition for language
 *)
open Parse
} 

let blank = [' ' '\012' '\r' '\t' '\n']

rule initial = parse
      "/*"      { let _ = comment lexbuf in initial lexbuf }
    | "//"      { endline lexbuf }
    | blank     { initial lexbuf }
    
    | ","       { TCOMMA }
    | "."       { TDOT }
    | ";"       { TSEMICOLON }
    | ":"       { TCOLON }
    | '('       { TLPAREN }
    | ')'       { TRPAREN }
    | '{'       { TLBRACE }
    | '}'       { TRBRACE }
    
    | "int"     { TINT }
    | "Counter" { TCOUNTER }
    | "macro"   { TMACRO }
    | "Network" { TNETWORK }
    | "report"  { TREPORT }
    | "filter"  { TFILTER }
    
    | "="       { TASSIGN }
    
    | "=="      { TEQ }
    | "!="      { TNEQ }
    | "<="      { TLEQ }
    | ">="      { TGEQ }
    | "<"       { TLT }
    | ">"       { TGT }
    | "&&"      { TAND }
    | "||"      { TOR }
    | '!'       { TNOT }
    
    | "foreach" { TFOREACH }
    | "while"   { TWHILE }
    | "if"      { TIF }
    | "then"    { TTHEN }
    | "else"    { TELSE }
    
    | ("0x")?['0'-'9']+ {
        let str = Lexing.lexeme lexbuf in
            INT((int_of_string str))
    }
    
    | ['A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''_']* {
        let str = Lexing.lexeme lexbuf in 
            IDENT(str)
    }
    
    | eof       { EOF }
    | _         {
        Printf.printf "invalid character '%s'\n" (Lexing.lexeme lexbuf) ;
    }

and comment = parse
      "*/"      { () }
    | '\n'      { comment lexbuf }
    | eof       { Printf.printf "unterminated /* comment\n" ; exit 1 }
    | _         { comment lexbuf }

and endline = parse
      '\n'      { initial lexbuf }
    | _         { endline lexbuf }
    | eof       { EOF }