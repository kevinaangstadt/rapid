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
    
    | "String"  { TSTRING }
    | "int"     { TINT }
    | "char"    { TCHAR }
    | "list"    { TLIST }
    | "Counter" { TCOUNTER }
    | "macro"   { TMACRO }
    | "network" { TNETWORK }
    | "report"  { TREPORT }
    | "filter"  { TFILTER }
    
    | "="       { TASSIGN }
    | '-'       { TMINUS }
    
    | "=="      { TEQ }
    | "!="      { TNEQ }
    | "<="      { TLEQ }
    | ">="      { TGEQ }
    | "<"       { TLT }
    | ">"       { TGT }
    | "&&"      { TAND }
    | "||"      { TOR }
    | '!'       { TNOT }
    
    | "true"    { TRUE }
    | "false"   { FALSE }
    
    | "foreach" { TFOREACH }
    | "while"   { TWHILE }
    | "if"      { TIF }
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
        exit 1 (*FIXME Nice error handling needed here*)
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