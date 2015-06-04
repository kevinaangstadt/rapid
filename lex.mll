{
(* Kevin Angstadt
 * ocamllex definition for language
 *)
open Parse
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
    | "."       { TDOT(where lexbuf) }
    | ";"       { TSEMICOLON(where lexbuf) }
    | ":"       { TCOLON(where lexbuf) }
    | '('       { TLPAREN(where lexbuf) }
    | ')'       { TRPAREN(where lexbuf) }
    | '{'       { TLBRACE(where lexbuf) }
    | '}'       { TRBRACE(where lexbuf) }
    | '['       { TLBRACKET(where lexbuf) }
    | ']'       { TRBRACKET(where lexbuf) }
    
    | "String"  { TSTRING(where lexbuf) }
    | "int"     { TINT(where lexbuf) }
    | "char"    { TCHAR(where lexbuf) }
    | "Counter" { TCOUNTER(where lexbuf) }
    | "macro"   { TMACRO(where lexbuf) }
    | "network" { TNETWORK(where lexbuf) }
    | "report"  { TREPORT(where lexbuf) }
    | "filter"  { TFILTER(where lexbuf) }
    | "input"   { TINPUT(where lexbuf) }
    | "START_OF_INPUT" { TSTARTIN(where lexbuf) }
    | "ALL_INPUT"      { TALLIN(where lexbuf) }
    
    | "="       { TASSIGN(where lexbuf) }
    | '-'       { TMINUS(where lexbuf) }
    | '+'       { TPLUS(where lexbuf) }
    | '*'       { TTIMES(where lexbuf) }
    | '%'       { TMOD(where lexbuf) }
    
    | "=="      { TEQ(where lexbuf) }
    | "!="      { TNEQ(where lexbuf) }
    | "<="      { TLEQ(where lexbuf) }
    | ">="      { TGEQ(where lexbuf) }
    | "<"       { TLT(where lexbuf) }
    | ">"       { TGT(where lexbuf) }
    | "&&"      { TAND(where lexbuf) }
    | "&"       { TPAND(where lexbuf) }
    | "||"      { TOR(where lexbuf) }
    | '!'       { TNOT(where lexbuf) }
    
    | "true"    { TRUE(where lexbuf) }
    | "false"   { FALSE(where lexbuf) }
    
    | "foreach" { TFOREACH(where lexbuf) }
    | "while"   { TWHILE(where lexbuf) }
    | "whenever"{ TWHENEVER(where lexbuf) }
    | "if"      { TIF(where lexbuf) }
    | "else"    { TELSE(where lexbuf) }
    | "either"  { TEITHER(where lexbuf) }
    | "orelse"  { TORELSE(where lexbuf) }
    | "allof"   { TALLOF(where lexbuf) }
    | "andalso" { TANDALSO(where lexbuf) }
    | "some"    { TSOME(where lexbuf) }
    
    | ("0x")?['0'-'9']+ {
        let str = Lexing.lexeme lexbuf in
            INT((int_of_string str),(where lexbuf))
    }
    
    | ['A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''_']* {
        let str = Lexing.lexeme lexbuf in 
            IDENT(str,(where lexbuf))
    }
    
    | '"' (([^'\000''"''\n''\\'] | ('\\'_))* as str) '"' {
        STRINGLIT(str,(where lexbuf))
    }
    
    | '\'' (([^'\000''\'''\n''\\'] | ('\\'_))* as str) '\'' {
        let str = Lexing.lexeme lexbuf in
            CHARLIT(String.get str 1, (where lexbuf))
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