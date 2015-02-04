{
(* Kevin Angstadt
 * ocamllex definition for language
 *)
open Parse
open Util

let string_buff = Buffer.create 256

let reset_string () = Buffer.clear string_buff

let store_char c = Buffer.add_char string_buff c

let get_string () = Buffer.contents string_buff


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
    
    | "String"  { TSTRING(where lexbuf) }
    | "int"     { TINT(where lexbuf) }
    | "char"    { TCHAR(where lexbuf) }
    | "list"    { TLIST(where lexbuf) }
    | "Counter" { TCOUNTER(where lexbuf) }
    | "macro"   { TMACRO(where lexbuf) }
    | "network" { TNETWORK(where lexbuf) }
    | "report"  { TREPORT(where lexbuf) }
    | "filter"  { TFILTER(where lexbuf) }
    | "input"   { TINPUT(where lexbuf) }
    
    | "="       { TASSIGN(where lexbuf) }
    | '-'       { TMINUS(where lexbuf) }
    
    | "=="      { TEQ(where lexbuf) }
    | "!="      { TNEQ(where lexbuf) }
    | "<="      { TLEQ(where lexbuf) }
    | ">="      { TGEQ(where lexbuf) }
    | "<"       { TLT(where lexbuf) }
    | ">"       { TGT(where lexbuf) }
    | "&&"      { TAND(where lexbuf) }
    | "||"      { TOR(where lexbuf) }
    | '!'       { TNOT(where lexbuf) }
    
    | "true"    { TRUE(where lexbuf) }
    | "false"   { FALSE(where lexbuf) }
    
    | "foreach" { TFOREACH(where lexbuf) }
    | "while"   { TWHILE(where lexbuf) }
    | "if"      { TIF(where lexbuf) }
    | "else"    { TELSE(where lexbuf) }
    
    | ("0x")?['0'-'9']+ {
        let str = Lexing.lexeme lexbuf in
            INT((int_of_string str),(where lexbuf))
    }
    
    | ['A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''_']* {
        let str = Lexing.lexeme lexbuf in 
            IDENT(str,(where lexbuf))
    }
    
    | '"' {
        reset_string () ;
        string lexbuf ;
        STRINGLIT(get_string (),(where lexbuf))
    }
    
    | ('\'')(_)('\'') {
        let str = Lexing.lexeme lexbuf in
            CHARLIT(String.get str 1, (where lexbuf))
    }
    
    | eof       { EOF }
    | _         {
        Printf.printf "invalid character '%s'\n" (Lexing.lexeme lexbuf) ;
        exit 1 (*FIXME Nice error handling needed here*)
    }

and string = parse
    | '"'       { () }
    | _ as c    {
        store_char c;
        string lexbuf
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