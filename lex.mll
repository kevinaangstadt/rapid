{
(* Kevin Angstadt
 * ocamllex definition for language
 *)
open Parse

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
    | "input()" { TINPUT }
    | "count()" { TCOUNT }
    | "reset()" { TRESET }
    
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
    
    | '"' {
        reset_string () ;
        string lexbuf ;
        STRINGLIT(get_string ())
    }
    
    | ('\'')(_)('\'') {
        let str = Lexing.lexeme lexbuf in
            CHARLIT(String.get str 1)
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