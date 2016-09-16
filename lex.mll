{
(* Kevin Angstadt
 * ocamllex definition for language
 *)
open Parse
open Util

exception RapidLexError of string

let scan_escape (char: char) : char =
    let result = match char with
      'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | 'b' -> '\b'
    | 'f' -> '\012'  (* ASCII code 12 *)
    | 'v' -> '\011'  (* ASCII code 11 *)
    | 'a' -> '\007'  (* ASCII code 7 *)
    | 'e' | 'E' -> '\027'  (* ASCII code 27. This is a GCC extension *)
    | '\'' -> '\''    
    | '"'-> '"'     (* '"' *)
    | '?' -> '?'
    | '(' -> '('
    | '{' -> '{'
    | '[' -> '['
    | '%' -> '%'
    | '\\' -> '\\' 
    | other ->
         raise (RapidLexError (Printf.sprintf "Unrecognized escape sequence: \\%s" (String.make 1 other) ))
   in result

let scan_hex_octal_escape str =
    let char_code = int_of_string str in
    let result = Char.chr char_code in
    result
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
    | "bool"    { TBOOL(where lexbuf)}
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
    | "debug"   { TDEBUG(where lexbuf) }
    
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
          if (String.length str) < 3 then
            begin
              let line,col = where lexbuf in
              Printf.printf "Empty character literal line %i col %i\n" line col ;
              exit 1
            end
          else if (String.length str) = 3 then
            CHARLIT(String.get str 1, (where lexbuf))
          else if (String.get str 1) = '\\' then
            try
              CHARLIT(scan_escape (String.get str 2), (where lexbuf))
            with RapidLexError str ->
              Printf.printf "%s\n" str ;
              raise (RapidLexError str)
          else if ((String.get str 2) = 'x') || ((String.get str 2) = 'o') then
            CHARLIT(scan_hex_octal_escape (String.sub str 1 ((String.length str) - 2)), (where lexbuf))
          else
            let line,col = where lexbuf in
            Printf.printf "Unrecognized character literal: %s on line %i col %i\n" str line col;
            exit 1
    }
    
    | eof       { EOF }
    | _         {
        let line,col = where lexbuf in
        Printf.printf "invalid character on line %d col %d: '%s'\n" line col (Lexing.lexeme lexbuf) ;
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