{
(* 
 * ocamllex definition for RAPID
 *)
open Parse
open Util

exception RapidLexError of string

let enable_verbose = ref false

let debug typ lexbuf =
    if(!enable_verbose) then
       Printf.printf "DEBUG %s: '%s'\n" typ (Lexing.lexeme lexbuf)
    else
       ()

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
    
let str_loc = ref (0,0)
let str_buf = Buffer.create 16
} 

let blank = [' ' '\t']
let newline = ['\012' '\r' '\n']

let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']

rule initial = parse
      "/*"      { debug "block cmt" lexbuf; let _ = comment lexbuf in initial lexbuf }
    | "//"      { debug "line cmt" lexbuf; endline lexbuf }
    | blank     { debug "blank" lexbuf; initial lexbuf }
    | newline   { debug "newline" lexbuf ; Lexing.new_line lexbuf ; initial lexbuf }
    
    | ","       { debug "comma" lexbuf; TCOMMA(where lexbuf) }
    | "."       { debug "dot" lexbuf; TDOT(where lexbuf) }
    | ";"       { debug "comma" lexbuf; TSEMICOLON(where lexbuf) }
    | ":"       { debug "colon" lexbuf; TCOLON(where lexbuf) }
    | '('       { debug "lparen" lexbuf; TLPAREN(where lexbuf) }
    | ')'       { debug "rparen" lexbuf; TRPAREN(where lexbuf) }
    | '{'       { debug "lbrace" lexbuf; TLBRACE(where lexbuf) }
    | '}'       { debug "rbrace" lexbuf; TRBRACE(where lexbuf) }
    | '['       { debug "lbracket" lexbuf; TLBRACKET(where lexbuf) }
    | ']'       { debug "rbracket" lexbuf; TRBRACKET(where lexbuf) }
    
    | "String"  { debug "string" lexbuf; TSTRING(where lexbuf) }
    | "int"     { debug "int" lexbuf; TINT(where lexbuf) }
    | "char"    { debug "char" lexbuf; TCHAR(where lexbuf) }
    | "bool"    { debug "bool" lexbuf; TBOOL(where lexbuf)}
    | "Counter" { debug "counter" lexbuf; TCOUNTER(where lexbuf) }
    | "macro"   { debug "macro" lexbuf; TMACRO(where lexbuf) }
    | "network" { debug "network" lexbuf; TNETWORK(where lexbuf) }
    | "report"  { debug "report" lexbuf; TREPORT(where lexbuf) }
    | "filter"  { debug "filter" lexbuf; TFILTER(where lexbuf) }
    | "input"   { debug "input" lexbuf; TINPUT(where lexbuf) }
    | "START_OF_INPUT" { debug "start_of_input" lexbuf; TSTARTIN(where lexbuf) }
    | "ALL_INPUT"      { debug "all_input" lexbuf; TALLIN(where lexbuf) }
    
    | "="       { debug "assign" lexbuf;  TASSIGN(where lexbuf) }
    | '-'       { debug "minus" lexbuf; TMINUS(where lexbuf) }
    | '+'       { debug "plus" lexbuf; TPLUS(where lexbuf) }
    | '*'       { debug "times" lexbuf; TTIMES(where lexbuf) }
    | '%'       { debug "mod" lexbuf; TMOD(where lexbuf) }
    
    | "=="      { debug "eq" lexbuf; TEQ(where lexbuf) }
    | "!="      { debug "neq" lexbuf; TNEQ(where lexbuf) }
    | "<="      { debug "leq" lexbuf; TLEQ(where lexbuf) }
    | ">="      { debug "geq" lexbuf;  TGEQ(where lexbuf) }
    | "<"       { debug "lt" lexbuf; TLT(where lexbuf) }
    | ">"       { debug "gt" lexbuf; TGT(where lexbuf) }
    | "&&"      { debug "and" lexbuf; TAND(where lexbuf) }
    | "&"       { debug "pand" lexbuf; TPAND(where lexbuf) }
    | "||"      { debug "or" lexbuf; TOR(where lexbuf) }
    | '!'       { debug "not" lexbuf; TNOT(where lexbuf) }
    
    | "true"    { debug "true" lexbuf; TRUE(where lexbuf) }
    | "false"   { debug "false" lexbuf; FALSE(where lexbuf) }
    
    | "foreach" { debug "foreach" lexbuf; TFOREACH(where lexbuf) }
    | "while"   { debug "while" lexbuf; TWHILE(where lexbuf) }
    | "whenever"{ debug "whenever" lexbuf; TWHENEVER(where lexbuf) }
    | "break"   { debug "break" lexbuf; TBREAK(where lexbuf) }
    | "if"      { debug "if" lexbuf; TIF(where lexbuf) }
    | "else"    { debug "else" lexbuf; TELSE(where lexbuf) }
    | "either"  { debug "either" lexbuf; TEITHER(where lexbuf) }
    | "orelse"  { debug "orelse" lexbuf; TORELSE(where lexbuf) }
    (*| "allof"   { TALLOF(where lexbuf) }
    | "andalso" { TANDALSO(where lexbuf) }*)
    | "some"    { debug "some" lexbuf; TSOME(where lexbuf) }
    | "debug"   { debug "debug" lexbuf; TDEBUG(where lexbuf) }
    
    | ("0x")?['0'-'9']+ {
        debug "int_lit" lexbuf; 
        let str = Lexing.lexeme lexbuf in
            INT((int_of_string str),(where lexbuf))
    }
    
    | ['A'-'Z''a'-'z''_']['0'-'9''A'-'Z''a'-'z''_']* {
        debug "ident" lexbuf; 
        let str = Lexing.lexeme lexbuf in
            IDENT(str,(where lexbuf))
    }
    
    (*| '"' (([^'\000''"''\n''\\'] | ('\\'_))* as str) '"' {*)
      | '"' {
        debug "string_lit" lexbuf; 
        str_loc := where lexbuf;
        Buffer.reset str_buf;
        let (l,c) = !str_loc in
        string lexbuf
        (*STRINGLIT(str,(where lexbuf))*)
    }
    
    | '\'' (([^'\000''\'''\n''\\'] | ('\\'_))* as str) '\'' {
        debug "char_lit" lexbuf; 
        let str = Lexing.lexeme lexbuf in
          (* This is length 3 because of the single quotes *)
          if (String.length str) < 3 then
            begin
              let line,col = where lexbuf in
              Printf.printf "Empty character literal line %i col %i\n" line col ;
              exit 1
            end
          else if (String.length str) = 3 then
            CHARLIT(String.get str 1, (where lexbuf))
          else if (String.get str 1) = '\\' then
            if ((String.get str 2) = 'x') || ((String.get str 2) = 'o') then
               begin
               (* Convert number of format that OCaml understands *)
               CHARLIT(scan_hex_octal_escape ("0" ^ String.sub str 2 ((String.length str) - 3)), (where lexbuf))
               end
            else
             try
               CHARLIT(scan_escape (String.get str 2), (where lexbuf))
             with RapidLexError str ->
               Printf.printf "%s\n" str ;
               raise (RapidLexError str)            
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
    
and string = parse
      [^'\000' '"' '\n' '\\'] {
             let str = Lexing.lexeme lexbuf in
             debug "string_lit part" lexbuf;
             Buffer.add_string str_buf str;
             string lexbuf
    }
    | '\\' ['x''X'] hexdigit hexdigit {
             let str = Lexing.lexeme lexbuf in
             debug "string_lit part" lexbuf;
             let hex_char = scan_hex_octal_escape ("0" ^ String.sub str 1 ((String.length str) - 1)) in
             Buffer.add_char str_buf hex_char;
             string lexbuf
    }
    | '\\'_ {
            let str = Lexing.lexeme lexbuf in
            debug "string_lit part" lexbuf;
            let hex_char = scan_escape (String.get str 1) in
            Buffer.add_char str_buf hex_char ;
            string lexbuf
    }
    | '"'      { STRINGLIT(Buffer.contents str_buf, !str_loc) }

and endline = parse
      '\n'      { Lexing.new_line lexbuf ; initial lexbuf }
    | _         { endline lexbuf }
    | eof       { EOF }