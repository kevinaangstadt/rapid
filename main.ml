(**
 * Top-level driver for RAPID compiler
 * Kevin Angstadt
 * University of Virginia
 *)
 
open Tc

(* Helper for printing tokens *)
let token_to_str t = match t with
   | Parse.INT(_) -> "int"
   | Parse.IDENT(s,_) -> Printf.sprintf "string(%s)" s
   | Parse.STRINGLIT(s,_) -> Printf.sprintf "stringlit(%s)" s
   | Parse.CHARLIT(s,_) -> Printf.sprintf "charlit(%c)" s
   | Parse.TRUE(_) -> "true"
   | Parse.FALSE(_) -> "false"
   | Parse.TCOMMA(_) -> "comma"
   | Parse.TDOT(_) -> "dot"
   | Parse.TSEMICOLON(_) -> "semi"
   | Parse.TCOLON(_) -> "colon"
   | Parse.TLPAREN(_) -> "lparen"
   | Parse.TRPAREN(_) -> "rparen"
   | Parse.TLBRACE(_) -> "lbrace"
   | Parse.TRBRACE(_) -> "rbrace"
   | Parse.TLBRACKET(_) -> "lbracket"
   | Parse.TRBRACKET(_) -> "rbracket"
   | Parse.TINT(_) -> "int"
   | Parse.TSTRING(_) -> "string"
   | Parse.TCHAR(_) -> "char"
   | Parse.TBOOL(_) -> "bool"
   | Parse.TLIST(_) -> "list"
   | Parse.TCOUNTER(_) -> "counter"
   | Parse.TMACRO(_) -> "macro"
   | Parse.TNETWORK(_) -> "network"
   | Parse.TREPORT(_) -> "report"
   | Parse.TFILTER(_) -> "filter"
   | Parse.TINPUT(_) -> "input"
   | Parse.TSTARTIN(_) -> "startin"
   | Parse.TALLIN(_) -> "allin"
   | Parse.TMINUS(_) -> "minus"
   | Parse.TPLUS(_) -> "tplus"
   | Parse.TTIMES(_) -> "ttimes"
   | Parse.TMOD(_) -> "mod"
   | Parse.TASSIGN(_) -> "assign"
   | Parse.TEQ(_) -> "eq"
   | Parse.TNEQ(_) -> "neq"
   | Parse.TLEQ(_) -> "leq"
   | Parse.TGEQ(_) -> "geq"
   | Parse.TLT(_) -> "lt"
   | Parse.TGT(_) -> "gt"
   | Parse.TAND(_) -> "and"
   | Parse.TPAND(_) -> "pand"
   | Parse.TOR(_) -> "or"
   | Parse.TNOT(_) -> "not"
   | Parse.TFOREACH(_) -> "foreach"
   | Parse.TWHILE(_) -> "while"
   | Parse.TWHENEVER(_) -> "whenever"
   | Parse.TIF(_) -> "if"
   | Parse.TELSE(_) -> "else"
   | Parse.TEITHER(_) -> "either"
   | Parse.TORELSE(_) -> "orelse"
   | Parse.TSOME(_) -> "some"
   | Parse.TDEBUG(_) -> "debug"
   | _ -> "unknown token"

let file = ref ""

let intermediate = ref ""

let set_intermediate name = intermediate := name 

let process (lexbuf : Lexing.lexbuf) config =
    let program = begin
        try
            Parse.program Lex.initial lexbuf
        with exn ->
        begin
            let curr = lexbuf.Lexing.lex_curr_p in
            let line = curr.Lexing.pos_lnum in
            let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
            let tok = Lexing.lexeme lexbuf in
            Printf.printf "Parsing Error: \n";
            Printf.printf "line: %d\n" line ;
            Printf.printf "column: %d\n" cnum ;
            Printf.printf "token: %s\n" tok;
            exit(1)
        end
    end in
    let program_t = begin
        try
            Tc.check program
        with
        (* FIXME add location information *)
        | Type_error(str)
        | Macro_error(str)
        | Var_error(str) ->
            Printf.printf "Type checking error: %s\n" str;
            exit(2)
        | Type_mismatch -> Printf.printf "Type checking error: type mismatch.\n";
            exit(3)
    end in
    let program_i = Intermediate.intermediate program_t in
    (* Let's remove path from the file name and use that to name the network*)
    let net_name =
        let trim_front =
            try
                let loc = (String.rindex !file '/') in
                String.sub !file (loc + 1) ((String.length !file) - (loc + 1))
            with Not_found -> !file
        in
            String.map (fun c -> if c = '.' then '_' else c) trim_front
    in
        if !intermediate = "" then
            (*print_endline (Language.program_to_str program_i) ;*)
            ( (Compiler.compile program_i config net_name), program_t)
            
            (*print_endline (Language.program_to_str program) ; *)  (**)
        else
            (
            let channel = open_out (Printf.sprintf "%s" !intermediate) in
            Printf.fprintf channel "%s" (Language.program_to_str program_i) ;
            close_out channel ;
            exit(0)
            )

let process_config (lexbuf : Lexing.lexbuf) =
    try
        Config_parse.config Config_lex.initial lexbuf
    with exn ->
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Printf.printf "Config Parsing Error: \n";
        Printf.printf "line: %d\n" line ;
        Printf.printf "col: %d\n" cnum ;
        Printf.printf "tok: %s\n" tok;
        exit(-1)
      end    
      
let read_config (name : string) =
    try
        let channel = open_in name in
        let lexbuf = Lexing.from_channel channel in
            flush stdout;
            let return = process_config lexbuf in
            close_in_noerr channel ; return
    with Sys_error _ -> begin
        Printf.printf "Failed to open %s\n" name ; exit (-1)
        end;;

let read_file (name : string) config =
    try
        let channel = open_in name in
        let lexbuf = Lexing.from_channel channel in
            flush stdout;
            let return = process lexbuf config in
            close_in_noerr channel ; return
    with Sys_error _ -> begin
        Printf.printf "Failed to open %s\n" name ; exit (-1)
        end ;;

let output = ref "a.anml" in
let set_out out = output := out in

let config = ref "" in
let set_config new_c = config := new_c in

let merge = ref false in
let debug = ref false in

let argspec = [
        ("-o", Arg.String (set_out), "filename Names output file; a.anml by default" );
        ("-c", Arg.String (set_config), "config_file Provides variable configuration for network");
        ("-i", Arg.String (set_intermediate), "filename Write intermediate RAPID to filename");
        ("-g", Arg.Set debug, " Enable debugging output");
        ("--tiling", Arg.Set Compiler.do_tiling, " Enable tiling optimization");
        ("--merge", Arg.Set merge, " Combine output into single ANML file") ;
        ("--verbose-lexing", Arg.Set Lex.enable_verbose, " Enable verbose lexing")
    ] in
let argspec = Arg.align argspec in
    Arg.parse argspec (fun x -> file := x) "Usage: rapid [options] [input file]" ;
    if(String.length !file = 0) then
        begin
            Printf.printf "You must provide a file to compile!\n"; exit (-1)
        end
    else
    let config : Config.config =
        if String.length !config = 0 then []
        else read_config !config
    in
    let ((anml,abstract_mapping), ast) = read_file !file config in
    try
        if not !merge then
            begin
            List.iteri (fun i a ->
                let channel = open_out (Printf.sprintf "%d.%s" i !output) in
                (Automata.network_to_file a channel) ;
                close_out channel
            ) anml
            end
        else
        begin
            let channel = open_out !output in
            Automata.networks_to_file anml channel ;
            close_out channel
        end
        ;
         (* Write out debugging *)
        if !debug then
            let channel = open_out (Printf.sprintf "%s.debug.json" !output) in
            Debug.networks_to_ast_id anml channel;
            close_out channel;
            
            
            
            let channel = open_out (Printf.sprintf "%s.debug.ast-line" !output) in
            Language.program_to_line ast channel;
            close_out channel
        else ()
        
        
    with Sys_error _ ->
        Printf.printf "Failed to write output" ; exit (-1)
