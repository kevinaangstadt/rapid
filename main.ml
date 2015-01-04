let process (lexbuf : Lexing.lexbuf) =
    try
        let program = Parse.program Lex.initial lexbuf in
            (print_endline (Language.program_to_str program) ; Compiler.compile program )
    with exn ->
      begin
        let curr = lexbuf.Lexing.lex_curr_p in
        let line = curr.Lexing.pos_lnum in
        let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
        let tok = Lexing.lexeme lexbuf in
        Printf.printf "Parsing Error: \n";
        Printf.printf "line: %d\n" line ;
        Printf.printf "col: %d\n" cnum ;
        Printf.printf "tok: %s\n" tok;
        exit(-1)
      end

let read_file (name : string) =
    try
        let channel = open_in name in
        let lexbuf = Lexing.from_channel channel in
            flush stdout;
            process lexbuf;
            close_in_noerr channel
    with Sys_error _ ->
        Printf.printf "Failed to open %s" name ; exit(-1)
;;
read_file("tst.ap")