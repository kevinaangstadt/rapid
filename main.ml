let process (lexbuf : Lexing.lexbuf) =
    try
        let macro = Parse.macro Lex.initial lexbuf in
            print_endline (Language.macro_to_str macro)
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
        let lines = ref [] in
        try
            while true; do
                lines := input_line channel :: !lines
            done
        with End_of_file -> begin
            close_in channel;
            let lexbuf = Lexing.from_string (List.fold_left (fun prev a -> a ^ prev) "" !lines ) in
            flush stdout;
            process lexbuf;
            close_in_noerr channel
            end
    with Sys_error _ ->
        Printf.printf "Failed to open %s" name ; exit(-1)
;;
read_file("tst.ap")