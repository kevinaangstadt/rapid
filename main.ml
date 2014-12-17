let process (lexbuf : Lexing.lexbuf) =
    try
        while true do
            Parse.macro Lex.read lexbuf;
            flush stdout;
        done
    with End_of_file -> ()

let read_file (name : string) =
    try
        let channel = open_in name in
            let lexbuf = Lexing.from_channel = channel in
            flush stdout;
            process lexbuf;
            close_in_noerr channel
    with Sys_error _ ->
        printerror ("Failed to open " ^ name) ; exit(-1)
