let file = ref ""

let process (lexbuf : Lexing.lexbuf) config =
    (*try*)
        let program = Parse.program Lex.initial lexbuf in
        let program_t = Tc.check program in
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
            (*print_endline (Language.program_to_str program_i) ;*)
            Compiler.compile program_i config net_name
            
            (*print_endline (Language.program_to_str program) ; *)  (**)
    (*with exn ->
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
      end*)

let process_config (lexbuf : Lexing.lexbuf) =
    Config_parse.config Config_lex.initial lexbuf
      
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

let argspec = [
        ("-o", Arg.String (set_out), "Names output file; a.anml by default" );
        ("-i", Arg.String (set_config), "Provides variable configuration for network");
        ("--tiling", Arg.Set Compiler.do_tiling, "Enable tiling optimization")
    ] in
let argspec = Arg.align argspec in
    Arg.parse argspec (fun x -> file := x) "Usage: language [options] [input file]" ;
    if(String.length !file = 0) then
        begin
            Printf.printf "You must provide a file to compile!\n"; exit (-1)
        end
    else
    let config : Config.config =
        if String.length !config = 0 then []
        else read_config !config
    in
    let anml = read_file !file config in
    try
        List.mapi (fun i anml ->
            let channel = open_out (Printf.sprintf "%d.%s" i !output) in
            (Automata.network_to_file anml channel) ;
            close_out channel
        ) anml
        
    with Sys_error _ ->
        Printf.printf "Failed to write output" ; exit (-1)
