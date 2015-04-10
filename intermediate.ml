(* Kevin Angstadt
 * Converts to an intermediate form of Rapid
 *)

open Language

let rec and_or stmt =
    let rec e_and_or exp = match exp.exp with
        | EQ(a,b)
        | NEQ(a,b)
        | LEQ(a,b)
        | GEQ(a,b)
        | LT(a,b)
        | GT(a,b)
        | Plus(a,b)
        | Minus(a,b)
        | Times(a,b)
        | Mod(a,b) -> e_and_or a || e_and_or b
        | Not(a)
        | Negative(a) -> e_and_or a
        | And(_,_)
        | Or(_,_) -> true
        | _ -> false
    in
    match stmt with
        | Block(stmts) 
        | Either(stmts) -> List.exists and_or stmts
        | While(e,s1) -> and_or s1
        | ExpStmt(e) -> e_and_or e
        | _ -> false

(*TODO: This does not wrok for non-equal consumes with an or inside a not
  Nasal demons will occur in this case because no check is currently conducted
*)        
let rec resolve_exp exp =
    let rec starify e =
        match e.exp with
            | And(a,b) -> {e with exp = And(starify a,starify b)}
            | PAnd(a,b) -> {e with exp = PAnd(starify a,starify b)}
            | Or(a,b) -> {e with exp = Or(starify a,starify b)}
            | EQ(a,b) -> {e with exp = EQ(starify a,starify b)}
                (* NEQ gets flipped to EQ because we only care about consumption length *)
            | NEQ(a,b) -> {e with exp = EQ(starify a,starify b)}
            | Lit(CharLit(_,_)) -> {e with exp = Lit(CharLit('*',Char))}
            | Lval(_) -> {e with exp = Lit(CharLit('*',Char))}
            | _ -> e
    in
    match exp.expr_type with
        | Automata ->
            begin
            match exp.exp with 
                | And(a,b) -> {exp with exp = And(resolve_exp a,resolve_exp b)}
                | PAnd(a,b) -> {exp with exp = PAnd(resolve_exp a,resolve_exp b)}
                | Or(a,b) -> {exp with exp = Or(resolve_exp a,resolve_exp b)}
                | EQ(a,b) -> {exp with exp = EQ(resolve_exp a,resolve_exp b)}
                | NEQ(a,b) -> {exp with exp = NEQ(resolve_exp a,resolve_exp b)}
                | Not(e) ->
                    begin
                    match e.exp with
                        | And(a,b) ->
                            let or_1 = {e with exp = And( resolve_exp {a with exp = Not(a)},
                                                          starify (resolve_exp b)
                                                        )} in
                            let or_2 = {e with exp = And( resolve_exp a,
                                                          resolve_exp {b with exp = Not(b)}
                                                        )} in     
                            {e with exp = Or(or_1,or_2)}
                        | PAnd(a,b) -> {e with exp = Or( resolve_exp {a with exp = Not(a)},
                                                         resolve_exp {b with exp = Not(b)}
                                                       )}
                        | Or(a,b) -> {e with exp = PAnd( resolve_exp {a with exp = Not(a)},
                                                         resolve_exp {b with exp = Not(b)}
                                                       )}
                        | EQ(a,b) -> {exp with exp = NEQ(resolve_exp a,resolve_exp b)}
                        | NEQ(a,b) -> {exp with exp = EQ(resolve_exp a,resolve_exp b)}
                        | Not(a) -> resolve_exp a
                    end
                | _ -> exp
            end
        | _ -> exp

let rec resolve_exp_stmt stmt =
    match stmt with
        | Block(stmts) -> Block(List.map resolve_exp_stmt stmts)
        | Either(stmts) -> Either(List.map resolve_exp_stmt stmts)
        | ForEach(p,e,s1) -> ForEach(p,resolve_exp e,(resolve_exp_stmt s1))
        | While(e,s1) -> While(resolve_exp e,(resolve_exp_stmt s1))
        | ExpStmt(e) -> ExpStmt(resolve_exp e)
        | _ -> stmt

let rec resolve_if_stmt stmt =
    match stmt with
        | Block(stmts) -> Block(List.map resolve_if_stmt stmts)
        | If(e,s1,s2) ->
            begin
            match e.expr_type with
            | Counter -> If(e,resolve_if_stmt s1,resolve_if_stmt s2)
            | _ ->
                Either([
                    Block([
                        ExpStmt(e);
                        (resolve_if_stmt s1)
                    ]);
                    Block([
                        ExpStmt({e with exp = Not(e)});
                        (resolve_if_stmt s2)
                    ])
                ])
            end
        | Either(stmts) -> Either(List.map resolve_if_stmt stmts)
        | ForEach(p,e,s1) -> ForEach(p,e,(resolve_if_stmt s1))
        | While(e,s1) -> While(e,(resolve_if_stmt s1))
        | _ -> stmt

let rec resolve_while_stmt stmt =
    match stmt with
        | Block(stmts) -> Block(List.map resolve_while_stmt stmts)
        | If(e,s1,s2) -> If(e,(resolve_while_stmt s1),(resolve_while_stmt s2))
        | Either(stmts) -> Either(List.map resolve_while_stmt stmts)
        | ForEach(p,e,s1) -> ForEach(p,e,(resolve_while_stmt s1))
        | While(e,s1) ->
            While({e with exp = Lit(True)},
                Block([
                    Either([
                        Block([
                            ExpStmt(e);
                            (resolve_while_stmt s1)
                        ]);
                        Block([
                            ExpStmt({e with exp = Not(e)});
                            Break
                        ])
                    ])
                ])
            )
        | _ -> stmt

let resolve (Program(macros,(Network(p,stmt)))) f =
    let macros' = List.map (fun (Macro(s,p,stmt)) ->
        Macro(s,p,(f stmt))
    ) macros in
    let net' = Network(p,(f stmt)) in
        Program(macros',net')

let intermediate ast =
    (*let ast' = resolve ast resolve_while_stmt in*)
    let ast'' = resolve ast resolve_if_stmt in
        resolve ast'' resolve_exp_stmt
        (*ast''*)