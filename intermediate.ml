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

let rec resolve_exp_stmt stmt =
    let resolve_not_stmt (ExpStmt e) =
        match e.exp with
            | Not(exp) -> begin
                match exp.exp with
                    | Or(a,b) when exp.expr_type = Boolean -> resolve_exp_stmt (ExpStmt({e with exp = And({a with exp = Not(a)},{b with exp = Not(b)})}))
                    | And(a,b) when exp.expr_type = Boolean -> resolve_exp_stmt (ExpStmt({e with exp = Or({a with exp = Not(a)},{b with exp = Not(b)})}))
                    | Or(a,b) when exp.expr_type = Automata -> resolve_exp_stmt (Either([Allof([ExpStmt({a with exp=Not(a)}); ExpStmt(b)]); Allof([ExpStmt(a);ExpStmt({b with exp=Not(b)})])]))
                    | And(a,b) when exp.expr_type = Automata -> resolve_exp_stmt (ExpStmt({e with exp = Or({a with exp=Not(a)},{b with exp = And({b with exp = EQ({b with exp = Lit(CharLit('*',Char))},{b with exp = Input})},{b with exp = Not(b)})})}))
                    | EQ(a,b) -> resolve_exp_stmt (ExpStmt({e with exp = NEQ(a,b)}))
                    | NEQ(a,b) -> resolve_exp_stmt (ExpStmt({e with exp = EQ(a,b)}))
        
                    | _ -> resolve_exp_stmt (ExpStmt(exp))
            end
    in
    match stmt with
        | Block(stmts) -> Block(List.map resolve_exp_stmt stmts)
        | Either(stmts) -> Either(List.map resolve_exp_stmt stmts)
        | ForEach(p,e,s1) -> ForEach(p,e,(resolve_exp_stmt s1))
        | While(e,s1) -> While(e,(resolve_exp_stmt s1))
        | ExpStmt(e) ->
            begin
            (* TODO This won't catch something like (a&&b) == c *)
            match e.exp with
                | Or(a,b) -> Either([Block([(resolve_exp_stmt (ExpStmt(a)))]); Block([(resolve_exp_stmt(ExpStmt(b)))])])
                | And(a,b) -> Block([(resolve_exp_stmt (ExpStmt(a))); (resolve_exp_stmt(ExpStmt(b)))])
                | EQ(a,b) when e.expr_type = Boolean -> Either([Block([(resolve_exp_stmt (ExpStmt({e with exp =And(a,b)})))]); Block([(resolve_exp_stmt(ExpStmt({e with exp = And({a with exp= Not(a)},{b with exp= Not(b)})})))])])
                | NEQ(a,b) when e.expr_type = Boolean -> Either([Block([(resolve_exp_stmt (ExpStmt({e with exp =And(a,{b with exp = Not(b)})})))]); Block([(resolve_exp_stmt(ExpStmt({e with exp = And({a with exp= Not(a)},b)})))])])
                | Not(a) -> resolve_not_stmt stmt
                | _ -> stmt
            end
        | _ -> stmt

let rec resolve_if_stmt stmt =
    match stmt with
        | Block(stmts) -> Block(List.map resolve_if_stmt stmts)
        | If(e,s1,s2) ->
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
    let ast' = resolve ast resolve_while_stmt in
    let ast'' = resolve ast' resolve_if_stmt in
        (*resolve ast'' resolve_exp_stmt*)
        ast''
        