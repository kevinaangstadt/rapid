(*
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
    match stmt.stmt with
        | Block(stmts) 
        | Either(stmts) -> List.exists and_or stmts
        | While(e,s1) -> and_or s1
        | ExpStmt(e) -> e_and_or e
        | _ -> false

(*TODO: This does not work for non-equal consumes with an or inside a not
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
    match stmt.stmt with
        (*| Allof(stmts) -> Allof(List.map resolve_exp_stmt stmts)*)
        | Block(stmts) -> {stmt = Block(List.map resolve_exp_stmt stmts); loc=stmt.loc; id = stmt.id }
        | Either(stmts) -> {stmt = Either(List.map resolve_exp_stmt stmts); loc=stmt.loc; id = stmt.id }
        | SomeStmt(p,e,s1) -> {stmt = SomeStmt(p,resolve_exp e,(resolve_exp_stmt s1)); loc=stmt.loc;  id = stmt.id }
        | ForEach(p,e,s1) -> {stmt = ForEach(p,resolve_exp e,(resolve_exp_stmt s1)); loc=stmt.loc; id = stmt.id }
        | Whenever(e,s1) -> {stmt = Whenever(resolve_exp e,(resolve_exp_stmt s1)); loc=stmt.loc; id = stmt.id }
        | While(e,s1) -> {stmt = While(resolve_exp e,(resolve_exp_stmt s1)); loc=stmt.loc; id = stmt.id }
        | ExpStmt(e) -> {stmt = ExpStmt(resolve_exp e); loc=stmt.loc; id = stmt.id }
        | _ -> stmt

let rec resolve_counter_exp exp =
    match exp.expr_type with
    | Counter ->
        begin
        match exp.exp with
        | EQ(a,b) 
        | NEQ(a,b) ->
            begin
            let value = if a.expr_type = Counter then b else a in
            let counter = if a.expr_type = Counter then a else b in
            let counter_name = match counter.exp with
                | Lval(s,_) -> s
                | _ -> failwith "should be unreachable"
            in
            match exp.exp with
            | EQ(_,_) ->
                {exp with exp = PAnd({exp with exp=LEQ({counter with exp = Lval(counter_name^"l",NoOffset)},value)},
                                     {exp with exp=GEQ({counter with exp = Lval(counter_name^"r",NoOffset)},value)})}
            | NEQ(_,_) ->
                {exp with exp = Or({exp with exp=GT({counter with exp = Lval(counter_name^"l",NoOffset)},value)},
                                   {exp with exp=LT({counter with exp = Lval(counter_name^"r",NoOffset)},value)})}
            end
        | And(a,b) -> {exp with exp = And(resolve_counter_exp a, resolve_counter_exp b)}
        | PAnd(a,b) -> {exp with exp = PAnd(resolve_counter_exp a, resolve_counter_exp b)}
        | Or(a,b) -> {exp with exp = Or(resolve_counter_exp a, resolve_counter_exp b)}
        | Not(e) -> {exp with exp = Not(resolve_counter_exp e)}
        end
    | _ -> exp

let rec resolve_if_stmt stmt =
    match stmt.stmt with
        (*| Allof(stmts) -> Allof(List.map resolve_if_stmt stmts)*)
        | Block(stmts) -> {stmt = Block(List.map resolve_if_stmt stmts); loc=stmt.loc; id = stmt.id}
        | If(e,s1,s2) ->
            begin
            match e.expr_type with
            | Automata ->
                { stmt = Either([
                    { stmt = Block([
                        {stmt = ExpStmt(e); loc=e.loc; id=e.id};
                        (resolve_if_stmt s1)
                    ]); loc=stmt.loc; id=e.id };
                    { stmt = Block([
                        {stmt = ExpStmt({e with exp = Not(e)}); loc=e.loc; id=e.id};
                        (resolve_if_stmt s2)
                    ]); loc=stmt.loc; id=e.id }
                ]);
                loc=stmt.loc;
                id = stmt.id
                }
                
            | _ -> { stmt = If(e,resolve_if_stmt s1,resolve_if_stmt s2); loc=stmt.loc; id=stmt.id}
            end
        | Either(stmts) -> {stmt = Either(List.map resolve_if_stmt stmts); loc=stmt.loc; id=stmt.id}
        | SomeStmt(p,e,s1) -> {stmt = SomeStmt(p,e,(resolve_if_stmt s1)); loc=stmt.loc; id=stmt.id}
        | ForEach(p,e,s1) -> {stmt = ForEach(p,e,(resolve_if_stmt s1)); loc=stmt.loc; id=stmt.id}
        | Whenever(e,s1) -> {stmt = Whenever(e,(resolve_if_stmt s1)); loc=stmt.loc; id=stmt.id}
        | While(e,s1) -> {stmt = While(e,(resolve_if_stmt s1)); loc=stmt.loc; id=stmt.id}
        | _ -> stmt

(*TODO Not Currently used.  Possibly remove. Otherwise fix to cover all stmt types*)
let rec resolve_while_stmt stmt =
    match stmt.stmt with
        | Block(stmts) -> {stmt = Block(List.map resolve_while_stmt stmts); loc=stmt.loc; id=stmt.id}
        | If(e,s1,s2) -> {stmt = If(e,(resolve_while_stmt s1),(resolve_while_stmt s2)); loc=stmt.loc; id=stmt.id}
        | Either(stmts) -> {stmt = Either(List.map resolve_while_stmt stmts); loc=stmt.loc; id=stmt.id}
        | ForEach(p,e,s1) -> {stmt = ForEach(p,e,(resolve_while_stmt s1)); loc=stmt.loc; id=stmt.id}
        | While(e,s1) ->
            {stmt = While({e with exp = Lit(True)},
                {stmt = Block([
                    { stmt = Either([
                        { stmt = Block([
                            {stmt= ExpStmt(e); loc=e.loc; id=e.id};
                            (resolve_while_stmt s1)
                        ]); loc=s1.loc; id=s1.id};
                        {stmt = Block([
                            {stmt=ExpStmt({e with exp = Not(e)}); loc=e.loc; id=e.id};
                            {stmt=Break; loc=e.loc; id=e.id}
                        ]); loc=e.loc; id=e.id}
                    ]);
                    loc=stmt.loc;
                    id = stmt.id}
                ]);
                loc=stmt.loc;
                id = stmt.id }
            );
            loc=stmt.loc;
            id = stmt.id}
        | _ -> stmt

let resolve (Program(macros,(Network(p,stmt)))) f =
    let macros' = List.map (fun (Macro(s,p,stmt)) ->
        Macro(s,p,(f stmt))
    ) macros in
    let net' = Network(p,(f stmt)) in
        Program(macros',net')

(*TODO text_exp does not capture all possible ways whenever could be embedded*)
let implicit_whenever (Program(macros,(Network(p,stmt))) as prog) =
    let rec test_exp e =
        match e.exp with
            | EQ(e1,e2)
            | NEQ(e1,e2) when ((e1.exp = Input && (e2.exp = Lit(AllIn) || e2.exp = Lit(StartIn))) ||
                             (e2.exp = Input && (e1.exp = Lit(AllIn) || e1.exp = Lit(StartIn)))) ->
                true
            | Not(e) -> test_exp e
            | Or(e1,e2)
            | PAnd(e1,e2)
            | And(e1,e2) -> (test_exp e1) || (test_exp e2)
            | _ -> false
    in    
    let rec exists_whenever stmt =
        match stmt.stmt with
            | Either(stmts)
            (*| Allof(stmts)*)
            | Block(stmts) -> List.exists exists_whenever stmts
            | If(_,s1,s2) -> (exists_whenever s1) || (exists_whenever s2)
            | SomeStmt(_,_,s)
            | ForEach(_,_,s)
            | While(_,s)-> exists_whenever s
            | Whenever(e,s) -> (test_exp e) || exists_whenever s
            | _ -> false
    in
    let find_macro = List.exists (fun (Macro(s,p,stmt)) ->
        exists_whenever stmt
    ) macros in
    if find_macro || (exists_whenever stmt) then
        prog
    else
        let implicit_exp = {
            exp = EQ({exp=Lit(StartIn);expr_type=Automata;loc=stmt.loc;id=stmt.id}, {exp=Input;expr_type=Automata;loc=stmt.loc;id=stmt.id});
            expr_type = Automata;
            loc = stmt.loc;
            id = stmt.id
        } in
        let new_block =match stmt.stmt with
            | Block(s_list) -> {stmt = Block(List.map (fun s ->
                match s.stmt with
                    | SomeStmt(e,p,s1) -> {stmt=SomeStmt(e,p,{stmt=Whenever(implicit_exp,s1); loc=s1.loc; id=s1.id}); loc=s.loc; id=s.id}
                    | _ -> {stmt=Whenever(implicit_exp,s); loc=s.loc; id=s.id}) s_list); loc=stmt.loc; id = stmt.id}
            | _ -> failwith "unreachable"
        in
        Program(macros,(Network(p,new_block)))
    

let intermediate ast =
    (*let ast' = resolve ast resolve_while_stmt in*)
    let ast'' = resolve ast resolve_if_stmt in
    let ast''' = resolve ast'' resolve_exp_stmt in
        implicit_whenever ast'''
        (*ast''*)

let intermediate_cpu ast =
    implicit_whenever ast