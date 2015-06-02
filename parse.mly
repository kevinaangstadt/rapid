%{
(*
 * Kevin Angstadt
 * Parser for AP Language
 *)
 
 open Language
 
 let make_exp exp loc =
    {
        exp = exp ;
        expr_type = NoType ;
        loc = loc ;
    }
 
%}

%token  <int*Util.loc>    INT
%token  <string*Util.loc> IDENT
%token  <string*Util.loc> STRINGLIT
%token  <char*Util.loc>   CHARLIT
%token  <Util.loc>        TRUE
%token  <Util.loc>        FALSE

%token	<Util.loc>        TCOMMA
%token	<Util.loc>        TDOT
%token	<Util.loc>        TSEMICOLON
%token	<Util.loc>        TCOLON
%token	<Util.loc>        TLPAREN
%token	<Util.loc>        TRPAREN
%token	<Util.loc>        TLBRACE
%token	<Util.loc>        TRBRACE
%token  <Util.loc>        TLBRACKET
%token  <Util.loc>        TRBRACKET
%token	<Util.loc>        TINT
%token  <Util.loc>        TSTRING
%token  <Util.loc>        TCHAR
%token  <Util.loc>        TLIST
%token	<Util.loc>        TCOUNTER
%token	<Util.loc>        TMACRO
%token	<Util.loc>        TNETWORK
%token	<Util.loc>        TREPORT
%token	<Util.loc>        TFILTER
%token  <Util.loc>        TINPUT
%token  <Util.loc>        TMINUS
%token  <Util.loc>        TPLUS
%token  <Util.loc>        TTIMES
%token  <Util.loc>        TMOD
%token	<Util.loc>        TASSIGN
%token	<Util.loc>        TEQ
%token	<Util.loc>        TNEQ
%token	<Util.loc>        TLEQ
%token	<Util.loc>        TGEQ
%token	<Util.loc>        TLT
%token	<Util.loc>        TGT
%token	<Util.loc>        TAND
%token  <Util.loc>        TPAND
%token	<Util.loc>        TOR
%token	<Util.loc>        TNOT
%token	<Util.loc>        TFOREACH
%token	<Util.loc>        TWHILE
%token  <Util.loc>        TWHENEVER
%token	<Util.loc>        TIF
%token	<Util.loc>        TELSE
%token  <Util.loc>        TEITHER
%token  <Util.loc>        TORELSE
%token  <Util.loc>        TALLOF
%token  <Util.loc>        TANDALSO
%token  <Util.loc>        TSOME

%token	EOF

%left TAND
%left TOR
%left TEQ TNEQ
%left TLEQ TGEQ TLT TGT
%nonassoc TNOT UMINUS
%right TTHEN TELSE

%start program
%type<Language.program> program

%%

program:
      macro_list network EOF { Program($1,$2) }
;

macro_list:
      macro { [$1] }
    | macro macro_list { $1 :: $2 }
;

macro:
      TMACRO IDENT TLPAREN params TRPAREN block {
        let ident,loc = $2 in
            Macro(ident,$4,$6)
        }
;

network:
      TNETWORK TLPAREN params TRPAREN block { Network($3,$5) }
;

params:
    | /* empty */ { Parameters( [] ) }
    | param_list  { Parameters($1) }
;

param_list:
      formal_param_dec { [$1] }
    | formal_param_dec TCOMMA param_list { $1 :: $3 }
;

args:
    | /* empty */ { Arguments( [] ) }
    | arg_list    { Arguments($1) }
;

arg_list:
      operand { [$1] }
    | operand TCOMMA arg_list { $1 :: $3 }
;

formal_param_dec:
      typ abstract_declarator {
        let rec create_type level =
            if level = 0 then $1
            else Array((create_type (level - 1)))
        in
        let name,level = $2 in
            Param(name, (create_type level))
    }
    
abstract_declarator:
      IDENT {
        let name,loc = $1 in
            (name,0)
    }
    | abstract_declarator TLBRACKET TRBRACKET {
        let name,level = $1 in
            (name,level+1)
    }

block:
      TLBRACE statement_list TRBRACE { Block($2) }
;

statement_list:
      statement_list statement { $1 @ [$2] }
    | statement { [$1] }
;

opt_semi_list:
    | /* nothing */ { [] }
    | TSEMICOLON opt_semi_list { Block([]) :: $2 }
    ;

statement:
    | opt_semi_list { Block($1) }
    | if_statement { $1 }
    | either_statement { Either($1) }
    | some_statement { $1 }
    | allof_statement { Allof($1) }
    | foreach_statement { $1 }
    | while_statement { $1 }
    | whenever_statement { $1 }
    | block { $1 }
    | TREPORT TSEMICOLON { Report }
    | declaration TSEMICOLON { $1 }
    | expression_statement { $1 }
    | IDENT TLPAREN args TRPAREN TSEMICOLON {
        let name,loc = $1 in
            MacroCall(name,$3)
    }
;

declaration:
      typ declarator_list {
        let dec_list = List.map (fun (name,value) -> (name,$1,value)) $2 in
        VarDec(List.rev dec_list)
    }
;

declarator_list:
      declarator { [$1] }
    | declarator_list TCOMMA declarator { $3 :: $1 }
;

declarator:
      direct_declarator { ($1,None) }
    | direct_declarator TASSIGN initialize { ($1,Some $3) }
;

direct_declarator:
      IDENT { let(a,loc) = $1 in a }
    /*| direct_declarator TLBRACKET TRBRACKET {  }
    | direct_declarator TLBRACKET expression TRBRACKET {  }*/
;

initialize:
      expression { PrimitiveInit($1) }
    | TLBRACE initialize_list TRBRACE { ArrayInit(List.rev $2) }
    | TLBRACE initialize_list TCOMMA TRBRACE { ArrayInit(List.rev $2) }
;

initialize_list:
      initialize { [$1] }
    | initialize_list TCOMMA initialize { $3 :: $1 }
;

typ:
    | basic_typ { $1 }
    | typ TLBRACKET TRBRACKET { Array($1) }
;

basic_typ:
      TINT { Int }
    | TSTRING { String }
    | TCHAR { Char }
    | TCOUNTER { Counter }
;

either_statement:
    | TEITHER block orelse_list { $2 :: $3 }
;

orelse_list:
    | /* empty */ { [] }
    | TORELSE block orelse_list { $2 :: $3 }
;

allof_statement:
    | TALLOF block andalso_list { $2 :: $3 }
;

andalso_list:
    | /* empty */ { [] }
    | TANDALSO block andalso_list { $2 :: $3 }
;

if_statement:
      TIF TLPAREN expression TRPAREN statement %prec TTHEN { If($3,$5,Block([])) }
    | TIF TLPAREN expression TRPAREN statement TELSE statement { If($3,$5,$7) }
;

some_statement:
    | TSOME TLPAREN formal_param_dec TCOLON operand TRPAREN statement { SomeStmt($3,$5,$7) }
;

foreach_statement:
      TFOREACH TLPAREN formal_param_dec TCOLON operand TRPAREN statement { ForEach($3,$5,$7) }
;

while_statement:
      TWHILE TLPAREN expression TRPAREN statement { While($3,$5) }
;

whenever_statement:
      TWHENEVER TLPAREN expression TRPAREN statement { Whenever($3,$5) }
;

expression_statement:
      expression TSEMICOLON { ExpStmt($1) }
;

expression:
      disjunction                   { $1 }
;

disjunction:
      conjunction                   { $1 }
    | disjunction TOR conjunction   { make_exp (Or($1,$3)) ($1.loc) }
;

conjunction:
      p_conjunction                 { $1 }
    | conjunction TAND p_conjunction{ make_exp (And($1,$3)) ($1.loc)  }
;

p_conjunction:
      equality                      { $1 }
    | p_conjunction TPAND equality  { make_exp (PAnd($1,$3)) ($1.loc) }

/*(* TODO: Does this make sense to not allow chaining? *)*/
equality:
      relation                      { $1 }
    | relation TEQ relation         { make_exp (EQ($1,$3)) ($1.loc) }
    | relation TNEQ relation        { make_exp (NEQ($1,$3)) ($1.loc) }
;

relation:
      addition                      { $1 }
    | relation TLEQ addition        { make_exp (LEQ($1,$3)) ($1.loc) }
    | relation TGEQ addition        { make_exp (GEQ($1,$3)) ($1.loc) }
    | relation TGT addition         { make_exp (GT($1,$3)) ($1.loc) }
    | relation TLT addition         { make_exp (LT($1,$3)) ($1.loc) }
;

addition:
      multiplication                { $1 }
    | addition TPLUS multiplication { make_exp (Plus($1,$3)) ($1.loc) }
    | addition TMINUS multiplication{ make_exp (Minus($1,$3)) ($1.loc) }
;

multiplication:
      negation                      { $1 }
    | multiplication TTIMES negation{ make_exp (Times($1,$3)) ($1.loc) }
    | multiplication TMOD negation  { make_exp (Mod($1,$3)) ($1.loc) }

negation:
    | TNOT operand                  { make_exp (Not($2)) ($1) }
    | TMINUS operand %prec UMINUS   { make_exp (Negative($2)) ($1) }
    | operand                       { $1 }
;

operand:
      literal {
        let lit,loc = $1 in
            make_exp (Lit(lit)) (loc)
    }
    | TINPUT TLPAREN TRPAREN { make_exp (Input) ($1) }
    /* TODO make this actually for lvals */
    | IDENT {  
        let name,loc = $1 in
            make_exp (Lval((name,NoOffset))) loc
        }
    | IDENT TDOT IDENT TLPAREN args TRPAREN {
        let name,loc1 = $1 in
        let func,loc2 = $3 in
            make_exp (Fun((name,NoOffset),func,$5)) loc1
    }
    | TLPAREN expression TRPAREN { $2 }
;

literal:
      INT {
        let value,loc = $1 in
            IntLit(value,Int),loc
    }
    | STRINGLIT {
        let value,loc = $1 in
            StringLit(value,String),loc
    }
    | CHARLIT {
        let value,loc = $1 in
            CharLit(value,Char),loc
    }
    | TRUE { True,$1 }
    | FALSE { False,$1 }
;
