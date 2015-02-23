%{
(*
 * Kevin Angstadt
 * Parser for AP Language
 *)
 
 open Language
 
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
%token	<Util.loc>        TOR
%token	<Util.loc>        TNOT
%token	<Util.loc>        TFOREACH
%token	<Util.loc>        TWHILE
%token	<Util.loc>        TIF
%token	<Util.loc>        TELSE

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

void: { } ;

params:
    | /* empty */ { Parameters( [] ) }
    | param_list  { Parameters($1) }
;

param_list:
      formal_param_dec { [$1] }
    | formal_param_dec TCOMMA param_list { $1 :: $3 }
;

args:
      void { Arguments( [] ) }
    | arg_list { Arguments($1) }
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
            Param((name,NoOffset), (create_type level))
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

statement:
      if_statement { $1 }
    | foreach_statement { $1 }
    | while_statement { $1 }
    | block { $1 }
    | TREPORT TSEMICOLON { Report }
    | declaration { $1 }
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
      IDENT { let(a,loc) = $1 in (a,NoOffset) }
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
      TINT { Int }
    | TSTRING { String }
    | TCHAR { Char }
    | TCOUNTER { Counter }
;

if_statement:
      TIF TLPAREN expression TRPAREN statement %prec TTHEN { If($3,$5,Block([])) }
    | TIF TLPAREN expression TRPAREN statement TELSE statement { If($3,$5,$7) }
;

foreach_statement:
      TFOREACH TLPAREN formal_param_dec TCOLON operand TRPAREN statement { ForEach($3,$5,$7) }
;

while_statement:
      TWHILE TLPAREN expression TRPAREN statement { While($3,$5) }

expression_statement:
      expression TSEMICOLON { ExpStmt(Some $1) }
    | TSEMICOLON { ExpStmt(None) }
;

expression:
      disjunction                   { $1 }
;

disjunction:
      conjunction                   { $1 }
    | disjunction TOR conjunction   { Or($1,$3,None) }
;

conjunction:
      equality                      { $1 }
    | conjunction TAND equality     { And($1,$3,None) }
;

/*(* TODO: Does this make sense to not allow chaining? *)*/
equality:
      relation                      { $1 }
    | relation TEQ relation         { EQ($1,$3,None) }
    | relation TNEQ relation        { NEQ($1,$3,None) }
;

relation:
      addition                      { $1 }
    | relation TLEQ addition        { LEQ($1,$3,None) }
    | relation TGEQ addition        { GEQ($1,$3,None) }
    | relation TGT addition         { GT($1,$3,None) }
    | relation TLT addition         { LT($1,$3,None) }
;

addition:
      multiplication                { $1 }
    | addition TPLUS multiplication { Plus($1,$3,None) }
    | addition TMINUS multiplication{ Minus($1,$3,None) }
;

multiplication:
      negation                      { $1 }
    | multiplication TTIMES negation{ Times($1,$3,None) }
    | multiplication TMOD negation  { Mod($1,$3,None) }

negation:
    | TNOT operand                  { Not($2,None) }
    | TMINUS operand %prec UMINUS   { Negative($2) }
    | operand                       { $1 }
;

operand:
      literal { Lit($1) }
    | TINPUT TLPAREN TRPAREN { Input }
    /* TODO make this actually for lvals */
    | IDENT {  
        let name,loc = $1 in
            Lval((name,NoOffset),None)
        }
    | IDENT TDOT IDENT TLPAREN args TRPAREN {
        let name,loc1 = $1 in
        let func,loc2 = $3 in
            Fun((name,NoOffset),func,$5,None)
    }
    | TLPAREN expression TRPAREN { $2 }
;

literal:
      INT {
        let value,loc = $1 in
            IntLit(value,Int)
    }
    | STRINGLIT {
        let value,loc = $1 in
            StringLit(value,String)
    }
    | CHARLIT {
        let value,loc = $1 in
            CharLit(value,Char)
    }
    | TRUE { True }
    | FALSE { False }
;
