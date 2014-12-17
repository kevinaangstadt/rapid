%{
(*
 * Kevin Angstadt
 * Parser for AP Language
 *)
 
 open Language
%}

%token <int>    INT
%token <string> IDENT
%token TRUE
%token FALSE

%token	TCOMMA
%token	TDOT
%token	TSEMICOLON
%token	TCOLON
%token	TLPAREN
%token	TRPAREN
%token	TLBRACE
%token	TRBRACE
%token	TINT
%token  TSTRING
%token	TCOUNTER
%token	TMACRO
%token	TNETWORK
%token	TREPORT
%token	TFILTER
%token  TINPUT
%token  TMINUS
%token	TASSIGN
%token	TEQ
%token	TNEQ
%token	TLEQ
%token	TGEQ
%token	TLT
%token	TGT
%token	TAND
%token	TOR
%token	TNOT
%token	TFOREACH
%token	TWHILE
%token	TIF
%token	TELSE

%token	EOF

%left TAND
%left TOR
%left TEQ TNEQ
%left TLEQ TGEQ TLT TGT
%nonassoc TNOT UMINUS
%right TTHEN TELSE

%start macro
%type<Language.macro> macro

%%

macro:
      TMACRO IDENT TLPAREN args TRPAREN block EOF { Macro($2,$4,$6) }
;

void: { } ;

args:
      void { Args( [] ) }
    | arg_list { Args($1) }
;

arg_list:
      input_variable { [$1] }
    | input_variable TCOMMA arg_list { $1 :: $3 }
;

input_variable:
      TINT IDENT { Var($2, Int) }
    | TSTRING IDENT { Var($2, String ) }
;

block:
      TLBRACE statement_list TRBRACE { Statements($2) }
;

statement_list:
      statement_list TCOMMA statement { $1 @ [$3] }
    | statement { [] }
;

statement:
      if_statement { $1 }
    | block { Block($1) }
    | TREPORT TSEMICOLON { Report }
;

if_statement:
      TIF TLPAREN expression TRPAREN statement %prec TTHEN { IF($3,$5,Block()) }
    | TIF TLPAREN expression TRPAREN statement TELSE statement { IF($3,$5,$7) }
;

expression:
      expression TAND expression    { And($1,$3) }
    | expression TOR expression     { Or($1,$3) }
    | expression TEQ expression     { EQ($1,$3) }
    | expression TNEQ expression    { NEQ($1,$3) }
    | expression TLEQ expression    { LEQ($1,$3) }
    | expression TGEQ expression    { GEQ($1,$3) }
    | expression TGT expression     { GT($1,$3) }
    | expression TLT expression     { LT($1,$3) }
    | TNOT operand                  { Not($2) }
    | TMINUS operand %prec UMINUS   { Negative($2) } 
;

operand:
      literal { $1 }
    | IDENT { Var($1) }
    | TLPAREN expression TRPAREN { $2 }
;

literal:
      INT { IntLit($1,String) }
    | TRUE { True }
    | FALSE { False }
;
