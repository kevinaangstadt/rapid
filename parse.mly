%{
(*
 * Kevin Angstadt
 * Parser for AP Language
 *)
 
 open Language
 
 let scope = ref NetworkScope
%}

%token <int>    INT
%token <string> IDENT
%token <string> STRINGLIT
%token <char>   CHARLIT
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
%token  TCHAR
%token  TLIST
%token	TCOUNTER
%token	TMACRO
%token	TNETWORK
%token	TREPORT
%token	TFILTER
%token  TINPUT
%token  TCOUNT
%token  TRESET
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
      TMACRO IDENT TLPAREN params TRPAREN block { scope := MacroScope ; Macro($2,$4,$6) }
;

network:
      TNETWORK TLPAREN params TRPAREN block { scope := NetworkScope ; Network($3,$5) }
;

void: { } ;

params:
      void { Parameters( [] ) }
    | param_list { Parameters($1) }
;

param_list:
      input_variable { [$1] }
    | input_variable TCOMMA param_list { $1 :: $3 }
;

args:
      void { Arguments( [] ) }
    | arg_list { Arguments($1) }
;

arg_list:
      operand { [$1] }
    | operand TCOMMA arg_list { $1 :: $3 }
;

input_variable:
      typ IDENT { Param($2, $1) }

block:
      TLBRACE statement_list TRBRACE { Block($2,!scope) }
;

statement_list:
      statement_list statement { $1 @ [$2] }
    | statement { [$1] }
;

statement:
      if_statement { $1 }
    | foreach_statement { $1 }
    | block { $1 }
    | TREPORT TSEMICOLON { Report(!scope) }
    | declaration { $1 }
    | expression_statement { $1 }
    | IDENT TLPAREN args TRPAREN TSEMICOLON { Fun(Var($1),$3,!scope) }
    | IDENT TDOT TCOUNT TSEMICOLON { Count(Var($1),!scope) }
    | IDENT TDOT TRESET TSEMICOLON { Reset(Var($1),!scope) }
;

declaration:
      typ IDENT TSEMICOLON { VarDec($2,$1,!scope) }
;

typ:
      TINT { Int }
    | TSTRING { String }
    | TCHAR { Char }
    | TCOUNTER { Counter }
    | TLIST { List }
;

if_statement:
      TIF TLPAREN expression TRPAREN statement %prec TTHEN { IF($3,$5,Block([],!scope),!scope) }
    | TIF TLPAREN expression TRPAREN statement TELSE statement { IF($3,$5,$7,!scope) }
;

foreach_statement:
      TFOREACH TLPAREN input_variable TCOLON operand TRPAREN statement { ForEach($3,$5,$7,!scope) }
;

expression_statement:
      expression TSEMICOLON { ExpStmt(Some $1,!scope) }
    | TSEMICOLON { ExpStmt(None,!scope) }
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
    | operand                       { $1 }
;

operand:
      literal { Lit($1) }
    | TINPUT { Input }
    | IDENT { Var($1) }
    | TLPAREN expression TRPAREN { $2 }
;

literal:
      INT { IntLit($1,Int) }
    | STRINGLIT { StringLit($1,String) }
    | CHARLIT {CharLit($1,Char)}
    | TRUE { True }
    | FALSE { False }
;
