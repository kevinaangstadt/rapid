%{
(*
 * Kevin Angstadt
 * Parser for AP Language
 *)
 
 open Langugage
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

%start macro
%type<Language.macro> macro

%%

macro:
      TMACRO IDENT TLPAREN args TRPAREN block EOF {}
;

args:
      {}
    | arg_list {}
;

arg_list:
      input_variable {}
    | arg_list TCOMMA input_variable {}
;

input_variable:
      TINT IDENT {}
    | TSTRING IDENT {}
;

block:
      TLBRACE statement_list TRBRACE { $2 }
;

statement_list:
      { []Ê}
    | statement TCOMMA statement_list { $1 :: $3 }
;

statement:
      if_statement {}
    | block {}
    | TREPORT TSEMICOLON {}
;

if_statement:
      TIF TLPAREN expression TRPAREN statement {}
    | TIF TLPAREN expression TRPAREN statement TELSE statement {}
;

expression:
      expression TAND expression { And($1,$3) }
    | expression TOR expression { Or($1,$3) }
    | expression TEQ expression { EQ($1,$3) }
    | expression TNEQ expression { NEQ($1,$3) }
    | expression TLEQ expression { LEQ($1,$3) }
    | expression TGEQ expression { GEQ($1,$3) }
    | expression TGT expression { GT($1,$3) }
    | expression TLT expression { LT($1,$3) }
    | TNOT operand { Not($2) }
    | TMINUS operand %prec UMINUS { Negative($2) } 
;

operand:
      literal { $1 }
    | IDENT {}
    | TLPAREN expression TRPAREN { $2 }
;

literal:
      INT {}
    | TRUE {}
    | FALSE {}
;
