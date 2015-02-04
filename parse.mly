%{
(*
 * Kevin Angstadt
 * Parser for AP Language
 *)
 
 open Language
 
 let scope = ref NetworkScope
 
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
        scope := MacroScope ; Macro(ident,$4,$6)
        }
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
      typ IDENT {
        let name,loc = $2 in
            Param(name, $1)
    }

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
    | while_statement { $1 }
    | block { $1 }
    | TREPORT TSEMICOLON { Report(!scope) }
    | declaration { $1 }
    | expression_statement { $1 }
    | IDENT TLPAREN args TRPAREN TSEMICOLON {
        let name,loc = $1 in
            MacroCall(name,$3,!scope)
    }
;

declaration:
      typ IDENT TSEMICOLON {
        let name,loc = $2 in
            VarDec(name,$1,!scope)
    }
;

typ:
      TINT { Int }
    | TSTRING { String }
    | TCHAR { Char }
    | TCOUNTER { Counter }
    | TLIST { List }
;

if_statement:
      TIF TLPAREN expression TRPAREN statement %prec TTHEN { IfWhile($3,$5,Block([],!scope),false,!scope) }
    | TIF TLPAREN expression TRPAREN statement TELSE statement { IfWhile($3,$5,$7,false,!scope) }
;

foreach_statement:
      TFOREACH TLPAREN input_variable TCOLON operand TRPAREN statement { ForEach($3,$5,$7,!scope) }
;

while_statement:
      TWHILE TLPAREN expression TRPAREN statement { IfWhile($3,$5,ExpStmt(None,!scope),true,!scope) }

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
    | TINPUT TLPAREN TRPAREN { Input }
    | IDENT {
        let name,loc = $1 in
            Var(name)
        }
    | IDENT TDOT IDENT TLPAREN args TRPAREN {
        let name,loc1 = $1 in
        let func,loc2 = $3 in
            Fun(name,func,$5)
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
