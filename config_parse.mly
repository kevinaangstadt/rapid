%{
(*
 * Kevin Angstadt
 * Parser for configuration files
 *)
 
 open Config
%}

%token	<Util.loc> TCOMMA
%token	<Util.loc> TCOLON
%token	<Util.loc> TLBRACE
%token	<Util.loc> TRBRACE
%token	<Util.loc> TLBRACKET
%token	<Util.loc> TRBRACKET
%token  <Util.loc> TNAME
%token	<Util.loc> TDATA
%token	<Util.loc> TLENGTH
%token	<Util.loc> TTYPE
%token	<Util.loc> TLOW
%token	<Util.loc> THIGH
%token	<Util.loc> TSTRING
%token  <Util.loc> TINT
%token	<Util.loc> TARRAY

%token	<int * Util.loc> INT
%token	<string * Util.loc> STRINGLIT
%token EOF

%start config
%type<Config.config> config

%%

config:
    | TLBRACKET config_array TRBRACKET EOF          { $2 }
    | TLBRACKET config_array TRBRACKET TCOMMA EOF   { $2 }
;

config_array:
    | config_entry                      { [$1] }
    | config_entry TCOMMA               { [$1] }
    | config_entry TCOMMA config_array  { $1 :: $3 }
;

config_entry:
    | TLBRACE TNAME TCOLON STRINGLIT TCOMMA TDATA TCOLON data TRBRACE {
        let name,_ = $4 in
            (name,$8)
    }
    | | TLBRACE TNAME TCOLON STRINGLIT TCOMMA TDATA TCOLON data TCOMMA TRBRACE {
        let name,_ = $4 in
            (name,$8)
    }
    | TLBRACE TDATA TCOLON data TCOMMA TNAME TCOLON STRINGLIT TRBRACE {
        let name,_ = $8 in
            (name,$4)
    }
    | | TLBRACE TDATA TCOLON data TCOMMA TNAME TCOLON STRINGLIT TCOMMA TRBRACE {
        let name,_ = $8 in
            (name,$4)
    }
;

data:
    | TLBRACE TLENGTH TCOLON number TCOMMA TTYPE TCOLON typ TRBRACE {
        ($4,$8)
    }
    | TLBRACE TLENGTH TCOLON number TCOMMA TTYPE TCOLON typ TCOMMA TRBRACE {
        ($4,$8)
    }
    | TLBRACE TTYPE TCOLON typ TCOMMA TLENGTH TCOLON number TRBRACE {
        ($8,$4)
    }
    | TLBRACE TTYPE TCOLON typ TCOMMA TLENGTH TCOLON number TCOMMA TRBRACE {
        ($8,$4)
    }
;

number:
    | INT {
        let value,_ = $1 in
        value
    }
;

number_list:
    | value                     { [$1] }
    | value TCOMMA              { [$1] }
    | value TCOMMA number_list  { $1 :: $3 }
;

value:
    | INT {
        let value,_ = $1 in
            SingleValue(value)
    }
    | TLBRACE TLOW TCOLON INT TCOMMA THIGH TCOLON INT TRBRACE {
        let low,_ = $4 in
        let high,_ = $8 in
            RangeValue(low,high)
    }
    | TLBRACE TLOW TCOLON INT TCOMMA THIGH TCOLON INT TCOMMA TRBRACE {
        let low,_ = $4 in
        let high,_ = $8 in
            RangeValue(low,high)
    }
    | TLBRACE THIGH TCOLON INT TCOMMA TLOW TCOLON INT TRBRACE {
        let low,_ = $8 in
        let high,_ = $4 in
            RangeValue(low,high)
    }
    | TLBRACE THIGH TCOLON INT TCOMMA TLOW TCOLON INT TCOMMA TRBRACE {
        let low,_ = $8 in
        let high,_ = $4 in
            RangeValue(low,high)
    }
;

typ:
    | TSTRING  { StringInfo }
    | TINT     { IntInfo }
    | data     { ArrayInfo($1) }