(*
 * Abstract grammar for configuration language
 *)
 
type config = variable list
and variable = string * size
and size = int * typ
and typ =
    | StringInfo
    | IntInfo
    | ArrayInfo of size
and range =
    | RangeValue of int * int
    | SingleValue of int