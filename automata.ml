(*
 * Kevin Angstadt
 * Represents a Micron Automaton
 *)
 
type start =
    | NotStart
    | Start
    | AllStart

type at_target =
    | Pulse
    | Latch
    | Roll

type combinatorial =
    | Inverter
    | OR
    | AND
    | NAND
    | NOR
    | SOP
    | POS
    | NSOP
    | NPOS

type network = (string, element) Hashtbl.t
type element =
    | STE of string * string * start * boolean * boolean * element list * boolean (* id, symbol set, start, case, latch, activate on match, report on match *)
    | Counter of string * int * at_target * boolean * element list (* id, target, at_target, report on target, activate on target *)
    | Combinatorial of string * boolean * boolean * element list (* id, eod, report on high, activate on high *)

