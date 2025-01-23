{
open Sexplib.Std
type t = Mul of (string * string) | Do | Don't
[@@deriving sexp]
}
let digits = ['0' - '9'](['0' - '9']*)
let tuple = "mul(" (digits as d1) ',' (digits as d2) ')'

rule read = parse
| tuple                { Mul (d1, d2) :: read lexbuf }
| "do()"               { Do :: read lexbuf }
| "don't()"            { Don't :: read lexbuf }
| eof                  { [] }
| _                    { read lexbuf }  (* Ignore other characters *)
