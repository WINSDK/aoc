type t = Mul of (string * string) | Do | Don't
[@@deriving sexp]

val read : Lexing.lexbuf -> t list
