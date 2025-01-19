open! Core

let e1 = function
  | [ a ] -> a
  | _ -> raise_s [%message "Unexpected number of elements"]
;;

let e2 = function
  | [ a; b ] -> a, b
  | xs -> raise_s [%message "Unexpected number of elements" (xs : int list)]
;;

let e3 = function
  | [ a; b; c ] -> a, b, c
  | _ -> raise_s [%message "Unexpected number of elements"]
;;

let e4 = function
  | [ a; b; c; d ] -> a, b, c, d
  | _ -> raise_s [%message "Unexpected number of elements"]
;;

let not_empty s = List.filter s ~f:(fun s -> not (String.is_empty s))
let split_whitespace s = String.split_lines s |> not_empty
let split_space s = String.split s ~on:' ' |> not_empty
