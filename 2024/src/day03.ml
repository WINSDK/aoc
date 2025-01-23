open! Core
open Utils
open Day03_lexer

let short_input =
  {|xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))|}
;;

module Parsed = struct
  type t = Day03_lexer.t list

  let parse input =
    let lexer = Lexing.from_string input in
    Day03_lexer.read lexer
  ;;

  let%expect_test "short_input" =
    printf !"%{sexp: Day03_lexer.t list}\n%!" (parse short_input);
    [%expect {| ((Mul (2 4)) Don't (Mul (5 5)) (Mul (11 8)) Do (Mul (8 5))) |}]
  ;;
end

let with_parsed f input =
  let parsed = Parsed.parse input in
  f parsed
;;

module Part1 = struct
  let f input =
    let result = input
    |> List.filter_map ~f:(function Mul t -> Some t | _ -> None)
    |> List.map ~f:(Tuple2.map ~f:Int.of_string)
    |> List.sum (module Int) ~f:(fun (a, b) -> a * b) in
    printf "part 1: %d\n" result
  ;;

  let%expect_test "part 1" =
    with_parsed f short_input;
    [%expect {| part 1: 161 |}]
  ;;
end

module Part2 = struct
  let f input =
    let rec result lst on sum = 
      match lst with
      | [] -> sum
      | Mul t :: lst ->
          let a, b = Tuple2.map t ~f:Int.of_string in
          result lst on (if on then sum + a * b else sum)
      | Do :: lst -> result lst true sum
      | Don't :: lst -> result lst false sum
    in
    printf "part 2: %d\n" (result input true 0)
  ;;

  let%expect_test "part 2" =
    with_parsed f short_input;
    [%expect {| part 2: 48 |}]
  ;;
end

(* let command = Commands.one ~f:(with_parsed Part1.f) ~day:3 ~year:2024 *)
let command =
  Commands.both
    ~part1:(with_parsed Part1.f)
    ~part2:(with_parsed Part2.f)
    ~day:3
    ~year:2024
;;
