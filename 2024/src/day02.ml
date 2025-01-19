open! Core
open Utils

let short_input = {|
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
|}

module Parsed = struct
  type t = int list list [@@deriving sexp]

  let parse input =
    input
    |> Utils.split_whitespace
    |> List.map ~f:Utils.split_space
    |> List.map ~f:(List.map ~f:Int.of_string)
  ;;

  let%expect_test "short_input" =
    printf !"%{sexp: t}\n%!" (parse short_input);
    [%expect
      {| ((7 6 4 2 1) (1 2 7 8 9) (9 7 6 2 1) (1 3 2 4 5) (8 6 4 4 1) (1 3 6 7 9)) |}]
  ;;
end

let with_parsed f input =
  let parsed = Parsed.parse input in
  f parsed
;;

let rec gap_detection = function
  | [] | [ _ ] -> true
  | x :: y :: tl -> if abs (x - y) <= 3 then gap_detection (y :: tl) else false
;;

let rec is_strictly cmp lst =
  match lst with
  | [] | [ _ ] -> true
  | x :: y :: tl -> if cmp x y then is_strictly cmp (y :: tl) else false
;;

let is_valid lst = gap_detection lst && (is_strictly ( < ) lst || is_strictly ( > ) lst)

module Part1 = struct
  let f input =
    let result = List.filter input ~f:is_valid |> List.length in
    printf "part 1: %d\n" result
  ;;

  let%expect_test "part 1" =
    with_parsed f short_input;
    [%expect {| part 1: 2 |}]
  ;;
end

module Part2 = struct
  let f input =
    let enumerate_valid lst =
      let drop_one lst idx = List.take lst idx @ List.drop lst (idx + 1) in
      let variations = List.mapi lst ~f:(fun idx _ -> drop_one lst idx) in
      List.exists (lst :: variations) ~f:is_valid
    in
    let result = List.filter input ~f:enumerate_valid |> List.length in
    printf "part 2: %d\n" result
  ;;

  let%expect_test "part 2" =
    with_parsed f short_input;
    [%expect {| part 2: 4 |}]
  ;;
end

(* let command = Commands.one ~f:(with_parsed Part1.f) ~day:2 ~year:2024 *)
let command =
  Commands.both
    ~part1:(with_parsed Part1.f)
    ~part2:(with_parsed Part2.f)
    ~day:2
    ~year:2024
;;
