open! Core
open Utils

module Config = struct
  let short_input = {|
3   4
4   3
2   5
1   3
3   9
3   3|}

  let real_input = lazy (Utils.Commands.input ~day:1 ~year:2024)
end

module Parsed = struct
  type t = int list * int list [@@deriving sexp]

  let parse input =
    input
    |> Utils.split_whitespace
    |> List.map ~f:Utils.split_space
    |> List.map ~f:(List.map ~f:Int.of_string)
    |> List.map ~f:Utils.e2
    |> List.unzip
  ;;

  let%expect_test "short_input" =
    printf !"%{sexp: t}\n%!" (parse Config.short_input);
    [%expect {| ((3 4 2 1 3 3) (4 3 5 3 9 3)) |}]
  ;;
end

let with_parsed f input =
  let parsed = Parsed.parse input in
  f parsed
;;

module Part1 = struct
  let f input =
    let l, r = input in
    let l, r = List.sort l ~compare:Int.compare, List.sort r ~compare:Int.compare in
    let result = List.fold2_exn l r ~init:0 ~f:(fun acc x y -> acc + abs (x - y)) in
    printf "part 1: %d\n" result
  ;;

  let%expect_test "part 1" =
    with_parsed f Config.short_input;
    [%expect {| part 1: 11 |}]
  ;;
end

module Part2 = struct
  let f input =
    let l, r = input in
    let counts =
      List.fold r ~init:Int.Map.empty ~f:(fun counts k ->
        Map.update counts k ~f:(function
          | Some v -> v + 1
          | None -> 1))
    in
    let result =
      List.fold l ~init:0 ~f:(fun acc x ->
        match Map.find counts x with
        | Some c -> acc + (c * x)
        | None -> acc)
    in
    printf "part 2: %d\n" result
  ;;

  let%expect_test "part 2" =
    with_parsed f Config.short_input;
    [%expect {| part 2: 31 |}]
  ;;
end

(* let command = Commands.one ~f:(with_parsed Part2.f) ~day:1 ~year:2024 *)
let command =
  Commands.both
    ~part1:(with_parsed Part1.f)
    ~part2:(with_parsed Part2.f)
    ~day:1
    ~year:2024
;;
