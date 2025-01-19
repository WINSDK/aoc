open! Core

let input ~day ~year =
  let rec working_dir path =
    if Filename.check_suffix path "advent-of-code"
    then path
    else (
      let parent = Filename.concat path Filename.parent_dir_name in
      let parent = Filename_unix.realpath parent in
      working_dir parent)
  in
  let working_dir = working_dir Sys_unix.executable_name in
  let rec concat path components =
    match components with
    | [] -> path
    | comp :: tl -> concat (Filename.concat path comp) tl
  in
  let path =
    concat working_dir [ sprintf "%d" year; "input"; sprintf "day%02d.txt" day ]
  in
  In_channel.read_all path
;;

let cmd ~f ~day = Command.basic ~summary:(sprintf "Day %02d" day) (Command.Param.return f)
let one ~f ~day ~year = sprintf "%02d" day, cmd ~f:(fun () -> input ~day ~year |> f) ~day

let both ~part1 ~part2 ~day ~year =
  ( sprintf "%02d" day
  , cmd ~f:(fun () -> List.iter [ part1; part2 ] ~f:(fun f -> input ~day ~year |> f)) ~day
  )
;;
