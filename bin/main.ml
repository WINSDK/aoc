open! Core

let command =
  Command.group
    ~summary:"Advent of Code"
    [ Aoc2024.Day01.command; Aoc2024.Day02.command ; Aoc2024.Day03.command ]
;;

let () = Command_unix.run command
