open Brainfuck.Parser
open Brainfuck.Tape
open Brainfuck.Codegen

let rec execute t = function
| [] -> t
| Add :: rest -> execute (add t) rest
| Sub :: rest -> execute (sub t) rest
| Right :: rest -> execute (right t) rest
| Left :: rest -> execute (left t) rest
| Put :: rest -> print_char (Char.chr (get t)); execute t rest
| Get :: rest ->
  let input = try input_byte stdin  with _ -> -1 in
  execute (put input t) rest
| Loop loop :: rest ->
  if get t == 0 then
    execute t rest
  else
    let t2 = execute t loop in
    execute t2 (Loop loop :: rest)

let read_file path =
  let rec input_lines file =
    try
      let line = input_line file in line ^ input_lines file
    with _ ->
      close_in file;
      "" in
  input_lines (open_in path)

let () =
  match Array.length Sys.argv with
  | 2 -> let _ = execute empty (parse (read_file Sys.argv.(1))) in ()
  | 3 -> generate (parse (read_file Sys.argv.(1))) Sys.argv.(2)
  | _ -> print_endline "Usage:\n    brainfuck <input-path> [<output-path>]"
