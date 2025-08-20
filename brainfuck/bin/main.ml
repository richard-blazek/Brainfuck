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
  Js_of_ocaml.Js.export "brainfuck"
    (object%js
       method hello name =
         Js_of_ocaml.Js.string (compile (parse (Js_of_ocaml.Js.to_string name)))
    end)
