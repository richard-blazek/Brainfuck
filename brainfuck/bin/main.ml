let program = "+[-]+-[][][][]///lorem ipsum[[---]+++<><><><><>>>]"

let () = print_endline (Brainfuck.Parser.stringify (Brainfuck.Parser.parse program))
