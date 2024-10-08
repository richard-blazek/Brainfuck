type command = Add | Sub | Left | Right | Put | Get | Loop of command list

let rec parseChars = function
| [] -> [], []
| '+' :: rest -> let cmds, next = parseChars rest in Add :: cmds, next
| '-' :: rest -> let cmds, next = parseChars rest in Sub :: cmds, next
| '<' :: rest -> let cmds, next = parseChars rest in Left :: cmds, next
| '>' :: rest -> let cmds, next = parseChars rest in Right :: cmds, next
| '.' :: rest -> let cmds, next = parseChars rest in Put :: cmds, next
| ',' :: rest -> let cmds, next = parseChars rest in Get :: cmds, next
| ']' :: rest -> [], rest
| '[' :: rest ->
  let loopCmds, rest2 = parseChars rest in
  let nextCmds, next = parseChars rest2 in
  Loop loopCmds :: nextCmds, next
| _ :: rest -> parseChars rest

let parse s =
  let chars = List.init (String.length s) (String.get s) in
  let cmds, _ = parseChars chars in
  cmds

let rec stringify = function
| Add :: rest -> "+" ^ stringify rest
| Sub :: rest -> "-" ^ stringify rest
| Right :: rest -> ">" ^ stringify rest
| Left :: rest -> "<" ^ stringify rest
| Put :: rest -> "." ^ stringify rest
| Get :: rest -> "," ^ stringify rest
| Loop cmds :: rest -> "[" ^ stringify cmds ^ "]" ^ stringify rest
| [] -> ""
