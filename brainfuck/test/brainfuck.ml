open Alcotest
open Brainfuck.Parser

let test_stringify () =
  let test str cmds = check string "str eq" str (stringify cmds) in
  test "" [];
  test "+" [Add];
  test "-" [Sub];
  test ">" [Right];
  test "<" [Left];
  test "." [Put];
  test "," [Get];
  test "++" [Add; Add];
  test "+-" [Add; Sub];
  test "-+" [Sub; Add];
  test "<>" [Left; Right];
  test ".,><--+" [Put; Get; Right; Left; Sub; Sub; Add];
  test "[]" [Loop []];
  test "[+]" [Loop [Add]];
  test "+>[[-]>]+[]" [Add; Right; Loop [Loop [Sub]; Right]; Add; Loop []]

let test_parse () =
  let test code cannonical =
    check string "str eq" cannonical (stringify (parse code)) in
  test "" "";
  test "+" "+";
  test "-" "-";
  test "<" "<";
  test ">" ">";
  test "." ".";
  test "," ",";
  test "Oh Camel!" "";
  test "++" "++";
  test "+-" "+-";
  test ">+-" ">+-";
  test "<,/" "<,";
  test "[]" "[]";
  test "[][-+-][+-[.,[>++[--<>><><><><]...]>,,]+[]]" "[][-+-][+-[.,[>++[--<>><><><><]...]>,,]+[]]";
  test "]+++" "";
  test "<>]--" "<>";
  test "[" "[]";
  test "[[[" "[[[]]]";
  test "+[]]--" "+[]";
  test "+[[[[]+" "+[[[[]+]]]"

let () =
  run "Brainfuck" [
      "Parser", [
        test_case "stringify" `Quick test_stringify;
        test_case "parse"     `Quick test_parse;
      ];
    ]
