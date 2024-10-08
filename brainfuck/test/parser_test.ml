open Brainfuck.Parser

let check = Alcotest.check Alcotest.string "string should be equal"

let test_stringify () =
  let one_test str cmds = check str (stringify cmds) in
  one_test "" [];
  one_test "+" [Add];
  one_test "-" [Sub];
  one_test ">" [Right];
  one_test "<" [Left];
  one_test "." [Put];
  one_test "," [Get];
  one_test "++" [Add; Add];
  one_test "+-" [Add; Sub];
  one_test "-+" [Sub; Add];
  one_test "<>" [Left; Right];
  one_test ".,><--+" [Put; Get; Right; Left; Sub; Sub; Add];
  one_test "[]" [Loop []];
  one_test "[+]" [Loop [Add]];
  one_test "+>[[-]>]+[]" [Add; Right; Loop [Loop [Sub]; Right]; Add; Loop []]

let test_parse () =
  let one_test code cannonical = check cannonical (stringify (parse code)) in
  one_test "" "";
  one_test "+" "+";
  one_test "-" "-";
  one_test "<" "<";
  one_test ">" ">";
  one_test "." ".";
  one_test "," ",";
  one_test "Oh Camel!" "";
  one_test "++" "++";
  one_test "+-" "+-";
  one_test ">+-" ">+-";
  one_test "<,/" "<,";
  one_test "[]" "[]";
  one_test "[][-+-][+-[.,[>++[--<>><><><><]...]>,,]+[]]" "[][-+-][+-[.,[>++[--<>><><><><]...]>,,]+[]]";
  one_test "]+++" "";
  one_test "<>]--" "<>";
  one_test "[" "[]";
  one_test "[[[" "[[[]]]";
  one_test "+[]]--" "+[]";
  one_test "+[[[[]+" "+[[[[]+]]]"
