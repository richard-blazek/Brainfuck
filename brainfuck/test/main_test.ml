open Alcotest

let () =
  run "Brainfuck" [
      "Parser", [
        test_case "stringify" `Quick Parser_test.test_stringify;
        test_case "parse"     `Quick Parser_test.test_parse;
      ];
      "Tape", [
        test_case "add/sub"   `Quick Tape_test.test_add_sub;
        test_case "right/left"   `Quick Tape_test.test_right_left;
      ];
    ]
