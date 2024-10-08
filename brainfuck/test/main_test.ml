open Alcotest

let () =
  run "Brainfuck" [
      "Parser", [
        test_case "stringify" `Quick Parser_test.test_stringify;
        test_case "parse"     `Quick Parser_test.test_parse;
      ];
    ]
