(lang dune 2.9)
(name brainfuck)

(generate_opam_files true)
(package
 (name brainfuck)
 (authors "Richard Blažek")
 (maintainers "Richard Blažek")
 (synopsis "Brainfuck compiler and interpreter")
 (depends
  ocaml
  dune
  (alcotest :with-test)))
