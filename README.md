# Brainfuck compiler and interpreter
This is a compiler and an interpreter for Brainfuck, a general-purpose
programming language. Interpreter can work on any platform where it compiles,
the compiler generates ELF files for x86_64 Linux.

At the moment, compilation is not working as the generated ELF file always
causes a segfault. Interpreter is finished and working.

## Why Brainfuck?
- **Safety:**
    Brainfuck does not require manual memory management which eliminates many
    common security bugs.
- **Easy to learn:**
    Beginners can easily master the language in a single afternoon.
- **Standardized:**
    While Brainfuck lacks an official standard, there is a widely agreed-on
    specification of the eight commands all implementations of Brainfuck
    must support and their behaviour is consistent across implementations.
- **Portability:**
    There are implementations of Brainfuck for all major operating systems
    and CPU architectures.
- **Diverse ecosystem:**
    There are many competing implementations of Brainfuck, both compiled and
    interpreted, focusing on different goals. There are plugins available
    for Visual Studio Code and other editors.
- **Turing-completeness:**
    This has been one of the main objectives of the languages. Unlike other
    popular languages such as HTML or Markdown, Brainfuck is capable of
    computing any computable function.
- **Made in Switzerland:**
    Self-explanatory.
- **Industry standard:**
    Brainfuck has been used by Google, Paradigms of Intelligence Team and
    The University of Chicago. (See: https://arxiv.org/abs/2406.19108)
