interpreter code seperated 3 source file .

ast.lisp contain structures, enums and functiosn which
are manipualte tokens and create AST(Abstract Syntax Tree) from the token respect CFG(Contex free grammers) rules.

gpp_lexer.lisp tokenized the input

gpp_interpreter.lisp file perform CFG and AST.

makefile created to run interpreter with file or by console

Enter below command to run interpreter with test file:

make FILENAME=your-file-name.g++

Enter below command to run interpreter without test file:

make