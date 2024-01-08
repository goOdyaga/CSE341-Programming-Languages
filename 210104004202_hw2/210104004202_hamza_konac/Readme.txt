 I have used DFA for lisp and flex code.Firsly I have tokenized input,
 Then I have determined type of token.

You can run flex code at the below:

flex gpp_lexer.l

gcc lex.yy.c 

./out #no input file

./out test.gpp #run with sample file

You can run lisp code at the below:

clisp gpp_lexer.lisp #run with no input file

clisp gpp_lexer.lisp test.gpp #run with input file
