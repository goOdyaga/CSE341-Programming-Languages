FILENAME :=

all: clean compile1 compile2 run
clean:
	@rm -rf test gpp_interpreter.tab.c gpp_interpreter.tab.h lex.yy.c
compile1: gpp_interpreter.y gpp_lexer.l
	@bison -d gpp_interpreter.y
	@flex gpp_lexer.l
compile2: gpp_interpreter.tab.c lex.yy.c
	@gcc lex.yy.c gpp_interpreter.tab.c -o parser
run: parser 
	@./parser $(FILENAME)
