
%{
    #include "gpp_interpreter.tab.h"
    #include<string.h>
    // In gpp_lexer.l and gpp_interpreter.y


%}

%%

[ \t\n\r\f\v]+ ;   // Ignore whitespace



"and" { return KW_AND; }
"or" { return KW_OR; }
"not" { return KW_NOT; }
"equal" { return KW_EQUAL; }
"less" { return KW_LESS; }
"nil" { return KW_NIL; } 
"list" { return KW_LIST; }
"append" { return KW_APPEND; }
"concat" { return KW_CONCAT; }
"set" { return KW_SET; }
"def" { return KW_DEF; }
"for" { return KW_FOR; }
"if" { return KW_IF; }
"else" { return KW_ELSE;}
"exit" { return KW_EXIT; }
"load" { return KW_LOAD; }
"display" { return KW_DISP; }
"true" { yylval.str = strdup("1f-1");
return KW_TRUE; }
"false" { yylval.str = strdup("0f-1");
    return KW_FALSE; }
"+" { return OP_PLUS; }
"-" { return OP_MINUS; }
"/" { return OP_DIV; }
"*" { return OP_MULT; }
"(" { return OP_OP; }
")" { return OP_CP; }
"," { return OP_COM; }
[0-9]+f[1-9]+ {
    yylval.str = strdup(yytext);
    return VALUEF;
}

[a-zA-Z][a-zA-Z0-9]* { yylval.str = strdup(yytext);
return IDENTIFIER; }
. { printf("Unrecognized character: %s\n", yytext); }


%%
int yylex();

int yywrap(){}




