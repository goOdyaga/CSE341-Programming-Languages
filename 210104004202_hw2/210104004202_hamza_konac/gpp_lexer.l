
%{
    //#include "gpp_interpreter.tab.h"
    #include<string.h>
    // In gpp_lexer.l and gpp_interpreter.y
//#include "number.h"

%}

%%

[\x20]+|[\x09]+|[\x10-\x13]+ ;   // Ignore whitespace



"and" { printf(" KW_AND; \n");}
"or" { printf(" KW_OR; \n");}
"not" { printf(" KW_NOT; \n");}
"equal" { printf(" KW_EQUAL; \n");}
"less" { printf(" KW_LESS; \n");}
"nil" { printf(" KW_NIL; \n");}
"list" { printf(" KW_LIST; \n");}
"append" { printf(" KW_APPEND; \n");}
"concat" { printf(" KW_CONCAT; \n");}
"set" { printf(" KW_SET; \n");}
"def" { printf(" KW_DEF; \n");}
"for" { printf(" KW_FOR; \n");}
"if" { printf(" KW_IF; \n");}
"exit" { printf(" KW_EXIT; \n");}
"load" { printf(" KW_LOAD; \n");}
"display" { printf(" KW_DISP; \n");}
"true" { printf(" KW_TRUE; \n");}
"false" { printf(" KW_FALSE; \n");}
"+" { printf(" OP_PLUS; \n");}
"-" { printf(" OP_MINUS; \n");}
"/" { printf(" OP_DIV; \n");}
"*" { printf(" OP_MULT; \n");}
"(" { printf(" OP_OP\n"); }
")" { printf(" OP_CP; \n");}
"," { printf(" OP_COM; \n");}
[0-9]+f[1-9]* {
    
    printf(" VALUEF;\n");}

[a-zA-Z][a-zA-Z0-9]* { printf(" IDENTIFIER; \n");}
. { printf("Unrecognized character: %s\n", yytext); }


%%

int yywrap(){}


int main(int arg1, char *argv[])
{   
    if(argv[1]!=NULL)
    {
        yyin = fopen(argv[1], "r+");
        yylex();
        fclose(yyin);
    }
    
    else
    {
        while(1) 
        {
            printf("> ");
            yylex(); 
        }
    }

    return 0;
}