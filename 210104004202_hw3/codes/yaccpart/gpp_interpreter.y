%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_FUNCTIONS 100

//#include "lex.yy.c" // Include this if specific Flex functions or variables are used
void yyerror(const char *s);
int yylex();

extern FILE *yyin;

int gcd(int a, int b) {
    if (b == 0) {
        return a;
    } else {
        return gcd(b, a % b);
    }
}


typedef struct {
    int intval;
    int frac;
} Fraction;

typedef enum { NODE_FRACTION, NODE_ADD,NODE_MIN,NODE_MULT,NODE_DIV ,NODE_IDEN,NODE_IF,NODE_LESS} NodeType;

typedef struct ASTNode {
    NodeType type;
    union {
        Fraction fraction;
        char *c; // Identifier
        struct {
            char operator; // '+', '-', '*', '/'
            struct ASTNode *left;
            struct ASTNode *right;
        } binaryOp; // For binary operation nodes
        struct {
            int boolean;
            struct ASTNode *condition;
            struct ASTNode *thenBranch;
            struct ASTNode *elseBranch; // Can be NULL if no else branch
        } ifNode; // For if statement nodes
    } data;
} ASTNode;

typedef struct Function {
    char* name;        // Name of the function
    char* param1;      // Name of the first parameter (or NULL if not applicable)
    char* param2;      // Name of the second parameter (or NULL if not applicable)
    ASTNode* body;     // Pointer to the AST of the function body
} Function;

Function function_table[MAX_FUNCTIONS];
int function_count = 0;

void add_function(const char* name, const char* param1, const char* param2, ASTNode* body) {
    if (function_count >= MAX_FUNCTIONS) {
        fprintf(stderr, "Function table is full\n");
        return;
    }
    function_table[function_count].name = strdup(name);
    function_table[function_count].param1 = param1 ? strdup(param1) : NULL;
    function_table[function_count].param2 = param2 ? strdup(param2) : NULL;
    function_table[function_count].body = body;
    function_count++;
}


ASTNode* create_fraction_node(int value, int frac) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_FRACTION;
    node->data.fraction.intval	 = value;
    node->data.fraction.frac = frac;
    return node;
}

ASTNode* create_identifier_node(char * identifier) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_IDEN;
    node->data.c=malloc(sizeof(char)*strlen(identifier));
    strcpy( node->data.c,identifier);
    return node;
}

ASTNode* create_if_node(ASTNode* condition, ASTNode* thenBranch, ASTNode* elseBranch ,int boolean) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = NODE_IF;
    node->data.ifNode.condition = condition;
    node->data.ifNode.thenBranch = thenBranch;
    node->data.ifNode.elseBranch = elseBranch;
    node->data.ifNode.boolean=boolean;
    return node;
}

ASTNode* create_binary_op_node(NodeType type, ASTNode* left, ASTNode* right) {
    ASTNode* node = malloc(sizeof(ASTNode));
    node->type = type;
    node->data.binaryOp.left = left;
    node->data.binaryOp.right = right;
    return node;
}


Fraction evaluate_ast(ASTNode* node) {
	Fraction fractin={0,0};
    Fraction left_val;
    Fraction right_val ;
    Fraction conditionValue ;
    if (!node) return fractin;

    switch (node->type) {
        case NODE_FRACTION:
            return node->data.fraction;
        case NODE_ADD:
            // Evaluate based on the specific operation, e.g., '+'
             left_val= evaluate_ast(node->data.binaryOp.left);
            right_val= evaluate_ast(node->data.binaryOp.right);
            fractin.intval=left_val.intval*right_val.frac + right_val.intval*left_val.frac;
            fractin.frac=left_val.frac*right_val.frac;

                break;
        case NODE_MIN:
            // Evaluate based on the specific operation, e.g., '+'
             left_val = evaluate_ast(node->data.binaryOp.left);
             right_val = evaluate_ast(node->data.binaryOp.right);
            fractin.intval=left_val.intval*right_val.frac - right_val.intval*left_val.frac;
            fractin.frac=left_val.frac*right_val.frac;
            break;
        case NODE_MULT:
            // Evaluate based on the specific operation, e.g., '+'
             left_val = evaluate_ast(node->data.binaryOp.left);
             right_val = evaluate_ast(node->data.binaryOp.right);
            fractin.intval=left_val.intval*right_val.intval;
            fractin.frac=left_val.frac*right_val.frac;
            break;
        case NODE_DIV:
            // Evaluate based on the specific operation, e.g., '+'
             left_val = evaluate_ast(node->data.binaryOp.left);
             right_val = evaluate_ast(node->data.binaryOp.right);
             fractin.intval=left_val.intval*right_val.intval;
            fractin.frac=left_val.frac*right_val.frac;
            break;

        case NODE_IF:
            // Evaluate the condition
            if(node->data.ifNode.condition==NULL)
            {
                if(node->data.ifNode.boolean==1)
                {
                    fractin= evaluate_ast(node->data.ifNode.thenBranch);
                }

                    break;

            }
            conditionValue = evaluate_ast(node->data.ifNode.condition);
            
            // Check the condition (assuming 0 as false, non-zero as true)
            if (conditionValue.intval == 1) {
                fractin= evaluate_ast(node->data.ifNode.thenBranch);
            } else if (node->data.ifNode.elseBranch != NULL) {
                fractin= evaluate_ast(node->data.ifNode.elseBranch);
            }
            // Return a default value if the condition is false and there is no else branch
            //return (Fraction){0, 1};
            break;
        case NODE_LESS:
            left_val = evaluate_ast(node->data.binaryOp.left);
            right_val = evaluate_ast(node->data.binaryOp.right);
            int leftnumber=left_val.intval*right_val.frac;
            int rightnumber=right_val.intval*left_val.frac;
            if(leftnumber<rightnumber)
            {
                fractin.intval=1;
                fractin.frac=-1;

            }
            else
            {
               fractin.intval=0;
                fractin.frac=-1; 
            }
            return fractin;
    }
    int commonDivisor = gcd(fractin.intval, fractin.frac);
    if (commonDivisor != 0) {
        fractin.intval /= commonDivisor;
        fractin.frac /= commonDivisor;
    }
    return fractin;
}

ASTNode* copy_ast(ASTNode* node) {
    if (node == NULL) return NULL;

    // Create a new node and copy the data from the original node
    ASTNode* new_node = malloc(sizeof(ASTNode));
    *new_node = *node; // Shallow copy

    // Deep copy children for binary operation nodes
    if (node->type == NODE_ADD || node->type == NODE_MIN || node->type == NODE_MULT || node->type == NODE_DIV) {
        new_node->data.binaryOp.left = copy_ast(node->data.binaryOp.left);
        new_node->data.binaryOp.right = copy_ast(node->data.binaryOp.right);
    }

    // ... Handle copying for other node types ...

    return new_node;
}

ASTNode* substitute_arguments(ASTNode* body, const char* param1, ASTNode* arg1, const char* param2, ASTNode* arg2) {
    if (body == NULL) return NULL;

    if (body->type == NODE_IDEN) {
        if (strcmp(body->data.c, param1) == 0) {
            return copy_ast(arg1); // Substitute with first argument
        } else if (strcmp(body->data.c, param2) == 0) {
            return copy_ast(arg2); // Substitute with second argument
        }
        return copy_ast(body); // Return the identifier as-is if it's not a parameter
    }

    // For binary operation nodes, recursively substitute in left and right subtrees
    if (body->type == NODE_ADD || body->type == NODE_MIN || body->type == NODE_MULT || body->type == NODE_DIV) {
        ASTNode* left = substitute_arguments(body->data.binaryOp.left, param1, arg1, param2, arg2);
        ASTNode* right = substitute_arguments(body->data.binaryOp.right, param1, arg1, param2, arg2);
        return create_binary_op_node(body->type, left, right);
    }

    // ... Handle other node types ...

    return copy_ast(body); // Default case
}



Function* find_function_body(const char* name) {
    for (int i = 0; i < function_count; i++) {
        if (strcmp(function_table[i].name, name) == 0) {
            return &function_table[i];
        }
    }
    return NULL; // Function not found
}

%}

%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST KW_ELSE
%token KW_APPEND KW_CONCAT KW_SET KW_DEF KW_FOR KW_IF KW_EXIT
%token KW_LOAD KW_DISP KW_TRUE KW_FALSE OP_PLUS OP_MINUS OP_DIV
%token OP_MULT OP_OP OP_CP OP_COM IDENTIFIER,END_OF_LINE

%left OP_PLUS OP_MINUS
%left OP_MULT OP_DIV

%union {
    char* str;       // For string values
    struct ASTNode* node;  // For AST nodes
}
%type <node> exp 
%token <str> IDENTIFIER

%token <str> VALUEF 

%%

start
  : exp {
        Fraction result=evaluate_ast($1);
        if(result.frac==0)
        {
            yyerror("Divide By 0 is illegal!!");
            // Skip further processing of this input
            YYABORT;
        }
        else if(result.frac==-1 &&result.intval==1)
        {
            printf("True\n"); 
            return 0;
        }
        else if(result.frac==-1 &&result.intval==0)
        {
            printf("False\n"); 
            return 0;
        }
        else
        {

                printf("%df%d\n",result.intval,result.frac); 
                return 0;
        }
  }
  | FUNCTION { printf("#function\n"); }
  | OP_OP KW_EXIT OP_CP { printf("\n"); exit(EXIT_SUCCESS); }
  ;

exp
  : 
    VALUEF { 
        char* token = strdup($1); 
        char* intPart = strtok(token, "f"); // Split the string at 'f' to get the integer part
        char* fracPart = strtok(NULL, "f"); // Get the fractional part

        int intval = atoi(intPart); // Convert integer part to int
        int frac = atoi(fracPart); // Convert fractional part to int
        $$ = create_fraction_node(intval, frac);
        free(token); // Free the duplicated strin      
    }


    | IDENTIFIER { $$= create_identifier_node($1); /* Handle IDENTIFIER */ }
    | OP_OP OP_PLUS exp exp OP_CP{  $$ = create_binary_op_node(NODE_ADD, $3, $4);}
    | OP_OP OP_MINUS exp exp OP_CP {  $$ = create_binary_op_node(NODE_MIN, $3, $4); }
    | OP_OP OP_MULT exp exp OP_CP{$$ = create_binary_op_node(NODE_MULT, $3, $4);}
    | OP_OP OP_DIV exp exp OP_CP{$$ = create_binary_op_node(NODE_DIV, $3, $4);}
    | OP_OP KW_LESS exp exp OP_CP{$$ = create_binary_op_node(NODE_LESS, $3, $4);}
    |OP_OP KW_IF exp exp  exp OP_CP{      $$ = create_if_node($3, $4, $5,1);}
    |OP_OP KW_IF exp exp  OP_CP {      $$ = create_if_node($3, $4, NULL,1);}
    |OP_OP KW_IF KW_TRUE exp  OP_CP {      $$ = create_if_node(NULL, $4, NULL,1);}
    |OP_OP KW_IF KW_FALSE exp  OP_CP { $$ = create_if_node(NULL, NULL, NULL,0);}
    | OP_OP IDENTIFIER exp OP_CP{;}
    | OP_OP IDENTIFIER exp exp OP_CP
    {
        Function* func_body = find_function_body($2);
          if (func_body) {
              $$  = substitute_arguments(func_body->body, func_body->param1, $3, func_body->param2, $4);
              //Fraction result = evaluate_ast(substituted_body);
              //printf("Function call result: %df%d\n", result.intval, result.frac);
          } else {
              printf("Function not found: %s\n", $2);
          }
    }
  |  OP_OP IDENTIFIER exp exp exp  OP_CP{
          Function* func_body = find_function_body($2);
          if (func_body) {
              $$  = substitute_arguments(func_body->body, func_body->param1, $3, func_body->param2, $4);
              //Fraction result = evaluate_ast(substituted_body);
              //printf("Function call result: %df%d\n", result.intval, result.frac);
          } else {
              printf("Function not found: %s\n", $2);
          }
      }
    ;
  

FUNCTION
  : OP_OP KW_DEF IDENTIFIER exp  { //add_function($3, $4);
  ; }
  | OP_OP KW_DEF IDENTIFIER IDENTIFIER exp  {
    //add_function($3, $4, $5); /* Function definition logic */ 
    }
  | OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER exp  {
        add_function($3, $4, $5, $6);
      }
  ;

%%

void yyerror(const char *s) {
    fprintf(stderr, "Syntax error!\n" );
}

int main(int argc, char* argv[]) {
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (yyin == NULL) {
            perror(argv[1]);
            return 1;
        }
        yyparse();
        fclose(yyin);
    } else {
        // Continuous user input
       printf("Enter commands (Ctrl+C to exit):\n");
       while (1) {
            printf("> ");
            char c;
            
            // Ensure the prompt is printed immediately
           yyparse();
           //printf("%d\n",getchar());
           //
        }
        }
    return 0;
}
