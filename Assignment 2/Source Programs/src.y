%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
extern FILE *yyout;
extern FILE *yyin;
extern FILE *fp;
extern int yylineno;
int yylex();
extern char *yytext;
void yyerror(char *s);
int function_started = 0;
int return_statement = 0;
%}

%token SCOPE DATATYPE UDATATYPE ID CHARS STRINGS BOOLS INTS VOID
%token DECLARE EXPR CALL 
%token IF ELSE DO 
%token WHILE FOR 
%token POSTINCR POSTDECR 
%token LT GT GEQ LEQ NE EQ NOT AND OR
%token LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE 
%token SEMICOLON COMMA ARROW 
%token ADD SUB MULT DIV 
%token ASSIGN 
%token RETURN 
%token CLASS 
%token THIS
%left OR AND NOT

%nonassoc LOWER_THAN_ELSE

%%

program: 
       | program function
       | program class
       ;

constants: CHARS
         | STRINGS
         | BOOLS
         | INTS
         ;

datatype: DATATYPE
         | UDATATYPE
         | VOID
         ;

declaration: DECLARE declarebody SEMICOLON {fprintf(yyout, " : declaration statement");}
           ;

declarebody: datatype variable_list
           ;

variable_list: ID
             | ID COMMA variable_list
             ;

expression: EXPR ID ASSIGN rhs SEMICOLON {fprintf(yyout, " : expression statement");}
          | unary SEMICOLON {fprintf(yyout, " : call statement");}
          ;

rhs: operand
   | NOT predicate
   | callpredicate
   | binary
   | unary
   ;

operand: ID
       | constants
       ;

unary: POSTINCR LPAREN rhs RPAREN
     | POSTDECR LPAREN rhs RPAREN
     ;

binary: ADD LPAREN rhs COMMA rhs RPAREN 
      | SUB LPAREN rhs COMMA rhs RPAREN 
      | MULT LPAREN rhs COMMA rhs RPAREN 
      | DIV LPAREN rhs COMMA rhs RPAREN 
      ;

return: RETURN ID SEMICOLON {fprintf(yyout, " : return statement"); return_statement = 1;}
      | RETURN constants SEMICOLON {fprintf(yyout, " : return statement"); return_statement = 1;}
      | RETURN VOID SEMICOLON {fprintf(yyout, " : return statement"); return_statement = 1;}
      | RETURN callpredicate SEMICOLON {fprintf(yyout, " : return statement"); return_statement = 1;}
      | RETURN unary SEMICOLON {fprintf(yyout, " : return statement"); return_statement = 1;}
      ;

loop: for
    | while
    ;

while: WHILE LPAREN predicate RPAREN {fprintf(yyout, " : loop statement");} DO LBRACE statements RBRACE
     ;

express: EXPR ID ASSIGN rhs
       | unary
       ;

for: FOR LPAREN express SEMICOLON predicate SEMICOLON unary RPAREN {fprintf(yyout, " : loop statement");} LBRACE statements RBRACE
   | FOR LPAREN express SEMICOLON predicate SEMICOLON RPAREN {fprintf(yyout, " : loop statement");} LBRACE statements RBRACE
   ;

condition: IF LPAREN predicate RPAREN ifprintrule DO LBRACE statements RBRACE ELSE ifprintrule LBRACE statements RBRACE
         | IF LPAREN predicate RPAREN ifprintrule DO LBRACE statements RBRACE %prec LOWER_THAN_ELSE
         ;

comparators: LT|GT|GEQ|LEQ|NE|EQ;

predicate: operand
         | unary
         | binary
         | callpredicate
         | operand comparators operand
         | NOT predicate
         | LPAREN predicate RPAREN OR LPAREN predicate RPAREN 
         | LPAREN predicate RPAREN AND LPAREN predicate RPAREN 
         | predicate OR predicate
         | predicate AND predicate
         ;

classprintrule: {fprintf(yyout, " : class definition");} ;
ifprintrule: {fprintf(yyout, " : conditional statement");} ;

class: CLASS UDATATYPE classprintrule LBRACE classbody RBRACE
     | CLASS UDATATYPE LSQUARE number_of_arguements RSQUARE classprintrule LBRACE classbody RBRACE
     ;

classbody: declaration
         | function
         | classbody declaration
         | classbody function
         ;

function: SCOPE datatype ID LPAREN RPAREN { fprintf(yyout, " : function definition"); function_started = 1;} LBRACE funbody RBRACE {if(return_statement != 1){fprintf(stderr, "Encountered a function without return statement at %d\n", yylineno);  return 1;} return_statement = 0; function_started = 0;}
        | SCOPE datatype ID LSQUARE number_of_arguements RSQUARE LPAREN def_args RPAREN { fprintf(yyout, " : function definition"); function_started = 1;} LBRACE funbody RBRACE {if(return_statement != 1){fprintf(stderr, "Encountered a function without return statement at %d\n", yylineno); return 1;} return_statement = 0; function_started = 0;}
        ;

funcStatement: declaration
              | expression
              | call
              | loop
              | return
              | condition
              ;

funbody: funcStatement
       | funcStatement funbody
       ;

def_args: datatype ID 
        | def_args COMMA datatype ID
        ;

statements: statement
          | statements statement
          ;

statement: declaration
         | return
         | expression
         | condition
         | loop
         | call
         ;

call: CALL classfunc SEMICOLON {fprintf(yyout, " : call statement with object");}
    | CALL freefunc SEMICOLON {fprintf(yyout, " : call statement");}
    ;

callpredicate: CALL classfunc 
    | CALL freefunc 
    ;

call_args: constants
         | ID
         | call_args COMMA constants
         | call_args COMMA ID
         ;

number_of_arguements: INTS
                    ;

classfunc: ID ARROW ID LSQUARE number_of_arguements RSQUARE LPAREN call_args RPAREN 
         | ID ARROW ID LSQUARE RSQUARE LPAREN RPAREN
         | THIS ARROW ID LSQUARE number_of_arguements RSQUARE LPAREN call_args RPAREN 
         | THIS ARROW ID LSQUARE RSQUARE LPAREN RPAREN
         | ID ARROW ID LPAREN RPAREN
         | THIS ARROW ID LPAREN RPAREN
         ;

freefunc: ID LSQUARE number_of_arguements RSQUARE LPAREN call_args RPAREN
        | ID LSQUARE RSQUARE LPAREN RPAREN
        | ID LPAREN RPAREN
        ;

%%

void yyerror(char *s)
{
    fprintf(stderr, "error: %s at line number %d\n", s, yylineno);
}

int main(int argc, char *argv[])
{
    char inp[100], tok[100], par[100];

    sprintf(inp, "./P2/TPP/%s.clike", argv[1]);    
    sprintf(par, "./P2/TP2/seq_tokens_%s.txt", argv[1]);    
    sprintf(tok, "./P2/TP2/parsed_%s.parsed", argv[1]);

    yyin = fopen(inp, "r");
    yyout = fopen(tok, "w");
    fp = fopen(par, "w");
    int i = yyparse();

    if(i) printf("Failure!\n");
    return 0;
}