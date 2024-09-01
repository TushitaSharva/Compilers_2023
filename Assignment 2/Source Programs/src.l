%{
#include <stdio.h>
#include "y.tab.h"
#define MAX 2
FILE *fp;
void yerror(char *s);
int yylex();
int isUDTYPE = 0;
char table[MAX][50];
int filled = 0;
int if_found = 0;
int open_brack = 0;
%}
%option nodefault

digit [0-9]
integerset [-]?{digit}+
characterconst (\'(.)\')
stringconst ["]([^"]|\\(.|\n))*["]
booleanconst ("true"|"false")
scope ("local"|"global")
datatype ("int"|"char"|"string"|"bool")
brackets [\{\[\(\)\}\]]
punctuation (";"|","|"->")
binary ("add"|"sub"|"mul"|"div")
assign ("=")
unary ("postincr"|"postdecr")
loop ("loop while"|"for")
condition ("in case that"|"otherwise"|"do")
reserved ("return"|"class"|"this"|"declare"|"expr"|"call"|"lt"|"gt"|"geq"|"leq"|"ne"|"eq"|"and"|"or"|"neg")
null ("void")
id  [_a-zA-Z][_a-zA-Z0-9]*
newline [\n]
blank [ \t]
error .

%%

{scope} {
        
        fprintf(fp, "reserved keyword: %s\n", yytext);
        fprintf(yyout, "%s", yytext);
        return SCOPE;
    }
{datatype} {
        
        fprintf(fp, "datatype: %s\n", yytext);
        fprintf(yyout, "%s", yytext);
        return DATATYPE;
    }
{brackets} {
        char *bracksrc[6] = {"[", "]", "{", "}", "(", ")"};
        int brackdes[6] = {LSQUARE, RSQUARE, LBRACE, RBRACE, LPAREN, RPAREN};

        fprintf(fp, "special symbol: %s\n", yytext);
        fprintf(yyout, "%s", yytext);

        for(int i = 0; i < 6; i++)
        {
            if(strcmp(yytext, bracksrc[i]) == 0)
            {
                
                return brackdes[i];
            }
        }
    }
{stringconst} {
        
        char *str = malloc(yyleng -1);
        memcpy(str, yytext + 1, yyleng - 2);
        str[yyleng - 2] = 0;
        fprintf(fp, "punctuation: \"\n");
        fprintf(fp, "constant: %s\n", str);
        fprintf(fp, "punctuation: \"\n");
        fprintf(yyout, "%s", yytext);
        return STRINGS;
    }
{booleanconst} {
        
        fprintf(fp, "reserved keywords: %s\n", yytext);
        fprintf(yyout, "%s", yytext);
        return BOOLS;
    }
{characterconst} {
        
        fprintf(fp, "%s", yytext);
        char *str = malloc(yyleng -1);
        memcpy(str, yytext + 1, yyleng - 2);
        str[yyleng - 2] = 0;
        fprintf(fp, "punctuation: \'\n");
        fprintf(fp, "constant: %s\n", str);
        fprintf(fp, "punctuation: \'\n");
        fprintf(yyout, "%s", yytext);
        return CHARS;
    }
{integerset} {
        
        fprintf(fp, "constant: %s\n", yytext);
        fprintf(yyout, "%s", yytext);
        return INTS;
    }
{punctuation} {
        fprintf(fp, "punctuation: %s\n", yytext);
        fprintf(yyout, "%s", yytext);
        char *puntsrc[3] = {";", ",", "->"};
        int punctdes[3] = {SEMICOLON, COMMA, ARROW};
        for(int i = 0; i < 3; i++)
        {
            if(strcmp(yytext, puntsrc[i]) == 0)
            {
                return punctdes[i];
            }
        }
    }
{binary} {
        fprintf(fp, "operator: %s\n", yytext);
        fprintf(yyout, "%s", yytext);
        char *binsrc[4] = {"add", "sub", "mul", "div"};
        int bindes[4] = {ADD, SUB, MULT, DIV};
        for(int i = 0; i < 4; i++)
        {
            if(strcmp(yytext, binsrc[i]) == 0)
            {
                return bindes[i];
            }
        }
    }
{unary} {
        fprintf(fp, "reserved keyword: %s\n", yytext);
        fprintf(yyout, "%s", yytext);
        char *unisrc[2] = {"postincr", "postdecr"};
        int unides[2] = {POSTINCR, POSTDECR};

        for(int i = 0; i < 2; i++)
        {
            if(strcmp(yytext, unisrc[i]) == 0)
            {
                return unides[i];
            }
        }
    }
{assign} {
    fprintf(fp, "operator: =\n");
    fprintf(yyout, "%s", yytext);
        
        return ASSIGN;
    }
{loop} {
        if(strcmp(yytext, "loop while") == 0)
        {
            fprintf(fp, "reserved keyword: loop\nreserved keyword: while\n");
            fprintf(yyout, "%s", yytext);
            return WHILE;
        }

        else if(strcmp(yytext, "for") == 0)
        {
            
            fprintf(fp, "reserved keyword: %s\n", yytext);
            fprintf(yyout, "%s", yytext);
            return FOR;
        }
    }
{condition} {
        fprintf(fp, "reserved keyword: %s\n", yytext);
        if(strcmp(yytext, "in case that") == 0)
        {
            if_found = 1;
            fprintf(yyout, "%s", yytext);
            
            return IF;
        }

        else if(strcmp(yytext, "otherwise") == 0)
        {
            fprintf(yyout, "%s", yytext);
            
            return ELSE;
        }

        else if(strcmp(yytext, "do") == 0)
        {
            fprintf(yyout, "%s", yytext);
            
            return DO;
        }
    }
{reserved} {
        fprintf(fp, "reserved keyword: %s\n", yytext);
        fprintf(yyout, "%s", yytext);
        if(strcmp(yytext, "class") == 0)
        {
            isUDTYPE = 1;
        }

        char *reservedsrc[15] = {"return", "class", "this", "declare", "expr", "call", "lt", "gt", "geq", "leq", "ne", "eq", "and", "or", "neg"};
        int reservedes[15] = {RETURN, CLASS, THIS, DECLARE, EXPR, CALL, LT, GT, GEQ, LEQ, NE, EQ, AND, OR, NOT};

        for(int i = 0; i < 15; i++)
        {
            if(strcmp(yytext, reservedsrc[i]) == 0)
            {
                
                return reservedes[i];
            }
        }
    }
{null} {
        
        fprintf(fp, "datatype: %s\n", yytext);
        fprintf(yyout, "%s", yytext);
        return VOID;
    }
{id} {
        fprintf(yyout, "%s", yytext);

        if(isUDTYPE == 1)
        {
            fprintf(fp, "ID: %s\n", yytext);
            
            if(filled < MAX)
            {
                strcpy(table[filled], yytext);
                filled++;
            }
            else
            {
                fprintf(stderr, "Only two classes are allowed\n");
            }
            isUDTYPE= 0;
            return UDATATYPE;
        }

        else
        {
            fprintf(fp, "ID: %s\n", yytext);

            for(int i = 0; i < MAX; i++)
            {
                if(strcmp(table[i], yytext) == 0)
                {
                    return UDATATYPE;
                }
            }
            return ID;
        }
    }
{newline} {
    fprintf(yyout, "%s", yytext);
    yylineno++;
}
{blank} {
        fprintf(yyout, "%s", yytext);
    }

{error} {fprintf(stderr, "Lexical Error at line %d\n", yylineno);}

%%

int yywrap(void)
{
    return 1;
}
