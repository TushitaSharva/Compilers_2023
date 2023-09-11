%{
#define LABEL 1
#define PUNCTUATION 2
#define SPLSYM 3
#define DATATYPE 4
#define OPERATOR 5
#define IDENTIFIER 6
#define RESERVED 7
#define INTCONST 8
#define STRINGCONST 9
#define CHARCONST 10
#define BLANK 11
#define ROOT 12
#define ERROR 13

int if_bracket = 0;
int is_goto = 0;
%}
%option nodefault

letter [a-zA-Z]
digit [0-9]
integerset {digit}+
character (\'(.)\')
label pp[1-9][0-9]*
punctuation [:;,]
specialSymbol [\[\{\(\)\}\]]
specialIDs [@#*\-+/\\:_]
datatype ("integer_2"|"character_1"|"string"|"null")
reservedKeyWords ("in case that"|"otherwise"|"return"|"do"|"jump to"|"and"|"or"|"gt"|"gteq"|"lt"|"lteq"|"eq"|"neq")
ID (({specialIDs}|{digit}*)?{letter}+|{specialIDs}{digit}+)({letter}|{digit})*
operator [=+\-*/\%]
blank [ \t\n]
stringliterals ["]([^"]|\\(.|\n))*["]
rootliteral {ID}[ ][_][ ]{integerset}
error ({specialIDs}?{specialIDs}+)(letter|digit|specialIDs)*

%%

{punctuation} {return PUNCTUATION;}
{label} {return LABEL;}
{specialSymbol} {return SPLSYM;}
{reservedKeyWords} {return RESERVED;}
{datatype} {return DATATYPE;}
{operator} {return OPERATOR;}
{ID} {return IDENTIFIER;}
{blank} {return BLANK;}
{stringliterals} {return STRINGCONST;}
{integerset} {return INTCONST;}
{character} {return CHARCONST;}
{rootliteral} {return ROOT;}
{error} {return ERROR;}
. {return ERROR;}

%%
int yywrap()
{
    fclose(yyin);
    fclose(yyout);
    return 1;
}

int main(int argc, char *argv[])
{
    extern FILE *yyin, *yyout;
    FILE *fp;
    char inp1[50];
    char inp2[50];
    char inp3[50];

    sprintf(inp1, "../T1/%s", argv[1]);
    sprintf(inp2, "../TC/%s", argv[2]);
    sprintf(inp3, "../TK/%s", argv[3]);

    yyin = fopen(inp1, "r");
    fp = fopen(inp2, "w");
    yyout = fopen(inp3, "w");

    char *opsrc[6] = {"=", "+", "/", "-", "*", "%"};
    char *opdest[6] = {"=", "+", "/", "-", "*", "%"};

    char *bracksrc[6] = {"(", ")", "{", "}", "[", "]"};
    char *brackdes[6] = {"(", ")", "{", "}", "{", "}"};

    char *datatypesrc[4] = {"integer_2", "character_1", "string", "null"};
    char *datatypedes[4] = {"int", "char", "char*", "void"};

    char *srcReserved[13] = {"in case that", "otherwise", "return", "do", "jump to", "and", "or", "gt", "gteq", "lt", "lteq", "eq", "neq"};
    char *desReserved[13] = {"if", "else", "return", "", "goto", "&&", "||", ">", ">=", "<", "<=", "==", "!="};

    int ntoken = yylex();

    while(ntoken)
    {
        switch(ntoken)
        {
            case ERROR:
                {
                    fprintf(yyout, "Invalid Identifier at pp%d\n", yylineno);
                    fprintf(fp, "Error occured, Aborting Translation\n");
                    return 1;
                }
            case LABEL:
                {
                    fprintf(yyout, "label: %s\n", yytext);
                    if(is_goto == 1)
                    {
                        fprintf(fp, "%s", yytext);
                    }

                    is_goto = 0;
                    break;
                }
            case ROOT:
                {
                    char *str;
                    str = malloc(yyleng);
                    strcpy(str, yytext);
                    char id[50]; char num[50]; char opt[2];

                    sscanf(str, "%s %s %s", id, opt, num);
                    fprintf(fp, "pow(%s, 1./%s)", id, num);
                    fprintf(yyout, "ID: %s\n", id);
                    fprintf(yyout, "operator: _\n");
                    fprintf(yyout, "constant: %s\n", num);

                    break;
                }

            case PUNCTUATION:
                {
                    fprintf(yyout, "punctuation: %s\n", yytext);

                    if(strcmp(yytext, ":") != 0)
                    {
                        fprintf(fp, "%s", yytext);
                    }

                    break;
                }

            case SPLSYM:
                {
                    fprintf(yyout, "special symbol: %s\n", yytext);

                    if(if_bracket == 1)
                    {
                        if(strcmp(yytext, "[") == 0)
                        {
                            fprintf(fp, "(");
                        }

                        else if(strcmp(yytext, "]") == 0)
                        {
                            fprintf(fp, ")");
                            if_bracket = 0;
                        }
                    }

                    else
                    {
                        for(int i = 0; i < 6; i++)
                        {
                            if(strcmp(yytext, bracksrc[i]) == 0)
                            {
                                fprintf(fp, "%s" ,brackdes[i]);
                            }
                        }
                    }

                    break;
                }

            case DATATYPE:
                {
                    fprintf(yyout, "datatype: %s\n", yytext);
                    for(int i = 0; i < 4; i++)
                    {
                        if(strcmp(yytext, datatypesrc[i]) == 0)
                        {
                            fprintf(fp, "%s",datatypedes[i]);
                        }
                    }

                    break;
                }

            case OPERATOR:
                {
                    fprintf(yyout, "operator: %s\n", yytext);

                    for(int i = 0; i < 6; i++)
                    {
                        if(strcmp(opsrc[i], yytext) == 0)
                        {
                            fprintf(fp, "%s", opdest[i]);
                        }
                    }

                    break;
                }

            case IDENTIFIER:
                {
                    fprintf(yyout, "ID: %s\n", yytext);
                    fprintf(fp, "%s", yytext);  

                    break;
                }

            case RESERVED:
                {
                    fprintf(yyout, "reserved keyword: %s\n", yytext);

                    for(int i = 0; i < 13; i++)
                    {
                        if(strcmp(srcReserved[i], yytext) == 0)
                        {
                            fprintf(fp, "%s", desReserved[i]);
                        }
                    }

                    if(strcmp(yytext, "in case that") == 0)
                    {
                        if_bracket = 1;
                    }

                    if(strcmp(yytext, "jump to") == 0)
                    {
                        is_goto = 1;
                    }

                    break;
                }

            case INTCONST:
                {
                    fprintf(yyout, "constant: %s\n", yytext);
                    fprintf(fp, "%s", yytext);
                    break;
                }

            case CHARCONST:
                {
                    fprintf(fp, "%s", yytext);
                    char *str = malloc(yyleng -1);
                    memcpy(str, yytext + 1, yyleng - 2);
                    str[yyleng - 2] = 0;
                    fprintf(yyout, "punctuation: \'\n");
                    fprintf(yyout, "constant: %s\n", str);
                    fprintf(yyout, "punctuation: \'\n");

                    break;
                }

            case STRINGCONST:
                {
                    fprintf(fp, "%s", yytext);
                    char *str = malloc(yyleng -1);
                    memcpy(str, yytext + 1, yyleng - 2);
                    str[yyleng - 2] = 0;
                    fprintf(yyout, "punctuation: \"\n");
                    fprintf(yyout, "constant: %s\n", str);
                    fprintf(yyout, "punctuation: \"\n");

                    break;
                }

            case BLANK:
                {
                    if(strcmp(yytext, "\n") == 0)
                    {
                        yylineno++;
                    }
                    fprintf(fp, "%s", yytext);
                    break;
                }

            default:
                {
                    fprintf(yyout, "Error at pp%d\n", yylineno); return 1;
                }
        }

        ntoken = yylex();
    }

    fclose(fp);
    return 0;
}
