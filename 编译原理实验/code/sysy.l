%option noyywrap
%{
#include "string.h"
#include "sysy.tab.h"
#include "def.h"
int yycolumn=1;
#define YY_USER_ACTION    	yylloc.first_line=yylloc.last_line=yylineno; \
	yylloc.first_column=yycolumn;	yylloc.last_column=yycolumn+yyleng-1; yycolumn+=yyleng;
typedef union {
	int type_int;
	int type_float;
	char type_id[32];
	struct node *ptr;
} YYLVAL;
#define YYSTYPE YYLVAL

%}
%option yylineno

ID [a-z_A-Z][a-z_A-Z0-9]*
INT ([1-9][0-9]*|0[0-7]*|(0x|0X)[0-9a-fA-F]+)
EXP ([Ee][-+]?[0-9]+)
FLOAT (([0-9]*\.[0-9]+|[0-9]+\.){EXP}?[fF]?)|[0-9]+{EXP}[fF]?
MultilineComment "/*"([^\*]|(\*)*[^\*/])*(\*)*"*/"
SingleLineComment "//".*$
Invalid ([0-9]+[A-Za-z]+[0-9]*|0[0-9]*[8-9]+[0-9]*)

id   [A-Za-z][A-Za-z0-9]*  
int    [0-9]+
float  ([0-9]*\.[0-9]+)|([0-9]+\.)

%%
{INT}        {yylval.type_int=atoi(yytext); return INT;}
{FLOAT}      {yylval.type_float=atof(yytext); return FLOAT;}

"int"        {strcpy(yylval.type_id, yytext); return TYPE;}
"float"      {strcpy(yylval.type_id, yytext); return TYPE;}
"void"       {strcpy(yylval.type_id, yytext); return VOID;}
"const"      {return CONST;}
"return"     {return RETURN;}
"if"         {return IF;}
"else"       {return ELSE;}
"for"        {return FOR;}
"while"      {return WHILE;}
"do"         {return DO;}
"break"      {return BREAK;}
"continue"   {return CONTINUE;}

{ID}    {strcpy(yylval.type_id, yytext); return ID;/*由于关键字的形式也符合表示符的规则，所以把关键字的处理全部放在标识符的前面，优先识别*/}

">"|"<"|">="|"<="|"=="|"!=" {strcpy(yylval.type_id, yytext); return RELOP;}

"("			{return LP;}
")"			{return RP;}
"["			{return LB;}
"]"			{return RB;}
"{"			{return LC;}
"}"			{return RC;}
","			{return COMMA;}
";"			{return SEMICOLON;}
"?"			{return QUESTION;}
":"			{return COLON;}

"!"			{return NOT;}
"="			{return ASSIGN;}
"-"			{return MINUS;}
"+"			{return ADD;}
"*"			{return MUL;}
"/"			{return DIV;}
"%"			{return MOD;}
"&&"    {return AND;}
"||"    {return OR;}
"++"    {return SELF_ADD;}
"--"    {return SELF_MINUS;}

[\n]    {yycolumn=1;}
[ \r\t] {}
{SingleLineComment} {}
{MultilineComment} {}
.			{printf("Error type A :Mysterious character \"%s\"\n\t at Line %d\n",yytext,yylineno);}
%%



