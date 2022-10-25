%define parse.error verbose
%locations
%{
#include "stdio.h"
#include "math.h"
#include "string.h"
#include "def.h"
extern int yylineno;
extern char *yytext;
extern FILE *yyin;
extern void yyerror(const char* fmt, ...);
extern void display(struct node*,int);
extern void dfsAST(struct node*);
extern int yylex();
extern char filename[50];

%}

%union {
  int    type_int;
  float  type_float;
  char   type_id[32];
  struct node *ptr;
};

//  %type 定义非终结符的语义值类型
%type  <ptr> program ExtDefList ExtDef Specifier VoidType ExtDecList FuncDec CompSt VarList VarDec ParamDec Stmt StmDefList Def DecList Exp Args ForArgs ForArg Term Arrays NumList

//% token 定义终结符的语义值类型
%token <type_int> INT           // 指定INT字面量的语义值是type_int，有词法分析得到的数值
%token <type_id> ID RELOP TYPE VOID   // 指定ID,RELOP的语义值是type_id，有词法分析得到的标识符字符串, RELOP relation operator
%token <type_float> FLOAT       // 指定FLOAT字面量的语义值是type_float，有词法分析得到的数值

%token CONST RETURN IF ELSE FOR WHILE DO BREAK CONTINUE
%token LP RP LB RB LC RC COMMA SEMICOLON QUESTION COLON
//用bison对该文件编译时，带参数-d，生成的exp.tab.h中给这些单词进行编码，可在lex.l中包含parser.tab.h使用这些单词种类码
%token NOT ASSIGN MINUS ADD MUL DIV MOD AND OR UMINUS SELF_ADD SELF_MINUS

%left ASSIGN
%left OR AND
%left RELOP
%left ADD MINUS MOD
%left MUL DIV
%right NOT

%nonassoc LOWER_THEN_ELSE
%nonassoc ELSE

%%

program: ExtDefList  { dfsAST($1); }
         ;

ExtDefList: {$$=NULL;}
          | ExtDef ExtDefList  {$$=mknode(EXT_DEF_LIST,$1,$2,NULL,yylineno);}  //每一个EXTDEFLIST的结点，其第1棵子树对应一个外部变量声明或函数
          ;

ExtDef: Specifier ExtDecList SEMICOLON  {$$=mknode(EXT_VAR_DEF,$1,$2,NULL,yylineno);}  //该结点对应一个外部变量声明
      | CONST Specifier ExtDecList SEMICOLON  {$$=mknode(EXT_CONST_VAR_DEF,$2,$3,NULL,yylineno);}  //该结点对应一个const外部变量声明
      | Specifier FuncDec CompSt  {$$=mknode(FUNC_DEF,$1,$2,$3,yylineno);}  //该结点对应一个函数定义
      | VoidType FuncDec CompSt  {$$=mknode(FUNC_DEF,$1,$2,$3,yylineno);}  //该结点对应一个函数定义
      | error SEMICOLON  {$$=NULL; }
      ;

Specifier: TYPE  {$$=mknode(TYPE,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);$$->type=!strcmp($1,"int")?INT:FLOAT;}
         ; 

VoidType: VOID {$$=mknode(VOID,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);$$->type=VOID;}  

ExtDecList: VarDec  {$$=mknode(EXT_DEC_LIST,$1,NULL,NULL,yylineno);}
          | VarDec COMMA ExtDecList  {$$=mknode(EXT_DEC_LIST,$1,$3,NULL,yylineno);}
          ;

Term:   ID {$$=mknode(NODE_ID,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);}
	| ID Arrays {$$=mknode(ARRAYS,$2,NULL,NULL,yylineno);strcpy($$->type_id,$1);strcpy($$->arraytype,$2->arraytype);$$->arraynum=$2->arraynum;}
    	;

Arrays: LB Exp RB {$$=mknode(ARRAYS_SUBSCRIPT,$2,NULL,NULL,yylineno); if($2->kind==INT){sprintf($$->arraytype,"[%d]",$2->type_int);} $$->arraynum=1;}
      | LB Exp RB Arrays {$$=mknode(ARRAYS_SUBSCRIPT,$2,$4,NULL,yylineno); if($2->kind==INT){ sprintf($$->arraytype,"[%d]",$2->type_int); strcat($$->arraytype,$4->arraytype);} $$->arraynum=$4->arraynum+1;}
      ;

VarDec: Term {$$=$1;}
      | ID ASSIGN Exp  {$$=mknode(VAR_DEC,$3,NULL,NULL,yylineno);strcpy($$->type_id,$1);}
	| ID Arrays ASSIGN LC NumList RC {$$=mknode(ARRAYS_DEC,$2,$5,NULL,yylineno);strcpy($$->type_id,$1);strcpy($$->arraytype,$2->arraytype);$$->arraynum=$2->arraynum;}
      ;

NumList:  Exp {$$=mknode(NUM_LIST,$1,NULL,NULL,yylineno);}
	| Exp COMMA NumList {$$=mknode(NUM_LIST,$1,$3,NULL,yylineno);}
	;

FuncDec: ID LP VarList RP  {$$=mknode(FUNC_DEC,$3,NULL,NULL,yylineno);strcpy($$->type_id,$1);$$->paramnum=$3->paramnum;strcpy($$->paramtype,$3->paramtype);}  //函数名存放在$$->type_id
       | ID LP RP  {$$=mknode(FUNC_DEC,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);$$->paramnum=0;strcpy($$->paramtype,"void");}  //函数名存放在$$->type_id
       ;

VarList:  ParamDec  {$$=mknode(PARAM_LIST,$1,NULL,NULL,yylineno);$$->paramnum=1;strcpy($$->paramtype,$1->paramtype);}
       | ParamDec COMMA VarList  {$$=mknode(PARAM_LIST,$1,$3,NULL,yylineno);$$->paramnum=$3->paramnum+1;strcpy($$->paramtype,$1->paramtype);strcat($$->paramtype,",");strcat($$->paramtype,$3->paramtype);}
       ;

ParamDec: Specifier VarDec  {$$=mknode(PARAM_DEC,$1,$2,NULL,yylineno);strcpy($$->paramtype,$1->type_id);}
        ;

CompSt: LC StmDefList RC  {$$=mknode(COMP_STM,$2,NULL,NULL,yylineno);}
      ;

StmDefList: {$$=NULL; }  
          | Stmt StmDefList  {$$=mknode(STM_DEF_LIST,$1,$2,NULL,yylineno);}
          | Def StmDefList  {$$=mknode(STM_DEF_LIST,$1,$2,NULL,yylineno);}
          ;

Stmt: Exp SEMICOLON          {$$=mknode(EXP_STMT,$1,NULL,NULL,yylineno);}
    | CompSt                 {$$=$1;}      //复合语句结点直接最为语句结点，不再生成新的结点
    | RETURN Exp SEMICOLON   {$$=mknode(RETURN,$2,NULL,NULL,yylineno);}
    | RETURN SEMICOLON       {$$=mknode(RETURN,NULL,NULL,NULL,yylineno);}
    | IF LP Exp RP Stmt %prec LOWER_THEN_ELSE   {$$=mknode(IF_THEN,$3,$5,NULL,yylineno);}
    | IF LP Exp RP Stmt ELSE Stmt   {$$=mknode(IF_THEN_ELSE,$3,$5,$7,yylineno);}
    | WHILE LP Exp RP Stmt   {$$=mknode(WHILE,$3,$5,NULL,yylineno);}
    | CONTINUE SEMICOLON     {$$=mknode(CONTINUE_STMT,NULL,NULL,NULL,yylineno);}
    | BREAK SEMICOLON        {$$=mknode(BREAK_STMT,NULL,NULL,NULL,yylineno);}
    | FOR ForArgs Stmt       {$$=mknode(FOR_STMT,$2,$3,NULL,yylineno);}
    ;

ForArgs: LP ForArg SEMICOLON ForArg SEMICOLON ForArg RP  {$$=mknode(FOR_ARGS,$2,$4,$6,yylineno);}
       ;

ForArg: {$$=NULL;}  
      | Exp  {$$=$1;}
      ;

Def: Specifier DecList SEMICOLON {$$=mknode(VAR_DEF,$1,$2,NULL,yylineno);}
   ;
   
DecList: VarDec  {$$=mknode(DEC_LIST,$1,NULL,NULL,yylineno);}
       | VarDec COMMA DecList  {$$=mknode(DEC_LIST,$1,$3,NULL,yylineno);}
       ;

Exp: Exp ASSIGN Exp  {$$=mknode(ASSIGN,$1,$3,NULL,yylineno);strcpy($$->type_id,"=");}//$$结点type_id空置未用，正好存放运算符
   | Exp AND Exp     {$$=mknode(AND,$1,$3,NULL,yylineno);strcpy($$->type_id,"&&");}
   | Exp OR Exp      {$$=mknode(OR,$1,$3,NULL,yylineno);strcpy($$->type_id,"||");}
   | Exp RELOP Exp   {$$=mknode(RELOP,$1,$3,NULL,yylineno);strcpy($$->type_id,$2);}  //词法分析关系运算符号自身值保存在$2中
   | Exp ADD Exp     {$$=mknode(ADD,$1,$3,NULL,yylineno);strcpy($$->type_id,"+");}
   | Exp MINUS Exp   {$$=mknode(MINUS,$1,$3,NULL,yylineno);strcpy($$->type_id,"-");}
   | Exp MUL Exp     {$$=mknode(MUL,$1,$3,NULL,yylineno);strcpy($$->type_id,"*");}
   | Exp DIV Exp     {$$=mknode(DIV,$1,$3,NULL,yylineno);strcpy($$->type_id,"/");}
   | Exp MOD Exp     {$$=mknode(MOD,$1,$3,NULL,yylineno);strcpy($$->type_id,"%");}
   | Term SELF_ADD   {$$=mknode(SELF_ADD_EXP,$1,NULL,NULL,yylineno);}
   | Term SELF_MINUS {$$=mknode(SELF_MINUS_EXP,$1,NULL,NULL,yylineno);}
   | LP Exp RP       {$$=$2;}
   | MINUS Exp %prec UMINUS   {$$=mknode(UMINUS,$2,NULL,NULL,yylineno);strcpy($$->type_id,"UMINUS");}
   | NOT Exp         {$$=mknode(NOT,$2,NULL,NULL,yylineno);strcpy($$->type_id,"!");}
   | ID LP Args RP   {$$=mknode(FUNC_CALL,$3,NULL,NULL,yylineno);strcpy($$->type_id,$1);$$->paramnum=$3->paramnum;}
   | ID LP RP        {$$=mknode(FUNC_CALL,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);$$->paramnum=0;}
   | ID              {$$=mknode(EXP_ID,NULL,NULL,NULL,yylineno);strcpy($$->type_id,$1);}
   | ID Arrays       {$$=mknode(EXP_ARRAYS,$2,NULL,NULL,yylineno);strcpy($$->type_id,$1);strcpy($$->arraytype,$2->arraytype);$$->arraynum=$2->arraynum;}
   | INT             {$$=mknode(INT,NULL,NULL,NULL,yylineno);$$->type_int=$1;$$->type=INT;}
   | FLOAT           {$$=mknode(FLOAT,NULL,NULL,NULL,yylineno);$$->type_float=$1;$$->type=FLOAT;}
   ;

Args: Exp COMMA Args  {$$=mknode(ARGS,$1,$3,NULL,yylineno); $$->paramnum = $3->paramnum+1;}
    | Exp             {$$=mknode(ARGS,$1,NULL,NULL,yylineno);$$->paramnum = 1;}
    ;
       
%%

int main(int argc, char *argv[]) {
  yyin = fopen(argv[1],"r");
  if (!yyin) return 0;
  //char ch;
  //while((ch=getc(yyin))!=EOF) putchar(ch);
  yylineno = 1;
  strcpy(filename,strrchr(argv[1],'/')+1);
  yyparse();
  return 0;
}

#include<stdarg.h>

void yyerror(const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    fprintf(stderr, "%s:%d:%d ", filename,yylloc.first_line,yylloc.first_column);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, ".\n");
}	