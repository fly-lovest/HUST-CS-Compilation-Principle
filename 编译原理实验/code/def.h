#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "stdarg.h"
#include "sysy.tab.h"

enum node_kind
{
  EXT_DEF_LIST,
  EXT_VAR_DEF,
  EXT_CONST_VAR_DEF,
  FUNC_DEF,
  FUNC_DEC,
  EXT_DEC_LIST,
  PARAM_LIST,
  PARAM_DEC,
  VAR_DEF,
  VAR_DEC,
  DEC_LIST,
  COMP_STM,
  STM_DEF_LIST,
  EXP_STMT,
  IF_THEN,
  IF_THEN_ELSE,
  FUNC_CALL,
  ARGS,
  CONTINUE_STMT,
  BREAK_STMT,
  FOR_STMT,
  FOR_ARGS,
  FUNCTION,
  PARAM,
  ARG,
  CALL,
  LABEL,
  GOTO,
  JLT,
  JLE,
  JGT,
  JGE,
  EQ,
  NEQ,
  NODE_ID,
  SELF_ADD_EXP,
  SELF_MINUS_EXP,
  ARRAYS,
  ARRAYS_DEC,
  NUM_LIST,
  ARRAYS_SUBSCRIPT,
  EXP_ID,
  EXP_ARRAYS
};

enum error_list{
OK,UNDECLARED,CONFLICT,REDEFINITION,REDECLARED,NOT_FUNCTION,INVALID_OP,LVALUE_REQUIRED,
ARRAY_ASSIGNMENT,FEW_ARGUMENTS,MANY_ARGUMENTS,RETURN_VOID,ARRAY_SUBSCRIPT,VALUE_NOT_ARRAYS,
BREAK_WITHOUT_LOOP,CONTINUE_WITHOUT_LOOP,WITHOUT_RETURN_VALUE
};

#define MAXLENGTH 1000     //定义符号表的大小
#define DX 3 * sizeof(int) //活动记录控制信息需要的单元数
char filename[50];
int lev;

//以下语法树结点类型、三地址结点类型等定义仅供参考，实验时一定要根据自己的理解来定义
struct opn
{
  int kind; //标识操作的类型
  int type; //标识操作数的类型
  union
  {
    int const_int;     //整常数值，立即数
    float const_float; //浮点常数值，立即数
    char const_char;   //字符常数值，立即数
    char id[33];       //变量或临时变量的别名或标号字符串
  };
  int level;  //变量的层号，0表示是全局变量，数据保存在静态数据区
  int offset; //变量单元偏移量，或函数在符号表的定义位置序号，目标代码生成时用
};

struct codenode
{                                //三地址TAC代码结点,采用双向循环链表存放中间语言代码
  int op;                        //TAC代码的运算符种类
  struct opn opn1, opn2, result; //2个操作数和运算结果
  struct codenode *next, *prior;
};

struct node
{                      //以下对结点属性定义没有考虑存储效率，只是简单地列出要用到的一些属性
  enum node_kind kind; //结点类型
  union
  {
    char type_id[33]; //由标识符生成的叶结点
    int type_int;     //由整常数生成的叶结点
    float type_float; //由浮点常数生成的叶结点
  };
  struct node *ptr[3];        //子树指针，由kind确定有多少棵子树
  int level;                  //层号
  int place;                  //表示结点对应的变量或运算结果临时变量在符号表的位置序号
  char Etrue[15], Efalse[15]; //对布尔表达式的翻译时，真假转移目标的标号
  char Snext[15];             //该结点对应语句执行后的下一条语句位置标号
  struct codenode *code;      //该结点中间代码链表头指针
  char op[10];
  int type;   //结点对应值的类型
  int pos;    //语法单位所在位置行号
  int offset; //偏移量
  int width;  //各种数据占用的字节数

  int paramnum; //函数结点的形参个数
  char paramtype[20];  //记录形参类型（形参类型，...）
  int arraynum;  //记录数组维度
  char arraytype[20];  //记录数组大小
  char flag;  //记录变量种类，为‘V’或‘P’
  char printmore; //记录是否打印更多
  int breakflag;  //用于语义break分析
  int continueflag; //用于语义continue分析
  int returnflag;  //用于语义return分析
  int LorR;    //用于在等式判断时区分左右值，0为L，1为R
};

struct symbol
{                 //这里只列出了一个符号表项的部分属性，没考虑属性间的互斥
  char name[33];  //变量或函数名
  int level;      //层号，外部变量名或函数名层号为0，形参名为1，每到1个复合语句层号加1，退出减1
  int type;       //变量类型或函数返回值类型
  int paramnum;   //形式参数个数
  int arraynum;   //数组维度
  char alias[10]; //别名，为解决嵌套层次使用，使得每一个数据名称唯一
  char flag;      //符号标记，函数：'F'  变量：'V'   参数：'P'  临时变量：'T' 数组：‘A’
  char offset;    //外部变量和局部变量在其静态数据区或活动记录中的偏移量
                  //或函数活动记录大小，目标代码生成时使用
  char Ftype[20]; //函数类型，返回类型（参数类型，...）
  char Atype[20]; //数组类型，数组维度信息（[.][.]...）
} ;

//符号表，是一个顺序栈，index初值为0
struct symboltable
{
  struct symbol symbols[MAXLENGTH];
  int index;
} symbolTable;

struct symbol_scope_begin
{ /*当前作用域的符号在符号表的起始位置序号,这是一个栈结构，/每到达一个复合语句，将符号表的index值进栈，离开复合语句时，取其退栈值修改符号表的index值，完成删除该复合语句中的所有变量和临时变量*/
  int TX[30];
  int top;
} symbol_scope_TX;

// 为语义分析和代码生成预留
struct node *mknode(int kind,struct node *first,struct node *second, struct node *third,int pos );
void dfsAST(struct node *T);
void printError(int lineno,enum error_list error,char *errorident);
void DisplaySymbolTable();
void InitSymbolTable();
void InsertSymbolTable(char *name,int type,int level,char flag,int num,char *moretype);
int SearchSymbolTable(char *name,int type,int level,char flag,int num,char *moretype,int choice,int LorR);
void enterScope();
void exitScope();
//int Semantic_Analysis(struct node* T,int type,int level,char flag,int command);
//void boolExp(struct node *T);
//void Exp(struct node *T);
//void objectCode(struct codenode *head);
