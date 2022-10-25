#include "def.h"
char lastname[20] = "";  //为避免重复产生同一行的错误
int lasterror = -1;
int lastline = -1;

struct node * mknode(int kind,struct node *first,struct node *second, struct node *third,int pos ) {
  struct node *T=(struct node *)malloc(sizeof(struct node));
  T->kind=kind;
  T->ptr[0]=first;
  T->ptr[1]=second;
  T->ptr[2]=third;
  T->pos=pos;
  return T;
}

void dfsAST(struct node *T)  {  //对抽象语法树的先根遍历
	int i = 1,j = 1,k = 1;
	struct node *T0;
	char functparam[30];
	char arraydem[30];
	int res;
	if (T) {
	switch (T->kind) {
	case EXT_DEF_LIST:
		dfsAST(T->ptr[0]);
		dfsAST(T->ptr[1]);
		break;
	case TYPE:
		if(!strcmp(T->type_id,"int")){
			T->type = INT;
		}
		else{
			T->type = FLOAT;
		}
		break;
	case VOID:
		T->type = VOID;
		break;
	case EXT_VAR_DEF:
		dfsAST(T->ptr[0]);
		T->ptr[1]->type = T->ptr[0]->type;
		dfsAST(T->ptr[1]);
		break;
	case EXT_CONST_VAR_DEF:
		dfsAST(T->ptr[0]);
		T->ptr[1]->type = T->ptr[0]->type;
		dfsAST(T->ptr[1]);
		break;
	case EXT_DEC_LIST:
		T->ptr[0]->type = T->type;
		T->ptr[0]->flag = 'E';
		dfsAST(T->ptr[0]);
		if(T->ptr[1]){
			T->ptr[1]->type = T->type;
			dfsAST(T->ptr[1]);
		}
		break;
	case NODE_ID:
		if(T->flag == 'E'){
			res = SearchSymbolTable(T->type_id,T->type,lev,'V',0,"",0,-1);
			if(res == OK){
				InsertSymbolTable(T->type_id,T->type,lev,'V',0,"");
			}
			else{
				printError(T->pos,res,T->type_id);
				InsertSymbolTable(T->type_id,T->type,lev,'V',0,"");
			}
		}
		else{
			InsertSymbolTable(T->type_id,T->type,lev,'P',0,"");
		}
		//DisplaySymbolTable();
		break;
	case ARRAYS:
		if(T->type == INT){
			sprintf(arraydem,"int%s",T->arraytype);
		}
		else{
			sprintf(arraydem,"float%s",T->arraytype);
		}
		InsertSymbolTable(T->type_id,T->type,lev,'A',T->arraynum,arraydem);
		//DisplaySymbolTable();
		break;
	case ARRAYS_SUBSCRIPT:
		break;
	case VAR_DEC:
		res = SearchSymbolTable(T->type_id,T->type,lev,'V',0,"",0,-1);
		if(res == OK){
			InsertSymbolTable(T->type_id,T->type,lev,'V',0,"");
		}
		else{
			printError(T->pos,res,T->type_id);
			InsertSymbolTable(T->type_id,T->type,lev,'V',0,"");
		}
		//DisplaySymbolTable();
		dfsAST(T->ptr[0]);
		break;
	case ARRAYS_DEC:
		if(T->type == INT){
			sprintf(arraydem,"int%s",T->arraytype);
		}
		else{
			sprintf(arraydem,"float%s",T->arraytype);
		}
		InsertSymbolTable(T->type_id,T->type,lev,'A',T->arraynum,arraydem);
		//DisplaySymbolTable();
		break;
	case NUM_LIST:
		break;
	case FUNC_DEF:
		dfsAST(T->ptr[0]);
		T->ptr[1]->type = T->ptr[0]->type;
		T->ptr[2]->breakflag = 0;
		T->ptr[2]->continueflag = 0;
		if(T->ptr[0]->type == VOID)
			T->ptr[2]->returnflag = 0;
		else  T->ptr[2]->returnflag = 1;
		dfsAST(T->ptr[1]);
		dfsAST(T->ptr[2]);
		exitScope();
		//DisplaySymbolTable();
		break;
	case FUNC_DEC:
		if(T->type == INT){
			sprintf(functparam,"int(%s)",T->paramtype);
		}
		else if(T->type == FLOAT){
			sprintf(functparam,"float(%s)",T->paramtype);
		}
		else{
			sprintf(functparam,"void(%s)",T->paramtype);
		}
		res = SearchSymbolTable(T->type_id,T->type,lev,'F',T->paramnum,functparam,0,-1);
		if(res == OK){
			InsertSymbolTable(T->type_id,T->type,lev,'F',T->paramnum,functparam);
		}
		else{
			printError(T->pos,res,T->type_id);
			InsertSymbolTable(T->type_id,T->type,lev,'F',T->paramnum,functparam);
		}
		//DisplaySymbolTable();
		enterScope();
		if(T->ptr[0])
			dfsAST(T->ptr[0]);
		break;
	case PARAM_LIST:
		dfsAST(T->ptr[0]);
		dfsAST(T->ptr[1]);
		break;
	case PARAM_DEC:
		dfsAST(T->ptr[0]);
		T->ptr[1]->type = T->ptr[0]->type;
		T->ptr[1]->flag = 'P';
		dfsAST(T->ptr[1]);
		break;
	case COMP_STM:
		if(T->ptr[0]){
			T->ptr[0]->returnflag = T->returnflag;
			T->ptr[0]->breakflag = T->breakflag;
			T->ptr[0]->continueflag = T->continueflag;
			dfsAST(T->ptr[0]);
		}
		break;
	case STM_DEF_LIST:
		T->ptr[0]->returnflag = T->returnflag;
		T->ptr[0]->continueflag = T->continueflag;
		T->ptr[0]->breakflag = T->breakflag;
		if(T->ptr[1]){
			T->ptr[1]->returnflag = T->returnflag;
			T->ptr[1]->continueflag = T->continueflag;
			T->ptr[1]->breakflag = T->breakflag;
		}
		dfsAST(T->ptr[0]);
		dfsAST(T->ptr[1]);
		break;
	case EXP_STMT:
		dfsAST(T->ptr[0]);
		break;
	case RETURN:
		if(T->ptr[0]){
			if(T->returnflag == 0) printError(T->pos,RETURN_VOID,"return");
		}
		else{
			if(T->returnflag == 1) printError(T->pos,WITHOUT_RETURN_VALUE,"return");
		}
		break;
	case CONTINUE_STMT:
		if(T->continueflag == 0) printError(T->pos,CONTINUE_WITHOUT_LOOP,"continue");
		break;
	case BREAK_STMT:
		if(T->breakflag == 0) printError(T->pos,BREAK_WITHOUT_LOOP,"break");
		break;
	case WHILE:
		enterScope();
		T->ptr[1]->continueflag = 1;
		T->ptr[1]->breakflag = 1;
		T->ptr[1]->returnflag = T->returnflag;
		dfsAST(T->ptr[0]);
		dfsAST(T->ptr[1]);
		exitScope();
		//DisplaySymbolTable();
		break;
	case FOR_STMT:
		enterScope();
		T->ptr[1]->breakflag = 1;
		T->ptr[1]->continueflag = 1;
		T->ptr[1]->returnflag = T->returnflag;
		dfsAST(T->ptr[0]);
		dfsAST(T->ptr[1]);
		exitScope();
		//DisplaySymbolTable();
		break;
	case FOR_ARGS:
		dfsAST(T->ptr[0]);
		dfsAST(T->ptr[1]);
		dfsAST(T->ptr[2]);
		break;
	case IF_THEN:
		enterScope();
		T->ptr[1]->continueflag = T->continueflag;
		T->ptr[1]->breakflag = T->breakflag;
		T->ptr[1]->returnflag = T->returnflag;
		dfsAST(T->ptr[0]);
		dfsAST(T->ptr[1]);
		exitScope();
		//DisplaySymbolTable();
		break;
	case IF_THEN_ELSE:
		enterScope();
		T->ptr[1]->continueflag = T->continueflag;
		T->ptr[1]->breakflag = T->breakflag;
		T->ptr[1]->returnflag = T->returnflag;
		T->ptr[2]->continueflag = T->continueflag;
		T->ptr[2]->breakflag = T->breakflag;
		T->ptr[2]->returnflag = T->returnflag;
		dfsAST(T->ptr[0]);
		dfsAST(T->ptr[1]);
		dfsAST(T->ptr[2]);
		exitScope();
		//DisplaySymbolTable();
		break;
	case VAR_DEF: //局部变量
		dfsAST(T->ptr[0]);
		T->ptr[1]->type = T->ptr[0]->type;
		dfsAST(T->ptr[1]);
		//DisplaySymbolTable();
		break;
	case DEC_LIST:
		T->ptr[0]->flag = 'E';
		T->ptr[0]->type = T->type;
		dfsAST(T->ptr[0]);
		if(T->ptr[1]){
			T->ptr[1]->type = T->type;
			dfsAST(T->ptr[1]);
		}
		break;
	case INT:
	   	break;
	case FLOAT:
		break;
	case ASSIGN:
		T->ptr[0]->LorR = 0;
		T->ptr[1]->LorR = 1;
		dfsAST(T->ptr[0]);
		dfsAST(T->ptr[1]);
		break;
	case AND:
	case OR:
	case RELOP:
	case ADD:
	case MINUS:
	case MUL:
	case DIV:
	case MOD:
		T->ptr[0]->LorR = T->LorR;
		T->ptr[1]->LorR = T->LorR;
		if(T->ptr[0]->ptr[0])
			dfsAST(T->ptr[0]);
		else{
			strcpy(T->ptr[0]->op,T->type_id);
			dfsAST(T->ptr[0]);
		}
		if(T->ptr[1]->ptr[0])
			dfsAST(T->ptr[1]);
		else{
			strcpy(T->ptr[1]->op,T->type_id);
			dfsAST(T->ptr[1]);
		}
		break;
	case SELF_ADD_EXP:
	case SELF_MINUS_EXP:
		break;
	case NOT:
	case UMINUS:
		T->ptr[0]->LorR = T->LorR;
		dfsAST(T->ptr[0]);
		break;
	case FUNC_CALL:
		if(!strcmp(T->type_id,"putint")) break;
		res = SearchSymbolTable(T->type_id,0,lev,'F',T->paramnum,"",1,T->LorR);
		if(res != OK) printError(T->pos,res,T->type_id);
		break;
	case ARGS:
		break;
	case EXP_ID:
		res = SearchSymbolTable(T->type_id,0,lev,'V',0,"",1,T->LorR);
		if(res != OK){
			if(lastline!=T->pos || strcmp(T->type_id,lastname) || lasterror != res){
				if(res == INVALID_OP){
					printError(T->pos,res,T->op);
					strcpy(lastname,T->type_id);
					lastline = T->pos;
					lasterror = res;
				}
				else{
					printError(T->pos,res,T->type_id);
					strcpy(lastname,T->type_id);
					lastline = T->pos;
					lasterror = res;
				}
			}
		}
		break;
	case EXP_ARRAYS:
		res = SearchSymbolTable(T->type_id,0,lev,'A',0,"",1,T->LorR);
		if(res!=OK){
			printError(T->pos,res,T->type_id);
			break;
		}
		j = 0;
		for(i=0;i<T->arraynum;i++){
			k = 0;
			while(T->arraytype[j]!=']'){
				if(T->arraytype[j]!='['){
					arraydem[k] = T->arraytype[j];
					k++;
					}
				j++;
			}
			arraydem[k] = '\0';
			j++;
			for(k=0;k<strlen(arraydem);k++){
				if(arraydem[k]<'0'||arraydem[k]>'9') break;
			}
			if(k!=strlen(arraydem))
				res = SearchSymbolTable(T->type_id,0,lev,'A',0,arraydem,1,T->LorR);
			if(res!=OK){
				printError(T->pos,res,T->type_id);
				break;
			}
		}
		break;
	default: break;
	}
	}
}


void printError(int lineno,enum error_list error,char *errorident){
	switch(error) {
		case UNDECLARED: printf("%s:%d ‘%s’ undeclared\n",filename,lineno,errorident); break;
		case CONFLICT: printf("%s:%d conflicting types for ‘%s’\n",filename,lineno,errorident); break;
		case REDEFINITION: printf("%s:%d redefinition of ‘%s’\n",filename,lineno,errorident); break;
		case REDECLARED: printf("%s:%d ‘%s’ redeclared as different kind of symbol\n",filename,lineno,errorident); break;
		case NOT_FUNCTION: printf("%s:%d ‘%s’ is not a function\n",filename,lineno,errorident); break;
		case INVALID_OP: printf("%s:%d invalid operands to binary operator %s\n",filename,lineno,errorident); break;
		case LVALUE_REQUIRED: printf("%s:%d lvalue required as left operand of assignment\n",filename,lineno); break;
		case ARRAY_ASSIGNMENT: printf("%s:%d assignment to expression with array type\n",filename,lineno); break;
		case FEW_ARGUMENTS: printf("%s:%d too few arguments to function ‘%s’\n",filename,lineno,errorident); break;
		case MANY_ARGUMENTS: printf("%s:%d too many arguments to function ‘%s’\n",filename,lineno,errorident); break;
		case RETURN_VOID: printf("%s:%d ‘return’ with a value, in function returning void\n",filename,lineno); break;
		case WITHOUT_RETURN_VALUE: printf("%s:%d ‘return’ without a value, in function returning value\n",filename,lineno); break;
		case ARRAY_SUBSCRIPT: printf("%s:%d array subscript is not an integer\n",filename,lineno); break;
		case VALUE_NOT_ARRAYS: printf("%s:%d subscripted value is not an array\n",filename,lineno); break;
		case BREAK_WITHOUT_LOOP: printf("%s:%d break statement not within a loop\n",filename,lineno); break;
		case CONTINUE_WITHOUT_LOOP: printf("%s:%d continue statement not within a loop\n",filename,lineno); break;
	}
}

void InitSymbolTable(){
	symbol_scope_TX.top = 0;
	symbolTable.index = 0;
}

void InsertSymbolTable(char *name,int type,int level,char flag,int num,char *moretype){
	int nowindex = symbolTable.index;
	strcpy(symbolTable.symbols[nowindex].name,name);
	symbolTable.symbols[nowindex].type = type;
	symbolTable.symbols[nowindex].level = level;
	symbolTable.symbols[nowindex].flag = flag;
	if(flag == 'F'){
		symbolTable.symbols[nowindex].paramnum = num;
		strcpy(symbolTable.symbols[nowindex].Ftype,moretype);
	}
	else if(flag == 'A'){
		symbolTable.symbols[nowindex].arraynum = num;
		strcpy(symbolTable.symbols[nowindex].Atype,moretype);
	}
	symbolTable.index++;
}

int SearchSymbolTable(char *name,int type,int level,char flag,int num,char *moretype,int choice,int LorR){
	int nowindex = symbolTable.index;
	int i;
	int lastindex = -1;
	if(choice == 0){
		for(i=0;i<nowindex;i++){
			if(symbolTable.symbols[i].level == level){
				if(symbolTable.symbols[i].flag == 'F' && flag=='F'){
					if(!(strcmp(symbolTable.symbols[i].name,name)) && !(strcmp(symbolTable.symbols[i].Ftype,moretype))){
						return REDEFINITION;
					}
				}
				else if(symbolTable.symbols[i].flag == 'V' && flag=='V'){
					if(!(strcmp(symbolTable.symbols[i].name,name)) && (symbolTable.symbols[i].type == type))
						return REDEFINITION;
				}
				else if(!(strcmp(symbolTable.symbols[i].name,name)) && symbolTable.symbols[i].flag != flag)
					return REDECLARED;
			}
		}
	}
	else if(choice == 1){
		for(i=0;i<nowindex;i++){
			if(!strcmp(symbolTable.symbols[i].name,name)){
				lastindex = i;
			}
		}
		if(lastindex == -1) return UNDECLARED;
		else{
			if(flag == 'V' && symbolTable.symbols[lastindex].flag == 'A') return ARRAY_ASSIGNMENT;
			if(flag == 'V' && symbolTable.symbols[lastindex].flag == 'F' && LorR == 0) return LVALUE_REQUIRED;
			if(flag == 'V' && symbolTable.symbols[lastindex].flag == 'F' && LorR == 1) return INVALID_OP;
			if(flag == 'F' && symbolTable.symbols[lastindex].flag != 'F') return NOT_FUNCTION;
			if(flag == 'F' && symbolTable.symbols[lastindex].flag == 'F' && symbolTable.symbols[lastindex].paramnum<num) return MANY_ARGUMENTS;
			if(flag == 'F' && symbolTable.symbols[lastindex].flag == 'F' && symbolTable.symbols[lastindex].paramnum>num) return FEW_ARGUMENTS;
			if(flag == 'A' && symbolTable.symbols[lastindex].flag != 'A') return VALUE_NOT_ARRAYS;
			if(flag == 'A' && symbolTable.symbols[lastindex].flag == 'A' && strcmp(moretype,"")){
				for(i=0;i<nowindex;i++){
					if(!strcmp(symbolTable.symbols[i].name,moretype)){
						lastindex = i;
					}
				}
				if(lastindex != nowindex)
					if(symbolTable.symbols[lastindex].type!=INT) return ARRAY_SUBSCRIPT;
			}
		}
	}
	return OK;
}

void enterScope(){
	lev++;
	symbol_scope_TX.top = lev;
	symbol_scope_TX.TX[lev] = symbolTable.index;
}

void exitScope(){
	symbolTable.index = symbol_scope_TX.TX[lev];
	lev--;
	symbol_scope_TX.top = lev;
}


void DisplaySymbolTable()
{
    int i;
    printf("----------------symbol table-----------------------\n");
    printf("%s\t%s\t%s\t%s\t%s\t%s\n","Index","Name","Level","Type","Flag","num");
    printf("---------------------------------------------------\n");
    for(i=0;i<symbolTable.index;i++){
	if(symbolTable.symbols[i].flag == 'F'){
		printf("%d\t%s\t%d\t%s\t%c\t%d\n",i,symbolTable.symbols[i].name,symbolTable.symbols[i].level,symbolTable.symbols[i].Ftype,symbolTable.symbols[i].flag,symbolTable.symbols[i].paramnum);
	}
	else if(symbolTable.symbols[i].flag == 'A'){
		printf("%d\t%s\t%d\t%s\t%c\t%d\n",i,symbolTable.symbols[i].name,symbolTable.symbols[i].level,symbolTable.symbols[i].Atype,symbolTable.symbols[i].flag,symbolTable.symbols[i].arraynum);
	}
	else{
		if(symbolTable.symbols[i].type == INT){
			printf("%d\t%s\t%d\tint\t%c\n",i,symbolTable.symbols[i].name,symbolTable.symbols[i].level,symbolTable.symbols[i].flag);
		}
		else{
			printf("%d\t%s\t%d\tfloat\t%c\n",i,symbolTable.symbols[i].name,symbolTable.symbols[i].level,symbolTable.symbols[i].flag);
		}
	}
     }
    printf("---------------------------------------------------\n");
}
