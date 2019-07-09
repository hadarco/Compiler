%{
	#define _GNU_SOURCE
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
	#include "lex.yy.c"

	typedef enum {false,true } bool;

	typedef struct node
	{
		struct node *right;
		struct node *left;
		char *token;
		char *var;
		char *label;
		char *SCOPE;
		char *trueLab;
		char *falseLab;
		int cnt;
	} node;

    typedef struct Func 
	{
        char * name;
		struct Args* args;
        char *retType; 
		int argNum;
		int getRet;
    } Func;
	
	typedef struct Args
	{
		char* name;
		char* length;
		char* type;
	}Args;

	typedef struct Variables
	{	
		char*name;
		char*value;
		char*type;
		char* length;
		int isArg;
	}Variable;

	typedef struct SCOPE
	{	
		char *name;
		Func ** func;
		Variable * var;
		int varCount;
		int funcCount;
		struct SCOPE * nextScope;
		struct SCOPE * preScope;
	}SCOPE;

	int yylex();
	int yyerror(char *e);


	int mainFlag=false;
	static int scope=0;
	SCOPE* mkScope(char *);
	SCOPE* finScope(SCOPE * scopes);
	SCOPE* globalScope=NULL;
	void addFunc(char*, Args* , node*, int, SCOPE*);
	void addVar(Args*, int, int,SCOPE*);
	void pushScopes(SCOPE*, char*);
	void syntaxAnalyzer(node *tree,SCOPE * scope);
	char * getExpType(node *,SCOPE*);
	char* findFuncInScopes(node * tree,SCOPE * MYscope,int* count);
	char *findVar(node * tree,SCOPE * MYscope);
	node* mkNode(char*, node*, node*);
	Args * mkArguments(node *,int *);
	Args * callFuncArg(SCOPE *,node *tree,int*);

	static int t=0;
	static int lab=0;
	static int line=0;
	static node* startNode;
	int popParams(Args * args,int count);
	void make3AC(node*);
	void addScopeFirst(node*, char*, char*, char*,char*, char*);
	void addScopeSec(node*, char* ,char*, char*, char*,char*);
	char* freshVar();
	char* freshLabel();
	char* strCatFirst(char*des,char*src);
	char* Gen(char*,char*,char*,char*,char*);
	char* strCatSec(char*des,char*src);
	char * addSpaces(char*);
	char *replaceString(const char*, const char*, const char*);
	
%}

%union
{
    struct node *node;
    char *string;
}

%token <string> COMMENT WHILE IF ELSE FOR 
%token <string> RETURN
%token <string> BOOL STRING CHARPTR CHAR INT INTPTR PROCEDUR
%token <string> AND ADDRESS EQL ASSINGMENT OR LENGTH GREATEREQL GREATER LESSEQL LESS NOTEQL NOT
%token <string> DIVISION PLUS MINUS MULTI VARIABLE
%token <string> STRING_LTL REAL_LTL CHAR_LTL NULLL
%token <string> MAIN IDENTIFIER SEMICOLON COMMA OPENPAREN CLOSEPAREN OPENBRACKET CLOSEBRACKET OPENBRACE CLOSEBRACE
%token <string> DECIMAL_LTL HEX_LTL BOOLTRUE BOOLFALSE  REAL REALPTR FUNCTION COLON  DEREFRENCE 

%left IDENTIFIER OR AND 
%left  NOTEQL LESS LESSEQL GREATEREQL GREATER 
%left PLUS MINUS RETURN
%left MULTI DIVISION
%left SEMICOLON EQL
%right NOT CLOSEBRACE


%nonassoc OPENPAREN
%nonassoc IF
%nonassoc ELSE 


%type <node> address_expr stmnts stmnt_block derefrence_expr expr_list call_func 
%type <node> expr lhs assmnt_stmnt new_block 
%type <node> stmnt type_pro type_id var_id declear paren_expr
%type <node> pro_body para_list para_pro procedure procedures
%type <node> main program project declears 
%%
 
project: cmmnt program {startNode = $2; syntaxAnalyzer($2,globalScope);  make3AC($2);  }; 

program: procedures main{$$=mkNode("CODE",$1,$2);  }

cmmnt: COMMENT cmmnt {;}| ;

main: PROCEDUR MAIN OPENPAREN CLOSEPAREN cmmnt OPENBRACE pro_body CLOSEBRACE
{
$$=mkNode("Main",mkNode("ARGS",NULL,$7),NULL);
t=0;

};

procedures: procedures  procedure {$$=mkNode("procedures",$1,$2);}
	| {$$=NULL;};

procedure: FUNCTION IDENTIFIER OPENPAREN para_pro CLOSEPAREN cmmnt RETURN type_pro  OPENBRACE  pro_body CLOSEBRACE
{ 
		$$=mkNode("FUNC",mkNode($2,mkNode("",NULL,NULL),mkNode("ARGS",$4,mkNode("Return",$8,NULL))),mkNode("",$10,NULL));
		t=0; 
}
| PROCEDUR IDENTIFIER OPENPAREN para_pro CLOSEPAREN  OPENBRACE  pro_body CLOSEBRACE
{
	$$=mkNode("PROC",mkNode($2,mkNode("",NULL,NULL),NULL),mkNode("ARGS",$4,$7));
	t=0; 
};


para_pro: para_list {$$=$1;}
| {$$=NULL;};

para_list: var_id COLON type_id {$$=mkNode("(",$3,mkNode("",$1,mkNode(")",NULL,NULL)));}
	|  para_list SEMICOLON cmmnt  para_list {$$=mkNode("",$1,mkNode("",$4,NULL));}	;

pro_body: cmmnt  procedures declears stmnts 
{
	$$=mkNode("BODY", mkNode(" ",$2,NULL),mkNode(" ",$3,mkNode(" ",$4,mkNode(" ",NULL,NULL))));
	
};

declears: declears declear  {$$=mkNode("",$1,$2);} | {$$=NULL;}  ;

declear: VARIABLE var_id COLON type_id cmmnt SEMICOLON cmmnt
{
	$$=mkNode("var", $4,$2);
};

var_id: IDENTIFIER COMMA var_id {$$=mkNode($1, mkNode(" ", $3, NULL),NULL);}
	| IDENTIFIER {$$=mkNode($1, NULL, NULL);} ;

type_id: BOOL {$$=mkNode("boolean", NULL, NULL);}
	| STRING OPENBRACKET DECIMAL_LTL CLOSEBRACKET {$$=mkNode("string", mkNode("[",mkNode("$3",NULL,NULL),NULL), NULL);}
	| CHAR {$$=mkNode("char", NULL, NULL);}
	| INT {$$=mkNode("int", NULL, NULL);}
	| REAL {$$=mkNode("real", NULL, NULL);}
	| INTPTR {$$=mkNode("int*", NULL, NULL);}
	| CHARPTR {$$=mkNode("char*", NULL, NULL);}
	| REALPTR {$$=mkNode("real*", NULL, NULL);};

type_pro: BOOL {$$=mkNode("boolean", NULL, NULL);}
 	| STRING {$$=mkNode("string", NULL, NULL);}
	| CHAR {$$=mkNode("char", NULL, NULL);}
	| INT {$$=mkNode("int", NULL, NULL);}
	| REAL {$$=mkNode("real", NULL, NULL);}
	| INTPTR {$$=mkNode("int*", NULL, NULL);}
	| CHARPTR {$$=mkNode("char*", NULL, NULL);}
	| REALPTR {$$=mkNode("real*", NULL, NULL);};
	
stmnts: stmnts stmnt   { $$=mkNode("stmnts",$1,$2); if(strcmp($2->token, "if") == 0||strcmp($2->token, "for") == 0||strcmp($2->token, "if-else") == 0||strcmp($2->token, "while") == 0){ if($$->cnt==0) {addScopeFirst($2,NULL,NULL,freshLabel(),NULL,NULL); $$->cnt=1;}}   }| {$$=NULL;};

stmnt_block: stmnt {$$=$1; if(strcmp($1->token, "if") == 0||strcmp($1->token, "for") == 0||strcmp($1->token, "if-else") == 0||strcmp($1->token, "while") == 0) addScopeFirst($1,NULL,NULL,freshLabel(),NULL,NULL);}|declear {$$=$1;}|procedure {$$=$1;} |SEMICOLON  {$$=mkNode("",NULL,NULL);};

new_block: OPENBRACE procedures cmmnt declears stmnts CLOSEBRACE cmmnt
{
	$$=mkNode("{",$2,mkNode("", $4,mkNode("", $5,("}",NULL,NULL))));
	
}

stmnt: IF OPENPAREN expr CLOSEPAREN  stmnt_block 
{
	$$=mkNode("if",
	mkNode("(", $3, 
	mkNode(")",NULL,NULL)),$5);
	addScopeFirst($3,NULL,NULL,NULL,freshLabel(),NULL);
	
	
}%prec IF
| IF OPENPAREN expr CLOSEPAREN   stmnt_block    ELSE  stmnt_block  
{
	$$=mkNode("if-else",
	mkNode("", $3, 
	mkNode("",NULL,NULL)),
	mkNode("",$5,
	mkNode("",$7,NULL)));
	addScopeFirst($3,NULL,NULL,NULL,freshLabel(),NULL);
	addScopeFirst($3,NULL,NULL,NULL,NULL,freshLabel());
	
}
| WHILE cmmnt OPENPAREN expr CLOSEPAREN  stmnt_block  
{
	$$=mkNode("while",
	mkNode("(", $4, 
	mkNode(")",NULL,NULL)),$6);
addScopeFirst($$,NULL,NULL,NULL,freshLabel(),NULL);
	addScopeFirst($$,NULL,NULL,NULL,NULL,freshLabel());
}
| FOR cmmnt OPENPAREN assmnt_stmnt SEMICOLON expr SEMICOLON assmnt_stmnt CLOSEPAREN stmnt_block 
{
		$$= mkNode("for",
			mkNode("(",
			mkNode("",$4,$6),
			mkNode("",$8,
			mkNode(")",NULL,NULL))),$10);	
			addScopeFirst($$,NULL,NULL,NULL,freshLabel(),NULL);
			addScopeFirst($$,NULL,NULL,NULL,NULL,freshLabel());	

}
| assmnt_stmnt SEMICOLON cmmnt {$$=mkNode("assmnt_stmnt",$1,NULL);  }
| expr SEMICOLON cmmnt {$$=$1;}
| RETURN expr SEMICOLON cmmnt {$$=mkNode("return",$2,NULL);}
| new_block {$$=$1;} ;


assmnt_stmnt: lhs ASSINGMENT expr 
{
	$$=mkNode("=",$1,$3);
};

lhs: IDENTIFIER OPENBRACKET expr CLOSEBRACKET 
{
	$$=mkNode($1, mkNode("[",$3,mkNode("]",NULL,NULL)), NULL);

} 
| IDENTIFIER {$$=mkNode($1,NULL,NULL);}
| address_expr {$$=$1;}
| derefrence_expr{$$=$1;} ;


expr:  OPENPAREN expr CLOSEPAREN {$$=mkNode("(",$2,mkNode(")",NULL,NULL));}|
//bool oper
    expr EQL expr {$$=mkNode("==",$1,$3); }
	| expr NOTEQL expr {$$=mkNode("!=",$1,$3);}
	| expr GREATEREQL expr {$$=mkNode(">=",$1,$3);}
	| expr GREATER expr {$$=mkNode(">",$1,$3);}
	| expr LESSEQL expr {$$=mkNode("<=",$1,$3);}
	| expr LESS expr {$$=mkNode("<",$1,$3);}
//relope operator
	| expr AND expr {$$=mkNode("&&",$1,$3); addScopeFirst($1,NULL,NULL,NULL,freshLabel(),NULL); }
	| expr OR expr {$$=mkNode("||",$1,$3); addScopeFirst($1,NULL,NULL,NULL,NULL,freshLabel());  }
//aritmetical operator
	| expr PLUS expr {$$=mkNode("+",$1,$3); }
	| expr MINUS expr {$$=mkNode("-",$1,$3); }
	| expr MULTI expr {$$=mkNode("*",$1,$3); }
	| expr DIVISION expr {$$=mkNode("/",$1,$3);}
//not operator
	| NOT expr {$$=mkNode("!",$2,NULL);}
	| address_expr {$$=$1;}
	| derefrence_expr {$$=$1;}
	| call_func cmmnt {$$=$1;}
	| DECIMAL_LTL {$$=mkNode($1,mkNode("INT",NULL,NULL),NULL);}
	| HEX_LTL {$$=mkNode($1,mkNode("HEX", NULL, NULL),NULL);}
	| CHAR_LTL {$$=mkNode($1,mkNode("CHAR", NULL, NULL),NULL);}
	| REAL_LTL {$$=mkNode($1,mkNode("REAL", NULL, NULL),NULL);}
	| STRING_LTL {$$=mkNode($1,mkNode("STRING", NULL, NULL),NULL);}
	| BOOLFALSE {$$=mkNode($1,mkNode("BOOLEAN", NULL, NULL),NULL);}
	| BOOLTRUE {$$=mkNode($1,mkNode("BOOLEAN", NULL, NULL),NULL); }
	| LENGTH IDENTIFIER LENGTH 
	{
		$$=mkNode("|",
		mkNode($2,NULL,NULL),
		mkNode("|",NULL,NULL));
		
	}
	| IDENTIFIER OPENBRACKET expr CLOSEBRACKET 
	{$$=mkNode("solovar",mkNode($1,mkNode("[",$3,mkNode("]",NULL,NULL)),NULL),NULL);}
	| IDENTIFIER {$$=mkNode("solovar",mkNode($1,NULL,NULL),NULL);}
	| NULLL {$$=mkNode("null",NULL,NULL);};

address_expr: ADDRESS IDENTIFIER {$$=mkNode("&",mkNode($2,NULL,NULL),NULL);}
	| ADDRESS OPENPAREN IDENTIFIER CLOSEPAREN {$$=mkNode("&",mkNode("(",mkNode($3,NULL,NULL),NULL),mkNode(")",NULL,NULL));}
	| ADDRESS IDENTIFIER OPENBRACKET expr CLOSEBRACKET 
	{$$=mkNode("&", mkNode($2,mkNode("[",$4,mkNode("]",NULL,NULL)),NULL),NULL);
	}
	| ADDRESS OPENPAREN IDENTIFIER OPENBRACKET expr CLOSEBRACKET CLOSEPAREN 
	{
		$$=mkNode("&",
		mkNode("(", 
		mkNode($3,mkNode("[",$5,mkNode("]",NULL,NULL)),NULL)
		,mkNode(")",NULL,NULL)),NULL);
	};

	derefrence_expr: DEREFRENCE IDENTIFIER {$$=mkNode("^",mkNode($2,NULL,NULL),NULL);addScopeFirst($$,"",strCatFirst("*",$2),NULL,NULL,NULL);}
	| DEREFRENCE OPENPAREN expr CLOSEPAREN {$$=mkNode("^",mkNode("(",$3,NULL),mkNode(")",NULL,NULL));addScopeFirst($$,$3->SCOPE,strCatFirst("*",$3->var),NULL,NULL,NULL);}
	| DEREFRENCE IDENTIFIER OPENBRACKET expr CLOSEBRACKET 
	{$$=mkNode($1, mkNode($2,mkNode("[",$4,mkNode("]",NULL,NULL)),NULL), NULL);
	};

expr_list: expr COMMA expr_list {$$=mkNode("expr_list",$1,mkNode(",",$3,NULL));} 
	| expr {$$=mkNode("expr_list",$1,NULL);}
	| {$$=NULL;};

paren_expr:OPENPAREN expr_list CLOSEPAREN {$$=$2;};
call_func: IDENTIFIER paren_expr {$$=mkNode("Call func",mkNode($1,NULL,NULL),mkNode("ARGS",$2,NULL)); 

} ;
%%



int main()
{
	int ans= yyparse();
	FILE * f=fopen("output.txt","w+");
	if(ans==0)
	{
	printf(" The Syntax and Semantics Are Valid\n\n"); 
	}
	fprintf(f,"%s",startNode->SCOPE);
	printf("%s",startNode->SCOPE);
	return ans;

}

int yyerror(char *e)
{
	int yydebug=1; 
	fflush(stdout);
	fprintf(stderr,"Error %s at line %d\n" ,e,yylineno);
	fprintf(stderr, "does not accept '%s'\n",yytext);
	
	return 0;
}


Args * callFuncArg(SCOPE * MYscope,node *tree,int * count)
{
	Args  *arr1=NULL,arr2[50];
	char* type,*length;
	while(tree!=NULL)
	{
		arr2[(*count)++].type=getExpType(tree->left,MYscope);
		if(tree->right!=NULL)
			tree=tree->right->left;
		else
			tree=NULL;

	}
	arr1=(Args*)malloc(sizeof(Args)*(*count));
	for(int i=0;i<*count;i++)
		arr1[i].type=arr2[i].type;
	return arr1;
}
char* findFuncInScopes(node * tree,SCOPE * MYscope,int * countParams)
{
	SCOPE*tmp=MYscope;
	Args* args;
	int find=false,flag=true;
	while(tmp!=NULL)
	{
		for(int i=0;i<tmp->funcCount;i++)
		if(strcmp(tree->left->token,tmp->func[i]->name)==0)
		{
			find=true;
			flag=true;
			int count=0;
			args=callFuncArg(MYscope,tree->right->left,&count);
			if(count==tmp->func[i]->argNum)
			{
				for(int j=0,t=count-1;j<count;j++,t--)
				{
					if(strcmp(args[j].type,tmp->func[i]->args[t].type)!=0)
						flag=false;
				}
				if(flag==true){
					if(countParams!= NULL)
						*countParams = popParams(args,count);
					return tmp->func[i]->retType;
				}
			}
		}
		tmp=tmp->preScope;
	}
	printf("ERROR,func %s not find call in scope %s in func/proc %s\n",tree->left->token,MYscope->name,globalScope->func[globalScope->funcCount-1]->name);
	if(find==true)
		printf("There is a func with the same name that accepts different arguments\n");
		exit(1);
}
char *findVar(node * tree,SCOPE * MYscope)
{
	SCOPE*tmp=MYscope;
	if(strcmp(tree->token,"solovar")==0)
		tree=tree->left;
	while(tmp!=NULL)
	{
		for(int i=0;i<tmp->varCount;i++)
		if(strcmp(tree->token,tmp->var[i].name)==0)
		{
			
			if(tree->left!=NULL && strcmp(tree->left->token,"[")==0)
			{
				if(strcmp(tmp->var[i].type,"string")==0)
					if(strcmp(getExpType(tree->left->left,MYscope),"int")==0)
					{
						return "char";
					}
					else
					{
						printf("ERORR, index in string can be only int (<string>[<int>])in scope %s in func/proc %s\n",MYscope->name,globalScope->func[globalScope->funcCount-1]->name);
						exit(1);
					}
				else
				{
					printf("ERORR,you can use index only on string type (<string>[<int>]) in scope %s in func/proc %s\n",MYscope->name,globalScope->func[globalScope->funcCount-1]->name);
					exit(1);
				}

			}
			else
			return tmp->var[i].type;

		}
		tmp=tmp->preScope;
	}
	printf("ERORR,var %s not find in scope %s in func/proc %s\n ",tree->token,MYscope->name,globalScope->func[globalScope->funcCount-1]->name);
	exit(1);
	
}
char * getExpType(node * tree,SCOPE* MYscope){
	char* message=(char*)malloc(sizeof(char)*7);
	message="";
	if(strcmp(tree->token,"null")==0)
		message="NULL";
	else
	if(tree->left!=NULL){
		if(strcmp(tree->left->token,"INT")==0)
			message= "int";
		if(strcmp(tree->left->token,"HEX")==0)
			message= "hex";
		if(strcmp(tree->left->token,"CHAR")==0)
			message= "char";
		if(strcmp(tree->left->token,"REAL")==0)
			message= "real";
		if(strcmp(tree->left->token,"STRING")==0)
			message= "string";
		if(strcmp(tree->left->token,"BOOLEAN")==0)
			message= "boolean";
		if(strcmp(tree->token,"!")==0)
		if(strcmp(getExpType(tree->left,MYscope),"boolean")==0)
			message="boolean";
		else{
			printf("Erorr op ! you can use only on boolean type");
			exit(1);
		}
		if(strcmp(tree->token,"|")==0)
		if(strcmp(getExpType(tree->left,MYscope),"string")==0)
		message="int";
		else{
			printf("Erorr op | you can use only on string type in func/proc %s",globalScope->func[globalScope->funcCount-1]->name);
			exit(1);
		}
		if(strcmp(tree->token,"==")==0||strcmp(tree->token,"!=")==0)
		{
			if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))==0&&strcmp(getExpType(tree->right,MYscope),"string")!=0)
			message="boolean";
			else{
				printf("ERORR, you cant do %s between %s and %s in func/proc %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->funcCount-1]->name);
				exit(1);
			}
		}

		if(strcmp(tree->token,">=")==0||strcmp(tree->token,">")==0||strcmp(tree->token,"<=")==0||strcmp(tree->token,"<")==0)
		{
			if((strcmp(getExpType(tree->left,MYscope),"int")==0||strcmp(getExpType(tree->left,MYscope),"real")==0)&&(strcmp(getExpType(tree->right,MYscope),"int")==0||strcmp(getExpType(tree->right,MYscope),"real")==0))
			message="boolean";
			else{
				printf("ERORR, you cant do %s between %s and %s in func/proc %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->funcCount-1]->name);
				exit(1);
			}
		}

		if(strcmp(tree->token,"&&")==0||strcmp(tree->token,"||")==0)
		{

			if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))==0&&strcmp(getExpType(tree->right,MYscope),"boolean")==0)
			message="boolean";
			else{
				printf("ERORR, you cant do %s between %s and %s in func/proc %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->funcCount-1]->name);
				exit(1);
			}
			

		}
		if(strcmp(tree->token,"-")==0||strcmp(tree->token,"+")==0)
		{
			if((strcmp(getExpType(tree->left,MYscope),"int")==0||strcmp(getExpType(tree->left,MYscope),"real")==0)&&(strcmp(getExpType(tree->right,MYscope),"int")==0||strcmp(getExpType(tree->right,MYscope),"real")==0))
			{
			if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))==0&&strcmp(getExpType(tree->left,MYscope),"int")==0)
			message="int";
			else
			message="real";
			}

			if(strcmp(getExpType(tree->right,MYscope),"int")==0&&(strcmp(getExpType(tree->left,MYscope),"char*")==0||strcmp(getExpType(tree->right,MYscope),"int*")==0||strcmp(getExpType(tree->right,MYscope),"real*")==0)){
				message=getExpType(tree->left,MYscope);
			}
			else if(strcmp(message,"")==0)
			{
				printf("ERORR, you cant do %s between %s and %s in func/proc %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),globalScope->func[globalScope->funcCount-1]->name);
				exit(1);
			}

		}
		if(strcmp(tree->token,"*")==0||strcmp(tree->token,"/")==0)
		{
			if((strcmp(getExpType(tree->left,MYscope),"int")==0||strcmp(getExpType(tree->left,MYscope),"real")==0)&&(strcmp(getExpType(tree->right,MYscope),"int")==0||strcmp(getExpType(tree->right,MYscope),"real")==0))
			{
			if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))==0&&strcmp(getExpType(tree->left,MYscope),"int")==0)
			message="int";
			else
			message="real";
			}
			else
			{
				printf("ERORR, you cant do %s between %s and %s\n",tree->token,getExpType(tree->left,MYscope),getExpType(tree->right,MYscope));
				exit(1);
			}
		}
		if(strcmp(tree->token,"&")==0)
		{
			if(strcmp(tree->left->token,"(")==0)
				message=getExpType(tree->left->left,MYscope);
			else{
				message=getExpType(tree->left,MYscope);
				
				}
			if(strcmp(message,"char")==0)
			message="char*";
			else
			if(strcmp(message,"int")==0)
			message="int*";
			else
			if(strcmp(message,"real")==0)
			message="real*";
			else
			{
				printf("ERORR, you cant do %s on %s \n",tree->token,message);
				exit(1);
			}
		}
		if(strcmp(tree->token,"^")==0)
		{
			if(strcmp(tree->left->token,"(")==0)
				message=getExpType(tree->left->left,MYscope);
			else
				message=getExpType(tree->left,MYscope);
			
			if(strcmp(message,"char*")==0)
			message="char";
			else
			if(strcmp(message,"int*")==0)
			message="int";
			else
			if(strcmp(message,"real*")==0)
			message="real";
			else
			{
				printf("ERORR, you cant do %s on %s \n",tree->token,message);
				exit(1);
			}

		}
		if(strcmp(tree->token,"(")==0)
			message=getExpType(tree->left,MYscope);
		if(strcmp(tree->token,"Call func")==0)
			message=findFuncInScopes(tree,MYscope,NULL);
		
	}
	if(strcmp(message,"")==0)
		message=findVar(tree,MYscope);

	
	

	return message;
}

SCOPE* mkScope(char* name)
{	
	SCOPE *newScope = (SCOPE*)malloc(sizeof(SCOPE));
	newScope->name=name;
	newScope->var=NULL;
	newScope->varCount=0;
	newScope->func=NULL;
	newScope->funcCount=0;
	newScope->nextScope=NULL;
	newScope->preScope=NULL;
	return newScope;
}


void pushScopes(SCOPE* from,char* name)
{
	SCOPE * point;
	if(globalScope==NULL)
		globalScope=mkScope(name);
	else{
	point=globalScope;
	while(point->nextScope!=NULL)
		point=point->nextScope;
	point->nextScope=mkScope(name);
	point->nextScope->preScope=from;
	}
}


void addVar(Args * args,int countvars,int isArg,SCOPE * MYscope){
	if(countvars==0)
	return;
	Variable* tmp;
	SCOPE * scopes=MYscope;

	for(int i=0;i<countvars;i++)
		for(int j=0;j<countvars;j++)
	if(i!=j && strcmp(args[j].name,args[i].name)==0 )
	{
		printf("There is the same var %s in one declare",args[i].name);
		SCOPE * t=scopes->preScope;
		while(t->preScope!=NULL && t->preScope->funcCount==0)
			t=t->preScope;
		if(t->func!=NULL)
		printf(",in func %s\n",t->func[t->funcCount-1]->name);
			else
		printf("\n");
		exit(1);
	}
	if(scopes->var==NULL)
	{ 
		scopes->var=(Variable*) malloc(sizeof(Variable)*countvars);
	}
	else
	{
		tmp=scopes->var;
		scopes->var=(Variable*) malloc(sizeof(Variable)*(scopes->varCount+countvars));
		for(int i=0;i<scopes->varCount;i++)
		{
			for(int j=0;j<countvars;j++)
			{
				if(strcmp(tmp[i].name,args[j].name)==0 )
				{
					printf("There can't be the same var %s in same scope",tmp[i].name);
					SCOPE * t=scopes->preScope;
					while(t->preScope!=NULL && t->preScope->funcCount==0)
						t=t->preScope;
					if(t->func!=NULL)
					printf(",in func %s\n",t->func[t->funcCount-1]->name);
					else
					printf("\n");
					exit(1);
				}
			}
			scopes->var[i]=tmp[i];	
		}
	}
	for(int j=0;j<countvars;j++)
	{

		scopes->var[scopes->varCount].name=args[j].name;
		scopes->var[scopes->varCount].value=NULL;
		scopes->var[scopes->varCount].isArg=isArg;
		scopes->var[scopes->varCount].length=args[j].length;
		scopes->var[(scopes->varCount)++].type=args[j].type;
	}
}

void addFunc(char * name,Args * args,node *retType,int argNum,SCOPE * MYscope){
	Func** tmp;
	SCOPE * scopes=MYscope;
	for(int i=0;i<argNum;i++)
		for(int j=0;j<argNum;j++)
	if(i!=j && strcmp(args[j].name,args[i].name)==0 )
	{
		printf("ERROR, there are identicles arguments %s in func %s\n",args[i].name,name);
		exit(1);
	}
	if(scopes->func==NULL)
	{ 
		scopes->func=(Func**) malloc(sizeof(Func*));
	}
	else
	{
		tmp=scopes->func;
		scopes->func=(Func**) malloc(sizeof(Func*)*(scopes->funcCount+1));
		for(int i=0;i<scopes->funcCount;i++)
		{
				if(strcmp(tmp[i]->name,name)==0 )
				{
					printf("ERROR, there's already func %s in same scope \n",tmp[i]->name);
					exit(1);
				}
				scopes->func[i]=tmp[i];
		}
	}
		scopes->func[scopes->funcCount]=(Func*) malloc(sizeof(Func));
		scopes->func[scopes->funcCount]->name=name;
		scopes->func[scopes->funcCount]->args=args;
		if(retType==NULL)
		scopes->func[scopes->funcCount]->retType=NULL;
		else{
		if(strcmp(retType->token,"string")==0)
			{
				printf("ERORR,return type func %s can't be string\n",name);
				exit(1);
			}
		scopes->func[scopes->funcCount]->retType=retType->token;
		}
		scopes->func[scopes->funcCount]->argNum=argNum;
		scopes->func[scopes->funcCount]->getRet=false;
		++(scopes->funcCount); 
}

Args * mkArguments(node *tree,int *count){
	Args  *arr1=NULL,arr2[50];
	char* type,*length;
	if(tree!=NULL)
	{
		node * temp1=tree,*tmp=tree;
		do{
		if(strcmp(temp1->token, "")==0)
		{
			tmp=temp1->right->left;
			temp1=temp1->left;

		if(strcmp(tmp->token, "(")==0||strcmp(tmp->token, "var")==0)
		{
			type=tmp->left->token;
			if(tmp->left->left!=NULL)
			length=tmp->left->left->left->token;
			node * tmptree;
			tmptree=tmp->right->left;
			do{
			arr2[*count].name=tmptree->token;
			arr2[*count].type=type;
			arr2[*count].length=length;
			(*count)++;
			if(tmptree->left==NULL)
				tmptree=NULL;
			else
				tmptree=tmptree->left->left;
			}while(tmptree!=NULL);
		}
		}
		}while(strcmp(temp1->token, "(")!=0&&strcmp(tmp->token, "var")!=0);
		tmp=temp1;
		if(strcmp(tmp->token, "(")==0||strcmp(tmp->token, "var")==0)
		{
			type=tmp->left->token;
			node * tmptree;
			if(strcmp(tmp->token, "var")==0)
			tmptree=tmp->right;
			else
			tmptree=tmp->right->left;
			if(tmp->left->left!=NULL)
			length=tmp->left->left->left->token;
			do{
			arr2[*count].name=tmptree->token;
			arr2[*count].type=type;
			arr2[*count].length=length;
			(*count)++;
			if(tmptree->left==NULL)
				tmptree=NULL;
			else
				tmptree=tmptree->left->left;
			}while(tmptree!=NULL);
		}
		arr1=(Args*)malloc(sizeof(Args)*(*count));
		for(int i=0;i<*count;i++)
		{
			for(int j=0;j<*count;j++){
			}
			arr1[i].name=arr2[i].name;
			arr1[i].type=arr2[i].type;
		}
	}
	return arr1;
}

node* mkNode (char *token, node *left, node *right)
{
	node *newnode = (node*)malloc(sizeof(node));
	newnode->left=left;
	newnode->right=right;
	newnode->token=token;
	newnode->SCOPE=NULL;
	newnode->var=NULL;
	newnode->label=NULL;
	newnode->trueLab=NULL;
	newnode->falseLab=NULL;
    newnode->cnt=0;
	return newnode;
}

SCOPE* finScope(SCOPE * scopes)
{
	SCOPE * MYscope=scopes;
	if(MYscope!=NULL)
	while(MYscope->nextScope!=NULL)
		MYscope=MYscope->nextScope;
	return MYscope;
}

void syntaxAnalyzer(node *tree,SCOPE * MYscope){

	if(strcmp(tree->token, "=") == 0 )
	{
		if(!(strcmp(getExpType(tree->right,MYscope),"NULL")==0&& (strcmp(getExpType(tree->left,MYscope),"real*")==0||strcmp(getExpType(tree->left,MYscope),"int*")==0||strcmp(getExpType(tree->left,MYscope),"char*")==0)))
		if(strcmp(getExpType(tree->left,MYscope),getExpType(tree->right,MYscope))!=0)
		{
			printf("ERORR, you can't do = between %s and %s in scope %s in func/proc %s\n",getExpType(tree->left,MYscope),getExpType(tree->right,MYscope),MYscope->name,globalScope->func[globalScope->funcCount-1]->name);
			exit(1);
		}

	}
	else if(strcmp(tree->token, "var") == 0)
	{
		int varCount=0;
		Args * var=mkArguments(tree,&varCount);
		addVar(var,varCount,0,MYscope);
		
		
	}
	else if(strcmp(tree->token, "if") == 0)
	{
		if(strcmp(getExpType(tree->left->left,MYscope),"boolean")!=0)
		{
			printf("ERROR, the condition in if has to be boolean\n");
			exit(1);
		}
		

		if(strcmp(tree->right->token,"{")!=0)
		{
			pushScopes(MYscope,tree->token);
			if (tree->left) 
				syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
			if (tree->right)
				syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
        	scope--;
			return;
		}

	}
		else if(strcmp(tree->token, "while") == 0)
	{
		if(strcmp(getExpType(tree->left->left,MYscope),"boolean")!=0)
		{
			printf("ERROR, the condition in while has to be boolean\n");
			exit(1);
		}

		if(strcmp(tree->right->token,"{")!=0)
		{
			pushScopes(MYscope,tree->token);
			if (tree->left) 
				syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
			if (tree->right)
				syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
        	scope--;
			return;
		}
	
	}
			else if(strcmp(tree->token, "for") == 0)
	{

	 if(strcmp(getExpType(tree->left->left->right,MYscope),"boolean")!=0)
		{
			printf("ERROR, the condition in for has to be boolean\n");
			exit(1);
		}

		syntaxAnalyzer(tree->left->left->left,MYscope);

		syntaxAnalyzer(tree->left->right->left,MYscope);

		if(strcmp(tree->right->token,"{")!=0)
		{

			pushScopes(MYscope,tree->token);

			if (tree->left) 
				syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
			if (tree->right)
				syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
        	scope--;
			return;
		}		
	}

	else if(strcmp(tree->token, "FUNC") == 0 )
	{
        int count=0;
		Args * arg=mkArguments(tree->left->right->left,&count);
		addFunc(tree->left->token,arg,tree->left->right->right->left,count,MYscope);
		pushScopes(MYscope,tree->token);
		addVar(arg,count,1,finScope(MYscope));
	if (tree->left) 
		syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
	if (tree->right)
		syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
		if(MYscope->func[MYscope->funcCount-1]->getRet==false)
		{
			printf("ERORR, there is no return in func %s\n",tree->left->token);
			exit(1);
		}
        scope--;		
		return;
	}
    else if(strcmp(tree->token, "PROC") == 0)
	{
		
        int count=0;
		Args * arg=mkArguments(tree->right->left,&count);
		addFunc(tree->left->token,arg,NULL,count,MYscope);
		pushScopes(MYscope,tree->token);
		addVar(arg,count,1,finScope(MYscope));
	if (tree->left) 
		syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
	if (tree->right)
		syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
		scope--;	
		return;
    }

	else if(strcmp(tree->token, "Call func") == 0)
	{
		int count=0;
		findFuncInScopes(tree,MYscope,&count);
		tree->cnt=count;
	  }
	else if(strcmp(tree->token, "CODE") == 0)
	{
		pushScopes(NULL,tree->token);
		if (tree->left) 
			syntaxAnalyzer(tree->left,globalScope);
		
		if (tree->right)
			syntaxAnalyzer(tree->right,globalScope);
			scope--;
			return;
	}
    else if(strcmp(tree->token, "BODY") == 0)
	{  
		 
    }
	else if(strcmp(tree->token, "ARGS") == 0)
	{     
    }
    else if(strcmp(tree->token, "Main") == 0)
	{
		if(mainFlag==true && strcmp(MYscope->name,"CODE")==0)
		{
			printf("There can be only one Main and can't be in func/proc\n");
			exit(1);
		}
		mainFlag=true;
		addFunc(tree->token,NULL,NULL,0,MYscope);
		pushScopes(MYscope,tree->token);

	if (tree->left) 
		syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
	if (tree->right)
		syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
        scope--;
		return;
               
    }       
	else if(strcmp(tree->token, "if-else") == 0)
	{
		if(strcmp(getExpType(tree->left->left,MYscope),"boolean")!=0)
		{
			printf("ERORR, if condition must be of type boolean\n");
			exit(1);
		}

		if(strcmp(tree->right->left->token,"{")!=0)
		{
			pushScopes(MYscope,tree->token);
			syntaxAnalyzer(tree->right->left,finScope( MYscope->nextScope));
			scope--;
			pushScopes(MYscope,tree->token);
			syntaxAnalyzer(tree->right->right->left,finScope( MYscope->nextScope));
        	scope--;
			return;
		}
		
		
		
	}
	else if(strcmp(tree->token, "return") == 0)
	{
		SCOPE * tmp= MYscope;
		int flag=true;
		while(strcmp(tmp->name,"FUNC")!=0&&strcmp(tmp->name,"PROC")!=0&&strcmp(tmp->name,"CODE")!=0)
		{
			tmp=tmp->preScope;
			flag=false;
		}
		if(flag==false)
		{
			if(strcmp(getExpType(tree->left,MYscope),tmp->preScope->func[tmp->preScope->funcCount-1]->retType))
			{
			printf("ERORR,the return type doesn't match the type of func %s \n",tmp->preScope->func[tmp->preScope->funcCount-1]->name);
			printf("%s ,%s %s\n",getExpType(tree->left,MYscope),tmp->preScope->func[tmp->preScope->funcCount-1]->retType,tmp->preScope->func[tmp->preScope->funcCount-1]->name);
			exit(1);
			}
		}
		else{
		if(tmp->preScope->func[tmp->preScope->funcCount-1]->retType!=NULL){
		if(0==strcmp(getExpType(tree->left,MYscope),tmp->preScope->func[tmp->preScope->funcCount-1]->retType)){
			tmp->preScope->func[tmp->preScope->funcCount-1]->getRet=true;
		}
		else{
			printf("ERORR,the return type doesn't match the type of func %s \n",tmp->preScope->func[tmp->preScope->funcCount-1]->name);
			printf("%s ,%s %s\n",getExpType(tree->left,MYscope),tmp->preScope->func[tmp->preScope->funcCount-1]->retType,tmp->preScope->func[tmp->preScope->funcCount-1]->name);
			exit(1);
		}
		}
		else
		{
			printf("ERORR, there can't be return in proc %s\n",tmp->preScope->func[tmp->preScope->funcCount-1]->name);
			exit(1);
		}  
		}

	}
	else if(strcmp(tree->token, "{") == 0)
	{
    pushScopes(MYscope,tree->token);
	if (tree->left) 
		syntaxAnalyzer(tree->left,finScope( MYscope->nextScope));
	
	if (tree->right)
		syntaxAnalyzer(tree->right,finScope( MYscope->nextScope));
        scope--;
		return;		
	}

	else if(strcmp(tree->token, "}") == 0)
	{                   
                      
    }
	else if(strcmp(tree->token, "") == 0);
	else if(strcmp(tree->token, "(") == 0);
	else if(strcmp(tree->token, ")") == 0);
	else if(strcmp(tree->token, ",") == 0);
	else if(strcmp(tree->token, ";") == 0);
	else if(strcmp(tree->token, "&&") == 0 ||
		strcmp(tree->token, "/") == 0 || 
		strcmp(tree->token, "==") == 0 || 
		strcmp(tree->token, ">") == 0 || 
		strcmp(tree->token, ">=") == 0 || 
		strcmp(tree->token, "<") == 0 || 
		strcmp(tree->token, "<=") == 0 || 
		strcmp(tree->token, "-") == 0 || 
		strcmp(tree->token, "!") == 0 || 
		strcmp(tree->token, "!=") == 0 || 
		strcmp(tree->token, "||") == 0 || 
		strcmp(tree->token, "+") == 0 || 
		strcmp(tree->token, "*") == 0 || 
		strcmp(tree->token, "&") == 0 || 
		strcmp(tree->token, "^") == 0 || 
		strcmp(tree->token, ",") == 0 )
	{				
				
	}
	else if(strcmp(tree->token, "|") == 0 );
	else if(strcmp(tree->token, "solovar") == 0 )
	{
		findVar(tree->left,MYscope);
	}
	if (tree->left) 
		syntaxAnalyzer(tree->left,MYscope);
	
	if (tree->right)
		syntaxAnalyzer(tree->right,MYscope);

}

int popParams(Args * args,int count){
	int size=0;
	for(int i =0;i<count;i++)
	{
		if(strcmp(args[i].type,"int")==0)
			size += 4;
		else if(strcmp(args[i].type,"char")==0)
			size += 1;
		else if(strcmp(args[i].type,"real")==0)
			size += 8;
		else if(strcmp(args[i].type,"string")==0)
			size += atoi(args[i].length);
		else if(strcmp(args[i].type,"boolean")==0)
			size += 4;
		else if(strcmp(args[i].type,"int*")==0)
			size += 4;
		else if(strcmp(args[i].type,"char*")==0)
			size += 4;
		else if(strcmp(args[i].type,"real*")==0)
			size += 4;
	}
	return size;
}


/*PART 3*/

void addScopeFirst(node* node,char *SCOPE,char *var,char *label,char *trueLab,char *falseLab)
	{
		if(SCOPE!=NULL){
		node->SCOPE=(char*)malloc(sizeof(char)*(1+strlen(SCOPE)));
		strcpy(node->SCOPE,SCOPE);
		}
		else if(node->SCOPE==NULL)
		{
		node->SCOPE=(char*)malloc(sizeof(char)*2);
		strcpy(node->SCOPE,"");
		}

		if(var!=NULL){
		node->var=(char*)malloc(sizeof(char)*(1+strlen(var)));
		strcpy(node->var,var);
		}
		else if(node->var==NULL)
		{
		node->var=(char*)malloc(sizeof(char)*2);
		strcpy(node->var,"");
		}

		if(label!=NULL&& node->label==NULL){
		node->label=(char*)malloc(sizeof(char)*(1+strlen(label)));
		strcpy(node->label,label);
		}

		if(trueLab!=NULL && node->trueLab==NULL){
		node->trueLab=(char*)malloc(sizeof(char)*(1+strlen(trueLab)));
		strcpy(node->trueLab,trueLab);
		}
		
		if(falseLab!=NULL && node->falseLab==NULL){
		node->falseLab=(char*)malloc(sizeof(char)*(1+strlen(falseLab)));
		strcpy(node->falseLab,falseLab);
		}

	}
void addScopeSec(node* node,char *SCOPE,char *var,char *label,char *trueLab,char *falseLab)
	{
		if(SCOPE!=NULL){
		node->SCOPE=(char*)malloc(sizeof(char)*(1+strlen(SCOPE)));
		strcpy(node->SCOPE,SCOPE);
		}
		else if(node->SCOPE==NULL)
		{
		node->SCOPE=(char*)malloc(sizeof(char)*2);
		strcpy(node->SCOPE,"");
		}

		if(var!=NULL){
		node->var=(char*)malloc(sizeof(char)*(1+strlen(var)));
		strcpy(node->var,var);
		}
		else if(node->var==NULL)
		{
		node->var=(char*)malloc(sizeof(char)*2);
		strcpy(node->var,"");
		}

		if(label!=NULL){
		node->label=(char*)malloc(sizeof(char)*(1+strlen(label)));
		strcpy(node->label,label);
		}

		if(trueLab!=NULL){
		node->trueLab=(char*)malloc(sizeof(char)*(1+strlen(trueLab)));
		strcpy(node->trueLab,trueLab);
		}
		
		if(falseLab!=NULL && node->falseLab==NULL){
		node->falseLab=(char*)malloc(sizeof(char)*(1+strlen(falseLab)));
		strcpy(node->falseLab,falseLab);
		}

	}
	char* freshVar(){
		char* x;
		asprintf(&x,"t%d",t++);
		return x;
	}
	char* freshLabel(){
		char* x;
		asprintf(&x,"L%d",lab++);
		return x;
	}
	char* Gen(char*s1,char*s2,char*s3,char*s4,char*s5)
	{
		char* x;

		asprintf(&x,"%s %s %s %s %s\n",s1,s2,s3,s4,s5);
		return x;
	}
	

char *replaceString(const char *s, const char *old_word, const char *new_word) 
{ 
    char *res; 
    int i, c = 0; 
    int newWlen = strlen(new_word); 
    int oldWlen = strlen(old_word); 
    for (i = 0; s[i] != '\0'; i++) 
    { 
        if (strstr(&s[i], old_word) == &s[i]) 
        { 
            c++; 
  
            i += oldWlen - 1; 
        } 
    } 
  
    res = (char *)malloc(i + c * (newWlen - oldWlen) + 1); 
  
    i = 0; 
    while (*s) 
    { 
        if (strstr(s, old_word) == s) 
        { 
            strcpy(&res[i], new_word); 
            i += newWlen; 
            s += oldWlen; 
        } 
        else
            res[i++] = *s++; 
    } 
  
    res[i] = '\0'; 
    return res; 
} 

 char* strCatFirst(char*des,char*src){
		char* tamp=des;
		char* num;
		asprintf(&num,"%d ",line++);
		if(src!=NULL){
			if(des==NULL){
				des=(char*)malloc((strlen(src)+1)*sizeof(char));
				strcpy(des,src);
				return des;
			}
		des=(char*)malloc((strlen(des)+strlen(src)+1+strlen(num))*sizeof(char));
		if(tamp!=NULL){
		strcat(des,tamp);
		}
		if(src!=NULL)
		{
		strcat(des,src);
		}
		}
		return des;
	}

char* addSpaces(char *label)
{
	char * message;
	char x=' ';
	int length = strlen(strCatFirst(label,"\0"));
	if(label==NULL || !strcmp(label,""))
		message="";
	else
		{
		asprintf(&message,"%c",x);
		while(length<5){
			asprintf(&message,"%c%s",x,message);
			length++;
		}
		asprintf(&label,"%s: ",strCatFirst(label,"\0"));
		message=strCatFirst(message,label);
		}
		return message;
}


void make3AC(node * tree)
{ 
	
	if(strcmp(tree->token, "=") == 0 )
	{ if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
	   addScopeFirst(tree,strCatFirst(tree->right->SCOPE,Gen(tree->left->var,"=",tree->right->var,"","")),NULL,NULL,NULL,NULL); 
	   return;  
	}
	else if(strcmp(tree->token, "if") == 0)
	{ 
		if(tree->left->left)
		addScopeFirst(tree->left->left,NULL,NULL,NULL,NULL,tree->label);
		if(tree->right)
		addScopeSec(tree->right,NULL,NULL,tree->label,NULL,NULL);
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		addScopeFirst(tree,strCatFirst(tree->left->left->SCOPE,strCatFirst(addSpaces(tree->left->left->label),strCatFirst(addSpaces(tree->left->left->trueLab),strCatFirst(tree->right->SCOPE,addSpaces(tree->trueLab))))),NULL,NULL,NULL,NULL);
		return;
	}
	else if(strcmp(tree->token, "if-else") == 0)
	{ 
		if(tree->right->left)
		addScopeFirst(tree->right->left,NULL,NULL,tree->label,NULL,NULL);			
		if(tree->right->right->left)
		addScopeSec(tree->right->right->left,NULL,NULL,tree->label,NULL,tree->label);
		if(tree->right->left)
		addScopeSec(tree->right->left,NULL,NULL,tree->label,NULL,tree->label);
		
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		addScopeFirst(tree,strCatFirst(strCatFirst(tree->left->left->SCOPE,strCatFirst(addSpaces(tree->left->left->trueLab),tree->right->left->SCOPE))
		,strCatFirst(strCatFirst("goto ",strCatFirst(strCatFirst(tree->label,"\n"),strCatFirst(addSpaces(tree->left->left->falseLab),tree->right->right->left->SCOPE))),addSpaces(tree->label))),NULL,NULL,NULL,NULL);
	return;
	}
	else if(strcmp(tree->token, "while") == 0)
	{ 
		if(tree->left->left)
			addScopeFirst(tree->left->left,NULL,NULL,NULL,tree->falseLab,tree->label);
		if(tree->right)
			addScopeSec(tree->right,NULL,NULL,tree->label,NULL,NULL);
		
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
			
			addScopeFirst(tree,strCatFirst(strCatFirst(strCatFirst( addSpaces(tree->trueLab),tree->left->left->SCOPE),addSpaces(tree->falseLab)),
				strCatFirst(tree->right->SCOPE,strCatFirst(strCatFirst("\tgoto ",strCatFirst(tree->trueLab,"\n")),addSpaces(tree->label)))),NULL,NULL,NULL,NULL);
		return ;
	}
	else if(strcmp(tree->token, "stmnts") == 0)
	{ 
		
		if(tree->right!=NULL)
			if(strcmp(tree->right->token, "for") == 0||strcmp(tree->right->token, "if-else") == 0||strcmp(tree->right->token, "while") == 0)
				addScopeSec(tree->right,NULL,NULL,tree->label,NULL,NULL);
        if(tree->right!=NULL && tree->left!=NULL)
        if(strcmp(tree->left->right->token, "if") == 0||strcmp(tree->left->right->token, "for") == 0||strcmp(tree->left->right->token, "if-else") == 0||strcmp(tree->left->right->token, "while") == 0)
				addScopeSec(tree->right,NULL,NULL,NULL,tree->left->right->label,NULL);
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);

			if(tree->right!=NULL && tree->left!=NULL)
                if((strcmp(tree->right->token, "while") == 0||strcmp(tree->right->token, "for") == 0||strcmp(tree->right->token, "if-else") == 0)&&(strcmp(tree->left->right->token, "if") == 0||strcmp(tree->left->right->token, "for") == 0||strcmp(tree->left->right->token, "if-else") == 0||strcmp(tree->left->right->token, "while") == 0))
                    addScopeFirst(tree,strCatFirst(tree->left->SCOPE,&tree->right->SCOPE[8]),NULL,NULL,NULL,NULL);
                    else
					addScopeFirst(tree,strCatFirst(tree->left->SCOPE,tree->right->SCOPE),NULL,NULL,NULL,NULL);
			else if(tree->right!=NULL)
            {
                if(strcmp(tree->right->token, "for") == 0||strcmp(tree->right->token, "if-else") == 0||strcmp(tree->right->token, "while") == 0)
                    addScopeFirst(tree,tree->right->SCOPE,NULL,NULL,NULL,NULL);
                    else        
				    addScopeFirst(tree,strCatFirst(tree->right->SCOPE ,addSpaces(tree->right->label)),NULL,NULL,NULL,NULL);
            }else
				addScopeFirst(tree,"",NULL,NULL,NULL,NULL);
			
	return;
		
	}
	else if(strcmp(tree->token, "for") == 0)
	{ 
		if(tree->left->left->right)
			addScopeFirst(tree->left->left->right,NULL,NULL,NULL,tree->falseLab,tree->label);
		if(tree->right)
			addScopeSec(tree->right,NULL,NULL,tree->label,NULL,NULL);
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
					addScopeFirst(tree,	
		strCatFirst(strCatFirst(strCatFirst(tree->left->left->left->SCOPE, addSpaces(tree->trueLab)),tree->left->left->right->SCOPE),
		strCatFirst(strCatFirst(strCatFirst(addSpaces(tree->falseLab),tree->right->SCOPE),tree->left->right->left->SCOPE),strCatFirst("\tgoto ",strCatFirst(tree->trueLab,strCatFirst("\n",addSpaces(tree->label)))))),NULL,NULL,NULL,NULL);
	}
    else if(strcmp(tree->token, "PROC") == 0)
	{ if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
	 char*x; asprintf(&x," %s:\n",tree->left->token);addScopeFirst(tree,strCatFirst(x,tree->right->right->SCOPE),NULL,NULL,NULL,NULL);
		return;
	}
	else if(strcmp(tree->token, "FUNC") == 0)
	{
		 if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		
	 char*x; asprintf(&x," %s:\n",tree->left->token);addScopeFirst(tree,strCatFirst(x,tree->right->left->SCOPE),NULL,NULL,NULL,NULL);
		return;
	}
		else if(strcmp(tree->token, "expr_list") == 0)
	{
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
			if(tree->right==NULL)
				addScopeFirst(tree,strCatFirst(tree->left->SCOPE,strCatFirst("PushParam ",strCatFirst(tree->left->var,"\n"))),NULL,NULL,NULL,NULL);
			else
				addScopeFirst(tree,strCatFirst(strCatFirst(tree->left->SCOPE,strCatFirst("PushParam ",strCatFirst(tree->left->var,"\n"))),tree->right->left->SCOPE),NULL,NULL,NULL,NULL);
	}	
	else if(strcmp(tree->token, "Call func") == 0)
	{ 

		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		char * x,*parm=(char*)malloc(sizeof(char));
		if(tree->right->left==NULL)
			strcpy(parm,"");
		else
			parm=tree->right->left->SCOPE;
		addScopeFirst(tree,NULL,freshVar(),NULL,NULL,NULL);
		asprintf(&x,"%s%s = LCALL %s\n‪\tPopParams %d‬‬‬‬\n",parm,tree->var,tree->left->token,tree->cnt);
		addScopeFirst(tree,x,NULL,NULL,NULL,NULL);
		return;
	}
	else if(strcmp(tree->token, "CODE") == 0)
	{ if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		 if(tree->left)
			addScopeFirst(tree,strCatFirst(tree->left->SCOPE,tree->right->SCOPE),NULL,NULL,NULL,NULL);
		else
			addScopeFirst(tree,tree->right->SCOPE,NULL,NULL,NULL,NULL);
		tree->SCOPE=replaceString(tree->SCOPE, "\n\n", "\n") ;
		char x='a',*y,*z;

		while (x<='z')
		{
			asprintf(&y,"\n%c",x);
			asprintf(&z,"\n\t%c",x);
			tree->SCOPE=replaceString(tree->SCOPE, y, z) ;
			x++;
		}
		x='A';
				while (x<='Z')
		{
			asprintf(&y,"\n%c",x);
			asprintf(&z,"\n\t%c",x);
			tree->SCOPE=replaceString(tree->SCOPE, y, z) ;
			x++;
		}
		return;
	}
    else if(strcmp(tree->token, "BODY") == 0)
	{ 
		
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		if(tree->right->right->left){
		
		if(tree->right->right->left->SCOPE[strlen(strCatFirst(tree->right->right->left->SCOPE,"\0"))-2]==':')
			addScopeFirst(tree,strCatFirst(strCatFirst("\tBeginFunc‬‬\n",tree->right->right->left->SCOPE),"EndFunc\n"),NULL,NULL,NULL,NULL);
		else
		    addScopeFirst(tree,strCatFirst(strCatFirst("\tBeginFunc‬‬\n",tree->right->right->left->SCOPE),"\tEndFunc\n"),NULL,NULL,NULL,NULL);
		}
		else
			 addScopeFirst(tree,strCatFirst("\tBeginFunc‬‬\n","\tEndFunc\n"),NULL,NULL,NULL,NULL);
		
		return;
	}
    else if(strcmp(tree->token, "Main") == 0)
	{ 
		 
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		
		addScopeFirst(tree,strCatFirst(" Main:\n",tree->left->right->SCOPE),NULL,NULL,NULL,NULL);
          return;   
    } 
	    else if(strcmp(tree->token, "procedures") == 0)
	{ if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		if(tree->left!=NULL) addScopeFirst(tree,strCatFirst(tree->left->SCOPE,tree->right->SCOPE),NULL,NULL,NULL,NULL);else addScopeFirst(tree,tree->right->SCOPE,NULL,NULL,NULL,NULL);
    return;
	}        

	else if(strcmp(tree->token, "return") == 0)
	{
		
		 if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);

		addScopeFirst(tree,strCatFirst(tree->left->SCOPE,Gen("return",tree->left->var,"","","")),NULL,NULL,NULL,NULL);
		return;
	}
	else if(strcmp(tree->token, "{") == 0)
	{ 
		if(tree->right->right->left) addScopeFirst(tree,NULL,NULL,tree->right->right->left->label,tree->right->right->left->trueLab,tree->right->right->left->falseLab); 
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		if(tree->right->right->left) addScopeFirst(tree,tree->right->right->left->SCOPE,tree->right->right->left->var,NULL,NULL,NULL); 
		return;			
	}
	else if(strcmp(tree->token, "}") == 0)
	{ if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
                      
                      
    }
	else if(strcmp(tree->token, "assmnt_stmnt") == 0)
	{ if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
                     addScopeFirst(tree,tree->left->SCOPE,tree->left->var,tree->left->label,tree->left->trueLab,tree->left->falseLab); 
					 return;
                      
    }
	
    else if(strcmp(tree->token, "+") == 0 || 
            strcmp(tree->token, "*") == 0 || 
            strcmp(tree->token, "-") == 0 || 
            strcmp(tree->token, "/") == 0 )
	{ 
		
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		addScopeFirst(tree,NULL,freshVar(),NULL,NULL,NULL);
		addScopeFirst(tree,strCatFirst(strCatFirst(tree->left->SCOPE,tree->right->SCOPE),Gen(tree->var,"=",tree->left->var,tree->token,tree->right->var)),NULL,NULL,NULL,NULL);
    return;}
    
	else if(strcmp(tree->token, "&") == 0)
	{ 
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
				if((tree->left->left == NULL))
				addScopeFirst(tree,"",strCatFirst("&",(tree->left->token)),NULL,NULL,NULL);
			else if(strcmp(tree->left->left->token,"[")==0)
					{
						char *x,*fv1,*fv2;
						asprintf(&fv1,"%s",freshVar()); 
						asprintf(&fv2,"%s",freshVar());
						asprintf(&x,"\t%s = &%s\n\t%s = %s + %s\n",fv1,tree->left->token,fv2,fv1,tree->left->left->left->var);
						addScopeFirst(tree,strCatFirst(tree->left->left->left->SCOPE,x),fv2,NULL,NULL,NULL);
					}
				else if (tree->left->left->left==NULL)
				addScopeFirst(tree,"",strCatFirst("&",(tree->left->left->token)),NULL,NULL,NULL);
			else
			{
				char *x,*fv1,*fv2;
				asprintf(&fv1,"%s",freshVar());
				asprintf(&fv2,"%s",freshVar()); 
				asprintf(&x,"\t%s = &%s\n\t%s = %s + %s\n",fv1,tree->left->left->token,fv2,fv1,tree->left->left->left->left->var); 
				addScopeFirst(tree,strCatFirst(tree->left->left->left->left->SCOPE,x),fv2,NULL,NULL,NULL);
			}
			
			

	return;}
	else if(strcmp(tree->token, "^") == 0 )
	{ 
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
			if((tree->left->left == NULL))
				addScopeFirst(tree,"",strCatFirst("*",(tree->left->token)),NULL,NULL,NULL);
			else
			{
				addScopeFirst(tree,"",strCatFirst("*",(tree->left->left->token)),NULL,NULL,NULL);
			}
			
	return;}
	else if(strcmp(tree->token, "==") == 0 || 
			strcmp(tree->token, ">") == 0 || 
			strcmp(tree->token, ">=") == 0 || 
			strcmp(tree->token, "<") == 0 || 
			strcmp(tree->token, "<=") == 0 || 
			strcmp(tree->token, "!=") == 0) 
	{ 

		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
				addScopeFirst(tree,strCatFirst(strCatFirst(tree->left->SCOPE,tree->right->SCOPE),strCatFirst(Gen("if",tree->left->var,tree->token,tree->right->var,strCatFirst("goto ",strCatFirst(tree->trueLab,"\n")))
				,strCatFirst("\tgoto ",strCatFirst(tree->falseLab,"\n")))),NULL,NULL,NULL,NULL);

				
	return;}
	else if(strcmp(tree->token, "(") == 0)
	{
			addScopeFirst(tree->left,NULL,NULL,NULL,tree->trueLab,tree->falseLab);
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		addScopeFirst(tree,tree->left->SCOPE,tree->left->var,NULL,NULL,NULL);
	return;}
	else if(strcmp(tree->token, "!") == 0)
	{ 
		addScopeFirst(tree->left,NULL,NULL,NULL,tree->trueLab,tree->falseLab);
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
				
		 addScopeFirst(tree,tree->left->SCOPE,NULL,NULL,NULL,NULL);
		
	return;}
	else if(strcmp(tree->token, "||") == 0) 
	{
		addScopeFirst(tree->left,NULL,NULL,NULL,tree->trueLab,NULL);
		addScopeFirst(tree->right,NULL,NULL,NULL,tree->trueLab,tree->falseLab);
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		addScopeFirst(tree,strCatFirst(tree->left->SCOPE,strCatFirst(addSpaces(tree->left->falseLab),tree->right->SCOPE)),NULL,NULL,NULL,NULL);
	return;}
	else if(strcmp(tree->token, "&&") == 0 )
	{
		
		addScopeFirst(tree->left,NULL,NULL,NULL,NULL,tree->falseLab);
		addScopeFirst(tree->right,NULL,NULL,NULL,tree->trueLab,tree->falseLab);
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
			addScopeFirst(tree,strCatFirst(tree->left->SCOPE,strCatFirst(addSpaces(tree->left->trueLab),tree->right->SCOPE)),NULL,NULL,NULL,NULL);
	return;}
	else if(strcmp(tree->token, "null") == 0 )
	{ 
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		addScopeFirst(tree,"",tree->token,NULL,NULL,NULL);
	return;}	
	else if(strcmp(tree->token, "solovar") == 0 )
	{ 
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
			if(tree->left->left==NULL)
				addScopeFirst(tree,"",tree->left->token,NULL,NULL,NULL);
			else
			{
				char *x,*fv1,*fv2; asprintf(&fv1,"%s",freshVar()); asprintf(&fv2,"%s",freshVar()); asprintf(&x,"\t%s = &%s\n\t%s = %s + %s\n",fv1,tree->left->token,fv2,fv1,tree->left->left->left->var); addScopeFirst(tree,strCatFirst(tree->left->left->left->SCOPE,x),strCatFirst("*",fv2),NULL,NULL,NULL);
			}
			
	return;}
	else if((tree->left!=NULL)&&
			(strcmp(tree->left->token,"INT")==0||
			strcmp(tree->left->token,"HEX")==0||
			strcmp(tree->left->token,"CHAR")==0||
			strcmp(tree->left->token,"REAL")==0||
			strcmp(tree->left->token,"STRING")==0||
			strcmp(tree->left->token,"BOOLEAN")==0))
			{

			if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
			if(strcmp(tree->left->token,"STRING")==0)
			addScopeFirst(tree,"",tree->token,NULL,NULL,NULL);
			else
			if(strcmp(tree->left->token,"BOOLEAN")==0)
			{
			if(strcmp(tree->token,"true")==0 && tree->trueLab!=NULL)	
			addScopeFirst(tree,strCatFirst("goto ",strCatFirst(tree->trueLab,"\n")),tree->token,NULL,NULL,NULL);
			else if(strcmp(tree->token,"false")==0 && tree->falseLab!=NULL)
			addScopeFirst(tree,strCatFirst("goto ",strCatFirst(tree->falseLab,"\n")),tree->token,NULL,NULL,NULL);
			else
			addScopeFirst(tree,tree->token,tree->token,NULL,NULL,NULL);
			}
			else
			addScopeFirst(tree,"",tree->token,NULL,NULL,NULL);
			return;}
	else if(strcmp(tree->token, "") == 0||strcmp(tree->token, " ") == 0)
	{
		
		if(tree->left)
		addScopeFirst(tree->left,NULL,NULL,tree->label,tree->trueLab,tree->falseLab);
		if(tree->right)
		addScopeFirst(tree->right,NULL,NULL,tree->label,tree->trueLab,tree->falseLab);
		if(tree->left!=NULL) make3AC(tree->left); if(tree->right!=NULL) make3AC(tree->right);
		if(tree->left && tree->right)
			addScopeFirst(tree,strCatFirst(tree->left->SCOPE,tree->right->SCOPE),tree->right->var,NULL,NULL,NULL);
		else if(tree->left)
			addScopeFirst(tree,tree->left->SCOPE,tree->left->var,NULL,NULL,NULL);	
		else if(tree->right)
			addScopeFirst(tree,tree->right->SCOPE,tree->right->var,NULL,NULL,NULL);	
	return;
	}
	else
	{
		if (tree->left) 
			make3AC(tree->left);
		
		if (tree->right)
			make3AC(tree->right);
		addScopeFirst(tree,"",tree->token,NULL,NULL,NULL);
		return;
	}
	if (tree->left) 
		make3AC(tree->left);
	if (tree->right)
		make3AC(tree->right);
}