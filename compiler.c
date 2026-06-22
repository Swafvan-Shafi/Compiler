#include<stdio.h>
#include<ctype.h>
#include<string.h>
#include<stdlib.h>
#define MAX 1000
#define WORD_SIZE 4

//--------------------------------FRONTEND---------------------------------
//Lexer
//Parser and AST
//Semantic Analysis
//--------------------------------LEXER------------------------------------


enum State{
    START,
    IN_ID,//identifier
    IN_NUM//number
};


//token
typedef struct{
    char type[20];
    char val[20];
}token;
token tokens[MAX];
int token_count=0;
void emit(token t){
    tokens[token_count++]=t;
}


//keyword check
int keywordlist(char *word){
    if(strcmp(word,"if")==0)return 1;
    if(strcmp(word, "while")==0)return 1;
    if(strcmp(word,"for")==0)return 1;
    if(strcmp(word,"else")==0)return 1;
    if(strcmp(word,"return")==0)return 1;
    return 0;
}


//-----------------------------SYMBOL TABLE----------------------------
//symbol table(otherwise 2 x are treated as seperate variables)


int current_offset = 0;

//user variables
typedef struct {
    char name[20];
    int offset;
}Symbol;
Symbol variables[MAX];
int varcount=0;
int lookup_symbol(char *name){
    for(int i=0;i<varcount;i++){
        if(strcmp(variables[i].name,name)==0){
            return i;
        }
    }
    return -1;
}
void insert_symbol(char *name){
    if(lookup_symbol(name)!=-1)return;
    current_offset=current_offset+WORD_SIZE;
    strcpy(variables[varcount].name,name);
    variables[varcount].offset=current_offset;
    varcount++;   
}
int get_offset_symbol(char *name)
{
    int idx = lookup_symbol(name);
    if(idx == -1) return -1;
    return variables[idx].offset;
}


//compiler variables
typedef struct {
    char name[20];
    int offset;   // [ebp - offset]
}comsymbol;
comsymbol compilersymbol[MAX];
int compilersymbolcount=0;
int lookup_compilersymbol(char *name){
    for(int i=0;i<compilersymbolcount;i++){
        if(strcmp(compilersymbol[i].name,name)==0){
            return i;
        }
    }
    return -1;
}
void insert_compilersymbol(char *name){
    if(lookup_compilersymbol(name)!=-1)return;
    current_offset=current_offset+WORD_SIZE;
    strcpy(compilersymbol[compilersymbolcount].name,name);
    compilersymbol[compilersymbolcount].offset=current_offset;
    compilersymbolcount++;
}
int get_offset_compilersymbol(char *name)
{
    int idx = lookup_compilersymbol(name);
    if(idx == -1) return -1;
    return compilersymbol[idx].offset;
}


//off set finder for both used by code generator
int get_offset(char *name)
{
    int v = lookup_symbol(name);
    if(v != -1)
        return variables[v].offset;

    int t = lookup_compilersymbol(name);
    if(t != -1)
        return compilersymbol[t].offset;

    return -1;
}


//-------------------------PARSER AND AST GENERATION-------------------


//for token managing
int pos=0;
token curr_token(){
    return tokens[pos];
}
void advance_token(){
    pos++;
}


//ast node
typedef struct Node{
    char val[20];
    struct Node* left;
    struct Node* right;
}node;
node* create_node(char *val){
    node*newnode=malloc(sizeof(node));
    strcpy(newnode->val,val);
    newnode->left=NULL;
    newnode->right=NULL;
    return newnode;
}


//building structure(factor,term,expr)

void print_ast(node *root, char *prefix, int is_left, int is_root);
void semantic_check(node *root);
char* generate_TAC(node *root);

node* expr();//(we use expr()in factor function,thats why i write this here)
//level 1(lowest element identifier/digit)
node* factor(){
    token t=curr_token();

    //CASE-1(parenthesis)
    if(strcmp(t.type,"PAREN")==0 && strcmp(t.val,"(")==0){
        advance_token();
        node* n = expr();

        if(strcmp(curr_token().type,"PAREN")==0 && strcmp(curr_token().val,")")==0){
            advance_token();
        }
        return n;
    }

    //CASE-2(numsber or identifier)
    node* newnode=create_node(t.val);
    advance_token();
    return newnode;
}
//level 2 (for multiplication and division)
node *term(){
    node* left=factor();
    while(pos<token_count && ((strcmp(curr_token().type,"MULTIPLY")==0) || (strcmp(curr_token().type,"DIVIDE")==0))){
        token op=curr_token();
        advance_token();

        node* right=factor();
        node *root=create_node(op.val);

        root->left=left;
        root->right=right;
        left=root;
    }
    return left;
}
//level 3(for addition and subtraction)
node* expr(){
    node* left=term();

    if(pos < token_count && strcmp(curr_token().type,"ASSIGN")==0){
        token t=curr_token();
        advance_token();
        node *right=expr();
        node *root = create_node(t.val);
        root->left=left;
        root->right=right;
        return root;
    }

    while(pos<token_count && ((strcmp(curr_token().type,"PLUS")==0) || (strcmp(curr_token().type,"MINUS")==0))){
        token op=curr_token();
        advance_token();

        node* right=term();

        node* root=create_node(op.val);

        root->left=left;
        root->right=right;
        left=root;
    }
    return left;
}
node* statement() {
    node *root = expr();   // parse one expression

    //if it is a newline then make another token
    if(strcmp(curr_token().type, "NEWLINE") == 0)
        advance_token();

    return root;
}
void program() {
    while(pos < token_count) {

        while(pos < token_count &&
              strcmp(curr_token().type, "NEWLINE") == 0)
        {
            advance_token();
        }

        if(pos >= token_count)
            break;

        node *root = statement();

        printf("\nAST:\n");
        print_ast(root,"",0,1);

        printf("\nAnalysing Semantics .....\n");
        semantic_check(root);
        printf("Semantics Succeed\n\n");

        generate_TAC(root);
    }
}

void print_ast(node *root, char *prefix, int is_left, int is_root)
{
    if(root == NULL)
        return;

    printf("%s", prefix);

    if(!is_root)
        printf(is_left ? "L-- " : "R-- ");

    printf("%s\n", root->val);

    char new_prefix[256];
    sprintf(new_prefix, "%s%s", prefix, is_root ? "" : (is_left ? "|   " : "    "));

    print_ast(root->left,  new_prefix, 1, 0);
    print_ast(root->right, new_prefix, 0, 0);
}


//---------------------------semantic analysis-------------------------


void semantic_check(node *root)
{
    if(root == NULL)
        return;

    // Assignment node
    if(strcmp(root->val, "=") == 0)
    {
        // Left side must be identifier
        if(!isalpha(root->left->val[0]))
        {
            printf("Semantic Error: invalid assignment target\n");
            exit(1);
        }

        // Declare variable
        insert_symbol(root->left->val);

        // Check RHS
        semantic_check(root->right);

        return;
    }

    // Identifier usage
    if(isalpha(root->val[0]))
    {
        if(lookup_symbol(root->val) == -1)
        {
            printf("Semantic Error: '%s' not declared\n",root->val);
            exit(1);
        }
    }

    semantic_check(root->left);
    semantic_check(root->right);
}


//------------------------------- MIDDLE-END---------------------------
//TAC generation
//Optimisation(folding,propogation,dead code elimination)

//---------------------------converting AST to TAC---------------------


//we are making tac statements as a linked list
typedef struct TAC{
    char op[10];
    char arg1[20];
    char arg2[20];
    char result[20];
    struct TAC *next;
}TAC;
TAC *head=NULL;
TAC *tail=NULL;
void add_to_tac(char *result, char *op, char *arg1, char *arg2){
    TAC *newnode=malloc(sizeof(TAC));
    strcpy(newnode->arg1,arg1);
    strcpy(newnode->result,result);

    if(op!=NULL){
        strcpy(newnode->op,op);
    }
    else{
        strcpy(newnode->op,"");
    }
    if(arg2!=NULL){
        strcpy(newnode->arg2,arg2);
    }
    else{
        strcpy(newnode->arg2,"");
    }
    newnode->next=NULL;


    if(head==NULL){
        head=newnode;
        tail=newnode;
    }
    else{
        tail->next=newnode;
        tail=tail->next;
    }
}

int temp_count = 1;
// returns variable name / temporary name corresponding to subtree
char* generate_TAC(node *root)
{
    if(root == NULL)
        return NULL;

    // Leaf node (identifier or number)
    if(root->left == NULL && root->right == NULL)
    {
        return root->val;
    }

    // Assignment node
    if(strcmp(root->val, "=") == 0)
    {
        char *rhs = generate_TAC(root->right);

        add_to_tac(root->left->val, "=",rhs, "");

        return root->left->val;
    }
    char *left=generate_TAC(root->left);
    char *right=generate_TAC(root->right);

    char *temp=malloc(20*sizeof(char));

    //instead of printing, it stores to the array temp
    sprintf(temp, "t%d",temp_count++);
    insert_compilersymbol(temp);

    add_to_tac(temp,root->val,left,right);

    return temp;
}

void print_TAC()
{
    TAC *curr = head;

    while(curr != NULL)
    {
        if(strcmp(curr->op, "=") == 0)
        {
            printf("%s = %s\n",
                   curr->result,
                   curr->arg1);
        }
        else
        {
            printf("%s = %s %s %s\n",
                   curr->result,
                   curr->arg1,
                   curr->op,
                   curr->arg2);
        }

        curr = curr->next;
    }
}


//--------------------------------OPTIMISATION---------------------------

//folding
int is_number(char* s){
    if(s[0]=='\0')return 0;
    int i=0;
    if(s[0] == '-' || s[0] == '+')i++;
    if(s[i] == '\0') return 0;
    for(; s[i] != '\0'; i++){
        if(!isdigit(s[i]))
            return 0;
    }
     return 1;   
}
int eval(int a, int b, char op){
    switch(op){
        case '+': return a+b;
        case '-': return a-b;
        case '*': return a*b;
        case '/': return a/b;
    }
    return 0;
}
int opt_fold(TAC *t, char *result){
    if(!is_number(t->arg1) || !is_number(t->arg2)){
        return 0;//cannot fold
    }
    int a=atoi(t->arg1);
    int b=atoi(t->arg2);
    int res=eval(a,b,t->op[0]);
    sprintf(result,"%d",res);
    return 1;
}


//propagation
#define MAX_CONST 100

typedef struct {
    char name[20];
    char value[20];
    int isConst; // 1 = constant, 0 = unknown
} Const_Table;
Const_Table constTable[MAX_CONST];
int constCount = 0;
int get_const_index(char *name){
    for(int i = 0; i < constCount; i++){
        if(strcmp(constTable[i].name, name) == 0)
            return i;
    }
    return -1;
}
char* resolve(char *x){
    int idx = get_const_index(x);
    if(idx != -1 && constTable[idx].isConst)
        return constTable[idx].value;
    return x;
}
void set_const(char *name, char *value, int isConst){
    int idx = get_const_index(name);

    if(idx == -1){
        strcpy(constTable[constCount].name, name);
        strcpy(constTable[constCount].value, value);
        constTable[constCount].isConst = isConst;
        constCount++;
    } else {
        strcpy(constTable[idx].value, value);
        constTable[idx].isConst = isConst;
    }
}
void constant_propagation(TAC *head)
{
    TAC *curr = head;

    while(curr != NULL)
    {
        // STEP 1: Replace RHS operands
        char *a1 = resolve(curr->arg1);
        char *a2 = resolve(curr->arg2);

        strcpy(curr->arg1, a1);
        strcpy(curr->arg2, a2);

        // STEP 2: costant folding
        char result[20];
        if(opt_fold(curr, result)){
            strcpy(curr->arg1,result);
            strcpy(curr->arg2,"");
            strcpy(curr->op,"=");
        }

        // STEP 3: update constant table
        if(strcmp(curr->op,"=")==0){
            set_const(curr->result, curr->arg1, is_number(curr->arg1));
        }
        else{
            set_const(curr->result, curr->result, 0);
        }
        curr=curr->next;
    }
}


//Dead Code Elimination
char live[MAX][20];
int live_count = 0;
int is_live(char *name) {
    for(int i = 0; i < live_count; i++) {
        if(strcmp(live[i], name) == 0)
            return 1;
    }
    return 0;
}
void add_live(char *name) {
    if(name[0] == '\0') return;
    if(is_live(name)) return;
    strcpy(live[live_count++], name);
}
void remove_live(char *name) {
    for(int i = 0; i < live_count; i++) {
        if(strcmp(live[i], name) == 0) {
            for(int j = i; j < live_count - 1; j++)
                strcpy(live[j], live[j + 1]);
            live_count--;
            return;
        }
    }
}
int is_temp(char *s) {
    return (s[0] == 't'); // t1, t2...
}
TAC *DCE(TAC *head){
    if(head == NULL) return NULL;

    // STEP 1: copy to array(to go backwards)
    TAC *arr[MAX];
    int n=0;
    TAC *curr=head;
    while(curr){
        arr[n++]=curr;
        curr=curr->next;
    }

    // STEP 2: Go backwars and check
    int keep[MAX];
    for(int i = 0; i < n; i++){
        keep[i] = 1;
    }
    live_count = 0;
    for(int i=n-1;i>=0;i--){

        TAC *t = arr[i];

        // CASE 1: assignment
        if(strcmp(t->op, "=") == 0) {

            if(!is_live(t->result) && is_temp(t->result)) {
                keep[i] = 0;   // DEAD CODE
                continue;
            }

            remove_live(t->result);
            if(!is_number(t->arg1)) add_live(t->arg1);
        }

        // CASE 2: arithmetic
        else {

            if(!is_live(t->result) && is_temp(t->result)) {
                keep[i] = 0;   // DEAD CODE
                continue;
            }

            remove_live(t->result);
            if(!is_number(t->arg1)) add_live(t->arg1);
            if(!is_number(t->arg2)) add_live(t->arg2);
        }
    }

    // STEP 3: rebuilt
    TAC *new_head = NULL;
    TAC *new_tail = NULL;

    for(int i = 0; i < n; i++) {
        if(keep[i]) {
            if(new_head == NULL) {
                new_head = arr[i];
                new_tail = arr[i];
            } else {
                new_tail->next = arr[i];
                new_tail = arr[i];
            }
        }
    }
    return new_head;
}



//---------------------------------BACKEND-------------------------------
//Register allocation
//real code generation


//register allocation
//did not do graph colouring currently

#define REG_COUNT 5
char *reg[5]={"eax","ebx","ecx","edx","esi"};
char reg_var[5][20];   // which variable is in register
int reg_used[5];
typedef struct {
    char name[20];
    int reg;
} RegMap;
RegMap reg_table[1000];
int reg_count = 0;
int find_mapping(char *var)
{
    for(int i = 0; i < reg_count; i++)
    {
        if(strcmp(reg_table[i].name, var) == 0)
            return i;
    }
    return -1;
}
char* get_reg_name(int idx)
{
    return reg_var[idx];
}
int allocate_reg(char *var)
{
    // already assigned
    int idx = find_mapping(var);
    if(idx != -1)
        return reg_table[idx].reg;

    // find free register
    int r = -1;
    for(int i = 2; i < REG_COUNT; i++)
    {
        if(!reg_used[i])
        {
            r = i;
            break;
        }
    }

    // if no free register → simple spill (overwrite eax)
    if(r == -1)
        r = 2;

    reg_used[r] = 1;
    strcpy(reg_var[r], var);

    // store mapping
    strcpy(reg_table[reg_count].name, var);
    reg_table[reg_count].reg = r;
    reg_count++;

    return r;
}
void register_allocation(TAC *head)
{
    TAC *curr = head;

    while(curr != NULL)
    {
        // assignment: x = y
        if(strcmp(curr->op, "=") == 0)
        {
            allocate_reg(curr->arg1);
            allocate_reg(curr->result);
        }
        else
        {
            allocate_reg(curr->arg1);
            allocate_reg(curr->arg2);
            allocate_reg(curr->result);
        }

        curr = curr->next;
    }
}


//Code generation
int get_reg_index(char *var)
{
    for(int i = 0; i < reg_count; i++)
    {
        if(strcmp(reg_table[i].name, var) == 0)
            return reg_table[i].reg;
    }
    return -1;
}
char* R(int idx)
{
    return reg_var[idx];
}
void generate_x86(TAC *head)
{
    TAC *curr = head;

    printf("section .text\n");
    printf("global _start\n\n");
    printf("_start:\n");

    while(curr != NULL)
    {
        int r1, r2, rd;

        // ---------------- ASSIGNMENT ----------------
        if(strcmp(curr->op, "=") == 0)
        {
            rd = get_reg_index(curr->result);
            if(is_number(curr->arg1)){
                printf("    mov %s, %s\n",R(rd), curr->arg1);
            }
            else{

                r1 = get_reg_index(curr->arg1);

                printf("    mov %s, %s\n",R(rd), R(r1));
            }
        }

        // ---------------- ADD ----------------
        else if(strcmp(curr->op, "+") == 0)
        {
            rd = get_reg_index(curr->result);
            r1 = get_reg_index(curr->arg1);
            r2 = get_reg_index(curr->arg2);

            if(is_number(curr->arg1))
            printf("    mov %s, %s\n", R(rd), curr->arg1);
            else
            printf("    mov %s, %s\n", R(rd), R(r1));

            if(is_number(curr->arg2))
                printf("    add %s, %s\n", R(rd), curr->arg2);
            else
                printf("    add %s, %s\n", R(rd), R(r2));
        }

        // ---------------- SUB ----------------
        else if(strcmp(curr->op, "-") == 0)
        {
            rd = get_reg_index(curr->result);
            r1 = get_reg_index(curr->arg1);
            r2 = get_reg_index(curr->arg2);

           if(is_number(curr->arg1))
            printf("    mov %s, %s\n", R(rd), curr->arg1);
            else
            printf("    mov %s, %s\n", R(rd), R(r1));

            if(is_number(curr->arg2))
            printf("    sub %s, %s\n", R(rd), curr->arg2);
            else
            printf("    sub %s, %s\n", R(rd), R(r2));
        }

        // ---------------- MUL ----------------
        else if(strcmp(curr->op, "*") == 0)
        {
            rd = get_reg_index(curr->result);
            r1 = get_reg_index(curr->arg1);
            r2 = get_reg_index(curr->arg2);

            if(is_number(curr->arg1))
            printf("    mov %s, %s\n", R(rd), curr->arg1);
            else
            printf("    mov %s, %s\n", R(rd), R(r1));

            if(is_number(curr->arg2))
            printf("    imul %s, %s\n", R(rd), curr->arg2);
            else
            printf("    imul %s, %s\n", R(rd), R(r2));
        }

        // ---------------- DIV ----------------
        else if(strcmp(curr->op, "/") == 0)
        {
            r1 = get_reg_index(curr->arg1);
            r2 = get_reg_index(curr->arg2);
            rd = get_reg_index(curr->result);

            if(is_number(curr->arg1))
            printf("    mov eax, %s\n", curr->arg1);
            else
            printf("    mov eax, %s\n", R(r1));

            printf("    cdq\n");

            if(is_number(curr->arg2))
            printf("    mov ebx, %s\n", curr->arg2);
            else
            printf("    mov ebx, %s\n", R(r2));

            printf("    idiv ebx\n");
            printf("    mov %s, eax\n", R(rd));
        }

        curr = curr->next;
    }

    printf("\n    mov eax, 1\n");
    printf("    mov ebx, 0\n");
    printf("    int 0x80\n");
}



//-----------------------------------------------------------------------


int main(int argc,char *argv[]){
    printf("\n\n");
    printf("HI Swafvan, your Programme is Starting: \n\n");
    /*if(argc<2){
        printf("Error: Insert the file\n");
        return 1;
    }*/


    for(int i=0;i<69;i++){
        printf("-");
    }
    printf("\n");
    for(int i=0;i<69;i++){
        printf("-");
    }
    printf("\n");
    printf("\t\t\t\tFRONTEND\n");
    //open the file pointer
    FILE *fp=fopen("input_file.txt","r");
    printf("File opening...\n");
    

    if(fp == NULL){
        printf("Error: File not found\n");
        return 1;
    }
    printf("File openeing Succeed\n\n");

    for(int i=0;i<69;i++){
        printf("-");
    }
    printf("\n");
    printf("LEXING starts: \n\n");
    printf("Reading...\n");
    

    //lexer work  begins
    char ch;
    char buffer[MAX];
    int i=0;
    enum State state=START;

    printf("This are the Tokens: \n");
    while((ch=fgetc(fp)) !=EOF){

        switch(state){
            case START :{
                if(isalpha(ch)){
                    state=IN_ID;
                    buffer[i++]=ch;
                }
                else if(isdigit(ch)){
                    state=IN_NUM;
                    buffer[i++]=ch;
                }
                else if(ch=='=' || ch=='+' || ch=='-' || ch=='*' ||ch=='/'){
                    token t;
                    t.val[0]=ch;
                    t.val[1]='\0';
                    if(ch=='='){
                        strcpy(t.type,"ASSIGN");
                        printf("ASSIGN(=)\n");
                    }
                    else if(ch=='+'){
                        strcpy(t.type,"PLUS");
                        printf("PLUS(+)\n");
                    }
                    else if(ch=='-'){
                        strcpy(t.type,"MINUS");
                        printf("MINUS(-)\n");
                    }
                    else if(ch=='*'){
                        strcpy(t.type,"MULTIPLY");
                        printf("MULTIPLY(*)\n");
                    }
                    else if (ch=='/'){
                        strcpy(t.type,"DIVIDE");
                        printf("DIVIDE(/)\n");
                    }
                    emit(t);
                }
                else if(ch=='(' || ch ==')'){
                    token t;
                    t.val[0]=ch;
                    t.val[1]='\0';
                    strcpy(t.type,"PAREN");
                    printf("PARENTHESIS\n");
                    emit(t);
                }
                else if(ch=='\n'){
                    token t;
                    strcpy(t.val,"//n");
                    strcpy(t.type,"NEWLINE");
                    printf("NEWLINE\n");
                    emit(t);
                }
            }
            break;

            case IN_ID :{
                if(isalnum(ch)){
                    buffer[i++]=ch;
                }
                else{
                    buffer[i]='\0';
                    token t;
                    strcpy(t.val,buffer);
                    //checks if the word is a keyword or not
                    if(keywordlist(t.val)){
                        strcpy(t.type,"KEYWORD");
                        printf("KEYWORD(%s)\n",buffer);
                        emit(t);
                    }
                    else{
                        strcpy(t.type,"IDENTIFIER");
                        printf("IDENTIFIER(%s)\n",buffer);
                        emit(t);
                    }
                    i=0;
                    state=START;

                    ungetc(ch,fp);
                }
            }
            break;

            case IN_NUM :{
                if(isdigit(ch)){
                    buffer[i++]=ch;
                }
                else{
                    buffer[i]='\0';
                    token t;
                    strcpy(t.type,"NUMBER");
                    strcpy(t.val,buffer);
                    printf("NUMBER(%s)\n",buffer);
                    emit(t);
                    i=0;
                    state=START;

                    ungetc(ch,fp);
                }
            }
            break;
        }

    }


    //closing the last element befor EOF 
    //Beacuse EOF != '\0'
    if(state==IN_ID){
        buffer[i]='\0';
        token t;
        strcpy(t.type,"IDENTIFIER");
        strcpy(t.val,buffer);
        emit(t);
    }
    else if(state==IN_NUM){
        buffer[i]='\0';
        token t;
        strcpy(t.type,"NUMBER");
        strcpy(t.val,buffer);
        emit(t);
    }
    printf("Reading Complete\n");

    fclose(fp);
    printf("File closed\n");


    printf("LEXER work completed\n\n");

    for(int i=0;i<69;i++){
        printf("-");
    }
    printf("\nPARSING starts: \n");

    //calling parser
    program();


    for(int i=0;i<69;i++){
        printf("-");
    }
    printf("\n");
    for(int i=0;i<69;i++){
        printf("-");
    }
    printf("\n");
    printf("\t\t\t\tMIDDLE-END\n\n");

    printf("Converting to TAC ....\n\n");
    printf("TAC:\n");
    print_TAC();
    printf("\n");


    for(int i=0;i<69;i++){
        printf("-");
    }

    printf("\n");
    printf("Optimising....\n\n");
    printf("Folding and Propogation starts: \n");
    constant_propagation(head);
    printf("Complete\n");
    printf("TAC after folding and propogation: \n");
    print_TAC();
    printf("\n");

    printf("Dead code elimination starts: \n");
    head=DCE(head);//dead code elimination
    printf("Complete\n");
    printf("TAC AFTER DCE: \n");
    print_TAC();
    printf("\n");

    for(int i=0;i<69;i++){
        printf("-");
    }
    printf("\n");
    for(int i=0;i<69;i++){
        printf("-");
    }
    printf("\n");


    printf("\t\t\t\tBACKEND\n\n");

    printf("Register allocation starts\n");
    register_allocation(head);



    printf("Code generation starts: \n\n");
    printf("\t\t\tThe assembly code is: \n");
    generate_x86(head);
    

    printf("\nComplete\n");
    return 0;
}