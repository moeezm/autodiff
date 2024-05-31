#include <stdio.h>
#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

typedef struct CompNode CompNode;
typedef struct A A;
// ==== DYNAMIC ARRAY ====
// dynamically resized circular array
// useful for vector/aueue
struct A {
	CompNode** arr;
	int arrsz;
	int n;
	int head;

	// functions
	int (*sz)(A* a);
	CompNode* (*front)(A* a);
	CompNode* (*get)(A* a, int i);
	void (*set)(A* a, int i, CompNode* x);
	CompNode* (*pop_front)(A* a);
	void (*push_back)(A* a, CompNode* x);
};
CompNode* afront(A* a) {
	return a->arr[a->head];
}
int asize(A* a) {
	return a->n;
}
CompNode* aget(A* a, int i) {
	return (a->arr)[(a->head + i)%(a->arrsz)];
}
void aset(A* a, int i, CompNode* x) {
	a->arr[(a->head + i)%(a->arrsz)] = x;
}
CompNode* apopfront(A* a) {
	CompNode* x = aget(a, 0);
	a->head = (a->head+1)%(a->arrsz);
	(a->n)--;
	return x;
}
void apushback(A* a, CompNode* x) {
	if (a->n == a->arrsz) {
		// double array size and copy over
		CompNode** newarr;
		if (a->arrsz == 0) {
			newarr = malloc(sizeof(CompNode*));
			a->arrsz = 1;
		}
		else {
			newarr = malloc(2 * a->arrsz * sizeof(CompNode*));
			a->arrsz *= 2;
		}
		for (int i = 0; i < a->n; i++) {
			newarr[i] = aget(a, i);
		}
		free(a->arr);
		a->arr = newarr;
	}
	aset(a, a->n, x); 
	(a->n)++;
}
void ainit(A* a) {
	a->arr = NULL;
	a->arrsz = 0;
	a->n = 0;
	a->head = 0;
	a->sz = asize;
	a->front = afront;
	a->get = aget;
	a->set = aset;
	a->push_back = apushback;
	a->pop_front = apopfront;
}
// ==== COMPUTATIONAL GRAPH ====
// IDT is "identity" aka just outputs value
enum nodeType {IDT, ADD, SUB, MUL, DIV, EXP, LOG, SIN, COS};

struct CompNode {
	double val;
	double deriv;
	enum nodeType type;
	A children;
	A parents;
	int id;
	int counter;
};

void initCompNode(CompNode* u) {
	u->val = 0;
	u->deriv = 0;
	u->type = IDT;
	ainit(&u->children);
	ainit(&u->parents);
	u->counter = -1;
}

double nodeEval(CompNode* u) {
	switch (u->type) {
		case IDT:
			return u->val;
		case ADD:
			return aget(&u->children, 0)->val + aget(&u->children, 1)->val;
		case SUB:
			return aget(&u->children, 0)->val - aget(&u->children, 1)->val;
		case MUL:
			return aget(&u->children, 0)->val * aget(&u->children, 1)->val;
		case DIV:
			return aget(&u->children, 0)->val / aget(&u->children, 1)->val;
		case EXP:
			return exp(afront(&u->children)->val);
		case LOG:
			return log(afront(&u->children)->val);
		case SIN:
			return sin(afront(&u->children)->val);
		case COS:
			return cos(afront(&u->children)->val);
	}
}

// bool is w.r.t x or w.r.t y
double nodeEvalDeriv(CompNode* u, double x, double y, bool wrtx) {
	switch(u->type) {
		case IDT:
			if (wrtx) return 1; else return 0;
		case ADD:
			return 1;
		case SUB:
			if (wrtx) return 1; else return -1;
		case MUL:
			if (wrtx) return y; else return x;
		case DIV:
			if (wrtx) return 1/y; else return -1*x/(y*y);
		case EXP:
			if (wrtx) return exp(x); else return 0;
		case LOG:
			if (wrtx) return 1/x; else return 0;
		case SIN:
			if (wrtx) return cos(x); else return 0;
		case COS:
			if (wrtx) return -1*sin(x); else return 0;
	}
}

typedef struct CompGraph {
	A inputs;
	A outputs;
} CompGraph;

void initCompGraph(CompGraph* cg) {
	ainit(&cg->inputs);
	ainit(&cg->outputs);
}

void forwardPass(CompGraph* cg) {
	A q;
	ainit(&q);
	// initalize queue with leaves and do topological sort
	for (int i = 0; i < cg->inputs.n; i++) {
		q.push_back(&q, cg->inputs.get(&cg->inputs, i));
	}
	while (q.sz(&q) > 0) {
		CompNode* cn = q.pop_front(&q);
		cn->val = nodeEval(cn);
		for (int i = 0; i < cn->parents.n; i++) {
			CompNode* par = cn->parents.get(&cn->parents, i);
			if (par->counter <= 0) par->counter = par->children.n;
			par->counter--;
			if (par->counter == 0) {
				q.push_back(&q, par);
			}
		}
		if (cn->parents.n == 0) cg->outputs.push_back(&cg->outputs, cn);
	}
}

void backwardPass(CompGraph* cg, CompNode* start) {
	A q;
	ainit(&q);
	// do topo sort backwards
	//for (int i = 0; i < cg->outputs.n; i++) {
	//	CompNode* tmp = cg->outputs.get(&cg->inputs, i);
	//	tmp->deriv = 1;
	//	q.push_back(&q, tmp);
	//}
	start->deriv = 1;
	q.push_back(&q, start);
	while (q.n > 0) {
		CompNode* cn = q.pop_front(&q);
		for (int i = 0; i < cn->children.n; i++) {
			CompNode* child = cn->children.get(&cn->children, i);
			if (child->counter <= 0) child->counter = child->parents.n;
			child->counter--;
			child->deriv += cn->deriv * nodeEvalDeriv(cn, cn->children.get(&cn->children, 0)->val, cn->children.get(&cn->children, 1)->val, i==0);
			if (child->counter == 0) {
				q.push_back(&q, child);
			}
		}
	}
}

void postfixToGraph(char* str, CompGraph* cg, double init_vals[]) {
	// operations +, -, *, -
	// lowercase letter variables 
	// functions E, L, S, C (exp,log,sin,cos)
	// not the most efficient, doesn't reuse intermediate steps
	CompNode* vars[26]; 
	for (int i = 0; i < 26; i++) vars[i] = NULL;
	CompNode* stack[100];
	int sp = 0; // stack pointer
	for (int i = 0; str[i] != '\0'; i++) {
		if (str[i] == ' ') continue;
		printf("%c %d\n", str[i], sp);
		char c = str[i];
		if ('a' <= c && c <= 'z') {
			CompNode* x;
			if (vars[c-'a'] == NULL) {
				x = malloc(sizeof(CompNode));
				initCompNode(x);
				vars[c-'a'] = x;
			}
			x = vars[c-'a'];
			stack[sp++] = x;
		}
		else if (c == '+' || c == '-' || c == '*' || c == '/') {
			CompNode* x = malloc(sizeof(CompNode));
			CompNode* n2 = stack[--sp];
			CompNode* n1 = stack[--sp];
			initCompNode(x);
			x->children.push_back(&x->children, n1);
			x->children.push_back(&x->children, n2);
			n1->parents.push_back(&n1->parents, x);
			n2->parents.push_back(&n2->parents, x);
			switch (c) {
				case '+':
					x->type = ADD;
					break;
				case '-':
					x->type = SUB;
					break;
				case '*':
					x->type = MUL;
					break;
				case '/':
					x->type = DIV;
					break;
			}
			stack[sp++] = x;
		}
		else {
			CompNode* x = malloc(sizeof(CompNode));
			CompNode* n = stack[--sp];
			initCompNode(x);
			x->children.push_back(&x->children, n);
			n->parents.push_back(&n->parents, x);
			switch (c) {
				case 'E':
					printf("ryo\n");
					x->type = EXP;
					break;
				case 'L':
					x->type = LOG;
					break; 
				case 'S':
					x->type = SIN;
					break; 
				case 'C':
					x->type = COS;
					break; 
			}
			stack[sp++] = x;
		}
	}
	printf("hi\n");
	CompNode* res = stack[0];
	for (int i = 0; i < 26; i++) {
		if (vars[i] != NULL) {
			vars[i]->val = init_vals[i];
			cg->inputs.push_back(&cg->inputs, vars[i]);
		}
	}
	printf("poop\n");
	forwardPass(cg);
	printf("%f\n", res->val);
	printf("bocchi\n");
	backwardPass(cg, res);
	for (int i = 0; i < 26; i++) {
		if (vars[i] == NULL) continue;
		printf("partial w.r.t %c is %f\n", (char)i + 'a', vars[i]->deriv);
	}
}

void test_queue() {
	A tst;
	ainit(&tst);
	for (int i = 0; i < 5; i++) tst.push_back(&tst, NULL);
	tst.pop_front(&tst);
	printf("%d\n", tst.sz(&tst));
	printf("%d\n", tst.head); 
}

void simple_postfix_test() {
	char expr[] = "a b +";
	double init_values[26];
	CompGraph cg;
	initCompGraph(&cg);
	init_values[0] = 1; init_values[1] = 2;
	for (int i = 2; i < 26; i++) init_values[i] = 0;
	postfixToGraph(expr, &cg, init_values);
}

int main() {
	// do the function (e^sin(xy))*cos(xy) + x + y at x=2, y=1
	// check against wolframalpha:
	// https://www.wolframalpha.com/input?i=gradient+of+%28e%5Esin%28xy%29%29*cos%28xy%29+%2B+x+%2B+y+at+x%3D2%2C+y%3D1
	//char expr[] = "x E";
	char expr[] = "x y * S E x y * C * x + y +";
	double init_values[26];
	for (int i = 0; i < 26; i++) init_values[0] = 0;
	init_values[23] = 2; init_values[24] = 1;
	CompGraph cg;
	initCompGraph(&cg);
	postfixToGraph(expr, &cg, init_values);
	return 0;
}

// here lie primitive technology (i.e. pre postfixToGraph)
void simple_autodiff_test() {
	// sin(x)
	CompGraph cg;
	initCompGraph(&cg);
	CompNode x;
	initCompNode(&x);
	x.val = 0;
	CompNode ex;
	initCompNode(&ex);
	ex.type = SIN;

	printf("inited\n");
	printf("%d %d %d\n", cg.inputs.sz(&cg.inputs), x.parents.sz(&x.parents), ex.children.sz(&ex.children));
	printf("passed\n");
	cg.inputs.push_back(&cg.inputs, &x);
	x.parents.push_back(&x.parents, &ex);
	ex.children.push_back(&ex.children, &x);
	printf("pushed\n");
	forwardPass(&cg);
	backwardPass(&cg, &ex);
	printf("%f\n", ex.val);
	printf("%f\n", x.deriv);
}
void long_autodiff_test() {
	// do the function (e^sin(xy))*cos(xy) + x + y at x=2, y=1
	// check against wolframalpha:
	// https://www.wolframalpha.com/input?i=gradient+of+%28e%5Esin%28xy%29%29*cos%28xy%29+%2B+x+%2B+y+at+x%3D2%2C+y%3D1
	CompNode x;
	CompNode y;
	CompNode xy;
	CompNode sinxy;
	CompNode cosxy;
	CompNode esinxy;
	CompNode esinxycosxy;
	CompNode penultimate; // (e^sin(xy))*cos(xy) + x
	CompNode final;
	initCompNode(&x);
	initCompNode(&y);
	initCompNode(&xy);
	initCompNode(&sinxy);
	initCompNode(&cosxy);
	initCompNode(&esinxy);
	initCompNode(&esinxycosxy);
	initCompNode(&penultimate);
	initCompNode(&final);

	xy.type = MUL;
	sinxy.type = SIN;
	cosxy.type = COS;
	esinxy.type = EXP;
	esinxycosxy.type = MUL;
	penultimate.type = ADD;
	final.type = ADD;

	CompGraph cg;
	initCompGraph(&cg);

	cg.inputs.push_back(&cg.inputs, &x);
	cg.inputs.push_back(&cg.inputs, &y);
	x.parents.push_back(&x.parents, &xy);
	y.parents.push_back(&y.parents, &xy);

	x.parents.push_back(&x.parents, &penultimate);
	y.parents.push_back(&y.parents, &final);

	xy.parents.push_back(&xy.parents, &sinxy);
	xy.parents.push_back(&xy.parents, &cosxy);
	
	sinxy.parents.push_back(&sinxy.parents, &esinxy);

	cosxy.parents.push_back(&cosxy.parents, &esinxycosxy);
	esinxy.parents.push_back(&esinxy.parents, &esinxycosxy);

	esinxycosxy.parents.push_back(&esinxycosxy.parents, &penultimate);
	penultimate.parents.push_back(&penultimate.parents, &final);

	xy.children.push_back(&xy.children, &x);
	xy.children.push_back(&xy.children, &y);

	sinxy.children.push_back(&sinxy.children, &xy);
	cosxy.children.push_back(&cosxy.children, &xy);

	esinxy.children.push_back(&esinxy.children, &sinxy);

	esinxycosxy.children.push_back(&esinxycosxy.children, &esinxy);
	esinxycosxy.children.push_back(&esinxycosxy.children, &cosxy);

	penultimate.children.push_back(&penultimate.children, &esinxycosxy);
	penultimate.children.push_back(&penultimate.children, &x);

	final.children.push_back(&final.children, &penultimate);
	final.children.push_back(&final.children, &y);

	x.val = 2;
	y.val = 1;
	printf("initalized\n");
	forwardPass(&cg);
	printf("%f\n", final.val);

	backwardPass(&cg, &final);
	printf("%f\n", x.deriv);
	printf("%f\n", y.deriv);
}
