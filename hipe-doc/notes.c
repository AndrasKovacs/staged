
#include <stdint.h>

struct Tree {
	uint64_t ptr; // tagged pointer, Leaf: 0 tag, Node: 1 tag
};

typedef struct Tree Tree;

struct Node {
	Tree left;
  Tree right;
};

typedef struct Node Node;

struct Leaf {
  uint64_t field;
};

typedef struct Leaf Leaf;

struct Res{
	Tree  res;
	uint64_t* hp;
};

typedef struct Res Res;

inline Res leaf(uint64_t* hp, uint64_t field) {
	hp[0] = field;
	Res res = {{(uint64_t) hp}, hp + 1};
	return res;
}

inline Res node(uint64_t* hp, Tree l, Tree r){
	((Tree*)hp)[0] = l;
	((Tree*)hp)[1] = r;
	Res res = {{(uint64_t)hp | 1}, hp + 2};
	return res;
}

inline Leaf* toLeaf(Tree t){
	return (Leaf*)(t.ptr);    // 0 last bit
}

inline Node* toNode(Tree t){
	return (Node*)(t.ptr - 1);  // 1 last bit
}

Res foo (uint64_t* hp, Tree t){
	uint64_t tag = t.ptr & 1;

	// Leaf
	if (tag == 0) {
		return leaf(hp, toLeaf(t)->field + 10);

  // Node
	} else {
		Node* n = toNode(t);
		Res r1  = foo(hp, n->left);
		Res r2  = foo(r1.hp, n->right);
		return node(r2.hp, r1.res, r2.res);
	}
}

int main(){}
