

#include <stdint.h>

register uint64_t* hp asm ("r12");

typedef struct Tree {
	uint64_t ptr; // tagged pointer, Leaf: 0 tag, Node: 1 tag
} Tree;

typedef struct Node {
	Tree left;
  Tree right;
} Node;

typedef struct Leaf {
  uint64_t field;
} Leaf;

typedef struct Leaf Leaf;

inline Tree leaf(uint64_t field) {
	uint64_t* oldhp = hp;
	hp[0] = field;
	hp += 1;
	return (Tree){(uint64_t) oldhp};
}

inline Tree node(Tree l, Tree r){
	uint64_t* oldhp = hp;
	((Tree*)hp)[0] = l;
	((Tree*)hp)[1] = r;
	hp += 2;
	return (Tree){(uint64_t)oldhp | 1};
}

inline Leaf* toLeaf(Tree t){
	return (Leaf*)(t.ptr);    // 0 last bit
}

inline Node* toNode(Tree t){
	return (Node*)(t.ptr - 1);  // 1 last bit
}

Tree foo (Tree t){
	uint64_t* oldhp = hp;
	uint64_t tag = t.ptr & 1;

	// Leaf
	if (tag == 0) {
		return leaf(toLeaf(t)->field + 10);

  // Node
	} else {
		Node* n = toNode(t);
		Tree l2 = foo(n->left);
		Tree l3 = foo(n->right);
		return node(l2, l3);
	}
}

int main(){}
