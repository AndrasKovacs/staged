{-# language UnliftedDatatypes, GADTs, MagicHash #-}

module Notes (foo) where

import GHC.Exts hiding (List)

-- data List :: UnliftedType where
--   Nil :: List
--   Cons :: Int# -> List -> List

-- foo :: List -> List
-- foo Nil = Nil
-- foo (Cons x xs) = Cons (x +# 10#) (foo xs)

data Tree :: UnliftedType where
  Leaf :: Int# -> Tree
  Node :: Tree -> Tree -> Tree

foo :: Tree -> Tree
foo (Leaf x) = Leaf (x +# 10#)
foo (Node l r) = Node (foo l) (foo r)


{-

     {offset
       cD3: // global
           if (R2 & 7 != 1) goto cCZ; else goto cCY;
       cCZ: // global
           I64[Sp - 16] = cDd;
           _sCJ::I64 = I64[R2 + 14];
           R2 = P64[R2 + 6];
           I64[Sp - 8] = _sCJ::I64;
           Sp = Sp - 16;
           call Notes.foo_info(R2) returns to cDd, args: 8, res: 8, upd: 8;
       cDd: // global
           Hp = Hp + 24;
           I64[Hp - 16] = Notes.Cons_con_info;
           P64[Hp - 8] = R1;
           I64[Hp] = I64[Sp + 8] + 10;
           R1 = Hp - 14;
           Sp = Sp + 16;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
       cCY: // global
           R1 = Notes.Nil_closure+1;
           call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
     }
 },

rbp      = Sp
r15      = SpLim
r12      = Hp
r13      = BaseReg
r13[856] = HpLim
r13[904] = HpAlloc

.LcD0:
	leaq -16(%rbp),%rax
	cmpq %r15,%rax
	jb .LcD1
.LcD2:
	movq %r14,%rax
	andl $7,%eax
	cmpq $1,%rax
	je .LcCX
.LcCY:
	movq $.LcDc_info,-16(%rbp)
	movq 14(%r14),%rax
	movq 6(%r14),%r14
	movq %rax,-8(%rbp)
	addq $-16,%rbp
	jmp Notes_foo_info
.LcCX:
	leaq Notes_Nil_closure+1(%rip),%rbx
	jmp *(%rbp)
.LcD1:
	leaq Notes_foo_closure(%rip),%rbx
	jmp *-8(%r13)
.LcDc_info:
.LcDc:
	addq $24,%r12
	cmpq 856(%r13),%r12
	ja .LcDn
.LcDm:
	movq $Notes_Cons_con_info,-16(%r12)
	movq %rbx,-8(%r12)
	movq 8(%rbp),%rax
	addq $10,%rax
	movq %rax,(%r12)
	leaq -14(%r12),%rbx
	addq $16,%rbp
	jmp *(%rbp)
.LcDn:
	movq $24,904(%r13)
	jmp stg_gc_unpt_r1




.LcD2:
	movq %r14,%rax
	andl $7,%eax
	cmpq $1,%rax
	je .LcCX
.LcCY:
	movq $.LcDc_info,-16(%rbp)
	movq 14(%r14),%rax
	movq 6(%r14),%r14
	movq %rax,-8(%rbp)
	addq $-16,%rbp
	jmp Notes_foo_info
.LcCX:
	leaq Notes_Nil_closure+1(%rip),%rbx
	jmp *(%rbp)
.LcDc_info:
.LcDm:
	movq $Notes_Cons_con_info,-16(%r12)
	movq %rbx,-8(%r12)
	movq 8(%rbp),%rax
	addq $10,%rax
	movq %rax,(%r12)
	leaq -14(%r12),%rbx
	addq $16,%rbp
	jmp *(%rbp)


foo : List -> List

hp  : rax
arg : rdi
ret : rdi


foo:
  test rdi
  jnz Cons
Nil:
  ret
Cons:
  push [rdi - 1]
  mov rdi [rdi + 7]
  call foo              -- statepoint
  pop rsi
  add rsi 10
  add rax 16
  mov [rax-8] rdi       -- statepoint
  mov [rax-16] rsi
  lea rdi [rax+1]
  ret

-}
