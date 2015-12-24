Dynamic language and runtime system.  Check back later as we're writing code now...

```
---------- Source ---------
{
        a = 13;
  b = 7;
  i = 0;
  while(i < 15) {
    b += a * a;
    if(b == 176) {
      b = 10;
    };
    i += 1;
  };
  dump b + 3;
}


========== Scanning/Parsing =========
  Assignment
    Reference
    Expression
      Boolean logic
        Boolean math
          Term
            Factor
              Simple expression
  Assignment
    Reference
    Expression
      Boolean logic
        Boolean math
          Term
            Factor
              Simple expression
  Assignment
    Reference
    Expression
      Boolean logic
        Boolean math
          Term
            Factor
              Simple expression
  If
    Expression
      Boolean logic
        Boolean math
          Term
            Factor
              Simple expression
                Reference
          Term
            Factor
              Simple expression
  Assignment
    Reference
    Expression
      Boolean logic
        Boolean math
          Term
            Factor
              Simple expression
                Reference
              Simple expression
                Reference
  If
    Expression
      Boolean logic
        Boolean math
          Term
            Factor
              Simple expression
                Reference
          Term
            Factor
              Simple expression
  Assignment
    Reference
    Expression
      Boolean logic
        Boolean math
          Term
            Factor
              Simple expression
  Assignment
    Reference
    Expression
      Boolean logic
        Boolean math
          Term
            Factor
              Simple expression
Dump
  Expression
    Boolean logic
      Boolean math
        Term
          Factor
            Simple expression
              Reference
          Factor
            Simple expression

---------- Emitting Instructions ---------
0: load literal: type=integer, value=13
1: store: name='a', id=0
2: load literal: type=integer, value=7
3: store: name='b', id=1
4: load literal: type=integer, value=0
5: store: name='i', id=2
6: label: id=0
7: load literal: type=integer, value=15
8: load: name='i', id=2
9: operator: '<'
10: jump false: id=1
11: load: name='a', id=0
12: load: name='a', id=0
13: operator: '*'
14: load: name='b', id=1
15: operator: '+'
16: store: name='b', id=1
17: load literal: type=integer, value=176
18: load: name='b', id=1
19: operator: '=='
20: jump false: id=3
21: load literal: type=integer, value=10
22: store: name='b', id=1
23: label: id=3
24: load literal: type=integer, value=1
25: load: name='i', id=2
26: operator: '+'
27: store: name='i', id=2
28: jump: id=0
29: label: id=1
30: dump value
30: load literal: type=integer, value=3
31: load: name='b', id=1
32: operator: '+'
Leaders
  0
  6
  11
  21
  23
  29
========== Executing Code =========
LOAD_INT_LIT: value=13
  push: type=integer; value=13
STOR_VAR: id=0
  pop: type=integer; value=13
LOAD_INT_LIT: value=7
  push: type=integer; value=7
STOR_VAR: id=1
  pop: type=integer; value=7
LOAD_INT_LIT: value=0
  push: type=integer; value=0
STOR_VAR: id=2
  pop: type=integer; value=0
LBL: id=0, hit_count=0, loop_pos=0
LOAD_INT_LIT: value=15
  push: type=integer; value=15
LOAD_VAR: id=2
  push: type=integer; value=0
LES
  pop: type=integer; value=0
  pop: type=integer; value=15
  push: type=boolean; value=true
JMP: false, to=1
  pop: type=boolean; value=true
LOAD_VAR: id=0
  push: type=integer; value=13
LOAD_VAR: id=0
  push: type=integer; value=13
MUL
  pop: type=integer; value=13
  pop: type=integer; value=13
  push: type=integer; value=169
LOAD_VAR: id=1
  push: type=integer; value=7
ADD
  pop: type=integer; value=7
  pop: type=integer; value=169
  push: type=integer; value=176
STOR_VAR: id=1
  pop: type=integer; value=176
LOAD_INT_LIT: value=176
  push: type=integer; value=176
LOAD_VAR: id=1
  push: type=integer; value=176
EQL
  pop: type=integer; value=176
  pop: type=integer; value=176
  push: type=boolean; value=true
JMP: false, to=3
  pop: type=boolean; value=true
LOAD_INT_LIT: value=10
  push: type=integer; value=10
STOR_VAR: id=1
  pop: type=integer; value=10
LBL: id=3, hit_count=0, loop_pos=0
LOAD_INT_LIT: value=1
  push: type=integer; value=1
LOAD_VAR: id=2
  push: type=integer; value=0
ADD
  pop: type=integer; value=0
  pop: type=integer; value=1
  push: type=integer; value=1
STOR_VAR: id=2
  pop: type=integer; value=1
JMP: unconditional, to=-1
LBL: id=0, hit_count=1, loop_pos=0
LOAD_INT_LIT: value=15
  push: type=integer; value=15
LOAD_VAR: id=2
  push: type=integer; value=1
LES
  pop: type=integer; value=1
  pop: type=integer; value=15
  push: type=boolean; value=true
JMP: false, to=1
  pop: type=boolean; value=true
LOAD_VAR: id=0
  push: type=integer; value=13
LOAD_VAR: id=0
  push: type=integer; value=13
MUL
  pop: type=integer; value=13
  pop: type=integer; value=13
  push: type=integer; value=169
LOAD_VAR: id=1
  push: type=integer; value=10
ADD
  pop: type=integer; value=10
  pop: type=integer; value=169
  push: type=integer; value=179
STOR_VAR: id=1
  pop: type=integer; value=179
LOAD_INT_LIT: value=176
  push: type=integer; value=176
LOAD_VAR: id=1
  push: type=integer; value=179
EQL
  pop: type=integer; value=179
  pop: type=integer; value=176
  push: type=boolean; value=false
JMP: false, to=3
  pop: type=boolean; value=false
LBL: id=3, hit_count=0, loop_pos=0
LOAD_INT_LIT: value=1
  push: type=integer; value=1
LOAD_VAR: id=2
  push: type=integer; value=1
ADD
  pop: type=integer; value=1
  pop: type=integer; value=1
  push: type=integer; value=2
STOR_VAR: id=2
  pop: type=integer; value=2
JMP: unconditional, to=-1
LBL: id=0, hit_count=2, loop_pos=0
LOAD_INT_LIT: value=15
  push: type=integer; value=15
LOAD_VAR: id=2
  push: type=integer; value=2
LES
  pop: type=integer; value=2
  pop: type=integer; value=15
  push: type=boolean; value=true
JMP: false, to=1
  pop: type=boolean; value=true
LOAD_VAR: id=0
  push: type=integer; value=13
LOAD_VAR: id=0
  push: type=integer; value=13
MUL
  pop: type=integer; value=13
  pop: type=integer; value=13
  push: type=integer; value=169
LOAD_VAR: id=1
  push: type=integer; value=179
ADD
  pop: type=integer; value=179
  pop: type=integer; value=169
  push: type=integer; value=348
STOR_VAR: id=1
  pop: type=integer; value=348
LOAD_INT_LIT: value=176
  push: type=integer; value=176
LOAD_VAR: id=1
  push: type=integer; value=348
EQL
  pop: type=integer; value=348
  pop: type=integer; value=176
  push: type=boolean; value=false
JMP: false, to=3
  pop: type=boolean; value=false
LBL: id=3, hit_count=0, loop_pos=0
LOAD_INT_LIT: value=1
  push: type=integer; value=1
LOAD_VAR: id=2
  push: type=integer; value=2
ADD
  pop: type=integer; value=2
  pop: type=integer; value=1
  push: type=integer; value=3
STOR_VAR: id=2
  pop: type=integer; value=3
JMP: unconditional, to=-1
LBL: id=0, hit_count=3, loop_pos=0
LOAD_INT_LIT: value=15
  push: type=integer; value=15
LOAD_VAR: id=2
  push: type=integer; value=3
LES
  pop: type=integer; value=3
  pop: type=integer; value=15
  push: type=boolean; value=true
JMP: false, to=1
  pop: type=boolean; value=true
LOAD_VAR: id=0
  push: type=integer; value=13
LOAD_VAR: id=0
  push: type=integer; value=13
MUL
  pop: type=integer; value=13
  pop: type=integer; value=13
  push: type=integer; value=169
LOAD_VAR: id=1
  push: type=integer; value=348
ADD
  pop: type=integer; value=348
  pop: type=integer; value=169
  push: type=integer; value=517
STOR_VAR: id=1
  pop: type=integer; value=517
LOAD_INT_LIT: value=176
  push: type=integer; value=176
LOAD_VAR: id=1
  push: type=integer; value=517
EQL
  pop: type=integer; value=517
  pop: type=integer; value=176
  push: type=boolean; value=false
JMP: false, to=3
  pop: type=boolean; value=false
LBL: id=3, hit_count=0, loop_pos=0
LOAD_INT_LIT: value=1
  push: type=integer; value=1
LOAD_VAR: id=2
  push: type=integer; value=3
ADD
  pop: type=integer; value=3
  pop: type=integer; value=1
  push: type=integer; value=4
STOR_VAR: id=2
  pop: type=integer; value=4
JMP: unconditional, to=-1
============ RECORDING ============
LBL: id=0, hit_count=0, loop_pos=0
LOAD_INT_LIT: value=15
  push: type=integer; value=15
LOAD_VAR: id=2
  push: type=integer; value=4
LES
  pop: type=integer; value=4
  pop: type=integer; value=15
  push: type=boolean; value=true
JMP: false, to=1
  pop: type=boolean; value=true
LOAD_VAR: id=0
  push: type=integer; value=13
LOAD_VAR: id=0
  push: type=integer; value=13
MUL
  pop: type=integer; value=13
  pop: type=integer; value=13
  push: type=integer; value=169
LOAD_VAR: id=1
  push: type=integer; value=517
ADD
  pop: type=integer; value=517
  pop: type=integer; value=169
  push: type=integer; value=686
STOR_VAR: id=1
  pop: type=integer; value=686
LOAD_INT_LIT: value=176
  push: type=integer; value=176
LOAD_VAR: id=1
  push: type=integer; value=686
EQL
  pop: type=integer; value=686
  pop: type=integer; value=176
  push: type=boolean; value=false
JMP: false, to=3
  pop: type=boolean; value=false
LBL: id=3, hit_count=0, loop_pos=0
LOAD_INT_LIT: value=1
  push: type=integer; value=1
LOAD_VAR: id=2
  push: type=integer; value=4
ADD
  pop: type=integer; value=4
  pop: type=integer; value=1
  push: type=integer; value=5
STOR_VAR: id=2
  pop: type=integer; value=5
JMP: unconditional, to=-1
============================================
=== Compiling block for AMD64 (LLP) host ===
============================================
  1: [<prolog>]
  2: [mov %rcx, -8(%rbp)]
LBL: id=0
LOAD_INT: value=15; regs=2,8
LOAD_INT_VAR: id=2; regs=2,8
LES_INT: regs=2,8
  3: [mov -8(%rbp), %rax]
  4: [addq $56, %rax]
  5: [mov 0(%rax), %rax]
  6: [cmpq $15, %rax]
JMP: id=1, regs=1,8
  7: [jge]
LOAD_INT_VAR: id=0; regs=1,8
LOAD_INT_VAR: id=0; regs=1,8
MUL_INT: regs=1,8
  8: [mov -8(%rbp), %rbx]
  9: [addq $8, %rbx]
  10: [mov 0(%rbx), %rbx]
  11: [mov -8(%rbp), %r10]
  12: [imuq 8(%r10), %rbx]
LOAD_INT_VAR: id=1; regs=1,7
ADD_INT: regs=1,7
  13: [mov -8(%rbp), %r10]
  14: [addq $32, %r10]
  15: [mov 0(%r10), %r10]
  16: [addq %rbx, %r10]
STOR_INT_VAR: id=1; regs=1,7
  17: [mov -8(%rbp), %rbx]
  18: [addq $32, %rbx]
  19: [mov %r10, 0(%rbx)]
LOAD_INT: value=176; regs=2,7
LOAD_INT_VAR: id=1; regs=2,7
EQL_INT: regs=2,7
  20: [mov -8(%rbp), %rbx]
  21: [addq $32, %rbx]
  22: [mov 0(%rbx), %rbx]
  23: [cmpq $176, %rbx]
JMP: id=3, regs=1,7
  24: [jne]
  25: [<epilog>]
  26: [movq $3, %rax]
LBL: id=3
LOAD_INT: value=1; regs=1,7
LOAD_INT_VAR: id=2; regs=1,7
ADD_INT: regs=1,7
  27: [mov -8(%rbp), %r10]
  28: [addq $56, %r10]
  29: [mov 0(%r10), %r10]
  30: [addq $1, %r10]
STOR_INT_VAR: id=2; regs=0,7
  31: [mov -8(%rbp), %r11]
  32: [addq $56, %r11]
  33: [mov %r10, 0(%r11)]
JMP: id=0, regs=2,6
LBL: id=1
  34: [<epilog>]
  35: [movq $1, %rax]
jump update: id=0; src=311; dest=69
jump update: id=1; src=99; dest=315
jump update: id=3; src=214; dest=261
JIT code: actual_size=358, buffer_size=4096 byte(s)
--------------------------
(Executing machine code...)
============ END RECORDING ============
LBL: id=1, hit_count=0, loop_pos=0
LOAD_INT_LIT: value=3
  push: type=integer; value=3
LOAD_VAR: id=1
  push: type=integer; value=2376
ADD
  pop: type=integer; value=2376
  pop: type=integer; value=3
  push: type=integer; value=2379
DUMP
  pop: type=integer; value=2379
type=integer, value=2379
==========================
ending stack pos=0
```