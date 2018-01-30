: uc pop.s uc ;
: OPERATION_NOP 0 ;
: OPERATION_ADD 1 ;
: OPERATION_SUB 2 ;
: OPERATION_MUL 3 ;
: OPERATION_DIV 4 ;
: OPERATION_MOD 5 ;
: OPERATION_NOT 6 ;
: OPERATION_MINUS 7 ;
: OPERATION_AND 8 ;
: OPERATION_OR 9 ;
: OPERATION_GREATER_THAN 10 ;
: OPERATION_LESS_THAN 11 ;
: OPERATION_XOR 12 ;
: OPERATION_SHIFT_LEFT 13 ;
: OPERATION_SHIFT_RIGHT 14 ;
: OPERATION_POP_A 15 ;
: OPERATION_POP_B 16 ;
: OPERATION_POP_C 17 ;
: OPERATION_POP_S 18 ;
: OPERATION_POP_X 19 ;
: OPERATION_POP_T 20 ;
: OPERATION_POP_TA 21 ;
: OPERATION_POP_TB 22 ;
: OPERATION_POP_TX 23 ;
: OPERATION_PUSH_A 24 ;
: OPERATION_PUSH_B 25 ;
: OPERATION_PUSH_C 26 ;
: OPERATION_PUSH_S 27 ;
: OPERATION_PUSH_X 28 ;
: OPERATION_PUSH_T 29 ;
: OPERATION_PUSH_TA 30 ;
: OPERATION_PUSH_TB 31 ;
: OPERATION_PUSH_TX 32 ;
: OPERATION_EQUALS 33 ;
: OPERATION_TYPE_VALUE 34 ;
: OPERATION_LOAD 35 ; 
: OPERATION_STORE 36 ;
: OPERATION_POW 37 ;
: nop OPERATION_NOP uc ;
: pop.t OPERATION_POP_T uc ;
: pop.ta OPERATION_POP_TA uc ;
: pop.tb OPERATION_POP_TB uc ;
: pop.tx OPERATION_POP_TX uc ;
: pop.a OPERATION_POP_A uc ;
: pop.b OPERATION_POP_B uc ;
: pop.c OPERATION_POP_C uc ;
: pop.x OPERATION_POP_X uc ;
: pop.s OPERATION_POP_S uc ;
: push.t OPERATION_PUSH_T uc ;
: push.ta OPERATION_PUSH_TA uc ;
: push.tb OPERATION_PUSH_TB uc ;
: push.tx OPERATION_PUSH_TX uc ;
: push.a OPERATION_PUSH_A uc ;
: push.b OPERATION_PUSH_B uc ;
: push.c OPERATION_PUSH_C uc ;
: push.x OPERATION_PUSH_X uc ;
: push.s OPERATION_PUSH_S uc ;
cache-basic-entries 

: cache-basic-entries ;
: DISCRIMINANT_SIGNED 0 ;
: DISCRIMINANT_ADDRESS 1 ;
: DISCRIMINANT_FLOATING_POINT 2 ;
: DISCRIMINANT_BOOLEAN 3 ;
: + OPERATION_ADD uc ;
: - OPERATION_SUB uc ;
: * OPERATION_MUL uc ;
: / OPERATION_DIV uc ;
: mod OPERATION_MOD uc ;
: not.a OPERATION_NOT uc ;
: minus.a OPERATION_MINUS uc ;
: == OPERATION_EQUALS uc ;
: ** OPERATION_POW uc ;
: mstore OPERATION_STORE uc ;
: mload OPERATION_LOAD uc ;
: type.a OPERATION_TYPE_VALUE uc ;
: and OPERATION_AND uc ;
: < OPERATION_LESS_THAN uc ;
: > OPERATION_GREATER_THAN uc ;
: or OPERATION_OR uc ;
: xor OPERATION_XOR uc ;
: >> OPERATION_SHIFT_RIGHT uc ;
: << OPERATION_SHIFT_RIGHT uc ;

: zero.a 0 pop.a ;
: zero.b 0 pop.b ;
: zero.c 0 pop.c ;
: zero.s 0 pop.s ;
: zero.x 0 pop.x ;

: t.signed DISCRIMINANT_SIGNED pop.t ;
: t.address DISCRIMINANT_ADDRESS pop.t ;
: t.fp DISCRIMINANT_FLOATING_POINT pop.t ;
: t.boolean DISCRIMINANT_BOOLEAN pop.t ;
: ta.signed DISCRIMINANT_SIGNED pop.ta ;
: ta.address DISCRIMINANT_ADDRESS pop.ta ;
: ta.fp DISCRIMINANT_FLOATING_POINT pop.ta ;
: ta.boolean DISCRIMINANT_BOOLEAN pop.ta ;
: tb.signed DISCRIMINANT_SIGNED pop.tb ;
: tb.address DISCRIMINANT_ADDRESS pop.tb ;
: tb.fp DISCRIMINANT_FLOATING_POINT pop.tb ;
: tb.boolean DISCRIMINANT_BOOLEAN pop.tb ;
: tx.signed DISCRIMINANT_SIGNED pop.tx ;
: tx.address DISCRIMINANT_ADDRESS pop.tx ;
: tx.fp DISCRIMINANT_FLOATING_POINT pop.tx ;
: tx.boolean DISCRIMINANT_BOOLEAN pop.tx ;

: zero.ta ta.signed ;
: zero.tb tb.signed ;
: zero.t t.signed ;
: zero.tx tx.signed ;

: clear-registers zero.t zero.a zero.b zero.c zero.x zero.ta zero.tb zero.tx zero.s ;

: load-ab pop.a pop.b ;
: load-ba pop.b pop.a ;
: c-to-a push.c pop.a ;

: + load-ab + push.c ;
: * load-ab * push.c ;

: +f t.fp + ;
: +u t.address + ;
: + t.signed + ;

: *f t.fp * ;
: *u t.address * ;
: * t.signed * ;


: drop pop.a ;
: dup pop.a push.a push.a ;
: swap load-ab push.b push.a ;
: over load-ab push.b push.a push.b ;
: @ pop.a mload push.c ;
: = load-ab mstore ;
: print.a pop.a type.a ;
: , t.signed print.a ;
: ,f t.fp print.a ;
: ,u t.address print.a ;
: ,b t.boolean print.a ;
: op-not pop.a not.a push.c ;
: negate t.signed op-not ;
: negateu t.address op-not ;
: not t.boolean op-not ;

: op-minus pop.a minus.a push.c ;
: minus t.signed op-minus ;
: minusf t.fp op-minus ;

: == load-ab == push.c ;
: != == not ;
: ==u t.address == ;
: ==f t.fp == ;
: ==b t.boolean == ;
: == t.signed == ;

: !=l t.boolean != ;
: !=f t.fp != ;
: !=u t.address != ;
: != t.signed != ;

: zero 0 == ;
: zerou 0 ==u ;
: zerof 0.0 ==f ;

: not-zero 0 != ;
: not-zerou 0 !=u ;
: not-zerof 0.0 !=f ;

: ** load-ba ** push.c ;
: **f t.fp ** ;
: **u t.address ** ;
: ** t.signed ** ;

: - load-ba - push.c ;
: -f t.fp - ;
: -u t.address - ;
: - t.signed - ;

: / load-ba / push.c ;
: /f t.fp / ;
: /u t.address / ;
: / t.signed / ;

: mod load-ba mod push.c ;
: modu t.address mod ;
: modf t.fp mod ;
: mod t.signed mod ;

: > load-ba > push.c ;
: >f t.fp > ;
: >u t.address > ;
: > t.signed > ;

: < load-ba < push.c ;
: <u t.address < ;
: <f t.fp < ;
: < t.signed < ;

: or load-ba or push.c ;
: ors t.signed or ;
: oru t.address or ;
: orf t.fp or ;
: or t.boolean or ;

: >= < not ;
: >=f <f not ;
: >=u <u not ;

: <= > not ;
: <=f >f not ;
: <=u >u not ;


: implies or not ;

: xor load-ba xor push.c ;
: xors t.signed xor ;
: xoru t.address xor ;
: xor t.boolean xor ;

: and load-ba and push.c ;
: ands t.signed and ;
: andu t.address and ;
: and t.boolean and ;

: << load-ba << push.c ;
: <<u t.address << ;
: << t.signed << ;

: >> load-ba >> push.c ;
: >>u t.address >> ;
: >> t.signed >> ;

: @@ pop.a mload mload push.c ;

: rot pop.c pop.b pop.a push.b push.c push.a ;
: -rot rot rot ;

: nip swap drop ;
: tuck swap over ;

: 2dup over over ;
: 2drop drop drop ;

: 3* * * ;
: 3+ + + ;
: minus.c c-to-a minus ;
: minusf.c c-to-a minusf ;
: abs dup 0 < if minus.c then ;
: bye quit ;

clear-registers
