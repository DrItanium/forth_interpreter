: uc pop.s uc ;
: op:NOP 0 ;
: op:ADD 1 ;
: op:SUB 2 ;
: op:MUL 3 ;
: op:DIV 4 ;
: op:MOD 5 ;
: op:NOT 6 ;
: op:MINUS 7 ;
: op:AND 8 ;
: op:OR 9 ;
: op:GREATER_THAN a# ;
: op:LESS_THAN b# ;
: op:XOR c# ;
: op:SHIFT_RIGHT d# ;
: op:SHIFT_LEFT e# ;
: op:POP_REGISTER f# ;
: op:PUSH_REGISTER 10# ;
: op:EQUALS 11# ;
: op:TYPE_VALUE 12# ;
: op:LOAD 13# ; 
: op:STORE 14# ;
: op:POW 15# ;
: op:SET_LOWEST 16# ;
: op:SET_LOWER 17# ;
: op:SET_HIGHER 18# ;
: op:SET_HIGHEST 19# ;
: op:MOVE 1a# ;
: op:SWAP 1b# ;
: nop op:NOP uc ;
: pop.a  op:POP_REGISTER uc ;
: pop.b  010f# uc ;
: pop.ab 010f000f# uc ;
: pop.c  020f# uc ;
: pop.x  040f# uc ;
: pop.t  050f# uc ;
: pop.ta 060f# uc ;
: pop.tb 070f# uc ;
: pop.tx 080f# uc ;
: pop.ip 090f# uc ;
: pop.tip 0a0f# uc ;
: push.a op:PUSH_REGISTER uc ;
: push.b 0110# uc ;
: push.c 0210# uc ;
: push.s 0310# uc ;
: push.x 0410# uc ;
: push.t 0510# uc ;
: push.ta 0610# uc ;
: push.tb 0710# uc ;
: push.tx 0810# uc ;
: push.ip 0910# uc ;
: push.tip 0a10# uc ;
: << 0002100e010f000f# uc ;
: >> 0002100d010f000f# uc ;
: +  00021001010f000f# uc ;
: -  00021002010f000f# uc ;
: *  00021003010f000f# uc ;
: /  00021004010f000f# uc ;
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
