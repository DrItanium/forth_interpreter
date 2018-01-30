: uc pop.s uc ;
: register:a 0 ;
: register:b 1 ;
: register:c 2 ;
: register:s 3 ;
: register:x 4 ;
: register:t 5 ;
: register:ta 6 ;
: register:tb 7 ;
: register:tx 8 ;
: register:ip 9 ;
: register:tip a# ;
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
: op:POPA 1c# ;
: op:POPB 1d# ;
: op:POPT 1e# ;
: op:PUSHC 1f# ;

: nop      op:NOP uc ;
: pop.a    op:POPA uc ;
: pop.b    op:POPB uc ;
: pop.t    op:POPT uc ;
: push.c   op:PUSHC uc ;
: push.a   op:PUSH_REGISTER uc ;
: pop.ab   0000000000001d1c# uc ;
: pop.ba   0000000000001c1d# uc ;
: pop.c    000000000000020f# uc ;
: pop.x    000000000000040f# uc ;
: pop.ta   000000000000060f# uc ;
: pop.tb   000000000000070f# uc ;
: pop.tx   000000000000080f# uc ;
: pop.ip   000000000000090f# uc ;
: pop.tip  0000000000000a0f# uc ;
: push.b   0000000000000110# uc ;
: push.s   0000000000000310# uc ;
: push.x   0000000000000410# uc ;
: push.t   0000000000000510# uc ;
: push.ta  0000000000000610# uc ;
: push.tb  0000000000000710# uc ;
: push.tx  0000000000000810# uc ;
: push.ip  0000000000000910# uc ;
: push.tip 0000000000000a10# uc ;
: +        0000001f011d1c1e# uc ;
: -        0000001f021d1c1e# uc ;
: *        0000001f031d1c1e# uc ;
: /        0000001f041d1c1e# uc ;
: mod      0000001f051d1c1e# uc ;
: not      000000001f061c1e# uc ;
: minus    000000001f071c1e# uc ;
: and      0000001f081d1c1e# uc ;
: or       0000001f091d1c1e# uc ;
: >        0000001f0a1d1c1e# uc ;
: <        0000001f0b1d1c1e# uc ;
: ^        0000001f0c1d1c1e# uc ;
: >>       0000001f0d1d1c1e# uc ;
: <<       0000001f0e1d1c1e# uc ;
: ==       0000001f111d1c1e# uc ;
: type     0000000000121c1e# uc ;
: load     00000000001f131c# uc ;
: iload    00001f13201a131c# uc ;
: store    0000000000141d1c# uc ;
: istore   000014201a131d1c# uc ;
: pow      0000001f151d1c1e# uc ;
: swap.ab  00000000101b1d1c# uc ;
: swap.ba  swap.ab ;
: drop     pop.a ;
: dup      000000001000101c# uc ;
: 3*       011d201a011d1c1e# uc push.c ;
: square   00001f01011a1c1e# uc ;
: cube     01021b01011a1c1e# uc push.c ;
: muc load uc ;
: iuc uc ;

cache-basic-entries 
: cache-basic-entries ;
: dataType:SIGNED 0 ;
: dataType:ADDRESS 1 ;
: dataType:FLOATING_POINT 2 ;
: dataType:BOOLEAN 3 ;

: zero.a 0 pop.a ;
: zero.b 0 pop.b ;
: zero.c 0 pop.c ;
: zero.s 0 pop.s ;
: zero.x 0 pop.x ;
: zero.ip 0 pop.ip ;

: t.signed dataType:SIGNED pop.t ;
: t.address dataType:ADDRESS pop.t ;
: t.fp dataType:FLOATING_POINT pop.t ;
: t.boolean dataType:BOOLEAN pop.t ;
: ta.signed dataType:SIGNED pop.ta ;
: ta.address dataType:ADDRESS pop.ta ;
: ta.fp dataType:FLOATING_POINT pop.ta ;
: ta.boolean dataType:BOOLEAN pop.ta ;
: tb.signed dataType:SIGNED pop.tb ;
: tb.address dataType:ADDRESS pop.tb ;
: tb.fp dataType:FLOATING_POINT pop.tb ;
: tb.boolean dataType:BOOLEAN pop.tb ;
: tx.signed dataType:SIGNED pop.tx ;
: tx.address dataType:ADDRESS pop.tx ;
: tx.fp dataType:FLOATING_POINT pop.tx ;
: tx.boolean dataType:BOOLEAN pop.tx ;
: tip.signed dataType:SIGNED pop.tip ;
: tip.address dataType:ADDRESS pop.tip ;
: tip.fp dataType:FLOATING_POINT pop.tip ;
: tip.boolean dataType:BOOLEAN pop.tip ;

: zero.ta ta.signed ;
: zero.tb tb.signed ;
: zero.t t.signed ;
: zero.tx tx.signed ;
: zero.tip tip.signed ;

: clear-registers zero.t zero.a zero.b zero.c zero.x zero.ta zero.tb zero.tx zero.ip zero.tip zero.s ;

: c-to-a push.c pop.a ;

: + load-ab + push.c ;
: * load-ab * push.c ;

: +f t.fp + ;
: +u t.address + ;
: + t.signed + ;

: *f t.fp * ;
: *u t.address * ;
: * t.signed * ;


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
