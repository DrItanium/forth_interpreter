: bye quit ;
: DISCRIMINANT_SIGNED 0 ;
: DISCRIMINANT_ADDRESS 1 ;
: DISCRIMINANT_FLOATING_POINT 2 ;
: DISCRIMINANT_BOOLEAN 3 ;

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

: clear-registers 0 pop.a 0 pop.b 0 pop.c t.signed ta.signed tb.signed ;

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

clear-registers
