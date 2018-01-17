: DISCRIMINANT_SIGNED 0 ;
: DISCRIMINANT_ADDRESS 1 ;
: DISCRIMINANT_FLOATING_POINT 2 ;
: DISCRIMINANT_BOOLEAN 3 ;

: t.signed DISCRIMINANT_SIGNED pop.t ;
: t.address DISCRIMINANT_ADDRESS pop.t ;
: t.fp DISCRIMINANT_FLOATING_POINT pop.t ;
: t.boolean DISCRIMINANT_BOOLEAN pop.t ;
: clear-registers 0 pop.a 0 pop.b 0 pop.c t.signed ;

: load-ab pop.a pop.b ;
: load-ba pop.b pop.a ;

: op-add load-ab add push.c ;
: op-mul load-ab mul push.c ;

: + t.signed op-add ;
: +f t.fp op-add ;
: +u t.address op-add ;

: * t.signed op-mul ;
: *f t.fp op-mul ;
: *u t.address op-mul ;

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

: non-zero zero not ;

: op-equals load-ab equals push.c ;
: eq t.signed op-equals ;
: equ t.address op-equals ;
: eqf t.fp op-equals ;
: eql t.boolean op-equals ;

: op-not-equals op-equals not ;
: neq t.signed op-not-equals ;
: nequ t.address op-not-equals ;
: neqf t.fp op-not-equals ;
: neql t.boolean op-not-equals ;

: zero 0 eq ;
: zerou 0 equ ;
: zerof 0.0 eqf ;

: not-zero 0 neq ;
: not-zerou 0 nequ ;
: not-zerof 0.0 neqf ;

: op-pow load-ba pow push.c ;
: ** t.signed op-pow ;
: **f t.fp op-pow ;
: **u t.address op-pow ;

: op-sub load-ba subtract push.c ;
: - t.signed op-sub ;
: -f t.fp op-sub ;
: -u t.address op-sub ;

: op-div load-ba divide push.c ;
: / t.signed op-div ;
: /f t.fp op-div ;
: /u t.address op-div ;

: mod load-ba modulo push.c ;
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
