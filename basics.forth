: DISCRIMINANT_SIGNED 0 ;
: DISCRIMINANT_ADDRESS 1 ;
: DISCRIMINANT_FLOATING_POINT 2 ;
: DISCRIMINANT_BOOLEAN 3 ;

: t.signed DISCRIMINANT_SIGNED pop.t ;
: t.address DISCRIMINANT_ADDRESS pop.t ;
: t.fp DISCRIMINANT_FLOATING_POINT pop.t ;
: t.boolean DISCRIMINANT_BOOLEAN pop.t ;
: clear-registers 0 pop.a 0 pop.b 0 pop.c t.signed ;
: op-add pop.a pop.b add push.c ;
: + t.signed op-add ;
: +f t.fp op-add ;
: +u t.address op-add ;

: drop pop.a ;
: dup pop.a push.a push.a ;
: swap pop.a pop.b push.b push.a ;
: over pop.a pop.b push.b push.a push.b ;
: @ pop.a mload push.c ;
: = pop.a pop.b mstore ;
: print.a pop.a type.a ;
: , t.signed print.a ;
: ,f t.fp print.a ;
: ,u t.address print.a ;
: ,b t.boolean print.a ;
