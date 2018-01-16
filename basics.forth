: DISCRIMINANT_SIGNED 0 ;
: DISCRIMINANT_ADDRESS 1 ;
: DISCRIMINANT_FLOATING_POINT 2 ;
: DISCRIMINANT_BOOLEAN 3 ;
: set.a pop.a ;
: set.b pop.b ;
: set.c pop.c ;
: set.t pop.t ;
: get.a push.a ;
: get.b push.b ;
: get.c push.c ;
; get.t push.t ;

: t.signed DISCRIMINANT_SIGNED set.t ;
: t.address DISCRIMINANT_ADDRESS set.t ;
: t.fp DISCRIMINANT_FLOATING_POINT set.t ;
: clear-registers 0 set.a 0 set.b 0 set.c t.signed ;
: op-add set.a set.b add get.c ;
: + t.signed op-add ;
: +f t.fp op-add ;
: +u t.address op-add ;

