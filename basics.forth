( take advantage of forth to build up a system :D )
: uc pop.s uc ;

( op codes :D )
: op:STOP 0 ;            : op:ADD 1 ;            : op:SUB 2 ;           : op:MUL 3 ;
: op:DIV 4  ;            : op:MOD 5 ;            : op:NOT 6 ;           : op:MINUS 7 ;
: op:AND 8  ;            : op:OR 9 ;             : op:GREATER_THAN a# ; : op:LESS_THAN b# ;
: op:XOR c# ;            : op:SHIFT_RIGHT d# ;   : op:SHIFT_LEFT e# ;   : op:POP_REGISTER f# ;
: op:PUSH_REGISTER 10# ; : op:EQUALS 11# ;       : op:TYPE_VALUE 12# ;  : op:LOAD 13# ;
: op:STORE 14# ;         : op:POW 15# ;          : op:SET_LOWEST 16# ;  : op:SET_LOWER 17# ;
: op:SET_HIGHER 18# ;    : op:SET_HIGHEST 19# ;  : op:MOVE 1a# ;        : op:SWAP 1b# ;
: op:POPA 1c# ;          : op:POPB 1d# ;         : op:POPT 1e# ;        : op:PUSHC 1f# ;
: op:POPC 20# ;          : op:PUSHA 21# ;        : op:PUSHB 22# ;

: not      00000000001f061c# uc ;
: minus    00000000001f071c# uc ;
: type     000000000000121c# uc ;
: load     000000001f00131c# uc ;
: iload    001f13201a00131c# uc ;
: store    0000000010141d1c# uc ;
: istore   1014201a00131d1c# uc ;
: swap.ab  00000000101b1d1c# uc ;
: swap.ba  swap.ab ;
: print.a  000000000000121c# uc ;

: @        load ;
: @@       iload ;
: =        store ;
: ==       istore ;

: ,  dataType:SIGNED  pop.t print.a ;
: ,f dataType:FP      pop.t print.a ;
: ,u dataType:ADDRESS pop.t print.a ;
: ,b dataType:BOOLEAN pop.t print.a ;

: negate  dataType:SIGNED  pop.t not ;
: negateu dataType:ADDRESS pop.t not ;
: not     dataType:BOOLEAN pop.t not ;

: minusf dataType:FP     pop.t minus ;
: minus  dataType:SIGNED pop.t minus ;

: equ dataType:ADDRESS pop.t eq ;
: eqf dataType:FP pop.t eq ;
: eqb dataType:BOOLEAN pop.t eq ;
: eq  dataType:SIGNED pop.t eq ;

: nequ dataType:ADDRESS pop.t neq ;
: neqf dataType:FP pop.t neq ;
: neqb dataType:BOOLEAN pop.t neq ;
: neq dataType:SIGNED pop.t neq ;

: zero? 0 eq ;
: zerou? 0 equ ;
: zerof? 0.0 eqf ;

: not-zero? 0 neq ;
: not-zerou? 0 nequ ;
: not-zerof? 0.0 neqf ;

: powf dataType:FP pow ;
: powu dataType:ADDRESS pow ;
: pow dataType:SIGNED pow ;

: +u dataType:ADDRESS pop.t + ;
: +f dataType:FP pop.t + ;
: + dataType:SIGNED pop.t + ;

: -u dataType:ADDRESS pop.t - ;
: -f dataType:FP pop.t - ;
: - dataType:SIGNED pop.t - ;

: /u dataType:ADDRESS pop.t / ;
: /f dataType:FP pop.t / ;
: / dataType:SIGNED pop.t / ;

: modu dataType:ADDRESS pop.t mod ;
: modf dataType:FP pop.t mod ;
: mod dataType:SIGNED pop.t mod ;

: >u dataType:ADDRESS pop.t > ;
: >f dataType:FP pop.t > ;
: > dataType:SIGNED pop.t > ;

: <u dataType:ADDRESS pop.t < ;
: <f dataType:FP pop.t < ;
: < dataType:SIGNED pop.t < ;

: ors dataType:SIGNED pop.t or ;
: oru dataType:ADDRESS pop.t or ;
: orf dataType:FP pop.t or ;
: or dataType:BOOLEAN pop.t or ;

: ands dataType:SIGNED pop.t and ;
: andu dataType:ADDRESS pop.t and ;
: andf dataType:FP pop.t and ;
: and dataType:BOOLEAN pop.t and ;

: abs dup 0 < if minus then ;
: absf dup 0.0 <f if minusf then ;

: >= < not ;
: >=f <f not ;
: >=u <u not ;

: <= > not ;
: <=f >f not ;
: <=u >u not ;

: ^s dataType:SIGNED pop.t ^ ;
: ^u dataType:ADDRESS pop.t ^ ;
: ^ dataType:BOOLEAN pop.t ^ ;

: <<u dataType:ADDRESS pop.t << ;
: << dataType:SIGNED pop.t << ;

: >>u dataType:ADDRESS pop.t >> ;
: >> dataType:SIGNED pop.t >> ;

: nor  001f06201a091d1c# uc ;
: noru dataType:ADDRESS pop.t nor ;
: nors dataType:SIGNED pop.t nor ;
: nor dataType:BOOLEAN pop.t nor ;

: nand 001f06201a081d1c# uc ;
: nandu dataType:ADDRESS pop.t nand ;
: nands dataType:SIGNED pop.t nand ;
: nand dataType:BOOLEAN pop.t nand ;

