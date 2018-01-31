: uc pop.s uc ;

: register:a 0 ;         : register:b 1 ;        : register:c 2 ;       : register:s 3 ;
: register:x 4 ;         : register:t 5 ;        : register:ta 6 ;      : register:tb 7 ;
: register:tx 8 ;        : register:ip 9 ;       : register:tip a# ;

: op:NOP 0  ;            : op:ADD 1 ;            : op:SUB 2 ;           : op:MUL 3 ;
: op:DIV 4  ;            : op:MOD 5 ;            : op:NOT 6 ;           : op:MINUS 7 ;
: op:AND 8  ;            : op:OR 9 ;             : op:GREATER_THAN a# ; : op:LESS_THAN b# ;
: op:XOR c# ;            : op:SHIFT_RIGHT d# ;   : op:SHIFT_LEFT e# ;   : op:POP_REGISTER f# ;
: op:PUSH_REGISTER 10# ; : op:EQUALS 11# ;       : op:TYPE_VALUE 12# ;  : op:LOAD 13# ;
: op:STORE 14# ;         : op:POW 15# ;          : op:SET_LOWEST 16# ;  : op:SET_LOWER 17# ;
: op:SET_HIGHER 18# ;    : op:SET_HIGHEST 19# ;  : op:MOVE 1a# ;        : op:SWAP 1b# ;
: op:POPA 1c# ;          : op:POPB 1d# ;         : op:POPT 1e# ;        : op:PUSHC 1f# ;

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
: dup      000000001000101c# uc ;
: 3+       011d201a011d1c1e# uc push.c ;
: 3-       021d201a021d1c1e# uc push.c ;
: 3*       031d201a031d1c1e# uc push.c ;
: 3/       041d201a041d1c1e# uc push.c ;
: square   00001f01011a1c1e# uc ;
: cube     01021b01011a1c1e# uc push.c ;
: print.a  0000000000121c1e# uc ;
: swap     0000000101101d1c# uc ;
: over     0110001001101d1c# uc ;

: dataType:SIGNED 0 ;
: dataType:ADDRESS 1 ;
: dataType:FP 2 ;
: dataType:BOOLEAN 3 ;

: swap.ba  swap.ab ;
: drop     pop.a ;
: @        load ;
: =        store ;

: ,  dataType:SIGNED  print.a ;
: ,f dataType:FP      print.a ;
: ,u dataType:ADDRESS print.a ;
: ,b dataType:BOOLEAN print.a ;

: negate  dataType:SIGNED  not ;
: negateu dataType:ADDRESS not ;
: not     dataType:BOOLEAN not ;

: minusf dataType:FP     minus ;
: minus  dataType:SIGNED minus ;

cache-basic-entries
: cache-basic-entries ;
