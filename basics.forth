: uc pop.s uc ;

: register:a 0 ;         : register:b 1 ;        : register:c 2 ;       : register:s 3 ;
: register:x 4 ;         : register:t 5 ;        : register:ta 6 ;      : register:tb 7 ;
: register:tx 8 ;        : register:ip 9 ;       : register:tip a# ;

: op:STOP 0 ;            : op:ADD 1 ;            : op:SUB 2 ;           : op:MUL 3 ;
: op:DIV 4  ;            : op:MOD 5 ;            : op:NOT 6 ;           : op:MINUS 7 ;
: op:AND 8  ;            : op:OR 9 ;             : op:GREATER_THAN a# ; : op:LESS_THAN b# ;
: op:XOR c# ;            : op:SHIFT_RIGHT d# ;   : op:SHIFT_LEFT e# ;   : op:POP_REGISTER f# ;
: op:PUSH_REGISTER 10# ; : op:EQUALS 11# ;       : op:TYPE_VALUE 12# ;  : op:LOAD 13# ;
: op:STORE 14# ;         : op:POW 15# ;          : op:SET_LOWEST 16# ;  : op:SET_LOWER 17# ;
: op:SET_HIGHER 18# ;    : op:SET_HIGHEST 19# ;  : op:MOVE 1a# ;        : op:SWAP 1b# ;
: op:POPA 1c# ;          : op:POPB 1d# ;         : op:POPT 1e# ;        : op:PUSHC 1f# ;

: stop     op:STOP uc ;
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
: +        000000001f011d1c# uc ;
: -        000000001f021d1c# uc ;
: *        000000001f031d1c# uc ;
: /        000000001f041d1c# uc ;
: mod      000000001f051d1c# uc ;
: not      00000000001f061c# uc ;
: minus    00000000001f071c# uc ;
: and      000000001f081d1c# uc ;
: or       000000001f091d1c# uc ;
: >        000000001f0a1d1c# uc ;
: <        000000001f0b1d1c# uc ;
: ^        000000001f0c1d1c# uc ;
: >>       000000001f0d1d1c# uc ;
: <<       000000001f0e1d1c# uc ;
: eq       000000001f111d1c# uc ;
: type     000000000000121c# uc ;
: load     00000000001f131c# uc ;
: iload    00001f13201a131c# uc ;
: store    0000000000141d1c# uc ;
: istore   000014201a131d1c# uc ;
: pow      000000001f151d1c# uc ;
: swap.ab  00000000101b1d1c# uc ;
: swap.ba  swap.ab ;
: dup      000000001000101c# uc ;
: 3+       + + ;
: 3-       - - ;
: 3*       * * ;
: 3/       / / ;
: print.a  000000000000121c# uc ;
: swap     0000000101101d1c# uc ;
: over     0110001001101d1c# uc ;
: neq      001f06201b111d1c# uc ;
: implies  001f06201a091d1c# uc ;
: drop     pop.a ;
: nip      swap drop ;
: tuck     swap over ;
: 2dup     over over ;
: 2drop    pop.ab ;
: rot      001f01101c1d020f# uc 0000000000000010# uc ;
: -rot     rot rot ;
: nop      1a# uc ;
: a->b     101a# uc ;
: c->a     021a# uc ;
: cube     1f03021a03101a1c# uc ;
: square   0000001f03101a1c# uc ;

: dataType:SIGNED 0 ;
: dataType:ADDRESS 1 ;
: dataType:FP 2 ;
: dataType:BOOLEAN 3 ;

: @        load ;
: @@       iload ;
: =        store ;
: ==       istore ;

: ,  dataType:SIGNED  print.a ;
: ,f dataType:FP      print.a ;
: ,u dataType:ADDRESS print.a ;
: ,b dataType:BOOLEAN print.a ;

: negate  dataType:SIGNED  not ;
: negateu dataType:ADDRESS not ;
: not     dataType:BOOLEAN not ;

: minusf dataType:FP     minus ;
: minus  dataType:SIGNED minus ;

cache-basic-entries : cache-basic-entries ;

: equ dataType:ADDRESS eq ;
: eqf dataType:FP eq ;
: eqb dataType:BOOLEAN eq ;
: eq  dataType:SIGNED eq ;

: nequ dataType:ADDRESS neq ;
: neqf dataType:FP neq ;
: neqb dataType:BOOLEAN neq ;
: neq dataType:SIGNED neq ;

: zero? 0 eq ;
: zerou? 0 equ ;
: zerof? 0.0 eqf ;

: not-zero? 0 neq ;
: not-zerou? 0 nequ ;
: not-zerof? 0.0 neqf ;

: powf dataType:FP pow ;
: powu dataType:ADDRESS pow ;
: pow dataType:SIGNED pow ;

: +u dataType:ADDRESS + ;
: +f dataType:FP + ;
: + dataType:SIGNED + ;

: -u dataType:ADDRESS - ;
: -f dataType:FP - ;
: - dataType:SIGNED - ;

: /u dataType:ADDRESS / ;
: /f dataType:FP / ;
: / dataType:SIGNED / ;

: modu dataType:ADDRESS mod ;
: modf dataType:FP mod ;
: mod dataType:SIGNED mod ;

: >u dataType:ADDRESS > ;
: >f dataType:FP > ;
: > dataType:SIGNED > ;

: <u dataType:ADDRESS < ;
: <f dataType:FP < ;
: < dataType:SIGNED < ;

: ors dataType:SIGNED or ;
: oru dataType:ADDRESS or ;
: orf dataType:FP or ;
: or dataType:BOOLEAN or ;

: ands dataType:SIGNED and ;
: andu dataType:ADDRESS and ;
: andf dataType:FP and ;
: and dataType:BOOLEAN and ;

: abs dup 0 < if minus then ;
: absf dup 0.0 <f if minusf then ;

: >= < not ;
: >=f <f not ;
: >=u <u not ;

: <= > not ;
: <=f >f not ;
: <=u >u not ;

: ^s dataType:SIGNED ^ ;
: ^u dataType:ADDRESS ^ ;
: ^ dataType:BOOLEAN ^ ;

: <<u dataType:ADDRESS << ;
: << dataType:SIGNED << ;

: >>u dataType:ADDRESS >> ;
: >> dataType:SIGNED >> ;

: nor  001f06201a091d1c# uc ;
: noru dataType:ADDRESS pop.t nor ;
: nors dataType:SIGNED pop.t nor ;
: nor dataType:BOOLEAN pop.t nor ;

: nand 001f06201a081d1c# uc ;
: nandu dataType:ADDRESS pop.t nand ;
: nands dataType:SIGNED pop.t nand ;
: nand dataType:BOOLEAN pop.t nand ;

