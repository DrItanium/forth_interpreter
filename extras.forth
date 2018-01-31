: == load-ab == push.c ;
: != == not ;
: ==u dataType:ADDRESS == ;
: ==f dataType:FP == ;
: ==b dataType:BOOLEAN == ;
: == dataType:SIGNED == ;

: !=l dataType:BOOLEAN != ;
: !=f dataType:FP != ;
: !=u dataType:ADDRESS != ;
: != dataType:SIGNED != ;

: zero 0 == ;
: zerou 0 ==u ;
: zerof 0.0 ==f ;

: not-zero 0 != ;
: not-zerou 0 !=u ;
: not-zerof 0.0 !=f ;

: ** load-ba ** push.c ;
: **f dataType:FP ** ;
: **u dataType:ADDRESS ** ;
: ** dataType:SIGNED ** ;

: - load-ba - push.c ;
: -f dataType:FP - ;
: -u dataType:ADDRESS - ;
: - dataType:SIGNED - ;

: / load-ba / push.c ;
: /f dataType:FP / ;
: /u dataType:ADDRESS / ;
: / dataType:SIGNED / ;

: mod load-ba mod push.c ;
: modu dataType:ADDRESS mod ;
: modf dataType:FP mod ;
: mod dataType:SIGNED mod ;

: > load-ba > push.c ;
: >f dataType:FP > ;
: >u dataType:ADDRESS > ;
: > dataType:SIGNED > ;

: < load-ba < push.c ;
: <u dataType:ADDRESS < ;
: <f dataType:FP < ;
: < dataType:SIGNED < ;

: or load-ba or push.c ;
: ors dataType:SIGNED or ;
: oru dataType:ADDRESS or ;
: orf dataType:FP or ;
: or dataType:BOOLEAN or ;

: >= < not ;
: >=f <f not ;
: >=u <u not ;

: <= > not ;
: <=f >f not ;
: <=u >u not ;


: implies or not ;

: xor load-ba xor push.c ;
: xors dataType:SIGNED xor ;
: xoru dataType:ADDRESS xor ;
: xor dataType:BOOLEAN xor ;

: and load-ba and push.c ;
: ands dataType:SIGNED and ;
: andu dataType:ADDRESS and ;
: and dataType:BOOLEAN and ;

: << load-ba << push.c ;
: <<u dataType:ADDRESS << ;
: << dataType:SIGNED << ;

: >> load-ba >> push.c ;
: >>u dataType:ADDRESS >> ;
: >> dataType:SIGNED >> ;

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
