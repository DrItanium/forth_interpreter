
: >= < not ;
: >=f <f not ;
: >=u <u not ;

: <= > not ;
: <=f >f not ;
: <=u >u not ;


: xor load-ba xor push.c ;
: xors dataType:SIGNED xor ;
: xoru dataType:ADDRESS xor ;
: xor dataType:BOOLEAN xor ;

: << load-ba << push.c ;
: <<u dataType:ADDRESS << ;
: << dataType:SIGNED << ;

: >> load-ba >> push.c ;
: >>u dataType:ADDRESS >> ;
: >> dataType:SIGNED >> ;
