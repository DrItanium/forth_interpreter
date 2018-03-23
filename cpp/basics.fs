( some words ! )
: 0 ( -- 0 ) 0 ;
: 1 ( -- 1 ) 1 ;
: 2 ( -- 2 ) 2 ;
: 4 ( -- 4 ) 4 ;
: of-type? ( type code -- flag ) swap type-code ==u ;
: number? ( a -- flag ) *number-variant-code* of-type? ;
: word? ( a -- flag ) *word-variant-code* of-type? ;
: native-function? ( a -- flag ) *native-function-variant-code* of-type? ;
: string? ( a -- flag ) *string-variant-code* of-type? ;
: 2drop ( b a -- ) drop drop ;
: -rot ( a b c -- c b a ) rot rot ;
: ?dup ( a -- a a | 0 ) dup if dup then ;
: nip ( a b -- b ) swap drop ;
: tuck ( a b -- b a b ) swap over ;
( taken from stack computers: Appendix B )
: 1+ ( n1 -- n2 ) 1 +s ;
: 1- ( n1 -- n2 ) 1 -s ;
: 2+ ( n1 -- n2 ) 2 +s ;
: 2- ( n1 -- n2 ) 2 -s ;
: 2* ( n1 -- n2 ) 2 *s ;
: 2/ ( n1 -- n2 ) 2 /s ;
: 4+ ( n1 -- n2 ) 4 +s ;
: <> ( n1 n2 -- flag ) !=u ;
: = ( n1 n2 -- flag ) ==u ;
: nop ( -- ) ;
: between.s ( n min max -- f ) rot dup -rot swap <=s -rot swap >=s &&s ;
: between.u ( n min max -- f ) rot dup -rot swap <=u -rot swap >=u &&u ;


: quit ( -- ) bye ;




( must always be the last word in the file )
close-input-file
