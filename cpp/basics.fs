( some words ! )
: 0 ( -- 0 ) 0 ; : 0u ( -- 0u ) 0u ;
: 1 ( -- 1 ) 1 ; : 1u ( -- 1u ) 1u ;
: 2 ( -- 2 ) 2 ; : 2u ( -- 2u ) 2u ;
: 4 ( -- 4 ) 4 ; : 4u ( -- 4u ) 4u ;
: 2drop ( b a -- ) drop drop ;
: -rot ( a b c -- c b a ) rot rot ;
: ?dup ( a -- a a | 0 ) dup if dup then ;
: nip ( a b -- b ) swap drop ;
: tuck ( a b -- b a b ) swap over ;
: 2dup ( a b -- a b a b ) over over ;
( taken from stack computers: Appendix B )
: + ( n1 n2 -- n3 ) +s ;
: - ( n1 n2 -- n3 ) -s ;
: * ( n1 n2 -- n3 ) *s ;
: / ( n1 n2 -- n3 ) /s ;
: mod ( n1 n2 -- n3 ) %s ;
: modu ( n1 n2 -- n3 ) %u ;
: < ( n1 n2 -- f ) <s ;
: <= ( n1 n2 -- f ) <=s ;
: > ( n1 n2 -- f ) >s ;
: >= ( n1 n2 -- f ) >=s ;
: = ( n1 n2 -- f ) ==s ;
: <> ( n1 n2 -- f ) !=s ;
: u< ( u1 u2 -- f ) <u ;
: u<= ( u1 u2 -- f ) <=u ;
: u> ( u1 u2 -- f ) >u ;
: u>= ( u1 u2 -- f ) >=u ;

: 1+ ( n1 -- n2 ) 1 + ;
: 1- ( n1 -- n2 ) 1 - ;
: 2+ ( n1 -- n2 ) 2 + ;
: 2- ( n1 -- n2 ) 2 - ;
: 2* ( n1 -- n2 ) 2 * ;
: 2/ ( n1 -- n2 ) 2 / ;
: 4+ ( n1 -- n2 ) 4 + ;
: nop ( -- ) ;
: between.s ( n min max -- f ) rot dup -rot swap <=s -rot swap >=s &&s ;
: between.u ( n min max -- f ) rot dup -rot swap <=u -rot swap >=u &&u ;
: between ( n min max -- f ) between.s ;
( compatibility with most other forth dialects )
: 0< ( n -- f ) 0 < ;
: 0<= ( n -- f ) 0 <= ;
: 0> ( n -- f ) 0 > ;
: 0>= ( n -- f ) 0 >= ;
: 0<> ( n -- f ) 0 <> ;
: 0=  ( n -- f ) 0 = ;
( TODO: add support for within ! )

: under+ ( n1 n2 n3 -- n n2 ) rot + swap ;
: min-or-max-eliminate ( n1 n2 cond -- n ) if swap then drop ;
: min ( n1 n2 -- n ) 2dup > min-or-max-eliminate ;
: max ( n1 n2 -- n ) 2dup < min-or-max-eliminate ;

: quit ( -- ) bye ;


: of-type? ( type code -- flag ) swap type-code ==u ;
: number? ( a -- flag ) *number-variant-code* of-type? ;
: word? ( a -- flag ) *word-variant-code* of-type? ;
: native-function? ( a -- flag ) *native-function-variant-code* of-type? ;
: string? ( a -- flag ) *string-variant-code* of-type? ;


( must always be the last word in the file )
close-input-file
