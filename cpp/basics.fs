( some words ! )
: 0 ( -- 0 ) 0 ; : 0u ( -- 0u ) 0u ;
: -1 ( -- -1 ) -1 ;
: true ( -- -1 ) -1 ;
: false ( -- 0 ) 0 ;
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
: + ( n1 n2 -- n3 ) +.s ;
: - ( n1 n2 -- n3 ) -.s ;
: * ( n1 n2 -- n3 ) *.s ;
: / ( n1 n2 -- n3 ) /.s ;
: mod ( n1 n2 -- n3 ) %.s ;
: modu ( n1 n2 -- n3 ) %.u ;
: < ( n1 n2 -- f ) <.s ;
: <= ( n1 n2 -- f ) <=.s ;
: > ( n1 n2 -- f ) >.s ;
: >= ( n1 n2 -- f ) >=.s ;
: = ( n1 n2 -- f ) ==.s ;
: <> ( n1 n2 -- f ) !=.s ;
: u< ( u1 u2 -- f ) <.u ;
: u<= ( u1 u2 -- f ) <=.u ;
: u> ( u1 u2 -- f ) >.u ;
: u>= ( u1 u2 -- f ) >=.u ;
: 1+ ( n1 -- n2 ) 1 + ;
: 1- ( n1 -- n2 ) 1 - ;
: 2+ ( n1 -- n2 ) 2 + ;
: 2- ( n1 -- n2 ) 2 - ;
: 2* ( n1 -- n2 ) 2 * ;
: 2/ ( n1 -- n2 ) 2 / ;
: 4+ ( n1 -- n2 ) 4 + ;
: 4/ ( n1 -- n2 ) 4 / ;
: nop ( -- ) ;
: and ( n1 n2 -- n3 ) &&.b ;
: or ( n1 n2 -- n3 ) ||.b ;
: xor ( n1 n2 -- n3 ) ^.b ;
: not ( n1 -- n2 ) complement.u ;
: between ( n min max -- f ) rot dup -rot swap <= -rot swap >= and ;
( compatibility with most other forth dialects )
: 0< ( n -- f ) 0 < ;
: 0<= ( n -- f ) 0 <= ;
: 0> ( n -- f ) 0 > ;
: 0>= ( n -- f ) 0 >= ;
: 0<> ( n -- f ) 0 <> ;
: 0=  ( n -- f ) 0 = ;
( TODO: add support for within ! )
: << ( n c -- v ) <<.s ;
: <<u ( n c -- v ) <<.u ;
: >> ( n c -- v )  >>.s ;
: >>u ( n c -- v ) >>.u ;
: under+ ( n1 n2 n3 -- n n2 ) rot + swap ;
: min ( n1 n2 -- n ) 2dup > if swap then drop ;
: max ( n1 n2 -- n ) 2dup < if swap then drop ;
: negate ( n1 -- n2 ) minus.s ;
: *sizeof-byte* ( -- 1 ) 1 ;
: bitcount ( n -- v ) *bitwidth* * ;
: quit ( -- ) bye ;

: half-of ( n -- v ) 2/ ;
: quarter-of ( n -- v ) 4/ ;
: *address-plus-offset* ( addr offset -- addr+offset addr )
    over ( addr offset -- addr offset addr )
    +    ( addr offset -- addr offset+addr )
    swap ( addr offset+addr -- offset+addr addr ) ;
: *shift-left-then-bitwise-or* ( a b c -- n )
  <<u ( a b c -- a d )
  |.u ( a d -- n ) ;
( TODO take advantage of constants to get rid of magic constants )
: mload.quarter ( addr -- value )
  1 *address-plus-offset*
  mload.byte
  swap
  mload.byte
  8 *shift-left-then-bitwise-or* ;

: mload.half ( addr -- value )
  2 *address-plus-offset*
  mload.quarter
  swap
  mload.quarter
  16 *shift-left-then-bitwise-or* ;

: mload.full ( addr -- value )
  4 *address-plus-offset*
  mload.half 
  swap
  mload.half
  32 *shift-left-then-bitwise-or* ;

: mstore.quarter ( addr value -- )
    2dup 
    mstore.byte 
    8 >>u
    swap
    1+
    swap
    mstore.byte ;

: mstore.half ( addr value -- )
  2dup
  mstore.quarter
  16 >>u 
  swap
  2+
  swap
  mstore.quarter ;

: mstore.full ( addr value -- )
  2dup
  mstore.half
  32 >>u
  swap
  4+
  mstore.half ;

: of-type? ( type code -- flag ) swap type-code = ;
: number? ( a -- flag ) *number-variant-code* of-type? ;
: word? ( a -- flag ) *word-variant-code* of-type? ;
: native-function? ( a -- flag ) *native-function-variant-code* of-type? ;
: string? ( a -- flag ) *string-variant-code* of-type? ;


( must always be the last word in the file )
close-input-file
