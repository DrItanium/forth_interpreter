( some words ! )
: 0 ( -- 0 ) 0 ; 
: -1 ( -- -1 ) -1 ;
: true ( -- -1 ) -1 ;
: false ( -- 0 ) 0 ;
: space ( -- ) 0x20 emit ;
: CR ( -- ) 0xA emit ;
: 1 ( -- 1 ) 1 ; 
: 2 ( -- 2 ) 2 ; 
: 4 ( -- 4 ) 4 ; 
: 0x1 ( -- 0x1 ) 0x1 ;
: 0x2 ( -- 0x2 ) 0x2 ;
: 0x4 ( -- 0x4 ) 0x4 ;
: 0xFF ( -- 0xFF ) 0xFF ;
: 0xFFFF ( -- 0xFFFF ) 0xFFFF ;
: 2drop ( b a -- ) drop drop ;
: 3drop ( c b a -- ) 2drop drop ;
: -rot ( a b c -- c b a ) rot rot ;
: ?dup ( a -- a a | 0 ) dup if dup then ;
: nip ( a b -- b ) swap drop ;
: tuck ( a b -- b a b ) swap over ;
: 2dup ( a b -- a b a b ) over over ;
( taken from stack computers: Appendix B )
: 1+ ( n1 -- n2 ) 1 + ;
: 1- ( n1 -- n2 ) 1 - ;
: 2+ ( n1 -- n2 ) 2 + ;
: 2- ( n1 -- n2 ) 2 - ;
: 2* ( n1 -- n2 ) 2 * ;
: 2/ ( n1 -- n2 ) 2 / ;
: 4+ ( n1 -- n2 ) 4 + ;
: 4/ ( n1 -- n2 ) 4 / ;
: noop ( -- ) ;
: not ( n1 -- n2 ) invert ;
: between ( n min max -- f ) rot dup -rot swap <= -rot swap >= and ;
( compatibility with most other forth dialects )
: 0< ( n -- f ) 0 < ;
: 0<= ( n -- f ) 0 <= ;
: 0> ( n -- f ) 0 > ;
: 0>= ( n -- f ) 0 >= ;
: 0<> ( n -- f ) 0 <> ;
: 0=  ( n -- f ) 0 = ;
( TODO: add support for within ! )
: under+ ( n1 n2 n3 -- n n2 ) rot + swap ;
: min ( n1 n2 -- n ) 2dup > if swap then drop ;
: max ( n1 n2 -- n ) 2dup < if swap then drop ;
: negate ( n1 -- n2 ) minus ;
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
  u<< ( a b c -- a d )
  or ( a d -- n ) ;
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
    8 u>>
    swap
    1+
    swap
    mstore.byte ;

: mstore.half ( addr value -- )
  2dup
  mstore.quarter
  16 u>>
  swap
  2+
  swap
  mstore.quarter ;

: mstore.full ( addr value -- )
  2dup
  mstore.half
  32 u>>
  swap
  4+
  mstore.half ;


: bitwise-oru ( a b -- c ) or ;
: bitwise-andu ( a b -- c ) and ;
: bitwise-notu ( a b -- c ) not ;

: pow ( a b -- c ) ** ;

: ! ( a var -- ) store.variable ;
: @ ( a -- b ) load.variable ;
: ? ( a -- ) @ . ;
: 0! ( var -- ) 0 swap ! ;
: 1+var ( var -- ) dup @ 1+ swap ! ;

variable enum-index
: enum-index@ ( -- v ) enum-index @ ;
: {enum ( -- 0 0 ) enum-index 0! enum-index@ ;
: enum,  ( n1 -- n2 ) enum-index 1+var enum-index@ ;
: enum} ( n1? -- ) ;
{enum 
( section ids )
*number-variant-code* ! enum,
*word-variant-code* ! enum, 
*native-function-variant-code* ! enum,
*string-variant-code* ! enum, 
*variable-variant-code* !
enum}

: of-type? ( type cv -- flag ) @ swap type-code = ;
: number? ( a -- flag ) *number-variant-code* of-type? ;
: word? ( a -- flag ) *word-variant-code* of-type? ;
: native-function? ( a -- flag ) *native-function-variant-code* of-type? ;
: string? ( a -- flag ) *string-variant-code* of-type? ;
: variable? ( a -- flag ) *variable-variant-code* of-type? ;

: lowerq ( a -- b ) 0xFF bitwise-andu ;
: upperq ( a -- b ) 8 u>> lowerq ;
: lowerh ( a -- b ) 0xFFFF bitwise-andu ;
: upperh ( a -- b ) 16 u>> lowerh ;
: lowerw ( a -- b ) 0xFFFFFFFF bitwise-andu ;
: upperw ( a -- b ) 32 u>> lowerw ;

: {bin ( path -- ) open-binary-file ;
: bin} ( -- ) close-binary-file ;
: bin<< ( a -- ) write-binary-file ;
: bin<<q ( a -- ) dup lowerq bin<< upperq bin<< ;
: bin<<h ( a -- ) dup lowerh bin<<q upperh bin<<q ;
: bin<<w ( a -- ) dup lowerw bin<<q upperw bin<<q ;

\ must always be last in the file 
close-input-file
