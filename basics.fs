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
: between ( n min max -- f ) 
  rot ( min max n )
  swap ( min n max ) 
  over ( min n max n )
  >= ( min n f )
  -rot ( f min n )
  <= ( f f )
  and ( f ) ;
  
( compatibility with most other forth dialects )
: 0< ( n -- f ) 0 < ;
: 0<= ( n -- f ) 0 <= ;
: 0> ( n -- f ) 0 > ;
: 0>= ( n -- f ) 0 >= ;
: 0<> ( n -- f ) 0 <> ;
: 0=  ( n -- f ) 0 = ;
: abs ( n -- u ) dup 0< if minus then ;
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
: q@ ( addr -- value )
  1 *address-plus-offset*
  c@ 
  swap
  c@
  8 *shift-left-then-bitwise-or* ;

: h@ ( addr -- value )
  2 *address-plus-offset*
  q@
  swap
  q@
  16 *shift-left-then-bitwise-or* ;

: w@ ( addr -- value )
  4 *address-plus-offset*
  h@
  swap
  h@
  32 *shift-left-then-bitwise-or* ;

: q! ( value addr -- )
    2dup  ( v a v a )
    c!  ( v a )
    swap 8 u>> ( a v>>8 )
    swap ( v>>8 a )
    1+ ( v>>8 a+1 )
    c! ;
: h! ( v a -- )
  2dup ( v a v a )
  q!  ( v a )
  swap 16 u>> ( a v>>16 )
  swap ( v>>16 a )
  2+ ( v>>16 a+2 )
  q! ; 

: w! ( v a -- )
  2dup ( v a v a )
  h! ( v a )
  swap 32 u>> ( a v>>32 )
  swap ( v>>32 a )
  4+ ( v>>32 a+4 )
  h! ;


: bitwise-oru ( a b -- c ) or ;
: bitwise-andu ( a b -- c ) and ;
: bitwise-notu ( a b -- c ) not ;

: pow ( a b -- c ) ** ;

variable enum-index
: {enum ( -- 0 0 ) 0 enum-index v! enum-index v@ ;
: enum,  ( n1 -- n2 ) enum-index v@ 1+ enum-index v! enum-index v@ ;
: enum} ( n1? -- ) ;

{enum 
( section ids )
constant *number-variant-code* enum,
constant *word-variant-code* enum, 
constant *native-function-variant-code* enum,
constant *string-variant-code* enum, 
constant *variable-variant-code*
enum}


: of-type? ( type cv -- flag ) v@ swap type-code = ;
: number? ( a -- flag ) *number-variant-code* of-type? ;
: word? ( a -- flag ) *word-variant-code* of-type? ;
: native-function? ( a -- flag ) *native-function-variant-code* of-type? ;
: string? ( a -- flag ) *string-variant-code* of-type? ;
: variable? ( a -- flag ) *variable-variant-code* of-type? ;

: ! ( a var -- ) dup variable? if v! else w! then ;
: @ ( a -- b ) dup variable? if v@ else w@ then ;
: ? ( a -- ) @ . ;

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

: +! ( n adr -- )
  dup -rot ( adr n adr -- )
  @ ( adr n val -- )
  + ( adr comb -- )
  swap ( comb adr -- )
  ! ;

\ must always be last in the file 
close-input-file
