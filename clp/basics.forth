: 0 ( -- 0 ) 0 ;
: 1 ( -- 1 ) 1 ;
: 2 ( -- 2 ) 2 ;
: true ( -- TRUE ) TRUE ;
: false ( -- FALSE ) FALSE ;
: 1+ ( a -- b ) 1 + ;
: 1- ( a -- b ) 1 - ;
: 2+ ( a -- b ) 2 + ;
: 2- ( a -- b ) 2 - ;
: 2* ( a -- b ) 2 * ;
: 2/ ( a -- b ) 2 / ;
: 2div ( a -- b ) 2 div ;
: 0= ( n -- flag ) 0 eq ;
: 0< ( a b -- flag ) 0 < ;
: 0> ( a b -- flag ) 0 > ;
: difference ( a b -- c ) - abs ;
: on ( adr -- ) true swap store ;
: off ( adr -- ) false swap store ;
: over swap ( n1 n2 -- n1 n2 n1 ) dup 0 store swap 0 load ;
: dup? ( a -- a a | 0 ) dup if dup then ;
: nip ( a b -- b ) swap drop ;
: tuck ( a b -- b a b ) swap over ;
: 2dup ( a b -- a b a b ) over over ;
: 2drop ( a b -- ) drop drop ;
: view ( a -- a ) dup . ;
: space ( -- ) 32 emit ;
: CR ( -- ) 10 emit ;
: bye ( -- ) quit ;
: -rot ( n1 n2 n3 -- n3 n1 n2 ) rot rot ;


( make sure we close at the end of the day! )
close
