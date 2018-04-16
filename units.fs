( numeric conversion units )

: KB ( a -- b ) 1024 * ;
: MB ( a -- b ) KB KB ;
: GB ( a -- b ) MB MB ;
: B->KB ( a -- b ) kilobyte / ;
: B->MB ( a -- b ) B->KB B->KB ;
: B->GB ( a -- b ) B->MB B->MB ;

1 MB constant 1MB
2 MB constant 2MB
1 KB constant 1KB

: *5/9 ( v -- n ) 5 * 9 / ;
: *9/5 ( v -- n ) 9 * 5 / ;
: fahrenheit->celsius ( f -- c ) 32 - *5/9 ;
: celsius->fahrenheit ( c -- f ) *9/5 32 + ;
: fahrenheit->kelvin ( f -- k ) 460 + *5/9 ;
: kelvin->fahrenheit ( k -- f ) *9/5 460 - ;
: rankine->fahrenheit ( r -- f ) 460 - ;
: fahrenheit->rankine ( f -- r ) 460 + ;
: kilo ( a -- b ) 1000 * ;
: mega ( a -- b ) kilo kilo ;
: giga ( a -- b ) mega mega ;


close-input-file
