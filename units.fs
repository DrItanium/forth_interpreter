( numeric conversion units )

: KB ( a -- b ) 1024 * ;
: MB ( a -- b ) KB 1024 * ;
: GB ( a -- b ) MB 1024 * ;
: B->KB ( a -- b ) 1024 / ;
: B->MB ( a -- b ) B->KB 1024 / ;
: B->GB ( a -- b ) B->MB 1024 / ;

: 1MB ( -- 1MB ) 1 MB ;
: 2MB ( -- 2MB ) 2 MB ;
: 1KB ( -- 1KB ) 1 KB ;


close-input-file
