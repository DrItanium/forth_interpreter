( Simple words to make instruction encoding easy )
: encodeOpSection ( op -- op2 ) 3 << F8# and.u ;
: encodeVariantSection ( v -- v2 ) 7# and.u ;
: makeOpByte ( op variant -- encoded ) encodeVariantSection swap encodeOpSection or.u ;
: variant:one-byte 0 ;
: variant:two-byte 1 ;
: encodeOneByte ( op -- b ) variant:one-byte makeOpByte ;
: encodeDestination ( dest -- b ) 4# and.u ;
: encodeSource ( src -- b ) 4# and.u 4# << ;

: encodeRegisterPair ( src dest -- b ) 
    encodeDestination swap
    encodeSource or.u ;
: encodeOneRegister ( dest c v -- b ) 
    makeOpByte swap encodeDestination 8 <<.u or.u ;
: encodeTwoRegister ( src dest c v -- b )
    makeOpByte rot encodeRegisterPair 8 <<.u or.u ;
: encodeThreeRegister ( src2 src dest op v -- b ) 
    encodeTwoRegister ( src2 src dest op v -- src2 c )
    swap ( src2 c -- c src2 )
    encodeDestination ( c src2 -- c enc2 )
    16# <<.u ( c enc2 16# -- c shift2 )
    or.u ( c shift2 -- b ) ;

: encodeFourRegister  ( src3 src2 src dest op v -- b)
    encodeThreeRegister ( src3 src2 src dest op v -- src3 b )
    swap ( src3 b -- b src3 )
    encodeSource 16# <<.u ( b src3 -- b shift16 )
    or.u ; 

: encodeFiveRegister ( src4 src3 src2 src dest op v -- b)
    encodeFourRegister ( src4 src3 src2 src dest op v -- src4 b )
    swap 
    encodeDestination #24 <<.u 
    or.u ;


