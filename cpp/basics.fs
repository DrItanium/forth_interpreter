( some words ! )
: of-type? ( type code -- flag ) swap type-code ==s ;
: integer? ( a -- flag ) *integer-variant-code* of-type? ;
: address? ( a -- flag ) *address-variant-code* of-type? ;
: bool? ( a -- flag ) *bool-variant-code* of-type? ;
: word? ( a -- flag ) *word-variant-code* of-type? ;
: native-function? ( a -- flag ) *native-function-variant-code* of-type? ;
: string? ( a -- flag ) *string-variant-code* of-type? ;

: quit ( -- ) bye ;



( must always be the last word in the file )
close-input-file
