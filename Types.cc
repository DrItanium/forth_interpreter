#include "Types.h"
#include "Problem.h"
#include <optional>
namespace forth {
union QuarterIntegerConversion {
    QuarterAddress a;
    QuarterAddress i;
};
QuarterInteger safeExtract(QuarterAddress addr) noexcept {
    QuarterIntegerConversion k;
    k.a = addr;
    return k.i;
}

QuarterAddress safeExtract(QuarterInteger i) noexcept {
    QuarterIntegerConversion k;
    k.i = i;
    return k.a;
}

HalfAddress make24bit(Address input) {
    if (input > mask24) {
        throw Problem("make24bit", "Provided address is larger than 24-bits");
    } else {
        return HalfAddress(input) & mask24;
    }
}


} // end namespace forth
