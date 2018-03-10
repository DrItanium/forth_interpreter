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


} // end namespace forth
