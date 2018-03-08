#include "Types.h"
#include "Problem.h"
#include <optional>
namespace forth {
QuarterInteger safeExtract(QuarterAddress addr) noexcept {
    union {
        QuarterAddress a;
        QuarterInteger i;
    } k;
    k.a = addr;
    return k.i;
}


} // end namespace forth
