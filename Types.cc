#include "Types.h"
#include "Problem.h"
#include <optional>
namespace forth {

byte getLowerHalf(const std::optional<byte>& value) {
    if (value) {
        return getLowerHalf(value.value());
    } else {
        throw Problem(__func__, "can't get lower half, contents empty!");
    }
}

byte getLowerHalf(const std::optional<QuarterAddress>& value) {
    if (value) {
        return getLowerHalf(value.value());
    } else {
        throw Problem(__func__, "can't get lower half, contents empty!");
    }
}

} // end namespace forth
