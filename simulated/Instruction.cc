#include "Instruction.h"
#include <optional>


namespace forth {

byte encodeDestinationRegister(const OptionalRegister& value) {
    if (value) {
        return encodeDestinationRegister(byte(value.value()));
    } else {
        throw Problem("encodeDestinationRegister", "No register provided!");
    }
}

byte encodeSourceRegister(const OptionalRegister& value) {
    if (value) {
        return encodeSourceRegister(byte(value.value()));
    } else {
        throw Problem("encodeSourceRegister", "No register provided!");
    }
}

std::ostream& operator<<(std::ostream& out, const TargetRegister& dt) {
    // save flags
    auto flags = out.flags();
    switch (dt) {
#define Register(x) \
        case TargetRegister:: x : \
                                  out << #x ; \
        break;
#define RegisterFirst(x) Register(x)
#include "Registers.def"
#undef Register
#undef RegisterFirst
        default:
            throw Problem("operator<<(TargetRegister)", "Illegal register!");
    }
    out.setf(flags); // restore after done
    return out;
}
std::ostream& operator<<(std::ostream& out, const std::optional<TargetRegister>& dt) {
    if (dt) {
        out << dt.value();
        return out;
    } else {
        throw Problem("operator<<(std::optional<TargetRegister>)", "container empty!");
    }
}

} // end namespace forth
