#include "Instruction.h"


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


} // end namespace forth
