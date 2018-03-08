#include "Instruction.h"


namespace forth {
	Molecule::Molecule(Address v) : _value(v) { }
	Molecule::Molecule(const Molecule& other) : _value(other._value) { }

	byte Molecule::getByte(Address index) const {
        if (index >= sizeof(Address)) {
            throw Problem("getByte", "INSTRUCTION MISALIGNED");
        } else {
            return backingStore[index];
        }
	}
    QuarterAddress Molecule::getQuarterAddress(Address index) const {
        auto lower = static_cast<QuarterAddress>(getByte(index));
        auto upper = static_cast<QuarterAddress>(getByte(index + 1));
        return (upper << 8) | lower;
    }
	QuarterInteger Molecule::getQuarterOffset(Address index) const {
        auto lower = static_cast<QuarterInteger>(getByte(index));
        auto upper = static_cast<QuarterInteger>(getByte(index + 1));
		return (upper << 8) | lower;
	}
	Address Molecule::getImm48(Address index) const {
		auto b0 = static_cast<Address>(getByte(index));
		auto b1 = static_cast<Address>(getByte(index + 1));
		auto b2 = static_cast<Address>(getByte(index + 2));
		auto b3 = static_cast<Address>(getByte(index + 3));
		auto b4 = static_cast<Address>(getByte(index + 4));
		auto b5 = static_cast<Address>(getByte(index + 5));
		return ((b5 << 40) | (b4 << 32) | (b3 << 24) | (b2 << 16) | (b1 << 8) | b0) & 0x00FF'FFFF'FFFF'FFFF;
	}

InstructionWidth determineInstructionWidth(OneByteOpcode op) { return OneByteInstruction(); }

InstructionWidth determineInstructionWidth(TwoByteOpcode op) { return TwoByteInstruction(); }

InstructionWidth determineInstructionWidth(ThreeByteOpcode op) { return ThreeByteInstruction(); }

InstructionWidth determineInstructionWidth(FourByteOpcode op) { return FourByteInstruction(); }

InstructionWidth determineInstructionWidth(GrabBagOpcode op){
    GrabBagInstruction gb;
    switch (op) {
        case GrabBagOpcode::Minus:
        case GrabBagOpcode::MinusUnsigned:
        case GrabBagOpcode::MinusFloatingPoint:
        case GrabBagOpcode::PrintString:
        case GrabBagOpcode::NotBoolean:
        case GrabBagOpcode::NotSigned:
        case GrabBagOpcode::Not:
            gb.kind = TwoByteInstruction();
            break;
		case GrabBagOpcode::LoadImmediate32:
			gb.kind = SixByteInstruction();
			break;
		case GrabBagOpcode::LoadImmediate64:
			gb.kind = TenByteInstruction();
			break;
		case GrabBagOpcode::LoadImmediate16:
			gb.kind = FourByteInstruction();
			break;

        default:
            throw Problem("determineInstructionWidth", "Illegal grab bag operation specified!");
    }
    return gb;
}

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
