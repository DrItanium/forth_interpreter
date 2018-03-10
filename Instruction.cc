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
        default:
            throw Problem("determineInstructionWidth", "Illegal grab bag operation specified!");
    }
    return gb;
}

InstructionWidth determineInstructionWidth(Opcode code) {
	switch (code) {
#define DispatchOneRegister(title) return TwoByteInstruction() ;
#define DispatchTwoRegister(title) return TwoByteInstruction() ;
#define DispatchThreeRegister(title) return ThreeByteInstruction() ;
#define DispatchSignedImm16(title) return FourByteInstruction() ;
#define DispatchImmediate24(title) return FourByteInstruction() ;
#define DispatchTwoRegisterWithImm16(title) return FourByteInstruction();
#define DispatchOneRegisterWithImm16(title) return FourByteInstruction();
#define DispatchFourRegister(title) return ThreeByteInstruction();
#define DispatchFiveRegister(title) return FourByteInstruction();
#define DispatchOneRegisterWithImm64(title) return TenByteInstruction();
#define DispatchOneRegisterWithImm32(title) return SixByteInstruction();
#define DispatchNoArguments(title) return OneByteInstruction();
#define X(title, k) case Opcode :: title: INDIRECTION(Dispatch, k)(title)
#include "InstructionData.def"
#undef X
#undef DispatchNoArguments
#undef DispatchOneRegister
#undef DispatchTwoRegister
#undef DispatchThreeRegister
#undef DispatchSignedImm16
#undef DispatchImmediate24
#undef DispatchTwoRegisterWithImm16
#undef DispatchOneRegisterWithImm16
#undef DispatchFiveRegister
#undef DispatchFourRegister
#undef DispatchOneRegisterWithImm32
#undef DispatchOneRegisterWithImm64
		default:
			throw Problem("determineInstructionWidth", "Unable to determine instruction width!");
	}
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
