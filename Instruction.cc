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

InstructionWidth determineInstructionWidth(OneRegisterOpcode op) { return TwoByteInstruction(); }
InstructionWidth determineInstructionWidth(TwoRegisterOpcode op) { return TwoByteInstruction(); }

InstructionWidth determineInstructionWidth(ThreeByteOpcode op) { return ThreeByteInstruction(); }

InstructionWidth determineInstructionWidth(FourByteOpcode op) { return FourByteInstruction(); }

InstructionWidth determineInstructionWidth(FiveByteOpcode op) { return FiveByteInstruction(); }

InstructionWidth determineInstructionWidth(EightByteOpcode op) { return EightByteInstruction(); }

InstructionWidth determineInstructionWidth(GrabBagOpcode op){
    GrabBagInstruction gb;
    switch (op) {
        case GrabBagOpcode::ExtendedTwoRegister0_Minus:
        case GrabBagOpcode::ExtendedTwoRegister0_MinusUnsigned:
        case GrabBagOpcode::ExtendedTwoRegister0_MinusFloatingPoint:
        case GrabBagOpcode::ExtendedTwoRegister0_PrintString:
            gb.kind.emplace<0>(TwoByteInstruction());
            break;
        case GrabBagOpcode::ExtendedTwoRegister1_NotBoolean:
        case GrabBagOpcode::ExtendedTwoRegister1_NotSigned:
        case GrabBagOpcode::ExtendedTwoRegister1_Not:
            gb.kind.emplace<1>(TwoByteInstruction());
            break;
        default:
            throw Problem("determineInstructionWidth", "Illegal grab bag operation specified!");
    }
    return gb;
}

InstructionWidth determineInstructionWidth(ExtendedVariantOpcode op){
    switch(op) {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b) case ExtendedVariantOpcode :: TenByte ## _ ## title : determineInstructionWidth(ExtendedVariantToSubVariant< ExtendedVariantOpcode :: TenByte ## _ ## title > ); break;
#define ExtendedVariantSixByte(title, b) case ExtendedVariantOpcode :: SixByte ## _ ## title : determineInstructionWidth(ExtendedVariantToSubVariant< ExtendedVariantOpcode :: SixByte ## _ ## title > ); break;
#define ExtendedVariant(st, b, c) INDIRECTION(ExtendedVariant, st)(b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
        default:
            throw Problem("determineInstructionWidth", "Illegal ExtendedVariantOpcode specified!");
    }

}

InstructionWidth determineInstructionWidth(TenByteOpcode op) {
    ExtendedVariantInstruction ev;
    ev.kind = TenByteInstruction();
    return ev;
}

InstructionWidth determineInstructionWidth(SixByteOpcode op){
    ExtendedVariantInstruction ev;
    ev.kind = SixByteInstruction();
    return ev;
}


} // end namespace forth
