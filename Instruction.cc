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

InstructionWidth determineInstructionWidth(OneByteOperation op) { return OneByte(); }

InstructionWidth determineInstructionWidth(TwoByteOperation op) { return TwoByte(); }

InstructionWidth determineInstructionWidth(ThreeByteOperation op) { return ThreeByte(); }

InstructionWidth determineInstructionWidth(FourByteOperation op) { return FourByte(); }

InstructionWidth determineInstructionWidth(FiveByteOperation op) { return FiveByte(); }

InstructionWidth determineInstructionWidth(EightByteOperation op) { return EightByte(); }

InstructionWidth determineInstructionWidth(GrabBagOperation op){
    GrabBag gb;
    switch (op) {
        case GrabBagOperation::ExtendedTwoRegister0_Minus:
        case GrabBagOperation::ExtendedTwoRegister0_MinusUnsigned:
        case GrabBagOperation::ExtendedTwoRegister0_MinusFloatingPoint:
        case GrabBagOperation::ExtendedTwoRegister0_PrintString:
            gb.kind.emplace<0>(TwoByte());
            break;
        case GrabBagOperation::ExtendedTwoRegister1_NotBoolean:
        case GrabBagOperation::ExtendedTwoRegister1_NotSigned:
        case GrabBagOperation::ExtendedTwoRegister1_Not:
            gb.kind.emplace<1>(TwoByte());
            break;
        default:
            throw Problem("determineInstructionWidth", "Illegal grab bag operation specified!");
    }
    return gb;
}

InstructionWidth determineInstructionWidth(ExtendedVariantOperation op){
    switch(op) {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b) case ExtendedVariantOperation :: TenByte ## _ ## title : determineInstructionWidth(ExtendedVariantToSubVariant< ExtendedVariantOperation :: TenByte ## _ ## title > ); break;
#define ExtendedVariantSixByte(title, b) case ExtendedVariantOperation :: SixByte ## _ ## title : determineInstructionWidth(ExtendedVariantToSubVariant< ExtendedVariantOperation :: SixByte ## _ ## title > ); break;
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
            throw Problem("determineInstructionWidth", "Illegal ExtendedVariantOperation specified!");
    }

}

InstructionWidth determineInstructionWidth(TenByteOperation op) {
    ExtendedVariant ev;
    ev.kind = TenByte();
    return ev;
}

InstructionWidth determineInstructionWidth(SixByteOperation op){
    ExtendedVariant ev;
    ev.kind = SixByte();
    return ev;
}


} // end namespace forth
