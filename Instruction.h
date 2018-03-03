// concept of an internal machine instruction
#ifndef INSTRUCTION_H__
#define INSTRUCTION_H__
#include "Types.h"
#include "Problem.h"
#include <iostream>
#include <list>
#include <tuple>
#include <variant>
namespace forth {

union Molecule {
    Molecule(Address v);
    Molecule(const Molecule& other);
    Address _value;
    byte backingStore[sizeof(Address)];

    byte getByte(Address index) const;
    QuarterAddress getQuarterAddress(Address index) const;
	QuarterInteger getQuarterOffset(Address index) const;
	Address getImm48(Address index) const;
};
enum class TargetRegister : byte {
    Zero, // always zero
    A,
    B,
    C,
    S, // register select
    X, // misc data
    SP, // stack pointer (parameter)
    SP2, // second stack pointer (subroutine)
    DP, // Dictionary Pointer 
    Index, // Index Pointer
    Temporary, // used to store temporary data we requested
    Temporary2, // for the cases where the assembler needs another temporary register
    Count,
};
static_assert(byte(TargetRegister::Count) <= 16, "Too many registers defined!");
constexpr byte encodeDestinationRegister(byte value, TargetRegister reg) noexcept {
	return setLowerHalf(value, byte(reg));
}
constexpr byte encodeDestinationRegister(TargetRegister reg) noexcept {
    return encodeDestinationRegister(0, reg);
}
constexpr byte encodeSourceRegister(byte value, byte reg) noexcept {
	return setUpperHalf(value, reg);
}
constexpr byte encodeSourceRegister(byte value, TargetRegister reg) noexcept {
    return encodeSourceRegister(value, byte(reg));
}
template<typename T>
constexpr byte encodeRegisterPair(TargetRegister dest, T src) noexcept {
	return setLowerUpperHalves<byte>(byte(dest), byte(src));
}
template<byte c>
struct SizedType {
    constexpr SizedType() { }
    constexpr byte size() noexcept { return c; }
};
struct OneByteInstruction final : SizedType<1> { };
struct TwoByteInstruction final : SizedType<2> { };
struct ThreeByteInstruction final : SizedType<3> { };
struct FourByteInstruction final : SizedType<4> { };
struct FiveByteInstruction final : SizedType<5> { };
struct EightByteInstruction final : SizedType<8> { };
struct SixByteInstruction final : SizedType<6> { };
struct TenByteInstruction final : SizedType<10> {  };
struct GrabBagInstruction final {
    std::variant<TwoByteInstruction, SixByteInstruction, TenByteInstruction> kind;
    constexpr byte size() noexcept {
        switch (kind.index()) {
            case 0:
                return std::get<0>(kind).size();
			case 1:
				return std::get<1>(kind).size();
			case 2:
				return std::get<2>(kind).size();
        }
    }
};
using InstructionWidth = std::variant<OneByteInstruction, TwoByteInstruction, ThreeByteInstruction, FourByteInstruction, EightByteInstruction, FiveByteInstruction, GrabBagInstruction>;
enum class VariantKind : byte {
    OneByte,
    TwoByte,
    ThreeByte,
    FourByte,
    EightByte,
    FiveByte,
    GrabBag,
    Count,
};
static_assert(byte(VariantKind::Count) <= 8, "Too many variants specified!");
constexpr VariantKind decodeVariant(byte input) noexcept {
    return decodeBits<byte, VariantKind, 0b00000111, 0>(input);
}
enum class OneByteOpcode : byte {
#define OneByte(title) title,
#define TwoByte(title, b) 
#define ThreeByte(title, b)
#define FourByte(title, b)
#define FiveByte(title, b)
#define EightByte(title, b)
#define GrabBag(title, b)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
Count,
};

enum class TwoByteOpcode : byte {
#define OneByte(title) 
#define TwoByte(title, b) title,
#define ThreeByte(title, b)
#define FourByte(title, b)
#define FiveByte(title, b)
#define EightByte(title, b)
#define GrabBag(title, b)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
Count,
};


enum class ThreeByteOpcode : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) title,
#define FourByte(title, b)
#define FiveByte(title, b)
#define EightByte(title, b)
#define GrabBag(title, b)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
Count,
};

enum class FourByteOpcode : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b) title,
#define FiveByte(title, b)
#define EightByte(title, b)
#define GrabBag(title, b)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
Count,
};

enum class FiveByteOpcode : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) title,
#define EightByte(title, b)
#define GrabBag(title, b)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
Count,
};

enum class EightByteOpcode : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) title,
#define GrabBag(title, b)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
Count,
};

enum class GrabBagOpcode : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, cl) title,
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
    Count,
};



struct UndefinedOpcode final { constexpr UndefinedOpcode() { } };

InstructionWidth determineInstructionWidth(OneByteOpcode op);
InstructionWidth determineInstructionWidth(TwoByteOpcode op);
InstructionWidth determineInstructionWidth(ThreeByteOpcode op);
InstructionWidth determineInstructionWidth(FourByteOpcode op);
InstructionWidth determineInstructionWidth(FiveByteOpcode op);
InstructionWidth determineInstructionWidth(EightByteOpcode op);
InstructionWidth determineInstructionWidth(GrabBagOpcode op);

constexpr TargetRegister getDestinationRegister(byte field) noexcept { 
	return TargetRegister(getLowerHalf(field));
}
constexpr TargetRegister getSourceRegister(byte field) noexcept { 
	return TargetRegister(getUpperHalf(field));
}


} // end namespace forth
#endif // end INSTRUCTION_H__
