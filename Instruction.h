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
struct OneByte final : SizedType<1> { };
struct TwoByte final : SizedType<2> { };
struct ThreeByte final : SizedType<3> { };
struct FourByte final : SizedType<4> { };
struct FiveByte final : SizedType<5> { };
struct EightByte final : SizedType<8> { };
struct GrabBag final {
    std::variant<TwoByte, TwoByte> kind;
    constexpr byte size() noexcept {
        switch (kind.index()) {
            case 0:
                return std::get<0>(kind).size();
            case 1:
                return std::get<1>(kind).size();
        }
    }
};
struct SixByte final : SizedType<6> { };
struct TenByte final : SizedType<10> {  };
struct ExtendedVariant final {
    std::variant<TenByte, SixByte> kind;
    constexpr byte size() noexcept {
        switch (kind.index()) {
            case 0:
                return std::get<0>(kind).size();
            case 1:
                return std::get<1>(kind).size();
        }
    }
};
using InstructionWidth = std::variant<OneByte, TwoByte, ThreeByte, FourByte, EightByte, FiveByte, GrabBag, ExtendedVariant>;
enum class OneByteOperation : byte {
#define OneByte(title) title,
#define TwoByte(title, b) 
#define ThreeByte(title, b)
#define FourByte(title, b)
#define FiveByte(title, b)
#define EightByte(title, b)
#define GrabBag(title, b)
#define ExtendedVariant(a, b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
Count,
};

enum class TwoByteOperation : byte {
#define OneByte(title) 
#define TwoByte(title, b) title,
#define ThreeByte(title, b)
#define FourByte(title, b)
#define FiveByte(title, b)
#define EightByte(title, b)
#define GrabBag(title, b)
#define ExtendedVariant(a, b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
Count,
};

enum class ThreeByteOperation : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) title,
#define FourByte(title, b)
#define FiveByte(title, b)
#define EightByte(title, b)
#define GrabBag(title, b)
#define ExtendedVariant(a, b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
Count,
};

enum class FourByteOperation : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b) title,
#define FiveByte(title, b)
#define EightByte(title, b)
#define GrabBag(title, b)
#define ExtendedVariant(title, b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
Count,
};

enum class FiveByteOperation : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) title,
#define EightByte(title, b)
#define GrabBag(title, b)
#define ExtendedVariant(title, b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
Count,
};

enum class EightByteOperation : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) title,
#define GrabBag(title, b)
#define ExtendedVariant(title, b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
Count,
};

enum class GrabBagOperation : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, cl) cl ## _ ## title,
#define ExtendedVariant(title, b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
    Count,
};

enum class ExtendedVariantOperation : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariant(st, title, c) st ## _ ## title,
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
    Count,
};

enum class TenByteOperation : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b) title,
#define ExtendedVariantSixByte(title, b)
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
    Count,
};

enum class SixByteOperation : byte {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b) 
#define ExtendedVariantSixByte(title, b) title,
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
    Count,
};


InstructionWidth determineInstructionWidth(OneByteOperation op);
InstructionWidth determineInstructionWidth(TwoByteOperation op);
InstructionWidth determineInstructionWidth(ThreeByteOperation op);
InstructionWidth determineInstructionWidth(FourByteOperation op);
InstructionWidth determineInstructionWidth(FiveByteOperation op);
InstructionWidth determineInstructionWidth(EightByteOperation op);
InstructionWidth determineInstructionWidth(GrabBagOperation op);
InstructionWidth determineInstructionWidth(ExtendedVariantOperation op);
InstructionWidth determineInstructionWidth(TenByteOperation op);
InstructionWidth determineInstructionWidth(SixByteOperation op);

constexpr TargetRegister getDestinationRegister(byte field) noexcept { 
	return TargetRegister(getLowerHalf(field));
}
constexpr TargetRegister getSourceRegister(byte field) noexcept { 
	return TargetRegister(getUpperHalf(field));
}


} // end namespace forth
#endif // end INSTRUCTION_H__
