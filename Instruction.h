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
            default:
                return 0;
        }
    }
};
struct SixByte final { static constexpr byte size = 6; };
struct TenByte final { static constexpr byte size = 10; };
using InstructionWidth = std::variant<OneByte, TwoByte, ThreeByte, FourByte, EightByte, FiveByte, GrabBag, TenByte>;
enum class Operation : byte {
#define X(title, a, b, c, d) title,
#include "InstructionData.def"
    Count,
#undef X
};

template<Operation op>
constexpr byte instructionWidth = 0;

#define X(title, sz, a, c, d) template<> constexpr auto instructionWidth < Operation :: title > = sz ## Byte :: size ;
#include "InstructionData.def"
#undef X


static_assert(QuarterAddress(Operation::Count) <= 256, "Too many operations defined!");


InstructionWidth determineInstructionWidth(Operation op);

constexpr byte getInstructionWidth(Operation op) noexcept {
    switch (op) {
#define X(title, sz, a, c, d) case Operation :: title : return sz ## Byte :: size ;
#include "InstructionData.def"
#undef X
		default:
			return 0;
    }
}
static_assert(getInstructionWidth(Operation::FloatingPointMultiply) == 3, "FloatingPointMultiplyFull is not 3 bytes wide!");

constexpr byte getInstructionWidth(byte value) noexcept {
    return getInstructionWidth(static_cast<Operation>(value));
}
constexpr byte getInstructionWidth(QuarterAddress value) noexcept {
    return getInstructionWidth(byte(value));
}
constexpr byte getInstructionWidth(HalfAddress value) noexcept {
    return getInstructionWidth(byte(value));
}
constexpr byte getInstructionWidth(Address value) noexcept {
    return getInstructionWidth(byte(value));
}
constexpr byte getInstructionWidth(QuarterAddressWrapper w) noexcept {
	return getInstructionWidth(w.get());
}
constexpr byte getInstructionWidth(HalfAddressWrapper w) noexcept {
	return getInstructionWidth(w.get());
}
constexpr TargetRegister getDestinationRegister(byte field) noexcept { 
	return TargetRegister(getLowerHalf(field));
}
constexpr TargetRegister getSourceRegister(byte field) noexcept { 
	return TargetRegister(getUpperHalf(field));
}
constexpr Operation getOperation(byte i) noexcept {
    return static_cast<Operation>(i);
}

template<typename T>
constexpr byte getInstructionWidth(std::tuple<byte, T> value) noexcept {
	return std::get<0>(value);
}
static_assert(static_cast<byte>(-1) >= static_cast<byte>(Operation::Count), "Too many operations defined!");

} // end namespace forth
#endif // end INSTRUCTION_H__
