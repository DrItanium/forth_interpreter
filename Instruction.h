// concept of an internal machine instruction
#ifndef INSTRUCTION_H__
#define INSTRUCTION_H__
#include "Types.h"
#include "Problem.h"
#include <iostream>
#include <list>
#include <tuple>
#include <variant>
#include <optional>
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
using OptionalRegister = std::optional<TargetRegister>;
static_assert(byte(TargetRegister::Count) <= 16, "Too many registers defined!");
constexpr byte encodeDestinationRegister(byte value) noexcept {
    return value & 0x0F;
}
byte encodeDestinationRegister(const OptionalRegister& value);
constexpr byte encodeSourceRegister(byte value) noexcept {
    return (value << 4) & 0xF0;
}
byte encodeSourceRegister(const OptionalRegister& reg);

template<typename Dest, typename Src>
byte encodeRegisterPair(Dest dest, Src src) {
    return encodeSourceRegister(src) |
           encodeDestinationRegister(src);
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
struct SixByteInstruction final : SizedType<6> { };
struct TenByteInstruction final : SizedType<10> {  };
using InstructionWidth = std::variant<OneByteInstruction, TwoByteInstruction, ThreeByteInstruction, FourByteInstruction>;
enum class Opcode : byte {
#define X(title, b) title,
#include "InstructionData.def"
#undef X
	Count,
};
static_assert(byte(Opcode::Count) <= 256, "Too many opcodes defined!");



struct UndefinedOpcode final { constexpr UndefinedOpcode() { } };
InstructionWidth determineInstructionWidth(Opcode op);
template<typename T>
InstructionWidth determineInstructionWidth(T value) {
    if constexpr (std::is_enum<T>::value) {
        return determineInstructionWidth(value);
    } else {
        return determineInstructionWidth(value.getOpcode());
    }
}

template<typename T, typename ... Rest>
Address computeInstructionWidth(T value, Rest&& ... rest) {
    auto combine = Address(std::visit([](auto&& value) { return value.size(); }, determineInstructionWidth(value)));
    if constexpr (sizeof...(rest) > 0) {
        combine += computeInstructionWidth(std::move(rest)...);
    } 
    return combine;
}

constexpr TargetRegister getDestinationRegister(byte field) noexcept { 
	return TargetRegister(getLowerHalf(field));
}
constexpr TargetRegister getSourceRegister(byte field) noexcept { 
	return TargetRegister(getUpperHalf(field));
}
template<typename T>
byte getInstructionWidth(T opcode) {
    return std::visit([](auto&& value) { return value.size(); }, determineInstructionWidth(opcode));
}



} // end namespace forth
#endif // end INSTRUCTION_H__
