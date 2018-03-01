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

struct OneByte final { static constexpr auto size = 1; };
struct TwoByte final { static constexpr auto size = 2; };
struct ThreeByte final { static constexpr auto size = 3; };
struct FourByte final { static constexpr auto size = 4; };
struct EightByte final { static constexpr auto sie = 8; };
using InstructionWidth = std::variant<OneByte, TwoByte, ThreeByte, FourByte, EightByte>;
enum class Operation : byte {
#define X(title, a, b) title,
#include "InstructionData.def"
    Count,
#undef X
};




static_assert(QuarterAddress(Operation::Count) <= 256, "Too many operations defined!");


InstructionWidth determineInstructionWidth(Operation op);



constexpr bool legalOperation(Operation op) noexcept {
    return static_cast<byte>(Operation::Count) > static_cast<byte>(op);
}
constexpr bool subtractOperation(Operation op) noexcept {
	switch(op) {
		case Operation::Subtract:
		case Operation::SubtractImmediate:
		case Operation::FloatingPointSubtract:
		case Operation::UnsignedSubtract:
		case Operation::UnsignedSubtractImmediate:
			return true;
		default:
			return false;
	}
}
constexpr bool isModuloOperation(Operation op) noexcept {
	switch (op) {
		case Operation::Modulo:
		case Operation::ModuloImmediate:
			return true;
		default:
			return false;
	}
}
constexpr byte getInstructionWidth(Operation op) noexcept {
    if (!legalOperation(op)) {
        return 0;
    }
    switch (op) {
		case Operation::Add:
		case Operation::Subtract:
		case Operation::Multiply:
		case Operation::Divide:
		case Operation::Modulo:
		case Operation::And:
		case Operation::Or:
#define FullImmediate(x) \
        Operation:: Unsigned ## x 
		case FullImmediate(Add):
		case FullImmediate(Subtract):
		case FullImmediate(Multiply):
		case FullImmediate(Divide): 
		case FullImmediate(Modulo): 
		case FullImmediate(And): 
		case FullImmediate(Or): 
		case FullImmediate(GreaterThan): 
		case FullImmediate(LessThan): 
		case FullImmediate(Xor):
		case FullImmediate(ShiftRight):
		case FullImmediate(ShiftLeft):
		case FullImmediate(Equals):
#undef FullImmediate
#define FullImmediate(x) \
        Operation:: FloatingPoint ## x
		case FullImmediate(Add):
		case FullImmediate(Subtract):
		case FullImmediate(Multiply):
		case FullImmediate(Divide): 
		case FullImmediate(GreaterThan): 
		case FullImmediate(LessThan): 
		case FullImmediate(Equals):
#undef FullImmediate
        case Operation::CallSubroutine:
        case Operation::Jump:
        case Operation::DecodeBits:
            return 3;
        case Operation::ConditionalBranch:
        case Operation::ConditionalCallSubroutine:
        case Operation::SetImmediate16_Lower:
        case Operation::SetImmediate16_Lowest:
        case Operation::SetImmediate16_Higher:
        case Operation::SetImmediate16_Highest:
		case Operation::JumpAbsolute:
#define FullImmediate(x) \
        case Operation:: x ## Immediate: \
        case Operation:: Unsigned ## x ## Immediate
		FullImmediate(Add): 
		FullImmediate(Subtract): 
		FullImmediate(Multiply): 
		FullImmediate(Divide): 
		FullImmediate(Modulo): 
		FullImmediate(And): 
		FullImmediate(Or): 
		FullImmediate(GreaterThan): 
		FullImmediate(LessThan): 
		FullImmediate(Xor):
		FullImmediate(ShiftRight):
		FullImmediate(ShiftLeft):
		FullImmediate(Equals):
#undef FullImmediate
        case Operation::EncodeBits:
            return 4;
		case Operation::TypeValue:
		case Operation::FloatingPointTypeValue:
		case Operation::BooleanTypeValue:
		case Operation::UnsignedTypeValue:
        case Operation::PopRegister:
        case Operation::PushRegister:
        case Operation::Move:
        case Operation::Swap:
        case Operation::Load:
        case Operation::Store:
		case Operation::NotFull:
		case Operation::BooleanNotFull:
		case Operation::UnsignedNotFull:
		case Operation::MinusFull:
		case Operation::FloatingPointMinusFull:
		case Operation::UnsignedMinusFull:
		case Operation::PowFull:
		case Operation::UnsignedPowFull:
		case Operation::FloatingPointPowFull:
        case Operation::JumpIndirect:
        case Operation::ConditionalBranchIndirect:
        case Operation::CallSubroutineIndirect:
        case Operation::ConditionalCallSubroutineIndirect:
        case Operation::ConditionalReturnSubroutine:
		case Operation::PrintString:
		case Operation::PrintChar:
		case Operation::TypeDatum:
            return 2;
		case Operation::LoadImmediateLower48:
			return 8;
        default:
            return 1;
    }
}
static_assert(getInstructionWidth(Operation::FloatingPointMultiplyFull) == 3, "FloatingPointMultiplyFull is not 3 bytes wide!");
#undef FVersion
#undef UVersion
#undef BVersion

	constexpr bool immediateForm(Operation op) noexcept {
		switch(op) {
#define Immediate(x) case Operation :: x ## Immediate :
#define ImmediateSU(x) Immediate(Unsigned ## x) Immediate(x) 
	ImmediateSU(Add)
	ImmediateSU(Subtract)
	ImmediateSU(Multiply)
	ImmediateSU(Divide)
	ImmediateSU(Modulo)
	ImmediateSU(And)
	ImmediateSU(Or)
	ImmediateSU(GreaterThan)
	ImmediateSU(LessThan)
	ImmediateSU(Xor)
	ImmediateSU(ShiftRight)
	ImmediateSU(ShiftLeft)
	ImmediateSU(Equals)
				return true;
			default:
				return false;
#undef Immediate
		}
	}
	constexpr bool fullForm(Operation op) noexcept {
		switch(op) {
#define Full(x) Operation :: x ## Full 
#define FullSUF(x) Full(x) : case Full(FloatingPoint ## x ) : case Full(Unsigned ## x)
#define FullSU(x) Full(x) : case Full(Unsigned ## x)
#define FullSUFB(x) Full(x) : case Full(FloatingPoint ## x ) : case Full(Unsigned ## x) : case Full(Boolean ## x)
#define FullSUB(x) FullSU(x) : case Full(Boolean ## x)
			case FullSUF(Add):
			case FullSUF(Subtract):
			case FullSUF(Multiply):
			case FullSUF(Divide):
			case FullSU(Modulo):
			case FullSUB(And):
			case FullSUB(Or):
			case FullSUF(GreaterThan):
			case FullSUF(LessThan):
			case FullSUB(Xor):
			case FullSU(ShiftRight):
			case FullSU(ShiftLeft):
			case FullSUFB(Equals):
			case FullSUF(Pow):
			case FullSUB(Not):
			case FullSUF(Minus):
				return true;
			default:
				return false;
#undef Full
		}
	}

constexpr bool andForm(Operation op) noexcept {
	switch (op) {
		case Operation::And:
		case Operation::AndImmediate:
		case Operation::AndFull:
		case Operation::UnsignedAnd:
		case Operation::UnsignedAndImmediate:
		case Operation::UnsignedAndFull:
		case Operation::BooleanAnd:
		case Operation::BooleanAndFull:
			return true;
		default:
			return false;
	}
}
constexpr bool orForm(Operation op) noexcept {
	switch (op) {
		case Operation::Or:
		case Operation::OrImmediate:
		case Operation::OrFull:
		case Operation::UnsignedOr:
		case Operation::UnsignedOrImmediate:
		case Operation::UnsignedOrFull:
		case Operation::BooleanOr:
		case Operation::BooleanOrFull:
			return true;
		default:
			return false;
	}
}
constexpr bool xorForm(Operation op) noexcept {
	switch (op) {
		case Operation::Xor:
		case Operation::XorImmediate:
		case Operation::XorFull:
		case Operation::UnsignedXor:
		case Operation::UnsignedXorImmediate:
		case Operation::UnsignedXorFull:
		case Operation::BooleanXor:
		case Operation::BooleanXorFull:
			return true;
		default:
			return false;
	}
}
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

static_assert(getInstructionWidth(byte(0x001274)) == 3, "FloatingPointMultiplyFull is not three bytes!");



} // end namespace forth
#endif // end INSTRUCTION_H__
