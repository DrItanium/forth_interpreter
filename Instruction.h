// concept of an internal machine instruction
#ifndef INSTRUCTION_H__
#define INSTRUCTION_H__
#include "Types.h"
#include "Problem.h"
#include <iostream>
#include <list>
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


enum class Operation : byte {
    Stop,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Not,
    Minus,
    And,
    Or,
    GreaterThan,
    LessThan,
    Xor,
    ShiftRight,
    ShiftLeft,
    PopRegister,
    PushRegister,
    Equals,
    TypeValue,
    Load,
    Store,
    Pow,
    SetImmediate16_Lowest,
    SetImmediate16_Lower,
    SetImmediate16_Higher,
    SetImmediate16_Highest,
    Move,
    Swap,
    // common operations
    PopA,
    PopB,
    PushC,
	PopC,
	PushA,
	PushB,
#define FVersion(x) FloatingPoint ## x 
#define UVersion(x) Unsigned ## x
#define BVersion(x) Boolean ## x
#define FUVersion(x) FVersion(x) , UVersion(x)
#define BUVersion(x) BVersion(x) , UVersion(x)
#define FUBVersion(x) FVersion(x), UVersion(x), BVersion(x)
    FUVersion(Add),
    FUVersion(Subtract),
    FUVersion(Multiply),
    FUVersion(Divide),
    FUVersion(Modulo),
    BUVersion(Not),
    FUVersion(Minus),
    BUVersion(And),
    BUVersion(Or),
    FUVersion(GreaterThan),
    FUVersion(LessThan),
    BUVersion(Xor),
    UVersion(ShiftRight),
    UVersion(ShiftLeft),
    FUBVersion(Equals),
    FUVersion(Pow),
    FUBVersion(TypeValue),

	// full versions of operations
	// these forms are:
	// ?op ?dest = ?src0, ?src1   // full
	// or
	// ?op ?dest = ?src0, ?imm16 // immediate16
	// For the the two operand forms some of the other immediate forms don't
	// make total sense so something like the and operator will have immediate
	// mode be an 8 bit immediate instead.
#define FullImmediate(x) \
	x ## Full ,  \
	x ## Immediate
	FullImmediate(Add),
	FullImmediate(Subtract),
	FullImmediate(Multiply),
	FullImmediate(Divide),
	FullImmediate(Modulo),
	NotFull,
	MinusFull,
	FullImmediate(And),
	FullImmediate(Or),
	FullImmediate(GreaterThan),
	FullImmediate(LessThan),
	FullImmediate(Xor),
	FullImmediate(ShiftRight),
	FullImmediate(ShiftLeft),
	FullImmediate(Equals),
	PowFull,
#undef FullImmediate
    Jump,
    JumpIndirect,
	JumpAbsolute,
    CallSubroutine,
    CallSubroutineIndirect,
    ReturnSubroutine,
    ConditionalBranch,
    ConditionalBranchIndirect,
    ConditionalCallSubroutine,
    ConditionalCallSubroutineIndirect,
    ConditionalReturnSubroutine,
    Increment,
    FUVersion(Increment),
    Decrement,
    FUVersion(Decrement),
	LoadImmediateLower48,
    // type field manipulation
#define FullImmediate(x) FVersion(x ## Full) 
	FullImmediate(Add),
	FullImmediate(Subtract),
	FullImmediate(Multiply),
	FullImmediate(Divide),
	FloatingPointMinusFull,
	FullImmediate(GreaterThan),
	FullImmediate(LessThan),
	FullImmediate(Equals),
    FloatingPointPowFull,
#undef FullImmediate
#define FullImmediate(x) UVersion(x ## Full) , UVersion(x ## Immediate)
	FullImmediate(Add),
	FullImmediate(Subtract),
	FullImmediate(Multiply),
	FullImmediate(Divide),
	FullImmediate(Modulo),
	UnsignedNotFull,
	UnsignedMinusFull,
	FullImmediate(And),
	FullImmediate(Or),
	FullImmediate(GreaterThan),
	FullImmediate(LessThan),
	FullImmediate(Xor),
	FullImmediate(ShiftRight),
	FullImmediate(ShiftLeft),
	FullImmediate(Equals),
	UnsignedPowFull,
#undef FullImmediate
    BVersion(NotFull),
    BVersion(AndFull),
    BVersion(OrFull),
    BVersion(XorFull),
    BVersion(EqualsFull),
    Count,
};
#undef FUBVersion
#undef BUVersion
#undef FUVersion

static_assert(QuarterAddress(Operation::Count) <= 256, "Too many operations defined!");


constexpr bool legalOperation(Operation op) noexcept {
    return static_cast<byte>(Operation::Count) > static_cast<byte>(op);
}
constexpr bool subtractOperation(Operation op) noexcept {
	switch(op) {
		case Operation::Subtract:
		case Operation::SubtractFull:
		case Operation::SubtractImmediate:
		case Operation::FloatingPointSubtract:
		case Operation::FloatingPointSubtractFull:
		case Operation::UnsignedSubtract:
		case Operation::UnsignedSubtractFull:
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
		case Operation::ModuloFull:
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
#define FullImmediate(x) \
        case Operation:: x ## Full: \
        case Operation:: Unsigned ## x ## Full
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
#define FullImmediate(x) \
        case Operation:: FloatingPoint ## x ## Full
		FullImmediate(Add):
		FullImmediate(Subtract):
		FullImmediate(Multiply):
		FullImmediate(Divide): 
		FullImmediate(GreaterThan): 
		FullImmediate(LessThan): 
		FullImmediate(Equals):
#undef FullImmediate
        case Operation::CallSubroutine:
        case Operation::Jump:
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
		case Operation::MinusFull:
		case Operation::PowFull:
        case Operation::JumpIndirect:
        case Operation::ConditionalBranchIndirect:
        case Operation::CallSubroutineIndirect:
        case Operation::ConditionalCallSubroutineIndirect:
        case Operation::ConditionalReturnSubroutine:
        case Operation::Increment:
        case Operation::FloatingPointIncrement:
        case Operation::UnsignedIncrement:
        case Operation::Decrement:
        case Operation::FloatingPointDecrement:
        case Operation::UnsignedDecrement:
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
constexpr byte getDestinationRegister(byte field) noexcept { 
	return getLowerHalf(field);
}
constexpr byte getSourceRegister(byte field) noexcept { 
	return getUpperHalf(field);
}
constexpr Operation getOperation(byte i) noexcept {
    return static_cast<Operation>(i);
}
static_assert(static_cast<byte>(-1) >= static_cast<byte>(Operation::Count), "Too many operations defined!");

static_assert(getInstructionWidth(byte(0x001275)) == 3, "FloatingPointMultiplyFull is not three bytes!");


} // end namespace forth
#endif // end INSTRUCTION_H__
