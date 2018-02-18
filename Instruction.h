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
#undef FVersion
#undef UVersion
#undef BVersion

	constexpr bool immediateForm(Operation op) noexcept {
		switch(op) {
#define Immediate(x) case Operation :: x ## Immediate :
	Immediate(Add)
	Immediate(Subtract)
	Immediate(Multiply)
	Immediate(Divide)
	Immediate(Modulo)
	Immediate(And)
	Immediate(Or)
	Immediate(GreaterThan)
	Immediate(LessThan)
	Immediate(Xor)
	Immediate(ShiftRight)
	Immediate(ShiftLeft)
	Immediate(Equals)
				return true;
			default:
				return false;
#undef Immediate
		}
	}
	constexpr bool fullForm(Operation op) noexcept {
		switch(op) {
#define Full(x) case Operation :: x ## Full :
	Full(Add)
	Full(Subtract)
	Full(Multiply)
	Full(Divide)
	Full(Modulo)
	Full(And)
	Full(Or)
	Full(GreaterThan)
	Full(LessThan)
	Full(Xor)
	Full(ShiftRight)
	Full(ShiftLeft)
	Full(Equals)
	Full(Pow)
	Full(Not)
	Full(Minus)
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
    return getInstructionWidth(byte(value & 0xFF));
}
constexpr byte getInstructionWidth(HalfAddress value) noexcept {
    return getInstructionWidth(byte(value & 0xFF));
}
constexpr byte getInstructionWidth(Address value) noexcept {
    return getInstructionWidth(byte(value & 0xFF));
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

constexpr byte encodeSingleByteOperation(Operation op) noexcept {
	return static_cast<byte>(op);
}

constexpr QuarterAddress encodeTwoByte(byte a, byte b) noexcept {
	return setLowerUpperHalves<QuarterAddress>(a, b);
}
constexpr QuarterAddress encodeTwoByte(Operation op, byte b) noexcept {
	return encodeTwoByte(byte(op), b);
}
constexpr QuarterAddress makeQuarterAddress(byte lower, byte upper) noexcept {
	return encodeTwoByte(lower, upper);
}

template<typename T>
constexpr QuarterAddress encodeTwoByte(Operation first, TargetRegister dest, T src) noexcept {
	return encodeTwoByte(first, encodeRegisterPair(dest, src));
}

constexpr HalfAddress encodeThreeByte(Operation first, byte second, byte third) noexcept {
	return setFourQuarters<HalfAddress>(byte(first), second, third, 0);
}
constexpr HalfAddress encodeThreeByte(Operation first, QuarterAddress second) noexcept {
	return encodeThreeByte(first, getLowerHalf(second), getUpperHalf(second));
}

constexpr HalfAddress encodeThreeByte(Operation first, TargetRegister dest, TargetRegister src0, TargetRegister src1) noexcept {
	return encodeThreeByte(first, encodeRegisterPair(dest, src0), setLowerHalf(byte(0), byte(src1)));
}

constexpr HalfAddress encodeFourByte(Operation first, byte second, byte third, byte fourth) noexcept {
	return setFourQuarters<HalfAddress>(byte(first), second, third, fourth);
}
static_assert(std::is_same<byte, QuarterOf<HalfAddress>>::value, "Invalid type inference!");
constexpr HalfAddress encodeFourByte(Operation first, TargetRegister dest, QuarterAddress upper) noexcept {
	return encodeFourByte(first, byte(dest), getLowerHalf(upper), getUpperHalf(upper));
}
constexpr HalfAddress encodeFourByte(Operation first, TargetRegister dest, TargetRegister src0, TargetRegister src1, QuarterAddress offset = 0) noexcept {
	return encodeFourByte(first, encodeRegisterPair(dest, src0), 
								 setLowerUpperHalves<byte>(byte(src1), byte(offset)),
								 decodeBits<QuarterAddress, byte, 0x0FF0, 4>(offset));
}

constexpr HalfAddress encodeFourByte(Operation first, TargetRegister dest, TargetRegister src0, QuarterAddress offset = 0) noexcept {
	return encodeFourByte(first, encodeRegisterPair(dest, src0), 
								 getLowerHalf(offset),
								 getUpperHalf(offset));
}

namespace Instruction {
	constexpr byte stop() noexcept { return encodeSingleByteOperation(Operation::Stop); }
#define DefTypeDispatchCase(x) case forth::Operation :: x :
#define DefTypeDispatchCaseU(x) DefTypeDispatchCase( Unsigned ## x )
#define DefTypeDispatchCaseF(x) DefTypeDispatchCase( FloatingPoint ## x )
#define DefTypeDispatchCaseB(x) DefTypeDispatchCase( Boolean ## x )

#define BeginDefTypeDispatchSingleByteOp(name, def) \
    constexpr byte name ( Operation op  = forth::Operation::  def) noexcept { \
        switch (op) {  \
            case forth::Operation:: def :

#define EndDefTypeDispatchSingleByteOp(name, def) \
                return encodeSingleByteOperation(op); \
            default: \
                     return stop(); \
        } \
    }

#define DefTypeDispatchSingleByteOpSU(name, base) \
    BeginDefTypeDispatchSingleByteOp(name, base) \
    DefTypeDispatchCaseU(base) \
    EndDefTypeDispatchSingleByteOp(name, base)
#define DefTypeDispatchSingleByteOpSUF(name, base) \
    BeginDefTypeDispatchSingleByteOp(name, base) \
    DefTypeDispatchCaseU(base) \
    DefTypeDispatchCaseF(base) \
    EndDefTypeDispatchSingleByteOp(name, base)
#define DefTypeDispatchSingleByteOpSUFB(name, base) \
    BeginDefTypeDispatchSingleByteOp(name, base) \
    DefTypeDispatchCaseU(base) \
    DefTypeDispatchCaseF(base) \
    DefTypeDispatchCaseB(base) \
    EndDefTypeDispatchSingleByteOp(name, base)

#define DefTypeDispatchSingleByteOpSUB(name, base) \
    BeginDefTypeDispatchSingleByteOp(name, base) \
    DefTypeDispatchCaseU(base) \
    DefTypeDispatchCaseB(base) \
    EndDefTypeDispatchSingleByteOp(name, base)

    DefTypeDispatchSingleByteOpSUF(div, Divide)
    DefTypeDispatchSingleByteOpSU(mod, Modulo)
    DefTypeDispatchSingleByteOpSU(shiftRight, ShiftRight);
    DefTypeDispatchSingleByteOpSU(shiftLeft, ShiftLeft);
    DefTypeDispatchSingleByteOpSUF(greaterThan, GreaterThan);
    DefTypeDispatchSingleByteOpSUF(lessThan, LessThan);
    DefTypeDispatchSingleByteOpSUFB(equals, Equals);
    DefTypeDispatchSingleByteOpSUFB(typeValue, TypeValue);
    DefTypeDispatchSingleByteOpSUF(pow, Pow);
    BeginDefTypeDispatchSingleByteOp(notOp, Not)
        DefTypeDispatchCaseU(Not)
        DefTypeDispatchCaseB(Not)
    EndDefTypeDispatchSingleByteOp(notOp, Not);
    BeginDefTypeDispatchSingleByteOp(minus, Minus)
        DefTypeDispatchCaseF(Minus)
        DefTypeDispatchCaseU(Minus)
    EndDefTypeDispatchSingleByteOp(minus, Minus);
    DefTypeDispatchSingleByteOpSUB(andOp, And);
    DefTypeDispatchSingleByteOpSUB(orOp, Or);
    DefTypeDispatchSingleByteOpSUB(xorOp, Xor);
	constexpr bool argumentsImplyCompactedForm(TargetRegister dest, TargetRegister src0, TargetRegister src1) {
		return (dest == TargetRegister::C && src0 == TargetRegister::A && src1 == TargetRegister::B);
	}
	constexpr HalfAddress add(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Add);
		} 
		return encodeThreeByte(Operation::AddFull, dest, src0, src1);
	}
	constexpr HalfAddress add(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::AddImmediate, dest, src0, value);
	}
	constexpr HalfAddress addf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::FloatingPointAdd);
		} 
		return encodeThreeByte(Operation::FloatingPointAddFull, dest, src0, src1);
	}
	constexpr HalfAddress addu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedAdd);
		} 
		return encodeThreeByte(Operation::UnsignedAddFull, dest, src0, src1);
	}
	constexpr HalfAddress addiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedAddImmediate, dest, src0, value);
	}
	constexpr HalfAddress sub(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Subtract);
		} 
		return encodeThreeByte(Operation::SubtractFull, dest, src0, src1);
	}
	constexpr HalfAddress sub(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::SubtractImmediate, dest, src0, value);
	}
	constexpr HalfAddress subf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::FloatingPointSubtract);
		} 
		return encodeThreeByte(Operation::FloatingPointSubtractFull, dest, src0, src1);
	}
	constexpr HalfAddress subu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedSubtract);
		} 
		return encodeThreeByte(Operation::UnsignedSubtractFull, dest, src0, src1);
	}
	constexpr HalfAddress subiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedSubtractImmediate, dest, src0, value);
	}
	constexpr HalfAddress mul(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Multiply);
		} 
		return encodeThreeByte(Operation::MultiplyFull, dest, src0, src1);
	}
	constexpr HalfAddress mul(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::MultiplyImmediate, dest, src0, value);
	}
	constexpr HalfAddress mulf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::FloatingPointMultiply);
		} 
		return encodeThreeByte(Operation::FloatingPointMultiplyFull, dest, src0, src1);
	}
	constexpr HalfAddress mulu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedMultiply);
		} 
		return encodeThreeByte(Operation::UnsignedMultiplyFull, dest, src0, src1);
	}
	constexpr HalfAddress muliu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedMultiplyImmediate, dest, src0, value);
	}
    constexpr QuarterAddress popRegister(TargetRegister destination, TargetRegister sp = TargetRegister::SP) noexcept {
		if (sp == TargetRegister::SP) {
			switch(destination) {
				case TargetRegister::A:
					return QuarterAddress(encodeSingleByteOperation(Operation::PopA));
				case TargetRegister::B:
					return QuarterAddress(encodeSingleByteOperation(Operation::PopB));
				case TargetRegister::C:
					return QuarterAddress(encodeSingleByteOperation(Operation::PopC));
				default:
					return encodeTwoByte(Operation::PopRegister, encodeRegisterPair(destination, sp));
			}
		} else {
        	return encodeTwoByte(Operation::PopRegister, encodeRegisterPair(destination, sp));
		}
    }
    constexpr QuarterAddress pushRegister(TargetRegister value, TargetRegister sp = TargetRegister::SP) noexcept {
		if (sp == TargetRegister::SP) {
			switch(value) {
				case TargetRegister::A:
					return QuarterAddress(encodeSingleByteOperation(Operation::PushA));
				case TargetRegister::B:
					return QuarterAddress(encodeSingleByteOperation(Operation::PushB));
				case TargetRegister::C:
					return QuarterAddress(encodeSingleByteOperation(Operation::PushC));
				default:
					return encodeTwoByte(Operation::PushRegister, encodeRegisterPair(sp, value));
			}
		} else {
        	return encodeTwoByte(Operation::PushRegister, encodeRegisterPair(sp, value));
		}
    }
    constexpr QuarterAddress load(TargetRegister dest, TargetRegister src) noexcept { return encodeTwoByte(Operation::Load, dest, src); }
    constexpr QuarterAddress store(TargetRegister dest, TargetRegister src) noexcept { return encodeTwoByte(Operation::Store, dest, src); }
    constexpr QuarterAddress move(TargetRegister dest, TargetRegister src) noexcept { return encodeTwoByte(Operation::Move, dest, src); }
    constexpr QuarterAddress swap(TargetRegister dest, TargetRegister src) noexcept { return encodeTwoByte(Operation::Swap, dest, src); }
    constexpr HalfAddress setImmediate16_Lower(TargetRegister dest, QuarterAddress value) noexcept { return encodeFourByte(Operation::SetImmediate16_Lower, dest, value);  }
    constexpr HalfAddress setImmediate16_Lowest(TargetRegister dest, QuarterAddress value) noexcept { return encodeFourByte(Operation::SetImmediate16_Lowest, dest, value); }
    constexpr HalfAddress setImmediate16_Higher(TargetRegister dest, QuarterAddress value) noexcept { return encodeFourByte(Operation::SetImmediate16_Higher, dest, value); }
    constexpr HalfAddress setImmediate16_Highest(TargetRegister dest, QuarterAddress value) noexcept { return encodeFourByte(Operation::SetImmediate16_Highest, dest, value); }
    constexpr HalfAddress setImmediate64_Lowest(TargetRegister dest, Address value) noexcept { 
        return setImmediate16_Lowest(dest, getLowestQuarter(value));
    }
    constexpr HalfAddress setImmediate64_Lower(TargetRegister dest, Address value) noexcept { 
        return setImmediate16_Lower(dest, getLowerQuarter(value));
    }
    constexpr HalfAddress setImmediate64_Higher(TargetRegister dest, Address value) noexcept { 
        return setImmediate16_Higher(dest, getHigherQuarter(value));
    }
    constexpr HalfAddress setImmediate64_Highest(TargetRegister dest, Address value) noexcept { 
        return setImmediate16_Highest(dest, getHighestQuarter(value));
    }

    template<Address mask, Address shift>
    constexpr Address encodeByte(byte value, Address target = 0) noexcept {
        return encodeBits<Address, byte, mask, shift>(target, value);
    }
    template<Address mask, Address shift>
    constexpr Address encodeQuarterAddress(QuarterAddress value, Address target = 0) noexcept {
        return encodeBits<Address, QuarterAddress, mask, shift>(target, value);
    }
	template<Address mask, Address shift>
	constexpr Address encodeHalfAddress(HalfAddress value, Address target = 0) noexcept {
		return encodeBits<Address, HalfAddress, mask, shift>(target, value);
	}
	template<Address mask, Address shift>
	constexpr Address encodeAddress(Address value, Address target = 0) noexcept {
		return encodeBits<Address, Address, mask, shift>(target, value);
	}
	template<byte startOffset>
	constexpr Address encodeThreeByteAddress(HalfAddress value, Address target = 0) noexcept {
		static_assert(startOffset < 6, "Illegal three byte address!");
		return encodeHalfAddress<Address(0x0000'0000'00FF'FFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeFourByteAddress(HalfAddress value, Address target = 0) noexcept {
		static_assert(startOffset < 5, "Illegal half address start address");
		return encodeHalfAddress<Address(0x0000'0000'FFFF'FFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeFiveByteAddress(Address value, Address target = 0) noexcept {
		static_assert(startOffset < 4, "Illegal address start address");
		return encodeAddress<Address(0x0000'00FF'FFFF'FFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeSixByteAddress(Address value, Address target = 0) noexcept {
		static_assert(startOffset < 3, "Illegal address start address");
		return encodeAddress<Address(0x0000'FFFF'FFFF'FFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeSevenByteAddress(Address value, Address target = 0) noexcept {
		static_assert(startOffset < 2, "Illegal address start address");
		return encodeAddress<Address(0x00FF'FFFF'FFFF'FFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeEightByteAddress(Address value, Address target = 0) noexcept {
		static_assert(startOffset < 1, "Illegal address start address");
		return encodeAddress<Address(0xFFFFFFFFFFFFFFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}

    template<byte startOffset>
    constexpr Address encodeQuarterOperation(QuarterAddress value, Address target = 0) noexcept {
        static_assert(startOffset < 7, "Illegal quarter address start address");
        return encodeQuarterAddress<Address(0xFFFF)<< (startOffset * 8), startOffset * 8>(value, target);
    }
    template<byte startOffset>
    constexpr Address encodeByteOperation(byte value, Address target = 0) noexcept {
        static_assert(startOffset < 8, "Illegal byte offset start address!");
        return encodeByte<Address(0xFF) << (startOffset * 8), startOffset * 8>(value, target);
    }
	template<byte startOffset>
	constexpr Address encodeOperation(byte value, Address target = 0) noexcept {
		return encodeByteOperation<startOffset>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeOperation(QuarterAddress value, Address target = 0) noexcept {
		if (auto width = getInstructionWidth(value); width == 1) {
			return encodeByteOperation<startOffset>(static_cast<byte>(value & 0x00'FF), target);
		} else if (width == 2) {
			return encodeQuarterOperation<startOffset>(value, target);
		} else {
			// skip the contents
			return target;
		}
	}
	template<byte startOffset>
	constexpr Address encodeOperation(HalfAddress value, Address target = 0) noexcept {
		if (auto width = getInstructionWidth(value); width == 1) {
			return encodeOperation<startOffset>((byte)value, target);
		} else if (width == 2) {
			return encodeOperation<startOffset>(static_cast<QuarterAddress>(value), target);
		} else if (width == 3) {
			return encodeThreeByteAddress<startOffset>(value, target);
		} else if (width == 4) {
			return encodeFourByteAddress<startOffset>(value, target);
		} else {
			// skip the contents
			return target;
		}
	}

    template<byte startOffset>
    constexpr Address encodeOperation(Address value, Address target = 0) noexcept {
        if (auto width = getInstructionWidth(value); width < 5) {
            return encodeOperation<startOffset>(HalfAddress(value), target);
        } else if (width == 5) {
            return encodeFiveByteAddress<startOffset>(value, target);
        } else if (width == 6) {
            return encodeSixByteAddress<startOffset>(value, target);
        } else if (width == 7) {
            return encodeSevenByteAddress<startOffset>(value, target);
        } else if (width == 8) {
            return encodeEightByteAddress<startOffset>(value, target);
        } else {
            // skip
            return target;
        }
    }


    template<byte offset, typename T>
    constexpr Address encodeOperation(Address curr, T first) noexcept {
        static_assert(offset < 8, "Too many fields provided!");
        return encodeOperation<offset>(first, curr);
    }
    template<byte offset, typename T, typename ... Args>
    constexpr Address encodeOperation(Address curr, T first, Args&& ... rest) noexcept {
        static_assert(offset < 8, "Too many fields provided!");
		auto encoded = encodeOperation<offset>(curr, first);
		if constexpr (std::is_same<T, HalfAddress>::value) {
			switch (getInstructionWidth(first)) {
				case 1: return encodeOperation<offset + 1, Args...>(encoded, std::move(rest)...);
				case 2: return encodeOperation<offset + 2, Args...>(encoded, std::move(rest)...);
				case 3: return encodeOperation<offset + 3, Args...>(encoded, std::move(rest)...);
				case 4: return encodeOperation<offset + 4, Args...>(encoded, std::move(rest)...);
				default:
					return encodeOperation<offset, Args...>(curr, std::move(rest)...);
			}
        } else if constexpr (std::is_same<T, Address>::value) {
			switch (getInstructionWidth(first)) {
				case 1: return encodeOperation<offset + 1, Args...>(encoded, std::move(rest)...);
				case 2: return encodeOperation<offset + 2, Args...>(encoded, std::move(rest)...);
				case 3: return encodeOperation<offset + 3, Args...>(encoded, std::move(rest)...);
				case 4: return encodeOperation<offset + 4, Args...>(encoded, std::move(rest)...);
				case 5: return encodeOperation<offset + 5, Args...>(encoded, std::move(rest)...);
				case 6: return encodeOperation<offset + 6, Args...>(encoded, std::move(rest)...);
				case 7: return encodeOperation<offset + 7, Args...>(encoded, std::move(rest)...);
				case 8: return encodeOperation<offset + 8, Args...>(encoded, std::move(rest)...);
				default:
					return encodeOperation<offset, Args...>(curr, std::move(rest)...);
			}

		} else if constexpr (std::is_same<T, QuarterAddress>::value) {
			switch (getInstructionWidth(first)) {
				case 1:
					return encodeOperation<offset + 1, Args...>(encoded, std::move(rest)...);
				case 2:
					return encodeOperation<offset + 2, Args...>(encoded, std::move(rest)...);
				default:
					return encodeOperation<offset, Args...>(curr, std::move(rest)...);
			}
		} else {
        	return encodeOperation<offset + sizeof(T), Args...>( 
        	        encodeOperation<offset, T>(curr, first),
        	        std::move(rest)...);
		}
    }

    template<typename T, typename ... Args>
    constexpr Address encodeOperation(T first, Args&& ... rest) noexcept {
        return encodeOperation<0, T, Args...>(0, first, std::move(rest)...);
    }
    constexpr size_t operationLength(byte b) noexcept { return getInstructionWidth(static_cast<Operation>(b)); }
    constexpr size_t operationLength(QuarterAddress b) noexcept { return getInstructionWidth(byte(b & 0xFF)); }
    constexpr size_t operationLength(HalfAddress b) noexcept { return getInstructionWidth(byte(b & 0xFF)); }
    constexpr size_t operationLength(Address b) noexcept { return getInstructionWidth(byte(b & 0xFF)); }

    template<typename T, typename ... Rest>
    constexpr size_t operationLength(T first, Rest ... rest) noexcept {
        if constexpr (sizeof...(rest) > 0) {
            return operationLength(first) + operationLength(std::move(rest)...);
        } else {
            return operationLength(first);
        }
    }
#define FullImmediate(x, name) \
	constexpr HalfAddress name (TargetRegister dest, TargetRegister src0, TargetRegister src1) noexcept { return encodeFourByte(Operation:: x ## Full , dest, src0, src1, 0); } \
	constexpr HalfAddress name (TargetRegister dest, TargetRegister src0, QuarterAddress offset) noexcept { return encodeFourByte(Operation:: x ## Immediate , dest, src0, offset); }
	FullImmediate(Divide, div);
	FullImmediate(Modulo, mod);
	constexpr QuarterAddress notOp(TargetRegister dest, TargetRegister src) noexcept {
		return encodeTwoByte(Operation::NotFull, dest, src);
	}
	constexpr QuarterAddress minus(TargetRegister dest, TargetRegister src) noexcept {
		return encodeTwoByte(Operation::MinusFull, dest, src);
	}

	FullImmediate(And, andOp);
	FullImmediate(Or, orOp);
	FullImmediate(GreaterThan, greaterThan);
	FullImmediate(LessThan, lessThan);
	FullImmediate(Xor, xorOp);
	FullImmediate(ShiftRight, shiftRight);
	FullImmediate(ShiftLeft, shiftLeft);
	FullImmediate(Equals, equals);
	constexpr QuarterAddress pow(TargetRegister dest, TargetRegister src0, TargetRegister src1) noexcept {
		return encodeFourByte(Operation::PowFull, dest, src0, src1, 0);
	}
#undef FullImmediate
	constexpr Address loadAddressLowerHalf(TargetRegister reg, Address value) noexcept {
		return Instruction::encodeOperation(
				Instruction::setImmediate64_Lowest(reg, value),
				Instruction::setImmediate64_Lower(reg, value));
	}
	constexpr Address loadAddressUpperHalf(TargetRegister reg, Address value) noexcept {
		return Instruction::encodeOperation(
				Instruction::setImmediate64_Higher(reg, value),
				Instruction::setImmediate64_Highest(reg, value));
	}
    constexpr QuarterAddress increment(TargetRegister reg, byte imm4) noexcept {
        return encodeTwoByte(Operation::Increment, reg, imm4);
    }
    constexpr QuarterAddress decrement(TargetRegister reg, byte imm4) noexcept {
        return encodeTwoByte(Operation::Decrement, reg, imm4);
    }
    constexpr HalfAddress jump(QuarterInteger offset) noexcept {
        return encodeFourByte(Operation::Jump,
                decodeBits<QuarterInteger, byte, 0x00FF, 0>(offset),
                decodeBits<QuarterInteger, byte, 0xFF00, 8>(offset),
                0);
    }
    constexpr QuarterAddress jumpIndirect(TargetRegister reg) noexcept {
        // put a zero there!
        return encodeTwoByte(Operation::JumpIndirect, reg, TargetRegister::A);
    }
    constexpr HalfAddress conditionalBranch(TargetRegister cond, QuarterInteger offset) noexcept {
        return encodeFourByte(Operation::ConditionalBranch, byte(cond), 
                decodeBits<QuarterInteger, byte, 0x00FF, 0>(offset),
                decodeBits<QuarterInteger, byte, 0xFF00, 8>(offset));
    }
    constexpr HalfAddress conditionalCallSubroutine(TargetRegister cond, QuarterInteger offset) noexcept {
        return encodeFourByte(Operation::ConditionalCallSubroutine, cond, offset);
    }
	constexpr HalfAddress callSubroutine(QuarterInteger offset) noexcept {
		return encodeThreeByte(Operation::CallSubroutine, offset);
    }
    constexpr byte returnSubroutine() noexcept {
        return encodeSingleByteOperation(Operation::ReturnSubroutine);
    }
    constexpr QuarterAddress conditionalReturnSubroutine(TargetRegister cond) noexcept {
        return encodeTwoByte(Operation::ConditionalReturnSubroutine, cond, 0);
    }
    constexpr QuarterAddress conditionalCallSubroutineIndirect(TargetRegister dest, TargetRegister cond) noexcept {
        return encodeTwoByte(Operation::ConditionalCallSubroutineIndirect, dest, cond);
    }
	constexpr Address loadLowerImmediate48(TargetRegister dest, Address value) noexcept {
		return encodeBits<Address, Address, 0xFFFFFFFFFFFF0000, 16>(
				encodeBits<Address, byte, 0x000000000000FF00, 8>(
					encodeBits<Address, byte, 0x00000000000000FF, 0>(0,
						static_cast<byte>(Operation::LoadImmediateLower48)),
					encodeDestinationRegister(dest)),
				value);
	}
	constexpr byte popA() noexcept { return popRegister(TargetRegister::A, TargetRegister::SP); }
	constexpr byte popB() noexcept { return popRegister(TargetRegister::B, TargetRegister::SP); }
	constexpr byte popC() noexcept { return popRegister(TargetRegister::C, TargetRegister::SP); }
	constexpr byte pushA() noexcept { return pushRegister(TargetRegister::A, TargetRegister::SP); }
	constexpr byte pushB() noexcept { return pushRegister(TargetRegister::B, TargetRegister::SP); }
	constexpr byte pushC() noexcept { return pushRegister(TargetRegister::C, TargetRegister::SP); }
    constexpr QuarterAddress popAB() noexcept {
        return (QuarterAddress)encodeOperation(popA(), popB());
    }
    constexpr QuarterAddress swapAB() noexcept {
        return swap(TargetRegister::B, TargetRegister::A);
    }
    constexpr QuarterAddress imm16TestValue = 0xfded;
	static_assert(byte(0xFD) == getUpperHalf(imm16TestValue), "getUpperHalf is not working correctly!");
	static_assert(byte(0xED) == getLowerHalf(imm16TestValue), "getUpperHalf is not working correctly!");
	static_assert(byte(0x00) == byte(TargetRegister::A), "register cast assumptions broken!");
	static_assert(byte(0x16) == byte(Operation::SetImmediate16_Lowest), "Operation index is no longer correct!");
	static_assert(Address(0xFDED0016) == encodeFourByte(Operation::SetImmediate16_Lowest, TargetRegister::A, imm16TestValue), "Encoding is broken!");
    static_assert(Address(0xFDED0016) == setImmediate16_Lowest(TargetRegister::A, imm16TestValue), "setImmediate16_Lowest is broken!");
    static_assert(Address(0xFDED0017) == setImmediate16_Lower(TargetRegister::A, imm16TestValue), "setImmediate16_Lower is broken!");
    static_assert(Address(0xFDED0018) == setImmediate16_Higher(TargetRegister::A, imm16TestValue), "setImmediate16_Higher is broken!");
    static_assert(Address(0xFDED0019) == setImmediate16_Highest(TargetRegister::A, imm16TestValue), "setImmediate16_Highest is broken!");
} // end namespace Instruction


} // end namespace forth
#endif // end INSTRUCTION_H__
