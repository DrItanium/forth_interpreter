// concept of an internal machine instruction
#ifndef INSTRUCTION_H__
#define INSTRUCTION_H__
#include "Types.h"
#include "Problem.h"
#include <iostream>
#include <list>
namespace forth {

template<typename T, typename R, T mask, T shift = 0>
constexpr T encodeBits(T value, R newValue) noexcept {
    return (value & ~mask) | ((static_cast<T>(newValue) << shift) & mask);
}

template<typename T, typename R, T mask, T shift = 0>
constexpr R decodeBits(T value) noexcept {
    return static_cast<R>((value & mask) >> shift);
}


union Molecule {
    Molecule(Address v) : _value(v) { };
    Molecule(const Molecule& other) : _value(other._value) { };
    Address _value;
    byte backingStore[sizeof(Address)];

    byte getByte(Address index) const {
        if (index >= sizeof(Address)) {
            throw Problem("getByte", "INSTRUCTION MISALIGNED");
        } else {
            return backingStore[index];
        }
    }
    QuarterAddress getQuarterAddress(Address index) const {
        auto lower = static_cast<QuarterAddress>(getByte(index));
        auto upper = static_cast<QuarterAddress>(getByte(index + 1));
        return (upper << 8) | lower;
    }
	QuarterInteger getQuarterOffset(Address index) const {
        auto lower = static_cast<QuarterInteger>(getByte(index));
        auto upper = static_cast<QuarterInteger>(getByte(index + 1));
		return (upper << 8) | lower;
	}
	Address getImm48(Address index) const {
		auto b0 = static_cast<Address>(getByte(index));
		auto b1 = static_cast<Address>(getByte(index + 1));
		auto b2 = static_cast<Address>(getByte(index + 2));
		auto b3 = static_cast<Address>(getByte(index + 3));
		auto b4 = static_cast<Address>(getByte(index + 4));
		auto b5 = static_cast<Address>(getByte(index + 5));
		return ((b5 << 40) | (b4 << 32) | (b3 << 24) | (b2 << 16) | (b1 << 8) | b0) & 0x00FFFFFFFFFFFFFF;
	}
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
    return encodeBits<byte, byte, 0x0F, 0>(value, (byte)reg);
}
constexpr byte encodeDestinationRegister(TargetRegister reg) noexcept {
    return encodeDestinationRegister(0, reg);
}
constexpr byte encodeSourceRegister(byte value, byte reg) noexcept {
    return encodeBits<byte, byte, 0xF0, 4>(value, reg);
}
constexpr byte encodeSourceRegister(byte value, TargetRegister reg) noexcept {
    return encodeSourceRegister(value, byte(reg));
}
template<typename T>
constexpr byte encodeRegisterPair(TargetRegister dest, T src) noexcept {
    return encodeSourceRegister(encodeDestinationRegister(dest), src);
}

static constexpr bool involvesDiscriminantRegister(TargetRegister r) noexcept {
    return false;
}
static constexpr bool legalValue(TargetRegister r) noexcept {
    return static_cast<byte>(r) < static_cast<byte>(TargetRegister::Count);
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
constexpr byte getDestinationRegister(byte field) noexcept { 
    return field & 0x0F; 
}
constexpr byte getSourceRegister(byte field) noexcept { 
    return (field & 0xF0) >> 4; 
}
constexpr Operation getOperation(byte i) noexcept {
    return static_cast<Operation>(i);
}
static_assert(static_cast<byte>(-1) >= static_cast<byte>(Operation::Count), "Too many operations defined!");


namespace Instruction {
    constexpr byte singleByteOp(Operation op) noexcept { return static_cast<byte>(op); }
    constexpr QuarterAddress encodeTwoByte(byte a, byte b) noexcept {
        return encodeBits<QuarterAddress, byte, 0xFF00, 8>(QuarterAddress(a), b);
    }
	constexpr QuarterAddress makeQuarterAddress(byte lower, byte upper) noexcept {
		return encodeTwoByte(lower, upper);
	}
    constexpr QuarterAddress encodeTwoByte(Operation first, byte second) noexcept {
        return encodeTwoByte(byte(first), second);
    }
    template<typename T>
    constexpr QuarterAddress encodeTwoByte(Operation first, TargetRegister dest, T src) noexcept {
        return encodeTwoByte(first, encodeRegisterPair(dest, src));
    }
    constexpr HalfAddress encodeThreeByte(Operation first, byte second, byte third) noexcept {
        return encodeBits<HalfAddress, byte, 0xFF0000, 16>( encodeBits<HalfAddress, byte, 0xFF00, 8>(static_cast<HalfAddress>(first), second), third);
    }
    constexpr HalfAddress encodeThreeByte(Operation first, QuarterAddress second) noexcept {
        return encodeBits<HalfAddress, QuarterAddress, 0xFFFF00, 8>(static_cast<HalfAddress>(first), second);
    }
    constexpr HalfAddress encodeFourByte(Operation first, byte second, byte third, byte fourth) noexcept {
		return encodeBits<HalfAddress, byte, 0xFF000000, 24>(
				encodeBits<HalfAddress, byte, 0x00FF0000, 16>(
					encodeBits<HalfAddress, byte, 0x0000FF00, 8>(
						static_cast<HalfAddress>(first), second), third), fourth);
    }
	constexpr byte encodeDual4BitQuantities(byte lower, byte upper) noexcept {
		return encodeBits<byte, byte, 0xF0, 4>( encodeBits<byte, byte, 0x0F, 0>(0, lower), upper);
	}
	constexpr byte encodeDual4BitQuantities(TargetRegister dest, TargetRegister src0) noexcept {
		return encodeDual4BitQuantities(static_cast<byte>(dest), static_cast<byte>(src0));
	}
	constexpr byte encodeDual4BitQuantities(TargetRegister dest, byte upper) noexcept {
		return encodeDual4BitQuantities(static_cast<byte>(dest), 0xF & upper);
	}
	constexpr byte encodeDual4BitQuantities(TargetRegister dest, QuarterAddress upper) noexcept {
		return encodeDual4BitQuantities(dest, static_cast<byte>(upper));
	}
	constexpr HalfAddress encodeFourByte(Operation first, TargetRegister destination, QuarterAddress third) noexcept {
		return encodeFourByte(first, static_cast<byte>(destination), static_cast<byte>(third), static_cast<byte>(third >> 8));
	}
	constexpr HalfAddress encodeFourByte(Operation first, TargetRegister dest, TargetRegister src0, TargetRegister src1, QuarterAddress offset = 0) noexcept {
		return encodeFourByte(first, encodeDual4BitQuantities(dest, src0),
				encodeDual4BitQuantities(src1, offset),
				decodeBits<QuarterAddress, byte, 0x0FF0, 4>(offset));
	}
	constexpr HalfAddress encodeFourByte(Operation first, TargetRegister dest, TargetRegister src0, QuarterAddress offset = 0) noexcept {
		return encodeFourByte(first, encodeDual4BitQuantities(dest, src0),
				static_cast<byte>(offset),
				decodeBits<QuarterAddress, byte, 0xFF00, 8>(offset));
	}
	constexpr byte stop() noexcept { return singleByteOp(Operation::Stop); }
#define DefTypeDispatchCase(x) case forth::Operation :: x :
#define DefTypeDispatchCaseU(x) DefTypeDispatchCase( Unsigned ## x )
#define DefTypeDispatchCaseF(x) DefTypeDispatchCase( FloatingPoint ## x )
#define DefTypeDispatchCaseB(x) DefTypeDispatchCase( Boolean ## x )

#define BeginDefTypeDispatchSingleByteOp(name, def) \
    constexpr byte name ( Operation op  = forth::Operation::  def) noexcept { \
        switch (op) {  \
            case forth::Operation:: def :

#define EndDefTypeDispatchSingleByteOp(name, def) \
                return singleByteOp(op); \
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

    DefTypeDispatchSingleByteOpSUF(add, Add)
    DefTypeDispatchSingleByteOpSUF(sub, Subtract)
    DefTypeDispatchSingleByteOpSUF(mul, Multiply)
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

    constexpr byte popA() noexcept { return singleByteOp(Operation::PopA); }
    constexpr byte popB() noexcept { return singleByteOp(Operation::PopB); }
    constexpr byte pushC() noexcept { return singleByteOp(Operation::PushC); }
    constexpr byte pushA() noexcept { return singleByteOp(Operation::PushA); }
    constexpr byte pushB() noexcept { return singleByteOp(Operation::PushB); }
    constexpr byte popC() noexcept { return singleByteOp(Operation::PopC); }

    constexpr QuarterAddress popRegister(TargetRegister destination, TargetRegister sp = TargetRegister::SP) noexcept {
        return encodeTwoByte(Operation::PopRegister, encodeRegisterPair(destination, sp));
    }
    constexpr QuarterAddress pushRegister(TargetRegister destination, TargetRegister sp = TargetRegister::SP) noexcept {
        return encodeTwoByte(Operation::PushRegister, encodeRegisterPair(destination, sp));
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
        return setImmediate16_Lowest(dest, decodeBits<Address, QuarterAddress, 0x000000000000FFFF>(value)); 
    }
    constexpr HalfAddress setImmediate64_Lower(TargetRegister dest, Address value) noexcept { 
        return setImmediate16_Lower(dest, decodeBits<Address, QuarterAddress, 0x00000000FFFF0000, 16>(value)); 
    }
    constexpr HalfAddress setImmediate64_Higher(TargetRegister dest, Address value) noexcept { 
        return setImmediate16_Higher(dest, decodeBits<Address, QuarterAddress, 0x0000FFFF00000000, 32>(value)); 
    }
    constexpr HalfAddress setImmediate64_Highest(TargetRegister dest, Address value) noexcept { 
        return setImmediate16_Highest(dest, decodeBits<Address, QuarterAddress, 0xFFFF000000000000, 48>(value)); 
    }
    constexpr HalfAddress setImmediate32_Lowest(TargetRegister dest, HalfAddress value) noexcept { 
        return setImmediate16_Lowest(dest, decodeBits<HalfAddress, QuarterAddress, 0x0000FFFF>(value)); 
    }
    constexpr HalfAddress setImmediate32_Lower(TargetRegister dest, HalfAddress value) noexcept { 
        return setImmediate16_Lower(dest, decodeBits<HalfAddress, QuarterAddress, 0xFFFF0000>(value)); 
    }
    constexpr HalfAddress setImmediate32_Higher(TargetRegister dest, HalfAddress value) noexcept { 
        return setImmediate16_Higher(dest, decodeBits<HalfAddress, QuarterAddress, 0x0000FFFF>(value)); 
    }
    constexpr HalfAddress setImmediate32_Highest(TargetRegister dest, HalfAddress value) noexcept { 
        return setImmediate16_Highest(dest, decodeBits<HalfAddress, QuarterAddress, 0xFFFF0000>(value)); 
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
	template<byte startOffset>
	constexpr Address encodeThreeByteAddress(HalfAddress value, Address target = 0) noexcept {
		static_assert(startOffset < 6, "Illegal three byte address!");
		return encodeHalfAddress<Address(0xFFFFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeFourByteAddress(HalfAddress value, Address target = 0) noexcept {
		static_assert(startOffset < 5, "Illegal half address start address");
		return encodeHalfAddress<Address(0xFFFFFFFF) << (startOffset * 8), startOffset * 8>(value, target);
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
    constexpr Address encodeOperation(QuarterAddress value, Address target = 0) noexcept {
        static_assert(startOffset < 7, "Illegal quarter address start address");
        return encodeQuarterAddress<Address(0xFFFF)<< (startOffset * 8), startOffset * 8>(value, target);
    }
    template<byte startOffset>
    constexpr Address encodeOperation(byte value, Address target = 0) noexcept {
        static_assert(startOffset < 8, "Illegal byte offset start address!");
        return encodeByte<Address(0xFF) << (startOffset * 8), startOffset * 8>(value, target);
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
    static constexpr QuarterAddress imm16TestValue = 0xfded;
    static_assert(Address(0xFDED0016) == setImmediate16_Lowest(TargetRegister::A, imm16TestValue), "setImmediate16_Lowest is broken!");
    static_assert(Address(0xFDED0017) == setImmediate16_Lower(TargetRegister::A, imm16TestValue), "setImmediate16_Lower is broken!");
    static_assert(Address(0xFDED0018) == setImmediate16_Higher(TargetRegister::A, imm16TestValue), "setImmediate16_Higher is broken!");
    static_assert(Address(0xFDED0019) == setImmediate16_Highest(TargetRegister::A, imm16TestValue), "setImmediate16_Highest is broken!");
    constexpr QuarterAddress popAB() noexcept {
        return (QuarterAddress)encodeOperation(popA(), popB());
    }
    constexpr QuarterAddress swapAB() noexcept {
        return swap(TargetRegister::B, TargetRegister::A);
    }
    constexpr size_t operationLength(byte b) noexcept { return getInstructionWidth(static_cast<Operation>(b)); }
    constexpr size_t operationLength(QuarterAddress b) noexcept { return getInstructionWidth(byte(b & 0xFF)); }
    constexpr size_t operationLength(HalfAddress b) noexcept { return getInstructionWidth(byte(b & 0xFF)); }

    template<typename T, typename ... Rest>
    constexpr size_t operationLength(T first, Rest ... rest) noexcept {
        return operationLength(first) + operationLength(std::move(rest)...);
    }
	constexpr QuarterAddress moveXtoC() noexcept {
		return move(TargetRegister::C, TargetRegister::X);
	}
#define FullImmediate(x, name) \
	constexpr HalfAddress name (TargetRegister dest, TargetRegister src0, TargetRegister src1) noexcept { return encodeFourByte(Operation:: x ## Full , dest, src0, src1, 0); } \
	constexpr HalfAddress name (TargetRegister dest, TargetRegister src0, QuarterAddress offset) noexcept { return encodeFourByte(Operation:: x ## Immediate , dest, src0, offset); }
	FullImmediate(Add, add);
	FullImmediate(Subtract, sub);
	FullImmediate(Multiply, mul);
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
        return encodeFourByte(Operation::ConditionalCallSubroutine, byte(cond), 
                decodeBits<QuarterInteger, byte, 0x00FF, 0>(offset),
                decodeBits<QuarterInteger, byte, 0xFF00, 8>(offset));
    }
    constexpr HalfAddress callSubroutine(QuarterInteger offset) noexcept {
        return encodeFourByte(Operation::CallSubroutine,
                decodeBits<QuarterInteger, byte, 0x00FF, 0>(offset),
                decodeBits<QuarterInteger, byte, 0xFF00, 8>(offset),
                0);
    }
    constexpr byte returnSubroutine() noexcept {
        return singleByteOp(Operation::ReturnSubroutine);
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
} // end namespace Instruction


} // end namespace forth
#endif // end INSTRUCTION_H__
