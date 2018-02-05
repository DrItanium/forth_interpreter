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
};
enum class TargetRegister : byte {
    A,
    B,
    C,
    S, // register select
    X, // misc data
    T,
    TA,
    TB,
    TX, // misc type
    IP, // instruction pointer contents
    SP, // stack pointer (parameter)
    SP2, // second stack pointer (subroutine)
    Error,
};
constexpr byte encodeDestinationRegister(byte value, TargetRegister reg) noexcept {
    return encodeBits<byte, byte, 0x0F, 0>(value, (byte)reg);
}
constexpr byte encodeDestinationRegister(TargetRegister reg) noexcept {
    return encodeDestinationRegister(0, reg);
}
constexpr byte encodeSourceRegister(byte value, TargetRegister reg) noexcept {
    return encodeBits<byte, byte, 0xF0, 4>(value, (byte)reg);
}
constexpr byte encodeRegisterPair(TargetRegister dest, TargetRegister src) noexcept {
    return encodeSourceRegister(encodeDestinationRegister(dest), src);
}
static_assert(byte(TargetRegister::Error) <= 16, "Too many registers defined!");
static constexpr bool involvesDiscriminantRegister(TargetRegister r) noexcept {
    switch (r) {
        case TargetRegister::T:
        case TargetRegister::TA:
        case TargetRegister::TB:
        case TargetRegister::TX:
            return true;
        default:
            return false;
    }
}
static constexpr bool legalValue(TargetRegister r) noexcept {
    return static_cast<byte>(r) < static_cast<byte>(TargetRegister::Error);
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
    PopT,
    PushC,
	PopC,
	PushA,
	PushB,
	// full versions of operations
	// these forms are:
	// ?op ?dest = ?src0, ?src1, ?imm12 // full
	// or
	// ?op ?dest = ?src0, ?imm16 // immediate16
	// For the the two operand forms some of the other immediate forms don't
	// make total sense so something like the and operator will have immediate
	// mode be an 8 bit immediate instead.
#define FullImmediate(x) \
	x ## Full ,  \
	x ## Immediate
	FullImmediate(Add),
	FullImmediate(Sub),
	FullImmediate(Mul),
	FullImmediate(Div),
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
    Count,
};


constexpr bool legalOperation(Operation op) noexcept {
    return static_cast<byte>(Operation::Count) > static_cast<byte>(op);
}
constexpr byte getInstructionWidth(Operation op) noexcept {
    if (!legalOperation(op)) {
        return 0;
    }
    switch (op) {
        case Operation::SetImmediate16_Lower:
        case Operation::SetImmediate16_Lowest:
        case Operation::SetImmediate16_Higher:
        case Operation::SetImmediate16_Highest:
#define FullImmediate(x) \
		case Operation:: x ## Full : \
		case Operation:: x ## Immediate : 
		FullImmediate(Add) 
		FullImmediate(Sub) 
		FullImmediate(Mul) 
		FullImmediate(Div) 
		FullImmediate(Modulo) 
		FullImmediate(And) 
		FullImmediate(Or) 
		FullImmediate(GreaterThan) 
		FullImmediate(LessThan) 
		FullImmediate(Xor)
		FullImmediate(ShiftRight)
		FullImmediate(ShiftLeft)
		FullImmediate(Equals)
#undef FullImmediate
            return 4;
        case Operation::PopRegister:
        case Operation::PushRegister:
        case Operation::Move:
        case Operation::Swap:
        case Operation::Load:
        case Operation::Store:
		case Operation::NotFull:
		case Operation::MinusFull:
		case Operation::PowFull:
            return 2;
        default:
            return 1;
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
    constexpr QuarterAddress encodeTwoByte(Operation first, byte second) noexcept {
        return encodeTwoByte(byte(first), second);
    }
    constexpr QuarterAddress encodeTwoByte(Operation first, TargetRegister dest, TargetRegister src) noexcept {
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
    constexpr byte add() noexcept { return singleByteOp(Operation::Add); }
    constexpr byte sub() noexcept { return singleByteOp(Operation::Subtract); }
    constexpr byte mul() noexcept { return singleByteOp(Operation::Multiply); }
    constexpr byte div() noexcept { return singleByteOp(Operation::Divide); }
    constexpr byte mod() noexcept { return singleByteOp(Operation::Modulo); }
    constexpr byte notOp() noexcept { return singleByteOp(Operation::Not); }
    constexpr byte minus() noexcept { return singleByteOp(Operation::Minus); }
    constexpr byte andOp() noexcept { return singleByteOp(Operation::And); }
    constexpr byte orOp() noexcept { return singleByteOp(Operation::Or); }
    constexpr byte greaterThan() noexcept { return singleByteOp(Operation::GreaterThan); }
    constexpr byte lessThan() noexcept { return singleByteOp(Operation::LessThan); }
    constexpr byte xorOp() noexcept { return singleByteOp(Operation::Xor); }
    constexpr byte shiftRight() noexcept { return singleByteOp(Operation::ShiftRight); }
    constexpr byte shiftLeft() noexcept { return singleByteOp(Operation::ShiftLeft); }

    constexpr QuarterAddress popRegister(TargetRegister destination) noexcept {
        return encodeTwoByte(Operation::PopRegister, encodeDestinationRegister(destination));
    }
    constexpr QuarterAddress pushRegister(TargetRegister destination) noexcept {
        return encodeTwoByte(Operation::PushRegister, encodeDestinationRegister(destination));
    }
    constexpr byte equals() noexcept { return singleByteOp(Operation::Equals); }
    constexpr byte typeValue() noexcept { return singleByteOp(Operation::TypeValue); }
    constexpr QuarterAddress load(TargetRegister dest, TargetRegister src) noexcept { return encodeTwoByte(Operation::Load, dest, src); }
    constexpr QuarterAddress store(TargetRegister dest, TargetRegister src) noexcept { return encodeTwoByte(Operation::Store, dest, src); }
    constexpr byte pow() noexcept { return singleByteOp(Operation::Pow); }
    constexpr QuarterAddress move(TargetRegister dest, TargetRegister src) noexcept { return encodeTwoByte(Operation::Move, dest, src); }
    constexpr QuarterAddress swap(TargetRegister dest, TargetRegister src) noexcept { return encodeTwoByte(Operation::Swap, dest, src); }
    constexpr byte popA() noexcept { return singleByteOp(Operation::PopA); }
    constexpr byte popB() noexcept { return singleByteOp(Operation::PopB); }
    constexpr byte popT() noexcept { return singleByteOp(Operation::PopT); }
    constexpr byte pushC() noexcept { return singleByteOp(Operation::PushC); }
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
	constexpr Address encodeOperation(HalfAddress value, Address target = 0) noexcept {
		static_assert(startOffset < 5, "Illegal half address start address");
		return encodeHalfAddress<Address(0xFFFFFFFF) << (startOffset * 8), startOffset * 8>(value, target);
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
        return encodeOperation<offset + sizeof(T), Args...>( 
                encodeOperation<offset, T>(curr, first),
                std::move(rest)...);
    }
    template<byte offset, typename ... Args>
    constexpr Address encodeOperation(Address curr, HalfAddress value, Args&& ... rest) noexcept {
        // we will either have 3 or four bytes to look at
        switch (getInstructionWidth(value)) {
            case 1:
                return encodeOperation<offset>(curr, static_cast<byte>(value), std::move(rest)...);
            case 2:
                return encodeOperation<offset>(curr, static_cast<QuarterAddress>(value), std::move(rest)...);
            case 4:
                return encodeOperation<offset>(curr, static_cast<QuarterAddress>(value), static_cast<QuarterAddress>(value >> 16), std::move(rest)...);
            default:
                    return encodeOperation<offset>(curr, std::move(rest)...);
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
    constexpr byte pushA() noexcept { return singleByteOp(Operation::PushA); }
    constexpr byte pushB() noexcept { return singleByteOp(Operation::PushB); }
    constexpr byte popC() noexcept { return singleByteOp(Operation::PopC); }
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
	constexpr HalfAddress name (TargetRegister dest, TargetRegister src0, TargetRegister src1, QuarterAddress offset = 0) noexcept { return encodeFourByte(Operation:: x ## Full , dest, src0, src1, offset); } \
	constexpr HalfAddress name (TargetRegister dest, TargetRegister src0, QuarterAddress offset) noexcept { return encodeFourByte(Operation:: x ## Immediate , dest, src0, offset); }
#define FullImmediateNoOffset(x, name) \
	constexpr HalfAddress name (TargetRegister dest, TargetRegister src0, TargetRegister src1) noexcept { return encodeFourByte(Operation:: x ## Full , dest, src0, src1, 0); } \
	constexpr HalfAddress name (TargetRegister dest, TargetRegister src0, QuarterAddress offset) noexcept { return encodeFourByte(Operation:: x ## Immediate , dest, src0, offset); }
	FullImmediate(Add, add);
	FullImmediate(Sub, sub);
	FullImmediate(Mul, mul);
	FullImmediate(Div, div);
	FullImmediate(Modulo, mod);
	constexpr QuarterAddress notOp(TargetRegister dest, TargetRegister src) noexcept {
		return encodeTwoByte(Operation::NotFull, dest, src);
	}
	constexpr QuarterAddress minus(TargetRegister dest, TargetRegister src) noexcept {
		return encodeTwoByte(Operation::MinusFull, dest, src);
	}

	FullImmediate(And, andOp);
	FullImmediate(Or, orOp);
	FullImmediateNoOffset(GreaterThan, greaterThan);
	FullImmediateNoOffset(LessThan, lessThan);
	FullImmediateNoOffset(Xor, xorOp);
	FullImmediateNoOffset(ShiftRight, shiftRight);
	FullImmediateNoOffset(ShiftLeft, shiftLeft);
	FullImmediateNoOffset(Equals, equals);
	constexpr QuarterAddress pow(TargetRegister dest, TargetRegister src0, TargetRegister src1) noexcept {
		return encodeFourByte(Operation::PowFull, dest, src0, src1, 0);
	}
#undef FullImmediate
} // end namespace Instruction


} // end namespace forth
#endif // end INSTRUCTION_H__
