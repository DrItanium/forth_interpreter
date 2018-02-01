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
    RegisterA,
    RegisterB,
    RegisterC,
    RegisterS, // register select
    RegisterX, // misc data
    RegisterT,
    RegisterTA,
    RegisterTB,
    RegisterTX, // misc type
    RegisterIP, // instruction pointer contents
    RegisterTIP, // instruction pointer type
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
        case TargetRegister::RegisterT:
        case TargetRegister::RegisterTA:
        case TargetRegister::RegisterTB:
        case TargetRegister::RegisterTX:
        case TargetRegister::RegisterTIP:
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
    // Full versions of already existing operations
    SetImmediate16_LowestFull,
    SetImmediate16_LowerFull,
    SetImmediate16_HigherFull,
    SetImmediate16_HighestFull,
    //LoadFull, // two argument
    //StoreFull, // two argument
    //SetImmediate32_Lower,
    //SetImmediate32_Upper,
    //
    Count,
};


constexpr bool legalOperation(Operation op) noexcept {
    return static_cast<byte>(Operation::Count) <= static_cast<byte>(op);
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
            return 3;
        case Operation::PopRegister:
        case Operation::PushRegister:
        case Operation::Move:
        case Operation::Swap:
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
    constexpr QuarterAddress encodeTwoByte(Operation first, byte second) noexcept {
        return encodeBits<QuarterAddress, byte, 0xFF00, 8>(QuarterAddress(first), second);
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
    constexpr byte load() noexcept { return singleByteOp(Operation::Load); }
    constexpr byte store() noexcept { return singleByteOp(Operation::Load); }
    constexpr byte pow() noexcept { return singleByteOp(Operation::Pow); }
    constexpr QuarterAddress move(TargetRegister dest, TargetRegister src) noexcept { return encodeTwoByte(Operation::Move, dest, src); }
    constexpr QuarterAddress swap(TargetRegister dest, TargetRegister src) noexcept { return encodeTwoByte(Operation::Swap, dest, src); }
    constexpr byte popA() noexcept { return singleByteOp(Operation::PopA); }
    constexpr byte popB() noexcept { return singleByteOp(Operation::PopB); }
    constexpr byte popT() noexcept { return singleByteOp(Operation::PopT); }
    constexpr byte pushC() noexcept { return singleByteOp(Operation::PushC); }
    constexpr HalfAddress setImmediate16_Lower(QuarterAddress value) noexcept { return encodeThreeByte(Operation::SetImmediate16_Lower, value);  }
    constexpr HalfAddress setImmediate16_Lowest(QuarterAddress value) noexcept { return encodeThreeByte(Operation::SetImmediate16_Lowest, value); }
    constexpr HalfAddress setImmediate16_Higher(QuarterAddress value) noexcept { return encodeThreeByte(Operation::SetImmediate16_Higher, value); }
    constexpr HalfAddress setImmediate16_Highest(QuarterAddress value) noexcept { return encodeThreeByte(Operation::SetImmediate16_Highest, value); }
    template<Address mask, Address shift>
    constexpr Address encodeByte(byte value, Address target = 0) noexcept {
        return encodeBits<Address, byte, mask, shift>(target, value);
    }
    template<Address mask, Address shift>
    constexpr Address encodeQuarterAddress(QuarterAddress value, Address target = 0) noexcept {
        return encodeBits<Address, QuarterAddress, mask, shift>(target, value);
    }
    template<byte startOffset>
    constexpr Address encodeOperation(QuarterAddress value, Address target = 0) noexcept {
        static_assert(startOffset < 7, "Illegal quarter address start address");
        return encodeQuarterAddress<0xFFFF << (startOffset * 8), startOffset * 8>(value, target);
    }
    template<byte startOffset>
    constexpr Address encodeOperation(byte value, Address target = 0) noexcept {
        static_assert(startOffset < 8, "Illegal byte offset start address!");
        return encodeByte<0xFF << (startOffset * 8), startOffset * 8>(value, target);
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
        auto width = getInstructionWidth(value);
        switch (width) {
            case 1:
                return encodeOperation<offset>(curr, static_cast<byte>(value), std::move(rest)...);
            case 2:
                return encodeOperation<offset>(curr, static_cast<QuarterAddress>(value), std::move(rest)...);
            case 3:
                return encodeOperation<offset>(curr, static_cast<byte>(value), QuarterAddress(value >> 8), std::move(rest)...);
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
    static_assert(Address(0xFDED16) == setImmediate16_Lowest(0xfded), "setImmediate16_Lowest is broken!");
    static_assert(Address(0xFDED17) == setImmediate16_Lower(0xfded), "setImmediate16_Lower is broken!");
    static_assert(Address(0xFDED18) == setImmediate16_Higher(0xfded), "setImmediate16_Higher is broken!");
    static_assert(Address(0xFDED19) == setImmediate16_Highest(0xfded), "setImmediate16_Highest is broken!");
} // end namespace Instruction

} // end namespace forth
#endif // end INSTRUCTION_H__
