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
    //LoadFull, // two argument
    //StoreFull, // two argument
    Count,
};


namespace Instruction {
    constexpr byte singleByteOp(Operation op) noexcept { return static_cast<byte>(op); }
    constexpr QuarterAddress encodeTwoByte(Operation first, byte second) noexcept {
        return encodeBits<QuarterAddress, byte, 0xFF00, 8>(QuarterAddress(first), second);
    }
    constexpr QuarterAddress encodeTwoByte(Operation first, TargetRegister dest, TargetRegister src) noexcept {
        return encodeTwoByte(first, encodeRegisterPair(dest, src));
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
    //constexpr QuarterAddress encodeQuarterAddress(byte a = 0, byte b = 0) noexcept {
    //    return encodeBits<QuarterAddress, byte, 0xFF00, 8>(static_cast<QuarterAddress>(a), b);
    //}
    //constexpr HalfAddress encodeHalfAddress(byte a = 0, byte b = 0, byte c = 0, byte d = 0) noexcept {
    //    return encodeBits<HalfAddress, QuarterAddress, 0xFFFF0000, 16>(
    //            static_cast<HalfAddress>(encodeQuarterAddress(a, b)),
    //            encodeQuarterAddress(c, d));
    //}
    //constexpr Address encodeOperation(byte a = 0, byte b = 0, byte c = 0, byte d = 0, byte e = 0, byte f = 0, byte g = 0, byte h = 0) {
    //    return encodeBits<Address, HalfAddress, 0xFFFFFFFF00000000, 32>( static_cast<Address>(encodeHalfAddress(a, b, c, d)), encodeHalfAddress(e, f, g, h));
    //}
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

    template<typename T, typename ... Args>
    constexpr Address encodeOperation(T first, Args&& ... rest) noexcept {
        return encodeOperation<0, T, Args...>(0, first, std::move(rest)...);
    }
    
} // end namespace Instruction
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



} // end namespace forth
#endif // end INSTRUCTION_H__
