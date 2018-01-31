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
    constexpr byte add() noexcept { return static_cast<byte>(Operation::Add); }
    constexpr byte sub() noexcept { return static_cast<byte>(Operation::Subtract); }
    constexpr byte mul() noexcept { return static_cast<byte>(Operation::Multiply); }
    constexpr byte div() noexcept { return static_cast<byte>(Operation::Divide); }
    constexpr byte mod() noexcept { return static_cast<byte>(Operation::Modulo); }
    constexpr byte notOp() noexcept { return static_cast<byte>(Operation::Not); }
    constexpr byte minus() noexcept { return static_cast<byte>(Operation::Minus); }
    constexpr byte andOp() noexcept { return static_cast<byte>(Operation::And); }
    constexpr byte orOp() noexcept { return static_cast<byte>(Operation::Or); }
    constexpr byte greaterThan() noexcept { return static_cast<byte>(Operation::GreaterThan); }
    constexpr byte lessThan() noexcept { return static_cast<byte>(Operation::LessThan); }
    constexpr byte xorOp() noexcept { return static_cast<byte>(Operation::Xor); }
    constexpr byte shiftRight() noexcept { return static_cast<byte>(Operation::ShiftRight); }
    constexpr byte shiftLeft() noexcept { return static_cast<byte>(Operation::ShiftLeft); }
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
