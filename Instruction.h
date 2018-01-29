// concept of an internal machine instruction
#ifndef INSTRUCTION_H__
#define INSTRUCTION_H__
#include "Types.h"
#include <iostream>
namespace forth {
union Instruction {
    Instruction(Address v) : _value(v) { };
    Instruction(const Instruction& other) : _value(other._value) { };
    ~Instruction() { _value = 0; };
    Address _value;
    byte backingStore[sizeof(Address)];
    QuarterAddress quads[sizeof(Address) / sizeof(QuarterAddress)];
    HalfAddress halves[sizeof(Address) / sizeof(HalfAddress)];
};

enum class Operation : byte {
    Nop,
    Combine,
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
    Shift,
    PopRegister,
    PushRegister,
    Equals,
    TypeValue,
    Load,
    Store,
    Pow,
    Sin,
    Cos,
    Tan,
    Atan,
    Atan2,
    Nor,
    Nand,
    Move,
    Swap,
    Count,
};
constexpr int getInstructionWidth(Operation count) noexcept {
    switch (count) {
        case Operation::Nop:
        case Operation::TypeValue:
        case Operation::Multiply:
        case Operation::Divide:
        case Operation::Modulo:
        case Operation::Not:
        case Operation::Minus:
        case Operation::Pow:
        case Operation::Sin:
        case Operation::Cos:
        case Operation::Tan:
        case Operation::Atan:
        case Operation::Atan2:
        case Operation::Load:
        case Operation::Store:
        case Operation::Nor:
        case Operation::Nand:
            return 1;
        case Operation::Combine:
        case Operation::PushRegister:
        case Operation::PopRegister:
        case Operation::Move:
        case Operation::Swap:
            return 2;
        default:
            return 0;
    }
}

static_assert(static_cast<byte>(-1) >= static_cast<byte>(Operation::Count), "Too many operations defined!");

} // end namespace forth
#endif // end INSTRUCTION_H__
