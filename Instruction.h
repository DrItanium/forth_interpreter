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
    Count,
};

static_assert(static_cast<byte>(-1) >= static_cast<byte>(Operation::Count), "Too many operations defined!");

} // end namespace forth
#endif // end INSTRUCTION_H__
