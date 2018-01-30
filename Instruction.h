// concept of an internal machine instruction
#ifndef INSTRUCTION_H__
#define INSTRUCTION_H__
#include "Types.h"
#include <iostream>
#include <list>
namespace forth {
union Molecule {
    Molecule(Address v) : _value(v) { };
    Molecule(const Molecule& other) : _value(other._value) { };
    ~Molecule() { _value = 0; };
    Address _value;
    byte backingStore[sizeof(Address)];
    QuarterAddress quads[sizeof(Address) / sizeof(QuarterAddress)];
    HalfAddress halves[sizeof(Address) / sizeof(HalfAddress)];
};

class MoleculeWalker {
    public:
        MoleculeWalker(Molecule& backingData);
        ~MoleculeWalker();
        void reset();
        int getCurrentPosition() const noexcept { return _index; }
        byte& operator[](int x);
        MoleculeWalker& operator++() {
            ++_index;
            return *this;
        }
        byte currentByte() const;
        int end() const noexcept { return sizeof(Address); }
    private:
        Molecule& _backingData;
        int _index;
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
constexpr byte getDestinationRegister(byte field) noexcept { 
    return field & 0x0F; 
}
constexpr byte getSourceRegister(byte field) noexcept { 
    return (field & 0xF0) >> 4; 
}
constexpr Operation getOperation(byte i) noexcept {
    return static_cast<Operation>(i);
}
constexpr int getInstructionWidth(byte i) noexcept {
    return getInstructionWidth(getOperation(i));
}
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

class Instruction {
    public:
        static bool decodeMolecule(Molecule m, std::list<Instruction> container) noexcept;
    public: 
        Instruction(Operation op, byte field0 = 0, byte field1 = 0);
        ~Instruction();
        Operation getOperation() const noexcept { return _op; }
        byte getField0() const noexcept { return _field0; }
        byte getField1() const noexcept { return _field1; }
        byte getDestinationRegister() const noexcept { return getDestinationRegister(_field0); }
        byte getSourceRegister() const noexcept { return getSourceRegister(_field1); }
    private:
        Operation _op;
        byte _field0;
        byte _field1;
};

} // end namespace forth
#endif // end INSTRUCTION_H__
