// concept of an internal machine instruction
#ifndef INSTRUCTION_H__
#define INSTRUCTION_H__
#include "Types.h"
#include <iostream>
namespace forth {
union Instruction {
    Instruction(Address v) : _value(v) { };
    Address _value;
    byte backingStore[sizeof(Address)];
    QuarterAddress quads[sizeof(Address) / sizeof(QuarterAddress)];
    HalfAddress halves[sizeof(Address) / sizeof(HalfAddress)];
};
} // end namespace forth
#endif // end INSTRUCTION_H__
