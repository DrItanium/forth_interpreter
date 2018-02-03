#include <iostream>
#include "Machine.h"
#include "Instruction.h"


namespace Instruction = forth::Instruction;
// TODO: add support for invoking some keywords at compile time!
int main() {
	forth::Machine machine (std::cout, std::cin);
    machine.initializeBaseDictionary();
    // more custom words
    machine.addMicrocodedWord<
        Instruction::popA(), 
        Instruction::popB(), 
        Instruction::add(), 
        Instruction::pushC()>("+");
    machine.addMicrocodedWord<
        Instruction::popA(), 
        Instruction::popB(), 
        Instruction::sub(), 
        Instruction::pushC()>("-");
    machine.addMicrocodedWord<
        Instruction::popA(),
        Instruction::popB(),
        Instruction::mul(),
        Instruction::popC()>("*");
    machine.addMicrocodedWord<
        Instruction::popA(),
        Instruction::popB(),
        Instruction::div(),
        Instruction::popC()>("/");
	machine.controlLoop();
	return 0;
}
