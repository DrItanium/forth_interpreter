#include <iostream>
#include "Machine.h"
#include "Instruction.h"


namespace Instruction = forth::Instruction;
template<typename T, typename ... Args>
constexpr forth::Address molecule(T first, Args&& ... rest) noexcept {
    return Instruction::encodeOperation(first, std::move(rest)...);
}
// TODO: add support for invoking some keywords at compile time!
#define load32BitLower(value) \
    molecule(Instruction::setImmediate16_Lowest(value), \
             Instruction::setImmediate16_Lower(value))

#define load32BitUpper(value) \
    molecule(Instruction::setImmediate16_Higher(value), \
             Instruction::setImmediate16_Highest(value))
             
#define load64BitImmediate(value) \
    load32BitLower(value), \
    load32BitUpper(value)

static constexpr auto ra = forth::TargetRegister::RegisterA;
static constexpr auto rb = forth::TargetRegister::RegisterB;
static constexpr auto rc = forth::TargetRegister::RegisterC;
static constexpr auto rt = forth::TargetRegister::RegisterT;
static constexpr auto rx = forth::TargetRegister::RegisterX;
static constexpr auto moveCtoA = Instruction::move(ra, rc);
static constexpr auto moveXtoA = Instruction::move(ra, rx);
static constexpr auto moveXtoB = Instruction::move(rb, rx);
static constexpr auto popA = Instruction::popA();
static constexpr auto popB = Instruction::popB();
static constexpr auto pushC = Instruction::pushC();
static constexpr auto pushA = Instruction::pushA();
static constexpr auto pushB = Instruction::pushB();

template<auto op>
void addBinaryOperation(forth::Machine& machine, const std::string& name, bool compileTimeInvoke = false) {
    machine.addMachineCodeWord<
        popA,
        popB,
        op,
        pushC>(name, compileTimeInvoke);
}

int main() {
    forth::Machine machine (std::cout, std::cin);
    machine.initializeBaseDictionary();
    // more custom words
    addBinaryOperation<Instruction::add()>(machine, "+");
    addBinaryOperation<Instruction::sub()>(machine, "-");
    addBinaryOperation<Instruction::mul()>(machine, "*");
    addBinaryOperation<Instruction::div()>(machine, "/");
    addBinaryOperation<Instruction::mod()>(machine, "mod");
	addBinaryOperation<Instruction::shiftRight()>(machine, ">>");
	addBinaryOperation<Instruction::shiftLeft()>(machine, "<<");
	addBinaryOperation<Instruction::andOp()>(machine, "and");
	addBinaryOperation<Instruction::orOp()>(machine, "or");
	addBinaryOperation<Instruction::greaterThan()>(machine, ">");
	addBinaryOperation<Instruction::lessThan()>(machine, "<");
	addBinaryOperation<Instruction::xorOp()>(machine, "xor");
	addBinaryOperation<Instruction::equals()>(machine, "eq");
	addBinaryOperation<Instruction::pow()>(machine, "pow");
	machine.addMachineCodeWord<
		popA,
		pushA,
		pushA>("dup");
	machine.addMachineCodeWord<
		popA,
		popB,
		pushA,
		pushB>("swap");
	machine.addMachineCodeWord<popA>("drop");
	machine.addMachineCodeWord<
		popA,
		popB,
		pushA>("nip");
	machine.addMachineCodeWord<
		popA,
		popB,
		pushB,
		pushA,
		pushB>("over");
    machine.addMachineCodeWord<
        popA,
        popB,
        Instruction::sub(),
        moveCtoA,
        popB,
        Instruction::sub(),
        pushC>("3+");
    machine.addMachineCodeWord<
        popA,
        popB,
        Instruction::sub(),
        moveCtoA,
        popB,
        Instruction::sub(),
        pushC>("3-");
    machine.addMachineCodeWord<
        popA, 
        popB,
        Instruction::mul(),
        moveCtoA,
        popB,
        Instruction::mul(),
        pushC>("3*");
	machine.addMachineCodeWord<
		popA,
		popB,
		pushA,
		pushB,
		pushA>("tuck"); // originally swap over

    machine.controlLoop();
    return 0;
}
