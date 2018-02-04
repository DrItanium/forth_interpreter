#include <iostream>
#include "Machine.h"
#include "Instruction.h"


using Address = forth::Address;
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
static constexpr auto moveAtoB = Instruction::move(rb, ra);
static constexpr auto popA = Instruction::popA();
static constexpr auto popB = Instruction::popB();
static constexpr auto popC = Instruction::popRegister(rc);
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

template<auto op>
void threeArgumentVersion(forth::Machine& machine, const std::string& name, bool compileTimeInvoke = false) {
	machine.addMachineCodeWord<
		popA,
		popB,
		op,
		moveCtoA,
		popB,
		op,
		pushC>(name);
}
void arithmeticOperators(forth::Machine& machine) {
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
	threeArgumentVersion<Instruction::add()>(machine, "3+");
	threeArgumentVersion<Instruction::sub()>(machine, "3-");
	threeArgumentVersion<Instruction::mul()>(machine, "3*");
	threeArgumentVersion<Instruction::div()>(machine, "3/");
	machine.addMachineCodeWord<popA, moveAtoB, Instruction::mul(), pushC>("square");
	machine.addMachineCodeWord<popA, moveAtoB, Instruction::mul(), moveCtoA, Instruction::mul(), pushC>("cube");
}
void stackOperators(forth::Machine& machine) {
	machine.addMachineCodeWord<
		popA,
		pushA,
		pushA>("dup");
	machine.addMachineCodeWord<
		popA,
		popB,
		pushB,
		pushA,
		pushB,
		pushA>("2dup");
	machine.addMachineCodeWord<
		popA,
		popB,
		pushA,
		pushB>("swap");
	machine.addMachineCodeWord<popA>("drop");
	machine.addMachineCodeWord<popA, popB>("2drop");
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
		pushA,
		pushB,
		pushA>("tuck"); // originally swap over

	machine.addMachineCodeWord<
		popC,
		popB,
		popA,
		pushB,
		pushC,
		pushA>("rot");
	machine.addMachineCodeWord<
		popC,
		popB,
		popA,
		pushC,
		pushA,
		pushB>("-rot");
}
#define enumWord(title, value) \
	machine.buildWord(title, false, value)
#define registerWord(name) \
	enumWord ( ( "R" #name ) , forth::TargetRegister:: Register ## name )
void registerDecls(forth::Machine& machine) {
	registerWord(A);
	registerWord(B);
	registerWord(C);
	registerWord(S);
	registerWord(X);
	registerWord(T);
	registerWord(TA);
	registerWord(TB);
	registerWord(TX);
	registerWord(IP);
	registerWord(SP);
	registerWord(SP2);
}
#undef registerWord
#define discriminantWord(name) \
	enumWord( "discriminant:" #name , forth::Discriminant:: name )
void addDiscriminantWords(forth::Machine& machine) {
	discriminantWord(Number);
	discriminantWord(MemoryAddress);
	discriminantWord(FloatingPoint);
	discriminantWord(Boolean);
	discriminantWord(Word);
	discriminantWord(Molecule);
	discriminantWord(DictionaryEntry);
	machine.buildWord("t.signed", false, "discriminant:Number", "pop.t");
	machine.buildWord("t.fp", false, "discriminant:FloatingPoint", "pop.t");
	machine.buildWord("t.address", false, "discriminant:MemoryAddress", "pop.t");
	machine.buildWord("t.boolean", false, "discriminant:Boolean", "pop.t");
	machine.buildWord("t.word", false, "discriminant:Word", "pop.t");
	machine.buildWord("t.molecule", false, "discriminant:Molecule", "pop.t");
	machine.buildWord("t.dict-entry", false, "discriminant:DictionaryEntry", "pop.t");
}
#undef discriminantWord
#undef enumWord
void microarchitectureWords(forth::Machine& machine) {
	machine.addMachineCodeWord<Instruction::stop()>("nop");
	machine.addMachineCodeWord<Instruction::popT()>("pop.t");
	machine.addMachineCodeWord<Instruction::pushRegister(forth::TargetRegister::RegisterT)>("push.t");
	machine.addMachineCodeWord<popA>("pop.a");
	machine.addMachineCodeWord<popB>("pop.b");
	machine.addMachineCodeWord<pushC>("push.c");
	machine.addMachineCodeWord<pushA>("push.a");
	machine.addMachineCodeWord<pushB>("push.b");
	machine.addMachineCodeWord<Instruction::pushRegister(forth::TargetRegister::RegisterS)>("push.s");
#define pushPopGeneric(postfix, target) \
	machine.addMachineCodeWord<Instruction::pushRegister(forth::TargetRegister:: Register ## target)> ("push." #postfix); \
	machine.addMachineCodeWord<Instruction::popRegister(forth::TargetRegister:: Register ## target)> ("pop." #postfix)
	pushPopGeneric(x, X);
	pushPopGeneric(ta, TA);
	pushPopGeneric(tb, TB);
	pushPopGeneric(tx, TX);
	pushPopGeneric(ip, IP);
	pushPopGeneric(sp, SP);
	pushPopGeneric(sp2, SP2);
#undef pushPopGeneric

	machine.addMachineCodeWord<moveCtoA>("c->a");
	machine.addMachineCodeWord<moveAtoB>("a->b");
	machine.addMachineCodeWord<moveXtoA>("x->a");
	machine.addMachineCodeWord<moveXtoB>("x->b");
	machine.addMachineCodeWord<moveXtoA, moveXtoB>("x->a,b");
}
int main() {
    forth::Machine machine (std::cout, std::cin);
	microarchitectureWords(machine);
    machine.initializeBaseDictionary();
	arithmeticOperators(machine);
	stackOperators(machine);
	registerDecls(machine);
	addDiscriminantWords(machine);
    machine.controlLoop();
    return 0;
}
