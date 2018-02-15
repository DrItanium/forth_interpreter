#include <iostream>
#include "Machine.h"
#include "Instruction.h"
#include <variant>


using Address = forth::Address;
using Operation = forth::Operation;
namespace Instruction = forth::Instruction;
template<typename T, typename ... Args>
constexpr forth::Address molecule(T first, Args&& ... rest) noexcept {
    return Instruction::encodeOperation(first, std::move(rest)...);
}

static constexpr auto ra = forth::TargetRegister::A;
static constexpr auto rb = forth::TargetRegister::B;
static constexpr auto rc = forth::TargetRegister::C;
static constexpr auto rx = forth::TargetRegister::X;
static constexpr auto moveCtoA = Instruction::move(ra, rc);
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
#define DefBinaryOp(fn, e, str) addBinaryOperation<Instruction:: fn( Operation:: e )>(machine, str )
#define DefBinaryOpU(fn, e, str) DefBinaryOp(fn, Unsigned ## e , str "u")
#define DefBinaryOpF(fn, e, str) DefBinaryOp(fn, FloatingPoint ## e , str "f")
    DefBinaryOp(add, Add, "+");
    DefBinaryOpU(add, Add, "+");
    addBinaryOperation<Instruction::add(forth::Operation::FloatingPointAdd)>(machine, "+f");
    addBinaryOperation<Instruction::sub()>(machine, "-");
    addBinaryOperation<Instruction::sub(forth::Operation::FloatingPointSubtract)>(machine, "-f");
    addBinaryOperation<Instruction::sub(forth::Operation::UnsignedSubtract)>(machine, "-u");
    addBinaryOperation<Instruction::mul()>(machine, "*");
    addBinaryOperation<Instruction::mul(Operation::FloatingPointMultiply)>(machine, "*f");
    addBinaryOperation<Instruction::mul(Operation::UnsignedMultiply)>(machine, "*u");
    addBinaryOperation<Instruction::div()>(machine, "/");
    addBinaryOperation<Instruction::div(Operation::FloatingPointDivide)>(machine, "/f");
    addBinaryOperation<Instruction::div(Operation::UnsignedDivide)>(machine, "/u");
    addBinaryOperation<Instruction::mod()>(machine, "mod");
    addBinaryOperation<Instruction::mod(Operation::UnsignedModulo)>(machine, "modu");
	addBinaryOperation<Instruction::shiftRight()>(machine, ">>");
	addBinaryOperation<Instruction::shiftRight(Operation::UnsignedShiftRight)>(machine, ">>u");
	addBinaryOperation<Instruction::shiftLeft()>(machine, "<<");
	addBinaryOperation<Instruction::shiftLeft(Operation::UnsignedShiftLeft)>(machine, "<<u");
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
	machine.addMachineCodeWord<popA, Instruction::mul(rc, ra, ra), pushC>("square");
	machine.addMachineCodeWord<
		popA, 
		Instruction::mul(rb, ra, ra),
		Instruction::mul(),
		pushC>("cube");
	machine.addMachineCodeWord<
		popA,
		popB,
		Instruction::equals(),  // C = top == equals 
		Instruction::notOp(rc, rc), 
		pushC>("neq");
	machine.addMachineCodeWord<popA, Instruction::notOp(), pushC>("not");
	machine.addMachineCodeWord<popA, Instruction::minus(), pushC>("minus");
	machine.addMachineCodeWord<popA, Instruction::load(rc, ra), pushC>("load");
	machine.addMachineCodeWord<popA, Instruction::load(ra, ra), Instruction::load(rc, ra), pushC>("iload");
	machine.addMachineCodeWord<popA, popB, Instruction::store(ra, rb)>("store");
	machine.addMachineCodeWord<popA, popB, Instruction::load(ra, ra), Instruction::store(ra, rb)>("istore");
	machine.buildWord("@", "load");
	machine.buildWord("@@", "iload");
	machine.buildWord("=", "store");
	machine.buildWord("==", "istore");
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
	machine.buildWord(title, value)
#define registerWord(name) \
	enumWord ( ( "R" #name ) , forth::TargetRegister:: name )
void registerDecls(forth::Machine& machine) {
	registerWord(A);
	registerWord(B);
	registerWord(C);
	registerWord(S);
	registerWord(X);
	//registerWord(T);
	//registerWord(TA);
	//registerWord(TB);
	//registerWord(TX);
	registerWord(SP);
	registerWord(SP2);
}
#undef registerWord
#define discriminantWord(name) \
	enumWord( "discriminant:" #name , forth::Discriminant:: name )
void addDiscriminantWords(forth::Machine& machine) {
	//discriminantWord(Number);
	//discriminantWord(MemoryAddress);
	//discriminantWord(FloatingPoint);
	//discriminantWord(Boolean);
	//discriminantWord(Word);
	//discriminantWord(Molecule);
	//discriminantWord(DictionaryEntry);
	//machine.buildWord("t.signed", "discriminant:Number", "pop.t");
	//machine.buildWord("t.fp", "discriminant:FloatingPoint", "pop.t");
	//machine.buildWord("t.address", "discriminant:MemoryAddress", "pop.t");
	//machine.buildWord("t.boolean", "discriminant:Boolean", "pop.t");
	//machine.buildWord("t.word", "discriminant:Word", "pop.t");
	//machine.buildWord("t.molecule", "discriminant:Molecule", "pop.t");
	//machine.buildWord("t.dict-entry", "discriminant:DictionaryEntry", "pop.t");
}
#undef discriminantWord
#undef enumWord
void microarchitectureWords(forth::Machine& machine) {
	machine.addMachineCodeWord<Instruction::stop()>("nop");
	machine.addMachineCodeWord<popA>("pop.a");
	machine.addMachineCodeWord<popB>("pop.b");
	machine.addMachineCodeWord<pushC>("push.c");
	machine.addMachineCodeWord<pushA>("push.a");
	machine.addMachineCodeWord<pushB>("push.b");
	machine.addMachineCodeWord<Instruction::pushRegister(forth::TargetRegister::S)>("push.s");
#define pushPopGeneric(postfix, target) \
	machine.addMachineCodeWord<Instruction::pushRegister(forth::TargetRegister:: target)> ("push." #postfix); \
	machine.addMachineCodeWord<Instruction::popRegister(forth::TargetRegister:: target)> ("pop." #postfix)
	pushPopGeneric(x, X);
	//pushPopGeneric(ta, TA);
	//pushPopGeneric(tb, TB);
	//pushPopGeneric(tx, TX);
	pushPopGeneric(sp, SP);
	pushPopGeneric(sp2, SP2);
#undef pushPopGeneric

	machine.addMachineCodeWord<moveCtoA>("c->a");
	machine.addMachineCodeWord<moveAtoB>("a->b");
	machine.addMachineCodeWord<popA, popB>("pop.ab");
	machine.addMachineCodeWord<popA, Instruction::typeValue()>(",");
	machine.addMachineCodeWord<Instruction::swap(ra, rb)>("swap.ab");
    machine.addMoleculeSequence<
        Instruction::loadAddressLowerHalf(forth::TargetRegister::X, forth::Machine::shouldKeepExecutingLocation),
        Instruction::loadAddressUpperHalf(forth::TargetRegister::X, forth::Machine::shouldKeepExecutingLocation),
        Instruction::encodeOperation(Instruction::xorOp(rc, ra, ra),
                Instruction::store(rx, rc))>("quit");
}
void compoundWords(forth::Machine& machine) {
	//machine.buildWord(",u", "t.address", ",");
	//machine.buildWord(",f", "t.fp", ",");
	//machine.buildWord(",b", "t.boolean", ",");
	//machine.buildWord(",", "t.signed", ",");
	//machine.buildWord("negate", "t.signed", "not");
	//machine.buildWord("negateu", "t.address", "not");
	//machine.buildWord("not", "t.boolean", "not");
	//machine.buildWord("minusf", "t.fp", "minus");
	//machine.buildWord("minus", "t.signed", "minus");
}
void systemSetup(forth::Machine& machine) {
	// initial system values that we need to use
	machine.store(forth::Machine::shouldKeepExecutingLocation, true);
	machine.store(forth::Machine::isCompilingLocation, false);
	machine.store(forth::Machine::ignoreInputLocation, false);
	machine.store(forth::Machine::subroutineStackEmptyLocation, Address(0xFF0000));
	machine.store(forth::Machine::subroutineStackFullLocation, Address(0xFE0000));
	machine.store(forth::Machine::parameterStackEmptyLocation, Address(0xFE0000));
	machine.store(forth::Machine::parameterStackFullLocation, Address(0xFD0000));
	// TODO: set SP2 to the correct register!
	machine.dispatchInstruction(Instruction::loadAddressLowerHalf(forth::TargetRegister::X, forth::Machine::subroutineStackEmptyLocation));
	machine.dispatchInstruction(Instruction::loadAddressUpperHalf(forth::TargetRegister::X, forth::Machine::subroutineStackEmptyLocation));
	machine.dispatchInstruction(Instruction::encodeOperation(Instruction::load(forth::TargetRegister::SP2, forth::TargetRegister::X)));
	machine.dispatchInstruction(Instruction::loadAddressLowerHalf(forth::TargetRegister::X, forth::Machine::parameterStackEmptyLocation));
	machine.dispatchInstruction(Instruction::loadAddressUpperHalf(forth::TargetRegister::X, forth::Machine::parameterStackEmptyLocation));
	machine.dispatchInstruction(Instruction::encodeOperation(Instruction::load(forth::TargetRegister::SP, forth::TargetRegister::X)));
}
int main() {
    forth::Machine machine (std::cout, std::cin);
    machine.initializeBaseDictionary();
	microarchitectureWords(machine);
	arithmeticOperators(machine);
	stackOperators(machine);
	registerDecls(machine);
	addDiscriminantWords(machine);
	compoundWords(machine);
	systemSetup(machine);
    machine.controlLoop();

    return 0;
}
