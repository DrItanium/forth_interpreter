#include <iostream>
#include "Machine.h"
#include "Instruction.h"
#include "Assembler.h"
#include <variant>


using Address = forth::Address;
using Operation = forth::Operation;
namespace Instruction = forth::Instruction;

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
#define DefBinaryOp(fn, e, str) addBinaryOperation<Instruction:: fn()>(machine, str )
#define DefBinaryOpU(fn, e, str) DefBinaryOp(fn ## u, Unsigned ## e , str "u")
#define DefBinaryOpF(fn, e, str) DefBinaryOp(fn ## f, FloatingPoint ## e , str "f")
#define DefBinaryOpB(fn, e, str) DefBinaryOp(fn ## b, Boolean ## e, str "b")
#define DefBinaryOpS(fn, e, str) DefBinaryOp(fn, e, str)
#define DefBinaryOpSU(fn, e, str) DefBinaryOpS(fn, e, str); DefBinaryOpU(fn, e, str)
#define DefBinaryOpSUB(fn, e, str) DefBinaryOpSU(fn, e, str); DefBinaryOpB(fn, e, str)
#define DefBinaryOpSUF(fn, e, str) DefBinaryOpSU(fn, e, str); DefBinaryOpF(fn, e, str)
#define DefBinaryOpSUFB(fn, e, str) DefBinaryOpSUF(fn, e, str); DefBinaryOpB(fn, e, str)
    DefBinaryOpSUF(add, Add, "+");
    DefBinaryOpSUF(sub, Subtract, "-");
    DefBinaryOpSUF(mul, Multiply, "*");
    DefBinaryOpSUF(div, Divide, "/");
    DefBinaryOpSU(mod, Modulo, "mod");
    DefBinaryOpSU(shl, ShiftRight, ">>");
    DefBinaryOpSU(shr, ShiftLeft, "<<");
    DefBinaryOpSUF(cmpgt, GreaterThan, ">");
    DefBinaryOpSUF(cmplt, LessThan, "<");
    DefBinaryOpSUFB(cmpeq, Equals, "eq");
    DefBinaryOpSUF(pow, Pow, "pow");
	DefBinaryOpS(andl, And, "and");
	DefBinaryOpU(and, And, "and");
	DefBinaryOpB(and, And, "and");
	DefBinaryOpS(orl, Or, "or");
	DefBinaryOpU(or, Or, "or");
	DefBinaryOpB(or, Or, "or");
	DefBinaryOpS(xorl, Xor, "xor");
	DefBinaryOpU(xor, Xor, "xor");
	DefBinaryOpB(xor, Xor, "xor");
	//threeArgumentVersion<Instruction::add()>(machine, "3+");
	//threeArgumentVersion<Instruction::sub()>(machine, "3-");
	//threeArgumentVersion<Instruction::mul()>(machine, "3*");
	//threeArgumentVersion<Instruction::div()>(machine, "3/");
	machine.addMachineCodeWord<
		popA, 
		Instruction::mul(rc, ra, ra), 
		pushC>("square");
	machine.addMachineCodeWord<
		popA, 
		Instruction::mulf(rc, ra, ra), 
		pushC>("squaref");
	machine.addMachineCodeWord<
		popA, 
		Instruction::mulu(rc, ra, ra), 
		pushC>("squareu");
	machine.addMachineCodeWord<
		popA, 
		Instruction::mul(rb, ra, ra),
		Instruction::mul(),
		pushC>("cube");
	machine.addMachineCodeWord<
		popA, 
		Instruction::mulf(rb, ra, ra),
		Instruction::mulf(),
		pushC>("cubef");
	machine.addMachineCodeWord<
		popA, 
		Instruction::mulu(rb, ra, ra),
		Instruction::mulu(),
		pushC>("cubeu");
    /// @todo: the not op needs to be the boolean kind
	machine.addMachineCodeWord<
		popA,
		popB,
		(Instruction::cmpeq()),  // C = top == equals 
		Instruction::notb(rc, rc), 
		pushC>("neq");
    machine.addMachineCodeWord<
        popA, 
        popB, 
        Instruction::cmpeqf(),
        Instruction::notb(rc, rc),
        pushC>("neqf");
    machine.addMachineCodeWord<
        popA, 
        popB, 
        Instruction::cmpequ(),
        Instruction::notb(rc, rc),
        pushC>("nequ");
    machine.addMachineCodeWord<
        popA, 
        popB, 
        Instruction::cmpeqb(),
        Instruction::notb(rc, rc),
        pushC>("neqb");

	machine.addMachineCodeWord<popA, Instruction::notl(), pushC>("not");
	machine.addMachineCodeWord<popA, Instruction::notlu(), pushC>("notu");
	machine.addMachineCodeWord<popA, Instruction::notb(), pushC>("notb");
	machine.addMachineCodeWord<popA, Instruction::minusl(), pushC>("minus");
	machine.addMachineCodeWord<popA, Instruction::minusf(), pushC>("minusf");
	machine.addMachineCodeWord<popA, Instruction::minuslu(), pushC>("minusu");
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
	pushPopGeneric(sp, SP);
	pushPopGeneric(sp2, SP2);
#undef pushPopGeneric

	machine.addMachineCodeWord<moveCtoA>("c->a");
	machine.addMachineCodeWord<moveAtoB>("a->b");
	machine.addMachineCodeWord<popA, popB>("pop.ab");
	machine.addMachineCodeWord<popA, Instruction::typeval(ra)>(",");
	machine.addMachineCodeWord<popA, Instruction::typevalu(ra)>(",u");
	machine.addMachineCodeWord<popA, Instruction::typevalf(ra)>(",f");
	machine.addMachineCodeWord<popA, Instruction::typevalb(ra)>(",b");
	machine.addMachineCodeWord<Instruction::swap(ra, rb)>("swap.ab");
    machine.addMachineCodeWord<
        Instruction::loadLowerImmediate48(forth::TargetRegister::X, forth::Machine::shouldKeepExecutingLocation),
        Instruction::setImmediate64_Highest(forth::TargetRegister::X, forth::Machine::shouldKeepExecutingLocation),
        Instruction::xorl(rc, ra, ra),
        Instruction::store(rx, rc)>("quit");
}
void compoundWords(forth::Machine& machine) {
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
