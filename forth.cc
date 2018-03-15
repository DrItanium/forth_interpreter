#include <iostream>
#include "Machine.h"
#include "Instruction.h"
#include "Assembler.h"
#include <variant>


//using Address = forth::Address;
//using Operation = forth::Operation;

//static constexpr auto ra = forth::TargetRegister::A;
//static constexpr auto rb = forth::TargetRegister::B;
//static constexpr auto rc = forth::TargetRegister::C;
//static constexpr auto rs = forth::TargetRegister::S;
//static constexpr auto rx = forth::TargetRegister::X;
//static constexpr auto moveCtoA = forth::move(ra, rc);
//static constexpr auto moveAtoB = forth::move(rb, ra);
//static constexpr auto popA = forth::popA();
//static constexpr auto popB = forth::popB();
//static constexpr auto popC = forth::popRegister(rc);
//static constexpr auto pushC = forth::pushC();
//static constexpr auto pushA = forth::pushA();
//static constexpr auto pushB = forth::pushB();


//template<auto op>
//void addBinaryOperation(forth::Machine& machine, const std::string& name, bool compileTimeInvoke = false) {
//	machine.addMachineCodeWord<
//		popA,
//		popB,
//		op,
//		pushC>(name, compileTimeInvoke);
//}
//
//template<auto op>
//void threeArgumentVersion(forth::Machine& machine, const std::string& name, bool compileTimeInvoke = false) {
//	machine.addMachineCodeWord<
//		popA,
//		popB,
//		op,
//		moveCtoA,
//		popB,
//		op,
//		pushC>(name);
//}
//void arithmeticOperators(forth::Machine& machine) {
//#define DefBinaryOp(fn, e, str) addBinaryOperation<forth:: fn()>(machine, str )
//#define DefBinaryOpU(fn, e, str) DefBinaryOp(fn ## u, Unsigned ## e , str "u")
//#define DefBinaryOpF(fn, e, str) DefBinaryOp(fn ## f, FloatingPoint ## e , str "f")
//#define DefBinaryOpB(fn, e, str) DefBinaryOp(fn ## b, Boolean ## e, str "b")
//#define DefBinaryOpS(fn, e, str) DefBinaryOp(fn, e, str)
//#define DefBinaryOpSU(fn, e, str) DefBinaryOpS(fn, e, str); DefBinaryOpU(fn, e, str)
//#define DefBinaryOpSUB(fn, e, str) DefBinaryOpSU(fn, e, str); DefBinaryOpB(fn, e, str)
//#define DefBinaryOpSUF(fn, e, str) DefBinaryOpSU(fn, e, str); DefBinaryOpF(fn, e, str)
//#define DefBinaryOpSUFB(fn, e, str) DefBinaryOpSUF(fn, e, str); DefBinaryOpB(fn, e, str)
//	DefBinaryOpSUF(add, Add, "+");
//	DefBinaryOpSUF(sub, Subtract, "-");
//	DefBinaryOpSUF(mul, Multiply, "*");
//	DefBinaryOpSUF(div, Divide, "/");
//	DefBinaryOpSU(mod, Modulo, "mod");
//	DefBinaryOpSU(shl, ShiftRight, ">>");
//	DefBinaryOpSU(shr, ShiftLeft, "<<");
//	DefBinaryOpSUF(cmpgt, GreaterThan, ">");
//	DefBinaryOpSUF(cmplt, LessThan, "<");
//	DefBinaryOpSUFB(cmpeq, Equals, "eq");
//	DefBinaryOpSUF(pow, Pow, "pow");
//	DefBinaryOpS(andl, And, "and");
//	DefBinaryOpU(and, And, "and");
//	DefBinaryOpB(and, And, "and");
//	DefBinaryOpS(orl, Or, "or");
//	DefBinaryOpU(or, Or, "or");
//	DefBinaryOpB(or, Or, "or");
//	DefBinaryOpS(xorl, Xor, "xor");
//	DefBinaryOpU(xor, Xor, "xor");
//	DefBinaryOpB(xor, Xor, "xor");
//	//threeArgumentVersion<add()>(machine, "3+");
//	//threeArgumentVersion<sub()>(machine, "3-");
//	//threeArgumentVersion<mul()>(machine, "3*");
//	//threeArgumentVersion<div()>(machine, "3/");
//	machine.addMachineCodeWord< popA, forth::mul(rc, ra, ra), pushC>("square");
//	machine.addMachineCodeWord< popA, forth::mulf(rc, ra, ra), pushC>("squaref");
//	machine.addMachineCodeWord< popA, forth::mulu(rc, ra, ra), pushC>("squareu");
//	machine.addMachineCodeWord< popA, forth::mul(rb, ra, ra),  forth::mul(), pushC>("cube");
//	machine.addMachineCodeWord< popA, forth::mulf(rb, ra, ra), forth::mulf(), pushC>("cubef");
//	machine.addMachineCodeWord< popA, forth::mulu(rb, ra, ra), forth::mulu(), pushC>("cubeu");
//	machine.addMachineCodeWord< popA, popB, forth::cmpeq(),  notb(rc, rc), pushC>("neq");
//	machine.addMachineCodeWord< popA, popB, forth::cmpeqf(), notb(rc, rc), pushC>("neqf");
//	machine.addMachineCodeWord< popA, popB, forth::cmpequ(), notb(rc, rc), pushC>("nequ");
//	machine.addMachineCodeWord< popA, popB, forth::cmpeqb(), notb(rc, rc), pushC>("neqb");
//
//	machine.addMachineCodeWord< popA, forth::notl(), pushC>("not");
//	machine.addMachineCodeWord< popA, forth::notlu(), pushC>("notu");
//	machine.addMachineCodeWord< popA, forth::notb(), pushC>("notb");
//	machine.addMachineCodeWord< popA, forth::minusl(), pushC>("minus");
//	machine.addMachineCodeWord<popA, forth::minusf(), pushC>("minusf");
//	machine.addMachineCodeWord<popA, forth::minuslu(), pushC>("minusu");
//	machine.addMachineCodeWord<popA, forth::load(rc, ra), pushC>("load");
//	machine.addMachineCodeWord<popA, forth::load(ra, ra), load(rc, ra), pushC>("iload");
//	machine.addMachineCodeWord<popA, popB, store(ra, rb)>("store");
//	machine.addMachineCodeWord<popA, popB, load(ra, ra), store(ra, rb)>("istore");
//	machine.buildWord("@", "load");
//	machine.buildWord("@@", "iload");
//	machine.buildWord("=", "store");
//	machine.buildWord("==", "istore");
//}
//void stackOperators(forth::Machine& machine) {
//	machine.addMachineCodeWord<
//		popA,
//		pushA,
//		pushA>("dup");
//	machine.addMachineCodeWord<
//		popA,
//		popB,
//		pushB,
//		pushA,
//		pushB,
//		pushA>("2dup");
//	machine.addMachineCodeWord<
//		popA,
//		popB,
//		pushA,
//		pushB>("swap");
//	machine.addMachineCodeWord<popA>("drop");
//	machine.addMachineCodeWord<popA, popB>("2drop");
//	machine.addMachineCodeWord<
//		popA,
//		popB,
//		pushA>("nip");
//	machine.addMachineCodeWord<
//		popA,
//		popB,
//		pushB,
//		pushA,
//		pushB>("over");
//	machine.addMachineCodeWord<
//		popA,
//		popB,
//		pushA,
//		pushB,
//		pushA>("tuck"); // originally swap over
//
//	machine.addMachineCodeWord<
//		popC,
//		popB,
//		popA,
//		pushB,
//		pushC,
//		pushA>("rot");
//	machine.addMachineCodeWord<
//		popC,
//		popB,
//		popA,
//		pushC,
//		pushA,
//		pushB>("-rot");
//}
//#define enumWord(title, value) \
//	machine.buildWord(title, value)
//#define registerWord(name) \
//	enumWord ( ( "R" #name ) , forth::TargetRegister:: name )
//void registerDecls(forth::Machine& machine) {
//	registerWord(A);
//	registerWord(B);
//	registerWord(C);
//	registerWord(S);
//	registerWord(X);
//	registerWord(SP);
//	registerWord(SP2);
//}
//#undef registerWord
//#undef enumWord
//void microarchitectureWords(forth::Machine& machine) {
//	machine.addMachineCodeWord<popA>("pop.a");
//	machine.addMachineCodeWord<popB>("pop.b");
//	machine.addMachineCodeWord<pushC>("push.c");
//	machine.addMachineCodeWord<pushA>("push.a");
//	machine.addMachineCodeWord<pushB>("push.b");
//	machine.addMachineCodeWord<pushRegister(rs)>("push.s");
//#define pushPopGeneric(postfix, target) \
//	machine.addMachineCodeWord<pushRegister(forth::TargetRegister:: target)> ("push." #postfix); \
//	machine.addMachineCodeWord<popRegister(forth::TargetRegister:: target)> ("pop." #postfix)
//	pushPopGeneric(x, X);
//	pushPopGeneric(sp, SP);
//	pushPopGeneric(sp2, SP2);
//#undef pushPopGeneric
//
//	machine.addMachineCodeWord<moveCtoA>("c->a");
//	machine.addMachineCodeWord<moveAtoB>("a->b");
//	machine.addMachineCodeWord<popA, popB>("pop.ab");
//	machine.addMachineCodeWord<popA, typeval(ra)>(",");
//	machine.addMachineCodeWord<popA, typevalu(ra)>(",u");
//	machine.addMachineCodeWord<popA, typevalf(ra)>(",f");
//	machine.addMachineCodeWord<popA, typevalb(ra)>(",b");
//	machine.addMachineCodeWord<swap(ra, rb)>("swap.ab");
//}
namespace forth {
    template<typename T>
    EagerInstruction binaryOperationFunction(const std::string& name, T op) {
        return function(name, opPopRegisterAB(), 
                op,
                opPushRegisterC());
    }
    void systemSetup(forth::Machine& machine) {
        // initial system values that we need to use
        forth::AssemblerBuilder init(0);
        auto makeFunctionPrinter = [](const std::string& name, TargetRegister reg) {
            return instructions(opPrintChar(name),
                    opPrintChar(": "),
                    opTypeDatum(reg),
                    opSubroutineCall("PrintNewline"));
        };
        init.addInstruction(

                opStoreImmediate64(forth::Machine::shouldKeepExecutingLocation, 1),
                opStoreImmediate64(forth::Machine::isCompilingLocation, 0),
                opStoreImmediate64(forth::Machine::ignoreInputLocation, 0),
                opStoreImmediate64(forth::Machine::subroutineStackEmptyLocation, 0xFF0000),
                opStoreImmediate64(forth::Machine::subroutineStackFullLocation, 0xFE0000),
                opStoreImmediate64(forth::Machine::parameterStackEmptyLocation, 0xFE0000),
                opStoreImmediate64(forth::Machine::parameterStackFullLocation, 0xFD0000),
                opLoadImmediate64(TargetRegister::X, forth::Machine::subroutineStackEmptyLocation),
                opLoad(forth::TargetRegister::SP2, TargetRegister::X),
                opLoadImmediate64(TargetRegister::X, forth::Machine::parameterStackEmptyLocation),
                opLoad(forth::TargetRegister::SP, TargetRegister::X),
                opStoreImmediate64(forth::Machine::terminateControlLoopLocation, "terminateControlLoop"),
                // now start using the other system variables to 
                forth::opLeaveExecutionLoop(),
                label("InvokeAndReturnToMicrocode"),
                
                // the S register has been prepped by the microcode to execute a routine
                opCallSubroutineIndirect(TargetRegister::S),
                forth::opLeaveExecutionLoop(),
                function("StoreValue",
                    opPopRegisterAB(),
                    opStore(TargetRegister::B, TargetRegister::A)),
                function("LoadValue",
                        opPopRegisterA(),
                        opLoad(TargetRegister::C, TargetRegister::A),
                        opPushRegisterC()),
                function("WriteFalse",
                        opPushRegister(TargetRegister::Zero),
                        opSubroutineCall("StoreValue")),
                function("WriteTrue",
                        opLoadImmediate16(TargetRegister::A, 1),
                        opPushRegisterA(),
                        opSubroutineCall("StoreValue")),
                function("GetParameterStackEmpty",
                        opPushImmediate64(Core::spStackEmpty),
                        opSubroutineCall("LoadValue")),
                function("GetSubroutineStackEmpty",
                        opPushImmediate64(Core::sp2StackEmpty),
                        opSubroutineCall("LoadValue")),
                function("ClearSubroutineStack",
                        opSubroutineCall("GetSubroutineStackEmpty"),
                        opPopRegisterA(),
                        opMove(TargetRegister::SP2, TargetRegister::A)),
                function("ClearParameterStack",
                        opSubroutineCall("GetParameterStackEmpty"),
                        opPopRegisterA(),
                        opMove(TargetRegister::SP, TargetRegister::A)),
                binaryOperationFunction("EqualsAddress", 
                        opEquals(TargetRegister::C, TargetRegister::A, TargetRegister::B)),
                binaryOperationFunction("NotEqualsAddress", 
                        opNotEqual(TargetRegister::C, TargetRegister::A, TargetRegister::B)),
                function("NotEqualZero",
                        opPopRegisterA(),
                        opNotEqual(TargetRegister::C, TargetRegister::A, TargetRegister::Zero),
                        opPushRegisterC()),
                function("MemoryLocationNotEqualZero", // ( addr -- c )
                        opSubroutineCall("LoadValue"),
                        opSubroutineCall("NotEqualZero")),
                function("ActivateCompilationMode",
                        opPushImmediate64(Machine::isCompilingLocation),
                        opSubroutineCall("WriteTrue")),
                function("DeactivateCompilationMode",
                        opPushImmediate64(Machine::isCompilingLocation),
                        opSubroutineCall("WriteFalse")),
                function("InCompilationMode",
                        opPushImmediate64(Machine::isCompilingLocation),
                        opSubroutineCall("MemoryLocationNotEqualZero")),
                function("InIgnoreInputMode",
                        opPushImmediate64(Machine::ignoreInputLocation),
                        opSubroutineCall("MemoryLocationNotEqualZero")),
                function("DispatchInstruction",
                        opPopRegisterA(),
                        opCallSubroutineIndirect(TargetRegister::A)),
                function("ShouldKeepExecuting",
                        opPushImmediate64(Machine::shouldKeepExecutingLocation),
                        opSubroutineCall("MemoryLocationNotEqualZero")),
                function("PrintNewline", opPrintChar("\n")),
                function("PrintRegisters",
                        makeFunctionPrinter("SP", TargetRegister::SP),
                        makeFunctionPrinter("SP2", TargetRegister::SP2),
                        makeFunctionPrinter("A", TargetRegister::A),
                        makeFunctionPrinter("B", TargetRegister::B),
                        makeFunctionPrinter("C", TargetRegister::C),
                        makeFunctionPrinter("X", TargetRegister::X),
                        makeFunctionPrinter("S", TargetRegister::S),
                        makeFunctionPrinter("Index", TargetRegister::Index),
                        makeFunctionPrinter("DP", TargetRegister::DP)),
                function("PrintStack",
                        opLoadImmediate(TargetRegister::X, Machine::parameterStackEmptyLocation),
                        opLoad(TargetRegister::X, TargetRegister::X),
                        zeroRegister(TargetRegister::A),
                        opMove(TargetRegister::B, TargetRegister::SP),
                        opEquals(TargetRegister::C, TargetRegister::X, TargetRegister::B),
                        opConditionalBranch(TargetRegister::C, "PrintStack_Done"),
                        label("PrintStack_LoopRestart"),
                        opLoad(TargetRegister::A, TargetRegister::B),
                        opUnsignedAddImmediate(TargetRegister::B, TargetRegister::B, 8),
                        opNotEqual(TargetRegister::C, TargetRegister::X, TargetRegister::B),
                        opPrintChar("\t- "),
                        opTypeDatum(TargetRegister::A),
                        opSubroutineCall("PrintNewline"),
                        opConditionalBranch(TargetRegister::C, "PrintStack_LoopRestart"),
                        label("PrintStack_Done")),
                function("TerminateControlLoop",
                        opPushImmediate64(Machine::shouldKeepExecutingLocation),
                        opSubroutineCall("WriteFalse"))
                    );
    }
}



//template<auto op>
//void addBinaryOperation(forth::Machine& mach, const std::string& name) {
//	mach.addMachineCodeWord<
//		forth::popA(),
//		forth::popB(),
//		op,
//		forth::pushC()>(name);
//}
int main() {
    //forth::Machine machine (std::cout, std::cin);
    //machine.initializeBaseDictionary();
    //addBinaryOperation<forth::add()>(machine, "+");
    //addBinaryOperation<forth::addf()>(machine, "+f");
    //addBinaryOperation<forth::sub()>(machine, "-");
    //addBinaryOperation<forth::subf()>(machine, "-f");
    //machine.addMachineCodeWord<forth::addiu(forth::TargetRegister::C, forth::TargetRegister::Zero, 1), pushC>("true");
    //machine.addMachineCodeWord<forth::zeroRegister(forth::TargetRegister::C), pushC>("false");
    //machine.addMachineCodeWord<forth::popA()>("drop");
    //machine.addMachineCodeWord<forth::popA(), forth::popA()>("2drop");
    //machine.addMachineCodeWord<forth::popA(), forth::pushA(), forth::pushA()>("dup");
    //microarchitectureWords(machine);
    //arithmeticOperators(machine);
    //stackOperators(machine);
    //registerDecls(machine);
    //forth::systemSetup(machine);
    //machine.controlLoop();

    return 0;
}
