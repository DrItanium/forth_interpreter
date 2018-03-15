#include <iostream>
#include "Machine.h"
#include "Instruction.h"
#include "Assembler.h"
#include <variant>


namespace forth {
    template<typename T>
    EagerInstruction binaryOperationFunction(const std::string& name, T op) {
        return function(name, opPopRegisterAB(), 
                op,
                opPushRegisterC());
    }
#define DEFAULT_REGISTER_ARGS3 TargetRegister::C, TargetRegister::A, TargetRegister::B
    void systemSetup(forth::Machine& machine) {
        // initial system values that we need to use
        forth::AssemblerBuilder init;
        auto makeFunctionPrinter = [](const std::string& name, TargetRegister reg) {
            return instructions(opPrintChar(name),
                    opPrintChar(": "),
                    opTypeDatum(reg),
                    opSubroutineCall("PrintNewline"));
        };
        init.addInstruction(
				directiveOrg(0),
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
                opStoreImmediate64(forth::Machine::terminateControlLoopLocation, "TerminateControlLoop"),
                // now start using the other system variables to 
                forth::opLeaveExecutionLoop(),
				directiveOrg(0x01000),
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
                        opSubroutineCall("WriteFalse")),
#define DispatchOneRegister(title) 
#define DispatchTwoRegister(title) 
#define DispatchThreeRegister(title) binaryOperationFunction( #title , op ## title ( DEFAULT_REGISTER_ARGS3 )),
#define DispatchSignedImm16(title)
#define DispatchImmediate24(title) 
#define DispatchTwoRegisterWithImm16(title) 
#define DispatchCustomTwoRegisterWithImm16(title) 
#define DispatchOneRegisterWithImm16(title) 
#define DispatchFourRegister(title) 
#define DispatchFiveRegister(title)
#define DispatchOneRegisterWithImm64(title) 
#define DispatchOneRegisterWithImm48(title) 
#define DispatchOneRegisterWithImm32(title)
#define DispatchNoArguments(title) 
#define X(title, b) \
	INDIRECTION(Dispatch, b)(title)
#define FirstX(title, b) X(title, b)
#include "InstructionData.def"
#undef X
#undef FirstX
#undef DispatchNoArguments
#undef DispatchOneRegister
#undef DispatchTwoRegister
#undef DispatchThreeRegister
#undef DispatchSignedImm16
#undef DispatchImmediate24
#undef DispatchTwoRegisterWithImm16
#undef DispatchOneRegisterWithImm16
#undef DispatchFiveRegister
#undef DispatchFourRegister
#undef DispatchOneRegisterWithImm48
#undef DispatchOneRegisterWithImm32
#undef DispatchOneRegisterWithImm64
				directiveOrg(0x02000),
				label("NativeDispatchTable"),
				directiveAddresses(
                        "StoreValue",
						"LoadValue",
						"WriteFalse",
						"WriteTrue",
						"GetParameterStackEmpty",
						"GetSubroutineStackEmpty",
						"ClearSubroutineStack",
						"ActivateCompilationMode",
						"DeactivateCompilationMode",
						"InCompilationMode",
						"DispatchInstruction",
						"ShouldKeepExecuting",
						"PrintNewline",
						"PrintRegisters",
						"PrintStack",
						"TerminateControlLoop",
						"InIgnoreInputMode"),
#define DispatchOneRegister(title) 
#define DispatchTwoRegister(title) 
#define DispatchThreeRegister(title) directiveAddress( #title ) ,
#define DispatchSignedImm16(title)
#define DispatchImmediate24(title) 
#define DispatchTwoRegisterWithImm16(title) 
#define DispatchCustomTwoRegisterWithImm16(title) 
#define DispatchOneRegisterWithImm16(title) 
#define DispatchFourRegister(title) 
#define DispatchFiveRegister(title)
#define DispatchOneRegisterWithImm64(title) 
#define DispatchOneRegisterWithImm48(title) 
#define DispatchOneRegisterWithImm32(title)
#define DispatchNoArguments(title) 
#define FirstX(title, b) INDIRECTION(Dispatch, b)(title)
#define X(title, b) FirstX(title, b)
#include "InstructionData.def"
#undef X
#undef FirstX
#undef DispatchNoArguments
#undef DispatchOneRegister
#undef DispatchTwoRegister
#undef DispatchThreeRegister
#undef DispatchSignedImm16
#undef DispatchImmediate24
#undef DispatchTwoRegisterWithImm16
#undef DispatchOneRegisterWithImm16
#undef DispatchFiveRegister
#undef DispatchFourRegister
#undef DispatchOneRegisterWithImm48
#undef DispatchOneRegisterWithImm32
#undef DispatchOneRegisterWithImm64
                    directiveAddress(0));
		machine.installInCore(init);
#undef DEFAULT_REGISTER_ARGS3
    }
}

int main() {
    try {
        forth::Machine machine (std::cout, std::cin);
        machine.initializeBaseDictionary();
	    forth::systemSetup(machine);
    } catch (forth::Problem& p) {
        std::cout << p.getWord() << ": " << p.getMessage() << std::endl;
        std::cout << "terminating...." << std::endl;
    }

    return 0;
}
