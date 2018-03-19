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
    template<typename T>
    EagerInstruction unaryOperationFunction(const std::string& name, T op) {
        return function(name, opPopRegisterA(), 
                op,
                opPushRegisterC());
    }
#define DEFAULT_REGISTER_ARGS2 TargetRegister::C, TargetRegister::A
#define DEFAULT_REGISTER_ARGS3 DEFAULT_REGISTER_ARGS2 , TargetRegister::B
    void systemSetup(forth::Machine& machine) {
		forth::Compiler init;
        // initial system values that we need to use
        auto makeFunctionPrinter = [](const std::string& name, TargetRegister reg) {
            return instructions(opPrintChar(name),
                    opPrintChar(": "),
                    opTypeDatum(reg),
                    opSubroutineCall("PrintNewline"));
        };
        init.addInstruction(
                directiveOrg(0),
                opStoreImmediate64(forth::Machine::locationSubroutineStackEmpty, 0xFF0000),
                opStoreImmediate64(forth::Machine::locationSubroutineStackFull, 0xFE0000),
                opStoreImmediate64(forth::Machine::locationParameterStackEmpty, 0xFE0000),
                opStoreImmediate64(forth::Machine::locationParameterStackFull, 0xFD0000),
                // User variable defines start!
                opStoreImmediate64(forth::Machine::locationShouldKeepExecuting, 1),
                opStoreImmediate64(forth::Machine::locationInCompilationMode, 0),
                opStoreImmediate64(forth::Machine::locationInIgnoreInputMode, 0),
#define UserVariable(x) \
                opLoadImmediate64(TargetRegister::Temporary, forth::Machine:: location ## x ), \
                opLoadImmediate64(TargetRegister::Temporary2, #x ), \
                opStore(TargetRegister::Temporary, TargetRegister::Temporary2),
#define UserVariableFirst(x) UserVariable(x)
#include "UserVariables.def"
#undef UserVariableFirst
#undef UserVariable
                opLoadImmediate64(TargetRegister::X, forth::Machine::locationSubroutineStackEmpty),
                opLoad(forth::TargetRegister::SP2, TargetRegister::X),
                opLoadImmediate64(TargetRegister::X, forth::Machine::locationParameterStackEmpty),
                opLoad(forth::TargetRegister::SP, TargetRegister::X),
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
                function("EqualZero",
                        opPopRegisterA(),
                        opEquals(TargetRegister::C, TargetRegister::A, TargetRegister::Zero),
                        opPushRegisterC()),
                function("MemoryLocationEqualZero", // ( addr -- c )
                        opSubroutineCall("LoadValue"),
                        opSubroutineCall("EqualZero")),
                function("ActivateCompilationMode",
                        opPushImmediate64(Machine::locationInCompilationMode),
                        opSubroutineCall("WriteTrue")),
                function("DeactivateCompilationMode",
                        opPushImmediate64(Machine::locationInCompilationMode),
                        opSubroutineCall("WriteFalse")),
                function("InCompilationMode",
                        opPushImmediate64(Machine::locationInCompilationMode),
                        opSubroutineCall("MemoryLocationNotEqualZero")),
                function("InIgnoreInputMode",
                        opPushImmediate64(Machine::locationInIgnoreInputMode),
                        opSubroutineCall("MemoryLocationNotEqualZero")),
                function("DispatchInstruction",
                        opPopRegisterA(),
                        opCallSubroutineIndirect(TargetRegister::A)),
                function("ShouldKeepExecuting",
                        opPushImmediate64(Machine::locationShouldKeepExecuting),
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
				function("TypeInteger",
						opPopRegisterA(),
						opTypeInteger(TargetRegister::A)),
				function("TypeFloatingPoint",
						opPopRegisterA(),
						opTypeFloatingPoint(TargetRegister::A)),
				function("TypeBoolean",
						opPopRegisterA(),
						opTypeBoolean(TargetRegister::A)),
				function("TypeUnsigned",
						opPopRegisterA(),
						opTypeUnsigned(TargetRegister::A)),
                function("PrintStack",
                        opLoadImmediate(TargetRegister::X, Machine::locationParameterStackEmpty),
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
                        opPushImmediate64(Machine::locationShouldKeepExecuting),
                        opSubroutineCall("WriteFalse")),
                function("IndirectLoad", // ( addr -- v )
                        opPopRegisterA(),
                        opLoad(TargetRegister::A, TargetRegister::A),
                        opLoad(TargetRegister::A, TargetRegister::A),
                        opPushRegisterA()),
                function("IndirectStore", // ( addr value -- )
                        opPopRegisterAB(), // A = value, B = addr
                        opLoad(TargetRegister::B, TargetRegister::B),
                        opStore(TargetRegister::B, TargetRegister::A)),
                function("MemorySwap", // ( addr0 addr1 -- )
                        opPopRegisterAB(),
                        opEquals(TargetRegister::C, TargetRegister::A, TargetRegister::B),
                        // don't do the memory swap if it is the same addresses!
                        opConditionalBranch(TargetRegister::C, "MemorySwapSkip"),
                        opLoad(TargetRegister::Temporary, TargetRegister::A),
                        opLoad(TargetRegister::Temporary2, TargetRegister::B),
                        opStore(TargetRegister::B, TargetRegister::Temporary),
                        opStore(TargetRegister::A, TargetRegister::Temporary2),
                        label("MemorySwapSkip")),
                function("DropTopParameter",
                        opPopRegisterA()),
                function("DuplicateTopParameter",
                        opPopRegisterA(),
                        opPushRegisterA(),
                        opPushRegisterA()),
                function("SwapTopAndLowerParameters", // ( b a -- a b )
                        opPopRegisterAB(), // A = top, B = lower
                        opPushRegisterA(),
                        opPushRegisterB()),
                function("MoveLowerParameterOverTopParameter", // ( b a -- b a b )
                        opPopRegisterAB(), // A = top, B = lower
                        opPushRegisterB(),
                        opPushRegisterA(),
                        opPushRegisterB()),

                function("RotateTopThreeParameters", // ( b a c -- a c b )
                        opPopRegisterCAB(), // c = Top, a = lower, b = third
                        opPushRegisterA(),
                        opPushRegisterC(),
                        opPushRegisterB()),
                function("ReverseRotateTopThreeParameters",
                        opSubroutineCall("RotateTopThreeParameters"),
                        opSubroutineCall("RotateTopThreeParameters")),
                function("ConditionalDispatch", // ( cond onFalse onTrue -- )
                        // we want to put the cond first!
                        opSubroutineCall("RotateTopThreeParameters"), // ( cond onFalse onTrue -- onFalse onTrue cond )
                        opSubroutineCall("EqualZero"), // ( onFalse onTrue cond -- onFalse onTrue cond )
                        opPopRegisterCAB(), // C = cond, A = onTrue, B = onFalse
                        opCallIfStatement(TargetRegister::C, TargetRegister::A, TargetRegister::B)),
                function("DropLowerParameter", // ( b a -- a )
                        opPopRegisterAB(),
                        opPushRegisterA()),
                function("CopyTopParameterToBelowLowerParameter", // ( b a -- a b a )
                        opPopRegisterAB(),
                        opPushRegisterA(),
                        opPushRegisterB(),
                        opPushRegisterA()),
                function("EmitCharacter", // ( character-code -- )
                        opPopRegisterA(),
                        opPrintChar(TargetRegister::A)),
                function("PrintString", // ( length string -- )
                        opPopRegisterAB(), // A = string, B = length
                        opPrintString(TargetRegister::A, TargetRegister::B)),
                function("DuplicateTopParameterIfNonZero",
                        opPopRegisterA(),
                        opEquals(TargetRegister::C, TargetRegister::A, TargetRegister::Zero),
                        opConditionalBranch(TargetRegister::C, "DuplicateTopParameterIfNonZeroSkip"),
                        opPushRegisterA(),
                        label("DuplicateTopParameterIfNonZeroSkip"),
                        opPushRegisterA()),
				unaryOperationFunction("NotOperation", opNot(DEFAULT_REGISTER_ARGS2)),
				unaryOperationFunction("NotBooleanOperation", opNotBoolean(DEFAULT_REGISTER_ARGS2)),
				unaryOperationFunction("NotSignedOperation", opNotSigned(DEFAULT_REGISTER_ARGS2)),
				unaryOperationFunction("MinusOperation", opMinus(DEFAULT_REGISTER_ARGS2)),
				unaryOperationFunction("MinusUnsignedOperation", opMinusUnsigned(DEFAULT_REGISTER_ARGS2)),
				unaryOperationFunction("MinusFloatingPointOperation", opMinusFloatingPoint(DEFAULT_REGISTER_ARGS2)),
#define DispatchOneRegister(title) 
#define DispatchTaggedOneRegister(title) 
#define DispatchTwoRegister(title) 
#define DispatchThreeRegister(title) binaryOperationFunction( #title "Operation" , op ## title ( DEFAULT_REGISTER_ARGS3 )),
#define DispatchSignedImm16(title)
#define DispatchImmediate24(title) 
#define DispatchTwoRegisterWithImm16(title) 
#define DispatchCustomTwoRegisterWithImm16(title) 
#define DispatchOneRegisterWithImm16(title) 
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
#undef DispatchTaggedOneRegister
#undef DispatchNoArguments
#undef DispatchOneRegister
#undef DispatchTwoRegister
#undef DispatchThreeRegister
#undef DispatchSignedImm16
#undef DispatchImmediate24
#undef DispatchTwoRegisterWithImm16
#undef DispatchOneRegisterWithImm16
#undef DispatchOneRegisterWithImm48
#undef DispatchOneRegisterWithImm32
#undef DispatchOneRegisterWithImm64

                function("NewWord", 
                        opPushRegister(TargetRegister::Compile, TargetRegister::SP2),
                        opUnsignedAddImmediate(TargetRegister::Compile, TargetRegister::DP, 32)),
                function("FinishCompilingWord",
                        opMove(TargetRegister::Temporary, TargetRegister::Compile),
                        opUnsignedAddImmediate(TargetRegister::Temporary, TargetRegister::Temporary, 24),
                        opStore(TargetRegister::Temporary, TargetRegister::DP),
                        opMove(TargetRegister::DP, TargetRegister::Compile),
                        opPopRegister(TargetRegister::Compile, TargetRegister::SP2)),
                function("GetStringLength", // ( addr -- length )
                        opPopRegisterA(),
                        opLoad(TargetRegister::C, TargetRegister::A), 
                        opPushRegisterC()),
                function("GetStringStart", // ( addr -- length )
                        opPopRegisterA(),
                        opUnsignedAddImmediate(TargetRegister::A, TargetRegister::A, 8),
                        opPushRegisterC()),
                function("UnpackString", // ( addr -- str length )
                        opSubroutineCall("DuplicateTopParameter"), // ( addr -- addr addr )
                        opSubroutineCall("GetStringStart"), // ( addr addr -- addr str )
                        opSubroutineCall("SwapTopAndLowerParameters"), // ( addr str -- str addr )
                        opSubroutineCall("GetStringLength")), // ( str addr -- str len )
                function("GetStringAddressFromDictionaryEntry", // ( dict -- addr )
                        opSubroutineCall("LoadValue")),
                function("GetFlagsFromDictionaryEntry", // ( dict -- flags)
                        opPopRegisterA(),
                        opUnsignedAddImmediate(TargetRegister::A, TargetRegister::A, 8),
                        opPushRegisterA(),
                        opSubroutineCall("LoadValue")),
                function("GetNextFromDictionaryEntry", // ( dict -- next )
                        opPopRegisterA(),
                        opUnsignedAddImmediate(TargetRegister::A, TargetRegister::A, 16),
                        opPushRegisterA(),
                        opSubroutineCall("LoadValue")),
                function("GetSubroutineFromDictionaryEntry", // ( dict -- next )
                        opPopRegisterA(),
                        opUnsignedAddImmediate(TargetRegister::A, TargetRegister::A, 24),
                        opPushRegisterA(),
                        opSubroutineCall("LoadValue")),
                function("PrintOk",
                        opPushImmediate64(init.installString("ok")),
                        opSubroutineCall("UnpackString"),
                        opSubroutineCall("PrintString")),
                function("PushParameterXOntoStack",
                        opPushRegister(TargetRegister::X)),
                function("PopParameterIntoX",
                        opPopRegister(TargetRegister::X)),
                function("PushSubroutineXOntoStack",
                        opPushRegister(TargetRegister::X, TargetRegister::SP2)),
                function("PopSubroutineIntoX",
                        opPopRegister(TargetRegister::X, TargetRegister::SP2)),
                function("PushTrue",
                        opLoadImmediate16(TargetRegister::C, 1),
                        opPushRegister(TargetRegister::C)),
                function("PushFalse",
                        opPushRegister(TargetRegister::Zero)),
                function("ComputeBitPosition", // ( position -- pos )
                        opPushImmediate64(1), // ( position -- position 1 )
                        opSubroutineCall("ShiftLeftOperation")),
                function("BitSet", // ( addr position -- truth )
                        opSubroutineCall("ComputeBitPosition"),
                        opSubroutineCall("AndUnsignedOperation"),
                        opSubroutineCall("NotEqualZero")),
                function("IsFakeEntry", // ( addr -- truth )
                        opPushRegister(TargetRegister::Zero),
                        opSubroutineCall("BitSet")),
                function("HasCompileTimeInvoke", // ( addr -- truth )
                        opLoadImmediate16(TargetRegister::A, 1), 
                        opPushRegisterA(), // ( addr -- addr 1 )
                        opSubroutineCall("BitSet"))
                        // the first byte 
                );
        machine.installInCore(init);
#undef DEFAULT_REGISTER_ARGS3
#undef DEFAULT_REGISTER_ARGS2
    }
}

int main() {
    try {
        forth::Machine machine (std::cout, std::cin);
        machine.initializeBaseDictionary();
        forth::systemSetup(machine);
        machine.dispatchInstruction(0);
        //machine.controlLoop();
    } catch (forth::Problem& p) {
        std::cout << p.getWord() << ": " << p.getMessage() << std::endl;
        std::cout << "terminating...." << std::endl;
    }

    return 0;
}
