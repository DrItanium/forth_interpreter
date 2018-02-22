#ifndef CORE_H__
#define CORE_H__
#include "Types.h"
#include "Datum.h"
#include <functional>
#include <tuple>
#include "Instruction.h"
#include <memory>
namespace forth {
class Core {
	public:
		static constexpr Address largestAddress = 0xFF'FFFF;
		static constexpr Address memoryCapacity = (largestAddress + 1);
		// we have a 64-kiloword area for storing internal system values.
		static constexpr Address systemVariableStart = 0xFFFF'FFFF'FFFF'0000;
		static constexpr Address systemVariableEnd = 0xFFFF'FFFF'FFFF'FFFF;
		static constexpr Address terminateExecutionVariable = systemVariableEnd;
		static constexpr Address sp2StackEmpty = systemVariableEnd - 1;
		static constexpr Address sp2StackFull = systemVariableEnd - 2;
		static constexpr Address spStackEmpty = systemVariableEnd - 3;
		static constexpr Address spStackFull = systemVariableEnd - 4;
		static constexpr Address userVariableEnd = 0xFFFF'FFFF'FFFF'7FFF;
		static constexpr Address userVariableStart = systemVariableStart;
		static constexpr Address systemVariableSize = (systemVariableEnd - systemVariableStart) + 1;
		using OutputFunction = std::function<void(Discriminant, TargetRegister, const Register&)>;
	public:
		Core(OutputFunction output);
		~Core() = default;
		//void executionLoop();
		void setOutputFunction(OutputFunction output);
		void dispatchInstruction(const Molecule& m);
		Datum load(Address addr);
		void store(Address addr, const Datum& value);
		Register& getRegister(TargetRegister reg);
		void push(const Datum& d, TargetRegister sp);
		Datum pop(TargetRegister sp);
        void executionCycle(Address startAddress = 0);
	private:
		void push(TargetRegister reg, TargetRegister sp);
		void pop(TargetRegister dest, TargetRegister sp);
		void savePositionToSubroutineStack();
		void numericCombine(Operation op);
		void multiplyOperation(Operation op);
		void divideOperation(Operation op);
		void equalsOperation(Operation op);
		void push(Operation op);
		void pop(Operation op);
		void notOperation(Operation op);
		void minusOperation(Operation op);
		void booleanAlgebra(Operation op);
		void shiftOperation(Operation op);
		void powOperation(Operation op);
		void rangeChecks(Operation op);
		void incrDecr(Operation op);
		void jumpOperation(Operation op);
		void conditionalBranch(Operation op);
		void loadImm48(Operation op);
		void moveOrSwap(Operation op);
		void typeValue(Operation op);
		void loadStore(Operation op);
		void setImm16(Operation op);
        void encodeDecodeBits(Operation op);
	private:
		using ThreeRegisterForm = std::tuple<TargetRegister, TargetRegister, TargetRegister>;
		using ThreeRegisterImmediateForm = std::tuple<TargetRegister, TargetRegister, QuarterAddress>;
		using TwoRegisterForm = std::tuple<TargetRegister, TargetRegister>;
		using ThreeRegisterArguments = std::tuple<Register&, Register&, Register&>;
		using TwoRegisterArguments = std::tuple<Register&, Register&>;
        using FourRegisterForm = std::tuple<TargetRegister, TargetRegister, TargetRegister, TargetRegister>;
		using FourRegisterArguments = std::tuple<Register&, Register&, Register&, Register&>;
        using FiveRegisterForm = std::tuple<TargetRegister, TargetRegister, TargetRegister, TargetRegister, TargetRegister>;
		using FiveRegisterArguments = std::tuple<Register&, Register&, Register&, Register&, Register&>;
		ThreeRegisterArguments extractArguments(Operation op, std::function<void(Register&, Address)> onImmediate = nullptr);
        FourRegisterArguments extractArguments4(Operation op);
        FiveRegisterArguments extractArguments5(Operation op);
        FourRegisterForm extractFourRegisterForm();
        FiveRegisterForm extractFiveRegisterForm();
		ThreeRegisterForm extractThreeRegisterForm();
		ThreeRegisterImmediateForm extractThreeRegisterImmediateForm();
		TwoRegisterForm extractTwoRegisterForm();
		QuarterInteger extractQuarterIntegerFromMolecule();
		QuarterAddress extractQuarterAddressFromMolecule();
		byte extractByteFromMolecule();
		Address extractImm48();
		Operation extractOperationFromMolecule();
		void setCurrentMolecule(const Molecule& m);
		void advanceMoleculePosition(Address amount = 1);
	private:
		static constexpr bool inSystemVariableArea(Address value) noexcept {
			return (systemVariableStart <= value) && (systemVariableEnd >= value);
		}
		Datum& getSystemVariable(Address index);
	private:
	private:
		Register _a, _b, _c, _s, _x;
		Register _sp, _sp2, _imm, _pc;
        Register _dp, _index;
		Register _tmp0, _tmp1;
		Register _currentMolecule, _moleculePosition;
		OutputFunction _output;
		// mapped to 0xFFFFFFFFFFFF0000
		std::unique_ptr<Datum[]> _memory, _systemVariables;
        bool _advancePC = false;

};
} // end namespace forth
#endif // end CORE_H__
