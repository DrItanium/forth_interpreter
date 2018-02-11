#ifndef CORE_H__
#define CORE_H__
#include "Types.h"
#include "Datum.h"
#include <functional>
#include "Instruction.h"
namespace forth {
class Core {
	public:
		static constexpr Address largestAddress = 0xFFFFFF;
		static constexpr Address memoryCapacity = (largestAddress + 1);
		// we have a 64-kiloword area for storing internal system values.
		static constexpr Address systemVariableStart = 0xFFFFFFFFFFFF0000;
		static constexpr Address systemVariableEnd = 0xFFFFFFFFFFFFFFFF;
		static constexpr Address systemVariableSize = (systemVariableEnd - systemVariableStart) + 1;
	public:
		Core();
		~Core() = default;
		//void executionLoop();
		void dispatchInstruction(const Molecule& m);
		Datum load(Address addr);
		void store(Address addr, const Datum& value);
	private:
		void push(TargetRegister reg, TargetRegister sp);
		void pop(TargetRegister dest, TargetRegister sp);
	private:
		void numericCombine(Operation op);
		void multiplyOperation(Operation op);
		void equalsOperation(Operation op);
		void popOperation(Operation op);
		void pushOperation(Operation op);
		void notOperation(Operation op);
		void minusOperation(Operation op);
		void andOperation(Operation op);
		void orOperation(Operation op);
		void greaterThanOperation(Operation op);
		void lessThanOperation(Operation op);
		void xorOperation(Operation op);
		void shiftOperation(Operation op);
		void jumpOperation(Operation op);
		void conditionalBranch(Operation op);
		void push(Operation op);
		void pop(Operation op);
	private:
		using ThreeRegisterForm = std::tuple<TargetRegister, TargetRegister, TargetRegister>;
		using ThreeRegisterImmediateForm = std::tuple<TargetRegister, TargetRegister, QuarterAddress>;
		using TwoRegisterForm = std::tuple<TargetRegister, TargetRegister>;
		using ThreeRegisterArguments = std::tuple<Register&, Register&, Register&>;
		using TwoRegisterArguments = std::tuple<Register&, Register&>;
		ThreeRegisterArguments extractArguments(Operation op, std::function<void(Register&, Address)> onImmediate = nullptr);
		TwoRegisterArguments extractArguments2(Operation op);
		ThreeRegisterForm extractThreeRegisterForm();
		ThreeRegisterImmediateForm extractThreeRegisterImmediateForm();
		TwoRegisterForm extractTwoRegisterForm();
		QuarterAddress extractQuarterAddressFromMolecule();
		byte extractByteFromMolecule();
		Operation extractOperationFromMolecule();
		void setCurrentMolecule(const Molecule& m);
		void advanceMoleculePosition(Address amount = 1);
	private:
		constexpr bool inSystemVariableArea(Address value) noexcept {
			return (systemVariableStart <= value) && (systemVariableEnd >= value);
		}
		Datum& getSystemVariable(Address index);
	private:
		Register& getRegister(TargetRegister reg);
	private:
		Register _a, _b, _c, _s, _x;
		Register _sp, _sp2, _imm, _pc;
		Register _tmp0, _tmp1;
		Register _currentMolecule, _moleculePosition;
		// mapped to 0xFFFFFFFFFFFF0000
		std::unique_ptr<Datum[]> _memory, _systemVariables;

};
} // end namespace forth
#endif // end CORE_H__
