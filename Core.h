#ifndef CORE_H__
#define CORE_H__
#include "Types.h"
#include "Datum.h"
#include <functional>
#include "Instruction.h"
namespace forth {
class Core {
	public:
		using LoadInterface = std::function<Datum(Address)>;
		using StoreInterface = std::function<void(Address, const Datum&)>;
	public:
		Core(LoadInterface load, StoreInterface store);
		~Core() = default;
		void dispatchInstruction(const Molecule& m);
		Datum load(Address addr);
		void store(Address addr, const Datum& value);
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
		void push(TargetRegister reg, TargetRegister sp);
		void pop(TargetRegister dest, TargetRegister sp);
	private:
		Register _a, _b, _c, _s, _x;
		Register _ip, _sp, _sp2, _imm;
		Register _pc;
		Register _tmp0, _tmp1;
		Register _currentMolecule, _moleculePosition;
		LoadInterface _load;
		StoreInterface _store;

};
} // end namespace forth
#endif // end CORE_H__
