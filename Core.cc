#include "Core.h"
#include "Problem.h"
#include <sstream>
#include <string>

namespace forth {
Core::Core() : _memory(new Datum[memoryCapacity]), _systemVariables(new Datum[systemVariableSize]) { }

Register& Core::getRegister(TargetRegister reg) {
	using Type = decltype(reg);
	switch (t) {
		case Type::A:
		case Type::TA:
			return _a;
		case Type::B:
		case Type::TB:
			return _b;
		case Type::C:
		case Type::TC:
			return _c;
		case Type::S:
		case Type::TS:
			return _s;
		case Type::X:
		case Type::TX:
			return _x;
		case Type::PC:
		case Type::TPC:
			return _pc;
		case Type::SP:
		case Type::TSP:
			return _sp;
		case Type::SP2:
		case Type::TSP2:
			return _sp2;
		default:
			throw Problem("getRegister", "Undefined register!");
	}
}
Datum& Core::getSystemVariable(Address index) {
	if (!Core::inSystemVariableArea(index)) {
		std::stringstream ss;
		ss << "Illegal address: " << std::hex << index;
		auto msg = ss.str();
		throw Problem("getSystemVariable", msg);
	}
	return _systemVariables[index - systemVariableStart];
}
void Core::setCurrentMolecule(const Molecule& m) {
	_currentMolecule.setValue(m._value);
	_moleculePosition.reset();
}


void Core::advanceMoleculePosition(Address amount) {
	_moleculePosition.increment(amount);
}


Operation Core::extractOperationFromMolecule() {
	return static_cast<Operation>(extractByteFromMolecule());
}

byte Core::extractByteFromMolecule() {
	auto b = Molecule(_currentMolecule.getValue()._address).getByte(_moleculePosition.getValue()._address);
	advanceMoleculePosition();
	return b;
}

QuarterAddress Core::extractQuarterAddressFromMolecule() {
	auto q = Molecule(_currentMolecule.getValue()._address).getQuarterAddress(_moleculePosition.getValue()._address);
	advanceMoleculePosition(sizeof(q));
	return q;
}

TwoRegisterForm Core::extractTwoRegisterForm() {
	// single byte
	auto data = extractByteFromMolecule();
	return std::make_tuple(TargetRegister(getDestinationRegister(data)), TargetRegister(getSourceRegister(data)));
}

ThreeRegisterImmediateForm Core::extractThreeRegisterImmediateForm() {
	auto regs = extractByteFromMolecule();
	auto imm = extractQuarterAddressFromMolecule();
	return std::make_tuple(TargetRegister(getDestinationRegister(regs)), TargetRegister(getSourceRegister(data)), imm);
}

ThreeRegisterForm Core::extractThreeRegisterForm() {
	auto regs = extractByteFromMolecule();
	auto regs2 = extractByteFromMolecule();
	return std::make_tuple(TargetRegister(getDestinationRegister(regs)), TargetRegister(getSourceRegister(regs)), TargetRegister(getDestinationRegister(regs2)));
}

TwoRegisterArguments Core::extractArguments2(Operation op) {
	auto regs = extractTwoRegisterForm();
	return std::forward_as_tuple(getRegister(std::get<0>(regs)), getRegister(std::get<1>(regs)));
}

ThreeRegisterArguments Core::extractArguments(Operation op, std::function<void(Register&, Address)> onImmediate) {
	if (immediateForm(op)) {
		auto x = extractThreeRegisterImmediateForm();
		if (onImmediate) {
			onImmediate(_tmp0, std::get<2>(x));
		} else {
			_tmp0.setValue(std::get<2>(x));
		}
		auto tuple = std::forward_as_tuple(getRegister(std::get<0>(x)), getRegister(std::get<1>(x)), _tmp0);
		return tuple;
	} else if (fullForm(op)) {
		auto x = extractThreeRegisterForm();
		return std::forward_as_tuple(getRegister(std::get<0>(x)), getRegister(std::get<1>(x)), getRegister(std::get<2>(x)));
	} else {
		return std::forward_as_tuple(_c, _a, _b);
	}
}


void Core::store(Address addr, const Datum& value) {
	if (addr <= largestAddress) {
		_memory[addr] = value.numValue;
	} else {
		// see if we're in the system variable area instead!
		getSystemVariable(addr).address = value.address;
	}
}

Datum Core::load(Address addr) {
	if (addr <= largestAddress) {
		return _memory[addr];
	} else {
		return getSystemVariable(addr);
	}
}

void Core::push(TargetRegister reg, TargetRegister sp) {
	if (involvesDiscriminantRegister(sp)) {
		throw Problem("push", "Can't use the discriminant field as a stack pointer!");
	}
	auto& stackPointer = getRegister(sp);
	auto& value = getRegister(reg);
	stackPointer.decrement();
	if (involvesDiscriminantRegister(reg)) {
		store(stackPointer.getAddress(), (Address)value.getType());
	} else {
		store(stackPointer.getAddress(), value.getValue());
	}
}

void Core::pop(TargetRegister reg, TargetRegister sp) {
	if (involvesDiscriminantRegister(sp)) {
		throw Problem("pop", "Can't use the discriminant field as a stack pointer!");
	}
	auto& stackPointer = getRegister(sp);
	auto& dest = getRegister(reg);
	auto result = load(stackPointer.getAddress());
	if (involvesDiscriminantRegister(sp)) {
		dest.setType(static_cast<Discriminant>(result.address));
	} else {
		dest.setValue(result);
	}
	stackPointer.increment();
}
template<typename T>
void performOperation(Register& dest, T src0, T src1, std::function<T(T, T)> fn) {
	dest.setValue(fn(src0, src1));
}
template<typename T>
void numericOperation(const std::string& op, Register& dest, const Register& src0, const Register& src1, T fn) {
	switch(dest.getType()) {
		case Discriminant::Number:
			performOperation(dest, src0.getInt(), src1.getInt(), fn);
			break;
		case Discriminant::MemoryAddress:
			performOperation(dest, src0.getAddress(), src1.getAddress(), fn);
			break;
		case Discriminant::FloatingPoint:
			performOperation(dest, src0.getFP(), src1.getFP());
			break;
		default:
			throw Problem(op, "ILLEGAL DISCRIMINANT!");
	}
}
void Core::numericCombine(Operation op) {
	auto result = extractArguments(op, m, [this](Register& r, auto val) {
				if (_c.getType() == Discriminant::FloatingPoint) {
					r.setValue(static_cast<Floating>(val));
				} else {
					r.setValue(val);
				}
			});
	auto subtract = subtractOperation(op);
	auto& [dest, src0, src1] = result;
	auto fn = [this, subtract](auto a, auto b) { 
		return subtract ? (a - b) : (a + b );
	};
	numericOperation(subtract ? "-" : "+", dest, src0, src1, fn);
}

void Core::multiplyOperation(Operation op) {
	using Type = decltype(_c.getType());
	auto t = extractArguments(op, m, nullptr);
	auto& [dest, src0, src1] = t;
	auto fn = [this](auto a, auto b) { return a * b; };
	numericOperation("*", dest, src0, src1, fn);
}


} // namespace forth
