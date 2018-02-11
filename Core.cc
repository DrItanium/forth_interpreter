#include "Core.h"
#include "Problem.h"
#include <sstream>
#include <string>

namespace forth {
	constexpr bool immediateForm(Operation op) noexcept {
		switch(op) {
#define Immediate(x) case Operation :: x ## Immediate :
	Immediate(Add)
	Immediate(Subtract)
	Immediate(Multiply)
	Immediate(Divide)
	Immediate(Modulo)
	Immediate(And)
	Immediate(Or)
	Immediate(GreaterThan)
	Immediate(LessThan)
	Immediate(Xor)
	Immediate(ShiftRight)
	Immediate(ShiftLeft)
	Immediate(Equals)
				return true;
			default:
				return false;
#undef Immediate
		}
	}
	constexpr bool fullForm(Operation op) noexcept {
		switch(op) {
#define Full(x) case Operation :: x ## Full :
	Full(Add)
	Full(Subtract)
	Full(Multiply)
	Full(Divide)
	Full(Modulo)
	Full(And)
	Full(Or)
	Full(GreaterThan)
	Full(LessThan)
	Full(Xor)
	Full(ShiftRight)
	Full(ShiftLeft)
	Full(Equals)
	Full(Pow)
	Full(Not)
	Full(Minus)
				return true;
			default:
				return false;
#undef Full
		}
	}
Core::Core(LoadInterface load, StoreInterface store) : _load(load), _store(store), _systemVariables(new Datum[systemVariableSize]) { }

Register& Core::getRegister(TargetRegister reg) {
	using Type = decltype(t);
	switch (t) {
		case Type::A:
		case Type::TA:
			return _a;
		case Type::B:
		case Type::TB:
			return _b;
		case Type::C:
		case Type::T:
			return _c;
		case Type::S:
			return _s;
		case Type::X:
		case Type::TX:
			return _x;
		case Type::IP:
			return _ip;
		case Type::SP:
			return _sp;
		case Type::SP2:
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

ThreeRegisterArguments extractArguments(Operation op, std::function<void(Register&, Address)> onImmediate) {
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
		auto tup = std::forward_as_tuple(getRegister(std::get<0>(x)), getRegister(std::get<1>(x)), getRegister(std::get<2>(x)));
		return tup;
	} else {
		return std::forward_as_tuple(_c, _a, _b);
	}
}





} // namespace forth
