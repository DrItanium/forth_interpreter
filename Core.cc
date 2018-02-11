#include "Core.h"
#include "Problem.h"
#include <sstream>
#include <string>
#include <cmath>

namespace forth {
Core::Core() : _memory(new Datum[memoryCapacity]), _systemVariables(new Datum[systemVariableSize]) { }

Register& Core::getRegister(TargetRegister reg) {
	using Type = decltype(reg);
	switch (reg) {
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
	auto b = Molecule(_currentMolecule.getAddress()).getByte(_moleculePosition.getAddress());
	advanceMoleculePosition();
	return b;
}

QuarterAddress Core::extractQuarterAddressFromMolecule() {
	auto q = Molecule(_currentMolecule.getAddress()).getQuarterAddress(_moleculePosition.getAddress());
	advanceMoleculePosition(sizeof(q));
	return q;
}

Core::TwoRegisterForm Core::extractTwoRegisterForm() {
	// single byte
	auto data = extractByteFromMolecule();
	return std::make_tuple(TargetRegister(getDestinationRegister(data)), TargetRegister(getSourceRegister(data)));
}

Core::ThreeRegisterImmediateForm Core::extractThreeRegisterImmediateForm() {
	auto regs = extractByteFromMolecule();
	auto imm = extractQuarterAddressFromMolecule();
	return std::make_tuple(TargetRegister(getDestinationRegister(regs)), TargetRegister(getSourceRegister(regs)), imm);
}

Core::ThreeRegisterForm Core::extractThreeRegisterForm() {
	auto regs = extractByteFromMolecule();
	auto regs2 = extractByteFromMolecule();
	return std::make_tuple(TargetRegister(getDestinationRegister(regs)), TargetRegister(getSourceRegister(regs)), TargetRegister(getDestinationRegister(regs2)));
}


Core::ThreeRegisterArguments Core::extractArguments(Operation op, std::function<void(Register&, Address)> onImmediate) {
	if (immediateForm(op)) {
		auto x = extractThreeRegisterImmediateForm();
		Address immediate = std::get<2>(x);
		if (onImmediate) {
			onImmediate(_tmp0, immediate);
		} else {
			_tmp0.setValue(immediate);
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
void numericOperation(const std::string& op, Register& dest, const Register& src0, const Register& src1, T fn) {
	switch(dest.getType()) {
		case Discriminant::Number:
			dest.setValue(fn(src0.getInt(), src1.getInt()));
			break;
		case Discriminant::MemoryAddress:
			dest.setValue(fn(src0.getAddress(), src1.getAddress()));
			break;
		case Discriminant::FloatingPoint:
			dest.setValue(fn(src0.getFP(), src1.getFP()));
			break;
		default:
			throw Problem(op, "ILLEGAL DISCRIMINANT!");
	}
}

template<typename T>
void numericOperationAndBool(const std::string& op, Register& dest, const Register& src0, const Register& src1, T fn) {
	switch(dest.getType()) {
		case Discriminant::Number:
			dest.setValue(fn(src0.getInt(), src1.getInt()));
			break;
		case Discriminant::MemoryAddress:
			dest.setValue(fn(src0.getAddress(), src1.getAddress()));
			break;
		case Discriminant::FloatingPoint:
			dest.setValue(fn(src0.getFP(), src1.getFP()));
			break;
		case Discriminant::Boolean:
			dest.setValue(fn(src0.getTruth(), src1.getTruth()));
			break;
		default:
			throw Problem(op, "ILLEGAL DISCRIMINANT!");
	}
}

template<typename T>
void numericOperationIntegerOnly(const std::string& op, Register& dest, const Register& src0, const Register& src1, T fn) {
	switch(dest.getType()) {
		case Discriminant::Number:
			dest.setValue(fn(src0.getInt(), src1.getInt()));
			break;
		case Discriminant::MemoryAddress:
			dest.setValue(fn(src0.getAddress(), src1.getAddress()));
			break;
		default:
			throw Problem(op, "ILLEGAL DISCRIMINANT!");
	}
}
void Core::numericCombine(Operation op) {
	auto result = extractArguments(op, [this](Register& r, auto val) {
				if (_c.getType() == Discriminant::FloatingPoint) {
					r.setValue(static_cast<Floating>(val));
				} else {
					r.setValue(val);
				}
			});
	auto subtract = subtractOperation(op);
	auto& [dest, src0, src1] = result;
	auto fn = [subtract](auto a, auto b) { 
		return subtract ? (a - b) : (a + b );
	};
	numericOperation(subtract ? "-" : "+", dest, src0, src1, fn);
}

void Core::multiplyOperation(Operation op) {
	auto t = extractArguments(op, nullptr);
	auto& [dest, src0, src1] = t;
	auto fn = [this](auto a, auto b) { return a * b; };
	numericOperation("*", dest, src0, src1, fn);
}

void Core::divideOperation(Operation op) {

	auto t = extractArguments(op);
	auto& [dest, src0, src1] = t;
	auto isModulo = isModuloOperation(op);
	if (src1.getAddress() == 0) {
		throw Problem(isModulo ? "mod" : "/", "DIVIDE BY ZERO!");
	}
	if (isModulo) {
		numericOperationIntegerOnly("mod", dest, src0, src1, [](auto a, auto b) { return a % b; });
	} else {
		numericOperation("/", dest, src0, src1, [](auto a, auto b) { return a / b; });
	}
}

void Core::equalsOperation(Operation op) {
	auto t = extractArguments(op);
	auto& [dest, src0, src1] = t;
	numericOperationAndBool("eq", dest, src0, src1, [](auto a, auto b) { return a == b; });
}

void Core::push(Operation op) {
	switch (op) {
		case Operation::PushA:
			push(TargetRegister::A, TargetRegister::SP);
			break;
		case Operation::PushB:
			push(TargetRegister::B, TargetRegister::SP);
			break;
		case Operation::PushC:
			push(TargetRegister::C, TargetRegister::SP);
			break;
		default: {
					 auto args = extractTwoRegisterForm();
					 // push sp, src
					 push(std::get<1>(args), std::get<0>(args));
					 break;
				 }
	}
}

void Core::pop(Operation op) {
	switch (op) {
		case Operation::PopA:
			pop(TargetRegister::A, TargetRegister::SP);
			break;
		case Operation::PopB:
			pop(TargetRegister::B, TargetRegister::SP);
			break;
		case Operation::PopT:
			pop(TargetRegister::T, TargetRegister::SP);
			break;
		case Operation::PopC:
			pop(TargetRegister::C, TargetRegister::SP);
			break;
		default: {
					 auto args = extractTwoRegisterForm();
					 // pop dest, sp
					 pop(std::get<0>(args), std::get<1>(args));
					 break;
				 }
	}
}

template<typename T>
void numericBoolAndInteger(const std::string& op, Register& dest, const Register& src0, T fn) {
	switch(dest.getType()) {
		case Discriminant::Number:
			dest.setValue(fn(src0.getInt()));
			break;
		case Discriminant::MemoryAddress:
			dest.setValue(fn(src0.getAddress()));
			break;
		case Discriminant::Boolean:
			dest.setValue(fn(src0.getTruth()));
			break;
		default:
			throw Problem(op, "ILLEGAL DISCRIMINANT!");
	}
}

template<typename T>
void numericBoolAndInteger(const std::string& op, Register& dest, const Register& src0, const Register& src1, T fn) {
	switch(dest.getType()) {
		case Discriminant::Number:
			dest.setValue(fn(src0.getInt(), src1.getInt()));
			break;
		case Discriminant::MemoryAddress:
			dest.setValue(fn(src0.getAddress(), src1.getAddress()));
			break;
		case Discriminant::Boolean:
			dest.setValue(fn(src0.getTruth(), src1.getTruth()));
			break;
		default:
			throw Problem(op, "ILLEGAL DISCRIMINANT!");
	}
}

void Core::notOperation(Operation op) {
	auto tup = extractTwoRegisterForm();
	auto& dest = getRegister(std::get<0>(tup));
	auto& src = getRegister(std::get<1>(tup));
	switch (dest.getType()) {
		case Discriminant::Number:
			dest.setValue(~src.getInt());
			break;
		case Discriminant::MemoryAddress:
			dest.setValue(~src.getAddress());
			break;
		case Discriminant::Boolean:
			dest.setValue(!src.getTruth());
			break;
		default:
			throw Problem("not", "ILLEGAL DISCRIMINANT");
	}
}

void Core::minusOperation(Operation op) {
	auto tup = extractTwoRegisterForm();
	auto& dest = getRegister(std::get<0>(tup));
	auto& src = getRegister(std::get<1>(tup));
	switch (dest.getType()) {
		case Discriminant::Number:
			dest.setValue(-src.getInt());
			break;
		case Discriminant::FloatingPoint:
			dest.setValue(-src.getFP());
			break;
		default:
			throw Problem("not", "ILLEGAL DISCRIMINANT");
	}
}

void Core::booleanAlgebra(Operation op) {
	auto tup = extractArguments(op);
	auto& [dest, src0, src1] = tup;
	if (andForm(op)) {
		numericBoolAndInteger("and", dest, src0, src1, [](auto a, auto b) { if constexpr (std::is_same<decltype(a), bool>::value) { return a && b; } else { return a & b; }});
	} else if (orForm(op)) {
		numericBoolAndInteger("or", dest, src0, src1, [](auto a, auto b) { if constexpr (std::is_same<decltype(a), bool>::value) { return a || b; } else { return a | b; }});
	} else if (xorForm(op)) {
		numericBoolAndInteger("xor", dest, src0, src1, [](auto a, auto b) { if constexpr (std::is_same<decltype(a), bool>::value) { return a != b; } else { return a ^ b; }});
	} else {
		throw Problem("booleanAlgebra", "UNKNOWN OPERATION GROUP!");
	}
}

void Core::shiftOperation(Operation op) {
	auto tup = extractArguments(op);
	auto& [dest, src0, src1] = tup;
	switch (op) {
		case Operation::ShiftLeft:
		case Operation::ShiftLeftImmediate:
		case Operation::ShiftLeftFull:
			numericOperationIntegerOnly("<<", dest, src0, src1, [](auto a, auto b) { return a << b; });
			break;
		case Operation::ShiftRight:
		case Operation::ShiftRightImmediate:
		case Operation::ShiftRightFull:
			numericOperationIntegerOnly(">>", dest, src0, src1, [](auto a, auto b) { return a >> b; });
			break;
		default:
			throw Problem("shiftOperation", "Unknown shift operation!");
			
	}
}

void Core::powOperation(Operation op) {
	auto tup = extractArguments(op);
	auto& [dest, src0, src1] = tup;
	numericOperation("pow", dest, src0, src1, [](auto a, auto b) { return static_cast<decltype(a)>(std::pow(Floating(a), Floating(b))); });
}

void Core::rangeChecks(Operation op) {
	auto tup = extractArguments(op);
	auto& [dest, src0, src1] = tup;
	switch (op) {
		case Operation::GreaterThan:
		case Operation::GreaterThanImmediate:
		case Operation::GreaterThanFull:
			numericOperation(">", dest, src0, src1, [](auto a, auto b) { return a > b; });
			break;
		case Operation::LessThan:
		case Operation::LessThanImmediate:
		case Operation::LessThanFull:
			numericOperation("<", dest, src0, src1, [](auto a, auto b) { return a < b; });
			break;
		default:
			throw Problem("rangeChecks", "Unknown range check operation!");
	}
}

void Core::incrDecr(Operation op) {
	auto tmp = extractByteFromMolecule();
	auto d = static_cast<TargetRegister>(getDestinationRegister(tmp));
	auto imm4 = getSourceRegister(tmp);
	_tmp1.setValue(Address(imm4));
	auto& dest = getRegister(d);
	switch (op) {
		case Operation::Increment:
			numericOperation("increment", dest, dest, _tmp1, [](auto a, auto b) { return a + b; });
			break;
		case Operation::Decrement:
			numericOperation("decrement", dest, dest, _tmp1, [](auto a, auto b) { return a - b; });
			break;
		default:
			throw Problem("incrDecr", "Unknown increment decrement style operation!");
	}
}

} // namespace forth
