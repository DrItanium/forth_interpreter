#include "Types.h"
#include "Core.h"
#include "Problem.h"
#include "Datum.h"
#include <sstream>
#include <string>
#include <cmath>
#include <map>

namespace forth {
Core::Core(Core::OutputFunction output) : _output(output), _memory(new Datum[memoryCapacity]), _systemVariables(new Datum[systemVariableSize]) { }

Register& Core::getRegister(TargetRegister reg) {
	using Type = decltype(reg);
	switch (reg) {
		case Type::A:
			return _a;
		case Type::B:
			return _b;
		case Type::C:
			return _c;
		case Type::S:
			return _s;
		case Type::X:
			return _x;
		case Type::SP:
			return _sp;
		case Type::SP2:
			return _sp2;
        case Type::Temporary:
            return _tmp0;
        case Type::DP:
            return _dp;
        case Type::Index:
            return _index;
        case Type::Zero:
            return _zero;
		default:
			throw Problem("getRegister", "Undefined register!");
	}
}
Datum& Core::getSystemVariable(Address index) {
    if (getByteOffset(index) != 0) {
        throw Problem("getSystemVariable", "System variables are 8-bytes wide and must be accessed on 8-byte boundaries!");
    }
	if (!Core::inSystemVariableArea(index)) {
		std::stringstream ss;
		ss << "Illegal address: " << std::hex << index;
		auto msg = ss.str();
		throw Problem("getSystemVariable", msg);
	}
	// convert from byte oriented to word oriented
	return _systemVariables[(index - systemVariableStart) >> 3];
}


void Core::advanceMoleculePosition(Address amount) {
    _pc.increment(amount);
}


Operation Core::extractOperationFromMolecule() {
	return static_cast<Operation>(extractByteFromMolecule());
}

byte Core::extractByteFromMolecule() {
    auto b = loadByte(_pc.getAddress());
	advanceMoleculePosition();
	return b;
}

QuarterAddress Core::extractQuarterAddressFromMolecule() {
    auto q = loadQuarterAddress(_pc.getAddress());
	static_assert(sizeof(q) == 2, "q is not two bytes!");
	advanceMoleculePosition(sizeof(q));
	return q;
}
QuarterInteger Core::extractQuarterIntegerFromMolecule() {
    union {
        QuarterAddress v;
        QuarterInteger i;

    } k;
    k.v = loadQuarterAddress(_pc.getAddress());
	advanceMoleculePosition(sizeof(QuarterAddress));
	return k.i;
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
			onImmediate(_tmp1, immediate);
		} else {
			_tmp1.setValue(immediate);
		}
		auto tuple = std::forward_as_tuple(getRegister(std::get<0>(x)), getRegister(std::get<1>(x)), _tmp1);
		return tuple;
	} else if (fullForm(op)) {
		auto x = extractThreeRegisterForm();
		return std::forward_as_tuple(getRegister(std::get<0>(x)), getRegister(std::get<1>(x)), getRegister(std::get<2>(x)));
	} else {
		return std::forward_as_tuple(_c, _a, _b);
	}
}
Core::FourRegisterArguments Core::extractArguments4(Operation op) {
    auto x = extractFourRegisterForm();
    return std::forward_as_tuple(getRegister(std::get<0>(x)), getRegister(std::get<1>(x)), getRegister(std::get<2>(x)), getRegister(std::get<3>(x)));
}

Core::FourRegisterForm Core::extractFourRegisterForm() {
    auto regs = extractByteFromMolecule();
    auto regs2 = extractByteFromMolecule();
    return std::make_tuple(TargetRegister(getDestinationRegister(regs)),
            TargetRegister(getSourceRegister(regs)),
            TargetRegister(getDestinationRegister(regs2)),
            TargetRegister(getSourceRegister(regs2)));
}

Core::FiveRegisterArguments Core::extractArguments5(Operation op) {
    auto x = extractFiveRegisterForm();
    return std::forward_as_tuple(getRegister(std::get<0>(x)), getRegister(std::get<1>(x)), getRegister(std::get<2>(x)), 
            getRegister(std::get<3>(x)),
            getRegister(std::get<4>(x)));
}

Core::FiveRegisterForm Core::extractFiveRegisterForm() {
    auto regs = extractByteFromMolecule();
    auto regs2 = extractByteFromMolecule();
    auto regs3 = extractByteFromMolecule();
    return std::make_tuple(TargetRegister(getDestinationRegister(regs)),
            TargetRegister(getSourceRegister(regs)),
            TargetRegister(getDestinationRegister(regs2)),
            TargetRegister(getSourceRegister(regs2)),
            TargetRegister(getDestinationRegister(regs3)));
}

void Core::storeByte(Address addr, byte value) {
    auto& word = (addr <= largestByteAddress ) ? _memory[getNearestWordAddress(addr)] : getSystemVariable(addr);
	std::cout << "Installed to " << getNearestWordAddress(addr) << std::endl;
    word.setField(getByteOffset(addr), value);
}
void Core::store(Address addr, const Datum& value) {
    // check for unaligned addresses
    if (auto offset = getByteOffset(addr); offset != 0) {
        if (addr > largestByteAddress) {
            throw Problem("Core::store", "Can't do unaligned stores on system variables!");
        }
        for (auto loc = addr, j = 0ul; loc < (addr + sizeof(Address)); ++loc, ++j) {
            storeByte(loc, value.getField(j));
        }
    } else {
        auto wordAddr = getWordAddress(addr);
        if (wordAddr <= largestAddress) {
            _memory[wordAddr] = value.numValue;
        } else {
            // see if we're in the system variable area instead!
            getSystemVariable(addr).address = value.address;
        }
    }
}
byte Core::loadByte(Address addr) {
    return loadWord(getNearestWordAddress(addr)).getField(getByteOffset(addr));
}
QuarterAddress Core::loadQuarterAddress(forth::Address addr) {
    auto offset = getByteOffset(addr);
    switch(offset) {
        case 0:
            return loadWord(addr).lowestQuarterAddress();
        case 2:
            return loadWord(getNearestWordAddress(addr)).lowerQuarterAddress();
        case 4:
            return loadWord(getNearestWordAddress(addr)).higherQuarterAddress();
        case 6:
            return loadWord(getNearestWordAddress(addr)).highestQuarterAddress();
        case 1:
        case 3:
        case 5:
        case 7:
            break;
        default:
            throw Problem("Core::loadQuarterAddress", "bad offset");
    }
    Datum tmp(Address(0));
    tmp.setField(0, loadByte(addr));
    tmp.setField(1, loadByte(addr + 1));
    return tmp.lowestQuarterAddress();
}
HalfAddress Core::loadHalfAddress(Address addr) {
    auto offset = getByteOffset(addr);
    switch (offset) {
        case 0:
            return loadWord(addr).lowerHalfAddress();
        case 4:
            return loadWord(getNearestWordAddress(addr)).upperHalfAddress();
        case 1:
        case 2:
        case 3:
        case 5:
        case 6:
        case 7:
            break;
        default:
            throw Problem("Core::loadHalfAddress", "bad offset!");
    }
    Datum tmp(Address(0));
    tmp.setField(0, loadByte(addr + 0));
    tmp.setField(1, loadByte(addr + 1));
    tmp.setField(2, loadByte(addr + 2));
    tmp.setField(3, loadByte(addr + 3));
    return tmp.lowerHalfAddress();
}
Datum Core::loadWord(Address addr) {
    if (auto offset = getByteOffset(addr); offset != 0) {
        if (addr >= largestByteAddress) {
            throw Problem("loadWord", "Accessing system variables must occur on 8-byte boundaries!");
        } else {
            // do the much more expensive byte by byte load
            Datum storage(Address(0));
            for (auto loc = addr, offset = (Address)0; loc < (addr + sizeof(Address)); ++loc, ++offset) {
                storage.setField(offset, loadByte(loc));
            }
            return storage;
        }
    } else {
        if (addr <= largestByteAddress) {
            return _memory[getWordAddress(addr)];
        } else {
            return getSystemVariable(addr);
        }
    }
}

void Core::push(TargetRegister reg, TargetRegister sp) {
	auto& value = getRegister(reg);
    push(value.getValue(), sp);
}
void Core::push(const Datum& d, TargetRegister sp) {
	auto& stackPointer = getRegister(sp);
	if (sp == TargetRegister::SP) {
		// do a check and see if we have a full stack!
		if (auto maxAddress = getSystemVariable(spStackFull).address; stackPointer.getAddress() == maxAddress) {
			throw Problem("push_sp", "Parameter Stack Full!");
		}
	} else if (sp == TargetRegister::SP2) {
		// do a check and see if we have a full stack!
		if (auto maxAddress = getSystemVariable(sp2StackFull).address; stackPointer.getAddress() == maxAddress) {
			throw Problem("push_sp2", "Subroutine Stack Full!");
		}
	}
	stackPointer.decrement();
	store (stackPointer.getAddress(), d);
}

Datum Core::pop(TargetRegister sp) {
	auto& stackPointer = getRegister(sp);
	if (sp == TargetRegister::SP) {
		// do a check and see if we have a full stack!
		if (auto minAddress = getSystemVariable(spStackEmpty).address; stackPointer.getAddress() == minAddress) {
			throw Problem("push_sp", "Parameter Stack Empty!");
		}
	} else if (sp == TargetRegister::SP2) {
		// do a check and see if we have a full stack!
		if (auto minAddress = getSystemVariable(sp2StackEmpty).address; stackPointer.getAddress() == minAddress) {
			throw Problem("push_sp2", "Subroutine Stack Empty!");
		}
	}
	auto result = loadWord(stackPointer.getAddress());
	stackPointer.increment();
	return result;
}
void Core::pop(TargetRegister reg, TargetRegister sp) {
	auto& dest = getRegister(reg);
	auto result = pop(sp);
    dest.setValue(result);
}

template<typename T>
void numericOperation(Operation op, const std::string& name, Register& dest, const Register& src0, const Register& src1, T fn) {
	switch(involvesDiscriminantType(op)) {
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
			throw Problem(name, "ILLEGAL DISCRIMINANT!");
	}
}

template<typename T>
void numericOperationAndBool(Operation op, const std::string& name, Register& dest, const Register& src0, const Register& src1, T fn) {
	switch(involvesDiscriminantType(op)) {
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
			throw Problem(name, "ILLEGAL DISCRIMINANT!");
	}
}

template<typename T>
void numericOperationIntegerOnly(Operation op, const std::string& name, Register& dest, const Register& src0, const Register& src1, T fn) {
	switch(involvesDiscriminantType(op)) {
		case Discriminant::Number:
			dest.setValue(fn(src0.getInt(), src1.getInt()));
			break;
		case Discriminant::MemoryAddress:
			dest.setValue(fn(src0.getAddress(), src1.getAddress()));
			break;
		default:
			throw Problem(name, "ILLEGAL DISCRIMINANT!");
	}
}
void Core::numericCombine(Operation op) {
	auto result = extractArguments(op, [op](Register& r, auto val) {
				if (involvesDiscriminantType(op) == Discriminant::FloatingPoint) {
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
	numericOperation(op, subtract ? "-" : "+", dest, src0, src1, fn);
}

void Core::multiplyOperation(Operation op) {
	auto t = extractArguments(op, nullptr);
	auto& [dest, src0, src1] = t;
	auto fn = [](auto a, auto b) { return a * b; };
	numericOperation(op, "*", dest, src0, src1, fn);
}

void Core::divideOperation(Operation op) {

	auto t = extractArguments(op);
	auto& [dest, src0, src1] = t;
	auto isModulo = isModuloOperation(op);
	if (src1.getAddress() == 0) {
		throw Problem(isModulo ? "mod" : "/", "DIVIDE BY ZERO!");
	}
	if (isModulo) {
		numericOperationIntegerOnly(op, "mod", dest, src0, src1, [](auto a, auto b) { return a % b; });
	} else {
		numericOperation(op, "/", dest, src0, src1, [](auto a, auto b) { return a / b; });
	}
}

void Core::equalsOperation(Operation op) {
	auto t = extractArguments(op);
	auto& [dest, src0, src1] = t;
	numericOperationAndBool(op, "eq", dest, src0, src1, [](auto a, auto b) { return a == b; });
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
void numericBoolAndInteger(Operation op, const std::string& name, Register& dest, const Register& src0, T fn) {
	switch(involvesDiscriminantType(op)) {
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
			throw Problem(name, "ILLEGAL DISCRIMINANT!");
	}
}

template<typename T>
void numericBoolAndInteger(Operation op, const std::string& name, Register& dest, const Register& src0, const Register& src1, T fn) {
	switch(involvesDiscriminantType(op)) {
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
			throw Problem(name, "ILLEGAL DISCRIMINANT!");
	}
}

void Core::notOperation(Operation op) {
    std::tuple<TargetRegister, TargetRegister> tup;
    switch (op) {
        case Operation::Not:
            tup = std::make_tuple(TargetRegister::C, TargetRegister::A);
            break;
        case Operation::NotFull:
            tup = extractTwoRegisterForm();
            break;
        default:
            throw Problem("not", "ILLEGAL OPERATION!");
    }
	auto& dest = getRegister(std::get<0>(tup));
	auto& src = getRegister(std::get<1>(tup));
	switch (involvesDiscriminantType(op)) {
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
    std::tuple<TargetRegister, TargetRegister> tup;
    switch (op) {
        case Operation::Minus:
            tup = std::make_tuple(TargetRegister::C, TargetRegister::A);
            break;
        case Operation::MinusFull:
            tup = extractTwoRegisterForm();
            break;
        default:
            throw Problem("minus", "ILLEGAL OPERATION!");
    }
	auto& dest = getRegister(std::get<0>(tup));
	auto& src = getRegister(std::get<1>(tup));
	switch (involvesDiscriminantType(op)) {
		case Discriminant::Number:
			dest.setValue(-src.getInt());
			break;
		case Discriminant::FloatingPoint:
			dest.setValue(-src.getFP());
			break;
		default:
			throw Problem("minus", "ILLEGAL DISCRIMINANT");
	}
}

void Core::booleanAlgebra(Operation op) {
	auto tup = extractArguments(op);
	auto& [dest, src0, src1] = tup;
	if (andForm(op)) {
		numericBoolAndInteger(op, "and", dest, src0, src1, [](auto a, auto b) { if constexpr (std::is_same<decltype(a), bool>::value) { return a && b; } else { return a & b; }});
	} else if (orForm(op)) {
		numericBoolAndInteger(op, "or", dest, src0, src1, [](auto a, auto b) { if constexpr (std::is_same<decltype(a), bool>::value) { return a || b; } else { return a | b; }});
	} else if (xorForm(op)) {
		numericBoolAndInteger(op, "xor", dest, src0, src1, [](auto a, auto b) { if constexpr (std::is_same<decltype(a), bool>::value) { return a != b; } else { return a ^ b; }});
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
		case Operation::UnsignedShiftLeft:
		case Operation::UnsignedShiftLeftImmediate:
		case Operation::UnsignedShiftLeftFull:
			numericOperationIntegerOnly(op, "<<", dest, src0, src1, [](auto a, auto b) { return a << b; });
			break;
		case Operation::ShiftRight:
		case Operation::ShiftRightImmediate:
		case Operation::ShiftRightFull:
		case Operation::UnsignedShiftRight:
		case Operation::UnsignedShiftRightImmediate:
		case Operation::UnsignedShiftRightFull:
			numericOperationIntegerOnly(op, ">>", dest, src0, src1, [](auto a, auto b) { return a >> b; });
			break;
		default:
			throw Problem("shiftOperation", "Unknown shift operation!");
			
	}
}

void Core::powOperation(Operation op) {
	auto tup = extractArguments(op);
	auto& [dest, src0, src1] = tup;
	numericOperation(op, "pow", dest, src0, src1, [](auto a, auto b) { return static_cast<decltype(a)>(std::pow(Floating(a), Floating(b))); });
}

void Core::rangeChecks(Operation op) {
	auto tup = extractArguments(op);
	auto& [dest, src0, src1] = tup;
	switch (op) {
		case Operation::GreaterThan:
		case Operation::GreaterThanImmediate:
		case Operation::GreaterThanFull:
		case Operation::FloatingPointGreaterThan:
		case Operation::FloatingPointGreaterThanFull:
		case Operation::UnsignedGreaterThan:
		case Operation::UnsignedGreaterThanImmediate:
		case Operation::UnsignedGreaterThanFull:
			numericOperation(op, ">", dest, src0, src1, [](auto a, auto b) { return a > b; });
			break;
		case Operation::LessThan:
		case Operation::LessThanImmediate:
		case Operation::LessThanFull:
		case Operation::FloatingPointLessThan:
		case Operation::FloatingPointLessThanFull:
		case Operation::UnsignedLessThan:
		case Operation::UnsignedLessThanImmediate:
		case Operation::UnsignedLessThanFull:
			numericOperation(op, "<", dest, src0, src1, [](auto a, auto b) { return a < b; });
			break;
		default:
			throw Problem("rangeChecks", "Unknown range check operation!");
	}
}

void Core::incrDecr(Operation op) {
	auto tmp = extractByteFromMolecule();
	auto d = static_cast<TargetRegister>(getDestinationRegister(tmp));
	auto imm4 = getSourceRegister(tmp) + 1; // always increment by one since increment and decrementing by zero makes no sense
	_tmp1.setValue(Address(imm4));
	auto& dest = getRegister(d);
    _imm.setValue(dest.getValue());
	switch (op) {
		case Operation::Increment:
			numericOperation(op, "increment", dest, _imm, _tmp1, [](auto a, auto b) { return a + b; });
			break;
		case Operation::Decrement:
			numericOperation(op, "decrement", dest, _imm, _tmp1, [](auto a, auto b) { return a - b; });
			break;
		default:
			throw Problem("incrDecr", "Unknown increment decrement style operation!");
	}
}

constexpr HalfAddress makeImm24(QuarterAddress lower16, byte upper8) noexcept {
	return encodeBits<HalfAddress, QuarterAddress, 0x00FF0000, 16>(HalfAddress(lower16), static_cast<QuarterAddress>(upper8));
}
void Core::savePositionToSubroutineStack() {
    push(_pc.getValue(), TargetRegister::SP2);
}
void Core::jumpOperation(Operation op) {
    auto callSubroutine = [this]() {
        _advancePC = false;
        auto lower16 = extractQuarterIntegerFromMolecule();
        auto upper8 = extractByteFromMolecule();
        savePositionToSubroutineStack();
        _pc.setValue(Address(makeImm24(lower16, upper8)));
    };
    auto callSubroutineIndirect = [this]() {
        _advancePC = false;
        auto b = extractByteFromMolecule();
        savePositionToSubroutineStack();
        _pc.setValue(getRegister((TargetRegister)getDestinationRegister(b)).getValue());
    };
	switch (op) {
		case Operation::Jump:
            _advancePC = false;
			_pc.setValue(_pc.getInt() + extractQuarterIntegerFromMolecule());
			break;
		case Operation::JumpAbsolute: 
            _advancePC = false;
			_pc.setValue((Address)makeImm24(extractQuarterIntegerFromMolecule(), extractByteFromMolecule()));
			break;
		case Operation::JumpIndirect: 
            _advancePC = false;
			_pc.setValue(getRegister((TargetRegister)getDestinationRegister(extractByteFromMolecule())).getValue());
			break;
		case Operation::CallSubroutine: 
            callSubroutine();
            break;
		case Operation::CallSubroutineIndirect: 
            callSubroutineIndirect();
			break;
		case Operation::ReturnSubroutine: 
            _advancePC = false;
			_pc.setValue(pop(TargetRegister::SP2));
			break;
		default:
			throw Problem("jumpOperation", "unknown jump operation!");
	}
}
void Core::conditionalBranch(Operation op) {
	auto k = extractByteFromMolecule();
	if (getRegister(TargetRegister(getDestinationRegister(k))).getTruth()) {
        _advancePC = false;
		switch (op) {
			case Operation::ConditionalBranch:
				jumpOperation(Operation::Jump);
				break;
			case Operation::ConditionalBranchIndirect:
				_pc.setValue(getRegister((TargetRegister)getSourceRegister(k)).getValue());
				break;
			case Operation::ConditionalCallSubroutine:
				savePositionToSubroutineStack();
				_pc.setValue(_pc.getInt() + extractQuarterIntegerFromMolecule());
				break;
			case Operation::ConditionalCallSubroutineIndirect:
				savePositionToSubroutineStack();
				_pc.setValue(getRegister((TargetRegister)getSourceRegister(k)).getAddress());
				break;
			case Operation::ConditionalReturnSubroutine:
				_pc.setValue(pop(TargetRegister::SP2));
				break;
			default:
				throw Problem("conditionalBranch", "unknown conditional branch operation!");
		}
	} else {
		switch (op) {
			case Operation::ConditionalBranch:
			case Operation::ConditionalCallSubroutine:
                _pc.increment(2);
				break;
			case Operation::ConditionalBranchIndirect:
			case Operation::ConditionalReturnSubroutine:
			case Operation::ConditionalCallSubroutineIndirect:
				break;
			default:
				throw Problem("conditionalBranch", "unknown conditional branch operation!");
		}
	}
}

Address Core::extractImm48() {
    auto lowest16 = Address(extractQuarterAddressFromMolecule());
    auto lower16 = Address(extractQuarterAddressFromMolecule()) << 16;
    auto higher16 = Address(extractQuarterAddressFromMolecule()) << 32;
    return lowest16 & lower16 & higher16;
}

void Core::loadImm48(Operation op) {
    auto tr = TargetRegister(getDestinationRegister(extractByteFromMolecule()));
    auto imm48 = extractImm48();
    getRegister(tr).setValue(imm48);
}
void Core::nop(Operation op) { }

void Core::dispatchInstruction() {
	static std::map<Operation, decltype(std::mem_fn(&Core::numericCombine))> dispatchTable = {
#define DefEntry(t, fn) { Operation :: t , std::mem_fn<void(Operation)>(&Core:: fn ) }
#define DefEntryS(t, fn) DefEntry(t, fn)
#define DefEntryU(t, fn) DefEntry( Unsigned ## t , fn )
#define DefEntryF(t, fn) DefEntry( FloatingPoint ## t , fn )
#define DefEntryB(t, fn) DefEntry( Boolean ## t , fn )
#define DefEntrySUF(t, fn) DefEntryS(t, fn) , DefEntryU(t, fn), DefEntryF(t, fn)
#define DefEntrySU(t, fn) DefEntryS(t, fn) , DefEntryU(t, fn) 
#define DefEntrySUB(t, fn) DefEntrySU(t, fn) , DefEntryB(t, fn)
#define DefEntrySUFB(t, fn) DefEntrySUF(t, fn) , DefEntryB(t, fn)
		DefEntrySUF(Add,numericCombine),          DefEntrySUF(AddFull, numericCombine),         DefEntrySU(AddImmediate, numericCombine),
		DefEntrySUF(Subtract,numericCombine),     DefEntrySUF(SubtractFull, numericCombine),    DefEntrySU(SubtractImmediate, numericCombine),
		DefEntrySUF(Multiply, multiplyOperation), DefEntrySUF(MultiplyFull, multiplyOperation), DefEntrySU(MultiplyImmediate, multiplyOperation),
		DefEntrySUF(Divide, divideOperation),     DefEntrySUF(DivideFull, divideOperation),     DefEntrySU(DivideImmediate, divideOperation),
		DefEntrySU(Modulo, divideOperation),      DefEntrySU(ModuloFull, divideOperation),      DefEntrySU(ModuloImmediate, divideOperation),
		DefEntrySUB(Not, notOperation),           DefEntrySUB(NotFull, notOperation),
		DefEntrySUF(Minus, minusOperation),       DefEntrySUF(MinusFull, minusOperation),
		DefEntrySU(ShiftRight, shiftOperation),   DefEntrySU(ShiftRightFull, shiftOperation),   DefEntrySU(ShiftRightImmediate, shiftOperation),
		DefEntrySU(ShiftLeft, shiftOperation),    DefEntrySU(ShiftLeftFull, shiftOperation),   DefEntrySU(ShiftLeftImmediate, shiftOperation),
		DefEntrySUF(Pow, powOperation),           DefEntrySUF(PowFull, powOperation),
		DefEntrySUB(And, booleanAlgebra), DefEntrySUB(AndFull, booleanAlgebra), DefEntrySU(AndImmediate, booleanAlgebra),
		DefEntrySUB(Or, booleanAlgebra), DefEntrySUB(OrFull, booleanAlgebra), DefEntrySU(OrImmediate, booleanAlgebra),
		DefEntrySUB(Xor, booleanAlgebra), DefEntrySUB(XorFull, booleanAlgebra), DefEntrySU(XorImmediate, booleanAlgebra),
		DefEntrySUB(Equals, equalsOperation), DefEntrySUB(EqualsFull, equalsOperation), DefEntrySU(EqualsImmediate, equalsOperation),
		DefEntrySUF(GreaterThan, rangeChecks), DefEntrySUF(GreaterThanFull, rangeChecks), DefEntrySU(GreaterThanImmediate, rangeChecks),
		DefEntrySUF(LessThan, rangeChecks), DefEntrySUF(LessThanFull, rangeChecks), DefEntrySU(LessThanImmediate, rangeChecks),
		DefEntrySUF(Increment, incrDecr), DefEntrySUF(Decrement, incrDecr),
		DefEntry(LoadImmediateLower48, loadImm48),
		DefEntry(PopRegister, pop), DefEntry(PopA, pop), DefEntry(PopB, pop), DefEntry(PopC, pop),
		DefEntry(PushRegister, push), DefEntry(PushC, push), DefEntry(PushA, push), DefEntry(PushB, push),
		DefEntry(Load, loadStore), DefEntry(Store, loadStore),
		DefEntry(Move, moveOrSwap), DefEntry(Swap, moveOrSwap),
		DefEntrySUFB(TypeValue, typeValue),
		DefEntry(SetImmediate16_Lowest, setImm16), DefEntry(SetImmediate16_Lower, setImm16), 
		DefEntry(SetImmediate16_Higher, setImm16), DefEntry(SetImmediate16_Highest, setImm16),
		DefEntry(Jump, jumpOperation), DefEntry(JumpIndirect, jumpOperation), 
		DefEntry(JumpAbsolute, jumpOperation), DefEntry(CallSubroutine, jumpOperation), 
		DefEntry(CallSubroutineIndirect, jumpOperation), DefEntry(ReturnSubroutine, jumpOperation),
		DefEntry(ConditionalBranch, conditionalBranch), 
        DefEntry(ConditionalBranchIndirect, conditionalBranch), DefEntry(ConditionalCallSubroutine, conditionalBranch),
		DefEntry(ConditionalCallSubroutineIndirect, conditionalBranch), DefEntry(ConditionalReturnSubroutine, conditionalBranch),
		DefEntry(EncodeBits, encodeDecodeBits), DefEntry(DecodeBits, encodeDecodeBits),
		DefEntry(Nop, nop), DefEntry(LeaveExecutionLoop, returnToNative),
#undef DefEntry
	};
    auto op = static_cast<Operation>(extractByteFromMolecule());
    if (auto result = dispatchTable.find(op); result == dispatchTable.end()) {
		std::stringstream msg;
		msg << "Unknown instruction address: 0x" << std::hex << static_cast<int>(op);
		auto str = msg.str();
		throw Problem("dispatchInstruction", str);
    } else {
        result->second(this, op);
    }
}

void Core::loadStore(Operation op) {
    auto k = extractByteFromMolecule();
    auto trd = TargetRegister(getDestinationRegister(k));
    auto trs = TargetRegister(getSourceRegister(k));
    auto& dest = getRegister(trd);
    auto& src = getRegister(trs);
	if (op == Operation::Load) {
         dest.setValue(loadWord(src.getAddress()));
	} else if (op == Operation::Store) {
         store(dest.getAddress(), src.getValue());
	} else {
		throw Problem("loadStore", "Unknown load/store operation!");
	}
}

void Core::moveOrSwap(Operation op) {
    auto k = extractByteFromMolecule();
    auto trd = TargetRegister(getDestinationRegister(k));
    auto trs = TargetRegister(getSourceRegister(k));
    if (trd == trs) {
        return;
    }
    auto& dest = getRegister(trd);
    auto& src = getRegister(trs);
	if (op == Operation::Move) {
        dest.setValue(src.getAddress());
	} else if (op == Operation::Swap) {
        _tmp1.setValue(src.getValue());
        src.setValue(dest.getValue());
        dest.setValue(_tmp1.getValue());
	} else {
		throw Problem("moveOrSwap", "Unknown move/swap operation!");
	}
}

void Core::typeValue(Operation op) {
	auto tr = TargetRegister(getDestinationRegister(extractByteFromMolecule()));
	if (_output) {
		_output(involvesDiscriminantType(op), tr, getRegister(tr));
	} else {
		throw Problem("typeValue", "no output function defined!");
	}
}
void Core::setOutputFunction(Core::OutputFunction output) {
	_output = output;
}
enum class Immediate16Positions : byte {
	Lowest = 0,
	Lower,
	Higher,
	Highest,
};
template<Immediate16Positions pos>
constexpr auto Immediate16ShiftIndex = static_cast<Address>(pos) * 16;
template<Immediate16Positions pos>
constexpr auto Immediate16Mask = static_cast<Address>(0xFFFF) << Immediate16ShiftIndex<pos>;

template<Immediate16Positions pos>
constexpr Address computeImmediate16(Address base, QuarterAddress value) noexcept {
	return encodeBits<Address, QuarterAddress, Immediate16Mask<pos>, Immediate16ShiftIndex<pos>>(base, value);
}

void Core::setImm16(Operation op) {
	auto k = extractByteFromMolecule();
	auto tr = static_cast<TargetRegister>(getDestinationRegister(k));
    auto& dest = getRegister(tr);
    switch (op) {
        case Operation::SetImmediate16_Lowest:
            dest.setValue(computeImmediate16<Immediate16Positions::Lowest>(dest.getAddress(), extractQuarterAddressFromMolecule()));
            break;
        case Operation::SetImmediate16_Lower:
            dest.setValue(computeImmediate16<Immediate16Positions::Lower>(dest.getAddress(), extractQuarterAddressFromMolecule()));
            break;
        case Operation::SetImmediate16_Higher:
            dest.setValue(computeImmediate16<Immediate16Positions::Higher>(dest.getAddress(), extractQuarterAddressFromMolecule()));
            break;
        case Operation::SetImmediate16_Highest:
            dest.setValue(computeImmediate16<Immediate16Positions::Highest>(dest.getAddress(), extractQuarterAddressFromMolecule()));
            break;
        default:
            throw Problem("setImm16", "unknown set imm16 operation!");
    }
}

void Core::executionCycle(Address startAddress) {
    auto& value = getSystemVariable(Core::terminateExecutionVariable);
    value.address = 0;
    _pc.setValue(startAddress);
    while(value.address == 0) {
        // load the current address
        dispatchInstruction();
		if (value.address != 0) {
			break;
		}
        if (_advancePC) {
            // goto the next byte if it makes sense
            _pc.increment();
        }
        _pc.setValue(_pc.getAddress() & Core::largestByteAddress);
        _advancePC = true;
    }
    // we've halted at this point
}

void Core::encodeDecodeBits(Operation op) {
    if (op == Operation::DecodeBits) {
        auto t = extractArguments4(op);
        auto& [dest, value, mask, shift] = t;
        dest.setValue(decodeBits<Address, Address>(value.getAddress(), mask.getAddress(), shift.getAddress()));
    } else if (op == Operation::EncodeBits) {
        auto t = extractArguments5(op);
        auto& [dest, src, value, mask, shift] = t;
        dest.setValue(encodeBits<Address, Address>(src.getAddress(), value.getAddress(), mask.getAddress(), shift.getAddress()));
    } else {
        throw Problem("encodeDecodeBits", "unknown encode-decode operation!");
    }
}

std::function<void(Address, Address)> Core::getMemoryInstallationFunction() noexcept {
	return [this](Address location, Address value) { store(location, value); };
}

std::function<void(Address, Address)> Core::getInstructionInstallationFunction() noexcept {
	return [this](Address location, Address instruction) {
		auto width = getInstructionWidth(instruction);
		if (width == 0) {
			throw Problem("Core::getInstructionInstallationFunction", "Got an \"instruction\" with zero width!");
		}
		Datum conv(instruction);
		for (auto curr = location, i = Address(0); curr < (location + width); ++curr, ++i) {
			auto value = conv.getField(byte(i));
			std::cout << "Installing " << std::hex << int(value) << " to location " << std::hex << curr << std::endl;
			storeByte(curr, value);
		}
	};
}

void Core::returnToNative(Operation op) {
	if (op != Operation::LeaveExecutionLoop) {
		throw Problem("returnToNative", "Illegal operation provided!");
	}
	store(Core::terminateExecutionVariable, Address(1));
}


} // namespace forth
