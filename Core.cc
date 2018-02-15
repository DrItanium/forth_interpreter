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
QuarterInteger Core::extractQuarterIntegerFromMolecule() {
	auto q = Molecule(_currentMolecule.getAddress()).getQuarterOffset(_moleculePosition.getAddress());
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
	auto result = load(stackPointer.getAddress());
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
	auto result = extractArguments(op, [op, this](Register& r, auto val) {
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
	auto fn = [this](auto a, auto b) { return a * b; };
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
			numericOperation(op, ">", dest, src0, src1, [](auto a, auto b) { return a > b; });
			break;
		case Operation::LessThan:
		case Operation::LessThanImmediate:
		case Operation::LessThanFull:
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
	switch (op) {
		case Operation::Increment:
			numericOperation(op, "increment", dest, dest, _tmp1, [](auto a, auto b) { return a + b; });
			break;
		case Operation::Decrement:
			numericOperation(op, "decrement", dest, dest, _tmp1, [](auto a, auto b) { return a - b; });
			break;
		default:
			throw Problem("incrDecr", "Unknown increment decrement style operation!");
	}
}

constexpr HalfAddress makeImm24(QuarterAddress lower16, byte upper8) noexcept {
	return encodeBits<HalfAddress, QuarterAddress, 0x00FF0000, 16>(HalfAddress(lower16), static_cast<QuarterAddress>(upper8));
}
void Core::savePositionToSubroutineStack() {
	auto value = _moleculePosition.getValue();
	++value.address;
    if (value.address >= 8) {
        push(_pc.getAddress() + 1, TargetRegister::SP2);
        push(Address(0), TargetRegister::SP2);
    } else {
        push(_pc.getValue(), TargetRegister::SP2);
        push(value, TargetRegister::SP2);
    }
}
void Core::jumpOperation(Operation op) {
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
            _advancePC = false;
			savePositionToSubroutineStack();
			_pc.setValue((Address)makeImm24(extractQuarterIntegerFromMolecule(), extractByteFromMolecule()));
			break;
		case Operation::CallSubroutineIndirect: 
            _advancePC = false;
			savePositionToSubroutineStack();
			_pc.setValue(getRegister((TargetRegister)getDestinationRegister(extractByteFromMolecule())).getValue());
			break;
		case Operation::ReturnSubroutine: 
            _advancePC = false;
			_moleculePosition.setValue(pop(TargetRegister::SP2));
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
			case Operation::ConditionalBranchAbsolute:
				jumpOperation(Operation::JumpAbsolute);
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
				_moleculePosition.setValue(pop(TargetRegister::SP2));
				_pc.setValue(pop(TargetRegister::SP2));
				break;
			default:
				throw Problem("conditionalBranch", "unknown conditional branch operation!");
		}
	} else {
		switch (op) {
			case Operation::ConditionalBranch:
			case Operation::ConditionalBranchAbsolute:
			case Operation::ConditionalCallSubroutine:
				_moleculePosition.increment(2);
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
	auto b = Molecule(_currentMolecule.getAddress()).getImm48(_moleculePosition.getAddress());
	advanceMoleculePosition(6);
	return b;
}

void Core::loadImm48(Operation op) {
	getRegister((TargetRegister)(getDestinationRegister(extractByteFromMolecule()))).setValue(extractImm48());
}

void Core::dispatchInstruction(const Molecule& m) {
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
		DefEntry(ConditionalBranch, conditionalBranch), DefEntry(ConditionalBranchAbsolute, conditionalBranch),
		DefEntry(ConditionalBranchIndirect, conditionalBranch), DefEntry(ConditionalCallSubroutine, conditionalBranch),
		DefEntry(ConditionalCallSubroutineIndirect, conditionalBranch), DefEntry(ConditionalReturnSubroutine, conditionalBranch),
#undef DefEntry
	};
	auto throwError = [](Operation op) {
		std::stringstream msg;
		msg << "Unknown instruction address: 0x" << std::hex << static_cast<int>(op);
		auto str = msg.str();
		throw Problem("dispatchInstruction", str);
	};
	setCurrentMolecule(m);
	while (_moleculePosition.getAddress() < sizeof(Molecule)) {
		auto op = static_cast<Operation>(extractByteFromMolecule());
		if (op == Operation::Stop) {
			return;
		} else {
			if (auto result = dispatchTable.find(op); result == dispatchTable.end()) {
				throwError(op);
			} else {
				result->second(this, op);
			}
		}
	}
}

void Core::loadStore(Operation op) {
    auto k = extractByteFromMolecule();
    auto trd = TargetRegister(getDestinationRegister(k));
    auto trs = TargetRegister(getSourceRegister(k));
    auto& dest = getRegister(trd);
    auto& src = getRegister(trs);
	if (op == Operation::Load) {
         dest.setValue(load(src.getAddress()));
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
        auto value = load(_pc.getAddress());
        dispatchInstruction(value.address);
        if (_advancePC) {
            _pc.increment();
        }
        _pc.setValue(_pc.getAddress() & Core::largestAddress);
        _advancePC = true;
    }
    // we've halted at this point
}


} // namespace forth
