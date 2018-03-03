#include "Types.h"
#include "Core.h"
#include "Problem.h"
#include "Datum.h"
#include "DictionaryEntry.h"
#include <sstream>
#include <string>
#include <cmath>
#include <map>

namespace forth {
Core::Core() : _memory(new Datum[memoryCapacity]), _systemVariables(new Datum[systemVariableSize]) { }

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
Register& Core::getDestinationRegister(byte value) {
	return getRegister(forth::getDestinationRegister(value));
}
Register& Core::getSourceRegister(byte value) {
	return getRegister(forth::getSourceRegister(value));
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


byte Core::extractByteFromMolecule() {
    auto b = loadByte(_pc.getAddress());
	advanceMoleculePosition();
	return b;
}

QuarterAddress Core::extractQuarterAddressFromMolecule() {
    auto q = loadQuarterAddress(_pc.getAddress());
	advanceMoleculePosition(2);
	return q;
}
QuarterInteger Core::extractQuarterIntegerFromMolecule() {
    union {
        QuarterAddress v;
        QuarterInteger i;

    } k;
    k.v = loadQuarterAddress(_pc.getAddress());
	advanceMoleculePosition(2);
	return k.i;
}



void Core::storeByte(Address addr, byte value) {
    auto& word = (addr <= largestByteAddress ) ? _memory[getWordAddress(addr)] : getSystemVariable(addr);
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
	stackPointer.decrement(sizeof(Address));
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
	stackPointer.increment(sizeof(Address));
	return result;
}
void Core::pop(TargetRegister reg, TargetRegister sp) {
	auto& dest = getRegister(reg);
	auto result = pop(sp);
    dest.setValue(result);
}

//template<typename T>
//void numericOperation(Operation op, const std::string& name, Register& dest, const Register& src0, const Register& src1, T fn) {
//    if (auto k = involvesDiscriminantType(op); k) {
//        switch (k.value()) {
//            case Discriminant::Number:
//                dest.setValue(fn(src0.getInt(), src1.getInt()));
//                break;
//            case Discriminant::MemoryAddress:
//                dest.setValue(fn(src0.getAddress(), src1.getAddress()));
//                break;
//            case Discriminant::FloatingPoint:
//                dest.setValue(fn(src0.getFP(), src1.getFP()));
//                break;
//            default:
//                throw Problem(name, "ILLEGAL DISCRIMINANT!");
//        }
//    }
//}
//
//template<typename T>
//void numericOperationAndBool(Operation op, const std::string& name, Register& dest, const Register& src0, const Register& src1, T fn) {
//    if (auto k = involvesDiscriminantType(op); k) {
//        switch(k.value()) {
//            case Discriminant::Number:
//                dest.setValue(fn(src0.getInt(), src1.getInt()));
//                break;
//            case Discriminant::MemoryAddress:
//                dest.setValue(fn(src0.getAddress(), src1.getAddress()));
//                break;
//            case Discriminant::FloatingPoint:
//                dest.setValue(fn(src0.getFP(), src1.getFP()));
//                break;
//            case Discriminant::Boolean:
//                dest.setValue(fn(src0.getTruth(), src1.getTruth()));
//                break;
//            default:
//                throw Problem(name, "ILLEGAL DISCRIMINANT!");
//        }
//    }
//}
//
//template<typename T>
//void numericOperationIntegerOnly(Operation op, const std::string& name, Register& dest, const Register& src0, const Register& src1, T fn) {
//    if (auto k = involvesDiscriminantType(op)) {
//        switch(k.value()) {
//            case Discriminant::Number:
//                dest.setValue(fn(src0.getInt(), src1.getInt()));
//                break;
//            case Discriminant::MemoryAddress:
//                dest.setValue(fn(src0.getAddress(), src1.getAddress()));
//                break;
//            default:
//                throw Problem(name, "ILLEGAL DISCRIMINANT!");
//        }
//    }
//}
//void Core::numericCombine(Operation op, DecodedArguments args) {
//	//auto subtract = subtractOperation(std::get<Operation>(op));
//    //auto value = std::get<DecodedArguments>(op);
//	//auto fn = [subtract](auto a, auto b) { 
//	//	return subtract ? (a - b) : (a + b );
//	//};
//    //if (std::holds_alternative<ThreeRegister>(value)) {
//
//    //} else if (std::holds_alternative<NoArguments>(value)) {
//
//    //} else {
//    //    throw Problem(__FUNCTION__, "ILLEGAL OPERATION VARIANT!");
//    //}
//	//auto& [dest, src0, src1] = result;
//	//numericOperation(op, subtract ? "-" : "+", dest, src0, src1, fn);
//}
//
//void Core::multiplyOperation(Operation op, DecodedArguments args) {
//	//auto t = extractArguments(op, nullptr);
//	//auto& [dest, src0, src1] = t;
//	//auto fn = [](auto a, auto b) { return a * b; };
//	//numericOperation(op, "*", dest, src0, src1, fn);
//}
//
//void Core::divideOperation(Operation op, DecodedArguments args) {
//	//auto t = extractArguments(op);
//	//auto& [dest, src0, src1] = t;
//	//auto isModulo = isModuloOperation(op);
//	//if (src1.getAddress() == 0) {
//	//	throw Problem(isModulo ? "mod" : "/", "DIVIDE BY ZERO!");
//	//}
//	//if (isModulo) {
//	//	numericOperationIntegerOnly(op, "mod", dest, src0, src1, [](auto a, auto b) { return a % b; });
//	//} else {
//	//	numericOperation(op, "/", dest, src0, src1, [](auto a, auto b) { return a / b; });
//	//}
//}
//
//void Core::equalsOperation(Operation op, DecodedArguments args) {
//	//auto t = extractArguments(op);
//	//auto& [dest, src0, src1] = t;
//	//numericOperationAndBool(op, "eq", dest, src0, src1, [](auto a, auto b) { return a == b; });
//}
//
//void Core::push(Operation op, DecodedArguments args) {
//	//switch (op) {
//	//	case Operation::PushA:
//	//		push(TargetRegister::A, TargetRegister::SP);
//	//		break;
//	//	case Operation::PushB:
//	//		push(TargetRegister::B, TargetRegister::SP);
//	//		break;
//	//	case Operation::PushC:
//	//		push(TargetRegister::C, TargetRegister::SP);
//	//		break;
//	//	default: {
//	//				 auto args = extractTwoRegisterForm();
//	//				 // push sp, src
//	//				 push(std::get<1>(args), std::get<0>(args));
//	//				 break;
//	//			 }
//	//}
//}
//
//void Core::pop(Operation op, DecodedArguments args) {
//	//switch (op) {
//	//	case Operation::PopA:
//	//		pop(TargetRegister::A, TargetRegister::SP);
//	//		break;
//	//	case Operation::PopB:
//	//		pop(TargetRegister::B, TargetRegister::SP);
//	//		break;
//	//	case Operation::PopC:
//	//		pop(TargetRegister::C, TargetRegister::SP);
//	//		break;
//	//	default: {
//	//				 auto args = extractTwoRegisterForm();
//	//				 // pop dest, sp
//	//				 pop(std::get<0>(args), std::get<1>(args));
//	//				 break;
//	//			 }
//	//}
//}
//
//template<typename T>
//void numericBoolAndInteger(Operation op, const std::string& name, Register& dest, const Register& src0, T fn) {
//	//switch(involvesDiscriminantType(op)) {
//	//	case Discriminant::Number:
//	//		dest.setValue(fn(src0.getInt()));
//	//		break;
//	//	case Discriminant::MemoryAddress:
//	//		dest.setValue(fn(src0.getAddress()));
//	//		break;
//	//	case Discriminant::Boolean:
//	//		dest.setValue(fn(src0.getTruth()));
//	//		break;
//	//	default:
//	//		throw Problem(name, "ILLEGAL DISCRIMINANT!");
//	//}
//}
//
//template<typename T>
//void numericBoolAndInteger(Operation op, const std::string& name, Register& dest, const Register& src0, const Register& src1, T fn) {
//	//switch(involvesDiscriminantType(op)) {
//	//	case Discriminant::Number:
//	//		dest.setValue(fn(src0.getInt(), src1.getInt()));
//	//		break;
//	//	case Discriminant::MemoryAddress:
//	//		dest.setValue(fn(src0.getAddress(), src1.getAddress()));
//	//		break;
//	//	case Discriminant::Boolean:
//	//		dest.setValue(fn(src0.getTruth(), src1.getTruth()));
//	//		break;
//	//	default:
//	//		throw Problem(name, "ILLEGAL DISCRIMINANT!");
//	//}
//}
//
//void Core::notOperation(Operation op, DecodedArguments args) {
//    //std::tuple<TargetRegister, TargetRegister> tup;
//    //switch (op) {
//	//	case Operation::BooleanNot:
//	//	case Operation::UnsignedNot:
//    //    case Operation::Not:
//    //        tup = std::make_tuple(TargetRegister::C, TargetRegister::A);
//    //        break;
//    //    case Operation::BooleanNotFull:
//	//	case Operation::UnsignedNotFull:
//    //    case Operation::NotFull:
//    //        tup = extractTwoRegisterForm();
//    //        break;
//    //    default:
//    //        throw Problem("not", "ILLEGAL OPERATION!");
//    //}
//	//auto& dest = getRegister(std::get<0>(tup));
//	//auto& src = getRegister(std::get<1>(tup));
//	//switch (involvesDiscriminantType(op)) {
//	//	case Discriminant::Number:
//	//		dest.setValue(~src.getInt());
//	//		break;
//	//	case Discriminant::MemoryAddress:
//	//		dest.setValue(~src.getAddress());
//	//		break;
//	//	case Discriminant::Boolean:
//	//		dest.setValue(!src.getTruth());
//	//		break;
//	//	default:
//	//		throw Problem("not", "ILLEGAL DISCRIMINANT");
//	//}
//}
//
//void Core::minusOperation(Operation op, DecodedArguments args) {
//	//switch (involvesDiscriminantType(op)) {
//	//	case Discriminant::Number:
//	//		dest.setValue(-src.getInt());
//	//		break;
//	//	case Discriminant::FloatingPoint:
//	//		dest.setValue(-src.getFP());
//	//		break;
//	//	default:
//	//		throw Problem("minus", "ILLEGAL DISCRIMINANT");
//	//}
//}
//
//void Core::booleanAlgebra(Operation op, DecodedArguments args) {
//	//auto tup = extractArguments(op);
//	//auto& [dest, src0, src1] = tup;
//	//if (andForm(op)) {
//	//	numericBoolAndInteger(op, "and", dest, src0, src1, [](auto a, auto b) { if constexpr (std::is_same<decltype(a), bool>::value) { return a && b; } else { return a & b; }});
//	//} else if (orForm(op)) {
//	//	numericBoolAndInteger(op, "or", dest, src0, src1, [](auto a, auto b) { if constexpr (std::is_same<decltype(a), bool>::value) { return a || b; } else { return a | b; }});
//	//} else if (xorForm(op)) {
//	//	numericBoolAndInteger(op, "xor", dest, src0, src1, [](auto a, auto b) { if constexpr (std::is_same<decltype(a), bool>::value) { return a != b; } else { return a ^ b; }});
//	//} else {
//	//	throw Problem("booleanAlgebra", "UNKNOWN OPERATION GROUP!");
//	//}
//}
//
//void Core::shiftOperation(Operation op, DecodedArguments args) {
//	//auto tup = extractArguments(op);
//	//auto& [dest, src0, src1] = tup;
//	//switch (op) {
//	//	case Operation::ShiftLeft:
//	//	case Operation::ShiftLeftImmediate:
//	//	case Operation::ShiftLeftFull:
//	//	case Operation::UnsignedShiftLeft:
//	//	case Operation::UnsignedShiftLeftImmediate:
//	//	case Operation::UnsignedShiftLeftFull:
//	//		numericOperationIntegerOnly(op, "<<", dest, src0, src1, [](auto a, auto b) { return a << b; });
//	//		break;
//	//	case Operation::ShiftRight:
//	//	case Operation::ShiftRightImmediate:
//	//	case Operation::ShiftRightFull:
//	//	case Operation::UnsignedShiftRight:
//	//	case Operation::UnsignedShiftRightImmediate:
//	//	case Operation::UnsignedShiftRightFull:
//	//		numericOperationIntegerOnly(op, ">>", dest, src0, src1, [](auto a, auto b) { return a >> b; });
//	//		break;
//	//	default:
//	//		throw Problem("shiftOperation", "Unknown shift operation!");
//	//		
//	//}
//}
//
//void Core::powOperation(Operation op, DecodedArguments args) {
//	//auto tup = extractArguments(op);
//	//auto& [dest, src0, src1] = tup;
//	//numericOperation(op, "pow", dest, src0, src1, [](auto a, auto b) { return static_cast<decltype(a)>(std::pow(Floating(a), Floating(b))); });
//}
//
//void Core::rangeChecks(Operation op, DecodedArguments args) {
//	//auto tup = extractArguments(op);
//	//auto& [dest, src0, src1] = tup;
//	//switch (op) {
//	//	case Operation::GreaterThan:
//	//	case Operation::GreaterThanImmediate:
//	//	case Operation::GreaterThanFull:
//	//	case Operation::FloatingPointGreaterThan:
//	//	case Operation::FloatingPointGreaterThanFull:
//	//	case Operation::UnsignedGreaterThan:
//	//	case Operation::UnsignedGreaterThanImmediate:
//	//	case Operation::UnsignedGreaterThanFull:
//	//		numericOperation(op, ">", dest, src0, src1, [](auto a, auto b) { return a > b; });
//	//		break;
//	//	case Operation::LessThan:
//	//	case Operation::LessThanImmediate:
//	//	case Operation::LessThanFull:
//	//	case Operation::FloatingPointLessThan:
//	//	case Operation::FloatingPointLessThanFull:
//	//	case Operation::UnsignedLessThan:
//	//	case Operation::UnsignedLessThanImmediate:
//	//	case Operation::UnsignedLessThanFull:
//	//		numericOperation(op, "<", dest, src0, src1, [](auto a, auto b) { return a < b; });
//	//		break;
//	//	default:
//	//		throw Problem("rangeChecks", "Unknown range check operation!");
//	//}
//}


constexpr HalfAddress makeImm24(QuarterAddress lower16, byte upper8) noexcept {
	return encodeBits<HalfAddress, QuarterAddress, 0x00FF0000, 16>(HalfAddress(lower16), static_cast<QuarterAddress>(upper8));
}
void Core::savePositionToSubroutineStack() {
    push(_pc.getValue(), TargetRegister::SP2);
}
//void Core::jumpOperation(Operation op, DecodedArguments args) {
//	/*
//    auto callSubroutine = [this]() {
//        auto lower16 = extractQuarterIntegerFromMolecule();
//        auto upper8 = extractByteFromMolecule();
//        savePositionToSubroutineStack();
//        _pc.setValue(Address(makeImm24(lower16, upper8)));
//    };
//    auto callSubroutineIndirect = [this]() {
//        auto b = extractByteFromMolecule();
//        savePositionToSubroutineStack();
//        _pc.setValue(getRegister((TargetRegister)getDestinationRegister(b)).getValue());
//    };
//	auto jumpRelativeToPC = [this]() {
//		auto offset = extractQuarterIntegerFromMolecule();
//		_pc.setValue(_pc.getInt() + offset);
//	};
//	switch (op) {
//		case Operation::Jump:
//			jumpRelativeToPC();
//			break;
//		case Operation::JumpAbsolute: 
//			_pc.setValue((Address)makeImm24(extractQuarterIntegerFromMolecule(), extractByteFromMolecule()));
//			break;
//		case Operation::JumpIndirect: 
//			_pc.setValue(getRegister((TargetRegister)getDestinationRegister(extractByteFromMolecule())).getValue());
//			break;
//		case Operation::CallSubroutine: 
//            callSubroutine();
//            break;
//		case Operation::CallSubroutineIndirect: 
//            callSubroutineIndirect();
//			break;
//		case Operation::ReturnSubroutine: 
//			_pc.setValue(pop(TargetRegister::SP2));
//			break;
//		default:
//			throw Problem("jumpOperation", "unknown jump operation!");
//	}
//	*/
//}
//void Core::conditionalBranch(Operation op, DecodedArguments args) {
//	//auto k = extractByteFromMolecule();
//	//auto& cond = getRegister(TargetRegister(getDestinationRegister(k)));
//	//auto jumpRelativeToPC = [this]() {
//	//	auto offset = extractQuarterIntegerFromMolecule();
//	//	_pc.setValue(_pc.getInt() + offset);
//	//	// the offset is messed up right now and is ahead by two bytes so we
//	//	// have to move the address back three places as the address is computed
//	//	// relative to the _front_ of the instruction
//	//};
//	//if (cond.getTruth()) {
//	//	switch (op) {
//	//		case Operation::ConditionalBranch:
//	//			jumpRelativeToPC();
//	//			break;
//	//		case Operation::ConditionalBranchIndirect:
//	//			_pc.setValue(getRegister((TargetRegister)getSourceRegister(k)).getValue());
//	//			break;
//	//		case Operation::ConditionalCallSubroutine:
//	//			savePositionToSubroutineStack();
//	//			_pc.setValue(_pc.getInt() + extractQuarterIntegerFromMolecule());
//	//			break;
//	//		case Operation::ConditionalCallSubroutineIndirect:
//	//			savePositionToSubroutineStack();
//	//			_pc.setValue(getRegister((TargetRegister)getSourceRegister(k)).getAddress());
//	//			break;
//	//		case Operation::ConditionalReturnSubroutine:
//	//			_pc.setValue(pop(TargetRegister::SP2));
//	//			break;
//	//		default:
//	//			throw Problem("conditionalBranch", "unknown conditional branch operation!");
//	//	}
//	//} else {
//	//	switch (op) {
//	//		case Operation::ConditionalBranch:
//	//		case Operation::ConditionalCallSubroutine:
//	//			(void)extractQuarterAddressFromMolecule();
//	//			break;
//	//		case Operation::ConditionalBranchIndirect:
//	//		case Operation::ConditionalReturnSubroutine:
//	//		case Operation::ConditionalCallSubroutineIndirect:
//	//			break;
//	//		default:
//	//			throw Problem("conditionalBranch", "unknown conditional branch operation!");
//	//	}
//	//}
//}

Address Core::extractImm48() {
    auto lowest16 = Address(extractQuarterAddressFromMolecule());
    auto lower16 = Address(extractQuarterAddressFromMolecule()) << 16;
    auto higher16 = Address(extractQuarterAddressFromMolecule()) << 32;
    return lowest16 | lower16 | higher16;
}

//void Core::loadImm48(Operation op, DecodedArguments args) {
//    //auto tr = TargetRegister(getDestinationRegister(extractByteFromMolecule()));
//    //auto imm48 = extractImm48();
//    //getRegister(tr).setValue(imm48);
//}
//void Core::nop(Operation op, DecodedArguments args) { }
void Core::dispatchInstruction() {
    // okay now we start with the variant kind
	/*
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
		DefEntrySUF(Add,numericCombine),          DefEntrySU(AddImmediate, numericCombine),
		DefEntrySUF(Subtract,numericCombine),     DefEntrySU(SubtractImmediate, numericCombine),
		DefEntrySUF(Multiply, multiplyOperation), DefEntrySU(MultiplyImmediate, multiplyOperation),
		DefEntrySUF(Divide, divideOperation),     DefEntrySU(DivideImmediate, divideOperation),
		DefEntrySU(Modulo, divideOperation),      DefEntrySU(ModuloImmediate, divideOperation),
		DefEntrySUB(Not, notOperation),           
		DefEntrySUF(Minus, minusOperation),       
		DefEntrySU(ShiftRight, shiftOperation),   DefEntrySU(ShiftRightImmediate, shiftOperation),
		DefEntrySU(ShiftLeft, shiftOperation),    DefEntrySU(ShiftLeftImmediate, shiftOperation),
		DefEntrySUF(Pow, powOperation),           
		DefEntrySUB(And, booleanAlgebra), DefEntrySU(AndImmediate, booleanAlgebra),
		DefEntrySUB(Or, booleanAlgebra), DefEntrySU(OrImmediate, booleanAlgebra),
		DefEntrySUB(Xor, booleanAlgebra), DefEntrySU(XorImmediate, booleanAlgebra),
		DefEntrySUB(Equals, equalsOperation), DefEntrySU(EqualsImmediate, equalsOperation),
		DefEntrySUF(GreaterThan, rangeChecks), 
		DefEntrySU(GreaterThanImmediate, rangeChecks),
		DefEntrySUF(LessThan, rangeChecks), 
		DefEntrySU(LessThanImmediate, rangeChecks),
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
		DefEntry(PrintString, printString), DefEntry(PrintChar, printString), DefEntry(TypeDatum, printString),
#undef DefEntry
	};
    auto decodedOp = decode();
    auto op = std::get<Operation>(decodedOp);
    if (auto result = dispatchTable.find(op); result == dispatchTable.end()) {
		std::stringstream msg;
		msg << "Unknown instruction address: 0x" << std::hex << static_cast<int>(op);
		auto str = msg.str();
		throw Problem("dispatchInstruction", str);
    } else {
        result->second(this, decodedOp);
    }
	*/
}

//void Core::loadStore(Operation op, DecodedArguments args) {
//	/*
//    auto k = extractByteFromMolecule();
//    auto trd = TargetRegister(getDestinationRegister(k));
//    auto trs = TargetRegister(getSourceRegister(k));
//    auto& dest = getRegister(trd);
//    auto& src = getRegister(trs);
//	if (op == Operation::Load) {
//         dest.setValue(loadWord(src.getAddress()));
//	} else if (op == Operation::Store) {
//         store(dest.getAddress(), src.getValue());
//	} else {
//		throw Problem("loadStore", "Unknown load/store operation!");
//	}
//	*/
//}
//
//void Core::moveOrSwap(Operation op, DecodedArguments args) {
//	/*
//    auto k = extractByteFromMolecule();
//    auto trd = TargetRegister(getDestinationRegister(k));
//    auto trs = TargetRegister(getSourceRegister(k));
//    if (trd == trs) {
//        return;
//    }
//    auto& dest = getRegister(trd);
//    auto& src = getRegister(trs);
//	if (op == Operation::Move) {
//        dest.setValue(src.getAddress());
//	} else if (op == Operation::Swap) {
//        _tmp1.setValue(src.getValue());
//        src.setValue(dest.getValue());
//        dest.setValue(_tmp1.getValue());
//	} else {
//		throw Problem("moveOrSwap", "Unknown move/swap operation!");
//	}
//	*/
//}
//
//void Core::typeValue(Operation op, DecodedArguments args) {
//	/*
//	auto tr = TargetRegister(getDestinationRegister(extractByteFromMolecule()));
//	auto flags = std::cout.flags();
//	auto value = getRegister(tr).getValue();
//	switch(involvesDiscriminantType(op)) {
//		case Discriminant::Number:
//			std::cout << std::dec << value.numValue;
//			break;
//		case Discriminant::FloatingPoint:
//			std::cout << value.fp;
//			break;
//		case Discriminant::MemoryAddress:
//			std::cout << std::hex << value.address << "#";
//			break;
//		case Discriminant::Boolean:
//			std::cout << std::boolalpha << value.truth << std::noboolalpha;
//			break;
//		case Discriminant::Word:
//			std::cout << std::hex << value.entry << ": " << std::dec << value.entry->getName();
//			break;
//		default:
//			throw Problem("typeValue", "BAD DISCRIMINANT!");
//
//	}
//	std::cout << ' ' << std::endl;
//	std::cout.setf(flags);
//	*/
//}
//enum class Immediate16Positions : byte {
//	Lowest = 0,
//	Lower,
//	Higher,
//	Highest,
//};
//template<Immediate16Positions pos>
//constexpr auto Immediate16ShiftIndex = static_cast<Address>(pos) << 4;
//template<Immediate16Positions pos>
//constexpr auto Immediate16Mask = static_cast<Address>(0xFFFF) << Immediate16ShiftIndex<pos>;
//
//template<Immediate16Positions pos>
//constexpr Address computeImmediate16(Address base, QuarterAddress value) noexcept {
//	return encodeBits<Address, QuarterAddress, Immediate16Mask<pos>, Immediate16ShiftIndex<pos>>(base, value);
//}
//
//void Core::setImm16(Operation op, DecodedArguments args) {
//	/*
//	auto k = extractByteFromMolecule();
//	auto tr = static_cast<TargetRegister>(getDestinationRegister(k));
//    auto& dest = getRegister(tr);
//	auto oldPC = _pc.getAddress();
//	auto q = extractQuarterAddressFromMolecule();
//	if (oldPC != (_pc.getAddress() - 2)) {
//		throw Problem("setImm16", "PC is not being correctly updated!");
//	}
//	auto addr = dest.getAddress();
//    switch (op) {
//        case Operation::SetImmediate16_Lowest:
//            dest.setValue(computeImmediate16<Immediate16Positions::Lowest>(addr, q)); 
//            break;
//        case Operation::SetImmediate16_Lower:
//            dest.setValue(computeImmediate16<Immediate16Positions::Lower>(addr, q));
//            break;
//        case Operation::SetImmediate16_Higher:
//            dest.setValue(computeImmediate16<Immediate16Positions::Higher>(addr, q));
//            break;
//        case Operation::SetImmediate16_Highest:
//            dest.setValue(computeImmediate16<Immediate16Positions::Highest>(addr, q));
//            break;
//        default:
//            throw Problem("setImm16", "unknown set imm16 operation!");
//    }
//	if (op == Operation::SetImmediate16_Highest) {
//		if (dest.getAddress() != (addr | (Address(q) << 48))) {
//			throw Problem("setImm16", "Corruption of destination by setImm16");
//		}
//	}
//	*/
//}

void Core::executionCycle(Address startAddress) {
    auto& value = getSystemVariable(Core::terminateExecutionVariable);
	auto& rmc = getSystemVariable(Core::returnToMicrocode);
    value.address = 0;
    _pc.setValue(startAddress);
    while(value.address == 0) {
        // load the current address
        dispatchInstruction();

		if (rmc.truth) {
			// reset it
			rmc.address = 0;
			break;
		}
        _pc.setValue(_pc.getAddress() & Core::largestByteAddress);
    }
    // we've halted at this point
}

//void Core::encodeDecodeBits(Operation op, DecodedArguments args) {
//	std::visit([op, this](auto&& value) {
//				using T = std::decay_t<decltype(value)>;
//				if constexpr (std::is_same_v<T, FourRegister>) {
//					if (op == Operation::DecodeBits) {
//						value.destination.value().get().setValue(
//								decodeBits<Address, Address>(value.source.value().get().getAddress(),
//															 value.source2.value().get().getAddress(),
//															 value.source3.value().get().getAddress()));
//					} else {
//						throw Problem("encodeDecodeBits", "Unexpected four argument operation!");
//					}
//				} else if constexpr (std::is_same_v<T, FiveRegister>) {
//					if (op == Operation::EncodeBits) {
//
//					} else {
//					}
//				} else {
//        			throw Problem("encodeDecodeBits", "unknown encode-decode operation!");
//				}
//			}, args);
//}

std::function<void(Address, Address)> Core::getMemoryInstallationFunction() noexcept {
	return [this](Address location, Address value) { store(location, value); };
}

//std::function<void(Address, Address)> Core::getInstructionInstallationFunction() noexcept {
//	return [this](Address location, Address instruction) {
//		auto width = getInstructionWidth(instruction);
//		if (width == 0) {
//			throw Problem("Core::getInstructionInstallationFunction", "Got an \"instruction\" with zero width!");
//		}
//		Datum conv(instruction);
//		for (auto curr = location, i = Address(0); curr < (location + width); ++curr, ++i) {
//			auto value = conv.getField(byte(i));
//			storeByte(curr, value);
//		}
//	};
//}

//void Core::returnToNative(Operation op, DecodedArguments) {
//	switch (op) {
//		case Operation::LeaveExecutionLoop:
//			store(Core::returnToMicrocode, Address(1));
//			break;
//		default:
//			throw Problem("returnToNative", "Illegal operation provided!");
//	}
//}
//
//void Core::printString(Operation op, DecodedArguments args) {
//	std::visit([this,op](auto&& value) {
//				using T = std::decay_t<decltype(value)>;
//				if constexpr (std::is_same_v<T, OneRegister>) {
//					auto& dest = value.destination.value().get();
//					if (op == Operation::PrintChar) {
//						auto c = char(dest.getAddress());
//						std::cout << c;
//					} else if (op == Operation::TypeDatum) {
//						std::cout << dest.getValue();
//					}
//				} else if constexpr (std::is_same_v<T, TwoRegister>) {
//					if (op == Operation::PrintString) {
//						auto begin = value.destination.value().get().getAddress();
//						auto length = value.destination.value().get().getAddress();
//						auto end = begin + length;
//						for (auto loc = begin; loc < end; ++loc) {
//							std::cout << char(loadByte(loc));
//						}
//					} else {
//						throw Problem("Core::printString", "Unexpected Two Register operation provided!");
//					}
//				} else {
//					throw Problem("Core::printString", "Undefined operations provided!");
//				}
//			}, args);
//}

/*
Core::DecodedInstruction Core::decode() {
    auto b = Operation(extractByteFromMolecule());
    return std::visit([b, this](auto&& type) { return decode(b, type); }, determineInstructionWidth(b));
}



Core::DecodedInstruction Core::decode(Operation op, const OneByte& b) {
    auto v = Core::getVariant(op, b);
    if (!v) {
        throw Problem("Core::decode", "Illegal one byte variant!");
    }
	return Core::DecodedInstruction(decodeOpcode(op), std::visit([](auto&& value) {
					Core::DecodedArguments da;
					using T = std::decay_t<decltype(value)>;
					if constexpr (std::is_same_v<T, Core::IsNoArguments>) {
						da = Core::NoArguments();
					} else {
						static_assert(AlwaysFalse<T>::value, "Unimplemented one byte variant!");
					}
					return da;
				}, v.value()));
}

Core::DecodedInstruction Core::decode(Operation op, const TwoByte& b) {
    auto byte2 = extractByteFromMolecule();
    auto v = Core::getVariant(op, b);
    if (!v) {
        throw Problem("Core::decode", "Illegal two byte variant!");
    }
    return Core::DecodedInstruction(decodeOpcode(op), std::visit([this, byte2](auto&& value) {
                Core::DecodedArguments da;
                using T = std::decay_t<decltype(value)>;
                using K = typename T::Type;
                K r;
                if constexpr (std::is_same_v<T, Core::IsOneRegister>) {
                    r.destination = getDestinationRegister(byte2);
                } else if constexpr (std::is_same_v<T, Core::IsTwoRegister>) {
                    r.destination = getDestinationRegister(byte2);
                    r.source = getSourceRegister(byte2);
                } else {
                    static_assert(AlwaysFalse<T>::value, "Unimplemented two byte variant!");
                }
                da = r;
                return da;
            }, v.value()));
}

Core::DecodedInstruction Core::decode(Operation op, const ThreeByte& b) {
    auto byte2 = extractByteFromMolecule();
    auto byte3 = extractByteFromMolecule();
    auto v = Core::getVariant(op, b);
    if (!v) {
        throw Problem("Core::decode", "Illegal three byte variant!");
    }
    return Core::DecodedInstruction(decodeOpcode(op),
            std::visit([this, byte2, byte3, quarter = setLowerUpperHalves<QuarterAddress>(byte2, byte3)](auto&& value) {
                Core::DecodedArguments da;
                using T = std::decay_t<decltype(value)>;
                using K = typename T::Type;
                K r;
                if constexpr (std::is_same_v<T, Core::IsThreeRegister>) {
                    r.destination = getDestinationRegister(byte2);
                    r.source = getSourceRegister(byte2);
                    r.source2 = getDestinationRegister(byte3);
                } else if constexpr (std::is_same_v<T, Core::IsFourRegister>) {
                    r.destination = getDestinationRegister(byte2);
                    r.source = getSourceRegister(byte2);
                    r.source2 = getDestinationRegister(byte3);
                    r.source3 = getSourceRegister(byte3);
                } else if constexpr (std::is_same_v<T, Core::IsSignedImm16>) {
                    union {
                     QuarterAddress a;
                     QuarterInteger i;
                    } tmp;
                    tmp.a = quarter;
					r.value = tmp.i;
                } else {
                    static_assert(AlwaysFalse<T>::value, "Unimplemented three byte variant");
                }
                da = r;
                return da;
            }, v.value()));
}

Core::DecodedInstruction Core::decode(Operation op, const FourByte& b) {
    auto byte2 = extractByteFromMolecule();
    auto byte3 = extractByteFromMolecule();
    auto byte4 = extractByteFromMolecule();
    auto v = Core::getVariant(op, b);
    if (!v) {
        throw Problem("Core::decode", "Illegal four byte variant");
    }
    return Core::DecodedInstruction(decodeOpcode(op),
            std::visit([this, byte2, byte3, byte4, quarter = setLowerUpperHalves<QuarterAddress>(byte3, byte4)](auto&& value) {
                Core::DecodedArguments da;
                using T = std::decay_t<decltype(value)>;
                using K = typename T::Type;
                K r;
                if constexpr (std::is_same_v<T, Core::IsFiveRegister>) {
                    r.destination = getDestinationRegister(byte2);
                    r.source0 = getSourceRegister(byte2);
                    r.source1 = getDestinationRegister(byte3);
                    r.source2 = getSourceRegister(byte3);
                    r.source3 = getDestinationRegister(byte4);
                } else if constexpr (std::is_same_v<T, Core::IsImm24>) {
                    r.value = setFourQuarters<HalfAddress>(byte2, byte3, byte4, 0);
                } else if constexpr (std::is_same_v<T, Core::IsTwoRegisterWithImm16>) {
                    r.destination = getDestinationRegister(byte2);
                    r.source = getSourceRegister(byte2);
                    r.imm16 = quarter;
                } else if constexpr (std::is_same_v<T, Core::IsOneRegisterWithImm16>) {
                    r.destination = getDestinationRegister(byte2);
                    r.imm16 = quarter;
                } else {
                    static_assert(AlwaysFalse<T>::value, "Unimplemented four byte variant");
                }
                da = r;
                return da;
            }, v.value()));
}

Core::DecodedInstruction Core::decode(Operation op, const EightByte& b) {
    auto byte2 = extractByteFromMolecule();
    auto imm48 = extractImm48();
    auto v = Core::getVariant(op, b);
    if (!v) {
        throw Problem("Core::decode", "Illegal eight byte variant");
    }
    return Core::DecodedInstruction(decodeOpcode(op),
            std::visit([this, byte2, imm48](auto&& value) {
                Core::DecodedArguments da;
                using T = std::decay_t<decltype(value)>;
                using K = typename T::Type;
                K r;
                if constexpr (std::is_same_v<T, Core::IsLoadImm48>) {
                    r.destination = getDestinationRegister(byte2);
                    r.imm48 = imm48;
                } else {
                    static_assert(AlwaysFalse<T>::value, "Unimplemented eight byte variant");
                }
                da = r;
                return da;
            }, v.value()));
}
std::optional<Core::DecodedOpcode> Core::decodeOpcode(Operation op) {
	std::optional<Core::DecodedOpcode> c;
#define XTwoRegisterWithImm16(_) Core::ImmediateType
#define XOneRegisterWithImm16(_) Core::ImmediateType
#define XOneRegister(_) Core::RegisterType
#define XTwoRegister(_) Core::RegisterType
#define XThreeRegister(_) Core::RegisterType
#define XFourRegister(_) Core::RegisterType
#define XFiveRegister(_) Core::RegisterType
#define XCount(title, sz, typ, t2) \
	if (op == Operation:: title) { \
		Core:: t2 v; \
		c = v; \
		return c; \
	}
#define XNumber(title, sz, typ, t2) \
	if (op == Operation:: title) {  \
		Core:: t2 v; \
		v.type = Core::Signed(); \
		v.args = INDIRECTION(X, typ)(title) (); \
		c = v; \
		return c; \
	}
#define XMemoryAddress(title, sz, typ, t2) \
	if (op == Operation:: title) {  \
		Core:: t2 v; \
		v.type = Core::Unsigned(); \
		v.args = INDIRECTION(X, typ)(title) (); \
		c = v; \
		return c; \
	}
#define XFloatingPoint(title, sz, typ, t2) \
	if (op == Operation:: title) {  \
		Core:: t2 v; \
		v.type = Core::FloatingPoint(); \
		v.args = INDIRECTION(X, typ)(title) (); \
		c = v; \
		return c; \
	}
#define XBoolean(title, sz, typ, t2) \
	if (op == Operation:: title) {  \
		Core:: t2 v; \
		v.type = Core::Boolean(); \
		v.args = INDIRECTION(X, typ)(title) (); \
		c = v; \
		return c; \
	}
#define X(title, sz, typ, discriminant) INDIRECTION(X, discriminant)(title, sz, typ); 
#include "InstructionData.def"
#undef X
#undef XCount
#undef XNumber
#undef XMemoryAddress
#undef XBoolean
#undef XFloatingPoint
	return c;
}
*/
std::optional<Core::DecodedOperation> Core::decodeInstruction(byte control, OneByteInstruction) {
    // code 0: OneByte [ variant:3 | op: 5 ] // maximum of 32 ops in this class
    Core::OneByteOperation op;
    switch (decodeBits<byte, OneByteOpcode, 0b11111000, 3>(control)) {
#define OneByte(title) case OneByteOpcode:: title : op = Core:: title () ; break;
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b) 
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b) 
#define ExtendedVariantSixByte(title, b)
#define ExtendedVariant(st, b, c) INDIRECTION(ExtendedVariant, st)(b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
        default:
            break;
    }
    return std::optional<Core::DecodedOperation>(op);
}

std::optional<Core::DecodedOperation> Core::decodeInstruction(byte control, TwoByteInstruction) {
    // code 1: TwoByte [ variant:3 | control: 13 ] // multiple layouts
    //          OneRegister [ variant:3 | op: 5 | dest: 4 | unused: 4 ]
    //          TwoRegister [ variant:3 | op: 5 | dest: 4 | src: 4 ] // not as many allowed here
   	Core::TwoByteOperation op ;
    switch (decodeBits<byte, TwoByteOpcode, 0b11111000, 3>(control)) {
#define OneByte(title) 
#define TwoByte(title, b) \
		case TwoByteOpcode :: title : \
		op = Core:: title () ; \
		decodeArguments(control, std::get< Core:: title > (op).args); \
		break;
#define ThreeByte(title, b) 
#define FourByte(title, b) 
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b) 
#define ExtendedVariantSixByte(title, b)
#define ExtendedVariant(st, b, c) INDIRECTION(ExtendedVariant, st)(b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
        default:
            break;
    }
    return std::optional<Core::DecodedOperation>(op);
}

std::optional<Core::DecodedOperation> Core::decodeInstruction(byte control, ThreeByteInstruction) {
	// code 2: ThreeByte [ variant:3 | opcontrol:21 ]
	//          ThreeRegister [ variant:3 [2] | op: 6 | unused: 3 | dest: 4 | src0: 4 | src1: 4] 
	Core::ThreeByteOperation op;
	auto nextByte = extractByteFromMolecule();
	auto opcodeValue = ThreeByteOpcode(((nextByte & 0x1) << 5) | decodeBits<byte, byte, 0b11111000, 3>(control));
	switch(opcodeValue) {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title , b) \
		case ThreeByteOpcode:: title : \
		op = Core:: title () ; \
		decodeArguments(nextByte, std::get< Core:: title > (op).args); \
		break; 
#define FourByte(title, b) 
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b) 
#define ExtendedVariantSixByte(title, b)
#define ExtendedVariant(st, b, c) INDIRECTION(ExtendedVariant, st)(b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
		default:
			break;
	}
	return std::optional<Core::DecodedOperation>(op);
}

std::optional<Core::DecodedOperation> Core::decodeInstruction(byte control, FourByteInstruction) {
	// code 3: FourByte [ variant:3 | op: 5 | control: 24 ]
	//          SignedImm16          [ variant:3 [3] | op: 5 | unused: 8 | imm16 ] 
	//          FourRegister         [ variant:3 [3] | op: 5 | dest: 4 | src0: 4 | src1: 4 | src2: 4 | unused: 8 ] // compact version
	//          FiveRegister         [ variant:3 [3] | op: 5 | dest: 4 | src0: 4 | src1: 4 | src2: 4 | src3: 4 | unused: 4 ]
	//          OneRegisterWithImm16 [ variant:3 [3] | op: 5 | dest: 4 | unused: 4 | imm16 ]
	Core::FourByteOperation op;
	switch (decodeBits<byte, FourByteOpcode, 0b11111000, 3>(control)) {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title , b) \
		case FourByteOpcode:: title : \
		op = Core:: title () ; \
		decodeArguments(control, std::get< Core:: title > (op).args); \
		break; 
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
		default:
			break;
	}
	return std::optional<Core::DecodedOperation>(op);
}
std::optional<Core::DecodedOperation> Core::decodeInstruction(byte control, EightByteInstruction) {
	// code 4: EightByte [ variant:3 | op:5 | control: 56 ]
	//         OneRegisterWithImm48  [ variant: 3 [4] | op: 5 | dest: 4 | unused: 4 | imm48 ]
	Core::EightByteOperation op;
	switch (decodeBits<byte, EightByteOpcode, 0b11111000, 3>(control)) {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title , b) 
#define FiveByte(title, b) 
#define EightByte(title, b) \
		case EightByteOpcode:: title : \
		op = Core:: title () ; \
		decodeArguments(control, std::get< Core:: title > (op).args); \
		break; 
#define GrabBag(title, b) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
		default:
			break;
	}
	return std::optional<Core::DecodedOperation>(op);
}
std::optional<Core::DecodedOperation> Core::decodeInstruction(byte control, FiveByteInstruction) {
// code 5: FiveByte [ variant:3 | op: 5 | control: 32 ]
//          Immediate24          [ variant:3 [3] | op: 5 | unused: 8 | imm24] 
//          TwoRegisterWithImm16 [ variant:3 [3] | op: 5 | dest:4 | src0:4 | unused: 8 | imm16 ]
	Core::FiveByteOperation op;
	switch (decodeBits<byte, FiveByteOpcode, 0b11111000, 3>(control)) {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title , b) 
#define FiveByte(title, b) \
		case FiveByteOpcode:: title : \
		op = Core:: title () ; \
		decodeArguments(control, std::get< Core:: title > (op).args); \
		break; 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
		default:
			break;
	}
	return std::optional<Core::DecodedOperation>(op);
}

std::optional<Core::DecodedOperation> Core::decodeInstruction(byte control, GrabBagInstruction) {
	// code 6: GrabBag [ variant:3 | op: 5 | ... ] 
	//         TwoRegister          [ variant: 3 [6] | op: 5 | dest: 4 | src: 4 ]
	//         OneRegisterWithImm64 [ variant: 3 [6] | op: 5 | dest: 4 | unused: 4 | imm64 ]
	//         OneRegisterWithImm32 [ variant: 3 [6] | op: 5 | dest: 4 | unused: 4 | imm32 ]
	Core::GrabBagOperation op;
	switch (decodeBits<byte, GrabBagOpcode, 0b11111000, 3>(control)) {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title , b) 
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) \
		case GrabBagOpcode :: title : \
		op = Core:: title () ; \
		decodeArguments(control, std::get< Core:: title > (op).args); \
		break; 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag

		default:
			break;
	}


	return std::optional<Core::DecodedOperation>(op);
}

std::optional<Core::DecodedOperation> Core::decodeInstruction(byte control) {
    switch(decodeVariant(control)) {
        case VariantKind::OneByte: 
            return decodeInstruction(control, OneByteInstruction()); 
        case VariantKind::TwoByte: 
            return decodeInstruction(control, TwoByteInstruction()); 
        case VariantKind::ThreeByte: 
            return decodeInstruction(control, ThreeByteInstruction()); 
        case VariantKind::FourByte: 
            return decodeInstruction(control, FourByteInstruction()); 
        case VariantKind::FiveByte: 
            return decodeInstruction(control, FiveByteInstruction()); 
        case VariantKind::EightByte:
            return decodeInstruction(control, EightByteInstruction()); 
        case VariantKind::GrabBag:
            return decodeInstruction(control, GrabBagInstruction());
        default:
            return std::optional<Core::DecodedOperation>();
    }
}

void Core::dispatchOperation(const Core::OneByteOperation& op) {
	std::visit([this](auto&& value) {
				using T = std::decay_t<decltype(value)>;
				if constexpr (std::is_same_v<T, Core::Nop>) {
					// do nothing
				} else if constexpr (std::is_same_v<T, Core::LeaveExecutionLoop>) {
					store(Core::returnToMicrocode, Address(1));
				} else if constexpr (std::is_same_v<T, Core::ReturnSubroutine>) {
					_pc.setValue(pop(TargetRegister::SP2));
				} else if constexpr (std::is_same_v<T, UndefinedOpcode>) {
					throw Problem("dispatchOperation(OneByte)", "Undefined opcode provided to the one byte operation!");
				} else {
					static_assert(AlwaysFalse<T>::value, "Unimplemented one byte operation!");
				}
			}, op);
}
Register& Core::getDestinationRegister(const Core::DestinationRegister& reg) {
	if (reg) {
		return getRegister(reg.value());
	} else {
		throw Problem("getDestinationRegister", "Destination register is unpopulated!");
	}
}

const Register& Core::getSourceRegister(const Core::SourceRegister& reg) {
	if (reg) {
		return getRegister(reg.value());
	} else {
		throw Problem("getSourceRegister", "Source register is unpopulated!");
	}
}

void Core::dispatchOperation(const Core::TwoByteOperation& op) {
	std::visit([this](auto&& value) {
				using T = std::decay_t<decltype(value)>;
				auto flags = std::cout.flags();
				auto& dest = getDestinationRegister(value.args);
				if constexpr (std::is_same_v<T,Core::TypeValue>) {
					std::cout << std::dec << dest.getInt();
				} else if constexpr (std::is_same_v<T,Core::TypeValueUnsigned>) {
					std::cout << std::hex << dest.getAddress() << "#";
				} else if constexpr (std::is_same_v<T,Core::TypeValueBoolean>) {
					std::cout << std::boolalpha << dest.getTruth() << std::noboolalpha;
				} else if constexpr (std::is_same_v<T,Core::TypeValueFloatingPoint>) {
					std::cout << dest.getFP();
				} else if constexpr (std::is_same_v<T,Core::PrintChar>) {
					auto c = char(dest.getAddress());
					std::cout << c;
				} else if constexpr (std::is_same_v<T,Core::TypeDatum>) {
						std::cout << dest.getValue();
				} else if constexpr (std::is_same_v<T, Core::ConditionalReturnSubroutine>) {
					if (dest.getTruth()) {
						dispatchOperation(Core::ReturnSubroutine());
					}
				} else if constexpr (std::is_same_v<T, Core::CallSubroutineIndirect>) {
					
				} else if constexpr (std::is_same_v<T, UndefinedOpcode>) {
					throw Problem("dispatchOperation(TwoByte)", "undefined two byte operation");
				} else {
					//static_assert(AlwaysFalse<T>::value, "Unimplemented two byte operation");
					throw Problem("dispatchOperation(TwoByte)", "Unimplemented two byte operation");
				}
				std::cout.setf(flags);
			}, op);
}

} // namespace forth
