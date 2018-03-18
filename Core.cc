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
        case Type::Temporary2:
            return _tmp1;
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



constexpr HalfAddress makeImm24(QuarterAddress lower16, byte upper8) noexcept {
	return encodeBits<HalfAddress, QuarterAddress, 0x00FF0000, 16>(HalfAddress(lower16), static_cast<QuarterAddress>(upper8));
}
void Core::savePositionToSubroutineStack() {
    push(_pc.getValue(), TargetRegister::SP2);
}

Address Core::extractImm48() {
    auto lowest16 = Address(extractQuarterAddressFromMolecule());
    auto lower16 = Address(extractQuarterAddressFromMolecule()) << 16;
    auto higher16 = Address(extractQuarterAddressFromMolecule()) << 32;
    return lowest16 | lower16 | higher16;
}



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

std::optional<Core::DecodedOperation> Core::decodeInstruction(byte control) {
	std::optional<Core::DecodedOperation> op;
	switch (Opcode(control)) {
#define PerformDecode(title) decodeArguments(std::get< Core:: title > (op.value()) .args )
#define XOneRegister(title) PerformDecode(title)
#define XTaggedOneRegister(title) PerformDecode(title)
#define XTwoRegister(title) PerformDecode(title)
#define XThreeRegister(title) PerformDecode(title)
#define XTaggedThreeRegister(title) PerformDecode(title)
#define XSignedImm16(title) PerformDecode(title)
#define XImmediate24(title) PerformDecode(title)
#define XTwoRegisterWithImm16(title) PerformDecode(title) 
#define XCustomTwoRegisterWithImm16(title) PerformDecode(title) 
#define XOneRegisterWithImm16(title) PerformDecode(title) 
#define XOneRegisterWithImm64(title) PerformDecode(title)
#define XOneRegisterWithImm48(title) PerformDecode(title)
#define XOneRegisterWithImm32(title) PerformDecode(title)
#define XNoArguments(title) 
#define X(title, b) \
		case Opcode:: title : \
			op = Core:: title () ; \
			INDIRECTION(X, b)(title) ; \
			break;
#define FirstX(title, b) X(title, b)
#include "InstructionData.def"
#undef X
#undef FirstX
#undef XCustomTwoRegisterWithImm16
#undef XTaggedOneRegister
#undef XTaggedThreeRegister
#undef XNoArguments
#undef XOneRegister
#undef XTwoRegister
#undef XThreeRegister
#undef XSignedImm16
#undef XImmediate24
#undef XTwoRegisterWithImm16
#undef XOneRegisterWithImm16
#undef XOneRegisterWithImm64
#undef XOneRegisterWithImm48
#undef XOneRegisterWithImm32
		default:
			break;

	}
	return op;
}

Register& Core::getDestinationRegister(const Core::DestinationRegister& reg) {
	if (reg) {
		return getRegister(reg.value());
	} else {
		throw Problem("getDestinationRegister", "Destination register is unpopulated!");
	}
}

Register& Core::getSourceRegister(const Core::SourceRegister& reg) {
	if (reg) {
		return getRegister(reg.value());
	} else {
		throw Problem("getSourceRegister", "Source register is unpopulated!");
	}
}

auto add(auto a, auto b) noexcept -> decltype(a + b) { return a + b; }
auto subtract(auto a, auto b) noexcept -> decltype(a - b) { return a - b; }
auto multiply(auto a, auto b) noexcept -> decltype(a * b) { return a * b; }
auto divide(auto a, auto b) -> decltype(a / b) { 
	if constexpr (!std::is_floating_point_v<decltype(a)>) {
		if (b == 0) {
			throw Problem("divide", "Dividing by zero!");
		}
	}
	return a / b;
}

auto modulo(auto a, auto b) -> decltype(a / b) { 
	if (b == 0) {
		throw Problem("modulo", "Dividing by zero!");
	}
	return a % b;
}
auto andOp(auto a, auto b) noexcept -> decltype(a & b) { return a & b; }
bool andOp(bool a, bool b) noexcept { return a && b; }
auto orOp(auto a, auto b) noexcept -> decltype(a | b) { return a | b; }
bool orOp(bool a, bool b) noexcept { return a || b; }
auto xorOp(auto a, auto b) noexcept -> decltype(a ^ b) { return a ^ b; }
bool xorOp(bool a, bool b) noexcept { return a != b; }
auto greaterThan(auto a, auto b) noexcept -> decltype(a > b) { return a > b; }
auto lessThan(auto a, auto b) noexcept -> decltype(a < b) { return a < b; }
auto shiftRight(auto a, auto b) noexcept -> decltype(a >> b) { return a >> b; }
auto shiftLeft(auto a, auto b) noexcept -> decltype(a << b) { return a << b; }
auto equals(auto a, auto b) noexcept -> decltype(a == b) { return a == b; }
auto powFunc(auto a, auto b) noexcept -> decltype(a) { return decltype(a)(std::pow(double(a), double(b))); }
auto minus(auto a) noexcept -> decltype(-a) { return -a; }
auto notOp(auto a) noexcept -> decltype(~a) { return ~a; }
bool notOp(bool a) noexcept { return !a; }

void Core::typeValue(const TypeValue& value) {
    auto flags = std::cout.flags();
    switch (value.args.type) {
        case Core::TypeTag::Integer:
            std::cout << std::dec << getDestinationRegister(value.args).getInt();
            break;
        case Core::TypeTag::Unsigned:
            std::cout << std::hex << getDestinationRegister(value.args).getAddress() << "#";
            break;
        case Core::TypeTag::FloatingPoint:
            std::cout << getDestinationRegister(value.args).getFP();
            break;
        case Core::TypeTag::Boolean:
            std::cout << std::boolalpha << getDestinationRegister(value.args).getTruth() << std::noboolalpha;
            break;
        case Core::TypeTag::Char:
            std::cout << char(getDestinationRegister(value.args).getAddress());
            break;
        case Core::TypeTag::Datum:
            std::cout << getDestinationRegister(value.args).getValue();
            break;
        default:
            throw Problem("typeValue", "Illegal type specified!");
    }
    std::cout.setf(flags);
}
void Core::dispatchInstruction() {
	auto control = extractByteFromMolecule();
	auto result = decodeInstruction(control);
	if (result) {
		std::visit([this](auto&& value) { 
				auto intFunction = [](const Register& value) { return value.getInt(); };
				auto floatFunction = [](const Register& value) { return value.getFP(); };
				auto unsignedFunction = [](const Register& value) { return value.getAddress(); };
				auto booleanFunction = [](const Register& value) { return value.getTruth(); };
				using T = std::decay_t<decltype(value)>;
				if constexpr (std::is_same_v<T, Core::Nop>) {
					// do nothing
				} else if constexpr (std::is_same_v<T, Core::LeaveExecutionLoop>) {
					store(Core::returnToMicrocode, Address(1));
				} else if constexpr (std::is_same_v<T, Core::ReturnSubroutine>) {
					_pc.setValue(pop(TargetRegister::SP2));
				} 
					else if constexpr (std::is_same_v<T,Core::TypeValue>) {
                        typeValue(value);
					} else if constexpr (std::is_same_v<T, Core::ConditionalReturnSubroutine>) {
						if (getDestinationRegister(value.args).getTruth()) {
							_pc.setValue(pop(TargetRegister::SP2));
						}
					} else if constexpr (std::is_same_v<T, Core::CallSubroutineIndirect>) {
			    	    savePositionToSubroutineStack();
						_pc.setValue(getDestinationRegister(value.args).getValue());
					} else if constexpr (std::is_same_v<T, Core::JumpIndirect>) {
						_pc.setValue(getDestinationRegister(value.args).getValue());
					} else if constexpr (std::is_same_v<T, Core::ConditionalBranchIndirect>) {
						if (getDestinationRegister(value.args).getTruth()) {
							_pc.setValue(getSourceRegister(value.args).getValue());
						}
					} else if constexpr (std::is_same_v<T, Core::ConditionalCallSubroutineIndirect>) {
						if (getDestinationRegister(value.args).getTruth()) {
							savePositionToSubroutineStack();
							_pc.setValue(getDestinationRegister(value.args).getValue());
						}
					} else if constexpr (std::is_same_v<T, Core::Load>) {
						getDestinationRegister(value.args).setValue(loadWord(getSourceRegister(value.args).getAddress()));
					} else if constexpr (std::is_same_v<T, Core::Store>) {
						store(getDestinationRegister(value.args).getAddress(), getSourceRegister(value.args).getValue());
					} else if constexpr (std::is_same_v<T, Core::Move>) {
						getDestinationRegister(value.args).setValue(getSourceRegister(value.args).getValue());
					} else if constexpr (std::is_same_v<T, Core::Swap>) {
						auto temp = getDestinationRegister(value.args).getValue();
						getDestinationRegister(value.args).setValue(getSourceRegister(value.args).getValue());
						getSourceRegister(value.args).setValue(temp);
					} else if constexpr (std::is_same_v<T, Core::PopRegister>) {
						// pop dest, sp
						auto& stackPointer = getSourceRegister(value.args);
						getDestinationRegister(value.args).setValue(loadWord(stackPointer.getAddress()));
						stackPointer.increment(sizeof(Address));
					} else if constexpr (std::is_same_v<T, Core::PushRegister>) {
						// push sp, src
						auto& stackPointer = getDestinationRegister(value.args);
						stackPointer.decrement(sizeof(Address));
						store(stackPointer.getAddress(), getSourceRegister(value.args).getValue());
					} 
#define IsType(t) std::is_same_v<T, t>
#define InvokeConv(bfun, cfun) \
	getDestinationRegister(value.args).setValue( bfun ( cfun(getSourceRegister(value.args)) , cfun(getSource2Register(value.args)) ))
#define InvokeS(t, func) \
					else if constexpr (IsType( t )) { InvokeConv(func, intFunction); }
#define InvokeU(t, func) \
					else if constexpr (IsType( t ## Unsigned )) { InvokeConv(func, unsignedFunction); }
#define InvokeF(t, func) \
					else if constexpr (IsType( FloatingPoint ## t )) { InvokeConv(func, floatFunction); }
#define InvokeB(t, func) \
					else if constexpr (IsType( t ## Boolean)) { InvokeConv(func, booleanFunction); }
#define InvokeSU(t, func) \
					InvokeS(t, func) \
					InvokeU(t, func)
#define InvokeSUF(t, func) \
					InvokeSU(t, func) \
					InvokeF(t, func)
#define InvokeSUB(t, func) \
					InvokeSU(t, func) \
					InvokeB(t, func)

					InvokeSUF(Add, add)
					InvokeSUF(Subtract, subtract)
					InvokeSUF(Multiply, multiply)
					InvokeSUF(Divide, divide)
					InvokeSU(Modulo, modulo)
					InvokeSUB(And, andOp)
					InvokeSUB(Or, orOp)
					InvokeSUB(Xor, xorOp)
					InvokeSUF(GreaterThan, greaterThan)
					InvokeSUF(LessThan, lessThan)
#undef InvokeS
#undef InvokeU
#undef InvokeF
#undef InvokeB
#undef InvokeSU
#undef InvokeSUF
#undef InvokeSUB
#undef InvokeConv
#undef IsType
#define IsType(t) std::is_same_v<T, t>
#define InvokeS(t, func) \
				else if constexpr (IsType( t ## Immediate )) { \
					auto& dest = getDestinationRegister(value.args); \
					auto& src = getSourceRegister(value.args); \
					dest.setValue ( func ( src.getInt() , Integer(value.args.imm16) )); \
				}
#define InvokeU(t, func) \
				else if constexpr (IsType( Unsigned ## t ## Immediate )) { \
					auto& dest = getDestinationRegister(value.args); \
					auto& src = getSourceRegister(value.args); \
					dest.setValue ( func ( src.getAddress() , Address(value.args.imm16) )); \
				}
#define InvokeSU(t, func) \
				InvokeS(t, func) \
				InvokeU(t, func)
				 else if constexpr (std::is_same_v<T, Jump>) {
					_pc.setValue(_pc.getInt() + value.args.value);
				} else if constexpr (std::is_same_v<T, CallSubroutine>) {
					savePositionToSubroutineStack();
					_pc.setValue(_pc.getInt() + value.args.value);
				} else if constexpr (std::is_same_v<T, JumpAbsolute>) {
					_pc.setValue(Address(value.args.getImm24()));
				} else if constexpr (std::is_same_v<T, CallSubroutineAbsolute>) {
					savePositionToSubroutineStack();
					_pc.setValue(Address(value.args.getImm24()));
				} else if constexpr (std::is_same_v<T, JumpAbsolute>) {
                    _pc.setValue(value.args.value);
				} else if constexpr (std::is_same_v<T, ConditionalBranch>) {
                    if (getDestinationRegister(value.args).getTruth()) {
                        _pc.setValue(_pc.getInt() + safeExtract(value.args.imm16));
                    }
				} else if constexpr (std::is_same_v<T, ConditionalCallSubroutine>) { 
                    if (getDestinationRegister(value.args).getTruth()) {
                        savePositionToSubroutineStack();
                        _pc.setValue(_pc.getInt() + safeExtract(value.args.imm16));
                    }
				}
				InvokeSU(Add, add)
				InvokeSU(Subtract, subtract)
				InvokeSU(Multiply, multiply)
				InvokeSU(Divide, divide)
				InvokeSU(Modulo, modulo)
				InvokeSU(And, andOp)
				InvokeSU(Or, orOp)
				InvokeSU(Xor, xorOp)
				InvokeSU(ShiftLeft, shiftLeft)
				InvokeSU(ShiftRight, shiftRight)
				InvokeSU(GreaterThan, greaterThan)
				InvokeSU(LessThan, lessThan)
				InvokeSU(Equals, equals)
#undef IsType
#undef InvokeS
#undef InvokeU
#undef InvokeSU
#define IsType(t) std::is_same_v<T, t>
#define InvokeConv(bfun, cfun) \
	getDestinationRegister(value.args).setValue( bfun ( cfun(getSourceRegister(value.args)) , cfun(getSourceRegister(value.args.source2)) ))
#define InvokeS(t, func) \
					else if constexpr (IsType( t )) { InvokeConv(func, intFunction); }
#define InvokeU(t, func) \
					else if constexpr (IsType( t ## Unsigned )) { InvokeConv(func, unsignedFunction); }
#define InvokeF(t, func) \
					else if constexpr (IsType( FloatingPoint ## t )) { InvokeConv(func, floatFunction); }
#define InvokeB(t, func) \
					else if constexpr (IsType( t ## Boolean)) { InvokeConv(func, booleanFunction); }
#define InvokeSU(t, func) \
					InvokeS(t, func) \
					InvokeU(t, func)
#define InvokeSUF(t, func) \
					InvokeSU(t, func) \
					InvokeF(t, func)
#define InvokeSUB(t, func) \
					InvokeSU(t, func) \
					InvokeB(t, func)
					InvokeSU(ShiftRight, shiftRight)
					InvokeSU(ShiftLeft, shiftLeft)
					InvokeSUB(Equals, equals)
					InvokeF(Equals, equals)
					InvokeSUF(Pow, powFunc)
#undef InvokeS
#undef InvokeU
#undef InvokeF
#undef InvokeB
#undef InvokeSU
#undef InvokeSUF
#undef InvokeSUB
#undef InvokeConv
#undef IsType
				else if constexpr (std::is_same_v<T, DecodeBits>) {
					auto& dest = getDestinationRegister(value.args);
					auto& src = getSourceRegister(value.args);
					auto& src2 = getSource2Register(value.args);
					auto& src3 = getSourceRegister(value.args.source3);
					dest.setValue(forth::decodeBits<Address, Address> (src.getAddress(), src2.getAddress(), src3.getAddress()));
				} else if constexpr (std::is_same_v<T, EncodeBits>) {
					auto& dest = getDestinationRegister(value.args);
					auto& src = getSourceRegister(value.args);
					auto& src2 = getSource2Register(value.args);
					auto& src3 = getSourceRegister(value.args.source3);
					auto& src4 = getSourceRegister(value.args.source4);
					dest.setValue(forth::encodeBits<Address, Address>(src.getAddress(), src2.getAddress(), src3.getAddress(), src4.getAddress()));
				} else if constexpr (std::is_same_v<T, PrintString>) {
						auto begin = getDestinationRegister(value.args).getAddress();
						auto length = getSourceRegister(value.args).getAddress();
						auto end = begin + length;
						for (auto loc = begin; loc < end; ++loc) {
							std::cout << char(loadByte(loc));
						}
				} else if constexpr (std::is_same_v<T, Minus>) {
					getDestinationRegister(value.args).setValue(minus(getSourceRegister(value.args).getInt()));
				} else if constexpr (std::is_same_v<T, MinusUnsigned>) {
					getDestinationRegister(value.args).setValue(minus(getSourceRegister(value.args).getAddress()));
				} else if constexpr (std::is_same_v<T, MinusFloatingPoint>) {
					getDestinationRegister(value.args).setValue(minus(getSourceRegister(value.args).getFP()));
				} else if constexpr (std::is_same_v<T, Not>) {
					getDestinationRegister(value.args).setValue(notOp(getSourceRegister(value.args).getAddress()));
				} else if constexpr (std::is_same_v<T, NotBoolean>) {
					getDestinationRegister(value.args).setValue(notOp(getSourceRegister(value.args).getTruth()));
				} else if constexpr (std::is_same_v<T, NotSigned>) {
					getDestinationRegister(value.args).setValue(notOp(getSourceRegister(value.args).getInt()));
				} else if constexpr (std::is_same_v<T, LoadImmediate32>) {
					getDestinationRegister(value.args).setValue(Address(value.args.imm32));
				} else if constexpr (std::is_same_v<T, LoadImmediate64>) {
					getDestinationRegister(value.args).setValue(Address(value.args.imm64));
                } else if constexpr (std::is_same_v<T, IfStatement>) {
                    _pc.setValue(getDestinationRegister(value.args).getTruth() ?
                            getSourceRegister(value.args).getAddress() : 
                            getSource2Register(value.args).getAddress());
                } else if constexpr (std::is_same_v<T, CallIfStatement>) {
                    savePositionToSubroutineStack();
                    _pc.setValue(getDestinationRegister(value.args).getTruth() ?
                            getSourceRegister(value.args).getAddress() : 
                            getSource2Register(value.args).getAddress());
				} else if constexpr (std::is_same_v<T, GreaterThanOrEqualTo>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getInt() >=
							getSource2Register(value.args).getInt());
				} else if constexpr (std::is_same_v<T, GreaterThanOrEqualToUnsigned>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getAddress() >=
							getSource2Register(value.args).getAddress());
				} else if constexpr (std::is_same_v<T, FloatingPointGreaterThanOrEqualTo>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getFP() >=
							getSource2Register(value.args).getFP());
				} else if constexpr (std::is_same_v<T, LessThanOrEqualTo>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getInt() <=
							getSource2Register(value.args).getInt());
				} else if constexpr (std::is_same_v<T, LessThanOrEqualToUnsigned>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getAddress() <=
							getSource2Register(value.args).getAddress());
				} else if constexpr (std::is_same_v<T, FloatingPointLessThanOrEqualTo>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getFP() <=
							getSource2Register(value.args).getFP());
				} else if constexpr (std::is_same_v<T, NotEqual>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getInt() !=
							getSource2Register(value.args).getInt());
				} else if constexpr (std::is_same_v<T, NotEqualUnsigned>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getAddress() !=
							getSource2Register(value.args).getAddress());
				} else if constexpr (std::is_same_v<T, FloatingPointNotEqual>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getFP() !=
							getSource2Register(value.args).getFP());
				} else if constexpr (std::is_same_v<T, NotEqualBoolean>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getTruth() !=
							getSource2Register(value.args).getTruth());
				} else if constexpr (std::is_same_v<T, NotEqualImmediate>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getInt() !=
							Integer(value.args.imm16));
				} else if constexpr (std::is_same_v<T, UnsignedNotEqualImmediate>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getAddress() !=
							Address(value.args.imm16));
				} else if constexpr (std::is_same_v<T, GreaterThanOrEqualToImmediate>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getInt() >=
							Integer(value.args.imm16));
				} else if constexpr (std::is_same_v<T, UnsignedGreaterThanOrEqualToImmediate>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getAddress() >=
							Address(value.args.imm16));
				} else if constexpr (std::is_same_v<T, LessThanOrEqualToImmediate>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getInt() <=
							Integer(value.args.imm16));
				} else if constexpr (std::is_same_v<T, UnsignedLessThanOrEqualToImmediate>) {
					getDestinationRegister(value.args).setValue(
							getSourceRegister(value.args).getAddress() <=
							Address(value.args.imm16));
				} else {
					static_assert(AlwaysFalse<T>::value, "Unimplemented instruction!");
				}
				}, result.value());
	} else {
		throw Problem("dispatchInstruction", "Bad Instruction!");
	}
}
void Core::decodeArguments(OneRegister& args) {
    populateDestination(args);
}

void Core::decodeArguments(TwoRegister& args) {
    populateDestinationAndSource(args);
}

void Core::decodeArguments(ThreeRegister& args) {
    populateDestinationAndSource(args);
	auto b2 = extractByteFromMolecule();
	args.source2 = forth::getDestinationRegister(b2);
}


void Core::decodeArguments(SignedImm16& args) {
	args.value = extractQuarterIntegerFromMolecule();
}

void Core::decodeArguments(Immediate24& args) {
	auto lower16 = HalfAddress(extractQuarterAddressFromMolecule());
	auto upper8 = HalfAddress(extractByteFromMolecule() << 16); 
    args.setImm24((upper8 | lower16));
}

void Core::decodeArguments(OneRegisterWithImm16& args) {
    populateDestination(args);
	args.imm16 = extractQuarterAddressFromMolecule();
}

void Core::decodeArguments(TwoRegisterWithImm16& args) {
    populateDestinationAndSource(args);
	args.imm16 = extractQuarterAddressFromMolecule();
}

void Core::decodeArguments(OneRegisterWithImm32& args) {
    populateDestination(args);
    auto lowerHalf = extractQuarterAddressFromMolecule();
    auto upperHalf = extractQuarterAddressFromMolecule();
    args.imm32 = forth::setLowerUpperHalves<decltype(args.imm32)>(lowerHalf, upperHalf);
}

void Core::decodeArguments(OneRegisterWithImm64& args) {
    populateDestination(args);
    auto lower48 = extractImm48();
    auto upper16 = Address(extractQuarterAddressFromMolecule()) << 48;
    args.imm64 = lower48 | upper16;
}

void Core::encodeArguments(QuarterAddress value) {
    storeAndAdvance(_pc, value);
}
void Core::storeAndAdvance(Register& reg, QuarterAddress value) {
    auto addr = reg.getAddress();
    storeByte(addr, forth::getLowerHalf(value));
    storeByte(addr+1, forth::getUpperHalf(value));
    reg.increment(sizeof(QuarterAddress));
}

void Core::encodeArguments(HalfAddress value) {
    storeAndAdvance(_pc, value);
}
void Core::storeAndAdvance(Register& reg, HalfAddress value) {
    auto addr = reg.getAddress();
    storeByte(addr + 0, forth::getLowerHalf(forth::getLowerHalf(value)));
    storeByte(addr + 1, forth::getUpperHalf(forth::getLowerHalf(value)));
    storeByte(addr + 2, forth::getLowerHalf(forth::getUpperHalf(value)));
    storeByte(addr + 3, forth::getUpperHalf(forth::getUpperHalf(value)));
    reg.increment(sizeof(HalfAddress));
}
void Core::storeAndAdvance(Register& reg, Address value) {
    store(reg.getAddress(), value);
    reg.increment(sizeof(Address));
}
void Core::encodeArguments(const DestinationRegister& dest) {
    storeAndAdvance(_pc, encodeRegisterPair(dest, TargetRegister::Zero));
}
void Core::encodeArguments(const DestinationRegister& dest, const SourceRegister& src) {
    storeAndAdvance(_pc, encodeRegisterPair(dest, src));
}

void Core::encodeArguments(const OneRegister& args) { 
    encodeArguments(args.destination);
}
void Core::encodeArguments(const TwoRegister& args) { 
    encodeArguments(args.destination, args.source);
}
void Core::encodeArguments(const ThreeRegister& args) { 
    encodeArguments(args.destination, args.source);
    encodeArguments(args.source2);
}
void Core::encodeArguments(const SignedImm16& args) { 
    storeAndAdvance(_pc, forth::getLowerHalf(args.value));
    storeAndAdvance(_pc, forth::getUpperHalf(args.value));
}
void Core::encodeArguments(const Immediate24& args) { 
    storeAndAdvance(_pc, forth::getLowerHalf(forth::getLowerHalf(args.getImm24())));
    storeAndAdvance(_pc, forth::getUpperHalf(forth::getLowerHalf(args.getImm24())));
    storeAndAdvance(_pc, forth::getLowerHalf(forth::getUpperHalf(args.getImm24())));
}
void Core::encodeArguments(const OneRegisterWithImm16& args) { 
    encodeArguments(args.destination);
    encodeArguments(args.imm16);
}
void Core::encodeArguments(const TwoRegisterWithImm16& args) { 
    encodeArguments(args.destination, args.source);
    encodeArguments(args.imm16);
}
void Core::encodeArguments(const OneRegisterWithImm32& args) { 
    encodeArguments(args.destination);
    encodeArguments(args.imm32);
}
void Core::encodeArguments(const OneRegisterWithImm64& args) 
{ 
    encodeArguments(args.destination);
    encodeArguments(getLowerHalf(args.imm64));
    encodeArguments(getUpperHalf(args.imm64));
}

void Core::storeAndAdvance(Register& reg, byte value) {
    storeByte(reg.getAddress(), value);
    reg.increment();
}

void Core::encodeInstruction(const Core::DecodedOperation& op) {
	std::visit([this](auto&& value) {
				using T = std::decay_t<decltype(value)>;
				storeAndAdvance(_pc, byte(value.getOpcode()));
				encodeArguments(value.args);
			}, op);
}
void Core::encodeArguments(const NoArguments&) { }

void Core::Immediate24::setImm24(HalfAddress value) {
    imm24 = make24bit(value);
}

void Core::installIntoMemory(Address addr, Core::DataEntry entry) {
	std::visit([this, addr](auto&& value) { encodeAndInstallIntoMemory(addr, value); }, entry);
}

void Core::encodeAndInstallIntoMemory(Address addr, byte value) {
	storeByte(addr, value);
}

void Core::encodeAndInstallIntoMemory(Address addr, Address a) {
	store(addr, a);
}

void Core::encodeAndInstallIntoMemory(Address addr, std::shared_ptr<Address> a) {
	encodeAndInstallIntoMemory(addr, *a);
}

void Core::encodeAndInstallIntoMemory(Address addr, QuarterAddress a) {
	storeByte(addr, getLowerHalf(a));
	storeByte(addr + 1, getUpperHalf(a));
}
void Core::encodeAndInstallIntoMemory(Address addr, HalfAddress a) {
	encodeAndInstallIntoMemory(addr, getLowerHalf(a));
	encodeAndInstallIntoMemory(addr + 2, getUpperHalf(a));
}
void Core::encodeAndInstallIntoMemory(Address addr, std::shared_ptr<DecodedOperation> a) {
	encodeAndInstallIntoMemory(addr, *a);
}
void Core::encodeAndInstallIntoMemory(Address addr, DecodedOperation a) {
	auto originalLoc = _pc.getAddress();
	_pc.setValue(addr);
	encodeInstruction(a);
	_pc.setValue(originalLoc);
}

void Core::encodeArguments(const Core::TaggedOneRegister& args) {
    // a bit of a hack but it will work well :D
    encodeArguments(args.destination, TargetRegister(byte(args.type) & 0xF));
}

void Core::decodeArguments(Core::TaggedOneRegister& args) {
    args.type = decodeBits<byte, TypeTag, 0xF0, 4>(populateDestination(args));
}


} // namespace forth
