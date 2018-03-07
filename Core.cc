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

std::optional<Core::DecodedOperation> Core::decodeInstruction(byte control, OneByteInstruction) {
    // code 0: OneByte [ variant:3 | op: 5 ] // maximum of 32 ops in this class
    Core::OneByteOperation op;
    switch (decodeBits<byte, OneByteOpcode, 0b11111000, 3>(control)) {
#define OneByte(title) case OneByteOpcode:: title : op = Core:: title () ; break;
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b) 
#define GrabBag(title, b) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef GrabBag
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
		decodeArguments(std::get< Core:: title > (op).args); \
		break;
#define ThreeByte(title, b) 
#define FourByte(title, b) 
#define GrabBag(title, b) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef GrabBag
        default:
            break;
    }
    return std::optional<Core::DecodedOperation>(op);
}

std::optional<Core::DecodedOperation> Core::decodeInstruction(byte control, ThreeByteInstruction) {
	// code 2: ThreeByte [ variant:3 | opcontrol:21 ]
	//          ThreeRegister [ variant:3 [2] | op: 5 | dest: 4 | src0: 4 | src1: 4 | unused: 4 ] 
	Core::ThreeByteOperation op;
	switch (decodeBits<byte, ThreeByteOpcode, 0b11111000, 3>(control)) {
#define OneByte(title) 
#define TwoByte(title, b) 
#define ThreeByte(title , b) \
		case ThreeByteOpcode:: title : \
		op = Core:: title () ; \
		decodeArguments(std::get< Core:: title > (op).args); \
		break; 
#define FourByte(title, b) 
#define GrabBag(title, b) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef GrabBag
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
		decodeArguments(std::get< Core:: title > (op).args); \
		break; 
#define GrabBag(title, b) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
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
#define GrabBag(title, b) \
		case GrabBagOpcode :: title : \
		op = Core:: title () ; \
		decodeArguments(std::get< Core:: title > (op).args); \
		break; 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
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

Register& Core::getSourceRegister(const Core::SourceRegister& reg) {
	if (reg) {
		return getRegister(reg.value());
	} else {
		throw Problem("getSourceRegister", "Source register is unpopulated!");
	}
}

void Core::dispatchOperation(const Core::TwoByteOperation& op) {
	std::visit([this](auto&& value) {
				using T = std::decay_t<decltype(value)>;
				if constexpr (std::is_same_v<T, UndefinedOpcode>) {
					throw Problem("dispatchOperation(TwoByte)", "undefined two byte operation");
				} else {
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
			    	    savePositionToSubroutineStack();
						_pc.setValue(dest.getValue());
					} else if constexpr (std::is_same_v<T, Core::JumpIndirect>) {
						_pc.setValue(dest.getValue());
					} else if constexpr (std::is_same_v<T, Core::ConditionalBranchIndirect>) {
						if (dest.getTruth()) {
							_pc.setValue(getSourceRegister(value.args).getValue());
						}
					} else if constexpr (std::is_same_v<T, Core::ConditionalCallSubroutineIndirect>) {
						if (dest.getTruth()) {
							savePositionToSubroutineStack();
							_pc.setValue(dest.getValue());
						}
					} else if constexpr (std::is_same_v<T, Core::Load>) {
						dest.setValue(loadWord(getSourceRegister(value.args).getAddress()));
					} else if constexpr (std::is_same_v<T, Core::Store>) {
						store(dest.getAddress(), getSourceRegister(value.args).getValue());
					} else if constexpr (std::is_same_v<T, Core::Move>) {
						dest.setValue(getSourceRegister(value.args).getValue());
					} else if constexpr (std::is_same_v<T, Core::Swap>) {
						auto temp = dest.getValue();
						dest.setValue(getSourceRegister(value.args).getValue());
						getSourceRegister(value.args).setValue(temp);
					} else if constexpr (std::is_same_v<T, Core::PopRegister>) {
						// pop dest, sp
						auto& stackPointer = getSourceRegister(value.args);
						dest.setValue(loadWord(stackPointer.getAddress()));
						stackPointer.increment(sizeof(Address));
					} else if constexpr (std::is_same_v<T, Core::PushRegister>) {
						// push sp, src
						auto& stackPointer = dest;
						stackPointer.decrement(sizeof(Address));
						store(stackPointer.getAddress(), getSourceRegister(value.args).getValue());
					} else {
						//static_assert(AlwaysFalse<T>::value, "Unimplemented two byte operation");
						throw Problem("dispatchOperation(TwoByte)", "Unimplemented two byte operation");
					}
					std::cout.setf(flags);
				}
			}, op);
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


void Core::dispatchOperation(const Core::ThreeByteOperation& op) {
	auto intFunction = [](const Register& value) { return value.getInt(); };
	auto floatFunction = [](const Register& value) { return value.getFP(); };
	auto unsignedFunction = [](const Register& value) { return value.getAddress(); };
	auto booleanFunction = [](const Register& value) { return value.getTruth(); };
	std::visit([this, intFunction, floatFunction, unsignedFunction, booleanFunction](auto&& value) {
				using T = std::decay_t<decltype(value)>;
				if constexpr (std::is_same_v<T, UndefinedOpcode>) {
					throw Problem("dispatchOperation(ThreeByte)", "UndefinedOpcode provided");
				} else {
#define IsType(t) std::is_same_v<T, t>
					auto& dest = getDestinationRegister(value.args);
					auto& src0 = getSourceRegister(value.args);
					auto& src1 = getSource2Register(value.args);
#define InvokeConv(bfun, cfun) \
	dest.setValue( bfun ( cfun(src0) , cfun(src1) ))
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

					if constexpr (IsType(Nop3)) { }
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
					else {
						static_assert(AlwaysFalse<T>::value, "Unimplemented three byte operation!");
					}
#undef InvokeS
#undef InvokeU
#undef InvokeF
#undef InvokeB
#undef InvokeSU
#undef InvokeSUF
#undef InvokeSUB
#undef InvokeConv
#undef IsType
				}
			}, op);
}

void Core::dispatchOperation(const Core::FourByteOperation& op) {
	std::visit([this](auto&& value) {
				using T = std::decay_t<decltype(value)>;
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
				if constexpr (std::is_same_v<T, UndefinedOpcode>) {
					throw Problem("dispatchOperation(FourByte)", "UndefinedOpcode provided");
				} else if constexpr (std::is_same_v<T, Jump>) {
					_pc.setValue(_pc.getInt() + value.args.value);
				} else if constexpr (std::is_same_v<T, CallSubroutine>) {
					savePositionToSubroutineStack();
					_pc.setValue(_pc.getInt() + value.args.value);
				} else if constexpr (std::is_same_v<T, JumpAbsolute>) {
					_pc.setValue(Address(value.args.value));
				} else if constexpr (std::is_same_v<T, CallSubroutineAbsolute>) {
					savePositionToSubroutineStack();
					_pc.setValue(Address(value.args.value));
				} else if constexpr (std::is_same_v<T, JumpAbsolute>) {
				} else if constexpr (std::is_same_v<T, ConditionalBranch>) {
				} else if constexpr (std::is_same_v<T, ConditionalCallSubroutine>) { 
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
				else {
					static_assert(AlwaysFalse<T>::value, "Unimplemented four byte operation!");
				}
#undef IsType
#undef InvokeS
#undef InvokeU
#undef InvokeSU
			}, op);
}

auto minus(auto a) noexcept -> decltype(-a) { return -a; }
auto notOp(auto a) noexcept -> decltype(~a) { return ~a; }
bool notOp(bool a) noexcept { return !a; }


void Core::dispatchOperation(const Core::GrabBagOperation& op) {
	std::visit([this](auto&& value) {
				auto intFunction = [](const Register& value) { return value.getInt(); };
				auto floatFunction = [](const Register& value) { return value.getFP(); };
				auto unsignedFunction = [](const Register& value) { return value.getAddress(); };
				auto booleanFunction = [](const Register& value) { return value.getTruth(); };
				using T = std::decay_t<decltype(value)>;
				if constexpr (std::is_same_v<T, UndefinedOpcode>) {
					throw Problem("dispatchOperation(GrabBag)", "UndefinedOpcode provided");
				} 
#define IsType(t) std::is_same_v<T, t>
#define InvokeConv(bfun, cfun) \
	auto& dest = getDestinationRegister(value.args); \
	auto& src0 = getSourceRegister(value.args); \
	auto& src1 = getSourceRegister(value.args.source2); \
	dest.setValue( bfun ( cfun(src0) , cfun(src1) ))
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
				} else if constexpr (std::is_same_v<T, LoadImmediate16>) {
					getDestinationRegister(value.args).setValue(Address(value.args.imm16));
				} else if constexpr (std::is_same_v<T, LoadImmediate32>) {
					getDestinationRegister(value.args).setValue(Address(value.args.imm32));
				} else if constexpr (std::is_same_v<T, LoadImmediate48>) {
					getDestinationRegister(value.args).setValue(Address(value.args.imm48));
				} else if constexpr (std::is_same_v<T, LoadImmediate64>) {
					getDestinationRegister(value.args).setValue(Address(value.args.imm64));
				} else {
					static_assert(AlwaysFalse<T>::value, "Unimplemented grab bag operation!");
				}
			}, op);
}
void Core::dispatchInstruction() {
	auto control = extractByteFromMolecule();
	auto result = decodeInstruction(control);
	if (result) {
		std::visit([this](auto&& value) { dispatchOperation(value); }, result.value());
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

void Core::decodeArguments(FourRegister& args) {
    populateDestinationAndSource(args);
	auto b2 = extractByteFromMolecule();
	args.source2 = forth::getDestinationRegister(b2);
	args.source3 = forth::getDestinationRegister(b2);
}

void Core::decodeArguments(FiveRegister& args) {
    populateDestinationAndSource(args);
	auto b2 = extractByteFromMolecule();
	args.source2 = forth::getDestinationRegister(b2);
	args.source3 = forth::getDestinationRegister(b2);
	auto b3 = extractByteFromMolecule();
	args.source4 = forth::getDestinationRegister(b3);
}

void Core::decodeArguments(SignedImm16& args) {
	args.value = extractQuarterIntegerFromMolecule();
}

void Core::decodeArguments(Immediate24& args) {
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

void Core::decodeArguments(OneRegisterWithImm48& args) {
    populateDestination(args);
    args.imm48 = extractImm48();
}

void Core::decodeArguments(OneRegisterWithImm64& args) {
    populateDestination(args);
    auto lower48 = extractImm48();
    auto upper16 = Address(extractQuarterAddressFromMolecule()) << 48;
    args.imm64 = lower48 | upper16;
}


} // namespace forth
