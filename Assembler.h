// construct assembly instructions
#ifndef ASSEMBLER_H__
#define ASSEMBLER_H__

#include "Types.h"
#include "Instruction.h"
#include "Problem.h"
#include <variant>
#include <type_traits>
#include <functional>
#include <map>
#include <list>
#include "Datum.h"

namespace forth {
class AssemblerBuilder;
using ResolvableLazyFunction = std::function<Address(AssemblerBuilder&, Address from)>;
using SizedResolvableLazyFunction = std::tuple<byte, ResolvableLazyFunction>;
using LazyInstruction = std::function<Address()>;
using SizedLazyInstruction = std::tuple<byte, LazyInstruction>;
/**
 * Used to denote a modifier to an instruction to be performed then and there
 * useful for macros!
 */
using EagerInstruction = std::function<void(AssemblerBuilder&)>;
class AssemblerBuilder {
	public:
		using AddressToMolecule = std::tuple<Address, Molecule>;
		using NameToAddress = std::tuple<std::string, Address>;
		using DelayedInstruction = std::variant<Address, LazyInstruction>;
	public:
		AssemblerBuilder(Address baseAddress);
		~AssemblerBuilder();
		void installIntoMemory(std::function<void(Address, Address)> fn);
		void installMolecule(Address address, const Molecule& m);
		void installMolecule(Address address, LazyInstruction op);
		void installMolecule(AddressToMolecule tup);
		void labelHere(const std::string& name);
		Address absoluteLabelAddress(const std::string& name) const;
		Integer relativeLabelAddress(const std::string& name) const;
		Integer relativeLabelAddress(const std::string& name, Address from) const;
		Address here() const noexcept { return _currentLocation; }
		Address getBaseAddress() const noexcept { return _baseAddress; }
		void addInstruction(const Molecule& m);
		void addInstruction(LazyInstruction op, byte width = sizeof(Address));
		void addInstruction(ResolvableLazyFunction op, byte width = sizeof(Address));
		void addInstruction(SizedResolvableLazyFunction op);
		void addInstruction(SizedLazyInstruction op);
		void addInstruction(EagerInstruction op);
		void addInstruction(const std::string& label);
		template<typename T>
		void addInstruction(T first) {
            if constexpr (std::is_same<EagerInstruction, T>::value) {
                addInstruction(EagerInstruction(first));
            } else {
			    if (auto width = getInstructionWidth(first); width == 0) {
			    	throw Problem("AssemblerBuilder::addInstruction", "Got an instruction of width 0");
			    } else if (width > sizeof(T)) {
			    	throw Problem("AssemblerBuilder::addInstruction", "Got an instruction which is wider than the type provided!");
			    } else {
			    	_operations.emplace(_currentLocation, first);
			    	_currentLocation += width;
			    }
            }
		}
		template<typename T, typename ... Rest>
		void addInstruction(T first, Rest&& ... rest) {
			addInstruction(first);
			if constexpr (sizeof...(rest) > 0) {
				addInstruction<Rest...>(std::move(rest)...);
			}
		}
	private:
		Address _baseAddress, _currentLocation;
		std::map<std::string, Address> _names;
		std::map<Address, DelayedInstruction> _operations;
};
constexpr byte encodeSingleByteOperation(Operation op) noexcept {
	return static_cast<byte>(op);
}

constexpr QuarterAddress encodeTwoByte(byte a, byte b) noexcept {
	return setLowerUpperHalves<QuarterAddress>(a, b);
}
constexpr QuarterAddress encodeTwoByte(Operation op, byte b) noexcept {
	return encodeTwoByte(byte(op), b);
}
constexpr QuarterAddress makeQuarterAddress(byte lower, byte upper) noexcept {
	return encodeTwoByte(lower, upper);
}

template<typename T>
constexpr QuarterAddress encodeTwoByte(Operation first, TargetRegister dest, T src) noexcept {
	return encodeTwoByte(first, encodeRegisterPair(dest, src));
}

constexpr HalfAddress encodeThreeByte(Operation first, byte second, byte third) noexcept {
	return setFourQuarters<HalfAddress>(byte(first), second, third, 0);
}
constexpr HalfAddress encodeThreeByte(Operation first, QuarterAddress second) noexcept {
	return encodeThreeByte(first, getLowerHalf(second), getUpperHalf(second));
}

constexpr HalfAddress encodeThreeByte(Operation first, TargetRegister dest, TargetRegister src0, TargetRegister src1) noexcept {
	return encodeThreeByte(first, encodeRegisterPair(dest, src0), setLowerHalf(byte(0), byte(src1)));
}

constexpr HalfAddress encodeFourByte(Operation first, byte second, byte third, byte fourth) noexcept {
	return setFourQuarters<HalfAddress>(byte(first), second, third, fourth);
}
static_assert(std::is_same<byte, QuarterOf<HalfAddress>>::value, "Invalid type inference!");
constexpr HalfAddress encodeFourByte(Operation first, TargetRegister dest, QuarterAddress upper) noexcept {
	return encodeFourByte(first, byte(dest), getLowerHalf(upper), getUpperHalf(upper));
}
constexpr HalfAddress encodeFourByte(Operation first, TargetRegister dest, TargetRegister src0, TargetRegister src1, QuarterAddress offset = 0) noexcept {
	return encodeFourByte(first, encodeRegisterPair(dest, src0), 
								 setLowerUpperHalves<byte>(byte(src1), byte(offset)),
								 decodeBits<QuarterAddress, byte, 0x0FF0, 4>(offset));
}

constexpr HalfAddress encodeFourByte(Operation first, TargetRegister dest, TargetRegister src0, QuarterAddress offset = 0) noexcept {
	return encodeFourByte(first, encodeRegisterPair(dest, src0), 
								 getLowerHalf(offset),
								 getUpperHalf(offset));
}

constexpr HalfAddress xorl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::Xor, dest, src0, src1);
}
constexpr HalfAddress xorl(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::XorImmediate, dest, src0, value);
}
constexpr HalfAddress xoru(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedXor, dest, src0, src1);
}
constexpr HalfAddress xoriu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedXorImmediate, dest, src0, value);
}
constexpr HalfAddress xorb(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::BooleanXor, dest, src0, src1);
}
constexpr HalfAddress orl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::Or, dest, src0, src1);
}
constexpr HalfAddress orl(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::OrImmediate, dest, src0, value);
}
constexpr HalfAddress oru(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedOr, dest, src0, src1);
}
constexpr HalfAddress oriu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedOrImmediate, dest, src0, value);
}
constexpr HalfAddress orb(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::BooleanOr, dest, src0, src1);
}
constexpr HalfAddress andl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::And, dest, src0, src1);
}
constexpr HalfAddress andl(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::AndImmediate, dest, src0, value);
}
constexpr HalfAddress andu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedAnd, dest, src0, src1);
}
constexpr HalfAddress andiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedAndImmediate, dest, src0, value);
}
constexpr HalfAddress andb(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::BooleanAnd, dest, src0, src1);
}
constexpr QuarterAddress minusl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
	return encodeTwoByte(Operation::Minus, dest, src0);
}
constexpr QuarterAddress minuslu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
	return encodeTwoByte(Operation::UnsignedMinus, dest, src0);
}
constexpr QuarterAddress minusf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
	return encodeTwoByte(Operation::FloatingPointMinus, dest, src0);
}
constexpr QuarterAddress notl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
	return encodeTwoByte(Operation::Not, dest, src0);
}
constexpr QuarterAddress notlu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
	return encodeTwoByte(Operation::UnsignedNot, dest, src0);
}
constexpr QuarterAddress notb(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
	return encodeTwoByte(Operation::BooleanNot, dest, src0);
}
constexpr HalfAddress pow(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::Pow, dest, src0, src1);
}
constexpr HalfAddress powf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::FloatingPointPow, dest, src0, src1);
}
constexpr HalfAddress powu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedPow, dest, src0, src1);
}

constexpr HalfAddress cmpeq(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::Equals, dest, src0, src1);
}
constexpr HalfAddress cmpeq(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::EqualsImmediate, dest, src0, value);
}
constexpr HalfAddress cmpeqf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::FloatingPointEquals, dest, src0, src1);
}
constexpr HalfAddress cmpequ(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedEquals, dest, src0, src1);
}
constexpr HalfAddress cmpeqiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedEqualsImmediate, dest, src0, value);
}
constexpr HalfAddress cmpeqb(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::BooleanEquals, dest, src0, src1);
}

constexpr QuarterAddress typeval(TargetRegister dest = TargetRegister::A) noexcept {
	return encodeTwoByte(Operation::TypeValue, byte(dest));
}
constexpr QuarterAddress typevalf(TargetRegister dest = TargetRegister::A) noexcept {
	return encodeTwoByte(Operation::FloatingPointTypeValue, byte(dest));
}
constexpr QuarterAddress typevalb(TargetRegister dest = TargetRegister::A) noexcept {
	return encodeTwoByte(Operation::BooleanTypeValue, byte(dest));
}
constexpr QuarterAddress typevalu(TargetRegister dest = TargetRegister::A) noexcept {
	return encodeTwoByte(Operation::UnsignedTypeValue, byte(dest));
}

constexpr HalfAddress cmplt(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::LessThan, dest, src0, src1);
}
constexpr HalfAddress cmplt(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::LessThanImmediate, dest, src0, value);
}
constexpr HalfAddress cmpltf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::FloatingPointLessThan, dest, src0, src1);
}
constexpr HalfAddress cmpltu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedLessThan, dest, src0, src1);
}
constexpr HalfAddress cmpltiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedLessThanImmediate, dest, src0, value);
}


constexpr HalfAddress cmpgt(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::GreaterThan, dest, src0, src1);
}
constexpr HalfAddress cmpgt(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::GreaterThanImmediate, dest, src0, value);
}
constexpr HalfAddress cmpgtf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::FloatingPointGreaterThan, dest, src0, src1);
}
constexpr HalfAddress cmpgtu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedGreaterThan, dest, src0, src1);
}
constexpr HalfAddress cmpgtiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedGreaterThanImmediate, dest, src0, value);
}
constexpr HalfAddress shr(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::ShiftRight, dest, src0, src1);
}
constexpr HalfAddress shr(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::ShiftRightImmediate, dest, src0, value);
}
constexpr HalfAddress shru(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedShiftRight, dest, src0, src1);
}
constexpr HalfAddress shriu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedShiftRightImmediate, dest, src0, value);
}
constexpr HalfAddress shl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::ShiftLeft, dest, src0, src1);
}
constexpr HalfAddress shl(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::ShiftLeftImmediate, dest, src0, value);
}
constexpr HalfAddress shlu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedShiftLeft, dest, src0, src1);
}
constexpr HalfAddress shliu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedShiftLeftImmediate, dest, src0, value);
}
constexpr HalfAddress add(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::Add, dest, src0, src1);
}
constexpr HalfAddress add(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::AddImmediate, dest, src0, value);
}
constexpr HalfAddress addf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::FloatingPointAdd, dest, src0, src1);
}
constexpr HalfAddress addu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedAdd, dest, src0, src1);
}
constexpr HalfAddress addiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedAddImmediate, dest, src0, value);
}
constexpr HalfAddress sub(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::Subtract, dest, src0, src1);
}
constexpr HalfAddress sub(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::SubtractImmediate, dest, src0, value);
}
constexpr HalfAddress subf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::FloatingPointSubtract, dest, src0, src1);
}
constexpr HalfAddress subu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedSubtract, dest, src0, src1);
}
constexpr HalfAddress subiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedSubtractImmediate, dest, src0, value);
}
constexpr HalfAddress mul(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::Multiply, dest, src0, src1);
}
constexpr HalfAddress mul(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::MultiplyImmediate, dest, src0, value);
}
constexpr HalfAddress mulf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::FloatingPointMultiply, dest, src0, src1);
}
constexpr HalfAddress mulu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedMultiply, dest, src0, src1);
}
constexpr HalfAddress muliu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedMultiplyImmediate, dest, src0, value);
}
constexpr HalfAddress div(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::Divide, dest, src0, src1);
}
constexpr HalfAddress div(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::DivideImmediate, dest, src0, value);
}
constexpr HalfAddress divf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::FloatingPointDivide, dest, src0, src1);
}
constexpr HalfAddress divu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedDivide, dest, src0, src1);
}
constexpr HalfAddress diviu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedDivideImmediate, dest, src0, value);
}
constexpr HalfAddress mod(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::Modulo, dest, src0, src1);
}
constexpr HalfAddress mod(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::ModuloImmediate, dest, src0, value);
}
constexpr HalfAddress modu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
	return encodeThreeByte(Operation::UnsignedModulo, dest, src0, src1);
}
constexpr HalfAddress modiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
	return encodeFourByte(Operation::UnsignedModuloImmediate, dest, src0, value);
}
constexpr QuarterAddress popRegister(TargetRegister destination, TargetRegister sp = TargetRegister::SP) noexcept {
	return encodeTwoByte(Operation::PopRegister, encodeRegisterPair(destination, sp));
}
constexpr QuarterAddress pushRegister(TargetRegister value, TargetRegister sp = TargetRegister::SP) noexcept {
    	return encodeTwoByte(Operation::PushRegister, encodeRegisterPair(sp, value));
}
constexpr QuarterAddress load(TargetRegister dest, TargetRegister src) noexcept { 
	return encodeTwoByte(Operation::Load, dest, src); 
}
constexpr QuarterAddress store(TargetRegister dest, TargetRegister src) noexcept { 
	return encodeTwoByte(Operation::Store, dest, src); 
}
constexpr QuarterAddress move(TargetRegister dest, TargetRegister src) noexcept { 
	return encodeTwoByte(Operation::Move, dest, src); 
}
constexpr QuarterAddress swap(TargetRegister dest, TargetRegister src) noexcept { 
	return encodeTwoByte(Operation::Swap, dest, src); 
}
//constexpr HalfAddress setImmediate16_Lower(TargetRegister dest, QuarterAddress value) noexcept {
//	return encodeFourByte(Operation::SetImmediate16_Lower, dest, value);  
//}
//constexpr HalfAddress setImmediate16_Lower(TargetRegister dest, QuarterAddressWrapper value) noexcept {
//	return encodeFourByte(Operation::SetImmediate16_Lower, dest, value.get());  
//}
//constexpr HalfAddress setImmediate16_Lowest(TargetRegister dest, QuarterAddress value) noexcept {
//	return encodeFourByte(Operation::SetImmediate16_Lowest, dest, value); 
//}
//constexpr HalfAddress setImmediate16_Lowest(TargetRegister dest, QuarterAddressWrapper value) noexcept {
//	return encodeFourByte(Operation::SetImmediate16_Lowest, dest, value.get()); 
//}
//constexpr HalfAddress setImmediate16_Higher(TargetRegister dest, QuarterAddress value) noexcept { 
//	return encodeFourByte(Operation::SetImmediate16_Higher, dest, value); 
//}
//constexpr HalfAddress setImmediate16_Higher(TargetRegister dest, QuarterAddressWrapper value) noexcept {
//	return encodeFourByte(Operation::SetImmediate16_Lowest, dest, value.get()); 
//}
//constexpr HalfAddress setImmediate16_Highest(TargetRegister dest, QuarterAddress value) noexcept { 
//	return encodeFourByte(Operation::SetImmediate16_Highest, dest, value); 
//}
//constexpr HalfAddress setImmediate16_Highest(TargetRegister dest, QuarterAddressWrapper value) noexcept { 
//	return encodeFourByte(Operation::SetImmediate16_Highest, dest, value.get()); 
//}
//constexpr HalfAddress setImmediate64_Lowest(TargetRegister dest, Address value) noexcept { 
//    return setImmediate16_Lowest(dest, getLowestQuarter(value));
//}
//constexpr HalfAddress setImmediate64_Lower(TargetRegister dest, Address value) noexcept { 
//    return setImmediate16_Lower(dest, getLowerQuarter(value));
//}
//constexpr HalfAddress setImmediate64_Higher(TargetRegister dest, Address value) noexcept { 
//    return setImmediate16_Higher(dest, getHigherQuarter(value));
//}
//constexpr HalfAddress setImmediate64_Highest(TargetRegister dest, Address value) noexcept { 
//    return setImmediate16_Highest(dest, getHighestQuarter(value));
//}
//static_assert(0xFFFF0519 == setImmediate64_Highest(TargetRegister::X, 0xFFFF'FFFF'FFFF'0001), "Encoding is wrong!");

constexpr QuarterAddress notOp(TargetRegister dest, TargetRegister src) noexcept {
	return encodeTwoByte(Operation::Not, dest, src);
}
constexpr QuarterAddress minus(TargetRegister dest, TargetRegister src) noexcept {
	return encodeTwoByte(Operation::Minus, dest, src);
}

constexpr HalfAddress jump(QuarterInteger offset) noexcept {
    return encodeFourByte(Operation::Jump,
            decodeBits<QuarterInteger, byte, 0x00FF, 0>(offset),
            decodeBits<QuarterInteger, byte, QuarterInteger(0xFF00), 8>(offset),
            0);
}
constexpr QuarterAddress jumpIndirect(TargetRegister reg) noexcept {
    // put a zero there!
    return encodeTwoByte(Operation::JumpIndirect, reg, TargetRegister::A);
}
constexpr HalfAddress conditionalBranch(TargetRegister cond, QuarterInteger offset) noexcept {
    return encodeFourByte(Operation::ConditionalBranch, 
			byte(cond), 
            decodeBits<QuarterInteger, byte, 0x00FF, 0>(offset),
            decodeBits<QuarterInteger, byte, QuarterInteger(0xFF00), 8>(offset));
}
constexpr HalfAddress conditionalCallSubroutine(TargetRegister cond, QuarterInteger offset) noexcept {
    return encodeFourByte(Operation::ConditionalCallSubroutine, cond, offset);
}
constexpr HalfAddress callSubroutine(QuarterInteger offset) noexcept {
	return encodeThreeByte(Operation::CallSubroutine, offset);
}
constexpr byte returnSubroutine() noexcept {
    return encodeSingleByteOperation(Operation::ReturnSubroutine);
}
constexpr QuarterAddress conditionalReturnSubroutine(TargetRegister cond) noexcept {
    return encodeTwoByte(Operation::ConditionalReturnSubroutine, cond, 0);
}
constexpr QuarterAddress conditionalCallSubroutineIndirect(TargetRegister dest, TargetRegister cond) noexcept {
    return encodeTwoByte(Operation::ConditionalCallSubroutineIndirect, dest, cond);
}
constexpr Address loadLowerImmediate48(TargetRegister dest, Address value) noexcept {
	auto imm48 = decodeBits<Address, Address, 0x0000'FFFF'FFFF'FFFF, 0>(value) << 16;
	auto edest = Address(encodeDestinationRegister(dest)) << 8;
	auto op = Address(byte(Operation::LoadImmediateLower48));
	return imm48 | edest | op;
}
constexpr QuarterAddress popA() noexcept { return popRegister(TargetRegister::A, TargetRegister::SP); }
constexpr QuarterAddress popB() noexcept { return popRegister(TargetRegister::B, TargetRegister::SP); }
constexpr QuarterAddress popC() noexcept { return popRegister(TargetRegister::C, TargetRegister::SP); }
constexpr QuarterAddress pushA() noexcept { return pushRegister(TargetRegister::A, TargetRegister::SP); }
constexpr QuarterAddress pushB() noexcept { return pushRegister(TargetRegister::B, TargetRegister::SP); }
constexpr QuarterAddress pushC() noexcept { return pushRegister(TargetRegister::C, TargetRegister::SP); }
EagerInstruction popAB();

constexpr QuarterAddress swapAB() noexcept {
    return swap(TargetRegister::B, TargetRegister::A);
}
constexpr auto zeroRegister(TargetRegister reg) noexcept -> decltype(move(reg, TargetRegister::Zero)) {
    return move(reg, TargetRegister::Zero);
}
constexpr QuarterAddress imm16TestValue = 0xfded;
static_assert(popA() == popRegister(TargetRegister::A, TargetRegister::SP), "Two different code paths for popA should yield the same result!");
static_assert(getInstructionWidth(cmpeq()) == 3, "Compare eq should only be one byte if the args are defaulted");
static_assert(getInstructionWidth(cmpeq()) == 3, "Compare eq should only be one byte if the args are defaulted");
static_assert(byte(0xFD) == getUpperHalf(imm16TestValue), "getUpperHalf is not working correctly!");
static_assert(byte(0xED) == getLowerHalf(imm16TestValue), "getUpperHalf is not working correctly!");
static_assert(byte(0x01) == byte(TargetRegister::A), "register cast assumptions broken!");
//static_assert(byte(0x16) == byte(Operation::SetImmediate16_Lowest), "Operation index is no longer correct!");
//static_assert(Address(0xFDED0116) == encodeFourByte(Operation::SetImmediate16_Lowest, TargetRegister::A, imm16TestValue), "Encoding is broken!");
//static_assert(Address(0xFDED0116) == setImmediate16_Lowest(TargetRegister::A, imm16TestValue), "setImmediate16_Lowest is broken!");
//static_assert(Address(0xFDED0117) == setImmediate16_Lower(TargetRegister::A, imm16TestValue), "setImmediate16_Lower is broken!");
//static_assert(Address(0xFDED0118) == setImmediate16_Higher(TargetRegister::A, imm16TestValue), "setImmediate16_Higher is broken!");
//static_assert(Address(0xFDED0119) == setImmediate16_Highest(TargetRegister::A, imm16TestValue), "setImmediate16_Highest is broken!");
static_assert(getInstructionWidth(mulf(TargetRegister::A, TargetRegister::A, TargetRegister::A)) == 3, "FloatingPointMultiplyFull is not three bytes!");
static_assert(getInstructionWidth(mulf()) == 3, "FloatingPointMultiplyFull is not three bytes!");

SizedResolvableLazyFunction setImmediate16_Lowest(TargetRegister r, const std::string& name);
SizedResolvableLazyFunction setImmediate16_Lower(TargetRegister r, const std::string& name);
SizedResolvableLazyFunction setImmediate16_Higher(TargetRegister r, const std::string& name);
SizedResolvableLazyFunction setImmediate16_Highest(TargetRegister r, const std::string& name);
SizedResolvableLazyFunction loadLowerImmediate48(TargetRegister r, const std::string& name);
constexpr HalfAddress jumpAbsolute(QuarterAddress imm16) noexcept {
	return encodeThreeByte(Operation::JumpAbsolute, imm16);
}
SizedResolvableLazyFunction jumpAbsolute(const std::string& name);
constexpr HalfAddress jumpRelative(QuarterInteger imm16) noexcept {
	return encodeThreeByte(Operation::Jump, byte(imm16), byte(imm16 >> 8));
}
SizedResolvableLazyFunction jumpRelative(const std::string& name);
SizedResolvableLazyFunction conditionalBranch(TargetRegister reg, const std::string& name);

EagerInstruction loadImmediate64(TargetRegister r, Address value);
EagerInstruction loadImmediate64(TargetRegister r, const std::string& name);

constexpr byte returnToNative() noexcept {
	return encodeSingleByteOperation(Operation::LeaveExecutionLoop);
}

constexpr auto loadImmediate16(TargetRegister dest, QuarterAddress value) noexcept -> decltype(addiu(dest, TargetRegister::Zero, value)) { 
	return addiu(dest, TargetRegister::Zero, value);
}

/**
 * Store into register X our contents!
 */
EagerInstruction storeImmediate64(Address value);
EagerInstruction storeImmediate64(TargetRegister addr, Address value);
/**
 * load a specific address into X and a value into Temporary, then store temporary into X
 */
EagerInstruction storeImmediate64(Address addr, Address value);
EagerInstruction storeImmediate64(Address addr, const std::string& value);

EagerInstruction indirectLoad(TargetRegister dest, TargetRegister src = TargetRegister::X);

EagerInstruction label(const std::string& name);
EagerInstruction pushImmediate(const Datum& value, TargetRegister sp);
EagerInstruction pushImmediate(Address value, TargetRegister sp);

constexpr QuarterAddress printString(TargetRegister start, TargetRegister length) noexcept {
	return encodeTwoByte(Operation::PrintString, start, length);
}


constexpr QuarterAddress printChar(TargetRegister src) noexcept {
	return encodeTwoByte(Operation::PrintChar, encodeDestinationRegister(src));
}
EagerInstruction printChar(char c);
EagerInstruction printChar(const std::string& str);

constexpr QuarterAddress typeDatum(TargetRegister src) noexcept {
	return encodeTwoByte(Operation::TypeDatum, encodeDestinationRegister(src));
}

} // end namespace forth
static_assert(sizeof(unsigned long long int) >= sizeof(forth::Address), "Unsigned long long int is a 64-bit value or greater!");
constexpr forth::QuarterAddressWrapper operator "" _addrqlowest(unsigned long long int addr) {  return forth::getLowestQuarter(forth::Address(addr)); }
constexpr forth::QuarterAddressWrapper operator "" _addrqlower(unsigned long long int addr) {   return forth::getLowerQuarter(forth::Address(addr)); }
constexpr forth::QuarterAddressWrapper operator "" _addrqhigher(unsigned long long int addr) {  return forth::getHigherQuarter(forth::Address(addr)); }
constexpr forth::QuarterAddressWrapper operator "" _addrqhighest(unsigned long long int addr) { return forth::getHighestQuarter(forth::Address(addr)); }

constexpr forth::HalfAddressWrapper operator "" _hupper(unsigned long long int addr) {  return forth::getUpperHalf(forth::Address(addr)); }
constexpr forth::HalfAddressWrapper operator "" _hlower(unsigned long long int addr) { return forth::getLowerHalf(forth::Address(addr)); }

#endif
