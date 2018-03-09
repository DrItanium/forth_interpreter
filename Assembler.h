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
#include "Core.h"

namespace forth {
class AssemblerBuilder;
using ResolvableLazyFunction = std::function<Core::DecodedOperation(AssemblerBuilder&, Address from)>;
using SizedResolvableLazyFunction = std::tuple<byte, ResolvableLazyFunction>;
using LazyInstruction = std::function<Core::DecodedOperation()>;
using SizedLazyInstruction = std::tuple<byte, LazyInstruction>;
/**
 * Used to denote a modifier to an instruction to be performed then and there
 * useful for macros!
 */
using EagerInstruction = std::function<void(AssemblerBuilder&)>;
class AssemblerBuilder {
	public:
		using NameToAddress = std::tuple<std::string, Address>;
		using DelayedInstruction = std::variant<Core::DecodedOperation, LazyInstruction>;
	public:
		AssemblerBuilder(Address baseAddress);
		~AssemblerBuilder();
		void installIntoCore(Core& core);
		void labelHere(const std::string& name);
		Address absoluteLabelAddress(const std::string& name) const;
		Integer relativeLabelAddress(const std::string& name) const;
		Integer relativeLabelAddress(const std::string& name, Address from) const;
		Address here() const noexcept { return _currentLocation; }
		Address getBaseAddress() const noexcept { return _baseAddress; }
		void addInstruction(LazyInstruction op, byte width = sizeof(Address));
		void addInstruction(ResolvableLazyFunction op, byte width = sizeof(Address));
		void addInstruction(SizedResolvableLazyFunction op);
		void addInstruction(SizedLazyInstruction op);
		void addInstruction(EagerInstruction op);
		template<typename T>
		void addInstruction(T first) {
            if constexpr (std::is_same<EagerInstruction, T>::value) {
                addInstruction(EagerInstruction(first));
            } else {
			    _operations.emplace(_currentLocation, first);
				_currentLocation += first.size();
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


#define OneByte(title) Core:: title op ## title () noexcept ;
#define TwoByte(title, b) Core:: title op ## title (const Core:: b &) noexcept ;
#define ThreeByte(title, b) Core:: title op ## title (const Core:: b &) noexcept ;
#define FourByte(title, b) Core:: title op ## title (const Core:: b &) noexcept ;
#define GrabBag(title, b) Core:: title op ## title (const Core:: b &) noexcept ;
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef GrabBag

EagerInstruction opPrintChar(char c) noexcept;
EagerInstruction opPrintChar(const std::string& str) noexcept;
Core::PushRegister opPushRegister(TargetRegister reg, TargetRegister sp = TargetRegister::SP) noexcept;
Core::PopRegister opPopRegister(TargetRegister reg, TargetRegister sp = TargetRegister::SP) noexcept;
Core::Move zeroRegister(TargetRegister reg) noexcept;
Core::LoadImmediate64 loadImmediate64(TargetRegister reg, Address value) noexcept;
EagerInstruction useRegister(TargetRegister reg, EagerInstruction body) noexcept;
Core::PopRegister popA() noexcept;
Core::PopRegister popB() noexcept;
Core::PopRegister popC() noexcept;
Core::PushRegister pushA() noexcept;
Core::PushRegister pushB() noexcept;
Core::PushRegister pushC() noexcept;
EagerInstruction popAB() noexcept;
Core::Swap swapAB() noexcept;
EagerInstruction label(const std::string&);
//
//constexpr QuarterAddress swapAB() noexcept {
//    return swap(TargetRegister::B, TargetRegister::A);
//}
//constexpr auto zeroRegister(TargetRegister reg) noexcept -> decltype(move(reg, TargetRegister::Zero)) {
//    return move(reg, TargetRegister::Zero);
//}
//constexpr QuarterAddress imm16TestValue = 0xfded;
//static_assert(popA() == popRegister(TargetRegister::A, TargetRegister::SP), "Two different code paths for popA should yield the same result!");
//static_assert(getInstructionWidth(cmpeq()) == 3, "Compare eq should only be one byte if the args are defaulted");
//static_assert(getInstructionWidth(cmpeq()) == 3, "Compare eq should only be one byte if the args are defaulted");
//static_assert(byte(0xFD) == getUpperHalf(imm16TestValue), "getUpperHalf is not working correctly!");
//static_assert(byte(0xED) == getLowerHalf(imm16TestValue), "getUpperHalf is not working correctly!");
//static_assert(byte(0x01) == byte(TargetRegister::A), "register cast assumptions broken!");
////static_assert(byte(0x16) == byte(Operation::SetImmediate16_Lowest), "Operation index is no longer correct!");
////static_assert(Address(0xFDED0116) == encodeFourByte(Operation::SetImmediate16_Lowest, TargetRegister::A, imm16TestValue), "Encoding is broken!");
////static_assert(Address(0xFDED0116) == setImmediate16_Lowest(TargetRegister::A, imm16TestValue), "setImmediate16_Lowest is broken!");
////static_assert(Address(0xFDED0117) == setImmediate16_Lower(TargetRegister::A, imm16TestValue), "setImmediate16_Lower is broken!");
////static_assert(Address(0xFDED0118) == setImmediate16_Higher(TargetRegister::A, imm16TestValue), "setImmediate16_Higher is broken!");
////static_assert(Address(0xFDED0119) == setImmediate16_Highest(TargetRegister::A, imm16TestValue), "setImmediate16_Highest is broken!");
//static_assert(getInstructionWidth(mulf(TargetRegister::A, TargetRegister::A, TargetRegister::A)) == 3, "FloatingPointMultiplyFull is not three bytes!");
//static_assert(getInstructionWidth(mulf()) == 3, "FloatingPointMultiplyFull is not three bytes!");
//
//SizedResolvableLazyFunction setImmediate16_Lowest(TargetRegister r, const std::string& name);
//SizedResolvableLazyFunction setImmediate16_Lower(TargetRegister r, const std::string& name);
//SizedResolvableLazyFunction setImmediate16_Higher(TargetRegister r, const std::string& name);
//SizedResolvableLazyFunction setImmediate16_Highest(TargetRegister r, const std::string& name);
//SizedResolvableLazyFunction loadLowerImmediate48(TargetRegister r, const std::string& name);
//constexpr HalfAddress jumpAbsolute(QuarterAddress imm16) noexcept {
//	return encodeThreeByte(Operation::JumpAbsolute, imm16);
//}
//SizedResolvableLazyFunction jumpAbsolute(const std::string& name);
//constexpr HalfAddress jumpRelative(QuarterInteger imm16) noexcept {
//	return encodeThreeByte(Operation::Jump, byte(imm16), byte(imm16 >> 8));
//}
//SizedResolvableLazyFunction jumpRelative(const std::string& name);
//SizedResolvableLazyFunction conditionalBranch(TargetRegister reg, const std::string& name);
//
//EagerInstruction loadImmediate64(TargetRegister r, Address value);
//EagerInstruction loadImmediate64(TargetRegister r, const std::string& name);
//
//
//constexpr auto loadImmediate16(TargetRegister dest, QuarterAddress value) noexcept -> decltype(addiu(dest, TargetRegister::Zero, value)) { 
//	return addiu(dest, TargetRegister::Zero, value);
//}
//
///**
// * Store into register X our contents!
// */
//EagerInstruction storeImmediate64(Address value);
//EagerInstruction storeImmediate64(TargetRegister addr, Address value);
///**
// * load a specific address into X and a value into Temporary, then store temporary into X
// */
//EagerInstruction storeImmediate64(Address addr, Address value);
//EagerInstruction storeImmediate64(Address addr, const std::string& value);
//
//EagerInstruction indirectLoad(TargetRegister dest, TargetRegister src = TargetRegister::X);
//
//EagerInstruction label(const std::string& name);
//EagerInstruction pushImmediate(const Datum& value, TargetRegister sp);
//EagerInstruction pushImmediate(Address value, TargetRegister sp);
//
//constexpr QuarterAddress printString(TargetRegister start, TargetRegister length) noexcept {
//	return encodeTwoByte(Operation::PrintString, start, length);
//}
//
//
//constexpr QuarterAddress printChar(TargetRegister src) noexcept {
//	return encodeTwoByte(Operation::PrintChar, encodeDestinationRegister(src));
//}
//EagerInstruction printChar(char c);
//EagerInstruction printChar(const std::string& str);
//
//constexpr QuarterAddress typeDatum(TargetRegister src) noexcept {
//	return encodeTwoByte(Operation::TypeDatum, encodeDestinationRegister(src));
//}

} // end namespace forth

#endif
