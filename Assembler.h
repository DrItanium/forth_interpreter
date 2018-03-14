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
using ResolvableLazyFunction = std::function<void(AssemblerBuilder&, Address from)>;
/**
 * Used to denote a modifier to an instruction to be performed then and there
 * useful for macros!
 */
using EagerInstruction = std::function<void(AssemblerBuilder&)>;
class AssemblerBuilder {
	public:
		using NameToAddress = std::tuple<std::string, Address>;
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
		void addInstruction(EagerInstruction fn);
		void addInstruction(ResolvableLazyFunction fn);
		void addInstruction(const Core::DecodedOperation& op);
		//template<typename T>
		//void addInstruction(T first) {
        //    if constexpr (std::is_same<EagerInstruction, T>::value) {
		//		addInstruction(EagerInstruction(first));
        //    } else {
		//	    _operations.emplace(_currentLocation, first);
		//		_currentLocation += first.size();
        //    }
		//}
		template<typename T, typename ... Rest>
		void addInstruction(T first, Rest&& ... rest) {
			addInstruction(first);
			if constexpr (sizeof...(rest) > 0) {
				addInstruction(std::move(rest)...);
			}
		}

	private:
		Address _baseAddress, _currentLocation;
		std::map<std::string, Address> _names;
		std::map<Address, Core::DecodedOperation> _operations;
		std::vector<EagerInstruction> _toResolve;
};
#define DispatchOneRegister(title) 
#define DispatchTwoRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src) noexcept;
#define DispatchThreeRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, TargetRegister src1) noexcept;
#define DispatchFourRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, TargetRegister src1, TargetRegister src2) noexcept;
#define DispatchFiveRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, TargetRegister src1, TargetRegister src2, TargetRegister src3) noexcept;
#define DispatchSignedImm16(title)
#define DispatchImmediate24(title) Core:: title op ## title (HalfAddress addr) noexcept;
#define DispatchTwoRegisterWithImm16(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept;
#define DispatchCustomTwoRegisterWithImm16(title) EagerInstruction op ## title (TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept;
#define DispatchOneRegisterWithImm16(title) Core:: title op ## title (TargetRegister dest, QuarterAddress value) noexcept;
#define DispatchOneRegisterWithImm64(title) Core:: title op ## title (TargetRegister dest, Address addr) noexcept;
#define DispatchOneRegisterWithImm48(title) Core:: title op ## title (TargetRegister dest, Address addr) noexcept;
#define DispatchOneRegisterWithImm32(title) Core:: title op ## title (TargetRegister dest, HalfAddress addr) noexcept;
#define DispatchNoArguments(title) Core:: title op ## title () noexcept;
#define X(title, b) Core:: title op ## title (const Core:: b & x) noexcept ; \
	INDIRECTION(Dispatch, b)(title);
#define FirstX(title, b) X(title, b)
#include "InstructionData.def"
#undef DispatchNoArguments
#undef FirstX
#undef X
#undef DispatchOneRegister
#undef DispatchTwoRegister 
#undef DispatchThreeRegister
#undef DispatchSignedImm16
#undef DispatchImmediate24
#undef DispatchTwoRegisterWithImm16
#undef DispatchOneRegisterWithImm16
#undef DispatchFiveRegister
#undef DispatchFourRegister
#undef DispatchOneRegisterWithImm48
#undef DispatchOneRegisterWithImm32
#undef DispatchOneRegisterWithImm64
#undef DispatchCustomTwoRegisterWithImm16

Core::Move zeroRegister(TargetRegister reg) noexcept;
EagerInstruction useRegister(TargetRegister reg, EagerInstruction body) noexcept;
EagerInstruction label(const std::string&);
EagerInstruction opLoadImmediate16(TargetRegister r, QuarterAddress value);
EagerInstruction opLoadImmediate16(TargetRegister r, const std::string& name);
EagerInstruction opLoadImmediate32(TargetRegister r, const std::string& name);
EagerInstruction opLoadImmediate64(TargetRegister r, const std::string& name);
EagerInstruction opJumpAbsolute(const std::string& name);
EagerInstruction opJump(const std::string& name);
EagerInstruction opConditionalBranch(TargetRegister reg, const std::string& name);
EagerInstruction opPrintChar(char c);
EagerInstruction opPrintChar(const std::string& str);
EagerInstruction opIndirectLoad(TargetRegister dest, TargetRegister src = TargetRegister::X);
EagerInstruction opPushImmediate64(const Datum& value, TargetRegister sp = TargetRegister::SP);
EagerInstruction opPushImmediate64(Address value, TargetRegister sp = TargetRegister::SP);
EagerInstruction opSubroutineCall(Address value);
EagerInstruction opSemicolon();
inline auto opPopRegister(TargetRegister reg) noexcept -> decltype(opPopRegister(reg, TargetRegister::SP)) { 
    return opPopRegister(reg, TargetRegister::SP); 
}
inline auto opPushRegister(TargetRegister reg) noexcept -> decltype(opPushRegister(reg, TargetRegister::SP)) { 
    return opPushRegister(reg, TargetRegister::SP); 
}

EagerInstruction opStoreImmediate64(TargetRegister addr, Address value);
EagerInstruction opStoreImmediate64(TargetRegister addr, const std::string& value);
/**
 * load a specific address into X and a value into Temporary, then store temporary into X
 */
EagerInstruction opStoreImmediate64(Address addr, Address value);
EagerInstruction opStoreImmediate64(Address addr, const std::string& value);

/**
 * Do some logic to select the best instruction to store an immediate with
 */
EagerInstruction opStoreImmediate(TargetRegister addr, Address value);
EagerInstruction opStoreImmediate(Address addr, Address value);
EagerInstruction opLoadImmediate(TargetRegister addr, Address value);
EagerInstruction opEquals(TargetRegister destination, TargetRegister src, Address addr);
EagerInstruction opMemoryEquals(TargetRegister destination, TargetRegister src, Address memLoc);
EagerInstruction opSubroutineStackEmpty(TargetRegister dest);
EagerInstruction opSubroutineStackFull(TargetRegister dest);
EagerInstruction opParameterStackEmpty(TargetRegister dest);
EagerInstruction opParameterStackFull(TargetRegister dest);
inline auto opPopRegisterA() noexcept -> decltype(opPopRegister(TargetRegister::A)) { return opPopRegister(TargetRegister::A); }
inline auto opPopRegisterB() noexcept -> decltype(opPopRegister(TargetRegister::B)) { return opPopRegister(TargetRegister::B); }
inline auto opPopRegisterC() noexcept -> decltype(opPopRegister(TargetRegister::C)) { return opPopRegister(TargetRegister::C); }
inline auto opPushRegisterA() noexcept -> decltype(opPushRegister(TargetRegister::A)) { return opPushRegister(TargetRegister::A); }
inline auto opPushRegisterB() noexcept -> decltype(opPushRegister(TargetRegister::B)) { return opPushRegister(TargetRegister::B); }
inline auto opPushRegisterC() noexcept -> decltype(opPushRegister(TargetRegister::C)) { return opPushRegister(TargetRegister::C); }
inline auto opPopRegisterAB() noexcept -> EagerInstruction {
    return [](AssemblerBuilder& ab) {
        ab.addInstruction(opPopRegisterA(), 
                opPopRegisterB());
    };
}
inline auto opPopRegisterCAB() noexcept -> EagerInstruction {
    return [](AssemblerBuilder& ab) {
        ab.addInstruction(opPopRegisterC(),
                          opPopRegisterAB());
    };
}

EagerInstruction ifThenElseStatement(TargetRegister cond, Address onTrue, Address onFalse = Address(-1));
EagerInstruction opNotEquals(TargetRegister destination, TargetRegister src0, TargetRegister src1);

} // end namespace forth

#endif
