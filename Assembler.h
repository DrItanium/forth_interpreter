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
SizedResolvableLazyFunction opLoadImmediate16(TargetRegister r, const std::string& name);
SizedResolvableLazyFunction opLoadImmediate32(TargetRegister r, const std::string& name);
SizedResolvableLazyFunction opLoadImmediate48(TargetRegister r, const std::string& name);
SizedResolvableLazyFunction opLoadImmediate64(TargetRegister r, const std::string& name);
SizedResolvableLazyFunction opJumpAbsolute(const std::string& name);
SizedResolvableLazyFunction opJumpRelative(const std::string& name);
SizedResolvableLazyFunction opConditionalBranch(TargetRegister reg, const std::string& name);
EagerInstruction opPrintChar(char c);
EagerInstruction opPrintChar(const std::string& str);
EagerInstruction opIndirectLoad(TargetRegister dest, TargetRegister src = TargetRegister::X);
EagerInstruction opPushImmediate(const Datum& value, TargetRegister sp = TargetRegister::SP);
EagerInstruction opPushImmediate(Address value, TargetRegister sp = TargetRegister::SP);

inline auto opLoad(TargetRegister dest, TargetRegister src) noexcept -> decltype(opLoad({dest, src})) {
    return opLoad({dest, src});
}

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

} // end namespace forth

#endif
