// concept of an internal machine instruction
#ifndef INSTRUCTION_H__
#define INSTRUCTION_H__
#include "Types.h"
#include "Problem.h"
#include <iostream>
#include <list>
#include <tuple>
#include <variant>
#include <optional>
namespace forth {

enum class TargetRegister : byte {
#define RegisterFirst(x) x = 0,
#define Register(x) x,
#include "Registers.def"
#undef Register
#undef RegisterFirst
    Count,
};
std::ostream& operator<<(std::ostream& out, const TargetRegister& d);
std::ostream& operator<<(std::ostream& out, const std::optional<TargetRegister>& d);
using OptionalRegister = std::optional<TargetRegister>;
static_assert(byte(TargetRegister::Count) <= 16, "Too many registers defined!");
constexpr byte encodeDestinationRegister(byte value) noexcept {
    return value & 0x0F;
}
byte encodeDestinationRegister(const OptionalRegister& value);
constexpr byte encodeSourceRegister(byte value) noexcept {
    return (value << 4) & 0xF0;
}
byte encodeSourceRegister(const OptionalRegister& reg);

template<typename Dest, typename Src>
byte encodeRegisterPair(Dest dest, Src src) {
    return encodeSourceRegister(src) |
           encodeDestinationRegister(src);
}
template<byte c>
struct SizedType {
    constexpr SizedType() { }
    constexpr byte size() noexcept { return c; }
};
struct ZeroByteInstruction final : SizedType<0> { };
struct OneByteInstruction final : SizedType<1> { };
struct TwoByteInstruction final : SizedType<2> { };
struct ThreeByteInstruction final : SizedType<3> { };
struct FourByteInstruction final : SizedType<4> { };
struct SixByteInstruction final : SizedType<6> { };
struct TenByteInstruction final : SizedType<10> {  };
struct InstructionWidth {
	using Args = std::variant<ZeroByteInstruction, OneByteInstruction, TwoByteInstruction, ThreeByteInstruction, FourByteInstruction, SixByteInstruction, TenByteInstruction>;
	Args contents;
	constexpr InstructionWidth() { }
	template<typename T>
	constexpr InstructionWidth(T&& arg) noexcept : contents(std::move(arg)) { }
	constexpr byte size() noexcept {
		return std::visit([](auto&& v) { return v.size(); }, contents);
	}
};
enum class Opcode : byte {
#define X(title, b) title,
#define FirstX(title, b) X(title, b)
#include "InstructionData.def"
#undef FirstX
#undef X
	Count,
};
static_assert(byte(Opcode::Count) <= 256, "Too many opcodes defined!");



constexpr InstructionWidth determineInstructionWidth(Opcode op) noexcept {
	switch (op) {
#define DispatchOneRegister(title) return TwoByteInstruction() ;
#define DispatchTaggedOneRegister(title) return TwoByteInstruction() ;
#define DispatchTwoRegister(title) return  TwoByteInstruction() ;
#define DispatchThreeRegister(title) return  ThreeByteInstruction() ;
#define DispatchSignedImm16(title) return  FourByteInstruction() ;
#define DispatchImmediate24(title) return  FourByteInstruction() ;
#define DispatchTwoRegisterWithImm16(title) return  FourByteInstruction();
#define DispatchCustomTwoRegisterWithImm16(title) return  FourByteInstruction();
#define DispatchOneRegisterWithImm16(title) return  FourByteInstruction();
#define DispatchOneRegisterWithImm64(title) return  TenByteInstruction();
#define DispatchOneRegisterWithImm32(title) return  SixByteInstruction();
#define DispatchNoArguments(title) return  OneByteInstruction();
#define X(title, k) case Opcode :: title: INDIRECTION(Dispatch, k)(title)
#define FirstX(title, k) X(title, k)
#include "InstructionData.def"
#undef FirstX
#undef X
#undef DispatchCustomTwoRegisterWithImm16
#undef DispatchNoArguments
#undef DispatchOneRegister
#undef DispatchTaggedOneRegister
#undef DispatchTwoRegister
#undef DispatchThreeRegister
#undef DispatchSignedImm16
#undef DispatchImmediate24
#undef DispatchTwoRegisterWithImm16
#undef DispatchOneRegisterWithImm16
#undef DispatchOneRegisterWithImm32
#undef DispatchOneRegisterWithImm64
		default:
			return ZeroByteInstruction();
	}
}
template<typename T>
InstructionWidth determineInstructionWidth(T value) {
    if constexpr (std::is_enum<T>::value) {
        return determineInstructionWidth(value);
    } else {
        return determineInstructionWidth(value.getOpcode());
    }
}

template<typename T, typename ... Rest>
Address computeInstructionWidth(T value, Rest&& ... rest) {
    auto combine = Address(std::visit([](auto&& value) { return value.size(); }, determineInstructionWidth(value)));
    if constexpr (sizeof...(rest) > 0) {
        combine += computeInstructionWidth(std::move(rest)...);
    } 
    return combine;
}

constexpr TargetRegister getDestinationRegister(byte field) noexcept { 
	return TargetRegister(getLowerHalf(field));
}
constexpr TargetRegister getSourceRegister(byte field) noexcept { 
	return TargetRegister(getUpperHalf(field));
}
template<typename T>
byte getInstructionWidth(T opcode) {
    return std::visit([](auto&& value) { return value.size(); }, determineInstructionWidth(opcode));
}



} // end namespace forth
#endif // end INSTRUCTION_H__
