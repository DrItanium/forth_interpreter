// construct assembly instructions
#ifndef ASSEMBLER_H__
#define ASSEMBLER_H__

#include "Types.h"
#include "Instruction.h"
#include <type_traits>

namespace forth {
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

namespace Instruction {
	constexpr byte stop() noexcept { 
		return encodeSingleByteOperation(Operation::Stop); 
	}

	constexpr bool argumentsImplyCompactedForm(TargetRegister dest, TargetRegister src0, TargetRegister src1 = TargetRegister::B) noexcept {
		return (dest == TargetRegister::C && src0 == TargetRegister::A && src1 == TargetRegister::B);
	}
	constexpr HalfAddress xorl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Xor);
		} 
		return encodeThreeByte(Operation::XorFull, dest, src0, src1);
	}
	constexpr HalfAddress xorl(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::XorImmediate, dest, src0, value);
	}
	constexpr HalfAddress xoru(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedXor);
		} 
		return encodeThreeByte(Operation::UnsignedXorFull, dest, src0, src1);
	}
	constexpr HalfAddress xoriu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedXorImmediate, dest, src0, value);
	}
	constexpr HalfAddress xorb(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::BooleanXor);
		} 
		return encodeThreeByte(Operation::BooleanXorFull, dest, src0, src1);
	}
	constexpr HalfAddress orl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Or);
		} 
		return encodeThreeByte(Operation::OrFull, dest, src0, src1);
	}
	constexpr HalfAddress orl(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::OrImmediate, dest, src0, value);
	}
	constexpr HalfAddress oru(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedOr);
		} 
		return encodeThreeByte(Operation::UnsignedOrFull, dest, src0, src1);
	}
	constexpr HalfAddress oriu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedOrImmediate, dest, src0, value);
	}
	constexpr HalfAddress orb(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::BooleanOr);
		} 
		return encodeThreeByte(Operation::BooleanOrFull, dest, src0, src1);
	}
	constexpr HalfAddress andl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::And);
		} 
		return encodeThreeByte(Operation::AndFull, dest, src0, src1);
	}
	constexpr HalfAddress andl(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::AndImmediate, dest, src0, value);
	}
	constexpr HalfAddress andu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedAnd);
		} 
		return encodeThreeByte(Operation::UnsignedAndFull, dest, src0, src1);
	}
	constexpr HalfAddress andiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedAndImmediate, dest, src0, value);
	}
	constexpr HalfAddress andb(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::BooleanAnd);
		} 
		return encodeThreeByte(Operation::BooleanAndFull, dest, src0, src1);
	}
	constexpr QuarterAddress minusl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
		if (argumentsImplyCompactedForm(dest, src0)) {
			return encodeSingleByteOperation(Operation::Minus);
		} 
		return encodeTwoByte(Operation::MinusFull, dest, src0);
	}
	constexpr QuarterAddress minuslu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
		if (argumentsImplyCompactedForm(dest, src0)) {
			return encodeSingleByteOperation(Operation::UnsignedMinus);
		} 
		return encodeTwoByte(Operation::UnsignedMinusFull, dest, src0);
	}
	constexpr QuarterAddress minusf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
		if (argumentsImplyCompactedForm(dest, src0)) {
			return encodeSingleByteOperation(Operation::FloatingPointMinus);
		} 
		return encodeTwoByte(Operation::FloatingPointMinusFull, dest, src0);
	}
	constexpr QuarterAddress notl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
		if (argumentsImplyCompactedForm(dest, src0)) {
			return encodeSingleByteOperation(Operation::Not);
		} 
		return encodeTwoByte(Operation::NotFull, dest, src0);
	}
	constexpr QuarterAddress notlu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
		if (argumentsImplyCompactedForm(dest, src0)) {
			return encodeSingleByteOperation(Operation::UnsignedNot);
		} 
		return encodeTwoByte(Operation::UnsignedNotFull, dest, src0);
	}
	constexpr QuarterAddress notb(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A) noexcept {
		if (argumentsImplyCompactedForm(dest, src0)) {
			return encodeSingleByteOperation(Operation::BooleanNot);
		} 
		return encodeTwoByte(Operation::BooleanNotFull, dest, src0);
	}
	constexpr HalfAddress pow(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Pow);
		}
		return encodeThreeByte(Operation::PowFull, dest, src0, src1);
	}
	constexpr HalfAddress powf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::FloatingPointPow);
		}
		return encodeThreeByte(Operation::FloatingPointPowFull, dest, src0, src1);
	}
	constexpr HalfAddress powu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedPow);
		}
		return encodeThreeByte(Operation::UnsignedPowFull, dest, src0, src1);
	}

	constexpr HalfAddress cmpeq(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Equals);
		} 
		return encodeThreeByte(Operation::EqualsFull, dest, src0, src1);
	}
	constexpr HalfAddress cmpeq(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::EqualsImmediate, dest, src0, value);
	}
	constexpr HalfAddress cmpeqf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::FloatingPointEquals);
		} 
		return encodeThreeByte(Operation::FloatingPointEqualsFull, dest, src0, src1);
	}
	constexpr HalfAddress cmpequ(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedEquals);
		} 
		return encodeThreeByte(Operation::UnsignedEqualsFull, dest, src0, src1);
	}
	constexpr HalfAddress cmpeqiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedEqualsImmediate, dest, src0, value);
	}
	constexpr HalfAddress cmpeqb(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::BooleanEquals);
		} 
		return encodeThreeByte(Operation::BooleanEqualsFull, dest, src0, src1);
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
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::LessThan);
		} 
		return encodeThreeByte(Operation::LessThanFull, dest, src0, src1);
	}
	constexpr HalfAddress cmplt(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::LessThanImmediate, dest, src0, value);
	}
	constexpr HalfAddress cmpltf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::FloatingPointLessThan);
		} 
		return encodeThreeByte(Operation::FloatingPointLessThanFull, dest, src0, src1);
	}
	constexpr HalfAddress cmpltu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedLessThan);
		} 
		return encodeThreeByte(Operation::UnsignedLessThanFull, dest, src0, src1);
	}
	constexpr HalfAddress cmpltiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedLessThanImmediate, dest, src0, value);
	}


	constexpr HalfAddress cmpgt(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::GreaterThan);
		} 
		return encodeThreeByte(Operation::GreaterThanFull, dest, src0, src1);
	}
	constexpr HalfAddress cmpgt(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::GreaterThanImmediate, dest, src0, value);
	}
	constexpr HalfAddress cmpgtf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::FloatingPointGreaterThan);
		} 
		return encodeThreeByte(Operation::FloatingPointGreaterThanFull, dest, src0, src1);
	}
	constexpr HalfAddress cmpgtu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedGreaterThan);
		} 
		return encodeThreeByte(Operation::UnsignedGreaterThanFull, dest, src0, src1);
	}
	constexpr HalfAddress cmpgtiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedGreaterThanImmediate, dest, src0, value);
	}
	constexpr HalfAddress shr(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::ShiftRight);
		} 
		return encodeThreeByte(Operation::ShiftRightFull, dest, src0, src1);
	}
	constexpr HalfAddress shr(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::ShiftRightImmediate, dest, src0, value);
	}
	constexpr HalfAddress shru(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedShiftRight);
		} 
		return encodeThreeByte(Operation::UnsignedShiftRightFull, dest, src0, src1);
	}
	constexpr HalfAddress shriu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedShiftRightImmediate, dest, src0, value);
	}
	constexpr HalfAddress shl(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::ShiftLeft);
		} 
		return encodeThreeByte(Operation::ShiftLeftFull, dest, src0, src1);
	}
	constexpr HalfAddress shl(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::ShiftLeftImmediate, dest, src0, value);
	}
	constexpr HalfAddress shlu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedShiftLeft);
		} 
		return encodeThreeByte(Operation::UnsignedShiftLeftFull, dest, src0, src1);
	}
	constexpr HalfAddress shliu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedShiftLeftImmediate, dest, src0, value);
	}
	constexpr HalfAddress add(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Add);
		} 
		return encodeThreeByte(Operation::AddFull, dest, src0, src1);
	}
	constexpr HalfAddress add(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::AddImmediate, dest, src0, value);
	}
	constexpr HalfAddress addf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::FloatingPointAdd);
		} 
		return encodeThreeByte(Operation::FloatingPointAddFull, dest, src0, src1);
	}
	constexpr HalfAddress addu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedAdd);
		} 
		return encodeThreeByte(Operation::UnsignedAddFull, dest, src0, src1);
	}
	constexpr HalfAddress addiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedAddImmediate, dest, src0, value);
	}
	constexpr HalfAddress sub(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Subtract);
		} 
		return encodeThreeByte(Operation::SubtractFull, dest, src0, src1);
	}
	constexpr HalfAddress sub(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::SubtractImmediate, dest, src0, value);
	}
	constexpr HalfAddress subf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::FloatingPointSubtract);
		} 
		return encodeThreeByte(Operation::FloatingPointSubtractFull, dest, src0, src1);
	}
	constexpr HalfAddress subu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedSubtract);
		} 
		return encodeThreeByte(Operation::UnsignedSubtractFull, dest, src0, src1);
	}
	constexpr HalfAddress subiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedSubtractImmediate, dest, src0, value);
	}
	constexpr HalfAddress mul(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Multiply);
		} 
		return encodeThreeByte(Operation::MultiplyFull, dest, src0, src1);
	}
	constexpr HalfAddress mul(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::MultiplyImmediate, dest, src0, value);
	}
	constexpr HalfAddress mulf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::FloatingPointMultiply);
		} 
		return encodeThreeByte(Operation::FloatingPointMultiplyFull, dest, src0, src1);
	}
	constexpr HalfAddress mulu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedMultiply);
		} 
		return encodeThreeByte(Operation::UnsignedMultiplyFull, dest, src0, src1);
	}
	constexpr HalfAddress muliu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedMultiplyImmediate, dest, src0, value);
	}
	constexpr HalfAddress div(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Divide);
		} 
		return encodeThreeByte(Operation::DivideFull, dest, src0, src1);
	}
	constexpr HalfAddress div(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::DivideImmediate, dest, src0, value);
	}
	constexpr HalfAddress divf(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::FloatingPointDivide);
		} 
		return encodeThreeByte(Operation::FloatingPointDivideFull, dest, src0, src1);
	}
	constexpr HalfAddress divu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedDivide);
		} 
		return encodeThreeByte(Operation::UnsignedDivideFull, dest, src0, src1);
	}
	constexpr HalfAddress diviu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedDivideImmediate, dest, src0, value);
	}
	constexpr HalfAddress mod(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::Modulo);
		} 
		return encodeThreeByte(Operation::ModuloFull, dest, src0, src1);
	}
	constexpr HalfAddress mod(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::ModuloImmediate, dest, src0, value);
	}
	constexpr HalfAddress modu(TargetRegister dest = TargetRegister::C, TargetRegister src0 = TargetRegister::A, TargetRegister src1 = TargetRegister::B) noexcept {
		if (argumentsImplyCompactedForm(dest, src0, src1)) {
			return encodeSingleByteOperation(Operation::UnsignedModulo);
		} 
		return encodeThreeByte(Operation::UnsignedModuloFull, dest, src0, src1);
	}
	constexpr HalfAddress modiu(TargetRegister dest, TargetRegister src0, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::UnsignedModuloImmediate, dest, src0, value);
	}
    constexpr QuarterAddress popRegister(TargetRegister destination, TargetRegister sp = TargetRegister::SP) noexcept {
		if (sp == TargetRegister::SP) {
			switch(destination) {
				case TargetRegister::A:
					return QuarterAddress(encodeSingleByteOperation(Operation::PopA));
				case TargetRegister::B:
					return QuarterAddress(encodeSingleByteOperation(Operation::PopB));
				case TargetRegister::C:
					return QuarterAddress(encodeSingleByteOperation(Operation::PopC));
				default:
					return encodeTwoByte(Operation::PopRegister, encodeRegisterPair(destination, sp));
			}
		} else {
        	return encodeTwoByte(Operation::PopRegister, encodeRegisterPair(destination, sp));
		}
    }
    constexpr QuarterAddress pushRegister(TargetRegister value, TargetRegister sp = TargetRegister::SP) noexcept {
		if (sp == TargetRegister::SP) {
			switch(value) {
				case TargetRegister::A:
					return QuarterAddress(encodeSingleByteOperation(Operation::PushA));
				case TargetRegister::B:
					return QuarterAddress(encodeSingleByteOperation(Operation::PushB));
				case TargetRegister::C:
					return QuarterAddress(encodeSingleByteOperation(Operation::PushC));
				default:
					return encodeTwoByte(Operation::PushRegister, encodeRegisterPair(sp, value));
			}
		} else {
        	return encodeTwoByte(Operation::PushRegister, encodeRegisterPair(sp, value));
		}
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
	constexpr HalfAddress setImmediate16_Lower(TargetRegister dest, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::SetImmediate16_Lower, dest, value);  
	}
	constexpr HalfAddress setImmediate16_Lower(TargetRegister dest, QuarterAddressWrapper value) noexcept {
		return encodeFourByte(Operation::SetImmediate16_Lower, dest, value.get());  
	}
	constexpr HalfAddress setImmediate16_Lowest(TargetRegister dest, QuarterAddress value) noexcept {
		return encodeFourByte(Operation::SetImmediate16_Lowest, dest, value); 
	}
	constexpr HalfAddress setImmediate16_Lowest(TargetRegister dest, QuarterAddressWrapper value) noexcept {
		return encodeFourByte(Operation::SetImmediate16_Lowest, dest, value.get()); 
	}
    constexpr HalfAddress setImmediate16_Higher(TargetRegister dest, QuarterAddress value) noexcept { 
		return encodeFourByte(Operation::SetImmediate16_Higher, dest, value); 
	}
	constexpr HalfAddress setImmediate16_Higher(TargetRegister dest, QuarterAddressWrapper value) noexcept {
		return encodeFourByte(Operation::SetImmediate16_Lowest, dest, value.get()); 
	}
    constexpr HalfAddress setImmediate16_Highest(TargetRegister dest, QuarterAddress value) noexcept { 
		return encodeFourByte(Operation::SetImmediate16_Highest, dest, value); 
	}
    constexpr HalfAddress setImmediate16_Highest(TargetRegister dest, QuarterAddressWrapper value) noexcept { 
		return encodeFourByte(Operation::SetImmediate16_Highest, dest, value.get()); 
	}
    constexpr HalfAddress setImmediate64_Lowest(TargetRegister dest, Address value) noexcept { 
        return setImmediate16_Lowest(dest, getLowestQuarter(value));
    }
    constexpr HalfAddress setImmediate64_Lower(TargetRegister dest, Address value) noexcept { 
        return setImmediate16_Lower(dest, getLowerQuarter(value));
    }
    constexpr HalfAddress setImmediate64_Higher(TargetRegister dest, Address value) noexcept { 
        return setImmediate16_Higher(dest, getHigherQuarter(value));
    }
    constexpr HalfAddress setImmediate64_Highest(TargetRegister dest, Address value) noexcept { 
        return setImmediate16_Highest(dest, getHighestQuarter(value));
    }
	static_assert(0xFFFF0419 == setImmediate64_Highest(TargetRegister::X, 0xFFFF'FFFF'FFFF'0001), "Encoding is wrong!");

    template<Address mask, Address shift>
    constexpr Address encodeByte(byte value, Address target = 0) noexcept {
        return encodeBits<Address, byte, mask, shift>(target, value);
    }
    template<Address mask, Address shift>
    constexpr Address encodeQuarterAddress(QuarterAddress value, Address target = 0) noexcept {
        return encodeBits<Address, QuarterAddress, mask, shift>(target, value);
    }
	template<Address mask, Address shift>
	constexpr Address encodeHalfAddress(HalfAddress value, Address target = 0) noexcept {
		return encodeBits<Address, HalfAddress, mask, shift>(target, value);
	}
	template<Address mask, Address shift>
	constexpr Address encodeAddress(Address value, Address target = 0) noexcept {
		return encodeBits<Address, Address, mask, shift>(target, value);
	}
	template<byte startOffset>
	constexpr Address encodeThreeByteAddress(HalfAddress value, Address target = 0) noexcept {
		static_assert(startOffset < 6, "Illegal three byte address!");
		return encodeHalfAddress<Address(0x0000'0000'00FF'FFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeFourByteAddress(HalfAddress value, Address target = 0) noexcept {
		static_assert(startOffset < 5, "Illegal half address start address");
		return encodeHalfAddress<Address(0x0000'0000'FFFF'FFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeFiveByteAddress(Address value, Address target = 0) noexcept {
		static_assert(startOffset < 4, "Illegal address start address");
		return encodeAddress<Address(0x0000'00FF'FFFF'FFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeSixByteAddress(Address value, Address target = 0) noexcept {
		static_assert(startOffset < 3, "Illegal address start address");
		return encodeAddress<Address(0x0000'FFFF'FFFF'FFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeSevenByteAddress(Address value, Address target = 0) noexcept {
		static_assert(startOffset < 2, "Illegal address start address");
		return encodeAddress<Address(0x00FF'FFFF'FFFF'FFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeEightByteAddress(Address value, Address target = 0) noexcept {
		static_assert(startOffset < 1, "Illegal address start address");
		return encodeAddress<Address(0xFFFFFFFFFFFFFFFF) << (startOffset * 8), startOffset * 8>(value, target);
	}

    template<byte startOffset>
    constexpr Address encodeQuarterOperation(QuarterAddress value, Address target = 0) noexcept {
        static_assert(startOffset < 7, "Illegal quarter address start address");
        return encodeQuarterAddress<Address(0xFFFF)<< (startOffset * 8), startOffset * 8>(value, target);
    }
    template<byte startOffset>
    constexpr Address encodeByteOperation(byte value, Address target = 0) noexcept {
		if constexpr (startOffset < 8) {
        	return encodeByte<Address(0xFF) << Address (startOffset * 8), startOffset * 8>(value, target);
		} else {
        	static_assert(startOffset < 8, "Illegal byte offset start address!");
			return target;
		}
    }
	template<byte startOffset>
	constexpr Address encodeOperation(byte value, Address target = 0) noexcept {
		return encodeByteOperation<startOffset>(value, target);
	}
	template<byte startOffset>
	constexpr Address encodeOperation(QuarterAddress value, Address target = 0) noexcept {
		if (auto width = getInstructionWidth(value); width == 1) {
			return encodeByteOperation<startOffset>(static_cast<byte>(value), target);
		} else if (width == 2) {
			return encodeQuarterOperation<startOffset>(value, target);
		} else {
			// skip the contents
			return target;
		}
	}
	template<byte startOffset>
	constexpr Address encodeOperation(QuarterAddressWrapper value, Address target = 0) noexcept {
		return encodeOperation<startOffset>(value.get(), target);
	}
	template<byte startOffset>
	constexpr Address encodeOperation(HalfAddress value, Address target = 0) noexcept {
		if (auto width = getInstructionWidth(value); width == 1) {
			return encodeOperation<startOffset>((byte)value, target);
		} else if (width == 2) {
			return encodeOperation<startOffset>(static_cast<QuarterAddress>(value), target);
		} else if (width == 3) {
			return encodeThreeByteAddress<startOffset>(value, target);
		} else if (width == 4) {
			return encodeFourByteAddress<startOffset>(value, target);
		} else {
			// skip the contents
			return target;
		}
	}
	template<byte startOffset>
	constexpr Address encodeOperation(HalfAddressWrapper value, Address target = 0) noexcept {
		return encodeOperation<startOffset>(value.get(), target);
	}

	template<byte startOffset, Address target, auto value>
	constexpr Address compileSingleOperation() noexcept {
		if constexpr (std::is_same<decltype(value), QuarterAddressWrapper>::value ||
				      std::is_same<decltype(value), HalfAddressWrapper>::value) {
			return compileSingleOperation<startOffset, target, value.get()>();
		} else {
			if constexpr (constexpr auto width = getInstructionWidth(value); width == 1) {
				return encodeOperation<startOffset>((byte)value, target);
			} else if constexpr (width == 2) {
				return encodeOperation<startOffset>(static_cast<QuarterAddress>(value), target);
			} else if constexpr (width == 3) {
				return encodeThreeByteAddress<startOffset>(value, target);
			} else if constexpr (width == 4) {
				return encodeFourByteAddress<startOffset>(value, target);
			} else if constexpr (width == 5) {
				return encodeFiveByteAddress<startOffset>(value, target);
			} else if constexpr (width == 6) {
				return encodeSixByteAddress<startOffset>(value, target);
			} else if constexpr (width == 7) {
				return encodeSevenByteAddress<startOffset>(value, target);
			} else if constexpr (width == 8) {
				return encodeEightByteAddress<startOffset>(value, target);
			} else {
				// skip
				return target;
			}
		}
	}

    template<byte startOffset>
    constexpr Address encodeOperation(Address value, Address target = 0) noexcept {
        if (auto width = getInstructionWidth(value); width < 5) {
            return encodeOperation<startOffset>(HalfAddress(value), target);
        } else if (width == 5) {
            return encodeFiveByteAddress<startOffset>(value, target);
        } else if (width == 6) {
            return encodeSixByteAddress<startOffset>(value, target);
        } else if (width == 7) {
            return encodeSevenByteAddress<startOffset>(value, target);
        } else if (width == 8) {
            return encodeEightByteAddress<startOffset>(value, target);
        } else {
            // skip
            return target;
        }
    }



    template<byte offset, typename T>
    constexpr Address encodeOperation(Address curr, T first) noexcept {
        static_assert(offset < 8, "Too many fields provided!");
        return encodeOperation<offset>(first, curr);
    }
    template<byte offset, typename T, typename ... Args>
    constexpr Address encodeOperation(Address curr, T first, Args&& ... rest) noexcept {
        static_assert(offset < 8, "Too many fields provided!");
		auto encoded = encodeOperation<offset>(first, curr);
		if constexpr (std::is_same<T, HalfAddress>::value || std::is_same<T, HalfAddressWrapper>::value) {
			switch (getInstructionWidth(first)) {
				case 1: return encodeOperation<offset + 1, Args...>(encoded, std::move(rest)...);
				case 2: return encodeOperation<offset + 2, Args...>(encoded, std::move(rest)...);
				case 3: return encodeOperation<offset + 3, Args...>(encoded, std::move(rest)...);
				case 4: 
						return encodeOperation<offset + 4, Args...>(encoded, std::move(rest)...);
				default:
					return encodeOperation<offset, Args...>(curr, std::move(rest)...);
			}
        } else if constexpr (std::is_same<T, Address>::value) {
			switch (getInstructionWidth(first)) {
				case 1: return encodeOperation<offset + 1, Args...>(encoded, std::move(rest)...);
				case 2: return encodeOperation<offset + 2, Args...>(encoded, std::move(rest)...);
				case 3: return encodeOperation<offset + 3, Args...>(encoded, std::move(rest)...);
				case 4: return encodeOperation<offset + 4, Args...>(encoded, std::move(rest)...);
				case 5: return encodeOperation<offset + 5, Args...>(encoded, std::move(rest)...);
				case 6: return encodeOperation<offset + 6, Args...>(encoded, std::move(rest)...);
				case 7: return encodeOperation<offset + 7, Args...>(encoded, std::move(rest)...);
				case 8: return encodeOperation<offset + 8, Args...>(encoded, std::move(rest)...);
				default:
					return encodeOperation<offset, Args...>(curr, std::move(rest)...);
			}
		} else if constexpr (std::is_same<T, QuarterAddress>::value || std::is_same<T, QuarterAddressWrapper>::value) {
			switch (getInstructionWidth(first)) {
				case 1:
					return encodeOperation<offset + 1, Args...>(encoded, std::move(rest)...);
				case 2:
					return encodeOperation<offset + 2, Args...>(encoded, std::move(rest)...);
				default:
					return encodeOperation<offset, Args...>(curr, std::move(rest)...);
			} 
		} else {
			static_assert(sizeof(T) == 1, "Should only get bytes through here!");
        	return encodeOperation<offset + sizeof(T), Args...>(encoded, std::move(rest)...);
		}
    }
    template<byte offset, Address curr, auto first, auto ... rest>
    constexpr Address compileOperation() noexcept {
        static_assert(offset < 8, "Too many fields provided!");
		if constexpr (sizeof...(rest) > 0) {
			return compileOperation<offset + getInstructionWidth(first), compileSingleOperation<offset, curr, first>(), rest...>();
		} else {
			return compileSingleOperation<offset, curr, first>();
		}
    }
	template<auto first, auto ... rest>
	constexpr Address encodeOperation() noexcept {
		return compileOperation<0, 0u, first, rest...>();
	}
	template<auto first, auto ... rest>
	constexpr Address preCompileOperation() noexcept {
		return encodeOperation<first, rest...>();
	}
    template<typename T, typename ... Args>
    constexpr Address encodeOperation(T first, Args&& ... rest) noexcept {
        return encodeOperation<0, T, Args...>(0, first, std::move(rest)...);
    }
    constexpr size_t operationLength(byte b) noexcept { return getInstructionWidth(static_cast<Operation>(b)); }
    constexpr size_t operationLength(QuarterAddress b) noexcept { return getInstructionWidth(byte(b & 0xFF)); }
    constexpr size_t operationLength(HalfAddress b) noexcept { return getInstructionWidth(byte(b & 0xFF)); }
    constexpr size_t operationLength(Address b) noexcept { return getInstructionWidth(byte(b & 0xFF)); }

    template<typename T, typename ... Rest>
    constexpr size_t operationLength(T first, Rest ... rest) noexcept {
        if constexpr (sizeof...(rest) > 0) {
            return operationLength(first) + operationLength(std::move(rest)...);
        } else {
            return operationLength(first);
        }
    }
	constexpr QuarterAddress notOp(TargetRegister dest, TargetRegister src) noexcept {
		return encodeTwoByte(Operation::NotFull, dest, src);
	}
	constexpr QuarterAddress minus(TargetRegister dest, TargetRegister src) noexcept {
		return encodeTwoByte(Operation::MinusFull, dest, src);
	}

	constexpr Address loadAddressLowerHalf(TargetRegister reg, Address value) noexcept {
		return Instruction::encodeOperation(
				Instruction::setImmediate64_Lowest(reg, value),
				Instruction::setImmediate64_Lower(reg, value));
	}
	constexpr Address loadAddressUpperHalf(TargetRegister reg, Address value) noexcept {
		return Instruction::encodeOperation(
				Instruction::setImmediate64_Higher(reg, value),
				Instruction::setImmediate64_Highest(reg, value));
	}
    constexpr QuarterAddress increment(TargetRegister reg, byte imm4) noexcept {
        return encodeTwoByte(Operation::Increment, reg, imm4);
    }
    constexpr QuarterAddress decrement(TargetRegister reg, byte imm4) noexcept {
        return encodeTwoByte(Operation::Decrement, reg, imm4);
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
		return encodeBits<Address, Address, 0xFFFFFFFFFFFF0000, 16>(
				encodeBits<Address, byte, 0x000000000000FF00, 8>(
					encodeBits<Address, byte, 0x00000000000000FF, 0>(0,
						static_cast<byte>(Operation::LoadImmediateLower48)),
					encodeDestinationRegister(dest)),
				value);
	}
	constexpr byte popA() noexcept { return popRegister(TargetRegister::A, TargetRegister::SP); }
	constexpr byte popB() noexcept { return popRegister(TargetRegister::B, TargetRegister::SP); }
	constexpr byte popC() noexcept { return popRegister(TargetRegister::C, TargetRegister::SP); }
	constexpr byte pushA() noexcept { return pushRegister(TargetRegister::A, TargetRegister::SP); }
	constexpr byte pushB() noexcept { return pushRegister(TargetRegister::B, TargetRegister::SP); }
	constexpr byte pushC() noexcept { return pushRegister(TargetRegister::C, TargetRegister::SP); }
    constexpr QuarterAddress popAB() noexcept {
        return (QuarterAddress)encodeOperation(popA(), popB());
    }
    constexpr QuarterAddress swapAB() noexcept {
        return swap(TargetRegister::B, TargetRegister::A);
    }
	constexpr auto zeroRegister(TargetRegister reg) noexcept -> decltype(xorl(reg, reg, reg)) {
		return xorl(reg, reg, reg);
	}
    constexpr QuarterAddress imm16TestValue = 0xfded;
	static_assert(popA() == popRegister(TargetRegister::A, TargetRegister::SP), "Two different code paths for popA should yield the same result!");
	static_assert(getInstructionWidth(cmpeq()) == 1, "Compare eq should only be one byte if the args are defaulted");
	static_assert(operationLength(popA(), popB(), cmpeq(), notOp(TargetRegister::C, TargetRegister::C), pushC()) < 8, "This instruction sequence is incorrectly encoded!");
	static_assert(getInstructionWidth(cmpeq()) == 1, "Compare eq should only be one byte if the args are defaulted");
	static_assert(byte(0xFD) == getUpperHalf(imm16TestValue), "getUpperHalf is not working correctly!");
	static_assert(byte(0xED) == getLowerHalf(imm16TestValue), "getUpperHalf is not working correctly!");
	static_assert(byte(0x00) == byte(TargetRegister::A), "register cast assumptions broken!");
	static_assert(byte(0x16) == byte(Operation::SetImmediate16_Lowest), "Operation index is no longer correct!");
	static_assert(Address(0xFDED0016) == encodeFourByte(Operation::SetImmediate16_Lowest, TargetRegister::A, imm16TestValue), "Encoding is broken!");
    static_assert(Address(0xFDED0016) == setImmediate16_Lowest(TargetRegister::A, imm16TestValue), "setImmediate16_Lowest is broken!");
    static_assert(Address(0xFDED0017) == setImmediate16_Lower(TargetRegister::A, imm16TestValue), "setImmediate16_Lower is broken!");
    static_assert(Address(0xFDED0018) == setImmediate16_Higher(TargetRegister::A, imm16TestValue), "setImmediate16_Higher is broken!");
    static_assert(Address(0xFDED0019) == setImmediate16_Highest(TargetRegister::A, imm16TestValue), "setImmediate16_Highest is broken!");
	static_assert(getInstructionWidth(mulf(TargetRegister::A, TargetRegister::A, TargetRegister::A)) == 3, "FloatingPointMultiplyFull is not three bytes!");
	static_assert(getInstructionWidth(mulf()) == 1, "FloatingPointMultiplyFull is not three bytes!");
} // end namespace Instruction
} // end namespace forth
static_assert(sizeof(unsigned long long int) >= sizeof(forth::Address), "Unsigned long long int is a 64-bit value or greater!");
constexpr forth::QuarterAddressWrapper operator "" _qlowest(unsigned long long int addr) {  return forth::getLowestQuarter(forth::Address(addr)); }
constexpr forth::QuarterAddressWrapper operator "" _qlower(unsigned long long int addr) {   return forth::getLowerQuarter(forth::Address(addr)); }
constexpr forth::QuarterAddressWrapper operator "" _qhigher(unsigned long long int addr) {  return forth::getHigherQuarter(forth::Address(addr)); }
constexpr forth::QuarterAddressWrapper operator "" _qhighest(unsigned long long int addr) { return forth::getHighestQuarter(forth::Address(addr)); }

constexpr forth::HalfAddressWrapper operator "" _hupper(unsigned long long int addr) {  return forth::getUpperHalf(forth::Address(addr)); }
constexpr forth::HalfAddressWrapper operator "" _hlower(unsigned long long int addr) { return forth::getLowerHalf(forth::Address(addr)); }

#endif
