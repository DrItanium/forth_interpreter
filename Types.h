#ifndef TYPES_H__
#define TYPES_H__
#include <cstdint>
#include <type_traits>
namespace forth {
    using Address = uint64_t;
    using HalfAddress = uint32_t;
    using QuarterAddress = uint16_t;
    using Integer = int64_t;
    using HalfInteger = int32_t;
    using QuarterInteger = int16_t;
    using Floating = double;
    using byte = uint8_t;
    static_assert(sizeof(Address) == sizeof(Integer), "Address and integer must be the same size!");
    static_assert(sizeof(Integer) == sizeof(Floating), "Integer and Floating must be the same size!");
    static_assert((sizeof(Address) / sizeof(QuarterAddress)) == 4, "Number of quarter addresses in an address is not 4!");
    static_assert((sizeof(Address) / sizeof(HalfAddress)) == 2, "Number of half addresses in an address is not 2!");

	template<typename T, typename R, T mask, T shift = 0>
	constexpr T encodeBits(T value, R newValue) noexcept {
		return (value & ~mask) | ((static_cast<T>(newValue) << shift) & mask);
	}

	template<typename T, typename R, T mask, T shift = 0>
	constexpr R decodeBits(T value) noexcept {
		return static_cast<R>((value & mask) >> shift);
	}

	constexpr byte getUpperHalf(byte input) noexcept {
		return decodeBits<byte, byte, 0xF0, 4>(input);
	}
	constexpr byte getLowerHalf(byte input) noexcept {
		return decodeBits<byte, byte, 0x0F, 0>(input);
	}
	constexpr byte setUpperHalf(byte input, byte value) noexcept {
		return encodeBits<byte, byte, 0xF0, 4>(input, value);
	}
	constexpr byte setLowerHalf(byte input, byte value) noexcept {
		return encodeBits<byte, byte, 0x0F, 0>(input, value);
	}

	constexpr byte getUpperHalf(QuarterAddress input) noexcept {
		return decodeBits<QuarterAddress, byte, 0xFF'00, 8>(input);
	}
	constexpr byte getLowerHalf(QuarterAddress input) noexcept {
		return decodeBits<QuarterAddress, byte, 0x00'FF, 0>(input);
	}
	constexpr QuarterAddress setUpperHalf(QuarterAddress input, byte value) noexcept {
		return encodeBits<QuarterAddress, byte, 0xFF'00, 8>(input, value);
	}
	constexpr QuarterAddress setLowerHalf(QuarterAddress input, byte value) noexcept {
		return encodeBits<QuarterAddress, QuarterAddress, 0x00FF, 0>(input, value);
	}

	template<typename T, typename R>
	constexpr R setLowerUpperHalves(T lower, T upper) noexcept {
		return setUpperHalf(setLowerHalf(R(0), lower), upper);
	}

	template<typename T, typename C = byte>
	constexpr bool legalValue(T r) noexcept {
		static_assert(std::is_enum<T>::value, "This is an enum value check!");
		static_assert(std::is_integral<C>::value, "C must cast to an integral type");
		return static_cast<C>(r) < static_cast<C>(T::Count);
	}
}

#endif // end TYPES_H__
