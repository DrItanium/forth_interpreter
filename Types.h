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

	constexpr byte getLowestPart(HalfAddress input) noexcept {
		return decodeBits<HalfAddress, byte, 0x0000'00FF, 0>(input);
	}
	constexpr byte getLowerPart(HalfAddress input) noexcept {
		return decodeBits<HalfAddress, byte, 0x0000'FF00, 8>(input);
	}
	constexpr byte getHigherPart(HalfAddress input) noexcept {
		return decodeBits<HalfAddress, byte, 0x00FF'0000, 16>(input);
	}
	constexpr byte getHighestPart(HalfAddress input) noexcept {
		return decodeBits<HalfAddress, byte, 0xFF00'0000, 24>(input);
	}
	constexpr HalfAddress setLowestPart(HalfAddress input, byte value) noexcept {
		return encodeBits<HalfAddress, byte, 0x000000FF, 0>(input, value);
	}
	constexpr HalfAddress setLowerPart(HalfAddress input, byte value) noexcept {
		return encodeBits<HalfAddress, byte, 0x0000FF00, 8>(input, value);
	}
	constexpr HalfAddress setHigherPart(HalfAddress input, byte value) noexcept {
		return encodeBits<HalfAddress, byte, 0x00FF0000, 16>(input, value);
	}
	constexpr HalfAddress setHighestPart(HalfAddress input, byte value) noexcept {
		return encodeBits<HalfAddress, byte, 0xFF000000, 24>(input, value);
	}
	constexpr QuarterAddress getUpperHalf(HalfAddress input) noexcept {
		return decodeBits<HalfAddress, QuarterAddress, 0xFFFF'0000, 16>(input);
	}
	constexpr QuarterAddress getLowerHalf(HalfAddress input) noexcept {
		return decodeBits<HalfAddress, QuarterAddress, 0x0000'FFFF, 0>(input);
	}
	constexpr HalfAddress setUpperHalf(HalfAddress input, QuarterAddress value) noexcept {
		return encodeBits<HalfAddress, QuarterAddress, 0xFFFF'0000, 8>(input, value);
	}
	constexpr HalfAddress setLowerHalf(HalfAddress input, QuarterAddress value) noexcept {
		return encodeBits<HalfAddress, HalfAddress, 0x0000'FFFF, 0>(input, value);
	}

	constexpr HalfAddress getUpperHalf(Address input) noexcept {
		return decodeBits<Address, HalfAddress, 0xFFFF'FFFF'0000'0000, 32>(input);
	}
	constexpr HalfAddress getLowerHalf(Address input) noexcept {
		return decodeBits<Address, HalfAddress, 0x0000'0000'FFFF'FFFF, 0>(input);
	}
	constexpr Address setUpperHalf(Address input, HalfAddress value) noexcept {
		return encodeBits<Address, HalfAddress, 0xFFFF'FFFF'0000'0000, 32>(input, value);
	}
	constexpr Address setLowerHalf(Address input, HalfAddress value) noexcept {
		return encodeBits<Address, HalfAddress, 0x0000'0000'FFFF'FFFF, 0>(input, value);
	}


	template<typename T, typename R>
	constexpr R setLowerUpperHalves(T lower, T upper) noexcept {
		return setUpperHalf(setLowerHalf(R(0), lower), upper);
	}

	template<typename T, typename R>
	constexpr R setFourParts(T lowest, T lower, T higher, T highest) noexcept {
		return setHighestPart( setHigherPart( setLowerPart( setLowestPart(R(0), lowest), lower), higher), highest);
	}

	template<typename T, typename C = byte>
	constexpr bool legalValue(T r, T count = T::Count) noexcept {
		static_assert(std::is_enum<T>::value, "This is an enum value check!");
		static_assert(std::is_integral<C>::value, "C must cast to an integral type");
		return static_cast<C>(r) < static_cast<C>(count);
	}
	template<typename T>
	using HalfOf = decltype(getUpperHalf(T(0)));
	template<typename T>
	using QuarterOf = HalfOf<HalfOf<T>>;
	template<typename T>
	constexpr auto upperHalfMask = HalfOf<T>(getUpperHalf(static_cast<T>(-1)));
	template<typename T>
	constexpr auto lowerHalfMask = HalfOf<T>(getLowerHalf(static_cast<T>(-1)));
	template<typename T>
	constexpr auto lowestQuarterMask = getLowerHalf(lowerHalfMask<T>);
	template<typename T>
	constexpr auto lowerQuarterMask = getUpperHalf(lowerHalfMask<T>);
	template<typename T>
	constexpr auto higherQuarterMask = getLowerHalf(upperHalfMask<T>);
	template<typename T>
	constexpr auto highestQuarterMask = getUpperHalf(upperHalfMask<T>);
}

#endif // end TYPES_H__
