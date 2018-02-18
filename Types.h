#ifndef TYPES_H__
#define TYPES_H__
#include <climits>
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
	template<typename T>
	constexpr auto bitWidth = sizeof(T) * CHAR_BIT;
	template<typename T>
	constexpr auto quarterBitWidth = bitWidth<T> >> 2;
	template<typename T>
	constexpr auto halfBitWidth = bitWidth<T> >> 1;
	template<typename T>
	constexpr auto fullMask = (static_cast<T>(-1));

	template<typename T, typename R, T mask, T shift = 0>
	constexpr T encodeBits(T value, R newValue) noexcept {
		return (value & ~mask) | ((static_cast<T>(newValue) << shift) & mask);
	}

	template<typename T, typename R, T mask, T shift = 0>
	constexpr R decodeBits(T value) noexcept {
		return static_cast<R>((value & mask) >> shift);
	}

	constexpr byte getUpperHalf(byte input) noexcept {
		return decodeBits<byte, byte, 0xF0, halfBitWidth<byte>>(input);
	}
	constexpr byte getLowerHalf(byte input) noexcept {
		return decodeBits<byte, byte, 0x0F>(input);
	}

	constexpr byte getUpperHalf(QuarterAddress input) noexcept {
		return decodeBits<QuarterAddress, byte, 0xFF'00, halfBitWidth<QuarterAddress>>(input);
	}
	constexpr byte getLowerHalf(QuarterAddress input) noexcept {
		return decodeBits<QuarterAddress, byte, 0x00'FF>(input);
	}

	constexpr QuarterAddress getUpperHalf(HalfAddress input) noexcept {
		return decodeBits<HalfAddress, QuarterAddress, 0xFFFF'0000, halfBitWidth<HalfAddress>>(input);
	}
	constexpr QuarterAddress getLowerHalf(HalfAddress input) noexcept {
		return decodeBits<HalfAddress, QuarterAddress, 0x0000'FFFF>(input);
	}

	constexpr HalfAddress getUpperHalf(Address input) noexcept {
		return decodeBits<Address, HalfAddress, 0xFFFF'FFFF'0000'0000, halfBitWidth<Address>>(input);
	}
	constexpr HalfAddress getLowerHalf(Address input) noexcept {
		return decodeBits<Address, HalfAddress, 0x0000'0000'FFFF'FFFF>(input);
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
	constexpr auto halfMask = T(getLowerHalf(fullMask<T>));
	static_assert(halfMask<QuarterAddress> == 0x00FF, "Illegal halfMask!");
	template<typename T>
	constexpr auto quarterMask = T(getLowerHalf(getLowerHalf(fullMask<T>)));
	static_assert(quarterMask<QuarterAddress> == 0x000F, "Illegal address!");

	template<typename T>
	constexpr auto lowerHalfMask = halfMask<T>;
	template<typename T>
	constexpr auto upperHalfMask = halfMask<T> << halfBitWidth<T>;
	template<typename T>
	constexpr auto lowestQuarterMask = quarterMask<T>;
	template<typename T>
	constexpr auto lowerQuarterMask = quarterMask<T> << quarterBitWidth<T>;
	template<typename T>
	constexpr auto higherQuarterMask = quarterMask<T> << (quarterBitWidth<T> * 2);
	template<typename T>
	constexpr auto highestQuarterMask= quarterMask<T> << (quarterBitWidth<T> * 3);


	template<typename T>
	constexpr QuarterOf<T> getLowestQuarter(T value) noexcept {
		return decodeBits<T, QuarterOf<T>, lowestQuarterMask<T>, quarterBitWidth<T> * 0>(value);
	}
	template<typename T>
	constexpr QuarterOf<T> getLowerQuarter(T value) noexcept {
		return decodeBits<T, QuarterOf<T>, lowerQuarterMask<T>, quarterBitWidth<T> * 1>(value);
	}
	template<typename T>
	constexpr QuarterOf<T> getHigherQuarter(T value) noexcept {
		return decodeBits<T, QuarterOf<T>, higherQuarterMask<T>, quarterBitWidth<T> * 2 >(value);
	}

	template<typename T>
	constexpr QuarterOf<T> getHighestQuarter(T value) noexcept {
		return decodeBits<T, QuarterOf<T>, higherQuarterMask<T>, quarterBitWidth<T> * 3>(value);
	}

	template<typename T>
	constexpr T setLowestQuarter(T input, QuarterOf<T> value) noexcept {
		return encodeBits<decltype(input), decltype(value), lowestQuarterMask<T>, quarterBitWidth<T> * 0>(input, value);
	}
	template<typename T>
	constexpr T setLowerQuarter(T input, QuarterOf<T> value) noexcept {
		return encodeBits<decltype(input), decltype(value), lowerQuarterMask<T>, quarterBitWidth<T> * 1>(input, value);
	}

	template<typename T>
	constexpr T setHigherQuarter(T input, QuarterOf<T> value) noexcept {
		return encodeBits<decltype(input), decltype(value), higherQuarterMask<T>, quarterBitWidth<T> * 2>(input, value);
	}

	template<typename T>
	constexpr T setHighestQuarter(T input, QuarterOf<T> value) noexcept {
		return encodeBits<decltype(input), decltype(value), highestQuarterMask<T>, quarterBitWidth<T> * 3>(input, value);
	}

	template<typename T>
	constexpr T setFourQuarters(QuarterOf<T> lowest, QuarterOf<T> lower, QuarterOf<T> higher, QuarterOf<T> highest) noexcept {
		return setHighestQuarter<T>( setHigherQuarter<T>( setLowerQuarter<T>( setLowestQuarter<T>(T(0), lowest), lower), higher), highest);
	}
	template<typename T>
	constexpr T setUpperHalf(T input, HalfOf<T> value) noexcept {
		return encodeBits<T, HalfOf<T>, upperHalfMask<T>, halfBitWidth<T>>(input, value);
	}
	template<typename T>
	constexpr Address setLowerHalf(T input, HalfOf<T>value) noexcept {
		return encodeBits<T, HalfOf<T>, lowerHalfMask<T>, halfBitWidth<T>*0>(input, value);
	}

	template<typename T>
	constexpr T setLowerUpperHalves(HalfOf<T> lower, HalfOf<T> upper) noexcept {
		return setUpperHalf<T>(setLowerHalf<T>(T(0), lower), upper);
	}
}

#endif // end TYPES_H__
