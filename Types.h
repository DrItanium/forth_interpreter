#ifndef TYPES_H__
#define TYPES_H__
#include <cstdint>
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
    static_assert((sizeof(Address) / sizeof(HalfAddress)) == 4, "Number of half addresses in an address is not 2!");
}

#endif // end TYPES_H__
