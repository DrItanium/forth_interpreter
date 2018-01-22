#ifndef TYPES_H__
#define TYPES_H__
#include <cstdint>
namespace forth {
    using Address = uint64_t;
    using Integer = int64_t;
    using Floating = double;
    using byte = uint8_t;
    static_assert(sizeof(Address) == sizeof(Integer), "Address and integer must be the same size!");
    static_assert(sizeof(Integer) == sizeof(Floating), "Integer and Floating must be the same size!");
}

#endif // end TYPES_H__
