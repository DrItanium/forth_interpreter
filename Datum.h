// concept of a stack cell 
#ifndef DATUM_H__
#define DATUM_H__
#include "Types.h"
namespace forth {
    class DictionaryEntry;
    enum class Discriminant : Address {
        Number,
        MemoryAddress,
        FloatingPoint,
        Boolean,
        Word,
        Count,
    };
    union Datum {
        Datum() = default;
        Datum(Integer x) : numValue(x) { }
        Datum(Address x) : address(x) { }
        Datum(Floating x) : fp(x) { }
        Datum(bool x) : truth(x) { }
        Datum(const DictionaryEntry* x) : entry(x) { }
        ~Datum() = default;
        Datum(const Datum& other);
        bool truth;
        Integer numValue;
        Address address;
        Floating fp;
        const DictionaryEntry* entry;
        byte backingStore[sizeof(Integer)];
    };
    std::ostream& operator<<(std::ostream& out, const Datum& dt);
} // end namespace forth
#endif // end DATUM_H__
