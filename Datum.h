// concept of a stack cell 
#ifndef DATUM_H__
#define DATUM_H__
#include "Types.h"
#include <iostream>
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
	std::ostream& operator<<(std::ostream& out, const Discriminant& d);

    class Register {
        public:
            Register() = default;
            void setType(Discriminant type) noexcept { _type = type; }
            Discriminant getType() const noexcept { return _type; }
            const Datum& getValue() const noexcept { return _value; }
            void setValue(Datum d) noexcept { _value = d; }
            void setValue(Address a) noexcept { _value.address = a; }
            void setValue(bool v) noexcept { _value.truth = v; }
            void setValue(Integer i) noexcept { _value.numValue = v; }
            void setValue(const DictionaryEntry* e) noexcept { _value.entry = e; }
            void setValue(Floating f) noexcept { _value.fp = f; }
            bool getTruth() const noexcept { return _value.truth; }
            Floating getFP() const noexcept { return _value.fp; }
            Integer getInt() const noexcept { return _value.numValue; }
            Address getAddress() const noexcept { return _value.address; }
            const DictionaryEntry* getWord() const noexcept { return _value.entry; }
        private:
            Discriminant _type;
            Datum _value;
    };
} // end namespace forth
#endif // end DATUM_H__
