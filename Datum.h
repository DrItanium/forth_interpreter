// concept of a stack cell 
#ifndef DATUM_H__
#define DATUM_H__
#include "Types.h"
#include "Instruction.h"
#include <iostream>
namespace forth {
    class DictionaryEntry;
    enum class Discriminant : Address {
        Number,
        MemoryAddress,
        FloatingPoint,
        Boolean,
        Word,
        Molecule,
        DictionaryEntry,
        Count,
    };
    constexpr bool legalValue(Discriminant value) noexcept {
        return static_cast<Address>(value) < static_cast<Address>(Discriminant::Count); 
    }

    union Datum {
        Datum() = default;
        ~Datum() = default;
        Datum(Integer x) : numValue(x) { }
        Datum(Address x) : address(x) { }
        Datum(Floating x) : fp(x) { }
        Datum(bool x) : truth(x) { }
        Datum(const DictionaryEntry* x) : entry(x) { }
        Datum(DictionaryEntry* x) : subroutine(x) { }
        Datum(const Datum& other);
        bool truth;
        Integer numValue;
        Address address;
        Floating fp;
        const DictionaryEntry* entry;
        DictionaryEntry* subroutine;
        byte backingStore[sizeof(Integer)];
    };
    std::ostream& operator<<(std::ostream& out, const Datum& dt);
	std::ostream& operator<<(std::ostream& out, const Discriminant& d);

    class Register {
        public:
            Register();
            Register(const Register& other);
            const Datum& getValue() const noexcept { return _value; }
            void setValue(Datum d) noexcept { _value = d; }
            bool getTruth() const noexcept { return _value.truth; }
            Floating getFP() const noexcept { return _value.fp; }
            Integer getInt() const noexcept { return _value.numValue; }
            Address getAddress() const noexcept { return _value.address; }
            const DictionaryEntry* getWord() const noexcept { return _value.entry; }
            Molecule getMolecule() const noexcept { return static_cast<Molecule>(_value.address); }
            void reset() { 
                _value.address = 0;
                _type = static_cast<decltype(_type)>(0); 
            }
            void increment(Address amount = 1) { _value.address += amount; }
			void decrement(Address amount = 1) { _value.address -= amount; }
        private:
            Datum _value;
    };
} // end namespace forth
#endif // end DATUM_H__
