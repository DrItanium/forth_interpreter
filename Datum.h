// concept of a stack cell 
#ifndef DATUM_H__
#define DATUM_H__
#include "Types.h"
#include "Instruction.h"
#include <variant>
#include <optional>
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
		return legalValue<Discriminant, Address>(value);
    }
    //std::optional<forth::Discriminant> involvesDiscriminantType(Operation op);

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
        Datum(const std::string& other);
        Datum(const std::string* other);
        Datum(Datum&& other);
        bool truth;
        Integer numValue;
        Address address;
        Floating fp;
        const DictionaryEntry* entry;
        DictionaryEntry* subroutine;
        byte backingStore[sizeof(Address)];
        const std::string* _string;
        void setField(byte index, byte value);
        byte getField(byte index) const;
        HalfAddress upperHalfAddress() const noexcept;
        HalfAddress lowerHalfAddress() const noexcept;
        QuarterAddress highestQuarterAddress() const noexcept;
        QuarterAddress higherQuarterAddress() const noexcept;
        QuarterAddress lowerQuarterAddress() const noexcept;
        QuarterAddress lowestQuarterAddress() const noexcept;

    };
    std::ostream& operator<<(std::ostream& out, const Datum& dt);
	std::ostream& operator<<(std::ostream& out, const Discriminant& d);

    class Register {
        public:
            Register(const Register& r) = delete;
            Register(bool readonly = false);
            ~Register();
            const Datum& getValue() const noexcept;
            void setValue(Datum d) noexcept;
            bool getTruth() const noexcept;
            Floating getFP() const noexcept;
            Integer getInt() const noexcept;
            Address getAddress() const noexcept;
            const DictionaryEntry* getWord() const noexcept;
            Molecule getMolecule() const noexcept;
            bool isReadonly() const noexcept { return _readonly; }
            void reset();
            void increment(Address amount = 1);
            void decrement(Address amount = 1);
			template<Address mask, Address shift>
			void encodeBits(Address value) noexcept {
                if (!_readonly) {
				    _value.address = encodeBits<Address, Address, mask, shift>(_value.address, value);
                }
			}
        private:
            Datum _value;
            bool _readonly;
    };
    class ReadOnlyRegister : public Register {
        public:
            ReadOnlyRegister(const ReadOnlyRegister&) = delete;
            ReadOnlyRegister() : Register(true) { }
    };
} // end namespace forth
#endif // end DATUM_H__
