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
    constexpr forth::Discriminant involvesDiscriminantType(Operation op) noexcept {
#define FVersion(x) FloatingPoint ## x 
#define UVersion(x) Unsigned ## x
#define BVersion(x) Boolean ## x
        switch (op) {
            case Operation::UnsignedPowFull:
            case Operation::UnsignedNotFull:
            case Operation::UnsignedMinusFull:
            case Operation:: UVersion(Add):
            case Operation:: UVersion(Subtract):
            case Operation:: UVersion(Multiply):
            case Operation:: UVersion(Divide):
            case Operation:: UVersion(Modulo):
            case Operation:: UVersion(Not):
            case Operation:: UVersion(Minus):
            case Operation:: UVersion(And):
            case Operation:: UVersion(Or):
            case Operation:: UVersion(GreaterThan):
            case Operation:: UVersion(LessThan):
            case Operation:: UVersion(Xor):
            case Operation:: UVersion(ShiftRight):
            case Operation:: UVersion(ShiftLeft):
            case Operation:: UVersion(Equals):
            case Operation:: UVersion(Pow):
#define FullImmediate(x) Operation:: UVersion(x ## Full): case Operation:: UVersion(x ## Immediate) 
            case FullImmediate(Add):
            case FullImmediate(Subtract):
            case FullImmediate(Multiply):
            case FullImmediate(Divide):
            case FullImmediate(Modulo):
            case FullImmediate(And):
            case FullImmediate(Or):
            case FullImmediate(GreaterThan):
            case FullImmediate(LessThan):
            case FullImmediate(Xor):
            case FullImmediate(ShiftRight):
            case FullImmediate(ShiftLeft):
            case FullImmediate(Equals):
#undef FullImmediate
            case Operation:: UVersion(TypeValue):
            case Operation::UnsignedIncrement:
            case Operation::UnsignedDecrement:
                return Discriminant::MemoryAddress;
            case Operation:: FloatingPointPowFull:
            case Operation:: FloatingPointMinusFull:
            case Operation:: FVersion(Add):
            case Operation:: FVersion(Subtract):
            case Operation:: FVersion(Multiply):
            case Operation:: FVersion(Divide):
            case Operation:: FVersion(Minus):
            case Operation:: FVersion(GreaterThan):
            case Operation:: FVersion(LessThan):
            case Operation:: FVersion(Equals):
            case Operation:: FVersion(Pow):
            case Operation:: FVersion(TypeValue):
#define FullImmediate(x) Operation:: FVersion(x ## Full)
            case FullImmediate(Add):
            case FullImmediate(Subtract):
            case FullImmediate(Multiply):
            case FullImmediate(Divide):
            case FullImmediate(GreaterThan):
            case FullImmediate(LessThan):
            case FullImmediate(Equals):
#undef FullImmediate
        case Operation::FloatingPointIncrement:
        case Operation::FloatingPointDecrement:
                return Discriminant::FloatingPoint;
            case Operation:: BVersion(Not):
            case Operation:: BVersion(And):
            case Operation:: BVersion(Or):
            case Operation:: BVersion(Xor):
            case Operation:: BVersion(Equals):
            case Operation:: BVersion(TypeValue):
            case Operation:: BVersion(NotFull):
            case Operation:: BVersion(AndFull):
            case Operation:: BVersion(OrFull):
            case Operation:: BVersion(XorFull):
            case Operation:: BVersion(EqualsFull):
                return Discriminant::Boolean;
#define FullImmediate(x) Operation:: x ## Full: case Operation:: x ## Immediate
            case FullImmediate(Add):
            case FullImmediate(Subtract):
            case FullImmediate(Multiply):
            case FullImmediate(Divide): 
            case FullImmediate(Modulo): 
            case FullImmediate(And): 
            case FullImmediate(Or): 
            case FullImmediate(GreaterThan): 
            case FullImmediate(LessThan): 
            case FullImmediate(Xor):
            case FullImmediate(ShiftRight):
            case FullImmediate(ShiftLeft):
            case FullImmediate(Equals):
#undef FullImmediate
            case Operation::Add:
            case Operation::Subtract:
            case Operation::Multiply:
            case Operation::Divide:
            case Operation::Modulo:
            case Operation::Not:
            case Operation::Minus:
            case Operation::And:
            case Operation::Or:
            case Operation::GreaterThan:
            case Operation::LessThan:
            case Operation::Xor:
            case Operation::ShiftRight:
            case Operation::ShiftLeft:
            case Operation::Equals:
            case Operation::TypeValue:
            case Operation::Pow:
            case Operation::Increment:
            case Operation::Decrement:
                return Discriminant::Number;
            default:
                return Discriminant::Count;

        }
#undef FVersion
#undef UVersion
#undef BVersion
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
            void reset();
            void increment(Address amount = 1) { _value.address += amount; }
			void decrement(Address amount = 1) { _value.address -= amount; }
        private:
            Datum _value;
    };
} // end namespace forth
#endif // end DATUM_H__
