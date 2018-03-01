#include <iostream>
#include "Types.h"
#include "Datum.h"
namespace forth {
	std::ostream& operator<<(std::ostream& out, const Datum& dt) {
		// save flags
		auto flags = out.flags();
		out << "{" << dt.numValue << ", 0x" << std::hex << dt.address << ", " << std::dec << dt.fp << ", " << std::boolalpha << dt.truth;
		out << ", !" << std::hex << dt.entry;
		out << "}" ;
		out.setf(flags); // restore after done
		return out;
	}
	std::ostream& operator<<(std::ostream& out, const Discriminant& dt) {
		auto flags = out.flags();
		out << "0x" << std::hex << static_cast<Address>(dt);
		out.setf(flags);
		return out;
	}
	Datum::Datum(const Datum& other) {
		for (auto k = 0; k < sizeof(Integer); ++k) {
			// make sure that the compiler won't do something goofy when doing
			// copying
			backingStore[k] = other.backingStore[k];
		}
	}

    Datum::Datum(const std::string* value) : _string(value) { }
    Datum::Datum(const std::string& value) : _string(&value) { }


	const Datum& Register::getValue() const noexcept { 

		return _value; 
	}
	void Register::setValue(Datum d) noexcept {
		if (!_readonly) {
			_value = d;
		}
	}
	bool Register::getTruth() const noexcept { 
		return _value.truth; 
	}
	Floating Register::getFP() const noexcept {
		return _value.fp; 
	}
	Integer Register::getInt() const noexcept { 
		return _value.numValue; 
	}
	Address Register::getAddress() const noexcept { 
		return _value.address; 
	}
	const DictionaryEntry* Register::getWord() const noexcept {
		return _value.entry; 
	}
	Molecule Register::getMolecule() const noexcept { 
		return static_cast<Molecule>(_value.address); 
	}
	void Register::reset() {
		if (!_readonly) {
			_value.address = 0;
		}
	}
	void Register::increment(Address amount) { 
		if (!_readonly) {
			_value.address += amount; 
		}
	}
	void Register::decrement(Address amount) { 
		if (!_readonly) {
			_value.address -= amount; 
		}
	}
	Register::Register(bool readonly) : _value(Address(0)), _readonly(readonly) { }
	Register::~Register() { 
		_value.address = 0;
		_readonly = false;
	}

    void Datum::setField(byte index, byte value) {
        if (index >= sizeof(Address)) {
            throw Problem("Datum::setField", "index is too large!");
        } else {
            // cross platform way to do this
            address = encodeBits<Address, byte>(address, value, Address(0xFF) << (index << 3), index << 3);
        }
    }
    byte Datum::getField(byte index) const {
        if (index >= sizeof(Address)) {
            throw Problem("Datum::getfield", "index is too large!");
        } else {
			auto mask = Address(0xFF) << (index << 3);
			auto shift = index << 3;
            return decodeBits<Address, byte>(address, mask, shift);
        }
    }

    HalfAddress Datum::upperHalfAddress() const noexcept {
        return getUpperHalf(address);
    }
    HalfAddress Datum::lowerHalfAddress() const noexcept {
        return getLowerHalf(address);
    }

    QuarterAddress Datum::highestQuarterAddress() const noexcept { return getUpperHalf(upperHalfAddress()); }
    QuarterAddress Datum::higherQuarterAddress() const noexcept { return getLowerHalf(upperHalfAddress()); }
    QuarterAddress Datum::lowerQuarterAddress() const noexcept { return getUpperHalf(lowerHalfAddress()); }
    QuarterAddress Datum::lowestQuarterAddress() const noexcept { return getLowerHalf(lowerHalfAddress()); }
    std::optional<forth::Discriminant> involvesDiscriminantType(Operation op) {
        std::optional<forth::Discriminant> r;
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
                r = Discriminant::MemoryAddress;
                break;
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
                r = Discriminant::FloatingPoint;
                break;
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
                r = Discriminant::Boolean;
                break;
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
                r = Discriminant::Number;
                break;
            default:
                break;

        }
#undef FVersion
#undef UVersion
#undef BVersion
        return r;
    }

} // end namespace forth
