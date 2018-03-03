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
	Datum::Datum(const Datum& other) : address(other.address) { }
    Datum::Datum(Datum&& other) : address(std::move(other.address)) { }

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
    /*
    std::optional<forth::Discriminant> involvesDiscriminantType(Operation op) {
        std::optional<forth::Discriminant> r;
        switch (op) {
#define X(title, sz, arg, disc) case Operation :: title : { if constexpr (Discriminant:: disc != Discriminant::Count) { r = Discriminant:: disc ; } break; } 
#include "InstructionData.def"
#undef X
            default:
                break;

        }
        return r;
    }
    */

} // end namespace forth
