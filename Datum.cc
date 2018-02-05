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
	Register::Register(const Register& r) : _type(r._type), _value(r._value) { }
	Register::Register() : _type(static_cast<Discriminant>(0)), _value(Address(0)) { }

} // end namespace forth
