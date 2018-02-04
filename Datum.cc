#include <iostream>
#include "Types.h"
#include "Datum.h"
#include "Machine.h"
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

	void addDiscriminantWords(Machine& machine) {
		machine.buildWord("dataType:SIGNED", false, Discriminant::Number);
		machine.buildWord("dataType:ADDRESS", false, Discriminant::MemoryAddress);
		machine.buildWord("dataType:FP", false, Discriminant::FloatingPoint);
		machine.buildWord("dataType:BOOLEAN", false, Discriminant::Boolean);
        machine.buildWord("dataType:WORD", false, Discriminant::Word);
        machine.buildWord("dataType:MOLECULE", false, Discriminant::Molecule);
        machine.buildWord("dataType:DICTIONARY_ENTRY", false, Discriminant::DictionaryEntry);
	}
} // end namespace forth
