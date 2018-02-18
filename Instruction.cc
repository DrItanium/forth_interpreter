#include "Instruction.h"


namespace forth {
	Molecule::Molecule(Address v) : _value(v) { }
	Molecule::Molecule(const Molecule& other) : _value(other._value) { }

	byte Molecule::getByte(Address index) const {
        if (index >= sizeof(Address)) {
            throw Problem("getByte", "INSTRUCTION MISALIGNED");
        } else {
            return backingStore[index];
        }
	}
    QuarterAddress Molecule::getQuarterAddress(Address index) const {
        auto lower = static_cast<QuarterAddress>(getByte(index));
        auto upper = static_cast<QuarterAddress>(getByte(index + 1));
        return (upper << 8) | lower;
    }
	QuarterInteger Molecule::getQuarterOffset(Address index) const {
        auto lower = static_cast<QuarterInteger>(getByte(index));
        auto upper = static_cast<QuarterInteger>(getByte(index + 1));
		return (upper << 8) | lower;
	}
	Address Molecule::getImm48(Address index) const {
		auto b0 = static_cast<Address>(getByte(index));
		auto b1 = static_cast<Address>(getByte(index + 1));
		auto b2 = static_cast<Address>(getByte(index + 2));
		auto b3 = static_cast<Address>(getByte(index + 3));
		auto b4 = static_cast<Address>(getByte(index + 4));
		auto b5 = static_cast<Address>(getByte(index + 5));
		return ((b5 << 40) | (b4 << 32) | (b3 << 24) | (b2 << 16) | (b1 << 8) | b0) & 0x00FF'FFFF'FFFF'FFFF;
	}

} // end namespace forth
