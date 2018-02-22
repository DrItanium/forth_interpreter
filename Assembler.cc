#include "Types.h"
#include "Instruction.h"
#include "Assembler.h"

namespace forth {
	AssemblerBuilder::AssemblerBuilder(Address baseAddress) : _baseAddress(baseAddress), _currentLocation(baseAddress), _currentMolecule(0) { }
	AssemblerBuilder::~AssemblerBuilder() {

	}

	void AssemblerBuilder::installMolecule(Address address, const Molecule& m) {
		_operations.emplace_back(std::make_tuple(address, m));
	}

	void AssemblerBuilder::installIntoMemory(std::function<void(Address, Address)> fn) {
		for (const auto& m : _operations) {
			// mapping between an address and the value to install into
			fn(std::get<0>(m), std::get<1>(m)._value);
		}
	}

	void AssemblerBuilder::newMolecule() {
		installMolecule(_currentLocation, _currentMolecule);
		++_currentLocation;
		_currentMolecule._value = 0;
	}

	void AssemblerBuilder::installMolecule(AssemblerBuilder::AddressToMolecule tup) {
		installMolecule(std::get<0>(tup), std::get<1>(tup));
	}

} // end namespace forth

