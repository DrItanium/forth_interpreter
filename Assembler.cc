#include "Types.h"
#include "Instruction.h"
#include "Assembler.h"

namespace forth {
	AssemblerBuilder::AssemblerBuilder(Address baseAddress) : _baseAddress(baseAddress), _currentLocation(baseAddress) { }
	AssemblerBuilder::~AssemblerBuilder() {

	}

	void AssemblerBuilder::installMolecule(const Molecule& m) {

	}

	void AssemblerBuilder::installIntoMemory(std::function<void(Address, Address)> fn) {
		for (const auto& m : _operations) {
		}
	}

} // end namespace forth

