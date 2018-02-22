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

	void AssemblerBuilder::labelHere(const std::string& name) {
		if (auto result = _names.find(name); result == _names.cend()) {
			_names.emplace(name, _currentLocation);
		} else {
			throw Problem("labelHere", "Requested label already registered!");
		}
	}

	Address AssemblerBuilder::absoluteLabelAddress(const std::string& name) const {
		if (auto result = _names.find(name); result != _names.cend()) {
			return result->second;
		} else {
			throw Problem("absoluteLabelAddress", "Can't find label name!");
		}
	}
	Integer AssemblerBuilder::relativeLabelAddress(const std::string& name) const {
		return relativeLabelAddress(name, here());
	}
	Integer AssemblerBuilder::relativeLabelAddress(const std::string& name, Address from) const {
		if (auto result = _names.find(name); result != _names.cend()) {
			auto loc = result->second;
			if (loc > from) {
				return -(Integer(loc - from));
			} else if (loc < from) {
				return Integer(from - loc);
			} else {
				return 0;
			}
		} else {
			throw Problem("relativeLabelAddress", "Can't find label name!");
		}
	}
} // end namespace forth

