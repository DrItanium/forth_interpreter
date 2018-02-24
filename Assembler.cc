#include "Types.h"
#include "Instruction.h"
#include "Assembler.h"

namespace forth {
	AssemblerBuilder::AssemblerBuilder(Address baseAddress) : _baseAddress(baseAddress), _currentLocation(baseAddress) {}
	AssemblerBuilder::~AssemblerBuilder() {

	}

	void AssemblerBuilder::installMolecule(Address address, const Molecule& m) {
		_operations.emplace(address, m._value);
	}

	void AssemblerBuilder::installIntoMemory(std::function<void(Address, Address)> fn) {
		for (const auto& m : _operations) {
			// mapping between an address and the value to install into
			if (std::holds_alternative<Address>(m.second)) {
				fn(m.first, std::get<Address>(m.second));
			} else if (std::holds_alternative<LazyInstruction>(m.second)) {
				fn(m.first, std::get<LazyInstruction>(m.second)());
			} else {
				throw Problem("AssemblerBuilder::installIntoMemory", "Bad Discrminant type!");
			}
		}
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
	void AssemblerBuilder::addInstruction(const Molecule& m) {
		_operations.emplace(_currentLocation, m._value);
		_currentLocation += sizeof(Address);
	}
	void AssemblerBuilder::addInstruction(LazyInstruction op, byte width) {
		addInstruction(std::make_tuple(width, op));
	}
	void AssemblerBuilder::addInstruction(ResolvableLazyFunction op, byte width) {
		addInstruction(std::make_tuple(width, op));
	}
	void AssemblerBuilder::addInstruction(SizedResolvableLazyFunction op) {
		// unpack and repack this with a new function
		_operations.emplace(_currentLocation, [op, this]() { return std::get<1>(op)(*this, here()); });
		_currentLocation += std::get<0>(op);
	}
	void AssemblerBuilder::addInstruction(SizedLazyInstruction op) {
		_operations.emplace(_currentLocation, std::get<1>(op));
		_currentLocation += std::get<0>(op);
	}
	SizedResolvableLazyFunction jumpRelative(const std::string& name) {
		return std::make_tuple(getInstructionWidth(Operation::Jump), 
							[name](AssemblerBuilder& ab, Address from) {
								auto addr = ab.relativeLabelAddress(name, from);
								if (addr > 32767 || addr < -32768) {
									throw Problem("jumpRelative", "Can't encode label address into 16-bits");
								} else {
									return jumpRelative(QuarterInteger(addr));
								}
							   });
	}
	SizedResolvableLazyFunction jumpAbsolute(const std::string& name) {
		return std::make_tuple(getInstructionWidth(Operation::JumpAbsolute),
							   [name](AssemblerBuilder& ab, Address from) {
									auto addr = ab.absoluteLabelAddress(name);
									if (addr > 0xFFFF) {
										throw Problem("jumpAbsolute", "Can't encode label address into 16-bits");
									} else {
										return jumpAbsolute(QuarterAddress(addr));
									}
							   });
	}
	SizedResolvableLazyFunction setImmediate16_Lowest(TargetRegister r, const std::string& name) {
		return std::make_tuple(getInstructionWidth(Operation::SetImmediate16_Lowest),
				[name, r](AssemblerBuilder& ab, Address _) {
						return forth::setImmediate64_Lowest(r, ab.absoluteLabelAddress(name));
				});
	}
	SizedResolvableLazyFunction setImmediate16_Lower(TargetRegister r, const std::string& name) {
		return std::make_tuple(getInstructionWidth(Operation::SetImmediate16_Lower),
				[name, r](AssemblerBuilder& ab, Address _) {
						return forth::setImmediate64_Lower(r, ab.absoluteLabelAddress(name));
				});
	}
	SizedResolvableLazyFunction setImmediate16_Higher(TargetRegister r, const std::string& name) {
		return std::make_tuple(getInstructionWidth(Operation::SetImmediate16_Higher),
				[name, r](AssemblerBuilder& ab, Address _) {
						return forth::setImmediate64_Higher(r, ab.absoluteLabelAddress(name));
				});
	}
	SizedResolvableLazyFunction setImmediate16_Highest(TargetRegister r, const std::string& name) {
		return std::make_tuple(getInstructionWidth(Operation::SetImmediate16_Highest),
				[name, r](AssemblerBuilder& ab, Address _) {
						return forth::setImmediate64_Highest(r, ab.absoluteLabelAddress(name));
				});
	}
	SizedResolvableLazyFunction loadLowerImmediate48(TargetRegister r, const std::string& name) {
		return std::make_tuple(getInstructionWidth(Operation::LoadImmediateLower48),
				[name, r](AssemblerBuilder& ab, Address _) {
					return forth::loadLowerImmediate48(r, ab.absoluteLabelAddress(name));
				});
	}
} // end namespace forth

