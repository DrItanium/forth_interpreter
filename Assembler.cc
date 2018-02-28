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
				return Integer(loc - from);
			} else if (loc < from) {
				return -Integer(from - loc);
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
		auto start = _currentLocation;
		_currentLocation += std::get<0>(op);
		_operations.emplace(start, [from = _currentLocation, op, this]() { return std::get<1>(op)(*this, from); });
	}
	void AssemblerBuilder::addInstruction(SizedLazyInstruction op) {
		_operations.emplace(_currentLocation, std::get<1>(op));
		_currentLocation += std::get<0>(op);
	}
	void AssemblerBuilder::addInstruction(EagerInstruction op) {
		// invoke it
		op(*this);
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
	EagerInstruction loadImmediate64(TargetRegister r, Address value) {
		return [r, value](AssemblerBuilder& ab) {
			ab.addInstruction(loadLowerImmediate48(r, value), setImmediate64_Highest(r, value));
		};
	}
	EagerInstruction loadImmediate64(TargetRegister r, const std::string& name) {
		return [r, name](AssemblerBuilder& ab) {
			ab.addInstruction(loadLowerImmediate48(r, name));
			ab.addInstruction(setImmediate16_Highest(r, name));
		};
	}
	EagerInstruction label(const std::string& name) {
		return [name](AssemblerBuilder& ab) {
			ab.labelHere(name);
		};
	}
	void AssemblerBuilder::addInstruction(const std::string& labelName) {
		labelHere(labelName);
	}
	SizedResolvableLazyFunction conditionalBranch(TargetRegister cond, const std::string& name) {
		return std::make_tuple(getInstructionWidth(Operation::ConditionalBranch),
					[name, cond](AssemblerBuilder& ab, Address from) {
						auto addr = ab.relativeLabelAddress(name, from);
						if (addr > 32767 || addr < -32768) {
							throw Problem("conditionalBranch", "Can't encode label into a relative 16-bit offset!");
						} else {
							return conditionalBranch(cond, addr);
						}
					});
	}
	EagerInstruction storeImmediate64(Address addr, const std::string& name) {
		return [addr, name](AssemblerBuilder& ab) {
			ab.addInstruction(loadImmediate64(TargetRegister::X, addr),
							  loadImmediate64(TargetRegister::Temporary, name),
							  store(TargetRegister::X, TargetRegister::Temporary));
		};
	}
    EagerInstruction storeImmediate64(TargetRegister addr, Address value) {
        if (addr == TargetRegister::Temporary) {
            throw Problem("Assembler::storeImmediate64", "Temporary register already in use!");
        } else {
            if (value == 0) {
                return [addr](AssemblerBuilder& ab) { ab.addInstruction(store(addr, TargetRegister::Zero)); };
            } else {
                return [addr, value](AssemblerBuilder& ab) {
                    ab.addInstruction(loadImmediate64(TargetRegister::Temporary, value),
                                      store(addr, TargetRegister::Temporary));
                };
            }
        }
    }
    EagerInstruction storeImmediate64(Address value) {
        return storeImmediate64(TargetRegister::X, value);
    }
    EagerInstruction storeImmediate64(Address addr, Address value) {
        return [addr, value](AssemblerBuilder& ab) {
            ab.addInstruction(loadImmediate64(TargetRegister::X, addr),
                              storeImmediate64(TargetRegister::X, value));
        };
    }
    EagerInstruction indirectLoad(TargetRegister dest, TargetRegister src) {
        if (dest == src) {
            return [dest, src](AssemblerBuilder& ab) {
                ab.addInstruction(load(dest, src), 
                        load(dest, dest));
            };
        } else {
            if (src == TargetRegister::Temporary) {
                throw Problem("indirectLoad", "Temporary already in use!");
            }
            return [dest, src](AssemblerBuilder& ab) {
                ab.addInstruction(load(TargetRegister::Temporary, src),
                        load(dest, TargetRegister::Temporary));
            };
        }
    }
    EagerInstruction pushImmediate(Address value, TargetRegister sp) {
        return [value, sp](AssemblerBuilder& ab) {
            ab.addInstruction(loadImmediate64(TargetRegister::Temporary, value),
                    pushRegister(TargetRegister::Temporary, sp));
        };
    }
    EagerInstruction pushImmediate(const Datum& value, TargetRegister sp) {
        return pushImmediate(value.address, sp);
    }
	EagerInstruction printChar(char c) {
		return [c](AssemblerBuilder& ab) {
			ab.addInstruction(loadImmediate16(TargetRegister::Temporary, QuarterAddress(c)),
							  printChar(TargetRegister::Temporary));
		};
	}
	EagerInstruction printChar(const std::string& str) {
		return [str](AssemblerBuilder& ab) {
			for (const auto c : str) {
				ab.addInstruction(printChar(c));
			}
		};
	}
	EagerInstruction popAB() {
		return [](AssemblerBuilder& ab) {
			ab.addInstruction(popA(), popB());
		};
	}

} // end namespace forth
