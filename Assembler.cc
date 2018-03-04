#include "Types.h"
#include "Instruction.h"
#include "Assembler.h"
#include "Core.h"

namespace forth {
	AssemblerBuilder::AssemblerBuilder(Address baseAddress) : _baseAddress(baseAddress), _currentLocation(baseAddress) {}
	AssemblerBuilder::~AssemblerBuilder() {

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
#define OneByte(title) Core:: title op ## title () noexcept { return Core:: title (); }
#define TwoByte(title, b) \
	Core:: title op ## title (const Core:: b & x) noexcept { \
		Core:: title value; \
		value.args = x; \
		return value; \
	}
#define ThreeByte(title, b) \
	Core:: title op ## title (const Core:: b & x) noexcept { \
		Core:: title value; \
		value.args = x; \
		return value; \
	}
#define FourByte(title, b) \
	Core:: title op ## title (const Core:: b & x) noexcept { \
		Core:: title value; \
		value.args = x; \
		return value; \
	}
#define GrabBag(title, b) \
	Core:: title op ## title (const Core:: b & x) noexcept { \
		Core:: title value; \
		value.args = x; \
		return value; \
	}
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef GrabBag

	EagerInstruction opPrintChar(char c) noexcept {
		return [c](AssemblerBuilder& ab) {
			ab.addInstruction(opAddImmediate({TargetRegister::Temporary,
											 TargetRegister::Zero,
											 QuarterAddress(c)}),
							  opPrintChar(TargetRegister::Temporary));
		};
	}
	EagerInstruction opPrintChar(const std::string& str) noexcept {
		return [str](AssemblerBuilder& ab) {
			for (auto const& c : str) {
				ab.addInstruction(opPrintChar(c));
			}
		};
	}
	Core::Move zeroRegister(TargetRegister reg) noexcept {
		return opMove({reg, TargetRegister::Zero});
	}
	Core::LoadImmediate64 loadImmediate64(TargetRegister reg, Address value) noexcept {
		return opLoadImmediate64({reg, value});
	}

	EagerInstruction useRegister(TargetRegister reg, EagerInstruction body) noexcept {
		return [reg, body](AssemblerBuilder& ab) {
			ab.addInstruction(opPushRegister(reg),
							  body,
							  opPopRegister(reg));
		};
	}
	EagerInstruction popAB() noexcept {
		return [](AssemblerBuilder& ab) {
			ab.addInstruction(popA(), popB());
		};
	}

	Core::Swap swapAB() noexcept {
		return opSwap({TargetRegister::A, TargetRegister::B});
	}

	EagerInstruction label(const std::string& str) {
		return [str](auto& ab) { ab.labelHere(str); };
	}
	Core::PushRegister opPushRegister(TargetRegister reg, TargetRegister sp) noexcept {
		return opPushRegister({sp, reg});
	}
	Core::PopRegister opPopRegister(TargetRegister reg, TargetRegister sp ) noexcept {
		return opPopRegister({reg, sp});
	}

	Core::PopRegister popA() noexcept { return opPopRegister(TargetRegister::A); }
	Core::PopRegister popB() noexcept { return opPopRegister(TargetRegister::B); }
	Core::PopRegister popC() noexcept { return opPopRegister(TargetRegister::C); }
	Core::PushRegister pushA() noexcept { return opPushRegister(TargetRegister::A); }
	Core::PushRegister pushB() noexcept { return opPushRegister(TargetRegister::B); }
	Core::PushRegister pushC() noexcept { return opPushRegister(TargetRegister::C); }
	//SizedResolvableLazyFunction jumpRelative(const std::string& name) {
	//	return std::make_tuple(getInstructionWidth(Operation::Jump), 
	//						[name](AssemblerBuilder& ab, Address from) {
	//							auto addr = ab.relativeLabelAddress(name, from);
	//							if (addr > 32767 || addr < -32768) {
	//								throw Problem("jumpRelative", "Can't encode label address into 16-bits");
	//							} else {
	//								return jumpRelative(QuarterInteger(addr));
	//							}
	//						   });
	//}
	//SizedResolvableLazyFunction jumpAbsolute(const std::string& name) {
	//	return std::make_tuple(getInstructionWidth(Operation::JumpAbsolute),
	//						   [name](AssemblerBuilder& ab, Address from) {
	//								auto addr = ab.absoluteLabelAddress(name);
	//								if (addr > 0xFFFF) {
	//									throw Problem("jumpAbsolute", "Can't encode label address into 16-bits");
	//								} else {
	//									return jumpAbsolute(QuarterAddress(addr));
	//								}
	//						   });
	//}
	//SizedResolvableLazyFunction setImmediate16_Lowest(TargetRegister r, const std::string& name) {
	//	return std::make_tuple(getInstructionWidth(Operation::SetImmediate16_Lowest),
	//			[name, r](AssemblerBuilder& ab, Address _) {
	//					return forth::setImmediate64_Lowest(r, ab.absoluteLabelAddress(name));
	//			});
	//}
	//SizedResolvableLazyFunction setImmediate16_Lower(TargetRegister r, const std::string& name) {
	//	return std::make_tuple(getInstructionWidth(Operation::SetImmediate16_Lower),
	//			[name, r](AssemblerBuilder& ab, Address _) {
	//					return forth::setImmediate64_Lower(r, ab.absoluteLabelAddress(name));
	//			});
	//}
	//SizedResolvableLazyFunction setImmediate16_Higher(TargetRegister r, const std::string& name) {
	//	return std::make_tuple(getInstructionWidth(Operation::SetImmediate16_Higher),
	//			[name, r](AssemblerBuilder& ab, Address _) {
	//					return forth::setImmediate64_Higher(r, ab.absoluteLabelAddress(name));
	//			});
	//}
	//SizedResolvableLazyFunction setImmediate16_Highest(TargetRegister r, const std::string& name) {
	//	return std::make_tuple(getInstructionWidth(Operation::SetImmediate16_Highest),
	//			[name, r](AssemblerBuilder& ab, Address _) {
	//					return forth::setImmediate64_Highest(r, ab.absoluteLabelAddress(name));
	//			});
	//}
	//SizedResolvableLazyFunction loadLowerImmediate48(TargetRegister r, const std::string& name) {
	//	return std::make_tuple(getInstructionWidth(Operation::ImmediateLower48),
	//			[name, r](AssemblerBuilder& ab, Address _) {
	//				return forth::loadLowerImmediate48(r, ab.absoluteLabelAddress(name));
	//			});
	//}
	//EagerInstruction loadImmediate64(TargetRegister r, Address value) {
	//	return [r, value](AssemblerBuilder& ab) {
	//		ab.addInstruction(loadLowerImmediate48(r, value), setImmediate64_Highest(r, value));
	//	};
	//}
	//EagerInstruction loadImmediate64(TargetRegister r, const std::string& name) {
	//	return [r, name](AssemblerBuilder& ab) {
	//		ab.addInstruction(loadLowerImmediate48(r, name));
	//		ab.addInstruction(setImmediate16_Highest(r, name));
	//	};
	//}
	//SizedResolvableLazyFunction conditionalBranch(TargetRegister cond, const std::string& name) {
	//	return std::make_tuple(getInstructionWidth(Operation::ConditionalBranch),
	//				[name, cond](AssemblerBuilder& ab, Address from) {
	//					auto addr = ab.relativeLabelAddress(name, from);
	//					if (addr > 32767 || addr < -32768) {
	//						throw Problem("conditionalBranch", "Can't encode label into a relative 16-bit offset!");
	//					} else {
	//						return conditionalBranch(cond, addr);
	//					}
	//				});
	//}
	//EagerInstruction storeImmediate64(Address addr, const std::string& name) {
	//	return [addr, name](AssemblerBuilder& ab) {
	//		ab.addInstruction(loadImmediate64(TargetRegister::X, addr),
	//						  loadImmediate64(TargetRegister::Temporary, name),
	//						  store(TargetRegister::X, TargetRegister::Temporary));
	//	};
	//}
    //EagerInstruction storeImmediate64(TargetRegister addr, Address value) {
    //    if (addr == TargetRegister::Temporary) {
    //        throw Problem("Assembler::storeImmediate64", "Temporary register already in use!");
    //    } else {
    //        if (value == 0) {
    //            return [addr](AssemblerBuilder& ab) { ab.addInstruction(store(addr, TargetRegister::Zero)); };
    //        } else {
    //            return [addr, value](AssemblerBuilder& ab) {
    //                ab.addInstruction(loadImmediate64(TargetRegister::Temporary, value),
    //                                  store(addr, TargetRegister::Temporary));
    //            };
    //        }
    //    }
    //}
    //EagerInstruction storeImmediate64(Address value) {
    //    return storeImmediate64(TargetRegister::X, value);
    //}
    //EagerInstruction storeImmediate64(Address addr, Address value) {
    //    return [addr, value](AssemblerBuilder& ab) {
    //        ab.addInstruction(loadImmediate64(TargetRegister::X, addr),
    //                          storeImmediate64(TargetRegister::X, value));
    //    };
    //}
    //EagerInstruction indirectLoad(TargetRegister dest, TargetRegister src) {
    //    if (dest == src) {
    //        return [dest, src](AssemblerBuilder& ab) {
    //            ab.addInstruction(load(dest, src), 
    //                    load(dest, dest));
    //        };
    //    } else {
    //        if (src == TargetRegister::Temporary) {
    //            throw Problem("indirectLoad", "Temporary already in use!");
    //        }
    //        return [dest, src](AssemblerBuilder& ab) {
    //            ab.addInstruction(load(TargetRegister::Temporary, src),
    //                    load(dest, TargetRegister::Temporary));
    //        };
    //    }
    //}
    //EagerInstruction pushImmediate(Address value, TargetRegister sp) {
    //    return [value, sp](AssemblerBuilder& ab) {
    //        ab.addInstruction(loadImmediate64(TargetRegister::Temporary, value),
    //                pushRegister(TargetRegister::Temporary, sp));
    //    };
    //}
    //EagerInstruction pushImmediate(const Datum& value, TargetRegister sp) {
    //    return pushImmediate(value.address, sp);
    //}

} // end namespace forth
