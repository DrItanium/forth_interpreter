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
#define DispatchOneRegister(title) 
#define DispatchTwoRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src) noexcept { return op ## title ({dest, src}); }
#define DispatchThreeRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, TargetRegister src1) noexcept { return op ## title ({dest, src, src1}); }
#define DispatchSignedImm16(title)
#define DispatchImmediate24(title)
#define DispatchTwoRegisterWithImm16(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept { return op ## title ( {dest, src, value}); }
#define DispatchOneRegisterWithImm16(title) Core:: title op ## title (TargetRegister dest, QuarterAddress value) noexcept { return op ## title ( {dest, value}); }
#define DispatchFourRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, TargetRegister src1, TargetRegister src2) noexcept { return op ## title ( { dest, src, src1, src2 }); }
#define DispatchFiveRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, TargetRegister src1, TargetRegister src2, TargetRegister src3) noexcept { return op ## title ( { dest, src, src1, src2, src3 }); }
#define DispatchOneRegisterWithImm64(title) Core:: title op ## title (TargetRegister dest, Address addr) noexcept { return op ## title ( {dest, addr}); }
#define DispatchOneRegisterWithImm48(title) Core:: title op ## title (TargetRegister dest, Address addr) noexcept { return op ## title ( {dest, addr}); }
#define DispatchOneRegisterWithImm32(title) Core:: title op ## title (TargetRegister dest, HalfAddress addr) noexcept { return op ## title ( {dest, addr}); }
#define OneByte(title) Core:: title op ## title () noexcept { return Core:: title (); }
#define TwoByte(title, b) \
	Core:: title op ## title (const Core:: b & x) noexcept { \
		Core:: title value; \
		value.args = x; \
		return value; \
	} \
    INDIRECTION(Dispatch, b)(title)
#define ThreeByte(title, b) \
	Core:: title op ## title (const Core:: b & x) noexcept { \
		Core:: title value; \
		value.args = x; \
		return value; \
	} \
    INDIRECTION(Dispatch, b)(title)
#define FourByte(title, b) \
	Core:: title op ## title (const Core:: b & x) noexcept { \
		Core:: title value; \
		value.args = x; \
		return value; \
	} \
    INDIRECTION(Dispatch, b)(title)
#define GrabBag(title, b) \
	Core:: title op ## title (const Core:: b & x) noexcept { \
		Core:: title value; \
		value.args = x; \
		return value; \
	} \
    INDIRECTION(Dispatch, b)(title)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef GrabBag
#undef DispatchOneRegister
#undef DispatchTwoRegister
#undef DispatchThreeRegister
#undef DispatchSignedImm16
#undef DispatchImmediate24
#undef DispatchTwoRegisterWithImm16
#undef DispatchOneRegisterWithImm16
#undef DispatchFiveRegister
#undef DispatchFourRegister
#undef DispatchOneRegisterWithImm48
#undef DispatchOneRegisterWithImm32
#undef DispatchOneRegisterWithImm64
	Core::Move zeroRegister(TargetRegister reg) noexcept {
		return opMove(reg, TargetRegister::Zero);
	}
	Core::LoadImmediate64 loadImmediate64(TargetRegister reg, Address value) noexcept {
		return opLoadImmediate64(reg, value);
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
	Core::PopRegister popA() noexcept { return opPopRegister(TargetRegister::A, TargetRegister::SP); }
	Core::PopRegister popB() noexcept { return opPopRegister(TargetRegister::B, TargetRegister::SP); }
	Core::PopRegister popC() noexcept { return opPopRegister(TargetRegister::C, TargetRegister::SP); }
	Core::PushRegister pushA() noexcept { return opPushRegister(TargetRegister::A, TargetRegister::SP); }
	Core::PushRegister pushB() noexcept { return opPushRegister(TargetRegister::B, TargetRegister::SP); }
	Core::PushRegister pushC() noexcept { return opPushRegister(TargetRegister::C, TargetRegister::SP); }
	SizedResolvableLazyFunction jumpRelative(const std::string& name) {
		return std::make_tuple(getInstructionWidth(FourByteOpcode::Jump), 
							[name](AssemblerBuilder& ab, Address from) {
								auto addr = ab.relativeLabelAddress(name, from);
								if (addr > 32767 || addr < -32768) {
									throw Problem("jumpRelative", "Can't encode label address into 16-bits");
								} else {
                                    return opJump(addr);
								}
							   });
	}
	SizedResolvableLazyFunction jumpAbsolute(const std::string& name) {
		return std::make_tuple(getInstructionWidth(FourByteOpcode::JumpAbsolute),
							   [name](AssemblerBuilder& ab, Address from) {
									auto addr = ab.absoluteLabelAddress(name);
									if (addr > 0xFFFF) {
										throw Problem("jumpAbsolute", "Can't encode label address into 16-bits");
									} else {
										return opJumpAbsolute(HalfAddress(addr) & 0x00FFFFFF);
									}
							   });
	}
	SizedResolvableLazyFunction loadImmediate48(TargetRegister r, const std::string& name) {
		return std::make_tuple(getInstructionWidth(GrabBagOpcode::LoadImmediate48),
				[name, r](AssemblerBuilder& ab, Address _) {
                    return opLoadImmediate48(r, ab.absoluteLabelAddress(name) & 0x0000'FFFF'FFFF'FFFF);
				});
	}
	SizedResolvableLazyFunction loadImmediate32(TargetRegister r, const std::string& name) {
        return std::make_tuple(getInstructionWidth(GrabBagOpcode::LoadImmediate32),
                [name, r](AssemblerBuilder& ab, Address _) {
                    return opLoadImmediate32(r, HalfAddress(ab.absoluteLabelAddress(name)) );
                });
	}
	SizedResolvableLazyFunction loadImmediate16(TargetRegister r, const std::string& name) {
        return std::make_tuple(getInstructionWidth(GrabBagOpcode::LoadImmediate16),
                [name, r](AssemblerBuilder& ab, Address _) {
                    return opLoadImmediate16(r, QuarterAddress(ab.absoluteLabelAddress(name)) );
                });
	}
	SizedResolvableLazyFunction loadImmediate64(TargetRegister r, const std::string& name) {
        return std::make_tuple(getInstructionWidth(GrabBagOpcode::LoadImmediate64),
                [name, r](AssemblerBuilder& ab, Address _) {
                    return opLoadImmediate64(r, ab.absoluteLabelAddress(name));
                });
	}
	SizedResolvableLazyFunction conditionalBranch(TargetRegister cond, const std::string& name) {
		return std::make_tuple(getInstructionWidth(FourByteOpcode::ConditionalBranch),
					[name, cond](AssemblerBuilder& ab, Address from) {
						auto addr = ab.relativeLabelAddress(name, from);
						if (addr > 32767 || addr < -32768) {
							throw Problem("conditionalBranch", "Can't encode label into a relative 16-bit offset!");
						} else {
							return opConditionalBranch(cond, safeExtract(QuarterInteger(addr)));
						}
					});
	}
    EagerInstruction indirectLoad(TargetRegister dest, TargetRegister src) {
        if (dest == src) {
            return [dest, src](AssemblerBuilder& ab) {
                ab.addInstruction(opLoad(dest, src), opLoad(dest, dest));
            };
        } else {
            if (src == TargetRegister::Temporary) {
                throw Problem("indirectLoad", "Temporary already in use!");
            }
            return [dest, src](AssemblerBuilder& ab) {
                ab.addInstruction(opLoad(TargetRegister::Temporary, src),
                        opLoad(dest, TargetRegister::Temporary));
            };
        }
    }
    EagerInstruction pushImmediate(Address value, TargetRegister sp) {
        return [value, sp](AssemblerBuilder& ab) {
            ab.addInstruction(opLoadImmediate64(TargetRegister::Temporary, value),
                              opPushRegister(TargetRegister::Temporary, sp));
        };
    }
    EagerInstruction pushImmediate(const Datum& value, TargetRegister sp) {
        return pushImmediate(value.address, sp);
    }
    EagerInstruction opPrintChar(char c) {
        return [c](AssemblerBuilder& ab) {
            ab.addInstruction(opAddImmediate(TargetRegister::Temporary, TargetRegister::Zero, QuarterAddress(c)),
                              opPrintChar(TargetRegister::Temporary));
        };
    }
    EagerInstruction opPrintChar(const std::string& str) {
        return [str](AssemblerBuilder& ab) {
            for (auto const& c : str) {
                ab.addInstruction(opPrintChar(c));
            }
        };
    }

} // end namespace forth
