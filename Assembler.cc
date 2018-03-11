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
#define DispatchOneRegister(title) 
#define DispatchTwoRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src) noexcept { return op ## title ({dest, src}); }
#define DispatchThreeRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, TargetRegister src1) noexcept { return op ## title ({dest, src, src1}); }
#define DispatchSignedImm16(title)
#define DispatchImmediate24(title) Core::title op ## title (HalfAddress addr) noexcept { return op ## title ({addr}); }
#define DispatchTwoRegisterWithImm16(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept { return op ## title ( {dest, src, value}); }
#define DispatchOneRegisterWithImm16(title) Core:: title op ## title (TargetRegister dest, QuarterAddress value) noexcept { return op ## title ( {dest, value}); }
#define DispatchFourRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, TargetRegister src1, TargetRegister src2) noexcept { return op ## title ( { dest, src, src1, src2 }); }
#define DispatchFiveRegister(title) Core:: title op ## title (TargetRegister dest, TargetRegister src, TargetRegister src1, TargetRegister src2, TargetRegister src3) noexcept { return op ## title ( { dest, src, src1, src2, src3 }); }
#define DispatchOneRegisterWithImm64(title) Core:: title op ## title (TargetRegister dest, Address addr) noexcept { return op ## title ( {dest, addr}); }
#define DispatchOneRegisterWithImm48(title) Core:: title op ## title (TargetRegister dest, Address addr) noexcept { return op ## title ( {dest, addr}); }
#define DispatchOneRegisterWithImm32(title) Core:: title op ## title (TargetRegister dest, HalfAddress addr) noexcept { return op ## title ( {dest, addr}); }
#define DispatchNoArguments(title) Core:: title op ## title () noexcept { return Core:: title () ; }
#define X(title, b) \
	Core:: title op ## title (const Core:: b & x) noexcept { \
		Core:: title value; \
		value.args = x; \
		return value; \
	} \
	INDIRECTION(Dispatch, b)(title)
#define FirstX(title, b) X(title, b)
#include "InstructionData.def"
#undef X
#undef FirstX
#undef DispatchNoArguments
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
	void AssemblerBuilder::addInstruction(EagerInstruction fn) { fn(*this); }
	void AssemblerBuilder::addInstruction(ResolvableLazyFunction fn) {
		_toResolve.emplace_back([from = _currentLocation, fn](AssemblerBuilder& ab) {
					return fn(ab, from); 
				});
	}
	void AssemblerBuilder::addInstruction(const Core::DecodedOperation& op) {
		_operations.emplace(_currentLocation, op);
		byte result = std::visit([](auto&& value) constexpr { return value.size(); }, op);
		_currentLocation += result;
	}
	EagerInstruction opJump(const std::string& name) {
		return [name](AssemblerBuilder& ab) {
			auto jmp = opJump(Core::SignedImm16(0));
			ResolvableLazyFunction fn = [name, &jmp](AssemblerBuilder& ab, Address from) {
								auto addr = ab.relativeLabelAddress(name);
								if (addr > 32767 || addr < - 32768) {
									throw Problem("jumpRelative", "Can't encode label address into 16-bits");
								} else {
									jmp.args.value = addr;
								}
							  };
			ab.addInstruction(jmp, fn);
		};
	}
	EagerInstruction opJumpAbsolute(const std::string& name) {
		return [name](AssemblerBuilder& ab) {
			auto jmp = opJumpAbsolute({HalfAddress(0)});
			ResolvableLazyFunction fn = [name, &jmp](AssemblerBuilder& ab, Address from) {
				auto addr = ab.absoluteLabelAddress(name);
				if (addr > 0xFFFFFF) {
					throw Problem("opJumpAbsolute", "Can't encode label that is outside the first 24 bits");
				} else {
					jmp.args.imm24 = (HalfAddress(addr) & 0x00FFFFFF);
				}
			};
			ab.addInstruction(jmp, fn);
		};
	}
	EagerInstruction opLoadImmediate32(TargetRegister r, const std::string& name) {
		return [r, name](AssemblerBuilder& ab) {
			auto ld = opLoadImmediate32(r, 0);
			ResolvableLazyFunction fn = [name, &ld](AssemblerBuilder& ab, Address _) {
				auto addr = ab.absoluteLabelAddress(name);
				ld.args.imm32 = forth::getLowerHalf(addr);
			};
			ab.addInstruction(ld, fn);
		};
	}
	EagerInstruction opLoadImmediate16(TargetRegister r, const std::string& name) {
		return [r, name](AssemblerBuilder& ab) {
			auto add = opUnsignedAddImmediate(r, TargetRegister::Zero, 0);
			ResolvableLazyFunction fn = [name, &add](auto& ab, auto from) {
				auto addr = ab.absoluteLabelAddress(name);
				add.args.imm16 = QuarterAddress(addr);
			};
			ab.addInstruction(add, fn);
		};
	}
	EagerInstruction opLoadImmediate64(TargetRegister r, const std::string& name) {
		return [name, r](AssemblerBuilder& ab) {
			auto ld = opLoadImmediate64(r, 0);
			ResolvableLazyFunction fn = [name, &ld](AssemblerBuilder& ab, Address _) {
				ld.args.imm64 = ab.absoluteLabelAddress(name);
			};
		};
	}
	EagerInstruction opConditionalBranch(TargetRegister r, const std::string& name) {
		return [r, name](auto& ab) {
			auto cond = opConditionalBranch(r, 0);
			ResolvableLazyFunction fn = [name, &cond](auto& ab, auto from) {
				auto addr = ab.relativeLabelAddress(name, from);
				if (addr > 32767 || addr < -32768) {
					throw Problem("opConditionalBranch", "Can't encode label into a relative 16-bit offset!");
				} else {
					cond.args.imm16 = safeExtract(QuarterInteger(addr));
				}
			};
			ab.addInstruction(cond, fn);
		};
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
    EagerInstruction opPushImmediate64(Address value, TargetRegister sp) {
        return [value, sp](AssemblerBuilder& ab) {
            ab.addInstruction(opLoadImmediate64(TargetRegister::Temporary, value),
                              opPushRegister(TargetRegister::Temporary, sp));
        };
    }
    EagerInstruction opPushImmediate64(const Datum& value, TargetRegister sp) {
        return opPushImmediate64(value.address, sp);
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
	EagerInstruction storeImmediate64(TargetRegister dest, Address value) {
		if (dest == TargetRegister::Temporary) {
			throw Problem("storeImmediate64", "Destination cannot be temp as it will lead to unpredictable behavior");
		}
		return [dest, value](AssemblerBuilder& ab) {
			ab.addInstruction(opLoadImmediate64(TargetRegister::Temporary, value),
							  opStore(dest, TargetRegister::Temporary));
		};
	}

	EagerInstruction storeImmediate64(Address addr, Address value) {
		return [addr, value](AssemblerBuilder& ab) {
			ab.addInstruction(opLoadImmediate64(TargetRegister::Temporary2, addr),
							  storeImmediate64(TargetRegister::Temporary2, value));
		};
	}

	EagerInstruction storeImmediate64(TargetRegister addr, const std::string& value) {
		return [addr, value](AssemblerBuilder& ab) {
			ab.addInstruction(opLoadImmediate64(TargetRegister::Temporary, value),
							  opStore(addr, TargetRegister::Temporary));
		};
	}

	EagerInstruction storeImmediate64(Address addr, const std::string& value) {
		return [addr, value](AssemblerBuilder& ab) {
			ab.addInstruction(opLoadImmediate64(TargetRegister::Temporary2, addr),
							  opLoadImmediate64(TargetRegister::Temporary, value),
							  opStore(TargetRegister::Temporary2, TargetRegister::Temporary));
		};
	}

} // end namespace forth
