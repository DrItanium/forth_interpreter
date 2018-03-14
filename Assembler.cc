#include "Types.h"
#include "Instruction.h"
#include "Assembler.h"
#include "Core.h"
#include "Machine.h"

namespace forth {
	constexpr bool outOfRange16(Integer value) noexcept {
		constexpr Integer maxRange = 32767;
		constexpr Integer minRange = -32768;
		return (value > maxRange) || (value < minRange);
	}
	static constexpr HalfAddress mask24 = 0x00FF'FFFF;
	HalfAddress make24bit(Address input) {
		if (input > mask24) {
			throw Problem("make24bit", "Provided address is larger than 24-bits");
		} else {
			return HalfAddress(input) & mask24;
		}
	}
	AssemblerBuilder::AssemblerBuilder(Address baseAddress) : _baseAddress(baseAddress), _currentLocation(baseAddress), _gensymIndex(0) {}
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
#define DispatchCustomTwoRegisterWithImm16(title) 
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
        return instructions(opPushRegister(reg), body, opPopRegister(reg));
	}

	EagerInstruction label(const std::string& str) {
		return [str](auto& ab) { ab.labelHere(str); };
	}
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
								if (outOfRange16(addr)) {
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
				jmp.args.imm24 = make24bit(ab.absoluteLabelAddress(name));
			};
			ab.addInstruction(jmp, fn);
		};
	}
	EagerInstruction opLoadImmediate32(TargetRegister r, const std::string& name) {
		return [r, name](AssemblerBuilder& ab) {
			auto ld = opLoadImmediate32(r, 0);
			ResolvableLazyFunction fn = [name, &ld](AssemblerBuilder& ab, Address _) {
				ld.args.imm32 = forth::getLowerHalf(ab.absoluteLabelAddress(name));
			};
			ab.addInstruction(ld, fn);
		};
	}
	EagerInstruction opLoadImmediate16(TargetRegister r, QuarterAddress value) {
        if (value == 0) {
            return instructions(zeroRegister(r));
        } else {
            return instructions(opUnsignedAddImmediate(r, TargetRegister::Zero, value));
        }
	}
	EagerInstruction opLoadImmediate16(TargetRegister r, const std::string& name) {
		return [r, name](AssemblerBuilder& ab) {
			auto add = opUnsignedAddImmediate(r, TargetRegister::Zero, 0);
			ResolvableLazyFunction fn = [name, &add](auto& ab, auto from) {
				add.args.imm16 = QuarterAddress(ab.absoluteLabelAddress(name));
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
				if (outOfRange16(addr)) {
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
            return instructions(opLoad(dest, src), 
                    opLoad(dest, dest));
        } else {
            if (src == TargetRegister::Temporary) {
                throw Problem("indirectLoad", "Temporary already in use!");
            }
            return instructions(opLoad(TargetRegister::Temporary, src),
                    opLoad(dest, TargetRegister::Temporary));
        }
    }
    EagerInstruction opPushImmediate64(Address value, TargetRegister sp) {
        return instructions(opLoadImmediate64(TargetRegister::Temporary, value),
                opPushRegister(TargetRegister::Temporary, sp));
    }
    EagerInstruction opPushImmediate64(const Datum& value, TargetRegister sp) {
        return opPushImmediate64(value.address, sp);
    }
    EagerInstruction opPrintChar(char c) {
        return instructions(opAddImmediate(TargetRegister::Temporary, TargetRegister::Zero, QuarterAddress(c)),
                opPrintChar(TargetRegister::Temporary));
    }
    EagerInstruction opPrintChar(const std::string& str) {
        return [str](AssemblerBuilder& ab) {
            for (auto const& c : str) {
                ab.addInstruction(opPrintChar(c));
            }
        };
    }
	EagerInstruction opStoreImmediate64(TargetRegister dest, Address value) {
		if (dest == TargetRegister::Temporary) {
			throw Problem("opStoreImmediate64", "Destination cannot be temp as it will lead to unpredictable behavior");
		}
        return instructions(opLoadImmediate64(TargetRegister::Temporary, value),
                opStore(dest, TargetRegister::Temporary));
	}

	EagerInstruction opStoreImmediate64(Address addr, Address value) {
        return instructions( opLoadImmediate64(TargetRegister::Temporary2, addr),
                opStoreImmediate64(TargetRegister::Temporary2, value));
	}

	EagerInstruction opStoreImmediate64(TargetRegister addr, const std::string& value) {
        return instructions(opLoadImmediate64(TargetRegister::Temporary, value),
                opStore(addr, TargetRegister::Temporary));
	}

	EagerInstruction opStoreImmediate64(Address addr, const std::string& value) {
        return instructions(opLoadImmediate64(TargetRegister::Temporary2, addr),
                opLoadImmediate64(TargetRegister::Temporary, value),
                opStore(TargetRegister::Temporary2, TargetRegister::Temporary));
	}

	EagerInstruction subroutineCall(Address addr) {
		return [addr](AssemblerBuilder& ab) {
			if (addr <= mask24) {
				// do a non indirect call!
				ab.addInstruction(opCallSubroutineAbsolute(HalfAddress(addr) & mask24));
			} else {
				// We need to load the address into temporary
				// Then we call the address via temporary
				ab.addInstruction(opLoadImmediate64(TargetRegister::Temporary, addr),
								  opCallSubroutineIndirect(TargetRegister::Temporary));
			}
		};
	}
	EagerInstruction semicolon() {
        return instructions(opReturnSubroutine());
	}

	static constexpr Address mask16 = 0xFFFF;
	static constexpr Address mask32 = 0xFFFF'FFFF;
	static constexpr Address mask64 = 0xFFFF'FFFF'FFFF'FFFF;
	EagerInstruction opStoreImmediate(TargetRegister addr, Address value) {
		if (addr == TargetRegister::Temporary2) {
			throw Problem("storeImmediate", "Cannot use Temporary2 as the address");
		}
		if (value == 0) {
            return instructions(opStore(addr, TargetRegister::Zero));
		} else {
            return instructions(opLoadImmediate(TargetRegister::Temporary2, value),
                    opStore(addr, TargetRegister::Temporary2));
		}
	}

	EagerInstruction opLoadImmediate(TargetRegister addr, Address value) {
		if (value == 0) {
            return instructions(zeroRegister(addr));
        } else if (value <= mask16) {
            return instructions(opLoadImmediate16(addr, QuarterAddress(value)));
        } else if (value <= mask32) {
            return instructions(opLoadImmediate32(addr, HalfAddress(value)));
        } else if (value == mask64) {
            return instructions(opNot(addr, TargetRegister::Zero));
        } else {
            return instructions(opLoadImmediate64(addr, value));
        }
	}

	EagerInstruction opStoreImmediate(Address addr, Address value) {
		if (addr == 0) {
			return opStoreImmediate(TargetRegister::Zero, value);
		} else {
            return instructions(opLoadImmediate(TargetRegister::Temporary, addr),
                    opStoreImmediate(TargetRegister::Temporary, value));
		}
	}

    EagerInstruction opEquals(TargetRegister dest, TargetRegister src, Address addr) {
        if (dest == TargetRegister::Temporary2 || src == TargetRegister::Temporary2) {
            throw Problem("opEquals", "Cannot use temporary2 as destination or source!");
        }
        if (addr == 0) {
            return instructions(opEquals(dest, src, TargetRegister::Zero));
        } else {
            return instructions(opLoadImmediate(TargetRegister::Temporary2, addr),
                    opEquals(dest, src, TargetRegister::Temporary2));
        }
    }
    EagerInstruction opMemoryEquals(TargetRegister dest, TargetRegister src, Address addr) {
        if (dest == TargetRegister::Temporary2 || src == TargetRegister::Temporary2) {
            throw Problem("opEquals", "Cannot use temporary2 as destination or source!");
        }
        return instructions(opLoadImmediate(TargetRegister::Temporary2, addr),
                opLoad(TargetRegister::Temporary2, TargetRegister::Temporary2),
                opEquals(dest, src, TargetRegister::Temporary2));
    }

    EagerInstruction opSubroutineStackEmpty(TargetRegister dest) {
        return opMemoryEquals(dest, TargetRegister::SP2, Machine::subroutineStackEmptyLocation);
    }
    EagerInstruction opSubroutineStackFull(TargetRegister dest) {
        return opMemoryEquals(dest, TargetRegister::SP2, Machine::subroutineStackFullLocation);
    }
    EagerInstruction opParameterStackEmpty(TargetRegister dest) {
        return opMemoryEquals(dest, TargetRegister::SP, Machine::parameterStackEmptyLocation);
    }
    EagerInstruction opParameterStackFull(TargetRegister dest) {
        return opMemoryEquals(dest, TargetRegister::SP, Machine::parameterStackFullLocation);
    }

    EagerInstruction ifThenElseStatement(TargetRegister cond, Address onTrue, Address onFalse) {
        if (onFalse == Address(-1)) {
            return instructions(opLoadImmediate(TargetRegister::Temporary, onTrue),
                    opConditionalCallSubroutineIndirect(cond, TargetRegister::Temporary));
        } else {
            constexpr auto t = TargetRegister::Temporary;
            constexpr auto f = TargetRegister::Temporary2;
            return instructions(opLoadImmediate(t, onTrue),
                                opLoadImmediate(f, onFalse),
                                opCallIfStatement(cond, t, f));
        }
    }

    EagerInstruction opMultiplyImmediate(TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept {
        auto fn = [dest, src](auto count) {
            return instructions(opShiftLeftImmediate(dest, src, count));
        };
        switch (value) {
            case 0:
                return instructions(zeroRegister(dest));
            case 1:
                return instructions(opMove(dest, src));
            case 2:
                return instructions(opAdd(dest, src, src));
            case 4: return fn(2);
            case 8: return fn(3);
            case 16: return fn(4);
            case 32: return fn(5);
            case 64: return fn(6);
            case 128: return fn(7);
            case 256: return fn(8);
            case 512: return fn(9);
            case 1024: return fn(10);
            case 4096: return fn(11);
            case 8192: return fn(12);
            case 16384: return fn(13);
            case 32768: return fn(14);
            default: return instructions(opMultiplyImmediate({dest, src, value}));
        }
    }

    EagerInstruction opUnsignedMultiplyImmediate(TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept {
        auto fn = [dest, src](auto count) {
            return instructions(opUnsignedShiftLeftImmediate(dest, src, count));
        };
        switch (value) {
            case 0:
                return instructions(zeroRegister(dest));
            case 1:
                return instructions(opMove(dest, src));
            case 2:
                return instructions(opAddUnsigned(dest, src, src));
            case 4: return fn(2);
            case 8: return fn(3);
            case 16: return fn(4);
            case 32: return fn(5);
            case 64: return fn(6);
            case 128: return fn(7);
            case 256: return fn(8);
            case 512: return fn(9);
            case 1024: return fn(10);
            case 4096: return fn(11);
            case 8192: return fn(12);
            case 16384: return fn(13);
            case 32768: return fn(14);
            default: return instructions(opUnsignedMultiplyImmediate({dest, src, value}));
        }
    }
	EagerInstruction opNotEqualImmediate(TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept {
		if (value == 0) {
			return instructions(opNotEqual(dest, src, TargetRegister::Zero));
		} else {
			return instructions(opNotEqualImmediate({dest, src, value}));
		}
	}

	EagerInstruction opUnsignedNotEqualImmediate(TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept {
		if (value == 0) {
			return instructions(opNotEqualUnsigned(dest, src, TargetRegister::Zero));
		} else {
			return instructions(opUnsignedNotEqualImmediate({dest, src, value}));
		}
	}
	EagerInstruction opLessThanOrEqualToImmediate(TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept {
		if (value == 0) {
			return instructions(opLessThanOrEqualTo(dest, src, TargetRegister::Zero));
		} else {
			return instructions(opLessThanOrEqualToImmediate({dest, src, value}));
		}
	}

	EagerInstruction opUnsignedLessThanOrEqualToImmediate(TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept {
		if (value == 0) {
			return instructions(opLessThanOrEqualToUnsigned(dest, src, TargetRegister::Zero));
		} else {
			return instructions(opUnsignedLessThanOrEqualToImmediate({dest, src, value}));
		}
	}

	EagerInstruction opGreaterThanOrEqualToImmediate(TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept {
		if (value == 0) {
			return instructions(opGreaterThanOrEqualTo(dest, src, TargetRegister::Zero));
		} else {
			return instructions(opGreaterThanOrEqualToImmediate({dest, src, value}));
		}
	}

	EagerInstruction opUnsignedGreaterThanOrEqualToImmediate(TargetRegister dest, TargetRegister src, QuarterAddress value) noexcept {
		if (value == 0) {
			return instructions(opGreaterThanOrEqualToUnsigned(dest, src, TargetRegister::Zero));
		} else {
			return instructions(opUnsignedGreaterThanOrEqualToImmediate({dest, src, value}));
		}
	}

	std::string AssemblerBuilder::gensym() noexcept {
		std::stringstream str;
		str << "gensym" << _gensymIndex;
		auto x = str.str();
		++_gensymIndex;
		return x;
	}


} // end namespace forth
