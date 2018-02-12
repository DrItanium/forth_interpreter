// Currently on chapter 4.4.4
// TODO: support nested if statements
#include <iostream>
#include <string>
#include <list>
#include <memory>
#include <limits>
#include <sstream>
#include <cmath>
#include "Types.h"
#include "Problem.h"
#include "Datum.h"
#include "DictionaryEntry.h"
#include "Machine.h"

namespace forth {
	static constexpr Address storeFalse = Instruction::encodeOperation(
			Instruction::xorOp(TargetRegister::C, TargetRegister::A, TargetRegister::A),
			Instruction::store(TargetRegister::X, TargetRegister::C));
	void Machine::seeWord(const DictionaryEntry* entry) {
		if (entry->isFake()) {
			_output << "compiled entry: { " << std::endl;
		} else {
			_output << entry->getName() << ": " << std::endl;
		}
		if (entry->getCode() != nullptr) {
			_output << "\tNative Operation!" << std::endl;
		} else {
			auto flags = _output.flags();
			auto outputDictionaryEntry = [this, entry](auto space) {
                auto innerTarget = std::get<const DictionaryEntry*>(space->_data);
				if (innerTarget->isFake()) {
					seeWord(innerTarget);
				} else {
					_output << innerTarget->getName();
				}
			};
			auto outputWord = [this, entry](auto space) {
                auto innerTarget = std::get<const DictionaryEntry*>(space->_data);
				if (innerTarget->isFake()) {
					_output << "0x" << std::hex << innerTarget;
				} else {
					_output << innerTarget->getName();
				}
			};
			for (auto x = entry->begin(); x != entry->end(); ++x) {
				using Type = decltype(x->_type);
				_output << "\t";
				switch (x->_type) {
					case Type::Signed:
						_output << std::dec << std::get<Integer>(x->_data);
						break;
					case Type::Unsigned:
						_output << std::hex << "0x" << std::get<Address>(x->_data);
						break;
					case Type::FloatingPoint:
						_output << std::dec << std::get<Floating>(x->_data);
						break;
					case Type::Boolean:
						_output << std::boolalpha << std::get<bool>(x->_data);
						break;
					case Type::DictEntry:
						outputDictionaryEntry(x);
						break;
					case Type::Word:
						outputWord(x);
						break;
					default:
						_output << std::endl;
						throw Problem("see", "Unknown entry type!");
				}
				_output << std::endl;
			}
			_output.setf(flags);
		}
		if (entry->isFake()) {
			_output << "}" << std::endl;
		}
	}
	void Machine::seeWord() {
		// read the next word
		auto word = readWord();
		const auto* entry = lookupWord(word);
		if (entry != nullptr) {
			seeWord(entry);
		} else {
			std::stringstream str;
			str << word << " is not a word!";
			auto msg = str.str();
			throw Problem("see", msg);
		}
	}
	const DictionaryEntry* Machine::lookupWord(const std::string& word) noexcept {
		if (_words == nullptr) {
			return nullptr;
		}
		for (const auto* entry = _words; entry != nullptr; entry = entry->getNext()) {
			if (entry->getName() == word && !entry->isFake()) {
				return entry;
			}
		}
		return nullptr;
	}
	void Machine::chooseRegister() {
		_core.getRegister(TargetRegister::C) = _core.getRegister(TargetRegister::C).getTruth() ? _core.getRegister(TargetRegister::A) : _core.getRegister(TargetRegister::B);
	}
	void Machine::invokeCRegister() {
		using Type = decltype(_core.getRegister(TargetRegister::C).getType());
		switch (_core.getRegister(TargetRegister::C).getType()) {
			case Type::Word:
				_core.getRegister(TargetRegister::C).getWord()->operator()(this);
				break;
			case Type::Number:
			case Type::FloatingPoint:
				pushRegister(TargetRegister::C);
				break;
			default:
				throw Problem("invoke.c", "incorrect discriminant!");
		}
	}
	void Machine::ifCondition() {
		static constexpr auto prepRegisters = Instruction::encodeOperation(
				Instruction::popRegister(TargetRegister::TB),
				Instruction::popB(), 
				Instruction::popRegister(TargetRegister::TA),
				Instruction::popA(),
				Instruction::popC());
		// if we're not in compilation mode then error out
		if (!inCompilationMode()) {
			throw Problem("if", "must be defining a word!");
		}
		auto currentTarget = _compileTarget;
		pushSubroutine(currentTarget);
		_compileTarget = new DictionaryEntry("");
		_compileTarget->markFakeEntry();
		auto* elseBlock = new DictionaryEntry("");
		elseBlock->markFakeEntry();
		pushSubroutine(elseBlock);

		currentTarget->pushWord(_compileTarget);
		currentTarget->addSpaceEntry(static_cast<Address>(Discriminant::Word));
		currentTarget->pushWord(elseBlock);
		currentTarget->addSpaceEntry(static_cast<Address>(Discriminant::Word));
		compileMicrocodeInvoke(prepRegisters, currentTarget);
	}
	void Machine::compileMicrocodeInvoke(const Molecule& m, DictionaryEntry* current) {
		current->addSpaceEntry(m._value);
		current->addSpaceEntry(_microcodeInvoke);
	}
	void Machine::elseCondition() {
		if (!inCompilationMode()) {
			throw Problem("else", "must be defining a word!");
		}
		if (subroutineStackEmpty()) {
            throw Problem("else", "subroutine stack is empty!");
        }
		// pop the else block off of the subroutine stack
		auto* elseBlock = popSubroutine().subroutine;
		pushSubroutine(new DictionaryEntry(""));
		// let the if block dangle off since it is referenced else where
		addWord(_compileTarget);
		_compileTarget = elseBlock;
	}

	void Machine::thenStatement() {
		if (!inCompilationMode()) {
			throw Problem("then", "must be defining a word!");
		}
		if (subroutineStackEmpty()) {
			throw Problem("then", "Not in a function");
		}
		// there will always be a garbage entry at the top of the subroutine stack, just eliminate it
		popSubroutine();
		auto parent = popSubroutine();
		addWord(_compileTarget);
		_compileTarget = parent.subroutine;
		_compileTarget->addSpaceEntry(lookupWord("choose.c"));
		_compileTarget->addSpaceEntry(lookupWord("invoke.c"));
	}
	std::string Machine::readWord() {
		std::string word;
		_input >> word;
		return word;
	}
	void Machine::printRegisters() {
		auto flags = _output.flags();
		auto fn = [this](const std::string& title, auto value) noexcept {
			_output << title << ": " << value << std::endl;
		};
		fn("A", _core.getRegister(TargetRegister::A).getValue());
		fn("B", _core.getRegister(TargetRegister::B).getValue());
		fn("C", _core.getRegister(TargetRegister::C).getValue());
		fn("T", _core.getRegister(TargetRegister::C).getType());
		fn("S", _core.getRegister(TargetRegister::S).getValue());
		fn("X", _core.getRegister(TargetRegister::X).getValue());
		fn("SP", _core.getRegister(TargetRegister::SP).getValue());
		fn("SP2", _core.getRegister(TargetRegister::SP2).getValue());
		fn("A.T", _core.getRegister(TargetRegister::A).getType());
		fn("B.T", _core.getRegister(TargetRegister::B).getType());
		fn("X.T", _core.getRegister(TargetRegister::X).getType());
		_output.setf(flags); // restore after done
	}
	void Machine::defineWord() {
		if (inCompilationMode() || _compileTarget != nullptr) {
			throw Problem(":", "already compiling");
		}
		activateCompileMode();
		// pass address "execute" to the entry subroutine
		_compileTarget = new DictionaryEntry(readWord());
	}
	void Machine::endDefineWord() {
		if (!inCompilationMode() || _compileTarget == nullptr) {
			throw Problem(";", "not compiling!");
		}
		deactivateCompileMode();
		addWord(_compileTarget);
		_compileTarget = nullptr;
	}
	void Machine::listWords() {
		if (_words == nullptr) {
			return;
		}
		for (const auto* entry = _words; entry != nullptr; entry = entry->getNext()) {
			if (entry->isFake()) {
				continue;
			}
			_output << "\t - " << entry->getName() << std::endl;
		}
	}
	void Machine::addWord(const std::string& name, NativeMachineOperation op, bool compileTimeInvoke) {
		auto* entry = new DictionaryEntry(name, op);
		if (compileTimeInvoke) {
			entry->markCompileTimeInvoke();
		}
		addWord(entry);
	}

	void Machine::semicolonOperation() {
		// in assembly level impls, this resets the instruction
		// counter to zero, however, with how we use iterators,
		// this isn't necessary!
	}
	void Machine::addWord(DictionaryEntry* entry) {
		if (_words != nullptr) {
			entry->setNext(_words);
		}
		_words = entry;
	}
	void Machine::typeValue(Discriminant discriminant, const Datum& value) {
		auto flags = _output.flags();
		switch(discriminant) {
			case Discriminant::Number:
				_output << std::dec << value.numValue;
				break;
			case Discriminant::FloatingPoint:
				_output << value.fp;
				break;
			case Discriminant::MemoryAddress:
				_output << std::dec << value.address;
				break;
			case Discriminant::Boolean:
				_output << std::boolalpha << value.truth << std::noboolalpha;
				break;
			case Discriminant::Word:
				_output << std::hex << value.entry << ": " << std::dec << value.entry->getName();
			default:
				throw Problem("type.a", "BAD DISCRIMINANT!");
		}
		// always type a space out after the number
		_output << ' ' << std::endl;
		_output.setf(flags);
	}
	Machine::Machine(std::ostream& output, std::istream& input) :  _output(output), _input(input), _words(nullptr) { }

	Datum Machine::popParameter() {
		if (_parameter.empty()) {
			throw Problem("", "STACK EMPTY!");
		}
		auto top(_parameter.front());
		_parameter.pop_front();
		return top;
	}

	void Machine::pushParameter(Datum value) {
		_parameter.emplace_front(value);
	}

	bool Machine::numberRoutine(const std::string& word) noexcept {
		static constexpr auto loadTrueToStack = Instruction::encodeOperation(
				Instruction::setImmediate16_Lowest(TargetRegister::C, 1),
				Instruction::pushC());
		static_assert(loadTrueToStack == 0x1f00010216, "Load true to stack is incorrect!");
		static constexpr auto loadFalseToStack = Instruction::encodeOperation(
				Instruction::setImmediate16_Lowest(TargetRegister::C, 0),
				Instruction::pushC());
		static_assert(loadFalseToStack == 0x1f00000216, "Load false to stack is incorrect!");
        static constexpr auto pushC = Instruction::encodeOperation(Instruction::pushC());
		if (word.empty()) { 
			return false; 
		}
		// floating point
		// integers
		// first do some inspection first
		// We need to load into c and then push it to the stack
		if (word == "true") {
			microcodeInvoke(loadTrueToStack);
			return true;
		}
		if (word == "false") {
			microcodeInvoke(loadFalseToStack);
			return true;
		}
		std::istringstream parseAttempt(word);
		if (word.find('#') != std::string::npos) {
			Address tmpAddress;
			parseAttempt >> std::hex >> tmpAddress;
			if (!parseAttempt.fail()) {
				// TODO: do checks to compact parsing
				microcodeStreamInvoke(
						Instruction::encodeOperation(
							Instruction::setImmediate64_Lowest(TargetRegister::C, tmpAddress),
							Instruction::setImmediate64_Lower(TargetRegister::C, tmpAddress)),
						Instruction::encodeOperation(
							Instruction::setImmediate64_Higher(TargetRegister::C, tmpAddress),
							Instruction::setImmediate64_Highest(TargetRegister::C, tmpAddress)),
                        pushC);
				return true;
			}
			return false;
		}
		if (word.find('u') != std::string::npos) {
			Address tmpAddress;
			parseAttempt >> tmpAddress;
			if (!parseAttempt.fail()) {
				microcodeStreamInvoke(
						Instruction::encodeOperation(
							Instruction::setImmediate64_Lowest(TargetRegister::C, tmpAddress),
							Instruction::setImmediate64_Lower(TargetRegister::C, tmpAddress)),
						Instruction::encodeOperation(
							Instruction::setImmediate64_Higher(TargetRegister::C, tmpAddress),
							Instruction::setImmediate64_Highest(TargetRegister::C, tmpAddress)),
                        pushC);
				return true;
			}
			return false;
		}
		parseAttempt.clear();
		if (word.find('.') != std::string::npos) {
			Floating tmpFloat;
			parseAttempt >> tmpFloat;
			if (!parseAttempt.fail() && parseAttempt.eof()) {
				Datum a(tmpFloat);
				microcodeStreamInvoke(
						Instruction::encodeOperation(
							Instruction::setImmediate64_Lowest(TargetRegister::C, a.address),
							Instruction::setImmediate64_Lower(TargetRegister::C, a.address)),
						Instruction::encodeOperation(
							Instruction::setImmediate64_Higher(TargetRegister::C, a.address),
							Instruction::setImmediate64_Highest(TargetRegister::C, a.address)),
                        pushC);
				return true;
			}
			// get out of here early since we hit something that looks like
			// a float
			return false;
		}
		Integer tmpInt;
		parseAttempt.clear();
		parseAttempt >> tmpInt;
		if (!parseAttempt.fail() && parseAttempt.eof()) {
			Datum a(tmpInt);
			microcodeStreamInvoke(
					Instruction::encodeOperation(
						Instruction::setImmediate64_Lowest(TargetRegister::C, a.address),
						Instruction::setImmediate64_Lower(TargetRegister::C, a.address)),
					Instruction::encodeOperation(
						Instruction::setImmediate64_Higher(TargetRegister::C, a.address),
						Instruction::setImmediate64_Highest(TargetRegister::C, a.address)),
                    pushC);
			return true;
		}
		return false;
	}

	void Machine::controlLoop() noexcept {
		// setup initial dictionary
		initializeBaseDictionary();
        bool ignoreInput = false;
		while (keepExecuting()) {
			try {
				auto result = readWord();
                if (ignoreInput) {
                    if (result == ")") {
                        ignoreInput = false;
                    }
                    continue;
                }
                if (result == "(") {
                    ignoreInput = true;
                    continue;
                }
				auto* entry = lookupWord(result);
				if (inCompilationMode()) {
					auto finishedCompiling = (result == ";");
					if (entry != nullptr) {
						// okay we have a word, lets add it to the top word in the dictionary
						// get the front word first and foremost
						if (entry->compileTimeInvoke()) {
							// add a new spaceEntry to push this value onto the data stack
							entry->operator()(this);
						} else {
							_compileTarget->addSpaceEntry(entry);
							if (finishedCompiling) {
								endDefineWord();
							}
						}
						continue;
					}
				} else {
					if (entry != nullptr) {
						entry->operator()(this);
						continue;
					}
				}
				// okay, we need to see if it is a value to compile in or
				// add to the stack
				if (numberRoutine(result)) {
					continue;
				}
				// fall through case, we couldn't figure it out!
				handleError(result, "?");
			} catch(Problem& p) {
				handleError(p.getWord(), p.getMessage());
			}
		}
	}
    void Machine::raiseError() {
        throw Problem("raiseError", "Raised an error from within interpreter!");
    }
	void Machine::initializeBaseDictionary() {
		if (!_initializedBaseDictionary) {
			_initializedBaseDictionary = true;
			// add dictionary entries
			addWord(";", std::mem_fn(&Machine::semicolonOperation));
			addWord("registers", std::mem_fn(&Machine::printRegisters));
			addWord("words", std::mem_fn(&Machine::listWords));
			addWord("stack", std::mem_fn(&Machine::printStack));
			addWord(":", std::mem_fn(&Machine::defineWord));
			addWord("see", std::mem_fn<void()>(&Machine::seeWord));
			addWord("if", std::mem_fn(&Machine::ifCondition), true);
			addWord("else", std::mem_fn(&Machine::elseCondition), true);
			addWord("then", std::mem_fn(&Machine::thenStatement), true);
			addWord("begin", std::mem_fn(&Machine::beginStatement), true);
			addWord("end", std::mem_fn(&Machine::endStatement), true);
			addWord("uc", std::mem_fn<void()>(&Machine::dispatchInstruction));
			addWord("do", std::mem_fn(&Machine::doStatement), true);
			addWord("continue", std::mem_fn(&Machine::continueStatement), true);
			addWord("choose.c", std::mem_fn(&Machine::chooseRegister));
			addWord("invoke.c", std::mem_fn(&Machine::invokeCRegister));
            addWord("'", std::mem_fn(&Machine::injectWord));
            addWord("execute", std::mem_fn(&Machine::executeTop));
            addWord("raiseError", std::mem_fn(&Machine::raiseError));
			_microcodeInvoke = lookupWord("uc");
		}
	}
	void Machine::doStatement() {
		if (!inCompilationMode()) {
			throw Problem("do", "Not compiling!");
		}
		pushSubroutine(_compileTarget);
		_compileTarget = new DictionaryEntry("");
		_compileTarget->markFakeEntry();
	}
	void Machine::microcodeInvoke(const Molecule& m) {
        if (inCompilationMode()) {
            if (_compileTarget) {
                compileMicrocodeInvoke(m, _compileTarget);
            } else {
                throw Problem("microcodeInvoke", "No compile target yet compiling!");
            }
        } else {
            // otherwise do the actual action itself 
			dispatchInstruction(m);
        }
	}
	void Machine::continueStatement() {
		if (!inCompilationMode()) {
			throw Problem("continue", "not compiling!");
		} 
		if (subroutineStackEmpty()) {
			throw Problem("continue", "subroutine stack is empty!");
		}
		auto parent = popSubroutine();
		addWord(_compileTarget);
		auto container = new DictionaryEntry("", [this, body = _compileTarget](Machine* m) {
				static constexpr auto performEqualityCheck = Instruction::encodeOperation(Instruction::popA(), Instruction::popB(), Instruction::equals());
				static constexpr auto saveABToStack = Instruction::encodeOperation(Instruction::pushB(), Instruction::pushA());
				static_assert(Address(0x111d1c) == performEqualityCheck, "Equality check operation failed!");
				static_assert(Address(0x2122) == saveABToStack, "Save AB to stack routine failed!");
				microcodeInvoke(performEqualityCheck);
				if (_core.getRegister(TargetRegister::C).getTruth()) {
					microcodeInvoke(saveABToStack); // put the values back on the stack
                    // super gross but far more accurately models the underlying micro architecture
loopTop:
					body->operator()(m);
					// compacted operation:
					// popa
					// popb
					// eq
					microcodeInvoke(performEqualityCheck);
                    if (!_core.getRegister(TargetRegister::C).getTruth()) {
						microcodeInvoke(saveABToStack); // put the values back on the stack
                        goto loopTop;
                    }
				}

		});
		container->markFakeEntry();
		addWord(container);
		_compileTarget = parent.subroutine;
		_compileTarget->addSpaceEntry(container);

	}
	void Machine::beginStatement() {
		if (!inCompilationMode()) {
			throw Problem("begin", "Must be compiling!");
		}
		pushSubroutine(_compileTarget);
		_compileTarget = new DictionaryEntry("");
		_compileTarget->markFakeEntry();
	}
	void Machine::endStatement() {
		if (!inCompilationMode()) {
			throw Problem("end", "Must be compiling!");
		}
		if (subroutineStackEmpty()) {
			throw Problem("end", "subroutine stack is empty!");
		}

		auto parent = popSubroutine();
		addWord(_compileTarget);
		auto container = new DictionaryEntry("", [this, body = _compileTarget](Machine* m) {
				static constexpr auto checkCondition = Instruction::encodeOperation( Instruction::popA(), Instruction::notOp());
				static_assert(0x061c == checkCondition, "conditional operation failed!");
                // super gross but far more accurately models the underlying micro architecture
endLoopTop:
				body->operator()(m);
				// pop.a
				// not
				microcodeInvoke(checkCondition);
                if (!_core.getRegister(TargetRegister::C).getTruth()) {
                    goto endLoopTop;
                }
				});
		container->markFakeEntry();
		addWord(container);
		_compileTarget = parent.subroutine;
		_compileTarget->addSpaceEntry(container);
		// now we have to construct a single entry for the parent which has the conditional code added as well
	}
	void Machine::printStack() {
		for (const auto& element : _parameter) {
			_output << "\t- " << element << std::endl;
		}
	}
	void Machine::dispatchInstruction(const Molecule& molecule) {
		_core.dispatchInstruction(molecule);
	}
	void Machine::popRegister(TargetRegister t) {
		using Type = decltype(t);
		auto top(popParameter());
		Register& target = _core.getRegister(t);
		if (involvesDiscriminantRegister(t)) {
			target.setType(static_cast<Discriminant>(top.address));
		} else {
			target.setValue(top);
		}
	}
	void Machine::pushRegister(TargetRegister t) {
		using Type = decltype(t);
		Register& target = _core.getRegister(t);
		if (involvesDiscriminantRegister(t)) {
			pushParameter(static_cast<Address>(target.getType()));
		} else {
			pushParameter(target.getValue());
		}
	}
	/*
	void Machine::load(const Molecule& m) {
		try {
			// figure out which register to get the address from!
			auto args = m.getByte(_registerIP.getAddress());
			auto dest = static_cast<TargetRegister>(getDestinationRegister(args));
			auto src = static_cast<TargetRegister>(getSourceRegister(args));
			if (involvesDiscriminantRegister(src)) {
				throw Problem("", "Can't use the discriminant field of a register as an address!");
			} else if (!legalValue(src)) {
				throw Problem("", "Illegal undefined register!");
			}
			Register& d = getRegister(dest);
			Register& s = getRegister(src);
			if (involvesDiscriminantRegister(dest)) {
				d.setType(static_cast<Discriminant>(load(s.getAddress()).address));
			} else {
				d.setValue(load(s.getAddress()));
			}
			_registerIP.increment();
		} catch (Problem& p) {
			throw Problem("load", p.getMessage());
		}
	}

	void Machine::store(const Molecule& m) {
		try {
			auto tb = m.getByte(_registerIP.getAddress());
			// figure out which register to get the address from!
			auto dest = static_cast<TargetRegister>(getDestinationRegister(tb));
			if (!legalValue(dest)) {
				throw Problem("", "Illegal undefined destination register!");
			} else if (involvesDiscriminantRegister(dest)) {
				throw Problem("", "Can't use the discriminant field of a register as an address!");
			}
			auto src = static_cast<TargetRegister>(getSourceRegister(tb));
			if (!legalValue(dest)) {
				throw Problem("", "Illegal undefined source register!");
			}
			store(getRegister(dest).getAddress(), getRegister(src).getInt());
			_registerIP.increment();
		} catch (Problem& p) {
			throw Problem("store", p.getMessage());
		}
	}
	void Machine::moveOrSwap(TargetRegister from, TargetRegister to, bool swap) {
		if (from == to) {
			// do nothing :)
			return;
		}
		Register& src = getRegister(from);
		Register& dest = getRegister(to);
		if (swap) {
			if (involvesDiscriminantRegister(to)) {
				auto v0 = dest.getType();
				if (involvesDiscriminantRegister(from)) {
					dest.setType(src.getType());
					src.setType(v0);
				} else {
					dest.setType(static_cast<Discriminant>(src.getAddress()));
					src.setValue(static_cast<Address>(v0));
				}
			} else {
				Datum v0(dest.getValue());
				if (involvesDiscriminantRegister(from)) {
					dest.setValue(static_cast<Address>(src.getType()));
					src.setType(static_cast<Discriminant>(v0.address));
				} else {
					dest.setValue(src.getValue());
					src.setValue(v0);
				}
			}
		} else {
			if (involvesDiscriminantRegister(to)) {
				dest.setType(involvesDiscriminantRegister(from) ? src.getType() : static_cast<Discriminant>(src.getAddress()));
			} else {
				dest.setValue(involvesDiscriminantRegister(from) ? static_cast<Address>(src.getType()) : src.getValue());
			}
		}
	}

	void Machine::swapRegisters(const Molecule& m) {
		auto args = m.getByte(_registerIP.getAddress());
		auto dest = getDestinationRegister(args);
		auto src = getSourceRegister(args);
		moveOrSwap(static_cast<TargetRegister>(src), static_cast<TargetRegister>(dest), true);
		_registerIP.increment();
	}

	void Machine::moveRegister(const Molecule& m) {
		auto args = m.getByte(_registerIP.getAddress());
		auto dest = getDestinationRegister(args);
		auto src = getSourceRegister(args);
		moveOrSwap(static_cast<TargetRegister>(src), static_cast<TargetRegister>(dest), false);
		_registerIP.increment();
	}
	*/
    void Machine::injectWord() {
        // read the next word and then lookup that entry
		auto word = readWord();
        auto entry = lookupWord(word);
        if (entry) {
            // since this exists, put it onto the stack!
            pushParameter(entry);
        } else {
            std::stringstream name;
            name << word << "?";
            auto str = name.str();
            throw Problem("'", str);
        }
    }
    void Machine::executeTop() {
        auto top = popParameter();
        top.entry->operator()(this);
    }
	bool Machine::keepExecuting() noexcept {
		static constexpr auto loadValueIntoX = Instruction::encodeOperation(
				Instruction::load(TargetRegister::X, TargetRegister::X));
        static constexpr auto loadLower = Instruction::loadAddressLowerHalf(TargetRegister::X, shouldKeepExecutingLocation);
        static constexpr auto loadUpper = Instruction::loadAddressUpperHalf(TargetRegister::X, shouldKeepExecutingLocation);
        dispatchInstructionStream<loadLower, loadUpper, loadValueIntoX>();
		return _core.getRegister(TargetRegister::X).getTruth();
	}

	void Machine::handleError(const std::string& word, const std::string& msg) noexcept {
		// clear the stacks and the input pointer
		_parameter.clear();
		clearSubroutineStack();
		_input.clear();
		_input.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		_output << word << " " << msg << std::endl;
		if (_compileTarget != nullptr) {
			delete _compileTarget;
			_compileTarget = nullptr;
		}
		// _compiling = false;
		dispatchInstructionStream<
			Instruction::loadAddressLowerHalf(TargetRegister::X, isCompilingLocation),
			Instruction::loadAddressUpperHalf(TargetRegister::X, isCompilingLocation),
			storeFalse>();
	}
	void Machine::dispatchInstruction() {
		// just pop a molecule off of the stack and pass it to
		// dispatchInstruction
		dispatchInstruction(popParameter().address);
	}
	bool Machine::inCompilationMode() noexcept {
		dispatchInstructionStream<
			Instruction::loadAddressLowerHalf(TargetRegister::X, isCompilingLocation),
			Instruction::loadAddressUpperHalf(TargetRegister::X, isCompilingLocation),
			Instruction::encodeOperation(Instruction::load(TargetRegister::X, TargetRegister::X))>();
		return _core.getRegister(TargetRegister::X).getTruth();
	}
	void Machine::activateCompileMode() {
		dispatchInstructionStream<
			Instruction::loadAddressLowerHalf(TargetRegister::X, isCompilingLocation),
			Instruction::loadAddressUpperHalf(TargetRegister::X, isCompilingLocation),
			Instruction::encodeOperation(
					Instruction::move(TargetRegister::B, TargetRegister::A),
					Instruction::xorOp(),
					Instruction::setImmediate16_Lowest(TargetRegister::C, 1)),
			Instruction::encodeOperation(
					Instruction::store(TargetRegister::X, TargetRegister::C))>();
	}
	void Machine::deactivateCompileMode() {
		dispatchInstructionStream<
			Instruction::loadAddressLowerHalf(TargetRegister::X, isCompilingLocation),
			Instruction::loadAddressUpperHalf(TargetRegister::X, isCompilingLocation),
			storeFalse>();
	}

	bool Machine::stackEmpty(TargetRegister sp, Address location) {
		dispatchInstruction(Instruction::loadAddressLowerHalf(TargetRegister::X, location));
		dispatchInstruction(Instruction::loadAddressUpperHalf(TargetRegister::X, location));
		dispatchInstruction(Instruction::encodeOperation(
					Instruction::load(TargetRegister::S, TargetRegister::X),
					Instruction::equals(TargetRegister::C, TargetRegister::S, sp)));
		return _core.getRegister(TargetRegister::C).getTruth();
	}
	bool Machine::stackFull(TargetRegister sp, Address location) {
		dispatchInstruction(Instruction::loadAddressLowerHalf(TargetRegister::X, location));
		dispatchInstruction(Instruction::loadAddressUpperHalf(TargetRegister::X, location));
		dispatchInstruction(Instruction::encodeOperation(
					Instruction::load(TargetRegister::S, TargetRegister::X),
					Instruction::equals(TargetRegister::C, TargetRegister::S, sp)));
		return _core.getRegister(TargetRegister::C).getTruth();
	}
	void Machine::pushOntoStack(TargetRegister sp, Datum value, Address fullLocation) {
		if (stackFull(sp, fullLocation)) {
			throw Problem("pushOntoStack", "STACK FULL!!!");
		}
		dispatchInstruction(Instruction::loadAddressLowerHalf(TargetRegister::X, value.address));
		dispatchInstruction(Instruction::loadAddressUpperHalf(TargetRegister::X, value.address));
		dispatchInstruction(Instruction::encodeOperation(
					Instruction::sub(sp, sp, 1),
					Instruction::store(sp, TargetRegister::X)));
	}
	void Machine::pushSubroutine(Datum value) {
		try {
			pushOntoStack(TargetRegister::SP2, value, subroutineStackFullLocation);
		} catch (Problem& p) {
			throw Problem("pushSubroutine", p.getMessage());
		}
	}
	Datum Machine::popOffStack(TargetRegister sp, Address emptyLocation) {
		if (stackEmpty(sp, emptyLocation)) {
			throw Problem("pop", "STACK EMPTY!");
		}
		dispatchInstruction(Instruction::encodeOperation(
					Instruction::load(TargetRegister::C, sp),
					Instruction::add(sp, sp, 1)));
		return _core.getRegister(TargetRegister::C).getValue();
	}
	Datum Machine::popSubroutine() {
		try {
			return popOffStack(TargetRegister::SP2, subroutineStackEmptyLocation);
		} catch (Problem& p) {
			throw Problem ("popSubroutine", p.getMessage());
		}
	}
	bool Machine::subroutineStackEmpty() {
		return stackEmpty(TargetRegister::SP2, subroutineStackEmptyLocation);
	}
	bool Machine::subroutineStackFull() {
		return stackFull(TargetRegister::SP2, subroutineStackFullLocation);
	}
	void Machine::clearSubroutineStack() {
		dispatchInstruction(Instruction::loadAddressLowerHalf(TargetRegister::X, subroutineStackEmptyLocation));
		dispatchInstruction(Instruction::loadAddressUpperHalf(TargetRegister::X, subroutineStackEmptyLocation));
		dispatchInstruction(Instruction::encodeOperation(Instruction::load(TargetRegister::SP2, TargetRegister::X)));
	}
	void Machine::setA(const Datum& target) noexcept { _core.getRegister(TargetRegister::A).setValue(target); }
	void Machine::setB(const Datum& target) noexcept { _core.getRegister(TargetRegister::B).setValue(target); }
	void Machine::setTA(Discriminant target) noexcept { _core.getRegister(TargetRegister::TA).setType(target); }
	void Machine::setTB(Discriminant target) noexcept { _core.getRegister(TargetRegister::TB).setType(target); }

	Datum Machine::load(Address addr) {
		return _core.load(addr);
	}
	void Machine::store(Address addr, const Datum& value) {
		_core.store(addr, value);
	}
	void Machine::load() {
		_core.getRegister(TargetRegister::C).setValue(load(_core.getRegister(TargetRegister::A).getAddress()));
	}
	void Machine::store() {
		store(_core.getRegister(TargetRegister::A).getAddress(), _core.getRegister(TargetRegister::B).getValue());
	}
	void Machine::typeValue(const Datum& value) {
		typeValue(_core.getRegister(TargetRegister::C).getType(), value);
	}
	void Machine::typeValue() {
		typeValue(_core.getRegister(TargetRegister::A).getValue());
	}
} // end namespace forth

