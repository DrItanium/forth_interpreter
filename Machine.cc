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
				auto innerTarget = space->_entry;
				if (innerTarget->isFake()) {
					seeWord(innerTarget);
				} else {
					_output << innerTarget->getName();
				}
			};
			auto outputWord = [this, entry](auto space) {
				auto innerTarget = space->_entry;
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
						_output << std::dec << x->_int;
						break;
					case Type::Unsigned:
						_output << std::hex << "0x" << x->_addr;
						break;
					case Type::FloatingPoint:
						_output << std::dec << x->_fp;
						break;
					case Type::Boolean:
						_output << std::boolalpha << x->_truth;
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
		_registerC = _registerC.getTruth() ? _registerA : _registerB;
	}
	void Machine::invokeCRegister() {
		using Type = decltype(_registerC.getType());
		switch (_registerC.getType()) {
			case Type::Word:
				_registerC.getWord()->operator()(this);
				break;
			case Type::Number:
			case Type::FloatingPoint:
				pushRegister(TargetRegister::RegisterC);
				break;
			default:
				throw Problem("invoke.c", "incorrect discriminant!");
		}
	}
	void Machine::ifCondition() {
		static constexpr auto prepRegisters = Instruction::encodeOperation(
				Instruction::popRegister(TargetRegister::RegisterTB),
				Instruction::popB(), 
				Instruction::popRegister(TargetRegister::RegisterTA),
				Instruction::popA(),
				Instruction::popC());
		// if we're not in compilation mode then error out
		if (!_compiling) {
			throw Problem("if", "must be defining a word!");
		}
		auto currentTarget = _compileTarget;
		_subroutine.push_back(_compileTarget);
		_compileTarget = new DictionaryEntry("");
		_compileTarget->markFakeEntry();
		auto* elseBlock = new DictionaryEntry("");
		elseBlock->markFakeEntry();
		_subroutine.push_back(elseBlock);

		currentTarget->pushWord(_compileTarget);
		currentTarget->addSpaceEntry(static_cast<Address>(Discriminant::Word));
		currentTarget->pushWord(elseBlock);
		currentTarget->addSpaceEntry(static_cast<Address>(Discriminant::Word));
		compileMicrocodeInvoke(prepRegisters, currentTarget);
	}
	void Machine::compileMicrocodeInvoke(const Molecule& m, DictionaryEntry* current) {
		current->addSpaceEntry(m._value);
		current->addSpaceEntry(_popS);
		current->addSpaceEntry(_microcodeInvoke);
	}
	void Machine::elseCondition() {
		if (!_compiling) {
			throw Problem("else", "must be defining a word!");
		}
		// pop the else block off of the subroutine stack
		auto* elseBlock = _subroutine.back();
		_subroutine.pop_back();
		_subroutine.emplace_back(new DictionaryEntry(""));
		// let the if block dangle off since it is referenced else where
		addWord(_compileTarget);
		_compileTarget = elseBlock;
	}

	void Machine::thenStatement() {
		if (!_compiling) {
			throw Problem("then", "must be defining a word!");
		}
		if (_subroutine.empty()) {
			throw Problem("then", "Not in a function");
		}
		// there will always be a garbage entry at the top of the subroutine stack, just eliminate it
		_subroutine.pop_back();
		auto parent = _subroutine.back();
		_subroutine.pop_back();
		addWord(_compileTarget);
		_compileTarget = parent;
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
		fn("A", _registerA.getValue());
		fn("B", _registerB.getValue());
		fn("C", _registerC.getValue());
		fn("T", _registerC.getType());
		fn("S", _registerS.getValue());
		fn("X", _registerX.getValue());
		fn("A.T", _registerA.getType());
		fn("B.T", _registerB.getType());
		fn("X.T", _registerX.getType());
		_output.setf(flags); // restore after done
	}
	void Machine::defineWord() {
		if (_compiling || _compileTarget != nullptr) {
			throw Problem(":", "already compiling");
		}
		activateCompileMode();
		// pass address "execute" to the entry subroutine
		_compileTarget = new DictionaryEntry(readWord());
	}
	void Machine::endDefineWord() {
		if (!_compiling || _compileTarget == nullptr) {
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
	void Machine::terminateExecution() {
		_keepExecuting = false;
	}
	void Machine::notOperation() {
		// invert register a
		auto fn = [this](auto a) {
			_registerC.setValue(~a);
		};
		using Type = decltype(_registerC.getType());
		switch(_registerC.getType()) {
			case Type::Number:
				fn(_registerA.getInt());
				break;
			case Type::MemoryAddress:
				fn(_registerA.getAddress());
				break;
			case Type::Boolean:
				_registerC.setValue(!_registerA.getTruth());
				break;
			default:
				throw Problem("not", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::minusOperation() {
		auto fn = [this](auto a) { _registerC.setValue(-a); };
		using Type = decltype(_registerC.getType());
		switch(_registerC.getType()) {
			case Type::Number:
				fn(_registerA.getInt());
				break;
			case Type::FloatingPoint:
				fn(_registerA.getFP());
				break;
			default:
				throw Problem("minus", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::multiplyOperation() {
		using Type = decltype(_registerC.getType());
		auto fn = [this](auto a, auto b) { _registerC.setValue(a * b); };
		switch(_registerC.getType()) {
			case Type::Number:
				fn(_registerA.getInt(), _registerB.getInt());
				break;
			case Type::MemoryAddress:
				fn(_registerA.getAddress(), _registerB.getAddress());
				break;
			case Type::FloatingPoint:
				fn(_registerA.getFP(), _registerB.getFP());
				break;
			default:
				throw Problem("*", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::equals() {
		auto fn = [this](auto a, auto b) { _registerC.setValue(a == b); };
		using Type = decltype(_registerC.getType());
		switch(_registerC.getType()) {
			case Type::Number:
				fn(_registerA.getInt(), _registerB.getInt());
				break;
			case Type::MemoryAddress:
				fn(_registerA.getAddress(), _registerB.getAddress());
				break;
			case Type::FloatingPoint:
				fn(_registerA.getFP(), _registerB.getFP());
				break;
			case Type::Boolean:
				fn(_registerA.getTruth(), _registerB.getTruth());
				break;
			default:
				throw Problem("==", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::powOperation() {
		auto fn = [this](auto a, auto b) {
			_registerC.setValue(static_cast<decltype(a)>(std::pow(a, b)));
		};
		using Type = decltype(_registerC.getType());
		switch(_registerC.getType()) {
			case Type::Number:
				fn(_registerA.getInt(), _registerB.getInt());
				break;
			case Type::MemoryAddress:
				fn(_registerA.getAddress(), _registerB.getAddress());
				break;
			case Type::FloatingPoint:
				fn(_registerA.getFP(), _registerB.getFP());
				break;
			default:
				throw Problem("**", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::divide(bool remainder) {
		auto title = remainder ? "mod" : "/";
		auto fn = [this, remainder, &title](auto a, auto b) {
			if (b == 0) {
				throw Problem(title, "DIVIDE BY ZERO!");
			} else {
				_registerC.setValue(remainder ? a % b : a / b);
			}
		};
		using Type = decltype(_registerC.getType());
		if (remainder && (_registerC.getType() == Type::FloatingPoint)) {
			throw Problem("mod", "FLOATING POINT MODULO makes no sense!");
		}
		switch(_registerC.getType()) {
			case Type::Number:
				fn(_registerA.getInt(), _registerB.getInt());
				break;
			case Type::MemoryAddress:
				fn(_registerA.getAddress(), _registerB.getAddress());
				break;
			case Type::FloatingPoint:
				_registerC.setValue(_registerA.getFP() / _registerB.getFP());
				break;
			default:
				throw Problem(title, "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::numericCombine(bool subtract) {
		auto fn = [this, subtract](auto a, auto b) { 
			_registerC.setValue(subtract ? (a - b) : (a + b));
		};
		switch(_registerC.getType()) {
			case Discriminant::Number:
				fn(_registerA.getInt(), _registerB.getInt());
				break;
			case Discriminant::MemoryAddress:
				fn(_registerA.getAddress(), _registerB.getAddress());
				break;
			case Discriminant::FloatingPoint:
				fn(_registerA.getFP(), _registerB.getFP());
				break;
			default:
				throw Problem(subtract ? "-" : "+", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::andOperation() {
		using Type = decltype(_registerC.getType());
		switch (_registerC.getType()) {
			case Type::Number:
				_registerC.setValue(_registerA.getInt() & _registerB.getInt());
				break;
			case Type::MemoryAddress:
				_registerC.setValue(_registerA.getAddress() & _registerB.getAddress());
				break;
			case Type::Boolean:
				_registerC.setValue(_registerA.getTruth() && _registerB.getTruth());
				break;
			default:
				throw Problem("and", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::orOperation() {
		using Type = forth::Discriminant;
		switch (_registerC.getType()) {
			case Type::Number:
				_registerC.setValue(_registerA.getInt() | _registerB.getInt());
				break;
			case Type::MemoryAddress:
				_registerC.setValue(_registerA.getAddress() | _registerB.getAddress());
				break;
			case Type::Boolean:
				_registerC.setValue(_registerA.getTruth() || _registerB.getTruth());
				break;
			default:
				throw Problem("or", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::greaterThanOperation() {
		using Type = decltype(_registerC.getType());
		auto result = false;
		switch (_registerC.getType()) {
			case Type::Number:
				result = _registerA.getInt() > _registerB.getInt();
				break;
			case Type::MemoryAddress:
				result = _registerA.getAddress() > _registerB.getAddress();
				break;
			case Type::FloatingPoint:
				result = _registerA.getFP() > _registerB.getFP();
				break;
			default:
				throw Problem(">", "ILLEGAL DISCRIMINANT!");
		}
		_registerC.setValue(result);
	}

	void Machine::lessThanOperation() {
		using Type = decltype(_registerC.getType());
		switch (_registerC.getType()) {
			case Type::Number:
				_registerC.setValue(_registerA.getInt() < _registerB.getInt());
				break;
			case Type::MemoryAddress:
				_registerC.setValue(_registerA.getAddress() < _registerB.getAddress());
				break;
			case Type::FloatingPoint:
				_registerC.setValue(_registerA.getFP() < _registerB.getFP());
				break;
			default:
				throw Problem("<", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::xorOperation() {
		using Type = decltype(_registerC.getType());
		switch (_registerC.getType()) {
			case Type::Number:
				_registerC.setValue(_registerA.getInt() ^ _registerB.getInt());
				break;
			case Type::MemoryAddress:
				_registerC.setValue(_registerA.getAddress() ^ _registerB.getAddress());
				break;
			case Type::Boolean:
				_registerC.setValue(bool(_registerA.getTruth() ^ _registerB.getTruth()));
				break;
			default:
				throw Problem("xor", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::semicolonOperation() {
		// in assembly level impls, this resets the instruction
		// counter to zero, however, with how we use iterators,
		// this isn't necessary!
	}
	void Machine::shiftOperation(bool shiftLeft) {
		using Type = decltype(_registerC.getType());
		switch (_registerC.getType()) {
			case Type::Number:
				_registerC.setValue(shiftLeft ? (_registerA.getInt()  << _registerB.getInt()) : (_registerA.getInt() >> _registerB.getInt()));
				break;
			case Type::MemoryAddress:
				_registerC.setValue(shiftLeft ? (_registerA.getAddress()  << _registerB.getAddress()) : (_registerA.getAddress() >> _registerB.getAddress()));
				break;
			default:
				throw Problem(shiftLeft ? "<<" : ">>", "ILLEGAL DISCRIMINANT");
		}
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
	Machine::Machine(std::ostream& output, std::istream& input) :  _output(output), _input(input), _memory(new Datum[memoryCapacity]), _dynamic(new Datum[memoryCapacity]), _words(nullptr) { }

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

	Datum Machine::load(Address addr) {
		if (addr > largestAddress) {
			throw Problem("mload", "BAD ADDRESS!");
		} else {
			return Datum (_memory[addr]);
		}
	}
	void Machine::store(Address addr, const Datum& value) {
		if (addr > largestAddress) {
			throw Problem("mstore", "BAD ADDRESS!");
		} else {
			_memory[addr] = value.numValue;
		}
	}
	bool Machine::numberRoutine(const std::string& word) noexcept {
		if (word.empty()) { 
			return false; 
		}
		// floating point
		// integers
		// first do some inspection first
		if (word == "true") {
			if (_compiling) {
				_compileTarget->addSpaceEntry(true);
			} else {
				pushParameter(true);
			}
			return true;
		}
		if (word == "false") {
			if (_compiling) {
				_compileTarget->addSpaceEntry(false);
			} else {
				pushParameter(false);
			}
			return true;
		}
		std::istringstream parseAttempt(word);
		if (word.find('#') != std::string::npos) {
			Address tmpAddress;
			parseAttempt >> std::hex >> tmpAddress;
			if (!parseAttempt.fail()) {
				if (_compiling) {
					_compileTarget->addSpaceEntry(tmpAddress);
				}  else {
					pushParameter(tmpAddress);
				}
				return true;
			}
			return false;
		}
		if (word.find('u') != std::string::npos) {
			Address tmpAddress;
			parseAttempt >> tmpAddress;
			if (!parseAttempt.fail()) {
				if (_compiling) {
					_compileTarget->addSpaceEntry(tmpAddress);
				} else {
					pushParameter(tmpAddress);
				}
				return true;
			}
			return false;
		}
		parseAttempt.clear();
		if (word.find('.') != std::string::npos) {
			Floating tmpFloat;
			parseAttempt >> tmpFloat;
			if (!parseAttempt.fail() && parseAttempt.eof()) {
				if (_compiling) {
					_compileTarget->addSpaceEntry(tmpFloat);
				} else {
					pushParameter(tmpFloat);
				}
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
			if (_compiling) {
				_compileTarget->addSpaceEntry(tmpInt);
			} else {
				// if we hit the end of the word provided then it is an integer, otherwise it is not!
				pushParameter(tmpInt);
			}
			return true;
		}
		return false;
	}

	void Machine::handleError(const std::string& word, const std::string& msg) noexcept {
		// clear the stacks and the input pointer
		_parameter.clear();
		_subroutine.clear();
		_input.clear();
		_input.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		_output << word << " " << msg << std::endl;
		if (_compileTarget != nullptr) {
			delete _compileTarget;
			_compileTarget = nullptr;
		}
		_compiling = false;
	}
	void Machine::controlLoop() noexcept {
		// setup initial dictionary
		initializeBaseDictionary();
        bool ignoreInput = false;
		while (_keepExecuting) {
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
				if (_compiling) {
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
			addWord("quit", std::mem_fn(&Machine::terminateExecution));
			addWord(";", std::mem_fn(&Machine::semicolonOperation));
			addWord("registers", std::mem_fn(&Machine::printRegisters));
			addWord("words", std::mem_fn(&Machine::listWords));
			addWord("stack", std::mem_fn(&Machine::printStack));
			addWord(":", std::mem_fn(&Machine::defineWord));
			addWord("see", std::mem_fn<void()>(&Machine::seeWord));
			addWord("pop.s", std::mem_fn(&Machine::popSRegister));
			addWord("if", std::mem_fn(&Machine::ifCondition), true);
			addWord("else", std::mem_fn(&Machine::elseCondition), true);
			addWord("then", std::mem_fn(&Machine::thenStatement), true);
			addWord("begin", std::mem_fn(&Machine::beginStatement), true);
			addWord("end", std::mem_fn(&Machine::endStatement), true);
			addWord("uc", std::mem_fn(&Machine::dispatchInstruction));
			addWord("do", std::mem_fn(&Machine::doStatement), true);
			addWord("continue", std::mem_fn(&Machine::continueStatement), true);
			addWord("choose.c", std::mem_fn(&Machine::chooseRegister));
			addWord("invoke.c", std::mem_fn(&Machine::invokeCRegister));
            addWord("'", std::mem_fn(&Machine::injectWord));
            addWord("execute", std::mem_fn(&Machine::executeTop));
            addWord("raiseError", std::mem_fn(&Machine::raiseError));
			_microcodeInvoke = lookupWord("uc");
			_popS = lookupWord("pop.s");
		}
	}
	void Machine::doStatement() {
		if (!_compiling) {
			throw Problem("do", "Not compiling!");
		}
		_subroutine.push_back(_compileTarget);
		_compileTarget = new DictionaryEntry("");
		_compileTarget->markFakeEntry();
	}
	void Machine::microcodeInvoke(const Molecule& m) {
        if (_compiling) {
            if (_compileTarget) {
                compileMicrocodeInvoke(m, _compileTarget);
            } else {
                throw Problem("microcodeInvoke", "No compile target yet compiling!");
            }
        } else {
            // otherwise do the actual action itself 
		    _registerS.setValue(m._value);
		    dispatchInstruction();
        }
	}
	void Machine::continueStatement() {
		if (!_compiling) {
			throw Problem("continue", "not compiling!");
		} 
		if (_subroutine.empty()) {
			throw Problem("continue", "subroutine stack is empty!");
		}
		auto parent = _subroutine.back();
		_subroutine.pop_back();
		addWord(_compileTarget);
		auto container = new DictionaryEntry("", [this, body = _compileTarget](Machine* m) {
				static constexpr auto performEqualityCheck = Instruction::encodeOperation(Instruction::popA(), Instruction::popB(), Instruction::equals());
				static constexpr auto saveABToStack = Instruction::encodeOperation(Instruction::pushB(), Instruction::pushA());
				static_assert(Address(0x111d1c) == performEqualityCheck, "Equality check operation failed!");
				static_assert(Address(0x00100110) == saveABToStack, "Save AB to stack routine failed!");
				microcodeInvoke(performEqualityCheck);
				if (_registerC.getTruth()) {
					microcodeInvoke(saveABToStack); // put the values back on the stack
                    // super gross but far more accurately models the underlying micro architecture
loopTop:
					body->operator()(m);
					// compacted operation:
					// popa
					// popb
					// eq
					microcodeInvoke(performEqualityCheck);
                    if (!_registerC.getTruth()) {
						microcodeInvoke(saveABToStack); // put the values back on the stack
                        goto loopTop;
                    }
				}

		});
		container->markFakeEntry();
		addWord(container);
		_compileTarget = parent;
		_compileTarget->addSpaceEntry(container);

	}
	void Machine::beginStatement() {
		if (!_compiling) {
			throw Problem("begin", "Must be compiling!");
		}
		_subroutine.push_back(_compileTarget);
		_compileTarget = new DictionaryEntry("");
		_compileTarget->markFakeEntry();
	}
	void Machine::endStatement() {
		if (!_compiling) {
			throw Problem("end", "Must be compiling!");
		}
		if (_subroutine.empty()) {
			throw Problem("end", "subroutine stack is empty!");
		}

		auto parent = _subroutine.back();
		_subroutine.pop_back();
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
                if (!_registerC.getTruth()) {
                    goto endLoopTop;
                }
				});
		container->markFakeEntry();
		addWord(container);
		_compileTarget = parent;
		_compileTarget->addSpaceEntry(container);
		// now we have to construct a single entry for the parent which has the conditional code added as well
	}
	void Machine::printStack() {
		for (const auto& element : _parameter) {
			_output << "\t- " << element << std::endl;
		}
	}
	void Machine::dispatchInstruction() {
		_registerIP.reset();
		// use the s register as the current instruction
		// slice out the lowest eight bits
		auto molecule = _registerS.getMolecule();
		while (_registerIP.getAddress() < sizeof(Molecule)) {
			auto pos = _registerIP.getAddress();
			auto op = getOperation(molecule.getByte(pos));
			_registerIP.increment();

			switch (op) {
				case Operation::Stop: return; // stop executing code within this molecule
				case Operation::Add: numericCombine(); break; // add or subtract
				case Operation::Subtract: numericCombine(true); break; // add or subtract
				case Operation::ShiftRight: shiftOperation(); break; // shift right 
				case Operation::ShiftLeft: shiftOperation(true); break; // shift left
				case Operation::Multiply: multiplyOperation(); break;
				case Operation::Equals: equals(); break;
				case Operation::Pow: powOperation(); break;
				case Operation::Divide: divide(); break;
				case Operation::Modulo: divide(true); break;
				case Operation::Not: notOperation(); break;
				case Operation::Minus: minusOperation(); break;
				case Operation::And: andOperation(); break;
				case Operation::Or: orOperation(); break;
				case Operation::GreaterThan: greaterThanOperation(); break; // greater than
				case Operation::LessThan: lessThanOperation(); break;
				case Operation::Xor: xorOperation(); break;
				case Operation::TypeValue: typeValue(); break;
				case Operation::PopRegister: popRegister(molecule); break;
				case Operation::PushRegister: pushRegister(molecule); break;
				case Operation::Load: load(molecule); break;
				case Operation::Store: store(molecule); break;
				case Operation::SetImmediate16_Lowest: setImmediate16<Immediate16Positions::Lowest>(molecule); break;
				case Operation::SetImmediate16_Lower: setImmediate16<Immediate16Positions::Lower>(molecule); break;
				case Operation::SetImmediate16_Higher: setImmediate16<Immediate16Positions::Higher>(molecule); break;
				case Operation::SetImmediate16_Highest: setImmediate16<Immediate16Positions::Highest>(molecule); break;
				case Operation::Move: moveRegister(molecule); break;
				case Operation::Swap: swapRegisters(molecule); break;
				case Operation::PopA: popRegister(TargetRegister::RegisterA); break;
				case Operation::PopB: popRegister(TargetRegister::RegisterB); break;
				case Operation::PopT: popRegister(TargetRegister::RegisterT); break;
				case Operation::PushC: pushRegister(TargetRegister::RegisterC); break;
				default: throw Problem("uc", "Unknown instruction address!");
			}
		}
	}
	void Machine::popRegister(TargetRegister t) {
		using Type = decltype(t);
		auto top(popParameter());
		Register& target = getRegister(t);
		if (involvesDiscriminantRegister(t)) {
			target.setType(static_cast<Discriminant>(top.address));
		} else {
			target.setValue(top);
		}
	}
	void Machine::pushRegister(TargetRegister t) {
		using Type = decltype(t);
		Register& target = getRegister(t);
		if (involvesDiscriminantRegister(t)) {
			pushParameter(static_cast<Address>(target.getType()));
		} else {
			pushParameter(target.getValue());
		}
	}
	Register::Register(const Register& r) : _type(r._type), _value(r._value) { }
	Register::Register() : _type(static_cast<Discriminant>(0)), _value(Address(0)) { }
	void Machine::popRegister(const Molecule& m) {
		try {
			// read the current field
			// get the destination register to use as a target
			popRegister(static_cast<TargetRegister>(getDestinationRegister(m.getByte(_registerIP.getAddress()))));
			_registerIP.increment();
		} catch (Problem& p) {
			throw Problem("pop.register", p.getMessage());
		}
	}
	void Machine::pushRegister(const Molecule& m) {
		try {
			// read the current field
			// get the destination register to use as a target
			pushRegister(static_cast<TargetRegister>(getDestinationRegister(m.getByte(_registerIP.getAddress()))));
			_registerIP.increment();
		} catch (Problem& p) {
			throw Problem("push.register", p.getMessage());
		}
	}
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
	Register& Machine::getRegister(TargetRegister t) {
		using Type = decltype(t);
		switch (t) {
			case Type::RegisterA:
			case Type::RegisterTA:
				return _registerA;
			case Type::RegisterB:
			case Type::RegisterTB:
				return _registerB;
			case Type::RegisterC:
			case Type::RegisterT:
				return _registerC;
			case Type::RegisterS:
				return _registerS;
			case Type::RegisterX:
			case Type::RegisterTX:
				return _registerX;
			case Type::RegisterIP:
				return _registerIP;
			case Type::RegisterSP:
				return _registerSP;
			case Type::RegisterSP2:
				return _registerSP2;
			default:
				throw Problem("getRegister", "Undefined register!");
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
} // end namespace forth

