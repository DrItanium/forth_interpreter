// Currently on chapter 4.4.4
// TODO: support nested if statements
#include <iostream>
#include <string>
#include <list>
#include <memory>
#include <limits>
#include <sstream>
#include <cmath>
#include <set>
#include "Types.h"
#include "Problem.h"
#include "Datum.h"
#include "DictionaryEntry.h"
#include "Machine.h"

namespace forth {
	Problem::Problem(const std::string& word, const std::string& message) : _word(word), _message(message) { }


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
					case Type::LoadWordIntoA:
						_output << "native:lw.a " << x->_entry->getName();
						break;
					case Type::LoadWordIntoB:
						_output << "native:lw.b " << x->_entry->getName();
						break;
					case Type::ChooseRegisterAndStoreInC:
						_output << "native:choose-and-store-into-c";
						break;
					case Type::InvokeRegisterC:
						_output << "native:invoke.c";
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
		// if we're not in compilation mode then error out
		if (!_compiling) {
			throw Problem("if", "must be defining a word!");
		}
        auto currentTarget = _compileTarget;
		_subroutine.push_back(_compileTarget);
		_compileTarget = new DictionaryEntry("");
		_compileTarget->markFakeEntry();
        currentTarget->addSpaceEntry(_popC);
        currentTarget->addLoadWordEntryIntoA(_compileTarget);
        currentTarget->addLoadWordEntryIntoB(_nop);
        // use the normal compilation process onto the _compileTarget we built
	}
	void Machine::elseCondition() {
		if (!_compiling) {
			throw Problem("else", "must be defining a word!");
		}
        auto* elseBlock = new DictionaryEntry("");
        elseBlock->markFakeEntry();
        _subroutine.back()->addLoadWordEntryIntoB(elseBlock);
        // let the if block dangle off since it is referenced else where
        _compileTarget = elseBlock;
	}

	void Machine::thenStatement() {
		if (!_compiling) {
			throw Problem("then", "must be defining a word!");
		}
		if (_subroutine.empty()) {
			throw Problem("then", "Not in a function");
		}
        auto parent = _subroutine.back();
        _subroutine.pop_back();
        _compileTarget = parent;
		_compileTarget->addChooseOperation();
		_compileTarget->addInvokeCOperation();
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
        _output << "words: " << std::endl;
        if (_words == nullptr) {
            return;
        }
		std::set<std::string> entries;
		for (const auto* entry = _words; entry != nullptr; entry = entry->getNext()) {
            if (!entries.count(entry->getName())) {
                _output << "\t - " << entry->getName() << std::endl;
                entries.insert(entry->getName());
            }
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

	void Machine::divide() {
        auto fn = [this](auto a, auto b) {
            if (b == 0) {
                throw Problem("/", "DIVIDE BY ZERO!");
            } else {
                _registerC.setValue(a / b);
            }
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
                _registerC.setValue(_registerA.getFP() / _registerB.getFP());
				break;
			default:
				throw Problem("/", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::modulo() {
        auto fn = [this](auto a, auto b) {
            _registerC.setValue(a % b);
        };
		using Type = decltype(_registerC.getType());
		switch(_registerC.getType()) {
			case Type::Number:
                fn(_registerA.getInt(), _registerB.getInt());
				break;
			case Type::MemoryAddress:
                fn(_registerA.getAddress(), _registerB.getAddress());
				break;
			default:
				throw Problem("mod", "ILLEGAL DISCRIMINANT!");
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
	Machine::Machine(std::ostream& output, std::istream& input) :  _output(output), _input(input), _memory(new Integer[memoryCapacity]), _words(nullptr) { }

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
		while (_keepExecuting) {
			try {
				auto result = readWord();
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
			addWord("cache-basic-entries", std::mem_fn(&Machine::cacheBasicEntries));
            addWord("do", std::mem_fn(&Machine::doStatement), true);
            addWord("continue", std::mem_fn(&Machine::continueStatement), true);
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
                do {
                    body->operator()(m);
                    // compacted operation:
                    // popa
                    // popb
                    // eq
                    _registerS.setValue((Address)0x111d1c); // encode our operation set into the body
                    dispatchInstruction();
                    if (_registerC.getTruth()) {
                        break;
                    } else {
                        _registerS.setValue((Address)0x00100110); // put the values back on the stack
                        dispatchInstruction();
                    }
                } while (true);

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
                    Datum condition;
                    do {
                        body->operator()(m);
                        // pop.a
                        // not
                        _registerS.setValue((Address)0x061c); // 
                        if (_registerC.getTruth()) {
                            break;
                        }
                    } while (true);
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
                case Operation::Modulo: modulo(); break;
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
                case Operation::SetImmediate16_Lowest: setImmediate16Lowest(molecule); break;
                case Operation::SetImmediate16_Lower: setImmediate16Lower(molecule); break;
                case Operation::SetImmediate16_Higher: setImmediate16Higher(molecule); break;
                case Operation::SetImmediate16_Highest: setImmediate16Highest(molecule); break;
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
	void Machine::cacheBasicEntries() {
		if (!_cachedBasicEntries) {
			_cachedBasicEntries = true;
			_popT = lookupWord("pop.t");
			_popA = lookupWord("pop.a");
			_popB = lookupWord("pop.b");
			_popC = lookupWord("pop.c");
			_pushT = lookupWord("push.t");
			_pushA = lookupWord("push.a");
			_pushB = lookupWord("push.b");
			_pushC = lookupWord("push.c");
			_popTA = lookupWord("pop.ta");
			_popTB = lookupWord("pop.tb");
			_popTX = lookupWord("pop.tx");
			_popX = lookupWord("pop.x");
			_popS = lookupWord("pop.s");
			_pushS = lookupWord("push.s");
			_pushX = lookupWord("push.x");
			_nop = lookupWord("nop");
		}
	}
	void Machine::popRegister(Machine::TargetRegister t) {
		using Type = decltype(t);
		auto top(popParameter());
		if (involvesDiscriminantRegister(t) && !forth::legalValue(static_cast<Discriminant>(top.address))) {
			throw Problem("pop.register", "ILLEGAL DISCRIMINANT!");
		}
        auto fn = [t, this, &top](Register& current) {
            if (involvesDiscriminantRegister(t)) {
                current.setType((Discriminant)top.address);
            } else {
                current.setValue(top);
            }
        };
		switch (t) {
			case Type::RegisterA:
            case Type::RegisterTA:
                fn(_registerA);
                break;
			case Type::RegisterB:
			case Type::RegisterTB:
                fn(_registerB);
				break;
			case Type::RegisterC:
			case Type::RegisterT:
                fn(_registerC);
				break;
			case Type::RegisterS:
                fn(_registerS);
				break;
			case Type::RegisterX:
			case Type::RegisterTX:
                fn(_registerX);
				break;
            case Type::RegisterIP:
            case Type::RegisterTIP:
                fn(_registerIP);
                break;
			default:
				throw Problem("pop.register", "Unknown register!");
		}
	}
	void Machine::pushRegister(Machine::TargetRegister t) {
		using Type = decltype(t);
		Datum tmp;
		switch (t) {
			case Type::RegisterA:
                pushParameter(_registerA.getValue());
				break;
			case Type::RegisterB:
                pushParameter(_registerB.getValue());
				break;
			case Type::RegisterC:
                pushParameter(_registerC.getValue());
				break;
			case Type::RegisterT:
				pushParameter(static_cast<Address>(_registerC.getType()));
				break;
			case Type::RegisterTA:
				pushParameter(static_cast<Address>(_registerA.getType()));
				break;
			case Type::RegisterTB:
				pushParameter(static_cast<Address>(_registerB.getType()));
				break;
			case Type::RegisterTX:
				pushParameter(static_cast<Address>(_registerX.getType()));
				break;
            case Type::RegisterTIP:
                pushParameter(static_cast<Address>(_registerIP.getType()));
                break;
			case Type::RegisterS:
                pushParameter(_registerS.getValue());
				break;
			case Type::RegisterX:
                pushParameter(_registerX.getValue());
				break;
            case Type::RegisterIP:
                pushParameter(_registerIP.getValue());
                break;
			default:
				throw Problem("push.register", "Unknown register!");
		}
	}
    Register::Register(const Register& r) : _type(r._type), _value(r._value) { }
    Register::Register() : _type(static_cast<Discriminant>(0)), _value(Address(0)) { }
    void Machine::popRegister(const Molecule& m) {
        try {
            // read the current field
            // get the destination register to use as a target
            popRegister(static_cast<Machine::TargetRegister>(getDestinationRegister(m.getByte(_registerIP.getAddress()))));
            _registerIP.increment();
        } catch (Problem& p) {
            throw Problem("pop.register", p.getMessage());
        }
    }
    void Machine::pushRegister(const Molecule& m) {
        try {
            // read the current field
            // get the destination register to use as a target
            pushRegister(static_cast<Machine::TargetRegister>(getDestinationRegister(m.getByte(_registerIP.getAddress()))));
            _registerIP.increment();
        } catch (Problem& p) {
            throw Problem("push.register", p.getMessage());
        }
    }
    void Machine::load(const Molecule& m) {
        try {
            // figure out which register to get the address from!
            auto target = static_cast<Machine::TargetRegister>(getDestinationRegister(m.getByte(_registerIP.getAddress())));
            if (involvesDiscriminantRegister(target)) {
                throw Problem("", "Can't use the discriminant field of a register as an address!");
            } else if (!legalValue(target)) {
                throw Problem("", "Illegal undefined register!");
            }
            using Type = decltype(target);
            load(getRegister(target).getAddress());
            switch (target) {
                case Type::RegisterA:
                    load(_registerA.getAddress());
                    break;
                case Type::RegisterB:
                    load(_registerB.getAddress());
                    break;
                case Type::RegisterC:
                    load(_registerC.getAddress());
                    break;
                case Type::RegisterIP:
                    load(_registerIP.getAddress());
                    break;
                case Type::RegisterS:
                    load(_registerS.getAddress());
                    break;
                case Type::RegisterX:
                    load(_registerX.getAddress());
                    break;
                default:
                    throw Problem("", "Undefined register!");
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
            case Type::RegisterTIP:
                return _registerIP;
            default:
                throw Problem("getRegister", "Undefined register!");
        }
    }

    void Machine::store(const Molecule& m) {
        try {
            auto tb = m.getByte(_registerIP.getAddress());
            // figure out which register to get the address from!
            auto dest = static_cast<Machine::TargetRegister>(getDestinationRegister(tb));
            if (!legalValue(dest)) {
                throw Problem("", "Illegal undefined destination register!");
            } else if (involvesDiscriminantRegister(dest)) {
                throw Problem("", "Can't use the discriminant field of a register as an address!");
            }
            auto src = static_cast<Machine::TargetRegister>(getSourceRegister(tb));
            if (!legalValue(dest)) {
                throw Problem("", "Illegal undefined source register!");
            }
            store(getRegister(dest).getAddress(), getRegister(src).getInt());
            _registerIP.increment();
        } catch (Problem& p) {
            throw Problem("load", p.getMessage());
        }
    }
    template<typename T, typename R, T mask, T shift = 0>
    constexpr T encodeBits(T value, R newValue) noexcept {
        return (value & ~mask) | ((static_cast<T>(newValue) << shift) & mask);
    }
    void Machine::setImmediate16Lowest(const Molecule& m) {
        try {
            _registerX.setValue(encodeBits<Address, QuarterAddress, 0x000000000000FFFF>(_registerX.getAddress(), m.getQuarterAddress(_registerIP.getAddress())));
            _registerIP.increment();
            _registerIP.increment();
        } catch (Problem& p) {
            throw Problem("set-immediate-lowest-16", p.getMessage());
        }
    }
    void Machine::setImmediate16Lower(const Molecule& m) {
        try {
            _registerX.setValue(encodeBits<Address, QuarterAddress, 0x00000000FFFF0000, 16>(_registerX.getAddress(), m.getQuarterAddress(_registerIP.getAddress())));
            _registerIP.increment();
            _registerIP.increment();
        } catch (Problem& p) {
            throw Problem("set-immediate-lower-16", p.getMessage());
        }
    }

    void Machine::setImmediate16Higher(const Molecule& m) {
        try {
            _registerX.setValue(encodeBits<Address, QuarterAddress, 0x0000FFFF00000000, 32>(_registerX.getAddress(), m.getQuarterAddress(_registerIP.getAddress())));
            _registerIP.increment();
            _registerIP.increment();
        } catch (Problem& p) {
            throw Problem("set-immediate-higher-16", p.getMessage());
        }
    }

    void Machine::setImmediate16Highest(const Molecule& m) {
        try {
            _registerX.setValue(encodeBits<Address, QuarterAddress, 0xFFFF000000000000, 48>(_registerX.getAddress(), m.getQuarterAddress(_registerIP.getAddress())));
            _registerIP.increment();
            _registerIP.increment();
        } catch (Problem& p) {
            throw Problem("set-immediate-highest-16", p.getMessage());
        }
    }
    void Machine::moveRegister(TargetRegister from, TargetRegister to) {
        if (from == to) {
            // do nothing :)
            return;
        }
        Register& src = getRegister(from);
        Register& dest = getRegister(to);
        if (involvesDiscriminantRegister(to)) {
            dest.setType(involvesDiscriminantRegister(from) ? src.getType() : static_cast<Discriminant>(src.getAddress()));
        } else {
            dest.setValue(involvesDiscriminantRegister(from) ? static_cast<Address>(src.getType()) : src.getValue());
        }
    }
    void Machine::swapRegisters(TargetRegister from, TargetRegister to) {
        if (from == to) {
            // do nothing
            return;
        }
        Register& src = getRegister(from);
        Register& dest = getRegister(to);
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
    }
    void Machine::swapRegisters(const Molecule& m) {
        auto args = m.getByte(_registerIP.getAddress());
        auto dest = getDestinationRegister(args);
        auto src = getDestinationRegister(args);
        swapRegisters(static_cast<TargetRegister>(src), static_cast<TargetRegister>(dest));
        _registerIP.increment();
    }

    void Machine::moveRegister(const Molecule& m) {
        auto args = m.getByte(_registerIP.getAddress());
        auto dest = getDestinationRegister(args);
        auto src = getDestinationRegister(args);
        moveRegister(static_cast<TargetRegister>(src), static_cast<TargetRegister>(dest));
        _registerIP.increment();
    }
} // end namespace forth


// TODO: add support for invoking some keywords at compile time!
int main() {
	forth::Machine machine (std::cout, std::cin);
	machine.controlLoop();
	return 0;
}
