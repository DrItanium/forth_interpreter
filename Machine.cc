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
#include "Assembler.h"
#include <iomanip>

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
			for (auto x = entry->begin(); x != entry->end(); ++x) {
				_output << "\t";
                if (x->_data.valueless_by_exception()) {
                    _output << std::endl;
                    throw Problem("see", "variant is empty!");
                } 
                std::visit(overloaded {
                            [this](Integer x) { _output << std::dec << x; },
                            [this](Address x) { _output << std::hex << "0x" << x; },
                            [this](Floating x) { _output << std::dec << x; },
                            [this](bool x) { _output << std::boolalpha << x << std::noboolalpha; },
                            [this](DictionaryEntry::Invokable x) { 
                                  auto innerTarget = std::get<0>(x);
                                  if (innerTarget->isFake()) {
                                     seeWord(innerTarget);
                                  } else {
                                    _output << innerTarget->getName();
                                  }
                            },
                            [this](const DictionaryEntry* x) {
                                if (x->isFake()) {
                                    _output << std::hex << "0x" << x;
                                } else {
                                    _output << x->getName(); 
                                }
                            }
                        }, x->_data);
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
        auto& dest = _core.getRegister(TargetRegister::C);
        auto& onTrue = _core.getRegister(TargetRegister::A);
        auto& onFalse = _core.getRegister(TargetRegister::B);
        if (dest.getTruth()) {
            dest.setValue(onTrue.getValue());
        } else {
            dest.setValue(onFalse.getValue());
        }
	}
	void Machine::invokeCRegister() {
        // SUPER FUCKING DANGEROUS!
        _core.getRegister(TargetRegister::C).getWord()->operator()(this);
	}
	void Machine::ifCondition() {
		//static constexpr auto prepRegisters = preCompileOperation<
		//		popB(), 
		//		popA(),
		//		popC()>();
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

        currentTarget->wordToPush(_compileTarget);
        currentTarget->wordToPush(elseBlock);
		//compileMicrocodeInvoke(prepRegisters, currentTarget);
	}
	void Machine::compileMicrocodeInvoke(const Molecule& m, DictionaryEntry* current) {
		current->addSpaceEntry(m._value);
		current->wordToInvoke(_microcodeInvoke);
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
		_compileTarget->wordToInvoke(lookupWord("choose.c"));
		_compileTarget->wordToInvoke(lookupWord("invoke.c"));
	}
	std::string Machine::readWord(bool allowEscapedQuotes) {
		std::string word;
        if (allowEscapedQuotes) {
		    _input >> std::quoted(word);
        } else {
            _input >> word;
        }
		return word;
	}
	void Machine::printRegisters() {
		auto printRegister = [](const std::string& title, TargetRegister reg) noexcept -> forth::EagerInstruction {
			return [title, reg](AssemblerBuilder& ab) {
				//ab.addInstruction(printChar(title),
				//				  printChar(": "),
				//				  typeDatum(reg),
				//				  printChar("\n"));
			};
		};
		//dispatchInstruction(
		//		printRegister("SP", TargetRegister::SP),
		//		printRegister("SP2", TargetRegister::SP2),
		//		printRegister("A", TargetRegister::A),
		//		printRegister("B", TargetRegister::B),
		//		printRegister("C", TargetRegister::C),
		//		printRegister("X", TargetRegister::X),
		//		printRegister("S", TargetRegister::S),
		//		printRegister("Index", TargetRegister::Index),
		//		printRegister("DP", TargetRegister::DP));
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
	Machine::Machine(std::ostream& output, std::istream& input) :  _output(output), _input(input), _words(nullptr) { }

	Datum Machine::popParameter() {
	//	dispatchInstruction(forth::popRegister(TargetRegister::Temporary, TargetRegister::SP));
		return _core.getRegister(TargetRegister::Temporary).getValue();
	}

	void Machine::pushParameter(Datum value) {
		_core.push(value, TargetRegister::SP);
	}

	bool Machine::numberRoutine(const std::string& word) noexcept {
        auto saveToStack = [this](const Datum& value) {
            if (inCompilationMode()) {
                _compileTarget->addSpaceEntry(value.address);
            } else {
			    dispatchInstruction(loadImmediate64(TargetRegister::C, value.address), pushC());
            }
        };
		if (word.empty()) { 
			return false; 
		}
		// floating point
		// integers
		// first do some inspection first
		// We need to load into c and then push it to the stack
		if (word == "true") {
            if (inCompilationMode()) {
                _compileTarget->addSpaceEntry(true);
            } else {
			    //dispatchInstruction(addiu(TargetRegister::C, TargetRegister::Zero, 1), pushC());
            }
			return true;
		}
		if (word == "false") {
            if (inCompilationMode()) {
                _compileTarget->addSpaceEntry(false);
            } else {
			    dispatchInstruction(zeroRegister(TargetRegister::C), pushC());
            }
			return true;
		}
		std::istringstream parseAttempt(word);
		if (word.find('#') != std::string::npos) {
			Address tmpAddress;
			parseAttempt >> std::hex >> tmpAddress;
			if (!parseAttempt.fail()) {
				// TODO: do checks to compact parsing
                saveToStack(tmpAddress);
				return true;
			}
			return false;
		}
		if (word.find('u') != std::string::npos) {
			Address tmpAddress;
			parseAttempt >> tmpAddress;
			if (!parseAttempt.fail()) {
                saveToStack(tmpAddress);
				return true;
			}
			return false;
		}
		parseAttempt.clear();
		if (word.find('.') != std::string::npos) {
			Floating tmpFloat;
			parseAttempt >> tmpFloat;
			if (!parseAttempt.fail() && parseAttempt.eof()) {
                saveToStack(tmpFloat);
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
            saveToStack(tmpInt);
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
                            _compileTarget->wordToInvoke(entry);
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
			addWord("do", std::mem_fn(&Machine::doStatement), true);
			addWord("continue", std::mem_fn(&Machine::continueStatement), true);
			addWord("choose.c", std::mem_fn(&Machine::chooseRegister));
			addWord("invoke.c", std::mem_fn(&Machine::invokeCRegister));
            addWord("'", std::mem_fn(&Machine::injectWord));
            addWord("execute", std::mem_fn(&Machine::executeTop));
            addWord("raiseError", std::mem_fn(&Machine::raiseError));
            addWord("uc", std::mem_fn(&Machine::invokeCore));
            addWord("quit", std::mem_fn(&Machine::terminateControlLoop));
            addWord("\"", std::mem_fn(&Machine::constructString), true);
            addWord(",str", std::mem_fn(&Machine::printString));
            addWord(".CR", std::mem_fn(&Machine::printNewLine));
			_microcodeInvoke = lookupWord("uc");
		}
	}
    void Machine::printNewLine() {
        _output << std::endl;
    }
    void Machine::printString() {
        _output << *(popParameter()._string);
    }
    void Machine::constructString() {
			auto flags = _output.flags();
            // keep reading until we get a word that ends with "
            std::stringstream input;
            while (true) {
                auto str = readWord();
                input << str << " ";
                if (!str.empty() && str.back() == '"') {
                    // remove the ending "
                    auto s = input.str();
                    _stringCache.emplace_back(s.substr(0, s.size() - 2));
                    // place the reference onto the stack
                    if (inCompilationMode()) {
                        _compileTarget->addSpaceEntry(_stringCache.back());
                    } else {
                        pushParameter(_stringCache.back());
                    }
                    break;
                }
            }
            _output.setf(flags);
    }
    void Machine::terminateControlLoop() {
		//dispatchInstruction(loadImmediate64(TargetRegister::X, terminateControlLoopLocation),
		//					forth::load(TargetRegister::X, TargetRegister::X),
		//					jumpIndirect(TargetRegister::X));
    }
    void Machine::invokeCore() {
        // invoke a single molecule/address
        //auto value = Molecule(popParameter().address);
        //dispatchInstruction(value);
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
			//dispatchInstruction(m);
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
				//dispatchInstruction(popAB(), cmpeq());
				if (_core.getRegister(TargetRegister::C).getTruth()) {
				//	dispatchInstruction(pushB(), pushA());
                    // super gross but far more accurately models the underlying micro architecture
loopTop:
					body->operator()(m);
					// compacted operation:
					// popa
					// popb
					// eq
					//dispatchInstruction(popAB(), cmpeq());
                    if (!_core.getRegister(TargetRegister::C).getTruth()) {
						//dispatchInstruction(pushB(), pushA()); // put the values back on the stack
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
				//static constexpr auto checkCondition = preCompileOperation< popA(), notl()>();
				//static_assert(0x061c == checkCondition, "conditional operation failed!");
                // super gross but far more accurately models the underlying micro architecture
endLoopTop:
				body->operator()(m);
				// pop.a
				// not
				//microcodeInvoke(checkCondition);
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
	void Machine::dispatchInstruction(AssemblerBuilder& ab) {
		//ab.addInstruction(forth::returnToNative());
		//ab.installIntoMemory(_core.getInstructionInstallationFunction());
		//_core.executionCycle(ab.getBaseAddress());
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
	bool Machine::keepExecuting() noexcept {
		//dispatchInstruction(loadImmediate64(TargetRegister::X, shouldKeepExecutingLocation),
		//		forth::load(TargetRegister::X, TargetRegister::X));
		return _core.getRegister(TargetRegister::X).getTruth();
	}

	void Machine::handleError(const std::string& word, const std::string& msg) noexcept {
		// clear the stacks and the input pointer
		//dispatchInstruction(loadImmediate64(TargetRegister::X, parameterStackEmptyLocation),
		//				  forth::load(TargetRegister::SP, TargetRegister::X));
		clearSubroutineStack();
		_input.clear();
		_input.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
		_output << word << " " << msg << std::endl;
		if (_compileTarget != nullptr) {
			delete _compileTarget;
			_compileTarget = nullptr;
		}
		//dispatchInstruction(loadImmediate64(TargetRegister::X, isCompilingLocation), 
		//		forth::store(TargetRegister::X, TargetRegister::Zero));
	}
	void Machine::dispatchInstruction() {
		// just pop a molecule off of the stack and pass it to
		// dispatchInstruction
		//Molecule tmp(popParameter().address);
		//dispatchInstruction(tmp);
	}
	bool Machine::inCompilationMode() noexcept {
		//dispatchInstruction(loadImmediate64(TargetRegister::X, isCompilingLocation),
		//				  forth::load(TargetRegister::X, TargetRegister::X));
		return _core.getRegister(TargetRegister::X).getTruth();
	}
	void Machine::activateCompileMode() {
		//dispatchInstruction(loadImmediate64(TargetRegister::X, isCompilingLocation),
		//		loadImmediate16(TargetRegister::C, 1),
		//		forth::store(TargetRegister::X, TargetRegister::C));
	}
	void Machine::deactivateCompileMode() {
		//dispatchInstruction(loadImmediate64(TargetRegister::X, isCompilingLocation),
		//		forth::store(TargetRegister::X, TargetRegister::Zero));
	}

	bool Machine::stackEmpty(TargetRegister sp, Address location) {
		//dispatchInstruction(loadImmediate64(TargetRegister::X, location),
		//		forth::load(TargetRegister::Temporary, TargetRegister::X),
		//		cmpeq(TargetRegister::C, TargetRegister::Temporary, sp));
		return _core.getRegister(TargetRegister::C).getTruth();
	}
	bool Machine::stackFull(TargetRegister sp, Address location) {
		//dispatchInstruction(loadImmediate64(TargetRegister::X, location),
		//		forth::load(TargetRegister::Temporary, TargetRegister::X),
		//		cmpeq(TargetRegister::C, TargetRegister::Temporary, sp));
		return _core.getRegister(TargetRegister::C).getTruth();
	}
	void Machine::pushOntoStack(TargetRegister sp, Datum value, Address fullLocation) {
		if (stackFull(sp, fullLocation)) {
			throw Problem("pushOntoStack", "STACK FULL!!!");
		}
		//dispatchInstruction(loadImmediate64(TargetRegister::X, value.address),
		//		pushRegister(TargetRegister::X, sp));
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
		//dispatchInstruction(popRegister(TargetRegister::C, sp));
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
		//dispatchInstruction(loadImmediate64(TargetRegister::X, subroutineStackEmptyLocation),
		//		forth::load(TargetRegister::SP2, TargetRegister::X));
	}

	void Machine::printStack() {
        // load the bottom of the stack
		/*
		dispatchInstruction(loadImmediate64(TargetRegister::X, parameterStackEmptyLocation),
				forth::load(TargetRegister::X, TargetRegister::X),
				zeroRegister(TargetRegister::A),
				forth::move(TargetRegister::B, TargetRegister::SP),
				cmpeq(TargetRegister::C, TargetRegister::X, TargetRegister::B),
				forth::conditionalBranch(TargetRegister::C, "done"),
				label("loopRestart"),
				forth::load(TargetRegister::A, TargetRegister::B),
				forth::addiu(TargetRegister::B, TargetRegister::B, 8),
				cmpeq(TargetRegister::C, TargetRegister::X, TargetRegister::B),
				printChar("\t- "),
				typeDatum(TargetRegister::A),
				printChar('\n'),
				notb(TargetRegister::C, TargetRegister::C),
				forth::conditionalBranch(TargetRegister::C, "loopRestart"),
				label("done"));
				*/
	}
	std::function<void(Address, Address)> Machine::getMemoryInstallationFunction() {
		return _core.getMemoryInstallationFunction();
	}
	//std::function<void(Address, Address)> Machine::getInstructionInstallationFunction() {
	//	return _core.getInstructionInstallationFunction();
	//}
} // end namespace forth
