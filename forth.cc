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
		if (_registerC.truth) {
			_registerC._type = _registerA._type;
			_registerC._value = _registerA;
		} else {
			_registerC._type = _registerB._type;
			_registerC._value = _registerB;
		}
	}
	void Machine::invokeCRegister() {
		using Type = decltype(_registerC._type);
		switch (_registerC._type) {
			case Type::Word:
				_registerC.entry->operator()(this);
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
		_subroutine.push_back(_compileTarget);
		_compileTarget = new DictionaryEntry("");
		_compileTarget->markFakeEntry();
		auto word = readWord();
		const auto* entry = lookupWord(word);
		// Since else is optional, we have to be careful and somehow detail that 
		// this operation is 
		if (entry != nullptr) {
			// TODO: add support for nesting entries by adding addresses onto the
			// stack temporarily
			// load the c register from the stack
			_compileTarget->addSpaceEntry(_popC);
			// compile the address of the entry we're looking for into 
			_compileTarget->addLoadWordEntryIntoA(entry);
			// compile in a fake address to the nop command
			_compileTarget->addLoadWordEntryIntoB(_nop);
		} else {
			_compileTarget->addSpaceEntry(_popC);
			if (numberRoutine(word, true)) {
				_compileTarget->addSpaceEntry(_popTA);
				_compileTarget->addSpaceEntry(_popA);
				// compile in a fake address to the nop command
				_compileTarget->addLoadWordEntryIntoB(_nop);
			} else {
				throw Problem(word, "?");
			}
		}
	}
	void Machine::elseCondition() {
		if (!_compiling) {
			throw Problem("else", "must be defining a word!");
		}
		auto word = readWord();
		const auto* entry = lookupWord(word);
		// Right now, we just add a new entry to the current compileTarget
		if (entry != nullptr) {
			_compileTarget->addLoadWordEntryIntoB(entry);
		} else {
			if (numberRoutine(word, true)) {
				// we use the type data that will be pushed onto the stack
				_compileTarget->addSpaceEntry(_popTB);
				// this is a special case where we may need to make a custom fake 
				// word for the purposes of invocation
				_compileTarget->addSpaceEntry(_popB);
			} else {
				throw Problem(word, "?");
			}
		}
	}

	void Machine::thenStatement() {
		if (!_compiling) {
			throw Problem("then", "must be defining a word!");
		}
		if (_subroutine.empty()) {
			throw Problem("then", "Not in a function");
		}
		_compileTarget->addChooseOperation();
		_compileTarget->addInvokeCOperation();
		addWord(_compileTarget);
		auto parent = _subroutine.back();
		_subroutine.pop_back();
		parent->addSpaceEntry(_compileTarget);
		_compileTarget = parent;
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
		fn("A", _registerA);
		fn("B", _registerB);
		fn("C", _registerC);
		fn("T", _registerC._type);
		fn("S", _registerS._value);
		fn("X", _registerX._value);
		fn("A.T", _registerA._type);
		fn("B.T", _registerB._type);
		fn("X.T", _registerX._type);
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
		std::set<std::string> entries;
		for (const auto* entry = _words; entry != nullptr; entry = entry->getNext()) {
			entries.insert(entry->getName());
		}
		_output << "words: " << std::endl;
		for (const auto& entry : entries) {
			_output << "\t - " << entry << std::endl;
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
		using Type = decltype(_registerC._type);
		switch(_registerC._type) {
			case Type::Number:
				_registerC._value.numValue = ~_registerA.numValue;
				break;
			case Type::MemoryAddress:
				_registerC._value.address = ~_registerA._value.address;
				break;
			case Type::Boolean:
				_registerC.truth = !_registerA.truth;
				break;
			default:
				throw Problem("not", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::minusOperation() {
		using Type = decltype(_registerC._type);
		switch(_registerC._type) {
			case Type::Number:
				_registerC._value.numValue = -_registerA.numValue;
				break;
			case Type::FloatingPoint:
				_registerC.fp = -_registerA._value.fp;
				break;
			default:
				throw Problem("minus", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::multiplyOperation() {
		using Type = decltype(_registerC._type);
		switch(_registerC._type) {
			case Type::Number:
				_registerC._value.numValue = _registerA.numValue * _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC._value.address = _registerA._value.address * _registerB._value.address;
				break;
			case Type::FloatingPoint:
				_registerC.fp = _registerA._value.fp * _registerB.fp;
				break;
			default:
				throw Problem("*", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::equals() {
		using Type = decltype(_registerC._type);
		switch(_registerC._type) {
			case Type::Number:
				_registerC.truth = _registerA.numValue == _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC.truth = _registerA._value.address == _registerB._value.address;
				break;
			case Type::FloatingPoint:
				_registerC.truth = _registerA._value.fp == _registerB.fp;
				break;
			case Type::Boolean:
				_registerC.truth = _registerA.truth == _registerB.truth;
			default:
				throw Problem("==", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::powOperation() {
		using Type = decltype(_registerC._type);
		switch(_registerC._type) {
			case Type::Number:
				_registerC._value = Integer(std::pow(_registerA.numValue, _registerB.numValue));
				break;
			case Type::MemoryAddress:
				_registerC._value = Address(std::pow(_registerA._value.address, _registerB._value.address));
				break;
			case Type::FloatingPoint:
				_registerC._value = Floating(std::pow(_registerA._value.fp, _registerB.fp));
				break;
			default:
				throw Problem("**", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::divide() {
		using Type = decltype(_registerC._type);
		switch(_registerC._type) {
			case Type::Number:
				_registerC._value = _registerA.numValue / _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC._value = _registerA._value.address / _registerB._value.address;
				break;
			case Type::FloatingPoint:
				_registerC._value = _registerA._value.fp / _registerB.fp;
				break;
			default:
				throw Problem("/", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::modulo() {
		using Type = decltype(_registerC._type);
		switch(_registerC._type) {
			case Type::Number:
				_registerC._value = _registerA.numValue % _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC._value = _registerA._value.address % _registerB._value.address;
				break;
			default:
				throw Problem("mod", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::numericCombine(bool subtractOp) {
		switch(_registerC._type) {
			case Discriminant::Number:
				_registerC._value = subtractOp ? (_registerA.numValue - _registerB.numValue) : _registerA.numValue + _registerB.numValue;
				break;
			case Discriminant::MemoryAddress:
				_registerC._value = subtractOp ? (_registerA._value.address - _registerB._value.address) : _registerA._value.address + _registerB._value.address;
				break;
			case Discriminant::FloatingPoint:
				_registerC._value = subtractOp ? (_registerA._value.fp - _registerB.fp) : _registerA._value.fp + _registerB.fp;
				break;
			default:
				throw Problem(subtractOp ? "-" : "+", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::andOperation() {
		using Type = decltype(_registerC._type);
		switch (_registerC._type) {
			case Type::Number:
				_registerC._value = _registerA.numValue & _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC._value = _registerA._value.address & _registerB.numValue;
				break;
			case Type::Boolean:
				_registerC._value = _registerA.truth && _registerB.truth;
				break;
			default:
				throw Problem("and", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::orOperation() {
		using Type = decltype(_registerC._type);
		switch (_registerC._type) {
			case Type::Number:
				_registerC._value = _registerA.numValue | _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC._value = _registerA._value.address | _registerB.numValue;
				break;
			case Type::Boolean:
				_registerC._value = _registerA.truth || _registerB.truth;
				break;
			default:
				throw Problem("or", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::greaterThanOperation() {
		using Type = decltype(_registerC._type);
		switch (_registerC._type) {
			case Type::Number:
				_registerC._value = _registerA.numValue > _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC._value = _registerA._value.address > _registerB.numValue;
				break;
			case Type::FloatingPoint:
				_registerC._value = _registerA._value.fp > _registerB.fp;
				break;
			default:
				throw Problem(">", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::lessThanOperation() {
		using Type = decltype(_registerC._type);
		switch (_registerC._type) {
			case Type::Number:
				_registerC._value = _registerA.numValue < _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC._value = _registerA._value.address < _registerB.numValue;
				break;
			case Type::FloatingPoint:
				_registerC._value = _registerA._value.fp < _registerB.fp;
				break;
			default:
				throw Problem("<", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::xorOperation() {
		using Type = decltype(_registerC._type);
		switch (_registerC._type) {
			case Type::Number:
				_registerC._value = _registerA.numValue ^ _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC._value = _registerA._value.address ^ _registerB.numValue;
				break;
			case Type::Boolean:
				_registerC.truth = _registerA.truth ^ _registerB.truth;
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
		using Type = decltype(_registerC._type);
		switch (_registerC._type) {
			case Type::Number:
				_registerC._value.numValue = shiftLeft ? (_registerA.numValue  << _registerB.numValue) : (_registerA.numValue >> _registerB.numValue);
				break;
			case Type::MemoryAddress:
				_registerC._value.address = shiftLeft ? (_registerA._value.address  << _registerB._value.address) : (_registerA._value.address >> _registerB._value.address);
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
			Datum storage;
			storage.numValue = _memory[addr];
			return storage;
		}
	}
	void Machine::store(Address addr, const Datum& value) {
		if (addr > largestAddress) {
			throw Problem("mstore", "BAD ADDRESS!");
		} else {
			_memory[addr] = value.numValue;
		}
	}
	bool Machine::numberRoutine(const std::string& word, bool putTypeDataOntoStack) noexcept {
		// floating point
		// integers
		// first do some inspection first
		if (word == "true") {
			if (_compiling) {
				_compileTarget->addSpaceEntry(true);
				if (putTypeDataOntoStack) {
					_compileTarget->addTypeDataEntry(Discriminant::Boolean);
				}
			} else {
				pushParameter(true);
			}
			return true;
		}
		if (word == "false") {
			if (_compiling) {
				_compileTarget->addSpaceEntry(false);
				if (putTypeDataOntoStack) {
					_compileTarget->addTypeDataEntry(Discriminant::Boolean);
				}
			} else {
				pushParameter(false);
			}
			return true;
		}
		std::istringstream parseAttempt(word);
		if (word.find('u') != std::string::npos) {
			Address tmpAddress;
			parseAttempt >> tmpAddress;
			if (!parseAttempt.fail() && parseAttempt.eof()) {
				if (_compiling) {
					_compileTarget->addSpaceEntry(tmpAddress);
					if (putTypeDataOntoStack) {
						_compileTarget->addTypeDataEntry(Discriminant::MemoryAddress);
					}
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
					if (putTypeDataOntoStack) {
						_compileTarget->addTypeDataEntry(Discriminant::FloatingPoint);
					}
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
				if (putTypeDataOntoStack) {
					_compileTarget->addTypeDataEntry(Discriminant::Number);
				}
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
			addWord("pop.s", std::mem_fn(&Machine::pushSRegister));
			addWord("if", std::mem_fn(&Machine::ifCondition), true);
			addWord("else", std::mem_fn(&Machine::elseCondition), true);
			addWord("then", std::mem_fn(&Machine::thenStatement), true);
			addWord("uc", std::mem_fn(&Machine::dispatchInstruction));
			addWord("cache-basic-entries", std::mem_fn(&Machine::cacheBasicEntries));
		}
	}
	void Machine::printStack() {
		for (const auto& element : _parameter) {
			_output << "\t- " << element << std::endl;
		}
	}
	void Machine::dispatchInstruction() {
		// use the s register as the current instruction
		// slice out the lowest eight bits
		auto op = static_cast<byte>(_registerS._value.address & 0x00000000000000FF);

		switch (op) {
			case 0x00: break; // nop
			case 0x01: numericCombine(false); break; // add
			case 0x02: numericCombine(true); break;  // subtract
			case 0x03: multiplyOperation(); break; 
			case 0x04: divide(); break;
			case 0x05: modulo(); break;
			case 0x06: notOperation(); break; 
			case 0x07: minusOperation(); break; 
			case 0x08: andOperation(); break; 
			case 0x09: orOperation(); break; 
			case 0x0A: greaterThanOperation(); break; // greater than
			case 0x0B: lessThanOperation(); break;
			case 0x0C: xorOperation(); break;
			case 0x0D: shiftOperation(true); break; // shift left
			case 0x0E: shiftOperation(false); break; // shift right
			case 0x0F: popRegister(TargetRegister::RegisterA); break;
			case 0x10: popRegister(TargetRegister::RegisterB); break;
			case 0x11: popRegister(TargetRegister::RegisterC); break;
			case 0x12: popRegister(TargetRegister::RegisterS); break;
			case 0x13: popRegister(TargetRegister::RegisterX); break;
			case 0x14: popRegister(TargetRegister::RegisterT); break;
			case 0x15: popRegister(TargetRegister::RegisterTA); break;
			case 0x16: popRegister(TargetRegister::RegisterTB); break;
			case 0x17: popRegister(TargetRegister::RegisterTX); break;
			case 0x18: pushRegister(TargetRegister::RegisterA); break;
			case 0x19: pushRegister(TargetRegister::RegisterB); break;
			case 0x1A: pushRegister(TargetRegister::RegisterC); break;
			case 0x1B: pushRegister(TargetRegister::RegisterS); break;
			case 0x1C: pushRegister(TargetRegister::RegisterX); break;
			case 0x1D: pushRegister(TargetRegister::RegisterT); break;
			case 0x1E: pushRegister(TargetRegister::RegisterTA); break;
			case 0x1F: pushRegister(TargetRegister::RegisterTB); break;
			case 0x20: pushRegister(TargetRegister::RegisterTX); break;
			case 0x21: equals(); break;
			case 0x22: typeValue(); break;
			case 0x23: load(); break;
			case 0x24: store(); break;
			case 0x25: powOperation(); break;
			case 0x26: sinOperation(); break;
			case 0x27: cosOperation(); break;
			case 0x28: tanOperation(); break;
			case 0x29: atanOperation(); break;
			case 0x2A: atan2Operation(); break;
			default:
					   throw Problem("uc", "Unknown instruction address!");
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
		static constexpr Address max = (Address)Discriminant::Count;
		auto top(popParameter());
		if (involvesDiscriminantRegister(t) && top.address >= max) {
			throw Problem("pop.register", "ILLEGAL DISCRIMINANT!");
		}
		switch (t) {
			case Type::RegisterA:
				_registerA._value = top;
				break;
			case Type::RegisterB:
				_registerB._value = top;
				break;
			case Type::RegisterC:
				_registerC._value = top;
				break;
			case Type::RegisterT:
				_registerC._type = (Discriminant)top.address;
				break;
			case Type::RegisterTA:
				_registerA._type = (Discriminant)top.address;
				break;
			case Type::RegisterTB:
				_registerB._type = (Discriminant)top.address;
				break;
			case Type::RegisterTX:
				_registerX._type = (Discriminant)top.address;
				break;
			case Type::RegisterS:
				_registerS._value = top;
				break;
			case Type::RegisterX:
				_registerX._value = top;
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
				tmp = _registerA._value;
				break;
			case Type::RegisterB:
				tmp = _registerB._value;
				break;
			case Type::RegisterC:
				tmp = _registerC._value;
				break;
			case Type::RegisterT:
				tmp = static_cast<Address>(_registerC._type);
				break;
			case Type::RegisterTA:
				tmp = static_cast<Address>(_registerA._type);
				break;
			case Type::RegisterTB:
				tmp = static_cast<Address>(_registerB._type);
				break;
			case Type::RegisterTX:
				tmp = static_cast<Address>(_registerX._type);
				break;
			case Type::RegisterS:
				tmp = _registerS._value;
				break;
			case Type::RegisterX:
				tmp = _registerX._value;
				break;
			default:
				throw Problem("push.register", "Unknown register!");
		}
		pushParameter(tmp);
	}
	void Machine::sinOperation() {
		switch (_registerC._type) {
			case Discriminant::FloatingPoint:
				_registerC._value = sin(_registerA._value.fp);
				break;
			default:
				throw Problem("sin", "Incorret Discriminant!");
		}
	}
	void Machine::cosOperation() {
		switch (_registerC._type) {
			case Discriminant::FloatingPoint:
				_registerC._value = cos(_registerA._value.fp);
				break;
			default:
				throw Problem("cos", "Incorret Discriminant!");
		}
	}
	void Machine::tanOperation() {
		switch (_registerC._type) {
			case Discriminant::FloatingPoint:
				_registerC._value = tan(_registerA._value.fp);
				break;
			default:
				throw Problem("tan", "Incorret Discriminant!");
		}
	}
	void Machine::atanOperation() {
		switch (_registerC._type) {
			case Discriminant::FloatingPoint:
				_registerC._value = atan(_registerA._value.fp);
				break;
			default:
				throw Problem("atan", "Incorret Discriminant!");
		}
	}
	void Machine::atan2Operation() {
		switch (_registerC._type) {
			case Discriminant::FloatingPoint:
				_registerC._value = atan(_registerA._value.fp);
				break;
			default:
				throw Problem("atan", "Incorret Discriminant!");
		}
	}
} // end namespace forth


// TODO: add support for invoking some keywords at compile time!
int main() {
	forth::Machine machine (std::cout, std::cin);
	machine.controlLoop();
	return 0;
}
