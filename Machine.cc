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
		fn("A", _registerA.getValue());
		fn("B", _registerB.getValue());
		fn("C", _registerC.getValue());
		fn("T", _registerC.getType());
		fn("S", _registerS.getValue());
		fn("X", _registerX.getValue());
		fn("SP", _registerSP.getValue());
		fn("SP2", _registerSP2.getValue());
		fn("A.T", _registerA.getType());
		fn("B.T", _registerB.getType());
		fn("X.T", _registerX.getType());
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
	void Machine::notOperation(Operation op, const Molecule& m) {
		// invert register a
		auto tup = extractArgs2(op, m);
		auto& [dest, src] = tup;
		auto fn = [this, &dest](auto a) { dest.setValue(~a); };

		using Type = decltype(_registerC.getType());
		switch(_registerC.getType()) {
			case Type::Number:
				fn(src.getInt());
				break;
			case Type::MemoryAddress:
				fn(src.getAddress());
				break;
			case Type::Boolean:
				dest.setValue(!src.getTruth());
				break;
			default:
				throw Problem("not", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::minusOperation(Operation op, const Molecule& m) {
		auto tup = extractArgs2(op, m);
		auto& [dest, src] = tup;
		auto fn = [this, &dest](auto a) { dest.setValue(-a); };
		using Type = decltype(_registerC.getType());
		switch(_registerC.getType()) {
			case Type::Number:
				fn(src.getInt());
				break;
			case Type::FloatingPoint:
				fn(src.getFP());
				break;
			default:
				throw Problem("minus", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::multiplyOperation(Operation op, const Molecule& m) {
		using Type = decltype(_registerC.getType());
		auto tup = extractArguments(op, m, nullptr);
		auto [dest, src0, src1] = tup;
		auto fn = [this, &dest](auto a, auto b) { dest.setValue(a * b); };
		switch(_registerC.getType()) {
			case Type::Number:
				fn(src0.getInt(), src1.getInt());
				break;
			case Type::MemoryAddress:
				fn(src0.getAddress(), src1.getAddress());
				break;
			case Type::FloatingPoint:
				fn(src0.getFP(), src1.getFP());
				break;
			default:
				throw Problem("*", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::equals(Operation op, const Molecule& m) {
		auto tup = extractArguments(op, m, nullptr);
		auto& [dest, src0, src1] = tup;
		auto fn = [this, &dest](auto a, auto b) { dest.setValue(a == b); };
		using Type = decltype(_registerC.getType());
		switch(_registerC.getType()) {
			case Type::Number:
				fn(src0.getInt(), src1.getInt());
				break;
			case Type::MemoryAddress:
				fn(src0.getAddress(), src1.getAddress());
				break;
			case Type::FloatingPoint:
				fn(src0.getFP(), src1.getFP());
				break;
			case Type::Boolean:
				fn(src0.getTruth(), src1.getTruth());
				break;
			default:
				throw Problem("==", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::powOperation(Operation op, const Molecule& m) {
		auto tup = extractArguments(op, m, nullptr);
		auto [dest, src0, src1] = tup;
		auto fn = [&dest, this](auto a, auto b) {
			dest.setValue(static_cast<decltype(a)>(std::pow(a, b)));
		};
		using Type = decltype(_registerC.getType());
		switch(_registerC.getType()) {
			case Type::Number:
				fn(src0.getInt(), src1.getInt());
				break;
			case Type::MemoryAddress:
				fn(src0.getAddress(), src1.getAddress());
				break;
			case Type::FloatingPoint:
				fn(src0.getFP(), src1.getFP());
				break;
			default:
				throw Problem("**", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::divide(Operation op, const Molecule& m, bool remainder) {
		auto tup = extractArguments(op, m, nullptr);
		auto [dest, src0, src1] = tup;
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
	constexpr bool subtractOperation(Operation op) noexcept {
		switch(op) {
			case Operation::Subtract:
			case Operation::SubtractFull:
			case Operation::SubtractImmediate:
				return true;
			default:
				return false;
		}
	}
	constexpr bool immediateForm(Operation op) noexcept {
		switch(op) {
#define Immediate(x) case Operation :: x ## Immediate :
	Immediate(Add)
	Immediate(Subtract)
	Immediate(Multiply)
	Immediate(Divide)
	Immediate(Modulo)
	Immediate(And)
	Immediate(Or)
	Immediate(GreaterThan)
	Immediate(LessThan)
	Immediate(Xor)
	Immediate(ShiftRight)
	Immediate(ShiftLeft)
	Immediate(Equals)
				return true;
			default:
				return false;
#undef Immediate
		}
	}
	constexpr bool fullForm(Operation op) noexcept {
		switch(op) {
#define Full(x) case Operation :: x ## Full :
	Full(Add)
	Full(Subtract)
	Full(Multiply)
	Full(Divide)
	Full(Modulo)
	Full(And)
	Full(Or)
	Full(GreaterThan)
	Full(LessThan)
	Full(Xor)
	Full(ShiftRight)
	Full(ShiftLeft)
	Full(Equals)
	Full(Pow)
	Full(Not)
	Full(Minus)
				return true;
			default:
				return false;
#undef Full
		}
	}
	void Machine::numericCombine(bool subtract, Register& dest, const Register& src0, const Register& src1) {
		auto fn = [this, subtract](Register& dest, auto a, auto b) { 
			dest.setValue(subtract ? (a - b) : (a + b ));
		};
		switch(_registerC.getType()) {
			case Discriminant::Number:
				fn(dest, src0.getInt(), src1.getInt());
				break;
			case Discriminant::MemoryAddress:
				fn(dest, src0.getAddress(), src1.getAddress());
				break;
			case Discriminant::FloatingPoint:
				fn(dest, src0.getFP(), src1.getFP());
				break;
			default:
				throw Problem(subtract ? "-" : "+", "ILLEGAL DISCRIMINANT!");
		}

	}
	void Machine::numericCombine(bool subtract) {
		numericCombine(subtract, _registerC, _registerA, _registerB);
	}
	void Machine::numericCombine(Operation op, const Molecule& m) {
		if (op == Operation::Add || op == Operation::Subtract) {
			numericCombine(op == Operation::Subtract);
			return;
		}
		auto result = extractArguments(op, m, [this](Register& r, auto val) {
					if (_registerC.getType() == Discriminant::FloatingPoint) {
						r.setValue(static_cast<Floating>(val));
					} else {
						r.setValue(val);
					}
				});
		auto& [dest, src0, src1] = result;
		numericCombine(subtractOperation(op), dest, src0, src1);
	}
	std::tuple<TargetRegister, TargetRegister, TargetRegister> Machine::extractThreeRegisterForm(const Molecule& m, bool skipOverLastByte) {
		auto b0 = m.getByte(_registerIP.getAddress()); _registerIP.increment();
		auto b1 = m.getByte(_registerIP.getAddress()); _registerIP.increment();
		// skip over the last byte
        if (skipOverLastByte) {
		    m.getByte(_registerIP.getAddress()); _registerIP.increment();
        }
		return std::make_tuple(TargetRegister(getDestinationRegister(b0)), TargetRegister(getSourceRegister(b0)), TargetRegister(getDestinationRegister(b1)));
	}
	std::tuple<TargetRegister, TargetRegister, Address> Machine::extractThreeRegisterImmediateForm(const Molecule& m) {
		auto b0 = m.getByte(_registerIP.getAddress()); _registerIP.increment();
		auto b1 = m.getByte(_registerIP.getAddress()); _registerIP.increment();
		auto b2 = m.getByte(_registerIP.getAddress()); _registerIP.increment();
		return std::make_tuple(TargetRegister(getDestinationRegister(b0)), TargetRegister(getSourceRegister(b0)), Instruction::makeQuarterAddress(b1, b2));
	}
	void Machine::andOperation(Operation op, const Molecule& m) {
		using Type = decltype(_registerC.getType());
		auto result = extractArguments(op, m, nullptr);
		auto& [dest, src0, src1] = result;
		switch (_registerC.getType()) {
			case Type::Number:
				dest.setValue(src0.getInt() & src1.getInt());
				break;
			case Type::MemoryAddress:
				dest.setValue(src0.getAddress() & src1.getAddress());
				break;
			case Type::Boolean:
				dest.setValue(src0.getTruth() && src1.getTruth());
				break;
			default:
				throw Problem("and", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::orOperation(Operation op, const Molecule& m) {
		using Type = forth::Discriminant;
		auto result = extractArguments(op, m, nullptr);
		auto [dest, src0, src1] = result;
		switch (_registerC.getType()) {
			case Type::Number:
				dest.setValue(src0.getInt() | src1.getInt());
				break;
			case Type::MemoryAddress:
				dest.setValue(src0.getAddress() | src1.getAddress());
				break;
			case Type::Boolean:
				dest.setValue(src0.getTruth() || src1.getTruth());
				break;
			default:
				throw Problem("or", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::greaterThanOperation(Operation op, const Molecule& m) {
		using Type = decltype(_registerC.getType());
		auto result = false;
		auto tup = extractArguments(op, m, nullptr);
		auto [dest, src0, src1] = tup;
		switch (_registerC.getType()) {
			case Type::Number:
				result = src0.getInt() > src1.getInt();
				break;
			case Type::MemoryAddress:
				result = src0.getAddress() > src1.getAddress();
				break;
			case Type::FloatingPoint:
				result = src0.getFP() > src1.getFP();
				break;
			default:
				throw Problem(">", "ILLEGAL DISCRIMINANT!");
		}
		dest.setValue(result);
	}

	void Machine::lessThanOperation(Operation op, const Molecule& m) {
		using Type = decltype(_registerC.getType());
		auto result = false;
		auto tup = extractArguments(op, m, nullptr);
		auto [dest, src0, src1] = tup;
		switch (_registerC.getType()) {
			case Type::Number:
				result = src0.getInt() < src1.getInt();
				break;
			case Type::MemoryAddress:
				result = src0.getAddress() < src1.getAddress();
				break;
			case Type::FloatingPoint:
				result = src0.getFP() < src1.getFP();
				break;
			default:
				throw Problem(">", "ILLEGAL DISCRIMINANT!");
		}
		dest.setValue(result);
	}


	void Machine::xorOperation(Operation op, const Molecule& m) {
		using Type = decltype(_registerC.getType());
		auto tup = extractArguments(op, m, nullptr);
		auto [dest, src0, src1] = tup;
		switch (_registerC.getType()) {
			case Type::Number:
				dest.setValue(src0.getInt() ^ src1.getInt());
				break;
			case Type::MemoryAddress:
				dest.setValue(src0.getAddress() ^ src1.getAddress());
				break;
			case Type::Boolean:
				dest.setValue(bool(src0.getTruth() ^ src1.getTruth()));
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
	void Machine::shiftOperation(Operation op, const Molecule& m, bool shiftLeft) {
		using Type = decltype(_registerC.getType());
		auto tup = extractArguments(op, m, nullptr);
		auto& [dest, src0, src1] = tup;
		switch (_registerC.getType()) {
			case Type::Number:
				dest.setValue(shiftLeft ? (src0.getInt()  << src1.getInt()) : (src0.getInt() >> src1.getInt()));
				break;
			case Type::MemoryAddress:
				dest.setValue(shiftLeft ? (src0.getAddress()  << src1.getAddress()) : (src0.getAddress() >> src1.getAddress()));
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
	Machine::Machine(std::ostream& output, std::istream& input) :  _output(output), _input(input), _memory(new Datum[memoryCapacity]), _words(nullptr) { }

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
                if (!_registerC.getTruth()) {
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
		_registerIP.reset();
		// use the s register as the current instruction
		// slice out the lowest eight bits
		//auto molecule = _registerS.getMolecule();
		auto throwError = [this, molecule](Operation op) {

			std::stringstream msg;
			msg << "Unknown instruction address: 0x" << std::hex << static_cast<int>(op);
			auto str = msg.str();
			throw Problem("uc", str);
		};
		while (_registerIP.getAddress() < sizeof(Molecule)) {
			auto pos = _registerIP.getAddress();
			auto op = getOperation(molecule.getByte(pos));
			_registerIP.increment();

			switch (op) {
				case Operation::Stop: 
					return; // stop executing code within this molecule
				case Operation::Add:
				case Operation::AddFull:
				case Operation::AddImmediate:
				case Operation::Subtract:
				case Operation::SubtractFull:
				case Operation::SubtractImmediate:
					numericCombine(op, molecule); 
					break; // add or subtract with other forms as well
				case Operation::ShiftRight:
				case Operation::ShiftRightFull:
				case Operation::ShiftRightImmediate:
					shiftOperation(op, molecule, false); 
					break;
				case Operation::ShiftLeft:
				case Operation::ShiftLeftFull:
				case Operation::ShiftLeftImmediate:
					shiftOperation(op, molecule, true); 
					break;
				case Operation::Multiply: 
				case Operation::MultiplyFull:
				case Operation::MultiplyImmediate:
					multiplyOperation(op, molecule); 
					break;
				case Operation::Equals: 
				case Operation::EqualsFull: 
				case Operation::EqualsImmediate: 
					equals(op, molecule); 
					break;
				case Operation::Pow: 
				case Operation::PowFull:
					powOperation(op, molecule); 
					break;
				case Operation::Divide:
				case Operation::DivideFull:
				case Operation::DivideImmediate:
					divide(op, molecule);
					break;
				case Operation::Not:
				case Operation::NotFull:
					notOperation(op, molecule);
					break;
				case Operation::Minus:
				case Operation::MinusFull:
					minusOperation(op, molecule);
					break;
				case Operation::And: 
				case Operation::AndFull: 
				case Operation::AndImmediate: 
					andOperation(op, molecule); 
					break;
				case Operation::Or: 
				case Operation::OrFull: 
				case Operation::OrImmediate: 
					orOperation(op, molecule); 
					break;
				case Operation::GreaterThan: 
				case Operation::GreaterThanFull:
				case Operation::GreaterThanImmediate:
					greaterThanOperation(op, molecule); 
					break; // greater than
				case Operation::LessThan: 
				case Operation::LessThanFull:
				case Operation::LessThanImmediate:
					lessThanOperation(op, molecule); 
					break;
				case Operation::Xor: 
				case Operation::XorFull: 
				case Operation::XorImmediate: 
					xorOperation(op, molecule); 
					break;
				case Operation::TypeValue: 
					typeValue(); 
					break;
				case Operation::PopRegister: 
					popRegister(molecule); 
					break;
				case Operation::PushRegister: 
					pushRegister(molecule); 
					break;
				case Operation::Load: 
					load(molecule); 
					break;
				case Operation::Store: 
					store(molecule); 
					break;
				case Operation::SetImmediate16_Lowest: 
					setImmediate16<Immediate16Positions::Lowest>(molecule); 
					break;
				case Operation::SetImmediate16_Lower: 
					setImmediate16<Immediate16Positions::Lower>(molecule); 
					break;
				case Operation::SetImmediate16_Higher: 
					setImmediate16<Immediate16Positions::Higher>(molecule); 
					break;
				case Operation::SetImmediate16_Highest: 
					setImmediate16<Immediate16Positions::Highest>(molecule); 
					break;
				case Operation::Move:
					moveRegister(molecule); 
					break;
				case Operation::Swap: 
					swapRegisters(molecule); 
					break;
				case Operation::PopA: 
					popRegister(TargetRegister::A); 
					break;
				case Operation::PopB: 
					popRegister(TargetRegister::B); 
					break;
				case Operation::PopT: 
					popRegister(TargetRegister::T); 
					break;
				case Operation::PushC: 
					pushRegister(TargetRegister::C); 
					break;
				case Operation::PopC:
					popRegister(TargetRegister::C);
					break;
				case Operation::PushA:
					pushRegister(TargetRegister::A);
					break;
				case Operation::PushB:
					pushRegister(TargetRegister::B);
					break;
				default: 
					throwError(op);
					break;
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
			case Type::A:
			case Type::TA:
				return _registerA;
			case Type::B:
			case Type::TB:
				return _registerB;
			case Type::C:
			case Type::T:
				return _registerC;
			case Type::S:
				return _registerS;
			case Type::X:
			case Type::TX:
				return _registerX;
			case Type::IP:
				return _registerIP;
			case Type::SP:
				return _registerSP;
			case Type::SP2:
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
	bool Machine::keepExecuting() noexcept {
		static constexpr auto loadValueIntoX = Instruction::encodeOperation(
				Instruction::load(TargetRegister::X, TargetRegister::X));
        static constexpr auto loadLower = Instruction::loadAddressLowerHalf(TargetRegister::X, shouldKeepExecutingLocation);
        static constexpr auto loadUpper = Instruction::loadAddressUpperHalf(TargetRegister::X, shouldKeepExecutingLocation);
        dispatchInstructionStream<loadLower, loadUpper, loadValueIntoX>();
		return _registerX.getTruth();
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
		return _registerX.getTruth();
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
		return _registerC.getTruth();
	}
	bool Machine::stackFull(TargetRegister sp, Address location) {
		dispatchInstruction(Instruction::loadAddressLowerHalf(TargetRegister::X, location));
		dispatchInstruction(Instruction::loadAddressUpperHalf(TargetRegister::X, location));
		dispatchInstruction(Instruction::encodeOperation(
					Instruction::load(TargetRegister::S, TargetRegister::X),
					Instruction::equals(TargetRegister::C, TargetRegister::S, sp)));
		return _registerC.getTruth();
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
		return _registerC.getValue();
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
	std::tuple<Register&, Register&> Machine::extractArgs2(Operation op, const Molecule& m) {
		Register& dest = _registerC;
		Register& src = _registerA;
		if (fullForm(op)) {
			auto next = m.getByte(_registerIP.getAddress()); 
			_registerIP.increment();
			return std::forward_as_tuple(getRegister(TargetRegister(getDestinationRegister(next))), getRegister(TargetRegister(getSourceRegister(next))));
		} else {
			return std::forward_as_tuple(_registerC, _registerA);
		}
	}
	std::tuple<Register&, Register&, Register&> Machine::extractArguments(Operation op, const Molecule& m, std::function<void(Register&, Address)> onImmediate) {
		if (immediateForm(op)) {
			auto x = extractThreeRegisterImmediateForm(m);
			if (onImmediate) {
				onImmediate(_registerImmediate, std::get<2>(x));
			} else {
				_registerImmediate.setValue(std::get<2>(x));
			}
			auto tuple = std::forward_as_tuple(getRegister(std::get<0>(x)), getRegister(std::get<1>(x)), _registerImmediate);
			return tuple;
		} else if (fullForm(op)) {
			auto x = extractThreeRegisterForm(m);
			auto tup = std::forward_as_tuple(getRegister(std::get<0>(x)), getRegister(std::get<1>(x)), getRegister(std::get<2>(x)));
			return tup;
		} else {
			return std::forward_as_tuple(_registerC, _registerA, _registerB);
		}
	}
} // end namespace forth

