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

namespace forth {
    Problem::Problem(const std::string& word, const std::string& message) : _word(word), _message(message) { }
    void DictionaryEntry::addTypeDataEntry(forth::Discriminant type) {
        addSpaceEntry(static_cast<Address>(type));
    }
    void DictionaryEntry::operator()(Machine* machine) const {
        if (_code != nullptr) {
            _code(machine);
        } else {
            try {
                for (const auto & value : _space) {
                    value(machine);
                }
            } catch (Problem& p) {
                throw Problem(getName(), p.getMessage());
            }
        }
    }
    void DictionaryEntry::addChooseOperation() {
        addSpaceEntry(SpaceEntry::Discriminant::ChooseRegisterAndStoreInC, nullptr);
    }
    void DictionaryEntry::addInvokeCOperation() {
        addSpaceEntry(SpaceEntry::Discriminant::InvokeRegisterC, nullptr);
    }
    void DictionaryEntry::addSpaceEntry(SpaceEntry::Discriminant type, const DictionaryEntry* value) {
        SpaceEntry se;
        se._type = type;
        se._entry = value;
        _space.emplace_back(se);
    }
    void DictionaryEntry::addLoadWordEntryIntoA(const DictionaryEntry* value) {
        addSpaceEntry(SpaceEntry::Discriminant::LoadWordIntoA, value);
    }

    void DictionaryEntry::addLoadWordEntryIntoB(const DictionaryEntry* value) {
        addSpaceEntry(SpaceEntry::Discriminant::LoadWordIntoB, value);
    }


	void DictionaryEntry::addSpaceEntry(Integer x) {
		SpaceEntry se;
		se._type = SpaceEntry::Discriminant::Signed;
		se._int = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(Address x) {
		SpaceEntry se;
		se._type = SpaceEntry::Discriminant::Unsigned;
		se._addr = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(Floating x) {
		SpaceEntry se;
		se._type = SpaceEntry::Discriminant::FloatingPoint;
		se._fp = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(bool x) {
		SpaceEntry se;
		se._type = SpaceEntry::Discriminant::Boolean;
		se._truth = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(const DictionaryEntry* x) {
		SpaceEntry se;
		se._type = SpaceEntry::Discriminant::DictEntry;
		se._entry = x;
		_space.emplace_back(se);
	}


	DictionaryEntry::DictionaryEntry(const std::string& name, NativeMachineOperation code) : _name(name), _code(code), _next(nullptr) { }


	class Machine {
		public:
			static constexpr auto largestAddress = 0xFFFFFF;
			static constexpr auto memoryCapacity = (largestAddress + 1);
		public:
			Machine(std::ostream& output, std::istream& input);
			~Machine() = default;
			const DictionaryEntry* lookupWord(const std::string& word) noexcept;
			void controlLoop() noexcept;
			void handleError(const std::string& word, const std::string& msg) noexcept;
			Datum load(Address addr);
			void load() { _registerC = load(_registerA.address); }
			void store(Address addr, const Datum& value);
			void store() { store(_registerA.address, _registerB); }
            void pushWord(DictionaryEntry* entry);
			void pushParameter(Datum value);
			Datum popParameter();
			bool numberRoutine(const std::string& word, bool putTypeDataOntoStack = false) noexcept;
			void typeValue(Discriminant discriminant, const Datum& value);
			void typeValue(const Datum& value) { typeValue(_registerT, value); }
			void typeValue() { typeValue(_registerA); }
			void addWord(DictionaryEntry* entry);
			void addWord(const std::string& name, NativeMachineOperation op, bool compileTimeInvoke = false);
			void addition(Discriminant type);
			void listWords();
			void activateCompileMode() { _compiling = true; }
			void deactivateCompileMode() { _compiling = false; }
			void defineWord();
			void endDefineWord();
			void semicolonOperation();
			void setA(const Datum& target) noexcept { _registerA = target; }
			void setTA(Discriminant target) noexcept { _registerTA = target; }
			void setTB(Discriminant target) noexcept { _registerTB = target; }
			void setB(const Datum& target) noexcept { _registerB = target; }
            void chooseRegister();
            void invokeCRegister();
		private:
            enum class TargetRegister : byte {
                RegisterA,
                RegisterTA,
                RegisterB,
                RegisterTB,
                RegisterC,
                RegisterT,
            };
            static constexpr bool involvesDiscriminantRegister(TargetRegister r) {
                switch (r) {
                    case TargetRegister::RegisterT:
                    case TargetRegister::RegisterTA:
                    case TargetRegister::RegisterTB:
                        return true;
                    default:
                        return false;
                }
            }
			void printRegisters();
            void printStack();
            template<TargetRegister t>
            void pushRegister() {
                using Type = decltype(t);
                Datum tmp;
                switch (t) {
                    case Type::RegisterA:
                        tmp = _registerA;
                        break;
                    case Type::RegisterB:
                        tmp = _registerB;
                        break;
                    case Type::RegisterC:
                        tmp = _registerC;
                        break;
                    case Type::RegisterT:
                        tmp = static_cast<Address>(_registerT);
                        break;
                    case Type::RegisterTA:
                        tmp = static_cast<Address>(_registerTA);
                        break;
                    case Type::RegisterTB:
                        tmp = static_cast<Address>(_registerTB);
                        break;
                    default:
                        throw Problem("push.register", "Unknown register!");
                }
                pushParameter(tmp);
            }
            template<TargetRegister t>
            void popRegister() {
                using Type = decltype(t);
                static constexpr Address max = (Address)Discriminant::Count;
                auto top(popParameter());
                if (involvesDiscriminantRegister(t) && top.address >= max) {
                    throw Problem("pop.register", "ILLEGAL DISCRIMINANT!");
                }
                switch (t) {
                    case Type::RegisterA:
                        _registerA = top;
                        break;
                    case Type::RegisterB:
                        _registerB = top;
                        break;
                    case Type::RegisterC:
                        _registerC = top;
                        break;
                    case Type::RegisterT:
                        _registerT = (Discriminant)top.address;
                        break;
                    case Type::RegisterTA:
                        _registerTA = (Discriminant)top.address;
                        break;
                    case Type::RegisterTB:
                        _registerTA = (Discriminant)top.address;
                        break;
                    default:
                        throw Problem("pop.register", "Unknown register!");
                }

            }

            void aluOperation();
			void add();
			void subtract();
			void multiplyOperation();
			void equals();
			void powOperation();
			void modulo();
			void divide();
			void notOperation();
			void minusOperation();
			void andOperation();
			void orOperation();
			void greaterThanOperation();
			void lessThanOperation();
			void xorOperation();
			void shiftLeftOperation();
			void shiftRightOperation();
            void ifCondition();
            void elseCondition();
            void thenStatement();
            /**
             * Printout the contents of the given word!
             */
            void seeWord();
		private:
			void terminateExecution();
            void seeWord(const DictionaryEntry* entry);
			void initializeBaseDictionary();
			std::string readWord();
		private:
			// define the CPU that the forth interpreter sits on top of
			std::ostream& _output;
			std::istream& _input;
			std::unique_ptr<Integer[]> _memory;
			DictionaryEntry* _words;
			// no need for the subroutine stack
            std::list<DictionaryEntry*> _subroutine;
            std::list<Datum> _parameter;
			bool _initializedBaseDictionary = false;
			bool _keepExecuting = true;
			bool _compiling = false;
			DictionaryEntry* _compileTarget = nullptr;
			// internal "registers"
			Datum _registerA, _registerB, _registerC;
			Discriminant _registerT, _registerTA, _registerTB;
            const DictionaryEntry* _popTA = nullptr;
            const DictionaryEntry* _popTB = nullptr;
            const DictionaryEntry* _popA = nullptr;
            const DictionaryEntry* _popB = nullptr;
            const DictionaryEntry* _popC = nullptr;
            const DictionaryEntry* _popT = nullptr;
            const DictionaryEntry* _pushA = nullptr;
            const DictionaryEntry* _pushB = nullptr;
            const DictionaryEntry* _pushC = nullptr;
            const DictionaryEntry* _pushT = nullptr;
            const DictionaryEntry* _nop = nullptr;

	};
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
            _registerT = _registerTA;
            _registerC = _registerA;
        } else {
            _registerT = _registerTB;
            _registerC = _registerB;
        }
    }
    void Machine::invokeCRegister() {
        using Type = decltype(_registerT);
        switch (_registerT) {
            case Type::Word:
                _registerC.entry->operator()(this);
                break;
            case Type::Number:
            case Type::FloatingPoint:
                pushRegister<TargetRegister::RegisterC>();
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
		auto fn = [this](const std::string& title, const Datum& r) {
			_output << title << ": " << r << std::endl;
		};
        auto printT = [this](const std::string& title, auto value) {
            _output << title << ": 0x" << std::hex << static_cast<Address>(value) << std::endl;
        };
		fn("A", _registerA);
		fn("B", _registerB);
		fn("C", _registerC);
        printT("T", _registerT);
        printT("A.T", _registerTA);
        printT("B.T", _registerTB);
#ifdef DEBUG
        // extended data
        _output << "IsCompiling: " << _compiling << std::endl;
#endif
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
    void DictionaryEntry::SpaceEntry::operator()(Machine* machine) const {
        invoke(machine);
    }
	void DictionaryEntry::SpaceEntry::invoke(Machine* machine) const {
        using Type = DictionaryEntry::SpaceEntry::Discriminant;
		switch (_type) {
			case Type::Signed:
				machine->pushParameter(_int);
				break;
			case Type::Unsigned:
				machine->pushParameter(_addr);
				break;
			case Type::FloatingPoint:
				machine->pushParameter(_fp);
				break;
			case Type::Boolean:
				machine->pushParameter(_truth);
				break;
			case Type::DictEntry:
				_entry->operator()(machine);
				break;
            case Type::LoadWordIntoA:
                machine->setTA(forth::Discriminant::Word);
                machine->setA(_entry);
                break;
            case Type::LoadWordIntoB:
                machine->setTB(forth::Discriminant::Word);
                machine->setB(_entry);
                break;
            case Type::ChooseRegisterAndStoreInC:
                machine->chooseRegister();
                break;
            case Type::InvokeRegisterC:
                machine->invokeCRegister();
                break;
			default:
                throw Problem("unknown", "UNKNOWN ENTRY KIND!");
		}
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
		using Type = decltype(_registerT);
		switch(_registerT) {
			case Type::Number:
				_registerC.numValue = ~_registerA.numValue;
				break;
			case Type::MemoryAddress:
				_registerC.address = ~_registerA.address;
				break;
			case Type::Boolean:
				_registerC.truth = !_registerA.truth;
				break;
			default:
                throw Problem("not", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::minusOperation() {
		using Type = decltype(_registerT);
		switch(_registerT) {
			case Type::Number:
				_registerC.numValue = -_registerA.numValue;
				break;
			case Type::FloatingPoint:
				_registerC.fp = -_registerA.fp;
				break;
			default:
                throw Problem("minus", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::multiplyOperation() {
		using Type = decltype(_registerT);
		switch(_registerT) {
			case Type::Number:
				_registerC.numValue = _registerA.numValue * _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC.address = _registerA.address * _registerB.address;
				break;
			case Type::FloatingPoint:
				_registerC.fp = _registerA.fp * _registerB.fp;
				break;
			default:
                throw Problem("*", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::equals() {
		using Type = decltype(_registerT);
		switch(_registerT) {
			case Type::Number:
				_registerC.truth = _registerA.numValue == _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC.truth = _registerA.address == _registerB.address;
				break;
			case Type::FloatingPoint:
				_registerC.truth = _registerA.fp == _registerB.fp;
				break;
			case Type::Boolean:
				_registerC.truth = _registerA.truth == _registerB.truth;
			default:
                throw Problem("==", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::powOperation() {
		using Type = decltype(_registerT);
		switch(_registerT) {
			case Type::Number:
				_registerC = Integer(std::pow(_registerA.numValue, _registerB.numValue));
				break;
			case Type::MemoryAddress:
				_registerC = Address(std::pow(_registerA.address, _registerB.address));
				break;
			case Type::FloatingPoint:
				_registerC = Floating(std::pow(_registerA.fp, _registerB.fp));
				break;
			default:
                throw Problem("**", "ILLEGAL DISCRIMINANT!");
		}
	}
	void Machine::subtract() {
		using Type = decltype(_registerT);
		switch(_registerT) {
			case Type::Number:
				_registerC = _registerA.numValue - _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC = _registerA.address - _registerB.address;
				break;
			case Type::FloatingPoint:
				_registerC = _registerA.fp - _registerB.fp;
				break;
			default:
                throw Problem("-", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::divide() {
		using Type = decltype(_registerT);
		switch(_registerT) {
			case Type::Number:
				_registerC = _registerA.numValue / _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC = _registerA.address / _registerB.address;
				break;
			case Type::FloatingPoint:
				_registerC = _registerA.fp / _registerB.fp;
				break;
			default:
                throw Problem("/", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::modulo() {
		using Type = decltype(_registerT);
		switch(_registerT) {
			case Type::Number:
				_registerC = _registerA.numValue % _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC = _registerA.address % _registerB.address;
				break;
			default:
                throw Problem("mod", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::add() {
		switch(_registerT) {
			case Discriminant::Number:
				_registerC = _registerA.numValue + _registerB.numValue;
				break;
			case Discriminant::MemoryAddress:
				_registerC = _registerA.address + _registerB.address;
				break;
			case Discriminant::FloatingPoint:
				_registerC = _registerA.fp + _registerB.fp;
				break;
			default:
                throw Problem("+", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::andOperation() {
		using Type = decltype(_registerT);
		switch (_registerT) {
			case Type::Number:
				_registerC = _registerA.numValue & _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC = _registerA.address & _registerB.numValue;
				break;
			case Type::Boolean:
				_registerC = _registerA.truth && _registerB.truth;
				break;
			default:
                throw Problem("and", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::orOperation() {
		using Type = decltype(_registerT);
		switch (_registerT) {
			case Type::Number:
				_registerC = _registerA.numValue | _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC = _registerA.address | _registerB.numValue;
				break;
			case Type::Boolean:
				_registerC = _registerA.truth || _registerB.truth;
				break;
			default:
                throw Problem("or", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::greaterThanOperation() {
		using Type = decltype(_registerT);
		switch (_registerT) {
			case Type::Number:
				_registerC = _registerA.numValue > _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC = _registerA.address > _registerB.numValue;
				break;
			case Type::FloatingPoint:
				_registerC = _registerA.fp > _registerB.fp;
				break;
			default:
                throw Problem(">", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::lessThanOperation() {
		using Type = decltype(_registerT);
		switch (_registerT) {
			case Type::Number:
				_registerC = _registerA.numValue < _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC = _registerA.address < _registerB.numValue;
				break;
			case Type::FloatingPoint:
				_registerC = _registerA.fp < _registerB.fp;
				break;
			default:
                throw Problem("<", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::xorOperation() {
		using Type = decltype(_registerT);
		switch (_registerT) {
			case Type::Number:
				_registerC = _registerA.numValue ^ _registerB.numValue;
				break;
			case Type::MemoryAddress:
				_registerC = _registerA.address ^ _registerB.numValue;
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

	void Machine::shiftLeftOperation() {
		using Type = decltype(_registerT);
		switch (_registerT) {
			case Type::Number:
				_registerC.numValue = _registerA.numValue << _registerB.address;
				break;
			case Type::MemoryAddress:
				_registerC.address = _registerA.address << _registerB.address;
				break;
			default:
                throw Problem("<<", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::shiftRightOperation() {
		using Type = decltype(_registerT);
		switch (_registerT) {
			case Type::Number:
				_registerC.numValue = _registerA.numValue >> _registerB.address;
				break;
			case Type::MemoryAddress:
				_registerC.address = _registerA.address >> _registerB.address;
				break;
			default:
                throw Problem(">>", "ILLEGAL DISCRIMINANT!");
		}
	}

	void Machine::addWord(DictionaryEntry* entry) {
		if (_words != nullptr) {
			entry->setNext(_words);
		}
		_words = entry;
	}
	void Machine::typeValue(Discriminant discriminant, const Datum& value) {
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
            addWord("nop", [](Machine*) { });
			addWord("quit", std::mem_fn(&Machine::terminateExecution));
			addWord(";", std::mem_fn(&Machine::semicolonOperation));
			addWord("registers", std::mem_fn(&Machine::printRegisters));
			addWord("words", std::mem_fn(&Machine::listWords));
            addWord("stack", std::mem_fn(&Machine::printStack));
			addWord(":", std::mem_fn(&Machine::defineWord));
            addWord("see", std::mem_fn<void()>(&Machine::seeWord));
			addWord("type.a", std::mem_fn<void()>(&Machine::typeValue));
			addWord("pop.t",  std::mem_fn(&Machine::popRegister<TargetRegister::RegisterT>));
            addWord("pop.tb", std::mem_fn(&Machine::popRegister<TargetRegister::RegisterTB>));
            addWord("pop.ta", std::mem_fn(&Machine::popRegister<TargetRegister::RegisterTA>));
			addWord("pop.a", std::mem_fn(&Machine::popRegister<TargetRegister::RegisterA>));
			addWord("pop.b", std::mem_fn(&Machine::popRegister<TargetRegister::RegisterB>));
			addWord("pop.c", std::mem_fn(&Machine::popRegister<TargetRegister::RegisterC>));
			addWord("push.a", std::mem_fn(&Machine::pushRegister<TargetRegister::RegisterA>));
			addWord("push.b", std::mem_fn(&Machine::pushRegister<TargetRegister::RegisterB>));
			addWord("push.c", std::mem_fn(&Machine::pushRegister<TargetRegister::RegisterC>));
			addWord("push.t", std::mem_fn(&Machine::pushRegister<TargetRegister::RegisterT>));
			addWord("mload", std::mem_fn<void()>(&Machine::load));
			addWord("mstore", std::mem_fn<void()>(&Machine::store));
			addWord("+", std::mem_fn(&Machine::add));
			addWord("-", std::mem_fn(&Machine::subtract));
			addWord("not.a", std::mem_fn(&Machine::notOperation));
			addWord("minus.a", std::mem_fn(&Machine::minusOperation));
			addWord("*", std::mem_fn(&Machine::multiplyOperation));
			addWord("==", std::mem_fn(&Machine::equals));
			addWord("**", std::mem_fn(&Machine::powOperation));
			addWord("mod", std::mem_fn(&Machine::modulo));
			addWord("/", std::mem_fn(&Machine::divide));
			addWord("and", std::mem_fn(&Machine::andOperation));
			addWord("<", std::mem_fn(&Machine::lessThanOperation));
			addWord(">", std::mem_fn(&Machine::greaterThanOperation));
			addWord("or", std::mem_fn(&Machine::orOperation));
			addWord("xor", std::mem_fn(&Machine::xorOperation));
			addWord(">>", std::mem_fn(&Machine::shiftRightOperation));
			addWord("<<", std::mem_fn(&Machine::shiftLeftOperation));
            addWord("if", std::mem_fn(&Machine::ifCondition), true);
            addWord("else", std::mem_fn(&Machine::elseCondition), true);
            addWord("then", std::mem_fn(&Machine::thenStatement), true);
            _nop = lookupWord("nop");
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
		}
	}
    void Machine::printStack() {
        for (const auto& element : _parameter) {
            _output << "\t- " << element << std::endl;
        }
    }
} // end namespace forth


// TODO: add support for invoking some keywords at compile time!
int main() {
	forth::Machine machine (std::cout, std::cin);
	machine.controlLoop();
	return 0;
}
