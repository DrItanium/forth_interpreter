// Currently on chapter 4.4.3
//#define DEBUG
#include <iostream>
#include <string>
#include <list>
#include <cstdint>
#include <memory>
#include <limits>
#include <sstream>
#include <cmath>
#include <set>

namespace forth {
	using Address = uint64_t;
	using Integer = int64_t;
	using Floating = double;
	using byte = uint8_t;
	static_assert((sizeof(Address) == sizeof(Integer)) && (sizeof(Integer) == sizeof(Floating)), "Address, Integer, and Floating are not equal!");
    class Problem {
        public:
            Problem(const std::string& word, const std::string& message);
            ~Problem() = default;
            const std::string& getWord() const noexcept { return _word; }
            const std::string& getMessage() const noexcept { return _message; }
        private:
            std::string _word;
            std::string _message;
    };
    Problem::Problem(const std::string& word, const std::string& message) : _word(word), _message(message) { }
    class DictionaryEntry;
	enum class Discriminant : Address {
		Number,
		MemoryAddress,
		FloatingPoint,
		Boolean,
        Word,
		Count,
	};
	union Datum {
		Datum() = default;
		Datum(Integer x) : numValue(x) { }
		Datum(Address x) : address(x) { }
		Datum(Floating x) : fp(x) { }
		Datum(bool x) : truth(x) { }
        Datum(const DictionaryEntry* x) : entry(x) { }
		~Datum() = default;
		Datum(const Datum& other);
		bool truth;
		Integer numValue;
		Address address;
		Floating fp;
        const DictionaryEntry* entry;
		byte backingStore[sizeof(Integer)];
	};

	std::ostream& operator<<(std::ostream& out, const Datum& dt) {
		// save flags
		auto flags = out.flags();
		out << "{" << dt.numValue << ", 0x" << std::hex << dt.address << ", " << std::dec << dt.fp << ", " << std::boolalpha << dt.truth;
        out << ", !" << std::hex << dt.entry;
        out << "}" ;
		out.setf(flags); // restore after done
		return out;
	}
	Datum::Datum(const Datum& other) {
		for (auto k = 0; k < sizeof(Integer); ++k) {
			// make sure that the compiler won't do something goofy when doing
			// copying
			backingStore[k] = other.backingStore[k];
		}
	}
	class Machine;
	using NativeMachineOperation = std::function<void(Machine*)>;
	class DictionaryEntry {
		public:
			struct SpaceEntry {
				enum class Discriminant {
					Signed,
					Unsigned,
					FloatingPoint,
					Boolean,
					DictEntry,
                    LoadWordIntoA,
                    LoadWordIntoB,
                    ChooseRegisterAndStoreInC,
                    InvokeRegisterC,
				};
				Discriminant _type;
				union {
					Integer _int;
					Address _addr;
					Floating _fp;
					bool _truth;
					const DictionaryEntry* _entry;
				};
				void invoke(Machine* machine) const;
                void operator()(Machine* machine) const;
			};
		public:
			DictionaryEntry() = default;
			DictionaryEntry(const std::string& name, NativeMachineOperation code = nullptr);
			~DictionaryEntry() = default;
            void markFakeEntry() noexcept { _fake = true; }
            bool isFake() const noexcept { return _fake; }
			const std::string& getName() const noexcept { return _name; }
			NativeMachineOperation getCode() const noexcept { return _code; }
			const DictionaryEntry* getNext() const noexcept { return _next; }
			bool hasNext() const noexcept { return getNext() != nullptr; }
			void setNext(DictionaryEntry* next) noexcept { _next = next; }
			void addSpaceEntry(Integer value);
			void addSpaceEntry(Address value);
			void addSpaceEntry(Floating value);
			void addSpaceEntry(bool value);
			void addSpaceEntry(const DictionaryEntry* value);
            void addLoadWordEntryIntoA(const DictionaryEntry* value);
            void addLoadWordEntryIntoB(const DictionaryEntry* value);
            void addChooseOperation() { 
                SpaceEntry tmp;
                tmp._type = SpaceEntry::Discriminant::ChooseRegisterAndStoreInC;
                tmp._entry = nullptr;
                _space.emplace_back(tmp);
            }
            void addInvokeCOperation() {
                SpaceEntry tmp;
                tmp._type = SpaceEntry::Discriminant::InvokeRegisterC;
                tmp._entry = nullptr;
                _space.emplace_back(tmp);
            }
			void operator()(Machine* machine) const {
				if (_code != nullptr) {
					_code(machine);
				} else {
#ifdef DEBUG
					std::cout << "Invoking body of " << getName() << std::endl;
#endif
                    try {
                        for (const auto & value : _space) {
                            value(machine);
                        }
                    } catch (Problem& p) {
                        throw Problem(getName(), p.getMessage());
                    }
				}
			}
		private:
			std::string _name;
			NativeMachineOperation _code;
			DictionaryEntry* _next;
			// the parameters field is the only thing that doesn't make total sense right now
			// but give it some byte storage of about 128 datums
			std::list<SpaceEntry> _space;
            bool _fake = false;
	};

    void DictionaryEntry::addLoadWordEntryIntoA(const DictionaryEntry* value) {
        SpaceEntry se;
        se._type = SpaceEntry::Discriminant::LoadWordIntoA;
        se._entry = value;
        _space.emplace_back(se);
    }

    void DictionaryEntry::addLoadWordEntryIntoB(const DictionaryEntry* value) {
        SpaceEntry se;
        se._type = SpaceEntry::Discriminant::LoadWordIntoB;
        se._entry = value;
        _space.emplace_back(se);
    }


	void DictionaryEntry::addSpaceEntry(Integer x) {
		DictionaryEntry::SpaceEntry se;
		se._type = decltype(se)::Discriminant::Signed;
		se._int = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(Address x) {
		DictionaryEntry::SpaceEntry se;
		se._type = decltype(se)::Discriminant::Unsigned;
		se._addr = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(Floating x) {
		DictionaryEntry::SpaceEntry se;
		se._type = decltype(se)::Discriminant::FloatingPoint;
		se._fp = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(bool x) {
#ifdef DEBUG
		std::cout << "Adding boolean entry: " << x << std::endl;
#endif
		DictionaryEntry::SpaceEntry se;
		se._type = decltype(se)::Discriminant::Boolean;
		se._truth = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(const DictionaryEntry* x) {
#ifdef DEBUG
		std::cout << "Adding dictionary entry: " << x << std::endl;
#endif
		DictionaryEntry::SpaceEntry se;
		se._type = decltype(se)::Discriminant::DictEntry;
		se._entry = x;
		_space.emplace_back(se);
	}


    template<typename T>
    using Stack = std::list<T>;

	DictionaryEntry::DictionaryEntry(const std::string& name, NativeMachineOperation code) : _name(name), _code(code), _next(nullptr) { }


	class Machine {
		public:
			static constexpr auto largestAddress = 0xFFFFFF;
			static constexpr auto memoryCapacity = (largestAddress + 1);
		public:
			Machine(std::ostream& output, std::istream& input);
			~Machine() = default;
			const DictionaryEntry* lookupWord(const std::string& word) noexcept {
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
			void controlLoop() noexcept;
			void handleError(const std::string& word, const std::string& msg) noexcept;
			Datum load(Address addr);
			void load() { _registerC = load(_registerA.address); }
			void store(Address addr, const Datum& value);
			void store() { store(_registerA.address, _registerB); }
            void pushWord(DictionaryEntry* entry);
			void pushParameter(Datum value);
			Datum popParameter();
			bool numberRoutine(const std::string& word) noexcept;
			void typeValue(Discriminant discriminant, const Datum& value);
			void typeValue(const Datum& value) { typeValue(_registerT, value); }
			void typeValue() { typeValue(_registerA); }
			void addWord(DictionaryEntry* entry);
			void addWord(const std::string& name, NativeMachineOperation op);
			void terminateExecution();
			void addition(Discriminant type);
			void listWords();
			void activateCompileMode() { _compiling = true; }
			void deactivateCompileMode() { _compiling = false; }
			void defineWord();
			void endDefineWord();
			void semicolonOperation();
			DictionaryEntry* getFrontWord();
			bool compileNumber(const std::string& word) noexcept;
			void setA(const Datum& target) noexcept { _registerA = target; }
			void setB(const Datum& target) noexcept { _registerB = target; }
			void setC(const Datum& target) noexcept { _registerC = target; }
			void setT(Discriminant type) noexcept { _registerT = type; }
			Datum& getA() noexcept { return _registerA; }
			Datum& getB() noexcept { return _registerB; }
			Datum& getC() noexcept { return _registerC; }
			Discriminant getT() const noexcept { return _registerT; }
			void printRegisters();
            void printStack();
		public:
			void popA() { setA(popParameter()); }
			void popB() { setB(popParameter()); }
			void popC() { setC(popParameter()); }
			void popT();
			void pushA() { pushParameter(getA()); }
			void pushB() { pushParameter(getB()); }
			void pushC() { pushParameter(getC()); }
			void pushT() { pushParameter((Address)getT()); }
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
			void absoluteValue();
			void shiftLeftOperation();
			void shiftRightOperation();
            void ifCondition();
            void elseCondition();
            void thenStatement();
            void invokeCRegister();
            void chooseRegister();
		private:
			void initializeBaseDictionary();
			std::string readWord();
		private:
			// define the CPU that the forth interpreter sits on top of
			std::ostream& _output;
			std::istream& _input;
			std::unique_ptr<Integer[]> _memory;
			DictionaryEntry* _words;
			// no need for the subroutine stack
			Stack<DictionaryEntry*> _subroutine;
			Stack<Datum> _parameter;
			bool _initializedBaseDictionary = false;
			bool _keepExecuting = true;
			bool _compiling = false;
			DictionaryEntry* _compileTarget = nullptr;
			// internal "registers"
			Datum _registerA, _registerB, _registerC;
			Discriminant _registerT;
	};
    void Machine::chooseRegister() {
        _registerC = _registerC.truth ? _registerA : _registerB;
    }
    void Machine::invokeCRegister() {
        using Type = decltype(_registerT);
        if (_registerT != Type::Word) {
            throw Problem("invoke.c", "incorrect discriminant!");
        } else {
            // THIS IS SUPER DUPER UNSAFE AND BAD THINGS CAN HAPPEN!!!!
            // TODO: figure out a safer way to solve this issue
            _registerC.entry->operator()(this);
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
            _compileTarget->addSpaceEntry(lookupWord("pop.c"));
            // compile the address of the entry we're looking for into 
            _compileTarget->addLoadWordEntryIntoA(entry);
            // compile in a fake address to the nop command
            _compileTarget->addLoadWordEntryIntoB(lookupWord("nop"));
        } else {
            throw Problem(word, "?");
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
            throw Problem(word, "?");
        }
    }

    void Machine::thenStatement() {
        if (!_compiling) {
            throw Problem("then", "must be defining a word!");
        }
        _compileTarget->addChooseOperation();
        _compileTarget->addInvokeCOperation();
        if (_subroutine.empty()) {
            throw Problem("then", "Not in a function");
        }
        addWord(_compileTarget);
        auto parent = _subroutine.back();
        parent->addSpaceEntry(_compileTarget);
        _compileTarget = parent;
    }
	std::string Machine::readWord() {
		std::string word;
		_input >> word;
		return word;
	}
	void Machine::printRegisters() {
		auto fn = [this](const std::string& title, const Datum& r) {
			_output << title << ": " << r << std::endl;
		};
		fn("A", _registerA);
		fn("B", _registerB);
		fn("C", _registerC);
		_output << "T: 0x" << std::hex << (Address)_registerT << std::dec << std::endl;
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
#ifdef DEBUG
				std::cout << "pushing integer " << std::dec << _int << " onto stack!" << std::endl;
#endif
				machine->pushParameter(_int);
				break;
			case Type::Unsigned:
#ifdef DEBUG
				std::cout << "pushing address " << std::hex << _addr  << std::dec << " onto stack!" << std::endl;
#endif
				machine->pushParameter(_addr);
				break;
			case Type::FloatingPoint:
#ifdef DEBUG
				std::cout << "pushing fp " << _fp << " onto stack!" << std::endl;
#endif
				machine->pushParameter(_fp);
				break;
			case Type::Boolean:
#ifdef DEBUG
				std::cout << "pushing boolean " << _truth << " onto stack!" << std::endl;
#endif
				machine->pushParameter(_truth);
				break;
			case Type::DictEntry:
#ifdef DEBUG
				std::cout << "calling dictionary entry: '" << _entry->getName() << "' at " << _entry << std::dec << std::endl;
#endif
				_entry->operator()(machine);
				break;
            case Type::LoadWordIntoA:
                machine->setA(_entry);
                break;
            case Type::LoadWordIntoB:
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
	void Machine::addWord(const std::string& name, NativeMachineOperation op) {
		addWord(new DictionaryEntry(name, op));
	}
	void Machine::terminateExecution() {
		_keepExecuting = false;
	}
	NativeMachineOperation binaryOperation(std::function<Datum(const Datum&, const Datum&)> fn) noexcept {
		return [fn](Machine* machine) {
			auto top(machine->popParameter());
			auto lower(machine->popParameter());
#ifdef DEBUG
			std::cout << "top: " << top.numValue << std::endl;
			std::cout << "lower: " << lower.numValue << std::endl;
#endif
			machine->pushParameter(fn(top, lower));
		};
	}
	NativeMachineOperation unaryOperation(std::function<Datum(const Datum&)> fn) noexcept {
		return [fn](Machine* machine) {
			auto top(machine->popParameter());
			machine->pushParameter(fn(top));
		};
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
				_registerC.numValue = Integer(std::pow(_registerA.numValue, _registerB.numValue));
				break;
			case Type::MemoryAddress:
				_registerC.address = Address(std::pow(_registerA.address, _registerB.address));
				break;
			case Type::FloatingPoint:
				_registerC.fp = Floating(std::pow(_registerA.fp, _registerB.fp));
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

	void Machine::absoluteValue() {
		using Type = decltype(_registerT);
		switch (_registerT) {
			case Type::Number:
				_registerC = _registerA.numValue < 0 ? -_registerA.numValue : _registerA.numValue;
				break;
			case Type::MemoryAddress:
				_registerC = _registerA;
				break;
			case Type::FloatingPoint:
				_registerC = _registerA.fp < 0 ? -_registerA.fp : _registerA.fp;
				break;
			default:
                throw Problem("abs", "ILLEGAL DISCRIMINANT!");
		}
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

	void Machine::popT() {
		static constexpr Address max = (Address)Discriminant::Count;
		auto top(popParameter());
		if (top.address >= max) {
            throw Problem("pop.t", "ILLEGAL DISCRIMINANT!");
		}
		setT((Discriminant)top.address);
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
	bool Machine::numberRoutine(const std::string& word) noexcept {
		// floating point
		// integers
		// first do some inspection first
		if (word == "true") {
			pushParameter(true);
			return true;
		}
		if (word == "false") {
			pushParameter(false);
			return true;
		}
		std::istringstream parseAttempt(word);
		if (word.find('u') != std::string::npos) {
			Address tmpAddress;
			parseAttempt >> tmpAddress;
			if (!parseAttempt.fail() && parseAttempt.eof()) {
				pushParameter(tmpAddress);
				return true;
			}
			return false;
		}
		parseAttempt.clear();
		if (word.find('.') != std::string::npos) {
			Floating tmpFloat;
			parseAttempt >> tmpFloat;
			if (!parseAttempt.fail()) {
#ifdef DEBUG
				_output << "attempt floating point number push: " << tmpFloat << std::endl;
#endif // end DEBUG
				if (parseAttempt.eof()) {
					pushParameter(tmpFloat);
					return true;
				} else {
					// get out of here early since we hit something that looks like
					// a float
					return false;
				}
			}
		}
		Integer tmpInt;
		parseAttempt.clear();
		parseAttempt >> tmpInt;
		if (!parseAttempt.fail()) {
#ifdef DEBUG
			_output << "attempt integer number push: " << tmpInt << std::endl;
#endif // end DEBUG
			if (parseAttempt.eof()) {
				// if we hit the end of the word provided then it is an integer, otherwise it is not!
				pushParameter(tmpInt);
				return true;
			}
		}
		return false;
	}

	bool Machine::compileNumber(const std::string& word) noexcept {
		// floating point
		// integers
		// first do some inspection first
		if (word == "true") {
			_compileTarget->addSpaceEntry(true);
			return true;
		}
		if (word == "false") {
			_compileTarget->addSpaceEntry(false);
			return true;
		}
		std::istringstream parseAttempt(word);
		if (word.find('u') != std::string::npos) {
			Address tmpAddress;
			parseAttempt >> tmpAddress;
			if (!parseAttempt.fail() && parseAttempt.eof()) {
				_compileTarget->addSpaceEntry(tmpAddress);
				return true;
			}
			return false;
		}
		parseAttempt.clear();
		if (word.find('.') != std::string::npos) {
			Floating tmpFloat;
			parseAttempt >> tmpFloat;
			if (!parseAttempt.fail() && parseAttempt.eof()) {
				_compileTarget->addSpaceEntry(tmpFloat);
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
			// if we hit the end of the word provided then it is an integer, otherwise it is not!
			_compileTarget->addSpaceEntry(tmpInt);
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
	}
	void Machine::controlLoop() noexcept {
		// setup initial dictionary
		initializeBaseDictionary();
		while (_keepExecuting) {
            try {
                auto result = readWord();
                //bool dontExecuteWord = !result.empty() && (result.front() == "~");
                // okay, we need to see if we can find the given word, first strip off the ~ 
                //if (dontExecuteWord) {
                //    result = result.substr(1);
                //}
                auto* entry = lookupWord(result);
                if (_compiling) {
                    auto finishedCompiling = (result == ";");
                    if (entry != nullptr) {
                        // okay we have a word, lets add it to the top word in the dictionary
                        // get the front word first and foremost
#ifdef DEBUG
                        _output << "Found an entry for: " << result << std::endl;
                        _output << "Location: " << std::hex << entry << std::dec << std::endl;
#endif
                        //if (dontExecuteWord) {
                        //    // add a new spaceEntry to push this value onto the data stack
                        //} else {
                        _compileTarget->addSpaceEntry(entry);
                        if (finishedCompiling) {
                            endDefineWord();
                        }
                        //}
                        continue;
                    }
                    // okay, we need to see if it is a value to compile in
                    if (compileNumber(result)) {
                        continue;
                    }
                } else {
                    if (entry != nullptr) {
                        entry->operator()(this);
                        continue;
                    }
                    if (numberRoutine(result)) {
                        continue;
                    }
                    // fall through case, we couldn't figure it out!
                }
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
			addWord("type.a", std::mem_fn<void()>(&Machine::typeValue));
			addWord("pop.t", std::mem_fn(&Machine::popT));
			addWord("pop.a", std::mem_fn(&Machine::popA));
			addWord("pop.b", std::mem_fn(&Machine::popB));
			addWord("pop.c", std::mem_fn(&Machine::popC));
			addWord("push.a", std::mem_fn(&Machine::pushA));
			addWord("push.b", std::mem_fn(&Machine::pushB));
			addWord("push.c", std::mem_fn(&Machine::pushC));
			addWord("push.t", std::mem_fn(&Machine::pushT));
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
			addWord("abs", std::mem_fn(&Machine::absoluteValue));
			addWord(">>", std::mem_fn(&Machine::shiftRightOperation));
			addWord("<<", std::mem_fn(&Machine::shiftLeftOperation));
		}
	}
    void Machine::printStack() {
        for (const auto& element : _parameter) {
            _output << "\t- " << element << std::endl;
        }
    }
} // end namespace forth


//TODO: add support for catching exceptions and calling handleError with the exception information

int main() {
	forth::Machine machine (std::cout, std::cin);
	machine.controlLoop();
	return 0;
}
