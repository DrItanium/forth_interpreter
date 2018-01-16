// Currently on chapter 4.4.3
//#define DEBUG
#include <iostream>
#include <string>
#include <stack>
#include <list>
#include <cstdint>
#include <memory>
#include <limits>
#include <sstream>
#include <cmath>

namespace forth {
    using Address = uint32_t;
    using Integer = int32_t;
    using Floating = float;
    using byte = uint8_t;
    static_assert((sizeof(Address) == sizeof(Integer)) && (sizeof(Integer) == sizeof(Floating)), "Address, Integer, and Floating are not equal!");
    enum class Discriminant : Address {
        Number,
        MemoryAddress,
        FloatingPoint,
        Boolean,
        Count,
    };
    union Datum {
        Datum() = default;
        Datum(Integer x) : numValue(x) { }
        Datum(Address x) : address(x) { }
        Datum(Floating x) : fp(x) { }
        Datum(bool x) : truth() { }
        ~Datum() = default;
        Datum(const Datum& other);
        bool truth;
        Integer numValue;
        Address address;
        Floating fp;
        byte backingStore[sizeof(Integer)];
    };

    Datum::Datum(const Datum& other) {
        for (auto k = 0; k < sizeof(Integer); ++k) {
            // make sure that the compiler won't do something goofy when doing
            // copying
            backingStore[k] = other.backingStore[k];
        }
    }
    class Machine;
    using NativeMachineOperation = std::function<void(Machine&)>;
    class DictionaryEntry {
        public:
            struct SpaceEntry {
                enum class Discriminant {
                    Signed,
                    Unsigned,
                    FloatingPoint,
                    Boolean,
                    DictEntry,
                };
                Discriminant _type;
                union {
                    Integer _int;
                    Address _addr;
                    Floating _fp;
                    bool _truth;
                    const DictionaryEntry* _entry;
                };
                void invoke(Machine& machine) const;
            };
        public:
            DictionaryEntry() = default;
            DictionaryEntry(const std::string& name, NativeMachineOperation code = nullptr);
            ~DictionaryEntry() = default;
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
            void operator()(Machine& machine) const {
                if (_code != nullptr) {
                    _code(machine);
                } else {
#ifdef DEBUG
                    std::cout << "Invoking body of " << getName() << std::endl;
#endif
                    for (const auto & value : _space) {
                        value.invoke(machine);
                    }
                    // iterate through the set of space entries
                }
            }
        private:
            std::string _name;
            NativeMachineOperation _code;
            DictionaryEntry* _next;
            // the parameters field is the only thing that doesn't make total sense right now
            // but give it some byte storage of about 128 datums
            std::list<SpaceEntry> _space;
    };

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


    template<typename T = int>
    using Stack = std::stack<T, std::list<T>>;

    DictionaryEntry::DictionaryEntry(const std::string& name, NativeMachineOperation code) : _name(name), _code(code), _next(nullptr)
    {
    }

    std::string readWord(std::istream& input) {
        std::string word;
        input >> word;
        return word;
    }

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
                    if (entry->getName() == word) {
                        return entry;
                    }
                }
                return nullptr;
            }
            void controlLoop() noexcept;
            void handleError(const std::string& word, const std::string& msg) noexcept;
            void dropParameter();
            void swapParameters();
            void duplicateParameter();
            void placeOverParameter();
            const Datum& topParameter();
            const Datum& lowerParameter();
            Datum load(Address addr);
            void store(Address addr, Datum value);
            void store(Address addr, Integer value);
            void store(Address addr, Address value);
            void store(Address addr, Floating fp);
            void pushParameter(Datum value);
            void pushParameter(Integer value);
            void pushParameter(Floating value);
            void pushParameter(Address value);
            Datum popParameter();
            bool numberRoutine(const std::string& word) noexcept;
            void typeValue(Discriminant discriminant, const Datum& value);
            void callSubroutine();
            void returnFromSubroutine();
            void addWord(DictionaryEntry* entry);
            void addWord(const std::string& name, NativeMachineOperation op);
            void terminateExecution();
            void addition(Discriminant type);
            void listWords();
            void activateCompileMode() { _compiling = true; }
            void deactivateCompileMode() { _compiling = false; }
            void defineWord();
            void endDefineWord();
            DictionaryEntry* getFrontWord();
            bool compileNumber(const std::string& word) noexcept;
            void setA(const Datum& target) noexcept { _registerA = target; }
            Datum& getA() noexcept { return _registerA; }
            void setB(const Datum& target) noexcept { _registerB = target; }
            Datum& getB() noexcept { return _registerB; }
            void setC(const Datum& target) noexcept { _registerC = target; }
            Datum& getC() noexcept { return _registerC; }
            void setT(Discriminant type) noexcept { _registerT = type; }
            Discriminant getT() const noexcept { return _registerT; }
            void add();
            void printRegisters();
            //void subtract();
            //void multiply();
            //void divide();
            //void modulus();
            //void greaterThan();
            //void lessThan();
            //void equal();
        private:
            void initializeBaseDictionary();
        private:
            // define the CPU that the forth interpreter sits on top of
            std::ostream& _output;
            std::istream& _input;
            std::unique_ptr<Integer[]> _memory;
            DictionaryEntry* _words;
            Stack<Address> _subroutine;
            Stack<Datum> _parameter;
            bool _initializedBaseDictionary = false;
            bool _keepExecuting = true;
            bool _compiling = false;
            DictionaryEntry* _compileTarget = nullptr;
            // internal "registers"
            Datum _registerA, _registerB, _registerC;
            Discriminant _registerT;
    };
    void Machine::printRegisters() {
        auto fn = [this](const std::string& title, const Datum& r) {
            _output << title << ": {" << r.numValue << ", 0x" << std::hex << r.address << ", " << std::dec << r.fp << "}" << std::endl;
        };
        fn("A", _registerA);
        fn("B", _registerB);
        fn("C", _registerC);
        _output << "T: 0x" << std::hex << (Address)_registerT << std::dec << std::endl;
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
                _registerC = _registerA.fp + _registerB.address;
                break;
            default:
                throw "ILLEGAL DISCRIMINANT!";
        }
    }
    void Machine::defineWord() {
        activateCompileMode();
        // pass address "execute" to the entry subroutine
        _compileTarget = new DictionaryEntry(readWord(_input));
    }
    void Machine::endDefineWord() {
        deactivateCompileMode();
        addWord(_compileTarget);
        _compileTarget = nullptr;
    }
    void DictionaryEntry::SpaceEntry::invoke(Machine& machine) const {
        switch (_type) {
            case DictionaryEntry::SpaceEntry::Discriminant::Signed:
#ifdef DEBUG
                std::cout << "pushing integer " << std::dec << _int << " onto stack!" << std::endl;
#endif
                machine.pushParameter(_int);
                break;
            case DictionaryEntry::SpaceEntry::Discriminant::Unsigned:
#ifdef DEBUG
                std::cout << "pushing address " << std::hex << _addr  << std::dec << " onto stack!" << std::endl;
#endif
                machine.pushParameter(_addr);
                break;
            case DictionaryEntry::SpaceEntry::Discriminant::FloatingPoint:
#ifdef DEBUG
                std::cout << "pushing fp " << _fp << " onto stack!" << std::endl;
#endif
                machine.pushParameter(_fp);
                break;
            case DictionaryEntry::SpaceEntry::Discriminant::Boolean:
#ifdef DEBUG
                std::cout << "pushing boolean " << _truth << " onto stack!" << std::endl;
#endif
                machine.pushParameter(_truth);
                break;
            case DictionaryEntry::SpaceEntry::Discriminant::DictEntry:
#ifdef DEBUG
                std::cout << "calling dictionary entry: '" << _entry->getName() << "' at " << _entry << std::dec << std::endl;
#endif
                _entry->operator()(machine);
                break;
            default:
                throw "UNKNOWN ENTRY KIND!";
        }
    }
    void Machine::listWords() {
        if (_words == nullptr) {
            return;
        }
        for (const auto* entry = _words; entry != nullptr; entry = entry->getNext()) {
            _output << entry->getName() << std::endl;
        }
    }
    void Machine::addWord(const std::string& name, NativeMachineOperation op) {
        addWord(new DictionaryEntry(name, op));
    }
    void Machine::terminateExecution() {
        _keepExecuting = false;
    }
    NativeMachineOperation binaryOperation(std::function<Datum(const Datum&, const Datum&)> fn) noexcept {
        return [fn](Machine& machine) {
            auto top(machine.popParameter());
            auto lower(machine.popParameter());
#ifdef DEBUG
            std::cout << "top: " << top.numValue << std::endl;
            std::cout << "lower: " << lower.numValue << std::endl;
#endif
            machine.pushParameter(fn(top, lower));
        };
    }
    NativeMachineOperation unaryOperation(std::function<Datum(const Datum&)> fn) noexcept {
        return [fn](Machine& machine) {
            auto top(machine.popParameter());
            machine.pushParameter(fn(top));
        };
    }

    void Machine::initializeBaseDictionary() {
        if (!_initializedBaseDictionary) {
            _initializedBaseDictionary = true;
            // add dictionary entries
            addWord("quit", [](Machine& machine) { machine.terminateExecution(); });
            addWord("@", [](Machine& machine) {
                        // load the value at the given address onto the stack
                        auto top(machine.popParameter());
                        machine.pushParameter(machine.load(top.address));
                    });
            addWord("=", [](Machine& machine) {
                         // store into memory at the top address the lower value
                         auto addr(machine.popParameter());
                         auto value(machine.popParameter());
                         machine.store(addr.address, value);
                    });
            addWord("drop", [](Machine& machine) { machine.dropParameter(); });
            addWord("dup", [](Machine& machine) { machine.duplicateParameter(); });
            addWord("over", [](Machine& machine) { machine.placeOverParameter(); });
            addWord("swap", [](Machine& machine) { machine.swapParameters(); });
            addWord("minus", unaryOperation([](auto top) { return -top.numValue; }));
            addWord("abs", unaryOperation([](auto top) { return top.numValue < 0 ? -top.numValue : top.numValue; }));
            addWord(",", [](Machine& machine) {
                            auto top(machine.popParameter());
                            machine.typeValue(Discriminant::Number, top);
                        });
            addWord("zero", unaryOperation([](auto top) { return top.numValue == 0; }));
            addWord("nonzero", unaryOperation([](auto top) { return top.numValue != 0; }));
            addWord("+", binaryOperation([](auto top, auto lower) { return top.numValue + lower.numValue; }));
            addWord("*", binaryOperation([](auto top, auto lower) { return top.numValue * lower.numValue; }));
            addWord("-", binaryOperation([](auto top, auto lower) { return lower.numValue - top.numValue; }));
            addWord("/", binaryOperation([](auto top, auto lower) { return lower.numValue / top.numValue; }));
            addWord("mod", binaryOperation([](auto top, auto lower) { return lower.numValue % top.numValue; }));
            addWord("==", binaryOperation([](auto top, auto lower) { return lower.numValue == top.numValue; }));
            addWord("<", binaryOperation([](auto top, auto lower) { return lower.numValue < top.numValue; }));
            addWord(">", binaryOperation([](auto top, auto lower) { return lower.numValue > top.numValue; }));
            addWord(">=", binaryOperation([](auto top, auto lower) { return lower.numValue >= top.numValue; }));
            addWord("<=", binaryOperation([](auto top, auto lower) { return lower.numValue <= top.numValue; }));
            addWord("+f", binaryOperation([](auto top, auto lower) { return top.fp + lower.fp; }));
            addWord("*f", binaryOperation([](auto top, auto lower) { return top.fp * lower.fp; }));
            addWord("-f", binaryOperation([](auto top, auto lower) { return lower.fp - top.fp; }));
            addWord("/f", binaryOperation([](auto top, auto lower) { return lower.fp / top.fp; }));
            addWord("==f", binaryOperation([](auto top, auto lower) { return lower.fp == top.fp; }));
            addWord("<f", binaryOperation([](auto top, auto lower) { return lower.fp < top.fp; }));
            addWord(">f", binaryOperation([](auto top, auto lower) { return lower.fp > top.fp; }));
            addWord(">=f", binaryOperation([](auto top, auto lower) { return lower.fp >= top.fp; }));
            addWord("<=f", binaryOperation([](auto top, auto lower) { return lower.fp <= top.fp; }));
            addWord("+u", binaryOperation([](auto top, auto lower) { return top.address + lower.address; }));
            addWord("*u", binaryOperation([](auto top, auto lower) { return top.address * lower.address; }));
            addWord("-u", binaryOperation([](auto top, auto lower) { return lower.address - top.address; }));
            addWord("/u", binaryOperation([](auto top, auto lower) { return lower.address / top.address; }));
            addWord("modu", binaryOperation([](auto top, auto lower) { return lower.address % top.address; }));
            addWord("==u", binaryOperation([](auto top, auto lower) { return lower.address == top.address; }));
            addWord("<u", binaryOperation([](auto top, auto lower) { return lower.address < top.address; }));
            addWord(">u", binaryOperation([](auto top, auto lower) { return lower.address > top.address; }));
            addWord(">=u", binaryOperation([](auto top, auto lower) { return lower.address >= top.address; }));
            addWord("<=u", binaryOperation([](auto top, auto lower) { return lower.address <= top.address; }));
            addWord("not", unaryOperation([](auto top) { return ~top.address; }));
            addWord("lnot", unaryOperation([](auto top) { return top.truth ? 0 : 1; }));
            addWord("and", binaryOperation([](auto top, auto lower) { return top.address & lower.address; }));
            addWord("or", binaryOperation([](auto top, auto lower) { return top.address | lower.address; }));
            addWord("xor", binaryOperation([](auto top, auto lower) { return top.address ^ lower.address; }));
            addWord("land", binaryOperation([](auto top, auto lower) { return top.truth && lower.truth; }));
            addWord("lor", binaryOperation([](auto top, auto lower) { return top.truth || lower.truth; }));
            addWord("lxor", binaryOperation([](auto top, auto lower) { return top.truth ^ lower.truth; }));
            addWord("implies", binaryOperation([](auto top, auto lower) { return (!top.truth) || lower.truth; }));
            addWord("**", binaryOperation([](auto top, auto lower) { return static_cast<Integer>(std::pow(lower.numValue, top.numValue)); }));
            addWord("**f", binaryOperation([](auto top, auto lower) { return static_cast<Floating>(std::pow(lower.fp, top.fp)); }));
            addWord("**u", binaryOperation([](auto top, auto lower) { return static_cast<Address>(std::pow(lower.address, top.address)); }));
            addWord("words", [](Machine& machine) { machine.listWords(); });
            addWord(":", [](Machine& machine) { machine.defineWord(); });
            addWord(";", [](Machine& machine) {
                        // in assembly level impls, this resets the instruction
                        // counter to zero, however, with how we use iterators,
                        // this isn't necessary!
                    });
            addWord("pop.t", [](Machine& machine) { 
                    static constexpr Address max = (Address)Discriminant::Count;
                    auto top(machine.popParameter());
                    if (top.address >= max) {
                        throw "ILLEGAL DISCRIMINANT!";
                    }
                    machine.setT((Discriminant)top.address);
                    });
            addWord("pop.a", [](Machine& machine) { machine.setA(machine.popParameter()); });
            addWord("pop.b", [](Machine& machine) { machine.setB(machine.popParameter()); });
            addWord("pop.c", [](Machine& machine) { machine.setC(machine.popParameter()); });
            addWord("push.a", [](Machine& machine) { machine.pushParameter(machine.getA()); });
            addWord("push.b", [](Machine& machine) { machine.pushParameter(machine.getB()); });
            addWord("push.c", [](Machine& machine) { machine.pushParameter(machine.getC()); });
            addWord("push.t", [](Machine& machine) { machine.pushParameter((Address)machine.getT()); });
            addWord("registers", [](Machine& machine) { machine.printRegisters(); });
            addWord("add", [](Machine& machine) { machine.add(); });
        }
    }
    void Machine::addWord(DictionaryEntry* entry) {
        if (_words != nullptr) {
            entry->setNext(_words);
        }
        _words = entry;
    }
    const Datum& Machine::topParameter() {
        if (_parameter.empty()) {
            throw "STACK EMPTY!";
        } else {
            return _parameter.top();
        }
    }
    const Datum& Machine::lowerParameter() {
        auto top = popParameter();
        if (_parameter.empty()) {
            _parameter.push(top);
            throw "STACK EMPTY";
        } else {
            auto& lower = _parameter.top();
            _parameter.push(top);
            return lower;
        }
    }
    void Machine::placeOverParameter() {
        // a b -- a b a
        auto top = popParameter();
        Datum lower(_parameter.top());
        _parameter.push(top);
        _parameter.push(lower);
    }
    void Machine::duplicateParameter() {
        // a -- a a
        Datum top(_parameter.top());
        _parameter.push(top);
    }
    void Machine::swapParameters() {
        // a b -- b a
        auto top = popParameter();
        auto lower = popParameter();
        _parameter.push(top);
        _parameter.push(lower);
    }
    void Machine::dropParameter() {
        // a --
        if (_parameter.empty()) {
            throw "STACK EMPTY!";
        } else {
            _parameter.pop();
        }
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
            default:
                throw "BAD DISCRIMINANT";
        }
        // always type a space out after the number
        _output << ' ' << std::endl;
    }
    Machine::Machine(std::ostream& output, std::istream& input) :  _output(output), _input(input), _memory(new Integer[memoryCapacity]), _words(nullptr) { }

    Datum Machine::popParameter() {
        if (_parameter.empty()) {
            throw "STACK EMPTY!";
        }
        auto top(_parameter.top());
        _parameter.pop();
        return top;
    }

    void Machine::pushParameter(Datum value) {
        _parameter.push(value);
    }
    void Machine::pushParameter(Integer value) {
        Datum tmp;
        tmp.numValue = value;
        pushParameter(tmp);
    }

    void Machine::pushParameter(Address value) {
        Datum tmp;
        tmp.address = value;
        pushParameter(tmp);
    }

    void Machine::pushParameter(Floating fp) {
        Datum tmp;
        tmp.fp = fp;
        pushParameter(tmp);
    }

    Datum Machine::load(Address addr) {
        if (addr > largestAddress) {
            throw "BAD ADDRESS";
        } else {
            Datum storage;
            storage.numValue = _memory[addr];
            return storage;
        }
    }
    void Machine::store(Address addr, Integer value) {
        if (addr > largestAddress) {
            throw "BAD ADDRESS";
        } else {
            _memory[addr] = value;
        }
    }
    void Machine::store(Address addr, Datum value) {
        store(addr, value.numValue);
    }
    void Machine::store(Address addr, Address value) {
        Datum tmp;
        tmp.address = value;
        store(addr, tmp);
    }
    void Machine::store(Address addr, Floating value) {
        Datum tmp;
        tmp.fp = value;
        store(addr, tmp);
    }
    bool Machine::numberRoutine(const std::string& word) noexcept {
        // floating point
        // integers
        // first do some inspection first
        if (word == "true") {
            pushParameter(1);
            return true;
        }
        if (word == "false") {
            pushParameter(0);
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
        decltype(_subroutine) _purge0;
        decltype(_parameter) _purge1;
        _subroutine.swap(_purge0);
        _parameter.swap(_purge1);
        _input.clear();
        _input.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        _output << word << msg << std::endl;
    }
    void Machine::controlLoop() noexcept {
        // setup initial dictionary
        initializeBaseDictionary();
        while (_keepExecuting) {
            auto result = readWord(_input);
            // okay, we need to see if we can find the given word!
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
                    _compileTarget->addSpaceEntry(entry);
                    if (finishedCompiling) {
                        endDefineWord();
                    }
                    continue;
                }
                // okay, we need to see if it is a value to compile in
                if (compileNumber(result)) {
                    continue;
                }
            } else {
                if (entry != nullptr) {
                    entry->operator()(*this);
                    continue;
                }
                if (numberRoutine(result)) {
                    continue;
                }
                // fall through case, we couldn't figure it out!
            }
            handleError(result, "?");
        }
    }
} // end namespace forth


//TODO: add support for catching exceptions and calling handleError with the exception information

int main() {
    forth::Machine machine (std::cout, std::cin);
    machine.controlLoop();
    return 0;
}
