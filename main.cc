#include "Types.h"
#include "Problem.h"
#include <functional>
#include <string>
#include <list>
#include <optional>
#include <variant>
#include <memory>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>
#include <cmath>

#define YTypeAddress forth::Address
#define YTypebool bool
#define YTypeInteger forth::Integer
#define YTypeStringAddress ".u" 
#define YTypeStringbool ".b"
#define YTypeStringInteger ".s"

class DictionaryEntry;
class Machine;

using Integer = forth::Integer;
using Address = forth::Address;
using byte = forth::byte;

template<typename T>
using GenericStack = std::list<T>;
using Problem = forth::Problem;
using Word = std::shared_ptr<DictionaryEntry>;
using OptionalWord = std::optional<Word>;
using NativeFunction = std::function<void(Machine&)>;
void toUpper(std::string& str) {
    std::transform(str.begin(), str.end(), str.begin(), ::toupper);
}
bool equalsIgnoreCase(const std::string& a, const std::string& b) {
    auto ca = a;
    auto cb = b;
    toUpper(ca);
    toUpper(cb);
    return ca == cb;
}
union Number {
	Number() : integer(0) { }
	Number(int i) : integer(Integer(i)) { }
	Number(unsigned int i) : address(Address(i)) { }
	Number(Integer i) : integer(i) { }
	Number(Address a) : address(a) { }

	template<typename T>
	T get() noexcept {
		using K = std::decay_t<T>;
		if constexpr (std::is_same_v<K, Integer>) {
			return integer;
		} else if constexpr (std::is_same_v<K, Address>) {
			return address;
		} else if constexpr (std::is_same_v<K, bool>) {
			return getTruth();
		} else {
			static_assert(forth::AlwaysFalse<T>::value, "Unsupported type specified!");
		}
	}
    bool getTruth() const noexcept { 
        return address != 0;
    }
	Integer integer;
	Address address;
	byte bytes[sizeof(Address)];
};
struct Variable;
using SharedVariable = std::shared_ptr<Variable>;
struct Variable {
    using Contents = std::variant<Number, std::string, SharedVariable>;
    Contents _value;
};

using Datum = std::variant<Number, Word, NativeFunction, std::string, SharedVariable>;

using UnaryOperation = std::function<Datum(Machine&, Datum)>;
using BinaryOperation = std::function<Datum(Machine&, Datum, Datum)>;
using Stack = GenericStack<Datum>;
using InputStack = GenericStack<std::unique_ptr<std::ifstream>>;
using OutputStack = GenericStack<std::unique_ptr<std::ofstream>>;
constexpr Address defaultMemorySize = 4096;
template<typename T>
T expect(const std::string& function, const Datum& d) {
    try {
        return std::get<T>(d);
    } catch (std::bad_variant_access&) {
        using K = std::decay_t<T>;
        std::stringstream ss;
        ss << " expected the top of the stack to be a ";
        if constexpr (std::is_same_v<std::string, T>) {
            ss << "string";
        } else if constexpr (std::is_same_v<Number, T>) {
            ss << "number";
        } else if constexpr (std::is_same_v<Word, T>) {
            ss << "word";
        } else if constexpr (std::is_same_v<SharedVariable, T>) {
            ss << "variable";
        } else if constexpr (std::is_same_v<NativeFunction, T>) {
            ss << "native function";
        } else {
            static_assert(forth::AlwaysFalse<T>::value, "Unimplemented type!");
        }
        auto str = ss.str();
        throw Problem(function, str);
    }
}

bool printOk = false;
Address executionDepth = 0;
class Machine {
    public:
        explicit Machine(Address memSize = defaultMemorySize);
        ~Machine();
		void pushParameter(Number n);
        void pushParameter(Word ent);
        void pushParameter(NativeFunction fn);
        void pushParameter(const std::string&);
        void pushParameter(Datum d);
        void pushParameter(SharedVariable v);
		void pushSubroutine(Number n);
        void pushSubroutine(Word ent);
        void pushSubroutine(NativeFunction fn);
        void pushSubroutine(const std::string&);
        void pushSubroutine(Datum d);
        void pushSubroutine(SharedVariable v);
        Datum popSubroutine();
        Datum popParameter();
        OptionalWord lookupWord(const std::string&);
        OptionalWord getCurrentlyCompilingWord() const noexcept { return _compile; }
        void saveCurrentlyCompilingWord();
        void restoreCurrentlyCompilingWord();
        void newCompilingWord(const std::string& str = "");
        void compileCurrentWord();
        bool currentlyCompiling() const noexcept { return (bool)_compile; }
        std::string readNext();
        void errorOccurred() noexcept;
        void addWord(const std::string& name, NativeFunction fn, bool fake = false, bool compileTimeInvoke = false);
        std::ostream& getOutput() const noexcept;
        std::istream& getInput() const noexcept;
        void openFileForOutput(const std::string& path);
        void openFileForInput(const std::string& path);
        void closeFileForOutput();
        void closeFileForInput();
        void store(Address addr, byte value);
        byte load(Address addr);
        Address getMaximumAddress() const noexcept { return _capacity - 1; }
        Address getCapacity() const noexcept { return _capacity; }
        void viewParameterStack();
        void viewSubroutineStack();
		OptionalWord getFront() const noexcept { return _front; }
        void resizeMemory(Address newCapacity);
        void dumpMemoryToFile(const std::string& path);
        void openBinaryFile(const std::string& path);
        void closeBinaryFile();
        void writeBinaryFile(byte v);
        bool parameterStackEmpty() const noexcept { return _parameter.empty(); }
        void pushParameterDepth() noexcept;
        void clearCurrentWord();
        void setDebugging(bool value) noexcept { _enableDebugging = value; }
        bool debugActive() const noexcept { return _enableDebugging; }
    private:
        std::unique_ptr<std::ifstream> _in;
        std::unique_ptr<std::ofstream> _out;
        std::unique_ptr<std::ofstream> _binaryFile;
        OptionalWord _front;
        OptionalWord _compile;
        Stack _parameter;
        Stack _subroutine;
        InputStack _inputs;
        OutputStack _outputs;
        std::unique_ptr<forth::byte[]> _memory;
        Address _capacity;
        bool _enableDebugging = false;
};
void Machine::pushParameterDepth() noexcept {
    auto len = _parameter.size();
    pushParameter(Number(len));
}
void Machine::openBinaryFile(const std::string& path) {
    if (_binaryFile) {
        throw Problem("openBinaryFile", " Already have an open binary file!");
    }
    _binaryFile = std::make_unique<std::ofstream>();
    _binaryFile->open(path.c_str(), std::ofstream::binary | std::ofstream::out );
    if (!_binaryFile->is_open()) {
        throw Problem("openBinaryFile", " Unable to open file for writing!");
    } 
}
void Machine::writeBinaryFile(byte b) {
    if (!_binaryFile) {
        throw Problem("closeBinaryFile", " Cannot write to a non existent binary file!");
    }
    union {
        byte b;
        char c;
    } value;
    value.b = b;
    _binaryFile->put(value.c);
}
void Machine::closeBinaryFile() {
    if (!_binaryFile) {
        throw Problem("closeBinaryFile", " Cannot close a binary file that is not open!");
    }
    _binaryFile->close();
    _binaryFile.reset();
}
void Machine::dumpMemoryToFile(const std::string& path) {
    std::ofstream stream(path.c_str(), std::fstream::binary);
    if (!stream.is_open()) {
        throw Problem("dumpToFile", " couldn't open file for writing!");
    }
    for (auto a = 0; a < _capacity; ++a) {
        stream << char(_memory[a]);
    }
    stream.close();
}
std::ostream& Machine::getOutput() const noexcept {
    if (_out) {
        return *(_out.get());
    } else {
        return std::cout;
    }
}
std::istream& Machine::getInput() const noexcept {
    if (_in) {
        return *(_in.get());
    } else {
        return std::cin;
    }
}
void Machine::resizeMemory(Address newCapacity) {
    _memory = std::make_unique<forth::byte[]>(newCapacity);
    _capacity = newCapacity;
}
std::string Machine::readNext() {
    std::string word;
    getInput() >> word;
    return word;
}
void Machine::openFileForOutput(const std::string& path) {
    auto value = std::make_unique<std::ofstream>(path.c_str());
    if (!value->is_open()) {
        std::stringstream ss;
        ss << "Could not open " << path << " for writing!";
        auto str = ss.str();
        throw Problem("openFileForOutput", str);
    }
    if (_out) {
        _outputs.emplace_front(std::move(_out));
    }
    _out.swap(value);
}

void Machine::openFileForInput(const std::string& path) {
    auto value = std::make_unique<std::ifstream>(path.c_str());
    if (!value->is_open()) {
        std::stringstream ss;
        ss << "Could not open " << path << " for reading!";
        auto str = ss.str();
        throw Problem("openFileForInput", str);
    }
    if (_in) {
        _inputs.emplace_front(std::move(_in));
    }
    _in.swap(value);
}
void Machine::closeFileForOutput() {
    if (_out) {
        _out->close();
        if (!_outputs.empty()) {
            _out.swap(_outputs.front());
            _outputs.pop_front();
        } else {
            _out.reset();
        }
    }
}

void Machine::closeFileForInput() {
    if (_in) {
        _in->close();
        if (!_inputs.empty()) {
            _in.swap(_inputs.front());
            _inputs.pop_front();
        } else {
            _in.reset();
        }
    }
}
NativeFunction binaryOperation(BinaryOperation op) {
    return [op](auto& mach) {
        auto top = mach.popParameter();
        auto lower = mach.popParameter();
        mach.pushParameter(op(mach, lower, top));
    };
}

NativeFunction unaryOperation(UnaryOperation op) {
    return [op](auto& mach) {
        auto top = mach.popParameter();
        mach.pushParameter(op(mach, top));
    };
}


class DictionaryEntry {
    public:
        explicit DictionaryEntry(const std::string& name) : _name(name), _fake(false), _compileTimeInvoke(false) { }
        ~DictionaryEntry();
        void setNext(Word next) noexcept { _next = next; }
        void setNext(OptionalWord next) noexcept { _next = next; }
        OptionalWord getNext() const noexcept { return _next; }
        OptionalWord findWord(const std::string& name);
        void addWord(NativeFunction);
        void addWord(Word);
        void addWord(OptionalWord);
		void addWord(Number n);
        void addWord(const std::string&);
        void addWord(SharedVariable v);
        void invoke(Machine& machine);
        bool isFake() const noexcept { return _fake; }
        void setFake(bool value) noexcept { _fake = value; }
        bool compileTimeInvokable() const noexcept { return _compileTimeInvoke; }
        void setCompileTimeInvokable(bool value) noexcept { _compileTimeInvoke = value; }
        bool matches(const std::string& name);
        const std::string& getName() const noexcept { return _name; }
        auto size() const noexcept -> std::list<NativeFunction>::size_type { return _contents.size(); }
    private:
        std::string _name;
        bool _fake, _compileTimeInvoke;
        OptionalWord _next;
        std::list<NativeFunction> _contents;
};

bool DictionaryEntry::matches(const std::string& name) {
    return !_fake && equalsIgnoreCase(name, _name);
}

void Machine::newCompilingWord(const std::string& str) {
    // this can leak but it is the most straight forward
    auto entry = std::make_shared<DictionaryEntry>(str);
    if (str.empty()) {
        entry->setFake(true);
    }
    _compile = entry;
}

void Machine::compileCurrentWord() {
    if (!currentlyCompiling()) {
        throw Problem("compileCurrentWord", "Not in compilation mode!");
    } else {
        if (_front) {
            _compile->get()->setNext(*_front);
        }
        _front.swap(_compile);
        clearCurrentWord();
    }
}
void Machine::clearCurrentWord() {
    _compile.reset();
}
void Machine::saveCurrentlyCompilingWord() {
    pushSubroutine(*_compile);
}
void Machine::restoreCurrentlyCompilingWord() {
    _compile = std::get<Word>(popSubroutine());
}

Machine::Machine(Address capacity) {
    _memory = std::make_unique<forth::byte[]>(capacity);
    _capacity = capacity;
}
Machine::~Machine() { 
    while (_in) {
        closeFileForInput();
    }
    while (_out) {
        closeFileForOutput();
    }
}

OptionalWord Machine::lookupWord(const std::string& str) {
    if (_front) {
        // unpack it
        auto& val = *_front;
        if (val->matches(str)) {
            return _front;
        } else {
            return _front->get()->findWord(str);
        }
    } else {
        return _front;
    }
}

void Machine::pushParameter(SharedVariable d) { _parameter.push_front(d); }
void Machine::pushParameter(Datum d) { _parameter.push_front(d); }
void Machine::pushParameter(Number n) { _parameter.push_front(n); }
void Machine::pushParameter(Word v) { _parameter.push_front(v); }
void Machine::pushParameter(NativeFunction fn) { _parameter.push_front(fn); }
void Machine::pushParameter(const std::string& str) { _parameter.push_front(str); }

void Machine::pushSubroutine(SharedVariable d) { _subroutine.push_front(d); }
void Machine::pushSubroutine(Datum d) { _subroutine.push_front(d); }
void Machine::pushSubroutine(Number n) { _parameter.push_front(n); }
void Machine::pushSubroutine(Word v) { _subroutine.push_front(v); }
void Machine::pushSubroutine(NativeFunction fn) { _subroutine.push_front(fn); }
void Machine::pushSubroutine(const std::string& str) { _subroutine.push_front(str); }

Datum Machine::popParameter() {
    if (_parameter.empty()) {
        throw Problem("popParameter", "Stack Empty!");
    } else {
        Datum top = _parameter.front();
        _parameter.pop_front();
        return top;
    }
}

Datum Machine::popSubroutine() {
    if (_subroutine.empty()) {
        throw Problem("popSubroutine", "Stack Empty!");
    } else {
        auto top = _subroutine.front();
        _subroutine.pop_front();
        return top;
    }
}

void DictionaryEntry::invoke(Machine& mach) {
    if (mach.debugActive()) {
        for (auto a = 0; a < executionDepth; ++a) {
            mach.getOutput() << "-";
        }
        mach.getOutput() << "> " << getName() << std::endl;
    }
    ++executionDepth;
    for (auto& x : _contents) {
        x(mach);
    }
    --executionDepth;
    if (mach.debugActive()) {
        mach.getOutput() << "<";
        for (auto a = 0; a < executionDepth; ++a) {
            mach.getOutput() << "-";
        }
        mach.getOutput() << " " << getName() << std::endl;
    }
}

DictionaryEntry::~DictionaryEntry() { }


OptionalWord DictionaryEntry::findWord(const std::string& name) {
    if (_next) {
        auto& _n = *_next;
        if (_n->matches(name)) {
            return _next;
        } else {
            return _n->findWord(name);
        }
    } else {
        return _next;
    }
}

void DictionaryEntry::addWord(SharedVariable v) {
    _contents.emplace_back([v](Machine& mach) { mach.pushParameter(v); });
}
void DictionaryEntry::addWord(NativeFunction fn) {
    _contents.emplace_back(fn);
}
void DictionaryEntry::addWord(Word dict) {
    addWord([dict](Machine& mach) { dict->invoke(mach); });
}
void DictionaryEntry::addWord(OptionalWord dict) {
    if (!dict) {
        throw Problem("DictionaryEntry::addWord", "Cannot add unpopulated item!");
    } else {
        addWord(*dict);
    }
}


void DictionaryEntry::addWord(Number n) {
	addWord([n](Machine& mach) { mach.pushParameter(n); });
}
void DictionaryEntry::addWord(const std::string& str) {
    addWord([str](Machine& mach) { mach.pushParameter(str); });
}

void semicolon(Machine& mach) {
    if (mach.currentlyCompiling()) {
        mach.getCurrentlyCompilingWord().value()->addWord(NativeFunction(semicolon));
        mach.compileCurrentWord();
    }
}

void colon(Machine& mach) {
    if (mach.currentlyCompiling()) {
        throw Problem(":", "Already compiling!");
    } else {
        auto str = mach.readNext();
        mach.newCompilingWord(str);
    }
}

bool keepExecuting = true;
void bye(Machine&) {
    keepExecuting = false;
}



void Machine::errorOccurred() noexcept {
    _parameter.clear();
    _subroutine.clear();
    executionDepth = 0;
    if (_compile) {
        _compile.reset();
    }
}

void Machine::addWord(const std::string& str, NativeFunction fn, bool fake, bool compileInvoke) {
    Word ptr = std::make_shared<DictionaryEntry>(str);
    ptr->setFake(fake);
    ptr->setCompileTimeInvokable(compileInvoke);
    ptr->addWord(fn);
    if (_front) {
        ptr->setNext(_front);
    }
    _front = OptionalWord(ptr);
}
void drop(Machine& mach) { 
    mach.popParameter();
}
void swap(Machine& mach) { 
    auto top = mach.popParameter();
    auto lower = mach.popParameter();
    mach.pushParameter(top);
    mach.pushParameter(lower);
}
// binary operations
#define X(name, op) \
    template<typename T> \
    Number name (Number a, Number b) { \
        return Number (a.get<T>() op b.get<T>()) ; \
    }
#define Y(name, op, type) 
#include "BinaryOperators.def"
#undef Y
#undef X
template<>
Number logicalXor<bool>(Number a, Number b) {
    return Number (a.getTruth() != b.getTruth());
}
bool numberRoutine(Machine& mach, const std::string& word) {
    auto fn = [&mach](Number value) {
        if (mach.currentlyCompiling()) {
            mach.getCurrentlyCompilingWord().value()->addWord(value);
        } else {
            mach.pushParameter(value);
        }
    };
    // floating point
    // integers
    // first do some inspection first
    if (word[0] == '0' && (word[1] == 'x' || word[1] == 'X')) {
        auto copy = word;
        copy[1] = '0';
        std::istringstream hexAddress (copy);
        forth::Address tmpAddress;
        hexAddress >> std::hex >> tmpAddress;
        if (!hexAddress.fail()) {
            fn(tmpAddress);
            return true;
        }
        return false;
    }
    forth::Integer tmpInt;
    std::istringstream parseAttempt(word);
    parseAttempt >> tmpInt;
    if (!parseAttempt.fail() && parseAttempt.eof()) {
        fn(tmpInt);
        return true;
    }
    return false;
}
void Machine::store(Address addr, byte value) {
    if (addr > getMaximumAddress()) {
        throw Problem("store", "Illegal address!");
    }
    _memory[addr] = value;
}
byte Machine::load(Address addr) {
    if (addr > getMaximumAddress()) {
        throw Problem("load", "Illegal address!");
    }
    return _memory[addr];
}
void storeByte(Machine& m) {
    auto value = std::get<Number>(m.popParameter());
    auto addr = std::get<Number>(m.popParameter()).address;
    m.store(addr, forth::decodeBits<Address, byte, 0xFF, 0>(value.address));
}
void loadByte(Machine& m) {
    auto addr = std::get<Number>(m.popParameter()).address;
	Number n(Address(m.load(addr)) & 0xFF);
    m.pushParameter(n);
}
void storeVariable(Machine& m) {
    // backwards compared to most other words
    auto variable = std::get<SharedVariable>(m.popParameter());
    auto value = m.popParameter();
    std::visit([variable](auto&& value) {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<T, Number>) {
                    variable->_value = value;
                } else if constexpr (std::is_same_v<T, std::string>) {
                    variable->_value = value;
                } else if constexpr (std::is_same_v<T, SharedVariable>) {
                    variable->_value = value;
                } else {
                    throw Problem("storeVariable", " illegal value type!");
                }
            }, value);
}
void loadVariable(Machine& m) {
    auto variable = std::get<SharedVariable>(m.popParameter());
    std::visit([&m](auto&& value) { m.pushParameter(value); }, variable->_value);
}
void printDatum(Machine& m, Datum& top) {
    std::visit([&m](auto&& value) {
            auto f = m.getOutput().flags();
            using T = std::decay_t<decltype(value)>;
			if constexpr (std::is_same_v<T, Number>) {
				m.getOutput() << std::dec << value.integer;
            } else if constexpr (std::is_same_v<T, Word>) {
                if (value->isFake()) {
                    m.getOutput() << "fake compiled entry 0x" << std::hex << value.get();
                } else {
                    m.getOutput() << "word: " << value->getName();
                }
            } else if constexpr (std::is_same_v<T, std::string>) {
                m.getOutput() << value;
            } else if constexpr (std::is_same_v<T, NativeFunction>) {
                m.getOutput() << "Native Function";
            } else if constexpr (std::is_same_v<T, SharedVariable>) {
                m.getOutput() << "Variable!";
            } else {
                static_assert(forth::AlwaysFalse<T>::value, "Unimplemented type!");
            }
            m.getOutput().flush();
            m.getOutput().setf(f);
            }, top);
}
void printTop(Machine& m) {
    auto top = m.popParameter();
    printDatum(m, top);
}
void Machine::viewParameterStack() {
    for (auto & x : _parameter) {
        printDatum(*this, x);
        getOutput() << std::endl;
    }
}
void enterCompileModeWithName(Machine& mach, const std::string& name) {
    if (mach.currentlyCompiling()) {
        throw Problem("enterCompileMode", "Already compiling!");
    } 
    mach.newCompilingWord(name);
}
void enterCompileMode(Machine& mach) {
    auto name = mach.readNext();
    enterCompileModeWithName(mach, name);
}

void processString(Machine& mach) {
    // keep reading from input until we get a word which ends with "
    std::stringstream ss;
    while (true) {
        auto str = mach.readNext();
        ss << str << " ";
        if (!str.empty() && str.back() == '"') {
            break;
        }
    }
    auto s = ss.str();
    auto str = s.substr(0, s.size() - 2);
    if (mach.currentlyCompiling()) {
        mach.getCurrentlyCompilingWord().value()->addWord(str);
    } else {
        mach.pushParameter(str);
    }
}
Datum typeCode(Machine& mach, Datum top) {
    return Address(top.index());
}
void closeInputFile(Machine& mach) {
    mach.closeFileForInput();
}
void openInputFile(Machine& mach) {
    auto top = mach.popParameter();
    auto path = std::get<std::string>(top);
    mach.openFileForInput(path);
}
void ifStatement(Machine& mach) {
    if (!mach.currentlyCompiling()) {
        throw Problem("ifStatement", "Must be compiling for this word to work!");
    } 
    auto c = mach.getCurrentlyCompilingWord().value(); // do an evaluation of the contents
    // we need to add a native function to perform the check which assumes a boolean
    mach.saveCurrentlyCompilingWord(); // save our outer operation to the stack
    // now we need to generate two compile entries
    mach.newCompilingWord(); // this no name means fake :D
    auto z = mach.getCurrentlyCompilingWord().value();
    c->addWord(z);
    mach.saveCurrentlyCompilingWord(); // save this to the stack 
    mach.newCompilingWord(); // another fake entry for the onTrue portion
}
void elseStatement(Machine& mach) {
    if (!mach.currentlyCompiling()) {
        throw Problem("elseStatement", "Must be compiling for this word to work!");
    }

    // now we need to finalize the ontrue portion
    auto front = mach.getCurrentlyCompilingWord().value();
    mach.compileCurrentWord(); // so compile it and eliminate it, the front of the dictionary will have what we need!
    mach.restoreCurrentlyCompilingWord(); // go back one level
    mach.getCurrentlyCompilingWord().value()->addWord(NativeFunction([front](auto& x) { x.pushParameter(front); }));
    mach.saveCurrentlyCompilingWord(); // then put it back onto the stack
    mach.newCompilingWord(); // now make the else conditional
}
void thenStatement(Machine& mach) {
    if (!mach.currentlyCompiling()) {
        throw Problem("thenStatement", "Must be compiling for this word to work!");
    }
    auto front = mach.getCurrentlyCompilingWord().value();
    mach.compileCurrentWord(); // regardless if we have hit else or not we need to compile this word
    mach.restoreCurrentlyCompilingWord(); // go up one level
    // make sure that we load this argument onto the stack
    auto ifStatement = mach.getCurrentlyCompilingWord().value();
    ifStatement->addWord(NativeFunction([front](auto& x) { x.pushParameter(front); }));
    // we have to make a fake else statement if there is only one body in the if statement
    ifStatement->addWord(mach.lookupWord(ifStatement->size() == 1 ? "predicated" : "choose"));
    mach.compileCurrentWord();
    mach.restoreCurrentlyCompilingWord();
}
void bodyInvoke(Machine& mach) {
    auto onFalse = mach.popParameter();
    auto onTrue = mach.popParameter();
    if (auto condition = mach.popParameter() ; std::get<Number>(condition).getTruth()) {
        std::get<Word>(onTrue)->invoke(mach);
    } else {
        std::get<Word>(onFalse)->invoke(mach);
    }
}
void predicatedInvoke(Machine& mach) {
    auto onTrue = mach.popParameter();
    if (auto condition = mach.popParameter() ; std::get<Number>(condition).getTruth()) {
        std::get<Word>(onTrue)->invoke(mach);
    }
    // do nothing on false
}
void dup(Machine& mach) {
	auto a = mach.popParameter();
	mach.pushParameter(a);
	mach.pushParameter(a);
}
void rot(Machine& mach) {
	// ( a b c -- b c a )
	auto c = mach.popParameter();
	auto b = mach.popParameter();
	auto a = mach.popParameter();
	mach.pushParameter(b);
	mach.pushParameter(c);
	mach.pushParameter(a);
}
void over(Machine& mach) {
	// ( a b -- a b a )
	auto b = mach.popParameter();
	auto a = mach.popParameter();
	mach.pushParameter(a);
	mach.pushParameter(b);
	mach.pushParameter(a);
}
void pushOntoReturnStack(Machine& mach) {
	// ( n1 -- )
	mach.pushSubroutine(mach.popParameter());
}
void subroutineToParameterStack(Machine& mach) {
    // ( -- n1 )
    mach.pushParameter(mach.popSubroutine());
}
void printTitle(Machine& mach, OptionalWord curr) {
	if (curr) {
		auto p = curr.value();
		printTitle(mach, p->getNext());
		if (!p->isFake()) {
			mach.getOutput() << p->getName() << std::endl;
		}
	}
}
void words(Machine& mach) {
	printTitle(mach, mach.getFront());
}
NativeFunction callBinaryNumberOperation(std::function<Number(Number, Number)> fn) {
    return [fn](auto& mach) {
        auto top = std::get<Number>(mach.popParameter());
        auto lower = std::get<Number>(mach.popParameter());
        mach.pushParameter(fn(lower, top));
    };
}
void invokeBinaryNumberOperation(Machine& mach, std::function<Number(Number, Number)> fn) {
    callBinaryNumberOperation(fn)(mach);
}

NativeFunction callUnaryNumberOperation(std::function<Number(Number)> fn) {
    return [fn](auto& mach) {
        auto top = std::get<Number>(mach.popParameter());
        mach.pushParameter(fn(top));
    };
}
template<typename T>
Number notOperation(Number a) {
    if constexpr (std::is_same_v<T, bool>) {
        return Number(!a.getTruth());
    } else {
        return Number(~a.get<T>());
    }
}
template<typename T>
Number minusOperation(Number a) {
    return Number(- a.get<T>());
}

void getMemorySize(Machine& mach) {
    mach.pushParameter(Number(mach.getCapacity()));
}

void resizeMemory(Machine& mach) {
    std::visit([&mach](auto&& value) {
                using T = std::decay_t<decltype(value)>;
                if constexpr (std::is_same_v<T, Number>) {
                    mach.resizeMemory(value.address);
                } else {
                    throw Problem("resizeMemory", " top is not a number!");
                }
            }, mach.popParameter());
}
void addLiteralToCompilation(Machine& mach) {
    if (!mach.currentlyCompiling()) {
        throw Problem("literal", " not currently compiling!");
    }
    auto top = mach.popParameter();
    std::visit([&mach](auto&& value) { mach.getCurrentlyCompilingWord().value()->addWord(value);}, top);
}
Number powOperation(Number a, Number b) {
    return Number(Integer(pow(double(a.integer), double(b.integer))));
}
Number powOperationUnsigned(Number a, Number b) {
    return Number(Address(pow(double(a.address), double(b.address))));
}

void emitCharacter(Machine& mach) {
    auto top = mach.popParameter();
    mach.getOutput() << char(std::get<Number>(top).bytes[0]);
}
void defineVariableWithName(Machine& mach, const std::string& name) {
    if (mach.currentlyCompiling()) {
        throw Problem("defineVariable", " cannot define variables while compiling!");
    }
    // we need to define a new variable via a dictionary entry
    auto ptr = std::make_shared<Variable>();
    enterCompileModeWithName(mach, name);
    mach.getCurrentlyCompilingWord().value()->addWord(ptr);
    semicolon(mach);
    if (mach.debugActive()) {
        for (auto a = 0; a < executionDepth; ++a) {
            mach.getOutput() << "-";
        }
        mach.getOutput() << "- defined variable: " << name << std::endl;
    }
}
void defineVariable(Machine& mach) {
    auto name = mach.readNext();
    defineVariableWithName(mach, name);
}
void defineVariableThenLoad(Machine& mach) {
    defineVariable(mach);
    mach.getFront().value()->invoke(mach); // put the variable onto the stack right now!
}
void dumpMemoryToFile(Machine& mach) {
    try {
        mach.dumpMemoryToFile(std::get<std::string>(mach.popParameter()));
    } catch (std::bad_variant_access&) {
        throw Problem("dumpMemoryToFile", " top of the stack was not a string!");
    }
}
void openBinaryFile(Machine& mach) {
    auto top = expect<std::string>("openBinaryFile", mach.popParameter());
    mach.openBinaryFile(top);
}
void writeBinaryFile(Machine& mach) {
    auto top = static_cast<byte>(expect<Number>("writeBinaryFile", mach.popParameter()).address);
    mach.writeBinaryFile(top);
}
void closeBinaryFile(Machine& mach) {
    mach.closeBinaryFile();
}
void stackDepth(Machine& mach) {
    mach.pushParameterDepth();
}
void switchOutOfCompileMode(Machine& mach) {
    mach.saveCurrentlyCompilingWord();
    mach.clearCurrentWord();
}
void switchBackToCompileMode(Machine& mach) {
    mach.restoreCurrentlyCompilingWord();
}
void switchBackToCompileModeWithLiteral(Machine& mach) {
    mach.restoreCurrentlyCompilingWord();
    addLiteralToCompilation(mach);
}
void ignoreInputUntilNewline(Machine& mach) {
    mach.getInput().ignore(std::numeric_limits<std::streamsize>::max(), '\n');
}
void enterIgnoreInputMode(Machine& mach) {
    mach.getInput().ignore(std::numeric_limits<std::streamsize>::max(), ')');
}
void setupDictionary(Machine& mach) {
    mach.addWord("open-binary-file", openBinaryFile);
    mach.addWord("close-binary-file", closeBinaryFile);
    mach.addWord("write-binary-file", writeBinaryFile);
	mach.addWord("words", words);
    mach.addWord("dump-memory-to-file", dumpMemoryToFile);
	mach.addWord("R", pushOntoReturnStack);
	mach.addWord("dup", dup);
	mach.addWord("rot", rot);
	mach.addWord("over", over);
    mach.addWord("drop", drop);
    mach.addWord("depth", stackDepth);
    mach.addWord("swap", swap);
    mach.addWord("^.b", callBinaryNumberOperation(logicalXor<bool>));
    mach.addWord("(", enterIgnoreInputMode, false, true);
    mach.addWord(";", semicolon, false, true);
    mach.addWord("bye", bye);
    mach.addWord("mload.byte", loadByte);
    mach.addWord("mstore.byte", storeByte);
    mach.addWord("load.variable", loadVariable);
    mach.addWord("store.variable", storeVariable);
    mach.addWord(".", printTop);
    mach.addWord(":", enterCompileMode);
    mach.addWord(".s", std::mem_fn(&Machine::viewParameterStack));
    mach.addWord("\"", processString, false, true);
    mach.addWord("emit", emitCharacter);
    mach.addWord("type-code", unaryOperation(typeCode));
    mach.addWord("open-input-file", openInputFile);
    mach.addWord("close-input-file", closeInputFile);
    mach.addWord("choose", bodyInvoke);
    mach.addWord("predicated", predicatedInvoke);
    mach.addWord("if", ifStatement, false, true);
    mach.addWord("else", elseStatement, false, true);
    mach.addWord("then", thenStatement, false, true);
    mach.addWord("complement.u", callUnaryNumberOperation(notOperation<Address>));
    mach.addWord("complement.s", callUnaryNumberOperation(notOperation<Integer>));
    mach.addWord("complement.b", callUnaryNumberOperation(notOperation<bool>));
    mach.addWord("minus.s", callUnaryNumberOperation(minusOperation<Integer>));
    mach.addWord("minus.u", callUnaryNumberOperation(minusOperation<Address>));
    defineVariableWithName(mach, "*number-variant-code*"); //, 0);
    defineVariableWithName(mach, "*word-variant-code*"); // , 1);
    defineVariableWithName(mach, "*native-function-variant-code*"); // , 2);
    defineVariableWithName(mach, "*string-variant-code*"); //, 3);
    defineVariableWithName(mach, "*variable-variant-code*"); //, 4);
#define X(name, op)
#define Y(name, op, type) mach.addWord(#op INDIRECTION(YTypeString, type) , callBinaryNumberOperation( name < INDIRECTION(YType, type) >));
#include "BinaryOperators.def"
#undef X
    mach.addWord("*sizeof-address*", [](auto& x) { x.pushParameter(Number(sizeof(Address))); });
    mach.addWord("*sizeof-integer*", [](auto& x) { x.pushParameter(Number(sizeof(Integer))); });
    mach.addWord("*sizeof-half-address*", [](auto& x) { x.pushParameter(Number(sizeof(forth::HalfAddress))); });
    mach.addWord("*sizeof-quarter-address*", [](auto& x) { x.pushParameter(Number(sizeof(forth::QuarterAddress))); });
    mach.addWord("*bitwidth*", [](auto& x) { x.pushParameter(Number(CHAR_BIT)); });
    mach.addWord("*memory-size*", getMemorySize);
    mach.addWord("resize-memory", resizeMemory);
    mach.addWord("literal", addLiteralToCompilation, false, true);
    mach.addWord("**.s", callBinaryNumberOperation(powOperation));
    mach.addWord("**.u", callBinaryNumberOperation(powOperationUnsigned));
    mach.addWord("variable", defineVariable);
    mach.addWord("variable$", defineVariableThenLoad);
    mach.addWord("[", switchOutOfCompileMode, false, true);
    mach.addWord("]", switchBackToCompileMode);
    mach.addWord("]L", switchBackToCompileModeWithLiteral);
    mach.addWord("\\", ignoreInputUntilNewline, false, true);
    mach.addWord("enable-debug", [](Machine& mach) { mach.setDebugging(true); });
    mach.addWord("disable-debug", [](Machine& mach) { mach.setDebugging(false); });
    mach.addWord("debug?", [](Machine& mach) { mach.pushParameter(Number(mach.debugActive() ? Address(-1) : Address(0)) ); });
    mach.addWord("set-print-ok", [](Machine& mach) { printOk = std::get<Number>(mach.popParameter()).getTruth(); });
    mach.addWord("print-ok?", [](Machine& mach) { mach.pushParameter(Number(printOk)); });
}
int main(int argc, char** argv) {
    Machine mach;
    setupDictionary(mach);
    std::list<std::string> temp;
    // only the first argument is actually read, the rest are ignored
    if (argc > 1) {
        mach.pushParameter(std::string(argv[1]));
        openInputFile(mach);
    }
    while (keepExecuting) {
        try {
            if (auto str = mach.readNext() ; !str.empty()) {
                if (auto entry = mach.lookupWord(str); entry) {
                    if (auto value = entry.value() ; mach.currentlyCompiling()) {
                        if (value->compileTimeInvokable()) {
                            value->invoke(mach);
                        } else {
                            mach.getCurrentlyCompilingWord().value()->addWord(entry);
                        }
                    } else {
                        value->invoke(mach);
                    }
                } else {
                    if (!numberRoutine(mach, str)) {
                        throw Problem(str, "?");
                    }
                }
                if (!mach.currentlyCompiling() && printOk) {
                    mach.getOutput() << " ok" << std::endl;
                } 
            }

        } catch (Problem& p) {
            // clear out the stacks as well
            mach.errorOccurred();
            std::cerr << p.getWord() << p.getMessage() << std::endl;
        } catch (std::bad_variant_access& a) {
            mach.errorOccurred();
            std::cerr << "bad variant: " << a.what() << std::endl;
        }
    }
    return 0;
}

#undef YTypeStringAddress
#undef YTypeStringbool
#undef YTypeStringInteger
#undef YTypeAddress
#undef YTypebool
#undef YTypeInteger
