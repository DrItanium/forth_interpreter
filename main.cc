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
    Number(bool value) : integer(value ? -1 : 0) { }
    Number(char c) : integer(c) { }

	template<typename T>
	T get() noexcept {
		using K = std::decay_t<T>;
		if constexpr (std::is_same_v<K, Integer>) {
			return integer;
		} else if constexpr (std::is_same_v<K, Address>) {
			return address;
		} else if constexpr (std::is_same_v<K, bool>) {
			return getTruth();
        } else if constexpr (std::is_same_v<K, char>) {
            return char(address);
        } else if constexpr (std::is_same_v<K, byte>) {
            return byte(address);
		} else {
			static_assert(forth::AlwaysFalse<T>::value, "Unsupported type specified!");
		}
	}
    bool getTruth() const noexcept { return address != 0; }
    Integer getInteger() const noexcept { return integer; }
    Address getAddress() const noexcept { return address; }
	Integer integer;
	Address address;
};
struct Variable;
using SharedVariable = std::shared_ptr<Variable>;
struct Variable {
    using Contents = std::variant<Number, std::string, SharedVariable>;
    std::string _name;
    Contents _value;
};

using Datum = std::variant<Number, Word, NativeFunction, std::string, SharedVariable>;

using UnaryOperation = std::function<Datum(Machine&, Datum)>;
using BinaryOperation = std::function<Datum(Machine&, Datum, Datum)>;
using Stack = GenericStack<Datum>;
using InputStack = GenericStack<std::unique_ptr<std::ifstream>>;
using OutputStack = GenericStack<std::unique_ptr<std::ofstream>>;
constexpr Address defaultByteCount = 1024 * 1024;
template<typename T>
T expect(const Datum& d) {
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
        throw Problem(str);
    }
}

bool printOk = false;
Address executionDepth = 0;
class Machine {
    public:
        explicit Machine(Address blockCount = defaultByteCount);
        ~Machine();
		void pushParameter(Number n);
        void pushParameter(Word ent);
        void pushParameter(NativeFunction fn);
        void pushParameter(const std::string&);
        void pushParameter(Datum d);
        void pushParameter(SharedVariable v);
		void pushParameter(Address addr) { pushParameter(Number(addr)); }
		void pushParameter(bool b) { pushParameter(Number(b)); }
		void pushParameter(Integer addr) { pushParameter(Number(addr)); }
		void pushParameter(OptionalWord word);
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
        void addWord(const std::string& name, NativeFunction fn, bool compileTimeInvoke = false);
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
        std::unique_ptr<byte[]> _memory;
        Address _capacity;
        Address _mask;
        bool _enableDebugging = false;
};
void mustBeCompiling(Machine& mach);
void Machine::pushParameterDepth() noexcept {
    auto len = _parameter.size();
    pushParameter(Number(len));
}
void Machine::openBinaryFile(const std::string& path) {
    if (_binaryFile) {
        throw Problem(" Already have an open binary file!");
    }
    _binaryFile = std::make_unique<std::ofstream>();
    _binaryFile->open(path.c_str(), std::ofstream::binary | std::ofstream::out );
    if (!_binaryFile->is_open()) {
        throw Problem(" Unable to open file for writing!");
    } 
}
void Machine::writeBinaryFile(byte b) {
    if (!_binaryFile) {
        throw Problem(" Cannot write to a non existent binary file!");
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
        throw Problem(" Cannot close a binary file that is not open!");
    }
    _binaryFile->close();
    _binaryFile.reset();
}
void Machine::dumpMemoryToFile(const std::string& path) {
    std::ofstream stream(path.c_str(), std::fstream::binary);
    if (!stream.is_open()) {
        throw Problem(" couldn't open file for writing!");
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
    _memory = std::make_unique<byte[]>(newCapacity);
    _capacity = newCapacity;
    _mask = newCapacity - 1;
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
        throw Problem( str);
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
        throw Problem(str);
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

bool leaveEarly = false;
class DictionaryEntry {
    public:
        explicit DictionaryEntry(const std::string& name) : _name(name), _compileTimeInvoke(false) { }
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
        bool isFake() const noexcept { return _name.empty(); }
        bool compileTimeInvokable() const noexcept { return _compileTimeInvoke; }
        void setCompileTimeInvokable(bool value) noexcept { _compileTimeInvoke = value; }
        bool matches(const std::string& name);
        const std::string& getName() const noexcept { return _name; }
        auto size() const noexcept -> std::list<NativeFunction>::size_type { return _contents.size(); }
    private:
        std::string _name;
        bool _compileTimeInvoke;
        OptionalWord _next;
        std::list<NativeFunction> _contents;
};

bool DictionaryEntry::matches(const std::string& name) {
    return !isFake() && equalsIgnoreCase(name, _name);
}

void Machine::newCompilingWord(const std::string& str) {
    // this can leak but it is the most straight forward
    auto entry = std::make_shared<DictionaryEntry>(str);
    _compile = entry;
}

void Machine::compileCurrentWord() {
    if (!currentlyCompiling()) {
        throw Problem("Not in compilation mode!");
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
    _memory = std::make_unique<byte[]>(capacity);
    _capacity = capacity;
    _mask = capacity - 1;
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
void Machine::pushParameter(OptionalWord word) { 
	if (word) {
		pushParameter(word.value());
	} else {
		throw Problem("OPTIONAL WORD IS EMPTY!");
	}
}

void Machine::pushSubroutine(SharedVariable d) { _subroutine.push_front(d); }
void Machine::pushSubroutine(Datum d) { _subroutine.push_front(d); }
void Machine::pushSubroutine(Number n) { _parameter.push_front(n); }
void Machine::pushSubroutine(Word v) { _subroutine.push_front(v); }
void Machine::pushSubroutine(NativeFunction fn) { _subroutine.push_front(fn); }
void Machine::pushSubroutine(const std::string& str) { _subroutine.push_front(str); }

Datum Machine::popParameter() {
    if (_parameter.empty()) {
        throw Problem( "Stack Empty!");
    } else {
        Datum top = _parameter.front();
        _parameter.pop_front();
        return top;
    }
}

Datum Machine::popSubroutine() {
    if (_subroutine.empty()) {
        throw Problem("Stack Empty!");
    } else {
        auto top = _subroutine.front();
        _subroutine.pop_front();
        return top;
    }
}

void DictionaryEntry::invoke(Machine& mach) {
    if (mach.debugActive()) {
        mach.getOutput() << ">";
        for (auto a = 0; a < executionDepth; ++a) {
            mach.getOutput() << "-";
        }
        mach.getOutput() << " " << getName() << std::endl;
    }
    ++executionDepth;
    for (auto& x : _contents) {
        x(mach);
        if (leaveEarly) {
            leaveEarly = false;
            break;
        }
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
        throw Problem("Cannot add unpopulated item!");
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
        throw Problem("Already compiling!");
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

void Machine::addWord(const std::string& str, NativeFunction fn, bool compileInvoke) {
    Word ptr = std::make_shared<DictionaryEntry>(str);
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
        throw Problem("Illegal address!");
    }
    _memory[addr] = value;
}
byte Machine::load(Address addr) {
    if (addr > getMaximumAddress()) {
        throw Problem("Illegal address!");
    }
    return _memory[addr];
}
void storeByte(Machine& m) {
    auto addr = std::get<Number>(m.popParameter()).address;
    auto value = std::get<Number>(m.popParameter());
    m.store(addr, forth::decodeBits<Address, byte, 0xFF, 0>(value.address));
}
void loadByte(Machine& m) {
    auto addr = std::get<Number>(m.popParameter()).address;
	Number n(Address(m.load(addr)) & 0xFF);
    m.pushParameter(n);
}
void storeVariable(Machine& m) {
	auto variable = expect<SharedVariable>(m.popParameter());
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
                    throw Problem(" illegal value type!");
                }
            }, value);
}
void loadVariable(Machine& m) {
    auto variable = std::get<SharedVariable>(m.popParameter());
    std::visit([&m](auto&& value) { m.pushParameter(value); }, variable->_value);
}
void printDatum(Machine& m, Datum& top) {
    std::visit([&m](auto&& value) {
            using T = std::decay_t<decltype(value)>;
			if constexpr (std::is_same_v<T, Number>) {
				m.getOutput() << value.integer;
            } else if constexpr (std::is_same_v<T, Word>) {
                if (value->isFake()) {
                    m.getOutput() << "fake compiled entry: " << Integer(value.get());
                } else {
                    m.getOutput() << "word: " << value->getName();
                }
            } else if constexpr (std::is_same_v<T, std::string>) {
                m.getOutput() << value;
            } else if constexpr (std::is_same_v<T, NativeFunction>) {
				auto tmp = value.template target<void(*)(Machine&)>();
                m.getOutput() << "Native Function: " << (Integer)tmp;
            } else if constexpr (std::is_same_v<T, SharedVariable>) {
                m.getOutput() << "Variable: " << value->_name;
            } else {
                static_assert(forth::AlwaysFalse<T>::value, "Unimplemented type!");
            }
            }, top);
}

void printDatumUnsigned(Machine& m, Datum& top) {
    std::visit([&m](auto&& value) {
            using T = std::decay_t<decltype(value)>;
			if constexpr (std::is_same_v<T, Number>) {
				m.getOutput() << value.address;
            } else if constexpr (std::is_same_v<T, Word>) {
                if (value->isFake()) {
                    m.getOutput() << "fake compiled entry: " << Address(value.get());
                } else {
                    m.getOutput() << "word: " << value->getName();
                }
            } else if constexpr (std::is_same_v<T, std::string>) {
                m.getOutput() << value;
            } else if constexpr (std::is_same_v<T, NativeFunction>) {
				auto tmp = value.template target<void(*)(Machine&)>();
                m.getOutput() << "Native Function: " << (Address)tmp;
            } else if constexpr (std::is_same_v<T, SharedVariable>) {
                m.getOutput() << "Variable: " << value->_name;
            } else {
                static_assert(forth::AlwaysFalse<T>::value, "Unimplemented type!");
            }
            }, top);
}
void printTop(Machine& m) {
    auto top = m.popParameter();
    printDatum(m, top);
}
void printTopAsUnsignedNumber(Machine& m) {
	auto top = m.popParameter();
	printDatumUnsigned(m, top);
}
void Machine::viewParameterStack() {
    for (auto & x : _parameter) {
        printDatum(*this, x);
        getOutput() << std::endl;
    }
}
void enterCompileModeWithName(Machine& mach, const std::string& name) {
    if (mach.currentlyCompiling()) {
        throw Problem("Already compiling!");
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
    mustBeCompiling(mach);
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
    mustBeCompiling(mach);

    // now we need to finalize the ontrue portion
    auto front = mach.getCurrentlyCompilingWord().value();
    mach.compileCurrentWord(); // so compile it and eliminate it, the front of the dictionary will have what we need!
    mach.restoreCurrentlyCompilingWord(); // go back one level
    mach.getCurrentlyCompilingWord().value()->addWord(NativeFunction([front](auto& x) { x.pushParameter(front); }));
    mach.saveCurrentlyCompilingWord(); // then put it back onto the stack
    mach.newCompilingWord(); // now make the else conditional
}
void thenStatement(Machine& mach) {
    mustBeCompiling(mach);
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
void copyTopOfSubroutineToParameter(Machine& mach) {
    // ( -- n1 )
	auto result = mach.popSubroutine();
    mach.pushParameter(result);
	mach.pushSubroutine(result);
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
                    throw Problem(" top is not a number!");
                }
            }, mach.popParameter());
}
void addLiteralToCompilation(Machine& mach) {
    mustBeCompiling(mach);
    auto top = mach.popParameter();
    std::visit([&mach](auto&& value) { mach.getCurrentlyCompilingWord().value()->addWord(value);}, top);
}

Number powOperation(Number a, Number b) {
    return Number(Integer(pow(double(a.integer), double(b.integer))));
}

void emitCharacter(Machine& mach) {
    mach.getOutput() << expect<Number>(mach.popParameter()).get<char>();
}

void defineVariableWithName(Machine& mach, const std::string& name) {
    if (mach.currentlyCompiling()) {
        throw Problem(" cannot define variables while compiling!");
    }
    // we need to define a new variable via a dictionary entry
    auto ptr = std::make_shared<Variable>();
    ptr->_name = name;
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
        throw Problem(" top of the stack was not a string!");
    }
}
void openBinaryFile(Machine& mach) {
    auto top = expect<std::string>(mach.popParameter());
    mach.openBinaryFile(top);
}
void writeBinaryFile(Machine& mach) {
    auto top = static_cast<byte>(expect<Number>(mach.popParameter()).address);
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
void mustBeCompiling(Machine& mach) {
    if (!mach.currentlyCompiling()) {
        throw Problem(" Must be currently compiling!");
    }
}
void nestedFakeBodyForCompiling(Machine& mach) {
    mustBeCompiling(mach);
    mach.saveCurrentlyCompilingWord();
    mach.newCompilingWord();
}
void beginStatement(Machine& mach) {
    // need to capture the loop body as a word, it must be free floating right
    // now
    nestedFakeBodyForCompiling(mach);
}
void endStatement(Machine& mach) {
    mustBeCompiling(mach);
    auto c = mach.getCurrentlyCompilingWord().value(); // get the body out and save it
    mach.compileCurrentWord();
    mach.restoreCurrentlyCompilingWord();
    auto fn = [c](Machine& mach) {
        bool terminate = false;
        do {
            c->invoke(mach);
            // now get the top of the stack to see if we should continue
            terminate = std::get<Number>(mach.popParameter()).getTruth();
        } while (!terminate);
    };
    mach.getCurrentlyCompilingWord().value()->addWord(fn);
}
void continueStatement(Machine& mach) {
    mustBeCompiling(mach);
    auto c = mach.getCurrentlyCompilingWord().value();
    mach.compileCurrentWord();
    mach.restoreCurrentlyCompilingWord();
    auto fn = [c](Machine& mach) {
        Address top = std::get<Number>(mach.popParameter()).getAddress();
        Address lower = std::get<Number>(mach.popParameter()).getAddress();
        if (top != lower) {
            mach.pushParameter(Number(lower));
            mach.pushParameter(Number(top));
            do {
                c->invoke(mach);
                top = std::get<Number>(mach.popParameter()).getAddress();
                lower = std::get<Number>(mach.popParameter()).getAddress();
                if (top == lower ) {
                    break;
                } else {
                    mach.pushParameter(Number(lower));
                    mach.pushParameter(Number(top));
                }
            } while ( true );
        }
    };
    mach.getCurrentlyCompilingWord().value()->addWord(fn);
}
void doStatement(Machine& mach) {
    nestedFakeBodyForCompiling(mach);
}
void makeConstant(Machine& mach);
void raiseError(Machine& mach);
void markImmediate(Machine& mach);
void invokeTopOfStack(Machine& mach) {
    expect<NativeFunction>(mach.popParameter())(mach);
}
void putWordOnTopOfStack(Machine& mach) {
    auto next = mach.readNext();
    if (auto ofn = mach.lookupWord(next); ofn) {
        NativeFunction fn = [ptr = ofn.value()](Machine& mach) { ptr->invoke(mach); };
        if (mach.currentlyCompiling()) {
            mach.getCurrentlyCompilingWord().value()->addWord([fn](Machine& mach) { mach.pushParameter(fn); });
        } else {
            mach.pushParameter(fn);
        }
    } else {
		throw Problem("?");
	}
}
void exitWordEarly(Machine&) {
    leaveEarly = true;
}
template<typename T>
void pushSize(Machine& mach) {
	mach.pushParameter(Number(sizeof(T)));
}
void hereOperation(Machine& mach) {
	if (mach.currentlyCompiling()) {
		mach.getCurrentlyCompilingWord().value()->addWord(mach.getFront());
	} else {
		mach.pushParameter(mach.getFront());
	}
}
void markFrontAsImmediate(Machine& mach) {
	if (auto opt = mach.getFront(); opt) {
		opt.value()->setCompileTimeInvokable(true);
	} else {
		throw Problem("DICTIONARY EMPTY!");
	}
}
void frontIsImmediate(Machine& mach) {
	if (auto opt = mach.getFront(); opt) {
		mach.pushParameter(opt.value()->compileTimeInvokable());
	} else {
		mach.pushParameter(Number(false));
	}
}
void compileNextWord(Machine& mach) {
	if (!mach.currentlyCompiling()) {
		throw Problem("Must be compiling!");
	}
	auto nextWord = mach.readNext();
	if (auto ofn = mach.lookupWord(nextWord); ofn) {
		mach.getCurrentlyCompilingWord().value()->addWord(ofn.value());
	} else {
		std::stringstream ss;
		ss << " : " << nextWord << "?";
		auto str = ss.str();
		throw Problem(str);
	}
}
void setupDictionary(Machine& mach) {
	mach.addWord("here", hereOperation, true);
	mach.addWord("immediate", markFrontAsImmediate);
    mach.addWord("'", putWordOnTopOfStack, true);
    mach.addWord("invoke-tos", invokeTopOfStack);
    mach.addWord("begin", beginStatement, true);
    mach.addWord("end", endStatement, true);
    mach.addWord("do", doStatement, true);
    mach.addWord("continue", continueStatement, true);
    mach.addWord("open-binary-file", openBinaryFile);
    mach.addWord("close-binary-file", closeBinaryFile);
    mach.addWord("write-binary-file", writeBinaryFile);
	mach.addWord("words", words);
    mach.addWord("dump-memory-to-file", dumpMemoryToFile);
	mach.addWord(">r", pushOntoReturnStack);
	mach.addWord("r>", subroutineToParameterStack);
	mach.addWord("r@", copyTopOfSubroutineToParameter);
	mach.addWord("dup", dup);
	mach.addWord("rot", rot);
	mach.addWord("over", over);
    mach.addWord("drop", drop);
    mach.addWord("depth", stackDepth);
    mach.addWord("swap", swap);
    mach.addWord("(", enterIgnoreInputMode, true);
    mach.addWord(";", semicolon, true);
    mach.addWord("bye", bye);
    mach.addWord("c@", loadByte);
    mach.addWord("c!", storeByte);
    mach.addWord("v@", loadVariable);
    mach.addWord("v!", storeVariable);
    mach.addWord(".", printTop);
	mach.addWord("u.", printTopAsUnsignedNumber);
    mach.addWord(":", enterCompileMode);
    mach.addWord(".s", std::mem_fn(&Machine::viewParameterStack));
    mach.addWord("\"", processString, true);
    mach.addWord("emit", emitCharacter);
    mach.addWord("type-code", unaryOperation(typeCode));
    mach.addWord("open-input-file", openInputFile);
    mach.addWord(";s", closeInputFile);
    mach.addWord("choose", bodyInvoke);
    mach.addWord("predicated", predicatedInvoke);
    mach.addWord("if", ifStatement, true);
    mach.addWord("else", elseStatement, true);
    mach.addWord("then", thenStatement, true);
    mach.addWord("minus", callUnaryNumberOperation(minusOperation<Integer>));
    mach.addWord("invert", callUnaryNumberOperation(notOperation<Address>));
	auto mulU = callBinaryNumberOperation([](Number a, Number b) {
		return a.getAddress() * b.getAddress();
	});
    mach.addWord("*", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() * b.getInteger(); }));
    mach.addWord("u*", mulU);
    mach.addWord("+", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() + b.getInteger(); }));
    mach.addWord("u+", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() + b.getAddress(); }));
    mach.addWord("-", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() - b.getInteger(); }));
    mach.addWord("u-", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() - b.getAddress(); }));
    mach.addWord("/", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() / b.getInteger(); }));
    mach.addWord("u/", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() / b.getAddress(); }));
    mach.addWord("mod", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() % b.getInteger(); }));
    mach.addWord("umod", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() % b.getAddress(); }));
    mach.addWord("min", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() > b.getInteger() ? b.getInteger() : a.getInteger(); }));
    mach.addWord("umin", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() > b.getAddress() ? b.getAddress() : a.getAddress(); }));
    mach.addWord("max", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() > b.getInteger() ? a.getInteger() : b.getInteger(); }));
    mach.addWord("umax", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() > b.getAddress() ? a.getAddress() : b.getAddress(); }));
    mach.addWord("=", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() == b.getInteger() ? -1 : 0 ; }));
    mach.addWord("<>", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() != b.getInteger() ? -1 : 0 ; }));
    mach.addWord("<", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() < b.getInteger() ? -1 : 0 ; }));
    mach.addWord("u<", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() < b.getAddress() ? -1 : 0 ; }));
    mach.addWord(">", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() > b.getInteger() ? -1 : 0 ; }));
    mach.addWord("u>", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() > b.getAddress() ? -1 : 0 ; }));
    mach.addWord("<=", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() <= b.getInteger() ? -1 : 0 ; }));
    mach.addWord("u<=", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() <= b.getAddress() ? -1 : 0 ; }));
    mach.addWord(">=", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() >= b.getInteger() ? -1 : 0 ; }));
    mach.addWord("u>=", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() >= b.getAddress() ? -1 : 0 ; }));
    mach.addWord("<<", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() << b.getAddress(); }));
    mach.addWord("u<<", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() << b.getAddress(); }));
    mach.addWord(">>", callBinaryNumberOperation([](Number a, Number b) { return a.getInteger() >> b.getAddress(); }));
    mach.addWord("u>>", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() >> b.getAddress(); }));
    mach.addWord("and", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() & b.getAddress(); }));
    mach.addWord("or", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() | b.getAddress(); }));
    mach.addWord("xor", callBinaryNumberOperation([](Number a, Number b) { return a.getAddress() ^ b.getAddress(); }));
    mach.addWord("memory-size", getMemorySize);
    mach.addWord("resize-memory", resizeMemory);
    mach.addWord("literal", addLiteralToCompilation, true);
    mach.addWord("**", callBinaryNumberOperation(powOperation));
    mach.addWord("variable", defineVariable);
    mach.addWord("variable$", defineVariableThenLoad);
    mach.addWord("[", switchOutOfCompileMode, true);
    mach.addWord("]", switchBackToCompileMode);
    mach.addWord("]L", switchBackToCompileModeWithLiteral);
    mach.addWord("\\", ignoreInputUntilNewline, true);
	mach.addWord("\\c", ignoreInputUntilNewline, true);
    mach.addWord("enable-debug", [](Machine& mach) { mach.setDebugging(true); });
    mach.addWord("disable-debug", [](Machine& mach) { mach.setDebugging(false); });
    mach.addWord("debug?", [](Machine& mach) { mach.pushParameter(Number(mach.debugActive() ? Address(-1) : Address(0)) ); });
    mach.addWord("raise", raiseError);
    mach.addWord("constant", makeConstant);
    mach.addWord("immediate", markImmediate);
    mach.addWord("exit", exitWordEarly);
	mach.addWord("/c", pushSize<byte>);
	mach.addWord("/l", pushSize<Number>);
	mach.addWord("/w", pushSize<Number>);
	mach.addWord("/n", pushSize<Number>);
	mach.addWord("/link", pushSize<Number>);
	mach.addWord("/token", pushSize<Number>);
	auto performMod = [](Machine& mach) {
		// ( n1 n2 -- n3 n4 )
		auto top = expect<Number>(mach.popParameter());
		auto lower = expect<Number>(mach.popParameter());
		auto numerator = lower.getInteger();
		auto denominator = top.getInteger();
		if (denominator == 0) {
			throw Problem("Divide by zero!");
		}
		auto remainder = numerator % denominator;
		auto divisor = numerator / denominator;
		mach.pushParameter(Number(remainder));
		mach.pushParameter(Number(divisor));
	};
	mach.addWord("/mod", performMod);
	mach.addWord("um/mod", performMod);
	mach.addWord("ul*", mulU);
	mach.addWord("um*", mulU);
	mach.addWord("[compile]", compileNextWord, true);
	mach.addWord("abort\"", [](Machine& mach) {
			    auto normalBody = [](Machine& mach) {
					swap(mach);
					auto result = expect<Number>(mach.popParameter());
					if (result.getTruth()) {
						raiseError(mach);
					} else {
						drop(mach);
					}
				};
				processString(mach);
				if (mach.currentlyCompiling()) {
					mach.getCurrentlyCompilingWord().value()->addWord(normalBody);
				} else {
					normalBody(mach);
				}
			}, true);
	mach.addWord("string-length", [](Machine& mach) {
				auto top = expect<std::string>(mach.popParameter());
				mach.pushParameter(Number(top.size()));
			});
}
void raiseError(Machine& mach) {
    // raise an error to be caught by the runtime and reported
    // this will cause the stacks to be cleared too!
    // also giving the incorrect set of args will do the same thing :)
    auto msg = std::get<std::string>(mach.popParameter());
    throw Problem(msg);
}
void makeConstant(Machine& mach) {
    // ( a "name" -- )
    auto value = mach.popParameter();
    auto name = mach.readNext();
    mach.addWord(name, [value](Machine& mach) { mach.pushParameter(value); });
}
void interpret(Machine& mach) {
	std::string str;
    while (keepExecuting) {
        try {
			str = mach.readNext();
            if (!str.empty()) {
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
                        throw Problem("?");
                    }
                }
                if (!mach.currentlyCompiling() && printOk) {
                    mach.getOutput() << " ok" << std::endl;
                } 
            }

        } catch (Problem& p) {
            // clear out the stacks as well
            mach.errorOccurred();
			std::cerr << str << p.getMessage() << std::endl;
        } catch (std::bad_variant_access& a) {
            mach.errorOccurred();
            std::cerr << str <<  " bad variant: " << a.what() << std::endl;
        }
    }
}
void markImmediate(Machine& mach) {
    if (auto front = mach.getFront(); front) {
        front.value()->setCompileTimeInvokable(true);
    } else {
        throw Problem("no words defined!");
    }
}
int main(int argc, char** argv) {
    Machine mach;
    setupDictionary(mach);
    // only the first argument is actually read, the rest are ignored
    if (argc > 1) {
        mach.pushParameter(std::string(argv[1]));
        openInputFile(mach);
    }
    interpret(mach);
    return 0;
}

