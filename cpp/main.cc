#include "../Types.h"
#include "../Problem.h"
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

#define YTypeFloat forth::Floating
#define YTypeAddress forth::Address
#define YTypebool bool
#define YTypeInteger forth::Integer
#define YTypeStringFloat "f"
#define YTypeStringAddress "u" 
#define YTypeStringbool "b"
#define YTypeStringInteger "s"

class DictionaryEntry;
class Machine;

using Integer = forth::Integer;
using Address = forth::Address;
using byte = forth::byte;
#ifdef ALLOW_FLOATING_POINT
using Floating = forth::Floating;
#endif // end ALLOW_FLOATING_POINT

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
#ifdef ALLOW_BOOLEAN
	Number(bool t) : truth(t) { }
#endif // end ALLOW_BOOLEAN
#ifdef ALLOW_FLOATING_POINT
    Number(Floating f) : fp(f) { }
#endif // end ALLOW_FLOATING_POINT

	template<typename T>
	T get() noexcept {
		using K = std::decay_t<T>;
		if constexpr (std::is_same_v<K, Integer>) {
			return integer;
		} else if constexpr (std::is_same_v<K, Address>) {
			return address;
		} else if constexpr (std::is_same_v<K, bool>) {
			return getTruth();
		}
#ifdef ALLOW_FLOATING_POINT
		else if constexpr (std::is_same_v<K, Floating>) {
			return fp;
		}
#endif // end ALLOW_FLOATING_POINT
		else {
			static_assert(forth::AlwaysFalse<T>::value, "Unsupported type specified!");
		}
	}
    bool getTruth() const noexcept { 
#ifdef ALLOW_BOOLEAN
        return truth;
#else // !ALLOW_BOOLEAN
        return address != 0;
#endif // end ALLOW_BOOLEAN
    }
	Integer integer;
	Address address;
#ifdef ALLOW_BOOLEAN
	bool truth;
#endif
#ifdef ALLOW_FLOATING_POINT
	Floating fp;
#endif 
	byte bytes[sizeof(Address)];
};
using Datum = std::variant<Number,
      Word, 
      NativeFunction, 
      std::string >;

using UnaryOperation = std::function<Datum(Machine&, Datum)>;
using BinaryOperation = std::function<Datum(Machine&, Datum, Datum)>;
using Stack = GenericStack<Datum>;
using InputStack = GenericStack<std::istream*>;
using OutputStack = GenericStack<std::ostream*>;
constexpr Address defaultMemorySize = 4096;

class Machine {
    public:
        explicit Machine(Address memSize = defaultMemorySize);
        ~Machine();
		void pushParameter(Number n);
        void pushParameter(Word ent);
        void pushParameter(NativeFunction fn);
        void pushParameter(const std::string&);
        void pushParameter(Datum d);
        bool parameterStackEmpty() const noexcept { return _parameter.empty(); }
		void pushSubroutine(Number n);
        void pushSubroutine(Word ent);
        void pushSubroutine(NativeFunction fn);
        void pushSubroutine(const std::string&);
        void pushSubroutine(Datum d);
        bool subroutineStackEmpty() const noexcept { return _subroutine.empty(); }
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
        std::ostream& getOutput() const noexcept { return *_out; }
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
    private:
        std::ostream* _out = &std::cout;
        std::istream* _in = &std::cin;
        OptionalWord _front;
        OptionalWord _compile;
        Stack _parameter;
        Stack _subroutine;
        InputStack _inputs;
        OutputStack _outputs;
        std::unique_ptr<forth::byte[]> _memory;
        Address _capacity;
};
std::string Machine::readNext() {
    std::string word;
    (*_in) >> word;
    return word;
}
void Machine::openFileForOutput(const std::string& path) {
    auto* tmp = new std::ofstream(path.c_str());
    if (!tmp->is_open()) {
        std::stringstream ss;
        ss << "Could not open " << path << " for writing!";
        auto str = ss.str();
        throw Problem("openFileForOutput", str);
    }
    _outputs.push_front(_out);
    _out = tmp;
}

void Machine::openFileForInput(const std::string& path) {
    auto* tmp = new std::ifstream(path.c_str());
    if (!tmp->is_open()) {
        std::stringstream ss;
        ss << "Could not open " << path << " for reading!";
        auto str = ss.str();
        throw Problem("openFileForInput", str);
    }
    _inputs.push_front(_in);
    _in = tmp;
}
void Machine::closeFileForOutput() {
    if (_out != &std::cout) {
        std::ofstream* tmp = (std::ofstream*)_out;
        tmp->close();
        if (_outputs.empty()) {
            throw Problem("closeFileForOutput", "Stack underflow");
        } else {
            delete tmp;
            _out = _outputs.front();
            _outputs.pop_front();
        }
    }
}

void Machine::closeFileForInput() {
    if (_in != &std::cin) {
        std::ifstream* tmp = (std::ifstream*)_in;
        tmp->close();
        if (_inputs.empty()) {
            throw Problem("closeFileForInput", "Stack underflow");
        } else {
            delete tmp;
            _in = _inputs.front();
            _inputs.pop_front();
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
        _compile.reset();
    }
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
Machine::~Machine() { }

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

void Machine::pushParameter(Datum d) { _parameter.push_front(d); }
void Machine::pushParameter(Number n) { _parameter.push_front(n); }
void Machine::pushParameter(Word v) { _parameter.push_front(v); }
void Machine::pushParameter(NativeFunction fn) { _parameter.push_front(fn); }
void Machine::pushParameter(const std::string& str) { _parameter.push_front(str); }

void Machine::pushSubroutine(Datum d) { _subroutine.push_front(d); }
void Machine::pushSubroutine(Number n) { _parameter.push_front(n); }
void Machine::pushSubroutine(Word v) { _subroutine.push_front(v); }
void Machine::pushSubroutine(const std::string& str) { _subroutine.push_front(str); }

Datum Machine::popParameter() {
    if (parameterStackEmpty()) {
        throw Problem("popParameter", "Stack Empty!");
    } else {
        auto top(_parameter.front());
        _parameter.pop_front();
        return top;
    }
}

Datum Machine::popSubroutine() {
    if (subroutineStackEmpty()) {
        throw Problem("popSubroutine", "Stack Empty!");
    } else {
        auto top(_subroutine.front());
        _subroutine.pop_front();
        return top;
    }
}

void DictionaryEntry::invoke(Machine& mach) {
    for (auto& x : _contents) {
        x(mach);
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

bool ignoreInput = false;
void enterIgnoreInputMode(Machine&) {
    ignoreInput = true;
}


void Machine::errorOccurred() noexcept {
    _parameter.clear();
    _subroutine.clear();
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
#ifdef ALLOW_BOOLEAN
template<>
Number logicalXor<bool>(Number a, Number b) {
    return Number (a.getTruth() != b.getTruth());
}
#endif // end ALLOW_BOOLEAN

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
#ifdef ALLOW_BOOLEAN
    if (word == "true") {
        fn(true);
        return true;
    } else if (word == "false") {
        fn(false);
        return true;
    } 
#endif // end ALLOW_BOOLEAN
    std::istringstream parseAttempt(word);
    if (word.find('#') != std::string::npos) {
        forth::Address tmpAddress;
        parseAttempt >> std::hex >> tmpAddress;
        if (!parseAttempt.fail()) {
            fn(tmpAddress);
            return true;
        }
        return false;
    } 
    if (word.find('u') != std::string::npos) {
        forth::Address tmpAddress;
        parseAttempt >> tmpAddress;
        if (!parseAttempt.fail()) {
            fn(tmpAddress);
            return true;
        }
        return false;
    }
    parseAttempt.clear();
#ifdef ALLOW_FLOATING_POINT
    if (word.find('.') != std::string::npos) {
        forth::Floating tmpFloat;
        parseAttempt >> tmpFloat;
        if (!parseAttempt.fail() && parseAttempt.eof()) {
            fn(tmpFloat);
            return true;
        }
        // get out of here early since we hit something that looks like
        // a float
        return false;
    }
#endif // end ALLOW_FLOATING_POINT
    forth::Integer tmpInt;
    parseAttempt.clear();
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
	Number n(Address(m.load(addr)));
    m.pushParameter(n);
}
void getLowestEightBits(Machine& m) {
    // ( v -- lo8 shiftv )
    // this design allows us to do little endian saves
    auto num = std::get<Number>(m.popParameter()).address;
    m.pushParameter(Number(Address(forth::decodeBits<Address, byte, Address(0xFF), 0>(num))));
    m.pushParameter(Number(Address(forth::decodeBits<Address, Address, ~Address(0xFF), 8>(num))));
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
void enterCompileMode(Machine& mach) {
    if (mach.currentlyCompiling()) {
        throw Problem("enterCompileMode", "Already compiling!");
    } else {
        auto name = mach.readNext();
        mach.newCompilingWord(name);
    }
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
#ifdef ALLOW_BOOLEAN
    c->addWord(true);
    c->addWord(mach.lookupWord("==b"));
#else // ! ALLOW_BOOLEAN
    c->addWord(Number(0));
    c->addWord(mach.lookupWord("!=u")); // not equal zero
#endif

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
    mach.getCurrentlyCompilingWord().value()->addWord(NativeFunction([front](auto& x) { front->invoke(x); }));
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
    mach.getCurrentlyCompilingWord().value()->addWord(NativeFunction([front](auto& x) { front->invoke(x); }));
    if (mach.getCurrentlyCompilingWord().value()->size() == 1) {
        // we have to make a fake else statement
        mach.getCurrentlyCompilingWord().value()->addWord(mach.lookupWord("predicated"));
    } else {
        mach.getCurrentlyCompilingWord().value()->addWord(mach.lookupWord("choose"));
    }
    mach.compileCurrentWord();
    mach.restoreCurrentlyCompilingWord();
}
void addConstantWord(Machine& mach, const std::string& name, Number value) {
    std::stringstream ss;
    ss << "*" << name << "*";
    auto str = ss.str();
    mach.addWord(str, [value](auto& x) { x.pushParameter(value); });
}
#ifndef ALLOW_BOOLEAN
void addConstantWord(Machine& mach, const std::string& name, bool value) {
    addConstantWord(mach, name, Number(value ? 0 : -1));
}
#endif // end !ALLOW_BOOLEAN
void bodyInvoke(Machine& mach) {
    auto onFalse = mach.popParameter();
    auto onTrue = mach.popParameter();
    if (auto condition = mach.popParameter() ; std::get<Number>(condition).getTruth()) {
        std::get<NativeFunction>(onTrue)(mach);
    } else {
        std::get<NativeFunction>(onFalse)(mach);
    }
}
void predicatedInvoke(Machine& mach) {
    auto onTrue = mach.popParameter();
    if (auto condition = mach.popParameter() ; std::get<Number>(condition).getTruth()) {
        std::get<NativeFunction>(onTrue);
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
void setupDictionary(Machine& mach) {
#ifndef ALLOW_BOOLEAN
    mach.addWord("true", [](Machine& x) { x.pushParameter(Number(-1)); });
    mach.addWord("false", [](Machine& x) { x.pushParameter(Number(0)); });
#endif
	mach.addWord("words", words);
	mach.addWord("R", pushOntoReturnStack);
	mach.addWord("dup", dup);
	mach.addWord("rot", rot);
	mach.addWord("over", over);
    mach.addWord("drop", drop);
    mach.addWord("swap", swap);
#ifdef ALLOW_BOOLEAN
    mach.addWord("^b", callBinaryNumberOperation(logicalXor<bool>));
#endif // end ALLOW_BOOLEAN
    mach.addWord("(", enterIgnoreInputMode, false, true);
    mach.addWord(";", semicolon, false, true);
    mach.addWord("bye", bye);
    mach.addWord("@8", loadByte);
    mach.addWord("=8", storeByte);
    mach.addWord("lo8", getLowestEightBits);
    mach.addWord(".", printTop);
    mach.addWord(":", enterCompileMode);
    mach.addWord(".s", std::mem_fn(&Machine::viewParameterStack));
    mach.addWord("\"", processString, false, true);
    mach.addWord("type-code", unaryOperation(typeCode));
    mach.addWord("open-input-file", openInputFile);
    mach.addWord("close-input-file", closeInputFile);
    mach.addWord("choose", bodyInvoke);
    mach.addWord("predicated", predicatedInvoke);
    mach.addWord("if", ifStatement, false, true);
    mach.addWord("else", elseStatement, false, true);
    mach.addWord("then", thenStatement, false, true);
    addConstantWord(mach, "number-variant-code", 0);
    addConstantWord(mach, "word-variant-code", 1);
    addConstantWord(mach, "native-function-variant-code", 2);
    addConstantWord(mach, "string-variant-code", 3);
    addConstantWord(mach, "supports-boolean", 
#ifdef ALLOW_BOOLEAN
            true
#else
            false
#endif // end ALLOW_BOOLEAN
            );
	addConstantWord(mach, "supports-floating-point",
#ifdef ALLOW_FLOATING_POINT
			true
#else
			false
#endif  // end ALLOW_FLOATING_POINT
			);
#define X(name, op)
#define Y(name, op, type) mach.addWord(#op INDIRECTION(YTypeString, type) , callBinaryNumberOperation( name < INDIRECTION(YType, type) >));
#include "BinaryOperators.def"
#undef X
}
int main() {
    Machine mach;
    setupDictionary(mach);
    while (keepExecuting) {
        try {
            if (auto str = mach.readNext() ; !str.empty()) {
                if (ignoreInput) {
                    ignoreInput = (str != ")");
                    continue;
                } else {
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
                }
            }
            if (!mach.currentlyCompiling() && 
                    !ignoreInput) {
                mach.getOutput() << " ok" << std::endl;
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

#undef YTypeStringFloat
#undef YTypeStringAddress
#undef YTypeStringbool
#undef YTypeStringInteger
#undef YTypeFloat
#undef YTypeAddress
#undef YTypebool
#undef YTypeInteger
