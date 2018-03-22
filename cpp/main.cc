#include "../Types.h"
#include "../Problem.h"
#include <functional>
#include <string>
#include <list>
#include <optional>
#include <variant>
#include <stack>
#include <memory>
#include <iostream>
#include <fstream>
#include <sstream>

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

template<typename T>
using GenericStack = std::stack<T, std::list<T>>;
using Problem = forth::Problem;
using Word = std::shared_ptr<DictionaryEntry>;
using OptionalWord = std::optional<Word>;
using NativeFunction = std::function<void(Machine&)>;
using Datum = std::variant<forth::Integer, forth::Floating, forth::Address, bool, Word, NativeFunction, std::string>;
using UnaryOperation = std::function<Datum(Machine&, Datum)>;
using BinaryOperation = std::function<Datum(Machine&, Datum, Datum)>;
using Stack = GenericStack<Datum>;
using InputStack = GenericStack<std::istream*>;
using OutputStack = GenericStack<std::ostream*>;

class Machine {
    public:
        explicit Machine();
        ~Machine();
        void pushParameter(forth::Integer integer);
        void pushParameter(forth::Floating fp);
        void pushParameter(forth::Address addr);
        void pushParameter(bool truth);
        void pushParameter(DictionaryEntry* ent);
        void pushParameter(NativeFunction fn);
        void pushParameter(const std::string&);
        void pushParameter(Datum d);
        bool parameterStackEmpty() const noexcept { return _parameter.empty(); }
        void pushSubroutine(forth::Integer integer);
        void pushSubroutine(forth::Floating fp);
        void pushSubroutine(forth::Address addr);
        void pushSubroutine(bool truth);
        void pushSubroutine(DictionaryEntry* ent);
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
        bool currentlyCompiling() const noexcept { return _compile != nullptr; }
        std::string readNext();
        void errorOccurred() noexcept;
        void addWord(const std::string& name, NativeFunction fn, bool fake = false, bool compileTimeInvoke = false);
        std::ostream& getOutput() const noexcept { return *_out; }
        void openFileForOutput(const std::string& path);
        void openFileForInput(const std::string& path);
        void closeFileForOutput(const std::string& path);
        void closeFileForInput(const std::string& path);
    private:
        std::ostream* _out = &std::cout;
        std::istream* _in = &std::cin;
        OptionalWord _front;
        OptionalWord _compile;
        Stack _parameter;
        Stack _subroutine;
        InputStack _inputs;
        OutputStack _outputs;
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
    _outputs.push(_out);
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
    _inputs.push(_in);
    _in = tmp;
}
void Machine::closeFileForOutput(const std::string& path) {
    if (_out != &std::cout) {
        std::ofstream* tmp = (std::ofstream*)_out;
        tmp->close();
        if (_outputs.empty()) {
            throw Problem("closeFileForOutput", "Stack underflow");
        } else {
            delete tmp;
            _out = _outputs.top();
            _outputs.pop();
        }
    }
}

void Machine::closeFileForInput(const std::string& path) {
    if (_in != &std::cin) {
        std::ifstream* tmp = (std::ifstream*)_out;
        tmp->close();
        if (_inputs.empty()) {
            throw Problem("closeFileForInput", "Stack underflow");
        } else {
            delete tmp;
            _in = _inputs.top();
            _inputs.pop();
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
        void addWord(DictionaryEntry*);
        void addWord(forth::Integer);
        void addWord(forth::Floating);
        void addWord(const std::string&);
        void addWord(forth::Address);
        void addWord(bool);
        void invoke(Machine& machine);
        bool isFake() const noexcept { return _fake; }
        void setFake(bool value) noexcept { _fake = value; }
        bool compileTimeInvokable() const noexcept { return _compileTimeInvoke; }
        void setCompileTimeInvokable(bool value) noexcept { _compileTimeInvoke = value; }
        bool matches(const std::string& name);
    private:
        std::string _name;
        bool _fake, _compileTimeInvoke;
        OptionalWord _next;
        std::list<NativeFunction> _contents;
};
bool DictionaryEntry::matches(const std::string& name) {
    return !_fake && (name == _name);
}

void Machine::newCompilingWord(const std::string& str) {
    // this can leak but it is the most straight forward
    auto entry = std::make_shared<DictionaryEntry>(str);
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
    if (!currentlyCompiling()) {
        throw Problem("saveCurrentlyCompilingWord", "Not in compilation mode!");
    } else {
        pushSubroutine(*_compile);
    }
}
void Machine::restoreCurrentlyCompilingWord() {
    if (!currentlyCompiling()) {
        throw Problem("restoreCurrentlyCompilingWord", "Not in compilation mode!");
    } else {
        _compile = std::get<Word>(popSubroutine());
    }
}

Machine::Machine() { }
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

void Machine::pushParameter(Datum d) { _parameter.push(d); }
void Machine::pushParameter(forth::Integer v) { _parameter.push(v); }
void Machine::pushParameter(forth::Floating v) { _parameter.push(v); }
void Machine::pushParameter(forth::Address v) { _parameter.push(v); }
void Machine::pushParameter(bool v) { _parameter.push(v); }
void Machine::pushParameter(DictionaryEntry* v) { _parameter.push(v); }
void Machine::pushParameter(NativeFunction fn) { _parameter.push(fn); }
void Machine::pushParameter(const std::string& str) { _parameter.push(str); }

void Machine::pushSubroutine(Datum d) { _subroutine.push(d); }
void Machine::pushSubroutine(forth::Integer v) { _subroutine.push(v); }
void Machine::pushSubroutine(forth::Floating v) { _subroutine.push(v); }
void Machine::pushSubroutine(forth::Address v) { _subroutine.push(v); }
void Machine::pushSubroutine(bool v) { _subroutine.push(v); }
void Machine::pushSubroutine(DictionaryEntry* v) { _subroutine.push(v); }
void Machine::pushSubroutine(const std::string& str) { _subroutine.push(str); }

Datum Machine::popParameter() {
    if (parameterStackEmpty()) {
        throw Problem("popParameter", "Stack Empty!");
    } else {
        auto top = _parameter.top();
        _parameter.pop();
        return top;
    }
}

Datum Machine::popSubroutine() {
    if (subroutineStackEmpty()) {
        throw Problem("popSubroutine", "Stack Empty!");
    } else {
        auto top = _subroutine.top();
        _subroutine.pop();
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
void DictionaryEntry::addWord(DictionaryEntry* dict) {
    addWord([dict](Machine& mach) { dict->invoke(mach); });
}



void DictionaryEntry::addWord(forth::Integer v) {
    addWord([v](Machine& mach) { mach.pushParameter(v); });
}
void DictionaryEntry::addWord(forth::Floating v) {
    addWord([v](Machine& mach) { mach.pushParameter(v); });
}
void DictionaryEntry::addWord(forth::Address v) {
    addWord([v](Machine& mach) { mach.pushParameter(v); });
}

void DictionaryEntry::addWord(bool v) {
    addWord([v](Machine& mach) { mach.pushParameter(v); });
}

NativeFunction pushToParameterStack(DictionaryEntry* entry) {
    return [entry](Machine& mach) { mach.pushParameter(entry); };
}
NativeFunction pushToSubroutineStack(DictionaryEntry* entry) {
    return [entry](Machine& mach) { mach.pushSubroutine(entry); };
}

void semicolon(Machine& mach) {
    if (!mach.currentlyCompiling()) {
        throw Problem(";", "Not currently compiling!");
    } else {
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
    if (!_parameter.empty()) {
        Stack temp;
        _parameter.swap(temp);
    }
    if (!_subroutine.empty()) {
        Stack temp;
        _subroutine.swap(temp);
    }
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
void drop(Machine& mach) { mach.popParameter(); }
void swap(Machine& mach) { 
    auto top = mach.popParameter();
    auto lower = mach.popParameter();
    mach.pushParameter(top);
    mach.pushParameter(lower);
}
// binary operations
#define X(name, op) \
    template<typename T> \
    Datum name ( Machine& mach, Datum a, Datum b) { \
        try { \
            Datum ret; \
            auto f = std::get<T>(a); \
            auto s = std::get<T>(b); \
            auto result = (f op s); \
            ret = result; \
            return ret; \
        } catch (std::bad_variant_access& a) { \
            throw Problem(#name , a.what()); \
        } \
    } \
    template<typename T> \
    void name ( Machine& mach ) { \
        auto top = mach.popParameter(); \
        auto lower = mach.popParameter(); \
        mach.pushParameter( name <T> (mach, lower, top) ); \
    }
#define Y(name, op, type) 
#include "BinaryOperators.def"
#undef Y
#undef X
template<>
Datum logicalXor<bool>(Machine& mach, Datum a, Datum b) {
    try {
        Datum ret;
        auto f = std::get<bool>(a);
        auto s = std::get<bool>(b);
        ret = f != s;
        return ret;
    } catch (std::bad_variant_access& a) {
        throw Problem("logicalXor", a.what());
    }
}

int main() {
    Machine mach;
    mach.addWord("drop", drop);
    mach.addWord("swap", swap);
#define X(name, op)
#define Y(name, op, type) mach.addWord(#op INDIRECTION(YTypeString, type) , [](Machine& mach) { name < INDIRECTION(YType, type) >(mach); } );
#include "BinaryOperators.def"
#undef X
    mach.addWord("^b", [](Machine& mach) { logicalXor<bool>(mach); });
    mach.addWord("(", enterIgnoreInputMode);
    bool ignoreInput = false;
    while (keepExecuting) {
        try {
            auto str = mach.readNext();
            if (ignoreInput) {
               if (str == ")") {
                   ignoreInput = false;
                   continue;
               }
            } else {

            }
        } catch (Problem& p) {
            // clear out the stacks as well
            mach.errorOccurred();
            std::cerr << p.getWord() << p.getMessage() << std::endl;
        } catch (std::bad_variant_access& a) {
            mach.errorOccurred();
            std::cerr << "Uncaught bad variant: " << a.what() << std::endl;
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
