#include "../Types.h"
#include "../Problem.h"
#include <functional>
#include <string>
#include <list>
#include <optional>
#include <variant>
#include <stack>
#include <memory>

class DictionaryEntry;
class Machine;

template<typename T>
using GenericStack = std::stack<T, std::list<T>>;
using Problem = forth::Problem;
using NativeFunction = std::function<void(Machine&)>;
using Word = std::shared_ptr<DictionaryEntry>;
using OptionalWord = std::optional<Word>;
using Datum = std::variant<forth::Integer, forth::Floating, forth::Address, bool, Word, NativeFunction, std::string>;
using Stack = GenericStack<Datum>;
using InputStack = GenericStack<std::istream>;
using OutputStack = GenericStack<std::ostream>;

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
    private:
        OptionalWord _front;
        OptionalWord _compile;
        Stack _parameter;
        Stack _subroutine;
};


class DictionaryEntry {
    public:
        explicit DictionaryEntry(const std::string& name) : _name(name), _fake(false), _compileTimeInvoke(false) { }
        ~DictionaryEntry();
        void setNext(Word next) noexcept { _next = next; }
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
    private:
        std::string _name;
        bool _fake, _compileTimeInvoke;
        OptionalWord _next;
        std::optional<Word> _next;
        std::list<NativeFunction> _contents;
};


void Machine::newCompilingWord(const std::string& str) {
    // this can leak but it is the most straight forward
    _compile = new DictionaryEntry(str);
}

void Machine::compileCurrentWord() {
    if (!currentlyCompiling()) {
        throw Problem("compileCurrentWord", "Not in compilation mode!");
    } else {
        _compile->setNext(_front);
        _front = _compile;
        _compile = nullptr;
    }
}
void Machine::saveCurrentlyCompilingWord() {
    if (!currentlyCompiling()) {
        throw Problem("saveCurrentlyCompilingWord", "Not in compilation mode!");
    } else {
        pushSubroutine(_compile);
    }
}
void Machine::restoreCurrentlyCompilingWord() {
    if (!currentlyCompiling()) {
        throw Problem("restoreCurrentlyCompilingWord", "Not in compilation mode!");
    } else {
        _compile = std::get<DictionaryEntry*>(popSubroutine());
    }
}

Machine::Machine() : _front(nullptr), _compile(nullptr) { }
Machine::~Machine() {
    if (_front) {
        delete _front;
        _front = nullptr;
    }
    if (_compile) {
        delete _compile;
        _compile = nullptr;
    }
}

OptionalWord Machine::lookupWord(const std::string& str) {
    if (_front) {
        return _front->findWord(str);
    } else {
        return std::optional<DictionaryEntry*>();
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

DictionaryEntry::~DictionaryEntry() {
    if (_next) {
        delete _next;
        _next = nullptr;
    }
}

DictionaryEntry* DictionaryEntry::findWordInternal(const std::string& name) {
    if (~_fake && (_name == name)) {
        return this;
    } else if (hasNext()) {
        return _next->findWordInternal(name);
    } else {
        return nullptr;
    }
}

std::optional<DictionaryEntry*> DictionaryEntry::findWord(const std::string& name) {
    std::optional<DictionaryEntry*> out;
    auto result = findWordInternal(name);
    if (result) {
        out = result;
    }
    return out;
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
        delete _compile;
        _compile = nullptr;
    }
}

int main() {
    Machine mach;
    bool ignoreInput = false;
    while (keepExecuting) {
        try {
            auto str = mach.readNext();
            if (ignoreInput) {
                
            }
        } catch (Probem& p) {
            // clear out the stacks as well
            mach.errorOccurred();
            std::cerr << p.getWord() << p.getMessage() << std::endl;
        }
    }
    return 0;
}
