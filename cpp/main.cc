#include "../Types.h"
#include "../Problem.h"
#include <functional>
#include <string>
#include <list>
#include <optional>
#include <variant>
#include <stack>

class DictionaryEntry;

class Machine;
using NativeFunction = std::function<void(Machine&)>;
using Datum = std::variant<forth::Integer, forth::Floating, forth::Address, bool, DictionaryEntry*, NativeFunction, std::string>;
using Stack = std::stack<Datum, std::list<Datum>>;

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
        std::optional<DictionaryEntry*> lookupWord(const std::string&);
    private:
        DictionaryEntry* _front;
        Stack _parameter;
        Stack _subroutine;
};

class DictionaryEntry {
    public:
        explicit DictionaryEntry(const std::string& name) : _name(name) { }
        ~DictionaryEntry();
        void setNext(DictionaryEntry* next) noexcept { _next = next; }
        DictionaryEntry* getNext() const noexcept { return _next; }
        bool hasNext() const noexcept { return _next != nullptr; }
        std::optional<DictionaryEntry*> findWord(const std::string& name);
        void addWord(NativeFunction);
        void addWord(DictionaryEntry*);
        void addWord(forth::Integer);
        void addWord(forth::Floating);
        void addWord(const std::string&);
        void addWord(forth::Address);
        void addWord(bool);
        void invoke(Machine& machine);
    private:
        DictionaryEntry* findWordInternal(const std::string& name);
    private:
        std::string _name;
        DictionaryEntry* _next;
        std::list<NativeFunction> _contents;
};

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
    if (_name == name) {
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


Machine::Machine() : _front(nullptr) { }
Machine::~Machine() {
    if (_front) {
        delete _front;
        _front = nullptr;
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
        throw forth::Problem("popParameter", "Stack Empty!");
    } else {
        auto top = _parameter.top();
        _parameter.pop();
        return top;
    }
}

Datum Machine::popSubroutine() {
    if (subroutineStackEmpty()) {
        throw forth::Problem("popSubroutine", "Stack Empty!");
    } else {
        auto top = _subroutine.top();
        _subroutine.pop();
        return top;
    }
}

int main() {

    return 0;
}
