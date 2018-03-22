#include "../Types.h"
#include "../Problem.h"
#include <functional>
#include <string>
#include <list>
#include <optional>

class Machine;
class DictionaryEntry {
    public:
        using NativeFunction = std::function<void(Machine&)>;
    public:
        DictionaryEntry(const std::string& name);
        void setNext(DictionaryEntry* next) noexcept { _next = next; }
        DictionaryEntry* getNext() const noexcept { return _next; }
        bool hasNext() const noexcept { return _next != nullptr; }
        std::optional<DictionaryEntry*> findWord(const std::string& name);
        void addEntry(NativeFunction);
        void addEntry(DictionaryEntry*);
        void addEntry(forth::Integer);
        void addEntry(forth::Floating);
        void addEntry(const std::string&);
        void addEntry(forth::Address);
        void addEntry(bool);
        void invoke(Machine& machine);
    private:
        DictionaryEntry* findWordInternal(const std::string& name);
    private:
        std::string _name;
        DictionaryEntry* _next;
        std::list<NativeFunction> _contents;
};

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



class Machine {
    public:
    private:
        DictionaryEntry* _front;
};

int main() {

    return 0;
}
