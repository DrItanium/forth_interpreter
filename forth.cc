#include <iostream>
#include <string>
#include <stack>
#include <list>
#include <cstdint>
#include <memory>
#include <limits>
#include <sstream>

namespace forth {
    using Address = uint32_t;
    using Integer = int32_t;
    using Floating = float;

    union Datum {
        Integer numValue;
        Address address;
        Floating fp;
    };
    class DictionaryEntry {
        public:
            DictionaryEntry() = default;
            DictionaryEntry(const std::string& name);
            ~DictionaryEntry() = default;
            const std::string& getName() const noexcept { return _name; }
        private:
            std::string _name;
    };

    class Dictionary : public std::list<DictionaryEntry> {
        public:
            using Parent = std::list<DictionaryEntry>;
            using Self = Dictionary;
            static const DictionaryEntry& getNullEntry() noexcept {
                static DictionaryEntry nullEntry;
                return nullEntry;
            }
        public:
            using Parent::Parent;
            const DictionaryEntry& lookupWord(const std::string& word) noexcept;
    };


    template<typename T = int>
    using Stack = std::stack<T, std::list<T>>;

    DictionaryEntry::DictionaryEntry(const std::string& name) : _name(name) { }

    std::string readWord(std::istream& input) {
        std::string word;
        input >> word;
        return word;
    }
    const DictionaryEntry& Dictionary::lookupWord(const std::string& word) noexcept {
        return getNullEntry();
    }

    class Machine {
        public:
            static bool isNullEntry(const DictionaryEntry& entry) noexcept {
                return &entry == &(Dictionary::getNullEntry());
            }
            static constexpr auto largestAddress = 0xFFFFFF;
            static constexpr auto memoryCapacity = (largestAddress + 1);
        public:
            Machine(std::istream& input);
            ~Machine() = default;
            const DictionaryEntry& lookupWord(const std::string& word) noexcept {
                return _words.lookupWord(word);
            }
            void controlLoop() noexcept;
            void handleError(const std::string& word, const std::string& msg) noexcept;
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
        private:
            bool numberRoutine(const std::string& word) noexcept;
        private:
            std::istream& _input;
            std::unique_ptr<Integer[]> _memory;
            Dictionary _words;
            Stack<Address> _subroutine;
            Stack<Datum> _parameter;
    };

    Machine::Machine(std::istream& input) : _input(input), _memory(new Integer[memoryCapacity]) { }

    Datum Machine::popParameter() {
        auto top = _parameter.top();
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
        std::istringstream parseAttempt(word);
        if (word.find('.') != std::string::npos) {
            Floating tmpFloat;
            parseAttempt >> tmpFloat;
            if (!parseAttempt.fail()) {
#ifdef DEBUG
                std::cout << "floating point number pushed: " << tmpFloat << std::endl;
#endif // end DEBUG
                pushParameter(tmpFloat);
                return true;
            }
        }
        Integer tmpInt;
        parseAttempt.clear();
        parseAttempt >> tmpInt;
        if (!parseAttempt.fail()) {
#ifdef DEBUG
            std::cout << "integer number pushed: " << tmpInt << std::endl;
#endif // end DEBUG
            pushParameter(tmpInt);
            return true;
        }
        return false;
    }
    void Machine::controlLoop() noexcept {
        while (true) {
            auto result = readWord(_input);
            if (result == "quit") {
                break;
            } else {
                if (numberRoutine(result)) {
                    continue;
                }
                auto& entry = lookupWord(result);
                // check and see if were'
                if (forth::Machine::isNullEntry(entry)) {
                    handleError(result, "?");
                    continue;
                }
            }
        }
    }
    void Machine::handleError(const std::string& word, const std::string& msg) noexcept {
        // clear the stacks and the input pointer
        decltype(_subroutine) _purge0;
        decltype(_parameter) _purge1;
        _subroutine.swap(_purge0);
        _parameter.swap(_purge1);
        _input.clear();
        _input.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        std::cout << word << msg << std::endl;
    }
} // end namespace forth


int main() {
    forth::Machine machine (std::cin);
    machine.controlLoop();
    return 0;
}
