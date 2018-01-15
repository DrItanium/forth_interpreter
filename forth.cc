//#define DEBUG
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
    using byte = uint8_t;
    static_assert((sizeof(Address) == sizeof(Integer)) && (sizeof(Integer) == sizeof(Floating)), "Address, Integer, and Floating are not equal!");
    enum Discriminant {
        Number,
        MemoryAddress,
        FloatingPoint,
    };
    union Datum {
        Datum() = default;
        ~Datum() = default;
        Datum(const Datum& other);
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
            Machine(std::ostream& output, std::istream& input);
            ~Machine() = default;
            const DictionaryEntry& lookupWord(const std::string& word) noexcept {
                return _words.lookupWord(word);
            }
            void controlLoop() noexcept;
            void handleError(const std::string& word, const std::string& msg) noexcept;
            void dropParameter();
            void swapParameters();
            void duplicateParameter();
            void placeOverParameter();
            const Datum& topParameter();
            const Datum& lowerParameter();
        private:
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
        private:
            std::ostream& _output;
            std::istream& _input;
            std::unique_ptr<Integer[]> _memory;
            Dictionary _words;
            Stack<Address> _subroutine;
            Stack<Datum> _parameter;
    };
    void Machine::duplicateParameter() {
        // a -- a a
        Datum top(_parameter.top());
        _parameter.push(top);
    }
    void Machine::placeOverParameter() {
        // a b -- a b a
        auto top = popParameter();
        Datum lower(_parameter.top());
        _parameter.push(top);
        _parameter.push(lower);
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
            default:
                throw "BAD DISCRIMINANT";
        }
        // always type a space out after the number
        _output << ' ' << std::endl;
    }
    Machine::Machine(std::ostream& output, std::istream& input) : _output(output), _input(input), _memory(new Integer[memoryCapacity]) { }

    Datum Machine::popParameter() {
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
        std::istringstream parseAttempt(word);
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
        _output << word << msg << std::endl;
    }
} // end namespace forth


int main() {
    forth::Machine machine (std::cout, std::cin);
    machine.controlLoop();
    return 0;
}
