#include <iostream>
#include <string>
#include <stack>
#include <list>

namespace forth {
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
        public:
            Machine() = default;
            ~Machine() = default;
            const DictionaryEntry& lookupWord(const std::string& word) noexcept {
                return _words.lookupWord(word);
            }
            void controlLoop() noexcept;
        private:
            Dictionary _words;
            Stack<int> _subroutine;
            Stack<int> _parameter;
    };
    void Machine::controlLoop() noexcept {
        while (true) {
            auto result = readWord(std::cin);
            if (result == "quit") {
                break;
            } else {
                auto& entry = lookupWord(result);
                if (forth::Machine::isNullEntry(entry)) {
                    std::cout << "Unknown word: " << result << std::endl;
                    continue;
                }
                std::cout << "word: " << result << std::endl;
            }
        }
    }
} // end namespace forth


int main() {
    forth::Machine machine;
    machine.controlLoop();
    return 0;
}
