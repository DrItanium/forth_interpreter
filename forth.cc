#include <iostream>
#include <string>

class DictionaryEntry {
    public:
        DictionaryEntry() = default;
        ~DictionaryEntry() = default;
};
std::string readWord(std::istream& input) {
    std::string word;
    input >> word;
    return word;
}
DictionaryEntry& getNullEntry() noexcept {
    static DictionaryEntry nullEntry;
    return nullEntry;
}
DictionaryEntry& lookupWord(const std::string& word) noexcept {
    return getNullEntry();
}

int main() {
    while (true) {
        auto result = readWord(std::cin);
        if (result == "quit") {
            break;
        } else {
            auto& entry = lookupWord(result);
            if (&entry == &getNullEntry()) {
                std::cout << "Unknown word: " << result << std::endl;
                continue;
            }
            std::cout << "word: " << result << std::endl;
        }
    }
    return 0;
}
