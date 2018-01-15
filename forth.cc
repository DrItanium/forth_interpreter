#include <iostream>
#include <string>

std::string readWord(std::istream& input) {
    std::string word;
    input >> word;
    return word;
}
int main() {
    auto execute = true;
    while (execute) {
        auto result = readWord(std::cin);
        if (result == "quit") {
            execute = false;
        } else {
            std::cout << "word: " << result << std::endl;
        }
    }
    return 0;
}
