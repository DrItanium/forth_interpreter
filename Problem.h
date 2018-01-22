// concept of a problem to be thrown
#include <string>
namespace forth {
    class Problem {
        public:
            Problem(const std::string& word, const std::string& message);
            ~Problem() = default;
            const std::string& getWord() const noexcept { return _word; }
            const std::string& getMessage() const noexcept { return _message; }
        private:
            std::string _word;
            std::string _message;
    };
} // end namespace forth
