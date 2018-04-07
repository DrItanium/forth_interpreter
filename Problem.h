#ifndef PROBLEM_H__
#define PROBLEM_H__
// concept of a problem to be thrown
#include <string>
namespace forth {
    class Problem {
        public:
            Problem(const std::string& message);
            ~Problem() = default;
            const std::string& getMessage() const noexcept { return _message; }
        private:
            std::string _message;
    };
} // end namespace forth

#endif
