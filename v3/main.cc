/*
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in 
 * the Software without restriction, including without limitation the rights to 
 * use, copy, modify, merge, publish, distribute, and/or sell copies of the 
 * Software, and to permit persons to whom the Software is furnished to do so.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT OF THIRD PARTY RIGHTS. IN NO EVENT 
 * SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, OR ANY SPECIAL INDIRECT OR 
 * CONSEQUENTIAL DAMAGES, OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, 
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS 
 * SOFTWARE.
 */

#include <iostream>
#include <cstdint>
#include <functional>
#include <string>

using byte = uint8_t;
using Integer = int64_t;

using Number = Integer;

constexpr bool getTruth(Number number) noexcept {
	return number != 0;
}

byte* memory = nullptr;
byte* parameterStart = nullptr;
byte* subroutineStart = nullptr;
byte* dictionaryStart = nullptr;
byte* variables = nullptr;
byte* dictionaryFront = nullptr;
byte* systemVariables = nullptr;

std::string _input;
std::string _output;

void raiseError(const std::string& msg) {
	_output = msg;
	systemVariables[0] = 0xFF;
}
bool errorHappened() noexcept {
	return systemVariables[0] != 0;
}


