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
#include <sstream>

using byte = uint8_t;
using Number = int64_t;
using Address = uint64_t;

constexpr bool getTruth(Number number) noexcept {
	return number != 0;
}
constexpr auto capacity = 0x10000;
byte* memory = nullptr;
constexpr auto inputStringStart = 0x0700; // 31 characters
constexpr auto inputStringEnd = 0x071F;
constexpr auto outputStringStart = 0x0720; // 127 characters
constexpr auto outputStringEnd = 0x07A0;
constexpr auto systemVariablesStart = 0x0800;
constexpr auto systemVariableEnd = 0x0A00;
constexpr auto parameterStackTop = 0x0A00;
constexpr auto parameterStackBottom = 0x0B00;
constexpr auto subroutineStackTop = 0x0B00;
constexpr auto subroutineStackBottom = 0x0C00;

std::string _input;

void setupMemory() {
	memory = new byte[capacity];
}
void installOutputString(const std::string& msg) {
	if (msg.size() >= 127) {
		memory[outputStringStart] = 127;
		for (int i = 0, j = outputStringStart + 1; i < 127; ++i, ++j) {
			memory[j] = msg[i];
		}
	} else {
		memory[outputStringStart] = msg.size();
		for (int i = 0, j = outputStringStart + 1; i < msg.size(); ++i, ++j) {
			memory[j] = msg[i];
		}
	}
}
void installInputString(const std::string& msg) {
	if (msg.size() >= 31) {
		memory[inputStringStart] = 31;
		for (int i = 0, j = inputStringStart + 1; i < 31; ++i, ++j) {
			memory[j] = msg[i];
		}
	} else {
		memory[inputStringStart] = msg.size();
		for (int i = 0, j = inputStringStart + 1; i < msg.size(); ++i, ++j) {
			memory[j] = msg[i];
		}
	}
}
void raiseError(const std::string& msg) {
	memory[systemVariablesStart] = 0xFF;
	installOutputString(msg);
}
bool inRange(Address addr) noexcept {
	return addr < capacity;
}
void writeMemory(Address addr, byte value) noexcept {
	if (!inRange(addr)) {
		raiseError("!bad address");
		return;
	} else {
		memory[addr] = value;
	}
}
byte readMemory(Address addr) noexcept {
	if (!inRange(addr)) {
		raiseError("!bad address");
		return 0;
	} else {
		return memory[addr];
	}
}

bool readFlag(Address addr) noexcept {
	return readMemory(addr) != 0;
}
Address readNumber16(Address addr) noexcept {
	auto lower = readMemory(addr);
	auto upper = readMemory(addr + 1);
	return Address(lower) | (Address(upper) << 8);
}
Address readNumber32(Address addr) noexcept {
	auto lower = readNumber16(addr);
	auto upper = readNumber16(addr + (sizeof(Address) / 4));
	return lower | (upper << 16);
}

Address readAddress(Address addr) noexcept {
	auto lower = readNumber32(addr);
	auto upper = readNumber32(addr + (sizeof(Address) / 2));
	return lower | (upper << 32);
}
Address systemVariable(Address offset) noexcept {
	return (systemVariables - memory) + offset;
}

bool errorHappened() noexcept {
	return readFlag(systemVariable(0));
}
bool keepExecuting() noexcept {
	return readFlag(systemVariable(1));
}
bool isCompiling() noexcept {
	return readFlag(systemVariable(2));
}
Address parameterBottom() noexcept {
	return readAddress(systemVariable(8));
}
Address parameterTop() noexcept {
	return readAddress(systemVariable(16));
}
byte getDictionaryControlByte(Address addr) {
	return readMemory(addr);
}
byte getDictionaryNameLength(Address addr) {
	auto result = getDictionaryControlByte(addr);
	return result & 0b00011111;
}
bool entryIsFake(Address addr) {
	auto result = getDictionaryControlByte(addr);
	return (result & 0b00100000) != 0;
}
bool invokeAtCompileTime(Address addr) {
	auto result = getDictionaryControlByte(addr);
	return (result & 0b01000000) != 0;
}
char* getDictionaryStringStart(Address addr) {
	if (!inRange(addr)) {
		raiseError("!bad address");
		return nullptr;
	} else {
		return (char*)(&memory[addr + 1]);
	}
}
void loadDictionaryName(Address addr) {
	auto length = getDictionaryNameLength(addr);
	if (!errorHappened()) {
		auto str = getDictionaryStringStart(addr);
		_output.assign(str, length);
	}
}
void pushParameter(Number n) {

}
void readNext() noexcept {
	std::cin >> _input;
}
void number() noexcept {
	if (_input[0] == '0' && _input[1] == 'x') {
		auto copy = _input;
		copy[1] = 0;
		std::istringstream input(copy);
		Number n;
		input >> std::hex >> n;
		if (!input.fail() && input.eof()) {
			pushParameter(n);
			return;
		}
	} else {
		Number n;
		std::istringstream input(_input);
		input >> n;
		if (!input.fail() && input.eof()) {
			pushParameter(n);
			return;
		}
	}

	std::stringstream ss;
	ss << _input << "?";
	auto str = ss.str();
	raiseError(str);
}

void controlLoop() {
	while (keepExecuting()) {

		if (errorHappened()) {
			std::cout << _output << std::endl;

		}
	}
}



