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
#ifndef DICTIONARY_H__
#define DICTIONARY_H__
#include "Types.h"
#include "Entry.h"
#include <string>
namespace forth {
	class DictionaryEntry {
		public:
			DictionaryEntry(byte* ptr = nullptr) : _container(ptr) { }
			Entry& getContainer() noexcept { return _container; }
		private:
			Address _current;
			Entry _container;
	};
	class Dictionary {
		public:
			Dictionary(byte* start = nullptr, byte* end = nullptr) : _start(start), _end(end), _front(nullptr), _compile(start) { }
			void setup(byte* start, byte* end) noexcept {
				_start = start;
				_end = end;
				_compile = start;
				_front = nullptr;
			}
			void clear() noexcept {
				_front = nullptr;
				_compile = _start;
			}
			bool empty() const noexcept { return _front == nullptr; }
			DictionaryEntry getCompile() noexcept;
			DictionaryEntry getFront() noexcept;
		private:
			byte* _start = nullptr;
			byte* _end = nullptr;
			byte* _front = nullptr;
			byte* _compile = nullptr;
	};
} // end namespace forth
#endif // end DICTIONARY_H__

