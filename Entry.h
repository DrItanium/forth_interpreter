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
#ifndef ENTRY_H__
#define ENTRY_H__
#include "Types.h"
#include <string>
namespace forth {
	/**
	 * A way to view a byte stream, it is not deleted by the class on
	 * destruction.
	 */
	class Entry {
		public:
			Entry() : _base(nullptr) { }
			Entry(byte* start) : _base(start) { }
			Entry(const Entry& other) : _base(other._base) { }
			Entry(Entry&& other) : _base(std::move(other._base)) { }
			~Entry() { _base = nullptr; }
			const byte* data() const noexcept { return _base; }
			byte* data() noexcept { return _base; }
			bool valid() const noexcept { return _base != nullptr; }
			Address relativeTo(byte* start) const noexcept { return _base - start; }
			byte at(Address index) const noexcept { return _base[index]; }
			byte& at(Address index) noexcept { return _base[index]; }
			byte operator[](Address index) const noexcept { return at(index); }
			byte& operator[](Address index) noexcept { return at(index); }
			byte& first() noexcept { return *_base; }
			byte first() const noexcept { return *_base; }
			template<typename T>
			T* viewAs(Address offset = 0) noexcept {
				return reinterpret_cast<T*>(_base + offset);
			}
			template<typename T>
			const T* viewAs(Address offset = 0) const noexcept {
				return reinterpret_cast<const T*>(_base + offset);
			}
			void assignToString(std::string& storage, Address length, Address offset = 0) const noexcept;
		private:
			byte* _base;
	};
} // end namespace forth
#endif // end ENTRY_H__

