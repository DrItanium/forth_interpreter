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
#ifndef NUMBER_H__
#define NUMBER_H__
#include "Types.h"
#include "Entry.h"
#include <string>
namespace forth {
	using Function = void(*)();
	union Number {
		public:
			Number() : _ptr(nullptr) { }
			Number(byte* ptr) : _ptr(ptr) { }
			Number(Function func) : _func(func) { }
			Number(Address addr) : _addr(addr) { }
			Number(Integer i) : _int(i) { }
			Number(bool b) : _int(b ? -1 : 0) { }
			Number(const Number& other) : _ptr(other._ptr) { }
			Number(Number&& other) : _ptr(std::move(other._ptr)) { }
			~Number() { _ptr = nullptr; }
			bool getTruth() const noexcept { return _int != 0; }
			Function getFunction() const noexcept { return _func; }
			Address getAddress() const noexcept { return _addr; }
			Integer getInteger() const noexcept { return _int; }
			byte* getPointer() const noexcept { return _ptr; }
			void setPointer(byte* ptr) noexcept { _ptr = ptr; }
			void setPointer(char* ptr) noexcept { _ptr = reinterpret_cast<byte*>(ptr); }
			void setPointer(Entry e) noexcept { setPointer(e.data()); }
			void setFunction(Function func) noexcept { _func = func; }
			void setAddress(Address addr) noexcept { _addr = addr; }
			void setInteger(Integer i) noexcept { _int = i; }
			void setTruth(bool b) noexcept { _int = b ? -1 : 0; }
			template<typename T>
			T get() noexcept {
				using K = std::decay_t<T>;
				if constexpr (std::is_same_v<K, byte*>) {
					return getPointer();
				} else if constexpr (std::is_same_v<K, Integer>) {
					return getInteger();
				} else if constexpr (std::is_same_v<K, Address>) {
					return getAddress();
				} else if constexpr (std::is_same_v<K, Function>) {
					return getFunction();
				} else if constexpr (std::is_same_v<K, bool>) {
					return getTruth();
				} else if constexpr (std::is_same_v<K, char*>) {
					return reinterpret_cast<char*>(getPointer());
				} else if constexpr (std::is_same_v<K, Entry>) {
					return Entry(_ptr);
				} else {
					static_assert(AlwaysFalse<T>::value, "Unsupported acquisition target!");
				}
			}
			template<typename T>
			void set(T value) noexcept {
				using K = std::decay_t<T>;
				if constexpr (std::is_same_v<K, byte*>) {
					setPointer(value);
				} else if constexpr (std::is_same_v<K, Integer>) {
					setInteger(value);
				} else if constexpr (std::is_same_v<K, Address>) {
					setAddress(value);
				} else if constexpr (std::is_same_v<K, Function>) {
					setFunction(value);
				} else if constexpr (std::is_same_v<K, bool>) {
					setTruth(value);
				} else if constexpr (std::is_same_v<K, char*>) {
					setPointer(value);
				} else if constexpr (std::is_same_v<K, Entry>) {
					setPointer(value);
				} else {
					static_assert(AlwaysFalse<T>::value, "Unsupported assignment target!");
				}
			}
			void absorb(const Number& other) noexcept { _ptr = other.getPointer(); }
		private:
			Function _func;
			Address _addr;
			Integer _int;
			byte* _ptr;
	};
} // end namespace forth
#endif // end NUMBER_H__

