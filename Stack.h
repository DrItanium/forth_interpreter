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

#ifndef STACK_H__
#define STACK_H__
#include "Types.h"
#include "Entry.h"
#include "Problem.h"
#include "Number.h"
#include <iostream>

namespace forth {
	class Stack {
		public:
			Stack(Number* base = nullptr, Number* full = nullptr) : _base(base), _full(full) { }
			void setup(Number* base, Number* full) noexcept {
				_base = base;
				_full = full;
				clear();
			}
			void clear() noexcept {
				_curr = _base;
			}
			bool empty() const noexcept { return _base == _curr; }
			bool full() const noexcept { return _base == _full; }
			Address depth() const noexcept { return _curr - _base; }
			Address capacity() const noexcept { return _full - _base; }
			void push(Number value);
			Number pop();
			void dup(); // ( a -- a a )
			void dupIfNotZero(); // ( a -- a a | 0 )
			void dup2(); // ( a b -- a b a b )
			void swap(); // ( a b -- b a )
			void rot(); // ( a b c -- b c a )
			void rotMinus(); // ( a b c -- c a b )
			void over(); // ( a b -- a b a )
			void drop(); // ( a -- )
			void drop2(); // ( a b -- )
			void pushDepth() { 
				// ( -- n )
				push(depth()); 
			}
			void nip() {
				// ( a b -- b )
				swap();
				drop();
			}
			void tuck() {
				// ( a b -- b a b )
				swap();
				over();
			}
			/**
			 * Throw if the expected stack depth is not there!
			 */
			void expectStackDepthAtLeast(Address value);
			void showStack(std::ostream& out = std::cout);
		private:
			Number* _base;
			Number* _full;
			Number* _curr = nullptr;
	};
} // end namespace forth

#endif // end STACK_H__
