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

#include "Stack.h"
#include "Problem.h"

namespace forth {
	void Stack::push(Number value) {
		if (full()) {
			throw Problem("STACK FULL!");
		} 
		++_curr;
		_curr->absorb(value);
	}
	Number Stack::pop() {
		if (empty()) {
			throw Problem("STACK EMPTY!");
		}
		Number curr = *_curr;
		--_curr;
		return curr;
	}
	void Stack::dup() {
		if (empty()) {
			throw Problem("STACK EMPTY!");
		}
		push(*_curr);
	}
	void Stack::swap() {
		expectStackDepthAtLeast(2);
		auto top = *_curr;
		auto lower = *(_curr - 1);
		(_curr - 1)->absorb(top);
		_curr->absorb(lower);
	}
	void Stack::rot() {
		expectStackDepthAtLeast(3);
		auto top = *_curr;
		auto lower = *(_curr - 1);
		auto third = *(_curr - 2);
		_curr->absorb(third);
		(_curr - 1)->absorb(top);
		(_curr - 2)->absorb(lower);
	}
	void Stack::over() {
		expectStackDepthAtLeast(2);
		push(*(_curr - 1));
	}
	void Stack::drop() {
		if (empty()) {
			throw Problem("STACK EMPTY!");
		}
		--_curr;
	}
	void Stack::expectStackDepthAtLeast(Address v) {
		if (depth() < v) {
			throw Problem("STACK UNDERFLOW!");
		}
	}
	void Stack::drop2() {
		expectStackDepthAtLeast(2);
		_curr -= 2;
	}
	void Stack::rotMinus() {
		expectStackDepthAtLeast(3);
		auto top = *_curr;
		auto third = *(_curr - 2);
		_curr->absorb(third);
		(_curr - 2)->absorb(top);
	}
} // end namespace forth
