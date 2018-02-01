#include <iostream>
#include "Machine.h"


// TODO: add support for invoking some keywords at compile time!
int main() {
	forth::Machine machine (std::cout, std::cin);
	machine.controlLoop();
	return 0;
}
