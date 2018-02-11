#include "Core.h"

namespace forth {

void Core::setCurrentMolecule(const Molecule& m) {
	_currentMolecule.setValue(m._value);
	_moleculePosition.setValue(0);
}

} // namespace forth
