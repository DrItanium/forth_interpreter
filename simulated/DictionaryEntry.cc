#include <list>
#include "Datum.h"
#include "DictionaryEntry.h"
#include "Problem.h"
#include "Machine.h"
namespace forth {
	void DictionaryEntry::operator()(Machine* machine) const {
		if (_code != nullptr) {
			_code(machine);
		} else {
			try {
				for (const auto & value : _space) {
					value(machine);
				}
			} catch (Problem& p) {
				throw Problem(getName(), p.getMessage());
			}
		}
	}

	void DictionaryEntry::wordToInvoke(const DictionaryEntry* x) {
		SpaceEntry se;
        se._data = Invokable(x, [x](Machine* m) { x->operator()(m); });
		_space.emplace_back(se);
	}

	void DictionaryEntry::wordToPush(const DictionaryEntry* x) {
		SpaceEntry se;
        se._data = x;
		_space.emplace_back(se);
	}
    void DictionaryEntry::addSpaceEntry(const std::string& value) {
        SpaceEntry se;
        se._data = &value;
        _space.emplace_back(se);
    }


	DictionaryEntry::DictionaryEntry(const std::string& name, NativeMachineOperation code) : _name(name), _code(code), _next(nullptr) { }
	void DictionaryEntry::SpaceEntry::operator()(Machine* machine) const {
		invoke(machine);
	}
	void DictionaryEntry::SpaceEntry::invoke(Machine* machine) const {
        if (_data.valueless_by_exception()) {
            throw Problem("SpaceEntry::invoke", "variant is empty!");
        } else {
            std::visit(overloaded {
                    [machine](auto arg) { machine->pushParameter(arg); },
                    [machine](Invokable fn) { std::get<1>(fn)(machine); },
                    }, _data);
        }
	}
} // end namespace forth
