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
	void DictionaryEntry::addSpaceEntry(SpaceEntry::Discriminant type, const DictionaryEntry* value) {
		SpaceEntry se;
		se._type = type;
		se._data = value;
		_space.emplace_back(se);
	}

	void DictionaryEntry::pushWord(const DictionaryEntry* value) {
		addSpaceEntry(SpaceEntry::Discriminant::Word, value);
	}

	void DictionaryEntry::addSpaceEntry(Integer x) {
		SpaceEntry se;
		se._type = SpaceEntry::Discriminant::Signed;
		se._data = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(Address x) {
		SpaceEntry se;
		se._type = SpaceEntry::Discriminant::Unsigned;
		se._data = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(Floating x) {
		SpaceEntry se;
		se._type = SpaceEntry::Discriminant::FloatingPoint;
		se._data = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(bool x) {
		SpaceEntry se;
		se._type = SpaceEntry::Discriminant::Boolean;
		se._data = x;
		_space.emplace_back(se);
	}

	void DictionaryEntry::addSpaceEntry(const DictionaryEntry* x) {
		SpaceEntry se;
		se._type = SpaceEntry::Discriminant::DictEntry;
		se._data = x;
		_space.emplace_back(se);
	}


	DictionaryEntry::DictionaryEntry(const std::string& name, NativeMachineOperation code) : _name(name), _code(code), _next(nullptr) { }
	void DictionaryEntry::SpaceEntry::operator()(Machine* machine) const {
		invoke(machine);
	}
	void DictionaryEntry::SpaceEntry::invoke(Machine* machine) const {
		using Type = DictionaryEntry::SpaceEntry::Discriminant;
		switch (_type) {
			case Type::Signed:
				machine->pushParameter(std::get<Integer>(_data));
				break;
			case Type::Unsigned:
				machine->pushParameter(std::get<Address>(_data));
				break;
			case Type::FloatingPoint:
				machine->pushParameter(std::get<Floating>(_data));
				break;
			case Type::Boolean:
				machine->pushParameter(std::get<bool>(_data));
				break;
			case Type::DictEntry:
				std::get<const DictionaryEntry*>(_data)->operator()(machine);
				break;
			case Type::Word:
				machine->pushParameter(std::get<const DictionaryEntry*>(_data));
				break;
			default:
				throw Problem("unknown", "UNKNOWN ENTRY KIND!");
		}
	}
} // end namespace forth
