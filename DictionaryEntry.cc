#include <list>
#include "Datum.h"
#include "DictionaryEntry.h"
#include "Problem.h"
namespace forth {
    void DictionaryEntry::addTypeDataEntry(forth::Discriminant type) {
        addSpaceEntry(static_cast<Address>(type));
    }
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
    void DictionaryEntry::addChooseOperation() {
        addSpaceEntry(SpaceEntry::Discriminant::ChooseRegisterAndStoreInC, nullptr);
    }
    void DictionaryEntry::addInvokeCOperation() {
        addSpaceEntry(SpaceEntry::Discriminant::InvokeRegisterC, nullptr);
    }
    void DictionaryEntry::addSpaceEntry(SpaceEntry::Discriminant type, const DictionaryEntry* value) {
        SpaceEntry se;
        se._type = type;
        se._entry = value;
        _space.emplace_back(se);
    }
    void DictionaryEntry::addLoadWordEntryIntoA(const DictionaryEntry* value) {
        addSpaceEntry(SpaceEntry::Discriminant::LoadWordIntoA, value);
    }

    void DictionaryEntry::addLoadWordEntryIntoB(const DictionaryEntry* value) {
        addSpaceEntry(SpaceEntry::Discriminant::LoadWordIntoB, value);
    }


    void DictionaryEntry::addSpaceEntry(Integer x) {
        SpaceEntry se;
        se._type = SpaceEntry::Discriminant::Signed;
        se._int = x;
        _space.emplace_back(se);
    }

    void DictionaryEntry::addSpaceEntry(Address x) {
        SpaceEntry se;
        se._type = SpaceEntry::Discriminant::Unsigned;
        se._addr = x;
        _space.emplace_back(se);
    }

    void DictionaryEntry::addSpaceEntry(Floating x) {
        SpaceEntry se;
        se._type = SpaceEntry::Discriminant::FloatingPoint;
        se._fp = x;
        _space.emplace_back(se);
    }

    void DictionaryEntry::addSpaceEntry(bool x) {
        SpaceEntry se;
        se._type = SpaceEntry::Discriminant::Boolean;
        se._truth = x;
        _space.emplace_back(se);
    }

    void DictionaryEntry::addSpaceEntry(const DictionaryEntry* x) {
        SpaceEntry se;
        se._type = SpaceEntry::Discriminant::DictEntry;
        se._entry = x;
        _space.emplace_back(se);
    }


    DictionaryEntry::DictionaryEntry(const std::string& name, NativeMachineOperation code) : _name(name), _code(code), _next(nullptr) { }
} // end namespace forth
