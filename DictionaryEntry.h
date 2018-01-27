// concept of a stack cell 
#ifndef DICTIONARY_ENTRY_H__
#define DICTIONARY_ENTRY_H__
#include "Types.h"
#include "Datum.h"
#include <list>
#include <string>
#include <functional>

namespace forth {
	class Machine;
	using NativeMachineOperation = std::function<void(Machine*)>;
	class DictionaryEntry {
		public:
			struct SpaceEntry {
				enum class Discriminant {
					Signed,
					Unsigned,
					FloatingPoint,
					Boolean,
					DictEntry,
                    LoadWordIntoA,
                    LoadWordIntoB,
                    ChooseRegisterAndStoreInC,
                    InvokeRegisterC,
                    IfContainer,
				};
				Discriminant _type;
				union {
					Integer _int;
					Address _addr;
					Floating _fp;
					bool _truth;
					const DictionaryEntry* _entry;
				};
				void invoke(Machine* machine) const;
                void operator()(Machine* machine) const;
			};
            using SpaceEntries = std::list<SpaceEntry>;
		public:
			DictionaryEntry() = default;
			DictionaryEntry(const std::string& name, NativeMachineOperation code = nullptr);
			~DictionaryEntry() = default;
            void markFakeEntry() noexcept { _fake = true; }
            bool isFake() const noexcept { return _fake; }
			const std::string& getName() const noexcept { return _name; }
			NativeMachineOperation getCode() const noexcept { return _code; }
			const DictionaryEntry* getNext() const noexcept { return _next; }
			bool hasNext() const noexcept { return getNext() != nullptr; }
			void setNext(DictionaryEntry* next) noexcept { _next = next; }
			void addSpaceEntry(Integer value);
			void addSpaceEntry(Address value);
			void addSpaceEntry(Floating value);
			void addSpaceEntry(bool value);
			void addSpaceEntry(const DictionaryEntry* value);
            void addSpaceEntry(SpaceEntry::Discriminant type, const DictionaryEntry* value);
            void addTypeDataEntry(forth::Discriminant type);
            void addLoadWordEntryIntoA(const DictionaryEntry* value);
            void addLoadWordEntryIntoB(const DictionaryEntry* value);
            void addChooseOperation();
            void addInvokeCOperation();
			void operator()(Machine* machine) const;
            void markCompileTimeInvoke() noexcept { _compileTimeInvoke = true; }
            bool compileTimeInvoke() const noexcept { return _compileTimeInvoke; }
            SpaceEntries::const_iterator begin() const noexcept { return _space.cbegin(); }
            SpaceEntries::const_iterator end() const noexcept { return _space.cend(); }
            SpaceEntries::const_iterator cbegin() const noexcept { return _space.cbegin(); }
            SpaceEntries::const_iterator cend() const noexcept { return _space.cend(); }
		private:
			std::string _name;
			NativeMachineOperation _code;
			DictionaryEntry* _next;
			// the parameters field is the only thing that doesn't make total sense right now
			// but give it some byte storage of about 128 datums
			SpaceEntries _space;
            bool _fake = false;
            bool _compileTimeInvoke = false;
	};

} // end namespace forth

#endif // end DICTIONARY_ENTRY_H__
