// concept of a stack cell 
#ifndef DICTIONARY_ENTRY_H__
#define DICTIONARY_ENTRY_H__
#include "Types.h"
#include "Datum.h"
#include "Instruction.h"
#include <tuple>
#include <list>
#include <string>
#include <functional>
#include <variant>

namespace forth {
	class Machine;
	using NativeMachineOperation = std::function<void(Machine*)>;
	class DictionaryEntry {
		public:
            using InvokableFunction = std::function<void(Machine*)>;
            using Invokable = std::pair<const DictionaryEntry*, InvokableFunction>;
			struct SpaceEntry final {
                SpaceEntry() = default;
                ~SpaceEntry() = default;
                std::variant<Integer, Address, Floating, bool, Invokable, const DictionaryEntry*, const std::string*> _data;
				void invoke(Machine* machine) const;
                void operator()(Machine* machine) const;
			};
            static Invokable makeInvokable(const DictionaryEntry* entry) noexcept {
                return Invokable(entry, [entry](Machine* m) { entry->operator()(m);});
            }
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
            template<typename T>
            void addSpaceEntry(T value) {
                SpaceEntry se;
                se._data = value;
                _space.emplace_back(se);
            }
			void addSpaceEntry(const DictionaryEntry* value);
            void addSpaceEntry(const std::string& value);
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
