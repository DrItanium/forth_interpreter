// concept of a stack cell 
#ifndef MACHINE_H__
#define MACHINE_H__
#include "Types.h"
#include "DictionaryEntry.h"
#include "Datum.h"
#include "Instruction.h"
#include <iostream>
#include <memory>
#include <sstream>
#include "Core.h"
#include "Assembler.h"

namespace forth {
    class IndirectAddress {
        public:
            explicit IndirectAddress(Address value) : _location(value) { };
            Address getLocation() const noexcept { return _location; }
        private:
            Address _location;
    };
	class Machine {
		public:
            enum class UserVariableLocations : Address {
#define UserVariable(x) x,
#define UserRoutine(x) x,
#include "UserVariables.def"
#undef UserVariable
#undef UserRoutine 
            };
            template<UserVariableLocations loc>
            static constexpr auto userVariableAddress = Core::userVariableStart + Core::wordToByteOffset<Address(loc)>;
#define UserVariable(x) static constexpr Address location ## x = userVariableAddress<UserVariableLocations:: x>;
#define UserRoutine(x) UserVariable(x)
#include "UserVariables.def"
#undef UserRoutine
#undef UserVariable
			static constexpr Address locationSubroutineStackEmpty = Core::sp2StackEmpty;
			static constexpr Address locationSubroutineStackFull = Core::sp2StackFull;
			static constexpr Address locationParameterStackEmpty = Core::spStackEmpty;
			static constexpr Address locationParameterStackFull = Core::spStackFull;
            static constexpr Address jitCacheLocation = 0x1000;
            static constexpr Address builtinRoutinesStart = 0x2000; 
            // capacity variables for the two stacks, each one has 64k worth of data
		public:
			Machine(std::ostream& output, std::istream& input);
			~Machine() = default;
			const DictionaryEntry* lookupWord(const std::string& word) noexcept;
			void controlLoop() noexcept;
			void handleError(const std::string& word, const std::string& msg) noexcept;
            void pushWord(DictionaryEntry* entry);
			void pushParameter(Datum value);
			Datum popParameter();
			void installInCore(AssemblerBuilder& ab);
			bool numberRoutine(const std::string& word) noexcept;
			void addWord(DictionaryEntry* entry);
			void addWord(const std::string& name, NativeMachineOperation op, bool compileTimeInvoke = false);
			void addition(Discriminant type);
			void listWords();
			void defineWord();
			void endDefineWord();
			void initializeBaseDictionary();
            void dispatchInstruction(const IndirectAddress& targetRoutine);
            void dispatchInstruction(Address targetRoutine);
			template<typename T, typename ... Rest>
			void dispatchInstruction(T first, Rest&& ... rest) {
				dispatchInstruction(first);
                if constexpr (sizeof...(rest) > 0) {
                    dispatchInstruction(std::move(rest)...);
                }
			}
		private:
            void raiseError();
            void chooseRegister();
            void invokeCRegister();
			void semicolonOperation();
			void printRegisters();
            void printStack();
            void dispatchInstruction();
            void ifCondition();
            void elseCondition();
            void thenStatement();
            void beginStatement();
            void endStatement();
            void doStatement();
            void continueStatement();
            /**
             * Printout the contents of the given word!
             */
            void seeWord();
            void seeWord(const DictionaryEntry* entry);
			std::string readWord(bool allowEscapedQuotes = false);
            void moveOrSwap(TargetRegister from, TargetRegister to, bool swap = false);
			//void compileMicrocodeInvoke(const Molecule& m, DictionaryEntry* current);
			//void microcodeInvoke(const Molecule& m);
            void injectWord();
            void executeTop();
			bool keepExecuting() noexcept;
			bool inCompilationMode() noexcept;
			void activateCompileMode();
			void deactivateCompileMode();
			void pushOntoStack(TargetRegister sp, Datum value, Address fullLocation);
			Datum popOffStack(TargetRegister sp, Address emptyLocation);
			bool stackEmpty(TargetRegister sp, Address compareLocation);
			bool stackFull(TargetRegister sp, Address compareLocation);
			void clearStack(TargetRegister sp, Address emptyLocation);
			void pushSubroutine(Datum value);
			Datum popSubroutine();
			bool subroutineStackEmpty();
			void clearSubroutineStack();
			bool subroutineStackFull();
            void invokeCore();
            void terminateControlLoop();
            void constructString();
            void printString();
            void printNewLine();
            void printOk();
		private:
			// define the CPU that the forth interpreter sits on top of
			std::ostream& _output;
			std::istream& _input;
			DictionaryEntry* _words;
			// no need for the subroutine stack
			bool _initializedBaseDictionary = false;
            Compiler* _compileTarget = nullptr;
			Core _core;
            // TODO: update the compiler to use this cache when necessary!
            std::map<std::string, Address> _stringCache;
	};
} // end namespace forth

#endif // end MACHINE_H__
