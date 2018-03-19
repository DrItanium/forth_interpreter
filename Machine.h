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
	class Machine {
		public:
            enum class UserVariableLocations : Address {
#define UserVariableFirst(x) x = 0,
#define UserVariable(x) x,
#include "UserVariables.def"
#undef UserVariable
#undef UserVariableFirst
            };
            template<Address index>
            static constexpr auto userVariableAddress = Core::userVariableStart + Core::wordToByteOffset<index>;
#define UserVariable(x) static constexpr Address location ## x = userVariableAddress<Address(UserVariableLocations:: x )> ;
#define UserVariableFirst(x) UserVariable(x)
#include "UserVariables.def"
#undef UserVariableFirst
#undef UserVariable
			static constexpr Address locationSubroutineStackEmpty = Core::sp2StackEmpty;
			static constexpr Address locationSubroutineStackFull = Core::sp2StackFull;
			static constexpr Address locationParameterStackEmpty = Core::spStackEmpty;
			static constexpr Address locationParameterStackFull = Core::spStackFull;
            static constexpr Address jitCacheLocation = 0x1000;
            static constexpr Address builtinRoutinesStart = 0x2000; 
            // capacity variables for the two stacks, each one has 64k worth of data
        public:
            // list of builtins with assembly routines
            enum class BuiltinRoutines {
                StoreValue, // ( addr value -- )
                /*
                 * pop A, SP
                 * pop B, SP
                 * store B, A
                 * ret
                 */
                LoadValue, // ( addr -- value )
                /*
                 * pop A, SP
                 * load C, A
                 * push C, SP
                 * ret
                 */
                WriteFalse, // ( addr -- )
                /*
                 * push Zero, SP
                 * call StoreValue
                 * ret
                 */
                WriteTrue, // ( addr -- )
                /*
                 * addiu A = Zero, 1
                 * push A, SP
                 * call StoreValue
                 * ret
                 */
                GetParameterStackEmpty, // ( -- addr)
                /* 
                 * loadimm64 A, Core::spStackEmpty
                 * push A, SP
                 * call LoadValue
                 * ret
                 */
                GetSubroutineStackEmpty, // ( -- addr)
                /* 
                 * loadimm64 A, Core::sp2StackEmpty
                 * push A, SP
                 * call LoadValue
                 * ret
                 */
                ClearSubroutineStack, // ( -- )
                /*
                 * call GetSubroutineStackEmpty
                 * pop A, SP
                 * move SP2, A
                 * ret
                 */
                ClearParameterStack, // ( -- )
                /*
                 * call GetParameterStackEmpty
                 * pop A, SP
                 * move SP, A
                 * ret
                 */
                EqualsAddress, // ( a b -- t )
                /* 
                 * pop A, SP
                 * pop B, SP
                 * cmpeq C = A, B
                 * push C, SP
                 * ret
                 */
                ActivateCompilationMode,
                DeactivateCompilationMode,
                InCompilationMode,
                DispatchInstruction, // ( addr -- )
                /*
                 * pop A, SP
                 * calli A
                 * ret
                 */
                ShouldKeepExecuting,
                PrintRegisters,
                PrintStack,
            };
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
			template<typename T, typename ... Rest>
			void buildWord(const std::string& name, T word, Rest ... words) {
				// compile up a series of words from c++
				activateCompileMode();
				_compileTarget = new DictionaryEntry(name);
				tryCompileWord(word, words..., ";");
				endDefineWord();
			}
            /**
             * Compile a sequence of microcode instructions into a word :D
             */
            template<auto first, auto ... rest>
            void addMachineCodeWord(const std::string& name, bool compileTimeInvoke = false) {
                addWord(name, [this](Machine* _) { dispatchInstruction(first, rest...); }, compileTimeInvoke);
            }
			void addition(Discriminant type);
			void listWords();
			void defineWord();
			void endDefineWord();
			void initializeBaseDictionary();
			void dispatchInstruction(AssemblerBuilder&);
			template<typename T, typename ... Rest>
			void dispatchInstruction(T first, Rest&& ... rest) {
				AssemblerBuilder ab(jitCacheLocation);
				ab.addInstruction(first, std::move(rest)...);
				dispatchInstruction(ab);
			}
		private:
			template<typename ... Rest>
			void tryCompileWord(const std::string& word, Rest ... words) {
				auto entry = lookupWord(word);
				if (entry) {
					// replace with the DictionaryEntry
					tryCompileWord(entry, words...);
				} else {
					std::stringstream ss;
					ss << "unknown word to compile: " << word << "?";
					auto str = ss.str();
					throw Problem(_compileTarget->getName(), str);
				}
			}
			template<typename ... Rest>
			void tryCompileWord(const char* word, Rest ... words) {
				auto entry = lookupWord(word);
				if (entry) {
					// replace with the DictionaryEntry
					tryCompileWord(entry, words...);
				} else {
					std::stringstream ss;
					ss << "unknown word to compile: " << word << "?";
					auto str = ss.str();
					throw Problem(_compileTarget->getName(), str);
				}
			}
			template<typename T, typename ... Rest>
			void tryCompileWord(T word, Rest ... words) {
				if constexpr (std::is_enum<T>::value) {
					_compileTarget->addSpaceEntry(static_cast<Address>(word));
				} else {
					_compileTarget->addSpaceEntry(word);
				}
				if constexpr (sizeof...(words) > 0) {
					tryCompileWord(words...);
				}
			}
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
		private:
			// define the CPU that the forth interpreter sits on top of
			std::ostream& _output;
			std::istream& _input;
			DictionaryEntry* _words;
			// no need for the subroutine stack
			bool _initializedBaseDictionary = false;
			DictionaryEntry* _compileTarget = nullptr;
			Core _core;
			const DictionaryEntry* _microcodeInvoke = nullptr;
            std::list<std::string> _stringCache;
	};
} // end namespace forth

#endif // end MACHINE_H__
