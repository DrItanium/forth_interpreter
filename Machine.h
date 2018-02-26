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
            template<Address index>
            static constexpr auto userVariableAddress = Core::userVariableStart + Core::wordToByteOffset<index>;
			static constexpr Address shouldKeepExecutingLocation = userVariableAddress<0>;
			static constexpr Address isCompilingLocation = userVariableAddress<1>;
			static constexpr Address ignoreInputLocation = userVariableAddress<2>;
            static constexpr Address captureInputIntoStringLocation = userVariableAddress<3>;
			static constexpr Address subroutineStackEmptyLocation = Core::sp2StackEmpty;
			static constexpr Address subroutineStackFullLocation = Core::sp2StackFull;
			static constexpr Address parameterStackEmptyLocation = Core::spStackEmpty;
			static constexpr Address parameterStackFullLocation = Core::spStackFull;
			static constexpr Address jitCacheLocation = 0x1000; // byte based
            // capacity variables for the two stacks, each one has 64k worth of data
		public:
			Machine(std::ostream& output, std::istream& input);
			~Machine() = default;
			const DictionaryEntry* lookupWord(const std::string& word) noexcept;
			void controlLoop() noexcept;
			void handleError(const std::string& word, const std::string& msg) noexcept;
			Datum load(Address addr);
			void load();
			void store(Address addr, const Datum& value);
			void store();
            void pushWord(DictionaryEntry* entry);
			void pushParameter(Datum value);
			Datum popParameter();
			bool numberRoutine(const std::string& word) noexcept;
			void typeValue(Discriminant discriminant, const Datum& value);
			void addWord(DictionaryEntry* entry);
			void addWord(const std::string& name, NativeMachineOperation op, bool compileTimeInvoke = false);
			std::function<void(Address, Address)> getMemoryInstallationFunction();
			std::function<void(Address, Address)> getInstructionInstallationFunction();
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
            void moveRegister(const Molecule& m);
            void swapRegisters(const Molecule& m);
			void compileMicrocodeInvoke(const Molecule& m, DictionaryEntry* current);
			void microcodeInvoke(const Molecule& m);
			template<typename First, typename ... Rest>
			void microcodeStreamInvoke(First addr, Rest&& ... rest) {
				static_assert(std::is_same<typename std::remove_reference<typename std::remove_cv<First>::type>::type, Address>::value, "All arguments must be of type Address!");
				microcodeInvoke(addr);
				if constexpr (sizeof...(rest) > 0) {
					microcodeStreamInvoke(std::move(rest)...);
				}
			}
            void injectWord();
            void executeTop();
			void installMoleculeToMemory0(Address value, Address memory) {
				AssemblerBuilder ab(jitCacheLocation);
				ab.addInstruction(loadImmediate64(TargetRegister::A, memory),
								  loadImmediate64(TargetRegister::B, value),
								  forth::store(TargetRegister::A, TargetRegister::B));
				dispatchInstruction(ab);
			}
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
