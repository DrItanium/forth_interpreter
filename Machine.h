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

namespace forth {
	class Machine {
		public:
			static constexpr Address shouldKeepExecutingLocation = Core::userVariableStart;
			static constexpr Address isCompilingLocation = Core::userVariableStart + 1;
			static constexpr Address ignoreInputLocation = Core::userVariableStart + 2;
			static constexpr Address subroutineStackEmptyLocation = Core::sp2StackEmpty;
			static constexpr Address subroutineStackFullLocation = Core::sp2StackFull;
			static constexpr Address parameterStackEmptyLocation = Core::spStackEmpty;
			static constexpr Address parameterStackFullLocation = Core::spStackFull;
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
			void typeValue(const Datum& value);
			void typeValue();
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
				activateCompileMode();
				_compileTarget = new DictionaryEntry(name);
				if (compileTimeInvoke) {
					_compileTarget->markCompileTimeInvoke();
				}
				moleculeSequence<first, rest...>();
				endDefineWord();
            }
            template<Address first, Address ... rest>
            void addMoleculeSequence(const std::string& name, bool compileTimeInvoke = false) {
				activateCompileMode();
				_compileTarget = new DictionaryEntry(name);
				if (compileTimeInvoke) {
					_compileTarget->markCompileTimeInvoke();
				}
				moleculeWord<first, rest...>();
				endDefineWord();
            }
			void addition(Discriminant type);
			void listWords();
			void defineWord();
			void endDefineWord();
			void setA(const Datum& target) noexcept;
			void setB(const Datum& target) noexcept;
			void setTA(Discriminant target) noexcept;
			void setTB(Discriminant target) noexcept;
			void initializeBaseDictionary();
			void dispatchInstruction(const Molecule& m);
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
			void pushRegister(TargetRegister t);
			void popRegister(TargetRegister t); 
            void dispatchInstruction();
			template<Address first, Address ... rest>
			void dispatchInstructionStream() noexcept {
				dispatchInstruction(first);
				if constexpr (sizeof...(rest) > 0) {
					dispatchInstructionStream<rest...>();
				}
			}
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
			std::string readWord();
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
            template<Address first, Address ... rest>
            void moleculeWord() {
                microcodeInvoke(first);
                if constexpr (sizeof...(rest) > 0) {
                    moleculeWord<rest...>();
                }
            }
            template<auto first, auto ... rest>
            void moleculeSequence() {
                if constexpr (Instruction::operationLength(first, std::move(rest)...) <= 8) {
                    moleculeWord<Instruction::encodeOperation(first, std::move(rest)...)>();
                } else {
                    makeMoleculeSequence<getInstructionWidth(first), Instruction::encodeOperation<0, decltype(first)>(0, first), rest...>();
                }
            }
            template<byte depth, Address current, auto first, auto ... rest>
            void makeMoleculeSequence() {
                // we could encode each operation into a separate word to start with
				static constexpr byte newDepth = depth + getInstructionWidth(first);
				if constexpr (newDepth > 7) {
					// we need to start a new molecule, with the current
					// instruction added
					moleculeWord<current>();
					makeMoleculeSequence<0, 0, first, rest...>();
				} else if constexpr (newDepth == 7) {
					static constexpr auto newAddress = Instruction::encodeOperation<depth, decltype(first)>(current, first);
					// okay, we filled up the current molecule and need to
					// restart
					moleculeWord<newAddress>();
					if constexpr (sizeof...(rest) > 0) {
						makeMoleculeSequence<0, 0, rest...>();
					}
				} else {
					// known DRY because the compiler won't accept it
					// otherwise!
					static constexpr auto newAddress = Instruction::encodeOperation<depth, decltype(first)>(current, first);
					// we did not fill up the current molecule either
					if constexpr (sizeof... (rest) > 0) {
						makeMoleculeSequence<newDepth, newAddress, rest...>();
					} else {
						// nothing left to do so just stop here!
						moleculeWord<newAddress>();
					}
				}
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
	};
} // end namespace forth

#endif // end MACHINE_H__
