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

namespace forth {
	class Machine {
		public:
			static constexpr Address largestAddress = 0xFFFFFF;
			static constexpr Address memoryCapacity = (largestAddress + 1);
            // capacity variables for the two stacks, each one has 64k worth of data
		public:
			Machine(std::ostream& output, std::istream& input);
			~Machine() = default;
			const DictionaryEntry* lookupWord(const std::string& word) noexcept;
			void controlLoop() noexcept;
			void handleError(const std::string& word, const std::string& msg) noexcept;
			Datum load(Address addr);
			void load() { _registerC.setValue(load(_registerA.getAddress())); }
			void store(Address addr, const Datum& value);
			void store() { store(_registerA.getAddress(), _registerB.getValue()); }
            void pushWord(DictionaryEntry* entry);
			void pushParameter(Datum value);
			Datum popParameter();
			bool numberRoutine(const std::string& word) noexcept;
			void typeValue(Discriminant discriminant, const Datum& value);
			void typeValue(const Datum& value) { typeValue(_registerC.getType(), value); }
			void typeValue() { typeValue(_registerA.getValue()); }
			void addWord(DictionaryEntry* entry);
			void addWord(const std::string& name, NativeMachineOperation op, bool compileTimeInvoke = false);
			template<typename T, typename ... Rest>
			void buildWord(const std::string& name, bool compileTimeInvoke, T word, Rest ... words) {
				// compile up a series of words from c++
				activateCompileMode();
				_compileTarget = new DictionaryEntry(name);
				if (compileTimeInvoke) {
					_compileTarget->markCompileTimeInvoke();
				}
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
			void activateCompileMode() { _compiling = true; }
			void deactivateCompileMode() { _compiling = false; }
			void defineWord();
			void endDefineWord();
			void setA(const Datum& target) noexcept { _registerA.setValue(target); }
			void setTA(Discriminant target) noexcept { _registerA.setType(target); }
			void setTB(Discriminant target) noexcept { _registerB.setType(target); }
			void setB(const Datum& target) noexcept { _registerB.setValue(target); }
			void initializeBaseDictionary();
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
            Register& getRegister(TargetRegister t);
            void load(const Molecule& m);
            void store(const Molecule& m);
			void semicolonOperation();
			void printRegisters();
            void printStack();
			void popSRegister() { popRegister(TargetRegister::RegisterS); }
			void pushRegister(TargetRegister t);
			void popRegister(TargetRegister t); 
            void pushRegister(const Molecule& m);
            void popRegister(const Molecule& m);
            void dispatchInstruction();
			void numericCombine(bool subtract = false);
			void multiplyOperation();
			void equals();
			void powOperation();
			void divide(bool remainder = false);
			void notOperation();
			void minusOperation();
			void andOperation();
			void orOperation();
			void greaterThanOperation();
			void lessThanOperation();
			void xorOperation();
			void shiftOperation(bool shiftLeft = false);
            void ifCondition();
            void elseCondition();
            void thenStatement();
            void beginStatement();
            void endStatement();
            void doStatement();
            void continueStatement();
            enum class Immediate16Positions : byte {
                Lowest,
                Lower,
                Higher,
                Highest,
            };
            template<Immediate16Positions pos>
            static constexpr auto Immediate16ShiftIndex = static_cast<Address>(pos) * 16;
            template<Immediate16Positions pos>
            static constexpr auto Immediate16Mask = static_cast<Address>(0xFFFF) << Immediate16ShiftIndex<pos>;

            template<Immediate16Positions pos>
            void setImmediate16(TargetRegister reg, QuarterAddress value) {
                Register& dest = getRegister(reg);
                dest.setValue(encodeBits<Address, QuarterAddress, Immediate16Mask<pos>, Immediate16ShiftIndex<pos>>(dest.getAddress(), value));
            }
            template<Immediate16Positions pos>
            void setImmediate16(const Molecule& m, TargetRegister target = TargetRegister::RegisterX) {
                try {
                    setImmediate16<pos>(target, m.getQuarterAddress(_registerIP.getAddress()));
                    _registerIP.increment();
                    _registerIP.increment();
                } catch (Problem& p) {
                    std::string msg("set-immediate16-unknown");
                    switch (pos) {
                        case Immediate16Positions::Lowest:
                            msg = "set-immediate16-lowest";
                            break;
                        case Immediate16Positions::Lower:
                            msg = "set-immediate16-lower";
                            break;
                        case Immediate16Positions::Higher:
                            msg = "set-immediate16-higher";
                            break;
                        case Immediate16Positions::Highest:
                            msg = "set-immediate16-highest";
                            break;
                    }
                    throw Problem(msg, p.getMessage());
                }
            }
            template<Immediate16Positions pos>
            void setImmediate16Full(const Molecule& m) {
                try {
                    auto dest = getDestinationRegister(m.getByte(_registerIP.getAddress()));
                    _registerIP.increment();
                    setImmediate16<pos>(m.getQuarterAddress(_registerIP.getAddress()), static_cast<TargetRegister>(dest));
                } catch (Problem& p) {
                    std::string msg("set-immediate16-full-unknown");
                    switch (pos) {
                        case Immediate16Positions::Lowest:
                            msg = "set-immediate16-full-lowest";
                            break;
                        case Immediate16Positions::Lower:
                            msg = "set-immediate16-full-lower";
                            break;
                        case Immediate16Positions::Higher:
                            msg = "set-immediate16-full-higher";
                            break;
                        case Immediate16Positions::Highest:
                            msg = "set-immediate16-full-highest";
                            break;
                    }
                    throw Problem(msg, p.getMessage());
                }
            }
            /**
             * Printout the contents of the given word!
             */
            void seeWord();
			void terminateExecution();
            void seeWord(const DictionaryEntry* entry);
			std::string readWord();
            void moveOrSwap(TargetRegister from, TargetRegister to, bool swap = false);
            void moveRegister(const Molecule& m);
            void swapRegisters(const Molecule& m);
			void compileMicrocodeInvoke(const Molecule& m, DictionaryEntry* current);
			void microcodeInvoke(const Molecule& m);
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

		private:
			// define the CPU that the forth interpreter sits on top of
			std::ostream& _output;
			std::istream& _input;
			std::unique_ptr<Datum[]> _memory;
			DictionaryEntry* _words;
			// no need for the subroutine stack
            std::list<Datum> _subroutine;
            std::list<Datum> _parameter;
			bool _initializedBaseDictionary = false;
			bool _keepExecuting = true;
			bool _compiling = false;
			DictionaryEntry* _compileTarget = nullptr;
			// internal "registers"
            Register _registerA, _registerB, _registerC, _registerS, _registerX;
            Register _registerIP, _registerSP, _registerSP2;
			const DictionaryEntry* _microcodeInvoke = nullptr;
			const DictionaryEntry* _popS = nullptr;
	};
} // end namespace forth

#endif // end MACHINE_H__
