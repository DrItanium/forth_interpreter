// concept of a stack cell 
#ifndef MACHINE_H__
#define MACHINE_H__
#include "Types.h"
#include "DictionaryEntry.h"
#include "Datum.h"
#include "Instruction.h"
#include <iostream>
#include <memory>

namespace forth {
	class Machine {
		public:
			static constexpr auto largestAddress = 0xFFFFFF;
			static constexpr auto memoryCapacity = (largestAddress + 1);
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
		private:
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
			void initializeBaseDictionary();
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
            void singleMoleculeSequence() {
                moleculeWord<Instruction::encodeOperation(first, std::move(rest)...)>();
            }
            template<auto first, auto ... rest>
            void moleculeSequence() {
                makeMoleculeSequence<0,0,first, rest...>();
            }
            template<byte count, auto first>
            static constexpr bool canFitIntoInstruction() noexcept {
                return ((8 - count) >= getInstructionWidth(first));
            }
            template<byte depth, Address current, auto first, auto ... rest>
            void makeMoleculeSequence() {
                // we could encode each operation into a separate word to start with
                if constexpr (canFitIntoInstruction<depth, first>()) {
                    static_assert(getInstructionWidth(first) != 0, "Illegal instruction found!");
                    static constexpr byte newDepth = depth + getInstructionWidth(first);
                    static constexpr auto newAddress = Instruction::encodeOperation<newDepth, decltype(first)>(current, first);
                    if constexpr (sizeof...(rest) > 0) {
                        makeMoleculeSequence<newDepth, newAddress, rest...>();
                    } else {
                        moleculeWord<newAddress>();
                    }
                } else {
                    moleculeWord<current>();
                    makeMoleculeSequence<0, 0u, first, rest...>();
                }
            }

		private:
			// define the CPU that the forth interpreter sits on top of
			std::ostream& _output;
			std::istream& _input;
			std::unique_ptr<Integer[]> _memory;
			DictionaryEntry* _words;
			// no need for the subroutine stack
            std::list<DictionaryEntry*> _subroutine;
            std::list<Datum> _parameter;
			bool _initializedBaseDictionary = false;
			bool _keepExecuting = true;
			bool _compiling = false;
			DictionaryEntry* _compileTarget = nullptr;
			// internal "registers"
            Register _registerA, _registerB, _registerC, _registerS, _registerX, _registerIP;
			const DictionaryEntry* _microcodeInvoke = nullptr;
			const DictionaryEntry* _popS = nullptr;
	};
} // end namespace forth

#endif // end MACHINE_H__
