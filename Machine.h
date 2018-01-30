// concept of a stack cell 
#ifndef MACHINE_H__
#define MACHINE_H__
#include "Types.h"
#include <iostream>
#include <memory>
#include "DictionaryEntry.h"
#include "Datum.h"

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
			bool numberRoutine(const std::string& word, bool putTypeDataOntoStack = false) noexcept;
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
            void chooseRegister();
            void invokeCRegister();
		private:
            enum class TargetRegister : byte {
                RegisterA,
                RegisterB,
                RegisterC,
                RegisterS, // register select
                RegisterX, // misc data
                RegisterT,
                RegisterTA,
                RegisterTB,
                RegisterTX, // misc type
                RegisterIP, // instruction pointer contents
                RegisterTIP, // instruction pointer type
                Error,
            };
            static_assert(byte(TargetRegister::Error) <= 16, "Too many registers defined!");
            static constexpr bool involvesDiscriminantRegister(TargetRegister r) noexcept {
                switch (r) {
                    case TargetRegister::RegisterT:
                    case TargetRegister::RegisterTA:
                    case TargetRegister::RegisterTB:
                    case TargetRegister::RegisterTX:
                    case TargetRegister::RegisterTIP:
                        return true;
                    default:
                        return false;
                }
            }
            static constexpr bool legalValue(TargetRegister r) noexcept {
                return static_cast<byte>(r) < static_cast<byte>(TargetRegister::Error);
            }
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
			void modulo();
			void divide();
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
            void setImmediate16Lowest(const Molecule& m);
            void setImmediate16Lower(const Molecule& m);
            void setImmediate16Higher(const Molecule& m);
            void setImmediate16Highest(const Molecule& m);
            /**
             * Printout the contents of the given word!
             */
            void seeWord();
			void terminateExecution();
            void seeWord(const DictionaryEntry* entry);
			void initializeBaseDictionary();
            void cacheBasicEntries();
			std::string readWord();
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
            bool _cachedBasicEntries = false;
			bool _keepExecuting = true;
			bool _compiling = false;
			DictionaryEntry* _compileTarget = nullptr;
			// internal "registers"
            Register _registerA, _registerB, _registerC, _registerS, _registerX, _registerIP;
            const DictionaryEntry* _popTA = nullptr;
            const DictionaryEntry* _popTB = nullptr;
            const DictionaryEntry* _popTX = nullptr;
            const DictionaryEntry* _popA = nullptr;
            const DictionaryEntry* _popB = nullptr;
            const DictionaryEntry* _popC = nullptr;
            const DictionaryEntry* _popT = nullptr;
            const DictionaryEntry* _popS = nullptr;
            const DictionaryEntry* _popX = nullptr;
            const DictionaryEntry* _pushA = nullptr;
            const DictionaryEntry* _pushB = nullptr;
            const DictionaryEntry* _pushC = nullptr;
            const DictionaryEntry* _pushT = nullptr;
            const DictionaryEntry* _pushX = nullptr;
            const DictionaryEntry* _pushS = nullptr;
            const DictionaryEntry* _nop = nullptr;

	};
} // end namespace forth

#endif // end MACHINE_H__
