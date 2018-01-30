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
			void load() { _registerC = load(_registerA.address); }
			void store(Address addr, const Datum& value);
			void store() { store(_registerA.address, _registerB); }
            void pushWord(DictionaryEntry* entry);
			void pushParameter(Datum value);
			Datum popParameter();
			bool numberRoutine(const std::string& word, bool putTypeDataOntoStack = false) noexcept;
			void typeValue(Discriminant discriminant, const Datum& value);
			void typeValue(const Datum& value) { typeValue(_registerT, value); }
			void typeValue() { typeValue(_registerA._value); }
			void addWord(DictionaryEntry* entry);
			void addWord(const std::string& name, NativeMachineOperation op, bool compileTimeInvoke = false);
			void addition(Discriminant type);
			void listWords();
			void activateCompileMode() { _compiling = true; }
			void deactivateCompileMode() { _compiling = false; }
			void defineWord();
			void endDefineWord();
			void setA(const Datum& target) noexcept { _registerA._value = target; }
			void setTA(Discriminant target) noexcept { _registerA._type = target; }
			void setTB(Discriminant target) noexcept { _registerB._type = target; }
			void setB(const Datum& target) noexcept { _registerB._value = target; }
            void chooseRegister();
            void invokeCRegister();
		private:
			void semicolonOperation();
            enum class TargetRegister : byte {
                RegisterA,
                RegisterTA,
                RegisterB,
                RegisterTB,
                RegisterC,
                RegisterT,
                RegisterS, // register select
                RegisterX, // misc data
                RegisterTX, // misc type
            };
            static constexpr bool involvesDiscriminantRegister(TargetRegister r) {
                switch (r) {
                    case TargetRegister::RegisterT:
                    case TargetRegister::RegisterTA:
                    case TargetRegister::RegisterTB:
                    case TargetRegister::RegisterTX:
                        return true;
                    default:
                        return false;
                }
            }
			void printRegisters();
            void printStack();
			void pushSRegister() { pushRegister(TargetRegister::RegisterS); }
			void pushRegister(TargetRegister t);
			void popRegister(TargetRegister t); 
            void dispatchInstruction();
			void numericCombine(bool subtractB = false);
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
			void sinOperation();
			void cosOperation();
			void tanOperation();
			void atanOperation();
			void atan2Operation();
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
            Register _registerA, _registerB, _registerC, _registerS, _registerX;
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
