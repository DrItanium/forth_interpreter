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
			void typeValue() { typeValue(_registerA); }
			void addWord(DictionaryEntry* entry);
			void addWord(const std::string& name, NativeMachineOperation op, bool compileTimeInvoke = false);
			void addition(Discriminant type);
			void listWords();
			void activateCompileMode() { _compiling = true; }
			void deactivateCompileMode() { _compiling = false; }
			void defineWord();
			void endDefineWord();
			void semicolonOperation();
			void setA(const Datum& target) noexcept { _registerA = target; }
			void setTA(Discriminant target) noexcept { _registerTA = target; }
			void setTB(Discriminant target) noexcept { _registerTB = target; }
			void setB(const Datum& target) noexcept { _registerB = target; }
            void chooseRegister();
            void invokeCRegister();
		private:
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
            template<TargetRegister t>
            void pushRegister() {
                using Type = decltype(t);
                Datum tmp;
                switch (t) {
                    case Type::RegisterA:
                        tmp = _registerA;
                        break;
                    case Type::RegisterB:
                        tmp = _registerB;
                        break;
                    case Type::RegisterC:
                        tmp = _registerC;
                        break;
                    case Type::RegisterT:
                        tmp = static_cast<Address>(_registerT);
                        break;
                    case Type::RegisterTA:
                        tmp = static_cast<Address>(_registerTA);
                        break;
                    case Type::RegisterTB:
                        tmp = static_cast<Address>(_registerTB);
                        break;
                    case Type::RegisterTX:
                        tmp = static_cast<Address>(_registerTX);
                        break;
                    case Type::RegisterS:
                        tmp = _registerS;
                        break;
                    case Type::RegisterX:
                        tmp = _registerX;
                        break;
                    default:
                        throw Problem("push.register", "Unknown register!");
                }
                pushParameter(tmp);
            }
            template<TargetRegister t>
            void popRegister() {
                using Type = decltype(t);
                static constexpr Address max = (Address)Discriminant::Count;
                auto top(popParameter());
                if (involvesDiscriminantRegister(t) && top.address >= max) {
                    throw Problem("pop.register", "ILLEGAL DISCRIMINANT!");
                }
                switch (t) {
                    case Type::RegisterA:
                        _registerA = top;
                        break;
                    case Type::RegisterB:
                        _registerB = top;
                        break;
                    case Type::RegisterC:
                        _registerC = top;
                        break;
                    case Type::RegisterT:
                        _registerT = (Discriminant)top.address;
                        break;
                    case Type::RegisterTA:
                        _registerTA = (Discriminant)top.address;
                        break;
                    case Type::RegisterTB:
                        _registerTB = (Discriminant)top.address;
                        break;
                    case Type::RegisterTX:
                        _registerTX = (Discriminant)top.address;
                        break;
                    case Type::RegisterS:
                        _registerS = top;
                        break;
                    case Type::RegisterX:
                        _registerX = top;
                        break;
                    default:
                        throw Problem("pop.register", "Unknown register!");
                }

            }
            void dispatchInstruction();
            void aluOperation();
			void add();
			void subtract();
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
            /**
             * Printout the contents of the given word!
             */
            void seeWord();
		private:
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
			Datum _registerA, _registerB, _registerC, _registerS, _registerX;
			Discriminant _registerT, _registerTA, _registerTB, _registerTX;
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
