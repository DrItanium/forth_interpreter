#ifndef CORE_H__
#define CORE_H__
#include "Types.h"
#include "Datum.h"
#include <functional>
#include <tuple>
#include "Instruction.h"
#include <memory>
namespace forth {
class Core {
	public:
        static constexpr Address largestByteAddress = 0x7FF'FFFF;
		static constexpr Address largestAddress = largestByteAddress >> 3;
		static constexpr Address memoryCapacity = (largestAddress + 1);
        static constexpr Address memoryByteCapacity = (largestByteAddress + 1);
		static constexpr Address byteAddressMask = 0b111;
		static constexpr Address wordAddressMask = ~byteAddressMask;
		template<Address index>
		static constexpr auto wordToByteOffset = index * 8;
		// we have a 64-kiloword area for storing internal system values.
		static constexpr Address byteSystemVariableStart = 0xFFFF'FFFF'FFF8'0000;
        static constexpr Address byteSystemVariableEnd = 0xFFFF'FFFF'FFFF'FFFF;
        static constexpr Address byteUserVariableStart = byteSystemVariableStart;
        static constexpr Address byteUserVariableEnd = 0xFFFF'FFFF'FFFB'FFFF;
        static constexpr Address systemVariableStart = byteSystemVariableStart & wordAddressMask;
		static constexpr Address systemVariableEnd = byteSystemVariableEnd & wordAddressMask;
        static constexpr Address userVariableStart = byteUserVariableStart & wordAddressMask;
        static constexpr Address userVariableEnd = byteUserVariableEnd & wordAddressMask;
		static constexpr Address terminateExecutionVariable = systemVariableEnd;
		static constexpr Address sp2StackEmpty = systemVariableEnd - wordToByteOffset<1>;
		static constexpr Address sp2StackFull = systemVariableEnd - wordToByteOffset<2>;
		static constexpr Address spStackEmpty = systemVariableEnd - wordToByteOffset<3>;
		static constexpr Address spStackFull = systemVariableEnd - wordToByteOffset<4>;
		static constexpr Address returnToMicrocode = systemVariableEnd - wordToByteOffset<5>;
		static constexpr Address systemVariableMask = (systemVariableEnd - systemVariableStart) >> 3;
		static_assert(systemVariableMask == 0xFFFF, "System variable mask is not correct!");
		static constexpr Address systemVariableSize = ((systemVariableEnd - systemVariableStart) + wordToByteOffset<1>) >> 3;
        static_assert(systemVariableSize == 0x10000, "System variable size is not 64 kwords");
        static_assert((byteSystemVariableEnd - byteSystemVariableStart) == 0x7FFFF, "System variable size is not 512k in size!");
		static constexpr Address getNearestWordAddress(Address input) noexcept {
			return decodeBits<Address, Address, wordAddressMask, 0>(input);
		}
        static constexpr Address getWordAddress(Address input) noexcept {
			return getNearestWordAddress(input) >> 3;
        }
        static constexpr Address getByteOffset(Address input) noexcept {
            return decodeBits<Address, Address, byteAddressMask, 0>(input);
        }
        template<typename T>
        static constexpr bool spansTwoAddresses(Address addr) noexcept {
            if constexpr (sizeof(T) > sizeof(Address)) {
                return true;
            } else if constexpr (sizeof(T) == sizeof(Address)) {
                return getByteOffset(addr) != 0;
            } else {
                return getByteOffset(addr) <= (sizeof(Address) - sizeof(T));
            }
        }
        static constexpr bool spansTwoAddresses(Address addr, byte size) noexcept {
            if (sizeof(Address) < size) {
                return true;
            } else {
                return getByteOffset(addr) <= (sizeof(Address) - size);
            } 
        }
        struct NoArguments final { };
#define InstructionKind(x) \
        struct x ; \
        struct Is ## x final { using Type = x ; }; \
        struct x final
        InstructionKind(TwoRegister) {
            TargetRegister destination;
            TargetRegister source;
        };
        InstructionKind(OneRegister) {
            TargetRegister destination;
        };
        using TwoByteVariant = std::variant<IsTwoRegister, IsOneRegister>;
        InstructionKind(FourRegister) {
            TargetRegister destination;
            TargetRegister source;
            TargetRegister source2;
            TargetRegister source3;
        };
        InstructionKind(ThreeRegister) {
            TargetRegister destination;
            TargetRegister source;
            TargetRegister source2;
        };
        struct IsSignedImm16 final { using Type = QuarterInteger; };
        using ThreeByteVariant = std::variant<IsFourRegister, IsThreeRegister, IsSignedImm16>;
        InstructionKind(FiveRegister) {
            TargetRegister destination;
            TargetRegister source0;
            TargetRegister source1;
            TargetRegister source2;
            TargetRegister source3;
        };
        InstructionKind(Imm24) {
            HalfAddress value;
        };
        InstructionKind(TwoRegisterWithImm16) {
            TargetRegister destination;
            TargetRegister source;
            QuarterAddress imm16;
        };
        InstructionKind(OneRegisterWithImm16) {
            TargetRegister destination;
            QuarterAddress imm16;
        };
        using FourByteVariant = std::variant<IsFiveRegister, IsImm24, IsTwoRegisterWithImm16, IsOneRegisterWithImm16>;
        InstructionKind(LoadImm48) {
            TargetRegister destination;
            Address imm48;
        };
        using EightByteVariant = std::variant<IsLoadImm48>;
#undef InstructionKind
        using DecodedArguments = std::variant<NoArguments, OneRegister, TwoRegister, FourRegister, FiveRegister , ThreeRegister, QuarterInteger, IsImm24, TwoRegisterWithImm16, OneRegisterWithImm16, LoadImm48, Imm24>;
        using DecodedInstruction = std::tuple<Operation, DecodedArguments>;
    public:
        static TwoByteVariant getVariant(Operation op, const TwoByte&);
        static ThreeByteVariant getVariant(Operation op, const ThreeByte&);
        static FourByteVariant getVariant(Operation op, const FourByte&);
        static EightByteVariant getVariant(Operation op, const EightByte&);
	public:
		Core();
		~Core() = default;
		//void executionLoop();
		void dispatchInstruction();
        /**
         * Returns the word closest to the target addres
         */
		Datum loadWord(Address addr);
        byte loadByte(Address addr);
        QuarterAddress loadQuarterAddress(Address addr);
        HalfAddress loadHalfAddress(Address addr);
        Address loadLower48(Address addr);
		void store(Address addr, const Datum& value);
        void storeByte(Address addr, byte value);
		Register& getRegister(TargetRegister reg);
		void push(const Datum& d, TargetRegister sp);
		Datum pop(TargetRegister sp);
        void executionCycle(Address startAddress = 0);
		std::function<void(Address, Address)> getMemoryInstallationFunction() noexcept;
		std::function<void(Address, Address)> getInstructionInstallationFunction() noexcept;
	private:
		void push(TargetRegister reg, TargetRegister sp);
		void pop(TargetRegister dest, TargetRegister sp);
		void returnToNative(Operation op);
		void savePositionToSubroutineStack();
		void numericCombine(Operation op);
		void multiplyOperation(Operation op);
		void divideOperation(Operation op);
		void equalsOperation(Operation op);
		void push(Operation op);
		void pop(Operation op);
		void notOperation(Operation op);
		void minusOperation(Operation op);
		void booleanAlgebra(Operation op);
		void shiftOperation(Operation op);
		void powOperation(Operation op);
		void rangeChecks(Operation op);
		void jumpOperation(Operation op);
		void conditionalBranch(Operation op);
		void loadImm48(Operation op);
		void moveOrSwap(Operation op);
		void typeValue(Operation op);
		void loadStore(Operation op);
		void setImm16(Operation op);
        void encodeDecodeBits(Operation op);
		void nop(Operation op);
		void printString(Operation op);
    private:
        // from the current position, perform the entire decode process prior to 
        // executing
        DecodedInstruction decode();
        DecodedInstruction decode(Operation op, const OneByte& b);
        DecodedInstruction decode(Operation op, const TwoByte& b);
        DecodedInstruction decode(Operation op, const ThreeByte& b);
        DecodedInstruction decode(Operation op, const FourByte& b);
        DecodedInstruction decode(Operation op, const EightByte& b);
	private:
		using ThreeRegisterForm = std::tuple<TargetRegister, TargetRegister, TargetRegister>;
		using ThreeRegisterImmediateForm = std::tuple<TargetRegister, TargetRegister, QuarterAddress>;
		using TwoRegisterForm = std::tuple<TargetRegister, TargetRegister>;
		using ThreeRegisterArguments = std::tuple<Register&, Register&, Register&>;
		using TwoRegisterArguments = std::tuple<Register&, Register&>;
        using FourRegisterForm = std::tuple<TargetRegister, TargetRegister, TargetRegister, TargetRegister>;
		using FourRegisterArguments = std::tuple<Register&, Register&, Register&, Register&>;
        using FiveRegisterForm = std::tuple<TargetRegister, TargetRegister, TargetRegister, TargetRegister, TargetRegister>;
		using FiveRegisterArguments = std::tuple<Register&, Register&, Register&, Register&, Register&>;
		ThreeRegisterArguments extractArguments(Operation op, std::function<void(Register&, Address)> onImmediate = nullptr);
        FourRegisterArguments extractArguments4(Operation op);
        FiveRegisterArguments extractArguments5(Operation op);
        FourRegisterForm extractFourRegisterForm();
        FiveRegisterForm extractFiveRegisterForm();
		ThreeRegisterForm extractThreeRegisterForm();
		ThreeRegisterImmediateForm extractThreeRegisterImmediateForm();
		TwoRegisterForm extractTwoRegisterForm();
		QuarterInteger extractQuarterIntegerFromMolecule();
		QuarterAddress extractQuarterAddressFromMolecule();
		byte extractByteFromMolecule();
		Address extractImm48();
		Operation extractOperationFromMolecule();
		void advanceMoleculePosition(Address amount = 1);
	private:
		static constexpr bool inSystemVariableArea(Address value) noexcept {
			return (systemVariableStart <= value) && (systemVariableEnd >= value);
		}
		Datum& getSystemVariable(Address index);
	private:
		Register _a, _b, _c, _s, _x;
		Register _sp, _sp2, _imm, _pc;
        Register _dp, _index;
		Register _tmp0, _tmp1;
        ReadOnlyRegister _zero;
		// mapped to 0xFFFFFFFFFFFF0000
		std::unique_ptr<Datum[]> _memory, _systemVariables;
};
} // end namespace forth
#endif // end CORE_H__
