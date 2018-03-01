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
		using DestinationRegister = std::optional<std::reference_wrapper<Register>>;
		using SourceRegister = std::optional<std::reference_wrapper<const Register>>;
#define OperationKind(x) \
        struct x ; \
        struct Is ## x final { using Type = x ; }; \
        struct x final
        OperationKind(TwoRegister) {
            DestinationRegister destination;
            SourceRegister source;
        };
        OperationKind(OneRegister) {
            DestinationRegister destination;
        };
        using TwoByteVariant = std::variant<IsTwoRegister, IsOneRegister>;
        OperationKind(FourRegister) {
            DestinationRegister destination;
            SourceRegister source;
            SourceRegister source2;
            SourceRegister source3;
        };
        OperationKind(ThreeRegister) {
            DestinationRegister destination;
            SourceRegister source;
            SourceRegister source2;
        };
		OperationKind(SignedImm16) {
			QuarterInteger value;
		};
        using ThreeByteVariant = std::variant<IsFourRegister, IsThreeRegister, IsSignedImm16>;
        OperationKind(FiveRegister) {
            DestinationRegister destination;
			SourceRegister source0;
			SourceRegister source1;
			SourceRegister source2;
			SourceRegister source3;
        };
        OperationKind(Imm24) {
            HalfAddress value;
        };
        OperationKind(TwoRegisterWithImm16) {
            DestinationRegister destination;
            SourceRegister source;
            QuarterAddress imm16;
        };
        OperationKind(OneRegisterWithImm16) {
            DestinationRegister destination;
            QuarterAddress imm16;
        };
        using FourByteVariant = std::variant<IsFiveRegister, IsImm24, IsTwoRegisterWithImm16, IsOneRegisterWithImm16>;
        OperationKind(LoadImm48) {
            DestinationRegister destination;
            Address imm48;
        };
        using EightByteVariant = std::variant<IsLoadImm48>;
#undef OperationKind
        using DecodedArguments = std::variant<OneRegister, TwoRegister, FourRegister, FiveRegister , ThreeRegister, QuarterInteger, IsImm24, TwoRegisterWithImm16, OneRegisterWithImm16, LoadImm48, Imm24>;
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
		void savePositionToSubroutineStack();
	private:
		void returnToNative(Operation op, DecodedArguments args);
		void numericCombine(Operation op, DecodedArguments args);
		void multiplyOperation(Operation op, DecodedArguments args);
		void divideOperation(Operation op, DecodedArguments args);
		void equalsOperation(Operation op, DecodedArguments args);
		void push(Operation op, DecodedArguments args);
		void pop(Operation op, DecodedArguments args);
		void notOperation(Operation op, DecodedArguments args);
		void minusOperation(Operation op, DecodedArguments args);
		void booleanAlgebra(Operation op, DecodedArguments args);
		void shiftOperation(Operation op, DecodedArguments args);
		void powOperation(Operation op, DecodedArguments args);
		void rangeChecks(Operation op, DecodedArguments args);
		void jumpOperation(Operation op, DecodedArguments args);
		void conditionalBranch(Operation op, DecodedArguments args);
		void loadImm48(Operation op, DecodedArguments args);
		void moveOrSwap(Operation op, DecodedArguments args);
		void typeValue(Operation op, DecodedArguments args);
		void loadStore(Operation op, DecodedArguments args);
		void setImm16(Operation op, DecodedArguments args);
        void encodeDecodeBits(Operation op, DecodedArguments args);
		void nop(Operation op, DecodedArguments args);
		void printString(Operation op, DecodedArguments args);
    private:
		Register& getDestinationRegister(byte value);
		Register& getSourceRegister(byte value);
        // from the current position, perform the entire decode process prior to 
        // executing
        DecodedInstruction decode();
        DecodedInstruction decode(Operation op, const OneByte& b);
        DecodedInstruction decode(Operation op, const TwoByte& b);
        DecodedInstruction decode(Operation op, const ThreeByte& b);
        DecodedInstruction decode(Operation op, const FourByte& b);
        DecodedInstruction decode(Operation op, const EightByte& b);
	private:
		QuarterInteger extractQuarterIntegerFromMolecule();
		QuarterAddress extractQuarterAddressFromMolecule();
		byte extractByteFromMolecule();
		Address extractImm48();
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
