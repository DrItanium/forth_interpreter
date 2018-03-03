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
		using DestinationRegister = std::optional<std::reference_wrapper<Register>>;
		using SourceRegister = std::optional<std::reference_wrapper<const Register>>;
#define OperationKind(x) \
        struct x ; \
        struct Is ## x final { using Type = x ; }; \
        struct x final
		OperationKind(NoArguments) { };
		using OneByteSelector = std::variant<IsNoArguments>;
		using OneByteVariant = std::variant<NoArguments>;
        OperationKind(TwoRegister) {
            DestinationRegister destination;
            SourceRegister source;
        };
        OperationKind(OneRegister) {
            DestinationRegister destination;
        };
        using TwoByteSelector = std::variant<IsTwoRegister, IsOneRegister>;
		using TwoByteVariant = std::variant<OneRegister, TwoRegister>;
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
        using ThreeByteSelector = std::variant<IsFourRegister, IsThreeRegister, IsSignedImm16>;
		using ThreeByteVariant = std::variant<ThreeRegister, FourRegister, SignedImm16>;
        OperationKind(FiveRegister) {
            DestinationRegister destination;
			SourceRegister source0;
			SourceRegister source1;
			SourceRegister source2;
			SourceRegister source3;
        };
        OperationKind(Immediate24) {
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
        using FourByteSelector = std::variant<IsFiveRegister, IsImmediate24, IsTwoRegisterWithImm16, IsOneRegisterWithImm16>;
        using FourByteVariant = std::variant<FiveRegister, Immediate24, TwoRegisterWithImm16, OneRegisterWithImm16>;
        OperationKind(LoadImm48) {
            DestinationRegister destination;
            Address imm48;
        };
        using EightByteSelector = std::variant<IsLoadImm48>;
		using EightByteVariant = std::variant<LoadImm48>;
#undef OperationKind
		using DecodedArguments = std::variant<OneByteVariant, TwoByteVariant, ThreeByteVariant, FourByteVariant, EightByteVariant>;
	private:

        using ExtendedTwoRegister0 = TwoRegister;
        using ExtendedTwoRegister1 = TwoRegister;
#define OneByte(title) struct title final { };
#define TwoByte(title, b) struct title final { Core:: b args ; };
#define ThreeByte(title, b) struct title final { Core:: b args; };
#define FourByte(title, b) struct title final { Core:: b args; };
#define FiveByte(title, b) struct title final { Core:: b args; };
#define EightByte(title, b)  struct title final { Core:: b args; };
#define GrabBag(title, b) struct title final { Core:: b args; };
#define ExtendedVariantTenByte(title, b) struct title final { Core:: b args; };
#define ExtendedVariantSixByte(title, b) struct title final { Core:: b args; };
#define ExtendedVariant(st, b, c) INDIRECTION(ExtendedVariant, st)(b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
        struct OneByteOperation {

        std::variant<
#define OneByte(title) Core::title,
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b)
#define ExtendedVariantSixByte(title, b) 
#define ExtendedVariant(st, b, c) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
            UndefinedOpcode
            > kind;
        };
        struct TwoByteOperation {
            std::variant<
#define OneByte(title)
#define TwoByte(title, b) Core:: title,
#define ThreeByte(title, b) 
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b)
#define ExtendedVariantSixByte(title, b) 
#define ExtendedVariant(st, b, c) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
                UndefinedOpcode> kind;
        };
        struct ThreeByteOperation {
            std::variant<
#define OneByte(title)
#define TwoByte(title, b) 
#define ThreeByte(title, b) Core:: title,
#define FourByte(title, b)
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b)
#define ExtendedVariantSixByte(title, b) 
#define ExtendedVariant(st, b, c) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
                UndefinedOpcode> kind;
        };

        struct FourByteOperation {
            std::variant<
#define OneByte(title)
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b) Core:: title,
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b)
#define ExtendedVariantSixByte(title, b) 
#define ExtendedVariant(st, b, c) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
                UndefinedOpcode> kind;
        };
        struct FiveByteOperation {
            std::variant<
#define OneByte(title)
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b) 
#define FiveByte(title, b) Core:: title,
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b)
#define ExtendedVariantSixByte(title, b) 
#define ExtendedVariant(st, b, c) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
                UndefinedOpcode> kind;
        };

        struct EightByteOperation {
            std::variant<
#define OneByte(title)
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b) 
#define FiveByte(title, b) 
#define EightByte(title, b) Core:: title,
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b)
#define ExtendedVariantSixByte(title, b) 
#define ExtendedVariant(st, b, c) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
                UndefinedOpcode> kind;
        };
        struct GrabBagOperation {
            std::variant<
#define OneByte(title)
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b) 
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) Core:: title,
#define ExtendedVariantTenByte(title, b)
#define ExtendedVariantSixByte(title, b) 
#define ExtendedVariant(st, b, c) 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
                UndefinedOpcode> kind;
        };

        struct TenByteOperation {
            std::variant<
#define OneByte(title)
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b) 
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b) Core:: title,
#define ExtendedVariantSixByte(title, b) 
#define ExtendedVariant(st, b, c) INDIRECTION(ExtendedVariant, st)(b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
                UndefinedOpcode> kind;
        };

        struct SixByteOperation {
            std::variant<
#define OneByte(title)
#define TwoByte(title, b) 
#define ThreeByte(title, b) 
#define FourByte(title, b) 
#define FiveByte(title, b) 
#define EightByte(title, b) 
#define GrabBag(title, b) 
#define ExtendedVariantTenByte(title, b) 
#define ExtendedVariantSixByte(title, b) Core:: title,
#define ExtendedVariant(st, b, c) INDIRECTION(ExtendedVariant, st)(b, c)
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef FiveByte
#undef EightByte
#undef GrabBag
#undef ExtendedVariant
#undef ExtendedVariantSixByte
#undef ExtendedVariantTenByte
                UndefinedOpcode> kind;
        };

        using ExtendedVariantOperation = std::variant<TenByteOperation, SixByteOperation>;
        using DecodedOperation = std::variant<OneByteOperation, TwoByteOperation, 
              ThreeByteOperation, 
              FourByteOperation, 
              FiveByteOperation, 
              EightByteOperation,
              GrabBagOperation,
              ExtendedVariantOperation>;
	private:
        void decodeArguments(OneRegister& args);
        void decodeArguments(TwoRegister& args);
        void decodeArguments(ThreeRegister& args);
        void decodeArguments(FourRegister& args);
        void decodeArguments(FiveRegister& args);
        void decodeArguments(SignedImm16& args);
        void decodeArguments(Immediate24& args);
        std::optional<DecodedOperation> decodeInstruction(byte control, OneByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, TwoByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, ThreeByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, FourByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, FiveByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, SixByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, EightByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, TenByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, GrabBagInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, ExtendedVariantInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte top);
    private:
		Register& getDestinationRegister(byte value);
		Register& getSourceRegister(byte value);
        // from the current position, perform the entire decode process prior to 
        // executing
		//std::optional<DecodedOpcode> decodeOpcode(Operation op);
        //DecodedInstruction decode();
        //DecodedInstruction decode(Operation op, const OneByte& b);
        //DecodedInstruction decode(Operation op, const TwoByte& b);
        //DecodedInstruction decode(Operation op, const ThreeByte& b);
        //DecodedInstruction decode(Operation op, const FourByte& b);
        //DecodedInstruction decode(Operation op, const EightByte& b);
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
