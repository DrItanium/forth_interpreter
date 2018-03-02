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
        using FourByteSelector = std::variant<IsFiveRegister, IsImm24, IsTwoRegisterWithImm16, IsOneRegisterWithImm16>;
        using FourByteVariant = std::variant<FiveRegister, Imm24, TwoRegisterWithImm16, OneRegisterWithImm16>;
        OperationKind(LoadImm48) {
            DestinationRegister destination;
            Address imm48;
        };
        using EightByteSelector = std::variant<IsLoadImm48>;
		using EightByteVariant = std::variant<LoadImm48>;
#undef OperationKind
		using DecodedArguments = std::variant<OneByteVariant, TwoByteVariant, ThreeByteVariant, FourByteVariant, EightByteVariant>;
	private:
		struct FloatingPoint { };
		struct Signed { };
		struct Unsigned { };
		struct Boolean { };
		struct RegisterType { };
		struct ImmediateType { };
		using RegisterImmediate = std::variant<RegisterType, ImmediateType>;
		// Begin Types
		struct Lowest { };
		struct Lower { };
		struct Higher { };
		struct Highest { };
		using SetImmediate16 = std::variant<Lowest, Lower, Higher, Highest>;
		struct LoadImmediateLower48 { };
		struct LessThan { };
		struct GreaterThan { };
		struct RangeCheckOperation {
			std::variant<LessThan, GreaterThan> kind;
			std::variant<Signed, Unsigned, FloatingPoint> type;
			RegisterImmediate args;
		};
		struct EqualityOperation {
			std::variant<Signed, Unsigned, FloatingPoint, Boolean> type;
			RegisterImmediate args;
		};
		struct And { };
		struct Or { };
		struct Xor { };
		struct Logical {
			std::variant<And, Or, Xor> kind;
			std::variant<Signed, Unsigned, Boolean> type;
			RegisterImmediate args;
		};

		struct Not {
			std::variant<Signed, Unsigned, Boolean> type;
		};


		struct MinusOperation {
			std::variant<Signed, Unsigned, FloatingPoint> type;
		};
		struct Add { };
		struct Subtract { };
		struct Multiply { };
		struct Divide { };
		struct Pow { };
		struct MathOperation {
			std::variant<Add, Subtract, Multiply, Divide, Pow> kind;
			std::variant<Signed, Unsigned, FloatingPoint> type;
			RegisterImmediate args;
		};
		struct ModuloOperation {
			std::variant<Signed, Unsigned> type;
			RegisterImmediate args;
		};
		struct ShiftLeft { };
		struct ShiftRight { };
		struct ShiftOperation {
			RegisterImmediate args;
			std::variant<ShiftLeft, ShiftRight> direction;
			std::variant<Signed, Unsigned> type;
		};
		struct Jump { };
		struct JumpIndirect { };
		struct JumpAbsolute { };
		struct CallSubroutine { };
		struct CallSubroutineIndirect { };
		struct ReturnSubroutine { };
		using JumpOperation = std::variant<Jump, JumpIndirect, JumpAbsolute, CallSubroutine, CallSubroutineIndirect, ReturnSubroutine> ;
		struct ConditionalBranch { };
		struct ConditionalBranchIndirect { };
		struct ConditionalCallSubroutine { };
		struct ConditionalCallSubroutineIndirect { };
		struct ConditionalReturnSubroutine { };

		struct Move { };
		struct Swap { };
		struct Load { };
		struct Store { };
		using RegisterManipulator = std::variant<Move, Swap, Load, Store>;
		struct Push { };
		struct Pop { };
		using StackModifiers = std::variant<Push, Pop>;
		struct Nop { };
		struct ReturnToNative { };
		struct EncodeBits { };
		struct DecodeBits { };
		using EncodeDecodeOps = std::variant<EncodeBits, DecodeBits>;
		struct PrintString { };
		struct PrintChar { };
		struct TypeDatum { };
		struct TypeValue {
			std::variant<Signed, Unsigned, FloatingPoint, Boolean> type;
		};
		using PrintRoutines = std::variant<PrintString, PrintChar, TypeDatum, TypeValue>;
		using ImmediateManipulators = std::variant<SetImmediate16, LoadImmediateLower48>;
		using ComparisonOperation = std::variant<RangeCheckOperation, EqualityOperation>;
		using LogicalOperation = std::variant<Logical, Not>;
		using ALUOperation = std::variant<MinusOperation, MathOperation, ModuloOperation, ShiftOperation>;
		using ConditionalJumpOperation = std::variant< ConditionalBranch, ConditionalBranchIndirect, ConditionalCallSubroutine, ConditionalCallSubroutineIndirect, ConditionalReturnSubroutine>;

		using BranchOperation = std::variant<JumpOperation, ConditionalJumpOperation>;
		using MiscOperations = std::variant<Nop, ReturnToNative>;
		using DecodedOpcode = std::variant<PrintRoutines, 
			  EncodeDecodeOps, 
			  MiscOperations, 
			  StackModifiers, 
			  RegisterManipulator, 
			  BranchOperation, 
			  ALUOperation, 
			  LogicalOperation, 
			  ImmediateManipulators>;
        using DecodedInstruction = std::tuple<DecodedOpcode, DecodedArguments>;
	private:
		void dispatchInstruction(PrintRoutines op, DecodedArguments args);
		void dispatchInstruction(EncodeDecodeOps op, DecodedArguments args);
		void dispatchInstruction(MiscOperations op, DecodedArguments args);
		void dispatchInstruction(StackModifiers op, DecodedArguments args);
		void dispatchInstruction(ALUOperation op, DecodedArguments args);
		void dispatchInstruction(BranchOperation op, DecodedArguments args);
		void dispatchInstruction(ComparisonOperation op, DecodedArguments args);
		void dispatchInstruction(RegisterManipulator op, DecodedArguments args);
		void dispatchInstruction(ImmediateManipulators op, DecodedArguments args);
    private:
        static std::optional<OneByteSelector> getVariant(Operation op, const OneByte&);
        static std::optional<TwoByteSelector> getVariant(Operation op, const TwoByte&);
        static std::optional<ThreeByteSelector> getVariant(Operation op, const ThreeByte&);
        static std::optional<FourByteSelector> getVariant(Operation op, const FourByte&);
        static std::optional<EightByteSelector> getVariant(Operation op, const EightByte&);

    private:
		Register& getDestinationRegister(byte value);
		Register& getSourceRegister(byte value);
        // from the current position, perform the entire decode process prior to 
        // executing
		DecodedOpcode decode(Operation op);
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
