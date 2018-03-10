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
		//std::function<void(Address, Address)> getInstructionInstallationFunction() noexcept;
	private:
        void storeAndAdvance(Register& reg, byte value);
        void storeAndAdvance(Register& reg, QuarterAddress value);
        void storeAndAdvance(Register& reg, HalfAddress value);
        void storeAndAdvance(Register& reg, Address value);
		void push(TargetRegister reg, TargetRegister sp);
		void pop(TargetRegister dest, TargetRegister sp);
		void savePositionToSubroutineStack();
	public:
        using DestinationRegister = forth::OptionalRegister;
        using SourceRegister = forth::OptionalRegister;
#define OperationKind(x) struct x final
	OperationKind(NoArguments) { };
        OperationKind(TwoRegister) {
		TwoRegister() = default;
		TwoRegister(DestinationRegister dest, SourceRegister src) : destination(dest), source(src) { };
		TwoRegister(TargetRegister dest, TargetRegister src) : destination(dest), source(src) { };
		DestinationRegister destination;
		SourceRegister source;
        };
        OperationKind(OneRegister) {
		OneRegister() = default;
		OneRegister(DestinationRegister dest) : destination(dest) { };
		OneRegister(TargetRegister dest) : destination(dest) { };
		DestinationRegister destination;
        };
        OperationKind(FourRegister) {
			FourRegister() = default;
			FourRegister(DestinationRegister dest, SourceRegister src, SourceRegister src2, SourceRegister src3) : destination(dest), source(src), source2(src2), source3(src3) { }
			FourRegister(TargetRegister dest, TargetRegister src, TargetRegister src2, TargetRegister src3) : destination(dest), source(src), source2(src2), source3(src3) { }
            DestinationRegister destination;
            SourceRegister source;
            SourceRegister source2;
            SourceRegister source3;
        };
        OperationKind(ThreeRegister) {
			ThreeRegister() = default;
			ThreeRegister(DestinationRegister dest, SourceRegister src, SourceRegister src2) : destination(dest), source(src), source2(src2) { }
			ThreeRegister(TargetRegister dest, TargetRegister src, TargetRegister src2) : destination(dest), source(src), source2(src2) { }
            DestinationRegister destination;
            SourceRegister source;
            SourceRegister source2;
        };
		OperationKind(SignedImm16) {
			SignedImm16() = default;
			SignedImm16(QuarterInteger imm16) : value(imm16) { }
			QuarterInteger value;
		};
        OperationKind(FiveRegister) {
			FiveRegister() = default;
			FiveRegister(DestinationRegister dest, SourceRegister src, SourceRegister src2, SourceRegister src3, SourceRegister src4) : destination(dest), source(src), source2(src2), source3(src3), source4(src4) { }
			FiveRegister(TargetRegister dest, TargetRegister src, TargetRegister src2, TargetRegister src3, TargetRegister src4) : destination(dest), source(src), source2(src2), source3(src3), source4(src4) { }
            DestinationRegister destination;
			SourceRegister source;
			SourceRegister source2;
			SourceRegister source3;
			SourceRegister source4;
        };
        OperationKind(Immediate24) {
			Immediate24() = default;
			Immediate24(HalfAddress addr) : value(addr) { };
            HalfAddress value;
        };
        OperationKind(TwoRegisterWithImm16) {
			TwoRegisterWithImm16() = default;
			TwoRegisterWithImm16(DestinationRegister dest, SourceRegister src, QuarterAddress imm) : destination(dest), source(src), imm16(imm) { }
			TwoRegisterWithImm16(TargetRegister dest, TargetRegister src, QuarterAddress imm) : destination(dest), source(src), imm16(imm) { }
            DestinationRegister destination;
            SourceRegister source;
            QuarterAddress imm16;
        };
        OperationKind(OneRegisterWithImm16) {
			OneRegisterWithImm16() = default;
			OneRegisterWithImm16(DestinationRegister dest, QuarterAddress imm) : destination(dest), imm16(imm) { }
			OneRegisterWithImm16(TargetRegister dest, QuarterAddress imm) : destination(dest), imm16(imm) { }
            DestinationRegister destination;
            QuarterAddress imm16;
        };
        OperationKind(OneRegisterWithImm32) {
			OneRegisterWithImm32() = default;
			OneRegisterWithImm32(DestinationRegister dest, HalfAddress imm) : destination(dest), imm32(imm) { }
			OneRegisterWithImm32(TargetRegister dest, HalfAddress imm) : destination(dest), imm32(imm) { }
            DestinationRegister destination;
            HalfAddress imm32;
        };
        OperationKind(OneRegisterWithImm64) {
			OneRegisterWithImm64() = default;
			OneRegisterWithImm64(DestinationRegister dest, Address imm) : destination(dest), imm64(imm) { }
			OneRegisterWithImm64(TargetRegister dest, Address imm) : destination(dest), imm64(imm) { }
            DestinationRegister destination;
            Address imm64;
        };
#undef OperationKind
#define OneByte(title) struct title final { \
	OneByteOpcode getOpcode() { return  OneByteOpcode:: title ; } \
	byte size() { return std::visit([](auto&& value) { return value.size(); }, determineInstructionWidth(getOpcode())); } \
	};
#define TwoByte(title, b) struct title final { \
	TwoByteOpcode getOpcode() { return  TwoByteOpcode:: title ; } \
	byte size() { return std::visit([](auto&& value) { return value.size(); }, determineInstructionWidth(getOpcode())); } \
	Core:: b args; }; 
#define ThreeByte(title, b) struct title final { \
	ThreeByteOpcode getOpcode() { return  ThreeByteOpcode:: title ; } \
	byte size() { return std::visit([](auto&& value) { return value.size(); }, determineInstructionWidth(getOpcode())); } \
	Core:: b args; };  
#define FourByte(title, b) struct title final { \
	FourByteOpcode getOpcode() { return  FourByteOpcode:: title ; } \
	byte size() { return std::visit([](auto&& value) { return value.size(); }, determineInstructionWidth(getOpcode())); } \
	Core:: b args; }; 
#define GrabBag(title, b) struct title final { \
	GrabBagOpcode getOpcode() { return  GrabBagOpcode:: title ; } \
	byte size() { return std::visit([](auto&& value) { return value.size(); }, determineInstructionWidth(getOpcode())); } \
	Core:: b args; }; 
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef GrabBag

//        using OneByteOperation = std::variant<
//#define OneByte(title) Core::title,
//#define TwoByte(title, b) 
//#define ThreeByte(title, b) 
//#define FourByte(title, b)
//#define GrabBag(title, b) 
//#include "InstructionData.def"
//#undef OneByte
//#undef TwoByte
//#undef ThreeByte
//#undef FourByte
//#undef GrabBag
//            UndefinedOpcode
//            >;
//        using TwoByteOperation = 
//            std::variant<
//#define OneByte(title)
//#define TwoByte(title, b) Core:: title,
//#define ThreeByte(title, b) 
//#define FourByte(title, b)
//#define GrabBag(title, b) 
//#include "InstructionData.def"
//#undef OneByte
//#undef TwoByte
//#undef ThreeByte
//#undef FourByte
//#undef GrabBag
//                UndefinedOpcode>;
//        using ThreeByteOperation =
//            std::variant<
//#define OneByte(title)
//#define TwoByte(title, b) 
//#define ThreeByte(title, b) Core:: title,
//#define FourByte(title, b)
//#define GrabBag(title, b) 
//#include "InstructionData.def"
//#undef OneByte
//#undef TwoByte
//#undef ThreeByte
//#undef FourByte
//#undef GrabBag
//                UndefinedOpcode> ;
//
//        using FourByteOperation =
//            std::variant<
//#define OneByte(title)
//#define TwoByte(title, b) 
//#define ThreeByte(title, b) 
//#define FourByte(title, b) Core:: title,
//#define GrabBag(title, b) 
//#include "InstructionData.def"
//#undef OneByte
//#undef TwoByte
//#undef ThreeByte
//#undef FourByte
//#undef GrabBag
//                UndefinedOpcode>;
//        using GrabBagOperation = 
//            std::variant<
//#define OneByte(title)
//#define TwoByte(title, b) 
//#define ThreeByte(title, b) 
//#define FourByte(title, b) 
//#define GrabBag(title, b) Core:: title,
//#include "InstructionData.def"
//#undef OneByte
//#undef TwoByte
//#undef ThreeByte
//#undef FourByte
//#undef GrabBag
                //UndefinedOpcode>;

//        using DecodedOperation = std::variant<
//			OneByteOperation, 
//			TwoByteOperation, 
//              ThreeByteOperation, 
//              FourByteOperation, 
//              GrabBagOperation>;
//
		using DecodedOperation = std::variant<
#define OneByte(title) Core:: title ,
#define TwoByte(title, b) Core:: title ,
#define ThreeByte(title, b) Core:: title ,
#define FourByte(title, b) Core:: title ,
#define GrabBag(title, b) Core:: title,
#include "InstructionData.def"
#undef OneByte
#undef TwoByte
#undef ThreeByte
#undef FourByte
#undef GrabBag
			UndefinedOpcode>;

	private:
        void decodeArguments(OneRegister& args);
        void decodeArguments(TwoRegister& args);
        void decodeArguments(ThreeRegister& args);
        void decodeArguments(FourRegister& args);
        void decodeArguments(FiveRegister& args);
        void decodeArguments(SignedImm16& args);
        void decodeArguments(Immediate24& args);
		void decodeArguments(OneRegisterWithImm16&);
		void decodeArguments(TwoRegisterWithImm16&);
		void decodeArguments(OneRegisterWithImm32&);
		void decodeArguments(OneRegisterWithImm64&);
        std::optional<DecodedOperation> decodeInstruction(byte control, OneByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, TwoByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, ThreeByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, FourByteInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte control, GrabBagInstruction op);
        std::optional<DecodedOperation> decodeInstruction(byte top);
        template<typename T>
        void populateDestination(T& args) {
            auto nextByte = extractByteFromMolecule();
            args.destination = forth::getDestinationRegister(nextByte);
        }
        template<typename T>
        void populateDestinationAndSource(T& args) {
            auto nextByte = extractByteFromMolecule();
            args.destination = forth::getDestinationRegister(nextByte);
            args.source = forth::getSourceRegister(nextByte);
        }
        void encodeArguments(const DestinationRegister& dest, const SourceRegister& src);
        void encodeArguments(const DestinationRegister& dest);
        void encodeArguments(HalfAddress value);
        void encodeArguments(QuarterAddress value);
        void encodeArguments(OneRegister& args);
        void encodeArguments(TwoRegister& args);
        void encodeArguments(ThreeRegister& args);
        void encodeArguments(FourRegister& args);
        void encodeArguments(FiveRegister& args);
        void encodeArguments(SignedImm16& args);
        void encodeArguments(Immediate24& args);
		void encodeArguments(OneRegisterWithImm16& args);
		void encodeArguments(TwoRegisterWithImm16& args);
		void encodeArguments(OneRegisterWithImm32& args);
		void encodeArguments(OneRegisterWithImm64& args);

    private:
		void dispatchOperation(const OneByteOperation& op);
		void dispatchOperation(const TwoByteOperation& op);
		void dispatchOperation(const ThreeByteOperation& op);
		void dispatchOperation(const FourByteOperation& op);
		void dispatchOperation(const GrabBagOperation& op);
		Register& getDestinationRegister(const DestinationRegister& reg);
		Register& getSourceRegister(const SourceRegister& reg);
		template<typename T>
		Register& getDestinationRegister(const T& reg) {
			return getDestinationRegister(reg.destination);
		}
		template<typename T>
		Register& getSourceRegister(const T& reg) {
			return getSourceRegister(reg.source);
		}
		template<typename T>
		const Register& getSource2Register(const T& reg) {
			return getSourceRegister(reg.source2);
		}
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
