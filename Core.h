#ifndef CORE_H__
#define CORE_H__
#include "Types.h"
#include "Datum.h"
#include <functional>
#include <tuple>
#include "Instruction.h"
#include <memory>
#include <ostream>
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
        static constexpr Address varMemoryCapacity = systemVariableEnd - wordToByteOffset<6>;
        static constexpr Address varLargestMemoryAddress = systemVariableEnd - wordToByteOffset<7>;
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
		struct NoArguments final { 
            void print(std::ostream& out) { }
        };
        struct OneRegister final {
            OneRegister() = default;
            OneRegister(DestinationRegister dest) : destination(dest) { };
            OneRegister(TargetRegister dest) : destination(dest) { };
            OneRegister(const OneRegister& other) : destination(other.destination) { }
            DestinationRegister destination;
            void print(std::ostream& out);
        };
        struct TaggedOneRegister final {
            enum class TypeTag {
                Integer,
                Unsigned,
                FloatingPoint,
                Boolean,
                Char,
                Datum,
            };
            TaggedOneRegister() = default;
            TaggedOneRegister(DestinationRegister dest, TypeTag tag) : destination(dest), type(tag) { };
            TaggedOneRegister(TargetRegister dest, TypeTag tag) : destination(dest), type(tag) { };
            TaggedOneRegister(const TaggedOneRegister& other) : destination(other.destination), type(other.type) { }
            DestinationRegister destination;
            TypeTag type;
            void print(std::ostream& out);
        };
        struct TwoRegister final {
		    TwoRegister() = default;
		    TwoRegister(DestinationRegister dest, SourceRegister src) : destination(dest), source(src) { };
		    TwoRegister(TargetRegister dest, TargetRegister src) : destination(dest), source(src) { };
            TwoRegister(const TwoRegister& other) : destination(other.destination), source(other.source) { }
		    DestinationRegister destination;
		    SourceRegister source;
            void print(std::ostream& out);
        };
        struct ThreeRegister final {
			ThreeRegister() = default;
			ThreeRegister(DestinationRegister dest, SourceRegister src, SourceRegister src2) : destination(dest), source(src), source2(src2) { }
			ThreeRegister(TargetRegister dest, TargetRegister src, TargetRegister src2) : destination(dest), source(src), source2(src2) { }
            ThreeRegister(const ThreeRegister& other) : ThreeRegister(other.destination, other.source, other.source2) { }
            DestinationRegister destination;
            SourceRegister source;
            SourceRegister source2;
            void print(std::ostream& out);
        };
		struct SignedImm16 final {
			SignedImm16() = default;
			SignedImm16(QuarterInteger imm16) : value(imm16) { }
			QuarterInteger value;
            void print(std::ostream& out);
		};
        class Immediate24 final {
            public:
                Immediate24() = default;
                Immediate24(HalfAddress addr) : imm24(addr) { };
                Immediate24(const Immediate24& other) : imm24(other.imm24) { }
                void setImm24(HalfAddress value);
                HalfAddress getImm24() const noexcept { return imm24; }
                void print(std::ostream& out);
            private:
                HalfAddress imm24;
        };
        struct TwoRegisterWithImm16 {
			TwoRegisterWithImm16() = default;
			TwoRegisterWithImm16(DestinationRegister dest, SourceRegister src, QuarterAddress imm) : destination(dest), source(src), imm16(imm) { }
			TwoRegisterWithImm16(TargetRegister dest, TargetRegister src, QuarterAddress imm) : destination(dest), source(src), imm16(imm) { }
            TwoRegisterWithImm16(const TwoRegisterWithImm16& other) : TwoRegisterWithImm16(other.destination, other.source, other.imm16) { }
            DestinationRegister destination;
            SourceRegister source;
            QuarterAddress imm16;
            void print(std::ostream& out);
        };
        struct CustomTwoRegisterWithImm16 final : public TwoRegisterWithImm16 {
            using TwoRegisterWithImm16::TwoRegisterWithImm16;
        };
        struct OneRegisterWithImm16 final {
			OneRegisterWithImm16() = default;
			OneRegisterWithImm16(DestinationRegister dest, QuarterAddress imm) : destination(dest), imm16(imm) { }
			OneRegisterWithImm16(TargetRegister dest, QuarterAddress imm) : destination(dest), imm16(imm) { }
            OneRegisterWithImm16(const OneRegisterWithImm16& other) : OneRegisterWithImm16(other.destination, other.imm16) { }
            DestinationRegister destination;
            QuarterAddress imm16;
            void print(std::ostream& out);
        };
        struct OneRegisterWithImm32 final {
			OneRegisterWithImm32() = default;
			OneRegisterWithImm32(DestinationRegister dest, HalfAddress imm) : destination(dest), imm32(imm) { }
			OneRegisterWithImm32(TargetRegister dest, HalfAddress imm) : destination(dest), imm32(imm) { }
            OneRegisterWithImm32(const OneRegisterWithImm32& other) : OneRegisterWithImm32(other.destination, other.imm32) { }
            DestinationRegister destination;
            HalfAddress imm32;
            void print(std::ostream& out);
        };
        struct OneRegisterWithImm64 final {
			OneRegisterWithImm64() = default;
			OneRegisterWithImm64(DestinationRegister dest, Address imm) : destination(dest), imm64(imm) { }
			OneRegisterWithImm64(TargetRegister dest, Address imm) : destination(dest), imm64(imm) { }
            OneRegisterWithImm64(const OneRegisterWithImm64& other) : OneRegisterWithImm64(other.destination, other.imm64) { }
            DestinationRegister destination;
            Address imm64;
            void print(std::ostream& out);
        };
        template<typename Args, Opcode code>
        struct PrintableInstruction {
            public:
                explicit PrintableInstruction() { }
                explicit PrintableInstruction(const Args& value) : args(value) { }
                ~PrintableInstruction() { }
                void print(std::ostream& out) { 
                    printOpcode(out);
                    printArguments(out);
                }
                void printArguments(std::ostream& out) { args.print(out); }
                virtual void printOpcode(std::ostream& out) = 0;
                constexpr Opcode getOpcode() const noexcept { return code; }
                constexpr byte size() const noexcept { return determineInstructionWidth(getOpcode()).size(); }
                Args args;
        };
#define X(title, b) struct title final : public PrintableInstruction<b, Opcode:: title > { \
    using Parent = PrintableInstruction<b, Opcode:: title >; \
    using Parent::Parent; \
    virtual void printOpcode(std::ostream& out) override { \
        out << #title; \
    } \
};
#define FirstX(title, b) X(title, b)
#include "InstructionData.def"
#undef X
#undef FirstX

		using DecodedOperation = std::variant<
#define X(title, b) , Core:: title
#define FirstX(title, b) Core:: title
#include "InstructionData.def"
#undef X
#undef FirstX
			>;
		using DataEntry = std::variant<std::shared_ptr<DecodedOperation>, 
						 			   DecodedOperation,
						 		  	   std::shared_ptr<Address>,
						 		  	   Address,
						 		  	   HalfAddress,
						 		  	   QuarterAddress,
						 		  	   byte>;
		void installIntoMemory(Address addr, DataEntry entry);
	private:
		void encodeAndInstallIntoMemory(Address addr, byte a);
		void encodeAndInstallIntoMemory(Address addr, Address a);
		void encodeAndInstallIntoMemory(Address addr, std::shared_ptr<Address> a);
		void encodeAndInstallIntoMemory(Address addr, QuarterAddress a);
		void encodeAndInstallIntoMemory(Address addr, HalfAddress a);
		void encodeAndInstallIntoMemory(Address addr, std::shared_ptr<DecodedOperation> a);
		void encodeAndInstallIntoMemory(Address addr, DecodedOperation a);
	private:
        void decodeArguments(OneRegister& args);
        void decodeArguments(TwoRegister& args);
        void decodeArguments(ThreeRegister& args);
        void decodeArguments(SignedImm16& args);
        void decodeArguments(Immediate24& args);
		void decodeArguments(OneRegisterWithImm16&);
		void decodeArguments(TwoRegisterWithImm16&);
		void decodeArguments(OneRegisterWithImm32&);
		void decodeArguments(OneRegisterWithImm64&);
        void decodeArguments(TaggedOneRegister& args);
        std::optional<DecodedOperation> decodeInstruction(byte top);
        template<typename T>
        byte populateDestination(T& args) {
            auto nextByte = extractByteFromMolecule();
            args.destination = forth::getDestinationRegister(nextByte);
            return nextByte;
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
        void encodeArguments(const OneRegister& args);
        void encodeArguments(const TwoRegister& args);
        void encodeArguments(const ThreeRegister& args);
        void encodeArguments(const SignedImm16& args);
        void encodeArguments(const Immediate24& args);
		void encodeArguments(const OneRegisterWithImm16& args);
		void encodeArguments(const TwoRegisterWithImm16& args);
		void encodeArguments(const OneRegisterWithImm32& args);
		void encodeArguments(const OneRegisterWithImm64& args);
		void encodeArguments(const NoArguments& args);
        void encodeArguments(const TaggedOneRegister& args);
		void encodeInstruction(const DecodedOperation& op);
    private:
        void typeValue(const TypeValue& args);
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
        Register _pc, _imm;
#define RegisterFirst(x) ReadOnlyRegister _ ## x;
#define Register(x) Register _ ## x;
#include "Registers.def"
#undef Register
#undef RegisterFirst
		//Register _a, _b, _c, _s, _x;
		//Register _sp, _sp2, _imm, _pc;
        //Register _dp, _index;
		//Register _tmp0, _tmp1;
        //ReadOnlyRegister _zero;
		// mapped to 0xFFFFFFFFFFFF0000
		std::unique_ptr<Datum[]> _memory, _systemVariables;
};
} // end namespace forth
#endif // end CORE_H__
