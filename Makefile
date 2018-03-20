include config.mk

OBJECTS = forth.o \
		  Datum.o \
		  DictionaryEntry.o \
		  Machine.o \
		  Problem.o \
		  Core.o \
		  Instruction.o \
		  Assembler.o \
		  Types.o

OUTPUT_BINARY = forth

all: options ${OUTPUT_BINARY}

options:
	@echo build options:
	@echo "CXXFLAGS  = ${CXXFLAGS}"
	@echo "LDFLAGS   = ${LDFLAGS}"
	@echo "CXX       = ${CXX}"

${OUTPUT_BINARY}: ${OBJECTS}
	@echo Building ${OUTPUT_BINARY}
	@${CXX} ${LDFLAGS} -o ${OUTPUT_BINARY} ${OBJECTS}

%.o: %.cc
	@echo CXX $<
	@${CXX} ${CXXFLAGS} -c $< -o $@

clean:
	@echo Cleaning...
	@rm -f ${OUTPUT_BINARY} ${OBJECTS}

.PHONY: options clean all

Assembler.o: Assembler.cc Types.h Instruction.h Problem.h Registers.def \
 InstructionData.def Assembler.h Datum.h Core.h Machine.h \
 DictionaryEntry.h UserVariables.def
Core.o: Core.cc Types.h Core.h Datum.h Instruction.h Problem.h \
 Registers.def InstructionData.def DictionaryEntry.h
Datum.o: Datum.cc Types.h Datum.h Instruction.h Problem.h Registers.def \
 InstructionData.def
DictionaryEntry.o: DictionaryEntry.cc Datum.h Types.h Instruction.h \
 Problem.h Registers.def InstructionData.def DictionaryEntry.h Machine.h \
 Core.h Assembler.h UserVariables.def
Instruction.o: Instruction.cc Instruction.h Types.h Problem.h \
 Registers.def InstructionData.def
Machine.o: Machine.cc Types.h Problem.h Datum.h Instruction.h \
 Registers.def InstructionData.def DictionaryEntry.h Machine.h Core.h \
 Assembler.h UserVariables.def
Problem.o: Problem.cc Problem.h
Types.o: Types.cc Types.h Problem.h
forth.o: forth.cc Machine.h Types.h DictionaryEntry.h Datum.h \
 Instruction.h Problem.h Registers.def InstructionData.def Core.h \
 Assembler.h UserVariables.def
