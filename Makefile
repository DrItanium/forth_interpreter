include config.mk

OBJECTS = forth.o \
		  Datum.o \
		  DictionaryEntry.o \
		  Machine.o \
		  Problem.o \
		  Core.o \
		  Instruction.o \
		  Assembler.o

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

forth.o: forth.cc Machine.h Instruction.h Assembler.h
Datum.o: Datum.cc Types.h Datum.h Instruction.h
Machine.o: Machine.cc Machine.h Types.h DictionaryEntry.h Datum.h Instruction.h Problem.h Core.h Assembler.h
DictionaryEntry.o: DictionaryEntry.cc DictionaryEntry.h Types.h Datum.h Problem.h Machine.h Instruction.h Problem.h
Problem.o: Problem.cc Problem.h
Core.o: Core.cc Core.h Instruction.h Datum.h Types.h Problem.h
Instruction.o: Instruction.cc Instruction.h Types.h Problem.h
Assembler.o: Assembler.cc Assembler.h Types.h Instruction.h Datum.h Problem.h
