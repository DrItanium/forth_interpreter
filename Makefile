include config.mk

OBJECTS = forth.o \
		  Datum.o \
		  DictionaryEntry.o \
		  Machine.o \
		  Problem.o 

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

forth.o: forth.cc Machine.h Instruction.h
Datum.o: Datum.cc Types.h Datum.h Instruction.h 
Machine.o: Machine.cc Machine.h Types.h DictionaryEntry.h Datum.h Instruction.h Problem.h
DictionaryEntry.o: DictionaryEntry.cc DictionaryEntry.h Types.h Datum.h Problem.h Machine.h Instruction.h Problem.h
Problem.o: Problem.cc Problem.h
