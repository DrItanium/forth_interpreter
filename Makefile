include config.mk

OBJECTS = forth.o \
		  Datum.o

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

forth.o: forth.cc Problem.h Types.h Datum.h
Datum.o: Datum.cc Types.h Datum.h
