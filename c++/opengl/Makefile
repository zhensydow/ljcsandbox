.SECONDARY:
.PHONY: clean

PROGRAMS=$(patsubst main%.cpp,example%,$(wildcard main*.cpp))

all: ${PROGRAMS}

GCCVERSION=$(shell gcc -dumpversion)

GLCFLAGS=$(shell pkg-config --cflags gl)
CPPFLAGS=-Wall -O3 ${GLCFLAGS}
ifneq (,$(findstring 4.7, ${GCCVERSION}))
	CXXFLAGS=-std=c++11
else
	CXXFLAGS=-std=c++0x
endif

GLLIBS=$(shell pkg-config --libs gl)
LIBS=${GLLIBS} -lglut

example%: main%.o shaders.o fileutils.o
	${CXX} $^ ${LIBS} -o $@

clean:
	rm -f *.o ${PROGRAMS}
