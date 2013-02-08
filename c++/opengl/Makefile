.PHONY : clean

PROGRAMS=$(patsubst main%.cpp,example%,$(wildcard main*.cpp))

all: ${PROGRAMS}

GCCVERSION=$(shell gcc -dumpversion)

GLCFLAGS=$(shell pkg-config --cflags gl)
CPPFLAGS=-Wall -O2 ${GLCFLAGS}
ifeq "${GCCVERSION}" "4.5.2"
	CXXFLAGS=-std=c++0x
else
	CXXFLAGS=-std=c++11
endif

GLLIBS=$(shell pkg-config --libs gl)
LIBS=${GLLIBS} -lglut

example%: main%.o shaders.o fileutils.o
	${CXX} $^ ${LIBS} -o $@

clean:
	rm -f *.o ${PROGRAMS}
