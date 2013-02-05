PROGRAM=example

all: ${PROGRAM}

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

${PROGRAM}: main.o shaders.o fileutils.o
	${CXX} $^ ${LIBS} -o ${PROGRAM}

clean:
	rm -f *.o onionworld
