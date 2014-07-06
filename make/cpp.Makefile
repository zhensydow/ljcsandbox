CC=gcc
CXX=g++
RM=rm -f
CFLAGS=
CPPFLAGS=
CXXFLAGS=-std=c++11 -Wall -Werror
LDFLAGS=
LDLIBS=

HEADERS=Class.hpp
SRCS=main.cpp Class.cpp
OBJS=$(subst .cpp,.o,$(SRCS))
PROGRAM=program

all: $(PROGRAM)

clean:
	$(RM) *~ $(OBJS)

dist-clean: clean
	$(RM) $(PROGRAM)

$(PROGRAM): $(OBJS)
	$(CXX) $(LDFLAGS) -o $(PROGRAM) $(OBJS) $(LDLIBS)

Class.o: Class.cpp $(HEADERS)
main.o: main.cpp $(HEADERS)

