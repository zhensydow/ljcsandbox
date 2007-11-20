CC = gcc
CFLAGS = -Wall -Werror
LDFLAGS = -lstdc++
HEADERS = Class.h
SOURCES = main.cpp Class.cpp
OBJECTS = $(SOURCES:.cpp=.o)
PROGRAM = program

all: $(PROGRAM)

clean:
	rm -rf *~ *.o $(PROGRAM)

%.o: %.cpp $(HEADERS)
	$(CC) $(CFLAGS) -c $<

$(PROGRAM): $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $(OBJECTS)
