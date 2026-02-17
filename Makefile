CC = gcc
COBC = cobc
CFLAGS = $(shell sdl2-config --cflags)
LDFLAGS = $(shell sdl2-config --libs)
CPYS = $(wildcard copy/*.cpy)

doom2: doom2.cob display.c $(CPYS)
	$(COBC) -x -I copy -o doom2 doom2.cob display.c $(CFLAGS) $(LDFLAGS)

clean:
	rm -f doom2 *.o

.PHONY: clean
