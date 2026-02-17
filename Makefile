CC = gcc
COBC = cobc
CFLAGS = $(shell sdl2-config --cflags)
LDFLAGS = $(shell sdl2-config --libs)

doom2: doom2.cob display.c
	$(COBC) -x -o doom2 doom2.cob display.c $(CFLAGS) $(LDFLAGS)

clean:
	rm -f doom2 *.o

.PHONY: clean
