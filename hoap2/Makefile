CC=gcc
CFLAGS=-g -O2 -Wall -D_GNU_SOURCE -std=c99 -I../src
LIBS=../libsimulation.a -lm

all: hoap2

SOURCES= \
	hoap2.c

OBJECTS=$(SOURCES:.c=.o) 

hoap2: $(OBJECTS) ../libsimulation.a
	$(CC) -o hoap2 $(OBJECTS) $(LIBS)

clean:
	rm -f $(OBJECTS) hoap2

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

