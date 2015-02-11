
# common variables
SHELL=/bin/sh
CC=gcc
CPP=gcc -E
CFLAGS=-W -Wall -std=gnu11 -pthread -g -DDEBUG -O0
CPPFLAGS=-iquote src/
LIBS=-L/usr/local/lib -lm -lev -lsndfile

# dependency extraction for C files
.%.d: %.c
	@ $(SHELL) -ec '$(CPP) $(CPPFLAGS) -MM $< | sed "s|$.*.o|& $@|g" > $@'

obj/%.o: src/%.c
	$(CC) -o $@ -c $< $(CFLAGS) $(CPPFLAGS)

