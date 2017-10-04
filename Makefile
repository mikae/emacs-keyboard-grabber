EMACS_ROOT = `emacs --batch --eval "(append-to-file source-directory nil \"/dev/stdout\")"`
CC         = gcc
LD         = gcc
CFLAGS     = -ggdb3 -Wall
LDFLAGS    =

default:
	@echo $(EMACS_ROOT)

all: clean compile

compile:
	$(CC) $(CFLAGS) -lxcb -fPIC -o lib.o -c lib.c
	$(CC) $(CFLAGS) -I$(EMACS_ROOT)/src -fPIC -o module.o -c module.c
	$(LD) -shared $(LDFLAGS) -o module.so lib.o module.o
	rm -f module.o
	rm -f lib.o
	mv module.so keyboard-grabber.so

clean:
	@rm -f test
	@rm -f lib.o
	@rm -f lib.so
	@rm -f module.o
	@rm -f module.so
	@rm -f keyboard-grabber.so

test-compile: clean
	$(CC) $(CFLAGS) -lxcb -o test test.c
