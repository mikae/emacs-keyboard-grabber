EMACS_ROOT = `emacs --batch --eval "(append-to-file source-directory nil \"/dev/stdout\")"`
CC         = gcc
LD         = gcc
CFLAGS     = -ggdb3 -Wall
LDFLAGS    =

default:
	@echo $(EMACS_ROOT)

all: clean compile

compile:
	$(CC) $(CFLAGS) -I$(EMACS_ROOT)/src -fPIC -o module.o -c module.c
	$(LD) -shared $(LDFLAGS) -o module.so module.o
	rm -f module.o
	mv module.so keyboard-grabber.so

clean:
	@rm -f test
	@rm -f lib.o
	@rm -f lib.so
	@rm -f module.o
	@rm -f module.so
	@rm -f keyboard-grabber.so

test-compile:
	$(CC) $(CFLAGS) -Llibevdev-1.0/libevdev -levdev -o test test.c
