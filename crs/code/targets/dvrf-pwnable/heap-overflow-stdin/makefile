.SUFFIXES: .exe

CSRCS =  $(wildcard *.c)

.c.exe:
	gcc -ggdb -fno-stack-protector $*.c -o $@

all:  ${CSRCS:.c=.exe} 
