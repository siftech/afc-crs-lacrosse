.SUFFIXES: .exe

CSRCS =  $(wildcard *.c)

.c.exe:
	gcc $*.c -o $@

all:  ${CSRCS:.c=.exe} 

clean:
	rm -f a.out *.exe
