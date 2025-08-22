import os
import pytest
from string import whitespace
from lacrosse_llm.utils import double_braces, strip_doubles

# __file__ is the path of the current file, in this case the path of test_logic.py
NOTEBOOKS_DIR = os.path.join(os.path.dirname(__file__), '../notebooks')


def compare(s1, s2):
    remove = whitespace
    mapping = {ord(c): None for c in remove}
    if s1.translate(mapping) == s2.translate(mapping):
        return True
    i: int = 0
    prefix1 = ""
    prefix2 = ""
    for x,y in zip(s1.translate(mapping), s2.translate(mapping)):
        prefix1 += x
        prefix2 += y
        if x != y:
            print(f"Mismatch at position {i}: {x} != {y}")
            print(f"String 1: {prefix1}")
            print(f"String 2: {prefix2}")

            return False
        i += 1


orig_file = os.path.join(NOTEBOOKS_DIR, 'heap_overflow_01_stdin.c')
with open(orig_file, 'r') as file:
    raw_source = file.read()

doubled = r"""
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

//Simple Heap Overflow by b1ack0wl for E1550

#define HEAP_SIZE 100

int main(int argc, char **argv[]){{
char *heap_of;
char *heap_of2;
char line[2*HEAP_SIZE];

/*
if (argc < 2){{
printf("Usage: heap_overflow_01 argument\n-By b1ack0wl\n");
exit(1);
}} 
*/

printf("\nWelcome to the first Heap Overflow exercise!\n\n");
heap_of = malloc(HEAP_SIZE); 
printf("Heap Address 1 of %d bytes: %p\n", HEAP_SIZE, heap_of);
heap_of2 = malloc(HEAP_SIZE);
printf("Heap Address 2 of %d bytes: %p\n", HEAP_SIZE, heap_of2);
printf("Copying contents of argv[1] to Heap Address 1\n\n");
scanf("%s", line);
strcpy(heap_of, line);
//free(heap_of);
free(heap_of2);

printf("You entered %s \n", heap_of);
printf("Try Again\n");

return 0;
}}

void dat_shell(){{

printf("Congrats! I will now execute /bin/sh - b1ack0wl\n");
system("/bin/sh -c");
exit(0);

}}

"""

def test_doubling():
    assert compare(double_braces(raw_source),doubled)

def test_stripping():
    assert compare(raw_source, strip_doubles(doubled))

def test_rt():
    assert compare(raw_source, strip_doubles(double_braces(raw_source)))