#include <string.h>
#include <stdio.h>
#include <stdlib.h>

//Simple Heap Overflow by b1ack0wl for E1550

#define HEAP_SIZE 100

int main(int argc, char **argv[]){
char *heap_of;
char *heap_of2;
char line[2*HEAP_SIZE];

/*
if (argc < 2){
printf("Usage: heap_overflow_01 argument\n-By b1ack0wl\n");
exit(1);
} 
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
}

void dat_shell(){

printf("Congrats! I will now execute /bin/sh - b1ack0wl\n");
system("/bin/sh -c");
exit(0);

}
