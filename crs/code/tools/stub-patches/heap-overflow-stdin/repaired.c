// Created by ChatGPT4o 5/28/2024 with prompt "here's some more code.  Are there any serious memory security flaws? please fix the entire program, but do not explain"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

// Simple Heap Overflow by b1ack0wl for E1550

#define HEAP_SIZE 100

void dat_shell(){
    printf("Congrats! I will now execute /bin/sh - b1ack0wl\r\n");
    system("/bin/sh");
    exit(0);
}

int main(int argc, char **argv[]){
    char *heap_of;
    char *heap_of2;
    char line[2 * HEAP_SIZE];

    printf("\r\nWelcome to the first Heap Overflow exercise!\r\n\r\n");
    heap_of = malloc(HEAP_SIZE);
    if (!heap_of) {
        perror("Failed to allocate memory");
        exit(1);
    }
    printf("Heap Address 1 of %d bytes: %p\r\n", HEAP_SIZE, heap_of);

    heap_of2 = malloc(HEAP_SIZE);
    if (!heap_of2) {
        perror("Failed to allocate memory");
        free(heap_of);
        exit(1);
    }
    printf("Heap Address 2 of %d bytes: %p\r\n", HEAP_SIZE, heap_of2);

    printf("Copying contents of input to Heap Address 1\r\n\r\n");
    if (scanf("%99s", line) != 1) {
        perror("Failed to read input");
        free(heap_of);
        free(heap_of2);
        exit(1);
    }

    strncpy(heap_of, line, HEAP_SIZE - 1); // Use strncpy() with appropriate size limit
    heap_of[HEAP_SIZE - 1] = '\0'; // Ensure null termination

    free(heap_of2);

    printf("You entered %s \r\n", heap_of);
    printf("Try Again\r\n");

    free(heap_of); // Free the allocated memory

    return 0;
}

