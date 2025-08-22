/******************************************************************
  mmhealth.c 		$Revision: 1.4 $

 - call up matchmaker and send RESET cmd

******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <strings.h>
#include <sys/time.h>
#include <signal.h>

#define EXTERNAL 
/* some defs we're no longer borrowing from defs.h */
#define MAX_HOSTNAME_LENGTH 100

#include "sockets.h"
#include "matchmaker.h"
#include "defs.h"
#include "collab.h"

int
main(int argc, char **argv) {
  int mm_port;
  int mm_socket;
  char *mm_hostname;
  char mm_buffer[255];
  int write_len;

  mm_port = matchmaker_port();
  mm_hostname = getenv("CIRCA_MM_HOST");
  /* Default to localhost if CIRCA_MM_HOST not set. */
  if(NULL == mm_hostname) {
    mm_hostname = (char *)malloc(sizeof(char)*MAX_HOSTNAME_LENGTH);
    strcpy(mm_hostname, "localhost");
  }
  printf("attempting connect to matchmaker at %s %d\n", mm_hostname, mm_port);
  mm_socket = socket_connect(mm_hostname, mm_port);
    
  if (mm_socket < 0) {
    printf("Error in matchmaker socket connection.\n");
    exit(1);
  }

  quiet=0;
  printf("Sending HEALTH message to matchmaker.\n");
  sprintf(mm_buffer, "HEALTH");
  socket_write(mm_socket, mm_buffer, strlen(mm_buffer), &write_len);
  socket_close(mm_socket);

  exit(0);
}
