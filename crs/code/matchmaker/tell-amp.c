/******************************************************************
$Id: $
-	C code to open connection to the ACCEPTOR in fake-bridge in AMP and send command line arg string
- usage: tell-amp [-h host] [-p port] "string"
******************************************************************/

#define EXTERNAL

#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <string.h>
#include <strings.h>
#include <stdio.h>
#include "sockets.h"
#include "defs.h"

#include "collab.h"

//#define MAX_HOSTNAME_LENGTH 100

#define DEFAULT_PORT (atoi(getenv("CIRCA_BASEPORT"))+9)
int newsock, sock, port;

/******************************************************************/
int main (argc,argv)
  int argc;
  char **argv;
{
  char hostname[MAX_HOSTNAME_LENGTH];
  char agentname[MAX_HOSTNAME_LENGTH];
  char msg[MAXLEN];
  int forced, named, write_len, read_len=0;
  int r;
  char *mm_hostname;
  char force_hostname[MAX_HOSTNAME_LENGTH];
  char mm_buffer[255];
  int mm_socket;

  gethostname(hostname,MAX_HOSTNAME_LENGTH);
  port = DEFAULT_PORT;

  while (*++argv)
    {
	if (!strcmp(*argv,"-h")) strcpy(hostname,*++argv);
	else if (!strcmp(*argv,"-p")) sscanf(*++argv,"%d",&port);
	else if (!strcmp(*argv,"-k")) sprintf(msg,"((:type :halt))\n");
	else if (!strcmp(*argv,"-r")) sprintf(msg,"((:type :reset))\n");
  else if (!strcmp(*argv,"-f")) { strcpy(force_hostname,*++argv); forced=1;}
	else if (!strcmp(*argv,"-n")) { strcpy(agentname,*++argv); add_collaborator(agentname); named=1;}
	else sprintf(msg,"%s\n",*argv);
    }

	// if given agent name, connect to MM and get his host/port
  if (named) {
    mm_hostname = getenv("CIRCA_MM_HOST");
    /* Default to localhost if CIRCA_MM_HOST not set. */
    if(NULL == mm_hostname) {
      mm_hostname = (char *)malloc(sizeof(char)*MAX_HOSTNAME_LENGTH);
      strcpy(mm_hostname, "localhost");
    }
    mm_port = matchmaker_port();
    printf("attempting connect to matchmaker at %s %d\n", mm_hostname, mm_port);

	if (talk_to_matchmaker(mm_hostname, mm_port, "TELL-AMP",hostname, 0)) // the zero is b/c we dont have a listen port
	  {
    		printf("Error: failure during talk_to_matchmaker\n");
      		exit(1);
	  }
	// there is only one collab, at the head of the default clist var
  strcpy(hostname,clist->hostname);
  port=clist->port;
  ndebugss("Found named agent at host",hostname);
  ndebugsd("Found named agent at port",port);
  }

  if (forced) {
    printf("Attempting to use forced hostname: %s\n",force_hostname);
    newsock = socket_connect(force_hostname,port);
  }
  else {
    printf("Attempting to use hostname: %s\n",hostname);
    newsock = socket_connect(hostname,port);
  }
  ndebugss("Sending msg to AMP", msg);
  socket_write(newsock, msg, strlen(msg), &write_len);
  if (write_len ==0)
	{
 	  fprintf(stderr,"ERROR: tell-amp wrote zero bytes to AMP\n");
	}
  sleep(5);	 // wait for the acceptor to receive before dying and nuking the socket
  socket_close(newsock);
}
