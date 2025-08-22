/******************************************************************
  matchmaker.c 		M. Sullivan		$Revision: 1.13 $

  - server which sets up rts connections
  - rqmts: 
    - maintain list of collaborators
    - accept connections
    - match requests

******************************************************************/
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <string.h>
#include <strings.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

//#include <stdio.h>
//#include <unistd.h>
//#include <sys/types.h>
//#include <stdlib.h>
//#include <string.h>
//#include <strings.h>
//#include <sys/time.h>
//#include <signal.h>

#define EXTERNAL 

#include "defs.h"

#include "sockets.h"
#include "collab.h"

struct collaborator *client_list = NULL;

/* internal declarations */
int handle_incoming_connection(int listen_sock);
void listen_on_socket(int socket); 
void terminate_mm();


/* external declarations */
extern struct collaborator* lookup_collaborator_by_name(char *name);
void remove_collaborator_by_name(char *name);

int listen_socket;  

/* --------------------------------------------------------------- */
void sig_handler(sig,code,scp)
  int sig,code;
  struct sigcontext *scp;
{
  fprintf(stderr,"MM: Caught signal %d\n",sig);
  terminate_mm();
}

/* --------------------------------------------------------------- */

int
main(int argc, char **argv) {
  int port;

  signal(SIGINT,&sig_handler);
  signal(SIGQUIT,&sig_handler);
  signal(SIGHUP,&sig_handler);
  signal(SIGTERM,&sig_handler);

  /* default: determine port from CIRCA_BASEPORT */
  port = matchmaker_port();

  while(*++argv) {
    if (!strcmp(*argv, "-p")) {
      sscanf(*++argv, "%d", &port);
    }
    else if (!strcmp(*argv, "-d")) {
	fprintf(stderr,"Debug mode on\n");
	debug=1;
    }
    else if (!strcmp(*argv, "-q")) {
	fprintf(stderr,"Quiet mode on\n");
	quiet=1;
    }
    else
     {
	fprintf(stderr,"Unrecognized arg: %s",*argv);
     }
  }

  if ((listen_socket = socket_init(port, MAX_REQUESTS)) < 0) {
    fprintf(stderr,"MM: couldn't open listen socket\n");
    exit(1);
  }

  if (!quiet) fprintf(stderr,"MM: successfully opened listen socket: %d\n", listen_socket);

  while (1) {
    handle_incoming_connection(listen_socket);
  }
  
  /* NEVER GET HERE... return 0; */
}

int
handle_incoming_connection(int listen_sock) {
  int sock;
  int read_len;
  int return_val=0;
  int write_len;
  char inbuf[MAX_MSG_LENGTH];
  char outbuf[MAX_MSG_LENGTH];
  char name[MAX_MSG_LENGTH];
  char hostname[MAX_HOSTNAME_LENGTH];
  int port;
  struct collaborator *c;
  char *ack_msg = "ACK\n";

  char reg_params[MAX_MSG_LENGTH];
  char request[MAX_MSG_LENGTH];

  sock = socket_accept(listen_sock);
  
  if (!quiet) fprintf(stderr,"MM: new incoming connection on socket %d\n",sock);
  read_len=0;
  while (read_len ==0 && return_val>=0) {
    return_val = socket_read(sock, inbuf, MAX_MSG_LENGTH, &read_len);
  }
  if (return_val<0)
    {
	socket_close(sock);
	return(-1);
    }
  inbuf[read_len]='\0';

  /* parse input */
  if (!quiet) fprintf(stderr,"MM: RCVD [%s]\n",inbuf);
  if (!(1 == sscanf(inbuf, "REGISTER %[^\n]", reg_params))) {
    if (!strcmp(inbuf, "DIE")) {
      fprintf(stderr,"MM: harikari\n");
      socket_close(sock);
      terminate_mm();
      return(0);
    } 
  else if (1 == sscanf(inbuf, "FORGETME %[^\n]",name)) {
	if (!quiet) fprintf(stderr,"MM: forgetme from %s\n",name);
	remove_collaborator_by_name(name);
	socket_close(sock);
	return(0);
    }
  else if (!strcmp(inbuf, "RESET")) {
	if (!quiet) fprintf(stderr,"MM: got reset \n");
	socket_close(sock);
	remove_all_collaborators();
	return(0);
    }
  else if (!strcmp(inbuf, "HEALTH")) {
	if (!quiet) fprintf(stderr,"MM: got healthcheck \n");
	socket_close(sock);
	return(0);
    }
  else  if (1 == sscanf(inbuf, "REQUEST %s", request)) {
    	if (!quiet) fprintf(stderr,"MM: finding by name %s\n", request);
      /* find name, return response */
      c = lookup_collaborator_by_name(request);
      if (c == NULL) {
	socket_write(sock, "NOTFOUND\n", strlen("NOTFOUND\n"), &write_len);
    	fprintf(stderr,"MM: NOTFOUND: %s\n",request);
      } else {
	sprintf(outbuf, "COLLABORATOR %s %s %d\n", c->name, c->hostname, c->port);
	socket_write(sock, outbuf, strlen(outbuf), &write_len);
    	if (!quiet) fprintf(stderr,"MM:	found %s %s %d\n", c->name, c->hostname, c->port);
      }}
  else {
      fprintf(stderr,"MM: received unparsable message.\n");
      goto FAIL;
    }
  }
  
  /* handle registration */
  if (!(3 == sscanf(reg_params, "%s %s %d", name, hostname, &port))) {
    goto FAIL;
  }

  if (!quiet) fprintf(stderr,"MM: Checking for existing registration\n");
  c = lookup_collaborator_by_name(name);
  if (c == NULL) {
    if (!quiet) fprintf(stderr,"MM: Adding collaborator %s %s %d\n",name,hostname,port);
    register_collaborator(reg_params);
  } else if (strcmp(c->hostname, hostname) ||
	     c->port != port) {
    fprintf(stderr,"MM: Collaborator with this name, but different address has already registered.\n");
    goto FAIL;
  } else
	{

    	if (!quiet) fprintf(stderr,"MM: A pre-registered collaborator is re-joining\n");
	}
	

  /* ack */
  socket_write(sock, ack_msg, strlen(ack_msg), &write_len);

  /* read loop */
  while(1) {
    if (!quiet) fprintf(stderr,"MM: Waiting for input from %s\n",name);
    read_len=0;
    while (read_len ==0) {
      int read_ret = socket_read(sock, inbuf, MAX_MSG_LENGTH, &read_len);
      if (read_ret < 0) {
        fprintf(stderr,"MM: unexpected socket close on read\n");
        goto FAIL;
      }
    }
    inbuf[read_len]='\0';
    
    /* parse request */
    if (!quiet) fprintf(stderr,"MM: RCVD [%s]\n", inbuf);
    if (1 == sscanf(inbuf, "REQUEST %s", request)) {
    	if (!quiet) fprintf(stderr,"MM: finding by name %s\n", request);
      /* find name, return response */
      c = lookup_collaborator_by_name(request);
      if (c == NULL) {
	socket_write(sock, "NOTFOUND\n", strlen("NOTFOUND\n"), &write_len);
    	fprintf(stderr,"MM: NOTFOUND: %s\n",request);
      } else {
	sprintf(outbuf, "COLLABORATOR %s %s %d\n", c->name, c->hostname, c->port);
	socket_write(sock, outbuf, strlen(outbuf), &write_len);
    	if (!quiet) fprintf(stderr,"MM:	found %s %s %d\n", c->name, c->hostname, c->port);
      }

    } else if (1 == sscanf(inbuf, "BYE %[^\n]", request)) {
      if (!quiet) fprintf(stderr,"MM: got goodbye from %s, closing socket\n", name);
      socket_close(sock);
      return(0);

    } else {
      fprintf(stderr,"MM: Unable to parse message: %s\n", inbuf);
      goto FAIL;
    }
  }

 FAIL:
  fprintf(stderr,"MM: failure during conversation\n");
  if (sock > 0) {
    socket_close(sock);
  }
  return(1);
}


void terminate_mm() {
  fprintf(stderr, "MM: Shutting down.\n");
  if (listen_socket > 0) {
    socket_close(listen_socket);
  }
  exit(1);
}

