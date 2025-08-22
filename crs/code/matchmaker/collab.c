/******************************************************************
  collab.c  -- $Revision: 1.20 $ -- $Date: 2003/05/21 02:43:27 $

  M. Sullivan		
  - collaborator support functions

******************************************************************/

#define EXTERNAL extern
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <strings.h>
#include <sys/time.h>
#include <fcntl.h>
#include "defs.h"
#include "collab.h"

char mm_buffer[MAX_MSG_LENGTH];
int mm_socket;

/* --------------------------------------------------------------- */
struct collaborator*
collaborator_alloc() {
  struct collaborator *c;
  c = (struct collaborator *) malloc(sizeof(struct collaborator));
  c->socket = NO_SOCKET;
  c->port = UNKNOWN_PORT;
  return(c);
}

/* --------------------------------------------------------------- */
/* computes expected MM port from CIRCA_BASEPORT env variable */

int matchmaker_port ()
{
 char *portstring;

  portstring = getenv("CIRCA_BASEPORT");
  if (portstring == NULL) {
    fprintf(stderr,"MM: CIRCA_BASEPORT isn't set!\n");
    fprintf(stderr,"MM: Exiting on error.\n");
    exit(1);
  }
  return(atoi(portstring) +2);
}

/* for a single CIRCA agent, the default AMP port is CIRCA_BASEPORT */
int default_amp_port ()
{
 char *portstring;

  portstring = getenv("CIRCA_BASEPORT");
  if (portstring == NULL) {
    fprintf(stderr,"Error: CIRCA_BASEPORT isn't set!\n");
    fprintf(stderr,"Exiting on error.\n");
    exit(1);
  }
  return(atoi(portstring));
}

int rts_my_port ()
{
 char *portstring;

  if (myindex<0)
    {
	fatal_error("No index set by config file or cmd line argument!");
    }
  portstring = getenv("CIRCA_BASEPORT");
  return(atoi(portstring) + port_offset + myindex*10);
}

/* --------------------------------------------------------------- */
/* computes expected flightsim port from CIRCA_BASEPORT env variable */

int flightsim_port ()
{
 char *portstring;

  portstring = getenv("CIRCA_BASEPORT");
  return(atoi(portstring) +7);
}

/* --------------------------------------------------------------- */

void free_collaborator(struct collaborator *c) {
  if (c) {
    free(c);
  }
}

/* --------------------------------------------------------------- */
int register_collaborator(char *param_string) {
  struct collaborator *c;
  c = collaborator_alloc();
  if (3 == sscanf(param_string, "%s %s %d", 
		  c->name, c->hostname, &c->port)) {
    if (lookup_collaborator_by_name(c->name) != NULL) {
      printf("RTS: register_collaborator: requested name exists\n");
      free_collaborator(c);
      return(1);
    }
    
    c->socket = NO_SOCKET;
    c->next = clist;
    clist = c;
    return(0);
  } else {
    printf("RTS: register_collaborator: couldn't parse parameters: %s\n", param_string);
    return(1);
  }
}

/* --------------------------------------------------------------- */
int remove_all_collaborators ()
{
  struct collaborator *c,*last;

        for (c = clist; c != NULL;)
          {
                last=c;
		c=c->next;
		/* note we cannot free this here, b/c we depend on the
			acm_collab still being around to retrieve his data */
                /* free_collaborator(last); */
          }
        clist = NULL;
  return(1);
}

/* --------------------------------------------------------------- */

int add_collaborator(char *param_string) {
  struct collaborator *c;
  c = collaborator_alloc();
  if (1 == sscanf(param_string, "%s", c->name)) {
    if (lookup_collaborator_by_name(c->name) != NULL) {
      printf("RTS: add_collaborator: requested name exists\n");
      free_collaborator(c);
      return(1);
    }
    
    c->socket = NO_SOCKET;
    c->next = clist;
    clist = c;
    return(0);
  } else {
    printf("RTS: add_collaborator: couldn't parse parameters: %s\n", param_string);
    return(1);
  }
}
  
/* --------------------------------------------------------------- */
void print_collaborator_list() {
  struct collaborator *c;

  c = clist;
  while (c != NULL) {
     print_collaborator(c);
     c = c->next;
  }
}

void print_collaborator(struct collaborator *c) {
    printf("RTS: %s %s %d", c->name, c->hostname, c->port);
    if (c->socket != NO_SOCKET) { 
      printf(" <connected> ");
    } else {
      printf(" <not connected> ");
    }
    printf("\n");   
}

/* --------------------------------------------------------------- */
struct collaborator *lookup_collaborator_by_name(char *name) {
  struct collaborator *c;
  
  debugss("Looking up collaborator ",name);
  c = clist;
  while (c != NULL) {
    if (!strcmp(c->name, name)) {
  	debugs("	Found him!");
      break;
    }
    c = c->next;
  }
  return(c);
}

/* --------------------------------------------------------------- */

void remove_collaborator_by_name(char *name) {
  struct collaborator *c;
  
  c = clist;	/* clist is global collaborator obj list */

	/* if the first in list is the guy to kill */
  if (!strcmp(c->name,name))
    {
	clist = c->next;
  	if (!quiet) print_collaborator_list();
	return;
    }
  
	/* else loop over list; when next is guy to kill, bypass in list */
  while (c->next != NULL) 
   {
    if (!strcmp(c->next->name, name)) 
    {
	free(c->next);
	c->next = c->next->next;
  	if (!quiet) print_collaborator_list();
	return;
    }
    c = c->next;
   }

  printf("MM WARNING: couldnt remove collaborator %s\n",name);
  return;
}

/* --------------------------------------------------------------- */
int all_collaborators_connected_p() {
  int retval = 1;
  struct collaborator *c;
  
  c = clist;
  
  while (c != NULL) {
    if (c->socket == NO_SOCKET) {
      retval = 0;
      break;
    }
    c = c->next;
  }
  return retval;
}
    
/* --------------------------------------------------------------- */

int initialize_collaborations(char *mm_hostname, int mm_port, 
			  char *my_name, char *my_hostname, int my_port) {
  if (talk_to_matchmaker(mm_hostname, mm_port, 
			 my_name, my_hostname, my_port)) {
    printf("RTS: Failure conversing with matchmaker\n");
    return(1);
  }

  /* NOTE megahack here: since we put bogus ACM collaborator at front of clist,
	we now need to remove it before try to talk to ACM like an RTS... */

#ifdef ACM
  remove_collaborator_by_name(acmname);
#endif

  if (!quiet) print_collaborator_list();

  printf("RTS: Connecting to waiting collaborators\n");
  if (connect_to_waiting_collaborators(my_name, my_hostname, my_port)) {
    printf("RTS: Failure connecting to collaborators\n");
    return(1);
  }

  if (!quiet) print_collaborator_list();

  if (wait_for_collaborators(my_name, my_port)) {
    printf("RTS: Failure waiting for collaborators\n");
    return(1);
  }

  if (!quiet) print_collaborator_list();

  say_forgetme_to_matchmaker(mm_hostname, mm_port, my_name);

  debugs("Successfully initialized collaborations");
  return(0);
}

/* --------------------------------------------------------------- */

void read_string_from_socket(int sock, char *buf) {
  int read_len = 0;

  while (read_len == 0) {
    socket_read(sock, buf, MAX_MSG_LENGTH, &read_len);
  }
  buf[read_len] = '\0';
}


/* --------------------------------------------------------------- */
int talk_to_matchmaker(char *mm_hostname, int mm_port, 
		   char *my_name, char *my_hostname, int my_port) {
  int write_len;
  char in_name[MAX_MSG_LENGTH];
  char in_hostname[MAX_MSG_LENGTH];
  int in_port;
  struct collaborator *c;

  /* connect to matchmaker */
  printf("RTS: Connecting to matchmaker at %s, port %d\n",
	 mm_hostname, mm_port);
  mm_socket = socket_connect(mm_hostname, mm_port);
  if (mm_socket < 0) 
   {
    printf("RTS: Error in matchmaker socket connection.\n");
    return(1);	/* failure */
   }
  debugs("Connected to matchmaker; sending registration");
  sprintf(mm_buffer, "REGISTER %s %s %d", my_name, my_hostname, my_port);
  socket_write(mm_socket, mm_buffer, strlen(mm_buffer), &write_len);

  /* read response */
  read_string_from_socket(mm_socket, mm_buffer);
  ndebugss("Received from MM: ", mm_buffer);

  /* make requests, collecting responses */
  c = clist;
  while (c != NULL) 
   {
    ndebugss("Requesting from MM", c->name);
    sprintf(mm_buffer, "REQUEST %s", c->name);
    socket_write(mm_socket, mm_buffer, strlen(mm_buffer), &write_len);
    read_string_from_socket(mm_socket, mm_buffer);
    ndebugss("Received from MM", mm_buffer);

    /* if NOTFOUND, make next request */
    if (!strcmp(mm_buffer, "NOTFOUND\n")) {
      /* have to wait for this one later */

      /* if exists, store data for later cnxn */
    } else if (3 == sscanf(mm_buffer, "COLLABORATOR %s %s %d",
			   in_name, in_hostname, &in_port)) {
      ndebugss("	parsed collab information OK", in_name);
      strcpy(c->name, in_name);
      strcpy(c->hostname, in_hostname);
      c->port = in_port;

      /* didn't parse */
    } else {
      ndebugss("Can't parse response", mm_buffer);
    }
    c = c->next;
   }
	/* signoff for now... */
  sprintf(mm_buffer, "BYE %s",my_name);
  socket_write(mm_socket, mm_buffer, strlen(mm_buffer), &write_len);
  socket_close(mm_socket);
  mm_socket=0;
  return(0);	/* success */
}

/* --------------------------------------------------------------- */
/* after you have done all business of connecting to the other
	agents, you can tell matchmaker to forget about your record
	so that future connections will have a chance of working... */

int say_forgetme_to_matchmaker(char *mm_hostname, int mm_port, char *my_name)
{
  int write_len;

  /* connect to matchmaker */
  ndebugss("Attempting connect to MM for forgetme", mm_hostname);
  mm_socket = socket_connect(mm_hostname, mm_port);
  if (mm_socket < 0) 
   {
    printf("RTS: Error in matchmaker socket connection.\n");
    return(1);	/* failure */
   }
  sprintf(mm_buffer, "FORGETME %s", my_name);
  socket_write(mm_socket, mm_buffer, strlen(mm_buffer), &write_len);
  socket_close(mm_socket);
  mm_socket=0;
  ndebugs("Sent forgetme and closed socket");
  return(0);
}

/* --------------------------------------------------------------- */

int connect_to_waiting_collaborators(char *my_name, char *my_hostname,
				 int my_port) {
  struct collaborator *c;
  char msg_buffer[MAX_MSG_LENGTH];
  int write_len;
  int flags;

  c = clist;
  while (c != NULL) {
    if (c->port != UNKNOWN_PORT && c->socket == NO_SOCKET) {
      c->socket = socket_connect(c->hostname, c->port);
      if (c->socket < 0) {
	printf("RTS: Error connecting to %s\n", c->name);
      }
      sprintf(msg_buffer, "REGISTER %s %s %d", my_name, my_hostname, my_port);
      socket_write(c->socket, msg_buffer, strlen(msg_buffer), &write_len);
      read_string_from_socket(c->socket, msg_buffer);
      debugss("RCVD from collaborator", msg_buffer);
	
      /* make socket non-blocking so we don't wait during rts comm. */
      flags = fcntl(c->socket, F_GETFL, 0);
      fcntl(c->socket, F_SETFL, flags | O_NONBLOCK);

      if (!strcmp(msg_buffer, "ACK")) {
	debugss("successfully connected to", c->name);
      } else {
	printf("RTS: failed connection to %s\n",c->name);
	socket_close(c->socket);
	c->socket = NO_SOCKET;
      }
    }
    c = c->next;
  }
  return(0);
}

/* --------------------------------------------------------------- */

int wait_for_collaborators(char *my_name, int port) {
  int in_socket;
  int connect_socket;
  char in_buffer[MAX_MSG_LENGTH];
  char in_name[MAX_MSG_LENGTH];
  char in_hostname[MAX_MSG_LENGTH];
  int in_port;
  struct collaborator *c;
  int write_len;
  int flags;

  if (all_collaborators_connected_p()) { return(0);}

  ndebugs("Opening my listen socket for collaborators");

  if ((in_socket = socket_init(port, MAX_REQUESTS)) < 0) {
    perror("RTS: couldn't open listen socket");
    return(1);
  }

  ndebugs("Waiting for collaborators");

  while (!all_collaborators_connected_p()) {
    connect_socket = socket_accept(in_socket);

    read_string_from_socket(connect_socket, in_buffer);
    
    if ((3 == sscanf(in_buffer, "REGISTER %s %s %d",
		     in_name, in_hostname, &in_port))) {
      c = lookup_collaborator_by_name(in_name);
      if (c != NULL) {
	strcpy(c->hostname, in_hostname);
	c->port = in_port;
	
	/* make socket non-blocking so we don't wait during rts comm. */
	flags = fcntl(connect_socket, F_GETFL, 0);
	fcntl(connect_socket, F_SETFL, flags | O_NONBLOCK);

	c->socket = connect_socket;
	socket_write(connect_socket, "ACK", strlen("ACK"), &write_len);
      } else {
	printf("RTS: Collaborator not found: %s\n", in_name);
	socket_close(connect_socket);
      }
    } else {
      printf("RTS: Couldn't parse registration: %s", in_buffer);
    }
  }
  socket_close(in_socket);
  return(0);
}

/* note we cannot close in_socket from terminate routine if this gets
interrupted... 

also: make sur we go over the clist and close their sockets when die.

*/

/* --------------------------------------------------------------- */

int deinitialize_collaborations() 
{
  
 return(1);
}
