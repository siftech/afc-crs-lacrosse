/******************************************************************
  collab.h 		M. Sullivan		$Revision: 1.8 $

  - collaborator data structures

******************************************************************/

#ifndef __COLLAB_H
#define __COLLAB_H

#define UNKNOWN_PORT -1
#define NO_SOCKET -1

#ifndef MAXLEN
#define MAXLEN 5000
#endif

/* As of Linux kernel 2.2, the backlog in listen attempts means the
   number of _completed_ tcp connections that have not yet
   been accepted. This number is silently truncated to the
   number located in the file /proc/sys/net/core/somaxconn
   (which is 128 on a 2014-era linux machine). The number of
   _incomplete_ tcp connections backlogged is located in this file:
   /proc/sys/net/ipv4/tcp_max_syn_backlog (which was 1024 on a 2014-era linux
   machine).

   So, we've upped our server machine to a limit of 1024 for the listen()
   backlog (somaxconn) and set our codes to that.
*/
#define MAX_REQUESTS 1024


#define MAX_MSG_LENGTH 255

struct collaborator {
  char name[255];
  char hostname[MAX_HOSTNAME_LENGTH];
  int port;
  int socket;
  int push_cache_port;
  int push_cache_socket;
  struct collaborator* next;
  char buffer[MAXLEN];
};
	
EXTERNAL struct collaborator *clist;

/* Declarations */
int matchmaker_port();
void print_collaborator_list();
void print_collaborator(struct collaborator *c);
void remove_collaborator_by_name(char *name);
int remove_all_collaborators();
int rts_my_port();
struct collaborator* lookup_collaborator_by_name(char *name);
void read_string_from_socket(int sock, char *buf);
int add_collaborator(char *param_string);
int initialize_collaborations(char *mm_hostname, int mm_port, 
			  char *my_name, char *my_hostname, int my_port);
int register_collaborator(char *param_string);

#endif /* __COLLAB_H */
