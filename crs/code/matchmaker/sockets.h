/******************************************************************************
* 									      *
* File:         sockets.h						      *
* Author:       Michael Hucka						      *
* Organization: University of Michigan					      *
* Created:      Wed Jul 26 22:18:03 1989				      *
* Contents:     Include file for socket routines.                             *
* 									      *
******************************************************************************/


#ifndef SOCKETS_H
#define SOCKETS_H

#define  socket_module_code                      900
#define  socket_internal_error                ( socket_module_code +  1 )
#define  socket_illegal_argument              ( socket_module_code +  2 )
#define  socket_unknown_host                  ( socket_module_code +  3 )
#define  socket_connection_refused            ( socket_module_code +  4 )
#define  socket_address_in_use                ( socket_module_code +  5 )
#define  socket_illegal_handle                ( socket_module_code +  6 )
#define  socket_would_block                   ( socket_module_code +  7 )
#define  socket_no_connection                 ( socket_module_code +  8 )
#define  socket_nothing_to_read               ( socket_module_code +  9 )
#define  socket_bad_port                      ( socket_module_code +  10 )

/* prototype declarations, barf */
int socket_init(int port, int max_req);
int socket_accept(int sock);
int socket_connect(char *hostname, int port);
int socket_read(int sock, char *buf, int buf_len, int *read_len);
int socket_write(int sock, char *buf, int buf_len, int *write_len);
int socket_nonblocking(int sock);
int socket_blocking(int sock);
int socket_close(int sock);

#endif /* SOCKETS_H */
