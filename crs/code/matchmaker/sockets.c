
#define _GNU_SOURCE

#include <sys/types.h>
#include <errno.h> 
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <string.h>
#include <netdb.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <stdio.h>

#ifndef __CYGWIN__
#ifndef CYGWIN
#ifndef LINUX
#include <sys/filio.h>
#endif
#endif
#endif

#define fatal_error(X) {perror(X); fflush(stderr); return(-1);}

/* Function prototypes */
int get_peer_name(int sock, char *buf, size_t buf_size);
int get_host_ip(char *buf, size_t buf_size);

/************************************************************/
int socket_init(int port, int max_req) {
    int sock;
    struct sockaddr_in server;

    sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock < 0) {
        perror("Socket creation failed");
        return -1;
    }

    // Optional: Use standard close if socket_close is undefined
    // close(sock); or define a wrapper function socket_close()

    memset(&server, 0, sizeof(server));
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = INADDR_ANY;
    server.sin_port = htons(port);

    if (bind(sock, (struct sockaddr *)&server, sizeof(server)) < 0) {
        perror("Bind failed");
        close(sock);  // or socket_close(sock);
        return -1;
    } else if (listen(sock, max_req) < 0) {
        perror("Listen failed");
        close(sock);  // or socket_close(sock);
        return -1;
    }

    return sock;
}

/************************************************************/
int socket_accept(int sock)
{
  int new_sock;

  errno = 0;
  if ((new_sock = accept(sock, 0, 0)) < 0) { 
    if ((errno == EBADF) || (errno == ENOTSOCK) || (errno == EFAULT))
      fatal_error("socket_accept: illegal arg")
	else
	  fatal_error("socket_accept: socket would block")
	}
  return new_sock;
}

/************************************************************/
int socket_connect(const char *hostname, int port) {
    struct addrinfo hints, *res, *rp;
    int sock;
    char port_str[6];

    snprintf(port_str, sizeof(port_str), "%d", port);

    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_INET;
    hints.ai_socktype = SOCK_STREAM;

    int status = getaddrinfo(hostname, port_str, &hints, &res);
    if (status != 0) {
        fprintf(stderr, "socket_connect: getaddrinfo: %s\n", gai_strerror(status));
        exit(1);
    }

    for (rp = res; rp != NULL; rp = rp->ai_next) {
        sock = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (sock == -1)
            continue;

        struct linger ling = {1, 30};
        if (setsockopt(sock, SOL_SOCKET, SO_LINGER, &ling, sizeof(ling)) < 0) {
            close(sock);
            continue;
        }

        if (connect(sock, rp->ai_addr, rp->ai_addrlen) == 0)
            break;

        close(sock);
    }

    freeaddrinfo(res);

    if (rp == NULL) {
        fprintf(stderr, "socket_connect: failed to connect to %s:%d\n", hostname, port);
        exit(1);
    }

    return sock;
}

/************************************************************/
int socket_read(int sock, char buf[], int buf_len, int *read_len)
{
  if ((buf_len < 0) || (sock < 0))
    fatal_error("socket_read: sock arg or buf len error");

  errno = 0;
  *read_len = read(sock, buf, buf_len);

  if (*read_len == 0) {
    fprintf(stderr, "socket_read: socket_no_connection\n");
    fflush(stderr);
    return -1;
  } else if (*read_len == -1) {
    if (errno == EWOULDBLOCK) {
      *read_len = 0;
      return 0;
    } else {
      *read_len = 0;
      fatal_error("socket_read: read");
    }
  }
  return 0;
}

/************************************************************/
int socket_write(int sock, char *buf, int buf_len, int *write_len)
{
    if ((buf_len < 0) || (sock < 0))
        fatal_error("socket_write: socket_illegal_argument");

    errno = 0;
    *write_len = write(sock, buf, buf_len);

    if (*write_len > 0) {
        return 0;
    } else if (*write_len == 0) {
        perror("socket_write problem");
        fatal_error("socket_write: socket_no_connection");
    } else if ((*write_len == -1) && (errno == EWOULDBLOCK)) {
        *write_len = 0;
        return 0;
    } else {
        *write_len = 0;
        fatal_error("socket_write: socket_internal_error");
    }
}

/************************************************************/
int socket_nonblocking(int sock)
{
  int one = 1;
  int rval = ioctl(sock, FIONBIO, &one);
  if (rval < 0)
    fatal_error("socket_nonblocking failed");
  return rval;
}

int socket_blocking(int sock)
{
  int zero = 0;
  int rval = ioctl(sock, FIONBIO, &zero);
  if (rval < 0)
    fatal_error("socket_blocking failed");
  return rval;
}

/************************************************************/
int socket_close(int sock)
{
  if (sock < 0)
    fatal_error("socket_close: illegal argument");

  if (shutdown(sock, 2) < 0)
    perror("socket_close: shutdown failed");
  if (close(sock) < 0)
    fatal_error("socket_close: close failed");

  return 1;
}

/************************************************************
 * Get peer's IP:port for the given socket.
 ************************************************************/
int get_peer_name(int sock, char *buf, size_t buf_size)
{
    struct sockaddr_in addr;
    socklen_t len = sizeof(addr);

    if (getpeername(sock, (struct sockaddr *)&addr, &len) == -1) {
        perror("get_peer_name: getpeername failed");
        return -1;
    }

    snprintf(buf, buf_size, "%s:%d", inet_ntoa(addr.sin_addr), ntohs(addr.sin_port));
    return 0;
}

/************************************************************
 * Get this machine's IP address (best guess)
 ************************************************************/
int get_host_ip(char *buf, size_t buf_size)
{
    char hostname[256];
    struct hostent *h;

    if (gethostname(hostname, sizeof(hostname)) == -1) {
        perror("get_host_ip: gethostname failed");
        return -1;
    }

    if ((h = gethostbyname(hostname)) == NULL) {
        perror("get_host_ip: gethostbyname failed");
        return -1;
    }

    struct in_addr **addr_list = (struct in_addr **) h->h_addr_list;
    if (addr_list[0] != NULL) {
        strncpy(buf, inet_ntoa(*addr_list[0]), buf_size);
        buf[buf_size - 1] = '\0';  // Ensure null-termination
        return 0;
    }

    return -1;
}
