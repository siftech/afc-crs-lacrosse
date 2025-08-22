// Copyright 2020 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
////////////////////////////////////////////////////////////////////////////////
extern "C" {
#include <ngx_config.h>
#include <ngx_core.h>
#include <ngx_event.h>
#include <ngx_mail.h>
#include <ngx_http.h>
#include <ngx_mail_pop3_module.h>
#include <ngx_process_cycle.h>
#include <string.h>
#include <sys/sendfile.h>
}

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <stdint.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <grp.h>
#include <pwd.h>
#include <sys/ioctl.h>
#include <sys/epoll.h>
#include <sys/syscall.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/select.h>
#include <errno.h>
#include <sys/uio.h>

static char configuration[] =
"events {\n"
"    worker_connections 4;\n"
"    multi_accept off;\n"
"    accept_mutex off;\n"
"}\n"
"daemon off;\n"
"mail {\n"
"    error_log stderr emerg;\n"
"    # Specify the authentication server\n"
"    auth_http localhost:9999;\n"
"    server {\n"
"        listen 80;\n"
"        protocol smtp;\n"
"        smtp_auth plain;\n"
"    }\n"
"}\n";

static ngx_cycle_t *cycle;
static ngx_log_t ngx_log;
static ngx_open_file_t ngx_log_file;
static char *my_argv[2];
static char arg1[] = {0, 0xA, 0};

extern char **environ;

int http_auth_fd = -1;
int auth_done = 0;

int mail_proxy_fd = -1;
int start_send = 0;
int received_quit = 0;
int final_ok_sent = 0;

int http_listen_fd = -1;
int http_client_fd = -1;

static const char *config_file = "/tmp/http_config.conf";

int pipefd[2];

// Opens a pipe, dupes that over the opened client socket and writes the fuzz data there
int setup_pipe_data(const uint8_t *data, size_t size)
{
  ssize_t numBytes;
  int flags;

  // If the client isn't connected then that is bad
  if (http_client_fd == -1) {
    exit(-1);
  }

  if (pipe(pipefd) == -1) {
    perror("pipe");
    exit(-1);
  }

  // Write the data then close the write end of the pipe
  numBytes = write(pipefd[1], data, size);
  if (numBytes == -1) {
    perror("write");
    exit(-1);
  }

  // Set the read end of the pipe to non-blocking
    flags = fcntl(pipefd[0], F_GETFL, 0);
    if (flags == -1) {
        perror("fcntl F_GETFL");
        exit(-1);
    }

    if (fcntl(pipefd[0], F_SETFL, flags | O_NONBLOCK) == -1) {
        perror("fcntl F_SETFL");
        exit(-1);
    }

  // Dup the read end of the pipe over the client fd
  if (dup2(pipefd[0], http_client_fd) == -1) {
        perror("dup2");
        exit(-1);
    }

  return 0;

}

// Create a base state for Nginx without starting the server
extern "C" int InitializeNginx(void)
{
  ngx_log_t *log;
  ngx_cycle_t init_cycle;
  ngx_core_conf_t  *ccf;

  ngx_debug_init();

  // Just output logs to stderr
  ngx_log.file = &ngx_log_file;
  ngx_log.log_level = NGX_LOG_EMERG;
  ngx_log_file.fd = ngx_stderr;
  log = &ngx_log;

  ngx_memzero(&init_cycle, sizeof(ngx_cycle_t));
  init_cycle.log = log;
  ngx_cycle = &init_cycle;

  init_cycle.pool = ngx_create_pool(1024, log);

  // Set custom argv/argc
  my_argv[0] = arg1;
  my_argv[1] = NULL;
  ngx_argv = ngx_os_argv = my_argv;
  ngx_argc = 0;

  if (ngx_strerror_init() != NGX_OK) {
    fprintf(stdout, "[ERROR] !!Failed to ngx_strerror_init\n");
    exit(-1);
  }

  ngx_time_init();

  ngx_regex_init();
  
  // Weird trick to free a leaking buffer always caught by ASAN
  // We basically let ngx overwrite the environment variable, free the leak and
  // restore the environment as before.
  char *env_before = environ[0];
  environ[0] = my_argv[0] + 1;

  if (ngx_os_init(log) != NGX_OK) {
    return 1;
  }

  free(environ[0]);
  environ[0] = env_before;

  ngx_crc32_table_init();

  ngx_slab_sizes_init();

  ngx_preinit_modules();

  FILE *fptr = fopen(config_file, "w");
  fprintf(fptr, "%s", configuration);
  fclose(fptr);
  init_cycle.conf_file.len = strlen(config_file);
  init_cycle.conf_file.data = (unsigned char *) config_file;

  cycle = ngx_init_cycle(&init_cycle);

  if ( cycle == NULL ) {
    fprintf(stdout, "[ERROR] init cycle failed probably bad config\n");
    exit(-1);
  }
  ngx_os_status(cycle->log);

  ngx_cycle = cycle;

  ccf = (ngx_core_conf_t *) ngx_get_conf(cycle->conf_ctx, ngx_core_module);
  
  if (ccf->master && ngx_process == NGX_PROCESS_SINGLE) {
    ngx_process = NGX_PROCESS_MASTER;
  }


  if (ngx_create_pidfile(&ccf->pid, cycle->log) != NGX_OK) {
    fprintf(stdout, "[ERROR] !!Failed to ngx_create_pidfile\n");
    exit(-1);
  }
  
  if (ngx_log_redirect_stderr(cycle) != NGX_OK) {
    fprintf(stdout, "[ERROR] !!Failed to ngx_log_redirect_stderr\n");
    exit(-1);
  }

  ngx_event_flags = 1;
  ngx_queue_init(&ngx_posted_accept_events);
  ngx_queue_init(&ngx_posted_next_events);
  ngx_queue_init(&ngx_posted_events);
  ngx_event_timer_init(cycle->log);

  for (int i = 0; cycle->modules[i]; i++) {
    if (cycle->modules[i]->init_process) {
      if (cycle->modules[i]->init_process(cycle) == NGX_ERROR) {
        //fatal
        exit(2);
      }
    }
  }

  return 0;
}

extern "C"
int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{ 
  static int init = InitializeNginx();
  if(init != 0) {
    fprintf(stdout, "[ERROR] Init failed\n");
    exit(0);
  }

  ngx_connection_t  *c;

  // Counter to ensure that no input can trigger an infinite loop
  int process_counter = 0;
  
  // This being here triggers a call to accept. The wrappers will handle the call
  //  and create the first socket.
  ngx_process_events_and_timers((ngx_cycle_t *)ngx_cycle);

  // Create the pipe that will allow nginx to read the data as if it were a socket.
  setup_pipe_data( data, size );

  // The accept takes a connection, which drops the free connection count to 2. There
  //    could also be a connection to the http auth server which takes up a connection
  //    as well as a connection to the mail proxy. The auth server connection will likely
  //    be disconnected prior to the proxy. Once all these connections are done it means
  //    that there is no additional data in the pipe previously set up so it is time to bail.
  while (ngx_cycle->free_connection_n != 3) {
    ngx_process_events_and_timers((ngx_cycle_t *)ngx_cycle);

    if (process_counter == 25) {
      fprintf(stdout, "[ERROR] Bailing out of mail harness due to hitting counter maximum\n");
      fflush(stdout);

      // Get the set of existing connections
      c = ngx_cycle->connections;

      // Loop through the connections and release them
      for (int i = 0; i < cycle->connection_n; i++) {

        if (c[i].fd != -1 && c[i].fd != http_listen_fd) {
          ngx_close_connection(&c[i]);
          }
      }
    } else {
      process_counter += 1;
    }
  }

  // Clean up the pipes
  close(pipefd[0]);
  close(pipefd[1]);

  // Make sure that all of the global state variables are reset.
  auth_done = 0;
  mail_proxy_fd = -1;
  start_send = 0;
  received_quit = 0;
  final_ok_sent = 0;

  http_client_fd = -1;

  return 0;
}

/*************
 * The code below here are wrappers that mimic the network traffic expected
 * of a mail proxy. They will be specific to each fuzzer and so must be
 * included in the fuzzer itself. Initially, when there was just the single
 * http fuzzer these were separate but with additional fuzzers comes the
 * need for individualized wrappers.
 * ************/
extern "C" typeof (writev) __real_writev;

extern "C"
ssize_t __wrap_writev(int fd, const struct iovec *iov, int iovcnt)
{ 
  return __real_writev(fd, iov, iovcnt);
}

extern "C"
int __wrap_ioctl(int fd, unsigned long request, ...) {
  return 0;
}

extern "C"
int __wrap_connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
{
  struct sockaddr_in *sin = (struct sockaddr_in*)addr;

  // This is for the auth connection
  if (ntohs(sin->sin_port) == 9999 ) {
    http_auth_fd = sockfd;
    return 0;
  } else if (ntohs(sin->sin_port) == 9998 ) {
    mail_proxy_fd = sockfd;

    received_quit = 0;
    final_ok_sent = 0;

    return 0;
  }

  return 0;
}

extern "C"
ssize_t __wrap_recv(int sockfd, void *buf, size_t len, int flags)
{
  ssize_t count;
  ssize_t res;
  char c;

  // This is a workaround for a wierd behavior of nginx. I don't know if it is a bug or not
  // It is in the ngx_unix_recv. If the length returned is equal to the size requested
  //   the rev->ready is not set to 0. This causes the select event to be deleted and
  //   it will stop reading from the socket. I am probably doing something wrong but this
  //   is an ok work around for the moment.
  size_t max_len = len - 1;

  // Specific handler for the auth connection
  if (sockfd == http_auth_fd && !auth_done) {
    auth_done = 1;
    strncpy((char*)buf, 
      "HTTP/1.1 200 OK\r\nAuth-Status: OK\r\nAuth-Server: 127.0.0.1\r\nAuth-Port: 9998\r\n\r\n",
      max_len);
    return strlen((char*)buf);

  } else if (sockfd == http_auth_fd ) {
    strncpy((char*)buf, "250 Ok\r\n", max_len);
    return strlen((char*)buf);
  }
  
  // Handle the commands that are proxied in
  if ( sockfd == mail_proxy_fd && !start_send) {
    start_send = 1;
    strncpy((char*)buf, "220 smtp.aixcc.com ESMTP\r\n", max_len);
    return strlen((char*)buf);
  } else if (sockfd == mail_proxy_fd ) {
    // If the proxy has received a QUIT then it is time to start closing down
    // Send the final ok then close it out
    if ( received_quit == 1 ) {
      final_ok_sent = 1;
    }

    if ( received_quit == 1 && final_ok_sent == 1 ) {
      return 0;
    }

    strncpy((char*)buf, "250 Ok\r\n", max_len);


    return strlen((char*)buf);
  }

  if (sockfd == http_client_fd ) {
    count = 0;

      while ( count < max_len ) {
        res = syscall(SYS_read, sockfd, &c, 1);

        if (res == 0 ) {
          return -1;
        } else if ( res < 0 ) {
          close(sockfd);

          return res;
        }
        
        ((char *)buf)[count++] = c;

        if ( c == '\n') {
          return count;
        }
      }

      return count;
  }

  return len;
}

extern "C"
int __wrap_bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen) {
  http_listen_fd = sockfd;

  return 0;
}

extern "C"
int __wrap_open(const char *pathname, int flags, mode_t mode)
{
  int fd = syscall(SYS_open, pathname, flags, mode);

  return fd;
}

extern "C"
int __wrap_close(int sockfd) {
  if (sockfd == http_auth_fd ) {
    http_auth_fd = -1;
    auth_done = 0;
  }

  if (sockfd == mail_proxy_fd ) {
    mail_proxy_fd = -1;

    start_send = 0;
    received_quit = 0;
    final_ok_sent = 0;
  }

  if (sockfd == http_client_fd ) {
    http_client_fd = -1;
  }

  if ( sockfd != 255 && sockfd != 256 && sockfd != 257 ) {
    return syscall(SYS_close, sockfd);
  }

  return 0;
}

extern "C"
ssize_t __wrap_send(int sockfd, const void *buf, size_t len, int flags)
{

  if ( sockfd == http_auth_fd ) {
  } else if ( sockfd == mail_proxy_fd ) {

    // When QUIT\r\n is received it is time to end the proxy connection
    if ( memcmp( buf, "QUIT\r\n", 6) == 0) {
      received_quit = 1;

    }
  } else if (sockfd == http_client_fd ) {
  } else {
  }

  for( size_t i = 0; i < len; i++) {
  }

  return len;
}

extern "C"
int __wrap_select(int nfds, fd_set *readfds, fd_set *writefds,
                  fd_set *exceptfds, struct timeval *timeout)
{
  int count = 0;

  if ( readfds ) {
    if ( http_auth_fd != -1) {
      FD_SET(http_auth_fd, readfds);

      count++;
    }

    if ( http_client_fd != -1) {
      FD_SET(http_client_fd, readfds);

      count++;
    }

    if ( mail_proxy_fd != -1) {
      FD_SET(mail_proxy_fd, readfds);

      count++;
    }

    if ( http_listen_fd != -1) {
      FD_SET(http_listen_fd, readfds);

      count++;
    }
  }

  if ( writefds ) {
    if ( http_auth_fd != -1) {
      FD_SET(http_auth_fd, writefds);

      count++;
    }

    if ( http_client_fd != -1) {
      FD_SET(http_client_fd, writefds);

      count++;
    }

    if ( mail_proxy_fd != -1) {
      FD_SET(mail_proxy_fd, writefds);

      count++;
    }
  }

  return count;
}

extern "C"
ssize_t __wrap_read(int fd, void *buf, size_t count)
{
  return 0;
}

extern "C"
int __wrap_epoll_create(int size) {
  return 256;
}

extern "C"
int  __wrap_epoll_create1(int flags) {
  return 256;
}

extern "C"
int __wrap_epoll_ctl(int epfd, int op, int fd, struct epoll_event *event) {
  return 0;
}

extern "C"
int __wrap_epoll_wait(int epfd, struct epoll_event *events,
                      int maxevents, int timeout) {
  events[0].data.fd = 255; // Use this fd everywhere
  return 1;
}

extern "C"
int __wrap_accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen) {
  struct sockaddr_in * sin = (struct sockaddr_in *)addr;

  // If this is the accept to take in the fake client then give it a valid socket
  if ( sockfd == http_listen_fd && http_client_fd == -1) {
    http_client_fd = syscall(SYS_socket, AF_INET, SOCK_STREAM, 0);

    sin->sin_family = AF_INET;
    sin->sin_port = htons(9999);
    sin->sin_addr.s_addr = 0x0100007f; // "127.0.0.1"

    return http_client_fd;
  }

  // We don't actually want to accept new connections aside from the main one
  // This causes the accept to return but then bails out of the ngx_accept_event() call quickly
  errno = 11; // NGX_EAGAIN
  
  return -1;
}

extern "C"
int __wrap_accept4(int sockfd, struct sockaddr *addr, socklen_t *addrlen, int flags) {
  return 0;
}

extern "C"
int __wrap_shutdown(int socket, int how)
{ 
  return 0; 
}

extern "C"
ssize_t __wrap_listen(int fd, int backlog)
{ 
  return 0; 
}

extern "C"
int __wrap_setsockopt(int fd, int level, int optname, const void *optval,
                      socklen_t optlen) {
  return 0;
}

extern "C"
int __wrap_getsockopt(int sockfd, int level, int optname,
                      void *optval, socklen_t *optlen)
{
  int *n = (int*)optval;

  *n = SOCK_STREAM;

  return 0;
}

extern "C"
int __wrap_chmod(const char *pathname, mode_t mode){
  return 0;
}

extern "C"
int __wrap_chown(const char *pathname, uid_t owner, gid_t group){
  return 0;
}

struct passwd pwd;
struct group grp;

extern "C"
struct passwd *__wrap_getpwnam(const char *name){
  pwd.pw_uid = 1;
  return &pwd;
}

extern "C"
struct group *__wrap_getgrnam(const char *name){
  grp.gr_gid = 1;
  return &grp;
}
