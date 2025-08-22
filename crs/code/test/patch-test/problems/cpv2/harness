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
#include <ngx_http.h>
#include <sys/sendfile.h>
}
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <sys/uio.h>

static char configuration[] =
"error_log stderr emerg;\n"
"trace on;\n"
"worker_rlimit_nofile 8192;\n"
"pid logs/nginx.pid;\n"
"remote_admin off;\n"
"events {\n"
"    worker_connections 2;\n"
"    multi_accept off;\n"
"    accept_mutex off;\n"
"}\n"
"http {\n"
"    server_tokens off;\n"
"    sendfile        on;\n"
"    default_type application/octet-stream;\n"
"    map $http_upgrade $connection_upgrade {\n"
"      default upgrade;\n"
"      '' close;\n"
"    }\n"
"    error_log stderr emerg;\n"
"    access_log stderr;\n"
"    map $subdomain $nss {\n"
"      default local_upstream;\n"
"    }\n"
"    upstream local_upstream {\n"
"      server 127.0.0.1:1010 max_fails=0;\n"
"      server 127.0.0.1:1011 max_fails=0;\n"
"      server 127.0.0.1:1012 max_fails=0;\n"
"      server 127.0.0.1:1013 max_fails=0;\n"
"      server 127.0.0.1:1014 max_fails=0;\n"
"      server 127.0.0.1:1015 max_fails=0;\n"
"      server 127.0.0.1:1016 max_fails=0;\n"
"      server 127.0.0.1:1017 max_fails=0;\n"
"      server 127.0.0.1:1018 max_fails=0;\n"
"      server 127.0.0.1:1019 max_fails=0;\n"
"    }\n"
"    map $http_user_agent $is_modern_browser {\n"
"        default         0;\n"
"        \"~*Firefox\"     1;\n"
"        \"~*Chrome\"      1;\n"
"        \"~*Safari\"      1;\n"
"        \"~*Opera\"       1;\n"
"        \"~*Edge\"        1;\n"
"    }\n"
"    client_max_body_size 256M;\n"
"    client_body_temp_path /tmp/;\n"
"    proxy_temp_path /tmp/;\n"
"    proxy_buffer_size 24K;\n"
"    proxy_max_temp_file_size 0;\n"
"    proxy_buffers 8 4K;\n"
"    proxy_busy_buffers_size 28K;\n"
"    proxy_buffering off;\n"
"    server {\n"
"      listen 80;\n"
"      server_name ~^(?<subdomain>.+)\\.url.com$;\n"
"      proxy_next_upstream off;\n"
"      proxy_read_timeout 5m;\n"
"      proxy_http_version 1.1;\n"
"      proxy_set_header Host $http_host;\n"
"      proxy_set_header X-Real-IP $remote_addr;\n"
"      proxy_set_header X-Real-Port $remote_port;\n"
"      location / {\n"
"          root   /out/html;\n"
"          index  index.html;\n"
"          userid          on;\n"
"          userid_name     uid;\n"
"          userid_path     /;\n"
"          userid_expires  365d;\n"
"          userid_service  1;\n"
"          if ($is_modern_browser) {\n"
"              # Special configuration for modern browsers\n"
"              add_header Set-Cookie \"cookie=$http_cookie;host=$host\";\n"
"          }\n"
"      }\n"
"      location /old {\n"
"          rewrite ^/old/(.*)$ /new/$1 last;\n"
"      }\n"
"      location /lastConnection {\n"
"          return 200 \"Last IP: $last_ip\";\n"
"      }\n"
"      location /host_specs {\n"
"          return 200 \"Host Specifications:\\n$host_specs\";\n"
"      }\n"
"      location /prox/ {\n"
"        proxy_pass http://$nss;\n"
"        proxy_set_header Host $http_host;\n"
"        proxy_set_header X-Real-IP $remote_addr;\n"
"        proxy_set_header X-Real-Port $remote_port;\n"
"        proxy_set_header Connection '';\n"
"        chunked_transfer_encoding off;\n"
"        proxy_buffering off;\n"
"        proxy_cache off;\n"
"      }\n"
"        location = /empty {\n"
"            empty_gif;\n"
"        }\n"
"      }\n"
"}\n"
"\n";

static ngx_cycle_t *cycle;
static ngx_log_t ngx_log;
static ngx_open_file_t ngx_log_file;
static char *my_argv[2];
static char arg1[] = {0, 0xA, 0};

extern char **environ;

static const char *config_file = "/tmp/http_config.conf";

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

int http_listen_fd = -1;
int http_client_fd = -1;

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


extern "C"
int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
  static int init = InitializeNginx();
  if(init != 0) {
    fprintf(stdout, "[ERROR] Init failed\n");
    exit(0);
  }

  //data = (const uint8_t *)"GET / HTTP/1.1\r\nHost: localhost\r\nAccept: */*\r\n\r\n";
  //size = strlen((const char *)data);

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
  while (ngx_cycle->free_connection_n != 1) {
    ngx_process_events_and_timers((ngx_cycle_t *)ngx_cycle);
  }

  // Clean up the pipes
  close(pipefd[0]);
  close(pipefd[1]);

  // Make sure that all of the global state variables are reset.
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

#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <grp.h>
#include <pwd.h>
#include <sys/epoll.h>

// Because the __real_<symbol> doesn't get resolved at compile time we need to help out a bit
extern "C" typeof (recv) __real_recv;
extern "C" typeof (open) __real_open;
extern "C" typeof (close) __real_close;
extern "C" typeof (send) __real_send;
extern "C" typeof (select) __real_select;
extern "C" typeof (read) __real_read;
extern "C" typeof (epoll_create) __real_epoll_create;
extern "C" typeof (epoll_create1) __real_epoll_create1;
extern "C" typeof (epoll_ctl) __real_epoll_ctl;
extern "C" typeof (epoll_wait) __real_epoll_wait;
extern "C" typeof (accept) __real_accept;
extern "C" typeof (accept4) __real_accept4;
extern "C" typeof (getsockopt) __real_getsockopt;
extern "C" typeof (ioctl) __real_ioctl;
extern "C" typeof (writev) __real_writev;

extern "C"
ssize_t __wrap_writev(int fd, const struct iovec *iov, int iovcnt)
{
  size_t totalBytes = 0;
  
  for (int i = 0; i < iovcnt; ++i) {
    totalBytes += iov[i].iov_len;
  }
  
  return totalBytes;
}

extern "C"
int __wrap_ioctl(int fd, unsigned long request, ...) {
  return 0;
}

extern "C"
int __wrap_connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen) {
  return 0;
}

extern "C"
ssize_t __wrap_recv(int sockfd, void *buf, size_t len, int flags)
{
  ssize_t count;
  ssize_t res;
  char c;

  if (sockfd == http_client_fd ) {
    count = 0;

    while ( count < len ) {
      res = __real_read(sockfd, &c, 1);
      
      if (res == 0 ) {
        return count;
      } else if ( res < 0 ) {
        return 0;
      }
      
      ((char *)buf)[count++] = c;
      
      if ( c == '\n') {
        return count;
      }
    }
    
    return count;
  }

  return 0;
}

extern "C"
int __wrap_bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
{
  return 0;
}

extern "C"
int __wrap_open(const char *pathname, int flags, mode_t mode)
{
  return __real_open(pathname, flags, mode);
}

extern "C"
int __wrap_close(int sockfd)
{
  if ( sockfd == http_client_fd ) {
    http_client_fd = -1;
  }

  if ( sockfd == http_listen_fd ) {
    http_listen_fd = -1;
  }

  return __real_close(sockfd);
}

extern "C"
ssize_t __wrap_send(int sockfd, const void *buf, size_t len, int flags)
{
  return __real_send(sockfd, buf, len, flags);
}

extern "C"
int __wrap_select(int nfds, fd_set *readfds, fd_set *writefds,
                  fd_set *exceptfds, struct timeval *timeout)
{
  int count = 0;

  if ( readfds ) {
    if ( http_listen_fd != -1) {
      FD_SET(http_listen_fd, readfds);
      count++;
    }

    if ( http_client_fd != -1) {
      FD_SET(http_client_fd, readfds);
      count++;
    }
  }

  if ( writefds ) {
    if ( http_client_fd != -1) {
      FD_SET(http_client_fd, writefds);
      count++;
    }
  }

  return count;
}

extern "C"
ssize_t __wrap_read(int fd, void *buf, size_t count)
{
  return __real_read(fd, buf, count);
}

extern "C"
int __wrap_epoll_create(int size)
{
  return __real_epoll_create(size);
}

extern "C"
int  __wrap_epoll_create1(int flags)
{
  return __real_epoll_create1(flags);
}

extern "C"
int __wrap_epoll_ctl(int epfd, int op, int fd, struct epoll_event *event)
{
  return __real_epoll_ctl(epfd, op, fd, event);
}

extern "C"
int __wrap_epoll_wait(int epfd, struct epoll_event *events,
                      int maxevents, int timeout)
{
  return __real_epoll_wait(epfd, events, maxevents, timeout);
}

extern "C"
int __wrap_accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen)
{
  struct sockaddr_in * sin = (struct sockaddr_in *)addr;

  // We shouldn't ever actually call accept
  if ( sockfd == http_listen_fd && http_client_fd == -1) {
    // We do want a real socket though
    http_client_fd = socket( AF_INET, SOCK_STREAM, 0);

    // Setup the appropriate false connection information
    sin->sin_family = AF_INET;
    sin->sin_port = htons(9999);
    sin->sin_addr.s_addr = 0x0100007f; // "127.0.0.1"

    return http_client_fd;
  }

  // Otherwise, set errno and return a failure
  errno = 11; // NGX_EAGAIN

  return -1;
}

extern "C"
int __wrap_accept4(int sockfd, struct sockaddr *addr, socklen_t *addrlen, int flags)
{  
  return __real_accept4(sockfd, addr, addrlen, flags);
}

extern "C"
int __wrap_shutdown(int socket, int how)
{ 
  return 0; 
}

extern "C"
ssize_t __wrap_listen(int fd, void *buf, size_t bytes)
{
  // There should only be one listener set
  http_listen_fd = fd;

  return 0;
}

extern "C"
int __wrap_setsockopt(int fd, int level, int optname, const void *optval,
                      socklen_t optlen)
{
  return 0;
}

extern "C"
int __wrap_getsockopt(int sockfd, int level, int optname,
                      void *optval, socklen_t *optlen)
{
  int *n = (int*)optval;

  // The getsockopt wants to confirm that the socket is a sock_stream
  // SOL_SOCKET, SO_TYPE

  *n = SOCK_STREAM;

  return 0;
}

extern "C"
int __wrap_chmod(const char *pathname, mode_t mode)
{
  return 0;
}

extern "C"
int __wrap_chown(const char *pathname, uid_t owner, gid_t group)
{
  return 0;
}

struct passwd pwd;
struct group grp;

extern "C"
struct passwd *__wrap_getpwnam(const char *name)
{
  pwd.pw_uid = 1;
  return &pwd;
}

extern "C"
struct group *__wrap_getgrnam(const char *name)
{
  grp.gr_gid = 1;
  return &grp;
}
