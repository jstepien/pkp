#define _POSIX_C_SOURCE 1
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/select.h>
#include "build/config.h"
#include "pkp.h"

static int make_non_blocking(int fd) {
  int flags = fcntl(fd, F_GETFL, 0);
  return fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

static pid_t in_out_popen(const char *cmd, int descriptors[2]) {
  pid_t pid;
  int to[2], from[2];
  pipe(to);
  pipe(from);
  if (make_non_blocking(to[1]) < 0)
    return -1;
  if ((pid = fork()) > 0) {
    close(to[0]);
    close(from[1]);
    descriptors[0] = from[0];
    descriptors[1] = to[1];
    return pid;
  } else if (pid == 0) {
    close(to[1]);
    close(from[0]);
    dup2(to[0], fileno(stdin));
    dup2(from[1], fileno(stdout));
    execl("/bin/sh", "sh", "-c", cmd, NULL);
    exit(0);
  } else {
    return -2;
  }
  return -3;
}

static int run(const char *cmd, char *out, size_t nout, char *in, size_t nin) {
  int fds[2], maxfd, retval;
  char *cur_out = out, *cur_in = in;
  pid_t pid = in_out_popen(cmd, fds);
  if (pid < 0)
    return -1;
  fd_set rd, wr;
  maxfd = fds[0] > fds[1] ? fds[0] : fds[1];
  while (1) {
    int nread, nwritten, selected;
    do {
      FD_ZERO(&rd);
      FD_ZERO(&wr);
      if (cur_in - nin < in)
        FD_SET(fds[1], &wr);
      FD_SET(fds[0], &rd);
      selected = select(maxfd + 1, &rd, &wr, 0, 0);
    } while (selected == -1 && errno == EINTR);
    if (selected == -1) {
      retval = -7;
      break;
    }
    if (FD_ISSET(fds[0], &rd)) {
      nread = read(fds[0], cur_out, nout - (cur_out - out));
      if (nread < 0) {
        retval = -3;
        break;
      } else if (nread == 0) {
        retval = cur_out - out;
        break;
      }
      cur_out += nread;
    }
    if (FD_ISSET(fds[1], &wr)) {
      nwritten = write(fds[1], cur_in, nin - (cur_in - in));
      if (nwritten < 0) {
        retval = -2;
        break;
      }
      cur_in += nwritten;
      if (cur_in - nin >= in)
        close(fds[1]);
    }
  }
  close(fds[1]);
  close(fds[0]);
  if (pid != wait(0))
    return -4;
  return retval;
}

int pkp_compress(char *out, size_t nout, char *in, size_t nin, int width) {
  const char *fmt = PREFIX "/lib/" APPNAME "/Encode %i | xz";
  const size_t bufsize = 255;
  char buf[bufsize + 1];
  if (width <= 0 ||
      width >= (2 << 16) - 1 ||
      snprintf(buf, bufsize, fmt, width) < 0)
    return -5;
  return run(buf, out, nout, in, nin);
}

int pkp_decompress(char *out, size_t nout, char *in, size_t nin, int* width) {
  signal(SIGPIPE, SIG_IGN);
  run("unxz | head -c 2", out, nout, in, nin);
  signal(SIGPIPE, SIG_DFL);
  *width = ((unsigned char) out[0] << 8) + (unsigned char) out[1];
  return run("unxz |" PREFIX "/lib/" APPNAME "/Decode", out, nout, in, nin);
}
