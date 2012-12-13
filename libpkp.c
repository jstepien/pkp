#define _POSIX_C_SOURCE 1
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <errno.h>
#include "build/config.h"
#include "pkp.h"

static pid_t in_out_popen(const char *cmd, int descriptors[2]) {
  pid_t pid;
  int to[2], from[2], flags;
  pipe(to);
  pipe(from);
  flags = fcntl(from[0], F_GETFL, 0);
  if (fcntl(from[0], F_SETFL, flags | O_NONBLOCK) < 0)
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
  int fds[2], nread, nwritten;
  char *cur_out = out, *cur_in = in;
  pid_t pid = in_out_popen(cmd, fds);
  if (pid < 0)
    return -1;
  while (1) {
    if (cur_in - nin < in) {
      nwritten = write(fds[1], in, nin - (cur_in - in));
      if (nwritten < 0)
        return -2;
      else
        cur_in += nwritten;
    } else {
      close(fds[1]);
    }
    nread = read(fds[0], cur_out, nout - (cur_out - out));
    if (nread < 0) {
      if (errno == EAGAIN)
        continue;
      return -3;
    } else if (nread == 0)
      break;
    else
      cur_out += nread;
  }
  if (pid != wait(0))
    return -4;
  return cur_out - out;
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
  if (run("unxz | head -c 2", out, nout, in, nin) < 0)
    return -5;
  *width = ((unsigned char) out[0] << 8) + (unsigned char) out[1];
  return run("unxz |" PREFIX "/lib/" APPNAME "/Decode", out, nout, in, nin);
}
