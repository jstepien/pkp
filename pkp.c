#include "pkp.h"
#include <stdint.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static int read_whole_file(int filedes, char **out, int *n) {
  size_t bufsize = 64 * 1024, total = 0, nread;
  char *buf = malloc(bufsize * sizeof(*buf));
  *n = 0;
  *out = 0;
  if (!buf)
    return 0;
  while ((nread = read(filedes, buf + total, bufsize - total)) > 0) {
    if (nread + total == bufsize) {
      bufsize *= 2;
      buf = realloc(buf, bufsize * sizeof(*buf));
      if (!buf)
        return 0;
    }
    total += nread;
  }
  *n = total;
  *out = buf;
  return (nread == 0 && total > 0) ? 0 : -1;
}

int main(int argc, char **argv) {
  const int size = 32 * 1024 * 1024;
  char *in, *out = malloc(size * sizeof(*out));
  int result, width, nin;
  if (argc < 2)
    return 3;
  if (read_whole_file(0, &in, &nin) < 0)
    return 2;
  int decompress = !strcmp(argv[1], "-d");
  if (decompress) {
    result = pkp_decompress(out, size, in, nin, &width);
  } else {
    width = atoi(argv[1]);
    result = pkp_compress(out, size, in, nin, width);
  }
  if (result < 0)
    return result;
  write(1, out, result);
  if (decompress) {
    fprintf(stderr, "%i\n", width);
  }
  free(in);
  free(out);
  return 0;
}
