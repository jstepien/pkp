#ifndef pkp_h_has_been_already_included_thanks_for_asking
#define pkp_h_has_been_already_included_thanks_for_asking

#include <stdlib.h>

/*
 * pkp_compress encodes RGBO as PKP
 *
 * out    an output buffer, where the encoded image will be saved
 * nout   the size of the output buffer
 * in     an input buffer with data in the RGBO format
 * nin    the size of the input buffer
 * width  the width of the image in pixels
 *
 * Returns the number of bytes written to the output buffer or a negative number
 * in case of errors.
 */
int pkp_compress(char *out, size_t nout, char *in, size_t nin, int width);

/*
 * pkp_decompress decodes PKP to PKP
 *
 * out    an output buffer, where the decoded RGBO data will be saved
 * nout   the size of the output buffer
 * in     an input buffer with data in the PKP format
 * nin    the size of the input buffer
 * width  a pointer to an int where the width of the image will be saved
 *
 * Returns the number of bytes written to the output buffer or a negative number
 * in case of errors.
 */
int pkp_decompress(char *out, size_t nout, char *in, size_t nin, int* width);

#endif /* pkp_h_has_been_already_included_thanks_for_asking */
