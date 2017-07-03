/*
 * Incredibly minimal implementation of SHA1.
 * Totally independent of any other code (even libc) so it can be
 * run on bare hardware.
 *
 * Originally developed as part of the WinKexec project.
 * (This SHA1 implementation is the only part of WinKexec that is under the
 *  license given below; the rest is under GPLv3 or later.)
 *
 * Copyright (C) 2009 John Stumpo
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY JOHN STUMPO ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL JOHN STUMPO BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "sha1.h"

/* All of these macros assume use on a 32-bit variable.
   Additionally, SWAP assumes we're little-endian.  */
#define SWAP(a) ((((a) >> 24) & 0x000000ff) | (((a) >>  8) & 0x0000ff00) | \
                 (((a) <<  8) & 0x00ff0000) | (((a) << 24) & 0xff000000))
#define ROL(a, b) (((a) << (b)) | ((a) >> (32 - (b))))
#define ROR(a, b) ROL((a), (32 - (b)))

/* Copy srclen bytes of src to dest, padding with zeros to dstlen. */
static void _copy_buffer(void* dest, const void* src, size_t srclen, size_t dstlen)
{
  size_t i;
  for (i = 0; i < dstlen; i++) {
    if (i < srclen)
      *(unsigned char*)(dest + i) = *(unsigned char*)(src + i);
    else
      *(unsigned char*)(dest + i) = 0;
  }
}

/* The SHA1 implementation proper. */
void sha1(void* outbuf, const void* inbuf, size_t length)
{
  size_t i, j;
  int remaining_bytes;
  uint32_t h0, h1, h2, h3, h4, a, b, c, d, e, temp;
  uint32_t w[80];
  unsigned char buf[64];

  /* Initialize SHA1 hash state. */
  h0 = 0x67452301;
  h1 = 0xefcdab89;
  h2 = 0x98badcfe;
  h3 = 0x10325476;
  h4 = 0xc3d2e1f0;

  /* The extra 9 bytes are the pad byte (0x80) and 64-bit bit count that
     are appended to the data being hashed.  (There will more than likely
     also be some zeroes in between the 0x80 and the bit count so that we
     operate on a multiple of 64 bytes; 9 bytes, though, is the minimal
     amount of extra data.)  */
  for (i = 0; i < length + 9; i += 64) {

    /* Perform any padding necessary. */
    remaining_bytes = length - i;
    if (remaining_bytes >= 64) {
      _copy_buffer(buf, inbuf + i, 64, 64);
    } else if (remaining_bytes >= 0) {
      _copy_buffer(buf, inbuf + i, remaining_bytes, 64);
      buf[remaining_bytes] = 0x80;
    } else {
      _copy_buffer(buf, NULL, 0, 64);
    }
    if (remaining_bytes < 56)
      *(uint32_t*)(buf + 60) = SWAP(length * 8);

    /* Build the input array. */
    for (j = 0; j < 16; j++)
      w[j] = SWAP(*(uint32_t*)(buf + j * 4));
    for (j = 16; j < 80; j++)
      w[j] = ROL(w[j - 3] ^ w[j - 8] ^ w[j - 14] ^ w[j - 16], 1);

    /* Load hash state. */
    a = h0;
    b = h1;
    c = h2;
    d = h3;
    e = h4;

    for (j = 0; j < 80; j++) {
      if (j < 20)
        temp = ((b & c) | ((~b) & d)) + 0x5a827999;
      else if (j < 40)
        temp = (b ^ c ^ d) + 0x6ed9eba1;
      else if (j < 60)
        temp = ((b & c) | (b & d) | (c & d)) + 0x8f1bbcdc;
      else
        temp = (b ^ c ^ d) + 0xca62c1d6;
      temp += ROL(a, 5) + e + w[j];

      e = d;
      d = c;
      c = ROR(b, 2);
      b = a;
      a = temp;
    }

    /* Incorporate the results of the hash operation. */
    h0 += a;
    h1 += b;
    h2 += c;
    h3 += d;
    h4 += e;
  }

  /* Write the hash into the output buffer. */
  *(uint32_t*)(outbuf) = SWAP(h0);
  *(uint32_t*)(outbuf + 4) = SWAP(h1);
  *(uint32_t*)(outbuf + 8) = SWAP(h2);
  *(uint32_t*)(outbuf + 12) = SWAP(h3);
  *(uint32_t*)(outbuf + 16) = SWAP(h4);
}
