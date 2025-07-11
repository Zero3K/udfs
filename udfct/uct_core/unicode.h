/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : unicode.h
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __UCT_UNICODE_H__
#define __UCT_UNICODE_H__

#include <stddef.h>

#include "mytypes.h"

/***********************************************************************
 * OSTA compliant Unicode compression, uncompression routines.
 * Copyright 1995 Micro Design International, Inc.
 * Written by Jason M. Rinn.
 * Micro Design International gives permission for the free use of the
 * following source code.
 */

/*
 * The following two typedefs are to remove compiler dependancies.
 * byte needs to be unsigned 8-bit, and unicode_t needs to be
 * unsigned 16-bit.
 */
typedef unsigned short  unicode_t;
typedef unsigned char   byte;

extern int UncompressUnicodeEnvelope(
  int        numberOfBytes, /* (Input) number of bytes read from media.  */
  byte      *UDFCompressed, /* (Input) bytes read from media.            */
  unicode_t *unicode);      /* (Output) uncompressed unicode characters. */


/**** Dstring equivalents *****************************
 * uncompressDstring:
 * Check Dstring length byte before calling UncompressUnicode()
 *
 * return value: -1 in case of an error,
 *         else: nmb of unicode chars found
 */
extern int uncompressDstring(
    Uint32     dstringSize, /* Dstring total byte size */
    byte      *dstring,     /* (Input) Dstring pointer */
    unicode_t *unicode);    /* (Output) uncompressed unicode chars */

#endif /* __UCT_UNICODE_H__ */

