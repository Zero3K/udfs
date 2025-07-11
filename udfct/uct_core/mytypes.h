/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : mytypes.h
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __UCT_MYTYPES_H__
#define __UCT_MYTYPES_H__

#ifdef        __GNUC__
#define __int64 long long int
#endif

#define Uint8   unsigned char
#define Uint16  unsigned short
#define Uint32  unsigned int
#define Uint64  unsigned __int64

#define Int8    signed char
#define Int16   signed short
#define Int32   signed int
#define Int64   __int64

#define Byte    unsigned char

#define bool int
#define TRUE    1
#define FALSE   0

#ifndef NULL
#define NULL    (0)
#endif

#endif /* __UCT_MYTYPES_H__ */

