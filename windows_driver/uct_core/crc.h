/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : crc.h
 *
 * Description : Header file used to calculate the CRC-CCITT checksum
 *               used in the TAG descriptors of ECMA 167.
 *               The algoritms are directly copied from the UDF 2.00
 *               document appendix pages 95 and 96.
 *               However chunk_cksum has a slightly different calling
 *               sequence than cksum(), in order to give the possibility
 *               of calculating a CRC in chunks.
 *               chunk_cksum(s,n,0) is identical to cksum(s,n).
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __UCT_CRC_H__
#define __UCT_CRC_H__

/* chunk_cksum is identical to cksum, except that a CRC
 * can be build up in chunks.
 * chunk_cksum(s,n,0) is identical to cksum(s,n)
 */
extern unsigned short chunk_cksum(register unsigned char *s, register int n,
                                  register unsigned short crc);
extern unsigned short unicode_cksum(register unsigned short *s, register int n);

/* 6.8 Extended Attribute Checksum Algorithm
 *
 * Calculates a 16-bit checksum of the Implementation Use
 * and Application Use Extended Attribute header. The fields
 * AttributeType through ImplementationIdentifier inclusively
 * represent the data covered by the checksum (48 bytes).
 */
extern Uint16 computeEAChecksum(Byte *data);

#endif  /* __UCT_CRC_H__ */


