/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : unicode.c
 *
 * Description : OSTA CS0 unicode compression functions and
 *               supporting functions.
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#include "general.h"
#include "unicode.h"

/* UncompressUnicodeOSTA() and CompressUnicodeOSTA() are the
 * functions UncompressUnicode() and CompressUnicode() as in
 * 6.4 of the UDF 2.01 specification.
 */

/***********************************************************************
* Takes an OSTA CS0 compressed unicode name, and converts
* it to Unicode.
* The Unicode output will be in the byte order
* that the local compiler uses for 16-bit values.
* NOTE: This routine only performs error checking on the compID.
* It is up to the user to ensure that the unicode buffer is large
* enough, and that the compressed unicode name is correct.
*
* RETURN VALUE
*
*    The number of unicode characters which were uncompressed.
*    A -1 is returned if the compression ID is invalid.
*
* Note that if numberOfBytes is an even number for compression
* id 16, the least significant byte of the last unicode character
* will be padded with #00.
*/
static int UncompressUnicodeOSTA(
            int          numberOfBytes, /* (Input) number of bytes read from media.  */
            byte        *UDFCompressed, /* (Input) bytes read from media.            */
            unicode_t   *unicode)       /* (Output) uncompressed unicode characters. */
{
    unsigned int compID;
    int returnValue, unicodeIndex, byteIndex;

    /* Use UDFCompressed to store current byte being read. */
    compID = UDFCompressed[0];

    /* First check for valid compID. */
    if (compID != 8 && compID != 16)
    {
        returnValue = -1;
    }
    else
    {
        unicodeIndex = 0;
        byteIndex = 1;

        /* Loop through all the bytes. */
        while (byteIndex < numberOfBytes)
        {
            if (compID == 16)
            {
                /* Move first byte to high byte of
                 * unicode char, #00 in low byte.
                 */
                unicode[unicodeIndex] = (unicode_t) (UDFCompressed[byteIndex++] << 8);
            }
            else
            {
                unicode[unicodeIndex] = 0;
            }
            if (byteIndex < numberOfBytes)
            {
                /* Then the next byte to the low bits. */
                unicode[unicodeIndex] |= UDFCompressed[byteIndex++];
            }
            unicodeIndex++;
        }
        returnValue = unicodeIndex;
    }
    return(returnValue);
}


/* UncompressUnicodeEnvelope():
 * Envelope function for UncompressUnicodeOSTA() in
 * order to be able to convert erroneous compressed unicode
 * identifiers to a sensible unicode string.
 *
 * Temporary replace compression ID by 8 or 16.
 * - Replace 254 by 8 and 255 by 16 as defined for UDF 2.01+.
 * - For illegal compression IDs use compression ID 16.
 * Mind that although the compression IDs 254 and 255 are illegal
 * for UDF 1.50- and no compression algorithm is defined for
 * UDF 2.00, the compression algorithm defined in UDF 2.01+ is used.
 * Errors are not flagged here but in verifyDstring() and
 * verifyDchars().
 *
 * Check for numberOfBytes == 0 error.
 *
 * Note that if numberOfBytes is an even number for
 * (temporary) compression ID 16, the least significant byte
 * of the last unicode character will be padded with #00
 * by UncompressUnicodeOSTA().
 *
 * Return value as for UncompressUnicodeOSTA();
 */
extern int UncompressUnicodeEnvelope(
            int          numberOfBytes, /* (Input) number of bytes read from media.  */
            byte        *UDFCompressed, /* (Input) bytes read from media.            */
            unicode_t   *unicode)       /* (Output) uncompressed unicode characters. */
{
    byte compID;
    int  result;

    if( numberOfBytes == 0 )
    {   return 0;               /* ok, e.g. for parent FID */
    }
    else if( numberOfBytes < 0 )
    {   return -1;              /* error */
    }
    compID = UDFCompressed[0];

    /* temp replace compression ID by 8 or 16,
     * for rules see header text above.
     */
    UDFCompressed[0] = (byte)((compID == 8 || compID == 254) ? 8 : 16);

    result = UncompressUnicodeOSTA(numberOfBytes, UDFCompressed, unicode);

    UDFCompressed[0] = compID;      /* restore original value */
    return result;
}


/**** Dstring equivalents *****************************
 * uncompressDstring:
 * Check Dstring length byte before calling UncompressUnicodeEnvelope().
 * ECMA 1/7.2.12
 *
 * return value: -1 in case of a fatal error,
 *         else: nmb of unicode chars found
 *
 * Note: If dstring[dstringSize-1] >= dstringSize then shrink
 *       it to dstringSize-1 in order to uncompress the Dstring
 *       content, this is not an error here.
 */
extern int uncompressDstring(
    Uint32     dstringSize, /* Dstring total byte size */
    byte      *dstring,     /* (Input) Dstring pointer */
    unicode_t *unicode )    /* (Output) uncompressed unicode chars */
{
    Uint8 dchSize = dstring[dstringSize-1];

    if( (Uint32) dchSize >= dstringSize )
    {   dchSize = (Uint8) (dstringSize-1);  /* shrink */
    }
    return UncompressUnicodeEnvelope(dchSize, dstring, unicode);
}

