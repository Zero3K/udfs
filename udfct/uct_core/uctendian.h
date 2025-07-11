/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctendian.h
 *
 * Description : Endian swap for big endian machines.
 *               Endian swap is done only once for each UDF structure
 *               directly after it has been read from the medium.
 *
 * Author(s)   : Gerrit Scholl
 */

#ifndef __UCT_UCTENDIAN_H__
#define __UCT_UCTENDIAN_H__

#include "udfstruct.h"

/*************** Endian byte swapping ****************************************
 *
 * isBigEndianPlatform() function:
 * This function detects whether the running platform
 * is big endian or not.
 * Used by endianSwap().
 */
extern bool isBigEndianPlatform();

/* endianSwap() function:
 * This function must be called for ALL platforms for
 * all fields with multi-byte units like Uint16, Uint32,
 * etc., after reading udf descriptors. This is because
 * block-read is done directly into PACKED structures.
 *
 * Besides endian swap it also may do an alignment check,
 * therefore endianSwap() must also be called for little
 * endian platforms.
 *
 * operation:
 * if alignBase != NULL, an alignment check is done first,
 * then an endian swap from little endian to big endian
 * is done for big endian systens, because UDF descriptors
 * are defined in little endian.
 * Big endian systems are detected using isBigEndianPlatform().
 *
 * endianSwap(...) must perform endian byte swapping for an array
 * of <nElem> elements, each element having a size of <elSize> bytes.
 *
 * Endian swap for an element size of 1 (byte), is defined
 * as a "no action" operation for completeness.
 */
extern void endianSwap(register Byte *arr, int elSize,
                       register Uint32 nElem, Byte *alignBase);

/* endian swap for all approriate UDF structures ***********************
 */
extern void endianSwapBoot2Tail(BootDescriptorTail *bt, Byte *d);
extern void endianSwapVat150Tail(VAT150Tail *vatTail, Byte *d);
extern void endianSwapVat200Head(VAT200Head *vh, Byte *d, Uint32 bytesRead);
extern void endianSwapEAGenericHead(EAGenericHead *gea);

/* endianSwapExtendedAttributeTail():
 * This function will perform endian swap of the
 * EA part after the EAGenericHead.
 * It is assumed that endian swap of EAGenericHead
 * has been performed successfully which means that
 * there is no need to check the attribute type here.
 * Also inspectEAbeforeSwapTail() must have been executed
 * successfully which means that enough bytes are read
 * and that isUdf is only TRUE for UDF Impl Use and Appl
 * Use EAs. If set TRUE, there is also enough
 * room for headerChecksum endian swap.
 * The only optional field that requires a 'bytesRead'
 * check is the implementationUse.impUseID EntityID
 * for a EATYPE_DEVSPEC EA.
 */
extern void endianSwapExtendedAttributeTail(ExtendedAttribute *ea,
                                            Uint32 bytesRead,
                                            bool   isUdfEA,
                                            Byte  *EASpace);

/* endianSwapDescriptorTag():
 *
 * Swap appropriate bytes for big endian machimes
 * Byte order may change. This is no problem for
 * TagChecksum and DescriptorCRC because the
 * TagChecksum calculation is byte order independent
 * and the DescriptorCRC calculation involves bytes
 * outside the DescriptorTag itself.
 */
extern void endianSwapDescriptorTag(Tag *dTag);

/* endianSwapDescriptorTail():
 *
 * Assumes that descriptor tag is swapped already if needed,
 * and the CRC check has been performed.
 * Swap now part following the tag.
 */
extern void endianSwapDescriptorTail(Tag *t);

/* endianSwapAllocationDescriptors:
 * Only adType ADT_SHORT and ADT_LONG allowed, all other types
 * (even ADT_INFE) will cause a FALSE result.
 *
 * Note that ads and pLenOfAds must be pointers to the actual
 * Allocation Descriptors and lengthOfAllocationDescriptors
 * fields in descriptor d.
 *
 * return result: FALSE for error, else TRUE
 * No error messages printed in case of error,
 * assumed to be done in swapAndVerifyAllocationDescriptors()
 */
extern bool endianSwapAllocationDescriptors(
                    Byte *ads, Uint32 *pLenOfAds,
                    Uint8 adType,  Byte *d);

#endif /* __UCT_UCTENDIAN_H__ */

