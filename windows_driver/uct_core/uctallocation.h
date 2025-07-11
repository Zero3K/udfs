/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctallocation.h
 *
 * Description : UDF block allocation verification.
 *
 * Author(s)   : Alexandre Sinitsyn, Gerrit Scholl.
 */

#ifndef __UCT_UCTALLOCATION_H__
#define __UCT_UCTALLOCATION_H__

/************ partition allocation ********************
 */

/* Find next range of bits that are all set to ONE. Start searching on
 * bit position *bitNr, and then change *bitNr to the start position
 * of range.
 * return value: Length of the range.
 */
extern Uint32 getNextBitRange(BitVector *vec,
                              Uint32     vecUnits,
                              Uint32    *bitNr);

extern bool bitVectorGetElement(BitVector *vec, Uint32 number);

/* vatFabricateBitmap:
 * Fabricate kind of unallocated bitmap from the VAT entries.
 * Entries containing VAT_UNUSED_ENTRY are unallocated blocks.
 */
extern bool vatFabricateBitmap( UdfMountContext  *mc,
                                Uint16            virtualPRef,
                                PartitionMapInfo *vPmi );


/* markUnallocatedPartitionSpace helps convert Tables to Bitmaps to be
 * comparable with partition allocation bit vector. It marks bits in
 * fabricatedBitmap bit vector
 */
extern bool markUnallocatedPartitionSpace(UdfMountContext *mc,
                                          Uint16  partRefNr,
                                          Uint32  logicalBlockNr,
                                          Uint32  nrOfBlocks,
                                          Uint32 *pNrOfBlocksMarkedAlready,
                                          bool    createOnly);

extern bool markPartitionAllocation(UdfMountContext *mc,
                                    Uint16  partRefNr,
                                    Uint32  logicalBlockNr,
                                    Uint32  nrOfBlocks,
                                    Uint32 *pNrOfBlocksMarkedAlready,
                                    bool    inspectOnly);


/* verifyPartitionAllocation():
 * ---------------------------------------------------------------
 * NOTE that this compare function is DESTRUCTIVE, pmi->pPartAlloc
 * and pPss->fabricatedBitmap are destroyed and freed !!!!!!!!
 * ---------------------------------------------------------------
 * FINAL compare for each partition of merged Space Set
 * Bitmaps/Tables and calculated bitmap.
 * verifyPartitionAllocation() can handle inconsistent
 * partition length and Space Bitmap numberOfBits/NumberOfBytes.
 */
extern bool verifyPartitionAllocation(UdfMountContext *mc);

#endif /* __UCT_UCTALLOCATION_H__ */

