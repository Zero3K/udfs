/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctallocation.c
 *
 * Description : UDF block allocation verification.
 *
 * Author(s)   : Alexandre Sinitsyn, Gerrit Scholl.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mytypes.h"
#include "general.h"
#include "uctdata.h"
#include "uctgeneral.h"
#include "uctstatus.h"
#include "uctallocation.h"


/************ partition allocation ********************
 *
 * Forward references:
 */
static Uint32 lw_offset(Uint32 number);
static Uint32 bit_offset(Uint32 number);
static void   bitVectorSetElement(BitVector *vec, Uint32 number);
static void   bitVectorResetElement(BitVector *vec, Uint32 number);
/** extern bool bitVectorGetElement(BitVector *vec, Uint32 number); **/
static void   bitVectorCompare(BitVector *vec1, BitVector* vec2,
                               Uint32     vecUnits);
static void   bitVectorMerge(BitVector *vec1, BitVector* vec2,
                             Uint32     vecUnits);
/** static void   bitVectorInvert(BitVector *vec, Uint32 vecLen); **/
static void   bitVectorResetTail(BitVector *vec, Uint32 vecLength);
static Uint32 bitVectorCountBitsSet(BitVector *vec, Uint32 vecUnits);
static void   printBlockNumbers(BitVector *vec, Uint32 vecUnits,
                                UdfMountContext *mc, Uint16 partRefNr,
                                char *txt1, char *txt2,
                                bool printBoth);

/* allocateBitMap()
 * No fatal uctExit() but FALSE return in case of allocation error.
 * initValue should be 0 or 0xFF.
 */
static bool allocateBitMap( BitVector **pBitvec,
                            Uint32      vecUnits,
                            Byte        initValue )
{
    Uint64 byteSize = (Uint64) vecUnits * sizeof(BitVector);

    if( byteSize > (Uint64) UCT_MAXBITMAP_BYTESIZE )
    {   MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
             "\tError: Bitmap requires ");
          printUint64(VERBOSE00level, byteSize, FALSE, "%lu");
          fprintf(uctout,
                                    " bytes. The verifier refuses\n"
            "-\t       to alloacte more than %lu Mbyte for a bitmap.\n"
            "-\t=====> Please report if this still is a valid bitmap.\n",
                        UCT_MAXBITMAP_MBSIZE);
        MLIMITend;
        return FALSE;
    }
    if( ((*pBitvec) = tst_malloc((Uint32) byteSize, __FILE__, __LINE__))
        == NULL )
    { return FALSE; /* NO fatal uctExit() */
    }
    memset((char*)(*pBitvec), initValue, (Uint32) byteSize);
    return TRUE;
}

/* vatFabricateBitmap:
 * Fabricate kind of unallocated bitmap from the VAT entries.
 * Entries containing VAT_UNUSED_ENTRY are unallocated blocks.
 */
extern bool vatFabricateBitmap( UdfMountContext  *mc,
                                Uint16            virtualPRef,
                                PartitionMapInfo *vPmi )
{
    Uint32 numberOfEntries = vPmi->vatRec.numberOfEntries,
           i, *pVatEntry;

    /* Create bitmap vector, needed for tables that
     * do not have unused entries.
     */
    if( !markUnallocatedPartitionSpace(mc, virtualPRef,
                    0, 0, NULL, TRUE) )     /* createOnly */
    { return FALSE;
    }
    /* test VAT table entries
     */
    for( i = 0, pVatEntry = vPmi->vatRec.vatTable;
         i < numberOfEntries;
         i++,  pVatEntry++ )
    {   /* fabricate 'unallocated bitmap'
         * for virtual partition.
         */
        if(   *pVatEntry == VAT_UNUSED_ENTRY
           && !markUnallocatedPartitionSpace(mc, virtualPRef,
                                             i, 1, NULL, FALSE) )
        { return FALSE;
        }
    }
    return TRUE;
}

/* markUnallocatedPartitionSpace helps convert Tables to Bitmaps to be
 * comparable with partition allocation bit vector. It marks bits in
 * fabricatedBitmap bit vector
 *
 * TODO: error if extent is PARTLY unallocated already
 */
extern bool markUnallocatedPartitionSpace(UdfMountContext *mc,
                                          Uint16  partRefNr,
                                          Uint32  logicalBlockNr,
                                          Uint32  nrOfBlocks,
                                          Uint32 *pNrOfBlocksMarkedAlready,
                                          bool    createOnly)
{
    PartitionMapInfo  *pmi;         /* partition map info pointer */
    PartitionSpaceSet *pPss;        /* partition space set pointer */
    Uint32 actualPartitionLength;   /* nmb of logical blocks in a partition */
    Uint32 vecUnits;                /* nmb of BitVector units in fabricatedBitmap */
    Uint32 i;                       /* cycle counter */
    Uint32 blocksMarkedAlready;
    static bool staticFabricatedBitmapError = FALSE; /* flag for all partitions */

    if(  pNrOfBlocksMarkedAlready != NULL )
    {   *pNrOfBlocksMarkedAlready = 0;
    }
    /* Make some checking before marking */

    /* logicalBlockNr may be absolute media block address,
     * in that case: partRefNr == (Uint16) -1
     * Check partition reference number
     * Check memory allocation error
     */
    if(   partRefNr == (Uint16) -1
       || partRefNr >= mc->vi->lvd->numberOfPartitionMaps
       || staticFabricatedBitmapError )
    {   return FALSE;
    }

    /* current partition map info
     */
    pmi = &mc->partitionMapInfo[partRefNr];

    /* For virtual partition, no special acts for
     * underlying partition here.
     */
    pPss = &pmi->pss;
    actualPartitionLength = pmi->actualPartitionLength;

    /* Check whether it is the first call for this partition or not
     * If it is the first call then we have to initialize
     * partition allocation bit vector
     */
    if( pPss->fabricatedBitmap == NULL )
    {   /* partition allocation initialization
         * Initially set all bits to allocated (ZERO)
         * to be comparable with SBD.
         */
        vecUnits = ROUNDUPELEMENTS(actualPartitionLength, 8 * sizeof(BitVector));

        if( !allocateBitMap((BitVector**) &pPss->fabricatedBitmap,
                            vecUnits, 0x00) )
        {   Uint16 pr;
            staticFabricatedBitmapError = TRUE; /* for all partitions *
            /* Free other partition fabricatedBitmap, if any */
            for(pr = 0; pr < mc->vi->lvd->numberOfPartitionMaps; pr++)
            {   checkFree((void**)&mc->partitionMapInfo[pr].pss.fabricatedBitmap);
            }
            return FALSE;       /* NO fatal uctExit(...) */
        }
    }
    if( createOnly )            /* done */
    {   return TRUE;
    }

    /* Ready to mark bits in BitVector array
     */
    blocksMarkedAlready = 0;
    for(i = 0; i < nrOfBlocks; i++)
    {
        if( (logicalBlockNr + i) >= actualPartitionLength )
        {   /* logical block number is outside the partition */
            return FALSE;
        }
        if( bitVectorGetElement(pPss->fabricatedBitmap, logicalBlockNr + i))
        {
            blocksMarkedAlready++;
        }
        bitVectorSetElement(pPss->fabricatedBitmap, logicalBlockNr + i);
    }

#ifdef DEBUG01
    if(   blocksMarkedAlready != 0
       && blocksMarkedAlready != nrOfBlocks )
    {   ifPRINTdebug01(uctout,
          "markUnallocatedPartitionSpace error: Extent"
          " is PARTLY unallocated already, %lu of %lu blocks\n",
                blocksMarkedAlready, nrOfBlocks);
        ENDif;
    }
#endif  /* DEBUG01 */

    if(  pNrOfBlocksMarkedAlready != NULL )
    {   *pNrOfBlocksMarkedAlready = blocksMarkedAlready;
    }
    return TRUE;
} /* extern bool markUnallocatedPartitionSpace(UdfMountContext *mc,... */


extern bool markPartitionAllocation(UdfMountContext *mc,
                                    Uint16  partRefNr,
                                    Uint32  logicalBlockNr,
                                    Uint32  nrOfBlocks,
                                    Uint32 *pNrOfBlocksMarkedAlready,
                                    bool    inspectOnly)
{
    PartitionMapInfo *pmi;      /* partition map info pointer */
    Uint32 actualPartitionLength;   /* number of logical blocks in a partition */
    Uint32 vecUnits;            /* number of BitVector units in pPartAlloc */
    Uint32 i;                   /* cycle counter */
    Uint32 blocksMarkedAlready, nonVirtualMarkedAlready;

    if(  pNrOfBlocksMarkedAlready != NULL )
    {   *pNrOfBlocksMarkedAlready = 0;
    }
    /* Make some checking before marking */

    /* logicalBlockNr may be absolute media block address,
     * in that case: partRefNr == (Uint16) -1
     * Check partition reference number
     * Check memory allocation error
     */
    if(   partRefNr == (Uint16) -1
       || partRefNr >= mc->vi->lvd->numberOfPartitionMaps )
    {   return FALSE;
    }

    /* current partition map info
     */
    pmi = &mc->partitionMapInfo[partRefNr];

    if( pmi->partAllocOutOfMem )    /* previously problems */
    {   return FALSE;
    }

    /* Check type of partition
     * For Virtual partition, mark blocks in both the virtual
     * and the underlying partition, the latter first.
     * Translate VIRTUAL partition address
     *     into PHYSICAL partition address.
     */
    nonVirtualMarkedAlready = 0;
    if( pmi->pMapType == PMAPTYPE_VIRTUAL )
    {   Uint32 n, virtualBlockNr, markedAlready, lbn;
        Uint16 prn;
        /* Translate virtual address to logical address in
         * physical partition. We are prepared here to the fact
         * that a virtual extent may be more than 1 block.
         * Translate virtual address to logical address for
         * each block and mark each block using recursion.
         * No markPartitionAllocation() for unused virtual
         * addresses. For inspectOnly, be silent about these
         * unused virtual addresses.
         * Count nonVirtualMarkedAlready in underlying partition
         * for later compare to those found in virtual partition.
         */
        for( n = 0, virtualBlockNr = logicalBlockNr;
             n < nrOfBlocks;
             n++,   virtualBlockNr++ )
        {   if( translateVirtualAddress( pmi, virtualBlockNr,
                                        &lbn, &prn, inspectOnly) ) /* silent */
            { /* translation ok, mark in underlying partition
               * markedAlready will become 0 or 1.
               */
              if( !markPartitionAllocation( mc, prn, lbn, 1,
                                           &markedAlready, inspectOnly) )
              { return FALSE;
              }
              nonVirtualMarkedAlready += markedAlready;
            }
        }
        /* continue to mark blocks in virtual partition
         */
    }
    actualPartitionLength = pmi->actualPartitionLength;

    /* Check whether it is the first call for this partition or not
     * If it is the first call then we have to initialize
     * partition allocation bit vector
     */
    if( pmi->pPartAlloc == NULL )
    {   /* partition allocation initialization
         * Initially set all bits to unallocated (ONE)
         * to be comparable with SBD.
         */
        if( inspectOnly )   /* avoid allocation */
        {   return TRUE;    /* pNrOfBlocksMarkedAlready == 0 */
        }
        vecUnits = ROUNDUPELEMENTS( actualPartitionLength,
                                    8 * sizeof(BitVector) );
        if( !allocateBitMap((BitVector**) &pmi->pPartAlloc,
                            vecUnits, 0xFF) )
        { pmi->partAllocOutOfMem = TRUE;
          return FALSE;     /* NO fatal uctExit(...) */
        }
        /** pmi->partAllocOutOfMem == FALSE by default **/
    } /* if(pmi->pPartAlloc == NULL) */

    /* Ready to mark bits in BitVector array
     */
    blocksMarkedAlready = 0;    /* init for count */
    for(i = 0; i < nrOfBlocks; i++)
    {
        if( (logicalBlockNr + i) >= actualPartitionLength )
        {   /* logical block number is outside the partition */
            return FALSE;
        }
        if( !bitVectorGetElement(pmi->pPartAlloc, logicalBlockNr + i))
        {   blocksMarkedAlready++;
        }
        if( !inspectOnly )
        { bitVectorResetElement(pmi->pPartAlloc, logicalBlockNr + i);
        }
    }

    if(  pNrOfBlocksMarkedAlready != NULL )
    {   *pNrOfBlocksMarkedAlready = blocksMarkedAlready;
    }
    return TRUE;
} /* extern bool markPartitionAllocation(UdfMountContext *mc,... */

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
extern bool verifyPartitionAllocation(UdfMountContext *mc)
{
    PartitionMapInfo  *pmi, *pmiCntp;   /* partition map info */
    PartitionSpaceSet *pPss;    /* partition Space Sets info  */
    Uint16      partRef;
    Uint32      actualPartitionLength, partitionStart,
                vectorLength, vecUnits, freeInPartAlloc = 0;
    BitVector  *fabricatedBitVector;

    for(partRef = 0;
        partRef < mc->vi->lvd->numberOfPartitionMaps;
        partRef++)
    {
        pmi     = &mc->partitionMapInfo[partRef];
        pmiCntp = &mc->partitionMapInfo[pmi->counterpartPRef];
        pPss    = &pmi->pss;
        actualPartitionLength = pmi->actualPartitionLength;
        partitionStart = pmi->pdPointer->partitionStartingLocation;
        pmi->spaceSetsFreeSpace = 0;
        pmi->usedUnallocatedBlocks = pmi->unusedAllocatedBlocks = 0;

        VERBOSE00(uctout,
          "\n ===>\t%8s Partition p%d:  size %lu blocks, %s\n",
                PMAPTYPE_TEXT(pmi->pMapType), partRef,
                actualPartitionLength,
                PDAT_TEXT(pmi->pdPointer->accessType));

        if(   pmi->pMapType == PMAPTYPE_VIRTUAL
           || pmi->pMapType == PMAPTYPE_METADATA )
        { VERBOSE00(uctout, "\t\t\t\tmapped on %s Partition p%d\n\n",
                    PMAPTYPE_TEXT(pmiCntp->pMapType),
                    pmi->counterpartPRef);
        }
        else
        { VERBOSE00(uctout, "\t\t\t\tblocks %7lu thru %7lu\n\n",
                    partitionStart,
                    partitionStart + actualPartitionLength - 1);
        }

        /* first check and maybe show partition bitmap as calculated
         * by the verifier, vector length == partition length
         */
        vecUnits = ROUNDUPELEMENTS(actualPartitionLength,
                                   8 * sizeof(BitVector));

        if( pmi->pPartAlloc == NULL )   /* no bitmap yet or out of memory */
        {
          if( !pmi->partAllocOutOfMem ) /* no block used in partition */
          { /* Initially set all bits to unallocated (ONE)
             * to be comparable with SBD.
             */
            if( !allocateBitMap((BitVector**) &pmi->pPartAlloc,
                                vecUnits, 0xFF) )
            { pmi->partAllocOutOfMem = TRUE;
            }
          }
          if( pmi->partAllocOutOfMem )
          { MLIMITbegin(WARN01level,uctMessageLimit);
              VERBOSE00(uctout,
                "\t  Warning: Unable to build partition p%u Bitmap\n",
                                partRef);
            MLIMITend;
            /* Free as promised:
             */
            checkFree((void**)&(pmi->pPartAlloc));
            checkFree((void**)&(pPss->fabricatedBitmap));
            continue;       /* next partition map */
          }
        }

        if( !pmi->partAllocOutOfMem )       /* bitmap ok */
        { /* vectorLength == actualPartitionLength for the moment
           * Clear tail, count unallocated blocks
           */
          bitVectorResetTail(pmi->pPartAlloc, actualPartitionLength);
          freeInPartAlloc =
                bitVectorCountBitsSet(pmi->pPartAlloc, vecUnits);

          /* check -showalloc option:
           * Shows lists of allocated and unallocated contiguous
           * areas for each partition.
           */
          if( uctDoShowAlloc )      /* -showalloc option*/
          {
            VERBOSE00(uctout,
              "  ==>\t-showalloc: Partition p%u,  allocated blocks: %8lu\n"
                                      "\t\t\t\t unallocated blocks: %8lu\n\n",
                partRef, actualPartitionLength - freeInPartAlloc,
                                                 freeInPartAlloc);
            /* note that printBlockNumbers() will print "sparse:"
             * instead of "  used:" for a (metadata) sparse extent.
             */
            printBlockNumbers(pmi->pPartAlloc, vecUnits, mc, partRef,
                              "unused:", "  used:", TRUE);  /* printBoth */
            VERBOSE00(uctout,
                   "\t----------- -showalloc: end of partition p%u\n\n",
                        partRef);
          }
        }

        /* Now Space Sets as read from the medium.
         * Merge all Space Set bitmaps together,
         * Length of fabricatedBitmap and calculated
         * pPartAlloc bitmap are equal to actualPartitionLength.
         * First determine bit length of Unallocated and
         * Freed Space Bitmaps.
         */
        vectorLength = actualPartitionLength;   /* for the moment */

        if( pPss->unallocatedSpaceBitmap != NULL )
        {   vectorLength = MIN(vectorLength,
                MIN(pPss->unallocatedSpaceBitmap->numberOfBits,
                8 * pPss->unallocatedSpaceBitmap->numberOfBytes));
        }
        if( pPss->freedSpaceBitmap != NULL )
        {   vectorLength = MIN(vectorLength,
                MIN(pPss->freedSpaceBitmap->numberOfBits,
                8 * pPss->freedSpaceBitmap->numberOfBytes));
        }
        vecUnits = ROUNDUPELEMENTS(vectorLength,
                                    8 * sizeof(BitVector));
        VERBOSE00(uctout,
            "  ==>\tCompare partition p%u calculated bitmap"
                                    " to %s\n",
                partRef, (pmi->pMapType == PMAPTYPE_VIRTUAL)
                            ? "VAT unused entries"
                            : "recorded Space Set");

        /* vectorLength may be less than actualPartitionLength now.
         * Merge all Space Set Bitmaps together,
         * Start with fabricatedBitmap result from
         * Unallocated and Freed Space Tables,
         * maybe NULL.
         */
        fabricatedBitVector = pPss->fabricatedBitmap;

        if( fabricatedBitVector != NULL )
        {   VERBOSE00(uctout,
              "\t  using: %s\n",
                (pmi->pMapType == PMAPTYPE_VIRTUAL)
                    ? "VAT fabricated bitmap"
                    : "merged Space Tables");
        }

        /* Merge Unallocated and fabricated Bitmap
         */
        if( pPss->unallocatedSpaceBitmap != NULL )
        {   if( fabricatedBitVector == NULL )
            {   fabricatedBitVector = (BitVector *)
                    &pPss->unallocatedSpaceBitmap->startOfBitmap;
                VERBOSE00(uctout,
                  "\t  using: Unallocated Space Bitmap\n");
            }
            else
            {   bitVectorMerge( fabricatedBitVector, (BitVector*)
                    &pPss->unallocatedSpaceBitmap->startOfBitmap,
                                vecUnits );
                VERBOSE00(uctout,
                  "\tmerging: Unallocated Space Bitmap\n");
            }
        }
        if( pPss->freedSpaceBitmap != NULL )
        {   if( fabricatedBitVector == NULL )
            {   fabricatedBitVector = (BitVector *)
                    &pPss->freedSpaceBitmap->startOfBitmap;
                VERBOSE00(uctout,
                  "\t  using: Freed Space Bitmap\n");
            }
            else
            {   bitVectorMerge( fabricatedBitVector, (BitVector*)
                    &pPss->freedSpaceBitmap->startOfBitmap,
                                vecUnits );
                VERBOSE00(uctout,
                  "\tmerging: Freed Space Bitmap\n");
            }
        }

        /* check fabricatedBitVector and prepair for compare
         */
        if(   fabricatedBitVector == NULL
           || pmi->pPartAlloc   == NULL )
        {   if( fabricatedBitVector == NULL )
            {   VERBOSE00(uctout,
                  "\t  No %sSpace Set found for partition p%u\n",
                  (pmi->pPartAlloc == NULL)
                    ? "calculated " : "", partRef);
            }
            /* no compare, skip to next partition
             * Free as promised:
             */
            checkFree((void**)&(pmi->pPartAlloc));
            checkFree((void**)&(pPss->fabricatedBitmap));
            continue;       /* next partition map */
        }
        bitVectorResetTail(fabricatedBitVector, vectorLength);
        pmi->spaceSetsFreeSpace =
                bitVectorCountBitsSet(fabricatedBitVector, vecUnits);

        /* check if fabricatedBitVector and pmi->pPartAlloc
         * have different length.
         * implementation NOTE: here we start change/destroy
         * the pmi->pPartAlloc bitvector.
         */
        if( vectorLength != actualPartitionLength )
        {   MLIMITbegin(ERROR00level,uctMessageLimit);
              fprintf(uctout,
                "\tError: Merged Space Sets number of bits: %lu,\n"
                                 "-\t\t\t\t       expected: %lu.\n"
                "-\tCaused by previous Space Bitmap NumberOfBits\n"
                "-\tor NumberOfBytes error. Assuming that missing\n"
                "-\tpart is equal to verifier calculated bitmap.\n",
                vectorLength, actualPartitionLength);
            MLIMITend;
            /* Assume missing part of fabricatedBitVector is
             * equal to that part of pPartAlloc.
             * Truncate pPartAlloc and make 'freeInSpaceSets'
             * correction.
             */
            bitVectorResetTail(pmi->pPartAlloc, vectorLength);
            pmi->spaceSetsFreeSpace += freeInPartAlloc -
                        bitVectorCountBitsSet(pmi->pPartAlloc,
                                              vecUnits);
        }

        /* Compare fabricatedBitVector to calculated BitVector
         * Both have length vectorLength and tail bits are reset
         * Used-but-unallocated blocks go to fabricatedBitVector
         * Unused-but-allocated blocks go to pmi->pPartAlloc
         */
        bitVectorCompare(fabricatedBitVector, pmi->pPartAlloc, vecUnits);
        pmi->usedUnallocatedBlocks =
                    bitVectorCountBitsSet(fabricatedBitVector, vecUnits);
        pmi->unusedAllocatedBlocks =
                    bitVectorCountBitsSet(pmi->pPartAlloc, vecUnits);

        if( pmi->usedUnallocatedBlocks == 0 )
        {   VERBOSE00(uctout,
              "\n\t  All used blocks marked as allocated\n");
        }
        else    /* used blocks marked as unallocated */
        {   MLIMITbegin(ERROR00level,uctMessageLimit);
              fprintf(uctout,
                "\n\t  Error: %lu used block%s marked as"
                                    " unallocated or freed\n",
                             pmi->usedUnallocatedBlocks ,
                    PLURAL_S(pmi->usedUnallocatedBlocks));
              printBlockNumbers(fabricatedBitVector, vecUnits, mc, partRef,
                                " ", NULL, FALSE);  /* NOT printBoth */
            MLIMITend;
        }

        if( pmi->unusedAllocatedBlocks == 0 )
        { VERBOSE00(uctout,
              "\n\t  No unused blocks marked as allocated.\n");
        }
        else
        { MLIMITbegin(WARN01level,uctMessageLimit);
            fprintf(uctout,
               "\n\t  Warning: %lu unused block%s NOT marked %s.\n"
                "-\t\t   This may be due to previous errors,\n",
                             pmi->unusedAllocatedBlocks,
                    PLURAL_S(pmi->unusedAllocatedBlocks),
                    (pmi->pMapType == PMAPTYPE_VIRTUAL)
                        ? "free in VAT table"
                        : "as unallocated");
            if( pmi->pMapType != PMAPTYPE_VIRTUAL )
            { if(   pmi->pdPointer->accessType == PDAT_READONLY
                 || pmi->pdPointer->accessType == PDAT_WRITEONCE )
              { fprintf(uctout,
                  "-\t\t   %s blocks that cannot be reused,\n",
                    PDAT_TEXT(pmi->pdPointer->accessType));
              }
              fprintf(uctout,
                "-\t\t   or otherwise it is Orphan Space, UDF 5.2.2.\n");
            }
            fprintf(uctout,
              "-\t  Usage of following extents not yet identified:\n");
            printBlockNumbers(pmi->pPartAlloc, vecUnits, mc, partRef,
                                " ", NULL, FALSE);  /* NOT printBoth */
          MLIMITend;
        }
        /* because the pmi->pPartAlloc bitmap has been destroyed,
         * it will be freed here as well as the merged Table bitmap.
         * Free as promised:
         */
        checkFree((void**)&(pmi->pPartAlloc));
        checkFree((void**)&(pPss->fabricatedBitmap));
    }   /* endfor(partRef, ...) */

    return TRUE;
}   /* end verifyPartitionAllocation() */


/*
 * Utility functions
 */
static Uint32 lw_offset(Uint32 number)
{
    return(number/(8 * sizeof(BitVector)));
}

static Uint32 bit_offset(Uint32 number)
{
    return((Uint32) (number%(8 * sizeof(BitVector))));
}

static void bitVectorSetElement(BitVector *vec, Uint32 number)
{
    vec[lw_offset(number)] |= (Uint32) 1 << bit_offset(number);
}

static void bitVectorResetElement(BitVector *vec, Uint32 number)
{
    vec[lw_offset(number)] &= ~((Uint32) 1 << bit_offset(number));
}

extern bool bitVectorGetElement(BitVector *vec, Uint32 number)
{
    return((bool)
        ((vec[lw_offset(number)] >> bit_offset(number)) & (Uint32)1));
}

static void bitVectorCompare(BitVector *vec1, BitVector* vec2,
                             Uint32     vecUnits)
/* bitwise-XOR vec1 and vec2, result to temp (unequal bits set).
 * bitwise-AND vec1 and temp, result to vec1.
 * bitwise-AND vec2 and temp, result to vec2.
 */
{
    Uint32 j;
    BitVector temp;

    for( j = 0; j < vecUnits; j++ )
    {   temp = (BitVector) ((*vec1) ^ (*vec2));
        (*vec1) &= temp;
        (*vec2) &= temp;
        vec1++; vec2++;
    }
}

static void bitVectorMerge(BitVector *vec1, BitVector* vec2,
                           Uint32     vecUnits)
{   Uint32 j;
    /* add one to another (OR), result to vec1
     */
    for(j = 0; j < vecUnits; j++)
    {   (*vec1) |= (*vec2);
        vec1++; vec2++;
    }
}

static void bitVectorResetTail(BitVector *vec, Uint32 vecLength)
{
    Uint32 j, vecUnits = ROUNDUPELEMENTS(vecLength,
                                         8 * sizeof(BitVector));
    for(j = vecUnits * 8 * sizeof(BitVector);
        j > vecLength; )
    {   bitVectorResetElement(vec, --j);
    }
}

static Uint32 bitVectorCountBitsSet(BitVector *vec, Uint32 vecUnits)
{
    Uint32 j, n1 = 0;
    BitVector tempBV;

    for(j = 0; j < vecUnits; j++)
    {
        tempBV = *vec;
        while(tempBV != 0)
        {
            n1 += tempBV & 0x1;
            tempBV >>= 1;
        };
        vec++;
    }

    return n1;
}

/* Print block numbers in absolute contiguous ranges,
 * one line per range.
 *
 * Exception for sparse extents:
 *  If absStart is LBA_SPARSE_EXTENT and txt is "  used:",
 *  then print "sparse:" instead of txt.
 */
static void printBlockNumbersRange(Uint32 absStart,
                                   Uint32 logStart,
                                   Uint32 len,
                                   char  *txt,
                                   bool   isVirtual)
{
    if( absStart == LBA_SPARSE_EXTENT ) /* special value */
    {   VERBOSE00(uctout, "    ***");
        if( strcmp(txt, "  used:") == 0 )
        {   txt = "sparse:";
        }
    }
    else
    {   VERBOSE00(uctout, "%7lu", absStart);
    }
    VERBOSE00(uctout, "\t%s %s block %8lu", txt,
                (isVirtual) ? "virtual"
                            : "logical", logStart);
    if( len > 1 )
    {   VERBOSE00(uctout, " thru %8lu, %7lu blocks",
                  logStart + len - 1, len);
    }
    VERBOSE00(uctout, "\n");

}   /* end printBlockNumbersRange() */

/* Print block numbers in absolute contiguous ranges,
 * one line per range.
 * if( printBoth == FALSE )
 * then only the ranges where bits are set (ONE)
 *      are printed using txt1
 * else also the ranges with bits reset (ZERO)
 *      are printed using txt2.
 * Exception:
 *  printBlockNumbers() will print "sparse:" instead
 *  of "  used:" for a (metadata) sparse extent.
 */
static void printBlockNumbers(BitVector *vec, Uint32 vecUnits,
                              UdfMountContext *mc, Uint16 partRefNr,
                              char *txt1, char *txt2,
                              bool printBoth)
{
    Uint32 nxt, len = 0, absoluteBlockNr,
           pLen = mc->partitionMapInfo[partRefNr].actualPartitionLength;
    bool   isVirtual = (mc->partitionMapInfo[partRefNr].pMapType
                        == PMAPTYPE_VIRTUAL);
    char  *txt;

    for( nxt = 0; nxt < pLen; nxt += len)
    {   Uint32 maxRangeLen,
               startBit = nxt;
        len = getNextBitRange(vec, vecUnits, &startBit);
        /* note that startBit is normally updated
         * by getNextBitRange() !!
         */
        if( len == 0 )      /* no range of 'ONE' bits found */
        { startBit = pLen;  /* after end of partition */
        }
        if( printBoth && nxt < startBit )
        {
            txt = txt2;         /* 'printBoth' txt2 case */
            len = startBit - nxt;
        }
        else
        {   txt = txt1;         /* normal txt1 case */
            nxt = startBit;
        }
        if( len == 0 )
        { break;                /* done, break loop */
        }
        /* print message for logical contiguous block range
         * (start: nxt, length: len).
         * however:
         *  May be truncate to absolute contiguous block range
         *  because of possible spared (defective) packets on a
         *  sparable partition. Use translateAddress() to find
         *  absolute start address and max contiguous range length.
         * Implementation note:
         *  translateAddress() may return ok and a special value
         *  LBA_SPARSE_EXTENT for absoluteBlockNr. This special
         *  value will also be (mis)used in case of failure for
         *  a virtual extent.
         */
        if( !translateAddress( mc, partRefNr, nxt,
                              &absoluteBlockNr, &maxRangeLen,
                              isVirtual) )  /* silent if virtual */
        { /* failed */
          if( !isVirtual )
          { MLIMITbegin(ERROR00level, MLIMITdefault10);
              fprintf(uctout, "  ==>\ttranslateAddress() error: "
                            "for (%lu,%lu), please report.\n",
                            nxt, partRefNr);
            MLIMITend;
//          return;
          }
          /* translateAddress() failed, do not try to translate
           * rest of (maybe virtual) address range.
           */
//        maxRangeLen = 1;                      /** testing **/
          maxRangeLen = len;
          absoluteBlockNr = LBA_SPARSE_EXTENT;  /* special value */
        }
        len = MIN(len, maxRangeLen);            /* len != 0 */

        /* note that value LBA_SPARSE_EXTENT for absoluteBlockNr is
         * used for a metadata sparse extent, but also in case of
         * any translateAddress() failure.
         */
        printBlockNumbersRange(absoluteBlockNr, nxt, len, txt, isVirtual);

    }   /* endfor */

}   /* end printBlockNumbers() */


/* Find next range of bits that are all set to ONE. Start searching on
 * bit position *bitNr, and then change *bitNr to the start position
 * of range.
 * return value: Length of the range.
 */
extern Uint32 getNextBitRange(BitVector *vec,
                              Uint32     vecUnits,
                              Uint32    *bitNr)
{
    Uint32    startBit = *bitNr;
    Uint32    lBitNr   = startBit;
    Uint32    rangeLen = 0;
    BitVector *tempBV  = vec;

    while( lBitNr < (vecUnits * 8 * sizeof(BitVector)) )
    {
        if( bitVectorGetElement(tempBV, lBitNr) )
        { /* may be we found a range */
            if(rangeLen == 0)
            {
                startBit = lBitNr;
            }
            rangeLen++;
        }
        else if( rangeLen ) /* end of range ? */
        {   break;          /* yes, ready to return */
        }
        lBitNr++;
    }
    *bitNr = startBit;
    return rangeLen;
}

