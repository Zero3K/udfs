/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctgeneral.c
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "general.h"
#include "uctverify.h"
#include "uctgeneral.h"
#include "uctstatus.h"
#include "uctfiles.h"
#include "uctendian.h"
#include "uctallocation.h"
#include "uctnodes.h"


/* some static system asserts, etc.
 */
extern bool uctInitialize()
{   bool result = TRUE;

    /* assert that MIN_UDFREVISION and MAX_UDFREVISION
     * are both 'known UDF revisions'.
     */
    if(   !verifyUdfRevision(MIN_UDFREVISION,  MIN_UDFREVISION,
                             MAX_UDFREVISION, "MIN_UDFREVISION", NULL)
       || !verifyUdfRevision(MAX_UDFREVISION,  MIN_UDFREVISION,
                             MAX_UDFREVISION, "MAX_UDFREVISION", NULL) )
    { fprintf(uctout,
          "==> Serious program error for MIN/MAX UDF revision range"
                                            " 0x%x thru 0x%x\n"
          "    file: %s, line: %u\n",
            MIN_UDFREVISION, MAX_UDFREVISION, __FILE__,__LINE__);
        result = FALSE;
    }
    udfStructStaticAsserts();   /* no return if any assert fails */
    return result;
}

/********** Volume Recognition stuff ********************/

/* Volume and boot block recognition *****************************************
 * ECMA-167 2/9.
 * Read and check the Volume Recognition Sequence (VRS).
 * A VRS contains Volume Structure Descriptors (VSD) that have a FIXED size
 * of 2048 bytes and always start at the begin of a sector !!
 * (also if sectorsize > 2048) !! For details, see ECMA-167 2/9.
 * Do not mix up the acronyms VRS and VSD with VDS (Volume Descriptor Sequence).
 */
extern bool readVolumeRecognitionSequence(Device *device,
                                          Uint32 verifySessionStart)
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32      blockSize = vmi->blockSize;
    Byte       *readbuffer = NULL;
    bool        ready, result;
    VsdType     vsdType, prevVsdType;
    int         BEA_TEA_balance = 0,
                cntTotal, cntBEA01, cntTEA01, cntNSR02, cntNSR03;
    Uint32      sectNumb, blocksPerVSD;

    VERBOSE00(uctout, "\tRead Volume Recognition Sequence");
    if( verifySessionStart != 0 )
    {      VERBOSE00(uctout, ", S = %lu\n", verifySessionStart); }
    else { VERBOSE00(uctout, "\n"); }

    UCTASSERT( VRS_VSD_SIZE == sizeof(GenericVolumeStructureDescriptor));

    blocksPerVSD = ROUNDUPELEMENTS(VRS_VSD_SIZE, blockSize);

    /* calculate first block address of VRS, ECMA 2/8.3
     */
    sectNumb = verifySessionStart
             + ROUNDUPELEMENTS(MIN_BYTES_BEFORE_VRS, blockSize);

    if( (readbuffer = (Byte*) tst_malloc(blocksPerVSD * blockSize,
                                         __FILE__,__LINE__)) == NULL )
    {
        uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }

    vsdType = prevVsdType = vsdTYPE_UNKNOWN;
    cntTotal = cntBEA01 = cntTEA01 = cntNSR02 = cntNSR03 = 0;

    for( ready = FALSE; ready == FALSE; sectNumb += blocksPerVSD )
    {
        prevVsdType = vsdType;
        if( deviceReadBlock(device, sectNumb,
                            blocksPerVSD, readbuffer,
                            FALSE)      /* NOT allowBelowVRS */
            != blocksPerVSD )
        {
            vsdType = vsdTYPE_UNKNOWN;  /* read error */
            /** TODO: assume unrecorded block ?? **/
        }
        else    /* find VSD type */
        {
            vsdType = verifyVolRecVSD((GenericVolumeStructureDescriptor *) readbuffer);
            /* If VSD type unknown and VRS recorded, then check if blank sector
             * termination according to UDF 2.01+ 2.1.7. In most cases, the exact
             * UDF revision is not yet known, but only the range given by the type
             * of NSR descriptor (so specify -udf <rev>).
             * Check for blank sector if not being blank may be an error.
             * Skip the test if no VRS is yet recorded maybe because of
             * intermediate state (VRS missing will be reported).
             */
            /** TODO: VRS Volume allocation check done ??,
             **       one sector extra for unrecorded or blank sector.
             **       (mind that cntTotal does not count sectors !!
             **        maybe multiple sectors per VSD !!!)
             **/
            if(    vsdType == vsdTYPE_UNKNOWN           /* end of VRS   */
               &&  cntTotal > 0                         /* VRS recorded */
               &&  getUctMaxUdfRevision() >= 0x201      /* at the safe side */
               && !verifyZeros(readbuffer, blockSize, NULL, NULL, NULL) )
            {
              warnIfUdfRevisionUncertain(0x200, 0x201); /* outside MLIMIT... !! */
              MLIMITbegin(ERROR00level, uctMessageLimit);
                fprintf(uctout,
                  "\t  Error: Blank or unrecorded sector expected"
                                        " after VRS, UDF 2.1.7%s.\n%s",
                   (getUctMinUdfRevision() < 0x250)
                     ? "\n-\t\t (improved text in UDF 2.50+)"   : "",
                   (getUctMinUdfRevision() < 0x201)
                     ?   "-\t\t This is an error for UDF"
                                " revison 2.01 and higher.\n" : "");
              MLIMITend;
            }
        }
        /* count all known VSR VSD descriptors
         */
        if( vsdType != vsdTYPE_UNKNOWN )
        {
            cntTotal++;
        }
        switch( vsdType )
        {
        case vsdTYPE_UNKNOWN:       /* read error or unknown descriptor */
            ready = TRUE;
            break;
        case vsdTYPE_BEA01:
            if( cntBEA01 == 0 )     /* first BEA01 */
            {
                if( cntTEA01 != 0 )
                { MLIMITbegin(WARN01level,uctMessageLimit);
                    fprintf(uctout, "\tWarning: %lu times %s before first %s\n",
                        cntTEA01, VRS_TEA01, VRS_BEA01);
                  MLIMITend;
                }
                if( cntTotal != 1 )
                {
                    ifPRINTinfo02(uctout, "\t%lu Volume Structure Descriptors found"
                                " before first %s\n", cntTotal-1, VRS_BEA01);
                    ENDif;
                }
                VERBOSE00(uctout, "\tStart of Extended Area\n");
                BEA_TEA_balance = 0;
            }
            else                    /* not first BEA01 */
            {
                if( prevVsdType != vsdTYPE_TEA01 )
                { MLIMITbegin(WARN01level,uctMessageLimit);
                    fprintf(uctout, "\tWarning: %s not preceded by %s\n",
                                    VRS_BEA01, VRS_TEA01);
                  MLIMITend;
                }
            }

            if( BEA_TEA_balance != 0 )
            {   MLIMITbegin(WARN01level,uctMessageLimit);
                 fprintf(uctout, "\tWarning: %s / %s unbalance\n",
                                    VRS_BEA01, VRS_TEA01);
                MLIMITend;
            }
            BEA_TEA_balance = 1;
            cntBEA01++;
            break;
        case vsdTYPE_TEA01:
            if(    cntBEA01 != 0            /* within Extended Area */
                && BEA_TEA_balance != 1
              )
            {   MLIMITbegin(WARN01level,uctMessageLimit);
                  fprintf(uctout, "\tWarning: %s / %s unbalance\n",
                                    VRS_BEA01, VRS_TEA01);
                MLIMITend;
            }
            BEA_TEA_balance = 0;
            cntTEA01++;
            break;
        case vsdTYPE_NSR02:
            cntNSR02++;
            notifyUdfNsrFound();
            modifyUdfRevisionRange( MIN_UDFREVISION, 0x150,
                                    "NSR02 descriptor" );
            break;
        case vsdTYPE_NSR03:
            cntNSR03++;
            notifyUdfNsrFound();
            modifyUdfRevisionRange( 0x200, MAX_UDFREVISION,
                                    "NSR03 descriptor" );
            break;
        default:        /* no action (keep compiler happy) */
            break;
        }
    }       /* endfor */

    if( cntBEA01 != 0 )
    {   VERBOSE00(uctout,"\tEnd of Extended Area\n");
    }
    VERBOSE00(uctout,    "\tEnd of Volume Recognition Sequence\n\n");

    result = TRUE;          /* check results */

    /* Check VRS, Clarification about VRS added
     * in UDF 2.01 2.1.7, improved in UDF 2.50.
     */
    if( cntBEA01 == 0 )     /* no Extended Area */
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError: %s Volume Recognition Sequence,\n",
                    (cntTotal==0) ? "Empty" : "No Extended Area in");
          fprintf(uctout,
            "-\t       UDF section 2 Basic Restrictions ...\n"
            "-\t       For clarification see UDF 2.50+ 2.1.7.\n"
            "-\t Note: A VRS may not yet be recorded for an\n"
            "-\t       intermediate state sequential file system.\n");
        MLIMITend;
        result = FALSE;
    }
    else if( prevVsdType != vsdTYPE_TEA01 ) /* last valid descriptor read */
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError: End of Extended Area was no %s\n"
            "-\t       Volume Recognition Sequence not properly closed\n",
                                            VRS_TEA01);
        MLIMITend;
        result = FALSE;
    }

    /* test NSR descriptors. Clarification about VRS added
     * in UDF 2.01 2.1.7, improved in UDF 2.50.
     */
    if( cntNSR02 != 0 && cntNSR03 != 0 )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout, "\tError: %lu %s and %lu %s descriptors found\n",
                            cntNSR02, VRS_NSR02, cntNSR03, VRS_NSR03);
        MLIMITend;
        result = FALSE;
    }
    else if( (cntNSR02 + cntNSR03) == 0 )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError: NSR descriptor missing, ECMA 2/9.1.2, 3/3.1\n"
            "-\t       and 3/9.1. For clarification UDF 2.50 2.1.7.\n");
        MLIMITend;
        result = FALSE;
    }
    free(readbuffer);
    VERBOSE00(uctout, "\n");

    return result;

}   /* end readVolumeRecognitionSequence() */


/* calculateOverlap(),
 * check if two extents are overlapping and return
 * start and length of overlapping part.
 *
 * output values in:
 * (*pLenOverlap)   : length in blocks of overlapping part,
 *                    zero if no overlap.
 * (*pStartOverlap) : start location of overlapping part,
 *                    undefined if (*pLenOverlap) == 0.
 * pLenOverlap and pStartOverlap may be NULL;
 *
 * return value: TRUE if overlap, else FALSE;
 */
extern bool calculateOverlap(
                    Uint32  start1,         Uint32  blockLen1,
                    Uint32  start2,         Uint32  blockLen2,
                    Uint32 *pStartOverlap,  Uint32 *pLenOverlap)
{
    Uint32 maxStart = MAX(start1, start2);
    Uint32 minEnd   = MIN(start1 + blockLen1,   /* mind: end+1 */
                          start2 + blockLen2);

    if( minEnd <= maxStart )    /* no overlap */
    {   minEnd = maxStart;
    }
    /* minEnd >= maxStart
     */
    if( pStartOverlap != NULL ) *pStartOverlap = maxStart;
    if( pLenOverlap   != NULL ) *pLenOverlap   = minEnd - maxStart;
    return (minEnd != maxStart);
}

/***
 * functions that help us reading descriptors from the disc
 ***/

/* translateVirtualAddress() translates a virtual address in a
 * virtual partition into a logical address and a physical
 * partition.
 *
 * return value: TRUE if translation ok,
 *         else: FALSE
 */
extern bool translateVirtualAddress(PartitionMapInfo *virtualPmi,
                                    Uint32            virtualAddress,
                                    Uint32           *pLogicalAddress,
                                    Uint16           *pPhysPRef,
                                    bool              silent)
{
    VirtualRecord *v = &virtualPmi->vatRec;
    Uint32  logicalAddress;         /* in counterpart partition */

    if( v == NULL || v->vatFile == NULL )
    {   return FALSE;
    }

    if( virtualAddress >= v->numberOfEntries )
    { if( !silent )
      { MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout, "\ttranslateVirtualAddress error: "
                "Virtual address %lu beyond VAT table size (%lu)\n",
                    virtualAddress, v->numberOfEntries);
        MLIMITend;
      }
      return FALSE;
    }
    logicalAddress = (v->vatTable)[virtualAddress];

    if( logicalAddress == VAT_UNUSED_ENTRY )
    {
      if( !silent )
      { MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout, "\ttranslateVirtualAddress error:"
            " Virtual address %lu translates to unused block\n", virtualAddress);
        MLIMITend;
      }
      return FALSE;
    }

    *pPhysPRef       = virtualPmi->counterpartPRef;
    *pLogicalAddress = logicalAddress;
    return TRUE;

}   /* end translateVirtualAddress() */

/* translateMetadataAddress() translates an address in a metadata
 * partition into a logical address and a physical/sparable partition.
 *
 * return value: TRUE if translation ok,
 *         else: FALSE
 *
 * Implementation notes:
 *  Only ShortAd is allowed, but this will be tested elsewhere.
 *  Here we take care that also LongAd works in case the LongAd
 *  references the right non-metadata partition, otherwise an
 *  infinite recursion could occur.
 * Exception:
 *  If address falls in sparse metadata extent, then the
 *  special value of LBA_SPARSE_EXTENT is returned for
 *  (*pLogicalAddress) and ((Uint16)-1) for (*pTargetPref).
 *  Note that (*pContiguousBlocks) is still valid in this case!!
 */
static bool translateMetadataAddress(PartitionMapInfo *metaPmi,
                                     Uint32            metadataAddress,
                                     Uint16            metadataPref,
                                     Uint32           *pLogicalAddress,
                                     Uint16           *pTargetPref,
                                     Uint32           *pContiguousBlocks,
                                     bool              silent)
{
    const MediumInfo    *vmi = getTheMediumInfo();
    MetadataRecord      *mRec = &metaPmi->metadataRec;
    UdfAllocationList   *metadataAl = mRec->pMetadataFileAllocList;
    UdfAllocationItem   *item;
    Uint32               totalBlocks, firstBlockMetaAddress;

    /* avoid recursion
     */
    UCTASSERT( metadataPref != metaPmi->counterpartPRef );

    firstBlockMetaAddress = totalBlocks = 0;
    for( item  = metadataAl->head;
         item != NULL;
         item  = item->next )
    {
        /* totalblocks holds metadata addres of first block
         * in this extent
         */
        firstBlockMetaAddress = totalBlocks;
        totalBlocks += ROUNDUPELEMENTS( adGetExtentSize(&item->aad.anyAd),
                                        vmi->blockSize );
        if( totalBlocks > metadataAddress )
        {   break;  /* done, metadataAddress falls in this extent */
        }
    }

    /* check loop termination (address out of range)
     * or break after: totalBlocks > metadataAddress
     */
    if( item == NULL )
    { if( !silent )
      { MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout, "\ttranslateMetadataAddress error: "
                "Metadata address %lu beyond\n"
                "-\t\tMetadata Partition (%lu blocks).\n",
                    metadataAddress, totalBlocks);
        MLIMITend;
      }
      return FALSE;
    }

    /* get logical address of extent first block
     */
    if( !udfGetLocation(&item->aad, metadataAl->itemAdType,
                         metaPmi->counterpartPRef,
                         pTargetPref, pLogicalAddress) )
    { if( !silent )
      { MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout, "\ttranslateMetadataAddress error: "
                "udfGetLocation error, please report: (%lu)\n",
                        metadataAl->itemAdType);
        MLIMITend;
      }
      return FALSE;
    }

    /* Correct (*pLogicalAddress) for offset position within extent.
     * (*pContiguousblocks) is the remainder of the extent,
     * including (*pLogicalAddress).
     */
    (*pLogicalAddress) += metadataAddress - firstBlockMetaAddress;
    (*pContiguousBlocks) = totalBlocks - metadataAddress;

    /* exception: sparse metadata extents.
     * In this case, a special value will be returned for
     * (*pLogicalAddress) and (*pTargetPref).
     */
    if( adGetExtentType(&item->aad.anyAd) == ADEL_NOT_RECORDED_NOT_ALLOCATED )
    { /* address in sparse metadata extent
       * Note that (*pContiguousBlocks) is valid !!
       */
      (*pLogicalAddress) = LBA_SPARSE_EXTENT;
      (*pTargetPref) = (Uint16) -1;     /* no target partition */
      return TRUE;
    }

    /* final test on part ref number in order to avoid recursion.
     * (obvious for ShortAd, but maybe a problem with LongAd)
     * LongAd is not allowed for metadata partition, but we want
     * that it still works with a correct part ref number.
     */
    if( (*pTargetPref) != metaPmi->counterpartPRef )
    { if( !silent )
      { MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout, "\ttranslateMetadataAddress error: "
                "Partition reference mismatch, please\n"
            "-\t\treport: (%lu, %lu).\n",
                        *pTargetPref, metaPmi->counterpartPRef);
        MLIMITend;
      }
      return FALSE;
    }

    return TRUE;    /* result ok */

}   /* end translateMetadataAddress() */

/* Translate from a logical address to an absolute address
 * for a sparable partition.
 * A second result is the number of contiguous blocks that can be
 * read without the need for an intermediate address translation
 * as performed by this function.
 */
static bool translateSparableAddress( PartitionMapInfo *pmi,
                                      Uint32            logicalAddress,
                                      Uint32           *pAbsoluteAddress,
                                      Uint32           *pContiguousBlocks,
                                      bool              silent)
{
    Uint32            spPacketLength, packetAddr,
                      packetOffset, noSparingAddr;
    Uint16            n, nmbOfEnt;
    SparablePartitionMapTail *sTail;
    SparingEntry             *spMapEnt;
    SparingTable             *st = pmi->pSparingTable;

    UCTASSERT( st != NULL && pmi->pMapType == PMAPTYPE_SPARABLE );

    /* Get sparing packet length from sparable partition map.
     * (nmb of logical blocks per sparing packet).
     * Normally, this is the (ECC) blocking factor,
     * however, for UDF 2.00-, this is a fixed value of 32,
     * which should be a multiple of the (ECC) blocking factor.
     */
    sTail = &pmi->pPartitionMap->type2PartitionMap.SharedTail.sparableTail;
    spPacketLength = sTail->packetLength;

    UCTASSERT( spPacketLength != 0 );

    packetOffset = logicalAddress % spPacketLength;
    packetAddr   = logicalAddress - packetOffset;

    UCTASSERT(       packetAddr < 0xFFFFFFF0
              && logicalAddress < pmi->pdPointer->partitionLength );

    /* abs address in case of no sparing
     */
    noSparingAddr = logicalAddress + pmi->pdPointer->partitionStartingLocation;

    /* See if packet is in Sparing Table and must be remapped.
     * Note that implementation here relies on sorting of the
     * Sparing Map Entries on originalLocation, as required
     * by UDF 2.2.12 (was 2.2.11), and that Sparing Map Entries have
     * originalLocation and mappedLocation values that are
     * a multiple of the sparing packet length.
     * This means that these conditions have to be checked
     * when the Sparing Table is read.
     */
    nmbOfEnt = st->reallocationTableLength;
    spMapEnt = (SparingEntry *) &st->startOfMapEntries;

    for( n = 0;
         n < nmbOfEnt && packetAddr > spMapEnt->originalLocation;
         n++, spMapEnt++ )
    {}

    /* n == nmbOfEnt || packetAddr <= spMapEnt->originalLocation
     * if n == nmbOfEnt, then: spMapEnt points outside mapping table
     */

    if( n < nmbOfEnt && packetAddr == spMapEnt->originalLocation )
    {
        /* logicalAddress in defective spared packet
         * address translation needed for next packet
         */
        *pAbsoluteAddress  = spMapEnt->mappedLocation + packetOffset;
        *pContiguousBlocks = spPacketLength - packetOffset;

        if( !silent )
        { ifPRINTinfo02(uctout,
            "\tSparing defect remapping: %lu -> %lu\n",
                        noSparingAddr, *pAbsoluteAddress);
          ENDif;
        }
    }
    else    /* logicalAddress not in defective spared packet */
    {
        *pAbsoluteAddress = noSparingAddr;
        if(    n < nmbOfEnt
            && spMapEnt->originalLocation < 0xFFFFFFF0 )
        {                       /* higher defective packet in table */
            *pContiguousBlocks =
                    spMapEnt->originalLocation - logicalAddress;
        }
        else                    /* no more higher defective packets */
        {
            *pContiguousBlocks =
                    pmi->pdPointer->partitionLength - logicalAddress;
        }
    }
    return TRUE;

}   /* end translateSparableAddress() */

/* Translate logical address to absolute address.
 * A second result is the number of contiguous blocks that can be
 * read without the need for an intermediate address translation
 * as performed by this function.
 * (*pContiguousBlocks) is the number of physically contiguous blocks
 * that can be read without an intermediate call to translateAddress()
 * for each subsequent block.
 */
extern bool translateAddress(UdfMountContext *mc,
                             Uint16  originalPref,
                             Uint32  originalAddress,
                             Uint32 *pAbsoluteAddress,
                             Uint32 *pContiguousBlocks,
                             bool silent)
{
    PartitionMapInfo *originalPmi, *mappedPmi;
    Uint16            mappedPref;
    Uint32            mappedAddress, tmp32,
                      numberOfPartitionMaps;

    /* direct mapping for absolute addressing
     * (originalPref == -1)
     */
    if( originalPref == (Uint16) -1 )       /* no translation */
    {
        /* Be prepaired for: 0 <= originalAddress <= (MAX_UINT32-1).
         * So limit (*pContiguousBlocks) to MAX_UINT32 and
         * return FALSE if originalAddress == MAX_UINT32.
         */
        *pAbsoluteAddress = originalAddress;
        *pContiguousBlocks = MAX_UINT32 - originalAddress;
        return( originalAddress != MAX_UINT32 ) ;
    }
    UCTASSERT( mc != NULL && mc->vi != NULL && mc->vi->lvd != NULL );

    numberOfPartitionMaps = mc->vi->lvd->numberOfPartitionMaps;

    /* Original logical/virtual/metadata address in partition
     * Translate to absolute block address.
     * First test partition reference number and partition size.
     */
    if( ((Uint32) originalPref) >= numberOfPartitionMaps )
    { if( !silent )
      { MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\ttranslateAddress error: Partition Reference Number: %lu,\n"
            "-\t  expected: less than %u, because"
                        " only %lu partition map%s present.\n",
            originalPref, numberOfPartitionMaps,
            numberOfPartitionMaps, PLURAL_S(numberOfPartitionMaps));
        MLIMITend;
      }
      return FALSE;
    }
    originalPmi = &mc->partitionMapInfo[originalPref];

    /* Test logical addres on original partition size.
     */
    if( originalAddress >= originalPmi->actualPartitionLength )
    { if( !silent )
      { MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\ttranslateAddress error: original logical address p%u: %lu,\n"
            "-\t\texpected: less than %lu. Pointing outside partition.\n",
                    originalPref, originalAddress,
                    originalPmi->actualPartitionLength);
        MLIMITend;
      }
      return FALSE;
    }

    /* (*pContiguousBlocks) is never more than the remainder
     * of the original partition
     */
    (*pContiguousBlocks) = originalPmi->actualPartitionLength
                         - originalAddress;

    if( originalPmi->pMapType == PMAPTYPE_VIRTUAL )
    {   /* translate virtual address in virtual partition
         * into logical address in physical partition.
         */
        if( !translateVirtualAddress( originalPmi, originalAddress,
                                     &mappedAddress,
                                     &mappedPref, silent) )
        {   return FALSE;
        }
        mappedPmi = &mc->partitionMapInfo[mappedPref];
    }
    else if( originalPmi->pMapType == PMAPTYPE_METADATA )
    {   /* translate address in metadata partition
         * into logical address in physical/sparable partition.
         */
        if( !translateMetadataAddress( originalPmi,
                                 originalAddress, originalPref,
                                &mappedAddress, &mappedPref,
                                &tmp32, silent) )
        {   return FALSE;
        }
        (*pContiguousBlocks) = MIN(tmp32, *pContiguousBlocks);

        /* handle sparse metadata extent exception
         */
        if( mappedAddress == LBA_SPARSE_EXTENT )
        { (*pAbsoluteAddress) = LBA_SPARSE_EXTENT;
          return TRUE;      /* sparse metadata extent */
        }
        mappedPmi = &mc->partitionMapInfo[mappedPref];
    }
    else                /* no virtual or metadata address */
    {   mappedAddress = originalAddress;
        mappedPref   = originalPref;
        mappedPmi   = originalPmi;
    }

    /* Test logical addres on mapped partition size.
     */
    if( mappedAddress >= mappedPmi->actualPartitionLength )
    { if( !silent )
      { MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\ttranslateAddress error: mapped logical address p%u: %lu,\n"
            "-\t\texpected: less than %lu. Pointing outside partition.\n",
            mappedPref, mappedAddress, mappedPmi->actualPartitionLength);
          if( mappedPref != originalPref )
          { fprintf(uctout,
                "-\t\tOriginates from virtual or metadata address p%u: %lu\n",
                        originalPref, originalAddress);
          }
        MLIMITend;
      }
      return FALSE;
    }

    /* mappedPmi points to physical or sparable partition info.
     * originalPmi points to original partition info.
     * further: see also (*pContiguousBlocks) correction later.
     */
    switch ( mappedPmi->pMapType )
    {
    case PMAPTYPE_PHYSICAL:
        *pAbsoluteAddress =
            mappedAddress + mappedPmi->pdPointer->partitionStartingLocation;
        *pContiguousBlocks = MIN(*pContiguousBlocks,
                    mappedPmi->pdPointer->partitionLength - mappedAddress);
        break;
    case PMAPTYPE_SPARABLE:
        if( !translateSparableAddress(mappedPmi, mappedAddress,
                    pAbsoluteAddress, &tmp32, silent) )
        {   return FALSE;
        }
        *pContiguousBlocks = MIN(tmp32, *pContiguousBlocks);
        break;
    default:        /* unknown maptype or virt part maps on virt part */
        MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,   /* print, also if silent == TRUE */
            "\nSerious translateAddress error: Unexpected PMAPTYPE: %lu,"
                    " please report\n", mappedPmi->pMapType);
        MLIMITend;
        return FALSE;
        /** break; **/
    }

    /* read block by block from a virtual partition.
     */
    if(  originalPmi->pMapType == PMAPTYPE_VIRTUAL )
    {   *pContiguousBlocks = 1;
    }
    return TRUE;

}   /* end translateAddress() */

/* readBlocksFromPartition():
 * returns actually correctly read nmb of blocks
 * fake read if buffer == NULL
 *
 * TODO: Correct for multi-partition case
 *       and SINGLE VOLUME CASE, but:
 *       Mult Vol case: Find the right DISC!
 */
 extern Uint32 readBlocksFromPartition( UdfMountContext *mc,
                                        Byte  *buffer,
                                        Uint16 partRefNr,
                                        Uint32 logicalBlockNr,
                                        Uint32 nrOfBlocks )
{
    PartitionMapInfo *pmi;
    const MediumInfo *vmi  = getTheMediumInfo();
    Uint32  blockSize = vmi->blockSize;
    Uint32  absoluteBlockNr = 0;
    Uint32  blocksToGo, blocksPerRead, blocksRead = 0;

    /* logicalBlockNr may be absolute media block address,
     * in that case: partRefNr == (Uint16) -1
     */
    pmi = NULL;
    if( partRefNr != (Uint16) -1 )
    {   if( partRefNr >= mc->vi->lvd->numberOfPartitionMaps )
        {   MLIMITbegin(ERROR00level,uctMessageLimit);
              fprintf(uctout,
                "\tError: Partition Reference Number out of range: %lu\n",
                    partRefNr);
            MLIMITend;
            return 0;           /* error, no blocks read */
        }
        pmi = &mc->partitionMapInfo[partRefNr];

        /* Further only packetLength test for sparable partition.
         * Verifier can handle virtual extents longer than
         * the blockSize (translateAddress for each block).
         */
        switch( pmi->pMapType )
        {
        case PMAPTYPE_SPARABLE:
            if( pmi->pPartitionMap->type2PartitionMap.SharedTail.sparableTail.packetLength
                == 0 )
            {   MLIMITbegin(ERROR00level,uctMessageLimit);
                  fprintf(uctout, "\treadBlocksFromPartition: "
                    "Serious error: Packet Length zero in Sparable Partition Map.\n");
                MLIMITend;
                return 0;       /* error, no blocks read */
            }
            break;
        default:        /* no action (keep compiler happy) */
            break;
        }
    }

    /* read policy: read as many contiguous blocks as possible at once,
     *              this number of blocks is returned by translateAddress().
     * NOTE: buffer may be NULL for fake read !!!
     */
    for( blocksToGo = nrOfBlocks;
         blocksToGo > 0;
         blocksToGo -= blocksPerRead,
         logicalBlockNr += blocksPerRead,   /* for next deviceReadBlock() */
         buffer = (buffer == NULL)
                    ? buffer
                    : buffer + (blocksPerRead * blockSize) )
    {

#define SWAP_METADATAPARTITION_ON_ERROR
#ifdef  SWAP_METADATAPARTITION_ON_ERROR
/* try to change to/from mirror if read error in metadata partition
 */
{     int retryCnt = 0,
          retryMax = 0;
      do                /* retry loop, if error, then try read mirror */
      { if(   partRefNr == mc->metadataPref
           && IS_PREF_PARTITION_FOUND(mc->metadataPref)
           && mc->metadataFile            != NULL
           && mc->metadataFile->node       != NULL
           && mc->metadataFile->node->al    != NULL
           && mc->metadataMirrorFile         != NULL
           && mc->metadataMirrorFile->node    != NULL
           && mc->metadataMirrorFile->node->al != NULL )
        {
          retryMax = 1;     /* allow one retry to switch to/from mirror */
          if( retryCnt != 0 )
          { VERBOSE00(uctout,
                "-\tMetadata Partition read error at %lu, retry using mirror\n",
                        absoluteBlockNr + blocksRead);
            /* determine if Metadata File or its mirror is in use
             * by translateMetadataAddress() and switch.
             */
            pmi->metadataRec.pMetadataFileAllocList =
                (    pmi->metadataRec.pMetadataFileAllocList
                  != mc->metadataFile->node->al )       /* MF ADs not in use  */
                    ? mc->metadataFile->node->al        /* switch to MF ADs    */
                    : mc->metadataMirrorFile->node->al; /* switch to mirror ADs */
          }
        }
#endif  /* SWAP_METADATAPARTITION_ON_ERROR */

        /* translateAddress will also test partRefNr, which may be
         * (Uint16) -1 for an absolute media block address.
         * Further it will calculate the contiguous blocks that can
         * be read without intermediate address translation
         * (e.g. in case of sparing defective blocks).
         */
        if( !translateAddress( mc, partRefNr, logicalBlockNr,
                              &absoluteBlockNr, &blocksPerRead,
                               FALSE) ) /* not silent, show reallocation */
        {   return nrOfBlocks - blocksToGo;     /* error */
        }
        blocksPerRead = MIN(blocksPerRead, blocksToGo);

        if( absoluteBlockNr == LBA_SPARSE_EXTENT )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout, "\tError: Attempt to read %lu "
                    "block%s from SPARSE %sextent (%lu,%lu)\n",
                        blocksPerRead, PLURAL_S(blocksPerRead),
                (partRefNr == mc->metadataPref) ? "metadata " : "",
                        logicalBlockNr, partRefNr);
          MLIMITend;
          return nrOfBlocks - blocksToGo;       /* error */
        }
        blocksRead    = deviceReadBlock(mc->device, absoluteBlockNr,
                                        blocksPerRead, buffer,
                                        FALSE); /* NOT allowBelowVRS */

#ifdef  SWAP_METADATAPARTITION_ON_ERROR
//testing begin
//      if( retryCnt < retryMax )
//      {   blocksRead--;   /** fake error to force retry **/
//      }
//testing end

      } while(    blocksRead != blocksPerRead   /* read error */
              && (retryCnt++) < retryMax );
}
#endif  /* SWAP_METADATAPARTITION_ON_ERROR */

        if(   blocksPerRead == 0
           || blocksRead    != blocksPerRead )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout, "-\treadBlocksFromPartition error: "
                "Error reading %lu block%s at %lu (%lu)\n",
                    blocksPerRead, PLURAL_S(blocksPerRead),
                    absoluteBlockNr, logicalBlockNr);
            if( blocksRead != blocksPerRead )
            { fprintf(uctout,
                "-\t\t\t\t       %s%lu block%s read correctly.\n",
                (blocksRead == 0) ? "" : "Only first ",
                 blocksRead, PLURAL_S(blocksRead));
            }
          MLIMITend;
          return nrOfBlocks - blocksToGo + blocksRead;  /* error */
        }
    }

    return nrOfBlocks - blocksToGo;     /* assert */

}   /* end readBlocksFromPartition() */


/* returns tidUNKNOWN in *pTagId if an error occurs before
 * the execution of swapAndVerifyDescriptor(), else the value
 * returned by swapAndVerifyDescriptor() in its *pTagId argument.
 */
static bool readDescriptor( UdfMountContext *mc,
                            Uint32 blockNr,       Uint16  partRefNr,
                            Uint32 nrBlocks,      Uint32 *pNrBlocksRead,
                            Uint16 expectedTagId, Uint16 *pTagId,
                            Byte **pMem, Node *node )
{
    const MediumInfo *vmi       = getTheMediumInfo();
    Uint32            blockSize = vmi->blockSize;
    Byte             *readbuffer, *tmp;
    Uint32            descriptorSize, blocksToRead;

    *pTagId = tidUNKNOWN;   /* if errors before swapAndVerifyDescriptor() */
    *pMem   = NULL;
    *pNrBlocksRead = 0;

    if( nrBlocks == 0 )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
            "\terror: Zero blocks available for %s desciptor read\n",
            (expectedTagId == tidUNKNOWN) ? "" : tidTEXT4(expectedTagId));
        MLIMITend;
        return FALSE;
    }

    /* Steps to be taken:
     * - allocate and read one block
     * - determine descriptor length
     * - reallocate and read more blocks if needed
     * - swap and verify descriptor
     */
    if( (readbuffer = (Byte*) tst_malloc(blockSize,
                                         __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    if( readBlocksFromPartition(mc, readbuffer, partRefNr, blockNr, 1)
        != 1 )
    {
        VERBOSE00(uctout, "-\treadDescriptor: "
            "Error reading logical block %lu\n", blockNr);
        free(readbuffer);
        return FALSE;
    }
    *pNrBlocksRead = 1;

    /* Check unswapped head of descriptor for recognition and
     * determination of descriptor length.
     */
    if( !inspectDescriptorHead(readbuffer, blockSize, blockSize, TRUE,
                               expectedTagId, pTagId, &descriptorSize) )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
            printAndClearUctErrorMessage("-");      /* error: ... */
            fprintf(uctout, "\treadDescriptor: inspect descriptor error\n");
        MLIMITend;
        free(readbuffer);
        return FALSE;
    }
    blocksToRead = ROUNDUPELEMENTS(descriptorSize, blockSize);

    if( blocksToRead > nrBlocks )
    {   MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
            "\tError: %s descriptor requires %lu blocks,"
                                    " available in extent: %lu\n",
                tidTEXT4(*pTagId), blocksToRead, nrBlocks);
        MLIMITend;
        free(readbuffer);   /** TODO: allow continue ?? **/
        return FALSE;       /** no real problem, because allocation is ok **/
                            /** but fatal error maybe better **/
    }
    else if(  (Uint64) blocksToRead * blockSize
            > (Uint64) UCT_MAXDESCRIPTOR_BYTESIZE )
    {   MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
             "\tError: %s descriptor requires %lu blocks. The verifier\n"
            "-\t       refuses to read more than %lu Mbyte for one descriptor.\n"
            "-\t=====> Please report if this still is a valid descriptor.\n",
                tidTEXT4(*pTagId), blocksToRead, UCT_MAXDESCRIPTOR_MBSIZE);
        MLIMITend;
        free(readbuffer);
        return FALSE;
    }

    if( blocksToRead != 1 )     /* read extra blocks, reallocate */
    {
        tmp = readbuffer;       /* save for reallocation */
        if( (readbuffer = (Byte*) tst_realloc(readbuffer,
                                              blocksToRead * blockSize,
                                            __FILE__,__LINE__)) == NULL )
        {   free(tmp);
            uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
        }

        if( readBlocksFromPartition(mc, readbuffer+blockSize, partRefNr,
                                    blockNr+1, blocksToRead-1)
            != (blocksToRead-1) )
        {
            VERBOSE00(uctout, "-\treadDescriptor: "
                "Error reading logical block %lu ...\n", blockNr+1);
            free(readbuffer);
            return FALSE;
        }
        *pNrBlocksRead = blocksToRead;
    }
    if( !swapAndVerifyDescriptor( readbuffer, blocksToRead * blockSize,
                                  expectedTagId, pTagId, blockNr,
                                  mc, node ) )
    {
        VERBOSE00(uctout, "-\tDescriptor error\n");
        free(readbuffer);
        return FALSE;
    }
    *pMem = readbuffer;
    return TRUE;

}   /* end readDescriptor() */

/***
 * Functions for handling allocation extent lists
 ***/

/* allocate space for new UdfAllocationList
 * and initialize.
 */
extern UdfAllocationList *createNewAllocationList(Uint8 adType)
{
    UdfAllocationList *newList;

    newList = (UdfAllocationList*) tst_calloc(sizeof(UdfAllocationList),
                    1, __FILE__,__LINE__);
    if( newList == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    /* all fields 0 or NULL, also overheadList;
     * set adType;
     */
    newList->itemAdType = adType;

    return newList;
}


#ifdef UCT_NOTUSED

/* alDetermineTotalAllocatedSpace():
 * ADEL_NOT_RECORDED_NOT_ALLOCATED extents are NOT counted
 * overheadList Ads, like ADEL_EXTENTPOINTER extents ARE counted.
 */
static void alDetermineTotalAllocatedSpace(UdfAllocationList *al,
                                           Uint64 *totalAllocatedSpace)
{
    const MediumInfo  *vmi = getTheMediumInfo();
    UdfAllocationItem *ai;
    Uint64             overheadSpace;

    *totalAllocatedSpace = 0;   /* before return */
    if( al == NULL ) return;

    for( ai  = al->head;
         ai != NULL;
         ai  = ai->next )
    {   Uint32 exl = ai->aad.anyAd.extentLength;
        if( elGetExtentType(exl) != ADEL_NOT_RECORDED_NOT_ALLOCATED )
        {   *totalAllocatedSpace +=
                (Uint64) ROUNDUPMULT(elGetExtentSize(exl), vmi->blockSize);
        }
    }
    /* recursion, end at first al->overheadList == NULL
     */
    alDetermineTotalAllocatedSpace(al->overheadList, &overheadSpace);
    *totalAllocatedSpace += overheadSpace;
}

#endif  /* UCT_NOTUSED */


static void allocationListFreeHeadList(UdfAllocationList *al)
{
    if( al )
    {   while( al->head )
        {   UdfAllocationItem *old = al->head;
            al->head = al->head->next;
            free(old);
        }
        al->tail = NULL;    /* and all->head == NULL */
    }
}

static void allocationListFreeContents(UdfAllocationList *al)
{
    if( al )
    {   allocationListFreeHeadList(al);
        allocationListFreeContents(al->overheadList); /* recursion */
        checkFree((void**) &al->overheadList);        /* NULL */
    }
}

/* allocationListFree():
 * free UdfAllocationList and set (*pAl) = NULL.
 */
extern void allocationListFree(UdfAllocationList **pAl)
{
    if( pAl != NULL && (*pAl) != NULL )
    {   allocationListFreeContents(*pAl);
        checkFree((void**)pAl);       /* NULL */
    }
}

extern void nodeFreeAllocationLists(Node *node)
{
    if( node != NULL && node->al )
    {   allocationListFreeContents(node->al); /* overheadList too */
        checkFree((void**) &node->al);        /* NULL */
    }
}

/* alAddDescriptor():
 * al must exist and adType must be consistent with
 * one used at createNewAllocationList() call.
 * adType of overheadlist is equal to that of main list.
 */
extern bool alAddDescriptor(UdfAllocationList *al,
                            AnyAllocationDescriptor *aad,
                            bool  toOverheadList)
{
    UdfAllocationItem *pNewItem =
        (UdfAllocationItem *) tst_calloc( sizeof(UdfAllocationItem),
                                          1, __FILE__, __LINE__);
    UCTASSERT( al != NULL );

    if( pNewItem == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    /* fill the freshly created structure
     */
    switch( al->itemAdType )
    {
    case ADT_SHORT:
        pNewItem->aad.shortAd = aad->shortAd;       /* copy sizeof(ShortAd) */
        break;
    case ADT_LONG:
        pNewItem->aad.longAd  = aad->longAd;        /* copy sizeof(LongAd) */
        break;
    case ADT_EXTENT:
        pNewItem->aad.extentAd  = aad->extentAd;    /* copy sizeof(ExtentAd) */
        break;
    default:
        return FALSE;
        /** break; **/
    }
    pNewItem->next = NULL;

    /* Place in UdfAllocationList al
     * If type is ADEL_EXTENTPOINTER,
     * then place in al->overheadList branch
     */
    if( toOverheadList )
    {   if(    al->overheadList == NULL     /* NULL, create one */
           && (al->overheadList = createNewAllocationList(al->itemAdType))
                == NULL )
        {   return FALSE;   /* could not create overheadList */
        }
        /* use overheadList to add pNewItem
         */
        al = al->overheadList;
    }

    /* mind that al may point to overheadList
     * for ADEL_EXTENTPOINTER, etc overhead.
     */
    if( al->head == NULL )          /* first one */
    {   al->head = al->tail = pNewItem;
    }
    else
    {   al->tail->next = pNewItem;      /* in this */
        al->tail       = pNewItem;      /*  order  */
    }
    return TRUE;
}


/* addToContExtentList:
 * add new ContExtentItem at tail of continuation extent list
 * Used for continuation extents of descriptor sequences.
 * The sequenceId denotes the specific sequence,
 * see the ContExtentItem definition.
 */
static bool addToContExtentList( ContExtentItem         **pHead,
                                 AnyAllocationDescriptor *aad,
                                 Uint8                    adType,
                                 Uint8                    sequenceId )
{
    ContExtentItem *pNewItem, *item;

    pNewItem = (ContExtentItem *) tst_calloc( sizeof(ContExtentItem),
                                              1, __FILE__, __LINE__);
    if( pNewItem == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    switch( adType )
    {
    case ADT_SHORT:
        pNewItem->aad.shortAd  = aad->shortAd;  /* copy sizeof(ShortAd) */
        break;
    case ADT_LONG:
        pNewItem->aad.longAd   = aad->longAd;   /* copy sizeof(LongAd) */
        break;
    case ADT_EXTENT:
        pNewItem->aad.extentAd = aad->extentAd; /* copy sizeof(ExtentAd) */
        break;
    default:
        return FALSE;
        /** break; **/
    }
    pNewItem->next     = NULL;
    pNewItem->adType    = adType;
    pNewItem->sequenceId = sequenceId;

    if( (item = (*pHead)) == NULL )
    {   (*pHead) = pNewItem;
    }
    else
    {   while( item->next != NULL )
        {   item = item->next;
        }
        item->next = pNewItem;
    }
    return TRUE;

}   /* end addToContExtentList() */

/* Find extent and offset in that extent where
 * bytePos is located.
 * All extents that are not a multiple of the block size
 * are rounded up to the next multiple of the block size.
 */
extern bool nodeFindAllocationDescriptor(Node* node, Uint64 bytePos,
                                         UdfAllocationItem **ai,
                                         Uint32 *offset)
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint64 currentBytePos, nextBytePos;

    for( currentBytePos = nextBytePos = 0,
         *ai  = node->al->head;
         *ai != NULL;
         *ai  = (*ai)->next )
    {
        nextBytePos +=
            (Uint64) ROUNDUPMULT( adGetExtentSize(&((*ai)->aad.anyAd)),
                                                  vmi->blockSize );
        if( nextBytePos > bytePos )
            break;
        currentBytePos = nextBytePos;
    }

    if( (*ai) == NULL )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
            "\tnodeFindAllocationDescriptor error: No more descriptors\n");
        MLIMITend;
        return FALSE;
    }
    *offset = (Uint32) (bytePos - currentBytePos);  /* less than one extent */
    return TRUE;
}

/* extractAllocationDescriptors()
 * Verify allocation descriptors included all extra extents
 * of Allocation Descriptors.
 * if node != NULL, the Allocation Descriptors are converted
 * into an UdfAllocationList structure in node->al.
 */
static bool extractAllocationDescriptors(UdfMountContext *mc, Byte *ADs,
                                         Uint32  adsLength, Uint8 adType,
                                         Uint32 *pNumberOfADs,
                                         Uint64 *pTotalRecordedBlocks,
                                         Uint16  shortAdPartRefNr,
                                         Node   *node,
                                         bool    inMetadataPartition)
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32 blockSize = vmi->blockSize;
    AllocationExtentDescriptor  *aed;
    Byte                        *ads;
    Uint32                       bp, adSize, nrBlocks;
    UdfAllocationList          **pAllocList;
    bool isUnallocatedSpaceEntryADs = (node == NULL);

    /* Create UdfAllocationList list for file data and
     * put ADEL_EXTENTPOINTER ADs in special overheadList.
     * Only overheadList for Unallocated Space Entry ADs.
     */
    if( isUnallocatedSpaceEntryADs )
    { if( adType != ADT_SHORT )     /* assert */
      { return FALSE;
      }
      pAllocList = &mc->partitionMapInfo[shortAdPartRefNr].shortOverheadList;
    }
    else
    { pAllocList = &node->al;
    }

    if(      adType == ADT_SHORT )  adSize = sizeof(ShortAd);
    else if( adType == ADT_LONG )   adSize = sizeof(LongAd);
    else return FALSE;

    /* Only ADT_SHORT or ADT_LONG allocation
     * descriptors from here. Be prepared that
     * adsLength may not be a multiple of adSize.
     */
#ifdef DEBUG01
    if( (adsLength % adSize) != 0 )
    {
        ifPRINTdebug01(uctout,
            "DEBUG01:\textractAllocationDescriptors:"
            " AD length no multiple of AD size: %lu, %lu\n",
                adsLength, adSize);
        ENDif;
/**     return FALSE;   **/     /* no error here */
    }
#endif  /* DEBUG01 */

    /* prepare for UdfAllocationList structure
     */
    if(    (*pAllocList) == NULL        /* does not yet exist */
       && ((*pAllocList) = createNewAllocationList(adType)) == NULL )
    {   return FALSE;
    }

    aed = NULL;     /* no space allocated for extent of allocation descr */
    ads = ADs;      /* begin of allocation descriptors argument  */
    bp  = 0;        /* byte position with respect to ads pointer */

    while( bp + adSize <= adsLength )   /* whole AD must fit */
    {
        AnyAllocationDescriptor *ad =
                        (AnyAllocationDescriptor *) (ads + bp);
        Uint32 extentSize = adGetExtentSize(&ad->anyAd);
        Uint8  extentType = adGetExtentType(&ad->anyAd);

        if( extentSize == 0 )   /* end of sequence */
        {   break;              /* may skip last ADs */
        }

        /* If NOT isUnallocatedSpaceEntryADs, then add all nonzero
         * size ADs to UdfAllocationList.
         * ADEL_EXTENTPOINTER type goes to separate overheadList
         * branch, also if isUnallocatedSpaceEntryADs.
         */
        if(    (   !isUnallocatedSpaceEntryADs
                ||  extentType == ADEL_EXTENTPOINTER )
           && !alAddDescriptor((*pAllocList), ad,
                    (extentType == ADEL_EXTENTPOINTER)) )   /* toOverheadList */
        {   checkFree((void**)&aed);
            return FALSE;
        }

        if( extentType != ADEL_EXTENTPOINTER )
        {   bp += adSize;               /* points to next one */
        }
        else    /* ADEL_EXTENTPOINTER, read new extent of allocation descriptors */
        {   Uint16  tagId;
            Uint32  nrBlocksRead = 0;
            Uint32  logicalBlockNumber = 0;
            Uint16  partRefNr = PREF_PARTITION_NOT_FOUND;

            switch( adType )
            {
            case ADT_SHORT:
                logicalBlockNumber = ad->shortAd.extentPosition;
                partRefNr = shortAdPartRefNr;
                break;
            case ADT_LONG:
                logicalBlockNumber = ad->longAd.extentLocation.logicalBlockNumber;
                partRefNr = ad->longAd.extentLocation.partitionReferenceNumber;
                /* If a Metadata Partition is present (UDF 2.50+),
                 * see "an extent type of 3" in 2.3.10
                 */
                if(   IS_PREF_PARTITION_FOUND(mc->metadataPref)
                   && partRefNr != shortAdPartRefNr )
                {
                  MLIMITbegin(ERROR00level, MLIMITdefault20);
                    fprintf(uctout,
                       "\tError: AD extent type 3, Partition Reference number: %lu, expected: %lu.\n"
                      "-\t       For volumes with a Metadata Partition, see UDF 2.3.10\n"
                      "-\t       about \"an extent of type 3 (continuation)\".\n",
                            partRefNr, shortAdPartRefNr);
                    nodePrintUnicodeNameContLine(node,mc);
                  MLIMITend;
                }
                break;
            }


            /* Do not read more than one block !!
             * (extent size tested in verifyExtentLength()).
             */
            nrBlocks = MIN(1, ROUNDUPELEMENTS(extentSize, blockSize));

            /* readDescriptor() will allocate new aed buffer
             * and verify AllocationExtentDescriptor but
             * not call swapAndVerifyAllocationDescriptors().
             * Free aed (if any) before allocating new one.
             * Mind that all pointers that may be pointing
             * in aed (like ad) are invalid after checkFree().
             */
            checkFree((void**)&aed);    /* clean up */

            if( !readDescriptor( mc, logicalBlockNumber, partRefNr,
                                 nrBlocks, &nrBlocksRead,
                                 tidAED, &tagId, (Byte**)&aed, node) )
            {   return FALSE;
            }

            /* AED plus allocation descriptors read into newly
             * allocated aed buffer.
             * Mind that AED is handled as if Allocation Descriptors
             * were part of the descriptor itself.
             */
            ads = (Byte*) &aed->startOfAllocationDescriptors;
            if( !swapAndVerifyAllocationDescriptors(
                         ads, &aed->lengthOfAllocationDescriptors,
                         (Byte*) aed, adType,
                         pNumberOfADs, pTotalRecordedBlocks,
                         shortAdPartRefNr, mc, node,
                         inMetadataPartition ) )
            {
                return FALSE;   /* TODO: check if error message */
            }
            adsLength = aed->lengthOfAllocationDescriptors;
            bp = 0;     /* start of new extent of ADs */
        }
    }       /* endwhile */
    checkFree((void**)&aed);            /* clean up */
    return TRUE;

}   /* end extractAllocationDescriptors() */

extern bool nodeReadAllocationDescriptors(UdfMountContext *mc, Node *node)
{
    Byte   *fe,             /* FE or EFE */
           *feStartOfAds;
    Uint8   feAdType, feFileType;
    Uint32  feLenOfAds;
    Uint64  feInformationLength;
    Uint64  totalRecordedBlocks = 0;
    bool    dataInMetadataPartition;

    if( node == NULL || node->fe == NULL )
    {   return FALSE;
    }

    fe = node->fe;
    feAdType = GET_ADTYPE(pFE_icbTag(fe)->flags);
    feFileType = (pFE_icbTag(fe))->fileType;
    feStartOfAds =  pFE_startOfExtendedAttributes(fe) +
                 (*(pFE_lengthOfExtendedAttributes(fe)));
    feLenOfAds = (*(pFE_lengthOfAllocationDescriptors(fe)));
    feInformationLength = (*pFE_informationLength(fe));

    /* Initialize for file body length and allocation
     * descriptors verification.
     */
    if( feAdType == ADT_INFE )
         node->feFileBodyLength = (Uint64) feLenOfAds;
    else node->feFileBodyLength = (Uint64) 0;
    node->feNumberOfADs         = (Uint16) 0;
    node->feTotalExtentsLength  = node->feFileBodyLength;

    if(        feAdType == ADT_INFE )
    { printMessageHead(INFO02level, fe, feStartOfAds, NULL);
      ifPRINTinfo02(uctout, "Embedded data, %lu byte%s\n",
                    feLenOfAds, PLURAL_S(feLenOfAds));
      ENDif;
      return TRUE;      /* no action */
    }
    else if(   feAdType != ADT_SHORT
            && feAdType != ADT_LONG )
    { return FALSE;     /* error, reported already */
    }
    else if( feLenOfAds == 0 )
    { printMessageHead(INFO02level, fe, feStartOfAds,
                       "No allocation descriptors\n");
      /* No return TRUE here because node->al must also be created
       * by extractAllocationDescriptors() for feLenOfAds == 0
       */
    }

    /* Only ADT_SHORT or ADT_LONG from here.
     * Determine if Ads must define data space inside
     * or outside the Metadata Partition (if any).
     */
    dataInMetadataPartition =
                (   feFileType == FT_DIRECTORY
                 || feFileType == FT_STREAM_DIRECTORY);

    /* Now read and verify all ADs.
     * First verify the FE embedded ADs by a direct call to
     * swapAndVerifyAllocationDescriptors() and then for
     * possible continuation extents read and verify the ADs
     * by extractAllocationDescriptors().
     *
     * call swapAndVerifyAllocationDescriptors(), mind:
     * verify lengthOfAllocationDescriptors for multiple of AD size too,
     * Mind that feLenOfAds may be 0 while AD type is short or long,
     * which is allowed for the FE, but not for a continuation extent
     * of allocation descriptors.
     */
    if(     feLenOfAds != 0
        && !swapAndVerifyAllocationDescriptors( feStartOfAds,
                         pFE_lengthOfAllocationDescriptors(fe),
                         fe, feAdType,
                        &node->feNumberOfADs, &totalRecordedBlocks,
                         node->fePartRef, mc, node,
                         dataInMetadataPartition ) )
    {                           /* error messages printed in */
#ifdef  DEBUG01
        ifVERBOSE(DEBUG01level) /* swapAndVerifyAllocationDescriptors() */
        {   fprintf(uctout, "DEBUG01: nodeReadAllocationDescriptors:"
                        " swapAndVerifyAllocationDescriptors failed\n");
            nodePrintUnicodeNameContLine(node,mc);
        }
        ENDif;
#endif  /* DEBUG01 */
        return FALSE;
    }

    if( !extractAllocationDescriptors(mc, feStartOfAds,
                feLenOfAds, feAdType,
                &node->feNumberOfADs, &totalRecordedBlocks,
                node->fePartRef, node,
                dataInMetadataPartition) )
    {   /** No error return here, allocation descriptors may be
         ** partially read and consistent.
         **     return FALSE;
        /**/
    }

    /* Only ADT_SHORT or ADT_LONG allocation descriptors.
     * Test if sufficient file body allocations.
     */
    if( feInformationLength > node->feFileBodyLength )
    {
        /* no FALSE return result */
        MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
             "\tError: Insufficient file body allocations of ");
          printUint64( MLIMIT.vl,
                       (Uint64) (feInformationLength
                                 - node->feFileBodyLength),
                                 FALSE, "%5lu");
          fprintf(uctout, " bytes\n"
            "-\t  Allocation Descriptors Information Length: ");
          printUint64(MLIMIT.vl, node->feFileBodyLength,
                                 FALSE, "%5lu");
          fprintf(uctout,
                "\n-\t\t      File Entry Information Length: ");
          printUint64(MLIMIT.vl, feInformationLength,
                                 FALSE, "%5lu");
          fprintf(uctout, "\n");
        MLIMITend;
    }
    return TRUE;

}   /* end nodeReadAllocationDescriptors() */


/***
 * Unicode related functions
 ***/

extern bool convertChar2Unicode(Byte *toBeConverted, unicode_t **converted)
{
    Uint32 i;
    if( (*converted = (unicode_t *)
          tst_malloc((strlen((char*)toBeConverted)+1) * sizeof(unicode_t),
                                    __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    for( i = 0; i < strlen((const char *) toBeConverted); i++ )
    {
        (*converted)[i] = toBeConverted[i];
    }
    (*converted)[i] = 0;
    return TRUE;
}

extern bool convertUnicode2Char(unicode_t *toBeConverted, Uint8 lengthOfUnicodeInBytes,
                         Byte *converted)
{
    Uint8 i;
    for( i = 0; i < lengthOfUnicodeInBytes/2; i++ )
    {
        converted[i] = (Uint8) ((Byte *) toBeConverted)[i * 2];
    }
    converted[i] = '\0';
    return TRUE;
}

/***
 * Handle allocation tables, bitmaps, etc.
 ***/

/* Free structures pointed to in pss structure
 */
static void checkFreePss(PartitionSpaceSet *pPss)
{
    if( pPss != NULL )
    {   checkFree((void**) &pPss->unallocatedSpaceBitmap);
        checkFree((void**) &pPss->unallocatedSpaceTable);
        checkFree((void**) &pPss->freedSpaceBitmap);
        checkFree((void**) &pPss->freedSpaceTable);
        checkFree((void**) &pPss->fabricatedBitmap);
    }
}

extern char *partitionSpaceSetText(PartitionSpaceSet *pss)
{
  char  *UFSS = "Unallocated/Freed Space Set",
        *UFB  = "Unallocated/Freed Bitmap",
        *UFT  = "Unallocated/Freed Table",
        *FSB  = "Freed Space Bitmap",
        *USB  = "Unallocated Space Bitmap",
        *FST  = "Freed Space Table",
        *UST  = "Unallocated Space Table",
        *ELSE = "<no Space Set found>";
  int cntB, cntT, cntU, cntF;

  cntB = cntT = cntU = cntF = 0;
  if(pss->unallocatedSpaceBitmap) {cntU++; cntB++;}
  if(pss->unallocatedSpaceTable)  {cntU++; cntT++;}
  if(pss->freedSpaceBitmap)       {cntF++; cntB++;}
  if(pss->freedSpaceTable)        {cntF++; cntT++;}
  return (
    (cntB && cntT)               ? UFSS :
    (cntB == 2)                   ? UFB :
    (cntT == 2)                    ? UFT :
    (cntB && pss->freedSpaceBitmap) ? FSB :
    (cntB)                          ? USB :
    (cntT && pss->freedSpaceTable)  ? FST :
    (cntT)                          ? UST : ELSE );
}

/* readUseICB():
 * Read Unallocated Space Entry ICB.
 * In case of strategy 4096, expand ICB
 * reading last valid USE descriptor.
 */
static bool readUseICB( UdfMountContext *mc,
                        Uint32  logicalBlockNr,
                        Uint16  partRefNr,
                        Uint32  extentSize,
                        Uint32 *pNrBlocksRead,
                        Byte  **pMem)
{
    Uint16  tagId;
    bool    result = FALSE,
            isStrategy4096;

    /* read USE ICB, expand if strategy 4096
     */
    *pNrBlocksRead = 1;         /* default */

    if( readIcbDirectEntry( mc, NULL,   /* NULL node */
                            logicalBlockNr, partRefNr,
                            extentSize,
                           &tagId, &isStrategy4096,
                            pMem, 0) )  /* start recursion */
    {   /* read ICB ok */
        if( isStrategy4096 )
        { (*pNrBlocksRead)++;       /* one block extra */
        }
        if( tagId != tidUSE )       /* descriptor is not USE */
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            printMessageHead(MLIMIT.vl, *pMem, *pMem,
              "Error: Wrong descriptor, expected Unallocated Space Entry.\n");
          MLIMITend;
          checkFree((void**)pMem);      /* no USE found */
        }
        else
        { result = TRUE;                /* read USE ok */
        }
    }
    return result;  /* not implemented */
}


/* read Unallocated or Freed Bitmap or Table.
 *
 * txt1: "Unallocated" for Unallocated Space Bitmap/Table
 *       "Freed"       for       Freed Space Bitmap/Table
 *       "Metadata"    for          Metadata Bitmap
 * expectedTagId: tidSBD for bitmap
 * expectedTagId: tidUSE for table
 *
 * TODO: For Space Table, read and verify possible
 *       additional extents of ADs (with AED).
 */
static bool readSpaceTableOrBitmap(
                        char *txt1, UdfMountContext *mc,
                        ShortAd *pShortAd, Uint16 partRef,
                        Uint16 expectedTagId,
                        Byte **ppBuffer )
{
    const MediumInfo    *vmi = getTheMediumInfo();
    Uint32               blockSize = vmi->blockSize;
    Uint32               allocBlocks, nettBlocks;
    Uint64               dummyRecordedBlocks;
    Uint16               tagId;
    bool                 expectBitmap, tmpResult;
    char                *spaceTypeTxt;
    PartitionMapInfo    *pmi = &mc->partitionMapInfo[partRef];
    PartitionDescriptor *pd  =  pmi->pdPointer;

    if( expectedTagId == tidSBD )
    {   expectBitmap = TRUE;
        spaceTypeTxt = "Space Bitmap";
    }
    else if( expectedTagId == tidUSE )
    {   expectBitmap = FALSE;
        spaceTypeTxt = "Space Table";
    }
    else
    {   return FALSE;
    }

    /* do the actual read
     */
    if( pmi->pMapType == PMAPTYPE_METADATA )
    {   Uint32 sbdSize;
        (*ppBuffer) = (Byte*) readMetadataBitmapFile(mc, partRef, &sbdSize);
        if( (*ppBuffer) == NULL )
        { return FALSE; /* error, message in readMetadataBitmapFile() */
        }
        allocBlocks = ROUNDUPELEMENTS(mc->metadataBitmapFile->size32, blockSize);
        nettBlocks = ROUNDUPELEMENTS(sbdSize, blockSize);
    }
    else    /* 'normal' partition */
    {   Uint32 extentPosition = pShortAd->extentPosition,
               extentSize     = adGetExtentSize(pShortAd);
        allocBlocks = ROUNDUPELEMENTS(extentSize, blockSize);

        VERBOSE00(uctout, "\t    %s %s %sextent: %lu block%s\n",
                        txt1, spaceTypeTxt,
                        (expectBitmap) ? "" : "first ",
                        allocBlocks, PLURAL_S(allocBlocks));

        tmpResult = ( expectBitmap )
            ? readDescriptor(mc, extentPosition, partRef,   /* SBD */
                                  allocBlocks, &nettBlocks,
                                  expectedTagId, &tagId,
                                  ppBuffer, NULL)
            : readUseICB(mc, extentPosition, partRef,       /* USE */
                         extentSize, &nettBlocks, ppBuffer);

        if( tmpResult == FALSE )
        {   VERBOSE00(uctout, "-");     /* continuation message */
            printMessageHead(VERBOSE00level, (Byte*)pd,
                             (Byte*)pShortAd, NULL);
            VERBOSE00(uctout, "Serious error reading %s %s defined on\n",
                              txt1, spaceTypeTxt);
            printExtraInfoForPHD((Byte*)pd, (Byte*)pShortAd);
            return FALSE;
        }
    }

    /* Warn if not all allocated blocks were read/used
     * (mind: 2 blocks (DE+IE) for strategy 4096 USE).
     */
    if( nettBlocks != allocBlocks ) /* not all allocated blocks read */
    {   VERBOSE00(uctout,
            "\t%s %s uses %lu of %lu allocated blocks\n",
                txt1, spaceTypeTxt, nettBlocks, allocBlocks);
    }

    /* verify bitmap size / partition size
     */
    if( expectedTagId == tidSBD )
    {   SpaceBitmapDescriptor *sbd = (SpaceBitmapDescriptor*) (*ppBuffer);
        if( sbd->numberOfBits != pmi->actualPartitionLength )
        {
            MLIMITbegin(ERROR00level, uctMessageLimit);
              printMessageHead(MLIMIT.vl, (Byte*)sbd,
                               (Byte*)&sbd->numberOfBits, NULL);
              fprintf(uctout,
                "Error: Inconsistent Space Bitmap NumberOfBits and\n"
                "-\t\t\tpartition size: %lu, %lu. ECMA 4/14.12.4\n",
                sbd->numberOfBits, pmi->actualPartitionLength);
            MLIMITend;
            /* no FALSE return, not fatal,
             * verifyPartitionAllocation() can handle this
             */
        }
    }
    else if( expectedTagId == tidUSE )
    {   UnallocatedSpaceEntry *use = (UnallocatedSpaceEntry*) (*ppBuffer);
        Uint32 countADs = 0;

        /* Verify Allocation Descriptors in USE
         * Read and verify extra AED extents if needed
         * adType shall be ADT_SHORT but ADT_LONG will also
         * correctly be handled.
         * Mind extra requirements, UDF 2.3.7.1
         * Fabricate bitmap vector from USE ADs for
         * final allocation checks.
         */
        if( !markUnallocatedPartitionSpace(mc, partRef,
                    0, 0, NULL, TRUE) )     /* createOnly */
        { return FALSE;
        }
        verifyUseAdOrderInit(blockSize);
        if( !swapAndVerifyAllocationDescriptors(
                    (Byte*) &use->startOfAllocationDescriptors,
                            &use->lengthOfAllocationDescriptors,
                    (Byte*)  use, GET_ADTYPE(use->icbTag.flags),
                            &countADs, &dummyRecordedBlocks,
                             partRef, mc, NULL,
                             FALSE ) )  /* NOT inMetadataPartition */
        {
            return FALSE;
        }
        (void) extractAllocationDescriptors( mc,
                            &use->startOfAllocationDescriptors,
                             use->lengthOfAllocationDescriptors,
                             GET_ADTYPE(use->icbTag.flags),
                            &countADs, &dummyRecordedBlocks,
                             partRef, NULL,
                             FALSE );   /* NOT inMetadataPartition */
        VERBOSE00(uctout,
            "  ==>\tp%u %s %s, partition total %s blocks: %lu\n",
                partRef, txt1, spaceTypeTxt, txt1,
                getUseAdOrderTotalBlocks());
    }
    return TRUE;

}   /* end readSpaceTableOrBitmap() */


/* Read the Unallocated and Freed Space Tables or Bitmaps
 * for a given partition in a logical volume.
 * Note: For checks, see also verifyPartitionHeaderDescriptor().
 *
 * UDF 250: Also the Metadata Bitmap File is read here and handled
 *          more or less as an Unallocated Space Bitmap for the
 *          Metadata Partition.
 *
 * TODO: verify bitmap, etc. size,
 *       ((SpaceBitmapDescriptor*)d)->numberOfBytes;
 */
static bool readPartitionSpaceSets(UdfMountContext *mc, Uint16 partRef)
{
    PartitionMapInfo          *pmi  = &mc->partitionMapInfo[partRef];
    PartitionDescriptor       *pd   =  pmi->pdPointer;
    PartitionSpaceSet         *pPss = &pmi->pss;
    bool    unallTableFound  = FALSE,
            freedTableFound  = FALSE,
            unallBitmapFound = FALSE,
            freedBitmapFound = FALSE,
            isSequentialPartition;
    Byte   *buffer;
    Uint32  errorCnt = 0;
    Uint16  cntpRef;

    /* Handle the special situation of a Metadata Partition first.
     * In that case, there is an SBD in the body of the Metadata
     * Bitmap File. This SBD is the Space Set for the Metadata Partition
     * and is handled in the same way as the Unallocated Space Bitmap
     * for a normal partition.
     */
    if( pmi->pMapType == PMAPTYPE_METADATA )
    {
        if( !readSpaceTableOrBitmap( "Metadata", mc, NULL,
                                     partRef, tidSBD, &buffer) )
        { errorCnt++;
        }
        else        /* SBD in Metadata Bitmap File ok */
        { pPss->unallocatedSpaceBitmap =
                            (SpaceBitmapDescriptor *) buffer;
        }
    }
    else    /* handle 'normal' partition */
    {   PartitionHeaderDescriptor *phd =
            &pd->partitionContentsUse.partitionHeaderDescriptor;
        /* check Unallocated Space Table
         */
        if( adGetExtentSize(&phd->unallocatedSpaceTable) != 0 )
        {
            if( !readSpaceTableOrBitmap( "Unallocated", mc,
                        &phd->unallocatedSpaceTable, partRef,
                        tidUSE, &buffer) )
                 errorCnt++;
            else pPss->unallocatedSpaceTable =
                          (UnallocatedSpaceEntry *) buffer;
        }
        /* check Freed Space Table
         */
        if( adGetExtentSize(&phd->freedSpaceTable) != 0 )
        {
            if( !readSpaceTableOrBitmap( "Freed", mc,
                        &phd->freedSpaceTable, partRef,
                        tidUSE, &buffer) )
                 errorCnt++;
            else pPss->freedSpaceTable =
                                (UnallocatedSpaceEntry *) buffer;
        }
        /* check Unallocated Space Bitmap
         */
        if( adGetExtentSize(&phd->unallocatedSpaceBitmap) != 0 )
        {
            if( !readSpaceTableOrBitmap( "Unallocated", mc,
                        &phd->unallocatedSpaceBitmap, partRef,
                        tidSBD, &buffer) )
                 errorCnt++;
            else pPss->unallocatedSpaceBitmap =
                                (SpaceBitmapDescriptor *) buffer;
        }
        /* check Freed Space Bitmap
         */
        if( adGetExtentSize(&phd->freedSpaceBitmap) != 0 )
        {
            if( !readSpaceTableOrBitmap( "Freed", mc,
                        &phd->freedSpaceBitmap, partRef,
                        tidSBD, &buffer) )
                 errorCnt++;
            else pPss->freedSpaceBitmap =
                               (SpaceBitmapDescriptor *) buffer;
        }
    }

    unallTableFound  = (pPss->unallocatedSpaceTable  != NULL);
    freedTableFound  = (pPss->freedSpaceTable        != NULL);
    unallBitmapFound = (pPss->unallocatedSpaceBitmap != NULL);
    freedBitmapFound = (pPss->freedSpaceBitmap       != NULL);

    /* Expect a Space Set to be found for each partition,
     * except for a read-only, pseudo-overwritable or
     * sequential partition, see UDF 2.60 2.3.3.
     */
    cntpRef = mc->partitionMapInfo[partRef].counterpartPRef;
    isSequentialPartition =
        (   mc->partitionMapInfo[partRef].pMapType == PMAPTYPE_VIRTUAL
         || mc->partitionMapInfo[cntpRef].pMapType == PMAPTYPE_VIRTUAL);

    if(   !unallTableFound && !unallBitmapFound
       && !freedTableFound && !freedBitmapFound )
    {
        VERBOSE00(uctout, "\tp%u: No partition Space set found\n",
                            partRef);
        /* If one was expected, complain only in
         * the following cases (see UDF 2.60, 2.3.3):
         */
        if(   !isSequentialPartition
           && (   pd->accessType == PDAT_WRITEONCE
               || pd->accessType == PDAT_REWRITABLE
               || pd->accessType == PDAT_OVERWRITABLE) )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
                "\tError: Space Set missing for %s%s %s partition p%u\n",
                (    pd->accessType == PDAT_WRITEONCE
                 && !isSequentialPartition ) ? "nonsequential " : "",
                PDAT_TEXT(pd->accessType),
                PMAPTYPE_TEXT(pmi->pMapType), partRef );
          MLIMITend;
        }
    }
    else
    {   VERBOSE00(uctout, "\tp%u: Space set found\n", partRef);
        /* If unexpected, complain only in cases mentioned
         * in UDF 2.60 2.3.3. Note that illegal Bitmap File test
         * is already done in readMetadataBitmapOrLVSpaceSets(),
         * so skip test here for Metadata Partition.
         */
        if(    pmi->pMapType != PMAPTYPE_METADATA
           && (       isSequentialPartition
               ||     pd->accessType == PDAT_READONLY
               || (   pd->accessType == PDAT_POW_OR_UNKNOWN
                   && getUctUdfRevision() >= 0x260) ) )
        { char format1000[1000];
          sprintf(format1000,
               "\t%%8s Space Set %%s for%s\n"
                "-\t\t %s %s partition p%u%%s.\n",
                (isSequentialPartition) ? " sequential" : "",
                PDAT_TEXT(pd->accessType),
                PMAPTYPE_TEXT(pmi->pMapType), partRef);

          if( getUctUdfRevision() >= 0x250 )    /* UDF 2.50 errata DCN-5102 */
          {     /* UDF 2.50+: Space Set "shall not be recorded" */
            MLIMITbegin(ERROR00level,uctMessageLimit);
              fprintf(uctout, format1000,
                 "Error:", "shall not be recorded",
                 (getUctUdfRevision() >= 0x260)
                    ? ", UDF 2.3.3"                     /* UDF 2.60+ */
                    : ", UDF 2.50 errata DCN-5102");    /* UDF 2.50  */
            MLIMITend;
          }
          else  /* UDF 2.01-: Space Set "not required" */
          { MLIMITbegin(WARN01level,uctMessageLimit);
              fprintf(uctout, format1000,
                 "Warning:", "not required", "");
            MLIMITend;
          }
        }

        /* check if mixed table and bitmap usage
         */
        if(   (unallTableFound && unallBitmapFound)
           || (freedTableFound && freedBitmapFound) )
        {   MLIMITbegin(ERROR00level,uctMessageLimit);
              fprintf(uctout,
                "\tError: Mix of space table and space bitmap"
                                " in partition p%u\n", partRef );
            MLIMITend;
            errorCnt++;
        }
    }
    VERBOSE00(uctout,"\t--\n");

    /* all done, check errors.
     */
    if( errorCnt != 0 )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError: %lu Unallocated/Freed"
                                " Space Set errors found.\n"
            "-\t       Unable to perform Partition"
                                " Space allocation checks.\n",
                    errorCnt);
        MLIMITend;
        /* free all tables/bitmaps, except fabricatedBitmap
         */
        checkFree((void**)&pPss->unallocatedSpaceTable);
        checkFree((void**)&pPss->freedSpaceTable);
        checkFree((void**)&pPss->unallocatedSpaceBitmap);
        checkFree((void**)&pPss->freedSpaceBitmap);
    }

    /* check if Space Set present
     */
    if(   pPss->unallocatedSpaceTable  || pPss->freedSpaceTable
       || pPss->unallocatedSpaceBitmap || pPss->freedSpaceBitmap )
    {   pPss->status |= F_PSS_LOADED;       /* loaded something */
    }
    return TRUE;    /* does not mean there are no errors */

}   /* end readPartitionSpaceSets() */


/* Read partition Space Sets.
 * For Metadata Partition, read Metadata Bitmap File,
 * else: Read unallocated/freed space bitmaps/tables for all
 *       partitions referenced by the LVD Partition Maps.
 */
static bool readMetadataBitmapOrLVSpaceSets(UdfMountContext *mc)
{
    Uint16 pRef;
    Uint32 numberOfPartitionMaps = mc->vi->lvd->numberOfPartitionMaps;

    /* only load space set for mapped partitions,
     * so a partition for which a partition map exists
     */
    for( pRef = 0; pRef < numberOfPartitionMaps; pRef++ )
    {
        PartitionMapInfo    *pmi  = &mc->partitionMapInfo[pRef];
        PartitionSpaceSet   *pPss = &pmi->pss;
        PMapType             pMapType = pmi->pMapType;

        /* no space set for virtual partition
         * and read-only exception for metadata partition
         */
        if(      pMapType == PMAPTYPE_VIRTUAL )
        {   continue;       /* to next partition map */
        }
        else if( pMapType == PMAPTYPE_METADATA )
        {   Uint32 accessType = pmi->pdPointer->accessType;
            MetadataPartitionMapTail *mTail =
              &pmi->pPartitionMap->type2PartitionMap.SharedTail.metadataTail;

            /* Bitmap File must be present, except that it is not allowed
             * for a read-only or pseudo-overwritable partition
             * (UDF 2.60 and UDF 2.50 errata DCN-5102)
             */
            if( mTail->metadataBitmapFileLocation == MAX_UINT32 )
            { VERBOSE00(uctout,
                "\n  ==>\tp%u: No Metadata Bitmap File for %s partition.\n"
                       "\t--\n", pRef, PDAT_TEXT(accessType));

              if(   accessType != PDAT_READONLY
                 && accessType != PDAT_POW_OR_UNKNOWN )
              { MLIMITbegin(ERROR00level,uctMessageLimit);
                  fprintf(uctout,
                     "\tError: Metadata Bitmap File undefined for "
                                                    "%s partition.\n"
                    "-\t       Metadata Bitmap File shall be present, "
                                                    "UDF 2.2.13%s.\n",
                        PDAT_TEXT(accessType),
                        (getUctUdfRevision() < 0x260)
                            ? ",\n-\t       UDF 2.50 errata DCN-5102"
                            : "");
                MLIMITend;
              }
              continue;     /* no bitmap file, to next partition map */
            }

            /* bitmap file present. Test here if allowed, so in
             * readPartitionSpaceSets() test whether a Space Set
             * is allowed or not can be skipped.
             */
            if(        accessType == PDAT_READONLY
                || (   accessType == PDAT_POW_OR_UNKNOWN
                    && getUctUdfRevision() >= 0x260) )
            { MLIMITbegin(ERROR00level, uctMessageLimit);
                fprintf(uctout, "\n  ==>\tp%u:"
                    " Error: Metadata Bitmap File shall"
                                    " not be recorded for %s\n"
                    "-\t\t   partition, UDF 2.2.13%s.\n",
                    pRef, PDAT_TEXT(accessType),
                    (getUctUdfRevision() < 0x260)
                        ? ", UDF 2.50 errata DCN-5102"
                        : "");
              MLIMITend;
            }
        }
        VERBOSE00(uctout,
            "\n  ==>\tp%u: read %s\n", pRef,
                (pMapType == PMAPTYPE_METADATA)
                    ? "Space Set from Metadata Bitmap File"
                    : "Unallocated or Freed Partition Space Sets");
        fflush(uctout);

        if( pPss->status & F_PSS_LOADED )
        {   MLIMITbegin(ERROR00level,uctMessageLimit);
              fprintf(uctout,
                "\tError: Space set for partition p%u already loaded,\n"
                "-\t      program error, please report\n", pRef);
            MLIMITend;
            return FALSE;
        }
        else if( !readPartitionSpaceSets(mc, pRef) )
        {
            return FALSE;       /* fatal error */
        }
    }       /* endfor */

    return TRUE;

}   /* end readMetadataBitmapOrLVSpaceSets() */

/* Verify if 2 Volume Descriptors are equivalent
 * according to ECMA 3/8.4.2.3 or 'identical' according
 * to ECMA 3/8.4.1, so discarding the tagChecksum,
 * descriptorCRC, descriptorCRCLength and tagLocation
 * fields.
 *
 * return value: TRUE if equivalent descriptors,
 *               *pOffset remains unchanged.
 *         else: FALSE and *pOffset denotes the
 *               byte offset of the first field
 *               that shows non-equivalent values.
 */
static bool isEquivalentTag(Tag *t1, Tag *t2, Uint32 *pOffset)
{
    UCTASSERT( t1 != NULL && t2 != NULL);

    if( t1->tagIdentifier != t2->tagIdentifier )
    {   *pOffset = offsetof(Tag, tagIdentifier);
        return FALSE;
    }
    if( t1->descriptorVersion != t2->descriptorVersion )
    {   *pOffset = offsetof(Tag, descriptorVersion);
        return FALSE;
    }
    if( t1->reserved != t2->reserved )
    {   *pOffset = offsetof(Tag, reserved);
        return FALSE;
    }
    if( t1->tagSerialNumber != t2->tagSerialNumber )
    {   *pOffset = offsetof(Tag, tagSerialNumber);
        return FALSE;
    }
    return TRUE;
}

/* Check if descriptor byte must be discarded for
 * equivalence test according to ECMA 3/8.4.1 or
 * 3/8.4.2.3, so discarding the tagChecksum,
 * descriptorCRC, descriptorCRCLength and
 * tagLocation fields.
 * return value: TRUE if byte must be discarded
 *         else: FALSE
 */
static bool isEquivalentByteDiscard(Uint32 offset)
{
    return(   offset >= offsetof(Tag, tagChecksum)
           && offset <  offsetof(Tag, reserved))
       || (   offset >= offsetof(Tag, descriptorCRC)
           && offset <  sizeof(Tag));
}

/* isEquivalentDescriptor():
 * Verify if 2 Descriptors are equivalent
 * testId defines the error text and assures
 * separate MLIMIT global error counting.
 */
#define EQ_VDS_EQUAL_VDSN       0
#define EQ_VDS_MAIN_RESERVE     1
#define EQ_SPARINGTABLES        2

/* if     ( testId == EQ_VDS_EQUAL_VDSN )
 * then: check for ECMA 3/8.4.1,
 *       d1 and d2 have identical VDS Number values and
 *       VDS Number field (Uint32) is first field after Tag.
 *
 * else if( testId == EQ_VDS_MAIN_RESERVE )
 * then: check for ECMA 3/8.4.2.3
 *
 * else if( testId == EQ_SPARINGTABLES )
 * then: check for UDF 2.2.9, 2.2.12 (was 2.2.11)
 *
 * else: return FALSE
 *
 * Note: Also 2 NULL pointers are equivalent/identical !!
 */
static bool isEquivalentDescriptor( Byte *d1, Byte *d2,
                                    int testId )
{
           Uint32 dLen1, dLen2, offset, cnt,
                  vdsn = 0;             /* vol descr seq nmb */
           char  *refTxt1, *refTxt2;
           bool   result = TRUE;
    static bool   firstTime = TRUE,
                  isBigEndian;

    if( firstTime )
    {   firstTime = FALSE;      /* execute only once */
        isBigEndian = isBigEndianPlatform();
    }

    if( d1 == NULL && d2 == NULL )
    {   return TRUE;                /* equivalent too !! */
    }

    switch( testId )
    {
    case EQ_VDS_EQUAL_VDSN:
        refTxt2 = "ECMA 3/8.4.1";
        refTxt1 =
          "-\t  All volume descriptors with identical"
                                            " Volume Descriptor\n"
          "-\t  Sequence Numbers shall have identical contents";

        vdsn =             *((Uint32*)(d1 + sizeof(Tag)));
        UCTASSERT( vdsn == *((Uint32*)(d2 + sizeof(Tag))) );
        break;
    case EQ_VDS_MAIN_RESERVE:
        refTxt2 = "ECMA 3/8.4.2.3";
        refTxt1 =
          "-\t  Prevailing descriptor in Main VDS shall"
                                    " have equivalent contents\n"
          "-\t  as corresponding descriptor in Reserve VDS";
        break;
    case EQ_SPARINGTABLES:
        refTxt2 = "UDF 2.2.9";
        refTxt1 =
          "-\t     Multiple Sparing Tables shall have equivalent contents";
        break;
    default:
        return FALSE;   /* testId error */
        /** break; **/
    }

    if( d1 == NULL || d2 == NULL )      /* not both */
    {   Byte *d = (d1 != NULL) ? d1 : d2;
        MLIMITbegin(ERROR00level,uctMessageLimit);
          printMessageHead( MLIMIT.vl, d, NULL, NULL);
          fprintf(uctout,
              "Error: Missing descriptor for equivalence check,\n"
            "-\t\t    %s.\n", refTxt2);
        MLIMITend;
        return FALSE;       /* d1 or d2 NULL */
    }

    /* separate message for different tag ids
     */
    if( ((Tag*)d1)->tagIdentifier != ((Tag*)d2)->tagIdentifier )
    {
        switch( testId )
        {
        case EQ_VDS_EQUAL_VDSN:     /* ECMA 3/8.4.1 */
          MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
               "\tError: Different Tag Ids %u, %u (%s, %s),"
                                        " for descriptors\n"
              "-\t       with identical VDS number %lu, %s.\n",
                       ((Tag*)d1)->tagIdentifier,
                       ((Tag*)d2)->tagIdentifier,
              tidTEXT4(((Tag*)d1)->tagIdentifier),
              tidTEXT4(((Tag*)d2)->tagIdentifier), vdsn, refTxt2 );

          MLIMITend;
          break;
        case EQ_VDS_MAIN_RESERVE:   /* ECMA 3/8.4.2.3 */
          MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
               "\tisEquivalentDescriptor error:"
                    " Different Tag Ids: %u, %u\n"
              "-\t(%s, %s), please report, %s.\n",
                       ((Tag*)d1)->tagIdentifier,
                       ((Tag*)d2)->tagIdentifier,
              tidTEXT4(((Tag*)d1)->tagIdentifier),
              tidTEXT4(((Tag*)d2)->tagIdentifier), refTxt2);
          MLIMITend;
          break;
        case EQ_SPARINGTABLES:      /* Sparing Tables */
          MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
               "\tisEquivalentDescriptor error:"
                    " Different Tag Ids: %u, %u\n"
              "-\t(%s, %s), %s.\n",
                       ((Tag*)d1)->tagIdentifier,
                       ((Tag*)d2)->tagIdentifier,
              tidTEXT4(((Tag*)d1)->tagIdentifier),
              tidTEXT4(((Tag*)d2)->tagIdentifier), refTxt2);
          MLIMITend;
          break;
        }
        return FALSE;   /* different tag id */
    }

    if(   !getLengthOfDescriptor(d1, TRUE, &dLen1)
       || !getLengthOfDescriptor(d2, TRUE, &dLen2) )
    {
      MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageHead( MLIMIT.vl, d1, NULL,
                  "Descriptor equivalence error: Program error,\n"
          "-\t     unable to get descriptor lengths, please report.\n");
      MLIMITend;
      return FALSE;     /* cannot get length */
    }

    if( dLen1 != dLen2 )
    {
      MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageHead( MLIMIT.vl, d1, NULL, NULL);
        fprintf(uctout,
           "Error: Unequal descriptor lengths for"
                            " equivalent descriptors.\n"
          "-\t\t    lengths: %lu, %lu, %s.\n%s, %s.\n",
            dLen1, dLen2,
            tidECMAREF( ((Tag*)d1)->tagIdentifier,
                        getUctUdfRevision() ),
            refTxt1, refTxt2);
      MLIMITend;
      dLen1 = MIN(dLen1, dLen2); /* compare common part only */
      result = FALSE;            /* differnet length */
    }

    /* use dLen1 only from here
     * assume dLen1 is at least sizeof(Tag)
     * note that at least the tag Identifiers are equal
     */
    offset = 0;
    if(   !isEquivalentTag((Tag*) d1, (Tag*) d2, &offset)
       ||  memcmp(d1 + sizeof(Tag), d2 + sizeof(Tag),
                  dLen1 - sizeof(Tag)) != 0 )
    {                   /* not equivalent */
      if( offset == 0 ) /* memcmp() did not set offset */
      { for( offset = sizeof(Tag); offset < dLen1; offset++ )
        { if( d1[offset] != d2[offset] ) break;
        }
      }
      /* offset points at first non-equivalent byte or field.
       * Note that offset may be different
       * for little and big endian machines.
       * However, they are always inside the
       * same descriptor field.
       */
      MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageHead( MLIMIT.vl, d1, d1 + offset, NULL);
        fprintf(uctout,
          "Error: Non-equivalent bytes in equivalent descriptors,\n"
          "-\t\t\t%s.\n%s, %s.\n",
          tidECMAREF( ((Tag*)d1)->tagIdentifier,
                      getUctUdfRevision() ),
          refTxt1, refTxt2);

        if( isBigEndian )
        { fprintf(uctout,
            "-\tNote: For big endian machines, the byte offset"
                                            " may be inaccurate,\n"
            "-\t      but it points in the correct descriptor field.\n");
        }
        fprintf(uctout, "-\tbyte differences:"
                " [<BP>: <Main descr value> <Reserve descr value>]+");  /* no \n */

        cnt = 0;        /* count error bytes */
        for( ; offset < dLen1; offset++ )
        { if(  !isEquivalentByteDiscard(offset)
             && d1[offset] != d2[offset] )
          { if( cnt >= 20 )
            { fprintf(uctout,
                "\n-\t  etc., max 20 differences listed.");
              break;
            }
            if( ((cnt++) % 5) == 0 )
            { fprintf(uctout, "\n-\t");
            }
            fprintf(uctout, "[%3d: #%02X #%02X]",
                    offset, d1[offset], d2[offset]);
          }
        }
        fprintf(uctout, "\n-\t ------\n");
      MLIMITend;
      return FALSE;     /* not equivalent */
    }

    return result;

}   /* end isEquivalentDescriptor() */


/* storePVDInUdfVolumeInformation:
 * There shall be exactly one prevailing PVD,
 * UDF 2. Restrictions & ...
 */
static bool storePVDInUdfVolumeInformation(
                      UdfVolumeInformation *vi, Byte *buffer,
                      Uint32 buflen)
{
    PrimaryVolumeDescriptor *bufPvd =
                (PrimaryVolumeDescriptor*) buffer;
    char *txt = "Add PVD to VDS info";      /* default */

    UCTASSERT( buflen == sizeof(PrimaryVolumeDescriptor) );

    /* for first one: vi->pvd == NULL
     */
    if( vi->pvd != NULL )       /* already one present */
    {
        if(   memcmp( bufPvd->volumeIdentifier,
                     vi->pvd->volumeIdentifier,
               sizeof(bufPvd->volumeIdentifier)) == 0
           && memcmp( bufPvd->volumeSetIdentifier,
                     vi->pvd->volumeSetIdentifier,
               sizeof(bufPvd->volumeSetIdentifier) + sizeof(Charspec))
              == 0 )
        {   /* equal Volume Identifier, Volume Set Identifier
             * and Descriptor Character Set.
             */
            if(    bufPvd->volumeDescriptorSequenceNumber
                == vi->pvd->volumeDescriptorSequenceNumber )
            {
                if( isEquivalentDescriptor(buffer, (Byte*) vi->pvd,
                                EQ_VDS_EQUAL_VDSN) ) /* ECMA 3/8.4.1 */
                {   VERBOSE00(uctout,
                        "  ==>\tIgnore equivalent PVD\n");
                    return TRUE;            /* legal, ignore */
                }
                /* not equivalent, error flagged in
                 * isEquivalentDescriptor(), ignore.
                 */
                txt = "Replace PVD in VDS info by non-equivalent one";
            }
            else if(   bufPvd->volumeDescriptorSequenceNumber
                    < vi->pvd->volumeDescriptorSequenceNumber )
            {
                VERBOSE00(uctout,
                    "  ==>\tIgnore non-prevailing PVD\n");
                return TRUE;            /* legal, ignore */
            }
            else
            {   txt = "Replace PVD in VDS info by prevailing one";
            }
        }
        else    /* more than one prevailing PVD */
        {
            MLIMITbegin(ERROR00level,uctMessageLimit);
              printMessageHead( MLIMIT.vl, (Byte*) bufPvd,
                                (Byte*) &bufPvd->volumeIdentifier,
                      "Error: Multiple prevailing PVDs, UDF 2.\n"
                "-\t\t Unequal contents of ECMA 3/8.4.3 fields.\n"
                "-\t       Volume Identifiers: " );
              PRINTDSTRING( bufPvd->volumeIdentifier,
                     sizeof(bufPvd->volumeIdentifier), "\n");
              fprintf(uctout,     "-\t\t\t\t : ");
              PRINTDSTRING( vi->pvd->volumeIdentifier,
                     sizeof(vi->pvd->volumeIdentifier), "\n");
              fprintf(uctout,
                "-\t   Volume Set Identifiers: ");
              PRINTDSTRING( bufPvd->volumeSetIdentifier,
                     sizeof(bufPvd->volumeSetIdentifier), "\n");
              fprintf(uctout,     "-\t\t\t\t : ");
              PRINTDSTRING( vi->pvd->volumeSetIdentifier,
                     sizeof(vi->pvd->volumeSetIdentifier), "\n");
              fprintf(uctout,
                "-\tDescr Char Set Type, Info: %3u, ",
                       bufPvd->descriptorCharacterSet.characterSetType);
              printBytesName(
                       bufPvd->descriptorCharacterSet.characterSetInfo,
                sizeof(bufPvd->descriptorCharacterSet.characterSetInfo),
                       FALSE, MLIMIT.vl);
              fprintf(uctout,   "\n-\t\t\t\t : %3u, ",
                       vi->pvd->descriptorCharacterSet.characterSetType);
              printBytesName(
                       vi->pvd->descriptorCharacterSet.characterSetInfo,
                sizeof(vi->pvd->descriptorCharacterSet.characterSetInfo),
                       FALSE, MLIMIT.vl);
              fprintf(uctout,
                "\n-\tNote: Identifiers can be equal but"
                                " have different compression IDs.\n");
              if(    bufPvd->volumeDescriptorSequenceNumber
                 == vi->pvd->volumeDescriptorSequenceNumber )
              {     /* extra error, do not count extra */
                fprintf(uctout,
                  "-\tExtra error: Equal VDS Number (%u) for"
                                    " unequal PVDs, ECMA 3/8.4.1.\n",
                    bufPvd->volumeDescriptorSequenceNumber);
              }
            MLIMITend;

            txt = "Replace PVD in VDS info, unequal ECMA 3/8.4.3 fields";
        }
    }

    /* add/replace PVD, log reason
     */
    VERBOSE00(uctout, "  ==>\t%s\n", txt);

    if( vi->pvd == NULL )
    {   /* all PVD have equal size, allocate once
         */
        vi->pvd = (PrimaryVolumeDescriptor*) tst_malloc(
                   sizeof(PrimaryVolumeDescriptor),
                   __FILE__,__LINE__ );

        if( vi->pvd == NULL )       /* out of memory */
        {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
        }
    }
    memcpy((Byte *) vi->pvd, buffer, sizeof(PrimaryVolumeDescriptor));
    return TRUE;
}

/* storeLVDInUdfVolumeInformation:
 * There shall be exactly one prevailing LVD recorded
 * per Volume Set, UDF 2. Restrictions & ...
 * So keep only one !!
 */
static bool storeLVDInUdfVolumeInformation(
                      UdfVolumeInformation *vi, Byte *buffer,
                      Uint32 buflen )
{
    LogicalVolumeDescriptor *bufLvd =
                (LogicalVolumeDescriptor*) buffer;
    char *txt = "Add LVD to VDS info";      /* default */

    /* Note that LVD has a variable descriptor length: buflen.
     * For first one: vi->lvd == NULL
     */
    if( vi->lvd != NULL )       /* already one present */
    {
        if( memcmp( &bufLvd->descriptorCharacterSet,
                    &vi->lvd->descriptorCharacterSet,
                    sizeof(Charspec) + sizeof(bufLvd->logicalVolumeIdentifier) ) == 0)
        {   /* They have the same character set and identifier
             */
          if(    bufLvd->volumeDescriptorSequenceNumber
              == vi->lvd->volumeDescriptorSequenceNumber )
          {
            if( isEquivalentDescriptor(buffer, (Byte*) vi->lvd,
                            EQ_VDS_EQUAL_VDSN) ) /* ECMA 3/8.4.1 */
            {   VERBOSE00(uctout,
                    "  ==>\tIgnore equivalent LVD\n");
                return TRUE;            /* legal, ignore */
            }
            /* not equivalent, error flagged in
             * isEquivalentDescriptor(), ignore.
             */
            txt = "Replace LVD in VDS info by non-equivalent one";
          }
          else if(  bufLvd->volumeDescriptorSequenceNumber
                  < vi->lvd->volumeDescriptorSequenceNumber )
          {
            VERBOSE00(uctout,
                "  ==>\tIgnore non-prevailing LVD\n");
            return TRUE;            /* legal, ignore */
          }
          else
          { txt = "Replace LVD in VDS info by prevailing one";
          }
        }
        else    /* more than one prevailing LVD */
        {
            MLIMITbegin(ERROR00level,uctMessageLimit);
              printMessageHead( MLIMIT.vl, (Byte*) bufLvd,
                                (Byte*) &bufLvd->logicalVolumeIdentifier,
                        "Error: Multiple prevailing LVDs, UDF 2.\n"
                  "-\t\t Unequal contents of ECMA 3/8.4.3 fields.\n"
                  "-\tLogical Volume Identifiers: " );
              PRINTDSTRING( bufLvd->logicalVolumeIdentifier,
                     sizeof(bufLvd->logicalVolumeIdentifier), "\n");
              fprintf(uctout, "-\t\t\t\t  : ");
              PRINTDSTRING( vi->lvd->logicalVolumeIdentifier,
                     sizeof(vi->lvd->logicalVolumeIdentifier), "\n");
              fprintf(uctout,
                "-\tDescr Char Set Type, Info : %3u, ",
                       bufLvd->descriptorCharacterSet.characterSetType);
              printBytesName(
                       bufLvd->descriptorCharacterSet.characterSetInfo,
                sizeof(bufLvd->descriptorCharacterSet.characterSetInfo),
                       FALSE, MLIMIT.vl);
              fprintf(uctout,     "\n-\t\t\t\t  : %3u, ",
                       vi->lvd->descriptorCharacterSet.characterSetType);
              printBytesName(
                       vi->lvd->descriptorCharacterSet.characterSetInfo,
                sizeof(vi->lvd->descriptorCharacterSet.characterSetInfo),
                       FALSE, MLIMIT.vl);
              fprintf(uctout, "\n-\tNote: Identifiers can be equal but"
                                    " have different compression IDs.\n");
              if(    bufLvd->volumeDescriptorSequenceNumber
                 == vi->lvd->volumeDescriptorSequenceNumber )
              {     /* extra error, do not count extra */
                fprintf(uctout,
                  "-\tExtra error: Equal VDS Number (%u) for unequal LVDs,"
                                            " ECMA 3/8.4.1.\n",
                    bufLvd->volumeDescriptorSequenceNumber);
              }
            MLIMITend;

            txt = "Replace LVD in VDS info, unequal ECMA 3/8.4.3 fields";
        }
    }

    /* add/replace LVD, log reason
     */
    VERBOSE00(uctout, "  ==>\t%s\n", txt);

    /* always (re)allocate !!
     */
    vi->lvd = (LogicalVolumeDescriptor*) tst_realloc(
                            vi->lvd, buflen, __FILE__,__LINE__ );
    if( vi->lvd == NULL )       /* out of memory */
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    memcpy((Byte *) vi->lvd, buffer, buflen);
    return TRUE;
}

/* storeUSDInUdfVolumeInformation:
 * A single prevailing USD shall be recorded per volume.
 * UDF 2. Restrictions & ...
 */
static bool storeUSDInUdfVolumeInformation(
                      UdfVolumeInformation *vi, Byte *buffer,
                      Uint32 buflen)
{
    UnallocatedSpaceDescriptor *bufUsd =
                (UnallocatedSpaceDescriptor*) buffer;
    char *txt = "Add USD to VDS info";      /* default */

    /* Note that USD has a variable descriptor length: buflen.
     * For first one: vi->usd == NULL
     */
    if( vi->usd != NULL )       /* already one present */
    {
        if(    bufUsd->volumeDescriptorSequenceNumber
            == vi->usd->volumeDescriptorSequenceNumber )
        {
            if( isEquivalentDescriptor(buffer, (Byte*) vi->usd,
                            EQ_VDS_EQUAL_VDSN) ) /* ECMA 3/8.4.1 */
            {   VERBOSE00(uctout,
                    "  ==>\tIgnore equivalent USD\n");
                return TRUE;            /* legal, ignore */
            }
            /* not equivalent, ignore error
             */
            txt = "Replace USD in VDS info by non-equivalent one";
        }
        else if(  bufUsd->volumeDescriptorSequenceNumber
                < vi->usd->volumeDescriptorSequenceNumber )
        {
            VERBOSE00(uctout,
                "  ==>\tIgnore non-prevailing USD\n");
            return TRUE;            /* legal, ignore */
        }
        else
        {   txt = "Replace USD in VDS info by prevailing one";
        }
    }

    /* add/replace USD, log reason
     */
    VERBOSE00(uctout, "  ==>\t%s\n", txt);

    /* always (re)allocate !!
     */
    vi->usd = (UnallocatedSpaceDescriptor*) tst_realloc(
                            vi->usd, buflen, __FILE__,__LINE__ );
    if( vi->usd == NULL )       /* out of memory */
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }

    memcpy((Byte *) vi->usd, buffer, buflen);
    return TRUE;
}

/* storePDInUdfVolumeInformation:
 */
static bool storePDInUdfVolumeInformation(
                     UdfVolumeInformation *vi, Byte *buffer,
                     Uint32 buflen )
{
    PartitionDescriptor *curPd,
                        *bufPd = (PartitionDescriptor*) buffer;
    Uint16 i;
    char *txt = "Add PD to VDS info";       /* default */

    UCTASSERT( buflen == sizeof(PartitionDescriptor) );

    /* For first one: vi->pd == NULL && vi->nrPDs == 0
     */
    for( i = 0, curPd = vi->pd;
         i < vi->nrPDs;
         i++,   curPd++ )
    {                           /* already one present */
        if( bufPd->partitionNumber == curPd->partitionNumber)
        {
            if(    bufPd->volumeDescriptorSequenceNumber
                == curPd->volumeDescriptorSequenceNumber )
            {
                if( isEquivalentDescriptor(buffer, (Byte*) curPd,
                                EQ_VDS_EQUAL_VDSN) ) /* ECMA 3/8.4.1 */
                {   VERBOSE00(uctout,
                        "  ==>\tIgnore equivalent PD\n");
                    return TRUE;            /* legal, ignore */
                }
                /* non-equivalent, error flagged by
                 * isEquivalentDescriptor(), ignore.
                 */
                txt = "Replace PD in VDS info by non-equivalent one";
            }
            else if(   bufPd->volumeDescriptorSequenceNumber
                     < curPd->volumeDescriptorSequenceNumber )
            {   VERBOSE00(uctout,
                    "  ==>\tIgnore non-prevailing PD\n");
                return TRUE;            /* legal, ignore */
            }
            else
            {   txt = "Replace PD in VDS info by prevailing one";
            }

            /* replace PD, log reason
             */
            VERBOSE00(uctout, "  ==>\t%s, partition number: %lu\n",
                      txt, bufPd->partitionNumber);
            memcpy( curPd, bufPd, sizeof(PartitionDescriptor));
            return TRUE;
        }
    }

    /* add PD with new partitionNumber, log reason
     */
    VERBOSE00(uctout, "  ==>\t%s, partition number: %lu\n",
              txt, bufPd->partitionNumber);

    /* Add PD at the end of the array
     * pointed to by vi->pd (first time NULL).
     * Always (re)allocate.
     */
    vi->pd = (PartitionDescriptor*) tst_realloc( vi->pd,
                    (vi->nrPDs + 1) * sizeof(PartitionDescriptor),
                                        __FILE__,__LINE__);
    if( vi->pd == NULL )    /* out of memory */
    {   vi->nrPDs = 0;      /* maintain consistency */
        uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    /* mind that curPd is invalid now
     * because of possible reallocation.
     */
    memcpy( &(vi->pd[vi->nrPDs]),
            bufPd, sizeof(PartitionDescriptor));
    (vi->nrPDs)++;
    return TRUE;
}

/* storeIUVDInUdfVolumeInformation:
 * Keep all IUVDs, except identical ones.
 * Whether the one with the highest VDSN is prevailing
 * over the others or not, is NOT defined in ECMA or UDF.
 * For UDF, the one with the "*UDF LV Info" is important.
 */
static bool storeIUVDInUdfVolumeInformation(
                       UdfVolumeInformation *vi, Byte *buffer,
                       Uint32 buflen )
{
    ImplementationUseVolumeDescriptor *pIUVD,
                                      *bIUVD =
                (ImplementationUseVolumeDescriptor*) buffer;
    Uint32 n;
    char  *txt = "Add IUVD to VDS info";    /* default */

    UCTASSERT( buflen == sizeof(ImplementationUseVolumeDescriptor) );

#ifdef TESTING_IUVD_isEquivalentDescriptor
{ static ImplementationUseVolumeDescriptor xiuvd;
  memcpy(&xiuvd, buffer, sizeof(xiuvd));
  vi->iuvd = &xiuvd;
  vi->nrIUVDs = 1;
  for( n = 20; n < sizeof(xiuvd); n +=3 )
  { (*(buffer+n))++;    /* beyond VDS Number, every 3 */
  }
}
#endif

    /* and for first one: vi->nrIUVDs == 0 AND vi->iuvd == NULL
     */
    for( n = 0, pIUVD = vi->iuvd;
         n < vi->nrIUVDs;
         n++,   pIUVD++ )
    {
        if(    bIUVD->volumeDescriptorSequenceNumber
            == pIUVD->volumeDescriptorSequenceNumber )
        {
            if( isEquivalentDescriptor(buffer, (Byte*) pIUVD,
                                       TRUE) )  /* ECMA 3/8.4.1 */
            {   VERBOSE00(uctout,
                    "  ==>\tIgnore equivalent IUVD\n");
                return TRUE;            /* legal, ignore */
            }
            /* not equivalent, error flagged by
             * isEquivalentDescriptor(), ignore.
             */
            txt = "Add non-equivalent IUVD to VDS info";
        }
    }

    /* not equivalent, add IUVD, log reason
     */
    VERBOSE00(uctout, "  ==>\t%s\n", txt);

    vi->iuvd = (ImplementationUseVolumeDescriptor*) tst_realloc( vi->iuvd,
            (vi->nrIUVDs + 1) * sizeof(ImplementationUseVolumeDescriptor),
                __FILE__,__LINE__ );

    if( vi->iuvd == NULL )      /* out of memory */
    {   vi->nrIUVDs = 0;        /* maintain consistency */
        uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    /* mind that pIUVD is invalid now
     * because of possible reallocation.
     */
    memcpy( &(vi->iuvd[vi->nrIUVDs]), buffer,
            sizeof(ImplementationUseVolumeDescriptor) );

    (vi->nrIUVDs)++;
    return TRUE;
}


static bool storeVDSDescriptorInUdfVI( UdfVolumeInformation *vi,
                                       Uint16 tagId,
                                       Byte *buffer,
                                       Uint32 buflen )
{
    bool (*function)(UdfVolumeInformation *VI, Byte *BUF, Uint32 BLEN);

    /* VolumeDescriptorPointer no longer handled here
     * but in readVolumeDescriptorSequence().
     */
    switch(tagId)
    {
    case tidPVD:
        function = storePVDInUdfVolumeInformation;
        break;
    case tidIUVD:
        function = storeIUVDInUdfVolumeInformation;
        break;
    case tidPD:
        function = storePDInUdfVolumeInformation;
        break;
    case tidLVD:
        function = storeLVDInUdfVolumeInformation;
        break;
    case tidUSD:
        function = storeUSDInUdfVolumeInformation;
        break;
    default:
        function = NULL;
        break;
    }

    if( function == NULL )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
          "\tVolume Descriptor Sequence error:"
            " Unexpected descriptor, tag id: %u %-4s\n",
                    tagId, tidTEXT4(tagId));
      MLIMITend;
      return FALSE;
    }

#undef  VDS_TESTING_REVERSE_VDSN
#ifdef  VDS_TESTING_REVERSE_VDSN
/* change the order of volumeDescriptorSequenceNumber values
 */
{  Uint32 vdsn = *((Uint32*) (buffer + sizeof(Tag)));
    *((Uint32*) (buffer + sizeof(Tag))) = 100 - vdsn;
}
#endif  /** VDS_TESTING_REVERSE_VDSN **/

    return function(vi, buffer, buflen);

}   /* end storeVDSDescriptorInUdfVI() */

/* Check equivalence of descriptors in Main and Reserve VDS.
 *
 * return value: TRUE if both VDSs are equivalent according
 *                    to ECMA 3/8.4.2.3.
 *         else: FALSE.
 *
 * precondition: mainVi != NULL && resVi != NULL
 *
 * NOTE: Mind that if two corresponding members are NULL
 *  (e.g. mainVi->pvd and resVi->pvd) this still means that
 *  they are equivalent. So equivalence does not mean that
 *  the VDS content is correct or complete.
 */
static bool checkVdsEquivalence( UdfVolumeInformation *mainVi,
                                 UdfVolumeInformation *resVi )
{   bool result = TRUE;
    Uint32 m, r, min;

    UCTASSERT( mainVi != NULL && resVi != NULL );

    /* PVD, LVD, USD each only one prevailing descr.
     * test equivalence according to ECMA 3/8.4.2.3
     * if both are not present, the VDSs are still equivalent !!
     */
    if( !isEquivalentDescriptor((Byte*) mainVi->pvd,
                                (Byte*)  resVi->pvd,
                                 EQ_VDS_MAIN_RESERVE) )
    {   result = FALSE;
    }
    if( !isEquivalentDescriptor((Byte*) mainVi->lvd,
                                (Byte*)  resVi->lvd,
                                 EQ_VDS_MAIN_RESERVE) )
    {   result = FALSE;
    }
    if( !isEquivalentDescriptor((Byte*) mainVi->usd,
                                 (Byte*)  resVi->usd,
                                  EQ_VDS_MAIN_RESERVE) )
    {   result = FALSE;
    }

    /* Partition Descriptors:
     * There can be more than 1 PD per VDS, and
     * (theoretically) they can be in a different order in
     * the Main VDS compared to the order in the Reserve VDS.
     * There can be no 2 PDs with equal partition numbers that
     * are both in the Main VDS or both in the Reserve VDS,
     * see storePDInUdfVolumeInformation().
     * First check if nmb of PDs is equal in Main and Reserve VDS
     */
    if( mainVi->nrPDs != resVi->nrPDs )
    {   result = FALSE;
        MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError: Unequal number of prevailing PDs\n"
            "-\t       in Main VDS and Reserve VDS:"
                                " %lu, %lu, ECMA 3/8.4.2.3.\n",
            mainVi->nrPDs, resVi->nrPDs);
        MLIMITend;
    }

    /* Check equivalence of PDs in Main VDS and PDs in
     * Reserve VDS having a common partitionNumber field.
     * Flag PDs in Main VDS that are not in Reserve VDS.
     * For symmetric reasons also
     * flag PDs in Reserve VDS that are not in Main VDS.
     */
    for(     m = 0; m < mainVi->nrPDs; m++ )
    {   for( r = 0; r <  resVi->nrPDs; r++ )
        {   if(  mainVi->pd[m].partitionNumber
               == resVi->pd[r].partitionNumber )
            {   break;
            }
        }
        if( r >= resVi->nrPDs )
        { result = FALSE;           /* no match found */
          MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageHead( MLIMIT.vl, (Byte*) &mainVi->pd[m],
                    (Byte*) &mainVi->pd[m].partitionNumber, NULL);
            fprintf(uctout,
               "Error: Missing PD with Partition Number %lu in \n"
              "-\t\t\tReserve VDS, ECMA 3/8.4.2.3, %s.\n",
                mainVi->pd[m].partitionNumber,
                tidECMAREF(tidPD, getUctUdfRevision()) );
          MLIMITend;
        }
        else if( !isEquivalentDescriptor(
                        (Byte*) &mainVi->pd[m],
                        (Byte*)  &resVi->pd[r],
                         EQ_VDS_MAIN_RESERVE ) ) /* ECMA 3/8.4.2.3 */
        { result = FALSE;
        }
    }
    /* flag PDs in Reserve VDS that are not in main VDS
     */
    for(     r = 0; r <  resVi->nrPDs; r++ )
    {   for( m = 0; m < mainVi->nrPDs; m++ )
        {   if(    resVi->pd[r].partitionNumber
               == mainVi->pd[m].partitionNumber )
            {   break;
            }
        }
        if( m >= mainVi->nrPDs )
        { result = FALSE;           /* no match found */
          MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageHead( MLIMIT.vl, (Byte*) &resVi->pd[r],
                    (Byte*) &resVi->pd[r].partitionNumber, NULL);
            fprintf(uctout,
               "Error: Missing PD with Partition Number %lu in \n"
              "-\t\t\tMain VDS, ECMA 3/8.4.2.3, %s.\n",
                resVi->pd[r].partitionNumber,
                tidECMAREF(tidPD, getUctUdfRevision()) );
          MLIMITend;
        }
    }

    /* IUVDs:
     * There may be more prevailing IUVDs
     */
    if( mainVi->nrIUVDs != resVi->nrIUVDs )
    {   result = FALSE;
        MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError: Unequal number of prevailing IUVDs\n"
            "-\t       in Main VDS and Reserve VDS:"
                        " %lu, %lu, ECMA 3/8.4.2.3.\n",
            mainVi->nrIUVDs, resVi->nrIUVDs);
        MLIMITend;
    }

    /* TODO: not yet implemented: order of IUVDs maybe
     *       different in Main and Reserve VDS
     */
    min = MIN(mainVi->nrIUVDs, resVi->nrIUVDs);

    for( m = 0; m < min; m++ )
    {   if( !isEquivalentDescriptor(
                    (Byte*) &mainVi->iuvd[m],
                    (Byte*) &resVi->iuvd[m],
                     EQ_VDS_MAIN_RESERVE ) ) /* ECMA 3/8.4.2.3 */
        { result = FALSE;
        }
    }

    return result;
}


/* (re)create VDS descriptor pointer array in vi->dpa.
 */
static bool fillVdsDpa( UdfVolumeInformation *vi )
{
    Uint32 n;

    UCTASSERT(  vi != NULL );
    UCTASSERT( (vi->nrPDs   == 0) == (vi->pd   == NULL) );
    UCTASSERT( (vi->nrIUVDs == 0) == (vi->iuvd == NULL) );

    /* create VDS descriptor pointer array
     */
    vi->dpa.len = 0;    /* start with zero descriptor pointers */

    if(    vi->pvd != NULL
       && !voidPointerArrayAdd((void*) vi->pvd, &vi->dpa) )
    {   return FALSE;
    }
    if(    vi->lvd != NULL
       && !voidPointerArrayAdd((void*) vi->lvd, &vi->dpa) )
    {   return FALSE;
    }
    if(    vi->usd != NULL
       && !voidPointerArrayAdd((void*) vi->usd, &vi->dpa) )
    {   return FALSE;
    }
    for( n = 0; n < vi->nrPDs; n++ )
    {   if( !voidPointerArrayAdd((void*) &vi->pd[n], &vi->dpa) )
        {   return FALSE;
        }
    }
    for( n = 0; n < vi->nrIUVDs; n++ )
    {   if( !voidPointerArrayAdd((void*) &vi->iuvd[n], &vi->dpa) )
        {   return FALSE;
        }
    }
    return TRUE;
}

/* Check Volume information read from Main VDS or Reserve VDS.
 * - Print VDS summary and (re)create descriptor
 *   pointer array in vi->dpa.
 * - Check presence of obligatory descriptors.
 * - Check equivalence of descriptors with equal VDS Number.
 *
 * return value: FALSE if FATAL errors, else TRUE
 *
 * return value in *pDescrCount:
 *  number of prevailing desciptors in VDS.
 */
static bool verifyVdsInfo(UdfVolumeInformation *vi,
                          Uint32 *pDescrCount,
                          char   *vdsName)
{   Uint32 n, m, cntUdfIuvd;
    bool   result = TRUE;

    VERBOSE00(uctout,
        "\n====>\tCheck %s VDS. Summary:\n", vdsName);

    /* register VDS descriptors in descriptor pointer array
     */
    if( !fillVdsDpa(vi) )
    {   vi->dpa.len = *pDescrCount = 0;
        return FALSE;           /* out of memory ?? */
    }
    *pDescrCount = vi->dpa.len; /* nmb of prevailing VDS descr */

    /* print VDS summary and create VDS descriptor pointer array
     */
    if( vi->pvd != NULL )
    {   VERBOSE00(uctout,  "\t PVD VDS Number %2lu\n",
            vi->pvd->volumeDescriptorSequenceNumber);
    }
    if( vi->lvd != NULL )
    {   VERBOSE00(uctout,  "\t LVD VDS Number %2lu\n",
            vi->lvd->volumeDescriptorSequenceNumber);
    }
    if( vi->usd != NULL )
    {   VERBOSE00(uctout,  "\t USD VDS Number %2lu\n",
            vi->usd->volumeDescriptorSequenceNumber);
    }
    for( n = 0; n < vi->nrPDs; n++ )
    {   VERBOSE00(uctout,  "\t  PD VDS Number %2lu\n",
            vi->pd[n].volumeDescriptorSequenceNumber);
    }

    /* IUVD: separate count of UDF defined IUVDs
     *     (with "*UDF LV Info" EntityID Identifier)
     */
    cntUdfIuvd = 0;
    for( n = 0; n < vi->nrIUVDs; n++ )
    {   VERBOSE00(uctout,  "\tIUVD VDS Number %2lu  ID: ",
                  vi->iuvd[n].volumeDescriptorSequenceNumber);
        printBytesName(vi->iuvd[n].implementationIdentifier.Identifier,
                       ENTITYID_IDSIZE, FALSE, VERBOSE00level );
        VERBOSE00(uctout,  "\n");
        if( memcmp( vi->iuvd[n].implementationIdentifier.Identifier,
                    UDF_LVI_IUVD_ID, ENTITYID_IDSIZE ) == 0 )
        { cntUdfIuvd++;
        }
    }

    VERBOSE00(uctout, "\t %3u prevailing VDS descriptors found\n\n",
                            vi->dpa.len);

    /* Check presence of obligatory descriptors, mark if fatal;
     * PVD:
     */
    if( vi->pvd == NULL )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError:  PVD missing, UDF section 2 Basic Restrictions ...\n");
        MLIMITend;
        /* not FATAL, does not really harm,
         * no FALSE return result
         */
    }

    /* LVD, impossible to verify logical volume without LVD
     */
    if( vi->lvd == NULL )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError:  LVD missing, UDF section 2 Basic Restrictions ...\n");
        MLIMITend;
        result = FALSE;     /* FATAL */
    }

    /* USD, impossible to verify logical volume without USD
     */
    if( vi->usd == NULL )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError:  USD missing, UDF section 2 Basic Restrictions ...\n");
        MLIMITend;
        /* not FATAL, does not really harm,
         * no FALSE return result
         */
    }

    /* PD, impossible to verify logical volume without PD
     */
    if( vi->nrPDs == 0 )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError:   PD missing, UDF section 2 Basic Restrictions ...\n");
        MLIMITend;
        result = FALSE;     /* FATAL */
    }

    /* IUVD: exactly one UDF IUVD
     */
    if( cntUdfIuvd != 1 )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError: Expected exactly one IUVD with ID ");
          printBytesName((Byte*) UDF_LVI_IUVD_ID, ENTITYID_IDSIZE,
                          FALSE, VERBOSE00level );
          fprintf(uctout, ", UDF 2.2.7.\n");
        MLIMITend;
    }

    /* check equivalence of all descr with
     * identical VDS number, ECMA 3/8.4.1,
     * FALSE result from isEquivalentDescriptor()
     * is NOT fatal.
     * For all descr: VDSN field is Uint32 directly after Tag.
     */
    for( n = 0;   n < vi->dpa.len; n++ )
     for( m = n+1; m < vi->dpa.len; m++ )
    {  Uint32 vdsn1 = *((Uint32*) (((Byte*)vi->dpa.arr[n])
                                           + sizeof(Tag)));
       Uint32 vdsn2 = *((Uint32*) (((Byte*)vi->dpa.arr[m])
                                           + sizeof(Tag)));
        if( vdsn1 == vdsn2 )
        {   (void) isEquivalentDescriptor(
                        (Byte*) vi->dpa.arr[n],
                        (Byte*) vi->dpa.arr[m],
                         EQ_VDS_EQUAL_VDSN); /* for ECMA 3/8.4.1 */
        }
    }

    if( !result )
    { VERBOSE00(uctout,
        "  ==>\t%s VDS serious error%s%s.\n",
             vdsName,
            (vi->lvd == NULL) ? ", missing LVD" : "",
            (vi->pd  == NULL) ? ", missing PD"  : "");
    }
    return result;      /* only FALSE for FATAL errors */
}

/* mergeAndCheckVDSs()
 * Start with Reserve VDS and add descriptors from
 * Main VDS, using mainVi->dpa descriptor pointer array
 * that was set up by verifyVdsInfo() on Main VDS.
 * Calls verifyVdsInfo() with merged VDS.
 *
 * The reason that Main VDS descriptors arr added to
 * the Reserve VDS is that I trust the Main more.
 * In case of "non-equivalent"
 *         or "unequal ECMA 3/8.4.3 fields" descriptors,
 * a 'Reserve' descriptor will be replaced by a 'Main' one.
 */
static bool mergeAndCheckVDSs( UdfVolumeInformation *mainVi,
                               UdfVolumeInformation *resVi )
{   Uint32 n;

    VERBOSE00(uctout,
        "\n====>\tMerge VDSs, start with Reserve,"
                    " then add Main VDS descriptors.\n");

    for( n = 0; n < mainVi->dpa.len; n++ )
    {   Byte   *descr = (Byte*) mainVi->dpa.arr[n];
        Uint16  tagId = ((Tag*)descr)->tagIdentifier;
        Uint32  dLen;
        if(   !getLengthOfDescriptor(descr, TRUE, &dLen)
           || !storeVDSDescriptorInUdfVI(resVi, tagId, descr, dLen) )
        { VERBOSE00(uctout, "-\tMerge VDS error\n");
          return FALSE;
        }
    }
    /* Check Merged VDS
     */
    if( !verifyVdsInfo(resVi, &n, "Merged") )
    {   return FALSE;
    }
    return TRUE;    /* Merged VDS ok */
}


/* printAvdpLocationText():
 * check special locations:
 * print "256", "S+256", "N-256", "N", 512 or "S+512"
 * if nothing fits, then print the absolute location block address.
 */
extern void printAvdpLocationText( Uint32 location,
                                   char  *trailText )
{
    const MediumInfo *vmi       = getTheMediumInfo();
    Uint32   verifySession      =  vmi->verifySession;
    Uint32   verifySessionStart = (vmi->sessionStartBlocks)[verifySession-1];
    Uint32   N     = vmi->lastValidBlockNr;
    Uint32   Nm256 = N - 256;
    Uint32   Sp256 = verifySessionStart + 256;
    Uint32   Sp512 = verifySessionStart + 512;

    if(      location == Sp256 )
    {    if( location ==   256 ) fprintf(uctout,    "256");
         else                    fprintf(uctout,  "S+256");
    }
    else if( location == Nm256 ) fprintf(uctout, "N-256");
    else if( location == N )     fprintf(uctout, "N");
    else if( location == Sp512 )
    {    if( location ==   512 ) fprintf(uctout,   "512");
         else                    fprintf(uctout, "S+512");
    }
    else    /* the absolute address */
    {   fprintf(uctout, "%lu", location);
    }

    if( trailText != NULL )
    {   fprintf(uctout, "%s", trailText);
    }
}       /* end printAvdpLocationText() */

/* checkNmbOfAvdpsAndAvdpLocations():
 * Check number of AVDPs and AVDP locations after it is clear
 * if a virtual partition is present in which case a single
 * AVDP is possible.
 */
static bool checkNmbOfAvdpsAndAvdpLocations( UdfMountContext *mc)
{
    const MediumInfo *vmi    = getTheMediumInfo();
    Uint32  verifySession     =  vmi->verifySession,
            verifySessionStart = (vmi->sessionStartBlocks)[verifySession-1],
            lastValidBlockNr = vmi->lastValidBlockNr,
            Sp256  = verifySessionStart + 256,
            Sp512  = verifySessionStart + 512,
            n;
    char   *avdpUdfRef = "UDF 2.2.3, 6.10..., 6.13..., 6.14....";
    bool    singleAvdpOk = FALSE,
            result = TRUE;

    /* MTYPE_CL_FINAL / MTYPE_CL_INTERMEDIATE for
     * finalized / intermediate state.
     * Intermediate state only for single AVDP and
     * (virtual partition or BG status not complete).
     */
    if( mc->nmbOfAvdps > 1 )    /* finalized state */
    { (void) modifyTheMediumTypeCl(MTYPE_CL_FINAL,
                            "more than one AVDP found");
    }
    else if( mc->nmbOfAvdps == 1 )  /* maybe intermediate state */
    { bool bgfIntermediate =
            (   vmi->bgFormatStatus == 1    /* Incomplete */
             || vmi->bgFormatStatus == 2);  /* In progress */
      if(   bgfIntermediate
         || IS_PREF_PARTITION_FOUND(mc->virtualPref) )
      { /* Intermediate state.
         */
        char *txt = (bgfIntermediate)
                    ? "single AVDP and BG format status not complete"
                    : "single AVDP and sequential file system with VAT";

        (void) modifyTheMediumTypeCl(MTYPE_CL_INTERMEDIATE, txt);

        MLIMITbegin(WARN01level,uctMessageLimit);
          fprintf(uctout,
              "\n\tWarning: Intermediate state for %s.\n"
                   "-\t\t Only one AVDP at location ", txt);
          printAvdpLocationText(mc->avdpLocations[0], NULL);
          fprintf(uctout, ", %s\n", avdpUdfRef);
        MLIMITend;

        /* Check location of the intermediate state single AVDP.
         * Location must be 256 or 512,
         * the latter for virtual partition only.
         */
        if(      mc->avdpLocations[0] == Sp256
           || (   mc->avdpLocations[0] == Sp512
               && IS_PREF_PARTITION_FOUND(mc->virtualPref)) )
        { singleAvdpOk = TRUE;      /* remember */
        }
        else    /* wrong location for intermediate state single AVDP */
        { singleAvdpOk = FALSE;     /* remember */
          MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
              "\tError: Intermediate state single AVDP location must be ");
            printAvdpLocationText(Sp256, NULL);
            if( IS_PREF_PARTITION_FOUND(mc->virtualPref) )
            { fprintf(uctout, " or ");
              printAvdpLocationText(Sp512, NULL);
            }
            fprintf(uctout,
              "%s\n-\t       %s%s\n",
                (vmi->numberOfSessions > 1) ? "" : ",",
                (vmi->numberOfSessions > 1) ? "for this session, " : "",
                avdpUdfRef);
          MLIMITend;
        }
      }
    }   /* endif mc->nmbOfAvdps == 1 */


    /* nmb of AVDPs test
     */
    if(      mc->nmbOfAvdps == 0
       || (   mc->nmbOfAvdps == 1
           && vmi->closedType != MTYPE_CL_INTERMEDIATE) )
    { result = FALSE;
      MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
             "\n\tError: Number of AVDPs less than 2: %lu",
                    mc->nmbOfAvdps);
        if( mc->nmbOfAvdps == 1 )
        { fprintf(uctout, ", AVDP at ");
          printAvdpLocationText(mc->avdpLocations[0], NULL);
        }
        fprintf(uctout, "\n-\t       %s\n", avdpUdfRef);
      MLIMITend;
    }

    /* AVDP locations test
     */
    for( n = 0; n < mc->nmbOfAvdps; n++ )
    {   Uint32 loc = mc->avdpLocations[n];
        if(     loc !=  Sp256
           &&   loc != (lastValidBlockNr-256)
           &&   loc !=  lastValidBlockNr
           && !(loc ==  Sp512 && singleAvdpOk) )
        { result = FALSE;
          MLIMITbegin(ERROR00level, uctMessageLimit);
            fprintf(uctout,
              "\n\tError: Illegal AVDP location at ");
            printAvdpLocationText(loc, " for this type of media,\n");
            fprintf(uctout, "-\t       %s\n", avdpUdfRef);
          MLIMITend;
        }
    }

    return result;

}   /* end checkNmbOfAvdpsAndAvdpLocations() */

/* getAvdps(): read and verify AVDPs.
 * Check blocks S+256, N-256, N and maybe S+512 for CD-R.
 *
 * all results go to UdfMountContext fields mc->* :
 *  nmbOfAvdps      : number of found and relevant AVDP locations
 *  avdpLocations[] : these locations
 *  avdp            : pointer to first read AVDP.
 */
static bool getAvdps( UdfMountContext *mc )
{
    const MediumInfo *vmi       = getTheMediumInfo();
    Uint32   blockSize          =  vmi->blockSize;
    Uint32   verifySession      =  vmi->verifySession;
    Uint32   verifySessionStart = (vmi->sessionStartBlocks)[verifySession-1];
    Uint32   N     = vmi->lastValidBlockNr;
    Uint32   Nm256 = N - 256;
    Uint32   Sp256 = verifySessionStart + 256;
    Uint32   Sp512 = verifySessionStart + 512;
    Uint32   beginVRS;
    Uint8    cnt, cntFound;
    Byte    *readbuffer = NULL;
    bool     result;

    cnt = 0;
    beginVRS = verifySessionStart
             + ROUNDUPELEMENTS(MIN_BYTES_BEFORE_VRS, blockSize);

    /* Determine locations to be checked.
     * Check locations Sp256, N-256 and N, plus maybe S512
     * for sequential write-once in intermediate state,
     * Do not check any location twice !!
     */
    if( N >= Sp256 )
    {
        if( !uctIgnoreAVDP256 )
        { mc->avdpLocations[cnt++] = Sp256;
        }
        if( Nm256 <= beginVRS )
        { VERBOSE00(uctout, "\tNote: N-256 (%lu) is equal to,"
                    " or less than VRS location %lu.\n",
                                Nm256, beginVRS);
        }
        else if( !inArrayUint32(Nm256, mc->avdpLocations, cnt) )
        {   mc->avdpLocations[cnt++] = Nm256;
        }
    }

    if( N <= beginVRS )     /* assert */
    { VERBOSE00(uctout, "\tNote: N (%lu) is equal to,"
                    " or less than VRS location %lu.\n",
                                N, beginVRS);
    }
    else if( !inArrayUint32(N, mc->avdpLocations, cnt) )
    {   mc->avdpLocations[cnt++] = N;
    }
    /* Maybe single AVDP only on Sp256 or Sp512 for intermediate state CD-R.
     * Add Sp512 for that case. Medium type checked later.
     */
    if(     N >= Sp512
       && !inArrayUint32(Sp512, mc->avdpLocations, cnt) )
    {   mc->avdpLocations[cnt++] = Sp512;
    }
    mc->nmbOfAvdps = cnt;   /* nmb of possible locations */
                     /* later: nmb of   found  locations */

    UCTASSERT( mc->nmbOfAvdps <= MAX_AVDP_LOCATIONS );

    /* allocate memory for descriptor and read buffer
     */
    if(   (mc->avdp = (AnchorVolumeDescriptorPointer*) tst_malloc(
                sizeof(AnchorVolumeDescriptorPointer),
                           __FILE__,__LINE__)) == NULL
       || (readbuffer = (Byte*) tst_malloc(
                blockSize, __FILE__,__LINE__)) == NULL )
    {
        checkFree((void**)&mc->avdp);
        mc->nmbOfAvdps = 0;
        uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }

    /* try AVDP read on all locations
     */
    cntFound = 0;
/**patchOneAVDP**/  for( cnt = 0; cnt < mc->nmbOfAvdps; cnt++ )
/**patchOneAVDP**/
/**patchOneAVDP**   for( cnt = 0, result = FALSE;
 **patchOneAVDP**        cnt < mc->nmbOfAvdps && result == FALSE;
 **patchOneAVDP**        cnt++ )
 **patchOneAVDP**/
    {
        AnchorVolumeDescriptorPointer *AVDPread;
        Uint32 location = mc->avdpLocations[cnt];

        /* if Background format status is not complete, then no
         * attempt to read beyound last recorded block number.
         */
        if(  !uctDoReadGap
           && location > vmi->lastRecordedBlockNr
           && (   vmi->bgFormatStatus == 1          /* Incomplete */
               || vmi->bgFormatStatus == 2) )       /* In progress */
        { MLIMITbegin(WARN01level, uctMessageLimit);
            fprintf(uctout,
              "==>\tWarning: BG format status not complete, "
                                    "no attempt to read possible\n"
                       "-\t\t AVDP at %lu. Beyond last"
                            " recorded block number %lu.\n",
                    location, vmi->lastRecordedBlockNr);
            fflush(uctout);
          MLIMITend;
          continue;     /* try next location */
        }

        /* if        (location == Sp512)
         *       AND (an AVDP is found already)
         *       AND (Sp512 != N) AND (Sp512 != Nm256)
         * then: skip this location.
         */
        if(    location == Sp512
            && cntFound != 0
            && Sp512 != N
            && Sp512 != Nm256 )
        {
            continue;   /* skip location Sp512 */
        }
        /* read a block and check if an AVDP is read.
         * no context tests (mc == NULL)
         * first call inspectDescriptorHead() because it is silent
         * if no AVDP found at all.
         */
        if( deviceReadBlock(mc->device, location, 1, readbuffer,
                            FALSE)      /* NOT allowBelowVRS */
            != 1 )
        {   result = FALSE;             /* read error */
        }
        else if( (result = inspectDescriptorHead(readbuffer, blockSize,
                                        blockSize, TRUE,
                                        tidAVDP, NULL, NULL)) == FALSE )
        {   /* error, nevertheless do endian swap */
            endianSwapDescriptorTag((Tag *)readbuffer);
        }

        /* if inspectDescriptorHead() or deviceReadBlock() failed,
         * swapAndVerifyDescriptor() will not be executed.
         */
        if(   result == FALSE
           || swapAndVerifyDescriptor(readbuffer, blockSize,
                                      tidAVDP, NULL, location,
                                      NULL, NULL ) == FALSE )
        {   ifVERBOSE( INFO01level )
            {   fprintf(uctout, "\tNo AVDP at ");
                printAvdpLocationText(location, "\n");
            }
            ENDif;
            continue;           /* error, try next location */
        }

        /* AVDP accepted, copy (shift) avdp location in order to
         * end up with all found avdps in the first cntFound places
         * of mc->avdpLocations.
         * Note that mc->avdpLocations[cnt] does not change and
         * that cntFound <= cnt.
         */
        mc->avdpLocations[cntFound] = location;
        cntFound++;
        AVDPread = (AnchorVolumeDescriptorPointer *) readbuffer;

        /* Check found AVDP, check and compare to existing one in
         * mc->avdp, if any. Count all AVDPs, some maybe ignored
         * because of errors, but in this way, no new errors will
         * be introduced on the number of AVDPs. In all cases the
         * first read AVDP will be used.
         *
         * IMPLEMENTATION NOTE:
         *  If two different AVDPs are found (e.g. point to different VDS),
         *  then by default use first one, because that's what implementations
         *  typically do, see #undef USE_LAST_AVDP.
         */
#undef  USE_LAST_AVDP           /* normally #undef (default) */

        if( cntFound == 1 )     /* first one, copy to mc->avdp */
        {
            memcpy((Byte*)mc->avdp, readbuffer, sizeof(AnchorVolumeDescriptorPointer));
        }
        else if(   memcmp( &AVDPread->mainVolumeDescriptorSequenceExtent,
                           &mc->avdp->mainVolumeDescriptorSequenceExtent,
                            sizeof(ExtentAd) ) != 0
                || memcmp( &AVDPread->reserveVolumeDescriptorSequenceExtent,
                           &mc->avdp->reserveVolumeDescriptorSequenceExtent,
                            sizeof(ExtentAd) ) != 0 )
        {   MLIMITbegin(ERROR00level,uctMessageLimit);
              fprintf(uctout,
                 "\tAVDP error: Volume Descriptor Sequence Extent not equal to\n"
                      "-\t\t    the one read in first AVDP\n"
                "-\t   Main: length,location: %3lu, %-5lu expected:  %3lu, %-5lu\n"
                "-\tReserve: length,location: %3lu, %-5lu expected:  %3lu, %-5lu\n"

#ifdef  USE_LAST_AVDP   /* e.g. always use last session for multisession image file */
                "-\tUsing last read AVDP\n",
#else
                "-\tUsing first AVDP\n",
#endif
                   AVDPread->mainVolumeDescriptorSequenceExtent.extentLength,
                   AVDPread->mainVolumeDescriptorSequenceExtent.extentLocation,
                      mc->avdp->mainVolumeDescriptorSequenceExtent.extentLength,
                      mc->avdp->mainVolumeDescriptorSequenceExtent.extentLocation,
                AVDPread->reserveVolumeDescriptorSequenceExtent.extentLength,
                AVDPread->reserveVolumeDescriptorSequenceExtent.extentLocation,
                   mc->avdp->reserveVolumeDescriptorSequenceExtent.extentLength,
                   mc->avdp->reserveVolumeDescriptorSequenceExtent.extentLocation);
            MLIMITend;

#ifdef  USE_LAST_AVDP
            memcpy(mc->avdp, readbuffer, sizeof(AnchorVolumeDescriptorPointer));
#endif
        }
    }       /* endfor read avdps */

    mc->nmbOfAvdps = cntFound;  /* nmb of found AVDP locations */
    checkFree((void**)&readbuffer);

    VERBOSE00(uctout, "\n\tNumber of AVDPs: %lu", cntFound);
    if( cntFound > 0 )
    {  VERBOSE00(uctout, ", AVDP%1s at", PLURAL_S(cntFound));
        for( cnt = 0; cnt < mc->nmbOfAvdps; cnt++ )
        {   if( cnt != 0 ) VERBOSE00(uctout, ",");
            VERBOSE00(uctout, "  ");
            printAvdpLocationText(mc->avdpLocations[cnt], NULL);
        }
    }
    VERBOSE00(uctout, "\n");

    return (cntFound != 0);     /* test nmb of AVDPs later */

}   /* end getAvdps() */

/* udfGetVolumeInformation:
 * return result:
 * if ok, then: TRUE  and mc->vi != NULL
 *        else: FALSE and mc->vi unchanged (NULL).
 */
extern bool udfGetVolumeInformation(UdfMountContext *mc)
{
    UdfVolumeInformation *mainVi, *resVi = NULL;
    bool    mainReadOk, resReadOk,
            mainCheckOk, resCheckOk, vdsEquivalent;
    Uint32  mainDescrCnt, resDescrCnt, retryCnt;

    /* get AVDPs, if none found and -readgap is not on,
     * then retry with -readgap
     */
    for( retryCnt = 0; retryCnt < 2; retryCnt++ )
    { if( getAvdps(mc) )        /* AVDP found */
      { if( retryCnt != 0 )
        { MLIMITbegin(WARN01level, uctMessageLimit);
            fprintf(uctout,
              "\tWarning: AVDP found after -readgap option was activated.\n"
                   "-\t\t Recommended to repeat verification using -readgap.\n\n");
          MLIMITend;
        }
        break;      /* AVDP found */
      }
      /* No AVDP found. If uctDoReadGap not set, then
       * retry with -readgap and deviceReadCacheFree();
       */
      if( retryCnt == 0 && uctDoReadGap == FALSE )
      { uctDoReadGap = TRUE;        /* activate -readgap */
        deviceReadCacheFree();      /* force medium access */
        MLIMITbegin(WARN01level, uctMessageLimit);
          fprintf(uctout,
            "\tWarning: No AVDP found. The -readgap "
                                "option will be activated\n"
                 "-\t\t and a retry of AVDP reading will "
                                        "be started.\n\n");
        MLIMITend;
      }
      else      /* no retry or retry failed */
      { MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout, "\tFatal error: No AVDP found\n");
        MLIMITend;
        return FALSE;   /* fatal, abort */
      }
    }   /* endwhile retryCnt */

    /* check number of AVDPs later when it is known if
     * a virtual partition is present
     */

    clearUctErrorMessage();

    /* alloc and clear memory for main and reserve volume information
     */
    if(  (mainVi = (UdfVolumeInformation*)
                    tst_calloc( sizeof(UdfVolumeInformation), 1,
                                __FILE__,__LINE__)) == NULL
       || (resVi = (UdfVolumeInformation*)
                    tst_calloc( sizeof(UdfVolumeInformation), 1,
                                __FILE__,__LINE__)) == NULL )
    {   checkFree((void**)&mainVi); /* maybe allocated correctly */
        uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }

    VERBOSE00(uctout, "\n====>\tRead Main VDS extent: %7lu, length: %6lu\n",
            mc->avdp->mainVolumeDescriptorSequenceExtent.extentLocation,
            mc->avdp->mainVolumeDescriptorSequenceExtent.extentLength);
    mainReadOk = readVolumeDescriptorSequence( mc, mainVi,
            mc->avdp->mainVolumeDescriptorSequenceExtent.extentLocation,
            mc->avdp->mainVolumeDescriptorSequenceExtent.extentLength,
            TRUE );         /* isMainVds */
    if( !mainReadOk )
    {   VERBOSE00(uctout, "  ==>\tError in Main VDS\n");
    }

    VERBOSE00(uctout, "\n====>\tRead Reserve VDS extent: %7lu, length: %6lu\n",
            mc->avdp->reserveVolumeDescriptorSequenceExtent.extentLocation,
            mc->avdp->reserveVolumeDescriptorSequenceExtent.extentLength);
    resReadOk = readVolumeDescriptorSequence( mc, resVi,
            mc->avdp->reserveVolumeDescriptorSequenceExtent.extentLocation,
            mc->avdp->reserveVolumeDescriptorSequenceExtent.extentLength,
            FALSE );        /* NOT isMainVds */
    if( !resReadOk )
    {   VERBOSE00(uctout, "  ==>\tError in Reserve VDS\n");
    }

#undef  VDS_TESTING /** add Main VDS descr to Reserve VDS **/
#ifdef  VDS_TESTING /** add Main VDS descr to resVi, causes **/
                    /** non-prevailing, etc descr for OSTA_V1_D6.img **/
    VERBOSE00(uctout,
        "\n====> TESTING: Add Main VDS to Reserve VDS: %7lu, length: %6lu\n",
        pavdp->mainVolumeDescriptorSequenceExtent.extentLocation,
        mc->avdp->mainVolumeDescriptorSequenceExtent.extentLength);
    if( !(readVolumeDescriptorSequence( mc, resVi,  /* ignore result */
            mc->avdp->mainVolumeDescriptorSequenceExtent.extentLocation,
            mc->avdp->mainVolumeDescriptorSequenceExtent.extentLength,
            FALSE)) )       /* NOT isMainVds */
    {   VERBOSE00(uctout,
        "  ==> TESTING: Error adding Main VDS to Reserve VDS\n");
    }
#endif  /** VDS_TESTING**/

    /* Compare main and reserve volume info for equivalence.
     */
    VERBOSE00(uctout,
            "\n====>\tCheck equivalence of"
                            " Main VDS and Reserve VDS\n");
    vdsEquivalent = checkVdsEquivalence(mainVi, resVi);

    VERBOSE00(uctout,
            "\n  ==>\tMain and Reserve VDS are%s equivalent\n",
            (vdsEquivalent) ? "" : " NOT");

    /* check Main VDS
     * if NOT vdsEquivalent, then also check Reserve VDS.
     */
    mainCheckOk = verifyVdsInfo(mainVi, &mainDescrCnt, "Main");
    if( vdsEquivalent )     /* need no Reserve VDS check */
    {   /* mimic verifyVdsInfo(resVi) results
         */
        resCheckOk  = mainCheckOk;
        resDescrCnt = mainDescrCnt;
        if( !fillVdsDpa(resVi) )    /* fill resVi->dpa array */
        {   resVi->dpa.len = 0;
            return FALSE;           /* out of memory ?? */
        }
    }
    else    /* Check Reserve VDS */
    {   resCheckOk = verifyVdsInfo(resVi, &resDescrCnt, "Reserve");
    }

    /* select Main or Reserve by installing on mc->vi,
     * symetric approach, but select Main if equal.
     * For xx is 'main' or 'res':
     *  xxReadOk  : FALSE if errors from readVolumeDescriptorSequence().
     *  xxCheckOk : FALSE if fatal errors from verifyVdsInfo().
     *  xxDescrCnt: number of descriptors from verifyVdsInfo().
     */
    mc->vi = NULL;

    if(     mainCheckOk && !resCheckOk )       /* Main VDS ok, */
    {   mc->vi = mainVi;             /* fatal error in Reserve */
    }
    else if( resCheckOk && !mainCheckOk )   /* Reserve VDS ok, */
    {   mc->vi = resVi;                 /* fatal error in Main */
    }
    else if( mainCheckOk )        /* both have no fatal errors */
    {   /* precondition: mainCheckOk && resCheckOk
         */
        if(     mainDescrCnt == resDescrCnt )     /* cnt equal */
        {   if( mainReadOk || !resReadOk )
                 mc->vi = mainVi;
            else mc->vi = resVi;   /* resReadOk && !mainReadOk */
        }
        else if( mainDescrCnt > resDescrCnt )
             mc->vi = mainVi;
        else mc->vi = resVi;     /* resDescrCnt > mainDescrCnt */
    }

    /* check if Main or Reserve was chosen
     */
    if( mc->vi != NULL )
    {   VERBOSE00(uctout, "\n  ==>\tUsing %s VDS\n",
            (mc->vi == mainVi) ? "Main" : "Reserve");
    }
    else        /* none could be used, maybe merge */
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError: Fatal error in both Main and Reserve VDS,\n"
            "-\t       %s.\n", (vdsEquivalent)
                ? "unable to proceed, abort verification"
                : "trying merge of non-equivalent"
                                " Main and Reserve VDS");
        MLIMITend;
        /* If Main and Reserve VDS not equivalent, then
         * try to merge them and use the merged VDS.
         * merged result in Reserve VDS (resVi).
         */
        if(  !vdsEquivalent
           && mergeAndCheckVDSs(mainVi, resVi) )
        {   mc->vi = resVi;         /* merged VDS ok */
            VERBOSE00(uctout, "\n  ==>\tUsing merged VDS\n");
        }
    }

    /* clean up
     */
    if( mc->vi != mainVi )
    {   udfFreeVolumeInformation(mainVi);
        checkFree((void**)&mainVi); /* set NULL */
    }
    if( mc->vi != resVi )
    {   udfFreeVolumeInformation(resVi);
        checkFree((void**)&resVi);  /* set NULL */
    }

    return (mc->vi != NULL);

}   /* end udfGetVolumeInformation() */

extern void udfFreeVolumeInformation(UdfVolumeInformation *vi)
{
    if( vi != NULL )
    {   checkFree((void**)&(vi->pvd));
        checkFree((void**)&(vi->lvd));
        checkFree((void**)&(vi->usd));
        checkFree((void**)&(vi->pd));
        checkFree((void**)&(vi->iuvd));
        checkFree((void**)&(vi->dpa.arr));
        vi->dpa.len = vi->dpa.alen = 0;
    }
}


static void errorInDescriptorSequence(Uint16 tagId,
                                      Uint16 expectedTagId,
                                      bool   isReadError)
{
    char *txt4exp = tidTEXT4(expectedTagId);

    if( isReadError )
    {
      MLIMITbegin(WARN01level,uctMessageLimit);
        printAndClearUctErrorMessage("-");
        fprintf(uctout,
          "\tWarning: %s descriptor sequence error "
                        "(may be an unrecorded block).\n"
            "- =>\t\t The verifier will use the last "
                                "prevailing %s, if any.\n",
                      txt4exp, txt4exp);
        fflush(uctout);
      MLIMITend;
    }
    else
    { char *txt4    = tidTEXT4(tagId);
      char *txt4td  = tidTEXT4(tidTD);

      MLIMITbegin(ERROR00level,uctMessageLimit);
        printAndClearUctErrorMessage("-");
        if(   tagId == expectedTagId
           || tagId == tidTD )
             fprintf(uctout, "\tError: Incorrect %s", txt4);
        else if( tagId == tidUNKNOWN )
             fprintf(uctout, "\tError: Unknown");
        else fprintf(uctout, "\tError: Unexpected %s (tag id %u)",
                                                txt4, tagId);

        fprintf(uctout, " descriptor in %s Descriptor Sequence,\n"
             "-\t       expected: %s or %s (tag id %u or %u)\n"
          "- =>\t       The verifier will use the last prevailing %s, if any.\n",
                txt4exp, txt4exp, txt4td,
                expectedTagId, tidTD, txt4exp );
        fflush(uctout);
      MLIMITend;
    }
}

/* checkEndOfExtentInVolumeSpace():
 * check if last sector of extent still inside Volume Space
 */
static void checkEndOfExtentInVolumeSpace( Uint32 firstSector,
                                           Uint32 nmbOfSectors,
                                           char  *extentName )
{
  const MediumInfo *vmi = getTheMediumInfo();
  Uint32     lastSector = firstSector + nmbOfSectors - 1;

  if( nmbOfSectors > 0 && lastSector > vmi->lastValidBlockNr )
  { MLIMITbegin(ERROR00level,uctMessageLimit);
      fprintf(uctout,
        "\tError: Last sector %lu of %s extent beyond Volume Space,"
                    " ECMA 3/8.5.\n", lastSector, extentName);
    MLIMITend;
  }
}

/* Read Main or Reserve VDS extent and continuation extents if any.
 * In case of an error, try reading till end of extent.
 */
extern bool readVolumeDescriptorSequence(
                UdfMountContext *mc, UdfVolumeInformation *vi,
                Uint32 firstBlock, Uint32 extentLength, /* length in bytes */
                bool   isMainVds )
{
    /* Process only one extent of a Volume Descriptor sequence.
     * Stop if a block was read containing a Terminating Descriptor
     * or did not contain a descriptor at all, or extentLength was exceeded.
     */
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32      blockSize = vmi->blockSize;
    Uint16      tagId;
    Byte       *descriptor;
    bool        aDescriptorFound = FALSE,
                result = TRUE;
    Uint32      blockNr = firstBlock;       /* current block position */
    Uint32      totalBytesRead = 0;

/** TEST_VDS_CONTINUATION_ALLOCATION, see also TESTING_FAR_APART
 **/
#undef  TEST_VDS_CONTINUATION_ALLOCATION    /* normally #undef */
#ifdef  TEST_VDS_CONTINUATION_ALLOCATION
{   static count = 0;
    ExtentAd tmp;
//0 tmp.extentLocation = firstBlock;
//0 tmp.extentLength   = extentLength;
    tmp.extentLocation = 48;
    tmp.extentLength   = 2 * 2048;
    if( (count++ % 2) == 0 )    /* 1st of every 2 (Main VDS )*/
    {//1tmp.extentLocation =      147590;
//1   tmp.extentLength   = (1 + 147635 - tmp.extentLocation) * blockSize;
//2   tmp.extentLocation =      200000;
//2   tmp.extentLength   = (1 + 200009 - tmp.extentLocation) * blockSize;
    }
    else                        /* 2nd of every 2 (Reserve VDS )*/
    {//1tmp.extentLocation =      147630;
//1   tmp.extentLength   = (1 + 147650 - tmp.extentLocation) * blockSize;
//2   tmp.extentLocation =      200100;
//2   tmp.extentLength   = (1 + 200103 - tmp.extentLocation) * blockSize;
    }
    if( !addToContExtentList( &mc->contExtentList,
                    (AnyAllocationDescriptor*) &tmp,
                               ADT_EXTENT,
                 (isMainVds) ? CEI_ID_MAINVDS
                             : CEI_ID_RESVDS ) )
    {   return FALSE;
    }
}
#endif  /** TEST_VDS_CONTINUATION_ALLOCATION **/

    while( totalBytesRead < extentLength )
    {   Uint32 nrBlocksRead, dLen,
               nrBl = ROUNDUPELEMENTS(extentLength - totalBytesRead,
                                      blockSize);
        if( totalBytesRead == 0 )           /* first/new VDS extent */
        { checkEndOfExtentInVolumeSpace(blockNr, nrBl, "VDS");
        }
        /* nrBl is number of blocks left in current VDS extent
         */
        if( !readDescriptor( mc, blockNr, (Uint16) -1, nrBl,
                            &nrBlocksRead, tidUNKNOWN, &tagId,
                            (Byte **) &descriptor, NULL) )
        {   VERBOSE00(uctout, "-\tVolume Descriptor Sequence error\n");
            /* Error, no space allocated for descriptor.
             * Consider only ONE block has been read, skip it
             * and try to read next block.
             * Do not continue if already read after lastRecordedBlockNr,
             * except for -readgap.
             *
             * TODO: check if blank block (unrecorded block) ??
             *       is termination ??
             */
            blockNr++;
            totalBytesRead += blockSize;
            result = FALSE;     /* no UDF descriptor found */
            if( !uctDoReadGap && blockNr > vmi->lastRecordedBlockNr )
            {   break;          /* do not continue */
            }
            continue;           /* proceed to next block if any */
        }

        /* correct descriptor read
         */
        blockNr += nrBlocksRead;
        totalBytesRead += nrBlocksRead * blockSize;

/* seen no test image with a VDP, so fake some VDP testing
 */
#undef  VDP_TESTING     /* if testing: no test image with VDP, so change */
#ifdef  VDP_TESTING     /*    every 1st, 3rd, etc. TerminatingDescriptor */
{static count = 0;      /*                into a VolumeDescriptorPointer */
 if(   tagId == tidTD           /* TD */
    && (count++ % 2) == 0 )     /* for 1st of every 2 */
 {  VolumeDescriptorPointer *vdp = (VolumeDescriptorPointer*) descriptor;
    static Uint32 xxFirstBlock, xxExtentLength = 0;  /** undefined **/
#define VDP_TESTING_REMEMBERMAIN /** Main VDS as continuation for Reserve VDS  **/
#ifdef  VDP_TESTING_REMEMBERMAIN /** (also introduces nice 'far apart' errors) **/
    if( xxExtentLength == 0 )           /** undefined **/
#endif
    { xxFirstBlock   = firstBlock;      /** first call, normally **/
      xxExtentLength = extentLength;    /**   Main VDS extent.   **/
    }
    fprintf(uctout, "VDP_TESTING: TD 'converted' to VDP, reread first (Main) extent.\n");
    tagId = ((Tag*)vdp)->tagIdentifier = tidVDP;
    vdp->nextVolumeDescriptorSequenceExtent.extentLocation = xxFirstBlock;
    vdp->nextVolumeDescriptorSequenceExtent.extentLength   = xxExtentLength;
 }
}
#endif  /**  VDP_TESTING **/

        if(      tagId == tidTD )   /* We'r done */
        {
            free(descriptor);
            break;
        }
        else if( tagId == tidVDP )
        {   VolumeDescriptorPointer *vdp = (VolumeDescriptorPointer*) descriptor;
            /* VDS continuation extent,
             * save for later allocation tests.
             */
            if( !addToContExtentList( &mc->contExtentList,
                        (AnyAllocationDescriptor*)
                                      &vdp->nextVolumeDescriptorSequenceExtent,
                                       ADT_EXTENT,
                         (isMainVds) ? CEI_ID_MAINVDS
                                     : CEI_ID_RESVDS ) )
            {   free(descriptor);
                return FALSE;
            }
            blockNr      = vdp->nextVolumeDescriptorSequenceExtent.extentLocation;
            extentLength = vdp->nextVolumeDescriptorSequenceExtent.extentLength;
            totalBytesRead = 0;
            VERBOSE00(uctout,
                "\n\tVDS continuation extent: %lu, length: %lu\n",
                            blockNr, extentLength);
        }
        else    /* one of the other VDS descriptors */
        {
            if(   !getLengthOfDescriptor(descriptor, TRUE, &dLen)
               || !storeVDSDescriptorInUdfVI(vi, tagId, descriptor, dLen) )
            {   VERBOSE00(uctout, "-\tVolume Descriptor Sequence error\n");
                free(descriptor);   /* abort VDS reading, correct   */
                return FALSE;       /*  but unexpected descr found. */
            }
            aDescriptorFound = TRUE;    /* TD and VDP excluded */
        }
        free(descriptor);   /* descriptor copied or not needed any more */

    }   /** endwhile totalBytesRead < extentLength **/

    if( !aDescriptorFound )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout, "\tError: Empty Volume Descriptor Sequence\n");
        MLIMITend;
        return FALSE;
    }
    return result;

}   /* end readVolumeDescriptorSequence() */


/* Unmount, also used for cleanup in case an error occurred
 * during udfMountLogicalVolume(),
 * so take care that mc can be partly used.
 */
extern void udfUnmountLogicalVolume(UdfMountContext *mc)
{
    Uint16 pRef;

    checkFree((void**) &mc->lvid);
    checkFree((void**) &mc->fsd);

    if(   mc->vi != NULL && mc->vi->lvd != NULL
       && mc->partitionMapInfo != NULL )
    {   for( pRef = 0;
             pRef < mc->vi->lvd->numberOfPartitionMaps;
             pRef++ )
        {   PartitionMapInfo *pmi = &mc->partitionMapInfo[pRef];
            checkFree((void**) &pmi->vatRec.vatFile);
            checkFree((void**) &pmi->pSparingTable);
            checkFreePss(      &pmi->pss);
        }
    }
    checkFree((void**) &(mc->partitionMapInfo));    /* NULL */

    nodeFreeHierarchy(mc->rootNode);
    nodeFreeHierarchy(mc->systemStreamDirNode);
    nodeFreeHierarchy(mc->vatNode);
    mc->rootNode = mc->systemStreamDirNode = mc->vatNode = NULL;

    freeFileNodeInfo(&mc->metadataFile);
    freeFileNodeInfo(&mc->metadataMirrorFile);
    freeFileNodeInfo(&mc->metadataBitmapFile);

    udfFreeVolumeInformation(mc->vi);
    checkFree((void**) &mc->vi);

    /* No free() for other mc members because they are pointers
     * to other structures like device, etc.
     * Clear UdfMountContext, pointers to NULL, bool to FALSE, etc.
     * TODO: check possible memory leakage.
     */

}   /* end udfUnmountLogicalVolume() */

/* readLogicalVolumeIntegrityDescriptor: ECMA 3/10.10, UDF 2.2.6
 * Process the Logical Volume Integrity Descriptor (LVID) sequence, which
 * may consist of several extents. Stop if a block was read not containing
 * a LVID (this is the case if the sequence is terminated by an unrecorded
 * logical block or a Terminating Descriptor) or stop when the extent length
 * is exceeded. AFTER processing all such extents, the last LVID processed
 * is installed in mc->lvid.
 *
 * LVD must be read before the LVID, because the number
 * of partition maps must be known for LVID interpretation.
 *
 * TODO: verifyLVID();  extensive test.
 */
static bool readLogicalVolumeIntegrityDescriptor(UdfMountContext *mc)
{   const MediumInfo *vmi = getTheMediumInfo();
    Uint32       blockSize = vmi->blockSize;
    LogicalVolumeIntegrityDescriptor *lvidRead,
                                     *lvidPrevail = NULL;
    Uint16       tagId;
    Uint32       nrBlocksRead, totalBytesRead = 0;
    Uint32       extentLength, blockNr;
    bool         readOk;

    UCTASSERT( mc->vi != NULL && mc->vi->lvd != NULL );

    /* Now first test LVD Integrity Sequence extent length
     * because writability type is set now, UDF 2.2.4.5/6.
     */
    if(   mc->vi->lvd->integritySequenceExtent.extentLength < (8 * KbBYTES)
       && (   vmi->writabilityType == MTYPE_WR_OVERWRITABLE
           || vmi->writabilityType == MTYPE_WR_REWRITABLE ) )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout, "\n");
        printMessageHead(MLIMIT.vl, (Byte*) mc->vi->lvd,
            (Byte*) &mc->vi->lvd->integritySequenceExtent.extentLength, NULL);
        fprintf(uctout,
               "Error: Integrity Sequence Extent length: %lu,\n"
          "-\t\t\texpected: at least %lu bytes for overwritable and\n"
          "-\t\t\trewritable media, %s, ECMA 3/10.6.12.\n",
                mc->vi->lvd->integritySequenceExtent.extentLength,
                8 * KbBYTES, UDFREF_ISEQ(getUctUdfRevision()) );
      MLIMITend;
    }

    /* TODO: mark all blocks in extent and continuation extents
     *       as allocated using markVOLUMEAllocation() ??
     */
    blockNr      = mc->vi->lvd->integritySequenceExtent.extentLocation;
    extentLength = mc->vi->lvd->integritySequenceExtent.extentLength;

    VERBOSE00(uctout,
        "\n\tRead LVID sequence extent: %lu, length: %lu\n",
                blockNr, extentLength);

#ifdef TEST_LVID_CONTINUATION_ALLOCATION
{   ExtentAd tmp;
    tmp.extentLength   = extentLength;
    tmp.extentLocation = blockNr;
    if( !addToContExtentList( &mc->contExtentList,
                               (AnyAllocationDescriptor*) &tmp,
                               ADT_EXTENT, CEI_ID_LVID) )
    {   return FALSE;
    }
}
#endif  /** TEST_LVID_CONTINUATION_ALLOCATION **/

    while( totalBytesRead < extentLength )
    {
        Uint32 nrBl = ROUNDUPELEMENTS(extentLength - totalBytesRead,
                                      blockSize);
        if( totalBytesRead == 0 )           /* first/new LVID seq extent */
        { checkEndOfExtentInVolumeSpace(blockNr, nrBl, "Integrity Sequence");
        }
        /* nrBl is number of blocks left in current
         * LV Integrity Sequence Extent.
         */
        lvidRead = NULL;
        readOk = readDescriptor( mc, blockNr, (Uint16) -1,
                    nrBl, &nrBlocksRead,        /* absolute address */
                    tidUNKNOWN, &tagId,
                    (Byte **) (&lvidRead), NULL );  /* no node context */

        if( !readOk || tagId != tidLVID )
        {   /* (read) error or no LVID
             */
            if( !readOk || tagId != tidTD )
            {   /* (read) error or no TD
                 * This is an error. A read error or a blank block may
                 * be an unrecorded block and is flagged as a warning.
                 */
                errorInDescriptorSequence( tagId, tidLVID,
                                          !readOk );  /* isReadError */
            }
            checkFree((void**)&lvidRead);
            break;      /* (read) error or no LVID, we're done. */
        }
        /* readOk && tagId == tidLVID
         */
        if( lvidPrevail != NULL )   /* already found an LVID */
        {   free(lvidPrevail);
            VERBOSE00(uctout, "\tFound prevailing LVID\n");
        }
        lvidPrevail = lvidRead;

        if( lvidRead->nextIntegrityExtent.extentLength == 0 )
        {   blockNr += nrBlocksRead;
            totalBytesRead += nrBlocksRead * blockSize;
        }
        else
        {   /* LVID sequence continuation extent,
             * save for later allocation tests.
             */
            if( !addToContExtentList( &mc->contExtentList,
                    (AnyAllocationDescriptor*) &lvidRead->nextIntegrityExtent,
                           ADT_EXTENT, CEI_ID_LVID) )
            {   return FALSE;   /* out of memory */
            }
            blockNr      = lvidRead->nextIntegrityExtent.extentLocation;
            extentLength = lvidRead->nextIntegrityExtent.extentLength;
            totalBytesRead = 0;
            VERBOSE00(uctout,
                "\n\tLVID continuation extent: %lu, length: %lu\n",
                            blockNr, extentLength);
        }
    }

    /* check/install prevailing LVID in mount context
     */
    if( lvidPrevail == NULL )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
            "\tError: Empty Logical Volume Integrity Descriptor Sequence\n");
        MLIMITend;
        return FALSE;
    }
    mc->lvid = lvidPrevail;
    return TRUE;

}   /* end readLogicalVolumeIntegrityDescriptor() */

/* Process the File Set Descriptor sequence, which may consist of several
 * extents. Stop if a block was read not containing a FSD (this
 * is the case if the sequence is terminated by an unrecorded logical block or
 * a Terminating Descriptor) or stop when the extent length is exceeded.
 * After processing all such extents, the FSD is stored in mc which has first the
 * highest file set number and second the highest file set descriptor number in
 * the sequence.
 */
static bool readFileSetDescriptor( UdfMountContext *mc )
{
    const MediumInfo    *vmi = getTheMediumInfo();
    Uint32               blockSize = vmi->blockSize;
    Uint16               tagId, partRefNr;
    Uint32               nrBlocksRead, totalBytesRead;
    Uint32               blockNr, extentSize;
    FileSetDescriptor   *fsdRead,
                        *fsdDefault = NULL;
    LongAd              *fsdExtent;
    bool                 readOk,
                         multiFSD = FALSE;

    fsdExtent = &mc->vi->lvd->logicalVolumeContentsUse.fileSetDescriptorSequenceExtent;
    blockNr   = fsdExtent->extentLocation.logicalBlockNumber;
    partRefNr  = fsdExtent->extentLocation.partitionReferenceNumber;
    extentSize = ROUNDUPMULT(adGetExtentSize(fsdExtent), blockSize);

    /* verifyLongAd() for first FSD extent in LVD here because it could not
     * be tested in verifyLogicalVolumeDescriptor() because possible virtual
     * or metadata partition was not yet installed then.
     */
    (void) verifyLongAd( fsdExtent, (Byte*) mc->vi->lvd, mc,
                  TRUE,         /* isIntegralBlock   */
                  TRUE,         /* isFixedRecAndAlloc */
                  TRUE );       /* inMetadataPartition */

    VERBOSE00(uctout, "\n\tRead FSD sequence extent: (%lu,p%u), length: %6lu\n\n",
                        blockNr, partRefNr, extentSize);

#ifdef TEST_FSD_CONTINUATION_ALLOCATION
{   if( !addToContExtentList( &mc->contExtentList,
                               (AnyAllocationDescriptor*) fsdExtent,
                               ADT_LONG, CEI_ID_FSD) )
    {   return FALSE;
    }
}
#endif  /** TEST_FSD_CONTINUATION_ALLOCATION **/

    totalBytesRead = 0;
    while( totalBytesRead < extentSize )
    {
        Uint32 nrBl = ROUNDUPELEMENTS(extentSize - totalBytesRead,
                                      blockSize);
        /* nrBl is number of blocks left in current FSD Extent.
         */
        fsdRead = NULL;
        readOk = readDescriptor( mc, blockNr, partRefNr,
                                 nrBl, &nrBlocksRead,
                                (fsdDefault == NULL)
                                    ? tidFSD        /* first FSD */
                                    : tidUNKNOWN,   /* FSD or TD */
                                 &tagId, (Byte**)(&fsdRead),
                                 NULL );            /* no node context */

        if( !readOk || tagId != tidFSD )
        {   /* (read) error or no FSD
             */
            if( !readOk || tagId != tidTD )
            {   /* (read) error or no TD
                 * This is an error. A read error or a blank block may
                 * be an unrecorded block and is flagged as a warning.
                 */
                errorInDescriptorSequence( tagId, tidFSD,
                                          !readOk );  /* isReadError */
            }
            checkFree((void**)&fsdRead);
            break;      /* (read) error or no FSD, we're done. */
        }
        /* readOk && tagId == tidFSD
         *   prevailing FSD: equal fileSetNumber
         *                 and max fileSetDescriptorNumber, ECMA 4/8.3.1
         * default File Set:   max fileSetNumber, UDF 2.3.2
         *                     (inconsistent with ECMA 4/17.2.2, 4/18.1)
         */
        if( fsdDefault == NULL )
        {   fsdDefault = fsdRead;       /* first FSD */
        }
        else
        { if( fsdRead->fileSetNumber != fsdDefault->fileSetNumber )
          { multiFSD = TRUE;    /* more than one prevailing FSD */
            VERBOSE00(uctout, "  ==>\tMore than one File Set found\n");
          }
          if(       fsdRead->fileSetNumber >  fsdDefault->fileSetNumber /* default */
             || (   fsdRead->fileSetNumber == fsdDefault->fileSetNumber /* prevail */
                 && fsdRead->fileSetDescriptorNumber > fsdDefault->fileSetDescriptorNumber) )
          { free(fsdDefault);
            fsdDefault = fsdRead;
            VERBOSE00(uctout, "\tOverruled FSD, FSN: %lu, FSDN: %lu.\n",
                fsdRead->fileSetNumber, fsdRead->fileSetDescriptorNumber);
          }
        }

        if( adGetExtentSize(&fsdRead->nextExtent) == 0 )
        {   blockNr += nrBlocksRead;
            totalBytesRead += nrBlocksRead * blockSize;
        }
        else
        {   /* FSD sequence continuation extent,
             * save for later allocation tests.
             */
            if( !addToContExtentList( &mc->contExtentList,
                    (AnyAllocationDescriptor*) &fsdRead->nextExtent,
                             ADT_LONG, CEI_ID_FSD) )
            {   return FALSE;   /* out of memory */
            }
            blockNr  = fsdRead->nextExtent.extentLocation.logicalBlockNumber;
            partRefNr = fsdRead->nextExtent.extentLocation.partitionReferenceNumber;
            extentSize = adGetExtentSize(&fsdRead->nextExtent);
            totalBytesRead = 0;
            VERBOSE00(uctout,
                "\n\tFSD continuation extent: %lu,p%u length: %lu\n",
                            blockNr, partRefNr, extentSize);
        }

        if( fsdRead != fsdDefault )
        {   free(fsdRead);  /* for non-default/non-prevailing FSD */
        }
    }

    /* final test of the default FSD
     */
    if( fsdDefault == NULL )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout, "\tError: Empty File Set Descriptor Sequence\n");
      MLIMITend;
      return FALSE;
    }
    else
    { /* test fileSetNumber after detection of possible
       * multiple prevailing FSDs (File Sets).
       */
      if(   fsdDefault->fileSetNumber != 0
         && multiFSD == FALSE )             /* not mult File Sets */
      {
        MLIMITbegin( ERROR00level, uctMessageLimit );
          printMessageExpectedU32( MLIMIT.vl, (Byte*)fsdDefault,
            (Byte*) &fsdDefault->fileSetNumber,
              "Error: File Set Number", "%lu",
                        fsdDefault->fileSetNumber, 0,
            "\n-\t\t\tfor a single File Set, ECMA 4/8.3.1.\n" );
        MLIMITend;
      }
    }

    mc->fsd     = fsdDefault;
    return TRUE;
}

/* readSparingTable: In case of an error, mc allocated memory will be
 * freed in udfMountLogicalVolume(), using udfUnmountLogicalVolume().
 */
static bool readSparingTable(
                UdfMountContext *mc,
                PartitionMap    *pm,
                Uint32           tableNumber,   /* 0 for first one */
                Uint32          *pNextTableNumber,
                SparingTable   **pST )
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32            blockSize = vmi->blockSize;
    Uint32            stAllocBytes, dLen,
                      stAllocBlocks, blocksRead, n;
    Uint32           *pSparingTableAddress;
    Uint16           tagId;
    SparablePartitionMapTail *sTail;

    sTail = &pm->type2PartitionMap.SharedTail.sparableTail;

    if( tableNumber >= sTail->numberOfSparingTables )   /* assert */
    {   return FALSE;
    }
    *pNextTableNumber = tableNumber + 1;
    if( (*pNextTableNumber) >= sTail->numberOfSparingTables )
    {   *pNextTableNumber = 0;  /* no more after this one */
    }
    stAllocBytes   = sTail->sizeOfEachSparingTable;
    pSparingTableAddress =
      (Uint32 *) &sTail->startOfLocationsOfSparingTables;
    stAllocBlocks  = ROUNDUPELEMENTS(stAllocBytes, blockSize);

    if( tableNumber == 0 )  /* only first time */
    { VERBOSE00(uctout,
        "\n\tSparable Partition Map, Sparing Packet Length: %lu\n",
        sTail->packetLength);
    }
    VERBOSE00(uctout,
        "\t%u Sparing Table%s at:", sTail->numberOfSparingTables,
                           PLURAL_S(sTail->numberOfSparingTables));

    for( n = 0; n < sTail->numberOfSparingTables; n++ )
    {   VERBOSE00(uctout, " %lu", *(pSparingTableAddress+n));
    }
    VERBOSE00(uctout, "\n");
    pSparingTableAddress += tableNumber;    /* the one to be read */
    VERBOSE00(uctout,
        "\n\tReading Sparing Table %lu, block %lu, allocated bytes: %lu\n",
                tableNumber+1, (*pSparingTableAddress), stAllocBytes);

    if( !readDescriptor(mc, (*pSparingTableAddress), (Uint16) -1,
                        stAllocBlocks, &blocksRead,
                        tidST, &tagId, (Byte**) pST, NULL) )
    {   return FALSE;
    }
    VERBOSE00(uctout,
        "\tBlocks read for Sparing Table: %lu, allocated: %lu\n\n",
                blocksRead, stAllocBlocks);

    /* Verify sizeOfEachSparingTable (== stAllocBytes)
     * sizeOfEachSparingTable is ALLOCATED byte size.
     */
    if( !getLengthOfDescriptor((Byte*)(*pST),
                               TRUE, &dLen) ) /* isSwapped */
    {   return FALSE;
    }
    if( stAllocBytes < dLen )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        printMessageHead( MLIMIT.vl, (Byte*) mc->vi->lvd,
            (Byte*) &sTail->sizeOfEachSparingTable, NULL);
        fprintf(uctout,
                "Error: Size Of Each Sparing Table: %lu,\n"
                "-\t\t\texpected: at least %lu in Sparable\n"
          "-\t\t Partition Map RBP 44. Actual size of Sparing\n"
          "-\t\t Table size is higher than allocated size, UDF 2.2.9.\n",
                            stAllocBytes, dLen);
      MLIMITend;
    }

    return TRUE;
}

/* checkPartitionBeyondLastBlock():
 */
static void checkPartitionBeyondLastBlock(UdfMountContext *mc)
{
    const MediumInfo *vmi = getTheMediumInfo();
    PartitionMapInfo *pmi;
    Uint16            pRef, numberOfPartitionMaps;

    UCTASSERT(   mc != NULL && mc->vi != NULL && mc->vi->lvd != NULL
              && mc->partitionMapInfo != NULL );

    numberOfPartitionMaps = (Uint16) mc->vi->lvd->numberOfPartitionMaps;

    for( pRef = 0; pRef < numberOfPartitionMaps; pRef++ )
    { /* It does not make sense to test the PD partition end
       * for virtual and metadata partitions, because this is
       * tested for the underlying physical/sparable partition.
       */
      pmi = &mc->partitionMapInfo[pRef];
      if(   pmi->pMapType != PMAPTYPE_VIRTUAL
         && pmi->pMapType != PMAPTYPE_METADATA )
      { Uint32 partitionEnd = pmi->pdPointer->partitionStartingLocation
                            + pmi->pdPointer->partitionLength - 1;
        if( partitionEnd > vmi->lastValidBlockNr )
        { Uint8 vLevel; char *txt;
          if( vmi->closedType == MTYPE_CL_INTERMEDIATE )
               { vLevel = INFO01level;  txt = "Note:" ; }
          else { vLevel = ERROR00level; txt = "Error:"; }
          MLIMITbegin(vLevel, uctMessageLimit);
            fprintf(uctout,
                "\n\t%6s Partition p%lu last block: %7lu,\n"
              "-\t       beyond last valid block: %7lu, ECMA 3/5.7, 3/8.5.\n"
              "-\t       Allowed for intermediate state only.\n",
                    txt, pRef, partitionEnd, vmi->lastValidBlockNr);
          MLIMITend;            /* intermediate state means: unfinalized */
        }
      }
    }

}   /* end checkPartitionBeyondLastBlock() */


/* A virtual or metadata partition is found.
 * Determine its associated counterpart mapping partition and
 * set the appropriate PartitionMapInfo counterpartPRef fields.
 */
static bool mapPartitionFindCounterpart(UdfMountContext *mc,
                                        Uint16 partRefNmb)
{
    PartitionMapInfo *pmiPart;
    char             *partMapTxt;
    Uint16            pref, cntpPref;
    Uint32            numberOfPartitionMaps;

    if( IS_PREF_PARTITION_NOT_FOUND(partRefNmb) )
    {   return FALSE;
    }
    pmiPart    = &mc->partitionMapInfo[partRefNmb];
    partMapTxt = PMAPTYPE_TEXT(pmiPart->pMapType);
    cntpPref   = PREF_PARTITION_NOT_FOUND;
    numberOfPartitionMaps = mc->vi->lvd->numberOfPartitionMaps;

    for( pref = 0; pref < numberOfPartitionMaps; pref++ )
    {   PartitionMapInfo *pmiAsso = &mc->partitionMapInfo[pref];
        if(    pref != partRefNmb
            &&    pmiAsso->pdPointer->partitionNumber
               == pmiPart->pdPointer->partitionNumber )
        {
            if( IS_PREF_PARTITION_FOUND(cntpPref) )
            { MLIMITbegin(ERROR00level,uctMessageLimit);
                fprintf(uctout,
                    "Error: %s Partition associated with multiple"
                        " counterpart partitions\n", partMapTxt );
              MLIMITend;
              return FALSE;
            }
            if(       pmiAsso->pMapType == PMAPTYPE_PHYSICAL
               || (   pmiAsso->pMapType == PMAPTYPE_SPARABLE
                   && pmiPart->pMapType == PMAPTYPE_METADATA) )
            {   cntpPref = pref;        /* found */
            }
            else
            { MLIMITbegin(ERROR00level,uctMessageLimit);
                fprintf(uctout,
                  "Error: %s Partition must be mapped on Physical"
                                "%s Partition\n", partMapTxt,
                    (pmiPart->pMapType == PMAPTYPE_METADATA)
                                ? " or Sparable" : "");
              MLIMITend;
              return FALSE;         /* fatal */
            }
        }
    }
    if( IS_PREF_PARTITION_NOT_FOUND(cntpPref) )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout, "\tError: Missing counterpart"
                            " partition for %s Partition p%u\n",
                            partMapTxt, partRefNmb);
        MLIMITend;
        return FALSE;
    }

    UCTASSERT(   IS_PREF_PARTITION_FOUND(partRefNmb)
           && IS_PREF_PARTITION_FOUND(cntpPref)
           && cntpPref != partRefNmb
           &&    mc->partitionMapInfo[partRefNmb].pdPointer
              == mc->partitionMapInfo[cntpPref].pdPointer);

    /* counterpart partition found, let them point to each other
     * as counterpart partitions
     */
    mc->partitionMapInfo[partRefNmb].counterpartPRef = cntpPref;
    mc->partitionMapInfo[cntpPref].counterpartPRef = partRefNmb;

    return TRUE;

}   /* end mapPartitionFindCounterpart() */

/* getMetadataNextExtent():
 * return abs extents of Metadata Partition
 * (*pLengSofar) == 0 means start with first extent.
 */
static bool getMetadataNextExtent(UdfMountContext *mc,
                            Uint16  metadataPref,
                            Uint32 *pLengSofar,
                            Uint32 *pStartExtentAbs,
                            Uint32 *pEndExtentAbs)
{
    Uint32 contiguousBlocks;

    if( !translateAddress(mc, metadataPref, *pLengSofar,
                          pStartExtentAbs, &contiguousBlocks,
                          TRUE) )   /* silent */
    {   return FALSE;
    }
    (*pEndExtentAbs) = ((*pStartExtentAbs) == LBA_SPARSE_EXTENT )
                        ? LBA_SPARSE_EXTENT
                        : (*pStartExtentAbs) + contiguousBlocks - 1;
    (*pLengSofar)   += contiguousBlocks;
    return TRUE;
}

/* Check partition information and map partitions as part of mounting
 * a specific logical volume.
 * Construct pointer array which maps from a partitionReferenceNumber
 * to the corresponding PartitionDescriptor in the VolumeInformation.
 * Load VAT and/or sparing tables in case of a virtual or sparable partition.
 * In case of an error, mc allocated memory will be freed in
 * udfMountLogicalVolume(), using udfUnmountLogicalVolume().
 */
static bool checkAndMapPartitionInfo(UdfMountContext *mc)
{
    char *multipleOrMixedErrorFormat =
       "\tError: Multiple occurence of %s Partition Map or illegal\n"
      "-\t       combination with Virtual, Sparable or Metadata Partition Map.\n";

    const MediumInfo *vmi = getTheMediumInfo();
    Uint32      blockSize = vmi->blockSize;
    UdfVolumeInformation    *vi = mc->vi;
    PartitionDescriptor     *pd1, *pd2;
    Byte                    *bpPmap;        /* part map byte pointer */
    MediumWritabilityType    typeWr = MTYPE_WR_UNKNOWN;
    Uint32  numberOfPartitionMaps = vi->lvd->numberOfPartitionMaps;
    Uint32  j, mapSize, expPDs, accessType, nextTableNumber;
    Uint16  pref;
    bool    partitionFound,
            nonReadOnlyPartitionFound = FALSE;

    if(   vi->nrPDs == 0
       || numberOfPartitionMaps == 0 )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError: Number of Partition Descriptors: %2lu,\n"
            "-\t       Number of Partition Maps       : %2lu,\n"
            "-\t       expected: at least 1 of each.\n",
                    vi->nrPDs, numberOfPartitionMaps);
        MLIMITend;
        return FALSE;
    }

    /* At least one Partition Decsriptor and one Partition Map
     * Show PDs and check their +NSR0x Partition Contents
     */
    VERBOSE00(uctout, "\tPrevailing Partition Descriptors:\n");
    for( j = 0; j < vi->nrPDs; j++ )
    {
        pd1 = &(vi->pd[j]);
        VERBOSE00(uctout,
          "\t  pNmb: %4u, start: %5lu, length: %7lu, access: %s\n",
            pd1->partitionNumber, pd1->partitionStartingLocation,
            pd1->partitionLength, PDAT_TEXT(pd1->accessType));

        if(      memcmp( pd1->partitionContents.Identifier,
                         PARTCONTENTS_NSR02_ID, ENTITYID_IDSIZE) == 0 )
        {
            char *txt = "+NSR02 Partition Contents Identifier";
            modifyUdfRevisionRange( MIN_UDFREVISION, 0x150, txt );  /* 1.50- */
        }
        else if( memcmp( pd1->partitionContents.Identifier,
                         PARTCONTENTS_NSR03_ID, ENTITYID_IDSIZE) == 0 )
        {
            char *txt = "+NSR03 Partition Contents Identifier";
            modifyUdfRevisionRange( 0x200, MAX_UDFREVISION, txt );  /* 2.00+ */
        }
        else    /* no "+NSR02" or "+NSR03" found */
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            fprintf(uctout,
                "\t   Error: Found non-UDF PD Partition Contents Identifier:\n"
                      "\t\t  ");
            printBytesName((Byte*)pd1->partitionContents.Identifier,
                            ENTITYID_IDSIZE, FALSE, MLIMIT.vl);
            fprintf(uctout,
              "\n\texpected: \"%s\" Identifier,"
                    " see %s.1, 2.1.5.2.\n",
                (getUctUdfRevision()>= 0x200) ? PARTCONTENTS_NSR03_ID
                                              : PARTCONTENTS_NSR02_ID,
                UDFREF_PD(getUctUdfRevision()));
          MLIMITend;
        }
    }
    VERBOSE00(uctout, "\n");

    /* Allocate memory for partitionMapInfo[],
     * 1 for each partition map.
     * Use tst_calloc(), so all embedded pointers are NULL,
     * bools FALSE, etc.
     */
    if( (mc->partitionMapInfo = NEWSTRUCT(PartitionMapInfo,
                                          numberOfPartitionMaps))
         == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }

    /* mc->virtualPref, mc->sparablePref and mc->metadataPref are
     * already initialized as PREF_PARTITION_NOT_FOUND.
     * Mind that LVD numberOfPartitionMaps and mapTableLength
     * consistency is already checked when reading the LVD
     * (see endianSwapLogicalVolumeDescriptor()).
     */
    VERBOSE00(uctout, "\tLVD Partition Maps:\n");
    for( pref = 0,  bpPmap = &vi->lvd->startOfPartitionMaps;
         pref < numberOfPartitionMaps;
         pref++,    bpPmap += mapSize )
    {
        PartitionMap     *pm = (PartitionMap*) bpPmap;
        PartitionMapInfo *pmi = &mc->partitionMapInfo[pref];
        Uint16 partitionNumber = 0, volumeSequenceNumber;
        /* set default partitionMapInfo values
         */
        pmi->pMapType = PMAPTYPE_UNKNOWN;
        pmi->pPartitionMap = pm;
        pmi->counterpartPRef = pref;            /* default */
        pmi->calculatedFreeSpace = MAX_UINT32;  /* not yet known */
        switch( pm->genericPartitionMap.partitionMapType )
        {
        case PMAP_TYPE1:
            volumeSequenceNumber =
                              pm->type1PartitionMap.volumeSequenceNumber;
            partitionNumber = pm->type1PartitionMap.partitionNumber;
            mapSize = sizeof(Type1PartitionMap);
            pmi->pMapType = PMAPTYPE_PHYSICAL;
            VERBOSE00(uctout, "\t  p%u: %8s Partition Map (Type 1), pNmb: %4u\n",
                            pref, PMAPTYPE_TEXT(pmi->pMapType), partitionNumber);
            break;
        case PMAP_TYPE2:
            {   Byte *id = pm->type2PartitionMap.partitionTypeIdentifier.Identifier;
                volumeSequenceNumber =
                                  pm->type2PartitionMap.volumeSequenceNumber;
                partitionNumber = pm->type2PartitionMap.partitionNumber;
                mapSize = sizeof(Type2PartitionMap);
                if(      memcmp(id, VIRTUAL_PARTITION_ID, ENTITYID_IDSIZE) == 0 )
                {
                    pmi->pMapType = PMAPTYPE_VIRTUAL;
                }
                else if( memcmp(id, SPARABLE_PARTITION_ID, ENTITYID_IDSIZE) == 0)
                {
                    pmi->pMapType = PMAPTYPE_SPARABLE;
                }
                else if( memcmp(id, METADATA_PARTITION_ID, ENTITYID_IDSIZE) == 0 )
                {
                    pmi->pMapType = PMAPTYPE_METADATA;
                }
                /* else: pmi->pMapType == PMAPTYPE_UNKNOWN
                 */
                VERBOSE00(uctout,
                    "\t  p%u: %8s Partition Map (Type 2), pNmb: %4u\n",
                        pref, PMAPTYPE_TEXT(pmi->pMapType), partitionNumber);

                switch( pmi->pMapType )
                {
                case PMAPTYPE_VIRTUAL:
                    if(   IS_PREF_PARTITION_FOUND(mc->virtualPref)
                       || IS_PREF_PARTITION_FOUND(mc->metadataPref)
                       || IS_PREF_PARTITION_FOUND(mc->sparablePref) )
                    {   MLIMITbegin(ERROR00level,uctMessageLimit);
                            fprintf(uctout, multipleOrMixedErrorFormat,
                                        PMAPTYPE_TEXT(pmi->pMapType));
                        MLIMITend;
                        return FALSE;       /* fatal */
                    }
                    mc->virtualPref = pref; /* found Virtual Partition */
                    /* read VAT later when counterpart partition is found
                     */
                    break;

                case PMAPTYPE_SPARABLE:
                    if(   IS_PREF_PARTITION_FOUND(mc->virtualPref)
                       || IS_PREF_PARTITION_FOUND(mc->sparablePref) )
                    {   MLIMITbegin(ERROR00level,uctMessageLimit);
                          fprintf(uctout, multipleOrMixedErrorFormat,
                                        PMAPTYPE_TEXT(pmi->pMapType));
                        MLIMITend;
                        return FALSE;   /* fatal, virtual or another sparable */
                    }
                    mc->sparablePref = pref;  /* found Sparable Partition */
                    break;

                case PMAPTYPE_METADATA:
                    if(   IS_PREF_PARTITION_FOUND(mc->metadataPref)
                       || IS_PREF_PARTITION_FOUND(mc->virtualPref) )
                    {   MLIMITbegin(ERROR00level,uctMessageLimit);
                            fprintf(uctout, multipleOrMixedErrorFormat,
                                        PMAPTYPE_TEXT(pmi->pMapType));
                        MLIMITend;
                        return FALSE;   /* fatal */
                    }
                    mc->metadataPref = pref;    /* found Metadata Partition */
                    /* more checks later when counterpart partition is found
                     */
                    break;

                case PMAPTYPE_UNKNOWN:  /* fall through */
                default:
                  MLIMITbegin(ERROR00level,uctMessageLimit);
                    fprintf(uctout,
                        "\tError: Undefined type 2 partition map\n");
                  MLIMITend;
                  return FALSE;         /* fatal */
                  /** break; **/
                }           /* end switch(pmi->pMapType) */
            }
            break;  /* end case PMAP_TYPE2 */

        default:        /* undefined map Type */
            VERBOSE00(uctout,
                "\t  p%u: %8s Partition Map (Type unknown), pNmb: %4u\n",
                        pref, "unknown", partitionNumber);
            MLIMITbegin(ERROR00level,uctMessageLimit);
              fprintf(uctout,
                "\tError: Partition map type: %u, shall be 1 or 2\n",
                            pm->genericPartitionMap.partitionMapType);
            MLIMITend;
            return FALSE;
            /** break; **/
        }

        if( volumeSequenceNumber != 1 ) /* ignore partition */
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            fprintf(uctout,
               "\tSerious error: Partition Map Volume Sequence Number:"
                                    " %lu, expected: 1.\n"
              "-\tThe verifier does not support multi-volume"
                                    " Volume Sets, sorry.\n"
              "- ====>\tTrying to continue assuming that a value 1 was intended.\n"
              "- ====>\tThis may cause other (fatal) errors.\n",
                                volumeSequenceNumber);
          MLIMITend;
          /** no return FALSE; **/
        }

        if( mapSize != pm->genericPartitionMap.partitionMapLength)
        {
            return FALSE; /* checked in verifyLogicalVolumeDescriptor() */
        }

        /* Find PD with same partition number in vi->pd[]
         */
        partitionFound = FALSE;
        for( j = 0; j < vi->nrPDs; j++ )
        {
            if( partitionNumber == vi->pd[j].partitionNumber )
            {   /* connect Partitioon Map to PD in vi->pd[j]
                 */
                partitionFound = TRUE;
                break;      /* found one, abort search for PD */
            }
        }       /* endfor */
        if( !partitionFound )
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            fprintf(uctout,
                 "\tSerious error: Partition Number in Partition Map:"
                                        " %lu, but no Partition\n"
                "-\t      Descriptor with Partition Number %u,"
                                            " ECMA 3/10.7.2.4.\n"
                "-\t      THIS IS A SERIOUS ERROR !!!!\n",
                        partitionNumber, partitionNumber);
            if( vi->nrPDs == 1 )
            { fprintf(uctout,
                "-\tNote: Because there is exactly one Partition Descriptor, the\n"
                "-\t      verifier will assume that this Partition Descriptor\n"
                "-\t      with Partition Number %lu must be used.\n",
                            vi->pd[0].partitionNumber);
            }
          MLIMITend;
          if( vi->nrPDs != 1 )
          { return FALSE;       /* abort */
          }
          partitionFound = TRUE;
          j = 0;        /* assume use of first and only PD */
        }
        /* if found, connect Partition Map with
         * PD in vi->pd[j].
         */
        if( partitionFound )
        {   pmi->pdPointer = &(vi->pd[j]);
            /* For the moment, pmi->actualPartitionLength will be set to
             * the PD partitionLength. Later, corrections will be made
             * for a virtual and for a metadata partition.
             */
            pmi->actualPartitionLength = pmi->pdPointer->partitionLength;
            /* avoid double test of PD Partition Flags
             * in case of a virtual or metadata partition
             * (2 maps with same partitionNumber).
             */
            if(   pmi->pMapType != PMAPTYPE_VIRTUAL
               && pmi->pMapType != PMAPTYPE_METADATA )
            { verifyPdPartitionFlags(pmi, pref);
            }
        }

        /* set File System writability type
         * remenber last non-read-only access type.
         * 'translate' from partition access type PDAT_ value
         *               to medium writability MTYPE_ value.
         */
        accessType = pmi->pdPointer->accessType;
        if( accessType != PDAT_READONLY )
        {   nonReadOnlyPartitionFound = TRUE;
            typeWr =
                (accessType == PDAT_WRITEONCE)    ? MTYPE_WR_WRITEONCE
              : (accessType == PDAT_REWRITABLE)   ? MTYPE_WR_REWRITABLE
              : (accessType == PDAT_OVERWRITABLE) ? MTYPE_WR_OVERWRITABLE
              : (   accessType == PDAT_POW_OR_UNKNOWN
                 && getUctUdfRevision() >= 0x260) ? MTYPE_WR_POW
                                                  : MTYPE_WR_UNKNOWN;
        }
    }       /* endfor */
    VERBOSE00(uctout, "\n");

    /* determine 'medium' writability and sequential type
     */
    (void) modifyTheMediumTypeWr(
                    (nonReadOnlyPartitionFound)
                                ? typeWr
                                : MTYPE_WR_READONLY,
                    "partition access type" );

    /* The presence of a virtual partion determines
     * whether a UDF file system is sequential or not.
     */
    (void) modifyTheMediumTypeSe(
                    IS_PREF_PARTITION_FOUND(mc->virtualPref)
                                ? MTYPE_SE_SEQUENTIAL
                                : MTYPE_SE_NONSEQUENTIAL,
                    IS_PREF_PARTITION_FOUND(mc->virtualPref)
                                ? "Virtual Partition found"
                                : "no Virtual Partition found");
    VERBOSE00(uctout, "\n");

    /* pMapType in place now. Do some more tests and
     * read Sparing Table for a sparable partition.
     * TODO: Check if we really can handle multiple sparable partitions,
     *       excluded for the moment !!
     */
    for( pref = 0; pref < numberOfPartitionMaps; pref++ )
    {   PartitionMapInfo *pmi = &mc->partitionMapInfo[pref];

        /* verify partition alignment
         */
        (void) verifyPdPartitionAlignment(mc, pref);

        /* check sparing tables now
         */
        if( pmi->pMapType == PMAPTYPE_SPARABLE )
        {   /* read first sparing table
             */
            if( !readSparingTable( mc, pmi->pPartitionMap, 0,
                                  &nextTableNumber,
                                  &pmi->pSparingTable) )
            {   VERBOSE00(uctout, "-\tError reading Sparing Table 1.\n");
                return FALSE;
            }

            /* check more tables, if any
             */
            while( nextTableNumber != 0 )
            {   SparingTable *spt;
                Uint32        tableNumber = nextTableNumber;
                VERBOSE00(uctout,
                  "  ==>\tSparing Table %lu,"
                        " check equivalence with first one.\n",
                            1 + tableNumber);
                if( !readSparingTable( mc, pmi->pPartitionMap,
                                       tableNumber,
                                      &nextTableNumber, &spt) )
                { VERBOSE00(uctout,
                    "-\tError reading Sparing Table %lu,"
                            " ignore further tables.\n",
                            1 + tableNumber);
                    nextTableNumber = 0;    /* ignore further tables */
                    spt = NULL;             /* no memory allocation */
                }
                else
                { bool ok;   /* messages in isEquivalentDescriptor() */
                  ok = isEquivalentDescriptor(  /* check equivalence */
                        (Byte*) spt,
                        (Byte*) pmi->pSparingTable,
                         EQ_SPARINGTABLES);
                  VERBOSE00(uctout,
                        "\tSparing Table %sequivalent with first one.\n",
                                    (ok) ? "" : "not ");
                          /* ignore result */
                }
                VERBOSE00(uctout, "\n");
                checkFree((void**)&spt);
            }
        }
    }       /* endfor */

    /* Conditions here:
     * No combination of virtual and metadata partition exists.
     * PartitionMapInfo for physical and sparable partition is in
     * place now, so that address translations for logical blocks in
     * a physical or a sparable partition are operable which means
     * that VAT file and Metadata files are readable now (if any).
     * If a virtual or metadata partition is found then determine
     * its associated counterpart mapping partition and read
     * VAT file or Metadata File(s);
     */
    if(    uctUseMetadataMirror     /* -usemirror option */
       && !IS_PREF_PARTITION_FOUND(mc->metadataPref) )
    { MLIMITbegin(WARN01level, uctMessageLimit);
        fprintf(uctout,
              "\tWarning: Ignore -usemirror option because "
                            "no Metadata Partition present.\n");
      MLIMITend;
      uctUseMetadataMirror = FALSE;     /* ignore */
    }

    /* read VAT or Metadata files, if any.
     */
    if(   IS_PREF_PARTITION_FOUND(mc->virtualPref)
       || IS_PREF_PARTITION_FOUND(mc->metadataPref) )
    {   /* set PartitionMapInfo counterpartPRef values for
         * virtual OR metadata partion.
         */
        Uint16 pRef = (Uint16)((IS_PREF_PARTITION_FOUND(mc->virtualPref))
                                    ? mc->virtualPref
                                    : mc->metadataPref);
        if( !mapPartitionFindCounterpart(mc, pRef) )
        {   return FALSE;   /* fatal */
        }
        /* counterpart partition known now, read
         * VAT file or Metadata File.
         * The PartitionMapInfo actualPartitionLength is
         * now updated for a metadata (or virtual??) partition.
         */
        if(   (    IS_PREF_PARTITION_FOUND(mc->virtualPref)
               && !readVAT(mc, mc->virtualPref))
           || (    IS_PREF_PARTITION_FOUND(mc->metadataPref)
               && !readMetadataFiles(mc, mc->metadataPref)) )
        {   return FALSE;       /* fatal */
        }
    }
    VERBOSE00(uctout,"\n");

    /* Show information about the mounted partitions.
     */
    VERBOSE00(uctout, "\tMounted Partitions:\n");
    for( pref = 0; pref < numberOfPartitionMaps; pref++ )
    {   PartitionMapInfo *pmi = &mc->partitionMapInfo[pref];
        Uint32  startAbs, endAbs, blocksSofar;

        pd1        = pmi->pdPointer;
        accessType = pd1->accessType;

        /* Show start address, end address, etc.
         * Cumbersome for metadata partition because it
         * can have more than one extent.
         * Defaults for non-metadata partition:
         */
        startAbs = pd1->partitionStartingLocation;
        endAbs   = startAbs + pmi->actualPartitionLength - 1;

        blocksSofar = 0;
        if( pmi->pMapType == PMAPTYPE_METADATA )
        { Uint32 start, end;
          if( getMetadataNextExtent( mc, pref,
                                    &blocksSofar,
                                    &start, &end) )
          { startAbs = start;
            endAbs = end;
          }
          else  /* getMetadataNextExtent() error */
          { /* leave startAbs and endAbs defaults and avoid
             * calling getMetadataNextExtent() again.
             */
             blocksSofar = pmi->actualPartitionLength;
          }
        }
        VERBOSE00(uctout, "-\tp%lu: %8s, pNmb: %4u, blocks: ",
            pref, PMAPTYPE_TEXT(pmi->pMapType), pd1->partitionNumber);
        if( pmi->pMapType == PMAPTYPE_VIRTUAL )
             VERBOSE00(uctout, "    *** VIRTUAL  ***");
        else if( startAbs == LBA_SPARSE_EXTENT )
             VERBOSE00(uctout, "*** SPARSE (%8lu)", blocksSofar);
        else VERBOSE00(uctout,   "%7lu thru %7lu", startAbs, endAbs);

        VERBOSE00(uctout, ", access: %s", PDAT_TEXT(accessType));

        if( pmi->pMapType == PMAPTYPE_METADATA )
        { /* check if more than one extent in metadata file.
           */
          while( blocksSofar < pmi->actualPartitionLength )
          { Uint32 tmpLen = blocksSofar;
            VERBOSE00(uctout,
                      "\n-\t\t\t\t     and: ");
            /* by default followed by:      "%7lu thru %7lu"
             */
            if( !getMetadataNextExtent( mc, pref,
                                       &blocksSofar,
                                       &startAbs, &endAbs) )
            { VERBOSE00(uctout, "???????");
              MLIMITbegin(ERROR00level, uctMessageLimit);
                fprintf(uctout,
                  "\n\tError: getMetadataNextExtent() error, "
                                    "please report: %lu, %lu\n",
                    blocksSofar, pmi->actualPartitionLength);
              MLIMITend;
              break;    /* getMetadataNextExtent error */
            }
            if( startAbs == LBA_SPARSE_EXTENT )
            { VERBOSE00(uctout, "*** SPARSE (%8lu blocks)",
                    blocksSofar - tmpLen);
            }
            else    /* normal case */
            { VERBOSE00(uctout, "%7lu thru %7lu", startAbs, endAbs);
            }
          }     /* endwhile */
        }
        VERBOSE00(uctout,
            "\n-\t\t\t  %s blocks: %7lu thru %7lu\n",
            (pmi->pMapType == PMAPTYPE_VIRTUAL) ? "virtual"
                                                : "logical",
                    0, (pmi->actualPartitionLength == 0)
                        ? 0 : pmi->actualPartitionLength - 1);

        /* warn for huge partition sizes
         * (not for virtual and metadata partition)
         */
        if(    pmi->pMapType != PMAPTYPE_VIRTUAL
           &&  pmi->pMapType != PMAPTYPE_METADATA
           && ((Uint64) pmi->pdPointer->partitionLength * blockSize)
                     >= UCT_MAXPARTITION_BYTESIZE )
        { MLIMITbegin(WARN01level, uctMessageLimit);
            fprintf(uctout,
              "\tWarning: This is a huge partition of more"
                " than %lu Gbyte !!\n", UCT_MAXPARTITION_GBSIZE);
          MLIMITend;
        }
    }
    VERBOSE00(uctout, "\n");

    (void) verifyMetadataFilesAllocation(mc);

    /* check write-once for virtual partition, warning because:
     * where is this in the spec ?? TODO: find it ??
     */
    if( IS_PREF_PARTITION_FOUND(mc->virtualPref) )
    {   Uint32 pat = mc->partitionMapInfo[mc->virtualPref].pdPointer->accessType;
        if( pat != PDAT_WRITEONCE )
        {   MLIMITbegin(WARN01level,uctMessageLimit);
              fprintf(uctout,
                "==>\tWarning: Virtual Partition mapped on \"%s\" partition\n",
                                PDAT_TEXT(pat));
            MLIMITend;
        }
    }

    /* check number of PDs, partition overlap, etc.
     * calculate expected nmb of PDs (expPDs), based
     * on number of partition maps and map type.
     * A Virtual Partition Map shares a PD with
     * a non-virtual Physical (== Type 1) Partition Map.
     * A Metadata Partition Map shares a PD with
     * a Physical or a Sparable Partition Map.
     *
     * Condition met here: No Virtual Partition Map and
     *  Metadata Partition Map are present together.
     */
    expPDs = numberOfPartitionMaps;     /* init */
    if( IS_PREF_PARTITION_FOUND(mc->virtualPref) ) expPDs--;
    if( IS_PREF_PARTITION_FOUND(mc->metadataPref) ) expPDs--;
    UCTASSERT( expPDs >= 1 );

    if( vi->nrPDs != expPDs )
    {   Uint8 vLev = (vi->nrPDs > expPDs)   /* unused PDs */
                ? WARN01level       /* global warning: (count) */
                : ERROR00level;     /* global   error: (count) */
        MLIMITbegin(vLev, uctMessageLimit);
          fprintf(uctout,
             "\t%8s Number of prevailing Partition"
                        " Descriptors: %lu,\n"
            "-\t\t expected: %lu for %lu partition map%s%s,\n%s"
            "-\t\t UDF section 2 Basic Restrictions ...\n",
              (vLev == ERROR00level) ?   "Error:"
                                     : "Warning:",
                     vi->nrPDs, expPDs,
                     numberOfPartitionMaps,
            PLURAL_S(numberOfPartitionMaps),
            (IS_PREF_PARTITION_FOUND(mc->virtualPref))
                ?     " (one virtual map)"
                : (IS_PREF_PARTITION_FOUND(mc->metadataPref))
                    ? " (one metadata map)" : "",
            (vi->nrPDs > expPDs) ?
            "-\t\t unused Partition Descriptors,\n" : "");
        MLIMITend;
    }

    /* further checks for expPDs >= 2
     */
    if( expPDs > 2 )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError: Too many Physical plus Sparable Partitions"
                            " defined in LVD: %lu,\n"
            "-\t       expected: at most 2,"
                        " UDF 2., Partition Descriptor.\n"
            "-\t Note: Partition overlap testing will be skipped.\n",
            expPDs);
        MLIMITend;
    }
    else if( expPDs == 2 )          /* special two-partition case, see UDF 2. */
    {   Uint32 oStart, oBlocks;
        Uint16 x1, x2;              /* first check overlap */
        /* find partition reference numbers x1 and x2 for the two PDs of
         * non-virtual and non-metedata partitions (virtual and metadata
         * partitions always share a PD with some other partition).
         */
        x1 = 0;                     /* first partition map */
        if( x1 == mc->virtualPref )  x1++;
        if( x1 == mc->metadataPref ) x1++;
        x2 = (Uint16) (x1 + 1);
        if( x2 == mc->virtualPref )  x2++;
        if( x2 == mc->metadataPref ) x2++;
        UCTASSERT( x2 < numberOfPartitionMaps );

        pd1 = mc->partitionMapInfo[x1].pdPointer;
        pd2 = mc->partitionMapInfo[x2].pdPointer;

        if( calculateOverlap( pd1->partitionStartingLocation,
                              pd1->partitionLength,
                              pd2->partitionStartingLocation,
                              pd2->partitionLength,
                              &oStart, &oBlocks ) )
        {
          MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
               "\tError: Overlapping partitions p%u and p%u"
                                    " (location %lu thru %lu),\n"
              "-\t       UDF section 2 Basic Restrictions ...,\n"
              "-\t       UDF 2.01 errata, ECMA 3/8.7, Note 10.\n",
                x1, x2, oStart, oStart + oBlocks - 1);
          MLIMITend;
        }

        /* check access type of special two-partitions case.
         * Mind that access type of each PD has been tested
         * already in verifyPartitionDescriptor().
         * Exactly one must be read-only and the other must
         * NOT be pseudo-overwritable (UDF 2.60+).
         */
        if(   (   pd1->accessType == PDAT_READONLY
               && pd2->accessType == PDAT_READONLY)
           || (   pd1->accessType != PDAT_READONLY
               && pd2->accessType != PDAT_READONLY)
           ||    pd1->accessType == PDAT_POW_OR_UNKNOWN
           ||    pd2->accessType == PDAT_POW_OR_UNKNOWN )
        {
            MLIMITbegin(ERROR00level,uctMessageLimit);
              fprintf(uctout,
                   "\tError: Partition Access Type error for special"
                                    " two-partitions case.\n"
                  "-\t       Found %s partition p%u and %s partition p%u.\n"
                  "-\t       Exactly one partition shall have Access Type %s.\n",
                        PDAT_TEXT(pd1->accessType), x1,
                        PDAT_TEXT(pd2->accessType), x2,
                        PDAT_TEXT(PDAT_READONLY));
              if( getUctUdfRevision() >= 0x260 )
              { fprintf(uctout,
                  "-\t       No partition shall have Access Type %s.\n",
                        PDAT_TEXT(PDAT_POW_OR_UNKNOWN));
              }
              fprintf(uctout,
                "-\t       UDF section 2 Basic Restrictions ...\n");
            MLIMITend;
        }
    }

    /* Now check rules for presence and Access Type of a Metadata Partition:
     * UDF 2.50+ 2.2.10: A Metadata Partition Map shall be recorded for
     *  volumes that contain a single partition having an access type of
     *  1 (read only) or 4 (overwritable). It shall not be recorded in
     *  all other cases. Also the combination of a metadata partition and
     *  a virtual partition is excluded, see UDF 2.60 and UDF 2.50 errata
     *  DCN-5101 and reflector discussion July 2003.
     *  Further, UDF 2.50 errata DCN-5101 requires a Metadata Partition
     *  for an overwritable partition in the special two-partition (PD)
     *  case (combination with a read-only partition).
     * UDF 2.60+: A Metadata Partition Map shall be recorded for a
     *  pseudo-overwritable (POW) Partition. The special two-partition
     *  case of a read-only and a pseudo-overwritable partition is not
     *  allowed. This is already tested above.
     */
    if( getUctMinUdfRevision() >= 0x250 )               /* UDF 2.50+ */
    {   bool foundMeta, foundVirtual, foundOV, foundRO, foundPOW,
             expectMeta, metaAccessOk;
        Uint32 metaAccessType;
        /* prepare for decision on required presence and
         * access type of a metadata partition.
         */
        foundMeta    = IS_PREF_PARTITION_FOUND(mc->metadataPref);
        foundVirtual = IS_PREF_PARTITION_FOUND(mc->virtualPref);
        foundOV = foundRO = foundPOW = FALSE;
        for( pref = 0; pref < numberOfPartitionMaps; pref++ )
        {   Uint32 acc = mc->partitionMapInfo[pref].pdPointer->accessType;
            if( acc == PDAT_OVERWRITABLE )       foundOV = TRUE;
            if( acc == PDAT_READONLY )           foundRO = TRUE;
            if(   acc == PDAT_POW_OR_UNKNOWN
               && getUctUdfRevision() >= 0x260 ) foundPOW = TRUE;
        }
        /* Follow the rules of UDF 2.2.10 and UDF 2.50 errata DCN-5101.
         * A metadata partition is required if:
         * - No virtual partition is present on the medium.
         * and:
         * - In the single PD case for a read-only, overwritable
         *   and pseudo-overwritable partition.
         *   In the special two-PD case, only for an overwritable partition,
         *   if combined with a read-only partition.
         * IN ALL OTHER CASES, no metadata partition is allowed.
         * (mind that foundPOW is FALSE for UDF 2.50-);
         */
        expectMeta = ( !foundVirtual &&                      /* no virtual partition */
           (   (expPDs == 1 && (foundRO || foundOV || foundPOW)) /* single partition */
            || (expPDs == 2 &&  foundRO && foundOV) ));  /* special 2 partition case */

        /* It is possible that a Metadata is present as expected, but
         * for the wrong PD in the special two-PD case, so check the
         * access type of the actual metadata partition too.
         */
        metaAccessType = PDAT_READONLY;     /* keep compiler happy */
        metaAccessOk = TRUE;                /* default */
        if( foundMeta )
        { metaAccessType =
            mc->partitionMapInfo[mc->metadataPref].pdPointer->accessType;
          metaAccessOk =
            (   (   metaAccessType == PDAT_READONLY       && expPDs == 1 )
             || (   metaAccessType == PDAT_POW_OR_UNKNOWN && expPDs == 1
                 && getUctUdfRevision() >= 0x260 )
             || (   metaAccessType == PDAT_OVERWRITABLE   && expPDs <= 2 ));
        }
        /* condition:
         *  if foundMeta == TRUE,
         *  then: metaAccessType is valid and metaAccessOk has a meaning
         *  else: metaAccessType is INvalid and metaAccessOk == TRUE.
         *
         * Expect Metadata and presence of MetadataMap must both be
         * TRUE or both be FALSE and the access type must be ok.
         */
        if( (foundMeta !=  expectMeta) || !metaAccessOk )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout, "\tError: Metadata Partition Map ");
            if(      !foundMeta )       /* && expectMeta && metaAccessOk */
            { fprintf(uctout, "missing for %s partition.\n",
                PDAT_TEXT( (expPDs == 1)
                    ? mc->partitionMapInfo[0].pdPointer->accessType
                    : PDAT_OVERWRITABLE ));
            }
            else    /* foundMeta && (!metaAccessOk || !expectMeta */
            { fprintf(uctout, "not allowed for %s%s partition%s.\n",
                        (expPDs > 1) ? "this " : "",
                        PDAT_TEXT(metaAccessType),
                        (foundVirtual)\
                          ? "\n-\t       with a virtual partition"
                                                " on the same volume"
                          : "");
            }
            /* Print extra explanation.
             * Note: The virtual partition (if any) is only mentioned above
             */
            fprintf(uctout,
              "-\t  A Metadata Partition Map shall be "
                                "recorded for a single partition\n"
              "-\t  with access type %s%s%s or %s and for\n"
              "-\t  the %s partition in the special "
                                "two Partition Descriptors\n"
              "-\t  case with a %s and an %s partition on one volume.\n"
              "-\t  It shall NOT be recorded IN ALL OTHER CASES,"
                                                    " UDF 2.2.10%s.\n",
                    (getUctUdfRevision() >= 0x260)
                                ? PDAT_TEXT(PDAT_POW_OR_UNKNOWN) : "",
                    (getUctUdfRevision() >= 0x260)        ? ", " : "",
                    PDAT_TEXT(PDAT_READONLY), PDAT_TEXT(PDAT_OVERWRITABLE),
                    PDAT_TEXT(PDAT_OVERWRITABLE),
                    PDAT_TEXT(PDAT_READONLY), PDAT_TEXT(PDAT_OVERWRITABLE),
                    (getUctUdfRevision() <= 0x250)
                        ? ",\n-\t  UDF 2.50 errata DCN-5101"
                        : "");
            MLIMITend;
        }
    }

    return TRUE;

}   /* end checkAndMapPartitionInfo() */

/* udfMountLogicalVolume:
 * mc->device and mc->vi are already assigned
 * All other fields must be cleared, so values,
 * pointers 0, NULL resp.
 *
 * Note: The exact UDF revision must be known here !!
 */
extern bool udfMountLogicalVolume(UdfMountContext *mc,
                                  Timestamp *pVerifyStartTime)
{
    const MediumInfo *vmi = getTheMediumInfo();

    /* Construct pointer array which maps from a partitionReferenceNumber
     * to the corresponding PartitionDescriptor in VolumeInformation.
     *
     * checkAndMapPartitionInfo() may read the VAT, Metadata Files
     * and/or Sparing Tables. This is BEFORE the LVID is read. After
     * the LVID only descriptors in the Logical Volume will be read.
     * This order is important for the overall Timestamp verification,
     * see verifyTimestampLVIDorVAT();
     */
    if( !checkAndMapPartitionInfo(mc) )
    {   udfUnmountLogicalVolume(mc);
        return FALSE;
    }

    /* The presence of a virtual partion is known now and determines
     * whether a UDF file system is sequential or not and if it may
     * be in an intermediate state with a single AVDP).
     * Now check nmb of AVDPs, etc.
     */
    (void) checkNmbOfAvdpsAndAvdpLocations(mc);

    /* Sequential, closed and writability types should
     * be set now. Test MTYPE_SE_SEQUENTIAL combinations.
     */
    if(   (   vmi->sequentialType  == MTYPE_SE_SEQUENTIAL
           && vmi->closedType      == MTYPE_CL_INTERMEDIATE
           && vmi->writabilityType != MTYPE_WR_WRITEONCE )
       || (   vmi->sequentialType  == MTYPE_SE_SEQUENTIAL
           && vmi->closedType      == MTYPE_CL_FINAL
           && vmi->writabilityType != MTYPE_WR_WRITEONCE
           && vmi->writabilityType != MTYPE_WR_READONLY ) )
    {   MLIMITbegin(WARN01level, uctMessageLimit);
          fprintf(uctout,
            "\tWarning: Unexpected combination of medium types"
                                    " %s,\n"
                 "-\t\t %s and %s, expected: %s",
            MTYPE_SE_TEXT(vmi->sequentialType),
            MTYPE_CL_TEXT(vmi->closedType),
            MTYPE_WR_TEXT(vmi->writabilityType),
            MTYPE_WR_TEXT(MTYPE_WR_WRITEONCE));
          if( vmi->closedType == MTYPE_CL_FINAL )
          { fprintf(uctout,
              " or %s", MTYPE_WR_TEXT(MTYPE_WR_READONLY));
          }
          fprintf(uctout,"\n");
        MLIMITend;
    }

    /* Partition end beyond last block allowed for
     * intermediate state only.
     * Cannot be tested earlier because finalization
     * state is determined in checkNumberOfAvdps().
     */
    checkPartitionBeyondLastBlock(mc);

    /* Read LVID and check if it is open or not.
     */
    if( !readLogicalVolumeIntegrityDescriptor(mc) )
    {   udfUnmountLogicalVolume(mc);
        return FALSE;
    }

    if( IS_PREF_PARTITION_FOUND(mc->virtualPref) )
    {   if( mc->lvid->integrityType != LVIDINTEGRITY_OPEN )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageHead( MLIMIT.vl, (Byte*) mc->lvid,
                    (Byte*) &mc->lvid->integrityType,
              "Error: LVID not Open for a volume containing\n"
              "-\t\t\ta Virtual Partition, UDF 6.10.1.1.\n");
          MLIMITend;
        }
    }
    else if( mc->lvid->integrityType == LVIDINTEGRITY_OPEN )
    { /* Open LVID for file system without VAT
       * print error message but further assume it to be Close
       * in order to show all volume inconsistencies
       */
      MLIMITbegin(ERROR00level, uctMessageLimit);
        printMessageHead( MLIMIT.vl, (Byte*) mc->lvid,
            (Byte*) &mc->lvid->integrityType,
                "Error: Dirty Volume. An Open prevailing LVID was\n"
          "-\t\t found but no Virtual Partition. The Logical Volume\n"
          "-\t\t is in an inconsistent state, unexpected errors\n"
          "-\t\t may occur, ECMA 3/8.8.2, 3/10.10.3.\n"
          "-\tNote: The verifier will assume the volume to have a Close LVID\n"
          "-\t      in order to show all LVID and volume inconsistencies.\n");
      MLIMITend;
      /* not fatal */
    }

    /* verify medium last modification time (from LVID or VAT).
     * verify and set Next UniqueID values
     */
    (void) verifyTimeAndSetNextUniqueId(pVerifyStartTime, mc);

    /* PVD recordingDateAndTime could not yet be compared to
     * VAT/LVID medium last modification time, so do it now.
     */
    if( mc->vi->pvd != NULL )
    {   verifyTimestampLVIDorVAT(&mc->vi->pvd->recordingDateAndTime,
                                 "PVD Recording Time",
                                 (Byte*)mc->vi->pvd, mc, NULL);
    }

    /* read unallocate/freed space descriptors/files
     */
    if( !readMetadataBitmapOrLVSpaceSets(mc) )
    {
        udfUnmountLogicalVolume(mc);
        return FALSE;
    }

    /* Compare of Metadata File and its Mirror cannot be done
     * before the verifier created the bitmap for the metadata
     * partition, because the Metadata Bitmap File may not be
     * present for a read-only partition.
     * See cmpMetadataFiles().
     */

    /* read FSD
     */
    if( !readFileSetDescriptor(mc) )
    {
        udfUnmountLogicalVolume(mc);
        return FALSE;
    }
    return TRUE;

}   /* end udfMountLogicalVolume() */


