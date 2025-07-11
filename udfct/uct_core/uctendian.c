/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctendian.c
 *
 * Description : Endian swap for big endian machines.
 *               Endian swap is done only once for each UDF structure
 *               directly after it has been read from the medium.
 *
 * Author(s)   : Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "general.h"
#include "uctgeneral.h"
#include "uctendian.h"
#include "uctstatus.h"


/*************** Endian byte swapping ****************************************
 *
 * isBigEndianPlatform() function:
 * This function detects whether the running platform
 * is big endian or not.
 * Used by endianSwap().
 */
extern bool isBigEndianPlatform()
{
    Uint16 testEndian16 = 0xFF00;   /* different 'high' and 'low' byte */

    if( (Byte) (*((Byte*)&testEndian16)) == 0xFF )
    { return TRUE;      /* big endian */
    }
    return FALSE;       /* little endian */
}

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
                       register Uint32 nElem, Byte *alignBase)
{
    static bool firstTime = TRUE,
                isBigEndian;

    if( firstTime )
    {   firstTime = FALSE;      /* execute only once */
        isBigEndian = isBigEndianPlatform();
    }

    if( elSize == 1 )
    {
        return;             /* no action */
    }

    if( alignBase != NULL && ((arr - alignBase) % elSize) != 0 )
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageHead(MLIMIT.vl, alignBase, arr, NULL);
          fprintf(uctout,
              "Alignment error: For %d bytes field, please report\n",
                    elSize);
          fflush(uctout);       /* try to get this printed */
        MLIMITend;
    }

#ifdef  DEBUG01
    if( (((Uint32)arr) % elSize) != 0 )
    {   MLIMITbegin(DEBUG01level, MLIMITdefault01);
          printMessageHead(MLIMIT.vl, alignBase, arr, NULL);
          fprintf(uctout,
            "DEBUG01: Maybe %d bytes memory address alignment problem\n"
             "-\t\t\t  for some platforms, address 0x%X\n", elSize, arr);
          fflush(uctout);       /* try to get this printed */
        MLIMITend;
    }
#endif  /* DEBUG01 */

    if( isBigEndian )
    {   /* convert from little to big endian
         */
        register Uint32 n;
        register Byte   h, *p1, *p2;

        UCTASSERT( elSize == 2 || elSize == 4 || elSize == 8 );
        for( n = 0; n < nElem; n++ )
        {
            p1 = arr;  arr += elSize;
            p2 = arr - 1;
            while( p1 < p2 )
            {
                h = *p1;
                *p1++ = *p2;
                *p2-- = h;
            }
        }
    }
}


/* endian swap for all approriate UDF structures ***********************
 */
static void endianSwapTimestamp(Timestamp *t, Byte *d)
{
    endianSwap((Byte *) &t->TypeAndTimezone, sizeof(Uint16), 2,d);
}

static void endianSwapExtentAd(ExtentAd *ex, Byte *d)
{
    endianSwap((Byte *) &ex->extentLength, sizeof(Uint32), 2,d);
}

static void endianSwapLBAddr(LBAddr *lba, Byte *d)
{
    endianSwap((Byte *)&lba->logicalBlockNumber, sizeof(Uint32),1,d);
    endianSwap((Byte *)&lba->partitionReferenceNumber, sizeof(Uint16), 1,d);
}

/* endianSwapShortAd()
 * ECMA 4/14.14.1
 */
static void endianSwapShortAd(ShortAd *sad, Byte *d)
{
    endianSwap((Byte *)&sad->extentLength, sizeof(Uint32), 2,d);

}   /* end endianSwapShortAd() */

/* endianSwapLongAd:
 * ECMA 4/14.14.2, UDF 2.00 2.3.10.1
 * If isFidIcb == TRUE and the UDF revision is UDF 2.00 or higher
 * then swap the implementationUse.ImpUse.UDFUniqueID field.
 *
 */
static void endianSwapLongAd(LongAd *lad, Byte *d, bool isFidIcb)
{
    endianSwap((Byte *)&lad->extentLength, sizeof(Uint32), 1,d);
    endianSwapLBAddr(  &lad->extentLocation,d);

    /* ADImpUse
     */
    endianSwap((Byte *)&(lad->implementationUse.flags), sizeof(Uint16), 1,d);

    if( isFidIcb && getUctMinUdfRevision() >= 0x200 )   /* 2.00+ */
    { endianSwap((Byte *)&(lad->implementationUse.ImpUse.UDFUniqueID),
                 sizeof(Uint32), 1,d);
    }
}

/* endianSwapEntityID():
 * Context dependent suffix swapping for different suffix types
 */
static void endianSwapEntityID(EntityID *eid,
                               ENTITY_SUFFIX_TYPE suffixType,
                               Byte *d)
{
    switch( suffixType )
    {
    case ENTITY_SUFFIX_DOMAIN:
        endianSwap((Byte *)&eid->idSuffix.domainSuffix.udfRevision,
                    sizeof(Uint16), 1,d);
        break;
    case ENTITY_SUFFIX_UDF:
        endianSwap((Byte *)&eid->idSuffix.udfSuffix.udfRevision,
                    sizeof(Uint16), 1,d);
        break;
    case ENTITY_SUFFIX_IMPL:        /* no action */
    case ENTITY_SUFFIX_APPL:        /* no action */
        break;
    }
}       /* end endianSwapEntityID() */

static void endianSwapICBTag(ICBTag *it, Byte *d)
{
    endianSwap((Byte *)&it->priorRecordedNumberOfDirectEntries,
                                          sizeof(Uint32), 1,d);
    endianSwap((Byte *)&it->strategyType, sizeof(Uint16), 1,d);
    /* Exception: strategyParameter[2] is always interpreted as a Uint16 !!!
     */
    endianSwap((Byte *)&it->strPar.strategyParameterU16, sizeof(Uint16), 1,d);
    endianSwap((Byte *)&it->maximumNumberOfEntries, sizeof(Uint16), 1,d);
    endianSwapLBAddr(  &it->parentICBLocation, d);
    endianSwap((Byte *)&it->flags, sizeof(Uint16), 1,d);
}


extern void endianSwapBoot2Tail(BootDescriptorTail *bt, Byte *d)
{
    endianSwapEntityID( &bt->ArchitectureType, ENTITY_SUFFIX_UNDEFINED,d);
    endianSwapEntityID( &bt->BootIdentifier,   ENTITY_SUFFIX_UNDEFINED,d);
    endianSwap((Byte *) &bt->BootExtentLocation, sizeof(Uint32), 2,d);
    endianSwap((Byte *) &bt->LoadAddress,        sizeof(Uint64), 2,d);
    endianSwapTimestamp(&bt->DescriptorCreationDateAndTime,d);
    endianSwap((Byte *) &bt->Flags,              sizeof(Uint16), 1,d);
}

extern void endianSwapVat150Tail(VAT150Tail *vt, Byte *d)
{
    endianSwapEntityID(&vt->EntityIdentifier, ENTITY_SUFFIX_UDF, d);
    endianSwap((Byte *)&vt->PreviousVATICBlocation, sizeof(Uint32), 1,d);
}

extern void endianSwapVat200Head(VAT200Head *vh, Byte *d, Uint32 bytesRead)
{
    endianSwap((Byte *)&vh->lengthofHeader, sizeof(Uint16), 2,d);
    endianSwap((Byte *)&vh->previousVATICBlocation, sizeof(Uint32), 3,d);
    endianSwap((Byte *)&vh->minUDFReadVersion, sizeof(Uint16), 3,d);

    if(    bytesRead >= (  offsetof(VAT200Head, startOfImplementationUse)
                         + sizeof(EntityID))
        && vh->lengthofImplementationUse >= sizeof(EntityID) )
    {   endianSwapEntityID((EntityID*) &vh->startOfImplementationUse,
                            ENTITY_SUFFIX_IMPL, NULL);
    }
}

extern void endianSwapEAGenericHead(EAGenericHead *gea)
{
    endianSwap((Byte *)&gea->attributeType, sizeof(Uint32), 1,(Byte*) gea);
    endianSwap((Byte *)&gea->attributeLength, sizeof(Uint32), 1,(Byte*) gea);
}

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
                                            Byte  *EASpace)
{
    /* endian swap of the fields after genericHead
     */
    switch( ea->genericHead.attributeType )
    {
    case EATYPE_CHARSETINFO:    /* ECMA 4/14.10.3 */
        {   CharSetInformationExtendedAttribute *cea = &ea->charSetInfoEA;
            endianSwap((Byte *) &cea->escapeSequencesLength,
                       sizeof(Uint32), 1, EASpace);
        }
        break;
    case EATYPE_FILETIMES:      /* ECMA 4/14.10.5, UDF 3.3.4.3 */
        {   FileTimesExtendedAttribute *fea = &ea->fileTimesEA;
            endianSwap((Byte *) &fea->dataLength,
                       sizeof(Uint32), 2, EASpace);
        }
        break;
    case EATYPE_INFOTIMES:      /* ECMA 4/14.10.6 */
        {   InformationTimesExtendedAttribute *iea = &ea->infoTimesEA;
            endianSwap((Byte *) &iea->dataLength,
                       sizeof(Uint32), 2, EASpace);
        }
        break;
    case EATYPE_DEVSPEC:        /* ECMA 4/14.10.7, UDF 3.3.4.4 */
        {   DeviceSpecificationExtendedAttribute *dea = &ea->devSpecEA;
            endianSwap((Byte *) &dea->implementationUseLength,
                       sizeof(Uint32), 3, EASpace);
            if( bytesRead >= offsetof(DeviceSpecificationExtendedAttribute,
                                      implementationUse.impUseID)
                             + sizeof(EntityID) )
            {   endianSwapEntityID(&dea->implementationUse.impUseID,
                                    ENTITY_SUFFIX_IMPL, EASpace);
            }
        }
        break;
    case EATYPE_IMPLUSE:        /* ECMA 4/14.10.8 */
        {   ImplementationUseExtendedAttribute *iea = &ea->implUseEA;
            endianSwap((Byte *) &iea->implementationUseLength,
                       sizeof(Uint32), 1, EASpace);
            if( isUdfEA )
            {           /* UDF Implementation EA, with eaCheckSsum */
                endianSwapEntityID(&iea->implementationIdentifier,
                                   ENTITY_SUFFIX_UDF, EASpace);
                endianSwap((Byte *) &iea->implementationUse.headerChecksum,
                           sizeof(Uint16), 1, EASpace);
            }
            else        /* non-UDF Implementation EA */
            {
                endianSwapEntityID(&iea->implementationIdentifier,
                                   ENTITY_SUFFIX_IMPL, EASpace);
            }
        }
        break;
    case EATYPE_APPLUSE:
        {   ApplicationUseExtendedAttribute *aea = &ea->applUseEA;
            endianSwap((Byte *) &aea->applicationUseLength,
                       sizeof(Uint32), 1, EASpace);
            if( isUdfEA )
            {           /* UDF Application EA, with eaCheckSsum */
                endianSwapEntityID(&aea->applicationIdentifier,
                                   ENTITY_SUFFIX_UDF, EASpace);
                endianSwap((Byte *) &aea->applicationUse.headerChecksum,
                           sizeof(Uint16), 1, EASpace);
            }
            else        /* non-UDF Application EA */
            {
                endianSwapEntityID(&aea->applicationIdentifier,
                                   ENTITY_SUFFIX_APPL, EASpace);
            }
        }
        break;
    }
}


/* descriptor endian swap *********************************************
 *
 * endianSwapDescriptorTag():
 *
 * Swap appropriate bytes for big endian machimes
 * Byte order may change. This is no problem for
 * TagChecksum and DescriptorCRC because the
 * TagChecksum calculation is byte order independent
 * and the DescriptorCRC calculation involves bytes
 * outside the DescriptorTag itself.
 */
extern void endianSwapDescriptorTag(Tag *dTag)
{
    endianSwap((Byte *) &dTag->tagIdentifier,
                                    sizeof(Uint16), 2,(Byte*)dTag);
    endianSwap((Byte *) &dTag->tagSerialNumber,
                                    sizeof(Uint16), 3,(Byte*)dTag);
    endianSwap((Byte *) &dTag->tagLocation,
                                    sizeof(Uint32), 1,(Byte*)dTag);
}

/* Now swap functions for all descriptor tails,
 * which is the part after the descriptor tag.
 *
 * Precondition for all following endianSwap<descriptor>() functions :
 *  It is assumed that the descriptor tag is checked (specially
 *  the CRC check) and swapped to the correct endian.
 *
 * NOTE: no swap for: Dstring
 *       no swap for: Charspec
 */

/* Sparing Table, UDF 2.2.12 (was 2.2.11)
 */
static void endianSwapSparingTable(SparingTable *st)
{
    endianSwapEntityID( &st->sparingIdentifier,
                            ENTITY_SUFFIX_UDF, (Byte*)st);
    endianSwap((Byte *) &st->reallocationTableLength,
                                sizeof(Uint16), 1,(Byte*)st);
    endianSwap((Byte *) &st->sequenceNumber,
                                sizeof(Uint32), 1,(Byte*)st);
    /* swap Map Entries, 2 * Uint32 for each Map Entry
     */
    endianSwap((Byte *) &st->startOfMapEntries, sizeof(Uint32),
                    2 * st->reallocationTableLength, (Byte*)st);
}

/* Primary Volume Descriptor: ECMA 3/10.1. UDF 2.00 2.2.2.
 */
static void endianSwapPrimaryVolumeDescriptor(PrimaryVolumeDescriptor *d)
{
    endianSwap( (Byte *)&d->volumeDescriptorSequenceNumber,
                                    sizeof(Uint32), 2,(Byte*)d);
    endianSwap( (Byte *)&d->volumeSequenceNumber,
                                    sizeof(Uint16), 4,(Byte*)d);
    endianSwap( (Byte *)&d->characterSetList,
                                    sizeof(Uint32), 2,(Byte*)d);
    endianSwapExtentAd( &d->volumeAbstract, (Byte*)d);
    endianSwapExtentAd( &d->volumeCopyrightNotice, (Byte*)d);
    endianSwapEntityID( &d->applicationIdentifier,
                                ENTITY_SUFFIX_APPL, (Byte*)d);
    endianSwapTimestamp(&d->recordingDateAndTime, (Byte*)d);
    endianSwapEntityID( &d->implementationIdentifier,
                                ENTITY_SUFFIX_IMPL, (Byte*)d);
    endianSwap( (Byte *)&d->predecessorVolumeDescriptorSequenceLocation,
                                    sizeof(Uint32), 1,(Byte*)d);
    endianSwap( (Byte *)&d->flags,  sizeof(Uint16), 1,(Byte*)d);
}

static void endianSwapAnchorVolumeDescriptorPointer(AnchorVolumeDescriptorPointer *d)
{
    endianSwapExtentAd(&d->mainVolumeDescriptorSequenceExtent,   (Byte*)d);
    endianSwapExtentAd(&d->reserveVolumeDescriptorSequenceExtent, (Byte*)d);
}

static void endianSwapVolumeDescriptorPointer(VolumeDescriptorPointer *d)
{
    endianSwap( (Byte *)&d->volumeDescriptorSequenceNumber,
                                             sizeof(Uint32), 1,(Byte*)d);
    endianSwapExtentAd(&d->nextVolumeDescriptorSequenceExtent, (Byte*)d);
}



/* LVInformation
 * UDF 2.2.7.2
 * Located in implementationUse of Implementation Use Volume Descriptor.
 */
static void endianSwapLVInformation(LVInformation *lvi, Byte *iuvd)
{
    endianSwapEntityID(&lvi->implementationID, ENTITY_SUFFIX_IMPL, iuvd);
}

/* Implementation Use Volume Descriptor
 * ECMA 3/10.4
 * UDF 2.00 2.2.7 and 2.2.7.2
 *
 * A IUVD with EntityID Identifier starting with "*UDF"
 * is assumed to be a UDF IUVD with "UDF" EntityID suffix type.
 * else a non-UDF IUVD with "Application" EntityID suffix type
 * is assumed.
 *
 * For all UDF IUVDs, a UDF LV Info IUVD layout is assumed,
 * because that is the only defined UDf IUVD, UDF 2.2.7.1+2.
 */
static void endianSwapImplementationUseVolumeDescriptor(
                      ImplementationUseVolumeDescriptor *d)
{
    endianSwap( (Byte*) &d->volumeDescriptorSequenceNumber,
                         sizeof(Uint32), 1,(Byte*)d );

    /* Test if UDF or 'foreign' IUVD.
     * EntityID Identifier fields are endian swap independent,
     * assume UDF IUVD if first 4 chars are "*UDF"
     * else foreign IUVD "in its own format", UDF 2.2.7.
     */
    if( memcmp( &d->implementationIdentifier.Identifier,
                "*UDF", 4 ) != 0 )
    {   /* Non UDF IUVD, Implementation EntityID suffix type
         */
        endianSwapEntityID( &d->implementationIdentifier,
                             ENTITY_SUFFIX_IMPL, (Byte*)d);
    }
    else
    {   /* UDF IUVD, UDF EntityID suffix type
         */
        endianSwapEntityID( &d->implementationIdentifier,
                             ENTITY_SUFFIX_UDF, (Byte*)d);

        /* assume verifyLVInformation in implementationUse
         * because the UDF LV Info IUVD is the only
         * UDF IUVD defined so far, UDF 2.2.7.2
         */
        endianSwapLVInformation( &d->implementationUse.lvInformation,
                                 (Byte*)d);
    }
}

/* partition header descriptor
 * ECMA 4/14.3, UDF 2.3.3
 * Located in partitionContentsUse of Partition Descriptor.
 */
static void endianSwapPartitionHeaderDescriptor(PartitionHeaderDescriptor *phd,
                                                Byte *pd)
{
    endianSwapShortAd(&phd->unallocatedSpaceTable, pd);
    endianSwapShortAd(&phd->unallocatedSpaceBitmap, pd);
    endianSwapShortAd(&phd->partitionIntegrityTable, pd);
    endianSwapShortAd(&phd->freedSpaceTable, pd);
    endianSwapShortAd(&phd->freedSpaceBitmap, pd);
}

/* Partition Descriptor
 * ECMA 3/10.5 and 4/3.1
 */
static void endianSwapPartitionDescriptor(PartitionDescriptor *d)
{
    endianSwap( (Byte *)&d->volumeDescriptorSequenceNumber,
                                            sizeof(Uint32), 1,(Byte*)d);
    endianSwap( (Byte *)&d->partitionFlags, sizeof(Uint16), 2,(Byte*)d);
    endianSwapEntityID( &d->partitionContents,
                            ENTITY_SUFFIX_APPL, (Byte*)d);
    endianSwapPartitionHeaderDescriptor(
                        &d->partitionContentsUse.partitionHeaderDescriptor,
                            (Byte*)d);
    endianSwap( (Byte *)&d->accessType, sizeof(Uint32), 3,(Byte*)d);
    endianSwapEntityID( &d->implementationIdentifier,
                                    ENTITY_SUFFIX_IMPL, (Byte*)d);

#undef  UCT_TESTING_FAKE_ONE_POW_PARTITION      /* normally #undef */
#ifdef  UCT_TESTING_FAKE_ONE_POW_PARTITION
{ static bool firstTime = TRUE;
//  if( firstTime )
//  { firstTime = FALSE;
      d->accessType = PDAT_POW_OR_UNKNOWN;  /** UCT_TESTING_FAKE_ONE_POW_PARTITION **/
//    d->accessType = 77;                   /** UCT_TESTING_FAKE_ONE_POW_PARTITION **/
//  }
}
#endif  /** UCT_TESTING_FAKE_ONE_POW_PARTITION **/
}

/* Logical Volume Descriptor
 * ECMA      3/10.6 and 4/3.1
 * UDF 2.00 2.2.4
 *
 * Implementation NOTE:
 *  A Uint32 (or bigger unit) in a type 2 partition map may not
 *  be aligned in the descriptor block as expected because of a
 *  preceding type 1 map that has a size of 6 bytes. There is no
 *  padding on 4 bytes at the end of a partition map like with FIDs.
 */
static void endianSwapLogicalVolumeDescriptor(LogicalVolumeDescriptor *d)
{
    Byte  *bpPmap;      /* partition map byte pointer */
    Uint8  pmgLen;
    Uint32 npm, nm, mtl, mtSize;

    endianSwap( (Byte *)&d->volumeDescriptorSequenceNumber,
                                             sizeof(Uint32), 1,(Byte*)d);
    endianSwap( (Byte *)&d->logicalBlockSize, sizeof(Uint32), 1,(Byte*)d);
    endianSwapEntityID( &d->domainIdentifier,
                                    ENTITY_SUFFIX_DOMAIN, (Byte*)d);
    endianSwapLongAd( &d->logicalVolumeContentsUse.fileSetDescriptorSequenceExtent,
                            (Byte*)d, FALSE);
    endianSwap( (Byte *)&d->mapTableLength, sizeof(Uint32), 2,(Byte*)d);
    endianSwapEntityID( &d->implementationIdentifier,
                                    ENTITY_SUFFIX_IMPL, (Byte*)d);
    endianSwapExtentAd( &d->integritySequenceExtent, (Byte*)d);

    /* Now swap all partition maps
     */
    mtl = d->mapTableLength;
    npm = d->numberOfPartitionMaps;

    /* Carefully check npm, mtl and partitionMapLength consistency.
     * Do endian swap for at most npm maps that fit in map table.
     * Proceed to next map if previous partitionMapLength is equal
     * to the expected length according to partitionMapType.
     * Mind that a Type 1 map is the smallest one possible
     *      (type 1 map size) is less than (type 2 map size).
     * Some messages are printed here, but the real testing
     * is done in verifyLogicalVolumeDescriptor().
     */
    bpPmap = &d->startOfPartitionMaps;
    for( nm = 0,      mtSize = 0;
         nm < npm && (mtSize + sizeof(Type1PartitionMap)) <= mtl;
         nm++,        mtSize += pmgLen,
                      bpPmap += pmgLen )
    {
        GenericPartitionMap *pmg = (GenericPartitionMap*) bpPmap;
        Uint32 mapSize;

        pmgLen = pmg->partitionMapLength;

        switch( pmg->partitionMapType )
        {
        case PMAP_TYPE1:
            mapSize = sizeof(Type1PartitionMap);
            break;
        case PMAP_TYPE2:
            mapSize = sizeof(Type2PartitionMap);
            break;
        default:        /* Unknown Partition Map Type */
            mapSize = 0;    /* map inconsistency */
            break;
        }

        if(   mapSize == 0
           || mapSize > pmgLen
           || (mtSize + mapSize) > mtl )
        {
            break;  /* cannot swap this map, map inconsistency */
        }
        /* condition: mapSize bytes in map table available for parsing
         */
        switch( pmg->partitionMapType )
        {
        case PMAP_TYPE1:
          { Type1PartitionMap *pmt1 = (Type1PartitionMap*) pmg;
            endianSwap((Byte *)&pmt1->volumeSequenceNumber,
                                  sizeof(Uint16), 2,(Byte*)d);
            break;
          }
        case PMAP_TYPE2:
          { Type2PartitionMap *pmt2 = (Type2PartitionMap*) pmg;
            bool isSparable, isMetadata;

            /* swap part shared by different type 2 maps first:
             * EntityID partitionTypeIdentifier;
             */
            endianSwapEntityID(&pmt2->partitionTypeIdentifier,
                                            ENTITY_SUFFIX_UDF, (Byte*)d);
            /* Uint16 volumeSequenceNumber;
             * Uint16 partitionNumber;
             */
            endianSwap((Byte *)&pmt2->volumeSequenceNumber,
                                            sizeof(Uint16), 2,(Byte*)d);
            /* deternine type of part 2 map
             */
            isSparable = (memcmp(&pmt2->partitionTypeIdentifier.Identifier,
                            SPARABLE_PARTITION_ID, ENTITYID_IDSIZE) == 0);
            isMetadata = (memcmp(&pmt2->partitionTypeIdentifier.Identifier,
                            METADATA_PARTITION_ID, ENTITYID_IDSIZE) == 0);

            if(  !isSparable && !isMetadata
               && memcmp(&pmt2->partitionTypeIdentifier.Identifier,
                            VIRTUAL_PARTITION_ID, ENTITYID_IDSIZE) != 0)
            { MLIMITbegin(ERROR00level,uctMessageLimit);
                fprintf(uctout, "\tFatal error: Unknown Type 2 Partition Map\n");
              MLIMITend;
              return;   /** uctExit(EXIT_NON_CONFORMANT); **/ /* swap not complete */
            }

            /* We have now a sparable, virtual or metadata partition map.
             * No more swapping for virtual partition map
             */
            if( isSparable )
            {   /* Uint32 may not be aligned because of preceding
                 * type 1 map that has a size of 6 bytes.
                 */
                SparablePartitionMapTail *sTail = &pmt2->SharedTail.sparableTail;

                endianSwap((Byte *)&sTail->packetLength,
                                                sizeof(Uint16), 1,(Byte*)d);
                endianSwap((Byte *)&sTail->sizeOfEachSparingTable,
                                                sizeof(Uint32), 1, NULL);
                endianSwap((Byte *)&sTail->startOfLocationsOfSparingTables,
                    sizeof(Uint32), sTail->numberOfSparingTables, NULL);
            }
            else if( isMetadata )
            {   /* Uint32 may not be aligned because of preceding
                 * type 1 map that has a size of 6 bytes.
                 */
                MetadataPartitionMapTail *mTail = &pmt2->SharedTail.metadataTail;

                endianSwap((Byte *)&mTail->metadataFileLocation,
                                                sizeof(Uint32), 4, NULL);
                endianSwap((Byte *)&mTail->alignmentUnitSize,
                                                sizeof(Uint16), 1,(Byte*)d);
            }
            break;
          }
        default:    /* assert */
            return; /** uctExit(EXIT_NON_CONFORMANT); **/ /* swap not complete */
            /** break; **/
        }

        if( mapSize != pmgLen )
        {   /* mapSize < pmg->partitionMapLength
             * could swap this map, but cannot continue with
             * next one, because it may start
             * at: ((Byte*)pmg) + mapSize
             * or: ((Byte*)pmg) + pmg->partitionMapLength.
             */
            nm++;
            mtSize += mapSize;
            break;  /* map table inconsistency */
        }
    }       /* endfor, all maps done or map table inconsistency */
    /* endfor postconditions:
     *  nm is number of maps that could be parsed
     *  nmBytes is number of bytes parsed for nm maps
     */

    /* more detailed tests in verifyLogicalVolumeDescriptor()
     */
    if( nm != npm || mtSize != mtl )
    {   VERBOSE00(uctout,
          "  ==>\tNote: LVD endianSwap: Partition Map Table inconsistency.\n"
            "-\t  numberOfPartitionMaps: %lu, mapTableLength: %lu.\n"
            "-\t  Parsed%s %lu of %lu Partition Maps.\n"
            "-\t  Parsed%s %lu of %lu Partition Map Table bytes.\n",
                npm, mtl, (nm < npm) ? " only" : "", nm, npm,
                      (mtSize < mtl) ? " only" : "", mtSize, mtl);
    }
}

/* Unallocated Space Descriptor
 */
static void endianSwapUnallocatedSpaceDescriptor(UnallocatedSpaceDescriptor *d)
{
    ExtentAd *ead = (ExtentAd *) &d->startOfAllocationDescriptors;
    Uint32    i;

    endianSwap( (Byte *)&d->volumeDescriptorSequenceNumber,
                                    sizeof(Uint32), 2,(Byte*)d);
        /* includes swap d->numberOfAllocationDescriptors */

    for( i = 0; i < d->numberOfAllocationDescriptors; i++ )
    {
        endianSwapExtentAd( ead++, (Byte*)d );
    }
}

/* Logical Volume Header Descriptor
 * ECMA 4/3.1, 4/14.15
 * UDF 2.00 3.2.1
 * Located in logicalVolumeContentsUse of LVID
 */
static void endianSwapLogicalVolumeHeaderDescriptor(LogicalVolumeHeaderDescriptor *lvh,
                                                    Byte *lvi)
{
    endianSwap((Byte *)&lvh->uniqueID, sizeof(Uint64), 1,lvi);
}

/* endianSwapLvidImplementationUse
 * UDF 2.2.6.4
 */
static bool endianSwapLvidImplementationUse(LvidImplementationUse *iu,
                                            Byte *d)
{
    endianSwapEntityID(&iu->implementationID,ENTITY_SUFFIX_IMPL,d);

    /* Uint32 numberOfFiles;
     * Uint32 numberOfDirectories;
     */
    endianSwap((Byte *)&iu->numberOfFiles,sizeof(Uint32),2,d);

    /* Uint16 minimumUDFReadRevision;
     * Uint16 minimumUDFWriteRevision;
     * Uint16 maximumUDFWriteRevision;
     */
    endianSwap((Byte *)&iu->minimumUDFReadRevision,sizeof(Uint16),3,d);

    /* remaining implementation use:
     * Byte implementationUse[lvid.lengthOfImplementationUse-46];
     */
    return TRUE;
}

/* Logical Volume Integrity Descriptor
 * ECMA 3/10.10
 */
static void endianSwapLogicalVolumeIntegrityDescriptor(LogicalVolumeIntegrityDescriptor *d)
{
    endianSwapTimestamp(&d->recordingDateAndTime, (Byte*)d);
    endianSwap( (Byte *)&d->integrityType, sizeof(Uint32), 1,(Byte*)d);
    endianSwapExtentAd( &d->nextIntegrityExtent, (Byte*)d);
    endianSwapLogicalVolumeHeaderDescriptor(
                        &d->logicalVolumeContentsUse.logicalVolumeHeaderDescriptor,
                        (Byte*)d);
    endianSwap( (Byte *)&d->numberOfPartitions, sizeof(Uint32), 2,(Byte*)d);

    /* tables and implementation use:
     *  Uint32      FreeSpaceTable[numberOfPartitions];
     *  Uint32      SizeTable[numberOfPartitions];
     *  union {
     *    Byte                  iuBytes[lengthOfImplementationUse];
     *    LvidImplementationUse lvidImplementationUse;
     *  } implementationUse;
     */
    endianSwap(&d->startOfTables, sizeof(Uint32),
                2 * d->numberOfPartitions, (Byte*)d);

    endianSwapLvidImplementationUse( (LvidImplementationUse*)
        (&d->startOfTables + 2 * d->numberOfPartitions * sizeof(Uint32)),
        (Byte*)d);
}

/* File Set Descriptor
 * ECMA 4/14.1
 * UDF 2.00 2.3.2
 */
static void endianSwapFileSetDescriptor(FileSetDescriptor *d)
{
    endianSwapTimestamp(&d->recordingDateAndTime, (Byte*)d);
    endianSwap( (Byte *)&d->interchangeLevel, sizeof(Uint16), 2,(Byte*)d);
    endianSwap( (Byte *)&d->characterSetList, sizeof(Uint32), 4,(Byte*)d);
    endianSwapLongAd(   &d->rootDirectoryICB, (Byte*)d, FALSE);
    endianSwapEntityID( &d->domainIdentifier, ENTITY_SUFFIX_DOMAIN,(Byte*)d);
    endianSwapLongAd(   &d->nextExtent, (Byte*)d, FALSE);

    /* systemStreamDirectoryICB introduced in UDF 2.00,
     * ECMA-167 3rd edition, so descriptorVersion 3.
     */
    if( d->descriptorTag.descriptorVersion == 3 )
    {   endianSwapLongAd( &d->new200.systemStreamDirectoryICB,
                        (Byte*)d, FALSE);       /* no FID ICB */
    }
}

/* File Identifier Descriptor
 * ECMA 4/14.4
 * UDF 2.00 2.3.4
 */
static void endianSwapFileIdentifierDescriptor(FileIdentifierDescriptor *d)
{
    endianSwap( (Byte *)&d->fileVersionNumber,
                                        sizeof(Uint16), 1,(Byte*)d);
    endianSwapLongAd(   &d->ICB, (Byte*)d, TRUE);   /* is FID ICB LongAd */
    endianSwap( (Byte *)&d->lengthOfImplementationUse,
                                        sizeof(Uint16), 1,(Byte*)d);

    if( d->lengthOfImplementationUse >= sizeof(EntityID) )
    {
        endianSwapEntityID((EntityID *)&d->startOfImplementationUse,
                                    ENTITY_SUFFIX_IMPL, (Byte*)d);
        /* No endian swap done here for possible
         * implementationUse bytes after EntityID.
         * Implementation specific use or extra 'padding'.
         */
    }
    /* no endian swap for fileIdentifier[] and padding[] fields
     */
}

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
                    Uint8 adType,  Byte *d)
{
    const MediumInfo *vmi = getTheMediumInfo();
    size_t   adSize, sz;
    size_t   lenOfAds, maxBytes, maxDescr, adsDescr;
    bool     result = TRUE;

    if(      adType == ADT_SHORT ) adSize = sizeof(ShortAd);
    else if( adType == ADT_LONG  ) adSize = sizeof(LongAd);
    else return FALSE;  /* even ADT_INFE is not allowed here */

    /* Only ADT_SHORT or ADT_LONG allocation
     * descriptors from here, now swap them
     * Only complete descriptors
     */
    lenOfAds = *pLenOfAds,
    maxBytes = d + vmi->blockSize - ads;

    if( lenOfAds > maxBytes )
    {
        result = FALSE;         /* swap may not be complete */
#ifdef  DEBUG01
        MLIMITbegin(DEBUG01level, uctMessageLimit);
            printMessageHead( MLIMIT.vl, d, (Byte*)pLenOfAds, NULL);
            fprintf(uctout,
                "DEBUG01: Length Of Allocation Descriptors mismatch: %lu,\n"
                "-\t\t expected at most: %lu, endian swap may not be complete\n",
                lenOfAds, maxBytes);
        MLIMITend;
#endif  /* DEBUG01 */
    }
    maxDescr = maxBytes / adSize;
    adsDescr = lenOfAds / adSize;

    /* swap descriptors, only complete descriptors within
     * lenOfAds and within sector.
     */
    for( sz = MIN(adsDescr,maxDescr);
         sz > 0;                /* swap only integral ADs */
         sz--, ads += adSize )
    {
        switch( adType )
        {
        case ADT_SHORT:
            endianSwapShortAd((ShortAd *)ads, (Byte*)d);
            break;
        case ADT_LONG:
            endianSwapLongAd((LongAd *)ads, (Byte*)d, FALSE);
            break;
        }
    }
    return result;
}

/* Allocation Extent Descriptor
 * ECMA 4/14.5
 * UDF 2.00 2.3.11
 */
static void endianSwapAllocationExtentDescriptor( AllocationExtentDescriptor *aed )
{
    endianSwap( (Byte*)&aed->previousAllocationExtentLocation,
                                        sizeof(Uint32), 2,(Byte*)aed);

    /* Note that endian swap of Allocation Descriptors is not done here
     * but in swapAndVerifyAllocationDescriptors. The main reason for
     * this is that the AD type cannot easily be determined here.
     * Strictly according to ECMA, the ADs are outside the AED descriptor.
     */
}

static void endianSwapIndirectEntry(IndirectEntry *ie)
{
    endianSwapICBTag(&ie->icbTag, (Byte*)ie);
    endianSwapLongAd(&ie->indirectICB, (Byte*)ie, FALSE);
}

static void endianSwapTerminalEntry(TerminalEntry *te)
{
    endianSwapICBTag(&te->icbTag, (Byte*)te);
}

/* FEorEFE_TryToRepairLenEA:
 * endianSwapFEorEFE() found that lengthOfExtendedAttributes
 * is no integral multiple of 4:
 * Try to repair.
 * precondition: (lengthOfExtendedAttributes % 4) != 0
 */
static void FEorEFE_TryToRepairLenEA(Byte *d)
{
    Uint32            lenEA, oldLenEA, lenAD;
    int               i;
    Byte             *b;
    Byte             *pb   = pFE_startOfExtendedAttributes(d);
    Tag              *pTag = (Tag*) d;
    const MediumInfo *vmi  = getTheMediumInfo();

    oldLenEA = (*(pFE_lengthOfExtendedAttributes(d)));
    lenEA    = ROUNDUPMULT(oldLenEA,4);             /* correction */
    lenAD    = (*(pFE_lengthOfAllocationDescriptors(d)));
    VERBOSE00(uctout,
        "- ==>\tThe above error may cause alignment problems on some platforms.\n"
            "-\tTrying to repair the descriptor in order to avoid this and\n"
            "-\tstill be able to continue verify: Move AllocationDescriptors\n"
            "-\tfield %u bytes up and correct lengthOfExtendedAttributes\n"
            "-\tand maybe descriptorCRCLength accordingly.\n-\n",
            lenEA - oldLenEA);
    if( (pb + lenEA + lenAD) > (d + vmi->blockSize) )
    {
        VERBOSE00(uctout,
            "- ==>\tFatal error: unable to repair.\n"
                "-\tDescriptor would grow out of block, abort.\n");
        uctExit(EXIT_NON_CONFORMANT);   /* continue may cause bus error */
    }
    memmove( pb + lenEA, pb + oldLenEA, lenAD);     /* repair */
    for( b = pb + oldLenEA, i = 0; i < (int)(lenEA - oldLenEA); i++, b++ )
    {
        *b = 0;                                     /* padding */
    }
    if(   (d + sizeof(Tag) + pTag->descriptorCRCLength)
        > (pb + oldLenEA) )
    {
        /* CRC Length includes 1st byte of ADs.
         * Repair descriptorCRCLength because it is
         * checked after endian swap.
         * Mind assumption that CRC check is already done.
         * This is valid, because CRC check must be
         * done before any endian swap.
         */
        pTag->descriptorCRCLength = (Uint16)        /* repair */
            (pTag->descriptorCRCLength + (lenEA - oldLenEA));
    }
    *(pFE_lengthOfExtendedAttributes(d)) = lenEA;   /* repair */
}

/* endianSwapFEorEFE():
 * handles both FE and EFE using special FE_* macros.
 * FE  : File Entry.
 * EFE : Extended File Entry.
 *
 * Note: No endian swap of Extended Attributes and Allocation Descriptors !!
 */
static void endianSwapFEorEFE(Byte *d)
{
    Uint16 tagId = ((Tag *)d)->tagIdentifier;

    endianSwapICBTag(    pFE_icbTag(d), d);
    endianSwap( (Byte *) pFE_uid(d),    sizeof(Uint32), 3,d);
    endianSwap( (Byte *) pFE_fileLinkCount(d),
                                        sizeof(Uint16), 1,d);
    endianSwap( (Byte *) pFE_recordLength(d),
                                        sizeof(Uint32), 1,d);
    endianSwap( (Byte *) pFE_informationLength(d),
                                        sizeof(Uint64), 1,d);
    /* EFE: objectSize */
    endianSwap( (Byte *) pFE_logicalBlocksRecorded(d),
                                        sizeof(Uint64), 1,d);
    endianSwapTimestamp( pFE_accessTime(d), d);
    endianSwapTimestamp( pFE_modificationTime(d), d);
    /* EFE: creationTime */
    endianSwapTimestamp( pFE_attributeTime(d), d);
    endianSwap( (Byte *) pFE_checkpoint(d), sizeof(Uint32), 1,d);
    endianSwapLongAd(    pFE_extendedAttributeICB(d), d, FALSE);
    /* EFE: streamDirectoryICB */
    endianSwapEntityID(  pFE_implementationIdentifier(d),
                                    ENTITY_SUFFIX_IMPL, d);
    endianSwap( (Byte *) pFE_uniqueID(d), sizeof(Uint64), 1,d);
    endianSwap( (Byte *) pFE_lengthOfExtendedAttributes(d),
                                        sizeof(Uint32), 2,d);

    /* extra for EFE
     */
    if( tagId == tidEFE )
    {
        ExtendedFileEntry *efe = (ExtendedFileEntry *) d;

        endianSwap( (Byte *)&efe->objectSize, sizeof(Uint64), 1,d);
        endianSwapTimestamp(&efe->creationTime, d);
        endianSwapLongAd(   &efe->streamDirectoryICB, d, FALSE);
    }

    /* NOTE: Alignment problems may arise for some platforms - specially in
     *       case of embedded FIDs - if lengthOfExtendedAttributes is not
     *       an integral multiple of 4, so check and maybe repair this
     *       first in order to continue.
     *       For repair, it is assumed that CRC check has already been
     *       done, and at least a one block buffer was allocated for d.
     */
    if( (*(pFE_lengthOfExtendedAttributes(d)) % 4) != 0 )   /* error */
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        printMessageHead( MLIMIT.vl, d,
            (Byte*) pFE_lengthOfExtendedAttributes(d), NULL);
        fprintf(uctout,
            "Error: Length of Extended Attributes is no integral\n"
            "-\t\t\tmultiple of 4: %lu, ECMA 4/14.9.19\n",
                (*(pFE_lengthOfExtendedAttributes(d))) );
      MLIMITend;
      FEorEFE_TryToRepairLenEA(d);              /* maybe abort */
      /* mind that descriptor has changed !!
       */
    }

    /* Variable sized arrays follow :
     *  Byte extendedAttributes[lengthOfExtendedAttributes] PACKED;
     *  Byte allocationdescriptors[lengthOfAllocationDescriptors] PACKED;
     *
     * endian swap for these arrays done
     *  in swapAndVerifyExtendedAttributesSpace()
     * and swapAndVerifyAllocationDescriptors() respectively.
     */

}   /* end endianSwapFEorEFE() */

/* ECMA 4/14.10.1, UDF 3.3.4.1
 */
static void endianSwapExtendedAttributeHeaderDescriptor(ExtendedAttributeHeaderDescriptor *d)
{
    endianSwap((Byte *)&d->implementationAttributesLocation,
               sizeof(Uint32), 2,(Byte*) d);
}

/* endianSwapUnallocatedSpaceEntry():
 * Note: No endian swap of Allocation Descriptors !!
 */
static void endianSwapUnallocatedSpaceEntry(UnallocatedSpaceEntry *d)
{
    endianSwapICBTag(  &d->icbTag, (Byte*) d);
    endianSwap((Byte *)&d->lengthOfAllocationDescriptors,
               sizeof(Uint32), 1,(Byte*) d);

    /* allocation descriptors:
     * Byte allocationdescriptors[lengthOfAllocationDescriptors] PACKED;
     *
     * endian swap done in swapAndVerifyAllocationDescriptors()
     */
}

static void endianSwapSpaceBitmapDescriptor(SpaceBitmapDescriptor *d)
{
    endianSwap((Byte *)&d->numberOfBits, sizeof(Uint32), 2,(Byte*)d);
    /* no endian swap for bitmap itself
     */
}

/* endianSwapDescriptorTail():
 *
 * Precondition:
 *  It is assumed that the descriptor tag is checked (specially
 *  the CRC check) and swapped to the correct endian.
 *
 * endianSwapDescriptorTail() will swap the part following the tag.
 */
extern void endianSwapDescriptorTail(Tag *t)
{
    Uint16 tagId = t->tagIdentifier;

    switch( tagId )
    {
    case tidST:             /* special UDF descriptor */
        endianSwapSparingTable((SparingTable *) t);
        break;
                                        /* group 1, Part 3 descriptors */
    case tidPVD:
        endianSwapPrimaryVolumeDescriptor((PrimaryVolumeDescriptor *) t);
        break;
    case tidAVDP:
        endianSwapAnchorVolumeDescriptorPointer((AnchorVolumeDescriptorPointer *) t);
        break;
    case tidVDP:
        endianSwapVolumeDescriptorPointer((VolumeDescriptorPointer *) t);
        break;
    case tidIUVD:
        endianSwapImplementationUseVolumeDescriptor((ImplementationUseVolumeDescriptor *) t);
        break;
    case tidPD:
        endianSwapPartitionDescriptor((PartitionDescriptor *) t);
        break;
    case tidLVD:
        endianSwapLogicalVolumeDescriptor((LogicalVolumeDescriptor *) t);
        break;
    case tidUSD:
        endianSwapUnallocatedSpaceDescriptor((UnallocatedSpaceDescriptor *) t);
        break;
    case tidTD:         /* special, used in part 4 too */
                        /* no tail swapping for Terminating Descriptor */
        break;
    case tidLVID:
        endianSwapLogicalVolumeIntegrityDescriptor((LogicalVolumeIntegrityDescriptor *) t);
        break;
                                        /* group 2, Part 4 descriptors */
    case tidFSD:
        endianSwapFileSetDescriptor((FileSetDescriptor *) t);
        break;
    case tidFID:
        endianSwapFileIdentifierDescriptor((FileIdentifierDescriptor *) t);
        break;
    case tidAED:
        endianSwapAllocationExtentDescriptor((AllocationExtentDescriptor *) t);
        break;
    case tidIE:
        endianSwapIndirectEntry((IndirectEntry *) t);
        break;
    case tidTE:
        endianSwapTerminalEntry((TerminalEntry *) t);
        break;
    case tidFE:                     /* fall through */
    case tidEFE:
        endianSwapFEorEFE((Byte *) t);      /* exception, handles FE and EFE */
        break;
    case tidEAHD:
        endianSwapExtendedAttributeHeaderDescriptor((ExtendedAttributeHeaderDescriptor *) t);
        break;
    case tidUSE:
        endianSwapUnallocatedSpaceEntry((UnallocatedSpaceEntry *) t);
        break;
    case tidSBD:
        endianSwapSpaceBitmapDescriptor((SpaceBitmapDescriptor *) t);
        break;
    default:
        break;
    }
}

