/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctverify.h
 *
 * Description : Check and process functions
 *               for several UDF structures.
 *
 * Author(s)   : Gerrit Scholl
 */

#ifndef __UCT_UCTVERIFY_H__
#define __UCT_UCTVERIFY_H__

#include "mytypes.h"
#include "general.h"
#include "udfstruct.h"
#include "uctgeneral.h"
#include "uctdata.h"


/* verifyFileIdentifierDescriptor() global data *************************
 */
extern bool globalFidIdentifierError;

extern void udfStructStaticAsserts();   /* no return on failure */

/* Volume and boot block recognition. ***********************************
 * ECMA Part 2, 3/3.1 and 3/9.
 *
 * Volume Structure Descriptor types for verifyVolRecVSD() :
 */
typedef enum
{
    vsdTYPE_UNKNOWN = 0,
    vsdTYPE_BEA01,
    vsdTYPE_BOOT2,
    vsdTYPE_CD001,
    vsdTYPE_CDW02,
    vsdTYPE_NSR02,
    vsdTYPE_NSR03,
    vsdTYPE_TEA01
} VsdType;

/* verifyVolRecVSD():
 *
 * Check a Volume Structure Descriptor in a Volume Recognition Sequence.
 *
 * Return value : Volume Structure Descriptor type,
 */
extern VsdType verifyVolRecVSD(GenericVolumeStructureDescriptor *vsd);

extern Uint8 calculateTagChecksum(Tag *gt);

extern Uint16 calculateCrc(Byte *src, Uint16 numBytes);

/* getLengthOfDescriptor():
 * Determine total length of a descriptor (fixed + variable part).
 * Argument isSwapped notifies if endian swap has been done.
 *
 * ECMA, ...       - descriptor layouts.
 * UDF 2.50 2.2.12 - Sparing Table  (was 2.2.11)
 * UDF 2.50 5.1    - overview
 *
 * Exception:
 *  For the AllocationExtentDescriptor, the
 *  Byte AllocationDescriptors[LengthOfAllocationDescriptors]
 *  field is handled as part of the AllocationExtentDescriptor,
 *  while it is no part of the descriptor itself, according to
 *  ECMA and UDF. According to ECMA, the descriptor size is 24.
 */
extern bool getLengthOfDescriptor(Byte *d, bool isSwapped, Uint32 *dLen);

/* print HEX UniqueID in 17 char positions
 * if( isFid ) then "        #HHHHHHHH" (Uint32)
 * else             "#HHHHHHHHHHHHHHHH" (Uint64)
 */
extern void printHexUniqueId17Chars(Uint64 uniqueID,
                                    bool   isFid);

/* incrementUniqueId(): according to UDF 3.2.1.1
 */
extern Uint64 incrementUniqueId(Uint64 uniqueID);


/* verifyTimeAndSetNextUniqueId():
 * Verify medium modification time (LVID or VAT)..
 * ECMA 3/8.8.2, 3/10.10.2, UDF 2.2.6".
 * Verify and set mc Next UniqueID values
 * ECMA 4/14.15.1,
 * UDF 2.00+: 2.3.4.3, 2.3.6.7, 3.2.1.1, 3.3.3.4.
 * UDF 1.50-: 2.3.6.5, 3.3.3.4.
 */
extern bool verifyTimeAndSetNextUniqueId(Timestamp *pVerifyStartTime,
                                         UdfMountContext *mc);

/* finalCheckPartitionAccessType()
 * Check Partition Access Type of all prevailing PDs if this
 * was skipped for ALL PDs, because the medium UDF revision
 * was not yet known at the time when the first PD was read.
 * No action if test already done.
 */
extern bool finalCheckPartitionAccessType(UdfMountContext *mc);

/* verifyPdPartitionFlags():
 * Only call for PDs that are referenced by an LVD Partition Map.
 */
extern bool verifyPdPartitionFlags(PartitionMapInfo *pmi,
                                   Uint16 partRefNmb);

/* verifyPdPartitionAlignment():
 * Call after ECC blocking factor and sparing Packet Length are
 * established. MediumInfo eccLength is used for ECC blocking
 * factor in a Physical Partition.
 */
extern bool verifyPdPartitionAlignment( UdfMountContext *mc,
                                        Uint16 partRefNmb );

/* verifyMetadataFilesAllocation():
 * Verifies allocation and alignment of the Metadata Partition.
 * Call after sparing and ECC packet lengths are established.
 */
extern bool verifyMetadataFilesAllocation(UdfMountContext *mc);

/* verifyTimestampLVIDorVAT:
 * verify if timestamp is later than LVID (or VAT) timestamp.
 *
 * Mind return value:
 *  if comparison was completed and a time comparison error
 *     was found (maybe 'shall not be later' message printed).
 *  then: FALSE
 *  else: TRUE.
 */
extern bool verifyTimestampLVIDorVAT(Timestamp *t2, char *txt2, Byte *d,
                                     UdfMountContext *mc, Node *node);

/* verifyFinalLVIDandVAT():
 * Verify LVID and VAT field after Free Space,
 * Number of directories, etc. have been determined.
 * Mind that most syntax checks have already be done
 * in verifyLogicalVolumeIntegrityDescriptor()
 * and swapAndVerifyVatFile().
 * ECMA 3/10.10, UDF 2.2.6
 *
 * Implementation Notes:
 *  1) verifyFinalLVIDandVAT() must be called AFTER
 *     verifyPartitionAllocation().
 *  2) totalNumberOfFilesMFD and totalNumberOfDirsMFD are included
 *     in totalNumberOfFiles, totalNumberOfDirs respectively.
 */
extern bool verifyFinalLVIDandVAT(UdfMountContext *mc,
                                  Uint32 totalNumberOfFiles,
                                  Uint32 totalNumberOfDirs,
                                  Uint32 totalNumberOfFilesMFD,
                                  Uint32 totalNumberOfDirsMFD);

/* init for Unallocated Space Entry AD order verification
 */
extern void verifyUseAdOrderInit(Uint32 blockSize);

/* get Unallocated Space Entry AD total block count.
 */
extern Uint32 getUseAdOrderTotalBlocks();

/* swapAndVerifyAllocationDescriptors:
 * Swap and verify adType, *pLenOfAds and Ads itself.
 * If node == NULL, the start of the Allocation Descriptors was for
 * an Unallocated Space Entry, else for a FE ir EFE file node->fe.
 * The descriptor d can however also be a AED, in case of a continuation
 * Extent of ADs in one of the above cases.
 * So if node == NULL, some of the tests below will not be performed.
 * Only ADT_SHORT or ADT_LONG allowed, all other types
 * (even ADT_INFE) will cause a FALSE result.
 *
 * Note that ads and pLenOfAds must be pointers to the actual
 * Allocation Descriptors and lengthOfAllocationDescriptors
 * fields in descriptor d (FE, EFE, USE or AED).
 *
 * Total number of effective ADs, total AD extents length
 * and total AD file body length is maintained in
 * node->feTotalExtentsLength, node->feFileBodyLength
 * and *pNumberOfADs.
 * These fields must initialized before caling this function
 * for the first AD of a list of ADs.
 *
 * Note:
 *  if (node != NULL) then pCountADs == &node->feNumberOfADs
 *           and shortAdPartitionRef ==  node->fePartRef
 */
extern bool swapAndVerifyAllocationDescriptors(
                Byte   *ads, Uint32 *pLenOfAds, Byte *d,
                Uint8   adType,
                Uint32 *pCountADs,           /* &node->feNumberOfADs */
                Uint64 *pTotalRecordedBlocks,
                Uint16  shortAdPartitionRef, /*  node->fePartRef */
                UdfMountContext *mc, Node *node,
                bool    inMetadataPartition);

/* ECMA 4/9.1, 4/14.10.1, UDF 3.3.4.1
 * ECMA: Extended Attributes Space
 *  UDF: Extended Attribute Space
 *                         ^
 */
extern bool swapAndVerifyExtendedAttributesSpace(
                                Byte *EASpace, Uint32 EASpaceLength,
                                Uint32 expectedTagLocation, Byte *d,
                                UdfMountContext *mc, Node *node);

/* verifyUdfDefinedStreams():
 * UDF 2.00+ : 3.3.5.2, 3.3.7 and 3.3.8
 * Check if UDF defined stream name.
 *
 * implementation note: similar to the above isUdfExtendedAttribute().
 */
extern void verifyUdfDefinedStreams( Node *node, UdfMountContext *mc );

/* verify FID / (E)FE consistency
 * execute after both FID and (E)FE have been read
 * normally the FID is read first, exept for a parent FID which
 * is read after the (E)FE.
 */
extern bool verifyFidFeConsistency(Node *node, UdfMountContext *mc);

/* verifyEfeObjectSize():
 * EFE Uint64 objectSize : ECMA 4/14.17.11
 */
extern bool verifyEfeObjectSize( ExtendedFileEntry *efe,
                                 Uint64 streamsObjectSize,
                                 char  *txt1,
                                 UdfMountContext *mc,
                                 Node *node );

/* verifyLongAd:
 * ECMA 4/14.14.2, UDF 2.00+ 2., 2.3.10.
 */
extern bool verifyLongAd(LongAd *lad, Byte *d, UdfMountContext *mc,
                         bool isIntegralBlock,
                         bool isFixedRecAndAlloc,
                         bool inMetadataPartition);

/* print all fields of a LongAd as continuation lines
 * after an error has been detected.
 */
extern void printLongAd( LongAd *lad, bool isFidIcb );

/* Print extra information in case of a message in
 * verifyPartitionHeaderDescriptor() or one of the
 * functions called by it (verifyShortAd(), etc).
 */
extern void printExtraInfoForPHD(Byte *d, Byte *pBP);


/* determine if tagId is FE, EFE, USE or TE.
 */
extern bool isIcbDirectEntry(Uint16 tagId);

/* inspectDescriptorHead():
 * Checks if buffer contains the head of a valid UDF descriptor.
 * The head means: that part of the descriptor, enough to determine
 * the length of the descriptor, so at least the descriptor tag.
 *
 * In case expectedTagId is tidUNKNOWN, an extra test of a Sparing
 * Table Sparing Identifier is done because often the tag checksum is
 * ok, while it appears to be no ST after all
 * (-inspect_image finds too many ST).
 *
 * It is assumed that NO endian swap has been performed on
 * the buffer contents yet and this function will remain the
 * buffer contents unchanged.
 * inspectDescriptorHead() operates silently, error messages are
 * stored in uctErrorMessage using sprintf.
 * Therefore uctErrorMessage will be cleared as first action !!
 *
 * A blank tag (all #00 bytes) will never be accepted as a valid
 * descriptor head.
 * Further only the tagIdentifier and tagChecksum fields will
 * be inspected in order to recognize a valid descriptor head.
 *
 * If (expectedTagId == tidUNKNOWN), any valid descriptor will
 * be accepted, else only a descriptor with <tag id> equal to
 * expectedTagId will be accepted.
 *  Exception: If (expectedTagId == tidICBDIRECTENTRY), any
 *      <tag id> returning TRUE for isIcbDirectEntry(<tag id>)
 *      will be accepted.
 *
 * Function return value:
 *  if buffer contains a valid and accepted UDF descriptor head,
 *  then: TRUE
 *  else: FALSE.
 *
 * Other exported results (only if their pointer != NULL)
 *  *pExportTagId : if valid tag found then: tag id
 *                                     else: tidUNKNOWN
 *  *pDescriptorLength : descriptor length
 */
extern bool inspectDescriptorHead(Byte   *buffer,
                                  Uint32  bufferLength,
                                  Uint32  blockSize,
                                  bool    checksumIsFatal,
                                  Uint16  expectedTagId,
                                  Uint16 *pExportTagId,
                                  Uint32 *pDescriptorLength);

/* swapAndVerifyDescriptor():
 *
 * Checks if buffer contains a valid UDF descriptor.
 * Performs endian swap.
 *
 * expectedTagId determines which descriptors are accepted
 * and which are not, for details see inspectDescriptorHead().
 *
 * Function return value:
 *  if buffer contains a valid and accepted UDF descriptor
 *  then: TRUE
 *  else: FALSE.
 *
 * Return value in *pExportTagId only if pExportTagId != NULL:
 *      if valid tag found  then: found tag id
 *                          else: tidUNKNOWN
 * Important NOTE:
 *  If FALSE is returned, at least the descriptor tag will
 *  be endian swapped, but further buffer information is undefined,
 *  because of possible partial endian swaps.
 *
 * numberOfBytesRead shall be :
 *  either: an integral number of blocks, at least one
 *      or: exactly the descriptor size
 */
extern bool swapAndVerifyDescriptor( Byte   *buffer,
                                     Uint32  numberOfBytesRead,
                                     Uint16  expectedTagId,
                                     Uint16 *pExportTagId,
                                     Uint32  expectedTagLocation,
                                     UdfMountContext *mc, Node *node );

/*****************************************************************************
 */

/* verifyUdfRevision():
 *
 * Verify UDF revision in hex (BCD in fact).
 */
extern bool verifyUdfRevision(Uint16 revision,
                              Uint16 minRevision,
                              Uint16 maxRevision,
                              char *txt, char *refTxt);

/***** VAT file **************************************************************
 *
 * swapAndVerifyVatFile():
 *
 * Check and endian swap for VAT file.
 * The VAT file was introduced in UDF 1.50 but the file layout
 * was changed completely in UDF 2.00.
 *
 * input arguments:  PartitionMapInfo *vPmi,
 *                   fileSize
 *                   vPmi->vatRec->vatIntroRevision
 *
 * output arguments: vPmi->vatRec->vatTable
 *                   vPmi->vatRec->numberOfEntries
 *
 * return result: if VAT file error, then FALSE
 *                                   else TRUE.
 */
extern bool swapAndVerifyVatFile( PartitionMapInfo *vPmi,
                                  Uint32 fileSize32 );


/***** Non-Allocatable Space *************************************************
 *
 * In UDF 1.50 2.3.13 the HEX definition of the
 * Non-Allocatable Space file name is erroneous.
 * We'll stick to "Non-Allocatable Space", however, the erroneous
 * definition will be handled as a lookalike because the' HEAD'
 * is cut off before the 'c'.
 * For a lookalike, only the 'HEAD' of the name is used for compare.
 */
#define NONALLOCNAME150_HEX_ERR   "Non-Alloatable pace"  /* hex in UDF 1.50 */
#define NONALLOCNAME150           "Non-Allocatable Space"       /* UDF 1.50 */
#define NONALLOCNAME150_LOOKALIKE "Non-Allo"                    /* UDF 1.50 */

#define NONALLOCNAME200            UDF_SS_NONALLOC              /* UDF 2.00+ */
                            /**   "*UDF Non-Allocatable Space"  /* UDF 2.00+ */
#define NONALLOCNAME200_LOOKALIKE "*UDF Non-Allo"               /* UDF 2.00+ */

#define NONALLOC_UDFREF_TXT \
    ((getUctMaxUdfRevision() <= 0x150)  \
      ? "UDF 1.50 2.3.13"   \
      : "UDF 2.00+ 3.3.7.2")

/* verifyNonAllocatableSpace():
 * Called by handleNonAllocatableSpace().
 * Non-Allocatable Space node,
 * either a root directory file (UDF 1.50)
 *     or a System Stream (UDF 2.00+).
 * For details, see uctverify.c
 *
 * return value:
 *  If inclomplete verification,
 *  then: FALSE
 *  else: TRUE
 */
extern bool verifyNonAllocatableSpace( UdfMountContext *mc,
                                       Node   *node );

#endif  /* __UCT_UCTVERIFY_H__ */


