/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctgeneral.h
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __UCT_UCTGENERAL_H__
#define __UCT_UCTGENERAL_H__

#include "mytypes.h"
#include "general.h"
#include "crc.h"
#include "unicode.h"
#include "ucterror.h"
#include "device.h"
#include "udfstruct.h"
#include "uctdata.h"
#include "uctnodes.h"


#define uctout stdout       /* error/warning/info output */

/* warn for very big volumes/partitions.
 * do not create bitmaps or descriptors of more than 16 Mbyte
 */
#define UCT_MAXVOLUME_GBSIZE         1024   /** 1 TB **/
#define UCT_MAXVOLUME_BYTESIZE  ((Uint64)UCT_MAXVOLUME_GBSIZE*GbBYTES)

#define UCT_MAXPARTITION_GBSIZE     UCT_MAXVOLUME_GBSIZE
#define UCT_MAXPARTITION_BYTESIZE   UCT_MAXVOLUME_BYTESIZE

#define UCT_MAXBITMAP_MBSIZE         64 /** == (UCT_MAXVOLUME_GBSIZE / 16) **/
#define UCT_MAXBITMAP_BYTESIZE  ((Uint32)UCT_MAXBITMAP_MBSIZE*MbBYTES)

#define UCT_MAXDESCRIPTOR_MBSIZE    (1 + UCT_MAXBITMAP_MBSIZE)  /* e.g. SBD */
#define UCT_MAXDESCRIPTOR_BYTESIZE  ((Uint32)UCT_MAXDESCRIPTOR_MBSIZE*MbBYTES)

/* some static system asserts, etc.
 */
extern bool uctInitialize();

extern bool readVolumeRecognitionSequence(Device *device,
                                          Uint32 verifySessionStart);

/* Read Main or Reserve VDS extent and continuation extents if any.
 * In case of an error, try reading till end of extent.
 */
extern bool readVolumeDescriptorSequence(
                UdfMountContext *mc, UdfVolumeInformation *vi,
                Uint32 firstBlock, Uint32 extentLength, /* length in bytes */
                bool   isMainVds );

extern UdfAllocationList *createNewAllocationList(Uint8 adType);
extern bool alAddDescriptor(UdfAllocationList *al,
                            AnyAllocationDescriptor *ad,
                            bool toOverheadList);
/* allocationListFree():
 * Free all space occupied by an UdfAllocationList and set the
 * pointer pointing at it to NULL [set (*pAl) to NULL].
 */
extern void allocationListFree(UdfAllocationList **pAl);

extern void nodeFreeAllocationLists(Node *node);
extern bool nodeFindAllocationDescriptor(Node* node, Uint64 bytePos,
                                         UdfAllocationItem **ai,
                                         Uint32 *offset);
extern bool nodeReadAllocationDescriptors(UdfMountContext *mc, Node *node);

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
 *
 * Note non-standard return value.
 */
extern bool calculateOverlap(
                    Uint32  start1,         Uint32  blockLen1,
                    Uint32  start2,         Uint32  blockLen2,
                    Uint32 *pStartOverlap,  Uint32 *pLenOverlap);

/* translateVirtualAddress() translates a virtual address in a
 * virtual partition into a logical address and a physical
 * partition.
 *
 * return value: TRUE if translation ok,
 *         else: FALSE
 */
#define VAT_UNUSED_ENTRY    MAX_UINT32

extern bool translateVirtualAddress(PartitionMapInfo *virtualPmi,
                                    Uint32            virtualAddress,
                                    Uint32           *pLogicalAddress,
                                    Uint16           *pPhysPRef,
                                    bool              silent);

/* Translates virtual or logical address to absolute address.
 * A second result is the number of contiguous blocks that can be
 * read without the need for an intermediate address translation
 * as performed by this function.
 * Exception:
 *  If address falls in sparse metadata extent, then the
 *  special value of LBA_SPARSE_EXTENT is returned
 *  for (*pAbsoluteAddress).
 */
#define LBA_SPARSE_EXTENT MAX_UINT32

extern bool translateAddress(UdfMountContext *mc,
                             Uint16  partRefNumber,
                             Uint32  logicalAddress,
                             Uint32 *pAbsoluteAddress,
                             Uint32 *pContiguousBlocks,
                             bool    silent);

/* readBlocksFromPartition():
 * returns actually correctly read nmb of blocks
 * fake read if buffer == NULL
 *
 * TODO: Correct for multi-partition case
 *       SINGLE VOLUME CASE
 *       MV case; Find the right DISC!
 * TODO: translate for sparable partition,
 *       translate (and maybe) read for each sector/packet !!!
 */
extern Uint32 readBlocksFromPartition( UdfMountContext *mc,
                                        Byte  *buffer,
                                        Uint16 partRefNr,
                                        Uint32 logicalBlockNr,
                                        Uint32 nrOfBlocks );

extern bool convertChar2Unicode(Byte *toBeConverted, unicode_t **converted);
extern bool convertUnicode2Char(unicode_t *toBeConverted, Uint8 lengthOfUnicodeInBytes,
                         Byte *converted);

extern char *partitionSpaceSetText(PartitionSpaceSet *pss);

/* printAvdpLocationText():
 * check special locations:
 * print "256", "S+256", "N-256", "N", 512 or "S+512"
 * if nothing fits, then print the absolute location block address.
 */
extern void printAvdpLocationText( Uint32 location,
                                   char  *trailText );

extern bool udfGetVolumeInformation(UdfMountContext *mc);
extern void udfFreeVolumeInformation(UdfVolumeInformation *vi);

extern bool udfMountLogicalVolume(UdfMountContext *mc,
                                  Timestamp *pVerifyStartTime);

extern void udfUnmountLogicalVolume(UdfMountContext *mc);


/* print Dstring macro,
 * print Unicode chars between "" quotes,
 * DSTR is Dstring pointer, evaluated once.
 * DSIZE must be a constant of at least 2.
 * Max nmb of unicode chars: DSIZE-2
 * It is tried to print a sensible output in case
 * of erroneous Dstrings, see uncompressDstring().
 * No real error checks here.
 */
#define PRINTDSTRING(DSTR, DSIZE, TXT)  \
{   unicode_t uName[(DSIZE)-2];         \
    Dstring  *dstr    = (DSTR);         \
    char     *endText = (TXT);          \
    int       uLen;                     \
                                        \
    if( verifyZeros(dstr, (DSIZE), NULL, NULL, NULL) )  \
         VERBOSE00(uctout, "<undefined>");              \
    else if( (uLen = uncompressDstring((DSIZE), dstr, uName)) == -1 )  \
         VERBOSE00(uctout, "<uncompress Dstring error>");              \
    else printUnicodeName( uName, uLen, TRUE, FALSE, VERBOSE00level ); \
                                        \
    if( endText != NULL )               \
    { VERBOSE00(uctout, "%s", endText); \
    }                                   \
}       /* end PRINTDSTRING() macro */


#endif /* __UCT_UCTGENERAL_H__ */

