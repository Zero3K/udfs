/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctfiles.h
 *
 * Description : File structure functions and definitions.
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __UCT_UCTFILES_H__
#define __UCT_UCTFILES_H__


/* get node icb, handle exceptions
 */
extern LongAd *nodeGetIcb(Node *node, UdfMountContext *mc);

/* NEWNODE() macro:
 * Create and initialize a new Node or FileNodeInfo structure.
 * Use NEWSTRUCT(). All fields will be initialized on 0, NULL, etc.
 * For free Node use nodeFreeHierarchy().
 * For free FileNodeInfo use nodeFreeFileNodeInfo().
 */
#define NEWNODE()   NEWSTRUCT(Node, 1)

extern void nodeFreeChildren( Node *node );
extern void nodeFreeHierarchy( Node *node );

extern bool udfGetLocation( AnyAllocationDescriptor *ad,
                            Uint8   adType,
                            Uint16  shortPartRefNumber,
                            Uint16 *partRefNumber,
                            Uint32 *logicalBlockNr);

/* readVAT: In case of an error, mc allocated memory will be freed
 * in udfMountLogicalVolume(), using udfUnmountLogicalVolume().
 */
extern bool readVAT( UdfMountContext *mc,
                     Uint16 virtualPRef );

/* readIcbDirectEntry:
 * Allocate memory and Read final Direct Entry
 * in an ICB hierarchy. Strategy 4 or or 4096.
 * The Direct Entry can be an (E)FE, USE or TE.
 * Note that an ICB Direct Entry shall not occupy more than one block,
 * so we always allocate a descriptor buffer of exactly one block.
 *
 * return value: TRUE if a correct Direct Entry was read or if a valid
 *                    ICB terminating condition was found.
 *         else: FALSE
 *
 * Exception: if (node != NULL), the result buffer will also be assigned
 * to node->fe and the node->fe* context will be maintained for use
 * in swapAndVerifyDescriptor(). [swapAndVerifyDescriptor() wil fail for
 *                                (E)FE if node or node->fe == NULL !!!!!!]
 * It is assumed that (node == NULL) if no (E)FE is expected.
 * For valid ICB terminating condition see acceptAs4096ICBtermination().
 * If pIsStrategy4096 != NULL, an appropriate boolean result is returned
 * in (*pIsStrategy4096).
 */
extern bool readIcbDirectEntry( UdfMountContext *mc,
                                Node    *node,
                                Uint32   logicalBlock,
                                Uint16   partRef,
                                Uint32   extentSize,
                                Uint16  *pTagIdentifier,
                                bool    *pIsStrategy4096,
                                Byte   **ppDirectEntry,
                                Uint32   readIcbDirectRecursionCount);

/* cmpMetadataFiles():
 * Compare blocks in Metadata File and its mirror.
 * Determine which bitmap to use and call
 * cmpMetadataFilesUsingBitmap().
 *
 * By default, the bitmap from the Metadata Bitmap File is used.
 * If not present (e.g. for read-only partition),
 * then the bitmap created by the verifier is used.
 * Because of the latter, cmpMetadataFiles() can not be
 * executed before the verifier has created the bitmap.
 */
extern bool cmpMetadataFiles(UdfMountContext *mc);

/* readMetadataFiles: In case of an error, mc allocated memory will
 * be freed in udfMountLogicalVolume(), using udfUnmountLogicalVolume().
 */
extern bool readMetadataFiles( UdfMountContext *mc,
                               Uint16 metadataPRef );

/* readMetadataBitmapFile:
 * read Metadata Bitmap File (introduced in UDF 2.50).
 * Also some metadata bitmap specific tests.
 * Returns value: pointer to SBD, NULL on failure.
 *  further: file size in mc->metadataBitmapFile->size32
 *           and the SBD size in (*pSBDsize).
 */
extern SpaceBitmapDescriptor *readMetadataBitmapFile(
                                    UdfMountContext *mc,
                                    Uint16           metadataPRef,
                                    Uint32          *pSBDsize );

/* getMetadataBitmapFileSBDtagLocation():
 * pre-condition:
 *  preceding call of readMetadataBitmapFile()
 *  returned TRUE.
 *  (meaning mc->metadataBitmapFile != NULL, etc.
 */
extern Uint32 getMetadataBitmapFileSBDtagLocation(
                                UdfMountContext *mc);

/* test if string matches with node->unicodeName,
 * or if it matches with the 'head' of node->unicodeName.
 * temporarily conversion of string FName to unicode.
 */
extern bool stringIsUnicodeName( Node *node, char *fName,
                                 bool *pHeadMatches );

/* allocBlockBuffer() TRIES to allocate a buffer of size
 * as indicated by reqSize. If this fails, the size is
 * divided by 2 and then rounded up to the nearest multiple of
 * the blockSize and then allocation is tried again.
 * This repeats untill a succesfull allocation is achieved or
 * the size is less than or equal to the blockSize.
 *
 * If an allocation attempt is successful, but an earlier
 * attempt failed, the buffer will be reduced with an extra
 * factor 4 (rounded up to the blockSize), in order to avoid
 * swapping and allocation of too much of the available memory.
 *
 * iff allocation was succcessful then return the buffer
 *      pointer and the allocated size in (*pAllocSize)
 * else return NULL and an UNDEFINED value in (*pAllocSize);
 */
extern Byte *allocBlockBuffer( Uint32  reqSize,
                               Uint32 *pAllocSize );


extern bool checkFileStructure( UdfMountContext *mc );

#endif  /* __UCT_UCTFILES_H__ */

