/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctstatus.h
 *
 * Description : UDF verifier status and status access functions.
 *
 * Author(s)   : Gerrit Scholl
 */

#ifndef __UCT_UCTSTATUS_H__
#define __UCT_UCTSTATUS_H__

#include "mytypes.h"
#include "unicode.h"
#include "udfstruct.h"
#include "uctnodes.h"


extern void notifyUdfNsrFound();
extern void notifyUdfCorrectDescriptorFound();

/* UDF Revision Range covered by this verifier
 *
 * NOTE: Inspect ALL use of MIN_UDFREVISION or
 *       MAX_UDFREVISION if one of the 2 values changes !!
 */
#define MIN_UDFREVISION (0x102)     /* UDF 1.02 */
#define MAX_UDFREVISION (0x260)     /* UDF 2.60 */

extern Uint16 getUctMinUdfRevision();
extern Uint16 getUctMaxUdfRevision();

/* getUctUdfRevision: return UDF revision.
 * return value:
 * if uctMaxUdfRevision != uctMinUdfRevision
 * then 0
 * else uctMaxUdfRevision
 */
extern Uint16 getUctUdfRevision();

/** warnIfUdfRevisionUncertain():
 ** For many tests, the UDF verifier can do a better job
 ** if the exact UDF revision of the medium is known instead
 ** of a UDF revision range.
 ** For tests where the exact UDF revision is needed, but not
 ** yet knwon, execute this function first as a warning and
 ** then execute the test at hand on the safe side using
 ** getUctMinUdfRevision() or getUctMaxUdfRevision().
 ** This means that the test may produce an error/warning message
 ** while not strictly needed if the exact UDF revision was known.
 **
 ** This function prints a warning if the exact UDF revision is
 ** not yet known and the revisions tstLow and tstHigh both fall
 ** inside the currently known UDF revision range (the uncertain range),
 ** where tstLow < tstHigh.
 ** E.g. for a test where it is important to know if the medium
 **      UDF revision is (2.00 and lower) or (2.01 and higher),
 **      call: warnIfUdfRevisionUncertain(0x200, 0x201);
 **/
extern void warnIfUdfRevisionUncertain( Uint16 tstLow,
                                        Uint16 tstHigh );

/* isKnownUdfRevision:
 * return TRUE if revision is member of
 * knownUdfRevisions[] else return FALSE
 */
extern bool isKnownUdfRevision(Uint16 revision);

/* printUdfRevision():
 * Print UDF revision string and additional text.
 * revision is coded in hex (BCD in fact),
 * e.g. 0x200 for UDF revision 2.00
 *
 * Return value: FALSE for unregistered revision,
 *         else: TRUE.
 */
extern bool printUdfRevision(Uint8 vLevel, Uint16 revision,
                             char *additionalText);

extern void printUdfRevisionRange(Uint8 vLevel, char *endTxt);

/* Modify the current verifier UDF revision range
 *  (static uctMinUdfRevision to uctMaxUdfRevision included).
 * Proposed UDF revision range :
 *  (minRevision to maxRevision included).
 *
 * Modify only by shrinking to the overlapping range.
 * There is a UDF revision range conflict if both
 * ranges do not overlap.
 *
 * return value: FALSE for a UDF revision range conflict,
 *         else: TRUE
 */
extern bool modifyUdfRevisionRange(Uint16 minRevision, Uint16 maxRevision,
                                   char  *reason);

/* checkDescriptorVersion()
 * verify Descriptor Version and handle status,
 * ECMA-167 3rd edition 3/7.2.2, 4/7.2.2.
 *          ===
 */
extern bool checkDescriptorVersion( Tag *t, Node *node,
                                    UdfMountContext *mc );

/* EntityID stuff ****************************************************
 *
 * printEntityID():
 * Print EntityId
 * Return value: nmb of errors found.
 *
 * Note:
 *  MLIMIT... macros are not used here because printEntityID
 *  is only called twice for each EntityId, once at first
 *  occurrence and once from printEntityIDSummary().
 *  When first called, the total count for this EntityId is
 *  unknown and therefore, the value of <eidCount> shall be 0.
 *  When called from printEntityIDSummary(), the value for
 *  <eidCount> will be the actual EntityId count (>0)
 *  (EntityIDListItem.count) from the EntityID list.
 *  In the latter case, the global error and warning counts
 *  will be updated properly. Note that this is independent
 *  of the value of vLevel;
 */
extern int printEntityID( EntityID *eid, ENTITY_SUFFIX_TYPE suffixType,
                          Uint32 eidCount, Uint8 vLevel );

/* storeAndPrintNewEntityID():
 * Maintains a linked list of EntityID's encountered and prints the
 * newly added ones. Maintains sorted order and occurrence count as well.
 * Primary sort on ENTITY_SUFFIX_TYPE, secondary on EntityID bytes.
 * A summary of all info in the linked list can be
 * printed using printEntityIDSummary();
 *
 * Return value: FALSE if a new EntityID is added to the linked list
 *                     AND that EntityID contains errors
 *          else TRUE;
 *
 * So an erroneus EntityID is only flagged once when it is encountered
 * for the first time (and in the printEntityIDSummary()).
 */
extern bool storeAndPrintNewEntityID(EntityID *eid,
                                     ENTITY_SUFFIX_TYPE suffixType,
                                     Byte *descriptor);
/* printEntityIDSummary():
 * Prints summary of all info in the linked list
 * built by storeAndPrintNewEntityID().
 */
extern void printEntityIDSummary();


/* Disaster Recovery Support for Tag Serial Numbers ******************
 *
 * resetTagSerialNumberSupport():
 */
extern void resetTagSerialNumberSupport();


/* Print final status report.
 */
extern void printFinalStatusReport(bool fatalError);


/* General Print Functions *******************************************
 */


/* printUnicodeName() :
 * Simple print unicode name on uctout.
 * Some characters may be printed in hex "#HHHH" form,
 * see printUnicodeAndBytesName().
 * Return value:
 *  Number of real byte chars printed (not unicode chars !!).
 *  Mind that this can be zero if verbose level does not apply.
 */
extern Uint32 printUnicodeName( unicode_t *uName, Uint32 nameLen,
                                bool printTrailZeros,
                                bool isPath,      Uint8 vLevel );

/* printBytesName() :
 * Simple print byte characters name on uctout.
 * Some characters may be printed in hex "#HH" form,
 * see printUnicodeAndBytesName().
 * Return value:
 *  Number of real byte chars printed (not unicode chars !!).
 *  Mind that this can be zero if verbose level does not apply.
 */
extern Uint32 printBytesName( Byte *bName, Uint32 nameLen,
                              bool  printTrailZeros, Uint8 vLevel );

/* Low level general functions ***************************************
 */

/* verifyZeros():
 * Test block of bytes that shall all have
 * the value #00.
 *
 * Input arguments:
 * beginZeros: pointer to first byte to be checked.
 * nBytes    : nmb of bytes to be checked.
 *
 * Output arguments:
 * *pCount     : nmb of bytes unequal to zero found
 * *pFirstIndex: index of first byte unequal to zero
 * *pFirstValue: value of first byte unequal to zero
 *
 * The pointers pCount, pFirstIndex and pFirstValue may
 * each be NULL, independent of each other, which means
 * that no value will be returned.
 *
 * Return value: TRUE if no byte unequal to zero is found,
 *         else: FALSE.
 * Note: *pFirstIndex and *pFirstValue are undefined if
 *       TRUE is returned.
 */
extern bool verifyZeros(Byte   *beginZeros, size_t  nBytes,
                        size_t *pCount,     Uint32 *pFirstIndex,
                        Byte   *pFirstValue);

/* checkVolumeIdentifiersSummary():
 * Show final summary of Volume, Volume Set,
 * Logical Volume and File Set identifiers.
 *
 * Check relation of logicalVolumeIdentifier in LVD, IUVD, FSD
 * and VAT2 checked in checkVolumeIdentifiersSummary().
 *
 * Show checkLogVolIds() errors and warnings at each call of
 * checkVolumeIdentifiersSummary(), but only count
 * global error/warning occurrences at first call.
 */
extern void checkVolumeIdentifiersSummary( UdfMountContext *mc );

#endif /* __UCT_UCTSTATUS_H__ */

