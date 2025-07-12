/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctmedium.h
 *
 * Description : Medium info definitions and functions.
 *
 * Author(s)   : Gerrit Scholl
 */

#ifndef __UCT_UCTMEDIUM_H__
#define __UCT_UCTMEDIUM_H__

#include "mytypes.h"
#include "general.h"
#include "udfstruct.h"


/* Medium type values definition.
 * Equal to PDAT_* values, except for MTYPE_WR_UNKNOWN
 */
typedef enum
{
    MTYPE_WR_UNKNOWN      = MAX_UINT32,     /* not PDAT_POW_OR_UNKNOWN !! */
    MTYPE_WR_POW          = PDAT_POW_OR_UNKNOWN,
    MTYPE_WR_READONLY     = PDAT_READONLY,
    MTYPE_WR_WRITEONCE    = PDAT_WRITEONCE,
    MTYPE_WR_REWRITABLE   = PDAT_REWRITABLE,    /* may need preprocessing */
    MTYPE_WR_OVERWRITABLE = PDAT_OVERWRITABLE   /* NO preprocessing */
} MediumWritabilityType;

typedef enum
{
    MTYPE_SE_UNKNOWN = 20,
    MTYPE_SE_SEQUENTIAL,
    MTYPE_SE_NONSEQUENTIAL
} MediumSequentialType;

typedef enum
{
    MTYPE_CL_UNKNOWN = 30,
    MTYPE_CL_FINAL,
    MTYPE_CL_INTERMEDIATE       /* intermediate state */
} MediumClosedType;

/* Text macros for printing, etc.
 */
#define    MTYPE_WR_TEXT(X)     \
    (((X)==MTYPE_WR_UNKNOWN)    \
                    ? "unknown" \
                    : PDAT_TEXT(X))

#define    MTYPE_SE_TEXT(X) \
    (((X)==MTYPE_SE_UNKNOWN)        ? "unknown"       : \
     ((X)==MTYPE_SE_SEQUENTIAL)     ? "sequential"    : \
     ((X)==MTYPE_SE_NONSEQUENTIAL)  ? "nonsequential" : \
                                      "<illegal>")
#define    MTYPE_CL_TEXT(X) \
    (((X)==MTYPE_CL_UNKNOWN) ? "unknown finalization state" : \
     ((X)==MTYPE_CL_FINAL)       ? "finalized"              : \
     ((X)==MTYPE_CL_INTERMEDIATE) ? "intermediate state"    : \
                                    "<illegal>")

/* TrackInfo  structure, basically information delivered by
 * READ TRACK INFORMATION command.
 * Undefined fields have the value zero.
 */
typedef struct
{
    Uint16  trackNumber;
    Uint16  sessionNumber;
    Uint32  trackStartAddress;
    Uint32  trackLength;
    Uint32  trackRecordedLength;
} TrackInfo;

/* GapInfo structure registers ranges of unrecorded sectors in
 * the volume space, like gaps between tracks, freeBlocks and
 * 'background format areas'.
 * GapInfo can be derived from TrackInfo records, that may be
 * delivered at drive initialisation, e.g. see scsiStartDevice().
 * Later, also unrecorded gap information may be retrieved from
 * 'background formatting' administration.
 */
typedef struct
{
    Uint32  gapStart;       /* logical sector address */
    Uint32  gapLength;      /* nmb of logical sectors */
} GapInfo;

/* Medium info structure containing values that can be
 * read from a medium by physical devices, or be defined
 * in image configuration files or command line options.
 */
typedef struct
{
    MediumWritabilityType writabilityType;      /* for values */
    MediumSequentialType  sequentialType;       /*   see      */
    MediumClosedType      closedType;           /*     above  */
    Uint32                blockSize;
    Uint32                eccLength;            /* (ECC) blocking factor */
    Uint32                lastValidBlockNr;
    Uint32                lastRecordedBlockNr;
    Uint32                numberOfSessions; /* in sessionStartBlocks */
    Uint32               *sessionStartBlocks;       /* array pointer */
    Uint32                verifySession;
    Uint32                dummySessionTotalBlocks; /* image file only */
                                 /****** 1.0r5 release extension */
    TrackInfo            *trackTable;           /* array pointer */
    Uint32                numberOfTracks;       /* in trackTable */
    GapInfo              *gapTable;             /* array pointer */
    Uint32                numberOfGaps;         /* in gapTable   */
                                 /****** 1.1r0 release extension */
    Uint32                bgFormatStatus;       /* MMC-4 6.26.2.13 */
                                 /****** 1.3r0 release extension */
    bool                  isOTP;     /* ML: OTP, else PTP or Single Layer */
    Uint32                L0capacity;  /* Multi-Layer: Layer zero capacity */
                                 /* if L0capacity == 0, then Single Layer */
} MediumInfo;

/* Medium Info defines
 */
#define MI_DEFAULT_BLOCKSIZE    2048
#define MI_DEFAULT_ECC_BLOCKING   16    /* sectors */

#define MI_OTP_TEXT(X)  ((X) ? "OTP" : "PTP")

/***** Medium Info functions *****************************************
 */

/* miFreeArrays():
 * Prepare MediumInfo structure for destroy by freeing all internal
 * arrays, etc. No free() of MediumInfo structure itself !!!
 * Separately use checkFree() for that purpose.
 * No consitency check of resulting MediumInfo structure.
 */
extern void miFreeArrays(MediumInfo *mi);

/* printMediumInfo():
 * print MediumInfo structure
 */
extern void printMediumInfo(const MediumInfo *mi);

/* clearMediumInfo():
 * Fill in initial values that will later be changed by information
 * read from the medium by a device, or by command line options.
 * Changing these values is not seen as "overruling" by
 * parseMediumOptions() and mergeMediumInfo().
 * If the values set here are not changed before use, they should
 * be set to their default values using finishMediumInfo().
 */
extern void clearMediumInfo(MediumInfo *mi);

/* finishMediumInfo():
 * Set defaults and check internal consistency of a
 * MediumInfo structure.
 *
 * If MediumInfo fields are still equal to their initial values as
 * set by clearMediumInfo(), then change those fields to their
 * default values.
 *
 * Special for lastValidBlockNr:
 * if mi->verifySession not equal to the last session,
 * then mi->lastValidBlockNr will be set to the
 * highest value in the verify session and a warning
 * will be printed.
 *
 * Return value: FALSE if inconsistency found, else TRUE.
 */
extern bool finishMediumInfo(MediumInfo *mi);

/* mergeMediumInfo():
 * Merge MediumInfo structures.
 * Copy values of primary or secondary MediumInfo structures to
 * the output MediumInfo structure. If the primary and secondary
 * value of a field are unequal, the value that is unequal to the
 * value set by clearMediumInfo() will be written to the output
 * structure. If the values are unequal and both values are unequal
 * to the value set by clearMediumInfo(), the primary value will
 * overrule the secondary one and be written to the output structrure.
 * In the latter case, a merge warning message like:
 *      "  Merge warning: .... from <txt02>"
 *      "   overruled by: .... from <txt01>"
 * will be printed and the return value of this function will
 * be set to TRUE.
 *
 * Note that the "output" MediumInfo pointer is allowed
 * to be equal to one of the "primary" or "secondary"
 * input pointers.
 *
 * Return value: TRUE if any member was overruled,
 *         else: FALSE
 *
 * Note: The "secondary" pointer may be NULL.
 *       If so, the "primary" MediumInfo is copied
 *       to "output".
 *
 * NOTE: No merge for numberOfTracks, trackTable,
 *                    numberOfGaps and gapTable,
 *  because they cannot be defined on the command line
 *  or in a configuration file, so merge is not relevant.
 *  They are however copied to output if primary == NULL.
 */
extern bool mergeMediumInfo(MediumInfo *primary,
                            MediumInfo *secondary,
                            MediumInfo *output,
                            char       *txt01,
                            char       *txt02);

/* addSessionToMediumInfo():
 * Maintains MediumInfo sessionStartBlocks and
 * numberOfSessions consistency.
 *
 * Consistency rules:
 * - numberOfSessions reflects the number of elements
 *   in sessionStartBlocks[].
 * - the initial state (set by clearMediumInfo())
 *   will be: numberOfSessions == 0
 *       and: sessionStartBlocks == NULL
 * - sessionStartBlocks[] will always be in a so called
 *   "sort unique" state, so no duplicates.
 * - if numberOfSessions != 0, sessionStartBlocks[0] will
 *   be equal to 0, so a session with startblock 0 will
 *   implicitly be added.
 *
 * Actions:
 * - handle all dynamic memory allocation for the
 *   mi->sessionStartBlocks array.
 * - add new session startblock to mi->sessionStartBlocks array,
 * - if first one, then implicitly add session startblock 0.
 * - sort unique mi->sessionStartBlocks
 * - update mi->numberOfSessions.
 *
 * NOTE: Be aware of the fact that numberOfSessions may be
 *       equal to 1 or 2 after the first call, and that it
 *       does not change if a duplicate startBlock is added.
 */
extern void addSessionToMediumInfo( MediumInfo *mi,
                                    Uint32      startBlock );

/* addTrackToMediumInfo():
 * Add new TrackInfo record to MediumInfo
 * like addSessionToMediumInfo() above for sessions.
 */
extern void addTrackToMediumInfo( MediumInfo *mi,
                            Uint16 trackNumber,
                            Uint16 sessionNumber,
                            Uint32 trackStartAddress,
                            Uint32 trackLength,
                            Uint32 trackRecordedLength );

/* addGapToMediumInfo():
 * Add new GapInfo record to MediumInfo
 * like addSessionToMediumInfo() above for sessions.
 * Addition of gap above lastValidBlockNr is ignored.
 */
extern void addGapToMediumInfo( MediumInfo *mi,
                            Uint32 gapStart,
                            Uint32 gapLength );

/* gapFindFirstUnrecordedBlock():
 * Inspect a block range for unrecorded gaps in order to
 * find the first unrecorded block number in that range.
 * output argument:
 *  If an unrecorded block is found in the block range,
 *  then assign the lowest unrecorded block number
 *  to (*pFirstUnrecorded)
 *  else (*pFirstUnrecorded) is unchanged.
 *
 * returns TRUE if a value is assigned to (*pFirstUnrecorded)
 *    else FALSE.
 */
extern bool gapFindFirstUnrecordedBlock(
                        const MediumInfo *mi,
                        Uint32  firstBlockInRange,
                        Uint32  nmbBlocksRange,
                        Uint32 *pFirstUnrecorded);

/* gapFindFirstRecordedBlock():
 * Inspect a block range for unrecorded gaps in order to
 * find the first recorded block number in that range.
 * output argument:
 *  If a recorded block is found in the block range,
 *  then assign the lowest recorded block number
 *  to (*pFirstRecorded)
 *  else (*pFirstRecorded) is unchanged.
 *
 * returns TRUE if a value is assigned to (*pFirstRecorded)
 *    else FALSE.
 */
extern bool gapFindFirstRecordedBlock(
                        const MediumInfo *mi,
                        Uint32  firstBlockInRange,
                        Uint32  nmbBlocksRange,
                        Uint32 *pFirstRecorded);

/* gapFindLastRecordedBlock():
 * Inspect a block range for unrecorded gaps in order to
 * find the last recorded block number in that range.
 * output argument:
 *  If a recorded block is found in the block range,
 *  then assign the highest recorded block number
 *  to (*pLastRecorded)
 *  else (*pLastRecorded) is unchanged.
 *
 * returns TRUE if a value is assigned to (*pLastRecorded)
 *    else FALSE.
 */
extern bool gapFindLastRecordedBlock(
                        const MediumInfo *mi,
                        Uint32  firstBlockInRange,
                        Uint32  nmbBlocksRange,
                        Uint32 *pLastRecorded);

/***** Verifier theMediumInfo functions ******************************
 */

/* getTheMediumInfo():
 * Get pointer to verifier MediumInfo structure.
 * This structure is initialized using initTheMediumInfo()
 * and its data can only be modified using the
 * modifyTheMedium...() functions.
 *
 * This function will take an uninitialized verifier
 * MediumInfo structure as a fatal error and will in
 * that case perform an uctExit(EXIT_PROGRAM_ERROR).
 *
 * Return value: pointer to the verifier MediumInfo structure.
 */
extern const MediumInfo *getTheMediumInfo();

/* initTheMediumInfo():
 * Initialize verifier theMediumInfo structure
 *
 * Return value: FALSE if error found, else TRUE
 */
extern bool initTheMediumInfo(MediumInfo *mi);

/* modifyTheMediumType<xx>():
 * Modify medium type in verifier theMediumInfo because some
 * restricting construction was found during verify.
 * Print messages in case of change or conflict.
 *
 * Return value, FALSE in case of conflict, else TRUE;
 *
 * Three separate functions for medium WR, SE and CL type.
 */
/* For medium WR type:
 * Note that MTYPE_WR_UNKNOWN and PDAT_POW_OR_UNKNOWN
 *      ARE NOT EQUAL VALUES !!
 */
extern bool modifyTheMediumTypeWr( MediumWritabilityType typeWr,
                                   char *reason );
extern bool modifyTheMediumTypeSe( MediumSequentialType typeSe,
                                   char *reason );
extern bool modifyTheMediumTypeCl( MediumClosedType typeCl,
                                   char *reason );

/* getEccPacketNr():
 * get ECC packet number from sector or block address.
 * Do not call before eccLength is set.
 */
extern Uint32 getEccPacketNr( Uint32 ad );

/******* multi layer functions used for 'far apart' calculations.
 ******* for PTP, only Dual Layer calculations are implemented,
 *******          (all >= L0Capacity is on layer L1).
 *******/

/* mlGetLayerNumber():
 * returns layer number where address ad is recorded,
 * 0 for L0 or for a single layer disc,
 * 1 for L1,
 * ... etc.
 * IMPLEMENTATION NOTES:
 *  For OTP, this function returns a valid result for
 *  more than 2 layers.
 *  For PTP, the result is 0 or 1 for single or dual layer.
 *  For PTP, mlGetLayerNumber(ad) will only give a guaranteed correct
 *  result if ad is a real address (not a 'L0 equivalent address').
 *  details:
 * PTP PITFALL: !!!
 *  Note that for PTP the capacity on L1 may be larger than on L0 !!
 *  This may result in the fact that translation of an address ad on L1
 *  to a 'L0 equivalent address' on L0 may result in a value resultAd
 *  that is still >= L0Capacity !!!!!
 *  mlGetLayerNumber(resultAd) will still return 1 in that case.
 *  For 'L0 equivalent address' and other details, see also
 *  mlTranslateToLayer() explanation.
 */
extern Uint32 mlGetLayerNumber( Uint32 ad );

/* mlOnSameLayer(): Return TRUE iff ad0 and ad1 are on same layer.
 *
 * NOTE: For PTP, only correct result is guaranteed if ad0 and ad1
 *       are real addresses, see mlGetLayerNumber() explanation.
 */
extern bool mlOnSameLayer( Uint32 ad0, Uint32 ad1 );

/* mlShowAndAssertLayers():
 */
extern bool mlShowAndAssertLayers();

/* mlFaCalcDistance():
 * Calculate lowest physical sector distance (*pSecDist) and ECC
 * packet distance (*pEccDist) between any sector in ExtentAd ex0
 * and any sector in ExtentAd ex1.
 * Also calculate the logical sector addresses (*pAd0) and (*pAd1)
 * for which this occurs.
 * Takes into account the fact that e.g. for a Multi-Layer medium,
 * these physical distances can be zero for quite different values
 * of (*pAd0) and (*pAd1), where these are each on different layers
 * on an identical radius of a ML medium.
 *
 * Implementation notes: see uctmedium.c
 */
extern void mlFaCalcDistance( ExtentAd  *ex0,
                              ExtentAd  *ex1,
                              Uint32    *pAd0,
                              Uint32    *pAd1,
                              Uint32    *pSecDist,
                              Uint32    *pEccDist );

#endif /* __UCT_UCTMEDIUM_H__ */

