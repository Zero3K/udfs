/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctmedium.c
 *
 * Description : Medium info definitions and functions.
 *
 * Author(s)   : Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "general.h"
#include "uctgeneral.h"
#include "uctstatus.h"
#include "uctmedium.h"

/* Verifier MediumInfo structure theMediumInfo
 * Initialized by initTheMediumInfo()
 * Modified by modifyTheMedium...() functions
 * A pointer to theMediumInfo for read-only purposes
 * can be obtained using getTheMediumInfo.
 */
static MediumInfo theMediumInfo;
static bool       theMediumInfoIsInitialized = FALSE;

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
extern const MediumInfo *getTheMediumInfo()
{
    if( !theMediumInfoIsInitialized )
    {   MLIMITbegin(ERROR00level, uctMessageLimit); /* dummy */
        MLIMITend;
        VERBOSE00(uctout, "getTheMediumInfo fatal error: "
            "Verifier MediumInfo not initialized, please report\n");
        uctExit(EXIT_PROGRAM_ERROR);
    }
    return &theMediumInfo;
}

/* miFreeArrays():
 * Prepare MediumInfo structure for destroy by freeing all internal
 * arrays, etc. No free() of MediumInfo structure itself !!!
 * Separately use checkFree() for that purpose.
 * No consitency check of resulting MediumInfo structure.
 */
extern void miFreeArrays(MediumInfo *mi)
{
    checkFree((void **)&mi->sessionStartBlocks);    /* NULL */
    mi->numberOfSessions = 0;
    checkFree((void **)&mi->trackTable);            /* NULL */
    mi->numberOfTracks = 0;
    checkFree((void **)&mi->gapTable);              /* NULL */
    mi->numberOfGaps = 0;
 }

/* printMediumInfo():
 * print MediumInfo structure
 */
extern void printMediumInfo(const MediumInfo *mi)
{Uint32 u;

    /* use 24 char (3 tabs) before ':'
     */
    if( mi->dummySessionTotalBlocks != 0 )
    { VERBOSE00(uctout,
        "     dummy blocks\t: %lu\n",
            mi->dummySessionTotalBlocks);
    }

    /* show lastValidBlockNr and lastRecordedBlockNr
     * (only if unequal to lastValidBlockNr).
     */
    VERBOSE00(uctout,
        "  last valid block\t: %lu   Volume Space: ",
            mi->lastValidBlockNr);
    nBytesDoublePrint4f(
            ((Uint64)mi->lastValidBlockNr + 1) * mi->blockSize,
                        "\n");

    if( mi->lastRecordedBlockNr != mi->lastValidBlockNr)
    { VERBOSE00(uctout,
        "  last recorded block\t: %lu\n",
            mi->lastRecordedBlockNr);
    }

    /* show L0capacity (and isOTP) only if non zero
     */
    if( mi->L0capacity != 0 )       /* ML */
    { VERBOSE00(uctout,
        "  L0 capacity ML %3s\t: %lu   sectors     : ",
                MI_OTP_TEXT(mi->isOTP), mi->L0capacity );
      nBytesDoublePrint4f(
          (Uint64) mi->L0capacity * mi->blockSize, "\n");
    }

    VERBOSE00(uctout,
        "  block size\t\t: %lu\n"
        "  (ECC) blocking factor : %lu\n"
        "  nmb of sessions\t: %lu\n"
        "  verify session\t: %lu\n",
            mi->blockSize,
            mi->eccLength,
            mi->numberOfSessions,
            mi->verifySession);

    /* Session startBlocks, max 6 on a line
     */
    if( mi->numberOfSessions == 0 )
    { VERBOSE00(uctout, "  no sessions defined\n");
    }
    else
    { VERBOSE00(uctout,
        "  session starts\t: ");
      for( u = 0; u < mi->numberOfSessions; u++ )
      { if( u != 0 && (u % 6) == 0 )
        { VERBOSE00(uctout, "\n\t\t\t  ");
        }
        VERBOSE00(uctout, "%-7u ", mi->sessionStartBlocks[u]);
      }
      VERBOSE00(uctout, "\n");
    }

    /* Show track starts if more than one exists
     * max 6 on one line.
     */
    if( mi->numberOfTracks > 1 )
    { VERBOSE00(uctout,
        "  track starts\t\t: ");
      for( u = 0; u < mi->numberOfTracks; u++ )
      { if( u != 0 && (u % 6) == 0 )
        { VERBOSE00(uctout, "\n\t\t\t  ");
        }
        VERBOSE00(uctout, "%-7u ",
                mi->trackTable[u].trackStartAddress);
      }
      VERBOSE00(uctout, "\n");
    }

    /* Gap message, only if one exists, max 4 on one line
     */
    if( mi->numberOfGaps != 0 )
    { VERBOSE00(uctout,
        "  unrecorded gaps\t: ");
      for( u = 0; u < mi->numberOfGaps; u++ )
      { if( u != 0 && (u % 4) == 0 )
        { VERBOSE00(uctout, "\n\t\t\t  ");
        }
        VERBOSE00(uctout, "(%lu-%lu) ",
                mi->gapTable[u].gapStart,
                mi->gapTable[u].gapStart
              + mi->gapTable[u].gapLength - 1);
      }
      VERBOSE00(uctout, "\n");
    }

    /* bgFormatStatus only if not zero
     */
    if( mi->bgFormatStatus != 0 )
    { VERBOSE00(uctout,
        "  BG format status\t: %s  (%u)\n",
              (mi->bgFormatStatus == 1)     ? "Incomplete"
            : (mi->bgFormatStatus == 2)     ? "In progress"
           /** mi->bgFormatStatus == 3 **/  : "Complete",
               mi->bgFormatStatus);
    }

    /* medium CL type is relevant for finalized/intermediate state
     */
    VERBOSE00(uctout,
        "  medium WR type\t: %s\n"
        "  medium SE type\t: %s\n"
        "  medium CL type\t: %s\n",
            MTYPE_WR_TEXT(mi->writabilityType),
            MTYPE_SE_TEXT(mi->sequentialType),
            MTYPE_CL_TEXT(mi->closedType));

}   /* end printMediumInfo() */

/* clearMediumInfo():
 * Fill in initial values that will later be changed by information
 * read from the medium by a device, or by command line options.
 * Changing these values is not seen as "overruling" by
 * parseMediumOptions() and mergeMediumInfo().
 * If the values set here are not changed before use, they should
 * be set to their default values using finishMediumInfo().
 */
extern void clearMediumInfo(MediumInfo *mi)
{
    mi->writabilityType         = MTYPE_WR_UNKNOWN;
    mi->sequentialType          = MTYPE_SE_UNKNOWN;
    mi->closedType              = MTYPE_CL_UNKNOWN;
    mi->blockSize               = 0;
    mi->eccLength           = 0;
    mi->lastValidBlockNr        = 0;
    mi->lastRecordedBlockNr     = 0;
    mi->numberOfSessions        = 0;
    mi->sessionStartBlocks      = NULL; /* array pointer */
    mi->verifySession           = 0;
    mi->dummySessionTotalBlocks = 0;    /* image device only */
    mi->numberOfTracks          = 0;
    mi->trackTable              = NULL;
    mi->numberOfGaps            = 0;
    mi->gapTable                = NULL;
    mi->bgFormatStatus          = 0;
    mi->L0capacity              = 0;
    mi->isOTP                   = TRUE; /* default */
}

/* finishMediumInfo():
 * Set defaults and check internal consistency of a
 * MediumInfo structure.
 *
 * If MediumInfo fields are still equal to their initial values as
 * set by clearMediumInfo(), then change those fields to their
 * default values.
 *
 * Special for lastValidBlockNr:
 * Avoid value MAX_UINT32 (decrement by one).
 * if mi->verifySession not equal to the last session,
 * then mi->lastValidBlockNr will be set to the
 * highest value in the verify session and a warning
 * will be printed.
 *
 * Return value: FALSE if inconsistency found, else TRUE.
 */
extern bool finishMediumInfo(MediumInfo *mi)
{
    Uint32 n, sessionStart, sessionEnd, lastRecorded;

    /* MediumInfo defaults:
     * Medium ... Type defaults are equal
     * to the values set by clearMediumInfo(),
     * so no check for them here.
     */
    if( mi->blockSize == 0 )
        mi->blockSize = MI_DEFAULT_BLOCKSIZE;

    /* No default for lastValidBlockNr.
     * lastValidBlockNr == MAX_UINT32 is tricky because then
     * lastValidBlockNr + 1 does not fit in a Uint32 any more.
     * (TODO: or is this a fatal error ??)
     */
    if( mi->lastValidBlockNr == MAX_UINT32 )
    { MLIMITbegin(WARN01level, MLIMITdefault02);
        fprintf(uctout, "Warning: Last valid block out of range: %lu (0x%X)\n"
                        "-        Value will be decremented by one !!!\n",
                        mi->lastValidBlockNr, mi->lastValidBlockNr);
      MLIMITend;
      mi->lastValidBlockNr--;
    }

    /* Handle numberOfSessions and sessionStartBlocks together.
     * Default: one session with startblock 0.
     * No check on dummySessionTotalBlocks.
     */
    if( mi->numberOfSessions == 0 )
        addSessionToMediumInfo( mi, 0 );

    /* verifySession:
     * minimum: 1 (for first session)
     * default: last session
     */
    if( mi->verifySession == 0 )
        mi->verifySession = mi->numberOfSessions;

    /* Remaining consistency checks:
     */
    if( (mi->blockSize % 512) != 0 )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout, "Error: Block size inconsistency, must"
                " be multiple of 512 bytes: %u\n", mi->blockSize);
      MLIMITend;
      return FALSE;     /* error */
    }

    if( mi->verifySession > mi->numberOfSessions )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout, "Error: verifySession > number of sessions"
                            " (%u, %u)\n",
            mi->verifySession, mi->numberOfSessions);
      MLIMITend;
      return FALSE;     /* error */
    }

    /* handle lastValidBlockNr as last, because session info
     * is consistent now.
     * If verifySession is not last session, then set
     * lastValidBlockNr to last block in verifySession.
     */
    if( mi->verifySession == mi->numberOfSessions ) /* last session */
         sessionEnd = mi->lastValidBlockNr;
    else sessionEnd = mi->sessionStartBlocks[mi->verifySession] - 1;
    sessionStart    = mi->sessionStartBlocks[mi->verifySession-1];

    if( mi->lastValidBlockNr == 0 )             /* undefined */
    {
        if( mi->verifySession == mi->numberOfSessions )  /* last session */
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout, "Error: Undefined lastValidBlockNr%s.\n",
                (mi->numberOfSessions == 1) ? "" : " for last session");
          MLIMITend;
          return FALSE;
        }
        mi->lastValidBlockNr = sessionEnd;      /* not last session */
    }

    if(      mi->lastValidBlockNr < sessionEnd )
    {   MLIMITbegin(WARN01level,uctMessageLimit);
          fprintf(uctout,
            "Warning: lastValidBlockNr less than end of verify session\n"
            "-        (%u, %u)\n", mi->lastValidBlockNr, sessionEnd);
        MLIMITend;
    }
    else if( mi->lastValidBlockNr > sessionEnd )
    {   MLIMITbegin(WARN01level,uctMessageLimit);
          fprintf(uctout,
            "Warning: Shrink lastValidBlockNr to end of verify session\n"
            "-        (%u -> %u)\n", mi->lastValidBlockNr, sessionEnd);
        MLIMITend;
        mi->lastValidBlockNr = sessionEnd;
    }

    /* ECMA 3/8.1.2.1: The largest logical sector number of a volume
     *                       shall be higher than 256.
     * TODO: DCN: largest ... shall be greater than sessionStart + 512
     *            to avoid that: N-256 <= sessionStart + 256
     */
    if( mi->lastValidBlockNr < sessionStart + 257 )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
            "  Fatal error: lastValidBlockNr less than start of verify session "
                                            "+ 257,\n"
            "-              (%lu, %lu), ECMA 3/8.1.2.1\n"
            "- Medium info:\n", mi->lastValidBlockNr, sessionStart + 257);
        printMediumInfo(mi);
        fprintf(uctout,"\n");
      MLIMITend;
      return FALSE;
    }

    /*** determines minimal number of unrecorded areas in gapTable.
     * For now, inspect trackTable only for unrecorded gaps.
     * Skip if output->numberOfGaps != 0;
     * Note that gaps are ignored or truncated above
     * mi->lastValidBlockNr, see addGapToMediumInfo().
     */
    if( mi->numberOfGaps == 0 )     /* no gap defined yet */
    {   Uint32 n, gapStart = 0,
                  gapLength, expectNextStart;
        mi->numberOfGaps = 0;
        mi->gapTable = NULL;
        gapLength = expectNextStart = 0;
        for( n=0; n < mi->numberOfTracks; n++ )
        {   TrackInfo *ti = &mi->trackTable[n];

            if( ti->trackStartAddress < expectNextStart )
            { /* overlapping with previous track
               * abort unrecorded gap calculation
               * destroy gaps found so far
               */
              MLIMITbegin(INFO01level, 1);
                fprintf(uctout,
                  "\n=> Note: Overlapping tracks, track start: %lu, expected: %lu\n"
                    "-        Cancel unrecorded gap tracking\n",
                   ti->trackStartAddress, expectNextStart);
              MLIMITend;
              /* destroy gaps found so far
               */
              checkFree((void **)&mi->gapTable);    /* NULL */
              mi->numberOfGaps = 0;
              gapLength = 0;            /* destroy */
              break;                    /* abort */
            }

            /* force consistency on trackRecordedLength
             */
            if( ti->trackRecordedLength > ti->trackLength )
            { MLIMITbegin(ERROR00level, uctMessageLimit);
                fprintf(uctout,
                  "\tError: Track %lu Recorded Length > Track Length "
                                                "(%lu, %lu), truncate.\n",
                ti->trackNumber, ti->trackRecordedLength, ti->trackLength);
              MLIMITend;
              ti->trackRecordedLength = ti->trackLength;    /* truncate */
            }

            /* find gaps
             */
            if( ti->trackStartAddress > expectNextStart )   /* track gap */
            {   if( gapLength == 0 )
                {   gapStart = expectNextStart;
                }
                gapLength += ti->trackStartAddress - expectNextStart;
            }
            if(   ti->trackRecordedLength != 0      /* recorded blocks */
               && gapLength != 0 )                  /* end of gap */
            {   addGapToMediumInfo(mi, gapStart, gapLength);
                gapLength = 0;                      /* next gap */
            }
            expectNextStart = ti->trackStartAddress + ti->trackLength;
            if( ti->trackRecordedLength < ti->trackLength )     /* gap */
            {   /* unrecorded end of track */
                if( gapLength == 0 )
                { gapStart = ti->trackStartAddress + ti->trackRecordedLength;
                }
                gapLength += (ti->trackLength - ti->trackRecordedLength);
            }
        }
        if( gapLength != 0 )    /* last gap, maybe truncate above lastValidBlockNr */
        {   addGapToMediumInfo(mi, gapStart, gapLength);
        }
    }

    /* Consistency check on gapTable[] with UCTASSERT()
     * No overlapping or contiguous gaps.
     */
    for( n = 0; n < mi->numberOfGaps; n++ )
    { GapInfo *g2 = &mi->gapTable[n];
      if( n > 0 )
      { GapInfo *g1 = &mi->gapTable[n-1];
        UCTASSERT(   g2->gapStart >  g1->gapStart
                  && g2->gapStart > (g1->gapStart + g1->gapLength) );
      }
    }

    /* handle legacy behaviour of selectDevice functions
     * that do not yet set lastRecordedBlockNr
     */
    if( mi->lastRecordedBlockNr == 0 )
    { mi->lastRecordedBlockNr = mi->lastValidBlockNr;
    }
    else
    { mi->lastRecordedBlockNr = MIN(mi->lastRecordedBlockNr,
                                    mi->lastValidBlockNr);
    }

    /* Determine last recorded block in range 0 till
     * lastValidBlockNr inclusive.
     * Check overlap with unrecorded gaps, if any
     */
    if( gapFindLastRecordedBlock( mi, 0,
                         1 + mi->lastValidBlockNr,
                        &lastRecorded) )
    { mi->lastRecordedBlockNr = MIN(mi->lastRecordedBlockNr,
                                    lastRecorded);
    }

    /* L0capacity: multiple of eccLength
     */
    if(    mi->eccLength != 0
       && (mi->L0capacity % mi->eccLength) != 0 )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
            "Error: L0 capacity: %lu,"
                    " expected: multiple of %lu (ECC).\n",
            mi->L0capacity, mi->eccLength);
      MLIMITend;
      return FALSE;     /* error */
    }
    return TRUE;

}   /* end finishMediumInfo() */

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
                            char       *txt02)
{
    bool        overruled = FALSE;
    MediumInfo *copyFrom;
    Uint32      n;

    /* Note that the ouput pointer may be equal to one of
     * the input pointers and that secondary may be NULL.
     */
    if( output != primary && output != secondary )
    {
        clearMediumInfo( output );      /* make consistent */
    }
    if( secondary == NULL && output != primary )
    {   /* copy primary to output, copy arrays separately
         */
        memcpy( (void *) output, (void *) primary, sizeof(MediumInfo));
        output->numberOfSessions = 0;
        output->sessionStartBlocks = NULL;
        for( n=0; n < primary->numberOfSessions; n++ )
        {
            addSessionToMediumInfo(output, primary->sessionStartBlocks[n]);
        }
        output->numberOfTracks = 0;
        output->trackTable = NULL;
        for( n=0; n < primary->numberOfTracks; n++ )
        {   TrackInfo *ti = &primary->trackTable[n];
            addTrackToMediumInfo(output, ti->trackNumber,
                                         ti->sessionNumber,
                                         ti->trackStartAddress,
                                         ti->trackLength,
                                         ti->trackRecordedLength);
        }
        output->numberOfGaps = 0;
        output->gapTable = NULL;
        for( n=0; n < primary->numberOfGaps; n++ )
        {   GapInfo *gi = &primary->gapTable[n];
            addGapToMediumInfo(output, gi->gapStart,
                                       gi->gapLength);
        }
    }
    else if( secondary != NULL )    /* do actual merge */
    {
        /* determine output writabilityType
         */
        if(      primary->writabilityType != MTYPE_WR_UNKNOWN
            && secondary->writabilityType != MTYPE_WR_UNKNOWN
            &&   primary->writabilityType != secondary->writabilityType )
        {   overruled = TRUE;
            MLIMITbegin(WARN01level,uctMessageLimit);
              fprintf(uctout, "  Merge warning: \"%s\" from %s\n"
                              "-  overruled by: \"%s\" from %s\n\n",
                        MTYPE_WR_TEXT(secondary->writabilityType), txt02,
                        MTYPE_WR_TEXT(  primary->writabilityType), txt01);
            MLIMITend;
        }
        if( primary->writabilityType != MTYPE_WR_UNKNOWN )
             output->writabilityType =   primary->writabilityType;
        else output->writabilityType = secondary->writabilityType;

        /* determine output sequentialType
         */
        if(      primary->sequentialType != MTYPE_SE_UNKNOWN
            && secondary->sequentialType != MTYPE_SE_UNKNOWN
            &&   primary->sequentialType != secondary->sequentialType )
        {   overruled = TRUE;
            MLIMITbegin(WARN01level,uctMessageLimit);
              fprintf(uctout, "  Merge warning: \"%s\" from %s\n"
                              "-  overruled by: \"%s\" from %s\n\n",
                        MTYPE_SE_TEXT(secondary->sequentialType), txt02,
                        MTYPE_SE_TEXT(  primary->sequentialType), txt01);
            MLIMITend;
        }
        if( primary->sequentialType != MTYPE_SE_UNKNOWN )
             output->sequentialType =   primary->sequentialType;
        else output->sequentialType = secondary->sequentialType;

        /* determine output closedType
         */
        if(      primary->closedType != MTYPE_CL_UNKNOWN
            && secondary->closedType != MTYPE_CL_UNKNOWN
            &&   primary->closedType != secondary->closedType )
        {   overruled = TRUE;
            MLIMITbegin(WARN01level,uctMessageLimit);
              fprintf(uctout, "  Merge warning: \"%s\" from %s\n"
                              "-  overruled by: \"%s\" from %s\n\n",
                        MTYPE_CL_TEXT(secondary->closedType), txt02,
                        MTYPE_CL_TEXT(  primary->closedType), txt01);
            MLIMITend;
        }
        if( primary->closedType != MTYPE_CL_UNKNOWN )
             output->closedType =   primary->closedType;
        else output->closedType = secondary->closedType;

        /* determine output blockSize
         */
        if(      primary->blockSize != 0
            && secondary->blockSize != 0
            &&   primary->blockSize != secondary->blockSize )
        {   overruled = TRUE;
            MLIMITbegin(WARN01level,uctMessageLimit);
              fprintf(uctout, "  Merge warning: block size %u from %s\n"
                              "-  overruled by: block size %u from %s\n\n",
                              secondary->blockSize, txt02,
                                primary->blockSize, txt01);
            MLIMITend;
        }
        if( primary->blockSize != 0 )
             output->blockSize =   primary->blockSize;
        else output->blockSize = secondary->blockSize;

        /* determine output eccLength
         */
        if(      primary->eccLength != 0
            && secondary->eccLength != 0
            &&   primary->eccLength != secondary->eccLength )
        {   overruled = TRUE;
            MLIMITbegin(WARN01level,uctMessageLimit);
              fprintf(uctout, "  Merge warning: ECC blocking factor %u from %s\n"
                              "-  overruled by: ECC blocking factor %u from %s\n\n",
                              secondary->eccLength, txt02,
                                primary->eccLength, txt01);
            MLIMITend;
        }
        if( primary->eccLength != 0 )
             output->eccLength =   primary->eccLength;
        else output->eccLength = secondary->eccLength;

        /* determine output lastValidBlockNr
         */
        if(      primary->lastValidBlockNr != 0
            && secondary->lastValidBlockNr != 0
            &&   primary->lastValidBlockNr != secondary->lastValidBlockNr )
        {   overruled = TRUE;
            MLIMITbegin(WARN01level,uctMessageLimit);
              fprintf(uctout,
                    "  Merge warning: last valid block number %6u from %s\n"
                    "-  overruled by: last valid block number %6u from %s\n\n",
                      secondary->lastValidBlockNr, txt02,
                        primary->lastValidBlockNr, txt01);
            MLIMITend;
        }
        if( primary->lastValidBlockNr != 0 )
             output->lastValidBlockNr =   primary->lastValidBlockNr;
        else output->lastValidBlockNr = secondary->lastValidBlockNr;

        /* determine output dummySessionTotalBlocks
         */
        if(      primary->dummySessionTotalBlocks != 0
            && secondary->dummySessionTotalBlocks != 0
            &&   primary->dummySessionTotalBlocks != secondary->dummySessionTotalBlocks )
        {   overruled = TRUE;
            MLIMITbegin(WARN01level,uctMessageLimit);
              fprintf(uctout,
                    "  Merge warning: dummysession total blocks %6lu from %s\n"
                    "-  overruled by: dummysession total blocks %6lu from %s\n\n",
                      secondary->dummySessionTotalBlocks, txt02,
                        primary->dummySessionTotalBlocks, txt01);
            MLIMITend;
        }
        if( primary->dummySessionTotalBlocks != 0 )
             output->dummySessionTotalBlocks =   primary->dummySessionTotalBlocks;
        else output->dummySessionTotalBlocks = secondary->dummySessionTotalBlocks;

        /* determine output numberOfSessions
         *              and sessionStartBlocks, handle together.
         * Important:
         *   It is assumed that consistency is maintained as
         *   defined in the rules for addSessionToMediumInfo().
         */
        if(      primary->numberOfSessions != 0
            && secondary->numberOfSessions != 0 )
        {
            if(  primary->numberOfSessions != secondary->numberOfSessions )
            {   overruled = TRUE;
                MLIMITbegin(WARN01level,uctMessageLimit);
                  fprintf(uctout,
                    "  Merge warning: %u session%s from %s\n"
                    "-  overruled by: %u session%s from %s\n\n",
                             secondary->numberOfSessions,
                    PLURAL_S(secondary->numberOfSessions), txt02,
                              primary->numberOfSessions,
                    PLURAL_S(primary->numberOfSessions), txt01);
                MLIMITend;
            }
            else    /* numberOfSessions equal, check startBlocks overrule */
            {
                for( n=0; n < primary->numberOfSessions; n++ )
                {
                    if(      primary->sessionStartBlocks[n]
                        != secondary->sessionStartBlocks[n] )
                    { overruled = TRUE;
                      MLIMITbegin(WARN01level,uctMessageLimit);
                        fprintf(uctout,
                          "  Merge warning: session startblock %u: %u from %s\n"
                          "-  overruled by: session startblock %u: %u from %s\n\n",
                            n+1, secondary->sessionStartBlocks[n], txt02,
                            n+1,   primary->sessionStartBlocks[n], txt01);
                      MLIMITend;
                    }
                }
            }
        }
        if( primary->numberOfSessions != 0 )
             copyFrom = primary;
        else copyFrom = secondary;

        if( output != copyFrom )        /* do actual sessionStartBlocks[] copy */
        {
            output->numberOfSessions = 0;
            for( n=0; n < copyFrom->numberOfSessions; n++ )
            {
                addSessionToMediumInfo(output, copyFrom->sessionStartBlocks[n]);
            }
        }

        /* determine output verifySession
         */
        if(      primary->verifySession != 0
            && secondary->verifySession != 0
            &&   primary->verifySession != secondary->verifySession )
        {   overruled = TRUE;
            MLIMITbegin(WARN01level,uctMessageLimit);
              fprintf(uctout, "  Merge warning: verify session %u from %s\n"
                              "-  overruled by: verify session %u from %s\n\n",
                              secondary->verifySession, txt02,
                                primary->verifySession, txt01);
            MLIMITend;
        }
        if( primary->verifySession != 0 )
             output->verifySession =   primary->verifySession;
        else output->verifySession = secondary->verifySession;

        /* handle L0capacity and isOTP together
         * AND ALWAYS copy/overrule them together !!
         * determine output L0capacity and isOTP
         * (means: never use -PTP option without -L0capacity option)
         */
        if(      primary->L0capacity != 0
            && secondary->L0capacity != 0
            && (   primary->L0capacity != secondary->L0capacity
                ||      primary->isOTP != secondary->isOTP) )
        {   overruled = TRUE;
            MLIMITbegin(WARN01level,uctMessageLimit);
              fprintf( uctout,
                    "  Merge warning: L0 capacity %6u %3s from %s\n"
                    "-  overruled by: L0 capacity %6u %3s from %s\n\n",
                                    secondary->L0capacity,
                        MI_OTP_TEXT(secondary->isOTP), txt02,
                                      primary->L0capacity,
                        MI_OTP_TEXT(  primary->isOTP), txt01 );
            MLIMITend;
        }
        if( primary->L0capacity != 0 )
             { output->L0capacity =   primary->L0capacity;
               output->isOTP      =   primary->isOTP;
             }
        else { output->L0capacity = secondary->L0capacity;
               output->isOTP      = secondary->isOTP;
             }

        /* NOTE: No merge for numberOfTracks, trackTable,
         *                    numberOfGaps, gapTable and bgFormatStatus,
         *  because they cannot be defined on the command
         *  line or in a configuration file, so merge is not relevant.
         *  they are however copied to output if primary == NULL.
         */
    }
    return overruled;

}   /* end mergeMediumInfo() */


/* Functions for maintaining of MediumInfo sessionStartBlocks
 * and numberOfSessions consistency.
 */
/* Sort unique of sessionStartBlocks array,
 * so remove multiple startblock values.
 * Return value: final number of session startblocks
 */
static int sortUniqueSessionStartBlocks( Uint32 *sessionStartBlocks,
                                         Uint32  numberOfSessions )
{
    int    cnt;
    Uint32 u;

    if( numberOfSessions == 0 )
    {
        return 0;                   /* empty */
    }
    qsort( (void *)sessionStartBlocks, numberOfSessions,
                    sizeof(Uint32), qsortCompareUint32);

    /* remove multiple defined startblocks
     */
    for( u = 0, cnt = 0; u < numberOfSessions; u++)
    {
        if( sessionStartBlocks[cnt] != sessionStartBlocks[u] )
        {
            sessionStartBlocks[++cnt] = sessionStartBlocks[u];
        }
    }
    return ++cnt;       /* at least one */
}

/* addSessionStartBlock():
 * Add session start block to sessionStartBlocks array.
 * Add one at a time, starting incrementally with index 0.
 * If sessionStartBlocks != NULL it means that it is pointing to
 * dynamically allocated memory, and it will be reallocated to point to
 * an array of (index+1) Uint32 elements.
 * If index == 0, sessionStartBlocks may be NULL.
 *
 * Return value:
 *  Pointer to new (possibly reallocated !!) sessionStartBlocks array.
 */
static Uint32 *addSessionStartBlock( Uint32 *sessionStartBlocks,
                                     Uint32  startBlock, int index )
{
    Uint32 *result;
    size_t  nBytes = (index+1) * sizeof(Uint32);

    /* tst_realloc() calls malloc() in case sessionStartBlocks == NULL
     */
    result = (Uint32*) tst_realloc(sessionStartBlocks, nBytes,
                                            __FILE__, __LINE__);
    if( result == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    result[index] = startBlock;
    return result;
}

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
                                    Uint32      startBlock )
{
    if( mi->numberOfSessions == 0 && startBlock != 0 )
    {
        mi->sessionStartBlocks =
            addSessionStartBlock( mi->sessionStartBlocks, 0,
                                 (mi->numberOfSessions)++ );
    }
    mi->sessionStartBlocks =
        addSessionStartBlock( mi->sessionStartBlocks, startBlock,
                             (mi->numberOfSessions)++ );

    mi->numberOfSessions =
        sortUniqueSessionStartBlocks( mi->sessionStartBlocks,
                                      mi->numberOfSessions );
}

/* addTrackInfo():
 * Realloc trackTable array to add one element of TrackInfo.
 * No fill of this new element.
 */
static TrackInfo *addTrackInfo(TrackInfo *trackTable, int nmbOfRecords)
{
    /* tst_realloc() calls malloc() in case trackTable == NULL
     */
    trackTable = (TrackInfo*) tst_realloc(trackTable,
                                (nmbOfRecords+1) * sizeof(TrackInfo),
                                __FILE__, __LINE__);
    if( trackTable == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    return trackTable;        /* reallocated result */
}

/* addTrackToMediumInfo():
 * Add new TrackInfo record to MediumInfo
 * like addSessionToMediumInfo() above for sessions.
 */
extern void addTrackToMediumInfo( MediumInfo *mi,
                            Uint16 trackNumber,
                            Uint16 sessionNumber,
                            Uint32 trackStartAddress,
                            Uint32 trackLength,
                            Uint32 trackRecordedLength )
{
    TrackInfo *lastTrack;
    /* be aware of possible relocation of mi->trackTable
     */
    mi->trackTable =
        addTrackInfo( mi->trackTable, mi->numberOfTracks );

    /* increment mi->numberOfTracks and
     * fill new element of mi->trackTable.
     */
    lastTrack = &mi->trackTable[(mi->numberOfTracks)++];

    lastTrack->trackNumber          = trackNumber;
    lastTrack->sessionNumber        = sessionNumber;
    lastTrack->trackStartAddress    = trackStartAddress;
    lastTrack->trackLength          = trackLength;
    lastTrack->trackRecordedLength  = trackRecordedLength;

}   /* end addTrackToMediumInfo() */

/* addGapInfo():
 * Realloc gapTable array to add one element of GapInfo.
 * No fill of this new element.
 */
static GapInfo *addGapInfo(GapInfo *gapTable, int nmbOfRecords)
{
    /* tst_realloc() calls malloc() in case gapTable == NULL
     */
    gapTable = (GapInfo*) tst_realloc(gapTable,
                                (nmbOfRecords+1) * sizeof(GapInfo),
                                __FILE__, __LINE__);
    if( gapTable == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    return gapTable;          /* reallocated result */
}

/* addGapToMediumInfo():
 * Add new GapInfo record to MediumInfo
 * like addSessionToMediumInfo() above for sessions.
 * Addition of gap above lastValidBlockNr is ignored.
 */
extern void addGapToMediumInfo( MediumInfo *mi,
                            Uint32 gapStart,
                            Uint32 gapLength )
{
    GapInfo *lastGap;
    Uint32 correction;

    /* check above lastVakidBlockNr first
     */
    if( (gapStart + gapLength) > (mi->lastValidBlockNr + 1) )
    {  correction = (gapStart + gapLength)
                  - (mi->lastValidBlockNr + 1);
       if( correction >= gapLength )
       { return;        /* no gap left */
       }
       gapLength -= correction ;
    }

    /* reallocate gapTable to add one element, no fill yet.
     */
    mi->gapTable =
        addGapInfo( mi->gapTable, mi->numberOfGaps );

    /* increment mi->numberOfGaps and
     * fill new element of mi->gapTable.
     */
    lastGap = &mi->gapTable[(mi->numberOfGaps)++];

    lastGap->gapStart   = gapStart;
    lastGap->gapLength  = gapLength;

}   /* end addGapToMediumInfo() */

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
                        Uint32 *pFirstUnrecorded)
{
    int     n, nGaps = mi->numberOfGaps;
    Uint32  startOverlap, lenOverlap;

    /* check gaps, if any, sorted on gapStart
     * mind that gapTable is sorted on gapStart.
     */
    for( n = 0; n < nGaps; n++ )
    { GapInfo *gi = &mi->gapTable[n];
      if( calculateOverlap(firstBlockInRange, nmbBlocksRange,
                           gi->gapStart, gi->gapLength,
                          &startOverlap, &lenOverlap) )
      { /* overlap with unrecorded gap
         * startOverlap >= firstBlockInRange;
         */
//fprintf(uctout, "==> gap overlap: %lu, %lu\n",
//                      startOverlap, lenOverlap);
        *pFirstUnrecorded = startOverlap;
        return TRUE;    /* found */
      }
    }
    return FALSE;       /* not found */

}   /* end gapFindFirstUnrecordedBlock() */

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
                        Uint32 *pFirstRecorded)
{
    int     n, nGaps = mi->numberOfGaps;
    Uint32  startOverlap, lenOverlap;

    if( nmbBlocksRange == 0 )
    { return FALSE;             /* no blocks */
    }

    /* check overlap with unrecorded gaps, if any.
     * mind that gapTable is sorted on gapStart.
     */
    for( n = 0; n < nGaps; n++ )
    { GapInfo *gi = &mi->gapTable[n];
      if( calculateOverlap(firstBlockInRange, nmbBlocksRange,
                           gi->gapStart, gi->gapLength,
                          &startOverlap, &lenOverlap) )
      { if( startOverlap == firstBlockInRange )
        { if( lenOverlap == nmbBlocksRange )
          { return FALSE;   /* complete overlap, no recorded block */
          }
          *pFirstRecorded = firstBlockInRange + lenOverlap;
        }
        else
        { *pFirstRecorded = firstBlockInRange;
        }
        return TRUE;    /* recorded block found */
      }
    }

    /* no overlapping unrecorded gaps
     */
    *pFirstRecorded = firstBlockInRange;
    return TRUE;        /* recorded block found */

}   /* end gapFindFirstRecordedBlock() */

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
                        Uint32 *pLastRecorded)
{
    int     n, nGaps = mi->numberOfGaps;
    Uint32  lastRec;

    /* check gaps, if any, sorted on gapStart
     * mind that gapTable is sorted on gapStart.
     * so start with last gap in gapTable.
     * First check if last block in range overlaps
     * with any gap.
     */
    lastRec = firstBlockInRange + nmbBlocksRange - 1;
    for( n = nGaps - 1; n >= 0; n-- )
    { GapInfo *gi = &mi->gapTable[n];
      if( calculateOverlap(lastRec, 1,
                           gi->gapStart, gi->gapLength,
                           NULL, NULL) )
      { /* overlap of lastRec with unrecorded gap
         */
        if( gi->gapStart == 0 ) /* no recorded block */
        { return FALSE;         /*  in range */
        }
        lastRec = gi->gapStart - 1;
      }
    }

    if( calculateOverlap(lastRec, 1, firstBlockInRange,
                         nmbBlocksRange, NULL, NULL) )
    {   *pLastRecorded = lastRec;
        return TRUE;    /* in range */
    }
    return FALSE;       /* not in range */

}   /* end gapFindLastRecordedBlock() */


/* initTheMediumInfo():
 * Initialize verifier theMediumInfo structure
 *
 * Return value: FALSE if error found, else TRUE
 */
extern bool initTheMediumInfo(MediumInfo *mi)
{
    theMediumInfoIsInitialized = FALSE;

    /* Use mergeMediumInfo() to copy (overruling impossible),
     * and finishMediumInfo() to set defaults.
     */
    if(    mergeMediumInfo(mi,NULL,&theMediumInfo,"x1","x2")
       || !finishMediumInfo(&theMediumInfo) )
    {
        return FALSE;
    }
    theMediumInfoIsInitialized = TRUE;
    return TRUE;
}

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
                                   char *reason )
{
    MediumInfo *mi = (MediumInfo *) getTheMediumInfo();
    if( mi->writabilityType == typeWr )
    {
        return TRUE;    /* ok, equal, or 'unknown' */
    }

    if( mi->writabilityType == MTYPE_WR_UNKNOWN )
    {
        VERBOSE00(uctout,
            "  ==>\tChanged medium WR type from %s to %s\n"
                "-\t        because of %s\n",
                    MTYPE_WR_TEXT(mi->writabilityType),
                    MTYPE_WR_TEXT(typeWr), reason);
        mi->writabilityType = typeWr;
        return TRUE;    /* ok, modified */
    }

    MLIMITbegin(ERROR00level, uctMessageLimit);
      fprintf(uctout,
        "  ==>\tError: Medium WR type conflict because of %s\n"
            "-\t       Shall be: %s, currently: %s\n",
                reason, MTYPE_WR_TEXT(typeWr),
                        MTYPE_WR_TEXT(mi->writabilityType));
    MLIMITend;
    return FALSE;       /* conflict */
}

/* For medium SE type:
 */
extern bool modifyTheMediumTypeSe( MediumSequentialType typeSe,
                                   char *reason )
{
    MediumInfo *mi = (MediumInfo *) getTheMediumInfo();
    if( mi->sequentialType == typeSe )
    {
        return TRUE;    /* ok, equal */
    }

    if( mi->sequentialType == MTYPE_SE_UNKNOWN )
    {
        VERBOSE00(uctout,
            "  ==>\tChanged medium SE type from %s to %s\n"
                "-\t        because of %s\n",
                    MTYPE_SE_TEXT(mi->sequentialType),
                    MTYPE_SE_TEXT(typeSe), reason);
        mi->sequentialType = typeSe;
        return TRUE;    /* ok, modified */
    }

    MLIMITbegin(ERROR00level, uctMessageLimit);
     fprintf(uctout,
        "  ==>\tError: Medium SE type conflict because of %s\n"
            "-\t       Shall be: %s, currently: %s\n",
                reason, MTYPE_SE_TEXT(typeSe),
                        MTYPE_SE_TEXT(mi->sequentialType));
    MLIMITend;
    return FALSE;       /* conflict */
}

/* For medium CL type:
 */
extern bool modifyTheMediumTypeCl( MediumClosedType typeCl,
                                   char *reason )
{
    MediumInfo *mi = (MediumInfo *) getTheMediumInfo();
    if( mi->closedType == typeCl )
    {
        return TRUE;    /* ok, equal */
    }

    if( mi->closedType == MTYPE_CL_UNKNOWN )
    {
        VERBOSE00(uctout,
            "  ==>\tChanged medium CL type from %s to %s\n"
                "-\t        because of %s\n",
                    MTYPE_CL_TEXT(mi->closedType),
                    MTYPE_CL_TEXT(typeCl), reason);
        mi->closedType = typeCl;
        return TRUE;    /* ok, modified */
    }

    MLIMITbegin(ERROR00level, uctMessageLimit);
      fprintf(uctout,
        "  ==>\tError: Medium CL type conflict because of %s\n"
            "-\t       Shall be: %s, currently: %s\n",
                reason, MTYPE_CL_TEXT(typeCl),
                        MTYPE_CL_TEXT(mi->closedType));
    MLIMITend;
    return FALSE;       /* conflict */
}

/* getEccPacketNr():
 * get ECC packet number from sector or block address.
 * Do not call before eccLength is set.
 */
extern Uint32 getEccPacketNr( Uint32 ad )
{   Uint32 eccLen = getTheMediumInfo()->eccLength;
    UCTASSERT( eccLen != 0 );
    return (Uint32) ad / eccLen;
}

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
extern Uint32 mlGetLayerNumber( Uint32 ad )
{
    const MediumInfo *mi = getTheMediumInfo();
    Uint32            startL1 = mi->L0capacity;
    /* startL1 is first address on L1, if any
     */
    return    (startL1 == 0) ? 0                /* single layer */
            : ( mi->isOTP )  ? (ad / startL1)   /* OTP layer number */
            : (ad < startL1) ? 0 : 1;           /* PTP layer number */
}

/* mlOnSameLayer(): Return TRUE iff ad0 and ad1 are on same layer.
 *
 * NOTE: For PTP, only correct result is guaranteed if ad0 and ad1
 *       are real addresses, see mlGetLayerNumber() explanation.
 */
extern bool mlOnSameLayer( Uint32 ad0, Uint32 ad1 )
{
    return (   mlGetLayerNumber(ad0)
            == mlGetLayerNumber(ad1) );
}

/* mlGetLastAddressL0():
 * return equivalent Layer L0 address of ad.
 */
static Uint32 mlGetLastAddressL0()
{   const MediumInfo *mi = getTheMediumInfo();
    Uint32 L0capacity = mi->L0capacity;
    return (L0capacity == 0) ? mi->lastValidBlockNr
                             : L0capacity - 1;
}

/* mlTranslateToLayer():
 * Return equivalent Layer 'lnTarget' address of ad.
 * Equivalent means that the result is recorded
 * on the same radius of layer lnTarget.
 * For a single layer case, ad is returned.
 *
 * For PTP: Only 1 or 2 layers (DL) are supported. For PTP,
 *          the size of L1 can be bigger than the size of L0.
 *          This means that an 'L0 equivalent address' of an
 *          address ad on L1 can still have a value >= L0Capacity.
 *          Therefore, if lnTarget == 1, it is assumed that ad is a
 *          'L0 equivalent address` that must be translated back to L1.
 *
 * For OTP: Multi layer is supported. No problems as mentioned above for PTP,
 *          because for OTP, it is assumed that the capacity of each layer
 *          is equal to the L0 Capacity.
 *
 * WARNING: UCTASSERT() failure for inconsistent arguments.
 *
 * IMPLEMENTATION NOTES:
 *   1) OTP for lnTarget > 1 is not yet defined in MMC !!
 *      The assumption here is that a 'zigzag' addressing
 *      schema will be used for layers 2, 3, etc.
 *   2) Note that for PTP the capacity on L1 may be
 *      larger than on L0 !! This may result in the fact
 *      that translation of an address ad on L1 may result
 *      in a value resultAd that is still >= L0Capacity !!!!!
 *      mlGetLayerNumber(resultAd) will still return 1 in that case.
 */
static Uint32 mlTranslateToLayer( Uint32 ad,
                                  Uint32 lnTarget )
{   const MediumInfo *mi = getTheMediumInfo();
    Uint32 L0capacity = mi->L0capacity,
           L0Double   = 2 * L0capacity,
           trAd;

    /* only 2 layers supported for PTP
     */
    UCTASSERT( mi->isOTP || lnTarget <= 1 );

    if( L0capacity == 0 )   /* SL */
    {   return ad;
    }
    if( mi->isOTP )         /* OTP */
    { trAd = ad % L0Double;         /* L0/L1 equivalent */
      if( trAd >= L0capacity )      /* L1 equi -> L0 equi */
      { trAd = L0Double - trAd - 1; /* translated L0 equi */
      }
      if( (lnTarget % 2) != 0 )     /* 'odd' target layer */
      { trAd = L0Double - trAd - 1; /* L0 equi -> L1 equi */
        lnTarget--;
      }
      while( lnTarget > 0 )         /* now even */
      { trAd += L0Double;           /* add 2 layers */
        lnTarget -= 2;
      }
    }
    else                    /* PTP, L0 or L1 */
    { UCTASSERT(   lnTarget == 1
                || ad >= L0capacity );  /* lnTarget == 0 */
      trAd = (lnTarget == 1)
           ? ad + L0capacity    /* L0-> L1 */
           : ad - L0capacity;   /* L1-> L0 */
    }
    return trAd;

}   /* end mlTranslateToLayer() */

/* mlExtentL0Equivalent():
 * Calculate layer zero equivalent for ExtentAd ex.
 * Represent result as separate start and end address,
 * rather than an ExtentAd result;
 *
 * Asserted conditions for the extent ex:
 *  It cannot be partly on one layer and partly on the other, see
 *  mlFaCalcDistOneLayer() and mlFaCalcDistance() comment.
 */
static void mlExtentL0Equivalent( ExtentAd *ex,
                                  Uint32   *pStr,
                                  Uint32   *pEnd )
{
    const MediumInfo *mi = getTheMediumInfo();
    Uint32 str, strEq, end, endEq, Ln;

   /* Translate the extent start and last address to
     * layer zero addresses with 'equivalent radius'.
     * Only translation for ML OPT or DL PTP medium.
     */
    str = strEq = ex->extentLocation;
    end = endEq = str + ROUNDUPELEMENTS( ex->extentLength,
                                   mi->blockSize) - 1;
    Ln = mlGetLayerNumber(str);
    UCTASSERT( Ln == mlGetLayerNumber(end) );  /* both on Ln */

    if( Ln != 0 )   /* test to avoid L0 translation for PTP */
    { strEq = mlTranslateToLayer(str, 0);  /* Ln -> L0 */
      endEq = mlTranslateToLayer(end, 0);  /* Ln -> L0 */
    }
    str = MIN(strEq, endEq);    /* Cope with possible REVERSE */
    end = MAX(strEq, endEq);    /*  of end and start for OTP  */

    (*pStr) = str;
    (*pEnd) = end;

}   /* end mlExtentL0Equivalent() */

/* mlFitL0adInExtent():
 * precondition: L0ad is a L0 equivalent address.
 * Check if L0ad or its layer n equivalent fits in extent ex,
 * where n is the layer of the first address in extent ex.
 *
 * return value in (*pAdFit):
 *  the Layer n equivalent of L0ad (if none, then L0ad).
 *
 * Function return value: TRUE if a fit is found, else FALSE.
 */
static bool mlFitL0adInExtent( ExtentAd *ex,
                               Uint32    L0ad,
                               Uint32   *pAdFit )
{
    const MediumInfo *mi = getTheMediumInfo();
    Uint32  str = ex->extentLocation,
            end = str + ROUNDUPELEMENTS( ex->extentLength,
                                         mi->blockSize) - 1;
    Uint32  ln = mlGetLayerNumber(str);

    if( ln != 0 )   /* test needed for PTP */
    { L0ad = mlTranslateToLayer(L0ad, ln);  /* to layer n equi */
    }
    if( L0ad >= str && L0ad <= end )    /* equi fits in extent */
    {   (*pAdFit) = L0ad;          /* return L0ad layer n equi */
        return TRUE;                    /* fit found */
    }
    return FALSE;                       /* no fit found */

}   /* end mlFitL0adInExtent() */


/* mlShowAndAssertLayers():
 */
extern bool mlShowAndAssertLayers()
{
    MediumInfo *mi = (MediumInfo*) getTheMediumInfo();  /* maybe correct */
    int         ln = mlGetLayerNumber(mi->lastValidBlockNr );

    if( mi->L0capacity == 0 )
    { VERBOSE00(uctout, "\tSingle Layer medium\n");
    }
    else
    { VERBOSE00( uctout, "\tMulti-Layer %s medium,",
                            MI_OTP_TEXT(mi->isOTP) );
      if( ln == 0 )
      { VERBOSE00( uctout, " last valid block on layer L0,");
      }
      else
      { VERBOSE00( uctout, " at least %lu layers,", 1+ln);
      }
      VERBOSE00( uctout, " L0 capacity: %lu\n", mi->L0capacity );
      UCTASSERT(   mi->isOTP    /* OTP */
                || ln <= 1 );   /* PTP: mlGetLayerNumber() returns 0 or 1 */
    }
    return TRUE;

}       /* end mlShowAndAssertLayers() */


/* mlFaCalcDistOneLayer():
 * Does the calculation job for mlFaCalcDistance() for the
 * case that the two extents are NOT PARTLY on one layer
 * and partly on the other, but completely on one layer.
 * The whole extent ex0 may be on a different layer than ex1.
 */
static void mlFaCalcDistOneLayer( ExtentAd  *ex0,
                                  ExtentAd  *ex1,
                                  Uint32    *pAd0,
                                  Uint32    *pAd1,
                                  Uint32    *pSecDist,
                                  Uint32    *pEccDist )
{
    Uint32  eccLength = getTheMediumInfo()->eccLength,  /* nmb of sectors */
            maxZeroStr, minZeroEnd, ad0, ad1, adFit,
            zeroStr0, zeroEnd0, zeroStr1, zeroEnd1;

    /* first translate the extents to layer zero
     * 'equivalent radius' extents.
     */
    mlExtentL0Equivalent(ex0, &zeroStr0, &zeroEnd0);
    mlExtentL0Equivalent(ex1, &zeroStr1, &zeroEnd1);

    /* str<n> and end<n> are now the start and end of the layer 0
     * equivalents of the extents ex0 and ex1.
     */
    maxZeroStr = MAX(zeroStr0, zeroStr1);
    minZeroEnd = MIN(zeroEnd0, zeroEnd1);

    if( maxZeroStr <= minZeroEnd )  /* physical overlap of  */
    { minZeroEnd = maxZeroStr;      /*  layer 0 equivalents */
    }

    /* precondition: maxZeroStr >= minZeroEnd
     * These two layer zero addresses have the lowest physical sector
     * distance for the layer zero equivalents of ex0 and ex1.
     * Calculate physical sector and ECC packet distances.
     */
    (*pSecDist) = maxZeroStr - minZeroEnd;
    (*pEccDist) = (maxZeroStr / eccLength)
                - (minZeroEnd / eccLength);

    /* Now determine the addresses in ex0 and ex1 respectively,
     * where maxZeroStr and minZeroEnd originate from.
     * For the overlap case (maxZeroStr == minZeroEnd) this
     * does not really matter.
     */
    if( maxZeroStr == zeroStr0 )    /* maxZeroStr originates from ex0 */
         { ad0 = maxZeroStr; ad1 = minZeroEnd; }
    else { ad1 = maxZeroStr; ad0 = minZeroEnd; }

    /* ad0 and ad1 are Layer 0 equivalents of original locations.
     * find original location:
     *     ad0 or its Ln equivalent fits in original ex0 extent
     * and ad1 or its Ln equivalent fits in original ex1 extent
     */
    if( !mlFitL0adInExtent(ex0, ad0, &adFit) )
    { UCTASSERT(FALSE);     /* not here please */
    }
    (*pAd0) = adFit;

    if( !mlFitL0adInExtent(ex1, ad1, &adFit) )
    { UCTASSERT(FALSE);     /* not here please */
    }
    (*pAd1) = adFit;

}   /* end mlFaCalcDistOneLayer() */


/** mlSplitLayerExtents():
 ** Split extent ex at the layer boundary into two extents exA and exB.
 ** If the layer boundary does not fall in ex, then exB will be
 ** empty (exB->extentLength = 0).
 ** For OTP, exB->extentLength will be truncated to the size of one layer.
 */
static void mlSplitLayerExtents( ExtentAd *ex,
                                 ExtentAd *exA,
                                 ExtentAd *exB )
{
    const MediumInfo *mi = getTheMediumInfo();
    Uint32  str, end, lnA,
            blockSize  = mi->blockSize,
            L0capacity = mi->L0capacity;

    str = ex->extentLocation;
    end = str + ROUNDUPELEMENTS( ex->extentLength,
                                 blockSize) - 1;
    exA->extentLocation = str;
    exA->extentLength   = ex->extentLength;
    exB->extentLocation = 0;
    exB->extentLength   = 0;
    lnA = mlGetLayerNumber(str);    /* start layer */

    /* Check if str and end on same layer.
     * (mlGetLayerNumber() will return zero for Single Layer).
     */
    if( lnA != mlGetLayerNumber(end) )
    {   Uint32 LAcapacity = (1+lnA) * L0capacity;
        /* str and end on different layers, split.
         * for OTP, truncate 2nd extent size to L0 size !!!!
         */
        UCTASSERT( str < LAcapacity && LAcapacity <= end );
        exA->extentLength   = (LAcapacity - str) * blockSize;
        exB->extentLocation = LAcapacity;
        exB->extentLength   = (1 + end - LAcapacity) * blockSize;
        if( (ex->extentLength % blockSize) != 0 )
        {   exB->extentLength -= (blockSize - (ex->extentLength % blockSize));
        }
        if( mi->isOTP )     /* trunctate extent B to L0 size */
        { Uint32 L0size = L0capacity * blockSize;
          exB->extentLength = MAX(exB->extentLength, L0size);
        }
    }
}       /* end mlSplitLayerExtents() */

/* mlFaCalcDistance():
 * Calculate lowest physical sector distance (*pSecDist) and ECC
 * packet distance (*pEccDist) between any sector in ExtentAd ex0
 * and any sector in ExtentAd ex1.
 * Also calculate the logical sector addresses (*pAd0) and (*pAd1)
 * for which this occurs.
 * Takes into account the fact that e.g. for a Multi Layer medium,
 * these physical distances can be zero for quite different values
 * of (*pAd0) and (*pAd1), where these are each on different layers
 * on an identical radius of a ML medium.
 *
 * Implementation notes:
 *  For DL PTP, care must be taken, because mlFaCalcDistOneLayer()
 *  (mlExtentL0Equivalent() in fact) cannot cope with the fact that an
 *  extent may partly be on one layer and partly on the other layer.
 *  Therefore, both extents ex0 and ex1 are each split at the layer
 *  boundary into two extents. If the layer boundary does not fall
 *  in the ex0, ex1 extent respectively, the second extent will be
 *  empty (extentLength zero).
 *  From the resulting (max) 2*2 extents, (max) 4 calls to
 *  mlFaCalcDistOneLayer() are performed. The one with the minimum
 *  ECC distances is returned as the final result.
 */
extern void mlFaCalcDistance( ExtentAd  *ex0,
                              ExtentAd  *ex1,
                              Uint32    *pAd0,
                              Uint32    *pAd1,
                              Uint32    *pSecDist,
                              Uint32    *pEccDist )
{
    ExtentAd ex0Split[2], ex1Split[2];
    Uint32   eccMin, secMin, ad0Min, ad1Min,
             ecc, sec, ad0, ad1;
    int      n, m;

    /* Split at the layer boundary:
     * ex0 into ex0Split[0] and ex0Split[1]
     * ex1 into ex1Split[0] and ex1Split[1].
     * If no layer boundary in the extent, then the '[1]'
     * extent will be empty (zero extentLength).
     */
    mlSplitLayerExtents(ex0, &ex0Split[0], &ex0Split[1]);
    mlSplitLayerExtents(ex1, &ex1Split[0], &ex1Split[1]);

    /* Now max 4 calls to mlFaCalcDistOneLayer(),
     * skip the empty extents.
     */
    eccMin = secMin = MAX_UINT32;
    ad0Min = ad1Min = 0;

    for( n = 0; n < 2 && ex0Split[n].extentLength != 0;
         n++ )
    { for( m = 0; m < 2 && ex1Split[m].extentLength != 0;
           m++ )
      { mlFaCalcDistOneLayer( &ex0Split[n], &ex1Split[m],
                              &ad0, &ad1, &sec, &ecc );
        if(    ecc < eccMin
           || (ecc == eccMin && sec < secMin) )
        { eccMin = ecc; secMin = sec;
          ad0Min = ad0; ad1Min = ad1;
        }
      }   /* endfor m */
    }   /* endfor n */

    UCTASSERT( eccMin != MAX_UINT32 );

    (*pSecDist) = secMin;
    (*pEccDist) = eccMin;
    (*pAd0) = ad0Min;
    (*pAd1) = ad1Min;

}   /* end mlFaCalcDistance() */




