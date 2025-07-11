/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : udf_test.c
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uct_core.h"
#include "uctfarapart.h"

#include "version.h"
#include "commandline.h"
#include "platform.h"
#include "write_image.h"
#include "selectdevice.h"
#include "scsi_device.h"    /* may #define  SCSI_DEVICE, etc. */
#include "drive_device.h"   /* may #define DRIVE_DEVICE, etc. */
#include "raw_device.h"     /* may #define   RAW_DEVICE, etc. */


static void showAbout()
{
#ifdef SCSI_DEVICE
    char *scsiWithNo = "with";
#else
    char *scsiWithNo = "no";
#endif

    fprintf(uctout,"UDF Conformance Testing Application\n");
    fprintf(uctout,"(c) Koninklijke Philips Electronics N.V. 1999-2007\n");
    fprintf(uctout, "Application version : %s\n", app_version);
    fprintf(uctout, "   UCT Core version : %s\n", uct_version);
    fprintf(uctout, "           platform : %s - %s scsi/atapi support\n",
                    UCT_PLATFORM_ID, scsiWithNo);

#ifdef DRIVE_DEVICE
    fprintf(uctout, "   '-drive' support : yes\n");
#endif
#ifdef RAW_DEVICE
    fprintf(uctout, "     '-raw' support : yes\n");
#endif
    fprintf(uctout, "\n");
    fflush(uctout);
}

static void Usage()
{
  static bool already_printed = FALSE;      /* print only once */
    if( already_printed )
    {
        return;
    }
    already_printed = TRUE;

    genericUsage();
    mediumOptionsUsage();
    selectDeviceUsage();
}

static void fatalErrorUsage()
{
    fprintf( uctout,
      "\nFor usage explatation: udf_test -help and FAQ.TXT\n\n" );
}


/* Show final summary of information gathered by the UDF verifier
 */
static void showFinalReport(bool fatalError)
{
    printEntityIDSummary();
    printFinalStatusReport(fatalError);
}


/* print start, end and elapsed times
 * pStartTime maybe NULL
 */
static void printElapsedTime( Timestamp *pStartTime )
{
    Timestamp endTime;
    Int16    tz;
    Int32   minutes;
    Int8   seconds;
    bool  timeOk = createCurrentTimeTimestamp(&endTime);

    if( pStartTime != NULL && timeOk )
    {   /* print elapsed time only */
        timeOk = subtractTimestamps(&endTime, pStartTime,
                                    &minutes, &seconds);
        fprintf(uctout, "\tElapsed time : ");
        if( timeOk )
        {   if( (minutes / 60) != 0 )
            {   fprintf(uctout, "%2d:", minutes / 60);
                minutes %= 60;
            }
            fprintf(uctout, "%02d:%02d\n", minutes, seconds);
        }
        else
        {   fprintf(uctout, "could not subtract times\n");
        }
    }
    else    /* try to print start or end time */
    {   fprintf(uctout, "\tStart time : ");
        if( pStartTime != NULL )
        {   tz = GET_TIMEZONE(pStartTime);
            printTimestampShort(pStartTime, TRUE, NULL);
            fprintf(uctout, " (%s)\n", TIMEZONE_TYPE1TXT(tz));
        }
        else
        {   fprintf(uctout, "time error\n");
        }
        fprintf(uctout, "\t  End time : ");
        if( timeOk )
        {   tz = GET_TIMEZONE(&endTime);
            printTimestampShort(&endTime, TRUE, NULL);
            fprintf(uctout, " (%s)\n", TIMEZONE_TYPE1TXT(tz));
        }
        else
        {   fprintf(uctout, "time error\n");
        }
    }
}

/* Determine presence of UDF file system on the device
 * Initialize UctVolume structure for the volumes that needs testing
 * Determine the UDF version (either specified or automatic) for all
 * volumes that need testing
 * Call the proper tests
 * TODO: There shall be exactly one prevailing Logical Volume (== LVD) !!
 */
static bool checkVolume( Device *device, genericOptions *localOptions )
{
    UdfMountContext     *mc;
    const MediumInfo    *vmi;
    Timestamp            startTime;
    bool                 timeOk,
                         fatalError = FALSE;
    Uint32               verifySession;
    Uint32               verifySessionStart;

    /* Initialize verifier 'theMediumInfo' structure
     * This structure initially is a copy of device->mediumInfo, but
     * may later be updated using the modifyTheMedium...() functions.
     * finishMediumInfo() will also check lastValidBlockNr.
     */
    if( !finishMediumInfo( &device->mediumInfo ) )  /* consistency check */
    {   return FALSE;
    }
    /* copy from device MediumInfo to 'TheMediumInfo'
     */
    if( !initTheMediumInfo(&device->mediumInfo) )
    {
        fprintf(uctout,
            "-\tUnable to initialize theMediumInfo correctly,\n"
            "-\tmaybe device info / command line options conflict.\n");
        return FALSE;
    }
    vmi = getTheMediumInfo();       /* verifier MediumInfo */
    verifySession       =  vmi->verifySession;
    verifySessionStart  = (vmi->sessionStartBlocks)[verifySession-1];

    if( vmi->numberOfSessions != 1 )
    {
        fprintf(uctout,
            "\nMultisession medium, %u sessions\n"
              "Start verify in session %u, start location %u\n",
                vmi->numberOfSessions, verifySession, verifySessionStart);
    }
    fprintf(uctout, "\n");
    (void) mlShowAndAssertLayers();

    fprintf(uctout,"\nVerification start medium info\n");
    printMediumInfo(vmi);

    if( localOptions->inspectImage || localOptions->writeImagePath )
    {
        /* note that (localOptions->writeImagePath == NULL)
         * means: inspect only.
         */
        inspectAndWriteImage(device, localOptions->writeImagePath,
                                     localOptions->imageStartBlock);
        uctExit(EXIT_OK);   /* exit after -write_image or -inspect_image */
    }
    timeOk = createCurrentTimeTimestamp(&startTime);

    fprintf(uctout, "\n====>\tStart verification"
                    "\n\tStart time   : ");
    if( timeOk )
    {   Int16 tz = GET_TIMEZONE(&startTime);
        printTimestampShort(&startTime, TRUE, NULL);
        fprintf(uctout, " (%s)", TIMEZONE_TYPE1TXT(tz));
    }
    else
    {   fprintf(uctout, "time error");
    }
    fprintf(uctout, "\n\tVerbose level: %d", uctVerboseLevel);
    fprintf(uctout, "\n\tMessage limit: ");
    if(      uctMessageLimit == 0 )
         fprintf(uctout, "all infinite");
    else if( uctMessageLimit >= MLIMITinfinite )
         fprintf(uctout, "infinite");
    else fprintf(uctout, "%lu", uctMessageLimit);

    if( uctDoFileCRC )
    {   fprintf(uctout, "\n\tFile body CRC calculation enabled");
    }
    else    /* print only if no file CRC */
    {   fprintf(uctout, "\n\tFake read %sabled",
                                (uctDoFakeRead) ? "en" : "dis");
    }
    fprintf(uctout, "\n\tRead cache %sabled",
                            (uctUseReadCache) ? "en" : "dis");

    fprintf(uctout, "\n\tInitial UDF Revision range: ");
    printUdfRevisionRange(VERBOSE00level, NULL);
    fprintf(uctout, "\n");
    (void) mlShowAndAssertLayers();

    fprintf(uctout, "\n====>\tVolume Structure verification\n");

    if( !readVolumeRecognitionSequence(device, verifySessionStart) )
    {
        printAndClearUctErrorMessage("-");
        fprintf(uctout, "\tVolume Recognition Sequence error\n\n");
    }
    fprintf(uctout, "\tReading Volume Information");

    if( verifySessionStart != 0 )
         fprintf(uctout, ", S = %u\n", verifySessionStart);
    else fprintf(uctout, "\n");

    /* alloc and clear memory for mount context
     * initialize for fields with an initial value
     * that is not equal to 0/NULL.
     */
    if( (mc = NEWSTRUCT(UdfMountContext,1)) == NULL )
    {   printElapsedTime((timeOk) ? &startTime : NULL);
        uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    mc->device = device;
    mc->virtualPref = mc->sparablePref
                    = mc->metadataPref = PREF_PARTITION_NOT_FOUND;

    if( !udfGetVolumeInformation(mc))
    {
        printAndClearUctErrorMessage("");
        fprintf(uctout,
            "- checkVolume fatal error, udfGetVolumeInformation failed\n");
        printElapsedTime((timeOk) ? &startTime : NULL);
        return FALSE;
    }

    /** check 'far apart' for Main VDS and Reserve VDS
     **/
    (void) checkVDSFarApart(mc);

    /* Exact medium UDF revision must be known by now.
     * If getUctMaxUdfRevision() != getUctMinUdfRevision(),
     * then getUctUdfRevision() returns zero.
     */
    if( getUctUdfRevision() == 0 )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
           "\tError: UDF revison range: ");
        printUdfRevisionRange(MLIMIT.vl, ". Could not determine\n"
          "-\t       exact UDF revision,"
                            " please specify: -udf <revision>\n");
      MLIMITend;
      return FALSE;
    }

    /* Medium UDF revision is known now.
     * Final check on partition access type.
     */
    (void) finalCheckPartitionAccessType(mc);

    fprintf(uctout, "\n====>\tChecking Logical Volume: ");
    PRINTDSTRING( mc->vi->lvd->logicalVolumeIdentifier,
           sizeof(mc->vi->lvd->logicalVolumeIdentifier), "\n\n");

    if( !udfMountLogicalVolume(mc, &startTime) )
    {
        printAndClearUctErrorMessage("");
        fprintf(uctout,
            "- checkVolume fatal error, udfMountLogicalVolume failed\n");
        fatalError = TRUE;
    }
    else    /* mount ok */
    { /* show/check all volume identifiers for the first time
       * and check File Structures
       */
      checkVolumeIdentifiersSummary(mc);

      fprintf(uctout, "\n====>\tFile Structure verification\n");
      if( !checkFileStructure(mc) )
      {
        printAndClearUctErrorMessage("");
        fprintf(uctout,
            "- checkVolume fatal error, checkFileStructure failed\n");
        fatalError = TRUE;
      }
    }
    fprintf(uctout, "\n" );

    if( !fatalError )
    {   fprintf(uctout, "\tTest complete\n" );
    }
    printElapsedTime((timeOk) ? &startTime : NULL);

    /* show/check all volume identifiers for the second time
     * (just before the EntityID summary)
     */
    checkVolumeIdentifiersSummary(mc);

    /** In a 'low memory' situation, udfUnmountLogicalVolume(mc)
     ** costs a lot more time then just quit and free all
     ** structures at exit from main(),
     ** so skip:
     **
     ** udfUnmountLogicalVolume(mc);
     ** free(mc);
     **/

    return (!fatalError);

}   /* end checkVolume() */

/* status definitions for inspectVATorAVDP()
 * and inspectVATorAVDPprint():
 */
#define IVA_STATUS_NONE 0
#define IVA_STATUS_VAT1 1
#define IVA_STATUS_VAT2 2
#define IVA_STATUS_AVDP 3

/* inspectVATorAVDPprint() used by inspectVATorAVDP():
 * print VAT or AVDP range from
 * statusStartBlock thru (blocknmb - 1).
 * Note: mainVDSloc and resVDSloc are only used
 * for (status == IVA_STATUS_AVDP).
 */
static void inspectVATorAVDPprint( int    status,
                                   Uint32 statusStartBlock,
                                   Uint32 blocknmb,
                                   Uint32 mainVDSloc,
                                   Uint32 resVDSloc)
{
    Uint32 nmbBlocks = blocknmb - statusStartBlock;

    if(   nmbBlocks > 0
       && status != IVA_STATUS_NONE )
    {
        fprintf(uctout, "%7lu\t%s",
            statusStartBlock,
              (status == IVA_STATUS_VAT1) ? "VAT1"
            : (status == IVA_STATUS_VAT2) ? "VAT2"
            : (status == IVA_STATUS_AVDP) ? "AVDP"
                                          : "????");
        if( status == IVA_STATUS_AVDP )
        { fprintf(uctout, "\t(MVDS: %lu, RVDS: %lu)",
                        mainVDSloc, resVDSloc);
        }
        if( nmbBlocks > 1 )
        { fprintf(uctout, "\t(%lu times at %lu thru %lu)",
            nmbBlocks, statusStartBlock, blocknmb - 1);
        }
        fprintf(uctout, "\n");
    }
}       /* end inspectVATorAVDPprint() */

/* inspectVATorAVDP():
 * Show VAT and AVDP blocks and if found any, determine
 * highest block number for VAT and AVDP separately.
 * Print range of descriptors on one line
 * using inspectVATorAVDPprint().
 *
 * (*pHighVAT)    : higest VAT address encountered
 * (*pHighAVDP)   : higest AVDP address encountered
 * (*pHighAVDP256): higest AVDP address that is less
 *                  than or equal to lastRecordedBlockNr - 256
 *
 * These values are only changed if their value increments,
 * So initializing (to 0) is assumed to be done elsewhere.
 */

static bool inspectVATorAVDP( Byte *buff,
                        Uint32  startBlockNmb, Uint32 nmbBlocks,
                        Uint32  blockSize, Uint32 lastRecorded,
                        Uint32 *pHighVAT,
                        Uint32 *pHighAVDP, Uint32 *pHighAVDP256 )
{
    Uint16  tagId;
    Uint32  n, blockNmb, statusStartBlock;
    Uint32  mainVDSloc, mainVDSprevloc,     /* VDS locations */
            resVDSloc, resVDSprevloc;       /* previous locations */
    int     status, newstatus;
    bool    foundAny = FALSE;

    mainVDSloc = mainVDSprevloc =       /* keep compiler */
     resVDSloc = resVDSprevloc = 0;             /* happy */

    /* keep track of AVDPs pointing at identical main/reserve VDS
     */
    for( n = 0, status = IVA_STATUS_NONE,
                blockNmb = statusStartBlock = startBlockNmb;
         n < nmbBlocks;
         n++,   blockNmb++, buff += blockSize )
    {   /* Determine if AVPD or VAT E(FE).
         */
        newstatus = IVA_STATUS_NONE;
        if( inspectDescriptorHead(
                    buff, blockSize, blockSize,
                    TRUE, tidUNKNOWN,   /* TagChecksum error is fatal */
                    &tagId, NULL) )
        {   /* recognized as decsriptor head
             */
            switch( tagId )
            {
            case tidAVDP:
              { AnchorVolumeDescriptorPointer *avdp =
                  (AnchorVolumeDescriptorPointer*) buff;
                mainVDSloc = avdp->mainVolumeDescriptorSequenceExtent.extentLocation;
                resVDSloc = avdp->reserveVolumeDescriptorSequenceExtent.extentLocation;
                endianSwap((Byte*) &mainVDSloc, sizeof(Uint32), 1, NULL);
                endianSwap((Byte*) &resVDSloc, sizeof(Uint32), 1, NULL);
                newstatus = IVA_STATUS_AVDP;
                if( status != IVA_STATUS_AVDP )     /* previous one was no AVDP */
                {   mainVDSprevloc = mainVDSloc;    /* fake 'equal' one */
                    resVDSprevloc = resVDSloc;
                }
                *pHighAVDP = MAX(*pHighAVDP, blockNmb);
                if( (blockNmb + 256) <= lastRecorded )
                { *pHighAVDP256 = MAX(*pHighAVDP256, blockNmb);
                }
              }
              break;
            case tidFE:     /* fall through */
            case tidEFE:
                /* check file type for VAT1 or VAT2.
                 * note that pFE_icbTag() macro is endian swap independant
                 * and fileType is Uint8, so no need for endian swap.
                 */
                switch( (pFE_icbTag(buff))->fileType )
                {
                case FT_UNKNOWN_OR_VAT150:          /* UDF 1.50 */
                    newstatus = IVA_STATUS_VAT1;
                    *pHighVAT = MAX(*pHighVAT, blockNmb);
                    break;
                case FT_VAT200:                     /* UDF 2.xx */
                    newstatus = IVA_STATUS_VAT2;
                    *pHighVAT = MAX(*pHighVAT, blockNmb);
                    break;
                }
                break;
            }
        }
        if(   newstatus != status
           || (   status == IVA_STATUS_AVDP     /* previous was AVDP */
               && (   mainVDSloc != mainVDSprevloc   /* VDS location */
                   ||  resVDSloc != resVDSprevloc)) )   /* not equal */
        {
            foundAny = TRUE;        /* found AVDP or VAT */
            inspectVATorAVDPprint(status, statusStartBlock, blockNmb,
                                  mainVDSprevloc, resVDSprevloc);
            status = newstatus;
            statusStartBlock = blockNmb;
            mainVDSprevloc = mainVDSloc;
            resVDSprevloc = resVDSloc;
        }
    }       /* endfor */

    /* print last range, if any
     */
    inspectVATorAVDPprint(status, statusStartBlock, blockNmb,
                          mainVDSprevloc, resVDSprevloc);
    return foundAny;

}   /* end inspectVATorAVDP() */

/* readAndInspectBlocks():
 * Inspect blocks for lastRecordedBlockCorrection().
 *
 * (*pHighOkRead) : highest block address that could be read
 * (*pHighVAT)    : highest VAT address encountered
 * (*pHighAVDP)   : highest AVDP address encountered
 * (*pHighAVDP256): highest AVDP address that is less
 *                  than or equal to lastRecordedBlockNr - 256
 * These values are only changed if their value increments,
 * So initializing (to 0) is assumed to be done elsewhere.
 * For details, see inspectVATorAVDP().
 */
#define IB_MAXBUFSIZE   (4*1024*1024)

static void readAndInspectBlocks( Device *device,
                                  Uint32  startAddress,
                                  Uint32  nmbOfBlocks,
                                  Uint32  sessionStart,
                                  Uint32 *pHighOkRead,
                                  Uint32 *pHighAVDP,
                                  Uint32 *pHighAVDP256,
                                  Uint32 *pHighVAT )
{
    Uint8 *buffer;
    Uint32 highAddress, blockAddress,
           chunkBlocks, okBlocks,
           blockSize = device->mediumInfo.blockSize,
           cntErrors = 0, cntSkipped = 0;

    time_t t1 = (time_t) 0,     /* make compiler happy */
           t2;

    /* chunkBlocks:
     *  nmb of blocks to read in one deviceReadBlockRaw() call.
     *  This value may decrement in subsequent read calls,
     *  but NEVER increment.
     */
    highAddress = startAddress + nmbOfBlocks - 1;
    chunkBlocks = MIN(nmbOfBlocks, IB_MAXBUFSIZE / blockSize);

    if( (buffer = tst_malloc(chunkBlocks * blockSize,
                             __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    fprintf(uctout,
      "\nInspect %lu block%s for presence of VAT or AVDP\n",
            nmbOfBlocks, PLURAL_S(nmbOfBlocks));
    fprintf(uctout,
        "starting at block: %lu%s\n",
             startAddress,
            (startAddress == (sessionStart + 513))
                      ? "   (session start + 513)"
          : (startAddress == (sessionStart + 257))
                      ? "   (session start + 257)" : "");
    fprintf(uctout, "\n");
    fflush(uctout);

    for(  blockAddress  = startAddress;
          blockAddress <= highAddress
       && blockAddress >= startAddress;
          blockAddress += okBlocks + (okBlocks != chunkBlocks) )
    {
        chunkBlocks = MIN(chunkBlocks,
                          highAddress - blockAddress + 1);

        /* in a sequence of error blocks, read one block
         * at a time, if an ok block is found, then read the
         * full chunk (repeating the read for the first block)
         * Use deviceReadBlockRaw() instead of deviceReadBlock()
         * in order to avoid extensive error logging.
         */
        okBlocks = 0;
        if(   cntErrors == 0
           || deviceReadBlockRaw(device, blockAddress,
                                 1, buffer)
              == 1 )
        {
            okBlocks = deviceReadBlockRaw(device, blockAddress,
                                          chunkBlocks, buffer);
        }

        if( okBlocks != 0 ) /* at least one correct block read */
        {
            if( cntErrors )     /* log error blocks first */
            {
                printReadErrorBlocks(cntErrors, cntSkipped,
                                     blockAddress - cntErrors);
                cntErrors = cntSkipped = 0;
            }
            fprintf(uctout,
                "%7lu\t     %4lu ok block%s read\n",
                blockAddress, okBlocks, PLURAL_S(okBlocks));

            /* remind highest ok block, VAT block
             * and AVDP block, show VATs and AVDPs.
             */
            *pHighOkRead = MAX(*pHighOkRead, blockAddress + okBlocks - 1);
            inspectVATorAVDP(buffer, blockAddress, okBlocks,
                        blockSize, device->mediumInfo.lastRecordedBlockNr,
                        pHighVAT, pHighAVDP, pHighAVDP256);
        }
        if( okBlocks != chunkBlocks )   /* one error block */
        {   /* log error block chunks after a correct
             * block has been found or at the end
             */
            cntErrors++;
            if( cntErrors == 1 )        /* first err after ok block */
            {   t1 = time(NULL);              /* freeze time */
            }

#undef  USE_WI_SHOWPROGRESS_FUNCTION    /* normally #undef (max only about 456 blocks) */
#ifdef  USE_WI_SHOWPROGRESS_FUNCTION

            wiShowProgress(cntErrors, "err:", INFO01level);

#else           /* use local progress code */

            if( cntErrors == 1 )    /* first err after ok block */
            {   fprintf(uctout, "err:\t"); /* show progress */
            }
            else if( (cntErrors % 50) == 1 )
            {   fprintf(uctout, "\n\t"); /* show progress */
            }
            fprintf(uctout, ".");       /* show progress */

#endif      /* USE_WI_SHOWPROGRESS_FUNCTION */

#define MAYBE_SKIP4OF7LINKBLOCKS
#ifdef  MAYBE_SKIP4OF7LINKBLOCKS
            /* If reading subsequent error blocks takes
             * too much time this may be because an attempt
             * is made to read 7 link blocks. In that case
             * reading link blocks 3 thru 6 may be skipped.
             * Never skip the very last block.
             */
            if( cntErrors > 1 && cntErrors < 6 )
            {   UCTASSERT( okBlocks == 0 );
                t2 = time(NULL);
                if( (t2 - t1) > 3 ) /* more than 3 seconds */
                {   while(   (blockAddress + 1) < highAddress
                          && cntErrors < 6)
                    { blockAddress++;   /* skip, assume  */
                      cntErrors++;       /* error block,  */
                      cntSkipped++;       /* mark skipped. */
                      fprintf(uctout, "s");/* show progress */
                    }
                }
                t1 = t2;
            }
#endif
        }
        fflush(uctout);
    }
    if( cntErrors )     /* log last errors if any */
    {   printReadErrorBlocks(cntErrors, cntSkipped,
                             blockAddress - cntErrors);
        cntErrors = cntSkipped = 0;
    }
    clearUctErrorMessage();
    free(buffer);
    fflush(uctout);

}   /* end readAndInspectBlocks() */

/* Note that lastRecordedBlockCorrection() only uses the
 * device MediumInfo.
 */
static bool lastRecordedBlockCorrection(Device *device)
{
    MediumInfo *dmi = &device->mediumInfo;
    Uint32 highOk = 0,      /* highest block that can be read */
           highVAT = 0,     /* highest VAT found */
           highAVDP = 0,    /* highest AVDP found at N */
           highAVDP256 = 0, /* highest AVDP found before N-255 */
           sessionStart, initialtLastRec,
           nBlocks, verifySession, newLRA;

    if( dmi->lastRecordedBlockNr != 0 )
    {   /* there are readable block(s)
         * Test if highest blocks are readable and if they
         * contain a VAT or AVDPs because many drives/media do
         * not return the correct value for N (last recorded block).
         * This can be very slow, because of read-retries.
         * First determine how many sectors to read.
         */

#define MAX_NBLOCKS_CORRECTION   200        /* post-gap + link blocks */
#define DEFAULT_INSPECT_NBLOCKS (1+256+MAX_NBLOCKS_CORRECTION)

        /* find session start S and start address for inspection.
         */
        verifySession = dmi->verifySession;
        sessionStart = (dmi->sessionStartBlocks)[verifySession-1];
        initialtLastRec = dmi->lastRecordedBlockNr;
        if( initialtLastRec < sessionStart )
        {   return FALSE;           /* error flagged elsewhere */
        }

        /* first try if VAT or AVDP at initialtLastRec
         * TODO: but if lastvalidblock differs from lastrecordedblock,
         *       then inspect 450 blocks at once in order to find
         *       a possible AVDP at N-256.
         */
        readAndInspectBlocks(device, initialtLastRec, 1,
                             sessionStart,
                            &highOk,      &highAVDP,
                            &highAVDP256, &highVAT);
        UCTASSERT( highAVDP >= highAVDP256 );   /* assert */

        newLRA = MAX(highAVDP, highVAT);

        /* inspect more blocks if no VAT or AVDP found
         * or if lastValidBlockNr differs from lastRecordedBlockNr
         * (in the latter caes, an AVDP at lastRecordedBlockNr - 256
         *  may give rise to a lastValidBlockNr correction).
         */
        if(   newLRA == 0               /* no AVDP or VAT found */
           || dmi->lastValidBlockNr != initialtLastRec )
        { Uint32 startAddress;
          /* Inspect more blocks.
           * Avoid starting lower than S+513 because of possible
           * AVDP at S+512 and reserved tracks below S+512. Likewise
           * avoid AVDP below S+257 for sessions smaller than 513.
           */
          startAddress = sessionStart;
          if( startAddress + DEFAULT_INSPECT_NBLOCKS < initialtLastRec + 1 )
          { startAddress = initialtLastRec + 1 - DEFAULT_INSPECT_NBLOCKS;
          }
          if( initialtLastRec >= (sessionStart + 513) )
          { startAddress = MAX(startAddress, sessionStart + 513);
          }
          else /* session less than 513 blocks, no inspection lower than S+257 */
          { startAddress = MAX(startAddress, sessionStart + 257);
          }
          startAddress = MIN(startAddress, initialtLastRec);    /* assert */

          /* nBlocks: total nmb of blocks for inspection,
           *            do not inspect initialtLastRec again.
           */
          nBlocks = initialtLastRec - startAddress;

          if( nBlocks != 0 )
          { readAndInspectBlocks(device, startAddress, nBlocks,
                                 sessionStart,
                                &highOk,      &highAVDP,
                                &highAVDP256, &highVAT);
          }
          /* if highAVDP256 found, then also highAVDP must be found,
           */
          UCTASSERT( highAVDP >= highAVDP256 );

          if( highOk == 0 ) /* no block could be read */
          { fprintf(uctout,
                "\nError: No readable last recorded block in range:"
                " %lu thru %lu\n", startAddress, initialtLastRec);
          }
        }

        /* determine value of N (last recorded block number)
         * if VAT or AVDP was found then assume:
         * VAT at N, AVDP at N or AVDP at N-256.
         * No correction for N if more than MAX_NBLOCKS_CORRECTION.
         */
        if( highOk == 0 )   /* no block could be read */
        {   newLRA = initialtLastRec;
/** do not handle as fatal for the moment
 **/    }
        else if( highAVDP == 0 && highVAT == 0 ) /* highAVDP256 == 0 */
        {   newLRA = highOk;    /* highOk != 0, last readable block */
        }
        else if( highVAT > highAVDP ) /* VAT highest, ignore highAVDP256 */
        {   newLRA = highVAT;
        }
        else if( highAVDP != 0 )
        {   /* highVAT not highest and AVDP found,
             * take higest of highAVDP and 256 + highAVDP256
             */
            newLRA = MAX(highAVDP, 256 + highAVDP256);
        }
        else
        {   UCTASSERT( FALSE ); /* please not here */
        }

        /* maybe correction of lastRecordedBlockNR to newLRA
         */
        if( (initialtLastRec - newLRA) <= MAX_NBLOCKS_CORRECTION )
        {   dmi->lastRecordedBlockNr = newLRA;  /* do correction */
        }
        else        /* correction too big */
        {   fprintf(uctout,
              "\nWarning: No last recorded block correction"
                                        " because correction\n"
                     "-\t to %lu would be more than %lu blocks (%lu).\n",
                    newLRA, MAX_NBLOCKS_CORRECTION,
                    (initialtLastRec - newLRA));
        }

        /* show possible correction
         */
        if( dmi->lastRecordedBlockNr != initialtLastRec )
        { /* last recorded address adapted */
          fprintf(uctout,
            "\nWarning: Last recorded block corrected from %lu to %lu (-%lu)\n",
                initialtLastRec, dmi->lastRecordedBlockNr,
                initialtLastRec - dmi->lastRecordedBlockNr);
        }

        /* correction of lastRecordedBlockNr may make highAVDP256
         * invalid, check.
         */
        if( (highAVDP256 + 256) > dmi->lastRecordedBlockNr )
        { /* reread to find new highAVDP256, check only
           * block dmi->lastRecordedBlockNr - 256 !!!
           */
          highAVDP256 = 0;
          readAndInspectBlocks(device,
                               dmi->lastRecordedBlockNr - 256, 1,
                               sessionStart,
                              &highOk,    &highAVDP,
                              &highAVDP256, &highVAT);

        }

        /* if 2nd avdp found, then also correct last valid block number
         * (no max correction)
         */
        if(   dmi->lastValidBlockNr != dmi->lastRecordedBlockNr
           && (             highAVDP == dmi->lastRecordedBlockNr
               || (highAVDP256 + 256) == dmi->lastRecordedBlockNr) )
        { /* adapt also last valid address */
          fprintf(uctout,
            "Warning: Last    valid block corrected from %lu to %lu (-%lu)\n"
                  "\t because second AVDP found.\n",
                dmi->lastValidBlockNr, dmi->lastRecordedBlockNr,
                dmi->lastValidBlockNr - dmi->lastRecordedBlockNr);
          dmi->lastValidBlockNr = dmi->lastRecordedBlockNr;
        }
        fprintf(uctout, "\n");

        if( highVAT == 0 && highAVDP == 0 ) /* highAVDP256 == 0 */
        {   fprintf(uctout, "no VAT or AVDP found so far\n");
        }
        /* Remind that (highAVDP >= highAVDP256) was asserted.
         */
        if( highAVDP != 0 )
        {   fprintf(uctout, "last AVDP at %lu (N", highAVDP);   /*)*/
            if( highAVDP != dmi->lastValidBlockNr )
            { fprintf(uctout, "-%lu",
                        dmi->lastValidBlockNr - highAVDP);
            }
            if( highAVDP256 != 0 && highAVDP256 != highAVDP )
            { fprintf(uctout, "),\tAVDP at %lu (N", highAVDP256); /*)*/
              if( highAVDP256 != dmi->lastValidBlockNr )
              { fprintf(uctout, "-%lu",
                        dmi->lastValidBlockNr - highAVDP256);
              }
            }
/*(*/       fprintf(uctout, ")\n");
        }

        if( highVAT != 0 )
        {   fprintf(uctout, "last VAT  at %lu (N", highVAT);    /*)*/
            if( highVAT != dmi->lastValidBlockNr )
            { fprintf(uctout, "-%lu",
                        dmi->lastValidBlockNr - highVAT);
            }
/*(*/       fprintf(uctout, ")\n");
        }
        fflush(uctout);
    }
    return TRUE;

}   /* end lastRecordedBlockCorrection() */


/* main program
 */
extern int main(int argc, char **argv)
{
    Device         *device = NULL;
    bool            overruled,
                    fatalError = FALSE;
    int             type;
    MediumInfo      clMediumInfo;
    genericOptions  localOptions;

    initForAlloc();     /* init spare mem for alloc problems */

    showAbout();
    clearUctErrorMessage();

    /* some static asserts, etc.
     */
    if( !uctInitialize() )
    {   uctExit(EXIT_PROGRAM_ERROR);    /* quit */
    }

    localOptions.helpOptionFound = FALSE;   /* defaults */
    localOptions.writeImagePath = NULL;
    localOptions.imageStartBlock = 0;
    localOptions.inspectImage  = FALSE;

    /* Parse command line arguments, generic options first.
     * Recognized arguments are marked out by: argv[x] = NULL
     * Some options directly set global variables.
     */
    if(    argc > 1
       && !parseGenericOptions(argc, argv, &localOptions) )
    {
        fatalErrorUsage();
        uctExit(EXIT_COMMANDLINE_ERROR);    /* quit */
    }
    else if( argc <= 1 || localOptions.helpOptionFound )
    {
        Usage();                /* usage text showed on errors   */
        extraHelpUsage();       /* showed with -help option only */
        uctExit(EXIT_OK);           /* no further action */
    }

    /* Parse medium options now
     * clMediumInfo will be "cleared" first
     */
    fprintf(uctout, "Medium info options parsing:\n");

    if( !parseMediumOptions(argc, argv, &clMediumInfo, &overruled))
    {
        fatalErrorUsage();
        uctExit(EXIT_COMMANDLINE_ERROR);    /* quit */
    }
    else if( overruled )
    {
        fprintf(uctout,
            "Error:  Option overruled on command line. Not allowed because\n"
            "-       parsing is not always done in command line order.\n");
        fatalErrorUsage();
        uctExit(EXIT_COMMANDLINE_ERROR);    /* quit */
    }
    fprintf(uctout, "\n");

    /* last command line arguments for device selection
     */
    if(    selectDevice(argc, argv, &clMediumInfo, &device) == FALSE
        || device == NULL )
    {
        fatalErrorUsage();
        uctExit(EXIT_QUIT);         /* no detailed reason, quit */
    }
    fprintf(uctout, "\n");

    /* ignore medium type info as read from a device
     */
    type = device->mediumInfo.writabilityType;
    if( type != MTYPE_WR_UNKNOWN )
    {   fprintf(uctout, "=> ignore device medium WR type: %s\n",
                MTYPE_WR_TEXT(type));
    }
    type = device->mediumInfo.sequentialType;
    if( type != MTYPE_SE_UNKNOWN )
    {   fprintf(uctout, "=> ignore device medium SE type: %s\n",
                MTYPE_SE_TEXT(type));
    }
    type = device->mediumInfo.closedType;
    if( type != MTYPE_CL_UNKNOWN )
    {   fprintf(uctout, "=> ignore device medium CL type: %s\n",
                MTYPE_CL_TEXT(type));
    }
    device->mediumInfo.writabilityType = MTYPE_WR_UNKNOWN;
    device->mediumInfo.sequentialType  = MTYPE_SE_UNKNOWN;
    device->mediumInfo.closedType      = MTYPE_CL_UNKNOWN;

    /* if eccLength still zero,
     * then set it to MI_DEFAULT_ECC_BLOCKING
     * This is not the default for UDF, but many error
     * messages may be missed if e.g. set to 1.
     */
    if( device->mediumInfo.eccLength == 0 )
    { device->mediumInfo.eccLength = MI_DEFAULT_ECC_BLOCKING;
      MLIMITbegin(WARN01level, uctMessageLimit);
        fprintf(uctout,
          "=> Warning: Undefined (ECC) blocking factor, set to %lu.\n"
          "-\t    Please specify \"-ecclength <n>\" to enable the verifier to do\n"
          "-\t    a better job. Media that do not have ECC or fixed size packets\n"
          "-\t    must specify: -ecclength 1\n",
        device->mediumInfo.eccLength);
      MLIMITend;
    }

    /* Iff no lastvalidblock defined on command line, then
     * check for VATs and AVDPs at the end of the volume space.
     * This may possibly lead to a final correction of the
     * volume space (lastvalidblock).
     */
    if( clMediumInfo.lastValidBlockNr == 0 )
    {   /* no -lastvalidblock command line option
         */
        lastRecordedBlockCorrection(device);
    }

    /* run checkVolume(),
     * maybe only write_image or inspect_image.
     */
    if( !checkVolume(device, &localOptions) )
    {   fatalError = TRUE;
    }

    showFinalReport(fatalError);
    device = deviceCloseAndFreeDevice( device );

    uctExit( (fatalError )                ? EXIT_INCOMPLETE_FATAL
           : (uctGlobalErrorCount != 0)   ? EXIT_WITH_ERR
           : (uctGlobalWarningCount != 0) ? EXIT_WITH_WARN_NOERR
                                          : EXIT_OK );
}   /* end main() */

