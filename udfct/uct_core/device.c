/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : device.c
 *
 * Description : Generic Device interface
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "general.h"
#include "mytypes.h"
#include "uctgeneral.h"

#include "device.h"


/* Lowest level deviceReadBlock function
 */
static Uint32 deviceReadLowest( Device *device,
                                Uint32  firstBlock,
                                Uint32  reqBlocks,
                                Byte   *buffer )
{   Uint32 firstUnrecorded;
    Uint32 blocksRead;

    /* If uctDoReadGap == TRUE
     * then: read in unrecorded gaps
     * else: do not read in unrecorded gaps, check
     *       overlap with unrecorded gaps, if any.
     * (uctDoReadGap set TRUE by -readgap option)
     */
    if(    (!uctDoReadGap)
        && gapFindFirstUnrecordedBlock(&device->mediumInfo,
                                     firstBlock, reqBlocks,
                                    &firstUnrecorded) )
    {   /* unrecorded block (due to gap) found in read area
         */
        reqBlocks = firstUnrecorded - firstBlock;
        if( reqBlocks == 0 )
        { return 0;         /* no blocks can be read */
        }
    }
    blocksRead = device->readBlock(device->impUse,
                                   device->mediumInfo.blockSize,
                                   firstBlock, reqBlocks, buffer);
    return blocksRead;

}   /* end deviceReadLowest() */


/* First simple read cache ***************************************
 * A maximum of READCACHE_MAXITEMS cache items with each
 *  a buffer of READCACHE_PACKET blocks.
 */
//#define READCACHE_MAXITEMS    16      /* low (Uint8), not zero */
//#define READCACHE_PACKET      64      /* blocks (logical sectors) */
#define READCACHE_MAXITEMS  32      /* low (Uint8), not zero */
#define READCACHE_PACKET    32      /* blocks (logical sectors) */
                                    /** 64k for 2k sectors **/
///** testing: make byte size of READCACHE_PACKET (blocks) equal
///**       to SCSIREAD_MAXBYTES (bytes) (only  for 2k sectors !!)
// **       in order to have one read request per 'read into cache
// **/
//#define READCACHE_PACKET   32     /** 64k for 2k sectors **/

//#define READCACHE_MAXITEMS 32     /* low (Uint8), not zero */
//#define READCACHE_PACKET   64     /* blocks (logical sectors) */

#undef  DEVICE_TESTING_READCACHE    /** normally #undef **/

typedef struct ReadCacheItem
{
    Byte            *buf;
    Uint32           firstBlockLsn;     /* log sect nmb */
    Uint16           accessCount;
    Uint16           nmbValidBlocks;    /* readOk */
} ReadCacheItem;

/* This read cache is local to the functions
 * deviceReadCachePacket() and deviceReadCacheFree().
 */
static ReadCacheItem *readCacheItemArray = NULL;
static Uint32         readCacheNmbOfActiveItems = 0;

/* deviceReadCacheFree():
 * Free all space allocated for read cache and bring it
 * back to initial consistent state.
 * Also used to force medium access from initial state
 */
extern void deviceReadCacheFree()
{
    Uint32         n;
    ReadCacheItem *item;

    for( n = 0, item = &readCacheItemArray[0];
         n < readCacheNmbOfActiveItems;
         n++, item++)
    {
        checkFree((void**) &item->buf);
    }
    checkFree((void**) &readCacheItemArray);

    readCacheNmbOfActiveItems = 0;

}   /* end deviceReadCacheFree() */

/* deviceReadCachePacket():
 * Do not call this function directly !!!
 * Instead, use deviceReadBlockRaw().
 *
 * This function implements a simple read cache.
 *
 * precondition:
 *  buffer != NULL and requested blocks are part of
 *  the same READCACHE_PACKET and the total size is
 *  LESS than a READCACHE_PACKET !!
 *
 * These preconditions are guaranteed if this function is
 * only called from deviceReadBlockRaw() !!
 */
static Uint32 deviceReadCachePacket(Device *device,
                                    Uint32  firstBlock,
                                    Uint32  reqBlocks,
                                    Byte   *buffer)
{

    ReadCacheItem *item, *foundItem, *replaceItem;

    Uint32         blockSize = device->mediumInfo.blockSize,
                   firstOfCachePacket, lastRecordedBlockNr,
                   n, nb, aln;

///** begin testing deviceReadCacheFree() **/
//if(   readCacheNmbOfActiveItems != 0
//   && ((readCacheNmbOfActiveItems + readCacheItemArray[0].accessCount)
//      % READCACHE_MAXITEMS) == 14 )
//{ deviceReadCacheFree();
//}
///** end   testing deviceReadCacheFree() **/

    /* first time, allocate for readCacheItemArray[READCACHE_MAXITEMS]
     * note that NEWSTRUCT() uses tst_calloc();
     */
    if( readCacheItemArray == NULL )
    {   readCacheItemArray = NEWSTRUCT(ReadCacheItem,
                                       READCACHE_MAXITEMS);
        if( readCacheItemArray == NULL )
        {   /* allocation error, no cache, do direct read
             */
            return deviceReadLowest(device, firstBlock,
                                    reqBlocks, buffer);
        }
        fprintf(uctout, "  ==>\tread cache: max %lu buffers"
                                " of %lu sectors, total %lu Kb\n",
             READCACHE_MAXITEMS, READCACHE_PACKET,
            (READCACHE_MAXITEMS * READCACHE_PACKET * blockSize)
                            / 1024 );
        fflush(uctout);
    }
    aln = (firstBlock % READCACHE_PACKET);
    firstOfCachePacket = firstBlock - aln;

    UCTASSERT(              buffer != NULL
              &&         reqBlocks < READCACHE_PACKET
              && (aln + reqBlocks) <= READCACHE_PACKET );

    /* Find item in readCacheItemArray starting with firstOfCachePacket block.
     * Increment accesscount of ALL active items !!
     * Determine item with highest accesscount as first candidate
     * to be replaced (replaceItem).
     * The latter is only relevant if item is not found and all items
     * are in use (readCacheNmbOfActiveItems == READCACHE_MAXITEMS).
     * Note that for the first time: readCacheNmbOfActiveItems == 0.
     */
    replaceItem = &readCacheItemArray[0];
    foundItem = NULL;
    for( n = 0; n < readCacheNmbOfActiveItems; n++)
    {   item = &readCacheItemArray[n];
        if( (++(item->accessCount)) == 0 )  /* overflow */
        {   item->accessCount = MAX_UINT16; /* fix */
        }
        if( item->firstBlockLsn == firstOfCachePacket )
        {   foundItem = item;
        }
        else if( item->accessCount > replaceItem->accessCount )
        {   replaceItem = item; /* new 'not found' maximum */
        }
    }

    /* Check if new cache item must be read.
     * Note: replaceItem is irrelevant if foundItem != NULL
     */
    if( foundItem == NULL )
    {   /* Check if all items are in use. If not, create new one,
         * else re-use one with highest accessCount (replaceItem).
         */
        if( readCacheNmbOfActiveItems < READCACHE_MAXITEMS )
        {   /* not all in use, create buffer for new item
             */
            replaceItem = &readCacheItemArray[readCacheNmbOfActiveItems];
            replaceItem->buf = NEWSTRUCT(Byte,
                                  READCACHE_PACKET * blockSize);
            if( replaceItem->buf == NULL )
            {   /* memory allocation problem, free complete
                 * read cache and try direct read.
                 */
                deviceReadCacheFree();
                return deviceReadLowest(device, firstBlock,
                                        reqBlocks, buffer);
            }
            readCacheNmbOfActiveItems++; /* new item active */

#ifdef  DEVICE_TESTING_READCACHE
            fprintf(uctout, "READCACHE: %7lu: (%7lu, %3lu)"
                                "\t-> NEW CACHE BUFFER (%lu)\n",
                         firstOfCachePacket, firstBlock,
                         reqBlocks, readCacheNmbOfActiveItems);
            fflush(uctout);
#endif  /** DEVICE_TESTING_READCACHE **/

        }
        else        /* replace existing cache item */
        {
#undef  DEVICE_READCACHE_SIMPLE /** #undef for the moment **/
#ifdef  DEVICE_READCACHE_SIMPLE
          /** *simple* round robin replace strategy
           ** means: accessCount field not used, can be removed)
           **/
          static Uint8 replCnt = 0;
          replaceItem = &readCacheItemArray[replCnt];
          replCnt = (replCnt + 1) % READCACHE_MAXITEMS;
#endif  /** DEVICE_READCACHE_SIMPLE **/

#ifdef  DEVICE_TESTING_READCACHE
          fprintf(uctout, "READCACHE: %7lu: (%7lu, %3lu)"
                                "\t-> REPLACE CACHE BUFFER (%lu)\n",
            replaceItem->firstBlockLsn, replaceItem->firstBlockLsn,
            replaceItem->nmbValidBlocks, replaceItem->accessCount);
          fflush(uctout);
#endif  /** DEVICE_TESTING_READCACHE **/
        }

        /* read new full READCACHE_PACKET in replaceItem.
         * Note that a read error may be no problem, because the
         * error blocks may not be requested.
         * There is one exception where not the full READCACHE_PACKET may be
         * read and that is if the packet would go beyond lastRecordedBlockNr.
         */
        nb = READCACHE_PACKET;
        if( (lastRecordedBlockNr = device->mediumInfo.lastRecordedBlockNr)
            >= firstOfCachePacket )
        {   nb = MIN(nb, lastRecordedBlockNr - firstOfCachePacket + 1);
        }
        replaceItem->firstBlockLsn = firstOfCachePacket;
        replaceItem->accessCount   = 0;
        replaceItem->nmbValidBlocks =   /* ok blocks <= READCACHE_PACKET */
                    (Uint16) deviceReadLowest(device, firstOfCachePacket,
                                              nb, replaceItem->buf);

#ifdef  DEVICE_TESTING_READCACHE
        fprintf(uctout, "READCACHE: %7lu: (%7lu, %3lu)"
                                    "\t-> READ TO CACHE\n",
                    replaceItem->firstBlockLsn,
                    replaceItem->firstBlockLsn,
                    replaceItem->nmbValidBlocks);
        fflush(uctout);
#endif

        foundItem = replaceItem;    /* found after all */
    }
    UCTASSERT( foundItem != NULL );

    /* Copy requested blocks from cache. Check if requested blocks
     * are not 'hidden' after an error block. If this is the case,
     * a direct read, not using the cache will be issued.
     * If foundItem->nmbValidBlocks == aln, we know that firstBlock
     * caused a read error, and zero blocks will be returned.
     */
    if( foundItem->nmbValidBlocks >= aln )  /* ok, copy from cache */
    {
        nb = MIN(reqBlocks, foundItem->nmbValidBlocks - aln);
        memcpy( buffer, foundItem->buf + (aln * blockSize),
                nb * blockSize);
        foundItem->accessCount = (Uint16)       /* blocks read from */
                (foundItem->accessCount + nb);  /*  cache, maybe 0  */
//VARIANT:
//INCR  foundItem->accessCount++;   /* extra for each copy from cache */

#ifdef  DEVICE_TESTING_READCACHE
        fprintf(uctout, "READCACHE: %7lu: (%7lu, %3lu)"
                                "\t-> COPIED FROM CACHE\n",
                    foundItem->firstBlockLsn, firstBlock, nb);
        fflush(uctout);
#endif
        /* nb blocks read from cache */
    }
    else    /* foundItem->nmbValidBlocks < aln */
    {   /* Requested blocks 'hidden' after an error block.
         * Do direct read bypassing read cache.
         */
#ifdef  DEVICE_TESTING_READCACHE
        fprintf(uctout, "READCACHE: %7s: (%7lu, %3lu)\t-> DIRECT READ\n",
                        "", firstBlock, reqBlocks);
        fflush(uctout);
#endif
        nb = deviceReadLowest(device, firstBlock, reqBlocks, buffer);
    }
    return nb;      /* total nmb of blocks that could be read */

}   /* end deviceReadCachePacket() */

/* deviceReadBlockRaw(): Called by deviceReadBlock().
 * Raw device reading, maybe use simple read cache, depending
 * on the value of (global) uctUseReadCache.
 * Fake read if buffer == NULL OR nrOfBlocks == 0 (no action).
 *
 * If cache read, then split up read request in READCACHE_PACKET
 * aligned requests for deviceReadCachePacket().
 * Begin-and-end aligned packets do NOT use cache read !!
 * See detailed comment below.
 */
extern Uint32 deviceReadBlockRaw( Device *device, Uint32 firstBlock,
                                  Uint32 nrOfBlocks, Byte *buffer )
{
    Uint32  blockSize = device->mediumInfo.blockSize,
            totalBlocksRead, blocksThisTime, nb, aln;

    /* Maybe use simple read cache
     * (not recommended for media with linkblocks in
     *  volume space like CD-R, use -nocache option).
     * For fake read, we'r done
     */
    if(   buffer == NULL        /* fake read */
       || nrOfBlocks == 0 )     /* nothing to be read */
    {   return nrOfBlocks;      /* done */
    }
    if( !uctUseReadCache )      /* no read cache used */
    { return deviceReadLowest(device, firstBlock,
                              nrOfBlocks, buffer);
    }

    /* ELSE:
     *  Use cache read, split up read request in 3 chunks.
     *  READCACHE_PACKET is the number of blocks in
     *  a ReadCacheItem buffer.
     *
     * Chunk 1:
     *  If firstBlock is not READCACHE_PACKET aligned
     *  then chunk 1 is nmb of blocks till first alignment
     *  else chunk 1 is empty.
     *  Chunk 1 is empty or less than a READCACHE_PACKET.
     *
     * Chunk 2:
     *  Largest possible chunk following chunk 1 for which
     *  begin and end are both READCACHE_PACKET aligned.
     *  Chunk 2 is empty or a multiple of a READCACHE_PACKET.
     *
     * Chunk 3:
     *  The remainder of the read request, so it is empty
     *  or begin-aligned on a READCACHE_PACKET.
     *
     * Chunks 1 and 3 are smaller than a READCACHE_PACKET and
     * are read using cache read [deviceReadCachePacket()].
     *
     * Chunk 2 is empty or a multiple of the READCACHE_PACKET
     * and is read WITHOUT using cache read !!!!
     */
    totalBlocksRead = 0;

    /* Chunk 1: Till first alignment using cache read.
     */
    if( (aln = (firstBlock % READCACHE_PACKET)) != 0 )
    {   blocksThisTime = MIN(nrOfBlocks, READCACHE_PACKET - aln);
        nb = deviceReadCachePacket(device, firstBlock,
                                   blocksThisTime, buffer);
        totalBlocksRead += nb;
        if( nb != blocksThisTime )
        {   return totalBlocksRead;     /* read error */
        }
        nrOfBlocks -= blocksThisTime;   /* for next chunk */
        firstBlock += blocksThisTime;
        buffer     += blocksThisTime * blockSize;
    }
    UCTASSERT(    nrOfBlocks == 0
              || (firstBlock % READCACHE_PACKET) == 0 );

    /* Chunk 2: Begin and end aligned blocks WITHOUT cache read
     */
    blocksThisTime = ROUNDDOWNMULT(nrOfBlocks, READCACHE_PACKET);
    if( blocksThisTime != 0 )
    {   /** bypass read cache **/
        nb = deviceReadLowest(device, firstBlock,
                              blocksThisTime, buffer);
        totalBlocksRead += nb;
        if( nb != blocksThisTime )
        {   return totalBlocksRead;     /* read error */
        }
        nrOfBlocks -= blocksThisTime;   /* for next chunk */
        firstBlock += blocksThisTime;
        buffer     += blocksThisTime * blockSize;
    }
    UCTASSERT(   nrOfBlocks == 0
              || (   (firstBlock % READCACHE_PACKET) == 0
                  && nrOfBlocks < READCACHE_PACKET) );

    /* Chunk 3: Remainder using cache read
     */
    if( nrOfBlocks != 0 )
    { totalBlocksRead +=
            deviceReadCachePacket( device, firstBlock,
                                   nrOfBlocks, buffer );
    }
    return totalBlocksRead;

}   /* end deviceReadBlockRaw() */


/* Generic Device API Functions *********************************
 */
extern BlockState deviceGetBlockState( Device *device, Uint32 blockNr )
{
    if( blockNr > device->mediumInfo.lastValidBlockNr )
    {   return BSTATE_ERROR;
    }
    if( blockNr > device->mediumInfo.lastRecordedBlockNr )
    {   return BSTATE_UNRECORDED;
    }
    return device->getBlockState( device->impUse, blockNr );
}

/* deviceReadBlock():
 *
 * allowBelowVRS: Denotes if reading in the
 *  verifySessionStart till VRS start area is allowed
 *  (e.g. allow for readAndInspectBlocks() calls).
 *  This test is done AFTER the read is executed and may
 *  shrink the nmb of correct read blocks.
 *
 * Fake read if buffer == NULL.
 *
 * deviceReadBlock() does not use getTheMediumInfo() fields,
 * except that it asserts device->mediumInfo.blocksize to be
 * equal to getTheMediumInfo()->blockSize.
 */
extern Uint32 deviceReadBlock(Device *device,     Uint32 firstBlock,
                              Uint32  nrOfBlocks, Byte  *buffer,
                              bool    allowBelowVRS)
{
    const MediumInfo *dmi = &device->mediumInfo;
    Uint32 blockSize       = dmi->blockSize;
    Uint32 lastValidBlockNr = dmi->lastValidBlockNr;
    Uint32 verifySession     =  dmi->verifySession;
    Uint32 verifySessionStart = (dmi->sessionStartBlocks)[verifySession-1];
    Uint32 vrsStart, okBlocks;
    Uint8  vLevel;

    UCTASSERT( blockSize == getTheMediumInfo()->blockSize );

#ifdef UCT_TESTING
/**************************/
//if( firstBlock == 273 )
//  firstBlock = firstBlock;    /** breakpoint **/
/**************************/
//if( firstBlock == 822 )   /** testing expandDirectoryHierarchy **/
//    firstBlock =  578;    /** recursion for OSTA_V1_D6.img     **/
/**************************/
//if( firstBlock == 346 )   /** testing directory in stream     **/
//    firstBlock =  384;    /** directory for mdi_0512_4.img    **/
/**************************/
//if( firstBlock == 275 )       /** testing readIcbDirectEntry  **/
//    firstBlock =  271;        /**         infinite            **/
//if( firstBlock == 276 )       /**         recursion           **/
//    firstBlock =  272;        /**     for Strategy4096.img    **/
/**************************/
#endif

    if( buffer == NULL )        /* fake read */
         vLevel = FAKE01level;  /* fake read informational */
    else vLevel = INFO02level;

    ifVERBOSE(vLevel)
    {
        fprintf(uctout, "%7lu\t%sread ", firstBlock,
                        (buffer==NULL) ? "fake " : "");
        if( nrOfBlocks == 1 )
             fprintf(uctout,"block\n");
        else fprintf(uctout,"%lu blocks\n", nrOfBlocks);
    }
    ENDif;      /* vLevel */

    /* Extra error message (and count) if attempt to read beyond
     * lastValidBlockNr.
     */
    if(   nrOfBlocks > 0
       && (Uint64)firstBlock + nrOfBlocks - 1 > (Uint64)lastValidBlockNr )
    {   MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
            "%s error: Beyond last valid block number %lu: %lu\n",
            (buffer==NULL) ? "fake read" : "deviceReadBlock",
            lastValidBlockNr, firstBlock + nrOfBlocks - 1);
        MLIMITend;
        /* reduce nmb of blocks to be read such
         * that last valid block is read as last
         */
        if( nrOfBlocks > 0 )
        { nrOfBlocks = (firstBlock <= lastValidBlockNr)
                     ? lastValidBlockNr - firstBlock + 1
                     : 0;           /* read nothing */
        }
    }
    fflush(uctout);             /* get messages written */

    /* Do actual read. Maybe use simple read cache.
     */
    okBlocks = deviceReadBlockRaw(device, firstBlock,
                                  nrOfBlocks, buffer);

    /* error check and logging.
     */
    if( okBlocks != nrOfBlocks )    /* error and no fake read */
    {   /* a read error is not a UDF compliance error,
         * so let's print a note here.
         */
        VERBOSE00(uctout, "%7lu\tNote: READ ERROR",
                            firstBlock + okBlocks);
        if( okBlocks != 0 )
        {   VERBOSE00(uctout,
                ", only first %lu of %lu blocks read",
                                okBlocks, nrOfBlocks);
        }
        else if( nrOfBlocks != 1 )
        {   VERBOSE00(uctout,
                ", first of %lu blocks could not be read",
                                nrOfBlocks);
        }
        VERBOSE00(uctout, "\n");
    }

    /* If reading below VRS not allowed, then flag error
     *  if it was the intention to read between
     *  verifySessionStart and the VRS area,
     *  EVEN IF nrOfBlocks == 0.
     * last block intended to read: firstBlock + nrOfBlocks - 1
     */
    vrsStart = verifySessionStart
           + ROUNDUPELEMENTS(MIN_BYTES_BEFORE_VRS, blockSize);

    if(   allowBelowVRS == FALSE    /* no reading below VRS */
       && firstBlock < vrsStart
       && firstBlock + nrOfBlocks - 1 >= verifySessionStart )
    {   MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
            "  ==>\t%s error: Reading in area from session start %lu\n"
            "-\t\t\t%s till start of VRS %lu: %lu.\n",
            (buffer==NULL) ? "fake read" : "deviceReadBlock",
             verifySessionStart,
            (buffer==NULL) ? "" : "      ",
             vrsStart, MAX(firstBlock, verifySessionStart));
        MLIMITend;
        /* mimic reduction of nmb of blocks read such that block
         * before verifySessionStart is read as last.
         */
        okBlocks = (firstBlock < verifySessionStart)
                 ? MIN(okBlocks, verifySessionStart - firstBlock)
                 : 0;           /* read nothing */
    }

    return okBlocks;
}                       /* end deviceReadBlock() */


extern Device *deviceCloseAndFreeDevice( Device *device)
{
    if( device != NULL )
    {   miFreeArrays(&device->mediumInfo);
        device->closeAndFreeImpUse( device->impUse );
        free(device);
    }
    return (Device *) NULL;
}

