/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : write_image.c
 *
 * Description :
 *
 * Author(s)   : Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uct_core.h"
#include "write_image.h"
#include "platform.h"       /* platform dependent */
#include "scsi_device.h"    /* may #define SCSI_DEVICE */
                            /*  for scsiErrorOut only  */

/* Global Int64 uctWiMaxChunksize, default WI_MAX_CHUNK_SIZE.
 * Value may be reduced if -chunksize option is defined.
 * extern */
Int64 uctWiMaxChunksize = WI_MAX_CHUNK_SIZE;    /* default */

static char *wiText;        /* "Write image" or "Inspect image" */

#define WI_MAX_READSIZE     ( 32*1024*1024) /* default buffer size */

/* writeImageBlocks() block types :
 */
#define WI_OK_BLOCKS    0
#define WI_ERROR_BLOCKS 1

static Uint32 wiStartBlockNr = 0;
static Uint32 wiBlankBlocks = 0;
static Uint32 wiErrorBlocks = 0;
static Uint32 wiAVDPcnt = 0;
static Uint32 wiVAT1cnt = 0;
static Uint32 wiVAT2cnt = 0;

static Byte  *wiBlockWithBlanks;

static Uint32 wiNmbOfWriteChunks = 0;
static Uint32 wiWriteChunkNumber = 0;
static Uint32 wiBlocksPerChunk = 0;
static char  *wiPathBase = NULL;
static char  *wiPathExt = NULL;
static char  *wiWritePath = NULL;

/* wiShowProgress():
 * Show progress in case of read error blocks by (initially)
 * printing a dot for each error block, but avoid too much
 * output in case of huge unrecorded gaps
 * (or read them at once, requires unrecorded gap detection),
 * for more detail see below.
 */
extern void wiShowProgress( Uint32 count, char *txt4,
                            Uint8 vLevel )
{
    char ch;
    int  factor;

    if( count == 0 )
    {   return;             /* no error */
    }
    ifNOTVERBOSE(vLevel)
        return;             /* verbose level does not apply */
    ENDif

    /* Granularity starts with one '.' character per error block (factor 1).
     * Granularity increases by a factor 10 after 500 error blocks
     * (10 lines with 50 '.' chars each), using a '+' character, etc.
     * ... till finally for an error block count of 50000000 and higher,
     * one '@' char per 1000000 error blocks is used (50000000 per line
     * of 50 '@' chars  == 100 GB per line for a 2k block size).
     * First determine 'counts per ch character' factor.
     */
    if(      count >  50000000 ) { factor = 1000000; ch = '@'; }
    else if( count >   5000000 ) { factor =  100000; ch = '$'; }
    else if( count >    500000 ) { factor =   10000; ch = '#'; }
    else if( count >     50000 ) { factor =    1000; ch = '='; }
    else if( count >      5000 ) { factor =     100; ch = '*'; }
    else if( count >       500 ) { factor =      10; ch = '+'; }
    else                         { factor =       1; ch = '.'; }
    /* first check for new line,
     * first time with txt4 printed.
     */
    if( count == 1 )
    { fprintf(uctout, "\n%-4s  0:", txt4);      /* first time */
    }
    if( ((count-1) % factor) == 0 )         /* show progress with one */
    { fprintf(uctout, "%c", ch);            /* ch character per factor */
    }
    if( (count % (50 * factor) ) == 0 )     /* next line */
    { fprintf(uctout, "\n%7lu:", count );
    }
    fflush(uctout);

}   /* end wiShowProgress() */

/* open next chunk file for write
 * call  writeImageOpenFirstChunk() BEFORE
 * first call not writeImageOpenNextChunk() !!!
 * Output chunk file paths in case of outPath argument supplied to
 * inspectAndWriteImage(): <basepath>[<extension>] (e.g. a/CD01.img)
 * if only 1 output chunk
 *  then: equal to supplied path name     (a/CD01.img)
 * else if at most 26 chunks
 *  then: <basepath>_<x>[<extension>]  (a/CD01_a.img,  a/CD01_b.img, etc.)
 * else: <basepath>_<nn>[<extension>] (a/CD01_01.img, a/CD01_02.img, etc.)
 */
static FILE *writeImageOpenNextChunk()
{   FILE *outHandle;
    if( wiNmbOfWriteChunks == 1 && wiWriteChunkNumber == 1)
    {   sprintf(wiWritePath, "%s%s", wiPathBase, wiPathExt);
    }
    else if( wiNmbOfWriteChunks <= 26 )
    {   char c = (char) ('a' + wiWriteChunkNumber - 1);
        sprintf(wiWritePath, "%s_%c%s", wiPathBase, c, wiPathExt);
    }
    else
    {   sprintf(wiWritePath, "%s_%02d%s",
                wiPathBase, wiWriteChunkNumber, wiPathExt);
    }
    wiWriteChunkNumber++;
    VERBOSE00(uctout,
        "  ==>\tOpen output image file\t: %s\n", wiWritePath);

    if( (outHandle = (FILE *) fopen(wiWritePath, "wb")) == NULL )
    {   VERBOSE00(uctout,
            "Write image: Cannot open for write: %s\n", wiWritePath);
        uctExit(731);   /* quit */
    }
    return outHandle;
}

/* writeImageOpenFirstChunk():
 * fopen first output file for binary write and
 * initialize for writeImageOpenNextChunk().
 */
static FILE *writeImageOpenFirstChunk(char  *path,
                                      Uint32 totalBlocks,
                                      Uint32 blockSize)
{   char  *p1, *p2;
    size_t pathLen = strlen(path),
           baseLen;

    /* Global uctWiChunksize holds the minimum of  WI_MAX_CHUNK_SIZE
     * and the result in bytes of the -chunksize option.
     * Open first output chunk and prepare for next chunks.
     * Mind that wiBlocksPerChunk is sufficient so that if
     * totalBlocks is not a multiple of wiNmbOfWriteChunks,
     * the last chunk will be smaller than the others.
     */
    wiBlocksPerChunk   = (Uint32)(uctWiMaxChunksize / blockSize);
    wiNmbOfWriteChunks = ROUNDUPELEMENTS(totalBlocks, wiBlocksPerChunk);
    wiNmbOfWriteChunks = MAX(1, wiNmbOfWriteChunks);
    /* correction to make chunk sizes as equal as possible
     */
    wiBlocksPerChunk   = ROUNDUPELEMENTS(totalBlocks, wiNmbOfWriteChunks);
    wiWriteChunkNumber = 1;     /* for first chunk */

    /* allocate for base path, path extension and actual
     * write chunk path name buffers. For chunk numbers,
     * reserve 10 extra bytes.
     */
    if(   (wiPathBase = (char*) tst_malloc(pathLen + 1,
                                    __FILE__,__LINE__)) == NULL
       || (wiPathExt  = (char*) tst_malloc(pathLen + 1,
                                    __FILE__,__LINE__)) == NULL
       || (wiWritePath = (char*) tst_malloc(pathLen + 11,
                                    __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }

    /* determine nmb of chars to be copied from path to
     * wiPathBase, rest is file name extension and goes
     * to wiPathExt.
     */
    p1 = strrchr(path, (int) '\\');
    p2 = strrchr(path, (int) '/');
    if(      p1 == NULL ) p1 = p2;
    else if( p2 != NULL ) p1 = (p1>p2) ? p1 : p2;

    /* p1 == NULL or points to last occurrence of '\\' or '/'
     */
    p2 = strrchr(path, (int) '.');

    if(     p2 != NULL          /* '.' found, maybe extension */
        && (p1 == NULL || p2 > p1) )
         baseLen = p2 - path;   /* base length */
    else baseLen = pathLen;     /* no extension */

    memcpy(wiPathBase, path, baseLen);
    memcpy(wiPathExt, path + baseLen, pathLen - baseLen);
    wiPathBase[baseLen] = '\0';
    wiPathExt[pathLen - baseLen] = '\0';

    VERBOSE00(uctout, "\ttotal blocks\t\t: %lu\n"
             "\tmax output chunk blocks\t: %lu\n"
             "\t%lu output image file%s\t: %s%s%s\n\n",
        totalBlocks, wiBlocksPerChunk, wiNmbOfWriteChunks,
        PLURAL_S(wiNmbOfWriteChunks), wiPathBase,
                (wiNmbOfWriteChunks == 1)  ? "" :
                (wiNmbOfWriteChunks <= 26) ? "_<x>"
                                           : "_<nn>",
        wiPathExt);

    return writeImageOpenNextChunk();   /* first chunk */
}


/* writeImageBlocks():
 * Notes:
 * - buffer is invalid for blocksType WI_ERROR_BLOCKS
 */
static void writeImageBlocks( Byte   *buffer,
                              Uint32  blockNr,
                              size_t  nmbBlocks,
                              Uint32  blockSize,
                              Uint8   blocksType,
                              FILE  **pOutHandle)
{   size_t blWritten, blocksThisChunk;

    if( nmbBlocks == 0 )        /* nothing to write */
    {   return;
    }

    /* nmb of blocks to go in this chunk
     */
    blocksThisChunk = wiBlocksPerChunk
                    - ((blockNr - wiStartBlockNr) % wiBlocksPerChunk);
    if( nmbBlocks > blocksThisChunk )
    {   /* does not fit in current chunk, split into 2 calls to
         * writeImageBlocks() [recursion], in such a way that
         * first one will exactly fill up current chunk.
         */
        writeImageBlocks(buffer, blockNr,
                         blocksThisChunk, blockSize,
                         blocksType, pOutHandle);
        writeImageBlocks(buffer + (blocksThisChunk * blockSize),
                         blockNr + blocksThisChunk,
                         nmbBlocks - blocksThisChunk, blockSize,
                         blocksType, pOutHandle);
        return;     /* ready */
    }

    /* if exactly at begin of a chunk, close current chunk
     * and open a new one, except for first chunk.
     */
    if(  ((blockNr - wiStartBlockNr) % wiBlocksPerChunk) == 0
       && blockNr != wiStartBlockNr )
    {   fclose(*pOutHandle);
        *pOutHandle = writeImageOpenNextChunk();    /* export */
    }

    /* log write, align 'nmb of blocks' with read logging layout,
     * see deviceReadBlock().
     * read logging layout: "%7lu\tread %lu block(s)
     */
    ifPRINTinfo01(uctout,   "%7lu\t     %lu %sblock%s written\n",
             blockNr, nmbBlocks,
            (blocksType == WI_ERROR_BLOCKS) ? "blank error " : "",
            PLURAL_S(nmbBlocks) );
    ENDif;

    if( blocksType == WI_OK_BLOCKS )
    {
        blWritten = fwrite(buffer, blockSize, nmbBlocks, *pOutHandle);
        if( blWritten != nmbBlocks )
        {
            VERBOSE00(uctout, "%s fwrite error: block %lu till %lu\n",
                            wiText, blockNr + blWritten,
                            blockNr + nmbBlocks - 1);
            uctExit(721);   /* quit */
        }
    }
    else if( blocksType == WI_ERROR_BLOCKS )
    {   Uint32 n;
        for( n = 0; n < nmbBlocks; n++ )
        {   if( fwrite(wiBlockWithBlanks, blockSize, 1, *pOutHandle) != 1 )
            {
                VERBOSE00(uctout, "%s fwrite error: block %lu\n",
                            wiText, blockNr + n);
                uctExit(722);   /* quit */
            }
        }
    }
}

/* Inspect non-blank block
 */
static void inspectNonBlankBlock(Byte *bf, Uint32 blockNr, Uint32 blockSize)
{   Uint8    fileType, vLevel;
    Uint16   tagId;
    Uint32   tagLocation;

    /* Determine if AVPD or VAT E(FE) for count and logging on
     * VERBOSE00level, log other descriptors on INFO01level.
     * It is important to mention that inspectDescriptorHead()
     * does not change the data in buffer (no endian swap).
     */
    if( inspectDescriptorHead(
                bf, blockSize, blockSize,
                TRUE, tidUNKNOWN,   /* TagChecksum error is fatal */
                &tagId, NULL) )
    {   /* recognized as decsriptor head
         */
        tagLocation = ((Tag*)bf)->tagLocation;  /* unswapped */
        endianSwap((Byte*) &tagLocation, sizeof(Uint32), 1, NULL);
        switch( tagId )
        {
        case tidFE:             /* fall through */
        case tidEFE:
            /* check file type for VAT1 or VAT2.
             * note that pFE_icbTag() macro is endian swap independant
             * and fileType is Uint8, so no need for endian swap.
             */
            fileType = (pFE_icbTag(bf))->fileType;
            if( fileType == FT_UNKNOWN_OR_VAT150 )  /* UDF 1.50 */
            {   wiVAT1cnt++;
                VERBOSE00(uctout, "%7lu\t%s VAT UDF 1.50 (%ld)\n",
                            blockNr, tidTEXT4(tagId),
                            (Int32)(blockNr - tagLocation));
                break;      /* switch */
            }
            else if( fileType == FT_VAT200 )        /* UDF 2.xx */
            {   wiVAT2cnt++;
                VERBOSE00(uctout, "%7lu\t%s VAT UDF 2.xx (%ld)\n",
                            blockNr, tidTEXT4(tagId),
                            (Int32)(blockNr - tagLocation));
                break;      /* switch */
            }
            /* else fall through for other (E)FEs !!
             */
        default:                    /* fall through */
          { Uint32 mainVDSloc = 0, resVDSloc = 0;
            if( tagId == tidAVDP )
            {   AnchorVolumeDescriptorPointer *avdp;
                wiAVDPcnt++;
                vLevel = VERBOSE00level;
                avdp = (AnchorVolumeDescriptorPointer*) bf;
                mainVDSloc = avdp->mainVolumeDescriptorSequenceExtent.extentLocation;
                resVDSloc = avdp->reserveVolumeDescriptorSequenceExtent.extentLocation;
                endianSwap((Byte*) &mainVDSloc, sizeof(Uint32), 1, NULL);
                endianSwap((Byte*) &resVDSloc, sizeof(Uint32), 1, NULL);
            }
            else
            {   vLevel = INFO01level;
            }
            ifVERBOSE( vLevel )
            {   fprintf(uctout, "%7lu\t%-4s (%ld)",
                        blockNr, tidTEXT4(tagId),
                        (Int32)(blockNr - tagLocation));
                if( tagId == tidAVDP )
                { fprintf(uctout, "\t(MVDS: %lu, RVDS: %lu)",
                            mainVDSloc, resVDSloc);
                }
                fprintf(uctout, "\n");
            }
            ENDif;
          }
          break;            /* switch */
        }
    }
}   /* end inspectNonBlankBlock() */

/* Inspect blocks in buffer.
 * If !inspectOnly, write blocks out.
 */
static void inspectAndWriteImageBlocks( Byte   *buffer,
                                        Uint32  blockNr,
                                        Uint32  nmbBlocks,
                                        Uint32  blockSize,
                                        bool    inspectOnly,
                                        FILE  **pOutHandle )
{
    Uint32   n;
    Byte    *bf;

    /* so from now, within inspectAndWriteImageBlocks(),
     * we have to bother about WI_OK_BLOCKS only.
     *
     * inspect read blocks before write,
     * look for blank blocks, AVDP, VAT1 and VAT2 descriptors
     * make it fast by looking for (possible endian swapped)
     * tag identifiers before calling inspectDescriptorHead().
     */
    for( n = 0, bf = buffer;
         n < nmbBlocks;
         n++, bf += blockSize )
    {
        /* first check for blank block
         */
        if( verifyZeros(bf, blockSize, NULL, NULL, NULL) )
        {   /* blank block found, count at read time.
             * further handle as WI_OK_BLOCKS
             */
            wiBlankBlocks++;
        }
        else
        {   /* inspect non-blank block
             */
            inspectNonBlankBlock(bf, blockNr + n, blockSize);
        }
    }   /* endfor */

    if( !inspectOnly )
    {   writeImageBlocks(buffer, blockNr, nmbBlocks, blockSize,
                         WI_OK_BLOCKS, pOutHandle);
    }
}   /* end inspectAndWriteImageBlocks() */

/* skipUnrecordedGap()
 * Calculate how many unrecorded gap blocks can be skipped
 * starting with firstBlock.
 */
static Uint32 skipUnrecordedGap( Uint32      firstBlock,
                                 Uint32      lastBlock,
                                 const MediumInfo *mi )
{
    Uint32 nmb, firstRec;

    if(   uctDoReadGap                  /* -readgap option */
       || firstBlock > lastBlock )      /* empty range */
    { return 0;                         /* skip none */
    }
    nmb = 1 + lastBlock - firstBlock;   /* nmb > 0 */

    if( gapFindFirstRecordedBlock( mi,  firstBlock,
                                  nmb, &firstRec ) )
    { return firstRec - firstBlock;     /* maybe skip blocks */
    }

    /* no recorded block found, complete overlap with unrecorded gap
     */
    return nmb;     /* skip all */

}   /* end skipUnrecordedGap() */

/* Inspect and write image from startBlockNr until lastValidBlockNr included.
 * If (outPath == NULL), then inspect only.
 * Blocks that cause an error when reading (unrecorded blocks,
 * etc.) are written in the same way as blank blocks, but are ignored
 * at the end of the image file.
 * The image file is written in chunks.
 * Chunk size and nmb of chunks is determined by:
 *   if image size is less than 2 Gbyte then there will only be one chunk,
 *   else chunks less than 2 Gbyte each of (about) equal size.
 */
#define INSPECT_ONLY ((bool)(outPath==NULL))    /* no write image */

extern void inspectAndWriteImage(Device *readDevice, char *outPath,
                                 Uint32  startBlockNr)
{
    const MediumInfo *vmi   = getTheMediumInfo(); /* verifier MediumInfo */
    Uint32  blockSize       = vmi->blockSize;
    Uint32  lastValidBlockNr = vmi->lastValidBlockNr;
    Uint32  blockNr, readBlocks, okBlocks, totalBlocks;
    Uint32  n, bufSize, buffBlocks;
    Byte   *readbuffer;
    FILE   *outHandle;          /* modified for each chunk */
    bool    firstErrorFound = FALSE;
    Uint32  unwrittenErrorBlocks = 0;

    /* It is not sure that deviceReadBlock() reads from a scsi device,
     * but extensive scsi error logging is disabled for read error
     * blocks for the case it will read from scsi.
     */
#ifdef  SCSI_DEVICE
    FILE *scsiErrorOutSave = scsiErrorOut;
    scsiErrorOut = NULL;    /* suppress scsi read error messages */
#endif

    wiStartBlockNr = startBlockNr;      /* global */
    wiBlankBlocks = wiErrorBlocks = 0;
    wiText = (INSPECT_ONLY) ? "Inspect image"
                            : "Write image";
    VERBOSE00(uctout, "\n%s start, address range: %lu -> %lu\n",
                        wiText, startBlockNr, lastValidBlockNr);
    if( startBlockNr > lastValidBlockNr )
    {   VERBOSE00(uctout,"%s: Address range error\n", wiText);
        uctExit(EXIT_PROGRAM_ERROR);    /* quit */
    }
    totalBlocks = 1 + lastValidBlockNr - startBlockNr;

    if( INSPECT_ONLY )
    {   outHandle = NULL;
        wiBlockWithBlanks = NULL;
    }
    else    /* write image, open write handle and create blank buffer */
    {   outHandle = writeImageOpenFirstChunk(outPath, totalBlocks,
                                             blockSize);
        if( (wiBlockWithBlanks = (Byte*) tst_calloc(blockSize, 1,
                                         __FILE__,__LINE__)) == NULL )
        {   uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
        }
    }
    buffBlocks = ROUNDUPELEMENTS(WI_MAX_READSIZE, blockSize);
    bufSize = blockSize * MIN(totalBlocks, buffBlocks);
    if( (readbuffer = allocBlockBuffer(bufSize, &bufSize)) == NULL )
    {   VERBOSE00(uctout, "%s: Out of memory\n", wiText);
        uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    /* MIND: bufSize, may have been changed by allocBlockBuffer(),
     *       but it will remain a multiple of the blockSize.
     * Determine max blocks per read
     */
    buffBlocks = bufSize / blockSize;

    for( blockNr = startBlockNr;
         blockNr <= lastValidBlockNr;
         blockNr += okBlocks )
    {
        /* Determine number of blocks to read
         * For full chunk read:
         *  resync/align on buffBlocks boundaries
         */
        readBlocks = MIN(buffBlocks,
                         1 + lastValidBlockNr - blockNr);

        if(   readBlocks == buffBlocks      /* full chunk */
           && readBlocks <  totalBlocks )   /*    read    */
        {   readBlocks -= (blockNr % buffBlocks);
        }
        okBlocks = deviceReadBlock(readDevice, blockNr,
                                   readBlocks, readbuffer,
                                   TRUE );  /* allowBelowVRS */

        /* first handle blocks that are read ok
         */
        if( okBlocks > 0 )
        {   /* first okBlocks blocks ok, inspect and write them
             * but write blank error blocks first, if any
             */
            if( unwrittenErrorBlocks )
            {   if( !INSPECT_ONLY )         /* write image */
                {   writeImageBlocks(NULL, blockNr - unwrittenErrorBlocks,
                                     unwrittenErrorBlocks, blockSize,
                                     WI_ERROR_BLOCKS, &outHandle);
                }
                wiErrorBlocks += unwrittenErrorBlocks;  /* total */
                unwrittenErrorBlocks = 0;
            }
            inspectAndWriteImageBlocks(readbuffer, blockNr, okBlocks,
                                       blockSize, INSPECT_ONLY, &outHandle);
        }

        /* now handle error blocks
         */
        if( okBlocks != readBlocks )        /* error block found */
        {   blockNr += okBlocks;        /* blockNr is the error block */
            okBlocks = 0;
            VERBOSE00(uctout, "%7lu\t", blockNr );
            if( !firstErrorFound )      /* show read error log once */
            {   firstErrorFound = TRUE;
                VERBOSE00(uctout, "First read error block" );
                if( !INSPECT_ONLY )     /* write image */
                {   VERBOSE00(uctout,
                      "\n-\tA blank block is written for each read error block.\n"
                        "-\tTrailing error blocks are not written. Continue reading\n"
                        "-\tsingle blocks until a correct block is read again.\n");
                }
            }
            /* Log the error block and read blocks in single block mode
             * using deviceReadBlockRaw() till a correct block is read.
             * Mind the special case that blockNr == lastValidBlockNr.
             * and the fact that blockNr has been read already.
             * Show progress in case of error blocks by (initially)
             * printing a dot for each error block, but avoid too much
             * output in case of huge unrecorded gaps
             * (or read them at once, requires unrecorded gap detection),
             * for more detail see wiShowprogress().
             */
            do
            {   unwrittenErrorBlocks++; /* also for INSPECT_ONLY */

#define USE_WI_SHOWPROGRESS_FUNCTION    /* please #define to avoid huge logs */
#ifdef  USE_WI_SHOWPROGRESS_FUNCTION

                wiShowProgress(unwrittenErrorBlocks, "err:", INFO01level);

#else       /* obsolete code, can produce huge log files */

                if( (unwrittenErrorBlocks % 50) == 1 )
                { ifPRINTinfo01(uctout, "\n%7lu\t", blockNr);
                  ENDif;
                }
                ifPRINTinfo01(uctout, "."); /* show progress */
                ENDif;
                fflush(uctout);

#endif  /* USE_WI_SHOWPROGRESS_FUNCTION */

                blockNr++;          /* check if more eror blocks */

                /* if not -readgap, then skip unrecorded gaps
                 */
                n = skipUnrecordedGap(blockNr, lastValidBlockNr, vmi);
                if( n > 0 )
                {   VERBOSE00(uctout,
                      "\n%7lu\tunrecorded gap, skipping %lu read error blocks\n",
                                    blockNr, n);
                    unwrittenErrorBlocks += n;  /* also for INSPECT_ONLY */
                    blockNr += n;   /* skip unrecorded blocks */
                }

                /* try next block in single block read mode
                 */
                if( blockNr <= lastValidBlockNr )
                {
                    okBlocks = deviceReadBlockRaw(readDevice, blockNr,
                                                1, readbuffer);
                }
            } while(   okBlocks == 0
                    && blockNr <= lastValidBlockNr );

            /* A correct block was read at blockNr
             * or (blockNr > lastValidBlockNr) is TRUE.
             * In both cases, the block is flagged as not read
             * by:
             */
            okBlocks = 0; /* do not increment blockNr starting next loop */

            /* repeat last block read in normal mode using deviceReadBlock()
             * (and its default logging)
             */
            ifVERBOSE(INFO01level)
              printReadErrorBlocks(unwrittenErrorBlocks, 0,
                               blockNr - unwrittenErrorBlocks);
            ENDif;
        }
        fflush(uctout);     /* try to get this printed */
    }       /* endfor blockNr ... */

    UCTASSERT( blockNr == lastValidBlockNr + 1 );
    wiErrorBlocks += unwrittenErrorBlocks;  /* total read error blocks */

    /* summary for write or inspect,
     * there may be unwritten error blocks
     */
    VERBOSE00(uctout,
               "\n====>\t%s summary\n"
                      "\tblock size\t\t: %8lu\n"
                      "\tfirst block read\t: %8lu\n",
                        wiText, blockSize, startBlockNr);

    if( startBlockNr + wiErrorBlocks <= lastValidBlockNr )
    {   VERBOSE00(uctout, "\tlast valid block read\t: %8lu\n",
                        lastValidBlockNr - unwrittenErrorBlocks);
    }
    VERBOSE00(uctout, "\ttotal blocks read\t: %8lu\n"
                      "\tblank blocks read\t: %8lu\n"
                      "\terror blocks read\t: %8lu\n"
                  "\ttrailing error blocks\t: %8lu\n"
                              "\tAVDPs\t\t\t: %8lu\n",
                        blockNr - startBlockNr,
                        wiBlankBlocks, wiErrorBlocks,
                        unwrittenErrorBlocks, wiAVDPcnt);

    if( wiVAT1cnt )
        VERBOSE00(uctout, "\tFEs VAT UDF 1.50\t: %8lu\n",
                            wiVAT1cnt);
    if( wiVAT2cnt )
        VERBOSE00(uctout, "\tFEs VAT UDF 2.xx\t: %8lu\n",
                            wiVAT2cnt);
    if( wiVAT1cnt + wiVAT2cnt == 0 )
        VERBOSE00(uctout, "\tno VAT File Entries\n");

    if( !INSPECT_ONLY )     /* write image */
    {
        Uint32 wrTotal, wrBlanks;
        wrTotal = blockNr - startBlockNr - unwrittenErrorBlocks;
        wrBlanks = wiBlankBlocks + wiErrorBlocks - unwrittenErrorBlocks;

        VERBOSE00(uctout, "\n\ttotal blocks written\t: %8lu\n"
                            "\tblank blocks written\t: %8lu"
                                " (%lu %%%s)\n"
                         "\tmax output chunk blocks\t: %8lu\n"
                         "\t%lu output image file%s\t: %s%s%s\n"
          "\n\tImage configuration file may be created using\n"
            "\tmedium information shown above in this log and\n"
            "\tinformation in additional verification output.\n",
          wrTotal, wrBlanks,
          (wrTotal == 0) ? 0  : (100 * wrBlanks) / wrTotal,
         ((wiErrorBlocks - unwrittenErrorBlocks) == 0)
                         ? "" : ", error blocks included",
            wiBlocksPerChunk, wiNmbOfWriteChunks,
            PLURAL_S(wiNmbOfWriteChunks), wiPathBase,
                    (wiNmbOfWriteChunks == 1)  ? "" :
                    (wiNmbOfWriteChunks <= 26) ? "_<x>"
                                               : "_<nn>",
            wiPathExt);
    }

    if( startBlockNr + wiErrorBlocks + wiBlankBlocks > lastValidBlockNr )
    {
        VERBOSE00(uctout,
            "\nNo valid blocks read at all or only blank blocks !!\n");
    }
    VERBOSE00(uctout, "\n");

    if( !INSPECT_ONLY )
    {   fclose(outHandle);  /* close last chunk */
        free(wiPathBase);
        free(wiPathExt);
        free(wiWritePath);
        free(wiBlockWithBlanks);
    }
    free(readbuffer);

#ifdef  SCSI_DEVICE
    scsiErrorOut = scsiErrorOutSave; /* restore scsi error logging */
#endif

}   /* end inspectAndWriteImage() */

