/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : mfile.c
 *
 * Description : multi-file abstraction implementation.
 *      Conceptually, a multi-file is an abstraction of a file that
 *      is in fact the concatenation of a number of separate files.
 *      Usually this is done to circumvent maximum file size as well
 *      as maximum partition size limitations.
 *
 * Author(s)   : Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uct_core.h"
#include "platform.h"   /* platform dependent definitions */
#include "mfile.h"      /* multi-file definitions */


/* Multi-file definitions *********************************************
 * For multi-file concept definition, see mfile.h
 */

/* mfileAppendChunk():
 * Append a chunk to the mfile by fopen() the chunk and updating the
 * MFile structure by appending the chunk at the end of the mfile.
 *
 * If( path == MFILE_DUMMY_CHUNK_PATH ), a dummy chunk
 * of size dummyChunkNrOfBlocks will be installed.
 *
 * return value: FALSE in case of an error, else TRUE.
 *
 * Note: Be aware of possible reallocation of the arrays in the
 *       MFile structure.
 */
extern bool mfileAppendChunk(MFileHandle mfh,
                             char *path,
                             Uint32 blockSize,
                             Uint32 dummyChunkNrOfBlocks)
{
    Uint32  *pBPC;
    FILE   **pFPo;
    char   **pFPa;
    Uint32   nrOfChunks = mfh->nrOfChunks,
             nrOfBlocks;
    Uint64   chunkByteSize;
    bool     isDummyChunk = (path == MFILE_DUMMY_CHUNK_PATH);

    UCTASSERT( blockSize != 0 );

    /* Adapt MFile structure without changing any information of
     * current chunks, but existing arrays may be reallocated.
     * First an array element will be added for the new chunk and
     * and then the nrOfChunks field will be adapted.
     * Now (re)alloc all MFile arrays for new chunk.
     */
    if( (pFPa = (char**) tst_realloc( mfh->filePaths,
                (nrOfChunks+1) * sizeof(char*), __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    mfh->filePaths = pFPa;

    if( (pFPo = (FILE**) tst_realloc( mfh->filePointers,
                (nrOfChunks+1) * sizeof(FILE*), __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    mfh->filePointers = pFPo;

    if( (pBPC = (Uint32*) tst_realloc( mfh->blockOffsetNextChunk,
                (nrOfChunks+1) * sizeof(Uint32), __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    mfh->blockOffsetNextChunk = pBPC;

    /* try to open new chunk
     */
    if( isDummyChunk )
    {   mfh->filePointers[nrOfChunks] = MFILE_DUMMY_CHUNK_HANDLE;
        chunkByteSize = (Uint64) dummyChunkNrOfBlocks * (Uint64) blockSize;
    }
    else if( (mfh->filePointers[nrOfChunks]
             = plFileOpenWithSize(path, &chunkByteSize )) == NULL )
    {   /* Unable to find size, show cause. In messages
         * use "file" instead of "chunk" for first chunk.
         */
        char *txt = (nrOfChunks > 0) ? "chunk" : "file";
        if( chunkByteSize == (Uint64) 0L )
             VERBOSE00(uctout,
                "Error: Cannot open image file for \"%s\", %s: %s\n",
                        IMAGE_OPEN_MODE, txt, path);
        else VERBOSE00(uctout,
                "Error: Cannot find image file %s size for: %s\n",
                        txt, path);
        return FALSE;
    }
    nrOfBlocks = (Uint32) (chunkByteSize / blockSize);

    if(    ((Uint64) nrOfBlocks * (Uint64) blockSize) != chunkByteSize
        ||  nrOfBlocks == 0
        ||  chunkByteSize > (Uint64) MFILE_MAX_CHUNK_SIZE )
    {   Uint64 maxBytes = blockSize * (Uint64)(MFILE_MAX_CHUNK_SIZE/blockSize);
        VERBOSE00(uctout, "  Error: Image file chunk byte size: ");
        printUint64(VERBOSE00level, chunkByteSize, FALSE, NULL);
        VERBOSE00(uctout,
          ",\n-\t expected: not zero, at most ");
        printUint64(VERBOSE00level, maxBytes, FALSE, NULL);
        VERBOSE00(uctout, " (%.10f %sbyte),\n"
             "-\t and an integral multiple of the block size (%lu).\n"
             "-   for: %s\n", nBytesDouble(maxBytes),
                nBytesChar(maxBytes), blockSize,
                isDummyChunk ? "<dummy chunk>" : path);
        if( !isDummyChunk )
        {   mfh->filePointers[nrOfChunks]
                = plFileClose(mfh->filePointers[nrOfChunks]);
        }
        return FALSE;
    }

    /* ok, update rest of MFile record
     */
    if( isDummyChunk )
    {   mfh->filePaths[nrOfChunks] = MFILE_DUMMY_CHUNK_PATH;
    }
    else
    {   if( (mfh->filePaths[nrOfChunks]
            = tst_malloc(1 + strlen(path), __FILE__,__LINE__)) == NULL )
        { uctExit(EXIT_OUT_OF_MEMORY);      /* quit */
        }
        strcpy(mfh->filePaths[nrOfChunks], path);
    }
    mfh->blockOffsetNextChunk[nrOfChunks] = (nrOfChunks == 0)
        ? 0
        : mfh->blockOffsetNextChunk[nrOfChunks-1];
    mfh->blockOffsetNextChunk[nrOfChunks] += nrOfBlocks;
    mfh->nrOfChunks++;
    return TRUE;

}   /* end mfileAppendChunk() */

/* mfileClose():
 * Close all chunks of an mfile and adapt the MFIle structure
 * accordingly freeing the dynamically allocated arrays and
 * setting all MFile values to the initial consistent state.
 */
extern void mfileClose(MFileHandle mfh)
{   Uint32 n;
    if( mfh == NULL ) return;
    for( n = 0; n < mfh->nrOfChunks; n++ )
    {   if( mfh->filePaths[n] != MFILE_DUMMY_CHUNK_PATH )
        {   checkFree((void**)&mfh->filePaths[n]);
        }
        if( mfh->filePointers[n] != MFILE_DUMMY_CHUNK_HANDLE )
        {   (void) plFileClose(mfh->filePointers[n]);
        }
    }
    checkFree((void**)&mfh->filePaths);
    checkFree((void**)&mfh->filePointers);
    checkFree((void**)&mfh->blockOffsetNextChunk);
    mfh->nrOfChunks = 0;
}

/* mfileReadBlock():
 * Reads a number of blocks from an mfile and returns the actual
 * number of blocks read.
 *
 * Precondition:
 *     MFileHandle != NULL
 *  && (nrOfBlocks * blockSize) must fit in a Uint32.
 */
extern Uint32 mfileReadBlock(MFileHandle mfh,
                             Uint32 blockSize,
                             Uint32 firstBlock,
                             Uint32 nrOfBlocks,
                             Byte  *buffer)
{
    Uint32  n, nbl, nblThisChunk, blocksRead,
            offsetThisChunk, offsetNextChunk;

    UCTASSERT(   mfh != NULL
              && ((Uint64) nrOfBlocks * (Uint64) blockSize) <= (Uint64) MAX_UINT32 );

    if( nrOfBlocks == 0 )
    {   return 0;           /* done */
    }

    /* All offsets are block offsets, not byte.
     * Find first chunk to be read
     */
    n = 0;
    while(   n < mfh->nrOfChunks
          && firstBlock >= mfh->blockOffsetNextChunk[n] )
    {   n++;
    }
    if( n >= mfh->nrOfChunks )
    {   return 0;       /* no chunk containing firstBlock */
    }
    /* start read in chunk n  (base 0)
     * read consequtive chunks untill all blocks read.
     */
    offsetThisChunk = (n == 0) ? 0 : mfh->blockOffsetNextChunk[n-1];
    for( blocksRead = 0; blocksRead != nrOfBlocks; n++ )
    {
        offsetNextChunk = mfh->blockOffsetNextChunk[n];
        nblThisChunk = MIN(nrOfBlocks - blocksRead,
                           offsetNextChunk - firstBlock);

        if( mfh->filePointers[n] == MFILE_DUMMY_CHUNK_HANDLE )
        {   nbl = 0;        /* done, nothing read */
        }
        else                /* no dummy chunk */
        { nbl = plFileReadBlock(mfh->filePointers[n], blockSize,
                                firstBlock - offsetThisChunk,
                                nblThisChunk, buffer);
        }
        blocksRead += nbl;
        if( nbl != nblThisChunk ) /* error, plFileReadBlock not complete */
        {   /** TODO: error message with (dummy) chunk path ?? **/
            return blocksRead;      /* error */
        }
        /* prepare for possible next chunk
         */
        firstBlock += nblThisChunk;
        buffer += nblThisChunk * blockSize;
        offsetThisChunk = offsetNextChunk;
    }
    return blocksRead;      /* nmb of blocks actually read */
}

/* mfileGetTotalBlocks():
 */
extern Uint32 mfileGetTotalBlocks(MFileHandle mfh)
{
    return (mfh->nrOfChunks == 0)
        ? 0
        : mfh->blockOffsetNextChunk[mfh->nrOfChunks-1];
}


