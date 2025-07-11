/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : mfile.h
 *
 * Description : multi-file abstraction definitions.
 *      Conceptually, a multi-file is an abstraction of a file that
 *      is in fact the concatenation of a number of separate files.
 *      Usually this is done to circumvent maximum file size as well
 *      as maximum partition size limitations.
 *
 * Author(s)   : Gerrit Scholl
 */

#ifndef __MFILE_H__
#define __MFILE_H__

#include "platform.h"

/* Multi-file definitions *********************************************
 * A multi-file (mfile) is the abstraction of the concatenation of
 * a number of separate files, handled as one big file.
 * The separate files are called the chunks of the mfile.
 * Each chunk has a size not equal to zero and at most equal to
 * MFILE_MAX_CHUNK_SIZE bytes and it is an integral multiple
 * of the blockSize.
 * MFILE_MAX_CHUNK_SIZE must be chosen such that a chunk of this
 * size can be handled by the platform dependent file access
 * functions defined in platform.c and platform.h.
 * In order to verify chunk sizes, the final value of the block
 * size must be known before opening the first mfile chunk.
 * Because blockOffsetNextChunk[n] in the MFile structure is
 * represented as a Uint32, and the fact that the minimum block
 * size is 512, the maximum mfile size can be at least
 * ((2^32 - 1) * 512) bytes, which is: 2048 Gbyte - 512 bytes.
 * (see also platform.h)
 */
#define MFILE_MAX_CHUNK_SIZE  PLATFORM_MAX_FILE_SIZE

/* dummy chunk definitions for -dummysession option
 */
#define MFILE_DUMMY_CHUNK_HANDLE    ((FILE *) NULL)
#define MFILE_DUMMY_CHUNK_PATH      ((char *) NULL)

/* The MFile structure will hold the information needed to
 * implement the mfile abstraction.
 * Creating an empty consistent Mfile structure must set
 * the initial values: { 0, NULL, NULL, NULL }
 */
typedef struct
{
  Uint32   nrOfChunks;          /* number of chunks in this mfile   */
                                /* arrays: one value for each chunk */
  char   **filePaths;           /* char pointers, chunk path name   */
  FILE   **filePointers;        /* FILE pointers, opened if != NULL */
  Uint32  *blockOffsetNextChunk; /* block offset for next chunk     */
} MFile,  *MFileHandle;

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
                             Uint32 dummyChunkNrOfBlocks);

/* mfileClose():
 * fclose() all chunks of an mfile and adapt the MFIle structure
 * accordingly freeing the dynamically allocated arrays and
 * setting all MFile values to the initial consistent state.
 */
extern void mfileClose(MFileHandle mfileHandle);

/* mfileReadBlock():
 * Reads a number of blocks from an mfile and returns the actual
 * number of blocks read.
 *
 * Precondition:
 *     MFileHandle != NULL
 *  && (nrOfBlocks * blockSize) must fit in a Uint32.
 */
extern Uint32 mfileReadBlock(MFileHandle mfileHandle,
                             Uint32 blockSize,
                             Uint32 firstBlock,
                             Uint32 nrOfBlocks,
                             Byte  *buffer);

/* mfileGetTotalBlocks():
 */
extern Uint32 mfileGetTotalBlocks(MFileHandle mfileHandle);


#endif /* __MFILE_H__ */

