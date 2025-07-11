/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : write_image.h
 *
 * Description :
 *
 * Author(s)   : Gerrit Scholl
 */


#ifndef __WRITE_IMAGE_H__
#define __WRITE_IMAGE_H__

#include "mfile.h"

/* Write image max file output chunks definitions.
 * Note that this can be reduced using -chunksize option.
 */
#define WI_MAX_CHUNK_SIZE   MFILE_MAX_CHUNK_SIZE

/* Global Int64 uctWiMaxChunksize, default WI_MAX_CHUNK_SIZE.
 * Value may be reduced if -chunksize option is defined.
 */
extern Int64 uctWiMaxChunksize;

/* wiShowProgress():
 * Show progress in case of read error blocks by (initially)
 * printing a dot for each error block, but avoid too much
 * output in case of huge unrecorded gaps
 * (or read them at once, requires unrecorded gap detection),
 * for more detail see wiShowProgress() in write_image.c.
 */
extern void wiShowProgress( Uint32 count, char *txt4,
                            Uint8 vLevel );

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
extern void inspectAndWriteImage(Device *readDevice, char *outPath,
                                 Uint32  startBlockNr);

#endif /* __WRITE_IMAGE_H__ */

