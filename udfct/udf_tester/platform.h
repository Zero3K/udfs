/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : platform.h
 *
 * Description : platform specific functions
 *
 * Author(s)   : Gerrit Scholl
 */

#ifndef __PLATFORM_H__
#define __PLATFORM_H__

#include <stdio.h>
#include "mytypes.h"


/* Define platform dependent identification id (text)
 */
#if     defined WIN32
#define UCT_PLATFORM_ID "Windows"
#elif   defined LINUX
#define UCT_PLATFORM_ID "Linux"
#elif   defined SOLARIS
#define UCT_PLATFORM_ID "Solaris"
#elif   defined HPUX
#define UCT_PLATFORM_ID "HP/UX"
#elif   defined IRIX64
#define UCT_PLATFORM_ID "IRIX64"
#elif   defined NETBSD
#define UCT_PLATFORM_ID "NetBSD"
#elif   defined MACOSX
#define UCT_PLATFORM_ID "Macintosh OS X"
#else
#define UCT_PLATFORM_ID "unknown"
#endif

/*********************************************************
 ** Platform dependent device support:
 *
 * SCSI_DEVICE:
 *    '-showscsi' and '-scsi <device id>' options.
 *    Preferred way of device support. Independent of proper
 *    mounting of the UDF file system by the OS.
 * ...Extensive logging of MMC command results.
 *
 * #include "scsi_device.h"      // takes care of SCSI_DEVICE support
 *
 * DRIVE_DEVICE:
 *    '-drive <device id>' option Drive letter device support
 *    Additional way of device support for Win32. Proper
 *    mounting of the UDF file system by the OS is required
 *    for proper functioning of DRIVE_DEVICE support.
 *
 * #include "drive_device.h"    // takes care of DRIVE_DEVICE support
 *
 * RAW_DEVICE:
 *    '-raw <device id>' option Raw device support for
 *    Macintosh OS X. Proper mounting of the UDF file system
 *    by the OS is required for proper functioning of
 *    RAW_DEVICE support.
 *
 * #include "raw_device.h"      // takes care of RAW_DEVICE support
 *
 *******************/


/*********************************************************
 ** 'image' device support:
 * Handle files with a size of at most PLATFORM_MAX_FILE_SIZE
 * bytes (defined below).
 * In this module, platform dependent implementations may
 * be defined to cope with Int64 file size and file offset
 * (file size of 2 Gbyte and higher).
 * If no such implementation is defined, the standard ANSI C
 * solution with fseek/fread is used. This implementation can
 * only deal with Int32 file (chunk) sizes lower than 2 Gbyte.
 */
#define IMAGE_OPEN_MODE "rb"

/* WIN32_64: default 64 bit offset support for WIN32
 * (WIN32_64 can be disabled when testing the ANSI C
 *  'smaller than 2 Gbyte' solution)
 */
#ifdef  WIN32
#ifndef WIN32_64
#define WIN32_64    /* WIN32 64 bit file offset */
#endif
#endif

/* PL_SIZE64: platforms supporting 64 bit file size and offset
 * (PL_SIZE64 is used below and in platform.c)
 */
#if    defined WIN32_64 \
    || defined LINUX    \
    || defined SOLARIS  \
    || defined HPUX     \
    || defined IRIX64   \
    || defined NETBSD   \
    || defined MACOSX
#define PL_SIZE64       /* 64 bit file size and offset */
#endif

/* PLATFORM_MAX_FILE_SIZE:
 * For implementations that can handle the bigger Int64 file
 * sizes, the file size limitation is that the number of
 * sectors must fit in a Uint32 for the minimal sector size
 * of 512 bytes. This max file size is nearly 2 Terabyte.
 */
#ifdef PL_SIZE64        /* 64 bit file size and offset */

#define PLATFORM_MAX_FILE_SIZE  (512 * (Uint64)MAX_UINT32)
/** TODO: make dependent on real blockSize instead of 512 ??
 **       looks like 2 Terabyte chunks is enough for the moment
 **/

#else   /* default ANSI C 'smaller than 2 Gbyte' solution */

#define PLATFORM_MAX_FILE_SIZE  MAX_INT32

#endif

/* plFileOpenWithSize():
 * fopen() file with mode "rb" and determine file size.
 * Put file pointer at begin of file.
 *
 * return value: NULL in case of an error
 *         else: file handle (FILE*).
 *
 * In case NULL is returned, *fileSize holds an error code:
 *      0: file could not be opened, may not be present.
 *      1: file present, could not determine file size.
 *
 * Otherwise the actual file size is returned in *fileSize.
 */
extern FILE *plFileOpenWithSize(char *path, Uint64 *fileSize);

/* plFileClose()
 * platform close File.
 * return value: NULL.
 */
extern FILE *plFileClose(FILE *handle);

/* plFileReadBlock()
 * Reads number of blocks from image chunk file.
 * Returns actual number of blocks read.
 */
extern Uint32 plFileReadBlock(FILE   *handle,
                              Uint32  blockSize,
                              Uint32  firstBlock,
                              Uint32  nrOfBlocks,
                              Byte   *buffer);

#endif /* __PLATFORM_H__ */

