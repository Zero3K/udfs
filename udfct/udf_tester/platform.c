/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : platform.c
 *
 * Description : platform specific code
 *
 * Author(s)   : Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>

#include "platform.h"       /* may define PL_SIZE64 */

#ifdef  PL_SIZE64           /* 64 bit file size and offset */
#include <sys/types.h>      /* several platforms */
#ifndef WIN32
#include <unistd.h>         /* several platforms, except WIN32 */
#endif
#endif

/* further platform dependent #include statements:
 */
#if   defined WIN32_64
#include <io.h>             /* WIN32 64 bit file offset */

#elif defined IRIX64        /* SGI IRIX 64 bit file offset */

#include <sys/stat.h>   /* stat() */
#include <fcntl.h>
#include <sys/dvh.h>    /* findsize() & getdiskheader() */
/* To build a binary for SGI IRIX64,
 * /lib/libdisk.so must exist with:
 */
__int64_t findsize(char *path);
char* findrawpath(char *path);
int getdiskheader(int fd, struct volume_header *vhdr);

#endif      /* WIN32_64, IRIX64 */

#include "uct_core.h"

/** end of #include ... **/

#ifdef  PL_SIZE64       /* 64 bit file size and offset */

/* In order to deal with Int64 file size and offset the following
 * macro functions are defined:
 *  PL_GET_SIZE64(FH)           // get Int64 file size
 *  PL_LSEEK64_SET(FH,OFF64)    // seek to Int64 offset
 *  PL_READ64(FH,BF,NB)         // read Int32 number of bytes
 *
 * All these macros use a (FILE*) fileHandle argument FH.
 * Note that for many platforms, a combination of an lseek()
 * variant and fread() fails and shall not be used
 * (e.g. WIN32_64, LINUX, IRIX64, etc.).
 */

#ifdef       WIN32_64   /* WIN32 64 bit file offset */
/* Use _fileno(), _lseeki64() and _read().
 * A combination of _lseeki64() and fread() will fail !!!!!!!!
 */
#undef  _fileno         /* do not use macro version */
#undef  _lseeki64       /* do not use macro version */
#undef  _read           /* do not use macro version */

#define PL_GET_SIZE64(FH)        (Int64)_lseeki64(_fileno(FH),0,SEEK_END)
#define PL_LSEEK64_SET(FH,OFF64) (Int64)_lseeki64(_fileno(FH),(OFF64), SEEK_SET)
#define PL_READ64(FH,BF,NB)      (Int32)_read(_fileno(FH),(BF),(NB))

#elif defined IRIX64     /* IRIX 64 bit */
/* IRIX64: Use fileno(), lseek64() and read().
 * A combination of lseek64() and fread() will fail !!!!!!!!
 */
#undef  fileno          /* do not use macro version */
#undef  lseek64         /* do not use macro version */
#undef  read            /* do not use macro version */

/* exception: for IRIX64, PL_GET_SIZE64() takes a (char*) path argument
 * instead of a (FILE*) file handle
 */
#define PL_GET_SIZE64(PATH)      (Int64)irix64_getFileSize(PATH)
#define PL_LSEEK64_SET(FH,OFF64) (Int64)lseek64(fileno(FH),(OFF64), SEEK_SET)
#define PL_READ64(FH,BF,NB)      (Int32)read(fileno(FH),(BF),(NB))

#else       /* most common 64 bit size and offset solution */

/* Common 64 bit size and offset solution for:
 * LINUX, SOLARIS, HPUX, NETBSD.
 * compiler/linker options, see Makefile.uct:
 * (not needed for NETBSD, MACOSX ??)
 * -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE
 * -D__USE_LARGEFILE64
 *
 * Use fileno(), lseek() and read().
 * A combination of lseek() and fread() will fail !!!!!!!!
 */
#undef  fileno          /* do not use macro version */
#undef  lseek           /* do not use macro version */
#undef  read            /* do not use macro version */

#define PL_GET_SIZE64(FH)        (Int64)lseek(fileno(FH),0,SEEK_END)
#define PL_LSEEK64_SET(FH,OFF64) (Int64)lseek(fileno(FH),(OFF64),SEEK_SET)
#define PL_READ64(FH,BF,NB)      (Int32)read(fileno(FH),(BF),(NB))

#endif  /* platforms */

#endif  /* PL_SIZE64 */

/*********************************************************
 * Handle files with a size of at most
 * PLATFORM_MAX_FILE_SIZE bytes.
 * If there is no standard ANSI C solution for any of
 * the access functions defined in this module, platform
 * dependent implementations may be defined in this module.
 * At least a platform independent solution is defined here
 * that can handle files smaller than 2 Gbyte.
 */

#undef  PLATFORM_READ_TESTING       /* normally #undef */

/* plFileSeekBlock()
 * Seek to relative block position in file
 * Returns TRUE for success, else FALSE.
 *
 * Implementation note:
 *  Seeking to the begin of the first block is done
 *  from the beginning of the file in steps of integral
 *  blocks of at most MAX_INT32 bytes, using fseek().
 *  This means that there seems not to be a real
 *  limitation for the maximum file size.
 */
static bool plFileSeekBlock(FILE   *fileHandle,
                            Uint32  blockSize,
                            Uint32  firstBlock)
{
    Int64 byteOffset, resultOffset;

    UCTASSERT( blockSize != 0 && blockSize <= MAX_INT32 );

    byteOffset = (Int64)((Uint64)firstBlock * (Uint64)blockSize);
    UCTASSERT( byteOffset >= 0 );

    resultOffset = (Int64) -1;      /* error */

    /* seek to byteOffset, result in resultOffset
     */
#ifdef PL_LSEEK64_SET       /* 64 bit file size and offset */

    resultOffset = PL_LSEEK64_SET(fileHandle, byteOffset);

#else       /* default ANSI C 'smaller than 2 Gbyte' solution */
  { Int32 maxSeek, offset32;

    /* Maybe multiple fseek() in steps of integral blocks of
     * at most MAX_INT32 bytes. One 'SEEK_SET' seek, maybe
     * followed by more 'SEEK_CUR' seeks.
     * Calculate max seek per step and offset for first seek.
     */
    maxSeek = (Int32) (blockSize * (Int32)(MAX_INT32 / blockSize));
    offset32 = (Int32) MIN(byteOffset, (Int64) maxSeek);
    UCTASSERT( offset32 >= 0 && maxSeek >= 0 );

    if( fseek(fileHandle, offset32, SEEK_SET) != 0 )
    {   resultOffset = (Int64) -1;      /* step 1 fseek error */
    }
    else
    { for( resultOffset  = (Uint64) offset32;   /* step 1 */
           resultOffset != byteOffset;
           resultOffset += (Uint64) offset32 )  /* step 2, etc */
      { offset32 = (Int32) MIN((Uint64)(byteOffset - resultOffset),
                               (Uint64) maxSeek);
        if( fseek(fileHandle, offset32, SEEK_CUR) != 0 )
        {   resultOffset = (Int64) -1;  /* step 2, etc. fseek error */
            break;
        }
      }
    }
  }
#endif  /* end of platform dependent seek code */

#ifdef PLATFORM_READ_TESTING
    fprintf(uctout, "resultOffset:\t %lu\t ", (Uint32)(resultOffset/blockSize));
    printUint64(VERBOSE00level, (Uint64) resultOffset, FALSE, NULL);
    fprintf(uctout, "\n");
    fflush(uctout);
#endif

    /* seek result in resultOffset
     */
    if( resultOffset != byteOffset )
    { VERBOSE00(uctout,
        "\tfileSeekBlock error: seeking for byte offset ");
      printUint64(VERBOSE00level, (Uint64) byteOffset, FALSE, NULL);
      VERBOSE00(uctout, "\n");
      return FALSE;
    }
    return TRUE;        /* seek ok */

}       /* end plFileSeekBlock() */

#ifdef  IRIX64      /* SGI IRIX 64 bit file size */
                    /* uses path instead of fileHandle */

/* irix64_getFileSize()
 * return value: Int64 value
 *  if value < 0
 *  then: file size could not be determined
 *  else: return value is file size.
 */
static Int64 irix64_getFileSize(char *path)
{   int fd = -1;
    Int64 size64 = (Int64) -1;      /* size or seek error */
    Int64 capacity64;
    struct stat64 stat_buf;
    struct volume_header vhdr;
    char *p_dev_rawpath = NULL;

    if( stat64(path, &stat_buf) != 0 )  /* stat64() failed */
    {   return (Int64) -1;      /* could not determine size */
    }

    /* If it's a character or block device */
    if(   S_ISBLK(stat_buf.st_mode)
       || S_ISCHR(stat_buf.st_mode) )
    {   memset(&vhdr, 0, sizeof(vhdr));

        /* findsize() returns the number of blocks */
        capacity64 = findsize((char *)path);
        if( capacity64 < 0 )
        {   return (Int64) -1;      /* could not determine size */
        }

        /* NEED THE RAW DEVICE PATH */
        p_dev_rawpath = findrawpath((char *)path);
        if( p_dev_rawpath == NULL )
        {   return (Int64) -1;      /* could not determine size */
        }
        fd = open(p_dev_rawpath, O_RDONLY);
        if( fd < 0 )
        {   if( p_dev_rawpath != path )
            {   free(p_dev_rawpath);
            }
            return (Int64) -1;      /* could not determine size */
        }

        /* findrawpath() mallocs the memory if p_dev_path was NOT a raw
         * device. However, if p_dev_path is a raw device then findrawpath()
         * returns the pointer of the argument, p_dev_path, instead of
         * malloced memory.
         */
        if( p_dev_rawpath != path )
        {   free(p_dev_rawpath);
        }

        if( getdiskheader(fd, &vhdr) )
        {   return (Int64) -1;      /* could not determine size */
        }
        /* Calculate nmb of bytes, no rounding to mult of block size
         */
        size64 = (Int64) (capacity64 * vhdr.vh_dp.dp_secbytes);
        close(fd);
    }
    else if( S_ISREG(stat_buf.st_mode) )
    {   /* if stat64() is not run on a block or character device */
        size64 = stat_buf.st_size;
    }
    else
    {   return (Int64) -1;      /* could not determine size */
    }
    return size64;

}   /* end irix64_getFileSize() */

#endif  /** IRIX64 **/

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
extern FILE *plFileOpenWithSize(char *path, Uint64 *fileSize)
{
    FILE    *fileHandle;
    Int64    size64 = -1;       /* size not determined yet */

    /* Always open with fopen() (ANSI C compatible function)
     */
    if( (fileHandle = fopen(path, IMAGE_OPEN_MODE)) == NULL )
    {   *fileSize = (Uint64) 0;     /* could not open, maybe */
        return NULL;                /*  file does not exist. */
    }

    /* determine the file size.
     */
#ifdef PL_GET_SIZE64        /* 64 bit file size and offset */

#ifdef IRIX64       /* IRIX64 exception to get file size of raw device */
    /* platform is also capable to get size (capacity) of raw block device
     */
    size64 = PL_GET_SIZE64(path);       /* IRIX64 path argument !! */
#else                                       /* other platforms */
    size64 = PL_GET_SIZE64(fileHandle);     /* fileHandle argument */
#endif  /** IRIX64, else **/

#else   /* default ANSI C 'smaller than 2 Gbyte' solution */

    /* This implementation can only handle chunk files
     * smaller than 2 Gbyte.
     */
    if(   fseek(fileHandle, 0, SEEK_END) != 0
       || (size64 = (Int64) ftell(fileHandle)) < 0 )
    {   size64 = -1;        /* fseek() or ftell() error */
    }
#endif      /* end of platform specific code */

#ifdef PLATFORM_READ_TESTING
    fprintf(uctout, "size64:\t %lu\t ", (Uint32)(size64/2048));
    printUint64(VERBOSE00level, (Uint64) size64, FALSE, NULL);
    fprintf(uctout, "\n");
    fflush(uctout);
#endif

    /* File size is in size64. First do platform independent
     * tests, then seek to begin of file
     * (blockSize for plFileSeekBlock() is don't care, take 2048).
     */
    if(    size64 < 0                   /* size or seek error */
       ||  size64 > PLATFORM_MAX_FILE_SIZE
       || !plFileSeekBlock(fileHandle, 2048, 0) )  /* seek to begin */
    {   *fileSize = (Uint64) 1;         /* means size or seek error */
        plFileClose(fileHandle);
        return NULL;                    /* file closed */
    }
    *fileSize = (Uint64) size64;        /* ok, correct size found */
    return fileHandle;

}   /* end plFileOpenWithSize() */

/* plFileClose()
 * platform close File.
 * return value: NULL.
 */
extern FILE *plFileClose(FILE *handle)
{   if( handle != NULL )
    { fclose(handle);
    }
    return NULL;    /* closed */
}

/* plFileReadBlock()
 * Reads number of blocks from image chunk file.
 * Returns actual number of blocks read.
 *
 * Implementation note:
 *  For seek details see plFileSeekBlock().
 */
extern Uint32 plFileReadBlock(FILE   *fileHandle,
                              Uint32  blockSize,
                              Uint32  firstBlock,
                              Uint32  nrOfBlocks,
                              Byte   *buffer)
{
    Uint32 blocksRead;

    UCTASSERT( blockSize != 0 && blockSize <= MAX_INT32 );

    if( nrOfBlocks == 0 )
    {   return 0;           /* nothing to read */
    }

    /* read blocks, result in blocksRead
     */
    blocksRead = 0;         /* nothing read */
    if( !plFileSeekBlock(fileHandle, blockSize, firstBlock) )
    {   /* seek error message printed in plFileSeekBlock(). No attempt
         * to read, but also print plFileReadBlock() error below.
         */
    }
    else    /* do actual read */
    {
#ifdef  PL_READ64   /* 64 bit file size and offset */
      { Int32 nBytes = PL_READ64(fileHandle, buffer, blockSize * nrOfBlocks);
        blocksRead = (nBytes <= 0) ? 0 : nBytes / blockSize;    /* round down */
      }
#else               /* ANSI C 'smaller than 2 Gbyte' solution */
      blocksRead = fread(buffer, blockSize, nrOfBlocks, fileHandle);
#endif      /* end of platform dependent read */
    }

#ifdef PLATFORM_READ_TESTING
    fprintf(uctout, "plFileReadBlock:\t %lu\t %lu\n", firstBlock, blocksRead);
    fflush(uctout);
#endif

    /* read done, actual nmb of blocks read in blocksRead
     */
    if( blocksRead != nrOfBlocks )
    { VERBOSE00(uctout,
        "\tfileReadBlock error: blocks read: %lu, requested: %lu\n",
                blocksRead, nrOfBlocks);
    }
    return blocksRead;

}   /* end plFileReadBlock() */

