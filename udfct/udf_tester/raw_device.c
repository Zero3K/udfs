/* Copyright (c) 2002
 *
 * File        : raw_device.c
 *
 * Description : Raw device support for MAC OS X
 *               Incorporated in UDF verifier by courtesy of Apple Inc.
 *
 * Author(s)   : John Bertagnolli - Apple Inc
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "uct_core.h"
#include "platform.h"
#include "raw_device.h"     /* may #define RAW_DEVICE */

#ifdef RAW_DEVICE

#include <ctype.h>
#include <sys/errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>

#include <IOKit/storage/IOMediaBSDClient.h>
#include <IOKit/storage/IOCDMediaBSDClient.h>

/* One of the above #includes may redefine the bool type
 * Change it back according to uct_core.h
 * (not a very nice solution).
 */
#undef  bool
#define bool int

#include "commandline.h"

typedef struct
{
    int fd;
} RawImpUse;


/* TODO: Image get block state function, only able to detect
 *       unrecorded blocks with extended image format
 */
static BlockState rawGetBlockState( void *impUse, Uint32  blockNr )
{
    impUse  = impUse;       /* make compiler happy */
    blockNr = blockNr;      /* make compiler happy */
    return BSTATE_UNKNOWN;
}

static Uint32 rawReadBlocks(void  *impUse,
                            Uint32 blockSize,
                            Uint32 firstBlock,
                            Uint32 nrOfBlocks,
                            Byte  *buffer )
{
    ssize_t i;
    off_t o;
    RawImpUse *imp = impUse;

    o = lseek(imp->fd, (off_t)firstBlock * blockSize, SEEK_SET);
    if (o == -1)
    {
        fprintf(uctout,"rawReadBlocks failed to seek to block %lu (offset = %d, errno = %d)\n", firstBlock, o, errno);
        return 0;
    }

    i = read(imp->fd, buffer, nrOfBlocks * blockSize);
    if (i != nrOfBlocks * blockSize)
    {
        fprintf(uctout,"rawReadBlocks read %d blocks, not %d blocks\n", i/blockSize, nrOfBlocks);
        return 0;
    }

    return nrOfBlocks;

#ifdef JBERT_UNUSED_CODE
/**
 dk_cd_read_t  readcd;
    RawImpUse    *imp = impUse;

    memset(&readcd, 0, sizeof(dk_cd_read_t));

    readcd.offset = firstBlock;
    readcd.bufferLength = blockSize * nrOfBlocks;
    readcd.buffer = buffer;

    if (ioctl(imp->fd, DKIOCCDREAD, 0) == -1)
    {
        fprintf(uctout,"rawReadBlocks failed to read %lu blocks at block %ul\n", nrOfBlocks, firstBlock);
        return 0;
    }

    return readcd.bufferLength / blockSize;
**/
#endif  /** JBERT_UNUSED_CODE **/

}


/* close image device and free space allocated by image impUse
 */

static void rawCloseAndFreeImpUse(void *impUse)
{
    if( impUse != NULL )
    {   close(((RawImpUse*)impUse)->fd);
        free(impUse);
    }
}


/* Fill Device API function table for image device.
 */

static void rawInitializeFunctionTable(Device *device)
{
    device->getBlockState      = rawGetBlockState;
    device->readBlock          = rawReadBlocks;
    device->closeAndFreeImpUse = rawCloseAndFreeImpUse;
}


static Device *createRawDevice(char       *devname,
                               MediumInfo *clMediumInfo)
{
    Device      *device;
    RawImpUse   *imp;
    u_int64_t   cnt;
    u_int32_t   bs;
    int         err;

    /* Create Device structure.
     * Use tst_calloc() so that all members that might
     * not be initialized are 0 or NULL.
     */
    if(    (device       = (Device*) tst_calloc(sizeof(Device), 1,
                                                __FILE__, __LINE__)) == NULL
        || (device->impUse = (void*) tst_calloc(sizeof(RawImpUse), 1,
                                                __FILE__, __LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);
    }
    clearMediumInfo( &device->mediumInfo );

    /* Image device impUse ok now, install access functions
     * there exists at least one image file chunk
     */
    rawInitializeFunctionTable(device);

    imp = device->impUse;
    imp->fd = open(devname, O_RDONLY, 0);
    if (imp->fd == -1)
    {
        fprintf(uctout, "could not open %s\n",devname);
        goto labelErr;
    }

    // get the device block count
    err = ioctl(imp->fd, DKIOCGETBLOCKCOUNT, &cnt);
    if (err < 0) {
        fprintf(uctout, "could not get block count for device %s\n", devname);
        goto labelErr;
    }

    // get the device block size
    err = ioctl(imp->fd, DKIOCGETBLOCKSIZE, &bs);
    if (err < 0) {
        fprintf(uctout, "could not get block size for device %s\n", devname);
        goto labelErr;
    }
#ifdef  JBERT_OBSOLETE_CODE
    if (bs > 0 && (bs % 512) == 0) {
        if (bs > device->mediumInfo.blockSize)
            cnt *= bs / device->mediumInfo.blockSize;
        else if (bs < device->mediumInfo.blockSize)
            cnt /= device->mediumInfo.blockSize / bs;
    }
#endif  /** JBERT_OBSOLETE_CODE **/

    /** Set device/medium  properties. Because raw_device does not
     ** support ECC blocking factor and multisession these properties
     ** must be defined as command line options.
     **/
    device->mediumInfo.writabilityType     = MTYPE_WR_UNKNOWN;
    device->mediumInfo.sequentialType      = MTYPE_SE_UNKNOWN;
    device->mediumInfo.closedType          = MTYPE_CL_UNKNOWN;
    device->mediumInfo.blockSize           = bs;    /* medium sector size */
    device->mediumInfo.eccLength           = 1;     /* no ECC blocking factor */
    device->mediumInfo.lastValidBlockNr    = (cnt) ? cnt-1 : 0;
    device->mediumInfo.numberOfSessions    = 0;     /* no multisession support */

    /* merge imageImpUse mediumInfo and command line mediumInfo
     * result to imageImpUse mediumInfo.
     */
    if( mergeMediumInfo( clMediumInfo, &device->mediumInfo,
                        &device->mediumInfo,
                        "command line", "raw device") )
    {   fprintf(uctout, "Warning: Overruling of device info may"
                            " cause problems in some cases\n");
    }

    /* set defaults and consistency checks in calling function
     */

    return device;

  labelErr:
    if (imp->fd >= 0)
        close(imp->fd);
    if (device && device->impUse)
        free(device->impUse);
    if (device)
        free(device);

    return NULL;
}

/****** device select function for raw_device support:
 * (-raw) <keyOption> is NOT optional for raw_device support.
 * This means that the return value will be TRUE if and only if
 * any command line argument will be marked out, which means
 * that at least <keyoption> was found on the command line.
 * Medium command line options are parsed already, and their values
 * are reflected in *clMediumInfo.
 */
extern bool selectRawDevice(int argc, char **argv, char *keyOption,
                            MediumInfo *clMediumInfo, Device **pDevice)
{
    int      n, index, nextIndex, count;
    bool     result;

    result   = FALSE;           /* no command line argument recognized */
    *pDevice = NULL;            /* no device created yet */

    /* check device keyOption
     */
    if( (index = clFindArgument(argc, argv, keyOption)) == 0 )
    {
        return FALSE;       /* keyOption not found */
    }
    argv[index++] = NULL;   /* mark out as used, go to next argument */
    fprintf(uctout,"\t%s", keyOption);
    if( argv[index] == NULL || argv[index][0] == '-' )
    {
        fprintf(uctout, "\n\nOption argument missing for: %s\n", keyOption);
        return TRUE;
    }
    fprintf(uctout," %s\n", argv[index]);

    /* end of command line argument parsing
     * create image device, verify <image file chunks list>
     */
    *pDevice = createRawDevice( argv[index], clMediumInfo );

    if (*pDevice)
    {
        result = TRUE;
        argv[index] = NULL;     /* mark out used device id */
    }

    return result;
}

#endif /* RAW_DEVICE */

