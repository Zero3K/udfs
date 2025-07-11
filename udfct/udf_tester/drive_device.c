/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999, 2000
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : drive_device.c
 *
 * Description :
 *
 * Author(s)   : cbiks@microsoft.com
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uct_core.h"
#include "platform.h"
#include "drive_device.h"   /* may #define DRIVE_DEVICE */

/****/

#ifdef DRIVE_DEVICE

#define MI_LASTVALIDBLOCKNR_UNDEF 0xffffffff

#define _WIN32_WINNT    0x500

#include <windows.h>
#include <winioctl.h>

#include "commandline.h"

typedef struct
{
    HANDLE      Drive;
} DriveImpUse;

FILE *driveErrorOut = NULL; /* default set in selectDriveDevice() */

/* Drive Device API functions
 */
static BlockState driveGetBlockState( void *driveImpUse, Uint32  blockNr )
{
    driveImpUse = driveImpUse;      /* make compiler happy */
    blockNr     = blockNr;          /* make compiler happy */
    return BSTATE_UNKNOWN;
}

static Uint32 driveReadBlock( void *driveUse,
                              Uint32 blockSize,
                              Uint32 firstBlock,
                              Uint32 nrOfBlocks,
                              Byte *buffer )
{
    DriveImpUse *driveImpUse = (DriveImpUse*) driveUse;
    Uint32 BlocksRead = 0;

    LARGE_INTEGER Offset;
    LARGE_INTEGER NewFilePointer;

    Offset.QuadPart = Int32x32To64( firstBlock, blockSize );
    NewFilePointer.QuadPart = 0;

    if (SetFilePointerEx( driveImpUse->Drive, Offset, &NewFilePointer, FILE_BEGIN )) {

        DWORD NumberOfBytesToRead = nrOfBlocks * blockSize;
        DWORD NumberOfBytesRead;
        if (ReadFile( driveImpUse->Drive, buffer, NumberOfBytesToRead, &NumberOfBytesRead, NULL )) {

            BlocksRead = NumberOfBytesRead / blockSize;

        }

    }

    if (BlocksRead != nrOfBlocks) {

        if (driveErrorOut != NULL) {

            fprintf(driveErrorOut, "\tdriveReadBlock error: ReadFile() fails,"
                    " blocks read: %lu, requested: %lu\n",
                    BlocksRead, nrOfBlocks);
        }
    }

    return BlocksRead;
}

/* close drive device and free space allocated by drive driveUse
 */
static void driveCloseAndFreeImpUse(void *driveUse)
{
    DriveImpUse *driveImpUse = (DriveImpUse *) driveUse;

    if( driveImpUse != NULL )
    {
        CloseHandle(driveImpUse->Drive);
        free(driveImpUse);
    }
}

/* Fill Device API function table for drive device.
 */
static void driveInitializeFunctionTable(Device *table)
{
    table->getBlockState      = driveGetBlockState;
    table->readBlock          = driveReadBlock;
    table->closeAndFreeImpUse = driveCloseAndFreeImpUse;
}

/* Initialize for drive file
 * Note that in case of an error, cleanup is done at the
 * calling side, using driveCloseAndFreeImpUse(driveImpUse);
 */
static bool initializeDriveImpUse(int argc, char **argv,
                                  MediumInfo  *clMediumInfo,
                                  Device *device)
{
    bool result = FALSE;
    int index;
    CHAR VolumeLabel[ MAX_PATH ];
    DriveImpUse *driveImpUse = (DriveImpUse *)(device->impUse);

    clearMediumInfo( &device->mediumInfo );

    /* merge device mediumInfo and command line mediumInfo
     * result to device mediumInfo.
     * overrule by command line MediumInfo allowed.
     */
    (void) mergeMediumInfo(  clMediumInfo, &device->mediumInfo,
                             &device->mediumInfo,
                             "command line", "configuration file" );

    index = clFindArgument(argc, argv, NULL);   /* get the drive to open */

    _snprintf( VolumeLabel, sizeof( VolumeLabel ), "\\\\.\\%s", argv[index] );

    driveImpUse->Drive = CreateFile( VolumeLabel,
        GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE,
        (LPSECURITY_ATTRIBUTES) NULL,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        NULL );

    if (driveImpUse->Drive != INVALID_HANDLE_VALUE) {

        PARTITION_INFORMATION   PartitionInfo;
        GET_LENGTH_INFORMATION  LengthInfo;
        DISK_GEOMETRY           DiskGeometry;

        DWORD                   BytesReturned;
        Uint32                  lastValidBlockNr = MI_LASTVALIDBLOCKNR_UNDEF;

        argv[index] = NULL;     /* not needed any more, mark out */

        result = DeviceIoControl( driveImpUse->Drive,
            FSCTL_ALLOW_EXTENDED_DASD_IO,
            NULL, 0,
            NULL, 0,
            &BytesReturned,
            NULL );

        if (!result) {

            fprintf(uctout,
              "Error: Unable to enable Extended DASD I/O for %s.\n"
              "       This is not a fatal error, but this may result in errors\n"
              "       because the entire UDF sturcture may not be accessable.\n",
              argv[index] );

            result = TRUE;

        }

        result = DeviceIoControl( driveImpUse->Drive,
            IOCTL_DISK_GET_DRIVE_GEOMETRY,
            NULL, 0,
            &DiskGeometry, sizeof( DISK_GEOMETRY ),
            &BytesReturned,
            NULL );

        if (result) {

            if (DiskGeometry.MediaType == Unknown) {

                memset( &DiskGeometry, 0, sizeof( DISK_GEOMETRY ) );
                DiskGeometry.MediaType = Unknown;

            }

            device->mediumInfo.blockSize = DiskGeometry.BytesPerSector;

            if (DiskGeometry.MediaType == FixedMedia || DiskGeometry.MediaType == RemovableMedia) {

                bool NonFatalResult;

                NonFatalResult = DeviceIoControl( driveImpUse->Drive,
                    IOCTL_DISK_GET_LENGTH_INFO,
                    NULL, 0,
                    &LengthInfo, sizeof( GET_LENGTH_INFORMATION ),
                    &BytesReturned,
                    NULL );

                if (NonFatalResult) {

                    NonFatalResult = DeviceIoControl( driveImpUse->Drive,
                        IOCTL_DISK_GET_PARTITION_INFO,
                        NULL, 0,
                        &PartitionInfo, sizeof( PartitionInfo ),
                        &BytesReturned,
                        NULL );

                    if (NonFatalResult) {

                        lastValidBlockNr = (Uint32) ((PartitionInfo.PartitionLength.QuadPart / device->mediumInfo.blockSize) - 1);

                        NonFatalResult = DeviceIoControl( driveImpUse->Drive,
                            IOCTL_DISK_IS_WRITABLE,
                            NULL, 0,
                            NULL, 0,
                            &BytesReturned,
                            NULL );

                        if (NonFatalResult) {

                            device->mediumInfo.writabilityType = MTYPE_WR_REWRITABLE;

                        } else {

                            //  GetLastError() == ERROR_WRITE_PROTECT
                            device->mediumInfo.writabilityType = MTYPE_WR_READONLY;

                        }

                    }

                }

            }

        } else {

            fprintf(uctout,
                "Fatal Error: Unable to query the disk geometry of drive %s.  Status returned = %x\n",
                argv[index], GetLastError() );

        }

        if (result) {

            if (lastValidBlockNr == MI_LASTVALIDBLOCKNR_UNDEF) {

                fprintf(uctout,
                    "Error: Unable to get the exact number of sectors on the disk.\n"
                    "-      This is not a fatal error, but may result in errors because the\n"
                    "-      entire UDF sturcture may not be accessable.\n",
                    argv[index] );

                lastValidBlockNr = (Uint32)(DiskGeometry.Cylinders.QuadPart * DiskGeometry.TracksPerCylinder * DiskGeometry.SectorsPerTrack);
            }

            if (device->mediumInfo.lastValidBlockNr == 0) {

                device->mediumInfo.lastValidBlockNr = lastValidBlockNr;
            }

            /* set other defaults and do consistency checks
             */
            //result = finishMediumInfo( &driveImpUse->mediumInfo );

        }

        if (!result) {

            driveCloseAndFreeImpUse( driveImpUse );
        }

    }

    return result;

}

static Device *createDriveDevice(int argc, char **argv,
                                 MediumInfo *clMediumInfo)
{
    Device      *device;
    DriveImpUse *myDriveImpUse;

    /* Create Device structure.
     * Use tst_calloc() so that all members that might
     * not be initialized are 0 or NULL.
     */
    if(    (device        = (Device*)      tst_calloc(sizeof(Device), 1,
                                                __FILE__, __LINE__)) == NULL
        || (myDriveImpUse = (DriveImpUse*) tst_calloc(sizeof(DriveImpUse), 1,
                                                __FILE__, __LINE__)) == NULL )
    {
        myDriveImpUse = NULL;           /* keep compiler happy */
        uctExit(EXIT_OUT_OF_MEMORY);
    }

    /* myDriveImpUse ok now
     * Install it and install access functions
     */
    device->impUse = (void *) myDriveImpUse;

    if( !initializeDriveImpUse(argc, argv, clMediumInfo, device) )
    {
        driveCloseAndFreeImpUse((void *)myDriveImpUse);
        miFreeArrays(&device->mediumInfo);
        free(device);
        return NULL;        /* error */
    }

    driveInitializeFunctionTable(device);

    return device;
}

/****** device select function for drive_device support:
 * (-drive) <keyOption> is NOT optional for drive_device support.
 * This means that the return value will be TRUE if and only if
 * any command line argument will be marked out, which means
 * that at least <keyoption> was found on the command line.
 * Medium command line options are parsed already, and their values
 * are reflected in *clMediumInfo.
 */
extern bool selectDriveDevice(int argc, char **argv, char *keyOption,
                              MediumInfo *clMediumInfo, Device **pDevice)
{
    int      index;

    *pDevice = NULL;            /* no device created yet */

    /* check device keyOption
     */
    if( (index = clFindArgument(argc, argv, keyOption)) == 0 )
    {
        return FALSE;           /* keyOption not found */
    }
    argv[index] = NULL;         /* mark out keyOption */

    /* We know now that <keyOption is present, so the return
     * result of selectDriveDevice() can only be TRUE from now.
     * We also require that the <keyOption> argument is at
     * argv[index+1].
     */
    fprintf(uctout,"\t%s", keyOption);
     if( argv[index+1] == NULL )
    {   fprintf(uctout, "\t\t <== error: missing argument\n");
    }
    else
    { index = clNextArgument(argc, argv, index);
      fprintf(uctout," %s\n", argv[index]);
      /* End of command line argument parsing, selectDevice()
       * will cope with remaining unparsed command line arguments.
       * Create drive device, NULL if cannot create.
       */
      *pDevice = createDriveDevice( argc, argv, clMediumInfo );
    }
    return TRUE;      /* <keyOption> found */
}

#endif /* DRIVE_DEVICE */

