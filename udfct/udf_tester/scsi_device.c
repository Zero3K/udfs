/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : scsi_device.c
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl,
 *               Alex Sinitsyn
 */

#include <stdio.h>
#include <time.h>

#include "scsi_device.h"    /* may #define SCSI_DEVICE */
#include "version.h"

#ifdef  SCSI_DEVICE         /* compile for scsi use only */

#ifdef          WIN32
struct _RPC_ASYNC_STATE;    /* make compiler happy (TODO: ok??) */
#include <windows.h>        /* for Sleep() */
//#include <winbase.h>      /* for Sleep() ?? */
#define SLEEP_SECONDS(SEC)  Sleep(1000 * SEC)

#elif defined( LINUX ) || defined( NETBSD )
#include <sys/types.h>      /* for open(), etc */
#include <sys/stat.h>       /* for open(), etc */
#include <sys/ioctl.h>      /* for IOCTLs */
#include <fcntl.h>          /* for open() */
#include <dirent.h>         /* for opendir(), readdir() */
#include <errno.h>
#define SLEEP_SECONDS(SEC)  sleep(SEC)

#ifdef LINUX
#include <linux/../scsi/sg.h>
#include <linux/../scsi/scsi.h>

#elif defined( NETBSD )
#include <sys/scsiio.h>
#include <dev/scsipi/scsipi_all.h>
#endif

#endif  /* LINUX, NETBSD */

#include "commandline.h"


/* extern
 */
 FILE *scsiErrorOut = NULL; /* default set in selectScsiDevice() */

/* TODO: SCSI get block state function,
 *       should be able to detect unrecorded blocks.
 */
static BlockState scsiGetBlockState( void *impUse, Uint32 blockNr )
{
    impUse  = impUse;       /* make compiler happy */
    blockNr = blockNr;      /* make compiler happy */
    return BSTATE_UNKNOWN;
}

/* Implementation note:
 *  Limit number of bytes to read at once to SCSIREAD_MAXBYTES
 *  because of:
 *  1) The Ricoh MP6200S won't let us read more than 32 blocks
 *     (64 Kbytes) at a time. It even does not give an error return
 *     in that case !!! Reason: See ASPI FAQ
 *  2) The PCMCIA APA-1480 card even has problems with 32 blocks (64 Kbyte).
 *
 *  So set max read for scsiRead() to 32 Kbyte for the moment.
 */
//#define SCSIREAD_MAXBYTES ( 32*1024)  /** release 1.0r2 and lower   **/
                                /** see ASPI FAQ, and PCMCIA APA-1480 **/
#define SCSIREAD_MAXBYTES   ( 64*1024)  /** release 1.0r5 and higher  **/

static Uint32 scsiReadBlocks(void  *impUse,
                             Uint32 blockSize,
                             Uint32 firstBlock,
                             Uint32 nrOfBlocks,
                             Byte  *buffer)
{
    Uint32 maxBlocks = SCSIREAD_MAXBYTES / blockSize,
           maxBlocksPerRead,
           blocks, blocksRead = 0;
    bool   retry = FALSE;

    /* For nmb of blocks per scsRead() limitation,
     * see explanation above.
     */
    maxBlocksPerRead = maxBlocks;
    while( nrOfBlocks > blocksRead )
    {
        blocks = MIN(maxBlocksPerRead, nrOfBlocks - blocksRead);

#undef  SCSI_DOUBLE_READ        /* normally #undef */
#ifdef  SCSI_DOUBLE_READ
    /*  robust read, read each sector twice ??
     */
    (void) scsiRead((ScsiImpUse*) impUse, blockSize,
                      firstBlock, blocks, buffer);
#endif

    if( !scsiRead((ScsiImpUse*) impUse, blockSize,
                      firstBlock, blocks, buffer) )
    {   /* scsiRead error */
            if( scsiErrorOut != NULL )          /* uctout */
            {   fprintf(scsiErrorOut, "error: scsiRead failed (%lu %lu)",
                        firstBlock, blocks);
                fflush(scsiErrorOut);
            }
            if( blocks == 1 )   /* single block read error */
            {   if( retry )     /* retry done */
                {   if( scsiErrorOut != NULL )  /* uctout */
                    {   fprintf(scsiErrorOut,   /* full error message */
                            "\n-\t%s\n", scsiGetLastErrorMessage());
                        fflush(scsiErrorOut);
                    }
                    break;      /* exit scsiReadBlocks */
                }
                if( scsiErrorOut != NULL )      /* uctout */
                {   fprintf(scsiErrorOut, ", retry\n");
                    fflush(scsiErrorOut);
                }
                retry = TRUE;   /* do single block retry */
            }
            else /* multiple block read error */
            {    /* back to single block read in order to
                  * read as many correct blocks as possible
                  */
#undef  BACK_IN_STEPS
#ifdef  BACK_IN_STEPS       /* not lower than 1 */
                maxBlocksPerRead = (maxBlocksPerRead + 1 ) / 2;
                if( scsiErrorOut != NULL )      /* uctout */
                {   fprintf(scsiErrorOut,
                                ", back to %lu blocks per read\n",
                                        maxBlocksPerRead);
                    fflush(scsiErrorOut);
                }
#else
                maxBlocksPerRead = 1;
                if( scsiErrorOut != NULL )      /* uctout */
                {   fprintf(scsiErrorOut,
                                ", retry with single block read\n");
                    fflush(scsiErrorOut);
                }
#endif
                retry = FALSE;
            }
        }
        else    /* scsiRead ok */
        {
            buffer += blocks * blockSize;
            firstBlock += blocks;
            blocksRead += blocks;
            retry = FALSE;

            /* if aligned on maxBlocks then
             * try to read more blocks at once
             */
            if(    maxBlocksPerRead < maxBlocks     /* not max */
               && (firstBlock % maxBlocks) == 0 )   /* aligned */
            {   maxBlocksPerRead = MIN(2 * maxBlocksPerRead, maxBlocks);
            }
        }
    }

    if(   scsiErrorOut != NULL          /* uctout */
       && blocksRead != nrOfBlocks )    /* read error */
    {   fprintf(scsiErrorOut,
                "scsiReadBlocks error: %lu %lu %lu\n",
                    firstBlock - blocksRead,
                    nrOfBlocks, blocksRead);
        fflush(scsiErrorOut);
    }

    return blocksRead;  /* error if blocksRead != (<) nrOfBlocks */

}   /* end scsiReadBlocks() */


/* close scsi device and free space allocated by scsi impUse
 * return value: (Device *) NULL
 */
static void scsiCloseAndFreeImpUse ( void *impUse )
{
    if( impUse != NULL )
    {   /** TODO: scsiClose((ScsiImpUse*) impUse); **/
        free(impUse);
    }
}


/* Fill Device API function table for scsi device.
 */
static void scsiInitializeFunctionTable(Device *device)
{
    device->getBlockState      = scsiGetBlockState;
    device->readBlock          = scsiReadBlocks;
    device->closeAndFreeImpUse = scsiCloseAndFreeImpUse;
}

static void printScsiApiVersionError(FILE *out,
                                     char *pSa_version)
{   fprintf( out,
      "\nScsi Api version must be equal to udf_test version,\n"
        "-\tScsi Api version: \"%s\",\n"
        "-\tudf_test version: \"%s\",\n"
#ifdef WIN32
        "-\twrong udf_scsi.dll version.\n"
#endif
        , pSa_version, app_version );
}

/* checkScsiApiVersion():
 * must be equal to app_version,
 * else fatal uctExit()
 */
static void checkScsiApiVersion()
{
    char scsiApi_version[50+1];

    getScsiApiVersion(scsiApi_version, sizeof(scsiApi_version));

    if( strcmp( scsiApi_version, app_version) != 0 )
    {   printScsiApiVersionError( uctout, scsiApi_version);
        printScsiApiVersionError( stderr, scsiApi_version);
        uctExit(EXIT_PROGRAM_ERROR);        /* quit */
    }
}

/* showScsiInfo():
 * Only used to show SCSI number of host adapters and
 * connected devices.
 * AGS 10.04.2000: give extended info on SCSI adapters
 * AGS 03.07.2000: addition Linux support
 */
#undef  DEBUG_SHOWSCSI      /* normally #undef */

#ifdef  WIN32       /* WIN32 version of showScsiInfo() */

static void showScsiInfo(void)      /* WIN32 version */
{
    ScsiImpUse tmpImpUse;
    int      nrHostAdapters, count = 0;
    BYTE     deviceType, inqVersion;
    char     vendorIdentification[8+1];
    char     productIdentification[16+1];
    char     productRevisionlevel[4+1];
    UCHAR    maxMaxScsiTargets = 0; /* max of 'max nmb of scsi targets'
                                     * for all host adapters */


    checkScsiApiVersion();      /* fatal uctExit() on error */
    fprintf( uctout, "Scsi devices information:\n");

    /* Init scsis aspi and determine number host adapters
     */
    if( !scsiInit() )
    { fprintf(uctout,
        "SCSI error: scsiInit() failed, file %s, line %lu :\n"
        "-\t%s\n", __FILE__, __LINE__, scsiGetLastErrorMessage());
      uctExit(EXIT_PROGRAM_ERROR);      /* quit */
    }
    if( !scsiNrHostAdapters(&nrHostAdapters) )
    { fprintf(uctout,
        "SCSI error: scsiNrHostAdapters() failed, file %s, line %lu :\n"
        "-\t%s\n", __FILE__, __LINE__, scsiGetLastErrorMessage());
      uctExit(EXIT_PROGRAM_ERROR);      /* quit */
    }

#ifdef  DEBUG_SHOWSCSI
    fprintf(uctout, "DEBUG: nrHostAdapters: %d", nrHostAdapters);
    if( nrHostAdapters < 1 )
    { nrHostAdapters = 3;
      fprintf(uctout, "  ==> force to 3");
    }
    printf("\n");
    fflush(uctout);
#endif  /** DEBUG_SHOWSCSI **/

    for( tmpImpUse.hostAdapter = 0;
         tmpImpUse.hostAdapter < nrHostAdapters;
         tmpImpUse.hostAdapter++ )
    {
        UCHAR maxScsiTargets;
        fflush(uctout);     /* avoid scsi dll and main program output mix up */
        if( !aspiPrintHostAdapterInfo((UCHAR)tmpImpUse.hostAdapter,
                                      &maxScsiTargets) )
        {   fprintf(uctout,
                "Error: aspiPrintHostAdapterInfo() failed for hostAdapter %2u\n",
                    tmpImpUse.hostAdapter);
            uctExit(EXIT_PROGRAM_ERROR);        /* quit */
        }
        fflush(uctout);     /* avoid scsi dll and main program output mix up */

        /* Try SCSI ID zero till (maxScsiTargets - 1) included
         * as returned by aspiPrintHostAdapterInfo()
         * (maxScsiTargets may be zero for unused host adapter).
         * Remember max of 'max nmb of SCSI targets'.
         */
        maxMaxScsiTargets = MAX( maxMaxScsiTargets, maxScsiTargets);

        for( tmpImpUse.scsiId = 0;
             tmpImpUse.scsiId < maxScsiTargets;
             tmpImpUse.scsiId++ )
        {
            if(    scsiInquiry(&tmpImpUse, &deviceType,
                                NULL, NULL, NULL, NULL, NULL)
                && deviceType != DTYPE_UNKNOWN )
            {   printDeviceType(&tmpImpUse, deviceType);
                fflush(uctout); /* avoid dll and main program output mix up */
            }
#ifdef  DEBUG_SHOWSCSI
            else            /* scsiInquiry() failed */
            { fflush(uctout);
              fprintf(uctout, "DEBUG: %2u:%-2u scsiInquiry() failed\n",
                     tmpImpUse.hostAdapter, tmpImpUse.scsiId);
              fflush(uctout);
            }
#endif  /** DEBUG_SHOWSCSI **/
        }
    }

    /* repeat for compact list
     */
    fprintf(uctout, "\nDevices possibly containing UDF:\n");

    for( tmpImpUse.hostAdapter = 0;
         tmpImpUse.hostAdapter < nrHostAdapters;
         tmpImpUse.hostAdapter++ )
    { /* Use maxMaxScsiTargets for all hostadaptors,
       * scsiInquiry() will simply fail if no device present.
       */
      for( tmpImpUse.scsiId = 0;
           tmpImpUse.scsiId < maxMaxScsiTargets;
           tmpImpUse.scsiId++ )
      { if( scsiInquiry(&tmpImpUse, &deviceType, &inqVersion,
                vendorIdentification, productIdentification,
                productRevisionlevel, NULL) )
        {                   /* INQUIRY command ok */
          fprintf(uctout,
            "  <h>:<s> %2u:%-2u %24s - %-8s %s %s, INQUIRY version %u\n",
                tmpImpUse.hostAdapter, tmpImpUse.scsiId,
                SCSI_DTYPE_TEXT(deviceType),
                vendorIdentification, productIdentification,
                productRevisionlevel, inqVersion);

          if( SCSI_DTYPE_UDF(deviceType) )  /* supported device */
          { count++;                        /* found one */
          }
          else                              /* unsupported device */
          { fprintf(uctout,
              "=======> ignore %2u:%-2u, unsupported device type %u <=====\n",
                tmpImpUse.hostAdapter, tmpImpUse.scsiId, deviceType);
          }
        }
      }
    }
    fprintf(uctout, "%s\n", (count == 0) ? "  None found\n" : "");

}   /* end static void showScsiInfo()           ** WIN32 version */


#elif defined( LINUX ) || defined( NETBSD ) /** UNIX_FLAVOUR_SCSI_ATAPI **/

/* showScsiInfo() and scsiFindDevice() for LINUX, NETBSD
 *
 * showScsiInfo() uses only a fixed limited range of /dev/* devices.
 */
static char *scsiDeviceNoneFoundMessage =
  "  Check if scsi/atapi compatible software has been installed\n"
  "  and if device permissions and privileges are sufficient.\n"
  "  Note that '-scsi /dev/YY' does not have rescrictions for YY,\n"
  "  so it accepts any string for YY.\n"
  "  If still no success, it is worth trying /dev/YY without \"-scsi\",\n"
  "  but mind that multisession media and media using VAT may fail\n"
  "  or give wrong verification results in that case.\n";

/* scsiDevTable for excludeDevDevice():
 * Only devices in scsiDevTable are shown
 * by showscsi and searched by findscsi
 * for the moment. A suffix of at most 2 arbitrary charaters
 * is allowed (and marked as XX).
 */
static char *scsiDevTable[] =
{
    "/dev/sg"  ,
    "/dev/scd" ,
    "/dev/sd"  ,
    "/dev/hd"  ,
    "/dev/loop",
    "/dev/cd"  ,
    "/dev/cdrom",
    "/dev/cdreader",
    "/dev/cdwriter",
    "/dev/cdburner",
    "/dev/burner"
};

#define NMB_devTable (sizeof(scsiDevTable) / sizeof(char*))

/* print list of searched devices.
 * XX suffix, 2 space indent.
 */
static void printScsiDevTable()
{   int n, cnt = 0;
    for( n = 0; n < NMB_devTable; n++ )
    { cnt += (4 + strlen(scsiDevTable[n]));
      fprintf(uctout, "%s%sXX",
            (n==0) ?    "  " :
        (cnt > 60) ? ",\n  "
                   :    ", ",
            scsiDevTable[n]);
      if( cnt > 60 ) cnt = 0;
    }
    fprintf(uctout,
      "\n  where XX is any suffix of at most 2 characters.\n");
}

/* excludeDevDevice():
 * Check devPath. Only devices in scsiDevTable
 * (with arbitraty suffix of max 2 chars) are
 * allowed for the moment, all others are excluded.
 *
 * return TRUE if device must be excluded
 *   else FALSE.
 */
static bool excludeDevDevice(unsigned char *devPath)
{   int n, pathlen = strlen(devPath);

    for( n = 0; n < NMB_devTable; n++ )
    {   char *prefix  = scsiDevTable[n];
        int   preflen = strlen(prefix);

        /* Check if prefix from scsiDevTable[] matches,
         * and max 2 char arbitrary suffix.
         */
        if(   pathlen >= preflen
           && (pathlen - preflen) <= 2
           && memcmp(devPath, prefix, preflen) == 0 )
        {   /* match and max 2 char suffix
             */
            return FALSE;   /* no exclusion */
        }
    }
    return TRUE;            /* exclude */
}


/* Try to open device and get scsi/atapi scsi Ids
 */
static bool openDeviceAndGetScsiIds(char       *devPath,
                                    ScsiImpUse *impUse)
{
    /* remark: O_RDONLY instead of O_RDWR does not work,
     * TODO: investigate ??
     */
    impUse->fd = open(devPath, O_RDWR);
    if( impUse->fd >= 0 )
    {   /* open() ok, get SCSI Ids.
         * platform dependent code.
         */

#ifdef  LINUX               /* get hostAdapter/scsiId ioctls */

typedef struct scsi_idlun
{   int dev_id;
    int host_unique_id;
} SCSI_idlun;

        SCSI_idlun idLun;
        int        host_no;

        if(   ioctl(impUse->fd,
                    SCSI_IOCTL_GET_IDLUN, &idLun)       >= 0
           && ioctl(impUse->fd,
                    SCSI_IOCTL_GET_BUS_NUMBER, &host_no) >= 0 )
        {   /* ioctl's ok, get hostAdapter and scsiId
             */
            impUse->hostAdapter = host_no;
            impUse->scsiId      = (idLun.dev_id & 0xFF);
            return TRUE;        /* device opened */
        }

#elif defined( NETBSD )     /* get hostAdapter/scsiId ioctls */

        struct scsi_addr saddr;

        if( ioctl(impUse->fd, SCIOCIDENTIFY, &saddr) == 0 )
        {   if( saddr.type == 0 )
            {   impUse->hostAdapter = saddr.addr.scsi.scbus;
                impUse->scsiId      = saddr.addr.scsi.target;
            }
            else    /* saddr.type != 0 */
            {   impUse->hostAdapter = saddr.addr.atapi.atbus;
                impUse->scsiId      = saddr.addr.atapi.drive;
            }
            return TRUE;    /* device opened on impUse->fd */
        }

#endif  /* NETBSD, LINUX */


        close(impUse->fd);  /* ioctl() not accepted */
    }
    impUse->fd = -1;        /* invalid */
    return FALSE;           /* device not opened */

}   /* end openDeviceAndGetScsiIds() */

static void showScsiInfo(void)          /* for LINUX & NETBSD */
{
    ScsiImpUse      tmpImpUse;
    BYTE            deviceType, inqVersion;
    char            vendorIdentification[8+1];
    char            productIdentification[16+1];
    char            productRevisionlevel[4+1];
    int             count;
    char            dev[100];
    DIR            *dirp;
    struct dirent  *dp;

    fprintf( uctout, "Scsi devices information:\n");
    fflush(uctout);

    /* check all /dev devices
     */
    dirp = opendir("/dev");
    errno = 0;
    count = 0;
    while( dirp != NULL )
    {
        errno = 0;
        if( (dp = readdir(dirp)) == NULL )
        {   break;      /* done */
        }
        /* skip if error and for 'too long name' devices
         */
        if(   errno != 0
           || dp->d_name == NULL
           || 5 + strlen(dp->d_name) >= 100 )
        {   continue;   /* skip */
        }
        sprintf( dev, "/dev/%s", dp->d_name);

        /* Device path ok, check for exclusions
         */
        if( excludeDevDevice(dev) )
        {   continue;   /* skip */
        }
#ifdef  DEBUG_SHOWSCSI
  fprintf(uctout, "showScsiInfo: %s\n", dev);
  fflush(uctout);
#endif

        /* check if device can be opened using scsi/atapi
         * on errors be silent, just skip the device
         */
        if( openDeviceAndGetScsiIds(dev, &tmpImpUse) )
        {   /* device opened, do scsiInquiry()
             * Be more critical than for Win32 because some (Linux)
             * devices report to support, but actually do not.
             */
            if(   !scsiInquiry(&tmpImpUse, &deviceType, &inqVersion,
                                vendorIdentification,
                                productIdentification,
                                productRevisionlevel, NULL) )
            {
#ifdef  DEBUG_SHOWSCSI
              fprintf(uctout,
                "%s INQUIRY error: file %s, line %lu :\n-\t%s\n",
                dev, __FILE__, __LINE__, scsiGetLastErrorMessage());
#endif
            }
            else    /* INQUIRY command ok */
            { fprintf(uctout,
                "  <h>:<s> %2u:%-2u %-10s %24s - %-8s %s %s, "
                                        "INQUIRY version %u\n",
                    tmpImpUse.hostAdapter, tmpImpUse.scsiId, dev,
                    SCSI_DTYPE_TEXT(deviceType),
                    vendorIdentification, productIdentification,
                    productRevisionlevel, inqVersion);
              if( SCSI_DTYPE_UDF(deviceType) )  /* supported device */
              {  count++;                       /* found one */
              }
              else                              /* unsupported device */
              { fprintf(uctout,
                  "=======> ignore %s : unsupported device type %u <=====\n",
                        dev, deviceType);
              }
            }
        }
        close(tmpImpUse.fd);
    }

    if( errno != 0)
    {                   /** TODO: message ?? **/
        printf( "showScsiInfo error: errno: %d\n", errno);
    }
    if( dirp != NULL )
    {   closedir(dirp);
    }

    if( count == 0 )
    { fprintf(uctout,   "None found, tested devices:\n");
      printScsiDevTable();
      fprintf(uctout, "%s", scsiDeviceNoneFoundMessage);
    }
    fprintf(uctout, "\n");

}   /* end showScsiInfo()   ** LINUX & NETBSD ** UNIX_FLAVOUR_SCSI_ATAPI **/


static bool scsiFindDevice(Device *device, char *devArch)
{
    char           dev[100];
    ScsiImpUse     tmpImpUse,
                  *scsiImpUse = (ScsiImpUse*) device->impUse;
    DIR           *dirp;
    struct dirent *dp;

    if( devArch != NULL )   /* device path -scsi argument */
    {   /* open using scsi/atapi
         */
        if( !openDeviceAndGetScsiIds(devArch, scsiImpUse) )
        {   scsiImpUse->fd = -1;    /* not opened */
        }
    }
    else    /* <h>:<s> -scsi argument */
    { /* Find and open device by host adapter and scsi ID
       */
      scsiImpUse->fd = -1;      /* invalid */
      errno = 0;
      dirp = opendir("/dev");
      while( dirp != NULL && scsiImpUse->fd < 0 )
      {
        errno = 0;
        if( (dp = readdir(dirp)) == NULL )
        {   break;      /* done */
        }
        /* skip if error and for 'too long name' devices
         */
        if(   errno != 0
           || dp->d_name == NULL
           || 5 + strlen(dp->d_name) >= 100 )
        {   continue;   /* skip */
        }
        sprintf( dev, "/dev/%s", dp->d_name);

        /* Device path ok, check for exclusions
         */
        if( excludeDevDevice(dev) )
        {   continue;   /* skip */
        }
#ifdef  DEBUG_SHOWSCSI
  fprintf(uctout,"scsiFindDevice: %s\n", dev);
  fflush(uctout);
#endif

        /* check if device can be opened using scsi/atapi
         */
        if( openDeviceAndGetScsiIds(dev, &tmpImpUse) )
        {   /* device opened, check hostAdapter and scsiId.
             * Some (Linux) devices report ambiguous <h>:<s> values, so
             * check also deviceType and vendor/product identification
             */
            char vendorIdentification[8+1];
            char productIdentification[16+1];
            BYTE deviceType;
            if(    scsiImpUse->hostAdapter != tmpImpUse.hostAdapter
               ||  scsiImpUse->scsiId     != tmpImpUse.scsiId
               || !scsiInquiry(&tmpImpUse, &deviceType, NULL,
                                    vendorIdentification,
                                    productIdentification,
                                    NULL, NULL)
               || !SCSI_DTYPE_UDF(deviceType) ) /* not supported */
            {   close(tmpImpUse.fd);     /* not the right device */
                scsiImpUse->fd = -1;
            }
            else    /* device ok */
            {   scsiImpUse->fd = tmpImpUse.fd;  /* opened device */
            }
        }
      }
      if( dirp != NULL )
      { closedir(dirp);
      }
    }

    if( scsiImpUse->fd < 0 )    /* none found */
    {   fprintf(uctout,
            "No scsi device found for: ");
        if( devArch != NULL )
        { fprintf(uctout, "%s\n", devArch);
        }
        else
        { fprintf(uctout, "%u:%u, tested devices:\n",
                  scsiImpUse->hostAdapter,
                  scsiImpUse->scsiId);
          printScsiDevTable();
        }
        fprintf(uctout,
          "%s", scsiDeviceNoneFoundMessage);
        return FALSE;       /* device not opened */
    }

    /* successfully opened device
     */
    fprintf(uctout, "  device      : %s\n",
                    (devArch != NULL) ? devArch : dev);
    return TRUE;

}   /* end scsiFindDevice() ** LINUX & NETBSD ** UNIX_FLAVOUR_SCSI_ATAPI **/

#endif  /* showScsiInfo() and scsiFindDevice() for LINUX, NETBSD */


/* Define nmb of retries for mandatory/non-mandatory commands
 * issied by ISSUECOMMANDbegin(), ISSUECOMMANDend.
 */
#define IC_RETRY_LOW     1  /* total  2 tries */
#define IC_RETRY_NORMAL  4  /* total  5 tries */
#define IC_RETRY_HIGH   14  /* total 15 tries */

/* Define ISSUECOMMANDbegin/end macros to be used for commands
 * sent to the drive. Maybe retry with SLEEP_SECONDS(1) in case
 * of error.
 * NOTE 1:
 *  Expressions cannot be used as argument for ISSUECOMMANDbegin();
 * NOTE 2:
 *  The IC_BODY() function must assign a boolean result to the
 *  boolean in IC_OK. TRUE for success, FALSE for error.
 */
#define ISSUECOMMANDbegin(IC_TXT1, IC_RETRY, IC_OK, IC_TXT2)    \
{   bool *pOk = &IC_OK; /* for ISSUECOMMANDend */   \
    int   ic_cnt;                                   \
    char *ic_txt2 = IC_TXT2;                        \
    fprintf(uctout,"\n%s command:", IC_TXT1);       \
    fflush(uctout);                                 \
    for( ic_cnt = 0,                 IC_OK = FALSE; \
         ic_cnt < (1 + IC_RETRY) && IC_OK == FALSE; \
         ic_cnt++ )                                 \
    {

      /** here call: IC_OK = IC_BODY(...)  **/

#define ISSUECOMMANDend             \
      if( !(*pOk) )    /* error */  \
      { fprintf(uctout,"%s.",       \
          (ic_cnt==0) ? " " : "");  \
        fflush(uctout);             \
        SLEEP_SECONDS(1);           \
      }                             \
    }                               \
    fprintf(uctout,ic_txt2);        \
    fflush(uctout);                 \
}

/** Usage example of ISSUECOMMANDbegin/end :
 */
//  ISSUECOMMANDbegin("READ DVD STRUCTURE", IC_RETRY_NORMAL,
//                     okDvdStruct, "\n");
//  { /* IC_BODY() function assignment:
//     */
//    okDvdStruct = scsiReadDvdStructure( scsiImpUse,
//                            &bookType, &partVersion, ... );
//    /* maybe use ic_cnt for message (0 for first time) */
//  }
//  ISSUECOMMANDend;
/**/

static bool execInquiry(ScsiImpUse *scsiImpUse,
                        BYTE *pPerDeviceType
                       )
{   bool ok;
    char    vendorIdentification[8+1];
    char    productIdentification[16+1];
    char    productRevisionlevel[4+1];
    USHORT  versionDescriptors[8];
    BYTE    inqVersion = 0;
    int     n;

    ISSUECOMMANDbegin("INQUIRY", IC_RETRY_NORMAL, ok, "\n");
    { /* IC_BODY() function assignment:
       */
      ok = scsiInquiry( scsiImpUse, pPerDeviceType, &inqVersion,
                        vendorIdentification, productIdentification,
                        productRevisionlevel, versionDescriptors );
    }
    ISSUECOMMANDend;

    if( !ok )
    {   fprintf(uctout, "INQUIRY failed, file %s, line %lu :\n-\t%s\n",
            __FILE__, __LINE__, scsiGetLastErrorMessage());
        return FALSE;
    }
    fprintf(uctout,   "  Vendor identification : %s\n", vendorIdentification);
    fprintf(uctout,   "  Product identification: %s\n", productIdentification);
    fprintf(uctout,   "  Product revision level: %s\n", productRevisionlevel);
    fprintf(uctout,   "  INQUIRY/ANSI Version  : %u\n", inqVersion);
    fprintf(uctout,   "  Version Descriptors   :");
    /* only show Version Descriptor unequal to zero
     * and not if equal to previous one
     */
    for( n = 0; n < 8; n++ )
    {   USHORT vd = versionDescriptors[n];
        if( vd != 0 && (n == 0 || vd != versionDescriptors[n-1]) )
        { fprintf( uctout, " 0x%04X", vd);
        }
    }
    fprintf(uctout, "\n  Peripheral Device Type: 0x%02X - %s\n",
                    (*pPerDeviceType), SCSI_DTYPE_TEXT(*pPerDeviceType));
    fflush(uctout);

    return TRUE;
}

/* scsiStartDeviceSetDL():
 * Set Multi-Layer arguments for 'far apart' calculations.
 * L0capacity == 0 means Single Layer
 */
static void scsiStartDeviceSetDL(MediumInfo *dmi,
                                 Uint32 L0capacity, bool isOTP )
{
    if(   L0capacity != 0       /* ML */
       && (   dmi->L0capacity != L0capacity
           || dmi->isOTP != isOTP ) )
    { fprintf(uctout,
        "\n==> set L0 capacity to: %lu   for %s %s%s\n",
            L0capacity, (isOTP) ? "ML" : "DL",
            MI_OTP_TEXT(isOTP),
            (dmi->L0capacity != 0)
                    ? "  (overruled)" : "" );
      dmi->L0capacity = L0capacity;
      dmi->isOTP = isOTP;
    }
}

/* start scsi device, fill mediumInfo.
 * if devArch != NULL
 *  then: use <device path> in devArch in order to find <h>:<s>
 *  else: use hostAdapter and scsiId in device->impUse.
 */
static bool scsiStartDevice(Device *device,
                            char   *devArch,
                            Uint32  clLastValidBlockNr)
{
    ULONG   maxReadableSector = 0,
            sectorLength = 0;
    Byte    senseMediumType = 0;

    MediumInfo *dmi = &device->mediumInfo;
    ScsiImpUse *scsiImpUse = (ScsiImpUse*) device->impUse;
    BYTE    perDeviceType = 0;

    BYTE    discStatus = 0, stateLastSession = 0,
            firstTrackOnDisc = 0;
    USHORT  RDInrOfSessions = 0,
            firstTrackLastSession = 0,
            lastTrackLastSession = 0,
            lastTrack;
    BYTE    bgFormatStatus = 0,
            discType = 0;
    BYTE    dataMode = 0, trackMode = 0, LJRS = 0;
    Uint32  theTrack;

    USHORT  rtiTrackNumber = 0, rtiSessionNumber = 0,
            theSession, lastSession, lastDataSession;
    bool    isDamageBitSet  = FALSE, isCopyBitSet   = FALSE,
            isRTBitSet      = FALSE, isBlankBitSet  = FALSE,
            isPacketBitSet  = FALSE, isFPBitSet     = FALSE,
            isNWA_VBitSet   = FALSE, isLRA_VBitSet  = FALSE,
            isErasableBitSet = FALSE;
    ULONG   trackStartAddress = 0, startNextTrack = 0,
            nextWritableAddress = 0,
            lastRecordedAddress = 0, freeBlocks     = 0,
            fixedPacketSize  = 0,    trackLength    = 0,
            startPsnDataArea = 0, endPsnDataArea    = 0,
            endPsnDataL0     = 0, L0DataZoneCapacity = 0,
            nextLJaddress    = 0, lastLJaddress     = 0;
    BYTE    tiRes04     = 0, tiRes07        = 0,
            tiRes34     = 0, tiRes35        = 0;
    USHORT  driveSupportLen = 0, currentProfile = 0;
    BYTE    bookType    = 0, partVersion    = 0,
            discSize    = 0, maxRate        = 0,
            nmbOfLayers = 0, trackPathOTP   = 0;    /* 0: PTP, 1: OTP */
    bool    ok, okInquiry,
            okGetConfig = FALSE,
            okDvdStruct = FALSE,
            okBdStruct  = FALSE;
    char   *txt = "";

    checkScsiApiVersion();      /* fatal uctExit() on error */

    clLastValidBlockNr = clLastValidBlockNr;    /* make compiler happy */
    clearMediumInfo(dmi);

#ifdef WIN32
    /* Win32 does not support -scsi <devpath> argument
     */
    if( devArch != NULL )       /* report <h>:<s> error here */
    { fprintf(uctout, "\nError: -scsi option argument: %s\n"
                        "       Only <h>:<s> allowed for Win32, where\n"
                        "       <h> and <s> are non-negative numbers.\n",
              devArch);
      return FALSE;
    }

    if( !scsiInit() )
    { fprintf(uctout,
        "SCSI error: scsiInit() failed, file %s, line %lu :\n"
        "-\t%s\n", __FILE__, __LINE__, scsiGetLastErrorMessage());
      uctExit(EXIT_PROGRAM_ERROR);      /* quit */
    }

#elif defined( LINUX ) || defined( NETBSD )
    if( !scsiFindDevice(device, devArch ) )
    {   return FALSE;
    }
#endif

    fprintf(uctout, "  host adapter: %u\n"
                    "  SCSI Id.    : %u\n",
        scsiImpUse->hostAdapter, scsiImpUse->scsiId);

    /* INQUIRY before TEST UNIT READY
     */
    okInquiry = execInquiry( scsiImpUse, &perDeviceType);

    /* Check the device for ready status. ASPI version.
     * TEST UNIT READY command, retry with 1 sec intervals.
     */

#ifdef SCSI_TESTING
    fprintf(uctout, "=> First testUnitReady()\n");
    fflush(uctout);
#endif
    (void) testUnitReady(scsiImpUse);   /* no message first time */
    ISSUECOMMANDbegin("TEST UNIT READY", IC_RETRY_HIGH, ok, "\n");
    { /* IC_BODY() function assignment:
       */
      ok = testUnitReady(scsiImpUse);
      if( !ok && ic_cnt == 0 )
      { fprintf(uctout, "\n  Device not ready, waiting: ");
      }
    }
    ISSUECOMMANDend;

    if( !ok )
    {   uctExit(EXIT_DEVICE_NOT_READY); /* quit */
    }
    fprintf(uctout, "  Device ready\n");

    /* device ready now, retry INQUIRY if first one failed
     */
    if( !okInquiry )
    { fprintf(uctout, "\n=> retry INQUIRY\n");
      if( !execInquiry(scsiImpUse, &perDeviceType) )
      { return FALSE;       /* failed for the second time */
      }
      okInquiry = TRUE;     /* ok now */
    }

    /* further action depends on INQUIRY Peripheral Device Type
     */
    okDvdStruct = okBdStruct = FALSE;   /* initially FALSE for ALL devices */
    switch( perDeviceType )
    {
    case DTYPE_SEQD:        /* Tape Device - not implemented */
    case DTYPE_JUKE:
    default:
        return FALSE;       /* unknown/unsupported perDeviceType */
        break;

    case DTYPE_CDVD:    /* MMC device, like CD/DVD, some MO */
    case DTYPE_DASD:    /* SBC Direct-access, (e.g. HDD)    */
    case DTYPE_SDASD:   /* RBC Simplified Direct-access, (e.g. HDD) */
    case DTYPE_OPTI:    /* SBC Optical memory device, like some MO  */
    case DTYPE_WORM:    /* SBC Write-once read-multiple - not implemented */

        /* Share READ CAPACITY COMMAND for
         *     DTYPE_DASD/DTYPE_SDASD/DTYPE_OPTI
         * and DTYPE_CDVD (MMC) devices.
         *
         ** scsiReadCapacity() may fail when a
         ** new medium was just inserted in the drive, so
         ** give it some retries with 1 second intervals.
         **/

        /* Repeat READ CAPACITY on failure
         * Because READ CAPACITY is a mandatory command,
         * IC_RETRY_NORMAL is used.
         */
        ISSUECOMMANDbegin("READ CAPACITY", IC_RETRY_NORMAL, ok, "\n");
        { /* IC_BODY() function assignment:
           */
          ok = scsiReadCapacity( scsiImpUse,
                                &maxReadableSector,
                                &sectorLength);
        }
        ISSUECOMMANDend;

        if( !ok )
        {   fprintf(uctout,
                "error: scsiReadCapacity() failed\n%s\n",
                                scsiGetLastErrorMessage());
            return FALSE;
        }

        dmi->blockSize = (Uint32) sectorLength;

        /* For the moment consider a total size beyond UCT_MAXVOLUME_GBSIZE Gbyte
         * as invalid: clear maxReadableSector.
         */
        if( (((Uint64) maxReadableSector + 1) * sectorLength)
                   > UCT_MAXVOLUME_BYTESIZE )
        {   fprintf(uctout,
              "=> Ignore highest valid block number: %lu,\n"
              "-  because volume would contain more than %lu Gbyte.\n",
                        maxReadableSector, UCT_MAXVOLUME_GBSIZE);
            maxReadableSector = 0;
        }
        else
        {   fprintf(uctout,"  Highest valid block number%s: %lu\n",
                (perDeviceType == DTYPE_CDVD) ? " in last complete session"
                                           : "",
                    maxReadableSector);
        }
        fprintf(uctout,  "  Block Size     : %lu\n", sectorLength);

        if( perDeviceType != DTYPE_CDVD )   /* DTYPE_DASD/DTYPE_SDASD/DTYPE_OPTI */
        {   dmi->lastValidBlockNr = dmi->lastRecordedBlockNr = maxReadableSector;
            dmi->eccLength = 1;             /* default: no blocking factor */
//          dmi->writabilityType  = MTYPE_WR_OVERWRITABLE;

            break;          /* that's it for DTYPE_DASD/DTYPE_SDASD/DTYPE_OPTI */
        }

        /* perDeviceType == DTYPE_CDVD, CD, DVD, etc.
         * READ CAPACITY done.
         * now: GET CONFIGURATION
         * and: READ DVD STRUCTURE Format 00h
         *
         * Information is informative and used for writability type
         * (READ DVD STRUCTURE Format 00h may overrule GET CONFIGURATION results)
         *
         * GET CONFIGURATION: Repeat with IC_RETRY_LOW on failure,
         * because it is not understood by legacy CD drives.
         */
        ISSUECOMMANDbegin("GET CONFIGURATION", IC_RETRY_LOW,
                           okGetConfig, "\n");
        { /* IC_BODY() function assignment:
           */
          okGetConfig = scsiGetConfiguration(scsiImpUse,
                                            &currentProfile);
        }
        ISSUECOMMANDend;

        /* check writability type and DVD/BD structure
         * (okDvdStruct == okBdStruct == FALSE)
         */
        if( !okGetConfig )
        {   fprintf(uctout, "  failed\n");
        }
        else    /* GET CONFIGURATION ok */
        {   fprintf(uctout,
                "  Current Profile:    0x%04X    --> %s\n",
                    currentProfile, PROF_TEXT(currentProfile));
            /* set medium writabilityType
             * and okDvdStruct, okBdStruct
             */
            switch( currentProfile )
            {
            case PROF_CDROM:
            case PROF_DVDROM:
            case PROF_BDROM:
            case PROF_HDDVDROM:
                dmi->writabilityType = MTYPE_WR_READONLY;
                break;
            case PROF_CDR:
            case PROF_DVDmR:
            case PROF_DVDmRdlsr:
            case PROF_DVDmRdllj:
            case PROF_DVDpR:
            case PROF_DVDpRdl:
            case PROF_BDRsrm:   /* TODO: POW ?? */
            case PROF_BDRrrm:   /* TODO: POW ?? */
            case PROF_HDDVDR:
                dmi->writabilityType = MTYPE_WR_WRITEONCE;
                break;
//          case PROF_BDRpow:   /* TODO: POW ?? */
//              dmi->writabilityType = MTYPE_WR_POW;
//              break;
            case PROF_CDRW:
            case PROF_DVDRAM:
            case PROF_DVDmRWro:
            case PROF_DVDmRWsr:
            case PROF_DVDpRW:
            case PROF_DVDpRWdl:
            case PROF_BDRE:
            case PROF_HDDVDRAM:
                dmi->writabilityType = MTYPE_WR_OVERWRITABLE;
                break;
            }

            switch( currentProfile )
            {
            case PROF_DVDROM:
            case PROF_DVDRAM:
            case PROF_DVDmR:
            case PROF_DVDmRdlsr:
            case PROF_DVDmRdllj:
            case PROF_DVDmRWro:
            case PROF_DVDmRWsr:
            case PROF_DVDpR:
            case PROF_DVDpRdl:
            case PROF_DVDpRW:
            case PROF_DVDpRWdl:
            case PROF_HDDVDROM:     /* HD DVD support READ DVD STRUCTURE */
            case PROF_HDDVDR:
            case PROF_HDDVDRAM:
                okDvdStruct = TRUE;
                break;
            case PROF_BDROM:
            case PROF_BDRsrm:
            case PROF_BDRrrm:
            case PROF_BDRE:
                okBdStruct = TRUE;
                break;
            }
        }

        if( okDvdStruct )   /* for DVD only */
        { /* READ DVD STRUCTURE Format 00h:
           *  Repeat with IC_RETRY_NORMAL on failure,
           *  because it is only issued for DVD profile devices.
           */
          ISSUECOMMANDbegin("READ DVD STRUCTURE Format 00h", IC_RETRY_NORMAL,
                            okDvdStruct, "\n");
            /* IC_BODY() function assignment:
             */
            okDvdStruct = scsiReadDvdStructure( scsiImpUse,
                                  &bookType, &partVersion,
                                  &discSize, &maxRate,
                                  &nmbOfLayers,
                                  &trackPathOTP,    /* 0: PTP, 1: OTP */
                                  &startPsnDataArea,
                                  &endPsnDataArea,
                                  &endPsnDataL0);
          ISSUECOMMANDend;

          if( !okDvdStruct )
          { fprintf(uctout, "  failed, maybe not supported.\n");
          }
          else      /* READ DVD STRUCTURE Format 00h read and ok */
          { fprintf(uctout,
                "  Book Type          : 0x%02X     --> %s"
                                " version %u, %s, speed %s.\n",
                           bookType, BT_TEXT(bookType),
                           partVersion,
                          (discSize == 0)  ? "120 mm"
                        : (discSize == 1)  ? "80 mm"
                                           : "?? mm",
                           (maxRate == 0)   ? "1x"
                        :  (maxRate == 1)   ? "2x"
                        :  (maxRate == 2)   ? "4x"
                        :  (maxRate == 15)  ? "unspecified"
                                            : "??" );
            if( nmbOfLayers != 0 )      /* meaning multi layer */
            { fprintf(uctout,
                "  Number of Layers   : 0x%02X     -->  %s %s\n",
                    nmbOfLayers,
                       (nmbOfLayers == 1) ? "DL"
                     : (nmbOfLayers > 1)  ? "ML"
                                          : "??",
                    (trackPathOTP != 0) ? "OTP - Opposite Track Path"
                                        : "PTP - Parallel Track Path");
            }
            L0DataZoneCapacity = 0;
            fprintf(uctout,
                "  PSN Start Data Area: 0x%06X\n"
                "  PSN   End Data Area: 0x%06X",
                startPsnDataArea, endPsnDataArea);
            if(    endPsnDataArea > startPsnDataArea    /* consistent */
               && (   nmbOfLayers == 0               /* Single Layer */
                   || trackPathOTP == 0) )           /* or PTP */
            { Uint32 lsnEndData = endPsnDataArea - startPsnDataArea; /* L0 */
              fprintf(uctout, " --> LSN %9lu  %s  ",
                      lsnEndData, (nmbOfLayers != 0) ? "L0" : "");
              nBytesDoublePrint4f(
                    (Uint64)(1 + lsnEndData) * dmi->blockSize,
                                  NULL);
              if( nmbOfLayers != 0 )        /* DL PTP */
              { L0DataZoneCapacity = 1 + lsnEndData;
              }
            }
            fprintf(uctout, "\n");
            if(   endPsnDataL0 > 0      /* ML OTP */
               && trackPathOTP != 0 )   /* OTP (assert) */
            { fprintf(uctout,
                "  PSN   End Data L0  : 0x%06X",
                                endPsnDataL0);
              if(   endPsnDataArea >  startPsnDataArea
                 &&   endPsnDataL0 >  startPsnDataArea )
              { Uint32 LSNendPsnDataL0 = (Uint32)
                       (endPsnDataL0 - startPsnDataArea);
                fprintf(uctout, " --> LSN %9lu  L0  ",
                                LSNendPsnDataL0);
                nBytesDoublePrint4f(
                    (Uint64)(1 + LSNendPsnDataL0) * dmi->blockSize,
                                    NULL);
                L0DataZoneCapacity = 1 + LSNendPsnDataL0;   /* ML OTP */
              }
              fprintf(uctout, "\n");
            }

            switch( bookType )
            {
            default:    /* was set to MTYPE_WR_UNKNOWN already */
                okDvdStruct = FALSE;    /* no bookType info retrieved */
                break;
            case BT_DVDROM:
            case BT_HDDVDROM:
                dmi->writabilityType = MTYPE_WR_READONLY;
                break;
            case BT_DVDmR:      /** TODO: check separate book type for DVD-R DL ?? **/
            case BT_DVDpR:
            case BT_DVDpRdl:
            case BT_HDDVDR:
                dmi->writabilityType = MTYPE_WR_WRITEONCE;
                break;
            case BT_DVDRAM:
            case BT_DVDmRW:     /** TODO: check separate book type for DVD-RW DL ?? **/
            case BT_DVDpRW:
            case BT_DVDpRWdl:
            case BT_HDDVDRAM:
                dmi->writabilityType = MTYPE_WR_OVERWRITABLE;
                break;
            }

            /* set L0 capacity for DL
             * consistency checked above.
             */
            if( L0DataZoneCapacity != 0 )
            { scsiStartDeviceSetDL( dmi, L0DataZoneCapacity,
                                    (trackPathOTP != 0) );
            }

            /* for DL disc, also try "READ DVD STRUCTURE Format 20h
             * for L0 capacity, maybe overrule.
             */
            if( nmbOfLayers != 0 )      /* zero means single layer */
            { L0DataZoneCapacity = 0;
              /* READ DVD STRUCTURE Format 20h:
               * Repeat with IC_RETRY_NORMAL on failure,
               * because it is only issued for DVD profile devices.
               * Format 20h, see MMC-5, 6.29.2.19.
               */
              ISSUECOMMANDbegin("READ DVD STRUCTURE F20h",
                                IC_RETRY_NORMAL, ok, "\n");
                /* IC_BODY() function assignment:
                 */
                ok = scsiReadDvdStructF20( scsiImpUse, &L0DataZoneCapacity );
              ISSUECOMMANDend;

              if( !ok )
              { fprintf(uctout, "  failed, maybe not supported.\n");
              }
              else      /* READ DVD STRUCTURE F20h read and ok */
              { fprintf(uctout,
                  "  L0 Data Zone Capacity: %7lu --> ",
                            L0DataZoneCapacity);
                nBytesDoublePrint4f(
                    (Uint64)L0DataZoneCapacity * dmi->blockSize, "\n");

                /* set L0 capacity, maybe overrule,
                 * check consistency
                 */
                if(       endPsnDataArea > startPsnDataArea
                   && L0DataZoneCapacity < (endPsnDataArea + 1) )
                { scsiStartDeviceSetDL( dmi, L0DataZoneCapacity,
                                        (trackPathOTP != 0) );
                }
              }
            }   /* endif try READ DVD STRUCTURE Format 20h */
          }
        }

        if(   (!okGetConfig)
           && (!okDvdStruct)
           && (!okBdStruct) )   /* all failed */
        {
            /* Both GET CONFIGURATION and READ DVD STRUCTURE Format 00h failed
             * or did not retrun valuable information.
             * Try obsolete Medium Type of MODE SENSE command that
             * is supported by some legacy drives.
             *
             * MODE SENSE: Repeat with IC_RETRY_NORMAL on
             * failure, because it is a mandatory command.
             * It may fail when a new medium was just inserted
             * in the drive, so give it some retries with
             * 1 second intervals.
             */
            ISSUECOMMANDbegin("MODE SENSE", IC_RETRY_NORMAL, ok, "\n");
            { /* IC_BODY() function assignment:
               */
              ok = scsiModeSense10(scsiImpUse, &senseMediumType);
            }
            ISSUECOMMANDend;

            if( !ok )
            { fprintf(uctout,"error: scsiModeSense10() failed\n%s\n",
                                        scsiGetLastErrorMessage());
              return FALSE;
            }

            /* TODO: The following is based on CDD3610mmc spec (table 23)
             *       values, other drives ??
             * dmi->writabilityType was initially set to MTYPE_WR_UNKNOWN
             */
            if(        senseMediumType >= 0x01
                    && senseMediumType <= 0x08 )            /* CD-ROM */
            {   dmi->writabilityType = MTYPE_WR_READONLY;
            }
            else if(   senseMediumType >= 0x10
                    && senseMediumType <= 0x18 )            /* CD-R */
            {   dmi->writabilityType = MTYPE_WR_WRITEONCE;
            }
            else if(   senseMediumType >= 0x20
                    && senseMediumType <= 0x28 )            /* CD-RW */
            {   dmi->writabilityType = MTYPE_WR_OVERWRITABLE;
            }
            fprintf(uctout,  "  Medium Type: %u (= 0x%02X) - %s\n",
                                senseMediumType, senseMediumType,
                                MTYPE_WR_TEXT(dmi->writabilityType));
            fflush(uctout);
        }

        ISSUECOMMANDbegin("READ DISC INFORMATION", IC_RETRY_NORMAL, ok, "\n");
        { /* IC_BODY() function assignment:
           */
          ok = scsiReadDiscInformation( scsiImpUse, &discStatus,
                            &stateLastSession, &isErasableBitSet,
                            &firstTrackOnDisc, &RDInrOfSessions,
                            &firstTrackLastSession,
                            &lastTrackLastSession,
                            &bgFormatStatus,
                            &discType );
        }
        ISSUECOMMANDend;

        if( !ok )
        {   fprintf(uctout, "  failed, skip read track info\n");
            if( maxReadableSector == 0 )
            {   fprintf(uctout, "-%s\n", scsiGetLastErrorMessage());
                return FALSE;
            }
            /* Here for e.g. DVD-RAM, that shows up as DTYPE_CDVD but on
             * some legacy drives cannot handle READ DISC INFORMATION command
             */
            dmi->lastValidBlockNr = dmi->lastRecordedBlockNr = maxReadableSector;
            break;  /* break from switch(perDeviceType), case DTYPE_CDVD */
        }

        switch(discStatus)
        {
        case DS_EMPTY:      txt = "Empty";
                            break;
        case DS_INCOMPLETE: txt = "Incomplete Disc (Appendable)";
                            break;
        case DS_COMPLETE:   txt = "Finalized Disc (last session closed)";
                            break;
        case DS_RESERVED:   txt = "Others (random access, no multiple sessions)";
                            break;
        default:            txt = "Illegal";
                            break;
        }
        fprintf(uctout,"  Disc Status             : %2u  --> %s\n",
                            discStatus, txt);

        switch( stateLastSession )
        {
        case SLS_EMPTY:      txt = "Empty session";
                             break;
        case SLS_INCOMPLETE: txt = "Incomplete session";
                             break;
        case SLS_RESERVED:   txt = "Reserved or Damaged session";
                             break;
        case SLS_COMPLETE:   txt = "Complete session";
                             break;
        default:             txt = "Illegal";
                             break;
        }
        fprintf(uctout,  "  State of last session   : %2u  --> %s\n",
                            stateLastSession, txt);

        fprintf(uctout,  "  Number of sessions      : %2u\n", RDInrOfSessions);
        fprintf(uctout,  "  First track on disc     : %2u\n", firstTrackOnDisc);
        fprintf(uctout,  "  First track last session: %2u\n", firstTrackLastSession);
        fprintf(uctout,  "  Last  track last session: %2u\n", lastTrackLastSession);

        if( bgFormatStatus != 0 )       /* MRW or DVD+RW, see MMC */
        { fprintf(uctout,"  Background format status: %2u  --> %s\n",
                       bgFormatStatus,
                      (bgFormatStatus == 1)     ? "Incomplete"
                    : (bgFormatStatus == 2)     ? "In progress"
                   /** bgFormatStatus == 3 **/  : "Complete");
        }
        dmi->bgFormatStatus = bgFormatStatus;

        switch( discType )
        {
        case 0x00:  if(   okGetConfig
                       && currentProfile != PROF_CDROM
                       && currentProfile != PROF_CDR
                       && currentProfile != PROF_CDRW )
                         txt = "no CD disc";
                    else txt = "CD-DA or CD-ROM disc";
                    break;
        case 0x10:  txt = "CD-I disc";
                    break;
        case 0x20:  txt = "CD-ROM XA disc";
                    break;
        case 0xff:  txt = "Undefined disc";
                    break;
        default:    txt = "Reserved value";
                    break;
        }
        fprintf(uctout, "  Disc Type               : %2xh --> %s\n",
                            discType, txt);
        fprintf(uctout, "  Erasable bit %sset"
                            "\t\t--> %sErasable medium present\n",
            (isErasableBitSet) ? "" : "NOT ",
            (isErasableBitSet) ? "" : "NO ");
        fflush(uctout);
        lastTrack = lastTrackLastSession;       /* default */

        if(   discStatus == DS_EMPTY                /* Empty disc */
           || firstTrackOnDisc != 1
           || firstTrackLastSession < firstTrackOnDisc
           || lastTrackLastSession  < firstTrackLastSession )
        {   /* Here if empty disc or inconsistent results
             * skip track info commands.
             */
            fprintf(uctout,
                "\n==> Empty disc or inconsistent"
                            " READ DISC INFORMATION result,\n"
                  "-   skip read track info.\n");
            if( maxReadableSector == 0 )
            {   fprintf(uctout, "-%s\n", scsiGetLastErrorMessage());
                return FALSE;
            }
            dmi->lastValidBlockNr = dmi->lastRecordedBlockNr = maxReadableSector;
            break;  /* break from switch(perDeviceType), case DTYPE_CDVD */
        }

        /* Initialize for session start and verifysession
         * detection and read track information for each track.
         * Actual session numbering is in theSession.
         * lastTrack may be zero for media that do not
         * have tracks/sessions (e.g. DVD-RAM) or only
         * one empty session.
         */
        theSession = lastSession = lastDataSession = 0;
        dmi->numberOfSessions = 0;
        for( theTrack = 1;
             theTrack <= lastTrack;
             theTrack++ )
        {   Uint32 nwa, lvbn;

            /* no trackNumber beyond 16 bits for READ TRACK INFO
             */
            if( theTrack != ((USHORT) theTrack) )
            { fprintf(uctout,
                "\n===>\tError: Track Number %lu does not fit in 16 bits.\n"
                "\n===>\t       Skip rest of tracks.\n", theTrack);
              fflush(uctout);
              break;        /* skip rest of tracks */
            }
            /* no "\n" yet, add session number at the end of line
             */
            ISSUECOMMANDbegin("READ TRACK INFORMATION", IC_RETRY_NORMAL, ok, "");
            { /* IC_BODY() function assignment:
               */
              if( ic_cnt == 0 )     /* first time */
              { fprintf(uctout," track %lu", theTrack);
              }
              ok = scsiReadTrackInformation( scsiImpUse, (USHORT) theTrack,
                        &driveSupportLen,       /* Data Length field included */
                        &rtiTrackNumber, &rtiSessionNumber,
                        &isDamageBitSet, &isCopyBitSet,  &isRTBitSet,
                        &isBlankBitSet,  &isPacketBitSet, &isFPBitSet,
                        &isNWA_VBitSet,  &isLRA_VBitSet,
                        &LJRS,           &trackMode,    &dataMode,
                        &fixedPacketSize,
                        &trackStartAddress, &trackLength,
                        &freeBlocks,
                        &nextWritableAddress, &lastRecordedAddress,
                        &nextLJaddress,      &lastLJaddress,
                        &tiRes04, &tiRes07, &tiRes34, &tiRes35 );
            }
            ISSUECOMMANDend;

            if( !ok )
            {   fprintf(uctout,"\n\nREAD TRACK INFORMATION failed, track %lu,"
                                            " skip rest of tracks\n%s\n",
                                theTrack, scsiGetLastErrorMessage());
                break;      /* skip rest of tracks */
            }
            fprintf(uctout, " in session %u :\n",rtiSessionNumber);

            /* print further READ TRACK INFORMATION data
             */
            fprintf(uctout,    "  Drive supports %2u bytes track info %s\n",
                     driveSupportLen, (driveSupportLen >= 48)
                        ? "\t (extended)" : "");
            if( driveSupportLen > READTRACKINFO_REPLY_LEN )
            { fprintf(uctout,  "=> Note: Only %lu of %lu bytes transferred !!!\n",
                        READTRACKINFO_REPLY_LEN, driveSupportLen);
            }

            /* special bits (if set) on one line
             */
            if(   isCopyBitSet   || isDamageBitSet || isFPBitSet
               || isPacketBitSet || isBlankBitSet  || isRTBitSet
               || isNWA_VBitSet  || isLRA_VBitSet )
            { fprintf(uctout,
                "  special track bits set     :%s%s%s%s%s%s%s%s\n",
                    (isCopyBitSet)   ? "  Copy"       : "",
                    (isDamageBitSet) ? "  Damage"     : "",
                    (isFPBitSet)     ? "  FP"         : "",
                    (isPacketBitSet) ? "  Packet/Inc" : "",
                    (isBlankBitSet)  ? "  Blank"      : "",
                    (isRTBitSet)     ? "  RT"         : "",
                    (isNWA_VBitSet)  ? "  NWA_V"      : "",
                    (isLRA_VBitSet)  ? "  LRA_V"      : "" );
            }

            if( LJRS )
            { fprintf(uctout, "  %-27s: %7u\n", "Layer Jump recording  LJRS",
                                    LJRS);
            }
            fprintf(uctout,   "  %-27s: %7u\n", "Track Mode", trackMode);
            fprintf(uctout,   "  %-27s: %7u\n", "Data Mode", dataMode);

            if(   (okDvdStruct    && fixedPacketSize == 16)
               || (okBdStruct     && fixedPacketSize == 32)
               || (isPacketBitSet && isFPBitSet && !isBlankBitSet) )
            { fprintf(uctout, "  %-27s: %7lu for %s\n",
                        (okDvdStruct || okBdStruct)
                            ? "(ECC) Blocking Factor"
                            : "Fixed Packet Size",
                        fixedPacketSize,
                        (okGetConfig) ? PROF_TEXT(currentProfile)
                      : (okDvdStruct) ? "DVD"
                      : (okBdStruct)  ? "BD" : "??" );
            }
            else        /* fixedPacketSize invalid */
            { fixedPacketSize = 0;  /* ignore for now, maybe set later */
            }
            fprintf(uctout,   "  %-27s: %7u\n", "Track Start Address",
                        trackStartAddress);

            /* Layer Jump recording for DVD-R dual layer (Mt. Fuji)
             */
            if( LJRS != 0 )
            { /* LJ: trackLength field holds last sector in track
               *     calculate and show trackLength
               */
              fprintf(uctout,"  Track End Address     (LJ) : %7lu", trackLength);
              trackLength = 1 + trackLength - trackStartAddress;    /* recalculate */
              fprintf(uctout,           "\t(Track Length: %7lu)\n", trackLength);
              fprintf(uctout,"  Next Layer Jump Address    : %7lu\n", nextLJaddress);
              fprintf(uctout,"  Last Layer Jump Address    : %7lu\n", lastLJaddress);
            }
            else    /* no LJ: normal use */
            { fprintf(uctout,"  Track Length (logical sect): %7lu\t\t( end: %7lu)\n",
                      trackLength, trackStartAddress + trackLength - 1);
            }

            fprintf(uctout,  "  Free Blocks                : %7lu\t\t(used: %7lu)\n",
                    freeBlocks, (trackLength > freeBlocks)
                                    ? (trackLength - freeBlocks) : 0);

            /* show LRA and NWA.
             * assert: set LRA to 0 if LRA_V not set
             *     and set NWA to 0 if NWA_V not set
             */
            if( isLRA_VBitSet || lastRecordedAddress != 0 )
            { fprintf(uctout,"  Last Recorded Address      : %7lu%s\n",
                      lastRecordedAddress,
                      (lastRecordedAddress != 0 && !isLRA_VBitSet)
                        ? "\t=> ignored, LRA_V bit not set"
                        : "" );
              if( !isLRA_VBitSet )
              { lastRecordedAddress = 0;    /* assert LRA/LRA_V consistency */
              }
            }
            if( isNWA_VBitSet || nextWritableAddress != 0 )
            { fprintf(uctout,"  Next Writable Address      : %7lu%s\n",
                      nextWritableAddress,
                      (nextWritableAddress != 0 && !isNWA_VBitSet)
                        ? "\t=> ignored, NWA_V bit not set"
                        : "" );
              if( !isNWA_VBitSet )
              { nextWritableAddress = 0;    /* assert NWA/NWA_V consistency */
              }
            }

            /* warn if reserved fields set, maybe new definitions.
             */
            if( tiRes04 || tiRes07 || tiRes34 || tiRes35 )
            { fprintf(uctout, "  Note: Reserved fields set  :"
                "    0x%02X 0x%02X 0x%02X 0x%02X \t (bytes 04,07,34,35)\n",
                    tiRes04, tiRes07, tiRes34, tiRes35 );
            }

            /***** End of show RTI data, check consistency, etc.
             * Most trusted values are Track Start, Track Length and Free Blocks,
             * while other fields validity often depends on bit settings, etc.
             * Check these bits later and give a warning if not appropriate.
             */

            /* test track and session number consistency
             */
            if( rtiTrackNumber != theTrack )
            { MLIMITbegin(ERROR00level,uctMessageLimit);
                VERBOSE00(uctout,
                    "=> Error: Inconsistent Track Number: %lu, expected: %lu.\n"
                  "-\t" "  Drive problem ?? Try a different drive.\n"
                  "-\t" "  Please report if this is a legal situation.\n"
                  "-  The verifier will assume that this is track number %lu.\n",
                                rtiTrackNumber, theTrack, theTrack);
                fflush(uctout);
              MLIMITend;
//**testing   return FALSE;     /* inconsistent rtiTrackNumber */
            }
            if(       rtiSessionNumber == 0
               ||     rtiSessionNumber > RDInrOfSessions
               || (   rtiSessionNumber !=  lastSession
                   && rtiSessionNumber != (lastSession + 1)) )
            { VERBOSE00(uctout,
                "=> Warning: Inconsistent Session Number: %lu, expected: %lu",
                                rtiSessionNumber,
                            (lastSession==0) ? 1 : lastSession);
              if(   lastSession != 0
                 && lastSession != RDInrOfSessions )
              { VERBOSE00(uctout, " or %lu", lastSession + 1);
              }
              VERBOSE00(uctout, ". Drive problem ??\n"
                      "-\t    Overrule session info using command"
                                " line options, see -help info.\n"
                      "-\t    The verifier will for now generate"
                                " its own session numbering.\n");
              fflush(uctout);
//            return FALSE;     /** TODO: check **/
            }

            /* check blank, NWA_V, etc bits now.
             */
//          if( isBlankBitSet && freeBlocks != trackLength )
//          { freeBlocks = trackLength;
//            fprintf(uctout,
//              "=> Note: Forced Free Blocks to %lu because blank bit set\n",
//              freeBlocks);
//          }

            /* note for non-contiguous tracks
             */
            if(   theTrack > 1
               && trackStartAddress != startNextTrack )
            { VERBOSE00(uctout,
                "=> Note: Non-contiguous track:\n"
                     "-\tend of previous track: %7lu\n"
                     "-\tstart of this track  : %7lu\n",
                    startNextTrack-1, trackStartAddress);
              fflush(uctout);
            }
            startNextTrack = trackStartAddress + trackLength;

            /* Check for new session start, also for session 1,
             * so theSession and lastSession are initialized to 0
             */
            if( theSession == 0 || rtiSessionNumber != lastSession )
            {   theSession++;           /* verifier own session count */
                lastSession = rtiSessionNumber;
                addSessionToMediumInfo(dmi, trackStartAddress);
                fprintf(uctout, "=> session %lu", theSession);
                if( theSession != rtiSessionNumber )
                { fprintf(uctout, " (%lu)", rtiSessionNumber);
                }
                fprintf(uctout, ", start at %lu\n", trackStartAddress);
            }

            /** check lastrecorded block consistency
             **/
            if(   isLRA_VBitSet
               && lastRecordedAddress >
                  (trackStartAddress + trackLength - freeBlocks -1) )
            {   fprintf(uctout,
                        "=> inconsistent track Last Recorded Address %lu, ",
                            lastRecordedAddress);
                lastRecordedAddress = trackStartAddress + trackLength - freeBlocks -1;
                if( lastRecordedAddress < trackStartAddress )
                { lastRecordedAddress = 0;      /* ignore */
                  fprintf(uctout, "ignore.\n");
                }
                else
                { fprintf(uctout, "reduce to %lu\n", lastRecordedAddress);
                }
            }

            /* check further track info consistency, etc.
             * Ignore Data Mode (note if not 1 or 2).
             * Special case for DVD-R DL Layer Jump recording
             */
            if(   LJRS != 0         /* Layer Jump recording */
               && dmi->lastValidBlockNr != 0
               && dmi->lastValidBlockNr >= trackStartAddress
               && freeBlocks != trackLength )   /* no empty track */
            {   fprintf(uctout,
                    "=> Layer Jump track start in previous track.\n");
            }
            else if(   (   dmi->lastValidBlockNr != 0
                        && dmi->lastValidBlockNr >= trackStartAddress)
                    || freeBlocks >= trackLength )
            {   fprintf(uctout, "=> ignore track information: ");
                if( freeBlocks == trackLength )
                    fprintf(uctout, "Empty track %lu.\n", theTrack);
                else if(   dmi->lastValidBlockNr != 0
                        && dmi->lastValidBlockNr >= trackStartAddress)
                    fprintf(uctout, "Track start error,\n"
                      "-  Track Start Address in previous track.\n");
                else if( freeBlocks > trackLength )
                    fprintf(uctout, "Track error,\n"
                      "-  Track Length less than Free Blocks.\n");
                else    /* assert */
                {   fprintf(uctout,"Undefined error, please report\n");
                }
                /* further ignore this track but nevertheless register
                 * track in device mediumInfo. May be used to detect
                 * unrecorded gaps and avoid reading there.
                 */
                if( lastRecordedAddress == 0 && nextWritableAddress > 0 )
                {   lastRecordedAddress = nextWritableAddress - 1;
                }
                addTrackToMediumInfo(dmi,
                    (Uint16) theTrack, rtiSessionNumber,
                    trackStartAddress, trackLength,
                    (   lastRecordedAddress == 0
                     || lastRecordedAddress < trackStartAddress) ? 0
                    : (1 + lastRecordedAddress - trackStartAddress));
                continue;   /* further ignore this track */
            }
            else if( dataMode != 1 && dataMode != 2 )
            {   fprintf(uctout,"=> track information note: "
                    "Data Mode %u (not 1 or 2)\n", dataMode);
            }

            /* Non-empty track, assume data session.
             * blocks recorded in this track.
             * check if new data session
             */
            if( lastDataSession != lastSession )
            {   lastDataSession = lastSession;
                dmi->numberOfSessions = theSession; /* last session not empty */
                fprintf(uctout, "=> non-empty %ssession: %lu",
                        (dataMode == 1 || dataMode == 2)
                            ? "data " : "", theSession);
                if( theSession != lastDataSession )
                { fprintf(uctout, " (%lu)", lastDataSession);
                }
                fprintf(uctout, "\n");
            }

            /* Calculate lastRecordedAddress if LRA_V bit
             * not set or if lastRecordedAddress is zero
             */
            if( !isLRA_VBitSet || lastRecordedAddress == 0 )
            {   /* No valid Last Recorded Address  (LRA).
                 * Assume that LRA is equal to: (NWA - 1).
                 * Next Writable Address (NWA) is valid if NWA_V
                 * bit set. Check NWA consistency:
                 * nextWritableAddress == trackStartAddress
                 *                      + trackLength - freeBlocks.
                 * Often this is not true, because NWA is invalid
                 * or inconsistent, in that case recalculate NWA.
                 */
                if( isLRA_VBitSet )
                { fprintf(uctout, "=> Ignore Last Recorded Address zero\n");
                }
                nwa = trackStartAddress + trackLength - freeBlocks;
                if(   !isNWA_VBitSet                /* invalid */
                   ||  nextWritableAddress != nwa ) /* inconsistent */
                {   fprintf(uctout,
                      "=> %s Next Writable Address, recalculate: %lu\n",
                        isNWA_VBitSet ? "inconsistent" : "invalid", nwa);
                    nextWritableAddress = nwa;
                }
                if( nextWritableAddress > 0 )
                {   lastRecordedAddress = nextWritableAddress - 1;
                }
            }

            /* adapt last valid and last recorded block numbers
             * show last recorded block adaptation only if different
             * from last valid block adaptation
             */
            lvbn = trackStartAddress + trackLength - 1;
            lastRecordedAddress = MIN(lvbn, lastRecordedAddress);

            if( lvbn > dmi->lastValidBlockNr )
            { fprintf(uctout, "=> adapt last    valid block number: %lu -> %lu\n",
                    dmi->lastValidBlockNr, lvbn);
            }
            if( lastRecordedAddress > dmi->lastRecordedBlockNr
               && (   dmi->lastRecordedBlockNr != dmi->lastValidBlockNr
                   || lastRecordedAddress != lvbn) )
            { fprintf(uctout, "=> adapt last recorded block number: %lu -> %lu\n",
                    dmi->lastRecordedBlockNr, lastRecordedAddress);
            }
            dmi->lastValidBlockNr    = MAX(lvbn, dmi->lastValidBlockNr);
            dmi->lastRecordedBlockNr = MAX(lastRecordedAddress,
                                           dmi->lastRecordedBlockNr);

            /* check fixed packet length
             */
            if( fixedPacketSize != 0 )
            {   dmi->eccLength = fixedPacketSize;
            }

            /* register track in device mediumInfo. May be used
             * to detect unrecorded gaps and avoid reading there.
             */
            addTrackToMediumInfo(dmi,
                (Uint16) theTrack, rtiSessionNumber,
                trackStartAddress, trackLength,
                (lastRecordedAddress == 0 ) ? 0
                : (1 + lastRecordedAddress - trackStartAddress));
        }   /* endfor theTrack */
        fprintf(uctout, "\n");

//      if(   theSession != lastSession )            /* assert */
////    if(   lastSession < lastDataSession          /* assert */
////       || dmi->numberOfSessions != lastSession ) /* assert */
//      {   fprintf(uctout,
//              "=> Fatal error: Inconsistent session info,\n"
//                         "-\t\tnumber of sessions: %lu\n"
//                         "-\t\tlast session      : %lu\n"
//                         "-\t\tlast data session : %lu\n"
//              "=> Drive problem, overrule drive info"
//                                  " using command line options\n",
//              dmi->numberOfSessions, lastSession, lastDataSession);
//          fflush(uctout);
//          return FALSE;       /** TODO: check **/
//      }

        /* If maxReadableSector is non-zero, then take
         * its value as last valid block value
         * in these two cases:
         * 1) lastValidBlockNr is zero
         * 2) last session is closed AND maxReadableSector
         *    is less than lastValidBlockNr
         */
        if(   maxReadableSector != 0
           && (   dmi->lastValidBlockNr == 0
               || (   maxReadableSector < dmi->lastValidBlockNr
                   && (         discStatus == DS_COMPLETE
                       || stateLastSession == SLS_COMPLETE)) ) )
        {   if( dmi->lastValidBlockNr == 0 )
            { fprintf(uctout,
                "=> last valid block zero, use READ CAPACITY result.\n");
            }
            else
            { fprintf(uctout,
                "=> medium inconsistency: Mismatch between READ CAPACITY\n"
                "-    result (%lu) and highest sector address in any track\n"
                "-    (%lu) for a complete disc or a complete last session.\n",
                    maxReadableSector, dmi->lastValidBlockNr);
            }
            fprintf(uctout,
                "=> adapt last valid block number: %lu -> %lu\n",
                    dmi->lastValidBlockNr, maxReadableSector);
            dmi->lastValidBlockNr = dmi->lastRecordedBlockNr = maxReadableSector;
            if( dmi->numberOfSessions == 0 )
            {   addSessionToMediumInfo(dmi, 0);
                theSession = lastSession = lastDataSession = 1;
            }
            /* maybe shrink last verify session
             */
            while(   dmi->numberOfSessions > 1
                  && dmi->sessionStartBlocks[dmi->numberOfSessions-1]
                     > dmi->lastValidBlockNr )
            {   dmi->numberOfSessions--;
            }
        }

        /* ignore trailing empty, non-data or inconsistent sessions
         */
        if( dmi->numberOfSessions != lastDataSession )
        {   fprintf(uctout,
              "=> ignored %lu trailing empty, non-data or inconsistent session%s\n",
                         dmi->numberOfSessions - lastDataSession,
                PLURAL_S(dmi->numberOfSessions - lastDataSession));
            dmi->numberOfSessions = lastDataSession;
        }

        /* last tests
         */
        if(   dmi->numberOfSessions == 0    /* no data session found */
           || dmi->lastValidBlockNr == 0 )
        {   fprintf(uctout, "=> no data session on disc\n");
            fflush(uctout);
            return FALSE;
        }
        dmi->verifySession = dmi->numberOfSessions;
        fprintf(uctout, "=> default verify session: %lu\n",
                                    dmi->verifySession);

        break;  /* end of case deviceType DTYPE_CDVD */

    }       /* end switch on deviceType */

    /* set default Packet Length (nmb of sectors in 'ECC' block)
     */
    if( dmi->eccLength == 0 )       /* not set yet */
    { dmi->eccLength = 1;           /* default ECC blocking factor */
      if( okDvdStruct )
      { dmi->eccLength = 16;            /* DVD */
      }
      else if( okBdStruct )
      { dmi->eccLength = 32;            /* BD */
      }
      else if( okGetConfig == TRUE )
      { switch( currentProfile )
        {
          case PROF_DVDROM:             /* DVD-ROM     */
          case PROF_DVDmR:              /* DVD-R       */
          case PROF_DVDmRdlsr:          /* DVD-R DL SR */
          case PROF_DVDmRdllj:          /* DVD-R DL LJ */
          case PROF_DVDRAM:             /* DVD-RAM     */
          case PROF_DVDmRWro:           /* DVD-RW RO  */
          case PROF_DVDmRWsr:           /* DVD-RW SR */
          case PROF_DVDpRW:             /* DVD+RW    */
          case PROF_DVDpRWdl:           /* DVD+RW DL */
          case PROF_DVDpR:              /* DVD+R     */
          case PROF_DVDpRdl:            /* DVD+R DL  */
            dmi->eccLength = 16;        /* DVD       */
            break;
          case PROF_CDRW:               /*  CD-RW   */
          case PROF_BDROM:              /*  BD-ROM   */
          case PROF_BDRsrm:             /*  BD-R SR   */
          case PROF_BDRrrm:             /*  BD-R RR    */
          case PROF_BDRE:               /*  BD-RE       */
          case PROF_HDDVDROM:           /*  HD DVD-ROM   */
          case PROF_HDDVDR:             /*  HD DVD-R      */
          case PROF_HDDVDRAM:           /*  HD DVD-RAM     */
            dmi->eccLength = 32;        /* (DD)CD/BD/HD DVD */
            break;
            /* TODO: don't know for: PROF_CDROM, PROF_CDR.
             */
        }
      }
      if( dmi->eccLength != 0 )     /* set to some value */
      { fprintf(uctout, "=> Undefined %s, set to %lu for %s\n",
                    (okDvdStruct || okBdStruct)
                        ? "(ECC) Blocking Factor"
                        : "Fixed Packet Size",
                    dmi->eccLength,
                    (okGetConfig) ? PROF_TEXT(currentProfile)
                  : (okDvdStruct) ? "DVD"
                  : (okBdStruct)  ? "BD" : "??" );
      }
    }

    if( dmi->lastValidBlockNr == 0 )
    {   fprintf(uctout, "\nEmpty medium\n");
    }
    fflush(uctout);

    return (dmi->lastValidBlockNr != 0);

}   /* end scsiStartDevice() */


static Device *createScsiDevice(char       *devArch,
                                MediumInfo *clMediumInfo)
{
    Device      *device;
    ScsiImpUse  *scsiImpUse;
    char        *tmpstr;

    scsiErrorOut = uctout;  /* low level logging on */

    /* Create Device structure.
     * Use tst_calloc() so that all members that might
     * not be initialized are 0, NULL or FALSE.
     */
    if(         (device = (Device*) tst_calloc(sizeof(Device), 1,
                                        __FILE__,__LINE__)) == NULL
       || (device->impUse = (void*) tst_calloc(sizeof(ScsiImpUse), 1,
                                        __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    scsiImpUse = (ScsiImpUse *) device->impUse;

    clearMediumInfo( &device->mediumInfo );

    /* devArch holds the -scsi option argument,
     *  either: <h>:,s>
     *      or: <device path>  (e.g. /dev/sg0 for Unix)
     */
    scsiImpUse->scsiId  = -1;
    scsiImpUse->hostAdapter = strtol(devArch, &tmpstr, 10);
    if(    tmpstr == devArch
       || *tmpstr != ':'
       ||  scsiImpUse->hostAdapter < 0 )
    {
        scsiImpUse->hostAdapter = -1;           /* invalid */
    }
    else
    { scsiImpUse->scsiId = strtol(tmpstr+1, &tmpstr, 10);
      if(   *(tmpstr-1) == ':'
         || *tmpstr != '\0'
         ||  scsiImpUse->scsiId < 0 )
      {
        scsiImpUse->scsiId  = -1;               /* invalid */
      }
      else
      { devArch = NULL;     /* <h>:<s> is ok, no device path */
      }
    }
    scsiImpUse->fd = -1;    /* not open yet */

    /* start scsi device, fill mediumInfo.
     * if devArch != NULL
     *  then: use <device path> in devArch in order to find <h>:<s>
     *  else: use <h>:<s> in scsiImpUse.
     */
    if( !scsiStartDevice(device, devArch,
                         clMediumInfo->lastValidBlockNr) )
    { if( clMediumInfo->lastValidBlockNr == 0 )
      { scsiCloseAndFreeImpUse(device->impUse);
        free(device);
        return NULL;    /* error and no -lastvalidblock specified */
      }
      /* scsiStartDevice() error, but last valid block number
       * specified in command line option, try to use it.
       * Flag this as a warning !!
       */
      MLIMITbegin(WARN01level,uctMessageLimit);
        fprintf(uctout,
          "=> Warning: Could not correctly initialize device, trying to\n"
               "-\t    use command line option last valid block: %lu.\n",
                        clMediumInfo->lastValidBlockNr);
        fflush(uctout);
      MLIMITend;
    }

    /* merge imageImpUse mediumInfo and command line mediumInfo
     * result to imageImpUse mediumInfo.
     */
    if( mergeMediumInfo( clMediumInfo, &device->mediumInfo,
                        &device->mediumInfo,
                        "command line", "SCSI device") )
    {   fprintf(uctout, "Warning: Overruling of device info may"
                            " cause problems in some cases\n");
    }
    scsiInitializeFunctionTable(device);

    fprintf(uctout,
        "\nCreated SCSI device\n"
          "  host adapter id\t: %u\n"
          "  scsi id\t\t: %u\n",
            scsiImpUse->hostAdapter, scsiImpUse->scsiId);

#ifndef SCSI_TESTING
    scsiErrorOut = NULL;    /* normaly suppress low level logging */
#endif
    return device;

}   /* end createScsiDevice() */

/*  device select function for scsi device:
 */
extern bool selectScsiDevice(int argc, char **argv, char *keyOption,
                             MediumInfo *clMediumInfo, Device **pDevice)
{
    char *devArch;
    int   index;

    *pDevice = NULL;        /* no device created */

    /* check device keyOption
     */
    if( (index = clFindArgument(argc, argv, keyOption)) == 0 )
    {
        return FALSE;       /* keyOption not found */
    }

    /* Found <keyOption> at argv[index], so return value will
     * be TRUE beyond this point.
     * Check key option argument: <h>:<s>
     */
    argv[index++] = NULL;   /* mark out as used, go to next argument */
    fprintf(uctout,"\t%s", keyOption);
    if( argv[index] == NULL || argv[index][0] == '-' )
    {
        fprintf(uctout, "\n\nOption argument missing for: %s\n", keyOption);
        return TRUE;
    }
    fprintf(uctout," %s\n", argv[index]);

    devArch     = argv[index];  /* <h>:<s> or <device path> */
    argv[index] = NULL;         /* mark out as used         */
    fprintf(uctout,"\n");       /* end of options parsing   */

    if( clCountValidArguments(argc, argv) == 0 )
    {
        /* no errors so far and no unknown arguments left,
         * so let's try to create a scsi device.
         */
        *pDevice = createScsiDevice(devArch, clMediumInfo);
    }
    return TRUE;
}

/* Check and execute showscsi option.
 * If -showscsi found, execute AND QUIT.
 * Note: The arugument list conforms to a device select function,
 *       so this function can be used in a device select table.
 */
extern bool checkShowScsi(int argc, char **argv, char *Option,
                          MediumInfo *miDummy, Device **devDummy)
{
    miDummy  = miDummy;     /* make compiler happy */
    devDummy = devDummy;    /* make compiler happy */

    if( clFindArgument(argc, argv, Option) != 0 )
    {   /* found -showscsi, execute and quit */
        fprintf(uctout,"\t%s\n\n", Option);
        showScsiInfo();     /* execute*/
        uctExit(EXIT_OK);   /* ok, no UDF verification */
    }
    return FALSE;           /* no showscsi option found */
}

#endif  /** SCSI_DEVICE **/

