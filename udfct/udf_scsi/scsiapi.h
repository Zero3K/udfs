/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : scsiapi.h
 *
 * Description :
 *
 * Decisions   :
 *
 * History     : 19980104: Christ Vriens, creation (win32).
 *               19970618: Erik Niessen, Kees Teunissen, creation (Linux)
 *               20000630: Alex Sinitsyn, Gerrit Scholl, integrate Win32 and Linux
 */

#ifndef __SCSIAPI_H__
#define __SCSIAPI_H__

#ifdef __cplusplus
extern "C" {
#endif


/**** SCSI_DEVICE implemented for WIN32, LINUX and NETBSD so far.
 * Use NO_SCSI to disable SCSI_DEVICE, but further,
 * SCSI_DEVICE will be used internally instead of NO_SCSI !!!!!
 * For further device support details, see platform.h
 */
#if( (!defined WIN32) && (!defined LINUX) && (!defined NETBSD) )
#define NO_SCSI
#endif

#ifdef  NO_SCSI
#undef  SCSI_DEVICE         /* SCSI_DEVICE support disabled */
#else
#define SCSI_DEVICE         /* default for WIN32, etc. */
#endif

#undef  NO_SCSI            /* internally, only SCSI_DEVICE must be used */


#ifdef  SCSI_DEVICE         /* compile for SCSI device only */

/* SYSTEM MACRO DEFINITIONS */
#ifndef bool
#define bool    int
#define FALSE   0
#define TRUE    1
#endif

#ifndef BYTE
  typedef unsigned char BYTE;
#endif
#ifndef PBYTE
  typedef unsigned char *PBYTE;
#endif
#ifndef UCHAR
  typedef unsigned char UCHAR;
#endif
#ifndef USHORT
  typedef unsigned short USHORT;
#endif
#ifndef UINT
  typedef unsigned int UINT;
#endif
#ifndef ULONG
  typedef unsigned long ULONG;
#endif

/* INCLUDE FILES */
#include "scsidefs.h"

/* TYPE DEFINITIONS */

typedef struct
{   int hostAdapter;
    int scsiId;
    int fd;         /* to support SCSI on Unix-alikes */
} ScsiImpUse;

#undef  SCSI_USE_DLL        /* normally #undef (no DLL) */

#if defined SCSI_USE_DLL && defined WIN32

/* DLL functions MACRO DEFINITIONS
 */
#ifdef  DLLEXPORT           /* defined in scsiapi.c */
#define DLLFUN DLLEXPORT
#else
#define DLLFUN __declspec( dllimport )
#endif

#else   /* NOT (SCSI_USE_DLL && WIN32) */

#ifdef  DLLEXPORT       /* defined in scsiapi.c */
#undef  DLLEXPORT       /* cancel scsiapi.c definition */
#endif
#define DLLEXPORT
#define DLLFUN

#endif  /* SCSI_USE_DLL, WIN32 */

#undef  SCSI_TESTING            /* normally #undef */

/* GLOBAL VARIABLES */

/* FUNCTION DEFINITIONS */
extern void DLLFUN getScsiApiVersion( char  *saVersion,
                                      size_t sizeSaVersion );

extern bool DLLFUN aspiPrintHostAdapterInfo( UCHAR hostAdapterID,
                                             UCHAR *pMaxScsiTagets );
extern bool DLLFUN printDeviceType( ScsiImpUse *scsiImpUse, UCHAR deviceType );

extern bool DLLFUN testUnitReady( ScsiImpUse *scsiImpUse );

extern bool DLLFUN scsiInit();
extern bool DLLFUN scsiNrHostAdapters( int* nrHostAdapters );

extern bool DLLFUN scsiInquiry( ScsiImpUse *scsiImpUse,
                                BYTE  *pDeviceType,
                                BYTE  *pInqVersion,
                                char   vendorIdentification[8+1],
                                char   productIdentification[16+1],
                                char   productRevisionlevel[4+1],
                                USHORT versionDescriptors[8]);

extern bool DLLFUN scsiModeSense10( ScsiImpUse *scsiImpUse,
                                    BYTE* senseMediumType );

extern bool DLLFUN scsiReadDiscInformation(
                        ScsiImpUse *scsiImpUse,
                        BYTE   *discStatus,
                        BYTE   *stateLastSession,
                        bool   *pIsErasableBitSet,
                        BYTE   *firstTrackOnDisc,
                        USHORT *nrOfSessions,
                        USHORT *firstTrackLastSession,
                        USHORT *lastTrackLastSession,
                        BYTE   *bgFormatStatus,
                        BYTE   *discType );

/* GET CONFIGURATION command.
 * MMC-4 6.6 (mmc4r02h draft, February 20, 2004)
 * currentProfile : Profile Number, see 5.3.1, Table 75
 */
extern bool DLLFUN scsiGetConfiguration( ScsiImpUse *scsiImpUse,
                            USHORT  *currentProfile );

/* READ DVD STRUCTURE Format 00h command.
 * MMC-5 6.29, 6.29.2.1, February 1, 2005
 */
extern bool DLLFUN scsiReadDvdStructure( ScsiImpUse *scsiImpUse,
                            BYTE  *pBookType, BYTE *pPartVersion,
                            BYTE  *pDiscSize, BYTE *pMaxRate,
                            BYTE  *pNmbOfLayers,
                            BYTE  *pTrackPathOTP,   /* 0: PTP, 1: OTP */
                            ULONG *pStartPsnDataArea,
                            ULONG *pEndPsnDataArea,
                            ULONG *pEndPsnDataL0 );

/* READ DVD STRUCTURE Format 20h command.
 * MMC-5 6.29.2.19, February 1, 2005
 */
extern bool DLLFUN scsiReadDvdStructF20( ScsiImpUse *scsiImpUse,
                                         ULONG *pL0DataZoneCapacity);

/* This function is shared for:
 *  Direct Access devices like HDD, some MO, etc.
 *  MM devices like CD, DVD, MO
 *
 * see MMC-4 6.23, READ CAPACITY command
 *  (in older specs, this command
 *   was called: READ CD RECORDED CAPACITY).
 */
extern bool DLLFUN scsiReadCapacity(ScsiImpUse *scsiImpUse,
                                    ULONG      *lastValidSector,
                                    ULONG      *sectorLength);

/* SCSI Multimedia Commands - 3 5.24 (MMC-3),
 * draft revision 10g, November 12, 2001.
 * Special CD/DVD commands including LRA extension.
 */
extern bool DLLFUN scsiReadTrackInformation(
    ScsiImpUse *scsiImpUse,     USHORT  reqTrack,
    USHORT  *driveSupportTotalLength,
    USHORT  *trackNumber,       USHORT *sessionNumber,
    bool    *pIsDamageBitSet,   bool   *pIsCopyBitSet,
    bool    *pIsRTBitSet,       bool   *pIsBlankBitSet,
    bool    *pIsPacketBitSet,   bool   *pIsFPBitSet,
    bool    *pIsNWA_VBitSet,    bool   *pIsLRA_VBitSet,
    BYTE    *pLJRS,
    BYTE    *trackMode,         BYTE   *dataMode,
    ULONG   *fixedPacketSize,
    ULONG   *trackStartAddress, ULONG  *trackLength,
    ULONG   *freeBlocks,
    ULONG   *nextWritableAddress, ULONG *lastRecordedAddress,
    ULONG   *nextLayerJumpAddress, ULONG *lastLayerJumpAddress,
    BYTE    *reserved04,        BYTE *reserved07,
    BYTE    *reserved34,        BYTE *reserved35 );

extern bool DLLFUN scsiRead(ScsiImpUse *scsiImpUse,
                            ULONG sectorLength, ULONG sector,
                            ULONG nrSectors, PBYTE pbMem);

extern const char DLLFUN *scsiGetLastErrorMessage();


#endif  /** SCSI_DEVICE **/     /* compiled for SCSI device only */

#ifdef __cplusplus
}
#endif

#endif /* __SCSIAPI_H__ */

