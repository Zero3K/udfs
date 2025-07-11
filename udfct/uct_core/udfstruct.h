/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : udfstruct.h
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __UCT_UDFSTRUCT_H__
#define __UCT_UDFSTRUCT_H__

#include "mytypes.h"

/* set the structure packing to one
 */
#ifdef __GNUC__
#define PACKED __attribute((aligned(1),packed))
#else
#define PACKED
#pragma pack(1)
#endif

/* ECMA Part 1: General ***********************************************
 */

/* UDF 2.1.2, OSTA CS0 Charspec field values.
 * Mind that all 63 bytes of CharacterSetInfo[63]
 * are specified (last char is extra \0 termination ).
 */
#define OSTA_CS0_CHARSETTYPE    0
#define OSTA_CS0_CHARSETINFO    \
    "OSTA Compressed Unicode\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0" \
    "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0" /* 40 + 22 + 1 */

typedef struct              /* ECMA 1/7.2 */
{
    Uint8       characterSetType PACKED;
    Byte        characterSetInfo[63] PACKED;
} Charspec;

typedef Byte    Dchars;     /* ECMA 1/7.2,   UDF 2.1.1 */
typedef Byte    Dstring;    /* ECMA 1/7.2.12 UDF 2.1.3 */

typedef struct              /* ECMA 1/7.3 */
{
    Uint16      TypeAndTimezone PACKED;
    Int16       Year PACKED;            /* NOT Uint16 !! */
    Uint8       Month PACKED;
    Uint8       Day PACKED;
    Uint8       Hour PACKED;
    Uint8       Minute PACKED;
    Uint8       Second PACKED;
    Uint8       Centiseconds PACKED;
    Uint8       HundredsofMicroseconds PACKED;
    Uint8       Microseconds PACKED;
} Timestamp;

/* TypeAndTimezone macros,
 * TIMEZONE_TYPE1TXT(TZ) assumes Type 1
 */
#define TIMEZONE_UNDEFINED ((Int16)-2047)
#define GET_TIMEZONE(pTS)  ((Int16)(((Int16)((pTS)->TypeAndTimezone<<4))>>4))
#define GET_TIMETYPE(pTS)  ((Uint8)((pTS)->TypeAndTimezone>>12))
#define TIMEZONE_TYPE1TXT(TZ)   \
    (  ((TZ) == TIMEZONE_UNDEFINED) ? "timezone undefined" \
     : ((TZ) == 0)                 ? "equal to UTC" \
     : ((TZ) >  0)                ? "east of UTC" \
                                 : "west of UTC")


/* EntityId: (== ECMA: regid !!)
 */
#define ENTITYID_IDSIZE     23      /* EntityId Identifier size */

/* The following \0 terminated EntityId Identifier constant strings must
 * have at least ENTITYID_IDSIZE characters (one invisible terminating
 * \0 character included). Pad with \0 chars if they are too short.
 * Comparing with actual EntityId Identifiers can be done using
 * memcmp(x,y,ENTITYID_IDSIZE), while the constant strings can also be
 * used for printing.
 */                                  /* mind one invisible \0 char at the end */
#define VIRTUAL_PARTITION_ID    "*UDF Virtual Partition\0"                  /* 22 +  1 */
#define SPARABLE_PARTITION_ID   "*UDF Sparable Partition"                   /* 23      */
#define METADATA_PARTITION_ID   "*UDF Metadata Partition"                   /* 23      */

#define VIRTUAL_ALLOC_TBL_ID    "*UDF Virtual Alloc Tbl\0"                  /* 22 +  1 */
#define SPARING_TABLE_ID        "*UDF Sparing Table\0\0\0\0\0"              /* 18 +  5 */

#define UDF_LVI_IUVD_ID         "*UDF LV Info\0\0\0\0\0\0\0\0\0\0\0"        /* 12 + 11 */
#define UDF_DOMAIN_ID           "*OSTA UDF Compliant\0\0\0\0"               /* 19 +  4 */

#define PARTCONTENTS_NSR02_ID   "+NSR02\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"  /*  6 + 17 */
#define PARTCONTENTS_NSR03_ID   "+NSR03\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"  /*  6 + 17 */

/* UDF EA Entity Id Identifiers
 * UDF 3.3.4.5+6, UDF 2.00 6.1
 *
 * Impl Use EA: UDF 1.02+
 */
#define UDFEA_FreeEASpace_ID        "*UDF FreeEASpace\0\0\0\0\0\0"  /* 16 +  7 * \0 */
#define UDFEA_DVD_CGMS_Info_ID      "*UDF DVD CGMS Info\0\0\0\0"    /* 18 +  5 * \0 */
#define UDFEA_OS2_EALength_ID       "*UDF OS/2 EALength\0\0\0\0"    /* 18 +  5 * \0 */
#define UDFEA_Mac_VolumeInfo_ID     "*UDF Mac VolumeInfo\0\0\0"     /* 19 +  4 * \0 */
#define UDFEA_Mac_FinderInfo_ID     "*UDF Mac FinderInfo\0\0\0"     /* 19 +  4 * \0 */
#define UDFEA_Mac_ResourceFork_ID   "*UDF Mac ResourceFork\0"       /* 21 +  2 * \0 */

/* Impl Use EA: UDF 1.02 and 1.50, stream in UDF 2.00+
 */
#define UDFEA_OS2_EA_ID             "*UDF OS/2 EA\0\0\0\0\0\0\0\0\0\0" /* 12 + 11 * \0 */
#define UDFEA_Mac_UniqueIDTable_ID  "*UDF Mac UniqueIDTable"          /* 22 +  1 * \0 */

/* Impl Use EA: UDF 1.50, errata only !!
 */
#define UDFEA_VAT_LVExtension_ID    "*UDF VAT LVExtension\0\0"      /* 20 +  3 * \0 */

/* Impl Use EA: UDF 2.01+
 */
#define UDFEA_OS400_DirInfo_ID      "*UDF OS/400 DirInfo\0\0\0"     /* 19 +  4 * \0 */

/* Appl Use EA: UDF 1.02+
 */
#define UDFEA_FreeAppEASpace_ID     "*UDF FreeAppEASpace\0\0\0"     /* 19 +  4 * \0 */


/* UDF Defined System and Non-System Stream names,
 * UDF 2.00+ 3.3.5.2, 3.3.7. 3.3.8
 * IMPLEMENTATION NOTE:
 *  Mind that these are stream names (not fixed-size EA Identifiers)
 *  that have to be converted to Unicode to compare with FID names.
 */
#define UDF_SS_UNIQMAPPING  "*UDF Unique ID Mapping Data"   /* 3.3.7 */
#define UDF_SS_NONALLOC     "*UDF Non-Allocatable Space"
#define UDF_SS_POWERCAL     "*UDF Power Cal Table"
#define UDF_SS_BACKUP       "*UDF Backup"

#define UDF_NSS_MACRESFORK  "*UDF Macintosh Resource Fork"  /* 3.3.8 */
#define UDF_NSS_OS2EA       "*UDF OS/2 EA"
#define UDF_NSS_NTACL       "*UDF NT ACL"
#define UDF_NSS_UNIXACL     "*UDF UNIX ACL"


/* Different layouts of IdentifierSuffix in EntityID (== regid in ECMA)
 *
 * DomainFlags bit definitions :
 * bits 2-7 are reserved,
 */
#define DOMAINFLAGS_HardWriteProtect    0x01    /* bit 0 */
#define DOMAINFLAGS_SoftWriteProtect    0x02    /* bit 1 */
#define DOMAINFLAGS_Reserved            0xFC    /* bit 2-7 */

/* OSCL_* OS Class definitions, see UDF 6.3 :
 * (mind also OSCI_* definitions below for OS Class/Id combination)
 */
#define OSCL_UNDEFINED  0   /* UDF 1.02:   Undefined    */
#define OSCL_DOS        1               /* DOS          */
#define OSCL_OS2        2               /* OS/2         */
#define OSCL_MACOS      3               /* Macintosh OS */
#define OSCL_UNIX       4               /* UNIX         */
#define OSCL_WIN9X      5   /* UDF 1.50:   Windows 9x   */
#define OSCL_WINNT      6               /* Windows NT   */
#define OSCL_OS400      7   /* UDF 2.01:   OS/400       */
#define OSCL_BEOS       8               /* BeOS         */
#define OSCL_WINCE      9               /* Windows CE   */
#define OSCL_MAX        9   /* UDF 2.01+, 10-255 reserved */

#define OSCL_TEXT(X)      \
    (((X)==OSCL_UNDEFINED) ? "Undefined"    :\
     ((X)==OSCL_DOS)      ? "DOS"           :\
     ((X)==OSCL_OS2)      ? "OS/2"          :\
     ((X)==OSCL_MACOS)    ? "Macintosh OS"  :\
     ((X)==OSCL_UNIX)     ? "UNIX"          :\
     ((X)==OSCL_WIN9X)    ? "Windows 9x"    :\
     ((X)==OSCL_WINNT)    ? "Windows NT"    :\
     ((X)==OSCL_OS400)    ? "OS/400"        :\
     ((X)==OSCL_BEOS)     ? "BeOS"          :\
     ((X)==OSCL_WINCE)    ? "Windows CE"    :\
                            "(reserved)")

/* OSCI_* OS Identifier definitions, see UDF 6.3 :
 * Values for combination of OS Class and OS Identifier in one Uint16,
 * most significant byte for OS Class, least significant byte for OS Id.
 * see UDF 6.3, 2nd table.
 * Introduction in UDF 1.02: OS Class 0-4 (not yet Mac OS X)
 */
#define OSCI_UNDEFINED      0x0000  /* Undefined */
#define OSCI_DOS            0x0100  /* DOS/Windows 3.x */
#define OSCI_OS2            0x0200  /* OS/2 */
#define OSCI_MACOS9         0x0300  /* Macintosh OS 9 and older */
#define OSCI_UNIX_GEN       0x0400  /* UNIX - generic */
#define OSCI_UNIX_IBMAIX    0x0401  /* UNIX - IBM AIX */
#define OSCI_UNIX_SUNOS     0x0402  /* UNIX - SUN OS / Solaris */
#define OSCI_UNIX_HPUX      0x0403  /* UNIX - HP/UX   */
#define OSCI_UNIX_SGI       0x0404  /* UNIX - Silicon Graphics Irix */

/* Introduction in UDF 1.50: new OS Class 4 additions and new Class 5-6
 */
#define OSCI_UNIX_LINUX     0x0405  /* UNIX - Linux   */
#define OSCI_UNIX_MKLIN     0x0406  /* UNIX - MKLinux */
#define OSCI_UNIX_FREEBSD   0x0407  /* UNIX - FreeBSD */

#define OSCI_WIN9X          0x0500  /* Windows 9x - generic */
#define OSCI_WINNT          0x0600  /* Windows NT - generic */

/* Introduction in UDF 2.01: new OS Class 7-9
 */
#define OSCI_OS400          0x0700  /* OS/400 - generic */
#define OSCI_BEOS           0x0800  /* BeOS - generic   */
#define OSCI_WINCE          0x0900  /* Windows CE - generic */

/* Introduction in UDF 2.50: new OS Id for Mac OS X
 */
#define OSCI_MACOSX         0x0301  /* Macintosh OS X and later */

/* Introduction in UDF 2.60: new OS Id for NetBSD
 */
#define OSCI_UNIX_NETBSD    0x0408  /* UNIX - NetBSD */

/* OS Class/Identifier OSCI_TEXT(X) text macro (UDF 6.3)
 * X is Uint16 with OS Class (MSbyte) and OS Id (LSbyte)
 */
#define OSCI_TEXT(X)    \
    ((((X) & 0xFF00)    \
         ==OSCI_UNDEFINED)      ? "Undefined"               :\
     ((X)==OSCI_DOS)            ? "DOS/Windows 3.x"         :\
     ((X)==OSCI_OS2)            ? "OS/2"                     :\
     ((X)==OSCI_MACOS9)         ? "Macintosh OS 9 and older" :\
     ((X)==OSCI_MACOSX)         ? "Macintosh OS X and later" :\
     ((X)==OSCI_UNIX_GEN)       ? "UNIX - generic"           :\
     ((X)==OSCI_UNIX_IBMAIX)    ? "UNIX - IBM AIX"            :\
     ((X)==OSCI_UNIX_SUNOS)     ? "UNIX - SUN OS / Solaris"    :\
     ((X)==OSCI_UNIX_HPUX)      ? "UNIX - HP/UX"                :\
     ((X)==OSCI_UNIX_SGI)       ? "UNIX - Silicon Graphics Irix" :\
     ((X)==OSCI_UNIX_LINUX)     ? "UNIX - Linux"         :\
     ((X)==OSCI_UNIX_MKLIN)     ? "UNIX - MKLinux"       :\
     ((X)==OSCI_UNIX_FREEBSD)   ? "UNIX - FreeBSD"       :\
     ((X)==OSCI_UNIX_NETBSD)    ? "UNIX - NetBSD"        :\
     ((X)==OSCI_WIN9X)          ? "Windows 9x - generic" :\
     ((X)==OSCI_WINNT)          ? "Windows NT - generic" :\
     ((X)==OSCI_OS400)          ? "OS/400"               :\
     ((X)==OSCI_BEOS)           ? "BeOS - generic"       :\
     ((X)==OSCI_WINCE)          ? "Windows CE - generic" :\
                                  "(reserved)")

/*** end OS Class/Id stuff
 */

typedef enum        /* EntityId Suffix types */
{
    ENTITY_SUFFIX_DOMAIN = 0,
    ENTITY_SUFFIX_UDF,
    ENTITY_SUFFIX_IMPL,
    ENTITY_SUFFIX_APPL      /* was ENTITY_SUFFIX_UNDEFINED for UDF 2.00- */
} ENTITY_SUFFIX_TYPE;

#define ENTITY_SUFFIX_UNDEFINED ENTITY_SUFFIX_APPL  /* see BOOT2 VRS */

typedef struct
{
    Uint16  udfRevision PACKED;
    Uint8   domainFlags PACKED;
    Byte    reserved[5] PACKED;
} DomainIdentifierSuffix;

typedef struct
{
    Uint16  udfRevision  PACKED;
    Uint8   osClass      PACKED;
    Uint8   osIdentifier PACKED;
    Byte    reserved[4] PACKED;
} UdfIdentifierSuffix;

typedef struct
{
    Uint8   osClass      PACKED;
    Uint8   osIdentifier PACKED;
    Byte    implementationUse[6] PACKED;
} ImplementationIdentifierSuffix;

typedef struct
{
    Byte    implementationUse[8] PACKED;
} ApplicationIdentifierSuffix;

typedef struct
{
    Uint8       Flags PACKED;
    Byte        Identifier[ENTITYID_IDSIZE] PACKED;
    union {
        Byte                            IdentifierSuffix[8];
        DomainIdentifierSuffix          domainSuffix;
        UdfIdentifierSuffix             udfSuffix;
        ImplementationIdentifierSuffix  implSuffix;
        ApplicationIdentifierSuffix     applSuffix;
    } idSuffix PACKED;
} EntityID;             /* this type is called: regid in ECMA !! */


/* Volume and boot block recognition, ECMA 2/8., 3/3.1 and 3/9.
 * Minimum nmb of bytes before the Volume Recognition Sequence
 * is determined by ECMA 2/8.3. Mind the byte numbering from 0 there,
 * so byte number 32767 is the 32768th byte.
 */
#define MIN_BYTES_BEFORE_VRS    32768   /* ECMA 2/8.3, 32767 + 1 */

/* GenericVolumeStructureDescriptor StandardIdentifier values :
 *
 * The following StandardIdentifier constant strings are \0 terminated
 * strings. Comparing with actual StandardIdentifier can be done using
 * memcmp() with 5 chars, while the constant strings can also be used
 * for printing, because they are \0 terminated.
 */
#define VRS_BEA01   "BEA01"     /* ECMA-167, 2/9.2  */
#define VRS_BOOT2   "BOOT2"     /* ECMA-167, 2/9.4  */
#define VRS_CD001   "CD001"     /* ECMA-119         */
#define VRS_CDW02   "CDW02"     /* ECMA-168         */
#define VRS_NSR02   "NSR02"     /* ECMA-167, 3/9.1 NOTE: ECMA-167 2nd edition !! */
#define VRS_NSR03   "NSR03"     /* ECMA-167, 3/9.1  */
#define VRS_TEA01   "TEA01"     /* ECMA-167, 2/9.3  */

typedef struct      /* ECMA 2/9.4 */
{
    Byte        Reserved01  PACKED;         /* #00 byte */
    EntityID    ArchitectureType PACKED;
    EntityID    BootIdentifier PACKED;
    Uint32      BootExtentLocation PACKED;
    Uint32      BootExtentLength PACKED;
    Uint64      LoadAddress PACKED;
    Uint64      StartAddress PACKED;
    Timestamp   DescriptorCreationDateAndTime PACKED;
    Uint16      Flags PACKED;
    Byte        Reserved02[32] PACKED;      /* #00 bytes */
    Byte        BootUse[1906] PACKED;
} BootDescriptorTail;


/* Generic Volume Structure Descriptor, ECMA 2/9.1, etc. and 3/9.1
 *
 * NOTE: StructureData[2041] as defined here according to
 *       ECMA 2/9.1 is the same as the "Reserved" +
 *       "StructureData[2040]" fields in the NSR descriptor
 *       as defined by ECMA 3/9.1
 */
#define VRS_VSD_SIZE  2048  /* VRS GenericVolumeStructureDescriptor size */

typedef struct
{
    Uint8                   StructureType PACKED;
    Byte                    StandardIdentifier[5] PACKED;
    Uint8                   StructureVersion PACKED;
    union {
        Byte                StructureData[2041];    /* see NOTE above */
        BootDescriptorTail  BootTail;               /* ECMA 2/9.4 */
    } VSDtail PACKED;
} GenericVolumeStructureDescriptor;


/* ECMA Part 3 : Volume Structure ****************************
 * UDF 2.00 2.2
 */

typedef struct      /* ECMA 3/7.1 */
{
    Uint32      extentLength PACKED;   /* 30 bits, specified in bytes */
    Uint32      extentLocation PACKED; /* Logical sector number. */
} ExtentAd;     /* ECMA: extent_ad */

/* ECMA Part 4 : File Structure ****************************
 * UDF 2.00 2.3
 */

/* ECMA 4/7.1
 */
typedef struct LBAddr LBAddr;   /* ECMA: lb_addr */
struct LBAddr
{
    Uint32      logicalBlockNumber PACKED;
    Uint16      partitionReferenceNumber PACKED;
} PACKED;


/* UDF 2.00 2.3.10.1 :
 * LongAd.implementationUse :
 * ADImpUse Flags :
 * bit 0 defined as EXTENTErased
 * bits 1-15 reserved for future use by UDF
 */
#define EXTENTErased (0x01)

typedef struct
{
    Uint16      flags PACKED;       /* ADImpUse */
    union {
        Byte    impUse[4];
        Uint32  UDFUniqueID; /* for UDF 2.00+, in FID ICB only */
    } ImpUse PACKED;
} ADImpUse;


/* macros and definitions for ECMA 4/14.14.1
 * extent type definitions ECMA 4/14.14.1.2
 */
#define ADEL_RECORDED_AND_ALLOCATED     ((Uint8) 0x00)
#define ADEL_NOT_RECORDED_BUT_ALLOCATED ((Uint8) 0x01)
#define ADEL_NOT_RECORDED_NOT_ALLOCATED ((Uint8) 0x02)
#define ADEL_EXTENTPOINTER              ((Uint8) 0x03)

#define TWO_POWER30     0x40000000
#define ADEL_MASK       0x3FFFFFFF

#define elGetExtentSize(LTH) ((LTH) & ADEL_MASK)
#define elGetExtentType(LTH) ((Uint8)(((LTH) >> 30) & 0x03))

#define adGetExtentSize(adPNT) elGetExtentSize((adPNT)->extentLength)
#define adGetExtentType(adPNT) elGetExtentType((adPNT)->extentLength)

#define makeExtentTypeSize(TYPE,SIZE) ((((TYPE)<<30)|(SIZE)))

/* AnyAd, common extentLength field for
 *        ShortAd, LongAd and ExtAd
 * (ECMA: short_ad, long_ad and ext_ad)
 *
 * extentLength:
 *  30 least significant bits hold extent byte size.
 *  2 most significant bits hold extent type.
 *  see ECMA 4/14.14.1
 */
typedef struct
{
    Uint32      extentLength PACKED;
} AnyAd;

typedef struct
{
    Uint32      extentLength PACKED;    /* see AnyAd.extentLength */
    Uint32      extentPosition PACKED;
} ShortAd;  /* ECMA: short_ad */

typedef struct
{
    Uint32      extentLength PACKED;    /* see AnyAd.extentLength */
    LBAddr      extentLocation PACKED;
    ADImpUse    implementationUse PACKED;
} LongAd;   /* ECMA: long_ad */

typedef struct      /* for completeness, unused in UDF */
{
    Uint32      extentLength PACKED;    /* see AnyAd.extentLength */
    Uint32      recordedLength PACKED;
    Uint32      informationLength PACKED;
    LBAddr      extentLocation PACKED;
    Byte        implementationUse[2] PACKED;
} ExtAd;    /* ECMA: ext_ad */

/* tag identifier definitions
 */
#define tidGroup01Start   0     /* 2 groups of conseq values */

#define tidST             0     /* UDF 2.50 2.2.12 */
#define tidPVD            1     /* ECMA 3/7.2.1 */
#define tidAVDP           2
#define tidVDP            3
#define tidIUVD           4
#define tidPD             5
#define tidLVD            6
#define tidUSD            7
#define tidTD             8
#define tidLVID           9

#define tidGroup01End     9
#define tidGroup02Start 256

#define tidFSD          256
#define tidFID          257
#define tidAED          258
#define tidIE           259
#define tidTE           260
#define tidFE           261
#define tidEAHD         262
#define tidUSE          263
#define tidSBD          264
#define tidPIE          265
#define tidEFE          266

#define tidGroup02End   266

#define tidICBDIRECTENTRY   (0xFFFE)
#define tidUNKNOWN          (0xFFFF)

/* Tag id text macros for printing, etc.
 * max 4 chars
 */
#define    tidTEXT4(X)  \
    (((X)==tidST)   ? "ST"      :\
     ((X)==tidPVD)  ? "PVD"     :\
     ((X)==tidAVDP) ? "AVDP"    :\
     ((X)==tidVDP)  ? "VDP"     :\
     ((X)==tidIUVD) ? "IUVD"    :\
     ((X)==tidPD)   ? "PD"      :\
     ((X)==tidLVD)  ? "LVD"     :\
     ((X)==tidUSD)  ? "USD"     :\
     ((X)==tidTD)   ? "TD"      :\
     ((X)==tidLVID) ? "LVID"    :\
     ((X)==tidFSD)  ? "FSD"     :\
     ((X)==tidFID)  ? "FID"     :\
     ((X)==tidAED)  ? "AED"     :\
     ((X)==tidIE)   ? "IE"      :\
     ((X)==tidTE)   ? "TE"      :\
     ((X)==tidFE)   ? "FE"      :\
     ((X)==tidEAHD) ? "EAHD"    :\
     ((X)==tidUSE)  ? "USE"     :\
     ((X)==tidSBD)  ? "SBD"     :\
     ((X)==tidPIE)  ? "PIE"     :\
     ((X)==tidEFE)  ? "EFE"     :\
                      "<il>" )  /* illegal */

/* Sometimes section numbers change in newer UDF revisions
 */
#define UDFREF_VAT(R)   ((R>=0x250) ? "UDF 2.2.11"  : "UDF 2.2.10")
#define UDFREF_SPTAB(R) ((R>=0x250) ? "UDF 2.2.12"  : "UDF 2.2.11")
#define UDFREF_PD(R)    ((R>=0x250) ? "UDF 2.2.14"  : "UDF 2.2.12")
#define UDFREF_PMAP(R)  ((R>=0x200) ? "UDF 2.2.4.7" : "UDF 2.2.4.6")
#define UDFREF_ISEQ(R)  ((R>=0x200) ? "UDF 2.2.4.6" : "UDF 2.2.4.5")

#define UDFREF_FIDICBCH(R) ((R>=0x200) ? "UDF 2.3.4.2+3" : "UDF 2.00+ 2.3.4.2+3")
#define UDFREF_FIDLIUIU(R) ((R>=0x200) ? "UDF 2.3.4.4+5" : "UDF 2.3.4.2+3")

/* ECMA descriptor format reference for each tag id.
 * UDF reference if descriptor does not exist in ECMA
 */
#define    tidECMAREF(X,R)  \
    (((X)==tidST)   ? UDFREF_SPTAB(R)   :\
     ((X)==tidPVD)  ? "ECMA 3/10.1"     :\
     ((X)==tidAVDP) ? "ECMA 3/10.2"     :\
     ((X)==tidVDP)  ? "ECMA 3/10.3"     :\
     ((X)==tidIUVD) ? "ECMA 3/10.4"     :\
     ((X)==tidPD)   ? "ECMA 3/10.5"     :\
     ((X)==tidLVD)  ? "ECMA 3/10.6"     :\
     ((X)==tidUSD)  ? "ECMA 3/10.8"     :\
     ((X)==tidTD)   ? "ECMA 3/10.9"     :\
     ((X)==tidLVID) ? "ECMA 3/10.10"    :\
     ((X)==tidFSD)  ? "ECMA 4/14.1"     :\
     ((X)==tidFID)  ? "ECMA 4/14.4"     :\
     ((X)==tidAED)  ? "ECMA 4/14.5"     :\
     ((X)==tidIE)   ? "ECMA 4/14.7"     :\
     ((X)==tidTE)   ? "ECMA 4/14.8"     :\
     ((X)==tidFE)   ? "ECMA 4/14.9"     :\
     ((X)==tidEAHD) ? "ECMA 4/14.10.1"  :\
     ((X)==tidUSE)  ? "ECMA 4/14.11"    :\
     ((X)==tidSBD)  ? "ECMA 4/14.12"    :\
     ((X)==tidPIE)  ? "ECMA 4/14.13"    :\
     ((X)==tidEFE)  ? "ECMA 4/14.17"    :\
                      "<illegal tag id>" )  /* illegal */
/* Descriptor tag
 */
typedef struct      /* descriptor tag */
{
    Uint16          tagIdentifier PACKED;
    Uint16          descriptorVersion PACKED;
    Uint8           tagChecksum PACKED;
    Byte            reserved PACKED;
    Uint16          tagSerialNumber PACKED;
    Uint16          descriptorCRC PACKED;
    Uint16          descriptorCRCLength PACKED;
    Uint32          tagLocation PACKED;
} Tag;

typedef struct      /* space bitmap */
{
    Tag             descriptorTag PACKED;
    Uint32          numberOfBits PACKED;
    Uint32          numberOfBytes PACKED;
    Byte            startOfBitmap PACKED;
 /* bitmap:
  * Byte            bitmap[numberOfBytes];
  */
} SpaceBitmapDescriptor;

typedef struct      /* partition header descriptor */
{
    ShortAd         unallocatedSpaceTable PACKED;
    ShortAd         unallocatedSpaceBitmap PACKED;
    ShortAd         partitionIntegrityTable PACKED;
    ShortAd         freedSpaceTable PACKED;
    ShortAd         freedSpaceBitmap PACKED;
    Byte            reserved[88] PACKED;
} PartitionHeaderDescriptor;

typedef struct      /* partition descriptor, ECMA 3/10.5 */
{
    Tag             descriptorTag PACKED;
    Uint32          volumeDescriptorSequenceNumber PACKED;
    Uint16          partitionFlags PACKED;
    Uint16          partitionNumber PACKED;
    EntityID        partitionContents PACKED;
    union {
        Byte        binary[128];
        PartitionHeaderDescriptor partitionHeaderDescriptor;
    } partitionContentsUse PACKED;
    Uint32          accessType PACKED;
    Uint32          partitionStartingLocation PACKED;
    Uint32          partitionLength PACKED;
    EntityID        implementationIdentifier PACKED;
    Byte            implementationUse[128] PACKED;
    Byte            reserved[156] PACKED;
} PartitionDescriptor;

typedef enum        /* PartitionDescriptor.accessType */
{
    PDAT_POW_OR_UNKNOWN = 0,    /* see UDF 2.60 2.2.14.2 */
    PDAT_READONLY       = 1,
    PDAT_WRITEONCE      = 2,
    PDAT_REWRITABLE     = 3,    /* may need preprocessing */
    PDAT_OVERWRITABLE   = 4,    /* NO preprocessing */
    PDAT_MAX = PDAT_OVERWRITABLE
} PD_ACCESS_TYPE;


/* Text macros for printing partition access type,
 * see UDF 2.60 2.2.14.2, ECMA 3/10.5.7.
 * The use of getUctMaxUdfRevision() may cause printing
 * "pseudo-overwritable" instead of "unknown" if the
 * final UDF revision has not yet been determined.
 */
#define PDAT_TEXT(X)    \
    (((getUctMaxUdfRevision())>=0x260 &&        \
      (X)==PDAT_POW_OR_UNKNOWN) ? "pseudo-overwritable" : \
     ((X)==PDAT_POW_OR_UNKNOWN) ? "unknown"            : \
     ((X)==PDAT_READONLY)       ? "read-only"         : \
     ((X)==PDAT_WRITEONCE)      ? "write-once"       : \
     ((X)==PDAT_REWRITABLE)     ? "rewritable"      : \
     ((X)==PDAT_OVERWRITABLE)   ? "overwritable"   : \
                                  "<illegal>")

typedef struct      /* volume descriptor pointer */
{
    Tag             descriptorTag PACKED;
    Uint32          volumeDescriptorSequenceNumber PACKED;
    ExtentAd        nextVolumeDescriptorSequenceExtent PACKED;
    Byte            reserved[484] PACKED;
} VolumeDescriptorPointer;

/* Primary Volume Descriptor:
 * PVD Flag Constants, ECMA 3/10.1.21
 */
#define PVDFL_MASK          ((Uint16) 0x00001)  /* bit 0 only */
#define PVDFL_VOLSETID_BIT  0

typedef struct      /* primary volume descriptor */
{
    Tag             descriptorTag PACKED;
    Uint32          volumeDescriptorSequenceNumber PACKED;
    Uint32          primaryVolumeDescriptorNumber PACKED;
    Dstring         volumeIdentifier[32] PACKED;
    Uint16          volumeSequenceNumber PACKED;
    Uint16          maximumVolumeSequenceNumber PACKED;
    Uint16          interchangeLevel PACKED;
    Uint16          maximumInterchangeLevel PACKED;
    Uint32          characterSetList PACKED;
    Uint32          maximumCharacterSetList PACKED;
    Dstring         volumeSetIdentifier[128] PACKED;
    Charspec        descriptorCharacterSet PACKED;
    Charspec        explanatoryCharacterSet PACKED;
    ExtentAd        volumeAbstract PACKED;
    ExtentAd        volumeCopyrightNotice PACKED;
    EntityID        applicationIdentifier PACKED;
    Timestamp       recordingDateAndTime PACKED;
    EntityID        implementationIdentifier PACKED;
    Byte            implementationUse[64] PACKED;
    Uint32          predecessorVolumeDescriptorSequenceLocation PACKED;
    Uint16          flags PACKED;       /* PrimaryVolumeDescriptor */
    Byte            reserved[22] PACKED;
} PrimaryVolumeDescriptor;

typedef struct      /* anchor volume descriptor pointer */
{
    Tag             descriptorTag PACKED;
    ExtentAd        mainVolumeDescriptorSequenceExtent PACKED;
    ExtentAd        reserveVolumeDescriptorSequenceExtent PACKED;
    Byte            reserved[480] PACKED;
} AnchorVolumeDescriptorPointer;

#define PMAP_TYPE1  1       /* partitionMap Type field values */
#define PMAP_TYPE2  2

typedef struct      /* all partition maps start like this */
{
    Uint8           partitionMapType PACKED;
    Uint8           partitionMapLength PACKED;
    Byte            startOfPartitionMapping PACKED;
} GenericPartitionMap;

typedef struct
{
    Uint8           partitionMapType PACKED;
    Uint8           partitionMapLength PACKED;
    Uint16          volumeSequenceNumber PACKED;
    Uint16          partitionNumber PACKED;
} Type1PartitionMap;

typedef struct
{
    Uint16          packetLength PACKED;
    Uint8           numberOfSparingTables PACKED;
    Byte            reserved2 PACKED;
    Uint32          sizeOfEachSparingTable PACKED;
    Byte            startOfLocationsOfSparingTables PACKED;
} SparablePartitionMapTail;

#define MP_DUPMETADATA_MASK 0x01    /* Duplicate Metadata Flag */

typedef struct
{
    Uint32          metadataFileLocation PACKED;
    Uint32          metadataMirrorFileLocation PACKED;
    Uint32          metadataBitmapFileLocation PACKED;
    Uint32          allocationUnitSize PACKED;
    Uint16          alignmentUnitSize PACKED;
    Uint8           flags PACKED;   /* MetadataPartitionMapTail */
    Byte            reserved2[5] PACKED;
} MetadataPartitionMapTail;

typedef struct
{
    Uint8           partitionMapType PACKED;
    Uint8           partitionMapLength PACKED;
    Byte            reserved1[2] PACKED;
    EntityID        partitionTypeIdentifier PACKED;
    Uint16          volumeSequenceNumber PACKED;
    Uint16          partitionNumber PACKED;
    union
    {
        Byte                     virtualReserved2[24];
        SparablePartitionMapTail sparableTail;
        MetadataPartitionMapTail metadataTail;
    } SharedTail PACKED;
} Type2PartitionMap;

typedef union
{
    GenericPartitionMap     genericPartitionMap;
    Type1PartitionMap       type1PartitionMap;
    Type2PartitionMap       type2PartitionMap;
} PartitionMap;     /* not PACKED, no UDF structure !! */

typedef struct      /* logical volume descriptor */
{
    Tag             descriptorTag PACKED;
    Uint32          volumeDescriptorSequenceNumber PACKED;
    Charspec        descriptorCharacterSet PACKED;
    Dstring         logicalVolumeIdentifier[128] PACKED;
    Uint32          logicalBlockSize PACKED;
    EntityID        domainIdentifier PACKED;
    union {
        Byte        binary[16];
        LongAd      fileSetDescriptorSequenceExtent;
    } logicalVolumeContentsUse PACKED;
    Uint32          mapTableLength PACKED;
    Uint32          numberOfPartitionMaps PACKED;
    EntityID        implementationIdentifier PACKED;
    Byte            implementationUse[128] PACKED;
    ExtentAd        integritySequenceExtent PACKED;
    Byte            startOfPartitionMaps PACKED;
 /*
  * partition map table:
  * Area of consecutive Type 1 or type 2 partition maps.
  */
} LogicalVolumeDescriptor;

typedef struct      /* unallocated space descriptor */
{
    Tag             descriptorTag PACKED;
    Uint32          volumeDescriptorSequenceNumber PACKED;
    Uint32          numberOfAllocationDescriptors PACKED;
    Byte            startOfAllocationDescriptors PACKED;
    /* allocation descriptors:
     * ExtentAd allocationDescriptors[numberOfAllocationDescriptors] PACKED;
     */
} UnallocatedSpaceDescriptor;

typedef struct      /* terminating descriptor */
{
    Tag             descriptorTag PACKED;
    Byte            reserved[496] PACKED;
} TerminatingDescriptor;

/* Logical Volume Header Descriptor
 * ECMA 4/3.1, 4/14.15
 * UDF 2.00 3.2.1
 * Located in logicalVolumeContentsUse of LVID
 */
typedef struct
{
    Uint64          uniqueID PACKED;
    Byte            reserved[24] PACKED;
} LogicalVolumeHeaderDescriptor;

/* LvidImplementationUse
 * UDF 2.2.6.4
 */
typedef struct
{
    EntityID    implementationID PACKED;
    Uint32      numberOfFiles PACKED;
    Uint32      numberOfDirectories PACKED;
    Uint16      minimumUDFReadRevision PACKED;
    Uint16      minimumUDFWriteRevision PACKED;
    Uint16      maximumUDFWriteRevision PACKED;
    Byte        startofImplementationUse PACKED;
 /* remaining implementation use:
  * Byte implementationUse[lvid.lengthOfImplementationUse-46];
  */
} LvidImplementationUse;

/* Logical Volume Integrity Descriptor (LVID)
 * ECMA 3/8.8.2, 3/10.10, UDF 2.2.6
 */
#define LVIDINTEGRITY_OPEN      0
#define LVIDINTEGRITY_CLOSE 1
#define LVIDINTEGRITY_MAX       LVIDINTEGRITY_CLOSE

typedef struct
{
    Tag             descriptorTag PACKED;
    Timestamp       recordingDateAndTime PACKED;
    Uint32          integrityType PACKED;
    ExtentAd        nextIntegrityExtent PACKED;
    union {
        Byte                            binary[32];
        LogicalVolumeHeaderDescriptor   logicalVolumeHeaderDescriptor;
    } logicalVolumeContentsUse PACKED;
    Uint32          numberOfPartitions PACKED;
    Uint32          lengthOfImplementationUse PACKED;
    Byte            startOfTables PACKED;

    /* tables and implementation use:
     *  Uint32      FreeSpaceTable[numberOfPartitions];
     *  Uint32      SizeTable[numberOfPartitions];
     *  union {
     *    Byte                  iuBytes[lengthOfImplementationUse];
     *    LvidImplementationUse lvidImplementationUse;
     *  } implementationUse;
     */
} LogicalVolumeIntegrityDescriptor;

typedef struct      /* LVInformation */
{
    Charspec        LVICharset PACKED;
    Dstring         logicalVolumeIdentifier[128] PACKED;
    Dstring         LVInfo1[36] PACKED;
    Dstring         LVInfo2[36] PACKED;
    Dstring         LVInfo3[36] PACKED;
    EntityID        implementationID PACKED;    /* UDF Entity Id */
    Byte            implementationUse[128] PACKED;
} LVInformation;

typedef struct      /* implementation use volume descriptor */
{
    Tag             descriptorTag PACKED;
    Uint32          volumeDescriptorSequenceNumber PACKED;
    EntityID        implementationIdentifier PACKED;    /* Implementation Entity Id */
    union {
        Byte            binary[460];
        LVInformation   lvInformation; /* this shall be used */
    } implementationUse PACKED;
} ImplementationUseVolumeDescriptor;


typedef struct
{
    Uint32          originalLocation PACKED;
    Uint32          mappedLocation PACKED;
} SparingEntry;

typedef struct
{
    Tag             descriptorTag PACKED;
    EntityID        sparingIdentifier PACKED;
    Uint16          reallocationTableLength PACKED;
    Byte            reserved[2] PACKED;
    Uint32          sequenceNumber PACKED;
    Byte            startOfMapEntries PACKED;
} SparingTable;

typedef struct  /* tail of UDF 1.50 VAT after Uint32[numberOfEntries] table */
{
    EntityID    EntityIdentifier PACKED;
    Uint32      PreviousVATICBlocation PACKED;
} VAT150Tail;

/* UDF 2.50 2.2.11  (was 2.2.10) :
 * Head of UDF 2.00+ VAT until first byte of ImplementationUse
 * After ImplementationUse follows Uint32[numberOfEntries] table
 */
typedef struct      /* UDF 2.00+ VAT file header */
{
    Uint16  lengthofHeader PACKED;
    Uint16  lengthofImplementationUse PACKED;
    Dstring logicalVolumeIdentifier[128] PACKED;
    Uint32  previousVATICBlocation PACKED;
    Uint32  numberOfFiles PACKED;
    Uint32  numberOfDirectories PACKED;
    Uint16  minUDFReadVersion PACKED;
    Uint16  minUDFWriteVersion PACKED;
    Uint16  maxUDFWriteVersion PACKED;
    Byte    reserved[2] PACKED;
    Byte    startOfImplementationUse PACKED;
} VAT200Head;


/* FE File types, reserved ranges first.
 */
#define FT_RES_ECMA02_FIRST     ((Uint8)  13)   /* ECMA 4/14.6.6 */
#define FT_RES_ECMA02_LAST      ((Uint8) 255)   /*   2nd edition */

#define FT_RES_ECMA03_FIRST     ((Uint8)  14)   /* ECMA 4/14.6.6 */
#define FT_RES_ECMA03_LAST      ((Uint8) 247)   /*   3rd edition */

#define FT_AGREE_UDF200_FIRST   ((Uint8) 249)   /* UDF 2.00, agreement between ... */
#define FT_AGREE_UDF200_LAST    ((Uint8) 255)   /* ECMA 4/14.6.6, UDF 2.3.5.2 */

#define FT_RES_UDF201_FIRST     ((Uint8) 250)   /* extra for UDF 2.01 */
#define FT_RES_UDF201_LAST      ((Uint8) 255)   /*  UDF 2.01  2.3.5.2 */

#define FT_RES_UDF250P_FIRST    ((Uint8) 253)   /* extra for UDF 2.50+ */
#define FT_RES_UDF250P_LAST     ((Uint8) 255)   /*  UDF 2.50  2.3.5.2 */

#define FT_UNKNOWN_OR_VAT150    ((Uint8)   0)
#define FT_UNALLOCATEDSPACE     ((Uint8)   1)
#define FT_PARTITIONINTEGRITY   ((Uint8)   2)
#define FT_INDIRECT_ENTRY       ((Uint8)   3)
#define FT_DIRECTORY            ((Uint8)   4)
#define FT_SEQUENCE_BYTES       ((Uint8)   5)
#define FT_BLOCK_DEVICE         ((Uint8)   6)
#define FT_CHARACTER_DEVICE     ((Uint8)   7)
#define FT_EXTENDED_ATTR        ((Uint8)   8)
#define FT_FIFO                 ((Uint8)   9)
#define FT_C_ISSOCK             ((Uint8)  10)
#define FT_TERMINAL_ENTRY       ((Uint8)  11)
#define FT_SYMBOLIC_LINK        ((Uint8)  12)
#define FT_STREAM_DIRECTORY     ((Uint8)  13)   /* new in UDF 2.00 */

#define FT_VAT200               ((Uint8) 248)   /* new in UDF 2.00 */
#define FT_REAL_TIME_FILE       ((Uint8) 249)   /* new in UDF 2.01 */
#define FT_METADATA_FILE        ((Uint8) 250)   /* new in UDF 2.50 */
#define FT_METADATA_MIRROR_FILE ((Uint8) 251)   /* new in UDF 2.50 */
#define FT_METADATA_BITMAP_FILE ((Uint8) 252)   /* new in UDF 2.50 */

#define FT_ILLEGAL              ((Uint8) 255)

/* FT_ text macros for printing, etc.
 * FT_TEXT4(fileType) : max 4 chars for 'normal' types.
 * For this text no UDf revision tests are done here.
 */
#define    FT_TEXT4(X)  \
    (((X)==FT_UNKNOWN_OR_VAT150)    ? "VAT1" :\
     ((X)==FT_UNALLOCATEDSPACE)     ? "USE"  :\
     ((X)==FT_PARTITIONINTEGRITY)   ? "PIE"  :\
     ((X)==FT_INDIRECT_ENTRY)       ? "IE"   :\
     ((X)==FT_DIRECTORY)            ? "DIR"  :\
     ((X)==FT_SEQUENCE_BYTES)       ? "FILE" :\
     ((X)==FT_BLOCK_DEVICE)         ? "BDEV" :\
     ((X)==FT_CHARACTER_DEVICE)     ? "CDEV" :\
     ((X)==FT_EXTENDED_ATTR)        ? "EAF"  :\
     ((X)==FT_FIFO)                 ? "FIFO" :\
     ((X)==FT_C_ISSOCK)             ? "SOCK" :\
     ((X)==FT_TERMINAL_ENTRY)       ? "TE"   :\
     ((X)==FT_SYMBOLIC_LINK)        ? "SYML" :\
     ((X)==FT_STREAM_DIRECTORY)     ? "SDIR" :\
     ((X)==FT_VAT200)               ? "VAT2" :\
     ((X)==FT_REAL_TIME_FILE)       ? "RTF"  :\
     ((X)==FT_METADATA_FILE)        ? "MF"  :\
     ((X)==FT_METADATA_MIRROR_FILE) ? "MMF" :\
     ((X)==FT_METADATA_BITMAP_FILE) ? "MBF" :\
                                 "<unknown>")

/* File Characteristics bit numbers
 */
#define FCB_HIDDEN          0       /* Existence bit */
#define FCB_DIRECTORY       1
#define FCB_DELETED         2
#define FCB_PARENT          3
#define FCB_METADATA        4

#define FC_RESERVED_MASK    ((Uint8) 0xE0)  /* Uint8 mask */

/* ICB Flag Constants: Allocation Descriptor type
 * ECMA 4/14.6.8
 */
#define ICBF_ADT_MASK       ((Uint16) 0x0007)   /* bits  0-2  */
#define ICBF_RESERVED_MASK  ((Uint16) 0xC000)   /* bits 14-15 */

#define GET_ADTYPE(ITFLAGS) ((Uint8)((ITFLAGS) & ICBF_ADT_MASK))

#define ADT_SHORT       ((Uint8) 0)
#define ADT_LONG        ((Uint8) 1)
#define ADT_EXTENDED    ((Uint8) 2)
#define ADT_INFE        ((Uint8) 3)
#define ADT_MAX         ADT_INFE        /* for ICBTag Flags */
#define ADT_EXTENT      ((Uint8) 0xFF)  /* execption, extent_ad */

/* ICB Flag Constants: other bits 3-13,
 * ECMA 4/14.6.8
 */
#define ICBF_SORTED_BIT          3  /* Shall be ZERO */
#define ICBF_NONRELOCATABLE_BIT  4
#define ICBF_ARCHIVE_BIT         5
#define ICBF_SETUID_BIT          6
#define ICBF_SETGID_BIT          7
#define ICBF_STICKY_BIT          8
#define ICBF_CONTIGUOUS_BIT      9
#define ICBF_SYSTEM_BIT         10
#define ICBF_TRANSFORMED_BIT    11  /* Shall be ZERO */
#define ICBF_MULTIVERSIONS_BIT  12  /* Shall be ZERO */
#define ICBF_STREAM_BIT         13  /* ECMA 167 3rd edition only */


typedef struct      /* fileset descriptor */
{
    Tag             descriptorTag PACKED;
    Timestamp       recordingDateAndTime PACKED;
    Uint16          interchangeLevel PACKED;
    Uint16          maximumInterchangeLevel PACKED;
    Uint32          characterSetList PACKED;
    Uint32          maximumCharacterSetList PACKED;
    Uint32          fileSetNumber PACKED;
    Uint32          fileSetDescriptorNumber PACKED;
    Charspec        logicalVolumeIdentifierCharacterSet PACKED;
    Dstring         logicalVolumeIdentifier[128] PACKED;
    Charspec        fileSetCharacterSet PACKED;
    Dstring         fileSetIdentifier[32] PACKED;
    Dstring         copyrightFileIdentifier[32] PACKED;
    Dstring         abstractFileIdentifier[32] PACKED;
    LongAd          rootDirectoryICB PACKED;
    EntityID        domainIdentifier PACKED;
    LongAd          nextExtent PACKED;
    union {
        LongAd      systemStreamDirectoryICB;       /* new in UDF 2.00 */
        Byte        reserved01[16];                 /* + 32, UDF 1.02/1.50 */
    } new200 PACKED;
    Byte            reserved02[32] PACKED;
} FileSetDescriptor;

/* Mind that strategyParameter is always interpreted as a Uint16
 */
typedef struct      /* icb tag */
{
    Uint32          priorRecordedNumberOfDirectEntries PACKED;
    Uint16          strategyType PACKED;
    union {
        Byte            strategyParameter[2];
        Uint16          strategyParameterU16;
    } strPar PACKED;
    Uint16          maximumNumberOfEntries PACKED;
    Byte            reserved PACKED;
    Uint8           fileType PACKED;
    LBAddr          parentICBLocation PACKED;
    Uint16          flags PACKED;       /* ICBTag */
} ICBTag;

/* File Entry (FE) and Extended File Entry (EFE) structs are similar.
 * The EFE has some extra fields.
 * Common access macros pFE... are used to access both structures.
 * These macros are defined after the FE amd EFE definitions.
 */
typedef struct  /* File Entry */
{
    Tag         descriptorTag PACKED;
    ICBTag      icbTag PACKED;
    Uint32      uid PACKED;
    Uint32      gid PACKED;
    Uint32      permissions PACKED;
    Uint16      fileLinkCount PACKED;
    Uint8       recordFormat PACKED;
    Uint8       recordDisplayAttributes PACKED;
    Uint32      recordLength PACKED;
    Uint64      informationLength PACKED;
    Uint64      logicalBlocksRecorded PACKED;
    Timestamp   accessTime PACKED;
    Timestamp   modificationTime PACKED;
    Timestamp   attributeTime PACKED;
    Uint32      checkpoint PACKED;
    LongAd      extendedAttributeICB PACKED;
    EntityID    implementationIdentifier PACKED;
    Uint64      uniqueID PACKED;
    Uint32      lengthOfExtendedAttributes PACKED;
    Uint32      lengthOfAllocationDescriptors PACKED;
    Byte        startOfExtendedAttributes PACKED;
    /*
     * Variable sized arrays follow :
     *  Byte    extendedAttributes[lengthOfExtendedAttributes] PACKED;
     *  Byte    allocationdescriptors[lengthOfAllocationDescriptors] PACKED;
     */
} FileEntry;

typedef struct  /* Extended File Entry,  */
{
    Tag         descriptorTag PACKED;
    ICBTag      icbTag PACKED;
    Uint32      uid PACKED;
    Uint32      gid PACKED;
    Uint32      permissions PACKED;
    Uint16      fileLinkCount PACKED;
    Uint8       recordFormat PACKED;
    Uint8       recordDisplayAttributes PACKED;
    Uint32      recordLength PACKED;
    Uint64      informationLength PACKED;
    Uint64      objectSize PACKED;                  /* not in FE */
    Uint64      logicalBlocksRecorded PACKED;
    Timestamp   accessTime PACKED;
    Timestamp   modificationTime PACKED;
    Timestamp   creationTime PACKED;                /* not in FE */
    Timestamp   attributeTime PACKED;
    Uint32      checkpoint PACKED;
    Byte        reserved[4] PACKED;                 /* not in FE */
    LongAd      extendedAttributeICB PACKED;
    LongAd      streamDirectoryICB PACKED;          /* not in FE */
    EntityID    implementationIdentifier PACKED;
    Uint64      uniqueID PACKED;
    Uint32      lengthOfExtendedAttributes PACKED;
    Uint32      lengthOfAllocationDescriptors PACKED;
    Byte        startOfExtendedAttributes PACKED;
    /*
     * Variable sized arrays follow :
     *  Byte    extendedAttributes[lengthOfExtendedAttributes] PACKED;
     *  Byte    allocationdescriptors[lengthOfAllocationDescriptors] PACKED;
     */
} ExtendedFileEntry;

/* The following macro's are introduced in order to the be able to handle
 * the File Entry and Extended File Entry in common code sections.
 * Note that all pFE_* macros are pointers, and that only the first
 * ones can be used before the descriptor tag has been endian swapped
 * (see note below).
 */
#define pFE_descriptorTag(ptr)  (&(((FileEntry *)(ptr))->descriptorTag))
#define pFE_icbTag(ptr)         (&(((FileEntry *)(ptr))->icbTag))
#define pFE_uid(ptr)            (&(((FileEntry *)(ptr))->uid))
#define pFE_gid(ptr)            (&(((FileEntry *)(ptr))->gid))
#define pFE_permissions(ptr)    (&(((FileEntry *)(ptr))->permissions))
#define pFE_fileLinkCount(ptr)  (&(((FileEntry *)(ptr))->fileLinkCount))
#define pFE_recordFormat(ptr)   (&(((FileEntry *)(ptr))->recordFormat))
#define pFE_recordDisplayAttributes(ptr) \
                                (&(((FileEntry *)(ptr))->recordDisplayAttributes))
#define pFE_recordLength(ptr)   (&(((FileEntry *)(ptr))->recordLength))
#define pFE_informationLength(ptr) \
                                (&(((FileEntry *)(ptr))->informationLength))
/* from here different byte offsets for FE and EFE
 * so the tag id is needed to distinguish, which means that the following
 * macros cannot be used if the tag id is not yet endian swapped !!!
 */
#define pFE_logicalBlocksRecorded(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidFE)?\
     (&(((FileEntry *)(ptr))->logicalBlocksRecorded)):\
     (&(((ExtendedFileEntry *)(ptr))->logicalBlocksRecorded)))
#define pFE_accessTime(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidFE)?\
     (&(((FileEntry *)(ptr))->accessTime)):\
     (&(((ExtendedFileEntry *)(ptr)))->accessTime))
#define pFE_modificationTime(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidFE)?\
     (&(((FileEntry *)(ptr))->modificationTime)):\
     (&(((ExtendedFileEntry *)(ptr))->modificationTime)))
#define pFE_attributeTime(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidFE)?\
     (&(((FileEntry *)(ptr))->attributeTime)):\
     (&(((ExtendedFileEntry *)(ptr))->attributeTime)))
#define pFE_checkpoint(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidFE)?\
     (&(((FileEntry *)(ptr))->checkpoint)):\
     (&(((ExtendedFileEntry *)(ptr))->checkpoint)))
#define pFE_extendedAttributeICB(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidFE)?\
     (&(((FileEntry *)(ptr))->extendedAttributeICB)):\
     (&(((ExtendedFileEntry *)(ptr))->extendedAttributeICB)))
#define pFE_implementationIdentifier(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidFE)?\
     (&(((FileEntry *)(ptr))->implementationIdentifier)):\
     (&(((ExtendedFileEntry *)(ptr))->implementationIdentifier)))
#define pFE_uniqueID(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidFE)?\
     (&(((FileEntry *)(ptr))->uniqueID)):\
     (&(((ExtendedFileEntry *)(ptr))->uniqueID)))
#define pFE_lengthOfExtendedAttributes(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidFE)?\
     (&(((FileEntry *)(ptr))->lengthOfExtendedAttributes)):\
     (&(((ExtendedFileEntry *)(ptr))->lengthOfExtendedAttributes)))
#define pFE_lengthOfAllocationDescriptors(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidFE)?\
     (&(((FileEntry *)(ptr))->lengthOfAllocationDescriptors)):\
     (&(((ExtendedFileEntry *)(ptr))->lengthOfAllocationDescriptors)))
#define pFE_startOfExtendedAttributes(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidFE)?\
     (&(((FileEntry *)(ptr))->startOfExtendedAttributes)):\
     (&(((ExtendedFileEntry *)(ptr))->startOfExtendedAttributes)))
/* from here different fields for EFE only
 * evaluates to NULL for FE
 */
#define pFE_streamDirectoryICB(ptr) \
    ((((FileEntry *)(ptr))->descriptorTag.tagIdentifier==tidEFE)?\
     (&(((ExtendedFileEntry *)(ptr))->streamDirectoryICB)):\
     ((LongAd*) NULL))

/* showPermissions(): ECMA 4/14.9.5, UDF 3.3.3.3.
 */
#define FE_PERM_BITNMB_OTHER_EXEC    0
#define FE_PERM_BITNMB_OTHER_WRITE   1
#define FE_PERM_BITNMB_OTHER_READ    2
#define FE_PERM_BITNMB_OTHER_ATR     3
#define FE_PERM_BITNMB_OTHER_DEL     4

#define FE_PERM_BITNMB_GROUP_EXEC    5
#define FE_PERM_BITNMB_GROUP_WRITE   6
#define FE_PERM_BITNMB_GROUP_READ    7
#define FE_PERM_BITNMB_GROUP_ATR     8
#define FE_PERM_BITNMB_GROUP_DEL     9

#define FE_PERM_BITNMB_OWNER_EXEC   10
#define FE_PERM_BITNMB_OWNER_WRITE  11
#define FE_PERM_BITNMB_OWNER_READ   12
#define FE_PERM_BITNMB_OWNER_ATR    13
#define FE_PERM_BITNMB_OWNER_DEL    14

#define FE_PERM_RESERVED_MASK   ((Uint32) 0xFFFF8000)   /* bits 15-31 */

typedef struct      /* file identifier descriptor */
{
    Tag         descriptorTag PACKED;
    Uint16      fileVersionNumber PACKED;
    Uint8       fileCharacteristics PACKED;
    Uint8       lengthOfFileIdentifier PACKED;
    LongAd      ICB PACKED;     /* may contain Uint32 UDFUniqueID */
    Uint16      lengthOfImplementationUse PACKED;
    Byte        startOfImplementationUse PACKED;
 /* implementation use, etc.:
  *   Byte   implementationUse[lengthOfImplementationUse];
  *   Dchars fileIdentifier[lengthOfFileIdentifier];
  *   Byte   padding[lengthOfPadding];
  * where lengthOfPadding is < 4 and such that the FID
  * descriptor length is a multiple of 4.
  */
} FileIdentifierDescriptor;

/* AllocationExtentDescriptor:
 * Exception:
 *  For the AllocationExtentDescriptor, the
 *  Byte AllocationDescriptors[LengthOfAllocationDescriptors]
 *  field, immediately following the lengthOfAllocationDescriptors
 *  field was added, BUT it is no part of the descriptor
 *  itself, according to ECMA and UDF.
 */
typedef struct
{
    Tag         descriptorTag PACKED;
    Uint32      previousAllocationExtentLocation PACKED;
    Uint32      lengthOfAllocationDescriptors PACKED;
    Byte        startOfAllocationDescriptors PACKED;
    /*
     *  Byte AllocationDescriptors[LengthOfAllocationDescriptors];
     */
} AllocationExtentDescriptor;

typedef struct
{
    Tag         descriptorTag PACKED;
    ICBTag      icbTag PACKED;
    Uint32      lengthOfAllocationDescriptors PACKED;
    Byte        startOfAllocationDescriptors PACKED;
 /* allocation descriptors:
  * Byte allocationDescriptors[lengthOfAllocationDescriptors];
  */
} UnallocatedSpaceEntry;

typedef struct          /* ECMA 4/14.7 */
{
    Tag         descriptorTag PACKED;
    ICBTag      icbTag PACKED;
    LongAd      indirectICB PACKED;
} IndirectEntry;

typedef struct          /* ECMA 4/14.7 */
{
    Tag         descriptorTag PACKED;
    ICBTag      icbTag PACKED;
} TerminalEntry;

/* Extended Attributes
 */

typedef struct      /* ECMA 4/14.10.1 */
{
    Tag         descriptorTag PACKED;
    Uint32      implementationAttributesLocation PACKED;
    Uint32      applicationAttributesLocation PACKED;
} ExtendedAttributeHeaderDescriptor;

typedef struct      /* ECMA 4/14.10.2, common part for all EAs */
{
    Uint32          attributeType PACKED;
    Uint8           attributeSubtype PACKED;    /* always 1 */
    Byte            reserved[3] PACKED;
    Uint32          attributeLength PACKED;
} EAGenericHead;    /* Generic Extended Attribute Head */


typedef struct      /* ECMA 4/14.10.3 */
{
    EAGenericHead   genericHead PACKED;
    Uint32          escapeSequencesLength PACKED;
    Uint8           characterSetType PACKED;
    Byte            startOfEscapeSequences PACKED;
} CharSetInformationExtendedAttribute;

/* ECMA 4/14.10.4
 * Alternate Permissions Extended Attribute not recorded in UDF
 * UDF 3.3.4.2
 */

typedef struct      /* ECMA 4/14.10.5 */
{
    EAGenericHead   genericHead PACKED;
    Uint32          dataLength PACKED;
    Uint32          fileTimeExistence PACKED;
    Byte            startOfFileTimes PACKED;
} FileTimesExtendedAttribute;

typedef struct      /* ECMA 4/14.10.6 */
{
    EAGenericHead   genericHead PACKED;
    Uint32          dataLength PACKED;
    Uint32          informationTimeExistence PACKED;
    Byte            startOfInformationTimes PACKED;
} InformationTimesExtendedAttribute;

typedef struct      /* ECMA 4/14.10.7, UDF 3.3.4.4 */
{
    EAGenericHead   genericHead PACKED;
    Uint32          implementationUseLength PACKED;
    Uint32          majorDeviceIdentification PACKED;
    Uint32          minorDeviceIdentification PACKED;
    union {
        Byte        startOfImplementationUse;
        EntityID    impUseID;
    } implementationUse PACKED;
} DeviceSpecificationExtendedAttribute;

/* ECMA 4/14.10.8
 */
typedef struct ImplementationUseExtendedAttribute
            ImplementationUseExtendedAttribute;
struct ImplementationUseExtendedAttribute
{
    EAGenericHead   genericHead PACKED;
    Uint32          implementationUseLength PACKED;
    EntityID        implementationIdentifier PACKED;
    union {
        Byte        startOfImplementationUse;
        Uint16      headerChecksum;
    } implementationUse PACKED;
} PACKED;

/* ECMA 4/14.10.9
 */
typedef struct ApplicationUseExtendedAttribute
            ApplicationUseExtendedAttribute;
struct ApplicationUseExtendedAttribute
{
    EAGenericHead   genericHead PACKED;
    Uint32          applicationUseLength PACKED;
    EntityID        applicationIdentifier PACKED;
    union {
        Byte        startOfApplicationUse;
        Uint16      headerChecksum;
    } applicationUse PACKED;
} PACKED;

typedef union       /* ECMA 4/14.10.* */
{
    EAGenericHead                        genericHead;
    CharSetInformationExtendedAttribute  charSetInfoEA;
    FileTimesExtendedAttribute           fileTimesEA;
    InformationTimesExtendedAttribute    infoTimesEA;
    DeviceSpecificationExtendedAttribute devSpecEA;
    ImplementationUseExtendedAttribute   implUseEA;
    ApplicationUseExtendedAttribute      applUseEA;
} ExtendedAttribute;

/* values for EAGenericHead.attributeType
 * ECMA 4/9.1 and 4/14.10.*, UDF 3.3.4
 */
#define EATYPE_CHARSETINFO  1
#define EATYPE_ALTPERMIS    3       /* not in UDF, UDF 3.3.4.2 */
#define EATYPE_FILETIMES    5
#define EATYPE_INFOTIMES    6
#define EATYPE_DEVSPEC      12
#define EATYPE_IMPLUSE      2048
#define EATYPE_APPLUSE      65536

#define    EATEXT(X)    \
    (((X)==EATYPE_CHARSETINFO)  ? "Character Set Information EA" :\
     ((X)==EATYPE_ALTPERMIS)    ? "Alternate Permissions EA" :\
     ((X)==EATYPE_FILETIMES)    ? "File Times EA"           :\
     ((X)==EATYPE_INFOTIMES)    ? "Information Times EA"    :\
     ((X)==EATYPE_DEVSPEC)      ? "Device Specification EA" :\
     ((X)==EATYPE_IMPLUSE)      ? "Implementation Use EA"   :\
     ((X)==EATYPE_APPLUSE)      ? "Application Use EA"      :\
                                  "<illegal> EA")
/* EA group definitions:
 * The EA_GROUP_xx values increase with their position
 * in an EA space as defined in UDF 3.3.4,
 * except for the EAGROUP_ERROR group.
 * Use UDF 3.3.4 terminology for EA groups
 */
#define EAGROUP_ERROR           0   /* illegal, reserved or not used by UDF */
#define EAGROUP_ECMA            1   /* completely defined by ECMA */
#define EAGROUP_IMPL_4ALIGN     2   /* Implementation Use EA */
#define EAGROUP_IMPL_BLOCKALIGN 3   /* Implementation Use EA, block aligned */
#define EAGROUP_APPL            4   /* Application Use EA */

#define EAGROUP_TEXT(X) \
    (((X)==EAGROUP_ERROR)           ? "EA type error"   :\
     ((X)==EAGROUP_ECMA)            ? "ECMA 167 EA"     :\
     ((X)==EAGROUP_IMPL_4ALIGN)     ? "non block aligned Implementation Use EA" :\
     ((X)==EAGROUP_IMPL_BLOCKALIGN) ? "block aligned Implementation Use EA" :\
     ((X)==EAGROUP_APPL)            ? "Application Use EA"  :\
                                      "(illegal EA Type)")

/* symbolic link Pathname
 * ECMA 167 4/14.16, UDF 2.3.12
 */
typedef struct      /* ECMA 167 4/14.16.1, UDF 2.3.12.1 */
{
    Uint8   componentType;
    Uint8   lengthofComponentIdentifier;
    Uint16  componentFileVersionNumber;
    Dchars  startOfComponentIdentifier;
 /* Dchars  componentIdentifier[lengthofComponentIdentifier];
  */
} PathComponent;

/* Restore default structure packing
 */
#ifndef __GNUC__
#pragma pack()
#endif

#undef PACKED

#endif /* __UCT_UDFSTRUCT_H__ */

