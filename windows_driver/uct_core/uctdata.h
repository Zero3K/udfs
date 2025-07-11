/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctdata.h
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __UCT_UCTDATA_H__
#define __UCT_UCTDATA_H__

#include "udfstruct.h"
#include "unicode.h"
#include "device.h"

typedef Uint8 BitVector;    /* NO bigger units because of endian swap, etc !! */

/* AnyAllocationDescriptor structure that can hold any
 * type of AD. All types have the extentLength field as
 * a sharable first Uint32 anyAd.extentLength element.
 * For type definition, use ADT_* macros,
 * e.g. ADT_LONG, etc. (also ADT_EXTENT).
 */
typedef union
{
    AnyAd       anyAd;      /* common length field */
    ShortAd     shortAd;    /*  short_ad: ECMA 4/14.14.1 */
    LongAd      longAd;     /*   long_ad: ECMA 4/14.14.2 */
/** ExtAd       extAd;      **    ext_ad: ECMA 4/14.14.3, unused in UDF **/
    ExtentAd    extentAd;   /* extent_ad: ECMA 3/7.1 */
} AnyAllocationDescriptor;

typedef struct UdfAllocationItem
{
    struct UdfAllocationItem    *next;
    AnyAllocationDescriptor      aad;
} UdfAllocationItem;

/* UdfAllocationList structure:
 * All ADs defining file/stream data extents
 * are in a linked list here.
 * There is a separate overheadList
 * for ADEL_EXTENTPOINTER type ADs, etc. in order
 * to know the extra allocated AED blocks.
 */
typedef struct UdfAllocationList
{
    UdfAllocationItem        *head;
    UdfAllocationItem        *tail;
    struct UdfAllocationList *overheadList; /* ADEL_EXTENTPOINTER, etc */
    Uint8                     itemAdType;
} UdfAllocationList;

/* ContExtentItem:
 * structure for Continuation Extents list
 * and sequenceId definition.
 */
#define CEI_ID_MAINVDS  1       /* Main VDS cont extent */
#define CEI_ID_RESVDS   2       /* Reserve VDS cont extent */
#define CEI_ID_LVID     3       /* LVID cont extent */
#define CEI_ID_FSD      4       /* FSD cont extent */

typedef struct ContExtentItem
{
    struct ContExtentItem   *next;
    AnyAllocationDescriptor  aad;           /* list */
    Uint8                    adType;        /* AD Type */
    Uint8                    sequenceId;    /* one of CEI_ID_* */
} ContExtentItem;


typedef struct
{
    /* Pointer is NULL when not used */
    SpaceBitmapDescriptor       *unallocatedSpaceBitmap;
    UnallocatedSpaceEntry       *unallocatedSpaceTable;
    SpaceBitmapDescriptor       *freedSpaceBitmap;
    UnallocatedSpaceEntry       *freedSpaceTable;
    BitVector                   *fabricatedBitmap;
    Uint8                        status; /* bit-fields, see below */
} PartitionSpaceSet;


/* PartitionSpaceSet.status bit definitions
 */
#define F_PSS_LOADED    1       /* bit 0 mask, loaded */

/* void pointer array (vpa) structure and
 * allocation strategy definitions.
 * Used for all pointer types.
 */
#define VoidPointer void*
typedef struct
{
    VoidPointer *arr;   /* points to VoidPointer array */
    Uint32       len;   /* nmb of void pointers */
    Uint32       alen;  /* nmb of void pointers allocated */
} VoidPointerArrayHead;

#define VPA_INIT     128    /* initial allocation */
#define VPA_MAXINC  1024    /* max allocation increment */
#define INIT_VPA_STRUCT {NULL,0,0}  /* initialize empty npa */

typedef struct      /* udf volume information */
{
    PrimaryVolumeDescriptor             *pvd;   /* one prevailing */
    LogicalVolumeDescriptor             *lvd;   /* one prevailing */
    UnallocatedSpaceDescriptor          *usd;   /* one prevailing */
    Uint32                               nrPDs;
    PartitionDescriptor                 *pd;    /* array of PDs   */
    Uint32                               nrIUVDs;
    ImplementationUseVolumeDescriptor   *iuvd;  /* array of IUVDs */
    VoidPointerArrayHead                 dpa;   /* descr Pointer Array */
} UdfVolumeInformation;

/* Virtual Partition references
 */
typedef struct
{
    Byte            *vatFile;           /* allocated memory for VAT,  */
                                        /* NULL for non-virtual partition maps */
    Uint32          *vatTable;          /* points to begin of VAT table */
    Uint16           vatIntroRevision;  /* 0x150 or 0x200 */
    Uint32           numberOfEntries;   /* nmb of entries in VAT table */
} VirtualRecord;

/* Metadata Partition references
 */
typedef struct
{
    UdfAllocationList *pMetadataFileAllocList;
} MetadataRecord;

/* TODO: Modify this for the multi-volume Volume Set case */

/* PMapType further specifies the exact type of a partition
 */
typedef enum
{
    PMAPTYPE_PHYSICAL = 0,      /* PMAP_TYPE1 */
    PMAPTYPE_VIRTUAL,           /* PMAP_TYPE2, virtual */
    PMAPTYPE_SPARABLE,          /* PMAP_TYPE2, sparable */
    PMAPTYPE_METADATA,          /* PMAP_TYPE2, metadata */
    PMAPTYPE_UNKNOWN
} PMapType;

#define PMAPTYPE_TEXT(X)    \
    (((X)==PMAPTYPE_PHYSICAL)   ? "Physical" :\
     ((X)==PMAPTYPE_VIRTUAL)    ? "Virtual"  :\
     ((X)==PMAPTYPE_SPARABLE)   ? "Sparable" :\
     ((X)==PMAPTYPE_METADATA)   ? "Metadata" :\
                                  "<unknown>")

typedef struct
{
    PMapType             pMapType;
    PartitionMap        *pPartitionMap;
    PartitionDescriptor *pdPointer;
    SparingTable        *pSparingTable;
    BitVector           *pPartAlloc;    /* verifier bitmap for Partition Allocation */
    bool                 partAllocOutOfMem;
    /* actualPartitionLength deviates from PD partitionLength
     * for metadata and virtual Partition.
     */
    Uint32               actualPartitionLength;
    PartitionSpaceSet    pss;                   /* Freed or Unallocated Space Sets */
    Uint32               spaceSetsFreeSpace;    /* Free Space as in Space Sets */
    Uint32               calculatedFreeSpace;   /* Free Space calculated by verifier */
    Uint32               usedUnallocatedBlocks;
    Uint32               unusedAllocatedBlocks;
    UdfAllocationList   *shortOverheadList;
    Uint16               counterpartPRef;   /* Cross ref to companion partition in case */
                                            /* virtual or metadata partitions. If no */
                                            /* companion then pointing to themselves */
    VirtualRecord        vatRec;
    MetadataRecord       metadataRec;
} PartitionMapInfo;


#endif /* __UCT_UCTDATA_H__ */

