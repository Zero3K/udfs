/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : device.h
 *
 * Description : Generic Device interface
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __UCT_DEVICE_H__
#define __UCT_DEVICE_H__

#include "mytypes.h"
#include "uctmedium.h"


/* Block state values definition for getBlockState() functions
 * TODO: getBlockState() is not used/implemented yet !!!
 */
typedef enum
{
    BSTATE_UNKNOWN = 0,
    BSTATE_UNRECORDED,
    BSTATE_READABLE,
    BSTATE_ERROR
} BlockState;

/* Text equivalents for printing, etc.
 */
#define    BSTATE_TEXT(X)   \
    (((X)==BSTATE_UNKNOWN)    ? "unknown"   :\
     ((X)==BSTATE_UNRECORDED) ? "unrecorded":\
     ((X)==BSTATE_READABLE)   ? "readable"  :\
     ((X)==BSTATE_ERROR)      ? "error"     :\
                                "(illegal)")

/* Generic Device structure
 * impUse        :  Pointer to device specific structure to
 *                  store device specific information;
 * Function table:  Device specific implementations of
 *                  generic device functions.
 */
typedef struct Device
{
    MediumInfo   mediumInfo;
    void        *impUse;        /* pointer to device specific info */

    /* Device API function table
     */
    BlockState  (*getBlockState) ( void *impUse, Uint32 blockNr );
    Uint32      (*readBlock)     ( void *impUse, Uint32 blockSize,
                                   Uint32 firstBlock, Uint32 nrOfBlocks,
                                   Byte *buffer );
    void    (*closeAndFreeImpUse)( void *impUse);
} Device;

/*
 * Generic Device API Functions
 */
extern BlockState  deviceGetBlockState ( Device *device, Uint32 blockNr );
extern Uint32      deviceReadBlock     ( Device *device, Uint32 firstBlock,
                                         Uint32  nrOfBlocks, Byte *buffer,
                                         bool    allowBelowVRS);
extern Device *deviceCloseAndFreeDevice( Device *device );

/* deviceReadBlockRaw(): Called by deviceReadBlock().
 * Raw device reading, maybe use simple read cache, depending
 * on the value of (global) uctUseReadCache.
 * Fake read if buffer == NULL OR nrOfBlocks == 0 (no action).
 *
 * If cache read, then split up read request in READCACHE_PACKET
 * aligned requests for deviceReadCachePacket().
 * Begin-and-end aligned packets do NOT use cache read !!
 * See detailed comment in implementation body.
 */
extern Uint32 deviceReadBlockRaw( Device *device, Uint32 firstBlock,
                                  Uint32 nrOfBlocks, Byte *buffer );

/* deviceReadCacheFree():
 * Free all space allocated for read cache and bring it
 * back to initial consistent empty state.
 * Also used to force medium access from initial state
 */
extern void deviceReadCacheFree();

#endif /* __UCT_DEVICE_H__ */

