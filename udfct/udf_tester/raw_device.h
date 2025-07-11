/* Copyright (c) 2002
 *
 * File        : raw_device.h
 *
 * Description : Raw device support for MAC OS X
 *               Incorporated in UDF verifier by courtesy of Apple Inc.
 *
 * Author(s)   : John Bertagnolli - Apple Inc
 */

#ifndef __RAW_DEVICE_H__
#define __RAW_DEVICE_H__

/**** RAW_DEVICE:
 *    '-raw <device id>' option Raw device support for Macintosh OS X.
 *    Proper mounting of the UDF file system by the OS is required for
 *    proper functioning of RAW_DEVICE support.
 *
 * RAW_DEVICE only implemented for MACOSX so far.
 * Use NO_RAW_DEVICE to disable RAW_DEVICE, but further,
 * RAW_DEVICE will be used internally instead of NO_RAW_DEVICE !!!!!
 */
#if( (!defined MACOSX) )
#define NO_RAW_DEVICE
#endif

#ifdef  NO_RAW_DEVICE
#undef  RAW_DEVICE          /* RAW_DEVICE support disabled */
#else
#define RAW_DEVICE          /* default for MACOSX, etc. */
#endif

#undef  NO_RAW_DEVICE       /* internally, only RAW_DEVICE must be used */

/****/

#ifdef  RAW_DEVICE          /* compile for RAW device only */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uct_core.h"
#include "commandline.h"


/****** device select function for raw_device support:
 * (-raw) <keyOption> is NOT optional for raw_device support.
 * This means that the return value will be TRUE if and only if
 * any command line argument will be marked out, which means
 * that at least <keyoption> was found on the command line.
 * Medium command line options are parsed already, and their values
 * are reflected in *clMediumInfo.
 */
extern bool selectRawDevice(int argc, char **argv, char *keyOption,
                            MediumInfo *clMediumInfo, Device **pDevice);

#endif  /* RAW_DEVICE */

#endif /* __RAW_DEVICE_H__ */

