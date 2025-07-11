/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : drive_device.h
 *
 * Description :
 *
 * Author(s)   : cbiks@microsoft.com
 */

#ifndef __DRIVE_DEVICE_H__
#define __DRIVE_DEVICE_H__

/**** DRIVE_DEVICE:
 *    '-drive <device id>' option Drive letter device support
 *    Additional way of device support for Win32. Proper
 *    mounting of the UDF file system by the OS is required
 *    for proper functioning of DRIVE_DEVICE support.
 *
 * DRIVE_DEVICE only implemented for WIN32 so far.
 * Use NO_DRIVE_DEVICE to disable DRIVE_DEVICE, but further,
 * DRIVE_DEVICE will be used internally instead of NO_DRIVE_DEVICE !!!!!
 */
#if( (!defined WIN32) )     /* must be better: NT, 2000 or XP only */
#define NO_DRIVE_DEVICE
#endif

#ifdef  NO_DRIVE_DEVICE
#undef  DRIVE_DEVICE        /* DRIVE_DEVICE support disabled */
#else
#define DRIVE_DEVICE        /* default for WIN32, etc. */
#endif

#undef  NO_DRIVE_DEVICE     /* internally, only DRIVE_DEVICE must be used */

/****/

#ifdef  DRIVE_DEVICE        /* compile for DRIVE device only */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uct_core.h"
#include "commandline.h"

/****** device select function for drive_device support:
 * (-drive) <keyOption> is NOT optional for drive_device support.
 * This means that the return value will be TRUE if and only if
 * any command line argument will be marked out, which means
 * that at least <keyoption> was found on the command line.
 * Medium command line options are parsed already, and their values
 * are reflected in *clMediumInfo.
 */
extern bool selectDriveDevice(int argc, char **argv, char *keyOption,
                              MediumInfo *clMediumInfo, Device **pDevice);

#endif  /* DRIVE_DEVICE */

#endif /* __DRIVE_DEVICE_H__ */

