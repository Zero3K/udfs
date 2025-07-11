/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : scsi_device.h
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __SCSI_DEVICE_H__
#define __SCSI_DEVICE_H__

#include <stdlib.h>

#include "uct_core.h"
#include "platform.h"
#include "scsiapi.h"        /* may #define SCSI_DEVICE */


#ifdef  SCSI_DEVICE

extern FILE *scsiErrorOut;

/* device select function for scsi device:
 */
extern bool selectScsiDevice(int argc, char **argv, char *keyOption,
                             MediumInfo *clMediumInfo, Device **pDevice);

/* Check and execute showscsi option.
 * If -showscsi found, execute AND QUIT.
 * Note: The arugument list conforms to a device select function,
 *       so this function can be used in a device select table.
 */
extern bool checkShowScsi(int argc, char **argv, char *Option,
                          MediumInfo *miDummy, Device **devDummy);
#endif  /** SCSI_DEVICE **/

#endif /* __SCSI_DEVICE_H__ */

