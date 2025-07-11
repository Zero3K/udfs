/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : selectdevice.h
 *
 * Description : select a device for UDF verifier input.
 *
 * Author(s)   : Gerrit Scholl
 */

#ifndef __SELECTDEVICE_H__
#define __SELECTDEVICE_H__

#include "uct_core.h"

extern void selectDeviceUsage();
/*
 * Print device command line arguments usage.
 */

extern bool selectDevice(int argc, char **argv,
                         MediumInfo *clMediumInfo, Device **pDevice);
/*
 * Select and create device from dsFunctionsTable[] that recognizes
 * all remaining command line arguments.
 * Medium command line options are parsed already, and their values
 * are reflected in *clMediumInfo.
 *
 * return value: TRUE if ok, else FALSE
 */

#endif  /* __SELECTDEVICE_H__ */

