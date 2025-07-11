/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : img_device.h
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __IMG_DEVICE_H__
#define __IMG_DEVICE_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uct_core.h"
#include "commandline.h"


/****** device select function for image device:
 *
 * EXCEPTION:
 *      <keyOption> is optional.
 *      This means that the return value will be TRUE if and
 *      only if any command line argument will be marked out.
 */

extern bool selectImageDevice(int argc, char **argv, char *keyOption,
                              MediumInfo *clMediumInfo, Device **pDevice);

#endif /* __IMG_DEVICE_H__ */

