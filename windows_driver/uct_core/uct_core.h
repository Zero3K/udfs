/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uct_core.h
 *
 * Description : This file is meant to be included in an application
 *               using the uct_core library. Such an application
 *               should not use any other uct_core header file.
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __UCT_UCT_CORE_H__
#define __UCT_UCT_CORE_H__

/* Included uct_core header files.
 */
#include "general.h"        /* this one as first */
#include "mytypes.h"
#include "uctversion.h"
#include "device.h"
#include "crc.h"
#include "unicode.h"
#include "udfstruct.h"
#include "uctdata.h"
#include "ucterror.h"
#include "uctgeneral.h"
#include "uctfiles.h"
#include "uctstatus.h"
#include "uctmedium.h"
#include "uctendian.h"
#include "uctverify.h"
#include "ucttimestamp.h"


/* API definitions that do not occur in any other .h file go here
 */

extern void uctModuleVersion(char **version);

#endif /* __UCT_UCT_CORE_H__ */

