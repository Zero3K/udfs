/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : version.h
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __VERSION_H__
#define __VERSION_H__

/* Convention used for version info: Version X.YzA
 * X = Major
 * Y = Minor
 * z = d(development)
 *     a(alpha)
 *     b(beta)
 *     p(pre-release)
 *     r(release)
 * A = z-sub
 *
 * addition: " distribution"    : for executables   delivered in distribution
 *           " src"             : for sources files delivered in distribution
 *           " development"     : for development and pre-release versions
 */
static char* app_version = "1.5r6 distribution src";

#endif /* __VERSION_H__ */

