/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctfarapart.h
 *
 * Description : Far apart testing functions
 *
 * Author(s)   : Gerrit Scholl
 */

#ifndef __UCT_UCTFARAPART_H__
#define __UCT_UCTFARAPART_H__

#include "mytypes.h"
#include "general.h"
#include "uctnodes.h"

/* checkMetadataFarApart():
 * Check if metadata and mirror 'far apart', UDF 2.2.13.1.
 * Precondition:
 *     mc->metadataFile->node->al != NULL
 *  && mc->metadataMirrorFile->node->al != NULL
 *
 * Create two lists of ExtentAd extents, one for all extents
 * associated with the Metadata File and one for its mirror.
 * The extents involved are the (E)FE and AED descriptor extents and
 * if the Metadata Duplicate Flag is set also the file data extents.
 * Finally analize the two lists for physically 'nearby' ECC packets,
 * using faProcessLists().
 * ML OPT and PTP geometries are taken into account.
 */
extern bool checkMetadataFarApart( UdfMountContext *mc,
                                   bool dupFlagSet );

/* checkVDSFarApart():
 * Check if Main and Reserve VDS 'far apart', UDF 2.50+ 2.2.3.2.
 *
 * Create two lists of ExtentAd extents, one for all extents
 * associated with the Main VDS and one for the Reserve VDS.
 * The extents involved are VDS extents including possible continuation extents.
 * Finally analize the two lists for physically 'nearby' ECC packets,
 * using faProcessLists().
 * ML OPT and PTP geometries are taken into account.
 */
extern bool checkVDSFarApart( UdfMountContext *mc );

#endif /* __UCT_UCTFARAPART_H__ */

