/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : selectdevice.c
 *
 * Description : select a device for UDF verifier input.
 *
 * Author(s)   : Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>

#include "uct_core.h"

#include "commandline.h"
#include "platform.h"
#include "selectdevice.h"
#include "img_device.h"

/* For extra device support explanation, see platform.h.
 * and appropriate include file.
 */
#include "scsi_device.h"    /* may #define SCSI_DEVICE  */
#include "drive_device.h"   /* may #define DRIVE_DEVICE */
#include "raw_device.h"     /* may #define RAW_DEVICE   */

/* The purpose of this module is to define an interface for selecting
 * devices, to read the verifier input. It should be easy to add a
 * new type of device to the verifier.
 * For each device, a number of definitions have to be performed.
 *
 * Define a short string <keyStr> that identifies the type of device
 * to be added. <keyStr> must not be equal to an already existing
 * <keyStr> or verifier command line option.
 * <keyStr> defines a command line option <keyOption> == -<keyStr>,
 * that will select the device for reading.
 * Implementation code for the <keyStr> device will be located in a
 * separate module <keyStr>_device.c (and .h).
 * Selection will be performed by a device select function
 * select<KeyStr>Device(). This involves recognition of the <keyOption>
 * and ALL other remaining command line options and arguments.
 * IF AND ONLY IF ALL remaining arguments are recognized and ok, a
 * function create<KeyStr>Device() is called that creates the device
 * and collects medium information from the medium to be stored in a
 * MediumInfo structure that is located in the generic device structure.
 * Medium information collected from the medium can be merged with
 * - or overruled by - medium information defined on the command line.
 * The image device will collect medium information from the image
 * configuration file.
 * As last, a <KEYSTR>_DEVICE (all capitals, e.g. SCSI_DEVICE)
 * preprocessor definition will allow for conditional compilation
 * for the specific device, in case the device cannot be used on
 * all platforms.
 *
 * Existing <keyStr>s at the moment:
 *
 * image   : for image device
 * scsi    : for SCSI device (WIN32 only)
 *
 * Examples for scsi and image devices :
 *
 * <keyStr> <keyOption>      select function         module
 * -------- -----------     -------------------     ------------------
 *  scsi      -scsi         selectScsiDevice()      scsi_device.c + .h
 *  image    [-image]       selectImageDevice()      img_device.c + .h
 *
 *****
 * Perform the following steps to include a new device. The existing
 * "scsi" device can be taken as an example for all these steps,
 * so copy the appropriate "scsi" part and adapt it. Ignore the
 * special "showscsi" stuff.
 *
 * selectdevice.h:
 * - Include the correct <KEYSTR>_DEVICE preprocessor definition
 *   if appropriate, or define it at compile time.
 *
 * selectdevice.c:
 * - Add (conditionally compiled) #include "<keyStr>_device.h"
 * - Add (conditionally compiled) fprintf(uctout, ...) statements to
 *   the selectDeviceUsage() function in order to show the device
 *   usage in case of errors and for the verifier -help option.
 * - Add a (conditionally compiled) record to the dsFunctionsTable[]
 *   table. The record must contain the device select function
 *   select<KeyStr>Device() and the <keyOption>.
 * - Create a (conditionally compiled) select<KeyStr>Device() function
 *   that parses the device command line arguments and calls the
 *   create<KeyStr>Device() function to create and initialize the device.
 *   For detailed information see the select<KeyStr>Device() calling
 *   convention description below.
 *
 * <keyStr>_device.c and .h:
 * - Create a separate module <keyStr>_device.c (and .h) that holds the
 *   select<KeyStr>Device(), create<KeyStr>Device() functions and further
 *   device specific code.
 * - #include "uct_core.h". This includes the prototypes in device.h,
 *   uctmedium.h, etc. that are needed to set the members of the Device
 *   and MediumInfo structures. MediumInfo information collected from the
 *   medium can be put in the device MediumInfo structure using
 *   clearMediumInfo(), addSessionToMediumInfo() and by direct assignments
 *   of the enumerated values defined in uctmedium.h. MediumInfo information
 *   can also be defined by command line options. If so, their values are
 *   reflected in clMediumInfo. These values can be be merged with, or
 *   overrule information read from the medium, using mergeMediumInfo().
 *   Some members can also get a default value using finishMediumInfo().
 * - #include "commandline.h" : prototypes for command line parsing.
 */

/* Calling convention for all select<KeyStr>Device() functions:
 *
 *  bool (*dsFunct)(int   argc,   char   **argv, char *keyOption,
 *                  MediumInfo *clMediumInfo, Device **pDevice);
 *
 * Preconditions:
 * argc and argv are command line arguments as usual for c-programs.
 * argv[0] will be left unused and untouched.
 * Command line arguments that have already been processed in previous
 * steps, are marked out by a NULL pointer in the appropriate argv[x]
 * position. Among these are MediumInfo command line options. Their
 * values are reflected in the clMediumInfo structure to be merged with
 * MediumInfo values read from the medium. Command line options may
 * overrule information read from the medium if allowed by the device.
 *
 * Actions:
 * Each device select function shall parse the remaining command line
 * arguments and handle its <keyOption> and option argument (if any)
 * as well as possible additional options.
 * All command line arguments handled by the device select function shall
 * be marked out by a NULL pointer as described above in the preconditions.
 * If all required options have been handled correctly AND NO UNUSED
 * command line arguments are left, a create<KeyStr>Device() function
 * shall be called to initialize the device.
 *
 * return value in *pDevice:
 *   if no device is created
 *   then *pDevice = NULL,
 *   else a pointer to the correctly created device.
 *
 * function return value:
 *   if the <keyOption> command line argument was found
 *   and marked out, then: TRUE
 *                   else: FALSE
 *
 * The following cases can be distinguished:
 * case  return value    *pDevice
 *   1:     FALSE           NULL        <keyOption> not found
 *   2:      TRUE           NULL        device could not be created
 *   3:      TRUE   <correctly created device>
 *
 * case 1: The <keyOption> was not found, the calling function
 *         will try another device.
 * case 2: The <keyOption> was found, but still a command line
 *         argument error was found, or creating the device failed.
 *         The calling function can distinguish these cases by
 *         counting the remaining arguments and act accordingly.
 * case 3: This device was correctly selected and created and will
 *         be used to read from.
 *
 * The function selectDevice() will call the device selection
 * functions from dsFunctionsTable[] until a device is selected.
**********************************************************************
 */

/* Show device command line arguments usage.
 * Conditional compilation for appropriate devices.
 */
extern void selectDeviceUsage()
{
    static bool already_printed = FALSE;        /* print only once */
    if( already_printed )
    {
        return;
    }
    already_printed = TRUE;

    fprintf( uctout,
      "\nImage file options:\n"
        "  [-image] <chunk 1 path> [<chunk 2 path> ...]\n"
                  "\t\t# The total image file is the concatenation"
                                                " of all chunks.\n"
                  "\t\t# Each chunk is a concatenation"
                                                " of blocks and shall\n"
                  "\t\t# have a size that is a multiple"
                                                " of the UDF blockSize.\n"
           );

#ifdef  SCSI_DEVICE
    fprintf( uctout,
      "\nScsi device options:\n"
          "   -showscsi\t\t  # Show host adapter and scsi id "
                                            "of scsi devices\n"
          "   -scsi <h>:<s>"

#if defined( LINUX ) || defined( NETBSD )
                          " | <device path>"
#endif
                  "\n\t\t\t  # Read from scsi device, e.g. -scsi 2:3\n"
                    "\t\t\t  #   <h> is scsi host adapter number,\n"
                    "\t\t\t  #   <s> is scsi id number,\n"
                    "\t\t\t  #   use -showscsi to find numbers.\n"

#if defined( LINUX ) || defined( NETBSD )
                    "\t\t\t  # <device path> example: -scsi /dev/sg0\n"
#endif
           );

#else
    fprintf( uctout,
      "\nNOTE: This built of the UDF verifier does not support"
                                " the -showscsi and -scsi options.\n\n");
#endif  /** SCSI_DEVICE **/

}   /* end selectDeviceUsage() */


/* Device select functions table.
 * Conditional compilation for appropriate records.
 */
typedef struct      /* device select table record */
{
    bool (*dsFunct)(int   argc,   char   **argv, char *keyOption,
                    MediumInfo *clMediumInfo, Device **pDevice);
    char  *dsKeyOption;
} dsRecord;

static dsRecord dsFunctionsTable[] =
{
#ifdef  SCSI_DEVICE
    {checkShowScsi,     "-showscsi"},   /* exception: show scsi info and quit */
    {selectScsiDevice,  "-scsi"},       /* select scsi device */
#endif

#ifdef  DRIVE_DEVICE                    /* 'drive' device support */
    {selectDriveDevice, "-drive"},      /* select 'drive' device */
#endif

#ifdef  RAW_DEVICE                      /* 'raw' device support */
    {selectRawDevice,   "-raw"},        /* select 'raw' device */
#endif

    {selectImageDevice, "-image"}       /* select image device */
};


/* NO conditional compilation beyond this point **********************
 *
 * selectDevice():
 * Select and create device from dsFunctionsTable[] that recognizes
 * all remaining command line arguments.
 * Medium command line options are parsed already, and their values
 * are reflected in *clMediumInfo.
 *
 * return value: TRUE if ok, else FALSE
 */
extern bool selectDevice(int argc, char **argv,
                         MediumInfo *clMediumInfo, Device **pDevice)
{
    char *keyOption = NULL;
    bool  result;
    int   i, countUnused;

    /* parse device options and create a device if appropriate
     */
    fprintf(uctout, "Device options parsing:\n");

    /* find device that recognizes at least one command line argument.
     */
    for( i = 0, result = FALSE, (*pDevice) = NULL;
         i < (sizeof(dsFunctionsTable)/sizeof(dsRecord))
             && result == FALSE         /** no argument parsed **/
             && (*pDevice) == NULL;     /** no device selected **/
         i++ )
    {
        keyOption =  dsFunctionsTable[i].dsKeyOption;
        result    = (dsFunctionsTable[i].dsFunct)(argc, argv, keyOption,
                                                  clMediumInfo, pDevice);
        /* special check:
         * -dummysession option handled correctly on command line
         * and for all devices, but allowed for image device only.
         * Check all 'MediumInfo'->dummySessionTotalBlocks.
         */
        if(   result == TRUE && (*pDevice) != NULL
           && dsFunctionsTable[i].dsFunct != selectImageDevice
           && (            clMediumInfo->dummySessionTotalBlocks != 0
               || (*pDevice)->mediumInfo.dummySessionTotalBlocks != 0) )
        { fprintf(uctout,
            "\nError, -dummysession option allowed for image device only\n");
          return FALSE;     /* fail */
        }
    }
    VERBOSE00(uctout, "\n");

    /* Check if command line arguments unused/unknown.
     */
    if( (countUnused = clCountValidArguments(argc, argv)) != 0 )
    {                               /* still unused arguments */
        clPrintRemainingArguments(argc, argv,
                    "\nUnknown, misplaced or unused command line arguments:");
        fprintf(uctout, "\n");
        if( (*pDevice) != NULL )
        {
            fprintf(uctout, "Error: Program error, no device should be selected,"
                                                        " please report\n");
            *pDevice = deviceCloseAndFreeDevice( *pDevice );
            return FALSE;
        }
    }

    if( result == FALSE )       /* no device arguments recognized at all */
    {
        fprintf(uctout, "\nError in device or image selection arguments\n");
        return FALSE;           /* error */
    }

    if( (*pDevice) == NULL )
    {
        fprintf(uctout, "\nCannot create \'%s\' device.\n", keyOption); /* -<keyStr> */
        if( countUnused != 0 )
        { fprintf(uctout, "Maybe due to erroneous or unprocessed command line arguments.\n");
        }
        return FALSE;       /* error */
    }

    /* check mediumInfo consistency, set defaults and print medium info.
     */
    if( !finishMediumInfo( &(*pDevice)->mediumInfo ) )
    {   return FALSE;
    }
    VERBOSE00(uctout, "\n\'%s\' device Medium Info:\n", keyOption);
    printMediumInfo( &(*pDevice)->mediumInfo );

    return TRUE;            /* ok */

}   /* end selectDevice() */

