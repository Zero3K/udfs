/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : img_device.c
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#include <stdio.h>
#include <ctype.h>

#include "uct_core.h"
#include "commandline.h"
#include "mfile.h"
#include "img_device.h"


typedef struct
{
    MFileHandle  mfileHandle;
} ImageImpUse;


/* TODO: Image get block state function, only able to detect
 *       unrecorded blocks with extended image format
 */
static BlockState imageGetBlockState( void *impUse, Uint32  blockNr )
{
    impUse  = impUse;       /* make compiler happy */
    blockNr = blockNr;      /* make compiler happy */
    return BSTATE_UNKNOWN;
}

static Uint32 imageReadBlocks(void  *impUse,
                              Uint32 blockSize,
                              Uint32 firstBlock,
                              Uint32 nrOfBlocks,
                              Byte  *buffer )
{
    return mfileReadBlock(((ImageImpUse*)impUse)->mfileHandle,
                    blockSize, firstBlock, nrOfBlocks, buffer);

}   /* end imageReadBlocks() */


/* close image device and free space allocated by image impUse
 */
static void imageCloseAndFreeImpUse(void *impUse)
{
    if( impUse != NULL )
    {   mfileClose(((ImageImpUse*) impUse)->mfileHandle);
        free(impUse);
    }
}

/* Fill Device API function table for image device.
 */
static void imageInitializeFunctionTable(Device *device)
{
    device->getBlockState      = imageGetBlockState;
    device->readBlock          = imageReadBlocks;
    device->closeAndFreeImpUse = imageCloseAndFreeImpUse;
}


/* getConfigFileInfo():
 * Get Medium information from image configuration file, if
 * present.
 * This configuration file holds Medium information in the
 * same way as the appropriate medium command line options
 * on the command line, except hat there may be any white
 * space (including newline) characters in between arguments
 * and no fancy things like white space within arguments,
 * excape sequences or line continuations are supported.
 *
 * It is assumed that imageChunk1Path holds a correct pathname.
 * Creation of the configuration file path from the first chunk
 * of the image file path is done as follows:
 * - strip off final extension from image path, if any.
 * - add ".cfg" extension to image path.
 *
 * Return value: FALSE if any error occurred or an unknown 'option'
 *                     was found in the configuration file.
 *         else: TRUE;
 */
static bool getConfigFileInfo(char *imageChunk1Path, MediumInfo *mi)
{
    size_t   len;
    char    *p1, *p2;
    FILE    *cfgHandle;
    char    *cfgExtension = ".cfg",
            *cfgPath,
            *cfgBuffer;
    Uint64   cfgSize64;
    Uint32   cfgSize;
    int      cfgArgc;
    char   **cfgArgv;
    bool     overruled;

    clearMediumInfo(mi);        /* initialize */

    /* let's see if a configuration file exists.
     * determine nmb of chars to be copied from imageChunk1Path
     */
    p1 = strrchr(imageChunk1Path, (int) '\\');
    p2 = strrchr(imageChunk1Path, (int) '/');
    if(      p1 == NULL ) p1 = p2;
    else if( p2 != NULL ) p1 = (p1>p2) ? p1 : p2;

    /* p1 == NULL or points to last occurrence of '\\' or '/'
     */
    p2 = strrchr(imageChunk1Path, (int) '.');

    if(     p2 != NULL                  /* '.' found, maybe extension */
        && (p1 == NULL || p2 > p1) )
    {
        len = p2 - imageChunk1Path;     /* strip off extension */
    }
    else
    {
        len = strlen(imageChunk1Path);  /* no image filename extension found */
    }

    if( (cfgPath = (char *) tst_malloc( 1 + len + sizeof(cfgExtension),
                                        __FILE__,__LINE__)) == NULL )
    {
        uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    memcpy( cfgPath, imageChunk1Path, len ); cfgPath[len] = '\0';
    strcat( cfgPath, cfgExtension );

    if( (cfgHandle = plFileOpenWithSize(cfgPath, &cfgSize64)) == NULL )
    {   /* unable to find size, show cause */
        if( cfgSize64 == (Uint64) 0L )
             fprintf(uctout,"No image configuration file %s\n",
                                                        cfgPath);
        else fprintf(uctout,"Error: Find file size failed for: %s\n",
                                                        cfgPath);
        free(cfgPath);
        return TRUE;
    }
    cfgSize = (Uint32) cfgSize64;

    if( cfgSize > 10*1024 || ((Uint64) cfgSize) != cfgSize64 )
    { fprintf(uctout,
        "Error: Configuration file bigger than 10 Kbyte not accepted: %s\n",
                            cfgPath);
      free(cfgPath);
      fclose(cfgHandle);
      return FALSE;
    }

    if( (cfgBuffer = (char *) tst_malloc( cfgSize + 1,
                                          __FILE__,__LINE__)) == NULL )
    {
        free(  cfgPath);
        fclose(cfgHandle);
        uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    if( fread( cfgBuffer, 1, cfgSize, cfgHandle) != cfgSize )
    {
        fprintf(uctout,"Error: fread error reading: %s\n", cfgPath);
        free(  cfgPath);
        free(  cfgBuffer);
        fclose(cfgHandle);
        return FALSE;
    }
    cfgBuffer[cfgSize] = '\0';      /* terminate */
    fclose(cfgHandle);

    /* count arguments.
     * No fancy things like white space within arguments,
     * excape sequences or line continuations supported.
     */
    cfgArgc = 0;
    p1      = cfgBuffer;
    while( *p1 != '\0' )
    {
        while( isspace((Uint8)*p1) ) p1++;
        if( *p1 != '\0' )
        {
            cfgArgc++;
            while( !isspace((Uint8)*p1) && *p1 != '\0' ) p1++;
        }
    }
    if( ((Uint32)(p1 - cfgBuffer)) != cfgSize )
    {
        fprintf(uctout,"Error: null character found in"
                         " configuration file: %s\n", cfgPath);
        free(cfgPath);
        free(cfgBuffer);
        return FALSE;
    }
    if( cfgArgc == 0 )
    {
        fprintf(uctout,"Empty image configuration file\n");
        free(cfgPath);
        free(cfgBuffer);
        return TRUE;        /* ok, no config file medium info */
    }

    /* create argc, argv -like structures, reading configuration file.
     * Mind that cfgArgv[0] will not be used, and that an extra last
     * cfgArgv[] pointer must always be NULL !!! So: 2 extra pointers.
     * Use tst_calloc for cfgArgv, so initializing on all NULL pointers.
     */
    if( (cfgArgv = (char **) tst_calloc( cfgArgc + 2, sizeof(char *),
                                          __FILE__,__LINE__)) == NULL )
    {   free(cfgPath);
        free(cfgBuffer);
        uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }

    /* parseMediumOptions will not free the memory where any
     * cfgArgv[] is pointing to, bus simply set the pointer to NULL when
     * marking out an argument, so we will not allocate memory for every
     * cfgArgv string, but let the cfgArgv pointers point directly into
     * cfgBuffer, and replace the closing white space char by a terminator.
     * let's do it.
     */
    cfgArgc = 1;    /* skip cfgArg[0] which is ignored in options parsing */
    p1      = cfgBuffer;
    while( p1 < (cfgBuffer + cfgSize) ) /* different end criterium because */
    {                                   /*  of terminating each string.    */
        while( isspace((Uint8)*p1) ) p1++;
        if( *p1 != '\0' )
        {
            cfgArgv[cfgArgc++] = p1;
            while( !isspace((Uint8)*p1) && *p1 != '\0' ) p1++;
            *p1++ = '\0';               /* terminate AND SKIP !! */
        }
    }

    /* Last argument: cfgArgv[cfgArgc] == NULL
     * Parse configuration file options now
     * *mi will (again) be "cleared" first
     */
    fprintf(uctout, "Configuration file parsing:\n\t%s\n", cfgPath);
    free(cfgPath);          /* not needed any more */

    if( !parseMediumOptions(cfgArgc, cfgArgv, mi, &overruled) )
    {
        free(cfgBuffer);
        free(cfgArgv);
        return FALSE;
    }
    if( overruled )
    {
        fprintf(uctout,
            "Error: No overruling allowed within configuration file\n");
        free(cfgBuffer);
        free(cfgArgv);
        return FALSE;
    }
    if( clCountValidArguments(cfgArgc, cfgArgv) != 0 )
    {
        clPrintRemainingArguments(cfgArgc, cfgArgv,
                    "Error: Unknown configuration file arguments:");
        fprintf(uctout, "\n");
        free(cfgBuffer);
        free(cfgArgv);
        return FALSE;
    }
    free(cfgBuffer);
    free(cfgArgv);
    return TRUE;
}


/* Initialize for image file
 * Note that in case of an error, cleanup is done at the
 * calling side, using imageCloseAndFreeImpUse(imageImpUse);
 */
static bool initializeImageDevice(int argc, char **argv,
                                  MediumInfo *clMediumInfo,
                                  Device     *device)
{
    static MFile        mfile = {0,NULL,NULL,NULL};
           Uint32       imageLastBlockNr;
           Uint32       totalDummyBlocks,
                        chunkBlocks;
           int          index;
           MediumInfo  *dmi = &device->mediumInfo;

    index = clFindArgument(argc, argv, NULL);   /* image file first chunk path */

    /* Get MediumInfo from image configuration file,
     * its path is derived from image file first chunk path.
     */
    if( index == 0 || !getConfigFileInfo(argv[index], dmi) )
    {   return FALSE;                       /* error */
    }
    VERBOSE00(uctout, "\n");

    /* merge device mediumInfo and command line mediumInfo
     * result to imageImpUse mediumInfo.
     * overrule by command line MediumInfo allowed.
     */
    (void) mergeMediumInfo( clMediumInfo, dmi, dmi,
                            "command line", "configuration file" );

    /* Set default blockSize if no blockSize defined, because we
     * need it for opening mfile chunks.
     */
    if( dmi->blockSize == 0 )
    {   dmi->blockSize = MI_DEFAULT_BLOCKSIZE;
    }

    /* first create dummy chunks, if any
     */
    for( totalDummyBlocks = 0;
         totalDummyBlocks < dmi->dummySessionTotalBlocks;
         totalDummyBlocks += chunkBlocks )
    {   chunkBlocks =
          MIN((Uint32)(MFILE_MAX_CHUNK_SIZE / dmi->blockSize),
              dmi->dummySessionTotalBlocks - totalDummyBlocks);

        if( !mfileAppendChunk(&mfile, MFILE_DUMMY_CHUNK_PATH,
                               dmi->blockSize,
                               chunkBlocks) )
        {   mfileClose(&mfile);
            return FALSE;           /* abort */
        }
    }

    /* open all 'normal' chunks and append to mfile
     */
    for( ; index != 0; index = clNextArgument(argc, argv, index) )
    {
        if( !mfileAppendChunk(&mfile, argv[index],
                               dmi->blockSize, 0) )
        {   mfileClose(&mfile);
            argv[index] = NULL;     /* error, but mark out */
            return FALSE;
        }
        argv[index] = NULL;     /* not needed any more, mark out */
    }
    ((ImageImpUse*)device->impUse)->mfileHandle = &mfile;

    /* finishMediumInfo() does not always set default for lastValidBlockNr,
     * set here to last image block.
     */
    imageLastBlockNr = mfileGetTotalBlocks(&mfile) - 1;

    if( dmi->lastValidBlockNr == 0 )    /* set its default */
    {
        dmi->lastValidBlockNr = imageLastBlockNr;
    }
    dmi->lastRecordedBlockNr = MIN(dmi->lastValidBlockNr,
                                   imageLastBlockNr);

    if( dmi->lastRecordedBlockNr > imageLastBlockNr )
    {
        fprintf(uctout,
            "Fatal error: Last recorded block number greater than last\n"
            "-            image block number (%u, %u)\n",
            dmi->lastRecordedBlockNr, imageLastBlockNr);
        uctExit(EXIT_NON_CONFORMANT);   /* quit */
    }

    return TRUE;

}   /* end initializeImageDevice() */

static Device *createImageDevice(int argc, char **argv,
                                 MediumInfo *clMediumInfo)
{
    Device      *device;
    MFileHandle  mfh;
    Uint32       n, beginOffset;

    /* Create Device structure.
     * Use tst_calloc() so that all members that might
     * not be initialized are 0, NULL or FALSE.
     */
    if(    (device       = (Device*) tst_calloc(sizeof(Device), 1,
                                                __FILE__, __LINE__)) == NULL
        || (device->impUse = (void*) tst_calloc(sizeof(ImageImpUse), 1,
                                                __FILE__, __LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    clearMediumInfo( &device->mediumInfo );

    /* Get MediumInfo from image configuration file,
     * its path is derived from image file first chunk path.
     * Then initialize image device impUse
     */
    if( !initializeImageDevice(argc, argv, clMediumInfo, device) )
    {
        imageCloseAndFreeImpUse(device->impUse);
        free(device);
        return NULL;        /* error */
    }

    /* Image device impUse ok now, install access functions
     * there exists at least one image file chunk
     */
    imageInitializeFunctionTable(device);

    fprintf(uctout, "\nCreated image device\n");
    mfh = ((ImageImpUse*)device->impUse)->mfileHandle;
    beginOffset = 0;
    for( n = 0; n < mfh->nrOfChunks; n++ )
    {   fprintf(uctout, "%s: %-8lu %s\n",
            (n == 0) ? "  image file blocks\t"
                     : "\t\t\t",
            mfh->blockOffsetNextChunk[n] - beginOffset,
            (mfh->filePaths[n] == MFILE_DUMMY_CHUNK_PATH)
              ? "<dummy chunk>"
              : mfh->filePaths[n]);
        beginOffset = mfh->blockOffsetNextChunk[n];
    }
    n = mfileGetTotalBlocks(mfh);
    fprintf(uctout,
            "     blocks, bytes\t: %-8lu ", n);
    printUint64(VERBOSE00level,
        (Uint64) n * (Uint64) device->mediumInfo.blockSize,
                FALSE, NULL);
    fprintf(uctout, "\n");
    return device;

}   /* end createImageDevice() */

/****** device select function for image device:
 *
 * EXCEPTION:
 *      <keyOption> is optional.
 *      This means that the return value will be TRUE if and
 *      only if any command line argument will be marked out.
 *
 * Medium command line options are parsed already, and their values
 * are reflected in *clMediumInfo.
 */
extern bool selectImageDevice(int argc, char **argv, char *keyOption,
                              MediumInfo *clMediumInfo, Device **pDevice)
{
    int      n, index, nextIndex, count;
    bool     result;

    result   = FALSE;           /* no command line argument recognized */
    *pDevice = NULL;            /* no device created yet */

    index = clFindArgument(argc, argv, keyOption);
    if( index != 0 )        /* key option found, must be first argument */
    {   n = clFindArgument(argc, argv, NULL);   /* find first argument */
        if( n != index || argv[index+1] == NULL )
        {   fprintf(uctout, "\nError: misplaced %s option\n", keyOption);
            return FALSE;
        }
        argv[index] = NULL; /* mark out keyOption */
        result = TRUE;
        fprintf(uctout,"\t%s\n", keyOption);
    }

    /* All that should remain now is a contiguous list of
     * <image file chunks list> arguments, no options any more.
     * Show all chunks, test if arguments are contiguous,
     * do not mark them out yet.
     * Image file chunk arguments will be marked out when the
     * chunk file is opened and appended to the mfile structure.
     */
    if( clCountOptions(argc,argv) != 0 )
    {   return result;      /* error, unprocessed options left */
    }
    count = 0;
    for( index = 0; (nextIndex = clNextArgument(argc, argv, index)) != 0; )
    {   fprintf(uctout,"\t%s\n", argv[nextIndex]);
        if( count != 0 && nextIndex != index + 1 )
        {   fprintf(uctout, "\nError: <image file chunks list> "
                    "arguments must be adjacent on command line\n");
            return result;
        }
        index = nextIndex;
        count++;
    }
    fprintf(uctout,"\n");
    if( count == 0 )
    {   fprintf(uctout, "\nError: Empty <image file chunks list>\n");
        return result;
    }
    result = TRUE;          /* all further arguments will be marked out */

    /* end of command line argument parsing
     * create image device, verify <image file chunks list>
     */
    *pDevice = createImageDevice( argc, argv, clMediumInfo );

    return result;
}

