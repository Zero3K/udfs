/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : general.c
 *
 * Description : Non UDF or uct (except uctout) specific general functions.
 *
 * Author(s)   : Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mytypes.h"
#include "general.h"
#include "uctgeneral.h"     /* for uctout */
#include "ucterror.h"


/* Global Int8 uctVerboseLevel for printing.
 * It holds the <level> value of the
 * "-verbose <option_level>" command line option.
 * extern */
 Int8 uctVerboseLevel = VERBOSEdefault;

/* Global Int32 uctMessageLimit.
 * It holds the <limit> value of the
 * "-mlimit <limit>" command line option.
 * extern */
 Int32 uctMessageLimit = MLIMITdefault; /* Int32 !! */

/* Global count of errors and warnings for verbose
 * levels ERROR00level and WARN01level respectively.
 * Set by the MLIMITbegin macro.
 * The ...Count is incremented when a MLIMITbegin macro
 * is entered for the first time. The ...Occurrences
 * is incremented for each time.
 * all extern */
 Uint32 uctGlobalErrorCount     = 0;
 Uint32 uctGlobalErrorOccurrences = 0;
 Uint32 uctGlobalWarningCount     = 0;
 Uint32 uctGlobalWarningOccurrences = 0;

/* Global file body sizes and CRC
 * all extern */
 Uint64 uctFileBodySize    = 0;
 Uint16 uctFileBodyCrc     = 0;
 Uint64 uctFileBodyCrcSize = 0;

/* Global bool uctDoFileCRC, default FALSE.
 * Set to TRUE if -filecrc option defined.
 * extern */
 bool uctDoFileCRC = FALSE;     /* default */

/* Global bool uctDoFakeRead, default TRUE.
 * Set to FALSE if -nofakeread option defined.
 * uctDoFakeRead is ignored if uctDoFileCRC == TRUE
 * extern */
 bool uctDoFakeRead = TRUE;     /* default */

/* Global bool uctDoUTCtime, default TRUE.
 * Set to FALSE if -localtime option defined.
 * Used to print local time instead of UTC with dir listings.
 * extern */
 bool uctDoUTCtime = TRUE;      /* default */

/* Global bool uctDoShowAlloc, default FALSE.
 * Set to TRUE if -showalloc option defined.
 * Used to print lists of allocated and unallocated
 * contiguous areas for each partition.
 * extern */
 bool uctDoShowAlloc = FALSE;   /* default */

/* Global bool uctDoShowPerm, default TRUE.
 * Set to FALSE if -noperm option defined.
 * Shows (E)FE permissions in dir listing.
 * extern */
 bool uctDoShowPerm = TRUE; /* default */

/* Global bool uctIgnoreAVDP256, default FALSE.
 * Set to TRUE if -ignoreAVDP256 option defined.
 * E.g. used to follow 'old' AVDP at 512
 * extern */
 bool uctIgnoreAVDP256 = FALSE; /* default */

/* Global bool uctUseReadCache, default TRUE.
 * Not recommended for media with linkblocks in
 * volume space like CD-R.
 * Set to FALSE if -nocache option defined.
 * extern */
 bool uctUseReadCache = TRUE;   /* default */

/* Global bool uctDoReadGap, default FALSE.
 * If TRUE, try to read in unrecorded gaps.
 * Set to TRUE if -readgap option defined.
 * extern */
 bool uctDoReadGap = FALSE;     /* default */

/* Global bool uctUseMetadataMirror, default FALSE.
 * Set to TRUE if -usemirror option defined.
 * extern */
 bool uctUseMetadataMirror = FALSE; /* default */


/* Function used in MLIMITbegin/MLIMITend clause for
 * message printing and global error/warning counting.
 * The MLIMITbegin macro declares a static instance
 * of the MLIMITstruct structure, where the specific
 * context for a message is stored. This MLIMITstruct
 * is initialized to {0,0,0,0} once.
 * MLIMITbeginFunction() handles the global
 * error counting, etc and returns TRUE if
 * the MLIMIT body must be executed.
 * If the MLIMIT body is executed, the MLIMITbegin/MLIMITend
 * clause is closed by a MLIMITendFunction() call.
 *
 * Note that counting of uctGlobalErrorCount and uctGlobalWarningCount
 * can both only occur once for each MLIMITbegin/MLIMITend clause.
 * This count is independent of the value of mls->cnt, also because
 * mls-cnt can be reset by a value zero for messageLimit.
 */
extern bool MLIMITbeginFunction( MLIMITstruct *mls,
                                 Uint8 verboseLevel,
                                 Int32 messageLimit )
{   bool executeBody = FALSE;


    /* global error/warning counting depends
     * on the value of verboseLevel
     */
    mls->vl = verboseLevel;
    if(      verboseLevel == ERROR00level )    /* error message */
    {   if( mls->err == 0 )             /* not yet counted here */
        {    uctGlobalErrorCount++;     /* count only once here */
             mls->err = 1;              /* avoid counting again */
        }
        uctGlobalErrorOccurrences++;    /* count all occurences */
    }
    else if( verboseLevel == WARN01level )   /* warning message */
    {   if( mls->wrn == 0 )             /* not yet counted here */
        {    uctGlobalWarningCount++;   /* count only once here */
             mls->wrn = 1;              /* avoid counting again */
        }
        uctGlobalWarningOccurrences++;  /* count all occurences */
    }

    if( messageLimit == 0 ) /* side feature */
    {   mls->cnt = 0;       /*  reset cnt   */
    }
    executeBody = FALSE;
    ifVERBOSE(verboseLevel)
    {   executeBody = TRUE; /* verbose level ok for excecution */
        /* now test if message limit exhausted
         */
        if( messageLimit >= MLIMITinfinite )    /* infinite  */
        { messageLimit = 0;                /* 'all infinite' */
        }
        messageLimit = MIN(messageLimit, uctMessageLimit);
        if(    mls->cnt >= messageLimit
            && messageLimit != 0 )         /* 'all infinite' */
        { executeBody = FALSE;    /* message limit exhausted */
        }
    }
    ENDif;

    /* prepare for MLIMITbeginFunction()
     */
    mls->endExecPrintmessage = 0;       /* for MLIMITendFunction() */
    if( executeBody )       /* verbose ok and mlimit not exhausted */
    { mls->cnt++;
      if(   mls->cnt == messageLimit    /* last time before exhaust */
         && messageLimit != 0 )
      { mls->endExecPrintmessage = 1;   /* for MLIMITendFunction() */
      }
    }
    return executeBody;

}       /* end MLIMITbeginFunction() */

/* MLIMITendFunction():
 * Executed as last of the MLIMIT body.
 * Note that this function is only executed directly
 * after the MLIMIT body.
 */
extern void MLIMITendFunction( MLIMITstruct *mls )
{
    if( mls->endExecPrintmessage != 0 )
    { fprintf(uctout, "- ==>\tMessage printed ");
      if( mls->cnt == 1 )
           fprintf(uctout, "once");
      else fprintf(uctout, "%lu times", mls->cnt);

      fprintf(uctout, ", ignored from now.\n");
    }
}       /* end MLIMITendFunction() */

/* return TRUE if value u is found in array,
 *   else FALSE
 */
extern bool inArrayUint32(Uint32 u, Uint32 *array, int len)
{
    int i;
    for( i=0; i < len; i++)
    {
        if( array[i] == u ) return TRUE;
    }
    return FALSE;
}

/* Memory allocation stuff **********************************
 *
 * checkFree(): If *buff != NULL, then free(*buff)
 * and set *buff = NULL.
 */
extern void checkFree( void **buff )
{
    if( *buff != NULL ) {
        free(*buff);
        *buff = NULL;
    }
}

/* initForAlloc(): Initialize before use of tst_*alloc()
 * functions. Claim some spare memory for release
 * at 'out of memory' problems, see tst_alloc.
 */
#define ALLOC_SPARE_MEMORY_01   (4*1024*1024)   /* bytes */
#define ALLOC_SPARE_MEMORY_02   (   128*1024)   /* bytes */

static Byte *allocSpareMemory01 = NULL;
static Byte *allocSpareMemory02 = NULL;

extern void initForAlloc()
{
///**/static Byte *allocSpareMemoryTESTING = tst_malloc(470000000,
///**/                                      __FILE__, __LINE__);
    if( allocSpareMemory01 == NULL )
    { allocSpareMemory01 = tst_malloc(ALLOC_SPARE_MEMORY_01,
                                      __FILE__, __LINE__);
    }
    if( allocSpareMemory02 == NULL )
    { allocSpareMemory02 = tst_malloc(ALLOC_SPARE_MEMORY_02,
                                      __FILE__, __LINE__);
    }
    if(    allocSpareMemory01 == NULL
        || allocSpareMemory02 == NULL )
    { uctExit(EXIT_OUT_OF_MEMORY);      /* quit */
    }
}

/* test result of all other tst_*alloc() functions below
 * message if allocation error.
 * Return value: input pointer.
 */
static void *tst_alloc( void *result, char *ident, char *file,
                        int line, size_t size )
{
    if( result == NULL )    /* out of memory problem */
    { /* free spare memory in order to keep the
       * system running for error messages, etc.
       * Mind:  free(allocSpareMemory01) may not be effective
       * before free(allocSpareMemory02) is executed.
       */
      checkFree((void**)&allocSpareMemory01);
      checkFree((void**)&allocSpareMemory02);
      fflush(uctout);

      MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
          "\n Fatal error: ");
        if( strcmp(ident, "tst_realloc") != 0 )
        { fprintf(uctout," ");
        }
        fprintf(uctout,
          "%s: Out of memory, requesting %u bytes\n"
                 "-\tfile: %s\n"
                 "-\tline: %u\n",
                    ident, size, file, line);
      MLIMITend;
    }
    fflush(uctout);
    return result;
}

/* tst_malloc():
 * Call malloc(), message if error result.
 */
extern void *tst_malloc(size_t size, char *file, int line)
{
    fflush(uctout);
    if(size == 0) size = 1;     /* avoid zero bytes allocation */
    return tst_alloc(malloc(size), "tst_malloc", file, line, size);
}

/* tst_calloc():
 * Call calloc(), message if error result.
 */
extern void *tst_calloc(size_t num, size_t size, char *file, int line)
{
    fflush(uctout);
    if((num*size) == 0) {num = 1; size = 1; } /* avoid zero bytes allocation */
    return tst_alloc(calloc(num,size), "tst_calloc", file, line, num * size);
}

/* tst_realloc():
 * Call realloc(), message if error result,
 * acts like malloc() if mem is NULL.
 */
extern void *tst_realloc(void *mem, size_t size, char *file, int line)
{
    fflush(uctout);
    if(size == 0) size = 1;     /* avoid zero bytes allocation */

#undef  REALLOC_TESTING         /* normally #undef */
#ifndef REALLOC_TESTING         /* normal operation */
    return tst_alloc(realloc(mem,size), "tst_realloc", file, line, size);
    /* done */

#else   /* Testing: mimic realloc by a malloc-memcpy-free combination
         *          in order to force pointer change, e.g for shrink.
         */
{   void *newMem = realloc(mem,size);       /* normal */
    if( newMem == mem && newMem != NULL )   /* pointer did not change */
    {   newMem = malloc(size);              /* force change */
        if( newMem != NULL )
        {   memcpy(newMem, mem, size);      /* note: mem != NULL */
            free(mem);
        }
    }
    return tst_alloc(newMem, "tst_realloc", file, line, size);
}
#endif  /* REALLOC_TESTING */

}   /* end tst_realloc() */


/* qsort compare functions
 */
extern int qsortCompareUint32(const void *elem1, const void *elem2)
{
    Uint32 *u1 = (Uint32 *) elem1,
           *u2 = (Uint32 *) elem2;

    return (*u1) - (*u2);
}


/* print Uint64,
 * format:
 * if fixedHex16 then: hex (#%08lX%08lX)
 * else if any of 32 most significant bits == 1
 *               then: hex   (#%lX%08lX)
 * else if defaultFormat defined
 *               then: use defaultFormat
 *               else: decimal (%lu)
 */
extern void printUint64(Uint8 vLevel, Uint64 u64,
                        bool  fixedHex16,
                        char *defaultFormat)
{
    Uint32 low32, high32;
    ifVERBOSE(vLevel)
    {   low32  = (Uint32) (u64 & 0xFFFFFFFF);
        high32 = (Uint32) (u64 >> 32);
        if( fixedHex16 )
             fprintf(uctout, "#%08lX%08lX", high32, low32);
        else if( high32 != 0 )
             fprintf(uctout, "#%lX%08lX", high32, low32);
        else if( defaultFormat != NULL )
             fprintf(uctout, defaultFormat, low32);
        else fprintf(uctout,         "%lu", low32);
    }
    ENDif;
}

/* Recursion control *************************
 */

/* checkRecursion:
 * calling example for recursive function xxFunction:
 *
 *  #define xx_RECURSION_LIMIT 100
 *
 *  static bool xxFunction(..., recursionCount)
 *  { STATIC_RECURSIONADMIN;    // declaration
 *    ...
 *    if( !checkRecursion(&recursionAdmin, unique32,
 *                         recursionCount, xx_RECURSION_LIMIT,
 *                         "xxFunction") )
 *    {  return FALSE;  // infinite resursion loop detected
 *                      // or recursion limit reached
 *                      // or memory allocation error
 *    }
 *    ...
 *    if( ... )         // recursive call
 *    { result = xxFunction(..., recursionCount + 1);
 *    }
 *    ...
 *  }
 *  The first time that xxFunction() is called from outside the
 *  recursion loop, the recursionCount argument SHALL BE ZERO.
 *  The unique32 argument of checkRecursion(), shall be a Uint32
 *  value that is unique for a specific instance of xxFunction().
 *  If the same value is encountered in a subsequent instance of
 *  xxFunction() in the same recursion, an infinite recursion loop
 *  is assumed and the recursion loop is aborted by returning FALSE.
 *  If such a unique value cannot be defined, the value
 *  of recursionCount shall be used for unique32, in which case no
 *  infinite recursion loop will be detected and checkRecursion()
 *  will not allocate memory and only return FALSE in case the
 *  recursionCount exceeds xx_RECURSION_LIMIT.
 *
 *  checkRecursion return result:
 *  if      infinite resursion loop detected
 *       or recursion limit reached
 *       or memory allocation error
 *  then FALSE
 *  else TRUE
 */
extern bool checkRecursion(RecursionAdmin *recAdmin, Uint32 unique32,
                           Uint32 recursionCount, Uint32 recursionLimit,
                           char *functionName)
{   Uint32 n, nmbElem, *p32;

#ifdef  DEBUG02
    ifPRINTdebug02(uctout, "checkRecursion: %4lu %6lu %5lu %4lu\t%s\n",
                    recursionCount, unique32, recursionLimit,
                    recAdmin->allocElem, functionName);
    ENDif;
#endif  /* DEBUG02 */

    UCTASSERT(
           (recAdmin->unique32Array == NULL && recAdmin->allocElem == 0)
        || (recAdmin->unique32Array != NULL && recAdmin->allocElem != 0) );

    /* check infinite recursion loop, mind that recursionCount == 0
     * for first call of a recursion instance.
     */
    for( n = 0, p32 = recAdmin->unique32Array;
         n < recursionCount && p32 != NULL;
         n++, p32++ )
    {
        if( (*p32) == unique32 )
        {
            break;
        }
    }
    if(    (p32 != NULL && n < recursionCount)
        || (p32 == NULL && unique32 < recursionCount) )
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
             "\tError: Infinite recursion loop detected for %s().\n"
            "-\t       Unique recursion id encountered before in this recursion.\n"
            "-\t       Recursion id, count and limit: %lu %lu %lu, loop length: %lu.\n"
            "-\t       In most cases the recursion id is an absolute block address.\n",
                functionName, unique32, recursionCount, recursionLimit,
                recursionCount - ((p32 == NULL) ? unique32 : n) );
        MLIMITend;

#ifdef  DEBUG02
        ifVERBOSE(DEBUG02level)
        {
            fprintf(uctout, "recursionAdmin:");
            for( n = 0, p32 = recAdmin->unique32Array;
                 n < recursionCount && p32 != NULL;
                 n++, p32++ )
            {
                fprintf(uctout, " %lu", (*p32));
            }
            fprintf(uctout, "\n");
        }
        ENDif;
#endif  /* end DEBUG02 */

        checkFree((void**)&recAdmin->unique32Array);     /* NULL */
        recAdmin->allocElem = 0;
        return FALSE;
    }


    /* check recursion limit
     */
    if( recursionCount >= recursionLimit )  /* NOT ">" !!*/
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
             "\tError: Recursion limit %lu reached for %s().\n"
            "-\t       Abort recursion, please report.\n",
                recursionLimit, functionName);
        MLIMITend;
        checkFree((void**)&recAdmin->unique32Array);     /* NULL */
        recAdmin->allocElem = 0;
        return FALSE;
    }

    /* check allocation and add new unique32 value
     * to recAdmin if needed.
     */
    if( unique32 == recursionCount )    /* allocation may not be needed */
    {
        if( recAdmin->unique32Array == NULL )   /* no allocation sofar */
        {       /* so also previous unique32 and recursionCount equal */
            return TRUE;
        }
        if( recursionCount == 0 )       /* first time, free */
        {
            checkFree((void**)&recAdmin->unique32Array);    /* NULL */
            recAdmin->allocElem = 0;
            return TRUE;
        }
    }

    /* unique32 != recursionCount, so we need at least
     * (recursionCount + 1) allocated elements.
     */
    if(      recAdmin->allocElem <= recursionCount )    /* not enough */
    {   nmbElem = recursionCount + 10;   /* alloacte 9 extra elements */
    }
    else if( recAdmin->allocElem > (recursionCount + 100) ) /* too many */
    {   nmbElem = recursionCount + 100;                      /* shrink */
    }
    else    /* enough and not too many, no reallocation needed */
    {       /* add new value and done */
        recAdmin->unique32Array[recursionCount] = unique32;
        return TRUE;
    }

    /* (re)allocate
     */
    if( (p32 = (Uint32*) tst_realloc(recAdmin->unique32Array,
                                     nmbElem * sizeof(Uint32),
                                     __FILE__,__LINE__)) == NULL )
    {
        checkFree((void**)&recAdmin->unique32Array);     /* NULL */
        recAdmin->allocElem = 0;
        uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    recAdmin->unique32Array = p32;  /* (re)allocated array */

    if( recAdmin->allocElem == 0 )     /* nothing was allocated before */
    {   /* all previous unique32 (if any) were equal to recursionCount */
        for( n = 0, p32 = recAdmin->unique32Array;
             n < recursionCount;
             n++, p32++ )
        {   (*p32) = n;         /* fill in previous unique32 values */
        }
    }
    recAdmin->allocElem = nmbElem;  /* current nmb of allocated elements */
    recAdmin->unique32Array[recursionCount] = unique32; /* add new value */
    return TRUE;

}   /* end checkRecursion() */

/* nBytesChar():
 * Determine 'nmb of bytes' character.
 * To be used together with nBytesDouble(),
 * e.g. in print statements.
 * See example in nBytesDouble() explanation below.
 * (char*) return value:
 *      "T", "G", "K", "" if nBytes >= (1 Terabyte,Gigabyte,
 *      Megabyte,Kilobyte,'else') respectively.
 */
extern char *nBytesChar(Uint64 nBytes)
{ return (nBytes >= (Uint64)TbBYTES) ? "T" :    /* Tera */
         (nBytes >= (Uint64)GbBYTES) ? "G" :    /* Giga */
         (nBytes >= (Uint64)MbBYTES) ? "M" :    /* Mega */
         (nBytes >= (Uint64)KbBYTES) ? "K"      /* Kilo */
                                     : "";      /* bytes */
}   /* end nBytesChar() */

/* nBytesDouble():
 * Convert an Uint64 number of bytes to a (double)
 * number of Tbytes/Gbytes/Mbytes/Kbytes/bytes value.
 * To be used together with nBytesChar(),
 * e.g. in print statements.
 * Example:
 * printf( "Volume Space size: %.4f %sbytes",
 *          nBytesDouble(nBytes),
 *          nBytesChar(nBytes));
 */
extern double nBytesDouble(Uint64 nmbBytes)
{   Uint32 roundDownInt;
    Uint64 factor, rest64;

    switch( *(nBytesChar(nmbBytes)) )
    { case 'T': factor = TbBYTES;
                break;
      case 'G': factor = (Uint64)GbBYTES;
                break;
      case 'M': factor = (Uint64)MbBYTES;
                break;
      case 'K': factor = (Uint64)KbBYTES;
                break;
      default:  factor = (Uint64)1;
                break;
    }
    /* condition: factor is such that
     * (int) (nmbBytes/factor) fits in a Uint32.
     */
    roundDownInt = (Uint32) (nmbBytes / factor);
    rest64 = nmbBytes - ((Uint64)roundDownInt * factor);

    return (double)roundDownInt + ((double)rest64/(double)factor);

}   /* end nBytesDouble() */

/* nBytesDoublePrint4f():
 * Print numer of bytes using nBytesDouble()
 * and nBytesDouble().
 * e.g. for: nmbBytes == (1196672 * 2048) bytes,
 *    print: "2.2825 Gbytes"
 * (see the nBytesDouble() example).
 *
 * bug alert: Do not forget to cast the nmbBytes
 *            argument to (Uint64) if needed !!
 */
extern void nBytesDoublePrint4f( Uint64 nmbBytes,
                               char *extraTxt )
{
    double nbD = nBytesDouble(nmbBytes);

    fprintf( uctout, "%.4f %sbyte%s%s", nbD,
             nBytesChar(nmbBytes),
             PLURAL_S((int)nbD),
            (extraTxt != NULL) ? extraTxt : "" );

}   /* end nBytesDoublePrint4f() */

/* absDiffUint32():
 * Calculates abs difference between two Uint32 arguments,
 * delivering a Uint32 result as well.
 * Note that abs(x1-x2) fails for this purpose
 * because it has an int argument and result.
 */
extern Uint32 absDiffUint32(Uint32 x1, Uint32 x2)
{
    return (x1 >= x2) ? (Uint32)(x1-x2)
                      : (Uint32)(x2-x1);
}

