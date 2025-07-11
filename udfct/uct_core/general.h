/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : general.h
 *
 * Description : Non UDF or uct specific general definitions and functions.
 *
 * Author(s)   : Gerrit Scholl
 */

#ifndef __UCT_GENERAL_H__
#define __UCT_GENERAL_H__

#include <stdlib.h>

#include "mytypes.h"


#undef  DEBUG01     /* normally #undef */
#undef  DEBUG02     /* normally #undef */

/* No platform dependent definitions here
 *
 * General definitions
 */

#define MAX_UINT64  ((Uint64) 0xFFFFFFFFFFFFFFFF)
#define MAX_INT64    ((Int64) 0x7FFFFFFFFFFFFFFF)

#define MAX_UINT32  ((Uint32) 0xFFFFFFFF)
#define MAX_INT32    ((Int32) 0x7FFFFFFF)

#define MAX_UINT16  ((Uint16) 0xFFFF)
#define MAX_INT16    ((Int16) 0x7FFF)

#define KbBYTES         (1024)
#define MbBYTES         (1024*1024)
#define GbBYTES         (1024*1024*1024)
#define TbBYTES ((Uint64)1024*1024*1024*1024)

#ifndef MIN
#define MIN(x,y) ((x) <= (y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y) ((x) >= (y) ? (x) : (y))
#endif

#define isBitOn(VAL,BITS)   (((((Uint32)(VAL)) >> (BITS)) & (Uint32)0x0001) != 0)
#define isBitOff(VAL,BITS)  (((((Uint32)(VAL)) >> (BITS)) & (Uint32)0x0001) == 0)

#define setBitOn(VAL,BITS)  ((VAL) |=   (1 << (BITS)))
#define setBitOff(VAL,BITS) ((VAL) &= (~(1 << (BITS))))

#define ROUNDUPELEMENTS(YY,YM)  (((YY)+(YM)-1)/(YM))

#define ROUNDUPMULT(XX,XM)   ((XM)*ROUNDUPELEMENTS((XX),(XM)))
#define ROUNDDOWNMULT(XX,XM) ((XX) - ((XX) % (XM)))

#define PLURAL_S(NN)        (((NN)==1) ?    "" : "s")
#define PLURAL_IES(NN)      (((NN)==1) ?   "y" : "ies")
#define PLURAL_ARE(NN)      (((NN)==1) ?  "is" : "are")
#define PLURAL_WERE(NN)     (((NN)==1) ? "was" : "were")
#define PLURAL_HAVE(NN)     (((NN)==1) ? "has" : "have")
#define PLURAL_A(NN)        (((NN)==1) ?  "a " : "")

/* 0th, 1st, 2nd, 3rd, 4th, 5th, etc.
 */
#define XTH2(NN)    ((   (NN)>= 4               \
                       &&(NN)<=20)    ? "th" :  \
                     ((  (NN)%10)==1) ? "st" :  \
                     ((  (NN)%10)==2) ? "nd" :  \
                     ((  (NN)%10)==3) ? "rd" : "th")


/* NEWSTRUCT(XXstruct,XXnmb): General memory allocation macro.
 * Allocates an array of XXnmb structures of type XXstruct,
 * using tst_calloc(). This means that all fields in these
 * structures are initialized with 0, NULL, etc.
 */
#define NEWSTRUCT(XXstruct,XXnmb)   \
        ((XXstruct*) tst_calloc( XXnmb, sizeof(XXstruct), \
                                __FILE__,__LINE__ ))

/* All macros that begin with an if statement shall begin
 * with BEGINif and explicitly end with ENDif, in order
 * to avoid interfering with surrounding if statements,
 * see example below.
 */
#define BEGINif {
#define ENDif   }
/*
 * Error example:
 *      #define ifPRINT if (a>b) fprintf
 *   usage:
 *      if(c) ifPRINT(uctout, "format ... %...", arguments);
 *      else ...
 *   The else will be executed for !(a>b), instead of for !c.
 *
 * Avoid this by:
 *      #define ifPRINT BEGINif if (a>b) fprintf
 *   usage:
 *      if(c)
 *      {   ifPRINT(uctout, ...);
 *          ENDif;
 *      }
 *      else ...
 *   The else will be executed for !c as (hopefully) intended.
 *   The solution looks unnecessary complicated, because the
 *   brackets only is sufficient, but in the solution above,
 *   the programmer is now forced to use ENDif.
 *   Not using it will result in compiler errors.
 */

/* Global Int8 uctVerboseLevel for printing.
 * It holds the <level> value of the
 * "-verbose <option_level>" command line option.
 *
 * Each message has its own <message_verbose_level>.
 * A messages is printed if <option_level> is greater
 * than or equal to the <message_verbose_level>, so a
 * higher <message_verbose_level> will normally
 * produce more verifier output.
 * VERBOSE00level messages cannot be disabled.
 */
extern Int8 uctVerboseLevel;

#define  VERBOSE00level       0         /* errors   */
#define    ERROR00level VERBOSE00level  /* errors   */
#define     WARN01level      10         /* warnings */
#define     INFO01level      20         /* informational */
#define     INFO02level      30         /* more informational */
#define     FAKE01level      60         /* fake read informational */
#define VERBOSERESlevel      61         /* reserved for -verbose option */
#define VERBOSEMAXlevel     100         /*      max for -verbose option */

#ifdef  DEBUG01
#define    DEBUG01level     120     /* debug, outside -verbose option levels */
#endif  /* DEBUG01 */

#ifdef  DEBUG02
#define    DEBUG02level     121     /* debug level 2 */
#endif  /* DEBUG02 */

#define VERBOSEdefault  VERBOSEMAXlevel

/* Macros used for conditional statements, depending
 * Explicitly close with ENDif.
 */
#define ifVERBOSE(VV)    BEGINif if(uctVerboseLevel>=(VV))
#define ifNOTVERBOSE(VV) BEGINif if(uctVerboseLevel<(VV))

/* Macros used for printing, depending on uctVerboseLevel.
 * All macros have the same arguments as fprintf.
 * All macros starting with "if" shall be explicitly
 * closed with ENDif.
 */
#define    VERBOSE00    fprintf         /* unconditional */
#define ifPRINTwarn01   ifVERBOSE(WARN01level) fprintf
#define ifPRINTinfo01   ifVERBOSE(INFO01level) fprintf
#define ifPRINTinfo02   ifVERBOSE(INFO02level) fprintf
#define ifPRINTfake01   ifVERBOSE(FAKE01level) fprintf

#ifdef  DEBUG01
#define ifPRINTdebug01  ifVERBOSE(DEBUG01level) fprintf
#endif  /* DEBUG01 */

#ifdef  DEBUG02
#define ifPRINTdebug02  ifVERBOSE(DEBUG02level) fprintf
#endif  /* DEBUG02 */


/* Message limit macros MLIMITbegin(VL,MX) and MLIMITend.
 * Set of macros, used to print verbose messages, but limit
 * the number of times those messages are printed.
 * The statements between MLIMITbegin(VL,MX) and MLIMITend
 * are called the MLIMIT body and are assumed to contain a.o.
 * at least one message print statement.
 *
 * Execution rules:
 * The MLIMIT body will NOT be executed if verbose level VL
 *       does NOT apply,
 * else: Execution of the MLIMIT body will be limited to MX times.
 *
 * At the end of the last body execution, an attention message
 * on verbose level VL is printed denoting the number of times
 * the messages in the MLIMIT body have been printed and the
 * fact that they will be ignored from now.
 *
 * Important note:
 *  When entering MLIMITbegin(VL,MX), MX (and preferable also VL)
 *  should have the same value as when entering MLIMITbegin(VL,MX)
 *  for the first time.
 *
 * The macros use the following local variables for operation:
 *  static Int32 MLIMIT.cnt - exececution count, start with 1
 *         Int32 MLIMIT.max - copy of (MX)
 *         Uint8 MLIMIT.vl  - copy of (VL)
 * These variables are defined in the macros itself and may be
 * used in the MLIMIT body statements for read-only purposes.
 *
 * Example:
 *  MLIMITbegin(WARN01level, MLIMITdefault10);
 *    fprintf(uctout, "Warning: text....%lu...\n", arguments);
 *    print..(...);
 *    globalcount++;
 *    etc.
 *  MLIMITend;
 */

/*  MLIMIT constants:
 * (it is not recommended to use fixed limits with a
 *  value >= MLIMITdefault).
 */
#define MLIMITinfinite  ((Int32) 1000000)   /* message limit infinite */
                                            /* must fit in Int32 */
#define MLIMITdefault01     ((Int32)   1)
#define MLIMITdefault02     ((Int32)   2)
#define MLIMITdefault05     ((Int32)   5)
#define MLIMITdefault10     ((Int32)  10)
#define MLIMITdefault20     ((Int32)  20)
//#define   MLIMITdefault50     ((Int32)  50)
//#define   MLIMITdefault100    ((Int32) 100)

#define MLIMITdefault   MLIMITdefault20     /* -mlimit default */

/* Global Int32 uctMessageLimit.
 * It holds the <limit> value of the
 * "-mlimit <limit>" command line option.
 */
extern Int32 uctMessageLimit;   /* infinite if zero or >= MLIMITinfinite */

/* Global count of errors and warnings for verbose
 * levels VERBOSE00level and WARN01level respectively.
 * Set by the MLIMITbegin macro.
 * The ...Count is incremented when a MLIMITbegin macro
 * is entered for the first time. The ...Occurrences
 * is incremented for each time.
 */
extern Uint32 uctGlobalErrorCount;
extern Uint32 uctGlobalErrorOccurrences;
extern Uint32 uctGlobalWarningCount;
extern Uint32 uctGlobalWarningOccurrences;

/* static MLIMITstruct saves context for each separate
 * message (== MLIMITbegin/MLIMITend clause).
 */
typedef struct
{   Int32 cnt;     /* local messsage limit count */
    Uint8 vl;                   /* verbose level */
    Uint8 err;                  /* error flag    */
    Uint8 wrn;                  /* warning flag  */
    Uint8 endExecPrintmessage;  /* for MLIMITendFunction() */
} MLIMITstruct;

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
 * This count is independent of the value of MLIMIT.cnt, also because
 * MLIMIT.cnt can be reset by a value zero for messageLimit.
 */
extern bool MLIMITbeginFunction( MLIMITstruct *mls,
                                 Uint8 verboseLevel,
                                 Int32 messageLimit );

/* MLIMITendFunction():
 * Executed as last of the MLIMIT body.
 * Note that this function is only executed directly
 * after the MLIMIT body.
 */
extern void MLIMITendFunction( MLIMITstruct *mls );

/* VL - verbose level, key for body execution and
 *      global error/warning counting.
 * MX - local message limit, secondary key for body execution.
 *      Body is executed if message limit not exhausted.
 *      Message limit can be 'infinite' or 'all infinite'.
 *
 * See MLIMITbeginFunction() and MLIMITendFunction()
 * more for details.
 * Note that MLIMITendFunction() is only executed if the
 * body is executed.
 */
#define MLIMITbegin(VL,MX)                  \
{   static MLIMITstruct MLIMIT = {0,0,0,0,0};   \
    if( MLIMITbeginFunction(&MLIMIT, (VL), (MX)) ) \
    {   /* MLIMITstruct.localCnt == 1 for first execution
         * and after a MLIMITreset.
         */

    /* <MLIMIT body here> */

#define MLIMITend   \
        MLIMITendFunction( &MLIMIT ); \
    }               \
}

/******* end MLIMITbegin/MLIMITend macros, etc.
 *******/

/* Global file body sizes and CRC
 */
extern Uint64 uctFileBodySize;
extern Uint16 uctFileBodyCrc;
extern Uint64 uctFileBodyCrcSize;

/* Global bool uctDoFileCRC, default FALSE.
 * Set to TRUE if -filecrc option defined.
 */
extern bool uctDoFileCRC;

/* Global bool uctDoFakeRead, default TRUE.
 * Set to FALSE if -nofakeread option defined.
 * uctDoFakeRead is ignored if uctDoFileCRC == TRUE
 */
extern bool uctDoFakeRead;

/* Global bool uctDoUTCtime, default TRUE.
 * Set to FALSE if -localtime option defined.
 * Used to print local time instead of UTC with dir listings.
 */
extern bool uctDoUTCtime;

/* Global bool uctDoShowAlloc, default FALSE.
 * Set to TRUE if -showalloc option defined.
 * Used to print lists of allocated and unallocated
 * contiguous areas for each partition.
 */
extern bool uctDoShowAlloc;

/* Global bool uctDoShowPerm, default TRUE.
 * Set to FALSE if -noperm option defined.
 * Shows (E)FE permissions in dir listing.
 */
extern bool uctDoShowPerm;

/* Global bool uctIgnoreAVDP256, default FALSE.
 * Set to TRUE if -ignoreAVDP256 option defined.
 * E.g. used to follow 'old' AVDP at 512
 */
extern bool uctIgnoreAVDP256;

/* Global bool uctUseReadCache, default TRUE.
 * Not recommended for media with linkblocks in
 * volume space like CD-R.
 * Set to FALSE if -nocache option defined.
 */
extern bool uctUseReadCache;

/* Global bool uctDoReadGap, default FALSE.
 * If TRUE, try to read in unrecorded gaps.
 * Set to TRUE if -readgap option defined.
 */
extern bool uctDoReadGap;

/* Global bool uctUseMetadataMirror, default FALSE.
 * Set to TRUE if -usemirror option defined.
 */
extern bool uctUseMetadataMirror;


/* Commonly needed functions
 */

/* return TRUE if value u is found in array,
 *   else FALSE
 */
extern bool inArrayUint32(Uint32 u, Uint32 *array, int len);


/* Memory allocation stuff **********************************
 *
 * checkFree(): If *buff != NULL, then free(*buff)
 * and set *buff = NULL.
 */
extern void checkFree( void **buff );

/* initForAlloc(): Initialize before use of tst_*alloc()
 * functions. Claim some spare memory for release
 * at 'out of memory' problems, see tst_alloc.
 */
extern void initForAlloc();

/* tst_malloc():
 * Call malloc(), message if error result.
 */
extern void *tst_malloc(size_t size, char *file, int line);

/* tst_calloc():
 * Call calloc(), message if error result.
 */
extern void *tst_calloc(size_t num, size_t size, char *file, int line);

/* tst_realloc():
 * Call realloc() or malloc(), message if error result.
 */
extern void *tst_realloc(void *mem, size_t size, char *file, int line);

/* qsort definitions
 */
#define QSORT_ELEM1_FIRST   (-1)
#define QSORT_ELEM1_LAST    (+1)
#define QSORT_EQUAL          (0)

/* qsort compare functions
 */
extern int qsortCompareUint32(const void *elem1, const void *elem2);

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
                        char *defaultFormat);

/* Recursion control *************************
 */
typedef struct
{
    Uint32 *unique32Array;  /* Uint32 unique32Array[allocElem] */
    Uint32  allocElem;      /* nmb of allocated elements */
} RecursionAdmin;

#define STATIC_RECURSIONADMIN   \
    static RecursionAdmin recursionAdmin = { NULL, 0 }

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
                           char *functionName);

/* nBytesChar():
 * Determine 'nmb of bytes' character.
 * To be used together with nBytesDouble(),
 * e.g. in print statements.
 * See example in nBytesDouble() explanation below.
 * (char*) return value:
 *      "T", "G", "K", "" if nBytes >= (1 Terabyte,Gigabyte,
 *      Megabyte,Kilobyte,'else') respectively.
 */
extern char *nBytesChar(Uint64 nBytes);

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
extern double nBytesDouble(Uint64 nmbBytes);

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
                               char *extraTxt );

/* absDiffUint32():
 * Calculates abs difference between two Uint32 arguments,
 * delivering a Uint32 result as well.
 * Note that abs(x1-x2) fails for this purpose
 * because it has an int argument and result.
 */
extern Uint32 absDiffUint32(Uint32 x1, Uint32 x2);

#endif /* __UCT_GENERAL_H__ */

