/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctfarapart.c
 *
 * Description : Far apart testing functions
 *
 * Author(s)   : Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "general.h"
#include "ucterror.h"
#include "uctgeneral.h"
#include "uctfiles.h"
#include "uctstatus.h"
#include "uctfarapart.h"


/* includeLogicalInAbsList():
 * Include logical extent, translate to abs,
 * maybe split into more abs extents.
 */
static bool includeLogicalInAbsList(
                        UdfMountContext         *mc,
                        AnyAllocationDescriptor *aad,
                        Uint8                    adType,
                        Uint16                   shortPref,
                        UdfAllocationList      **pAbsList )
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint16 pref;
    Uint32 lbn, totalBlocks = 0;
    bool   result = TRUE;

    if( (*pAbsList) == NULL )       /* initialise */
    { (*pAbsList) = createNewAllocationList(ADT_EXTENT);    /* abs address */
      if( (*pAbsList) == NULL )
      { return FALSE;           /* error, no abs list */
      }
    }

    if( !udfGetLocation(aad, adType, shortPref,
                       &pref, &lbn) )
    { result = FALSE;
    }
    else
    { totalBlocks = ROUNDUPELEMENTS(adGetExtentSize(&aad->anyAd),
                                    vmi->blockSize);
    }

    /* Take care of possible virtual or spared blocks that may
     * split logical extent into multiple (*pAbsList) extents.
     */
    while( result != FALSE && totalBlocks > 0 )
    { ExtentAd  absAd;
      Uint32    nmbBlocks;

      if( !translateAddress( mc, pref, lbn,
                            &absAd.extentLocation,
                            &nmbBlocks, FALSE ) )   /* NOT silent */
      { result = FALSE;
        break;              /* done */
      }
      nmbBlocks = MIN(totalBlocks, nmbBlocks);        /* extent may */
      absAd.extentLength = vmi->blockSize * nmbBlocks;  /* be split */

#undef  TESTING_FAR_APART   /** normally #undef **/
#ifdef  TESTING_FAR_APART
  fprintf(uctout, "TESTING_FAR_APART ABS 03:\t\t%lu %lu\n",
            absAd.extentLocation,
            absAd.extentLocation + nmbBlocks - 1);
#endif

      if( nmbBlocks == 0 )              /* avoid infinite loop */
      { MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
            "\n\tError: Translation error or beyond Volume Space for ");
          if( pref == (Uint16) -1 )     /* no translation */
               fprintf(uctout, "address %lu\n", lbn);
          else fprintf(uctout, "location (%lu,p%u)\n", lbn, pref);
        MLIMITend;
        return FALSE;
      }

      if( !alAddDescriptor(*pAbsList,
                (AnyAllocationDescriptor*) &absAd,
                            FALSE) )      /* NOT toOverheadList */
      { result = FALSE;
        break;              /* done */
      }
      totalBlocks -= nmbBlocks;
      lbn         += nmbBlocks;
    }       /* endwhile totalBlocks */

    if( result == FALSE )       /* error, free and NULL abs list */
    { allocationListFree(pAbsList); /* NULL */
    }
    return result;

}       /* end includeLogicalInAbsList() */


/* Far Apart faProcessLists() group of functions and definitions
 */
typedef struct
{   Uint32  eccDist;
    Uint32  address0;
    Uint32  address1;
} FarApartResultItem;

typedef struct
{   FarApartResultItem *array;
    Uint32              arrayLen;
    Uint32              arrayAlloc;     /* >= arrayLen */
} FarApartResult;

/* faAddResultItem():
 * add item to FarApartResult array
 * faResult != NULL
 * empty array if faResult->array == NULL
 */
static void faAddResultItem( FarApartResult *faResult,
                             Uint32 dist,
                             Uint32 a0, Uint32 a1 )
{   FarApartResultItem *newItem;

    UCTASSERT( faResult != NULL );

    if( faResult->array == NULL )           /* empty array */
    {   faResult->arrayAlloc = faResult->arrayLen = 0;
    }
    if( faResult->arrayLen == faResult->arrayAlloc ) /* need more items */
    {   faResult->arrayAlloc += 64;           /* allocate 64 more items */
        if( (faResult->array = (FarApartResultItem*)
                tst_realloc( faResult->array,
                             faResult->arrayAlloc * sizeof(FarApartResultItem),
                             __FILE__,__LINE__)) == NULL )
        {   uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
        }
    }
    /* Mind possible faResult->array reallocation !!
     */
    newItem = &faResult->array[faResult->arrayLen];
    newItem->address0 = a0;
    newItem->address1 = a1;
    newItem->eccDist  = dist;
    faResult->arrayLen++;

}   /* end faAddResultItem() */

/* qsortFaResult():
 * Primary sort on 'real sector distance zero'
 *  (overlap errors, equal address0 and address1).
 * Secondary sort keys:
 *  First on item->eccDist (equivalent packet distance),
 *  then on whether address0 and adress1 are on the same layer (ML)
 *  further address0 and address1 values (not differences).
 *
 * Implementation notes:
 * - Note that if item->eccDist == 0, address0 and address1 need
 *   not be equal. They can be different sectors in the same packet
 *   or even in a packet on a different layer for ML media.
 * - Apart from presentation issues, these secondary sort
 *   keys are for a unique sort result for different qsort
 *   implementationson on different platforms.
 */
static int qsortFaResult( const void *elem1,
                          const void *elem2 )
{
    FarApartResultItem *it1 = (FarApartResultItem*) elem1,
                       *it2 = (FarApartResultItem*) elem2;
    bool it1overlap = (it1->address0 == it1->address1),
         it2overlap = (it2->address0 == it2->address1);
    bool it1SameLay = mlOnSameLayer(it1->address0, it1->address1),
         it2SameLay = mlOnSameLayer(it2->address0, it2->address1);

    /* Sort keys, see as explained above.
     */
    return
        ( it1overlap   && !it2overlap)    ? QSORT_ELEM1_FIRST
      : (!it1overlap   &&  it2overlap)    ? QSORT_ELEM1_LAST
      : ( it1->eccDist  <  it2->eccDist)  ? QSORT_ELEM1_FIRST
      : ( it1->eccDist  >  it2->eccDist)  ? QSORT_ELEM1_LAST
      : ( it1SameLay   && !it2SameLay)    ? QSORT_ELEM1_FIRST
      : (!it1SameLay   &&  it2SameLay)    ? QSORT_ELEM1_LAST
      : ( it1->address0 <  it2->address0) ? QSORT_ELEM1_FIRST
      : ( it1->address0 >  it2->address0) ? QSORT_ELEM1_LAST
      : ( it1->address1 <  it2->address1) ? QSORT_ELEM1_FIRST
      : ( it1->address1 >  it2->address1) ? QSORT_ELEM1_LAST
                                          : QSORT_EQUAL;
}   /* end qsortFaResult() */

/* faMarginRound():
 * Calc ECC distance margin as a percentage of eccBase.
 * Margin will be at least minSect sectors, but not higher
 * than double the percentage of eccBase.
 * Further some more heuristic round-ups.
 */
static Uint32 faMarginRound( Uint32 eccBase,
                             Uint32 percentage,
                             Uint32 minSect )
{   const MediumInfo *vmi = getTheMediumInfo();
    Uint32 eccMargin, n, pBase;

    pBase = percentage * eccBase;       /* 100 * ECC */
    eccMargin = ROUNDUPELEMENTS(pBase, 100);  /* ECC */

    /* at least minSect sectors, but not higher than
     * double the percentage.
     * Convert sectors to ECC packets.
     */
    minSect = ROUNDUPELEMENTS(minSect, vmi->eccLength); /* ECC */
    pBase = 2 * pBase;               /* double 100 * ECC */
    pBase  = ROUNDUPELEMENTS(pBase, 100);  /* double ECC */
    n       = MIN(minSect, pBase);
    eccMargin = MAX(eccMargin, n);

    /* Round bigger values up to multiple of
     * 'nice' power of ten (round less than 0.05 percent).
     */
    n = 1;                         /* n is 'nice' power of 10 */
    while( (n * 20000) < eccBase )  /* less than 0.05 percent */
    { n = n * 10;                   /* power of ten */
    }
    /*              (n * 20000) >= eccBase
     * && (n == 1 || (n * 2000) < eccBase) (less than 0.05 percent)
     */
    eccMargin = ROUNDUPMULT(eccMargin, n);  /* mult of n */

    return MIN(eccMargin, eccBase);   /* assert <= 100 % */

}   /* end faMarginRound() */

/* faCalculateMargins():
 */
static void faCalculateMargins( Uint32  ex0Sect,
                                Uint32  ex1Sect,
                                Uint32 *pEccWarn,
                                Uint32 *pEccNote,
                                Uint32 *pEccBase,
                                char    **pBaseTxt )
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32 baseEcc, warnEcc, noteEcc, exEcc;
    char  *baseTxt;

    /* Test margin for 'far apart' test is questionable because
     * this is not specifically described in the UDF spec.
     * It should be something relative to:
     * - the volume space
     * - for ML, the Layer zero capacity, if defined.
     * - for multisession, the session space.
     * which of these has the smallest size.
     * First size in sectors, later convert to ECC packets.
     */
    baseEcc = vmi->lastValidBlockNr + 1;    /* sectors */
    baseTxt = "Volume Space";
    if(   vmi->L0capacity != 0
       && vmi->L0capacity < baseEcc )
    { baseEcc = vmi->L0capacity;
      baseTxt = "L0 capacity";
    }
    if( vmi->numberOfSessions > 1 )
    { Uint32 start, endP1, sizeSession;
      start = vmi->sessionStartBlocks[vmi->verifySession-1];
      endP1 = (vmi->verifySession == vmi->numberOfSessions) /* if last session */
                    ? vmi->lastValidBlockNr + 1
                    : vmi->sessionStartBlocks[vmi->verifySession];
      sizeSession = endP1 - start;
      if(   endP1 > start       /* sizeSession > 0 */
         && sizeSession < baseEcc )
      { baseEcc = sizeSession;
        baseTxt = "session size";
      }
    }
    /* convert nmb of sectors to nmb of ECC packets
     * and subtract nmb of ECC blocks for each group of extents
     */
    baseEcc = ROUNDUPELEMENTS(baseEcc, vmi->eccLength);
    exEcc   = ROUNDUPELEMENTS(ex0Sect, vmi->eccLength)
            + ROUNDUPELEMENTS(ex1Sect, vmi->eccLength);
    if( exEcc < baseEcc )
    { baseEcc -= exEcc;     /* remaining space */
    }

    /* Test margins first approach (detailed later):
     *  for warning: level1 percentage of testVolumeSize
     *               level1 minimum ECC packets
     *     for note: level2 percentage of testVolumeSize
     *               level2 minimum ECC packets
     * Test margins expressed in nmb of ECC packets.
     * For rounding, etc. details, see faMarginRound().
     */
#define FA_LEVEL1_PERC        10    /* 'warning' percentage */
#define FA_LEVEL2_PERC        25    /*    'note' percentage */
#define FA_LEVEL1_MINSEC     128    /* 'warning' minimum sectors */
#define FA_LEVEL2_MINSEC    1024    /*    'note' minimum sectors */

    warnEcc = faMarginRound(baseEcc, FA_LEVEL1_PERC, FA_LEVEL1_MINSEC);
    noteEcc = faMarginRound(baseEcc, FA_LEVEL2_PERC, FA_LEVEL2_MINSEC);

    /* asserts: warnEcc <= noteEcc <= baseEcc
     */
    if( noteEcc > baseEcc ) noteEcc = baseEcc;
    if( warnEcc > noteEcc ) warnEcc = noteEcc;

    (*pEccWarn) = warnEcc;
    (*pEccNote) = noteEcc;
    (*pEccBase) = baseEcc;
    (*pBaseTxt) = baseTxt;

}       /* end faCalculateMargins() */

/* faPrintSummary():
 * Print far apart summary after result array has been sorted
 * according to qsortFaResult(), and inspected by faInspectResult().
 * Print relevant records of the result array.
 * Always print the first 20 records, if any, but maybe more
 * (see below for details).
 */
static void faPrintSummary( FarApartResult *faResult,
                            Uint32   min0,       Uint32 max0,
                            Uint32   min1,       Uint32 max1,
                            Uint32   nEx0,       Uint32 nEx1,
                            Uint32   eccBase,    char  *eccBaseTxt,
                            Uint32   eccLevel1,  Uint32 eccLevel2,
                            Uint32   cntLevel1,  Uint32 cntLevel2,
                            char    *txtHead,
                            char    *txt0,       char  *txt1 )
{
    const MediumInfo  *vmi = getTheMediumInfo();
    Uint32  n, mxRecords, pdPrev = 0,
               eccMin = faResult->array[0].eccDist;
    double  percentage;
    bool    slPrev = FALSE;

    /* Log 'far apart' info summary.
     * Start with some notes.
     * Note that L0capacity may be nonzero, while lastValidBlockNr
     * is still located on the first layer L0.
     */
    if( vmi->L0capacity != 0 )      /* multi layer medium */
    { Uint64 nb64 = vmi->L0capacity * vmi->blockSize;
      VERBOSE00( uctout,
        "   =>\tFar apart distance calculations are based on"
                                        " multi-layer %s medium.\n"
            "-\tLast valid block is located on layer L%lu.\n%s"
            "-\tML %s, L0 capacity: %lu sectors   %.3f %sbyte.\n",
                MI_OTP_TEXT(vmi->isOTP),
                mlGetLayerNumber(vmi->lastValidBlockNr),
                (vmi->isOTP)
                  ? "-\tFor OTP, layer capacity is assumed to be"
                                        " equal for all layers.\n"
                  : "-\tFor PTP, only Dual Layer is supported.\n",
                MI_OTP_TEXT(vmi->isOTP), vmi->L0capacity,
                nBytesDouble(nb64), nBytesChar(nb64) );
    }
    if( vmi->eccLength == 1 )
    { VERBOSE00(uctout,
        "   =>\tNote: For this medium, an ECC packet"
                        " is equal to a logical sector.\n");
    }
    /* The summary itself.
     */
    VERBOSE00(uctout,
      "\n   =>\t%s \'far apart\' test summary:\n"
               "-\t %13s LBA range: %10lu  thru %10lu, %3lu extent%s\n"
               "-\t %13s LBA range: %10lu  thru %10lu, %3lu extent%s\n"
      "-\t  lowest packet distance: %7lu ECC packet%1s ",
        txtHead, txt0, min0, max0, nEx0, PLURAL_S(nEx0),    /** abslist[0] */
                 txt1, min1, max1, nEx1, PLURAL_S(nEx1),    /** abslist[1] */
        eccMin, PLURAL_S(eccMin) );

    /* It is possible that one of the groups defines extents
     * outside the base space, in which case the 'lowest packet
     * distance' percentage can be more than 100 percent.
     */
    percentage = ((double)100*eccMin)/(double)eccBase;
    if( percentage > 100.0 )
         VERBOSE00(uctout, "(outside %s \?\?)\n", eccBaseTxt);
    else VERBOSE00(uctout, "(%5.1f%% of remaining %s)\n",
                                    percentage, eccBaseTxt);
    VERBOSE00(uctout,
      "-\t     test margin level 1: %7lu ECC packet%1s (%5.1f%% of remaining %s)\n"
      "-\t     test margin level 2: %7lu ECC packet%1s (%5.1f%% of remaining %s)\n",
        eccLevel1, PLURAL_S(eccLevel1),
                    ((double)100*eccLevel1)/(double)eccBase, eccBaseTxt,
        eccLevel2, PLURAL_S(eccLevel1),
                    ((double)100*eccLevel2)/(double)eccBase, eccBaseTxt );

    if( cntLevel1 != 0 )
    { VERBOSE00(uctout,
        "-\t  %5lu extent combination%s less than %lu ECC packet%s apart\n",
          cntLevel1, PLURAL_S(cntLevel1), eccLevel1, PLURAL_S(eccLevel1) );
    }
    if( cntLevel2 != cntLevel1 )
    { VERBOSE00(uctout,
        "-\t  %5lu extent combination%s less than %lu ECC packet%s apart\n",
          cntLevel2, PLURAL_S(cntLevel2), eccLevel2, PLURAL_S(eccLevel2) );
    }

    /* header for records:
     */
    VERBOSE00(uctout,
        "-\t  %13s    %11s    ECC packet distance\n",
               txt0, txt1);

    /* Now print relevant records.
     * Show ECC packet distance.
     * Flag with "ML" if ECC packet distance is found
     * because of Multi-Layer geometry.
     * Normally print first 20 records
     * (first 1000 if 'all infinite' message limit).
     * Maybe 2 times round up max 10 in order to print
     * all cntLevel1, cntLevel2 records respectively.
     */
    mxRecords = (uctMessageLimit == 0) ? 1000 : 20;
    if( (mxRecords + 10) >= cntLevel1)
    { mxRecords = MAX(mxRecords, cntLevel1);
    }
    if( (mxRecords + 10) >= cntLevel2)
    { mxRecords = MAX(mxRecords, cntLevel2);
    }
    mxRecords = MIN(mxRecords, faResult->arrayLen);

    for( n = 0; n < mxRecords; n++)         /* for the moment */
    { FarApartResultItem *faItem = &faResult->array[n];
      Uint32 pd  = faItem->eccDist,
             ad0 = faItem->address0,
             ad1 = faItem->address1;
      bool   isOverlap = (ad0 == ad1),  /* multiple allocation */
             isSameLay = mlOnSameLayer(ad0, ad1);

      if( vmi->L0capacity == 0 )    /* SL */
      { fprintf(uctout, "-\t    %10lu %2s  %10lu %2s  %8lu  %2s",
                                  ad0, "",   ad1, "",  pd,   "");
      }
      else                         /* ML (multi layer) */
      { fprintf(uctout, "-\t    %10lu L%lu  %10lu L%lu  %8lu  ML",
                            ad0, mlGetLayerNumber(ad0),
                            ad1, mlGetLayerNumber(ad1), pd);
      }
      fprintf(uctout,   "  %16s\n",
            (isOverlap)                 ?  "OVERLAP ERROR !"
            : (   pd >= eccLevel2
               || (   n != 0
                   && pd == pdPrev
                   && isSameLay == slPrev)) ?             ""
            : (pd == 0 && isSameLay)      ?   "same packet "
            : (pd == 0)                 ? "opposite packet "  /* ML */
            : (pd == 1)                 ? "adjacent packets"
                                        :   "nearby packets" );
      pdPrev = pd;
      slPrev = isSameLay;
    }   /* endfor n */

    if( mxRecords < cntLevel2 )
    { fprintf(uctout,
        "-\t\t ..... only %lu lowest ECC distances printed\n",
                    mxRecords );
    }
}       /* end faPrintSummary() */

/* faPrintMessage():
 * Print error/warning/Note message for one result record.
 * called from faProcessLists() from within a
 * MLIMITbegin/MLIMITend clause in order to have
 * independent error/warning counting for e.g. VDS
 * and Metadata 'far apart' tests.
 * Verbose level vLevel determines whether this is an error,
 * warning or note message.
 * There is a special case if the sector distance is zero
 * and both are on the same layer.
 * This means that the extents really overlap, which is always
 * an error. Such cases will be the first occurences in the
 * lists because of the qsortFaResult() sorting algorithm.
 */
static void faPrintMessage( Int8     vLevel,
                            char    *txt0,
                            char    *txt1,
                            char    *ref,
                            char    *dcn250,
                            bool     isVDS )
{
    Uint16 udfRev = getUctUdfRevision();
    char  *txt = (vLevel == ERROR00level) ? "Error:"
               : (vLevel == WARN01level)  ? "Warning:"
                                          : "Note:";

    VERBOSE00(uctout,
         "\n\t%8s %s and %s extents not far apart, see\n"
              "-\t\t summary above, %s%s.\n%s",
        txt, txt0, txt1, ref,
        (udfRev <= 0x250) ? dcn250 : "",
        (udfRev <= 0x250 && isVDS)
          ? "-\t\t Far apart rules for VDS are introduced in UDF 2.50.\n"
          : "" );

}       /* end faPrintMessage() */

/* faInspectResult():
 * Inspect far apart result and remove record with
 * redundant ECC packets.
 * Result is sorted according to qsortFaResult().
 * Count overlap records, 'in-same-ECC' records and records
 *  with ECC-distance-less-than limit1, limit2 respectively.
 */
static void faInspectResult( FarApartResult *pFaResult,
                             Uint32          faEccLimit1,
                             Uint32          faEccLimit2,
                             Uint32         *pCntOverlap,
                             Uint32         *pCntSameEcc,
                             Uint32         *pCntLimit1,
                             Uint32         *pCntLimit2 )
{
    Uint32 n, m;

#undef  FA_REMOVE_REDUNDANT     /* #undef for the moment */
#ifdef  FA_REMOVE_REDUNDANT
    Uint32 pn0Prev = 0, pn1Prev = 0, sdPrev = 0;
#endif

    /* result array sorted now.
     * Maybe remove redundant records
     * (non-overlap with identical packet numbers)
     */
    (*pCntOverlap) = (*pCntSameEcc) =
        (*pCntLimit1) = (*pCntLimit2) = 0;

    for( n = 0, m = 0;
         n < pFaResult->arrayLen;
         n++, m++)
    { FarApartResultItem *item = &pFaResult->array[n];
      Uint32 pn0 = getEccPacketNr(item->address0),
             pn1 = getEccPacketNr(item->address1);

#ifdef  FA_REMOVE_REDUNDANT
      Uint32 sd  = absDiffUint32(item->address0, item->address1);

      if(   n > 0                               /* not first record */
         && item->address0 != item->address1    /* no 'overlap'     */
         && pn0 == pn0Prev && pn1 == pn1Prev )      /* identical ECC packets */
      {                     /* skip this record */
        m--;                /* skip */
        if( sd < sdPrev )   /* keep the lowest sector distance */
        { pFaResult->array[m].address0 = item->address0;
          pFaResult->array[m].address1 = item->address1;
        }
        continue;
      }
      pn0Prev = pn0; pn1Prev = pn1;
      sdPrev = sd;
      if( m != n )               /* skipped records */
      { pFaResult->array[m] = (*item);  /* copy */
      }
#endif  /* FA_REMOVE_REDUNDANT */

      /* record not skipped, handle statistics.
       */
      if( item->address0 == item->address1) (*pCntOverlap)++;
      if( pn0 == pn1)                       (*pCntSameEcc)++;
      if( item->eccDist < faEccLimit1 )     (*pCntLimit1)++;
      if( item->eccDist < faEccLimit2 )     (*pCntLimit2)++;
    }
    pFaResult->arrayLen = m;     /* maybe reduced length */

}   /* end faInspectResult() */

/* faListIsEmpty():
 */
static bool faListIsEmpty(UdfAllocationList *absList)
{   return ( absList == NULL || absList->head == NULL );
}

/* faProcessLists():
 * Two variants at the moment:
 * if isVDS then VDS far apart test,
 *          else Metadata files far apart test.
 *
 * Implementation notes:
 *  - far apart is not well defined
 *  - often, rule is difficult to implement,
 *    e.g. sequential write, mult session, POW (remaps), etc.
 *  - For 'warning' and 'note' test margins,
 *    see faCalculateMargins();
 *  - ML layout is taken into account !!
 */
static bool faProcessLists( UdfAllocationList *absLists[2],
                            bool isVDS )
{
    const MediumInfo  *vmi = getTheMediumInfo();
    UdfAllocationItem *ai0, *ai1;
    FarApartResult faResult = { NULL, 0, 0 };   /* empty */
    Uint32   pd,  sd_dummy,
             ad0, nEx0, nEx0Sect, min0, max0,   /* ... abslist[0] */
             ad1, nEx1, nEx1Sect, min1, max1,   /* ... abslist[1] */
             cntOverlap, cntSameEcc, cntLevel1, cntLevel2;
    Uint32   eccLevel1, eccLevel2, eccBase;
    char    *eccBaseTxt, *txt,
            *txtHead,       /* "VDS" or "Metadata" */
            *txt0,          /* associated with absLists[0], etc[0] */
            *txt1,          /* associated with absLists[1], etc[1] */
            *ref,           /* 'far apart' spec reference */
            *dcn250;        /* UDF 2.50 errata DCN reference */

    /* define texts associated with VDS or Metdata far apart test
     */
    if( isVDS )     /* VDS */
    { txtHead = "Volume Descriptor Sequence";
      txt0    =    "Main VDS";      /* associated with absLists[0], etc[0] */
      txt1    = "Reserve VDS";      /* associated with absLists[1], etc[1] */
      ref     =   "UDF 2.2.3.2",
      dcn250  = ", UDF 2.50 errata DCN-5113";
    }
    else            /* Metadata files far apart test */
    { txtHead = "Metadata";
      txt0    = "Metadata File";    /* associated with absLists[0], etc[0] */
      txt1    =   "Mirror File";    /* associated with absLists[1], etc[1] */
      ref     =   "UDF 2.2.13.1",
      dcn250  = ", UDF 2.50 errata DCN-5106";
    }

    /* first test if any of absLists[] still empty
     */
    txt = (faListIsEmpty(absLists[0])) ? txt0
        : (faListIsEmpty(absLists[1])) ? txt1
                                       : NULL;
    if( txt != NULL )
    { MLIMITbegin(WARN01level, uctMessageLimit);
        fprintf(uctout,
          "\n   =>\tWarning: Far apart test cannot"
                                " be performed because\n"
          "-\t\t of empty allocations for %s.\n"
          "-\t\t Probably caused by previous error.\n\n", txt);
      MLIMITend;
      return FALSE; /* abort, no faResult.array clean-up needed */
    }

    /* Two abslists present.
     * Now find address range and nmb of extents for both lists.
     */
    min0 = min1 = MAX_UINT32;
    max0 = max1 = 0;
    nEx0 = nEx1 = 0;            /* nmb of extents */
    nEx0Sect = nEx1Sect = 0;    /* total extent sectors */

    for( ai0 = absLists[0]->head;
         ai0 != NULL;
         ai0 = ai0->next )
    { Uint32 nb0 = ROUNDUPELEMENTS( ai0->aad.extentAd.extentLength,
                                    vmi->blockSize ),
             str0 = ai0->aad.extentAd.extentLocation,
             end0 = str0 + nb0 - 1;

      UCTASSERT( nb0 != 0 );

      nEx0++;
      nEx0Sect += nb0;
      min0 = MIN(min0, str0);
      max0 = MAX(max0, end0);

      for( ai1 = absLists[1]->head;
           ai1 != NULL;
           ai1 = ai1->next )
      { Uint32 nb1 = ROUNDUPELEMENTS( ai1->aad.extentAd.extentLength,
                                      vmi->blockSize ),
               str1 = ai1->aad.extentAd.extentLocation,
               end1 = str1 + nb1 - 1;

        UCTASSERT( nb1 != 0 );

        if( ai0 == absLists[0]->head )      /* first ai0 loop */
        { nEx1++;
          nEx1Sect += nb1;
          min1 = MIN(min1, str1);
          max1 = MAX(max1, end1);
        }

        /* Calculate lowest physical sector distance sd_dummy and packet distance
         * pd between any sector in the absList[0] ai0 extent and any one
         * in the abslist[1] ai1 extent. The logical sector addresses for
         * which this occurs are calculated in ad0 and ad1 respectively.
         * Takes into account the fact that e.g. for a Multi-Layer medium,
         * these physical distances can be zero for quite different values
         * of ad0 and ad1, where ad0 and ad1 are on different layers on an
         * identical radius of a ML medium.
         */
        mlFaCalcDistance( &ai0->aad.extentAd, &ai1->aad.extentAd,
                          &ad0, &ad1,
                          &sd_dummy,  &pd );  /* sector and packet distance */
        faAddResultItem( &faResult, pd, ad0, ad1 );
      }     /* endfor ai1 */
    }     /* endfor ai0 */

    if( faResult.array == NULL )        /* empty */
    {       /* no extents found, maybe more than 2 layers */
      VERBOSE00(uctout,
        "\n   =>\t%s \'far apart\' test summary:\n"
              "-\t  Note: Far apart calculations could not be performed.\n\n",
               txtHead);
      return FALSE;     /* no far apart calculation results found */
    }
    qsort((void *)faResult.array, faResult.arrayLen,
           sizeof(FarApartResultItem), qsortFaResult);

    /* Result array sorted now.
     * Determine test margins for far apart test and the base
     * to which these margins are related, e.g. the Volume Space,
     * the L0 capacity or the session size.
     * There are two test margins, one for a warning and one
     * for a note (first approach, detailed later).
     * We also need a text denoting which base is used.
     * For further details, see faCalculateMargins().
     */
    faCalculateMargins(  nEx0Sect,   nEx1Sect,
                        &eccLevel1, &eccLevel2,
                        &eccBase,   &eccBaseTxt );
    /* Asserted: eccLevel1 <= eccLevel2 <= eccBase.
     * Inspect results.
     */
    faInspectResult( &faResult, eccLevel1, eccLevel2,
                     &cntOverlap, &cntSameEcc,
                     &cntLevel1,  &cntLevel2 );

    UCTASSERT(   faResult.arrayLen >= cntLevel2
              && cntLevel2  >= cntLevel1
              && cntLevel1  >= cntSameEcc
              && cntSameEcc >= cntOverlap );

    faPrintSummary( &faResult, min0, max0, min1, max1,
                     nEx0, nEx1, eccBase, eccBaseTxt,
                     eccLevel1, eccLevel2,
                     cntLevel1, cntLevel2,
                     txtHead, txt0, txt1 );

    /* Now print error/warning/note messages if needed.
     * first overlap errors (multiple allocation)
     * Do not point at specific far apart rules, because
     * also a far apart message will be printed.
     */
    if( cntOverlap > 0 )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
          "\n\t  Error: Overlap of %s and %s extents, multiple\n"
                 "-\t\t allocation, see OVERLAP ERROR"
                            " in summary above.\n", txt0, txt1);
      MLIMITend;
    }

    /* Overlap handled now, but overlap will generate
     * a 'far apart ' message as well.
     * Print some far apart message if cntLevel2 != 0
     * - error for VDS and UDF 2.50+ and 'in-the-same-packet'
     * - warning for UDF 2.50+ and cntLevel1 != 0
     * - else a note.
     * For VDS and UDF 2.01-, 'in-the-same-packet' is a warning,
     * else a note.
     */
    if( cntLevel2 != 0 )    /* print some far apart message */
    { Uint16 udfRev = getUctUdfRevision();
      Uint8 vLevel =
            (isVDS && udfRev >= 0x250 && cntSameEcc != 0)
                        ? ERROR00level
          : (cntLevel1) ? WARN01level
                        : INFO01level;
      if( isVDS && udfRev < 0x250 )     /* 'lower' one level */
      { vLevel = (vLevel == ERROR00level)
                        ? WARN01level
                        : INFO01level;
      }

      /* Separate MLIMITbegin/MLIMITend clause for Metadata
       * and VDS cases in order to have independent
       * error/waring counting.
       * (identical code, but different static MLIMIT context)
       * Print message using faPrintMessage().
       */
      if( isVDS )   /* Main/Reserve VDS result messages */
      { MLIMITbegin( vLevel, uctMessageLimit );
          faPrintMessage( vLevel, txt0, txt1, ref, dcn250, isVDS );
        MLIMITend;
      }
      else      /* Metadata far apart result messages */
      { MLIMITbegin( vLevel, uctMessageLimit );
          faPrintMessage( vLevel, txt0, txt1, ref, dcn250, isVDS );
        MLIMITend;
      }
    }
    VERBOSE00(uctout, "\n");

    /* clean-up
     */
    checkFree((void**) &faResult.array);    /* NULL */

    return TRUE;

}   /* end faProcessLists() */

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
                                   bool dupFlagSet )
{
    FileNodeInfo      *metaFiles[2];
    UdfAllocationList *tmpAbsLists[2] = { NULL, NULL };
    Uint32 n;
    bool   result = TRUE;

    /* Several arrays of size 2 are using index [0] and [1],
     * index [0] : Metadata File
     * index [1] : Metadata Mirror File
     */
    metaFiles[0] = mc->metadataFile;
    metaFiles[1] = mc->metadataMirrorFile;

    VERBOSE00(uctout,
      "\n  ==>\tChecking Metadata \'far apart\' allocation for %s%s%s.\n",
        tidTEXT4(((Tag*)metaFiles[0]->node->fe)->tagIdentifier),
        (   metaFiles[0]->node->al->overheadList != NULL
         || metaFiles[1]->node->al->overheadList != NULL)
                     ? " + AED" : "",
        (dupFlagSet) ? " + duplicated data" : "");

    /* include (E)FE, AED and maybe ADs for
     * both Metadata File and its mirror.
     * Abort as soon as result becomes FALSE.
     */
    for( n = 0;
         n < 2 && result != FALSE;
         n++ )
    { UdfAllocationList *al;
      LongAd *pl = &metaFiles[n]->icb;
      Uint16  fePref = pl->extentLocation.partitionReferenceNumber;
      int     loop;

#ifdef  TESTING_FAR_APART
  fprintf(uctout, "TESTING_FAR_APART MET 01: %lu\n", n);
#endif
      /* include (E)FE extent
       */
      if( !includeLogicalInAbsList( mc,
                (AnyAllocationDescriptor*) pl,
                     ADT_LONG, fePref, &tmpAbsLists[n]) )
      { result = FALSE;     /* error, clean-up later */
      }

      /* if result == TRUE, then check AD extents.
       * 1st loop : AED extents (in overHeadList)
       * 2nd loop : other AD extents (except sparse ones)
       *    Note: 2nd loop only if dupFlagSet !!!
       * Abort as soon as result becomes FALSE.
       */
      for( loop = 0;
           loop < ((dupFlagSet) ? 2 : 1) && result != FALSE;
           loop++ )
      { UdfAllocationItem *ai;
        al = (loop == 0)
              ? metaFiles[n]->node->al->overheadList
              : metaFiles[n]->node->al;
        for( ai = (al == NULL) ? NULL : al->head;
             ai != NULL && result != FALSE;
             ai = ai->next )
        { Uint8 adType = al->itemAdType;
          Uint8 exType = adGetExtentType(&ai->aad.anyAd);
          switch( exType )
          { case ADEL_EXTENTPOINTER:            /* overheadList */
              UCTASSERT( loop == 0 );           /* not here please */
              break;
            case ADEL_RECORDED_AND_ALLOCATED:
            case ADEL_NOT_RECORDED_BUT_ALLOCATED: /* for completeness */
              UCTASSERT( loop != 0 );             /* not here please */
              break;
            case ADEL_NOT_RECORDED_NOT_ALLOCATED: /* sparse extent */
              UCTASSERT( loop != 0 );            /* not here please */
              continue;                         /* skip sparse extent */
              /** break; **/
          }
#ifdef  TESTING_FAR_APART
  fprintf(uctout, "TESTING_FAR_APART MET 02: %lu %lu %lu\n", n, loop, (Uint32) ai);
#endif
          if( !includeLogicalInAbsList(mc, &ai->aad, adType,
                                       fePref, &tmpAbsLists[n]) )
          { result = FALSE;     /* error, clean-up later */
          }
        }       /* endfor ai */
      }       /* endfor loop */
    }       /* endfor metadataFile, metadataMirrorFile */

    /* Final processing and message printing if tmpAbslists[]
     * was successfully created.
     */
    if(    result != FALSE
       && !faProcessLists(tmpAbsLists, FALSE) ) /* NOT isVDS (Metadata) */
    { result = FALSE;       /* error, clean-up later */
    }

    /* clean-up (also for all error cases).
     */
    allocationListFree(&tmpAbsLists[0]);    /* NULL */
    allocationListFree(&tmpAbsLists[1]);    /* NULL */
    return result;

}   /* end checkMetadataFarApart() */

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
extern bool checkVDSFarApart( UdfMountContext *mc )
{
    UdfAllocationList  *tmpAbsLists[2] = { NULL, NULL };
    ContExtentItem     *pContItem;
    bool                result = TRUE;

#ifdef  TESTING_FAR_APART
  fprintf(uctout, "TESTING_FAR_APART VDS 01: 0,1\n");
#endif
    /* arrays of size 2 are using index [0] and [1],
     * index [0] : Main VDS
     * index [1] : Reserve VDS
     */
    if(   !includeLogicalInAbsList( mc,
             (AnyAllocationDescriptor*)
                &mc->avdp->mainVolumeDescriptorSequenceExtent,
                 ADT_EXTENT, (Uint16) -1, &tmpAbsLists[0] )
       || !includeLogicalInAbsList( mc,
             (AnyAllocationDescriptor*)
                &mc->avdp->reserveVolumeDescriptorSequenceExtent,
                 ADT_EXTENT, (Uint16) -1, &tmpAbsLists[1] ) )
    {   result = FALSE;     /* error, clean-up later */
    }

    /** if result != FALSE, then:
     ** Add Main VDS and Reserve VDS continuation extents if any.
     ** Abort as soon as result becomes FALSE.
     **/
    for( pContItem = mc->contExtentList;
         pContItem != NULL && result != FALSE;
         pContItem = pContItem->next )
    {   Uint8 index = (pContItem->sequenceId == CEI_ID_MAINVDS) ? 0
                    : (pContItem->sequenceId == CEI_ID_RESVDS)  ? 1
                    :  2;       /* no VDS continuation extent */
#ifdef  TESTING_FAR_APART
  fprintf(uctout, "TESTING_FAR_APART VDS 02: %lu\n", index);
#endif
        if( index < 2 )
        { result = includeLogicalInAbsList( mc,
                                    &pContItem->aad,
                                     pContItem->adType,
                            (Uint16) -1, &tmpAbsLists[index] );
        }   /* in case of error, clean-up later */
    }       /* endfor pContItem */

    /* Final processing and message printing if tmpAbslists[]
     * was successfully created.
     */
    if( result == FALSE )       /* error */
    { VERBOSE00(uctout,
        "\n  ==>\tFailed to check \'far apart\'"
                " allocation for Main and Reserve VDS.\n");
    }
    else    /* preparations for VDS far apart check ok */
    { VERBOSE00(uctout,
        "\n  ==>\tChecking VDS \'far apart\'"
                " allocation for Main and Reserve VDS.\n");

      if( !faProcessLists( tmpAbsLists, TRUE) ) /* isVDS */
      { result = FALSE;         /* error, clean-up later */
      }
    }

    /* clean-up (also for all error cases).
     */
    allocationListFree(&tmpAbsLists[0]);    /* NULL */
    allocationListFree(&tmpAbsLists[1]);    /* NULL */
    return result;

}   /* end checkVDSFarApart() */


