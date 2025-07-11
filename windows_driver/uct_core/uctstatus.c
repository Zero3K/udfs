/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctstatus.c
 *
 * Description : UDF verifier status and status access functions.
 *
 * Author(s)   : Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "general.h"
#include "mytypes.h"
#include "uctnodes.h"
#include "uctgeneral.h"
#include "uctstatus.h"
#include "ucttimestamp.h"
#include "uctversion.h"

/* Verifier min and max UDF revision status ***************************
 * Static initialisation for the range that the verifier
 * is supposed to handle.
 */
static Uint16 uctMinUdfRevision = MIN_UDFREVISION;
static Uint16 uctMaxUdfRevision = MAX_UDFREVISION;
static int    uctUdfRevisionConflicts = 0;          /* count */
static int    uctUdfNsrFound = FALSE;
static bool   uctUdfCorrectDescriptorFound = FALSE;


/* set uctUdfNsrFound if NSR descriptor is found
 * in readVolumeRecognitionSequence().
 */
extern void notifyUdfNsrFound()
{   uctUdfNsrFound = TRUE;
}

/* set uctUdfCorrectDescriptorFound if
 * swapAndVerifyDescriptor() returns TRUE
 */
extern void notifyUdfCorrectDescriptorFound()
{   uctUdfCorrectDescriptorFound = TRUE;
}

static void printDescriptorPresenceSummary(char *indent)
{
    if( !uctUdfNsrFound )
    { VERBOSE00(uctout,
        "%sNSR missing in Volume Recognition Sequence.\n",
                indent);
    }
    if( !uctUdfCorrectDescriptorFound )
    { VERBOSE00(uctout,
        "%sNo correct UDF descriptor found on the medium.\n",
                indent);
    }
}

extern Uint16 getUctMinUdfRevision()
{
    return uctMinUdfRevision;
}

extern Uint16 getUctMaxUdfRevision()
{
    return uctMaxUdfRevision;
}

/* getUctUdfRevision: return UDF revision.
 * return value:
 * if uctMaxUdfRevision == uctMinUdfRevision
 * then uctMaxUdfRevision
 * else 0
 */
extern Uint16 getUctUdfRevision()
{
    if( uctMaxUdfRevision == uctMinUdfRevision )
         return uctMaxUdfRevision;
    else return 0;
}

/** warnIfUdfRevisionUncertain():
 ** For many tests, the UDF verifier can do a better job
 ** if the exact UDF revision of the medium is known instead
 ** of a UDF revision range.
 ** For tests where the exact UDF revision is needed, but not
 ** yet knwon, execute this function first as a warning and
 ** then execute the test at hand on the safe side using
 ** getUctMinUdfRevision() or getUctMaxUdfRevision().
 ** This means that the test may produce an error/warning message
 ** while not strictly needed if the exact UDF revision was known.
 **
 ** This function prints a warning if the exact UDF revision is
 ** not yet known and the revisions tstLow and tstHigh both fall
 ** inside the currently known UDF revision range (the uncertain range),
 ** where tstLow < tstHigh.
 ** E.g. for a test where it is important to know if the medium
 **      UDF revision is (2.00 and lower) or (2.01 and higher),
 **      call: warnIfUdfRevisionUncertain(0x200, 0x201);
 **/
extern void warnIfUdfRevisionUncertain( Uint16 tstLow,
                                        Uint16 tstHigh )
{   UCTASSERT( tstLow < tstHigh );
    if(   tstLow  >= getUctMinUdfRevision()
       && tstHigh <= getUctMaxUdfRevision() )
    { MLIMITbegin( WARN01level, uctMessageLimit);   /* both in known range */
        fprintf(uctout,
          "\tWarning: Known UDF revision range so far: ");
        printUdfRevisionRange(MLIMIT.vl, ". The exact\n"
          "-\t\t revision has not been determined yet, but is needed for the\n"
          "-\t\t next test. The UDF verifier can do a better job if the exact\n"
          "-\t\t UDF revision is known, please use the -udf <rev> option.\n");
      MLIMITend;
    }
}

static Uint16 knownUdfRevisions[] =
    { 0x102, 0x150, 0x200, 0x201, 0x250, 0x260 };

/* isKnownUdfRevision:
 * return TRUE if revision is member of
 * knownUdfRevisions[] else return FALSE
 */
extern bool isKnownUdfRevision(Uint16 revision)
{   int n,
        nmbRev = sizeof(knownUdfRevisions)
               / sizeof(knownUdfRevisions[0]);

    for( n = 0; n < nmbRev; n++ )
    {   if( revision == knownUdfRevisions[n] )
        {   return TRUE;    /* found */
        }
    }
    return FALSE;   /* not found */
}

/* printUdfRevision():
 * Print UDF revision string and additional text.
 * revision is coded in hex (BCD in fact),
 * e.g. 0x200 for UDF revision 2.00
 *
 * Return value: FALSE for unregistered revision,
 *         else: TRUE.
 */
extern bool printUdfRevision(Uint8 vLevel, Uint16 revision,
                             char *additionalText)
{
    bool result = TRUE;

    ifVERBOSE(vLevel)
    {   fprintf(uctout, "%x.%02x", (revision >> 8), (revision & 0xFF));
    }
    ENDif;

    if( !isKnownUdfRevision(revision) )
    {   ifVERBOSE(vLevel) fprintf(uctout, " (unknown)");
        ENDif;
        result = FALSE;
    }
    if( additionalText != NULL )
    {   ifVERBOSE(vLevel) fprintf(uctout, "%s", additionalText);
        ENDif;
    }
    return result;

}   /* end printUdfRevision() */

static void printRevRange(Uint16 minRev, Uint16 maxRev,
                          char *endTxt, Uint8 vLevel)
{
    if( minRev == maxRev ) {
        printUdfRevision(vLevel, minRev, " only");
    } else {
        printUdfRevision(vLevel, minRev, " thru ");
        printUdfRevision(vLevel, maxRev, NULL);
    }
    if( endTxt != NULL )
    {   ifVERBOSE(vLevel) fprintf(uctout, "%s", endTxt);
        ENDif;
    }
}

/* Print verifier UDF revision range
 */
extern void printUdfRevisionRange(Uint8 vLevel, char *endTxt)
{
    printRevRange(uctMinUdfRevision,uctMaxUdfRevision, endTxt, vLevel);
}


static void printUdfRevisionSummary()
{
    VERBOSE00(uctout,"  Final UDF Revision range: ");
    printUdfRevisionRange(VERBOSE00level, "");  /* no "\n" */

    if( uctUdfRevisionConflicts != 0 )
    { VERBOSE00(uctout, "\t\tNote:\n"   /* at end of previous line */
        "-   This may not be correct, because of at least"
                            " %lu UDF Revision conflict%s.\n"
        "-   Use the \"-udf <revision>\" option to verify"
                            " for a specific UDF revision.\n"
        "-   The verifier can do a better job if"
                            " the -udf <revision> option is used.\n",
                    uctUdfRevisionConflicts,
           PLURAL_S(uctUdfRevisionConflicts));
    }
    VERBOSE00(uctout, "\n");    /* extra line in case of conflicts */
}

/* Modify the current verifier UDF revision range
 *  (static uctMinUdfRevision to uctMaxUdfRevision included).
 * Proposed UDF revision range :
 *  (minRevision to maxRevision included).
 *
 * Modify only by shrinking to the overlapping range.
 * There is a UDF revision range conflict if both
 * ranges do not overlap.
 *
 * return value: FALSE for a UDF revision range conflict,
 *         else: TRUE
 */
extern bool modifyUdfRevisionRange(Uint16 minRevision, Uint16 maxRevision,
                                   char  *reason)
{
    UCTASSERT( uctMinUdfRevision <= uctMaxUdfRevision );

    if(    minRevision > maxRevision        /* assert */
        || minRevision > uctMaxUdfRevision
        || maxRevision < uctMinUdfRevision )
    {
        /* (fatal?) Error, no overlapping of new and current range
         */
        uctUdfRevisionConflicts++;
        MLIMITbegin(ERROR00level, MLIMITdefault05);
          fprintf(uctout,
            "  ==>\tError: UDF Revision conflict because of \"%s\"\n"
                "-\t        Current UDF Revision range: ", reason);
          printUdfRevisionRange(VERBOSE00level, "\n");
          fprintf(uctout,
                 "-\t       Proposed UDF Revision range: ");
          printRevRange(minRevision, maxRevision, "\n", VERBOSE00level);
          if( minRevision > maxRevision )   /* assert */
          { fprintf(uctout,
              "- ====> PLEASE REPORT with above text <====\n");
          }
        MLIMITend;

        return FALSE;       /* error, revision range conflict */
    }

    /*    minRevision <= maxRevision
     * && minRevision <= uctMaxUdfRevision
     * && maxRevision >= uctMinUdfRevision
     * so (partly) overlap is guaranteed.
     * maybe shrink current range to overlapping part
     */
    if(    minRevision > uctMinUdfRevision
        || maxRevision < uctMaxUdfRevision )
    {   /* modify UDF revision range
         */
        VERBOSE00(uctout,"  ==>\tChanged UDF Revision range from: ");
        printUdfRevisionRange(VERBOSE00level, " to: ");

        uctMinUdfRevision = (Uint16) MAX(minRevision, uctMinUdfRevision);
        uctMaxUdfRevision = (Uint16) MIN(maxRevision, uctMaxUdfRevision);

        printUdfRevisionRange(VERBOSE00level, "\n");
        VERBOSE00(uctout, "-\tbecause of \"%s\"\n", reason);
    }
    return TRUE;

}   /* end modifyUdfRevisionRange() */


/* Tag Descriptor Version functions ***********************************
 * ECMA-167 3rd edition 3/7.2.2, 4/7.2.2.
 *          ===
 * Normally the value is:
 *      2 for UDF 1.50-
 *      3 for UDF 2.00+.
 * For UDF 2.00+ however, 2 is also allowed as
 * backward compatibility. When upgrading a file
 * system from UDF 1.50- to UDF 2.00+ it is not
 * required to rewrite all Descriptor Version values.
 */
static Uint32 countDescriptorVersion2 = 0,
              countDescriptorVersion3 = 0;

/* print oneliner for final status report
 */
static void printDescriptorVersionSummary()
{
    if( countDescriptorVersion2 && countDescriptorVersion3 )
    { VERBOSE00(uctout,
        "  Found a mix of Descriptor Version values 2 and 3"
                                        " (cnt: %lu, %lu)\n",
        countDescriptorVersion2, countDescriptorVersion3);
    }
    if(   getUctMinUdfRevision() < 0x200    /* Min: on the safe side */
       && countDescriptorVersion3 )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        VERBOSE00(uctout,
          "   Fatal error: Found %lu ECMA-167 3rd edition Descriptors.\n"
          "-\t\tUDF ", countDescriptorVersion3);
        printUdfRevision( MLIMIT.vl, getUctMinUdfRevision(),
                    " uses ECMA-167 2nd edition (=ISO/IEC 13346:1995).\n"
          "-\t\tThis may also cause many other errors"
                                              " as reported above.\n" );
     MLIMITend;
    }
}

/* warn for first occurrence of mix.
 */
static void warnForFirstDescriptorVersionMix( Tag *t )
{
    MLIMITbegin(WARN01level,uctMessageLimit);
      printMessageHead( MLIMIT.vl, (Byte*) t,
                        (Byte*) &t->descriptorVersion,
            "Warning: Tag Descriptor Version: ");
      fprintf(uctout, "%lu.\n"
        "-\t\t First occurrence of Descriptor Version 2 and 3 mix.\n"
        "-\t\t Found so far: value 2: %lu time%s,"
                               " value 3: %lu time%s,\n"
        "-\t\t ECMA-167 3rd edition 3/7.2.2, 4/7.2.2.\n",
        t->descriptorVersion,
        countDescriptorVersion2, PLURAL_S(countDescriptorVersion2),
        countDescriptorVersion3, PLURAL_S(countDescriptorVersion3));
    MLIMITend;
}

/* checkDescriptorVersion()
 * verify Descriptor Version and handle status,
 * ECMA-167 3rd edition 3/7.2.2, 4/7.2.2.
 *          ===
 */
extern bool checkDescriptorVersion( Tag *t, Node *node,
                                    UdfMountContext *mc )
{   bool result = TRUE;

    switch( t->descriptorVersion )
    {
    case 2:
        /* Normally, this is UDF 1.50-, but UDF 2.00+ is
         * allowed too, see ECMA-167 3rd, 3/7.2.2, 4/7.2.2.
         * Error for EFE with descriptorVersion 2.
         * Warn for UDF 2.00+ with FSD descriptorVersion 2.
         */
        if((Uint32)(countDescriptorVersion2 + 1) != 0 )
        {   countDescriptorVersion2++;
        }
        if(   countDescriptorVersion2 == 1
           && countDescriptorVersion3 != 0 )
        {   warnForFirstDescriptorVersionMix(t);
        }
        if( t->tagIdentifier == tidEFE )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageExpectedU32( MLIMIT.vl,
              (Byte*) t, (Byte*) &t->descriptorVersion,
              "Error: EFE Tag Descriptor Version", "%lu",
              2, 3,
              ",\n-\t\t\tEFE only defined in ECMA-167 3rd edition.\n");
            nodePrintUnicodeNameContLine(node,mc);
          MLIMITend;
          result = FALSE;
        }
        if(   t->tagIdentifier == tidFSD
           && getUctMinUdfRevision() >= 0x200 )
        { MLIMITbegin(WARN01level, uctMessageLimit);
            printMessageExpectedU32( MLIMIT.vl,
                (Byte*) t, (Byte*) &t->descriptorVersion,
                "Warning: Tag Descriptor Version", "%lu",
              t->descriptorVersion, 3, " for\n"
              "-\t\t UDF 2.00 and higher."
                        " No System Stream Directory can\n"
              "-\t\t can be defined,"
                        " ECMA-167 3rd edition 4/14.1.18.\n");
          MLIMITend;
          /* no FALSE return result */
        }
        break;
    case 3:
        /* this must at least be UDF 2.00
         */
        if((Uint32)(countDescriptorVersion3 + 1) != 0 )
        {   countDescriptorVersion3++;
        }
        if(   countDescriptorVersion3 == 1
           && countDescriptorVersion2 != 0 )
        {   warnForFirstDescriptorVersionMix(t);
        }
        result = modifyUdfRevisionRange(0x200, MAX_UDFREVISION,
                                        "Descriptor Version 3");
        break;
    default:
        MLIMITbegin(ERROR00level,uctMessageLimit);
          printMessageExpectedU32( MLIMIT.vl,
            (Byte*) t, (Byte*) &t->descriptorVersion,
            "Error: Tag Descriptor Version", "%lu",
            t->descriptorVersion, 2, " or 3,\n"
            "-\t\t ECMA-167 3rd edition 3/7.2.2, 4/7.2.2.\n");
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        result = FALSE;
        break;
    }
    return result;
}


/* Implementation Entity Identifiers status ***************************
 * Log all newly encountered Implementation Entity Identifiers
 */

/* printEntityID():
 * Print EntityId
 * Return value: nmb of errors found.
 *
 * Note:
 *  MLIMIT... macros are not used here because printEntityID
 *  is only called twice for each EntityId, once at first
 *  occurrence and once from printEntityIDSummary().
 *  When first called, the total count for this EntityId is
 *  unknown and therefore, the value of <eidCount> shall be 0.
 *  When called from printEntityIDSummary(), the value for
 *  <eidCount> will be the actual EntityId count (>0)
 *  (EntityIDListItem.count) from the EntityID list.
 *  In the latter case, the global error and warning counts
 *  will be updated properly. Note that this is independent
 *  of the value of vLevel;
 */
extern int printEntityID( EntityID *eid, ENTITY_SUFFIX_TYPE suffixType,
                          Uint32 eidCount, Uint8 vLevel )
{
    Uint8    domainFlags;
    Byte    *b, *bytePointer = NULL;
    Uint32   n, nmbBytes  = 0;
    Uint16   udfRevision  = 0, introUdfRevision;
    Uint8    osClass      = 0;
    Uint8    osIdentifier = 0;
    Uint16   osCI         = 0;  /* OS Class (MSbyte) + Id (LSbyte) */
    char    *txt, *suffixText = NULL;
    bool     isIllegal, isIllegalId, isConflicting, isEmpty,
             dvdFlagBitSet  = FALSE;
    int      cntErrors   = 0,
             cntWarnings = 0;

    /* set suffix type name, OS values
     * and reserved or implementationUse bytes
     */
    switch( suffixType )
    {
    case ENTITY_SUFFIX_DOMAIN:
        suffixText  = "Domain";
        udfRevision =     eid->idSuffix.domainSuffix.udfRevision;
        bytePointer =     eid->idSuffix.domainSuffix.reserved;
        nmbBytes = sizeof(eid->idSuffix.domainSuffix.reserved);
        break;
    case ENTITY_SUFFIX_UDF:
        suffixText   = "UDF";
        udfRevision  =    eid->idSuffix.udfSuffix.udfRevision;
        osClass      =    eid->idSuffix.udfSuffix.osClass;
        osIdentifier =    eid->idSuffix.udfSuffix.osIdentifier;
        bytePointer  =    eid->idSuffix.udfSuffix.reserved;
        nmbBytes = sizeof(eid->idSuffix.udfSuffix.reserved);
        break;
    case ENTITY_SUFFIX_IMPL:
        suffixText = "Implementation";
        osClass      =    eid->idSuffix.implSuffix.osClass;
        osIdentifier =    eid->idSuffix.implSuffix.osIdentifier;
        bytePointer  =    eid->idSuffix.implSuffix.implementationUse;
        nmbBytes = sizeof(eid->idSuffix.implSuffix.implementationUse);
        break;
    case ENTITY_SUFFIX_APPL:    /* UDF 2.01, undefined for lower revisions */
        suffixText   = "Application";
        bytePointer  =    eid->idSuffix.applSuffix.implementationUse;
        nmbBytes = sizeof(eid->idSuffix.applSuffix.implementationUse);
        break;
    default:
        UCTASSERT( FALSE ); /* not here please */
        break;
    }

    ifVERBOSE(vLevel) fprintf(uctout,"\t%s Entity Identifier\n", suffixText);
    ENDif;

    /* check if an empty EntityID (all bytes #00)
     * In that case, no EntityID (regid) is defined according to
     * ECMA. For UDF, this is only valid for one Application Suffix case.
     */
    if( verifyZeros((Byte *) eid, sizeof(EntityID), NULL, NULL, NULL) )
    {   ifVERBOSE(vLevel) fprintf(uctout,"\t  <empty>\n");
        ENDif;
        /* This is allowed for the PVD Application Identifier field,
         * see UDF 2.2.9. For the other Application suffix cases,
         * extra tests are in place, e.g. in checkAndMapPartitionInfo(),
         * so we can quit here for all empty Application suffix cases.
         * Continuation for other cases will on purpose cause extra
         * messages like "Empty Identifier, ...".
         */
        if( suffixType == ENTITY_SUFFIX_APPL )  /* done */
        { return cntErrors;
        }
    }

    /* Flags: only print if non-zero
     */
    if( eid->Flags != 0 )
    {
        ifVERBOSE(vLevel) fprintf(uctout,
                    "\t  Flags\t\t     : #%02X  (illegal)\n", eid->Flags);
        ENDif;
        cntErrors++;
        dvdFlagBitSet = ((eid->Flags & 0x02) != 0);
    }

    /* Identifier: do not print Identifier trailing '\0' chars
     * show <empty> if empty (all zero or space)
     * If empty, then:
     *  - For ENTITY_SUFFIX_APPL this is no error or warning
     *  - For ENTITY_SUFFIX_IMPL this is a missing Developer ID warning
     *  - For all other suffix types this is an error.
     */
    isEmpty = TRUE;
    for( n = 0; n < sizeof(eid->Identifier); n++ )
    { if(   eid->Identifier[n] != '\0'
         && eid->Identifier[n] != ' ' )
      { isEmpty = FALSE;
        break;
      }
    }
    ifVERBOSE(vLevel)
    {   fprintf(uctout, "\t  Identifier\t     : ");
        printBytesName((Byte *) (eid->Identifier),
                       ENTITYID_IDSIZE, FALSE, vLevel);
        fprintf(uctout,"\n");
        if( isEmpty )
        { fprintf(uctout,
            "   =>\t%s: Empty Identifier,%s UDF 2.1.5.2+3.\n",
              (suffixType == ENTITY_SUFFIX_APPL) ? "   Note"
            : (suffixType == ENTITY_SUFFIX_IMPL) ? "Warning"
                                                 : "  Error",
              (suffixType == ENTITY_SUFFIX_IMPL)
                 ? " missing Developer ID," : "" );
        }
    }
    ENDif;
    /* count warnings or errors independent of vLevel
     */
    if(      isEmpty && suffixType == ENTITY_SUFFIX_IMPL )
    { cntWarnings++;
    }
    else if( isEmpty && suffixType != ENTITY_SUFFIX_APPL )
    { cntErrors++;
    }

    /* Print UDF revision,
     * maybe modify verifier UDF revision for
     * ENTITY_SUFFIX_DOMAIN and ENTITY_SUFFIX_UDF
     */
    isIllegal = isConflicting = FALSE;
    switch( suffixType )
    {
    case ENTITY_SUFFIX_DOMAIN:
    case ENTITY_SUFFIX_UDF:
        if( !isKnownUdfRevision(udfRevision) )
        {   isIllegal = TRUE;
            cntErrors++;                /* count as error */
        }
        else if(   udfRevision > getUctMaxUdfRevision()
                || udfRevision < getUctMinUdfRevision() )
        {   isConflicting = TRUE;       /* not in range */
            cntErrors++;                /* count as error */
            uctUdfRevisionConflicts++;  /* count as rev conflict */
        }
        ifVERBOSE(vLevel)
        {   fprintf(uctout, "\t  UDF revision\t     : ");
            printUdfRevision(vLevel, udfRevision,
                 (isIllegal)    ? "  (illegal)\n"
              : ((isConflicting) ? "  (conflicting)\n" : "\n"));
        }
        ENDif;

        /* only (try to) narrow UDF revision range for legal in-range values
         */
        if( (!isConflicting) && (!isIllegal) )
        {   (void) modifyUdfRevisionRange(udfRevision, udfRevision,
                    (suffixType == ENTITY_SUFFIX_DOMAIN)
                        ? "Domain EntityID UDF revision"
                        : "UDF EntityID UDF revision");
        }
        break;
    default:        /* no action (keep compiler happy) */
        break;
    }

    /* Domain flags, OS Class and OS Identifier
     */
    switch( suffixType )
    {
    case ENTITY_SUFFIX_DOMAIN:
        domainFlags = eid->idSuffix.domainSuffix.domainFlags;
        ifVERBOSE(vLevel)
        {   fprintf(uctout, "\t  Domain flags\t     : #%02X",
                                domainFlags);
        }
        ENDif;

        if( (domainFlags & DOMAINFLAGS_Reserved) != 0 )
        {
            ifVERBOSE(vLevel) fprintf(uctout, "  (illegal)");
            ENDif;
            cntErrors++;
        }

        ifVERBOSE(vLevel)
        {
            if(      (domainFlags & DOMAINFLAGS_HardWriteProtect) != 0
                  && (domainFlags & DOMAINFLAGS_SoftWriteProtect) != 0 )
                 txt = "Hard and Soft";
            else if( (domainFlags & DOMAINFLAGS_HardWriteProtect) != 0 )
                 txt = "Hard";
            else if( (domainFlags & DOMAINFLAGS_SoftWriteProtect) != 0 )
                 txt = "Soft";
            else txt = "";
            if( *txt != '\0' )
            {
                fprintf(uctout, "  %s Write-Protect", txt);
            }
            fprintf(uctout, "\n");
        }
        ENDif;
        break;
    case ENTITY_SUFFIX_UDF:
    case ENTITY_SUFFIX_IMPL:
        /* OS Classes and OS Identifier values introduced in different
         * UDF revisions. Concider all legal, but warn if used in a UDF
         * revision older than the one were the OS Class/Id was introduced.
         * Determine OS Class UDF introduction revision, etc.
         */
        isIllegal = (osClass > OSCL_MAX);   /* reserved OS Class */
        isIllegalId = FALSE;
        ifVERBOSE(vLevel) fprintf(uctout,
            "\t  OS Class\t     : #%02X  %s%s\n",
                osClass, OSCL_TEXT(osClass),
                (isIllegal) ? " (illegal)" : "");
        ENDif;

        /* Create Uint16 osCI with OS Class (MSbyte) and OS Id (LSbyte).
         * Note that isIllegal is already set for illegal OS Class.
         * Check OS Class / OS Identifier combination,
         * flag illegal OS Id for legal OS Class only.
         */
        introUdfRevision = MIN_UDFREVISION;     /* default */
        osCI = (osClass << 8) + osIdentifier;
        switch( osCI )
        {
          case OSCI_DOS:            /* Introduced in UDF 1.02 */
          case OSCI_OS2:
          case OSCI_MACOS9:
          case OSCI_UNIX_GEN:
          case OSCI_UNIX_IBMAIX:
          case OSCI_UNIX_SUNOS:
          case OSCI_UNIX_HPUX:
          case OSCI_UNIX_SGI:
            introUdfRevision = 0x102;
            break;
          case OSCI_UNIX_LINUX:     /* Introduced in UDF 1.50 */
          case OSCI_UNIX_MKLIN:
          case OSCI_UNIX_FREEBSD:
          case OSCI_WIN9X:
          case OSCI_WINNT:
            introUdfRevision = 0x150;
            break;
          case OSCI_OS400:          /* Introduced in UDF 2.01 */
          case OSCI_BEOS:
          case OSCI_WINCE:
            introUdfRevision = 0x201;
            break;
          case OSCI_MACOSX:         /* Introduced in UDF 2.50 */
            introUdfRevision = 0x250;
            break;
          case OSCI_UNIX_NETBSD:    /* Introduced in UDF 2.50 errata */
            introUdfRevision = 0x250;
            break;
          default:
            if(    osClass != OSCL_UNDEFINED
               && !isIllegal )
            { /* legal osClass, but illegal osId
               */
              isIllegal = isIllegalId = TRUE;   /* illegal osId */
            }
            break;
        }           /* endswitch( osCI ) */

        /* Flag illegal osId only for legal osClass
         * For illlegal osClass, osId is flagged "(reserved)"
         */
        ifVERBOSE(vLevel) fprintf(uctout,
                "\t  OS Identifier\t     : #%02X  %s%s\n",
                osIdentifier, OSCI_TEXT(osCI),
                (isIllegalId) ? " (illegal)" : "");
        ENDif;

        if( isIllegal )     /* illegal osClass or osId */
        {   cntErrors++;    /* count as one */
        }
        else if( introUdfRevision > getUctMaxUdfRevision() )
        {
            /* OS Class was introduced later than Max UDF revision
             * This note/warning may be missed
             *  when : (getUctMaxUdfRevision() != getUctMinUdfRevision())
             *  but not when called from printEntityIDSummary()
             *  where: (getUctMaxUdfRevision() == getUctMaxUdfRevision()).
             *
             * Reduce number of identical "OS Class/Id" messages:
             *  Unless in final summary or -mlimit 0, a message will only
             *  be printed if the value of osCI changes with respect to
             *  a previous message.
             */
#undef  OSCLASSID_WARNING_INSTEAD_OF_NOTE
#ifdef  OSCLASSID_WARNING_INSTEAD_OF_NOTE
            cntWarnings++;
#endif
            ifVERBOSE(vLevel)   /* no MLIMITbegin, see note above */
            { static Uint16 previousOsCI = MAX_UINT16;
              if(   eidCount != 0           /* final summary */
                 || uctMessageLimit == 0    /* 'all infinite' message limit */
                 || osCI != previousOsCI )  /* first time or change of osCI */
              { previousOsCI = osCI;
                fprintf(uctout,
#ifdef  OSCLASSID_WARNING_INSTEAD_OF_NOTE
                  "\tWarning: OS Class/Id \"%s\" was introduced\n"
#else
                  "\t   Note: OS Class/Id \"%s\" was introduced\n"
#endif
                       "-\t\t in UDF ", OSCI_TEXT(osCI));

                printUdfRevision(vLevel, introUdfRevision,
                               ". Current UDF revision range: ");
                printUdfRevisionRange(vLevel, ".\n");
              }
            }
            ENDif;
        }
        break;
    default:        /* no action (keep compiler happy) */
        break;
    }

    /* Print Reserved or Implementation Use bytes if any non-zero
     * byte found. This is illegal for Reserved field.
     */
    if( ! verifyZeros(bytePointer, nmbBytes, NULL, NULL, NULL) )
    {
        ifVERBOSE(vLevel)
        {   if(    suffixType == ENTITY_SUFFIX_DOMAIN
                || suffixType == ENTITY_SUFFIX_UDF )
                 fprintf(uctout, "\t  Reserved\t     :");
            else fprintf(uctout, "\t  Implementation Use :");

            for( n = 0, b = bytePointer; n < nmbBytes; n++, b++)
            {
                fprintf(uctout, " #%02X", *b);
            }
        }
        ENDif;

        if(    suffixType == ENTITY_SUFFIX_DOMAIN
            || suffixType == ENTITY_SUFFIX_UDF )
        {
             ifVERBOSE(vLevel) fprintf(uctout, "  (illegal)");
             ENDif;
             cntErrors++;
        }
        ifVERBOSE(vLevel) fprintf(uctout, "\n");
        ENDif;
    }
    if( cntErrors )
    { ifVERBOSE(vLevel) /* no MLIMITbegin, see note above */
      { fprintf(uctout,
                    "\tError: %lu error%s in this EntityID,"
                            " UDF 2.1.5, 6.3, ECMA 1/7.4.\n%s",
            cntErrors, PLURAL_S(cntErrors), (!dvdFlagBitSet)
            ? "" : "-\t Note: Flags bit 1 (Protected) may be"
                                        " set because of a\n"
                   "-\t       UDF versus DVD-Video spec conflict,"
                                        " UDF 2.1.5.1.\n");
      }
      ENDif;
    }

    /* Update global error and warning counts when called
     * from printEntityIDSummary(), see note above.
     * Count multiple errors in an EntityID as one.
     */
    if( eidCount != 0 ) /* called from printEntityIDSummary() */
    {   if( cntErrors != 0 )
        {   uctGlobalErrorCount++;  /* count as one */
            uctGlobalErrorOccurrences += eidCount;
        }
        if( cntWarnings != 0 )
        {   uctGlobalWarningCount++; /* count as one */
            uctGlobalWarningOccurrences += eidCount;
        }
    }
    return cntErrors;

}   /* end printEntityID() */


/* storeAndPrintNewEntityID():
 * Maintains a linked list of EntityID's encountered and prints
 * the newly added ones. Maintains sorted order and occurrence
 * count as well.
 * Primary sort on ENTITY_SUFFIX_TYPE, secondary on EntityID bytes.
 * A summary of all info in the linked list can be printed
 * using printEntityIDSummary();
 *
 * Return value: FALSE if a new EntityID is added to the linked
 *                     list AND that EntityID contains errors
 *          else TRUE;
 *
 * So an erroneus EntityID is only flagged and counted once
 * when it is encountered for the first time, see also remark
 * on global errors and warnings in printEntityIDSummary().
 */
typedef struct EntityIDListItem
{
    EntityID                 eid;
    ENTITY_SUFFIX_TYPE       suffixType;
    Uint32                   count;
    struct EntityIDListItem *next;
} EntityIDListItem;

static EntityIDListItem *theEntityIDList = NULL;

extern bool storeAndPrintNewEntityID(EntityID *eid,
                                     ENTITY_SUFFIX_TYPE suffixType,
                                     Byte *descriptor)
{
    EntityIDListItem *list, *previous, *newItem;
    int errors = 0;

    for( list = theEntityIDList, previous = NULL;
         list != NULL;
         list = list->next )
    {
        int result = (suffixType - list->suffixType);
        if( result == 0 )       /* primary sort equal */
        {
            result = memcmp(eid, &(list->eid), sizeof(EntityID));
            if( result == 0 )   /* secondary sort equal */
            {
                list->count++;  /* found equal EntityID, count */
                /* print only for 'all infinite' message limit
                 */
                if( uctMessageLimit == 0 )
                {   ifVERBOSE(INFO01level)
                    { printMessageHead( INFO01level, descriptor, (Byte*)eid,
                        "    Entity Identifier:\n");
                    }
                    ENDif;
                    errors = printEntityID(eid, suffixType, 0, INFO01level);
                }
                return TRUE;    /* done, NO error check */
            }
        }
        if( result < 0 )        /* found new one */
        {
            break;              /* print and insert in list */
        }
        previous = list;        /* save for insert */
    }

    /* here only for new EntityId
     * print and insert in list
     */
    ifVERBOSE(INFO01level)
    {
        fprintf(uctout,"  ==>");
        printMessageHead( INFO01level, descriptor, (Byte*)eid,
            "New Entity Identifier (regid):\n");
    }
    ENDif;
    errors = printEntityID(eid, suffixType, 0, INFO01level);

    if( (newItem = (EntityIDListItem *) tst_malloc( sizeof(EntityIDListItem),
                                                __FILE__,__LINE__)) == NULL )
    {
        uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    memcpy( &(newItem->eid), eid, sizeof(EntityID) );
    newItem->suffixType = suffixType;
    newItem->count = 1;
    newItem->next  = list;

    if( previous == NULL )      /* insert before first Item */
         theEntityIDList = newItem;
    else previous->next  = newItem;

    return (errors == 0);

}   /* end storeAndPrintNewEntityID() */

/* printEntityIDSummary():
 * Prints summary of all info in the linked list
 * built by storeAndPrintNewEntityID().
 *
 * Implementation note:
 * Global error and warning counts do not yet include errors
 * and warnings in EntityIDs, handle here using printEntityID()
 * because number of occurrencies for each EntityID is known now.
 */
extern void printEntityIDSummary()
{
    EntityIDListItem *list;
    Uint32 freezeRevisionConflicts;
    int    errors = 0;

    /* avoid dubble counting of UDF revision conflicts
     */
    freezeRevisionConflicts = uctUdfRevisionConflicts;

    VERBOSE00(uctout,"\n====>\tEncountered EntityID (regid) summary:\n");

    if( theEntityIDList == NULL )
    {   VERBOSE00(uctout, "\tNone found\n");
    }
    else
    {   VERBOSE00(uctout, "\n count\tEntityID\n\n");
        for( list = theEntityIDList;
             list != NULL;
             list = list->next )
        {   VERBOSE00(uctout, "%5lu", list->count);
            errors += printEntityID(&list->eid, list->suffixType,
                                     list->count, VERBOSE00level);
        }
        VERBOSE00(uctout, "\n");
        if( errors != 0 )
        {   VERBOSE00(uctout,
                "\t%lu error%s found in EntityID summary\n",
                    errors, PLURAL_S(errors));
        }
        VERBOSE00(uctout,
                "  These EntityIDs are also shown above"
                            " when read for the first time\n");
    }

    /* avoid dubble counting of UDF revision conflicts
     */
    uctUdfRevisionConflicts = freezeRevisionConflicts;

}   /* end printEntityIDSummary() */


/* Disaster Recovery Support for Tag Serial Numbers ******************
 */
static bool TagSerialNumberSupport = TRUE;  /* means maybe */

extern void resetTagSerialNumberSupport()
{
    TagSerialNumberSupport = FALSE;
}

static void printTagSerialNumberSummary()
{
    if( !TagSerialNumberSupport )
    {
        VERBOSE00(uctout,
            "  Disaster Recovery for Tag Serial Numbers not supported\n");
    }       /* else maybe */
}

/* General Print Functions *******************************************
 */

/* UnicodeIsPrint() :
 * Simple test if unicode char is printable.
 * Assumption is that a minimal range 0x0020 until 0x007E included
 * is 'printable' on printer and display on any OS.
 */
static bool UnicodeIsPrint( unicode_t uChar )
{
    if( uChar >= 0x0020 && uChar < 0x007F )
    {
        return TRUE;
    }
    return FALSE;
}

/* printUnicodeAndBytesName() :
 * Print simple unambiguous representation of unicode or byte name
 * on uctout. Called by printUnicodeName() and printBytesName().
 *
 * Characters #0020 till #007E included are printed in ascii form (mind exceptions).
 * Other characters and the exceptions are printed in hex form.
 * The exception characters are: '"' and '#' and if( isPath == TRUE ),
 * also the '/' character (because the verifier uses it as path separator).
 *
 * printing in hex form:
 *  if isBytesName then as "#HH" else as "#HHHH".
 *
 * If uName == NULL, then only print: <null>
 *
 * Return value:
 *  Number of real byte chars printed (not unicode chars !!).
 *  Mind that this can be zero if verbose level does not apply.
 */
static Uint32 printUnicodeAndBytesName(unicode_t *uName,
                                       Uint32 nameLen,
                                       bool  printTrailZeros,
                                       bool  isPath,
                                       bool  isBytesName,
                                       Uint8 vLevel)
{
    Uint32     i;
    unicode_t *u;
    Uint32     charsPrinted = 0;

    ifNOTVERBOSE(vLevel) return 0;  /* no action */
    ENDif;

    if( uName == NULL )
    {   fprintf(uctout, "<null>");
        return 6;                   /* that's all */
    }

    if( ! printTrailZeros )
    {
        while( nameLen > 0 && uName[nameLen-1] == 0x0000 ) nameLen--;
    }
    fprintf(uctout, "\"");
    charsPrinted++;

    for( i = 0,  u = uName;
         i < nameLen;
         i++,    u++ )
    {
        if(    UnicodeIsPrint(*u)
            && (*u) != (unicode_t) ((Uint8) '"')
            && (*u) != (unicode_t) ((Uint8) '#')
            && (  !isPath
                || (*u) != (unicode_t) ((Uint8) '/')) )
        {   fprintf(uctout, "%c", (char) (*u));
            charsPrinted++;
        }
        else if( isBytesName )  /* printBytesName() hex format */
        {   fprintf(uctout, "#%02X", (Uint8) *u);
            charsPrinted += 3;  /* 3 positions */
        }
        else                    /* printUnicodeName() hex format */
        {   fprintf(uctout, "#%04X", (Uint16) *u);
            charsPrinted += 5;  /* 5 positions */
        }
    }
    fprintf(uctout, "\"");
    return ++charsPrinted;

}   /* end printUnicodeAndBytesName() */

/* printUnicodeName() :
 * Simple print unicode name on uctout.
 * Some characters may be printed in hex "#HHHH" form,
 * see printUnicodeAndBytesName().
 * Return value:
 *  Number of real byte chars printed (not unicode chars !!).
 *  Mind that this can be zero if verbose level does not apply.
 */
extern Uint32 printUnicodeName( unicode_t *uName, Uint32 nameLen,
                                bool printTrailZeros,
                                bool isPath,      Uint8 vLevel )
{
    return printUnicodeAndBytesName(uName, nameLen, printTrailZeros,
                            isPath, FALSE, vLevel); /* NOT isBytesName */
}   /* end printUnicodeName() */

/* printBytesName() :
 * Simple print byte characters name on uctout.
 * Some characters may be printed in hex "#HH" form,
 * see printUnicodeAndBytesName().
 * Return value:
 *  Number of real byte chars printed (not unicode chars !!).
 *  Mind that this can be zero if verbose level does not apply.
 */
extern Uint32 printBytesName( Byte *bName, Uint32 nameLen,
                              bool  printTrailZeros, Uint8 vLevel )
{
    unicode_t *tmp_uName;
    Uint32     n, result;

    /* convert to temp unicode array for printUnicodeAndBytesName()
     * <null> is printed if bName == NULL (or tst_malloc() failure).
     */
    tmp_uName = (bName == NULL) ? NULL
              : (unicode_t *) tst_malloc(nameLen * sizeof(unicode_t),
                                         __FILE__,__LINE__);
    for( n = 0;
         n < nameLen && tmp_uName != NULL;
         n++ )
    {   tmp_uName[n] = (unicode_t) bName[n];
    }
    result = printUnicodeAndBytesName(tmp_uName, nameLen, printTrailZeros,
                                      FALSE,             /* NOT isPath */
                                      TRUE, vLevel);    /* isBytesName */
    checkFree((void**)&tmp_uName);
    return result;

}   /* end printBytesName() */


/* Low level general functions ***************************************
 */

/* verifyZeros():
 * Test block of bytes that shall all have
 * the value #00.
 *
 * Input arguments:
 * beginZeros: pointer to first byte to be checked.
 * nBytes    : nmb of bytes to be checked.
 *
 * Output arguments:
 * *pCount    : nmb of bytes unequal to zero found
 * *pFirstIndex: index of first byte unequal to zero
 * *pFirstValue: value of first byte unequal to zero
 *
 * The pointers pCount, pFirstIndex and pFirstValue may
 * each be NULL, independent of each other, which means
 * that no value will be returned.
 *
 * Return value: TRUE if only zero bytes are found,
 *         else: FALSE.
 * Note: *pFirstIndex and *pFirstValue are undefined if
 *       TRUE is returned.
 */
extern bool verifyZeros(Byte   *beginZeros, size_t  nBytes,
                        size_t *pCount,     Uint32 *pFirstIndex,
                        Byte   *pFirstValue)
{
    Byte    *b;
    Uint32   n;
    bool     result = TRUE;     /* no byte unequal to 0 found yet */

    if( pCount != NULL ) *pCount = 0;   /* count required */

    for( n = 0, b = beginZeros;
         n < nBytes;
         n++, b++ )
    {
        if( *b != 0 )       /* byte unequal to zero found */
        {
            result = FALSE;
            if( pCount == NULL || *pCount == 0 )    /* first time */
            {
                if( pFirstIndex != NULL ) *pFirstIndex = n;
                if( pFirstValue != NULL ) *pFirstValue = *b;
                if( pCount == NULL )
                    return FALSE;       /* no count required, done */
            }
            if( pCount != NULL ) (*pCount)++;
        }
    }
    return result;

}   /* end verifyZeros() */

/* Print final status report.
 */
extern void printFinalStatusReport(bool fatalError)
{
    const MediumInfo *vmi = getTheMediumInfo(); /* verifier MediumInfo */

    VERBOSE00(uctout,"\n====>\tFinal verify status report\n\n");

    printDescriptorPresenceSummary( "  ");
    reportUndefinedTimezoneCompare();
    printUdfRevisionSummary();
    printDescriptorVersionSummary();
    printTagSerialNumberSummary();

    VERBOSE00(uctout,"\n  File System info\n");
    printMediumInfo(vmi);

    /* Print summed file body size.
     * if >= 1 Kbyte, then add "(yyy.xxx [KMG]byte)" for
     * Kbyte, Mbyte, Gbyte respectively.
     */
    VERBOSE00(uctout, "\n  Summed file body sizes: ");
    printUint64(VERBOSE00level, uctFileBodySize, FALSE, "%6lu");
    VERBOSE00(uctout, " bytes");
    if( uctFileBodySize >= 1024 )   /* at least 1 Kbyte */
    { VERBOSE00( uctout, "\t(" );
      nBytesDoublePrint4f( uctFileBodySize, ")" );
    }
    VERBOSE00(uctout, "\n");
    if( uctDoFileCRC )
    { VERBOSE00(uctout, "  Overall file body CRC : 0x%04X",
                    uctFileBodyCrc);
      if( uctFileBodyCrcSize != uctFileBodySize )
      { VERBOSE00(uctout, "  (over only ");
        printUint64(VERBOSE00level, uctFileBodyCrcSize, FALSE, "%6lu");
        VERBOSE00(uctout, " bytes)");
      }
      VERBOSE00(uctout, "\n");
    }

    VERBOSE00(uctout,
      "\n    Error count: %3lu\ttotal occurrences: %5lu  -> search for   \"error:\"\n"
        "  Warning count: %3lu\ttotal occurrences: %5lu  -> search for \"warning:\"\n"
      "\n  Additional notes may have been printed:\t  -> search for    \"note:\"\n",
          uctGlobalErrorCount, uctGlobalErrorOccurrences,
          uctGlobalWarningCount,uctGlobalWarningOccurrences);

    /* TODO: detect if any message limit was exhausted
     *       instead of following test:
     */
    if(   uctMessageLimit < MLIMITinfinite
       && uctMessageLimit != 0
       && (   (Uint32) uctMessageLimit <=
              (1 + uctGlobalErrorOccurrences - uctGlobalErrorCount)
           || (Uint32) uctMessageLimit <=
              (1 + uctGlobalWarningOccurrences - uctGlobalWarningCount)))
    { VERBOSE00(uctout,
        "\n  Note: At most %lu occurrence%s of each error or"
                                            " warning message\n"
          "-       ha%s been printed because of the \"-mlimit"
                                                " %lu\" %s.\n",
                 uctMessageLimit, PLURAL_S(uctMessageLimit),
                (uctMessageLimit == 1) ? "s" : "ve",
                 uctMessageLimit,
                (uctMessageLimit == MLIMITdefault)
                    ? "default"
                    : "option" );
    }

    if( fatalError )
    { VERBOSE00(uctout,
    "\n==> Error: A fatal error occurred,"  /* too late to be included in error count */
                    " UDF verification could not be completed.\n");
      printDescriptorPresenceSummary( "-   ");
    }

    VERBOSE00(uctout, "\n  Disclaimer:\n"
      "-\tThe number of errors and warnings is an indication only.\n"
      "-\tThere is no guarantee that the number of errors and\n"
      "-\twarnings as shown by the UDF verifier is correct.\n"
      "\n" );

}   /* end printFinalStatusReport() */


/* isEqualLogVolIds(): used by checkVolumeIdentifiersSummary()
 * Compare 2 Logical Volume Identifier unicode arrays.
 * Undefined or empty identifiers are considered unequal.
 */
static bool isEqualLogVolIds( unicode_t uId1[], int uLen1,
                              unicode_t uId2[], int uLen2 )
{
    if(   uLen1 <= 0        /* undefined or empty */
       || uLen2 <= 0        /* undefined or empty */
       || uLen1 != uLen2    /* length not equal */
       || memcmp(uId1, uId2,
                 uLen1 * sizeof(unicode_t))
            != 0 )          /* content not equal */
    {
        return FALSE;       /* empty or not equal */
    }
    return TRUE;            /* non-empty and equal */
}

/* checkVolumeIdentifiersSummary():
 * Show final summary of Volume, Volume Set,
 * Logical Volume and File Set identifiers.
 *
 * Check relation of logicalVolumeIdentifier in LVD, IUVD, FSD
 * and VAT2 checked in checkVolumeIdentifiersSummary().
 *
 * Show checkLogVolIds() errors and warnings at each call of
 * checkVolumeIdentifiersSummary(), but only count
 * global error/warning occurrences at first call.
 */
#define LogVolId_SIZE (offsetof(LogicalVolumeDescriptor,logicalBlockSize) - \
                       offsetof(LogicalVolumeDescriptor,logicalVolumeIdentifier))

extern void checkVolumeIdentifiersSummary( UdfMountContext *mc )
{
    unicode_t   uNameLogVolId_lvd[ LogVolId_SIZE - 2],
                uNameLogVolId_iuvd[LogVolId_SIZE - 2],
                uNameLogVolId_fsd[ LogVolId_SIZE - 2],
                uNameLogVolId_vat2[LogVolId_SIZE - 2],
               *uNameLogVolId_ref;
    VAT200Head *vat2Head;
    int         uLen_lvd, uLen_iuvd, uLen_fsd, uLen_vat2, uLen_ref;
           bool isEq_iuvd, isEq_fsd, isEq_vat2,
                expectVAT2, expectVAT2ButUndefinedOrEmpty;
    static bool cntErrWarn = TRUE;      /* only first call */

    /* No action for fatal error.
     */
    if( mc == NULL ||  mc->vi == NULL
       || (   mc->vi->pvd  == NULL
           && mc->vi->lvd  == NULL
           && mc->vi->iuvd == NULL) )
    {   return;         /* no action */
    }
    VERBOSE00(uctout,
               "\n====>\tVolume identifiers summary:\n");

    /* condition: mc != NULL &&  mc->vi != NULL
     *
     * uncompressDstring() returns a value that is equal
     * to -1 or higher, so -2 can be used to denote that
     * the descriptor was not correctly read.
     */
    uLen_lvd = uLen_iuvd = uLen_fsd = uLen_vat2 = -2;
    isEq_iuvd = isEq_fsd = isEq_vat2 = FALSE;

    /* check if VAT2 must be present,
     * and if it was correctly read
     */
    expectVAT2 = (   IS_PREF_PARTITION_FOUND(mc->virtualPref)
                  && getUctMinUdfRevision() >= 0x200 );

    vat2Head = NULL;
    if( expectVAT2 && mc->partitionMapInfo != NULL )
    { VirtualRecord *vr = &mc->partitionMapInfo[mc->virtualPref].vatRec;
      if(   vr->vatIntroRevision == 0x200       /* UDF 2.00+ VAT */
         && vr->vatFile != NULL )
      { vat2Head = (VAT200Head *) vr->vatFile;
      }
    }

    /* Show/check all Vol Set, etc identifiers
     *
     * Volume Identifier        : PVD
     * Volume Set Identifier    : PVD
     * Logical Volume Identifier: LVD, IUVD, FSD and maybe VAT2
     * File Set Identifier      : FSD
     */
    VERBOSE00(uctout, "\n\t    PVD:         Volume Identifier  [32]: ");
    if( mc->vi->pvd != NULL )
    { PRINTDSTRING( mc->vi->pvd->volumeIdentifier,
             sizeof(mc->vi->pvd->volumeIdentifier), "\n");
    }
    else
    { VERBOSE00(uctout, "< PVD missing>\n");
    }

    VERBOSE00(uctout,   "\t    PVD:     Volume Set Identifier [128]: ");
    if( mc->vi->pvd != NULL )
    { PRINTDSTRING( mc->vi->pvd->volumeSetIdentifier,
             sizeof(mc->vi->pvd->volumeSetIdentifier), "\n");
    }
    else
    { VERBOSE00(uctout, "< PVD missing>\n");
    }

    VERBOSE00(uctout,   "\t    LVD: Logical Volume Identifier [128]: ");
    if( mc->vi->lvd != NULL )
    { PRINTDSTRING( mc->vi->lvd->logicalVolumeIdentifier,
             sizeof(mc->vi->lvd->logicalVolumeIdentifier), "\n");
      uLen_lvd = uncompressDstring(LogVolId_SIZE,
                    mc->vi->lvd->logicalVolumeIdentifier,
                    uNameLogVolId_lvd);
    }
    else
    { VERBOSE00(uctout, "< LVD missing>\n");
    }

    VERBOSE00(uctout,   "\t   IUVD: Logical Volume Identifier [128]: ");
    if( mc->vi->iuvd != NULL )
    { PRINTDSTRING(mc->vi->iuvd->implementationUse.lvInformation.logicalVolumeIdentifier,
            sizeof(mc->vi->iuvd->implementationUse.lvInformation.logicalVolumeIdentifier),
                    "\n");
      uLen_iuvd = uncompressDstring(LogVolId_SIZE,
                   mc->vi->iuvd->implementationUse.lvInformation.logicalVolumeIdentifier,
                   uNameLogVolId_iuvd);
    }
    else
    { VERBOSE00(uctout, "<IUVD missing>\n");
    }

    VERBOSE00(uctout,   "\t    FSD: Logical Volume Identifier [128]: ");
    if( mc->fsd != NULL )
    { PRINTDSTRING( mc->fsd->logicalVolumeIdentifier,
             sizeof(mc->fsd->logicalVolumeIdentifier), "\n");
      uLen_fsd = uncompressDstring(LogVolId_SIZE,
                    mc->fsd->logicalVolumeIdentifier,
                    uNameLogVolId_fsd);
    }
    else
    { VERBOSE00(uctout, "< FSD missing>\n");
    }

    if( expectVAT2 )
    { VERBOSE00(uctout, "\t   VAT2: Logical Volume Identifier [128]: ");
      if( vat2Head != NULL )
      { PRINTDSTRING( vat2Head->logicalVolumeIdentifier,
               sizeof(vat2Head->logicalVolumeIdentifier), "\n");
      uLen_vat2 = uncompressDstring(LogVolId_SIZE,
                      vat2Head->logicalVolumeIdentifier,
                      uNameLogVolId_vat2);
      }
      else
      { VERBOSE00(uctout, "<VAT2 missing>\n");
      }
    }
    expectVAT2ButUndefinedOrEmpty =
                (expectVAT2 && uLen_vat2 <= 0);

    /* File Set Identifier      : FSD
     */
    VERBOSE00(uctout,   "\t    FSD:       File Set Identifier  [32]: ");
    if( mc->fsd != NULL )
    { PRINTDSTRING( mc->fsd->fileSetIdentifier,
             sizeof(mc->fsd->fileSetIdentifier), "\n");
    }
    else
    { VERBOSE00(uctout, "< FSD missing>\n");
    }

    /* Check Logical Volume Identification (LVI) relations.
     * Compare all to reference value, initially LVD LVI.
     * isEqualLogVolIds() only returns TRUE
     * IFF both uLen_XXX values are > 0 (LVI defined and not empty)
     * AND LVI unicode arrays are identical.
     */
    uNameLogVolId_ref = uNameLogVolId_lvd;
    uLen_ref = uLen_lvd;

    /* compare IUVD LVI with 'ref', maybe after compare
     * change 'ref' value to IUVD LVI
     */
    isEq_iuvd = isEqualLogVolIds(uNameLogVolId_iuvd, uLen_iuvd,
                                 uNameLogVolId_ref, uLen_ref);
    if( uLen_iuvd > 0 && uLen_ref <= 0 )
    { uNameLogVolId_ref = uNameLogVolId_iuvd;
      uLen_ref = uLen_iuvd;
    }

    /* compare FSD LVI with 'ref', maybe after compare
     * change 'ref' value to FSD LVI
     */
    isEq_fsd = isEqualLogVolIds(uNameLogVolId_fsd, uLen_fsd,
                                uNameLogVolId_ref, uLen_ref);
    if( uLen_fsd > 0 && uLen_ref <= 0 )
    { uNameLogVolId_ref = uNameLogVolId_fsd;
      uLen_ref = uLen_fsd;
    }

    /* compare VAT2 LVI with 'ref' (if any)
     */
    isEq_vat2 = isEqualLogVolIds(uNameLogVolId_vat2, uLen_vat2,
                                 uNameLogVolId_ref, uLen_ref);

    /* Print error if at least one LVI is undefined, empty or unequal
     * If only VAT2 LVI is unequal,
     * then print 'renamed by user' note later.
     */
    if(     uLen_lvd  <= 0  /*  LVD undefined or empty */
       || (!isEq_iuvd)      /* IUVD undefined, empty or not equal */
       || (!isEq_fsd)       /*  FSD undefined, empty or not equal */
       ||  expectVAT2ButUndefinedOrEmpty )
    {
      if( !cntErrWarn ) uctGlobalErrorOccurrences--;  /* no count */
      MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
          "\n\tError: Logical Volume Identifier undefined"
                                            " or different\n"
           "-\t       for LVD, IUVD%s,%s%s\n"
           "-\t       %s%sUDF 2., ECMA 3/8.8, 3/10.6.4, 1/7.2.12.\n",
            (expectVAT2ButUndefinedOrEmpty)
                         ? ", FSD or VAT 2.xx" : " or FSD",
            (!isEq_iuvd) ? " UDF 2.2.7.2.2,"   : "",
            (!isEq_fsd)  ? " ECMA 4/14.1.10,"  : "",
            (expectVAT2ButUndefinedOrEmpty)
                         ? UDFREF_VAT(getUctUdfRevision()) : "",
            (expectVAT2ButUndefinedOrEmpty) ? ", " : "" );
      MLIMITend;
    }

    /* 'LVI renamed by user' note if VAT2 LVI non-empty but unequal.
     */
    if( vat2Head != NULL && uLen_vat2 > 0 && !isEq_vat2 )
    {
      MLIMITbegin(INFO01level,uctMessageLimit);
        fprintf(uctout,
          "\n\tNote: Logical Volume Identifier different for VAT 2.xx,\n"
           "-\t      Logical Volume renamed by user to: ");
        PRINTDSTRING( vat2Head->logicalVolumeIdentifier,
               sizeof(vat2Head->logicalVolumeIdentifier), ",\n");
        fprintf(uctout,
           "-\t      %s.\n", UDFREF_VAT(getUctUdfRevision()));
      MLIMITend;
    }

    cntErrWarn = FALSE; /* no global occurrences count next calls */
}

