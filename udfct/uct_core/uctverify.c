/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctverify.c
 *
 * Description : Check and process functions
 *               for several UDF structures.
 *
 * History     :           Gerrit Scholl, creation
 *             : 20000922: Alex Sinitsyn, Timestamps
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mytypes.h"
#include "general.h"
#include "udfstruct.h"
#include "ucttimestamp.h"
#include "uctgeneral.h"
#include "uctendian.h"
#include "uctdata.h"
#include "uctstatus.h"
#include "ucterror.h"
#include "uctfiles.h"
#include "uctallocation.h"
#include "uctnodes.h"
#include "uctverify.h"


/* verifyFileIdentifierDescriptor() global flag
 */
bool globalFidIdentifierError = FALSE;

/* one-time asserts to check PACKED structures.
 */
extern void udfStructStaticAsserts()    /* no return on failure */
{
    /* static general asserts and for the benefit of getLengthOfDescriptor()
     */
    UCTASSERT( sizeof(Uint8) == 1);
    UCTASSERT( sizeof(Uint16) == 2);
    UCTASSERT( sizeof(Uint32) == 4);
    UCTASSERT( sizeof(Uint64) == 8);
    UCTASSERT( offsetof(SparingTable,
                        startOfMapEntries) == 56);
    UCTASSERT( sizeof(PrimaryVolumeDescriptor) == 512);
    UCTASSERT( sizeof(AnchorVolumeDescriptorPointer) == 512);
    UCTASSERT( sizeof(VolumeDescriptorPointer) == 512);
    UCTASSERT( sizeof(ImplementationUseVolumeDescriptor) == 512);
    UCTASSERT( sizeof(PartitionDescriptor) == 512);
    UCTASSERT( offsetof(LogicalVolumeDescriptor,
                        startOfPartitionMaps) == 440);
    UCTASSERT( offsetof(UnallocatedSpaceDescriptor,
                        startOfAllocationDescriptors) == 24 );
    UCTASSERT( sizeof(TerminatingDescriptor) == 512);
    UCTASSERT( offsetof(LogicalVolumeIntegrityDescriptor,
                        startOfTables) == 80);
    UCTASSERT( sizeof(FileSetDescriptor) == 512);
    UCTASSERT( offsetof(FileIdentifierDescriptor,
                        startOfImplementationUse) == 38 );
    UCTASSERT( offsetof(AllocationExtentDescriptor,
                        startOfAllocationDescriptors) == 24 );
    UCTASSERT( sizeof(IndirectEntry) == 52);
    UCTASSERT( sizeof(TerminalEntry) == 36);
    UCTASSERT( offsetof(FileEntry,
                        startOfExtendedAttributes) == 176);
    UCTASSERT( offsetof(ExtendedFileEntry,
                        startOfExtendedAttributes) == 216);
    UCTASSERT( sizeof(ExtendedAttributeHeaderDescriptor) == 24);
    UCTASSERT( offsetof(UnallocatedSpaceEntry,
                        startOfAllocationDescriptors) == 40);
    UCTASSERT( offsetof(SpaceBitmapDescriptor,
                        startOfBitmap) == 24);
    UCTASSERT( sizeof(EntityID) == 32 );

    UCTASSERT( sizeof(Charspec) == 64);
    UCTASSERT( sizeof(Dchars) == 1);
    UCTASSERT( sizeof(Dstring) == 1);
    UCTASSERT( sizeof(Timestamp) == 12);
    UCTASSERT( sizeof(DomainIdentifierSuffix) == 8);
    UCTASSERT( sizeof(UdfIdentifierSuffix) == 8);
    UCTASSERT( sizeof(ImplementationIdentifierSuffix) == 8);
    UCTASSERT( sizeof(ApplicationIdentifierSuffix) == 8);
    UCTASSERT( sizeof(BootDescriptorTail) == 2041 );
    UCTASSERT( sizeof(GenericVolumeStructureDescriptor) == 2048 );
    UCTASSERT( sizeof(ExtentAd) == 8 );
    UCTASSERT( sizeof(LBAddr) == 6);
    UCTASSERT( sizeof(ADImpUse) == 6);
    UCTASSERT( sizeof(AnyAd) == 4);
    UCTASSERT( sizeof(ShortAd) == 8 );
    UCTASSERT( sizeof(LongAd) == 16 );
    UCTASSERT( sizeof(ExtAd) == 20 );
    UCTASSERT( sizeof(Tag) == 16 );
    UCTASSERT( sizeof(PartitionHeaderDescriptor) == 128);
    UCTASSERT( offsetof(GenericPartitionMap,
                        startOfPartitionMapping) == 2 );
    UCTASSERT( sizeof(Type1PartitionMap) == 6);
    UCTASSERT( sizeof(Type2PartitionMap) == 64);
    UCTASSERT( sizeof(MetadataPartitionMapTail) == 24);
    UCTASSERT( offsetof(SparablePartitionMapTail,
                        startOfLocationsOfSparingTables) == 8);
    UCTASSERT( sizeof(LogicalVolumeHeaderDescriptor) == 32);
    UCTASSERT( offsetof(LvidImplementationUse,
                        startofImplementationUse) == 46);
    UCTASSERT( sizeof(LVInformation) == 460);
    UCTASSERT( sizeof(SparingEntry) == 8);
    UCTASSERT( sizeof(VAT150Tail) == 36);
    UCTASSERT( offsetof(VAT200Head,
                        startOfImplementationUse) == 152);
    UCTASSERT( sizeof(ICBTag) == 20);
    UCTASSERT( sizeof(EAGenericHead) == 12);
    UCTASSERT( offsetof(CharSetInformationExtendedAttribute,
                        startOfEscapeSequences) == 17);
    UCTASSERT( offsetof(FileTimesExtendedAttribute,
                        startOfFileTimes) == 20);
    UCTASSERT( offsetof(InformationTimesExtendedAttribute,
                        startOfInformationTimes) == 20);
    UCTASSERT( sizeof(DeviceSpecificationExtendedAttribute) == 56);
    UCTASSERT( sizeof(ImplementationUseExtendedAttribute) == 50);
    UCTASSERT( sizeof(ApplicationUseExtendedAttribute) == 50);
    UCTASSERT( offsetof(PathComponent,
                        startOfComponentIdentifier) == 4);
}

/* Test block of bytes in a descriptor that shall
 * all have the value #00.
 *
 * Input arguments :
 * beginDescr: pointer to begin of descriptor.
 * beginZeros: pointer to begin of zero bytes.
 * nBytes    : nmb of bytes to be checked.
 * extraName : e.g. for BOOT2 which is no tag descriptor
 *
 * beginDescr and extraName for error message only.
 * extraName may be NULL.
 *
 * implementation note:
 *  MLIMIT... is not used here because it is assumed that
 *  verifyZerosInDescriptor() is called from within a
 *  MLIMITbegin(ERROR00level, ...); ... MLIMTend clause.
 */
static bool verifyZerosInDescriptor(Byte *beginDescr, Byte *beginZeros,
                                    size_t nBytes, char *extraName)
{
    Byte   value;
    size_t cnt;
    Uint32 first;

    if( !verifyZeros(beginZeros, nBytes, &cnt, &first, &value) )
    {   size_t displ = beginZeros - beginDescr;
        ifVERBOSE(VERBOSE00level)       /* NO MLIMITbegin, see above */
        { printMessageHead(VERBOSE00level, beginDescr, beginZeros, extraName);
          if( extraName != NULL ) fprintf(uctout, " " );
          fprintf(uctout,
             "Error: %lu non-zero byte%s found in a %lu byte%s blank area\n"
            "-\t\t      begin area at RBP %lu\n"
                  "-\t\t\tend area at RBP %lu\n"
            "-\t\t first violation at RBP %lu, value #%02X\n",
                cnt, PLURAL_S(cnt), nBytes, PLURAL_S(nBytes),
                displ, displ+nBytes-1, displ+first, value);
        }
        ENDif;
        return FALSE;
    }
    return TRUE;
}


/* ECMA Part 2: Volume and boot block recognition ****************************
 */

typedef struct      /* ECMA 2/9.1 etc. */
{
    VsdType  type;
    Byte    *string;    /* \0 terminated StandardIdentifier string */
} VsdTable;

static VsdTable vsdTable[] =
{
    { vsdTYPE_BEA01, (Byte*)VRS_BEA01 },
    { vsdTYPE_BOOT2, (Byte*)VRS_BOOT2 },
    { vsdTYPE_CD001, (Byte*)VRS_CD001 },
    { vsdTYPE_CDW02, (Byte*)VRS_CDW02 },
    { vsdTYPE_NSR02, (Byte*)VRS_NSR02 },
    { vsdTYPE_NSR03, (Byte*)VRS_NSR03 },
    { vsdTYPE_TEA01, (Byte*)VRS_TEA01 }
};

/* BOOT2 descriptor tail, ECMA 2/9.4
 * Tail is starting with first Reserved (Reserved01) field.
 * Boot descriptor not (yet) defined in UDF, UDF 5.3.
 */
static bool swapAndVerifyBoot2Tail(GenericVolumeStructureDescriptor *vsd)
{
    BootDescriptorTail *bt = &vsd->VSDtail.BootTail;
    bool  result = TRUE;
    char *dInfo  = "Boot Descriptor. ECMA 2/9.4";

    /* Swap first
     */
    endianSwapBoot2Tail(bt, (Byte*)vsd);

    /* start verify from Reserved01 field
     * TODO: verify/show EntityId  ArchitectureType     (regid in ECMA)
     * TODO: verify/show EntityId  BootIdentifier       (regid in ECMA)
     * TODO: verify/show Uint32    BootExtentLocation
     * TODO: verify/show Uint32    BootExtentLength
     * TODO: verify/show Uint64    LoadAddress
     * TODO: verify/show Uint64    StartAddress
     * TODO: verify/show Timestamp DescriptorCreationDateAndTime
     * TODO: verify/show Uint16    Flags
     * No    verify/show Byte      BootUse[1906]
     */

    /* Reserved01
     */
    if( bt->Reserved01 !=  0x00 )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout, "\tError: first Reserved field: %lu, expected: 0\n"
                       "-\t       in %s\n", bt->Reserved01, dInfo);
      MLIMITend;
      result = FALSE;
    }

    /* Flags
     */
    if( (bt->Flags & 0xFFFE) != 0x0000 )        /* bit 1-15 reserved */
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout, "\tError: Flags, reserved bits 1-15 not ZER0: 0x%04X\n"
                       "-\t       in %s\n", bt->Flags, dInfo);
      MLIMITend;
      result = FALSE;
    }

    /* reserved02,
     * Note that verifyZerosInDescriptor() assumes to be
     * called from within a MLIMITbegin(ERROR00level, ...)
     * ... MLIMTend clause.
     * TODO: test this message, it should not show <tag Id text4>
     */
    if( !verifyZeros( (Byte *)&bt->Reserved02, sizeof(bt->Reserved02),
                      NULL, NULL, NULL) )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          (void) verifyZerosInDescriptor((Byte *)vsd,
                                         (Byte *)&bt->Reserved02,
                                           sizeof(bt->Reserved02),
                                           "BOOT2");
          fprintf(uctout, "-\t       in %s\n", dInfo);
        MLIMITend;
        result = FALSE;
    }
    return result;

}   /* end swapAndVerifyBoot2Tail() */

/* Check a Volume Structure Descriptor in a Volume Recognition Sequence.
 *
 * Return value : Volume Structure Descriptor type.
 */
extern VsdType verifyVolRecVSD(GenericVolumeStructureDescriptor *vsd)
{
    VsdType  type = vsdTYPE_UNKNOWN;
    int      i;
    char    *dInfo = "Volume Structure Descriptor. ECMA 2/9. or 3/9.1";
    char    *typeStr = "";

    for( i = 0; i < (sizeof(vsdTable)/sizeof(vsdTable[0])); i++ )
    {
        if( memcmp(vsd->StandardIdentifier, vsdTable[i].string, 5) == 0)
        {
            type = vsdTable[i].type;
            typeStr = (char*)vsdTable[i].string;
            VERBOSE00(uctout, "\t%s", typeStr);

            /* for CD001, oprint ISO 9660 VSD type
             */
            if( type == vsdTYPE_CD001 )
            {   char *txt;
                /* ISO 9660 (== ECMA-119, 2nd edition)
                 * see ECMA-119 2nd edition 8.1.1
                 */
                VERBOSE00(uctout, "\t    - ISO 9660 ");
                switch( vsd->StructureType )
                {
                case 0:     txt = "Boot Record";
                            break;
                case 1:     txt = "Primary Volume Descriptor";
                            break;
                case 2:     txt = "Supplementary Volume Descriptor";
                            break;
                case 3:     txt = "Volume Partition Descriptor";
                            break;
                case 255:   txt = "Volume Descriptor Set Terminator";
                            break;
                default:    txt = NULL;
                            break;
                }
                if( txt != NULL )
                     VERBOSE00(uctout, "%s", txt);
                else VERBOSE00(uctout, "volume descriptor type: %u",
                                            vsd->StructureType);
            }
            VERBOSE00(uctout, "\n");
            /* no further tests for
             * vsdTYPE_CD001 and vsdTYPE_CDW02
             */
            if(    type != vsdTYPE_CD001
                && type != vsdTYPE_CDW02 )
            {
                if( vsd->StructureType != 0 )
                {
                    VERBOSE00(uctout,
                         "\tError: Structure Type   : %lu, expected: 0\n"
                        "-\t       in %s %s\n",
                            vsd->StructureType, typeStr, dInfo);
                }
                if( vsd->StructureVersion != 1 )
                {
                    VERBOSE00(uctout,
                         "\tError: Structure Version: %lu, expected: 1\n"
                        "-\t       in %s %s\n",
                            vsd->StructureVersion, typeStr, dInfo);
                }
                if( type == vsdTYPE_BOOT2 )
                {
                    (void) swapAndVerifyBoot2Tail(vsd);
                }
                else if( !verifyZeros((Byte *) &vsd->VSDtail.StructureData,
                                       sizeof(vsd->VSDtail.StructureData),
                                       NULL, NULL, NULL) )
                { /* NOTE:
                   *    StructureData[2041] as defined in ECMA 2/9.1 is
                   *    the same as the "Reserved" + "StructureData[2040]"
                   *    fields in the NSR descriptor as defined by ECMA 3/9.1
                   *
                   * Note that verifyZerosInDescriptor() assumes to be
                   * called from within a MLIMITbegin(ERROR00level, ...)
                   * ... MLIMTend clause.
                   */
                    MLIMITbegin(ERROR00level,uctMessageLimit);
                      (void) verifyZerosInDescriptor((Byte *)  vsd,
                                    (Byte *) &vsd->VSDtail.StructureData,
                                       sizeof(vsd->VSDtail.StructureData), NULL);
                      fprintf(uctout,"-\t       in %s %s\n", typeStr, dInfo);
                    MLIMITend;
                }
            }
            break;      /* type found */
        }
    }
    return type;

}   /* end verifyVolRecVSD() */


extern Uint16 calculateCrc(Byte *src, Uint16 numBytes)
{
    return chunk_cksum(src, numBytes, 0);
}

extern Uint8 calculateTagChecksum(Tag *gt)
{
    Byte        *b;
    Uint8       sum=0;
    Uint8       counter;

    b = (Byte*) gt;
    for(counter = 0; counter <= 3; counter++)
        sum = (Uint8) ((sum + b[counter]) % 256);
    for(counter = 5; counter <= 15; counter++)
        sum = (Uint8) ((sum + b[counter]) % 256);
    return sum;
}


/* verify for all approriate UDF structures ***********************
 */

/* verifyCharSetList: ECMA 1/7.2.11 !!
 * All Character Set List (Uint32) definitions shall
 * define CS0 only, UDF 2.2.2.3-4 and 2.3.2.3-4.
 * This is in conflict with ECMA 3/13.1 and 4/17.1 which say that
 * bit for CS2 (bit 2) shall be ONE.
 * We'll stick to the UDF rule, so bit 2 is ZERO.
 */
static bool verifyCharSetList( Uint32 *chSetList, Byte *d,
                               bool    isMaxCharSetList,
                               char   *udfRef )
{
    char *maxText = (isMaxCharSetList) ? "Maximum " : "";

    if( *(chSetList) != 1 )     /* only CS0: bit 0 set */
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageHead(MLIMIT.vl,
              (Byte*)d, (Byte*)chSetList, NULL);
          fprintf(uctout,
            "%sCharacter Set List error: #%04X, expected: #0001\n"
            "-\t\t CS0 only, UDF %s, ECMA 4/7.2.11.\n",
            maxText, *(chSetList), udfRef);
        MLIMITend;
        return FALSE;
    }

    return TRUE;
}

/* verifyCharspecCS0:
 * All Charspec fields shall define OSTA CS0 as defined
 * in UDF 2.1.2.
 */
static bool verifyCharspecCS0(Charspec *ch, Byte *d,
                              char *fieldName, char *udfRef)
{
    if(   ch->characterSetType != OSTA_CS0_CHARSETTYPE
       || memcmp(ch->characterSetInfo, OSTA_CS0_CHARSETINFO,
                 sizeof(ch->characterSetInfo)) != 0 )
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageHead(MLIMIT.vl, (Byte*) d, (Byte*) ch, NULL);
          fprintf(uctout,
                     "Charspec error: %s\n"
               "-\t\t  Type and Info: %3u, ", fieldName,
                         ch->characterSetType);
          printBytesName(ch->characterSetInfo,
                  sizeof(ch->characterSetInfo), FALSE, MLIMIT.vl);
          fprintf(uctout,
            ",\n-\t\t       expected: %3u, ", OSTA_CS0_CHARSETTYPE);
          printBytesName((Byte*) OSTA_CS0_CHARSETINFO,
                  sizeof(ch->characterSetInfo), FALSE, MLIMIT.vl);
          fprintf(uctout,
            ",\n-\t\t  (Info trailing #00 chars not printed),"
                                    " UDF 2.1.2, %s.\n", udfRef);
        MLIMITend;
        return FALSE;
    }

    return TRUE;
}

/* verifyDchars:    ECMA 1/7.2, UDF 2.1.1
 * (dCharsSize == 0) will be handled as an error, so
 * do not call for parent FID or undefined Dstring.
 *
 * return value:
 *  if any error found
 *  then FALSE
 *  else TRUE
 *
 * implementation note: It is very important that
 * the correct return value is returned here !!
 */
static bool verifyDchars( Dchars *dChars, Byte *d,
                          Uint32  dCharsSize,
                          bool    isDeletedFid,
                          char   *fieldRefTxt )
{   Uint32   n;
    Uint8    comprId;
    bool     result = TRUE,
             decompress16 = FALSE;
    char    *generalRefTxt;

    generalRefTxt =
        (getUctMinUdfRevision() <= 0x200) ? "ECMA 1/7.2, UDF 2.1.1"
      : (getUctMinUdfRevision() >= 0x260) ? "ECMA 1/7.2, UDF 2.1.1, 2.3.4.2.1"
                                          : "ECMA 1/7.2, UDF 2.1.1, 2.3.4.2";

    /* dCharsSize == 0: illegal (reserved for parent FID)
     * dCharsSize == 1: illegal (empty string)
     */
    if( dCharsSize < 2 )    /* illegal */
    {   MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead( MLIMIT.vl, d, dChars, NULL);
            fprintf(uctout,
              "Error: D-characters size less than 2: %u, %s.\n"
              "-\t\t\tEmpty string or no room for compression ID,\n"
              "-\t\t\t%s.\n",
                dCharsSize, fieldRefTxt, generalRefTxt);
        MLIMITend;
        if( dCharsSize == 0 )
        {   return FALSE;
        }
        result = FALSE;     /* dCharsSize > 0 */
    }
    comprId = dChars[0];

    /* check compression ID, UDF 2.1.1[, 2.3.4.2[.1]].
     * - Compression IDs 256 and 255 are allowed for deleted
     *   FIDs only from UDF release 2.00+.
     * - For UDF 2.00, the identifier length is at least 5 for
     *   compression ID 254 and 9 for compression ID 255.
     * - For UDF 2.01+ it is defined that 254 has the same
     *   compression algorithm as 8 and 255 the same as 16.
     *   From verifier release 1.5r6: For Deleted FIDs, 8 and 16 are not allowed!!
     */
    if(       comprId == 16
       || (   comprId == 255 && getUctMinUdfRevision() >= 0x201) )    /* UDF 2.01+ */
    {   decompress16 = TRUE;
    }
    if( isDeletedFid )
    {   MLIMITbegin(INFO01level, MLIMITdefault01);
          fprintf(uctout,
            "\tNote: deleted FID, compression ID %lu\n", comprId);
        MLIMITend;
    }
    if(   getUctUdfRevision() == 0x200        /* UDF 2.00 */
       && (   (comprId == 254 && dCharsSize <= 4)
           || (comprId == 255 && dCharsSize <= 8) ) )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        printMessageHead(MLIMIT.vl, d, dChars, NULL);
        fprintf(uctout,
          "Error: D-characters size: %lu, expected: at least %lu.\n"
          "-\t\t For UDF 2.00, compression ID %u shall be followed\n"
          "-\t\t by at least %u bytes, UDF 2.00 2.1.1.\n",
            dCharsSize, (comprId == 254) ? 5 : 9,
            comprId,    (comprId == 254) ? 4 : 8);
      MLIMITend;
      result = FALSE;
    }

    if(   (   ( comprId == 8 || comprId == 16 )
#ifdef CID8and16illegalForDeletedFIdsForUDF201plus
           && ( getUctUdfRevision() <= 0x200 || !isDeletedFid )
#endif   /** CID8and16illegalForDeletedFIdsForUDF201plus **/
          )
       || (   ( comprId == 254 || comprId == 255 )
           && getUctUdfRevision() >= 0x200 && isDeletedFid ) )
    {                       /* compression ID ok, no action */
    }
    else                    /* illegal compression ID */
    {   MLIMITbegin( ERROR00level, uctMessageLimit );
          printMessageHead( MLIMIT.vl, d, dChars, NULL);
          fprintf(uctout,
            "Error: Illegal OSTA CS0 compression ID %lu,\n"
            "-\t\t\t%s, %s.\n",
                comprId, fieldRefTxt, generalRefTxt);
          if( comprId == 254 || comprId == 255 )
          { fprintf(uctout,
              "-\t\t  Note: Compression ID %lu is only allowed for a\n"
                    "-\t\t\tdeleted FID and UDF release 2.00 or higher.\n",
                    comprId);
          }
        MLIMITend;
        result = FALSE;
    }

    /* For 2-byte compression, check odd dCharsSize value
     * and illegal FEFF and FFFE chars.
     */
    if( decompress16 )  /* test odd size and special characters */
    {   unicode_t uch = 0x0000;
        if( (dCharsSize % 2) == 0 ) /* dCharsSize no odd number */
        { MLIMITbegin(ERROR00level, uctMessageLimit );
            printMessageHead(MLIMIT.vl, d, dChars, NULL);
            fprintf(uctout,
              "Error: Length of d-characters: %lu.\n"
              "-\t\t\tShall be odd number for compression ID %u,\n"
              "-\t\t\t%s, %s.\n",
              dCharsSize, comprId, fieldRefTxt, generalRefTxt);
          MLIMITend;
          result = FALSE;
        }
        /* test FEFF, FEFE, mind Dchars big endian order and
         * last character in case dCharsSize is no odd number
         */
        for( n = 1; (n+1) < dCharsSize; n += 2 )
        {   /* mind Dchars big endian order
             */
            uch = (unicode_t) ((dChars[n] << 8) | dChars[n+1]);
            if( uch == 0xFEFF || uch == 0xFFFE )
            {   break;          /* illegal char, UDF 2.1.1 */
            }
        }
        if( (n+1) < dCharsSize )    /* FEFF or FFFE found */
        {   MLIMITbegin( ERROR00level, uctMessageLimit );
              printMessageHead(MLIMIT.vl, d, dChars, NULL);
              fprintf(uctout,
                "Error: Illegal Unicode character #%04X for\n"
                "-\t\t\td-characters with OSTA CS0 compression ID %u\n"
                "-\t\t\ton byte position %u,\n"
                "-\t\t\t%s, %s.\n",
                uch, comprId, n, fieldRefTxt, generalRefTxt);
            MLIMITend;
            result = FALSE;
        }
    }
    return result;
}

/* verifyDstring:   ECMA 1/7.2.12, UDF 2.1.3, 2.1.1.
 * precondition: dstr != NULL && dstrSize > 0
 *
 * verifyDstring() will use verifyDchars() for d-charater
 * tests, so only the Dstring specific tests here.
 * One exception: different messages for
 *     dstring 'last byte < 2'
 * compared to 'Dchars size < 2' cases.
 */
static bool verifyDstring( Dstring *dstr, Byte *d,
                           Uint32   dstrSize,
                           bool     isObligatory,
                           char    *fieldRefTxt )
{
    Uint8  comprId, dCharsSize;
    bool   result = TRUE;
    char  *generalRefTxt = "ECMA 1/7.2.12, UDF 2.1.3, 2.1.1";

    /* Dstrings have fixed size higher than 2
     */
    UCTASSERT( dstr != NULL && dstrSize > 2 );

    /* First check if all blank, or empty dstring.
     * If dstring value is obligatory, this is illegal.
     */
    if( verifyZeros(dstr, dstrSize, NULL, NULL, NULL) )
    {   /* all blanks, undefined Dstring
         */
        if( isObligatory )      /* dstring shall be defined */
        { MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead( MLIMIT.vl, d, dstr, NULL);
            fprintf(uctout,
              "Error: Undefined/empty Dstring, %s.\n"
              "-\t\t\tUnless otherwise specified,"
                            " a dstring shall not\n"
              "-\t\t\tbe all #00 bytes, %s.\n",
                fieldRefTxt, generalRefTxt);
          MLIMITend;
          result = FALSE;
        }
        return result;  /* all blanks, maybe legal undefined Dstring */
    }
    comprId    = dstr[0];
    dCharsSize = dstr[dstrSize-1];

    if(   dCharsSize >= dstrSize    /* compression ID included */
       || dCharsSize <= 1 )         /* illegal */
    {
      MLIMITbegin( ERROR00level, uctMessageLimit );
        printMessageHead( MLIMIT.vl, d, dstr, NULL);
        fprintf(uctout,
                "Error: Inconsistent %sDstring, %s.\n"
          "-\t\t Dstring size    : %3u\n"
          "-\t\t first byte value: %3u  (OSTA CS0 compression ID)\n"
          "-\t\t  last byte value: %3u"
                                "  (d-chars size, compr ID incl)\n"
          "-\t\t Expected for last byte value:"
                                " at least 2, at most %u.\n"
          "-\t\t %s%s.\n",
            (dCharsSize > 1) ? "non-blank " : "",
             fieldRefTxt, dstrSize, comprId, dCharsSize,
            (dstrSize - 1),
            (dCharsSize > 1) ? ""
              : "A zero length string shall be recorded by a Dstring\n"
          "-\t\t with all zeros bytes, ", generalRefTxt);
      MLIMITend;
      return FALSE;             /* no further checks */
    }
    UCTASSERT( dCharsSize >= 2 && dCharsSize < dstrSize );

    /* now verify d-characters
     */
    if( !verifyDchars( dstr, d, dCharsSize, FALSE, /* NO deletedFid */
                       fieldRefTxt ) )
    {   result = FALSE;
    }

    /* unused rest shall be all blanks
     * verifyZeros() is silent, but
     * verifyZerosInDescriptor() will print the error text.
     */
    if( !verifyZeros(dstr + dCharsSize, dstrSize - 1 - dCharsSize,
                     NULL, NULL, NULL) )
    {
        MLIMITbegin( ERROR00level, uctMessageLimit );
        (void) verifyZerosInDescriptor( d, dstr + dCharsSize,
                                        dstrSize - 1 - dCharsSize,
                                        NULL );
          fprintf(uctout,
            "-\t\t Dstring remaining bytes shall be #00, %s,\n"
            "-\t\t %s.\n", fieldRefTxt, generalRefTxt);
        MLIMITend;
        result = FALSE;     /* not fatal */
    }

    return result;
}

/* verifyTimestampRelation:
 * Print error message in case the Date and Time in
 * t2 is later than the one in t1.
 * txt1 and txt2 hold a short text for t1 and t2
 * respectively, e.g. "Creation" for a Creation Date and Time.
 * txt2 maybe NULL. In this case a default message will be printed.
 * ruleRef is the primary ECMA and/or UDF rule reference.
 * As a secondary UDF reference, always ", UDF 3.1.1" will
 * be added. It is about the lower-than-second fields which
 * often cause error messages in verifyTimestampRelation().
 * pField is the 'printMessageHead begin-of-field' pointer in d.
 * if pField == NULL, it will be replaced by the
 * default: (Byte*) t1.
 * Note that if d != NULL, the final value of pField must be
 * a pointer in the descriptor d.
 *
 *Mind return value:
 *  if comparison was completed and a time comparison error
 *     was found (maybe 'shall not be later' message printed).
 *  then: FALSE
 *  else: TRUE.
 *
 * TODO: Remove 3.1.1 ref below ??
 */
static bool verifyTimestampRelation(Timestamp *t1, char *txt1,
                                    Timestamp *t2, char *txt2,
                                    bool       isError,
                                    char      *ruleRef,
                                    Byte      *d,  Byte *pField,
                              UdfMountContext *mc, Node *node)
{   char *txt2b;
    int   cmpResult;        /* signed */

    if(   compareTimestamps(t1, t2, &cmpResult)
       && cmpResult < 0 )
    {   Uint16 udfRev = getUctUdfRevision();
        /* print message, first determine defaults
         * and format for proper allignment
         */
        if( pField == NULL )
        {   pField = (Byte*) t1;
        }

        if( txt2 == NULL )      /* get default */
        {   txt2  = "any Timestamp";
            txt2b = "Timestamp";
        }
        else
        {   txt2b = txt2;
        }
        /* 020711 changed: "shall not be earlier"
         *             to: "shall not be later" message,
         * so reversed t1 and t2 arguments as well.
         */
        MLIMITbegin(isError ? ERROR00level
                            : WARN01level, MLIMITdefault10);
          printMessageHead( MLIMIT.vl, d, pField, NULL);
          fprintf(uctout,
                  "%s: %s %s not be later than\n"
            "-\t\t %s, %s%s%sUDF 3.1.1.\n",
            (isError) ? "Error" : "Warning", txt2,
            (isError) ? "shall" : "should", txt1,
                (!isError && udfRev <= 0x260) ? "DCN-5153, "
              : (!isError && udfRev > 0x260) ? "UDF 2.3.6.9, "
                                            : "",
            (ruleRef != NULL) ? ruleRef : "",
            (ruleRef != NULL) ? ", "    : "");
          printFormattedTimestamps(t2, txt2b, t1, txt1,
                                   "-\t\t ", MLIMIT.vl);
          if(   GET_TIMEZONE(t1) == TIMEZONE_UNDEFINED
             || GET_TIMEZONE(t2) == TIMEZONE_UNDEFINED )
          { fprintf(uctout, "-\tNote: See final status report note"
              " on how undefined Timezones are compared.\n");
          }
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        return FALSE; /* compare completed and comparison error found */
    }
    return TRUE;     /* times ok or comparison could not be completed */

}   /* end verifyTimestampRelation() */


/* verifyTimestampLVIDorVAT:
 * verify if timestamp is later than LVID (or VAT) timestamp.
 *
 * Mind return value:
 *  if comparison was completed and a time comparison error
 *     was found (maybe 'shall not be later' message printed).
 *  then: FALSE
 *  else: TRUE.
 */
extern bool verifyTimestampLVIDorVAT(Timestamp *t2, char *txt2, Byte *d,
                                     UdfMountContext *mc, Node *node)
{   Timestamp *t1;
    char      *txt1, *ref1;

    if( mc->vatNode != NULL && mc->vatNode->fe != NULL )
    {   t1   = pFE_modificationTime(mc->vatNode->fe);
        txt1 = "VAT Modification Time";
        ref1 = UDFREF_VAT(getUctUdfRevision());
    }
    else if( mc->lvid != NULL )
    {   t1   = &mc->lvid->recordingDateAndTime;
        txt1 = "LVID Recording Time";
        ref1 = "ECMA 3/10.10.2, UDF 2.2.6";
    }
    else
    {   return TRUE;    /* no VAT or LVID found */
    }

    if( !isValidTimestamp(t1) || !isValidTimestamp(t2) )
    {   return TRUE;    /* could not compare */
    }
    /* check: t2 shall not be later than t1
     */
    return verifyTimestampRelation(
                    t1, txt1, t2, txt2,
                    TRUE, ref1, d,      /* isError */
                    (Byte*)t2, mc, node);

}   /* end verifyTimestampLVIDorVAT() */


/* verifyTimestamp01: using isValidTimestamp().
 * ECMA 1/7.3, 4/14.9.12-14
 * UDF 2., 2.1.4
 *
 * If obligatoryRefTxt == NULL
 * then: an unspecified timestamp (all zero bytes) is allowed,
 * else: not allowed and obligatoryRefTxt will point to the section
 *       in the ECMA or UDF spec where this is stated, e.g.
 *       "ECMA 4/14.9.12" for Access Date and Time field in File Entry.
 */
static bool verifyTimestamp01(Timestamp *t, char *obligatoryRefTxt,
                              Byte *d, UdfMountContext *mc,
                              Node *node)
{
  Uint8 type      = GET_TIMETYPE(t);
  Int16 timezone  = GET_TIMEZONE(t);
  bool  typeError = (type != 1),
        zoneError = (   timezone != TIMEZONE_UNDEFINED
                     && abs(timezone) > 1440);

  /* extra warning if correct timezone has offset
   * from UTC that is not a multiple of 15 minutes.
   */
  if(    timezone != TIMEZONE_UNDEFINED
     && !zoneError
     && (timezone % 15) != 0 )
  { MLIMITbegin(WARN01level, MLIMITdefault01);
      printMessageHead( MLIMIT.vl, d, (Byte*)t, NULL);
      fprintf(uctout,
        "Timezone warning: Offset is no multiple of 15 minutes,\n"
        "-\t\t timezone offset: %s%d minute%s (%s),\n"
        "-\t\t time: ",
          (timezone > 0) ? "+" : "", timezone,
          PLURAL_S(abs(timezone)),
          TIMEZONE_TYPE1TXT(timezone));
      printTimestampShort(t, TRUE, "\n");
      nodePrintUnicodeNameContLine(node,mc);
    MLIMITend;
  }

  /* verify, no further error messages yet
   */
  if( isValidTimestamp(t) )
  { return TRUE;            /* done for a correct timestamp */
  }

  /* Incorrect or blank timestamp.
   * ECMA 1/7.3, test if unspecified date and time because
   * all zero bytes. Only allowed for this Timestamp
   * if obligatoryRefTxt == NULL, else obligatoryRefTxt points
   * to a text denoting why it is not allowed.
   */
  if( verifyZeros((Byte*)t, sizeof(Timestamp), NULL, NULL, NULL) )
  {                                 /* all zero bytes */
    if( obligatoryRefTxt != NULL )  /* not allowed for this field */
    { MLIMITbegin(ERROR00level, MLIMITdefault20);
        printMessageHead(MLIMIT.vl, d, (Byte*)t, NULL);
        fprintf(uctout, "Timestamp error: date and time not specified, %s\n",
                            obligatoryRefTxt);
        nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
      return FALSE; /* unspecified date and time error */
    }
    return TRUE;    /* unspecified date and time ok */
  }

  /* Some Timestamp error found.
   */
  MLIMITbegin(ERROR00level, MLIMITdefault20);
    printMessageHead(MLIMIT.vl, d, (Byte*)t,
      "Timestamp error: See below, error fields marked with *,\n");
    if( typeError )
    { fprintf(uctout,
        "-\texpected timestamp type: 1\n");
    }
    if( zoneError )
    { fprintf(uctout,
        "-\texpected timezone: in +/- 1440 range or undefined (-2047)\n");
    }
    fprintf(uctout,
        "-\t\t ECMA 1/7.3, UDF 2., 2.1.4.1, 3.1.1.\n");
    nodePrintUnicodeNameContLine(node,mc);
    printTimestampFull(t);
  MLIMITend;

  return FALSE;

}   /* end verifyTimestamp01() */

/* verifyTimestamp:
 * First execute verifyTimestamp01()
 * then check relation with LVID or VAT Modification timestamp.
 * Return value: TRUE if Timestamp verification was completed,
 *         else: FALSE.
 */
static bool verifyTimestamp(Timestamp *t, char *obligatoryRefTxt,
                            char *shortText, Byte *d,
                            UdfMountContext *mc, Node *node)
{
    bool result = verifyTimestamp01(t, obligatoryRefTxt,
                                    d, mc, node);
    /* verifyTimestamp01() must be executed for valid Timestamps
     * too, it may print a warning for unusual timezones.
     * An undefined Timestamp (all zero's) may be correct. In
     * that case, no compare with medium last modification time.
     */
    if( !isValidTimestamp(t) )
    {   return result;
    }
    return verifyTimestampLVIDorVAT(t, shortText, d, mc, node);
}

/* Print extra information in case of a message in
 * verifyPartitionHeaderDescriptor() or one of the
 * functions called by it (verifyShortAd(), etc).
 */
extern void printExtraInfoForPHD( Byte *d, Byte *pBP )
{
    Uint16 tagId = ((Tag *)d)->tagIdentifier;

    if( tagId == tidPD )
    {
        size_t bp1, bp2, BP = pBP - d;
        bp1 = offsetof( PartitionDescriptor,
                        partitionContentsUse.partitionHeaderDescriptor);
        bp2 = bp1 + sizeof(PartitionHeaderDescriptor);
        if( BP >= bp1 && BP < bp2 )
        {
            fprintf(uctout,
                "-\t\t RBP %lu in Partition Header Descriptor,"
                            " ECMA 4/14.3, UDF 2.3.3\n", BP - bp1);
        }
    }
}   /* end printExtraInfoForPHD() */

/* verifyExtentLength: verifies both part 3 and part 4 extentLength.
 * name must be: extent_ad, short_ad, long_ad or ext_ad
 * ECMA 3/7.1.1 and 4/14.14.1.1
 * Mind that ALL extents may be at most 2^30 MINUS BLOCK SIZE,
 * UDF section 2 Basic Restrictions & Requirements.
 * For UDF 2.01 this value was corrected to ((2^30 - 1) rounded down
 * to the nearest multiple of the block size).
 * This is more accurate for the case that 2^30 is not a multiple
 * of the block size (e.g. block size is odd multiple of 512).
 * If isFixedRecAndAlloc, then the extent type for LongAd and ShortAd
 * shall be ADEL_RECORDED_AND_ALLOCATED.
 * isFixedRecAndAlloc is don't care for name "extent_ad"
 *
 * Implementation note: partRef is irrelevant if mc == NULL
 */
static bool verifyExtentLength( Uint32 extentLength,
                                Uint16 partRef,
                                bool   isIntegralBlock,
                                bool   isFixedRecAndAlloc,
                                Byte  *d, Byte *pBP, char *name,
                                UdfMountContext *mc)
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32   blockSize   = vmi->blockSize;
    Uint32   extentSize = 0,
             maxExtentSize;
    Uint8    extentType = 0;    /* keep compiler happy */
    char    *ecmaRef;
    bool     isExtentAd, result = TRUE;

    maxExtentSize = ROUNDDOWNMULT(TWO_POWER30 - 1, blockSize);

    if( strcmp(name, "extent_ad") == 0 )    /* extent_ad */
    {   isExtentAd = TRUE;
        extentSize = extentLength;
        ecmaRef = "ECMA 3/7.1.1";
    }
    else                /* short_ad, long_ad (, ext_ad) */
    {   isExtentAd = FALSE;
        extentSize = elGetExtentSize(extentLength);
        extentType = elGetExtentType(extentLength);
        ecmaRef = "ECMA 4/14.14.1.1";
    }

    if( extentSize > maxExtentSize )
    {   result = FALSE;     /* fatal */
        MLIMITbegin( ERROR00level, uctMessageLimit );
          printMessageHead(MLIMIT.vl, d, pBP, name);
          fprintf(uctout, " error: Extent Length: %lu,\n"
            "-\t\t expected: at most %lu, UDF 2., %s\n",
            extentSize, maxExtentSize, ecmaRef);
        MLIMITend;
    }

    if( !isExtentAd )       /* short_ad, long_ad (or ext_ad) */
    {
        if( extentSize == 0 && extentType != 0 )
        {
            /* no FALSE return result */
            MLIMITbegin( ERROR00level, uctMessageLimit );
              printMessageHead(MLIMIT.vl, d, pBP, name);
              fprintf(uctout, " error: Extent Type non-zero (%u),\n"
                "-\t\t while Extent Length is zero, %s\n",
                extentType, ecmaRef);
              printExtraInfoForPHD(d, pBP); /* for short_ad */
            MLIMITend;
        }
        if(   isFixedRecAndAlloc
           && extentType != ADEL_RECORDED_AND_ALLOCATED )
        {
            /* no FALSE return result */
            MLIMITbegin( ERROR00level, uctMessageLimit );
              printMessageHead(MLIMIT.vl, d, pBP, name);
              fprintf(uctout, " error: Extent Type: %u,"
                                        " expected: %u,\n"
                "-\t\t for this field, %s.\n",
                extentType, ADEL_RECORDED_AND_ALLOCATED, ecmaRef);
              printExtraInfoForPHD(d, pBP); /* for short_ad */
            MLIMITend;
        }
        if(    extentType == ADEL_EXTENTPOINTER
            && extentSize != blockSize )
        {
            /* no FALSE return result
             * Handle as a warning here, because it may be
             * allowed to ALLOCATE more that one block.
             */
            MLIMITbegin( WARN01level, uctMessageLimit );
              printMessageHead(MLIMIT.vl, d, pBP, name);
              fprintf(uctout,
                " warning: Extent Length: %lu, expected: %lu\n"
                "-\t\t Extent type is pointer to next extent"
                                            " of allocation\n"
                "-\t\t descriptors which shall be one block,"
                                    " UDF 2.3.11\n",
                    extentSize, blockSize);
              printExtraInfoForPHD(d, pBP); /* for short_ad */
            MLIMITend;
        }
        if(    mc != NULL && mc->vi->lvd != NULL
            && mc->partitionMapInfo != NULL
            && partRef < mc->vi->lvd->numberOfPartitionMaps
            && mc->partitionMapInfo[partRef].pMapType == PMAPTYPE_VIRTUAL
            && extentSize > blockSize )
        {   bool is150;
            /* Virtual extent > blockSize not allowed for UDF 2.00+
             * and not recommended for UDF 1.50.
             * The verifier has no problem reading these extents
             * because translateAddress() is called for each block.
             */
            is150 = (getUctUdfRevision() == 0x150);
            MLIMITbegin((is150) ? WARN01level
                                : VERBOSE00level, uctMessageLimit);
              printMessageHead(MLIMIT.vl, d, pBP, name);
              fprintf(uctout, " %s: Virtual extent length: %lu.\n"
                  "-\t\t Required for UDF 2.00+ and recommended"
                                        " for UDF 1.50:\n"
                  "-\t\t at most the block size,"
                                " UDF 2.00+ 2., 2.3.10.\n",
                  (is150) ? "warning" : "error", extentSize);
              printExtraInfoForPHD(d, pBP); /* for short_ad */
            MLIMITend;
            }
    }

    if( isIntegralBlock && (extentSize % blockSize) != 0 )
    {
        /* no FALSE return result */
        MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead(MLIMIT.vl, d, pBP, name);
            fprintf(uctout,
                " error: Extent Length: %lu, no integral"
                                    " number of blocks.\n"
                "-\t\t Only allowed for last extent of file body,"
                                            " %s, 4/12.1\n",
                        extentSize, ecmaRef);
            printExtraInfoForPHD(d, pBP);   /* for short_ad */
        MLIMITend;
    }

    return result;

}   /* end verifyExtentLength() */

/* verifyExtentAd: ECMA 3/7.1
 * Do not mix up: ExtentAd  (ECMA: extent_ad)
 *           and: ExtAd     (ECMA: ext_ad)
 */
static bool verifyExtentAd(ExtentAd *extentAd, Byte *d,
                           bool isIntegralBlock )
{
    char *name = "extent_ad";
    bool  result;

    result = verifyExtentLength( extentAd->extentLength, (Uint16) -1,
                                 isIntegralBlock,
                                 TRUE,      /* don't care for extent_ad */
                                 d, (Byte*) extentAd, name, NULL );

    if( extentAd->extentLength == 0 && extentAd->extentLocation != 0 )
    {
        /* no FALSE return result */
        MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead( MLIMIT.vl,
                d, (Byte*) extentAd, name );
            fprintf(uctout, " error: Extent Location non-zero (%lu),\n"
              "-\t\t while Extent Length is zero, ECMA 3/7.1.2\n",
                            extentAd->extentLocation);
        MLIMITend;
    }
    return result;
}

/* verifyLBAddr():
 *
 * Note: explicit pointer pBP for messages BP Byte offset (maybe NULL).
 * pBP and errorName should be consistent in some way, e.g. if
 * pBP points to a LongAd, the errorName should be "long_ad ...".
 * In that case, lba should point to the LongAd extentLocation.
 */
static bool verifyLBAddr( LBAddr *lba, Byte *d, Byte *pBP, char *name,
                          UdfMountContext *mc, bool isAllocated )
{
    LogicalVolumeDescriptor *lvd;

    /* For some tests a reference to the LVD is needed,
     * normally this is taken from the mc mount context.
     * For certain volume descriptor fields, verifyLBAddr()
     * may be called before mc->vi->lvd is installed.
     * This is also the case in LBAddr (LongAd) fields in
     * an LVD itself, in which case we can use d as the
     * LVD pointer. If d is a LVD pointer, using it is
     * prefered above the mount context.
     */
    lvd = NULL;
    if(   d != NULL
       && ((Tag*)d)->tagIdentifier == tidLVD )
    {   lvd = (LogicalVolumeDescriptor*) d;
    }
    else if( mc != NULL && mc->vi != NULL && mc->vi->lvd != NULL )
    {   lvd = mc->vi->lvd;          /* from mount context */
    }
    if( lvd == NULL )
    {   MLIMITbegin( WARN01level, uctMessageLimit );
          printMessageHead(MLIMIT.vl, d,
                           (pBP != NULL) ? pBP : (Byte*)lba,
                           name);
          fprintf(uctout,
            " DEBUG warning: please report:\n"
            "-\t\t Incomplete verifyLBAddr() test\n");
        MLIMITend;
        return FALSE;   /* no LVD found yet */
    }

    /* LVD found. Test Part Ref Number and Logical Block Number.
     * Partition Reference Number shall be less than number of
     * partition maps.
     * If the extent is unallocated, the Part Ref Number and Logical
     * Block Number shall be zero according to ECMA 4/14.9.22.
     * This is tested in verifyShortAd() and verifyLongAd()
     * already, so skip the test here if unallocated.
     */
    if(      isAllocated        /* skip test if unallocated */
       &&    (Uint32) lba->partitionReferenceNumber
          >= lvd->numberOfPartitionMaps )
    { /* no FALSE return result. A FALSE result will arise
       * when this partitionReferenceNumber is used in
       * readBlocksFromPartition() or translateAddress()
       */
      MLIMITbegin(ERROR00level, uctMessageLimit);
        printMessageHead(MLIMIT.vl, d,
                           (pBP != NULL) ? pBP : (Byte*)lba,
                           name);
        fprintf(uctout, " error: Partition Reference Number: %lu,\n"
            "-\t\t expected: less than %lu, because only %lu"
                                        " partition map%s present\n",
                   lba->partitionReferenceNumber,
                     lvd->numberOfPartitionMaps,
                      lvd->numberOfPartitionMaps,
              PLURAL_S(lvd->numberOfPartitionMaps));
      MLIMITend;
    }
    else if( isAllocated )      /* skip test if unallocated */
    { /* part ref nmb ok, test if LBN inside partition
       */
      PartitionMapInfo *pmi;
      Uint32 pLen;
      pmi = &mc->partitionMapInfo[lba->partitionReferenceNumber];
      pLen = pmi->actualPartitionLength;
      if( lba->logicalBlockNumber >= pLen )
      { /* no FALSE return result. A FALSE result will arise
         * when this logicalBlockNumber is used in
         * readBlocksFromPartition() or translateAddress()
         */
        MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageHead(MLIMIT.vl, d,
                           (pBP != NULL) ? pBP : (Byte*)lba,
                           name);
          fprintf(uctout, " error: Logical Block Number: %lu,\n"
              "-\t\t expected: less than partition size %lu for partition\n"
                  "-\t\t\t   with Partition Reference Number %lu.\n",
                   lba->logicalBlockNumber, pLen,
                   lba->partitionReferenceNumber);
        MLIMITend;
      }
    }

    return TRUE;

}   /* end verifyLBAddr() */

/* verifyShortAd()
 * ECMA 4/14.14.1
 */
static bool verifyShortAd( ShortAd *sad, Uint16 shortPartRef,
                           byte *d,
                           bool  isIntegralBlock,
                           bool  isFixedRecAndAlloc,
                           UdfMountContext *mc)
{
    char   *name       = "short_ad";
    Uint32  extentSize = adGetExtentSize(sad);
    Uint8   extentType = adGetExtentType(sad);
    bool    result;

    result = verifyExtentLength( sad->extentLength, shortPartRef,
                                 isIntegralBlock, isFixedRecAndAlloc,
                                 d, (Byte*)sad, name, mc );

    if( extentSize == 0 && sad->extentPosition != 0 )
    {
        /* no FALSE return result */
        MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead(MLIMIT.vl, d, (Byte*)sad, name);
            fprintf(uctout, " error: Extent Position non-zero (%lu),\n"
                "-\t\t while Extent Length is zero. ECMA 4/14.14.2.2\n",
                            sad->extentPosition);
            printExtraInfoForPHD(d, (Byte *) sad);  /* for short_ad */
        MLIMITend;
    }

    if(    extentType == ADEL_NOT_RECORDED_NOT_ALLOCATED
        && sad->extentPosition != 0 )
    {
        /* no FALSE return result */
        MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead(MLIMIT.vl, d, (Byte*)sad, name);
            fprintf(uctout, " error: Extent Position non-zero (%lu),\n"
                "-\t\t for Extent Type %u (not allocated), ECMA 4/14.9.22.\n",
                    sad->extentPosition, ADEL_NOT_RECORDED_NOT_ALLOCATED);
            printExtraInfoForPHD(d, (Byte *) sad);  /* for short_ad */
        MLIMITend;
    }
    return result;

}   /* end verifyShortAd() */


/* verifyMetadataPref():
 * Verify if (*pPref) is inside/outside the Metadata Partition
 * If inMetadataPartition == TRUE it must be inside, else outside.
 *
 * If return result == FALSE, a message was printed on VERBOSE00level
 *              so that additions can be printed at the calling level.
 * else return result == TRUE;
 *
 * Note: explicit pointer pBP for messages BP Byte offset (maybe NULL).
 * pBP and errorName should be consistent in some way, e.g. if
 * pBP points to a LongAd, the errorName should be "long_ad ...".
 * In that case, pPref should point to the
 * LongAd extentLocation.partitionReferenceNumber.
 * Note: see also UDF 2.50 errata DCN-5105.
 */
static bool verifyMetadataPref(UdfMountContext *mc,
                               Uint16   *pPref,
                               Byte     *d,
                               Byte     *pBP,
                               bool      inMetadataPartition,
                               char     *errorName)
{   bool result = TRUE;

    /* If metadata Partition present, then check if (*pPref)
     * points to the correct partition.
     */
    if( mc != NULL && IS_PREF_PARTITION_FOUND(mc->metadataPref) )
    { Uint16 mPref = mc->metadataPref;
      char  *txtNot = (inMetadataPartition) ? "" : "NOT ";

      if(   ( inMetadataPartition && (*pPref) != mPref)
         || (!inMetadataPartition && (*pPref) == mPref) )
      {
        MLIMITbegin(ERROR00level,uctMessageLimit);
          printMessageHead(MLIMIT.vl,(Byte*)d,
              (pBP != NULL) ? pBP : (Byte*)pPref, NULL);
          fprintf(uctout,
            "%s error: Partition Reference Number: %u, "
                                        "expected: %s%u.\n"
             "-\t\t Shall %srefer to the "
                        "Metadata Partition, UDF 2.2.13.1%s.\n",
                errorName, (*pPref), txtNot, mPref, txtNot,
                (getUctUdfRevision()<=0x250)
                    ? ",\n-\t\t UDF 2.50 errata DCN-5105" : "");
        MLIMITend;
        result = FALSE;     /* message printed */
      }
    }
    return result;

}   /* end verifyMetadataPref() */

/* verifyLongAd:
 * ECMA 4/14.14.2, UDF 2.00+ 2., 2.3.10.
 *
 * Note: descriptor pointer d cannot be NULL.
 *
 * inMetadataPartition denotes if the extent shall refer
 * to the Metadata Partition (if any), or if it shall not.
 */
extern bool verifyLongAd(LongAd *lad, Byte *d, UdfMountContext *mc,
                         bool isIntegralBlock,
                         bool isFixedRecAndAlloc,
                         bool inMetadataPartition)
{
    char   *name       = "long_ad";
    Uint8   extentType = adGetExtentType(lad);
    Uint32  extentSize = adGetExtentSize(lad);
    Uint32  messCnt    = 0;
    bool    result;

    result = verifyExtentLength(lad->extentLength,
                    lad->extentLocation.partitionReferenceNumber,
                                isIntegralBlock, isFixedRecAndAlloc,
                                d, (Byte*)lad, name, mc);

    /* consider extent also unallocated if extentSize is zero
     */
    if( !verifyLBAddr(&lad->extentLocation, d, (Byte*)lad,
                      "long_ad extentLocation", mc,
                      (   extentType != ADEL_NOT_RECORDED_NOT_ALLOCATED
                       && extentSize != 0)) )
    {   result = FALSE;
    }

    /* test partition reference number inside/outside Metadata Partition
     */
    if(    extentSize != 0
       &&  extentType != ADEL_NOT_RECORDED_NOT_ALLOCATED
       && !verifyMetadataPref(
                mc, &lad->extentLocation.partitionReferenceNumber,
                d, (Byte*)lad, inMetadataPartition, name) )
    { /* message was printed on VERBOSE00level
       */
       messCnt++;   /* print extra printLongAd() and tag id info */
    }

    if(        extentSize == 0
        && (   lad->extentLocation.logicalBlockNumber != 0
            || lad->extentLocation.partitionReferenceNumber != 0) )
    {
        /* no FALSE return result */
        MLIMITbegin( ERROR00level, uctMessageLimit );
            messCnt++;
            printMessageHead(MLIMIT.vl, d, (Byte*)lad, name);
            fprintf(uctout, " error: Extent Location non-zero (%lu,%lu),\n"
              "-\t\t while Extent Length is zero, ECMA 4/14.14.2.2\n",
                        lad->extentLocation.logicalBlockNumber,
                        lad->extentLocation.partitionReferenceNumber);
        MLIMITend;
    }

    if(    extentType == ADEL_NOT_RECORDED_NOT_ALLOCATED
        && (   lad->extentLocation.logicalBlockNumber != 0
            || lad->extentLocation.partitionReferenceNumber != 0) )
    {
        /* no FALSE return result */
        MLIMITbegin( ERROR00level, uctMessageLimit );
          messCnt++;
          printMessageHead(MLIMIT.vl, d, (Byte*)lad, name);
          fprintf(uctout, " error: Extent Location non-zero (%lu,%lu),\n"
            "-\t\t for Extent Type %u (not allocated), ECMA 4/14.9.22.\n",
                lad->extentLocation.logicalBlockNumber,
                lad->extentLocation.partitionReferenceNumber,
                ADEL_NOT_RECORDED_NOT_ALLOCATED);
        MLIMITend;
    }

    if( (lad->implementationUse.flags & EXTENTErased) != 0 )
    {   /* test access type, etc. */
        Uint16 pRef = lad->extentLocation.partitionReferenceNumber;
        Uint32 accessType;

        /* although UDF 2.3.10.1 talks about "Rewritable media",
         * the partition access type is tested here.
         * For long_ad in Volume descriptor (e.g. LVD) the
         * mc UdfMountContext may not yet be set up properly
         */
        if(    mc == NULL || mc->vi == NULL || mc->vi->lvd == NULL
           || ((Uint32) pRef) >= mc->vi->lvd->numberOfPartitionMaps
           ||  mc->partitionMapInfo == NULL
           ||  mc->partitionMapInfo[pRef].pdPointer == NULL )
        {
#ifdef  DEBUG01
            MLIMITbegin(DEBUG01level, MLIMITdefault01);
                messCnt++;
                printMessageHead(MLIMIT.vl, d, (Byte*)lad, name);
                fprintf(uctout,
                  " DEBUG01: Unable to test EXTENTErased"
                            " access type\n");
            MLIMITend;
#endif  /* DEBUG01 */
        }
        else
        { accessType = mc->partitionMapInfo[pRef].pdPointer->accessType;
          if( accessType != PDAT_REWRITABLE )
          { MLIMITbegin( ERROR00level, uctMessageLimit );
              messCnt++;
              printMessageHead(MLIMIT.vl, d, (Byte*)lad, name);
              fprintf(uctout,
                " error: ADImpUse EXTENTErased flag set for\n"
                "-\t\t\t\tpartition with access type %lu (%s),\n"
                "-\t\t\t\texpected: %lu (%s),\n"
                "-\t\t\t\tUDF 2.3.10.1, ECMA 3/10.5.7\n",
                         accessType, PDAT_TEXT(accessType),
                    PDAT_REWRITABLE, PDAT_TEXT(PDAT_REWRITABLE));
            MLIMITend;
            /* no FALSE return result */
          }
        }

        if( extentType != ADEL_NOT_RECORDED_BUT_ALLOCATED)
        {
            /* no FALSE return result */
            MLIMITbegin( ERROR00level, uctMessageLimit );
                messCnt++;
                printMessageHead(MLIMIT.vl, d, (Byte*)lad, name);
                fprintf(uctout,
                  " error: ADImpUse EXTENTErased flag set for extent\n"
                  "-\t\t type not equal to %u, UDF 2.3.10.1,"
                                             " ECMA 4/14.14.1.1\n",
                        ADEL_NOT_RECORDED_BUT_ALLOCATED);
            MLIMITend;
        }
    }
    if( ((Uint16)(lad->implementationUse.flags & (~EXTENTErased))) != 0 )
    {
            /* no FALSE return result */
        MLIMITbegin(ERROR00level, uctMessageLimit);
            messCnt++;
            printMessageHead(MLIMIT.vl, d, (Byte*)lad, name);
            fprintf(uctout, " error: ADImpUse flags, reserved bit set\n"
                            "-\t\t UDF 2.3.10.1\n");
        MLIMITend;
    }

    /* do not verify FID UDFUniqueID here.
     * if any error or warning message was printed above,
     * then dump LongAd.
     */
    if( messCnt != 0 )
    {   size_t offset;
        Uint16 tagId = ((Tag *)d)->tagIdentifier;
        char *txt;
        /* LongAd ICB is the only LongAd in a FID so:
         */
        printLongAd( lad, (tagId == tidFID));   /* isFidIcb */

        /* add field name if possible.
         * TODO: put stuff below in a getFieldName() function.
         */
        offset = ((Byte*)lad) - d;      /* offset of LongAd struct */
        txt = NULL;

        if( tagId == tidLVD && offset == 248 )          /* LVD */
        {   txt = "Logical Volume Contents Use FSD Sequence Extent field";
        }
        else if( tagId == tidFSD)                       /* FSD */
        { if(      offset == 400 ) txt = "Root Directory ICB field";
          else if( offset == 448 ) txt = "Next Extent field";
          else if( offset == 464 ) txt = "System Stream Directory ICB field";
        }
        else if( tagId == tidFID && offset == 20 )      /* FID */
        {   txt = "ICB field";
        }
        else if( (tagId == tidFE  && offset >= 176)     /* FE  */
              || (tagId == tidEFE && offset >= 216)     /* EFE */
              || (tagId == tidAED && offset >= 24) )    /* AED */
        {   txt = "AD, defining file or directory data space";
        }
        else if( tagId == tidFE                         /* FE  */
              || tagId == tidEFE )                      /* EFE */
        { if(      lad == pFE_extendedAttributeICB(d) )
            txt = "Extended Attribute ICB field";
          else if( lad == pFE_streamDirectoryICB(d) )   /* NULL for FE */
            txt = "Stream Directory ICB field";
        }
        else if( tagId == tidIE && offset == 36 )       /* IE */
        {   txt = "Indirect ICB field";
        }
        if( txt != NULL )
        {   VERBOSE00(uctout, "-\tMessage concerns %s %s.\n",
                                tidTEXT4(tagId), txt);
        }
        VERBOSE00(uctout, "\n");
    }

    return result;

}   /* end verifyLongAd() */

/* print all fields of a LongAd as continuation lines
 * after an error has been detected.
 * isFidIcb shall denote if lad is
 * pointing to a FID ICB field.
 */
extern void printLongAd( LongAd *lad, bool isFidIcb )
{
    Uint32  extentSize = adGetExtentSize(lad);
    Uint8   extentType = adGetExtentType(lad);
    int  n; Byte *b;

    VERBOSE00(uctout,
        "-\tlong_ad: Extent Length     : #%08lX, size %lu bytes, extent type %u\n"
        "-\t         Extent Location   : Logical Block Number      : %lu\n"
        "-\t                           : Partition Reference Number: %u\n"
        "-\t         Implementation Use: ADImpUse Flags : #%04X\n"
        "-\t                           :          ImpUse:",
                lad->extentLength, extentSize, extentType,
                lad->extentLocation.logicalBlockNumber,
                lad->extentLocation.partitionReferenceNumber,
                lad->implementationUse.flags);

    /* FID ICB UDFUniqueID was introduced in UDF 2.00.
     */
    if( isFidIcb && getUctMinUdfRevision() >= 0x200 ) /* 2.00+ */
    {   VERBOSE00(uctout, " #%X, UDF UniqueID for FID ICB",
                lad->implementationUse.ImpUse.UDFUniqueID);
    }
    else
    {   for( b = lad->implementationUse.ImpUse.impUse,
            n = sizeof(lad->implementationUse.ImpUse.impUse);
            n > 0;
            n--, b++ )
        {   VERBOSE00(uctout, " #%02X", *b);
        }
    }
    VERBOSE00(uctout, "\n");
}

/* call for short_ad and long_ad only
 */
static void printAdInfo( AnyAllocationDescriptor *ad, Uint8 adType,
                         Uint32 count, Uint16 shortAdPartitionRef,
                         Node *node, Uint32 bytesLeftInADarea,
                         bool printHeaderLine, Uint8 vLevel)
{
    Uint32 adSize;
    char  *adName,
          *endOfAdsTxt = NULL;  /* not last AD of current AD area */

    switch( adType )
    {
    case ADT_SHORT:
        adSize = sizeof(ShortAd);
        adName = "short_ad";
        break;
    case ADT_LONG:
        adSize = sizeof(LongAd);
        adName = "long_ad";
        break;
    case ADT_EXTENT:
        adSize = sizeof(ExtentAd);
        adName = "extent_ad";
        break;
    default:
        return;     /* assert ?? */
        /** break; **/
    }

    /* node maybe NULL for UnallocatedSpaceEntry AD
     */
    ifVERBOSE( vLevel )
    {
        Uint8  extentType = adGetExtentType(&ad->anyAd);
        Uint32 extentSize = adGetExtentSize(&ad->anyAd);
        Uint32 loc = 0;
        Uint16 pRef = PREF_PARTITION_NOT_FOUND;

        if( printHeaderLine )
        { fprintf(uctout,
            "\tcnt:  extent type,      size,  location,part%s\n",
              (node == NULL) ? "" : ",     body, total alloc");
        }

        if( extentType == ADEL_EXTENTPOINTER )
             fprintf(uctout, "\t    ");         /* no count */
        else fprintf(uctout, "\t%3lu:", count);

        fprintf(uctout, " %-8s  %2u %10lu",
                adName, extentType, extentSize);

        /* no \n printed yet
         */
        if( extentSize != 0 )
        {   switch( adType )
            {
            case ADT_SHORT:
                loc  = ((ShortAd*)ad)->extentPosition,
                pRef =  shortAdPartitionRef;
                break;
            case ADT_LONG:
                loc  = ((LongAd*)ad)->extentLocation.logicalBlockNumber;
                pRef = ((LongAd*)ad)->extentLocation.partitionReferenceNumber;
                break;
            case ADT_EXTENT:
                loc  = ((ExtentAd*)ad)->extentLocation;
                pRef = (Uint16) -1;
                break;
            }
            fprintf(uctout, " %10lu %-2u  ", loc, pRef);
            if( node != NULL)
            {   printUint64(vLevel, node->feFileBodyLength,
                            FALSE, "%10lu");
                fprintf(uctout, " ");
                printUint64(vLevel, node->feTotalExtentsLength,
                            FALSE, "%10lu");
            }
            fprintf(uctout, "\n");      /* possible endOfAdsTxt on new line */
            if( extentType == ADEL_EXTENTPOINTER )
            {   endOfAdsTxt = "-\t     points to next extent of ADs";
            }
            else if( bytesLeftInADarea < adSize && bytesLeftInADarea != 0 )
            {   endOfAdsTxt = "-\t     end of ADs";
            }
        }
        else    /* extentSize == 0 */
        {   endOfAdsTxt = " -> end of ADs";     /* add on same line */
        }

        if( endOfAdsTxt != NULL )
        {   fprintf(uctout, "%s", endOfAdsTxt);
            if( bytesLeftInADarea )
            {   fprintf(uctout,
                    ", %lu unused bytes in AD area.", bytesLeftInADarea);
            }
            fprintf(uctout, "\n");
        }
    }
    ENDif;
}


static bool verifyAD(AnyAllocationDescriptor *ad, Uint8 adType,
                     Uint16 shortPartRef,
                     Byte *d, UdfMountContext *mc,
                     bool isIntegralBlock,
                     bool isFixedRecAndAlloc,
                     bool inMetadataPartition)
{
    switch( adType )
    {
    case ADT_SHORT:
        return verifyShortAd(&ad->shortAd, shortPartRef, d,
                              isIntegralBlock, isFixedRecAndAlloc, mc);
        /** break; **/
    case ADT_LONG:
        return verifyLongAd(&ad->longAd, d, mc, isIntegralBlock,
                             isFixedRecAndAlloc, inMetadataPartition);
        /** break; **/
    case ADT_EXTENT:
        return verifyExtentAd(&ad->extentAd, d, isIntegralBlock);
        /** break; **/
    }
    return FALSE;

}   /* end verifyAD() */


/* verifyEntityID():
 * Context dependent check for different suffix types.
 * Maintains history list of all EntityIDs,
 * verify and print of newly added EntityID's only.
 *
 * expectedIdentifier is NULL, or it points to a Byte array of
 * ENTITYID_IDSIZE bytes holding the expected EntityID Identifier
 * (Byte expectedIdentifier[ENTITYID_IDSIZE]).
 *
 * If expectedIdentifier != NULL, then udfRef holds the
 * UDF section reference.
 *
 * Implementation note:
 *  Much of the testing, etc. is hided in printEntityID(),
 *  called by storeAndPrintNewEntityID().
 */
static bool verifyEntityID(EntityID *eid,
                           ENTITY_SUFFIX_TYPE suffixType,
                           Byte *expectedIdentifier,
                           char *udfRef, Byte *d)
{
    bool result = storeAndPrintNewEntityID(eid, suffixType, d);

    if(   expectedIdentifier != NULL
       && memcmp( eid->Identifier, expectedIdentifier,
                  ENTITYID_IDSIZE ) != 0 )
    {   MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageHead( MLIMIT.vl, (Byte*)d, eid->Identifier,
                "Error: Unexpected EntityID Identifier,\n"
                             "-\t\t\t");
          printBytesName(eid->Identifier, ENTITYID_IDSIZE,
                         FALSE, MLIMIT.vl);
          fprintf(uctout,
              ",\n-\t      Expected: ");
          printBytesName(expectedIdentifier, ENTITYID_IDSIZE,
                         FALSE, MLIMIT.vl);
          fprintf(uctout, ", %s.\n", udfRef);
        MLIMITend;
        result = FALSE;
    }
    return result;

}   /* end verifyEntityID() */

/* verify ICBTag
 * UDF 2.00 2.3.5 and 6.6
 * ECMA 4/14.6
 *
 * isStream must be TRUE iff a stream is expected according to
 * the node hierarchy position (child of a stream directory).
 *
 * TODO: ICBTAG flags bits not tested: 5-8 and 10, check if ok.
 */
static bool verifyICBTag(ICBTag *it, Byte *d,
                         bool isStream,
                         UdfMountContext *mc, Node *node)
{
    const MediumInfo *vmi = getTheMediumInfo();
    char    *name         = "icbtag";
    char    *refTxt;
    bool     result = TRUE;
    Uint32   countMessPrinted = 0;
    int      intFileType = (int)it->fileType;   /* int to avoid gcc warnings */
    Uint8    expFileType, adType;
    Uint16   strategyType     = it->strategyType;
    Uint16   expU16, udfRev   = getUctUdfRevision();
    Uint16   tagId            = ((Tag*)d)->tagIdentifier;

    /* node maybe NULL for UnallocatedSpaceEntry
     */
    if( strategyType != 4 && strategyType != 4096 )
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
          countMessPrinted++;
          printMessageExpectedU32(MLIMIT.vl, d, (Byte*) &it->strategyType,
            "icbtag error: Strategy Type", "%u", strategyType, 4,
            " or 4096\n-\t\t Cannot test many other icbtag fields\n");
        MLIMITend;
        result = FALSE;
    }
    else        /* Strategy Type 4 or 4096 */
    {   /* Only strategy type 4 for FE, EFE or USE, else 4096.
         * IE and TE are used for strat 4096 only because they
         * are no direct entries. PIE is illegal for UDF.
         */
        if(    strategyType == 4
            && tagId != tidFE
            && tagId != tidEFE
            && tagId != tidUSE )
        {
            MLIMITbegin(ERROR00level, uctMessageLimit);
              countMessPrinted++;
              printMessageExpectedU32(MLIMIT.vl, d, (Byte*) &it->strategyType,
                "icbtag error: Strategy Type", "%u", strategyType, 4096, "\n");
            MLIMITend;
            /* no FALSE return result */
        }
        else if(   udfRev >= 0x250          /* UDF 2.50 2.3.5.1 */
                && strategyType == 4096
                && (   vmi->sequentialType != MTYPE_SE_NONSEQUENTIAL
                    || vmi->writabilityType != MTYPE_WR_WRITEONCE
                    || intFileType == (int)FT_METADATA_FILE
                    || intFileType == (int)FT_METADATA_MIRROR_FILE
                    || intFileType == (int)FT_METADATA_BITMAP_FILE) )
        {
                /* note 1: need not test partition accessType
                 * note 2: metadata partition excludes write-once
                 */
            MLIMITbegin(ERROR00level, uctMessageLimit);
              countMessPrinted++;
              printMessageExpectedU32(MLIMIT.vl, d, (Byte*) &it->strategyType,
                "icbtag error: Strategy Type", "%u", strategyType, 4, ".\n"
                 "-\t\t For UDF 2.50+, Strategy Type 4096 is allowed for\n"
                 "-\t\t nonsequential write-once only, UDF 2.3.5.1.\n");
            MLIMITend;
            /* no FALSE return result */
        }

        /* Uint32 priorRecordedNumberOfDirectEntries,
         */
        if(    node != NULL
            && it->priorRecordedNumberOfDirectEntries
                != node->fePriorDirectEntries )
        {
          MLIMITbegin(ERROR00level, MLIMITdefault20);
            countMessPrinted++;
            printMessageHead(MLIMIT.vl, d,
              (Byte*) &it->priorRecordedNumberOfDirectEntries, NULL);
            fprintf(uctout,
              "icbtag error: Prior Recorded Number"
                                    " Of Direct Entries: %lu,\n"
              "-\t\t expected: %lu, Strategy Type: %u\n",
                    it->priorRecordedNumberOfDirectEntries,
                    node->fePriorDirectEntries, strategyType);
          MLIMITend;
          /* no FALSE return result */
        }

        /* Uint16 Strategy Type, test 4096 hierarchy
         */
        if(    node != NULL
            && node->fePriorDirectEntries != 0
            && strategyType != 4096 )
        {
            MLIMITbegin( ERROR00level, MLIMITdefault20 );
              countMessPrinted++;
              printMessageExpectedU32( MLIMIT.vl, d,
                (Byte*) &it->strategyType,
                "icbtag error: Strategy Type", "%u",
                strategyType, 4096, "\n-\t\t Inconsistent"
                        " Strategy Type in strategy 4096 hierarchy\n");
            MLIMITend;
          /* no FALSE return result */
        }

        /* Exception: strategyParameter[2] is always interpreted as a Uint16 !!!
         * In fact, strategy parameter for strategy type 4 is not defined,
         * assume zero.
         * Test strategyParameter.
         */
        expU16 = (Uint16)((strategyType == 4) ? 0 : 1);
        if( it->strPar.strategyParameterU16 != expU16 )
        {
          MLIMITbegin( ERROR00level, MLIMITdefault20 );
            countMessPrinted++;
            printMessageHead(MLIMIT.vl, d,
                    (Byte*)&it->strPar.strategyParameterU16, name);
            fprintf(uctout,
                    " error: (Uint16) strategyParameter: %u, expected: %u\n"
              "-\t\t\t       for Strategy Type %u, UDF 6.6.\n",
                    it->strPar.strategyParameterU16, expU16, strategyType);
          MLIMITend;
            /** no FALSE return result **/
        }
        /* Test maximumNumberOfEntries
         */
        expU16 = (Uint16)((strategyType == 4) ? 1 : 2);
        if( it->maximumNumberOfEntries != expU16 )
        {
            MLIMITbegin( ERROR00level, MLIMITdefault20 );
                countMessPrinted++;
                printMessageHead(MLIMIT.vl, d,
                                 (Byte*)&it->maximumNumberOfEntries, name);
                fprintf(uctout, " error: maximumNumberOfEntries: %u,"
                                    " expected: %u for strategyType %u\n",
                    it->maximumNumberOfEntries, expU16, strategyType);
            MLIMITend;
            /** no FALSE return result **/
        }
    }

    if( it->reserved != 0 )
    {
        MLIMITbegin(ERROR00level, MLIMITdefault10);
            countMessPrinted++;
            printMessageHead(MLIMIT.vl, d, (Byte*)&it->reserved, name);
            fprintf(uctout,
              " error: reserved byte: %u, expected: 0\n", it->reserved);
        MLIMITend;
        /** no FALSE return result **/
    }

    /* check File Type
     * check reserved, unspecified and 'agreement' values.
     */
    refTxt = NULL;      /* no File Type error so far */
    if(        udfRev < 0x200
            && intFileType >= (int)FT_RES_ECMA02_FIRST
            && intFileType <= (int)FT_RES_ECMA02_LAST )
    { refTxt = "ECMA 2nd edition 4/14.6.6";
    }
    else if(   udfRev >= 0x200
            && intFileType >= (int)FT_RES_ECMA03_FIRST
            && intFileType <= (int)FT_RES_ECMA03_LAST )
    { refTxt = "ECMA 3rd edition 4/14.6.6";
    }
    else if(   udfRev == 0x201
            && intFileType >= (int)FT_RES_UDF201_FIRST
            && intFileType <= (int)FT_RES_UDF201_LAST )
    { refTxt = "UDF 2.01 2.3.5.2";
    }
    else if(   udfRev >= 0x250  /* assumes no changes after UDF 2.50 */
            && intFileType >= (int)FT_RES_UDF250P_FIRST
            && intFileType <= (int)FT_RES_UDF250P_LAST )
    { refTxt = "UDF 2.3.5.2";
    }

    if( refTxt != NULL )    /* reserved file type error */
    { MLIMITbegin( ERROR00level, MLIMITdefault20 );
        countMessPrinted++;
        printMessageHead(MLIMIT.vl, d, (Byte*)&it->fileType, name);
        fprintf(uctout,
          " error: File Type: %lu, reserved value,\n"
            "-\t\t %s, UDF revision: ", intFileType, refTxt);
          printUdfRevisionRange(MLIMIT.vl, ".\n");
      MLIMITend;
      /** no FALSE return result **/
    }
    else    /* refTxt == NULL, further FileType Tests */
    {       /* check warning for unspecified file type */
      if(      udfRev != 0x150
            && intFileType == (int)FT_UNKNOWN_OR_VAT150 )
      { refTxt = "ECMA 4/14.6.6";
      }
      else if( udfRev == 0x200
            && intFileType >= (int)FT_AGREE_UDF200_FIRST
            && intFileType <= (int)FT_AGREE_UDF200_LAST )
      { refTxt = "ECMA 3rd edition 4/14.6.6";
      }
      if( refTxt != NULL )  /* warning for unspecified file type */
      { MLIMITbegin( WARN01level, MLIMITdefault20 );
          countMessPrinted++;
          printMessageHead(MLIMIT.vl, d, (Byte*)&it->fileType, name);
          fprintf(uctout,
                 " warning: File Type: %lu, %s.\n"
            "-\t\t %s, UDF revision: ",
            intFileType, (intFileType == (int)FT_UNKNOWN_OR_VAT150)
                        ? "unspecified"
                        : "subject to agreement ..",
            refTxt);
          printUdfRevisionRange(MLIMIT.vl, ".\n");
        MLIMITend;
        /** no FALSE return result **/
      }
    }

    if( isStream && intFileType != (int)FT_SEQUENCE_BYTES )
    {   MLIMITbegin( ERROR00level, MLIMITdefault20 );
            countMessPrinted++;
            printMessageExpectedU32( MLIMIT.vl, d, (Byte*)&it->fileType,
                "icbtag error: File Type", "%lu",
                intFileType, FT_SEQUENCE_BYTES,
                " for a named stream,\n"
                "-\t\t\t       UDF 3.3.5.1.\n");
        MLIMITend;
        /** void FALSE return result **/
    }

    expFileType = (tagId == tidUSE) ? FT_UNALLOCATEDSPACE
                : (tagId == tidIE)  ? FT_INDIRECT_ENTRY
                : (tagId == tidTE)  ? FT_TERMINAL_ENTRY
                : (Uint8)intFileType;

    if( intFileType != (int)expFileType )
    {   MLIMITbegin( ERROR00level, MLIMITdefault20 );
            countMessPrinted++;
            printMessageExpectedU32( MLIMIT.vl, d, (Byte*)&it->fileType,
                "icbtag error: File Type", "%lu",
                intFileType, expFileType, " for descriptor\n");
            fprintf(uctout,
                "-\t\t\t       type %lu (%s), ECMA 4/14.6.6.\n",
                        tagId, tidTEXT4(tagId));
        MLIMITend;
        /** void FALSE return result **/
    }

    /* Check parentICBLocation.
     * The parentICBLocation description changed when going from
     * ECMA 2nd to 3rd edition, see ECMA 2nd/3rd edition 4/8.10.1, 4/14.6.7,
     * UDF 2.3.5.3.
     * For strategy 4 and UDF 1.50- parentICBLocation shall be zero.
     * For strategy 4 and UDF 2.00+ parentICBLocation may be non-zero,
     * because the field is optional.
     * TODO: check: but that is very unlikely and not recommended
     *              (and I guess it is unforeseen).
     * NOTE: It is a misunderstanding that it->parentICBLocation should point
     *       to itself or to a higher parent in the directory hierarchy.
     *       So a special test for that is incorporated.
     */
    if( !verifyLBAddr(&it->parentICBLocation, d, NULL,
                      "icbtag parentICBLocation", mc, TRUE) )   /* isAllocated */
    {
        result = FALSE;
    }
    if(    strategyType == 4
       && (   it->parentICBLocation.logicalBlockNumber != 0
           || it->parentICBLocation.partitionReferenceNumber != 0) )
    { char *ewTxt, *txt2, *txt3; Uint8 mLevel;
      Node *tmpNode;
      bool  inDirHierarchy = FALSE;

      /* it->parentICBLocation shall not point to itself or to
       * a higher parent in the directory hierarchy.
       */
      tmpNode = node;
      do
      { LongAd *tmpIcb = nodeGetIcb(tmpNode, mc);
        if( memcmp( &tmpIcb->extentLocation,
                    &it->parentICBLocation, sizeof(LBAddr)
                  ) == 0 )
        { inDirHierarchy = TRUE;
        }
        if(   tmpNode != NULL
           && tmpNode->parent != tmpNode )
             tmpNode = tmpNode->parent;
        else tmpNode = NULL;

      } while( tmpNode != NULL );

      if( udfRev < 0x200 )          /* ECMA-2nd edition */
      { ewTxt = "error:";   mLevel = ERROR00level;
        txt2  = "expected"; txt3 = "2nd";
      }
      else                          /* ECMA-3rd edition */
      { ewTxt = "warning:"; mLevel = WARN01level;
        txt2  = "recommended"; txt3 = "3rd";
      }
      if( inDirHierarchy )          /* this is always an error */
      { ewTxt = "error:";   mLevel = ERROR00level;
      }
      MLIMITbegin(mLevel, MLIMITdefault10);
        countMessPrinted++;
        printMessageHead(MLIMIT.vl, d,
            (Byte*)&it->parentICBLocation, name);
        fprintf(uctout,
            " %s parentICBLocation: (%lu,%lu),\n"
            "-\t\t\t%s: (0,0) for strategy type 4.\n%s"
          "-\t\t ECMA %s edition 4/8.10.1, 4/14.6.7, UDF 2.3.5.3.\n",
            ewTxt, it->parentICBLocation.logicalBlockNumber,
            it->parentICBLocation.partitionReferenceNumber, txt2,
            (inDirHierarchy)
            ? "-\t\t Icbtag Parent ICB Location shall not point to itself\n"
              "-\t\t or to a higher parent in the directory hierarchy.\n"
            : "", txt3);
      MLIMITend;
      /** no FALSE return result **/
    }

    /* ICB flags: Allocation Descriptor type in bits 0-2
     * test also special short_ad/long_ad requirements for
     * UDF 2.50+ Metadata Partition.
     */
    adType = GET_ADTYPE(it->flags);
    if( adType == ADT_EXTENDED || adType > ADT_MAX )
    {
        MLIMITbegin( ERROR00level, MLIMITdefault20 );
            countMessPrinted++;
            printMessageHead(MLIMIT.vl, d, (Byte*)&it->flags, name);
            fprintf(uctout,
                   " error: flags #%02X, illegal Allocation Descriptor type %u,\n"
              "-\t\t UDF 2.3.10, ECMA 4/14.6.8\n",
                    it->flags, adType);
        MLIMITend;
        result = FALSE;
    }
    else if( IS_PREF_PARTITION_FOUND(mc->metadataPref) )    /* Metadata Partition */
    {   bool adTypeOk =
            (   adType == ADT_INFE                          /* always ok */
             || intFileType == (int)FT_METADATA_BITMAP_FILE /* always ok (omission ??) */
             || (   adType == ADT_SHORT
                 && (   intFileType == (int)FT_DIRECTORY
                     || intFileType == (int)FT_STREAM_DIRECTORY
                     || intFileType == (int)FT_METADATA_FILE
                     || intFileType == (int)FT_METADATA_MIRROR_FILE))
             || (   adType == ADT_LONG
                 && intFileType != (int)FT_DIRECTORY
                 && intFileType != (int)FT_STREAM_DIRECTORY
                 && intFileType != (int)FT_METADATA_FILE
                 && intFileType != (int)FT_METADATA_MIRROR_FILE) );

        if( !adTypeOk )
        { MLIMITbegin(ERROR00level, MLIMITdefault10);
            countMessPrinted++;
            printMessageHead(MLIMIT.vl, d,
                             (Byte*) &it->flags, name);
            fprintf(uctout,
              " error: flags #%02X, illegal AD type %u (%s)\n"
                "-\t\t\tfor file type %lu (%s)%s,\n"
                "-\t\t\tUDF 2.2.13, 2.2.13.1, ECMA 4/14.6.8.\n",
                it->flags, adType,
                (adType == ADT_SHORT) ? "short_ad" :
                (adType == ADT_LONG)  ? "long_ad"  : "embedded",
                intFileType,
                (intFileType == (int)FT_METADATA_FILE)
                    ? "Metadata File" :
                (intFileType == (int)FT_METADATA_MIRROR_FILE)
                    ? "Metadata Mirror File"
                    :  FT_TEXT4((Uint8)intFileType),
                (   intFileType == (int)FT_METADATA_FILE
                 || intFileType == (int)FT_METADATA_MIRROR_FILE)
                    ? "" :  " in a Metadata Partition");
          MLIMITend;
          /** no FALSE return result **/
        }
        else if(   adType == ADT_LONG        /* note for Metadata Bitmap File */
                && intFileType == (int)FT_METADATA_BITMAP_FILE )  /* File+long_ad */
        { MLIMITbegin(INFO01level, MLIMITdefault02);
            /** countMessPrinted++; **/ /** no extra ICBTAG info for this note **/
            printMessageHead(MLIMIT.vl, d,
                             (Byte*) &it->flags, name);
            fprintf(uctout,
                    " note: flags #%02X, AD type %u (long_ad) for Metadata Bitmap\n"
              "-\t\t\t      File. Short_ad could be used as for Metadata File.\n",
                  it->flags, ADT_LONG );
          MLIMITend;
        }
    }

    /* ICB flags: single bit flags
     * UDF 2.3.5.4.
     */
    if( isBitOn(it->flags, ICBF_SORTED_BIT) )
    {
        MLIMITbegin( ERROR00level, MLIMITdefault20 );
            countMessPrinted++;
            printMessageHead(MLIMIT.vl, d,
                             (Byte*) &it->flags, name);
            fprintf(uctout,
              " error: flags: #%02X, bit %2u set (SORTED)\n",
                    it->flags, ICBF_SORTED_BIT);
        MLIMITend;
        /** no FALSE return result **/
    }
    if( isBitOn(it->flags, ICBF_NONRELOCATABLE_BIT) )
    {
        MLIMITbegin(WARN01level, MLIMITdefault02);
            countMessPrinted++;
            printMessageHead(MLIMIT.vl, d,
                             (Byte*) &it->flags, name);
            fprintf(uctout,
              " warning: flags: #%02X, bit %2u set (NON-RELOCATABLE)\n",
                    it->flags, ICBF_NONRELOCATABLE_BIT);
        MLIMITend;
        /** no FALSE return result **/
    }
    if( isBitOn(it->flags, ICBF_CONTIGUOUS_BIT) )
    {
        MLIMITbegin(WARN01level, MLIMITdefault02);
            countMessPrinted++;
            printMessageHead(MLIMIT.vl, d,
                             (Byte*) &it->flags, name);
            fprintf(uctout,
              " warning: flags: #%02X, bit %2u set (CONTIGUOUS)\n",
                    it->flags, ICBF_CONTIGUOUS_BIT);
        MLIMITend;
        /** no FALSE return result **/
    }
    if( isBitOn(it->flags, ICBF_TRANSFORMED_BIT) )
    {
        MLIMITbegin( ERROR00level, MLIMITdefault20 );
            countMessPrinted++;
            printMessageHead(MLIMIT.vl, d,
                             (Byte*) &it->flags, name);
            fprintf(uctout,
              " error: flags: #%02X, bit %2u set (TRANSFORMED)\n",
                    it->flags, ICBF_TRANSFORMED_BIT);
        MLIMITend;
        /** no FALSE return result **/
    }
    if( isBitOn(it->flags, ICBF_MULTIVERSIONS_BIT) )
    {
        MLIMITbegin( ERROR00level, MLIMITdefault20 );
            countMessPrinted++;
            printMessageHead(MLIMIT.vl, d,
                             (Byte*) &it->flags, name);
            fprintf(uctout,
              " error: flags: #%02X, bit %2u set (MULTI-VERSIONS)\n",
                    it->flags, ICBF_MULTIVERSIONS_BIT);
        MLIMITend;
        /** no FALSE return result **/
    }

    /* Test if Stream bit is set to ONE for a Named stream (E)FE.
     * NOTE: see DCN-5152 :
     *  The Stream bit for a main stream (E)FE must be set to ZERO !!!
     *  Note 24 of ECMA 4.14.6.8 might suggest otherwise because the
     *  main stream (E)FE is referenced by the Parent FID in the
     *  Stream directory, but this explicitely clarified in DCN-5152
     *  (for UDF revisions after 2.60 and as errata for UDF 2.00-2.60).
     * This was always correctly implemented in the UDF verifier as such.
     */
    if( isStream != isBitOn(it->flags, ICBF_STREAM_BIT) )
    {
        MLIMITbegin( ERROR00level, MLIMITdefault20 );
            countMessPrinted++;
            printMessageExpectedU32( MLIMIT.vl, d, (Byte*) &it->flags,
                "icbtag error: Flags Stream bit", "%u",
                isBitOn(it->flags, ICBF_STREAM_BIT),
                (isStream) ? 1 : 0,
                (udfRev > 0x260) ? ", UDF 2.3.5.4.\n"
                                 : ", see DCN-5152.\n" );
        MLIMITend;
    }
    if( isBitOn(it->flags, ICBF_STREAM_BIT) )   /* Stream bit set */
    {
        if( udfRev < 0x200 )    /* streams and streamdir not allowed */
        {
            MLIMITbegin( ERROR00level, MLIMITdefault20 );
                countMessPrinted++;
                printMessageHead(MLIMIT.vl, d,
                                 (Byte*) &it->flags, name);
                fprintf(uctout,
                  " error: flags: #%02X, bit %2u set (STREAM)\n"
                  "-\t\t Illegal for current UDF revision range: ",
                        it->flags, ICBF_STREAM_BIT);
                printUdfRevisionRange(MLIMIT.vl, "\n");
            MLIMITend;
            /** no FALSE return result **/
        }
        (void) modifyUdfRevisionRange(0x200, MAX_UDFREVISION,
                                      "stream found");
        udfRev = getUctUdfRevision();   /* maybe modified */
    }

    if( (it->flags & ICBF_RESERVED_MASK) != 0 )
    {
        MLIMITbegin( ERROR00level, MLIMITdefault20 );
            countMessPrinted++;
            printMessageHead(MLIMIT.vl, d,
                             (Byte*) &it->flags, name);
            fprintf(uctout, "error: flags: #%02X, reserved bit set\n",
                                it->flags);
        MLIMITend;
        /** no FALSE return result **/
    }

    if( countMessPrinted != 0 )     /* ICBTag message printed */
    {       /* continuation line, NO MLIMIT... macros here !! */
        VERBOSE00(uctout,              /* general icbtag refs */
          "-\t\t icbtag info: ECMA 4/14.6.*, 4/A.5, UDF 2.3.5.*, 6.6.\n");
        nodePrintUnicodeNameContLine(node,mc);
    }
    return result;

}   /* end verifyICBTag() */


/* ***************** UniqueID stuff ***************************
 *
 * print HEX UniqueID in 17 char positions
 * if( isFid ) then "        #HHHHHHHH" (Uint32)
 * else             "#HHHHHHHHHHHHHHHH" (Uint64)
 */
extern void printHexUniqueId17Chars(Uint64 uniqueID,
                                    bool   isFid)
{   if( isFid )
    {   fprintf(uctout, "%8s#%08lX",    /* FID, 8 spaces */
                        "", (Uint32) uniqueID);
    }
    else
    {   printUint64(VERBOSE00level, uniqueID, TRUE, NULL);
    }
}

/* maintain max FID and FE UniqueID values
 */
static void keepMaxUniqueIdHistory(
                    Uint64 uniqueID, bool isFid,
                    Node  *node, UdfMountContext *mc )
{
    if( isFid )     /* FID */
    { mc->maxFidUniqueID = MAX((Uint32) uniqueID,
                               mc->maxFidUniqueID);
    }
    else            /* (E)FE, exclude 'the' VAT for max */
    { if( node != mc->vatNode )
      { mc->maxFeUniqueID = MAX(uniqueID, mc->maxFeUniqueID);
      }
    }
}

/* incrementUniqueId(): according to UDF 3.2.1.1
 */
extern Uint64 incrementUniqueId(Uint64 uniqueID)
{
    uniqueID++;     /* UDF 3.2.1.1 */
    while( UNIQUEID_LOW32_LESS_THAN_16(uniqueID) )
    { uniqueID++;
    }
    return uniqueID;
}

/* verifyTimeAndSetNextUniqueId():
 * Verify medium modification time (LVID or VAT)..
 * ECMA 3/8.8.2, 3/10.10.2, UDF 2.2.6".
 * Verify and set mc Next UniqueID values
 * ECMA 4/14.15.1,
 * UDF 2.00+: 2.3.4.3, 2.3.6.7, 3.2.1.1, 3.3.3.4.
 * UDF 1.50-: 2.3.6.5, 3.3.3.4.
 */
extern bool verifyTimeAndSetNextUniqueId(Timestamp *pVerifyStartTime,
                                         UdfMountContext *mc)
{
    char      *errTxt, *timeTxt;
    bool       fromVat = FALSE;
    Byte      *descr = NULL;
    Uint16     udfRevision = getUctUdfRevision();
    Timestamp *pLastModTime = NULL;

    mc->nextUniqueIdStatus = NEXTUNIQUEID_UNDEFINED;
    mc->nextUniqueId = (Uint64) 0;
    errTxt = timeTxt = NULL;            /* means no error */

    if( mc->vatNode != NULL )
    { fromVat = TRUE;
      timeTxt = "VAT Modification Time";
      if( mc->vatNode->fe != NULL ) /* increment VAT FE UniqueID */
      { descr = (Byte*) mc->vatNode->fe;
        pLastModTime     = pFE_modificationTime(mc->vatNode->fe);
        mc->nextUniqueId = incrementUniqueId(
                                *(pFE_uniqueID(mc->vatNode->fe)));
      }
      else errTxt = "VAT File Entry error";
    }
    else if( mc->lvid != NULL )
    { fromVat = FALSE;          /* from LVID, no VAT */
      /* Ignore Open LVID. Handle dirty volume as Close
       * in order to show all inconsistencies.
       */
      descr     = (Byte*) mc->lvid;
      pLastModTime     = &mc->lvid->recordingDateAndTime;
      timeTxt = "LVID Recording Time";
      mc->nextUniqueId =
        mc->lvid->logicalVolumeContentsUse.logicalVolumeHeaderDescriptor.uniqueID;
    }
    else
    { errTxt = "No LVID or VAT FE found";
    }

    if( errTxt != NULL )
    {   MLIMITbegin( ERROR00level, uctMessageLimit );
          fprintf(uctout,
             "\tError: %s. Medium Last\n"
            "-\t       Modification Time and NextUniqueID undefined.\n",
                        errTxt);
        MLIMITend;
        return FALSE;   /* error, status NEXTUNIQUEID_UNDEFINED */
    }

    /* no error so far,
     * check if VAT/LVID time in-the-future.
     */
    if( !verifyTimestampRelation(
            pVerifyStartTime, "Verification start time",
            pLastModTime, timeTxt,
            TRUE, NULL, descr,          /* isError */
            (Byte*)pLastModTime, mc, NULL) )
    { VERBOSE00(uctout,
          "-\t  %s is in the future.\n"
        "%s-\t  ECMA 3/8.8.2, 3/10.10.2, UDF 2.2.6.\n"
          "-\tNote: There may be more 'in-the-future' timestamps on\n"
          "-\t      this volume that will not be tested on this point.\n",
                timeTxt, (fromVat)
        ? "-\t  For a sequential file system, the VAT Modification Time\n"
          "-\t  is the replacement for the LVID Recording Time.\n"
        : "");
    }

    /* change NEXTUNIQUEID_UNDEFINED status.
     */
    if( mc->nextUniqueId > (Uint64) MAX_UINT32 )
         mc->nextUniqueIdStatus = NEXTUNIQUEID_WRAP32;
    else mc->nextUniqueIdStatus = NEXTUNIQUEID_NO_WRAP32;

    VERBOSE00(uctout, "\n\tNext UniqueID: ");
    printHexUniqueId17Chars(mc->nextUniqueId, FALSE);
    VERBOSE00(uctout, ",\n\tfrom %s.\n\n",
        (fromVat) ? "incremented VAT FE UniqueID"
                  : "LVID Logical Volume Header Descriptor");

    /* values 1-15 reserved in lower 32 bits.
     */
    if( UNIQUEID_LOW32_LESS_THAN_16(mc->nextUniqueId) )
    { MLIMITbegin( ERROR00level, uctMessageLimit );
        fprintf(uctout,
           "\tError: Next UniqueID lower 32 bits value"
                                " less than 16,\n"
          "-\t       UDF %s.\n",
          (udfRevision >= 0x200)
            ? "3.2.1.1"
            : "2.3.6.5, 3.3.3.4");
      MLIMITend;
    }

    /* in UDF 2.00, a 32-bit FID UniqueID was introduced.
     */
    if(   udfRevision >= 0x200
       && mc->nextUniqueIdStatus == NEXTUNIQUEID_WRAP32 )
    { MLIMITbegin( ERROR00level, uctMessageLimit );
        fprintf(uctout,
              "\tError: Next UniqueID higher than ");
        printUint64(MLIMIT.vl, (Uint64) MAX_UINT32, TRUE, NULL);
        fprintf(uctout,
          ".\n-\t       Uniqueness of 32-bit UDF UniqueID in a FID\n"
             "-\t       cannot be guaranteed, UDF 2.3.4.3, 3.2.1.1.\n"
             "-\tNote: Therefore the verifier is unable to test the\n"
             "-\t      relation between a FID UniqueID and its FE UniqueID.\n");
      MLIMITend;
    }

    /* warn for 31 bit wrap (Macintosh OS)
     */
    if( mc->nextUniqueId > (Uint64) MAX_INT32 )             /* 31, INT !! */
    { MLIMITbegin( WARN01level, uctMessageLimit );
        fprintf(uctout,
            "\tWarning: Next UniqueID greater than ");
        printUint64(MLIMIT.vl, (Uint64) MAX_INT32, TRUE, NULL);  /* INT !! */
        fprintf(uctout,                         ". Out of\n"
           "-\t         range for Macintosh OS UniqueID,"
                                        " UDF %s, 3.3.3.4.\n",
            (udfRevision >= 0x200) ? "3.2.1.1"
                                   : "2.3.6.5");
      MLIMITend;
    }
    return TRUE;

}   /* end verifyTimeAndSetNextUniqueId() */

/* printFidFeUniqueIdMessage():
 * print ERROR00level message for verifyFidFeUniqueIdConsistency().
 * This function is called from within a MLIMITbegin/MLIMITend
 * clause in order to enable separate error counting for the
 * two different error sub-types (isParentFid or it is not).
 */
static void printFidFeUniqueIdMessage( Node *fidNode,
                                       Node *feNode,
                                       UdfMountContext *mc )
{
    Uint16  feTagId  =((Tag*)feNode->fe)->tagIdentifier;
    Uint32 *pFidID32 =
              &fidNode->fid->ICB.implementationUse.ImpUse.UDFUniqueID;
    Uint64 *pFeID = (Uint64*) pFE_uniqueID(feNode->fe);
    bool    isParentFid = NODEFLAGS_IS_SET(fidNode, NFB_PARENTFID);

    /* so no MLIMITbegin/MLIMTend clause here
     */
    printMessageHead(ERROR00level, feNode->fe, (Byte*)pFeID,
            "Error: UniqueID: ");                   /* (E)FE */
    printHexUniqueId17Chars((*pFeID), FALSE);
    fprintf(uctout, "\n-");
    printMessageHead(ERROR00level, (Byte*) fidNode->fid,
                                   (Byte*) pFidID32,
                         "   UDF UniqueID: ");
    printHexUniqueId17Chars((Uint64) (*pFidID32), TRUE);
    if( isParentFid )
    { fprintf(uctout,
        "\n-\t\t Parent FID takes its UDF UniqueID from the %s of\n"
          "-\t\t the parent directory", tidTEXT4(feTagId));
    }
    else
    { fprintf(uctout,
        "\n-\t\t FID UDF UniqueID shall be equal to or greater\n"
          "-\t\t than %s UniqueID", tidTEXT4(feTagId));
    }
    fprintf(uctout, ", UDF 2.3.4.3, 3.2.1.1.\n");
    nodePrintUnicodeNameContLine(fidNode,mc);

}   /* end printFidFeUniqueIdMessage() */

/* Check consistency between 32 bits FID UDF UniqueId and the
 * 64-bit UniqueID of the (E)FE identified by the FID ICB field.
 * FID UDF UniqueID was introduced in UDF 2.00
 * Normally: FID ID is at least FE ID (UDF 3.2.1.1) because the
 * FE UniqueID is never changed after creation.
 * However, if  mc->nextUniqueId is greater than MAX_UINT32,
 * the comparison cannot be made.
 * FID UDF UniqueID was introduced in UDF 2.00.
 * For a parent FID, fidNode and feNode can be different nodes.
 */
static bool verifyFidFeUniqueIdConsistency(Node *fidNode,
                                           Node *feNode,
                                           UdfMountContext *mc)
{   bool   isParentFid, result = TRUE;
    Uint64 feID;
    Uint32 fidID32;

    if(   getUctUdfRevision() < 0x200   /* no FID UniqueID */
       || fidNode->fid == NULL
       || feNode->fe   == NULL )
    {   return TRUE;
    }
    feID    = ((Uint64)*pFE_uniqueID(feNode->fe));
    fidID32 = fidNode->fid->ICB.implementationUse.ImpUse.UDFUniqueID;
    isParentFid = NODEFLAGS_IS_SET(fidNode, NFB_PARENTFID);

    if(   (   isParentFid && feID != (Uint64) fidID32 )
       || (   mc->nextUniqueIdStatus == NEXTUNIQUEID_NO_WRAP32
           && feID > (Uint64) fidID32 ) )
    { /* Separate error counting for parent FID and other FID,
       * so two MLIMITbegin/MLIMITend clauses.
       */
      if( isParentFid )
      { MLIMITbegin(ERROR00level, MLIMITdefault10);
          printFidFeUniqueIdMessage( fidNode,  feNode, mc );
        MLIMITend;
      }
      else      /* no parent FID */
      { MLIMITbegin(ERROR00level, MLIMITdefault10);
          printFidFeUniqueIdMessage( fidNode,  feNode, mc );
        MLIMITend;
      }
      result = FALSE;
    }
    return result;

}   /* end verifyFidFeUniqueIdConsistency() */

/* Verify FID or (E)FE UniqueID and keep UniqueID history
 * using keepMaxUniqueIdHistory().
 * Node context in node and mc must be set for context verification.
 * Note: The UniqueID for the system stream Directory and all
 *       its associated streams is not (yet) defined by ECMA or UDF,
 *       ECMA says that it must be less than Next UniqueID, that's all.
 * UDF 1.50-:          2.3.6.5, 3.2.1.1
 * UDF 2.00+: 2.3.4.3, 2.3.6.7, 3.2.1.1
 * ECMA 4/14.15.1
 */
static bool verifyFidOrFeUniqueID(Byte *d, Node *node,
                                  UdfMountContext *mc)
{
    Uint16  tagId;
    Uint16  udfRevision = getUctUdfRevision();
    Uint64  uniqueID;
    Byte   *pID;                    /* points in descriptor !! */
    Node   *mainNode, *pfMainNode;
    bool    isFID, result = TRUE;
    char   *udfRef1 = (udfRevision <= 0x150)
                        ? "2.3.6.5" : "2.3.6.7";
    char   *itemText =
          (NODEFLAGS_IS_SET(node, NFB_PARENTFID)) ? "parent FID"
        : (NODEFLAGS_IS_SET(node, NFB_EA))        ? "EA file"
        : (NODEFLAGS_IS_SET(node, NFB_VAT))       ? "VAT file"
        : (NODEFLAGS_IS_SET(node, NFB_DIREXPAND)) ? "directory"
        : (NODEFLAGS_IS_SET(node, NFB_STREAMDIR)) ? "stream directory"
        : (NODEFLAGS_IS_SET(node, NFB_STREAM))    ? "named stream"
                                                  : "file";

    UCTASSERT( d != NULL && node != NULL && mc != NULL );

    tagId = ((Tag*)d)->tagIdentifier;
    isFID = (tagId == tidFID);

    UCTASSERT(   ( isFID || node->fe  != NULL)
              && (!isFID || node->fid != NULL) );

    /* FID UDF UniqueID was introduced in UDF 2.00
     */
    if( isFID && udfRevision < 0x200 )
    {   return TRUE;    /* FID has no UniqueID */
    }

    /* So continue here for: FE || (FID && (UDF 2.00+))
     * Uint64 for FE UniqueID, but
     * Uint32 for FID UDF UniqueID,
     * cast both to Uint64.
     * save max value of both FE and FID UniqueID.
     */
    if( isFID )     /* FID */
    { pID = (Byte*) &node->fid->ICB.implementationUse.ImpUse.UDFUniqueID;
      uniqueID = (Uint64) (*((Uint32*) pID));
    }
    else            /* (E)FE */
    { pID = (Byte*) pFE_uniqueID(node->fe);
      uniqueID = (*((Uint64*) pID));
    }
    keepMaxUniqueIdHistory(uniqueID, isFID, node, mc);

    /* determine mainNode and pfMainNode
     * mainNode  : 'main stream' node of association.
     * pfMainNode: node identified by a parent FID
     */
    mainNode   = nodeGetMainNode(node);
    pfMainNode = NULL;
    if( NODEFLAGS_IS_SET(node, NFB_PARENTFID) )
    {   UCTASSERT(   node->parent != NULL
                  && node->parent->parent != NULL);
        pfMainNode = node->parent->parent;
    }
    UCTASSERT( mainNode != NULL );

    /* First exit for all special cases where
     * UniqueID zero is allowed.
     * For legal zero cases, a 'return TRUE' is done
     * in order to avoid further tests.
     */
    if( uniqueID == (Uint64) 0 )
    {   /* FID or FE associated with RootDir or SysStreamDir
         * or parent FID pointing to RootDir or SysStreamDir
         */
        if(   NODEFLAGS_IS_ROOTDIR(          mainNode)
           || NODEFLAGS_IS_SYSTEMSTREAMDIR(  mainNode)
           || NODEFLAGS_IS_ROOTDIR(        pfMainNode)
           || NODEFLAGS_IS_SYSTEMSTREAMDIR(pfMainNode)
           || NODEFLAGS_IS_SET(mainNode, NFB_METADATAFILE)
           || NODEFLAGS_IS_SET(mainNode, NFB_METADATAMIRROR)
           || NODEFLAGS_IS_SET(mainNode, NFB_METADATABITMAP) )
        {   return TRUE;    /* done, no further checks */
        }
        /* warning for other stream or streamDir
         * FID or (E)FE for UDF 2.00 only.
         */
        if(   udfRevision == 0x200
           && (   NODEFLAGS_IS_SET(node, NFB_STREAM)
               || NODEFLAGS_IS_SET(node, NFB_STREAMDIR)) )
        { /* FID or FE not associated with RootDir or SysStreamDir.
           * In UDF 2.00, the rule for a Stream or Stream Directory
           * were inconsistent. Should be equal to the mainNode UniqueID
           * but maybe zero as well because of error in UDF 2.00 3.3.5.1.
           */
          MLIMITbegin(WARN01level, MLIMITdefault02);
            printMessageHead(MLIMIT.vl, d, pID,
                    "Warning: UniqueID zero for stream or stream directory.\n"
              "-\t\t This may be due to an error in UDF 2.00 3.3.5.1.\n"
              "-\t\t Stream or stream directory FIDs and (E)FEs shall\n"
              "-\t\t take their UniqueID from the (E)FE of the file\n"
              "-\t\t or directory they are associated with, as defined\n"
              "-\t\t in UDF 2.00 3.2.1.1. UDF 2.01 is corrected\n"
              "-\t\t accordingly, see UDF 2.01 3.2.1.1, 3.3.5.1.\n");
            nodePrintUnicodeNameContLine(node,mc);
          MLIMITend;
          return TRUE;      /* done, no further checks */
        }
    }

    /* if now still (UniqueID == 0), it is illegal
     *
     * Handle special cases for nodes associated with the
     * root directory or the system stream directory.
     */
    if( NODEFLAGS_IS_SYSTEMSTREAMDIR(mainNode) )
    {   /* uniqueID != 0
         * Zero UniqueID required (or recommended) for all
         * nodes associated with SysStreamDirNode.
         */
        if( udfRevision > 0x201 )
        {   MLIMITbegin(ERROR00level, MLIMITdefault20);
              printMessageHead(MLIMIT.vl, d, pID,
                        "Error: UniqueID: ");
              printHexUniqueId17Chars(uniqueID, isFID);
              fprintf(uctout,
                "\n-\t\t Shall be zero for %s associated with\n"
                  "-\t\t the System Stream Directory, UDF 3.2.1.1.\n",
                            itemText);
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
            result = FALSE;
        }
        else        /* (UDF 2.00 or 2.01) and uniqueID != 0 */
        {   MLIMITbegin(WARN01level, MLIMITdefault02);
              printMessageHead(MLIMIT.vl, d, pID,
                        "Warning: UniqueID: ");
              printHexUniqueId17Chars(uniqueID, isFID);
              fprintf(uctout,
                "\n-\t\t Recommending zero for %s associated with\n"
                  "-\t\t the System Stream Directory.\n",
                            itemText);
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
           /* no FALSE return result */
        }
    }
    else if( NODEFLAGS_IS_ROOTDIR(mainNode) )
    {   /* uniqueID != 0
         * Zero UniqueID required for all
         * nodes associated with the Root node.
         */
        MLIMITbegin(ERROR00level, MLIMITdefault20);
          printMessageHead(MLIMIT.vl, d, pID,
                    "Error: UniqueID: ");
          printHexUniqueId17Chars(uniqueID, isFID);
          fprintf(uctout,
            "\n-\t\t Shall be zero for %s associated with the\n"
              "-\t\t root directory, UDF %s, 3.2.1.1.\n",
                            itemText, udfRef1);
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        result = FALSE;
    }
    else if( node != mainNode ) /* EA, Stream or StreamDir */
    {   /* No parent FID and no association with RootDir or SysStreamDir.
         * UniqueID shall be same as main node FE uniqueID
         */
        Uint64 mainUniqueID = *(pFE_uniqueID(mainNode->fe));
        Uint16 mainFeTagId  = ((Tag*)mainNode->fe)->tagIdentifier;

        if(   ( isFID && uniqueID != (Uint32) mainUniqueID)
           || (!isFID && uniqueID !=          mainUniqueID) )
        { MLIMITbegin(ERROR00level, MLIMITdefault20);
            printMessageHead(MLIMIT.vl, d, (Byte*)pID,
                "Error:   UniqueID: ");
            printHexUniqueId17Chars(uniqueID, isFID);
            fprintf(uctout, ",\n-\t\t Main %-3s UniqueID: ",
                                tidTEXT4(mainFeTagId));
            printHexUniqueId17Chars(mainUniqueID, FALSE);
            fprintf(uctout,
              "\n-\t\t EA files, streams and stream directories shall\n"
                "-\t\t take their UniqueID from the %s of the file or\n"
                "-\t\t directory they are associated with, UDF %s.\n",
                tidTEXT4(mainFeTagId),
                (udfRevision >= 0x200) ? "3.2.1.1" : "2.3.6.5");
            nodePrintUnicodeNameContLine(node,mc);
            nodePrintUnicodeNameContLine(mainNode,mc);
          MLIMITend;
          result = FALSE;
        }
    }
    else if(   mc->nextUniqueIdStatus != NEXTUNIQUEID_UNDEFINED
            && uniqueID >= mc->nextUniqueId )
    {
      /* node == mainNode (includes parent FID )
       * All FEs and FIDs shall have UniqueID with
       * a value less than 'nextUniqueID',
       * UDF 3.2.1.1, ECMA 4/14.15.1.
       */
      MLIMITbegin(ERROR00level, MLIMITdefault20);
        printMessageHead(MLIMIT.vl, d, pID,
                    "Error: UniqueID: ");
        printHexUniqueId17Chars(uniqueID, isFID);
        fprintf(uctout,
            "\n-\t\t   Next UniqueID: ");
        printHexUniqueId17Chars(mc->nextUniqueId, FALSE);
        fprintf(uctout,
          "\n-\t\t %s UniqueID shall be less than Next UniqueID,\n"
            "-\t\t UDF 3.2.1.1, ECMA 4/14.15.1.\n",
                    tidTEXT4(tagId));
        nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
      result = FALSE;
    }

    /* now check some rules valid for all UniqueID values.
     * Mind that this test is not done for legal zero values
     * for which a 'return TRUE' is done already above.
     */
    if( UNIQUEID_LOW32_LESS_THAN_16(uniqueID) ) /* test 32 LSBits only */
    { MLIMITbegin(ERROR00level, MLIMITdefault20);
        printMessageHead(MLIMIT.vl, d, pID,
                "Error: UniqueID: ");
        printHexUniqueId17Chars(uniqueID, isFID);
        fprintf(uctout,
          "\n-\t\t\tBits 0-31 have a value lower than 16,\n"
            "-\t\t\tUDF %s3.2.1.1, 3.3.3.4.\n",
                        (isFID) ? "2.3.4.3, " : "");
        if( uniqueID == 0 )
        {   fprintf(uctout,
              "-\t\t No association with the Root Directory or\n"
              "-\t\t the System Stream Directory, UDF %s.\n",
                            udfRef1);
            /* TODO: there is no reference yet for the
             *       System Stream Directory, recommandation only
             *       for UDF 2.00 and 2.01.
             */
        }
        nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
      result = FALSE;
    }

    return result;

}   /* end verifyFidOrFeUniqueID() */


/* Now verify functions for all descriptor tails,
 * which is the part after the descriptor tag.
 */

/* Precondition for all following verify<descriptor>() functions :
 *  It is assumed that the descriptor is swapped to the correct endian
 *  and that the tag is checked already.
 */

#define PRINTSPARINGMAP_MAX_ENTRIES 100 /* mult of 2 value */

static void printSparingMap(SparingEntry *spm, Uint16 nmbEnt,
                            Uint16 spPacketLength)
{   Uint32 n;

    VERBOSE00(uctout,
        "\n\t  Sparing Map entries   : %lu\n"
          "\t  Sparing Packet Length : %lu\n",
            nmbEnt, spPacketLength);
    if( nmbEnt > PRINTSPARINGMAP_MAX_ENTRIES )
    {
        ifPRINTinfo02(uctout,
            "\t(only first %u and last %u entries are printed)\n",
            PRINTSPARINGMAP_MAX_ENTRIES / 2,
            PRINTSPARINGMAP_MAX_ENTRIES / 2);
        ENDif;
    }
    if( nmbEnt != 0 )
    {
        ifPRINTinfo02(uctout,
            "\t     Original Location -> Mapped Location\n");
        ENDif;
    }
    for( n = 0; n < (Uint32) nmbEnt; n++, spm++ )
    {
        if(    nmbEnt > PRINTSPARINGMAP_MAX_ENTRIES
            && n == (PRINTSPARINGMAP_MAX_ENTRIES / 2) )
        {   /* do not print more than PRINTSPARINGMAP_MAX_ENTRIES
             * entries, skip middle ones.
             */
            n   += nmbEnt - PRINTSPARINGMAP_MAX_ENTRIES - 1;
            spm += nmbEnt - PRINTSPARINGMAP_MAX_ENTRIES - 1;
            ifPRINTinfo02(uctout, "\t...\n");
            ENDif;
        }
        else        /* normal print Entry */
        {
            ifPRINTinfo02(uctout, "\t#%08lX = %10lu -> %10lu\n",
                spm->originalLocation, spm->originalLocation,
                spm->mappedLocation);
            ENDif;
        }
    }
    VERBOSE00(uctout, "\n");
}


/* qsortMappedLocations():
 * qsort compare function for SparingEntry
 * mappedLocation compare.
 * In fact a Uint16 sortArray containing the
 * SparingEntry indexes is sorted.
 * No 2 values in this sortArray are equal.
 * qsortSE must be initialized before each call to
 * qsort using qsortMappedLocations();
 */
static SparingEntry *qsortSE = NULL;

static int qsortMappedLocations(const void *elem1, const void *elem2)
{
    Uint16 u1 = *((Uint16 *) elem1),
           u2 = *((Uint16 *) elem2);

    if(        qsortSE[u1].mappedLocation >  qsortSE[u2].mappedLocation
        || (   qsortSE[u1].mappedLocation == qsortSE[u2].mappedLocation
            && u1 > u2) )
    {   return  QSORT_ELEM1_LAST;
    }
    return QSORT_ELEM1_FIRST;
}


static bool verifySparingMappedLocations(SparingEntry *spMap, Uint16 nmbEnt,
                                         Uint16 spPacketLength, Byte *d)
{   Uint32   mLoc, prevLoc = 0;
    Uint16  *sortArray, n, x0, x1;
    bool     isContiguous,
             result = TRUE;

    if( nmbEnt == 0 )
    {   return TRUE;        /* done */
    }

    if( (sortArray = (Uint16*) tst_malloc( nmbEnt * sizeof(Uint16),
                                           __FILE__,__LINE__)) == NULL )
    {
        uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    for( n = 0; n < nmbEnt; n++ )   /* initialize for qsortMappedLocations */
    {   sortArray[n] = n;
    }
    qsortSE = spMap;            /* initialize for qsortMappedLocations */

    qsort((void *)sortArray, nmbEnt, sizeof(Uint16), qsortMappedLocations);

    /* first add to Sparing Table statistics
     */
    isContiguous = TRUE;
    for( n = 0; n < nmbEnt; n++ )
    {
        mLoc = spMap[sortArray[n]].mappedLocation;
        if( n != 0 && mLoc != (prevLoc + spPacketLength) )
        {
            isContiguous = FALSE;
        }
        prevLoc = mLoc;
    }
    VERBOSE00(uctout,
        "\t Mapped Location area   : %lu thru %lu"
                                " : %scontiguous\n\n",
        spMap[sortArray[0]].mappedLocation,
        spMap[sortArray[nmbEnt-1]].mappedLocation
                                    + spPacketLength - 1,
        (isContiguous) ? "" : "non-");

    /* end of Sparing Table statistics
     * continue mappedLocation tests
     */
    for( n = 0; n < nmbEnt; n++ )
    {
        x1 = sortArray[n];
        mLoc = spMap[x1].mappedLocation;
        if(    n != 0
           && (mLoc / spPacketLength) == (prevLoc / spPacketLength) )
        {
            x0 = sortArray[n-1];
            MLIMITbegin(ERROR00level, uctMessageLimit);
                printMessageHead(VERBOSE00level, d,
                                 (Byte*) &spMap[x0], NULL);
                fprintf(uctout,
                    "error: Multiple defined Sparing"
                            " Mappped Location packet,\n"
                    "-\t\t entries nr %u and %u: %lu, %lu. %s\n",
                    x0+1, x1+1, prevLoc, mLoc,
                    UDFREF_SPTAB(getUctUdfRevision()) );
            MLIMITend;
            if(   spMap[x0].originalLocation < 0xFFFFFFF0
               || spMap[x1].originalLocation < 0xFFFFFFF0 )
            {   result = FALSE;     /* used entry, fatal error */
            }
        }
        prevLoc = mLoc;
    }

    free(sortArray);
    return result;

}   /* end verifySparingMappedLocations() */

static Uint16 countSortedSparingMapEntries(SparingEntry *spm, Uint16 nmbEnt)
{
    Uint16 n;
    Uint32 oLoc, prevLoc = 0;

    for( n = 0; n < nmbEnt; n++, spm++ )
    {
        oLoc = spm->originalLocation;
        if( n != 0 && oLoc < prevLoc )
        {
            return n;   /* not all sorted, n < nmbEnt */
        }
        prevLoc = oLoc;
    }
    return n;           /* all sorted, n == nmbEnt */
}

static bool verifySparingEntries(SparingEntry *spMap, Uint16 nmbEnt,
                                 Uint16 spPacketLength, Byte *d)
{
    Uint32   oLoc, prevLoc = 0;
    Uint32   n, cntFree, cntDefect1, cntDefect2;
    char    *error;
    bool     result = TRUE;
    SparingEntry *spm;

#ifdef UCT_TESTING
/**testing**/   spMap[1].originalLocation = spMap[0].originalLocation-1;
/**testing**/   spMap[3].originalLocation = spMap[2].originalLocation;
/**testing**/   spMap[4].originalLocation = 0xFFFFFFFF;
/**testing**/   spMap[5].originalLocation = 0xFFFFFFF0;
/**testing**/   spMap[6].originalLocation = 0xFFFFFFF1;
/**testing**/   spMap[31].originalLocation = 32;
/**testing**/
/**testing**/   spMap[3].mappedLocation   = 75;
/**testing**/   spMap[7].mappedLocation   = spMap[2].mappedLocation+1;
/**testing**/   spMap[11].mappedLocation  = spMap[2].mappedLocation+3;
/**testing**/
/**testing**/{  Uint32 sv;  /** swap places, still contiguous **/
/**testing**/   sv = spMap[9].mappedLocation;
/**testing**/   spMap[9].mappedLocation   = spMap[4].mappedLocation;
/**testing**/   spMap[4].mappedLocation   = spMap[13].mappedLocation;
/**testing**/   spMap[13].mappedLocation  = sv;
/**testing**/}
/**testing**/
/**testing**/if( nmbEnt >= 2 )                  /** testing patch   **/
/**testing**/{  Uint32 n = 0;                   /** for alex01.img  **/
/**testing**/   Uint32 defectBlock01 = 212032;  /** is abs 214496 **/
/**testing**/   Uint32 defectBlock02 = 252832;  /** is abs 255296 **/
/**testing**/   while( spMap[n].originalLocation < 0xFFFFFFF0 ) n++;
/**testing**/   if( n == 0 || defectBlock01 > spMap[n-1].originalLocation )
/**testing**/   {   spMap[n].originalLocation   = defectBlock01;
/**testing**/       spMap[n+1].originalLocation = defectBlock02;
/**testing**/   }
/**testing**/}
#endif  /** UCT_TESTING **/

/*** TODO: (plugfest) check sizeOfEachTable with real size of
 ***                  Sparing Table descriptor
 ***/

    /* informational output, dump Sparing Table, but
     * ONLY ONCE (relevant if multiple Sparing Tables ) !!
     */
    { static bool printOnce = FALSE;
        if( !printOnce ) printSparingMap(spMap, nmbEnt,
                                         spPacketLength);
        printOnce = TRUE;
    }

    /* check originalLocation sorting
     */
    n = 0;
    while( n < nmbEnt )     /* break on fatal error */
    {
        n += countSortedSparingMapEntries(&spMap[n], (Uint16) (nmbEnt - n));
        if( n != nmbEnt )
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            printMessageHead(MLIMIT.vl, d,
                (Byte*) &spMap[n-1], NULL);
            fprintf(uctout,
                "error: Sparing Map Original Locations not sorted,\n"
                "-\t\t\tentries nr %u and %u: %lu, %lu. %s\n",
                n, n+1, spMap[n-1].originalLocation,
                spMap[n].originalLocation,
                UDFREF_SPTAB(getUctUdfRevision()) );
          MLIMITend;
          if(   spMap[n-1].originalLocation < 0xFFFFFFF0
             ||   spMap[n].originalLocation < 0xFFFFFFF0 )
          { result = FALSE;     /* used entry, fatal error */
            break;              /* abort sort check */
          }
        }
    }

    cntFree = cntDefect1 = cntDefect2 = 0;
    for( n = 0, spm = spMap; n < nmbEnt; n++, spm++ )
    {
        /* special values for originalLocation:
         * #FFFFFFFF: entry available for sparing
         * #FFFFFFF0: mappedLocation marked defective
         * #FFFFFFF1-#FFFFFFFE : reserved
         */
        oLoc = spm->originalLocation;
        error = NULL;
        if( oLoc >= 0xFFFFFFF0 )        /* special value, check */
        {
            if(      oLoc == 0xFFFFFFFF ) cntFree++;
            else if( oLoc == 0xFFFFFFF0 ) cntDefect2++;
            else error = "Reserved value";
        }
        else
        {
            cntDefect1++;
            if( (oLoc % spPacketLength) != 0 )
            {   error = "No multiple of the packet length";
            }
            else if(    n != 0
                    && (oLoc / spPacketLength) == (prevLoc / spPacketLength) )
            {   error = "Ambiguous entry, previous one in same packet";
            }
        }
        if( error != NULL )
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            printMessageHead(MLIMIT.vl, d,
                                (Byte*) spm, NULL);
            fprintf(uctout,
                "error: Sparing Map entry nr %u"
                            " Original Location: %lu,\n"
                "-\t\t\t%s, %s\n", n+1, oLoc, error,
                    UDFREF_SPTAB(getUctUdfRevision()) );
          MLIMITend;
          error = NULL;
          if( oLoc < 0xFFFFFFF0 )
          { result = FALSE;     /* used entry, fatal error */
          }
        }

        if( (spm->mappedLocation % spPacketLength) != 0 )
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            printMessageHead(MLIMIT.vl, d,
                                (Byte*) spm, NULL);
            fprintf(uctout,
              "error: Sparing Map entry nr %u"
                            " Mapped Location: %lu,\n"
              "-\t\t\tNo multiple of the packet length, %s\n",
                n+1, spm->mappedLocation,
                UDFREF_SPTAB(getUctUdfRevision()) );
          MLIMITend;
          if( spm->originalLocation < 0xFFFFFFF0 )
          { result = FALSE;     /* used entry, fatal error */
            break;              /* abort sort check */
          }
        }
        prevLoc = oLoc;
    }

    if( nmbEnt > 0 )
    {   VERBOSE00(uctout,
          "\n\tSparing Table statistics:\n"
            "\t       Number of entries: %lu\n"
                  "\t\t    Used entries: %lu\n"
            "\t Mapped Location defects: %lu\n",
            nmbEnt, cntDefect1, cntDefect2);

        /** continue statistics in first part of
         ** verifySparingMappedLocations()
         **/
        if( !verifySparingMappedLocations(spMap, nmbEnt,
                                          spPacketLength, d) )
        {   result = FALSE;
        }
    }

    return result;

}   /* end verifySparingEntries() */


/* verifySparingTable(): UDF 2.2.12 (was 2.2.11)
 * A Sparing Table (ST) is a UDF descriptor introduced
 * in UDF 1.50, so there are NO ECMA references.
 * Location of ST in LVD by Sparable Partition Map.
 */
static bool verifySparingTable( SparingTable *st,
                                Uint16 spPacketLength )
{
    /* Sparing Table introduced in UDF 1.50,
     * void possible UDF revision conflict.
     */
    modifyUdfRevisionRange(0x150, MAX_UDFREVISION,
                           "Sparing Table");
    /* EntityID sparingIdentifier;
     */
    (void) verifyEntityID( &st->sparingIdentifier,
                           ENTITY_SUFFIX_UDF,
                           NULL, NULL, (Byte*)st);

    if( memcmp( st->sparingIdentifier.Identifier,
                SPARING_TABLE_ID, ENTITYID_IDSIZE) != 0 )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        printMessageHead(MLIMIT.vl, (Byte*)st,
            (Byte*) st->sparingIdentifier.Identifier, NULL);
        fprintf(uctout,
              "error: Sparing Identifier Identifier error\n"
                                  "-\t\t value: ");
        printBytesName((Byte*) st->sparingIdentifier.Identifier,
                            ENTITYID_IDSIZE, TRUE, VERBOSE00level);
        fprintf(uctout, "\n-\t      expected: ");
        printBytesName((Byte*) SPARING_TABLE_ID,
                            ENTITYID_IDSIZE, TRUE, VERBOSE00level);
        fprintf(uctout,
            "\n- ==>\tFatal error, assumed not to be a"
                                " Sparing Table after all.\n");
      MLIMITend;
      return FALSE;
    }

    /* Uint16 reallocationTableLength;
     */
    if( st->reallocationTableLength == 0 )
    { MLIMITbegin(WARN01level, MLIMITdefault01);
        printMessageHead(MLIMIT.vl, (Byte*)st,
            (Byte*)&st->reallocationTableLength, NULL);
        fprintf(uctout,
                "Warning: Reallocation Table Length zero. Empty\n"
          "-\t\t Sparing Mapping Table, the mapped locations\n"
          "-\t\t should be filled in at format time, %s.\n",
                UDFREF_SPTAB(getUctUdfRevision()) );
      MLIMITend;
    }

    /* Byte reserved[2];
     */
    if( !verifyZeros(st->reserved, sizeof(st->reserved),
                     NULL, NULL, NULL) )
    {   /* Note that verifyZerosInDescriptor() assumes to be
         * called from within a MLIMITbegin(ERROR00level, ...)
         * ... MLIMTend clause.
         */
        MLIMITbegin(ERROR00level,uctMessageLimit);
          (void) verifyZerosInDescriptor(
                            (Byte*) st, st->reserved,
                            sizeof(st->reserved), NULL);
          fprintf(uctout,
            "-\t\t Reserved field error, %s.\n",
                    UDFREF_SPTAB(getUctUdfRevision()));
        MLIMITend;
        /** no FALSE result **/
    }

    /* Uint32 sequenceNumber; Cannot think of any usefull test
     *
     * Byte   startOfMapEntries;
     */
    if( !verifySparingEntries(
                (SparingEntry *) &st->startOfMapEntries,
                                  st->reallocationTableLength,
                                  spPacketLength, (Byte*)st) )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
            "- ==>\tFatal error: in Sparing Table"
                            " \'in use\' Map Entry\n");
      MLIMITend;
/**/  return FALSE;     /* fatal, no sparing table ?? */
/**/
/**   return TRUE;      /* ignore sparing table error */
/**/
    }
    return TRUE;

}   /* end verifySparingTable() */

/* PVD Volume Set Identifier: UDF 2.2.2.5, ECMA 3/10.1.11.
 * Dstring volumeSetIdentifier[128]
 */
static bool verifyPvdVolSetId(PrimaryVolumeDescriptor *pvd)
{
    unicode_t uIdentifier[128-2];   /* max nmb of Unicode chars */
    Uint32 n, nchar;
    int    i;

    if( (i = uncompressDstring(128, &pvd->volumeSetIdentifier[0],
                               uIdentifier)) < 0 )
    {   VERBOSE00(uctout, "<uncompress Dstring error>");
        return FALSE;
    }
    nchar = (Uint32) i;     /* nchar >= 0 */

    for( n = 0; n < MIN(nchar,8); n++)
    { unicode_t uch = uIdentifier[n];
      if(   (uch < (unicode_t) '0' || uch > (unicode_t) '9')
         && (uch < (unicode_t) 'A' || uch > (unicode_t) 'F')
         && (uch < (unicode_t) 'a' || uch > (unicode_t) 'f') )
      { break;              /* non-hex char found */
      }
    }
    if( n < 8 )
    {   MLIMITbegin(WARN01level,uctMessageLimit);
          printMessageHead(MLIMIT.vl, (Byte*) pvd,
            (Byte*) pvd->volumeSetIdentifier, NULL);
          fprintf(uctout,
                  "Warning: Volume Set Identifier: ");
          printUnicodeName(uIdentifier, nchar, TRUE,    /* printTrailZeros */
                           FALSE, VERBOSE00level );     /* NOT isPath */
          fprintf(uctout,
            ",\n-\t\t expected: CS0 representation of unique hex number in\n"
                     "-\t\t\t   first 8 character positions, UDF 2.2.2.5.\n");
        MLIMITend;
    }
    return TRUE;

}   /* end verifyPvdVolSetId() */

/* Primary Volume Descriptor: ECMA 3/10.1. UDF 2.2.2.
 */
static bool verifyPrimaryVolumeDescriptor(PrimaryVolumeDescriptor *pvd,
                                          UdfMountContext *mc)
{
    bool result = TRUE;

    VERBOSE00(uctout,   "\tPVD   Recording Time: ");
    printTimestampShort(&pvd->recordingDateAndTime, TRUE, "\n");

    /* Uint32 volumeDescriptorSequenceNumber,
     *          used/verified when reading VDS,
     *          see also storePVDInUdfVolumeInformation().
     *
     * Uint32 primaryVolumeDescriptorNumber
     */
    if( pvd->primaryVolumeDescriptorNumber != 0 )
    {
      MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageExpectedU32( MLIMIT.vl, (Byte*)pvd,
            (Byte*) &pvd->primaryVolumeDescriptorNumber,
                   "Error: Primary Volume Descriptor Number","%lu",
                     pvd->primaryVolumeDescriptorNumber, 0,
          ".\n-\t\t\tThe default Primary Volume Descriptor shall\n"
             "-\t\t\thave a Primary Volume Descriptor Number of 0,\n"
             "-\t\t\tECMA 3/13.1, 3/14.1, 3/8.4.1, 3/10.1.3, UDF 2.\n");
      MLIMITend;
    }

    /* Dstring volumeIdentifier[32]
     */
    VERBOSE00(uctout, "\tPVD   Volume Identifier    : ");
    PRINTDSTRING(  pvd->volumeIdentifier,
            sizeof(pvd->volumeIdentifier), "\n");
    verifyDstring( pvd->volumeIdentifier, (Byte *)pvd,
            sizeof(pvd->volumeIdentifier),
            TRUE, "ECMA 3/10.1.4");     /* obligatory */

    /* Uint16 volumeSequenceNumber
     * Uint16 maximumVolumeSequenceNumber
     */
    if(       pvd->volumeSequenceNumber == 0
       || (   pvd->maximumVolumeSequenceNumber != 0
           && pvd->maximumVolumeSequenceNumber
            < pvd->volumeSequenceNumber) )
    {
      MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageHead(MLIMIT.vl, (Byte*) pvd,
            (Byte*) &pvd->volumeSequenceNumber, NULL);
        fprintf(uctout,
          "Error:     Volume Sequence Number: %lu  ==    VSN\n"
          "-\t\t\tMax Volume Sequence Number: %lu  == MaxVSN\n"
          "-\texpected: VSN > 0 AND ( MaxVSN == 0 OR MaxVSN >= VSN ),\n"
                "-\t\t  ECMA 3/8.6, 3/10.1.5+6.\n",
                pvd->volumeSequenceNumber,
                pvd->maximumVolumeSequenceNumber);
      MLIMITend;
    }
    if(   pvd->volumeSequenceNumber > 1
       || pvd->maximumVolumeSequenceNumber > 1 )
    {
      MLIMITbegin(WARN01level,uctMessageLimit);
        printMessageHead(MLIMIT.vl, (Byte*) pvd,
            (Byte*) &pvd->volumeSequenceNumber, NULL);
        fprintf(uctout,
          "Warning:     Volume Sequence Number: %lu and\n"
          "-\t\t\t  Max Volume Sequence Number: %lu.\n"
          "- ==>\tMulti-volume Volume Sets not supported,"
                                    " ECMA 3/10.1.5+6.\n",
            pvd->volumeSequenceNumber,
            pvd->maximumVolumeSequenceNumber);
      MLIMITend;
      /* not FATAL, let's see what happens
       */
    }

    /* Uint16 interchangeLevel             UDF 2.2.2.1+2, ECMA 3/11
     * Uint16 maximumInterchangeLevel
     */
    if(   (       pvd->interchangeLevel != 2 &&        pvd->interchangeLevel != 3)
       || (pvd->maximumInterchangeLevel != 2 && pvd->maximumInterchangeLevel != 3)
       || (       pvd->interchangeLevel > pvd->maximumInterchangeLevel) )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageHead(MLIMIT.vl, (Byte*)  pvd,
                                   (Byte*) &pvd->interchangeLevel, NULL);
        fprintf(uctout,
            "Error: Interchange Level: %lu, Maximum Interchange Level: %lu,\n"
            "-\t\t\texpected: 2 or 3, %sUDF 2.2.2.1+2, ECMA 3/11.\n",
                pvd->interchangeLevel, pvd->maximumInterchangeLevel,
                (pvd->maximumInterchangeLevel == 2)
                  ? "at most 2 for Interchange Level,\n-\t\t\t" : "");
      MLIMITend;
    }

    /* Cross check of interchangeLevel
     *            and maximumVolumeSequenceNumber
     */
    if(   pvd->maximumVolumeSequenceNumber > 1      /* multi-volume Volume Set */
       && pvd->interchangeLevel != 3 )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageHead(MLIMIT.vl, (Byte*)  pvd,
                                   (Byte*) &pvd->interchangeLevel, NULL);
        fprintf(uctout,
            "Error: Interchange Level: %lu, Maximum Volume Sequence Number: %lu,\n"
            "-\t\t expected: Interchange Level 3 for a volume that is part of a\n"
            "-\t\t multi-volume Volume Set, UDF 2.2.2.1, ECMA 3/10.1.6+7, 3/11.\n",
                pvd->interchangeLevel, pvd->maximumInterchangeLevel);
      MLIMITend;
    }

    /* Uint32 characterSetList
     */
    (void) verifyCharSetList(&pvd->characterSetList,
                             (Byte*)pvd, FALSE, "2.2.2.3");

    /* Uint32 maximumCharacterSetList
     */
    (void) verifyCharSetList(&pvd->maximumCharacterSetList,
                             (Byte*)pvd, TRUE, "2.2.2.4");

    /* Dstring volumeSetIdentifier[128]
     */
    VERBOSE00(uctout, "\tPVD   Volume Set Identifier: ");
    PRINTDSTRING(  pvd->volumeSetIdentifier,
            sizeof(pvd->volumeSetIdentifier), "\n");
    verifyDstring( pvd->volumeSetIdentifier, (Byte *)pvd,
            sizeof(pvd->volumeSetIdentifier),
            TRUE, "ECMA 3/10.1.11, UDF 2.2.2.5");   /* obligatory */

    /* extra verify volumeSetIdentifier
     */
    (void) verifyPvdVolSetId(pvd);

    /* Charspec descriptorCharacterSet
     */
    (void) verifyCharspecCS0(&pvd->descriptorCharacterSet,
                    (Byte*) pvd, "Descriptor Character Set",
                                 "2.2.2.6");

    /* Charspec explanatoryCharacterSet
     */
    (void) verifyCharspecCS0(&pvd->explanatoryCharacterSet,
                    (Byte*) pvd, "Explanatory Character Set",
                                 "2.2.2.7");

    /* ExtentAd volumeAbstract          ** TODO: check with USD !! **
     * ExtentAd volumeCopyrightNotice   ** TODO: check with USD !! **
     */
    if(   !verifyExtentAd(&pvd->volumeAbstract,
                         (Byte*)pvd,TRUE)   /* isIntegralBlock */
       || !verifyExtentAd(&pvd->volumeCopyrightNotice,
                         (Byte*)pvd,TRUE) ) /* isIntegralBlock */
    {   result = FALSE;
    }

    /* EntityID applicationIdentifier
     */
    (void) verifyEntityID(&pvd->applicationIdentifier,
                                ENTITY_SUFFIX_APPL, NULL, NULL, (Byte*)pvd);

    /* Timestamp recordingDateAndTime
     */
    (void) verifyTimestamp(&pvd->recordingDateAndTime,"ECMA 3/10.1.17",
                           "PVD Recording Time", (Byte*)pvd, mc, NULL);

    /* EntityID implementationIdentifier
     */
    (void) verifyEntityID( &pvd->implementationIdentifier,
                                ENTITY_SUFFIX_IMPL, NULL, NULL, (Byte*)pvd);

    /* Byte implementationUse[64]
     * Uint32 predecessorVolumeDescriptorSequenceLocation   ** TODO: check ?? **
     *
    /* Uint16 flags, bit 0 must be set if possibly part of volume set.
     * all other flags bits are reserved
     */
    if(   isBitOff(pvd->flags, PVDFL_VOLSETID_BIT)      /* bit 0 ZERO */
       && (   pvd->maximumVolumeSequenceNumber == 0
           || pvd->maximumVolumeSequenceNumber > 1 ) )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit)
          printMessageExpectedU32( MLIMIT.vl,
                  (Byte*)pvd, (Byte*) &pvd->flags,
              "Error: Flags", "%lu", pvd->flags, 0x0001, NULL);
          fprintf(uctout, ". Volume Set Identification\n"
              "-\t\t\tflag must be set. This Volume may be or become part\n"
              "-\t\t\tof a Volume Set, because the Maximum Volume Sequence\n"
              "-\t\t\tNumber is %lu, ECMA 3/10.1.21, 1/5.13, 3/8.6.\n",
                    pvd->maximumVolumeSequenceNumber);
        MLIMITend;
    }
    if( (Uint16)(pvd->flags & (~PVDFL_MASK)) != 0 )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit)
          printMessageHead(MLIMIT.vl, (Byte*) pvd,
                           (Byte*) &pvd->flags, NULL);
          fprintf(uctout, "Error: Flags #%04X, reserved bit set,"
                            " ECMA 3/10.1.21.\n", pvd->flags);
        MLIMITend;
    }

    /* Byte reserved[22], ECMA 3/10.1.22
     */
    if( !verifyZeros( pvd->reserved, sizeof(pvd->reserved),
                      NULL, NULL, NULL) )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          (void) verifyZerosInDescriptor((Byte *)pvd,
                                         (Byte *)pvd->reserved,
                                          sizeof(pvd->reserved),
                                          NULL);
          fprintf(uctout, "-\t\t Reserved field, ECMA 3/10.1.22.\n");
        MLIMITend;
    }

    return result;

}   /* end verifyPrimaryVolumeDescriptor() */

static bool verifyAnchorVolumeDescriptorPointer(
                    AnchorVolumeDescriptorPointer *avdp)
{
    const MediumInfo *vmi     = getTheMediumInfo();
          Uint32      block16 = 16 * vmi->blockSize;

    /* ExtentAd    mainVolumeDescriptorSequenceExtent
     * ExtentAd reserveVolumeDescriptorSequenceExtent
     */
    verifyExtentAd( &avdp->mainVolumeDescriptorSequenceExtent,
                    (Byte*) avdp, TRUE);            /* isIntegralBlock */
    verifyExtentAd( &avdp->reserveVolumeDescriptorSequenceExtent,
                    (Byte*) avdp, TRUE);            /* isIntegralBlock */

    if(    avdp->mainVolumeDescriptorSequenceExtent.extentLength < block16
        || avdp->reserveVolumeDescriptorSequenceExtent.extentLength < block16 )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageHead(MLIMIT.vl, (Byte*)avdp, NULL, NULL);
        fprintf(uctout,
              "error: Main or Reserve Volume Descriptor Sequence Extent\n"
            "-\t\t    has extent length less than 16 logical blocks (%lu bytes)\n"
            "-\t\t    Main: %lu bytes, Reserve: %lu bytes, UDF 2.2.3.1+2\n",
            block16,
            avdp->mainVolumeDescriptorSequenceExtent.extentLength,
            avdp->reserveVolumeDescriptorSequenceExtent.extentLength);
      MLIMITend;
    }

    /* Byte Reserved[480]
     * Note that verifyZerosInDescriptor() assumes to be
     * called from within a MLIMITbegin(ERROR00level, ...)
     *                  ... MLIMTend clause.
     */
    if( !verifyZeros( avdp->reserved, sizeof(avdp->reserved),
                      NULL, NULL, NULL) )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          (void) verifyZerosInDescriptor((Byte *)avdp,
                                         (Byte *)avdp->reserved,
                                          sizeof(avdp->reserved),
                                          NULL);
          fprintf(uctout, "-\t\t Reserved field, ECMA 3/10.2.4\n");
        MLIMITend;
    }

    return TRUE;
}

static bool verifyVolumeDescriptorPointer(VolumeDescriptorPointer *vdp)
{
#ifdef  DEBUG01
    MLIMITbegin(DEBUG01level, MLIMITdefault01);
        implementationNotReady("Volume Descriptor Pointer");    /* TODO: */
    MLIMITend;
#endif  /* DEBUG01 */

    verifyExtentAd( &vdp->nextVolumeDescriptorSequenceExtent,
                    (Byte*)vdp, TRUE ); /* isIntegralBlock */
    return TRUE;
}


/* LVInformation
 * UDF 2.2.7.2
 * Located in implementationUse of Implementation Use Volume Descriptor.
 */
static bool verifyLVInformation(ImplementationUseVolumeDescriptor *iuvd)
{
    LVInformation *lvi = &iuvd->implementationUse.lvInformation;

#ifdef  DEBUG01
    MLIMITbegin(DEBUG01level, MLIMITdefault01);
        implementationNotReady( "LVInformation" );
    MLIMITend;
#endif  /* DEBUG01 */

    (void) verifyCharspecCS0(&lvi->LVICharset,
                    (Byte*) iuvd, "LVICharset", "2.2.7.2.1");

    /* Dstring logicalVolumeIdentifier[128]
     * Relation of logicalVolumeIdentifier in LVD, IUVD, FSD
     * and VAT2 checked in checkVolumeIdentifiersSummary().
     */
    VERBOSE00(uctout, "\tUDF IUVD Logical Volume Identifier : ");
    PRINTDSTRING(  lvi->logicalVolumeIdentifier,
            sizeof(lvi->logicalVolumeIdentifier), "\n");
    verifyDstring( lvi->logicalVolumeIdentifier, (Byte *)iuvd,
            sizeof(lvi->logicalVolumeIdentifier),
                TRUE, "UDF 2.2.7.2.2");          /* obligatory */

    VERBOSE00(uctout, "\tUDF IUVD LVInfo1: ");
    PRINTDSTRING(  lvi->LVInfo1,
            sizeof(lvi->LVInfo1), "\n");
    verifyDstring( lvi->LVInfo1, (Byte *)iuvd,
            sizeof(lvi->LVInfo1),
                FALSE, "LVInfo1, UDF 2.2.7.2.3"); /* NOT obligatory */

    VERBOSE00(uctout, "\tUDF IUVD LVInfo2: ");
    PRINTDSTRING(  lvi->LVInfo2,
            sizeof(lvi->LVInfo2), "\n");
    verifyDstring( lvi->LVInfo2, (Byte *)iuvd,
            sizeof(lvi->LVInfo2),
                FALSE, "LVInfo2, UDF 2.2.7.2.3"); /* NOT obligatory */

    VERBOSE00(uctout, "\tUDF IUVD LVInfo3: ");
    PRINTDSTRING(  lvi->LVInfo3,
            sizeof(lvi->LVInfo3), "\n");
    verifyDstring( lvi->LVInfo3, (Byte *)iuvd,
            sizeof(lvi->LVInfo3),
                FALSE, "LVInfo3, UDF 2.2.7.2.3"); /* NOT obligatory */

    /* UDF 2.2.7.2.4
     */
    verifyEntityID( &lvi->implementationID,
                    ENTITY_SUFFIX_IMPL, NULL, NULL, (Byte*)iuvd );

    return TRUE;
}

/* Implementation Use Volume Descriptor, ECMA 3/10.4, UDF 2.2.7.
 * There is one UDF IUVD defined, it shall be recorded.
 * Other foreign IUVDs may exist, recorded "in its own format",
 * UDF 2.2.7.
 *
 * A IUVD with EntityID Identifier starting with "*UDF"
 * is assumed to be a UDF IUVD with "UDF" EntityID suffix type.
 * else a non-UDF IUVD with "Application" EntityID suffix type
 * is assumed (to be updated in UDF spec, DCN !! ).
 *
 * The only UDF IUVD defined at the moment
 * is the UDF LV Info IUVD, UDF 2.2.7.1+2
 */
static bool verifyImplementationUseVolumeDescriptor(
                  ImplementationUseVolumeDescriptor *d)
{
#ifdef  DEBUG01
    MLIMITbegin(DEBUG01level, MLIMITdefault01);
        implementationNotReady( "Implementation Use Volume Descriptor" );
    MLIMITend;
#endif  /* DEBUG01 */

    VERBOSE00(uctout, "\tIUVD EntityID Identifier: ");
    printBytesName( (Byte *) d->implementationIdentifier.Identifier,
                    ENTITYID_IDSIZE, FALSE, VERBOSE00level );
    VERBOSE00(uctout,"\n");

    /* Assume UDF IUVD if first 4 chars are "*UDF"
     * else foreign IUVD "in its own format", UDF 2.2.7.
     */
    if( memcmp( &d->implementationIdentifier.Identifier,
                "*UDF", 4 ) != 0 )
    {   /* Non UDF IUVD, Implementation EntityID suffix type
         */
        (void) verifyEntityID( &d->implementationIdentifier,
                    ENTITY_SUFFIX_IMPL, NULL, NULL, (Byte*)d);
        MLIMITbegin(INFO01level, uctMessageLimit);
          printMessageHead(MLIMIT.vl, (Byte*) d,
                (Byte*) &d->implementationIdentifier,
                "Note: Non-UDF IUVD: ");
          printBytesName(
                (Byte *) d->implementationIdentifier.Identifier,
                ENTITYID_IDSIZE, FALSE, MLIMIT.vl);
          fprintf(uctout,
              "\n-\t\t       recorded \"in its own format\","
                        " UDF 2.2.7.\n");
        MLIMITend;
    }
    else
    {   /* UDF IUVD, UDF EntityID suffix type, only one defined,
         * i.e. the UDF LV Info IUVD, UDF 2.2.7.1+2
         */
        (void) verifyEntityID( &d->implementationIdentifier,
                    ENTITY_SUFFIX_UDF, NULL, NULL, (Byte*)d);

        if( memcmp(&d->implementationIdentifier.Identifier,
                    UDF_LVI_IUVD_ID, ENTITYID_IDSIZE) != 0 )
        {   /* UDF IUVD, but not the UDF LV Info IUVD
             */
            MLIMITbegin(ERROR00level, uctMessageLimit);
              printMessageHead(MLIMIT.vl, (Byte*) d,
                    (Byte*) &d->implementationIdentifier,
                    "Error: Undefined UDF IUVD: ");
              printBytesName(
                    (Byte *) d->implementationIdentifier.Identifier,
                    ENTITYID_IDSIZE, FALSE, MLIMIT.vl);
              fprintf(uctout,
                ",\n-\t\t\t\t  Expected: ");
              printBytesName((Byte*) UDF_LVI_IUVD_ID, ENTITYID_IDSIZE,
                              FALSE, MLIMIT.vl );
              fprintf(uctout,
                ".\n-\t\t Layout handled as UDF LV Info IUVD,"
                                    " UDF 2.2.7.1+2\n");
            MLIMITend;
        }

        /* assume verifyLVInformation in implementationUse
         * because the UDF LV Info IUVD is the only
         * UDF IUVD defined so far, UDF 2.2.7.2
         */
        (void) verifyLVInformation(d);
    }

    return TRUE;
}


/* partition header descriptor
 * ECMA 4/14.3, UDF 2.3.3
 * Located in partitionContentsUse of Partition Descriptor.
 * Note: More checks in readPartitionSpaceSets().
 */
static bool verifyPartitionHeaderDescriptor(PartitionHeaderDescriptor *phd,
                                            Byte *d)
{
    static Uint16  countTables = 0;     /* accumulates */
    static Uint16  countBitmaps = 0;        /*  over calls */
           Byte   *lastOne;
           bool    USTpresent, USBpresent,
                   FSTpresent, FSBpresent;

    /* Unallocated and Freed Space Set
     * verify, set isIntegralBlock and isFixedRecAndAlloc flags.
     * verifyShortAd() arguments:
     *  shortPartRef   : -1 (irrelevant here because mc == NULL).
     *  isIntegralBlock and isFixedRecAndAlloc:
     *                 : both TRUE (also for table strategy 4096).
     *  mc             : NULL.
     */
    verifyShortAd(&phd->unallocatedSpaceTable, (Uint16) -1,
                                        d, TRUE, TRUE, NULL);
    verifyShortAd(&phd->unallocatedSpaceBitmap, (Uint16) -1,
                                        d, TRUE, TRUE, NULL);
    verifyShortAd(&phd->freedSpaceTable, (Uint16) -1,
                                        d, TRUE, TRUE, NULL);
    verifyShortAd(&phd->freedSpaceBitmap, (Uint16) -1,
                                        d, TRUE, TRUE, NULL);

#ifdef UCT_TESTING  /* overlap of unallocated and freed space set */
/**testing**/phd->unallocatedSpaceTable = phd->freedSpaceTable;
/**testing**/phd->unallocatedSpaceBitmap = phd->freedSpaceBitmap;
#endif  /** UCT_TESTING **/

    USTpresent = (adGetExtentSize(&phd->unallocatedSpaceTable) != 0);
    USBpresent = (adGetExtentSize(&phd->unallocatedSpaceBitmap) != 0);
    FSTpresent = (adGetExtentSize(&phd->freedSpaceTable) != 0);
    FSBpresent = (adGetExtentSize(&phd->freedSpaceBitmap) != 0);

    /* informational messages an mixed mode test
     */
    lastOne = NULL;
    if( USTpresent )
    {   countTables++;
        lastOne = (Byte*)&phd->unallocatedSpaceTable;
        ifPRINTinfo01(uctout, "\tUnallocated Space Table\n");
        ENDif;
    }
    if( USBpresent )
    {   countBitmaps++;
        lastOne = (Byte*)&phd->unallocatedSpaceBitmap;
        ifPRINTinfo01(uctout, "\tUnallocated Space Bitmap\n");
        ENDif;
    }
    if( FSTpresent )
    {   countTables++;
        lastOne = (Byte*)&phd->freedSpaceTable;
        ifPRINTinfo01(uctout, "\tFreed Space Table\n");
        ENDif;
    }
    if( FSBpresent )
    {   countBitmaps++;
        lastOne = (Byte*)&phd->freedSpaceBitmap;
        ifPRINTinfo01(uctout, "\tFreed Space Bitmap\n");
        ENDif;
    }

    if( lastOne == NULL )   /* none found, see readPartitionSpaceSets() */
    {   ifPRINTinfo01(uctout,
            "\tPD: No Unallocated/Freed Space Set in"
                                " Partition Header Descriptor\n");
        ENDif;
    }
    else if( countTables != 0 && countBitmaps != 0 )
    {                                       /* accumulated counts */
        MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageHead(MLIMIT.vl, d, (Byte*)phd,
              "Error: Mixed use of Space table and bitmap"
                                " in Logical Volume\n");
          printExtraInfoForPHD(d, lastOne);
        MLIMITend;
    }

    /* partitionIntegrityTable, UDF 2.3.3.1
     * Not used by UDF, all zero bytes.
     */
    if(    phd->partitionIntegrityTable.extentLength   != 0
        || phd->partitionIntegrityTable.extentPosition != 0 )
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageHead(MLIMIT.vl, d,
                           (Byte*)&phd->partitionIntegrityTable, NULL);
          fprintf(uctout,
            "Error: partitionIntegrityTable extent Length and\n"
            "-\t\t Position: %lu, %lu, shall both be zero, UDF 2.3.3.1\n",
                phd->partitionIntegrityTable.extentLength,
                phd->partitionIntegrityTable.extentPosition);
          printExtraInfoForPHD(d, (Byte*)&phd->partitionIntegrityTable);
        MLIMITend;
    }
    return TRUE;

}   /* end verifyPartitionHeaderDescriptor() */


/* Verify Partition Access Type.
 * Since UDF 2.60, PD Access Type zero is used for
 * pseudo-overwritable partitions, see UDF 2.2.14.2.
 * So for this test, it must be known whether the
 * medium UDF revision is (2.60 or higher) or not.
 * return value:
 *  if sufficient medium UDF revision knowledge available
 *  then: TRUE  (after access type test has been performed)
 *  else: FALSE (and no access type test performed)
 */
static bool verifyPartitionAccessType(PartitionDescriptor *pd)
{
    Uint32 accessType, pdat_min;
    Uint16 maxUdfRev = getUctMaxUdfRevision(),
           minUdfRev = getUctMinUdfRevision();

    if(   maxUdfRev >= 0x260    /* not enough revision knowledge for */
       && minUdfRev <  0x260 )  /*  testing of access type (zero),   */
    {   return FALSE;           /* access type could not be verified */
    }
    /* Enough medium UDF revision knowledge
     * because:    0x260 <= minUdfRev <= maxUdfRev
     *          ||        minUdfRev <= maxUdfRev < 0x260
     * Test Partition Access Type, zero for pseudo-overwritable
     * allowed for UDF 2.60 and later
     */
    pdat_min = (minUdfRev >= 0x260) ? 0 : 1;
    accessType = pd->accessType;

    if(   accessType < pdat_min
       || accessType > PDAT_MAX )
    { MLIMITbegin( ERROR00level, uctMessageLimit );
        printMessageHead(MLIMIT.vl, (Byte*)pd,
                         (Byte*)&pd->accessType, NULL);
        fprintf(uctout,
                "Error: Partition access type: %lu (%s),\n"
          "-\t\t expected: at least %u and at most %u\n"
                "-\t\t\t   for UDF revisions %s,\n"
          "-\t\t UDF section 2 Basic Rescrictions ...,\n"
          "-\t\t %sECMA 3/10.5.7.\n",
                accessType, PDAT_TEXT(accessType),
                pdat_min, PDAT_MAX,
                (minUdfRev >= 0x260) ? "2.60 and higher"
                                     : "lower than 2.60",
                (minUdfRev >= 0x250) ? "UDF 2.2.14.2, " : "");
                /* note: 2.2.14.2 introduced in UDF 2.50 */
      MLIMITend;
    }
    return TRUE;        /* access type was verified */

}   /* end verifyPartitionAccessType() */

/* finalCheckPartitionAccessType()
 * Check Partition Access Type of all prevailing PDs if this
 * was skipped for ALL PDs, because the medium UDF revision
 * was not yet known at the time when the first PD was read.
 * No action if test already done.
 */
static bool partitionAccessTestPostponed = FALSE;

extern bool finalCheckPartitionAccessType(UdfMountContext *mc)
{   Uint32 n;
    UdfVolumeInformation *vi;

    if( partitionAccessTestPostponed == FALSE )
    {   return TRUE;                    /* tested already */
    }

    /* testing Partition Access type was skipped for ALL PDs
     * so do it now for all prevailing PDs.
     */
    vi = mc->vi;
    for( n = 0; n < vi->nrPDs; n++ )
    { if(verifyPartitionAccessType(&vi->pd[n]) == FALSE )
      { UCTASSERT(FALSE);   /* please not here, UDF revision known */
      }
    }
    return TRUE;
}

/* Partition Descriptor
 * ECMA 3/10.5 and 4/3.1, UDF 2.2.14 (was 2.2.12)
 * A number of fields is only tested if the PD is
 * referenced by an LVD Partition Map. Most of these
 * tests are done in checkAndMapPartitionInfo().
 */
static bool verifyPartitionDescriptor(PartitionDescriptor *pd)
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32 tmp32;

    /* Uint16   volumeDescriptorSequenceNumber;
     *          used/verified when reading PDs in VDS.
     *
     * Uint16   partitionFlags;
     * Uint16   partitionNumber;
     * EntityID partitionContents;
     *          Verified in checkAndMapPartitionInfo()
     *          or verifyPdPartitionFlags() if PD
     *          is referenced by an LVD Partition Map.
     * Syntax check for partitionContents here:
     */
    verifyEntityID(&pd->partitionContents,
                    ENTITY_SUFFIX_APPL, NULL, NULL, (Byte*)pd);

    /* union{
     *   Byte partitionContentsUse[128];
     *   PartitionHeaderDescriptor partitionHeaderDescriptor;
     * }
     */
    verifyPartitionHeaderDescriptor(
                &pd->partitionContentsUse.partitionHeaderDescriptor,
                (Byte *) pd );

    /* Uint32   accessType;
     * Access Type can only be tested if it is known whether the medium
     * UDF revision is either (2.60 and higher) or (lower than 2.60), because
     * access type zero (pseudo-overwritable) was introduced in UDF 2.60.
     * If not enough revision range knowledge is present at the time when
     * the first PD is read, the access type verification is postponed
     * for ALL PDs till all Volume Descriptors are read.
     * On the other hand, postponement is avoided if the first PD holds
     * Access Type zero, because this fact is used to narrow the medium
     * UDF revision range sufficiently.
     * Only try to narrow the UDF revision range if the range 0x260 thru
     * MAX_UDFREVISION does not conflict with the current range in order
     * to avoid confusing messages.
     */
    if(   pd->accessType == PDAT_POW_OR_UNKNOWN         /* zero */
       && getUctMinUdfRevision() <= MAX_UDFREVISION     /* range not */
       && getUctMaxUdfRevision() >= 0x260 )             /*  conflicting */
    {   modifyUdfRevisionRange( 0x260, MAX_UDFREVISION,     /* UDF 2.60+ */
            "Partition Access Type zero (pseudo-overwritable)");
    }
    /* bool partitionAccessTestPostponed is statically initialized as FALSE.
     * verifyPartitionAccessType() only returns FALSE if there is not
     * enough knowledge about the current UDF revision range.
     * See also finalCheckPartitionAccessType();
     */
    if(   partitionAccessTestPostponed == FALSE
       && verifyPartitionAccessType(pd) == FALSE )  /* not enough revision info */
    {   partitionAccessTestPostponed = TRUE;        /* skip test here for all PDs */
    }

    /* Uint32   partitionStartingLocation;
     * Uint32   partitionLength;
     *
     * Check start here.
     * Verify partition alignment, etc. later
     * in checkAndMapPartitionInfo()
     * using verifyPdPartitionAlignment():
     * Do not start any partition in 1st 32K of volume space (error).
     */
    tmp32 = ROUNDUPELEMENTS((32 * 1024), vmi->blockSize);
    if( pd->partitionStartingLocation < tmp32 )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
            "\tError: Partition Starting Location: %lu, expected: at least %lu.\n"
           "-\t       Partition starts in first 32K of Volume Space,\n"
           "-\t       UDF section 2 Basic Restrictions ...\n",
                pd->partitionStartingLocation, tmp32);
      MLIMITend;
      /* no FALSE return result */
    }

    /* It is good practice to start a partition after 256 (Warning).
     */
    if( pd->partitionStartingLocation < 257 )
    { MLIMITbegin(WARN01level, uctMessageLimit);
        fprintf(uctout,
           "\tWarning: Partition Starting Location: %lu. It is recommended\n"
                "-\t\t to start a partition at 257 or higher in order to\n"
                "-\t\t avoid overlap with the VRS and Volume Descriptors\n"
                "-\t\t at the beginning of the volume space.\n",
            pd->partitionStartingLocation);
      MLIMITend;
      /* no FALSE return result */
    }

    /* EntityID  implementationIdentifier;
     */
    verifyEntityID( &pd->implementationIdentifier,
                    ENTITY_SUFFIX_IMPL, NULL, NULL, (Byte*)pd);

    /* Byte     implementationUse[128];
     *          no tests
     */
    /* Byte     reserved[156];      ECMA 3/10.5.12
     * Note that verifyZerosInDescriptor() assumes to be
     * called from within a MLIMITbegin(ERROR00level, ...)
     * ... MLIMTend clause.
     */
    if( !verifyZeros( (Byte *)&pd->reserved, sizeof(pd->reserved),
                      NULL, NULL, NULL) )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          (void) verifyZerosInDescriptor((Byte *)pd,
                                         (Byte *)pd->reserved,
                                          sizeof(pd->reserved),
                                          NULL);
        MLIMITend;
    }
    return TRUE;
}


static void verifyPartitionMapZeros(Byte *field, Uint32 size, Byte *d,
                                    char *txt1, Uint32 rbp,
                                    char *txt2, char *txt3)
{
    char *format = "-\t%s RBP %lu %s field error, %s\n";

    /* Note that verifyZerosInDescriptor() assumes to be
     * called from within a MLIMITbegin(ERROR00level, ...)
     * ... MLIMTend clause.
     */
    if( !verifyZeros(field, size, NULL, NULL, NULL) )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
          (void) verifyZerosInDescriptor(d, field, size, NULL);
          fprintf(uctout, format, txt1, rbp, txt2, txt3);
        MLIMITend;
        /** no FALSE result **/
    }
}


/* verifyPdPartitionFlags():
 * Only call for PDs that are referenced by an LVD Partition Map.
 */
extern bool verifyPdPartitionFlags(PartitionMapInfo *pmi,
                                   Uint16 partRefNmb)
{
    PartitionDescriptor *pd = pmi->pdPointer;
    bool   pdAllocationFlag = isBitOn(pd->partitionFlags, 0);

    /* Uint16 partitionFlags;   ECMA 3/10.5.3
     * Allocation bit (bit 0) shall be ONE for all PDs
     * referenced by an LVD Partition Map
     * All other bits reserved, so ZERO.
     */
    if( pd->partitionFlags != 0x0001 )
    {
      MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageHead(MLIMIT.vl, (Byte*) pd,
                         (Byte*)&pd->partitionFlags, NULL);
        fprintf(uctout,
          "Error: p%u Partition Flags: #%04X, expected: #0001,\n"
            "-\t\t%s, ECMA 3/10.5.3.\n",
            partRefNmb, pd->partitionFlags,
            (pdAllocationFlag) ? "\tReserved bit shall be ZERO"
          : " Allocation bit shall be set to ONE for all PDs\n"
            "-\t\t referenced by an LVD Partition Map");
      MLIMITend;
    }
    return TRUE;
}


/* verifyPdPartitionAlignment():
 * Call after ECC blocking factor and sparing Packet Length are
 * established. MediumInfo eccLength is used for ECC blocking
 * factor in a Physical Partition.
 */
extern bool verifyPdPartitionAlignment( UdfMountContext *mc,
                                        Uint16 partRefNmb )
{
    PartitionMapInfo    *pmi = &mc->partitionMapInfo[partRefNmb];
    PartitionDescriptor *pd = pmi->pdPointer;
    Uint16  minUdfRevision  = getUctMinUdfRevision();
    bool    result = TRUE;

    if( IS_PREF_PARTITION_NOT_FOUND(partRefNmb) )
    {   return FALSE;               /* partition does not exist */
    }

    /* Specific Sparable and Physical Partition tests.
     * UDF 2.2.14.3+4 (was 2.2.12.3.2+3).
     * Note that for UDF 2.50 Physical, alignment on start address only.
     */
    if( pmi->pMapType == PMAPTYPE_SPARABLE )
    { Uint16 spPacketLength = MAX(1,                /* sparing blocks per packet */
          pmi->pPartitionMap->type2PartitionMap.SharedTail.sparableTail.packetLength);
      if(   (pd->partitionStartingLocation % spPacketLength) != 0
         || (pd->partitionLength             % spPacketLength) != 0 )
      {
        MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
               "\tError: Sparable Partition Starting Location"
                                        " and Length: %lu, %lu,\n"
              "-\t       expected: both multiples of the"
                                " Sparing Packet Length (%lu),\n"
              "-\t       %s.*.\n",
                    pd->partitionStartingLocation,
                    pd->partitionLength, spPacketLength,
                    UDFREF_PD(minUdfRevision));
        MLIMITend;
        result = FALSE;
      }
    }
    else if(   minUdfRevision >= 0x250              /* UDF 2.50+ */
            && pmi->pMapType == PMAPTYPE_PHYSICAL ) /* 2.2.14.3 */
    {   Uint32 eccLength =                          /* ECC blocks per packet */
                getTheMediumInfo()->eccLength;
        /* NOTE: silly enough, there is no ECC alignment requirement
         *       for the partition end/length
         */
        if( (pd->partitionStartingLocation % eccLength) != 0 )
        {
          MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
               "\tError: p%u Physical Partition Starting Location: %lu,\n"
              "-\t       expected: multiple of the ECC"
                                " blocking factor (%lu), %s.3.\n",
                    partRefNmb, pd->partitionStartingLocation,
                    eccLength, UDFREF_PD(minUdfRevision));
          MLIMITend;
          result = FALSE;
        }
    }
    return result;

}   /* end verifyPdPartitionAlignment() */


/* compareMetadataAD(), called by compareMetadataAllocationList():
 *
 * Be prepaired for the case that LongAd is use (altough not allowed)
 * and that a 'main' extent is partly covered by a 'mirror' extent and
 * the remainder by a next 'mirror' extent.
 * remSizeMain/Mirr and remLocMain/Mirr address the (part of) the extent
 * that remains and is not yet covered by a previous mirror/main extent.
 *
 * precondition: remSizeMain != 0 && aiMain != NULL
 *            && remSizeMirr != 0 && aiMirr != NULL );
 */
static bool compareMetadataAD(UdfAllocationItem *aiMain,
                              UdfAllocationItem *aiMirr,
                              Uint32             cntMain,
                              Uint32             cntMirr,
                              Uint8              adTypeMain,
                              Uint8              adTypeMirr,
                              Uint32             remSizeMain,
                              Uint32             remSizeMirr,
                              Uint32             remLocMain,
                              Uint32             remLocMirr,
                              Uint16             cPref,
                              bool               duplicateFlag,
                              bool               isAEDextent)
{
    Uint32  sizeMain, sizeMirr;
    Uint16  prefMain, prefMirr;
    Uint8   typeMain, typeMirr;
    bool    duplicateError, typeOrPrefError, AEDerror,
            remLocEqual, bothSparse,
            eqFlag;         /* return result */

    UCTASSERT(   remSizeMain != 0 && aiMain != NULL
              && remSizeMirr != 0 && aiMirr != NULL );

    /* now extract values from the ADs, mind that
     * sizeMain/Mirr may be different from remSizeMain/Mirr,
     * the same for locMain/Mirr and remLocMain/Mirr.
     */
    if( !udfGetLocation(&aiMain->aad, adTypeMain,
                         cPref, &prefMain, NULL) )
    { UCTASSERT(FALSE);     /* please report */
    }
    sizeMain = adGetExtentSize(&aiMain->aad.anyAd);
    typeMain = adGetExtentType(&aiMain->aad.anyAd);

    if( !udfGetLocation(&aiMirr->aad, adTypeMirr,
                         cPref, &prefMirr, NULL) )
    { UCTASSERT(FALSE);     /* please report */
    }
    sizeMirr = adGetExtentSize(&aiMirr->aad.anyAd);
    typeMirr = adGetExtentType(&aiMirr->aad.anyAd);

    /* First determine if (the remaining part of) ADs are 'equal'.
     * (this does not mean that no error mesage is printed).
     * The size of the extent is don't care for eqFlag !!
     */
    typeOrPrefError = (   typeMain != typeMirr
                       || prefMain != prefMirr );

    /* location of sparse extent is don't care (shall be zero)
     */
    bothSparse = (   typeMain == ADEL_NOT_RECORDED_NOT_ALLOCATED
                  && typeMirr == ADEL_NOT_RECORDED_NOT_ALLOCATED );

    /* remLocEqual is TRUE for equal 'remaining' locations
     */
    if( bothSparse )    /* don't care remLocMain and remLocMirror */
    {   remLocEqual = TRUE;
    }
    else if(   typeMain == ADEL_NOT_RECORDED_NOT_ALLOCATED   /* SPARSE */
            || typeMirr == ADEL_NOT_RECORDED_NOT_ALLOCATED ) /* SPARSE */
    {   remLocEqual = FALSE;    /* only one is sparse */
    }
    else                        /* both not sparse */
    {   remLocEqual = ( remLocMain == remLocMirr );
    }
    eqFlag = ( remLocEqual  && !typeOrPrefError );

    /* The locations of type 3 extents from overheadList
     * (AED pointer) shall never be equal.
     * AEDs must be duplicated INDEPENDENT of the value
     * of the Duplicate Metadata Flag.
     */
    AEDerror = ( isAEDextent && remLocEqual );

/* TODO: test isAEDextent in "**Single*()" test ?? */

    /* if duplicateFlag set, locations must be equal.
     * if duplicateFlag not set, locations must not be equal.
     * Exceptions: No duplicateError for AEDextent (maybe AEDerror)
     *             and for sparse extents.
     */
    duplicateError = (    duplicateFlag == remLocEqual
                      && !isAEDextent
                      && !bothSparse );

    /* print message.
     */
    if( duplicateError || typeOrPrefError || AEDerror )
    { char *formatX = "-\t%9s %3u%s %8s extent: (%8lu,p%u),"
                                    " size: %5lu, type: %u%s\n";
      MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
             "\tError: Metadata %sextents %sequal and"
                            " Duplicate Metadada Flag %sset:\n",
                            isAEDextent     ? "AED " : "",
                            eqFlag          ? "" : "NOT ",
                            duplicateFlag   ? "" : "NOT ");
        fprintf(uctout, formatX,
                    (sizeMain != remSizeMain) ? "(part of)" : "",
                     cntMain, XTH2(cntMain), "Metadata",
                    remLocMain, prefMain, remSizeMain, typeMain,
                    (typeMain == ADEL_NOT_RECORDED_NOT_ALLOCATED)
                                 ? " (sparse)" : "");
        fprintf(uctout, formatX,
                    (sizeMirr != remSizeMirr) ? "(part of)" : "",
                     cntMirr, XTH2(cntMirr), "Mirror",
                    remLocMirr, prefMirr, remSizeMirr, typeMirr,
                    (typeMirr == ADEL_NOT_RECORDED_NOT_ALLOCATED)
                                 ? " (sparse)" : "");
        if( duplicateError )
        { fprintf(uctout,
            "-\t%sxtent locations shall %sbe equal%s%s.\n",
                ( isAEDextent)                ? "AED e" : "E",
                ( isAEDextent || duplicateFlag) ? "NOT " : "",
                (!isAEDextent)
                  ? " if Duplicate Metadata Flag is " : "",
                (!isAEDextent && !duplicateFlag)
                  ? "NOT set"
                  :  (!isAEDextent && duplicateFlag)
                  ? "set" : "");
        }
        if( typeOrPrefError )
        { fprintf(uctout,
            "-\tExtent type and partition reference"
                                " number shall be equal.\n");
        }
        if( AEDerror )
        { fprintf(uctout,
            "-\tAED continuation extent shared by"
                            " Metadata File and Metadata\n"
            "-\tMirror File. Allocation Descriptors"
                            " shall be duplicated, also\n"
            "-\tif the Duplicate Metadata Flag is not"
                            " set. See quote: \"... and\n"
            "-\tits associated allocation descriptors"
                            " are unique and distinct\".\n");
        }
        fprintf(uctout,
            "-\tUDF 2.2.10, 2.2.13.\n");
      MLIMITend;
    }

    return eqFlag;

}   /* end compareMetadataAD() */

/* compareMetadataAllocationList(), called by
 * verifyMetadataDuplicateAllocation():
 * maybe 'normal' list or overheadList.
 * one of them may be NULL.
 */
static bool compareMetadataAllocationList(UdfMountContext   *mc,
                              UdfAllocationList *alMain,
                              UdfAllocationList *alMirr,
                              bool               duplicateFlag,
                              bool               isOverheadList)
{
    UdfAllocationItem *aiMain, *aiMirr;
    Uint8              adTypeMain, adTypeMirr;
    Uint16             cPref;
    Uint32             remSizeMain, remLocMain, cntMain,
                       remSizeMirr, remLocMirr, cntMirr,
                       minSize;
    bool               isEqual = TRUE;

    /* check allocation lists
     */
    if( alMain == alMirr )
    {   if( alMain != NULL ) UCTASSERT(FALSE);  /* please report */
        return isEqual;        /* equal, (both NULL) */
    }
    remLocMain = remLocMirr = 0;    /* keep compiler happy */
    adTypeMain = adTypeMirr = 0;    /* keep compiler happy */
    aiMain = aiMirr = NULL;
    if( alMain )
    {   aiMain     = alMain->head;
        adTypeMain = alMain->itemAdType;
    }
    if( alMirr )
    {   aiMirr     = alMirr->head;
        adTypeMirr = alMirr->itemAdType;
    }
    cPref = mc->partitionMapInfo[mc->metadataPref].counterpartPRef;

    /* At least one allocation list exists.
     * Be prepaired for the case that the number of ADs in
     * the lists may be different, but still define the same
     * or equivalent extents. An extent may be partly covered
     * by a mirror extent and the remainder by a next mirror extent.
     * remSizeMain/Mirr and remLocMain/Mirr address (part of) the extent
     * that remains and is not yet covered by a mirror/main extent.
     */
    cntMain = cntMirr = 1;
    remSizeMain = remSizeMirr = 0;
    while( aiMain != NULL || aiMirr != NULL )
    {   /* one of aiMain or aiMirr maybe NULL
         */
        if(   remSizeMain == 0      /* fresh AD in aiMain */
           && aiMain != NULL )      /* if any */
        {   remSizeMain = adGetExtentSize(&aiMain->aad.anyAd);
            { if( !udfGetLocation(&aiMain->aad, adTypeMain,
                                   cPref, NULL, &remLocMain) )
              { UCTASSERT(FALSE);       /* please report */
              }
            }
        }
        if(   remSizeMirr == 0      /* fresh AD in aiMirr */
           && aiMirr != NULL )      /* if any */
        {   remSizeMirr = adGetExtentSize(&aiMirr->aad.anyAd);
            { if( !udfGetLocation(&aiMirr->aad, adTypeMirr,
                                   cPref, NULL, &remLocMirr) )
              { UCTASSERT(FALSE);       /* please report */
              }
            }
        }

        /* compare if remaining size in both Main and Mirr
         */
        if(    remSizeMain != 0
           &&  remSizeMirr != 0
           && !compareMetadataAD( aiMain, aiMirr,
                                cntMain, cntMirr,
                                adTypeMain, adTypeMirr,
                                remSizeMain, remSizeMirr,
                                remLocMain,  remLocMirr,
                                cPref, duplicateFlag,
                                isOverheadList) )   /* isAEDextent */
        {   isEqual = FALSE;
        }

        /* If not ready, at least one must proceed to a next AD
         * (the one with the smallest remSize)
         * Do not adapt remLocMain/Mirr for sparse extent.
         */
        minSize = MIN(remSizeMain, remSizeMirr);
        if( aiMain != NULL )
        { remSizeMain -= minSize;               /* proceed minSize */
          if( adGetExtentType(&aiMain->aad.anyAd)
              != ADEL_NOT_RECORDED_NOT_ALLOCATED )   /* NOT sparse */
          {  remLocMain += minSize;
          }
          if( remSizeMain == 0 )    /* main AD covered by mirror AD */
          { cntMain++;
            aiMain = aiMain->next;  /* next AD */
          }
        }
        if( aiMirr != NULL )
        { remSizeMirr -= minSize;               /* proceed minSize */
          if( adGetExtentType(&aiMirr->aad.anyAd)
              != ADEL_NOT_RECORDED_NOT_ALLOCATED )   /* NOT sparse */
          {  remLocMirr += minSize;
          }
          if( remSizeMirr == 0 )    /* mirror AD covered by main AD */
          { cntMirr++;
            aiMirr = aiMirr->next;  /* next AD */
          }
        }

        /* Check if remaining main/mirr size can be covered by a next
         * mirr/main AD. If not, we have a total size mismatch. In that
         * case we'll break the loop to avoid infinite looping and test
         * the loop end condition in order to flag the mismatch if needed.
         */
        if(   (remSizeMain != 0 && aiMirr == NULL)
           || (remSizeMirr != 0 && aiMain == NULL) )
        {   break;
        }
    }       /* endwhile */

    /* check the loop end condition
     */
    if(   aiMain != NULL
       || aiMirr != NULL
       || remSizeMain != 0
       || remSizeMirr != 0 )
    {   /* There is remaining uncovered size in a main/mirror AD,
         * but the other party (mirror/main) has no more extents,
         * means total size mismatch.
         * NOTE that this can happen legally for the overheadList.
         */
        if( !isOverheadList )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
               "\tError: Total size mismatch for Metadata File"
                                    " and Metadata Mirror File.\n"
              "-\t       Remaining from previous Metadata"
                                    " %sFile AD: %lu bytes.\n",
                        (aiMirr != NULL || remSizeMirr != 0)
                          ? "Mirror " : "",
                        remSizeMain + remSizeMirr);
          MLIMITend;
        }
    }

    return isEqual;

}   /* end compareMetadataAllocationList() */

/* verifyMetadataDuplicateAllocation():
 * NOTE: Mind that if verifyMetadataDuplicateAllocation()
 *       returns TRUE, this does not mean that there is
 *       no error message printed.
 */
static bool verifyMetadataDuplicateAllocation(
                                UdfMountContext *mc,
                                bool duplicateFlag )
{
    Node    *nodeMain, *nodeMirr;
    bool     isEqual;

    /* check presence
     */
    if( mc->metadataFile == mc->metadataMirrorFile )
    {   if( mc->metadataFile != NULL ) UCTASSERT(FALSE);    /* please report */
        return TRUE;            /* equal, both NULL */
    }
    else if(   mc->metadataFile == NULL
            || mc->metadataMirrorFile == NULL )
    {   return FALSE;           /* unequal, one is NULL */
    }
    nodeMain = mc->metadataFile->node;
    nodeMirr = mc->metadataMirrorFile->node;

    /* check nodes
     */
    if( nodeMain == nodeMirr )
    {   return TRUE;            /* equal, maybe both NULL */
    }
    else if(   nodeMain == NULL
            || nodeMirr == NULL )
    {   return FALSE;           /* unequal, one is NULL */
    }

    /* Compare allocation descriptors list, extents shall
     * not be shared if the Duplicate Metadata Flag is set,
     * else, they shall be shared.
     * Test also ADs with type 3 (continuation) extents for AED.
     * These ADs are copied to al->overheadList.
     * They shall NEVER be shared by the Metadata File and its
     * mirror, even if the Duplicate Metadata Flag is NOT set !!!
     * isEqual is FALSE if any node[Main,Mirr]->al == NULL
     */
    isEqual =
        (   nodeMain->al != NULL
         && nodeMirr->al != NULL
         && compareMetadataAllocationList( mc,
                    nodeMain->al, nodeMirr->al,
                    duplicateFlag, FALSE)   /* NOT isOverheadList */

         && compareMetadataAllocationList( mc,
                    nodeMain->al->overheadList,
                    nodeMirr->al->overheadList,
                    duplicateFlag, TRUE) ); /* isOverheadList */

    return isEqual;

}   /* end verifyMetadataDuplicateAllocation() */

/* verifySingleMetadataFile():
 * Call with (   metaFile == mc->metadataFile
 *            || metaFile == mc->metadataMirrorFile
 *            || metaFile == NULL )
 */
static bool verifySingleMetadataAllocation(UdfMountContext *mc,
                                           FileNodeInfo    *metaFile)
{
    const MediumInfo  *vmi       = getTheMediumInfo();
    Uint32             blockSize = vmi->blockSize;
    Uint16             mPref     =  mc->metadataPref;
    PartitionMapInfo  *pmi       = &mc->partitionMapInfo[mPref];
    Uint16             cPref     =  pmi->counterpartPRef;
    UdfAllocationItem *ai;
    MetadataPartitionMapTail *mTail =
        &pmi->pPartitionMap->type2PartitionMap.SharedTail.metadataTail;
    Uint32   allocationUnit = MAX(1, mTail->allocationUnitSize),
             alignmentUnit  = MAX(1, mTail->alignmentUnitSize),
             logicalBlockNr;
    Uint8    adType;
    char    *fileName;

    if(   metaFile == NULL
       || metaFile->node == NULL
       || metaFile->node->al == NULL )
    {   return FALSE;
    }
    adType   = metaFile->node->al->itemAdType;
    fileName = (metaFile == mc->metadataFile)
                            ? "Metadata File"
                            : "Metadata Mirror File";

    /* Check ADs extent type, size and alignment
     * Note on extent type: UDF 2.50 2.2.13.1 says that
     * only ADEL_RECORDED_AND_ALLOCATED
     *  and ADEL_NOT_RECORDED_NOT_ALLOCATED are allowed.
     * However ADEL_EXTENTPOINTER must also be allowed.
     * This was corrected in the UDF 2.50 errata DCN-5104.
     * Therefore, we do not have to check the al->overheadList list,
     * because it only contains ADEL_EXTENTPOINTER ADs and no size
     * or alignment requirements are in force for these ADs.
     */
    for( ai = metaFile->node->al->head;
         ai != NULL;
         ai = ai->next )
    {
        Uint32 exLenPlusType = ai->aad.anyAd.extentLength,
               exSize       = elGetExtentSize(exLenPlusType),
               exNrBlocks  = ROUNDUPELEMENTS(exSize, blockSize);

        if(    elGetExtentType(exLenPlusType)
            == ADEL_NOT_RECORDED_BUT_ALLOCATED )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
               "\tError: %s AD extent type 1"
                                " (not recorded but allocated).\n"
              "-\t       Type not allowed for this file,"
                            " UDF 2.2.13.1, ECMA 4/14.14.1.1.\n",
                            fileName);
          MLIMITend;
          /** no FALSE result **/
        }

        if(       (exSize % blockSize) != 0
           || (exNrBlocks % allocationUnit) != 0 )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
               "\tError: Size of %s extent: %lu, expected: multiple of %lu.\n"
              "-\t       The size IN BLOCKS (%lu) shall be"
                                    " an integer multiple of the\n"
              "-\t       Partition Map Allocation Unit"
                                    " Size (%lu), UDF 2.2.13.1, 2.2.10.\n",
                        fileName, exSize, allocationUnit * blockSize,
                        exNrBlocks, allocationUnit);
          MLIMITend;
          /** no FALSE result **/
        }

        if( !udfGetLocation(&ai->aad, adType, cPref,
                             NULL, &logicalBlockNr) )
        { return FALSE; /* assert */
        }
        if( (logicalBlockNr % alignmentUnit) != 0 )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
               "\tError: Logical address of %s extent: %lu,\n"
              "-\t       expected: integer multiple of %lu"
                            " (Partition Map Alignment Unit),\n"
              "-\t       UDF 2.2.13.1, 2.2.10.\n",
                        fileName, logicalBlockNr, alignmentUnit);
          MLIMITend;
          /** no FALSE result **/
        }
    }
    return TRUE;

}   /* end verifySingleMetadataAllocation() */

/** getSparingPacketLength():
 ** returns sparing packet length from sparable partition map
 ** returns zero if no sparable partition exists.
 **/
static Uint16 getSparingPacketLength(UdfMountContext *mc)
{   PartitionMap *pm;

    if(   !IS_PREF_PARTITION_FOUND(mc->sparablePref)
       ||  mc->partitionMapInfo == NULL
       ||  mc->partitionMapInfo[mc->sparablePref].pPartitionMap == NULL
        )
    { return 0;     /* no sparable partition */
    }
    pm  = mc->partitionMapInfo[mc->sparablePref].pPartitionMap;
    return pm->type2PartitionMap.SharedTail.sparableTail.packetLength;
}

/* verifyMetadataFilesAllocation():
 * UDF 2.50 2.2.10.
 * Verifies allocation and alignment of the Metadata Partition.
 * Call after sparing and ECC packet lengths are established.
 */
extern bool verifyMetadataFilesAllocation(UdfMountContext *mc)
{
    Uint32  spPacketLength, eccLength, oneUnit;
    Uint16  metadataPref    = mc->metadataPref;
    bool    duplicateMetadadaFlag,
            isIdenticalAlloc;
    PartitionMapInfo         *pmi;
    MetadataPartitionMapTail *mTail;

    if( IS_PREF_PARTITION_NOT_FOUND(metadataPref) )
    {   return TRUE;        /* no Metadata Partition present */
    }
    pmi   = &mc->partitionMapInfo[metadataPref];
    mTail = &pmi->pPartitionMap->type2PartitionMap.SharedTail.metadataTail;

    /* Now perform Metadata Partition Map specific tests here, because most
     * of them could not yet be performed in verifyMetadataPartitionMap().
     *
     *  Uint32 metadataFileLocation;
     *  Uint32 metadataMirrorFileLocation;
     *  Uint32 metadataBitmapFileLocation;
     *
     * Test some pitfalls.
     * The File Entries for the metadataFile and metadataMirrorFile
     * must be unique and distinct, independent of the value of the
     * Duplicate Metadata Flag.
     */
    if(    mTail->metadataFileLocation
        == mTail->metadataMirrorFileLocation )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
          printMessageHead(MLIMIT.vl, (Byte*)mc->vi->lvd,
                    (Byte*)&mTail->metadataFileLocation, NULL);
          fprintf(uctout,
            "Error: Metadata File Location and Metadata Mirror\n"
            "-\t\t\tFile Location are equal (%lu). Each File\n"
            "-\t\t\tEntry must be unique and distinct, UDF 2.2.10.\n",
                        mTail->metadataFileLocation);
        MLIMITend;
        /** no FALSE result **/
    }

    /** Test if allocationUnitSize and alignmentUnitSize are
     ** multiples as specified.
     ** Test alignmentUnitSize first because it must be a multiple
     ** of 2 of the same values as for allocationUnitSize
     ** (ECC and sparing packet).
     **
     ** Uint16 alignmentUnitSize;
     ** Integral multiple of Max of 2 values:
     ** ECC block size and (sparing packetLenth if any).
     **
     ** Note that if no Sparable Partition is present,
     ** getSparingPacketLength() will return zero.
     **/
    spPacketLength = getSparingPacketLength(mc);
    eccLength = getTheMediumInfo()->eccLength;
    oneUnit = MAX(1, MAX(eccLength, spPacketLength));   /* not zero */
    if(    mTail->alignmentUnitSize == 0
       || (mTail->alignmentUnitSize % oneUnit) != 0 )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
          printMessageHead(MLIMIT.vl, (Byte*)mc->vi->lvd,
                    (Byte*)&mTail->alignmentUnitSize, NULL);
          fprintf(uctout,
            "Metadata Partition Map error: Alignment Unit Size: %lu,\n"
            "-\t\t expected: an integer multiple of %lu, UDF 2.2.10.\n%s",
                 mTail->alignmentUnitSize, oneUnit,
                (mTail->alignmentUnitSize != 0) ? ""
            : "-\t\t Note: It is obvious that this value cannot be zero.\n"
              "-\t\t       The UDF verifier will use 1 as Alignment Unit Size.\n");
        MLIMITend;
        /** no FALSE result **/
    }

    /** Uint32 allocationUnitSize;
     ** Integral multiple of Max of 3 values:
     ** 32, ECC block size, (sparing packetLenth if any)
     ** remark: if no sparable partition, then: spPacketLength == 0
     **
     ** so 32 and the same values as for alignmentUnitSize.
     **/
    oneUnit = MAX(32, oneUnit);         /* obviously not zero */
    if(    mTail->allocationUnitSize == 0
       || (mTail->allocationUnitSize % oneUnit) != 0 )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
          printMessageHead(MLIMIT.vl, (Byte*)mc->vi->lvd,
                    (Byte*)&mTail->allocationUnitSize, NULL);
          fprintf(uctout,
            "Metadata Partition Map error: Allocation Unit Size: %lu,\n"
            "-\t\t expected: an integer multiple of %lu, UDF 2.2.10.\n%s",
                 mTail->allocationUnitSize, oneUnit,
                (mTail->allocationUnitSize != 0) ? ""
            : "-\t\t Note: It is obvious that this value cannot be zero.\n"
              "-\t\t       The UDF verifier will use 1 as Allocation Unit Size.\n");
        MLIMITend;
        /** no FALSE result **/
    }

    /* Now check the allocation size and alignment of the Metadata files
     * depending on the Duplicate Metadata Flag.
     */
    duplicateMetadadaFlag = ((mTail->flags & MP_DUPMETADATA_MASK) != 0);

    /* verify if metadata file and its mirror have
     * AD allocations as required by duplicateMetadadaFlag.
     * isIdenticalAlloc == TRUE does not mean that there is no
     * error message printed by verifyMetadataDuplicateAllocation()
     */
    isIdenticalAlloc = verifyMetadataDuplicateAllocation(mc,
                                    duplicateMetadadaFlag);

    /* Now do tests on each single Metadata allocation.
     * do not call verifySingleMetadataAllocation() for mirror in
     * case Metadata File and mirror allocations are identical.
     */
    (void) verifySingleMetadataAllocation(mc, mc->metadataFile);
    if( !isIdenticalAlloc )
    { (void) verifySingleMetadataAllocation(mc, mc->metadataMirrorFile);
    }

    return TRUE;

}   /* end verifyMetadataFilesAllocation() */

/* verifyMetadataPartitionMap():
 * Metadata Partition Map introduced in UDF 2.50 2.2.10.
 * Note that Metadata Partition and Sparable Partition can
 * exist together.
 */
static bool verifyMetadataPartitionMap(Type2PartitionMap *pm2, Byte *d)
{
    MetadataPartitionMapTail *mTail = &pm2->SharedTail.metadataTail;
    char *txt1 = "Metadata Partition Map",
         *txt2 = "Reserved",
         *txt3 = "UDF 2.2.10";  /** UDF 2.50 spec !! **/
    bool  result = TRUE;

    /* introduced in UDF 2.50, revision conflict is fatal.
     */
    if( !modifyUdfRevisionRange(0x250, MAX_UDFREVISION, txt1) )
    {   return FALSE;   /* fatal */
    }

    /* The first part of the Metadata Partition Map until the
     * partitionNumber field included is tested outside this
     * function, except for the reserved1 field.
     *
     * Check reserved fields on RBP 2 and 59
     */
    verifyPartitionMapZeros(pm2->reserved1, sizeof(pm2->reserved1),
                            d, txt1, 2, txt2, txt3);
    verifyPartitionMapZeros(mTail->reserved2, sizeof(mTail->reserved2),
                            d, txt1, 59 , txt2, txt3);

    /* Metadata Partition Map specific tests
     *  Uint32 metadataFileLocation;
     *  Uint32 metadataMirrorFileLocation;
     *  Uint32 metadataBitmapFileLocation;
     *  Uint32 allocationUnitSize;
     *  Uint16 alignmentUnitSize;
     *
     * Fields above will be tested in verifyMetadataFilesAllocation()
     * when the allocation descriptors, etc. are read.
     *
     * Uint8 flags; - Only bit 0 is used for the Duplicate Metadata Flag.
     */
    if( (mTail->flags & ~MP_DUPMETADATA_MASK) != 0 )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
          printMessageHead(MLIMIT.vl, d,
                           (Byte*)&mTail->flags, NULL);
          fprintf(uctout,
            "Error: Metadata Partition Map Flags: #%02X. Only bit 0\n"
            "-\t\t\tmay be set (Duplicate Metadata Flag), UDF 2.2.10.\n",
                mTail->flags);
        MLIMITend;
        /** no FALSE result **/
    }
    return result;

}   /* end verifyMetadataPartitionMap() */


/* verifySparablePartitionMap():
 * Check Sparable Partition Map, UDF 2.2.9.
 * The Sparing Tables itself are checked in checkAndMapPartitionInfo()
 * where the Sparing Tables are actually read.
 */
static bool verifySparablePartitionMap(Type2PartitionMap *pm2, Byte *d)
{
    const MediumInfo *vmi  = getTheMediumInfo();
    char            *txt1  = "Sparable Partition Map",
                    *txt2r = "Reserved",
                    *txt2p = "Pad",
                    *txt3  = "UDF 2.2.9";
    SparablePartitionMapTail *sTail = &pm2->SharedTail.sparableTail;
    Uint16  spPacketLength = sTail->packetLength,
            minUdfRev, maxUdfRev;
    Uint32 *pLocOfTable, tableBlockSize, eccLen;
    Uint8   N_ST, n;
    bool    expectEqualToEccLen, result = TRUE;

    /* introduced in UDF 1.50, void possible revision conflict.
     */
    modifyUdfRevisionRange(0x150, MAX_UDFREVISION, txt1);

    /* The first part of the Sparable Partition Map until the
     * partitionNumber field included is tested outside this
     * function, except for the reserved1 field.
     *
     * Check reserved fields
     */
    verifyPartitionMapZeros(pm2->reserved1, sizeof(pm2->reserved1),
                            d, txt1, 2, txt2r, txt3);
    verifyPartitionMapZeros(&sTail->reserved2, 1,
                            d, txt1, 43 , txt2r, txt3);

    /* Rules for Sparing Packet Length differ for
     * UDF 2.00- and UDF 2.01+. Test 'on the safe side',
     * see warnIfUdfRevisionUncertain().
     */
    warnIfUdfRevisionUncertain(0x200, 0x201);
    minUdfRev = getUctMinUdfRevision();     /* maybe different, use */
    maxUdfRev = getUctMaxUdfRevision();     /*   'on the safe side' */
    eccLen = vmi->eccLength;

    /* For UDF 2.00-: Packet Length shall be 32. The sole exception
     *  is 16 for DVD, see errata DCN-5163, June 2006.
     *
     * For UDF 2.01+: Packetlength is equal to the ECC length. This is
     *  defined in the medium dependent recommandation sections 6.x of
     *  the most recent UDF revision specification.
     *
     * Test UDF revision 'on the safe side' and mind that
     * spPacketLength is an Uint16, while ECC length is Uint32 !!
     */
    expectEqualToEccLen = !(minUdfRev <= 0x200 && spPacketLength == 32);
    if(   spPacketLength == 0                       /* fatal error */
       || ( minUdfRev <= 0x200 && spPacketLength != 32
                               && spPacketLength != 16 )  /* error */
       || (   expectEqualToEccLen
           && (Uint32)spPacketLength != eccLen )          /* warn */
       ||    ((Uint32)spPacketLength % eccLen) != 0 )     /* warn */
    { Uint8 vLevel =
        (   spPacketLength == 0
         || (minUdfRev <= 0x200 && spPacketLength != 32
                                && spPacketLength != 16) )
            ? ERROR00level          /*   Error: */
            : WARN01level;          /* Warning: */
      MLIMITbegin(vLevel, uctMessageLimit);
        printMessageHead(MLIMIT.vl, d,
                         (Byte*)&sTail->packetLength, NULL);
        fprintf(uctout, "%8s %s Packet Length: %lu,\n"
                "-\t\texpected: ",
                (MLIMIT.vl == ERROR00level) ? "Error:" : "Warning:",
                txt1, spPacketLength);
        if( minUdfRev > 0x200 )       /* UDF 2.01+ */
        { fprintf(uctout, "%lu for UDF 2.01+, being the (ECC) blocking factor,\n"
            "-\t\tsee UDF 2.2.9 and most recent 6.x media recommendations.\n",
            eccLen);
        }
        else                           /* UDF 1.50 and 2.00 */
        { fprintf(uctout, "32 for UDF revisions 1.50 and 2.00, UDF 2.2.9.\n"
             "-\t\t The sole exception is that Packet Length 16 is also\n"
             "-\t\t allowed for DVD. This may however reduce compatibility,\n"
             "-\t\t see errata DCN-5163, June 2006.\n");
          if( (expectEqualToEccLen
                 && (Uint32)spPacketLength != eccLen )
             ||    ((Uint32)spPacketLength % eccLen) != 0 )
          { fprintf(uctout,
              "-\t\t Additionally, it is expected that the Packet Length (%lu)\n"
              "-\t\t is %s the (ECC) blocking factor (%lu)%s.\n",
              spPacketLength, (spPacketLength==16) ? "equal to"
                                                   : "an integer multiple of",
                      eccLen, (spPacketLength==16) ? " for DVD" : "");
          }
        }
        if( spPacketLength == 0 )
        { fprintf(uctout,
            "-\tNote: Sparing Packet Length zero is a fatal error.\n");
        }
      MLIMITend;
      if( spPacketLength == 0 )
      { result = FALSE;     /* fatal */
      }
    }
    else if( minUdfRev <= 0x200 && spPacketLength == 16 )   /* note: */
    { fprintf(uctout,   /** value 16 according to DCN-5163 exception */
         "\tNote: %s Packet Length 16 is allowed for DVD but\n"
        "-\t      this may reduce compatibility, see errata DCN-5163,"
                                           " June 2006.\n", txt1);
    }

    /** Check number of Sparing Tables
     **/
    N_ST = sTail->numberOfSparingTables;
    if( N_ST == 0 || N_ST > 4 )
    {
        MLIMITbegin(WARN01level, uctMessageLimit);
          printMessageHead(MLIMIT.vl, d,
                           (Byte*)&sTail->numberOfSparingTables, NULL);
          fprintf(uctout,
              "error: Number OF Sparing Tables out of range: %u,\n"
              "-\t\t\t%s, %s\n", N_ST, txt1, txt3);
        MLIMITend;
        result = FALSE;
    }
    else if( N_ST < 2 )
    {
        /** warning, no FALSE result **/
        MLIMITbegin(WARN01level,uctMessageLimit);
          printMessageHead(MLIMIT.vl, d,
                           (Byte*)&sTail->numberOfSparingTables, NULL);
          fprintf(uctout,
              "warning: Number of Sparing Tables: %lu,"
                                    " should be at least 2,\n"
              "-\t\t\t  %s, %s\n", N_ST, txt1, txt3);
        MLIMITend;
    }

    if(     sTail->sizeOfEachSparingTable < 56
        || (sTail->sizeOfEachSparingTable % 8) != 0 )
    {
        /** no FALSE result **/
        MLIMITbegin(ERROR00level,uctMessageLimit);
          printMessageHead(MLIMIT.vl, d,
                           (Byte*)&sTail->sizeOfEachSparingTable, NULL);
          fprintf(uctout,
            "error: Size Of Each Sparing Table: %lu,\n"
            "-\t\t\texpected: at least 56 and multiple of 8.\n"
            "-\t\t\t%s, %s\n",
            sTail->sizeOfEachSparingTable,
            txt1, txt3);
        MLIMITend;
    }

    /* Pad field
     */
    verifyPartitionMapZeros((&sTail->startOfLocationsOfSparingTables)
                               + N_ST * sizeof(Uint32),
                            16 - N_ST * sizeof(Uint32), d, txt1,
                            48 + N_ST * sizeof(Uint32), txt2p, txt3);

    /* LocationsOfSparingTables tests
     */
    tableBlockSize = ROUNDUPELEMENTS(sTail->sizeOfEachSparingTable,
                                     vmi->blockSize);
    for( n = 0, pLocOfTable = (Uint32*) &sTail->startOfLocationsOfSparingTables;
         n < N_ST;
         n++, pLocOfTable++ )
    {
        Uint32 *p2, packetNr = (*pLocOfTable) / sTail->packetLength;
        Uint32  n2;
        if( ((*pLocOfTable) % sTail->packetLength) != 0 )
        {
          MLIMITbegin(WARN01level,uctMessageLimit);
            printMessageHead(MLIMIT.vl, d, (Byte*)pLocOfTable, NULL);
            fprintf(uctout,
              "warning: Sparing Table %u Location: %lu, should be\n"
              "-\t\t\t  aligned on packet boundary, UDF 2.2.9\n",
              n+1, *pLocOfTable);
          MLIMITend;
        }
        /** TODO: use generic 'far apart' test as for VDS and Metadata Files ??
         **/
        for( n2 = n + 1, p2 = pLocOfTable + 1;
             n2 < N_ST;
             n2++, p2++ )
        {   Uint32 maxStart, minEnd, dist;
            if( ((*p2) / sTail->packetLength) == packetNr )
            {
              MLIMITbegin(ERROR00level,uctMessageLimit);
                printMessageHead(MLIMIT.vl, d, (Byte*)pLocOfTable, NULL);
                fprintf(uctout,
                  "error: Sparing Table %u and %u are located in same packet.\n"
                  "-\t\t\tLocations: %lu and %lu, %s\n",
                  n+1, n2+1, *pLocOfTable, *p2,
                  UDFREF_SPTAB(getUctUdfRevision()) );
              MLIMITend;
            }
            maxStart = MAX(*pLocOfTable, *p2);
            minEnd   = MIN(*pLocOfTable + tableBlockSize, /* mind: end+1 */
                                  (*p2) + tableBlockSize);
            dist = (maxStart < minEnd) ? 0              /* overlap */
                 : (maxStart - minEnd);
#define ST_MIN_DISTANCE     1000                        /* blocks */
            if( dist < ST_MIN_DISTANCE )
            { MLIMITbegin(WARN01level,uctMessageLimit);
                printMessageHead(MLIMIT.vl, d, (Byte*)pLocOfTable, NULL);
                fprintf(uctout,
                  "Warning: Less than %lu (%lu) blocks between Sparing\n"
                    "-\t\t Tables at %lu and %lu may not be considered:\n"
                    "-\t\t \"in physically distant locations\","
                                                    " UDF 2.2.9.\n",
                        ST_MIN_DISTANCE, dist, *pLocOfTable, *p2);
              MLIMITend;
            }
        }   /* endfor n2 */
    }   /* endfor n */

    return result;      /* FALSE is fatal for this descriptor */

}   /* end verifySparablePartitionMap() */

static bool verifyVirtualPartitionMap(Type2PartitionMap *pm2, Byte *d)
{
    char    *txt1 = "Virtual Partition Map",
            *txt2 = "Reserved",
            *txt3 = "UDF 2.2.8";

    /* introduced in UDF 1.50, void possible revision conflict.
     */
    modifyUdfRevisionRange(0x150, MAX_UDFREVISION, txt1);

    /* The first part of the Virtual Partition Map until the
     * partitionNumber field included is tested outside this
     * function, except for the reserved1 field.
     *
     * Check reserved fields
     */
    verifyPartitionMapZeros(pm2->reserved1, sizeof(pm2->reserved1),
                            d, txt1, 2, txt2, txt3);
    verifyPartitionMapZeros(pm2->SharedTail.virtualReserved2,
                            sizeof(pm2->SharedTail.virtualReserved2),
                            d, txt1, 40, txt2, txt3);
    return TRUE;

}   /* end verifyVirtualPartitionMap() */


/* Logical Volume Descriptor
 * ECMA      3/10.6 and 4/3.1
 * UDF 2.00 2.2.4
 */
static bool verifyLogicalVolumeDescriptor(LogicalVolumeDescriptor *d)
{
    const MediumInfo *vmi = getTheMediumInfo();
    Byte    *bpPmap;
    Uint8    pmgLen;
    Uint32   nm, npm, mtl, mtSize;
    Uint16   volumeSequenceNumber = 0;
    bool     result = TRUE;

    /* Uint32   volumeDescriptorSequenceNumber
     *          used/verified when reading VDS,
     *          see also storeLVDInUdfVolumeInformation().
     *
     * Charspec descriptorCharacterSet, UDF 2.2.4.1
     */
    (void) verifyCharspecCS0(&d->descriptorCharacterSet,
                    (Byte*) d, "Descriptor Character Set", "2.2.4.1");

    /* Dstring logicalVolumeIdentifier[128]
     * Relation of logicalVolumeIdentifier in LVD, IUVD, FSD
     * and VAT2 checked in checkVolumeIdentifiersSummary().
     */
    VERBOSE00(uctout, "\tLVD   Logical Volume Identifier: ");
    PRINTDSTRING(  d->logicalVolumeIdentifier,
            sizeof(d->logicalVolumeIdentifier), "\n");
    verifyDstring( d->logicalVolumeIdentifier, (Byte *)d,
            sizeof(d->logicalVolumeIdentifier),
                TRUE, "ECMA 3/10.6.4");     /* obligatory */

    /* Uint32 logicalBlockSize
     */
      if( d->logicalBlockSize != vmi->blockSize )
      { MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageExpectedU32( MLIMIT.vl, (Byte*)d,
            (Byte*) &d->logicalBlockSize,
               "Error: Inconsistent Logical Block Size",
                "%u", d->logicalBlockSize, vmi->blockSize,
            ",\n-\t\t\tshall be equal to medium info"
                                " logical sector size,\n"
               "-\t\t\tUDF 2., 2.2.4.2.\n" );
        MLIMITend;
      }

    /** Establish UDF revision, because we want
     ** to know the exact UDF revision.
     ** check domainIdentifier.
     ** EntityID domainIdentifier, UDF 2.2.4.3
     **/
    verifyEntityID( &d->domainIdentifier, ENTITY_SUFFIX_DOMAIN,
                     (Byte*)UDF_DOMAIN_ID, "UDF 2.2.4.3", (Byte*)d);
    /** So the exact UDF revision should be known by now.
     ** For tests where this is important,
     ** see warnIfUdfRevisionUncertain().
     **/

    /*    Byte   logicalVolumeContentsUse[16]
     * == LongAd fileSetDescriptorSequenceExtent
     * verifyLongAd() for
     * d->logicalVolumeContentsUse.fileSetDescriptorSequenceExtent
     * done in readFileSetDescriptor() because possible virtual or
     * metadata partition is not installed yet.
     *
     * Uint32 mapTableLength
     * Uint32 numberOfPartitionMaps
     * see partitionmap verification below
     */
    VERBOSE00(uctout, "\tLVD   FSD at: (%lu,p%u)\n",
      d->logicalVolumeContentsUse.fileSetDescriptorSequenceExtent.extentLocation.logicalBlockNumber,
      d->logicalVolumeContentsUse.fileSetDescriptorSequenceExtent.extentLocation.partitionReferenceNumber);

    /* EntityID implementationIdentifier
     */
    verifyEntityID( &d->implementationIdentifier,
                                ENTITY_SUFFIX_IMPL, NULL, NULL, (Byte*)d);

    /* Byte     implementationUse[128]  ** not used by UDF **
     *
     * ExtentAd integritySequenceExtent
     *
     * Extent length according to UDF 2.2.4.5/2.2.4.6 cannot yet
     * be tested here because writability type not yet known.
     * Tested later in readLogicalVolumeIntegrityDescriptor().
     */
    verifyExtentAd( &d->integritySequenceExtent,
                    (Byte*)d, TRUE );           /* isIntegralBlock */

    /* Byte startOfPartitionMaps            UDF 2.2.4.6/2.2.4.7
     * Now verify mapTableLength, numberOfPartitionMaps
     * and all partition maps.
     */
    mtl = d->mapTableLength;
    npm = d->numberOfPartitionMaps;

    /* Carefully check npm, mtl and partitionMapLength consistency.
     * Parse at most npm maps that fit in map table.
     * Proceed to next map if previous partitionMapLength is equal
     * to the expected length according to partitionMapType.
     * Mind that a Type 1 map is the smallest one possible
     *      (type 1 map size) is less than (type 2 map size).
     * For endian swap in case of map table inconsistencies,
     * see  endianSwapLogicalVolumeDescriptor().
     */
    bpPmap = &d->startOfPartitionMaps;
    for( nm = 0,      mtSize = 0;
         nm < npm && (mtSize + sizeof(Type1PartitionMap)) <= mtl;
         nm++,        mtSize += pmgLen,
                      bpPmap += pmgLen )
    {
        GenericPartitionMap *pmg = (GenericPartitionMap*) bpPmap;
        Type1PartitionMap   *pmt1 = NULL;
        Type2PartitionMap   *pmt2 = NULL;
        Uint32 mapSize;

        pmgLen = pmg->partitionMapLength;

        switch( pmg->partitionMapType )
        {
        case PMAP_TYPE1:
            pmt1 = (Type1PartitionMap *) pmg;
            volumeSequenceNumber = pmt1->volumeSequenceNumber;
            mapSize = sizeof(Type1PartitionMap);
            break;
        case PMAP_TYPE2:
            pmt2 = (Type2PartitionMap *) pmg;
            volumeSequenceNumber = pmt2->volumeSequenceNumber;
            mapSize = sizeof(Type2PartitionMap);
            break;
        default:
            MLIMITbegin(ERROR00level,uctMessageLimit);
              printMessageHead(MLIMIT.vl, (Byte*) d,
                        (Byte*) &pmg->partitionMapType, NULL);
              fprintf(uctout,
                "Fatal error: Unknown Partition Map Type: %u,\n"
                "-\t\t %s, ECMA 3/10.6.13.\n",
                        pmg->partitionMapType,
                        UDFREF_PMAP(getUctUdfRevision()) );
            MLIMITend;
            mapSize = 0;    /* map inconsistency */
            result = FALSE;
            break;
        }

        if( mapSize != 0 && mapSize != pmg->partitionMapLength )
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            printMessageHead(MLIMIT.vl, (Byte*) d,
                        (Byte*) &pmg->partitionMapLength, NULL);
            fprintf(uctout,
                    "Error: p%u Partition Map Length: %lu, shall be %lu,\n"
              "-\t\t %s, ECMA 3/10.6.13.\n",
                        nm, pmg->partitionMapLength, mapSize,
                        UDFREF_PMAP(getUctUdfRevision()) );
          MLIMITend;
          result = FALSE;
        }

        if(   mapSize == 0
           || mapSize > pmg->partitionMapLength
           || (mtSize + mapSize) > mtl )
        {
            break;  /* cannot parse this map, map inconsistency */
        }
        /* condition: mapSize bytes in map table available for parsing.
         * partitionMapType ok,
         * generic and PMAP_TYPE1 tests first,
         * partitionNumber checked later in checkAndMapPartitionInfo().
         */
        if( volumeSequenceNumber != 1 )
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            printMessageHead(MLIMIT.vl, (Byte*) d,
                    (pmg->partitionMapType == PMAP_TYPE1)
                        ? (Byte*) &pmt1->volumeSequenceNumber
                        : (Byte*) &pmt2->volumeSequenceNumber, NULL);
            fprintf(uctout,
                    "Error: p%lu partition map volumeSequenceNumber:"
                                            " %lu, should be 1\n"
              "-\t\t %s, ECMA 3/10.6.13.\n"
              "-\t\t The verifier does not support multi-volume"
                                    " Volume Sets, sorry.\n",
                        nm, volumeSequenceNumber,
                        UDFREF_PMAP(getUctUdfRevision()) );
          MLIMITend;
          /* no FALSE return result */
        }

        /* PMAP_TYPE2 specific tests
         */
        if( pmg->partitionMapType == PMAP_TYPE2 )
        {
            verifyEntityID(&pmt2->partitionTypeIdentifier,
                           ENTITY_SUFFIX_UDF, NULL, NULL, (Byte*)d);

            if(      memcmp(&pmt2->partitionTypeIdentifier.Identifier,
                            VIRTUAL_PARTITION_ID, ENTITYID_IDSIZE) == 0 )
            {
                result = verifyVirtualPartitionMap(pmt2, (Byte*)d);
            }
            else if( memcmp(&pmt2->partitionTypeIdentifier.Identifier,
                            SPARABLE_PARTITION_ID, ENTITYID_IDSIZE) == 0 )
            {
                result = verifySparablePartitionMap(pmt2, (Byte*)d);
            }
            else if( memcmp(&pmt2->partitionTypeIdentifier.Identifier,
                            METADATA_PARTITION_ID, ENTITYID_IDSIZE) == 0 )
            {
                result = verifyMetadataPartitionMap(pmt2, (Byte*)d);
            }
            else
            { Uint16 maxRev = getUctMaxUdfRevision();
              MLIMITbegin(ERROR00level, uctMessageLimit);
                printMessageHead(MLIMIT.vl, (Byte*) d,
                                 pmt2->partitionTypeIdentifier.Identifier,
                  "Fatal error: Unknown Type 2 Partition Map identifier:\n"
                  "-\t\t\t       ");
                printBytesName(pmt2->partitionTypeIdentifier.Identifier,
                               ENTITYID_IDSIZE, TRUE, MLIMIT.vl);
                fprintf(uctout,     "\n-\t   Known Type 2 Maps :");
                if(      maxRev < 0x150 )
                {   fprintf(uctout, " None for UDF 1.02\n");
                }
                else
                {   fprintf(uctout, "\n-\t     UDF 1.50+ 2.2.8 : ");
                    printBytesName( (Byte*) VIRTUAL_PARTITION_ID,
                                    ENTITYID_IDSIZE, TRUE, MLIMIT.vl);
                    fprintf(uctout, "\n-\t     UDF 1.50+ 2.2.9 : ");
                    printBytesName( (Byte*) SPARABLE_PARTITION_ID,
                                    ENTITYID_IDSIZE, TRUE, MLIMIT.vl);
                }
                if( maxRev >= 0x250 )
                {   fprintf(uctout, "\n-\t     UDF 2.50+ 2.2.10: ");
                    printBytesName( (Byte*) METADATA_PARTITION_ID,
                                    ENTITYID_IDSIZE, TRUE, MLIMIT.vl);
                }
                fprintf(uctout,"\n");
              MLIMITend;
              return FALSE; /* error, endian swap maybe incomplete */
            }
        }

        if( mapSize != pmg->partitionMapLength )
        {   /* mapSize < pmg->partitionMapLength
             * could parse this map, but cannot continue with
             * next one, because it may start
             * at: ((Byte*)pmg) + mapSize
             * or: ((Byte*)pmg) + pmg->partitionMapLength.
             */
            nm++;
            mtSize += mapSize;
            break;  /* map table inconsistency */
        }
    }       /* endfor, all maps done or map table inconsistency */
    /* endfor postconditions:
     *  nm is number of maps that could be parsed
     *  nmBytes is number of bytes parsed for nm maps
     */

    if( nm != npm || mtSize != mtl )
    {   MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
             "\t%s: LVD Partition Map Table inconsistency.\n"
            "-\t  numberOfPartitionMaps: %lu, mapTableLength: %lu.\n"
            "-\t  Parsed%s %lu of %lu Partition Maps.\n"
            "-\t  Parsed%s %lu of %lu Partition Map Table bytes.\n",
                      (nm != npm) ? "Fatal error" : "Error",
                npm, mtl, (nm < npm) ? " only" : "", nm, npm,
                      (mtSize < mtl) ? " only" : "", mtSize, mtl);
        MLIMITend;
        if( nm != npm )     /* fatal error */
        { result = FALSE;
        }
    }

    /* verifier assumes, npm fits in 16 bits.
     */
    if( npm > (Uint32) MAX_UINT16 )
    {   MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
            "======> Fatal abort error: Verifier cannot "
                        "handle %lu Partition Maps, sorry.\n",
                    npm);
        MLIMITend;
        result = FALSE;     /* fatal error */
    }
    return result;

}   /* end verifyLogicalVolumeDescriptor() */

/* Unallocated Space Descriptor
 * ECMA 3/10.8, UDF 2.2.5
 * Surprisingly, no sorting order of the USD extents is required.
 */
static bool verifyUnallocatedSpaceDescriptor(UnallocatedSpaceDescriptor *usd)
{
    const MediumInfo *vmi = getTheMediumInfo();
    ExtentAd *ead, *ead2;
    Uint32    adNr, adNr2;

#ifdef  DEBUG01
    MLIMITbegin(DEBUG01level, MLIMITdefault01); /* TODO: check presence of USD */
        implementationNotReady( "Unallocated Space Descriptor" );
    MLIMITend;
#endif  /* DEBUG01 */

    /* show and check ADs, start numbering with 1
     */
    for( adNr = 1, ead = (ExtentAd *) &usd->startOfAllocationDescriptors;
         adNr <= usd->numberOfAllocationDescriptors;
         adNr++,   ead++ )
    {   Uint32 nrBlocks = ROUNDUPELEMENTS(ead->extentLength, vmi->blockSize);
        VERBOSE00(uctout, "\t%2lu: location %7lu", adNr, ead->extentLocation);
        if( nrBlocks > 1 )
             VERBOSE00(uctout, " thru %9lu,", ead->extentLocation + nrBlocks - 1);
        else VERBOSE00(uctout,       ",%15s", "");

        VERBOSE00(uctout, " %8lu block%s\n", nrBlocks, PLURAL_S(nrBlocks));

        verifyExtentAd(ead, (Byte*) usd, TRUE); /* isIntegralBlock */
        if( ead->extentLength == 0 )
        { MLIMITbegin( ERROR00level, MLIMITdefault10 );
            printMessageHead(MLIMIT.vl, (Byte*)usd, (Byte*) ead,
                "Error: Unspecified USD Allocation Descriptor (zero length),\n"
                "-\t\t\tIf no USD extents, then specify zero for Number of\n"
                "-\t\t\tAllocation Descriptors, ECMA 3/10.8, 3/7.1.2.\n");
          MLIMITend;
        }
    }

    /* warn for overlapping ADs
     */
    for( adNr = 1, ead = (ExtentAd *) &usd->startOfAllocationDescriptors;
         adNr <= usd->numberOfAllocationDescriptors;
         adNr++, ead++ )
    {   Uint32 nrBlocks = ROUNDUPELEMENTS(ead->extentLength, vmi->blockSize);
        for( adNr2 = adNr + 1, ead2 = ead + 1;
             adNr2 <= usd->numberOfAllocationDescriptors;
             adNr2++, ead2++ )
        { Uint32 nb2 = ROUNDUPELEMENTS(ead2->extentLength, vmi->blockSize);
          if( calculateOverlap( ead->extentLocation, nrBlocks,
                                ead2->extentLocation, nb2,
                                NULL, NULL ) )
          { MLIMITbegin( WARN01level, MLIMITdefault10 );
              printMessageHead(MLIMIT.vl, (Byte*)usd, (Byte*) ead, NULL);
              fprintf(uctout,
                      "Warning: USD Allocation Descriptors overlap,\n"
                      "-\t\t\t  %u%s extent overlaps with %u%s extent,\n"
                "-\t\t UDF 2.2.5, ECMA 3/8.4.1, 3/8.5, 3/10.8, 3/7.1.2.\n",
                adNr, XTH2(adNr), adNr2, XTH2(adNr2));
            MLIMITend;
          }
        }   /* endfor */
    }       /* endfor */

    return TRUE;

}   /* end verifyUnallocatedSpaceDescriptor() */

static bool verifyTerminatingDescriptor(TerminatingDescriptor *td)
{
    /* TD has only Tag and reserved field.
     */
    if( !verifyZeros(td->reserved, sizeof(td->reserved),
                     NULL, NULL, NULL) )
    {   /* Note that verifyZerosInDescriptor() assumes to be
         * called from within a MLIMITbegin(ERROR00level, ...)
         * ... MLIMTend clause.
         */
        MLIMITbegin(ERROR00level,uctMessageLimit);
          (void) verifyZerosInDescriptor(
                            (Byte*) td, td->reserved,
                            sizeof(td->reserved), NULL);
          fprintf(uctout,
            "-\t\t Reserved field error,"
                    " ECMA 3/10.9.2, 4/14.2.2\n");
        MLIMITend;
        /** no FALSE result **/
    }
    return TRUE;

}   /* end verifyTerminatingDescriptor() */

/* Logical Volume Header Descriptor (LVHD)
 * ECMA 4/3.1, 4/14.15
 * UDF 2.00 3.2.1
 * Located in logicalVolumeContentsUse of LVID
 */
static bool verifyLogicalVolumeHeaderDescriptor(
                  LogicalVolumeIntegrityDescriptor *lvid )
{
    LogicalVolumeHeaderDescriptor *lvh =
        &lvid->logicalVolumeContentsUse.logicalVolumeHeaderDescriptor;

    /* Uint64 uniqueID      - so called LVID 'Next UniqueID' value
     * This value is tested in verifyTimeAndSetNextUniqueId() for the normal
     * case, where the 'Next UniqueID' value is taken from this field.
     * For the case of a sequential file system, the 'Next UniqueId' is
     * taken from the incremented VAT FE UniqueID field.
     * In the latter case, the LVHD UniqueID here is considered to be
     * don't care, so no testing needed here.
     */

    /* Byte reserved[24] - zero bytes
     */
    if( !verifyZeros(lvh->reserved, sizeof(lvh->reserved),
                     NULL, NULL, NULL) )
    {   /* Note that verifyZerosInDescriptor() assumes to be
         * called from within a MLIMITbegin(ERROR00level, ...)
         * ... MLIMTend clause.
         * LVHD dus not have a tagId, its in the LVID.
         */
        MLIMITbegin(ERROR00level,uctMessageLimit);
          (void) verifyZerosInDescriptor(
                            (Byte*) lvid, lvh->reserved,
                            sizeof(lvh->reserved), "LVHD");
          fprintf(uctout,
            "-\t\t Reserved field error,"
                    " ECMA 4/14.15.2\n");
        MLIMITend;
        /** no FALSE result **/
    }
    return TRUE;

}   /* end verifyLogicalVolumeHeaderDescriptor() */

/* verifyLvidImplementationUse
 * syntax checks only here, further see verifyFinalLVIDandVAT().
 * UDF 2.2.6.4
 */
static bool verifyLvidImplementationUse(LvidImplementationUse *iu,
                                        Byte *d)
{
    (void) verifyEntityID(&iu->implementationID,
                           ENTITY_SUFFIX_IMPL, NULL, NULL, d);

    /* Uint32 numberOfFiles;
     * Uint32 numberOfDirectories;
     */

    (void) verifyUdfRevision(iu->minimumUDFReadRevision,
                             MIN_UDFREVISION, MAX_UDFREVISION,
                    "LVID Minimum UDF Read  Revision", "UDF 2.2.6.4");
    (void) verifyUdfRevision(iu->minimumUDFWriteRevision,
                             MIN_UDFREVISION, MAX_UDFREVISION,
                    "LVID Minimum UDF Write Revision", "UDF 2.2.6.4");
    (void) verifyUdfRevision(iu->maximumUDFWriteRevision,
                             MIN_UDFREVISION, MAX_UDFREVISION,
                    "LVID Maximum UDF Write Revision", "UDF 2.2.6.4");

    /* remaining implementation use:
     * Byte implementationUse[lvid.lengthOfImplementationUse-46];
     */
    return TRUE;
}

/* Logical Volume Integrity Descriptor
 * syntax checks only here, further see verifyFinalLVIDandVAT().
 * ECMA 3/10.10, UDF 2.2.6
 */
static bool verifyLogicalVolumeIntegrityDescriptor(
                                LogicalVolumeIntegrityDescriptor *lvid,
                                UdfMountContext *mc)
{   Uint16 nMaps;

#ifdef  DEBUG01
    MLIMITbegin(DEBUG01level, MLIMITdefault01);
        implementationNotReady( "Logical Volume Integrity Descriptor" );
    MLIMITend;
#endif  /* DEBUG01 */

    VERBOSE00(uctout, "\tLVID Recording Time: ");
    printTimestampShort(&lvid->recordingDateAndTime, TRUE, "\n");

    verifyTimestamp(&lvid->recordingDateAndTime,"ECMA 3/10.10.2",
                    "LVID Recording Time", (Byte*) lvid, mc, NULL);

    if( lvid->integrityType > LVIDINTEGRITY_MAX )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        printMessageHead(MLIMIT.vl, (Byte*)lvid,
                         (Byte*) &lvid->integrityType, NULL);
        fprintf(uctout, "Error: Invalid LVID Integrity Type: %lu,\n"
            "-\t\t\tassumed open, ECMA 3/10.10.3.\n", lvid->integrityType);
      MLIMITend;
    }

    /* lvid->numberOfPartitions shall be equal
     * to LVD numberOfPartitionMaps.
     */
    if( mc && mc->vi && mc->vi->lvd )       /* assert */
    { nMaps = (Uint16) mc->vi->lvd->numberOfPartitionMaps;
      if( lvid->numberOfPartitions != nMaps )
      { MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageExpectedU32( MLIMIT.vl, (Byte*)lvid,
            (Byte*) &lvid->numberOfPartitions,
              "Error: Number of Partitions", "%u",
              lvid->numberOfPartitions, nMaps,
            ",\n-\t\t shall be equal to number of partition maps in LVD,\n"
               "-\t\t ECMA 3/10.10.6+8+9, UDF 2.2.6.2+3.\n" );
        MLIMITend;
      }
    }

    if( lvid->lengthOfImplementationUse < 46 )      /* UDF 2.2.6.4 */
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        printMessageHead( MLIMIT.vl, (Byte*)lvid,
            (Byte*) &lvid->lengthOfImplementationUse, NULL);
        fprintf(uctout,
                "Error: Length of Implementation Use: %lu,\n"
          "-\t\t expected: at least 46, ECMA 3/10.10.7, UDF 2.2.6.4.\n",
                lvid->lengthOfImplementationUse);
      MLIMITend;
    }

    verifyExtentAd( &lvid->nextIntegrityExtent,
                    (Byte*)lvid, TRUE); /* isIntegralBlock */
    verifyLogicalVolumeHeaderDescriptor(lvid);

    /* tables and implementation use:
     *  Uint32      FreeSpaceTable[numberOfPartitions];
     *  Uint32      SizeTable[numberOfPartitions];
     *  union {
     *    Byte                  iuBytes[lengthOfImplementationUse];
     *    LvidImplementationUse lvidImplementationUse;
     *  } implementationUse;
     */
    verifyLvidImplementationUse( (LvidImplementationUse*)
            (&lvid->startOfTables + 2 * lvid->numberOfPartitions * sizeof(Uint32)),
                                 (Byte*) lvid );

    return TRUE;

}   /* end verifyLogicalVolumeIntegrityDescriptor() */


/* checkMinMaxReadWriteRevisions() group of functions.
 * Message printing:
 * messMinMaxReadWriteUnknown():
 *    Print "Unknown Minimum/Maximum UDF Read/Write Revision" message.
 * messMinMaxReadWriteLowHigh():
 *    Print "Minimum/Maximum UDF Read/Write Revision
 *                  lower/higher than Medium UDF Revision" message.
 */
static void messMinMaxReadWriteUnknown(
                    bool  verbose, Uint16 value,
                    char *txtMinMax, char *txtReadWrite )
{ if( verbose ) fprintf(uctout,
            "-\t     => Unknown %simum UDF %s Revision 0x%03X.\n",
                    txtMinMax, txtReadWrite, value);
}
static void messMinMaxReadWriteLowHigh(
                    bool  verbose, Uint16 value, char *txtMinMax,
                    char *txtReadWrite, char *txtLowHigh )
{ if( verbose ) fprintf(uctout,
            "-\t     => %simum UDF %s Revision 0x%03X"
                            " %s than Medium UDF Revision.\n",
                    txtMinMax, txtReadWrite, value, txtLowHigh);
}

/* checkMinMaxReadWriteRevisions():
 * Check features that are 'current' for this UDF file system
 * and determine Minimum UDF Read Revision
 *           and Minimum UDF Write Revision values.
 *
 * Note: The exact UDF revision must be known here !!
 */
static bool checkMinMaxReadWriteRevisions(
                UdfMountContext *mc,
                Uint16 minRead,  Uint16 minWrite,
                Uint16 maxWrite, Uint16 fsUdfRevision,
                VirtualRecord *pVatRec, bool verbose )
{   Uint32 n;
    bool   result = TRUE;       /* MEANS OK */

    /* first check if valid values for Min/Max UDF Read/Write Revisions
     * and if current UDF revision is within Min/Max UDF Revision range
     * test minRead:
     */
    if( !isKnownUdfRevision(minRead) )
    { result = FALSE;                   /* error */
      messMinMaxReadWriteUnknown(verbose, minRead,
                                 "Min", "Read");
      minRead = MAX_UDFREVISION;   /* avoid further messages */
    }
    else if( minRead > fsUdfRevision )
    { result = FALSE;                   /* error */
      messMinMaxReadWriteLowHigh(verbose, minRead,
                                 "Min", "Read", "higher");
    }

    /* test minWrite:
     */
    if( !isKnownUdfRevision(minWrite) )
    { result = FALSE;                   /* error */
      messMinMaxReadWriteUnknown(verbose, minWrite,
                                 "Min", "Write");
      minWrite = MAX_UDFREVISION;  /* avoid further messages */
    }
    else if( minWrite > fsUdfRevision )
    { result = FALSE;                   /* error */
      messMinMaxReadWriteLowHigh(verbose, minWrite,
                                 "Min", "Write", "higher");
    }

    /* test maxWrite:
     */
    if( !isKnownUdfRevision(maxWrite) )
    { result = FALSE;                   /* error */
      messMinMaxReadWriteUnknown(verbose, maxWrite,
                                 "Max", "Write");
      maxWrite = fsUdfRevision;    /* avoid further messages */
    }
    else if( maxWrite < fsUdfRevision )
    { result = FALSE;                   /* error */
      messMinMaxReadWriteLowHigh(verbose, maxWrite,
                                 "Max", "Write", "lower");
    }

    /* Now check used UDF properties in decreasing UDF revision order.
     * UDF 2.60: write Pseudo Overwritable Partition
     * UDF 2.50: read Pseudo Overwritable Partition
     */
    if(   mc->vi != NULL && mc->vi->lvd != NULL
       && mc->partitionMapInfo != NULL )
    { for( n = 0; n < mc->vi->lvd->numberOfPartitionMaps;
           n++ )
      { if( mc->partitionMapInfo[n].pdPointer->accessType == PDAT_POW_OR_UNKNOWN )
        { if( minWrite < 0x260 )    /* UDF 2.60+ needed */
          { result = FALSE;         /* error */
            if( verbose ) fprintf(uctout,
                "-\t     => UDF 2.60+ needed to write pseudo-overwritable partition.\n");
          }
          if( minRead < 0x250 )     /* UDF 2.50+ needed */
          { result = FALSE;         /* error */
            if( verbose ) fprintf(uctout,
                "-\t     => UDF 2.50+ needed to read pseudo-overwritable partition.\n");
          }
        }
      }
    }

    /* UDF 2.50: Metadata Partition
     */
    if(   IS_PREF_PARTITION_FOUND(mc->metadataPref)
       && (minRead < 0x250 || minWrite < 0x250) )
    { result = FALSE;                   /* error */
      if( verbose ) fprintf(uctout,
            "-\t     => UDF 2.50+ needed to read/write Metadata Partition.\n");
    }

    /* UDF 2.00: New ECMA-167 3rd edition structures
     */
    if(    fsUdfRevision >= 0x200       /* UDF 2.00+ */
        && (minRead  < 0x200 || minWrite < 0x200) )
    { result = FALSE;                   /* error */
      if( verbose ) fprintf(uctout,
            "-\t     => UDF 2.00+ needed to read/write ECMA-167 3rd edition structures.\n");
    }

    /* UDF 2.00: virtual partition (VAT200)
     * UDF 1.50: virtual partition (VAT150)
     */
    if( pVatRec != NULL )   /* VAT 1.50 or 2.00 */
    { if(    pVatRec->vatIntroRevision == 0x200
         && (minRead < 0x200 || minWrite < 0x200) )
      { result = FALSE;             /* error */
        if( verbose ) fprintf(uctout,
            "-\t     => UDF 2.00+ needed to read/write VAT 2.xx format.\n");
      }
      if(    pVatRec->vatIntroRevision == 0x150
         && (minRead < 0x150 || minWrite < 0x150) )
      { result = FALSE;             /* error */
        if( verbose ) fprintf(uctout,
            "-\t     => UDF 1.50+ needed to read/write VAT 1.50 format.\n");
      }
    }

    /* UDF 1.50: Sparable Partition
     */
    if(   IS_PREF_PARTITION_FOUND(mc->sparablePref)
       && (minRead < 0x150 || minWrite < 0x150) )
    { result = FALSE;               /* error */
      if( verbose ) fprintf(uctout,
            "-\t     => UDF 1.50+ needed to read/write Sparable Partition.\n");
    }
    return result;

}   /* end checkMinMaxReadWriteRevisions() */

/* verifyFinalLVIDandVAT():
 * Verify LVID and VAT field after Free Space,
 * Number of directories, etc. have been determined.
 * Mind that most syntax checks have already be done
 * in verifyLogicalVolumeIntegrityDescriptor()
 * and swapAndVerifyVatFile().
 * ECMA 3/10.10, UDF 2.2.6
 *
 * Implementation Notes:
 *  1) verifyFinalLVIDandVAT() must be called AFTER
 *     verifyPartitionAllocation().
 *  2) totalNumberOfFilesMFD and totalNumberOfDirsMFD are included
 *     in totalNumberOfFiles, totalNumberOfDirs respectively.
 */
extern bool verifyFinalLVIDandVAT(UdfMountContext *mc,
                                  Uint32 totalNumberOfFiles,
                                  Uint32 totalNumberOfDirs,
                                  Uint32 totalNumberOfFilesMFD,
                                  Uint32 totalNumberOfDirsMFD)
{
    const MediumInfo *vmi = getTheMediumInfo(); /* verifier MediumInfo */
    LogicalVolumeIntegrityDescriptor *lvid = mc->lvid;
    LvidImplementationUse *liu;
    VirtualRecord         *pVatRec;
    Timestamp             *pLastModTime;
    Byte                  *pLastDeveloperId;
    Uint32  *pFreeSpaceTable, *pSizeTable;
    Uint32   n, numberOfFiles, numberOfDirectories;
    Uint16   pRef, nMaps;
    Uint16   udfRevision, minUdfRead, minUdfWrite, maxUdfWrite;
    bool     isCloseLvid, handleAsCloseLvid, isDirtyLvid,
             isSequential = (vmi->sequentialType == MTYPE_SE_SEQUENTIAL),
             earlierErrorsPresent, extraNotePrinted;
    Byte    *descr;
    char     udfRef[100], txtFromLvid[100], /* 100 must be enough */
            *lvidStatusText,
            *extraNoteTxt =
        "-\tNote: The verifier may be unable to find the"
                                        " correct values for\n"
        "-\t      some of these LVID/VAT tests because of"
                                          " previous errors.\n";

    /* macro: maybe print extra Note once
     */
#define PRINT_EXTRA_NOTE    \
{   if( earlierErrorsPresent && !extraNotePrinted ) \
    {   extraNotePrinted = TRUE;            \
        VERBOSE00(uctout, extraNoteTxt);    \
    }   \
}
    /* earlierErrorsPresent ignores EntityID errors on purpose.
     */
    earlierErrorsPresent = (uctGlobalErrorCount != 0);
    extraNotePrinted = FALSE;

    if( lvid == NULL )
    {   return FALSE;
    }
    udfRevision = getUctUdfRevision();

    /* for dirty nonsequential medium, handle as Close
     */
    isCloseLvid = (lvid->integrityType == LVIDINTEGRITY_CLOSE);
    handleAsCloseLvid = (isCloseLvid || !isSequential);
    isDirtyLvid       = (isCloseLvid != handleAsCloseLvid);
    lvidStatusText    = (isDirtyLvid) ? "Dirty"
                      : (isCloseLvid) ? "Close" : "Open";

    VERBOSE00(uctout, "\t%s LVID%s\n", lvidStatusText,
                (isDirtyLvid) ? ", verify as Close" : "");

    /* tables and implementation use:
     *  Uint32      FreeSpaceTable[numberOfPartitions];
     *  Uint32      SizeTable[numberOfPartitions];
     *  union {
     *    Byte                  iuBytes[lengthOfImplementationUse];
     *    LvidImplementationUse lvidImplementationUse;
     *  } implementationUse;
     */
    pFreeSpaceTable = (Uint32*) &lvid->startOfTables;
    pSizeTable = pFreeSpaceTable + lvid->numberOfPartitions;
    liu = (LvidImplementationUse*)
                     (pSizeTable + lvid->numberOfPartitions);

    /* verify number of numberOfPartitions
     * together with FreeSpaceTable, etc.
     */
    UCTASSERT( mc && mc->vi && mc->vi->lvd );

    nMaps = (Uint16) mc->vi->lvd->numberOfPartitionMaps;
    if( lvid->numberOfPartitions != nMaps )
    { Uint32 mxp;
      MLIMITbegin(ERROR00level, uctMessageLimit);
        printMessageExpectedU32( MLIMIT.vl, (Byte*)lvid,
            (Byte*) &lvid->numberOfPartitions,
            "Error: Number of Partitions", "%u",
            lvid->numberOfPartitions, nMaps,
         ".\n-\t\t\tShall be equal to Number of Partition Maps\n"
            "-\t\t\tin LVD, ECMA 3/10.10.6+8+9, UDF 2.2.6.2+3.\n"
            "-\tNote: Unable to verify Free Space and Size Tables.\n"
            "-\t      Free Space Table:");
        /* print Free Space and Size Tables, limited numberOfPartitions
         */
        mxp = MIN(3, lvid->numberOfPartitions);
        for( n = 0; n < mxp; n++ )
        { fprintf(uctout, " %4lu", pFreeSpaceTable[n] );
        }
        fprintf(uctout,
          "%s\n-\t      Size Table      :",
            (n == lvid->numberOfPartitions) ? "" : " ...");
        for( n = 0; n < mxp && lvid->numberOfPartitions <= 100; n++ )
        { fprintf(uctout, " %4lu", pSizeTable[n]);
        }
        fprintf(uctout, "%s\n",
            (n == lvid->numberOfPartitions) ? "" : " ...");
      MLIMITend;
      fprintf(uctout, "\n");
    }
    else    /* lvid->numberOfPartitions ok */
    {
        for( pRef = 0; pRef < nMaps; pRef++ )
        {   Uint32 expFreeSpace, expSize;
            PartitionMapInfo *pmi  = &mc->partitionMapInfo[pRef];
            Uint32            pLen = pmi->actualPartitionLength;
            Uint32      accessType = pmi->pdPointer->accessType;
            Uint32   lvidFreeSpace = pFreeSpaceTable[pRef],
                     lvidParSize   = pSizeTable[pRef];
            bool     freeSpaceError, isVirtualOrPow,
                     nonSequentialWriteOnce;

            isVirtualOrPow =
                        (   pmi->pMapType == PMAPTYPE_VIRTUAL
                         || (   accessType == PDAT_POW_OR_UNKNOWN
                             && udfRevision >= 0x260) );
            nonSequentialWriteOnce =
                        (   pmi->pMapType != PMAPTYPE_VIRTUAL
                         && accessType == PDAT_WRITEONCE);

            /* UDF 2.2.6.2+3 Free Space Table and Size Table.
             */
            expSize = (pmi->pMapType == PMAPTYPE_VIRTUAL)
                        ? MAX_UINT32
                        : pLen;     /* partition length */

            if( isVirtualOrPow )
            {   expFreeSpace = MAX_UINT32;
            }
            else if( accessType == PDAT_READONLY )
            {   expFreeSpace = 0;
            }
            else    /* calculatedFreeSpace may be MAX_UINT32 */
            {       /*      if bitmap could not be allocated */
                expFreeSpace = pmi->calculatedFreeSpace;
            }

            VERBOSE00(uctout,
               "  ==>\t%s %s Partition p%u Space summary:\n"
                    "\t%38s: %7lu\n"
                    "\t%38s: %7lu%s\n"
                    "\t%38s: %7lu%s\n",
                      PDAT_TEXT(accessType),
                      PMAPTYPE_TEXT(pmi->pMapType), pRef,
                      "Partition Length    ", pLen,
                      "LVID Partition Size      ", lvidParSize,
                  (lvidParSize == MAX_UINT32) ? " (#FFFFFFFF)" : "",
                      "LVID Partition Free Space", lvidFreeSpace,
                (lvidFreeSpace == MAX_UINT32) ? " (#FFFFFFFF)" : "");

            if( pmi->pss.status & F_PSS_LOADED )    /* Space Set present */
            { VERBOSE00(uctout,
                    "\t%27s free space: %7lu\n",
                partitionSpaceSetText(&pmi->pss), pmi->spaceSetsFreeSpace);
            }
            if( isVirtualOrPow || expFreeSpace != MAX_UINT32 )
            { VERBOSE00(uctout, "\t%38s: %7lu%s%s\n",
                "Verifier expected free space", expFreeSpace,
                (expFreeSpace == MAX_UINT32) ? " (#FFFFFFFF)" : "",
                (nonSequentialWriteOnce)     ? " at most" : "");
            }
            if( pmi->usedUnallocatedBlocks != 0 )
            { VERBOSE00(uctout, "\t%38s: %7lu\n",
                "Used by UDF but unallocated blocks",
                pmi->usedUnallocatedBlocks);
            }
            if( pmi->unusedAllocatedBlocks != 0 )
            { VERBOSE00(uctout, "\t%38s: %7lu\n",
                (pmi->pMapType == PMAPTYPE_VIRTUAL)
                    ? "Unused by UDF, not marked free in VAT"
                    : "Unused by UDF but allocated blocks",
                pmi->unusedAllocatedBlocks);
            }
            VERBOSE00(uctout, "\n");

            /* End Space summary,
             * Check consistency between LVID Partition Free Space and
             * Unallocated/Freed Space Set (Bitmap or Table), if any.
             * These values must be consistent, also for read-only
             * and write-once, but normally read-only and write-once
             * have no Space Set defined.
             * No check on virtual or POW partitions or if value
             * could not be determined.
             * No message here if pmi->spaceSetsFreeSpace == expFreeSpace
             * to avoid message for both this case and a later
             * lvidFreeSpace versus expFreeSpace error message.
             */
            if(    expFreeSpace != MAX_UINT32   /* no virtual, POW, error */
               && (pmi->pss.status & F_PSS_LOADED)   /* Space Set present */
               &&  pmi->spaceSetsFreeSpace != lvidFreeSpace
               &&  pmi->spaceSetsFreeSpace != expFreeSpace )
            {
              MLIMITbegin(ERROR00level, uctMessageLimit);
                printMessageHead( MLIMIT.vl, (Byte*)lvid,
                    (Byte*) &pFreeSpaceTable[pRef], NULL);
                fprintf(uctout,
                  "Error: LVID and %s inconsistent\n"
                  "-\t\t\tPartition Free Space for a %s LVID,\n"
                  "-\t\t\tsee Space summary above and maybe use\n"
                  "-\t\t\t-showalloc output, ECMA 3/10.1, UDF 2.2.6.2.\n",
                    partitionSpaceSetText(&pmi->pss), lvidStatusText);
              MLIMITend;
            }

            /* Check lvidFreeSpace with verifier calculated expFreeSpace:
             * If expFreeSpace == MAX_UINT32, then it is a virtual or
             * pseudo-overwritable partition, or the verifier could not
             * not determine free space for this partition.
             * For a nonsequential write once partition, lvidFreeSpace
             * may be less than the verifier calculated expFreeSpace
             * because blocks cannot be re-used.
             * Virtual partition value is also valid for Open LVID,
             * else Close LVID only.
             */
            freeSpaceError = FALSE;
            if(   (!isVirtualOrPow)
               && (expFreeSpace == MAX_UINT32 || pmi->partAllocOutOfMem))
            { MLIMITbegin(WARN01level,uctMessageLimit);
                VERBOSE00(uctout,
                  "  ====>\tWarning: Unable to verify FreeSpaceTable because\n"
                              "-\t\t of previous (out of memory) errors.\n");
              MLIMITend;
            }
            else if(   (!isVirtualOrPow)
                    && lvidFreeSpace == MAX_UINT32 )
            {   freeSpaceError = TRUE;      /* UDF 2.2.6.2 */
            }
            else if(         handleAsCloseLvid
                     &&      lvidFreeSpace != expFreeSpace
                     && (    lvidFreeSpace >  expFreeSpace
                         || !nonSequentialWriteOnce) )
            {   freeSpaceError = TRUE;
            }
            /* mind exception of NO error for:
             * nonSequentialWriteOnce AND lvidFreeSpace less than expected
             */

            if( freeSpaceError )
            { Uint32 val32;
              MLIMITbegin(ERROR00level, uctMessageLimit);
                printMessageHead( MLIMIT.vl, (Byte*)lvid,
                        (Byte*) &pFreeSpaceTable[pRef], NULL);
                fprintf(uctout,
                  "FreeSpaceTable error: %s Partition p%u Free Space: ",
                            PMAPTYPE_TEXT(pmi->pMapType), pRef);
                fprintf(uctout, (lvidFreeSpace == MAX_UINT32)
                        ? "#%X" : "%lu", lvidFreeSpace);
                fprintf(uctout, ",\n-\t\t expected: ");

                val32 = expFreeSpace;
                if(   pmi->pMapType != PMAPTYPE_VIRTUAL
                   && lvidFreeSpace == MAX_UINT32 )
                { fprintf( uctout, "not equal to ");
                  val32 = MAX_UINT32;
                }
                else if( nonSequentialWriteOnce )
                { fprintf( uctout, "at most ");
                }
                fprintf(uctout, (val32 == MAX_UINT32)
                        ? "#%X" : "%lu", val32);
                fprintf(uctout, ", see Space summary above and\n"
                     "-\t\t maybe use -showalloc output,"
                                " UDF 2.2.6.2, ECMA 3/10.10.\n");
                PRINT_EXTRA_NOTE;
              MLIMITend;
            }

            /* Size Table, expected size
             */
            if( lvidParSize != expSize )
            { MLIMITbegin(ERROR00level, uctMessageLimit);
                printMessageHead( MLIMIT.vl, (Byte*)lvid,
                    (Byte*) &pSizeTable[pRef], NULL);
                fprintf(uctout,
                  "SizeTable error: %s Partition p%u Size: %lu%s,\n"
                  "-\t\t\texpected: %s,\n"
                  "-\t\t\tsee Space summary above, UDF 2.2.6.3.\n",
                   PMAPTYPE_TEXT(pmi->pMapType), pRef, lvidParSize,
                  (lvidParSize == MAX_UINT32) ? " (#FFFFFFFF)" : "",
                  (pmi->pMapType == PMAPTYPE_VIRTUAL)
                    ? "#FFFFFFFF for a Virtual Partition"
                    : "Equal to Partition Length");
              MLIMITend;
            }
            VERBOSE00(uctout, "\n");
        }
    }

    /* From here verify fields that are either in the
     * LVID ImplementationUse or in a UDF 2.00 VAT
     * or do not exist (1.50 VAT), unless the LVID
     * is not Open.
     * Find pointer to vatRec of first (and only) VAT file, if any.
     */
    pVatRec = NULL;
    for( pRef = 0; pRef < nMaps; pRef++ )
    {   PartitionMapInfo *pmi = &mc->partitionMapInfo[pRef];
        if(   pmi->pMapType == PMAPTYPE_VIRTUAL
           && pmi->vatRec.vatFile != NULL )
        {   pVatRec = &pmi->vatRec;
            break;
        }
    }

    /* consistency check:
     */
    UCTASSERT(   (pVatRec == NULL) == (!isSequential)
              && (pVatRec == NULL) == (mc->vatNode == NULL) );

    if( pVatRec == NULL )       /* LVID */
    {   strcpy(udfRef, "UDF 2.2.6.4");
        descr  = (Byte*) lvid;
        pLastModTime     = &lvid->recordingDateAndTime;
        pLastDeveloperId =  liu->implementationID.Identifier;
    }
    else                            /* VAT 1.50 or VAT 2.00+ */
    {   strcpy(udfRef, UDFREF_VAT(udfRevision));     /* VAT  */
        strcat(udfRef, ", 2.2.6.4");                 /* LVID */
        descr  = NULL;      /* VAT file is no UDF descriptor */
        pLastModTime     = pFE_modificationTime(mc->vatNode->fe);
        pLastDeveloperId = pFE_implementationIdentifier(mc->vatNode->fe)->Identifier;
    }

    /* First LVID/VAT status summary
     */
    VERBOSE00(uctout,
      "  ==>\t%11s status summary:\n"
                            "\t%-26s: ",
        (isSequential) ? "VAT" : "LVID", "Last modification Time");
    printTimestampShort(pLastModTime, TRUE, NULL);  /* local time */
    VERBOSE00(uctout," (%s)\n\t%-26s: ",
        TIMEZONE_TYPE1TXT(GET_TIMEZONE(pLastModTime)),
                                         "Last written Developer Id");
    printBytesName(pLastDeveloperId, ENTITYID_IDSIZE, FALSE, VERBOSE00level);

    VERBOSE00(uctout,     "\n\t%-26s: ", "Next UniqueID");
    if( mc->nextUniqueIdStatus == NEXTUNIQUEID_UNDEFINED )
    {   VERBOSE00(uctout, "undefined\n");
    }
    else
    {   printHexUniqueId17Chars(mc->nextUniqueId, FALSE);
        VERBOSE00(uctout, "%s\n",
            (isSequential)  ? " => incremented VAT FE UniqueID"
          : (isDirtyLvid)   ? " => from dirty LVID"
                            : " => from LVID");
    }
    VERBOSE00(uctout,     "\t%-26s: ", "max used FE  UniqueID");
    printHexUniqueId17Chars(mc->maxFeUniqueID, FALSE);
    VERBOSE00(uctout,     "%s\n",
              (isSequential) ? " => VAT FE excluded" : "");

    if( udfRevision >= 0x200 )      /* FID UDF UniqueID */
    { VERBOSE00(uctout,   "\t%-26s: ",
                "max used FID UniqueID");
      printHexUniqueId17Chars(mc->maxFidUniqueID, TRUE);
      VERBOSE00(uctout, "\n");
    }

    /* Continue LVID/VAT status summary and tests
     * for LVID/VAT 2.00+
     */
    if(   pVatRec != NULL
       && pVatRec->vatIntroRevision == 0x200 )  /* UDF 2.00+ VAT */
    {
        VAT200Head *vat200Head = (VAT200Head *) pVatRec->vatFile;
        numberOfFiles       =  vat200Head->numberOfFiles;
        numberOfDirectories =  vat200Head->numberOfDirectories;
        minUdfRead          =  vat200Head->minUDFReadVersion;
        minUdfWrite         =  vat200Head->minUDFWriteVersion;
        maxUdfWrite         =  vat200Head->maxUDFWriteVersion;
        /* Relation of logicalVolumeIdentifier in LVD, IUVD, FSD
         * and VAT2 checked in checkVolumeIdentifiersSummary().
         */
    }
    else if(   handleAsCloseLvid    /* LVID ImplementationUse, 2.2.6.4 */
            || pVatRec != NULL )    /* UDF 1.50 VAT */
    {   /* NOTE that number of Files/Directiries can and will
         * not be verified for the UDF 1.50 VAT case.
         */
        numberOfFiles       =  liu->numberOfFiles;
        numberOfDirectories =  liu->numberOfDirectories;
        minUdfRead          =  liu->minimumUDFReadRevision;
        minUdfWrite         =  liu->minimumUDFWriteRevision;
        maxUdfWrite         =  liu->maximumUDFWriteRevision;
    }
    else    /* Sequential medium with Open LVID and no VAT */
    {   /* TODO: remove this code ??
         * no error message printing, because tested before,
         * see: "Dirty Volume" error message in udfMountLogicalVolume().
         */
        printMessageHead( VERBOSE00level, (Byte*) lvid,
            (Byte*) &lvid->integrityType,
            "Note: Open LVID but no VAT found, dirty volume.\n");
        return FALSE;
    }

    /* indicate that some values are from LVID in the UDF 1.50 VAT case
     */
    txtFromLvid[0] = '\0';      /* clean */
    if( pVatRec != NULL && pVatRec->vatIntroRevision == 0x150 )
    { sprintf(txtFromLvid, "\t(from %s LVID)", lvidStatusText);
    }

    VERBOSE00(uctout,   "\t%-26s: %8lu%s\n"
                        "\t%-26s: %8lu%s\n"
                        "\t%-26s: UDF ",
        "Number of Files",       numberOfFiles,       txtFromLvid,
        "Number of Directories", numberOfDirectories, txtFromLvid,
        "Min UDF Read   Revision");
    printUdfRevision(VERBOSE00level, minUdfRead, txtFromLvid);

    VERBOSE00(uctout, "\n\t%-26s: UDF ", "Min UDF Write  Revision");
    printUdfRevision(VERBOSE00level, minUdfWrite, txtFromLvid);

    VERBOSE00(uctout, "\n\t%-26s: UDF ", "Max UDF Write  Revision");
    printUdfRevision(VERBOSE00level, maxUdfWrite, txtFromLvid);

    VERBOSE00(uctout, "\n\t%-26s: UDF ", "    Medium UDF Revision");
    printUdfRevision(VERBOSE00level, udfRevision, txtFromLvid);
    VERBOSE00(uctout, "\n\n");

    /* Verify fields that are either in LVID or in VAT
     * UDF 2.2.6.4, 2.2.11 (was 2.2.10): Marked for Delete (MFD) files
     * and directories shall not be included in the count, but:
     * totalNumberOfFilesMFD and totalNumberOfDirsMFD are included
     * in totalNumberOfFiles, totalNumberOfDirs counts respectively.
     * Make local totalNumberOfFiles, totalNumberOfDirs correction.
     */
    UCTASSERT( totalNumberOfFiles >= totalNumberOfFilesMFD );
    UCTASSERT( totalNumberOfDirs  >= totalNumberOfDirsMFD );
    totalNumberOfFiles -= totalNumberOfFilesMFD; /* local MFD correction */
    totalNumberOfDirs  -= totalNumberOfDirsMFD;

    /* Test nmb of Files/Dirs, except for UDF 1.50 VAT
     * TODO: check: (with Open LVID)
     */
    if(    pVatRec != NULL
       &&  pVatRec->vatIntroRevision == 0x150   /* UDF 1.50 VAT */
/**/   && !handleAsCloseLvid )                  /* no Close LVID */
    {   VERBOSE00(uctout,
           "\tNote: Unable to verify Number of Files and Directories,\n"
          "-\t      because not registered in UDF 1.50 VAT.\n"
          "-\t      Interpretation of \"%s\" EA, as defined\n"
          "-\t      in UDF 1.50 Errata is not implemented by the verifier.\n",
                UDFEA_VAT_LVExtension_ID);
    }
    else    /* no UDF 1.50 VAT */
    {  if( numberOfFiles != totalNumberOfFiles )
      { MLIMITbegin( ERROR00level, uctMessageLimit );
          printMessageExpectedU32( MLIMIT.vl, descr,
            (Byte*) &liu->numberOfFiles,        /* for VAT: descr == NULL */
            "Error: Number of Files", "%u",
            numberOfFiles, totalNumberOfFiles, NULL);
          fprintf(uctout, ",\n-\t\t\t");
          if( totalNumberOfFilesMFD != 0 )
          { fprintf(uctout,
                   "%lu 'marked for delete' file%s must\n"
              "-\t\t\tNOT be included in the count, ",
              totalNumberOfFilesMFD, PLURAL_S(totalNumberOfFilesMFD));
          }
          fprintf(uctout, "%s.\n", udfRef);
          PRINT_EXTRA_NOTE;
        MLIMITend;
      }
      if( numberOfDirectories != totalNumberOfDirs )
      { MLIMITbegin( ERROR00level, uctMessageLimit );
          printMessageExpectedU32( MLIMIT.vl, descr,
            (Byte*) &liu->numberOfDirectories,  /* for VAT: descr == NULL */
            "Error: Number of Directories", "%u",
            numberOfDirectories, totalNumberOfDirs, NULL);
          fprintf(uctout, ",\n-\t\t\t");
          if( totalNumberOfDirsMFD != 0 )
          { fprintf(uctout,
                   "%lu 'marked for delete' director%s must\n"
              "-\t\t\tNOT be included in the count, ",
              totalNumberOfDirsMFD, PLURAL_IES(totalNumberOfDirsMFD));
          }
          fprintf(uctout, "%s.\n", udfRef);
          PRINT_EXTRA_NOTE;
        MLIMITend;
      }
    }

    /* Now test Min/Max Revisions from LVID/VAT.
     * NOTE that ON PURPOSE, also these values from the open LVID
     * are tested in the UDF 1.50 VAT case !!!!
     */
    /* First test Minimum UDF Read Revision for UDF 2.60 media
     */
    if(   udfRevision == 0x260
       && minUdfRead > 0x250 )
    { MLIMITbegin( ERROR00level, uctMessageLimit );
        fprintf(uctout,
           "\tError: %s Minimum UDF Read Revision: #%03X.\n"
          "-\t       Shall be at most #250 for all UDF 2.60 media,\n"
          "-\t       UDF section 2 Basic Restrictions ..., %s.\n",
            isSequential ? "VAT" : "LVID", minUdfRead, udfRef);
      MLIMITend;
    }

    /* Test Min/Max revision on used UDF properties and test if
     * udfRevision is outside Min/Max range. At least a note if
     * min/max values are not equal to media UDF revision.
     *  (exception: NO note caused by:
     *   udfRevision == 0x260 && minUdfRead  == 0x250,
     *   see test above).
     */
    if(  (     minUdfRead != udfRevision
          && !(minUdfRead == 0x250 && udfRevision == 0x260))
       ||      minUdfWrite != udfRevision
       ||      maxUdfWrite != udfRevision )
    { char *vatLvidTxt, *extraTxt =
          "-\t\tThe \"-udf <revision>\" option can be used"
                                                " to get an\n"
          "-\t\timpression of medium behaviour for other"
                                            " UDF revisions.\n";
      /* There is an exception for the 1.50 VAT, where the
       * min/max values from the (Open) LVID are tested.
       */
      vatLvidTxt = (   pVatRec != NULL          /* VAT 2.00+ */
                    && pVatRec->vatIntroRevision == 0x200 )
                 ? "VAT" : "LVID";

      if( checkMinMaxReadWriteRevisions(mc,
                     minUdfRead,  minUdfWrite,
                     maxUdfWrite, udfRevision,
                     pVatRec,     FALSE) )  /* NOT yet verbose */
      { /* OK, only Note: */
        MLIMITbegin( INFO01level, uctMessageLimit );
          fprintf( uctout,
              "\t  Note: %s Min/Max Revision unequal to"
                                        " Medium Revision,\n"
              "-\t\tsee above status summary, %s.\n%s",
                 vatLvidTxt, udfRef, extraTxt);
        MLIMITend;
      }
      else      /* error message */
      { MLIMITbegin( ERROR00level, uctMessageLimit );
          fprintf( uctout,
              "\t Error: %s Min/Max UDF Read/Write Revision error.\n",
                 vatLvidTxt);
          (void) checkMinMaxReadWriteRevisions(mc,
                     minUdfRead,  minUdfWrite,
                     maxUdfWrite, udfRevision,
                     pVatRec,     TRUE);        /* verbose now */
          fprintf(uctout,
              "-\t\tSee above status summary, %s.\n%s",
                 udfRef, extraTxt);
        MLIMITend;
      }
    }

    return TRUE;

}   /* end verifyFinalLVIDandVAT() */

/* File Set Descriptor
 * ECMA 4/14.1
 * UDF 2.00 2.3.2
 */
static bool verifyFileSetDescriptor(FileSetDescriptor *fsd,
                                    UdfMountContext   *mc)
{
    Byte    *startOfReserved = NULL;
    size_t   sizeOfReserved;

#ifdef  DEBUG01
    MLIMITbegin(DEBUG01level, MLIMITdefault01);
        implementationNotReady( "File Set Descriptor" );
    MLIMITend;
#endif  /* DEBUG01 */

    /* Timestamp recordingDateAndTime
     */
    verifyTimestamp(&fsd->recordingDateAndTime,"ECMA 3/10.10.2",
                    "FSD Recording Time", (Byte*)fsd, mc, NULL);

    /* Uint16 interchangeLevel             UDF 2.3.2.1+2, ECMA 4/15
     * Uint16 maximumInterchangeLevel
     */
    if(   fsd->interchangeLevel != 3
       || fsd->maximumInterchangeLevel != 3 )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageHead(MLIMIT.vl, (Byte*)  fsd,
                                   (Byte*) &fsd->interchangeLevel, NULL);
        fprintf(uctout,
            "Error: Interchange Level: %lu, Maximum Interchange Level: %lu,\n"
            "-\t\t\texpected: both 3, UDF 2.3.2.1+2, ECMA 4/15.\n",
              fsd->interchangeLevel, fsd->maximumInterchangeLevel);
      MLIMITend;
    }

    /* Uint32 characterSetList
     * Uint32 maximumCharacterSetList
     */
    (void) verifyCharSetList(&fsd->characterSetList,
                             (Byte*)fsd, FALSE, "2.3.2.3");
    (void) verifyCharSetList(&fsd->maximumCharacterSetList,
                             (Byte*)fsd, TRUE, "2.3.2.4");

    /* Uint32 fileSetNumber
     * Uint32 fileSetDescriptorNumber
     *
     * test fileSetNumber in readFileSetDescriptor(),
     * because first detection of possible multiple
     * prevailing FSDs (File Sets) must be done.
     */

    /* Charspec logicalVolumeIdentifierCharacterSet
     */
    (void) verifyCharspecCS0(&fsd->logicalVolumeIdentifierCharacterSet,
                 (Byte*) fsd, "Logical Volume Identifier Character Set",
                            "2.3.2.5");

    /* Dstring logicalVolumeIdentifier[128]
     * Relation of logicalVolumeIdentifier in LVD, IUVD, FSD
     * and VAT2 checked in checkVolumeIdentifiersSummary().
     */
    VERBOSE00(uctout, "\tFSD   Logical Volume Identifier: ");
    PRINTDSTRING(  fsd->logicalVolumeIdentifier,
            sizeof(fsd->logicalVolumeIdentifier), "\n");
    verifyDstring( fsd->logicalVolumeIdentifier, (Byte *)fsd,
            sizeof(fsd->logicalVolumeIdentifier),
                TRUE, "ECMA 3/14.1.10");    /* obligatory */

    /* Charspec fileSetCharacterSet
     */
    (void) verifyCharspecCS0(&fsd->fileSetCharacterSet,
                    (Byte*) fsd, "File Set Character Set", "2.3.2.6");

    /* Dstring fileSetIdentifier[32]
     */
    VERBOSE00(uctout, "\tFSD         File Set Identifier: ");
    PRINTDSTRING(  fsd->fileSetIdentifier,
            sizeof(fsd->fileSetIdentifier), "\n");
    verifyDstring( fsd->fileSetIdentifier, (Byte *)fsd,
            sizeof(fsd->fileSetIdentifier),
                TRUE, "ECMA 3/14.1.12");    /* obligatory */

    /* Dstring copyrightFileIdentifier[32]
     */
    VERBOSE00(uctout, "\tFSD   Copyright File Identifier: ");
    PRINTDSTRING(  fsd->copyrightFileIdentifier,
            sizeof(fsd->copyrightFileIdentifier), "\n");
    verifyDstring( fsd->copyrightFileIdentifier, (Byte *)fsd,
            sizeof(fsd->copyrightFileIdentifier),
                FALSE, "ECMA 3/14.1.13");   /* NOT obligatory */

    /* Dstring abstractFileIdentifier[32]
     */
    VERBOSE00(uctout, "\tFSD    Abstract File Identifier: ");
    PRINTDSTRING(  fsd->abstractFileIdentifier,
            sizeof(fsd->abstractFileIdentifier), "\n");
    verifyDstring( fsd->abstractFileIdentifier, (Byte *)fsd,
            sizeof(fsd->abstractFileIdentifier),
                FALSE, "ECMA 3/14.1.14");   /* NOT obligatory */

    /* LongAd rootDirectoryICB
     */
    VERBOSE00(uctout, "\tFSD           Root Directory at: ");
    if( fsd->rootDirectoryICB.extentLength == 0 )
         VERBOSE00(uctout, "<undefined>\n");
    else VERBOSE00(uctout, "(%lu,p%u)\n",
          fsd->rootDirectoryICB.extentLocation.logicalBlockNumber,
          fsd->rootDirectoryICB.extentLocation.partitionReferenceNumber);
    verifyLongAd( &fsd->rootDirectoryICB, (Byte *) fsd, mc,
                   TRUE,        /* isIntegralBlock   */
                   TRUE,        /* isFixedRecAndAlloc */
                   TRUE );      /* inMetadataPartition */

    /* EntityID domainIdentifier
     */
    verifyEntityID( &fsd->domainIdentifier, ENTITY_SUFFIX_DOMAIN,
                     (Byte*)UDF_DOMAIN_ID, "UDF 2.3.2.7", (Byte*)fsd);

    /* LongAd nextExtent
     */
    verifyLongAd( &fsd->nextExtent, (Byte *) fsd, mc,
                   TRUE,        /* isIntegralBlock   */
                   TRUE,        /* isFixedRecAndAlloc */
                   TRUE );      /* inMetadataPartition */

    /* LongAd systemStreamDirectoryICB      ** UDF 2.00+ **
     * systemStreamDirectoryICB introduced in UDF 2.00,
     * ECMA-167 3rd edition.
     * Execute verification of systemStreamDirectoryICB under
     * same conditions as in endian swap of this field was done
     * in endianSwapFileSetDescriptor(), modifyUdfRevisionRange()
     * called in endianSwapFileSetDescriptor() already.
     * Check Descriptor Version if ECMA 167 2nd or 3rd edition.
     * Check systemStreamDirectoryICB if any and determine start
     * of Reserved field
     */
    if(      fsd->descriptorTag.descriptorVersion == 2 ) /* ECMA 2nd ed */
    {   startOfReserved = fsd->new200.reserved01;
    }
    else if( fsd->descriptorTag.descriptorVersion == 3 ) /* ECMA 3rd ed */
    {   startOfReserved = fsd->reserved02;
        VERBOSE00(uctout, "\tFSD  System Stream Directory at: ");
        if( fsd->new200.systemStreamDirectoryICB.extentLength == 0 )
             VERBOSE00(uctout, "<undefined>\n");
        else VERBOSE00(uctout, "(%lu,p%u)\n",
              fsd->new200.systemStreamDirectoryICB.extentLocation.logicalBlockNumber,
              fsd->new200.systemStreamDirectoryICB.extentLocation.partitionReferenceNumber);
        verifyLongAd(&fsd->new200.systemStreamDirectoryICB,
                      (Byte *) fsd, mc,
                      TRUE,         /* isIntegralBlock   */
                      TRUE,         /* isFixedRecAndAlloc */
                      TRUE );       /* inMetadataPartition */
    }

    /* Byte reserved01/02[32 or 48]
     */
    sizeOfReserved = sizeof(FileSetDescriptor) - (startOfReserved - (Byte*)fsd);
    if( !verifyZeros(startOfReserved, sizeOfReserved, NULL, NULL, NULL) )
    {   /* Note that verifyZerosInDescriptor() assumes to be
         * called from within a MLIMITbegin(ERROR00level, ...)
         * ... MLIMTend clause.
         */
        MLIMITbegin(ERROR00level,uctMessageLimit);
          (void) verifyZerosInDescriptor((Byte*) fsd, startOfReserved,
                                         sizeOfReserved, NULL);
          fprintf(uctout,
            "-\t\t Reserved field error,"
                    " ECMA-3 4/14.1.19 or ECMA-2 4/14.1.18\n");
        MLIMITend;
        /** no FALSE result **/
    }
    return TRUE;

}   /* end verifyFileSetDescriptor() */

/* File Identifier Descriptor
 * ECMA 4/14.4
 * UDF 2.00 2.3.4
 */
static bool verifyFileIdentifierDescriptor(FileIdentifierDescriptor *fid,
                                           UdfMountContext *mc, Node *node)
{
    Uint8 fileCharacteristics = fid->fileCharacteristics;
    int   result = TRUE;        /* flags 'fatal errors' only */

    if( node == NULL || mc == NULL )
    {   VERBOSE00(uctout,
            "  ==>\tNote: Unable to complete FID verification, insufficient context.\n");
        return FALSE;
    }

    /* Uint16 fileVersionNumber:
     */
    if( fid->fileVersionNumber != 1 )
    {   /* no FALSE result */
        MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageExpectedU32( MLIMIT.vl, (Byte*)fid,
                (Byte*) &fid->fileVersionNumber,
                "Error: File Version Number", "%u",
                fid->fileVersionNumber, 1, ", UDF 2.3.4.1\n" );
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }

    /* Uint8 fileCharacteristics:
     * Test reserved bits here.
     * Other bits are combined with later tests.
     */
    if( (fileCharacteristics & FC_RESERVED_MASK) != 0 )
    {   /* no FALSE result */
        MLIMITbegin( ERROR00level, uctMessageLimit );
          printMessageHead( MLIMIT.vl, (Byte*)fid,
                (Byte*) &fid->fileCharacteristics, NULL);
          fprintf(uctout,
                "Error: File Characteristics: #%02X,"
                                " reserved bit set,\n"
                "-\t\t\tECMA 4/14.4.3.\n",
                fileCharacteristics);
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }

#ifdef UCT_TESTING
/* Test hard link and UniqueID errors for both
 * IBM/OS400_01.img and IBM/OS400_04.img (UDF 2.01).
 * Make hard link, set equal UniqueID to lower value
 * that is already in use elsewhere.
 */
#define XX_PSTART    257        /* partition p0 start */
#define XX_AD1      (15926 - XX_PSTART)
#define XX_AD2      (15927 - XX_PSTART)
#define XX_UNIQUEID  0x1A
if(   fid->ICB.extentLocation.logicalBlockNumber == XX_AD1
   || fid->ICB.extentLocation.logicalBlockNumber == XX_AD2 )
{     fid->ICB.extentLocation.logicalBlockNumber =  XX_AD1;
      printHexUniqueId17Chars( (Uint64)
          fid->ICB.implementationUse.ImpUse.UDFUniqueID, TRUE);
      fprintf(uctout, " -> ");
      fid->ICB.implementationUse.ImpUse.UDFUniqueID = XX_UNIQUEID;
      printHexUniqueId17Chars( (Uint64)
          fid->ICB.implementationUse.ImpUse.UDFUniqueID, TRUE);
      fprintf(uctout, "\n");
}
#endif  /** UCT_TESTING **/

    /* LongAd ICB: FALSE result ??
     * All ICBs must be in the Metadata Partition, if any.
     */
    if( !verifyLongAd(&fid->ICB, (Byte *) fid, mc,
                       TRUE,    /* isIntegralBlock   */
                       TRUE,    /* isFixedRecAndAlloc */
                       TRUE) )  /* inMetadataPartition */
    {   return FALSE;
    }

    /* test combinations of ICB existence and deleted bit
     */
    if( adGetExtentSize(&fid->ICB) == 0 )   /* ICB not specified */
    {
        if( isBitOff(fileCharacteristics, FCB_DELETED) )
        {
            MLIMITbegin(ERROR00level, uctMessageLimit);
              printMessageHead( MLIMIT.vl, (Byte*)fid,
                            (Byte*)&fid->ICB, NULL);
              fprintf(uctout,
                "Error: long_ad ICB extent size zero"
                            " but deleted bit not set,\n"
                "-\t\t\tECMA 4/14.4.3+5, %s.\n",
                    UDFREF_FIDICBCH(getUctUdfRevision()));
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
        }
        else if( !verifyZeros((Byte *) &fid->ICB, sizeof(fid->ICB),
                               NULL,NULL,NULL) )
        {
            MLIMITbegin(ERROR00level, uctMessageLimit);
              printMessageHead( MLIMIT.vl, (Byte*)fid,
                            (Byte*)&fid->ICB, NULL);
              fprintf(uctout,
                "Error: long_ad ICB extent size zero"
                                " and deleted bit set.\n"
                "-\t\t\tHowever not all %lu long_ad bytes are"
                                    " zero as required\n"
                "-\t\t\tby ECMA, mind implementation use bytes%s,\n"
                "-\t\t\tECMA 4/14.4.3+5, %s.\n",
                sizeof(fid->ICB), (getUctUdfRevision() >= 0x200)
                                    ? " (UDF UniqueID)" : "",
                UDFREF_FIDICBCH(getUctUdfRevision()) );
              nodePrintUnicodeNameContLine(node,mc);
              printLongAd( &fid->ICB, TRUE );   /* isFidIcb */
            MLIMITend;
        }
    }

    /* Uint16 lengthOfImplementationUse
     */
    if(   (   fid->lengthOfImplementationUse > 0
           && fid->lengthOfImplementationUse < sizeof(EntityID))
       || (fid->lengthOfImplementationUse % 4) != 0 )
    {
        /* no FALSE result */
        MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead( MLIMIT.vl, (Byte*)fid,
                              (Byte*) &fid->lengthOfImplementationUse, NULL);
            fprintf(uctout,
              "Error: lengthOfImplementationUse: %lu, "
                                    "expected: if not 0, then at\n"
              "-\t\t\tleast 32 and a multiple of 4, %s, ECMA 4/14.4.6.\n",
                    fid->lengthOfImplementationUse,
                    UDFREF_FIDLIUIU(getUctUdfRevision()));
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }

    if( fid->lengthOfImplementationUse >= sizeof(EntityID) )
    {
        (void) verifyEntityID((EntityID *) &fid->startOfImplementationUse,
                              ENTITY_SUFFIX_IMPL, NULL, NULL, (Byte*)fid);
    }

    /* Verify Uint8  lengthOfFileIdentifier
     *    and Dchars fileIdentifier[lengthOfFileIdentifier]:
     */
    if(    isBitOn(fileCharacteristics, FCB_PARENT)
        && fid->lengthOfFileIdentifier != 0 )
    {
        MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageExpectedU32( MLIMIT.vl, (Byte*)fid,
                (Byte*) &fid->lengthOfFileIdentifier,
                "Error: Length Of File Identifier", "%u",
                fid->lengthOfFileIdentifier, 0,
                ",\n-\t\t\tbecause parent bit set, ECMA 4/14.4.4\n" );
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }
    else if(   isBitOff(fileCharacteristics, FCB_PARENT)
            && fid->lengthOfFileIdentifier == 0 )
    {   MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead( MLIMIT.vl, (Byte*)fid,
                (Byte*) &fid->lengthOfFileIdentifier,
                "Error: Zero length Of File Identifier, only allowed\n"
                "-\t\t\tfor parent FID, ECMA 4/8.6, 4/14.4.4.\n" );
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }

    /* verify Dchars, except for genuine parent FID,
     * empty Dchars allowed for parent FID only.
     */
    if(   fid->lengthOfFileIdentifier != 0
       || isBitOff(fileCharacteristics, FCB_PARENT) )
    {
        if( !verifyDchars(
                  (Dchars*) &fid->startOfImplementationUse +
                             fid->lengthOfImplementationUse,
                     (Byte*) fid,
                             fid->lengthOfFileIdentifier,
                     isBitOn(fileCharacteristics, FCB_DELETED),
                            "ECMA 4/14.4.8" ) )
        {   globalFidIdentifierError = TRUE;
        }
#undef  GLOBALFIDIDENTIFIERERROR_TESTING    /** testing **/
#ifdef  GLOBALFIDIDENTIFIERERROR_TESTING    /** testing **/
        { static cnt = 0;
          if( cnt < 10 && (cnt++ % 2) == 0 )
          { globalFidIdentifierError = TRUE;
            if(   (cnt % 4) == 1
               && node->unicodeNameLen >= 1 )
            { node->unicodeName[0] = '@';
              node->unicodeNameLen = 1;
            }
            else
            { checkFree((void**)&node->unicodeName);
              node->unicodeNameLen = 0;
            }
          }
        }
#endif  /** GLOBALFIDIDENTIFIERERROR_TESTING **/
    }

    /* test UDF defined stream names with UDF 3.3.5.2, 3.3.7, 3.3.8
     */
    verifyUdfDefinedStreams(node, mc);

    /* Test FID UDF UniqueID, introduced in UDF 2.00.
     * No test for deleted FID with unspecified ICB field,
     * these FIDs are not in use and ready for reuse.
     */
    if(   isBitOff(fileCharacteristics, FCB_DELETED)
       || adGetExtentSize(&fid->ICB) != 0 ) /* ICB specified */
    {   (void) verifyFidOrFeUniqueID((Byte*)fid, node, mc);
    }

    /* Check Padding field.
     */
    { Byte *pPadding = &fid->startOfImplementationUse +
                        fid->lengthOfImplementationUse +
                        fid->lengthOfFileIdentifier;
      size_t paddingSize = (4 - ((pPadding - (Byte*)fid) % 4)) % 4;

      if( !verifyZeros(pPadding, paddingSize, NULL, NULL, NULL) )
      { /* Note that verifyZerosInDescriptor() assumes to be
         * called from within a MLIMITbegin(ERROR00level, ...)
         * ... MLIMTend clause.
         */
        MLIMITbegin(ERROR00level, MLIMITdefault10);
          (void) verifyZerosInDescriptor((Byte*) fid, pPadding,
                                         paddingSize, NULL);
          fprintf(uctout,
            "-\t\t FID Padding field must hold #00 bytes, ECMA 4/14.4.9\n");
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /** no FALSE result **/
      }
    }

    return result;

}   /* end verifyFileIdentifierDescriptor() */


/* verifyUseAdOrder() and verifyUseAdOrderInit():
 * Unallocated Space Entry ECMA 4/14.11, UDF 2.3.7
 * Verify AD location order and contiguous extents,
 * see UDF 2.3.7.1
 * Note that overlap is already tested using markUnallocatedPartitionSpace()
 * which does also test overlap between Unallocated and Freed Space.
 * Test location order and contiguous extents here.
 */
static bool   useAdIsFirstAd = TRUE;
static Uint32 useAdMaxBlocksPerExtent;

static Uint32 useAdPrevBlockNr;         /* previous AD location */
static Uint32 useAdPrevNrOfBlocks;      /* previous AD size */
static Uint32 useAdTotalBlockCount;     /* accumulate */

extern void verifyUseAdOrderInit(Uint32 blockSize)
{   useAdIsFirstAd = TRUE;
    useAdTotalBlockCount = 0;
    useAdMaxBlocksPerExtent =
        ROUNDUPELEMENTS(TWO_POWER30 - blockSize, blockSize);
}

extern Uint32 getUseAdOrderTotalBlocks()
{   return useAdTotalBlockCount;
}

static bool verifyUseAdOrder(Uint32 blockNr, Uint32 nrOfBlocks,
                             Byte *ad, Byte *d)
{
    useAdTotalBlockCount += nrOfBlocks;
    if( useAdIsFirstAd )
    {   useAdIsFirstAd = FALSE;
    }
    else
    {   if( blockNr < useAdPrevBlockNr )    /* not ascending order */
        {   MLIMITbegin(ERROR00level, uctMessageLimit);
              printMessageHead(MLIMIT.vl, d, ad, NULL);
              fprintf(uctout,
                "Error: Unallocated Space Entry ADs not in ascending location\n"
                "-\t\t\torder, logical block numbers: %lu, %lu, UDF 2.3.7.1\n",
                            useAdPrevBlockNr, blockNr);
            MLIMITend;
        }
        useAdPrevBlockNr += useAdPrevNrOfBlocks;

        if(    blockNr == useAdPrevBlockNr      /* contiguous extents */
            &&          nrOfBlocks != useAdMaxBlocksPerExtent
            && useAdPrevNrOfBlocks != useAdMaxBlocksPerExtent )
        {   MLIMITbegin(ERROR00level, uctMessageLimit);
              printMessageHead(MLIMIT.vl, d, ad, NULL);
              fprintf(uctout,
                "Error: Unallocated Space Entry ADs illegal contiguous\n"
                "-\t\t\textents at logical block number %lu, UDF 2.3.7.1\n",
                            blockNr);
            MLIMITend;
        }
    }
    useAdPrevBlockNr    = blockNr;
    useAdPrevNrOfBlocks = nrOfBlocks;
    return TRUE;
}


/* swapAndVerifyAllocationDescriptors:
 * Swap and verify adType, *pLenOfAds and Ads itself.
 * If node == NULL, the start of the Allocation Descriptors was for
 * an Unallocated Space Entry, else for a FE ir EFE file node->fe.
 * The descriptor d can however also be a AED, in case of a continuation
 * Extent of ADs in one of the above cases.
 * So if node == NULL, some of the tests below will not be performed.
 * Only ADT_SHORT or ADT_LONG allowed, all other types
 * (even ADT_INFE) will cause a FALSE result.
 *
 * Note that ads and pLenOfAds must be pointers to the actual
 * Allocation Descriptors and lengthOfAllocationDescriptors
 * fields in descriptor d (FE, EFE, USE or AED).
 *
 * Total number of effective ADs, total AD extents length
 * and total AD file body length is maintained in
 * node->feTotalExtentsLength, node->feFileBodyLength
 * and *pNumberOfADs.
 * These fields must initialized before caling this function
 * for the first AD of a list of ADs.
 *
 * Note:
 *  if (node != NULL) then pCountADs == &node->feNumberOfADs
 *           and shortAdPartitionRef ==  node->fePartRef
 */
extern bool swapAndVerifyAllocationDescriptors(
                Byte   *ads, Uint32 *pLenOfAds, Byte *d,
                Uint8   adType,
                Uint32 *pCountADs,           /* &node->feNumberOfADs */
                Uint64 *pTotalRecordedBlocks,
                Uint16  shortAdPartitionRef, /*  node->fePartRef */
                UdfMountContext *mc, Node *node,
                bool    inMetadataPartition)
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32   blockSize = vmi->blockSize;
    Uint8    extentType = 0;
    Uint32   extentSize, extentNrBlocks;
    Uint32   adSize, lenOfAds, adsNr;
    size_t   maxBytes, maxDescr, adsDescr;
    char    *adName;
    Uint16   partRefNumber;
    Uint32   logicalBlockNr, blocksMarkedAlready;
    Uint64   feInformationLength = 0;
    bool     isTail = FALSE,
             isLastOfFileBody = FALSE,
             result = TRUE,
             isUnallocatedSpaceEntryADs = (node == NULL);

    if(      adType == ADT_SHORT )
    {   adName = "short_ad";
        adSize = sizeof(ShortAd);
    }
    else if( adType == ADT_LONG )
    {   adName = "long_ad";
        adSize = sizeof(LongAd);
    }
    else /* this cannot be, even ADT_INFE is not allowed here */
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageHead( MLIMIT.vl, d, NULL, NULL);
          fprintf(uctout,
            "Error: Allocation Descriptor type: %u, expected: %u or %u\n"
            "-\t\t for short_ad or long_ad\n", adType, ADT_SHORT, ADT_LONG);
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        return FALSE;   /* fatal */
    }

    /* Only ADT_SHORT or ADT_LONG allocation
     * descriptors from here, now first swap and verify ADs
     * Only complete descriptors within length and within block
     */
    if( !endianSwapAllocationDescriptors( ads, pLenOfAds, adType, d) )
    {   /* No error messages printed here. Tests will be repeated
         * and error messages printed when verifying ADs.
         */
#ifdef  DEBUG01
        ifPRINTdebug01(uctout,
            "DEBUG01: swapAndVerifyAllocationDescriptors:"
            " endianSwapAllocationDescriptors failed\n");
        ENDif;
#endif  /* DEBUG01 */
    }
    lenOfAds = *pLenOfAds;
    maxBytes = d + vmi->blockSize - ads;
    maxDescr = maxBytes / adSize;
    adsDescr = lenOfAds / adSize;

    /* ECMA 4/12. :
     *  [Extent of Allocation Descriptors]{
     *      <Allocation Extent Descriptor>
     *      <allocation descriptor> 1+
     *  }
     * TODO: must this be a multiple of adSize ??
     *       maybe last AD has extentSize 0 or poiter to
     *       next extent of all descr, in that case remainder content
     *       and remainder size maybe don't care ??
     *       Fore the moment, it is an error, see osta_tc reflector:
     *       Subject: verifier question: Length Of Allocation Descriptors
     *          Date: Thu, 27 Jan 2000 15:13:03 +0100
     *       and followup.
     */
    if(   (lenOfAds % adSize) != 0  /* osta_tc reflector: 27 Jan 2000 */
       || adsDescr == 0 || lenOfAds > maxBytes )    /* out of range */
    {
        /* no FALSE return result */
        MLIMITbegin(ERROR00level, uctMessageLimit);
            printMessageHead( MLIMIT.vl, d, (Byte*)pLenOfAds, NULL);
            fprintf(uctout,
                "Error: Length Of Allocation Descriptors: %lu,\n"
                "-\t\t\texpected for %s: a multiple of %lu (AD size),\n"
                "-\t\t\tat least %u, at most %lu.\n"
                "-\t\t\tnmb of descriptors: at least 1, at most %lu,\n"
                "-\t\t\tECMA 4/12. Figure 7, 1+\n",
                lenOfAds, adName, adSize,
                adSize, maxDescr * adSize, maxDescr);
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }

    if( !isUnallocatedSpaceEntryADs )
    {   feInformationLength = (*pFE_informationLength(node->fe));
        isTail = (    node->feTotalExtentsLength >= feInformationLength
                  || (node->feFileBodyLength % blockSize) != 0 );
        isLastOfFileBody = FALSE;
    }

    /* Only ADT_SHORT or ADT_LONG allocation
     * descriptors from here, now verify them
     * Only complete descriptors within lenOfAds and within block
     */
    adsDescr = MIN(adsDescr, maxDescr); /* nmb of ADs to be processed */
    for( adsNr = 1;                     /* local count, start with 1 */
         adsNr <= adsDescr;             /* verify only integral ADs */
         adsNr++, ads += adSize )
    {
        AnyAllocationDescriptor *ad = (AnyAllocationDescriptor*) ads;

        extentType     = adGetExtentType(&ad->anyAd);
        extentSize     = adGetExtentSize(&ad->anyAd);
        extentNrBlocks = ROUNDUPELEMENTS(extentSize, blockSize);

        if( extentType != ADEL_EXTENTPOINTER )
        {   (*pCountADs)++;     /* for printAdInfo() */
        }

        if( extentSize == 0 || extentType == ADEL_EXTENTPOINTER )
        {   /* end of ADs in this descriptor, break out of loop
             * but first do normal verifyAD() tests and printing.
             * Exception for inMetadataPartition:
             *  If AED pointer and no special metadata file,
             *  then inMetadataPartition is ignored and the AD
             *       partition reference number must point to
             *       the metadata partition !!
             */

            (void) verifyAD(ad, adType, shortAdPartitionRef,
                            d, mc, TRUE, /* isIntegralBlock, */
                            FALSE,       /* NOT isFixedRecAndAlloc */
                    (     extentType == ADEL_EXTENTPOINTER
                     &&   node != NULL
                     && !(NODEFLAGS_IS_SET(node, NFB_METADATAFILE))
                     && !(NODEFLAGS_IS_SET(node, NFB_METADATAMIRROR))
                     && !(NODEFLAGS_IS_SET(node, NFB_METADATABITMAP)) )
                        ? TRUE      /* in metadata partition (if any) */
                        : inMetadataPartition );
            printAdInfo(ad, adType, *pCountADs, shortAdPartitionRef, node,
                        lenOfAds - (adsNr * adSize),
                        (adsNr == 1), INFO01level);
            break;  /* last effective AD here */
        }

        /* count recorded blocks
         */
        if( extentType == ADEL_RECORDED_AND_ALLOCATED )
        {   *pTotalRecordedBlocks += extentNrBlocks;
        }

        /* mark allocated or unallocated blocks in bitmap
         */
        if( !udfGetLocation(ad, adType, shortAdPartitionRef,
                            &partRefNumber, &logicalBlockNr) )
        {   return FALSE;   /* TODO: check */
        }

        if( isUnallocatedSpaceEntryADs )
        {
#ifdef UCT_TESTING      /* for mdi_Table01.img test image     tests: */
/**testing**/if(logicalBlockNr == 46817) logicalBlockNr += 10; /* overlap */
/**testing**/if(logicalBlockNr == 9048 ) logicalBlockNr += 1; /* contiguous */
/**testing**/if(     logicalBlockNr == 9781 ) logicalBlockNr=9783; /* swap */
/**testing**/else if(logicalBlockNr == 9783 ) logicalBlockNr=9781; /* swap */
#endif  /** UCT_TESTING **/

            verifyUseAdOrder(logicalBlockNr, extentNrBlocks, (Byte*)ad, d);
            if(   markUnallocatedPartitionSpace(mc,
                             partRefNumber, logicalBlockNr, extentNrBlocks,
                            &blocksMarkedAlready,
                             FALSE)                 /* no error */
               && blocksMarkedAlready != 0 )        /* blocks were marked already */
            {   MLIMITbegin(ERROR00level, MLIMITdefault10);
                  printMessageHead( MLIMIT.vl, d, ads, NULL);
                  fprintf(uctout,
                    "Error: Multiple definition of Unallocated/Freed blocks in\n"
                     "-\t\t Space Table. %lu blocks of extent starting at"
                                                        " logical block %lu\n"
                     "-\t\t %s previously defined already."
                                              " UDF 2.3.3, 2.3.7.1, ECMA 4/10.1\n",
                        blocksMarkedAlready, logicalBlockNr,
                        PLURAL_WERE(blocksMarkedAlready)    );
                MLIMITend;
            }
        }
        else    /* !isUnallocatedSpaceEntryADs */
        {   /* maybe ignore previous unexpected end of file body if at least
             * 2 other conditions that should be TRUE for a file tail are
             * FALSE.
             */
            if(    node->feFileBodyLength != node->feTotalExtentsLength
                && node->feTotalExtentsLength < feInformationLength
                && extentType != ADEL_NOT_RECORDED_BUT_ALLOCATED )
            { MLIMITbegin(WARN01level, uctMessageLimit);
                printMessageHead( MLIMIT.vl, d, ads, adName);
                fprintf(uctout,
                  " warning: Ignore previous unexpected end of file body,\n"
                      "-\t\t because FE Information Length not"
                                                " exhausted and extent type\n"
                      "-\t\t illegal for file tail\n" );
                      nodePrintUnicodeNameContLine(node,mc);
              MLIMITend;
              node->feFileBodyLength = node->feTotalExtentsLength;  /* resync */
              isTail = FALSE;
            }
            node->feTotalExtentsLength += ROUNDUPMULT(extentSize, blockSize);

            if( !isTail )
            {
                /* Still in file body. Check if extentSize is allowed
                 * to be no integral of block size. Only allowed in last
                 * AD of the file body, that is the last AD contributing
                 * to the FE informationLength.
                 */
                isLastOfFileBody =
                    (node->feTotalExtentsLength >= feInformationLength);
            }
         }

        /* now first do normal verifyAD() tests
         * TODO: verifyAD() FALSE return result ??
         */
        (void) verifyAD( ad, adType, shortAdPartitionRef,
                         d, mc,         /* maybe isIntegralBlock */
                (isUnallocatedSpaceEntryADs || !isLastOfFileBody),
                         FALSE,         /* NOT isFixedRecAndAlloc */
                         inMetadataPartition );

        if( isUnallocatedSpaceEntryADs )
        {
            if( extentType != ADEL_NOT_RECORDED_BUT_ALLOCATED )
            {   MLIMITbegin(ERROR00level, uctMessageLimit);
                  printMessageHead( MLIMIT.vl, d, ads, adName);
                  fprintf(uctout,
                    " error: Extent Type: %u, expected: %u,\n"
                    "-\t\t (NOT_RECORDED_BUT_ALLOCATED), for Unallocated\n"
                    "-\t\t Space Entry AD, UDF 2.3.7.1, ECMA 4/14.14.1.1\n",
                        extentType, ADEL_NOT_RECORDED_BUT_ALLOCATED);
                MLIMITend;
            }
        }
        else if( isTail )       /* implies !isLastOfFileBody */
        {
            if( extentType != ADEL_NOT_RECORDED_BUT_ALLOCATED )
            {
                MLIMITbegin(ERROR00level, uctMessageLimit);
                  printMessageHead( MLIMIT.vl, d, ads, adName);
                  fprintf(uctout,
                    " file tail error: Extent Type: %u, expected: %u,\n"
                    "-\t\t (NOT_RECORDED_BUT_ALLOCATED),"
                                        " UDF 2.01 2.3.6.4, ECMA 4/12.1\n",
                        extentType, ADEL_NOT_RECORDED_BUT_ALLOCATED);
                  nodePrintUnicodeNameContLine(node,mc);
                MLIMITend;
                /* TODO: FALSE return result ?? */
            }
        }
        else    /* !isUnallocatedSpaceEntryADs && !isTail, still in file body */
        {
            if( isLastOfFileBody )  /* last AD of file body, close body */
            {   Uint64 extentSizeExp;
                isLastOfFileBody = FALSE;
                isTail = TRUE;
                extentSizeExp = (Uint64)         /* expected extentSize */
                    (feInformationLength - node->feFileBodyLength);
                if( ((Uint64) extentSize) != extentSizeExp )
                {
                    /* no FALSE return result */
                    MLIMITbegin(ERROR00level, uctMessageLimit);
                      printMessageHead( MLIMIT.vl, d, ads, adName);
                      fprintf(uctout,
                          " file body error: Extent Length: %lu, expected: ",
                                            extentSize);
                      printUint64(MLIMIT.vl, extentSizeExp, FALSE, NULL);
                      fprintf(uctout,
                          ",\n-\t\t for last extent of file body."
                                    " FE Information Length: ");
                      printUint64(MLIMIT.vl, feInformationLength, FALSE, NULL);
                      fprintf(uctout,
                           "\n-\t\t ECMA 4/14.9.10, 4/12.1,"
                                        " UDF 2.00+ 2.3.6.4\n");
                      nodePrintUnicodeNameContLine(node,mc);
                    MLIMITend;
                }
            }
            else if( (extentSize % blockSize) != 0 ) /* !isLastOfFileBody */
            {   /* This was reported by verifyAD() already,
                 * ignore as end of file body.
                 */
                MLIMITbegin(WARN01level, uctMessageLimit);
                  printMessageHead( MLIMIT.vl, d, ads, adName);
                  fprintf(uctout,
                      " warning: Unexpected end of file body\n");
                  nodePrintUnicodeNameContLine(node,mc);
                MLIMITend;
            }
            node->feFileBodyLength += extentSize; /* maybe not full last block */
        }

        /* AD informational message
         * unused bytes for last AD only
         */
        printAdInfo(ad, adType, *pCountADs, shortAdPartitionRef, node,
                    lenOfAds - (adsNr * adSize),
                    (adsNr == 1), INFO01level);
    }   /* endfor */

    /* verify (E)FE logicalBlocksRecorded after final AD
     * skip test if no AD processed
     */
    if(   !isUnallocatedSpaceEntryADs
       &&  adsDescr != 0             /* AD processed, extentType valid */
       &&  extentType != ADEL_EXTENTPOINTER )   /* no more ADs to come */
    { Uint64 feLogicalBlocksRecorded = (*pFE_logicalBlocksRecorded(node->fe));
      if( feLogicalBlocksRecorded != (*pTotalRecordedBlocks) )
      { MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead( MLIMIT.vl, node->fe,
                (Byte*) pFE_logicalBlocksRecorded(node->fe),
                "Error: Logical Blocks Recorded: ");
            printUint64(MLIMIT.vl, feLogicalBlocksRecorded, FALSE, NULL);
            fprintf(uctout, ", expected: ");
            printUint64(MLIMIT.vl, *pTotalRecordedBlocks, FALSE, NULL);
            fprintf(uctout, ", \n"
                "-\t\t\ticbtag Allocation Descriptor type: %u,\n"
                "-\t\t\tUDF 2.3.6.5, ECMA 4/14.9.11, 4/14.6.8.\n",
                adType);
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /** error, but no FALSE result **/
      }
    }
    return result;

}   /* end swapAndVerifyAllocationDescriptors() */

/* Allocation Extent Descriptor
 * ECMA 4/14.5
 * UDF 2.00 2.3.11
 *
 * ECMA 4/12. :
 * [Extent of Allocation Descriptors]{
 *      <Allocation Extent Descriptor>
 *      <allocation descriptor> 1+
 *  }
 */
static bool verifyAllocationExtentDescriptor(AllocationExtentDescriptor *aed,
                                             UdfMountContext *mc, Node *node)
{
    /* node maybe NULL for Unallocated Space Entry
     */
    if( aed->previousAllocationExtentLocation != 0 )
    {   MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead( MLIMIT.vl, (Byte*)aed,
                        (Byte*) &aed->previousAllocationExtentLocation, NULL);
            fprintf(uctout,
                "Error: Previous Allocation Extent Location: %lu, expected: 0\n"
                "-\t\t\tUDF 2.3.11\n",      /* 2.3.11.1 changed in UDF 2.01 */
                    aed->previousAllocationExtentLocation);
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /* no FALSE return result */
    }

    /* swapAndVerifyAllocationDescriptors() is called
     * in extractAllocationDescriptors()
     */
    return TRUE;
}

static bool verifyIndirectEntry(IndirectEntry *ie,
                                UdfMountContext *mc, Node *node)
{
    bool result = verifyICBTag(&ie->icbTag, (Byte*)ie,
                                FALSE,      /* isStream */
                                mc, node);
    if( !verifyLongAd(&ie->indirectICB, (Byte *) ie, mc,
                       TRUE,        /* isIntegralBlock   */
                       TRUE,        /* isFixedRecAndAlloc */
                       FALSE) )     /* NOT inMetadataPartition */
/** TODO: strategy type always 4096
/** TODO: extent size must always be 4096 (2 blocks)
/** TODO: (also for TE ??
/**/
    {   result = FALSE;
    }
    return result;
}

static bool verifyTerminalEntry(TerminalEntry *te,
                                UdfMountContext *mc, Node *node)
{
    return verifyICBTag(&te->icbTag, (Byte*)te,
                         FALSE,         /* isStream */
                         mc, node);
}


/* Extended Attributes verification:
 * Implementation note:
 * A FALSE return value denotes a fatal error. Extended
 * Attribute verification may in that case be aborted,
 * but this does not necessarily mean that this is fatal
 * on a higher level, e.g. for the associated File Entry.
 */
static char *udfEARef = "UDF 3.3.4, ECMA 4/9.1, 4/14.10";

static void printEAabortAndUDFref( Uint8 mLevel )
{
    ifVERBOSE( mLevel)
    {   fprintf(uctout,
          "-\tAbort EA Space verification, %s\n", udfEARef);
    }
    ENDif;
}


/* It is assumed that enough bytes are read for the EAGenericHead.
 */
static bool verifyEAGenericHead(ExtendedAttribute *ea,
                                Uint8 *pEAGroup,
                                Byte  *EASpace,
                                UdfMountContext *mc,
                                Node *node)
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32            blockSize = vmi->blockSize;
    EntityID         *ent;
    EAGenericHead    *ga      = &ea->genericHead;
    Uint32            attrType = ga->attributeType;

    /* Start with informational output showing the byte
     * position in the EA space, the EA Type and in some
     * cases an extra EntityID Identifier.
     * Mind in the EATYPE_DEVSPEC case, that the
     * implementationUseLength is not tested first,
     * but this shall be at least 32, UDF 3.3.4.4
     */
    ifVERBOSE(INFO02level)
    {   printMessageHead(VERBOSE00level, EASpace,
                             (Byte*) ea, NULL);
        fprintf(uctout, "EA: %s", EATEXT(attrType));
        if(      attrType == EATYPE_IMPLUSE )
             ent = &ea->implUseEA.implementationIdentifier;
        else if( attrType == EATYPE_APPLUSE )
             ent = &ea->applUseEA.applicationIdentifier;
        else if( attrType == EATYPE_DEVSPEC )
             ent = &ea->devSpecEA.implementationUse.impUseID;
        else ent = NULL;

        if( ent != NULL )
        {   fprintf(uctout, " - ");
            printBytesName((Byte *) (ent->Identifier),
                       ENTITYID_IDSIZE, FALSE, VERBOSE00level);
        }
        fprintf(uctout,"\n");
    }
    ENDif;

    /* Uint32 attributeType,
     * determine EA group, UDF 3.3.4, ECMA 4/9.1
     */
    switch( attrType )
    {
    case EATYPE_CHARSETINFO:
    case EATYPE_FILETIMES:
    case EATYPE_INFOTIMES:
    case EATYPE_DEVSPEC:
        *pEAGroup = EAGROUP_ECMA;   /* completely defined by ECMA */
        break;
    case EATYPE_IMPLUSE:            /* implementation use EA */
        if( ga->attributeLength < blockSize )   /* UDF 3.3.4 */
             *pEAGroup = EAGROUP_IMPL_4ALIGN;
        else *pEAGroup = EAGROUP_IMPL_BLOCKALIGN;
        break;
    case EATYPE_APPLUSE:
        *pEAGroup = EAGROUP_APPL;   /* application use EA */
        break;
    case EATYPE_ALTPERMIS:          /* not in UDF, UDF 3.3.4.2 */
    default:                        /* reserved, illegal, etc. */
        *pEAGroup = EAGROUP_ERROR;
        MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageHead(MLIMIT.vl, EASpace,
                             (Byte*) &ga->attributeType, NULL);
            fprintf(uctout,
                "Error: EA Attribute Type: %lu, illegal in UDF.\n"
                "-\t\t\t%s\n", attrType,
                (attrType != EATYPE_ALTPERMIS)
                        ? udfEARef : "UDF 3.3.4.2, ECMA 4/14.10.4");
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        return FALSE;       /* fatal on attributeType */
        /** break; **/
    }

    /* Uint8 attributeSubtype
     */
    if( ga->attributeSubtype != 1 )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageHead(MLIMIT.vl, EASpace,
                             (Byte*) &ga->attributeSubtype, NULL);
            fprintf(uctout,
                "Error: EA Attribute Subtype: %lu, shall always be 1\n"
                "-\t\t\t%s\n"
                "-\t\t EA Type %2lu: %s\n",
                ga->attributeSubtype, udfEARef,
                attrType, EATEXT(attrType));
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /* no FALSE return result */
    }

    /* Byte reserved[3] :
     */
    if(    ga->reserved[0] != 0 || ga->reserved[1] != 0
        || ga->reserved[2] != 0)
    {
        MLIMITbegin( ERROR00level, uctMessageLimit );
          printMessageHead( MLIMIT.vl, EASpace, ga->reserved, NULL);
          fprintf(uctout,
                "Error: Non-zero byte in Extended Attribute"
                                        " Reserved field:\n"
                "-\t\t\t#%02X #%02X #%02X, ECMA 4/14.10.2.3\n"
                "-\t\t EA Type %2lu: %s\n",
                ga->reserved[0], ga->reserved[1], ga->reserved[2],
                attrType, EATEXT(attrType));
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /* no FALSE return result */
    }

    /* Uint32 attributeLength, here
     * check on generic head size only
     */
    if( ga->attributeLength < sizeof(EAGenericHead) )
    {
        MLIMITbegin( ERROR00level, uctMessageLimit );
          printMessageHead(MLIMIT.vl, EASpace,
                           (Byte*)&ga->attributeLength, NULL);
          fprintf(uctout,
                "Error: EA Attribute Length: %lu, expected: at least\n"
                "-\t\t\tsize of EA generic part (%lu), ECMA 4/14.10.2\n",
                ga->attributeLength, sizeof(EAGenericHead));
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        return FALSE;   /* fatal */
    }

    return TRUE;

}   /* end verifyEAGenericHead() */


/* isUdfExtendedAttribute():
 * UDF 3.3.4.5+6, UDF 2.00 6.1
 *
 * Mind different UDF introduction revisions and the fact
 * that some have obligatory been converted to a name stream.
 */
static bool isUdfExtendedAttribute( Byte  *eidID,
                                    bool   isImplUse,
                                    Byte  *EASpace,
                                    UdfMountContext *mc,
                                    Node  *node)
{
    char  *x;
    Uint16 introRevision = MAX_UDFREVISION + 1, /* null  */
           lastRevision  = MIN_UDFREVISION;     /* range */

    /* save matching EntityId Identifier
     * in x because it is \0 terminated
     */
    if( isImplUse )     /* Implementation Use EA */
    {   if(  memcmp(eidID, (x=UDFEA_FreeEASpace_ID), ENTITYID_IDSIZE) == 0
          || memcmp(eidID, (x=UDFEA_DVD_CGMS_Info_ID), ENTITYID_IDSIZE) == 0
          || memcmp(eidID, (x=UDFEA_OS2_EALength_ID), ENTITYID_IDSIZE) == 0
          || memcmp(eidID, (x=UDFEA_Mac_VolumeInfo_ID), ENTITYID_IDSIZE) == 0
          || memcmp(eidID, (x=UDFEA_Mac_FinderInfo_ID), ENTITYID_IDSIZE) == 0
          || memcmp(eidID, (x=UDFEA_Mac_ResourceFork_ID), ENTITYID_IDSIZE) == 0 )
        {
            introRevision = 0x102;              /* Impl Use EA: */
            lastRevision  = MAX_UDFREVISION;    /*   UDF 1.02+  */
        }
        else if( memcmp(eidID, (x=UDFEA_OS2_EA_ID), ENTITYID_IDSIZE) == 0
              || memcmp(eidID, (x=UDFEA_Mac_UniqueIDTable_ID), ENTITYID_IDSIZE) == 0 )
        {
            introRevision = 0x102;  /* Impl Use EA: UDF 1.02 and 1.50   */
            lastRevision  = 0x150;               /* stream in UDF 2.00+ */
        }
        else if( memcmp(eidID, (x=UDFEA_OS400_DirInfo_ID), ENTITYID_IDSIZE) == 0 )
        {
            introRevision = 0x201;              /* Impl Use EA: */
            lastRevision  = MAX_UDFREVISION;    /*   UDF 2.01+   */
        }
        else if( memcmp(eidID, (x=UDFEA_VAT_LVExtension_ID), ENTITYID_IDSIZE) == 0 )
        {
            introRevision = 0x150;              /* Impl Use EA: */
            lastRevision  = 0x150;              /*   UDF 1.50 errata */
        }
    }
    else    /* !isImplUse, Appplication Use EA */
    {   if( memcmp(eidID, (x=UDFEA_FreeAppEASpace_ID), ENTITYID_IDSIZE) == 0 )
        {
            introRevision = 0x102;              /* Appl Use EA: */
            lastRevision  = MAX_UDFREVISION;    /*   UDF 1.02+  */
        }
    }

    if( lastRevision >= introRevision )     /* match with x found */
    {   /* maybe modify current verifier UDF revision range.
         * avoid 'conflicting' message (handled next).
         */
        modifyUdfRevisionRange(
                (Uint16) MIN(introRevision, getUctMaxUdfRevision()),
                (Uint16) MAX( lastRevision, getUctMinUdfRevision()),
                         x );
        /* error if UDF range does not overlap with current
         * verifier UDF revision range.
         */
        if(   introRevision > getUctMaxUdfRevision()    /* no overlap with */
           ||  lastRevision < getUctMinUdfRevision() )  /*  current range  */
        {
            MLIMITbegin( ERROR00level, uctMessageLimit );
              printMessageHead( MLIMIT.vl, EASpace,
                                (Byte*)eidID, NULL);
              fprintf(uctout,
                "Error: Found UDF %s Use EA Entity Identifier:\n"
                    "-\t\t\t",
                    (isImplUse) ? "Implementation" : "Application");
              printBytesName((Byte *)eidID, ENTITYID_IDSIZE,
                             FALSE, MLIMIT.vl);
              fprintf(uctout, ". Not a valid UDF EA for\n"
                    "-\t\t current UDF revison range ");
              printUdfRevisionRange(MLIMIT.vl, ", UDF 3.3.4.5+6.\n");
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
            return FALSE;   /* match, but UDF range does not fit */
        }
        return TRUE;        /* match ok */
    }

    /* lastRevision < introRevision, null range,
     * no match found, warning if first 4 chars are "*UDF"
     */
    if( memcmp(eidID, "*UDF", 4) == 0 )     /* first 4 chars are *UDF */
    {
        MLIMITbegin( WARN01level, MLIMITdefault20 );
          printMessageHead( MLIMIT.vl, EASpace,
                            (Byte*)eidID, NULL);
          fprintf(uctout,
                "Warning: This is not a UDF %s Use EA:\n"
                "-\t\t   ",
                (isImplUse) ? "Implementation" : "Application");
          printBytesName((Byte *)eidID, ENTITYID_IDSIZE,
                         TRUE, MLIMIT.vl);
          fprintf(uctout, ". Please do not\n"
            "-\t\t   use \"*UDF\" as first 4 characters of"
                                    " a non UDF\n"
            "-\t\t   Extended Attribute EntityID Identifier,"
                                " UDF 3.3.4.5+6\n");
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }
    return FALSE;       /* no match found */

}   /* end isUdfExtendedAttribute() */


/* nodeIsUdfDefinedStream():
 * Check if UDF defined System or Non-System Stream
 */
static char *UdfSysStreams[] =      /* UDF 3.3.7 */
{   UDF_SS_UNIQMAPPING,
    UDF_SS_NONALLOC,
    UDF_SS_POWERCAL,
    UDF_SS_BACKUP
};
static char *UdfNonSysStreams[] =   /* UDF 3.3.8 */
{   UDF_NSS_MACRESFORK,
    UDF_NSS_OS2EA,
    UDF_NSS_NTACL,
    UDF_NSS_UNIXACL
};
static char *nodeIsUdfDefinedStream( Node *node,
                                     bool *pIsSysStream,
                                     bool *pHeadMatch )
{   int n,
        nmbSysStreams    =   sizeof(UdfSysStreams)/sizeof(UdfSysStreams[0]),
        nmbNonSysStreams = sizeof(UdfNonSysStreams)/sizeof(UdfNonSysStreams[0]);

    /* first do 'headMatch', UDF 3.3.5.2
     */
    (*pHeadMatch) = FALSE;
    stringIsUnicodeName(node, "*UDF", pHeadMatch);

    /* match UDF Defined Non-System Stream names, UDF 3.3.8
     */
    for( n = 0; n < nmbNonSysStreams; n++ )
    { if( stringIsUnicodeName(node, UdfNonSysStreams[n], NULL) )
      { (*pIsSysStream) = FALSE;
        return UdfNonSysStreams[n]; /* found UDF Non-System Stream */
      }
    }
    /* match UDF Defined System Stream names, UDF 3.3.7
     */
    for( n = 0; n < nmbSysStreams; n++ )
    { if( stringIsUnicodeName(node, UdfSysStreams[n], NULL) )
      { (*pIsSysStream) = TRUE;
        return UdfSysStreams[n];     /* found UDF System Stream */
      }
    }
    return NULL;        /* none found */

}   /* end nodeIsUdfDefinedStream() */

/* verifyUdfDefinedStreams():
 * UDF 2.00+ : 3.3.5.2, 3.3.7 and 3.3.8
 * Check if UDF defined stream name.
 *
 * implementation note: similar to the above isUdfExtendedAttribute().
 */
extern void verifyUdfDefinedStreams( Node *node, UdfMountContext *mc )
{
    char    *xMatch;
    bool     foundSysStreamName, foundHeadMatch, metaSet;
    Byte    *pFidFileId;

    /* conditions that prevent further verification:
     */
    if(  !NODEFLAGS_IS_SET( node, NFB_STREAM)   /* no stream */
       || node->fid == NULL                     /* no FID */
       || node->unicodeNameLen == 0     /* no name or unicode error */
       || getUctMaxUdfRevision() < 0x200 )      /* UDF 1.xx */
    {   return;     /* no action */
    }
    pFidFileId = (Byte*) &node->fid->startOfImplementationUse
                        + node->fid->lengthOfImplementationUse;

    /* find match FID identifier in node->unicodeName[] with
     * all known UDF Defined stream names and determine whether
     * the match is a System or Non-System Stream (foundSysStreamName).
     * Verify also if first 4 chars match with "*UDF" (headMatch).
     * Note that headMatch is also determined correctly when
     * nodeIsUdfDefinedStream() returns NULL
     * [= no match with UDF Defined (Non-)System Stream name].
     */
    xMatch = nodeIsUdfDefinedStream( node, &foundSysStreamName,
                                           &foundHeadMatch );
    if( xMatch != NULL )        /* match found */
    {   bool isSystemStream;
        /* All current UDF Defined Stream names were defined in UDF 2.00
         * and none were deleted/modified since, so we need not to bother
         * about introRevision and lastRevision for each name.
         * Avoid confusing modifyUdfRevisionRange() messages.
         */
        modifyUdfRevisionRange(
                (Uint16) MIN(0x200,           getUctMaxUdfRevision()),
                (Uint16) MAX(MAX_UDFREVISION, getUctMinUdfRevision()),
                         xMatch );

        /* Error if UDF Non-System Stream name is used for System Stream
         * and vice-versa.
         */
        isSystemStream = NODEFLAGS_IS_SET( node, NFB_SYSTEM);
        if( isSystemStream != foundSysStreamName )
        { MLIMITbegin( ERROR00level, MLIMITdefault20 );
            printMessageHead( MLIMIT.vl, (Byte*) node->fid,
                              pFidFileId, NULL );
            fprintf(uctout,
              "Error: UDF Defined %s Stream name used for %s Stream,\n"
              "-\t\t\tUDF 3.3.5.2, 3.3.7, 3.3.8.\n",
                (isSystemStream) ? "Non-System" : "System",
                (isSystemStream) ? "System" : "Non-System" );
            nodePrintUnicodeNameContLine(node,mc);
          MLIMITend;
        }
        else    /* use ok */
        { /* Check FID metadata bit for UDF Defined Streams (3.3.5.2)
           * Currently (UDF 2.60),
           *    UDF Defined System Streams have metadata bit ONE  (3.3.7)
           *        and Non-System Streams have metadata bit ZERO (3.3.8).
           */
          metaSet = isBitOn(node->fid->fileCharacteristics,FCB_METADATA);
          if( metaSet != isSystemStream )
          { MLIMITbegin( ERROR00level, MLIMITdefault20 );
              printMessageHead( MLIMIT.vl, (Byte*) node->fid,
                  (Byte*) &node->fid->fileCharacteristics, NULL );
              fprintf(uctout,
                "Error: File Characteristics Metadata bit: %u, expected: %u\n"
                "-\t\t\tfor UDF Defined %s Stream, UDF %s.\n",
                (metaSet) ? 1 : 0, (metaSet) ? 0 : 1,
                (isSystemStream) ? "System" : "Non-System",
                (isSystemStream) ? "3.3.7"  : "3.3.8");
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
          }
        }
    }
    else if( foundHeadMatch )   /* no UDF Defined Stream name match */
    {                           /*  but first 4 chars match "*UDF"  */
      MLIMITbegin( ERROR00level, MLIMITdefault20 );
        printMessageHead( MLIMIT.vl, (Byte*) node->fid,
                            pFidFileId, NULL );
        fprintf(uctout,
          "Error: Stream names starting with \"*UDF\""
                        " are reserved, UDF 3.3.5.2.\n");
        nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
    }

}   /* end verifyUdfDefinedStreams() */


/* inspectBeforeEndianSwapEATail(),
 * execute before endianSwapEATail().
 * determine minimum length needed for endian swap of EA Tail
 * and check if UDF Impl Use or Appl Use EA.
 * endianSwapEAGenericHead() has successfully been executed.
 *
 * result in *pIsUdfEA:
 * iff Implementation EA or Application EA with
 *     EntityID Identifier starting with "*UDF"
 * then: TRUE   ( length enough for headerChecksum swap)
 * else: FALSE
 *
 * return result:
 *  if insuffucient bytes read for endian swap
 *  then: FALSE
 *  else: TRUE
 */
static bool inspectEAbeforeSwapTail(
                    ExtendedAttribute *ea,
                    Uint32   bytesRead,
                    bool    *pIsUdfEA,
                    Byte    *EASpace,
                    UdfMountContext *mc,
                    Node    *node)
{
    Uint32  minLen = 0;

    *pIsUdfEA = FALSE;

    /* determine minimum byte length needed for endian swap
     */
    switch( ea->genericHead.attributeType )
    {
    case EATYPE_CHARSETINFO:    /* ECMA 4/14.10.3 */
        minLen = 16;
        break;
    case EATYPE_FILETIMES:      /* ECMA 4/14.10.5 */
    case EATYPE_INFOTIMES:      /* ECMA 4/14.10.6 */
        minLen = 20;
        break;
    case EATYPE_DEVSPEC:        /* ECMA 4/14.10.7,  UDF 3.3.4.4 */
        minLen = 24;            /* EntityID NOT included */
        break;
    case EATYPE_IMPLUSE:        /* ECMA 4/14.10.8 */
        {   ImplementationUseExtendedAttribute *iea = &ea->implUseEA;
            *pIsUdfEA = isUdfExtendedAttribute(
                            iea->implementationIdentifier.Identifier,
                            TRUE, EASpace, mc, node);   /* isImpl */
            if( *pIsUdfEA )
                 minLen = 50;   /* includes headerChecksum */
            else minLen = 48;
        }
        break;
    case EATYPE_APPLUSE:        /* ECMA 4/14.10.9 */
        {   ApplicationUseExtendedAttribute *aea = &ea->applUseEA;
            *pIsUdfEA = isUdfExtendedAttribute(
                            aea->applicationIdentifier.Identifier,
                            FALSE, EASpace, mc, node);  /* !isImpl */
            if( *pIsUdfEA )
                 minLen = 50;   /* includes headerChecksum */
            else minLen = 48;
        }
        break;
    }

    if( bytesRead < minLen )
    {   return FALSE;           /* fatal */
    }
    return TRUE;
}


static bool swapAndVerifyExtendedAttribute(
                            ExtendedAttribute *ea,
                            Uint32 bytesRead,
                            Uint8 *pEAGroup,
                            Byte  *EASpace,
                            UdfMountContext *mc,
                            Node  *node)
{
    const MediumInfo  *vmi = getTheMediumInfo();
    Uint32  blockSize = vmi->blockSize;
    Uint16  checksum;
    Uint16 *pEAChecksum = NULL;
    Uint32  attType, attLen, netLen = 0, padLen;
    bool    isUdfEA = FALSE,
            result = FALSE;

    /* verify EA generic head and inspect tail
     */
    if( bytesRead >= sizeof(EAGenericHead) )
    {
        endianSwapEAGenericHead(&ea->genericHead);

        /* verifyEAGenericHead() will always first show
         * informational text, defining the Attribute Type
         */
        if( !verifyEAGenericHead(ea, pEAGroup, EASpace, mc, node) )
        {   return FALSE;       /* fatal, abort EA check */
        }
        result = inspectEAbeforeSwapTail(ea, bytesRead, &isUdfEA,
                                         EASpace, mc, node);
    }
    else    /* bytesRead < sizeof(EAGenericHead) */
    {   result = FALSE;
    }
    if( result == FALSE )        /* insufficient bytes read */
    {       /* unable to complete endian swap */
        MLIMITbegin( ERROR00level, MLIMITdefault20 );
          printMessageHead( MLIMIT.vl, EASpace,
                            (Byte*)ea, NULL);
          fprintf(uctout,
            "Error: EA does not fit in remaining EA Space"
                                        " of %lu bytes\n",
                    bytesRead);
          nodePrintUnicodeNameContLine(node,mc);
          printEAabortAndUDFref( MLIMIT.vl );
        MLIMITend;
        return FALSE;   /* fatal */
    }

    /* if verifyEAGenericHead() and inspectEAbeforeSwapTail()
     * ok,swap tail.
     */
    endianSwapExtendedAttributeTail(ea, bytesRead, isUdfEA, EASpace);
    attType = ea->genericHead.attributeType;
    attLen  = ea->genericHead.attributeLength;

    /* Verfy tail now.
     * endianSwapExtendedAttributeTail() has set isUdfEA TRUE for
     * a UDF Implementation Use EA and a UDF Application Use EA.
     * Further it checked if bytesRead was sufficient to do the endian
     * swap. Now determine the net EA length so that the number of
     * unused padding bytes at the end of the EA is known too.
     */
    switch( attType )
    {
    case EATYPE_CHARSETINFO:    /* ECMA 4/14.10.3 */
        {   CharSetInformationExtendedAttribute *cea = &ea->charSetInfoEA;
            netLen = 17 + cea->escapeSequencesLength;
        }
        break;
    case EATYPE_FILETIMES:      /* ECMA 4/14.10.5 */
        {   FileTimesExtendedAttribute *fea = &ea->fileTimesEA;
            netLen = 20 + fea->dataLength;
        }
        break;
    case EATYPE_INFOTIMES:      /* ECMA 4/14.10.6 */
        {   InformationTimesExtendedAttribute *iea = &ea->infoTimesEA;
            netLen = 20 + iea->dataLength;
        }
        break;
    case EATYPE_DEVSPEC:        /* ECMA 4/14.10.7, UDF 3.3.4.4 */
        {   DeviceSpecificationExtendedAttribute *dea = &ea->devSpecEA;
            netLen = 24 + dea->implementationUseLength;
            if( dea->implementationUseLength < sizeof(EntityID) )
            {
                MLIMITbegin( ERROR00level, MLIMITdefault20 );
                  printMessageHead( MLIMIT.vl, EASpace, (Byte*)ea, NULL);
                  fprintf(uctout,
                    "Error: Device Specification EA ImplementationUseLength: %lu,\n"
                    "-\t\t\tshall be at least %lu for EntityID, UDF 3.3.4.4\n",
                        dea->implementationUseLength, sizeof(EntityID));
                  nodePrintUnicodeNameContLine(node,mc);
                MLIMITend;
            }
            else
            {
                verifyEntityID(&dea->implementationUse.impUseID,
                               ENTITY_SUFFIX_IMPL, NULL, NULL, EASpace);
            }
        }
        break;
    case EATYPE_IMPLUSE:        /* ECMA 4/14.10.8 */
        {   ImplementationUseExtendedAttribute *iea = &ea->implUseEA;
            netLen = 48 + iea->implementationUseLength;
            if( isUdfEA )   /* UDF Implementation EA, with eaCheckSsum */
            {   verifyEntityID( &iea->implementationIdentifier,
                            ENTITY_SUFFIX_UDF, NULL, NULL, EASpace);
                if( iea->implementationUseLength < sizeof(*pEAChecksum) )
                     pEAChecksum = NULL;    /* no room for EA checksum */
                else pEAChecksum = &iea->implementationUse.headerChecksum;
            }
            else            /* non-UDF Implementation EA */
            {   verifyEntityID( &iea->implementationIdentifier,
                            ENTITY_SUFFIX_IMPL, NULL, NULL, EASpace);
            }
        }
        break;
    case EATYPE_APPLUSE:
        {   ApplicationUseExtendedAttribute *aea = &ea->applUseEA;
            netLen = 48 + aea->applicationUseLength;
            if( isUdfEA )   /* UDF Application EA, with eaCheckSsum */
            {   verifyEntityID( &aea->applicationIdentifier,
                            ENTITY_SUFFIX_UDF, NULL, NULL, EASpace);
                if( aea->applicationUseLength < sizeof(*pEAChecksum) )
                     pEAChecksum = NULL;    /* no room for EA checksum */
                else pEAChecksum = &aea->applicationUse.headerChecksum;
            }
            else            /* non-UDF Application EA */
            {   verifyEntityID( &aea->applicationIdentifier,
                            ENTITY_SUFFIX_APPL, NULL, NULL, EASpace);
            }
        }
        break;
    }

    /* netLen holds net EA length without possible
     * padding bytes at the end.
     */
    if( netLen > attLen || attLen > bytesRead )
    {
        MLIMITbegin( ERROR00level, MLIMITdefault20 );
          printMessageHead( MLIMIT.vl, EASpace,
                    (Byte*)&ea->genericHead.attributeLength, NULL);
          fprintf(uctout,
            "Error: Extended Attribute size error,\n"
            "-\t\t\t calculated EA length: %lu\n"
            "-\t\t\t  EA Attribute Length: %lu\n"
      "-\t\t available remaining EA Space: %lu\n",
                netLen, attLen, bytesRead);
          nodePrintUnicodeNameContLine(node,mc);
          printEAabortAndUDFref( MLIMIT.vl );
        MLIMITend;
        return FALSE;           /* fatal */
    }
    padLen = attLen - netLen;   /* padLen >= 0 */

    /* verify possible UDF EA Header checksum
     * mind that UDF EA headerChecksum calculation does not
     * depend on byte order, so endian swap is no problem.
     */
    if( isUdfEA )
    {
        if( pEAChecksum == NULL )   /* no room for EA checksum */
        {   Uint32 off, len; char *txt, *udf;
            if( attType == EATYPE_IMPLUSE )
            {   ImplementationUseExtendedAttribute *iea = &ea->implUseEA;
                len = iea->implementationUseLength;
                off = offsetof(ImplementationUseExtendedAttribute,
                               implementationUseLength);
                txt = "ImplementationUseLength";
                udf = "3.3.4.5";
            }
            else    /* attType == EATYPE_APPLUSE */
            {   ApplicationUseExtendedAttribute *aea = &ea->applUseEA;
                len = aea->applicationUseLength;
                off = offsetof(ApplicationUseExtendedAttribute,
                               applicationUseLength);
                txt = "ApplicationUseLength";
                udf = "3.3.4.6";
            }
            MLIMITbegin( ERROR00level, MLIMITdefault20 );
              printMessageHead( MLIMIT.vl, EASpace,
                                ((Byte*)ea) + off, NULL);
              fprintf(uctout,
                "Error: UDF EA %s: %lu, less than %lu.\n"
                "-\t\t\tNot enough for EA Header Checksum,"
                                        " UDF %s\n"
                "-\t\t\tEA Attribute Length: %lu\n"
                "-\t\t\tEA      used length: %lu\n",
                    txt, len, sizeof(*pEAChecksum), udf,
                        attLen, netLen);
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
            /* no FALSE return result */
        }
        else if(    (*pEAChecksum)
                 != (checksum = computeEAChecksum((Byte *)ea)) )
        {
            MLIMITbegin( ERROR00level, MLIMITdefault20 );
              printMessageExpectedU32( MLIMIT.vl, EASpace,
                               (Byte*) pEAChecksum,
                "Error: EA Header Checksum", "#%04X",
                            (*pEAChecksum), checksum, ",\n");
              fprintf(uctout,
                "-\t\t\t%s, UDF 3.3.4.5, 3.3.4.6.\n",
                        EATEXT(attType));
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
            /* no FALSE return result */
        }
    }

    /* warn if unneeded block alignment.
     */
    if( attLen >= blockSize && netLen < blockSize )
    {
        MLIMITbegin(INFO01level, MLIMITdefault01);
          printMessageHead( MLIMIT.vl, EASpace, (Byte*)ea, NULL);
          fprintf(uctout,
             "Note: Block alignment requirement for"
                                    " this EA only caused\n"
            "-\t\t\tby %lu padding bytes. Nett EA length:"
                                        " %lu, UDF 3.3.4\n",
            padLen, netLen);
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /* no FALSE return result */
    }

#ifdef  UCT_PUT_ON_HOLD     /* TODO: check EA #00 padding */
    /* padding bytes being #00 not required by ECMA 4/14.10.2.5,
     * but what about ECMA 4/14.9.21 'unused bytes' ??
     * posted question: osta_tc reflector Feb 17, 2000
     */

    /* padLen >= 0, check #00 padding bytes
     */
    {   int cnt;
        Byte *firsByte = ((Byte*)ea) + netLen;

        if( !verifyZeros(firsByte, padLen, &cnt, NULL, NULL) )
        {
            MLIMITbegin( ERROR00level, MLIMITdefault20 );
              printMessageHead( MLIMIT.vl, EASpace, firsByte, NULL);
              fprintf(uctout,
                "Error: %lu non-zero bytes in %lu bytes"
                                    " padding area at\n"
                "-\t\t\tthe end of this EA,"
                        " ECMA 4/14.10.2.5, 4/14.9.21, UDF 3.3.4\n"
                "-\t\t\tEA Attribute Length: %lu\n"
                "-\t\t\tEA      used length: %lu\n",
                    cnt, padLen, attlen, netLen);
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
            /* no FALSE return result */
        }
    }
#endif  /** UCT_PUT_ON_HOLD **/

    return TRUE;

}   /* end swapAndVerifyExtendedAttribute() */

/* verifyEAHeaderDescriptorLocation()
 * Implementation Attributes Location
 * and Application Attributes Location, UDF 3.3.4.1:
 * Mind 3 different rules for UDF 1.02, 1.50 and 2.00+
 *
 * UDF 1.02:
 *  If the attributes associated with the location fields
 *  do not exist, then the value of the location field shall
 *  be the end of the extended attribute space.
 * UDF 1.50:
 *  If the attributes associated with the location fields
 *  do not exist, then the value of the location field shall
 *  point to the byte after the extended attribute space.
 * UDF 2.00+:
 *  read:   A value in one of the location fields equal to or
 *          greater than the length of the EA space shall be
 *          interpreted as an indication that the corresponding
 *          attribute does not exist.
 *  write:  If an attribute associated with one of the location
 *          fields does not exist, then the value of the
 *          corresponding location field shall be set to #FFFFFFFF.
 *
 * Implementation notes:
 *  The interpretation here will be that UDF 1.50 and 1.02 mean
 *  the same, but UDF 1.02 does not have the exact formulation.
 */
static bool verifyEAHeaderDescriptorLocation(
                  ExtendedAttributeHeaderDescriptor *eahd,
                  Uint32  EASpaceLength,
                  Uint32  expLoc,
                  Uint32 *pLocation, char *fieldName,
                  UdfMountContext *mc, Node *node)
{
    Uint32 eahdLoc = *pLocation;        /* value in EAHD descriptor */
    bool   is200plus, is150min;
    Uint16 maxRevision;

    /* check Attribute Location in EAHD
     */
    if( expLoc == MAX_UINT32 )
    {   /* Attribute does not exist in EA Space
         * determine expected location value in EAHD
         */
        maxRevision = getUctMaxUdfRevision();

        /* If EAHD location value is MAX_UINT32 (0xFFFFFFFF)
         * then try to modify UDF (min) revision to 2.00+
         */
        if( eahdLoc == MAX_UINT32 && maxRevision >= 0x200 )
        {   (void) modifyUdfRevisionRange(0x200, MAX_UDFREVISION,
                                          fieldName);
        }
        is150min  = (maxRevision <= 0x150);
        is200plus = (getUctMinUdfRevision() >= 0x200);

        if(      is150min )  expLoc = EASpaceLength;
        else if( is200plus ) expLoc = MAX_UINT32;

        if( (!is150min) && (!is200plus) )
        {   /* not enough UDF revision range information
             * show first EASpace end offset
             */
            MLIMITbegin(WARN01level, MLIMITdefault01);
              printMessageHead(MLIMIT.vl, (Byte*)eahd,
                    ((Byte*)eahd) + EASpaceLength,
                    "End of EA Space verification. Warning:\n-");
              printMessageHead(MLIMIT.vl, (Byte*)eahd,
                               (Byte*)pLocation, NULL);
              fprintf(uctout, "%s: %lu,\n"
                  "-\t\t Unable to test 'not exist' value (UDF 3.3.4.1)\n"
                  "-\t\t for UDF revision range: ", fieldName, eahdLoc);
              printUdfRevisionRange(MLIMIT.vl,
                "\n-\t\t Please specify -udf <revision>\n");
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
            /* no FALSE return result */
        }
        else if( eahdLoc != expLoc )
        {   /* error && (is150min || is200plus)
             * show first EASpace end offset
             */
            MLIMITbegin(ERROR00level, MLIMITdefault20);
              printMessageHead(MLIMIT.vl, (Byte*)eahd,
                    ((Byte*)eahd) + EASpaceLength,
                    "End of EA Space verification. Error:\n-");
              printMessageHead(MLIMIT.vl, (Byte*)eahd,
                              (Byte*)pLocation, NULL);
              fprintf(uctout, "%s: %lu%s,\n"
                "-\t\t expected: %lu%s. If attributes do not exist, the\n"
                "-\t\t\tcorresponding Location value shall be equal\n"
                "-\t\t\tto %s, UDF %s, 3.3.4.1\n"
                "-\t\t Note: Rule has been changed for UDF 2.00+\n",
                fieldName,
                eahdLoc, (eahdLoc == MAX_UINT32) ? " (#FFFFFFFF)" : "",
                 expLoc,  (expLoc == MAX_UINT32) ? " (#FFFFFFFF)" : "",
                  (is200plus) ? "#FFFFFFFF" : "the EA Space Length",
                  (is200plus) ? "2.00+" : "1.50-");
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
            /* no FALSE return result */
        }
    }
    else if( eahdLoc != expLoc )
    {   /* Attribute exists in EA Space but EAHD value is wrong
         * show first EASpace end offset
         */
        MLIMITbegin( ERROR00level, MLIMITdefault20 );
          printMessageHead(MLIMIT.vl, (Byte*)eahd,
                ((Byte*)eahd) + EASpaceLength,
                    "End of EA Space verification. Error:\n-");
          printMessageHead(MLIMIT.vl, (Byte*)eahd,
                           (Byte*)pLocation, NULL);
          fprintf(uctout, "%s: %lu,\n"
            "-\t\t expected: %lu,"
                " Attribute exists in EA Space, UDF 3.3.4.1\n",
            fieldName, eahdLoc, expLoc);
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /* no FALSE return result */
    }

    return TRUE;

}   /* end verifyEAHeaderDescriptorLocation() */

/* EA Header Descriptor tail verification.
 * Called from verifyDescriptorTail(), no action.
 * Verify EA Header Descriptor locations by calling
 * verifyEAHeaderDescriptorLocation() twice from
 * swapAndVerifyExtendedAttributesSpace() after EA Space
 * verification when the expected values for the
 * Attributes Location fields are known.
 */
static bool verifyExtendedAttributeHeaderDescriptorTail()
{
    return TRUE;
}

/* ECMA 4/9.1, 4/14.10.1, UDF 3.3.4.1
 * ECMA: Extended Attributes Space
 *  UDF: Extended Attribute Space
 *                         ^
 * An Extended Attribute Space (EASpace) can either be
 * embedded in a (E)FE, or in an EA File, UDF 3.3.4.
 * In the first case, EAs greater that the block size cannot
 * exist, because the (E)FE itself does not exceed the blockSize.
 * This means that the Implementation Use EA block alignment test
 * is only done in the 'EA File' case and has to be done with respect
 * to the begin of the EA Space, which is the first byte of the
 * Extended Attribute Header Descriptor (EAHD).
 * Further 4 byte alignment of the begin of the EA Space is ensured
 * in both EA Space cases.
 *
 * All EA errors are reported with the bytes offset from the
 * begin of the EA Space as: EAHD <offset><tab><error message>.
 */
extern bool swapAndVerifyExtendedAttributesSpace(
                                Byte *EASpace, Uint32 EASpaceLength,
                                Uint32 expectedTagLocation, Byte *d,
                                UdfMountContext *mc, Node *node)
{   ExtendedAttributeHeaderDescriptor *eahd;
    ExtendedAttribute *ea;
    const MediumInfo  *vmi = getTheMediumInfo();
    Uint32   blockSize = vmi->blockSize;
    Byte    *bp;
    Uint8    eaGroup, eaGroupStatus;
    Uint32   off, restLen, attrType, attrLen, mult;
    Uint32   firstImplEaLocation, firstApplEaLocation;

    /* Mind that no endian swap done yet of EA Space
     */
    if( EASpaceLength < sizeof(ExtendedAttributeHeaderDescriptor) )
    {
        MLIMITbegin(ERROR00level, MLIMITdefault20);
          printMessageHead(MLIMIT.vl, d, EASpace, NULL);
          fprintf(uctout,
            "Error: Extended Attributes Space Length: %lu,\n"
            "-\t\t expected: at least %lu for"
                    " Extended Attribute Header Descriptor\n",
            EASpaceLength, sizeof(ExtendedAttributeHeaderDescriptor));
          nodePrintUnicodeNameContLine(node,mc);
          printEAabortAndUDFref( MLIMIT.vl );
        MLIMITend;
        return FALSE;       /* fatal */
    }

    if( !swapAndVerifyDescriptor(EASpace,
                    sizeof(ExtendedAttributeHeaderDescriptor),
                    tidEAHD, NULL, expectedTagLocation, mc, node) )
    {   /** TODO:  testing: EASpaceLength
         **     instead of: sizeof(...) above ??
         **/
        return FALSE;       /* fatal ?? */
    }
    eahd = (ExtendedAttributeHeaderDescriptor*) EASpace;

    /* find value for EAHD ... Location fields.
     */
    firstImplEaLocation = firstApplEaLocation = MAX_UINT32;

    /* The order that different EA types occur in an EA Space will
     * be checked by the EA group status. It has a value as defined
     * for the EA_GROUP_xx values that increase with their position
     * in an EA space as defined in UDF 3.3.4 and ECMA 4/9.1.
     * An obvious exeption is EAGROUP_ERROR.
     */
    eaGroupStatus = EAGROUP_ECMA;       /* first EA group */
    for( off = sizeof(ExtendedAttributeHeaderDescriptor),
         bp  = EASpace + off;
         off < EASpaceLength;
         off += attrLen,
         bp  += attrLen )
    {
        restLen = EASpaceLength - off;
        ea = (ExtendedAttribute*) bp;

        /* TODO: is it possible that unused space at the end of
         *       the EA space contains only #00 padding bytes,
         *       see "unused bytes" in ECMA 4/14.9.21.
         *       if so, then check here.
         */

        if( !swapAndVerifyExtendedAttribute(ea, restLen, &eaGroup,
                                            EASpace, mc, node) )
        {   break;  /* break of EA verification loop */
        }
        attrType = ea->genericHead.attributeType;
        attrLen  = ea->genericHead.attributeLength;

        /* by returning TRUE, swapAndVerifyExtendedAttribute()
         * did also ensure that EA fits in remaining EA Space
         * and that the EA group is ok, so:
         */
        UCTASSERT(   attrLen > 0 && attrLen <= restLen
                  && eaGroup != EAGROUP_ERROR);

        /* Find first Attributes for EAHD locations check.
         * Check after EA Space complete.
         */
        if(    attrType == EATYPE_IMPLUSE           /* first Impl */
            && firstImplEaLocation == MAX_UINT32 )  /*   Use EA   */
        {   firstImplEaLocation = off;
        }
        else if( attrType == EATYPE_APPLUSE          /* first Appl */
              && firstApplEaLocation == MAX_UINT32 ) /*  Use EA    */
        {   firstApplEaLocation = off;
        }

        /* Check EA group ordering.
         */
        if( eaGroup < eaGroupStatus )   /* wrong order */
        {
            MLIMITbegin(ERROR00level, MLIMITdefault20);
              printMessageHead(MLIMIT.vl, EASpace, bp, NULL);
              fprintf(uctout,
                      "EA order error: %s\n"
                "-\t\t appears after : %s,\n"
                "-\t\t %s\n",
                EAGROUP_TEXT(eaGroup),
                EAGROUP_TEXT(eaGroupStatus), udfEARef);
            MLIMITend;
            /* no reason for break of EA verification loop */
        }
        eaGroupStatus = eaGroup; /* yes: also in error case above */

        /* Check EA alignment on EA BEGIN and end, UDF 3.3.4.
         * Align on blockSize, or on 4 bytes. If block alignment
         * is ok, then also 4 bytes alignment is ok. A 4 byte
         * alignment error is fatal, abort EA Space verification.
         *
         * Mind that also other-than-EAGROUP_IMPL_BLOCKALIGN EAs
         * must be block-aligned if size >= blockSize,
         * UDF 3.3.4 !!.
         *
         * TODO: what if this is the first ECMA 167 EA in an
         *     EA Space ? The start cannot be block aligned !!
         *     see Note: ... printed in that case.
         */
        mult = (attrLen >= blockSize) ? blockSize : 4;
        if( (off % mult) != 0 || ((off + attrLen) % mult) != 0 )
        {   bool isFatal;
            isFatal = (mult == 4 || (attrLen % 4) != 0);
            MLIMITbegin( ERROR00level, MLIMITdefault20 );
              printMessageHead(MLIMIT.vl, EASpace,
                (Byte*)&ea->genericHead.attributeLength, NULL);
              fprintf(uctout,
                      "Error: %salignment error for %s,\n"
                      "-\t\t\tbegin offset: %4lu \tsee UDF 3.3.4\n"
                      "-\t\t\t end  offset: %4lu \tand ECMA 4/14.10.2.5\n"
                "-\t\t    Attribute Length: %4lu,\n"
                      "-\t\t\t    expected: all multiples of %s,\n",
                (isFatal) ? "Fatal " : "",
                EATEXT(attrType), off, off + attrLen, attrLen,
                (mult == 4) ? "4" : "the block size");
              nodePrintUnicodeNameContLine(node,mc);
              if(   attrLen >= blockSize
                 && off == sizeof(ExtendedAttributeHeaderDescriptor)
                 && eaGroupStatus == EAGROUP_ECMA )
              { fprintf(uctout,
                  "-\tNote: Start offset alignment may not"
                                        " be possible if this\n"
                  "-\t      is the one and only ECMA 167 EA"
                                        " in this EA Space\n");
              }
              else if( (off % mult) != 0 )
              { fprintf(uctout,
                    "-\tBegin offset alignment error caused"
                        " by previous EA (if any) in EA Space.\n");
              }
              if( isFatal )
              { fprintf(uctout,
                    "-\tFatal 4 byte alignment error,"
                        " remaining EA space: %lu\n",
                        restLen - attrLen);
                printEAabortAndUDFref( MLIMIT.vl );
              }
            MLIMITend;
            if( isFatal )
            {   break;  /* break of EA verification loop */
            }
            /* else: no reason for break of EA verification loop */
        }

    }   /* end of EA Space */

    /* verify EAHD Attribute Location fields
     */
    if(    off == EASpaceLength         /* NO break of EA verification loop */
        || firstImplEaLocation < off )  /* Impl Use EA found */
    {   (void) verifyEAHeaderDescriptorLocation(eahd, EASpaceLength,
                         firstImplEaLocation,
                        &eahd->implementationAttributesLocation,
                        "Implementation Attributes Location",
                         mc, node);
    }
    if(    off == EASpaceLength         /* NO break of EA verification loop */
        || firstApplEaLocation < off )  /* Appl Use EA found */
    {   (void) verifyEAHeaderDescriptorLocation(eahd, EASpaceLength,
                         firstApplEaLocation,
                        &eahd->applicationAttributesLocation,
                        "Application Attributes Location",
                         mc, node);
    }
    /* no specific test for:
     *  (   expectedImplLocation < EASpaceLength
     *   && expectedImplLocation > expectedApplLocation )
     * because an EA order error is already flagged in that case
     */
    return TRUE;    /* no fatal errors */
}

/* verify FID / (E)FE consistency
 * execute after both FID and (E)FE have been read
 * normally the FID is read first, exept for a parent FID which
 * is read after the (E)FE.
 *
 * return value:
 *  if not both FID and FE present
 *  then TRUE
 *  else if an inconsistency is found
 *  then FALSE
 *  else TRUE
 *
 * Mind that parent FID node refers to node->parent->parent
 */
extern bool verifyFidFeConsistency(Node *node, UdfMountContext *mc)
{
    Uint8 fileCharacteristics, fileType;
    bool  dirBit, dirBitExpect, result = TRUE;
    Node *feNode;

    /* TODO: implementation ready ?
     */
#ifdef  DEBUG01
    MLIMITbegin(DEBUG01level, MLIMITdefault01);     /* TODO: implement */
            implementationNotReady( "FID / (E)FE Consistency" );
    MLIMITend;
#endif  /* DEBUG01 */

    UCTASSERT( node != NULL && mc != NULL );

    if( node->fid == NULL )
    {   return TRUE;            /* no FID */
    }
    feNode = node;      /* default for (E)FE */

    /* special case for parent FID: (E)FE is
     * in node->parent->parent->fe.
     */
    if( NODEFLAGS_IS_SET(node, NFB_PARENTFID) )
    {   UCTASSERT(   node->parent != NULL
                  && node->parent->parent != NULL );
        feNode = node->parent->parent;
    }
    if( feNode->fe == NULL )
    {   return TRUE;            /* no (E)FE */
    }
    (void) verifyFidFeUniqueIdConsistency(node, feNode, mc);

    fileCharacteristics = node->fid->fileCharacteristics;
    fileType = (pFE_icbTag(feNode->fe))->fileType;
    dirBit   = isBitOn(fileCharacteristics, FCB_DIRECTORY);
    dirBitExpect =
        (   fileType == FT_DIRECTORY
         || fileType == FT_STREAM_DIRECTORY );  /* TODO: check */

    if( dirBit != dirBitExpect )
    {   result = FALSE;                     /* return result */
        MLIMITbegin( ERROR00level, uctMessageLimit );
          printMessageExpectedU32( MLIMIT.vl, (Byte*) node->fid,
            (Byte*) &node->fid->fileCharacteristics,
            "Error: File Characteristics Directory bit",
            "%u", dirBit, dirBitExpect, "\n-");
          printMessageHead(MLIMIT.vl, feNode->fe,   /* continuation line */
                           &(pFE_icbTag(feNode->fe))->fileType, NULL);
          fprintf(uctout,
            "\tFID/FE inconsistency for File Type %u (%s),\n"
            "-\t\t\tECMA 4/14.4.3, 4/14.6.6\n",
              fileType, FT_TEXT4(fileType));
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }

    return result;
}

/* verifyEfeObjectSize():
 * EFE Uint64 objectSize : ECMA 4/14.17.11
 */
extern bool verifyEfeObjectSize( ExtendedFileEntry *efe,
                                 Uint64 streamsObjectSize,
                                 char  *txt1,
                                 UdfMountContext *mc,
                                 Node *node )
{
    Uint64 expectedSize = efe->informationLength + streamsObjectSize;

    if( efe->objectSize != expectedSize )
    {
      MLIMITbegin( ERROR00level, uctMessageLimit );
        if( txt1 != NULL ) fprintf(uctout, "%s", txt1);
        printMessageHead( MLIMIT.vl, (Byte*) efe,
            (Byte*) &efe->objectSize, "Error: Object Size: " );
        printUint64(MLIMIT.vl, efe->objectSize, FALSE, NULL);
        fprintf(uctout, ", expected: ");
        printUint64(MLIMIT.vl, expectedSize, FALSE, NULL);
        fprintf(uctout, "\n-\t     EFE Information Length: ");
        printUint64(MLIMIT.vl, efe->informationLength, FALSE, NULL);

        if( adGetExtentSize(&efe->streamDirectoryICB) == 0 )
        { fprintf(uctout, "\n-\t\t No Stream Directory defined");
        }
        else
        { fprintf(uctout, "\n-\t streams Information Length: ");
          printUint64(MLIMIT.vl, streamsObjectSize, FALSE, NULL);
        }
        fprintf(uctout, ", ECMA 4/14.17.11.\n");
        nodePrintUnicodeNameContLine(node,mc);

        if(   adGetExtentSize(&efe->streamDirectoryICB) == 0
           && streamsObjectSize != 0 )      /* assert */
        { fprintf(uctout, "-\tProgram error: streamsObjectSize: ");
          printUint64(MLIMIT.vl, streamsObjectSize, FALSE, NULL);
          fprintf(uctout, ", should be 0, please report !!\n");
        }
      MLIMITend;
      return FALSE;
    }
    return TRUE;

}   /* end verifyEfeObjectSize() */

/* printEaNotAllowed
 * print error in case EAs are present, but not allowed,
 * Either in the (E)FE ExtendedAttributes field or in a
 * separate EA file pointed to by the extendedAttributeICB field.
 * if( isEANode ), then for an EA file,
 * else for a stream or stream directory.
 * Implementation note:
 *  1) Called from within MLIMITbegin/MLIMITend clause.
 *  2) pSize must point to the appropriate field in the descriptor
 *     pointed to by d.
 */
static void printEaNotAllowedError( Uint32  size,
                                    Byte   *fieldPointer,
                                    char   *fieldName,
                                    bool    isEANode,
                                    Byte   *d,
                                    Node   *node,
                                    UdfMountContext *mc )
{   char *txt, *ref;
    if( isEANode )
    { txt = "EA File";
      ref = "14/9.1, 4/14.6.6";     /* ECMA */
    }
    else
    { txt = "Stream or StreamDir";
      ref = "UDF 3.3.5.1";
    }
    printMessageHead( ERROR00level, d, fieldPointer, NULL);
    fprintf(uctout,
        "Error: %s: %lu, expected: 0 for\n"
        "-\t\t\t%s (E)FE, ECMA 4/14.9.19, %s.\n",
            fieldName, size, txt, ref);
    nodePrintUnicodeNameContLine(node,mc);
}

/* verifyFEorEFE():
 *
 * handles both FE and EFE using special FE_* macros.
 * FE  : File Entry.
 * EFE : Extended File Entry.
 *
 * Order of verify:
 * - FE and EFE common fields
 * - EFE-only fields
 * - FE/EFE context tests
 */
static bool verifyFEorEFE(Byte *d, UdfMountContext *mc, Node *node)
{
    ExtendedFileEntry *efe;
    Uint8   adType, fileType, osClass;
    Uint32  tmpU32, lenOfAds;
    Uint64  tmpU64, informationLength;
    Uint16  fileLinkCount, flcExp, osCI,
            tagId   = ((Tag *)d)->tagIdentifier;
    bool    isEANode, isStreamDirNode, isSystemStreamDirNode,
            isStream,
            result = TRUE;

#ifdef  UCT_TESTING
/**testing** (*(pFE_uid(d)))         = 0;
/**testing** (*(pFE_gid(d)))         = 0;
/**testing** (*(pFE_permissions(d))) = FE_PERM_RESERVED_MASK + 0x33;
/**testing** (*(pFE_recordFormat(d)))            = 44;
/**testing** (*(pFE_recordDisplayAttributes(d))) = 55;
/**testing** (*(pFE_recordLength(d)))            = 66;
/**testing** (*(pFE_logicalBlocksRecorded(d)))   = (Uint64) 0xfedcba9876543210;
/**testing** (*(pFE_checkpoint(d)))              = 0;
/**testing** (*(pFE_uniqueID(d)))                = (Uint64) 0x0000000100000000 + 15;
/**testing**/
#endif  /** UCT_TESTING **/

#ifdef  DEBUG01
    if( tagId == tidFE )
    {
        MLIMITbegin(DEBUG01level, MLIMITdefault01);     /* TODO: ?? */
            implementationNotReady( "File Entry" );
        MLIMITend;
    }
    else
    {
        MLIMITbegin(DEBUG01level, MLIMITdefault01);     /* TODO: ?? */
            implementationNotReady( "Extended File Entry" );
        MLIMITend;
    }
#endif  /* DEBUG01 */

    /* When a FE or EFE is expected,
     * then node and mc shall not be NULL !!
     * No assert, because it exits the verifier if a FE or EFE
     * shows up when node == NULL or mc == NULL
     * (e.g. encounter FE when expecting Volume Descriptor).
     */
    if( node == NULL || mc == NULL )
    {   MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead(MLIMIT.vl, d, NULL,
                    "Error: Unexpected descriptor type\n");
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        return FALSE;   /* so an error, no warning */
    }
    isSystemStreamDirNode = NODEFLAGS_IS_SYSTEMSTREAMDIR(node);
    isStreamDirNode       = NODEFLAGS_IS_SET(node, NFB_STREAMDIR);
    isStream              = NODEFLAGS_IS_SET(node, NFB_STREAM);
    isEANode              = NODEFLAGS_IS_SET(node, NFB_EA);

    UCTASSERT( isStreamDirNode || !isSystemStreamDirNode );

#ifdef  DEBUG01
    /* some debug aid
     */
    if( isEANode )
    {   ifVERBOSE(DEBUG01level)
        { nodePrintUnicodeNameTxtExtra(node,mc,
                        "DEBUG01: EA file: ", "\n");
        }
        ENDif;
    }
    if( isStreamDirNode )
    {   ifVERBOSE(DEBUG01level)
        { nodePrintUnicodeNameTxtExtra(node,mc,
                "DEBUG01: Stream Directory: ", "\n");
        }
        ENDif;
    }
#endif  /* DEBUG01 */

    /* For UDF 2.00+, an EFE should be used instead of a FE.
     */
    if(   tagId == tidFE
       && getUctMinUdfRevision() >= 0x200 )
    {   MLIMITbegin( WARN01level, MLIMITdefault02 );
          printMessageHead( MLIMIT.vl, d,
                        (Byte*)&((Tag *)d)->tagIdentifier,
                  "Warning: For UDF 2.00 and higher, an Extended File Entry\n"
            "-\t\t   should be used instead of a File Entry. However, a File\n"
            "-\t\t   Entry may be used for backward compatibility or if a\n"
            "-\t\t   medium was upgraded from a UDF revision lower than 2.00,\n");
          fprintf(uctout,
            "-\t\t   UDF %s.\n", (getUctMinUdfRevision() <= 0x260)
                            ? "3.3.5, 3.3.5.1. Clarification in DCN-5160"
                            : "2.3.6, 3.3.5, 3.3.5.1.");
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }

    adType = GET_ADTYPE(pFE_icbTag(d)->flags);
    fileType = pFE_icbTag(d)->fileType;
    lenOfAds          = (*(pFE_lengthOfAllocationDescriptors(d)));
    informationLength = (*(pFE_informationLength(d)));

    /* ICBTag icbTag
     */
    if( !verifyICBTag(pFE_icbTag(d), d, isStream, mc, node) )
    {   result = FALSE;
    }

    /* Uint32 uid and gid :
     * ECMA: SHOULD not be 0 for OSes that do not support the notion of
     *       UID and GID, but 0 is legal for UNIX, etc ... ??
     * so UDF defined:
     *  - #FFFFFFFF is the value for an 'undefined' uid/gid
     *  - OSes that do not have the UID/GID notion SHALL set UID/GID to #FFFFFFFF.
     * Note that for MAC OS 9 / MAC OS X, this depends on the OS Identifier
     * value, rather than the OS Class value only.
     *
     * Get OS Class and id from FE implementationIdentifier field, because
     * the scope of this entityID includes "the contents of the descriptors
     * that specify the contents and attributes of the file described by
     * this descriptor" as defined by ECMA 167 4/14.9.17.
     *
     * Violation is still a warning instead of an error, because I do not
     * exactly know which OSes really support UID/GID and which do not.
     * Known at the moment to support uid/gid:
     * UNIX, OS/400 and MAC OS X.
     */
    osClass = pFE_implementationIdentifier(d)->idSuffix.implSuffix.osClass;
    osCI = (osClass << 8)
         + pFE_implementationIdentifier(d)->idSuffix.implSuffix.osIdentifier;

    if(    osClass != OSCL_UNIX             /* no UNIX class */
        && osClass != OSCL_OS400            /* no OS/400 class */
        &&    osCI != OSCI_MACOSX           /* no MAC OS X   */
        && (   *(pFE_uid(d)) != 0xFFFFFFFF
            || *(pFE_gid(d)) != 0xFFFFFFFF) )
    {                                       /* no MAC OS X, ... */
      MLIMITbegin(WARN01level, MLIMITdefault05);
        printMessageHead(MLIMIT.vl, d, (Byte*) pFE_uid(d), NULL);
        fprintf(uctout,
                "Warning: Uid, Gid: #%X, #%X, expected: #FFFFFFFF for%s\n"
          "-\t\t \"%s\". Operating Systems that do not support the\n"
          "-\t\t notion of User Id and Group Id shall use #FFFFFFFF, unless otherwise\n"
          "-\t\t specified by the user. OS Class+Id extracted from %s Implementation\n"
          "-\t\t Identifier, ECMA 4/14.9.3+4, UDF 3.3.3.1+2, 2.1.5.3, 6.3.\n",
                *(pFE_uid(d)), *(pFE_gid(d)),
                (osClass != OSCL_MACOS) ? " OS Class" : "",
                OSCI_TEXT(osCI), tidTEXT4(tagId) );
        nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
      /** warning, no FALSE result **/
    }

    /* Uint32 permissions : reserved bits shall be 0
     */
    tmpU32 = (*(pFE_permissions(d)));
    if( (tmpU32 & FE_PERM_RESERVED_MASK) != 0 )
    {
        MLIMITbegin( ERROR00level, MLIMITdefault10 );
            printMessageHead( MLIMIT.vl,
                d, (Byte*) pFE_permissions(d), NULL );
            fprintf(uctout,
                "Error: Permissions: #%08lX."
                " Reserved bit set, ECMA 4/14.9.5\n",
                tmpU32 );
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /** error, but no FALSE result **/
    }

    /* Uint16 fileLinkCount, ECMA 4/14.9.6
     * test some special cases here
     * (see also npCheckFileLinkCounts()).
     * fixed values for File Link Count :
     * - streams and System Stream Directory   : 1
     * - other stream directories and EA files : 0
     * - all nodes with a FE and a FID: at least 1
     */
    fileLinkCount = (*(pFE_fileLinkCount(d)));
    flcExp = (Uint16)((isStream || isSystemStreamDirNode) ? 1 : 0);
    if(   (isEANode || isStream || isStreamDirNode)
       && fileLinkCount != flcExp )
    { MLIMITbegin( ERROR00level, uctMessageLimit );
        printMessageExpectedU32( MLIMIT.vl,
            d, (Byte*) pFE_fileLinkCount(d),
            "Error: File Link Count", "%lu", fileLinkCount,
                              flcExp, ", for\n");
        fprintf(uctout,
            "-\t\t\t%s, ECMA 4/14.9.6%s\n",
                (isEANode) ? "an Extended Attribute file"
              : (isStream) ? "a Named Stream"
              : (isSystemStreamDirNode)
                           ? "the System Stream Directory"
                           : "a Stream Directory",
                (isStream) ? ", UDF 3.3.5.1" : "");
        nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
    }
    else if( node->fid != NULL && fileLinkCount == 0 )
    { /* TODO: remove this test ?? All errors will be
       *       found in npCheckFileLinkCounts() ??
       */
      MLIMITbegin(ERROR00level, uctMessageLimit);
        printMessageHead( MLIMIT.vl,
                d, (Byte*) pFE_fileLinkCount(d), NULL );
        fprintf(uctout,
                "Error: File Link Count: %lu,"
                            " expected: at least 1,\n"
          "-\t\t\tECMA 4/14.9.6\n", fileLinkCount);
        nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
    }

    /* Uint8 recordFormat : shall be 0
     */
    if( (tmpU32 = (Uint32)(*(pFE_recordFormat(d)))) != 0 )
    {
        MLIMITbegin(ERROR00level, MLIMITdefault10);
            printMessageExpectedU32( MLIMIT.vl,
                d, (Byte*) pFE_recordFormat(d),
                "Error: Record Format", "%lu",
                tmpU32, 0, ", UDF 2.3.6.1\n" );
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /** error, but no FALSE result **/
    }

    /* Uint8 recordDisplayAttributes : shall be 0
     */
    if( (tmpU32 = (Uint32)(*(pFE_recordDisplayAttributes(d)))) != 0 )
    {
        MLIMITbegin( ERROR00level, MLIMITdefault10 );
          printMessageExpectedU32( MLIMIT.vl,
                d, (Byte*) pFE_recordDisplayAttributes(d),
                "Error: Record Display Attributes", "%lu",
                tmpU32, 0, ", UDF 2.3.6.2\n" );
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /** error, but no FALSE result **/
    }

    /* Uint32 recordLength : shall be 0
     */
    if( (tmpU32 = (*(pFE_recordLength(d)))) != 0 )
    {
        MLIMITbegin( ERROR00level, MLIMITdefault10 );
            printMessageExpectedU32( MLIMIT.vl,
                d, (Byte*) pFE_recordLength(d),
                "Error: Record Length", "%lu",
                tmpU32, 0, ", UDF 2.3.6.3\n");
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /** error, but no FALSE result **/
    }

    /* Uint64 informationLength :
     * checks below and in swapAndVerifyAllocationDescriptors()
     */
    if(    fileType == FT_DIRECTORY
        && (informationLength % 4) != 0 )
    {
        MLIMITbegin( ERROR00level, uctMessageLimit );
            printMessageHead( MLIMIT.vl, d,
                              (Byte*) pFE_informationLength(d),
                              "Error: Information Length: ");
            printUint64(MLIMIT.vl, informationLength, FALSE, NULL);
            fprintf(uctout, ", expected: multiple of 4 for\n"
              "-\t\t\tdirectory FE, because FID length shall be multiple\n"
              "-\t\t\tof 4, ECMA 4/14.4.9\n");
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /** error, but no FALSE result **/
    }

    /* Uint64 logicalBlocksRecorded : UDF 2.3.6.5
     * Mind Uint64 printing !!!
     * For files and directories with embedded data the value
     * of this field shall be ZERO.
     * For other files the number of RECORDED blocks has to
     * be counted, so no unrecorded extents ! This is done after
     * final AD testing in swapAndVerifyAllocationDescriptors().
     */
    tmpU64 = (*(pFE_logicalBlocksRecorded(d)));
    if( adType == ADT_INFE && tmpU64 != (Uint64) 0 )
    {
        MLIMITbegin( ERROR00level, MLIMITdefault10 );
            printMessageHead( MLIMIT.vl, d,
                (Byte*) pFE_logicalBlocksRecorded(d),
                "Error: Logical Blocks Recorded: ");
            printUint64(MLIMIT.vl, tmpU64, FALSE, NULL);
            fprintf(uctout, ", expected: 0 for\n"
                "-\t\t\ticbtag Allocation Descriptor type %u,\n"
                "-\t\t\tUDF 2.3.6.5, ECMA 4/14.9.11, 4/14.6.8.\n",
                adType);
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /** error, but no FALSE result **/
    }

    /* Timestamp checks
     */
    verifyTimestamp( pFE_accessTime(d),
                    (tagId == tidEFE) ? "ECMA 4/14.17.13"
                                      : "ECMA 4/14.9.12",
                     "FE Access Time", d, mc, node );
    verifyTimestamp( pFE_modificationTime(d),
                    (tagId == tidEFE) ? "ECMA 4/14.17.14"
                                      : "ECMA 4/14.9.13",
                     "FE Modification Time", d, mc, node);
    verifyTimestamp( pFE_attributeTime(d),
                    (tagId == tidEFE) ? "ECMA 4/14.17.16"
                                      : "ECMA 4/14.9.14",
                     "FE Attribute Time", d, mc, node);

    /* Uint32 checkpoint : at least one, ECMA 4/14.9.15.
     */
    if( (*pFE_checkpoint(d)) == 0 )
    { MLIMITbegin( ERROR00level, MLIMITdefault10 );
        printMessageHead( MLIMIT.vl, d,
                         (Byte*) pFE_checkpoint(d), NULL);
        fprintf(uctout,
          "Error: Checkpoint: 0, expected: at least 1, %s.\n",
                         (tagId == tidEFE) ? "ECMA 4/14.17.17"
                                           : "ECMA 4/14.9.15" );
        nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
    }

    /* LongAd extendedAttributeICB : 4/14.9.16 (4/14.17.18).
     */
    (void) verifyLongAd(pFE_extendedAttributeICB(d), d, mc,
                        TRUE,       /* isIntegralBlock   */
                        TRUE,       /* isFixedRecAndAlloc */
                        TRUE );     /* inMetadataPartition */

    tmpU32 = adGetExtentSize(pFE_extendedAttributeICB(d));
    if( tmpU32 != 0 && (isEANode || isStream || isStreamDirNode) )
    { MLIMITbegin( ERROR00level, MLIMITdefault10 );
        printEaNotAllowedError( tmpU32,
                    (Byte*) pFE_extendedAttributeICB(d),
                    "Extended Attribute ICB extent size",
                    isEANode, d, node, mc);
      MLIMITend;
    }

    /* EntityID implementationIdentifier : UDF 2.3.6.6
     */
    (void) verifyEntityID(pFE_implementationIdentifier(d),
                          ENTITY_SUFFIX_IMPL, NULL, NULL, (Byte*)d);

    /* Uint64 uniqueID : UDF 2.3.6.7, 3.2.1
     */
    (void) verifyFidOrFeUniqueID( d, node, mc);

    /* Uint32 lengthOfExtendedAttributes : 4/14.9.19 (4/14.17.22)
     *
     * Test lengthOfExtendedAttributes alignment again here.
     * It has been tested and maybe repaired in endianSwapFEorEFE().
     * If error still present than it will be assumed to be fatal.
     */
    tmpU32 = (*(pFE_lengthOfExtendedAttributes(d)));
    if( (tmpU32 % 4) != 0 )
    { MLIMITbegin(ERROR00level, MLIMITdefault10);
        printMessageHead( MLIMIT.vl, d,
            (Byte*) pFE_lengthOfExtendedAttributes(d), NULL);
        fprintf(uctout,
            "Fatal error: Length of Extended Attributes"
                                            " is no integral\n"
            "-\t\t\t      multiple of 4: %lu, ECMA 4/14.9.19\n",
                        tmpU32);
        nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
      return FALSE;     /* fatal error */
    }
    if( tmpU32 != 0 && (isEANode || isStream || isStreamDirNode) )
    { MLIMITbegin( ERROR00level, MLIMITdefault10 );
        printEaNotAllowedError( tmpU32,
                    (Byte*) pFE_lengthOfExtendedAttributes(d),
                    "Length of Extended Attributes",
                    isEANode, d, node, mc);
      MLIMITend;
    }

    /* Uint32 lengthOfAllocationDescriptors :
     * Verify later, together with AllocationDescriptors
     *
     * Before variable sized arrays, first extra checks
     * for 'EFE only' fields
     */
    if( tagId == tidEFE )
    {   efe = (ExtendedFileEntry *) d;

        /* Uint64 objectSize : ECMA 4/14.17.11
         * Test here for EFEs without a stream directory.
         * For EFEs with a stream directory, test in
         * expandDirectoryHierarchy().
         */
        if( adGetExtentSize(&efe->streamDirectoryICB) == 0 )
        {   /* no stream directory */
            (void) verifyEfeObjectSize( efe, (Uint64) 0,
                                        NULL, mc, node );
        }

        /* Timestamp creationTime : ECMA 4/14.17.15
         */
        verifyTimestamp(&efe->creationTime,"ECMA 4/14.17.15",
                                "EFE Creation Time", d,mc,node);

        /* compare EFE Access, Modify and Attribute Time to
         * Creation Time, mind UDF 3.3.4.3.1.
         * In DCN-5153 it was decided to change the 'later than creation time'
         * error into a warning (shall -> should) FOR ALL UDF revisions !!
         * TODO: For FE: Compare to Creation Time in File Times EA, if any.
         */
        verifyTimestampRelation(pFE_accessTime(d),  "Access Time",
                                &efe->creationTime, "Creation Time",
                                FALSE,          /* NOT isError (warning) */
                                "ECMA 4/14.9.12, UDF 3.3.4.3.1",
                                d, NULL, mc, node);

        verifyTimestampRelation(pFE_modificationTime(d), "Modification Time",
                                &efe->creationTime,      "Creation Time",
                                FALSE,          /* NOT isError (warning) */
                                "ECMA 4/14.9.13, UDF 3.3.4.3.1",
                                d, NULL, mc, node);

        verifyTimestampRelation(pFE_attributeTime(d), "Attribute Time",
                                &efe->creationTime,   "Creation Time",
                                FALSE,          /* NOT isError (warning) */
                                "ECMA 4/14.9.14, UDF 3.3.4.3.1",
                                d, NULL, mc, node);

        /* Byte reserved[4] :
         */
        if(    efe->reserved[0] != 0 || efe->reserved[1] != 0
            || efe->reserved[2] != 0 || efe->reserved[3] != 0 )
        {
            MLIMITbegin( ERROR00level, MLIMITdefault10 );
              printMessageHead( MLIMIT.vl, d, efe->reserved, NULL);
              fprintf(uctout,
                "Error: Non-zero Reserved byte: "
                    " #%02X #%02X #%02X #%02X, ECMA 4/14.17\n",
                    efe->reserved[0], efe->reserved[1],
                    efe->reserved[2], efe->reserved[3]);
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
            /** error, but no FALSE return result **/
        }

        /* LongAd streamDirectoryICB    :
         */
        if( !verifyLongAd( &efe->streamDirectoryICB, (Byte *)d, mc,
                            TRUE,       /* isIntegralBlock   */
                            TRUE,       /* isFixedRecAndAlloc */
                            TRUE ) )    /* inMetadataPartition */
        {
            result = FALSE;
        }
    }

    /* Extended Attributes (EAs), variable sized array.
     * Byte extendedAttributes[lengthOfExtendedAttributes];
     * and:
     * AllocationDescriptors (ADs), variable array.
     * Byte allocationdescriptors[lengthOfAllocationDescriptors];
     *
     * EAs and ADs are not verified here, but in readFileEntryEtc(),
     * because in case of strategy 4096 only the EAs and ADs of the
     * last FE in the 4096 chain must be verified.
     * Further we do not want to mix AD and EA output lines.
     */

    /* Verify Uint32 lengthOfAllocationDescriptors
     *    and Byte   AllocationDescriptors[lengthOfAllocationDescriptors]
     * Mind that informationLength is Uint64 !!
     */
    if( adType == ADT_INFE && ((Uint64) lenOfAds) != informationLength )
    {
        /** TODO: no FALSE return result ?? */
        MLIMITbegin( ERROR00level, MLIMITdefault10 );
            printMessageHead( MLIMIT.vl, d,
                (Byte*) pFE_lengthOfAllocationDescriptors(d), NULL);
            fprintf(uctout,
                      "Error: Length Of Allocation Descriptors: %lu (#%lX),\n"
                "-\t\t expected: equal to Information Length  : ",
                                                        lenOfAds, lenOfAds);
            printUint64(MLIMIT.vl, informationLength, FALSE, NULL);
            fprintf(uctout, ", because\n"
                "-\t\t icbtag Allocation Descriptor type %u,"
                                " ECMA 4/8.8.2, 4/14.6.8\n", adType );
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }

    return result;

}   /* end verifyFEorEFE() */


static bool verifyUnallocatedSpaceEntry(UnallocatedSpaceEntry *d,
                                        UdfMountContext       *mc)
{
    Uint8  adType, fileType;
    bool   result = TRUE;

    if( !verifyICBTag( &d->icbTag, (Byte*) d,
                        FALSE,          /* isStream */
                        mc, NULL) )     /* NULL node */
    {   result = FALSE;      /* fatal ?? */
    }
    fileType = d->icbTag.fileType;
    adType = GET_ADTYPE(d->icbTag.flags);
    if( fileType != FT_UNALLOCATEDSPACE )
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
            printMessageHead(MLIMIT.vl, (Byte*) d,
                             (Byte*) &d->icbTag.fileType, NULL);
            fprintf(uctout,
                "Error: File Type: %u, expected: %u\n"
                "-\t\t for Unallocated Space Entry,"
                            " ECMA 4/14.11.2, 4/14.6.6\n",
                    fileType, FT_UNALLOCATEDSPACE);
        MLIMITend;
        /* no FALSE return result */
    }

    /* UDF 2.3.7.1 requires only ShortAd's here, but in case of
     * a violation we may be prepared to handle LongAd's as well.
     * TODO: tobe tested
     */
    if( adType != ADT_SHORT )
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
            printMessageHead(MLIMIT.vl, (Byte*) d,
                             (Byte*) &d->icbTag.flags, NULL);
            fprintf(uctout,
                "Error: Allocation Descriptor type: %u,"
                                " expected: %u (short_ad)\n"
                "-\t\t for Unallocated Space Entry,"
                                " UDF 2.3.7.1, ECMA 4/14.6.8\n",
                    adType, ADT_SHORT);
        MLIMITend;
        if( adType != ADT_LONG )
        { return FALSE;             /* fatal */
        }
        /* no FALSE return result for ADT_LONG */
    }

    /* ADs verified later in swapAndVerifyAllocationDescriptors()
     * and verifyUnallocatedSpaceAdsOrder()
     */
    return result;

}   /* end verifyUnallocatedSpaceEntry() */

/* ECMA 4/14.12, UDF 2.3.8
 * NumberOfBits / partition size
 * tested in readSpaceTableOrBitmap().
 */
static bool verifySpaceBitmapDescriptor(SpaceBitmapDescriptor *d)
{
    Uint32 nmbOfBitBytes = ROUNDUPELEMENTS(d->numberOfBits, 8),
           nmbOfBytesT8  = 8 * d->numberOfBytes;

    /* error if Numberofbytes insufficient
     */
    if( d->numberOfBytes < nmbOfBitBytes )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        printMessageHead( MLIMIT.vl, (Byte*) d,
                          (Byte*) &d->numberOfBytes, NULL );
        fprintf(uctout,
           "Error: Space Bitmap NumberOfBits, NumberOfBytes: %lu, %lu\n"
            "-\t\t Expected for NumberOfBytes: %lu,"
                                " ECMA 4/14.12, UDF 2.3.8\n",
                    d->numberOfBits, d->numberOfBytes, nmbOfBitBytes);
      MLIMITend;
        /* no FALSE return, verifyPartitionAllocation() can handle this */
    }
    else if( nmbOfBytesT8 > d->numberOfBits )     /* note: room for  */
    { Uint32 dltBits = nmbOfBytesT8 - d->numberOfBits;  /* more bits */
      MLIMITbegin(INFO01level, uctMessageLimit);
        printMessageHead( MLIMIT.vl, (Byte*) d,
                          (Byte*) &d->numberOfBytes, NULL );
        fprintf(uctout,
          "Note: Number of Bytes: %lu (room for %lu more bit%s)\n",
                d->numberOfBytes, dltBits, PLURAL_S(dltBits) );
      MLIMITend;
    }

    return TRUE;

}   /* end verifySpaceBitmapDescriptor() */

/* verifyDescriptorTail():
 *
 * Precondition:
 *  It is assumed that the descriptor tag is checked (specially
 *  the CRC check) and that the whole descriptor is read and
 *  swapped to the correct endian.
 *
 * verifyDescriptorTail() will swap the part following the tag.
 */
static bool verifyDescriptorTail(Byte *bf,
                                 UdfMountContext *mc, Node *node)
{
    Uint16 spPacketLength,
           tagId  = ((Tag*)bf)->tagIdentifier;
    bool   result = TRUE;

    switch( tagId )
    {
    case tidST:                 /* special UDF descriptor */
        spPacketLength = getSparingPacketLength(mc);
        result = (spPacketLength > 0)
                    ? verifySparingTable((SparingTable*) bf,
                                         spPacketLength)
                    : FALSE;
        break;
                                /* group 1, Part 3 descriptors */
    case tidPVD:
        result = verifyPrimaryVolumeDescriptor((PrimaryVolumeDescriptor *) bf, mc);
        break;
    case tidAVDP:
        result = verifyAnchorVolumeDescriptorPointer((AnchorVolumeDescriptorPointer *) bf);
        break;
    case tidVDP:
        result = verifyVolumeDescriptorPointer((VolumeDescriptorPointer *) bf);
        break;
    case tidIUVD:
        result = verifyImplementationUseVolumeDescriptor(
                                    (ImplementationUseVolumeDescriptor *) bf);
        break;
    case tidPD:
        result = verifyPartitionDescriptor((PartitionDescriptor *) bf);
        break;
    case tidLVD:
        result = verifyLogicalVolumeDescriptor((LogicalVolumeDescriptor *) bf);
        break;
    case tidUSD:
        result = verifyUnallocatedSpaceDescriptor((UnallocatedSpaceDescriptor *) bf);
        break;
    case tidTD:         /* special, used in part 4 too */
        result = verifyTerminatingDescriptor((TerminatingDescriptor *) bf);
        break;
    case tidLVID:
        result = verifyLogicalVolumeIntegrityDescriptor(
                                (LogicalVolumeIntegrityDescriptor *) bf, mc);
        break;
                                        /* group 2, Part 4 descriptors */
    case tidFSD:
        result = verifyFileSetDescriptor((FileSetDescriptor *) bf, mc);
        break;
    case tidFID:
        result = verifyFileIdentifierDescriptor((FileIdentifierDescriptor *) bf,
                                                mc, node);
        break;
    case tidAED:
        result = verifyAllocationExtentDescriptor((AllocationExtentDescriptor *) bf,
                                                  mc, node);
        break;
    case tidIE:
        result = verifyIndirectEntry((IndirectEntry *) bf, mc,node);
        break;
    case tidTE:
        result = verifyTerminalEntry((TerminalEntry *) bf, mc, node);
        break;
    case tidFE:         /* fall through, verifyFEorEFE */
    case tidEFE:    /* handles both FE and EFE */
        result = verifyFEorEFE((Byte *) bf, mc, node);
        break;
    case tidEAHD:
        result = verifyExtendedAttributeHeaderDescriptorTail();
        break;
    case tidUSE:
        result = verifyUnallocatedSpaceEntry((UnallocatedSpaceEntry *) bf, mc);
        break;
    case tidSBD:
        result = verifySpaceBitmapDescriptor((SpaceBitmapDescriptor *) bf);
        break;
    default:
        result = FALSE;
        break;
    }

    /* Not all errorrs set result = FALSE, because it is
     * assumed that continue verifying is usefull.
     */
    return result;

}   /* verifyDescriptorTail */

/* getLengthOfDescriptor():
 * Determine total length of a descriptor (fixed + variable part).
 * Argument isSwapped notifies if endian swap has been done.
 *
 * ECMA, ...       - descriptor layouts.
 * UDF 2.50 2.2.12 - Sparing Table  (was 2.2.11)
 * UDF 2.50 5.1    - overview
 *
 * Exception:
 *  For the AllocationExtentDescriptor, the
 *  Byte AllocationDescriptors[LengthOfAllocationDescriptors]
 *  field is handled as part of the AllocationExtentDescriptor,
 *  while it is no part of the descriptor itself, according to
 *  ECMA and UDF. According to ECMA, the descriptor size is 24.
 *
 * IMPLEMENTATION NOTE:
 *  Be aware that not all fancy FE/EFE pFE_...((Byte*)d) macros can be
 *  used if the tag is not endian swapped yet, i.e. in those cases that
 *  inside the macro, the (unswapped) descriptorTag.tagIdentifier is
 *  used to distinguish between FE and EFE.
 */
extern bool getLengthOfDescriptor(Byte *d, bool isSwapped, Uint32 *dLen)
{
    Uint16 swapTagId;

    /* static asserts executed once in verifyStructAssert();
     */

    swapTagId = ((Tag*)d)->tagIdentifier;
    if( !isSwapped ) endianSwap((Byte*)&swapTagId, sizeof(Uint16),1,NULL);

    switch( swapTagId )
    {
    case tidST:
        {
          Uint16 swapTableLen = ((SparingTable*)d)->reallocationTableLength;
          if( !isSwapped )
              endianSwap((Byte*)&swapTableLen, sizeof(Uint16),1,NULL);
          *dLen = offsetof(SparingTable, startOfMapEntries)
                  + swapTableLen * sizeof(SparingEntry);
        }
        break;
    case tidPVD:
        *dLen = sizeof(PrimaryVolumeDescriptor);
        break;
    case tidAVDP:
        *dLen = sizeof(AnchorVolumeDescriptorPointer);
        break;
    case tidVDP:
        *dLen = sizeof(VolumeDescriptorPointer);
        break;
    case tidIUVD:
        *dLen = sizeof(ImplementationUseVolumeDescriptor);
        break;
    case tidPD:
        *dLen = sizeof(PartitionDescriptor);
        break;
    case tidLVD:        /* variable length, no max */
        {
          Uint32 swapTableLen = ((LogicalVolumeDescriptor*)d)->mapTableLength;
          if( !isSwapped )
              endianSwap((Byte*)&swapTableLen, sizeof(Uint32),1,NULL);
          *dLen = offsetof(LogicalVolumeDescriptor, startOfPartitionMaps)
                  + swapTableLen;
        }
        break;
    case tidUSD:        /* variable length, no max */
        {
          Uint32 swapNmbAD =
            ((UnallocatedSpaceDescriptor*)d)->numberOfAllocationDescriptors;
          if( !isSwapped )
              endianSwap((Byte*)&swapNmbAD, sizeof(Uint32),1,NULL);
          *dLen = offsetof(UnallocatedSpaceDescriptor, startOfAllocationDescriptors)
                  + swapNmbAD * sizeof(ExtentAd);
          break;
        }
    case tidTD:
        *dLen = sizeof(TerminatingDescriptor);
        break;
    case tidLVID:       /* variable length, no max */
        {
          Uint32 swapNmbPar =
            ((LogicalVolumeIntegrityDescriptor*)d)->numberOfPartitions;
          Uint32 swapLenIU =
            ((LogicalVolumeIntegrityDescriptor*)d)->lengthOfImplementationUse;
          if( !isSwapped )
          {
              endianSwap((Byte*)&swapNmbPar, sizeof(Uint32),1,NULL);
              endianSwap((Byte*)&swapLenIU, sizeof(Uint32),1,NULL);
          }
          *dLen = offsetof(LogicalVolumeIntegrityDescriptor, startOfTables)
                  + swapNmbPar * 2 * sizeof(Uint32)
                  + swapLenIU;
        }
        break;
    case tidFSD:
        *dLen = sizeof(FileSetDescriptor);
        break;
    case tidFID:    /* variable length, max: blockSize */
        {
          Uint16 swapLenIU =
            ((FileIdentifierDescriptor*)d)->lengthOfImplementationUse;
          if( !isSwapped )
              endianSwap((Byte*)&swapLenIU, sizeof(Uint16),1,NULL);
          /* note that lengthOfFileIdentifier is Uint8, so no endian swap
           */
          *dLen = ROUNDUPMULT(
                  offsetof(FileIdentifierDescriptor, startOfImplementationUse)
                  + ((FileIdentifierDescriptor*)d)->lengthOfFileIdentifier
                  + swapLenIU, 4);
        }
        break;
    case tidAED:
        /* Note: See exception for AllocationExtentDescriptor
         *       in function explanation above.
         */
        {
          Uint32 swapLenAD =
            ((AllocationExtentDescriptor*)d)->lengthOfAllocationDescriptors;
          if( !isSwapped )
              endianSwap((Byte*)&swapLenAD, sizeof(Uint32),1,NULL);
/** ====  *dLen = sizeof(AllocationExtentDescriptor);   ** according to ECMA **/
          *dLen = offsetof(AllocationExtentDescriptor, startOfAllocationDescriptors);
          /* exception, add  Allocation Descriptors !!!
           */
          *dLen += swapLenAD;
        }
        break;
    case tidIE:
        *dLen = sizeof(IndirectEntry);
        break;
    case tidTE:
        *dLen = sizeof(TerminalEntry);
        break;

    case tidFE:         /* variable length, max: blockSize */
        /* handle File Entry and Extended File Entry separately,
         * because not all fancy "pFE_...(d)" macros can be used,
         * see IMPLEMENTATION NOTE above.
         */
        { FileEntry *fe = (FileEntry *) d;
          Uint32 swapLenEA = fe->lengthOfExtendedAttributes;
          Uint32 swapLenAD = fe->lengthOfAllocationDescriptors;
          Uint32 offset;
          if( !isSwapped )
          {   endianSwap((Byte*)&swapLenEA, sizeof(Uint32),1,NULL);
              endianSwap((Byte*)&swapLenAD, sizeof(Uint32),1,NULL);
          }
          offset = offsetof(FileEntry, startOfExtendedAttributes);
          *dLen = offset + swapLenEA + swapLenAD;
        }
        break;
    case tidEFE:    /* variable length, max: blockSize */
        /* handle File Entry and Extended File Entry separately,
         * because not all fancy "pFE_...(d)" macros can be used,
         * see IMPLEMENTATION NOTE above.
         */
        { ExtendedFileEntry *efe = (ExtendedFileEntry *) d;
          Uint32 swapLenEA = efe->lengthOfExtendedAttributes;
          Uint32 swapLenAD = efe->lengthOfAllocationDescriptors;
          Uint32 offset;
          if( !isSwapped )
          {   endianSwap((Byte*)&swapLenEA, sizeof(Uint32),1,NULL);
              endianSwap((Byte*)&swapLenAD, sizeof(Uint32),1,NULL);
          }
          offset = offsetof(ExtendedFileEntry, startOfExtendedAttributes);
          *dLen = offset + swapLenEA + swapLenAD;
        }
        break;
    case tidEAHD:
        *dLen = sizeof(ExtendedAttributeHeaderDescriptor);
        break;
    case tidUSE:            /* variable length, max: blockSize */
        { Uint32 swapLenAD =
            ((UnallocatedSpaceEntry*)d)->lengthOfAllocationDescriptors;
          if( !isSwapped )
              endianSwap((Byte*)&swapLenAD, sizeof(Uint32),1,NULL);
          *dLen = offsetof(UnallocatedSpaceEntry, startOfAllocationDescriptors)
                  + swapLenAD;
          break;
        }
    case tidSBD:            /* variable length, no max */
        { Uint32 swapNmbBytes = ((SpaceBitmapDescriptor*)d)->numberOfBytes;
          if( !isSwapped )
              endianSwap((Byte*)&swapNmbBytes, sizeof(Uint32),1,NULL);
          *dLen = offsetof(SpaceBitmapDescriptor, startOfBitmap)
                  + swapNmbBytes;
        }
        break;
    default:
        MLIMITbegin(ERROR00level, uctMessageLimit); /* dummy */
        MLIMITend;
        VERBOSE00(uctout,
            "\ngetLengthOfDescriptor program error:"
                " Unknown descriptor tagIdentifier: %u, please report\n",
                        swapTagId);
        return FALSE;
        /** break; **/
    }

    return TRUE;

}   /* end getLengthOfDescriptor() */


/* Determine if tagId is an ICB Direct Entry
 * (FE, EFE, USE, PIE or TE).
 * (PIE not used by UDF)
 */
extern bool isIcbDirectEntry(Uint16 tagId)
{
    return(    tagId == tidFE
            || tagId == tidEFE
            || tagId == tidUSE
            || tagId == tidTE );
}

/* inspectDescriptorHead():
 * Checks if buffer contains the head of a valid UDF descriptor.
 * The head means: that part of the descriptor, enough to determine
 * the length of the descriptor, so at least the descriptor tag.
 *
 * In case expectedTagId is tidUNKNOWN, an extra test of a Sparing
 * Table Sparing Identifier is done because often the tag checksum is
 * ok, while it appears to be no ST after all
 * (-inspect_image finds too many ST).
 *
 * It is assumed that NO endian swap has been performed on
 * the buffer contents yet and this function will remain the
 * buffer contents unchanged.
 * inspectDescriptorHead() operates silently, error messages are
 * stored in uctErrorMessage using sprintf.
 * Therefore uctErrorMessage will be cleared as first action !!
 *
 * A blank tag (all #00 bytes) will never be accepted as a valid
 * descriptor head.
 * Further only the tagIdentifier and tagChecksum fields will
 * be inspected in order to recognize a valid descriptor head.
 *
 * If (expectedTagId == tidUNKNOWN), any valid descriptor will
 * be accepted, else only a descriptor with <tag id> equal to
 * expectedTagId will be accepted.
 *  Exception: If (expectedTagId == tidICBDIRECTENTRY), any
 *      <tag id> returning TRUE for isIcbDirectEntry(<tag id>)
 *      will be accepted.
 *
 * Function return value:
 *  if buffer contains a valid and accepted UDF descriptor head,
 *  then: TRUE
 *  else: FALSE.
 *
 * Other exported results (only if their pointer != NULL)
 *  *pExportTagId : if valid tag found then: tag id
 *                                     else: tidUNKNOWN
 *  *pDescriptorLength : descriptor length
 */
extern bool inspectDescriptorHead(Byte   *buffer,
                                  Uint32  bufferLength,
                                  Uint32  blockSize,
                                  bool    checksumIsFatal,
                                  Uint16  expectedTagId,
                                  Uint16 *pExportTagId,
                                  Uint32 *pDescriptorLength)
{
    Tag     *dTag = (Tag *) buffer;
    char    *dTxt4, *expTxt;
    Uint16   swapTagId;     /* endian swapped */
    Uint8    chsum;

    clearUctErrorMessage(); /* first action in inspectDescriptorHead() */

    /* export tidUNKNOWN as long as it is not sure
     * that a valid tag is recognized.
     */
    if( pExportTagId      != NULL ) *pExportTagId = tidUNKNOWN;
    if( pDescriptorLength != NULL ) *pDescriptorLength = 0;

    if( bufferLength < sizeof(Tag) )
    {
        sprintf(uctErrorMessage,
               "inspectDescriptor error: buffer length: %lu,\n"
            "-\t\tless than tag size, please report.\n", bufferLength);
        return FALSE;
    }
    expTxt =  (expectedTagId == tidUNKNOWN) ? ""
            : (expectedTagId == tidICBDIRECTENTRY)
            ? "<ICB Direct Entry>"
            :  tidTEXT4(expectedTagId);

    /* first check for blank sector, or blank Tag
     * these checks are endianSwap independent
     */
    if(    bufferLength >= blockSize
        && verifyZeros(buffer, blockSize, NULL, NULL, NULL) )
    {   sprintf(uctErrorMessage,
            "Error: Blank block, expected: %s descriptor", expTxt);
        return FALSE;
    }
    if( verifyZeros(buffer, sizeof(Tag), NULL, NULL, NULL) )
    {
        sprintf(uctErrorMessage,
            "Error: Blank descriptor tag, expected: %s descriptor", expTxt);
        return FALSE;
    }

    /* inspect tagIdentifier, endian swap
     */
    swapTagId = dTag->tagIdentifier;
    endianSwap((Byte*) &swapTagId, sizeof(Uint16), 1, NULL);

    dTxt4 = tidTEXT4(swapTagId);

    switch( expectedTagId )
    {
    case tidUNKNOWN:                    /* always ok */
        break;
    case tidICBDIRECTENTRY:             /* test if ICD Direct Entry */
        if( !isIcbDirectEntry(swapTagId) )
        { sprintf(uctErrorMessage,
            "Error: Unexpected descriptor tag id: %u (%s), expected: %s",
                    swapTagId, dTxt4, expTxt);
          return FALSE;
        }
        break;
    default:                            /* test if expectedTagId */
        if( swapTagId != expectedTagId )
        { sprintf(uctErrorMessage,
            "Error: Unexpected descriptor tag id: %u (%s), expected: %u (%s)",
                    swapTagId, dTxt4, expectedTagId, expTxt);
          return FALSE;
        }
        break;
    }

    if( (Int16) swapTagId < tidGroup01Start     /* (Int16) to keep compiler happy */
            || (swapTagId > tidGroup01End && swapTagId < tidGroup02Start)
            ||  swapTagId > tidGroup02End
            ||  swapTagId == tidPIE )           /* illegal for UDF */
    {
        sprintf(uctErrorMessage,
            "Error: %s descriptor tag identifier: %u",
                (swapTagId == tidPIE) ? "Illegal" : "Unknown",
                 swapTagId );
        return FALSE;
    }

    /* inspect tagChecksum, endian swap independent
     */
    if( dTag->tagChecksum != (chsum= calculateTagChecksum(dTag)) )
    {
        sprintf(uctErrorMessage,
            "%-4s Error: Tag Checksum: #%02X, expected: #%02X",
                    dTxt4, dTag->tagChecksum, chsum);
        if( checksumIsFatal )
        {   return FALSE;
        }
    }

    /* If expectedTagId is tidUNKNOWN, an extra test for
     * SparingTable is done: Test sparingIdentifier.Identifier.
     * This test is endian swap independent.
     */
    if(   expectedTagId == tidUNKNOWN
       &&     swapTagId == tidST )
    {   SparingTable *st = (SparingTable*)buffer;
        if( memcmp(st->sparingIdentifier.Identifier,
                   SPARING_TABLE_ID, ENTITYID_IDSIZE) != 0 )
        { sprintf(uctErrorMessage,
            "Error: Sparing Table unknown Identifier, expected: %s",
                            SPARING_TABLE_ID);
          return FALSE;
        }
    }

    /* From here, we assume that the tag itself is valid,
     * so export tag id and get descriptor length.
     */
    if( pExportTagId != NULL ) *pExportTagId = swapTagId;
    if(     pDescriptorLength != NULL
        && !getLengthOfDescriptor(buffer, FALSE, pDescriptorLength) )
    {   return FALSE;   /* could not determine length */
    }
    return TRUE;        /* valid descriptor head found */

}   /* end inspectDescriptorHead() */


/* Call printAedCRCLenMessage() from within
 * a MLIMITbegin/MLIMITend clause,
 * in order to have a proper (max) message
 * count for warnings and error separately.
 */
static void printAedCRCLenMessage( char   *ewTxt,
                                   Byte   *buffer,
                                   Uint16 *pDescriptorCRCLength,
                                   Uint32  expectedCRCLength,
                                   UdfMountContext *mc, Node *node )
{ char *format =
          "%s AED Descriptor CRC Length: %lu, expected:\n"
    "-\t\t   for UDF 2.00 and lower: 8, for UDF 2.01 and higher it is\n"
    "-\t\t   recommended to include the allocation descriptors after\n"
    "-\t\t   the AED descriptor in the AED CRC Length. Here this\n"
    "-\t\t   recommended value would be: %lu. A value of 8 is still\n"
    "-\t\t   allowed for backward compatibility, UDF 2.01+ 2.3.11.1.\n";

  printMessageHead( VERBOSE00level, buffer,
                    (Byte*)pDescriptorCRCLength, NULL );
  fprintf(uctout, format, ewTxt, (*pDescriptorCRCLength),
          expectedCRCLength);
  nodePrintUnicodeNameContLine(node,mc);
}

/* swapAndVerifyDescriptor():
 *
 * Checks if buffer contains a valid UDF descriptor.
 * Performs endian swap.
 *
 * expectedTagId determines which descriptors are accepted
 * and which are not, for details see inspectDescriptorHead().
 *
 * Function return value:
 *  if buffer contains a valid and accepted UDF descriptor
 *  then: TRUE
 *  else: FALSE.
 *
 * Return value in *pExportTagId only if pExportTagId != NULL:
 *      if valid tag found  then: found tag id
 *                          else: tidUNKNOWN
 * Important NOTE:
 *  If FALSE is returned, at least the descriptor tag will
 *  be endian swapped, but further buffer information is undefined,
 *  because of possible partial endian swaps.
 *
 * numberOfBytesRead shall be :
 *  either: an integral number of blocks, at least one
 *      or: exactly the descriptor size
 */
extern bool swapAndVerifyDescriptor( Byte   *buffer,
                                     Uint32  numberOfBytesRead,
                                     Uint16  expectedTagId,
                                     Uint16 *pExportTagId,
                                     Uint32  expectedTagLocation,
                                     UdfMountContext *mc, Node *node )
{
    static bool       isFirstTagSN = TRUE;
    static Uint16     theTagSerialNumber;

    const MediumInfo *vmi = getTheMediumInfo();
    Uint32            blockSize = vmi->blockSize;
    Tag              *dTag = (Tag *) buffer;
    char             *dTxt4;
    Uint8             chsum, vLevel;
    Uint16            tagId;
    Uint16            dCRC;
    Uint16            expectedCRCLen16;
    Uint32            expectedCRCLen32;
    Uint32            inspectDSize;
    bool              result = TRUE;

    /* Order of endian swap and checks:
     * - inspect descriptor tag
     * - endian swap of descriptor tag
     *   (also if inspect tag raised error)
     * - check descriptor tag
     * - check Descriptor CRC
     * If no fatal error so far, then:
     * - swap  Descriptor tail (after tag)
     * - check Descriptor tail (after tag)
     */
    result = inspectDescriptorHead(buffer, numberOfBytesRead,
                blockSize,
                (expectedTagId == tidUNKNOWN), /* checksumIsFatal */
                expectedTagId, pExportTagId, &inspectDSize);

    /* Tag endian swap,
     * also in case of inspectDescriptorHead() failure.
     */
    endianSwapDescriptorTag(dTag);

    /* Now in case of FALSE result so far, print
     * inspectDescriptorHead() style message and quit
     */
    if( result == FALSE )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        printAndClearUctErrorMessage("");   /* error: ... */
        nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
      return FALSE;
    }
    tagId = dTag->tagIdentifier;
    dTxt4 = tidTEXT4(tagId);

    /* Valid and swapped tag now, verify it
     * Log oneliner with shorthand descriptor name,
     * normally on VERBOSE00level, but for very
     * frequently occurring descriptors on INFO01level.
     * If node is defined, then log node name as last.
     * Other additions:
     *  - (E)FE: file type
     *  - all VDS descr: VDS Number
     *  - PD   : Pnmb
     *  - USD  : nmb of ADs
     *  - LVID : Open/Close
     * Mind that no endianswap has been done yet for
     * fields outside the descriptor Tag.
     */
    switch( tagId )
    {
    case tidFE:
    case tidEFE:
    case tidFID:
    case tidIE:
    case tidTE:
    case tidEAHD:
        vLevel = INFO01level;   /* for 'frequent' descr */
        break;
    default:
        vLevel = VERBOSE00level;    /* default */
        break;
    }
    ifVERBOSE(vLevel)   /* head oneliner log */
    {   fprintf(uctout, "\t%-4s", dTxt4);
    }
    ENDif;

    /* tag id dependent extra's
     * Mind that endian swap is done for descriptor tag only
     */
    ifVERBOSE(vLevel)   /* use fprintf() inside switch() */
    { switch( tagId )
      { Uint32 swapVDSN;
      case tidAVDP:     /* "AVDP at <xxx>   (MVDS: <yyy>, RVDS: <zzz>)" */
        { AnchorVolumeDescriptorPointer *avdp;
          Uint32 mainVDSloc, resVDSloc ;
          fprintf(uctout, " at ");
          printAvdpLocationText(expectedTagLocation, NULL);
          avdp = (AnchorVolumeDescriptorPointer*) buffer;
          mainVDSloc = avdp->mainVolumeDescriptorSequenceExtent.extentLocation;
          resVDSloc = avdp->reserveVolumeDescriptorSequenceExtent.extentLocation;
          endianSwap((Byte*) &mainVDSloc, sizeof(Uint32), 1, NULL);
          endianSwap((Byte*) &resVDSloc, sizeof(Uint32), 1, NULL);
          fprintf(uctout, "\t(MVDS: %lu, RVDS: %lu)", mainVDSloc, resVDSloc);
        }
        break;
      case tidFE:               /*  file type for FE and EFE */
      case tidEFE:
        { Uint64 swapUnID64 = *(pFE_uniqueID(buffer));
          endianSwap((Byte*)&swapUnID64, sizeof(Uint64), 1, NULL);
          fprintf(uctout,
            " file type %-4s  UniqueID: ",
                FT_TEXT4(pFE_icbTag(buffer)->fileType));
          printHexUniqueId17Chars(swapUnID64, FALSE);
        }
        break;
      case tidFID:
        { FileIdentifierDescriptor *fid;
          Uint16 swapLenOfImpl;
          fid = (FileIdentifierDescriptor*) buffer;
          if(getUctMinUdfRevision() >= 0x200 )
          { Uint32 swap32 = fid->ICB.implementationUse.ImpUse.UDFUniqueID;
            endianSwap((Byte*)&swap32, sizeof(Uint32), 1, NULL );
            fprintf(uctout, " UniqueID: #%08X", swap32);
          }
          swapLenOfImpl = fid->lengthOfImplementationUse;
          endianSwap((Byte *)&swapLenOfImpl, sizeof(swapLenOfImpl),
                     1, NULL);
          fprintf(uctout, "  cid: ");
          if( fid->lengthOfFileIdentifier == 0 )
               fprintf(uctout, "%3s", "");
          else fprintf(uctout, "%3d",
                    *((&fid->startOfImplementationUse)+swapLenOfImpl));
        }
        break;
      case tidLVID:
        { LogicalVolumeIntegrityDescriptor *lvid =
                (LogicalVolumeIntegrityDescriptor *) buffer;
          Uint32 swapIntegrityType = lvid->integrityType;
          endianSwap((Byte *)&swapIntegrityType, sizeof(swapIntegrityType),
                     1, NULL);
          fprintf(uctout, "  - %s",
              (swapIntegrityType == LVIDINTEGRITY_OPEN) ? "Open"
            : (swapIntegrityType == LVIDINTEGRITY_CLOSE) ? "Close"
            : "illegal Integrity Type");
          if( swapIntegrityType > LVIDINTEGRITY_MAX )
          { fprintf(uctout, ": %u", swapIntegrityType);
          }
        }
        break;
      case tidFSD:
        { FileSetDescriptor *fsd = (FileSetDescriptor *) buffer;
          Uint32 swapFSN  = fsd->fileSetNumber,
                 swapFSDN = fsd->fileSetDescriptorNumber;
          endianSwap((Byte *)&swapFSN, sizeof(swapFSN), 1, NULL);
          endianSwap((Byte *)&swapFSDN, sizeof(swapFSDN), 1, NULL);
          fprintf(uctout, "  FSN: %lu,  FSDN: %lu", swapFSN, swapFSDN);
        }
        break;
      case tidPVD:          /*  VDS Number for VDS descr */
      case tidLVD:
      case tidPD:
      case tidUSD:
      case tidIUVD:
      case tidVDP:
        /* Log Volume Descriptor Sequence Number for VDS descriptors.
         * These descriptors have this Unit32 field directly after
         * the Tag, but it has not been endianswapped yet.
         * For tidUSD, also log the nmb of ADs.
         */
        swapVDSN = *((Uint32*)(buffer + sizeof(Tag)));
        endianSwap((Byte*) &swapVDSN, sizeof(swapVDSN), 1, NULL);
        fprintf(uctout, "  VDS Number: %lu", swapVDSN);
        switch( tagId )     /* extra additions for: */
        { Uint32 swapADs; Uint16 swapPnmb;
        case tidUSD:        /*  USD: nmb of ADs */
          swapADs =
            ((UnallocatedSpaceDescriptor*)buffer)->numberOfAllocationDescriptors;
          endianSwap((Byte*) &swapADs, sizeof(swapADs), 1, NULL);
          fprintf(uctout, ", nmb of ADs: %lu", swapADs);
          break;
        case tidPD:             /* PD: Pnmb */
          swapPnmb = ((PartitionDescriptor*)buffer)->partitionNumber;
          endianSwap((Byte*) &swapPnmb, sizeof(swapPnmb), 1, NULL);
          fprintf(uctout, ", Partition Number: %lu", swapPnmb);
          break;
        }
        break;
      }

      /* tail of oneliner log, "D" for deleted or marked for delete
       * obligatory "\n", also if node == NULL
       */
      if(   node != NULL && node->fid != NULL
         && isBitOn(node->fid->fileCharacteristics, FCB_DELETED) )
            nodePrintUnicodeNameTxtExtra(node,mc, " D name: ", NULL);
      else  nodePrintUnicodeNameTxtExtra(node,mc, "   name: ", NULL);
      fprintf(uctout, "\n");
    }
    ENDif;  /* ifVERBOSE(vLevel) */

    /* Verify tagChecksum first, because it gives an indication
     * about the reliability of other fields.
     * TagChecksum check may not be performed in inspectDescriptorHead(),
     * so repeat it here with full error layout.
     */
    if( dTag->tagChecksum != (chsum= calculateTagChecksum(dTag)) )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
          printMessageExpectedU32( MLIMIT.vl,
            buffer, (Byte*) &dTag->tagChecksum,
            "Error: Tag Checksum", "#%02X",
            dTag->tagChecksum, chsum,
            ",\n-\t\t ECMA 3/7.2.3, 4/7.2.3\n");
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /* not fatal for the moment. TODO: fatal for -strict option ?? */
    }

    /* verify Descriptor Version and handle status.
     */
    (void) checkDescriptorVersion(dTag, node, mc);

    /* check if enough bytes read for whole descriptor
     */
    if( numberOfBytesRead < inspectDSize )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageExpectedU32( MLIMIT.vl,
                buffer, NULL,
                "Error: Descriptor number of bytes read", "%lu",
                numberOfBytesRead, inspectDSize, "\n");
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        return FALSE;   /* Fatal, not enough bytes read */
    }

    /* tagLocation
     */
    if( dTag->tagLocation != expectedTagLocation )
    {
        /* no FALSE return result */
        MLIMITbegin(ERROR00level, MLIMITdefault20);
          printMessageExpectedU32( MLIMIT.vl,
            buffer, (Byte*) &dTag->tagLocation,
            "Error: Tag Location", "%lu",
            dTag->tagLocation, expectedTagLocation,
            ",\n-\t\t UDF 2.2.1.3, ECMA 3/7.2.8 and 4/7.2.8\n");
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }

    /* Unable to verify CRC if CRC length is pointing outside
     * descriptor area (numberOfBytesRead).
     * Assume that this is caused by a CRC length error.
     */
    if( dTag->descriptorCRCLength + sizeof(Tag) > numberOfBytesRead )
    { MLIMITbegin(WARN01level, uctMessageLimit);
        printMessageHead(MLIMIT.vl, buffer,
                         (Byte*) &dTag->descriptorCRC,
          "Warning: Unable to verify Descriptor CRC, CRC Length is\n"
          "-\t\t pointing outside descriptor area,"
                            " see CRC Length error.\n");
        nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
    }
    else if( dTag->descriptorCRC !=
            (dCRC= calculateCrc(buffer + sizeof(Tag),
                                dTag->descriptorCRCLength)) )
    {   /* descriptor CRC length + Tag size <= numberOfBytesRead
         */
        MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageExpectedU32( MLIMIT.vl,
                buffer, (Byte*) &dTag->descriptorCRC,
                "Error: Descriptor CRC", "#%04X",
                dTag->descriptorCRC, dCRC,
                ", ECMA 3/7.2.6, 4/7.2.6\n");
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /* no FALSE return result */
    }

    /* check tag serial number now.
     * NO error return yet for tag serial number error.
     */
    if(    isFirstTagSN
        || theTagSerialNumber != dTag->tagSerialNumber )
    {
        if( isFirstTagSN )
        {
            isFirstTagSN = FALSE;
            VERBOSE00(uctout, "\tFirst Tag Serial Number: %u\n",
                        dTag->tagSerialNumber);
        }
        else
        {
            resetTagSerialNumberSupport();
            MLIMITbegin(ERROR00level, MLIMITdefault02);
              fprintf(uctout,
                 "\tError: Tag Serial Number changing: %u -> %u, no disaster\n"
                "-\t       recovery support, ECMA 3/7.2.5, 4/7.2.5, UDF 2.3.1.1.\n",
                            theTagSerialNumber, dTag->tagSerialNumber);
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
        }
        theTagSerialNumber = dTag->tagSerialNumber;
        if( theTagSerialNumber == 0 )
        {
            resetTagSerialNumberSupport();
            MLIMITbegin(INFO01level, MLIMITdefault01);
              fprintf(uctout,
                 "\tNote: Tag Serial Number 0, no disaster recovery support,\n"
                "-\t      ECMA 3/7.2.5, 4/7.2.5, UDF 2.3.1.1.\n");
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
        }
    }

    /* Passed all the tag id tests.
     * We can now be sure that this is a valid descriptor.
     * modifyUdfRevisionRange() may show revision conflict error.
     */
    if( tagId == tidEFE )   /* introduced in UDF 2.00 */
    {   modifyUdfRevisionRange( 0x200, MAX_UDFREVISION,
                                "Extended File Entry" );    /* 2.00 + */
    }

    /* Swap descriptor tail
     */
    endianSwapDescriptorTail((Tag *)buffer);

    /* getLengthOfDescriptor again, because length may have
     * been changed by an endianSwapDescriptorTail repair action !!!!!
     * so check again if enough bytes read for whole descriptor
     */
    if( !getLengthOfDescriptor(buffer, TRUE, &inspectDSize) )
    {   return FALSE;
    }
    if( numberOfBytesRead < inspectDSize )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageExpectedU32( MLIMIT.vl,
                buffer, NULL,
                "Fatal error: Descriptor number of bytes", "%lu",
                numberOfBytesRead, inspectDSize, "\n");
            fprintf(uctout,
                "-\t     Descriptor grown out of block"
                                " because of repair action.\n");
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        return FALSE;       /* error, not enough bytes read */
    }
    /* CONDITION: numberOfBytesRead >= inspectDSize
     */

    /* Verify descriptorCRCLength, special cases for
     * AED and SBD and FID (FID for UDF 2.00 only).
     * (see also repair of CRCLength Uin16 overflow (SBD and ST) and
     *  recommended value of 8 for SBD in errata DCN-5108 for UDF 2.50-,
     *  so DCN-5108 is only important for SBD and for Uint16 overflow).
     * Round down value for expectedCRCLen16 in case
     * of Uint16 overflow.
     */
    expectedCRCLen32 = inspectDSize - sizeof(Tag);      /* default */
    expectedCRCLen16 = (Uint16) MIN(expectedCRCLen32, MAX_UINT16);

    if( tagId == tidAED )
    {
        /* AED CRC length: UDF 2.01+ 2.3.11.1
         * For UDF 2.00-, 8 is required.
         * For UDF 2.01+, expectedCRCLen16 is recommended,
         * but 8 is still allowed.
         * Strictly according to ECMA-167, the AED descriptor length is 24,
         * but expectedCRCLen16 and getLengthOfDescriptor() do include
         * the allocation descriptors after the AED header !!!
         *
         * TODO: check single UDF revision for FS checks
         *       (getUctMinUdfRevision() == getUctMaxUdfRevision() ??)
         */
        if(    (   getUctMaxUdfRevision() <= 0x200
                && dTag->descriptorCRCLength == 8)      /* ok */
            || (   getUctMinUdfRevision() >= 0x201
                && dTag->descriptorCRCLength == expectedCRCLen16) )
        {       /* ok, no message */
        }
        else if(       dTag->descriptorCRCLength == 8
                || (   dTag->descriptorCRCLength == expectedCRCLen16
                    && getUctMaxUdfRevision() >= 0x201) )
        {       /* warn once */
          MLIMITbegin(WARN01level, MLIMITdefault01);
            printAedCRCLenMessage( "Warning:", buffer,
                                  &dTag->descriptorCRCLength,
                                   expectedCRCLen16,
                                   mc, node );
          MLIMITend;
        }
        else    /* error */
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            printAedCRCLenMessage( "Error:", buffer,
                                  &dTag->descriptorCRCLength,
                                   expectedCRCLen16,
                                   mc, node );
          MLIMITend;
        }
    }
    else if( tagId == tidSBD )          /* handle all SBD exceptions */
    {   bool printUdfRefMessage = FALSE,
             isMetadataBitmapFile =     /* special rule, 2.2.13.2 !! */
                (   node != NULL
                 && node == mc->metadataBitmapFile->node);

        /* UDF 2.2.13.2: For MetadataBitmapFile SBD,
         * only 8 or 0 allowed, see DCN-5108. Note that DCN-5108
         * is valid for ALL UDF revisions
         */
        if(       dTag->descriptorCRCLength != 8    /* recommended */
           &&     dTag->descriptorCRCLength != 0
           && (   dTag->descriptorCRCLength != expectedCRCLen16
               || isMetadataBitmapFile) )
        {   /* error */
          MLIMITbegin(ERROR00level, uctMessageLimit);
            printMessageHead( MLIMIT.vl, buffer,
                (Byte*) &dTag->descriptorCRCLength,
              "  Error: Descriptor CRC Length: ");
            fprintf(uctout, "%lu, expected: %s",
                dTag->descriptorCRCLength,
                (isMetadataBitmapFile) ? "8 or 0"
                                       : "8, 0 or ");
            if( !isMetadataBitmapFile )
            { fprintf(uctout, "%lu", expectedCRCLen16 );
            }
            fprintf(uctout, ",\n");
            printUdfRefMessage = TRUE;
          MLIMITend;
        }
        else if( dTag->descriptorCRCLength != 8 )
        { /* ok but not recommended value of 8.
           * This will be a warning for UDF revision 2.50 and higher,
           * else it is a note, because this is a very late errata
           * correction (DCN-5108) for these UDF releases.
           */
          bool isWarning = (getUctMinUdfRevision() >= 0x250);
          MLIMITbegin( (isWarning) ? WARN01level : INFO01level,
                       uctMessageLimit);
            printMessageHead( MLIMIT.vl, buffer,
                (Byte*) &dTag->descriptorCRCLength, NULL);
            fprintf(uctout, "%8s Descriptor CRC Length: %lu,\n",
                    (isWarning) ? "Warning:" : "Note:",
                    dTag->descriptorCRCLength);
            printUdfRefMessage = TRUE;      /* "8 is recommended" + ref DCN-5108 */
          MLIMITend;
        }
        else if( getUctMaxUdfRevision() < 0x260 )   /* && CRCLength == 8 */
        { /* For (older) UDF 2.50 and lower implementations, it may be
           * unexpected that this is a correct value, so add a note to
           * confirm that this is the recommended value for DCN-5108.
           */
          MLIMITbegin(INFO01level, uctMessageLimit);
            printMessageHead( MLIMIT.vl, buffer,
                (Byte*) &dTag->descriptorCRCLength,
              "   Note: Descriptor CRC Length: 8. This is the recommended value\n"
              "-\t\t\t  according to errata DCN-5108 for UDF 2.50 and lower.\n");
            /* NOT printUdfRefMessage */
          MLIMITend;
        }

        /* add reference text to error/warning message
         */
        if( printUdfRefMessage )        /* SBD ref */
        { fprintf(uctout,
              "-\t\t\t  8 is recommended, UDF 2.3.1.2, 2.3.8.1%s%s.\n",
            (isMetadataBitmapFile) ? ", 2.2.13.2" : "",
            (getUctUdfRevision() < 0x260)
                ? ",\n-\t\t\t  errata DCN-5108 for UDF 2.50 and lower" : "");
          nodePrintUnicodeNameContLine(node,mc);
        }
    }
    else if(   tagId == tidFID
            && dTag->descriptorCRCLength < expectedCRCLen16
            && getUctMaxUdfRevision() == 0x200 )
    {
        /* For UDF 2.00 ONLY, FID CRC length may be anything lower than
         * what is normally expected, warn if so.
         * For UDF 2.01+ and 1.50-, no exception !!
         */
        MLIMITbegin(WARN01level, uctMessageLimit);
            printMessageExpectedU32( MLIMIT.vl,
              buffer, (Byte*) &dTag->descriptorCRCLength,
              "Warning: Descriptor CRC Length", "%lu",
              dTag->descriptorCRCLength, expectedCRCLen16,
              ",\n-\t\t\t  FID CRC not till end of descriptor, "
                                                    "UDF 2.3.4.4,\n"
              ",\n-\t\t\t  allowed for UDF 2.00 only.\n" );
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }
    else if( dTag->descriptorCRCLength != expectedCRCLen16 )
    {   /* all exceptions handled , so this is a violation
         * of the default rules of UDF 2.2.1.2 or 2.3.1.2
         * (or the overflow rule of DCN-5108 for UDF2.50-)
         */
        MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageExpectedU32( MLIMIT.vl, buffer,
                    (Byte*) &dTag->descriptorCRCLength,
              "Error: Descriptor CRC Length", "%lu",
              dTag->descriptorCRCLength, expectedCRCLen16, ",\n");
          if( expectedCRCLen32 > MAX_UINT16 )   /* Uint16 overflow */
          { UCTASSERT( expectedCRCLen16 == MAX_UINT16 );
            fprintf(uctout,
              "-\t\t\tdefault CRC Length (%lu) does not fit in Uint16,\n",
                                expectedCRCLen32);
          }
          /* DCN-5108 only relevant for Uint16 overflow
           * (and SBD, but that's excluded here)
           */
          fprintf(uctout, "-\t\t\tUDF 2.2.1.2, 2.3.1.2%s.\n",
                (   expectedCRCLen32 > MAX_UINT16   /* Uint16 overflow */
                 && getUctUdfRevision() <= 0x250 )
                     ? ",\n-\t\t\terrata DCN-5108 for UDF 2.50 and lower"
                     : "");
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }

    /* check descriptor size,
     * some descriptors can be greater that the block size
     */
    if(    inspectDSize > blockSize
        && tagId != tidLVD
        && tagId != tidUSD
        && tagId != tidLVID
        && tagId != tidSBD
        && tagId != tidST )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageHead( MLIMIT.vl, buffer, NULL, NULL);
            fprintf(uctout,
                 "\t Error: Descriptor length: %lu, expected: at most: %lu\n"
              "-\t\t A %s descriptor shall not exceed the logical block size\n",
                        inspectDSize, blockSize, dTxt4);
            nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /* no FALSE return result, enough bytes read */
    }

    /* Verify descriptor tail
     */
    if( !verifyDescriptorTail(buffer, mc, node) )
    {
        result = FALSE;
    }

    /* Verify zero's from end of descr to end of buffer (block).
     * First test with verifyZeros() because it does not produce
     * any messages. If error and message limit not exceeded,
     * then use verifyZerosInDescriptor() that will produces
     * VERBOSE00 error message
     * PRECONDITION: numberOfBytesRead >= inspectDSize
     */
    if( !verifyZeros(buffer + inspectDSize,
                     numberOfBytesRead - inspectDSize,
                     NULL, NULL, NULL) )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          /* Note that verifyZerosInDescriptor() assumes to be
           * called from within a MLIMITbegin(ERROR00level, ...)
           * ... MLIMTend clause.
           */
          (void) verifyZerosInDescriptor(buffer, buffer + inspectDSize,
                                         numberOfBytesRead - inspectDSize,
                                         NULL);
          fprintf(uctout,
            "-\t\t End of %s till end of logical block shall\n"
            "-\t\t contain all #00 bytes. ECMA 3/8.4.4, 4/13.\n",
            (tagId == tidAED)
                ? "Allocation Descriptors" : "descriptor");
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
        /* no FALSE return result */
    }

    if( result != FALSE )   /* no fatal error */
    {   notifyUdfCorrectDescriptorFound();
    }
    return result;

}   /* end swapAndVerifyDescriptor() */

/***** UDF Revision **********************************************************
 *
 * verifyUdfRevision():
 * Verify UDF revision in hex (BCD in fact)
 * - check if known UDF Revision
 * - check if in minRevision thru maxRevision range
 */
extern bool verifyUdfRevision(Uint16 revision,
                              Uint16 minRevision,
                              Uint16 maxRevision,
                              char *txt, char *refTxt)
{   bool result = TRUE,
         printRefTxt = FALSE;

    if( !isKnownUdfRevision(revision) )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,"\tError: %s: ", txt);
        /* printUdfRevision will also flag it as "(unknown)"
         */
        printUdfRevision(MLIMIT.vl, revision, ", illegal revision\n");
        printRefTxt = TRUE;
      MLIMITend;
      result = FALSE;
    }
    else if( revision < minRevision )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,"\tError: %s: ", txt);
        printUdfRevision(MLIMIT.vl, revision, ", shall be at least: ");
        printUdfRevision(MLIMIT.vl, minRevision, "\n");
        printRefTxt = TRUE;
      MLIMITend;
      result = FALSE;
    }
    else if( revision > maxRevision )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,"\tError: %s: ", txt);
        printUdfRevision(MLIMIT.vl, revision, ", shall be at most : ");
        printUdfRevision(MLIMIT.vl, maxRevision, "\n");
        printRefTxt = TRUE;
      MLIMITend;
      result = FALSE;
    }

    if( printRefTxt && refTxt != NULL )
    { fprintf(uctout,"-\t       %s.\n", refTxt);
    }
    return result;

}   /* end verifyUdfRevision() */


/***** VAT file **************************************************************
 *
 * Verify VAT table.
 */
static bool verifyVatTable( PartitionMapInfo *vPmi )
{
    VirtualRecord *pVatRec  = &vPmi->vatRec;
    Uint32  numberOfEntries = pVatRec->numberOfEntries;
    Uint32  physicalPlen    = vPmi->pdPointer->partitionLength;
    Uint32 *pVatEntry;
    Uint32  i, cntErr, cntUnused;

    UCTASSERT( pVatRec->vatFile != NULL && pVatRec->vatTable != NULL );

    ifPRINTinfo02(uctout,
        "\tVAT table numberOfEntries: %5lu, Virtual Partition size: %lu\n",
                numberOfEntries, numberOfEntries);
    ENDif;

    if( numberOfEntries == 0 )
    {   MLIMITbegin(WARN01level,uctMessageLimit);
          fprintf(uctout,"\tWarning: Empty VAT table\n");
        MLIMITend;
        return TRUE;            /* no more checks */
    }
    else if( numberOfEntries > physicalPlen )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
            "\tError: VAT table length higher than underlying partition size\n");
        MLIMITend;
    }
    /* test VAT table entries
     */
    for( i = 0, pVatEntry = pVatRec->vatTable, cntUnused = cntErr = 0;
         i < numberOfEntries;
         i++,  pVatEntry++ )
    {
        if( *pVatEntry == VAT_UNUSED_ENTRY )
        {   cntUnused++;
        }
        else if( *pVatEntry >= physicalPlen )
        {   VERBOSE00(uctout,
              "\tError: VAT entry nr %lu points outside underlying partition: %lu\n",
                                    i+1, *pVatEntry );
            if( ++cntErr >= 10 )
            break;
        }
    }
    ifPRINTinfo02(uctout, "\tVAT table unused entries %s: %5lu\n",
                    ((cntErr==0) ? "" : "so far"), cntUnused);
    ENDif;
    return (cntErr == 0);
}

/* swapAndVerifyVatFile():
 *
 * Check and endian swap for VAT file.
 * The VAT file was introduced in UDF 1.50 but the file layout
 * was changed completely in UDF 2.00.
 *
 * input arguments:  PartitionMapInfo *vPmi,
 *                   fileSize
 *                   vPmi->vatRec->vatIntroRevision
 *
 * output arguments: vPmi->vatRec->vatTable
 *                   vPmi->vatRec->numberOfEntries
 *
 * return result: if VAT file error, then FALSE
 *                                   else TRUE.
 */
extern bool swapAndVerifyVatFile( PartitionMapInfo *vPmi,
                                  Uint32 fileSize32 )
{
    VirtualRecord *pVatRec = &vPmi->vatRec;
    Uint32       headSize = 0,
                 tailSize = 0,
                 vatTableSize = 0, nmbOfEntr;
    VAT150Tail  *vat150Tail  = NULL;
    VAT200Head  *vat200Head  = NULL;
    Uint32      *vatTable = NULL;
    char        *vatUdfRef;
    bool         result   = TRUE;

    /* which UDF spec to use depends on VAT intro revision
     */
    vatUdfRef = UDFREF_VAT( (pVatRec->vatIntroRevision == 0x150)
                            ? 0x150 : MAX_UDFREVISION );

    pVatRec->vatTable       = NULL;
    pVatRec->numberOfEntries = 0;

    switch( pVatRec->vatIntroRevision )
    {
    case 0x150:
        tailSize = sizeof(VAT150Tail);
        if( fileSize32 < tailSize )
        {
            MLIMITbegin(ERROR00level, uctMessageLimit);
              VERBOSE00(uctout,
                "\tError: VAT file size too small: %lu,"
                    " shall be at least: %lu\n",
                    fileSize32, tailSize);
            MLIMITend;
            result = FALSE;
            break;
        }
        vatTable = (Uint32 *) pVatRec->vatFile;
        vatTableSize = fileSize32 - tailSize;
        vat150Tail = (VAT150Tail *) (pVatRec->vatFile + vatTableSize);
        endianSwapVat150Tail(vat150Tail, pVatRec->vatFile);
        break;
    case 0x200:
#ifdef  DEBUG01
        MLIMITbegin(DEBUG01level, MLIMITdefault01);
            implementationNotTested( "VAT 2.00" );
        MLIMITend;
#endif  /* DEBUG01 */
        vat200Head  = (VAT200Head *) pVatRec->vatFile;

        headSize = offsetof(VAT200Head, startOfImplementationUse);
        if( fileSize32 < headSize )
        {
            MLIMITbegin(ERROR00level, uctMessageLimit);
              VERBOSE00(uctout,
                "\tError: VAT file size too small: %lu,"
                        " shall be at least: %lu\n",
                        fileSize32, headSize);
            MLIMITend;
            result = FALSE;
            break;
        }
        endianSwapVat200Head(vat200Head, pVatRec->vatFile, fileSize32);
        headSize += vat200Head->lengthofImplementationUse;
        if(    headSize > fileSize32
            || vat200Head->lengthofImplementationUse % 4 != 0 )
        {
            MLIMITbegin(ERROR00level, uctMessageLimit);
              VERBOSE00(uctout,
                "\tError: VAT file LengthofImplementationUse: %lu,"
                                                    " too high for\n"
                "-\t      file size %lu or no multiple of 4, %s\n",
                    vat200Head->lengthofImplementationUse,
                    fileSize32, vatUdfRef);
            MLIMITend;
            result = FALSE;
            break;
        }
        vatTable = (Uint32 *) (pVatRec->vatFile + headSize);
        vatTableSize = fileSize32 - headSize;
        break;
    default:
        MLIMITbegin(ERROR00level, uctMessageLimit);
          VERBOSE00(uctout,"\tError: Unknown VAT file UDF revision0x%X\n",
                                pVatRec->vatIntroRevision);
        MLIMITend;
        return FALSE;
        /** break; **/
    }
    if( result == TRUE && (vatTableSize % sizeof(Uint32)) != 0 )
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
          VERBOSE00(uctout,
            "\tError: VAT table size is no multiple of %u: %lu\n",
                        sizeof(Uint32), vatTableSize);
        MLIMITend;
        result = FALSE;
    }
    if( result == FALSE )
    {
        VERBOSE00(uctout, "-\tVAT file inconsistency, UDF ");
        printUdfRevision(VERBOSE00level, pVatRec->vatIntroRevision, "");
        VERBOSE00(uctout,", %s\n", vatUdfRef );
        return FALSE;
    }

    /* No errors, 4 byte alignment ok
     * swap VAT table
     */
    nmbOfEntr = vatTableSize / sizeof(Uint32);
    endianSwap((Byte *)vatTable, sizeof(Uint32), nmbOfEntr,pVatRec->vatFile);

    /* endian swap complete, verify.
     */
    switch( pVatRec->vatIntroRevision )
    {
    case 0x150:
        (void) verifyEntityID(&vat150Tail->EntityIdentifier,
                               ENTITY_SUFFIX_UDF, NULL, NULL, NULL);
        if( memcmp(vat150Tail->EntityIdentifier.Identifier, VIRTUAL_ALLOC_TBL_ID,
                   ENTITYID_IDSIZE) != 0 )
        {
            MLIMITbegin(ERROR00level, uctMessageLimit);
              VERBOSE00(uctout,
               "\tError: VAT EntityID.Identifier not equal to: \"%s\"\n"
              "-\t       Trying to continue. Be aware of possibly"
                                            " consequential errors\n",
                            VIRTUAL_ALLOC_TBL_ID);
            MLIMITend;
            /** no FALSE return result (for release > 0.7r1) **/
            break;
        }
        /* TODO: check rest of nd PreviousVATICBlocation
         */
        break;
    case 0x200:
        /* Relation of logicalVolumeIdentifier in LVD, IUVD, FSD
         * and VAT2 checked in checkVolumeIdentifiersSummary().
         */
        VERBOSE00(uctout, "\tVAT Logical Volume Identifier: ");
        PRINTDSTRING(  vat200Head->logicalVolumeIdentifier,
                sizeof(vat200Head->logicalVolumeIdentifier), "\n");
        verifyDstring( vat200Head->logicalVolumeIdentifier, NULL,
                sizeof(vat200Head->logicalVolumeIdentifier),
                TRUE, vatUdfRef);       /* obligatory */

        ifVERBOSE(INFO02level)
        {   fprintf(uctout, "\tVAT min UDF read  version: ");
            printUdfRevision(INFO02level, vat200Head->minUDFReadVersion, "\n");

            fprintf(uctout, "\tVAT min UDF write version: ");
            printUdfRevision(INFO02level, vat200Head->minUDFWriteVersion, "\n");

            fprintf(uctout, "\tVAT max UDF write version: ");
            printUdfRevision(INFO02level, vat200Head->maxUDFWriteVersion, "\n");
        }
        ENDif;  /* INFO02level */

        (void) verifyUdfRevision( vat200Head->minUDFReadVersion,
                                  0x200, MAX_UDFREVISION,
                        "VAT Minimum UDF Read  Revision", vatUdfRef);
        (void) verifyUdfRevision( vat200Head->minUDFWriteVersion,
                                  0x200, MAX_UDFREVISION,
                        "VAT Minimum UDF Write Revision", vatUdfRef);
        (void) verifyUdfRevision( vat200Head->maxUDFWriteVersion,
                                  0x200, MAX_UDFREVISION,
                        "VAT Maximum UDF Write Revision", vatUdfRef);

        if( vat200Head->lengthofImplementationUse >= sizeof(EntityID) )
        {   (void) verifyEntityID((EntityID*)&vat200Head->startOfImplementationUse,
                                  ENTITY_SUFFIX_IMPL, NULL, NULL, NULL);
        }
        else if( vat200Head->lengthofImplementationUse != 0 )
        {
            MLIMITbegin(ERROR00level, uctMessageLimit);
              VERBOSE00(uctout,
                 "\tError: VAT Length of Implementation Use: %lu, shall be\n"
                "-\t       0 or at least %lu for EntityID, %s\n",
                 vat200Head->lengthofImplementationUse,
                 sizeof(EntityID), vatUdfRef);
            MLIMITend;
        }

        /* TODO: more 2.00 VAT checks
         * like sort VAT entries, and check multiple ones
         * for at least a warning.
         */
        break;
    }
    if( result == FALSE )
    {
        VERBOSE00(uctout, "-\tVAT file inconsistency, UDF ");
        printUdfRevision(VERBOSE00level, pVatRec->vatIntroRevision, "");
        VERBOSE00(uctout, ", %s\n", vatUdfRef);
        return FALSE;
    }
    pVatRec->vatTable       = vatTable;
    pVatRec->numberOfEntries = nmbOfEntr;

    return verifyVatTable(vPmi);

}   /* end swapAndVerifyVatFile() */


/***** Non-Allocatable Space *************************************************
 */

/* all Non-Allocatable Space ADs shall have
 * extent type ADEL_NOT_RECORDED_BUT_ALLOCATED.
 */
static bool verifyNonAllocatableSpaceADs( UdfMountContext *mc,
                                          Node *node,
                                          char *refTxt )
{   UdfAllocationItem *ai;
    Uint32  n, firstOcc = 0, cnt = 0;
    Uint8   type, firstErrorType = 0;
    bool    result = TRUE;

    /* count ADs that have no type ADEL_NOT_RECORDED_BUT_ALLOCATED
     */
    for( ai  = node->al->head, n = 1;   /* start AD numbering with 1 */
         ai != NULL;
         ai  = ai->next,       n++ )
    {   if(    (type = adGetExtentType(&ai->aad.anyAd))
            != ADEL_NOT_RECORDED_BUT_ALLOCATED )
        {   if( (cnt++) == 0 )          /* first occurrence */
            {   firstOcc = n;           /* AD number */
                firstErrorType = type;
            }
        }
    }

    if( cnt != 0 )
    {   result = FALSE;
        MLIMITbegin(ERROR00level, uctMessageLimit);
          printMessageHead(MLIMIT.vl, node->fe, NULL, NULL);
          fprintf(uctout,
            "Error: %lu AD%s with extent type unequal to %lu (not recorded\n"
          "-\t\t    but allocated) for Non-Allocatable Space.\n"
          "-\t\t    First occurrence: AD number %lu, extent type %lu.\n"
          "-\t\t    %s, ECMA 4/14.14.1.1.\n",
            cnt, PLURAL_S(cnt), ADEL_NOT_RECORDED_BUT_ALLOCATED,
            firstOcc, firstErrorType, refTxt);
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }
    return result;
}


/* verifyNonAllocatableSpace():
 * Called by handleNonAllocatableSpace().
 * Non-Allocatable Space node.
 *
 * Non-Allocatable Space rules:
 * UDF 1.50 : Root directory file, UDF 1.50  2.3.13
 * FID file characteristics:
 *      bit 0 (Hidden)   : SHALL be set
 *      bit 4 (Metadata) : MAY  be set
 *      other bits       : all ZERO
 * (E)FE ICBTag flags      : bit 10 set (System)
 * (E)FE information length: 0 (all extents in file tail)
 * (E)FE AD extent type    : ADEL_NOT_RECORDED_BUT_ALLOCATED
 *                           (or ADEL_EXTENTPOINTER)
 *
 * UDF 2.00+: System Stream,       UDF 2.00+ 3.3.7.2
 * FID file characteristics:
 *      bit 4 (Metadata) : SHALL be set
 *      bit 0 (Hidden)   : MAY  be set
 *      other bits       : all ZERO
 * (E)FE ICBTag flags      : bit 10 set (System)
 * (E)FE information length: no restriction, from 0 thru allocated size ??
 * (E)FE AD extent type    : ADEL_NOT_RECORDED_BUT_ALLOCATED
 *
 * File Characteristics: ECMA 4/14.4.3
 * ICBTag flags        : UDF 2.3.5.4, ECMA 4/14.6.8
 *
 * return value:
 *  If inclomplete verification,
 *  then: FALSE
 *  else: TRUE
 */
extern bool verifyNonAllocatableSpace( UdfMountContext *mc,
                                       Node   *node )
{
    char  *refTxt, *charsRefTxt, *flagsRefTxt;
    Uint16 udfRevision = getUctUdfRevision();
    bool   result = TRUE;

    if( udfRevision < 0x150 )   /* Non-Allocatable Space not supported */
    {   result = FALSE;         /* incomplete verification */
    }

    /* messages will contain: "%s.",     refTxt
     *                    or: "%s, %s.", refTxt, charsRefTxt
     *                    or: "%s, %s.", refTxt, flagsRefTxt
     */
    refTxt      = NONALLOC_UDFREF_TXT;
    charsRefTxt =          "ECMA 4/14.4.3";
    flagsRefTxt = "2.3.5.4, ECMA 4/14.6.8";

    /* The file/stream name is already verified to be equal to
     * NON_ALLOC_NAME_150/NON_ALLOC_NAME_200 respectively.
     *
     * Now verify the attributes, etc.
     */
    if( node->fid == NULL ) /* no FID */
    {   result = FALSE;     /* incomplete verification */
    }
    else                    /* extra FID tests */
    {   Uint8 fileChars = node->fid->fileCharacteristics,
              testMask  = 0;
        /* UDF 1.50 : Hidden shall be set, Metadata may be set.
         * UDF 2.00+: Metadata shall be set, Hidden may be set.
         * All other bits shall be ZERO !!
         */
        if( udfRevision <= 0x150 )          /* UDF 1.50 */
        {   setBitOn(testMask, FCB_HIDDEN);
            if( isBitOn(fileChars, FCB_METADATA) )
                setBitOn(testMask, FCB_METADATA);
        }
        else                                /* UDF 2.00+ */
        {   setBitOn(testMask, FCB_METADATA);
            if( isBitOn(fileChars, FCB_HIDDEN) )
                setBitOn(testMask, FCB_HIDDEN);
        }

        if( fileChars != testMask )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageHead(MLIMIT.vl, (Byte*) node->fid,
                (Byte*) &node->fid->fileCharacteristics, NULL);
            fprintf(uctout,
              "Error: FID File Characteristics: #%02X, expected:\n"
             "-\t\t\tBit %s ONE, bit %s don't care,\n"
             "-\t\t\tother bits ZERO for Non-Allocatable Space,\n"
             "-\t\t\t%s, %s.\n",
                fileChars,
                (udfRevision <= 0x150) ? "0 (Hidden)"
                                       : "4 (Metadata)",
                (udfRevision <= 0x150) ? "4 (Metadata)"
                                       : "0 (Hidden)",
                refTxt, charsRefTxt);
            nodePrintUnicodeNameContLine(node,mc);
          MLIMITend;
        }
    }

    if( node->fe == NULL )  /* no FE */
    {   result = FALSE;     /* incomplete verification */
    }
    else                    /* extra (E)FE tests */
    {   Uint16 icbFlags = pFE_icbTag(node->fe)->flags;

        /* For ICBTag flags expect short or long AD type
         * and System bit set.
         */
        if(       isBitOff(icbFlags, ICBF_SYSTEM_BIT)
           || (   GET_ADTYPE(icbFlags) != ADT_SHORT
               && GET_ADTYPE(icbFlags) != ADT_LONG) )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageHead(MLIMIT.vl, node->fe,
                        (Byte*) &pFE_icbTag(node->fe)->flags, NULL);
            fprintf(uctout,
              "Error: FE ICBTag flags: #%04X, expected: Short or Long\n"
              "-\t\t\tAllocation Descriptors and bit 10 ONE (System)\n"
              "-\t\t\tfor Non-Allocatable Space,\n"
              "-\t\t\t%s, %s.\n",
                icbFlags, refTxt, flagsRefTxt);
            nodePrintUnicodeNameContLine(node,mc);
          MLIMITend;
        }

        /* FE informationLength 0 for UDF 1.50
         */
        if(   udfRevision == 0x150
           && *(pFE_informationLength(node->fe)) != (Uint64) 0 )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            printMessageHead(MLIMIT.vl, node->fe,
                        (Byte*)pFE_informationLength(node->fe), NULL);
            fprintf(uctout,
              "Error: FE Information Length: ");
            printUint64(VERBOSE00level, *(pFE_informationLength(node->fe)),
                        FALSE, NULL);
            fprintf(uctout, ", expected: 0 for\n"
              "-\t\t\tNon-Allocatable Space, %s.\n", refTxt);
            nodePrintUnicodeNameContLine(node,mc);
          MLIMITend;
        }
    }

    /* node->al must also be != NULL if
     * no allocation descriptors present
     */
    if( node->al == NULL )  /* Allocation descriptors error */
    {   result = FALSE;     /* incomplete verification */
    }
    else                    /* extra AD tests */
    {   (void) verifyNonAllocatableSpaceADs(mc, node, refTxt);
    }

    return result;

}   /* end verifyNonAllocatableSpace() */

