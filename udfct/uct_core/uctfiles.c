/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : uctfiles.c
 *
 * Description : File structure functions and definitions.
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mytypes.h"
#include "general.h"
#include "uctdata.h"
#include "uctnodes.h"
#include "uctgeneral.h"
#include "uctstatus.h"
#include "uctverify.h"
#include "uctendian.h"
#include "ucttimestamp.h"
#include "uctfiles.h"
#include "uctallocation.h"
#include "uctfarapart.h"


/* UCT_TESTING variants, normally #undef, #define for local testing
 */
#undef  UCT_TESTING_SWAP77          /* local: metadata swap trick 77 */
#undef  UCT_TESTING_EXTRA           /* local: extra debug */
#undef  UCT_TESTING_NP              /* local: NodePointer arrays/sorting */
#undef  UCT_TESTING_CONTINUATION    /* local: xxxx */

/* get node icb, handle exceptions
 */
extern LongAd *nodeGetIcb(Node *node, UdfMountContext *mc)
{
    Node *pNode;

    if( node == NULL )
    {   return NULL;
    }
    if( node->fid != NULL )
    {   return &node->fid->ICB;
    }
    /* some special cases:
     */
    if( mc != NULL )
    {   if( node == mc->rootNode )
        {   return &mc->fsd->rootDirectoryICB;
        }
        if( node == mc->systemStreamDirNode )
        {   return &mc->fsd->new200.systemStreamDirectoryICB;
        }
        if( node == mc->vatNode )
        {   return &mc->vatICB;
        }
        if(   mc->metadataFile != NULL
           && node == mc->metadataFile->node )
        {   return &mc->metadataFile->icb;
        }
        if(   mc->metadataMirrorFile != NULL
           && node == mc->metadataMirrorFile->node )
        {   return &mc->metadataMirrorFile->icb;
        }
        if(   mc->metadataBitmapFile != NULL
           && node == mc->metadataBitmapFile->node )
        {   return &mc->metadataBitmapFile->icb;
        }
    }
    pNode = node->parent;
    if( pNode != NULL && pNode->fe != NULL )
    {   if( node == pNode->eaFileNode )
        {   return pFE_extendedAttributeICB(pNode->fe);
        }
        if( node == pNode->streamDirNode )
        {   return pFE_streamDirectoryICB(pNode->fe);
        }
    }
    return NULL;

}   /* end nodeGetIcb() */


/* nodeGetFeExpectedTagLocation():
 * get node (E)FE expected tag location
 * (so not the tagLocation field value, but what it should be).
 * Use this function for e.g. embedded structures like FID and AEHD.
 * Mind implementation note below for strategy 4096.
 *
 * return value:
 *  if error, then MAX_UINT32
 *            else expected node FE tag location value.
 */
extern Uint32 nodeGetFeExpectedTagLocation(Node *node, UdfMountContext *mc)
{
    LongAd *pIcb;

    /* Exception:
     * For strategy type 4096 the icb address may be different from the real
     * final (E)FE logical block address.
     * In this case take the (E)FE tagLocation field value assuming that it
     * is correct. If it is not correct then this has already been flagged,
     * but new fake Tag Location errors may occur.
     */
    if(   node->fe != NULL
       && pFE_icbTag(node->fe)->strategyType == 4096 )
    { return pFE_descriptorTag(node->fe)->tagLocation;
    }
    if( (pIcb = nodeGetIcb(node, mc)) == NULL )
    {   return MAX_UINT32;      /* error */
    }
    return pIcb->extentLocation.logicalBlockNumber;     /* strategy 4 case */

}   /* end nodeGetFeExpectedTagLocation() */

/* Verify if Information Length is equal to File Body length.
 *
 * Return value: minimum of Information Length and File Body length.
 *
 * Note: For AD type ADT_INFE, node->feFileBodyLength must be set to
 * (*(pFE_lengthOfAllocationDescriptors(node->fe)).
 *
 * If warnMismatch is TRUE and a mismatch between FE Information Length
 * and File Body Length is detected, a warning message will be printed.
 */
static Uint64 nodeGetEndOfFile(Node *node, UdfMountContext *mc, bool warnMismatch)
{   Uint64 inf64;

    UCTASSERT( node != NULL );
    inf64 = (*(pFE_informationLength(node->fe)));

    if( warnMismatch && inf64 != node->feFileBodyLength )
    {
        MLIMITbegin(WARN01level, uctMessageLimit);
          fprintf(uctout,
            "\tWarning: Mismatch between %s Information Length: ",
                tidTEXT4(pFE_descriptorTag(node->fe)->tagIdentifier));
          printUint64(MLIMIT.vl, inf64, FALSE, "%5lu");
          fprintf(uctout,  "\n-\t\t\t\t   and File Body length: ");
          printUint64(MLIMIT.vl, node->feFileBodyLength, FALSE, "%5lu");
          fprintf(uctout,
            "\n-\t\t Lowest value used for read.\n");
          nodePrintUnicodeNameContLine(node,mc);
        MLIMITend;
    }
    return MIN(inf64, node->feFileBodyLength);
}


/* Determine if bytesToRead bytes can be read from node.
 * if not, then print warning and check possible mismatch
 * between Information Length and file body length.
 *
 * Note that node->feFileBodyLength must be set to
 * (*(pFE_lengthOfAllocationDescriptors(node->fe))
 * for Ad Type ADT_INFE.
 *
 * Return value: MIN(bytesToRead,'max bytes that can be read');
 */
static Uint64 nodeCheckMaxBytes(Node *node, UdfMountContext *mc,
                                Uint64 bytesToRead)
{
    Uint64 inf64 = (*(pFE_informationLength(node->fe))),
           max64 = MIN(inf64, node->feFileBodyLength);

    if( bytesToRead > max64 )   /* cannot be read */
    {
        if( inf64 == node->feFileBodyLength )
        {
            MLIMITbegin(WARN01level, uctMessageLimit);
                fprintf(uctout, "\tWarning: Cannot read ");
                printUint64(MLIMIT.vl, bytesToRead, FALSE, NULL);
                fprintf(uctout, " bytes, at most ");
                printUint64(MLIMIT.vl, max64, FALSE, NULL);
                fprintf(uctout, " bytes\n");
                nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
        }
        bytesToRead = nodeGetEndOfFile(node,mc,TRUE); /* warnMismatch */
    }
    return bytesToRead;
}


/* nodeGetNameNoSwap():
 * Get unicode name from UNSWAPPED node->fid.
 * The only field that needs endianSwap here is the
 * fid->lengthOfImplementationUse field that determines
 * the start position of the fileIdentifier field.
 *
 * Note that the FID Identifier Field is not tested here
 * and that for erroneous Identifiers it is tried to
 * convert to a sensiblle Unicode name.
 * FALSE is only returned for fatal errors.
 */
static bool nodeGetNameNoSwap( Node *node )
{
    FileIdentifierDescriptor* fid = node->fid;
    Dchars *fi;
    Uint8   l_fi;
    Uint16  l_iu;
    Uint32  nrOfUnicodeChars;
    int     unicodeLenInt;

    node->unicodeName = NULL;
    node->unicodeNameLen = 0;

    if( node->fid == NULL )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
            "\tudfNodeGetName error: No valid FID found\n");
      MLIMITend;
      return FALSE;
    }

    if( (l_fi = fid->lengthOfFileIdentifier) == 0 )
    {   return TRUE;        /* no name, e.g. for parent FID */
    }
    l_iu = fid->lengthOfImplementationUse;
    endianSwap((Byte*) &l_iu, sizeof(Uint16), 1, NULL);
    fi = ((Byte*) &(fid->startOfImplementationUse)) + l_iu;

    /* No verify of Identifier here, this will be done
     * in verifyFileIdentifierDescriptor();
     * see UncompressUnicodeEnvelope() for handling of
     * erroneous identifiers. Here only take care that
     * enough space is allocated for the unicode result,
     * specially if l_fi is even when compression ID 16
     * is applied.
     */
    nrOfUnicodeChars = (fi[0] == 8 || fi[0] == 254)
                        ? (l_fi - 1) : (l_fi / 2);

    if( (node->unicodeName = (unicode_t *)
                    tst_malloc(nrOfUnicodeChars * sizeof(unicode_t),
                                    __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    unicodeLenInt = UncompressUnicodeEnvelope(l_fi, fi, node->unicodeName);

    if( unicodeLenInt < 0 )                 /* uncompress error */
    {   checkFree((void**)&node->unicodeName);      /* set NULL */
        node->unicodeNameLen = 0;
        MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
            "\tUncompressUnicode error: file %s, line %u\n",
                            __FILE__, __LINE__);
        MLIMITend;
        return FALSE;
    }
    node->unicodeNameLen = (Uint32) unicodeLenInt;      /* >= 0 */
    UCTASSERT( node->unicodeNameLen <= nrOfUnicodeChars );

    if( node->unicodeNameLen != nrOfUnicodeChars )
    {
        MLIMITbegin(WARN01level,uctMessageLimit);
          fprintf(uctout,
            "\tUncompressUnicode warning: Number of unicode"
                                " chars: %lu, expected: %lu\n",
                node->unicodeNameLen, nrOfUnicodeChars);
        MLIMITend;
    }
    return TRUE;
}


/***
 * Reading (file) data
 * fake read if buffer == NULL
 ***/
extern bool udfGetLocation( AnyAllocationDescriptor *aad,
                            Uint8   adType,
                            Uint16  shortPartRefNumber,
                            Uint16 *partRefNumber,
                            Uint32 *logicalBlockNr)
{
    Uint16 partRef;
    Uint32 logBlock;

    switch(adType)
    {
    case ADT_SHORT:
        partRef  = shortPartRefNumber;
        logBlock = aad->shortAd.extentPosition;
        break;
    case ADT_LONG:
        partRef  = aad->longAd.extentLocation.partitionReferenceNumber;
        logBlock = aad->longAd.extentLocation.logicalBlockNumber;
        break;
    case ADT_EXTENT:
        partRef  = (Uint16) -1;
        logBlock = aad->extentAd.extentLocation;
        break;
    default:
        MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,"\tudfGetLocation error: AD type: %u,"
                            " expected: short_ad or long_ad only\n", adType);
        MLIMITend;
        return FALSE;
        /** break; **/
    }
    if( partRefNumber  != NULL ) *partRefNumber  = partRef;
    if( logicalBlockNr != NULL ) *logicalBlockNr = logBlock;
    return TRUE;

}   /* end udfGetLocation() */

static bool nodeFindLogicalBlock(Node *node, Uint64 fileOffset,
                                 Uint32 *pLogicalBlock)
{
    const MediumInfo    *vmi        = getTheMediumInfo();
    Uint32               blockSize  = vmi->blockSize;
    UdfAllocationItem   *ali;
    Uint32               offset32;

    if(       node->al == NULL
       || (   node->al->itemAdType != ADT_SHORT
           && node->al->itemAdType != ADT_LONG )
       || nodeFindAllocationDescriptor(node, fileOffset,
                                       &ali, &offset32) == FALSE
       || udfGetLocation(&ali->aad, node->al->itemAdType,
                         0, NULL, pLogicalBlock) == FALSE )
    {
        return FALSE;
    }
    (*pLogicalBlock) += offset32 / blockSize;
    return TRUE;
}


/* udfReadAllocatedBlocks():
 * returns actually correctly read nmb of blocks
 */
static Uint32 udfReadAllocatedBlocks(UdfMountContext *mc,
                                   AnyAllocationDescriptor *aad,
                                   Uint8  adType,
                                   Uint16 shortPartRefNumber,
                                   Byte  *buffer,
                                   Uint32 startBlock,
                                   Uint32 nrOfBlocks)
{   Uint16 partRefNumber;
    Uint32 logicalBlockNr;

    if( !udfGetLocation(aad, adType, shortPartRefNumber,
                        &partRefNumber, &logicalBlockNr) )
    {   return 0;       /* error, no blocks read */
    }
    return readBlocksFromPartition( mc, buffer, partRefNumber,
                                    logicalBlockNr + startBlock,
                                    nrOfBlocks);
}



static bool doFileTailFakeRead(UdfMountContext *mc, Node *node,
                               Uint64 fileSize)
{
    const MediumInfo  *vmi = getTheMediumInfo();
    UdfAllocationItem *ali;
    Uint32 nrOfBlocks;
    Uint64 totalSize = (Uint64) 0;

    if( node->al == NULL ) return TRUE;

    /* skip file body
     */
    for( ali = node->al->head;
         ali != NULL && totalSize < fileSize;
         ali = ali->next )
    {   totalSize +=
            (Uint64) ROUNDUPMULT(adGetExtentSize(&ali->aad.anyAd),
                                 vmi->blockSize);
    }
    /* fake read file tail
     */
    if( ali != NULL )
    {   ifVERBOSE( FAKE01level )
        { nodePrintUnicodeNameTxtExtra(node,mc, "\tfile tail read: ", "\n");
        }
        ENDif;
    }
    for( ; ali != NULL; ali = ali->next )
    {
        nrOfBlocks = ROUNDUPELEMENTS(adGetExtentSize(&ali->aad.anyAd),
                                     vmi->blockSize);
        if( adGetExtentType(&ali->aad.anyAd)
            == ADEL_NOT_RECORDED_NOT_ALLOCATED )
        {   /* not allocated, mimic kind of fake read logging */
            ifVERBOSE( FAKE01level )
            {   fprintf(uctout, " <nrna>\tfake read ");
                if( nrOfBlocks == 1 )
                     fprintf(uctout,"block");
                else fprintf(uctout,"%lu blocks", nrOfBlocks);
                fprintf(uctout, " for unallocated extent\n");
            }
            ENDif;
        }
        else if( udfReadAllocatedBlocks(
                        mc, &ali->aad, node->al->itemAdType,
                        node->fePartRef, NULL, 0, nrOfBlocks)
                 != nrOfBlocks )
        {   return FALSE;
        }
    }
    return TRUE;

}   /* end doFileTailFakeRead() */


/* THERead():
 * returns actually read nmb of bytes in (*pBytesRead).
 * return value: FALSE in case of error, else TRUE.
 * fake read if buffer == NULL, no read of file data.
 */
static bool THERead( UdfMountContext *mc, Node *node,
                     Uint64 fileOffset, Uint64  nrBytes,
                     Byte  *buffer,     Uint64 *pBytesRead )
{
    const MediumInfo   *vmi       = getTheMediumInfo();
    Uint32              blockSize = vmi->blockSize;
    Uint64              maxBytes;
    Uint32              offset32;       /* in extent or in block */
    Uint8               feAdType;
    Byte               *tempBuffer;
    UdfAllocationItem  *ali;
    bool                isNotRecorded, readError;

    *pBytesRead = 0;        /* export result */
    feAdType = GET_ADTYPE(pFE_icbTag(node->fe)->flags);

    /* check if nrBytes can be read, if not, reduce nrBytes.
     */
    maxBytes = nodeCheckMaxBytes(node, mc, fileOffset + nrBytes);

    if( maxBytes <= fileOffset )         /* (beyond) end of file */
    {                               /* maybe ok for nrBytes == 0 */
      return (maxBytes == (fileOffset + nrBytes));
    }
    nrBytes = MIN(nrBytes, maxBytes - fileOffset);

    switch( feAdType )
    {
    case ADT_EXTENT:
        return FALSE;         /* error, ADT_EXTENT not implemented */
        /** break; **/
    case ADT_INFE:                  /* file data embedded in (E)FE */
        if( buffer != NULL )        /* no fake read */
        {
            memcpy( buffer,
                    (Byte*)    (pFE_startOfExtendedAttributes(node->fe)) +
                    (Uint32) (*(pFE_lengthOfExtendedAttributes(node->fe))) +
                    (Uint32) fileOffset,
                    (Uint32) nrBytes );
        }
        *pBytesRead = nrBytes;      /* bytes are actually 'read' now */

        /* mimic kind of readBlock logging
         */
        if( buffer == NULL )
        {   ifPRINTfake01(uctout,
              "\tfake read FE embedded bytes: %lu\n", (Uint32) nrBytes);
            ENDif;
        }
        else
        {   ifPRINTinfo02(uctout,
              "\tread FE embedded bytes: %lu\n", (Uint32) nrBytes);
            ENDif;
        }
        return TRUE;            /* ok and ready for embedded (ADT_INFE) */
        /** break; **/
    }

    /* only ADT_SHORT and ADT_LONG allocation descriptors
     * from here
     */
    if( node->al == NULL )  /* ADT_INFE or nodeReadAllocationDescriptors() */
    {   return FALSE;       /* failed in readFileEntryEtc() */
    }
/**/UCTASSERT( feAdType == node->al->itemAdType );

    /* And now find the proper descriptors and return the proper values
     */
    if( !nodeFindAllocationDescriptor(node, fileOffset, &ali, &offset32) )
    {   return FALSE;
    }

    for( readError = FALSE;
         readError == FALSE && ali != NULL && nrBytes != 0;
         ali = ali->next )
    {   /* Read the file data extent by extent */
        Uint8        extentType;
        Uint32       readThisPass;  /* max is extent byte length (30 bits) */
        Uint32       startBlock, endBlock;  /* block offsets in extent */
        Uint32       nrOfBlocks;
        Uint64       leftInThisExtent;

        /* Always round up extent size to mult of blockSize because
         * of "ignore previous unexpected end of file body".
         */
        leftInThisExtent = adGetExtentSize(&ali->aad.anyAd);
        leftInThisExtent = ROUNDUPMULT(leftInThisExtent, blockSize);
        leftInThisExtent -= (Uint64) offset32;

        readThisPass = (Uint32) MIN(nrBytes, leftInThisExtent);

        startBlock = offset32 / blockSize;
        endBlock   = (offset32 + readThisPass - 1) / blockSize;
        nrOfBlocks = 1 + endBlock - startBlock;
        offset32   = offset32 % blockSize;  /* now offset in startBlock */

        extentType = adGetExtentType(&ali->aad.anyAd);

        isNotRecorded = (   extentType == ADEL_NOT_RECORDED_NOT_ALLOCATED
                         || extentType == ADEL_NOT_RECORDED_BUT_ALLOCATED);

        if( buffer != NULL && isNotRecorded )
        {   /* Force fake read for this extent, will read as zeros,
             * but log extents using (or mimic) fake read.
             * print message once.
             */
            MLIMITbegin(FAKE01level, MLIMITdefault05);
              ifPRINTfake01(uctout,
                "  ==>\tSwitching to fake read for unrecorded extent\n");
              ENDif;
            MLIMITend;
        }

        if( buffer == NULL || isNotRecorded )
        {
            tempBuffer = NULL;  /* fake read or 'forced' fake read */
        }
        else if( (nrOfBlocks * blockSize) <= nrBytes )  /* enough space, */
        {
            tempBuffer = buffer;            /* read directly into buffer */
        }
        else if( (tempBuffer = (Byte*) tst_malloc(nrOfBlocks * blockSize,
                                            __FILE__,__LINE__)) == NULL )
        {
            uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
        }

        /* In case of a read error, flag the error and reduce
         * readThisPass to the actual correctly read nmb of bytes.
         * Error return only after possible memmove() and adaptation
         * of (*pBytesRead).
         */
        if( extentType == ADEL_NOT_RECORDED_NOT_ALLOCATED )
        {       /* not allocated, mimic kind of fake read logging */
            ifVERBOSE( FAKE01level )
            {   fprintf(uctout, " <nrna>\tfake read ");
                if( nrOfBlocks == 1 )
                     fprintf(uctout,"block");
                else fprintf(uctout,"%lu blocks", nrOfBlocks);
                fprintf(uctout, " for unallocated extent\n");
            }
            ENDif;
        }
        else        /* allocated extent, do actual read */
        {   Uint32 nb = udfReadAllocatedBlocks(
                            mc, &ali->aad, feAdType,
                            node->fePartRef,
                            tempBuffer, startBlock,
                            nrOfBlocks);

            /* In case of read error, flag it AND reduce readThisPass
             */
            if( nb != nrOfBlocks )          /* read error */
            {   readError = TRUE;
                UCTASSERT( nb < nrOfBlocks );
                readThisPass = nb * blockSize;  /* reduce */
            }
        }

        if( buffer != NULL )        /* no fake read */
        {
            if( isNotRecorded )     /* read as all zero bytes */
            {   memset(buffer, '\0', readThisPass);
            }
            else if( tempBuffer != buffer || offset32 != 0 )
            {   memmove(buffer, tempBuffer + offset32, readThisPass);
            }
            if( tempBuffer != buffer ) checkFree((void**)&tempBuffer);
            buffer += readThisPass;
        }

        nrBytes     -= (Uint64) readThisPass;
        *pBytesRead += (Uint64) readThisPass;   /* export result */
        offset32 = 0;
    }   /* endfor readError, ali, nrBytes */

    /* readError == TRUE || ali == NULL || nrBytes == 0
     * actually nmb of bytes read is returned in (*pBytesRead).
     */
    return (!readError);        /* FALSE if read error */

}   /* end THERead() */

/* Free node children hierarchy pointed to by this node.
 * Leaves node in a consistent state.
 * Typically used when node itself cannot be removed,
 * because this would destroy its parents linked child-list.
 *
 * To remove node itself and its children hierarchy,
 * use nodeFreeHierarchy().
 * no action if node == NULL
 */
extern void nodeFreeChildren( Node *node )
{
    Node *child;

    if( node == NULL ) return;

    while( (child = node->firstChild) != NULL )
    {
        node->firstChild = child->nextInDirectory;
        nodeFreeHierarchy(child);
    }
    /* node->firstChild == NULL
     */
    node->lastChild = NULL;
    node->nrOfChildren = 0;
}

/* Free possibly nested EA File nodes
 * This nesting is not according to UDF, but
 * the verifier is able to follow them
 * no action if node == NULL
 */
static void nodeFreeEANodes( Node *node )
{
    if( node && node->eaFileNode )
    {   nodeFreeHierarchy(node->eaFileNode);
        node->eaFileNode = NULL;
    }
}

/* Free possibly nested StreamDirectory and Stream nodes.
 * This nesting is not according to UDF, but
 * the verifier is able to follow them
 * no action if node == NULL
 */
static void nodeFreeStreams( Node *node )
{
    if( node && node->streamDirNode )
    {   nodeFreeHierarchy(node->streamDirNode);
        node->streamDirNode = NULL;
    }
}

/* free Node and its Node hierarchies.
 * The normal directory hierarchy goes
 *                            via node->firstChild,
 * the EA file hierarchy goes via node->eaFileNode,
 * and the streams hierarchy  via node->streamDirNode
 * no action if node == NULL
 */
extern void nodeFreeHierarchy( Node *node )
{
    if( node == NULL ) return;

    /* remove sub-directories, EA file nodes
     * and streams first.
     */
    nodeFreeChildren(node);
    nodeFreeEANodes(node);
    nodeFreeStreams(node);

    /* remove node itself
     */
    if( node->unicodeName )     free(node->unicodeName);
    if( node->fidsAllocation )  free(node->fidsAllocation);
    if( node->fe )              free(node->fe);
    nodeFreeAllocationLists(node);
    free(node);
}


/* readFileEntryEtc() forward reference for swapAndVerifyAllEAs()
 */
static bool readFileEntryEtc(UdfMountContext *mc, Node *node,
                             LongAd *pIcb,
                             bool    expectEAFile,
                             bool    expectStreamDir,
                             Uint32  readFileEntryRecursionCount);

/* Read and verify all EAs.
 * FE embedded EAs read but not yet endian swapped.
 */
static bool swapAndVerifyAllEAs(UdfMountContext *mc, Node *node,
                                Uint32 readFileEntryRecursionCount)
{
    Byte    *nodeFe = node->fe;     /* FE or EFE */
    Uint32   lenOfEASpace, expectedAEDTagLocation;
    LongAd  *pEaICB;
    bool     result = TRUE;

    /* First verify FE embedded EAs
     */
    lenOfEASpace = (*(pFE_lengthOfExtendedAttributes(nodeFe)));
    if( lenOfEASpace > 0 )
    {   printMessageHead(INFO02level, nodeFe,
                         pFE_startOfExtendedAttributes(nodeFe),
                              NULL);
        ifPRINTinfo02(uctout,
                      "Verify embedded EA Space, %lu byte%s\n",
                      lenOfEASpace, PLURAL_S(lenOfEASpace));
        ENDif;

        /* Embedded EA space in FE Extended Attributes field.
         * Implementation mote:
         *  Ignore possible MAX_UINT32 error return value for
         *  nodeGetFeExpectedTagLocation(). This may cause a fake
         *  tag location error, caused by some previous error.
         */
        (void) swapAndVerifyExtendedAttributesSpace(
                        pFE_startOfExtendedAttributes(nodeFe),
                        lenOfEASpace,
                        nodeGetFeExpectedTagLocation(node, mc),
                        nodeFe, mc, node);
        /* no FALSE return result */
    }

    /* Now verify EA space in EA file (if any)
     * pointed to by node->fe.extendedAttributeICB.
     */
    pEaICB = pFE_extendedAttributeICB(nodeFe);
    if( adGetExtentSize(pEaICB) > 0 )   /* EA file exists */
    {
        Node    *eaNode;
        Uint64   size64, bytesRead;

        /* new node in node EA file hierarchy
         * install on node->eaFileNode.
         * it will be freed when parent node is freed.
         */
        if( (eaNode = NEWNODE()) == NULL )
        {   return FALSE;
        }
        NODEFLAGS_SET_BIT(eaNode, NFB_EA);
        if( NODEFLAGS_IS_SET( node, NFB_SYSTEM) )
        { NODEFLAGS_SET_BIT(eaNode, NFB_SYSTEM);
        }
        node->eaFileNode = eaNode;
        eaNode->parent = node;
        eaNode->fePartRef =
                pEaICB->extentLocation.partitionReferenceNumber;

        ifPRINTinfo02(uctout, "\tRead and verify attached EA file\n");
        ENDif;

        /* recursion !!
         * readFileEntryEtc() will read the EA ICB File Entry
         * and all ADs involved. Strategy 4096 will be handled.
         * If readFileEntryEtc() returns FALSE, it
         * will set eaNode->fe = NULL and
         * nodeFreeAllocationLists(eaNode) will be executed.
         */
        (void) readFileEntryEtc(mc, eaNode,
                                pEaICB, TRUE, FALSE,    /* expectEAFile */
                                readFileEntryRecursionCount + 1);

        if( eaNode->fe == NULL )    /* (E)FE error */
        {   result = FALSE;
        }
        else                        /* (E)FE ok */
        {   Byte *eaBuffer = NULL;  /* avoid free() if not used */
            /* check file length and read the EA file.
             */
            size64 = nodeGetEndOfFile(eaNode, mc, TRUE);
            lenOfEASpace = (Uint32) size64;
            if(             lenOfEASpace == 0
               || ((Uint64) lenOfEASpace) != size64 )
            {   MLIMITbegin(ERROR00level,uctMessageLimit);
                  fprintf(uctout, "\tError: EA file length: ");
                  printUint64(VERBOSE00level, size64, FALSE, NULL);
                  fprintf(uctout, "\n");
                MLIMITend;
                result = FALSE;
            }
            else if( (eaBuffer = (Byte*)tst_malloc(lenOfEASpace,
                                        __FILE__,__LINE__)) == NULL )
            {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
            }
            else if(  !THERead(mc, eaNode, 0, lenOfEASpace,
                               eaBuffer, &bytesRead)
                    || bytesRead != size64 )
            { MLIMITbegin(ERROR00level,uctMessageLimit);
                fprintf(uctout, "-\tread EA file error: THERead failed\n");
              MLIMITend;
              result = FALSE;
            }
            else        /* ok, get logical block number of EAHD tag */
            {   /* but first fake read file tail, if any.
                 */
                (void) doFileTailFakeRead(mc, eaNode, lenOfEASpace);

                if( GET_ADTYPE(pFE_icbTag(eaNode->fe)->flags) == ADT_INFE )
                {                       /* EA space embedded in eaNode FE */
                    expectedAEDTagLocation = nodeGetFeExpectedTagLocation(eaNode, mc);
                    if( expectedAEDTagLocation == MAX_UINT32 )
                    {   result = FALSE;     /* error */
                    }
                }
                else if( !nodeFindLogicalBlock( eaNode, (Uint64) 0,
                                               &expectedAEDTagLocation) )
                {   result = FALSE;
                }
                if( result != FALSE )   /* verify EA Space */
                {
                    (void) swapAndVerifyExtendedAttributesSpace(
                                eaBuffer, lenOfEASpace,
                                expectedAEDTagLocation,
                                eaBuffer, mc, eaNode);
                    /* no FALSE return result */
                }
            }
            checkFree((void**)&eaBuffer);
        }
        if( result == FALSE )
        {   VERBOSE00(uctout,
                "-\tUnable to verify Extended Attribute file\n");
            nodePrintUnicodeNameContLine(eaNode,mc);
        }
    }
    return result;

}   /* end swapAndVerifyAllEAs() */


/* streams were introduced in UDF 2.00
 */
static bool addStreamDirectoryNode(UdfMountContext *mc, Node *node,
                                   Uint32 readFileEntryRecursionCount)
{
    ExtendedFileEntry *efe;     /* points to EFE, else NULL */
    Byte    *fe = node->fe;
    LongAd  *sdICB;

    if( fe == NULL ) return FALSE;  /* error, no FE */

    if( ((Tag*)fe)->tagIdentifier != tidEFE )
    {   return TRUE;                /* ok, FE, no EFE */
    }
    efe   = (ExtendedFileEntry *) fe;
    sdICB = &efe->streamDirectoryICB;

    if( adGetExtentSize(sdICB) == 0 )
    {   return TRUE;        /* no stream directory */
    }

    /* stream directory exists, check UDF revision.
     */
    if( getUctMinUdfRevision() < 0x200 )    /* Min: on the safe side */
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
            "\tError: Stream Directory illegal for UDF ");
          printUdfRevision( MLIMIT.vl, getUctMinUdfRevision(),
                            ", ignore.\n" );
        MLIMITend;
        return FALSE;   /* stream directory ignored */
    }
    if( (node->streamDirNode = NEWNODE()) == NULL )
    {   return FALSE;
    }
    NODEFLAGS_SET_BIT(node->streamDirNode, NFB_STREAMDIR);
    if( NODEFLAGS_IS_SET(node, NFB_SYSTEM) )    /* normally not */
    {   NODEFLAGS_SET_BIT(node->streamDirNode, NFB_SYSTEM);
    }
    node->streamDirNode->parent = node;
    node->streamDirNode->fePartRef =
                        sdICB->extentLocation.partitionReferenceNumber;

    ifPRINTinfo02(uctout, "\tRead attached stream directory\n");
    ENDif;

    if ( !readFileEntryEtc(mc, node->streamDirNode,
                           sdICB, FALSE, TRUE,      /* expectStreamDir */
                           readFileEntryRecursionCount + 1) )
    {
        return FALSE;   /* error, do not remove streamdir, etc. nodes */
    }
    return TRUE;

}   /* end addStreamDirectoryNode() */


static bool acceptAs4096ICBtermination(bool   readBlockError,
                                       Byte  *blockBuffer,
                                       Uint16 tagId )
{
    const MediumInfo *vmi = getTheMediumInfo();
    char *format =
            "\tWarning: %s accepted as valid ICB hierarchy termination\n";

    /* not that each has its own MLIMIT 1 encapsulation
     */
    if( readBlockError )        /* maybe unrecorded block */
    {   MLIMITbegin(WARN01level, MLIMITdefault01);
            fprintf(uctout, format, "Read error");
        MLIMITend;
        return TRUE;    /* accepted */
    }
    /* read block ok
     */
    if(   blockBuffer != NULL
       && verifyZeros(blockBuffer, vmi->blockSize, NULL, NULL, NULL) )
    {   MLIMITbegin(WARN01level, MLIMITdefault01);
            fprintf(uctout, format, "Blank block");
        MLIMITend;
        return TRUE;    /* accepted */
    }
    if( tagId == tidTE )
    {   MLIMITbegin(WARN01level, MLIMITdefault01);
            fprintf(uctout, format, "Terminal Entry");
        MLIMITend;
        return TRUE;    /* accepted */
    }
    return FALSE;   /* not accepted */

}   /* end acceptAs4096ICBtermination() */

/* expandStrategy4096IE:
 * A Direct Entry with strategy 4096 was found by readIcbDirectEntry()
 * in the first block of a strategy 4096 ICB.
 * Expand ICB further by reading Indirect Entry from 2nd block, etc.
 *
 * return value: FALSE if errors
 *         else: TRUE
 */
static bool expandStrategy4096IE(UdfMountContext *mc, Node *node,
                                 Uint32  ieLogicalBlock,
                                 Uint16  iePartRef,
                                 Byte  **ppNextDE,
                                 Uint32  readIcbDirectRecursionCount)
{
    IndirectEntry    *pIE;
    const MediumInfo *vmi   = getTheMediumInfo();
    Uint32   blockSize      = vmi->blockSize;
    Uint16   tagIdentifier;
    Byte    *ieBlockBuffer;     /* read IE (or TE) */
    bool     readBlockError,
             result = TRUE;

    (*ppNextDE) = NULL;

    if( (ieBlockBuffer = (Byte *) tst_malloc( blockSize,
                                      __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    readBlockError = ( readBlocksFromPartition(mc, ieBlockBuffer,
                                        iePartRef, ieLogicalBlock, 1)
                       != 1 );

    /* accept as termination:
     * read error (unrecorded block) or blank block
     */
    if( acceptAs4096ICBtermination(readBlockError,
                                   ieBlockBuffer, tidUNKNOWN) )
    {   /* read error or blank block maybe accepted as termination */
        free(ieBlockBuffer);
        return TRUE;            /* found valid termination */
    }
    UCTASSERT( !readBlockError );

    if( !swapAndVerifyDescriptor(ieBlockBuffer, blockSize,
                                 tidUNKNOWN, &tagIdentifier,
                                 ieLogicalBlock, mc, node ) )
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
            "\tDescriptor error: strategy 4096 hierarchy termination error\n");
        MLIMITend;
        free(ieBlockBuffer);
        return FALSE;
    }

    if( tagIdentifier != tidIE )
    {   free(ieBlockBuffer);
        if( tagIdentifier == tidTE )
        {   return acceptAs4096ICBtermination(FALSE, NULL, tidTE);
        }
        VERBOSE00(uctout, "\tUnexpected Descriptor, "
                                "abort reading strategy 4096 hierarchy\n");
        return FALSE;
    }

    /* Indirect Entry, read next DE in ICB
     */
    pIE = (IndirectEntry*) ieBlockBuffer;
    VERBOSE00(uctout,
            "\tIndirect Entry, read next Direct Entry in strategy 4096 hierarchy\n");

    if( node != NULL )      /* (E)FE expected, set node context */
    {   UCTASSERT( iePartRef == node->fePartRef );
        node->fe = NULL;
//**    node->fePartRef =
//**        pIE->indirectICB.extentLocation.partitionReferenceNumber;
    }

    /* readIcbDirectEntry() will return TRUE if a correct DE
     * was read, or if a valid ICB termination condition was found.
     */
    if(    readIcbDirectEntry( mc, node,
                pIE->indirectICB.extentLocation.logicalBlockNumber,
                pIE->indirectICB.extentLocation.partitionReferenceNumber,
                adGetExtentSize(&pIE->indirectICB),
               &tagIdentifier, NULL,         /* dummy pIsStrategy4096 */
                ppNextDE,
                readIcbDirectRecursionCount + 1)    /* next recursion */
        && (*ppNextDE) == NULL )
    {   /* valid termination found, result TRUE
         */
    }
    else if( (*ppNextDE) == NULL )
    {   /* error in DE, or incorrect ICB hierarchy termination
         */
        result = FALSE;
    }
    else    /* (*ppNextDE) != NULL, next DE read */
    {       /* ICBTag always directly after Descriptor Tag */
        ICBTag *icbTag = (ICBTag*)((*ppNextDE) + sizeof(Tag));
        if( icbTag->strategyType != 4096 )
        {   /* correct next DE read, but wrong strategy type */
          MLIMITbegin(ERROR00level, uctMessageLimit);
            printMessageExpectedU32(MLIMIT.vl, *ppNextDE,
                (Byte*) &icbTag->strategyType,
                "expandStrategy4096IE error: Strategy Type", "%u",
                icbTag->strategyType, 4096, "\n");
          MLIMITend;
          /* no FALSE return result */
        }
    }

    free(ieBlockBuffer);        /* that was all we need from IE */
    return result;

}   /* end expandStrategy4096IE() */


/* readIcbDirectEntry:
 * Allocate memory and Read final Direct Entry
 * in an ICB hierarchy. Strategy 4 or or 4096.
 * The Direct Entry can be an (E)FE, USE or TE.
 * Note that an ICB Direct Entry shall not occupy more than one block,
 * so we always allocate a descriptor buffer of exactly one block.
 *
 * return value: TRUE if a correct Direct Entry was read or if a valid
 *                    ICB terminating condition was found.
 *         else: FALSE
 *
 * Exception: if (node != NULL), the result buffer will also be assigned
 * to node->fe and the node->fe* context will be maintained for use
 * in swapAndVerifyDescriptor(). [swapAndVerifyDescriptor() wil fail for
 *                                (E)FE if node or node->fe == NULL !!!!!!]
 * It is assumed that (node == NULL) if no (E)FE is expected.
 * For valid ICB terminating condition see acceptAs4096ICBtermination().
 * If pIsStrategy4096 != NULL, an appropriate boolean result is returned
 * in (*pIsStrategy4096).
 */
#define READICBDIRECT_RECURSION_LIMIT 1000

extern bool readIcbDirectEntry( UdfMountContext *mc,
                                Node    *node,
                                Uint32   logicalBlock,
                                Uint16   partRef,
                                Uint32   extentSize,
                                Uint16  *pTagIdentifier,
                                bool    *pIsStrategy4096,
                                Byte   **ppDirectEntry,
                                Uint32   readIcbDirectRecursionCount)
{
    STATIC_RECURSIONADMIN;
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32  blockSize = vmi->blockSize,
            recursionUnique32, dummy, dLen;
    Uint16  strategyType;
    Byte   *nextDE;
    ICBTag *icbTag;
    bool    readBlockError,
            result = TRUE;

    *pTagIdentifier  = tidUNKNOWN;  /* output defaults */
    *ppDirectEntry   = NULL;
    if( pIsStrategy4096 != NULL )
    {  *pIsStrategy4096 = FALSE;
    }

    /* check readIcbDirectEntry() recursion
     * take absolute block address for recursionUnique32
     */
    if( !translateAddress( mc, partRef, logicalBlock,
                          &recursionUnique32, &dummy, TRUE) ) /* silent */
    {
        return FALSE;
    }
    if( !checkRecursion(&recursionAdmin, recursionUnique32,
                        readIcbDirectRecursionCount, READICBDIRECT_RECURSION_LIMIT,
                        "readIcbDirectEntry") )
    {
        VERBOSE00(uctout,
            "-\tAbort strategy 4096 chain before (maybe again) reading block %lu\n",
                    recursionUnique32);
                        /* infinite resursion loop detected */
        return FALSE;   /* or recursion limit reached */
    }                   /* or memory allocation error */

#define TEMP_PATCH_FOR_STRAT4096_OVERHEAD_ALLOCATION
#ifdef  TEMP_PATCH_FOR_STRAT4096_OVERHEAD_ALLOCATION
    /* Mark all blocks in the ICB extent as allocated.
     * TODO: temp patch, problems for strat 4096 hard links.
     * (readIcbDirectRecursionCount == icb Prior Direct Entries)
     */
    if( readIcbDirectRecursionCount != 0 )      /* not first time */
    {   (void) markPartitionAllocation(mc, partRef, logicalBlock,
                            ROUNDUPELEMENTS(extentSize, blockSize),
                            NULL, FALSE);       /* TODO: check if marked already */
    }
#endif  /** TEMP_PATCH_FOR_STRAT4096_OVERHEAD_ALLOCATION **/

    if( ((*ppDirectEntry) = (Byte *) tst_malloc( blockSize,
                                      __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    readBlockError = ( readBlocksFromPartition(mc, *ppDirectEntry,
                                               partRef, logicalBlock, 1)
                       != 1 );

    /* FIRST check ICB extentSize (depending on strategy type).
     * Note that no endian swap has yet be done.
     * ICBTag always directly after Descriptor Tag !!
     */
    if(   !readBlockError
       &&  inspectDescriptorHead(*ppDirectEntry, blockSize, blockSize,
                                 FALSE,     /* NOT checksumIsFatal */
                                 tidICBDIRECTENTRY, pTagIdentifier,
                                 NULL) )

    {   /* descriptor ok (tidICBDIRECTENTRY), so it has an ICBTag.
         */
        icbTag = (ICBTag*)((*ppDirectEntry) + sizeof(Tag));
        strategyType = icbTag->strategyType;    /* unswapped */
        endianSwap((Byte*)&strategyType, sizeof(Uint16),1,NULL);

        /* check ICB extent size and type
         */
        if(    (strategyType == 4    && extentSize != blockSize)
            || (strategyType == 4096 && extentSize != (2 * blockSize)) )
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            fprintf(uctout,
               "\tError: Unexpected extent size: %lu, "
                                        "expected: %lu (%s)\n"
              "-\t       for strategy type %lu ICB, "
                                    "ECMA 4/14.6.2, UDF 6.6.\n",
                 extentSize,
                (strategyType == 4) ? blockSize : (2 * blockSize),
                (strategyType == 4) ? "1 block" : "2 blocks",
                 strategyType);
            nodePrintUnicodeNameContLine(node, mc);
          MLIMITend;
          /* no FALSE return result */
        }
    }

    /* check termination on Direct Entry position as well,
     * but do not accept termination for first DE.
     */
    if(   readIcbDirectRecursionCount != 0          /* not first DE */
       && acceptAs4096ICBtermination( readBlockError,
                                     *ppDirectEntry, tidUNKNOWN) )
    {   /* read error or blank block may be accepted as termination */
        checkFree((void**)ppDirectEntry);
        return TRUE;
    }

    if( readBlockError )        /* first DE in ICB */
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
            "\treadIcbDirectEntry error: Error reading logical block p%u: %lu\n",
                partRef, logicalBlock);
        MLIMITend;
        checkFree((void**)ppDirectEntry);
        return FALSE;
    }

    /* Now further check of descriptor.
     * Node context in node and mc must be set for context verification.
     * In case of an error, at least the descriptor tag will be swapped !!
     * swapAndVerifyDescriptor() will fail if descriptor is not a Direct
     * Entry, see isIcbDirectEntry();
     * It will also fail for (E)FE if node or node->fe is NULL.
     */
    if( node != NULL )      /* (E)FE expected, set node context */
    { node->fe = (*ppDirectEntry);
    }
    if( !swapAndVerifyDescriptor(*ppDirectEntry, blockSize,
                                 tidICBDIRECTENTRY, pTagIdentifier,
                                 logicalBlock, mc, node) )
    {   /* error in descriptor, error counted already.
         * (*pTagIdentifier) may be tidUNKNOWN,
         * get 'real' tag id for further use.
         */
        *pTagIdentifier = ((Tag *)(*ppDirectEntry))->tagIdentifier;
        VERBOSE00(uctout, "-\tICB Direct Entry error, tag id: %lu (%s)\n",
                    *pTagIdentifier, tidTEXT4(*pTagIdentifier));
        if( node != NULL )      /* (E)FE expected */
        { node->fe = NULL;      /* no (E)FE found */
        }
        /* don't return FALSE for error in TE
         */
        if( (*pTagIdentifier) != tidTE )
        {   checkFree((void**)ppDirectEntry);   /* free read descriptor */
            return FALSE;
        }
    }
    icbTag = (ICBTag*)((*ppDirectEntry) + sizeof(Tag));
    strategyType = icbTag->strategyType;        /* swapped now */

    /* Check TE termination on DE position.
     * Do not accept termination for first DE.
     */
    if( (*pTagIdentifier) == tidTE )        /* don't bother about */
    {                                       /*  possible error in TE */
        if(    readIcbDirectRecursionCount == 0   /* first DE in ICB */
           || !acceptAs4096ICBtermination(FALSE, NULL, tidTE) )
        {   result = FALSE;
        }
        checkFree((void**)ppDirectEntry);   /* free TE descriptor */
        return result;
    }

    /* here if and only if a correct Direct Entry was read
     * maybe expand for strategy 4096
     */
    if( strategyType == 4096 )
    {   /* read next IE and DE in strategy 4096 hierarchy,
         * RECURSION, show prior nmb of DEs !!
         */
        if( pIsStrategy4096 != NULL )
        {  *pIsStrategy4096 = TRUE;
        }
        VERBOSE00(uctout,
            "\tICB Strategy 4096 hierarchy, prior DEs: %lu, expand\n",
                    readIcbDirectRecursionCount);
        if( node != NULL )                  /* (E)FE expected */
        {   node->fePriorDirectEntries++;   /* count FEs in ICB hierarchy */
        }
        /* expandStrategy4096IE() may call readIcbDirectEntry(),
         * so possible recursion.
         */
        nextDE = NULL;
        if( !expandStrategy4096IE( mc, node,     /* recursion */
                                   logicalBlock + 1,    /* IE */
                                   partRef,
                                  &nextDE,
                                   readIcbDirectRecursionCount) )
        {   checkFree((void**)&nextDE);         /* error TODO: check */
            checkFree((void**)ppDirectEntry);   /* remove previous DE */
            return FALSE;
        }

        /* replace DE by the one from next cell if any
         */
        if( nextDE != NULL )
        {   checkFree((void**)ppDirectEntry);   /* free former one */
            (*ppDirectEntry) = nextDE;          /* replace */
            if( node != NULL )              /* (E)FE expected */
            {   node->fe = nextDE;
            }
        }
    }
    else if( strategyType != 4 ) /* TODO: check here or in swapAndVerify.. */
    {   MLIMITbegin(ERROR00level,uctMessageLimit);
          printMessageHead( MLIMIT.vl, *ppDirectEntry,
                            (Byte*) &icbTag->strategyType, NULL);
          fprintf(uctout, "Error: Illegal ICB Strategy Type: %u\n",
                            strategyType);
        MLIMITend;
        checkFree((void**)ppDirectEntry);   /* set to NULL */
        return FALSE;
    }

    /* Finaly: Shrink (tst_realloc) Direct Entry to real relevant length
     * in order to minimize memory usage, specially for (E)FE.
     * bug alert:
     *  tst_realloc can change pointer so if node exists,
     *  node->fe must also be updated.
     */
    if(  !getLengthOfDescriptor(*ppDirectEntry, TRUE, &dLen)    /* isSwapped */
       || dLen > blockSize )
    {   checkFree((void**)ppDirectEntry);
        return FALSE;
    }

    nextDE = (*ppDirectEntry);      /* temporary use */
    (*ppDirectEntry) = tst_realloc(nextDE, dLen, __FILE__, __LINE__);

    if( node != NULL )      /* (E)FE expected, set node context */
    { node->fe = (*ppDirectEntry);   /* maybe realloced or NULL */
    }
    if( (*ppDirectEntry) == NULL )
    {   checkFree((void**)&nextDE);
        uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
    }
    return TRUE;            /* (*ppDirectEntry) != NULL */

}   /* end readIcbDirectEntry() */


/* Allocate memory and read (Extended) File Entry in node->fe.
 * Read Allocation Descriptors in node->al, etc.
 * Note that a file entry shall not occupy more than one sector.
 * So we always return in node->fe a chunk of memory which is
 * exactly one sector in size.
 * node->fePartRef must hold the
 * correct partition reference number.
 *
 * Return value: If no error, then TRUE els FALSE.
 * An error can be:
 * 1) FE error, then node->fe == NULL
 *               and node->al == NULL
 * 2) AD error, only node->al == NULL
 *
 * Maybe recursion for extendedAttributeICB
 *
 * TODO: At the moment ADs and EAs of 'intermediate' FEs in
 *       a strategy 4096 chain are not read and tested.
 *       But partition allocation may find unallocated blocks
 *       marked as allocated for EA ICB (and streams).
 */
#define READFE_RECURSION_LIMIT 10

static bool readFileEntryEtc(UdfMountContext *mc, Node *node,
                             LongAd *pIcb,
                             bool    expectEAFile,
                             bool    expectStreamDir,
                             Uint32  readFileEntryRecursionCount)
{
    STATIC_RECURSIONADMIN;
    Uint32  recursionUnique32, dummy;
    Uint16  tagId;
    Uint8   fileType, expFileType;
    Byte   *tmpFE;

    /* assert that not both EA file and Stream Directory are requested
     * and that no FE was read for this node before
     * and that this is first FE in ICB hierarchy.
     */
    UCTASSERT( !(expectEAFile && expectStreamDir) );
    UCTASSERT(   node->fe == NULL
              && node->al == NULL
              && node->fePriorDirectEntries == 0);  /* first FE in ICB */

    /* check readFileEntryEtc() recursion
     * take absolute block address for recursionUnique32
     */
    if( !translateAddress(mc,
                          pIcb->extentLocation.partitionReferenceNumber,
                          pIcb->extentLocation.logicalBlockNumber,
                          &recursionUnique32, &dummy, TRUE) ) /* silent */
    {   MLIMITbegin(ERROR00level, uctMessageLimit);
          VERBOSE00(uctout,
              "\terror: readFileEntry() ICB address (%lu,p%u) translation error for ",
              pIcb->extentLocation.logicalBlockNumber,
              pIcb->extentLocation.partitionReferenceNumber);
          nodePrintUnicodeNameTxt(node, mc, NULL, NULL);
          VERBOSE00(uctout, "\n");
        MLIMITend;
        return FALSE;
    }
    if( !checkRecursion(&recursionAdmin, recursionUnique32,
                        readFileEntryRecursionCount, READFE_RECURSION_LIMIT,
                        "readFileEntryEtc") )
    {
        VERBOSE00(uctout,
            "-\tAbort nested (E)FE reading before (maybe again) reading block %lu\n",
                    recursionUnique32);
                        /* infinite resursion loop detected */
        return FALSE;   /* or recursion limit reached */
    }                   /* or memory allocation error */

    readIcbDirectEntry( mc, node,
                        pIcb->extentLocation.logicalBlockNumber,
                        pIcb->extentLocation.partitionReferenceNumber,
                        adGetExtentSize(pIcb),
                       &tagId, NULL,    /* dummy pIsStrategy4096 */
                       &tmpFE, 0);      /* start recursion */

    if( tagId != tidFE && tagId != tidEFE ) /* wrong descriptor */
    {   checkFree((void**)&tmpFE);          /* NULL */
    }
    if( tmpFE == NULL )     /* add to error message */
    {   VERBOSE00(uctout, "-\tNote: No usable file entry found for ");
        nodePrintUnicodeNameTxt(node, mc, NULL, NULL);
        VERBOSE00(uctout, "\n");
        return FALSE;
    }

    /* FE or EFE found in node->fe. In the case of strategy 4096
     * this is the target FE/EFE at the end of the 4096 chain.
     * Check for unexpected File Type.
     */
    fileType    = pFE_icbTag(node->fe)->fileType;
    expFileType = (Uint8)((expectEAFile)
                            ? FT_EXTENDED_ATTR
                            : (expectStreamDir)
                                ? FT_STREAM_DIRECTORY
                                : fileType);

    if( fileType != expFileType )
    {   MLIMITbegin(ERROR00level, uctMessageLimit);
          fprintf(uctout,
            "\tError: %s File Type: %u (%s),\n"
            "-\t       expected: %u (%s), ECMA 4/14.6.6\n",
              (expectEAFile) ? "Extended Attribute file"
                             : "Stream Directory",
                fileType, FT_TEXT4(fileType),
                expFileType, FT_TEXT4(expFileType));
          nodePrintUnicodeNameContLine(node, mc);
        MLIMITend;
    }
    else if(   (fileType == FT_EXTENDED_ATTR    && (!expectEAFile))
            || (fileType == FT_STREAM_DIRECTORY && (!expectStreamDir)) )
    {   MLIMITbegin(WARN01level, uctMessageLimit);
          fprintf(uctout,
            "\tWarning: Unexpected File Entry File Type: %u (%s)\n",
                fileType, FT_TEXT4(fileType));
          nodePrintUnicodeNameContLine(node, mc);
        MLIMITend;
    }

    /* verify FID/FE consistency if appropriate.
     * If a FID is pointing to this (E)FE, the FID is normally
     * read before the (E)FE. This is not the case for the
     * parent FID, so there will be an extra call to
     * verifyFidFeConsistency() after a parent FID is read.
     */
    (void) verifyFidFeConsistency(node, mc);

    /* Now first read and verify all Allocation Descriptors (ADs),
     * then all Extended Attributes (EAs).
     * Read and verify ADs in nodeReadAllocationDescriptors();
     * nodeReadAllocationDescriptors() must also be called
     * in case of FE embedded data for proper initialization.
     */
    (void) nodeReadAllocationDescriptors(mc, node);

    /* Read and verify all EAs, if any.
     * First FE embedded EAs, then those pointed to
     * by node->fe.extendedAttributeICB.
     */
    (void) swapAndVerifyAllEAs(mc, node, readFileEntryRecursionCount);

    /* Add Stream Directory Node, if any.
     */
    (void) addStreamDirectoryNode(mc, node, readFileEntryRecursionCount);

    return TRUE;

}   /* end readFileEntryEtc() */


/* readVAT: In case of an error, mc allocated memory will be freed
 * in udfMountLogicalVolume(), using udfUnmountLogicalVolume().
 */
extern bool readVAT( UdfMountContext *mc,
                     Uint16 virtualPRef )
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32       blockSize = vmi->blockSize;
    Uint32       lastRecorded = vmi->lastRecordedBlockNr;
    PartitionMapInfo *vPmi = &mc->partitionMapInfo[virtualPRef];
    Uint16       counterpartPRef = vPmi->counterpartPRef;
    Uint32       fileSize32, dummy01, dummy02;
    LongAd      *pIcb;
    Uint64       bytesRead;
    Byte        *vatFile = NULL;
    Uint16       vatIntroRevision;  /* VAT introduction UDF revision */
    Uint16       vatMaxRevision;    /* last revision supporting vatIntroRevision */

    ifPRINTinfo02(uctout, "\n\tRead VAT File Entry\n" );
    ENDif;

    /* Virtual partition present.
     */
    UCTASSERT( counterpartPRef != virtualPRef );

    if( (mc->vatNode = NEWNODE()) == NULL ) return FALSE;

    NODEFLAGS_SET_BIT(mc->vatNode, NFB_VAT);
    mc->vatNode->fePartRef = counterpartPRef;

    /* Read the VAT information in memory
     * VAT file entry first, must be FE, no EFE (Extended File Entry) allowed.
     * Calculate logical block number in partition referenced by
     * counterpartPRef, in order to check the tag location.
     * Set LongAd mc->vatICB, also used for tests later like
     * File Link Count, UniquID, etc.
     */
    pIcb = &mc->vatICB;     /* vatICB holds all zero bytes */
    pIcb->extentLocation.partitionReferenceNumber = counterpartPRef;
    pIcb->extentLocation.logicalBlockNumber = lastRecorded -
                            vPmi->pdPointer->partitionStartingLocation;
    pIcb->extentLength = blockSize; /* recorded and allocated, strategy 4 */

    /* Test address before actually reading because subsequent calls
     * to translateAddress() in readFileEntryEtc() may return FALSE
     * without error message (silent).
     */
    if(   !translateAddress( mc, counterpartPRef,
                             pIcb->extentLocation.logicalBlockNumber,
                            &dummy01, &dummy02, FALSE ) /* NOT silent */
       || !readFileEntryEtc(mc, mc->vatNode, pIcb, FALSE, FALSE,0) )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
            "-\treadVAT error: readFileEntryEtc failed for block %lu (p%u: %lu)\n",
                lastRecorded,
                pIcb->extentLocation.partitionReferenceNumber,
                pIcb->extentLocation.logicalBlockNumber);
      MLIMITend;
      nodeFreeHierarchy(mc->vatNode);   /* frees vatNode->fe, etc. as well */
      mc->vatNode = NULL;
      return FALSE;
    }
    if( ((Tag *)mc->vatNode->fe)->tagIdentifier == tidEFE )
    { MLIMITbegin(INFO01level,uctMessageLimit);
        fprintf(uctout, "\tNote: Extended File Entry for VAT ICB\n");
      MLIMITend;
    }

    VERBOSE00(uctout, "\tVAT Modification Time: ");
    printTimestampShort(pFE_modificationTime(mc->vatNode->fe), TRUE,"\n");

    if( pFE_icbTag(mc->vatNode->fe)->fileType == FT_UNKNOWN_OR_VAT150 )
    {
        char *txt = "1.50 VAT File Entry";
        VERBOSE00(uctout, "\tUDF %s\n", txt );
        vatIntroRevision = 0x150;               /* UDF 1.50 only */
        vatMaxRevision   = 0x150;
    }
    else if( pFE_icbTag(mc->vatNode->fe)->fileType == FT_VAT200 )
    {
        char *txt = "2.00 VAT File Entry";
        VERBOSE00(uctout, "\tUDF %s\n", txt );

        /* UDF 2.00+, inspect when change in MAX_UDFREVISION
         */
        vatIntroRevision = 0x200;               /* UDF 2.00+ */
        vatMaxRevision   = MAX_UDFREVISION;     /* inspect */
    }
    else
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
          "\tError: No VAT File Entry for Virtual Partition\n");
      MLIMITend;
      nodeFreeHierarchy(mc->vatNode);
      mc->vatNode = NULL;
      return FALSE;
    }
    vPmi->vatRec.vatIntroRevision = vatIntroRevision;

    /* It is unusual for a VAT FE to have EAs or a stream directory.
     * For UDF 1.50 there may be an embedded "*UDF VAT LVExtension" EA,
     * see UDF 1.50 errata.
     * TODO: Change UDF spec ?? No EA File and no stream dir allowed.
     */
    if( *(pFE_lengthOfExtendedAttributes(mc->vatNode->fe)) != 0 ) /* note */
    { MLIMITbegin(INFO01level,uctMessageLimit);
        fprintf(uctout,
            "\tNote: Embedded Extended Attributes in VAT FE.\n");
      MLIMITend;
    }
    if( mc->vatNode->eaFileNode != NULL )                   /* warning */
    { MLIMITbegin(WARN01level,uctMessageLimit);
        fprintf(uctout,
          "\tWarning: Extended Attribute file defined in VAT FE.\n"
               "-\t\t This is unusual and may cause "
                                        "other verifier errors.\n");
      MLIMITend;
    }
    if( mc->vatNode->streamDirNode != NULL )                /* error */
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
          "\tError: Stream Directory defined in VAT EFE.\n"
         "-\t       This is unusual and may cause "
                                        "other verifier errors.\n");
      MLIMITend;
    }

    /* modifyUdfRevisionRange() is already called by
     * swapAndVerifyDescriptor(), but swapAndVerifyDescriptor()
     * may not return FALSE in case of a conflict, because an
     * inactive VAT file may be in a directory hierarchy.
     */
    if(    getUctMaxUdfRevision() < vatIntroRevision
        || getUctMinUdfRevision() > vatMaxRevision )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
          "\tError: Abort because of VAT revision conflict\n");
      MLIMITend;
      return FALSE;
    }

    bytesRead  = nodeGetEndOfFile(mc->vatNode, mc, TRUE);
    fileSize32 = (Uint32) bytesRead;
    if( ((Uint64) fileSize32) != bytesRead )
    {
        return FALSE;                   /* TODO: message ?? */
    }

    if( (vatFile = (Byte *) tst_malloc(fileSize32,
                                __FILE__, __LINE__)) == NULL )
    {   nodeFreeHierarchy(mc->vatNode);
        mc->vatNode = NULL;
        uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }
    vPmi->vatRec.vatFile = vatFile;

    ifPRINTinfo02(uctout, "\n\tRead VAT File\n" );
    ENDif;

    if( !THERead(mc, mc->vatNode, 0, (Uint64) fileSize32,
                 vatFile, &bytesRead) )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout, "-\tread VAT file error: THERead failed\n");
      MLIMITend;
      nodeFreeHierarchy(mc->vatNode);
      mc->vatNode = NULL;
      checkFree((void**)&vPmi->vatRec.vatFile);
      return FALSE;
    }
    else if( bytesRead != (Uint64) fileSize32 )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
            "-\tread VAT file error: THERead, not enough bytes:"
                        " %lu, expected: %lu\n",
                        bytesRead, fileSize32 );
      MLIMITend;
      nodeFreeHierarchy(mc->vatNode);
      mc->vatNode = NULL;
      checkFree((void**)&vPmi->vatRec.vatFile);
      return FALSE;
    }

    /* fake read file tail, if any.
     */
    (void) doFileTailFakeRead(mc, mc->vatNode,
                              (Uint64) fileSize32);

    if( !swapAndVerifyVatFile(vPmi, fileSize32) )
    {   vPmi->actualPartitionLength = 0;
        return FALSE;
    }
    vPmi->actualPartitionLength = vPmi->vatRec.numberOfEntries;

    return vatFabricateBitmap(mc, virtualPRef, vPmi);

}   /* end readVAT() */

/* cmpMetadataFilesUsingBitmap():
 * Compare blocks in Metadata File and its mirror.
 * Only compare blocks that are marked as allocated
 * in the bitmap (UDF 2.50 errata DCN-5103).
 * If bitmap == NULL, then compare all blocks.
 *
 * Return value: FALSE if compare could not be completed
 *         else: TRUE
 * Note:
 *  CMPMETA_MAXBUFSIZE need not be mult of blocksize because
 *  allocBlockBuffer() rounds up to mult of blocksize but
 *  CMPMETA_MAXBUFSIZE must be at least equal to blockSize.
 */
#define CMPMETA_MAXBUFSIZE  (8*1024*1024)   /* 2 * 8 Mb */

static bool cmpMetadataFilesUsingBitmap(UdfMountContext *mc,
                                        BitVector *pBitmap )
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32       blockSize = vmi->blockSize,
                 size32, chunkSz32,
                 n, nBlocks, bufBlocks,
                 blocksThisTime, startBitUsed, vecUnits;
    Byte        *p1, *p2, *buf1, *buf2;

    /* Return FALSE, but do not complain further if both or one
     * of the Metadata (Mirror) Files is not present or not read
     * correctly, those problems have been reported already.
     */
    if(   mc->metadataFile           == NULL
       || mc->metadataFile->node      == NULL
       || mc->metadataFile->node->fe   == NULL
       || mc->metadataFile->node->al    == NULL
       || mc->metadataMirrorFile         == NULL
       || mc->metadataMirrorFile->node    == NULL
       || mc->metadataMirrorFile->node->fe == NULL
       || mc->metadataMirrorFile->node->al == NULL )
    {   return FALSE;       /* compare not completed */
    }

    /* compare file data of allocated blocks (bit reset in bitmap)
     * in metadataFile and metadataMirrorFile.
     * Do not compare more than the shortest one,
     * rounded down to a multiple of the blocksize.
     */
    size32 = ROUNDDOWNMULT( MIN(mc->metadataFile->size32,
                                mc->metadataMirrorFile->size32),
                            blockSize );
    nBlocks = size32 / blockSize;

    if(    mc->metadataFile->size32
        != mc->metadataMirrorFile->size32 )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        VERBOSE00(uctout,
           "\tError: Metadata File and Metadata Mirror"
                                " File sizes: %lu, %lu\n"
          "-\t       Compared size: %lu\n",
            mc->metadataFile->size32,
            mc->metadataMirrorFile->size32, size32 );
      MLIMITend;
      /** no FALSE result, compare process ok **/
    }

    /* size32 is a multiple of the blocksize.
     * Prefered size for both meta and mirror
     * buffers: they MUST be equal in size.
     * allocBlockBuffer() may reduce chunkSz32
     * if not enough memory is available.
     * allocBlockBuffer() always does roundup
     * to a multiple of the blocksize.
     */
    chunkSz32 = MIN(size32, CMPMETA_MAXBUFSIZE);
    buf1  = allocBlockBuffer(chunkSz32, &chunkSz32);
    n     = chunkSz32;      /* temp */
    buf2  = allocBlockBuffer(chunkSz32, &chunkSz32);
    UCTASSERT( chunkSz32 <= n );
    if( chunkSz32 != n )    /* reallocate buf1 */
    {                       /* reduce buf1 */
        buf1 = (Byte*) tst_realloc( buf1, chunkSz32,
                                __FILE__,__LINE__);
    }
    if( buf1 == NULL || buf2 == NULL )
    {   checkFree((void**)&buf1);
        checkFree((void**)&buf2);
        return FALSE;       /* compare not completed */
    }
    bufBlocks = chunkSz32 / blockSize;

    /* Bitmap by default from verifier calculated bitmap,
     * else: Metadata Bitmap File, else: NULL.
     * Compare all blocks if pBitmap == NULL.
     * Do a simple block by block compare, but read a contiguous
     * number of blocks as denoted by the bitmap.
     * Because the blocks of the Metadata File and
     * its mirror are recorded 'far apart', the order
     * of reading is changed for each chunk read,
     * reducing the number of seeks by 50 %.
     */
    p1 = buf1;      /* not really needed */
    p2 = buf2;      /* don't know which file */
    startBitUsed = blocksThisTime = 0;
    vecUnits = ROUNDUPELEMENTS(nBlocks, 8 * sizeof(BitVector));

    for( n = 0; n < nBlocks; n = startBitUsed + blocksThisTime )
    { Uint64 bytesRead64, off64;
      Uint32 x, cnt;

      if( pBitmap == NULL )     /* No bitmap found, read and check all */
      { blocksThisTime = nBlocks - n;  /* nmb of chunk blocks */
        startBitUsed   = n;            /* start at n */
      }
      else                      /* check bitmap */
      { /* Find range of contiguous blocks for which bitmap
         * shows that blocks are in use.
         * getNextBitRange() shows the range that is NOT used, so
         * we have to calulate the unused part from the result.
         */
        Uint32 startUnused = n,
               lenUnused = getNextBitRange(pBitmap, vecUnits, &startUnused);
        if(   lenUnused == 0            /* remainder is used */
           || startUnused >= nBlocks )  /* beyond bitmap */
        { lenUnused = 0;
          startUnused = nBlocks;        /* (> n) */
        }
        if( startUnused == n )          /* block n unused, proceed */
        { blocksThisTime = 0;           /* nothing to read this time */
          startBitUsed   = n + lenUnused;    /* skip unused blocks */
          UCTASSERT(startBitUsed > n);
        }
        else            /* startUnused != n, block n used, read */
        { UCTASSERT(startUnused > n);
          blocksThisTime = startUnused - n;  /* nmb of chunk blocks */
          startBitUsed   = n;                /* start at n */
        }
      }
      /* Reduce read chunks for limited buf size and assert
       */
      blocksThisTime = MIN(blocksThisTime, bufBlocks);
      UCTASSERT(   (blocksThisTime > 0 && startBitUsed == n)
                || (blocksThisTime == 0 && startBitUsed > n));

      if( blocksThisTime > 0 )
      { /* read and compare 'blocksThisTime' blocks,
         * starting at block n
         */
        static int alternate = 0;
        Node      *firstNode, *secndNode;
        if( (alternate++) % 2 == 0 )
        { firstNode = mc->metadataFile->node;
          secndNode = mc->metadataMirrorFile->node;
        }
        else
        { firstNode = mc->metadataMirrorFile->node;
          secndNode = mc->metadataFile->node;
        }
        off64 = (Uint64) startBitUsed * blockSize;
        chunkSz32 = blocksThisTime * blockSize;
        /* note that buf1 and buf2 do not matter, they can be
         * used for one file and the next time for another file.
         */
        if(   !THERead(         mc, firstNode, off64,
                       (Uint64) chunkSz32,
                                buf1, &bytesRead64 )
           ||  bytesRead64 != chunkSz32
           || !THERead(         mc, secndNode, off64,
                       (Uint64) chunkSz32,
                                buf2, &bytesRead64 )
           ||  bytesRead64 != chunkSz32 )
        {
          MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
              "-\tread error: Abort comparing Metadata Files\n");
          MLIMITend;
          checkFree((void**)&buf1);
          checkFree((void**)&buf2);
          return FALSE;       /* compare not completed */
        }

        /* which file corresponds with buf1 and buf2
         * respectively is don't care here
         */
        for( x = 0, p1  = buf1,      p2  = buf2, cnt = 0;
             x < blocksThisTime;
             x++,   p1 += blockSize, p2 += blockSize )
        { /* Compare allocated blocks referenced by p1 and p2.
           */
          int cmpResult = memcmp(p1, p2, blockSize);
          if( cmpResult != 0 )        /* unequal */
          { cnt++;                    /* count unequal range */
            if( (x + 1) == blocksThisTime )    /* last block */
            { cmpResult = 0;          /* take care that last */
              x++;               /* unequal range is printed */
            }
          }
          if( cmpResult == 0 && cnt != 0 )
          { /* equal, print preceding not-equal range if any
             */
            MLIMITbegin(ERROR00level,uctMessageLimit);
              VERBOSE00(uctout,
                 "\tError: Metadata File and Metadata "
                                "Mirror File difference,\n"
                "-\t       Metadata Partition block%s %lu",
                        PLURAL_S(cnt), startBitUsed + x - cnt);
              if( cnt > 1 )
              { VERBOSE00(uctout, " thru %lu", startBitUsed + x - 1);
              }
              VERBOSE00(uctout, ", UDF 2.2.13%s.\n",
                (getUctUdfRevision()<=0x250)
                  ? ",\n-\t       UDF 2.50 errata DCN-5103"
                  : "");
            MLIMITend;
            cnt = 0;            /* unequal block range printed */
            /** no FALSE result, compare process ok **/
          }
        }        /* endfor x */
        UCTASSERT( cnt == 0 );
      }       /* endif read and compare 'blocksThisTime' blocks */
   }        /* endfor n */

    checkFree((void**)&buf1);
    checkFree((void**)&buf2);

    return TRUE;        /* compare completed */

}   /* end cmpMetadataFilesUsingBitmap() */

/* cmpMetadataFiles():
 * Compare blocks in Metadata File and its mirror.
 * Determine which bitmap to use and call
 * cmpMetadataFilesUsingBitmap().
 *
 * By default, the bitmap from the Metadata Bitmap File is used.
 * If not present (e.g. for read-only partition),
 * then the bitmap created by the verifier is used.
 * Because of the latter, cmpMetadataFiles() can not be
 * executed before the verifier has created the bitmap.
 */
extern bool cmpMetadataFiles(UdfMountContext *mc)
{
    PartitionMapInfo         *pmi;
    MetadataPartitionMapTail *mTail;
    BitVector                *pBitmap = NULL;

    if( !IS_PREF_PARTITION_FOUND(mc->metadataPref) )
    {   return TRUE;        /* no metadata partition exists */
    }
    pmi   = &mc->partitionMapInfo[mc->metadataPref];
    mTail = &pmi->pPartitionMap->type2PartitionMap.SharedTail.metadataTail;

    /* no compare if metadata is not duplicated
     */
    if( (mTail->flags & MP_DUPMETADATA_MASK) == 0 )
    {   return TRUE;    /* Duplicate Metadata Flag not set */
    }

    /* By default use verifier created bitmap, else use bitmap
     * of the Metadata Bitmap File (if present).
     */
    pBitmap = pmi->pPartAlloc;  /* verifier created bitmap */
    if( pBitmap == NULL )       /* no bitmap or out of memory */
    { SpaceBitmapDescriptor *sbd = pmi->pss.unallocatedSpaceBitmap;
      if( sbd != NULL )
      { pBitmap = (BitVector*) &sbd->startOfBitmap;
      }
    }

    VERBOSE00(uctout,
        "\n====>\tCompare duplicated Metadata File to its mirror.\n");
    fflush(uctout);

    if( !cmpMetadataFilesUsingBitmap(mc, pBitmap) )
    {
      MLIMITbegin(WARN01level, uctMessageLimit);
        VERBOSE00(uctout,
          "\tWarning: Compare of duplicated Metadata File failed.\n");
      MLIMITend;
      return FALSE;
    }
    return TRUE;

}   /* end cmpMetadataFiles() */


/* readMetadataSpecialFile: Introduced in UDF 2.50.
 * Read one of the Metadata Partition special files.
 * metaFile fileType may be one of:
 * - FT_METADATA_FILE           fake read of data
 * - FT_METADATA_MIRROR_FILE    fake read of data
 * - FT_METADATA_BITMAP_FILE    real read of data
 *
 * Under all circumstances (e.g. return FALSE),
 * the metaFile FileNodeInfo structure is kept consistent
 * so that it can later be removed using freeFileNodeInfo();
 * In case of file data read errors
 *  and for fake read: metaFile->data == NULL.
 */
static bool readMetadataSpecialFile(UdfMountContext *mc,
                                    Uint8            fileType,
                                    Uint16           metadataPRef,
                                    FileNodeInfo    *metaFile)
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32            blockSize = vmi->blockSize;
    PartitionMapInfo *mPmi = &mc->partitionMapInfo[metadataPRef];
    Uint16            counterpartPRef = mPmi->counterpartPRef;
    Uint32            fileSize32, dummy01, dummy02;
    Uint64            bytesRead;
    char             *fName;
    LongAd           *pICB;
    Node             *pNode;
    MetadataPartitionMapTail *mTail =
        &mPmi->pPartitionMap->type2PartitionMap.SharedTail.metadataTail;

    /* Metadata partition present.
     */
    UCTASSERT( counterpartPRef != metadataPRef );

    /* Read File Entry first,
     * set all out parameters to 'empty' value.
     * create Node for file.
     */
    if( (metaFile->node = NEWNODE()) == NULL )
    {   return FALSE;
    }
    metaFile->node->fePartRef = counterpartPRef;
    pICB  = &metaFile->icb;
    pNode =  metaFile->node;

    /* Set values for ICB
     */
    pICB->extentLocation.partitionReferenceNumber = counterpartPRef;
    pICB->extentLength = blockSize; /* recorded and allocated, strategy 4 */

    /* fileType specific things:
     */
    switch( fileType )
    {
      case FT_METADATA_FILE:
        fName = "Metadata File";
        pICB->extentLocation.logicalBlockNumber =
                                mTail->metadataFileLocation;
        NODEFLAGS_SET_BIT(pNode, NFB_METADATAFILE);
        break;
      case FT_METADATA_MIRROR_FILE:
        fName = "Metadata Mirror File";
        pICB->extentLocation.logicalBlockNumber =
                                mTail->metadataMirrorFileLocation;
        NODEFLAGS_SET_BIT(pNode, NFB_METADATAMIRROR);
        break;
      case FT_METADATA_BITMAP_FILE:
        fName = "Metadata Bitmap File";
        pICB->extentLocation.logicalBlockNumber =
                                mTail->metadataBitmapFileLocation;
        NODEFLAGS_SET_BIT(pNode, NFB_METADATABITMAP);
        break;
      default:
        return FALSE;       /* assert */
        /** break; **/
    }
    ifPRINTinfo02(uctout, "\n\tRead %s FE\n", fName );
    ENDif;

    /* Test address before actually reading because subsequent calls
     * to translateAddress() in readFileEntryEtc() may return FALSE
     * without (error/warning) message (silent).
     */
    if(   !translateAddress( mc, counterpartPRef,
                             pICB->extentLocation.logicalBlockNumber,
                            &dummy01, &dummy02, FALSE ) /* NOT silent */
       || !readFileEntryEtc(mc, pNode, pICB, FALSE, FALSE, 0) )
    {
      MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
            "-\tError: readFileEntryEtc failed for %s, block %lu (p%u: %lu)\n",
                fName, mPmi->pdPointer->partitionStartingLocation +
                        pICB->extentLocation.logicalBlockNumber,
                        pICB->extentLocation.partitionReferenceNumber,
                        pICB->extentLocation.logicalBlockNumber);
      MLIMITend;
      return FALSE;     /* pNode->fe == NULL */
    }

    VERBOSE00(uctout, "\n\t%s Modification Time: ", fName);
    printTimestampShort(pFE_modificationTime(pNode->fe), TRUE,"\n");

    if(   pNode->streamDirNode != NULL
       || pNode->eaFileNode != NULL
/**out || *(pFE_lengthOfExtendedAttributes(pNode->fe)) != 0 ** TODO: check **/
/**TODO: show text denoting that the mirror file is used !!!!!
 **/
      )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
           "\tError: %s Extended Attribute or Stream Directory ignored,\n"
          "-\t       UDF 2.2.13.1.\n", fName);
      MLIMITend;
    }

    { Uint8 ft = pFE_icbTag(pNode->fe)->fileType;
      if( ft != fileType )
      { MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
            "\tError: %s fileType: %u, expected: %lu, UDF 2.3.5.2.\n",
                        fName, ft, fileType );
        MLIMITend;
//??    return FALSE;       /* TODO: fatal ?? */
      }
    }

    bytesRead  = nodeGetEndOfFile(pNode, mc, TRUE);
    fileSize32 = (Uint32) bytesRead;
    if( ((Uint64) fileSize32) != bytesRead )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout, "\tError: Verifier cannot handle"
                        " %s with a size larger than %lu.\n",
                fName, ROUNDDOWNMULT(MAX_UINT32, blockSize));
      MLIMITend;
      return FALSE;         /* fatal */
    }
    metaFile->size32 = fileSize32;  /* export */

    /* assign buffer for 'real read'.
     */
    switch( fileType )
    { case FT_METADATA_FILE:
      case FT_METADATA_MIRROR_FILE:
        metaFile->data = NULL;          /* fake read */
        break;
      case FT_METADATA_BITMAP_FILE:     /* real read */
        if( (metaFile->data = (Byte *) tst_malloc(fileSize32,
                                    __FILE__, __LINE__)) == NULL )
        {   return FALSE;           /* fatal */
        }
        break;
    }

    /* Now read the data,
     * fake read for Metadata File and its mirror,
     * real read for Metadata Bitmap File
     */
    ifPRINTinfo02(uctout, "\n\t%s %s data\n",
        (metaFile->data == NULL) ? "Fake read"
                                 : "Read",
        fName );
    ENDif;

    if( !THERead(mc, pNode, 0, (Uint64) fileSize32,
                 metaFile->data, &bytesRead) )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout, "-\tread %serror: THERead failed\n", fName);
      MLIMITend;
      checkFree((void**)&metaFile->data);   /* = NULL */
      return FALSE;     /* TODO: fatal ?? only cannot verify bitmap */
    }
    else if( bytesRead != (Uint64) fileSize32 )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
            "-\tread %serror: THERead, not enough bytes:"
                        " %lu, expected: %lu\n",
                    fName, bytesRead, fileSize32 );
      MLIMITend;
      checkFree((void**)&metaFile->data);   /* = NULL */
      return FALSE;     /* TODO: fatal ?? only cannot verify bitmap */
    }

    /* fake read file tail, if any.
     */
    (void) doFileTailFakeRead(mc, pNode,
                              (Uint64) fileSize32);
    return TRUE;

}   /* end readMetadataSpecialFile() */

/* read Metadata (Mirror) File(s): Introduced in UDF 2.50.
 */
extern bool readMetadataFiles( UdfMountContext *mc,
                               Uint16 metadataPRef )
{
    const MediumInfo *vmi = getTheMediumInfo();
    PartitionMapInfo *pmi = &mc->partitionMapInfo[metadataPRef];
    MetadataPartitionMapTail *mTail =
        &pmi->pPartitionMap->type2PartitionMap.SharedTail.metadataTail;
    FileNodeInfo     *metadataMount;
    bool              doSwap, metaOk, mirrorOk, dupFlagSet;

    dupFlagSet = ((mTail->flags & MP_DUPMETADATA_MASK) != 0);

    VERBOSE00(uctout,
      "  ==>\tMetadata Partition found\n"
           "\t  Allocation Unit Size: %lu (blocks)\n"
           "\t  Alignment Unit Size : %lu (blocks)\n"
           "\t  Duplicate Metadata Flag %sset\n",
        mTail->allocationUnitSize, mTail->alignmentUnitSize,
        dupFlagSet ? "" : "not ");

    if(   (mc->metadataFile = NEWSTRUCT(FileNodeInfo,1))
           == NULL
       || (mc->metadataMirrorFile = NEWSTRUCT(FileNodeInfo,1))
           == NULL )
    {   return FALSE;
    }

    /* Read metadataFile and metadataMirroFile, File Entry first.
     * Set LongAd mc->metadata*File->icb. It will be used for
     * tests later like File Link Count, UniquID, etc.
     * One of mc->metadata*File->al will be used for
     * translateMetadataAddress().
     * Fake read for file data. Maybe actual data is read in
     * cmpMetadataFiles() if duplicate flag set.
     *
     * Read Metadata Bitmap File later.
     */
    (void) readMetadataSpecialFile( mc, FT_METADATA_FILE,
                                    metadataPRef,
                                    mc->metadataFile );
    (void) readMetadataSpecialFile( mc, FT_METADATA_MIRROR_FILE,
                                    metadataPRef,
                                    mc->metadataMirrorFile );
    /* void FALSE returns,
     * possible errors can be:
     *  mc->metadata*File->node     == NULL for out of memory,
     *  mc->metadata*File->node->fe == NULL for FE read error,
     *  mc->metadata*File->node->al == NULL for AD error
     *  mc->metadata*File->size32   == 0    for file too big
     *  more ??
     */

#ifdef UCT_TESTING_SWAP77   /* swap metadataFile and metadataMirrorFile */
    { /** swap trick 77 **/
        FileNodeInfo *xx = mc->metadataFile;
        mc->metadataFile = mc->metadataMirrorFile;
        mc->metadataMirrorFile = xx;
    }
#endif

    /* swap mc->metadataFile and mc->metadataMirrorFile
     * in case the mirror file has less errors.
     * Both mc->metadataFile
     *  and mc->metadataMirrorFile are != NULL
     * TODO: check more ?? maybe 'merge' files ??
     */
    metaOk =   (   mc->metadataFile->node     != NULL
                && mc->metadataFile->node->fe != NULL
                && mc->metadataFile->node->al != NULL
                && mc->metadataFile->size32   != 0 );

    mirrorOk = (   mc->metadataMirrorFile->node     != NULL
                && mc->metadataMirrorFile->node->fe != NULL
                && mc->metadataMirrorFile->node->al != NULL
                && mc->metadataMirrorFile->size32   != 0 );

    /* check if metadata and mirror FE,AED, etc.
     * are recorded physically far apart.
     */
    if( metaOk && mirrorOk )
    {   (void) checkMetadataFarApart(mc, dupFlagSet);
    }

    doSwap = ( mirrorOk && !metaOk );   /* default */

    if( doSwap )
    { FileNodeInfo *xx = mc->metadataFile;
      mc->metadataFile = mc->metadataMirrorFile;
      mc->metadataMirrorFile = xx;

      MLIMITbegin(WARN01level,uctMessageLimit);
        VERBOSE00(uctout,
          "  ==>\tWarning: Using Metadata Mirror File instead"
                                            " of Metadata File\n"
                     "-\t\t because of Metadata File error.\n"
              "-\tNote: From now, all Metadata File errors/warnings\n"
              "-\t        are in fact Metadata Mirror File errors/warnings !!\n");
      MLIMITend;
    }

    /* final test and 'install' of Metadata (Mirror) File as
     * Metadata Partition.
     */
    if(   mc->metadataFile         == NULL
       || mc->metadataFile->node    == NULL
       || mc->metadataFile->node->fe == NULL
       || mc->metadataFile->node->al == NULL
       || mc->metadataFile->size32   == 0 )
    {
      MLIMITbegin(ERROR00level,uctMessageLimit);
        VERBOSE00(uctout,
          "  ==>\tSerious error: Metadata File and Metadata Mirror File error.\n");
      MLIMITend;
      return FALSE;     /* error */
    }

    /* At least mc->metadataFile does now contain a
     * correct Metadata (Mirror) File reference.
     * Maybe ignore -usemirror option
     */
    if( uctUseMetadataMirror && !mirrorOk ) /* no swap, no usemirror */
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        VERBOSE00(uctout,
          "  ==>\tError: -usemirror option ignored because"
                                        " Metadata Mirror File\n"
              "-\t       could not be read correctly or is empty.\n");
      MLIMITend;
      uctUseMetadataMirror = FALSE;     /* ignore */
    }

    /* TRUE: if( uctUseMetadataMirror == TRUE )
     *       then: (   doSwap   == FALSE
     *              && metaOk   == TRUE
     *              && mirrorOk == TRUE )
     * Decide which file to mount.
     */
    if( uctUseMetadataMirror )
    {
      MLIMITbegin(WARN01level,uctMessageLimit);
        VERBOSE00(uctout,
          "\n  ==>\tWarning: Using Metadata Mirror File"
                                " instead of Metadata File\n"
                      "-\t\t because of -usemirror option.\n");
      MLIMITend;
      metadataMount = mc->metadataMirrorFile;   /* -usemirror */
    }
    else
    { metadataMount = mc->metadataFile;         /* default */
    }

    /* Do the actual 'mount'.
     * Mount Metadata File or its mirror as Metadatada Partition:
     *  - Update the actualPartitionLength and set the metadataFile
     *    allocation list (used for translateMetadataAddress()).
     *
     * verifyMetadataFilesAllocation() will be called in
     * checkAndMapPartitionInfo() after 'Mounted Partitions:'
     * have been shown.
     */
    pmi->actualPartitionLength =
            ROUNDUPELEMENTS(metadataMount->size32, vmi->blockSize);
    pmi->metadataRec.pMetadataFileAllocList = metadataMount->node->al;

    return TRUE;

}   /* end readMetadataFiles() */


/* getMetadataBitmapFileSBDtagLocation():
 * pre-condition:
 *  preceding call of readMetadataBitmapFile()
 *  returned TRUE.
 *  (meaning mc->metadataBitmapFile != NULL, etc.
 */
extern Uint32 getMetadataBitmapFileSBDtagLocation(
                                UdfMountContext *mc)
{
    Node  *node   = mc->metadataBitmapFile->node;
    Uint8  adType = GET_ADTYPE(pFE_icbTag(node->fe)->flags);
    Uint32 tagLocation;

    if( adType == ADT_INFE )    /* embedded */
    { return mc->metadataBitmapFile->icb.extentLocation.logicalBlockNumber;
    }

    /* not embedded
     */
    if( !nodeFindLogicalBlock( node, (Uint64) 0,
                              &tagLocation) )
    { return MAX_UINT32;        /* error, assert failure */
    }
    return tagLocation;
}

/* readMetadataBitmapFile:
 * read Metadata Bitmap File (introduced in UDF 2.50).
 * Also some metadata bitmap specific tests.
 * Returns value: pointer to SBD, NULL on failure.
 *  further: file size in mc->metadataBitmapFile->size32
 *           and the SBD size in (*pSBDsize).
 */
extern SpaceBitmapDescriptor *readMetadataBitmapFile(
                                    UdfMountContext *mc,
                                    Uint16           metadataPRef,
                                    Uint32          *pSBDsize )
{   Uint16                  tagId;
    Uint32                  mdTagLoc;
    Uint64                 *pFeInfLen64;
    SpaceBitmapDescriptor  *pSbd;       /* result */

    mc->metadataBitmapFile = NEWSTRUCT(FileNodeInfo,1);
    (*pSBDsize) = 0;

    /* Read the MetadataFile information in memory, File Entry first.
     * Set LongAd metadata*ICB. It will be used for tests later like
     * File Link Count, UniquID, etc.
     */
    if( !readMetadataSpecialFile(mc, FT_METADATA_BITMAP_FILE,
                                 metadataPRef, mc->metadataBitmapFile) )
    { VERBOSE00(uctout,"-\tError reading Metadata Bitmap File\n");
      checkFree((void**)&mc->metadataBitmapFile->data);
      return NULL;          /* SBD fatal error */
    }

    /* File data (SBD) in mc->metadataBitmapFile->data
     * verify SBD descriptor and get length for extra tests
     */
    mdTagLoc = getMetadataBitmapFileSBDtagLocation(mc);
    if(   !swapAndVerifyDescriptor( mc->metadataBitmapFile->data,
                                    mc->metadataBitmapFile->size32,
                                    tidSBD, &tagId, mdTagLoc,
                                    mc, mc->metadataBitmapFile->node )
       || !getLengthOfDescriptor( mc->metadataBitmapFile->data,
                                  TRUE, pSBDsize ) )    /* isSwapped */
    { VERBOSE00(uctout,"-\tSerious error in Metadata Bitmap\n");
      checkFree((void**)&mc->metadataBitmapFile->data);
      return NULL;          /* SBD fatal error */
    }
    pFeInfLen64 = pFE_informationLength(mc->metadataBitmapFile->node->fe);

    /* check SBD descriptor length with Metadata Bitmap File size and
     * FE Information Length, UDF 2.2.13.2, 2.2.13.5+6, ECMA 4/14.12.3.
     */
    if( (Uint64) (*pSBDsize) != (*pFeInfLen64) )
    {  MLIMITbegin( ERROR00level, uctMessageLimit );
        printMessageHead( MLIMIT.vl, mc->metadataBitmapFile->node->fe,
                (Byte*) pFeInfLen64,
                "Error: Information Length: ");
        printUint64(MLIMIT.vl, (*pFeInfLen64), FALSE, "%7lu");
        fprintf(uctout, ", expected: %7lu\n"
                "-\t\t\tMetadata Bitmap SBD descriptor size  : %7lu\n",
                    (*pSBDsize), (*pSBDsize));
        if( (Uint64) mc->metadataBitmapFile->size32 != (*pFeInfLen64) )
        { fprintf(uctout,
                "-\t\t\tMetadata Bitmap File File size       : %7lu\n",
                mc->metadataBitmapFile->size32);
        }
        fprintf(uctout,
          "-\t\t The Metadata Bitmap File (E)FE Information Length shall\n"
          "-\t\t equal the size of the Metadata Bitmap SBD descriptor,\n"
          "-\t\t UDF 2.2.13.2, 2.2.13.5+6, ECMA 4/14.12.3.\n");
      MLIMITend;
    }
    if( (*pSBDsize) > mc->metadataBitmapFile->size32 )
    { /* TODO: maybe test not needed, because other tests above failed already
       */
      VERBOSE00(uctout,"-\tNot enough bytes read for Metadata Bitmap SBD, serious error.\n");
      checkFree((void**)&mc->metadataBitmapFile->data);
      return NULL;          /* SBD fatal error */
    }

    /* Pointer to file data will be returned as resulting pointer to SBD, so
     * then there are two pointers, each pointing to the same malloced space.
     * In order to avoid problems when freeing the space, clear the
     * mc->metadataBitmapFile->data pointer.
     */
    pSbd = (SpaceBitmapDescriptor*) mc->metadataBitmapFile->data;
    mc->metadataBitmapFile->data = NULL;

    return pSbd;        /* no fatal error */

}   /* end readMetadataBitmapFile() */


/* checkParentFid:
 * Check parent FID/FE consistency and ICB back reference
 * for the parent FID in node.
 */
static bool checkParentFid( UdfMountContext *mc, Node *node,
                            FileIdentifierDescriptor *pFid )
{
    Node   *dirNode    = node->parent,
           *parentNode = dirNode->parent;
    LongAd *parentICB;
    bool    result = TRUE;

    UCTASSERT( parentNode != NULL );

    /* verify FID/FE consistency.
     * For a parent FID, the (E)FE is read before the FID.
     */
    (void) verifyFidFeConsistency(node, mc);

    /* Check parent FID ICB field.
     * 1) The parent of the rootNode is the rootNode, its
     *    ICB address is in the FSD rootDirectoryICB field.
     * 2) The parent of the system stream directory is the
     *    system stream directory, its ICB address
     *    is in the FSD systemStreamDirectoryICB field.
     * 3) For any other directory or stream directory,
     *    the ICB address of its parent is in
     *    dirNode->parent->fid->ICB.
     *
     * assert:
     */
    if( (parentICB = nodeGetIcb(parentNode, mc)) == NULL )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageHead( MLIMIT.vl, (Byte*) pFid, NULL,
          "Error: Undefined parent FID ICB."
            " Unable to verify, please report.\n");
          nodePrintUnicodeNameContLine(node,mc);
      MLIMITend;
      return FALSE;
    }

    /* Check if parent FID ICB correctly identifies the parent.
     * For this check do not compare UDF 2.00+ UDFUniqueID in FID ICB
     */
    if( memcmp(&pFid->ICB.extentLocation,
               &parentICB->extentLocation, sizeof(LBAddr))
          != 0
       ||    pFid->ICB.extentLength != parentICB->extentLength
       ||    pFid->ICB.implementationUse.flags
          != parentICB->implementationUse.flags )
    {
      MLIMITbegin(ERROR00level,uctMessageLimit);
        printMessageHead( MLIMIT.vl, (Byte*) pFid,
                            (Byte*) &pFid->ICB, NULL);
        fprintf(uctout,
          "Error: Parent FID ICB does not correctly"
                                            " identify the\n"
          "-\t\t\tparent directory%s, ECMA 4/8.6, UDF 2.3.4\n",
                NODEFLAGS_IS_SET(dirNode, NFB_STREAMDIR)
                                ? " or file" : "");
        if(   pFid->ICB.implementationUse.flags
          != parentICB->implementationUse.flags )
        { fprintf(uctout,
            "-\t\t\tADImpUse flags differs, UDF 2.3.10.1\n");
        }
        nodePrintUnicodeNameContLine(node, mc);
        fprintf(uctout,
            "-\t=======> parent FID ICB:\n");
        printLongAd( &pFid->ICB, TRUE );    /* isFidIcb */
        fprintf(uctout,
            "-\t=======> parent to be identified:\n");
        printLongAd( parentICB, TRUE );     /* isFidIcb */
      MLIMITend;
      result = FALSE;
    }
    return result;

}   /* end checkParentFid() */

/* nodeGetFIDs:
 * Read and verify FIDs, add node for each FID
 * to dirNode children list.
 *
 * return result: FALSE for a fatal error, else TRUE
 *
 * Note: dirNode->nrOfChildren will hold the number of FIDs
 *       found, also in the case of a FALSE return result.
 */
static bool nodeGetFIDs(UdfMountContext *mc, Node *dirNode)
{
    Byte             *bpFid = NULL; /* byte pointer */
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32   blockSize    = vmi->blockSize;
    Byte    *blockBase;
    Uint32   expectedTagLocation = 0;
    Uint32   lengthOfFID = 0,
             minLengthOfFID, dSize;
    Uint32   totalLengthOfFids, lengthOfDirectory;
    Uint64   tmp64;
    Uint32   nrBytes;
    Uint32   nrOfBlocks;
    Uint32   countParentFids = 0;
    Uint8    feAdType = GET_ADTYPE(pFE_icbTag(dirNode->fe)->flags);
    Node    *child;
    bool     lenOk, fidsEmbedded,
             result         = TRUE,
             dirIsStreamDir = NODEFLAGS_IS_SET(dirNode, NFB_STREAMDIR),
             dirIsSystem    = NODEFLAGS_IS_SET(dirNode, NFB_SYSTEM);

    /* remove possible child nodes and reinitialize
     */
    nodeFreeChildren(dirNode);

    /* check if Information Length bytes can be read
     */
    tmp64 = nodeGetEndOfFile(dirNode, mc, TRUE);
    lengthOfDirectory = (Uint32) tmp64;

    /* ensure that directory file size fits in 32 bits
     * (do not handle bigger directories)
     */
    if( ((Uint64) lengthOfDirectory) != tmp64 )
    {
        MLIMITbegin(ERROR00level, uctMessageLimit);
            fprintf(uctout,
                "\tProgram error: Directory too big, byte size: ");
            printUint64(MLIMIT.vl, tmp64, FALSE, NULL);
            nodePrintUnicodeNameTxtExtra(dirNode, mc,
              ",\n-\t\t       skip directory: ","\n");
        MLIMITend;
        return FALSE;
    }

    fidsEmbedded = FALSE;
    if( feAdType == ADT_INFE )      /* FIDs embedded in (E)FE */
    {
        fidsEmbedded = TRUE;
        bpFid = pFE_startOfExtendedAttributes(dirNode->fe) +
             (*(pFE_lengthOfExtendedAttributes(dirNode->fe)));
        dirNode->fidsAllocation = NULL;
        blockBase = dirNode->fe;
    }
    else if( feAdType != ADT_SHORT && feAdType != ADT_LONG )
    {
        MLIMITbegin(ERROR00level,uctMessageLimit);
            fprintf(uctout,
                 "\tnodeGetFIDs error: Allocation Descriptor type: %u\n"
                "-\t\t Expected: %u or %u (short or long)\n",
                        feAdType, ADT_SHORT, ADT_LONG);
        MLIMITend;
        return FALSE;
    }
    else    /* ADT_SHORT or ADT_LONG */
    {
        ifPRINTinfo02(uctout, "\tRead FIDs\n");
        ENDif;
        if( dirNode->al == NULL ) /* ADT_INFE or nodeReadAllocationDescriptors() */
        {                         /* failed in readFileEntryEtc() */
            MLIMITbegin(ERROR00level,uctMessageLimit);
              fprintf(uctout,
                "\tnodeGetFIDs error: Failed to read"
                        " Allocation Descriptors for this directory\n");
            MLIMITend;
            return FALSE;
        }
        nrBytes    = lengthOfDirectory;
        nrOfBlocks = ROUNDUPELEMENTS(nrBytes, blockSize);

        /* allocate whole blocks for FIDs !!
         */
        if( (bpFid = (Byte *) tst_malloc( nrOfBlocks * blockSize,
                                          __FILE__,__LINE__))
            == NULL )
        {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
        }
        dirNode->fidsAllocation = blockBase = bpFid;

        if(    !THERead(mc, dirNode, 0, (Uint64) nrBytes, bpFid, &tmp64)
            ||  tmp64 != (Uint64) nrBytes )
        {
            MLIMITbegin(ERROR00level,uctMessageLimit);
              fprintf(uctout,
                "\tnodeGetFIDs error: THERead() failure\n");
            MLIMITend;
            return FALSE;
        }
        /* fake read file tail, if any.
         */
        (void) doFileTailFakeRead(mc, dirNode, (Uint64) lengthOfDirectory);
    }

    if( fidsEmbedded )
    {   printMessageHead(INFO02level, dirNode->fe, bpFid, NULL);
    }
    else
    {   ifPRINTinfo02(uctout, "\t");
        ENDif;
    }
    ifPRINTinfo02(uctout, "Verify%s FIDs, %lu byte%s\n",
        (fidsEmbedded) ? " embedded" : "",
        lengthOfDirectory, PLURAL_S(lengthOfDirectory));
    ENDif;

    /* swap and check all fids first !!
     */
    minLengthOfFID = ROUNDUPMULT(
                        offsetof(FileIdentifierDescriptor,
                                 startOfImplementationUse), 4);

    for( totalLengthOfFids = 0;                 /* fids length in bytes */
         totalLengthOfFids + minLengthOfFID <= lengthOfDirectory;
         totalLengthOfFids += lengthOfFID,
         bpFid             += lengthOfFID )
    {
        bool   isParentFID = FALSE;
        size_t tmp32;
        FileIdentifierDescriptor *pFid =
                (FileIdentifierDescriptor*) bpFid;

        /* For UDF 2.00 and higher write-once file systems:
         * Check if FID tag spans block boundary for write-once medium
         * The fact that the current FID is not swapped yet is not important.
         * We are only checking the FID position within a block and the
         * messages will show information from the blockBase structure
         * only (a previous FID or FE).
         */
        tmp32 = bpFid - blockBase;      /* offset */

        if(   getUctUdfRevision() >= 0x200
           && vmi->writabilityType == MTYPE_WR_WRITEONCE
           &&    (int)  (tmp32 / blockSize)
              != (int) ((tmp32 + sizeof(Tag) - 1) / blockSize) )
        {
            MLIMITbegin(ERROR00level, uctMessageLimit);
              printMessageHead( MLIMIT.vl, blockBase, bpFid,
                "Error: FID tag spans block boundary for write-once medium,\n"
                "-\t\t\tUDF 2.00+ 2.3.4.4\n");
            MLIMITend;
            /* no FALSE return result */
        }

        /* first simple inspect before creating node for this child.
         * MIND: no endian swap of FID yet !!
         */
        if( !inspectDescriptorHead(bpFid,
                                lengthOfDirectory - totalLengthOfFids,
                                blockSize, TRUE,
                                tidFID, NULL, &lengthOfFID) )
        {   MLIMITbegin(ERROR00level,uctMessageLimit);
                printAndClearUctErrorMessage("");
            MLIMITend;
            child = NULL;
            result = FALSE;
        }
        else        /* inspect FID tag ok */
        {
            if( totalLengthOfFids + lengthOfFID > lengthOfDirectory )
            {   break;      /* end of FID outside directory */
            }

            /* create child node for each FID,
             */
            if( (child = NEWNODE()) == NULL )
            {   return FALSE;
            }
            /* set nodeFlags, fileCharacteristics is Uint8,
             * so endian-swap independent.
             */
            child->parent = dirNode;
            child->fid    = pFid;
            isParentFID   = isBitOn(pFid->fileCharacteristics, FCB_PARENT);

            if( dirIsSystem )                   /* in SystemStreamDir */
            {   NODEFLAGS_SET_BIT(child, NFB_SYSTEM);   /*  hierarchy */
            }

            if( isParentFID )
            {   NODEFLAGS_SET_BIT(child, NFB_PARENTFID);
            }
            else if( dirIsStreamDir )
            {   NODEFLAGS_SET_BIT(child, NFB_STREAM);
            }

            if( adGetExtentSize(&child->fid->ICB) == 0 )
            {   NODEFLAGS_SET_BIT(child, NFB_UNUSED_FID);
            }
            else if( isBitOn(pFid->fileCharacteristics, FCB_DELETED) )
            {   NODEFLAGS_SET_BIT(child, NFB_MARKED_DELETE);
            }

            (void) nodeGetNameNoSwap(child);    /* get child unicodeName */

            /* Now swap and verify FID.
             * Calculate relative block number of first byte FID
             * for tagLocation check, fixed for ADT_INFE.
             */
            if( fidsEmbedded )
            {   LongAd *icb;
                if( (icb = nodeGetIcb(dirNode, mc)) == NULL )
                     result = FALSE;
                else if(   dirNode->fe != NULL
                        && pFE_icbTag(dirNode->fe)->strategyType == 4096 )
                     expectedTagLocation = pFE_descriptorTag(dirNode->fe)->tagLocation;
                else expectedTagLocation = icb->extentLocation.logicalBlockNumber;
            }
            else if( !nodeFindLogicalBlock(dirNode,
                                           (Uint64) totalLengthOfFids,
                                          &expectedTagLocation) )
            {   result = FALSE;
            }
            if( result != FALSE )
            {   result = swapAndVerifyDescriptor((Byte *) pFid,
                                lengthOfFID, tidFID,
                                NULL, expectedTagLocation, mc, child);
            }
        }
        if( result == FALSE )
        {   /* inspectDescriptorHead(), nodeFindLogicalBlock()
             * or swapAndVerifyDescriptor() failed
             */
            MLIMITbegin(ERROR00level,uctMessageLimit);
                printAndClearUctErrorMessage("");
                fprintf(uctout,
                    "  ==>\tError: FIDs processing out of sync.\n");
            MLIMITend;
            if( child != NULL ) nodeFreeHierarchy(child);
            break;          /* fid error, length mismatch too */
        }

        /* FID has been swapped and verified now
         * add child Node to dirNode child list
         */
        if( dirNode->lastChild == NULL )    /* no child yet */
             dirNode->firstChild = child;
        else dirNode->lastChild->nextInDirectory = child;
        dirNode->lastChild = child;
        dirNode->nrOfChildren++;

        /* check parent FID and its position
         */
        if( isParentFID )
        {   countParentFids++;
            /* check parent FID position
             */
            if( dirNode->nrOfChildren != 1 )    /* not first FID */
            {
                /* no FALSE return result */
                MLIMITbegin(ERROR00level,uctMessageLimit);
                  printMessageHead( MLIMIT.vl, bpFid,
                        &pFid->fileCharacteristics, NULL);
                  fprintf(uctout,
                    "Error: FID number %lu of directory is"
                                        " parent FID. Only\n"
                    "-\t\t\tfirst FID shall be parent FID,"
                                    " UDF 2.3.4, ECMA 4/8.6\n",
                        dirNode->nrOfChildren );
                  nodePrintUnicodeNameTxtExtra(dirNode,mc,
                    "-\t\t directory: ","\n");
                MLIMITend;
            }
            (void) checkParentFid(mc, child, pFid);
        }
        else if( dirNode->nrOfChildren == 1 ) /* first FID is no parent FID */
        {
            /* no FALSE return result */
            MLIMITbegin(ERROR00level,uctMessageLimit);
              printMessageHead( MLIMIT.vl, bpFid,
                &pFid->fileCharacteristics,
                "Error: First FID of directory is no parent FID,\n"
                "-\t\t\tUDF 2.3.4, ECMA 4/8.6\n" );
              nodePrintUnicodeNameTxtExtra(dirNode,mc,
                "-\tdirectory: ","\n");
            MLIMITend;
        }

        /* double check lengthOfFID
         */
        if(    (lenOk = getLengthOfDescriptor(bpFid, TRUE, &dSize))
                == FALSE
            || lengthOfFID != dSize )
        {   /* count as error, never suppress message
             */
            MLIMITbegin(ERROR00level,uctMessageLimit);  /* dummy */
            MLIMITend;
            fprintf(uctout, "\tFatal program error: length of FID inconsistency,"
                    " please report: %d %lu %lu\n", lenOk, lengthOfFID, dSize);
            fflush(uctout);
            uctExit(EXIT_PROGRAM_ERROR);        /* fatal */
        }
    }

    /* check if proper loop end condition
     */
    if(        totalLengthOfFids != lengthOfDirectory     /* mismatch */
        && (   totalLengthOfFids + minLengthOfFID > lengthOfDirectory
                    /* no break out of loop because of previous error */
            || totalLengthOfFids + lengthOfFID > lengthOfDirectory ) )
                      /* break out of loop because of length mismatch */
    {
        /* no FALSE return result */
        MLIMITbegin(ERROR00level, uctMessageLimit);
            fprintf(uctout,
                "\tError: FIDs total length mismatch: %lu, expected: %lu,\n",
                totalLengthOfFids, lengthOfDirectory);
            if( totalLengthOfFids + minLengthOfFID > lengthOfDirectory )
            {           /* no break out of loop because of previous error */
              fprintf(uctout, "-\t       minimum FID length: %lu\n",
                            minLengthOfFID);
            }
            else        /* break out of loop because of length mismatch */
            {
              fprintf(uctout, "-\t       last FID length: %lu\n",
                            lengthOfFID);
            }
            nodePrintUnicodeNameTxtExtra(dirNode, mc,
                              "-\t       directory: ","\n");
        MLIMITend;
    }

    /* if mismatch, then show nmb of FIDs and how many
     * FIDs maybe skipped (on average and at most)
     */
    if( totalLengthOfFids < lengthOfDirectory )
    {   Uint32 tmpMax, tmpAv;
        /* no FALSE return result */
        MLIMITbegin(WARN01level, uctMessageLimit);
            fprintf(uctout, "\tWarning: %lu FIDs found,",
                                    dirNode->nrOfChildren );
            tmpMax = (lengthOfDirectory - totalLengthOfFids) / minLengthOfFID;
            if( tmpMax == 0 )
            {   fprintf(uctout, " mismatch insufficient for one FID.\n"
                                "-\t\t");
            }
            else
            {   if( dirNode->nrOfChildren <= 1 )
                     tmpAv = minLengthOfFID + 5;             /* some minimal guess */
                else tmpAv = totalLengthOfFids / dirNode->nrOfChildren; /* average */
                tmpAv = (lengthOfDirectory - totalLengthOfFids) / tmpAv;
                fprintf(uctout,
                    " about %lu (at most %lu) FIDs ignored at\n"
                    "-\t\t end of", tmpAv, tmpMax);
            }
            nodePrintUnicodeNameTxtExtra(dirNode, mc, " directory: ","\n");
        MLIMITend;
    }

    if( countParentFids != 1 )
    {
        /* no FALSE return result */
        MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
             "\tError: Number of parent FIDS: %lu, expected exactly 1,\n"
            "-\t       ECMA 4/8.6, 4/8.8.3, UDF 2.3.4\n", countParentFids);
          nodePrintUnicodeNameTxtExtra(dirNode, mc, "-\tdirectory: ","\n");
        MLIMITend;
    }
    return result;

}   /* end nodeGetFIDs() */

static bool udfReadRootNode(UdfMountContext *mc)
{
    LongAd *rootDirICB;

    if( mc->rootNode != NULL)
    {   return FALSE;   /* already rootNode ?? */
    }

    if( (mc->rootNode = NEWNODE()) == NULL )
    {   return FALSE;
    }
    NODEFLAGS_SET_BIT(mc->rootNode, NFB_DIREXPAND);
    rootDirICB = &mc->fsd->rootDirectoryICB;

    mc->rootNode->parent = mc->rootNode;    /* ECMA 4/8.6 */
    mc->rootNode->fePartRef =
            rootDirICB->extentLocation.partitionReferenceNumber;

    if ( !readFileEntryEtc( mc, mc->rootNode,
                            rootDirICB, FALSE, FALSE, 0) )
    {
        nodeFreeHierarchy(mc->rootNode);
        mc->rootNode = NULL;
        return FALSE;
    }
    return TRUE;

}   /* end udfReadRootNode() */

/* streams were introduced in UDF 2.00
 */
static bool udfReadSystemStreamDirectoryNode(UdfMountContext *mc)
{
    LongAd *ssdICB;

    if( mc->systemStreamDirNode != NULL)
    {   return FALSE;   /* already systemStreamDirNode ?? */
    }
    if( getUctMinUdfRevision() < 0x200 )
    {   return TRUE;    /* UDF 1.50-, no streams */
    }
    if( mc->fsd->descriptorTag.descriptorVersion != 3 )
    {   return TRUE;    /* ECMA-167 2nd edition FSD descriptor */
    }                   /* warning printed in checkDescriptorVersion() */
    ssdICB = &mc->fsd->new200.systemStreamDirectoryICB;

    if( adGetExtentSize(ssdICB) == 0 )
    {   return TRUE;    /* systemStreamDirectory not defined for FSD */
    }                   /* with descriptorVersion 3, not an error */

    /* system stream directory exists,
     */
    if( (mc->systemStreamDirNode = NEWNODE()) == NULL )
    {   return FALSE;
    }
    NODEFLAGS_SET_BIT(mc->systemStreamDirNode, NFB_STREAMDIR);
    NODEFLAGS_SET_BIT(mc->systemStreamDirNode, NFB_SYSTEM);

    /* UDF 3.3.5 (not in ECMA, it should be):
     * The parent of the System Stream Directory shall be
     * the system stream directory
     */
    mc->systemStreamDirNode->parent = mc->systemStreamDirNode;
    mc->systemStreamDirNode->fePartRef =
                        ssdICB->extentLocation.partitionReferenceNumber;

    if ( !readFileEntryEtc(mc, mc->systemStreamDirNode,
                           ssdICB, FALSE, TRUE,0) ) /* expectStreamDir */
    {
        nodeFreeHierarchy(mc->systemStreamDirNode);
        mc->systemStreamDirNode = NULL;
        return FALSE;
    }
    return TRUE;

}   /* end udfReadSystemStreamDirectoryNode() */


/* get node UniqueID, handle exception
 * for parent FID.
 * return FALSE if none found.
 */
static bool nodeGetUniqueID(Node   *node,
                            Uint64 *pUniqueID,
                            bool   *pIsFid)
{   if( node == NULL )
    { return FALSE;
    }
    if(   getUctUdfRevision() >= 0x200  /* UDF 2.00+ */
       && node->fid != NULL )
    { *pIsFid = TRUE;                   /* FID */
      *pUniqueID = (Uint64)
        node->fid->ICB.implementationUse.ImpUse.UDFUniqueID;
    }
    else if( node->fe != NULL )     /* RootDir, SysStreamDir, VAT */
    { *pIsFid = FALSE;              /* (E)FE */
      *pUniqueID = *(pFE_uniqueID(node->fe));
    }
    else if(   NODEFLAGS_IS_SET(node, NFB_PARENTFID) /* UDF 1.50- */
            && node->parent     != NULL
            && node->parent->parent != NULL
            && node->parent->parent->fe != NULL )
    { *pIsFid = FALSE;              /* (E)FE */
      *pUniqueID =
          *(pFE_uniqueID(node->parent->parent->fe));
    }
    else
    {   return FALSE;
    }
    return TRUE;    /* found UniqueID */
}

/* Print Path for a number of node pointers
 * in a NodePointer array, using nodePrintPath(),
 * txt1 (if not NULL) is printed before each path
 * and txt2 after each path.
 * e.g. txt1 = " "   and txt2 = NULL
 *  or: txt1 = "-\t" and txt2 = "\n".
 *
 * If markNode == TRUE, extra text is printed,
 * denoting specific node information:
 *  " FID"  if the node has a FID
 *  " NULL" if no ICB is defined
 *  " ERR"  if address translation error
 *
 * If showUniqueID == TRUE, the UniqueID is printed:
 * "#HHHHHHHHHHHHHHHH " for a (E)FE UniqueID
 * "        #HHHHHHHH " for a FID UniqueID
 * else:       "\t\t  " if no UniqueID present
 *
 * print layout:
 *
 * <txt1> [markNode text] [\t] [UniqueID text] <path> <txt2>
 *
 * \t printed if (markNode || showUniqueID)
 *
 * For nppMode definition: see nodePrintPath()
 */
static void npPrintPath(NodePointer *npp, Uint32 len,
                        Uint8 nppMode,
                        char *txt1, char *txt2,
                        bool  markNode,
                        bool  showUniqueID,
                        UdfMountContext *mc)
{   Uint32  n;
    LongAd *icb;
    Node   *np;
    Uint32  physAddr = 0, dummy;

    for( n = 0; n < len; n++ )
    {   np  = *(npp++);
        icb = nodeGetIcb(np, mc);
        if( txt1 != NULL )
        {   fprintf(uctout, "%s", txt1);
        }

        if( markNode )  /* special node additions */
        {   if( np->fid != NULL )
            { fprintf(uctout, " FID");
            }
            if( icb == NULL )
            { fprintf(uctout, " NULL");
            }
            else if( !translateAddress(mc,
                 icb->extentLocation.partitionReferenceNumber,
                 icb->extentLocation.logicalBlockNumber,
                &physAddr, &dummy, TRUE) )  /* silent */
            { fprintf(uctout, " ERR"); /* cannot translate */
            }
        }
        if( markNode || showUniqueID )
        {   fprintf(uctout, "\t");
        }

#ifdef UCT_TESTING_EXTRA        /* extra debug */
        if( markNode )
        {   const MediumInfo *vmi = getTheMediumInfo();
            /* size in blocks and logical address
             */
//          if( icb == NULL )
//               fprintf(uctout, "\t\t");
//          else fprintf(uctout, "%u (%7lu,%u)\t",
//                  adGetExtentSize(icb) / vmi->blockSize,
//                  icb->extentLocation.logicalBlockNumber,
//                  icb->extentLocation.partitionReferenceNumber);
            /* physical address
             */
//          if( icb == NULL )
//               fprintf(uctout, "\t");
//          else fprintf(uctout, "%7lu\t", physAddr);
        }
#endif  /** UCT_TESTING_EXTRA **/

        if( showUniqueID )
        {   Uint64 uniqueID;
            bool   isFid;
            if( nodeGetUniqueID(np, &uniqueID, &isFid) )
            { printHexUniqueId17Chars( uniqueID, isFid);
            }
            else
            { fprintf(uctout, "\t\t ");
            }
            fprintf(uctout, " ");
        }
        nodePrintPath(np, mc, nppMode, VERBOSE00level, txt2);
    }
    fflush(uctout);

}   /* end npPrintPath() */


/* test if string matches with node->unicodeName,
 * or if it matches with the 'head' of node->unicodeName.
 * temporarily conversion of string FName to unicode.
 */
extern bool stringIsUnicodeName( Node *node, char *fName,
                                 bool *pHeadMatches )
{
    unicode_t  *uName;
    Uint32      n;
    size_t      len, fLen = strlen(fName);
    bool        result, headMatch;

    /* check if compare needed
     */
    result = headMatch = FALSE;
    if(      fLen == node->unicodeNameLen
       || (   fLen < node->unicodeNameLen
           && pHeadMatches != NULL) )
    {   /* node->unicodeNameLen is at least fLen
         * so there may be a (head) match.
         * convert fName to Unicode and compare
         */
        len  = MIN(fLen, node->unicodeNameLen);
        if( (uName = tst_malloc( len * sizeof(unicode_t),
                              __FILE__,__LINE__)) == NULL )
        {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
        }
        for( n = 0; n < len; n++ )
        {   uName[n] = (unicode_t) ((unsigned char) fName[n]);
        }
        if( memcmp( node->unicodeName, uName,
                    len * sizeof(unicode_t) ) == 0 )
        {   /* All characters of Fname match.
             * Complete match if node->unicodeNameLen is fLen.
             */
            headMatch = TRUE;
            result = (fLen == node->unicodeNameLen);
        }
        free( uName);
    }
    if( pHeadMatches != NULL) *pHeadMatches = headMatch;
    return result;

}   /* end stringIsUnicodeName() */

/* check if Non-Allocatable Space List/Stream node.
 * return value:
 *  If node is the Non-Allocatable Space List/Stream,
 *  or if the verifier decides to handle
 *  it as such because of an obvious naming error
 *  then: TRUE
 *  else: FALSE
 */
static bool handleNonAllocatableSpace( UdfMountContext *mc,
                                       Node   *node )
{   bool   foundNonAlloc, foundLookalike;
    Uint16 udfRevision;

    /* Non-Allocatable Space List/Stream is identified by its expanded
     * unicode name and it shall be a file in the root directory
     * or a System Stream
     */
    if(    NODEFLAGS_IS_SET(node, NFB_UNUSED_FID)
        || node->unicodeName == NULL
        || (   node->parent != mc->rootNode
            && node->parent != mc->systemStreamDirNode) )
    {   return FALSE;   /* node is not Non-Allocatable Space */
    }

    /* Test UDF 1.50 and UDF 2.00+ correct cases and lookalikes,
     * see uctverify.h.
     * Lookalikes may also have a wrong UDF revision
     * or parent (stream) directory.
     */
    foundNonAlloc = foundLookalike = FALSE;
    udfRevision = getUctUdfRevision();

    if(   (   udfRevision == 0x150              /* UDF 1.50 */
           && node->parent == mc->rootNode
           && stringIsUnicodeName(node, NONALLOCNAME150, NULL))
       || (   udfRevision >= 0x200              /* UDF 2.00+ */
           && node->parent == mc->systemStreamDirNode
           && stringIsUnicodeName(node, NONALLOCNAME200, NULL)) )
    {
        foundNonAlloc = TRUE;       /* correct case found */
    }
    else            /* test lookalikes */
    {   /* test if nameHeadMatches for 150 and 200 lookalikes.
         */
        (void) stringIsUnicodeName(node,
                        NONALLOCNAME150_LOOKALIKE, &foundLookalike);
        if( !foundLookalike )
        { (void) stringIsUnicodeName(node,
                        NONALLOCNAME200_LOOKALIKE, &foundLookalike);
        }
    }

    /* At most one of foundNonAlloc and foundLookalike is TRUE.
     * print lookalike warning, maybe try to use as Non-Allocatable Space.
     */
    if( foundLookalike )
    {   char *expTxt, *namTxt, *refTxt;
        /* lookalike may be handled as a real one,
         * but NOT for the following cases:
         * 1) UDF revision less than 1.50, Non-Allocatable Space not supported.
         * 2) there is already a Non-Allocatable Space Node accepted
         *    on mc->nonAllocatableSpaceNode.
         *
         * Add a special Note explaining whether a try to handle as
         * the real Non-Allocatable Space is performed or (why) not.
         */
        if(   udfRevision < 0x150
           || mc->nonAllocatableSpaceNode != NULL )
        {   foundLookalike = FALSE;     /* do not handle as real one */
        }
        MLIMITbegin( WARN01level, uctMessageLimit );
          /* construct 'expected' and UDF reference texts
           */
          refTxt = NONALLOC_UDFREF_TXT;
          if(  udfRevision == 0x150 )
          { expTxt = "File in root directory: ";
            namTxt = NONALLOCNAME150;
          }
          else if( udfRevision >= 0x200 )
          { expTxt = "System Stream: ";
            namTxt = NONALLOCNAME200;
          }
          else          /* maybe unknown UDF revision */
          { expTxt = "Non-Allocatable Space not"
                                    " defined in above UDF revision";
            namTxt = NULL;
            refTxt = "UDF 1.50 2.3.13, UDF 2.00+ 3.3.7.2";
          }
          fprintf(uctout, "\t Warning: %s looks like"
                                        " Non-Allocatable Space,\n"
            "-\t\t  UDF revision: ", (node->parent == mc->rootNode)
                                     ? "File in root directory"
                                     : "System Stream");
          printUdfRevision(MLIMIT.vl, udfRevision, ", name: ");
          nodePrintUnicodeNameTxt(node, mc, NULL, NULL);
          fprintf(uctout,
                   ".\n-\texpected: %s", expTxt);
          if( namTxt != NULL )
          { fprintf(uctout, "\"%s\"", namTxt);
          }
          fprintf(uctout, ",\n-\t\t  %s.\n", refTxt);

          /* extra Note:
           */
          if( foundLookalike )      /* handle as real */
          { fprintf(uctout,
              "-\t    Note: Try to handle as Non-Allocatable Space.\n"
                    "-\t\t  If it was not meant to be Non-Allocatable Space,\n"
                    "-\t\t  incorrect errors/warnings may occur.\n");
          }
          else                  /* do not handle as real */
          { fprintf(uctout,
              "-\t    Note: Will not be handled as"
                                " Non-Allocatable Space, because\n"
                    "-\t\t  %s.\n",
                (mc->nonAllocatableSpaceNode != NULL)
                ? "already Non-Allocatedable Space found before"
                : "UDF revision does not support Non-Allocatable Space");
          }
          fprintf(uctout, "\n");
        MLIMITend;
    }

    /* now check further attributes and maybe accept
     * Non-Allocatable Space for real
     * and remaining lookalike cases.
     */
    if( foundNonAlloc || foundLookalike)
    {
        nodePrintUnicodeNameTxt(node, mc,
                  "\tVerify  Non-Allocatable Space: ", "\n");
        if( verifyNonAllocatableSpace(mc, node) )
        {   /* no fatal errors
             */
            if( mc->nonAllocatableSpaceNode == NULL )
            {   nodePrintUnicodeNameTxt(node, mc,
                  "\tAccept  Non-Allocatable Space: ", "\n");
            }
            else    /* already Non-Allocatable Space accepted */
            {   /* this is no lookalike, because verifyNonAllocatableSpace()
                 * is only called for a lookalike if nothing accepted yet
                 * so overwrite current accepted entry.
                 */
                nodePrintUnicodeNameTxt(node, mc,
                  "\tReplace Non-Allocatable Space: ", "\n");
            }
            mc->nonAllocatableSpaceNode = node;     /* accept/replace */
            mc->nonAllocSpaceIsLookalike = foundLookalike;
        }
        else        /* incomplete verification or fatal error */
        {   MLIMITbegin(WARN01level, uctMessageLimit);
              fprintf(uctout,
                "\tWarning: Incomplete Non-Allocatable Space verification,\n"
                     "-\t\t probably because of a previous error.\n");
            MLIMITend;
        }
        VERBOSE00(uctout, "\n");
    }

    return foundNonAlloc;
}

/* Verify presence (or absence) of Non-Allocatable Space.
 * Only present on media that have no defect management,
 * see UDF 1.50 2.3.13 Non-Allocatable Space List
 * and UDF 2.00+ 3.3.7.2 Non-Allocatable Space Stream.
 * Non-Allocatable Space "shall be recorded only on media
 * systems that do not do defect management".
 *
 * The only one that we can detect here is a medium with
 * a Sparable Partition.
 */
static bool checkPresenceOfNonAllocatableSpace(UdfMountContext *mc)
{
    PartitionMapInfo *pmi;
    Uint16 pRef, numberOfPartitionMaps;
    bool   correctNonAllocExists,
           sparablePartitionExists = FALSE;
    char  *udfRef = NONALLOC_UDFREF_TXT;

    if(   mc == NULL || mc->vi == NULL || mc->vi->lvd == NULL
       || mc->partitionMapInfo == NULL )
    {   return FALSE;
    }
    /* Check if Sparable partition.
     */
    numberOfPartitionMaps = (Uint16) mc->vi->lvd->numberOfPartitionMaps;

    for( pRef = 0; pRef < numberOfPartitionMaps; pRef++ )
    {   pmi = &mc->partitionMapInfo[pRef];
        if( pmi->pMapType == PMAPTYPE_SPARABLE )
        {   sparablePartitionExists = TRUE;
            break;      /* sparable partition ref in pRef */
        }
    }

    /* in case a lookaloke is used, print a warning
     * as last message.
     */
    correctNonAllocExists =
            (    mc->nonAllocatableSpaceNode != NULL
             && !mc->nonAllocSpaceIsLookalike );

    if(   sparablePartitionExists
       && correctNonAllocExists )
    {
      MLIMITbegin(INFO01level, uctMessageLimit);
        fprintf(uctout,
          "\n  ==>\tFound Non-Allocatable Space for medium"
                                        " with Sparable Partition.\n");
      MLIMITend;
    }
    else if(    sparablePartitionExists
            && !correctNonAllocExists )
    { MLIMITbegin(INFO01level, uctMessageLimit);
        fprintf(uctout,
          "\n\tNote: No Non-Allocatable Space for medium with\n"
           "-\t      Sparable Partition, %s.\n", udfRef);
      MLIMITend;
    }
    else if(   !sparablePartitionExists
            &&  mc->nonAllocatableSpaceNode != NULL )   /* maybe lookalike used */
    { MLIMITbegin(WARN01level, uctMessageLimit);
        fprintf(uctout,
          "\n\tWarning: Non-Allocatable Space found but no Sparable Partition.\n"
                 "-\t\t Medium without defect management, %s.\n", udfRef);
      MLIMITend;
    }

    /* lookalike warning as last message
     */
    if(   mc->nonAllocatableSpaceNode != NULL       /* lookalike */
       && mc->nonAllocSpaceIsLookalike )            /*  was used */
    { MLIMITbegin(WARN01level, uctMessageLimit);
        fprintf(uctout,
          "\tWarning: Look-alike is used as Non-Allocatable Space, see\n"
               "-\t\t earlier error or warning messages, %s.\n", udfRef);
      MLIMITend;
    }
    return TRUE;

}   /* end checkPresenceOfNonAllocatableSpace() */


/* Node compare unicode names
 * Return value:
 *  if      unicodeName 1 'lower' then: value < 0
 *  else if unicodeName 2 'lower' then: value > 0
 *  else if unicode names equal   then: 0
 *  as usual for qsort compare functions.
 *
 * Note:
 *  A node->unicodeName NULL pointer is considered
 *  to be the lowest value possible.
 */
static int nodeCmpUnicodeNames(Node *n1, Node *n2)
{
    int        iResult;
    Uint32     l1,  l2, len;
    unicode_t *u1 = n1->unicodeName,
              *u2 = n2->unicodeName;

    if( u1 == NULL && u2 == NULL )  /* both NULL */
    {   return QSORT_EQUAL;         /* considered equal */
    }

    /* unicodeName not both NULL
     */
    if( u1 == NULL )
    {   return QSORT_ELEM1_FIRST;   /* n1 first */
    }
    if( u2 == NULL )
    {   return QSORT_ELEM1_LAST;    /* n2 first */
    }

    l1  = n1->unicodeNameLen;
    l2  = n2->unicodeNameLen;
    len = MIN(l1, l2);
    iResult = memcmp(u1, u2, len * sizeof(unicode_t));
    if( iResult == 0 )
    {   return (l1 - l2);   /* shortest first */
    }
    return iResult;
}

/* qsortUnicodeNames():
 * NULL pointer is lowest, further do byte compare.
 */
static int qsortUnicodeNames(const void *elem1, const void *elem2)
{
    return nodeCmpUnicodeNames(*((NodePointer*) elem1),
                               *((NodePointer*) elem2));
}

/* check for Non-Allocatable Space List/Stream
 * check for ambiguous directory entries
 * precondition: dirNode must be a consistent directory node
 * TODO: for UDF 2.01+, exclude deleted entries.
 * TODO: What with NULL pointers caused by wrong File
 *       Identifier that could not be translated to unicode.
 */
static bool udfNodeCheckDirectoryEntries(Node *dirNode,
                                         UdfMountContext *mc)
{
    Node        *child;
    NodePointer *nodePnts;
    Uint32       n;

    /* allocate and fill child nodes pointer array
     */
    if( (nodePnts = (NodePointer*) tst_malloc(dirNode->nrOfChildren * sizeof(Node*),
                                __FILE__,__LINE__)) == NULL )
    {   uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
    }

    /* Check for Non-Allocatable Space.
     * Initialize node pointer array for qsort and
     * sort on unicodeName.
     */
    for( child = dirNode->firstChild, n = 0;
         child != NULL;
         child = child->nextInDirectory, n++ )
    {   nodePnts[n] = child;
        (void) handleNonAllocatableSpace(mc, child);
    }
    qsort((void *)nodePnts, dirNode->nrOfChildren,
          sizeof(NodePointer), qsortUnicodeNames);

    /* Node pointers in nodePnts are sorted now,
     * count ambiguous entry names.
     */
#ifdef  DEBUG02
    ifPRINTdebug02(uctout, "DEBUG02: sorted directory:\n");
    ENDif;
#endif  /* DEBUG02 */

    for( n = 0; n < dirNode->nrOfChildren; n++ )
    {   Uint32 cnt = 0;

#ifdef  DEBUG02
        ifVERBOSE(DEBUG02level)
        {   fflush(uctout);
            nodePrintUnicodeNameTxtExtra(nodePnts[n], mc, NULL, "\n");
            fflush(uctout);
        }
        ENDif;
#endif  /* DEBUG02 */

        while(   n != 0 && n < dirNode->nrOfChildren
              && nodeCmpUnicodeNames(nodePnts[n-1],
                                     nodePnts[n]) == 0 )
        {   cnt++; n++;
#ifdef  DEBUG02
            ifVERBOSE(DEBUG02level)
            { if( n < dirNode->nrOfChildren )
              { fflush(uctout);
                nodePrintUnicodeNameTxtExtra(nodePnts[n], mc, NULL, "\n");
                fflush(uctout);
              }
            }
            ENDif;
#endif  /* DEBUG02 */
        }
        if( cnt != 0 )          /* there are ambiguous entries */
        {   Node  *node, *firstNode;
            Uint32 x, cntDelFids = 0;

            /* count how many have the deleted bit set.
             */
            cnt++;              /* count first one too, so: cnt >= 2 */
            firstNode = nodePnts[n-cnt];
            for( x = 0; x < cnt; x++ )
            {   node = nodePnts[n-cnt+x];
                if(           node->fid != NULL
                   && isBitOn(node->fid->fileCharacteristics,
                              FCB_DELETED) )
                { cntDelFids++; /* count deleted FIDs */
                }
            }

            /* - Error if there are more than one non-deleted
             *   FIDs involved.
             * - For UDF 2.00 and lower, a warning for ambiguous
             *   deleted FID names.
             */
            if( (cnt - cntDelFids) > 1 )
            { /*  error: more than one non-deleted FID involved
               */
              MLIMITbegin(ERROR00level, uctMessageLimit);
                fprintf(uctout,
                   "\tError: %2lu ambiguous unicode names for: ", cnt);
                nodePrintUnicodeNameTxt(firstNode, mc, NULL, "\n");
                nodePrintUnicodeNameTxt(dirNode, mc,
                                      "-\t\t\t\t in directory: ", "\n");
                fprintf(uctout,
                  "-\t %lu of them are for non-deleted FIDs."
                                            " Non-deleted FID names\n"
                  "-\t shall be unique AFTER uncompress,"
                                                    " ECMA 4/8.6%s,\n"
                  "-\t FOR EXTRA CLARIFICATION, see UDF 2.50 2.3.4.2+6.\n",
                   cnt-cntDelFids, (getUctMaxUdfRevision() >= 0x200)
                                        ? ", UDF 2.3.4.2" : "");
                if( globalFidIdentifierError )
                { /* uncompress problem in this directory */
                  fprintf(uctout,
                    "-\t Note: This error message may be caused by an"
                                            " identifier format\n"
                    "-\t       error reported earlier for one of"
                                        " the entries involved.\n");
                }
              MLIMITend;
            }

            /* Ambiguous deleted FID names are allowed for UDF 2.01+.
             * For UDF 2.00- this is not clear, print warning.
             */
            if(   getUctMaxUdfRevision() <= 0x200
               && cntDelFids != 0 )
            { /* warning for ambiguous deleted entries in
               * UDF release 2.00 and lower.
               */
              MLIMITbegin(WARN01level, MLIMITdefault01);
                fprintf(uctout,
                   "\tWarning: %2lu ambiguous unicode names for: ", cnt);
                nodePrintUnicodeNameTxt(firstNode, mc, NULL, "\n");
                nodePrintUnicodeNameTxt(dirNode, mc,
                                      "-\t\t\t\t   in directory: ", "\n");
                fprintf(uctout,
                  "-\t %lu of them %s for %sdeleted FID%s."
                                            " For UDF release 2.00\n"
                  "-\t and lower, it is unclear whether ambiguous deleted FID\n"
                  "-\t names are allowed or not, ECMA 4/8.6%s,\n"
                  "-\t FOR EXTRA CLARIFICATION, see UDF 2.50 2.3.4.2+6.\n",
                  cntDelFids, PLURAL_ARE(cntDelFids),
                  PLURAL_A(cntDelFids), PLURAL_S(cntDelFids),
                  (getUctMaxUdfRevision() >= 0x200)
                                        ? ", UDF 2.3.4.2" : "");
                if( globalFidIdentifierError )
                { /* uncompress problem in this directory */
                  fprintf(uctout,
                    "-\t Note: This warning message may be caused by an"
                                            " identifier format\n"
                    "-\t       error reported earlier for one of"
                                        " the entries involved.\n");
                }
              MLIMITend;
            }
        }
    }

    free((Byte*)nodePnts);
    return TRUE;
}


#define MAXUDFPATHSIZE 1023 /* dropped for UDF 2.01+ */

static void checkPathSize(Uint32 pathSize)
{
    if(    getUctMinUdfRevision() <= 0x200
        && pathSize > MAXUDFPATHSIZE )
    { MLIMITbegin(WARN01level, MLIMITdefault02);
        fprintf(uctout, "\n\tWarning: Path size: %lu,"
                    " expected: at most %lu, UDF 2.\n",
                        pathSize, MAXUDFPATHSIZE);
      MLIMITend;
    }
}


/* showPermissions(): ECMA 4/14.9.5
 * Order of presentation:
 * for each group start with colon ":",
 * then: FE_PERM_BITNMB_OWNER_DEL  (bit 14) first
 * etc.
 * then: FE_PERM_BITNMB_OTHER_EXEC (bit 0) as last.
 * The result is ":darwx:darwx:darwx"
 *                  own   grp   oth
 * which is like usual unix "rwx rwx rwx"
 *                           own grp oth
 * Extra: for unused FIDs print "<unused FID>"
 */
static void showPermissions(Byte *feOrEfe,
                            Node *node)
{
    Uint32 perm;

    /* undefined permission exceptions first
     */
    if( feOrEfe == NULL )
    { fprintf(uctout, "%18s",           /* 18 positions */
              (NODEFLAGS_IS_SET(node, NFB_UNUSED_FID))
                ? "<unused FID>"
                : "<missing FE>");
      return;
    }
    perm = *(pFE_permissions(feOrEfe));

    /* show permissions, ECMA 4/14.9.5
     * for layout see explanation above
     */
    fprintf(uctout, ":%c%c%c%c%c"   /* 'other' permissions */
                    ":%c%c%c%c%c"   /* 'group' permissions */
                    ":%c%c%c%c%c",  /* 'owner' permissions */
        isBitOn(perm, FE_PERM_BITNMB_OWNER_DEL)   ? 'd' : '.',
        isBitOn(perm, FE_PERM_BITNMB_OWNER_ATR)   ? 'a' : '.',
        isBitOn(perm, FE_PERM_BITNMB_OWNER_READ)  ? 'r' : '.',
        isBitOn(perm, FE_PERM_BITNMB_OWNER_WRITE) ? 'w' : '.',
        isBitOn(perm, FE_PERM_BITNMB_OWNER_EXEC)  ? 'x' : '.',

        isBitOn(perm, FE_PERM_BITNMB_GROUP_DEL)   ? 'd' : '.',
        isBitOn(perm, FE_PERM_BITNMB_GROUP_ATR)   ? 'a' : '.',
        isBitOn(perm, FE_PERM_BITNMB_GROUP_READ)  ? 'r' : '.',
        isBitOn(perm, FE_PERM_BITNMB_GROUP_WRITE) ? 'w' : '.',
        isBitOn(perm, FE_PERM_BITNMB_GROUP_EXEC)  ? 'x' : '.',

        isBitOn(perm, FE_PERM_BITNMB_OTHER_DEL)   ? 'd' : '.',
        isBitOn(perm, FE_PERM_BITNMB_OTHER_ATR)   ? 'a' : '.',
        isBitOn(perm, FE_PERM_BITNMB_OTHER_READ)  ? 'r' : '.',
        isBitOn(perm, FE_PERM_BITNMB_OTHER_WRITE) ? 'w' : '.',
        isBitOn(perm, FE_PERM_BITNMB_OTHER_EXEC)  ? 'x' : '.');
}

/* udfNodeShowDirectory():
 * Note that for this function, in case dirNode->child is a
 * 'parent FID', the File Entry information is not fetched
 * from child->fe (which is NULL), but from dirNode->parent->fe.
 */
static void udfNodeShowDirectory(UdfMountContext *mc, Node *dirNode)
{
    Node    *child;
    Uint8    fileCharacteristics, fileType;
    Uint16   icbFlags;
    Uint32   pathSize = 0;
    bool     isStreamDir = NODEFLAGS_IS_SET(dirNode, NFB_STREAMDIR);

    /* Do not exit if INFO01level does not apply because
     * this function does also check the path size limit.
     * First show full directory path
     */
    ifPRINTinfo01(uctout, "\n%sDirectory: ",
                (isStreamDir) ? "Stream " : "");
    ENDif;

    /* print full path
     * nodePrintPath() handles all exceptions
     */
    pathSize = nodePrintPath(dirNode, mc, NPP_FULL,
                             INFO01level, "\n\n");
    checkPathSize(pathSize);

    /* show ALL entries
     */
    for( child = dirNode->firstChild;
         child != NULL;
         child = child->nextInDirectory )
    {
        bool  childIsParentFID;
        Byte *fe;               /* points to FE or EFE, else NULL */
        ExtendedFileEntry *efe; /* points to EFE, else NULL */

        fileCharacteristics = child->fid->fileCharacteristics;
        childIsParentFID  = isBitOn(fileCharacteristics, FCB_PARENT);

        /* For parent FID, show File Entry information
         *              of: dirNode->parent->fe
         *      instead of: child->fe (which is NULL)
         */
        if( childIsParentFID )
             fe = dirNode->parent->fe;
        else fe = child->fe;

        if(     fe != NULL
            && ((Tag*)fe)->tagIdentifier == tidEFE )
             efe = (ExtendedFileEntry *) fe;
        else efe = NULL;

        /* Show File Characteristics bits, EA Space
         * and Stream Directory first.
         * Mind that fileType and icbFlags are only valid iff fe != NULL !!!
         */
        fileType =  (Uint8) ((fe != NULL) ? pFE_icbTag(fe)->fileType
                                          : FT_ILLEGAL);
        icbFlags = (Uint16) ((fe != NULL) ? pFE_icbTag(fe)->flags
                                          : 0);

        ifVERBOSE(INFO01level)
        {   fprintf(uctout, "%c",
                (isBitOn(fileCharacteristics,FCB_HIDDEN))    ? 'h' : '.');
            fprintf(uctout, "%c",
                (isBitOn(fileCharacteristics,FCB_DIRECTORY)) ? 'd' : '.');
            fprintf(uctout, "%c",
                (isBitOn(fileCharacteristics,FCB_DELETED))   ? 'D' : '.');
            fprintf(uctout, "%c",
                (childIsParentFID)                           ? 'p' : '.');
            fprintf(uctout, "%c",
                (isBitOn(fileCharacteristics,FCB_METADATA))  ? 'm' : '.');

            /* show icbtag Flags allocation type
             * (ADT_EXTENDED is illegal)
             */
            if( fe != NULL )
            { Uint8 adType = GET_ADTYPE(icbFlags);
              if(      adType == ADT_SHORT )    fprintf(uctout, "S");
              else if( adType == ADT_LONG )     fprintf(uctout, "L");
              else if( adType == ADT_EXTENDED ) fprintf(uctout, "E");    /* illegal */
              else if( adType == ADT_INFE )     fprintf(uctout, "e");   /* Embedded */
              else fprintf(uctout, "%d", adType);               /* illegal/reserved */
            }
            else            /* fe == NULL */
            {                                   fprintf(uctout, ".");
            }

            /* show rest of icbtag Flags (all ZERO if FE == NULL)
             */
            fprintf(uctout, "%c",
                (isBitOn(icbFlags, ICBF_NONRELOCATABLE_BIT)) ? 'N' : '.');
            fprintf(uctout, "%c",
                (isBitOn(icbFlags, ICBF_ARCHIVE_BIT))        ? 'A' : '.');
            fprintf(uctout, "%c",
                (isBitOn(icbFlags, ICBF_SETUID_BIT))         ? 'U' : '.');
            fprintf(uctout, "%c",
                (isBitOn(icbFlags, ICBF_SETGID_BIT))         ? 'G' : '.');
            fprintf(uctout, "%c",
                (isBitOn(icbFlags, ICBF_STICKY_BIT))         ? 'Y' : '.');
            fprintf(uctout, "%c",
                (isBitOn(icbFlags, ICBF_CONTIGUOUS_BIT))     ? 'C' : '.');
            fprintf(uctout, "%c",
                (isBitOn(icbFlags, ICBF_SYSTEM_BIT))         ? 'S' : '.');
            fprintf(uctout, "%c",
                (isBitOn(icbFlags, ICBF_STREAM_BIT))         ? 's' : '.');

            /* show presence of Stream Directory and EA Space
             */
            fprintf(uctout, "%c", ( efe != NULL &&
                adGetExtentSize(&efe->streamDirectoryICB) != 0 )
                                                             ? 'S' : '.');
            fprintf(uctout, "%c", ( fe != NULL &&
                (   (*(pFE_lengthOfExtendedAttributes(fe))) != 0
                 || adGetExtentSize(pFE_extendedAttributeICB(fe)) != 0 ))
                                                             ? 'E' : '.');

            /* show permissions or "<unused FID>"
             */
            if( uctDoShowPerm )
            { showPermissions(fe, child);
            }

            /* FE file type
             */
            fprintf(uctout, " %-4s",
                (fe != NULL) ? FT_TEXT4(fileType) : "");

            /* file link count
             */
            if( fe != NULL )
                 fprintf(uctout, " %2u ", *(pFE_fileLinkCount(fe)));
            else fprintf(uctout, " %2s ", "");

            /* print short form of modificationTime
             * in Coordinated Universal Time
             * no seconds and lower fields
             */
            if( fe == NULL )
            { fprintf(uctout, "%17s", "");          /* 16+1 chars */
            }
            else if( uctDoUTCtime )   /* default: UTC, 16+1 chars */
            { printUTCtime(pFE_modificationTime(fe), FALSE, " ");
            }
            else     /* local time, with second-and-lower, more chars */
            { printTimestampShort(pFE_modificationTime(fe), TRUE, " ");
            }

            /* Print File size in bytes, default 10+1 chars.
             * Take real (E)FE Information length, also if
             * this does not match with file body length.
             */
            if( fe == NULL )
            { fprintf(uctout, "%11s", "");              /* 10+1 chars */
            }
            else  /* printUint64: decimal if fits in Uint32, else hex */
            { printUint64(VERBOSE00level,
                          *(pFE_informationLength(fe)), /* 10+1 or:   */
                          FALSE, "%10lu");          /* max 17+1 chars */
              fprintf(uctout, " ");
            }

            /* print FID File Identifier
             * child->unicodeName may still be NULL (empty)
             */
            nodePrintUnicodeNameTxtExtra(child, mc, NULL, NULL);

            /* mark symbolic links and warn for some
             * unexpected file types in a directory.
             * TODO: expand symbolic link.
             */
            if(   fe != NULL        /* fileType is valid */
               && fileType == FT_SYMBOLIC_LINK )
            {   fprintf(uctout, " -> <symbolic link>");
            }
        }
        ENDif;      /* INFO01level */

        /* mind that line not yet closed with \n for INFO01level,
         * do test independent of verbose level.
         */
        if(             fe != NULL      /* fileType is valid */
           && (   fileType == FT_UNKNOWN_OR_VAT150
               || fileType == FT_VAT200) )
        { MLIMITbegin(WARN01level,uctMessageLimit);
            fprintf(uctout,
                "\n ==> Warning: Unusual File Type %s (%u) in a"
                    " directory\n", FT_TEXT4(fileType), fileType);
          MLIMITend;
        }

        ifVERBOSE(INFO01level)
        {   fprintf(uctout, "\n");  /* close directory listing line */
        }
        ENDif;      /* INFO01level */

        /* check path size, no check for parent FID
         */
        if( !childIsParentFID )
        { checkPathSize(pathSize + 1 + child->unicodeNameLen);
        }
    }
    ifPRINTinfo01(uctout, "\n");
    ENDif;

}   /* end udfNodeShowDirectory() */

/* Be aware of the fact that all FIDs get a child node,
 * so the 'parent' and deleted ones too.
 */
static bool udfNodeGetChildren( UdfMountContext *mc, Node *dirNode )
{
    Node  *child;
    Uint8  fileType;
    bool   dirIsStreamDir = NODEFLAGS_IS_SET(dirNode, NFB_STREAMDIR);

    if( dirNode->fe == NULL )
    {   return FALSE;
    }
    fileType = pFE_icbTag(dirNode->fe)->fileType;
    if(   fileType != FT_DIRECTORY
       && fileType != FT_STREAM_DIRECTORY )
    {   return FALSE;
    }

    (void) nodeGetFIDs(mc, dirNode);

    /* if nodeGetFIDs() returns FALSE, dirNode->nrOfChildren
     * denotes how many FIDs were found (maybe none).
     * Children node chain created,
     */
    if( dirNode->nrOfChildren != 0 )
    {   ifPRINTinfo02(uctout,
            "\tAdd FIDs to directory hierarchy and read FEs\n");
        ENDif;
    }

    /* read FE, set nodeFlags, etc. for all appropriate children
     */
    for( child = dirNode->firstChild;
         child != NULL;
         child = child->nextInDirectory )
    {
        FileIdentifierDescriptor *pFid = child->fid;
        Uint8 fileCharacteristics = pFid->fileCharacteristics;
        bool  dirBitSet = isBitOn(fileCharacteristics, FCB_DIRECTORY);

        child->fePartRef =
                pFid->ICB.extentLocation.partitionReferenceNumber;

        /* no further action for parent FID
         * and for unspecified ICB.
         */
        if( NODEFLAGS_IS_SET(child, NFB_PARENTFID) )
        {   continue;       /* no further action */
        }

        if( adGetExtentSize(&pFid->ICB) != 0 )  /* ICB specified */
        {   /* read child (E)FE.
             * If readFileEntryEtc() returns FALSE, it
             * will set child->fe = NULL and
             * nodeFreeAllocationLists() will be executed.
             */
/** this is too much text in the output
 **         ifPRINTinfo02(uctout, "  ==>\tRead child\n");
 **         ENDif;
 **/
            (void) readFileEntryEtc(mc, child, &pFid->ICB, FALSE,FALSE,0);

            fileType = (Uint8)((child->fe != NULL)
                                ? pFE_icbTag(child->fe)->fileType
                                : FT_ILLEGAL);

            /* check if subdirectory to be expanded.
             * warn if looks like a subdirectory.
             * If dirIsStreamDir, then no nested directories or stream
             * directories are allowed. Entries containing illegal or
             * inconsistent (stream) directories are handled as normal
             * files or streams.
             */
            if( child->fe == NULL )
            { /* Error message printed in readFileEntryEtc() already
               */
            }
            else if(    dirBitSet
                    &&  fileType == FT_DIRECTORY
                    && !dirIsStreamDir )
            { /* real directory, mark for expand
               */
              NODEFLAGS_SET_BIT(child, NFB_DIREXPAND);
            }
            else if(   dirBitSet
                    || fileType == FT_DIRECTORY
                    || fileType == FT_STREAM_DIRECTORY )
            { /* looks like a subdirectory,
               * child->fe != NULL
               */
              MLIMITbegin(ERROR00level, MLIMITdefault10);
                printMessageHead( MLIMIT.vl, (Byte*) child->fid,
                        (Byte*) &child->fid->fileCharacteristics,
                  "Error: Inconsistent or illegal directory entry.\n");
                fprintf(uctout, "-\t\t\tFID Directory bit %sset,",
                                    (dirBitSet) ? "" : "NOT ");
                fprintf(uctout, " File Type: %u (%s).\n",
                                fileType, FT_TEXT4(fileType));
                if( dirIsStreamDir )
                { fprintf(uctout, "-\t\t\tNested %sdirectory illegal"
                                            " in stream directory.\n",
                    (fileType == FT_STREAM_DIRECTORY) ? "stream " : "");
                }
                fprintf(uctout,
                    "-\t\t\tNo directory expand, handle as %s.\n",
                            (dirIsStreamDir) ? "stream" : "file");
                nodePrintUnicodeNameContLine(child,mc);
              MLIMITend;
            }
        }
    }
    return TRUE;

}   /* end udfNodeGetChildren() */


/* allocBlockBuffer() TRIES to allocate a buffer of size
 * as indicated by reqSize. If this fails, the size is
 * divided by 2 and then rounded up to the nearest multiple of
 * the blockSize and then allocation is tried again.
 * This repeats untill a succesfull allocation is achieved or
 * the size is less than or equal to the blockSize.
 *
 * If an allocation attempt is successful, but an earlier
 * attempt failed, the buffer will be reduced with an extra
 * factor 4 (rounded up to the blockSize), in order to avoid
 * swapping and allocation of too much of the available memory.
 *
 * iff allocation was succcessful then return the buffer
 *      pointer and the allocated size in (*pAllocSize)
 * else return NULL and an UNDEFINED value in (*pAllocSize);
 */
extern Byte *allocBlockBuffer( Uint32  reqSize,
                               Uint32 *pAllocSize )
{
    const MediumInfo *vmi = getTheMediumInfo();
    Byte   *pBlockBuffer;
    int     failCnt;

    /** first use malloc() instead of tst_malloc()
     ** (to avoid error logging on failure)
     **/
    failCnt = 0;
    do
    {   if( (pBlockBuffer = (Byte*)malloc(reqSize)) == NULL )
        { /* failed, not enough memory, try to use smaller buffer
           */
          failCnt++;
          if( reqSize <= vmi->blockSize )
          {  break;     /* go to final retry using tst_realloc() */
          }
          reqSize = ROUNDUPMULT(reqSize / 2, vmi->blockSize);
        }
    } while( pBlockBuffer == NULL );

    if( failCnt > 0 )   /* there has been a failure */
    {   /* do last reallocation, as final retry with 'blockSize'
         * buffer or reallocate to 'quarter' buffer in order not
         * to use too much of the available memory.
         * Further 'standard' tst_realloc() error logging is an
         * advantage over the 'silent' malloc() above.
         */
        if( pBlockBuffer != NULL )  /* last try was successful */
        { /* reduce to 'quarter' size
           */
          reqSize = ROUNDUPMULT(reqSize / 4, vmi->blockSize);
        }
        pBlockBuffer = (Byte*) tst_realloc(pBlockBuffer, reqSize,
                                           __FILE__,__LINE__);
    }
    (*pAllocSize) = reqSize;
    return pBlockBuffer;        /* success or NULL */

}   /* end allocBlockBuffer() */


#ifdef  FAKE_BIGFILES
#define FAKE_BIGFILES_MIN   (1024*(Uint64)(1024*1024))  /* 1 Gbyte */
#endif

/* readFileBodyAndTail:
 * 1) sum file body size in uctFileBodySize
 * 2) if( uctDoFileCrc )
 *      then: read file body and calculate file body crc, fake read file tail.
 *    else if( uctDoFakeRead )
 *      then: fake read file body and file tail.
 *    else: we are done.
 *
 * Mind that the file type can be anything, even FT_DIRECTORY, etc.
 *
 * FCRC_MAXBUFSIZE:
 *  buffer for file CRC calculation, multiple of the blockSize.
 */
#define FCRC_MAXBUFSIZE ((Uint32)(32*1024*1024))    /* mult of blockSize */

static bool readFileBodyAndTail( UdfMountContext *mc, Node *node )
{
       Uint64   bodySize, readChunk, bytesRead, readThisTime;
static Uint32   staticBufSize = FCRC_MAXBUFSIZE;
       Uint16   fileCrc = 0;
       Byte    *buffer;
       bool     result = TRUE;
       bool     isStream;

    /* NOTE: readChunk, etc must be Uint64, because fake read reads
     *       each file at once (also the biggest).
     */
    UCTASSERT( node != NULL && node->parent != NULL && mc != NULL );

    isStream = (   node->parent == node->parent->parent->streamDirNode
                || node->parent == mc->systemStreamDirNode );

    if( !uctDoFakeRead && !uctDoFileCRC )
    { /* update uctFileBodySize and the we are done, no (fake) read.
       * uctFileBodySize    : total file body size sum
       */
      uctFileBodySize += nodeGetEndOfFile(node, mc, TRUE);
      return TRUE;              /* we are done, no (fake) read */
    }

    /* TRUE condition: uctDoFakeRead || uctDoFileCRC
     * (fake) read File/Stream body
     */
    ifVERBOSE( (uctDoFileCRC) ? INFO02level : FAKE01level )
    {   nodePrintUnicodeNameTxtExtra( node,mc, "\tfile body read: ", "\n" );
    }
    ENDif;

    /* check if total Information Length can be read
     * uctFileBodySize    : total file body size sum
     */
    bodySize = nodeGetEndOfFile(node, mc, TRUE);
    uctFileBodySize += bodySize;

    if(   uctDoFileCRC      /* calculate CRC for all file bodies */
#ifdef FAKE_BIGFILES        /* force fake read for big files */
       && bodySize < FAKE_BIGFILES_MIN
#endif
      )
    { Uint32 bufsz = (Uint32) MIN(bodySize, (Uint64)staticBufSize);
      buffer = allocBlockBuffer(bufsz, &bufsz);
      if( buffer == NULL )
      { MLIMITbegin(ERROR00level,uctMessageLimit);
          fprintf(uctout,
            "\tFatal error: Unable to allocate a read buffer of one\n"
                 "-\t\t     block or less for -filecrc calculations.\n");
        MLIMITend;
        /** TODO: consider switching off the -filecrc option **/
        uctExit(EXIT_OUT_OF_MEMORY);        /* quit */
      }
      /* alloc ok, MIND: allocBlockBuffer() may have changed bufsz
       * (made it smaller) because not enough memory for requested size.
       */
      readChunk = MIN((Uint64)bufsz, bodySize);
      if(   readChunk < bodySize        /* requested bufsz could not */
         &&     bufsz < staticBufSize ) /*  be allocated, next time  */
      { staticBufSize = bufsz;          /*   request smaller buffer. */
      }
    }
    else        /* fake read, 'read' whole file at once */
    {
        readChunk = bodySize;
        buffer = NULL;          /* virtual buffer */
    }

    for( bytesRead = 0;
         bytesRead < bodySize && result == TRUE;
         bytesRead += readThisTime)
    {
        readChunk = MIN(readChunk, (Uint64)(bodySize - bytesRead));
        if(   !THERead(mc, node, bytesRead, readChunk, buffer, &readThisTime)
           ||  readThisTime != readChunk )      /* assert */
        { result = FALSE;
          /* read error, readThisTime is actually read nmb of bytes
           */
          MLIMITbegin(ERROR00level, uctMessageLimit);
            fprintf(uctout,
                "-\treadFileBody error: Not enough bytes: ");
            printUint64(VERBOSE00level, readThisTime, FALSE, NULL);
            fprintf(uctout, ", expected: ");
            printUint64(VERBOSE00level, readChunk, FALSE, NULL);
            fprintf(uctout, ", offset: ");
            printUint64(VERBOSE00level, bytesRead, FALSE, NULL);
            fprintf(uctout, "\n");
          MLIMITend;
          /* no break here for read error, finish this loop.
           * readThisTime may be less than readChunk
           */
        }

#ifdef  FAKE_BIGFILES       /* force fake read for big files */
        fileCrc = 0;
        if( uctDoFileCRC && buffer != NULL )
#else
        if( uctDoFileCRC )  /* normal case */
#endif
        {   /* calculate file CRC
             */
            fileCrc = chunk_cksum(buffer, (Uint32) readThisTime, fileCrc);
        }
    }   /* endfor bytesRead ... */

    /* result may be FALSE in case of a read error.
     * In that case, bytesRead is not equal to bodySize,
     * so use bytesRead for actual read size.
     */

    /* Calculate overall fiel body sizes and crc.
     * uctFileBodySize    : file body size sum
     * uctFileBodyCrc     : file body CRC
     * uctFileBodyCrcSize : file body CRC size sum
     * uctFileBodySize and uctFileBodyCrcSize may be different
     * in case of read errors.
     * The overall CRC is calculated as the Uint16 sum of all
     * fileCrc +  bytesRead      + (bytesRead>>16)
     *         + (bytesRead>>32) + (bytesRead>>48)
     * values for each file.
     * BytesRead is included because files filled with all zero
     * bytes have a fileCrc zero, independent of the file size.
     * The overall file body CRC is a quick way to see if discs
     * that should contain the same file body data indeed do, or
     * if the same disc verified on different drives indeed did
     * read the same file body data.
     * Because a modulo 16 sum is used, the final outcome is
     * independent of the order of files on a medium.
     * The outcome will be identical for an equal set of files on
     * different media that do not have the same directory structure
     * or order of files in a directory.
     */
    if( uctDoFileCRC )
    {   /* For OSTA test discs:
         * To be compared to FILECRC.TXT records,
         * see: UDF Compliance Test Disc Specification,
         *      Chapter 4.
         * Therefore CRC and size after appended path, format:
         * "\tfile CRC: <path>, 0x<hex crc>, <body size>"
         */
        VERBOSE00(uctout, "\tfile CRC: ");
        nodePrintPath(node, mc, NPP_FULL, VERBOSE00level, NULL);
        if( pFE_icbTag(node->fe)->fileType == FT_SYMBOLIC_LINK )
        {   VERBOSE00(uctout, " -> <symbolic link>\n");
        }
        else
        {   VERBOSE00(uctout, ", 0x%04X, ", fileCrc);
            printUint64(VERBOSE00level, bytesRead, FALSE, "%lu");
            VERBOSE00(uctout, "\n");
        }

        /* -filecrc option active:
         * Calculate overal File CRC, see explanation above.
         * Do not use '+=' for Uint16 in order to
         * keep compiler happy.
         */
        uctFileBodyCrc = (Uint16)
                           (    uctFileBodyCrc
                            +   fileCrc
                            + ( bytesRead      & 0xFFFF)
                            + ((bytesRead>>16) & 0xFFFF)
                            + ((bytesRead>>32) & 0xFFFF)
                            + ((bytesRead>>48) & 0xFFFF) );
        uctFileBodyCrcSize += bytesRead;
    }

    /* always do fake file tail read
     * only print message if there is one.
     * doFileTailFakeRead() needs the real bodySize (not bytesRead).
     */
    (void) doFileTailFakeRead(mc, node, bodySize);

    checkFree((void**)&buffer);
    return result;
}


/* verify directory fileLinkCount, ECMA 4/14.9.6
 * No hard links for directories, so file link count
 * shall be <number of sub-directories> + 1, and 1 extra in case
 * the directory has a stream directory.
 * This is also true for the root directory.
 * The root directory lacks the references by a FID from a higher
 * directory, but it has one extra reference from its own
 * parent FID.
 */
static bool verifyDirectoryFileLinkCount(UdfMountContext *mc, Node *node,
                                         Uint32 subdirCount)
{
    Byte *fe = node->fe;    /* points to FE or EFE, else NULL */
    ExtendedFileEntry *efe; /* points to EFE, else NULL */
    Uint16  flc;

    if( fe == NULL ) return FALSE;  /* no FE */

    if( ((Tag*)fe)->tagIdentifier == tidEFE )
         efe = (ExtendedFileEntry *) fe;
    else efe = NULL;
    /*  fe points to FE or EFE, else NULL
     * efe points to EFE,       else NULL
     */

    flc = (*(pFE_fileLinkCount(node->fe)));

    if( NODEFLAGS_IS_SET(node, NFB_STREAMDIR) )
    {   if( node == mc->systemStreamDirNode )
        {   if( flc != 1 )
            {   MLIMITbegin( ERROR00level, uctMessageLimit );
                  printMessageHead( MLIMIT.vl, node->fe,
                    (Byte*) pFE_fileLinkCount(node->fe), NULL);
                  fprintf(uctout,
                    "Error: System Stream Directory File Link Count: %lu,\n"
                    "-\t\t\texpected: exactly 1. Only identified by its own\n"
                       "-\t\t\tparent FID, ECMA 4/14.9.6, 4/8.8.3, UDF 3.3.5\n",
                       flc);
                  nodePrintUnicodeNameContLine(node,mc);
                MLIMITend;
                return FALSE;
            }
        }
        else if ( flc != 0 )
        {   MLIMITbegin( ERROR00level, uctMessageLimit );
              printMessageExpectedU32( MLIMIT.vl,
                 node->fe, (Byte*) pFE_fileLinkCount(node->fe),
                "Error: Stream Directory File Link Count",
                "%lu", flc, 0,
                ",\n-\t\t\tnot identified by any FID,"
                                        " ECMA 4/14.9.6, 4/8.8.3\n");
              nodePrintUnicodeNameContLine(node,mc);
            MLIMITend;
            return FALSE;
        }
    }
    else        /* no Stream Directory */
    {   bool hasStreamDir = (
            efe != NULL && adGetExtentSize(&efe->streamDirectoryICB) != 0);
        Uint32 expectedFlc32 = 1 + subdirCount + ((hasStreamDir) ? 1 : 0);

        if( (Uint32) flc != expectedFlc32 )
        {
            MLIMITbegin( ERROR00level, uctMessageLimit );
              printMessageExpectedU32( MLIMIT.vl,
                 node->fe, (Byte*) pFE_fileLinkCount(node->fe),
                "Error: Directory File Link Count", "%lu",
                 flc, expectedFlc32, ",\n");
              fprintf(uctout,
                "-\t\t\tfor directory with %lu subdirector%s%s,\n",
                 subdirCount, PLURAL_IES(subdirCount),
                 (hasStreamDir) ? "\n-\t\t\tand a stream directory"
                                : "");
              fprintf(uctout,
                "-\t\t\tECMA 4/14.9.6, 4/8.8.3\n");
              if( expectedFlc32 != (Uint32) ((Uint16)expectedFlc32) )
              { fprintf(uctout,
                "-\t\tNote: Too many links for Uint16 !!\n");
              }
              nodePrintUnicodeNameContLine(node,mc);
              if(     subdirCount > 1
                 && expectedFlc32 > flc )
              { fprintf(uctout,
                  "-\tNote: The above error message may also be caused by\n"
                  "-\t      illegal directory hard links.\n");
              }
            MLIMITend;
            return FALSE;
        }
    }
    return TRUE;
}

/* printFilesEtcCounts():
 * Print file, dirs, streams and stream dir counts on a single
 * continuation line.
 * Print if unconditional == TRUE or if any of the counts is nonzero.
 * Print stream and stream dir counts only if any of their counts nonzero.
 * text24 must be a 24 char text starting at a tab position, so
 * e.g. "\t\t\t" is ok.
 */
static void printFilesEtcCounts( bool  unconditional, char  *text24,
                                Uint32 cntFiles,   Uint32 cntDirs,
                                Uint32 cntStreams, Uint32 cntStreamDirs )
{
    if( unconditional ||
        cntFiles || cntDirs || cntStreams || cntStreamDirs )
    {
        VERBOSE00(uctout, "%s%4lu file%1s %3lu director%-3s",
            text24, cntFiles, PLURAL_S(  cntFiles),
                    cntDirs,  PLURAL_IES(cntDirs));
        if( cntStreams || cntStreamDirs )
        {   VERBOSE00(uctout, " %4u stream%1s",
                    cntStreams,    PLURAL_S(  cntStreams));
            if( cntStreamDirs )
            { VERBOSE00(uctout, " %3lu stream director%s",
                    cntStreamDirs, PLURAL_IES(cntStreamDirs));
            }
        }
        VERBOSE00(uctout, "\n");
    }
}

/* expandDirectoryHierarchy():
 * Used to expand normal (sub)directory trees, starting with rootNode
 * or a stream directory tree.
 * Count files, direcories, streams and streamdirs.
 * Count MFD (Marked for Delete) files, directories and streams separately.
 * Note that 'marked for delete' (MFD) stream directories do not exist
 * (no FID reference).
 * Precondition: node refers to a directory
 * nodeFeICB must define the node FE address
 */
#define EXPANDDIR_RECURSION_LIMIT 1000

static bool expandDirectoryHierarchy(
                        UdfMountContext *mc,
                        Node   *node,           LongAd *nodeFeICB,
                        Uint32 *pTotalFiles,    Uint32 *pTotalDirs,
                        Uint32 *pTotalFilesMFD, Uint32 *pTotalDirsMFD,
                        Uint32 *pTotalStreams,  Uint32 *pTotalStreamDirs,
                        Uint32 *pTotalStreamsMFD,
                        Uint32  ExpandDirRecursionCount )
{
    STATIC_RECURSIONADMIN;
            Uint32  recursionUnique32,  dummy,
                    cntLocalFiles,       cntLocalSubdirs,
                    cntLocalFilesMFD,    cntLocalSubdirsMFD,
                    cntLocalStreams,     cntLocalStreamDirs,
                    cntLocalStreamsMFD,
                    cntNestedFiles,  cntNestedSubdirs,
                    cntNestedFilesMFD,   cntNestedSubdirsMFD,
                    cntNestedStreams,    cntNestedStreamDirs,
                    cntNestedStreamsMFD;
            Uint64  streamsObjectSize = 0;
            Node   *child;
            bool    dirIsStreamDir    = NODEFLAGS_IS_SET(node, NFB_STREAMDIR),
                    dirIsMarkedDelete = NODEFLAGS_IS_SET(node, NFB_MARKED_DELETE),
                    expResult = TRUE;
            Uint32  depth;
     static Uint32  maxDepth;   /* static maintain of max depth, initialized below */

    /* ExpandDirRecursionCount starts with 0, depth and maxDepth with 1.
     * Do not count streams as extra depth level. Stream directory already
     * counts an extra level in ExpandDirRecursionCount.
     * Ignore illegal nesting of (stream) dirs, etc.
     */
    if( node == mc->rootNode || node == mc->systemStreamDirNode )
    { depth = maxDepth = 1;                 /* initialize for top level */
    }
    else if( dirIsStreamDir )
    { depth = ExpandDirRecursionCount;      /* correction for streams */
    }
    else
    { depth = ExpandDirRecursionCount + 1;  /* normal directory level */
    }
    maxDepth = MAX(depth, maxDepth);

    /* Initialize counts
     * Note that 'marked for delete' (MFD) stream directories
     * do not exist, (no FID reference).
     */
    cntLocalFiles     = cntLocalSubdirs     =
    cntLocalFilesMFD  = cntLocalSubdirsMFD  =
    cntLocalStreams   = cntLocalStreamDirs  = cntLocalStreamsMFD =
    cntNestedFiles    = cntNestedSubdirs    =
    cntNestedFilesMFD = cntNestedSubdirsMFD =
    cntNestedStreams  = cntNestedStreamDirs = cntNestedStreamsMFD = 0;

    /* check expandDirectoryHierarchy() recursion
     * Use absolute block address in nodeFeICB as unique32 number.
     * Mind that rootNode and systemStreamDirectory have no FID
     */
    if( !translateAddress(mc,
                nodeFeICB->extentLocation.partitionReferenceNumber,
                nodeFeICB->extentLocation.logicalBlockNumber,
                &recursionUnique32, &dummy, TRUE) ) /* silent */
    {   return FALSE;
    }
    if( !checkRecursion(&recursionAdmin, recursionUnique32,
                        ExpandDirRecursionCount, EXPANDDIR_RECURSION_LIMIT,
                        "expandDirectoryHierarchy") )
    {                   /* infinite resursion loop detected */
        return FALSE;   /* or recursion limit reached */
    }                   /* or memory allocation error */

    /* First expand an attached stream directory, if any.
     * For the system stream directory, this would result in a warning
     * that nested stream directories are not expanded.
     * (mind recursion)
     */
    if( node->streamDirNode != NULL )
    {   cntLocalStreamDirs++;
        if( !expandDirectoryHierarchy(
                     mc, node->streamDirNode,
                     pFE_streamDirectoryICB(node->fe),
                    &cntLocalFiles,     &cntLocalSubdirs,
                    &cntLocalFilesMFD,  &cntLocalSubdirsMFD,
                    &cntLocalStreams,   &cntLocalStreamDirs,
                    &cntLocalStreamsMFD,
                     ExpandDirRecursionCount + 1) )
        {   expResult = FALSE;
        }
    }

    /* If node is NO directory and NO stream directory, this is
     * the end of expandDirectoryHierarchy().
     */
    if(   !NODEFLAGS_IS_SET(node, NFB_DIREXPAND)
       && !NODEFLAGS_IS_SET(node, NFB_STREAMDIR) )
    {   /* return, but first add 'local' streams and streamdirs
         */
        *pTotalFiles      += cntLocalFiles      + cntNestedFiles;
        *pTotalFilesMFD   += cntLocalFilesMFD   + cntNestedFilesMFD;
        *pTotalDirs       += cntLocalSubdirs    + cntNestedSubdirs;
        *pTotalDirsMFD    += cntLocalSubdirsMFD + cntNestedSubdirsMFD;
        *pTotalStreams    += cntLocalStreams    + cntNestedStreams;
        *pTotalStreamsMFD += cntLocalStreamsMFD + cntNestedStreamsMFD;
        *pTotalStreamDirs += cntLocalStreamDirs + cntNestedStreamDirs;
        return expResult;       /* done for files, etc. */
    }

    /* NFB_DIREXPAND is set for node, so do the
     * regular (stream) directory tree expand.
     */
    ifVERBOSE(INFO01level)
    {   fprintf(uctout, "\n  ==>\t(max) depth: %2lu %2lu"
            "  Expand %s%sdirectory: ", depth, maxDepth,
            (dirIsMarkedDelete) ? "\'marked for delete\' "
                                : "",
            (dirIsStreamDir)    ? "stream "  : "");
        nodePrintUnicodeNameTxtExtra( node, mc, NULL, "\n" );
    }
    ENDif;  /* INFO01level */

    /* DO NOT EXPAND NESTED STREAM DIRECTORIES !!
     */
    if(   node->parent != NULL
       && node->parent != node
       && NODEFLAGS_IS_SET(node->parent, NFB_STREAMDIR) )
    { MLIMITbegin( ERROR00level, uctMessageLimit );
        fprintf(uctout,
          "-\tError: Nested Stream Directory, expand aborted !!\n");
        nodePrintUnicodeNameContLine(node,mc);
        fprintf(uctout,"\n");
      MLIMITend;
      return FALSE;     /* abort nested stream dir expand */
    }

    ifVERBOSE(INFO01level)
    { if( node->streamDirNode != NULL )
      { VERBOSE00(uctout, "-\t(attached streamdir expanded above)\n");
      }
      VERBOSE00(uctout, "\n");
    }
    ENDif;  /* INFO01level */

    /* start expand directory, reset globalFidIdentifierError
     * flag used by udfNodeCheckDirectoryEntries().
     */
    globalFidIdentifierError = FALSE;   /* reset */

    if( !udfNodeGetChildren(mc, node) )
    {
        VERBOSE00(uctout, "expandDirectoryHierarchy: "
                          "udfNodeGetChildren failed\n");
        return FALSE;
    }

    /* show directory with ALL entries
     * and check for ambiguous
     */
    udfNodeShowDirectory(mc, node);
    (void) udfNodeCheckDirectoryEntries(node, mc);

    /* 3 loops through all children now:
     * 1st: if child is NO legal subdirectory
     *      then read file body (files, streams, etc.)
     * 2nd: if child->streamDirNode != NULL and no
     *      stream dir nesting, then expand stream directory
     * 3rd: if child is legal subdirectory
     *      then expand subdirectory and all nested directories, etc.
     * If we are expanding a stream directory, no nested directories
     * or stream directories will be expanded.
     * Entries containing illegal or inconsistent (stream) directories
     * are handled as normal files or streams (readFileBodyAndTail()).
     */

    /* 1st loop: if child is NO legal subdirectory
     *           then read file body (files, streams, etc.)
     * Count files, subdirs and streamsObjectSize here.
     */
    for( child = node->firstChild;
         child != NULL;
         child = child->nextInDirectory )
    {
        /* no action for parent FID
         * and for deleted FID with unspecified ICB
         */
        if(   NODEFLAGS_IS_SET(child, NFB_PARENTFID)
           || NODEFLAGS_IS_SET(child, NFB_UNUSED_FID) )
        {   continue;       /* no action */
        }

        /* Count directories/files/streams with FE == NULL too.
         * The only ones that are not counted are the parent fid entry
         * and deleted entries with unspecified ICB.
         * UDF 2.2.6.4: do not count files/directories with
         * the deleted bit set, so count "Marked for Delete"
         * (MFD) files, etc. separately.
         */
        if( NODEFLAGS_IS_SET(child, NFB_DIREXPAND) )
                                  cntLocalSubdirs++;
        else if( dirIsStreamDir ) cntLocalStreams++;
        else                      cntLocalFiles++;

        if( NODEFLAGS_IS_SET(child, NFB_MARKED_DELETE) )
        {   if( NODEFLAGS_IS_SET(child, NFB_DIREXPAND) )
                                      cntLocalSubdirsMFD++;
            else if( dirIsStreamDir ) cntLocalStreamsMFD++;
            else                      cntLocalFilesMFD++;
        }

        /* For file CRC calculation or fake read, read file body
         * for all non-subdirectory FIDs with a valid FE, even
         * the ones with FCB_DELETED bit set.
         * They shall be consistent and not cause errors
         * while reading.
         * readFileBodyAndTail() will sum file body bytes in
         * uctFileBodySize (also if uctDoFakeRead and
         * uctDoFileCrc both are FALSE).
         */
        if(   isBitOff(child->nodeFlags, NFB_DIREXPAND)
           && child->fe != NULL )
        {
            if( !readFileBodyAndTail(mc, child) )
            { /* readFileBodyAndTail() error.
               * read error on *.VOB files may be caused
               * by copy protection (e.g. CSS).
               */
              Uint32 uLen = child->unicodeNameLen;
              unicode_t dotVOB[4] =
              { (unicode_t) '.', (unicode_t) 'V',
                (unicode_t) 'O', (unicode_t) 'B'
              };
              MLIMITbegin(ERROR00level, uctMessageLimit);
                fprintf(uctout, "\tError: read File Body failed\n");
                nodePrintUnicodeNameContLine(child,mc);
                if(   uLen >= 4
                   && memcmp(&child->unicodeName[uLen-4],
                              dotVOB, 4) == 0 ) /* *.VOB file */
                { fprintf(uctout,
                    "-\tNote: If read error, this may be caused by "
                                    "copy protection.\n");
                }
              MLIMITend;
            }
            if( dirIsStreamDir )
            { streamsObjectSize += (*(pFE_informationLength(child->fe)));
            }
        }
    }

    /* 2nd loop: Expand stream directory for non-directory children, if any.
     *           Test on nested stream dirs, etc.
     *           Stream directories for directory children will be expanded
     *           in the same call to expandDirectoryHierarchy() where the
     *           directory itself is expanded, see 3rd loop below.
     */
    for( child = node->firstChild;
         child != NULL;
         child = child->nextInDirectory )
    {   /* No action for parent FID
         * and for deleted FID with unspecified ICB.
         * No special test because they cannot have
         * a streamDirNode.
         */
        if(    child->streamDirNode != NULL
           && !NODEFLAGS_IS_SET(child, NFB_DIREXPAND)   /* node not expandable */
           && !expandDirectoryHierarchy(                /*  only streamDirNode */
                       mc, child, &child->fid->ICB,     /*   will be expanded. */
                      &cntNestedFiles,    &cntNestedSubdirs,
                      &cntNestedFilesMFD, &cntNestedSubdirsMFD,
                      &cntNestedStreams,  &cntNestedStreamDirs,
                      &cntNestedStreamsMFD,
                       ExpandDirRecursionCount + 1) )
        {   expResult = FALSE;
        }
    }

    /* 3rd loop: If child is legal subdirectory then first expand its
     *           stream directory - if any - and then the subdirectory
     *           itself and all nested directories, etc.
     */
    for( child = node->firstChild;
         child != NULL;
         child = child->nextInDirectory )
    {
        if(    NODEFLAGS_IS_SET(child, NFB_DIREXPAND)
           && !expandDirectoryHierarchy(
                         mc, child, &child->fid->ICB,
                        &cntNestedFiles,    &cntNestedSubdirs,
                        &cntNestedFilesMFD, &cntNestedSubdirsMFD,
                        &cntNestedStreams,  &cntNestedStreamDirs,
                        &cntNestedStreamsMFD,
                         ExpandDirRecursionCount + 1) )
        {   expResult = FALSE;
        }
    }

    (void) verifyDirectoryFileLinkCount(mc, node, cntLocalSubdirs);

    /* expand complete, show file, directory, streams, etc, counts
     * do not show streams if none found
     */
    VERBOSE00(uctout, "\n\tExpand complete, max depth %2lu for %sdirectory: ",
                        maxDepth, (dirIsStreamDir) ? "stream " : "");
    nodePrintUnicodeNameTxtExtra( node, mc, "", "\n" );

    printFilesEtcCounts(TRUE, "-\t\t\t",        /* unconditional */
                        cntLocalFiles,   cntLocalSubdirs,
                        cntLocalStreams, cntLocalStreamDirs);

    printFilesEtcCounts(FALSE, "-  incl. marked delete: ",
                        cntLocalFilesMFD,   cntLocalSubdirsMFD,
                        cntLocalStreamsMFD, 0);

    printFilesEtcCounts(FALSE,            "-\t\tnested: ",
                        cntNestedFiles,   cntNestedSubdirs,
                        cntNestedStreams, cntNestedStreamDirs );

    printFilesEtcCounts(FALSE, "-  incl. marked delete: ",
                        cntNestedFilesMFD,   cntNestedSubdirsMFD,
                        cntNestedStreamsMFD, 0);

    /* TODO: check: cntNestedStreamDirs shall be zero
     */
    *pTotalFiles      += cntLocalFiles      + cntNestedFiles;
    *pTotalFilesMFD   += cntLocalFilesMFD   + cntNestedFilesMFD;
    *pTotalDirs       += cntLocalSubdirs    + cntNestedSubdirs;
    *pTotalDirsMFD    += cntLocalSubdirsMFD + cntNestedSubdirsMFD;
    *pTotalStreams    += cntLocalStreams    + cntNestedStreams;
    *pTotalStreamsMFD += cntLocalStreamsMFD + cntNestedStreamsMFD;
    *pTotalStreamDirs += cntLocalStreamDirs + cntNestedStreamDirs;

    if( ExpandDirRecursionCount == 0 )  /* top level */
    {
      printFilesEtcCounts( TRUE, "-\t overall total: ", /* unconditional */
                          *pTotalFiles,   *pTotalDirs,
                          *pTotalStreams, *pTotalStreamDirs );
      printFilesEtcCounts( FALSE, "-  incl. marked delete: ",
                          *pTotalFilesMFD,   *pTotalDirsMFD,
                          *pTotalStreamsMFD,  0 );
      VERBOSE00(uctout, "\n\tMaximum directory depth: %lu\n", maxDepth);
    }

    /* Consistency check: Stream directory can only contain local streams
     */
    if(    dirIsStreamDir
        && (   cntLocalFiles    || cntLocalSubdirs || cntLocalStreamDirs
            || cntNestedFiles   || cntNestedSubdirs
            || cntNestedStreams || cntNestedStreamDirs) )
    { MLIMITbegin(ERROR00level,uctMessageLimit);
        fprintf(uctout,
          "\tConsistency error: Stream directory can only contain streams.\n");
      MLIMITend;
    }

    /* expand complete, test EFE objectSize for parent of
     * stream directory. For EFEs without a stream directory,
     * this test is already done in verifyFEorEFE().
     */
    if( dirIsStreamDir && isBitOff(node->nodeFlags, NFB_SYSTEM) )
    {   (void) verifyEfeObjectSize(
                (ExtendedFileEntry*) (node->parent->fe),
                streamsObjectSize, "\n", mc, node->parent );
    }

    return expResult;

}   /* end expandDirectoryHierarchy() */


/* nodeCmpFullPath(): used by qsort functions
 */
static int nodeCmpFullPath( Node *np1, Node *np2,
                            UdfMountContext *mc )
{
    static NodePointerArray npa1 = INIT_VPA_STRUCT,
                            npa2 = INIT_VPA_STRUCT;
    Uint32   n, minLen;
    int      iResult;
    Node    *tmpNode1, *tmpNode2;

    /* Put paths nodes in 2 node pointer arrays
     * starting with current nodes.
     * Use 2 static node pointer arrays of which
     * <npa>.arr will not be freed after use, saving
     * a lot of (re)alloc() and free() processing.
     */
    npa1.len = npa2.len = 0;    /* empty arrays */

    for( tmpNode2  = NULL,       tmpNode1  = np1;
         tmpNode2 != tmpNode1 && tmpNode1 != NULL;
         tmpNode2  = tmpNode1,   tmpNode1  = tmpNode1->parent )
    {   if( !nodePointerArrayAddNode(tmpNode1, &npa1) )
            uctExit(EXIT_PROGRAM_ERROR);
    }
    for( tmpNode2  = NULL,       tmpNode1  = np2;
         tmpNode2 != tmpNode1 && tmpNode1 != NULL;
         tmpNode2  = tmpNode1,   tmpNode1  = tmpNode1->parent )
    {   if( !nodePointerArrayAddNode(tmpNode1, &npa2) )
            uctExit(EXIT_PROGRAM_ERROR);
    }

    /* compare nodes of both paths
     * starting with 'root' nodes.
     */
    minLen = MIN(npa1.len, npa2.len);
    for( n  = 1,        iResult = 0;
         n <= minLen && iResult == 0;
         n++ )
    {   tmpNode1 = npa1.arr[npa1.len - n];
        tmpNode2 = npa2.arr[npa2.len - n];
        if( tmpNode1 == tmpNode2 )
        {   /* no action, identical nodes */
        }
        else if(   tmpNode1->unicodeName != NULL
                && tmpNode2->unicodeName != NULL )
        {   iResult = nodeCmpUnicodeNames(tmpNode1, tmpNode2);
        }
        else if( tmpNode1->unicodeName != NULL )
        {   return QSORT_ELEM1_LAST;    /* path 2 first */
        }
        else if( tmpNode2->unicodeName != NULL )
        {   return QSORT_ELEM1_FIRST;   /* path 1 first */
        }
        else    /* both unicode NULL */
        {   char *txt1, *txt2;
            txt1 = nodeGetNoUnicodeText(tmpNode1,mc),
            txt2 = nodeGetNoUnicodeText(tmpNode2,mc);
            iResult = strcmp(txt1, txt2);
        }
    }

    if( iResult == 0 )  /* equal so far, shortest */
    {   iResult = npa1.len - npa2.len;   /* first */
    }
    return iResult;

}   /* end nodeCmpFullPath() */

/* qsortMc: Global static agrument used by
 * qsortNodesFileLinkCount() and qsortNodesUniqueID().
 */
static UdfMountContext *qsortMc = NULL; /* static argument */

/* qsortNodesFileLinkCount():
 * qsort compare function for File Link Count tests.
 * Sort keys:
 * 1st: first if (ICB == NULL or translation error)
 * 2nd: first if physical address lower
 * 3rd: first if no FID present
 * 4th: sort on full path, needed for use in createTotalNodePointerArray().
 *      Further it results in better readable and reproducable
 *      messages on different platforms for npCheckFileLinkCounts()
 *      and createTotalNodePointerArray().
 *
 * Note:
 *  - sorting order as above is crucial because it is
 *    relied on elsewhere in the implementation.
 *  - qsortMc is a static argument that must be set to the
 *    proper value before calling qsortNodesFileLinkCount
 *  - this function will exit on assert if one of the
 *    node pointers == NULL
 */
static int qsortNodesFileLinkCount(const void *elem1,
                                           const void *elem2)
{
    Uint32  phys1 = 0, phys2 = 0, dummy;
    Node   *np1  = *((NodePointer*) elem1),
           *np2  = *((NodePointer*) elem2);
    LongAd *icb1 = nodeGetIcb(np1, qsortMc),
           *icb2 = nodeGetIcb(np2, qsortMc);

    UCTASSERT( np1 != NULL && np2 != NULL );

    /* sort icbs on physical address in order to match
     * possible icbs in virtual and non-virtual partitions.
     * first check if translation is possible. If not then
     * handle as icb == NULL.
     */
    if(    icb1 != NULL
       && !translateAddress(qsortMc,
                icb1->extentLocation.partitionReferenceNumber,
                icb1->extentLocation.logicalBlockNumber,
              &phys1, &dummy, TRUE) )   /* silent */
    {   icb1 = NULL;            /* could not translate */
    }
    if(    icb2 != NULL
       && !translateAddress(qsortMc,
                icb2->extentLocation.partitionReferenceNumber,
                icb2->extentLocation.logicalBlockNumber,
              &phys2, &dummy, TRUE) )   /* silent */
    {   icb2 = NULL;            /* could not translate */
    }

    /* 1st sort key: first if (ICB == NULL or translation error)
     */
    if(     icb1 == NULL )
    {   if( icb2 != NULL )
        {   return QSORT_ELEM1_FIRST;
        }
        return nodeCmpFullPath(np1, np2, qsortMc);
    }
    if( icb2 == NULL )
    {   return QSORT_ELEM1_LAST;
    }

    /* 2nd sort key: first if physical address lower
     * Do not use (int)(phys1-phys2) because of
     * possible overflow.
     */
    if( phys1 < phys2 ) return QSORT_ELEM1_FIRST;
    if( phys1 > phys2 ) return QSORT_ELEM1_LAST;

    /* physical locations are equal
     * 3rd sort key: first if no FID present
     * if both or none has FID,
     * then 4th sort key: sort on full path
     */
    if(     np1->fid == NULL )
    {   if( np2->fid != NULL )
        {   return QSORT_ELEM1_FIRST;
        }
    }
    else if( np2->fid == NULL )
    {   return QSORT_ELEM1_LAST;
    }
    return nodeCmpFullPath(np1, np2, qsortMc);

}   /* end qsortNodesFileLinkCount() */


/* npGroupCheckRemoveDup(),
 * Handle group of node pointers with equal physical address.
 * Add nodes to be removed to npRemove[].
 * called by createTotalNodePointerArray().
 * A group can be:
 * - empty (len == 0)
 * - contain all nodes with an undefined ICB or
 *   a translation error: (translateOk == FALSE)
 * - contain all nodes that refer to the
 *   same logical sector address.
 *
 * The group nodes have been sorted according to
 * qsortNodesFileLinkCount().
 */
static void npGroupCheckRemoveDup(
                    NodePointer      *npGroup,
                    Uint32            len,
                    bool              translateOk,
                    NodePointerArray *npRemove )
{
    Node    *np, *npFirstFID = NULL;
    Uint32   n;

#ifdef UCT_TESTING_NP
    VERBOSE00(uctout, "==> npGroupCheckRemoveDup: NPDEBUG 01: group\n");
    npPrintPath((NodePointer*)npGroup, len,
            NPP_FULL, " ", "\n", TRUE, TRUE, qsortMc); /* extra info */
    fflush(uctout);
#endif  /** UCT_TESTING_NP **/

    if(    len == 0         /* empty group */
       || !translateOk )    /* skip translation errors */
    { return;
    }

    /* check group of nodes identifying the same FE logical sector address
     * We need not to bother whether a real hard link (more than one or two
     * non-parent FIDs) or a strange multiple allocation hard link is used,
     * because we are not removing the hard linked nodes themselves, but only
     * duplicate subnodes/subtrees, that would introduce many extra FLC, etc.
     * error messages, but we have to exclude parent FIDs, because they are
     * pointing back up in the directory heirarchy.
     */
    for( n = 0; n < len; n++ )
    {   np = npGroup[n];
        if(   np->fid != NULL
           && NODEFLAGS_IS_SET(np, NFB_PARENTFID) )
        { continue;     /* skip parent FID node */
        }
        /* no parent FID node
         */
        if( npFirstFID == NULL )
        { npFirstFID = np;  /* the first one according to */
        }                       /*  qsortNodesFileLinkCount() */
        else    /* add sub-nodes of subsequent hardlinked nodes */
        {       /* to npRemove[]. Do not add np top node itself */
          /* BUG ALERT: always remove np and keep npFirstFID !!
           * (due to secondary sort on path name).
           */
          nodePointerAddHierarchy(npRemove, np, FALSE );    /* NOT processTopNode */
#ifdef  UCT_TESTING_NP
          fprintf(uctout, "==> npGroupCheckRemoveDup: remove subnodes of: ");
          nodePrintPath(np, qsortMc, NPP_FULL, VERBOSE00level, "\n");
#endif  /** UCT_TESTING_NP **/
        }
    }
}       /* end npGroupCheckRemoveDup() */


/* createTotalNodePointerArray():
 * Create node pointer array of all nodes in root directory and system stream
 * directory hierarchies, as well as a possible VAT node and metadata file nodes.
 * Remove the following nodes:
 *  1) Unused FID nodes and nodes with ICB extent size zero.
 *  2) Duplicate subnodes of hard linked nodes (not the hard linked nodes itself,
 *     but the subnodes and subtree nodes only). This can occur if:
 *     Legal cases: hard linked file with EA file or Stream Directory.
 *     Illegal cases: hard linked directories (these are not allowed but are
 *     nevertheless handled correctly), illegal 'hard linked' FE nodes, etc.
 *  NOTE: Point 2) was implemented correctly after UDF verifier release 1.3r0.
 *
 * precondition: npArray is pointer to empty and unalllocated NodePointerArray npArray.
 *
 * The resulting array is sorted according to qsortNodesFileLinkCount().
 */
static bool createTotalNodePointerArray(UdfMountContext *mc,
                                        NodePointerArray *npArray)
{
    Uint32  n, cntGroup;
    Uint32  physAddr = 0, physPrev, dummy;
    bool    result = FALSE, resultPrev = FALSE, newGroup;
    char   *errorTxt1;
    Node   *np;
    LongAd *icb;
    NodePointerArray npRemoveArray = INIT_VPA_STRUCT;   /* empty */

    checkFree((void**) &npArray->arr);  /* set NULL */
    npArray->len = npArray->alen = 0;

    /* add all nodes from the different node hierarchies to npArray->arr.
     */
    if(   !nodePointerAddHierarchy(npArray, mc->vatNode, TRUE)  /* processTopNode */
       || (    mc->metadataFile != NULL
           && !nodePointerAddHierarchy(npArray,
                            mc->metadataFile->node, TRUE))      /* processTopNode */
       || (    mc->metadataMirrorFile != NULL
           && !nodePointerAddHierarchy(npArray,
                        mc->metadataMirrorFile->node, TRUE))    /* processTopNode */
       || (    mc->metadataBitmapFile != NULL
           && !nodePointerAddHierarchy(npArray,
                        mc->metadataBitmapFile->node, TRUE))    /* processTopNode */
       || !nodePointerAddHierarchy(npArray,
                        mc->systemStreamDirNode, TRUE)          /* processTopNode */
       || !nodePointerAddHierarchy(npArray,
                                   mc->rootNode, TRUE) )        /* processTopNode */
    {   return FALSE;
    }

#ifdef UCT_TESTING_NP
    qsortMc = mc;       /* used by UCT_TESTING_NP in npGroupCheckRemoveDup() */
#endif

#ifdef UCT_TESTING_NP       /* show all nodes, unsorted */
    VERBOSE00(uctout, "==> createTotalNodePointerArray: NPDEBUG 01 unsorted:\n");
    npPrintPath((NodePointer*)npArray->arr, npArray->len, NPP_FULL,
                NULL, "\n", TRUE, TRUE, mc);    /* extra info */
    VERBOSE00(uctout, "\n");
    fflush(uctout);
#endif  /** UCT_TESTING_NP **/


    /* Remove nodes by first adding them to a separate node array npRemoveArray node pointer array
     * and then later actually remove them from npArray.
     * Nodes to be removed:
     *  1) Unused FID nodes and nodes with FID ICB extent size 0.
     *     (with message for the latter category).
     *  2) Duplicate subnodes of hard links (not the hard links itself).
     *
     * In order to find the hard links, npArray->arr is sorted first according
     * to qsortNodesFileLinkCount(). This is the same sort order as required
     * by npCheckFileLinkCounts().
     * Implementation note:
     *  1) Logical sector number of node FE is primary sort key.
     *  2) Ordering on all secondary sort keys is crucial !!
     */
    qsortMc = mc;           /* used by qsortNodesFileLinkCount() */
    qsort((void *)npArray->arr, npArray->len, sizeof(NodePointer),
          qsortNodesFileLinkCount);

#ifdef UCT_TESTING_NP       /* show all nodes, sorted */
    VERBOSE00(uctout, "==> createTotalNodePointerArray: NPDEBUG 02 sorted:\n");
    npPrintPath((NodePointer*)npArray->arr, npArray->len, NPP_FULL,
                NULL, "\n", TRUE, TRUE, mc);    /* extra info */
    VERBOSE00(uctout, "\n");
    fflush(uctout);
#endif  /** UCT_TESTING_NP **/

    /* Make groups with equal FE logical sector number.
     * Gather the nodes to be removed in a separate array npRemoveArray[].
     * npGroupCheckRemoveDup() is capable of handling empty groups (cntGroup == 0).
     */
    cntGroup = 0;           /* empty group */
    errorTxt1 = "\tError: Extent Size zero, but no deleted FID for:\n";
    for( n = 0; n < npArray->len; n++ )
    {   np  = npArray->arr[n];
        icb = nodeGetIcb(np, mc);
        /* 1) test if unused FID or ICB extent size zero
         */
        if( icb == NULL || adGetExtentSize(icb) == 0 )
        { nodePointerAddHierarchy(&npRemoveArray,   /* remove this node */
                                   np, TRUE );      /* processTopNode */
          if(   np->fid == NULL
             || isBitOff(np->fid->fileCharacteristics, FCB_DELETED) )
          { /* FID deleted bit not set, print error message.
             * Be carefull with MLIMIT... macros, if errorTxt1 is
             * printed, then rest of message must be completed
             * as well. Further mind uctGlobalError... counts.
             */
            if( errorTxt1 != NULL ) /* count and maybe print */
            { MLIMITbegin(ERROR00level, uctMessageLimit);
                fprintf(uctout, errorTxt1);
                errorTxt1 = NULL;   /* print only once */
              MLIMITend;
            }
            if( errorTxt1 == NULL ) /* errorTxt1 printed, add path name */
            { fprintf(uctout,"-\t       ");
              nodePrintPath(np, mc, NPP_FULL, VERBOSE00level, "\n");
            }
          }
#ifdef  UCT_TESTING_NP
          fprintf(uctout, "==> createTotalNodePointerArray: remove tree: ");
          nodePrintPath(np, qsortMc, NPP_FULL, VERBOSE00level, "\n");
#endif  /** UCT_TESTING_NP **/
        }
        /* Determine groups and for each group test and addition of duplicate
         * subnodes of hard links to npRemoveArray by npGroupCheckRemoveDup().
         */
        physPrev = physAddr;
        resultPrev = result;
        result = FALSE;
        if( icb != NULL )
        {   result = translateAddress(mc,
                 icb->extentLocation.partitionReferenceNumber,
                 icb->extentLocation.logicalBlockNumber,
                &physAddr, &dummy, TRUE);   /* silent */
        }
        newGroup = ( n != 0 && (  (result != FALSE && physAddr != physPrev)
                                || result != resultPrev ) );

        if( newGroup )  /* handle previous group if any */
        {   npGroupCheckRemoveDup(
                (NodePointer*) &npArray->arr[n-cntGroup],
                                cntGroup, resultPrev,
                               &npRemoveArray );
            cntGroup = 0;
        }
        cntGroup++;
    }
    /* handle final group, if any
     */
    npGroupCheckRemoveDup(
        (NodePointer*) &npArray->arr[n-cntGroup],
                        cntGroup, resultPrev,
                       &npRemoveArray );

#ifdef UCT_TESTING_NP   /* show nodes to be removed, unsorted (maybe duplicates) */
    VERBOSE00(uctout, "==> createTotalNodePointerArray: NPDEBUG 03 npRemove:\n");
    npPrintPath((NodePointer*)npRemoveArray.arr, npRemoveArray.len, NPP_FULL,
                NULL, "\n", TRUE, TRUE, mc);    /* extra info */
    VERBOSE00(uctout, "\n");
    fflush(uctout);
#endif  /** UCT_TESTING_NP **/

    /* Do the actual remove:
     * Remove all nodes in npRemoveArray[] from npArray[].
     * There may be multiple occurences of a specific node in npRemoveArray[].
     * First sort npRemoveArray[] using qsortNodesFileLinkCount().
     * (the same way as npArray[] is already sorted).
     */
    qsortMc = mc;           /* used by qsortNodesFileLinkCount() */
    qsort((void *)npRemoveArray.arr, npRemoveArray.len, sizeof(NodePointer),
          qsortNodesFileLinkCount);

#ifdef UCT_TESTING_NP       /* show nodes to be remove, sorted */
    VERBOSE00(uctout, "==> createTotalNodePointerArray: NPDEBUG 04 npRemove sorted:\n");
    npPrintPath((NodePointer*)npRemoveArray.arr, npRemoveArray.len, NPP_FULL,
                NULL, "\n", TRUE, TRUE, mc);    /* extra info */
    VERBOSE00(uctout, "\n");
    fflush(uctout);
#endif  /** UCT_TESTING_NP **/

    for( n = 0; n < npRemoveArray.len; n++ )
    { Node *npRem = npRemoveArray.arr[n];
      if(   n == 0                              /* first one or ... */
         || npRem != npRemoveArray.arr[n-1] )   /* not previous one */
      { Uint32 x, xLen = npArray->len;          /* remove npRem from */
        for( x = 0; x < npArray->len; x++ )     /* from npArray->arr */
        { if( npArray->arr[x] == npRem )        /* [x] is the one */
          { for( ; (x+1) < npArray->len; x++ )
            { npArray->arr[x] = npArray->arr[x+1];
            }
            npArray->len--;
            break;
          }
        }
        UCTASSERT( npArray->len == (xLen-1) );
      }
    }

    /* Nodes are removed from npArray now. Free npRemoveArray.arr.
     * We could also shrink the allocation for npArray->arr[] using
     * tst_realloc(), but it is not worth the trouble.
     */
    checkFree((void**) &npRemoveArray.arr);     /* NULL, clean up */

#ifdef UCT_TESTING_NP       /* show all nodes, sorted */
    VERBOSE00(uctout, "==> createTotalNodePointerArray: NPDEBUG 05 after remove:\n");
    npPrintPath((NodePointer*)npArray->arr, npArray->len, NPP_FULL,
                NULL, "\n", TRUE, TRUE, mc);    /* extra info */
    VERBOSE00(uctout, "\n");
    fflush(uctout);
#endif  /** UCT_TESTING_NP **/

    return TRUE;

}   /* end createTotalNodePointerArray() */


/* markNodesForUniqueIdMap()
 * Called by npHandleFileLinkCountGroup() for each group
 * of nodes sorted according to qsortNodesFileLinkCount().
 * Set NFB_UNIQUEID_MAP bit for all nodes relevant for
 * the UniqueID mapping table.
 * For UDF 2.00+, all 'main stream' nodes are relevant
 * because of different FID UDF UniqueID values,
 * For UDF 1.50-, only one link of a hard linked group is
 * relevant because the group shares the (E)FE UniqueID.
 *
 * Exception: NFB_UNIQUEID_MAP is also set for the VAT node.
 */
static bool markNodesForUniqueIdMap(NodePointer *npGroup,
                                    Uint32 len,
                                    Uint16 udfRevision)
{   Node  *np;
    Uint32 n;
    bool   firstLinkFound = FALSE;

    /* all 'main stream' nodes (includes VAT node)
     * except parent FID, rootDir and SysStreamDir.
     */
    for( n = 0; n < len; n++ )
    { np = npGroup[n];
      if(  !NODEFLAGS_IS_SET(np, NFB_PARENTFID)
         && np == nodeGetMainNode(np)   /* 'main stream' node */
         && (   np->fid != NULL     /* no rootDir and no SysStreamDir */
             || NODEFLAGS_IS_SET(np, NFB_VAT)) )    /* or VAT */
      {
        if(    udfRevision >= 0x200 /* FID UniqueID */
           || !firstLinkFound )
        { firstLinkFound = TRUE;
          NODEFLAGS_SET_BIT(np, NFB_UNIQUEID_MAP);
        }
      }
    }
    return TRUE;
}

/* npHandleFileLinkCountGroup(),
 * Handle group of node pointers, as
 * called by npCheckFileLinkCounts().
 * A group can be:
 * - empty (len == 0)
 * - contain all nodes with an undefined ICB or
 *   a translation error: (translateOk == FALSE)
 * - contain all nodes that refer to the
 *   same physical address in <physAddr>.
 *   <cntFids> nodes of this group have a FID.
 *
 * The nodes have been sorted according to
 * qsortNodesFileLinkCount().
 */
static void npHandleFileLinkCountGroup(
                    NodePointer *npGroup,
                    Uint32 len, Uint32 cntFids,
                    Uint32 physAddr, bool translateOk,
                    UdfMountContext *mc)
{   Node    *np;
    Uint16   udfRevision = getUctUdfRevision(),
             flc = 0;
    Uint32   n, cntHardlinks, cntParentFids,
                cntNoHardlinkAllowed;
    bool     foundFE, flcError;

    if( len == 0 ) return;  /* empty group */

    if( !translateOk )      /* error group */
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
          "\n\tError: Undefined ICB or"
                " address translation error for:\n");
        npPrintPath(npGroup, len, NPP_FULL, "-", "\n",
                    TRUE, FALSE, mc);   /* markNode */
      MLIMITend;
      return;       /* done */
    }

    /* address translation ok,
     * debug test, do not remove for the moment
     * only one node can have no FID
     * (root dir, SSD, Metadata file, metadata mirror file)
     */
    if( len > cntFids + 1 )
    { Uint32 x;
      MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
          "\nnpHandleFileLinkCountGroup debug error: "
                    "Number of Fids: %lu of %lu, PLEASE REPORT !!\n"
            "- %lu object%s with the same address, but only %lu"
                                    " %s referenced by a FID.\n",
                cntFids, len, len, PLURAL_S(len),
                cntFids, PLURAL_ARE(cntFids));
        for( x = 0; x < len; x++ )
        { fprintf(uctout, "- ");
          nodePrintPath(npGroup[x], mc, NPP_FULL,
                        VERBOSE00level, "\n");
        }
      MLIMITend;
    }

    /* mark nodes relevant for UniqueID mapping as
     * preparation for npFinalCheckUniqueID().
     */
    (void) markNodesForUniqueIdMap(npGroup, len, udfRevision);

    /* check group of node identifying physAddr
     * count Parent FIDs.
     * count Nodes for which no hardlinks are allowed.
     */
    cntParentFids = cntNoHardlinkAllowed = 0;
    foundFE = flcError = FALSE;
    for( n = 0; n < len; n++ )
    {   np = npGroup[n];
        if(        NODEFLAGS_IS_SET(np, NFB_PARENTFID) )
        {   cntParentFids++;
        }
        else if(   NODEFLAGS_IS_SET(np, NFB_EA)
                || NODEFLAGS_IS_SET(np, NFB_DIREXPAND)
                || NODEFLAGS_IS_SET(np, NFB_STREAMDIR)
                || NODEFLAGS_IS_SET(np, NFB_STREAM)
               )
        {   cntNoHardlinkAllowed++;
        }
        if( np->fe != NULL )
        {   foundFE = TRUE;
            flc = *(pFE_fileLinkCount(np->fe));
            if( flc != cntFids )
            {   flcError = TRUE;
            }
        }
    }
    UCTASSERT( cntFids >= cntParentFids );
    cntHardlinks = cntFids - cntParentFids;

    /* Show hardlinks before file link count error.
     * Do not consider parent FIDs as hardlinks.
     * Ordinary files can also have a parent FID
     * pointing to them in case of a stream directory.
     * Verify hardlinks FID UniqueID values for UDF 2.00+,
     * they shall not be equal. The (E)FE UniqueID is
     * always equal because hard links share the (E)FE.
     */
    if( cntHardlinks > 1 )  /* hardlinks present, show it */
    {   bool foundHL = FALSE;
        for( n = 0; n < len; n++ )
        {   np = npGroup[n];
            if(    np->fid != NULL
               && !NODEFLAGS_IS_SET(np, NFB_PARENTFID) )
            { /* hard link */
              if( !foundHL )            /* first one */
              { Uint32  physad, dummy;
                LongAd *icb = nodeGetIcb(np, mc);
                foundHL = TRUE;
                (void) translateAddress(mc,
                    icb->extentLocation.partitionReferenceNumber,
                    icb->extentLocation.logicalBlockNumber,
                    &physad, &dummy, TRUE);     /* silent */
                VERBOSE00(uctout,
                    "\n%7lu\tNote: %lu hard linked %s%s:\n",
                        physad, cntHardlinks,
                        (cntNoHardlinkAllowed) ? "nodes"
                                               : "files",
                        (udfRevision >= 0x200)
                        ? " (path preceded by FID UniqueID)"
                        : "");
              }
              VERBOSE00(uctout,"-\t");
              if( udfRevision >= 0x200 )    /* show and check UDF UniqueID */
              { Uint32 fidUdfUniqueID =
                  np->fid->ICB.implementationUse.ImpUse.UDFUniqueID;
                VERBOSE00(uctout, "#%08lX ", fidUdfUniqueID);
              }
              nodePrintPath(np, mc, NPP_FULL,       /* show path */
                            VERBOSE00level, "\n");
            }
        }
    }

    /* test for illegal harlinks
     */
    if(   cntHardlinks > 1              /* hardlinks present, */
       && cntNoHardlinkAllowed != 0 )   /* illegal hardlinks  */
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
          "\n\tError: Illegal hard links in list above."
                                        " No hard links allowed\n"
           "-\t       for directories, EA files, streams and"
                                          " stream directories.\n"
           "-\t       This may be fatal and may cause many"
                                        " unpredictable verifier\n"
           "-\t       error messages.\n");
                /** TODO: spec references ?? **/
      MLIMITend;
    }

    /* now FE and File Link Count messages
     * error (or warning ??) if no FE found.
     * normally FE reading was cancelled
     * because of some error in FE.
     */
    if( !foundFE )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        VERBOSE00(uctout,
            "\n\tError: Could not find FE for:\n");
        npPrintPath(npGroup, len, NPP_FULL, "-", "\n",
                    TRUE, FALSE, mc);   /* markNode */
      MLIMITend;
    }
    else if( flcError )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
          "\n\tError: File Link Count: %lu, expected:"
                        " %lu, ECMA 4/14.9.6,\n"
             "-\t%6lu FID%s identif%s an ICB translating"
                        " to physical address %lu for:\n",
            flc, cntFids, cntFids, PLURAL_S(cntFids),
            (cntFids!=1) ? "y" : "ies", /* NOT PLURAL_IES() */
            physAddr);
        npPrintPath(npGroup, len, NPP_FULL, "-", "\n",
                    TRUE, FALSE, mc);   /* markNode */
      MLIMITend;
    }

}   /* end npHandleFileLinkCountGroup() */

/* Check file link counts of all nodes in node pointer array npa.
 * The nodes in npa->arr must already be sorted according to
 * qsortNodesFileLinkCount().
 * return FALSE for fatal error only, else TRUE.
 */
static bool npCheckFileLinkCounts( NodePointerArray *npa,
                                   UdfMountContext  *mc )
{   Node   *np;
    LongAd *icb;
    Uint32  n, cntGroup, cntFids;
    Uint32  physAddr = 0, physPrev, dummy;
    bool    result = FALSE, resultPrev, newGroup;

    /* npa->arr is sorted according to qsortNodesFileLinkCount().
     * Make groups with equal physical address and check the
     * group file link count.
     * The first group may be a group of undefined icb and translate
     * error nodes. npHandleFileLinkCountGroup() is capable of handling
     * empty groups (cntGroup == 0).
     */
    cntGroup = cntFids = 0;         /* empty group */
    for( n = 0; n < npa->len; n++ )
    {   np  = npa->arr[n];
        icb = nodeGetIcb(np, mc),
        physPrev = physAddr;
        resultPrev = result;
        result = FALSE;
        if( icb != NULL )
        {   result = translateAddress(mc,
                 icb->extentLocation.partitionReferenceNumber,
                 icb->extentLocation.logicalBlockNumber,
                &physAddr, &dummy, TRUE);   /* silent */
        }
        newGroup = ( n != 0 && (  (result != FALSE && physAddr != physPrev)
                                || result != resultPrev ) );

        if( newGroup )      /* handle previous group if any */
        {   npHandleFileLinkCountGroup(
                (NodePointer*) &npa->arr[n-cntGroup],
                cntGroup, cntFids, physPrev, resultPrev, mc);
            cntGroup = cntFids = 0;
        }
        cntGroup++;
        if( np->fid != NULL )
        {   cntFids++;
        }
    }
    /* handle final group, if any
     */
    npHandleFileLinkCountGroup(
            (NodePointer*)&npa->arr[n-cntGroup],
            cntGroup, cntFids, physAddr, result, mc);

    return TRUE;

}   /* end npCheckFileLinkCounts() */


/* qsortNodesUniqueID():
 * qsort compare function for final UniqueID tests.
 * Sort keys:
 * 1st: last if UniqueID not found (error nodes)
 * 2nd: first node marked by NFB_UNIQUEID_MAP node flag.
 * 3nd: first for lowest uniqueid
 * 4th: sort on full path, this may not be needed for
 *      proper functioning, but it results in better
 *      readable and on different platforms more
 *      reproducable npCheckFileLinkCounts() messages.
 *
 * Note:
 *  - sorting order as above is crucial because it is
 *    relied on elsewhere in the implementation.
 *  - qsortMc is a static argument that must be set to the
 *    proper value before calling qsortNodesUniqueID
 *  - this function will exit on assert if one of the
 *    node pointers == NULL
 */
static int qsortNodesUniqueID(const void *elem1,
                                      const void *elem2)
{
    Node   *np1  = *((NodePointer*) elem1),
           *np2  = *((NodePointer*) elem2);
    Uint64  uniq1, uniq2;
    bool    ok1, ok2, isRelevant1, isRelevant2, dummy;

    UCTASSERT( np1 != NULL && np2 != NULL );

    /* find uniqueIDs
     */
    ok1 = nodeGetUniqueID(np1, &uniq1, &dummy);
    ok2 = nodeGetUniqueID(np2, &uniq2, &dummy);
    if( !ok1 )
    {   if( !ok2 )                  /* both not ok */
        {   return nodeCmpFullPath(np1, np2, qsortMc);
        }
        return QSORT_ELEM1_LAST;    /* !ok1 &&  ok2 */
    }
    if( !ok2 )
    {   return QSORT_ELEM1_FIRST;   /*  ok1 && !ok2 */
    }

    /* ok1 == ok2 == TRUE
     * nodes with NFB_UNIQUEID_MAP bit set first.
     */
    isRelevant1 = NODEFLAGS_IS_SET(np1, NFB_UNIQUEID_MAP);
    isRelevant2 = NODEFLAGS_IS_SET(np2, NFB_UNIQUEID_MAP);

    if( isRelevant1 != isRelevant2 ) /* only one is relevant */
    {   if( isRelevant1 )               /* elem1 is relevant */
        {   return QSORT_ELEM1_FIRST;
        }
        return QSORT_ELEM1_LAST;        /* elem2 is relevant */
    }

    /* isRelevant1 == isRelevant2
     * sort on UniqueID
     */
    if( uniq1 < uniq2 )
    {   return QSORT_ELEM1_FIRST;
    }
    if( uniq1 > uniq2 )
    {   return QSORT_ELEM1_LAST;
    }
    return nodeCmpFullPath(np1, np2, qsortMc);
}

/* npFinalCheckUniqueIDprintMessage():
 * print multiple defined UniqueID message
 * for npFinalCheckUniqueID().
 * Unless (uctMessageLimit == 0),
 * truncate list length to:
 */
#define FCU_MAX_LIST_LEN 10

static void npFinalCheckUniqueIDprintMessage(
                NodePointerArray *npa,
                UdfMountContext  *mc,
                Uint32            firstIndex,
                Uint32            nmbOfNodes )
{   Uint32 maxLen;

    maxLen = nmbOfNodes;
    if( uctMessageLimit != 0 )  /* maybe truncate */
    {   maxLen = MIN(maxLen, FCU_MAX_LIST_LEN);
    }
    MLIMITbegin(ERROR00level, uctMessageLimit);
      fprintf(uctout,
        "\tError: %lu files or directories with"
                    " identical UniqueID, UDF 3.2.1.1.\n",
                        nmbOfNodes);
      npPrintPath((NodePointer*)&npa->arr[firstIndex],
            maxLen, NPP_FULL, "-", "\n",
            (getUctUdfRevision() >= 0x200), /* maybe show "FID" */
            TRUE, mc);                      /* showUniqueID */
      if( maxLen != nmbOfNodes )
      { fprintf(uctout,
            "- ...\tNote: Above path list length truncated from %lu to %lu.\n",
                    nmbOfNodes, maxLen);
      }
    MLIMITend;
    /* count (nmbOfNodes-1) for 'total error occurrences'
     * One counted already in above MLIMIT clause.
     */
    uctGlobalErrorOccurrences += (nmbOfNodes - 2);
}

/* Check multiple occurences of UniqueID value for all
 * 'main stream' nodes in node pointer array npa.
 * All other UniqueID rules are tested
 *  in verifyFidOrFeUniqueID()
 * and verifyFidFeUniqueIdConsistency() already.
 *
 * return FALSE for fatal error only, else TRUE.
 */
static bool npFinalCheckUniqueID( NodePointerArray *npa,
                                  UdfMountContext  *mc )
{   Node   *node;
    Uint64  uniqueID = 0, prevUniqueID = 0;
    Uint32  n, cntEq;

    /* Sort Node Pointers according to UniqueID, 'main stream'
     * nodes relevant for UniqueID mapping first.
     * These nodes are marked by the NFB_UNIQUEID_MAP
     * node flag set in markNodesForUniqueIdMap(),
     * called by npHandleFileLinkCountGroup().
     * For further sort details see qsortNodesUniqueID().
     */
    qsortMc = mc;   /* used by qsortNodesUniqueID() */
    qsort((void *)npa->arr, npa->len, sizeof(NodePointer),
          qsortNodesUniqueID);

#ifdef UCT_TESTING_NP       /* show all nodes, sorted */
    VERBOSE00(uctout, "==> npFinalCheckUniqueID : NPDEBUG 01:\n");
    npPrintPath((NodePointer*)npa->arr, npa->len, NPP_FULL,
                NULL, "\n", TRUE, TRUE, mc);    /* extra info */
    VERBOSE00(uctout, "\n");
    fflush(uctout);
#endif  /** UCT_TESTING_NP **/

    /* Now first group of nodes are marked with
     * NFB_UNIQUEID_MAP bit set and sorted on
     * relevant uniqueid (FID or (E)FE UniqueID
     * depending on UDF revision).
     * Check multiple occurrence of relevant UniqueID
     * for this group of nodes.
     */
    cntEq = 0;
    for( n = 0; n < npa->len; n++ )
    {   bool dummy;
        node = npa->arr[n];
        if( !NODEFLAGS_IS_SET(node, NFB_UNIQUEID_MAP) )
        {           /* no more relevant nodes, done */
            break;  /* maybe print last message    */
        }
        if( !nodeGetUniqueID(node, &uniqueID, &dummy) )
        { UCTASSERT( FALSE );
        }
        if( n != 0 )
        {   if( uniqueID == prevUniqueID )
            {   cntEq++;
            }
            else if( cntEq != 0 )   /* print error */
            {   cntEq++;
                npFinalCheckUniqueIDprintMessage(
                            npa, mc, n - cntEq, cntEq );
                cntEq = 0;
            }
        }
        prevUniqueID = uniqueID;
    }   /* endfor */

    /* maybe print last error message
     */
    if( cntEq != 0 )
    {   cntEq++;
        npFinalCheckUniqueIDprintMessage(
                    npa, mc, n - cntEq, cntEq );
        cntEq = 0;
    }
    return TRUE;
}


/* addExtentToMultAlloc():
 * add extent to mc->multAlloc list
 * convert to LongAd ADs
 * the partRef number may be virtual.
 */
static bool addToMultAlloc( UdfMountContext *mc,
                            Uint32 startLocation,
                            Uint32 nrOfBlocks,
                            Uint16 partitionRef )
{
    const MediumInfo  *vmi = getTheMediumInfo();
    UdfAllocationList *mal = mc->multAlloc;
    LongAd longAd;

    /* create empty list if none present
     */
    if( mal == NULL )
    {   mal = mc->multAlloc = createNewAllocationList(ADT_LONG);
    }
    if( mal == NULL )
    {   return FALSE;       /* fatal error */
    }

    /* extent type 0, means: ADEL_RECORDED_AND_ALLOCATED
     */
    longAd.extentLocation.logicalBlockNumber = startLocation;
    longAd.extentLength                      = nrOfBlocks * vmi->blockSize;
    longAd.extentLocation.partitionReferenceNumber = partitionRef;

    if( !alAddDescriptor(mal, (AnyAllocationDescriptor*) &longAd, FALSE) )
    {   return FALSE;       /* fatal error */
    }
    return TRUE;
}

/* markAndVerifyPAHandleRange():
 * If presented range is contiguous to previous range,
 * then: extend range
 * else: for pass 1: add range to mc->multAlloc list
 *       for pass 2: print previous range message
 *       start a new range.
 * endif
 *
 * Notes:
 *  - Presenting an empty range will result
 *    in closing previous range, if any.
 *  - If partRef or translateOk is not equal to the value
 *    of the previous call, discontiguity is assumed
 *    and the previous range will be closed.
 *  - A message for a range with translateOk == FALSE
 *    will print the address range as logical
 *    in partition partRef, using "(loc,partRef)",
 *    else a physical address range is assumed.
 */
static bool markAndVerifyPAHandleRange(
                UdfMountContext *mc,
                Uint32 startLocation,
                Uint32 nrOfBlocks,
                Uint16 partRef,
                bool   translateOk,
                bool   initializeRange,
                int    pass )
{
    static Uint32 beginRange = 0;       /* empty */
    static Uint32 endRange   = 0;       /* range */
    static Uint16 previousPartRef;
    static bool   previousTranslateOk;
           Uint32 nrBlocks;

    if(   initializeRange       /* ignore previous range */
       || beginRange == endRange )
    {   beginRange = endRange = startLocation;
        previousPartRef = partRef;
        previousTranslateOk = translateOk;
    }
    /* check if contiguous to registered  range
     */
    if(   startLocation == endRange
       &&       partRef == previousPartRef
       &&   translateOk == previousTranslateOk
       &&    nrOfBlocks != 0 )
    {
        endRange += nrOfBlocks;     /* extend range */
    }
    else if( (nrBlocks = endRange - beginRange) != 0 )
    {   /* handle previous non-empty range.
         * pass 1: add to mc->multAlloc.
         * pass 2: print message
         */
        if(    pass == 1        /* add to mc->multAlloc */
           && !addToMultAlloc(mc, beginRange,
                              nrBlocks, previousPartRef) )
        {   return FALSE;       /* fatal error */
        }
        else if( pass == 2 )    /* print message */
        {
            if( previousTranslateOk )
                 VERBOSE00(uctout,   "-%6lu \t", beginRange);
            else VERBOSE00(uctout, "- (%4lu,%u)\t",
                                beginRange, previousPartRef);
            VERBOSE00(uctout,
                "%3lu multiple allocated block%s\n",
                        nrBlocks, PLURAL_S(nrBlocks));
        }
        /* init for new range
         */
        return markAndVerifyPAHandleRange(mc, startLocation,
                                          nrOfBlocks, partRef,
                                          translateOk, TRUE, pass);
    }
    return TRUE;
}


/* markAndVerifyPartitionAllocation():
 * Note that sizeAndType, contains a 30 bits extent Size and a
 * 2 bits extent type as defined for extentLength in ECMA 4/14.14.1.1.
 * For extents of type ADEL_NOT_RECORDED_NOT_ALLOCATED,
 * no mark allocation is done.
 *
 * NOTE: see buildPartitionBitmaps() for two pass
 *       approach explanation.
 */
static bool markAndVerifyPartitionAllocation(
                Uint16   partRefNr,
                Uint32   logicalBlockNr,
                Uint32   sizeAndType,   /* ECMA 4/14.14.1.1 */
                char    *idText,
                UdfMountContext *mc,
                Node            *node,
                int              pass )
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32 lb, nrOfBlocks, nrOfBlocksMarkedAlready,
           chunkSize, physChunkSize, cntTotal;
    Uint8  extentType;
    bool   printErrorMessage = FALSE;

    nrOfBlocks = ROUNDUPELEMENTS(elGetExtentSize(sizeAndType),
                                 vmi->blockSize);
    extentType = elGetExtentType(sizeAndType);

    /* do not mark zero size and unallocated extents
     */
    if(    nrOfBlocks == 0
       ||  extentType == ADEL_NOT_RECORDED_NOT_ALLOCATED )
    {   return TRUE;        /* done */
    }

    /* first inspect if blocks are allocated already.
     */
    if( !markPartitionAllocation( mc, partRefNr,
            logicalBlockNr, nrOfBlocks,
            &nrOfBlocksMarkedAlready, TRUE ) )  /* inspectOnly */
    {   return FALSE;       /* fatal error */
    }
    if( nrOfBlocksMarkedAlready == 0 )
    {   /* no blocks marked already,
         * mark them and we'r done
         */
        return markPartitionAllocation( mc, partRefNr,
                logicalBlockNr, nrOfBlocks,
                &nrOfBlocksMarkedAlready, FALSE );  /* mark */
    }

    /* nrOfBlocksMarkedAlready != 0.
     * At least a part of the blocks is marked already.
     * Find out which blocks, and mark all.
     * For pass 1:
     *      Add multiple allocated blocks to mc->multAlloc
     *      list (convert to LongAd), so use logical addresses.
     *      No error messages.
     * For pass 2:
     *      Prepare for error message, no message printed
     *      if message limit exhausted, etc.
     *      Convert to physical addresses in error messages.
     */
    if( pass == 2 )
    {
      MLIMITbegin(ERROR00level, uctMessageLimit);   /* error */
        printErrorMessage = TRUE;
        fprintf(uctout,
           "\t Error: %3lu multiple allocated block%s"
                                        " in %s extent.\n"
          "-\textent: %3lu block%s, logical address: (%lu,%lu).\n",
                                 nrOfBlocksMarkedAlready,
                        PLURAL_S(nrOfBlocksMarkedAlready),
                        idText, nrOfBlocks, PLURAL_S(nrOfBlocks),
                        logicalBlockNr, partRefNr);
        /* in next for() loop:
         * log exact physical contiguous ranges.
         */
      MLIMITend;
    }

    /* if number of blocks marked already is not equal
     * to nrOfBlocks, then mark blocks one by one to
     * see which ones were exactly marked already.
     * Note that printErrorMessage can only be TRUE for pass 2
     * In the error message, show (physically) contiguous
     * blocks as one range.
     */
    chunkSize = nrOfBlocks;
    if( nrOfBlocksMarkedAlready != nrOfBlocks )
    {   chunkSize = 1;
    }
    for( cntTotal = 0, lb = logicalBlockNr;
         cntTotal < nrOfBlocks;
         cntTotal += physChunkSize, lb += physChunkSize )
    {
        Uint32  physAddr = 0,
                contigBlocks;
        bool    translateOk;

        chunkSize = MIN(chunkSize, nrOfBlocks - cntTotal);
        physChunkSize = chunkSize;
        if( printErrorMessage )
        {   translateOk = translateAddress(mc, partRefNr, lb,
                                &physAddr, &contigBlocks, TRUE);
            if( translateOk )
            {   physChunkSize = MIN(chunkSize, contigBlocks);
            }
        }
        else        /* no error message */
        {   translateOk = FALSE;    /* stick to logical extents */
        }

        if( !markPartitionAllocation(mc, partRefNr, lb, physChunkSize,
                        &nrOfBlocksMarkedAlready, FALSE) )  /* mark */
        {   return FALSE;   /* fatal error */
        }
        if(    nrOfBlocksMarkedAlready != 0
           && (pass == 1 || printErrorMessage) )
        {   /* if translateOk, use physical address, else logical address
             */
            if( !markAndVerifyPAHandleRange(mc,
                        (translateOk) ? physAddr : lb,
                         nrOfBlocksMarkedAlready,
                         partRefNr, translateOk,
                        (cntTotal == 0), pass) )    /* init first time */
            {   return FALSE;   /* fatal error */
            }
        }
    }
    /* handle last range, if any
     */
    if( !markAndVerifyPAHandleRange(mc, 0, 0, partRefNr,
                                    FALSE, FALSE, pass) )
    {   return FALSE;   /* fatal error */
    }
    if( printErrorMessage )
    {   if( node != NULL )
        {   npPrintPath(&node, 1, NPP_FULL, "-\t", "\n",
                         FALSE, FALSE, mc);
        }
        VERBOSE00(uctout,"\n");     /* separate mult alloc messages */
    }
    return TRUE;

}   /* end markAndVerifyPartitionAllocation() */

static bool markLongAdPartitionAllocation(
                    LongAd *lad, char *idText,
                    UdfMountContext *mc, Node *node, int pass )
{
    if( lad != NULL )
    {   return markAndVerifyPartitionAllocation(
                lad->extentLocation.partitionReferenceNumber,
                lad->extentLocation.logicalBlockNumber,
                lad->extentLength,  /* size and type */
                idText, mc, node, pass);
    }
    return TRUE;
}

static bool markShortAdPartitionAllocation(
                    ShortAd *sad, Uint16 partRefNr, char *idText,
                    UdfMountContext *mc, Node *node, int pass )
{
    if( sad != NULL )
    {   return markAndVerifyPartitionAllocation( partRefNr,
                        sad->extentPosition,
                        sad->extentLength,  /* size and type */
                        idText, mc, node, pass);
    }
    return TRUE;
}

static bool markAlPartitionAllocation( UdfAllocationList *al,
                                       Uint16 shortPartRef,
                                       Node *node,
                                       UdfMountContext *mc,
                                       int pass )
{
    UdfAllocationItem *ai;
    bool result = TRUE;

    if( al == NULL ) return TRUE;

    for( ai = al->head; ai != NULL; ai = ai->next )
    {
        switch( al->itemAdType )
        {
        case ADT_SHORT:
            result = markShortAdPartitionAllocation(&ai->aad.shortAd,
                        shortPartRef, "short AD", mc, node, pass);
            break;
        case ADT_LONG:
            result = markLongAdPartitionAllocation(&ai->aad.longAd,
                                    "long AD", mc, node, pass);
            break;
        }
        if( !result )
        { return FALSE;         /* fatal error */
        }
    }
    /* now overheadList allocation, recursion.
     */
    if( !markAlPartitionAllocation(al->overheadList, shortPartRef,
                                   node, mc, pass) )
    { return FALSE;
    }
    return TRUE;

}   /* end markAlPartitionAllocation() */

/* preloadBitmapsWithMultAllocList():
 * preload partition bitmaps with mc->multAlloc extents, no messages
 * for possible multiple allocations in mc->multAlloc extents.
 * Therefore use markPartitionAllocation() directly here instead of
 * markAndVerifyPartitionAllocation() via markAlPartitionAllocation().
 *
 * Implementation is simplified version of markAlPartitionAllocation().
 *  Only LongAd, no overheadList.
 *  Use:        markPartitionAllocation()
 *  instead of: markLongAdPartitionAllocation();
 */
static bool preloadBitmapsWithMultAllocList(UdfMountContext *mc,
                                            Uint32 *pNmbOfBlocks)
{
    const MediumInfo  *vmi = getTheMediumInfo();
    UdfAllocationItem *ai;

    if( pNmbOfBlocks != NULL )
    {   *pNmbOfBlocks = 0;
    }
    if( mc->multAlloc == NULL )
    {   return TRUE;                /* done */
    }
    UCTASSERT(   mc->multAlloc->itemAdType == ADT_LONG
              && mc->multAlloc->overheadList == NULL );

    for( ai = mc->multAlloc->head; ai != NULL; ai = ai->next )
    {   LongAd *lad = &ai->aad.longAd;
        Uint32  nBlocks =
            ROUNDUPELEMENTS(adGetExtentSize(lad), vmi->blockSize);
        if( pNmbOfBlocks != NULL )
        {   *pNmbOfBlocks += nBlocks;
        }
        if( !markPartitionAllocation( mc,
                lad->extentLocation.partitionReferenceNumber,
                lad->extentLocation.logicalBlockNumber,
                nBlocks, NULL, FALSE ) )        /* mark */
        {   return FALSE;               /* fatal error */
        }
    }
    /* no overheadList
     */
    return TRUE;

}   /* end preloadBitmapsWithMultAllocList() */


/* mark allocation of a Node and all its associate nodes.
 * follow the child list for stream directories only.
 *
 * NOTE: see buildPartitionBitmaps() for two pass
 *       approach explanation.
 */
static bool markNodePartitionAllocation( Node *node,
                                         UdfMountContext *mc,
                                         int pass )
{
    /* no action for NULL node or parent FID node
     */
    if(   node == NULL
       || (   node->fid != NULL
           && isBitOn(node->fid->fileCharacteristics, FCB_PARENT)) )
    {   return TRUE;
    }

    /* allocate blocks of all Main Stream and associated nodes,
     * Main Stream FE and ADs, EA file, Stream Directory
     * and streams.
     */
    if(   !markLongAdPartitionAllocation(nodeGetIcb(node, mc),
                                         "FE", mc, node, pass)
       || !markAlPartitionAllocation(node->al, node->fePartRef,
                                     node, mc, pass)
       || !markNodePartitionAllocation(node->eaFileNode, mc, pass)
       || !markNodePartitionAllocation(node->streamDirNode, mc, pass) )
    { return FALSE;     /* fatal error */
    }

    /* follow child list for stream directory only
     */
    if(   node == mc->systemStreamDirNode
       || (   node->parent != NULL
           && node == node->parent->streamDirNode) )
    {   Node *child;
        for( child = node->firstChild;
             child != NULL;
             child = child->nextInDirectory )
        {   if( !markNodePartitionAllocation(child, mc, pass) )
                return FALSE;       /* fatal error */
        }
    }
    return TRUE;

}   /* end markNodePartitionAllocation() */

/* markMetadataSparseExtents() marks sparse extents in Metadata File
 * as allocated for Metadata Partition Space Set in order to be
 * compared to Metadata Bitmap File later.
 * precondition: metaPmi->pMapType == PMAPTYPE_METADATA
 *
 * return value: TRUE if ok,
 *         else: FALSE
 *
 * NOTE: see buildPartitionBitmaps() for two pass
 *       approach explanation.
 */
static bool markMetadataSparseExtents(UdfMountContext  *mc,
                                      PartitionMapInfo *metaPmi,
                                      Uint16            metadataPref,
                                      int               pass)
{
    const MediumInfo    *vmi = getTheMediumInfo();
    MetadataRecord      *mRec = &metaPmi->metadataRec;
    UdfAllocationList   *metadataAl = mRec->pMetadataFileAllocList;
    UdfAllocationItem   *item;
    Uint32               totalBlocks, nbl, sz;

    UCTASSERT( metaPmi->pMapType == PMAPTYPE_METADATA );

    totalBlocks = 0;
    for( item  = metadataAl->head;
         item != NULL;
         item  = item->next, totalBlocks += nbl )
    {
      /* Sparse extent (type ADEL_NOT_RECORDED_NOT_ALLOCATED)
       * of Metadata File in the counterpart partition must
       * be marked as an ADEL_RECORDED_AND_ALLOCATED extent
       * in Metadata Partition Space Set.
       */
      sz = adGetExtentSize(&item->aad.anyAd);
      nbl = ROUNDUPELEMENTS( sz, vmi->blockSize );
      if(       adGetExtentType(&item->aad.anyAd)
             == ADEL_NOT_RECORDED_NOT_ALLOCATED     /* sparse */
         && !markAndVerifyPartitionAllocation(
                            metadataPref, totalBlocks,
                makeExtentTypeSize(ADEL_RECORDED_AND_ALLOCATED,sz),
                            "Metadata sparse", mc, NULL, pass) )
      { return FALSE;           /* fatal error */
      }
    }
    return TRUE;    /* no fatal error */

}   /* end markMetadataSparseExtents() */

/* npBuildPartitionAllocationGroup(),
 * Handle group of node pointers, as
 * called by npBuildPartitionBitmaps().
 * A group can be:
 * - empty (len == 0)
 * - contain all nodes with an undefined ICB or
 *   a translation error: (translateOk == FALSE)
 * - contain all nodes that refer to the
 *   same physical address in <physAddr>.
 *   <cntFids> nodes of this group have a FID.
 *
 * The nodes have been sorted according to
 * qsortNodesFileLinkCount().
 *
 * Because hard links are made between Main Stream Nodes,
 * all nodes associated with a Main Stream Node are
 * approached via their Main Stream Node only, in order
 * to allocate the blocks of hard linked nodes only once.
 * Main Stream Nodes are:
 *  - all nodes having a FID, except parent FID
 *    nodes and Stream Nodes.
 *  - the root directory and System Stream Directory nodes
 *  - the VAT node.
 * Associate nodes are:
 *  - EA Nodes
 *  - Stream Directory Nodes and their Stream Nodes
 *    attatched to a Main Stream Node.
 *
 * NOTE: see buildPartitionBitmaps() for two pass
 *       approach explanation.
 */
static bool npBuildPartitionAllocationGroup(
                    NodePointer     *npGroup,
                    Uint32           len,
                    bool             translateOk,
                    UdfMountContext *mc,
                    int              pass)
{   Uint32 n;

#ifdef UCT_TESTING_NP
    VERBOSE00(uctout, "==> npBuildPartitionAllocationGroup: NPDEBUG 01: group\n");
    npPrintPath((NodePointer*)npGroup, len,
            NPP_FULL, " ", "\n", TRUE, TRUE, mc); /* extra info */
    fflush(uctout);
#endif  /** UCT_TESTING_NP **/

    /* exit for empty or error group
     */
    if( len == 0 || !translateOk)
    {   return TRUE;                /* not fatal */
    }
    /* find first Main Stream Node of this group
     * and mark allocation for this node only.
     * skip parent FID node and Stream node.
     */
    for( n = 0; n < len; n++ )
    { Node *node = npGroup[n];
      if(   node->fid != NULL
         && (   isBitOn(node->fid->fileCharacteristics, FCB_PARENT)
             || node->parent == mc->systemStreamDirNode
             || (   node->parent != NULL
                 && node->parent->parent != NULL
                 && node->parent ==
                    node->parent->parent->streamDirNode)) )
      { continue;   /* skip parent FID or Stream node */
      }

      /* find first Main Stream Node of this group,
       * mark all its associated allocations and
       * ignore rest of this group.
       */
      if(   node->fid != NULL
         || node == mc->rootNode
         || node == mc->systemStreamDirNode
         || node == mc->vatNode
         || (   mc->metadataFile != NULL
             && node == mc->metadataFile->node)
         || (   mc->metadataMirrorFile != NULL
             && node == mc->metadataMirrorFile->node)
         || (   mc->metadataBitmapFile != NULL
             && node == mc->metadataBitmapFile->node) )
      { return markNodePartitionAllocation(node, mc, pass);
      }
    }
    return TRUE;    /* ok, none found */

}   /* end npBuildPartitionAllocationGroup() */

/* npBuildPartitionBitmaps():
 * Create Space Bitmap of all extents allocated by the
 * Node hierarchy using the node pointer array as sorted
 * by npCheckFileLinkCounts().
 * The main loop of npBuildPartitionBitmaps() is
 * derived from the one of npCheckFileLinkCounts().
 *
 * A goal here is also to locate all multiple allocated
 * extents in a partition. This is done by calling
 * markPartitionAllocation() for all allocated extents
 * of a node, but only once for hard linked nodes.
 * If in this way, the value *pNrOfBlocksMarkedAlready
 * as returned by markPartitionAllocation() shall always
 * be zero, otherwise a multiple allocation is found.
 *
 * markPartitionAllocation() will create a Space Bitmap
 * for each partition when it is called for the first
 * time for that partition.
 * markPartitionAllocation() shall not be called before
 * when entering npBuildPartitionBitmaps() for
 * the first time !!
 *
 * return value: FALSE in case of FATAL error, else TRUE.
 *
 * NOTE: see buildPartitionBitmaps() for two pass
 *       approach explanation.
 */
static bool npBuildPartitionBitmaps( NodePointerArray *npa,
                                     UdfMountContext  *mc,
                                     int               pass )
{
    PartitionHeaderDescriptor *phd;
    PartitionMapInfo *pmi;
    ContExtentItem   *pContItem;
    Node   *np;
    LongAd *icb;
    Uint32  n, cntGroup, cntFids;
    Uint32  physAddr = 0, physPrev, dummy;
    Uint16  pRef, numberOfPartitionMaps;
    bool    result = FALSE, resultPrev;

#ifdef UCT_TESTING_NP       /* show all nodes, unsorted */
    VERBOSE00(uctout, "==> npBuildPartitionBitmaps: NPDEBUG 01:\n");
    npPrintPath((NodePointer*)npa->arr, npa->len,
                NPP_FULL, NULL, "\n", TRUE, TRUE, mc); /* extra info */
    fflush(uctout);
#endif  /** UCT_TESTING_NP **/

    /* Metadata sparse extents special case first
     */
    numberOfPartitionMaps = (Uint16) mc->vi->lvd->numberOfPartitionMaps;

    for( pRef = 0; pRef < numberOfPartitionMaps; pRef++ )
    {   pmi = &mc->partitionMapInfo[pRef];
        if(    pmi->pMapType == PMAPTYPE_METADATA
           && !markMetadataSparseExtents(mc, pmi, pRef, pass) )
        { return FALSE;     /* fatal error */
        }
    }

    /* LongAd FSD sequence extent and continuation extents.
     */
    if( !markLongAdPartitionAllocation(
            &mc->vi->lvd->logicalVolumeContentsUse.fileSetDescriptorSequenceExtent,
                        "FSD Sequence", mc, NULL, pass) )
    {   return FALSE;           /* fatal error */
    }
    /* FSD Continuation extents.
     */
    for( pContItem = mc->contExtentList;
         pContItem != NULL;
         pContItem = pContItem->next )
    {   if( pContItem->sequenceId == CEI_ID_FSD )
        {   LongAd *pLad = &pContItem->aad.longAd;
            UCTASSERT( pContItem->adType == ADT_LONG );

#ifdef  UCT_TESTING_CONTINUATION
/**/fprintf(uctout, "DEBUG: %s continuation extent: %lu,p%u length: %lu\n",
/**/                tidTEXT4(pContItem->tagId),
/**/                pLad->extentLocation.logicalBlockNumber,
/**/                pLad->extentLocation.partitionReferenceNumber,
/**/                adGetExtentSize(pLad));
/**/fflush(uctout);
#endif  /** UCT_TESTING_CONTINUATION **/

            if( !markLongAdPartitionAllocation( pLad,
                            "FSD Sequence Continuation", mc, NULL, pass) )
            {   return FALSE;           /* fatal error */
            }
        }
    }

    /* Partition Space Sets (ShortAd).
     * skip virtual partition and metadata partition.
     * Metadata Files are located in the physical/sparable partition.
     */
    for( pRef = 0; pRef < numberOfPartitionMaps; pRef++ )
    {
        pmi = &mc->partitionMapInfo[pRef];
        if(   pmi->pMapType == PMAPTYPE_VIRTUAL
           || pmi->pMapType == PMAPTYPE_METADATA )
        {   continue;   /* no Space Set extents in virt/meta partition */
        }
        phd = &pmi->pdPointer->partitionContentsUse.partitionHeaderDescriptor;

        if(   !markShortAdPartitionAllocation(
                        &phd->unallocatedSpaceTable, pRef,
                            "Unallocated Space Table", mc, NULL, pass)
           || !markShortAdPartitionAllocation(
                        &phd->unallocatedSpaceBitmap, pRef,
                            "Unallocated Space Bitmap", mc, NULL, pass)
           || !markShortAdPartitionAllocation(
                        &phd->freedSpaceTable, pRef,
                            "Freed Space Table", mc, NULL, pass)
           || !markShortAdPartitionAllocation(
                        &phd->freedSpaceBitmap, pRef,
                            "Freed Space Bitmap", mc, NULL, pass) )
        { return FALSE;         /* fatal error */
        }
        /* Partition Space Set overhead
         */
        if( !markAlPartitionAllocation(pmi->shortOverheadList,
                                       pRef, NULL, mc, pass) )
        {   return FALSE;       /* fatal error */
        }
    }           /* endfor */

    /* Handle now NodePointerArray npa.
     * Make groups with equal physical address
     * The entries of a group are hard linked together,
     * so mark only the allocations for one of them as allocated
     * in the Partition Bitmaps.
     * The first group may be a group of undefined
     * icb and translate error nodes.
     * npHandleFileLinkCountGroup() must be capable
     * of handling empty groups (cntGroup == 0).
     */
    cntGroup = cntFids = 0;         /* empty group */
    for( n = 0; n < npa->len; n++ )
    {   bool newGroup;
        np  = npa->arr[n];
        icb = nodeGetIcb(np, mc),
        physPrev = physAddr;
        resultPrev = result;
        result = FALSE;
        if( icb != NULL )
        {   result = translateAddress(mc,
                 icb->extentLocation.partitionReferenceNumber,
                 icb->extentLocation.logicalBlockNumber,
                &physAddr, &dummy, TRUE);   /* silent */
        }
        newGroup = ( n != 0 && (  (result != FALSE && physAddr != physPrev)
                                || result != resultPrev ) );

        if( newGroup )  /* handle previous group if any */
        {   npBuildPartitionAllocationGroup(
                        (NodePointer*) &npa->arr[n-cntGroup],
                        cntGroup, resultPrev, mc, pass);
            cntGroup = cntFids = 0;
        }
        cntGroup++;
        if( np->fid != NULL )
        {   cntFids++;
        }
    }
    /* handle final group, if any
     */
    npBuildPartitionAllocationGroup(
            (NodePointer*) &npa->arr[n-cntGroup],
             cntGroup, result, mc, pass );

    return TRUE;    /* ok */

}   /* end npBuildPartitionBitmaps() */


/* countNonAllocOverlapBlocks():
 * Count number of overlapping blocks of logical extent
 * with Non-Allocatable Space Allocation Descriptors.
 * precondition:
 *      The extent shall be a logical non-virtual extent.
 *
 * Note: In case of overlapping extents in the Non-Alloc
 *  ADs itself, the result count could become larger than
 *  'extentBlocks'. In that case the result will be decremented
 *  to 'extentBlocks'.
 */
static bool countNonAllocOverlapBlocks(
                        UdfMountContext *mc,
                        Uint32  extentStart,
                        Uint32  extentBlocks,
                        Uint16  extentPartRef,
                        Uint32 *overlapBlocks )
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32 oTotal;      /* total overlap */

    *overlapBlocks = oTotal = 0;
    if(   mc->nonAllocatableSpaceNode     != NULL   /* node present */
       && mc->nonAllocatableSpaceNode->al != NULL ) /* list present */
    {   UdfAllocationItem *ai;
        UdfAllocationList *al = mc->nonAllocatableSpaceNode->al;
        Uint32  logicalBlockNr;
        Uint16  partRef;

        /* There is a Non-Allocatable Space List/Stream.
         * Important precondition:
         *      The extent shall be a logical non-virtual extent.
         * Do not bother here about strange results in case of
         * overlapping extents in the Non-Alloc ADs,
         * so just count the overlapping blocks.
         * Only check with allocated extents.
         */
        for( ai = al->head; ai != NULL; ai = ai->next )
        {   Uint32 oStart, oBlocks;
            if( adGetExtentType(&ai->aad.anyAd) == ADEL_NOT_RECORDED_NOT_ALLOCATED )
            {   continue;       /* skip unallocated extent */
            }
            if( !udfGetLocation(&ai->aad, al->itemAdType,
                                 mc->nonAllocatableSpaceNode->fePartRef,
                                &partRef, &logicalBlockNr) )
            {   return FALSE;   /* fatal error */
            }
            if(   partRef == extentPartRef      /* same logical partition */
               && calculateOverlap( logicalBlockNr,
                            ROUNDUPELEMENTS(adGetExtentSize(&ai->aad.anyAd),
                                            vmi->blockSize),
                                    extentStart, extentBlocks,
                                   &oStart, &oBlocks ) )
            {   oTotal += oBlocks;  /* overlap */
            }
        }
    }

    /* avoid strange effects in case of overlapping extents
     * in the Non-Alloc file itself
     */
    *overlapBlocks = MIN(oTotal, extentBlocks);

    return TRUE;

}   /* end countNonAllocOverlapBlocks() */

/* handleNonAllocOverlap():
 * Check if a logical extent overlaps with Non-Allocatable Space.
 * Check if (part of) extent must be marked as allocated.
 * Actions depending on:
 *
 * bool allowMarkAlloc:
 *  if TRUE : Maybe mark (part of) extent as allocated.
 *  if FALSE: No mark of (part of) extent as allocated whatsoever.
 *
 * bool expectedInNonAlloc:
 *  TRUE : Flag extent blocks that do NOT overlap with
 *         Non-Allocatable Space as an error and if
 *         allowMarkAlloc == TRUE, then allocate those blocks
 *         using markAndVerifyPartitionAllocation().
 *  FALSE: Flag extent blocks overlapping with
 *         Non-Allocatable Space as an error.
 *         If allowMarkAlloc == TRUE, then Allocate whole extent
 *         using markAndVerifyPartitionAllocation().
 *
 * Important precondition:
 *      The extent shall be a logical non-virtual extent.
 *
 * Do not bother about strange results in case of overlapping
 * partitions or overlapping extents in the Non-Alloc file,
 * so just count the overlapping blocks and compare to the
 * number of blocks in the extent.
 *
 * Multiple allocation messages handled by
 * markAndVerifyPartitionAllocation().
 * Other local messages in pass 1 only !!!
 *
 * NOTE: see buildPartitionBitmaps() for two pass
 *       approach explanation.
 */
static bool handleNonAllocOverlap( UdfMountContext *mc,
                                   Uint32 extentStart,
                                   Uint32 extentBlocks,
                                   Uint16 extentPartRef,
                                   bool   expectedInNonAlloc,
                                   bool   allowMarkAlloc,
                                   char  *idText,
                                   char  *udfEcmaRef,
                                   int    pass )
{
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32 oBlocksTotal;

    /* Unless the extent has something to do with sparing,
     * use UDF reference: NONALLOC_UDFREF_TXT.
     * rules in sparing section 2.2.12 (was 2.2.11) are better !!
     */
    if(   udfEcmaRef != NULL
       && strcmp(udfEcmaRef, UDFREF_SPTAB(getUctUdfRevision())) != 0)
    {   udfEcmaRef = NONALLOC_UDFREF_TXT;
    }

    /* count blocks overlapping with Non-Allocatable Space
     */
    if( !countNonAllocOverlapBlocks( mc, extentStart, extentBlocks,
                                     extentPartRef, &oBlocksTotal ) )
    {   return FALSE;   /* fatal error */
    }

    if( pass == 1 )     /* local messages */
    {
        if( expectedInNonAlloc && oBlocksTotal != extentBlocks )
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            fprintf(uctout,
              "\t  Error: %s overlap with Non-Allocatable Space for\n"
                   "-\t\t %s, %s.\n"
             "-\t extent: %lu block%s, logical address: (%lu,%lu).\n"
                   "-\t\t %lu overlapping block%s.\n",
                (oBlocksTotal == 0) ? "No" : "Incomplete",
                idText, udfEcmaRef, extentBlocks, PLURAL_S(extentBlocks),
                extentStart, extentPartRef,
                oBlocksTotal, PLURAL_S(oBlocksTotal));
            if( oBlocksTotal == 0 )
            { if(   mc->nonAllocatableSpaceNode     == NULL     /* node not present */
                 || mc->nonAllocatableSpaceNode->al == NULL )   /* list not present */
              { fprintf(uctout,
                   "-\t   Note: Missing or erroneous Non-Allocatable Space.\n");
              }
            }
            else            /* partial overlap */
            {   fprintf(uctout,
                   "-\t   Note: Partial overlap may cause incorrect"
                                    " multiple allocation errors.\n");
            }
          MLIMITend;
        }
        if( (!expectedInNonAlloc) && oBlocksTotal != 0 )
        { MLIMITbegin(ERROR00level, uctMessageLimit);
            fprintf(uctout,
              "\t  Error: Unexpected overlap with Non-Allocatable Space for %s,\n"
                   "-\t\t %s.\n"
             "-\t extent: %lu block%s, logical address: (%lu,%lu).\n"
                   "-\t\t %lu overlapping block%s.\n", idText, udfEcmaRef,
                    extentBlocks, PLURAL_S(extentBlocks),
                    extentStart, extentPartRef,
                    oBlocksTotal, PLURAL_S(oBlocksTotal));
          MLIMITend;
        }
    }

    /* mark whole extent if allowMarkAlloc
     * AND( (NOT expectedInNonAlloc) OR (overlap is not complete) )
     * Partly overlap may cause extra multiple allocation errors,
     * but so be'it.
     */
    if(   allowMarkAlloc
       && ( !expectedInNonAlloc || oBlocksTotal != extentBlocks )
       && !markAndVerifyPartitionAllocation(extentPartRef, extentStart,
                                            extentBlocks * vmi->blockSize,
                                            idText, mc, NULL, pass) )
    {   return FALSE;   /* fatal error */
    }
    return TRUE;

}   /* end handleNonAllocOverlap() */

/* checkExtentPartitionOverlapMessage():
 * print partition overlap message for
 * checkExtentPartitionOverlap(), ECMA 3/8.5, UDF 2.2.12 (was2.2.11).
 * No test on vLevel because this function shall be
 * called from within a MLIMITbegin/MLIMITend clause.
 */
static void checkExtentPartitionOverlapMessage(
                    Uint8  vLevel,
                    char  *idText,
                    char  *udfEcmaText,
                    Uint16 pRef,
                    Uint32 accessType,
                    Uint32 oStart,
                    Uint32 oBlocks )
{
    /* vLevel determines error/warning or note
     */
    fprintf(uctout,
      "\n\t%8s %s overlap with %s partition p%lu,\n"
        "-\t\t location %lu",
          (vLevel == ERROR00level) ?   "Error:"
        : (vLevel ==  WARN01level) ? "Warning:"
                                   :    "Note:",
            idText, PDAT_TEXT(accessType),
            pRef, oStart);
    if( oBlocks > 1 )
    {   fprintf(uctout, " thru %lu", oStart + oBlocks - 1);
    }
    if( udfEcmaText != NULL )
    { fprintf(uctout, ", %s", udfEcmaText);
    }
    fprintf(uctout, ".\n");

}   /* end checkExtentPartitionOverlapMessage() */

/* checkExtentPartitionOverlap():
 * Check if an extent overlaps with partition space.
 *
 * Overlap messages:
 *  if maybeEcma385Error == TRUE and overlap with rewritable
 *     or overwritable partition,
 *  then: error message, see ECMA 3rd edition 3/8.5.
 *  else: flag overlap by a Note.
 *
 * Implementation note:
 *  Strictly for UDF 1.50 and lower, there should also be an
 *  error for other partition access types, see ECMA 2nd edition.
 *  However, this was a flaw in ECMA and it is unavoidable for a
 *  finalized sequential file system, because e.g. the 2nd AVDP
 *  must be at N-256, and the VAT FE (inside the partition) at N.
 *
 * Local messages are printed in pass 1 only !!!
 *
 * Check if overlapping with Non-Allocatable Space by
 * calling handleNonAllocOverlap() that will take the
 * appropriate actions according to 'bool expectedInNonAlloc',
 * see handleNonAllocOverlap() explanation.
 *
 * return value: TRUE if overlap found, else FALSE;
 *
 * NOTE: see buildPartitionBitmaps() for two pass
 *       approach explanation.
 */
static bool checkExtentPartitionOverlap( UdfMountContext *mc,
                                         Uint32 extentStart,
                                         Uint32 extentBlockLen,
                                         char  *idText,
                                         char  *udfEcmaText,
                                         bool   maybeEcma385Error,
                                         bool   expectedInNonAlloc,
                                         int    pass )
{
    PartitionMapInfo *pmi;
    Uint16  pRef, numberOfPartitionMaps;
    Uint32  accessType, oStart, oBlocks;
    bool    overlapFound = FALSE;

    UCTASSERT(   mc != NULL && mc->vi != NULL && mc->vi->lvd != NULL
              && mc->partitionMapInfo != NULL );

    numberOfPartitionMaps = (Uint16) mc->vi->lvd->numberOfPartitionMaps;

    /* always handle all partitions, because there may
     * be overlap with more than one partition.
     */
    for( pRef = 0; pRef < numberOfPartitionMaps; pRef++ )
    {
        pmi = &mc->partitionMapInfo[pRef];
        if(   pmi->pMapType == PMAPTYPE_VIRTUAL
           || pmi->pMapType == PMAPTYPE_METADATA )
        {   continue;       /* skip virtual and metadata partition */
        }
        if( calculateOverlap( extentStart, extentBlockLen,
                              pmi->pdPointer->partitionStartingLocation,
                              pmi->pdPointer->partitionLength,
                              &oStart, &oBlocks ) )
        {   Uint32 logicalStart =
                        oStart - pmi->pdPointer->partitionStartingLocation;
            overlapFound = TRUE;

            /* Local messages in pass 1 only.
             * Handle 'overlap' message.
             */
            if( pass == 1 )
            {   /* Whether handled/counted as error or as
                 * a note depends on partition access type,
                 * see also "Implementation note:" above.
                 */
                accessType = pmi->pdPointer->accessType;
                if(   maybeEcma385Error
                   && (   accessType == PDAT_REWRITABLE
                       || accessType == PDAT_OVERWRITABLE ) )
                { MLIMITbegin(ERROR00level, uctMessageLimit);
                    checkExtentPartitionOverlapMessage(     /* Error: */
                        MLIMIT.vl, idText, udfEcmaText,
                        pRef, accessType, oStart, oBlocks);
                  MLIMITend;
                }
                else
                { MLIMITbegin(INFO01level, uctMessageLimit);
                    checkExtentPartitionOverlapMessage(     /*  Note: */
                        MLIMIT.vl, idText, udfEcmaText,
                        pRef, accessType, oStart, oBlocks);
                  MLIMITend;
                }
            }

            /* overlap, pass 1 or 2.
             * check if partition overlap also overlaps with Non-Allocatable
             * Space according to expectedInNonAlloc. Maybe print messages
             * and mark (part of) overlapping blocks as allocated.
             */
            if( !handleNonAllocOverlap( mc, logicalStart, oBlocks, pRef,
                                        expectedInNonAlloc,
                                        TRUE,           /* allowMarkAlloc */
                                        idText, udfEcmaText, pass ) )
            {   /* Fatal error.
                 * Partition overlap found, so return TRUE,
                 * but do not check any more partitions
                 */
                return TRUE;    /* fatal error, but partition overlap found */
            }
        }
    }       /* endfor( pRef ... ) */

    return overlapFound;

}   /* end checkExtentPartitionOverlap() */


/* checkOriginalLocationNonAlloc():
 * Sparing Original Location (defect packet) shall
 * NOT be in Non-Allocatable Space, error otherwise.
 * Do not mark allocated, so call for pass 1 only !!
 *
 * NOTE: see buildPartitionBitmaps() for two pass
 *       approach explanation.
 */
static bool checkOriginalLocationNonAlloc( Uint32 originalLocation,
                                           Uint32 spPacketLen,
                                           Uint16 spPartRef,
                                           UdfMountContext *mc )
{
    Uint32 oBlocksTotal = 0;    /* for '> 0xFFFFFFF0' locations */

    /* Count blocks overlapping with Non-Allocatable Space
     */
    if(    originalLocation < 0xFFFFFFF0    /* entry in use */
       && !countNonAllocOverlapBlocks( mc,
                        ROUNDDOWNMULT(originalLocation, spPacketLen),
                                       spPacketLen, spPartRef,
                                      &oBlocksTotal ) )
    {   return FALSE;   /* fatal error */
    }
    if( oBlocksTotal != 0 )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
         "\n\t  Error: Sparing Original Location in Non-Allocatable Space.\n"
          "-\t extent: %2lu block%s, logical address: (%lu,%lu).\n"
                "-\t\t %2lu overlapping block%s, %s.\n"
          "-\tNote: Defective Sparing Original Location packets shall not be\n"
          "-\t      allocated by the Non-Allocatable Space file or stream.\n",
                spPacketLen, PLURAL_S(spPacketLen),
                originalLocation, spPartRef,
                oBlocksTotal, PLURAL_S(oBlocksTotal),
                UDFREF_SPTAB(getUctUdfRevision()) );

        if( MLIMIT.cnt == 1 )   /* print for first occurrence only */
        { fprintf(uctout,
            "-\t%s is correct in this respect, but UDF 1.50 2.3.13\n"
            "-\tand UDF 2.00+2.01, section 3.3.7.2 are unclear or\n"
            "-\tincorrect on this point. To be corrected in UDF 2.50.\n"
            "-\tHowever, the error above has to be taken seriously\n"
            "-\tbecause the consequence may be allocation inconsistency\n"
            "-\tor the whole Sparing implementation never being used.\n"
            "-\t(extensive explanation only printed at first occurrence).\n",
                            UDFREF_SPTAB(getUctUdfRevision()) );
        }

      MLIMITend;
    }
    return TRUE;

}   /* end checkOriginalLocationNonAlloc() */

/* checkSparingTablesPartitionOverlap():
 * Check partition overlap of Sparing Tables
 * and sparing mapping area.
 * Local messages and call checkOriginalLocationNonAlloc()
 * in pass 1 only.
 *
 * return value: TRUE if partition overlap found, else FALSE.
 *
 * NOTE: see buildPartitionBitmaps() for two pass
 *       approach explanation.
 */
static bool checkSparingTablesPartitionOverlap(
                            UdfMountContext *mc,
                            int pass )
{
    const MediumInfo *vmi = getTheMediumInfo();
    PartitionMapInfo *pmi;
    SparablePartitionMapTail *sTail;
    SparingEntry *se;
    Uint32  *pStartLoc;
    Uint32   n, nmbBlocks, nmbPack, spPacketLen, mLoc;
    Uint16   pRef, numberOfPartitionMaps, nmbEnt;
    Uint16   UDFrev = getUctUdfRevision();
    bool     partitionOverlap = FALSE;

    UCTASSERT( mc != NULL && mc->vi != NULL && mc->vi->lvd != NULL );

    numberOfPartitionMaps = (Uint16) mc->vi->lvd->numberOfPartitionMaps;

    /* check sparing table locations and sparing area.
     */
    for( pRef = 0; pRef < numberOfPartitionMaps; pRef++ )
    {
        pmi = &mc->partitionMapInfo[pRef];
        if( pmi == NULL || pmi->pMapType != PMAPTYPE_SPARABLE )
        {   continue;   /* no sparable partition, skip */
        }
        /* sparable partition, first check sparing table locations.
         */
        sTail = &pmi->pPartitionMap->type2PartitionMap.SharedTail.sparableTail;
        nmbBlocks = ROUNDUPELEMENTS(sTail->sizeOfEachSparingTable, vmi->blockSize);
        for( n = 1, pStartLoc = (Uint32 *) &sTail->startOfLocationsOfSparingTables;
             n <= sTail->numberOfSparingTables;
             n++,   pStartLoc++ )
        {   char txt[30];           /* should be enough */
            sprintf( txt, "Sparing Table %lu", n);

            /* Sparing Table n: Shall not be in Non-Allocatable Space.
             * bool flags:
             *  No partition overlap error, (so overlap message printed as Note:)
             *  expectedInNonAlloc          (overlapping part with partition)
             * See errata DCN-5107 for UDF revisions 1.50 thru 2.50
             */
            if( checkExtentPartitionOverlap( mc, (*pStartLoc),
                                    nmbBlocks, txt,
                                    UDFREF_SPTAB(UDFrev),
                                    FALSE,          /* overlap allowed */
                                    TRUE, pass ) )  /* in Non-alloc */
            { partitionOverlap = TRUE;
            }
        }

        spPacketLen = sTail->packetLength;
        if( pmi->pSparingTable == NULL || spPacketLen == 0 )
        {   continue;       /* no sparing table */
        }
        /* sparing mapping area, maybe contigious, check sparing table entries.
         * Note that for each entry mappedLocation must be a valid location
         * independent of the value for originalLocation.
         * If multiple tables, only check the one installed on pmi->pSparingTable.
         */
        nmbEnt = pmi->pSparingTable->reallocationTableLength;
        se = (SparingEntry *) &pmi->pSparingTable->startOfMapEntries;
/**testing**
/**testing** (se+ 5)->mappedLocation = 5000;    /** 4992 **/
/**testing** (se+ 6)->mappedLocation = 5032;    /** thru **/
/**testing** (se+ 7)->mappedLocation = 5064;    /** 5087, 96 blocks **/
/**testing** (se+17)->mappedLocation = 6000;    /** 5984 thru 6015, 32 blocks **/
/**testing**/

        for( n = 0; n < nmbEnt; n++, se++ )
        {   nmbPack = 1;        /* find contiguous mapped entries */
            mLoc = ROUNDDOWNMULT(se->mappedLocation, spPacketLen);

            /* Sparing Original Location (defect packet) shall NOT be in
             * Non-Allocatable Space, check. Do not mark allocated !!
             */
            if( pass == 1 )
            { checkOriginalLocationNonAlloc(
                            se->originalLocation, spPacketLen, pRef, mc );
            }
            /* Do Original Location overlap check only as long as next
             * Mapped Location is contiguous with this one. After that
             * do Mapped Location check over whole contiguous range.
             */
            while(   (n + 1) < nmbEnt
                  && (mLoc + spPacketLen)
                     == ROUNDDOWNMULT((se+1)->mappedLocation, spPacketLen) )
            {   n++, se++, nmbPack++, mLoc += spPacketLen;
                /* Sparing Original Location, see above
                 */
                if( pass == 1 )
                { checkOriginalLocationNonAlloc( se->originalLocation,
                                                 spPacketLen, pRef, mc );
                }
            }
            /* Sparing Mapping Area packets, shall be in Non-Allocatable Space.
             * bool flags:
             *  No partition overlap error, expectedInNonAlloc.
             */
            if( checkExtentPartitionOverlap( mc,
                    ROUNDDOWNMULT((se+1-nmbPack)->mappedLocation, spPacketLen),
                                nmbPack * spPacketLen,
                                "Sparing Mapping Area",
                                UDFREF_SPTAB(UDFrev),
                                FALSE, TRUE, pass ) )
            {   partitionOverlap = TRUE;
            }
        }
    }       /* endfor( pRef ... ) */

    return partitionOverlap;

}   /* end checkSparingTablesPartitionOverlap() */


/* checkExtentUsdOverlap():
 * Check overlap of an extent with extents of the
 * USD ExtentAd list. No overlap shall exist.
 *
 * precondition: mc->vi->usd != NULL
 * return value: TRUE if overlap, else FALSE.
 *
 * NOTE: similar to checkExtentPartitionOverlap()
 */
static bool checkExtentUsdOverlap( UdfMountContext *mc,
                                   Uint32 extentStart,
                                   Uint32 extentNmbOfSectors,
                                   char  *idText )
{
    const MediumInfo *vmi = getTheMediumInfo();
    UnallocatedSpaceDescriptor *usd = mc->vi->usd;
    ExtentAd *ead;
    Uint32    adNr, oStart, oBlocks;
    bool      overlapFound = FALSE;

    for( adNr = 1, ead = (ExtentAd *) &usd->startOfAllocationDescriptors;
         adNr <= usd->numberOfAllocationDescriptors;
         adNr++,   ead++ )
    {   Uint32 nrBlocks = ROUNDUPELEMENTS(ead->extentLength, vmi->blockSize);
        if( calculateOverlap( extentStart, extentNmbOfSectors,
                              ead->extentLocation, nrBlocks,
                              &oStart, &oBlocks ) )
        { overlapFound = TRUE;
          MLIMITbegin( ERROR00level, MLIMITdefault10 );
            printMessageHead(MLIMIT.vl, (Byte*)usd, (Byte*)ead, NULL);
            fprintf(uctout,
                "Error: USD AD overlap with %s at sector %lu,\n"
                "-\t\t\t%lu sector%s, UDF 2., 2.2.5, ECMA 3/10.8.4, 3/8.5.\n",
                idText, oStart, oBlocks, PLURAL_S(oBlocks));
          MLIMITend;
        }
    }       /* endfor */

    return overlapFound;    /* TRUE if overlap found */

}   /* end checkExtentUsdOverlap() */

/* checkSparingTablesUsdOverlap():
 * Check USD overlap of Sparing Tables
 * and sparing mapping area.
 * return value: TRUE if overlap found, else FALSE.
 */
static bool checkSparingTablesUsdOverlap(
                            UdfMountContext *mc )
{
    const MediumInfo *vmi = getTheMediumInfo();
    PartitionMapInfo *pmi;
    SparablePartitionMapTail *sTail;
    SparingEntry *se;
    Uint32  *pStartLoc;
    Uint32   n, nmbBlocks, nmbPack, spPacketLen, mLoc;
    Uint16   pRef, numberOfPartitionMaps, nmbEnt;
    bool     overlap = FALSE;

    UCTASSERT( mc != NULL && mc->vi != NULL && mc->vi->lvd != NULL );

    numberOfPartitionMaps = (Uint16) mc->vi->lvd->numberOfPartitionMaps;

    /* check sparing table locations and sparing area.
     */
    for( pRef = 0; pRef < numberOfPartitionMaps; pRef++ )
    {
        pmi = &mc->partitionMapInfo[pRef];
        if( pmi == NULL || pmi->pMapType != PMAPTYPE_SPARABLE )
        {   continue;   /* no sparable partition, skip */
        }
        /* sparable partition, first check sparing table locations.
         */
        sTail = &pmi->pPartitionMap->type2PartitionMap.SharedTail.sparableTail;
        nmbBlocks = ROUNDUPELEMENTS(sTail->sizeOfEachSparingTable, vmi->blockSize);
        for( n = 1, pStartLoc = (Uint32 *) &sTail->startOfLocationsOfSparingTables;
             n <= sTail->numberOfSparingTables;
             n++,   pStartLoc++ )
        {   char txt[30];           /* should be enough */
            sprintf( txt, "Sparing Table %lu", n);
            if( checkExtentUsdOverlap( mc, (*pStartLoc),
                                       nmbBlocks, txt) )
            {   overlap = TRUE;
            }
        }
        spPacketLen = sTail->packetLength;
        if( pmi->pSparingTable == NULL || spPacketLen == 0 )
        {   continue;       /* no sparing table */
        }
        /* sparing mapping area, maybe contigious, check sparing table entries.
         * Note that for each entry mappedLocation must be a valid location
         * independent of the value for originalLocation.
         * If multiple tables, only check the one installed one pmi->pSparingTable.
         */
        nmbEnt = pmi->pSparingTable->reallocationTableLength;
        se = (SparingEntry *) &pmi->pSparingTable->startOfMapEntries;
/**testing**
/**testing** (se+ 5)->mappedLocation = 5000;    /** 4992 **/
/**testing** (se+ 6)->mappedLocation = 5032;    /** thru **/
/**testing** (se+ 7)->mappedLocation = 5064;    /** 5087, 96 blocks **/
/**testing** (se+17)->mappedLocation = 6000;    /** 5984 thru 6015, 32 blocks **/
/**testing**/

        for( n = 0; n < nmbEnt; n++, se++ )
        {   nmbPack = 1;        /* find contiguous mapped entries */
            mLoc = ROUNDDOWNMULT(se->mappedLocation, spPacketLen);
            while(   (n + 1) < nmbEnt
                  && (mLoc + spPacketLen)
                     == ROUNDDOWNMULT((se+1)->mappedLocation, spPacketLen) )
            {   n++, se++, nmbPack++, mLoc += spPacketLen;
            }
            /* Sparing Mapping Area packets
             */
            if( checkExtentUsdOverlap( mc,
                    ROUNDDOWNMULT((se+1-nmbPack)->mappedLocation, spPacketLen),
                                nmbPack * spPacketLen,
                                "Sparing Mapping Area" ) )
            {   overlap = TRUE;
            }
        }
    }
    return overlap;

}   /* end checkSparingTablesUsdOverlap() */

/* checkUsdOverlap():
 * Check overlap of all Volume Structures and partitions with
 * extents of USD ExtentAd list. No overlap shall exist.
 *
 * return value: TRUE in case of a FATAL error, else FALSE.
 *
 * NOTE: Function similar to checkVolumeDescriptorsPartitionOverlap().
 */
static bool checkUsdOverlap( UdfMountContext *mc )
{
    ContExtentItem   *pContItem;
    const MediumInfo *vmi = getTheMediumInfo();
    Uint32  blockSize       = vmi->blockSize;
    Uint32  verifySession    =  vmi->verifySession;
    Uint32  verifySessionStart = (vmi->sessionStartBlocks)[verifySession-1];
    Uint32  n;
    bool    overlap = FALSE;

    if(   mc == NULL || mc->vi == NULL || mc->vi->usd == NULL
       || mc->vi->usd->numberOfAllocationDescriptors == 0 )
    { VERBOSE00(uctout,
        "\tNo free Volume Space defined in USD\n");
        return TRUE;        /* not fatal */
    }

    /* for all tests: no overlap allowed
     * 1st:  first 32K of Volume Space
     *       (actually first 32K of verify session)
     *       (not both, 1st session may be no data session)
     */
    if( checkExtentUsdOverlap( mc, verifySessionStart,
            ROUNDUPELEMENTS(MIN_BYTES_BEFORE_VRS, blockSize),
                (verifySessionStart == 0)
                    ? "first 32K of volume"
                    : "first 32K of session") )
    { overlap = TRUE;
    }

    /* 2nd:  AVDPs
     */
    for( n = 0; n < mc->nmbOfAvdps; n++ )
    { if( checkExtentUsdOverlap( mc, mc->avdpLocations[n], 1, "AVDP") )
      { overlap = TRUE;
      }
    }

    /* 3rd: Main and Reserve VDS sequences
     *      and VDS/VDP continuation extents.
     * first extent defined in used AVDP.
     */
    if(   mc->avdp != NULL      /* first Main extent */
       && checkExtentUsdOverlap( mc,
                        mc->avdp->mainVolumeDescriptorSequenceExtent.extentLocation,
        ROUNDUPELEMENTS(mc->avdp->mainVolumeDescriptorSequenceExtent.extentLength,
                        blockSize), "Main VDS") )
    { overlap = TRUE;
    }
    if(   mc->avdp != NULL      /* first Reserve extent */
       && checkExtentUsdOverlap( mc,
                        mc->avdp->reserveVolumeDescriptorSequenceExtent.extentLocation,
        ROUNDUPELEMENTS(mc->avdp->reserveVolumeDescriptorSequenceExtent.extentLength,
                        blockSize), "Reserve VDS") )
    { overlap = TRUE;
    }
    /* VDS/VDP Continuation extents, both Main and Reserve.
     */
    for( pContItem = mc->contExtentList;
         pContItem != NULL;
         pContItem = pContItem->next )
    {   if(   pContItem->sequenceId == CEI_ID_MAINVDS
           || pContItem->sequenceId == CEI_ID_RESVDS )
        {   ExtentAd *pEad = &pContItem->aad.extentAd;
            UCTASSERT( pContItem->adType == ADT_EXTENT );

#ifdef  UCT_TESTING_CONTINUATION
/**/fprintf(uctout, "DEBUG: %s continuation extent: %lu, length: %lu\n",
/**/                tidTEXT4(pContItem->tagId),
/**/            pEad->extentLocation, pEad->extentLength);
/**/fflush(uctout);
#endif  /** UCT_TESTING_CONTINUATION **/

            if( checkExtentUsdOverlap(mc, pEad->extentLocation,
                    ROUNDUPELEMENTS(pEad->extentLength, blockSize),
                                  "VDS Continuation") )
            { overlap = TRUE;
            }
        }
    }

    /* 4th: Volume Integrity Sequence extent (LVIDs)
     *      and continuation extents.
     * bool flags:
     *  No overlap error, NOT expectedInNonAlloc.
     */
    if(   mc->vi != NULL && mc->vi->lvd != NULL /* first extent */
       && checkExtentUsdOverlap( mc,
                        mc->vi->lvd->integritySequenceExtent.extentLocation,
        ROUNDUPELEMENTS(mc->vi->lvd->integritySequenceExtent.extentLength,
                        blockSize), "LVID Sequence" ) )
    { overlap = TRUE;
    }
    /* LVID Sequence Continuation extents.
     */
    for( pContItem = mc->contExtentList;
         pContItem != NULL;
         pContItem = pContItem->next )
    {   if( pContItem->sequenceId == CEI_ID_LVID )
        {   ExtentAd *pEad = &pContItem->aad.extentAd;
            UCTASSERT( pContItem->adType == ADT_EXTENT );

#ifdef  UCT_TESTING_CONTINUATION
/**/fprintf(uctout, "DEBUG: %s continuation extent: %lu, length: %lu\n",
/**/                tidTEXT4(pContItem->tagId),
/**/            pEad->extentLocation, pEad->extentLength);
/**/fflush(uctout);
#endif  /** UCT_TESTING_CONTINUATION **/

            if( checkExtentUsdOverlap(mc, pEad->extentLocation,
                    ROUNDUPELEMENTS(pEad->extentLength, blockSize),
                                  "LVID Sequence Continuation") )
            { overlap = TRUE;
            }
        }
    }

    /* 5th: Check Partition space USD overlap,
     *      avoid checking a PD twice.
     */
    if( mc->vi->lvd != NULL && mc->partitionMapInfo != NULL )
    { Uint16 pRef;
      Uint32 nMaps = mc->vi->lvd->numberOfPartitionMaps;
      for( pRef = 0; pRef < nMaps; pRef++ )
      { char txt[30];           /* 30 should do */
        PartitionDescriptor *pd;
        PartitionMapInfo *pmi = &mc->partitionMapInfo[pRef];
        if(   pmi->pMapType == PMAPTYPE_VIRTUAL
           || pmi->pMapType == PMAPTYPE_METADATA )
        {   continue;       /* skip secondary partitions */
        }
        pd = pmi->pdPointer;
        sprintf(txt, "partition p%u", pRef);
        if( checkExtentUsdOverlap(mc,
                            pd->partitionStartingLocation,
                            pd->partitionLength, txt) )
        { overlap = TRUE;
        }
      }
    }

    /* 6th: Check Sparing Tables and Sparing Area USD overlap.
     */
    if( checkSparingTablesUsdOverlap(mc) )
    { overlap = TRUE;
    }

    /* add note to USD overlap error message
     */
    if( overlap )
    { VERBOSE00(uctout,
        "-\tNote: For list of USD extents, see above where USD was read.\n");
    }
    return TRUE;    /* ok, no fatal error */

}   /* end checkUsdOverlap() */


/* checkVolumeDescriptorsPartitionOverlap():
 * check overlap of all Volume Structures with partition space.
 * In case of overlap mark partition allocation in order
 * to see if a possible Space set has marked these blocks.
 *
 * return value: TRUE in case of a FATAL error, else FALSE.
 *
 * NOTE: see buildPartitionBitmaps() for two pass
 *       approach explanation.
 */
static bool checkVolumeDescriptorsPartitionOverlap(
                                UdfMountContext *mc,
                                int pass )
{
    const MediumInfo *vmi = getTheMediumInfo();
    ContExtentItem   *pContItem;
    Uint32  n, blockSize  = vmi->blockSize;

    /* 1st:  AVDPs, maybe error in case of overlap
     *              depending on accessType.
     */
    for( n = 0; n < mc->nmbOfAvdps; n++ )
    {
        /* Shall not overlap with rewritable or overwritable partition.
         * bool flags:
         *  Maybe ECMA 3/8.5 overlap error, NOT expectedInNonAlloc.
         */
        checkExtentPartitionOverlap( mc, mc->avdpLocations[n], 1,
                                     "AVDP", "ECMA 3/8.5",
                                     TRUE, FALSE, pass );
    }

    /* 2nd: Main and Reserve VDS sequences
     *      and VDS/VDP continuation extents.
     * first extent defined in used AVDP.
     * Shall not overlap with rewritable or overwritable partition.
     * bool flags:
     *  Maybe ECMA 3/8.5 overlap error, NOT expectedInNonAlloc.
     */
    if( mc->avdp != NULL )      /* first Main extent */
    { checkExtentPartitionOverlap( mc,
                        mc->avdp->mainVolumeDescriptorSequenceExtent.extentLocation,
        ROUNDUPELEMENTS(mc->avdp->mainVolumeDescriptorSequenceExtent.extentLength,
                        blockSize), "Main VDS", "ECMA 3/8.5",
                                   TRUE, FALSE, pass );
    }
    if( mc->avdp != NULL )      /* first Reserve extent */
    { checkExtentPartitionOverlap( mc,
                        mc->avdp->reserveVolumeDescriptorSequenceExtent.extentLocation,
        ROUNDUPELEMENTS(mc->avdp->reserveVolumeDescriptorSequenceExtent.extentLength,
                        blockSize), "Reserve VDS", "ECMA 3/8.5",
                                   TRUE, FALSE, pass );
    }
    /* VDS/VDP Continuation extents, both Main and Reserve.
     */
    for( pContItem = mc->contExtentList;
         pContItem != NULL;
         pContItem = pContItem->next )
    {
        if(   pContItem->sequenceId == CEI_ID_MAINVDS
           || pContItem->sequenceId == CEI_ID_RESVDS )
        {   ExtentAd *pEad = &pContItem->aad.extentAd;
            UCTASSERT( pContItem->adType == ADT_EXTENT );

#ifdef  UCT_TESTING_CONTINUATION
/**/fprintf(uctout, "DEBUG: %s continuation extent: %lu, length: %lu\n",
/**/                tidTEXT4(pContItem->tagId),
/**/            pEad->extentLocation, pEad->extentLength);
/**/fflush(uctout);
#endif  /** UCT_TESTING_CONTINUATION **/

            checkExtentPartitionOverlap( mc,
                               pEad->extentLocation,
               ROUNDUPELEMENTS(pEad->extentLength,
                               blockSize), "VDS Continuation", "ECMA 3/8.5",
                                           TRUE, FALSE, pass );
        }
    }

    /* 3rd: Volume Integrity Sequence extent (LVIDs)
     *      and continuation extents.
     * bool flags:
     *  No overlap error, NOT expectedInNonAlloc.
     */
    if( mc->vi != NULL && mc->vi->lvd != NULL )     /* first extent */
    { checkExtentPartitionOverlap( mc,
                        mc->vi->lvd->integritySequenceExtent.extentLocation,
        ROUNDUPELEMENTS(mc->vi->lvd->integritySequenceExtent.extentLength,
                        blockSize), "LVID Sequence", "ECMA 3/8.5",
                                   FALSE, FALSE, pass );
    }
    /* LVID Sequence Continuation extents.
     */
    for( pContItem = mc->contExtentList;
         pContItem != NULL;
         pContItem = pContItem->next )
    {   if( pContItem->sequenceId == CEI_ID_LVID )
        {   ExtentAd *pEad = &pContItem->aad.extentAd;
            UCTASSERT( pContItem->adType == ADT_EXTENT );

#ifdef  UCT_TESTING_CONTINUATION
/**/fprintf(uctout, "DEBUG: %s continuation extent: %lu, length: %lu\n",
/**/                tidTEXT4(pContItem->tagId),
/**/            pEad->extentLocation, pEad->extentLength);
/**/fflush(uctout);
#endif  /** UCT_TESTING_CONTINUATION **/

            checkExtentPartitionOverlap( mc,
                               pEad->extentLocation,
               ROUNDUPELEMENTS(pEad->extentLength,
                               blockSize), "LVID Sequence Continuation",
                                         "ECMA 3/8.5",
                                         FALSE, FALSE, pass );
        }
    }

    /* 4th: Check Sparing Tables and Sparing Area Partition overlap.
     */
    (void) checkSparingTablesPartitionOverlap(mc, pass);

    return TRUE;        /* ok, no fatal error */

}   /* end checkVolumeDescriptorsPartitionOverlap() */


/* restoreMetadataSharedAlHead():
 * local to buildPartitionBitmaps().
 */
static void restoreMetadataSharedAlHead(
                    UdfMountContext   *mc,
                    UdfAllocationItem *alHeadToRestore,
                    bool restoreMetaAlHead,
                    bool restoreMirrorAlHead )

{
    /* maybe restore Metadata or mirror file al->head
     */
    if(      restoreMetaAlHead )
    {   mc->metadataFile->node->al->head = alHeadToRestore;
    }
    else if( restoreMirrorAlHead )
    {   mc->metadataMirrorFile->node->al->head = alHeadToRestore;
    }
}       /* end restoreMetadataSharedAlHead() */

/* buildPartitionBitmaps():
 * Build Partition Bitmaps using UdfMountContext mc
 * and nodePointerArray as sorted as by qsortNodesFileLinkCount()
 * as for npCheckFileLinkCounts(). Sorting this way is assumed
 * to be done already before calling this function.
 * First check if volume descriptors overlap with partition space,
 * If so, mark the overlapping extents in the partition bitmap.
 * Detect multiple allocations by first checking if Space Bitmap
 * bits are already set before actually setting them.
 * In order to be able to flag the first occurrence of a
 * multiple allocation, a two pass approach is chosen.
 * In Pass one, all multiple allocated extents are gathered in
 * the mc->multAlloc list, but no multiple allocation error
 * messages are printed yet.
 * If multiple allocations are found, a second pass will follow
 * where the bitmap is preloaded with the extents in the
 * mc->multAlloc list, gathered in pass one.
 * Other error messages will be printed in pass one in order
 * to separate different type of messages and because there
 * may be no pass two at all.
 *
 * NOTE: It is important that ALL bitmap allocation is done using
 *       the function markAndVerifyPartitionAllocation() that
 *       will act according to the pass identification.
 */
static bool buildPartitionBitmaps( NodePointerArray *pNpa,
                                   UdfMountContext *mc )
{
    UdfAllocationItem *alHeadToRestore = NULL;
    bool    restoreMetaAlHead   = FALSE,
            restoreMirrorAlHead = FALSE;
    int     pass;
    Uint32  nmbOfBlocks;
    Uint16  pRef,
            numberOfPartitionMaps
                = (Uint16) mc->vi->lvd->numberOfPartitionMaps;

    /* Avoid messages for legal multiple allocations for Metadata Mirror File
     */
    if( IS_PREF_PARTITION_FOUND(mc->metadataPref) )
    {   PartitionMapInfo *pmi = &mc->partitionMapInfo[mc->metadataPref];
        if(   mc->metadataFile != NULL
           && mc->metadataFile->node != NULL
           && mc->metadataMirrorFile != NULL
           && mc->metadataMirrorFile->node != NULL
           && (  pmi->pPartitionMap->type2PartitionMap.SharedTail.metadataTail.flags
                 & MP_DUPMETADATA_MASK) == 0 )
        {
            /* Duplicate Metadata Flag not set.
             * Tricky:
             * In order to avoid multiple allocation errors in this case,
             * temporary remove the Metadata File file allocations or the
             * Metadata Mirror File file allocations (the one that is not
             * in use), because they both reference the same extents.
             * DO NOT REMOVE overheadList, because a possible AED in the
             * overheadList must always be duplicated, also if the
             * Duplicate Metadata Flag is not set !!!
             * Restore it at all exits of buildPartitionBitmaps()
             * using restoreMetadataSharedAlHead() !!!!!!
             */
            if(   mc->metadataFile->node->al != NULL
               && mc->metadataMirrorFile->node->al != NULL )
            { /* temporary remove the one that is not in use
               * by translateMetadataAddress().
               */
              UdfAllocationList *al = pmi->metadataRec.pMetadataFileAllocList;
              if( al == mc->metadataMirrorFile->node->al )  /* mirror used */
              {   al = mc->metadataFile->node->al;        /* remove MF ADs */
                  restoreMetaAlHead = TRUE;
              }
              else if( al == mc->metadataFile->node->al )    /* MF is used */
              {   al = mc->metadataMirrorFile->node->al; /* remove MMF ADs */
                  restoreMirrorAlHead = TRUE;
              }
              else
              {   UCTASSERT( FALSE );       /* not here please */
              }
              alHeadToRestore = al->head;
              al->head = NULL;          /* temporary, restore later */
            }
        }
    }

    /* check if multiple allocations were found in pass 1
     * (but no multiple allocation messages printed yet),
     * if none, then skip pass 2.
     */
    for( pass = 1;
         pass == 1 || (pass == 2 && mc->multAlloc != NULL);
         pass++ )
    {
        if( pass == 2 )     /* multiple allocations found in pass 1 */
        {   /* Free bitmap for all partitions and preload
             * the mc->multAlloc extents.
             */
            UCTASSERT(   mc != NULL && mc->vi != NULL && mc->vi->lvd != NULL
                      && mc->partitionMapInfo != NULL );

            for( pRef = 0; pRef < numberOfPartitionMaps; pRef++ )
            {   PartitionMapInfo *pmi = &mc->partitionMapInfo[pRef];
                /* pmi->pPartAlloc == NULL for virtual partition,
                 * so no special test needed.
                 */
                checkFree((void**)&pmi->pPartAlloc);    /* free bitmap */
/** TODO: check this carefully in case bitmaps could not
 ** be allocated due to out of mem problems (partAllocOutOfMem set).
 **/
            }
            /* All partition bitmaps cleared now, markPartitionAllocation()
             * will allocate new ones (only if no partAllocOutOfMem problem).
             * Preload with mc->multAlloc extents, generated during pass 1.
             */
            if( !preloadBitmapsWithMultAllocList(mc, &nmbOfBlocks) )
            {
                restoreMetadataSharedAlHead( mc, alHeadToRestore,
                                             restoreMetaAlHead,
                                             restoreMirrorAlHead );
                return FALSE;   /* fatal error */
            }
            VERBOSE00(uctout,
              "\n  ==>\tNote: %lu multiple allocated block%s"
                                        " inside partition space,\n"
                    "-\t      list all corresponding multiple"
                                        " allocation errors:\n\n",
                       nmbOfBlocks, PLURAL_S(nmbOfBlocks));

        }   /* endif pass 2 */

        /* common part for pass 1 and 2
         * print multiple allocation messages in pass 2
         */
        if(   !checkVolumeDescriptorsPartitionOverlap(mc, pass)
           || !npBuildPartitionBitmaps(pNpa, mc, pass) )
        {
            restoreMetadataSharedAlHead( mc, alHeadToRestore,
                               restoreMetaAlHead,
                               restoreMirrorAlHead );
            return FALSE;   /* fatal error */
        }

        if( pass == 2 )     /* that's all for pass 2 */
        {   VERBOSE00(uctout,
              "  ==>\tEnd of multiple allocations inside partition space.\n");
        }
        else    /* finalize pass 1 and prepare for possible pass 2 */
        {   Uint32 markedBlocks;
            /* calculate free space per partition.
             */
            for( pRef = 0; pRef < numberOfPartitionMaps; pRef++ )
            {   PartitionMapInfo *pmi = &mc->partitionMapInfo[pRef];
                /* Note: pmi->calculatedFreeSpace is already initialized
                 *       as MAX_UINT32 meaning: free space could not (yet)
                 *       be determined).
                 */
                if( markPartitionAllocation( mc, pRef,
                                         0, pmi->actualPartitionLength,
                                        &markedBlocks, TRUE ) ) /* inspectOnly */
                { pmi->calculatedFreeSpace =   /* markPartitionAllocation() ok */
                        pmi->actualPartitionLength - markedBlocks;
                }
            }
            /* if  no multiple allocations were found in this pass 1,
             * then mc->multAlloc == NULL and pass 2 will be skipped.
             */
        }
    }       /* endfor pass 1 and maybe 2 */

    /* maybe restore Metadata or mirror file al->head
     */
    restoreMetadataSharedAlHead( mc, alHeadToRestore,
                                 restoreMetaAlHead,
                                 restoreMirrorAlHead );
    return TRUE;

}   /* end buildPartitionBitmaps() */


extern bool checkFileStructure( UdfMountContext *mc )
{
    const MediumInfo *vmi = getTheMediumInfo();
    NodePointerArray nodePointerArray = INIT_VPA_STRUCT;
    Uint32  totalFiles,    totalDirs,
            totalFilesMFD, totalDirsMFD,
            totalStreams,  totalStreamDirs,
            totalStreamsMFD;

    /* Install rootNode and systemStreamDirNode (if any)
     * on mount context mc.
     * Read and expand systemStreamDirNode first in order not
     * to intermix verification output with the rootNode output,
     * because it is not part of the rootNode directory hierarchy.
     */
    if(   getUctMinUdfRevision() >= 0x200
       && mc->fsd->descriptorTag.descriptorVersion == 3
       && adGetExtentSize(&mc->fsd->new200.systemStreamDirectoryICB) != 0 )
    {
        VERBOSE00(uctout, "\n\tRead System Stream Directory\n");
        totalFiles    = totalDirs       =
        totalFilesMFD = totalDirsMFD    =
        totalStreams  = totalStreamsMFD = 0;
        totalStreamDirs = 1;                /* system stream directory */
        if( !udfReadSystemStreamDirectoryNode(mc) )
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            printAndClearUctErrorMessage("-");
            fprintf(uctout, " checkFileStructure error:"
                            " udfReadSystemStreamDirectoryNode failed\n");
          MLIMITend;
          /* no FALSE return reuslt */
        }
        else if(    mc->systemStreamDirNode != NULL
                && !expandDirectoryHierarchy(
                         mc, mc->systemStreamDirNode,
                        &mc->fsd->new200.systemStreamDirectoryICB,
                        &totalFiles,    &totalDirs,
                        &totalFilesMFD, &totalDirsMFD,
                        &totalStreams,  &totalStreamDirs,
                        &totalStreamsMFD,
                         0) )           /* top recursion level */
        { MLIMITbegin(ERROR00level,uctMessageLimit);
            printAndClearUctErrorMessage("-");
            fprintf(uctout, " checkFileStructure error:"
                            " expandDirectoryHierarchy failed\n");
          MLIMITend;
          /* no FALSE return reuslt */
        }
    }

    /* now rootNode and its complete directory hierarchy
     * including all streams and EAs.
     */
    totalFiles   = totalFilesMFD   = totalDirsMFD =
    totalStreams = totalStreamDirs = totalStreamsMFD = 0;
    totalDirs = 1;                          /* root directory */
    VERBOSE00(uctout, "\n\tRead Root Directory\n");

    if( !udfReadRootNode(mc) )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        printAndClearUctErrorMessage("-");
        fprintf(uctout,
            " checkFileStructure error: udfReadRootNode failed\n");
      MLIMITend;
      return FALSE;
    }
    if( !expandDirectoryHierarchy( mc, mc->rootNode,
                                  &mc->fsd->rootDirectoryICB,
                                  &totalFiles,    &totalDirs,
                                  &totalFilesMFD, &totalDirsMFD,
                                  &totalStreams,  &totalStreamDirs,
                                  &totalStreamsMFD,
                                   0) )     /* top recursion level */
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        printAndClearUctErrorMessage("-");
        fprintf(uctout,
            " checkFileStructure error: expandDirectoryHierarchy failed\n");
      MLIMITend;
      return FALSE;
    }
    VERBOSE00(uctout, "\n\tEnd of directory tree expansion\n"
                        "\tExcluding deleted FIDs with cleared ICB\n");

    /* Check presence of Non-Allocatable Space
     */
    (void) checkPresenceOfNonAllocatableSpace(mc);

    /* Create node pointer array of all nodes in root directory and
     * system stream directory hierarchies, as well as a possible VAT
     * node and metadata file nodes.
     * Remove deleted FID nodes, nodes with ICB extent size 0 and multiple
     * occurences of subnodes of hard links that would otherwise be
     * traversed multiple times.
     * Node pointer array will be sorted according to qsortNodesFileLinkCount(),
     * ready for use by npCheckFileLinkCounts().
     */
    if( !createTotalNodePointerArray(mc, &nodePointerArray) )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
            "Serious error: createTotalNodePointerArray failed\n");
      MLIMITend;
      checkFree((void**) &nodePointerArray.arr);    /* set NULL */
      return FALSE;
    }

    VERBOSE00(uctout,
      "\n====>\tTesting File Link Count by cross reference of %lu paths.\n"
        "\tFile Link Count errors will be identified here by the\n"
        "\tphysical address of the File Entry as well as all\n"
        "\tpaths identifying the File Entry.\n"
        "\tThe physical address of the File Entry is also shown in\n"
        "\tthe informational read block messages above.\n"
        "\tNote that errors found here may have been reported before\n"
        "\tor may be caused by other previously reported errors.\n",
            nodePointerArray.len);
    fflush(uctout);

    if( !npCheckFileLinkCounts(&nodePointerArray, mc) )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
            "Serious error: npCheckFileLinkCounts failed\n");
      MLIMITend;
    }

    /* Check if USD really references space that is ready for use.
     */
    VERBOSE00(uctout,
      "\n====>\tTesting free Volume Space in USD Allocation Descriptors\n");
    fflush(uctout);

    if( !checkUsdOverlap(mc) )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
            "Serious error: checkUsdOverlap() failed\n");
      MLIMITend;
      /** TODO: check: abort, extit() ?? **/
    }

    /* Build Partition Bitmaps using mc UdfMountContext
     * and nodePointerArray as sorted as by npCheckFileLinkCounts().
     * Also check (volume) structures that overlap with
     * partition space.
     */
    VERBOSE00(uctout,
      "\n====>\tBuild Partition Space Bitmaps.\n"
             "\tAlso check structures that overlap"
                                " with partition space.\n");
    fflush(uctout);

    if( !buildPartitionBitmaps(&nodePointerArray, mc) )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
            "Serious error: buildPartitionBitmaps() failed\n");
      MLIMITend;
      /** TODO: check: abort, extit() ?? **/
    }

    /* cmpMetadataFiles() must be called here and only here
     * before verifyPartitionAllocation(), because the latter
     * will destroy the (just created) bitmaps !!!!
     * If the compare is actually done, this will be notified
     * by cmpMetadataFiles().
     * It will only compare if the Duplicate Metadata Flag is set.
     */
    if( IS_PREF_PARTITION_FOUND(mc->metadataPref) )
    {
        (void) cmpMetadataFiles(mc);
    }
    fflush(uctout);

    VERBOSE00(uctout,
        "\n====>\tPartition Allocation summary :\n");
    fflush(uctout);
    (void) verifyPartitionAllocation(mc);
    /* -----------------------------------------------------
     * NOTE that verifyPartitionAllocation() is DESTRUCTIVE,
     * pmi->pPartAlloc and pPss->fabricatedBitmap are
     * destroyed and freed now !!!!!!!!
     * -----------------------------------------------------
     */

    /* Free Space values determined by buildPartitionBitmaps(),
     * so now final LVID/VAT test can be done.
     */
    VERBOSE00(uctout, "\n====>\tFinal LVID%s verification\n",
                (vmi->sequentialType == MTYPE_SE_SEQUENTIAL)
                ? "/VAT" : "");
    fflush(uctout);
    (void) verifyFinalLVIDandVAT(mc, totalFiles,    totalDirs,
                                     totalFilesMFD, totalDirsMFD);

    /* test UniqueID uniqueness and mapping.
     * means resorting nodePointerArray using qsortNodesUniqueID().
     */
    VERBOSE00(uctout,
        "\n====>\tTesting uniqueness of relevant UniqueIDs.\n\n");
    fflush(uctout);

    if( !npFinalCheckUniqueID(&nodePointerArray, mc) )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout, "Serious error: npFinalCheckUniqueID failed\n");
      MLIMITend;
    }

    /* clean up
     */
    checkFree((void**) &nodePointerArray.arr);  /* set NULL */

    return TRUE;

}   /* end checkFileStructure() */

