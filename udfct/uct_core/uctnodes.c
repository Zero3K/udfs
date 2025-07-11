/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : UDF verifier uct_core
 *
 * File        : uctnodes.c
 *
 * Description : Node abstractions
 *
 * History     : 20011102 Gerrit Scholl, creation
 */

#include <stdio.h>

#include "mytypes.h"
#include "general.h"
#include "uctdata.h"
#include "ucterror.h"
#include "uctstatus.h"
#include "uctgeneral.h"
#include "uctfiles.h"
#include "uctnodes.h"


/* nodeGetMainNode:
 * Determine main association node of an object.
 * Main node can be the node itself or recursive
 * following the node->parent link in case of
 * and EA, Stream or StreamDir node.
 * For SysStreamDir node, the main association
 * node is the SysStreamDir node itself.
 */
extern Node *nodeGetMainNode(Node *node)
{
    /* main node is node itself if no
     * higher node in hierachy.
     * rootDir, SysStreamDir, VAT
     */
    if(    node == NULL
        || node->parent == NULL
        || node->parent == node )   /* points to itself */
    {
        return node;
    }

    /* node->parent != NULL
     * no top node, main node is 'higher' node if:
     * (use recursion)
     */
    if(    NODEFLAGS_IS_SET(node, NFB_EA)
        || NODEFLAGS_IS_SET(node, NFB_STREAM)
        || NODEFLAGS_IS_SET(node, NFB_STREAMDIR) )
    {
        return nodeGetMainNode(node->parent);
    }

    /* otherwise main node is node itself
     */
    return node;

}   /* end nodeGetMainNode() */


/* nodeTraverseHierarchy:
 * Traverse node hierarchy and process each node
 * one by one, using function processNode().
 * nodeTraverseHierarchy() uses itself recursively.
 *
 * Order of traverse:
 * 1) if processTopNode == TRUE then execute processNode(node, ... )
 * 2) traverse node->eaFileNode hierarchy and processNode() for all nodes
 * 3) traverse node->streamDirNode hierarchy and processNode() for all nodes
 * 4) traverse all children hierarchies and processNode() for all nodes
 *    starting with node->firstChild.
 *
 * return value:
 *  if node == NULL
 *  then: TRUE (and no further action)
 *  else if processNode() or nodeTraverseHierarchy()
 *          error (FALSE return value)
 *  then: FALSE
 *  else: TRUE
 */
extern bool nodeTraverseHierarchy( Node *node,
               bool (*processNode)(Node *n, void *arg02),
                                   void *arg02,
                                   bool  processTopNode)
{   Node *child;

    if( node == NULL )
    {   return TRUE;            /* ok */
    }
    if(   (processTopNode && !processNode(node, arg02))
       || !nodeTraverseHierarchy(node->eaFileNode,
                                 processNode, arg02, TRUE)      /* processTopNode */
       || !nodeTraverseHierarchy(node->streamDirNode,
                                 processNode, arg02, TRUE) )    /* processTopNode */
    {   return FALSE;
    }
    for( child = node->firstChild;
         child != NULL;
         child = child->nextInDirectory )
    {   if( !nodeTraverseHierarchy(child,
                                   processNode, arg02, TRUE) )  /* processTopNode */
        {   return FALSE;
        }
    }
    return TRUE;

}   /* end nodeTraverseHierarchy() */

/* voidPointerArrayAdd:
 * Add void pointer to voidPointerArray,
 * return value:
 *  if voidPointer == NULL or allocation error
 *  then: FALSE
 *  else: TRUE
 *
 * Note: All kind of ponters can be handled,
 *       e.g. see nodePointerArrayAddNode(), etc.
 */
extern bool voidPointerArrayAdd(void *vp, void *vpa)
{
    VoidPointerArrayHead *npa = (VoidPointerArrayHead*) vpa;
    VoidPointer *newArr;
    Uint32 alen = npa->alen;    /* allocated length */

    if( vp == NULL )
    {   return FALSE;           /* error */
    }
    if( npa->len >= alen )      /* (re)allocate array */
    {   if(      alen == 0 )
                 alen  = VPA_INIT;
        else if( alen >= VPA_MAXINC )
                 alen += VPA_MAXINC;
        else     alen *= 2;

        if( (newArr = (VoidPointer*) tst_realloc(npa->arr,
                                alen * sizeof(VoidPointer),
                                __FILE__,__LINE__)) == NULL )
        {   checkFree((void**) &npa->arr);
            npa->alen = npa->len = 0;
            uctExit(EXIT_OUT_OF_MEMORY);    /* quit */
        }
        npa->alen = alen;
        npa->arr  = newArr;
    }
    /* be aware of possible npa->start reallocation
     */
    npa->arr[(npa->len)++] = vp;    /* add */
    return TRUE;

}   /* end voidPointerArrayAdd() */


/* nodePointerArrayAddNode:
 * Add node to nodePointer array,
 * return value:
 *  if node == NULL or allocation error
 *  then: FALSE
 *  else: TRUE
 *
 * Note: The NodePointerArray pointer vnpa is passed
 * as a void pointer in order to enable the use of
 * nodePointerArrayAddNode() by nodeTraverseHierarchy();
 */
extern bool nodePointerArrayAddNode(Node *pNode, void *vnpa)
{
    UCTASSERT( sizeof(void*) == sizeof(Node*) );
    return voidPointerArrayAdd((void*) pNode, vnpa);
}

/* nodePointerAddHierarchy:
 * Add all nodes in node hierarchy to nodePointer array,
 * using nodeTraverseHierarchy() and nodePointerArrayAddNode().
 * If processTopNode == FALSE then the top node is skipped.
 *
 * return value:
 *  if no error
 *  then: TRUE
 *  else: FALSE
 *
 * note: node == NULL means no action and is not an error.
 */
extern bool nodePointerAddHierarchy(NodePointerArray *npArray,
                                    Node *node, bool processTopNode)
{
    return nodeTraverseHierarchy(node, nodePointerArrayAddNode,
                                 (void*) npArray, processTopNode);
}

/********** General print stuff *************************/

/* Print messages including txt1 + 'node name' + txt2
 * txt1 and txt2 may be NULL.
 * Node name is printed using nodePrintPath() in NPP_SPECIAL mode.
 *
 * If node == NULL then NOTHING is printed
 * (so no txt1 and txt2 either).
 * See also nodePrintUnicodeNameTxtExtra().
 */
extern void nodePrintUnicodeNameTxt(Node *node,
                                    UdfMountContext *mc,
                                    char *txt1, char *txt2)
{
    if( node == NULL ) return;      /* no action */

    if( txt1 != NULL ) fprintf(uctout, "%s", txt1);

    (void) nodePrintPath(node, mc, NPP_SPECIAL,
                         VERBOSE00level, txt2);
    return;
}

/* Print messages 'node->unicodeName'.
 * Same as nodePrintUnicodeNameTxt(),
 * but extra ", refers to: ..." text in case of a parent FID.
 */
extern void nodePrintUnicodeNameTxtExtra(Node *node,
                                         UdfMountContext *mc,
                                         char *txt1, char *txt2)
{   Node *dirNode, *parentNode;

    if(   node == NULL || node->fid == NULL
       || (   dirNode =    node->parent) == NULL
       || (parentNode = dirNode->parent) == NULL
       || isBitOff(node->fid->fileCharacteristics, FCB_PARENT) )
    {
        nodePrintUnicodeNameTxt(node, mc, txt1, txt2);
    }
    else    /* parent FID */
    {   nodePrintUnicodeNameTxt(node, mc, txt1, NULL);
        nodePrintUnicodeNameTxt(parentNode, mc,
                                ", refers to: ", txt2);
    }
}

/* Print messages 'node->unicodeName' continuation line
 */
extern void nodePrintUnicodeNameContLine(Node *node,
                                         UdfMountContext *mc)
{
    nodePrintUnicodeNameTxtExtra(node,mc, "-\t\t name: ", "\n");
}

/* get name text for special case nodes
 * return value:
 *  if (node->UnicodeName != NULL)
 *  then: NULL
 *  else: character pointer to node name text.
 */
extern char *nodeGetNoUnicodeText(Node *node,
                                  UdfMountContext *mc)
{   Node *pNode;
    char *txt = NULL;

    if( node->unicodeName != NULL )
    {   return NULL;
    }
    if( mc != NULL )
    {   if(      node == mc->rootNode )
            txt = "<root>";
        else if( node == mc->systemStreamDirNode )
            txt = "<SysStreamDir>//";
        else if( node == mc->vatNode )
            txt = "<VAT File>";
        else if(   mc->metadataFile != NULL
                && node == mc->metadataFile->node )
            txt = "<Metadata File>";
        else if(   mc->metadataMirrorFile != NULL
                && node == mc->metadataMirrorFile->node )
            txt = "<Metadata Mirror File>";
        else if(   mc->metadataBitmapFile != NULL
                && node == mc->metadataBitmapFile->node )
            txt = "<Metadata Bitmap File>";
    }
    /* check parent node
     */
    pNode = node->parent;
    if( txt == NULL && pNode != NULL )
    {   if(      node == pNode->eaFileNode )
            txt = "<EA file>";
        else if( node == pNode->streamDirNode )
            txt = "//";
    }
    /* check FID fileCharacteristics
     */
    if( txt == NULL && node->fid != NULL )  /* FID exists */
    {   Uint8 fileChar = node->fid->fileCharacteristics;
        if(      isBitOn(fileChar, FCB_PARENT) )
            txt = "<parent FID>";
        else if( isBitOn(fileChar, FCB_DELETED))
            txt = "<deleted FID, no name>";
        else
            txt = "<FID name error>";
    }
    if( txt == NULL )       /* still not found */
        txt = "<unknown node>";
    return txt;
}

/* nodePrintPath:
 * Print node path
 * There are 3 modes (nppMode) of printing:
 * (values #defined in uctgeneral.h)
 *
 * NPP_FULL         full node path
 * NPP_SINGLE       node only
 * NPP_SPECIAL      see explanation
 *
 * NPP_SPECIAL starts with node and goes up in the
 * node hierarchy untill a node that is not a stream
 * and has (unicodeName != NULL). If no such node
 * is found, then print full node path.
 * NPP_FULL is the default, which means that any nppMode
 * value that is not NPP_SINGLE and not NPP_SPECIAL
 * is considered to be NPP_FULL.
 *
 * return value:
 *  In case of an error
 *  then: 0
 *  else: path length in unicode characters, where
 *        separators and special cases are counted
 *        as only one unicode character.
 *
 * Note that the return value will also be calculated in
 * case nothing is printed because vLevel out of range.
 */
extern Uint32 nodePrintPath(Node *node,
                            UdfMountContext *mc,
                            Uint8 nppMode,
                            Uint8 vLevel,
                            char *extraText)
{   Node    *tmpNode, *prevNode, *pNode;
    char    *txt;
    Uint32   n, pathLen = 0;
    bool     ready;
    static NodePointerArray npa = INIT_VPA_STRUCT;

    if( node == NULL )
    {   printUnicodeName(NULL, 0, FALSE, /* NOT printTrailZeros */
                         TRUE, vLevel);  /* isPath */
        return 0;   /* no extraText printed !! */
    }

    /* Determine node path, start with node and go
     * up the node parent pointer untill node pointing
     * to itself or to NULL is found.
     * This may be aborted earlier if nppMode is
     * NPP_SINGLE or NPP_SPECIAL using ready flag.
     * Mind that a node parent is not always the same
     * as the parent in the UDF spec.
     * Store node pointers in NodePointer array
     * and later get them back in reverse order.
     * Reuse static NodePointerArray of previous call
     * in order to minimise tst_realloc() calls,
     * so init npa.alen and npa.start only once.
     */
    npa.len = 0;
    for( tmpNode  = node, prevNode = NULL, ready = FALSE;
         tmpNode != NULL && tmpNode != prevNode && !ready;
         prevNode = tmpNode, tmpNode = tmpNode->parent )
    {
        if( !nodePointerArrayAddNode(tmpNode, &npa) )
        {   return 0;       /* error */
        }
        if( nppMode == NPP_SINGLE )     /* single node */
        {   ready = TRUE;
        }
        else if(   nppMode == NPP_SPECIAL
                && tmpNode->unicodeName != NULL )
        {   /* Unicode name available, ready if NO stream.
             * Node is stream if node parent is streamDir.
             */
            if(   (   (pNode = tmpNode->parent) == NULL
                   ||            pNode->parent  == NULL
                   || pNode != pNode->parent->streamDirNode)
               && (   mc == NULL
                   || pNode != mc->systemStreamDirNode) )
            {   ready = TRUE; /* unicode name available */
            }        /* and parent node is NO streamDir */
        }
    }

    /* handle path nodes in reverse order
     */
    prevNode = NULL;
    for( n = npa.len; n > 0; )      /* get in reverse order */
    {   tmpNode = npa.arr[--n];     /* node pointer array */

        /* print '/' separator between 2 FIDs only
         */
        if(    tmpNode->fid != NULL
           &&      prevNode != NULL
           && prevNode->fid != NULL )
        {   ifVERBOSE(vLevel) fprintf(uctout, "/");
            ENDif;
            pathLen++;          /* count as one */
        }

        if( tmpNode->unicodeName != NULL )  /* 'normal' case */
        {   printUnicodeName(tmpNode->unicodeName,
                             tmpNode->unicodeNameLen,
                       TRUE, TRUE, vLevel);  /* printTrailZeros, isPath */
            pathLen += tmpNode->unicodeNameLen;
        }
        else    /* no unicode name, special case node */
        {   /* get name from nodeGetNoUnicodeText().
             * exception:
             *  print / instead of <root> if it
             *  is followed by a FID node.
             */
            if(   mc != NULL && tmpNode == mc->rootNode && n != 0
               && ((NodePointer) npa.arr[n-1])->fid != NULL )
                 txt = "/";     /* instead of "<root>" */
            else txt = nodeGetNoUnicodeText(tmpNode,mc);

            ifVERBOSE(vLevel) fprintf(uctout, txt);
            ENDif;
            pathLen++;  /* count special cases as one */
        }
        prevNode = tmpNode;
    }

    if( extraText != NULL )
    {   ifVERBOSE(vLevel) fprintf(uctout, "%s", extraText);
        ENDif;
    }
    return pathLen;

}   /* end nodePrintPath() */


/* freeFileNodeInfo():
 */
extern void freeFileNodeInfo( FileNodeInfo **ppSpecialFile )
{
    if( (*ppSpecialFile) != NULL )
    {   nodeFreeHierarchy((*ppSpecialFile)->node);
        checkFree((void**)&((*ppSpecialFile)->data));
    }
    checkFree((void**)ppSpecialFile);
}

