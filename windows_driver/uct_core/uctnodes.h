/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : UDF verifier uct_core
 *
 * File        : uctnodes.h
 *
 * Description : Node abstraction prototypes
 *
 * History     : 20011102 Gerrit Scholl, creation
 */

#ifndef __UCT_UCTNODES_H__
#define __UCT_UCTNODES_H__

#include "mytypes.h"
#include "general.h"
#include "uctdata.h"

#define NodePointerArray VoidPointerArrayHead

typedef struct Node
{
    unicode_t       *unicodeName;
    Uint32           unicodeNameLen;    /* in char, not bytes */
    Uint32           nrOfChildren;
    struct Node     *parent;
    struct Node     *firstChild;
    struct Node     *lastChild;
    struct Node     *nextInDirectory;
    struct Node     *eaFileNode;            /* EA Space file */
    struct Node     *streamDirNode;

    FileIdentifierDescriptor *fid;
    Byte            *fidsAllocation;

    UdfAllocationList *al;
    Byte            *fe;            /* File Entry or Extended File Entry */
    Uint64           feTotalExtentsLength;  /* swapAndVerifyAllocationDescriptors, */
    Uint64           feFileBodyLength;      /* etc. */
    Uint32           feNumberOfADs;
    Uint32           fePriorDirectEntries;  /* may imply Strategy Type too */
    Uint16           fePartRef;
    Uint16           nodeFlags;     /* see NFB_* definitions in uctnodes.h */
} Node, *NodePointer;


/* UDF mount context
 */
#define MAX_AVDP_LOCATIONS  4   /* Max number of possible AVDP locations */

typedef struct
{
    Node        *node;
    Byte        *data;
    Uint32       size32;
    LongAd       icb;
} FileNodeInfo;


/* Special partition ref number for PARTITION NOT FOUND
 */
#define PREF_PARTITION_NOT_FOUND        ((Uint16) MAX_UINT16)

#define IS_PREF_PARTITION_NOT_FOUND(XX) (XX == PREF_PARTITION_NOT_FOUND)
#define IS_PREF_PARTITION_FOUND(XX)     (XX != PREF_PARTITION_NOT_FOUND)

typedef struct
{
    Device                           *device;
    Uint32                            nmbOfAvdps;
    Uint32                            avdpLocations[MAX_AVDP_LOCATIONS];
    AnchorVolumeDescriptorPointer    *avdp;
    LogicalVolumeIntegrityDescriptor *lvid;
    FileSetDescriptor                *fsd;
    UdfVolumeInformation            *vi;
    Node                           *rootNode;
    Node                          *systemStreamDirNode;
    Node                         *nonAllocatableSpaceNode;
    bool                          nonAllocSpaceIsLookalike;
    Node                        *vatNode;
    LongAd                       vatICB;
        /* There can be at most one virtual partition or at most one
         * metadata partition. Their Partition Reference number (Pref) will
         * be recorded here, at the same time it is a global registration
         * whether such a partition is present or not, using the
         *          IS_PREF_PARTITION_FOUND(Pref)
         *      and IS_PREF_PARTITION_NOT_FOUND(Pref) macro's.
         */
    Uint16                   virtualPref;   /* initially PREF_PARTITION_NOT_FOUND */
    Uint16                   sparablePref;  /* initially PREF_PARTITION_NOT_FOUND */
    Uint16                   metadataPref;  /* initially PREF_PARTITION_NOT_FOUND */
    FileNodeInfo            *metadataFile;
    FileNodeInfo            *metadataMirrorFile;
    FileNodeInfo            *metadataBitmapFile;
    PartitionMapInfo    *partitionMapInfo;
        /* partitionMapInfo points to array of PartitionMapInfo structs,
         * one for each partition map, indexed by partitionReferenceNumber.
         * A PartitionMapInfo struct holds partition type, pointer
         * to corresponding PartitionDescriptor and pointers to VAT Table
         * and Sparing Table.
         */
    ContExtentItem      *contExtentList; /* continuation extents */
    UdfAllocationList   *multAlloc;      /* buildPartitionBitmaps() */
    Uint64               nextUniqueId;
    Uint8                nextUniqueIdStatus; /* see #define NEXTUNIQUEID_ */
    Uint64               maxFeUniqueID;
    Uint32               maxFidUniqueID;
} UdfMountContext;

/* nextUniqueIdStatus definitions:
 */
#define NEXTUNIQUEID_UNDEFINED      0   /* initially */
#define NEXTUNIQUEID_WRAP32         1
#define NEXTUNIQUEID_NO_WRAP32      2

#define UNIQUEID_LOW32_LESS_THAN_16(UU) (((Uint32)(UU)) < 16)


/* NFB_* Uint16 Node.nodeFlags bit definitions:
 *       bit numbers:
 */
#define NFB_PARENTFID        0
#define NFB_EA               1
#define NFB_VAT              2
#define NFB_DIREXPAND        3  /* Directory, no StreamDir */
#define NFB_STREAMDIR        4
#define NFB_STREAM           5
#define NFB_SYSTEM           6  /* System StreamDir hierarchy  */
#define NFB_MARKED_DELETE    7  /* MFD FID, FE, etc. still in use  */
#define NFB_UNUSED_FID       8  /* unused FID, ready for reuse */
#define NFB_UNIQUEID_MAP     9  /* relevant for UniqueID map   */
#define NFB_METADATAFILE    10
#define NFB_METADATAMIRROR  11
#define NFB_METADATABITMAP  12

/* NODEFLAGS_SET_* and NODEFLAGS_IS_* macros.
 * depend on node and nodeFlags only
 */
#define NODEFLAGS_SET_BIT(XNODE, XNFB_BITNMB)   \
    setBitOn((XNODE)->nodeFlags, (XNFB_BITNMB));

#define NODEFLAGS_IS_SET(PNODE, NFB_BITNMB) \
    (   (PNODE) != NULL                     \
     && isBitOn((PNODE)->nodeFlags, (NFB_BITNMB)) )

#define NODEFLAGS_IS_ROOTDIR(PNODE) \
    (    (PNODE) != NULL            \
     &&  (PNODE)->parent == (PNODE) \
     && !NODEFLAGS_IS_SET((PNODE), (NFB_SYSTEM)) )

#define NODEFLAGS_IS_SYSTEMSTREAMDIR(PNODE) \
    (    (PNODE) != NULL            \
     &&  (PNODE)->parent == (PNODE) \
     &&  NODEFLAGS_IS_SET((PNODE), (NFB_SYSTEM)) )



/* nodeGetMainNode:
 * Determine main node of an object. Main node can be the
 * node itself or one going up in the node hierarchy
 * following the node->parent link.
 */
extern Node *nodeGetMainNode(Node *node);


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
                                   bool  processTopNode);

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
extern bool voidPointerArrayAdd(void *vp, void *vpa);

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
extern bool nodePointerArrayAddNode(Node *node, void *vnpa);

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
                                    Node *node, bool processTopNode);


/* get name text for special case nodes
 * return value:
 *  if (node->UnicodeName != NULL)
 *  then: NULL
 *  else: character pointer to node name text.
 */
extern char *nodeGetNoUnicodeText(Node *node,
                                  UdfMountContext *mc);

/* nodePrintPath:
 * Print node path
 * There are 3 modes (nppMode) of printing:
 *
 */
#define NPP_FULL    0       /* full node path */
#define NPP_SINGLE  1       /* node only */
#define NPP_SPECIAL 2       /* see explanation */
/*
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
                            char *extraText);

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
                                    char *txt1, char *txt2);

/* Print messages 'node->unicodeName'...
 * Same as nodePrintUnicodeNameTxt(),
 * but extra ", refers to: ..." text in case of a parent FID.
 */
extern void nodePrintUnicodeNameTxtExtra(Node *node,
                                         UdfMountContext *mc,
                                         char *txt1, char *txt2);

/* Print messages 'node->unicodeName' continuation line
 */
extern void nodePrintUnicodeNameContLine(Node *node,
                                         UdfMountContext *mc);

/* freeFileNodeInfo():
 * Clean readMetadataSpecialFile() output arguments
 * initially and in case of errors.
 * Each parameter maybe NULL -> no action.
 * Also called outside readMetadataSpecialFile().
 */
extern void freeFileNodeInfo( FileNodeInfo **ppSpecialFile );

#endif  /* __UCT_UCTNODES_H__ */

