/*
 * UDFS - UDF File System Driver for Windows/ReactOS
 * Debug output infrastructure
 * 
 * Copyright (c) 2024 UDF Driver Project
 * 
 * This file provides verbose debug output that only appears under WinDbg
 * when DEBUG is defined and ensures messages print only once.
 */

#ifndef _UDFS_DEBUG_H_
#define _UDFS_DEBUG_H_

#include <ntifs.h>

#ifdef DEBUG

/* Maximum number of unique debug messages we can track */
#define UDFS_MAX_DEBUG_MESSAGES 256

/* Hash table size for tracking printed messages (power of 2) */
#define UDFS_DEBUG_HASH_SIZE 128
#define UDFS_DEBUG_HASH_MASK (UDFS_DEBUG_HASH_SIZE - 1)

/* Debug message categories */
#define UDFS_DEBUG_MOUNT    0x00000001
#define UDFS_DEBUG_CREATE   0x00000002
#define UDFS_DEBUG_READ     0x00000004
#define UDFS_DEBUG_WRITE    0x00000008
#define UDFS_DEBUG_DIRCTRL  0x00000010
#define UDFS_DEBUG_FSCTRL   0x00000020
#define UDFS_DEBUG_CLEANUP  0x00000040
#define UDFS_DEBUG_CLOSE    0x00000080
#define UDFS_DEBUG_UDFCT    0x00000100
#define UDFS_DEBUG_DEVICE   0x00000200
#define UDFS_DEBUG_INFO     0x00000400
#define UDFS_DEBUG_ERROR    0x00000800
#define UDFS_DEBUG_ALL      0xFFFFFFFF

/* Structure to track printed debug messages */
typedef struct _UDFS_DEBUG_MESSAGE {
    ULONG Hash;                    /* Hash of the message */
    BOOLEAN Printed;               /* Whether this message was already printed */
    struct _UDFS_DEBUG_MESSAGE *Next; /* Next in hash chain */
} UDFS_DEBUG_MESSAGE, *PUDFS_DEBUG_MESSAGE;

/* Global debug state */
typedef struct _UDFS_DEBUG_STATE {
    BOOLEAN Initialized;
    ULONG EnabledCategories;       /* Bitmask of enabled debug categories */
    FAST_MUTEX DebugMutex;        /* Synchronization for debug operations */
    PUDFS_DEBUG_MESSAGE HashTable[UDFS_DEBUG_HASH_SIZE]; /* Hash table for tracking messages */
    UDFS_DEBUG_MESSAGE MessagePool[UDFS_MAX_DEBUG_MESSAGES]; /* Pre-allocated message structures */
    ULONG NextFreeMessage;        /* Index of next free message in pool */
} UDFS_DEBUG_STATE, *PUDFS_DEBUG_STATE;

/* Global debug state instance */
extern UDFS_DEBUG_STATE UdfsDebugState;

/* Function prototypes */
VOID UdfsInitializeDebugSystem(VOID);
VOID UdfsCleanupDebugSystem(VOID);
BOOLEAN UdfsDebugPrintOnce(ULONG Category, PCSTR Format, ...);
ULONG UdfsHashString(PCSTR String);

/* Debug macros - only active when DEBUG is defined */
#define UDFS_DEBUG_PRINT_ONCE(Category, Format, ...) \
    UdfsDebugPrintOnce(Category, Format, ##__VA_ARGS__)

#define UDFS_DEBUG_MOUNT_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_MOUNT, "UDFS: [MOUNT] " Format, ##__VA_ARGS__)

#define UDFS_DEBUG_CREATE_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_CREATE, "UDFS: [CREATE] " Format, ##__VA_ARGS__)

#define UDFS_DEBUG_READ_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_READ, "UDFS: [READ] " Format, ##__VA_ARGS__)

#define UDFS_DEBUG_WRITE_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_WRITE, "UDFS: [WRITE] " Format, ##__VA_ARGS__)

#define UDFS_DEBUG_DIRCTRL_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_DIRCTRL, "UDFS: [DIRCTRL] " Format, ##__VA_ARGS__)

#define UDFS_DEBUG_FSCTRL_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_FSCTRL, "UDFS: [FSCTRL] " Format, ##__VA_ARGS__)

#define UDFS_DEBUG_CLEANUP_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_CLEANUP, "UDFS: [CLEANUP] " Format, ##__VA_ARGS__)

#define UDFS_DEBUG_CLOSE_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_CLOSE, "UDFS: [CLOSE] " Format, ##__VA_ARGS__)

#define UDFS_DEBUG_UDFCT_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_UDFCT, "UDFS: [UDFCT] " Format, ##__VA_ARGS__)

#define UDFS_DEBUG_DEVICE_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_DEVICE, "UDFS: [DEVICE] " Format, ##__VA_ARGS__)

#define UDFS_DEBUG_INFO_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_INFO, "UDFS: [INFO] " Format, ##__VA_ARGS__)

#define UDFS_DEBUG_ERROR_ONCE(Format, ...) \
    UDFS_DEBUG_PRINT_ONCE(UDFS_DEBUG_ERROR, "UDFS: [ERROR] " Format, ##__VA_ARGS__)

/* Convenience macros for common debug scenarios */
#define UDFS_DEBUG_ENTER_ONCE(FunctionName) \
    UDFS_DEBUG_INFO_ONCE("Entering %s\n", FunctionName)

#define UDFS_DEBUG_EXIT_ONCE(FunctionName, Status) \
    UDFS_DEBUG_INFO_ONCE("Exiting %s with status=0x%08X\n", FunctionName, Status)

#define UDFS_DEBUG_IRP_ONCE(MajorFunction, MinorFunction) \
    UDFS_DEBUG_INFO_ONCE("Processing IRP: Major=0x%02X, Minor=0x%02X\n", MajorFunction, MinorFunction)

#else /* !DEBUG */

/* When DEBUG is not defined, all debug macros become no-ops */
#define UdfsInitializeDebugSystem() ((void)0)
#define UdfsCleanupDebugSystem() ((void)0)
#define UDFS_DEBUG_PRINT_ONCE(Category, Format, ...) ((void)0)
#define UDFS_DEBUG_MOUNT_ONCE(Format, ...) ((void)0)
#define UDFS_DEBUG_CREATE_ONCE(Format, ...) ((void)0)
#define UDFS_DEBUG_READ_ONCE(Format, ...) ((void)0)
#define UDFS_DEBUG_WRITE_ONCE(Format, ...) ((void)0)
#define UDFS_DEBUG_DIRCTRL_ONCE(Format, ...) ((void)0)
#define UDFS_DEBUG_FSCTRL_ONCE(Format, ...) ((void)0)
#define UDFS_DEBUG_CLEANUP_ONCE(Format, ...) ((void)0)
#define UDFS_DEBUG_CLOSE_ONCE(Format, ...) ((void)0)
#define UDFS_DEBUG_UDFCT_ONCE(Format, ...) ((void)0)
#define UDFS_DEBUG_DEVICE_ONCE(Format, ...) ((void)0)
#define UDFS_DEBUG_INFO_ONCE(Format, ...) ((void)0)
#define UDFS_DEBUG_ERROR_ONCE(Format, ...) ((void)0)

/* When DEBUG is not defined, convenience macros also become no-ops */
#define UDFS_DEBUG_ENTER_ONCE(FunctionName) ((void)0)
#define UDFS_DEBUG_EXIT_ONCE(FunctionName, Status) ((void)0)
#define UDFS_DEBUG_IRP_ONCE(MajorFunction, MinorFunction) ((void)0)

#endif /* DEBUG */

#endif /* _UDFS_DEBUG_H_ */