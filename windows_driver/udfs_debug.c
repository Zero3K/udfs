/*
 * UDFS - UDF File System Driver for Windows/ReactOS
 * Debug output implementation
 * 
 * Copyright (c) 2024 UDF Driver Project
 * 
 * This file implements verbose debug output that only appears under WinDbg
 * when DEBUG is defined and ensures messages print only once.
 */

#include "udfsprocs.h"
#include "udfs_debug.h"
#include <stdarg.h>

#ifdef DEBUG

/* Global debug state */
UDFS_DEBUG_STATE UdfsDebugState = {0};

/*
 * Initialize the debug system
 */
VOID UdfsInitializeDebugSystem(VOID)
{
    ULONG i;
    
    if (UdfsDebugState.Initialized) {
        return;
    }
    
    /* Initialize the mutex for synchronization */
    ExInitializeFastMutex(&UdfsDebugState.DebugMutex);
    
    /* Initialize hash table */
    for (i = 0; i < UDFS_DEBUG_HASH_SIZE; i++) {
        UdfsDebugState.HashTable[i] = NULL;
    }
    
    /* Initialize message pool */
    for (i = 0; i < UDFS_MAX_DEBUG_MESSAGES; i++) {
        UdfsDebugState.MessagePool[i].Hash = 0;
        UdfsDebugState.MessagePool[i].Printed = FALSE;
        UdfsDebugState.MessagePool[i].Next = NULL;
    }
    
    UdfsDebugState.NextFreeMessage = 0;
    
    /* Enable all debug categories by default */
    UdfsDebugState.EnabledCategories = UDFS_DEBUG_ALL;
    
    UdfsDebugState.Initialized = TRUE;
    
    /* Print initialization message */
    DbgPrint("UDFS: Debug system initialized - verbose output enabled\n");
}

/*
 * Cleanup the debug system
 */
VOID UdfsCleanupDebugSystem(VOID)
{
    if (!UdfsDebugState.Initialized) {
        return;
    }
    
    /* Print cleanup message */
    DbgPrint("UDFS: Debug system cleaned up\n");
    
    UdfsDebugState.Initialized = FALSE;
}

/*
 * Simple hash function for strings
 */
ULONG UdfsHashString(PCSTR String)
{
    ULONG Hash = 5381;
    INT c;
    
    while ((c = *String++)) {
        Hash = ((Hash << 5) + Hash) + c; /* Hash * 33 + c */
    }
    
    return Hash;
}

/*
 * Check if a debug message should be printed and mark it as printed
 */
BOOLEAN UdfsShouldPrintMessage(ULONG Category, ULONG Hash)
{
    ULONG HashIndex;
    PUDFS_DEBUG_MESSAGE Message;
    PUDFS_DEBUG_MESSAGE NewMessage;
    
    /* Check if category is enabled */
    if (!(UdfsDebugState.EnabledCategories & Category)) {
        return FALSE;
    }
    
    HashIndex = Hash & UDFS_DEBUG_HASH_MASK;
    
    /* Search for existing message in hash table */
    Message = UdfsDebugState.HashTable[HashIndex];
    while (Message != NULL) {
        if (Message->Hash == Hash) {
            /* Message found - check if already printed */
            if (Message->Printed) {
                return FALSE; /* Already printed */
            }
            /* Mark as printed and allow printing */
            Message->Printed = TRUE;
            return TRUE;
        }
        Message = Message->Next;
    }
    
    /* Message not found - add new entry if we have space */
    if (UdfsDebugState.NextFreeMessage >= UDFS_MAX_DEBUG_MESSAGES) {
        /* No more space - print anyway but don't track */
        return TRUE;
    }
    
    /* Get next free message structure */
    NewMessage = &UdfsDebugState.MessagePool[UdfsDebugState.NextFreeMessage++];
    NewMessage->Hash = Hash;
    NewMessage->Printed = TRUE;
    NewMessage->Next = UdfsDebugState.HashTable[HashIndex];
    UdfsDebugState.HashTable[HashIndex] = NewMessage;
    
    return TRUE;
}

/*
 * Print a debug message only once
 */
BOOLEAN UdfsDebugPrintOnce(ULONG Category, PCSTR Format, ...)
{
    va_list args;
    CHAR Buffer[512];
    ULONG Hash;
    BOOLEAN ShouldPrint;
    INT Result;
    
    if (!UdfsDebugState.Initialized) {
        return FALSE;
    }
    
    /* Format the message using vsprintf (available in kernel mode) */
    va_start(args, Format);
    Result = vsprintf(Buffer, Format, args);
    va_end(args);
    
    if (Result < 0) {
        /* Fallback for formatting errors */
        strcpy(Buffer, "UDFS: Debug message formatting error\n");
    }
    
    /* Calculate hash of the formatted message */
    Hash = UdfsHashString(Buffer);
    
    /* Acquire mutex for thread safety */
    ExAcquireFastMutex(&UdfsDebugState.DebugMutex);
    
    ShouldPrint = UdfsShouldPrintMessage(Category, Hash);
    
    ExReleaseFastMutex(&UdfsDebugState.DebugMutex);
    
    /* Print message if it should be printed */
    if (ShouldPrint) {
        DbgPrint("%s", Buffer);
        return TRUE;
    }
    
    return FALSE;
}

#else /* !DEBUG */

/* When DEBUG is not defined, provide empty implementations */

#endif /* DEBUG */