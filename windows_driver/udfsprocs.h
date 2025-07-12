/*
 * UDFS - UDF File System Driver for Windows/ReactOS
 * Based on UDFCT (UDF Conformance Testing Application)
 * 
 * Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 * Copyright (c) 2024 UDF Driver Project
 * 
 * This file contains the main header definitions for the Windows
 * kernel mode UDF file system driver.
 */

#ifndef _UDFSPROCS_H_
#define _UDFSPROCS_H_

/* Windows kernel headers */
#include <ntifs.h>
#include <ntddk.h>
#include <ntstrsafe.h>
#include <ntdddisk.h>

/* Debug infrastructure */
#include "udfs_debug.h"

/* UDFCT core headers - adapted for kernel mode */
#ifdef KERNEL_MODE

/* Prevent standard library includes that are not available in kernel mode */
/* Only define if not already defined by system headers */
#ifndef _STDIO_H
#define _STDIO_H 1
#endif
#ifndef _STDLIB_H
#define _STDLIB_H 1  
#endif
#ifndef _STRING_H
#define _STRING_H 1
#endif
#ifndef _TIME_H
#define _TIME_H 1
#endif
#ifndef __STDIO_H__
#define __STDIO_H__ 1
#endif
#ifndef __STDLIB_H__
#define __STDLIB_H__ 1
#endif
#ifndef __STRING_H__
#define __STRING_H__ 1
#endif
#ifndef __TIME_H__
#define __TIME_H__ 1  
#endif

/* Windows kernel mode UDFCT adaptation */
#define printf DbgPrint
#define calloc(count, size) UdfsCalloc(count, size)
#define realloc(ptr, size) UdfsReallocatePool(ptr, size)
#define sprintf UdfsSprintf
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

/* Provide kernel mode equivalents for standard library functions */
#define memcpy(dest, src, count) RtlCopyMemory(dest, src, count)
#define memset(ptr, value, count) RtlFillMemory(ptr, count, (UCHAR)(value))
#define memcmp UdfsMemcmp
#define strlen UdfsStrlen
#define strcmp UdfsStrcmp  
#define strcpy UdfsStrcpy
#define strncpy UdfsStrncpy
#define strcat UdfsStrcat
#define vsprintf UdfsVsprintf

/* Function prototypes for kernel mode string functions */
PVOID UdfsCalloc(size_t count, size_t size);
PVOID UdfsRealloc(PVOID ptr, size_t size);
int UdfsSprintf(char *buffer, const char *format, ...);
int UdfsVsprintf(char *buffer, const char *format, va_list args);
size_t UdfsStrlen(const char *str);
int UdfsStrcmp(const char *str1, const char *str2);
int UdfsMemcmp(const void *ptr1, const void *ptr2, size_t count);
char *UdfsStrcpy(char *dest, const char *src);
char *UdfsStrncpy(char *dest, const char *src, size_t count);
char *UdfsStrcat(char *dest, const char *src);

/* Time functions - provide minimal stubs for kernel mode */
typedef long time_t;
struct tm {
    int tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year;
    int tm_wday, tm_yday, tm_isdst;
};
#define time(t) 0
#define mktime(tm) 0
#define localtime(t) NULL

/* Other standard library functions - only define if not already defined */
#ifndef abs
#define abs(x) ((x) < 0 ? -(x) : (x))
#endif
#ifndef labs  
#define labs(x) ((x) < 0 ? -(x) : (x))
#endif
#define exit(code) /* exit not supported in kernel mode */
#define abort() /* abort not supported in kernel mode */

/* Standard library types that may be needed */
/* Use Windows kernel definition of size_t - do not redefine */

/* Forward declare types before including UDFCT headers */
typedef struct Device Device;
typedef struct Node Node;

/* Include core UDFCT headers with adaptations */
#include "mytypes.h"
#include "general.h"
#include "udfstruct.h"
#include "uctdata.h"
#include "uctnodes.h"
#include "uctstatus.h"
#include "uctfiles.h"
#include "uctgeneral.h"
#include "uctallocation.h"
#include "uctverify.h"
#include "uctendian.h"
#include "unicode.h"
#include "device.h"
#include "uctmedium.h"

/* Windows kernel mode device adaptation */
typedef struct _WindowsDeviceAdapter {
    PDEVICE_OBJECT TargetDevice;
    ULONG SectorSize;
    LARGE_INTEGER DeviceSize;
    Device *uctDevice;
} WindowsDeviceAdapter;

/* Memory allocation wrapper for kernel mode */
PVOID UdfsReallocatePool(PVOID OldBuffer, SIZE_T NewSize);

/* Windows device interface functions */
Uint32 WindowsDeviceReadBlock(void *impUse, Uint32 blockSize, 
                             Uint32 firstBlock, Uint32 nrOfBlocks, Byte *buffer);
Uint32 WindowsDeviceWriteBlock(void *impUse, Uint32 blockSize, 
                              Uint32 firstBlock, Uint32 nrOfBlocks, Byte *buffer);
void WindowsDeviceCloseAndFree(void *impUse);
BlockState WindowsDeviceGetBlockState(void *impUse, Uint32 blockNr);

/* UDFCT initialization for Windows */
NTSTATUS UdfsInitializeUdfctDevice(PDEVICE_OBJECT TargetDevice, Device **uctDevice);

#else
/* User mode - include original UDFCT headers */
#include "uct_core.h"
#include "uctfiles.h"
#include "uctgeneral.h"
#include "unicode.h"
#endif

/* Driver tag for memory allocations */
#define UDFS_TAG 'SFDU'

/* File system name */
#define UDFS_DEVICE_NAME L"\\Udfs"
#define UDFS_FS_NAME L"UDF"

/* Volume device object extension */
typedef struct _UDFS_VCB {
    /* Standard VCB header */
    FSRTL_ADVANCED_FCB_HEADER Header;
    
    /* Device and volume info */
    PDEVICE_OBJECT DeviceObject;
    PDEVICE_OBJECT TargetDeviceObject;
    PVPB Vpb;
    
    /* UDF specific data from UDFCT */
    UdfMountContext *MountContext;  /* Full UDFCT mount context */
    
    /* Volume characteristics */
    ULONG SectorSize;
    LARGE_INTEGER VolumeSize;
    BOOLEAN IsReadOnly;
    
    /* Synchronization */
    ERESOURCE VcbResource;
    ERESOURCE FcbResource;
    FAST_MUTEX AdvancedFcbHeaderMutex;
    
    /* FCB list */
    LIST_ENTRY FcbList;
    LIST_ENTRY VcbLinks;  /* Links in global VCB list */
    
} UDFS_VCB, *PUDFS_VCB;

/* File control block */
typedef struct _UDFS_FCB {
    /* Standard FCB header */
    FSRTL_ADVANCED_FCB_HEADER Header;
    
    /* Section object pointers for memory manager */
    SECTION_OBJECT_POINTERS SectionObjectPointers;
    
    /* Links */
    LIST_ENTRY VcbLinks;
    
    /* VCB pointer */
    PUDFS_VCB Vcb;
    
    /* File info from UDFCT */
    Node *UdfNode;                  /* UDFCT file node */
    LARGE_INTEGER FileSize;
    LARGE_INTEGER AllocationSize;
    
    /* File attributes */
    ULONG FileAttributes;
    LARGE_INTEGER CreationTime;
    LARGE_INTEGER LastWriteTime;
    LARGE_INTEGER LastAccessTime;
    
    /* Reference counting */
    LONG ReferenceCount;
    
    /* Flags */
    ULONG Flags;
    
} UDFS_FCB, *PUDFS_FCB;

/* Context control block for file handles */
typedef struct _UDFS_CCB {
    /* Directory enumeration context */
    BOOLEAN IsDirectory;
    ULONG DirectoryIndex;
    
    /* File position */
    LARGE_INTEGER FileOffset;
    
} UDFS_CCB, *PUDFS_CCB;

/* Driver extension for the file system device */
typedef struct _UDFS_DATA {
    /* File system device object */
    PDEVICE_OBJECT FileSystemDeviceObject;
    
    /* Driver object */
    PDRIVER_OBJECT DriverObject;
    
    /* Global resource */
    ERESOURCE GlobalResource;
    
    /* List of mounted volumes */
    LIST_ENTRY VcbList;
    
} UDFS_DATA, *PUDFS_DATA;

/* Global driver data */
extern UDFS_DATA UdfsData;

/* FCB flags */
#define UDFS_FCB_DIRECTORY      0x00000001
#define UDFS_FCB_ROOT_DIRECTORY 0x00000002

/*
 * Function prototypes
 */

/* Main driver entry points (udfs.c) */
NTSTATUS
NTAPI
DriverEntry(
    IN PDRIVER_OBJECT DriverObject,
    IN PUNICODE_STRING RegistryPath
    );

VOID
NTAPI
UdfsUnload(
    IN PDRIVER_OBJECT DriverObject
    );

NTSTATUS
NTAPI
UdfsDispatch(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

/* Create/Open operations (create.c) */
NTSTATUS
UdfsCreate(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

/* Read operations (read.c) */
NTSTATUS
UdfsRead(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

/* Write operations (write.c) */
NTSTATUS
UdfsWrite(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

/* Directory control (dirctrl.c) */
NTSTATUS
UdfsDirectoryControl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

/* File information (fileinfo.c) */
NTSTATUS
UdfsQueryInformation(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
UdfsSetInformation(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

/* Volume information (volinfo.c) */
NTSTATUS
UdfsQueryVolumeInformation(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

/* File system control (fsctrl.c) */
NTSTATUS
UdfsFileSystemControl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

/* Cleanup operations (cleanup.c) */
NTSTATUS
UdfsCleanup(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

/* Close operations (close.c) */
NTSTATUS
UdfsClose(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

/* Utility functions */
NTSTATUS
UdfsCreateVcb(
    IN PDEVICE_OBJECT TargetDevice,
    IN PVPB Vpb,
    OUT PUDFS_VCB *Vcb
    );

VOID
UdfsDeleteVcb(
    IN PUDFS_VCB Vcb
    );

NTSTATUS
UdfsCreateFcb(
    IN PUDFS_VCB Vcb,
    IN Node *UdfNode,
    OUT PUDFS_FCB *Fcb
    );

VOID
UdfsDeleteFcb(
    IN PUDFS_FCB Fcb
    );

NTSTATUS
UdfsCreateCcb(
    OUT PUDFS_CCB *Ccb
    );

VOID
UdfsDeleteCcb(
    IN PUDFS_CCB Ccb
    );

/* UDFCT integration helpers */
NTSTATUS
UdfsInitializeUdfct(
    IN PDEVICE_OBJECT TargetDevice,
    OUT UdfMountContext **MountContext
    );

VOID
UdfsCleanupUdfct(
    IN UdfMountContext *MountContext
    );

NTSTATUS
UdfsReadUdfSectors(
    IN PUDFS_VCB Vcb,
    IN ULONG StartSector,
    IN ULONG SectorCount,
    OUT PVOID Buffer
    );

/* UDFCT Windows adaptation functions (udfct_windows.c) */
NTSTATUS UdfsInitializeUdfctSubsystem(void);
VOID UdfsCleanupUdfctSubsystem(void);
NTSTATUS UdfsCreateUdfctMountContext(PDEVICE_OBJECT TargetDevice, UdfMountContext **MountContext);
VOID UdfsCleanupUdfctMountContext(UdfMountContext *MountContext);
PVOID UdfsReallocatePool(PVOID OldBuffer, SIZE_T NewSize);
NTSTATUS UdfsInitializeUdfctDevice(PDEVICE_OBJECT TargetDevice, Device **uctDevice);

/* Write operation helpers (write.c) */
NTSTATUS
UdfsWriteFileData(
    IN PUDFS_FCB Fcb,
    IN LARGE_INTEGER FileOffset,
    IN ULONG Length,
    IN PVOID Buffer
    );

NTSTATUS
UdfsAllocateFileSpace(
    IN PUDFS_FCB Fcb,
    IN ULONGLONG NewFileSize
    );

BOOLEAN
UdfsWriteBlockToPartition(
    IN UdfMountContext *mc,
    IN PVOID Buffer,
    IN UINT16 PartitionRef,
    IN UINT32 LogicalBlockNr,
    IN UINT32 BlockCount,
    IN UINT32 BlockSize
    );

/* File system control helpers (fsctrl.c) */
NTSTATUS
UdfsMountVolume(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
UdfsVerifyVolume(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
UdfsUserFsRequest(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
UdfsIsUdfVolume(
    IN PDEVICE_OBJECT TargetDevice
    );

/* Create operation helpers (create.c) */
NTSTATUS
UdfsOpenRootDirectory(
    IN PUDFS_VCB Vcb,
    IN PFILE_OBJECT FileObject,
    OUT PUDFS_FCB *Fcb,
    OUT PUDFS_CCB *Ccb
    );

NTSTATUS
UdfsOpenFileByName(
    IN PUDFS_VCB Vcb,
    IN PUNICODE_STRING FileName,
    IN PFILE_OBJECT FileObject,
    OUT PUDFS_FCB *Fcb,
    OUT PUDFS_CCB *Ccb
    );

/* Read operation helpers (read.c) */
NTSTATUS
UdfsReadFileData(
    IN PUDFS_FCB Fcb,
    IN LARGE_INTEGER FileOffset,
    IN ULONG Length,
    OUT PVOID Buffer
    );

NTSTATUS
UdfsReadFileDataFromUdfct(
    IN PUDFS_FCB Fcb,
    IN LARGE_INTEGER FileOffset,
    IN ULONG Length,
    OUT PVOID Buffer
    );

/* Directory control helpers (dirctrl.c) */
NTSTATUS
UdfsQueryDirectory(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    );

NTSTATUS
UdfsEnumerateDirectory(
    IN PUDFS_FCB Fcb,
    IN PUDFS_CCB Ccb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    IN FILE_INFORMATION_CLASS FileInformationClass,
    IN PUNICODE_STRING FileName,
    IN BOOLEAN ReturnSingleEntry,
    OUT PULONG Information
    );

NTSTATUS
UdfsCreateDirectoryEntry(
    IN PWCHAR FileName,
    IN ULONG FileNameLength,
    IN ULONG FileAttributes,
    IN FILE_INFORMATION_CLASS FileInformationClass,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG EntrySize
    );

/* File information helpers (fileinfo.c) */
NTSTATUS
UdfsQueryBasicInformation(
    IN PUDFS_FCB Fcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

NTSTATUS
UdfsQueryStandardInformation(
    IN PUDFS_FCB Fcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

NTSTATUS
UdfsQueryInternalInformation(
    IN PUDFS_FCB Fcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

NTSTATUS
UdfsQueryEaInformation(
    IN PUDFS_FCB Fcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

NTSTATUS
UdfsQueryNameInformation(
    IN PUDFS_FCB Fcb,
    IN PFILE_OBJECT FileObject,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

NTSTATUS
UdfsQueryPositionInformation(
    IN PFILE_OBJECT FileObject,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

/* Volume information helpers (volinfo.c) */
NTSTATUS
UdfsQueryFsVolumeInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

NTSTATUS
UdfsQueryFsLabelInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

NTSTATUS
UdfsQueryFsSizeInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

NTSTATUS
UdfsQueryFsDeviceInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

NTSTATUS
UdfsQueryFsAttributeInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

NTSTATUS
UdfsQueryFsFullSizeInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    );

#endif /* _UDFSPROCS_H_ */