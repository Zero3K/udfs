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

/* UDFCT core headers - adapted for kernel mode */
#ifdef KERNEL_MODE
#define printf DbgPrint
#define malloc(size) ExAllocatePoolWithTag(PagedPool, size, 'UDFS')
#define free(ptr) ExFreePoolWithTag(ptr, 'UDFS')
#define FILE void

/* Define basic UDFCT types for kernel mode */
typedef struct _UdfMountContext {
    ULONG Signature;
    PDEVICE_OBJECT TargetDevice;
    ULONG SectorSize;
    /* Additional UDFCT context would go here */
} UdfMountContext;

typedef struct _SessionContext {
    ULONG SessionNumber;
    ULONG StartSector;
    ULONG SectorCount;
} SessionContext;

typedef struct _Node {
    ULONG NodeType;
    LARGE_INTEGER FileSize;
    ULONG FileAttributes;
    /* Additional UDFCT node data would go here */
} Node;

#else
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
    UdfMountContext *MountContext;  /* UDFCT mount context */
    SessionContext *SessionContext; /* UDFCT session context */
    
    /* Volume characteristics */
    ULONG SectorSize;
    LARGE_INTEGER VolumeSize;
    BOOLEAN IsReadOnly;
    
    /* Synchronization */
    ERESOURCE VcbResource;
    ERESOURCE FcbResource;
    
    /* FCB list */
    LIST_ENTRY FcbList;
    LIST_ENTRY VcbLinks;  /* Links in global VCB list */
    
} UDFS_VCB, *PUDFS_VCB;

/* File control block */
typedef struct _UDFS_FCB {
    /* Standard FCB header */
    FSRTL_ADVANCED_FCB_HEADER Header;
    
    /* Links */
    LIST_ENTRY VcbLinks;
    
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
DriverEntry(
    IN PDRIVER_OBJECT DriverObject,
    IN PUNICODE_STRING RegistryPath
    );

VOID
UdfsUnload(
    IN PDRIVER_OBJECT DriverObject
    );

NTSTATUS
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