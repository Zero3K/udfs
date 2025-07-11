/*
 * UDFS - UDF File System Driver for Windows/ReactOS
 * Directory control implementation
 */

#include "udfsprocs.h"

NTSTATUS
UdfsDirectoryControl(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
    PIO_STACK_LOCATION IrpSp;
    NTSTATUS Status;
    
    UNREFERENCED_PARAMETER(DeviceObject);
    
    IrpSp = IoGetCurrentIrpStackLocation(Irp);
    
    switch (IrpSp->MinorFunction) {
        case IRP_MN_QUERY_DIRECTORY:
            Status = UdfsQueryDirectory(DeviceObject, Irp);
            break;
            
        case IRP_MN_NOTIFY_CHANGE_DIRECTORY:
            /* Read-only file system - no change notifications */
            Status = STATUS_INVALID_DEVICE_REQUEST;
            break;
            
        default:
            Status = STATUS_INVALID_DEVICE_REQUEST;
            break;
    }
    
    if (Status != STATUS_PENDING) {
        Irp->IoStatus.Status = Status;
        IoCompleteRequest(Irp, IO_NO_INCREMENT);
    }
    
    return Status;
}

NTSTATUS
UdfsQueryDirectory(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
    PIO_STACK_LOCATION IrpSp;
    PFILE_OBJECT FileObject;
    PUDFS_FCB Fcb;
    PUDFS_CCB Ccb;
    NTSTATUS Status;
    PVOID Buffer;
    ULONG BufferLength;
    FILE_INFORMATION_CLASS FileInformationClass;
    PUNICODE_STRING FileName;
    BOOLEAN RestartScan;
    BOOLEAN ReturnSingleEntry;
    BOOLEAN IndexSpecified;
    ULONG FileIndex;
    
    UNREFERENCED_PARAMETER(DeviceObject);
    
    IrpSp = IoGetCurrentIrpStackLocation(Irp);
    FileObject = IrpSp->FileObject;
    
    Fcb = (PUDFS_FCB)FileObject->FsContext;
    Ccb = (PUDFS_CCB)FileObject->FsContext2;
    
    /* Check if this is a directory */
    if (!(Fcb->Flags & UDFS_FCB_DIRECTORY)) {
        Status = STATUS_INVALID_PARAMETER;
        goto complete;
    }
    
    /* Get query parameters */
    BufferLength = IrpSp->Parameters.QueryDirectory.Length;
    FileInformationClass = IrpSp->Parameters.QueryDirectory.FileInformationClass;
    FileName = IrpSp->Parameters.QueryDirectory.FileName;
    RestartScan = BooleanFlagOn(IrpSp->Flags, SL_RESTART_SCAN);
    ReturnSingleEntry = BooleanFlagOn(IrpSp->Flags, SL_RETURN_SINGLE_ENTRY);
    IndexSpecified = BooleanFlagOn(IrpSp->Flags, SL_INDEX_SPECIFIED);
    FileIndex = IrpSp->Parameters.QueryDirectory.FileIndex;
    
    /* Get output buffer */
    Buffer = Irp->UserBuffer;
    if (!Buffer) {
        Status = STATUS_INVALID_PARAMETER;
        goto complete;
    }
    
    /* Handle restart scan */
    if (RestartScan) {
        Ccb->DirectoryIndex = 0;
    }
    
    /* Handle index specified */
    if (IndexSpecified) {
        Ccb->DirectoryIndex = FileIndex;
    }
    
    /* Enumerate directory entries */
    Status = UdfsEnumerateDirectory(
        Fcb,
        Ccb,
        Buffer,
        BufferLength,
        FileInformationClass,
        FileName,
        ReturnSingleEntry,
        &Irp->IoStatus.Information
        );
    
complete:
    Irp->IoStatus.Status = Status;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return Status;
}

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
    )
{
    NTSTATUS Status = STATUS_SUCCESS;
    ULONG BytesWritten = 0;
    PUCHAR CurrentBuffer = (PUCHAR)Buffer;
    BOOLEAN FoundEntry = FALSE;
    
    UNREFERENCED_PARAMETER(Fcb);
    UNREFERENCED_PARAMETER(FileName);
    
    *Information = 0;
    
    __try {
        /* For demonstration, return a few dummy entries */
        if (Ccb->DirectoryIndex == 0) {
            /* Return "." entry */
            Status = UdfsCreateDirectoryEntry(
                L".",
                1 * sizeof(WCHAR),
                FILE_ATTRIBUTE_DIRECTORY,
                FileInformationClass,
                CurrentBuffer,
                BufferLength - BytesWritten,
                &BytesWritten
                );
                
            if (NT_SUCCESS(Status)) {
                CurrentBuffer += BytesWritten;
                FoundEntry = TRUE;
                Ccb->DirectoryIndex++;
                
                if (ReturnSingleEntry) {
                    goto complete;
                }
            }
        }
        
        if (Ccb->DirectoryIndex == 1 && BytesWritten < BufferLength) {
            /* Return ".." entry */
            ULONG EntrySize;
            Status = UdfsCreateDirectoryEntry(
                L"..",
                2 * sizeof(WCHAR),
                FILE_ATTRIBUTE_DIRECTORY,
                FileInformationClass,
                CurrentBuffer,
                BufferLength - BytesWritten,
                &EntrySize
                );
                
            if (NT_SUCCESS(Status)) {
                BytesWritten += EntrySize;
                FoundEntry = TRUE;
                Ccb->DirectoryIndex++;
                
                if (ReturnSingleEntry) {
                    goto complete;
                }
            }
        }
        
        /* No more entries */
        if (!FoundEntry) {
            Status = STATUS_NO_MORE_FILES;
        }
        
complete:
        *Information = BytesWritten;
        
    } __except(EXCEPTION_EXECUTE_HANDLER) {
        Status = STATUS_INVALID_USER_BUFFER;
        *Information = 0;
    }
    
    return Status;
}

NTSTATUS
UdfsCreateDirectoryEntry(
    IN PWCHAR FileName,
    IN ULONG FileNameLength,
    IN ULONG FileAttributes,
    IN FILE_INFORMATION_CLASS FileInformationClass,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG EntrySize
    )
{
    NTSTATUS Status = STATUS_SUCCESS;
    
    switch (FileInformationClass) {
        case FileDirectoryInformation:
            {
                PFILE_DIRECTORY_INFORMATION DirInfo = (PFILE_DIRECTORY_INFORMATION)Buffer;
                ULONG RequiredSize = FIELD_OFFSET(FILE_DIRECTORY_INFORMATION, FileName) + FileNameLength;
                
                if (BufferLength < RequiredSize) {
                    return STATUS_BUFFER_OVERFLOW;
                }
                
                RtlZeroMemory(DirInfo, RequiredSize);
                DirInfo->FileAttributes = FileAttributes;
                DirInfo->FileNameLength = FileNameLength;
                RtlCopyMemory(DirInfo->FileName, FileName, FileNameLength);
                
                *EntrySize = RequiredSize;
            }
            break;
            
        case FileFullDirectoryInformation:
            {
                PFILE_FULL_DIR_INFORMATION DirInfo = (PFILE_FULL_DIR_INFORMATION)Buffer;
                ULONG RequiredSize = FIELD_OFFSET(FILE_FULL_DIR_INFORMATION, FileName) + FileNameLength;
                
                if (BufferLength < RequiredSize) {
                    return STATUS_BUFFER_OVERFLOW;
                }
                
                RtlZeroMemory(DirInfo, RequiredSize);
                DirInfo->FileAttributes = FileAttributes;
                DirInfo->FileNameLength = FileNameLength;
                RtlCopyMemory(DirInfo->FileName, FileName, FileNameLength);
                
                *EntrySize = RequiredSize;
            }
            break;
            
        default:
            Status = STATUS_INVALID_INFO_CLASS;
            break;
    }
    
    return Status;
}