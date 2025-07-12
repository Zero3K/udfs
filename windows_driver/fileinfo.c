/*
 * UDFS - UDF File System Driver for Windows/ReactOS
 * File information implementation
 */

#include "udfsprocs.h"

NTSTATUS
UdfsQueryInformation(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
    PIO_STACK_LOCATION IrpSp;
    PFILE_OBJECT FileObject;
    PUDFS_FCB Fcb;
    NTSTATUS Status;
    FILE_INFORMATION_CLASS FileInformationClass;
    PVOID Buffer;
    ULONG BufferLength;
    
    UNREFERENCED_PARAMETER(DeviceObject);
    
    IrpSp = IoGetCurrentIrpStackLocation(Irp);
    FileObject = IrpSp->FileObject;
    
    Fcb = (PUDFS_FCB)FileObject->FsContext;
    if (!Fcb) {
        UDFS_DEBUG_ERROR_ONCE("File information query with null FCB\n");
        Status = STATUS_INVALID_PARAMETER;
        goto complete;
    }
    
    FileInformationClass = IrpSp->Parameters.QueryFile.FileInformationClass;
    BufferLength = IrpSp->Parameters.QueryFile.Length;
    Buffer = Irp->AssociatedIrp.SystemBuffer;
    
    UDFS_DEBUG_INFO_ONCE("File information query, class=%d, buffer length=%lu\n", 
                         FileInformationClass, BufferLength);
    
    if (!Buffer) {
        UDFS_DEBUG_ERROR_ONCE("File information query with null buffer\n");
        Status = STATUS_INVALID_PARAMETER;
        goto complete;
    }
    
    switch (FileInformationClass) {
        case FileBasicInformation:
            Status = UdfsQueryBasicInformation(Fcb, Buffer, BufferLength, &Irp->IoStatus.Information);
            break;
            
        case FileStandardInformation:
            Status = UdfsQueryStandardInformation(Fcb, Buffer, BufferLength, &Irp->IoStatus.Information);
            break;
            
        case FileInternalInformation:
            Status = UdfsQueryInternalInformation(Fcb, Buffer, BufferLength, &Irp->IoStatus.Information);
            break;
            
        case FileEaInformation:
            Status = UdfsQueryEaInformation(Fcb, Buffer, BufferLength, &Irp->IoStatus.Information);
            break;
            
        case FileNameInformation:
            Status = UdfsQueryNameInformation(Fcb, FileObject, Buffer, BufferLength, &Irp->IoStatus.Information);
            break;
            
        case FilePositionInformation:
            Status = UdfsQueryPositionInformation(FileObject, Buffer, BufferLength, &Irp->IoStatus.Information);
            break;
            
        default:
            Status = STATUS_INVALID_INFO_CLASS;
            Irp->IoStatus.Information = 0;
            break;
    }
    
complete:
    Irp->IoStatus.Status = Status;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return Status;
}

NTSTATUS
UdfsSetInformation(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
    /* Read-only file system - no set operations allowed */
    UNREFERENCED_PARAMETER(DeviceObject);
    
    Irp->IoStatus.Status = STATUS_MEDIA_WRITE_PROTECTED;
    Irp->IoStatus.Information = 0;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    
    return STATUS_MEDIA_WRITE_PROTECTED;
}

NTSTATUS
UdfsQueryBasicInformation(
    IN PUDFS_FCB Fcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_BASIC_INFORMATION BasicInfo;
    
    if (BufferLength < sizeof(FILE_BASIC_INFORMATION)) {
        *Information = 0;
        return STATUS_BUFFER_TOO_SMALL;
    }
    
    BasicInfo = (PFILE_BASIC_INFORMATION)Buffer;
    RtlZeroMemory(BasicInfo, sizeof(FILE_BASIC_INFORMATION));
    
    BasicInfo->CreationTime = Fcb->CreationTime;
    BasicInfo->LastAccessTime = Fcb->LastAccessTime;
    BasicInfo->LastWriteTime = Fcb->LastWriteTime;
    BasicInfo->ChangeTime = Fcb->LastWriteTime;  /* UDF doesn't track change time separately */
    BasicInfo->FileAttributes = Fcb->FileAttributes;
    
    *Information = sizeof(FILE_BASIC_INFORMATION);
    return STATUS_SUCCESS;
}

NTSTATUS
UdfsQueryStandardInformation(
    IN PUDFS_FCB Fcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_STANDARD_INFORMATION StandardInfo;
    
    if (BufferLength < sizeof(FILE_STANDARD_INFORMATION)) {
        *Information = 0;
        return STATUS_BUFFER_TOO_SMALL;
    }
    
    StandardInfo = (PFILE_STANDARD_INFORMATION)Buffer;
    RtlZeroMemory(StandardInfo, sizeof(FILE_STANDARD_INFORMATION));
    
    StandardInfo->AllocationSize = Fcb->AllocationSize;
    StandardInfo->EndOfFile = Fcb->FileSize;
    StandardInfo->NumberOfLinks = 1;
    StandardInfo->DeletePending = FALSE;
    StandardInfo->Directory = BooleanFlagOn(Fcb->Flags, UDFS_FCB_DIRECTORY);
    
    *Information = sizeof(FILE_STANDARD_INFORMATION);
    return STATUS_SUCCESS;
}

NTSTATUS
UdfsQueryInternalInformation(
    IN PUDFS_FCB Fcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_INTERNAL_INFORMATION InternalInfo;
    
    if (BufferLength < sizeof(FILE_INTERNAL_INFORMATION)) {
        *Information = 0;
        return STATUS_BUFFER_TOO_SMALL;
    }
    
    InternalInfo = (PFILE_INTERNAL_INFORMATION)Buffer;
    
    /* Use FCB address as file index for uniqueness */
    InternalInfo->IndexNumber.QuadPart = (ULONG_PTR)Fcb;
    
    *Information = sizeof(FILE_INTERNAL_INFORMATION);
    return STATUS_SUCCESS;
}

NTSTATUS
UdfsQueryEaInformation(
    IN PUDFS_FCB Fcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_EA_INFORMATION EaInfo;
    
    UNREFERENCED_PARAMETER(Fcb);
    
    if (BufferLength < sizeof(FILE_EA_INFORMATION)) {
        *Information = 0;
        return STATUS_BUFFER_TOO_SMALL;
    }
    
    EaInfo = (PFILE_EA_INFORMATION)Buffer;
    
    /* UDF supports extended attributes, but we report none for simplicity */
    EaInfo->EaSize = 0;
    
    *Information = sizeof(FILE_EA_INFORMATION);
    return STATUS_SUCCESS;
}

NTSTATUS
UdfsQueryNameInformation(
    IN PUDFS_FCB Fcb,
    IN PFILE_OBJECT FileObject,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_NAME_INFORMATION NameInfo;
    UNICODE_STRING FileName;
    ULONG RequiredSize;
    
    UNREFERENCED_PARAMETER(Fcb);
    
    /* For simplicity, use the file object's file name */
    FileName = FileObject->FileName;
    
    RequiredSize = FIELD_OFFSET(FILE_NAME_INFORMATION, FileName) + FileName.Length;
    
    if (BufferLength < RequiredSize) {
        *Information = RequiredSize;
        return STATUS_BUFFER_OVERFLOW;
    }
    
    NameInfo = (PFILE_NAME_INFORMATION)Buffer;
    NameInfo->FileNameLength = FileName.Length;
    
    if (FileName.Length > 0) {
        memcpy(NameInfo->FileName, FileName.Buffer, FileName.Length);
    }
    
    *Information = RequiredSize;
    return STATUS_SUCCESS;
}

NTSTATUS
UdfsQueryPositionInformation(
    IN PFILE_OBJECT FileObject,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_POSITION_INFORMATION PositionInfo;
    
    if (BufferLength < sizeof(FILE_POSITION_INFORMATION)) {
        *Information = 0;
        return STATUS_BUFFER_TOO_SMALL;
    }
    
    PositionInfo = (PFILE_POSITION_INFORMATION)Buffer;
    PositionInfo->CurrentByteOffset = FileObject->CurrentByteOffset;
    
    *Information = sizeof(FILE_POSITION_INFORMATION);
    return STATUS_SUCCESS;
}