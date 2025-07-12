/*
 * UDFS - UDF File System Driver for Windows/ReactOS
 * Volume information implementation
 */

#include "udfsprocs.h"

NTSTATUS
UdfsQueryVolumeInformation(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
    PIO_STACK_LOCATION IrpSp;
    PUDFS_VCB Vcb;
    NTSTATUS Status;
    FS_INFORMATION_CLASS FsInformationClass;
    PVOID Buffer;
    ULONG BufferLength;
    
    IrpSp = IoGetCurrentIrpStackLocation(Irp);
    Vcb = (PUDFS_VCB)DeviceObject->DeviceExtension;
    
    FsInformationClass = IrpSp->Parameters.QueryVolume.FsInformationClass;
    BufferLength = IrpSp->Parameters.QueryVolume.Length;
    Buffer = Irp->AssociatedIrp.SystemBuffer;
    
    UDFS_DEBUG_INFO_ONCE("Volume information query, class=%d, buffer length=%lu\n", 
                         FsInformationClass, BufferLength);
    
    if (!Buffer) {
        UDFS_DEBUG_ERROR_ONCE("Volume information query with null buffer\n");
        Status = STATUS_INVALID_PARAMETER;
        goto complete;
    }
    
    switch (FsInformationClass) {
        case FileFsVolumeInformation:
            Status = UdfsQueryFsVolumeInformation(Vcb, Buffer, BufferLength, &Irp->IoStatus.Information);
            break;
            
        case FileFsLabelInformation:
            Status = UdfsQueryFsLabelInformation(Vcb, Buffer, BufferLength, &Irp->IoStatus.Information);
            break;
            
        case FileFsSizeInformation:
            Status = UdfsQueryFsSizeInformation(Vcb, Buffer, BufferLength, &Irp->IoStatus.Information);
            break;
            
        case FileFsDeviceInformation:
            Status = UdfsQueryFsDeviceInformation(Vcb, Buffer, BufferLength, &Irp->IoStatus.Information);
            break;
            
        case FileFsAttributeInformation:
            Status = UdfsQueryFsAttributeInformation(Vcb, Buffer, BufferLength, &Irp->IoStatus.Information);
            break;
            
        case FileFsFullSizeInformation:
            Status = UdfsQueryFsFullSizeInformation(Vcb, Buffer, BufferLength, &Irp->IoStatus.Information);
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
UdfsQueryFsVolumeInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_FS_VOLUME_INFORMATION VolumeInfo;
    UNICODE_STRING VolumeLabel;
    ULONG RequiredSize;
    LARGE_INTEGER CreationTime;
    
    /* Get volume label (dummy for now) */
    RtlInitUnicodeString(&VolumeLabel, L"UDF_VOLUME");
    
    RequiredSize = FIELD_OFFSET(FILE_FS_VOLUME_INFORMATION, VolumeLabel) + VolumeLabel.Length;
    
    if (BufferLength < RequiredSize) {
        *Information = RequiredSize;
        return STATUS_BUFFER_OVERFLOW;
    }
    
    VolumeInfo = (PFILE_FS_VOLUME_INFORMATION)Buffer;
    RtlZeroMemory(VolumeInfo, RequiredSize);
    
    /* Set creation time to Unix epoch for UDF volumes */
    RtlSecondsSince1970ToTime(0, &CreationTime);
    VolumeInfo->VolumeCreationTime = CreationTime;
    
    /* Use VCB address as volume serial number */
    VolumeInfo->VolumeSerialNumber = (ULONG)(ULONG_PTR)Vcb;
    
    VolumeInfo->VolumeLabelLength = VolumeLabel.Length;
    VolumeInfo->SupportsObjects = FALSE;
    
    if (VolumeLabel.Length > 0) {
        memcpy(VolumeInfo->VolumeLabel, VolumeLabel.Buffer, VolumeLabel.Length);
    }
    
    *Information = RequiredSize;
    return STATUS_SUCCESS;
}

NTSTATUS
UdfsQueryFsLabelInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_FS_LABEL_INFORMATION LabelInfo;
    UNICODE_STRING VolumeLabel;
    ULONG RequiredSize;
    
    UNREFERENCED_PARAMETER(Vcb);
    
    /* Get volume label (dummy for now) */
    RtlInitUnicodeString(&VolumeLabel, L"UDF_VOLUME");
    
    RequiredSize = FIELD_OFFSET(FILE_FS_LABEL_INFORMATION, VolumeLabel) + VolumeLabel.Length;
    
    if (BufferLength < RequiredSize) {
        *Information = RequiredSize;
        return STATUS_BUFFER_OVERFLOW;
    }
    
    LabelInfo = (PFILE_FS_LABEL_INFORMATION)Buffer;
    LabelInfo->VolumeLabelLength = VolumeLabel.Length;
    
    if (VolumeLabel.Length > 0) {
        memcpy(LabelInfo->VolumeLabel, VolumeLabel.Buffer, VolumeLabel.Length);
    }
    
    *Information = RequiredSize;
    return STATUS_SUCCESS;
}

NTSTATUS
UdfsQueryFsSizeInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_FS_SIZE_INFORMATION SizeInfo;
    
    if (BufferLength < sizeof(FILE_FS_SIZE_INFORMATION)) {
        *Information = 0;
        return STATUS_BUFFER_TOO_SMALL;
    }
    
    SizeInfo = (PFILE_FS_SIZE_INFORMATION)Buffer;
    
    /* Standard UDF sector size is 2048 bytes */
    SizeInfo->BytesPerSector = 2048;
    SizeInfo->SectorsPerAllocationUnit = 1;
    
    /* Dummy values - in real implementation, get from UDFCT */
    SizeInfo->TotalAllocationUnits.QuadPart = Vcb->VolumeSize.QuadPart / 2048;
    SizeInfo->AvailableAllocationUnits.QuadPart = 0;  /* Read-only */
    
    *Information = sizeof(FILE_FS_SIZE_INFORMATION);
    return STATUS_SUCCESS;
}

NTSTATUS
UdfsQueryFsDeviceInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_FS_DEVICE_INFORMATION DeviceInfo;
    
    if (BufferLength < sizeof(FILE_FS_DEVICE_INFORMATION)) {
        *Information = 0;
        return STATUS_BUFFER_TOO_SMALL;
    }
    
    DeviceInfo = (PFILE_FS_DEVICE_INFORMATION)Buffer;
    
    DeviceInfo->DeviceType = FILE_DEVICE_CD_ROM_FILE_SYSTEM;  /* UDF is typically on optical media */
    DeviceInfo->Characteristics = Vcb->TargetDeviceObject->Characteristics;
    
    /* Add read-only characteristic */
    DeviceInfo->Characteristics |= FILE_READ_ONLY_DEVICE;
    
    *Information = sizeof(FILE_FS_DEVICE_INFORMATION);
    return STATUS_SUCCESS;
}

NTSTATUS
UdfsQueryFsAttributeInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_FS_ATTRIBUTE_INFORMATION AttributeInfo;
    UNICODE_STRING FileSystemName;
    ULONG RequiredSize;
    
    UNREFERENCED_PARAMETER(Vcb);
    
    RtlInitUnicodeString(&FileSystemName, L"UDF");
    
    RequiredSize = FIELD_OFFSET(FILE_FS_ATTRIBUTE_INFORMATION, FileSystemName) + FileSystemName.Length;
    
    if (BufferLength < RequiredSize) {
        *Information = RequiredSize;
        return STATUS_BUFFER_OVERFLOW;
    }
    
    AttributeInfo = (PFILE_FS_ATTRIBUTE_INFORMATION)Buffer;
    RtlZeroMemory(AttributeInfo, RequiredSize);
    
    /* UDF file system attributes */
    AttributeInfo->FileSystemAttributes = 
        FILE_CASE_PRESERVED_NAMES |
        FILE_UNICODE_ON_DISK |
        FILE_PERSISTENT_ACLS |
        FILE_READ_ONLY_VOLUME |
        FILE_VOLUME_IS_COMPRESSED;  /* Some UDF volumes support compression */
        
    AttributeInfo->MaximumComponentNameLength = 255;  /* UDF supports up to 255 character names */
    AttributeInfo->FileSystemNameLength = FileSystemName.Length;
    
    memcpy(AttributeInfo->FileSystemName, FileSystemName.Buffer, FileSystemName.Length);
    
    *Information = RequiredSize;
    return STATUS_SUCCESS;
}

NTSTATUS
UdfsQueryFsFullSizeInformation(
    IN PUDFS_VCB Vcb,
    OUT PVOID Buffer,
    IN ULONG BufferLength,
    OUT PULONG Information
    )
{
    PFILE_FS_FULL_SIZE_INFORMATION FullSizeInfo;
    
    if (BufferLength < sizeof(FILE_FS_FULL_SIZE_INFORMATION)) {
        *Information = 0;
        return STATUS_BUFFER_TOO_SMALL;
    }
    
    FullSizeInfo = (PFILE_FS_FULL_SIZE_INFORMATION)Buffer;
    
    /* Standard UDF sector size is 2048 bytes */
    FullSizeInfo->BytesPerSector = 2048;
    FullSizeInfo->SectorsPerAllocationUnit = 1;
    
    /* Dummy values - in real implementation, get from UDFCT */
    FullSizeInfo->TotalAllocationUnits.QuadPart = Vcb->VolumeSize.QuadPart / 2048;
    FullSizeInfo->CallerAvailableAllocationUnits.QuadPart = 0;  /* Read-only */
    FullSizeInfo->ActualAvailableAllocationUnits.QuadPart = 0;  /* Read-only */
    
    *Information = sizeof(FILE_FS_FULL_SIZE_INFORMATION);
    return STATUS_SUCCESS;
}