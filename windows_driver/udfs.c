/*
 * UDFS - UDF File System Driver for Windows/ReactOS
 * Main driver implementation
 * 
 * Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 * Copyright (c) 2024 UDF Driver Project
 */

#include "udfsprocs.h"

/* Global driver data */
UDFS_DATA UdfsData;

/*
 * Driver entry point
 */
NTSTATUS
NTAPI
DriverEntry(
    IN PDRIVER_OBJECT DriverObject,
    IN PUNICODE_STRING RegistryPath
    )
{
    NTSTATUS Status;
    PDEVICE_OBJECT DeviceObject;
    UNICODE_STRING DeviceName;
    
    UNREFERENCED_PARAMETER(RegistryPath);
    
    /* Initialize global data */
    RtlZeroMemory(&UdfsData, sizeof(UDFS_DATA));
    UdfsData.DriverObject = DriverObject;
    InitializeListHead(&UdfsData.VcbList);
    ExInitializeResourceLite(&UdfsData.GlobalResource);
    
    /* Initialize debug system */
    UdfsInitializeDebugSystem();
    UDFS_DEBUG_INFO_ONCE("Driver initializing, version compiled at %s %s\n", __DATE__, __TIME__);
    
    /* Initialize UDFCT subsystem */
    Status = UdfsInitializeUdfctSubsystem();
    if (!NT_SUCCESS(Status)) {
        UDFS_DEBUG_ERROR_ONCE("Failed to initialize UDFCT subsystem, status=0x%08X\n", Status);
        UdfsCleanupDebugSystem();
        ExDeleteResourceLite(&UdfsData.GlobalResource);
        return Status;
    }
    
    UDFS_DEBUG_INFO_ONCE("UDFCT subsystem initialized successfully\n");
    
    /* Set up dispatch table */
    DriverObject->MajorFunction[IRP_MJ_CREATE] = UdfsDispatch;
    DriverObject->MajorFunction[IRP_MJ_CLOSE] = UdfsDispatch;
    DriverObject->MajorFunction[IRP_MJ_READ] = UdfsDispatch;
    DriverObject->MajorFunction[IRP_MJ_WRITE] = UdfsDispatch;  /* Add write support */
    DriverObject->MajorFunction[IRP_MJ_QUERY_INFORMATION] = UdfsDispatch;
    DriverObject->MajorFunction[IRP_MJ_SET_INFORMATION] = UdfsDispatch;
    DriverObject->MajorFunction[IRP_MJ_QUERY_VOLUME_INFORMATION] = UdfsDispatch;
    DriverObject->MajorFunction[IRP_MJ_DIRECTORY_CONTROL] = UdfsDispatch;
    DriverObject->MajorFunction[IRP_MJ_FILE_SYSTEM_CONTROL] = UdfsDispatch;
    DriverObject->MajorFunction[IRP_MJ_CLEANUP] = UdfsDispatch;
    
    UDFS_DEBUG_INFO_ONCE("Dispatch table configured for all supported IRP major functions\n");
    
    /* Set unload routine */
    DriverObject->DriverUnload = UdfsUnload;
    
    /* Create the file system device object */
    RtlInitUnicodeString(&DeviceName, UDFS_DEVICE_NAME);
    
    Status = IoCreateDevice(
        DriverObject,
        0,
        &DeviceName,
        FILE_DEVICE_FILE_SYSTEM,
        0,
        FALSE,
        &DeviceObject
        );
        
    if (!NT_SUCCESS(Status)) {
        UDFS_DEBUG_ERROR_ONCE("Failed to create file system device object, status=0x%08X\n", Status);
        UdfsCleanupDebugSystem();
        ExDeleteResourceLite(&UdfsData.GlobalResource);
        return Status;
    }
    
    UdfsData.FileSystemDeviceObject = DeviceObject;
    UDFS_DEBUG_INFO_ONCE("File system device object created successfully\n");
    
    /* Register the file system */
    IoRegisterFileSystem(DeviceObject);
    UDFS_DEBUG_INFO_ONCE("UDF file system driver registered successfully\n");
    
    return STATUS_SUCCESS;
}

/*
 * Driver unload routine
 */
VOID
NTAPI
UdfsUnload(
    IN PDRIVER_OBJECT DriverObject
    )
{
    UNREFERENCED_PARAMETER(DriverObject);
    
    UDFS_DEBUG_INFO_ONCE("Driver unloading\n");
    
    /* Unregister the file system */
    if (UdfsData.FileSystemDeviceObject) {
        IoUnregisterFileSystem(UdfsData.FileSystemDeviceObject);
        IoDeleteDevice(UdfsData.FileSystemDeviceObject);
        UDFS_DEBUG_INFO_ONCE("File system device object deleted\n");
    }
    
    /* Clean up UDFCT subsystem */
    UdfsCleanupUdfctSubsystem();
    
    /* Clean up global resources */
    ExDeleteResourceLite(&UdfsData.GlobalResource);
    
    /* Clean up debug system last */
    UdfsCleanupDebugSystem();
}

/*
 * Main dispatch routine
 */
NTSTATUS
NTAPI
UdfsDispatch(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
    PIO_STACK_LOCATION IrpSp;
    NTSTATUS Status;
    
    IrpSp = IoGetCurrentIrpStackLocation(Irp);
    
    UDFS_DEBUG_INFO_ONCE("Main dispatch routine entered for IRP major function 0x%02X\n", 
                         IrpSp->MajorFunction);
    
    /* Check if this is the file system device object */
    if (DeviceObject == UdfsData.FileSystemDeviceObject) {
        UDFS_DEBUG_FSCTRL_ONCE("Request directed to file system device object\n");
        /* Only file system control is allowed on the FS device */
        if (IrpSp->MajorFunction == IRP_MJ_FILE_SYSTEM_CONTROL) {
            Status = UdfsFileSystemControl(DeviceObject, Irp);
        } else {
            UDFS_DEBUG_ERROR_ONCE("Invalid device request 0x%02X on FS device object\n", 
                                  IrpSp->MajorFunction);
            Status = STATUS_INVALID_DEVICE_REQUEST;
            Irp->IoStatus.Status = Status;
            Irp->IoStatus.Information = 0;
            IoCompleteRequest(Irp, IO_NO_INCREMENT);
        }
        return Status;
    }
    
    /* Dispatch to appropriate handler based on major function */
    switch (IrpSp->MajorFunction) {
        case IRP_MJ_CREATE:
            Status = UdfsCreate(DeviceObject, Irp);
            break;
            
        case IRP_MJ_CLOSE:
            Status = UdfsClose(DeviceObject, Irp);
            break;
            
        case IRP_MJ_READ:
            Status = UdfsRead(DeviceObject, Irp);
            break;
            
        case IRP_MJ_WRITE:
            Status = UdfsWrite(DeviceObject, Irp);
            break;
            
        case IRP_MJ_QUERY_INFORMATION:
            Status = UdfsQueryInformation(DeviceObject, Irp);
            break;
            
        case IRP_MJ_SET_INFORMATION:
            Status = UdfsSetInformation(DeviceObject, Irp);
            break;
            
        case IRP_MJ_QUERY_VOLUME_INFORMATION:
            Status = UdfsQueryVolumeInformation(DeviceObject, Irp);
            break;
            
        case IRP_MJ_DIRECTORY_CONTROL:
            Status = UdfsDirectoryControl(DeviceObject, Irp);
            break;
            
        case IRP_MJ_FILE_SYSTEM_CONTROL:
            Status = UdfsFileSystemControl(DeviceObject, Irp);
            break;
            
        case IRP_MJ_CLEANUP:
            Status = UdfsCleanup(DeviceObject, Irp);
            break;
            
        default:
            Status = STATUS_INVALID_DEVICE_REQUEST;
            Irp->IoStatus.Status = Status;
            Irp->IoStatus.Information = 0;
            IoCompleteRequest(Irp, IO_NO_INCREMENT);
            break;
    }
    
    return Status;
}

/*
 * Create VCB (Volume Control Block)
 */
NTSTATUS
UdfsCreateVcb(
    IN PDEVICE_OBJECT TargetDevice,
    IN PVPB Vpb,
    OUT PUDFS_VCB *Vcb
    )
{
    NTSTATUS Status;
    PUDFS_VCB NewVcb;
    PDEVICE_OBJECT VolumeDevice;
    
    /* Create volume device object */
    Status = IoCreateDevice(
        UdfsData.DriverObject,
        sizeof(UDFS_VCB),
        NULL,
        FILE_DEVICE_DISK_FILE_SYSTEM,
        FILE_DEVICE_SECURE_OPEN,
        FALSE,
        &VolumeDevice
        );
        
    if (!NT_SUCCESS(Status)) {
        return Status;
    }
    
    /* Initialize VCB */
    NewVcb = (PUDFS_VCB)VolumeDevice->DeviceExtension;
    RtlZeroMemory(NewVcb, sizeof(UDFS_VCB));
    
    NewVcb->DeviceObject = VolumeDevice;
    NewVcb->TargetDeviceObject = TargetDevice;
    NewVcb->Vpb = Vpb;
    NewVcb->IsReadOnly = TRUE;  /* UDF driver is read-only */
    
    /* Initialize resources */
    ExInitializeResourceLite(&NewVcb->VcbResource);
    ExInitializeResourceLite(&NewVcb->FcbResource);
    ExInitializeFastMutex(&NewVcb->AdvancedFcbHeaderMutex);
    
    /* Initialize FCB list */
    InitializeListHead(&NewVcb->FcbList);
    
    /* Set up FCB header */
    FsRtlSetupAdvancedHeader(&NewVcb->Header, &NewVcb->AdvancedFcbHeaderMutex);
    
    /* Initialize UDFCT context */
    Status = UdfsInitializeUdfct(TargetDevice, &NewVcb->MountContext);
    if (!NT_SUCCESS(Status)) {
        ExDeleteResourceLite(&NewVcb->VcbResource);
        ExDeleteResourceLite(&NewVcb->FcbResource);
        IoDeleteDevice(VolumeDevice);
        return Status;
    }
    
    /* Update VPB */
    Vpb->DeviceObject = VolumeDevice;
    Vpb->Flags |= VPB_MOUNTED;
    
    /* Set device flags */
    VolumeDevice->Flags &= ~DO_DEVICE_INITIALIZING;
    VolumeDevice->Flags |= DO_DIRECT_IO;
    
    *Vcb = NewVcb;
    return STATUS_SUCCESS;
}

/*
 * Delete VCB
 */
VOID
UdfsDeleteVcb(
    IN PUDFS_VCB Vcb
    )
{
    /* Clean up UDFCT context */
    if (Vcb->MountContext) {
        UdfsCleanupUdfct(Vcb->MountContext);
    }
    
    /* Clean up resources */
    ExDeleteResourceLite(&Vcb->VcbResource);
    ExDeleteResourceLite(&Vcb->FcbResource);
    
    /* Delete device object */
    IoDeleteDevice(Vcb->DeviceObject);
}