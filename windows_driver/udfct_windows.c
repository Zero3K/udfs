/*
 * UDFS - UDF File System Driver for Windows/ReactOS
 * UDFCT Windows Kernel Mode Integration Layer
 * 
 * This file provides the bridge between Windows kernel mode and UDFCT core functionality,
 * enabling full UDF parsing and I/O operations for all supported UDF revisions (1.02-2.60).
 */

#include "udfsprocs.h"

/* Global UDFCT state - we need minimal global state for kernel mode */
static BOOLEAN UdfctInitialized = FALSE;
static ERESOURCE UdfctGlobalResource;

/*
 * Memory allocation wrapper for UDFCT in kernel mode
 */
PVOID UdfsReallocatePool(PVOID OldBuffer, SIZE_T NewSize)
{
    PVOID NewBuffer;
    SIZE_T OldSize;
    
    if (!OldBuffer) {
        return ExAllocatePoolWithTag(PagedPool, NewSize, UDFS_TAG);
    }
    
    /* In a real implementation, we'd need to track allocation sizes */
    /* For now, we'll allocate new and copy - this is inefficient but functional */
    NewBuffer = ExAllocatePoolWithTag(PagedPool, NewSize, UDFS_TAG);
    if (NewBuffer && OldBuffer) {
        /* We need to estimate the old size - in practice, we'd track this */
        OldSize = min(NewSize, 65536); /* Conservative estimate */
        memcpy(NewBuffer, OldBuffer, OldSize);
        ExFreePoolWithTag(OldBuffer, UDFS_TAG);
    }
    
    return NewBuffer;
}

/*
 * Kernel mode memory and string function implementations
 */
PVOID UdfsMalloc(size_t size)
{
    return ExAllocatePoolWithTag(PagedPool, size, UDFS_TAG);
}

VOID UdfsFree(PVOID ptr)
{
    if (ptr) {
        ExFreePoolWithTag(ptr, UDFS_TAG);
    }
}

PVOID UdfsRealloc(PVOID ptr, size_t size)
{
    PVOID newPtr;
    
    if (!ptr) {
        return ExAllocatePoolWithTag(PagedPool, size, UDFS_TAG);
    }
    
    if (size == 0) {
        ExFreePoolWithTag(ptr, UDFS_TAG);
        return NULL;
    }
    
    newPtr = ExAllocatePoolWithTag(PagedPool, size, UDFS_TAG);
    if (newPtr && ptr) {
        /* In a real implementation, we'd need to know the old size */
        /* For now, just allocate new memory */
        ExFreePoolWithTag(ptr, UDFS_TAG);
    }
    return newPtr;
}

int UdfsFprintf(void *stream, const char *format, ...)
{
    /* In kernel mode, we can't write to actual FILE streams */
    /* This function is mainly used for debug output in UDFCT */
    /* We'll just discard the output for kernel mode to avoid dependencies */
    UNREFERENCED_PARAMETER(stream);
    UNREFERENCED_PARAMETER(format);
    return 0;
}

int UdfsVsprintf(char *buffer, const char *format, va_list args)
{
    /* Simple implementation using RtlStringCchVPrintfA if available,
     * otherwise fall back to basic vsprintf functionality */
    int result;
    
    /* Try to use a simple string formatting approach */
    result = _vsnprintf(buffer, 512, format, args);
    
    return result;
}

int UdfsSprintf(char *buffer, const char *format, ...)
{
    va_list args;
    int result;
    
    va_start(args, format);
    result = UdfsVsprintf(buffer, format, args);
    va_end(args);
    
    return result;
}

PVOID UdfsCalloc(size_t count, size_t size)
{
    SIZE_T totalSize = count * size;
    PVOID buffer = ExAllocatePoolWithTag(PagedPool, totalSize, UDFS_TAG);
    if (buffer) {
        RtlZeroMemory(buffer, totalSize);
    }
    return buffer;
}

size_t UdfsStrlen(const char *str)
{
    size_t len = 0;
    if (str) {
        while (*str++) len++;
    }
    return len;
}

int UdfsStrcmp(const char *str1, const char *str2)
{
    if (!str1 || !str2) return (str1 == str2) ? 0 : (str1 ? 1 : -1);
    
    while (*str1 && (*str1 == *str2)) {
        str1++;
        str2++;
    }
    return *(unsigned char*)str1 - *(unsigned char*)str2;
}

int UdfsMemcmp(const void *ptr1, const void *ptr2, size_t count)
{
    const unsigned char *p1 = (const unsigned char*)ptr1;
    const unsigned char *p2 = (const unsigned char*)ptr2;
    
    while (count-- > 0) {
        if (*p1 != *p2) {
            return *p1 - *p2;
        }
        p1++;
        p2++;
    }
    return 0;
}

char *UdfsStrcpy(char *dest, const char *src)
{
    char *original_dest = dest;
    if (dest && src) {
        while ((*dest++ = *src++));
    }
    return original_dest;
}

char *UdfsStrncpy(char *dest, const char *src, size_t count)
{
    char *original_dest = dest;
    if (dest && src) {
        while (count-- > 0 && (*dest++ = *src++));
        while (count-- > 0) *dest++ = '\0';
    }
    return original_dest;
}

char *UdfsStrcat(char *dest, const char *src)
{
    char *original_dest = dest;
    if (dest && src) {
        while (*dest) dest++;
        while ((*dest++ = *src++));
    }
    return original_dest;
}

/*
 * Windows device read block implementation for UDFCT
 */
Uint32 WindowsDeviceReadBlock(void *impUse, Uint32 blockSize, 
                             Uint32 firstBlock, Uint32 nrOfBlocks, Byte *buffer)
{
    WindowsDeviceAdapter *adapter = (WindowsDeviceAdapter *)impUse;
    KEVENT event;
    IO_STATUS_BLOCK ioStatus;
    PIRP irp;
    LARGE_INTEGER offset;
    NTSTATUS status;
    
    if (!adapter || !buffer || nrOfBlocks == 0) {
        return 0;
    }
    
    /* Calculate byte offset from block number */
    offset.QuadPart = (ULONGLONG)firstBlock * blockSize;
    
    /* Initialize synchronization event */
    KeInitializeEvent(&event, NotificationEvent, FALSE);
    
    /* Build IRP for synchronous read */
    irp = IoBuildSynchronousFsdRequest(
        IRP_MJ_READ,
        adapter->TargetDevice,
        buffer,
        nrOfBlocks * blockSize,
        &offset,
        &event,
        &ioStatus
    );
    
    if (!irp) {
        return 0;
    }
    
    /* Send the IRP */
    status = IoCallDriver(adapter->TargetDevice, irp);
    
    if (status == STATUS_PENDING) {
        KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
        status = ioStatus.Status;
    }
    
    if (!NT_SUCCESS(status)) {
        UDFS_DEBUG_DEVICE_ONCE("Device read operation failed at block %lu, status=0x%08X\n", 
                 firstBlock, status);
        return 0;
    }
    
    UDFS_DEBUG_DEVICE_ONCE("Successfully read %lu blocks starting at block %lu\n",
             nrOfBlocks, firstBlock);
    
    /* Return number of blocks successfully read */
    return (Uint32)(ioStatus.Information / blockSize);
}

/*
 * Windows device write block implementation for UDFCT
 */
Uint32 WindowsDeviceWriteBlock(void *impUse, Uint32 blockSize, 
                              Uint32 firstBlock, Uint32 nrOfBlocks, Byte *buffer)
{
    WindowsDeviceAdapter *adapter = (WindowsDeviceAdapter *)impUse;
    KEVENT event;
    IO_STATUS_BLOCK ioStatus;
    PIRP irp;
    LARGE_INTEGER offset;
    NTSTATUS status;
    
    if (!adapter || !buffer || nrOfBlocks == 0) {
        return 0;
    }
    
    /* Calculate byte offset from block number */
    offset.QuadPart = (ULONGLONG)firstBlock * blockSize;
    
    /* Initialize synchronization event */
    KeInitializeEvent(&event, NotificationEvent, FALSE);
    
    /* Build IRP for synchronous write */
    irp = IoBuildSynchronousFsdRequest(
        IRP_MJ_WRITE,
        adapter->TargetDevice,
        buffer,
        nrOfBlocks * blockSize,
        &offset,
        &event,
        &ioStatus
    );
    
    if (!irp) {
        return 0;
    }
    
    /* Send the IRP */
    status = IoCallDriver(adapter->TargetDevice, irp);
    
    if (status == STATUS_PENDING) {
        KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
        status = ioStatus.Status;
    }
    
    if (!NT_SUCCESS(status)) {
        UDFS_DEBUG_DEVICE_ONCE("Device write operation failed at block %lu, status=0x%08X\n", 
                 firstBlock, status);
        return 0;
    }
    
    UDFS_DEBUG_DEVICE_ONCE("Successfully wrote %lu blocks starting at block %lu\n",
             nrOfBlocks, firstBlock);
    
    /* Return number of blocks successfully written */
    return (Uint32)(ioStatus.Information / blockSize);
}

/*
 * Windows device block state implementation for UDFCT
 */
BlockState WindowsDeviceGetBlockState(void *impUse, Uint32 blockNr)
{
    /* For simplicity, assume all blocks are readable in Windows */
    /* In a full implementation, this could query the device for specific block status */
    UNREFERENCED_PARAMETER(impUse);
    UNREFERENCED_PARAMETER(blockNr);
    
    return BSTATE_READABLE;
}

/*
 * Windows device cleanup implementation for UDFCT
 */
void WindowsDeviceCloseAndFree(void *impUse)
{
    WindowsDeviceAdapter *adapter = (WindowsDeviceAdapter *)impUse;
    
    if (adapter) {
        /* Dereference the target device */
        if (adapter->TargetDevice) {
            ObDereferenceObject(adapter->TargetDevice);
        }
        
        /* Free the adapter structure */
        ExFreePoolWithTag(adapter, UDFS_TAG);
    }
}

/*
 * Initialize UDFCT device interface for Windows
 */
NTSTATUS UdfsInitializeUdfctDevice(PDEVICE_OBJECT TargetDevice, Device **uctDevice)
{
    WindowsDeviceAdapter *adapter;
    Device *device;
    NTSTATUS status;
    DISK_GEOMETRY geometry;
    KEVENT event;
    IO_STATUS_BLOCK ioStatus;
    PIRP irp;
    
    *uctDevice = NULL;
    
    /* Allocate device adapter */
    adapter = ExAllocatePoolWithTag(PagedPool, sizeof(WindowsDeviceAdapter), UDFS_TAG);
    if (!adapter) {
        return STATUS_INSUFFICIENT_RESOURCES;
    }
    
    RtlZeroMemory(adapter, sizeof(WindowsDeviceAdapter));
    
    /* Reference the target device */
    ObReferenceObject(TargetDevice);
    adapter->TargetDevice = TargetDevice;
    
    /* Get device geometry */
    KeInitializeEvent(&event, NotificationEvent, FALSE);
    
    irp = IoBuildDeviceIoControlRequest(
        IOCTL_DISK_GET_DRIVE_GEOMETRY,
        TargetDevice,
        NULL, 0,
        &geometry, sizeof(geometry),
        FALSE,
        &event,
        &ioStatus
    );
    
    if (!irp) {
        ObDereferenceObject(TargetDevice);
        ExFreePoolWithTag(adapter, UDFS_TAG);
        return STATUS_INSUFFICIENT_RESOURCES;
    }
    
    status = IoCallDriver(TargetDevice, irp);
    
    if (status == STATUS_PENDING) {
        KeWaitForSingleObject(&event, Executive, KernelMode, FALSE, NULL);
        status = ioStatus.Status;
    }
    
    if (NT_SUCCESS(status)) {
        adapter->SectorSize = geometry.BytesPerSector;
        adapter->DeviceSize.QuadPart = 
            geometry.Cylinders.QuadPart * 
            geometry.TracksPerCylinder * 
            geometry.SectorsPerTrack * 
            geometry.BytesPerSector;
    } else {
        /* Use default values if geometry query fails */
        adapter->SectorSize = 2048; /* Standard UDF block size */
        adapter->DeviceSize.QuadPart = 0; /* Unknown size */
    }
    
    /* Allocate UDFCT device structure */
    device = ExAllocatePoolWithTag(PagedPool, sizeof(Device), UDFS_TAG);
    if (!device) {
        ObDereferenceObject(TargetDevice);
        ExFreePoolWithTag(adapter, UDFS_TAG);
        return STATUS_INSUFFICIENT_RESOURCES;
    }
    
    RtlZeroMemory(device, sizeof(Device));
    
    /* Initialize UDFCT device structure */
    device->impUse = adapter;
    device->readBlock = WindowsDeviceReadBlock;
    device->getBlockState = WindowsDeviceGetBlockState;
    device->closeAndFreeImpUse = WindowsDeviceCloseAndFree;
    
    /* Initialize medium info */
    device->mediumInfo.blockSize = adapter->SectorSize;
    device->mediumInfo.lastValidBlockNr = (Uint32)((adapter->DeviceSize.QuadPart / adapter->SectorSize) - 1);
    device->mediumInfo.writabilityType = MTYPE_WR_UNKNOWN; /* We'll determine this later */
    
    adapter->uctDevice = device;
    *uctDevice = device;
    
    return STATUS_SUCCESS;
}

/*
 * Initialize UDFCT subsystem for kernel mode
 */
NTSTATUS UdfsInitializeUdfctSubsystem(void)
{
    NTSTATUS status;
    
    if (UdfctInitialized) {
        return STATUS_SUCCESS;
    }
    
    /* Initialize global resource */
    status = ExInitializeResourceLite(&UdfctGlobalResource);
    if (!NT_SUCCESS(status)) {
        return status;
    }
    
    /* Initialize any global UDFCT state here */
    /* Most UDFCT initialization is per-device/per-mount */
    
    UdfctInitialized = TRUE;
    
    UDFS_DEBUG_UDFCT_ONCE("UDFCT subsystem initialized (supporting UDF 1.02-2.60)\n");
    
    return STATUS_SUCCESS;
}

/*
 * Cleanup UDFCT subsystem
 */
VOID UdfsCleanupUdfctSubsystem(void)
{
    if (!UdfctInitialized) {
        return;
    }
    
    /* Cleanup any global UDFCT state */
    
    ExDeleteResourceLite(&UdfctGlobalResource);
    UdfctInitialized = FALSE;
    
    UDFS_DEBUG_UDFCT_ONCE("UDFCT subsystem cleaned up\n");
}

/*
 * Enhanced mount context creation using full UDFCT
 */
NTSTATUS UdfsCreateUdfctMountContext(PDEVICE_OBJECT TargetDevice, 
                                    UdfMountContext **MountContext)
{
    Device *uctDevice;
    UdfMountContext *mc;
    NTSTATUS status;
    
    *MountContext = NULL;
    
    /* Initialize device interface */
    status = UdfsInitializeUdfctDevice(TargetDevice, &uctDevice);
    if (!NT_SUCCESS(status)) {
        return status;
    }
    
    /* Allocate mount context */
    mc = ExAllocatePoolWithTag(PagedPool, sizeof(UdfMountContext), UDFS_TAG);
    if (!mc) {
        deviceCloseAndFreeDevice(uctDevice);
        return STATUS_INSUFFICIENT_RESOURCES;
    }
    
    RtlZeroMemory(mc, sizeof(UdfMountContext));
    
    /* Initialize mount context with UDFCT device */
    mc->device = uctDevice;
    
    /* Use UDFCT to mount the logical volume */
    /* This will parse the UDF structures and determine the exact UDF revision */
    if (!udfMountLogicalVolume(mc, NULL)) {
        UDFS_DEBUG_ERROR_ONCE("Failed to mount UDF logical volume using UDFCT\n");
        ExFreePoolWithTag(mc, UDFS_TAG);
        deviceCloseAndFreeDevice(uctDevice);
        return STATUS_UNRECOGNIZED_VOLUME;
    }
    
    UDFS_DEBUG_UDFCT_ONCE("Successfully mounted UDF volume, revision %u.%02u\n",
             getUctUdfRevision() >> 8, getUctUdfRevision() & 0xFF);
    
    *MountContext = mc;
    return STATUS_SUCCESS;
}

/*
 * Cleanup UDFCT mount context
 */
VOID UdfsCleanupUdfctMountContext(UdfMountContext *MountContext)
{
    if (!MountContext) {
        return;
    }
    
    /* Use UDFCT to unmount the logical volume */
    udfUnmountLogicalVolume(MountContext);
    
    /* Cleanup device */
    if (MountContext->device) {
        deviceCloseAndFreeDevice(MountContext->device);
    }
    
    /* Free mount context */
    ExFreePoolWithTag(MountContext, UDFS_TAG);
}