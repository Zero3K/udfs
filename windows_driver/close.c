/*
 * UDFS - UDF File System Driver for Windows/ReactOS
 * Close operations implementation
 */

#include "udfsprocs.h"

NTSTATUS
UdfsClose(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
    PIO_STACK_LOCATION IrpSp;
    PFILE_OBJECT FileObject;
    PUDFS_FCB Fcb;
    PUDFS_CCB Ccb;
    
    UNREFERENCED_PARAMETER(DeviceObject);
    
    IrpSp = IoGetCurrentIrpStackLocation(Irp);
    FileObject = IrpSp->FileObject;
    
    Fcb = (PUDFS_FCB)FileObject->FsContext;
    Ccb = (PUDFS_CCB)FileObject->FsContext2;
    
    /* Delete CCB if it exists */
    if (Ccb) {
        UdfsDeleteCcb(Ccb);
    }
    
    /* Handle FCB reference counting */
    if (Fcb) {
        InterlockedDecrement(&Fcb->ReferenceCount);
        
        /* If reference count reaches zero, we could delete the FCB */
        /* For now, we'll let cleanup handle this */
        if (Fcb->ReferenceCount == 0) {
            /* Delete FCB if no more references */
            UdfsDeleteFcb(Fcb);
        }
    }
    
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    
    return STATUS_SUCCESS;
}