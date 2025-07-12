/*
 * UDFS - UDF File System Driver for Windows/ReactOS
 * Cleanup operations implementation
 */

#include "udfsprocs.h"

NTSTATUS
UdfsCleanup(
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
    
    UDFS_DEBUG_CLEANUP_ONCE("Cleanup request for %s\n", 
                           (Fcb && (Fcb->Flags & UDFS_FCB_DIRECTORY)) ? "directory" : "file");
    
    UNREFERENCED_PARAMETER(Ccb);
    
    /* Clear file object pointers */
    FileObject->FsContext = NULL;
    FileObject->FsContext2 = NULL;
    
    /* For read-only file system, cleanup is minimal */
    /* Just ensure any cached data is released */
    
    if (Fcb) {
        /* Decrement reference count */
        InterlockedDecrement(&Fcb->ReferenceCount);
        
        /* If this was the last reference and FCB is not cached, delete it */
        if (Fcb->ReferenceCount == 0) {
            /* In a real implementation, we might keep FCBs cached */
            /* For simplicity, we'll delete them immediately */
        }
    }
    
    /* CCB cleanup is handled in Close */
    
    Irp->IoStatus.Status = STATUS_SUCCESS;
    Irp->IoStatus.Information = 0;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    
    return STATUS_SUCCESS;
}