/*
 * UDFS - UDF File System Driver for Windows/ReactOS
 * Read operations implementation
 */

#include "udfsprocs.h"

NTSTATUS
UdfsRead(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp
    )
{
    PIO_STACK_LOCATION IrpSp;
    PFILE_OBJECT FileObject;
    PUDFS_FCB Fcb;
    PUDFS_CCB Ccb;
    NTSTATUS Status;
    LARGE_INTEGER FileOffset;
    ULONG Length;
    PVOID Buffer;
    
    UNREFERENCED_PARAMETER(DeviceObject);
    
    IrpSp = IoGetCurrentIrpStackLocation(Irp);
    FileObject = IrpSp->FileObject;
    
    Fcb = (PUDFS_FCB)FileObject->FsContext;
    Ccb = (PUDFS_CCB)FileObject->FsContext2;
    
    /* Get read parameters */
    Length = IrpSp->Parameters.Read.Length;
    FileOffset = IrpSp->Parameters.Read.ByteOffset;
    
    /* Use file object's current position if not specified */
    if (FileOffset.LowPart == FILE_USE_FILE_POINTER_POSITION &&
        FileOffset.HighPart == -1) {
        FileOffset = FileObject->CurrentByteOffset;
    }
    
    /* Check if this is a directory */
    if (Fcb->Flags & UDFS_FCB_DIRECTORY) {
        Status = STATUS_INVALID_PARAMETER;
        goto complete;
    }
    
    /* Check for zero length read */
    if (Length == 0) {
        Status = STATUS_SUCCESS;
        Irp->IoStatus.Information = 0;
        goto complete;
    }
    
    /* Check if read starts beyond end of file */
    if (FileOffset.QuadPart >= Fcb->FileSize.QuadPart) {
        Status = STATUS_END_OF_FILE;
        Irp->IoStatus.Information = 0;
        goto complete;
    }
    
    /* Adjust length if reading past end of file */
    if (FileOffset.QuadPart + Length > Fcb->FileSize.QuadPart) {
        Length = (ULONG)(Fcb->FileSize.QuadPart - FileOffset.QuadPart);
    }
    
    /* Get system buffer */
    if (Irp->MdlAddress) {
        Buffer = MmGetSystemAddressForMdlSafe(Irp->MdlAddress, NormalPagePriority);
        if (!Buffer) {
            Status = STATUS_INSUFFICIENT_RESOURCES;
            goto complete;
        }
    } else {
        Buffer = Irp->UserBuffer;
    }
    
    if (!Buffer) {
        Status = STATUS_INVALID_PARAMETER;
        goto complete;
    }
    
    /* Perform the read operation */
    Status = UdfsReadFileData(Fcb, FileOffset, Length, Buffer);
    
    if (NT_SUCCESS(Status)) {
        /* Update file position and information */
        FileObject->CurrentByteOffset.QuadPart = FileOffset.QuadPart + Length;
        Ccb->FileOffset = FileObject->CurrentByteOffset;
        Irp->IoStatus.Information = Length;
    } else {
        Irp->IoStatus.Information = 0;
    }
    
complete:
    Irp->IoStatus.Status = Status;
    IoCompleteRequest(Irp, IO_NO_INCREMENT);
    return Status;
}

NTSTATUS
UdfsReadFileData(
    IN PUDFS_FCB Fcb,
    IN LARGE_INTEGER FileOffset,
    IN ULONG Length,
    OUT PVOID Buffer
    )
{
    NTSTATUS Status;
    UdfMountContext *mc;
    Node *fileNode;
    UdfAllocationList *allocList;
    UdfAllocationItem *allocItem;
    ULONG bytesRead = 0;
    ULONG currentOffset;
    ULONG remainingBytes;
    PUCHAR outputBuffer;
    PUDFS_VCB vcb;
    
    if (!Fcb || !Fcb->UdfNode || !Buffer) {
        return STATUS_INVALID_PARAMETER;
    }
    
    vcb = (PUDFS_VCB)Fcb->Header.Vcb;
    mc = vcb->MountContext;
    fileNode = Fcb->UdfNode;
    
    /* Use UDFCT to get the file's allocation list */
    allocList = &fileNode->al;
    if (!allocList) {
        return STATUS_FILE_INVALID;
    }
    
    currentOffset = FileOffset.LowPart;
    remainingBytes = Length;
    outputBuffer = (PUCHAR)Buffer;
    
    __try {
        /* Walk through the allocation descriptors to read file data */
        allocItem = allocList->head;
        
        while (allocItem && remainingBytes > 0) {
            AnyAllocationDescriptor *aad = &allocItem->aad;
            Uint32 extentLength = aad->anyAd.extentLength & UDF_EXTENT_LENGTH_MASK;
            Uint32 extentType = (aad->anyAd.extentLength >> 30) & 0x3;
            Uint32 logicalBlockNr;
            Uint16 partRefNumber;
            ULONG bytesToRead;
            ULONG blockOffset;
            ULONG blockSize = mc->device->mediumInfo.blockSize;
            PVOID blockBuffer;
            
            /* Skip if this is not a recorded extent */
            if (extentType != ADEL_RECORDED_AND_ALLOCATED) {
                allocItem = allocItem->next;
                continue;
            }
            
            /* Get the location of this extent */
            if (!udfGetLocation(aad, allocList->adType, 
                               allocList->shortPartRefNumber,
                               &partRefNumber, &logicalBlockNr)) {
                allocItem = allocItem->next;
                continue;
            }
            
            /* Check if our read offset falls within this extent */
            if (currentOffset >= extentLength) {
                currentOffset -= extentLength;
                allocItem = allocItem->next;
                continue;
            }
            
            /* Calculate how much to read from this extent */
            bytesToRead = min(remainingBytes, extentLength - currentOffset);
            blockOffset = currentOffset % blockSize;
            
            /* Allocate temporary buffer for block-aligned reads */
            blockBuffer = ExAllocatePoolWithTag(PagedPool, blockSize, 'UDFS');
            if (!blockBuffer) {
                Status = STATUS_INSUFFICIENT_RESOURCES;
                break;
            }
            
            /* Read the block(s) containing our data */
            while (bytesToRead > 0 && remainingBytes > 0) {
                ULONG blockNumber = logicalBlockNr + (currentOffset / blockSize);
                ULONG bytesFromBlock = min(bytesToRead, blockSize - blockOffset);
                
                /* Read the block using UDFCT */
                if (readBlocksFromPartition(mc, blockBuffer, partRefNumber, 
                                           blockNumber, 1) != 1) {
                    ExFreePoolWithTag(blockBuffer, 'UDFS');
                    return STATUS_DEVICE_DATA_ERROR;
                }
                
                /* Copy the relevant portion to output buffer */
                RtlCopyMemory(outputBuffer, (PUCHAR)blockBuffer + blockOffset, bytesFromBlock);
                
                outputBuffer += bytesFromBlock;
                bytesRead += bytesFromBlock;
                bytesToRead -= bytesFromBlock;
                remainingBytes -= bytesFromBlock;
                currentOffset += bytesFromBlock;
                blockOffset = 0; /* Only first block may have offset */
            }
            
            ExFreePoolWithTag(blockBuffer, 'UDFS');
            currentOffset = 0; /* Reset for next extent */
            allocItem = allocItem->next;
        }
        
        Status = STATUS_SUCCESS;
        
    } __except(EXCEPTION_EXECUTE_HANDLER) {
        Status = STATUS_INVALID_USER_BUFFER;
    }
    
    return Status;
}

/*
 * Alternative read implementation using UDFCT structures
 * This would be used when UDFCT integration is complete
 */
NTSTATUS
UdfsReadFileDataFromUdfct(
    IN PUDFS_FCB Fcb,
    IN LARGE_INTEGER FileOffset,
    IN ULONG Length,
    OUT PVOID Buffer
    )
{
    NTSTATUS Status = STATUS_SUCCESS;
    Node *FileNode;
    ULONG BytesRead = 0;
    ULONG CurrentOffset;
    ULONG RemainingBytes;
    PUCHAR OutputBuffer;
    
    /* Get the UDFCT file node */
    FileNode = Fcb->UdfNode;
    if (!FileNode) {
        return STATUS_FILE_INVALID;
    }
    
    /* Initialize read state */
    CurrentOffset = FileOffset.LowPart;  /* Simplified - ignore high part */
    RemainingBytes = Length;
    OutputBuffer = (PUCHAR)Buffer;
    
    __try {
        while (RemainingBytes > 0 && CurrentOffset < Fcb->FileSize.LowPart) {
            ULONG ChunkSize;
            ULONG BlockSize = 2048;  /* Standard UDF block size */
            ULONG BlockNumber;
            ULONG BlockOffset;
            UCHAR BlockBuffer[2048];
            
            /* Calculate block and offset within block */
            BlockNumber = CurrentOffset / BlockSize;
            BlockOffset = CurrentOffset % BlockSize;
            
            /* Calculate how much to read from this block */
            ChunkSize = min(RemainingBytes, BlockSize - BlockOffset);
            ChunkSize = min(ChunkSize, Fcb->FileSize.LowPart - CurrentOffset);
            
            /* This is where we would use UDFCT to read the block */
            /* For now, just zero the data */
            RtlZeroMemory(BlockBuffer, sizeof(BlockBuffer));
            
            /* Copy the relevant portion to output buffer */
            RtlCopyMemory(OutputBuffer, BlockBuffer + BlockOffset, ChunkSize);
            
            /* Update counters */
            OutputBuffer += ChunkSize;
            CurrentOffset += ChunkSize;
            RemainingBytes -= ChunkSize;
            BytesRead += ChunkSize;
        }
        
    } __except(EXCEPTION_EXECUTE_HANDLER) {
        Status = STATUS_INVALID_USER_BUFFER;
    }
    
    return Status;
}