# UDF File System Driver for Windows/ReactOS

This directory contains a Windows kernel mode UDF file system driver implementation that can be compiled using ReactOS' build environment.

## Overview

This Windows UDF driver is based on the existing Linux/Unix implementation in the `udfs_driver` directory, which itself is based on the UDFCT (UDF Conformance Testing Application) from Philips. The driver provides read-only access to UDF (Universal Disk Format) file systems on Windows and ReactOS.

## Features

- **Read-only UDF support**: Mount and read files from UDF volumes
- **Windows kernel mode**: Proper Windows file system driver implementation
- **ReactOS compatibility**: Builds with ReactOS build environment
- **Based on UDFCT**: Uses proven UDF parsing code from Philips UDFCT
- **Standard Windows FS interface**: Supports all standard Windows file operations

## Architecture

```
┌─────────────────────┐
│   Windows Apps      │
├─────────────────────┤
│   Windows I/O Mgr   │
├─────────────────────┤
│   UDFS Driver       │  ← This implementation
├─────────────────────┤
│   UDFCT Core        │  ← Adapted for kernel mode
├─────────────────────┤
│   Storage Stack     │
└─────────────────────┘
```

## File Structure

- **CMakeLists.txt** - ReactOS build system integration
- **udfsprocs.h** - Main driver header with Windows types and function prototypes
- **udfs.c** - Main driver implementation (DriverEntry, dispatch routines)
- **fsctrl.c** - File system control (mount/dismount operations)
- **create.c** - File/directory creation and opening
- **read.c** - Read operations
- **dirctrl.c** - Directory control and enumeration
- **fileinfo.c** - File information queries
- **volinfo.c** - Volume information queries
- **cleanup.c** - File cleanup operations
- **close.c** - File close operations
- **udfs.rc** - Resource file for version information

## Key Components

### Driver Entry Points

The driver implements standard Windows file system driver entry points:

- `IRP_MJ_CREATE` - File/directory open operations
- `IRP_MJ_READ` - Read file data
- `IRP_MJ_QUERY_INFORMATION` - Get file information
- `IRP_MJ_QUERY_VOLUME_INFORMATION` - Get volume information
- `IRP_MJ_DIRECTORY_CONTROL` - Directory enumeration
- `IRP_MJ_FILE_SYSTEM_CONTROL` - Mount/dismount operations
- `IRP_MJ_CLEANUP` - File cleanup
- `IRP_MJ_CLOSE` - File close

### UDFCT Integration

The driver adapts the UDFCT code for Windows kernel mode:

- Memory allocation using `ExAllocatePoolWithTag`
- I/O operations using Windows IRPs
- Synchronization using Windows kernel resources
- Error handling using NTSTATUS codes

### Data Structures

- **UDFS_VCB** - Volume Control Block for mounted volumes
- **UDFS_FCB** - File Control Block for open files
- **UDFS_CCB** - Context Control Block for file handles

## Building with ReactOS

### Prerequisites

- ReactOS source code
- ReactOS build environment (RosBE)
- CMake

### Integration Steps

1. Copy this directory to `drivers/filesystems/udfs` in ReactOS source
2. Add to `drivers/filesystems/CMakeLists.txt`:
   ```cmake
   add_subdirectory(udfs)
   ```
3. Build ReactOS as normal

### Build Commands

```bash
# From ReactOS source root
cd drivers/filesystems/udfs
mkdir build
cd build
cmake ..
make
```

## Current Implementation Status

### Completed Features

- [x] Basic Windows driver framework
- [x] Driver registration and unload
- [x] Volume mounting and recognition
- [x] File/directory open operations
- [x] Basic read operations
- [x] Directory enumeration
- [x] File and volume information queries
- [x] Proper cleanup and close handling

### Limitations

- **UDFCT Integration**: Currently uses simplified stubs instead of full UDFCT integration
- **Read Implementation**: Basic read operations implemented, full UDF block mapping needed
- **Performance**: No caching or optimization implemented yet
- **Error Handling**: Basic error handling, needs comprehensive error cases

### Future Enhancements

1. **Complete UDFCT Integration**: Fully integrate UDFCT code for UDF parsing
2. **Advanced Features**: Extended attributes, multi-session support
3. **Performance**: Add caching and read-ahead
4. **Testing**: Comprehensive testing with various UDF volumes

## UDFCT Adaptation Notes

The UDFCT code required several adaptations for Windows kernel mode:

1. **Memory Management**: Replace malloc/free with kernel pool allocation
2. **I/O Operations**: Replace file operations with device I/O
3. **Error Handling**: Convert return codes to NTSTATUS
4. **Threading**: Adapt to Windows kernel synchronization

## Testing

To test the driver:

1. Build and install in ReactOS/Windows
2. Mount a UDF volume (CD/DVD/Blu-ray or image file)
3. Verify volume appears in file manager
4. Test file reading and directory enumeration

## License

This implementation maintains the licensing terms of the original UDFCT code. See the main LICENSE.TXT file for details.

## Contributing

Contributions should focus on:

- Completing UDFCT integration
- Adding proper UDF block mapping
- Performance improvements
- Testing with various UDF volumes

## References

- [ReactOS FastFAT driver](https://github.com/reactos/reactos/tree/master/drivers/filesystems/fastfat) - Reference implementation
- [UDFCT source](../udfct/) - Base UDF parsing implementation
- [UDF Specification](https://www.ecma-international.org/publications-and-standards/standards/ecma-167/) - UDF standard documentation