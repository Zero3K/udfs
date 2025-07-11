# UDFS - UDF File System Driver

A read-only UDF (Universal Disk Format) file system driver based on the UDFCT (UDF Conformance Testing Application) source code.

## Overview

This project creates a simple, focused file system driver for reading UDF volumes from the robust UDF parsing code in UDFCT. While UDFCT is designed as a verification and testing tool, UDFS extracts the essential UDF reading functionality and provides a clean C API for applications.

## Features

- **Read-only UDF access**: Mount and read files from UDF volumes
- **Multiple device types**: Support for image files and block devices (via UDFCT's device abstraction)
- **Clean C API**: Simple, POSIX-like interface for file system operations
- **Based on proven code**: Built on UDFCT's robust UDF parsing implementation
- **Cross-platform**: Supports Linux, Unix, and potentially other platforms supported by UDFCT
- **Extended file attributes**: Access UDF extended attributes including file times, device specs, and implementation-specific data
- **Unicode filename support**: Proper Unicode handling for international filenames using UDFCT's conversion functions
- **Multi-session support**: Access and query information about multiple UDF sessions on optical media
- **Performance optimizations**: Directory caching and read-ahead buffering for improved performance

## Current Status

**Phase 1 - Foundation (✅ COMPLETED)**
- [x] Project structure and build system
- [x] Basic API definition with core data structures
- [x] Integration with UDFCT libraries (uct_core, udf_scsi)
- [x] Compilation and basic testing framework
- [x] Error handling and debug infrastructure

**Phase 2 - Core Implementation (✅ COMPLETED)**
- [x] UDF volume mounting using UDFCT's mount logic
- [x] Basic file operations (open, read, seek, close)
- [x] Directory operations (open, read entries, close)
- [x] Path resolution and file/directory lookup
- [x] Volume information retrieval

**Phase 3 - Advanced Features (✅ COMPLETED)**
- [x] Extended file attributes support
- [x] Unicode filename handling
- [x] Multi-session UDF support
- [x] Performance optimizations
- [x] Additional device types

## API Reference

### Core Types

```c
typedef struct udfs_volume udfs_volume_t;    // Opaque volume handle
typedef struct udfs_file udfs_file_t;        // Opaque file handle
typedef struct udfs_dir udfs_dir_t;          // Opaque directory handle

typedef enum {
    UDFS_OK = 0,
    UDFS_ERROR_INVALID_PARAM = -1,
    UDFS_ERROR_NOT_UDF = -2,
    UDFS_ERROR_IO = -3,
    UDFS_ERROR_NO_MEMORY = -4,
    UDFS_ERROR_NOT_FOUND = -5,
    UDFS_ERROR_NOT_MOUNTED = -6,
    UDFS_ERROR_ALREADY_MOUNTED = -7,
    UDFS_ERROR_NOT_SUPPORTED = -8
} udfs_result_t;
```

### Volume Operations

```c
// Mount UDF volume from image file
udfs_result_t udfs_mount_image(const char *image_path, udfs_volume_t **volume);

// Mount UDF volume from device
udfs_result_t udfs_mount_device(const char *device_path, udfs_volume_t **volume);

// Unmount volume
udfs_result_t udfs_unmount(udfs_volume_t *volume);

// Get volume information
udfs_result_t udfs_get_volume_info(udfs_volume_t *volume, 
                                   char *label, size_t label_size,
                                   uint64_t *total_size, uint64_t *free_size);
```

### File Operations

```c
// Open file for reading
udfs_result_t udfs_open_file(udfs_volume_t *volume, const char *path, udfs_file_t **file);

// Read data from file
udfs_result_t udfs_read_file(udfs_file_t *file, void *buffer, size_t size, size_t *bytes_read);

// Seek in file
udfs_result_t udfs_seek_file(udfs_file_t *file, uint64_t offset);

// Close file
udfs_result_t udfs_close_file(udfs_file_t *file);
```

### Directory Operations

```c
// Open directory
udfs_result_t udfs_open_dir(udfs_volume_t *volume, const char *path, udfs_dir_t **dir);

// Read directory entry
udfs_result_t udfs_read_dir(udfs_dir_t *dir, udfs_dir_entry_t *entry);

// Close directory
udfs_result_t udfs_close_dir(udfs_dir_t *dir);
```

### Extended Attributes Operations (Phase 3)

```c
// Extended attribute types
typedef enum {
    UDFS_EA_CHARSET_INFO = 1,
    UDFS_EA_FILE_TIMES = 5,
    UDFS_EA_INFO_TIMES = 6,
    UDFS_EA_DEVICE_SPEC = 12,
    UDFS_EA_IMPL_USE = 2048,
    UDFS_EA_APP_USE = 65536
} udfs_ea_type_t;

// Extended attribute information
typedef struct {
    udfs_ea_type_t type;
    uint32_t length;
    char name[64];
    bool available;
} udfs_ea_info_t;

// Get list of available extended attributes for a file
udfs_result_t udfs_list_extended_attributes(udfs_file_t *file, udfs_ea_info_t *ea_list, 
                                           size_t max_count, size_t *actual_count);

// Read extended attribute data
udfs_result_t udfs_read_extended_attribute(udfs_file_t *file, udfs_ea_type_t ea_type,
                                          void *buffer, size_t buffer_size, size_t *data_size);

// Get extended attribute info by type
udfs_result_t udfs_get_extended_attribute_info(udfs_file_t *file, udfs_ea_type_t ea_type,
                                              udfs_ea_info_t *ea_info);
```

### Multi-Session Support (Phase 3)

```c
// Session information structure
typedef struct {
    uint32_t session_number;
    uint32_t start_block;
    uint32_t total_blocks;
    bool is_verify_session;
} udfs_session_info_t;

// Get number of sessions on the volume
udfs_result_t udfs_get_session_count(udfs_volume_t *volume, uint32_t *session_count);

// Get information about a specific session
udfs_result_t udfs_get_session_info(udfs_volume_t *volume, uint32_t session_index,
                                   udfs_session_info_t *session_info);

// Get information about all sessions
udfs_result_t udfs_list_sessions(udfs_volume_t *volume, udfs_session_info_t *session_list,
                                size_t max_sessions, size_t *actual_sessions);
```

## Building

### Prerequisites

- GCC compiler
- Make
- Linux development environment (other Unix systems may work)

### Build Steps

```bash
cd udfs_driver

# Build the library
make lib

# Build and run tests
make test
./udfs_test

# Build example program
make example
./udfs_example <udf_image_file>

# Clean build artifacts
make clean
```

## Usage Example

```c
#include "udfs.h"

int main() {
    udfs_volume_t *volume;
    udfs_result_t result;
    
    // Mount UDF image
    result = udfs_mount_image("example.iso", &volume);
    if (result != UDFS_OK) {
        printf("Failed to mount: %s\n", udfs_strerror(result));
        return 1;
    }
    
    // Get volume info
    char label[256];
    uint64_t total_size;
    udfs_get_volume_info(volume, label, sizeof(label), &total_size, NULL);
    printf("Volume: %s, Size: %llu bytes\n", label, total_size);
    
    // List root directory
    udfs_dir_t *dir;
    result = udfs_open_dir(volume, "/", &dir);
    if (result == UDFS_OK) {
        udfs_dir_entry_t entry;
        while (udfs_read_dir(dir, &entry) == UDFS_OK && entry.valid) {
            printf("%s (%s, %llu bytes)\n", entry.info.name,
                   entry.info.type == UDFS_TYPE_FILE ? "file" : "dir",
                   entry.info.size);
        }
        udfs_close_dir(dir);
    }
    
    // Read a file
    udfs_file_t *file;
    result = udfs_open_file(volume, "/README.TXT", &file);
    if (result == UDFS_OK) {
        char buffer[256];
        size_t bytes_read;
        udfs_read_file(file, buffer, sizeof(buffer), &bytes_read);
        printf("Read %zu bytes from README.TXT\n", bytes_read);
        
        // Phase 3: Check for extended attributes
        udfs_ea_info_t ea_list[10];
        size_t ea_count;
        result = udfs_list_extended_attributes(file, ea_list, 10, &ea_count);
        if (result == UDFS_OK && ea_count > 0) {
            printf("File has %zu extended attributes:\n", ea_count);
            for (size_t i = 0; i < ea_count; i++) {
                printf("  - %s (type=%d, length=%u)\n", 
                       ea_list[i].name, ea_list[i].type, ea_list[i].length);
            }
        }
        
        udfs_close_file(file);
    }
    
    // Phase 3: Check session information
    uint32_t session_count;
    result = udfs_get_session_count(volume, &session_count);
    if (result == UDFS_OK) {
        printf("Volume has %u sessions\n", session_count);
        
        udfs_session_info_t session_list[10];
        size_t actual_sessions;
        result = udfs_list_sessions(volume, session_list, 10, &actual_sessions);
        if (result == UDFS_OK) {
            for (size_t i = 0; i < actual_sessions; i++) {
                printf("  Session %u: %u blocks starting at %u%s\n",
                       session_list[i].session_number,
                       session_list[i].total_blocks,
                       session_list[i].start_block,
                       session_list[i].is_verify_session ? " (verify)" : "");
            }
        }
    }
    
    // Unmount
    udfs_unmount(volume);
    return 0;
}
```

## Architecture

UDFS is built as a thin wrapper around UDFCT's core functionality with Phase 3 enhancements:

```
┌─────────────────┐
│   Application   │
├─────────────────┤
│   UDFS API      │  ← Clean, simple interface with Phase 3 features
├─────────────────┤
│   UDFS Core     │  ← Caching, buffering, and Unicode handling
├─────────────────┤
│   UDFCT Core    │  ← Robust UDF parsing (uct_core)
├─────────────────┤
│ Device Layer    │  ← Device abstraction (udf_scsi, etc.)
└─────────────────┘
```

### Phase 3 Enhancements:
- **Extended Attributes**: Full access to UDF extended attributes with proper parsing
- **Unicode Support**: Proper Unicode filename handling using UDFCT's conversion functions
- **Multi-Session**: Complete multi-session UDF support with session enumeration
- **Performance**: Directory caching and 64KB read-ahead buffering for better I/O performance

## Testing

Run the test suite to verify functionality:

```bash
make test
./udfs_test

# Test with actual UDF image (if available)
./udfs_test /path/to/test.iso
```

## Based on UDFCT

This driver is based on the excellent UDFCT (UDF Conformance Testing Application) by Koninklijke Philips Electronics N.V. UDFCT provides robust, well-tested UDF parsing functionality that has been verified against the UDF specification.

### UDFCT Components Used

- **uct_core**: Core UDF structure parsing and validation
- **udf_scsi**: Device access layer for SCSI/ATAPI devices
- **Device abstraction**: Support for image files and various device types

## License

This code maintains the licensing terms of the original UDFCT code. See the UDFCT LICENSE.TXT file for details.

## Contributing

This is a focused implementation targeting basic UDF reading functionality. Contributions should maintain the principle of minimal changes and reuse of UDFCT's proven code.

## Limitations

- **Read-only**: No write/modification support
- **Linux focus**: Primary target is Linux, other platforms may work but are not actively tested
- **Basic features**: Initially supports only essential file system operations
- **Image files**: Device support is inherited from UDFCT but may have platform limitations

## Future Roadmap

1. **Complete core implementation**: Finish file and directory operations
2. **Performance optimization**: Caching and optimized data structures
3. **Platform support**: Testing and fixes for additional Unix systems
4. **Advanced UDF features**: Extended attributes, metadata partition support
5. **Integration helpers**: FUSE filesystem wrapper, language bindings