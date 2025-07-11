/* UDFS - UDF File System Driver
 * Based on UDFCT (UDF Conformance Testing Application) source code
 *
 * Copyright notice from original UDFCT code:
 * Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 * 
 * This driver provides a simple read-only interface to UDF file systems
 * for accessing files and directories from UDF media or image files.
 */

#ifndef __UDFS_H__
#define __UDFS_H__

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* UDFS API Version */
#define UDFS_VERSION_MAJOR 1
#define UDFS_VERSION_MINOR 0
#define UDFS_VERSION_PATCH 0

/* Return codes */
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

/* File types */
typedef enum {
    UDFS_TYPE_UNKNOWN = 0,
    UDFS_TYPE_FILE = 1,
    UDFS_TYPE_DIRECTORY = 2,
    UDFS_TYPE_SYMLINK = 3
} udfs_file_type_t;

/* Extended attribute types */
typedef enum {
    UDFS_EA_CHARSET_INFO = 1,
    UDFS_EA_FILE_TIMES = 5,
    UDFS_EA_INFO_TIMES = 6,
    UDFS_EA_DEVICE_SPEC = 12,
    UDFS_EA_IMPL_USE = 2048,
    UDFS_EA_APP_USE = 65536
} udfs_ea_type_t;

/* Extended attribute information */
typedef struct {
    udfs_ea_type_t type;           /* Extended attribute type */
    uint32_t length;               /* Length of attribute data */
    char name[64];                 /* Human-readable name */
    bool available;                /* True if this EA is present */
} udfs_ea_info_t;

/* File information structure */
typedef struct {
    char name[256];                /* File/directory name */
    udfs_file_type_t type;         /* File type */
    uint64_t size;                 /* File size in bytes */
    uint64_t creation_time;        /* Creation time (Unix timestamp) */
    uint64_t modification_time;    /* Modification time (Unix timestamp) */
    uint64_t access_time;          /* Access time (Unix timestamp) */
    uint32_t unique_id;            /* Unique file identifier */
} udfs_file_info_t;

/* Opaque handles */
typedef struct udfs_volume udfs_volume_t;
typedef struct udfs_file udfs_file_t;
typedef struct udfs_dir udfs_dir_t;

/* Directory entry for iteration */
typedef struct {
    udfs_file_info_t info;
    bool valid;                    /* True if this entry is valid */
} udfs_dir_entry_t;

/*
 * Volume Operations
 */

/* Mount a UDF volume from an image file */
udfs_result_t udfs_mount_image(const char *image_path, udfs_volume_t **volume);

/* Mount a UDF volume from a device */
udfs_result_t udfs_mount_device(const char *device_path, udfs_volume_t **volume);

/* Unmount and free a UDF volume */
udfs_result_t udfs_unmount(udfs_volume_t *volume);

/* Get volume information */
udfs_result_t udfs_get_volume_info(udfs_volume_t *volume, 
                                   char *label, size_t label_size,
                                   uint64_t *total_size,
                                   uint64_t *free_size);

/*
 * File Operations
 */

/* Open a file for reading */
udfs_result_t udfs_open_file(udfs_volume_t *volume, const char *path, udfs_file_t **file);

/* Close a file */
udfs_result_t udfs_close_file(udfs_file_t *file);

/* Read data from a file */
udfs_result_t udfs_read_file(udfs_file_t *file, void *buffer, size_t size, size_t *bytes_read);

/* Seek to a position in a file */
udfs_result_t udfs_seek_file(udfs_file_t *file, uint64_t offset);

/* Get current position in a file */
udfs_result_t udfs_tell_file(udfs_file_t *file, uint64_t *offset);

/* Get file information */
udfs_result_t udfs_get_file_info(udfs_file_t *file, udfs_file_info_t *info);

/*
 * Directory Operations
 */

/* Open a directory for reading */
udfs_result_t udfs_open_dir(udfs_volume_t *volume, const char *path, udfs_dir_t **dir);

/* Close a directory */
udfs_result_t udfs_close_dir(udfs_dir_t *dir);

/* Read next directory entry */
udfs_result_t udfs_read_dir(udfs_dir_t *dir, udfs_dir_entry_t *entry);

/* Reset directory to beginning */
udfs_result_t udfs_rewind_dir(udfs_dir_t *dir);

/*
 * Path Operations
 */

/* Get file/directory information by path */
udfs_result_t udfs_stat(udfs_volume_t *volume, const char *path, udfs_file_info_t *info);

/* Check if a path exists */
bool udfs_exists(udfs_volume_t *volume, const char *path);

/*
 * Extended Attributes Operations (Phase 3)
 */

/* Get list of available extended attributes for a file */
udfs_result_t udfs_list_extended_attributes(udfs_file_t *file, udfs_ea_info_t *ea_list, 
                                           size_t max_count, size_t *actual_count);

/* Read extended attribute data */
udfs_result_t udfs_read_extended_attribute(udfs_file_t *file, udfs_ea_type_t ea_type,
                                          void *buffer, size_t buffer_size, size_t *data_size);

/* Get extended attribute info by type */
udfs_result_t udfs_get_extended_attribute_info(udfs_file_t *file, udfs_ea_type_t ea_type,
                                              udfs_ea_info_t *ea_info);

/*
 * Utility Functions
 */

/* Get UDFS library version */
const char *udfs_get_version(void);

/* Get human-readable error message */
const char *udfs_strerror(udfs_result_t result);

/* Enable/disable debug output */
void udfs_set_debug(bool enable);

#ifdef __cplusplus
}
#endif

#endif /* __UDFS_H__ */