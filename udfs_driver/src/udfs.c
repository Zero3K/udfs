/* UDFS - UDF File System Driver Implementation
 * Based on UDFCT (UDF Conformance Testing Application) source code
 */

#include "uct_core.h"
#include "udfs.h"
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdarg.h>
#include <time.h>

/* Internal structure for mounted volume */
struct udfs_volume {
    Device *device;
    UdfMountContext *mc;
    char *source_path;
    bool is_mounted;
};

/* Internal structure for open file */
struct udfs_file {
    udfs_volume_t *volume;
    Node *node;
    uint64_t position;
    uint64_t size;
};

/* Internal structure for open directory */
struct udfs_dir {
    udfs_volume_t *volume;
    Node *node;
    Node *current_child;
    uint32_t position;
};

/* Global debug flag */
static bool debug_enabled = false;

/*
 * Utility Functions
 */

const char *udfs_get_version(void) {
    return "1.0.0";
}

const char *udfs_strerror(udfs_result_t result) {
    switch (result) {
        case UDFS_OK: return "Success";
        case UDFS_ERROR_INVALID_PARAM: return "Invalid parameter";
        case UDFS_ERROR_NOT_UDF: return "Not a valid UDF file system";
        case UDFS_ERROR_IO: return "I/O error";
        case UDFS_ERROR_NO_MEMORY: return "Out of memory";
        case UDFS_ERROR_NOT_FOUND: return "File or directory not found";
        case UDFS_ERROR_NOT_MOUNTED: return "Volume not mounted";
        case UDFS_ERROR_ALREADY_MOUNTED: return "Volume already mounted";
        case UDFS_ERROR_NOT_SUPPORTED: return "Operation not supported";
        default: return "Unknown error";
    }
}

void udfs_set_debug(bool enable) {
    debug_enabled = (enable != 0);  /* Convert bool to int for compatibility */
}

static void debug_print(const char *format, ...) {
    if (debug_enabled) {
        va_list args;
        va_start(args, format);
        fprintf(stderr, "UDFS DEBUG: ");
        vfprintf(stderr, format, args);
        fprintf(stderr, "\n");
        va_end(args);
    }
}

/*
 * Internal helper functions
 */

/* Convert UDFCT timestamp to Unix timestamp */
static uint64_t timestamp_to_unix(const Timestamp *ts) {
    if (!ts) return 0;
    
    /* Basic conversion - this is simplified and may need refinement */
    struct tm tm = {0};
    tm.tm_year = ts->Year - 1900;
    tm.tm_mon = ts->Month - 1;
    tm.tm_mday = ts->Day;
    tm.tm_hour = ts->Hour;
    tm.tm_min = ts->Minute;
    tm.tm_sec = ts->Second;
    
    return (uint64_t)mktime(&tm);
}

/* Convert UDFCT file type to UDFS file type */
static udfs_file_type_t convert_file_type(uint8_t udf_type) {
    switch (udf_type) {
        case 4: return UDFS_TYPE_DIRECTORY;  /* UDF directory */
        case 5: return UDFS_TYPE_FILE;       /* UDF file */
        case 12: return UDFS_TYPE_SYMLINK;   /* UDF symbolic link */
        default: return UDFS_TYPE_UNKNOWN;
    }
}

/* Fill file info structure from UDFCT Node */
static void fill_file_info(const Node *node, udfs_file_info_t *info) {
    if (!node || !info) return;
    
    memset(info, 0, sizeof(udfs_file_info_t));
    
    /* Copy file name */
    if (node->unicodeName) {
        /* Convert Unicode name to UTF-8 - simplified */
        strncpy(info->name, (const char*)node->unicodeName, sizeof(info->name) - 1);
        info->name[sizeof(info->name) - 1] = '\0';
    }
    
    /* Set file type and size */
    if (node->fe) {
        FileEntry *fe = (FileEntry*)node->fe;
        info->type = convert_file_type(fe->icbTag.fileType);
        info->size = fe->informationLength;
        info->unique_id = fe->uniqueID;
        
        /* Set timestamps */
        info->creation_time = timestamp_to_unix(&fe->accessTime);
        info->modification_time = timestamp_to_unix(&fe->modificationTime);
        info->access_time = timestamp_to_unix(&fe->accessTime);
    }
    /* TODO: Handle Extended File Entry when we understand its structure */
}

/*
 * Volume Operations
 */

udfs_result_t udfs_mount_image(const char *image_path, udfs_volume_t **volume) {
    if (!image_path || !volume) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    debug_print("Mounting UDF image: %s", image_path);
    
    /* Allocate volume structure */
    udfs_volume_t *vol = calloc(1, sizeof(udfs_volume_t));
    if (!vol) {
        return UDFS_ERROR_NO_MEMORY;
    }
    
    /* Store source path */
    vol->source_path = strdup(image_path);
    if (!vol->source_path) {
        free(vol);
        return UDFS_ERROR_NO_MEMORY;
    }
    
    /* Try to open the image file as a device using UDFCT selectDevice mechanism */
    Device *device = NULL;
    MediumInfo mediumInfo = {0};  /* Initialize medium info */
    
    /* This is a simplified approach - in full implementation we'd use selectDevice() */
    /* For now, we'll assume this fails and return not supported */
    debug_print("Device opening not yet implemented");
    free(vol->source_path);
    free(vol);
    return UDFS_ERROR_NOT_SUPPORTED;
    
    /* TODO: Complete implementation following UDFCT pattern:
     * 1. Use selectDevice() or equivalent to open image file
     * 2. Initialize medium info with finishMediumInfo() and initTheMediumInfo()
     * 3. Allocate UdfMountContext with NEWSTRUCT(UdfMountContext,1)
     * 4. Call udfGetVolumeInformation(mc)
     * 5. Call udfMountLogicalVolume(mc, &startTime)
     */
}

udfs_result_t udfs_mount_device(const char *device_path, udfs_volume_t **volume) {
    /* For now, treat device mounting the same as image mounting */
    /* In a real implementation, this would use SCSI/device-specific code */
    return udfs_mount_image(device_path, volume);
}

udfs_result_t udfs_unmount(udfs_volume_t *volume) {
    if (!volume) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    if (!volume->is_mounted) {
        return UDFS_ERROR_NOT_MOUNTED;
    }
    
    debug_print("Unmounting UDF volume");
    
    /* Clean up UDFCT structures */
    if (volume->mc) {
        udfUnmountLogicalVolume(volume->mc);
    }
    
    if (volume->device) {
        if (volume->device->closeAndFreeImpUse) {
            volume->device->closeAndFreeImpUse(volume->device->impUse);
        }
        free(volume->device);
    }
    
    free(volume->source_path);
    free(volume);
    
    return UDFS_OK;
}

udfs_result_t udfs_get_volume_info(udfs_volume_t *volume, 
                                   char *label, size_t label_size,
                                   uint64_t *total_size,
                                   uint64_t *free_size) {
    if (!volume || !volume->is_mounted) {
        return UDFS_ERROR_NOT_MOUNTED;
    }
    
    /* Get volume label from UDFCT mount context */
    if (label && label_size > 0) {
        /* TODO: Find correct field for volume label in UdfVolumeInformation */
        strncpy(label, "UDF Volume", label_size - 1);
        label[label_size - 1] = '\0';
    }
    
    /* Get size information */
    if (total_size) {
        *total_size = volume->device ? 
            (uint64_t)volume->device->mediumInfo.lastValidBlockNr * 
            volume->device->mediumInfo.blockSize : 0;
    }
    
    if (free_size) {
        *free_size = 0; /* TODO: Calculate free space */
    }
    
    return UDFS_OK;
}

/*
 * File Operations - Basic stubs for now
 */

udfs_result_t udfs_open_file(udfs_volume_t *volume, const char *path, udfs_file_t **file) {
    if (!volume || !volume->is_mounted || !path || !file) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    debug_print("Opening file: %s", path);
    
    /* TODO: Implement file opening using UDFCT node traversal */
    return UDFS_ERROR_NOT_SUPPORTED;
}

udfs_result_t udfs_close_file(udfs_file_t *file) {
    if (!file) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    free(file);
    return UDFS_OK;
}

udfs_result_t udfs_read_file(udfs_file_t *file, void *buffer, size_t size, size_t *bytes_read) {
    if (!file || !buffer || !bytes_read) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    /* TODO: Implement file reading */
    *bytes_read = 0;
    return UDFS_ERROR_NOT_SUPPORTED;
}

udfs_result_t udfs_seek_file(udfs_file_t *file, uint64_t offset) {
    if (!file) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    /* TODO: Implement file seeking */
    return UDFS_ERROR_NOT_SUPPORTED;
}

udfs_result_t udfs_tell_file(udfs_file_t *file, uint64_t *offset) {
    if (!file || !offset) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    *offset = file->position;
    return UDFS_OK;
}

udfs_result_t udfs_get_file_info(udfs_file_t *file, udfs_file_info_t *info) {
    if (!file || !info) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    fill_file_info(file->node, info);
    return UDFS_OK;
}

/*
 * Directory Operations - Basic stubs for now  
 */

udfs_result_t udfs_open_dir(udfs_volume_t *volume, const char *path, udfs_dir_t **dir) {
    if (!volume || !volume->is_mounted || !path || !dir) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    debug_print("Opening directory: %s", path);
    
    /* TODO: Implement directory opening */
    return UDFS_ERROR_NOT_SUPPORTED;
}

udfs_result_t udfs_close_dir(udfs_dir_t *dir) {
    if (!dir) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    free(dir);
    return UDFS_OK;
}

udfs_result_t udfs_read_dir(udfs_dir_t *dir, udfs_dir_entry_t *entry) {
    if (!dir || !entry) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    /* TODO: Implement directory reading */
    entry->valid = false;
    return UDFS_ERROR_NOT_SUPPORTED;
}

udfs_result_t udfs_rewind_dir(udfs_dir_t *dir) {
    if (!dir) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    dir->position = 0;
    dir->current_child = dir->node ? dir->node->firstChild : NULL;
    return UDFS_OK;
}

/*
 * Path Operations - Basic stubs for now
 */

udfs_result_t udfs_stat(udfs_volume_t *volume, const char *path, udfs_file_info_t *info) {
    if (!volume || !volume->is_mounted || !path || !info) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    /* TODO: Implement path stat */
    return UDFS_ERROR_NOT_SUPPORTED;
}

bool udfs_exists(udfs_volume_t *volume, const char *path) {
    if (!volume || !volume->is_mounted || !path) {
        return 0;  /* Return int 0 for false for compatibility */
    }
    
    /* TODO: Implement path existence check */
    return 0;  /* Return int 0 for false for compatibility */
}