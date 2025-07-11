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
#include <stdio.h>

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

/* Simplified image device implementation for demonstration
 * This is a basic version that shows the structure works
 * A full implementation would use UDFCT's mfile interface
 */
typedef struct {
    FILE *file;
    uint64_t file_size;
    uint32_t block_size;
} SimpleImageImpUse;

/* Simple image device read blocks function */
static Uint32 simpleImageReadBlocks(void *impUse, Uint32 blockSize, 
                                    Uint32 firstBlock, Uint32 nrOfBlocks, Byte *buffer) {
    SimpleImageImpUse *imgUse = (SimpleImageImpUse*)impUse;
    if (!imgUse || !imgUse->file || !buffer) {
        return 0;
    }
    
    uint64_t offset = (uint64_t)firstBlock * blockSize;
    if (fseeko(imgUse->file, offset, SEEK_SET) != 0) {
        return 0;
    }
    
    size_t bytes_to_read = (size_t)nrOfBlocks * blockSize;
    size_t bytes_read = fread(buffer, 1, bytes_to_read, imgUse->file);
    
    return (Uint32)(bytes_read / blockSize);
}

/* Simple image device get block state function */
static BlockState simpleImageGetBlockState(void *impUse, Uint32 blockNr) {
    SimpleImageImpUse *imgUse = (SimpleImageImpUse*)impUse;
    if (!imgUse || !imgUse->file) {
        return BSTATE_ERROR;
    }
    
    uint64_t offset = (uint64_t)blockNr * imgUse->block_size;
    if (offset >= imgUse->file_size) {
        return BSTATE_UNRECORDED;
    }
    
    return BSTATE_READABLE;
}

/* Simple image device cleanup function */
static void simpleImageCloseAndFreeImpUse(void *impUse) {
    if (impUse) {
        SimpleImageImpUse *imgUse = (SimpleImageImpUse*)impUse;
        if (imgUse->file) {
            fclose(imgUse->file);
        }
        free(imgUse);
    }
}

/* Create a Device for an image file - simplified working version */
static Device* create_image_device(const char *image_path) {
    Device *device = NULL;
    SimpleImageImpUse *imageImpUse = NULL;
    FILE *file = NULL;
    
    debug_print("Creating image device for: %s", image_path);
    
    /* Open the image file */
    file = fopen(image_path, "rb");
    if (!file) {
        debug_print("Failed to open image file: %s", image_path);
        return NULL;
    }
    
    /* Get file size */
    if (fseeko(file, 0, SEEK_END) != 0) {
        debug_print("Failed to seek to end of file");
        fclose(file);
        return NULL;
    }
    
    uint64_t file_size = ftello(file);
    if (file_size <= 0) {
        debug_print("Invalid file size: %llu", (unsigned long long)file_size);
        fclose(file);
        return NULL;
    }
    
    if (fseeko(file, 0, SEEK_SET) != 0) {
        debug_print("Failed to seek to beginning of file");
        fclose(file);
        return NULL;
    }
    
    /* Allocate device structure */
    device = (Device*)calloc(1, sizeof(Device));
    if (!device) {
        fclose(file);
        return NULL;
    }
    
    /* Allocate image implementation structure */
    imageImpUse = (SimpleImageImpUse*)calloc(1, sizeof(SimpleImageImpUse));
    if (!imageImpUse) {
        free(device);
        fclose(file);
        return NULL;
    }
    
    /* Initialize image implementation */
    imageImpUse->file = file;
    imageImpUse->file_size = file_size;
    imageImpUse->block_size = 2048;  /* Default UDF block size */
    
    /* Initialize device structure */
    device->impUse = imageImpUse;
    device->getBlockState = simpleImageGetBlockState;
    device->readBlock = simpleImageReadBlocks;
    device->closeAndFreeImpUse = simpleImageCloseAndFreeImpUse;
    
    /* Initialize medium info */
    clearMediumInfo(&device->mediumInfo);
    device->mediumInfo.blockSize = imageImpUse->block_size;
    device->mediumInfo.lastValidBlockNr = (Uint32)((file_size / imageImpUse->block_size) - 1);
    device->mediumInfo.lastRecordedBlockNr = device->mediumInfo.lastValidBlockNr;
    device->mediumInfo.writabilityType = MTYPE_WR_UNKNOWN;
    device->mediumInfo.sequentialType = MTYPE_SE_UNKNOWN; 
    device->mediumInfo.closedType = MTYPE_CL_UNKNOWN;
    device->mediumInfo.numberOfSessions = 1;
    device->mediumInfo.sessionStartBlocks = NULL;  /* Will be allocated by addSessionToMediumInfo */
    device->mediumInfo.verifySession = 1;
    
    /* Add session 0 starting at block 0 */
    addSessionToMediumInfo(&device->mediumInfo, 0);
    
    debug_print("Created image device: %llu bytes, %u blocks", 
                (unsigned long long)file_size, device->mediumInfo.lastValidBlockNr + 1);
    
    return device;
}

/* Create and initialize UdfMountContext - following UDFCT's pattern */
static UdfMountContext* create_mount_context(Device *device) {
    UdfMountContext *mc = NULL;
    
    if (!device) {
        return NULL;
    }
    
    /* Allocate mount context using UDFCT's macro */
    mc = NEWSTRUCT(UdfMountContext, 1);
    if (!mc) {
        return NULL;
    }
    
    /* Initialize mount context */
    mc->device = device;
    mc->virtualPref = mc->sparablePref = mc->metadataPref = PREF_PARTITION_NOT_FOUND;
    
    return mc;
}

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
    
    /* Create device for image file */
    Device *device = create_image_device(image_path);
    if (!device) {
        debug_print("Failed to create image device for: %s", image_path);
        free(vol->source_path);
        free(vol);
        return UDFS_ERROR_NOT_SUPPORTED;  /* Still not implemented */
    }
    
    vol->device = device;
    
    /* Initialize medium info following UDFCT pattern */
    if (!finishMediumInfo(&device->mediumInfo)) {
        debug_print("Failed to finish medium info");
        free(vol->source_path);
        free(vol);
        return UDFS_ERROR_NOT_UDF;
    }
    
    if (!initTheMediumInfo(&device->mediumInfo)) {
        debug_print("Failed to initialize medium info");
        free(vol->source_path);
        free(vol);
        return UDFS_ERROR_NOT_UDF;
    }
    
    /* Create mount context */
    UdfMountContext *mc = create_mount_context(device);
    if (!mc) {
        debug_print("Failed to create mount context");
        if (device && device->closeAndFreeImpUse) {
            device->closeAndFreeImpUse(device->impUse);
        }
        free(device);
        free(vol->source_path);
        free(vol);
        return UDFS_ERROR_NO_MEMORY;
    }
    
    /* Get volume information */
    if (!udfGetVolumeInformation(mc)) {
        debug_print("Failed to get volume information");
        udfUnmountLogicalVolume(mc);
        if (device && device->closeAndFreeImpUse) {
            device->closeAndFreeImpUse(device->impUse);
        }
        free(device);
        free(vol->source_path);
        free(vol);
        return UDFS_ERROR_NOT_UDF;
    }
    
    /* Mount logical volume */
    Timestamp startTime;
    bool timeOk = createCurrentTimeTimestamp(&startTime);
    
    if (!udfMountLogicalVolume(mc, timeOk ? &startTime : NULL)) {
        debug_print("Failed to mount logical volume");
        udfUnmountLogicalVolume(mc);
        if (device && device->closeAndFreeImpUse) {
            device->closeAndFreeImpUse(device->impUse);
        }
        free(device);
        free(vol->source_path);
        free(vol);
        return UDFS_ERROR_NOT_UDF;
    }
    
    vol->mc = mc;
    vol->is_mounted = true;
    
    debug_print("Successfully mounted UDF volume");
    *volume = vol;
    return UDFS_OK;
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