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

#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif

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
    
    /* Initialize medium info with basic single session setup
     * Multi-session detection can be enhanced later by reading the
     * Volume Recognition Sequence and Anchor Volume Descriptor Pointers
     */
    clearMediumInfo(&device->mediumInfo);
    device->mediumInfo.blockSize = imageImpUse->block_size;
    device->mediumInfo.lastValidBlockNr = (Uint32)((file_size / imageImpUse->block_size) - 1);
    device->mediumInfo.lastRecordedBlockNr = device->mediumInfo.lastValidBlockNr;
    device->mediumInfo.writabilityType = MTYPE_WR_UNKNOWN;
    device->mediumInfo.sequentialType = MTYPE_SE_UNKNOWN; 
    device->mediumInfo.closedType = MTYPE_CL_UNKNOWN;
    device->mediumInfo.numberOfSessions = 0;  /* Will be set by addSessionToMediumInfo */
    device->mediumInfo.sessionStartBlocks = NULL;  /* Will be allocated by addSessionToMediumInfo */
    device->mediumInfo.verifySession = 0;  /* Use first session as verify session */
    
    /* Add session 0 starting at block 0 (basic single-session setup)
     * In a more advanced implementation, we would scan for multiple sessions
     * by looking for multiple Anchor Volume Descriptor Pointers
     */
    addSessionToMediumInfo(&device->mediumInfo, 0);
    
    /* TODO: Enhanced multi-session detection could be added here by:
     * 1. Scanning for Anchor Volume Descriptor Pointers at standard locations
     * 2. Detecting different UDF revisions in different sessions
     * 3. Supporting incremental UDF media (like DVD+RW with multiple writes)
     */
    
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

/* Convert UDFCT extended attribute type to UDFS type */
static udfs_ea_type_t convert_ea_type(uint32_t udf_ea_type) {
    switch (udf_ea_type) {
        case 1: return UDFS_EA_CHARSET_INFO;
        case 5: return UDFS_EA_FILE_TIMES;
        case 6: return UDFS_EA_INFO_TIMES;
        case 12: return UDFS_EA_DEVICE_SPEC;
        case 2048: return UDFS_EA_IMPL_USE;
        case 65536: return UDFS_EA_APP_USE;
        default: return UDFS_EA_CHARSET_INFO; /* fallback */
    }
}

/* Get human-readable name for extended attribute type */
static const char* get_ea_name(udfs_ea_type_t ea_type) {
    switch (ea_type) {
        case UDFS_EA_CHARSET_INFO: return "Character Set Information";
        case UDFS_EA_FILE_TIMES: return "File Times";
        case UDFS_EA_INFO_TIMES: return "Information Times";
        case UDFS_EA_DEVICE_SPEC: return "Device Specification";
        case UDFS_EA_IMPL_USE: return "Implementation Use";
        case UDFS_EA_APP_USE: return "Application Use";
        default: return "Unknown";
    }
}

/* Parse extended attributes from file entry
 * Returns pointer to EA space and fills in length
 */
static uint8_t* get_extended_attributes(Node *node, uint32_t *ea_length) {
    uint32_t length;
    uint8_t *ea_start;
    
    if (!node || !node->fe || !ea_length) {
        return NULL;
    }
    
    /* Get EA length and start from file entry */
    length = *(pFE_lengthOfExtendedAttributes(node->fe));
    *ea_length = length;
    
    if (length == 0) {
        return NULL; /* No extended attributes */
    }
    
    /* Get start of EA space */
    ea_start = (uint8_t*)pFE_startOfExtendedAttributes(node->fe);
    
    debug_print("Found %u bytes of extended attributes", length);
    return ea_start;
}

/* Parse next extended attribute from EA space
 * Returns pointer to next EA or NULL if done
 * Fills in ea_info with information about current EA
 */
static uint8_t* parse_next_ea(uint8_t *current_ea, uint8_t *ea_end, udfs_ea_info_t *ea_info) {
    EAGenericHead *ea_head;
    uint32_t ea_length;
    
    if (!current_ea || !ea_end || !ea_info || current_ea >= ea_end) {
        return NULL;
    }
    
    /* Check if we have enough space for EA header */
    if (current_ea + sizeof(EAGenericHead) > ea_end) {
        debug_print("Not enough space for EA header");
        return NULL;
    }
    
    ea_head = (EAGenericHead*)current_ea;
    ea_length = ea_head->attributeLength;
    
    /* Validate EA length */
    if (ea_length < sizeof(EAGenericHead) || current_ea + ea_length > ea_end) {
        debug_print("Invalid EA length: %u", ea_length);
        return NULL;
    }
    
    /* Fill in EA info */
    ea_info->type = convert_ea_type(ea_head->attributeType);
    ea_info->length = ea_length;
    strncpy(ea_info->name, get_ea_name(ea_info->type), sizeof(ea_info->name) - 1);
    ea_info->name[sizeof(ea_info->name) - 1] = '\0';
    ea_info->available = true;
    
    debug_print("Found EA: type=%u, length=%u, name=%s", 
                ea_head->attributeType, ea_length, ea_info->name);
    
    /* Return pointer to next EA (aligned to 4-byte boundary) */
    uint8_t *next_ea = current_ea + ea_length;
    
    /* Align to 4-byte boundary as required by UDF */
    next_ea = (uint8_t*)(((uintptr_t)next_ea + 3) & ~3);
    
    return (next_ea < ea_end) ? next_ea : NULL;
}
static udfs_file_type_t convert_file_type(uint8_t udf_type) {
    switch (udf_type) {
        case 4: return UDFS_TYPE_DIRECTORY;  /* UDF directory */
        case 5: return UDFS_TYPE_FILE;       /* UDF file */
        case 12: return UDFS_TYPE_SYMLINK;   /* UDF symbolic link */
        default: return UDFS_TYPE_UNKNOWN;
    }
}

/* Find a node by path from root 
 * Returns the node if found, NULL if not found
 * Supports both absolute paths (starting with /) and relative paths
 * Uses proper Unicode filename handling (Phase 3)
 */
static Node* find_node_by_path(udfs_volume_t *volume, const char *path) {
    Node *current_node;
    Node *child;
    char *path_copy, *token, *saveptr;
    bool found;
    
    if (!volume || !volume->mc || !path) {
        return NULL;
    }
    
    /* Start from root node */
    current_node = volume->mc->rootNode;
    if (!current_node) {
        debug_print("No root node available");
        return NULL;
    }
    
    /* Handle root path "/" */
    if (strcmp(path, "/") == 0) {
        return current_node;
    }
    
    /* Make a copy of the path for tokenization */
    path_copy = strdup(path);
    if (!path_copy) {
        return NULL;
    }
    
    /* Skip leading slash if present */
    char *work_path = path_copy;
    if (work_path[0] == '/') {
        work_path++;
    }
    
    /* Tokenize path and traverse */
    token = strtok_r(work_path, "/", &saveptr);
    while (token && current_node) {
        debug_print("Looking for path component: %s", token);
        
        /* Search for this component in current directory */
        found = false;
        for (child = current_node->firstChild; child; child = child->nextInDirectory) {
            /* Use UDFCT's proper Unicode string comparison function */
            bool head_matches_result = false;
            if (stringIsUnicodeName(child, token, (bool*)&head_matches_result)) {
                if (!head_matches_result) { /* Exact match */
                    current_node = child;
                    found = true;
                    debug_print("Found matching child (Unicode): %s", token);
                    break;
                }
            }
        }
        
        if (!found) {
            debug_print("Path component not found: %s", token);
            free(path_copy);
            return NULL;
        }
        
        token = strtok_r(NULL, "/", &saveptr);
    }
    
    free(path_copy);
    return current_node;
}

/* Internal file reading function using UDFCT APIs
 * This is a simplified version that handles basic file reading
 */
static bool udfs_read_file_data(UdfMountContext *mc, Node *node,
                               uint64_t file_offset, uint64_t nr_bytes,
                               uint8_t *buffer, uint64_t *bytes_read) {
    const MediumInfo *vmi;
    uint32_t block_size;
    uint64_t max_bytes;
    uint32_t offset32;
    uint8_t fe_ad_type;
    UdfAllocationItem *ali;
    FileEntry *fe;
    
    *bytes_read = 0;
    
    if (!mc || !node || !node->fe) {
        return false;
    }
    
    vmi = getTheMediumInfo();
    if (!vmi) {
        return false;
    }
    
    block_size = vmi->blockSize;
    fe = (FileEntry*)node->fe;
    fe_ad_type = GET_ADTYPE(fe->icbTag.flags);
    
    /* Check file bounds */
    max_bytes = fe->informationLength;
    if (file_offset >= max_bytes) {
        return true; /* EOF reached */
    }
    
    if (file_offset + nr_bytes > max_bytes) {
        nr_bytes = max_bytes - file_offset;
    }
    
    if (nr_bytes == 0) {
        return true;
    }
    
    /* Handle embedded files (data in FE) */
    if (fe_ad_type == ADT_INFE) {
        if (buffer) {
            memcpy(buffer,
                   (uint8_t*)(pFE_startOfExtendedAttributes(node->fe)) +
                   (*(pFE_lengthOfExtendedAttributes(node->fe))) +
                   (uint32_t)file_offset,
                   (uint32_t)nr_bytes);
        }
        *bytes_read = nr_bytes;
        return true;
    }
    
    /* For allocated files, we need allocation descriptors */
    if (!node->al) {
        debug_print("No allocation list for file");
        return false;
    }
    
    /* Find the allocation descriptor for this offset */
    if (!nodeFindAllocationDescriptor(node, file_offset, &ali, &offset32)) {
        debug_print("Could not find allocation descriptor");
        return false;
    }
    
    /* Read data extent by extent */
    while (ali && nr_bytes > 0) {
        uint8_t extent_type;
        uint32_t read_this_pass;
        uint32_t start_block, nr_of_blocks;
        uint64_t left_in_extent;
        uint8_t *temp_buffer = NULL;
        uint32_t blocks_read;
        uint16_t part_ref;
        uint32_t logical_block;
        
        /* Calculate extent parameters */
        left_in_extent = adGetExtentSize(&ali->aad.anyAd);
        left_in_extent = ROUNDUPMULT(left_in_extent, block_size);
        left_in_extent -= (uint64_t)offset32;
        
        read_this_pass = (uint32_t)MIN(nr_bytes, left_in_extent);
        start_block = offset32 / block_size;
        nr_of_blocks = 1 + ((offset32 + read_this_pass - 1) / block_size) - start_block;
        
        extent_type = adGetExtentType(&ali->aad.anyAd);
        
        if (extent_type == ADEL_NOT_RECORDED_NOT_ALLOCATED ||
            extent_type == ADEL_NOT_RECORDED_BUT_ALLOCATED) {
            /* Unrecorded extent - read as zeros */
            if (buffer) {
                memset(buffer, 0, read_this_pass);
                buffer += read_this_pass;
            }
        } else {
            /* Allocated extent - read from device */
            if (!udfGetLocation(&ali->aad, node->al->itemAdType, 0, &part_ref, &logical_block)) {
                debug_print("Could not get location for allocation descriptor");
                return false;
            }
            
            logical_block += start_block;
            
            if (buffer && nr_of_blocks > 0) {
                /* Allocate temporary buffer for block-aligned reading */
                temp_buffer = malloc(nr_of_blocks * block_size);
                if (!temp_buffer) {
                    debug_print("Could not allocate temporary buffer");
                    return false;
                }
                
                blocks_read = readBlocksFromPartition(mc, temp_buffer, part_ref, logical_block, nr_of_blocks);
                if (blocks_read != nr_of_blocks) {
                    debug_print("Could not read all blocks (got %u, expected %u)", blocks_read, nr_of_blocks);
                    free(temp_buffer);
                    return false;
                }
                
                /* Copy relevant portion to output buffer */
                memcpy(buffer, temp_buffer + (offset32 % block_size), read_this_pass);
                buffer += read_this_pass;
                
                free(temp_buffer);
            }
        }
        
        nr_bytes -= read_this_pass;
        *bytes_read += read_this_pass;
        offset32 = 0; /* Subsequent extents start at beginning */
        ali = ali->next;
    }
    
    return true;
}

/* Fill file info structure from UDFCT Node 
 * Uses proper Unicode to UTF-8 conversion (Phase 3)
 */
static void fill_file_info(const Node *node, udfs_file_info_t *info) {
    if (!node || !info) return;
    
    memset(info, 0, sizeof(udfs_file_info_t));
    
    /* Copy file name using proper Unicode conversion */
    if (node->unicodeName && node->unicodeNameLen > 0) {
        /* Use UDFCT's Unicode to char conversion function */
        Byte converted_name[256];
        if (convertUnicode2Char(node->unicodeName, (Uint8)node->unicodeNameLen, converted_name)) {
            /* Safely copy the converted name */
            strncpy(info->name, (const char*)converted_name, sizeof(info->name) - 1);
            info->name[sizeof(info->name) - 1] = '\0';
            debug_print("Converted Unicode name: %s (length=%u)", info->name, node->unicodeNameLen);
        } else {
            /* Fallback to simplified conversion if UDFCT function fails */
            size_t copy_len = MIN(node->unicodeNameLen, sizeof(info->name) - 1);
            for (size_t i = 0; i < copy_len; i++) {
                info->name[i] = (char)(node->unicodeName[i] & 0xFF);
            }
            info->name[copy_len] = '\0';
            debug_print("Used fallback Unicode conversion: %s", info->name);
        }
    } else {
        strcpy(info->name, "<no name>");
        debug_print("Node has no Unicode name");
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
 * File Operations 
 */

udfs_result_t udfs_open_file(udfs_volume_t *volume, const char *path, udfs_file_t **file) {
    Node *node;
    udfs_file_t *ufile;
    FileEntry *fe;
    
    if (!volume || !volume->is_mounted || !path || !file) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    debug_print("Opening file: %s", path);
    
    /* Find the node by path */
    node = find_node_by_path(volume, path);
    if (!node) {
        debug_print("File not found: %s", path);
        return UDFS_ERROR_NOT_FOUND;
    }
    
    /* Check if it's a file */
    if (!node->fe) {
        debug_print("Node has no file entry: %s", path);
        return UDFS_ERROR_NOT_FOUND;
    }
    
    fe = (FileEntry*)node->fe;
    if (fe->icbTag.fileType != 5) { /* 5 = UDF file type */
        debug_print("Node is not a file: %s (type=%d)", path, fe->icbTag.fileType);
        return UDFS_ERROR_NOT_FOUND;
    }
    
    /* Read allocation descriptors if not already loaded */
    if (!nodeReadAllocationDescriptors(volume->mc, node)) {
        debug_print("Failed to read allocation descriptors for: %s", path);
        return UDFS_ERROR_IO;
    }
    
    /* Create file handle */
    ufile = calloc(1, sizeof(udfs_file_t));
    if (!ufile) {
        return UDFS_ERROR_NO_MEMORY;
    }
    
    ufile->volume = volume;
    ufile->node = node;
    ufile->position = 0;
    ufile->size = fe->informationLength;
    
    debug_print("Successfully opened file: %s (size=%llu)", path, (unsigned long long)ufile->size);
    *file = ufile;
    return UDFS_OK;
}

udfs_result_t udfs_close_file(udfs_file_t *file) {
    if (!file) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    free(file);
    return UDFS_OK;
}

udfs_result_t udfs_read_file(udfs_file_t *file, void *buffer, size_t size, size_t *bytes_read) {
    uint64_t actual_bytes_read = 0;
    bool read_ok;
    
    if (!file || !buffer || !bytes_read) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    if (!file->volume || !file->volume->is_mounted || !file->node) {
        return UDFS_ERROR_NOT_MOUNTED;
    }
    
    *bytes_read = 0;
    
    /* Check if position is beyond end of file */
    if (file->position >= file->size) {
        return UDFS_OK; /* EOF reached */
    }
    
    /* Limit read size to what's available */
    if (file->position + size > file->size) {
        size = (size_t)(file->size - file->position);
    }
    
    if (size == 0) {
        return UDFS_OK;
    }
    
    debug_print("Reading %zu bytes from position %llu", size, (unsigned long long)file->position);
    
    /* Use our custom file reading function */
    read_ok = udfs_read_file_data(file->volume->mc, file->node, file->position, (uint64_t)size, 
                                 (uint8_t*)buffer, &actual_bytes_read);
    
    if (!read_ok) {
        debug_print("File read failed");
        return UDFS_ERROR_IO;
    }
    
    *bytes_read = (size_t)actual_bytes_read;
    file->position += actual_bytes_read;
    
    debug_print("Successfully read %zu bytes", *bytes_read);
    return UDFS_OK;
}

udfs_result_t udfs_seek_file(udfs_file_t *file, uint64_t offset) {
    if (!file) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    if (!file->volume || !file->volume->is_mounted) {
        return UDFS_ERROR_NOT_MOUNTED;
    }
    
    /* Allow seeking beyond EOF (POSIX behavior) */
    file->position = offset;
    
    debug_print("Seeked to position %llu", (unsigned long long)offset);
    return UDFS_OK;
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
 * Directory Operations
 */

udfs_result_t udfs_open_dir(udfs_volume_t *volume, const char *path, udfs_dir_t **dir) {
    Node *node;
    udfs_dir_t *udir;
    FileEntry *fe;
    
    if (!volume || !volume->is_mounted || !path || !dir) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    debug_print("Opening directory: %s", path);
    
    /* Find the node by path */
    node = find_node_by_path(volume, path);
    if (!node) {
        debug_print("Directory not found: %s", path);
        return UDFS_ERROR_NOT_FOUND;
    }
    
    /* Check if it's a directory */
    if (!node->fe) {
        debug_print("Node has no file entry: %s", path);
        return UDFS_ERROR_NOT_FOUND;
    }
    
    fe = (FileEntry*)node->fe;
    if (fe->icbTag.fileType != 4) { /* 4 = UDF directory type */
        debug_print("Node is not a directory: %s (type=%d)", path, fe->icbTag.fileType);
        return UDFS_ERROR_NOT_FOUND;
    }
    
    /* Create directory handle */
    udir = calloc(1, sizeof(udfs_dir_t));
    if (!udir) {
        return UDFS_ERROR_NO_MEMORY;
    }
    
    udir->volume = volume;
    udir->node = node;
    udir->current_child = node->firstChild;
    udir->position = 0;
    
    debug_print("Successfully opened directory: %s", path);
    *dir = udir;
    return UDFS_OK;
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
    
    if (!dir->volume || !dir->volume->is_mounted || !dir->node) {
        return UDFS_ERROR_NOT_MOUNTED;
    }
    
    /* Check if we have a current child to read */
    if (!dir->current_child) {
        entry->valid = false;
        return UDFS_OK; /* End of directory */
    }
    
    /* Fill in the directory entry */
    fill_file_info(dir->current_child, &entry->info);
    entry->valid = true;
    
    /* Move to next child */
    dir->current_child = dir->current_child->nextInDirectory;
    dir->position++;
    
    debug_print("Read directory entry: %s (type=%d)", 
                entry->info.name, entry->info.type);
    
    return UDFS_OK;
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
 * Path Operations
 */

udfs_result_t udfs_stat(udfs_volume_t *volume, const char *path, udfs_file_info_t *info) {
    Node *node;
    
    if (!volume || !volume->is_mounted || !path || !info) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    debug_print("Getting info for path: %s", path);
    
    /* Find the node by path */
    node = find_node_by_path(volume, path);
    if (!node) {
        debug_print("Path not found: %s", path);
        return UDFS_ERROR_NOT_FOUND;
    }
    
    /* Fill file info */
    fill_file_info(node, info);
    
    debug_print("Successfully got info for: %s (type=%d, size=%llu)", 
                path, info->type, (unsigned long long)info->size);
    
    return UDFS_OK;
}

bool udfs_exists(udfs_volume_t *volume, const char *path) {
    Node *node;
    
    if (!volume || !volume->is_mounted || !path) {
        return false;
    }
    
    node = find_node_by_path(volume, path);
    return (node != NULL);
}

/*
 * Extended Attributes Operations (Phase 3)
 */

udfs_result_t udfs_list_extended_attributes(udfs_file_t *file, udfs_ea_info_t *ea_list, 
                                           size_t max_count, size_t *actual_count) {
    uint8_t *ea_space, *current_ea, *ea_end;
    uint32_t ea_length;
    size_t count = 0;
    udfs_ea_info_t ea_info;
    
    if (!file || !ea_list || !actual_count || max_count == 0) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    if (!file->volume || !file->volume->is_mounted || !file->node) {
        return UDFS_ERROR_NOT_MOUNTED;
    }
    
    *actual_count = 0;
    
    /* Get extended attributes space */
    ea_space = get_extended_attributes(file->node, &ea_length);
    if (!ea_space || ea_length == 0) {
        debug_print("No extended attributes found");
        return UDFS_OK; /* No extended attributes is not an error */
    }
    
    current_ea = ea_space;
    ea_end = ea_space + ea_length;
    
    /* Parse all extended attributes */
    while (current_ea && count < max_count) {
        current_ea = parse_next_ea(current_ea, ea_end, &ea_info);
        if (!current_ea) {
            break; /* No more EAs or parse error */
        }
        
        /* Copy EA info to output array */
        ea_list[count] = ea_info;
        count++;
    }
    
    *actual_count = count;
    debug_print("Found %zu extended attributes", count);
    
    return UDFS_OK;
}

udfs_result_t udfs_read_extended_attribute(udfs_file_t *file, udfs_ea_type_t ea_type,
                                          void *buffer, size_t buffer_size, size_t *data_size) {
    uint8_t *ea_space, *current_ea, *ea_end;
    uint32_t ea_length;
    udfs_ea_info_t ea_info;
    EAGenericHead *ea_head;
    
    if (!file || !buffer || !data_size || buffer_size == 0) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    if (!file->volume || !file->volume->is_mounted || !file->node) {
        return UDFS_ERROR_NOT_MOUNTED;
    }
    
    *data_size = 0;
    
    /* Get extended attributes space */
    ea_space = get_extended_attributes(file->node, &ea_length);
    if (!ea_space || ea_length == 0) {
        debug_print("No extended attributes found");
        return UDFS_ERROR_NOT_FOUND;
    }
    
    current_ea = ea_space;
    ea_end = ea_space + ea_length;
    
    /* Search for the requested EA type */
    while (current_ea) {
        uint8_t *next_ea = parse_next_ea(current_ea, ea_end, &ea_info);
        if (!next_ea) {
            break; /* No more EAs or parse error */
        }
        
        if (ea_info.type == ea_type) {
            /* Found the requested EA */
            ea_head = (EAGenericHead*)current_ea;
            
            /* Calculate data size (total EA size minus header) */
            size_t data_length = ea_head->attributeLength - sizeof(EAGenericHead);
            *data_size = data_length;
            
            /* Check if buffer is large enough */
            if (buffer_size < data_length) {
                debug_print("Buffer too small for EA data: need %zu, have %zu", 
                           data_length, buffer_size);
                return UDFS_ERROR_INVALID_PARAM;
            }
            
            /* Copy EA data (excluding header) */
            if (data_length > 0) {
                memcpy(buffer, current_ea + sizeof(EAGenericHead), data_length);
            }
            
            debug_print("Read EA data: type=%d, size=%zu", ea_type, data_length);
            return UDFS_OK;
        }
        
        current_ea = next_ea;
    }
    
    debug_print("Extended attribute type %d not found", ea_type);
    return UDFS_ERROR_NOT_FOUND;
}

udfs_result_t udfs_get_extended_attribute_info(udfs_file_t *file, udfs_ea_type_t ea_type,
                                              udfs_ea_info_t *ea_info) {
    uint8_t *ea_space, *current_ea, *ea_end;
    uint32_t ea_length;
    udfs_ea_info_t current_ea_info;
    
    if (!file || !ea_info) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    if (!file->volume || !file->volume->is_mounted || !file->node) {
        return UDFS_ERROR_NOT_MOUNTED;
    }
    
    /* Initialize output */
    memset(ea_info, 0, sizeof(udfs_ea_info_t));
    ea_info->type = ea_type;
    strncpy(ea_info->name, get_ea_name(ea_type), sizeof(ea_info->name) - 1);
    ea_info->name[sizeof(ea_info->name) - 1] = '\0';
    ea_info->available = false;
    
    /* Get extended attributes space */
    ea_space = get_extended_attributes(file->node, &ea_length);
    if (!ea_space || ea_length == 0) {
        debug_print("No extended attributes found");
        return UDFS_OK; /* Return info with available=false */
    }
    
    current_ea = ea_space;
    ea_end = ea_space + ea_length;
    
    /* Search for the requested EA type */
    while (current_ea) {
        current_ea = parse_next_ea(current_ea, ea_end, &current_ea_info);
        if (!current_ea) {
            break; /* No more EAs or parse error */
        }
        
        if (current_ea_info.type == ea_type) {
            /* Found the requested EA */
            *ea_info = current_ea_info;
            debug_print("Found EA info: type=%d, length=%u, available=true", 
                       ea_type, ea_info->length);
            return UDFS_OK;
        }
    }
    
    debug_print("Extended attribute type %d not found", ea_type);
    return UDFS_OK; /* Return info with available=false */
}

/*
 * Multi-Session Support (Phase 3)
 */

udfs_result_t udfs_get_session_count(udfs_volume_t *volume, uint32_t *session_count) {
    if (!volume || !session_count) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    if (!volume->is_mounted || !volume->device) {
        return UDFS_ERROR_NOT_MOUNTED;
    }
    
    *session_count = volume->device->mediumInfo.numberOfSessions;
    debug_print("Volume has %u sessions", *session_count);
    
    return UDFS_OK;
}

udfs_result_t udfs_get_session_info(udfs_volume_t *volume, uint32_t session_index,
                                   udfs_session_info_t *session_info) {
    uint32_t session_count;
    uint32_t start_block, next_start_block;
    
    if (!volume || !session_info) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    if (!volume->is_mounted || !volume->device) {
        return UDFS_ERROR_NOT_MOUNTED;
    }
    
    session_count = volume->device->mediumInfo.numberOfSessions;
    if (session_index >= session_count) {
        debug_print("Session index %u out of range (0-%u)", session_index, session_count - 1);
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    if (!volume->device->mediumInfo.sessionStartBlocks) {
        debug_print("No session start blocks available");
        return UDFS_ERROR_NOT_SUPPORTED;
    }
    
    /* Get session information */
    start_block = volume->device->mediumInfo.sessionStartBlocks[session_index];
    
    /* Calculate session size */
    if (session_index + 1 < session_count) {
        next_start_block = volume->device->mediumInfo.sessionStartBlocks[session_index + 1];
    } else {
        /* Last session goes to end of medium */
        next_start_block = volume->device->mediumInfo.lastValidBlockNr + 1;
    }
    
    /* Fill session info */
    session_info->session_number = session_index;
    session_info->start_block = start_block;
    session_info->total_blocks = next_start_block - start_block;
    session_info->is_verify_session = (session_index == volume->device->mediumInfo.verifySession);
    
    debug_print("Session %u: start=%u, blocks=%u, verify=%s", 
                session_index, start_block, session_info->total_blocks,
                session_info->is_verify_session ? "yes" : "no");
    
    return UDFS_OK;
}

udfs_result_t udfs_list_sessions(udfs_volume_t *volume, udfs_session_info_t *session_list,
                                size_t max_sessions, size_t *actual_sessions) {
    uint32_t session_count;
    size_t i;
    udfs_result_t result;
    
    if (!volume || !session_list || !actual_sessions || max_sessions == 0) {
        return UDFS_ERROR_INVALID_PARAM;
    }
    
    if (!volume->is_mounted || !volume->device) {
        return UDFS_ERROR_NOT_MOUNTED;
    }
    
    /* Get session count */
    result = udfs_get_session_count(volume, &session_count);
    if (result != UDFS_OK) {
        return result;
    }
    
    /* Fill session list */
    *actual_sessions = MIN(session_count, max_sessions);
    
    for (i = 0; i < *actual_sessions; i++) {
        result = udfs_get_session_info(volume, (uint32_t)i, &session_list[i]);
        if (result != UDFS_OK) {
            debug_print("Failed to get info for session %zu", i);
            *actual_sessions = i; /* Return partial list */
            return result;
        }
    }
    
    debug_print("Listed %zu of %u sessions", *actual_sessions, session_count);
    return UDFS_OK;
}