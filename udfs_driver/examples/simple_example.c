/* Simple UDFS Driver Example
 * Demonstrates basic usage of the UDFS library
 */

#include <stdio.h>
#include <stdlib.h>
#include "udfs.h"

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <udf_image_file>\n", argv[0]);
        return 1;
    }
    
    const char *image_path = argv[1];
    udfs_volume_t *volume = NULL;
    udfs_result_t result;
    
    printf("UDFS Driver Example - Version %s\n", udfs_get_version());
    printf("Attempting to mount UDF image: %s\n", image_path);
    
    /* Enable debug output */
    udfs_set_debug(true);
    
    /* Try to mount the UDF volume */
    result = udfs_mount_image(image_path, &volume);
    if (result != UDFS_OK) {
        fprintf(stderr, "Failed to mount UDF image: %s\n", udfs_strerror(result));
        return 1;
    }
    
    printf("Successfully mounted UDF volume!\n");
    
    /* Get volume information */
    char label[256];
    uint64_t total_size, free_size;
    result = udfs_get_volume_info(volume, label, sizeof(label), &total_size, &free_size);
    if (result == UDFS_OK) {
        printf("Volume Label: %s\n", label);
        printf("Total Size: %llu bytes\n", (unsigned long long)total_size);
        printf("Free Size: %llu bytes\n", (unsigned long long)free_size);
    }
    
    /* TODO: Add directory listing and file reading examples when implemented */
    
    /* Unmount the volume */
    result = udfs_unmount(volume);
    if (result != UDFS_OK) {
        fprintf(stderr, "Warning: Failed to unmount volume: %s\n", udfs_strerror(result));
    } else {
        printf("Volume unmounted successfully.\n");
    }
    
    return 0;
}