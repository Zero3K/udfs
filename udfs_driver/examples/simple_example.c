/* Simple UDFS Driver Example
 * Demonstrates basic usage of the UDFS library
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
    
    /* Demonstrate Phase 2 functionality - directory listing */
    printf("\n--- Directory Listing (/) ---\n");
    udfs_dir_t *dir;
    result = udfs_open_dir(volume, "/", &dir);
    if (result == UDFS_OK) {
        udfs_dir_entry_t entry;
        int count = 0;
        
        while (true) {
            result = udfs_read_dir(dir, &entry);
            if (result != UDFS_OK) {
                fprintf(stderr, "Error reading directory: %s\n", udfs_strerror(result));
                break;
            }
            
            if (!entry.valid) {
                break; /* End of directory */
            }
            
            const char *type_str = "unknown";
            switch (entry.info.type) {
                case UDFS_TYPE_FILE: type_str = "file"; break;
                case UDFS_TYPE_DIRECTORY: type_str = "dir"; break;
                case UDFS_TYPE_SYMLINK: type_str = "link"; break;
            }
            
            printf("  %-20s %8s %12llu bytes\n", 
                   entry.info.name, type_str, 
                   (unsigned long long)entry.info.size);
            
            count++;
            if (count >= 20) {
                printf("  ... (showing first 20 entries)\n");
                break;
            }
        }
        
        if (count == 0) {
            printf("  (no entries found)\n");
        } else {
            printf("  Total: %d entries\n", count);
        }
        
        udfs_close_dir(dir);
    } else {
        printf("Failed to open root directory: %s\n", udfs_strerror(result));
    }
    
    /* Demonstrate file operations - try to read a small file */
    printf("\n--- File Reading Demo ---\n");
    udfs_file_info_t info;
    
    /* Try some common file paths */
    const char *test_paths[] = {
        "/README.TXT", "/README.txt", "/readme.txt", 
        "/AUTORUN.INF", "/autorun.inf",
        "/INDEX.HTM", "/index.htm", "/INDEX.HTML", "/index.html",
        NULL
    };
    
    for (int i = 0; test_paths[i]; i++) {
        if (udfs_exists(volume, test_paths[i])) {
            printf("Found file: %s\n", test_paths[i]);
            
            result = udfs_stat(volume, test_paths[i], &info);
            if (result == UDFS_OK) {
                printf("  Size: %llu bytes\n", (unsigned long long)info.size);
                
                if (info.size > 0 && info.size < 1024) {
                    /* Read small files completely */
                    udfs_file_t *file;
                    result = udfs_open_file(volume, test_paths[i], &file);
                    if (result == UDFS_OK) {
                        char *buffer = malloc(info.size + 1);
                        if (buffer) {
                            size_t bytes_read;
                            result = udfs_read_file(file, buffer, info.size, &bytes_read);
                            if (result == UDFS_OK) {
                                buffer[bytes_read] = '\0';
                                printf("  Content preview:\n");
                                printf("  ----------------------------------------\n");
                                /* Print first few lines only */
                                char *line = strtok(buffer, "\n\r");
                                int line_count = 0;
                                while (line && line_count < 10) {
                                    printf("  %s\n", line);
                                    line = strtok(NULL, "\n\r");
                                    line_count++;
                                }
                                if (line) {
                                    printf("  ... (truncated)\n");
                                }
                                printf("  ----------------------------------------\n");
                            }
                            free(buffer);
                        }
                        udfs_close_file(file);
                    }
                } else if (info.size > 0) {
                    /* For larger files, just read the beginning */
                    udfs_file_t *file;
                    result = udfs_open_file(volume, test_paths[i], &file);
                    if (result == UDFS_OK) {
                        char buffer[256];
                        size_t bytes_read;
                        result = udfs_read_file(file, buffer, sizeof(buffer) - 1, &bytes_read);
                        if (result == UDFS_OK && bytes_read > 0) {
                            buffer[bytes_read] = '\0';
                            printf("  First %zu bytes:\n", bytes_read);
                            printf("  ----------------------------------------\n");
                            printf("  %s\n", buffer);
                            printf("  ----------------------------------------\n");
                        }
                        udfs_close_file(file);
                    }
                }
            }
            break; /* Only show first file found */
        }
    }
    
    /* Unmount the volume */
    result = udfs_unmount(volume);
    if (result != UDFS_OK) {
        fprintf(stderr, "Warning: Failed to unmount volume: %s\n", udfs_strerror(result));
    } else {
        printf("Volume unmounted successfully.\n");
    }
    
    return 0;
}