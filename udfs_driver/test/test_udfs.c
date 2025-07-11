/* UDFS Driver Test Program
 * Basic unit tests for the UDFS library
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "udfs.h"

/* Test counter */
static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name) \
    do { \
        printf("Running test: %s... ", name); \
        fflush(stdout); \
        tests_run++; \
    } while(0)

#define PASS() \
    do { \
        printf("PASS\n"); \
        tests_passed++; \
    } while(0)

#define FAIL(msg) \
    do { \
        printf("FAIL: %s\n", msg); \
    } while(0)

void test_version(void) {
    TEST("udfs_get_version");
    const char *version = udfs_get_version();
    if (version && strlen(version) > 0) {
        PASS();
    } else {
        FAIL("version string is empty or null");
    }
}

void test_strerror(void) {
    TEST("udfs_strerror");
    const char *msg = udfs_strerror(UDFS_OK);
    if (msg && strlen(msg) > 0) {
        PASS();
    } else {
        FAIL("error message is empty or null");
    }
}

void test_invalid_params(void) {
    TEST("invalid parameter handling");
    udfs_volume_t *volume = NULL;
    udfs_result_t result;
    
    /* Test NULL parameters */
    result = udfs_mount_image(NULL, &volume);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("mount_image should fail with NULL path");
        return;
    }
    
    result = udfs_mount_image("test", NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("mount_image should fail with NULL volume pointer");
        return;
    }
    
    result = udfs_unmount(NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("unmount should fail with NULL volume");
        return;
    }
    
    PASS();
}

void test_nonexistent_file(void) {
    TEST("mounting nonexistent file");
    udfs_volume_t *volume = NULL;
    udfs_result_t result = udfs_mount_image("/nonexistent/file.iso", &volume);
    
    /* Should fail, but not crash */
    if (result == UDFS_OK) {
        FAIL("should not successfully mount nonexistent file");
        udfs_unmount(volume);
    } else {
        PASS();
    }
}

void test_debug_toggle(void) {
    TEST("debug toggle");
    /* Should not crash */
    udfs_set_debug(true);
    udfs_set_debug(false);
    PASS();
}

void test_path_operations(void) {
    TEST("path operations with invalid parameters");
    udfs_volume_t *volume = NULL;
    udfs_file_info_t info;
    udfs_result_t result;
    
    /* Test stat with NULL parameters */
    result = udfs_stat(NULL, "/", &info);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("stat should fail with NULL volume");
        return;
    }
    
    result = udfs_stat(volume, NULL, &info);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("stat should fail with NULL path");
        return;
    }
    
    result = udfs_stat(volume, "/", NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("stat should fail with NULL info");
        return;
    }
    
    /* Test exists with NULL parameters */
    if (udfs_exists(NULL, "/")) {
        FAIL("exists should return false with NULL volume");
        return;
    }
    
    if (udfs_exists(volume, NULL)) {
        FAIL("exists should return false with NULL path");
        return;
    }
    
    PASS();
}

void test_file_operations(void) {
    TEST("file operations with invalid parameters");
    udfs_volume_t *volume = NULL;
    udfs_file_t *file = NULL;
    udfs_result_t result;
    char buffer[100];
    size_t bytes_read;
    
    /* Test open_file with NULL parameters */
    result = udfs_open_file(NULL, "/test", &file);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("open_file should fail with NULL volume");
        return;
    }
    
    result = udfs_open_file(volume, NULL, &file);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("open_file should fail with NULL path");
        return;
    }
    
    result = udfs_open_file(volume, "/test", NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("open_file should fail with NULL file pointer");
        return;
    }
    
    /* Test read_file with NULL parameters */
    result = udfs_read_file(NULL, buffer, sizeof(buffer), &bytes_read);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("read_file should fail with NULL file");
        return;
    }
    
    result = udfs_read_file(file, NULL, sizeof(buffer), &bytes_read);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("read_file should fail with NULL buffer");
        return;
    }
    
    result = udfs_read_file(file, buffer, sizeof(buffer), NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("read_file should fail with NULL bytes_read");
        return;
    }
    
    /* Test seek_file with NULL parameters */
    result = udfs_seek_file(NULL, 0);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("seek_file should fail with NULL file");
        return;
    }
    
    /* Test close_file with NULL parameters */
    result = udfs_close_file(NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("close_file should fail with NULL file");
        return;
    }
    
    PASS();
}

void test_extended_attributes(void) {
    TEST("extended attributes with invalid parameters");
    udfs_file_t *file = NULL;
    udfs_ea_info_t ea_list[10];
    udfs_ea_info_t ea_info;
    size_t count;
    size_t data_size;
    char buffer[256];
    udfs_result_t result;
    
    /* Test list_extended_attributes with NULL parameters */
    result = udfs_list_extended_attributes(NULL, ea_list, 10, &count);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("list_extended_attributes should fail with NULL file");
        return;
    }
    
    result = udfs_list_extended_attributes(file, NULL, 10, &count);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("list_extended_attributes should fail with NULL ea_list");
        return;
    }
    
    result = udfs_list_extended_attributes(file, ea_list, 10, NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("list_extended_attributes should fail with NULL count");
        return;
    }
    
    result = udfs_list_extended_attributes(file, ea_list, 0, &count);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("list_extended_attributes should fail with zero max_count");
        return;
    }
    
    /* Test read_extended_attribute with NULL parameters */
    result = udfs_read_extended_attribute(NULL, UDFS_EA_FILE_TIMES, buffer, sizeof(buffer), &data_size);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("read_extended_attribute should fail with NULL file");
        return;
    }
    
    result = udfs_read_extended_attribute(file, UDFS_EA_FILE_TIMES, NULL, sizeof(buffer), &data_size);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("read_extended_attribute should fail with NULL buffer");
        return;
    }
    
    result = udfs_read_extended_attribute(file, UDFS_EA_FILE_TIMES, buffer, sizeof(buffer), NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("read_extended_attribute should fail with NULL data_size");
        return;
    }
    
    result = udfs_read_extended_attribute(file, UDFS_EA_FILE_TIMES, buffer, 0, &data_size);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("read_extended_attribute should fail with zero buffer_size");
        return;
    }
    
    /* Test get_extended_attribute_info with NULL parameters */
    result = udfs_get_extended_attribute_info(NULL, UDFS_EA_FILE_TIMES, &ea_info);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("get_extended_attribute_info should fail with NULL file");
        return;
    }
    
    result = udfs_get_extended_attribute_info(file, UDFS_EA_FILE_TIMES, NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("get_extended_attribute_info should fail with NULL ea_info");
        return;
    }
    
    PASS();
}

void test_multisession_operations(void) {
    TEST("multi-session operations with invalid parameters");
    udfs_volume_t *volume = NULL;
    udfs_session_info_t session_info;
    udfs_session_info_t session_list[10];
    uint32_t session_count;
    size_t actual_sessions;
    udfs_result_t result;
    
    /* Test get_session_count with NULL parameters */
    result = udfs_get_session_count(NULL, &session_count);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("get_session_count should fail with NULL volume");
        return;
    }
    
    result = udfs_get_session_count(volume, NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("get_session_count should fail with NULL session_count");
        return;
    }
    
    /* Test get_session_info with NULL parameters */
    result = udfs_get_session_info(NULL, 0, &session_info);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("get_session_info should fail with NULL volume");
        return;
    }
    
    result = udfs_get_session_info(volume, 0, NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("get_session_info should fail with NULL session_info");
        return;
    }
    
    /* Test list_sessions with NULL parameters */
    result = udfs_list_sessions(NULL, session_list, 10, &actual_sessions);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("list_sessions should fail with NULL volume");
        return;
    }
    
    result = udfs_list_sessions(volume, NULL, 10, &actual_sessions);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("list_sessions should fail with NULL session_list");
        return;
    }
    
    result = udfs_list_sessions(volume, session_list, 10, NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("list_sessions should fail with NULL actual_sessions");
        return;
    }
    
    result = udfs_list_sessions(volume, session_list, 0, &actual_sessions);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("list_sessions should fail with zero max_sessions");
        return;
    }
    
    PASS();
}

void test_directory_operations(void) {
    TEST("directory operations with invalid parameters");
    udfs_volume_t *volume = NULL;
    udfs_dir_t *dir = NULL;
    udfs_result_t result;
    udfs_dir_entry_t entry;
    
    /* Test open_dir with NULL parameters */
    result = udfs_open_dir(NULL, "/", &dir);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("open_dir should fail with NULL volume");
        return;
    }
    
    result = udfs_open_dir(volume, NULL, &dir);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("open_dir should fail with NULL path");
        return;
    }
    
    result = udfs_open_dir(volume, "/", NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("open_dir should fail with NULL dir pointer");
        return;
    }
    
    /* Test read_dir with NULL parameters */
    result = udfs_read_dir(NULL, &entry);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("read_dir should fail with NULL dir");
        return;
    }
    
    result = udfs_read_dir(dir, NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("read_dir should fail with NULL entry");
        return;
    }
    
    /* Test rewind_dir with NULL parameters */
    result = udfs_rewind_dir(NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("rewind_dir should fail with NULL dir");
        return;
    }
    
    /* Test close_dir with NULL parameters */
    result = udfs_close_dir(NULL);
    if (result != UDFS_ERROR_INVALID_PARAM) {
        FAIL("close_dir should fail with NULL dir");
        return;
    }
    
    PASS();
}

int main(int argc, char *argv[]) {
    printf("UDFS Driver Test Suite\n");
    printf("======================\n\n");
    
    /* Run basic functionality tests */
    test_version();
    test_strerror();
    test_invalid_params();
    test_nonexistent_file();
    test_debug_toggle();
    test_path_operations();
    test_file_operations();
    test_extended_attributes();
    test_multisession_operations();
    test_directory_operations();
    
    /* If a UDF image is provided, test mounting it */
    if (argc >= 2) {
        TEST("mounting provided UDF image");
        udfs_volume_t *volume = NULL;
        udfs_result_t result = udfs_mount_image(argv[1], &volume);
        
        if (result == UDFS_OK) {
            printf("PASS (mounted successfully)\n");
            tests_passed++;
            
            /* Test volume info */
            TEST("getting volume info");
            char label[256];
            uint64_t total_size, free_size;
            result = udfs_get_volume_info(volume, label, sizeof(label), &total_size, &free_size);
            if (result == UDFS_OK || result == UDFS_ERROR_NOT_SUPPORTED) {
                PASS();
            } else {
                FAIL("get_volume_info failed unexpectedly");
            }
            
            /* Test unmount */
            TEST("unmounting volume");
            result = udfs_unmount(volume);
            if (result == UDFS_OK) {
                PASS();
            } else {
                FAIL("unmount failed");
            }
            
            /* Test new Phase 2 functionality */
            TEST("remounting for Phase 2 tests");
            result = udfs_mount_image(argv[1], &volume);
            if (result == UDFS_OK) {
                printf("PASS\n");
                tests_passed++;
                
                /* Test session information - Phase 3 feature */
                TEST("testing session information");
                uint32_t session_count;
                result = udfs_get_session_count(volume, &session_count);
                if (result == UDFS_OK) {
                    printf("PASS (found %u sessions)\n", session_count);
                    tests_passed++;
                    
                    if (session_count > 0) {
                        TEST("testing session details");
                        udfs_session_info_t session_list[10];
                        size_t actual_sessions;
                        result = udfs_list_sessions(volume, session_list, 10, &actual_sessions);
                        if (result == UDFS_OK) {
                            printf("PASS (listed %zu sessions)\n", actual_sessions);
                            tests_passed++;
                            
                            /* Display session information */
                            for (size_t i = 0; i < actual_sessions; i++) {
                                printf("  Session %u: start_block=%u, total_blocks=%u, verify=%s\n",
                                       session_list[i].session_number,
                                       session_list[i].start_block,
                                       session_list[i].total_blocks,
                                       session_list[i].is_verify_session ? "yes" : "no");
                            }
                        } else {
                            printf("SKIP (%s)\n", udfs_strerror(result));
                        }
                    }
                } else {
                    printf("SKIP (%s)\n", udfs_strerror(result));
                }
                
                /* Test path operations */
                TEST("testing root directory stat");
                udfs_file_info_t info;
                result = udfs_stat(volume, "/", &info);
                if (result == UDFS_OK) {
                    printf("PASS (type=%d, name='%s')\n", info.type, info.name);
                    tests_passed++;
                } else {
                    printf("SKIP (%s)\n", udfs_strerror(result));
                }
                
                /* Test root directory existence */
                TEST("testing root directory existence");
                if (udfs_exists(volume, "/")) {
                    PASS();
                } else {
                    FAIL("root directory should exist");
                }
                
                /* Test directory operations */
                TEST("opening root directory");
                udfs_dir_t *dir;
                result = udfs_open_dir(volume, "/", &dir);
                if (result == UDFS_OK) {
                    printf("PASS\n");
                    tests_passed++;
                    
                    /* Test reading directory entries */
                    TEST("reading directory entries");
                    udfs_dir_entry_t entry;
                    int entry_count = 0;
                    while (true) {
                        result = udfs_read_dir(dir, &entry);
                        if (result != UDFS_OK) {
                            printf("FAIL (read_dir error: %s)\n", udfs_strerror(result));
                            break;
                        }
                        if (!entry.valid) {
                            break; /* End of directory */
                        }
                        entry_count++;
                        printf("  Entry: %s (type=%d, size=%llu)\n", 
                               entry.info.name, entry.info.type, 
                               (unsigned long long)entry.info.size);
                        
                        /* Try to open first file we find */
                        if (entry.info.type == UDFS_TYPE_FILE && entry_count == 1) {
                            TEST("opening first file found");
                            char full_path[512];
                            snprintf(full_path, sizeof(full_path), "/%s", entry.info.name);
                            
                            udfs_file_t *file;
                            result = udfs_open_file(volume, full_path, &file);
                            if (result == UDFS_OK) {
                                printf("PASS\n");
                                tests_passed++;
                                
                                /* Test file reading */
                                TEST("reading from file");
                                char read_buffer[256];
                                size_t bytes_read;
                                result = udfs_read_file(file, read_buffer, sizeof(read_buffer), &bytes_read);
                                if (result == UDFS_OK) {
                                    printf("PASS (read %zu bytes)\n", bytes_read);
                                    tests_passed++;
                                } else {
                                    printf("SKIP (%s)\n", udfs_strerror(result));
                                }
                                
                                /* Test extended attributes - Phase 3 feature */
                                TEST("testing extended attributes");
                                udfs_ea_info_t ea_list[10];
                                size_t ea_count;
                                result = udfs_list_extended_attributes(file, ea_list, 10, &ea_count);
                                if (result == UDFS_OK) {
                                    if (ea_count > 0) {
                                        printf("PASS (found %zu extended attributes)\n", ea_count);
                                        tests_passed++;
                                        
                                        /* List the extended attributes */
                                        for (size_t i = 0; i < ea_count; i++) {
                                            printf("  EA %zu: %s (type=%d, length=%u, available=%s)\n",
                                                   i, ea_list[i].name, ea_list[i].type, 
                                                   ea_list[i].length, ea_list[i].available ? "yes" : "no");
                                            
                                            /* Try to read the first EA */
                                            if (i == 0) {
                                                TEST("reading first extended attribute");
                                                char ea_buffer[512];
                                                size_t ea_data_size;
                                                result = udfs_read_extended_attribute(file, ea_list[i].type, 
                                                                                    ea_buffer, sizeof(ea_buffer), &ea_data_size);
                                                if (result == UDFS_OK) {
                                                    printf("PASS (read %zu bytes of EA data)\n", ea_data_size);
                                                    tests_passed++;
                                                } else {
                                                    printf("SKIP (%s)\n", udfs_strerror(result));
                                                }
                                            }
                                        }
                                    } else {
                                        printf("PASS (no extended attributes found)\n");
                                        tests_passed++;
                                    }
                                } else {
                                    printf("SKIP (%s)\n", udfs_strerror(result));
                                }
                                
                                /* Test file seeking */
                                TEST("seeking in file");
                                result = udfs_seek_file(file, 0);
                                if (result == UDFS_OK) {
                                    PASS();
                                } else {
                                    printf("FAIL (%s)\n", udfs_strerror(result));
                                }
                                
                                /* Test file position */
                                TEST("getting file position");
                                uint64_t position;
                                result = udfs_tell_file(file, &position);
                                if (result == UDFS_OK) {
                                    printf("PASS (position=%llu)\n", (unsigned long long)position);
                                    tests_passed++;
                                } else {
                                    printf("FAIL (%s)\n", udfs_strerror(result));
                                }
                                
                                /* Close file */
                                udfs_close_file(file);
                            } else {
                                printf("SKIP (%s)\n", udfs_strerror(result));
                            }
                        }
                        
                        if (entry_count >= 10) break; /* Limit output */
                    }
                    
                    if (entry_count > 0) {
                        printf("PASS (found %d entries)\n", entry_count);
                        tests_passed++;
                    } else {
                        printf("SKIP (no entries found)\n");
                    }
                    
                    /* Close directory */
                    udfs_close_dir(dir);
                } else {
                    printf("SKIP (%s)\n", udfs_strerror(result));
                }
                
                /* Final unmount */
                udfs_unmount(volume);
            } else {
                printf("SKIP (remount failed: %s)\n", udfs_strerror(result));
            }
        } else {
            printf("SKIP (mount failed: %s)\n", udfs_strerror(result));
        }
    }
    
    printf("\n======================\n");
    printf("Tests completed: %d/%d passed\n", tests_passed, tests_run);
    
    if (tests_passed == tests_run) {
        printf("All tests PASSED!\n");
        return 0;
    } else {
        printf("Some tests FAILED!\n");
        return 1;
    }
}