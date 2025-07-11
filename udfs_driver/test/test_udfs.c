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

int main(int argc, char *argv[]) {
    printf("UDFS Driver Test Suite\n");
    printf("======================\n\n");
    
    /* Run basic functionality tests */
    test_version();
    test_strerror();
    test_invalid_params();
    test_nonexistent_file();
    test_debug_toggle();
    
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