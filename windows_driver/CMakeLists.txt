# UDF File System Driver for Windows/ReactOS
# Based on UDFCT (UDF Conformance Testing Application)
# Compatible with ReactOS build environment

list(APPEND SOURCE
    udfs.c
    create.c
    read.c
    write.c
    dirctrl.c
    fileinfo.c
    volinfo.c
    fsctrl.c
    cleanup.c
    close.c
    udfct_windows.c
    udfsprocs.h)

# Include UDFCT core components needed for kernel mode
list(APPEND UDFCT_SOURCE
    ../udfct/uct_core/crc.c
    ../udfct/uct_core/device.c
    ../udfct/uct_core/general.c
    ../udfct/uct_core/uctallocation.c
    ../udfct/uct_core/uctendian.c
    ../udfct/uct_core/ucterror.c
    ../udfct/uct_core/uctfarapart.c
    ../udfct/uct_core/uctfiles.c
    ../udfct/uct_core/uctgeneral.c
    ../udfct/uct_core/uctmedium.c
    ../udfct/uct_core/uctnodes.c
    ../udfct/uct_core/uctstatus.c
    ../udfct/uct_core/ucttimestamp.c
    ../udfct/uct_core/uctverify.c
    ../udfct/uct_core/unicode.c)

# Create the kernel mode driver
add_library(udfs MODULE ${SOURCE} ${UDFCT_SOURCE} udfs.rc)

# Set as kernel mode driver
set_module_type(udfs kernelmodedriver)

# Include directories for UDFCT headers
target_include_directories(udfs PRIVATE 
    ../udfct/uct_core
    ../udfct/udf_scsi)

# Compiler definitions for kernel mode and UDF support
target_compile_definitions(udfs PRIVATE
    -DKERNEL_MODE=1
    -D_FILE_OFFSET_BITS=64
    -D_LARGEFILE_SOURCE
    -D__USE_LARGEFILE64
    -DUDF_KERNEL_DRIVER=1)

# Link with required kernel libraries
target_link_libraries(udfs ${PSEH_LIB})

# Import required kernel libraries
add_importlibs(udfs ntoskrnl hal)

# Set precompiled header
add_pch(udfs udfsprocs.h SOURCE)

# Install to drivers directory
add_cd_file(TARGET udfs DESTINATION reactos/system32/drivers NO_CAB FOR all)