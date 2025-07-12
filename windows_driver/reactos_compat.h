/*
 * ReactOS Build Environment Compatibility Header
 * 
 * This header provides specific compatibility fixes for ReactOS Build Environment 2.2.1
 * and addresses common compilation issues when building kernel drivers.
 */

#ifndef _REACTOS_COMPAT_H_
#define _REACTOS_COMPAT_H_

/* Detect ReactOS build environment */
#if defined(REACTOS) || defined(__REACTOS__) || defined(ROS_BUILD) || defined(_REACTOS_)
#define UDF_REACTOS_BUILD 1

/* ReactOS-specific includes and definitions */
#ifdef __cplusplus
extern "C" {
#endif

/* Ensure proper architecture detection for ReactOS */
#ifndef _M_IX86
#if defined(__i386__) || defined(i386) || defined(_X86_)
#define _M_IX86 1
#endif
#endif

#ifndef _M_AMD64
#if defined(__x86_64__) || defined(__amd64__) || defined(_AMD64_)
#define _M_AMD64 1
#endif
#endif

/* ReactOS GCC-specific attribute fixes */
#ifdef __GNUC__
#ifndef FORCEINLINE
#define FORCEINLINE static inline __attribute__((always_inline))
#endif

#ifndef DECLSPEC_ALIGN
#define DECLSPEC_ALIGN(x) __attribute__((aligned(x)))
#endif
#endif

/* ReactOS memory allocation tag format */
#ifndef UDFS_TAG
#define UDFS_TAG 'sfdU'
#endif

/* ReactOS debug output compatibility */
#ifndef DbgPrintEx
#define DbgPrintEx(Component, Level, Format, ...) DbgPrint(Format, ##__VA_ARGS__)
#endif

#ifdef __cplusplus
}
#endif

#endif /* UDF_REACTOS_BUILD */

#endif /* _REACTOS_COMPAT_H_ */