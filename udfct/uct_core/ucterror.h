/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : ucterror.h
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#ifndef __UCT_UCTERROR_H__
#define __UCT_UCTERROR_H__

#include "uctnodes.h"   /* for UdfMountContext */


extern char uctErrorMessage[4096];

/* to be used outside the uct_core library :
 */
extern void clearUctErrorMessage();
extern bool printAndClearUctErrorMessage(char *extraText);
/*
 * Print and clear uctErrorMessage[] if it is not cleared already.
 * return value:
 *       TRUE if an error message + extraText was printed
 *               and uctErrorMessage was cleared.
 * else: FALSE   (nothing printed)
 */


/* Print standard formatted messages *********************************
 */

/* print standard message header
 * arguments:
 *  Uint8 vLevel: verbose level.
 *  Byte *d     : points to begin of descriptor
 *  Byte *pBP   : points to structure within descriptor,
 *              : it is used to print byte position BP.
 *  char *extra : extra text
 * all pointer arguments can be NULL.
 */
extern void printMessageHead(Uint8 vLevel, Byte *d, Byte *pBP, char *extra);

/* Print standard formatted message in case a descriptor
 * field value does not have the value as expected.
 *
 * Uint8  vLevel     : verbose level
 * Byte  *d          : pointer to begin of descriptor
 * Byte  *pBP        : pointer to start of error field.
 * char  *text       : E.g. "Error: <value name>"
 *                      or: "Warning: <value name>"
 * char  *valueFormat: value fprintf format, e.g. "#02X"
 * Uint32 value      : field value
 * Uint32 expected   : expected value
 * char  *extraText  : extra text at end of error message,
 *                     maybe NULL or "\n".
 *
 * Note that e.g. if extraText == NULL, no \n is printed at the end
 */
extern void printMessageExpectedU32(
                Uint8 vLevel, Byte *d, Byte *pBP,
                char *text, char *valueFormat,
                Uint32 value, Uint32 expected,
                char *extraText );


#ifdef  DEBUG01

/* implementationNotStarted:
 * Useful to generate a list of unimplemented functions
 * using e.g.:
 *  MLIMITbegin(DEBUG01level, MLIMITdefault01);
 *      implementationNotStarted( "<structure or descriptor name>" );
 *  MLIMITend;
 */
extern void implementationNotStarted(char *name);

/* implementationNotReady:
 * see implementationNotStarted(), but "ready" instead of "started".
 */
extern void implementationNotReady(char *name);

/* implementationNotTested:
 * see implementationNotStarted(), but "tested" instead of "started".
 */
extern void implementationNotTested(char *name);

#endif  /* DEBUG01 */

/* error handling **********************************
 * uctExit() function and error codes.
 * (fatal) error if exitCode >= EXIT_FIRST_FATAL_VALUE
 * Exit code definitions:
 * 00    : verification complete, no errors, no warnings.
 * 01    : Verification complete with warnings, no errors.
 : 02    : verification complete with errors.
 : 03-99 : verification incomplete, fatal errors
 */
#define EXIT_OK                  0   /* vComplete, no warn, no err   */
#define EXIT_WITH_WARN_NOERR     1   /* vComplete with warn, no err  */
#define EXIT_WITH_ERR            2   /* vComplete with errors        */
#define EXIT_RESERVED            3   /* 3-9, reserved values         */
/* Fatal errors, verification could not be completed for reason of:
 */
#define EXIT_INCOMPLETE_FATAL   10   /* fatal error, ... */
#define EXIT_FIRST_FATAL_VALUE  EXIT_INCOMPLETE_FATAL
#define EXIT_NON_CONFORMANT     11   /* fatal error, ... */
#define EXIT_PROGRAM_ERROR      12   /* fatal error, ... */
#define EXIT_COMMANDLINE_ERROR  13   /* fatal error, ... */
#define EXIT_OUT_OF_MEMORY      14   /* fatal error, ... */
#define EXIT_DEVICE_NOT_READY   15   /* fatal error, ... */
#define EXIT_UCT_ASSERT_FAILED  16   /* fatal error, ... */
#define EXIT_QUIT               99   /* fatal error, ... */

extern void uctExit( Uint32 exitCode );

/* uctAssert(): use via UCTASSERT(CON) macro
 * 'own' uct assert function.
 * uctExit() if condition FALSE
 */
extern void uctAssert(bool condition, char *file, int line);
#define UCTASSERT(CON) uctAssert((CON), __FILE__, __LINE__)

/* printReadErrorBlocks()
 * assume that cntSkip is included in cntErr,
 * so: cntErr >= cntSkip
 */
extern void printReadErrorBlocks(Uint32 cntErr, Uint32 cntSkip,
                                 Uint32 blockAddress);


#endif /* __UCT_UCTERROR_H__ */

