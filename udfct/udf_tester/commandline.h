/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : commandline.h
 *
 * Description : General command line parsing functions.
 *
 * Author(s)   : Gerrit Scholl
 */

#ifndef __COMMANDLINE_H__
#define __COMMANDLINE_H__

#include "uct_core.h"


/* exported functions ************************************************
 */

extern void genericUsage();
extern void mediumOptionsUsage();

/* explanation printed with -help option only
 */
extern void extraHelpUsage();

/* print headText and valid arguments.
 * argv[0] is not printed. For any other contiguous sequence
 * of NULL arguments (argv[n]==NULL), 3 dots are printed.
 * Further one space is printed before each argument,
 * however if the line becomes too long "\n\t" is printed
 * instead of a space.
 */
extern void clPrintRemainingArguments(int argc, char **argv,
                                      char *headText);

/* Function clCountOptions:
 * argv[0] and arguments marked out by a NULL pointer are
 * excluded from the count.
 */
extern int clCountOptions(int argc, char **argv);

/* argv[0] and arguments marked out by a NULL pointer are
 * excluded from the count.
 */
extern int clCountValidArguments(int argc, char **argv);

/* Search for command line argument equal to searchArgument.
 * Skip argv[0] and arguments marked out by a NULL pointer.
 * First valid argument will match if searchArgument is
 * a NULL pointer.
 *
 * return value: int n
 *  n == 0 if searchArhument was NOT found, else searchArgument
 *  was found in argv[n].
 *  Calling program may use the found argument (and maybe the next one)
 *  and mark it out by: argv[n] = NULL (and maybe: argv[n+1] = NULL).
 */
extern int clFindArgument(int argc, char **argv, char *searchArgument);

/* Find index of next valid argument.
 * start searching at argv[index+1]
 * Skip argv[0] and arguments marked out by a NULL pointer.
 *
 * return value: int n
 *  n == 0 if searchArhument was NOT found, else searchArgument
 *  was found in argv[n].
 *  Calling program may use the found argument (and maybe the next one)
 *  and mark it out by: argv[n] = NULL (and maybe: argv[n+1] = NULL).
 */
extern int clNextArgument(int argc, char **argv, int index);

/* Parse and mark out option with Uint32 argument.
 */
extern int clParseOptionUint32( int argc, char **argv,
                                char *option, Uint32 *value);
/*
 * clParseOptionUint32() return values:
 */
#define OPT_NOT_FOUND   0
#define OPT_FOUND       1
#define OPT_ERROR       2

/* parse command line for generic options
 * Some options directly set global variables.
 */
typedef struct
{
    bool     helpOptionFound;
    bool     inspectImage;
    Uint32   imageStartBlock;
    char    *writeImagePath;
} genericOptions;

extern bool parseGenericOptions(int argc, char **argv,
                                genericOptions *gOptions);

/* parse command line for medium info options
 * Return value: TRUE if ok, else FALSE
 */
extern bool parseMediumOptions(int argc, char **argv,
                               MediumInfo *mediumInfo, bool *overruled);

#endif /* __COMMANDLINE_H__ */

