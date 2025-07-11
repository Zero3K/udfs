/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : ucterror.c
 *
 * Description :
 *
 * Author(s)   : Christ Vriens, Pieter-Bas IJdens, Gerrit Scholl
 */

#include <stdio.h>
#include <string.h>

#include "general.h"
#include "uctgeneral.h"     /* for uctout */
#include "uctfiles.h"
#include "ucterror.h"

char uctErrorMessage[4096];

extern void clearUctErrorMessage()
{
    uctErrorMessage[0] = '\0';
}

/* Print and clear uctErrorMessage[] if it is not cleared already.
 * return value:
 *       TRUE if an error message + extraText was printed
 *               and uctErrorMessage was cleared.
 * else: FALSE   (nothing printed)
 */
extern bool printAndClearUctErrorMessage(char *extraText)
{
    if( uctErrorMessage[0] != '\0' )
    {
        VERBOSE00(uctout, "\t%s\n%s", uctErrorMessage, extraText);
        uctErrorMessage[0] = '\0';
        return TRUE;
    }
    return FALSE;
}


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
extern void printMessageHead(Uint8 vLevel, Byte *d, Byte *pBP, char *extra)
{
    ifVERBOSE(vLevel)
    {   char *dTxt4;

        if( d == NULL )
        {   dTxt4 = NULL;
        }
        else if(   pBP != NULL  /* special for EAs after EAHD in EA Space */
                && ((Tag *)d)->tagIdentifier == tidEAHD
                && (pBP - d) >= sizeof(ExtendedAttributeHeaderDescriptor) )
        {   dTxt4 = "EASP";     /* instead of "EAHD" */
        }
        else
        {   dTxt4 = tidTEXT4(((Tag *)d)->tagIdentifier);    /* default */
        }

        /* print, mind -4x format in case of dTxt4 == NULL
         */
        if( d != NULL && pBP != NULL )  /* dTxt4 + BP */
        {
            fprintf(uctout, "\t%-4s %-3d ", dTxt4, pBP - d);
        }
        else                            /* dTxt4 only */
        {
            fprintf(uctout, "\t%-4s ", (dTxt4 != NULL) ? dTxt4 : "");
        }
        if( extra != NULL ) fprintf(uctout, "%s", extra);
    }
    ENDif;

}   /* end printMessageHead() */

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
                char *extraText )
{
    ifVERBOSE(vLevel)
    {
        printMessageHead(vLevel, d, pBP, text);
        fprintf(uctout, ": ");
        fprintf(uctout, valueFormat, value);
        fprintf(uctout, ", expected: ");
        fprintf(uctout, valueFormat, expected);
        if( extraText != NULL )
            fprintf(uctout, "%s", extraText);
    }
    ENDif;
}

#ifdef  DEBUG01
/* implementationNotStarted:
 * Useful to generate a list of unimplemented functions
 * using e.g.:
 *  MLIMITbegin(DEBUG01level, MLIMITdefault01);
 *      implementationNotStarted( "<structure or descriptor name>" );
 *  MLIMITend;
 */
extern void implementationNotStarted(char *name)
{
    fprintf(uctout,"\tDEBUG01: implementation not started: %s\n",name);
}

/* implementationNotReady:
 * see implementationNotStarted(), but "ready" instead of "started".
 */
extern void implementationNotReady(char *name)
{
    fprintf(uctout,"\tDEBUG01: implementation not ready: %s\n",name);
}

/* implementationNotTested:
 * see implementationNotStarted(), but "tested" instead of "started".
 */
extern void implementationNotTested(char *name)
{
    fprintf(uctout,"\tDEBUG01: implementation not tested: %s\n",name);
}
#endif  /* DEBUG01 */


/* error handling **********************************
 * uctExit(), fatal error if exitCode >= EXIT_FIRST_FATAL_VALUE
 * TODO: EXIT_DEVICE_NOT_READY: try to free memory for logging finish ??
 */
extern void uctExit( Uint32 exitCode )
{
    char *txt, *txt2;
    switch( exitCode )
    {
    case EXIT_OK:                txt = "complete, no errors, no warnings"; break;
    case EXIT_WITH_WARN_NOERR:   txt = "complete with warnings"; break;
    case EXIT_WITH_ERR:          txt = "complete with errors";   break;
    case EXIT_INCOMPLETE_FATAL:  txt = "incomplete verfication"; break;
    case EXIT_NON_CONFORMANT:    txt = "not conformant";         break;
    case EXIT_PROGRAM_ERROR:     txt = "program error";          break;
    case EXIT_COMMANDLINE_ERROR: txt = "command line error";     break;
    case EXIT_OUT_OF_MEMORY:     txt = "out of memory";          break;
    case EXIT_DEVICE_NOT_READY:  txt = "device not ready";       break;
    case EXIT_UCT_ASSERT_FAILED: txt = "assert failure";         break;
    case EXIT_QUIT:              /* fall through */
    default:                     txt = "quit";                   break;
    }
    txt2 = (exitCode >= EXIT_FIRST_FATAL_VALUE) ? ", fatal" : "";
    fprintf(uctout,     "\nExit code %lu, %s%s.\n\n", exitCode, txt, txt2);
    fflush(uctout);
    if( exitCode >= EXIT_FIRST_FATAL_VALUE )
    { if( uctout != stdout )
      { fprintf(stdout, "\nExit code %lu, %s%s.\n\n", exitCode, txt, txt2);
        fflush(stdout);
      }
      fprintf(stderr,   "\nExit code %lu, %s%s.\n\n", exitCode, txt, txt2);
      fflush(stderr);
    }
    exit(exitCode);     /* common uct exit */

}   /* end uctExit() */

/* uctAssert(): use via UCTASSERT(CON) macro
 * 'own' uct assert function.
 * exit if condition FALSE
 */
extern void uctAssert(bool condition, char *file, int line)
{
  if( condition == FALSE )
  { char *format =
        "\n\tAssert failed, please report !!!\n"
        "-\tline: %u, file: %s\n\n";
    fprintf(uctout, format, line, file);
    fflush(uctout);
    fprintf(stderr, format, line, file);
    fflush(stderr);
    uctExit(EXIT_UCT_ASSERT_FAILED);        /* quit */
  }
}

/* printReadErrorBlocks()
 * assume that cntSkip is included in cntErr,
 * so: cntErr >= cntSkip
 */
extern void printReadErrorBlocks(Uint32 cntErr, Uint32 cntSkip,
                                 Uint32 blockAddress)
{   if( cntErr )
    {   fprintf(uctout,
            "\n%7lu\t     %4lu error block%s read",
            blockAddress, cntErr, PLURAL_S(cntErr));
        if( cntSkip )
        {   fprintf(uctout, " (%lu skipped)", cntSkip);
        }
        fprintf(uctout, "\n");
        fflush(uctout);
    }
}


