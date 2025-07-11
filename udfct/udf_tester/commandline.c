/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : commandline.c
 *
 * Description : Usage and command line parsing functions.
 *
 * Author(s)   : Gerrit Scholl
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "uct_core.h"

#include "platform.h"
#include "write_image.h"
#include "commandline.h"


extern void genericUsage()
{
  fprintf( uctout,
    "\n________________________________________________________________________\n"
    "\nUsage: udf_test [<options>] [<image file chunks list>]\n"
    "\nGeneric options:\n"
      "   -help\t     "    "# Show usage plus extra explanation and quit\n"
      "   -verbose <level>  # Suppress messages with a verbose level\n"
                  "\t\t     # higher than <level>.\n"
            "\t  # <level>       : unsigned integer, default %d, max %d\n"
            "\t  #  level   %3d  : print messages that cannot be suppressed,\n"
            "\t  #                 like errors and some informational messages\n"
            "\t  #  level   %3d  : add warnings\n"
            "\t  #  level   %3d  : add informational context\n"
            "\t  #  level   %3d  : add more informational context\n"
            "\t  #  level   %3d  : add 'fake read' information,\n"
            "\t  #                 for 'fake read' explanation, see -nofakeread\n"
            "\t  #  level %3d-%3d: reserved for future use\n"

    "\n   -mlimit <limit>  # Set message limit. The same type of message will\n"
                  "\t\t    # not be printed more than <limit> times. An extra\n"
                  "\t\t    # attention note is printed when reaching <limit>.\n"
                  "\t\t    # A value greater than %lu for <limit> is considered\n"
                  "\t\t    # to be infinite. The default for <limit> is %lu.\n"
                  "\t\t    # Some messages have a fixed upper message limit,\n"
                  "\t\t    # e.g. warnings that are normally only shown once.\n"
                  "\t\t    # This limitation can be escaped by using 0 (zero)\n"
                  "\t\t    # for the message limit. In that 'all infinite' case,\n"
                  "\t\t    # an infinite message limit will be effective for all\n"
                  "\t\t    # messages, including all EntityID logging.\n"

    "\nNote: Using '-mlimit 1 -verbose %u' will produce a minimal output where\n"
      "                   every found error or warning is shown at least once.\n"
      "      Using '-mlimit 0' will produce a maximal output where all occurences\n"
      "                   of all errors and warnings are shown.\n"

    "\n   -nofakeread      # Disable fake read. Fake read means that code for\n"
                  "\t\t    # reading information, pointed to by all file entries\n"
                  "\t\t    # is executed, except for the final block read itself.\n"
                  "\t\t    # In this way, all blocks that would have been read\n"
                  "\t\t    # are shown on verbose level %d. -nofakeread will not\n"
                  "\t\t    # disable temporary fake read for unrecorded extents.\n"

    "\n   -nofake"  "\t    # == -nofakeread, Maintained for backward compatibility.\n"

    "\n   -filecrc" "\t    # Calculate and log 16-bit CRC for FE file bodies.\n"
                  "\t\t    # Instead of fake read, a real read of all file body\n"
                  "\t\t    # data is done. This may be a time consuming operation.\n"
                  "\t\t    # Therefore the default is that no file body CRC is calculated.\n"
                  "\t\t    # For files with reported FE or AD errors, the file body size\n"
                  "\t\t    # may be less than the FE Information Length. Logging format:\n"
                  "\t\t    # <tab>file CRC: <path>, 0x<hex body crc>, <body size>\n"
                  "\t\t    # Extra:\n"
                  "\t\t    #   An overall file body CRC is calculated. It is shown in the\n"
                  "\t\t    #   final verify status report. It is a Uint16 sum calculated\n"
                  "\t\t    #   over the file body CRC and body size values of each file\n"
                  "\t\t    #   (only the bytes that could be read correctly).\n"
                  "\t\t    #   The overall file body CRC is a quick indication to see\n"
                  "\t\t    #   if media that should contain the same set of files indeed\n"
                  "\t\t    #   do so, or if for a medium verified on different drives,\n"
                  "\t\t    #   the same file body data is read. The order in which\n"
                  "\t\t    #   files and directories appear on a medium does not\n"
                  "\t\t    #   influence the overall file body CRC value.\n"

    "\n   -localtime \t    # For directory listing, print local time with timezone as\n"
                  "\t\t    # recorded instead of UTC time. See below for directory\n"
                  "\t\t    # listing layout.\n"

    "\n   -showalloc""\t    # Shows lists of allocated and unallocated contiguous\n"
                   "\t\t    # areas for each partition.\n"

    "\n   -noperm"   "\t    # No ECMA 4/14.9.5 permissions for each file or directory,\n"
                   "\t\t    # see directory listing layout below.\n"

    "\n   -ignoreAVDP256""   # Ignore AVDP at 256. Meant to verify unfinalized history\n"
                    "\t\t    # of File System with a VAT (use -lastvalidblock option).\n"

    "\n   -nocache"   "\t    # Do not use read cache. Recommended for media with\n"
                    "\t\t    # linkblocks mapped in volume space, like CD-R.\n"

    "\n   -readgap"   "\t    # Try to read in unrecorded gaps.\n"

    "\n   -usemirror" "\t    # Use Metadata Mirror File instead of Metadata File (if any).\n"

    "\n   -write_image <path>\t# Write image file of input file system and quit.\n"
                        "\t\t\t# Inspect input image, searching for AVDPs, VAT File\n"
                        "\t\t\t# Entries, other UDF descriptors, blank blocks and error\n"
                        "\t\t\t# blocks. Begin and end block number of the input image can\n"
                        "\t\t\t# be modified using the -start and -lastvalidblock options.\n"
                        "\t\t\t# <path>: output image file path. It is recommended\n"
                        "\t\t\t#         to use .img extension in <path>.\n"
                        "\t\t\t# If the output image file would have a size higher\n"
                        "\t\t\t# than %.10f %sbyte, it will be split up into\n"
                        "\t\t\t# multiple chunks of about equal size. The size of\n"
                        "\t\t\t# each chunk will be a multiple of the block size\n"
                        "\t\t\t# and at most %.10f %sbyte. The maximum chunk\n"
                        "\t\t\t# size can be reduced using the -chunksize option or\n"
                        "\t\t\t# by using the -start and -lastvalidblock options.\n"
                        "\t\t\t# If <path> is <basepath>[.extension], the path\n"
                        "\t\t\t# names of multiple output chunk files will be\n"
                        "\t\t\t# <basepath>_<x>[.extension], where <x> is a\n"
                        "\t\t\t# character starting with \"a\" for the first chunk,\n"
                        "\t\t\t# \"b\" for the second, etc.\n"
                        "\t\t\t# The input file system can be read from a device\n"
                        "\t\t\t# or from another image file, following the normal\n"
                        "\t\t\t# input definition rules for verification.\n"
                        "\t\t\t# Read error blocks (like unrecorded blocks, reserved\n"
                        "\t\t\t# tracks, etc.) are written to the output image file\n"
                        "\t\t\t# as blank blocks (all #00 bytes), but read error\n"
                        "\t\t\t# blocks at the end of an image file are not written.\n"
                        "\t\t\t# Logging verbose levels:\n"
                        "\t\t\t#\t%3u: AVDPs and VAT File Entries.\n"
                        "\t\t\t#\t%3u: Other descriptors and write logging.\n"
                        "\t\t\t#\t%3u: Read logging.\n"

    "\n   -chunksize <MbSize>  # Reduce maximum chunk size for -write_image option.\n"
                        "\t\t\t# <MbSize> is the size in Megabytes. \"-chunksize 1024\"\n"
                        "\t\t\t# will reduce the max chunk size to 1 Gbyte.\n"

    "\n   -inspect_image"   "\t# Inspect image as for -write_image above, but no\n"
                        "\t\t\t# output image file is written.\n"

    "\n   -start <startblock>  # <startblock>: First block number definition for\n"
                        "\t\t\t#     -write_image and -inspect_image, default 0.\n"

            /* -verbose */
        , VERBOSEdefault,   VERBOSEMAXlevel
        , VERBOSE00level,   WARN01level, INFO01level, INFO02level, FAKE01level
        , VERBOSERESlevel,  VERBOSEMAXlevel
            /* -mlimit */
        , MLIMITinfinite - 1
        , MLIMITdefault
            /* Note: */
        , WARN01level
            /* -nofakeread */
        , FAKE01level
            /* -write_image verbose levels */
        , nBytesDouble(WI_MAX_CHUNK_SIZE), nBytesChar(WI_MAX_CHUNK_SIZE)
        , nBytesDouble(WI_MAX_CHUNK_SIZE), nBytesChar(WI_MAX_CHUNK_SIZE)
        , VERBOSE00level,   INFO01level, INFO02level
        );
}           /* end genericUsage() */

extern void mediumOptionsUsage()
{
    fprintf( uctout, "\nMedium info options:\n"

  "\n   -udf  <revision>"  "     # Verify UDF revision, range %x.%02x to %x.%02x\n"
                          "\t\t\t# If undefined, the verifier will try to find\n"
                          "\t\t\t# out itself, but it can do a better job if\n"
                          "\t\t\t# the -udf <revision> option is used.\n"

  "\n   -blocksize      <size in bytes>"    "\t# Logical block size, default: %lu\n"
    "   -ecclength      <sectors per ECC>"  "\t# ECC blocking factor, e.g 32 for CD-RW.\n"
    "   -packetlength"                  "\t\t\t# OBSOLETE, use -ecclength instead.\n"
    "   -lastvalidblock <block number>"     "\t# Highest Volume Space address\n"
    "   -L0capacity     <capacity>"   "\t    # Layer zero capacity (sectors) for ML media\n"
                                "\t\t\t\t    # 'far apart' physical distance calulation.\n"
    "   -PTP"                   "\t\t\t\t    # Set Parallel Track Path for -L0capacity\n"
                                "\t\t\t\t    # option, default is Opposite Track Path.\n"
    "   -sessionstart   <block number>"  "   # Session start address, one\n"
                                "\t\t\t\t    # -sessionstart <bn> for each session\n"
                                "\t\t\t\t    # Added by default: -sessionstart 0\n"
    "   -verifysession  <number>"     "\t    # Session number to start verify,\n"
                                "\t\t\t\t    #   1 for first session,\n"
                                "\t\t\t\t    # default: last session.\n"
    "   -dummysession   <nBlocks>"  "  # Insert dummy session of <nBlocks> blocks.\n"
                    "\t\t\t  "    "    # This option is for image file usage only\n"
                    "\t\t     # and meant as a placeholder for non-data sessions\n"
                    "\t\t     # for Mixed Mode Multisession images.\n"
                    "\t\t     # UDF references from other sessions to a dummy\n"
                    "\t\t     # session will result in read errors.\n"
                    "\t\t     # Dummy sessions are inserted before normal sessions.\n"
                    "\t\t     # An implicite \"-sessionstart <x>\" option is inserted\n"
                    "\t\t     # for the session following the dummy session.\n"
                    "\t\t     # By default, verification starts in the last session.\n"

  "\n  note 1: For image file verification, medium info options can also be\n"
          "\t  written to an image configuration file (text file). Medium info\n"
          "\t  options read from a configuration file are equivalent to medium\n"
          "\t  info gathered from a device. An image configuration file has\n"
          "\t  the same path as the first chunk of the image file it belongs\n"
          "\t  to, but with a file name extension .cfg\n"

  "\n  note 2: Command line options will overrule information that is gathered\n"
          "\t  from a device or from an image configuration file.\n"
          "\t  E.g. the command line option \"-lastvalidblock <x>\" will change\n"
          "\t  the last valid block number (N) as read from a device to <x>.\n"

    , (MIN_UDFREVISION >> 8), (MIN_UDFREVISION & 0xFF),
      (MAX_UDFREVISION >> 8), (MAX_UDFREVISION & 0xFF),
       MI_DEFAULT_BLOCKSIZE );
}           /* end mediumOptionsUsage() */

/* explanation printed with -help option only
 */
extern void extraHelpUsage()
{
    fprintf( uctout,
      "\nUDF verifier exit status:\n"
        "  The UDF verifier exit status value depends on the occurrence of errors or\n"
        "  warnings and the fact whether verification could normally be completed or not.\n"
        "  Exit status values:\n"
        "      %2u  -  Normal completion, no errors, no warnings\n"
        "      %2u  -  Normal completion, no errors, with warnings\n"
        "      %2u  -  Normal completion, with errors, maybe also warnings\n"
        "   >= %2u  -  Fatal errors, verification could not be completed for various reasons\n"
        "   All other values are reserved for future use.\n",
            EXIT_OK, EXIT_WITH_WARN_NOERR, EXIT_WITH_ERR, EXIT_FIRST_FATAL_VALUE );

    fprintf( uctout,
      "\nTimestamp time comparison policy:\n"
        "  For a time comparison where at least one undefined timezone\n"
        "  is involved, the timezone values are ignored.\n"
        "  When both timezone values are defined, the timezone\n"
        "  values are taken into account in the time comparison.\n"

      "\nOutput messages format:\n"
        "      Error messages contain :   \"Error:\" or   \"error:\"\n"
        "    Warning messages contain : \"Warning:\" or \"warning:\"\n"
        "  Attention messages contain :    \"Note:\" or    \"note:\"\n"
        "Message continuation lines start with a \"-\" character.\n"

      "\nThe 8 leftmost character positions are reserved for the physical\n"
        "block address of the informational block read messages.\n"
        "Most other messages use a tab character to cover these 8 positions.\n"
        "Many messages have the following format:\n"
        "<tab><4 char descriptor id> <BP> <message> <continuation lines>,\n"
        "where <BP> is the descriptor byte position of the field involved.\n"
        "Extended Attribute (EA) messages show a byte position with respect\n"
        "to the begin of the EA Space (EASP).\n"

      "\nUDF and ECMA references in verifier messages refer to the documents\n"
        "as appropiate for the recorded UDF revision. Note that often more\n"
        "clarification may be found in newer UDF revision documents.\n"

      "\nUnicode names in the output of this program are printed between\n"
        "double quotes (\"\"). Unicode characters in the range #0020 to #007E\n"
        "included, are printed as their ascii equivalent, except for the\n"
        "characters \" (#0022), # (#0023) and / (#002F). Unicode characters\n"
        "outside the range mentioned above and the exception characters \",# and\n"
        "/ are printed in hexadecimal form as: #HHHH, where H is a hex digit.\n"
        "All characters are exactly represented in this way.\n"
        "For files that do not have a unicode name, names between <> are used,\n"
        "like <root> and <SysStreamDir>. For denoting streams and stream\n"
        "directories, a double slash (//) is used, e.g. \"file\"//\"stream01\"\n"
        "for a named stream and \"file\"// for the stream directory itself.\n"

      "\nIdentifiers using one-byte characters - like in EntityIDs -\n"
        "are printed in the same way as explained above for unicode names,\n"
        "except that representation in hexadecimal form uses only 2 hex\n"
        "digits per character (#HH).\n"

      "\nDirectory listing line layout information:\n"
      "\n      <char>[<perm>]        <type> <flc>         <dtime>       <size> <name>\n"
        " e.g.:\n"
        "  .d......e.:..rwx:..r.x:..r.x DIR  10  1999-03-24 13:00 +02:00  2048 \"large_files\"\n"

        "where:\n"
        "<char> : FID File Characteristics, ICBTag Flags, etc., ECMA 4/14.4.3, 4/14.6.8\n"
        "  h...............  hidden    entry (FID)\n"
        "  .d..............  directory entry (FID)\n"
        "  ..D.............  Deleted   entry (FID)\n"
        "  ...p............  parent    entry (FID)\n"
        "  ....m...........  metadata  entry (FID)\n"

      "\n  .....S..........     Short ADs         (ICBTag Flags bit 0-2)\n"
        "  .....L..........      Long ADs         (ICBTag Flags bit 0-2)\n"
        "  .....E..........  Extended ADs         (ICBTag Flags bit 0-2) illegal\n"
        "  .....e..........   embedded allocation (ICBTag Flags bit 0-2) "
                                                             "data embedded in (E)FE\n"
        "  ....<v>.........    illegal allocation (ICBTag Flags bit 0-2) <v> : [4-7]\n"

        "  ......N.........  Non-relocatable flag (ICBTag)\n"
        "  .......A........          Archive flag (ICBTag)\n"
        "  ........U.......           SetUid flag (ICBTag)\n"
        "  .........G......           SetGid flag (ICBTag)\n"
        "  ..........Y.....           Sticky flag (ICBTag)\n"
        "  ...........C....       Contiguous flag (ICBTag)\n"
        "  ............S...           System flag (ICBTag)\n"
        "  .............s..           Stream flag (ICBTag)\n"

      "\n  ..............S.  (E)FE has Stream Directory\n"
        "  ...............E  (E)FE has Extended Attribute Space\n"

      "\n<perm> : permission bits, suppressed by -noperm option\n"
        "  :d....:d....:d....  owner/group/other  delete           permission\n"
        "  :.a...:.a...:.a...  owner/group/other  change attribute permission\n"
        "  :..r..:..r..:..r..  owner/group/other  read             permission\n"
        "  :...w.:...w.:...w.  owner/group/other  write            permission\n"
        "  :....x:....x:....x  owner/group/other  execute          permission\n"

      "\n<type> : ICB Tag File Type, see (ECMA 4/14.6.6)\n"
        "  e.g. DIR     type 4, directories\n"
        "       FILE    type 5, random access file\n"

      "\n<flc>  : File Link Count\n"

      "\n<dtime>: File modification date and time: [*]yyyy-mm-dd hh:mm[:ss[.cchhmm]].\n"
        "         UTC time or local time with zone addition: [[+|-]hh:mm] | 'noZone'.\n"
        "         Default is UTC time. If -localtime option or if unable to translate\n"
        "         to UTC, then local time as recorded. Default will not show\n"
        "         seconds-and-lower part. -localtime option will show seconds as well\n"
        "         and also lower-than-seconds part [.cchhmm] in case that part is not\n"
        "         equal to '.000000'. Invalid timestamps are preceded by a '*' char.\n"

        "\n<size> : Byte size in (E)FE Information Length field.\n"

      "\n<name> : FID File Identifier, if possible as unicode name, see above.\n"
      "\n"
      "\nFID verification logging:\n"
      "\n  FID cid: <OSTA CS0 compression ID> [D] name: <identifier unicode name>\n"
        "  where [D] is a \'D\' character if deleted bit set, else a space.\n"
      "\n"
      "\nMiscellaneous:\n"
      "\n  In messages, UDF 2.00+ means: UDF Revisions 2.00 and higher\n"
              "\t   and UDF 1.50- means: UDF Revisions 1.50 and lower\n"

      "\n  Error and warning counts are shown in the final verify status report.\n"

      "\n  Search for errors, warnings and notes in the verifier output text using a\n"
        "  case-insensitive search for \"error:\", \"warning:\" or \"note:\" respectively.\n"
      "\n"
    );
}       /* end extraHelpUsage() */


/* command line arguments parsing ************************************
 */

/* print headText and valid arguments.
 * argv[0] is not printed. For any other contiguous sequence
 * of NULL arguments (argv[n]==NULL), 3 dots are printed.
 * Further one space is printed before each argument,
 * however if the line becomes too long "\n\t" is printed
 * instead of a space.
 */
#define CLPRINT_MAX_CHARS_PER_LINE  64

extern void clPrintRemainingArguments(int argc, char **argv,
                                      char *headText)
{   size_t  charsPerLine, len;
    bool    print3Dots;

    fprintf(uctout, "%s", headText);

    /* Print remaining arguments. Print 3 dots
     * for each sequence of NULL arguments.
     */
    print3Dots = TRUE;
    charsPerLine = CLPRINT_MAX_CHARS_PER_LINE;  /* first time new line */
    while( --argc )
    {   if( *(++argv) != NULL )     /* valid command line argument */
             len = strlen(*argv);
        else if( print3Dots )       /* *argv == NULL */
             len = 3;
        else continue;              /* nothing to print */

        charsPerLine += 1 + len;
        if( charsPerLine <= CLPRINT_MAX_CHARS_PER_LINE )
        {   fprintf(uctout, " ");       /* default */
        }
        else
        {   fprintf(uctout, "\n\t");    /* new line */
            charsPerLine = 8 + len;
        }
        if( *argv != NULL )         /* valid command line argument */
        {   fprintf(uctout, "%s", *argv);
            print3Dots = TRUE;
        }
        else if( print3Dots )       /* *argv == NULL */
        {   fprintf(uctout, "...");
            print3Dots = FALSE;
        }
    }
    fprintf(uctout, "\n");
}

/* Function clCountValidArguments:
 * argv[0] and arguments marked out by a NULL pointer are
 * excluded from the count.
 */
extern int clCountValidArguments(int argc, char **argv)
{
    int n = 0;
    while( --argc )
    {   if( *(++argv) != NULL ) n++;    /* not marked out */
    }
    return n;
}

/* Function clCountOptions:
 * argv[0] and arguments marked out by a NULL pointer are
 * excluded from the count.
 */
extern int clCountOptions(int argc, char **argv)
{
    int n = 0;
    while( --argc )
    {   if(   *(++argv) != NULL         /* not marked out */
           && (*argv)[0] == '-' ) n++;  /* option found */
    }
    return n;
}

/* Function clFindArgument:
 * Search for command line argument equal to searchArgument.
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
extern int clFindArgument(int argc, char **argv, char *searchArgument)
{
    int  n;
    for( n = 1; --argc; n++ )
    {   if(    *(++argv) != NULL        /* skip marked out argument */
            && (   searchArgument == NULL
                || strcmp(*argv, searchArgument) == 0
               ))
        {   return n;   /* found */
        }
    }
    return 0;           /* not found */
}

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
extern int clNextArgument(int argc, char **argv, int index)
{
    if( index < 0 ) index = 0;
    argv += index;
    argc -= index;
    index++;
    for( ; --argc; index++ )
    {   if( *(++argv) != NULL )     /* skip marked out argument */
        {   return index;   /* found */
        }
    }
    return 0;           /* not found */
}

/* Function clParseOptionUint32();
 * Parse and mark out option with Uint32 argument.
 * Return values: see commandline.h
 */
extern int clParseOptionUint32( int argc, char **argv,
                                char *option, Uint32 *value)
{
    char    *tmpstr;
    Uint32   index = clFindArgument( argc, argv, option );

    if( index == 0 ) return OPT_NOT_FOUND;

    /* option found
     */
    fprintf(uctout,"\t%s", option);
    argv[index++] = NULL;       /* mark out, to next argument */

    if(    argv[index]    == NULL
        || argv[index][0] == '\0'
        || argv[index][0] == '-' )
    {
        fprintf(uctout, "\n\n<unsigned argument> missing for: %s\n",
                            option);
        return OPT_ERROR;
    }
    fprintf(uctout," %s\n", argv[index]);

    /* strtol() max range till ( MAX_INT32 - 1 )  !!!!!
     */
    *value = strtol( argv[index], &tmpstr, 10 );
    if( *tmpstr != '\0' || (Int32) (*value) < 0 || (*value) == MAX_INT32 )
    {
        fprintf(uctout,
            "\n<unsigned argument> error or out of range for: %s %s\n",
                            option, argv[index]);
        argv[index] = NULL;     /* error, but mark out as used */
        return OPT_ERROR;
    }
    argv[index] = NULL;         /* ok, now mark out */
    return OPT_FOUND;
}

/* parse command line for generic options
 * Some options directly set global variables.
 */
extern bool parseGenericOptions(int argc, char **argv,
                                genericOptions *localOptions)
{
    char    *option;
    Uint32   tmpU32;
    int      index, cnt1, cnt2;

    fprintf(uctout, "Command:\n  %s\n"
                     "Generic options parsing:\n", argv[0]);

    /* test multiple occurrence of options
     */
    uctVerboseLevel = VERBOSEdefault;
    for( cnt1 = 0;
         cnt1 != (cnt2 = clCountValidArguments(argc,argv));
         cnt1 = cnt2 )
    {
        option = "-help";
        if( (index = clFindArgument(argc, argv, option)) != 0 )
        {
            localOptions->helpOptionFound = TRUE;
            fprintf(uctout,"\t%s\n", option);
            argv[index] = NULL;     /* ok, mark out as used */
            return TRUE;            /* no further options parsing */
        }

        option = "-verbose";        /* -verbose <level> */
        switch( clParseOptionUint32(argc, argv, option, &tmpU32) )
        {
        case OPT_ERROR:
            return FALSE;           /* error */
            /** break; **/
        case OPT_FOUND:
            if( tmpU32 > VERBOSEMAXlevel )
            {
                fprintf(uctout, "\nVerbose level out of range 0 -> %d : %d\n",
                    VERBOSEMAXlevel, tmpU32);
                return FALSE;
            }
            uctVerboseLevel = (Int8) tmpU32;
            break;
        }

        option = "-mlimit";         /* -mlimit <limit> */
        switch( clParseOptionUint32(argc, argv, option, &tmpU32) )
        {
        case OPT_ERROR:
            return FALSE;           /* error */
            /** break; **/
        case OPT_FOUND:
            if( tmpU32 == 0 || tmpU32 >= MLIMITinfinite )
            {
                fprintf(uctout, "\t message limit %sinfinite\n",
                    (tmpU32 == 0) ? "all " : "");
            }
            /* value (and MLIMITinfinite) must fit in Int32
             * but not be negative !!
             */
            uctMessageLimit = (Int32)MIN(tmpU32,(Uint32)MLIMITinfinite);
            break;
        }

        option = "-localtime";
        if( (index = clFindArgument(argc, argv, option)) != 0 )
        {   uctDoUTCtime = FALSE;       /* local time in dir listing */
            fprintf(uctout,"\t%s\n", option);
            argv[index] = NULL;         /* ok, mark out as used */
        }

        option = "-showalloc";
        if( (index = clFindArgument(argc, argv, option)) != 0 )
        {   uctDoShowAlloc = TRUE;      /* show partition allocation */
            fprintf(uctout,"\t%s\n", option);
            argv[index] = NULL;         /* ok, mark out as used */
        }

        option = "-noperm";
        if( (index = clFindArgument(argc, argv, option)) != 0 )
        {   uctDoShowPerm = FALSE;      /* show (E)FE permissions */
            fprintf(uctout,"\t%s\n", option);
            argv[index] = NULL;         /* ok, mark out as used */
        }

        option = "-nofakeread";     /* == -nofake (for backw. comp.) */
        if(   (index = clFindArgument(argc, argv, option))    != 0
           || (index = clFindArgument(argc, argv, "-nofake")) != 0 )
        {   uctDoFakeRead = FALSE;      /* disable fake read */
            fprintf(uctout,"\t%s\n", option);
            argv[index] = NULL;         /* ok, mark out as used */
        }

        option = "-ignoreAVDP256";
        if( (index = clFindArgument(argc, argv, option)) != 0 )
        {   uctIgnoreAVDP256 = TRUE;    /* ignore AVDP at 256 */
            fprintf(uctout,"\t%s\n", option);
            argv[index] = NULL;         /* ok, mark out as used */
        }

        option = "-filecrc";
        if( (index = clFindArgument(argc, argv, option)) != 0 )
        {   uctDoFileCRC = TRUE;        /* calculate file CRC */
            fprintf(uctout,"\t%s\n", option);
            argv[index] = NULL;         /* ok, mark out as used */
        }

        option = "-nocache";
        if( (index = clFindArgument(argc, argv, option)) != 0 )
        {   uctUseReadCache = FALSE;    /* do not use read cache */
            fprintf(uctout,"\t%s\n", option);
            argv[index] = NULL;         /* ok, mark out as used */
        }

        option = "-readgap";
        if( (index = clFindArgument(argc, argv, option)) != 0 )
        {   uctDoReadGap = TRUE;    /* try to read in unrecorded gaps */
            fprintf(uctout,"\t%s\n", option);
            argv[index] = NULL;         /* ok, mark out as used */
        }

        option = "-usemirror";
        if( (index = clFindArgument(argc, argv, option)) != 0 )
        {   uctUseMetadataMirror = TRUE; /* use metadata mirror file */
            fprintf(uctout,"\t%s\n", option);
            argv[index] = NULL;         /* ok, mark out as used */
        }

        option = "-write_image";        /*  -write_image <path> */
        if( (index = clFindArgument(argc, argv, option)) != 0 )
        {   fprintf(uctout,"\t%s ", option);
            argv[index++] = NULL;   /* mark out as used, */
                                    /*  test <path> argument */
            if( argv[index] == NULL || *argv[index] == '-' )
            {   fprintf(uctout, "option argument missing\n");
                return FALSE;
            }
            fprintf(uctout,"%s\n", argv[index]);    /* ok */
            if( localOptions->writeImagePath != NULL )  /* -write_image */
            {   VERBOSE00(uctout,
                  "\t warning: Overruling previous -write_image option\n");
            }
            localOptions->writeImagePath = argv[index];
            argv[index++] = NULL;   /* mark out as used, */
        }

        option = "-inspect_image";      /* -inspect_image */
        if( (index = clFindArgument(argc, argv, option)) != 0 )
        {   fprintf(uctout,"\t%s\n", option);
            argv[index++] = NULL;   /* mark out as used, */
            localOptions->inspectImage = TRUE;
        }

        option = "-start";          /*  -start <startblock> */
        switch( clParseOptionUint32(argc, argv, option, &tmpU32) )
        {
        case OPT_ERROR:
            return FALSE;               /* error */
            /** break; **/
        case OPT_FOUND:
            if(   localOptions->writeImagePath == NULL  /* no -write_image */
               && localOptions->inspectImage == FALSE ) /* no -inspect_image */
            {   fprintf(uctout, "error: %s can be used with"
                    " -write_image or -inspect_image only.\n", option);
                return FALSE;
            }
            localOptions->imageStartBlock = tmpU32;
            break;
        }
        option = "-chunksize";          /*  -chunksize <MbSize> */
        switch( clParseOptionUint32(argc, argv, option, &tmpU32) )
        {
        case OPT_ERROR:
            return FALSE;               /* error */
            /** break; **/
        case OPT_FOUND:
            if( localOptions->writeImagePath == NULL )  /* no -write_image */
            {   fprintf(uctout, " error: %s can be used with"
                                " -write_image only\n", option);
                return FALSE;
            }
            /* tmpU32 holds size in Mbytes.
             * Round byte size down to WI_MAX_CHUNK_SIZE if the reqested
             * size is not more than 64 Kbytes too big
             * (64 Kbytes seen as a kind of max blockSize).
             * This is in order to avoid error messages if:
             *    (   tmpU32 == 2048                    // 2 Gb
             *     && WI_MAX_CHUNK_SIZE == MAX_INT32   // 2 Gb - 1 byte
             * || (   tmpU32 == 2097152               // 2 Tb
             *     && WI_MAX_CHUNK_SIZE ==   '2 Tb - 512 bytes' )
             * For platforms supporting 64 bit file size and offset,
             * WI_MAX_CHUNK_SIZE will be: 2 Terabyte - 512 bytes.
             */
            uctWiMaxChunksize = (Int64)tmpU32 * (Int64)MbBYTES;
            if(   uctWiMaxChunksize >  (Int64)WI_MAX_CHUNK_SIZE
               && uctWiMaxChunksize <= (Int64)WI_MAX_CHUNK_SIZE + (Int64)(64*KbBYTES) )
            { uctWiMaxChunksize = (Int64)WI_MAX_CHUNK_SIZE; /* decrement silently */
            }
            if(   uctWiMaxChunksize < 0
               || uctWiMaxChunksize > WI_MAX_CHUNK_SIZE )
            {   fprintf(uctout,
                     " error: %s max size: %.10f %sbyte\n"
                  "-\t  platform max size: %.10f %sbyte\n",
                    option, nBytesDouble(uctWiMaxChunksize),
                              nBytesChar(uctWiMaxChunksize),
                            nBytesDouble(WI_MAX_CHUNK_SIZE),
                              nBytesChar(WI_MAX_CHUNK_SIZE));

                uctWiMaxChunksize = WI_MAX_CHUNK_SIZE;
                return FALSE;
            }
            fprintf(uctout,
              "=>\t-write_image max chunk size: %.10f %sbyte\n",
                    nBytesDouble(uctWiMaxChunksize),
                      nBytesChar(uctWiMaxChunksize));
            break;
        }
    }
    fprintf(uctout,"\n");

#ifdef        DEBUG02
    uctVerboseLevel = DEBUG02level;     /* includes DEBUG01level */
#elif defined DEBUG01
    uctVerboseLevel = DEBUG01level;
#endif  /* DEBUG02, DEBUG01 */

#ifdef  UCT_TESTING
//  /** testing **/ uctVerboseLevel = INFO02level;      /* -verbose <> */
//  /** testing **/ uctVerboseLevel = VERBOSEMAXlevel;  /* -verbose <> */

    /** testing minimal output **/
//  /** testing **/ uctVerboseLevel = WARN01level;      /* -verbose <> */
//  /** testing **/ uctMessageLimit = 1;                /* -mlimit  <> */

    /** testing maximal output **/
//  /** testing **/ uctVerboseLevel = 127;              /* -verbose <> */
//  /** testing **/ uctMessageLimit = 0; /* 'all infinite' -mlimit  <> */

//  /** testing **/ uctDoFakeRead   = FALSE;            /* -nofakeread */
//  /** testing **/ uctDoFileCRC    = TRUE;             /* -filecrc    */
//  /** testing **/ uctUseReadCache = FALSE;            /* -nocache    */
//  /** testing **/ uctUseMetadataMirror = TRUE;        /* -usemirror  */
//  /** testing **/ if(   getUctMinUdfRevision() <= 0x260   /* -udf 2.60  */
//  /** testing **/    && getUctMaxUdfRevision() >= 0x260 ) /* -udf 2.60  */
//  /** testing **/ { modifyUdfRevisionRange(               /* -udf 2.60  */
//  /** testing **/         0x260, 0x260, "TESTING");       /* -udf 2.60  */
//  /** testing **/ }                                       /* -udf 2.60  */
#endif  /** UCT_TESTING **/

    return TRUE;

}   /* end parseGenericOptions() */


/* parse command line for medium info options
 * Return value: TRUE if ok, else FALSE
 */
extern bool parseMediumOptions( int argc, char **argv,
                                MediumInfo *mi, bool *overruled )
{
    int    index, cnt1, cnt2;
    Uint32 blockSize;
    char  *option;

    *overruled = FALSE;
    clearMediumInfo( mi );

    /* -blocksize <size in bytes>
     * repeat until no more found.
     */
    option = "-blocksize";
    for( cnt1 = 0;
         cnt1 != (cnt2 = clCountValidArguments(argc,argv));
         cnt1 = cnt2 )
    {
        switch( clParseOptionUint32(argc, argv, option, &blockSize) )
        {
        case OPT_ERROR:
            return FALSE;           /* error */
            /** break; **/
        case OPT_FOUND:
            if( blockSize == 0 || (blockSize % 512) != 0 )
            {
                fprintf(uctout,
                    "\nError: Block size: %lu, must be multiple of 512 bytes\n",
                                    blockSize);
                return FALSE;       /* error */
            }
            if(    mi->blockSize != 0
                && mi->blockSize != blockSize)
            {
                fprintf(uctout, "Medium Block Size overruled by: %s %lu (was %lu)\n",
                                    option, blockSize, mi->blockSize);
                *overruled = TRUE;
            }
            mi->blockSize = blockSize;
            break;
        }
    }       /* endfor: repeat until no more "option" found */

    /* OBSOLETE: -packetlength <ECC blocking factor>
     * notify that -packetlength is obsolete now (use -ecclength)
     */
    option = "-packetlength";
    if( (index = clFindArgument(argc, argv, option)) != 0 )
    {   /* found */
        fprintf(uctout,"\t%s\t\t  OBSOLETE: use -ecclength <n> instead\n",
                            option);
        return FALSE;           /* error */
    }

    /* -ecclength <ECC blocking factor>
     * repeat until no more found.
     */
    option = "-ecclength";
    for( cnt1 = 0;
         cnt1 != (cnt2 = clCountValidArguments(argc,argv));
         cnt1 = cnt2 )
    {   Uint32 eccLength;
        switch( clParseOptionUint32(argc, argv, option, &eccLength) )
        {
        case OPT_ERROR:
            return FALSE;           /* error */
            /** break; **/
        case OPT_FOUND:
            if( eccLength == 0 )
            {
                fprintf(uctout, "\nECC blocking factor must not be zero\n");
                return FALSE;       /* error */
            }
            if(    mi->eccLength != 0           /* not yet defined */
                && mi->eccLength != eccLength)
            {
                fprintf(uctout, "Medium ECC blocking factor"
                            " overruled by: %s %lu (was %lu)\n",
                            option, eccLength, mi->eccLength);
                *overruled = TRUE;
            }
            mi->eccLength = eccLength;
            break;
        }
    }       /* endfor: repeat until no more "option" found */

    /* -lastvalidblock <block number>
     * repeat until no more found.
     */
    option = "-lastvalidblock";
    for( cnt1 = 0;
         cnt1 != (cnt2 = clCountValidArguments(argc,argv));
         cnt1 = cnt2 )
    {
        Uint32 lastValidBlockNr;

        switch( clParseOptionUint32(argc, argv, option, &lastValidBlockNr) )
        {
        case OPT_ERROR:
            return FALSE;           /* error */
            /** break; **/
        case OPT_FOUND:
            if( lastValidBlockNr < 257 )        /* ECMA 3/8.1.2.1 */
            {   fprintf(uctout,
                    "\nerror: last valid block number less than 257: %lu,"
                                    " ECMA 3/8.1.2.1\n", lastValidBlockNr);
                return FALSE;       /* error */
            }
            if(    mi->lastValidBlockNr != 0
                && mi->lastValidBlockNr != lastValidBlockNr)
            {
                fprintf(uctout, "Medium last valid block"
                            " overruled by: %s %lu (was %lu)\n",
                            option, lastValidBlockNr, mi->lastValidBlockNr);
                *overruled = TRUE;
            }
            mi->lastValidBlockNr = lastValidBlockNr;
            break;
        }
    }       /* endfor: repeat until no more "option" found */

    /* -L0capacity <L0 capacity> (for 'far apart' calculation)
     * repeat until no more found.
     */
    option = "-L0capacity";
    for( cnt1 = 0;
         cnt1 != (cnt2 = clCountValidArguments(argc,argv));
         cnt1 = cnt2 )
    {
        Uint32 L0capacity;

        switch( clParseOptionUint32(argc, argv, option, &L0capacity) )
        {
        case OPT_ERROR:
            return FALSE;           /* error */
            /** break; **/
        case OPT_FOUND:
            if(    mi->eccLength != 0
                && (L0capacity % mi->eccLength) != 0 )
            {
                fprintf(uctout, "Error: L0 capacity: %lu,"
                            " expected: multiple of %lu (ECC).\n",
                    L0capacity, mi->eccLength);
                return FALSE;       /* error */
            }
            if(    mi->L0capacity != 0
                && mi->L0capacity != L0capacity)
            {
                fprintf(uctout, "L0 capacity"
                            " overruled by: %s %lu (was %lu)\n",
                            option, L0capacity, mi->L0capacity);
                *overruled = TRUE;
            }
            mi->L0capacity = L0capacity;
            mi->isOTP = TRUE;       /* default, see -PTP option */
            break;
        }
    }       /* endfor: repeat until no more "option" found */

    /* -PTP Parallel Track Path (for 'far apart' calculation)
     * ONLY use together with -L0capacity !!
     * Test after -L0capacity option.
     * repeat until no more found.
     */
    option = "-PTP";
    for( cnt1 = 0;
         cnt1 != (cnt2 = clCountValidArguments(argc,argv));
         cnt1 = cnt2 )
    {   if( (index = clFindArgument(argc, argv, option)) != 0 )
        {   /* found */
            fprintf(uctout,"\t%s\t\t  DL Parallel Track Path\n",
                            option);
            if( mi->L0capacity == 0 )           /* undefined */
            { fprintf(uctout,
                "Fatal error: Use %s option only together with -L0capacity <n>\n",
                                option);
              return FALSE;
            }
            mi->isOTP = FALSE;      /* set PTP, overrule silently */
            argv[index] = NULL;     /* ok, mark out as used */
        }
    }       /* endfor: repeat until no more "option" found */

    /* -dummysession <nBlocks>
     * Handle all -dummysession options here.
     * More than one dummy sessions can be defined.
     * An implicite -sessionstart <x> is generated for
     * the session following the dummy session.
     */
    option = "-dummysession";
    if( clFindArgument(argc, argv, option) != 0 )   /* found at least one */
    {
        for( cnt1 = 0;
             cnt1 != (cnt2 = clCountValidArguments(argc,argv));
             cnt1 = cnt2 )
        {   Uint32 dummySessionNrOfBlocks;

            switch( clParseOptionUint32(argc, argv,
                                option, &dummySessionNrOfBlocks) )
            {
            case OPT_ERROR:
                return FALSE;           /* error */
                /** break; **/
            case OPT_FOUND:
                if( dummySessionNrOfBlocks == 0 )
                {   fprintf(uctout,
                      "\nDummy session size zero, option ignored\n");
                }
                else
                {   mi->dummySessionTotalBlocks += dummySessionNrOfBlocks;
                    if( mi->dummySessionTotalBlocks < dummySessionNrOfBlocks )
                    {   fprintf(uctout, "\nOverflow on -dummy session size\n");
                        return FALSE;           /* error */
                    }
                    if( mi->numberOfSessions == 0 ) /* show -sessionstart 0 */
                    {   fprintf(uctout,"\t-sessionstart 0\n");
                    }
                    addSessionToMediumInfo( mi, mi->dummySessionTotalBlocks );
                    fprintf(uctout,"\t-sessionstart %lu\n",
                                mi->dummySessionTotalBlocks);
                }
                break;
            }
        }       /* endfor: repeat until no more "option" found */
    }

    /* -sessionstart <block number>
     * Handle all -sessionstart options here. Startblocks of
     * several sessions can be defined, so no overruling.
     * If any -sessionstart is found, a "-sessionstart 0"
     * will be added implicitly.
     * Equal startblock values will occur only once in the
     * final sessionStartBlocks[] array.
     */
    option = "-sessionstart";
    if( clFindArgument(argc, argv, option) != 0 )   /* found at least one */
    {
        for( cnt1 = 0;
             cnt1 != (cnt2 = clCountValidArguments(argc,argv));
             cnt1 = cnt2 )
        {
            Uint32 startBlock;
            switch( clParseOptionUint32(argc, argv, option, &startBlock) )
            {
            case OPT_ERROR:
                return FALSE;           /* error */
                /** break; **/
            case OPT_FOUND:
                if( mi->numberOfSessions == 0 && startBlock != 0 )
                {                   /* show implicite sessionstart 0 */
                    fprintf(uctout,"\t%s 0\n", option);
                }
                addSessionToMediumInfo( mi, startBlock );
                break;
            }
        }       /* endfor: repeat until no more "option" found */
    }

    /* -verifysession <number>
     * repeat until no more found.
     */
    option = "-verifysession";
    for( cnt1 = 0;
         cnt1 != (cnt2 = clCountValidArguments(argc,argv));
         cnt1 = cnt2 )
    {
        Uint32 verifySession;

        switch( clParseOptionUint32(argc, argv, option, &verifySession) )
        {
        case OPT_ERROR:
            return FALSE;           /* error */
            /** break; **/
        case OPT_FOUND:
            if( verifySession == 0 )
            {
                fprintf(uctout, "\nVerify session must not be zero\n");
                return FALSE;       /* error */
            }
            if(    mi->verifySession != 0
                && mi->verifySession != verifySession)
            {
                fprintf(uctout,
                    "Verify session overruled by: %s %lu (was %lu)\n",
                        option, verifySession, mi->verifySession);
                *overruled = TRUE;
            }
            mi->verifySession = verifySession;
            break;
        }
    }       /* endfor: repeat until no more "option" found */

    /* All -dummysession, -sessionstart and -verifysession options
     * are dealt with now.
     * Log total number of sessions and set default verifysession.
     */
    if(   mi->verifySession == 0            /* not set yet, default */
       && mi->numberOfSessions != 0 )       /*  is numberOfSessions */
    {   mi->verifySession = mi->numberOfSessions;
        fprintf(uctout,"\t Found %u session%s, default verify session: %lu\n",
                         mi->numberOfSessions,
                PLURAL_S(mi->numberOfSessions),
                         mi->verifySession);
    }

    /* test multiple occurrence of other options
     */
    for( cnt1 = 0;
         cnt1 != (cnt2 = clCountValidArguments(argc,argv));
         cnt1 = cnt2 )
    {
        char    *udfRevisionOption  = "-udf";
        Uint16   rev;
        /* option: -udf <revision>
         * test all known revisions, test range later
         * Note exception: result is directly put in uct revision range
         *      rather than in MediumInfo Structure pointed to by mi.
         */
        if( (index = clFindArgument(argc, argv, udfRevisionOption)) != 0 )
        {
            fprintf(uctout,"\t%s ", udfRevisionOption);
            argv[index++] = NULL;   /* mark out as used, */
                                    /*  test <revision>  */
            if( argv[index] == NULL || *argv[index] == '-' )
            {
                fprintf(uctout, "option argument missing\n");
                return FALSE;
            }
            fprintf(uctout,"%s\n", argv[index]);
            if(        strcmp(argv[index],"1.00") == 0
                    || strcmp(argv[index],"1.0" ) == 0) rev = 0x100;
            else if(   strcmp(argv[index],"1.01") == 0) rev = 0x101;
            else if(   strcmp(argv[index],"1.02") == 0) rev = 0x102;
            else if(   strcmp(argv[index],"1.50") == 0
                    || strcmp(argv[index],"1.5" ) == 0) rev = 0x150;
            else if(   strcmp(argv[index],"2.00") == 0
                    || strcmp(argv[index],"2.0" ) == 0) rev = 0x200;
            else if(   strcmp(argv[index],"2.01") == 0) rev = 0x201;
            else if(   strcmp(argv[index],"2.50") == 0
                    || strcmp(argv[index],"2.5" ) == 0) rev = 0x250;
            else if(   strcmp(argv[index],"2.60") == 0
                    || strcmp(argv[index],"2.6" ) == 0) rev = 0x260;
            else
            {   rev = 0;
            }
            if( rev == 0 || !isKnownUdfRevision(rev) )
            {   fprintf(uctout,
                    "\nUnknown UDF revision %s,"
                                " define as x.xx or x.x (e.g. 1.5)\n",
                                        argv[index]);
                argv[index] = NULL;     /* mark out as used */
                return FALSE;
            }
            if(    rev < getUctMinUdfRevision()
                || rev > getUctMaxUdfRevision() )
            {
                if(getUctMinUdfRevision() == getUctMaxUdfRevision())
                {
                    fprintf(uctout,
                        "\nConflicting -udf <revision> options\n");
                    argv[index] = NULL;     /* mark out as used */
                    return FALSE;
                }
                fprintf(uctout,
                    "\nUDF revision %s out of range for verifier\n",
                                    argv[index]);
                argv[index] = NULL;     /* mark out as used */
                return FALSE;
            }
            argv[index] = NULL;         /* mark out as used */
            if( !modifyUdfRevisionRange( rev, rev,
                        "-udf <revision> command line option") )
            {
                return FALSE;
            }
        }
    }           /* repeat untill no more found */

    return TRUE;            /* ok */

}   /* end parseMediumOptions() */

