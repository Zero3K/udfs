/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2007
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * File        : scsiapi.c
 *
 * Description :
 *
 * Decisions   :
 *
 * History     : 19980104: Christ Vriens, creation (win32).
 *               19970618: Erik Niessen, Kees Teunissen, creation (Linux)
 *               20000410: Alex Sinitsyn, replacement READ_CD with READ_10.
 *               20000630: Alex Sinitsyn, Gerrit Scholl, integrate Win32 and Linux
 *               20050218: Samuel Kvasnica, quick hack to support Linux v3 SG_IO
 *                         in HandleScsiCmd() (see #ifdef LINUX_V3_SG_IO)
 *               20050517: Gerrit Scholl, clean-up Samuel's hack in order to prepare
 *                         for making previous sg_header method and SCSI_OFF obsolete
 *               20051012: Gerrit Scholl, clean-up: remove obsolete sg_header method
 *                         and SCSI_OFF (removed #ifdef LINUX_V3_SG_IO)
 */

/* INCLUDE FILES */
#include <stdio.h>

#ifdef  WIN32
struct _RPC_ASYNC_STATE;    /* make compiler happy (TODO: ok??) */
#include <windows.h>

#elif defined( LINUX )
#include <sys/time.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>         /* needed by LINUX kernel v3 SG_IO api */
#include <linux/../scsi/sg.h>

#elif defined(  NETBSD )
#include <sys/time.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/scsiio.h>
#include <dev/scsipi/scsipi_all.h>
#endif

#include "ucterror.h"           /* subset of uct_core.h */

#ifdef  WIN32
/* Prepare DLLEXPORT for use of a SCSI DLL (udf_scsi.dll).
 * However, this MAY BE CANCELLED in scsiapi.h
 * by a #undef SCSI_USE_DLL  !!
 */
#define DLLEXPORT __declspec(dllexport) /* for "scsiapi.h" */
#endif

#include "scsiapi.h"    /* depends on DLLEXPORT */

#ifdef  SCSI_DEVICE     /* depends on scsiapi.h, compile for SCSI device only */

#ifdef  WIN32
#include "wnaspi32.h"   /* depends on scsiapi.h */
#endif


#if   defined( WIN32 ) || defined( NETBSD )
#define SG_BIG_BUFF ((64*2048)+20)

#elif defined( LINUX )
/** #define SG_BIG_BUFF ((32*1024)+18)  **/
/** #define SG_BIG_BUFF ((64*2048)+20)  **/
/** #define SG_BIG_BUFF ((64*2048)+20)  **/
/**  or take default value as defined in sg.h   **/
#endif

/* GLOBAL VARIABLES */
static char lastErrorMessage[5001]; // Should be large enough.
static UCHAR cmd[SG_BIG_BUFF];

/* scsi Api Version: MIND that scsiApiVersion must be equal to
 * udf_test version (app_version), see checkScsiApiVersion().
 * This is to avoid incompatible dll interface problems.
 */
static char *scsiApiVersion = "1.5r6 distribution src";

/* FUNCTIONS */
extern void DLLEXPORT getScsiApiVersion( char  *saVersion,
                                         size_t sizeSaVersion )
{   size_t sLen = strlen(scsiApiVersion);

    if( (1 + sLen) > sizeSaVersion )
    { sprintf(lastErrorMessage,
        "getScsiApiVersion: Scsi Api Version too long: \"%s\",\n"
            "\t\t   max %u characters.\n",
            scsiApiVersion, sizeSaVersion - 1 );
      fprintf(stdout, "%s", lastErrorMessage);
      fflush(stdout);
      fprintf(stderr, "%s", lastErrorMessage);
      uctExit(EXIT_PROGRAM_ERROR);
    }
    strcpy(saVersion, scsiApiVersion);
}


static void scsiAddDataBuffer(PBYTE DataBuffer, ULONG BufferLength,
                              bool ConvertToAscii)
{
    ULONG Cnt;
    char st[25];

    strcat(lastErrorMessage,
      "\n Sense key ---+                                  ASC---+   +---ASCQ  \n"
        "              |                                        |   |         \n"
        "              V                                        V   V         \n"
        "      00  01  02  03  04  05  06  07   08  09  0A  0B  0C  0D  0E  0F\n"
        "      ---------------------------------------------------------------");
    for (Cnt = 0; Cnt < BufferLength; Cnt++)
    {
        if ((Cnt % 16) == 0)
        {   sprintf(st, "\n %03X  ", Cnt);
            strcat(lastErrorMessage, st);
        }
        else if ((Cnt % 8) == 0)
        {   strcat(lastErrorMessage, " ");
        }
        if (ConvertToAscii && (DataBuffer[Cnt] >= 0x20) && (DataBuffer[Cnt] <= 0x7E))
        {   sprintf(st, " %c  ", DataBuffer[Cnt]);
        }
        else
        {   sprintf(st, "%02X  ", DataBuffer[Cnt]);
        }
        strcat(lastErrorMessage, st);
    }
    strcat(lastErrorMessage, "\n");
}


#ifdef  WIN32   /* HandleScsiCmd() group of functions */
/* ========================================================================== */

/* scsiCheckAndPrintStatus:
 * Determine if failure or success.
 * If failure, then print status, etc. results
 */
static bool scsiCheckAndPrintStatus(BYTE SRB_Status, BYTE SRB_TargStat,
                                    BYTE *SenseArea, BYTE SenseLength)
{
#ifdef  SCSI_TESTING
{ int n;
  fprintf(stdout,
    "  scsi status     : 0x%02X 0x%02X", SRB_Status, SRB_TargStat);
  for( n = 0; n < SenseLength; n++ )
  { fprintf(stdout, " %02X", SenseArea[n]);
  }
  fprintf(stdout, "\n");
  fflush(stdout);
}
#endif  /** SCSI_TESTING **/

    if(   SRB_Status != SS_COMP     /* not 'complete without error' */
       && SRB_Status != SS_ERR )    /* not 'complete with error' */
    {
        sprintf(lastErrorMessage, "Error 0x%02X when executing SCSI command.\n"
            "See file wnaspi32.h for list of SRB status codes.", SRB_Status);
        return FALSE;
    }
    if( SRB_TargStat != STATUS_GOOD )
    {
        sprintf(lastErrorMessage, "SCSI status and target status: 0x%02X 0x%02X",
                                   SRB_Status, SRB_TargStat);
        switch( SRB_TargStat )
        {
        case STATUS_CHKCOND:
            strcat(lastErrorMessage, " (Check Condition");
            if (SenseArea[2] == KEY_RECERROR) /* RECOVERED DATA ERROR. */
            { strcat(lastErrorMessage, ", recovered");
            }
            strcat(lastErrorMessage, ")");
            break;
        case STATUS_BUSY:
            strcat(lastErrorMessage, " (Busy)");
            break;
        }
        strcat(lastErrorMessage, "\nSense Info -- consult SCSI spec for details\n");
        scsiAddDataBuffer(SenseArea, SenseLength, FALSE);
        /* maybe still return with TRUE status,
         * e.g. for recovered KEY_RECERROR
         */
        return(   SenseArea[2] == KEY_RECERROR  /* RECOVERED DATA ERROR and */
               && SRB_Status == SS_COMP );      /*   complete without error */
    }
    return (SRB_Status == SS_COMP); /* TRUE for 'complete without error' */

}   /* end scsiCheckAndPrintStatus() */


/* GLOBAL VARIABLES */
static HANDLE scsiASPICompletionEvent;

/* process a complete scsi cmd. Use the generic scsi interface.
 */
static bool HandleScsiCmd(              /* WIN32 */
                ScsiImpUse *scsiImpUse,
                ULONG       cmd_len,    /* CDB length, must be 6, 10 or 12      */
                ULONG       in_size,    /* Number of bytes to write to device   */
                UCHAR      *i_buff,     /* Input buffer = CDB directly followed */
                                        /* with bytes to write to device        */
                ULONG       out_size,   /* Number of bytes to read from device  */
                                        /* (integral multiple of sectorsize)    */
                UCHAR      *o_buff )    /* Output buffer                        */
{
  DWORD aspiStatus;
  DWORD aspiEventStatus;
  SRB_ExecSCSICmd srb;
  bool retryCondition;
  int  retryCount = 0;

  lastErrorMessage[0] = '\0';   /* clean error message */

#ifdef SCSI_TESTING
  fprintf(stdout, "HandleScsiCmd %lu:%lu %lu "
                    "c: 0x%02X %lu %lu 0x%lX 0x%lX\n",
    scsiImpUse->hostAdapter, scsiImpUse->scsiId, cmd_len,
    i_buff[0], in_size, out_size, i_buff, o_buff);
  fflush(stdout);
#endif  /** SCSI_TESTING **/

#define HSC_MAX_RETRY 10        /* local to HandleScsiCmd() */
  do        /* retry loop */
  {
    memset(&srb, 0, sizeof(SRB_ExecSCSICmd));
    srb.SRB_Cmd = SC_EXEC_SCSI_CMD;
    srb.SRB_HaId = (BYTE) scsiImpUse->hostAdapter;
    srb.SRB_Target = (BYTE) scsiImpUse->scsiId;
    if( in_size > 0 )                   /* transfer to device */
    {   srb.SRB_Flags = SRB_DIR_OUT;
        srb.SRB_BufLen = in_size;
        srb.SRB_BufPointer = i_buff + cmd_len;
    }
    else                                /* transfer from device */
    {   srb.SRB_Flags = SRB_DIR_IN;
        srb.SRB_BufLen = out_size;
        srb.SRB_BufPointer = o_buff;
    }
    srb.SRB_Flags |= SRB_EVENT_NOTIFY;
    srb.SRB_SenseLen = SENSE_LEN;
    srb.SRB_CDBLen = (UCHAR)cmd_len;
//  srb.SRB_PostProc = (void (*)()) scsiASPICompletionEvent;
    srb.SRB_PostProc = scsiASPICompletionEvent; /* no more casting for new wnaspi32.h */
    memcpy(srb.CDBByte, i_buff, cmd_len);
    aspiStatus = SendASPI32Command((LPSRB) &srb);
    if (srb.SRB_Status == SS_PENDING)
    {
#ifdef SCSI_TESTING
        fprintf(stdout, "HandleScsiCmd srb status SS_PENDING\n");
        fflush(stdout);
#endif  /**  SCSI_TESTING **/
        aspiEventStatus = WaitForSingleObject(scsiASPICompletionEvent, INFINITE);
    }

    retryCondition = (   srb.SRB_Status == SS_ERR
                      && srb.SRB_TargStat == STATUS_BUSY);
    if( retryCount < HSC_MAX_RETRY && retryCondition )
    {   fprintf(stdout, "\nScsi busy retry %2d: 0x%02X 0x%02X\n",
            retryCount+1, srb.SRB_Status, srb.SRB_TargStat);
        fflush(stdout);
    }
  } while( retryCount++ < HSC_MAX_RETRY && retryCondition );    /* end retry loop */

  return scsiCheckAndPrintStatus(srb.SRB_Status, srb.SRB_TargStat,
                                 srb.SenseArea, SENSE_LEN);

}   /** end HandleScsiCmd() for WIN32 **/

#elif   defined( LINUX )    \
     || defined( NETBSD )   /* HandleScsiCmd() group of functions */

/* ========================================================================== */

/* SenseKeyDescr translates the value of the SenseKey to the ASCII description
 * Table 69+70, Page 119-120: Sense Key (0x0-0x7) + (0x8-0xf) descriptions
 */
static char *SenseKeyDescr(UCHAR SenseKey)
{
  static char KeyInfo[256];

  switch(SenseKey) {
   case 0x0: sprintf(KeyInfo, "NO SENSE");         break;
   case 0x1: sprintf(KeyInfo, "RECOVERED Error");  break;
   case 0x2: sprintf(KeyInfo, "NOT Ready");        break;
   case 0x3: sprintf(KeyInfo, "MEDIUM Error");     break;
   case 0x4: sprintf(KeyInfo, "HARDWARE Error");   break;
   case 0x5: sprintf(KeyInfo, "ILLEGAL Request");  break;
   case 0x6: sprintf(KeyInfo, "UNIT Attention");   break;
   case 0x7: sprintf(KeyInfo, "DATA Protect");     break;
   case 0x8: sprintf(KeyInfo, "BLANK Check");      break;
   case 0x9: sprintf(KeyInfo, "VENDOR-Specific");  break;
   case 0xa: sprintf(KeyInfo, "COPY Aborted");     break;
   case 0xb: sprintf(KeyInfo, "ABORTED Command");  break;
   case 0xc: sprintf(KeyInfo, "EQUAL");            break;
   case 0xd: sprintf(KeyInfo, "VOLUME Overflow");  break;
   case 0xe: sprintf(KeyInfo, "MISCOMPARE");       break;
   case 0xf: sprintf(KeyInfo, "RESERVED");         break;
   default:  sprintf(KeyInfo, "SENSE KEY OUT OF RANGE"); break;
  }
  return(KeyInfo);
}

/* AddSenseInfo translates the combination of values of ASC and ASCQ to the ASCII
 * description of the additional sense description
 * Table 71, Page 121-124: ASC and ASCQ assignments
 */
static char *AddSenseInfo( UCHAR ASC, UCHAR ASCQ )
{
  static char AddSenseCode[256];
  UCHAR       ASCh = ASC;

  if( ASCh >= 0x80 ) ASCh = 0x80;

  /* first the default text, saves a lot of 'else' clauses
   */
  sprintf(AddSenseCode,
            "Not yet Implemented, ASC = 0x%02X ASCQ = 0x%02X", ASCh, ASCQ);

  switch(ASCh) {
   case 0x00:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "No Additional Sense Information");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "FileMark Detected");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "End-of-Partition/Medium Detected");
     else if (ASCQ==0x03) sprintf(AddSenseCode, "Setmark Detected");
     else if (ASCQ==0x04) sprintf(AddSenseCode, "Beginning-of-Partition/Medium Detected");
     else if (ASCQ==0x05) sprintf(AddSenseCode, "End-of-Data Detected");
     else if (ASCQ==0x06) sprintf(AddSenseCode, "I/O Process Terminated");
     else if (ASCQ==0x11) sprintf(AddSenseCode, "AUDIO Play Operation in Progress");
     else if (ASCQ==0x12) sprintf(AddSenseCode, "AUDIO Play Operation Paused");
     else if (ASCQ==0x13) sprintf(AddSenseCode, "AUDIO Play Operation Successfully Completed");
     else if (ASCQ==0x14) sprintf(AddSenseCode, "AUDIO Play Operation Stopped due to Error");
     else if (ASCQ==0x15) sprintf(AddSenseCode, "NO Current Audio Status to Return");
     break;
   case 0x01:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "NO INDEX/SECTOR Signal");
     break;
   case 0x02:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "NO SEEK Complete");
     break;
   case 0x03:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Peripheral Device WRITE Fault");
     break;
   case 0x04:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Logical Unit NOT Ready, Cause NOT Reportable");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Logical Unit is in Process of Becoming Ready");
     else if (ASCQ==0x02) sprintf(AddSenseCode,
                            "Logical Unit NOT Ready, Initializing Command Required");
     else if (ASCQ==0x03) sprintf(AddSenseCode,
                            "Logical Unit NOT Ready, Manual Intervention Required");
     else if (ASCQ==0x04) sprintf(AddSenseCode, "Logical Unit NOT Ready, Format in Progress");
     break;
   case 0x05:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Logical Unit does NOT Respond to Selection");
     break;
   case 0x06:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "NO Reference Position Found");
     break;
   case 0x07:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Multiple Peripheral Devices Selected");
     break;
   case 0x08:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Logical Unit Communication Failure");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Logical Unit Communication Time-Out");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Logical Unit Communication Parity Error");
     break;
   case 0x09:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Track Following Error");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Trackking Servo Failure");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Focus Servo Failure");
     else if (ASCQ==0x03) sprintf(AddSenseCode, "Spindle Servo Failure");
     break;
   case 0x0a:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Error LOG Overflow");
     break;
   case 0x0c:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Write Error");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Write Error Recovered with Auto Reallocation");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Write Error - Auto Reallocation Failed");
     break;
   case 0x10:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "ID CRC or ECC Error");
     break;
   case 0x11:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Unrecoverd READ Error");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "READ Retries Exhausted");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Error too Long to Correct");
     else if (ASCQ==0x03) sprintf(AddSenseCode, "Multiple READ Errors");
     else if (ASCQ==0x04) sprintf(AddSenseCode, "Unrecoverd READ Error - Auto Reallocate Failed");
     else if (ASCQ==0x05) sprintf(AddSenseCode, "L-EC Uncorrectable Error");
     else if (ASCQ==0x06) sprintf(AddSenseCode, "CIRC Unrecovered Error");
     else if (ASCQ==0x07) sprintf(AddSenseCode, "DATA Resynchronization Error");
     else if (ASCQ==0x08) sprintf(AddSenseCode, "Incomplete block READ");
     else if (ASCQ==0x09) sprintf(AddSenseCode, "NO gap found");
     else if (ASCQ==0x0a) sprintf(AddSenseCode, "Miscorrected Error");
     else if (ASCQ==0x0b) sprintf(AddSenseCode, "Unrecoverd READ Error - Recommend Reassignment");
     else if (ASCQ==0x0c) sprintf(AddSenseCode, "Unrecoverd READ Error - Recommend REWRITE the Data");
     break;
   case 0x12:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Address Mark NOT Found for ID Field");
     break;
   case 0x13:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Address Mark NOT Found for Data Field");
     break;
   case 0x14:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Recorded Entity NOT Found");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Record NOT Found");
     break;
   case 0x15:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Random Positioning Error");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Mechanical Positioning Error");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Positioning Error Detected by READ of medium");
     break;
   case 0x16:
     if (ASCQ==0x00) sprintf(AddSenseCode, "DATA Synchronization Mark Error");
     break;
   case 0x17:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Recovered Data with NO Error Correction Applied");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Recovered Data with Retries");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Recovered Data with Positive Head Offset");
     else if (ASCQ==0x03) sprintf(AddSenseCode, "Recovered Data with Negative Head Offset");
     else if (ASCQ==0x04) sprintf(AddSenseCode, "Recovered Data with Retries and/or CIRC Applied");
     else if (ASCQ==0x05) sprintf(AddSenseCode, "Recovered Data using previous Sector ID");
     else if (ASCQ==0x06) sprintf(AddSenseCode, "Recovered Data without ECC - Data auto-reallocated");
     else if (ASCQ==0x07) sprintf(AddSenseCode, "Recovered Data without ECC - recommend reassignment");
     else if (ASCQ==0x08) sprintf(AddSenseCode, "Recovered Data without ECC - recommend rewrite");
     break;
   case 0x18:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Recovered Data with Error Correction Applied");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Recovered Data with Err Corr & Retries Applied");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Recovered Data - Data auto-reallocate");
     else if (ASCQ==0x05) sprintf(AddSenseCode, "Recovered Data - recommend reassignment");
     else if (ASCQ==0x06) sprintf(AddSenseCode, "Recovered Data - recommend rewrite");
     break;
   case 0x19:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Defect List Error");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Defect List NOT Available");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Defect List Error in Primary List");
     else if (ASCQ==0x03) sprintf(AddSenseCode, "Defect List Error in Grown List");
     break;
   case 0x1a:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Parameter List length Error");
     break;
   case 0x1b:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Synchronous Data Transfer Error");
     break;
   case 0x1c:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Defect List NOT Found");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Primary Defect List NOT Found");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Grown Defect List NOT Found");
     break;
   case 0x1d:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Miscompare during VERIFY Operation");
     break;
   case 0x1e:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Recovered ID with ECC Correction");
     break;
   case 0x20:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Invalid Command Operating Code");
     break;
   case 0x21:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Logical Block Address out of Range");
     break;
   case 0x22:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Illegal Function");
     break;
   case 0x24:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Invalid Field in CDB");
     break;
   case 0x25:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Logical Unit NOT Supported");
     break;
   case 0x26:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Invalid Field in Parameter List");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Parameter NOT supported");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Parameter value invalid");
     else if (ASCQ==0x03) sprintf(AddSenseCode, "Threshold parameters NOT supported");
     break;
   case 0x27:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "WRITE Protected");
     break;
   case 0x28:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "NOT Ready to Ready Transition, Medium may have Changed");
     break;
   case 0x29:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Power ON, Reset, or Bus Device Reset Occurred");
     break;
   case 0x2a:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Parameters Changed");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Mode Parameters Changed");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "LOG Parameters Changed");
     break;
   case 0x2b:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "COPY CANNOT Execute since host CANNOT Disconnect");
     break;
   case 0x2c:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Command Sequence Error");
     break;
   case 0x2d:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Overwrite Error on Update in Place");
     break;
   case 0x2f:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Commands Cleared by another Initiator");
     break;
   case 0x30:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Incompatible Medium Installed");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "CANNOT Read Medium - Unknown Format");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "CANNOT Read Medium - Incompatible Format");
     break;
   case 0x31:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "MEDIUM Format Corrupted");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "FORMAT Command Failed");
     break;
   case 0x32:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "NO Defect Spare Location Available");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Defect List Update Failure");
     break;
   case 0x33:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Tape Length Error");
     break;
   case 0x36:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Ribbon, Ink, or Toner Failure");
     break;
   case 0x37:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Rounded Parameter");
     break;
   case 0x39:
     if (ASCQ==0x00) sprintf(AddSenseCode, "Saving Parameters NOT Supported");
     break;
   case 0x3a:
     if (ASCQ==0x00) sprintf(AddSenseCode, "MEDIUM NOT Present");
     break;
   case 0x3b:
     if (ASCQ==0x00) sprintf(AddSenseCode, "Sequential Positioning Error");
     break;
   case 0x3d:
     if (ASCQ==0x00) sprintf(AddSenseCode, "Invalid Bits in Identify Message");
     break;
   case 0x3e:
     break;
   case 0x3f:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Target Operating Conditions have Changed");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Microcode has been changed");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Changed Operating Definition");
     else if (ASCQ==0x03) sprintf(AddSenseCode, "Inquiry data has changed");
     break;
   case 0x40:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "RAM Failure (should use 40 NN)");
     else if (ASCQ>=0x80) sprintf(AddSenseCode, "Diagnostic Failure on Component %d",ASCQ);
     break;
   case 0x41:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Data Path Failure (should use 40 NN)");
     break;
   case 0x42:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Power ON or SELF-Test Failure (should use 40 NN)");
     break;
   case 0x43:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Message Error");
     break;
   case 0x44:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Internal Target Failure");
     break;
   case 0x45:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Select or Reselect Failure");
     break;
   case 0x46:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Unsuccesfull Soft Reset");
     break;
   case 0x47:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "SCSI Parity Error");
     break;
   case 0x48:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Initiator Detected Error Message Received");
     break;
   case 0x49:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Invalid Message error");
     break;
   case 0x4a:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Command Phase Error");
     break;
   case 0x4b:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Data Phase Error");
     break;
   case 0x4c:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Logical Unit Failed Self-Configuration");
     break;
   case 0x4e:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Overlap Commands Attempted");
     break;
   case 0x50:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "WRITE Append error");
     break;
   case 0x51:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "ERASE Failure");
     break;
   case 0x52:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Cartridge Fault");
     break;
   case 0x53:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Media LOAD or EJECT Failed");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Unload Tape Failure");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "MEDIUM Removal Prevented");
     break;
   case 0x54:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "SCSI to Host System Interface Failure");
     break;
   case 0x55:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "System resource Failure");
     break;
   case 0x57:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Unable to Recover Table-of-Contents");
     break;
   case 0x58:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Generation does NOT Exist");
     break;
   case 0x59:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Updated Block Read");
     break;
   case 0x5a:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Operator Request or State Change Input (Unspecified)");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Operator MEDIUM Removal Request");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Operator selected WRITE Protect");
     else if (ASCQ==0x03) sprintf(AddSenseCode, "Operator selected WRITE Permit");
     break;
   case 0x5b:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "LOG Exception");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Threshold condition met");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "LOG counter at maximum");
     else if (ASCQ==0x03) sprintf(AddSenseCode, "LOG List codes exhausted");
     break;
   case 0x5c:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "RPL Status Change");
     else if (ASCQ==0x01) sprintf(AddSenseCode, "Spindles synchronized");
     else if (ASCQ==0x02) sprintf(AddSenseCode, "Spindles NOT synchronized");
     break;
   case 0x60:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Lamp Failure");
     break;
   case 0x61:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Video Acquisition Error");
     break;
   case 0x62:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Scan Head Positioning Error");
     break;
   case 0x63:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "End of User Area Encountered on this Track");
     break;
   case 0x64:
     if      (ASCQ==0x00) sprintf(AddSenseCode, "Illegal Mode for this Track");
     break;
   case 0x80: sprintf(AddSenseCode,
                "Vendor Specific ADDITIONAL SENSE INFO: ASC=0x%02X, ASCQ=0x%02X",ASC,ASCQ);
     break;
   default: sprintf(AddSenseCode,
                "UNKNOWN ADDITIONAL SENSE INFO: ASC=0x%02X, ASCQ=0x%02X",ASC,ASCQ);
     break;
  }
  return(AddSenseCode);

}   /** end AddSenseInfo() for LINUX, NETBSD **/


#ifdef  LINUX   /* HandleScsiCmd(), LINUX */

/* process a complete scsi cmd. Use the generic sg_io_hdr scsi interface.
 */
static bool HandleScsiCmd(                  /* LINUX_V3_SG_IO api */
                ScsiImpUse  *scsiImpUse,
                UINT         cmd_len,       /* command length   */
                UINT         in_size,       /* input data size  */
                UCHAR       *i_buff,        /* input buffer     */
                UINT         out_size,      /* output data size */
                UCHAR       *o_buff )       /* output buffer    */
{
    int              ioResult=0;
    int              scsiStat=0;
    int              responseCode=0;
    int              SenseKey=0;
    int              ASC=0;
    int              ASCQ=0;
    UCHAR            o_buff_dummy[1];
    UCHAR            senseBuffer[SG_MAX_SENSE];
    struct sg_io_hdr sg_io_hd;

    lastErrorMessage[0] = '\0';     /* clean error message */

#ifdef SCSI_TESTING
  fprintf(stdout, "HandleScsiCmd %lu:%lu %lu "
                    "c: 0x%02X %lu %lu 0x%lX 0x%lX\n",
    scsiImpUse->hostAdapter, scsiImpUse->scsiId, cmd_len,
    i_buff[0], in_size, out_size, i_buff, o_buff);
  fflush(stdout);
#endif  /** SCSI_TESTING **/

    /* safety asserts on programming errors:
     * do not support transfer in both directions
     * with a single command (SG_DXFER_TO_FROM_DEV)
     */
    UCTASSERT(   cmd_len != 0           /* need a command */
              && i_buff  != NULL        /* need an input buffer */
              && (in_size == 0 || out_size == 0)
              && (out_size == 0) == (o_buff == NULL) );

    if( o_buff == NULL )        /* out_size == 0 */
    { o_buff = o_buff_dummy;    /* valid pointer required */
    }

    /* generic scsi device header construction
     * support Linux v3 SG_IO ioctl interface using sg_io_hdr
     */
    memset(senseBuffer, 0, sizeof(senseBuffer));    // clear sense buffer
    memset(&sg_io_hd,   0, sizeof(sg_io_hd));       // clear command

    sg_io_hd.interface_id = 'S';                    // generic SCSI
    sg_io_hd.dxfer_direction =                      // not SG_DXFER_TO_FROM_DEV
          (    in_size == 0
           && out_size == 0) ? SG_DXFER_NONE        // no transfer
        :     (in_size != 0) ? SG_DXFER_TO_DEV      // transfer to device
                             : SG_DXFER_FROM_DEV;   // transfer from device
    sg_io_hd.cmd_len   = cmd_len;                   // SCSI command length
    sg_io_hd.mx_sb_len = sizeof(senseBuffer);       // sense buffer allocation
    sg_io_hd.iovec_count = 0;                       // no scatter gather
    sg_io_hd.dxfer_len = (in_size!=0) ? in_size     // transfer byte count
                                      : out_size;
    sg_io_hd.dxferp = (in_size!=0) ? i_buff + cmd_len // to device transfer buffer
                                   : o_buff;         // from device transfer buffer
    sg_io_hd.cmdp = i_buff;                         // input buffer
    sg_io_hd.sbp  = senseBuffer;                    // sense buffer
    sg_io_hd.timeout = 60000;                       // 1 minute for one command

    /* excecute command
     */
    ioResult = ioctl(scsiImpUse->fd, SG_IO, &sg_io_hd);
    if(   ioResult != 0
       || sg_io_hd.host_status != 0
       || sg_io_hd.driver_status != 0 )
    { sprintf(lastErrorMessage, "%s"    /* cat message */
        "-> SG_IO ioctl returned %d, %d, %d "
        "(return value, host_status, driver_status)\n",
          lastErrorMessage, ioResult, sg_io_hd.host_status,
          sg_io_hd.driver_status);
      /** patch: sometimes, driver status SG_ERR_DRIVER_SENSE and
       **        sense data is returned while sg_io_hd.status is
       **        equal to STATUS_GOOD.
       **        patch: Ignore SG_ERR_DRIVER_SENSE and check
       **               sg_io_hd.status and sense buffer instead
       **/
      if( (sg_io_hd.driver_status & 0xF) != 0x08 )  // SG_ERR_DRIVER_SENSE
      { return FALSE;       /* failed */
      }
    }

    /* check scsi status (CHECK CONDITION, etc.)
     * and sense buffer
     */
    scsiStat = sg_io_hd.status & 0x3E;      /* strip off vendor bits */

#ifdef  SCSI_TESTING
{ int n;
  fprintf(stdout,
      "\n%s  ioctl hex result: %02X %02X : ",
      lastErrorMessage, ioResult, scsiStat);
  for( n = 0; n < sizeof(senseBuffer); n++ )
  { fprintf(stdout, " %02X", senseBuffer[n]);
  }
  fprintf(stdout, "\n");
  fflush(stdout);
}
#endif  /** SCSI_TESTING **/

    /* test scsiStat STATUS_GOOD for return value,
     * but first check sense buffer
     */
    if( sg_io_hd.sb_len_wr  > 0 )       /* sense buffer returned */
    { int n;
      responseCode = senseBuffer[0] & 0x7F;
      sprintf(lastErrorMessage, "%s"            /* 'strcat' */
        "-> SG_IO returned: status: 0x%02X,"
             " sense response code: 0x%02X, sense length: %d\n"
        "->   hex sense :",
          lastErrorMessage, sg_io_hd.status,
          responseCode, sg_io_hd.sb_len_wr);
      for( n = 0; n < sg_io_hd.sb_len_wr; n++ )
      { sprintf(lastErrorMessage, "%s%s %02X",      /* 'strcat' */
          lastErrorMessage,
          (n != 0 && (n % 20) == 0) ? "\n\t\t" : "",
          senseBuffer[n]);
      }
       strcat(lastErrorMessage, "\n");

      SenseKey = senseBuffer[2] & 0x0F;
      ASC    = senseBuffer[12];
      ASCQ   = senseBuffer[13];
      sprintf(lastErrorMessage, "%s"            /* 'strcat' */
        "-> Scsi Status ............. = 0x%02X\n"
        "-> Sense Description ....... = %s\n"
        "-> Add. Sense Info =0x%02X,0x%02X= %s\n",
        lastErrorMessage, scsiStat, SenseKeyDescr(SenseKey),
        ASC, ASCQ, AddSenseInfo(ASC,ASCQ) );

      /* no return FALSE here !!
       * depends on scsiStatus
       */
    }

#ifdef SCSI_TESTING
    fprintf(stdout, "%s", lastErrorMessage);
    fflush(stdout);
    fprintf(stderr, "%s", lastErrorMessage);
#endif  /** SCSI_TESTING **/

    if( scsiStat != STATUS_GOOD )
    { return FALSE;         /* failure */
    }
    return TRUE;            /* success */

}   /** end HandleScsiCmd() for LINUX_V3_SG_IO **/


#elif defined( NETBSD )     /* HandleScsiCmd(), NETBSD */

/*
 * Process the SCSI command using the SCSI command IOCTL
 */
static bool HandleScsiCmd(                            /* NETBSD */
                ScsiImpUse  *scsiImpUse,
                UINT         cmd_len,       /* command length   */
                UINT         in_size,       /* input data size  */
                UCHAR       *i_buff,        /* input buffer     */
                UINT         out_size,      /* output data size */
                UCHAR       *o_buff )       /* output buffer    */
{
    int     sense_key, error_code;
    int     ASC;
    int     ASCQ;
    scsireq_t   req;

    lastErrorMessage[0] = '\0';     /* clean error message */

    /* safety checks : need a cmd_len != 0 and need an input buffer != NULL */
    if (!cmd_len || !i_buff) {
        return (bool) FALSE;        /* flag failure */
    }

    if (!o_buff) {
        out_size = 0;       /* no output buffer, no output size */
    }

    if (out_size && in_size) {
        fprintf(stderr, "NetBSD: in/out sizes assertion failed;"
                        " in_size = %d, out_size = %d\n", in_size, out_size);
        uctExit(EXIT_UCT_ASSERT_FAILED);
    };

    if (in_size == 0 && out_size == 0) return (bool) TRUE;      /* check ready */

    bzero(&req, sizeof(req));   /* clear the request and sense blob */

    memcpy(req.cmd, i_buff, cmd_len);
    req.cmdlen  = cmd_len;
    if (out_size) {
        req.databuf = o_buff;
        req.datalen = out_size;
        req.flags   = SCCMD_READ;
    } else {
        req.databuf = i_buff + cmd_len;
        req.datalen = in_size;
        req.flags   = SCCMD_WRITE;
    };
    req.timeout = 60000;    /* arbitrary, but probably enough */
    req.senselen = SENSEBUFLEN;

    if (ioctl(scsiImpUse->fd, SCIOCCOMMAND, &req) == -1) {
        perror("SCIOCCOMMAND returned");
        return (bool) FALSE;    /* ioctl() failed */
    };
//  error_code = req.sense[0] & 0x7F;
    error_code = req.status;
    if( error_code != STATUS_GOOD ) {   /* sense */
        sense_key = req.sense[ 2] & 0x0f;
        ASC       = req.sense[12];
        ASCQ      = req.sense[13];
        sprintf(lastErrorMessage,
            "SCSI error\n"
            "\terror code        0x%02x\n"
            "\tsense description %s\n"
            "\taditional info    0x%02x, 0x%02x : %s\n",
            error_code, SenseKeyDescr(sense_key), ASC, ASCQ,
            AddSenseInfo(ASC, ASCQ));
        return (bool) FALSE;    /* flag failure */
    };

    return (bool) TRUE;         /* flag success */

}   /** end HandleScsiCmd() for NETBSD **/

#endif      /* NETBSD, LINUX HandleScsiCmd() */

#endif      /* WIN32, LINUX, NETBSD HandleScsiCmd() group of functions */


#ifdef  WIN32
extern bool DLLEXPORT scsiInit()
{
    DWORD scsiASPIStatus;
    scsiASPIStatus = GetASPI32SupportInfo();
    if (HIBYTE(LOWORD(scsiASPIStatus)) != SS_COMP)
    {
        strcpy(lastErrorMessage, "Could not initialize ASPI for Win32.\n"
            "Possible causes :\n"
            "- ASPI32 service is not running.\n");
        return FALSE;
    }

    if ((scsiASPICompletionEvent = CreateEvent(NULL, FALSE, FALSE, NULL)) == NULL)
    {
        strcpy(lastErrorMessage, "Error when creating ASPI completion event.\n");
        return FALSE;
    }
    return TRUE;
}
#endif  /* WIN32 */


#ifdef  WIN32

/* queryHostAdapter() returns the host adapter inquiry
 * (SC_HA_INQUIRY) command info in a SRB_HAInquiry structure.
 * getHostAdapterInfo() returns FALSE if SendASPI32Command() does not
 * return the SRB status SS_COMP.
 * (a copy of the value returned by SendASPI32Command() is
 *  in pHaInfo->SRB_Status).
 */
static bool queryHostAdapter( UCHAR hostAdapterID,
                              SRB_HAInquiry *pHaInfo )
{
    memset(pHaInfo, 0, sizeof(SRB_HAInquiry));
    pHaInfo->SRB_Cmd  = SC_HA_INQUIRY;
    pHaInfo->SRB_HaId = hostAdapterID;

    if( SendASPI32Command((LPSRB) pHaInfo) != SS_COMP )
    { sprintf(lastErrorMessage,
        "Error status 0x%02X when inquiring host adapter %u.\n"
        "See wnaspi32.h for list of SRB status codes.\n",
            pHaInfo->SRB_Status, hostAdapterID);
      return FALSE;
    }
    return TRUE;
}

/* scsiNrHostAdapters()
 * Determines nmb of host adapters by calling queryHostAdapter()
 * for one or more host adapters. Note that some may not be
 * operable, e.g. unused: HA id 0 and 2, used: HA id 1 and 3.
 * This example wil return 4 host adapters 0 till 3 included.
 */
extern bool DLLEXPORT scsiNrHostAdapters(int* nrHostAdapters)
{
    int h;          // ASPI host adapter number must fit in a BYTE
    SRB_HAInquiry myHaInfo;

    for( h = 0,            myHaInfo.HA_Count = 0;
         h < MAX_NUM_HA && myHaInfo.HA_Count == 0;
         h++ )
    { if( !queryHostAdapter((UCHAR) h, &myHaInfo) )
      { return FALSE;       /* error handled in queryHostAdapter() */
      }
#undef  SCSI_DEBUG_SHOWSCSI     /* normally #undef */
#ifdef  SCSI_DEBUG_SHOWSCSI
      printf("SCSI DEBUG: h: %u, nrHostAdapters: %d\n",
             h, myHaInfo.HA_Count);
      fflush(stdout);
#endif  /** SCSI_DEBUG_SHOWSCSI **/
    }
    *nrHostAdapters = myHaInfo.HA_Count;
    return TRUE;
}

#endif  /* WIN32 */


/* copyAndNormalise():
 * copy string and remove trailing spaces.
 * dest must allocate maxlen+1 chars (1 extra for termination)
 */
static void inqCopyAndNormalise(char *dest, BYTE *src, int maxlen)
{   size_t sz;
    strncpy(dest, (char*) src, maxlen );
    dest[maxlen] = '\0';
    sz = strlen(dest);
    while( (--sz) >= 0 && dest[sz] == ' ' )
    {}
    dest[sz+1] = '\0';
}

/* output arguments can be NULL pointers
 * SPC-3r17 6.4 (for all kind of device types)
 */
extern bool DLLEXPORT scsiInquiry( ScsiImpUse *scsiImpUse,
                                   BYTE  *pDeviceType,
                                   BYTE  *pInqVersion,
                                   char   vendorIdentification[8+1],
                                   char   productIdentification[16+1],
                                   char   productRevisionlevel[4+1],
                                   USHORT versionDescriptors[8])
{
    int   n;
    BYTE  replyBuf[INQUIRY_REPLY_LEN];
    UCHAR cmdblk[6] =
    {
            SCSI_INQUIRY,       // INQUIRY Command - SPC-3r17 6.4
            0,                  // reserved/CmdDt/EVPD
            0,                  // page code or Operation Code
                                // reserved/allocation length
        (UCHAR)(INQUIRY_REPLY_LEN>>8),  // MSB
        (UCHAR) INQUIRY_REPLY_LEN,      // LSB
            0                   // Control
    };

    memcpy(cmd, cmdblk, sizeof(cmdblk));
    if( !HandleScsiCmd(scsiImpUse, sizeof(cmdblk), 0, cmd,
                       INQUIRY_REPLY_LEN, replyBuf) )
    {   return FALSE;
    }
    /* Peripheral device type
     */
    if( pDeviceType != NULL )
    {   *pDeviceType = (BYTE)(replyBuf[0] & 0x1F);
    }
    /* INQUIRY version (was 'ANSI' version).
     */
    if( pInqVersion != NULL )
    {   *pInqVersion = (BYTE)(replyBuf[2]);
    }
    /* Next 3 fields are left aligned ASCI bytes padded with spaces.
     * Note that the appropriate output variables has
     * one byte more for termination.
     * Remove trailing spaces in output argument.
     */
    if( vendorIdentification != NULL )
    { inqCopyAndNormalise(vendorIdentification, &replyBuf[8], 8);
    }
    if( productIdentification != NULL )
    { inqCopyAndNormalise(productIdentification, &replyBuf[16], 16);
    }
    if( productRevisionlevel != NULL )
    { inqCopyAndNormalise(productRevisionlevel, &replyBuf[32], 4);
    }

    /* Up to 8 Version Descriptors (USHORT)
     */
    if( versionDescriptors != NULL )
    {   for( n = 0; n < 8; n++ )        /* 0-7 == VD 1-8 */
        { int x = 58 + (2 * n);         /* VD 1 at 58 and 59 */
          versionDescriptors[n] = (USHORT) ( (replyBuf[x] << 8)
                                            + replyBuf[x+1] );
        }
    }
    return TRUE;
}   /* end scsiInquiry() */

/* test if unit is ready, only status reply.
 * can be issued for Direct Access and MM devices.
 * MMC-4 6.49 (mmc4r02h draft, February 20, 2004)
 */
extern bool DLLEXPORT testUnitReady(ScsiImpUse *scsiImpUse)
{
    UCHAR cmdblk[6] =
    {
            SCSI_TST_U_RDY, // TEST UNIT READY Command MMC-4 6.49
            0,              // reserved
            0,              // reserved
            0,              // reserved
            0,              // reserved
            0               // Control
    };

    memcpy(cmd, cmdblk, sizeof(cmdblk));
    return HandleScsiCmd(scsiImpUse, sizeof(cmdblk), 0, cmd, 0, NULL);
}


extern bool DLLEXPORT scsiModeSense10( ScsiImpUse *scsiImpUse,
                                       BYTE* senseMediumType )
{       /* read Mode parameter header only */
    BYTE  replyBuf[MP_HEADER10_LEN];
    UCHAR cmdblk[10] =
    {
            SCSI_MODE_SENSE10,  // MODE SENSE(10) Command (8.2.11)
            0,                  // reserved
            0x3F,               // return all pages
            0,                  // reserved
            0,                  // reserved
            0,                  // reserved
            0,                  // reserved
            0,                  // allocation length - MSB
            MP_HEADER10_LEN,    // allocation length - LSB
            0                   // Control
    };

    memcpy(cmd, cmdblk, sizeof(cmdblk));
    if( !HandleScsiCmd(scsiImpUse, sizeof(cmdblk), 0, cmd,
                       MP_HEADER10_LEN, replyBuf) )
    {   return FALSE;
    }

    *senseMediumType = replyBuf[2];
    return TRUE;
}

/* READ DISC INFORMATION Command, MMC-5 6.27
 */
extern bool DLLEXPORT
scsiReadDiscInformation(ScsiImpUse *scsiImpUse,
                        BYTE   *discStatus,
                        BYTE   *stateLastSession,
                        bool   *pIsErasableBitSet,
                        BYTE   *firstTrackOnDisc,
                        USHORT *nrOfSessions,
                        USHORT *firstTrackLastSession,
                        USHORT *lastTrackLastSession,
                        BYTE   *bgFormatStatus,
                        BYTE   *discType)
{   /* OPC info NOT included in reply */
    BYTE  replyBuf[READDISCINFO_REPLY_LEN];
    UCHAR cmdblk[10] =
    {
            SCSI_READDISCINFO,      // MMC-4 6.26
            0,                      // reserved
            0,                      // reserved
            0,                      // reserved
            0,                      // reserved
            0,                      // reserved
            0,                      // reserved
                                    // allocation length
        (UCHAR)(READDISCINFO_REPLY_LEN>>8), // MSB
        (UCHAR) READDISCINFO_REPLY_LEN,     // LSB
            0                       // Control
    };

    memcpy(cmd, cmdblk, sizeof(cmdblk));
    if( !HandleScsiCmd(scsiImpUse, sizeof(cmdblk), 0, cmd,
                       READDISCINFO_REPLY_LEN, replyBuf) )
    {   return FALSE;
    }

    *discStatus             =   (BYTE)    (replyBuf[2] & 0x3);
    *stateLastSession       =   (BYTE)   ((replyBuf[2] & 0xC) >> 2);
    *pIsErasableBitSet      =      isBitOn(replyBuf[2], 4);
    *firstTrackOnDisc       =              replyBuf[3];

    *nrOfSessions           = (USHORT)(    replyBuf[4]          /* LSB */
                                       + ( replyBuf[9] << 8));  /* MSB */

    *firstTrackLastSession  = (USHORT)(    replyBuf[5]          /* LSB */
                                       + (replyBuf[10] << 8));  /* MSB */

    *lastTrackLastSession   = (USHORT)(    replyBuf[6]          /* LSB */
                                       + (replyBuf[11] << 8));  /* MSB */

    *bgFormatStatus         =   (BYTE)(    replyBuf[7] & 0x3);
    *discType               =              replyBuf[8];
    return TRUE;
}


/* GET CONFIGURATION command.
 * MMC-4 6.6 (mmc4r02h draft, February 20, 2004)
 * currentProfile : Profile Number, see 5.3.1, Table 75
 */
extern bool DLLEXPORT
scsiGetConfiguration( ScsiImpUse *scsiImpUse,
                      USHORT     *currentProfile )
{
    BYTE  replyBuf[GETCONFIG_HEADER_REPLY_LEN];
    UCHAR cmdblk[10] =
    {
        SCSI_GETCONFIGURATION,      // GET CONFIGURATION command (MMC-4)
        0x00,                       // Reserved/RT
        0x00,                       // Starting Feature Number - MSB
        0x00,                       // Starting Feature Number - LSB
        0x00,                       // Reserved
        0x00,                       // Reserved
        0x00,                       // Reserved
                                    // Allocation Length, header only
    (UCHAR)(GETCONFIG_HEADER_REPLY_LEN>>8), // MSB
    (UCHAR) GETCONFIG_HEADER_REPLY_LEN,     // LSB
        0x00                        // Control
    };

    memcpy(cmd, cmdblk, sizeof(cmdblk));
    if( !HandleScsiCmd(scsiImpUse, sizeof(cmdblk), 0, cmd,
                       GETCONFIG_HEADER_REPLY_LEN, replyBuf) )
    {   return FALSE;
    }

    /*       see: MMC-4 6.6.2.1, Table 244
     * values in: MMC-4 5.3.1, Table 75
     */

    *currentProfile = (USHORT) ( (replyBuf[6] << 8)
                                + replyBuf[7] );
    return TRUE;
}


/* READ DVD STRUCTURE command.
 * MMC-5 6.29, 6.29.2.1, February 1, 2005
 */
extern bool DLLEXPORT scsiReadDvdStructure( ScsiImpUse *scsiImpUse,
                            BYTE  *pBookType, BYTE *pPartVersion,
                            BYTE  *pDiscSize, BYTE *pMaxRate,
                            BYTE  *pNmbOfLayers,
                            BYTE  *pTrackPathOTP,   /* 0: PTP, 1: OTP */
                            ULONG *pStartPsnDataArea,
                            ULONG *pEndPsnDataArea,
                            ULONG *pEndPsnDataL0)
{
    BYTE  replyBuf[READDVDSTRUCTF00_REPLY_LEN];
    UCHAR cmdblk[12] =
    {
        SCSI_READDVDSTRUCTURE,      // READ DVD STRUCTURE command (MMC-4)
        0x00,                       // Reserved / Sub-command 0x0 == DVD
        0x00,                       // Address - MSB
        0x00,                       // Address
        0x00,                       // Address
        0x00,                       // Address - LSB
        0x00,                       // Layer Number
        0x00,                       // Format 00h
                                    // Allocation Length
    (UCHAR)(READDVDSTRUCTF00_REPLY_LEN>>8), // MSB
    (UCHAR) READDVDSTRUCTF00_REPLY_LEN,     // LSB
        0x00,                       // AGID / Reserved
        0x00                        // Control
    };

    memcpy(cmd, cmdblk, sizeof(cmdblk));
    if( !HandleScsiCmd(scsiImpUse, sizeof(cmdblk), 0, cmd,
                       READDVDSTRUCTF00_REPLY_LEN, replyBuf) )
    {   return FALSE;
    }

    /* data from lead-in ??
     */
    *pBookType     = (BYTE) (0xF & (replyBuf[4+ 0] >> 4));
    *pPartVersion  = (BYTE) (0xF &  replyBuf[4+ 0]);
    *pDiscSize     = (BYTE) (0xF & (replyBuf[4+ 1] >> 4));
    *pMaxRate      = (BYTE) (0xF &  replyBuf[4+ 1]);
    *pNmbOfLayers  = (BYTE) (0x3 &  replyBuf[4+ 2] >> 5);
    *pTrackPathOTP = (BYTE) (0x1 &  replyBuf[4+ 2] >> 4);
    *pStartPsnDataArea = ((ULONG) (replyBuf[4+ 5] << 16) /* MSB */
                        + (ULONG) (replyBuf[4+ 6] <<  8)
                        + (ULONG)  replyBuf[4+ 7] );     /* LSB */
    *pEndPsnDataArea =   ((ULONG) (replyBuf[4+ 9] << 16) /* MSB */
                        + (ULONG) (replyBuf[4+10] <<  8)
                        + (ULONG)  replyBuf[4+11] );     /* LSB */
    *pEndPsnDataL0 =     ((ULONG) (replyBuf[4+13] << 16) /* MSB */
                        + (ULONG) (replyBuf[4+14] <<  8)
                        + (ULONG)  replyBuf[4+15] );     /* LSB */
    return TRUE;

}   /* end scsiReadDvdStructure() */

/* READ DVD STRUCTURE Format 20h command.
 * MMC-5 6.29.2.19, February 1, 2005
 */
extern bool DLLEXPORT scsiReadDvdStructF20( ScsiImpUse *scsiImpUse,
                                            ULONG *pL0DataZoneCapacity)
{
    BYTE  replyBuf[READDVDSTRUCTF00_REPLY_LEN];
    UCHAR cmdblk[12] =
    {
        SCSI_READDVDSTRUCTURE,      // READ DVD STRUCTURE command (MMC-5)
        0x00,                       // Reserved / Sub-command 0x0 == DVD
        0x00,                       // Address - MSB
        0x00,                       // Address
        0x00,                       // Address
        0x00,                       // Address - LSB
        0x00,                       // Layer Number
        0x20,                       // Format 20h
                                    // Allocation Length,
    (UCHAR)(READDVDSTRUCTF20_REPLY_LEN>>8), // MSB
    (UCHAR) READDVDSTRUCTF20_REPLY_LEN,     // LSB
        0x00,                       // AGID / Reserved
        0x00                        // Control
    };

    memcpy(cmd, cmdblk, sizeof(cmdblk));
    if( !HandleScsiCmd(scsiImpUse, sizeof(cmdblk), 0, cmd,
                       READDVDSTRUCTF20_REPLY_LEN, replyBuf) )
    {   return FALSE;
    }
    *pL0DataZoneCapacity =   ((ULONG) (replyBuf[4+4] << 24) /* MSB */
                            + (ULONG) (replyBuf[4+5] << 16)
                            + (ULONG) (replyBuf[4+6] <<  8)
                            + (ULONG)  replyBuf[4+7] );     /* LSB */
    return TRUE;

}   /* end scsiReadDvdStructF20() */


/* getNmbBE():
 * Get big endian number from buffer, 1 till 4 bytes.
 * pFirst points to first (MSB) byte.
 */
static ULONG getNmbBE( BYTE *pFirst, int nBytes )
{   ULONG number = 0;

    UCTASSERT(   pFirst != NULL
              && nBytes > 0 && nBytes <= 4 );

    while( nBytes-- != 0 )
    { number = (number << 8) + *(pFirst++);
    }
    return number;
}   /* end getNmbBE() */

/* SCSI Multimedia Commands - 3 5.24 (MMC-3),
 * draft revision 10g, November 12, 2001.
 * Special CD/DVD commands including LRA extension.
 * Mt. Fuji spec is included by MMC-3 now.
 */
extern bool DLLEXPORT scsiReadTrackInformation(
    ScsiImpUse *scsiImpUse,     USHORT  reqTrack,
    USHORT  *driveSupportTotalLength,
    USHORT  *trackNumber,       USHORT *sessionNumber,
    bool    *pIsDamageBitSet,   bool   *pIsCopyBitSet,
    bool    *pIsRTBitSet,       bool   *pIsBlankBitSet,
    bool    *pIsPacketBitSet,   bool   *pIsFPBitSet,
    bool    *pIsNWA_VBitSet,    bool   *pIsLRA_VBitSet,
    BYTE    *pLJRS,
    BYTE    *trackMode,         BYTE   *dataMode,
    ULONG   *fixedPacketSize,
    ULONG   *trackStartAddress, ULONG  *trackLength,
    ULONG   *freeBlocks,
    ULONG   *nextWritableAddress, ULONG *lastRecordedAddress,
    ULONG   *nextLayerJumpAddress, ULONG *lastLayerJumpAddress,
    BYTE    *reserved04,        BYTE *reserved07,
    BYTE    *reserved34,        BYTE *reserved35 )
{
    BYTE  replyBuf[READTRACKINFO_REPLY_LEN];
    UCHAR cmdblk[10];
    int replyLen;

    cmdblk[0] = SCSI_READTRACKINFO;     // Read Track Information Command (MMC-3)
    cmdblk[1] = 0x01;                   // reserved/track - use track number
    cmdblk[2] = 0x00;                               // lba/track nmb - MSB
    cmdblk[3] = 0x00;                               // lba/track nmb
    cmdblk[4] = (BYTE) (reqTrack >> 8);             // lba/track nmb
    cmdblk[5] = (BYTE)  reqTrack;                   // lba/track nmb - LSB
    cmdblk[6] = 0;                                  // reserved
    cmdblk[7] = (BYTE)(READTRACKINFO_REPLY_LEN>>8); // allocation length - MSB
    cmdblk[8] = (BYTE) READTRACKINFO_REPLY_LEN;     // allocation length - LSB
    cmdblk[9] = 0;                                  // Control

    memcpy(cmd, cmdblk, sizeof(cmdblk));
    if( !HandleScsiCmd(scsiImpUse, sizeof(cmdblk), 0, cmd,
                       READTRACKINFO_REPLY_LEN, replyBuf) )
    {   return FALSE;
    }

    /* note that Track Info Length does not include the
     * 2 bytes of Track Info Length itself, so add them in
     * order to avoid (or raise?) confusion.
     * It is assumed that replyLen is at least 28.
     */
    *driveSupportTotalLength =
        (USHORT)(2 + getNmbBE(&replyBuf[0],2)); /* MSB 00-01 LSB */

    /* actual reply length is MIN() of allocated size
     * and drive supported size
     */
    replyLen = ((*driveSupportTotalLength) < READTRACKINFO_REPLY_LEN)
             ?  (*driveSupportTotalLength) : READTRACKINFO_REPLY_LEN;

    *trackNumber   = replyBuf[2];               /* byte 02 LSB */
    if( replyLen > 32 )                         /* MMC-3 5.24 */
    { *trackNumber =
         (USHORT) (  (*trackNumber)
                   + (replyBuf[32] << 8));      /* byte 32 MSB */
    }
    *sessionNumber  = replyBuf[3];              /* byte 03 LSB */
    if( replyLen > 33 )                         /* MMC-3 5.24 */
    { *sessionNumber =
         (USHORT) (  (*sessionNumber)
                   + (replyBuf[33] << 8));      /* byte 33 MSB */
    }
    *reserved04      =         replyBuf[4];             /* byte 04 */
                                                        /* byte 05 */
    *trackMode       = (BYTE) (replyBuf[5] & 0xF);      /*  bit 0-3 */
    *pIsCopyBitSet   = isBitOn(replyBuf[5], 4);         /*  bit 4 */
    *pIsDamageBitSet = isBitOn(replyBuf[5], 5);
    *pLJRS           = (BYTE) (replyBuf[5] & 0xC0)>> 6; /*  bit 6-7 */
                                                        /* byte 06 */
    *dataMode        = (BYTE) (replyBuf[6] & 0xF);      /*  bit 0-3 */
    *pIsFPBitSet     = isBitOn(replyBuf[6], 4);
    *pIsPacketBitSet = isBitOn(replyBuf[6], 5);
    *pIsBlankBitSet  = isBitOn(replyBuf[6], 6);
    *pIsRTBitSet     = isBitOn(replyBuf[6], 7);
                                                        /* byte 07 */
    *pIsNWA_VBitSet  = isBitOn(replyBuf[7], 0);
    *pIsLRA_VBitSet  = isBitOn(replyBuf[7], 1);         /* LRA extension */
    *reserved07      =         replyBuf[7] & 0xFC;

    *trackStartAddress   = getNmbBE(&replyBuf[8],4);    /* MSB 08-11 LSB */
    *nextWritableAddress = getNmbBE(&replyBuf[12],4);   /* MSB 12-15 LSB */
    *freeBlocks          = getNmbBE(&replyBuf[16],4);   /* MSB 16-19 LSB */
    *fixedPacketSize     = getNmbBE(&replyBuf[20],4);   /* MSB 20-23 LSB */
    *trackLength         = getNmbBE(&replyBuf[24],4);   /* MSB 24-27 LSB */

    /* It is assumed that replyLen is at least 28.
     * More bytes is conditional, so test replyLen.
     */
    *lastRecordedAddress = (replyLen > 31)
                        ? getNmbBE(&replyBuf[28],4) : 0;  /* MSB 28-31 LSB */
    /* MSB byte of Track and Session Number, see above       byte 32-33 */
    *reserved34 = (replyLen > 34) ? replyBuf[34]    : 0;    /* byte 34  */
    *reserved35 = (replyLen > 35) ? replyBuf[35]    : 0;    /* byte 35  */

    /* Mt. Fuji Layer Jump recording
     */
    *nextLayerJumpAddress = (replyLen > 43)
                    ? getNmbBE(&replyBuf[40],4) : 0;    /* MSB 40-43 LSB */
    *lastLayerJumpAddress = (replyLen > 47)
                    ? getNmbBE(&replyBuf[44],4) : 0;    /* MSB 44-47 LSB */
    return TRUE;

}   /* end scsiReadTrackInformation() */


/* This function is shared for:
 *  Direct Access devices like HDD, some MO, etc.
 *  MM devices like CD, DVD, MO
 *
 * see MMC-4 6.23, READ CAPACITY command
 *  (in older specs, this command
 *   was called: READ CD RECORDED CAPACITY).
 */
extern bool DLLEXPORT
scsiReadCapacity(ScsiImpUse *scsiImpUse,
                 ULONG      *lastValidSector,
                 ULONG      *sectorLength)
{
    BYTE  replyBuf[READCAPACITY_REPLY_LEN];
    UCHAR cmdblk[10] =
    {
            SCSI_READCAPACITY,  // Read Capacity (MMC-3 5.1.10)
            0,                  // reserved/reserved/RELADR
            0,                  // lba - MSB
            0,                  // lba
            0,                  // lba
            0,                  // lba - LSB
            0,                  // reserved
            0,                  // reserved
            0,                  // reserved
            0                   // Control
    };
    memcpy(cmd, cmdblk, sizeof(cmdblk));
    if( !HandleScsiCmd(scsiImpUse, sizeof(cmdblk), 0, cmd,
                       READCAPACITY_REPLY_LEN, replyBuf) )
    {   return FALSE;
    }
    *lastValidSector = getNmbBE(&replyBuf[0],4);    /* MSB 00-03 LSB */
    *sectorLength    = getNmbBE(&replyBuf[4],4);    /* MSB 04-07 LSB */
    return TRUE;
}   /* end scsiReadCapacity() */

static bool scsiReadCD(ScsiImpUse *scsiImpUse,
                       const ULONG LBA, const ULONG nrSectors,
                       const ULONG sectorLength, PBYTE trbuf)
{   /* CDD 3600 READ CD command, optional scsi command */
    ULONG   Nbytes = nrSectors * sectorLength;
    UCHAR   cmdblk[12];

    cmdblk[0] = SCSI_READCD;        // READ CD - MMC-4 6.24
    cmdblk[1] = 0,                  // reserved/sector type/DAP/RelAdr
    cmdblk[2] = (UCHAR)(LBA>>24);       // starting lba - MSB
    cmdblk[3] = (UCHAR)(LBA>>16);       // starting lba
    cmdblk[4] = (UCHAR)(LBA>>8);        // starting lba
    cmdblk[5] = (UCHAR) LBA;            // starting lba - LSB
    cmdblk[6] = (UCHAR)(nrSectors>>16); // transfer length in blocks - MSB
    cmdblk[7] = (UCHAR)(nrSectors>>8);  // transfer length in blocks
    cmdblk[8] = (UCHAR) nrSectors;      // transfer length in blocks - LSB
    cmdblk[9] = 0x10;                   // SYNC/etc., set Read User Data
    cmdblk[10] = 0;                     // reserved/sub-channel selection bits
    cmdblk[11] = 0;                     // Control

#ifdef SCSI_TESTING
    fprintf(stdout, "scsiReadCD %lu %lu\n", LBA, nrSectors);
    fflush(stdout);
#endif  /** SCSI_TESTING **/

    memcpy(cmd, cmdblk, sizeof(cmdblk));
    if( !HandleScsiCmd(scsiImpUse, sizeof(cmdblk), 0, cmd, Nbytes, trbuf) )
    {   /* returns status message in lastErrorMessage, add: */
        strcat(lastErrorMessage, "scsiReadCD: Read CD command failed\n");
        return FALSE;
    }
    return TRUE;

}   /* end scsiReadCD() */

/* Read nrSectors sectors from disk device at logical block address LBA
 * Table 123 - READ(10) command, page 168
 */
static bool scsiRead10(ScsiImpUse *scsiImpUse,
                       const ULONG LBA, const ULONG nrSectors,
                       const ULONG sectorLength, PBYTE trbuf)
{
    ULONG   Nbytes = nrSectors * sectorLength;
    UCHAR   cmdblk[10];

    cmdblk[0] = SCSI_READ10;            // Read (10) Command (9.2.6)
    cmdblk[1] = 0x00;                   // FUA/RELADR - read from cache
    cmdblk[2] = (UCHAR)(LBA>>24);       // starting lba - MSB
    cmdblk[3] = (UCHAR)(LBA>>16);       // starting lba
    cmdblk[4] = (UCHAR)(LBA>>8);        // starting lba
    cmdblk[5] = (UCHAR) LBA;            // starting lba - LSB
    cmdblk[6] = 0;                      // reserved
    cmdblk[7] = (UCHAR)(nrSectors>>8);  // transfer length in blocks - MSB
    cmdblk[8] = (UCHAR) nrSectors;      // transfer length in blocks - LSB
    cmdblk[9] = 0;                      // Control

#ifdef SCSI_TESTING
    fprintf(stdout, "scsiRead10  %lu %lu\n", LBA, nrSectors);
    fflush(stdout);
#endif  /** SCSI_TESTING **/

    if( (ULONG)((USHORT) nrSectors) != nrSectors )  /* does not fit in USHORT */
    {   sprintf(lastErrorMessage, "scsiRead10: Too many blocks: %lu\n", nrSectors);
        return FALSE;
    }

    memcpy(cmd, cmdblk, sizeof(cmdblk));
    if( !HandleScsiCmd(scsiImpUse, sizeof(cmdblk), 0, cmd, Nbytes, trbuf) )
    {   /* returns status message in lastErrorMessage, add: */
        strcat(lastErrorMessage, "scsiRead10: Scsi Read10 command failed\n");
        return FALSE;
    }
    return TRUE;

}   /* end scsiRead10() */


extern bool DLLEXPORT scsiRead(ScsiImpUse *scsiImpUse,
                               ULONG sectorLength, ULONG sector,
                               ULONG nrSectors, PBYTE pbMem)
{
    static BYTE  backwardComp = 0;
           bool  result = FALSE;

///** overdone, scsiRead10() or scsiReadCD() will perform own logging **/
//#ifdef SCSI_TESTING
//  fprintf(stdout, "scsiRead   %lu %lu\n", sector, nrSectors);
//  fflush(stdout);
//#endif    /** SCSI_TESTING **/

    /* backwardComp: static backwards compatibility switch.
     * 1: Default MMC-1, use scsiRead10() for read.
     * 2: Backwards compatibility mode for e.g. some older CD-ROM
     *    drives, use scsiReadCD() for read.
     * 0: Initial situation, choice between 1 and 2 has not yet
     *    been made, first try 1, if error then try 2.
     */
    switch( backwardComp )
    {
    case 1:     /* Default MMC-1, use scsiRead10() */
        result = scsiRead10(scsiImpUse, sector, nrSectors,
                            sectorLength, pbMem);
        break;
    case 2:     /* backwards compatibility mode */
        result = scsiReadCD(scsiImpUse, sector, nrSectors,
                            sectorLength, pbMem);
        break;
    case 0:     /* choice between 1 and 2 not yet made, try 1 scsiRead10() */
        result = scsiRead10(scsiImpUse, sector, nrSectors,
                            sectorLength, pbMem);
        if( result == TRUE )
        {   backwardComp = 1;       /* choose 1 */
        }
        else    /* try 2, scsiReadCD() */
        {   result = scsiReadCD(scsiImpUse, sector, nrSectors,
                                sectorLength, pbMem);
            if( result == TRUE )
            {   backwardComp = 2;   /* choose 2 */
            }
            /* if result == FALSE, then choice not yet made, so
             * backwardComp is still 0, try to choose next time.
             */
        }
        break;
    }
    if( backwardComp == 0 && result == FALSE )
    {   strcat(lastErrorMessage, "scsiRead: Both scsiRead10 and scsiReadCD failed\n");
    }
#ifdef SCSI_TESTING
    fprintf(stdout, "scsiRead result: %lu\n", result);
    fflush(stdout);
#endif  /** SCSI_TESTING **/
    return result;
}


#ifdef  WIN32
/* aspiPrintHostAdapterInfo() returns the Maximum number of
 * SCSI targets for this hostAdapterID in (*pMaxScsiTagets).
 * This value is zero if this host adapter is not operable.
 */
extern bool DLLEXPORT aspiPrintHostAdapterInfo(UCHAR  hostAdapterID,
                                               UCHAR *pMaxScsiTagets)
{
    SRB_HAInquiry hostAdapterInfo;

    *pMaxScsiTagets = 0;
    if( !queryHostAdapter(hostAdapterID, &hostAdapterInfo) )
    { return FALSE;     /* error handled in queryHostAdapter() */
    }
    fprintf(stdout, "\nInquiry of host adapter %d", hostAdapterID);

    /* command ok, print result and find Max nmb of SCSI targets.
     */
    *pMaxScsiTagets = hostAdapterInfo.HA_Unique[3];

    if( hostAdapterInfo.HA_Count == 0 )
    {   /* Host adapter does not exist/not operable
         * (*pMaxScsiTagets) == 0 in this case
         */
        fprintf(stdout, "\t-> no device connected\n");
        *pMaxScsiTagets = 0;
    }
    else if( *pMaxScsiTagets == 0 )
    {   /* if HA_Unique[3] 'not set', the default is 8
         */
        *pMaxScsiTagets = 8;
    }

    if( *pMaxScsiTagets != 0 )  /* host adapter operable */
    { char terminatedString[16+1];
      /* terminatedString[] size:
       *    1 + max of HA_Identifier[] and HA_ManagerId[] sizes.
       * HA_Identifier[] and HA_ManagerId[] may not be '\0' terminated,
       * so normalize and terminate first using inqCopyAndNormalise().
       */
      inqCopyAndNormalise( terminatedString,
                           hostAdapterInfo.HA_Identifier, 16 );
      fprintf(stdout,
        "\n  Host adapter ASCII string     : \"%s\"\n", terminatedString);

      inqCopyAndNormalise( terminatedString,
                           hostAdapterInfo.HA_ManagerId, 16 );
      fprintf(stdout,
          "  ASPI manager ASCII string     : \"%s\"\n"
          "  Max supported SCSI targets    : %4d",
          terminatedString, hostAdapterInfo.HA_Unique[3]);
      if( hostAdapterInfo.HA_Unique[3] != *pMaxScsiTagets )
      { fprintf(stdout, "\t -> changed to %u", *pMaxScsiTagets );
      }

      /* Max transfer length in HA_Unique[4-7] (little endian)
       * (round down to Kbytes).
       */
      fprintf(stdout,
        "\n  Max transfer length           : %4d kB\n"
          "  Host adapter SCSI Id          : %4d\n",
          (*((ULONG *) &(hostAdapterInfo.HA_Unique[4])))/1024,
          hostAdapterInfo.HA_SCSI_ID);

      fprintf(stdout, "  Host adapter buffer alignment : ");
      if( hostAdapterInfo.HA_Unique[0] == 0 )
           fprintf(stdout, " not required\n");
      else fprintf(stdout, "%4d bytes\n",
                   1 + hostAdapterInfo.HA_Unique[0]);

      fprintf(stdout,
        "  Residual byte count reporting :  %ssupported\n",
            (isBitOn(hostAdapterInfo.HA_Unique[2],1))
                ? "" : "not ");
      fprintf(stdout,
        "  Total number of host adapters : %4d\n",
          hostAdapterInfo.HA_Count);
    }
    fprintf(stdout, "\n");
    fflush(stdout);     /* avoid scsi dll and main program output mix up */
    return TRUE;

}   /* end aspiPrintHostAdapterInfo() WIN32 */

#endif  /* WIN32 */


extern bool DLLEXPORT printDeviceType(ScsiImpUse *scsiImpUse, UCHAR deviceType)
{
    fprintf(stdout, "     Host adapter %d, SCSI Id %d  =  %s (0x%02X)\n",
                        scsiImpUse->hostAdapter, scsiImpUse->scsiId,
                        SCSI_DTYPE_TEXT(deviceType), deviceType);
    fflush(stdout);     /* avoid scsi dll and main program output mix up */
    return(TRUE);
}


extern const char DLLEXPORT *scsiGetLastErrorMessage(void)
{
    return lastErrorMessage;
}


#else   /** NOT SCSI_DEVICE (NO_SCSI)**/

/* udf_scsi_dummyFunction():
 * For NO_SCSI: avoid empty scsi library (udf_scsi.lib or libudfsc.a)
 * Some compilers/linkers complain about empty scsi library for NO_SCSI.
 * Let's keep them happy by supplying a dummy function.
 */
extern int udf_scsi_dummyFunction()
{ return 10;
}

#endif  /** (NOT) SCSI_DEVICE **/

