//***************************************************************************
//
// Name:    SCSIDEFS.H
//
// Description: SCSI definitions ('C' Language)
//

#ifndef __SCSIDEFS_H__
#define __SCSIDEFS_H__

//***************************************************************************

//***************************************************************************
//       %%% TARGET STATUS VALUES %%%
//          SCSI Architecture Model - 3 (SAM-3) Revision 14, 21 September 2004
//          section 5.3.1
//***************************************************************************
#define STATUS_GOOD       0x00   // Status Good
#define STATUS_CHKCOND    0x02   // Check Condition
#define STATUS_CONDMET    0x04   // Condition Met
#define STATUS_BUSY       0x08   // Busy
#define STATUS_INTERM     0x10   // Intermediate
#define STATUS_INTCDMET   0x14   // Intermediate-condition met
#define STATUS_RESCONF    0x18   // Reservation conflict
#define STATUS_OBSOLETE   0x22   // Obsolete
#define STATUS_TSFULL     0x28   // Task Set full
#define STATUS_ACACTIV    0x30   // ACA ACTIVE
#define STATUS_TABORT     0x40   // Task aborted

//***************************************************************************
//      %%% SCSI MISCELLANEOUS EQUATES %%%
//***************************************************************************
#define MAXLUN              7    // Maximum Logical Unit Id
#define MAXTARG             7    // Maximum Target Id
#define MAX_SCSI_LUNS      64    // Maximum Number of SCSI LUNs
#define MAX_NUM_HA          8    // Maximum Number of SCSI HA's

//---------------------------------------------------------------------------
//
//       %%% SCSI COMMAND OPCODES %%%
//
//---------------------------------------------------------------------------
//***************************************************************************
//     %%% Commands for all Device Types %%%
// March 8, 2004: brought in line with MMC-4,
//                (mmc4r02h draft, February 20, 2004)
//***************************************************************************
#define SCSI_CHANGE_DEF   0x40   // Change Definition (Optional)
#define SCSI_COMPARE      0x39   // Compare (O)
#define SCSI_COPY         0x18   // Copy (O)
#define SCSI_COP_VERIFY   0x3A   // Copy and Verify (O)
#define SCSI_INQUIRY      0x12   // Inquiry (MANDATORY)
#define SCSI_LOG_SELECT   0x4C   // Log Select (O)
#define SCSI_LOG_SENSE    0x4D   // Log Sense (O)
#define SCSI_MODE_SEL6    0x15   // Mode Select 6-byte (Device Specific)
#define SCSI_MODE_SEL10   0x55   // Mode Select 10-byte (Device Specific)
#define SCSI_MODE_SENSE6  0x1A   // Mode Sense 6-byte (Device Specific)
#define SCSI_MODE_SENSE10 0x5A   // Mode Sense 10-byte (Device Specific)
#define SCSI_READ_BUFF    0x3C   // Read Buffer (O)
#define SCSI_REQ_SENSE    0x03   // Request Sense (MANDATORY)
#define SCSI_SEND_DIAG    0x1D   // Send Diagnostic (O)
#define SCSI_TST_U_RDY    0x00   // TEST UNIT READY (MANDATORY)
#define SCSI_WRITE_BUFF   0x3B   // Write Buffer (O)

#define INQUIRY_REPLY_LEN       96 /* Standard INQUIRY data (SCSI-2, 8.2.5.1) */
#define MP_HEADER10_LEN          8 /* Mode param. header(10)(SCSI-2, 8.3.3)  */

//***************************************************************************
//     %%% Commands Unique to Direct Access Devices %%%
//***************************************************************************
#define SCSI_FORMAT       0x04   // Format Unit (MANDATORY)
#define SCSI_LCK_UN_CAC   0x36   // Lock Unlock Cache (O)
#define SCSI_PREFETCH     0x34   // Prefetch (O)
#define SCSI_MED_REMOVL   0x1E   // Prevent/Allow medium Removal (O)
#define SCSI_READ6        0x08   // Read 6-byte (MANDATORY)
#define SCSI_READ10       0x28   // Read 10-byte (MANDATORY)
#define SCSI_READCD       0xBE   // Read CD      (OPTIONAL, CDD 3600)
#define SCSI_RD_CAPAC     0x25   // Read Capacity (MANDATORY)
#define SCSI_RD_DEFECT    0x37   // Read Defect Data (O)
#define SCSI_READ_LONG    0x3E   // Read Long (O)
#define SCSI_REASS_BLK    0x07   // Reassign Blocks (O)
#define SCSI_RCV_DIAG     0x1C   // Receive Diagnostic Results (O)
#define SCSI_RELEASE      0x17   // Release Unit (MANDATORY)
#define SCSI_REZERO       0x01   // Rezero Unit (O)
#define SCSI_SRCH_DAT_E   0x31   // Search Data Equal (O)
#define SCSI_SRCH_DAT_H   0x30   // Search Data High (O)
#define SCSI_SRCH_DAT_L   0x32   // Search Data Low (O)
#define SCSI_SEEK6        0x0B   // Seek 6-Byte (O)
#define SCSI_SEEK10       0x2B   // Seek 10-Byte (O)
#define SCSI_SEND_DIAG    0x1D   // Send Diagnostics (MANDATORY)
#define SCSI_SET_LIMIT    0x33   // Set Limits (O)
#define SCSI_START_STP    0x1B   // Start/Stop Unit (O)
#define SCSI_SYNC_CACHE   0x35   // Synchronize Cache (O)
#define SCSI_CLOSETRACKSESSION   0x5B   // CLOSE TRACK/SESSION
#define SCSI_VERIFY       0x2F   // Verify (O)
#define SCSI_WRITE6       0x0A   // Write 6-Byte (MANDATORY)
#define SCSI_WRITE10      0x2A   // Write 10-Byte (MANDATORY)
#define SCSI_WRT_VERIFY   0x2E   // Write and Verify (O)
#define SCSI_WRITE_LONG   0x3F   // Write Long (O)
#define SCSI_WRITE_SAME   0x41   // Write Same (O)

//***************************************************************************
//   %%% Commands Unique to Sequential Access Devices %%%
//***************************************************************************
#define SCSI_ERASE        0x19   // Erase (MANDATORY)
#define SCSI_LOAD_UN      0x1b   // Load/Unload (O)
#define SCSI_LOCATE       0x2B   // Locate (O)
#define SCSI_RD_BLK_LIM   0x05   // Read Block Limits (MANDATORY)
#define SCSI_READ_POS     0x34   // Read Position (O)
#define SCSI_READ_REV     0x0F   // Read Reverse (O)
#define SCSI_REC_BF_DAT   0x14   // Recover Buffer Data (O)
#define SCSI_RESERVE      0x16   // Reserve Unit (MANDATORY)
#define SCSI_REWIND       0x01   // Rewind (MANDATORY)
#define SCSI_SPACE        0x11   // Space (MANDATORY)
#define SCSI_VERIFY_T     0x13   // Verify (Tape) (O)
#define SCSI_WRT_FILE     0x10   // Write Filemarks (MANDATORY)

//***************************************************************************
//      %%% Commands Unique to Printer Devices %%%
//***************************************************************************
#define SCSI_PRINT        0x0A   // Print (MANDATORY)
#define SCSI_SLEW_PNT     0x0B   // Slew and Print (O)
#define SCSI_STOP_PNT     0x1B   // Stop Print (O)
#define SCSI_SYNC_BUFF    0x10   // Synchronize Buffer (O)

//***************************************************************************
//     %%% Commands Unique to Processor Devices %%%
//***************************************************************************
#define SCSI_RECEIVE      0x08   // Receive (O)
#define SCSI_SEND         0x0A   // Send (O)

//***************************************************************************
//    %%% Commands Unique to Write-Once Devices %%%
//***************************************************************************
#define SCSI_MEDIUM_SCN   0x38   // Medium Scan (O)
#define SCSI_SRCHDATE10   0x31   // Search Data Equal 10-Byte (O)
#define SCSI_SRCHDATE12   0xB1   // Search Data Equal 12-Byte (O)
#define SCSI_SRCHDATH10   0x30   // Search Data High 10-Byte (O)
#define SCSI_SRCHDATH12   0xB0   // Search Data High 12-Byte (O)
#define SCSI_SRCHDATL10   0x32   // Search Data Low 10-Byte (O)
#define SCSI_SRCHDATL12   0xB2   // Search Data Low 12-Byte (O)
#define SCSI_SET_LIM_10   0x33   // Set Limits 10-Byte (O)
#define SCSI_SET_LIM_12   0xB3   // Set Limits 10-Byte (O)
#define SCSI_VERIFY10     0x2F   // Verify 10-Byte (O)
#define SCSI_VERIFY12     0xAF   // Verify 12-Byte (O)
#define SCSI_WRITE12      0xAA   // Write 12-Byte (O)
#define SCSI_WRT_VER10    0x2E   // Write and Verify 10-Byte (O)
#define SCSI_WRT_VER12    0xAE   // Write and Verify 12-Byte (O)

//***************************************************************************
//      %%% Commands Unique to CD-ROM Devices %%%
//***************************************************************************
#define SCSI_PLAYAUD_10         0x45  // Play Audio 10-Byte (O)
#define SCSI_PLAYAUD_12         0xA5  // Play Audio 12-Byte 12-Byte (O)
#define SCSI_PLAYAUDMSF         0x47  // Play Audio MSF (O)
#define SCSI_PLAYA_TKIN         0x48  // Play Audio Track/Index (O)
#define SCSI_PLYTKREL10         0x49  // Play Track Relative 10-Byte (O)
#define SCSI_PLYTKREL12         0xA9  // Play Track Relative 12-Byte (O)
#define SCSI_READCAPACITY       0x25  // Read Capacity (MANDATORY)
#define SCSI_READHEADER         0x44  // Read Header (O)
#define SCSI_SUBCHANNEL         0x42  // Read Subchannel (O)
#define SCSI_READTOC_PA         0x43  // Read TOC/PMA/ATIP
#define SCSI_GETCONFIGURATION   0x46  // GET CONFIGURATION  MMC-4
#define SCSI_READDISCINFO       0x51  // Read Disc Information   (CD, DVD)
#define SCSI_READTRACKINFO      0x52  // Read Track Information  (CD, DVD)
#define SCSI_READDVDSTRUCTURE   0xAD  // READ DVD STRUCTURE MMC-4

#define READCAPACITY_REPLY_LEN     8    /* MMC-3 5.16, MMC-5 6.24 */
#define READDISCINFO_REPLY_LEN    32    /* MMC-3 5.19, must be even !! OPC info NOT included */
#define READTOC_PA_REPLY_LEN10   4 + (10*8) /* MMC-3 5.23, max 10 tracks */
//#define READTRACKINFO_REPLY_LEN   36      /* MMC-3 5.24  */
#define READTRACKINFO_REPLY_LEN     48      /* Mt. Fuji 6 rev 0.7 */
#define GETCONFIG_HEADER_REPLY_LEN   8      /* MMC-4 6.6.2.1 */
#define READDVDSTRUCTF00_REPLY_LEN  (4+16)  /* MMC-5 6.29.2.1 */
#define READDVDSTRUCTF20_REPLY_LEN  (4+ 8)  /* MMC-5 6.29.2.19 */
#define READDVDSTRUCTF30_REPLY_LEN  56      /* DVD+RWbasic 22.2 */

//***************************************************************************
//      %%% Commands Unique to Scanner Devices %%%
//***************************************************************************
#define SCSI_GETDBSTAT    0x34   // Get Data Buffer Status (O)
#define SCSI_GETWINDOW    0x25   // Get Window (O)
#define SCSI_OBJECTPOS    0x31   // Object Postion (O)
#define SCSI_SCAN         0x1B   // Scan (O)
#define SCSI_SETWINDOW    0x24   // Set Window (MANDATORY)

//***************************************************************************
//    %%% Commands Unique to Optical Memory Devices %%%
//***************************************************************************
#define SCSI_UpdateBlk    0x3D   // Update Block (O)

//***************************************************************************
//    %%% Commands Unique to Medium Changer Devices %%%
//***************************************************************************
#define SCSI_EXCHMEDIUM   0xA6   // Exchange Medium (O)
#define SCSI_INITELSTAT   0x07   // Initialize Element Status (O)
#define SCSI_POSTOELEM    0x2B   // Position to Element (O)
#define SCSI_REQ_VE_ADD   0xB5   // Request Volume Element Address (O)
#define SCSI_SENDVOLTAG   0xB6   // Send Volume Tag (O)

//***************************************************************************
//     %%% Commands Unique to Communication Devices %%%
//***************************************************************************
#define SCSI_GET_MSG_6    0x08   // Get Message 6-Byte (MANDATORY)
#define SCSI_GET_MSG_10   0x28   // Get Message 10-Byte (O)
#define SCSI_GET_MSG_12   0xA8   // Get Message 12-Byte (O)
#define SCSI_SND_MSG_6    0x0A   // Send Message 6-Byte (MANDATORY)
#define SCSI_SND_MSG_10   0x2A   // Send Message 10-Byte (O)
#define SCSI_SND_MSG_12   0xAA   // Send Message 12-Byte (O)
//---------------------------------------------------------------------------
//
//       %%% END OF SCSI COMMAND OPCODES %%%
//
//---------------------------------------------------------------------------

//***************************************************************************
//      %%% Request Sense Data Format %%%
//***************************************************************************
typedef struct {
 BYTE  ErrorCode;       // Error Code (70H or 71H)
 BYTE  SegmentNum;      // Number of current segment descriptor
 BYTE  SenseKey;        // Sense Key(See bit definitions too)
 BYTE  InfoByte0;       // Information MSB
 BYTE  InfoByte1;       // Information MID
 BYTE  InfoByte2;       // Information MID
 BYTE  InfoByte3;       // Information LSB
 BYTE  AddSenLen;       // Additional Sense Length
 BYTE  ComSpecInf0;     // Command Specific Information MSB
 BYTE  ComSpecInf1;     // Command Specific Information MID
 BYTE  ComSpecInf2;     // Command Specific Information MID
 BYTE  ComSpecInf3;     // Command Specific Information LSB
 BYTE  AddSenseCode;    // Additional Sense Code
 BYTE  AddSenQual;      // Additional Sense Code Qualifier
 BYTE  FieldRepUCode;   // Field Replaceable Unit Code
 BYTE  SenKeySpec15;    // Sense Key Specific 15th byte
 BYTE  SenKeySpec16;    // Sense Key Specific 16th byte
 BYTE  SenKeySpec17;    // Sense Key Specific 17th byte
 BYTE  AddSenseBytes;   // Additional Sense Bytes
} SENSE_DATA_FMT;

//***************************************************************************
//       %%% REQUEST SENSE ERROR CODE %%%
//***************************************************************************
#define SERROR_CURRENT    0x70   // Current Errors
#define SERROR_DEFERED    0x71   // Deferred Errors

//***************************************************************************
//      %%% REQUEST SENSE BIT DEFINITIONS %%%
//***************************************************************************
#define SENSE_VALID       0x80   // Byte 0 Bit 7
#define SENSE_FILEMRK     0x80   // Byte 2 Bit 7
#define SENSE_EOM         0x40   // Byte 2 Bit 6
#define SENSE_ILI         0x20   // Byte 2 Bit 5

//***************************************************************************
//     %%% REQUEST SENSE SENSE KEY DEFINITIONS %%%
//***************************************************************************
#define KEY_NOSENSE       0x00   // No Sense
#define KEY_RECERROR      0x01   // Recovered Error
#define KEY_NOTREADY      0x02   // Not Ready
#define KEY_MEDIUMERR     0x03   // Medium Error
#define KEY_HARDERROR     0x04   // Hardware Error
#define KEY_ILLGLREQ      0x05   // Illegal Request
#define KEY_UNITATT       0x06   // Unit Attention
#define KEY_DATAPROT      0x07   // Data Protect
#define KEY_BLANKCHK      0x08   // Blank Check
#define KEY_VENDSPEC      0x09   // Vendor Specific
#define KEY_COPYABORT     0x0A   // Copy Abort
#define KEY_EQUAL         0x0C   // Equal (Search)
#define KEY_VOLOVRFLW     0x0D   // Volume Overflow
#define KEY_MISCOMP       0x0E   // Miscompare (Search)
#define KEY_RESERVED      0x0F   // Reserved

//***************************************************************************
//      %%% PERIPHERAL DEVICE TYPE DEFINITIONS (SPC-3) (28 Jan 2004)
//***************************************************************************
#define DTYPE_DASD     0x00   // Disk Device
#define DTYPE_SEQD     0x01   // Tape Device
#define DTYPE_PRNT     0x02   // Printer
#define DTYPE_PROC     0x03   // Processor
#define DTYPE_WORM     0x04   // Write-once read-multiple
#define DTYPE_CDVD     0x05   // CD/DVD device
#define DTYPE_SCAN     0x06   // Scanner device
#define DTYPE_OPTI     0x07   // Optical memory device
#define DTYPE_JUKE     0x08   // Medium Changer device
#define DTYPE_COMM     0x09   // Communications device
#define DTYPE_ASCIT8A  0x0A   // 'defined by IT8'
#define DTYPE_ASCIT8B  0x0B   // 'defined by IT8'
#define DTYPE_SACD     0x0C   // Storage Array Controller
#define DTYPE_ESD      0x0D   // Enclosure Services
#define DTYPE_SDASD    0x0E   // Simplified Direct-Access
#define DTYPE_OCRD     0x0F   // Optical Card read/write
#define DTYPE_RESBED   0x10   // Reserved for Bridging Expander
#define DTYPE_OBSD     0x11   // Object-based Storage
#define DTYPE_ADID     0x12   // Automation/Drive Interface
#define DTYPE_RESL     0x13   // (Reserved (low) until
#define DTYPE_RESH     0x1D   //  Reserved (high) included)
#define DTYPE_WNLUD    0x1E   // Well known logical unit
#define DTYPE_UNKNOWN  0x1F   // Unknown or no device type

#define SCSI_DTYPE_TEXT(X)  \
    (((X)==DTYPE_DASD)      ? "Direct-Access Device" :\
     ((X)==DTYPE_SEQD)      ? "Sequential-Access Device" :\
     ((X)==DTYPE_PRNT)      ? "Printer Device." :\
     ((X)==DTYPE_PROC)      ? "Processor Device" :\
     ((X)==DTYPE_WORM)      ? "Write-Once Device" :\
     ((X)==DTYPE_CDVD)      ? "CD/DVD Device"   :\
     ((X)==DTYPE_SCAN)      ? "Scanner Device"  :\
     ((X)==DTYPE_OPTI)      ? "Optical-Memory Device" :\
     ((X)==DTYPE_JUKE)      ? "Medium-Changer Device" :\
     ((X)==DTYPE_COMM)      ? "Communications Device" :\
     ((X)==DTYPE_ASCIT8A || \
      (X)==DTYPE_ASCIT8B)   ? "ASC IT8 Device" :\
     ((X)==DTYPE_SACD)      ? "Storage Array Controller Device" :\
     ((X)==DTYPE_ESD)       ? "Enclosure Services Device" :\
     ((X)==DTYPE_SDASD)     ? "Simplified Direct-Access Device" :\
     ((X)==DTYPE_OCRD)      ? "Optical Card Device" :\
     ((X)==DTYPE_RESBED)    ? "Reserved for Bridging Expander Device" :\
     ((X)==DTYPE_OBSD)      ? "Object-based Storage Device" :\
     ((X)==DTYPE_ADID)      ? "Automation/Drive Interface Device" :\
     ((X)>=DTYPE_RESL && \
      (X)<=DTYPE_RESH)      ? "Reserved Device type" :\
     ((X)==DTYPE_WNLUD)     ? "Well known logical unit Device" :\
     ((X)==DTYPE_UNKNOWN)   ? "Unknown Device type" :\
                              "Illegal Device type" )

#define SCSI_DTYPE_UDF(X) \
    ((X)==DTYPE_DASD  || \
     (X)==DTYPE_SDASD || \
     (X)==DTYPE_SEQD  || \
     (X)==DTYPE_WORM  || \
     (X)==DTYPE_CDVD  || \
     (X)==DTYPE_OPTI  || \
     (X)==DTYPE_JUKE)

//***************************************************************************
//      %%% ANSI APPROVED VERSION DEFINITIONS %%%
//***************************************************************************
#define ANSI_MAYBE     0x0  // Device may or may not be ANSI approved stand
#define ANSI_SCSI1     0x1  // Device complies to ANSI X3.131-1986 (SCSI-1)
#define ANSI_SCSI2     0x2  // Device complies to SCSI-2
#define ANSI_RESLO     0x3  // Reserved (low)
#define ANSI_RESHI     0x7  // Reserved (high)

//***************************************************************************
//      %%% Values FROM MMC-4 %%%
//***************************************************************************

/* discStatus and stateLastSession values:
 */
#define DS_EMPTY        0
#define DS_INCOMPLETE   1
#define DS_COMPLETE     2   /* ok !! */
#define DS_RESERVED     3   /* ok !! */

#define SLS_EMPTY       0
#define SLS_INCOMPLETE  1
#define SLS_RESERVED    2   /* ok !! */
#define SLS_COMPLETE    3   /* ok !! */

/* profile in GET CONFIGURATION Feature Header,
 * MMC-4 6.6.2.1 and 5.3.1 Table 75
 * MMC-5 6.6.??  and 5.3.1 Table 80 (mmc5r1e)
 * All undefined values are "Reserved"
 */
#define PROF_NONREMOV   0x0001  /* Non-removable disc */
#define PROF_REMOV      0x0002  /* Removable disc */
#define PROF_MOERAS     0x0003  /* MO Erasable */
#define PROF_OPTWO      0x0004  /* Optical Write Once */
#define PROF_ASMO       0x0005  /* AS-MO */

#define PROF_CDROM      0x0008  /* CD-ROM */
#define PROF_CDR        0x0009  /* CD-R   */
#define PROF_CDRW       0x000A  /* CD-RW  */

#define PROF_DVDROM     0x0010  /* DVD-ROM */
#define PROF_DVDmR      0x0011  /* DVD-R Sequential Recording */
#define PROF_DVDRAM     0x0012  /* DVD-RAM */
#define PROF_DVDmRWro   0x0013  /* DVD-RW Restricted Overwrite */
#define PROF_DVDmRWsr   0x0014  /* DVD-RW Sequential Recording */
#define PROF_DVDmRdlsr  0x0015  /* DVD-R Dual Layer Sequential Recording DL SR */
#define PROF_DVDmRdllj  0x0016  /* DVD-R Dual Layer Jump Recording      DL LJR */

#define PROF_DVDpRW     0x001A  /* DVD+RW  */
#define PROF_DVDpR      0x001B  /* DVD+R   */

#define PROF_DVDpRWdl   0x002A  /* DVD+RW DL */
#define PROF_DVDpRdl    0x002B  /* DVD+R DL  */

#define PROF_BDROM      0x0040  /* BD-ROM */
#define PROF_BDRsrm     0x0041  /* BD-R SRM Sequential Recording Mode */
#define PROF_BDRrrm     0x0042  /* BD-R RRM     Random Recording Mode */
#define PROF_BDRE       0x0043  /* BD-RE  */

#define PROF_HDDVDROM   0x0050  /* HD DVD-ROM Read-only  */
#define PROF_HDDVDR     0x0051  /* HD DVD-R   Write-once */
#define PROF_HDDVDRAM   0x0052  /* HD DVD-RAM Rewritable */

#define PROF_NONSTAN    0xFFFF  /* No Standard Profile */

#define    PROF_TEXT(X)     \
    (((X)==PROF_NONREMOV)   ? "Non-removable disc"  :\
     ((X)==PROF_REMOV)      ? "Removable disc"      :\
     ((X)==PROF_MOERAS)     ? "MO Erasable"         :\
     ((X)==PROF_OPTWO)      ? "Optical Write Once"  :\
     ((X)==PROF_ASMO)       ? "AS-MO"               :\
     ((X)==PROF_CDROM)      ? "CD-ROM"              :\
     ((X)==PROF_CDR)        ? "CD-R"                :\
     ((X)==PROF_CDRW)       ? "CD-RW"               :\
     ((X)==PROF_DVDROM)     ? "DVD-ROM"             :\
     ((X)==PROF_DVDmR)      ? "DVD-R"               :\
     ((X)==PROF_DVDmRdlsr)  ? "DVD-R DL SR"         :\
     ((X)==PROF_DVDmRdllj)  ? "DVD-R DL LJR"        :\
     ((X)==PROF_DVDRAM)     ? "DVD-RAM"             :\
     ((X)==PROF_DVDmRWro)   ? "DVD-RW RO"           :\
     ((X)==PROF_DVDmRWsr)   ? "DVD-RW SR"           :\
     ((X)==PROF_DVDpRW)     ? "DVD+RW"              :\
     ((X)==PROF_DVDpRWdl)   ? "DVD+RW DL"           :\
     ((X)==PROF_DVDpR)      ? "DVD+R"               :\
     ((X)==PROF_DVDpRdl)    ? "DVD+R DL"            :\
     ((X)==PROF_BDROM)      ? "BD-ROM"              :\
     ((X)==PROF_BDRsrm)     ? "BD-R SRM"            :\
     ((X)==PROF_BDRrrm)     ? "BD-R RRM"            :\
     ((X)==PROF_BDRE)       ? "BD-RE"               :\
     ((X)==PROF_HDDVDROM)   ? "HD DVD-ROM"          :\
     ((X)==PROF_HDDVDR)     ? "HD DVD-R"            :\
     ((X)==PROF_HDDVDRAM)   ? "HD DVD-RAM"          :\
     ((X)==PROF_NONSTAN)    ? "No Standard Profile" :\
                              "Reserved")

/* Book Type from READ DVD STRUCTURE,
 * MMC-4 6.27.2.1 Table 381
 * MMC-5 6.27.??, 4.3.1.4 Table 33  (mmc5r01e)
 * Mt. Fuji 6 1.0, Table 116        (Fuji6r100)
 * All undefined values are "Reserved"
 *
 * TODO: check separate book type for DVD-R/RW DL ??
 *       No, they use separate 'DL indicator' field.
 */
#define BT_DVDROM   0x0000  /* DVD-ROM  */
#define BT_DVDRAM   0x0001  /* DVD-RAM  */
#define BT_DVDmR    0x0002  /* DVD-R     */
#define BT_DVDmRW   0x0003  /* DVD-RW     */
#define BT_HDDVDROM 0x0004  /* HD DVD-ROM */
#define BT_HDDVDRAM 0x0005  /* HD DVD-RAM */
#define BT_HDDVDR   0x0006  /* HD DVD-R   */

#define BT_DVDpRW   0x0009  /* DVD+RW    */
#define BT_DVDpR    0x000A  /* DVD+R     */
#define BT_DVDpRWdl 0x000D  /* DVD+RW DL */
#define BT_DVDpRdl  0x000E  /* DVD+R DL  */

#define    BT_TEXT(X)   \
    (((X)==BT_DVDROM)   ? "DVD-ROM"     :\
     ((X)==BT_DVDRAM)   ? "DVD-RAM"     :\
     ((X)==BT_DVDmR)    ? "DVD-R"       :\
     ((X)==BT_DVDmRW)   ? "DVD-RW"      :\
     ((X)==BT_HDDVDROM) ? "HD DVD-ROM"  :\
     ((X)==BT_HDDVDRAM) ? "HD DVD-RAM"  :\
     ((X)==BT_HDDVDR)   ? "HD DVD-R"    :\
     ((X)==BT_DVDpRW)   ? "DVD+RW"      :\
     ((X)==BT_DVDpRWdl) ? "DVD+RW DL"   :\
     ((X)==BT_DVDpR)    ? "DVD+R"       :\
     ((X)==BT_DVDpRdl)  ? "DVD+R DL"    :\
                          "Reserved" )

#endif  /* __SCSIDEFS_H__ */

