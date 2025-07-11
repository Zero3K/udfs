/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : ucttimestamp.h
 *
 * Description : Timestamp support functions
 *
 * Author(s)   : Gerrit Scholl, Alex Sinitsyn
 *
 * History     : 20000922 GS, AS: creation
 */

#ifndef __UCT_UCTTIMESTAMP_H__
#define __UCT_UCTTIMESTAMP_H__

#include "general.h"
#include "mytypes.h"
#include "udfstruct.h"

/* isValidTimestamp():
 * check if valid timestamp, used by verifyTimestamp(), etc.
 * Mind that days per month checking is more strict than
 * according to ECMA 1/7.3
 */
extern bool isValidTimestamp(Timestamp *t);

/* print all Timestamp fields as continuation lines.
 * mark error fields.
 */
extern void printTimestampFull(Timestamp *t);

/* print short Timestamp, only recommended for
 * Timestamps without error fields.
 * format: yyyy-mm-dd hh:mm[:ss[.cchhmm]][ +/-<zone>][ Type: t]
 * precede by ERR in case of a Timestamp error.
 */
extern void printTimestampShort(Timestamp *t, bool printSecondAndLower,
                                char *endTxt);

/* printUTCtime
 * Print Timestamp in Coordinated Universal Time format
 *
 * Return value:
 *   FALSE if error occurred, TRUE if ok
 *
 * Note: printUTCtime does not perform verification of Timestamp fields.
 */
extern bool printUTCtime(Timestamp *t,
                         bool printSecondAndLower, char *endTxt);

/* printFormattedTimestamps():
 * Print max 2 formatted timestamps on separate lines
 * format: "[<txtPrefix>][<txt1>: <time t1>\n".
 * If 2 timestamps are printed, "<txt1>:" and "<txt2>:"
 * will be aligned vertically.
 * It is assumed that the printing character position
 * is zero at entry of this function.
 */
extern void printFormattedTimestamps(Timestamp *t1, char *txt1,
                                     Timestamp *t2, char *txt2,
                                     char *txtPrefix,
                                     Uint8 vLevel);

/* compareTimestamps():
 * return value:
 *  if one of the timestamps could not be converted to UTC time,
 *  then FALSE and resultValue undefined
 *  else TRUE  and resultValue:
 *       if      time 1 > time 2 then +1
 *       else if time 1 < time 2 then -1
 *                               else  0
 */
extern bool compareTimestamps(Timestamp *t1, Timestamp *t2,
                              int *resultValue);

/* reportUndefinedTimezoneCompare():
 * Print warning for final Timestamp summary
 * Call this function only once.
 */
extern void reportUndefinedTimezoneCompare();

/* subtractTimestamps():
 * Subtract time in t1 from time in t2.
 * Input times must be valid Timestamp values.
 * Normally used for determining the timezone by subtracting
 * UTC time from local time, see createLocaltimeTimestamp().
 * This means that the implementation may be inefficient
 * for bigger time differences.
 * The resulting difference is returned as *pMinutes minutes
 * and *pSeconds seconds, where *pSeconds is between +59 and
 * -59 and *pMinutes is between +2^31 and -2^31 minutes
 * (about 4082 years). Results beyond this limit will (and
 * results close to this limit may) cause an error return.
 * If both non-zero, *pMinutes and *pSeconds have
 * equal signs.
 * Lower-than-second fields of the Timestamps are ignored in
 * the subtraction.
 *
 * return value: FALSE if error, else TRUE;
 */
extern bool subtractTimestamps(Timestamp *t1, Timestamp *t2,
                               Int32 *pMinutes,
                               Int8  *pSeconds);

/* From here functions that use <time.h>
 * Only to create UDF Timestamps from
 * time_t and struct tm types.
 */
#include <time.h>

/* convertTime_tToTimestamp()
 * convert time_t value to UDF Timestamp with
 * correct TimeZone value. TimeZone is calculated
 * by subtracting UTC time from local time.
 * Result Timestamp in *tsLocal.
 */
extern bool convertTime_tToTimestamp(time_t t,
                                     Timestamp *tsLocal);

/* convertTmToTimestamp() using mktime().
 * convert struct tm time to UDF Timestamp with
 * correct TimeZone value. TimeZone is calculated
 * by subtracting UTC time from local time.
 * Result Timestamp in *tsLocal.
 */
extern bool convertTmToTimestamp(struct tm *tm1,
                                 Timestamp *tsLocal);

/* createCurrentTimeTimestamp()
 * Create Timestamp of current local time with
 * correct TimeZone value.
 * Result Timestamp in *tsLocal.
 */
extern bool createCurrentTimeTimestamp(Timestamp *tsLocal);

#endif  /* __UCT_UCTTIMESTAMP_H__ */


