/* Copyright (c) KONINKLIJKE PHILIPS ELECTRONICS N.V. 1999-2006
 *
 * All rights are reserved. Reproduction in whole or in part is
 * prohibited without the written consent of the copyright owner.
 *
 * Package     : uct_core
 *
 * File        : ucttimestamp.c
 *
 * Description : Timestamp support functions
 *
 * Author(s)   : Gerrit Scholl, Alex Sinitsyn
 *
 * History     : 20000922 GS, AS: creation
 */

#include <stdio.h>
#include <string.h>

#include "general.h"
#include "mytypes.h"
#include "udfstruct.h"
#include "uctgeneral.h"
#include "uctstatus.h"
#include "ucttimestamp.h"


/* return nmb of days in this month, check leap year.
 * month and year argument values according
 * to Timestamp fields, ECMA 1/7.3
 * Abnormal return values:
 *  if month [1..12] out of range then return 31
 *
 * For leap year calculation, no year 'out of range' is
 * tested. The calculation is valid for the whole Int16 year
 * range. Mind that year is an unsigned argument.
 *
 * note: leap year rules from
 *  http://www.faqs.org/faqs/astronomy/faq/part3/section-7.html
 */
static Uint8 monthDays(Uint8 month, Int16 year)
{
    if( month >= 1 && month <= 12 )
    {   switch( month )
        { case 4: case 6: case 9: case 11:  /* April, etc */
            return 30;
          case 2:               /* Feb, check leap year */
            return (Uint8)
                ((   (year % 4) == 0
                  && (   (year % 100) != 0
                      || (year % 400) == 0) )
                 ? 29 : 28);
        }
    }
    return (Uint8) 31;  /* default */
}

/* isValidTimestamp():
 * check if valid timestamp, used by verifyTimestamp(), etc.
 * Mind that days per month checking is more strict than
 * according to ECMA 1/7.3
 * return value:
 *       FALSE if erroneous  or blank (all #00) Timestamp
 *  else TRUE.
 */
extern bool isValidTimestamp(Timestamp *t)
{
  Uint8 type     = GET_TIMETYPE(t);
  Int16 timezone = GET_TIMEZONE(t);

  if( verifyZeros((Byte*)t, sizeof(Timestamp), NULL, NULL, NULL) )
  { return FALSE;       /* all zero bytes, undefined Timestamp */
  }

  if( type != 1 )
  { return FALSE;       /* Type error */
  }

  if(   timezone != TIMEZONE_UNDEFINED
     && abs(timezone) > 1440 )
  { return FALSE;       /* Timezone error */
  }

  /* check other fields bounadries
   * all Uint8, except Year is Int16 !!
   */
  if(   t->Year   < 1 || t->Year  > 9999    /* signed */
     || t->Month  < 1 || t->Month > 12
     || t->Day    < 1 || t->Day   > monthDays(t->Month,t->Year)
     || t->Hour   > 23
     || t->Minute > 59
     || t->Second > 59
     || t->Centiseconds > 99
     || t->HundredsofMicroseconds > 99
     || t->Microseconds > 99
    )
  { return FALSE;   /* field value out of range */
  }
  return TRUE;
}

/* print all Timestamp fields as continuation lines.
 * mark error fields. Mark Type not equal to 1 as an error,
 * but handle other fields in that case as Type 1.
 */
extern void printTimestampFull(Timestamp *t)
{
    char *ct99txt = "  -> will be changed to 99",
         *txt, ch, ch1, ch2;
    Uint8 type     = GET_TIMETYPE(t);
    Int16 timezone = GET_TIMEZONE(t);

    /* Print all fields and mark errors with * char.
     */
    if( type != 1 )
         ch1 = '*';
    else ch1 = ' ';
    if( timezone != TIMEZONE_UNDEFINED && abs(timezone) > 1440 )
         ch2 = '*';
    else ch2 = ' ';
    if( ch1 == '*' || ch2 == '*' )
         ch = '*';
    else ch = ' ';
    fprintf(uctout, "-\t%-22s : %c #%04X,"
                " %c Type: %u, %c Timezone: %d (%s)\n",
        "TypeAndTimezone", ch, t->TypeAndTimezone,
        ch1, type, ch2, timezone, TIMEZONE_TYPE1TXT(timezone));

    if( t->Year < 1 || t->Year > 9999 )
         ch = '*';
    else ch = ' ';
    fprintf(uctout, "-\t%-22s : %c %5d\n", "Year", ch, t->Year);

    if( t->Month < 1 || t->Month > 12 )
         ch = '*';
    else ch = ' ';
    fprintf(uctout, "-\t%-22s : %c %5u\n", "Month", ch, t->Month);

    txt = "";       /* for test more strict than ECMA 1/7.3.4 */
    ch = ' ';
    if( t->Day < 1 || t->Day > monthDays(t->Month,t->Year) )
    {   ch = '*';       /* date out of range */
        if( t->Day >= 1 && t->Day <= 31 )    /* ok with ECMA, but   */
        { txt = "  => date does not exists"; /* date does not exist */
        }
    }
    fprintf(uctout, "-\t%-22s : %c %5u%s\n", "Day", ch, t->Day, txt);

    if( t->Hour > 23 )
         ch = '*';
    else ch = ' ';
    fprintf(uctout, "-\t%-22s : %c %5u\n", "Hour", ch, t->Hour);

    if( t->Minute > 59 )
         ch = '*';
    else ch = ' ';
    fprintf(uctout, "-\t%-22s : %c %5u\n", "Minute", ch, t->Minute);

    if( t->Second > 59 )
         ch = '*';
    else ch = ' ';
    fprintf(uctout, "-\t%-22s : %c %5u\n", "Second", ch, t->Second);

    if( t->Centiseconds > 99 )
         ch = '*';
    else ch = ' ';
    fprintf(uctout, "-\t%-22s : %c %5u%s\n", "Centiseconds",
                                       ch, t->Centiseconds,
        (t->Centiseconds > 99) ? ct99txt : "");

    if( t->HundredsofMicroseconds > 99)
         ch = '*';
    else ch = ' ';
    fprintf(uctout, "-\t%-22s : %c %5u%s\n", "HundredsofMicroseconds",
                                       ch, t->HundredsofMicroseconds,
        (t->HundredsofMicroseconds > 99) ? ct99txt : "");

    if( t->Microseconds > 99 )
         ch = '*';
    else ch = ' ';
    fprintf(uctout, "-\t%-22s : %c %5u%s\n", "Microseconds",
                                       ch, t->Microseconds,
        (t->Microseconds > 99) ? ct99txt : "");

    /* end of all fields
     * print extra information
     */
    fprintf(uctout,  "-\t%22s : *\n", "==> errors marked with");

    /* extra: if any of last 3 fields is out of range,
     *        its value will be changed to 99 (see ct99txt)
     */
    if(    t->Centiseconds > 99
        || t->HundredsofMicroseconds > 99
        || t->Microseconds > 99 )
    { fprintf(uctout,
        "-\tnote: If any of last 3 fields is out of range,\n"
        "-\t      its value will be changed to 99.\n");

      if( t->Centiseconds > 99 )          t->Centiseconds = 99;
      if( t->HundredsofMicroseconds > 99) t->HundredsofMicroseconds = 99;
      if( t->Microseconds > 99 )          t->Microseconds = 99;
    }
}

/* print short Timestamp.
 * format: [*]yyyy-mm-dd hh:mm[:ss[.cchhmm]][ +/-<zone>][ Type: t]
 * Invalid timestamps marked with '*'. Invalid timestamps
 * may show strange results.
 */
extern void printTimestampShort(Timestamp *t, bool printSecondAndLower,
                                char *endTxt)
{
    Int16 timezone = GET_TIMEZONE(t);

    if( !isValidTimestamp(t) )
    { fprintf(uctout, "*");         /* mark timestamp error */
    }

    /* print compact form of Timestamp
     */
    fprintf(uctout, "%04d-%02u-%02u %02u:%02u",
                        t->Year, t->Month, t->Day,
                        t->Hour, t->Minute);

    if( printSecondAndLower )
    { fprintf(uctout, ":%02u", t->Second);
      if(   t->Centiseconds != 0
         || t->HundredsofMicroseconds != 0
         || t->Microseconds != 0 )
      { fprintf(uctout, ".%02u%02u%02u",
          t->Centiseconds, t->HundredsofMicroseconds, t->Microseconds);
      }
    }

    if( timezone == TIMEZONE_UNDEFINED )
    { fprintf(uctout, " noZone");
    }
    else if( abs(timezone) > 1440 )     /* error */
    { fprintf(uctout, " errZone");
    }
    else if( timezone != 0 )
    { fprintf(uctout, " %c%02u:%02u", (timezone >= 0) ? '+' : '-',
                abs(timezone)/60, abs(timezone) % 60);
    }

    if( endTxt != NULL )
    { fprintf(uctout, "%s", endTxt);
    }
}

/* addMinutesToTimestamp():
 * Normally used for subtracting a timezone from a timestamp
 * [see convertToUTC()], or determining the timezone by
 * subtracting UTC time from local time
 * [see createLocaltimeTimestamp()].
 * This means that the implementation may be inefficient
 * for bigger time differences.
 * Number of minutes is a signed value (Int16), which can
 * therefore hold at most 22.75 days in minutes !!
 * notes:
 *  - Input Timestamp will be verified on range of Year-to-Minute
 *    fields only, so no check of TypeAndTimezone and
 *    Second-and-lower fields.
 *  - Output Timestamp can have a '1 year out of range' Year value,
 *    which is represented correctly because Year is signed (Int16).
 */
static bool addMinutesToTimestamp(Timestamp *t, Int16 minutes)
{
    Int32 new_value, toBeAdded = (Int32) minutes;

    if(   t->Year   < 1 || t->Year  > 9999
       || t->Month  < 1 || t->Month > 12
       || t->Day    < 1 || t->Day   > monthDays(t->Month,t->Year)
       || t->Hour   > 23
       || t->Minute > 59 )
    {   return FALSE;
    }

#ifdef UCT_TESTING
    /**testing**/toBeAdded += (60 * 24 * 365) + (23 * 60) + 59;
#endif

    if( toBeAdded == 0 ) return TRUE;

    /* define local macro for Minute, Hour field addition
     */
#define ADD_FIELD(FIELD,MOD)    \
    new_value = (((Int32)(FIELD)) + toBeAdded) % (MOD); \
    if( new_value < 0 ) new_value += (MOD);     \
    toBeAdded -= (new_value - (Int32)(FIELD));  \
    (FIELD) = (Uint8) new_value;                \
    UCTASSERT( (toBeAdded % (MOD)) == 0 );      \
    toBeAdded /= (MOD);
    /* end of macro
     */

    ADD_FIELD(t->Minute, 60);   /* adapt Minute [0..60] */
    ADD_FIELD(t->Hour, 24);     /* adapt Hour, [0..23] */

#undef ADD_FIELD

    /* toBeAdded is in day units now
     * adapt Day, Month and Year fields
     * Day,   [1..<daysInMonth>]
     * Month, [1..12]
     * Year,  [1..9999] (unsigned)
     * extend Year range to hold overflow.
     */
    while( toBeAdded != 0 )     /* days */
    {   Uint8 daysInMonth = monthDays(t->Month, t->Year);
        new_value = t->Day + toBeAdded;
        if( new_value > daysInMonth )   /* proceed to next month */
        {   new_value = MIN(new_value, daysInMonth + 28);
            toBeAdded -= (new_value - (Int32)t->Day);
            t->Day = (Uint8) (new_value - daysInMonth);
            if( ++(t->Month) > 12 )
            {   t->Month = 1; t->Year++; /* maybe year out of range */
            }
        }
        else if( new_value < 1 )        /* back to previous month */
        {   if( --(t->Month) < 1 )
            {   t->Month = 12; t->Year--; /* maybe year out of range */
            }
            daysInMonth = monthDays(t->Month, t->Year);
            new_value = MAX(new_value, 1 - daysInMonth);
            toBeAdded -= (new_value - (Int32)t->Day);
            t->Day = (Uint8) (new_value + daysInMonth);
        }
        else    /* 1 <= new_value <=  daysInMonth, adapt Day only */
        {   toBeAdded -= (new_value - (Int32)t->Day);
            t->Day = (Uint8) new_value;             \
        }
    }
    return TRUE;
}

/* Convert Timestamp in t to UTC time
 * return result:
 * if conversion ok,
 * then TRUE,  converted Timestamp has Type 1 and Timezone 0.
 * else FALSE, and Timestamp undefined.
 *
 * Note: result t->Year may be 1 year out of range.
 */
static bool convertToUTC(Timestamp *t)
{
    Int16 Timezone = GET_TIMEZONE(t);

    /* verify Timestamp
     */
    if(  !isValidTimestamp(t)               /* error */
       || Timezone == TIMEZONE_UNDEFINED )  /* undefined Timezone */
    {   return FALSE;                       /* cannot convert */
    }

    /* subtract Timezone minutes for conversion
     * only check for minutes-and-higher fields.
     */
    if( addMinutesToTimestamp(t, (Int16) (-Timezone)) ) /* ok */
    {   t->TypeAndTimezone = 0x1000; /* to UTC, Type 1, Timezone 0 */
        return TRUE;    /* ok, Year may be out of range */
    }
    return FALSE;       /* addMinutesToTimestamp could not convert */
}

/* printUTCtime():
 * if time can be converted to UTC
 * then print  UTC  time and return TRUE;
 * else print local time and return FALSE
 */
extern bool printUTCtime(Timestamp *t,
                         bool printSecondAndLower, char *endTxt)
{
    Timestamp tUTC = *(t);  /* copy */

    /* convert Timestamp t to UTC time in tUTC
     */
    tUTC = *(t);        /* copy */
    if( !printSecondAndLower )
    {   /* avoid failure if any of SecondAndLower fields out of range
         */
        tUTC.Second = tUTC.Centiseconds =
        tUTC.HundredsofMicroseconds = tUTC.Microseconds = 0;
    }
    if( convertToUTC( &tUTC ) )
    {   printTimestampShort(&tUTC, printSecondAndLower, endTxt);
        return TRUE;
    }
    /* could not convert, print unconvert Timestamp
     */
    printTimestampShort(t, printSecondAndLower, endTxt);
    return FALSE;
}


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
                                     Uint8 vLevel)
{
    ifVERBOSE( vLevel )
    {   char       format[100], /* max 100 chars should do */
                  *txt;
        Timestamp *t;
        Uint32     n;
        size_t     len = 0;

        /* generate format for proper allignment
         * of txt1 and txt2.
         */
        if( txt1 != NULL ) len = strlen(txt1);
        if( txt2 != NULL ) len = MAX(len, strlen(txt2));
        if( len != 0 )
             sprintf(format, "%%%us: ", len);
        else sprintf(format, "%%s: ");

        for( n = 0, t = t1, txt = txt1;
             n < 2;
             n++,   t = t2, txt = txt2 )
        { if( t != NULL || txt != NULL )
          { if( txtPrefix != NULL )
            {   fprintf(uctout, "%s", txtPrefix);
            }
            fprintf(uctout, format, (txt != NULL) ? txt : "");
            if( t != NULL )
            {   printTimestampShort(t, TRUE, "");
            }
            fprintf(uctout, "\n");
          }
        }
    }
    ENDif;
}

/* compareTimeInZone():
 * return value:
 *  if      time 1 > time 2 then +1
 *  else if time 1 < time 2 then -1
 *                          else  0
 * Implementation notes:
 *  For comparison, compareTimeInZone() ignores Timezone and TimeType.
 *  If the Timezone is important, then convert to same TimeZone
 *  [convertToUTC()] before calling this function.
 *  It is assumed that t1 and t2 are valid 'normalized' Timestamps,
 *  with the sole exception that a '1 year out of range' that may
 *  be caused by addMinutesToTimestamp() [convertToUTC()] will be
 *  handled correctly in the comparison.
 */
static int compareTimeInZone(Timestamp *t1, Timestamp *t2)
{   Int32 diff;     /* signed */

    /* timestamp fields are of type Uint8 or Int16,
     * so a type Int32 for diff will do.
     */
    if(   (diff = t1->Year   - t2->Year) != 0
       || (diff = t1->Month  - t2->Month) != 0
       || (diff = t1->Day    - t2->Day )   != 0
       || (diff = t1->Hour   - t2->Hour )  != 0
       || (diff = t1->Minute - t2->Minute) != 0
       || (diff = t1->Second - t2->Second) != 0
       || (diff = t1->Centiseconds -
                  t2->Centiseconds)        != 0
       || (diff = t1->HundredsofMicroseconds -
                  t2->HundredsofMicroseconds) != 0
       || (diff = t1->Microseconds -
                  t2->Microseconds)        != 0 )
    {   return (diff > 0) ? +1 : -1;
    }
    return 0;       /* all fields equal */
}

/* compareTimestamps():
 * return value:
 *  if one of the timestamps could not be converted to UTC time,
 *  then FALSE and resultValue undefined
 *  else TRUE  and resultValue:
 *       if      time 1 > time 2 then +1
 *       else if time 1 < time 2 then -1
 *                               else  0
 */
static bool encounteredUndefinedTimezoneCompare = FALSE;

extern bool compareTimestamps(Timestamp *t1, Timestamp *t2,
                              int *resultValue)
{   Timestamp t1utc, t2utc;

    /* test if undefined timezone involved
     * (Type 1, TIMEZONE_UNDEFINED).
     */
    if(   (   GET_TIMETYPE(t1) == 1
           && GET_TIMEZONE(t1) == TIMEZONE_UNDEFINED)
       || (   GET_TIMETYPE(t2) == 1
           && GET_TIMEZONE(t2) == TIMEZONE_UNDEFINED) )
    {   /* At least one TIMEZONE_UNDEFINED timestamp involved
         * Flag, but ignore Timezone for comparison.
         */
        encounteredUndefinedTimezoneCompare = TRUE;
    }
    else    /* try to convert both Timestamps to UTC */
    {   t1utc = (*t1);          /* copy t1 */
        t2utc = (*t2);          /* copy t2 */
        if(   !convertToUTC( &t1utc )
           || !convertToUTC( &t2utc ) )
        {   MLIMITbegin(WARN01level, MLIMITdefault01);
              fprintf(uctout,
                   "\tWarning: Unable to compare undefined"
                                " or erroneous Timestamps\n");
              printFormattedTimestamps(t1, "Timestamp 1",
                                       t2, "Timestamp 2",
                                       "-\t\t ", MLIMIT.vl);
            MLIMITend;
            return FALSE;
        }
        t1 = &t1utc;
        t2 = &t2utc;
    }
    /* t1 and t2 may now point to (local)
     * UTC converted Timestamps
     */
    (*resultValue) = compareTimeInZone(t1, t2);
    return TRUE;
}

/* reportUndefinedTimezoneCompare():
 * Print Note for final Timestamp summary
 * Call this function only once.
 */
extern void reportUndefinedTimezoneCompare()
{
    if( encounteredUndefinedTimezoneCompare )
    {   /* at least one TIMEZONE_UNDEFINED Timestamp
         * involved in compareTimestamps() comparison.
         * Implementation note:
         *  Do not use MLIMIT* macros in order to avoid
         *  suppression by -verbose level or print of
         *  "Message printed once" line for -mlimit 1.
         */
      fprintf(uctout,
        "  Note: At least one Timestamp with an undefined Timezone\n"
             "-\twas involved in a Timestamp time comparison.\n"
        "-   For a Timestamp comparison where at least one undefined\n"
        "-   Timezone is involved, the Timezone values are ignored.\n\n");
    }
}


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
                               Int8  *pSeconds)
{   Timestamp tmp2;
    Int32     dlt;

    if( !isValidTimestamp(t1) || !isValidTimestamp(t2) )
    { return FALSE;             /* error in t1 or t2 */
    }
    tmp2 = *t2;     /* temp Timestamp copy */

    /* init on timezone difference,
     * Mind tmp2 - t1 here, further always t1 - tmp2 !!
     */
    *pMinutes = GET_TIMEZONE(&tmp2) - GET_TIMEZONE(t1);

    /* addMinutes to tmp2 untill equal to t1.
     * Max MAX_INT16 minutes (about 22.75 days) per step,
     * because Int16 miniutes for addMinutesToTimestamp().
     * Note that possible timezone difference
     * neglected, because already in *pMinutes.
     */
    while(   t1->Year   != tmp2.Year
          || t1->Month  != tmp2.Month
          || t1->Day    != tmp2.Day
          || t1->Hour   != tmp2.Hour
          || t1->Minute != tmp2.Minute )
    {
        dlt = t1->Year - tmp2.Year;
        if( abs(dlt) > 4080 )   /* 4480 years, */
        {   return FALSE;   /* avoid *pMinutes overflow */
        }
        /* To month, then to days.
         * Because one step is limited to about 22 days,
         * the Day field will only be examined if the Year
         * and Month fields of t1 and tmp2 are both equal.
         */
        dlt = 12 * dlt + (t1->Month - tmp2.Month);
        if( dlt > 0 )
        {   dlt =  28;      /* minimum month in days */
        }
        else if( dlt < 0 )
        {   dlt = -28;      /* minimum month in days */
        }
        else    /* dlt == 0, year and month equal */
        {   dlt = t1->Day - tmp2.Day;   /* days */
        }
        dlt = dlt * 24 * 60;    /* days -> minutes */
        dlt += 60 * (t1->Hour   - tmp2.Hour)
                  + (t1->Minute - tmp2.Minute);

        if(       dlt >  MAX_INT16 )    /* minutes */
        {   dlt = MAX_INT16;
        }
        else if(  dlt < -MAX_INT16 )
        {   dlt = -MAX_INT16;
        }
        *pMinutes += dlt;
        if( !addMinutesToTimestamp(&tmp2, (Int16) dlt) )
        { return FALSE;
        }
    }

    /* calculate seconds separately, ignore sub-seconds part.
     * ensure equal signs for *pSeconds and *pMinutes.
     */
    dlt = t1->Second - tmp2.Second;
    if(      dlt < 0 && *pMinutes > 0 )
    {   dlt += 60;
        (*pMinutes)--;
    }
    else if( dlt > 0 && *pMinutes < 0 )
    {   dlt -= 60;
        (*pMinutes)++;
    }
    *pSeconds = (Int8) dlt;

    return TRUE;
}


/* From here functions that use <time.h>
 * Only to create UDF Timestamps from
 * time_t and struct tm types.
 */
#include <time.h>

/* fillTimestampFromTm()
 * used by createLocaltimeTimestamp()
 * No test on any field.
 */
static void fillTimestampFromTm(struct tm *tm1,
                                Int16 timezone,
                                Timestamp *ts)
{
    /* Type 1 and Timezone
     */
    ts->TypeAndTimezone = (Uint16) (0x1000 | (timezone & 0xFFF));

    ts->Year    = (Int16) (tm1->tm_year + 1900); /* signed */
    ts->Month   = (Uint8) (tm1->tm_mon + 1);    /* tm from 0 -> 11 */
    ts->Day     = (Uint8) tm1->tm_mday;
    ts->Hour    = (Uint8) tm1->tm_hour;
    ts->Minute  = (Uint8) tm1->tm_min;
    ts->Second  = (Uint8) tm1->tm_sec;

    ts->Centiseconds = 0;
    ts->HundredsofMicroseconds = 0;
    ts->Microseconds = 0;
}

/* convertTime_tToTimestamp()
 * convert time_t value to UDF Timestamp with
 * correct TimeZone value. TimeZone is calculated
 * by subtracting UTC time from local time.
 * Result Timestamp in *tsLocal.
 */
extern bool convertTime_tToTimestamp(time_t t,
                                     Timestamp *tsLocal)
{
    struct tm tm1;
    Timestamp tmpUTC;
    Int32     timezoneMinutes;
    Int8      timezoneSeconds;
    bool      result;

    /* mind that tm1->tm_isdst value does not matter
     * DST will be included in timezoneMinutes
     * using subtractTimestamps().
     */
#ifdef UCT_TESTING
/**testing**/{static cnt = 0;
///**testing**/t = 972781199 + cnt++; /* October 2000 DST test */
///**testing**/t = 985481999 + cnt++; /* March   2001 DST test */
/**testing**/}
#endif
    tm1 = *(localtime(&t));                 /* local time */
    fillTimestampFromTm(&tm1, 0, tsLocal);  /* Timezone 0 */

    tm1 = *(gmtime(&t));                    /*   UTC time */
    fillTimestampFromTm(&tm1, 0, &tmpUTC);  /* Timezone 0 */

    result = subtractTimestamps( tsLocal, &tmpUTC,
                                &timezoneMinutes,
                                &timezoneSeconds);
    if( !result )
    {           /* subtractTimestamps() error */
    }
    else if( timezoneSeconds >=  30 )   /* round */
    {   timezoneMinutes++;
    }
    else if( timezoneSeconds <= -30 )   /* round */
    {   timezoneMinutes--;
    }
    if( (!result) || abs(timezoneMinutes) > 1440 )
    { MLIMITbegin(ERROR00level, uctMessageLimit);
        fprintf(uctout,
             "\tError: %s, please report.\n"
            "-\tLocal Time: ",
            (result)
            ? "Current time Timezone overflow"
            : "Unable to create current time Timestamp");
        printTimestampShort(tsLocal, TRUE,
          "\n-\t  UTC Time: ");
        printTimestampShort(&tmpUTC, TRUE, "\n");
        if( result )
        { fprintf(uctout,
            "-\tTimezone: %ld, expected for"
            " abs(Timezone): at most 1440\n",
                    timezoneMinutes);
        }
      MLIMITend;
      return FALSE;
    }
    /* Timezone ok, set 4-bits Type 1 and 12-bits Timezone
     */
    tsLocal->TypeAndTimezone =
            (Uint16) (0x1000 | (timezoneMinutes & 0xFFF));

    return TRUE;
}

/* convertTmToTimestamp() using mktime().
 * convert struct tm time to UDF Timestamp with
 * correct TimeZone value. TimeZone is calculated
 * by subtracting UTC time from local time.
 * Result Timestamp in *tsLocal.
 */
extern bool convertTmToTimestamp(struct tm *tm1,
                                 Timestamp *tsLocal)
{
    time_t t = mktime(tm1);     /* get time_t */
    return convertTime_tToTimestamp(t, tsLocal);
}

/* createCurrentTimeTimestamp()
 * Create Timestamp of current local time with
 * correct TimeZone value.
 * Result Timestamp in *tsLocal.
 */
extern bool createCurrentTimeTimestamp(Timestamp *tsLocal)
{
    time_t t = time(NULL);      /* current time */
    return convertTime_tToTimestamp(t, tsLocal);
}

