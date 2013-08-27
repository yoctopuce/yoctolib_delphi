{*********************************************************************
 *
 * $Id: yocto_wakeupschedule.pas 12469 2013-08-22 10:11:58Z seb $
 *
 * Implements yFindWakeUpSchedule(), the high-level API for WakeUpSchedule functions
 *
 * - - - - - - - - - License information: - - - - - - - - - 
 *
 *  Copyright (C) 2011 and beyond by Yoctopuce Sarl, Switzerland.
 *
 *  Yoctopuce Sarl (hereafter Licensor) grants to you a perpetual
 *  non-exclusive license to use, modify, copy and integrate this
 *  file into your software for the sole purpose of interfacing 
 *  with Yoctopuce products. 
 *
 *  You may reproduce and distribute copies of this file in 
 *  source or object form, as long as the sole purpose of this
 *  code is to interface with Yoctopuce products. You must retain 
 *  this notice in the distributed source file.
 *
 *  You should refer to Yoctopuce General Terms and Conditions
 *  for additional information regarding your rights and 
 *  obligations.
 *
 *  THE SOFTWARE AND DOCUMENTATION ARE PROVIDED 'AS IS' WITHOUT
 *  WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING 
 *  WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS 
 *  FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO
 *  EVENT SHALL LICENSOR BE LIABLE FOR ANY INCIDENTAL, SPECIAL,
 *  INDIRECT OR CONSEQUENTIAL DAMAGES, LOST PROFITS OR LOST DATA, 
 *  COST OF PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR 
 *  SERVICES, ANY CLAIMS BY THIRD PARTIES (INCLUDING BUT NOT 
 *  LIMITED TO ANY DEFENSE THEREOF), ANY CLAIMS FOR INDEMNITY OR
 *  CONTRIBUTION, OR OTHER SIMILAR COSTS, WHETHER ASSERTED ON THE
 *  BASIS OF CONTRACT, TORT (INCLUDING NEGLIGENCE), BREACH OF
 *  WARRANTY, OR OTHERWISE.
 *
 *********************************************************************}


unit yocto_wakeupschedule;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YWakeUpSchedule definitions)

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_MINUTESA_INVALID              = YAPI_INVALID_LONGWORD;
   Y_MINUTESB_INVALID              = YAPI_INVALID_LONGWORD;
   Y_HOURS_INVALID                 = -1;
   Y_WEEKDAYS_INVALID              = -1;
   Y_MONTHDAYS_INVALID             = YAPI_INVALID_LONGWORD;
   Y_MONTHS_INVALID                = -1;
   Y_NEXTOCCURENCE_INVALID         = YAPI_INVALID_LONGWORD;


//--- (end of YWakeUpSchedule definitions)

type
//--- (YWakeUpSchedule declaration)
 TYWakeUpSchedule = class;
 TUpdateCallback  = procedure(func: TYWakeUpSchedule; value:string);
////
/// <summary>
///   TYWakeUpSchedule Class: WakeUpSchedule function interface
/// <para>
///   The WakeUpSchedule function implements a wake-up condition. The wake-up time is
///   specified as a set of months and/or days and/or hours and/or minutes where the
///   wake-up should happen.
/// </para>
/// </summary>
///-
TYWakeUpSchedule=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _minutesA                 : LongWord;
   _minutesB                 : LongWord;
   _hours                    : LongInt;
   _weekDays                 : LongInt;
   _monthDays                : LongWord;
   _months                   : LongInt;
   _nextOccurence            : LongWord;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YWakeUpSchedule declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of wake-up schedules started using <c>yFirstWakeUpSchedule()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YWakeUpSchedule</c> object, corresponding to
   ///   a wake-up schedule currently online, or a <c>null</c> pointer
   ///   if there are no more wake-up schedules to enumerate.
   /// </returns>
   ///-
   function nextWakeUpSchedule():TYWakeUpSchedule;

   //--- (YWakeUpSchedule accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the wake-up schedule.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the wake-up schedule
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the wake-up schedule.
   /// <para>
   ///   You can use <c>yCheckLogicalName()</c>
   ///   prior to this call to make sure that your parameter is valid.
   ///   Remember to call the <c>saveToFlash()</c> method of the module if the
   ///   modification must be kept.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   a string corresponding to the logical name of the wake-up schedule
   /// </param>
   /// <para>
   /// </para>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function set_logicalName(newval:string):integer;

   ////
   /// <summary>
   ///   Returns the current value of the wake-up schedule (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the wake-up schedule (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the minutes 00-29 of each hour scheduled for wake-up.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the minutes 00-29 of each hour scheduled for wake-up
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_MINUTESA_INVALID</c>.
   /// </para>
   ///-
   function get_minutesA():LongWord;

   ////
   /// <summary>
   ///   Changes the minutes 00-29 where a wake up must take place.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the minutes 00-29 where a wake up must take place
   /// </param>
   /// <para>
   /// </para>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function set_minutesA(newval:LongWord):integer;

   ////
   /// <summary>
   ///   Returns the minutes 30-59 of each hour scheduled for wake-up.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the minutes 30-59 of each hour scheduled for wake-up
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_MINUTESB_INVALID</c>.
   /// </para>
   ///-
   function get_minutesB():LongWord;

   ////
   /// <summary>
   ///   Changes the minutes 30-59 where a wake up must take place.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the minutes 30-59 where a wake up must take place
   /// </param>
   /// <para>
   /// </para>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function set_minutesB(newval:LongWord):integer;

   ////
   /// <summary>
   ///   Returns the hours  scheduled for wake-up.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the hours  scheduled for wake-up
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_HOURS_INVALID</c>.
   /// </para>
   ///-
   function get_hours():LongInt;

   ////
   /// <summary>
   ///   Changes the hours where a wake up must take place.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the hours where a wake up must take place
   /// </param>
   /// <para>
   /// </para>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function set_hours(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns the days of week scheduled for wake-up.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the days of week scheduled for wake-up
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_WEEKDAYS_INVALID</c>.
   /// </para>
   ///-
   function get_weekDays():LongInt;

   ////
   /// <summary>
   ///   Changes the days of the week where a wake up must take place.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the days of the week where a wake up must take place
   /// </param>
   /// <para>
   /// </para>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function set_weekDays(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns the days of week scheduled for wake-up.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the days of week scheduled for wake-up
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_MONTHDAYS_INVALID</c>.
   /// </para>
   ///-
   function get_monthDays():LongWord;

   ////
   /// <summary>
   ///   Changes the days of the week where a wake up must take place.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the days of the week where a wake up must take place
   /// </param>
   /// <para>
   /// </para>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function set_monthDays(newval:LongWord):integer;

   ////
   /// <summary>
   ///   Returns the days of week scheduled for wake-up.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the days of week scheduled for wake-up
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_MONTHS_INVALID</c>.
   /// </para>
   ///-
   function get_months():LongInt;

   ////
   /// <summary>
   ///   Changes the days of the week where a wake up must take place.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the days of the week where a wake up must take place
   /// </param>
   /// <para>
   /// </para>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function set_months(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns the  nextwake up date/time (seconds) wake up occurence
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the  nextwake up date/time (seconds) wake up occurence
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_NEXTOCCURENCE_INVALID</c>.
   /// </para>
   ///-
   function get_nextOccurence():LongWord;

   ////
   /// <summary>
   ///   Returns every the minutes of each hour scheduled for wake-up.
   /// <para>
   /// </para>
   /// </summary>
   ///-
   function get_minutes():long;

   ////
   /// <summary>
   ///   Changes all the minutes where a wake up must take place.
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="bitmap">
   ///   Minutes 00-59 of each hour scheduled for wake-up.,
   /// </param>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function set_minutes(bitmap:long):integer;

   //--- (end of YWakeUpSchedule accessors declaration)
end;

//--- (WakeUpSchedule functions declaration)

////
/// <summary>
///   Retrieves a wake-up schedule for a given identifier.
/// <para>
///   The identifier can be specified using several formats:
/// </para>
/// <para>
/// </para>
/// <para>
///   - FunctionLogicalName
/// </para>
/// <para>
///   - ModuleSerialNumber.FunctionIdentifier
/// </para>
/// <para>
///   - ModuleSerialNumber.FunctionLogicalName
/// </para>
/// <para>
///   - ModuleLogicalName.FunctionIdentifier
/// </para>
/// <para>
///   - ModuleLogicalName.FunctionLogicalName
/// </para>
/// <para>
/// </para>
/// <para>
///   This function does not require that the wake-up schedule is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YWakeUpSchedule.isOnline()</c> to test if the wake-up schedule is
///   indeed online at a given time. In case of ambiguity when looking for
///   a wake-up schedule by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the wake-up schedule
/// </param>
/// <returns>
///   a <c>YWakeUpSchedule</c> object allowing you to drive the wake-up schedule.
/// </returns>
///-
function yFindWakeUpSchedule(func:string):TYWakeUpSchedule;
////
/// <summary>
///   Starts the enumeration of wake-up schedules currently accessible.
/// <para>
///   Use the method <c>YWakeUpSchedule.nextWakeUpSchedule()</c> to iterate on
///   next wake-up schedules.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YWakeUpSchedule</c> object, corresponding to
///   the first wake-up schedule currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstWakeUpSchedule():TYWakeUpSchedule;

//--- (end of WakeUpSchedule functions declaration)

implementation

//--- (YWakeUpSchedule implementation)

var
   _WakeUpScheduleCache : TStringList;

constructor TYWakeUpSchedule.Create(func:string);
 begin
   inherited Create('WakeUpSchedule', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _minutesA := Y_MINUTESA_INVALID;
   _minutesB := Y_MINUTESB_INVALID;
   _hours := Y_HOURS_INVALID;
   _weekDays := Y_WEEKDAYS_INVALID;
   _monthDays := Y_MONTHDAYS_INVALID;
   _months := Y_MONTHS_INVALID;
   _nextOccurence := Y_NEXTOCCURENCE_INVALID;
 end;

{$HINTS OFF}
function TYWakeUpSchedule._parse(j:PJSONRECORD):integer;
 var
   member,sub : PJSONRECORD;
   i,l        : integer;
 begin
   if (j^.recordtype <> JSON_STRUCT) then begin _parse:= -1; exit; end;
   for i:=0 to j^.membercount-1 do
    begin
      member := j^.members[i];
      if (member^.name = 'logicalName') then
       begin
         _logicalName := string(member^.svalue);
       end else
      if (member^.name = 'advertisedValue') then
       begin
         _advertisedValue := string(member^.svalue);
       end else
      if (member^.name = 'minutesA') then
       begin
         _minutesA := member^.ivalue;
       end else
      if (member^.name = 'minutesB') then
       begin
         _minutesB := member^.ivalue;
       end else
      if (member^.name = 'hours') then
       begin
         _hours := member^.ivalue;
       end else
      if (member^.name = 'weekDays') then
       begin
         _weekDays := member^.ivalue;
       end else
      if (member^.name = 'monthDays') then
       begin
         _monthDays := member^.ivalue;
       end else
      if (member^.name = 'months') then
       begin
         _months := member^.ivalue;
       end else
      if (member^.name = 'nextOccurence') then
       begin
         _nextOccurence := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the wake-up schedule.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the wake-up schedule
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYWakeUpSchedule.get_logicalName():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LOGICALNAME_INVALID;
         exit;
       end;
   result := _logicalName;
 end;

////
/// <summary>
///   Changes the logical name of the wake-up schedule.
/// <para>
///   You can use yCheckLogicalName()
///   prior to this call to make sure that your parameter is valid.
///   Remember to call the saveToFlash() method of the module if the
///   modification must be kept.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   a string corresponding to the logical name of the wake-up schedule
/// </param>
/// <para>
/// </para>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWakeUpSchedule.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the wake-up schedule (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the wake-up schedule (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYWakeUpSchedule.get_advertisedValue():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_ADVERTISEDVALUE_INVALID;
         exit;
       end;
   result := _advertisedValue;
 end;

////
/// <summary>
///   Returns the minutes 00-29 of each hour scheduled for wake-up.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the minutes 00-29 of each hour scheduled for wake-up
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_MINUTESA_INVALID.
/// </para>
///-
function TYWakeUpSchedule.get_minutesA():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_MINUTESA_INVALID;
         exit;
       end;
   result := _minutesA;
 end;

////
/// <summary>
///   Changes the minutes 00-29 where a wake up must take place.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the minutes 00-29 where a wake up must take place
/// </param>
/// <para>
/// </para>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWakeUpSchedule.set_minutesA(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('minutesA',rest_val);
 end;

////
/// <summary>
///   Returns the minutes 30-59 of each hour scheduled for wake-up.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the minutes 30-59 of each hour scheduled for wake-up
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_MINUTESB_INVALID.
/// </para>
///-
function TYWakeUpSchedule.get_minutesB():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_MINUTESB_INVALID;
         exit;
       end;
   result := _minutesB;
 end;

////
/// <summary>
///   Changes the minutes 30-59 where a wake up must take place.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the minutes 30-59 where a wake up must take place
/// </param>
/// <para>
/// </para>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWakeUpSchedule.set_minutesB(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('minutesB',rest_val);
 end;

////
/// <summary>
///   Returns the hours  scheduled for wake-up.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the hours  scheduled for wake-up
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_HOURS_INVALID.
/// </para>
///-
function TYWakeUpSchedule.get_hours():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_HOURS_INVALID;
         exit;
       end;
   result := _hours;
 end;

////
/// <summary>
///   Changes the hours where a wake up must take place.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the hours where a wake up must take place
/// </param>
/// <para>
/// </para>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWakeUpSchedule.set_hours(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('hours',rest_val);
 end;

////
/// <summary>
///   Returns the days of week scheduled for wake-up.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the days of week scheduled for wake-up
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_WEEKDAYS_INVALID.
/// </para>
///-
function TYWakeUpSchedule.get_weekDays():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_WEEKDAYS_INVALID;
         exit;
       end;
   result := _weekDays;
 end;

////
/// <summary>
///   Changes the days of the week where a wake up must take place.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the days of the week where a wake up must take place
/// </param>
/// <para>
/// </para>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWakeUpSchedule.set_weekDays(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('weekDays',rest_val);
 end;

////
/// <summary>
///   Returns the days of week scheduled for wake-up.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the days of week scheduled for wake-up
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_MONTHDAYS_INVALID.
/// </para>
///-
function TYWakeUpSchedule.get_monthDays():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_MONTHDAYS_INVALID;
         exit;
       end;
   result := _monthDays;
 end;

////
/// <summary>
///   Changes the days of the week where a wake up must take place.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the days of the week where a wake up must take place
/// </param>
/// <para>
/// </para>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWakeUpSchedule.set_monthDays(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('monthDays',rest_val);
 end;

////
/// <summary>
///   Returns the days of week scheduled for wake-up.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the days of week scheduled for wake-up
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_MONTHS_INVALID.
/// </para>
///-
function TYWakeUpSchedule.get_months():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_MONTHS_INVALID;
         exit;
       end;
   result := _months;
 end;

////
/// <summary>
///   Changes the days of the week where a wake up must take place.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the days of the week where a wake up must take place
/// </param>
/// <para>
/// </para>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWakeUpSchedule.set_months(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('months',rest_val);
 end;

////
/// <summary>
///   Returns the  nextwake up date/time (seconds) wake up occurence
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the  nextwake up date/time (seconds) wake up occurence
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_NEXTOCCURENCE_INVALID.
/// </para>
///-
function TYWakeUpSchedule.get_nextOccurence():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_NEXTOCCURENCE_INVALID;
         exit;
       end;
   result := _nextOccurence;
 end;

////
/// <summary>
///   Returns every the minutes of each hour scheduled for wake-up.
/// <para>
/// </para>
/// </summary>
///-
function TYWakeUpSchedule.get_minutes():long;
     var
        res : long;
     begin
        res = self.get_minutesB();
        res = res << 30;
        res = res + self.get_minutesA();
        result:= res;
            
     end;


////
/// <summary>
///   Changes all the minutes where a wake up must take place.
/// <para>
/// </para>
/// </summary>
/// <param name="bitmap">
///   Minutes 00-59 of each hour scheduled for wake-up.,
/// </param>
/// <returns>
///   <c>YAPI_SUCCESS</c> if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWakeUpSchedule.set_minutes(bitmap:long):integer;
     begin
        self.set_minutesA(bitmap & 0x3fffffff);
        bitmap = bitmap >> 30;
        result:= self.set_minutesB(bitmap & 0x3fffffff);
            
     end;


function TYWakeUpSchedule.nextWakeUpSchedule(): TYWakeUpSchedule;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextWakeUpSchedule := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextWakeUpSchedule := nil;
      exit;
    end;
    nextWakeUpSchedule := yFindWakeUpSchedule(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYWakeUpSchedule.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYWakeUpSchedule.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYWakeUpSchedule.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYWakeUpSchedule.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YWakeUpSchedule implementation)

//--- (WakeUpSchedule functions)

function yFindWakeUpSchedule(func:string): TYWakeUpSchedule;
 var
   index: integer;
   res  : TYWakeUpSchedule;
 begin
    if (_WakeUpScheduleCache.Find(func, index)) then
     begin
       yFindWakeUpSchedule := TYWakeUpSchedule(_WakeUpScheduleCache.objects[index]);
       exit;
     end;
   res := TYWakeUpSchedule.Create(func);
   _WakeUpScheduleCache.addObject(func, res);
   yFindWakeUpSchedule := res;
 end;

function yFirstWakeUpSchedule(): TYWakeUpSchedule;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('WakeUpSchedule', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstWakeUpSchedule := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstWakeUpSchedule := nil;
       exit;
    end;
   yFirstWakeUpSchedule := yFindWakeUpSchedule(serial+'.'+funcId);
 end;

procedure _WakeUpScheduleCleanup();
  var i:integer;
begin
  for i:=0 to _WakeUpScheduleCache.count-1 do 
    begin
     _WakeUpScheduleCache.objects[i].free();
     _WakeUpScheduleCache.objects[i]:=nil;
    end;
   _WakeUpScheduleCache.free();
   _WakeUpScheduleCache:=nil;
end;

//--- (end of WakeUpSchedule functions)

initialization
   //--- (WakeUpSchedule initialization)
   _WakeUpScheduleCache        := TstringList.create();
   _WakeUpScheduleCache.sorted := true;
   //--- (end of WakeUpSchedule initialization)

finalization
   //--- (WakeUpSchedule cleanup)
   _WakeUpScheduleCleanup();
   //--- (end of WakeUpSchedule cleanup)
end.
