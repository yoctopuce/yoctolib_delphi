{*********************************************************************
 *
 * $Id: yocto_wakeupmonitor.pas 12324 2013-08-13 15:10:31Z mvuilleu $
 *
 * Implements yFindWakeUpMonitor(), the high-level API for WakeUpMonitor functions
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


unit yocto_wakeupmonitor;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YWakeUpMonitor definitions)

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_POWERDURATION_INVALID         = YAPI_INVALID_LONGINT;
   Y_SLEEPCOUNTDOWN_INVALID        = YAPI_INVALID_LONGINT;
   Y_NEXTWAKEUP_INVALID            = YAPI_INVALID_LONGWORD;
   Y_WAKEUPREASON_USBPOWER = 0;
   Y_WAKEUPREASON_EXTPOWER = 1;
   Y_WAKEUPREASON_ENDOFSLEEP = 2;
   Y_WAKEUPREASON_EXTSIG1 = 3;
   Y_WAKEUPREASON_EXTSIG2 = 4;
   Y_WAKEUPREASON_EXTSIG3 = 5;
   Y_WAKEUPREASON_EXTSIG4 = 6;
   Y_WAKEUPREASON_SCHEDULE1 = 7;
   Y_WAKEUPREASON_SCHEDULE2 = 8;
   Y_WAKEUPREASON_SCHEDULE3 = 9;
   Y_WAKEUPREASON_SCHEDULE4 = 10;
   Y_WAKEUPREASON_SCHEDULE5 = 11;
   Y_WAKEUPREASON_SCHEDULE6 = 12;
   Y_WAKEUPREASON_INVALID = -1;

   Y_WAKEUPSTATE_SLEEPING = 0;
   Y_WAKEUPSTATE_AWAKE = 1;
   Y_WAKEUPSTATE_INVALID = -1;

   Y_RTCTIME_INVALID               = YAPI_INVALID_LONGWORD;


//--- (end of YWakeUpMonitor definitions)

type
//--- (YWakeUpMonitor declaration)
 TYWakeUpMonitor = class;
 TUpdateCallback  = procedure(func: TYWakeUpMonitor; value:string);
////
/// <summary>
///   TYWakeUpMonitor Class: WakeUpMonitor function interface
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
///-
TYWakeUpMonitor=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _powerDuration            : LongInt;
   _sleepCountdown           : LongInt;
   _nextWakeUp               : LongWord;
   _wakeUpReason             : Integer;
   _wakeUpState              : Integer;
   _rtcTime                  : LongWord;
   _endOfTime                : LongWord;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YWakeUpMonitor declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of monitors started using <c>yFirstWakeUpMonitor()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YWakeUpMonitor</c> object, corresponding to
   ///   a monitor currently online, or a <c>null</c> pointer
   ///   if there are no more monitors to enumerate.
   /// </returns>
   ///-
   function nextWakeUpMonitor():TYWakeUpMonitor;

   //--- (YWakeUpMonitor accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the monitor.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the monitor
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the monitor.
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
   ///   a string corresponding to the logical name of the monitor
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
   ///   Returns the current value of the monitor (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the monitor (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the maximal wake up time (seconds) before going to sleep automatically.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the maximal wake up time (seconds) before going to sleep automatically
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_POWERDURATION_INVALID</c>.
   /// </para>
   ///-
   function get_powerDuration():LongInt;

   ////
   /// <summary>
   ///   Changes the maximal wake up time (seconds) before going to sleep automatically.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the maximal wake up time (seconds) before going to sleep automatically
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
   function set_powerDuration(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns the delay before next sleep.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the delay before next sleep
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_SLEEPCOUNTDOWN_INVALID</c>.
   /// </para>
   ///-
   function get_sleepCountdown():LongInt;

   ////
   /// <summary>
   ///   Changes the delay before next sleep.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the delay before next sleep
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
   function set_sleepCountdown(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns the next scheduled wake-up date/time (UNIX format)
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the next scheduled wake-up date/time (UNIX format)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_NEXTWAKEUP_INVALID</c>.
   /// </para>
   ///-
   function get_nextWakeUp():LongWord;

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
   function set_nextWakeUp(newval:LongWord):integer;

   ////
   /// <summary>
   ///   Return the last wake up reason.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a value among <c>Y_WAKEUPREASON_USBPOWER</c>, <c>Y_WAKEUPREASON_EXTPOWER</c>,
   ///   <c>Y_WAKEUPREASON_ENDOFSLEEP</c>, <c>Y_WAKEUPREASON_EXTSIG1</c>, <c>Y_WAKEUPREASON_EXTSIG2</c>,
   ///   <c>Y_WAKEUPREASON_EXTSIG3</c>, <c>Y_WAKEUPREASON_EXTSIG4</c>, <c>Y_WAKEUPREASON_SCHEDULE1</c>,
   ///   <c>Y_WAKEUPREASON_SCHEDULE2</c>, <c>Y_WAKEUPREASON_SCHEDULE3</c>, <c>Y_WAKEUPREASON_SCHEDULE4</c>,
   ///   <c>Y_WAKEUPREASON_SCHEDULE5</c> and <c>Y_WAKEUPREASON_SCHEDULE6</c>
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_WAKEUPREASON_INVALID</c>.
   /// </para>
   ///-
   function get_wakeUpReason():Integer;

   ////
   /// <summary>
   ///   Returns  the current state of the monitor
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_WAKEUPSTATE_SLEEPING</c> or <c>Y_WAKEUPSTATE_AWAKE</c>, according to  the current state
   ///   of the monitor
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_WAKEUPSTATE_INVALID</c>.
   /// </para>
   ///-
   function get_wakeUpState():Integer;

   function set_wakeUpState(newval:Integer):integer;

   function get_rtcTime():LongWord;

   ////
   /// <summary>
   ///   Forces a wakeup.
   /// <para>
   /// </para>
   /// </summary>
   ///-
   function wakeUp():integer;

   ////
   /// <summary>
   ///   Go to sleep until the next wakeup condition is met,  the
   ///   RTC time must have been set before calling this function.
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="secBeforeSleep">
   ///   number of seconds before going into sleep mode,
   /// </param>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function sleep(secBeforeSleep:integer):integer;

   ////
   /// <summary>
   ///   Go to sleep for a specific time or until the next wakeup condition is met, the
   ///   RTC time must have been set before calling this function.
   /// <para>
   ///   The count down before sleep
   ///   can be canceled with resetSleepCountDown.
   /// </para>
   /// </summary>
   /// <param name="secUntilWakeUp">
   ///   sleep duration, in secondes
   /// </param>
   /// <param name="secBeforeSleep">
   ///   number of seconds before going into sleep mode
   /// </param>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function sleepFor(secUntilWakeUp:integer; secBeforeSleep:integer):integer;

   ////
   /// <summary>
   ///   Go to sleep until a specific date is reached or until the next wakeup condition is met, the
   ///   RTC time must have been set before calling this function.
   /// <para>
   ///   The count down before sleep
   ///   can be canceled with resetSleepCountDown.
   /// </para>
   /// </summary>
   /// <param name="wakeUpTime">
   ///   wake-up datetime (UNIX format)
   /// </param>
   /// <param name="secBeforeSleep">
   ///   number of seconds before going into sleep mode
   /// </param>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function sleepUntil(wakeUpTime:integer; secBeforeSleep:integer):integer;

   ////
   /// <summary>
   ///   Reset the sleep countdown.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   ///   On failure, throws an exception or returns a negative error code.
   /// </returns>
   ///-
   function resetSleepCountDown():integer;

   //--- (end of YWakeUpMonitor accessors declaration)
end;

//--- (WakeUpMonitor functions declaration)

////
/// <summary>
///   Retrieves a monitor for a given identifier.
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
///   This function does not require that the monitor is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YWakeUpMonitor.isOnline()</c> to test if the monitor is
///   indeed online at a given time. In case of ambiguity when looking for
///   a monitor by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the monitor
/// </param>
/// <returns>
///   a <c>YWakeUpMonitor</c> object allowing you to drive the monitor.
/// </returns>
///-
function yFindWakeUpMonitor(func:string):TYWakeUpMonitor;
////
/// <summary>
///   Starts the enumeration of monitors currently accessible.
/// <para>
///   Use the method <c>YWakeUpMonitor.nextWakeUpMonitor()</c> to iterate on
///   next monitors.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YWakeUpMonitor</c> object, corresponding to
///   the first monitor currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstWakeUpMonitor():TYWakeUpMonitor;

//--- (end of WakeUpMonitor functions declaration)

implementation

//--- (YWakeUpMonitor implementation)

var
   _WakeUpMonitorCache : TStringList;

constructor TYWakeUpMonitor.Create(func:string);
 begin
   inherited Create('WakeUpMonitor', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _powerDuration := Y_POWERDURATION_INVALID;
   _sleepCountdown := Y_SLEEPCOUNTDOWN_INVALID;
   _nextWakeUp := Y_NEXTWAKEUP_INVALID;
   _wakeUpReason := Y_WAKEUPREASON_INVALID;
   _wakeUpState := Y_WAKEUPSTATE_INVALID;
   _rtcTime := Y_RTCTIME_INVALID;
   _endOfTime := 2145960000;
 end;

{$HINTS OFF}
function TYWakeUpMonitor._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'powerDuration') then
       begin
         _powerDuration := member^.ivalue;
       end else
      if (member^.name = 'sleepCountdown') then
       begin
         _sleepCountdown := member^.ivalue;
       end else
      if (member^.name = 'nextWakeUp') then
       begin
         _nextWakeUp := member^.ivalue;
       end else
      if (member^.name = 'wakeUpReason') then
       begin
         _wakeUpReason := member^.ivalue;
       end else
      if (member^.name = 'wakeUpState') then
       begin
         _wakeUpState := member^.ivalue;
       end else
      if (member^.name = 'rtcTime') then
       begin
         _rtcTime := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the monitor.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the monitor
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYWakeUpMonitor.get_logicalName():string;
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
///   Changes the logical name of the monitor.
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
///   a string corresponding to the logical name of the monitor
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
function TYWakeUpMonitor.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the monitor (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the monitor (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYWakeUpMonitor.get_advertisedValue():string;
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
///   Returns the maximal wake up time (seconds) before going to sleep automatically.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the maximal wake up time (seconds) before going to sleep automatically
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_POWERDURATION_INVALID.
/// </para>
///-
function TYWakeUpMonitor.get_powerDuration():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_POWERDURATION_INVALID;
         exit;
       end;
   result := _powerDuration;
 end;

////
/// <summary>
///   Changes the maximal wake up time (seconds) before going to sleep automatically.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the maximal wake up time (seconds) before going to sleep automatically
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
function TYWakeUpMonitor.set_powerDuration(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('powerDuration',rest_val);
 end;

////
/// <summary>
///   Returns the delay before next sleep.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the delay before next sleep
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_SLEEPCOUNTDOWN_INVALID.
/// </para>
///-
function TYWakeUpMonitor.get_sleepCountdown():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_SLEEPCOUNTDOWN_INVALID;
         exit;
       end;
   result := _sleepCountdown;
 end;

////
/// <summary>
///   Changes the delay before next sleep.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the delay before next sleep
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
function TYWakeUpMonitor.set_sleepCountdown(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('sleepCountdown',rest_val);
 end;

////
/// <summary>
///   Returns the next scheduled wake-up date/time (UNIX format)
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the next scheduled wake-up date/time (UNIX format)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_NEXTWAKEUP_INVALID.
/// </para>
///-
function TYWakeUpMonitor.get_nextWakeUp():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_NEXTWAKEUP_INVALID;
         exit;
       end;
   result := _nextWakeUp;
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
function TYWakeUpMonitor.set_nextWakeUp(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('nextWakeUp',rest_val);
 end;

////
/// <summary>
///   Return the last wake up reason.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a value among Y_WAKEUPREASON_USBPOWER, Y_WAKEUPREASON_EXTPOWER, Y_WAKEUPREASON_ENDOFSLEEP,
///   Y_WAKEUPREASON_EXTSIG1, Y_WAKEUPREASON_EXTSIG2, Y_WAKEUPREASON_EXTSIG3, Y_WAKEUPREASON_EXTSIG4,
///   Y_WAKEUPREASON_SCHEDULE1, Y_WAKEUPREASON_SCHEDULE2, Y_WAKEUPREASON_SCHEDULE3,
///   Y_WAKEUPREASON_SCHEDULE4, Y_WAKEUPREASON_SCHEDULE5 and Y_WAKEUPREASON_SCHEDULE6
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_WAKEUPREASON_INVALID.
/// </para>
///-
function TYWakeUpMonitor.get_wakeUpReason():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_WAKEUPREASON_INVALID;
         exit;
       end;
   result := _wakeUpReason;
 end;

////
/// <summary>
///   Returns  the current state of the monitor
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_WAKEUPSTATE_SLEEPING or Y_WAKEUPSTATE_AWAKE, according to  the current state of the monitor
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_WAKEUPSTATE_INVALID.
/// </para>
///-
function TYWakeUpMonitor.get_wakeUpState():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_WAKEUPSTATE_INVALID;
         exit;
       end;
   result := _wakeUpState;
 end;

function TYWakeUpMonitor.set_wakeUpState(newval:Integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('wakeUpState',rest_val);
 end;

function TYWakeUpMonitor.get_rtcTime():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_RTCTIME_INVALID;
         exit;
       end;
   result := _rtcTime;
 end;

////
/// <summary>
///   Forces a wakeup.
/// <para>
/// </para>
/// </summary>
///-
function TYWakeUpMonitor.wakeUp():integer;
     begin
        result:= self.set_wakeUpState(Y_WAKEUPSTATE_AWAKE);
            
     end;


////
/// <summary>
///   Go to sleep until the next wakeup condition is met,  the
///   RTC time must have been set before calling this function.
/// <para>
/// </para>
/// </summary>
/// <param name="secBeforeSleep">
///   number of seconds before going into sleep mode,
/// </param>
/// <returns>
///   <c>YAPI_SUCCESS</c> if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWakeUpMonitor.sleep(secBeforeSleep:integer):integer;
     var
        currTime : integer;
     begin
        currTime = self.get_rtcTime();
        if not(currTime != 0) then begin self._throw( YAPI_RTC_NOT_READY, 'RTC time not set'); result:= YAPI_RTC_NOT_READY; exit; end;;
        self.set_nextWakeUp(self._endOfTime);
        self.set_sleepCountdown(secBeforeSleep);
        result:= YAPI_SUCCESS; 
            
     end;


////
/// <summary>
///   Go to sleep for a specific time or until the next wakeup condition is met, the
///   RTC time must have been set before calling this function.
/// <para>
///   The count down before sleep
///   can be canceled with resetSleepCountDown.
/// </para>
/// </summary>
/// <param name="secUntilWakeUp">
///   sleep duration, in secondes
/// </param>
/// <param name="secBeforeSleep">
///   number of seconds before going into sleep mode
/// </param>
/// <returns>
///   <c>YAPI_SUCCESS</c> if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWakeUpMonitor.sleepFor(secUntilWakeUp:integer; secBeforeSleep:integer):integer;
     var
        currTime : integer;
     begin
        currTime = self.get_rtcTime();
        if not(currTime != 0) then begin self._throw( YAPI_RTC_NOT_READY, 'RTC time not set'); result:= YAPI_RTC_NOT_READY; exit; end;;
        self.set_nextWakeUp(currTime+secUntilWakeUp);
        self.set_sleepCountdown(secBeforeSleep);
        result:= YAPI_SUCCESS; 
            
     end;


////
/// <summary>
///   Go to sleep until a specific date is reached or until the next wakeup condition is met, the
///   RTC time must have been set before calling this function.
/// <para>
///   The count down before sleep
///   can be canceled with resetSleepCountDown.
/// </para>
/// </summary>
/// <param name="wakeUpTime">
///   wake-up datetime (UNIX format)
/// </param>
/// <param name="secBeforeSleep">
///   number of seconds before going into sleep mode
/// </param>
/// <returns>
///   <c>YAPI_SUCCESS</c> if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWakeUpMonitor.sleepUntil(wakeUpTime:integer; secBeforeSleep:integer):integer;
     var
        currTime : integer;
     begin
        currTime = self.get_rtcTime();
        if not(currTime != 0) then begin self._throw( YAPI_RTC_NOT_READY, 'RTC time not set'); result:= YAPI_RTC_NOT_READY; exit; end;;
        self.set_nextWakeUp(wakeUpTime);
        self.set_sleepCountdown(secBeforeSleep);
        result:= YAPI_SUCCESS; 
            
     end;


////
/// <summary>
///   Reset the sleep countdown.
/// <para>
/// </para>
/// </summary>
/// <returns>
///   <c>YAPI_SUCCESS</c> if the call succeeds.
///   On failure, throws an exception or returns a negative error code.
/// </returns>
///-
function TYWakeUpMonitor.resetSleepCountDown():integer;
     begin
        self.set_sleepCountdown(0);
        self.set_nextWakeUp(0);
        result:= YAPI_SUCCESS; 
            
     end;


function TYWakeUpMonitor.nextWakeUpMonitor(): TYWakeUpMonitor;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextWakeUpMonitor := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextWakeUpMonitor := nil;
      exit;
    end;
    nextWakeUpMonitor := yFindWakeUpMonitor(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYWakeUpMonitor.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYWakeUpMonitor.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYWakeUpMonitor.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYWakeUpMonitor.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YWakeUpMonitor implementation)

//--- (WakeUpMonitor functions)

function yFindWakeUpMonitor(func:string): TYWakeUpMonitor;
 var
   index: integer;
   res  : TYWakeUpMonitor;
 begin
    if (_WakeUpMonitorCache.Find(func, index)) then
     begin
       yFindWakeUpMonitor := TYWakeUpMonitor(_WakeUpMonitorCache.objects[index]);
       exit;
     end;
   res := TYWakeUpMonitor.Create(func);
   _WakeUpMonitorCache.addObject(func, res);
   yFindWakeUpMonitor := res;
 end;

function yFirstWakeUpMonitor(): TYWakeUpMonitor;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('WakeUpMonitor', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstWakeUpMonitor := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstWakeUpMonitor := nil;
       exit;
    end;
   yFirstWakeUpMonitor := yFindWakeUpMonitor(serial+'.'+funcId);
 end;

procedure _WakeUpMonitorCleanup();
  var i:integer;
begin
  for i:=0 to _WakeUpMonitorCache.count-1 do 
    begin
     _WakeUpMonitorCache.objects[i].free();
     _WakeUpMonitorCache.objects[i]:=nil;
    end;
   _WakeUpMonitorCache.free();
   _WakeUpMonitorCache:=nil;
end;

//--- (end of WakeUpMonitor functions)

initialization
   //--- (WakeUpMonitor initialization)
   _WakeUpMonitorCache        := TstringList.create();
   _WakeUpMonitorCache.sorted := true;
   //--- (end of WakeUpMonitor initialization)

finalization
   //--- (WakeUpMonitor cleanup)
   _WakeUpMonitorCleanup();
   //--- (end of WakeUpMonitor cleanup)
end.
