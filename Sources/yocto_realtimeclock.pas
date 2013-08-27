{*********************************************************************
 *
 * $Id: yocto_realtimeclock.pas 12324 2013-08-13 15:10:31Z mvuilleu $
 *
 * Implements yFindRealTimeClock(), the high-level API for RealTimeClock functions
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


unit yocto_realtimeclock;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YRealTimeClock definitions)

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_UNIXTIME_INVALID              = YAPI_INVALID_LONGWORD;
   Y_DATETIME_INVALID              = YAPI_INVALID_STRING;
   Y_UTCOFFSET_INVALID             = YAPI_INVALID_LONGINT;
   Y_TIMESET_FALSE = 0;
   Y_TIMESET_TRUE = 1;
   Y_TIMESET_INVALID = -1;



//--- (end of YRealTimeClock definitions)

type
//--- (YRealTimeClock declaration)
 TYRealTimeClock = class;
 TUpdateCallback  = procedure(func: TYRealTimeClock; value:string);
////
/// <summary>
///   TYRealTimeClock Class: Real Time Clock function interface
/// <para>
///   The RealTimeClock function maintains and provides current date and time, even accross power cut
///   lasting several days. It is the base for automated wake-up functions provided by the WakeUpScheduler.
///   The current time may represent a local time as well as an UTC time, but no automatic time change
///   will occur to account for daylight saving time.
/// </para>
/// </summary>
///-
TYRealTimeClock=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _unixTime                 : LongWord;
   _dateTime                 : string;
   _utcOffset                : LongInt;
   _timeSet                  : Integer;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YRealTimeClock declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of clocks started using <c>yFirstRealTimeClock()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YRealTimeClock</c> object, corresponding to
   ///   a clock currently online, or a <c>null</c> pointer
   ///   if there are no more clocks to enumerate.
   /// </returns>
   ///-
   function nextRealTimeClock():TYRealTimeClock;

   //--- (YRealTimeClock accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the clock.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the clock
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the clock.
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
   ///   a string corresponding to the logical name of the clock
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
   ///   Returns the current value of the clock (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the clock (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the current time in Unix format (number of elapsed seconds since Jan 1st, 1970).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the current time in Unix format (number of elapsed seconds since Jan 1st, 1970)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_UNIXTIME_INVALID</c>.
   /// </para>
   ///-
   function get_unixTime():LongWord;

   ////
   /// <summary>
   ///   Changes the current time.
   /// <para>
   ///   Time is specifid in Unix format (number of elapsed seconds since Jan 1st, 1970).
   ///   If current UTC time is known, utcOffset will be automatically adjusted for the new specified time.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the current time
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
   function set_unixTime(newval:LongWord):integer;

   ////
   /// <summary>
   ///   Returns the current time in the form "YYYY/MM/DD hh:mm:ss"
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current time in the form "YYYY/MM/DD hh:mm:ss"
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_DATETIME_INVALID</c>.
   /// </para>
   ///-
   function get_dateTime():string;

   ////
   /// <summary>
   ///   Returns the number of seconds between current time and UTC time (time zone).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the number of seconds between current time and UTC time (time zone)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_UTCOFFSET_INVALID</c>.
   /// </para>
   ///-
   function get_utcOffset():LongInt;

   ////
   /// <summary>
   ///   Changes the number of seconds between current time and UTC time (time zone).
   /// <para>
   ///   The timezone is automatically rounded to the nearest multiple of 15 minutes.
   ///   If current UTC time is known, the current time will automatically be updated according to the
   ///   selected time zone.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the number of seconds between current time and UTC time (time zone)
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
   function set_utcOffset(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns true if the clock has been set, and false otherwise.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_TIMESET_FALSE</c> or <c>Y_TIMESET_TRUE</c>, according to true if the clock has been
   ///   set, and false otherwise
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_TIMESET_INVALID</c>.
   /// </para>
   ///-
   function get_timeSet():Integer;

   //--- (end of YRealTimeClock accessors declaration)
end;

//--- (RealTimeClock functions declaration)

////
/// <summary>
///   Retrieves a clock for a given identifier.
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
///   This function does not require that the clock is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YRealTimeClock.isOnline()</c> to test if the clock is
///   indeed online at a given time. In case of ambiguity when looking for
///   a clock by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the clock
/// </param>
/// <returns>
///   a <c>YRealTimeClock</c> object allowing you to drive the clock.
/// </returns>
///-
function yFindRealTimeClock(func:string):TYRealTimeClock;
////
/// <summary>
///   Starts the enumeration of clocks currently accessible.
/// <para>
///   Use the method <c>YRealTimeClock.nextRealTimeClock()</c> to iterate on
///   next clocks.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YRealTimeClock</c> object, corresponding to
///   the first clock currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstRealTimeClock():TYRealTimeClock;

//--- (end of RealTimeClock functions declaration)

implementation

//--- (YRealTimeClock implementation)

var
   _RealTimeClockCache : TStringList;

constructor TYRealTimeClock.Create(func:string);
 begin
   inherited Create('RealTimeClock', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _unixTime := Y_UNIXTIME_INVALID;
   _dateTime := Y_DATETIME_INVALID;
   _utcOffset := Y_UTCOFFSET_INVALID;
   _timeSet := Y_TIMESET_INVALID;
 end;

{$HINTS OFF}
function TYRealTimeClock._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'unixTime') then
       begin
         _unixTime := member^.ivalue;
       end else
      if (member^.name = 'dateTime') then
       begin
         _dateTime := string(member^.svalue);
       end else
      if (member^.name = 'utcOffset') then
       begin
         _utcOffset := member^.ivalue;
       end else
      if (member^.name = 'timeSet') then
       begin
         _timeSet := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the clock.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the clock
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYRealTimeClock.get_logicalName():string;
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
///   Changes the logical name of the clock.
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
///   a string corresponding to the logical name of the clock
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
function TYRealTimeClock.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the clock (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the clock (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYRealTimeClock.get_advertisedValue():string;
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
///   Returns the current time in Unix format (number of elapsed seconds since Jan 1st, 1970).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the current time in Unix format (number of elapsed seconds since Jan 1st, 1970)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_UNIXTIME_INVALID.
/// </para>
///-
function TYRealTimeClock.get_unixTime():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_UNIXTIME_INVALID;
         exit;
       end;
   result := _unixTime;
 end;

////
/// <summary>
///   Changes the current time.
/// <para>
///   Time is specifid in Unix format (number of elapsed seconds since Jan 1st, 1970).
///   If current UTC time is known, utcOffset will be automatically adjusted for the new specified time.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the current time
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
function TYRealTimeClock.set_unixTime(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('unixTime',rest_val);
 end;

////
/// <summary>
///   Returns the current time in the form "YYYY/MM/DD hh:mm:ss"
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current time in the form "YYYY/MM/DD hh:mm:ss"
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_DATETIME_INVALID.
/// </para>
///-
function TYRealTimeClock.get_dateTime():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_DATETIME_INVALID;
         exit;
       end;
   result := _dateTime;
 end;

////
/// <summary>
///   Returns the number of seconds between current time and UTC time (time zone).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the number of seconds between current time and UTC time (time zone)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_UTCOFFSET_INVALID.
/// </para>
///-
function TYRealTimeClock.get_utcOffset():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_UTCOFFSET_INVALID;
         exit;
       end;
   result := _utcOffset;
 end;

////
/// <summary>
///   Changes the number of seconds between current time and UTC time (time zone).
/// <para>
///   The timezone is automatically rounded to the nearest multiple of 15 minutes.
///   If current UTC time is known, the current time will automatically be updated according to the
///   selected time zone.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the number of seconds between current time and UTC time (time zone)
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
function TYRealTimeClock.set_utcOffset(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('utcOffset',rest_val);
 end;

////
/// <summary>
///   Returns true if the clock has been set, and false otherwise.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_TIMESET_FALSE or Y_TIMESET_TRUE, according to true if the clock has been set, and false otherwise
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_TIMESET_INVALID.
/// </para>
///-
function TYRealTimeClock.get_timeSet():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_TIMESET_INVALID;
         exit;
       end;
   result := _timeSet;
 end;

function TYRealTimeClock.nextRealTimeClock(): TYRealTimeClock;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextRealTimeClock := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextRealTimeClock := nil;
      exit;
    end;
    nextRealTimeClock := yFindRealTimeClock(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYRealTimeClock.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYRealTimeClock.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYRealTimeClock.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYRealTimeClock.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YRealTimeClock implementation)

//--- (RealTimeClock functions)

function yFindRealTimeClock(func:string): TYRealTimeClock;
 var
   index: integer;
   res  : TYRealTimeClock;
 begin
    if (_RealTimeClockCache.Find(func, index)) then
     begin
       yFindRealTimeClock := TYRealTimeClock(_RealTimeClockCache.objects[index]);
       exit;
     end;
   res := TYRealTimeClock.Create(func);
   _RealTimeClockCache.addObject(func, res);
   yFindRealTimeClock := res;
 end;

function yFirstRealTimeClock(): TYRealTimeClock;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('RealTimeClock', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstRealTimeClock := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstRealTimeClock := nil;
       exit;
    end;
   yFirstRealTimeClock := yFindRealTimeClock(serial+'.'+funcId);
 end;

procedure _RealTimeClockCleanup();
  var i:integer;
begin
  for i:=0 to _RealTimeClockCache.count-1 do 
    begin
     _RealTimeClockCache.objects[i].free();
     _RealTimeClockCache.objects[i]:=nil;
    end;
   _RealTimeClockCache.free();
   _RealTimeClockCache:=nil;
end;

//--- (end of RealTimeClock functions)

initialization
   //--- (RealTimeClock initialization)
   _RealTimeClockCache        := TstringList.create();
   _RealTimeClockCache.sorted := true;
   //--- (end of RealTimeClock initialization)

finalization
   //--- (RealTimeClock cleanup)
   _RealTimeClockCleanup();
   //--- (end of RealTimeClock cleanup)
end.
