{*********************************************************************
 *
 * $Id: yocto_realtimeclock.pas 15254 2014-03-06 10:16:24Z seb $
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

const Y_UNIXTIME_INVALID              = YAPI_INVALID_LONG;
const Y_DATETIME_INVALID              = YAPI_INVALID_STRING;
const Y_UTCOFFSET_INVALID             = YAPI_INVALID_INT;
const Y_TIMESET_FALSE = 0;
const Y_TIMESET_TRUE = 1;
const Y_TIMESET_INVALID = -1;



//--- (end of YRealTimeClock definitions)

type
  TYRealTimeClock = class;
  //--- (YRealTimeClock class start)
  TYRealTimeClockValueCallback = procedure(func: TYRealTimeClock; value:string);
  TYRealTimeClockTimedReportCallback = procedure(func: TYRealTimeClock; value:TYMeasure);

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
  //--- (end of YRealTimeClock class start)
  protected
  //--- (YRealTimeClock declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _unixTime                 : int64;
    _dateTime                 : string;
    _utcOffset                : LongInt;
    _timeSet                  : Integer;
    _valueCallbackRealTimeClock : TYRealTimeClockValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YRealTimeClock declaration)

  public
    //--- (YRealTimeClock accessors declaration)
    constructor Create(func:string);

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
    function get_unixTime():int64;

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
    function set_unixTime(newval:int64):integer;

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

    ////
    /// <summary>
    ///   Retrieves $AFUNCTION$ for a given identifier.
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
    ///   This function does not require that $THEFUNCTION$ is online at the time
    ///   it is invoked. The returned object is nevertheless valid.
    ///   Use the method <c>YRealTimeClock.isOnline()</c> to test if $THEFUNCTION$ is
    ///   indeed online at a given time. In case of ambiguity when looking for
    ///   $AFUNCTION$ by logical name, no error is notified: the first instance
    ///   found is returned. The search is performed first by hardware name,
    ///   then by logical name.
    /// </para>
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$
    /// </param>
    /// <returns>
    ///   a <c>YRealTimeClock</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindRealTimeClock(func: string):TYRealTimeClock;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every change of advertised value.
    /// <para>
    ///   The callback is invoked only during the execution of <c>ySleep</c> or <c>yHandleEvents</c>.
    ///   This provides control over the time when the callback is triggered. For good responsiveness, remember to call
    ///   one of these two functions periodically. To unregister a callback, pass a null pointer as argument.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="callback">
    ///   the callback function to call, or a null pointer. The callback function should take two
    ///   arguments: the function object of which the value has changed, and the character string describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerValueCallback(callback: TYRealTimeClockValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


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
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstRealTimeClock():TYRealTimeClock;
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

  constructor TYRealTimeClock.Create(func:string);
    begin
      inherited Create(func);
      _className := 'RealTimeClock';
      //--- (YRealTimeClock accessors initialization)
      _unixTime := Y_UNIXTIME_INVALID;
      _dateTime := Y_DATETIME_INVALID;
      _utcOffset := Y_UTCOFFSET_INVALID;
      _timeSet := Y_TIMESET_INVALID;
      _valueCallbackRealTimeClock := nil;
      //--- (end of YRealTimeClock accessors initialization)
    end;


//--- (YRealTimeClock implementation)
{$HINTS OFF}
  function TYRealTimeClock._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'unixTime') then
        begin
          _unixTime := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'dateTime') then
        begin
          _dateTime := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'utcOffset') then
        begin
          _utcOffset := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'timeSet') then
        begin
          _timeSet := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

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
  function TYRealTimeClock.get_unixTime():int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_UNIXTIME_INVALID;
              exit
            end;
        end;
      result := self._unixTime;
      exit;
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
  function TYRealTimeClock.set_unixTime(newval:int64):integer;
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
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_DATETIME_INVALID;
              exit
            end;
        end;
      result := self._dateTime;
      exit;
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
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_UTCOFFSET_INVALID;
              exit
            end;
        end;
      result := self._utcOffset;
      exit;
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
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_TIMESET_INVALID;
              exit
            end;
        end;
      result := self._timeSet;
      exit;
    end;


  ////
  /// <summary>
  ///   Retrieves $AFUNCTION$ for a given identifier.
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
  ///   This function does not require that $THEFUNCTION$ is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YRealTimeClock.isOnline()</c> to test if $THEFUNCTION$ is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   $AFUNCTION$ by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes $THEFUNCTION$
  /// </param>
  /// <returns>
  ///   a <c>YRealTimeClock</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYRealTimeClock.FindRealTimeClock(func: string):TYRealTimeClock;
    var
      obj : TYRealTimeClock;
    begin
      obj := TYRealTimeClock(TYFunction._FindFromCache('RealTimeClock', func));
      if obj = nil then
        begin
          obj :=  TYRealTimeClock.create(func);
          TYFunction._AddToCache('RealTimeClock',  func, obj)
        end;
      result := obj;
      exit;
    end;


  ////
  /// <summary>
  ///   Registers the callback function that is invoked on every change of advertised value.
  /// <para>
  ///   The callback is invoked only during the execution of <c>ySleep</c> or <c>yHandleEvents</c>.
  ///   This provides control over the time when the callback is triggered. For good responsiveness, remember to call
  ///   one of these two functions periodically. To unregister a callback, pass a null pointer as argument.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="callback">
  ///   the callback function to call, or a null pointer. The callback function should take two
  ///   arguments: the function object of which the value has changed, and the character string describing
  ///   the new advertised value.
  /// @noreturn
  /// </param>
  ///-
  function TYRealTimeClock.registerValueCallback(callback: TYRealTimeClockValueCallback):LongInt;
    var
      val : string;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateValueCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateValueCallbackList(self, false)
        end;
      self._valueCallbackRealTimeClock := callback;
      // Immediately invoke value callback with current value
      if (addr(callback) <> nil) and self.isOnline then
        begin
          val := self._advertisedValue;
          if not((val = '')) then
            begin
              self._invokeValueCallback(val)
            end;
        end;
      result := 0;
      exit;
    end;


  function TYRealTimeClock._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackRealTimeClock) <> nil) then
        begin
          self._valueCallbackRealTimeClock(self, value)
        end
      else
        begin
          inherited _invokeValueCallback(value)
        end;
      result := 0;
      exit;
    end;


  function TYRealTimeClock.nextRealTimeClock(): TYRealTimeClock;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextRealTimeClock := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextRealTimeClock := nil;
          exit;
        end;
      nextRealTimeClock := TYRealTimeClock.FindRealTimeClock(hwid);
    end;

  class function TYRealTimeClock.FirstRealTimeClock(): TYRealTimeClock;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('RealTimeClock', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
      if (YISERR(err) or (neededsize = 0)) then
        begin
          result := nil;
          exit;
        end;
      if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
        begin
          result := nil;
          exit;
        end;
     result := TYRealTimeClock.FindRealTimeClock(serial+'.'+funcId);
    end;

//--- (end of YRealTimeClock implementation)

//--- (RealTimeClock functions)

  function yFindRealTimeClock(func:string): TYRealTimeClock;
    begin
      result := TYRealTimeClock.FindRealTimeClock(func);
    end;

  function yFirstRealTimeClock(): TYRealTimeClock;
    begin
      result := TYRealTimeClock.FirstRealTimeClock();
    end;

  procedure _RealTimeClockCleanup();
    begin
    end;

//--- (end of RealTimeClock functions)

initialization
  //--- (RealTimeClock initialization)
  //--- (end of RealTimeClock initialization)

finalization
  //--- (RealTimeClock cleanup)
  _RealTimeClockCleanup();
  //--- (end of RealTimeClock cleanup)
end.
