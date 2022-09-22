{*********************************************************************
 *
 *  $Id: yocto_realtimeclock.pas 50595 2022-07-28 07:54:15Z mvuilleu $
 *
 *  Implements yFindRealTimeClock(), the high-level API for RealTimeClock functions
 *
 *  - - - - - - - - - License information: - - - - - - - - -
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
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YRealTimeClock definitions)

const Y_UNIXTIME_INVALID              = YAPI_INVALID_LONG;
const Y_DATETIME_INVALID              = YAPI_INVALID_STRING;
const Y_UTCOFFSET_INVALID             = YAPI_INVALID_INT;
const Y_TIMESET_FALSE = 0;
const Y_TIMESET_TRUE = 1;
const Y_TIMESET_INVALID = -1;
const Y_DISABLEHOSTSYNC_FALSE = 0;
const Y_DISABLEHOSTSYNC_TRUE = 1;
const Y_DISABLEHOSTSYNC_INVALID = -1;


//--- (end of YRealTimeClock definitions)
//--- (YRealTimeClock yapiwrapper declaration)
//--- (end of YRealTimeClock yapiwrapper declaration)

type
  TYRealTimeClock = class;
  //--- (YRealTimeClock class start)
  TYRealTimeClockValueCallback = procedure(func: TYRealTimeClock; value:string);
  TYRealTimeClockTimedReportCallback = procedure(func: TYRealTimeClock; value:TYMeasure);

  ////
  /// <summary>
  ///   TYRealTimeClock Class: real-time clock control interface, available for instance in the
  ///   YoctoHub-GSM-4G, the YoctoHub-Wireless-SR, the YoctoHub-Wireless-g or the YoctoHub-Wireless-n
  /// <para>
  ///   The <c>YRealTimeClock</c> class provide access to the embedded real-time clock available on some Yoctopuce
  ///   devices. It can provide current date and time, even after a power outage
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
    _unixTime                 : int64;
    _dateTime                 : string;
    _utcOffset                : LongInt;
    _timeSet                  : Integer;
    _disableHostSync          : Integer;
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
    ///   On failure, throws an exception or returns <c>YRealTimeClock.UNIXTIME_INVALID</c>.
    /// </para>
    ///-
    function get_unixTime():int64;

    ////
    /// <summary>
    ///   Changes the current time.
    /// <para>
    ///   Time is specifid in Unix format (number of elapsed seconds since Jan 1st, 1970).
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_unixTime(newval:int64):integer;

    ////
    /// <summary>
    ///   Returns the current time in the form "YYYY/MM/DD hh:mm:ss".
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the current time in the form "YYYY/MM/DD hh:mm:ss"
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRealTimeClock.DATETIME_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YRealTimeClock.UTCOFFSET_INVALID</c>.
    /// </para>
    ///-
    function get_utcOffset():LongInt;

    ////
    /// <summary>
    ///   Changes the number of seconds between current time and UTC time (time zone).
    /// <para>
    ///   The timezone is automatically rounded to the nearest multiple of 15 minutes.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
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
    ///   either <c>YRealTimeClock.TIMESET_FALSE</c> or <c>YRealTimeClock.TIMESET_TRUE</c>, according to true
    ///   if the clock has been set, and false otherwise
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRealTimeClock.TIMESET_INVALID</c>.
    /// </para>
    ///-
    function get_timeSet():Integer;

    ////
    /// <summary>
    ///   Returns true if the automatic clock synchronization with host has been disabled,
    ///   and false otherwise.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YRealTimeClock.DISABLEHOSTSYNC_FALSE</c> or <c>YRealTimeClock.DISABLEHOSTSYNC_TRUE</c>,
    ///   according to true if the automatic clock synchronization with host has been disabled,
    ///   and false otherwise
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRealTimeClock.DISABLEHOSTSYNC_INVALID</c>.
    /// </para>
    ///-
    function get_disableHostSync():Integer;

    ////
    /// <summary>
    ///   Changes the automatic clock synchronization with host working state.
    /// <para>
    ///   To disable automatic synchronization, set the value to true.
    ///   To enable automatic synchronization (default), set the value to false.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YRealTimeClock.DISABLEHOSTSYNC_FALSE</c> or <c>YRealTimeClock.DISABLEHOSTSYNC_TRUE</c>,
    ///   according to the automatic clock synchronization with host working state
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_disableHostSync(newval:Integer):integer;

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
    /// <para>
    ///   If a call to this object's is_online() method returns FALSE although
    ///   you are certain that the matching device is plugged, make sure that you did
    ///   call registerHub() at application initialization time.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$, for instance
    ///   <c>$FULLHARDWAREID$</c>.
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
    ///   one of these two functions periodically. To unregister a callback, pass a NIL pointer as argument.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="callback">
    ///   the callback function to call, or a NIL pointer. The callback function should take two
    ///   arguments: the function object of which the value has changed, and the character string describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerValueCallback(callback: TYRealTimeClockValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of real-time clocks started using <c>yFirstRealTimeClock()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned real-time clocks order.
    ///   If you want to find a specific a real-time clock, use <c>RealTimeClock.findRealTimeClock()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YRealTimeClock</c> object, corresponding to
    ///   a real-time clock currently online, or a <c>NIL</c> pointer
    ///   if there are no more real-time clocks to enumerate.
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

//--- (YRealTimeClock functions declaration)
  ////
  /// <summary>
  ///   Retrieves a real-time clock for a given identifier.
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
  ///   This function does not require that the real-time clock is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YRealTimeClock.isOnline()</c> to test if the real-time clock is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a real-time clock by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// <para>
  ///   If a call to this object's is_online() method returns FALSE although
  ///   you are certain that the matching device is plugged, make sure that you did
  ///   call registerHub() at application initialization time.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the real-time clock, for instance
  ///   <c>YHUBGSM5.realTimeClock</c>.
  /// </param>
  /// <returns>
  ///   a <c>YRealTimeClock</c> object allowing you to drive the real-time clock.
  /// </returns>
  ///-
  function yFindRealTimeClock(func:string):TYRealTimeClock;
  ////
  /// <summary>
  ///   Starts the enumeration of real-time clocks currently accessible.
  /// <para>
  ///   Use the method <c>YRealTimeClock.nextRealTimeClock()</c> to iterate on
  ///   next real-time clocks.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YRealTimeClock</c> object, corresponding to
  ///   the first real-time clock currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstRealTimeClock():TYRealTimeClock;

//--- (end of YRealTimeClock functions declaration)

implementation
//--- (YRealTimeClock dlldef)
//--- (end of YRealTimeClock dlldef)

  constructor TYRealTimeClock.Create(func:string);
    begin
      inherited Create(func);
      _className := 'RealTimeClock';
      //--- (YRealTimeClock accessors initialization)
      _unixTime := Y_UNIXTIME_INVALID;
      _dateTime := Y_DATETIME_INVALID;
      _utcOffset := Y_UTCOFFSET_INVALID;
      _timeSet := Y_TIMESET_INVALID;
      _disableHostSync := Y_DISABLEHOSTSYNC_INVALID;
      _valueCallbackRealTimeClock := nil;
      //--- (end of YRealTimeClock accessors initialization)
    end;

//--- (YRealTimeClock yapiwrapper)
//--- (end of YRealTimeClock yapiwrapper)

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
      if (member^.name = 'disableHostSync') then
        begin
          _disableHostSync := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYRealTimeClock.get_unixTime():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_UNIXTIME_INVALID;
              exit;
            end;
        end;
      res := self._unixTime;
      result := res;
      exit;
    end;


  function TYRealTimeClock.set_unixTime(newval:int64):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('unixTime',rest_val);
    end;

  function TYRealTimeClock.get_dateTime():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DATETIME_INVALID;
              exit;
            end;
        end;
      res := self._dateTime;
      result := res;
      exit;
    end;


  function TYRealTimeClock.get_utcOffset():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_UTCOFFSET_INVALID;
              exit;
            end;
        end;
      res := self._utcOffset;
      result := res;
      exit;
    end;


  function TYRealTimeClock.set_utcOffset(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('utcOffset',rest_val);
    end;

  function TYRealTimeClock.get_timeSet():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_TIMESET_INVALID;
              exit;
            end;
        end;
      res := self._timeSet;
      result := res;
      exit;
    end;


  function TYRealTimeClock.get_disableHostSync():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DISABLEHOSTSYNC_INVALID;
              exit;
            end;
        end;
      res := self._disableHostSync;
      result := res;
      exit;
    end;


  function TYRealTimeClock.set_disableHostSync(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('disableHostSync',rest_val);
    end;

  class function TYRealTimeClock.FindRealTimeClock(func: string):TYRealTimeClock;
    var
      obj : TYRealTimeClock;
    begin
      obj := TYRealTimeClock(TYFunction._FindFromCache('RealTimeClock', func));
      if obj = nil then
        begin
          obj :=  TYRealTimeClock.create(func);
          TYFunction._AddToCache('RealTimeClock',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYRealTimeClock.registerValueCallback(callback: TYRealTimeClockValueCallback):LongInt;
    var
      val : string;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateValueCallbackList(self, true);
        end
      else
        begin
          TYFunction._UpdateValueCallbackList(self, false);
        end;
      self._valueCallbackRealTimeClock := callback;
      // Immediately invoke value callback with current value
      if (addr(callback) <> nil) and self.isOnline then
        begin
          val := self._advertisedValue;
          if not((val = '')) then
            begin
              self._invokeValueCallback(val);
            end;
        end;
      result := 0;
      exit;
    end;


  function TYRealTimeClock._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackRealTimeClock) <> nil) then
        begin
          self._valueCallbackRealTimeClock(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
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

//--- (YRealTimeClock functions)

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

//--- (end of YRealTimeClock functions)

initialization
  //--- (YRealTimeClock initialization)
  //--- (end of YRealTimeClock initialization)

finalization
  //--- (YRealTimeClock cleanup)
  _RealTimeClockCleanup();
  //--- (end of YRealTimeClock cleanup)
end.
