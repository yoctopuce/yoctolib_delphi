{*********************************************************************
 *
 *  $Id: yocto_wakeupmonitor.pas 32610 2018-10-10 06:52:20Z seb $
 *
 *  Implements yFindWakeUpMonitor(), the high-level API for WakeUpMonitor functions
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


unit yocto_wakeupmonitor;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YWakeUpMonitor definitions)

const Y_POWERDURATION_INVALID         = YAPI_INVALID_INT;
const Y_SLEEPCOUNTDOWN_INVALID        = YAPI_INVALID_INT;
const Y_NEXTWAKEUP_INVALID            = YAPI_INVALID_LONG;
const Y_WAKEUPREASON_USBPOWER = 0;
const Y_WAKEUPREASON_EXTPOWER = 1;
const Y_WAKEUPREASON_ENDOFSLEEP = 2;
const Y_WAKEUPREASON_EXTSIG1 = 3;
const Y_WAKEUPREASON_SCHEDULE1 = 4;
const Y_WAKEUPREASON_SCHEDULE2 = 5;
const Y_WAKEUPREASON_INVALID = -1;
const Y_WAKEUPSTATE_SLEEPING = 0;
const Y_WAKEUPSTATE_AWAKE = 1;
const Y_WAKEUPSTATE_INVALID = -1;
const Y_RTCTIME_INVALID               = YAPI_INVALID_LONG;


//--- (end of YWakeUpMonitor definitions)
//--- (YWakeUpMonitor yapiwrapper declaration)
//--- (end of YWakeUpMonitor yapiwrapper declaration)

type
  TYWakeUpMonitor = class;
  //--- (YWakeUpMonitor class start)
  TYWakeUpMonitorValueCallback = procedure(func: TYWakeUpMonitor; value:string);
  TYWakeUpMonitorTimedReportCallback = procedure(func: TYWakeUpMonitor; value:TYMeasure);

  ////
  /// <summary>
  ///   TYWakeUpMonitor Class: WakeUpMonitor function interface
  /// <para>
  ///   The WakeUpMonitor function handles globally all wake-up sources, as well
  ///   as automated sleep mode.
  /// </para>
  /// </summary>
  ///-
  TYWakeUpMonitor=class(TYFunction)
  //--- (end of YWakeUpMonitor class start)
  protected
  //--- (YWakeUpMonitor declaration)
    // Attributes (function value cache)
    _powerDuration            : LongInt;
    _sleepCountdown           : LongInt;
    _nextWakeUp               : int64;
    _wakeUpReason             : Integer;
    _wakeUpState              : Integer;
    _rtcTime                  : int64;
    _endOfTime                : LongInt;
    _valueCallbackWakeUpMonitor : TYWakeUpMonitorValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YWakeUpMonitor declaration)

  public
    //--- (YWakeUpMonitor accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the maximal wake up time (in seconds) before automatically going to sleep.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximal wake up time (in seconds) before automatically going to sleep
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_POWERDURATION_INVALID</c>.
    /// </para>
    ///-
    function get_powerDuration():LongInt;

    ////
    /// <summary>
    ///   Changes the maximal wake up time (seconds) before automatically going to sleep.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the maximal wake up time (seconds) before automatically going to sleep
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
    ///   Returns the delay before the  next sleep period.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the delay before the  next sleep period
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SLEEPCOUNTDOWN_INVALID</c>.
    /// </para>
    ///-
    function get_sleepCountdown():LongInt;

    ////
    /// <summary>
    ///   Changes the delay before the next sleep period.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the delay before the next sleep period
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
    ///   Returns the next scheduled wake up date/time (UNIX format).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the next scheduled wake up date/time (UNIX format)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_NEXTWAKEUP_INVALID</c>.
    /// </para>
    ///-
    function get_nextWakeUp():int64;

    ////
    /// <summary>
    ///   Changes the days of the week when a wake up must take place.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the days of the week when a wake up must take place
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
    function set_nextWakeUp(newval:int64):integer;

    ////
    /// <summary>
    ///   Returns the latest wake up reason.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_WAKEUPREASON_USBPOWER</c>, <c>Y_WAKEUPREASON_EXTPOWER</c>,
    ///   <c>Y_WAKEUPREASON_ENDOFSLEEP</c>, <c>Y_WAKEUPREASON_EXTSIG1</c>, <c>Y_WAKEUPREASON_SCHEDULE1</c>
    ///   and <c>Y_WAKEUPREASON_SCHEDULE2</c> corresponding to the latest wake up reason
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_WAKEUPREASON_INVALID</c>.
    /// </para>
    ///-
    function get_wakeUpReason():Integer;

    ////
    /// <summary>
    ///   Returns  the current state of the monitor.
    /// <para>
    /// </para>
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

    function get_rtcTime():int64;

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
    ///   Use the method <c>YWakeUpMonitor.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a string that uniquely characterizes $THEFUNCTION$
    /// </param>
    /// <returns>
    ///   a <c>YWakeUpMonitor</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindWakeUpMonitor(func: string):TYWakeUpMonitor;

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
    function registerValueCallback(callback: TYWakeUpMonitorValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Forces a wake up.
    /// <para>
    /// </para>
    /// </summary>
    ///-
    function wakeUp():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Goes to sleep until the next wake up condition is met,  the
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
    function sleep(secBeforeSleep: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Goes to sleep for a specific duration or until the next wake up condition is met, the
    ///   RTC time must have been set before calling this function.
    /// <para>
    ///   The count down before sleep
    ///   can be canceled with resetSleepCountDown.
    /// </para>
    /// </summary>
    /// <param name="secUntilWakeUp">
    ///   number of seconds before next wake up
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
    function sleepFor(secUntilWakeUp: LongInt; secBeforeSleep: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Go to sleep until a specific date is reached or until the next wake up condition is met, the
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
    function sleepUntil(wakeUpTime: LongInt; secBeforeSleep: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Resets the sleep countdown.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function resetSleepCountDown():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of monitors started using <c>yFirstWakeUpMonitor()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YWakeUpMonitor</c> object, corresponding to
    ///   a monitor currently online, or a <c>NIL</c> pointer
    ///   if there are no more monitors to enumerate.
    /// </returns>
    ///-
    function nextWakeUpMonitor():TYWakeUpMonitor;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstWakeUpMonitor():TYWakeUpMonitor;
  //--- (end of YWakeUpMonitor accessors declaration)
  end;

//--- (YWakeUpMonitor functions declaration)
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
  /// <para>
  ///   If a call to this object's is_online() method returns FALSE although
  ///   you are certain that the matching device is plugged, make sure that you did
  ///   call registerHub() at application initialization time.
  /// </para>
  /// <para>
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
  ///   the first monitor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstWakeUpMonitor():TYWakeUpMonitor;

//--- (end of YWakeUpMonitor functions declaration)

implementation
//--- (YWakeUpMonitor dlldef)
//--- (end of YWakeUpMonitor dlldef)

  constructor TYWakeUpMonitor.Create(func:string);
    begin
      inherited Create(func);
      _className := 'WakeUpMonitor';
      //--- (YWakeUpMonitor accessors initialization)
      _powerDuration := Y_POWERDURATION_INVALID;
      _sleepCountdown := Y_SLEEPCOUNTDOWN_INVALID;
      _nextWakeUp := Y_NEXTWAKEUP_INVALID;
      _wakeUpReason := Y_WAKEUPREASON_INVALID;
      _wakeUpState := Y_WAKEUPSTATE_INVALID;
      _rtcTime := Y_RTCTIME_INVALID;
      _endOfTime := 2145960000;
      _valueCallbackWakeUpMonitor := nil;
      //--- (end of YWakeUpMonitor accessors initialization)
    end;

//--- (YWakeUpMonitor yapiwrapper)
//--- (end of YWakeUpMonitor yapiwrapper)

//--- (YWakeUpMonitor implementation)
{$HINTS OFF}
  function TYWakeUpMonitor._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'powerDuration') then
        begin
          _powerDuration := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'sleepCountdown') then
        begin
          _sleepCountdown := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'nextWakeUp') then
        begin
          _nextWakeUp := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'wakeUpReason') then
        begin
          _wakeUpReason := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'wakeUpState') then
        begin
          _wakeUpState := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'rtcTime') then
        begin
          _rtcTime := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYWakeUpMonitor.get_powerDuration():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_POWERDURATION_INVALID;
              exit;
            end;
        end;
      res := self._powerDuration;
      result := res;
      exit;
    end;


  function TYWakeUpMonitor.set_powerDuration(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('powerDuration',rest_val);
    end;

  function TYWakeUpMonitor.get_sleepCountdown():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SLEEPCOUNTDOWN_INVALID;
              exit;
            end;
        end;
      res := self._sleepCountdown;
      result := res;
      exit;
    end;


  function TYWakeUpMonitor.set_sleepCountdown(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('sleepCountdown',rest_val);
    end;

  function TYWakeUpMonitor.get_nextWakeUp():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NEXTWAKEUP_INVALID;
              exit;
            end;
        end;
      res := self._nextWakeUp;
      result := res;
      exit;
    end;


  function TYWakeUpMonitor.set_nextWakeUp(newval:int64):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('nextWakeUp',rest_val);
    end;

  function TYWakeUpMonitor.get_wakeUpReason():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_WAKEUPREASON_INVALID;
              exit;
            end;
        end;
      res := self._wakeUpReason;
      result := res;
      exit;
    end;


  function TYWakeUpMonitor.get_wakeUpState():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_WAKEUPSTATE_INVALID;
              exit;
            end;
        end;
      res := self._wakeUpState;
      result := res;
      exit;
    end;


  function TYWakeUpMonitor.set_wakeUpState(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('wakeUpState',rest_val);
    end;

  function TYWakeUpMonitor.get_rtcTime():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RTCTIME_INVALID;
              exit;
            end;
        end;
      res := self._rtcTime;
      result := res;
      exit;
    end;


  class function TYWakeUpMonitor.FindWakeUpMonitor(func: string):TYWakeUpMonitor;
    var
      obj : TYWakeUpMonitor;
    begin
      obj := TYWakeUpMonitor(TYFunction._FindFromCache('WakeUpMonitor', func));
      if obj = nil then
        begin
          obj :=  TYWakeUpMonitor.create(func);
          TYFunction._AddToCache('WakeUpMonitor',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYWakeUpMonitor.registerValueCallback(callback: TYWakeUpMonitorValueCallback):LongInt;
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
      self._valueCallbackWakeUpMonitor := callback;
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


  function TYWakeUpMonitor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackWakeUpMonitor) <> nil) then
        begin
          self._valueCallbackWakeUpMonitor(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYWakeUpMonitor.wakeUp():LongInt;
    begin
      result := self.set_wakeUpState(Y_WAKEUPSTATE_AWAKE);
      exit;
    end;


  function TYWakeUpMonitor.sleep(secBeforeSleep: LongInt):LongInt;
    var
      currTime : LongInt;
    begin
      currTime := integer(self.get_rtcTime);
      if not(currTime <> 0) then
        begin
          self._throw( YAPI_RTC_NOT_READY, 'RTC time not set');
          result:=YAPI_RTC_NOT_READY;
          exit;
        end;
      self.set_nextWakeUp(self._endOfTime);
      self.set_sleepCountdown(secBeforeSleep);
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYWakeUpMonitor.sleepFor(secUntilWakeUp: LongInt; secBeforeSleep: LongInt):LongInt;
    var
      currTime : LongInt;
    begin
      currTime := integer(self.get_rtcTime);
      if not(currTime <> 0) then
        begin
          self._throw( YAPI_RTC_NOT_READY, 'RTC time not set');
          result:=YAPI_RTC_NOT_READY;
          exit;
        end;
      self.set_nextWakeUp(currTime+secUntilWakeUp);
      self.set_sleepCountdown(secBeforeSleep);
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYWakeUpMonitor.sleepUntil(wakeUpTime: LongInt; secBeforeSleep: LongInt):LongInt;
    var
      currTime : LongInt;
    begin
      currTime := integer(self.get_rtcTime);
      if not(currTime <> 0) then
        begin
          self._throw( YAPI_RTC_NOT_READY, 'RTC time not set');
          result:=YAPI_RTC_NOT_READY;
          exit;
        end;
      self.set_nextWakeUp(wakeUpTime);
      self.set_sleepCountdown(secBeforeSleep);
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYWakeUpMonitor.resetSleepCountDown():LongInt;
    begin
      self.set_sleepCountdown(0);
      self.set_nextWakeUp(0);
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYWakeUpMonitor.nextWakeUpMonitor(): TYWakeUpMonitor;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextWakeUpMonitor := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextWakeUpMonitor := nil;
          exit;
        end;
      nextWakeUpMonitor := TYWakeUpMonitor.FindWakeUpMonitor(hwid);
    end;

  class function TYWakeUpMonitor.FirstWakeUpMonitor(): TYWakeUpMonitor;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('WakeUpMonitor', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYWakeUpMonitor.FindWakeUpMonitor(serial+'.'+funcId);
    end;

//--- (end of YWakeUpMonitor implementation)

//--- (YWakeUpMonitor functions)

  function yFindWakeUpMonitor(func:string): TYWakeUpMonitor;
    begin
      result := TYWakeUpMonitor.FindWakeUpMonitor(func);
    end;

  function yFirstWakeUpMonitor(): TYWakeUpMonitor;
    begin
      result := TYWakeUpMonitor.FirstWakeUpMonitor();
    end;

  procedure _WakeUpMonitorCleanup();
    begin
    end;

//--- (end of YWakeUpMonitor functions)

initialization
  //--- (YWakeUpMonitor initialization)
  //--- (end of YWakeUpMonitor initialization)

finalization
  //--- (YWakeUpMonitor cleanup)
  _WakeUpMonitorCleanup();
  //--- (end of YWakeUpMonitor cleanup)
end.
