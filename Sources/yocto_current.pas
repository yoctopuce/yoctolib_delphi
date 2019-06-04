{*********************************************************************
 *
 *  $Id: yocto_current.pas 35360 2019-05-09 09:02:29Z mvuilleu $
 *
 *  Implements yFindCurrent(), the high-level API for Current functions
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


unit yocto_current;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YCurrent definitions)

const Y_ENABLED_FALSE = 0;
const Y_ENABLED_TRUE = 1;
const Y_ENABLED_INVALID = -1;


//--- (end of YCurrent definitions)
//--- (YCurrent yapiwrapper declaration)
//--- (end of YCurrent yapiwrapper declaration)

type
  TYCurrent = class;
  //--- (YCurrent class start)
  TYCurrentValueCallback = procedure(func: TYCurrent; value:string);
  TYCurrentTimedReportCallback = procedure(func: TYCurrent; value:TYMeasure);

  ////
  /// <summary>
  ///   TYCurrent Class: Current function interface
  /// <para>
  ///   The Yoctopuce class YCurrent allows you to read and configure Yoctopuce current
  ///   sensors. It inherits from YSensor class the core functions to read measurements,
  ///   to register callback functions, to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYCurrent=class(TYSensor)
  //--- (end of YCurrent class start)
  protected
  //--- (YCurrent declaration)
    // Attributes (function value cache)
    _enabled                  : Integer;
    _valueCallbackCurrent     : TYCurrentValueCallback;
    _timedReportCallbackCurrent : TYCurrentTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YCurrent declaration)

  public
    //--- (YCurrent accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the activation state of this input.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_ENABLED_FALSE</c> or <c>Y_ENABLED_TRUE</c>, according to the activation state of this input
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ENABLED_INVALID</c>.
    /// </para>
    ///-
    function get_enabled():Integer;

    ////
    /// <summary>
    ///   Changes the activation state of this input.
    /// <para>
    ///   When an input is disabled,
    ///   its value is no more updated. On some devices, disabling an input can
    ///   improve the refresh rate of the other active inputs.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>Y_ENABLED_FALSE</c> or <c>Y_ENABLED_TRUE</c>, according to the activation state of this input
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
    function set_enabled(newval:Integer):integer;

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
    ///   Use the method <c>YCurrent.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YCurrent</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindCurrent(func: string):TYCurrent;

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
    function registerValueCallback(callback: TYCurrentValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every periodic timed notification.
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
    ///   arguments: the function object of which the value has changed, and an YMeasure object describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerTimedReportCallback(callback: TYCurrentTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of current sensors started using <c>yFirstCurrent()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned current sensors order.
    ///   If you want to find a specific a current sensor, use <c>Current.findCurrent()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YCurrent</c> object, corresponding to
    ///   a current sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more current sensors to enumerate.
    /// </returns>
    ///-
    function nextCurrent():TYCurrent;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstCurrent():TYCurrent;
  //--- (end of YCurrent accessors declaration)
  end;

//--- (YCurrent functions declaration)
  ////
  /// <summary>
  ///   Retrieves a current sensor for a given identifier.
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
  ///   This function does not require that the current sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YCurrent.isOnline()</c> to test if the current sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a current sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the current sensor
  /// </param>
  /// <returns>
  ///   a <c>YCurrent</c> object allowing you to drive the current sensor.
  /// </returns>
  ///-
  function yFindCurrent(func:string):TYCurrent;
  ////
  /// <summary>
  ///   Starts the enumeration of current sensors currently accessible.
  /// <para>
  ///   Use the method <c>YCurrent.nextCurrent()</c> to iterate on
  ///   next current sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YCurrent</c> object, corresponding to
  ///   the first current sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstCurrent():TYCurrent;

//--- (end of YCurrent functions declaration)

implementation
//--- (YCurrent dlldef)
//--- (end of YCurrent dlldef)

  constructor TYCurrent.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Current';
      //--- (YCurrent accessors initialization)
      _enabled := Y_ENABLED_INVALID;
      _valueCallbackCurrent := nil;
      _timedReportCallbackCurrent := nil;
      //--- (end of YCurrent accessors initialization)
    end;

//--- (YCurrent yapiwrapper)
//--- (end of YCurrent yapiwrapper)

//--- (YCurrent implementation)
{$HINTS OFF}
  function TYCurrent._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'enabled') then
        begin
          _enabled := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYCurrent.get_enabled():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ENABLED_INVALID;
              exit;
            end;
        end;
      res := self._enabled;
      result := res;
      exit;
    end;


  function TYCurrent.set_enabled(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('enabled',rest_val);
    end;

  class function TYCurrent.FindCurrent(func: string):TYCurrent;
    var
      obj : TYCurrent;
    begin
      obj := TYCurrent(TYFunction._FindFromCache('Current', func));
      if obj = nil then
        begin
          obj :=  TYCurrent.create(func);
          TYFunction._AddToCache('Current',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYCurrent.registerValueCallback(callback: TYCurrentValueCallback):LongInt;
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
      self._valueCallbackCurrent := callback;
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


  function TYCurrent._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackCurrent) <> nil) then
        begin
          self._valueCallbackCurrent(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYCurrent.registerTimedReportCallback(callback: TYCurrentTimedReportCallback):LongInt;
    var
      sensor : TYSensor;
    begin
      sensor := self;
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(sensor, true);
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(sensor, false);
        end;
      self._timedReportCallbackCurrent := callback;
      result := 0;
      exit;
    end;


  function TYCurrent._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackCurrent) <> nil) then
        begin
          self._timedReportCallbackCurrent(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYCurrent.nextCurrent(): TYCurrent;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextCurrent := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextCurrent := nil;
          exit;
        end;
      nextCurrent := TYCurrent.FindCurrent(hwid);
    end;

  class function TYCurrent.FirstCurrent(): TYCurrent;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Current', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYCurrent.FindCurrent(serial+'.'+funcId);
    end;

//--- (end of YCurrent implementation)

//--- (YCurrent functions)

  function yFindCurrent(func:string): TYCurrent;
    begin
      result := TYCurrent.FindCurrent(func);
    end;

  function yFirstCurrent(): TYCurrent;
    begin
      result := TYCurrent.FirstCurrent();
    end;

  procedure _CurrentCleanup();
    begin
    end;

//--- (end of YCurrent functions)

initialization
  //--- (YCurrent initialization)
  //--- (end of YCurrent initialization)

finalization
  //--- (YCurrent cleanup)
  _CurrentCleanup();
  //--- (end of YCurrent cleanup)
end.
