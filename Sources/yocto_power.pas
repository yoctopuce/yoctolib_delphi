{*********************************************************************
 *
 * $Id: yocto_power.pas 21551 2015-09-17 16:50:38Z seb $
 *
 * Implements yFindPower(), the high-level API for Power functions
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


unit yocto_power;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YPower definitions)

const Y_COSPHI_INVALID                = YAPI_INVALID_DOUBLE;
const Y_METER_INVALID                 = YAPI_INVALID_DOUBLE;
const Y_METERTIMER_INVALID            = YAPI_INVALID_UINT;


//--- (end of YPower definitions)

type
  TYPower = class;
  //--- (YPower class start)
  TYPowerValueCallback = procedure(func: TYPower; value:string);
  TYPowerTimedReportCallback = procedure(func: TYPower; value:TYMeasure);

  ////
  /// <summary>
  ///   TYPower Class: Power function interface
  /// <para>
  ///   The Yoctopuce class YPower allows you to read and configure Yoctopuce power
  ///   sensors. It inherits from YSensor class the core functions to read measurements,
  ///   register callback functions, access to the autonomous datalogger.
  ///   This class adds the ability to access the energy counter and the power factor.
  /// </para>
  /// </summary>
  ///-
  TYPower=class(TYSensor)
  //--- (end of YPower class start)
  protected
  //--- (YPower declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _unit                     : string;
    _currentValue             : double;
    _lowestValue              : double;
    _highestValue             : double;
    _currentRawValue          : double;
    _logFrequency             : string;
    _reportFrequency          : string;
    _calibrationParam         : string;
    _resolution               : double;
    _sensorState              : LongInt;
    _cosPhi                   : double;
    _meter                    : double;
    _meterTimer               : LongInt;
    _valueCallbackPower       : TYPowerValueCallback;
    _timedReportCallbackPower : TYPowerTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YPower declaration)

  public
    //--- (YPower accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the power factor (the ratio between the real power consumed,
    ///   measured in W, and the apparent power provided, measured in VA).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the power factor (the ratio between the real power consumed,
    ///   measured in W, and the apparent power provided, measured in VA)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_COSPHI_INVALID</c>.
    /// </para>
    ///-
    function get_cosPhi():double;

    function set_meter(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the energy counter, maintained by the wattmeter by integrating the power consumption over time.
    /// <para>
    ///   Note that this counter is reset at each start of the device.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the energy counter, maintained by the wattmeter by
    ///   integrating the power consumption over time
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_METER_INVALID</c>.
    /// </para>
    ///-
    function get_meter():double;

    ////
    /// <summary>
    ///   Returns the elapsed time since last energy counter reset, in seconds.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the elapsed time since last energy counter reset, in seconds
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_METERTIMER_INVALID</c>.
    /// </para>
    ///-
    function get_meterTimer():LongInt;

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
    ///   Use the method <c>YPower.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YPower</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindPower(func: string):TYPower;

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
    function registerValueCallback(callback: TYPowerValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every periodic timed notification.
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
    ///   arguments: the function object of which the value has changed, and an YMeasure object describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerTimedReportCallback(callback: TYPowerTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;

    ////
    /// <summary>
    ///   Resets the energy counter.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function reset():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of electrical power sensors started using <c>yFirstPower()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YPower</c> object, corresponding to
    ///   a electrical power sensor currently online, or a <c>null</c> pointer
    ///   if there are no more electrical power sensors to enumerate.
    /// </returns>
    ///-
    function nextPower():TYPower;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstPower():TYPower;
  //--- (end of YPower accessors declaration)
  end;

//--- (Power functions declaration)
  ////
  /// <summary>
  ///   Retrieves a electrical power sensor for a given identifier.
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
  ///   This function does not require that the electrical power sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YPower.isOnline()</c> to test if the electrical power sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a electrical power sensor by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the electrical power sensor
  /// </param>
  /// <returns>
  ///   a <c>YPower</c> object allowing you to drive the electrical power sensor.
  /// </returns>
  ///-
  function yFindPower(func:string):TYPower;
  ////
  /// <summary>
  ///   Starts the enumeration of electrical power sensors currently accessible.
  /// <para>
  ///   Use the method <c>YPower.nextPower()</c> to iterate on
  ///   next electrical power sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YPower</c> object, corresponding to
  ///   the first electrical power sensor currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstPower():TYPower;

//--- (end of Power functions declaration)

implementation
//--- (YPower dlldef)
//--- (end of YPower dlldef)

  constructor TYPower.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Power';
      //--- (YPower accessors initialization)
      _cosPhi := Y_COSPHI_INVALID;
      _meter := Y_METER_INVALID;
      _meterTimer := Y_METERTIMER_INVALID;
      _valueCallbackPower := nil;
      _timedReportCallbackPower := nil;
      //--- (end of YPower accessors initialization)
    end;


//--- (YPower implementation)
{$HINTS OFF}
  function TYPower._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'cosPhi') then
        begin
          _cosPhi := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'meter') then
        begin
          _meter := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'meterTimer') then
        begin
          _meterTimer := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Returns the power factor (the ratio between the real power consumed,
  ///   measured in W, and the apparent power provided, measured in VA).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the power factor (the ratio between the real power consumed,
  ///   measured in W, and the apparent power provided, measured in VA)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_COSPHI_INVALID.
  /// </para>
  ///-
  function TYPower.get_cosPhi():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_COSPHI_INVALID;
              exit;
            end;
        end;
      result := self._cosPhi;
      exit;
    end;


  function TYPower.set_meter(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('meter',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the energy counter, maintained by the wattmeter by integrating the power consumption over time.
  /// <para>
  ///   Note that this counter is reset at each start of the device.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the energy counter, maintained by the wattmeter by
  ///   integrating the power consumption over time
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_METER_INVALID.
  /// </para>
  ///-
  function TYPower.get_meter():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_METER_INVALID;
              exit;
            end;
        end;
      result := self._meter;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the elapsed time since last energy counter reset, in seconds.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the elapsed time since last energy counter reset, in seconds
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_METERTIMER_INVALID.
  /// </para>
  ///-
  function TYPower.get_meterTimer():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_METERTIMER_INVALID;
              exit;
            end;
        end;
      result := self._meterTimer;
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
  ///   Use the method <c>YPower.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YPower</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYPower.FindPower(func: string):TYPower;
    var
      obj : TYPower;
    begin
      obj := TYPower(TYFunction._FindFromCache('Power', func));
      if obj = nil then
        begin
          obj :=  TYPower.create(func);
          TYFunction._AddToCache('Power',  func, obj);
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
  function TYPower.registerValueCallback(callback: TYPowerValueCallback):LongInt;
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
      self._valueCallbackPower := callback;
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


  function TYPower._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackPower) <> nil) then
        begin
          self._valueCallbackPower(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Registers the callback function that is invoked on every periodic timed notification.
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
  ///   arguments: the function object of which the value has changed, and an YMeasure object describing
  ///   the new advertised value.
  /// @noreturn
  /// </param>
  ///-
  function TYPower.registerTimedReportCallback(callback: TYPowerTimedReportCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(self, true);
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(self, false);
        end;
      self._timedReportCallbackPower := callback;
      result := 0;
      exit;
    end;


  function TYPower._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackPower) <> nil) then
        begin
          self._timedReportCallbackPower(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Resets the energy counter.
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYPower.reset():LongInt;
    begin
      result := self.set_meter(0);
      exit;
    end;


  function TYPower.nextPower(): TYPower;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextPower := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextPower := nil;
          exit;
        end;
      nextPower := TYPower.FindPower(hwid);
    end;

  class function TYPower.FirstPower(): TYPower;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Power', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYPower.FindPower(serial+'.'+funcId);
    end;

//--- (end of YPower implementation)

//--- (Power functions)

  function yFindPower(func:string): TYPower;
    begin
      result := TYPower.FindPower(func);
    end;

  function yFirstPower(): TYPower;
    begin
      result := TYPower.FirstPower();
    end;

  procedure _PowerCleanup();
    begin
    end;

//--- (end of Power functions)

initialization
  //--- (Power initialization)
  //--- (end of Power initialization)

finalization
  //--- (Power cleanup)
  _PowerCleanup();
  //--- (end of Power cleanup)
end.
