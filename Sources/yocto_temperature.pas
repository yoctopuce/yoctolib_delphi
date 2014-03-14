{*********************************************************************
 *
 * $Id: yocto_temperature.pas 15254 2014-03-06 10:16:24Z seb $
 *
 * Implements yFindTemperature(), the high-level API for Temperature functions
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


unit yocto_temperature;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YTemperature definitions)

const Y_SENSORTYPE_DIGITAL = 0;
const Y_SENSORTYPE_TYPE_K = 1;
const Y_SENSORTYPE_TYPE_E = 2;
const Y_SENSORTYPE_TYPE_J = 3;
const Y_SENSORTYPE_TYPE_N = 4;
const Y_SENSORTYPE_TYPE_R = 5;
const Y_SENSORTYPE_TYPE_S = 6;
const Y_SENSORTYPE_TYPE_T = 7;
const Y_SENSORTYPE_PT100_4WIRES = 8;
const Y_SENSORTYPE_PT100_3WIRES = 9;
const Y_SENSORTYPE_PT100_2WIRES = 10;
const Y_SENSORTYPE_INVALID = -1;



//--- (end of YTemperature definitions)

type
  TYTemperature = class;
  //--- (YTemperature class start)
  TYTemperatureValueCallback = procedure(func: TYTemperature; value:string);
  TYTemperatureTimedReportCallback = procedure(func: TYTemperature; value:TYMeasure);

  ////
  /// <summary>
  ///   TYTemperature Class: Temperature function interface
  /// <para>
  ///   The Yoctopuce application programming interface allows you to read an instant
  ///   measure of the sensor, as well as the minimal and maximal values observed.
  /// </para>
  /// </summary>
  ///-
  TYTemperature=class(TYSensor)
  //--- (end of YTemperature class start)
  protected
  //--- (YTemperature declaration)
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
    _sensorType               : Integer;
    _valueCallbackTemperature : TYTemperatureValueCallback;
    _timedReportCallbackTemperature : TYTemperatureTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YTemperature declaration)

  public
    //--- (YTemperature accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the temperature sensor type.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_SENSORTYPE_DIGITAL</c>, <c>Y_SENSORTYPE_TYPE_K</c>, <c>Y_SENSORTYPE_TYPE_E</c>,
    ///   <c>Y_SENSORTYPE_TYPE_J</c>, <c>Y_SENSORTYPE_TYPE_N</c>, <c>Y_SENSORTYPE_TYPE_R</c>,
    ///   <c>Y_SENSORTYPE_TYPE_S</c>, <c>Y_SENSORTYPE_TYPE_T</c>, <c>Y_SENSORTYPE_PT100_4WIRES</c>,
    ///   <c>Y_SENSORTYPE_PT100_3WIRES</c> and <c>Y_SENSORTYPE_PT100_2WIRES</c> corresponding to the
    ///   temperature sensor type
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SENSORTYPE_INVALID</c>.
    /// </para>
    ///-
    function get_sensorType():Integer;

    ////
    /// <summary>
    ///   Modify the temperature sensor type.
    /// <para>
    ///   This function is used to
    ///   to define the type of thermocouple (K,E...) used with the device.
    ///   This will have no effect if module is using a digital sensor.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_SENSORTYPE_DIGITAL</c>, <c>Y_SENSORTYPE_TYPE_K</c>, <c>Y_SENSORTYPE_TYPE_E</c>,
    ///   <c>Y_SENSORTYPE_TYPE_J</c>, <c>Y_SENSORTYPE_TYPE_N</c>, <c>Y_SENSORTYPE_TYPE_R</c>,
    ///   <c>Y_SENSORTYPE_TYPE_S</c>, <c>Y_SENSORTYPE_TYPE_T</c>, <c>Y_SENSORTYPE_PT100_4WIRES</c>,
    ///   <c>Y_SENSORTYPE_PT100_3WIRES</c> and <c>Y_SENSORTYPE_PT100_2WIRES</c>
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
    function set_sensorType(newval:Integer):integer;

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
    ///   Use the method <c>YTemperature.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YTemperature</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindTemperature(func: string):TYTemperature;

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
    function registerValueCallback(callback: TYTemperatureValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYTemperatureTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of temperature sensors started using <c>yFirstTemperature()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YTemperature</c> object, corresponding to
    ///   a temperature sensor currently online, or a <c>null</c> pointer
    ///   if there are no more temperature sensors to enumerate.
    /// </returns>
    ///-
    function nextTemperature():TYTemperature;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstTemperature():TYTemperature;
  //--- (end of YTemperature accessors declaration)
  end;

//--- (Temperature functions declaration)

  ////
  /// <summary>
  ///   Retrieves a temperature sensor for a given identifier.
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
  ///   This function does not require that the temperature sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YTemperature.isOnline()</c> to test if the temperature sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a temperature sensor by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the temperature sensor
  /// </param>
  /// <returns>
  ///   a <c>YTemperature</c> object allowing you to drive the temperature sensor.
  /// </returns>
  ///-
  function yFindTemperature(func:string):TYTemperature;
  ////
  /// <summary>
  ///   Starts the enumeration of temperature sensors currently accessible.
  /// <para>
  ///   Use the method <c>YTemperature.nextTemperature()</c> to iterate on
  ///   next temperature sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YTemperature</c> object, corresponding to
  ///   the first temperature sensor currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstTemperature():TYTemperature;

//--- (end of Temperature functions declaration)

implementation

  constructor TYTemperature.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Temperature';
      //--- (YTemperature accessors initialization)
      _sensorType := Y_SENSORTYPE_INVALID;
      _valueCallbackTemperature := nil;
      _timedReportCallbackTemperature := nil;
      //--- (end of YTemperature accessors initialization)
    end;


//--- (YTemperature implementation)
{$HINTS OFF}
  function TYTemperature._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'sensorType') then
        begin
          _sensorType := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Returns the temperature sensor type.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_SENSORTYPE_DIGITAL, Y_SENSORTYPE_TYPE_K, Y_SENSORTYPE_TYPE_E, Y_SENSORTYPE_TYPE_J,
  ///   Y_SENSORTYPE_TYPE_N, Y_SENSORTYPE_TYPE_R, Y_SENSORTYPE_TYPE_S, Y_SENSORTYPE_TYPE_T,
  ///   Y_SENSORTYPE_PT100_4WIRES, Y_SENSORTYPE_PT100_3WIRES and Y_SENSORTYPE_PT100_2WIRES corresponding to
  ///   the temperature sensor type
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_SENSORTYPE_INVALID.
  /// </para>
  ///-
  function TYTemperature.get_sensorType():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SENSORTYPE_INVALID;
              exit
            end;
        end;
      result := self._sensorType;
      exit;
    end;


  ////
  /// <summary>
  ///   Modify the temperature sensor type.
  /// <para>
  ///   This function is used to
  ///   to define the type of thermocouple (K,E...) used with the device.
  ///   This will have no effect if module is using a digital sensor.
  ///   Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_SENSORTYPE_DIGITAL, Y_SENSORTYPE_TYPE_K, Y_SENSORTYPE_TYPE_E, Y_SENSORTYPE_TYPE_J,
  ///   Y_SENSORTYPE_TYPE_N, Y_SENSORTYPE_TYPE_R, Y_SENSORTYPE_TYPE_S, Y_SENSORTYPE_TYPE_T,
  ///   Y_SENSORTYPE_PT100_4WIRES, Y_SENSORTYPE_PT100_3WIRES and Y_SENSORTYPE_PT100_2WIRES
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
  function TYTemperature.set_sensorType(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('sensorType',rest_val);
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
  ///   Use the method <c>YTemperature.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YTemperature</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYTemperature.FindTemperature(func: string):TYTemperature;
    var
      obj : TYTemperature;
    begin
      obj := TYTemperature(TYFunction._FindFromCache('Temperature', func));
      if obj = nil then
        begin
          obj :=  TYTemperature.create(func);
          TYFunction._AddToCache('Temperature',  func, obj)
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
  function TYTemperature.registerValueCallback(callback: TYTemperatureValueCallback):LongInt;
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
      self._valueCallbackTemperature := callback;
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


  function TYTemperature._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackTemperature) <> nil) then
        begin
          self._valueCallbackTemperature(self, value)
        end
      else
        begin
          inherited _invokeValueCallback(value)
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
  function TYTemperature.registerTimedReportCallback(callback: TYTemperatureTimedReportCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(self, false)
        end;
      self._timedReportCallbackTemperature := callback;
      result := 0;
      exit;
    end;


  function TYTemperature._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackTemperature) <> nil) then
        begin
          self._timedReportCallbackTemperature(self, value)
        end
      else
        begin
          inherited _invokeTimedReportCallback(value)
        end;
      result := 0;
      exit;
    end;


  function TYTemperature.nextTemperature(): TYTemperature;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextTemperature := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextTemperature := nil;
          exit;
        end;
      nextTemperature := TYTemperature.FindTemperature(hwid);
    end;

  class function TYTemperature.FirstTemperature(): TYTemperature;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Temperature', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYTemperature.FindTemperature(serial+'.'+funcId);
    end;

//--- (end of YTemperature implementation)

//--- (Temperature functions)

  function yFindTemperature(func:string): TYTemperature;
    begin
      result := TYTemperature.FindTemperature(func);
    end;

  function yFirstTemperature(): TYTemperature;
    begin
      result := TYTemperature.FirstTemperature();
    end;

  procedure _TemperatureCleanup();
    begin
    end;

//--- (end of Temperature functions)

initialization
  //--- (Temperature initialization)
  //--- (end of Temperature initialization)

finalization
  //--- (Temperature cleanup)
  _TemperatureCleanup();
  //--- (end of Temperature cleanup)
end.
