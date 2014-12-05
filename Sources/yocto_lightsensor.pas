{*********************************************************************
 *
 * $Id: yocto_lightsensor.pas 18320 2014-11-10 10:47:48Z seb $
 *
 * Implements yFindLightSensor(), the high-level API for LightSensor functions
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


unit yocto_lightsensor;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YLightSensor definitions)

const Y_MEASURETYPE_HUMAN_EYE = 0;
const Y_MEASURETYPE_WIDE_SPECTRUM = 1;
const Y_MEASURETYPE_INFRARED = 2;
const Y_MEASURETYPE_HIGH_RATE = 3;
const Y_MEASURETYPE_HIGH_ENERGY = 4;
const Y_MEASURETYPE_INVALID = -1;


//--- (end of YLightSensor definitions)

type
  TYLightSensor = class;
  //--- (YLightSensor class start)
  TYLightSensorValueCallback = procedure(func: TYLightSensor; value:string);
  TYLightSensorTimedReportCallback = procedure(func: TYLightSensor; value:TYMeasure);

  ////
  /// <summary>
  ///   TYLightSensor Class: LightSensor function interface
  /// <para>
  ///   The Yoctopuce application programming interface allows you to read an instant
  ///   measure of the sensor, as well as the minimal and maximal values observed.
  /// </para>
  /// </summary>
  ///-
  TYLightSensor=class(TYSensor)
  //--- (end of YLightSensor class start)
  protected
  //--- (YLightSensor declaration)
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
    _measureType              : Integer;
    _valueCallbackLightSensor : TYLightSensorValueCallback;
    _timedReportCallbackLightSensor : TYLightSensorTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YLightSensor declaration)

  public
    //--- (YLightSensor accessors declaration)
    constructor Create(func:string);

    function set_currentValue(newval:double):integer;

    ////
    /// <summary>
    ///   Changes the sensor-specific calibration parameter so that the current value
    ///   matches a desired target (linear scaling).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="calibratedVal">
    ///   the desired target value.
    /// </param>
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function calibrate(calibratedVal: double):integer;

    ////
    /// <summary>
    ///   Returns the type of light measure.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_MEASURETYPE_HUMAN_EYE</c>, <c>Y_MEASURETYPE_WIDE_SPECTRUM</c>,
    ///   <c>Y_MEASURETYPE_INFRARED</c>, <c>Y_MEASURETYPE_HIGH_RATE</c> and <c>Y_MEASURETYPE_HIGH_ENERGY</c>
    ///   corresponding to the type of light measure
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MEASURETYPE_INVALID</c>.
    /// </para>
    ///-
    function get_measureType():Integer;

    ////
    /// <summary>
    ///   Modify the light sensor type used in the device.
    /// <para>
    ///   The measure can either
    ///   approximate the response of the human eye, focus on a specific light
    ///   spectrum, depending on the capabilities of the light-sensitive cell.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_MEASURETYPE_HUMAN_EYE</c>, <c>Y_MEASURETYPE_WIDE_SPECTRUM</c>,
    ///   <c>Y_MEASURETYPE_INFRARED</c>, <c>Y_MEASURETYPE_HIGH_RATE</c> and <c>Y_MEASURETYPE_HIGH_ENERGY</c>
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
    function set_measureType(newval:Integer):integer;

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
    ///   Use the method <c>YLightSensor.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YLightSensor</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindLightSensor(func: string):TYLightSensor;

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
    function registerValueCallback(callback: TYLightSensorValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYLightSensorTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of light sensors started using <c>yFirstLightSensor()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YLightSensor</c> object, corresponding to
    ///   a light sensor currently online, or a <c>null</c> pointer
    ///   if there are no more light sensors to enumerate.
    /// </returns>
    ///-
    function nextLightSensor():TYLightSensor;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstLightSensor():TYLightSensor;
  //--- (end of YLightSensor accessors declaration)
  end;

//--- (LightSensor functions declaration)
  ////
  /// <summary>
  ///   Retrieves a light sensor for a given identifier.
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
  ///   This function does not require that the light sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YLightSensor.isOnline()</c> to test if the light sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a light sensor by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the light sensor
  /// </param>
  /// <returns>
  ///   a <c>YLightSensor</c> object allowing you to drive the light sensor.
  /// </returns>
  ///-
  function yFindLightSensor(func:string):TYLightSensor;
  ////
  /// <summary>
  ///   Starts the enumeration of light sensors currently accessible.
  /// <para>
  ///   Use the method <c>YLightSensor.nextLightSensor()</c> to iterate on
  ///   next light sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YLightSensor</c> object, corresponding to
  ///   the first light sensor currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstLightSensor():TYLightSensor;

//--- (end of LightSensor functions declaration)

implementation
//--- (YLightSensor dlldef)
//--- (end of YLightSensor dlldef)

  constructor TYLightSensor.Create(func:string);
    begin
      inherited Create(func);
      _className := 'LightSensor';
      //--- (YLightSensor accessors initialization)
      _measureType := Y_MEASURETYPE_INVALID;
      _valueCallbackLightSensor := nil;
      _timedReportCallbackLightSensor := nil;
      //--- (end of YLightSensor accessors initialization)
    end;


//--- (YLightSensor implementation)
{$HINTS OFF}
  function TYLightSensor._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'measureType') then
        begin
          _measureType := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYLightSensor.set_currentValue(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('currentValue',rest_val);
    end;

  ////
  /// <summary>
  ///   Changes the sensor-specific calibration parameter so that the current value
  ///   matches a desired target (linear scaling).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="calibratedVal">
  ///   the desired target value.
  /// </param>
  /// <para>
  ///   Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// <returns>
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYLightSensor.calibrate(calibratedVal: double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(calibratedVal * 65536.0));
      result := _setAttr('currentValue', rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the type of light measure.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_MEASURETYPE_HUMAN_EYE, Y_MEASURETYPE_WIDE_SPECTRUM, Y_MEASURETYPE_INFRARED,
  ///   Y_MEASURETYPE_HIGH_RATE and Y_MEASURETYPE_HIGH_ENERGY corresponding to the type of light measure
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_MEASURETYPE_INVALID.
  /// </para>
  ///-
  function TYLightSensor.get_measureType():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MEASURETYPE_INVALID;
              exit
            end;
        end;
      result := self._measureType;
      exit;
    end;


  ////
  /// <summary>
  ///   Modify the light sensor type used in the device.
  /// <para>
  ///   The measure can either
  ///   approximate the response of the human eye, focus on a specific light
  ///   spectrum, depending on the capabilities of the light-sensitive cell.
  ///   Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_MEASURETYPE_HUMAN_EYE, Y_MEASURETYPE_WIDE_SPECTRUM, Y_MEASURETYPE_INFRARED,
  ///   Y_MEASURETYPE_HIGH_RATE and Y_MEASURETYPE_HIGH_ENERGY
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
  function TYLightSensor.set_measureType(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('measureType',rest_val);
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
  ///   Use the method <c>YLightSensor.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YLightSensor</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYLightSensor.FindLightSensor(func: string):TYLightSensor;
    var
      obj : TYLightSensor;
    begin
      obj := TYLightSensor(TYFunction._FindFromCache('LightSensor', func));
      if obj = nil then
        begin
          obj :=  TYLightSensor.create(func);
          TYFunction._AddToCache('LightSensor',  func, obj)
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
  function TYLightSensor.registerValueCallback(callback: TYLightSensorValueCallback):LongInt;
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
      self._valueCallbackLightSensor := callback;
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


  function TYLightSensor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackLightSensor) <> nil) then
        begin
          self._valueCallbackLightSensor(self, value)
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
  function TYLightSensor.registerTimedReportCallback(callback: TYLightSensorTimedReportCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(self, false)
        end;
      self._timedReportCallbackLightSensor := callback;
      result := 0;
      exit;
    end;


  function TYLightSensor._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackLightSensor) <> nil) then
        begin
          self._timedReportCallbackLightSensor(self, value)
        end
      else
        begin
          inherited _invokeTimedReportCallback(value)
        end;
      result := 0;
      exit;
    end;


  function TYLightSensor.nextLightSensor(): TYLightSensor;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextLightSensor := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextLightSensor := nil;
          exit;
        end;
      nextLightSensor := TYLightSensor.FindLightSensor(hwid);
    end;

  class function TYLightSensor.FirstLightSensor(): TYLightSensor;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('LightSensor', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYLightSensor.FindLightSensor(serial+'.'+funcId);
    end;

//--- (end of YLightSensor implementation)

//--- (LightSensor functions)

  function yFindLightSensor(func:string): TYLightSensor;
    begin
      result := TYLightSensor.FindLightSensor(func);
    end;

  function yFirstLightSensor(): TYLightSensor;
    begin
      result := TYLightSensor.FirstLightSensor();
    end;

  procedure _LightSensorCleanup();
    begin
    end;

//--- (end of LightSensor functions)

initialization
  //--- (LightSensor initialization)
  //--- (end of LightSensor initialization)

finalization
  //--- (LightSensor cleanup)
  _LightSensorCleanup();
  //--- (end of LightSensor cleanup)
end.
