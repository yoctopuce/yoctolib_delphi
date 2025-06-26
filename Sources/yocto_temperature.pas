{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindTemperature(), the high-level API for Temperature functions
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


unit yocto_temperature;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

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
const Y_SENSORTYPE_RES_OHM = 11;
const Y_SENSORTYPE_RES_NTC = 12;
const Y_SENSORTYPE_RES_LINEAR = 13;
const Y_SENSORTYPE_RES_INTERNAL = 14;
const Y_SENSORTYPE_IR = 15;
const Y_SENSORTYPE_RES_PT1000 = 16;
const Y_SENSORTYPE_CHANNEL_OFF = 17;
const Y_SENSORTYPE_INVALID = -1;
const Y_SIGNALVALUE_INVALID           = YAPI_INVALID_DOUBLE;
const Y_SIGNALUNIT_INVALID            = YAPI_INVALID_STRING;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;

//--- (end of YTemperature definitions)

//--- (YTemperature yapiwrapper declaration)
//--- (end of YTemperature yapiwrapper declaration)

type

  TYTemperature = class;
  //--- (YTemperature class start)
  TYTemperatureValueCallback = procedure(func: TYTemperature; value:string);
  TYTemperatureTimedReportCallback = procedure(func: TYTemperature; value:TYMeasure);

  ////
  /// <summary>
  ///   TYTemperature Class: temperature sensor control interface, available for instance in the
  ///   Yocto-Meteo-V2, the Yocto-PT100, the Yocto-Temperature or the Yocto-Thermocouple
  /// <para>
  ///   The <c>YTemperature</c> class allows you to read and configure Yoctopuce temperature sensors.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  ///   This class adds the ability to configure some specific parameters
  ///   for some sensors (connection type, temperature mapping table).
  /// </para>
  /// </summary>
  ///-
  TYTemperature=class(TYSensor)
  //--- (end of YTemperature class start)
  protected
  //--- (YTemperature declaration)
    // Attributes (function value cache)
    _sensorType               : Integer;
    _signalValue              : double;
    _signalUnit               : string;
    _command                  : string;
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
    ///   Changes the measuring unit for the measured temperature.
    /// <para>
    ///   That unit is a string.
    ///   If that strings end with the letter F all temperatures values will returned in
    ///   Fahrenheit degrees. If that String ends with the letter K all values will be
    ///   returned in Kelvin degrees. If that string ends with the letter C all values will be
    ///   returned in Celsius degrees.  If the string ends with any other character the
    ///   change will be ignored. Remember to call the
    ///   <c>saveToFlash()</c> method of the module if the modification must be kept.
    ///   WARNING: if a specific calibration is defined for the temperature function, a
    ///   unit system change will probably break it.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the measuring unit for the measured temperature
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
    function set_unit(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the temperature sensor type.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YTemperature.SENSORTYPE_DIGITAL</c>, <c>YTemperature.SENSORTYPE_TYPE_K</c>,
    ///   <c>YTemperature.SENSORTYPE_TYPE_E</c>, <c>YTemperature.SENSORTYPE_TYPE_J</c>,
    ///   <c>YTemperature.SENSORTYPE_TYPE_N</c>, <c>YTemperature.SENSORTYPE_TYPE_R</c>,
    ///   <c>YTemperature.SENSORTYPE_TYPE_S</c>, <c>YTemperature.SENSORTYPE_TYPE_T</c>,
    ///   <c>YTemperature.SENSORTYPE_PT100_4WIRES</c>, <c>YTemperature.SENSORTYPE_PT100_3WIRES</c>,
    ///   <c>YTemperature.SENSORTYPE_PT100_2WIRES</c>, <c>YTemperature.SENSORTYPE_RES_OHM</c>,
    ///   <c>YTemperature.SENSORTYPE_RES_NTC</c>, <c>YTemperature.SENSORTYPE_RES_LINEAR</c>,
    ///   <c>YTemperature.SENSORTYPE_RES_INTERNAL</c>, <c>YTemperature.SENSORTYPE_IR</c>,
    ///   <c>YTemperature.SENSORTYPE_RES_PT1000</c> and <c>YTemperature.SENSORTYPE_CHANNEL_OFF</c>
    ///   corresponding to the temperature sensor type
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YTemperature.SENSORTYPE_INVALID</c>.
    /// </para>
    ///-
    function get_sensorType():Integer;

    ////
    /// <summary>
    ///   Changes the temperature sensor type.
    /// <para>
    ///   This function is used
    ///   to define the type of thermocouple (K,E...) used with the device.
    ///   It has no effect if module is using a digital sensor or a thermistor.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YTemperature.SENSORTYPE_DIGITAL</c>, <c>YTemperature.SENSORTYPE_TYPE_K</c>,
    ///   <c>YTemperature.SENSORTYPE_TYPE_E</c>, <c>YTemperature.SENSORTYPE_TYPE_J</c>,
    ///   <c>YTemperature.SENSORTYPE_TYPE_N</c>, <c>YTemperature.SENSORTYPE_TYPE_R</c>,
    ///   <c>YTemperature.SENSORTYPE_TYPE_S</c>, <c>YTemperature.SENSORTYPE_TYPE_T</c>,
    ///   <c>YTemperature.SENSORTYPE_PT100_4WIRES</c>, <c>YTemperature.SENSORTYPE_PT100_3WIRES</c>,
    ///   <c>YTemperature.SENSORTYPE_PT100_2WIRES</c>, <c>YTemperature.SENSORTYPE_RES_OHM</c>,
    ///   <c>YTemperature.SENSORTYPE_RES_NTC</c>, <c>YTemperature.SENSORTYPE_RES_LINEAR</c>,
    ///   <c>YTemperature.SENSORTYPE_RES_INTERNAL</c>, <c>YTemperature.SENSORTYPE_IR</c>,
    ///   <c>YTemperature.SENSORTYPE_RES_PT1000</c> and <c>YTemperature.SENSORTYPE_CHANNEL_OFF</c>
    ///   corresponding to the temperature sensor type
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
    function set_sensorType(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the current value of the electrical signal measured by the sensor.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current value of the electrical signal measured by the sensor
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YTemperature.SIGNALVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_signalValue():double;

    ////
    /// <summary>
    ///   Returns the measuring unit of the electrical signal used by the sensor.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the measuring unit of the electrical signal used by the sensor
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YTemperature.SIGNALUNIT_INVALID</c>.
    /// </para>
    ///-
    function get_signalUnit():string;

    function get_command():string;

    function set_command(newval:string):integer;

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
    function registerValueCallback(callback: TYTemperatureValueCallback):LongInt; overload;

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
    ///   arguments: the function object of which the value has changed, and an <c>YMeasure</c> object describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerTimedReportCallback(callback: TYTemperatureTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;

    ////
    /// <summary>
    ///   Configures NTC thermistor parameters in order to properly compute the temperature from
    ///   the measured resistance.
    /// <para>
    ///   For increased precision, you can enter a complete mapping
    ///   table using set_thermistorResponseTable. This function can only be used with a
    ///   temperature sensor based on thermistors.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="res25">
    ///   thermistor resistance at 25 degrees Celsius
    /// </param>
    /// <param name="beta">
    ///   Beta value
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_ntcParameters(res25: double; beta: double):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Records a thermistor response table, in order to interpolate the temperature from
    ///   the measured resistance.
    /// <para>
    ///   This function can only be used with a temperature
    ///   sensor based on thermistors.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tempValues">
    ///   array of floating point numbers, corresponding to all
    ///   temperatures (in degrees Celsius) for which the resistance of the
    ///   thermistor is specified.
    /// </param>
    /// <param name="resValues">
    ///   array of floating point numbers, corresponding to the resistance
    ///   values (in Ohms) for each of the temperature included in the first
    ///   argument, index by index.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_thermistorResponseTable(tempValues: TDoubleArray; resValues: TDoubleArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Retrieves the thermistor response table previously configured using the
    ///   <c>set_thermistorResponseTable</c> function.
    /// <para>
    ///   This function can only be used with a
    ///   temperature sensor based on thermistors.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tempValues">
    ///   array of floating point numbers, that is filled by the function
    ///   with all temperatures (in degrees Celsius) for which the resistance
    ///   of the thermistor is specified.
    /// </param>
    /// <param name="resValues">
    ///   array of floating point numbers, that is filled by the function
    ///   with the value (in Ohms) for each of the temperature included in the
    ///   first argument, index by index.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function loadThermistorResponseTable(var tempValues: TDoubleArray; var resValues: TDoubleArray):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of temperature sensors started using <c>yFirstTemperature()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned temperature sensors order.
    ///   If you want to find a specific a temperature sensor, use <c>Temperature.findTemperature()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YTemperature</c> object, corresponding to
    ///   a temperature sensor currently online, or a <c>NIL</c> pointer
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

//--- (YTemperature functions declaration)
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
  /// <para>
  ///   If a call to this object's is_online() method returns FALSE although
  ///   you are certain that the matching device is plugged, make sure that you did
  ///   call registerHub() at application initialization time.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the temperature sensor, for instance
  ///   <c>METEOMK2.temperature</c>.
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
  ///   the first temperature sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstTemperature():TYTemperature;

//--- (end of YTemperature functions declaration)

implementation

//--- (YTemperature dlldef)
//--- (end of YTemperature dlldef)

  constructor TYTemperature.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Temperature';
      //--- (YTemperature accessors initialization)
      _sensorType := Y_SENSORTYPE_INVALID;
      _signalValue := Y_SIGNALVALUE_INVALID;
      _signalUnit := Y_SIGNALUNIT_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackTemperature := nil;
      _timedReportCallbackTemperature := nil;
      //--- (end of YTemperature accessors initialization)
    end;

//--- (YTemperature yapiwrapper)
//--- (end of YTemperature yapiwrapper)

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
      if (member^.name = 'signalValue') then
        begin
          _signalValue := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'signalUnit') then
        begin
          _signalUnit := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'command') then
        begin
          _command := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYTemperature.set_unit(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('unit',rest_val);
    end;

  function TYTemperature.get_sensorType():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SENSORTYPE_INVALID;
              exit;
            end;
        end;
      res := self._sensorType;
      result := res;
      exit;
    end;


  function TYTemperature.set_sensorType(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('sensorType',rest_val);
    end;

  function TYTemperature.get_signalValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNALVALUE_INVALID;
              exit;
            end;
        end;
      res := round(self._signalValue * 1000) / 1000;
      result := res;
      exit;
    end;


  function TYTemperature.get_signalUnit():string;
    var
      res : string;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNALUNIT_INVALID;
              exit;
            end;
        end;
      res := self._signalUnit;
      result := res;
      exit;
    end;


  function TYTemperature.get_command():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_COMMAND_INVALID;
              exit;
            end;
        end;
      res := self._command;
      result := res;
      exit;
    end;


  function TYTemperature.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYTemperature.FindTemperature(func: string):TYTemperature;
    var
      obj : TYTemperature;
    begin
      obj := TYTemperature(TYFunction._FindFromCache('Temperature', func));
      if obj = nil then
        begin
          obj :=  TYTemperature.create(func);
          TYFunction._AddToCache('Temperature', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYTemperature.registerValueCallback(callback: TYTemperatureValueCallback):LongInt;
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
      self._valueCallbackTemperature := callback;
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


  function TYTemperature._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackTemperature) <> nil) then
        begin
          self._valueCallbackTemperature(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYTemperature.registerTimedReportCallback(callback: TYTemperatureTimedReportCallback):LongInt;
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
      self._timedReportCallbackTemperature := callback;
      result := 0;
      exit;
    end;


  function TYTemperature._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackTemperature) <> nil) then
        begin
          self._timedReportCallbackTemperature(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYTemperature.set_ntcParameters(res25: double; beta: double):LongInt;
    var
      t0 : double;
      t1 : double;
      res100 : double;
      tempValues : TDoubleArray;
      resValues : TDoubleArray;
      tempValues_pos : LongInt;
      resValues_pos : LongInt;
    begin
      t0 := 25.0+273.15;
      t1 := 100.0+273.15;
      res100 := res25 * Exp(beta*(1.0/t1 - 1.0/t0));
      tempValues_pos := 0;
      SetLength(tempValues, 2);;
      resValues_pos := 0;
      SetLength(resValues, 2);;
      tempValues[tempValues_pos] := 25.0;
      inc(tempValues_pos);
      resValues[resValues_pos] := res25;
      inc(resValues_pos);
      tempValues[tempValues_pos] := 100.0;
      inc(tempValues_pos);
      resValues[resValues_pos] := res100;
      inc(resValues_pos);
      SetLength(tempValues, tempValues_pos);;
      SetLength(resValues, resValues_pos);;
      result := self.set_thermistorResponseTable(tempValues, resValues);
      exit;
    end;


  function TYTemperature.set_thermistorResponseTable(tempValues: TDoubleArray; resValues: TDoubleArray):LongInt;
    var
      siz : LongInt;
      res : LongInt;
      idx : LongInt;
      found : LongInt;
      prev : double;
      curr : double;
      currTemp : double;
      idxres : double;
    begin
      siz := length(tempValues);
      if not(siz >= 2) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'thermistor response table must have at least two points');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if not(siz = length(resValues)) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'table sizes mismatch');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;

      res := self.set_command('Z');
      if not(res=YAPI_SUCCESS) then
        begin
          self._throw(YAPI_IO_ERROR,'unable to reset thermistor parameters');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      // add records in growing resistance value
      found := 1;
      prev := 0.0;
      while found > 0 do
        begin
          found := 0;
          curr := 99999999.0;
          currTemp := -999999.0;
          idx := 0;
          while idx < siz do
            begin
              idxres := resValues[idx];
              if (idxres > prev) and(idxres < curr) then
                begin
                  curr := idxres;
                  currTemp := tempValues[idx];
                  found := 1;
                end;
              idx := idx + 1;
            end;
          if found > 0 then
            begin
              res := self.set_command('m'+inttostr(round(1000*curr))+':'+inttostr(round(1000*currTemp)));
              if not(res=YAPI_SUCCESS) then
                begin
                  self._throw(YAPI_IO_ERROR,'unable to reset thermistor parameters');
                  result:=YAPI_IO_ERROR;
                  exit;
                end;
              prev := curr;
            end;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYTemperature.loadThermistorResponseTable(var tempValues: TDoubleArray; var resValues: TDoubleArray):LongInt;
    var
      id : string;
      bin_json : TByteArray;
      paramlist : TTByteArrayArray;
      templist : TDoubleArray;
      siz : LongInt;
      idx : LongInt;
      temp : double;
      found : LongInt;
      prev : double;
      curr : double;
      currRes : double;
      templist_pos : LongInt;
      tempValues_pos : LongInt;
      resValues_pos : LongInt;
    begin
      SetLength(tempValues, 0);
      SetLength(resValues, 0);

      id := self.get_functionId;
      id := Copy(id, 11 + 1, Length(id) - 11);
      if (id = '') then
        begin
          id := '1';
        end;
      bin_json := self._download('extra.json?page='+id);
      paramlist := self._json_get_array(bin_json);
      // first convert all temperatures to float
      siz := ((length(paramlist)) shr 1);
      templist_pos := 0;
      SetLength(templist, siz);;
      idx := 0;
      while idx < siz do
        begin
          temp := _yapiStrToFloat(_ByteToString(paramlist[2*idx+1]))/1000.0;
          templist[templist_pos] := temp;
          inc(templist_pos);
          idx := idx + 1;
        end;
      // then add records in growing temperature value
      tempValues_pos := 0;
      SetLength(tempValues, siz);;
      resValues_pos := 0;
      SetLength(resValues, siz);;
      found := 1;
      prev := -999999.0;
      while found > 0 do
        begin
          found := 0;
          curr := 999999.0;
          currRes := -999999.0;
          idx := 0;
          while idx < siz do
            begin
              temp := templist[idx];
              if (temp > prev) and(temp < curr) then
                begin
                  curr := temp;
                  currRes := _yapiStrToFloat(_ByteToString(paramlist[2*idx]))/1000.0;
                  found := 1;
                end;
              idx := idx + 1;
            end;
          if found > 0 then
            begin
              tempValues[tempValues_pos] := curr;
              inc(tempValues_pos);
              resValues[resValues_pos] := currRes;
              inc(resValues_pos);
              prev := curr;
            end;
        end;
      SetLength(tempValues, tempValues_pos);;
      SetLength(resValues, resValues_pos);;
      result := YAPI_SUCCESS;
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

//--- (YTemperature functions)

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

//--- (end of YTemperature functions)

initialization
  //--- (YTemperature initialization)
  //--- (end of YTemperature initialization)

finalization
  //--- (YTemperature cleanup)
  _TemperatureCleanup();
  //--- (end of YTemperature cleanup)

end.
