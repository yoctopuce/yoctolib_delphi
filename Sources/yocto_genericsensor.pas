{*********************************************************************
 *
 * $Id: yocto_genericsensor.pas 19581 2015-03-04 10:57:44Z seb $
 *
 * Implements yFindGenericSensor(), the high-level API for GenericSensor functions
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


unit yocto_genericsensor;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YGenericSensor definitions)

const Y_SIGNALVALUE_INVALID           = YAPI_INVALID_DOUBLE;
const Y_SIGNALUNIT_INVALID            = YAPI_INVALID_STRING;
const Y_SIGNALRANGE_INVALID           = YAPI_INVALID_STRING;
const Y_VALUERANGE_INVALID            = YAPI_INVALID_STRING;
const Y_SIGNALBIAS_INVALID            = YAPI_INVALID_DOUBLE;
const Y_SIGNALSAMPLING_HIGH_RATE = 0;
const Y_SIGNALSAMPLING_HIGH_RATE_FILTERED = 1;
const Y_SIGNALSAMPLING_LOW_NOISE = 2;
const Y_SIGNALSAMPLING_LOW_NOISE_FILTERED = 3;
const Y_SIGNALSAMPLING_INVALID = -1;


//--- (end of YGenericSensor definitions)

type
  TYGenericSensor = class;
  //--- (YGenericSensor class start)
  TYGenericSensorValueCallback = procedure(func: TYGenericSensor; value:string);
  TYGenericSensorTimedReportCallback = procedure(func: TYGenericSensor; value:TYMeasure);

  ////
  /// <summary>
  ///   TYGenericSensor Class: GenericSensor function interface
  /// <para>
  ///   The YGenericSensor class allows you to read and configure Yoctopuce signal
  ///   transducers. It inherits from YSensor class the core functions to read measurements,
  ///   register callback functions, access to the autonomous datalogger.
  ///   This class adds the ability to configure the automatic conversion between the
  ///   measured signal and the corresponding engineering unit.
  /// </para>
  /// </summary>
  ///-
  TYGenericSensor=class(TYSensor)
  //--- (end of YGenericSensor class start)
  protected
  //--- (YGenericSensor declaration)
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
    _signalValue              : double;
    _signalUnit               : string;
    _signalRange              : string;
    _valueRange               : string;
    _signalBias               : double;
    _signalSampling           : Integer;
    _valueCallbackGenericSensor : TYGenericSensorValueCallback;
    _timedReportCallbackGenericSensor : TYGenericSensorTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YGenericSensor declaration)

  public
    //--- (YGenericSensor accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the measuring unit for the measured value.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the measuring unit for the measured value
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
    function set_unit(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the measured value of the electrical signal used by the sensor.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the measured value of the electrical signal used by the sensor
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SIGNALVALUE_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_SIGNALUNIT_INVALID</c>.
    /// </para>
    ///-
    function get_signalUnit():string;

    ////
    /// <summary>
    ///   Returns the electric signal range used by the sensor.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the electric signal range used by the sensor
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SIGNALRANGE_INVALID</c>.
    /// </para>
    ///-
    function get_signalRange():string;

    ////
    /// <summary>
    ///   Changes the electric signal range used by the sensor.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the electric signal range used by the sensor
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
    function set_signalRange(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the physical value range measured by the sensor.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the physical value range measured by the sensor
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_VALUERANGE_INVALID</c>.
    /// </para>
    ///-
    function get_valueRange():string;

    ////
    /// <summary>
    ///   Changes the physical value range measured by the sensor.
    /// <para>
    ///   As a side effect, the range modification may
    ///   automatically modify the display resolution.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the physical value range measured by the sensor
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
    function set_valueRange(newval:string):integer;

    ////
    /// <summary>
    ///   Changes the electric signal bias for zero shift adjustment.
    /// <para>
    ///   If your electric signal reads positif when it should be zero, setup
    ///   a positive signalBias of the same value to fix the zero shift.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the electric signal bias for zero shift adjustment
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
    function set_signalBias(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the electric signal bias for zero shift adjustment.
    /// <para>
    ///   A positive bias means that the signal is over-reporting the measure,
    ///   while a negative bias means that the signal is underreporting the measure.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the electric signal bias for zero shift adjustment
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SIGNALBIAS_INVALID</c>.
    /// </para>
    ///-
    function get_signalBias():double;

    ////
    /// <summary>
    ///   Returns the electric signal sampling method to use.
    /// <para>
    ///   The <c>HIGH_RATE</c> method uses the highest sampling frequency, without any filtering.
    ///   The <c>HIGH_RATE_FILTERED</c> method adds a windowed 7-sample median filter.
    ///   The <c>LOW_NOISE</c> method uses a reduced acquisition frequency to reduce noise.
    ///   The <c>LOW_NOISE_FILTERED</c> method combines a reduced frequency with the median filter
    ///   to get measures as stable as possible when working on a noisy signal.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_SIGNALSAMPLING_HIGH_RATE</c>, <c>Y_SIGNALSAMPLING_HIGH_RATE_FILTERED</c>,
    ///   <c>Y_SIGNALSAMPLING_LOW_NOISE</c> and <c>Y_SIGNALSAMPLING_LOW_NOISE_FILTERED</c> corresponding to
    ///   the electric signal sampling method to use
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SIGNALSAMPLING_INVALID</c>.
    /// </para>
    ///-
    function get_signalSampling():Integer;

    ////
    /// <summary>
    ///   Changes the electric signal sampling method to use.
    /// <para>
    ///   The <c>HIGH_RATE</c> method uses the highest sampling frequency, without any filtering.
    ///   The <c>HIGH_RATE_FILTERED</c> method adds a windowed 7-sample median filter.
    ///   The <c>LOW_NOISE</c> method uses a reduced acquisition frequency to reduce noise.
    ///   The <c>LOW_NOISE_FILTERED</c> method combines a reduced frequency with the median filter
    ///   to get measures as stable as possible when working on a noisy signal.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_SIGNALSAMPLING_HIGH_RATE</c>, <c>Y_SIGNALSAMPLING_HIGH_RATE_FILTERED</c>,
    ///   <c>Y_SIGNALSAMPLING_LOW_NOISE</c> and <c>Y_SIGNALSAMPLING_LOW_NOISE_FILTERED</c> corresponding to
    ///   the electric signal sampling method to use
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
    function set_signalSampling(newval:Integer):integer;

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
    ///   Use the method <c>YGenericSensor.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YGenericSensor</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindGenericSensor(func: string):TYGenericSensor;

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
    function registerValueCallback(callback: TYGenericSensorValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYGenericSensorTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;

    ////
    /// <summary>
    ///   Adjusts the signal bias so that the current signal value is need
    ///   precisely as zero.
    /// <para>
    /// </para>
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
    function zeroAdjust():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of generic sensors started using <c>yFirstGenericSensor()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YGenericSensor</c> object, corresponding to
    ///   a generic sensor currently online, or a <c>null</c> pointer
    ///   if there are no more generic sensors to enumerate.
    /// </returns>
    ///-
    function nextGenericSensor():TYGenericSensor;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstGenericSensor():TYGenericSensor;
  //--- (end of YGenericSensor accessors declaration)
  end;

//--- (GenericSensor functions declaration)
  ////
  /// <summary>
  ///   Retrieves a generic sensor for a given identifier.
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
  ///   This function does not require that the generic sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YGenericSensor.isOnline()</c> to test if the generic sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a generic sensor by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the generic sensor
  /// </param>
  /// <returns>
  ///   a <c>YGenericSensor</c> object allowing you to drive the generic sensor.
  /// </returns>
  ///-
  function yFindGenericSensor(func:string):TYGenericSensor;
  ////
  /// <summary>
  ///   Starts the enumeration of generic sensors currently accessible.
  /// <para>
  ///   Use the method <c>YGenericSensor.nextGenericSensor()</c> to iterate on
  ///   next generic sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YGenericSensor</c> object, corresponding to
  ///   the first generic sensor currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstGenericSensor():TYGenericSensor;

//--- (end of GenericSensor functions declaration)

implementation
//--- (YGenericSensor dlldef)
//--- (end of YGenericSensor dlldef)

  constructor TYGenericSensor.Create(func:string);
    begin
      inherited Create(func);
      _className := 'GenericSensor';
      //--- (YGenericSensor accessors initialization)
      _signalValue := Y_SIGNALVALUE_INVALID;
      _signalUnit := Y_SIGNALUNIT_INVALID;
      _signalRange := Y_SIGNALRANGE_INVALID;
      _valueRange := Y_VALUERANGE_INVALID;
      _signalBias := Y_SIGNALBIAS_INVALID;
      _signalSampling := Y_SIGNALSAMPLING_INVALID;
      _valueCallbackGenericSensor := nil;
      _timedReportCallbackGenericSensor := nil;
      //--- (end of YGenericSensor accessors initialization)
    end;


//--- (YGenericSensor implementation)
{$HINTS OFF}
  function TYGenericSensor._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'signalValue') then
        begin
          _signalValue := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'signalUnit') then
        begin
          _signalUnit := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'signalRange') then
        begin
          _signalRange := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'valueRange') then
        begin
          _valueRange := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'signalBias') then
        begin
          _signalBias := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'signalSampling') then
        begin
          _signalSampling := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Changes the measuring unit for the measured value.
  /// <para>
  ///   Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the measuring unit for the measured value
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
  function TYGenericSensor.set_unit(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('unit',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the measured value of the electrical signal used by the sensor.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the measured value of the electrical signal used by the sensor
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_SIGNALVALUE_INVALID.
  /// </para>
  ///-
  function TYGenericSensor.get_signalValue():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNALVALUE_INVALID;
              exit
            end;
        end;
      result := round(self._signalValue * 1000) / 1000;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_SIGNALUNIT_INVALID.
  /// </para>
  ///-
  function TYGenericSensor.get_signalUnit():string;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNALUNIT_INVALID;
              exit
            end;
        end;
      result := self._signalUnit;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the electric signal range used by the sensor.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the electric signal range used by the sensor
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_SIGNALRANGE_INVALID.
  /// </para>
  ///-
  function TYGenericSensor.get_signalRange():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNALRANGE_INVALID;
              exit
            end;
        end;
      result := self._signalRange;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the electric signal range used by the sensor.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the electric signal range used by the sensor
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
  function TYGenericSensor.set_signalRange(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('signalRange',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the physical value range measured by the sensor.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the physical value range measured by the sensor
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_VALUERANGE_INVALID.
  /// </para>
  ///-
  function TYGenericSensor.get_valueRange():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_VALUERANGE_INVALID;
              exit
            end;
        end;
      result := self._valueRange;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the physical value range measured by the sensor.
  /// <para>
  ///   As a side effect, the range modification may
  ///   automatically modify the display resolution.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the physical value range measured by the sensor
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
  function TYGenericSensor.set_valueRange(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('valueRange',rest_val);
    end;

  ////
  /// <summary>
  ///   Changes the electric signal bias for zero shift adjustment.
  /// <para>
  ///   If your electric signal reads positif when it should be zero, setup
  ///   a positive signalBias of the same value to fix the zero shift.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a floating point number corresponding to the electric signal bias for zero shift adjustment
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
  function TYGenericSensor.set_signalBias(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('signalBias',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the electric signal bias for zero shift adjustment.
  /// <para>
  ///   A positive bias means that the signal is over-reporting the measure,
  ///   while a negative bias means that the signal is underreporting the measure.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the electric signal bias for zero shift adjustment
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_SIGNALBIAS_INVALID.
  /// </para>
  ///-
  function TYGenericSensor.get_signalBias():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNALBIAS_INVALID;
              exit
            end;
        end;
      result := self._signalBias;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the electric signal sampling method to use.
  /// <para>
  ///   The HIGH_RATE method uses the highest sampling frequency, without any filtering.
  ///   The HIGH_RATE_FILTERED method adds a windowed 7-sample median filter.
  ///   The LOW_NOISE method uses a reduced acquisition frequency to reduce noise.
  ///   The LOW_NOISE_FILTERED method combines a reduced frequency with the median filter
  ///   to get measures as stable as possible when working on a noisy signal.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_SIGNALSAMPLING_HIGH_RATE, Y_SIGNALSAMPLING_HIGH_RATE_FILTERED,
  ///   Y_SIGNALSAMPLING_LOW_NOISE and Y_SIGNALSAMPLING_LOW_NOISE_FILTERED corresponding to the electric
  ///   signal sampling method to use
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_SIGNALSAMPLING_INVALID.
  /// </para>
  ///-
  function TYGenericSensor.get_signalSampling():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNALSAMPLING_INVALID;
              exit
            end;
        end;
      result := self._signalSampling;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the electric signal sampling method to use.
  /// <para>
  ///   The HIGH_RATE method uses the highest sampling frequency, without any filtering.
  ///   The HIGH_RATE_FILTERED method adds a windowed 7-sample median filter.
  ///   The LOW_NOISE method uses a reduced acquisition frequency to reduce noise.
  ///   The LOW_NOISE_FILTERED method combines a reduced frequency with the median filter
  ///   to get measures as stable as possible when working on a noisy signal.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_SIGNALSAMPLING_HIGH_RATE, Y_SIGNALSAMPLING_HIGH_RATE_FILTERED,
  ///   Y_SIGNALSAMPLING_LOW_NOISE and Y_SIGNALSAMPLING_LOW_NOISE_FILTERED corresponding to the electric
  ///   signal sampling method to use
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
  function TYGenericSensor.set_signalSampling(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('signalSampling',rest_val);
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
  ///   Use the method <c>YGenericSensor.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YGenericSensor</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYGenericSensor.FindGenericSensor(func: string):TYGenericSensor;
    var
      obj : TYGenericSensor;
    begin
      obj := TYGenericSensor(TYFunction._FindFromCache('GenericSensor', func));
      if obj = nil then
        begin
          obj :=  TYGenericSensor.create(func);
          TYFunction._AddToCache('GenericSensor',  func, obj)
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
  function TYGenericSensor.registerValueCallback(callback: TYGenericSensorValueCallback):LongInt;
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
      self._valueCallbackGenericSensor := callback;
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


  function TYGenericSensor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackGenericSensor) <> nil) then
        begin
          self._valueCallbackGenericSensor(self, value)
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
  function TYGenericSensor.registerTimedReportCallback(callback: TYGenericSensorTimedReportCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(self, false)
        end;
      self._timedReportCallbackGenericSensor := callback;
      result := 0;
      exit;
    end;


  function TYGenericSensor._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackGenericSensor) <> nil) then
        begin
          self._timedReportCallbackGenericSensor(self, value)
        end
      else
        begin
          inherited _invokeTimedReportCallback(value)
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Adjusts the signal bias so that the current signal value is need
  ///   precisely as zero.
  /// <para>
  /// </para>
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
  function TYGenericSensor.zeroAdjust():LongInt;
    var
      currSignal : double;
      currBias : double;
    begin
      currSignal := self.get_signalValue;
      currBias := self.get_signalBias;
      result := self.set_signalBias(currSignal + currBias);
      exit;
    end;


  function TYGenericSensor.nextGenericSensor(): TYGenericSensor;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextGenericSensor := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextGenericSensor := nil;
          exit;
        end;
      nextGenericSensor := TYGenericSensor.FindGenericSensor(hwid);
    end;

  class function TYGenericSensor.FirstGenericSensor(): TYGenericSensor;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('GenericSensor', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYGenericSensor.FindGenericSensor(serial+'.'+funcId);
    end;

//--- (end of YGenericSensor implementation)

//--- (GenericSensor functions)

  function yFindGenericSensor(func:string): TYGenericSensor;
    begin
      result := TYGenericSensor.FindGenericSensor(func);
    end;

  function yFirstGenericSensor(): TYGenericSensor;
    begin
      result := TYGenericSensor.FirstGenericSensor();
    end;

  procedure _GenericSensorCleanup();
    begin
    end;

//--- (end of GenericSensor functions)

initialization
  //--- (GenericSensor initialization)
  //--- (end of GenericSensor initialization)

finalization
  //--- (GenericSensor cleanup)
  _GenericSensorCleanup();
  //--- (end of GenericSensor cleanup)
end.
