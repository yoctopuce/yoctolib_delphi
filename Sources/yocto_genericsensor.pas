{*********************************************************************
 *
 *  $Id: yocto_genericsensor.pas 63506 2024-11-28 10:42:13Z seb $
 *
 *  Implements yFindGenericSensor(), the high-level API for GenericSensor functions
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


unit yocto_genericsensor;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

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
const Y_SIGNALSAMPLING_HIGHEST_RATE = 4;
const Y_SIGNALSAMPLING_AC = 5;
const Y_SIGNALSAMPLING_INVALID = -1;
const Y_ENABLED_FALSE = 0;
const Y_ENABLED_TRUE = 1;
const Y_ENABLED_INVALID = -1;

//--- (end of YGenericSensor definitions)

//--- (YGenericSensor yapiwrapper declaration)
//--- (end of YGenericSensor yapiwrapper declaration)

type

  TYGenericSensor = class;
  //--- (YGenericSensor class start)
  TYGenericSensorValueCallback = procedure(func: TYGenericSensor; value:string);
  TYGenericSensorTimedReportCallback = procedure(func: TYGenericSensor; value:TYMeasure);

  ////
  /// <summary>
  ///   TYGenericSensor Class: GenericSensor control interface, available for instance in the
  ///   Yocto-0-10V-Rx, the Yocto-4-20mA-Rx, the Yocto-Bridge or the Yocto-milliVolt-Rx
  /// <para>
  ///   The <c>YGenericSensor</c> class allows you to read and configure Yoctopuce signal
  ///   transducers. It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, to access the autonomous datalogger.
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
    _signalValue              : double;
    _signalUnit               : string;
    _signalRange              : string;
    _valueRange               : string;
    _signalBias               : double;
    _signalSampling           : Integer;
    _enabled                  : Integer;
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_unit(newval:string):integer;

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
    ///   On failure, throws an exception or returns <c>YGenericSensor.SIGNALVALUE_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YGenericSensor.SIGNALUNIT_INVALID</c>.
    /// </para>
    ///-
    function get_signalUnit():string;

    ////
    /// <summary>
    ///   Returns the input signal range used by the sensor.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the input signal range used by the sensor
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YGenericSensor.SIGNALRANGE_INVALID</c>.
    /// </para>
    ///-
    function get_signalRange():string;

    ////
    /// <summary>
    ///   Changes the input signal range used by the sensor.
    /// <para>
    ///   When the input signal gets out of the planned range, the output value
    ///   will be set to an arbitrary large value, whose sign indicates the direction
    ///   of the range overrun.
    /// </para>
    /// <para>
    ///   For a 4-20mA sensor, the default input signal range is "4...20".
    ///   For a 0-10V sensor, the default input signal range is "0.1...10".
    ///   For numeric communication interfaces, the default input signal range is
    ///   "-999999.999...999999.999".
    /// </para>
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the input signal range used by the sensor
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
    ///   On failure, throws an exception or returns <c>YGenericSensor.VALUERANGE_INVALID</c>.
    /// </para>
    ///-
    function get_valueRange():string;

    ////
    /// <summary>
    ///   Changes the output value range, corresponding to the physical value measured
    ///   by the sensor.
    /// <para>
    ///   The default output value range is the same as the input signal
    ///   range (1:1 mapping), but you can change it so that the function automatically
    ///   computes the physical value encoded by the input signal. Be aware that, as a
    ///   side effect, the range modification may automatically modify the display resolution.
    /// </para>
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the output value range, corresponding to the physical value measured
    ///   by the sensor
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
    function set_valueRange(newval:string):integer;

    ////
    /// <summary>
    ///   Changes the electric signal bias for zero shift adjustment.
    /// <para>
    ///   If your electric signal reads positive when it should be zero, set up
    ///   a positive signalBias of the same value to fix the zero shift.
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
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
    ///   while a negative bias means that the signal is under-reporting the measure.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the electric signal bias for zero shift adjustment
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YGenericSensor.SIGNALBIAS_INVALID</c>.
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
    ///   a value among <c>YGenericSensor.SIGNALSAMPLING_HIGH_RATE</c>,
    ///   <c>YGenericSensor.SIGNALSAMPLING_HIGH_RATE_FILTERED</c>, <c>YGenericSensor.SIGNALSAMPLING_LOW_NOISE</c>,
    ///   <c>YGenericSensor.SIGNALSAMPLING_LOW_NOISE_FILTERED</c>, <c>YGenericSensor.SIGNALSAMPLING_HIGHEST_RATE</c>
    ///   and <c>YGenericSensor.SIGNALSAMPLING_AC</c> corresponding to the electric signal sampling method to use
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YGenericSensor.SIGNALSAMPLING_INVALID</c>.
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
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YGenericSensor.SIGNALSAMPLING_HIGH_RATE</c>,
    ///   <c>YGenericSensor.SIGNALSAMPLING_HIGH_RATE_FILTERED</c>, <c>YGenericSensor.SIGNALSAMPLING_LOW_NOISE</c>,
    ///   <c>YGenericSensor.SIGNALSAMPLING_LOW_NOISE_FILTERED</c>, <c>YGenericSensor.SIGNALSAMPLING_HIGHEST_RATE</c>
    ///   and <c>YGenericSensor.SIGNALSAMPLING_AC</c> corresponding to the electric signal sampling method to use
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
    function set_signalSampling(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the activation state of this input.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YGenericSensor.ENABLED_FALSE</c> or <c>YGenericSensor.ENABLED_TRUE</c>, according to the
    ///   activation state of this input
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YGenericSensor.ENABLED_INVALID</c>.
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
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YGenericSensor.ENABLED_FALSE</c> or <c>YGenericSensor.ENABLED_TRUE</c>, according to the
    ///   activation state of this input
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
    ///   Use the method <c>YGenericSensor.isOnline()</c> to test if $THEFUNCTION$ is
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
    function registerValueCallback(callback: TYGenericSensorValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYGenericSensorTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;

    ////
    /// <summary>
    ///   Adjusts the signal bias so that the current signal value is need
    ///   precisely as zero.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
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
    ///   Caution: You can't make any assumption about the returned generic sensors order.
    ///   If you want to find a specific a generic sensor, use <c>GenericSensor.findGenericSensor()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YGenericSensor</c> object, corresponding to
    ///   a generic sensor currently online, or a <c>NIL</c> pointer
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

//--- (YGenericSensor functions declaration)
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
  /// <para>
  ///   If a call to this object's is_online() method returns FALSE although
  ///   you are certain that the matching device is plugged, make sure that you did
  ///   call registerHub() at application initialization time.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the generic sensor, for instance
  ///   <c>RX010V01.genericSensor1</c>.
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
  ///   the first generic sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstGenericSensor():TYGenericSensor;

//--- (end of YGenericSensor functions declaration)

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
      _enabled := Y_ENABLED_INVALID;
      _valueCallbackGenericSensor := nil;
      _timedReportCallbackGenericSensor := nil;
      //--- (end of YGenericSensor accessors initialization)
    end;

//--- (YGenericSensor yapiwrapper)
//--- (end of YGenericSensor yapiwrapper)

//--- (YGenericSensor implementation)
{$HINTS OFF}
  function TYGenericSensor._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
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
          _signalBias := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'signalSampling') then
        begin
          _signalSampling := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'enabled') then
        begin
          _enabled := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYGenericSensor.set_unit(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('unit',rest_val);
    end;

  function TYGenericSensor.get_signalValue():double;
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


  function TYGenericSensor.get_signalUnit():string;
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


  function TYGenericSensor.get_signalRange():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNALRANGE_INVALID;
              exit;
            end;
        end;
      res := self._signalRange;
      result := res;
      exit;
    end;


  function TYGenericSensor.set_signalRange(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('signalRange',rest_val);
    end;

  function TYGenericSensor.get_valueRange():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_VALUERANGE_INVALID;
              exit;
            end;
        end;
      res := self._valueRange;
      result := res;
      exit;
    end;


  function TYGenericSensor.set_valueRange(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('valueRange',rest_val);
    end;

  function TYGenericSensor.set_signalBias(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('signalBias',rest_val);
    end;

  function TYGenericSensor.get_signalBias():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNALBIAS_INVALID;
              exit;
            end;
        end;
      res := self._signalBias;
      result := res;
      exit;
    end;


  function TYGenericSensor.get_signalSampling():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNALSAMPLING_INVALID;
              exit;
            end;
        end;
      res := self._signalSampling;
      result := res;
      exit;
    end;


  function TYGenericSensor.set_signalSampling(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('signalSampling',rest_val);
    end;

  function TYGenericSensor.get_enabled():Integer;
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


  function TYGenericSensor.set_enabled(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('enabled',rest_val);
    end;

  class function TYGenericSensor.FindGenericSensor(func: string):TYGenericSensor;
    var
      obj : TYGenericSensor;
    begin
      obj := TYGenericSensor(TYFunction._FindFromCache('GenericSensor', func));
      if obj = nil then
        begin
          obj :=  TYGenericSensor.create(func);
          TYFunction._AddToCache('GenericSensor', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYGenericSensor.registerValueCallback(callback: TYGenericSensorValueCallback):LongInt;
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
      self._valueCallbackGenericSensor := callback;
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


  function TYGenericSensor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackGenericSensor) <> nil) then
        begin
          self._valueCallbackGenericSensor(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYGenericSensor.registerTimedReportCallback(callback: TYGenericSensorTimedReportCallback):LongInt;
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
      self._timedReportCallbackGenericSensor := callback;
      result := 0;
      exit;
    end;


  function TYGenericSensor._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackGenericSensor) <> nil) then
        begin
          self._timedReportCallbackGenericSensor(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


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

//--- (YGenericSensor functions)

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

//--- (end of YGenericSensor functions)

initialization
  //--- (YGenericSensor initialization)
  //--- (end of YGenericSensor initialization)

finalization
  //--- (YGenericSensor cleanup)
  _GenericSensorCleanup();
  //--- (end of YGenericSensor cleanup)

end.
