{*********************************************************************
 *
 * $Id: yocto_rangefinder.pas 31386 2018-07-31 12:26:57Z seb $
 *
 * Implements yFindRangeFinder(), the high-level API for RangeFinder functions
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


unit yocto_rangefinder;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YRangeFinder definitions)

const Y_RANGEFINDERMODE_DEFAULT = 0;
const Y_RANGEFINDERMODE_LONG_RANGE = 1;
const Y_RANGEFINDERMODE_HIGH_ACCURACY = 2;
const Y_RANGEFINDERMODE_HIGH_SPEED = 3;
const Y_RANGEFINDERMODE_INVALID = -1;
const Y_HARDWARECALIBRATION_INVALID   = YAPI_INVALID_STRING;
const Y_CURRENTTEMPERATURE_INVALID    = YAPI_INVALID_DOUBLE;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YRangeFinder definitions)
//--- (YRangeFinder yapiwrapper declaration)
//--- (end of YRangeFinder yapiwrapper declaration)

type
  TYRangeFinder = class;
  //--- (YRangeFinder class start)
  TYRangeFinderValueCallback = procedure(func: TYRangeFinder; value:string);
  TYRangeFinderTimedReportCallback = procedure(func: TYRangeFinder; value:TYMeasure);

  ////
  /// <summary>
  ///   TYRangeFinder Class: RangeFinder function interface
  /// <para>
  ///   The Yoctopuce class YRangeFinder allows you to use and configure Yoctopuce range finder
  ///   sensors. It inherits from the YSensor class the core functions to read measurements,
  ///   register callback functions, access the autonomous datalogger.
  ///   This class adds the ability to easily perform a one-point linear calibration
  ///   to compensate the effect of a glass or filter placed in front of the sensor.
  /// </para>
  /// </summary>
  ///-
  TYRangeFinder=class(TYSensor)
  //--- (end of YRangeFinder class start)
  protected
  //--- (YRangeFinder declaration)
    // Attributes (function value cache)
    _rangeFinderMode          : Integer;
    _hardwareCalibration      : string;
    _currentTemperature       : double;
    _command                  : string;
    _valueCallbackRangeFinder : TYRangeFinderValueCallback;
    _timedReportCallbackRangeFinder : TYRangeFinderTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YRangeFinder declaration)

  public
    //--- (YRangeFinder accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the measuring unit for the measured range.
    /// <para>
    ///   That unit is a string.
    ///   String value can be <c>"</c> or <c>mm</c>. Any other value is ignored.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    ///   WARNING: if a specific calibration is defined for the rangeFinder function, a
    ///   unit system change will probably break it.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the measuring unit for the measured range
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
    ///   Returns the range finder running mode.
    /// <para>
    ///   The rangefinder running mode
    ///   allows you to put priority on precision, speed or maximum range.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_RANGEFINDERMODE_DEFAULT</c>, <c>Y_RANGEFINDERMODE_LONG_RANGE</c>,
    ///   <c>Y_RANGEFINDERMODE_HIGH_ACCURACY</c> and <c>Y_RANGEFINDERMODE_HIGH_SPEED</c> corresponding to the
    ///   range finder running mode
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_RANGEFINDERMODE_INVALID</c>.
    /// </para>
    ///-
    function get_rangeFinderMode():Integer;

    ////
    /// <summary>
    ///   Changes the rangefinder running mode, allowing you to put priority on
    ///   precision, speed or maximum range.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_RANGEFINDERMODE_DEFAULT</c>, <c>Y_RANGEFINDERMODE_LONG_RANGE</c>,
    ///   <c>Y_RANGEFINDERMODE_HIGH_ACCURACY</c> and <c>Y_RANGEFINDERMODE_HIGH_SPEED</c> corresponding to the
    ///   rangefinder running mode, allowing you to put priority on
    ///   precision, speed or maximum range
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
    function set_rangeFinderMode(newval:Integer):integer;

    function get_hardwareCalibration():string;

    function set_hardwareCalibration(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the current sensor temperature, as a floating point number.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current sensor temperature, as a floating point number
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CURRENTTEMPERATURE_INVALID</c>.
    /// </para>
    ///-
    function get_currentTemperature():double;

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
    ///   Use the method <c>YRangeFinder.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YRangeFinder</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindRangeFinder(func: string):TYRangeFinder;

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
    function registerValueCallback(callback: TYRangeFinderValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYRangeFinderTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;

    ////
    /// <summary>
    ///   Returns the temperature at the time when the latest calibration was performed.
    /// <para>
    ///   This function can be used to determine if a new calibration for ambient temperature
    ///   is required.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a temperature, as a floating point number.
    ///   On failure, throws an exception or return YAPI_INVALID_DOUBLE.
    /// </returns>
    ///-
    function get_hardwareCalibrationTemperature():double; overload; virtual;

    ////
    /// <summary>
    ///   Triggers a sensor calibration according to the current ambient temperature.
    /// <para>
    ///   That
    ///   calibration process needs no physical interaction with the sensor. It is performed
    ///   automatically at device startup, but it is recommended to start it again when the
    ///   temperature delta since the latest calibration exceeds 8°C.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function triggerTemperatureCalibration():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Triggers the photon detector hardware calibration.
    /// <para>
    ///   This function is part of the calibration procedure to compensate for the the effect
    ///   of a cover glass. Make sure to read the chapter about hardware calibration for details
    ///   on the calibration procedure for proper results.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function triggerSpadCalibration():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Triggers the hardware offset calibration of the distance sensor.
    /// <para>
    ///   This function is part of the calibration procedure to compensate for the the effect
    ///   of a cover glass. Make sure to read the chapter about hardware calibration for details
    ///   on the calibration procedure for proper results.
    /// </para>
    /// </summary>
    /// <param name="targetDist">
    ///   true distance of the calibration target, in mm or inches, depending
    ///   on the unit selected in the device
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function triggerOffsetCalibration(targetDist: double):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Triggers the hardware cross-talk calibration of the distance sensor.
    /// <para>
    ///   This function is part of the calibration procedure to compensate for the the effect
    ///   of a cover glass. Make sure to read the chapter about hardware calibration for details
    ///   on the calibration procedure for proper results.
    /// </para>
    /// </summary>
    /// <param name="targetDist">
    ///   true distance of the calibration target, in mm or inches, depending
    ///   on the unit selected in the device
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function triggerXTalkCalibration(targetDist: double):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Cancels the effect of previous hardware calibration procedures to compensate
    ///   for cover glass, and restores factory settings.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function cancelCoverGlassCalibrations():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of range finders started using <c>yFirstRangeFinder()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YRangeFinder</c> object, corresponding to
    ///   a range finder currently online, or a <c>NIL</c> pointer
    ///   if there are no more range finders to enumerate.
    /// </returns>
    ///-
    function nextRangeFinder():TYRangeFinder;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstRangeFinder():TYRangeFinder;
  //--- (end of YRangeFinder accessors declaration)
  end;

//--- (YRangeFinder functions declaration)
  ////
  /// <summary>
  ///   Retrieves a range finder for a given identifier.
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
  ///   This function does not require that the range finder is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YRangeFinder.isOnline()</c> to test if the range finder is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a range finder by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the range finder
  /// </param>
  /// <returns>
  ///   a <c>YRangeFinder</c> object allowing you to drive the range finder.
  /// </returns>
  ///-
  function yFindRangeFinder(func:string):TYRangeFinder;
  ////
  /// <summary>
  ///   Starts the enumeration of range finders currently accessible.
  /// <para>
  ///   Use the method <c>YRangeFinder.nextRangeFinder()</c> to iterate on
  ///   next range finders.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YRangeFinder</c> object, corresponding to
  ///   the first range finder currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstRangeFinder():TYRangeFinder;

//--- (end of YRangeFinder functions declaration)

implementation
//--- (YRangeFinder dlldef)
//--- (end of YRangeFinder dlldef)

  constructor TYRangeFinder.Create(func:string);
    begin
      inherited Create(func);
      _className := 'RangeFinder';
      //--- (YRangeFinder accessors initialization)
      _rangeFinderMode := Y_RANGEFINDERMODE_INVALID;
      _hardwareCalibration := Y_HARDWARECALIBRATION_INVALID;
      _currentTemperature := Y_CURRENTTEMPERATURE_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackRangeFinder := nil;
      _timedReportCallbackRangeFinder := nil;
      //--- (end of YRangeFinder accessors initialization)
    end;

//--- (YRangeFinder yapiwrapper)
//--- (end of YRangeFinder yapiwrapper)

//--- (YRangeFinder implementation)
{$HINTS OFF}
  function TYRangeFinder._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'rangeFinderMode') then
        begin
          _rangeFinderMode := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'hardwareCalibration') then
        begin
          _hardwareCalibration := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'currentTemperature') then
        begin
          _currentTemperature := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
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

  ////
  /// <summary>
  ///   Changes the measuring unit for the measured range.
  /// <para>
  ///   That unit is a string.
  ///   String value can be " or mm. Any other value is ignored.
  ///   Remember to call the saveToFlash() method of the module if the modification must be kept.
  ///   WARNING: if a specific calibration is defined for the rangeFinder function, a
  ///   unit system change will probably break it.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the measuring unit for the measured range
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
  function TYRangeFinder.set_unit(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('unit',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the range finder running mode.
  /// <para>
  ///   The rangefinder running mode
  ///   allows you to put priority on precision, speed or maximum range.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_RANGEFINDERMODE_DEFAULT, Y_RANGEFINDERMODE_LONG_RANGE,
  ///   Y_RANGEFINDERMODE_HIGH_ACCURACY and Y_RANGEFINDERMODE_HIGH_SPEED corresponding to the range finder running mode
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_RANGEFINDERMODE_INVALID.
  /// </para>
  ///-
  function TYRangeFinder.get_rangeFinderMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RANGEFINDERMODE_INVALID;
              exit;
            end;
        end;
      res := self._rangeFinderMode;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the rangefinder running mode, allowing you to put priority on
  ///   precision, speed or maximum range.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_RANGEFINDERMODE_DEFAULT, Y_RANGEFINDERMODE_LONG_RANGE,
  ///   Y_RANGEFINDERMODE_HIGH_ACCURACY and Y_RANGEFINDERMODE_HIGH_SPEED corresponding to the rangefinder
  ///   running mode, allowing you to put priority on
  ///   precision, speed or maximum range
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
  function TYRangeFinder.set_rangeFinderMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('rangeFinderMode',rest_val);
    end;

  function TYRangeFinder.get_hardwareCalibration():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_HARDWARECALIBRATION_INVALID;
              exit;
            end;
        end;
      res := self._hardwareCalibration;
      result := res;
      exit;
    end;


  function TYRangeFinder.set_hardwareCalibration(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('hardwareCalibration',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the current sensor temperature, as a floating point number.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the current sensor temperature, as a floating point number
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_CURRENTTEMPERATURE_INVALID.
  /// </para>
  ///-
  function TYRangeFinder.get_currentTemperature():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTTEMPERATURE_INVALID;
              exit;
            end;
        end;
      res := self._currentTemperature;
      result := res;
      exit;
    end;


  function TYRangeFinder.get_command():string;
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


  function TYRangeFinder.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
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
  ///   Use the method <c>YRangeFinder.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YRangeFinder</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYRangeFinder.FindRangeFinder(func: string):TYRangeFinder;
    var
      obj : TYRangeFinder;
    begin
      obj := TYRangeFinder(TYFunction._FindFromCache('RangeFinder', func));
      if obj = nil then
        begin
          obj :=  TYRangeFinder.create(func);
          TYFunction._AddToCache('RangeFinder',  func, obj);
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
  function TYRangeFinder.registerValueCallback(callback: TYRangeFinderValueCallback):LongInt;
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
      self._valueCallbackRangeFinder := callback;
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


  function TYRangeFinder._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackRangeFinder) <> nil) then
        begin
          self._valueCallbackRangeFinder(self, value);
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
  function TYRangeFinder.registerTimedReportCallback(callback: TYRangeFinderTimedReportCallback):LongInt;
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
      self._timedReportCallbackRangeFinder := callback;
      result := 0;
      exit;
    end;


  function TYRangeFinder._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackRangeFinder) <> nil) then
        begin
          self._timedReportCallbackRangeFinder(self, value);
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
  ///   Returns the temperature at the time when the latest calibration was performed.
  /// <para>
  ///   This function can be used to determine if a new calibration for ambient temperature
  ///   is required.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a temperature, as a floating point number.
  ///   On failure, throws an exception or return YAPI_INVALID_DOUBLE.
  /// </returns>
  ///-
  function TYRangeFinder.get_hardwareCalibrationTemperature():double;
    var
      hwcal : string;
    begin
      hwcal := self.get_hardwareCalibration;
      if not((Copy(hwcal, 0 + 1, 1) = '@')) then
        begin
          result := YAPI_INVALID_DOUBLE;
          exit;
        end;
      result := _atoi(Copy(hwcal, 1 + 1, Length(hwcal)));
      exit;
    end;


  ////
  /// <summary>
  ///   Triggers a sensor calibration according to the current ambient temperature.
  /// <para>
  ///   That
  ///   calibration process needs no physical interaction with the sensor. It is performed
  ///   automatically at device startup, but it is recommended to start it again when the
  ///   temperature delta since the latest calibration exceeds 8°C.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYRangeFinder.triggerTemperatureCalibration():LongInt;
    begin
      result := self.set_command('T');
      exit;
    end;


  ////
  /// <summary>
  ///   Triggers the photon detector hardware calibration.
  /// <para>
  ///   This function is part of the calibration procedure to compensate for the the effect
  ///   of a cover glass. Make sure to read the chapter about hardware calibration for details
  ///   on the calibration procedure for proper results.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYRangeFinder.triggerSpadCalibration():LongInt;
    begin
      result := self.set_command('S');
      exit;
    end;


  ////
  /// <summary>
  ///   Triggers the hardware offset calibration of the distance sensor.
  /// <para>
  ///   This function is part of the calibration procedure to compensate for the the effect
  ///   of a cover glass. Make sure to read the chapter about hardware calibration for details
  ///   on the calibration procedure for proper results.
  /// </para>
  /// </summary>
  /// <param name="targetDist">
  ///   true distance of the calibration target, in mm or inches, depending
  ///   on the unit selected in the device
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYRangeFinder.triggerOffsetCalibration(targetDist: double):LongInt;
    var
      distmm : LongInt;
    begin
      if (self.get_unit = '"') then
        begin
          distmm := round(targetDist * 25.4);
        end
      else
        begin
          distmm := round(targetDist);
        end;
      result := self.set_command('O'+inttostr(distmm));
      exit;
    end;


  ////
  /// <summary>
  ///   Triggers the hardware cross-talk calibration of the distance sensor.
  /// <para>
  ///   This function is part of the calibration procedure to compensate for the the effect
  ///   of a cover glass. Make sure to read the chapter about hardware calibration for details
  ///   on the calibration procedure for proper results.
  /// </para>
  /// </summary>
  /// <param name="targetDist">
  ///   true distance of the calibration target, in mm or inches, depending
  ///   on the unit selected in the device
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYRangeFinder.triggerXTalkCalibration(targetDist: double):LongInt;
    var
      distmm : LongInt;
    begin
      if (self.get_unit = '"') then
        begin
          distmm := round(targetDist * 25.4);
        end
      else
        begin
          distmm := round(targetDist);
        end;
      result := self.set_command('X'+inttostr(distmm));
      exit;
    end;


  ////
  /// <summary>
  ///   Cancels the effect of previous hardware calibration procedures to compensate
  ///   for cover glass, and restores factory settings.
  /// <para>
  ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYRangeFinder.cancelCoverGlassCalibrations():LongInt;
    begin
      result := self.set_hardwareCalibration('');
      exit;
    end;


  function TYRangeFinder.nextRangeFinder(): TYRangeFinder;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextRangeFinder := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextRangeFinder := nil;
          exit;
        end;
      nextRangeFinder := TYRangeFinder.FindRangeFinder(hwid);
    end;

  class function TYRangeFinder.FirstRangeFinder(): TYRangeFinder;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('RangeFinder', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYRangeFinder.FindRangeFinder(serial+'.'+funcId);
    end;

//--- (end of YRangeFinder implementation)

//--- (YRangeFinder functions)

  function yFindRangeFinder(func:string): TYRangeFinder;
    begin
      result := TYRangeFinder.FindRangeFinder(func);
    end;

  function yFirstRangeFinder(): TYRangeFinder;
    begin
      result := TYRangeFinder.FirstRangeFinder();
    end;

  procedure _RangeFinderCleanup();
    begin
    end;

//--- (end of YRangeFinder functions)

initialization
  //--- (YRangeFinder initialization)
  //--- (end of YRangeFinder initialization)

finalization
  //--- (YRangeFinder cleanup)
  _RangeFinderCleanup();
  //--- (end of YRangeFinder cleanup)
end.
