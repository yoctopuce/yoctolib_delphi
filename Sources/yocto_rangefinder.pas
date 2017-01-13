{*********************************************************************
 *
 * $Id: yocto_rangefinder.pas 26329 2017-01-11 14:04:39Z mvuilleu $
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
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YRangeFinder definitions)

type
  TYRangeFinder = class;
  //--- (YRangeFinder class start)
  TYRangeFinderValueCallback = procedure(func: TYRangeFinder; value:string);
  TYRangeFinderTimedReportCallback = procedure(func: TYRangeFinder; value:TYMeasure);

  ////
  /// <summary>
  ///   TYRangeFinder Class: RangeFinder function interface
  /// <para>
  ///   The Yoctopuce class YRangeFinder allows you to use and configure Yoctopuce range finders
  ///   sensors. It inherits from YSensor class the core functions to read measurements,
  ///   register callback functions, access to the autonomous datalogger.
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
    _rangeFinderMode          : Integer;
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
    ///   Changes the measuring unit for the measured temperature.
    /// <para>
    ///   That unit is a string.
    ///   String value can be <c>"</c> or <c>mm</c>. Any other value will be ignored.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    ///   WARNING: if a specific calibration is defined for the rangeFinder function, a
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_unit(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the rangefinder running mode.
    /// <para>
    ///   The rangefinder running mode
    ///   allows to put priority on precision, speed or maximum range.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_RANGEFINDERMODE_DEFAULT</c>, <c>Y_RANGEFINDERMODE_LONG_RANGE</c>,
    ///   <c>Y_RANGEFINDERMODE_HIGH_ACCURACY</c> and <c>Y_RANGEFINDERMODE_HIGH_SPEED</c> corresponding to the
    ///   rangefinder running mode
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_RANGEFINDERMODE_INVALID</c>.
    /// </para>
    ///-
    function get_rangeFinderMode():Integer;

    ////
    /// <summary>
    ///   Changes the rangefinder running mode, allowing to put priority on
    ///   precision, speed or maximum range.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_RANGEFINDERMODE_DEFAULT</c>, <c>Y_RANGEFINDERMODE_LONG_RANGE</c>,
    ///   <c>Y_RANGEFINDERMODE_HIGH_ACCURACY</c> and <c>Y_RANGEFINDERMODE_HIGH_SPEED</c> corresponding to the
    ///   rangefinder running mode, allowing to put priority on
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
    ///   Triggers a sensor calibration according to the current ambient temperature.
    /// <para>
    ///   That
    ///   calibration process needs no physical interaction with the sensor. It is performed
    ///   automatically at device startup, but it is recommended to start it again when the
    ///   temperature delta since last calibration exceeds 8°C.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function triggerTempCalibration():LongInt; overload; virtual;


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

//--- (RangeFinder functions declaration)
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

//--- (end of RangeFinder functions declaration)

implementation
//--- (YRangeFinder dlldef)
//--- (end of YRangeFinder dlldef)

  constructor TYRangeFinder.Create(func:string);
    begin
      inherited Create(func);
      _className := 'RangeFinder';
      //--- (YRangeFinder accessors initialization)
      _rangeFinderMode := Y_RANGEFINDERMODE_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackRangeFinder := nil;
      _timedReportCallbackRangeFinder := nil;
      //--- (end of YRangeFinder accessors initialization)
    end;


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
  ///   Changes the measuring unit for the measured temperature.
  /// <para>
  ///   That unit is a string.
  ///   String value can be " or mm. Any other value will be ignored.
  ///   Remember to call the saveToFlash() method of the module if the modification must be kept.
  ///   WARNING: if a specific calibration is defined for the rangeFinder function, a
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
  ///   Returns the rangefinder running mode.
  /// <para>
  ///   The rangefinder running mode
  ///   allows to put priority on precision, speed or maximum range.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_RANGEFINDERMODE_DEFAULT, Y_RANGEFINDERMODE_LONG_RANGE,
  ///   Y_RANGEFINDERMODE_HIGH_ACCURACY and Y_RANGEFINDERMODE_HIGH_SPEED corresponding to the rangefinder running mode
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_RANGEFINDERMODE_INVALID.
  /// </para>
  ///-
  function TYRangeFinder.get_rangeFinderMode():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_RANGEFINDERMODE_INVALID;
              exit;
            end;
        end;
      result := self._rangeFinderMode;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the rangefinder running mode, allowing to put priority on
  ///   precision, speed or maximum range.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_RANGEFINDERMODE_DEFAULT, Y_RANGEFINDERMODE_LONG_RANGE,
  ///   Y_RANGEFINDERMODE_HIGH_ACCURACY and Y_RANGEFINDERMODE_HIGH_SPEED corresponding to the rangefinder
  ///   running mode, allowing to put priority on
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

  function TYRangeFinder.get_command():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_COMMAND_INVALID;
              exit;
            end;
        end;
      result := self._command;
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
  ///   Triggers a sensor calibration according to the current ambient temperature.
  /// <para>
  ///   That
  ///   calibration process needs no physical interaction with the sensor. It is performed
  ///   automatically at device startup, but it is recommended to start it again when the
  ///   temperature delta since last calibration exceeds 8°C.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYRangeFinder.triggerTempCalibration():LongInt;
    begin
      result := self.set_command('T');
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

//--- (RangeFinder functions)

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

//--- (end of RangeFinder functions)

initialization
  //--- (RangeFinder initialization)
  //--- (end of RangeFinder initialization)

finalization
  //--- (RangeFinder cleanup)
  _RangeFinderCleanup();
  //--- (end of RangeFinder cleanup)
end.
