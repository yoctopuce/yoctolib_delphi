{*********************************************************************
 *
 * $Id: pic24config.php 26169 2016-12-12 01:36:34Z mvuilleu $
 *
 * Implements yFindProximity(), the high-level API for Proximity functions
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


unit yocto_proximity;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YProximity definitions)

const Y_DETECTIONTHRESHOLD_INVALID    = YAPI_INVALID_UINT;
const Y_ISPRESENT_FALSE = 0;
const Y_ISPRESENT_TRUE = 1;
const Y_ISPRESENT_INVALID = -1;
const Y_LASTTIMEAPPROACHED_INVALID    = YAPI_INVALID_LONG;
const Y_LASTTIMEREMOVED_INVALID       = YAPI_INVALID_LONG;
const Y_PULSECOUNTER_INVALID          = YAPI_INVALID_LONG;
const Y_PULSETIMER_INVALID            = YAPI_INVALID_LONG;


//--- (end of YProximity definitions)

type
  TYProximity = class;
  //--- (YProximity class start)
  TYProximityValueCallback = procedure(func: TYProximity; value:string);
  TYProximityTimedReportCallback = procedure(func: TYProximity; value:TYMeasure);

  ////
  /// <summary>
  ///   TYProximity Class: Proximity function interface
  /// <para>
  ///   The Yoctopuce class YProximity allows you to use and configure Yoctopuce proximity
  ///   sensors. It inherits from YSensor class the core functions to read measurements,
  ///   register callback functions, access to the autonomous datalogger.
  ///   This class adds the ability to easily perform a one-point linear calibration
  ///   to compensate the effect of a glass or filter placed in front of the sensor.
  /// </para>
  /// </summary>
  ///-
  TYProximity=class(TYSensor)
  //--- (end of YProximity class start)
  protected
  //--- (YProximity declaration)
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
    _detectionThreshold       : LongInt;
    _isPresent                : Integer;
    _lastTimeApproached       : int64;
    _lastTimeRemoved          : int64;
    _pulseCounter             : int64;
    _pulseTimer               : int64;
    _valueCallbackProximity   : TYProximityValueCallback;
    _timedReportCallbackProximity : TYProximityTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YProximity declaration)

  public
    //--- (YProximity accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the threshold used to determine the logical state of the proximity sensor, when considered
    ///   as a binary input (on/off).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the threshold used to determine the logical state of the proximity
    ///   sensor, when considered
    ///   as a binary input (on/off)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DETECTIONTHRESHOLD_INVALID</c>.
    /// </para>
    ///-
    function get_detectionThreshold():LongInt;

    ////
    /// <summary>
    ///   Changes the threshold used to determine the logical state of the proximity sensor, when considered
    ///   as a binary input (on/off).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the threshold used to determine the logical state of the proximity
    ///   sensor, when considered
    ///   as a binary input (on/off)
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
    function set_detectionThreshold(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns true if the input (considered as binary) is active (detection value is smaller than the specified <c>threshold</c>), and false otherwise.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_ISPRESENT_FALSE</c> or <c>Y_ISPRESENT_TRUE</c>, according to true if the input
    ///   (considered as binary) is active (detection value is smaller than the specified <c>threshold</c>),
    ///   and false otherwise
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ISPRESENT_INVALID</c>.
    /// </para>
    ///-
    function get_isPresent():Integer;

    ////
    /// <summary>
    ///   Returns the number of elapsed milliseconds between the module power on and the last observed
    ///   detection (the input contact transitioned from absent to present).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of elapsed milliseconds between the module power on and the last observed
    ///   detection (the input contact transitioned from absent to present)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LASTTIMEAPPROACHED_INVALID</c>.
    /// </para>
    ///-
    function get_lastTimeApproached():int64;

    ////
    /// <summary>
    ///   Returns the number of elapsed milliseconds between the module power on and the last observed
    ///   detection (the input contact transitioned from present to absent).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of elapsed milliseconds between the module power on and the last observed
    ///   detection (the input contact transitioned from present to absent)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LASTTIMEREMOVED_INVALID</c>.
    /// </para>
    ///-
    function get_lastTimeRemoved():int64;

    ////
    /// <summary>
    ///   Returns the pulse counter value.
    /// <para>
    ///   The value is a 32 bit integer. In case
    ///   of overflow (>=2^32), the counter will wrap. To reset the counter, just
    ///   call the resetCounter() method.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the pulse counter value
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PULSECOUNTER_INVALID</c>.
    /// </para>
    ///-
    function get_pulseCounter():int64;

    function set_pulseCounter(newval:int64):integer;

    ////
    /// <summary>
    ///   Returns the timer of the pulses counter (ms).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the timer of the pulses counter (ms)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PULSETIMER_INVALID</c>.
    /// </para>
    ///-
    function get_pulseTimer():int64;

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
    ///   Use the method <c>YProximity.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YProximity</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindProximity(func: string):TYProximity;

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
    function registerValueCallback(callback: TYProximityValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYProximityTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;

    ////
    /// <summary>
    ///   Returns the pulse counter value as well as its timer.
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
    function resetCounter():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of proximity sensors started using <c>yFirstProximity()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YProximity</c> object, corresponding to
    ///   a proximity sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more proximity sensors to enumerate.
    /// </returns>
    ///-
    function nextProximity():TYProximity;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstProximity():TYProximity;
  //--- (end of YProximity accessors declaration)
  end;

//--- (Proximity functions declaration)
  ////
  /// <summary>
  ///   Retrieves a proximity sensor for a given identifier.
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
  ///   This function does not require that the proximity sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YProximity.isOnline()</c> to test if the proximity sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a proximity sensor by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the proximity sensor
  /// </param>
  /// <returns>
  ///   a <c>YProximity</c> object allowing you to drive the proximity sensor.
  /// </returns>
  ///-
  function yFindProximity(func:string):TYProximity;
  ////
  /// <summary>
  ///   Starts the enumeration of proximity sensors currently accessible.
  /// <para>
  ///   Use the method <c>YProximity.nextProximity()</c> to iterate on
  ///   next proximity sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YProximity</c> object, corresponding to
  ///   the first proximity sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstProximity():TYProximity;

//--- (end of Proximity functions declaration)

implementation
//--- (YProximity dlldef)
//--- (end of YProximity dlldef)

  constructor TYProximity.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Proximity';
      //--- (YProximity accessors initialization)
      _detectionThreshold := Y_DETECTIONTHRESHOLD_INVALID;
      _isPresent := Y_ISPRESENT_INVALID;
      _lastTimeApproached := Y_LASTTIMEAPPROACHED_INVALID;
      _lastTimeRemoved := Y_LASTTIMEREMOVED_INVALID;
      _pulseCounter := Y_PULSECOUNTER_INVALID;
      _pulseTimer := Y_PULSETIMER_INVALID;
      _valueCallbackProximity := nil;
      _timedReportCallbackProximity := nil;
      //--- (end of YProximity accessors initialization)
    end;


//--- (YProximity implementation)
{$HINTS OFF}
  function TYProximity._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'detectionThreshold') then
        begin
          _detectionThreshold := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'isPresent') then
        begin
          _isPresent := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'lastTimeApproached') then
        begin
          _lastTimeApproached := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'lastTimeRemoved') then
        begin
          _lastTimeRemoved := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'pulseCounter') then
        begin
          _pulseCounter := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'pulseTimer') then
        begin
          _pulseTimer := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Returns the threshold used to determine the logical state of the proximity sensor, when considered
  ///   as a binary input (on/off).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the threshold used to determine the logical state of the proximity
  ///   sensor, when considered
  ///   as a binary input (on/off)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DETECTIONTHRESHOLD_INVALID.
  /// </para>
  ///-
  function TYProximity.get_detectionThreshold():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_DETECTIONTHRESHOLD_INVALID;
              exit;
            end;
        end;
      result := self._detectionThreshold;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the threshold used to determine the logical state of the proximity sensor, when considered
  ///   as a binary input (on/off).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the threshold used to determine the logical state of the proximity
  ///   sensor, when considered
  ///   as a binary input (on/off)
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
  function TYProximity.set_detectionThreshold(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('detectionThreshold',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns true if the input (considered as binary) is active (detection value is smaller than the specified threshold), and false otherwise.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_ISPRESENT_FALSE or Y_ISPRESENT_TRUE, according to true if the input (considered as binary)
  ///   is active (detection value is smaller than the specified threshold), and false otherwise
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ISPRESENT_INVALID.
  /// </para>
  ///-
  function TYProximity.get_isPresent():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ISPRESENT_INVALID;
              exit;
            end;
        end;
      result := self._isPresent;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the number of elapsed milliseconds between the module power on and the last observed
  ///   detection (the input contact transitioned from absent to present).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the number of elapsed milliseconds between the module power on and the last observed
  ///   detection (the input contact transitioned from absent to present)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LASTTIMEAPPROACHED_INVALID.
  /// </para>
  ///-
  function TYProximity.get_lastTimeApproached():int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LASTTIMEAPPROACHED_INVALID;
              exit;
            end;
        end;
      result := self._lastTimeApproached;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the number of elapsed milliseconds between the module power on and the last observed
  ///   detection (the input contact transitioned from present to absent).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the number of elapsed milliseconds between the module power on and the last observed
  ///   detection (the input contact transitioned from present to absent)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LASTTIMEREMOVED_INVALID.
  /// </para>
  ///-
  function TYProximity.get_lastTimeRemoved():int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LASTTIMEREMOVED_INVALID;
              exit;
            end;
        end;
      result := self._lastTimeRemoved;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the pulse counter value.
  /// <para>
  ///   The value is a 32 bit integer. In case
  ///   of overflow (>=2^32), the counter will wrap. To reset the counter, just
  ///   call the resetCounter() method.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the pulse counter value
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PULSECOUNTER_INVALID.
  /// </para>
  ///-
  function TYProximity.get_pulseCounter():int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PULSECOUNTER_INVALID;
              exit;
            end;
        end;
      result := self._pulseCounter;
      exit;
    end;


  function TYProximity.set_pulseCounter(newval:int64):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('pulseCounter',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the timer of the pulses counter (ms).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the timer of the pulses counter (ms)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PULSETIMER_INVALID.
  /// </para>
  ///-
  function TYProximity.get_pulseTimer():int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PULSETIMER_INVALID;
              exit;
            end;
        end;
      result := self._pulseTimer;
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
  ///   Use the method <c>YProximity.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YProximity</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYProximity.FindProximity(func: string):TYProximity;
    var
      obj : TYProximity;
    begin
      obj := TYProximity(TYFunction._FindFromCache('Proximity', func));
      if obj = nil then
        begin
          obj :=  TYProximity.create(func);
          TYFunction._AddToCache('Proximity',  func, obj);
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
  function TYProximity.registerValueCallback(callback: TYProximityValueCallback):LongInt;
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
      self._valueCallbackProximity := callback;
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


  function TYProximity._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackProximity) <> nil) then
        begin
          self._valueCallbackProximity(self, value);
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
  function TYProximity.registerTimedReportCallback(callback: TYProximityTimedReportCallback):LongInt;
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
      self._timedReportCallbackProximity := callback;
      result := 0;
      exit;
    end;


  function TYProximity._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackProximity) <> nil) then
        begin
          self._timedReportCallbackProximity(self, value);
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
  ///   Returns the pulse counter value as well as its timer.
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
  function TYProximity.resetCounter():LongInt;
    begin
      result := self.set_pulseCounter(0);
      exit;
    end;


  function TYProximity.nextProximity(): TYProximity;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextProximity := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextProximity := nil;
          exit;
        end;
      nextProximity := TYProximity.FindProximity(hwid);
    end;

  class function TYProximity.FirstProximity(): TYProximity;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Proximity', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYProximity.FindProximity(serial+'.'+funcId);
    end;

//--- (end of YProximity implementation)

//--- (Proximity functions)

  function yFindProximity(func:string): TYProximity;
    begin
      result := TYProximity.FindProximity(func);
    end;

  function yFirstProximity(): TYProximity;
    begin
      result := TYProximity.FirstProximity();
    end;

  procedure _ProximityCleanup();
    begin
    end;

//--- (end of Proximity functions)

initialization
  //--- (Proximity initialization)
  //--- (end of Proximity initialization)

finalization
  //--- (Proximity cleanup)
  _ProximityCleanup();
  //--- (end of Proximity cleanup)
end.
