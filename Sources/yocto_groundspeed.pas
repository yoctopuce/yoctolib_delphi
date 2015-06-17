{*********************************************************************
 *
 * $Id: yocto_groundspeed.pas 20400 2015-05-21 14:58:16Z mvuilleu $
 *
 * Implements yFindGroundSpeed(), the high-level API for GroundSpeed functions
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


unit yocto_groundspeed;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YGroundSpeed definitions)



//--- (end of YGroundSpeed definitions)

type
  TYGroundSpeed = class;
  //--- (YGroundSpeed class start)
  TYGroundSpeedValueCallback = procedure(func: TYGroundSpeed; value:string);
  TYGroundSpeedTimedReportCallback = procedure(func: TYGroundSpeed; value:TYMeasure);

  ////
  /// <summary>
  ///   TYGroundSpeed Class: GroundSpeed function interface
  /// <para>
  ///   The Yoctopuce class YGroundSpeed allows you to read the ground speed from Yoctopuce
  ///   geolocalization sensors. It inherits from the YSensor class the core functions to
  ///   read measurements, register callback functions, access the autonomous
  ///   datalogger.
  /// </para>
  /// </summary>
  ///-
  TYGroundSpeed=class(TYSensor)
  //--- (end of YGroundSpeed class start)
  protected
  //--- (YGroundSpeed declaration)
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
    _valueCallbackGroundSpeed : TYGroundSpeedValueCallback;
    _timedReportCallbackGroundSpeed : TYGroundSpeedTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YGroundSpeed declaration)

  public
    //--- (YGroundSpeed accessors declaration)
    constructor Create(func:string);

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
    ///   Use the method <c>YGroundSpeed.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YGroundSpeed</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindGroundSpeed(func: string):TYGroundSpeed;

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
    function registerValueCallback(callback: TYGroundSpeedValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYGroundSpeedTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of ground speed sensors started using <c>yFirstGroundSpeed()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YGroundSpeed</c> object, corresponding to
    ///   a ground speed sensor currently online, or a <c>null</c> pointer
    ///   if there are no more ground speed sensors to enumerate.
    /// </returns>
    ///-
    function nextGroundSpeed():TYGroundSpeed;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstGroundSpeed():TYGroundSpeed;
  //--- (end of YGroundSpeed accessors declaration)
  end;

//--- (GroundSpeed functions declaration)
  ////
  /// <summary>
  ///   Retrieves a ground speed sensor for a given identifier.
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
  ///   This function does not require that the ground speed sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YGroundSpeed.isOnline()</c> to test if the ground speed sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a ground speed sensor by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the ground speed sensor
  /// </param>
  /// <returns>
  ///   a <c>YGroundSpeed</c> object allowing you to drive the ground speed sensor.
  /// </returns>
  ///-
  function yFindGroundSpeed(func:string):TYGroundSpeed;
  ////
  /// <summary>
  ///   Starts the enumeration of ground speed sensors currently accessible.
  /// <para>
  ///   Use the method <c>YGroundSpeed.nextGroundSpeed()</c> to iterate on
  ///   next ground speed sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YGroundSpeed</c> object, corresponding to
  ///   the first ground speed sensor currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstGroundSpeed():TYGroundSpeed;

//--- (end of GroundSpeed functions declaration)

implementation
//--- (YGroundSpeed dlldef)
//--- (end of YGroundSpeed dlldef)

  constructor TYGroundSpeed.Create(func:string);
    begin
      inherited Create(func);
      _className := 'GroundSpeed';
      //--- (YGroundSpeed accessors initialization)
      _valueCallbackGroundSpeed := nil;
      _timedReportCallbackGroundSpeed := nil;
      //--- (end of YGroundSpeed accessors initialization)
    end;


//--- (YGroundSpeed implementation)
{$HINTS OFF}
  function TYGroundSpeed._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

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
  ///   Use the method <c>YGroundSpeed.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YGroundSpeed</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYGroundSpeed.FindGroundSpeed(func: string):TYGroundSpeed;
    var
      obj : TYGroundSpeed;
    begin
      obj := TYGroundSpeed(TYFunction._FindFromCache('GroundSpeed', func));
      if obj = nil then
        begin
          obj :=  TYGroundSpeed.create(func);
          TYFunction._AddToCache('GroundSpeed',  func, obj)
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
  function TYGroundSpeed.registerValueCallback(callback: TYGroundSpeedValueCallback):LongInt;
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
      self._valueCallbackGroundSpeed := callback;
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


  function TYGroundSpeed._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackGroundSpeed) <> nil) then
        begin
          self._valueCallbackGroundSpeed(self, value)
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
  function TYGroundSpeed.registerTimedReportCallback(callback: TYGroundSpeedTimedReportCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(self, false)
        end;
      self._timedReportCallbackGroundSpeed := callback;
      result := 0;
      exit;
    end;


  function TYGroundSpeed._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackGroundSpeed) <> nil) then
        begin
          self._timedReportCallbackGroundSpeed(self, value)
        end
      else
        begin
          inherited _invokeTimedReportCallback(value)
        end;
      result := 0;
      exit;
    end;


  function TYGroundSpeed.nextGroundSpeed(): TYGroundSpeed;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextGroundSpeed := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextGroundSpeed := nil;
          exit;
        end;
      nextGroundSpeed := TYGroundSpeed.FindGroundSpeed(hwid);
    end;

  class function TYGroundSpeed.FirstGroundSpeed(): TYGroundSpeed;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('GroundSpeed', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYGroundSpeed.FindGroundSpeed(serial+'.'+funcId);
    end;

//--- (end of YGroundSpeed implementation)

//--- (GroundSpeed functions)

  function yFindGroundSpeed(func:string): TYGroundSpeed;
    begin
      result := TYGroundSpeed.FindGroundSpeed(func);
    end;

  function yFirstGroundSpeed(): TYGroundSpeed;
    begin
      result := TYGroundSpeed.FirstGroundSpeed();
    end;

  procedure _GroundSpeedCleanup();
    begin
    end;

//--- (end of GroundSpeed functions)

initialization
  //--- (GroundSpeed initialization)
  //--- (end of GroundSpeed initialization)

finalization
  //--- (GroundSpeed cleanup)
  _GroundSpeedCleanup();
  //--- (end of GroundSpeed cleanup)
end.
