{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindVirtualSensor(), the high-level API for VirtualSensor functions
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


unit yocto_virtualsensor;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YVirtualSensor definitions)

const Y_INVALIDVALUE_INVALID          = YAPI_INVALID_DOUBLE;

//--- (end of YVirtualSensor definitions)

//--- (YVirtualSensor yapiwrapper declaration)
//--- (end of YVirtualSensor yapiwrapper declaration)

type

  TYVirtualSensor = class;
  //--- (YVirtualSensor class start)
  TYVirtualSensorValueCallback = procedure(func: TYVirtualSensor; value:string);
  TYVirtualSensorTimedReportCallback = procedure(func: TYVirtualSensor; value:TYMeasure);

  ////
  /// <summary>
  ///   TYVirtualSensor Class: virtual sensor control interface
  /// <para>
  ///   The <c>YVirtualSensor</c> class allows you to use Yoctopuce virtual sensors.
  ///   These sensors make it possible to show external data collected by the user
  ///   as a Yoctopuce Sensor. This class inherits from <c>YSensor</c> class the core
  ///   functions to read measurements, to register callback functions, and to access
  ///   the autonomous datalogger. It adds the ability to change the sensor value as
  ///   needed, or to mark current value as invalid.
  /// </para>
  /// </summary>
  ///-
  TYVirtualSensor=class(TYSensor)
  //--- (end of YVirtualSensor class start)
  protected
  //--- (YVirtualSensor declaration)
    // Attributes (function value cache)
    _invalidValue             : double;
    _valueCallbackVirtualSensor : TYVirtualSensorValueCallback;
    _timedReportCallbackVirtualSensor : TYVirtualSensorTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YVirtualSensor declaration)

  public
    //--- (YVirtualSensor accessors declaration)
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
    ///   Changes the current value of the sensor (raw value, before calibration).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the current value of the sensor (raw value, before calibration)
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
    function set_currentRawValue(newval:double):integer;

    function set_sensorState(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Changes the invalid value of the sensor, returned if the sensor is read when in invalid state
    ///   (for instance before having been set).
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the invalid value of the sensor, returned if the sensor is
    ///   read when in invalid state
    ///   (for instance before having been set)
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
    function set_invalidValue(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the invalid value of the sensor, returned if the sensor is read when in invalid state
    ///   (for instance before having been set).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the invalid value of the sensor, returned if the sensor is
    ///   read when in invalid state
    ///   (for instance before having been set)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YVirtualSensor.INVALIDVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_invalidValue():double;

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
    ///   Use the method <c>YVirtualSensor.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YVirtualSensor</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindVirtualSensor(func: string):TYVirtualSensor;

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
    function registerValueCallback(callback: TYVirtualSensorValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYVirtualSensorTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;

    ////
    /// <summary>
    ///   Changes the current sensor state to invalid (as if no value would have been ever set).
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_sensorAsInvalid():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of virtual sensors started using <c>yFirstVirtualSensor()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned virtual sensors order.
    ///   If you want to find a specific a virtual sensor, use <c>VirtualSensor.findVirtualSensor()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YVirtualSensor</c> object, corresponding to
    ///   a virtual sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more virtual sensors to enumerate.
    /// </returns>
    ///-
    function nextVirtualSensor():TYVirtualSensor;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstVirtualSensor():TYVirtualSensor;
  //--- (end of YVirtualSensor accessors declaration)
  end;

//--- (YVirtualSensor functions declaration)
  ////
  /// <summary>
  ///   Retrieves a virtual sensor for a given identifier.
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
  ///   This function does not require that the virtual sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YVirtualSensor.isOnline()</c> to test if the virtual sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a virtual sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the virtual sensor, for instance
  ///   <c>MyDevice.virtualSensor1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YVirtualSensor</c> object allowing you to drive the virtual sensor.
  /// </returns>
  ///-
  function yFindVirtualSensor(func:string):TYVirtualSensor;
  ////
  /// <summary>
  ///   Starts the enumeration of virtual sensors currently accessible.
  /// <para>
  ///   Use the method <c>YVirtualSensor.nextVirtualSensor()</c> to iterate on
  ///   next virtual sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YVirtualSensor</c> object, corresponding to
  ///   the first virtual sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstVirtualSensor():TYVirtualSensor;

//--- (end of YVirtualSensor functions declaration)

implementation

//--- (YVirtualSensor dlldef)
//--- (end of YVirtualSensor dlldef)

  constructor TYVirtualSensor.Create(func:string);
    begin
      inherited Create(func);
      _className := 'VirtualSensor';
      //--- (YVirtualSensor accessors initialization)
      _invalidValue := Y_INVALIDVALUE_INVALID;
      _valueCallbackVirtualSensor := nil;
      _timedReportCallbackVirtualSensor := nil;
      //--- (end of YVirtualSensor accessors initialization)
    end;

//--- (YVirtualSensor yapiwrapper)
//--- (end of YVirtualSensor yapiwrapper)

//--- (YVirtualSensor implementation)
{$HINTS OFF}
  function TYVirtualSensor._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'invalidValue') then
        begin
          _invalidValue := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYVirtualSensor.set_unit(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('unit',rest_val);
    end;

  function TYVirtualSensor.set_currentRawValue(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('currentRawValue',rest_val);
    end;

  function TYVirtualSensor.set_sensorState(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('sensorState',rest_val);
    end;

  function TYVirtualSensor.set_invalidValue(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('invalidValue',rest_val);
    end;

  function TYVirtualSensor.get_invalidValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_INVALIDVALUE_INVALID;
              exit;
            end;
        end;
      res := self._invalidValue;
      result := res;
      exit;
    end;


  class function TYVirtualSensor.FindVirtualSensor(func: string):TYVirtualSensor;
    var
      obj : TYVirtualSensor;
    begin
      obj := TYVirtualSensor(TYFunction._FindFromCache('VirtualSensor', func));
      if (obj = nil) then
        begin
          obj :=  TYVirtualSensor.create(func);
          TYFunction._AddToCache('VirtualSensor', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYVirtualSensor.registerValueCallback(callback: TYVirtualSensorValueCallback):LongInt;
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
      self._valueCallbackVirtualSensor := callback;
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


  function TYVirtualSensor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackVirtualSensor) <> nil) then
        begin
          self._valueCallbackVirtualSensor(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYVirtualSensor.registerTimedReportCallback(callback: TYVirtualSensorTimedReportCallback):LongInt;
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
      self._timedReportCallbackVirtualSensor := callback;
      result := 0;
      exit;
    end;


  function TYVirtualSensor._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackVirtualSensor) <> nil) then
        begin
          self._timedReportCallbackVirtualSensor(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYVirtualSensor.set_sensorAsInvalid():LongInt;
    begin
      result := self.set_sensorState(1);
      exit;
    end;


  function TYVirtualSensor.nextVirtualSensor(): TYVirtualSensor;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextVirtualSensor := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextVirtualSensor := nil;
          exit;
        end;
      nextVirtualSensor := TYVirtualSensor.FindVirtualSensor(hwid);
    end;

  class function TYVirtualSensor.FirstVirtualSensor(): TYVirtualSensor;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('VirtualSensor', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYVirtualSensor.FindVirtualSensor(serial+'.'+funcId);
    end;

//--- (end of YVirtualSensor implementation)

//--- (YVirtualSensor functions)

  function yFindVirtualSensor(func:string): TYVirtualSensor;
    begin
      result := TYVirtualSensor.FindVirtualSensor(func);
    end;

  function yFirstVirtualSensor(): TYVirtualSensor;
    begin
      result := TYVirtualSensor.FirstVirtualSensor();
    end;

  procedure _VirtualSensorCleanup();
    begin
    end;

//--- (end of YVirtualSensor functions)

initialization
  //--- (YVirtualSensor initialization)
  //--- (end of YVirtualSensor initialization)

finalization
  //--- (YVirtualSensor cleanup)
  _VirtualSensorCleanup();
  //--- (end of YVirtualSensor cleanup)

end.
