{*********************************************************************
 *
 *  $Id: yocto_voltage.pas 32903 2018-11-02 10:14:32Z seb $
 *
 *  Implements yFindVoltage(), the high-level API for Voltage functions
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


unit yocto_voltage;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YVoltage definitions)

const Y_ENABLED_FALSE = 0;
const Y_ENABLED_TRUE = 1;
const Y_ENABLED_INVALID = -1;


//--- (end of YVoltage definitions)
//--- (YVoltage yapiwrapper declaration)
//--- (end of YVoltage yapiwrapper declaration)

type
  TYVoltage = class;
  //--- (YVoltage class start)
  TYVoltageValueCallback = procedure(func: TYVoltage; value:string);
  TYVoltageTimedReportCallback = procedure(func: TYVoltage; value:TYMeasure);

  ////
  /// <summary>
  ///   TYVoltage Class: Voltage function interface
  /// <para>
  ///   The Yoctopuce class YVoltage allows you to read and configure Yoctopuce voltage
  ///   sensors. It inherits from YSensor class the core functions to read measurements,
  ///   to register callback functions, to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYVoltage=class(TYSensor)
  //--- (end of YVoltage class start)
  protected
  //--- (YVoltage declaration)
    // Attributes (function value cache)
    _enabled                  : Integer;
    _valueCallbackVoltage     : TYVoltageValueCallback;
    _timedReportCallbackVoltage : TYVoltageTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YVoltage declaration)

  public
    //--- (YVoltage accessors declaration)
    constructor Create(func:string);

    function get_enabled():Integer;

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
    ///   Use the method <c>YVoltage.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YVoltage</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindVoltage(func: string):TYVoltage;

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
    function registerValueCallback(callback: TYVoltageValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYVoltageTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of voltage sensors started using <c>yFirstVoltage()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned voltage sensors order.
    ///   If you want to find a specific a voltage sensor, use <c>Voltage.findVoltage()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YVoltage</c> object, corresponding to
    ///   a voltage sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more voltage sensors to enumerate.
    /// </returns>
    ///-
    function nextVoltage():TYVoltage;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstVoltage():TYVoltage;
  //--- (end of YVoltage accessors declaration)
  end;

//--- (YVoltage functions declaration)
  ////
  /// <summary>
  ///   Retrieves a voltage sensor for a given identifier.
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
  ///   This function does not require that the voltage sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YVoltage.isOnline()</c> to test if the voltage sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a voltage sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the voltage sensor
  /// </param>
  /// <returns>
  ///   a <c>YVoltage</c> object allowing you to drive the voltage sensor.
  /// </returns>
  ///-
  function yFindVoltage(func:string):TYVoltage;
  ////
  /// <summary>
  ///   Starts the enumeration of voltage sensors currently accessible.
  /// <para>
  ///   Use the method <c>YVoltage.nextVoltage()</c> to iterate on
  ///   next voltage sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YVoltage</c> object, corresponding to
  ///   the first voltage sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstVoltage():TYVoltage;

//--- (end of YVoltage functions declaration)

implementation
//--- (YVoltage dlldef)
//--- (end of YVoltage dlldef)

  constructor TYVoltage.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Voltage';
      //--- (YVoltage accessors initialization)
      _enabled := Y_ENABLED_INVALID;
      _valueCallbackVoltage := nil;
      _timedReportCallbackVoltage := nil;
      //--- (end of YVoltage accessors initialization)
    end;

//--- (YVoltage yapiwrapper)
//--- (end of YVoltage yapiwrapper)

//--- (YVoltage implementation)
{$HINTS OFF}
  function TYVoltage._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'enabled') then
        begin
          _enabled := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYVoltage.get_enabled():Integer;
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


  function TYVoltage.set_enabled(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('enabled',rest_val);
    end;

  class function TYVoltage.FindVoltage(func: string):TYVoltage;
    var
      obj : TYVoltage;
    begin
      obj := TYVoltage(TYFunction._FindFromCache('Voltage', func));
      if obj = nil then
        begin
          obj :=  TYVoltage.create(func);
          TYFunction._AddToCache('Voltage',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYVoltage.registerValueCallback(callback: TYVoltageValueCallback):LongInt;
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
      self._valueCallbackVoltage := callback;
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


  function TYVoltage._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackVoltage) <> nil) then
        begin
          self._valueCallbackVoltage(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYVoltage.registerTimedReportCallback(callback: TYVoltageTimedReportCallback):LongInt;
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
      self._timedReportCallbackVoltage := callback;
      result := 0;
      exit;
    end;


  function TYVoltage._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackVoltage) <> nil) then
        begin
          self._timedReportCallbackVoltage(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYVoltage.nextVoltage(): TYVoltage;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextVoltage := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextVoltage := nil;
          exit;
        end;
      nextVoltage := TYVoltage.FindVoltage(hwid);
    end;

  class function TYVoltage.FirstVoltage(): TYVoltage;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Voltage', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYVoltage.FindVoltage(serial+'.'+funcId);
    end;

//--- (end of YVoltage implementation)

//--- (YVoltage functions)

  function yFindVoltage(func:string): TYVoltage;
    begin
      result := TYVoltage.FindVoltage(func);
    end;

  function yFirstVoltage(): TYVoltage;
    begin
      result := TYVoltage.FirstVoltage();
    end;

  procedure _VoltageCleanup();
    begin
    end;

//--- (end of YVoltage functions)

initialization
  //--- (YVoltage initialization)
  //--- (end of YVoltage initialization)

finalization
  //--- (YVoltage cleanup)
  _VoltageCleanup();
  //--- (end of YVoltage cleanup)
end.
