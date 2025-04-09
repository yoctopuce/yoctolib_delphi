{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindPressure(), the high-level API for Pressure functions
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


unit yocto_pressure;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YPressure definitions)


//--- (end of YPressure definitions)

//--- (YPressure yapiwrapper declaration)
//--- (end of YPressure yapiwrapper declaration)

type

  TYPressure = class;
  //--- (YPressure class start)
  TYPressureValueCallback = procedure(func: TYPressure; value:string);
  TYPressureTimedReportCallback = procedure(func: TYPressure; value:TYMeasure);

  ////
  /// <summary>
  ///   TYPressure Class: pressure sensor control interface, available for instance in the
  ///   Yocto-Altimeter-V2, the Yocto-CO2-V2, the Yocto-Meteo-V2 or the Yocto-Pressure
  /// <para>
  ///   The <c>YPressure</c> class allows you to read and configure Yoctopuce pressure sensors.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYPressure=class(TYSensor)
  //--- (end of YPressure class start)
  protected
  //--- (YPressure declaration)
    // Attributes (function value cache)
    _valueCallbackPressure    : TYPressureValueCallback;
    _timedReportCallbackPressure : TYPressureTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YPressure declaration)

  public
    //--- (YPressure accessors declaration)
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
    ///   Use the method <c>YPressure.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YPressure</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindPressure(func: string):TYPressure;

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
    function registerValueCallback(callback: TYPressureValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYPressureTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of pressure sensors started using <c>yFirstPressure()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned pressure sensors order.
    ///   If you want to find a specific a pressure sensor, use <c>Pressure.findPressure()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YPressure</c> object, corresponding to
    ///   a pressure sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more pressure sensors to enumerate.
    /// </returns>
    ///-
    function nextPressure():TYPressure;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstPressure():TYPressure;
  //--- (end of YPressure accessors declaration)
  end;

//--- (YPressure functions declaration)
  ////
  /// <summary>
  ///   Retrieves a pressure sensor for a given identifier.
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
  ///   This function does not require that the pressure sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YPressure.isOnline()</c> to test if the pressure sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a pressure sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the pressure sensor, for instance
  ///   <c>YALTIMK2.pressure</c>.
  /// </param>
  /// <returns>
  ///   a <c>YPressure</c> object allowing you to drive the pressure sensor.
  /// </returns>
  ///-
  function yFindPressure(func:string):TYPressure;
  ////
  /// <summary>
  ///   Starts the enumeration of pressure sensors currently accessible.
  /// <para>
  ///   Use the method <c>YPressure.nextPressure()</c> to iterate on
  ///   next pressure sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YPressure</c> object, corresponding to
  ///   the first pressure sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstPressure():TYPressure;

//--- (end of YPressure functions declaration)

implementation

//--- (YPressure dlldef)
//--- (end of YPressure dlldef)

  constructor TYPressure.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Pressure';
      //--- (YPressure accessors initialization)
      _valueCallbackPressure := nil;
      _timedReportCallbackPressure := nil;
      //--- (end of YPressure accessors initialization)
    end;

//--- (YPressure yapiwrapper)
//--- (end of YPressure yapiwrapper)

//--- (YPressure implementation)
{$HINTS OFF}
  function TYPressure._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  class function TYPressure.FindPressure(func: string):TYPressure;
    var
      obj : TYPressure;
    begin
      obj := TYPressure(TYFunction._FindFromCache('Pressure', func));
      if obj = nil then
        begin
          obj :=  TYPressure.create(func);
          TYFunction._AddToCache('Pressure', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYPressure.registerValueCallback(callback: TYPressureValueCallback):LongInt;
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
      self._valueCallbackPressure := callback;
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


  function TYPressure._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackPressure) <> nil) then
        begin
          self._valueCallbackPressure(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYPressure.registerTimedReportCallback(callback: TYPressureTimedReportCallback):LongInt;
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
      self._timedReportCallbackPressure := callback;
      result := 0;
      exit;
    end;


  function TYPressure._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackPressure) <> nil) then
        begin
          self._timedReportCallbackPressure(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYPressure.nextPressure(): TYPressure;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextPressure := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextPressure := nil;
          exit;
        end;
      nextPressure := TYPressure.FindPressure(hwid);
    end;

  class function TYPressure.FirstPressure(): TYPressure;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Pressure', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYPressure.FindPressure(serial+'.'+funcId);
    end;

//--- (end of YPressure implementation)

//--- (YPressure functions)

  function yFindPressure(func:string): TYPressure;
    begin
      result := TYPressure.FindPressure(func);
    end;

  function yFirstPressure(): TYPressure;
    begin
      result := TYPressure.FirstPressure();
    end;

  procedure _PressureCleanup();
    begin
    end;

//--- (end of YPressure functions)

initialization
  //--- (YPressure initialization)
  //--- (end of YPressure initialization)

finalization
  //--- (YPressure cleanup)
  _PressureCleanup();
  //--- (end of YPressure cleanup)

end.
