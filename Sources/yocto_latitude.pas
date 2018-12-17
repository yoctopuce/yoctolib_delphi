{*********************************************************************
 *
 *  $Id: yocto_latitude.pas 33711 2018-12-14 14:19:13Z seb $
 *
 *  Implements yFindLatitude(), the high-level API for Latitude functions
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


unit yocto_latitude;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YLatitude definitions)



//--- (end of YLatitude definitions)
//--- (YLatitude yapiwrapper declaration)
//--- (end of YLatitude yapiwrapper declaration)

type
  TYLatitude = class;
  //--- (YLatitude class start)
  TYLatitudeValueCallback = procedure(func: TYLatitude; value:string);
  TYLatitudeTimedReportCallback = procedure(func: TYLatitude; value:TYMeasure);

  ////
  /// <summary>
  ///   TYLatitude Class: Latitude function interface
  /// <para>
  ///   The Yoctopuce class YLatitude allows you to read the latitude from Yoctopuce
  ///   geolocation sensors. It inherits from the YSensor class the core functions to
  ///   read measurements, to register callback functions, to access the autonomous
  ///   datalogger.
  /// </para>
  /// </summary>
  ///-
  TYLatitude=class(TYSensor)
  //--- (end of YLatitude class start)
  protected
  //--- (YLatitude declaration)
    // Attributes (function value cache)
    _valueCallbackLatitude    : TYLatitudeValueCallback;
    _timedReportCallbackLatitude : TYLatitudeTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YLatitude declaration)

  public
    //--- (YLatitude accessors declaration)
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
    ///   Use the method <c>YLatitude.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YLatitude</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindLatitude(func: string):TYLatitude;

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
    function registerValueCallback(callback: TYLatitudeValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYLatitudeTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of latitude sensors started using <c>yFirstLatitude()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned latitude sensors order.
    ///   If you want to find a specific a latitude sensor, use <c>Latitude.findLatitude()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YLatitude</c> object, corresponding to
    ///   a latitude sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more latitude sensors to enumerate.
    /// </returns>
    ///-
    function nextLatitude():TYLatitude;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstLatitude():TYLatitude;
  //--- (end of YLatitude accessors declaration)
  end;

//--- (YLatitude functions declaration)
  ////
  /// <summary>
  ///   Retrieves a latitude sensor for a given identifier.
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
  ///   This function does not require that the latitude sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YLatitude.isOnline()</c> to test if the latitude sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a latitude sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the latitude sensor
  /// </param>
  /// <returns>
  ///   a <c>YLatitude</c> object allowing you to drive the latitude sensor.
  /// </returns>
  ///-
  function yFindLatitude(func:string):TYLatitude;
  ////
  /// <summary>
  ///   Starts the enumeration of latitude sensors currently accessible.
  /// <para>
  ///   Use the method <c>YLatitude.nextLatitude()</c> to iterate on
  ///   next latitude sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YLatitude</c> object, corresponding to
  ///   the first latitude sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstLatitude():TYLatitude;

//--- (end of YLatitude functions declaration)

implementation
//--- (YLatitude dlldef)
//--- (end of YLatitude dlldef)

  constructor TYLatitude.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Latitude';
      //--- (YLatitude accessors initialization)
      _valueCallbackLatitude := nil;
      _timedReportCallbackLatitude := nil;
      //--- (end of YLatitude accessors initialization)
    end;

//--- (YLatitude yapiwrapper)
//--- (end of YLatitude yapiwrapper)

//--- (YLatitude implementation)
{$HINTS OFF}
  function TYLatitude._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  class function TYLatitude.FindLatitude(func: string):TYLatitude;
    var
      obj : TYLatitude;
    begin
      obj := TYLatitude(TYFunction._FindFromCache('Latitude', func));
      if obj = nil then
        begin
          obj :=  TYLatitude.create(func);
          TYFunction._AddToCache('Latitude',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYLatitude.registerValueCallback(callback: TYLatitudeValueCallback):LongInt;
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
      self._valueCallbackLatitude := callback;
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


  function TYLatitude._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackLatitude) <> nil) then
        begin
          self._valueCallbackLatitude(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYLatitude.registerTimedReportCallback(callback: TYLatitudeTimedReportCallback):LongInt;
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
      self._timedReportCallbackLatitude := callback;
      result := 0;
      exit;
    end;


  function TYLatitude._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackLatitude) <> nil) then
        begin
          self._timedReportCallbackLatitude(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYLatitude.nextLatitude(): TYLatitude;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextLatitude := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextLatitude := nil;
          exit;
        end;
      nextLatitude := TYLatitude.FindLatitude(hwid);
    end;

  class function TYLatitude.FirstLatitude(): TYLatitude;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Latitude', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYLatitude.FindLatitude(serial+'.'+funcId);
    end;

//--- (end of YLatitude implementation)

//--- (YLatitude functions)

  function yFindLatitude(func:string): TYLatitude;
    begin
      result := TYLatitude.FindLatitude(func);
    end;

  function yFirstLatitude(): TYLatitude;
    begin
      result := TYLatitude.FirstLatitude();
    end;

  procedure _LatitudeCleanup();
    begin
    end;

//--- (end of YLatitude functions)

initialization
  //--- (YLatitude initialization)
  //--- (end of YLatitude initialization)

finalization
  //--- (YLatitude cleanup)
  _LatitudeCleanup();
  //--- (end of YLatitude cleanup)
end.
