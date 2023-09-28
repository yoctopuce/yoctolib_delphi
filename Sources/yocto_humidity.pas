{*********************************************************************
 *
 *  $Id: yocto_humidity.pas 56084 2023-08-15 16:13:01Z mvuilleu $
 *
 *  Implements yFindHumidity(), the high-level API for Humidity functions
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


unit yocto_humidity;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YHumidity definitions)

const Y_RELHUM_INVALID                = YAPI_INVALID_DOUBLE;
const Y_ABSHUM_INVALID                = YAPI_INVALID_DOUBLE;

//--- (end of YHumidity definitions)

//--- (YHumidity yapiwrapper declaration)
//--- (end of YHumidity yapiwrapper declaration)

type

  TYHumidity = class;
  //--- (YHumidity class start)
  TYHumidityValueCallback = procedure(func: TYHumidity; value:string);
  TYHumidityTimedReportCallback = procedure(func: TYHumidity; value:TYMeasure);

  ////
  /// <summary>
  ///   TYHumidity Class: humidity sensor control interface, available for instance in the Yocto-CO2-V2,
  ///   the Yocto-Meteo-V2 or the Yocto-VOC-V3
  /// <para>
  ///   The <c>YHumidity</c> class allows you to read and configure Yoctopuce humidity sensors.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYHumidity=class(TYSensor)
  //--- (end of YHumidity class start)
  protected
  //--- (YHumidity declaration)
    // Attributes (function value cache)
    _relHum                   : double;
    _absHum                   : double;
    _valueCallbackHumidity    : TYHumidityValueCallback;
    _timedReportCallbackHumidity : TYHumidityTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YHumidity declaration)

  public
    //--- (YHumidity accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the primary unit for measuring humidity.
    /// <para>
    ///   That unit is a string.
    ///   If that strings starts with the letter 'g', the primary measured value is the absolute
    ///   humidity, in g/m3. Otherwise, the primary measured value will be the relative humidity
    ///   (RH), in per cents.
    /// </para>
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification
    ///   must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the primary unit for measuring humidity
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
    ///   Returns the current relative humidity, in per cents.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current relative humidity, in per cents
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YHumidity.RELHUM_INVALID</c>.
    /// </para>
    ///-
    function get_relHum():double;

    ////
    /// <summary>
    ///   Returns the current absolute humidity, in grams per cubic meter of air.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current absolute humidity, in grams per cubic meter of air
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YHumidity.ABSHUM_INVALID</c>.
    /// </para>
    ///-
    function get_absHum():double;

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
    ///   Use the method <c>YHumidity.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YHumidity</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindHumidity(func: string):TYHumidity;

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
    function registerValueCallback(callback: TYHumidityValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYHumidityTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of humidity sensors started using <c>yFirstHumidity()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned humidity sensors order.
    ///   If you want to find a specific a humidity sensor, use <c>Humidity.findHumidity()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YHumidity</c> object, corresponding to
    ///   a humidity sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more humidity sensors to enumerate.
    /// </returns>
    ///-
    function nextHumidity():TYHumidity;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstHumidity():TYHumidity;
  //--- (end of YHumidity accessors declaration)
  end;

//--- (YHumidity functions declaration)
  ////
  /// <summary>
  ///   Retrieves a humidity sensor for a given identifier.
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
  ///   This function does not require that the humidity sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YHumidity.isOnline()</c> to test if the humidity sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a humidity sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the humidity sensor, for instance
  ///   <c>YCO2MK02.humidity</c>.
  /// </param>
  /// <returns>
  ///   a <c>YHumidity</c> object allowing you to drive the humidity sensor.
  /// </returns>
  ///-
  function yFindHumidity(func:string):TYHumidity;
  ////
  /// <summary>
  ///   Starts the enumeration of humidity sensors currently accessible.
  /// <para>
  ///   Use the method <c>YHumidity.nextHumidity()</c> to iterate on
  ///   next humidity sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YHumidity</c> object, corresponding to
  ///   the first humidity sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstHumidity():TYHumidity;

//--- (end of YHumidity functions declaration)

implementation

//--- (YHumidity dlldef)
//--- (end of YHumidity dlldef)

  constructor TYHumidity.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Humidity';
      //--- (YHumidity accessors initialization)
      _relHum := Y_RELHUM_INVALID;
      _absHum := Y_ABSHUM_INVALID;
      _valueCallbackHumidity := nil;
      _timedReportCallbackHumidity := nil;
      //--- (end of YHumidity accessors initialization)
    end;

//--- (YHumidity yapiwrapper)
//--- (end of YHumidity yapiwrapper)

//--- (YHumidity implementation)
{$HINTS OFF}
  function TYHumidity._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'relHum') then
        begin
          _relHum := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'absHum') then
        begin
          _absHum := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYHumidity.set_unit(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('unit',rest_val);
    end;

  function TYHumidity.get_relHum():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RELHUM_INVALID;
              exit;
            end;
        end;
      res := self._relHum;
      result := res;
      exit;
    end;


  function TYHumidity.get_absHum():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ABSHUM_INVALID;
              exit;
            end;
        end;
      res := self._absHum;
      result := res;
      exit;
    end;


  class function TYHumidity.FindHumidity(func: string):TYHumidity;
    var
      obj : TYHumidity;
    begin
      obj := TYHumidity(TYFunction._FindFromCache('Humidity', func));
      if obj = nil then
        begin
          obj :=  TYHumidity.create(func);
          TYFunction._AddToCache('Humidity',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYHumidity.registerValueCallback(callback: TYHumidityValueCallback):LongInt;
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
      self._valueCallbackHumidity := callback;
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


  function TYHumidity._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackHumidity) <> nil) then
        begin
          self._valueCallbackHumidity(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYHumidity.registerTimedReportCallback(callback: TYHumidityTimedReportCallback):LongInt;
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
      self._timedReportCallbackHumidity := callback;
      result := 0;
      exit;
    end;


  function TYHumidity._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackHumidity) <> nil) then
        begin
          self._timedReportCallbackHumidity(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYHumidity.nextHumidity(): TYHumidity;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextHumidity := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextHumidity := nil;
          exit;
        end;
      nextHumidity := TYHumidity.FindHumidity(hwid);
    end;

  class function TYHumidity.FirstHumidity(): TYHumidity;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Humidity', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYHumidity.FindHumidity(serial+'.'+funcId);
    end;

//--- (end of YHumidity implementation)

//--- (YHumidity functions)

  function yFindHumidity(func:string): TYHumidity;
    begin
      result := TYHumidity.FindHumidity(func);
    end;

  function yFirstHumidity(): TYHumidity;
    begin
      result := TYHumidity.FirstHumidity();
    end;

  procedure _HumidityCleanup();
    begin
    end;

//--- (end of YHumidity functions)

initialization
  //--- (YHumidity initialization)
  //--- (end of YHumidity initialization)

finalization
  //--- (YHumidity cleanup)
  _HumidityCleanup();
  //--- (end of YHumidity cleanup)

end.
