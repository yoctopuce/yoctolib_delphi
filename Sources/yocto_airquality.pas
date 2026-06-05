{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindAirQuality(), the high-level API for AirQuality functions
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


unit yocto_airquality;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YAirQuality definitions)

const Y_UBAINDEX_INVALID              = YAPI_INVALID_DOUBLE;
const Y_RELATIVEINDEX_INVALID         = YAPI_INVALID_DOUBLE;
const Y_AQIMODE_RELATIVE = 0;
const Y_AQIMODE_UBA = 1;
const Y_AQIMODE_INVALID = -1;

//--- (end of YAirQuality definitions)

//--- (YAirQuality yapiwrapper declaration)
//--- (end of YAirQuality yapiwrapper declaration)

type

  TYAirQuality = class;
  //--- (YAirQuality class start)
  TYAirQualityValueCallback = procedure(func: TYAirQuality; value:string);
  TYAirQualityTimedReportCallback = procedure(func: TYAirQuality; value:TYMeasure);

  ////
  /// <summary>
  ///   TYAirQuality Class: air quality sensor control interface
  /// <para>
  ///   The <c>YAirQuality</c> class allows you to read and configure Yoctopuce air quality sensors.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYAirQuality=class(TYSensor)
  //--- (end of YAirQuality class start)
  protected
  //--- (YAirQuality declaration)
    // Attributes (function value cache)
    _ubaIndex                 : double;
    _relativeIndex            : double;
    _aqiMode                  : Integer;
    _valueCallbackAirQuality  : TYAirQualityValueCallback;
    _timedReportCallbackAirQuality : TYAirQualityTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YAirQuality declaration)

  public
    //--- (YAirQuality accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the current air quality index, according to UBA (from 1 to 5).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current air quality index, according to UBA (from 1 to 5)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAirQuality.UBAINDEX_INVALID</c>.
    /// </para>
    ///-
    function get_ubaIndex():double;

    ////
    /// <summary>
    ///   Returns the relative air quality index, according to ScioSense (from 0 to 500).
    /// <para>
    ///   A value below 100 indicates better-than-average air quality compared to the past 24 hours,
    ///   while a value above 100 indicates poorer-than-average air quality compared to the past 24 hours.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the relative air quality index, according to ScioSense (from 0 to 500)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAirQuality.RELATIVEINDEX_INVALID</c>.
    /// </para>
    ///-
    function get_relativeIndex():double;

    ////
    /// <summary>
    ///   Returns the type of index reported by the get_currentValue function and callbacks (UBA index or relative index).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YAirQuality.AQIMODE_RELATIVE</c> or <c>YAirQuality.AQIMODE_UBA</c>, according to the type
    ///   of index reported by the get_currentValue function and callbacks (UBA index or relative index)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAirQuality.AQIMODE_INVALID</c>.
    /// </para>
    ///-
    function get_aqiMode():Integer;

    ////
    /// <summary>
    ///   Changes the the type of index reported by the get_currentValue function and callbacks (UBA index or relative index).
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YAirQuality.AQIMODE_RELATIVE</c> or <c>YAirQuality.AQIMODE_UBA</c>, according to the the
    ///   type of index reported by the get_currentValue function and callbacks (UBA index or relative index)
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
    function set_aqiMode(newval:Integer):integer;

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
    ///   Use the method <c>YAirQuality.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YAirQuality</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindAirQuality(func: string):TYAirQuality;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every change of advertised value.
    /// <para>
    ///   The callback is then invoked only during the execution of <c>ySleep</c> or <c>yHandleEvents</c>.
    ///   This provides control over the time when the callback is triggered. For good responsiveness,
    ///   remember to call one of these two functions periodically. The callback is called once juste after beeing
    ///   registered, passing the current advertised value  of the function, provided that it is not an empty string.
    ///   To unregister a callback, pass a NIL pointer as argument.
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
    function registerValueCallback(callback: TYAirQualityValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYAirQualityTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of air quality sensors started using <c>yFirstAirQuality()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned air quality sensors order.
    ///   If you want to find a specific a air quality sensor, use <c>AirQuality.findAirQuality()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YAirQuality</c> object, corresponding to
    ///   a air quality sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more air quality sensors to enumerate.
    /// </returns>
    ///-
    function nextAirQuality():TYAirQuality;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstAirQuality():TYAirQuality;
  //--- (end of YAirQuality accessors declaration)
  end;

//--- (YAirQuality functions declaration)
  ////
  /// <summary>
  ///   Retrieves a air quality sensor for a given identifier.
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
  ///   This function does not require that the air quality sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YAirQuality.isOnline()</c> to test if the air quality sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a air quality sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the air quality sensor, for instance
  ///   <c>MyDevice.airQuality</c>.
  /// </param>
  /// <returns>
  ///   a <c>YAirQuality</c> object allowing you to drive the air quality sensor.
  /// </returns>
  ///-
  function yFindAirQuality(func:string):TYAirQuality;
  ////
  /// <summary>
  ///   Starts the enumeration of air quality sensors currently accessible.
  /// <para>
  ///   Use the method <c>YAirQuality.nextAirQuality()</c> to iterate on
  ///   next air quality sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YAirQuality</c> object, corresponding to
  ///   the first air quality sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstAirQuality():TYAirQuality;

//--- (end of YAirQuality functions declaration)

implementation

//--- (YAirQuality dlldef)
//--- (end of YAirQuality dlldef)

  constructor TYAirQuality.Create(func:string);
    begin
      inherited Create(func);
      _className := 'AirQuality';
      //--- (YAirQuality accessors initialization)
      _ubaIndex := Y_UBAINDEX_INVALID;
      _relativeIndex := Y_RELATIVEINDEX_INVALID;
      _aqiMode := Y_AQIMODE_INVALID;
      _valueCallbackAirQuality := nil;
      _timedReportCallbackAirQuality := nil;
      //--- (end of YAirQuality accessors initialization)
    end;

//--- (YAirQuality yapiwrapper)
//--- (end of YAirQuality yapiwrapper)

//--- (YAirQuality implementation)
{$HINTS OFF}
  function TYAirQuality._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'ubaIndex') then
        begin
          _ubaIndex := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'relativeIndex') then
        begin
          _relativeIndex := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'aqiMode') then
        begin
          _aqiMode := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYAirQuality.get_ubaIndex():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_UBAINDEX_INVALID;
              exit;
            end;
        end;
      res := self._ubaIndex;
      result := res;
      exit;
    end;


  function TYAirQuality.get_relativeIndex():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RELATIVEINDEX_INVALID;
              exit;
            end;
        end;
      res := self._relativeIndex;
      result := res;
      exit;
    end;


  function TYAirQuality.get_aqiMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_AQIMODE_INVALID;
              exit;
            end;
        end;
      res := self._aqiMode;
      result := res;
      exit;
    end;


  function TYAirQuality.set_aqiMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('aqiMode',rest_val);
    end;

  class function TYAirQuality.FindAirQuality(func: string):TYAirQuality;
    var
      obj : TYAirQuality;
    begin
      obj := TYAirQuality(TYFunction._FindFromCache('AirQuality', func));
      if (obj = nil) then
        begin
          obj :=  TYAirQuality.create(func);
          TYFunction._AddToCache('AirQuality', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYAirQuality.registerValueCallback(callback: TYAirQualityValueCallback):LongInt;
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
      self._valueCallbackAirQuality := callback;
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


  function TYAirQuality._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackAirQuality) <> nil) then
        begin
          self._valueCallbackAirQuality(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYAirQuality.registerTimedReportCallback(callback: TYAirQualityTimedReportCallback):LongInt;
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
      self._timedReportCallbackAirQuality := callback;
      result := 0;
      exit;
    end;


  function TYAirQuality._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackAirQuality) <> nil) then
        begin
          self._timedReportCallbackAirQuality(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYAirQuality.nextAirQuality(): TYAirQuality;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextAirQuality := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextAirQuality := nil;
          exit;
        end;
      nextAirQuality := TYAirQuality.FindAirQuality(hwid);
    end;

  class function TYAirQuality.FirstAirQuality(): TYAirQuality;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('AirQuality', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYAirQuality.FindAirQuality(serial+'.'+funcId);
    end;

//--- (end of YAirQuality implementation)

//--- (YAirQuality functions)

  function yFindAirQuality(func:string): TYAirQuality;
    begin
      result := TYAirQuality.FindAirQuality(func);
    end;

  function yFirstAirQuality(): TYAirQuality;
    begin
      result := TYAirQuality.FirstAirQuality();
    end;

  procedure _AirQualityCleanup();
    begin
    end;

//--- (end of YAirQuality functions)

initialization
  //--- (YAirQuality initialization)
  //--- (end of YAirQuality initialization)

finalization
  //--- (YAirQuality cleanup)
  _AirQualityCleanup();
  //--- (end of YAirQuality cleanup)

end.
