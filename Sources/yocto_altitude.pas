{*********************************************************************
 *
 *  $Id: yocto_altitude.pas 32610 2018-10-10 06:52:20Z seb $
 *
 *  Implements yFindAltitude(), the high-level API for Altitude functions
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


unit yocto_altitude;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YAltitude definitions)

const Y_QNH_INVALID                   = YAPI_INVALID_DOUBLE;
const Y_TECHNOLOGY_INVALID            = YAPI_INVALID_STRING;


//--- (end of YAltitude definitions)
//--- (YAltitude yapiwrapper declaration)
//--- (end of YAltitude yapiwrapper declaration)

type
  TYAltitude = class;
  //--- (YAltitude class start)
  TYAltitudeValueCallback = procedure(func: TYAltitude; value:string);
  TYAltitudeTimedReportCallback = procedure(func: TYAltitude; value:TYMeasure);

  ////
  /// <summary>
  ///   TYAltitude Class: Altitude function interface
  /// <para>
  ///   The Yoctopuce class YAltitude allows you to read and configure Yoctopuce altitude
  ///   sensors. It inherits from the YSensor class the core functions to read measurements,
  ///   to register callback functions, to access the autonomous datalogger.
  ///   This class adds the ability to configure the barometric pressure adjusted to
  ///   sea level (QNH) for barometric sensors.
  /// </para>
  /// </summary>
  ///-
  TYAltitude=class(TYSensor)
  //--- (end of YAltitude class start)
  protected
  //--- (YAltitude declaration)
    // Attributes (function value cache)
    _qnh                      : double;
    _technology               : string;
    _valueCallbackAltitude    : TYAltitudeValueCallback;
    _timedReportCallbackAltitude : TYAltitudeTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YAltitude declaration)

  public
    //--- (YAltitude accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the current estimated altitude.
    /// <para>
    ///   This allows to compensate for
    ///   ambient pressure variations and to work in relative mode.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the current estimated altitude
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
    function set_currentValue(newval:double):integer;

    ////
    /// <summary>
    ///   Changes the barometric pressure adjusted to sea level used to compute
    ///   the altitude (QNH).
    /// <para>
    ///   This enables you to compensate for atmospheric pressure
    ///   changes due to weather conditions.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the barometric pressure adjusted to sea level used to compute
    ///   the altitude (QNH)
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
    function set_qnh(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the barometric pressure adjusted to sea level used to compute
    ///   the altitude (QNH).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the barometric pressure adjusted to sea level used to compute
    ///   the altitude (QNH)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_QNH_INVALID</c>.
    /// </para>
    ///-
    function get_qnh():double;

    ////
    /// <summary>
    ///   Returns the technology used by the sesnor to compute
    ///   altitude.
    /// <para>
    ///   Possibles values are  "barometric" and "gps"
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the technology used by the sesnor to compute
    ///   altitude
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_TECHNOLOGY_INVALID</c>.
    /// </para>
    ///-
    function get_technology():string;

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
    ///   Use the method <c>YAltitude.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YAltitude</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindAltitude(func: string):TYAltitude;

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
    function registerValueCallback(callback: TYAltitudeValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYAltitudeTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of altimeters started using <c>yFirstAltitude()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YAltitude</c> object, corresponding to
    ///   an altimeter currently online, or a <c>NIL</c> pointer
    ///   if there are no more altimeters to enumerate.
    /// </returns>
    ///-
    function nextAltitude():TYAltitude;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstAltitude():TYAltitude;
  //--- (end of YAltitude accessors declaration)
  end;

//--- (YAltitude functions declaration)
  ////
  /// <summary>
  ///   Retrieves an altimeter for a given identifier.
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
  ///   This function does not require that the altimeter is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YAltitude.isOnline()</c> to test if the altimeter is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   an altimeter by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the altimeter
  /// </param>
  /// <returns>
  ///   a <c>YAltitude</c> object allowing you to drive the altimeter.
  /// </returns>
  ///-
  function yFindAltitude(func:string):TYAltitude;
  ////
  /// <summary>
  ///   Starts the enumeration of altimeters currently accessible.
  /// <para>
  ///   Use the method <c>YAltitude.nextAltitude()</c> to iterate on
  ///   next altimeters.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YAltitude</c> object, corresponding to
  ///   the first altimeter currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstAltitude():TYAltitude;

//--- (end of YAltitude functions declaration)

implementation
//--- (YAltitude dlldef)
//--- (end of YAltitude dlldef)

  constructor TYAltitude.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Altitude';
      //--- (YAltitude accessors initialization)
      _qnh := Y_QNH_INVALID;
      _technology := Y_TECHNOLOGY_INVALID;
      _valueCallbackAltitude := nil;
      _timedReportCallbackAltitude := nil;
      //--- (end of YAltitude accessors initialization)
    end;

//--- (YAltitude yapiwrapper)
//--- (end of YAltitude yapiwrapper)

//--- (YAltitude implementation)
{$HINTS OFF}
  function TYAltitude._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'qnh') then
        begin
          _qnh := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'technology') then
        begin
          _technology := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYAltitude.set_currentValue(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('currentValue',rest_val);
    end;

  function TYAltitude.set_qnh(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('qnh',rest_val);
    end;

  function TYAltitude.get_qnh():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_QNH_INVALID;
              exit;
            end;
        end;
      res := self._qnh;
      result := res;
      exit;
    end;


  function TYAltitude.get_technology():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_TECHNOLOGY_INVALID;
              exit;
            end;
        end;
      res := self._technology;
      result := res;
      exit;
    end;


  class function TYAltitude.FindAltitude(func: string):TYAltitude;
    var
      obj : TYAltitude;
    begin
      obj := TYAltitude(TYFunction._FindFromCache('Altitude', func));
      if obj = nil then
        begin
          obj :=  TYAltitude.create(func);
          TYFunction._AddToCache('Altitude',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYAltitude.registerValueCallback(callback: TYAltitudeValueCallback):LongInt;
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
      self._valueCallbackAltitude := callback;
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


  function TYAltitude._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackAltitude) <> nil) then
        begin
          self._valueCallbackAltitude(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYAltitude.registerTimedReportCallback(callback: TYAltitudeTimedReportCallback):LongInt;
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
      self._timedReportCallbackAltitude := callback;
      result := 0;
      exit;
    end;


  function TYAltitude._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackAltitude) <> nil) then
        begin
          self._timedReportCallbackAltitude(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYAltitude.nextAltitude(): TYAltitude;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextAltitude := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextAltitude := nil;
          exit;
        end;
      nextAltitude := TYAltitude.FindAltitude(hwid);
    end;

  class function TYAltitude.FirstAltitude(): TYAltitude;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Altitude', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYAltitude.FindAltitude(serial+'.'+funcId);
    end;

//--- (end of YAltitude implementation)

//--- (YAltitude functions)

  function yFindAltitude(func:string): TYAltitude;
    begin
      result := TYAltitude.FindAltitude(func);
    end;

  function yFirstAltitude(): TYAltitude;
    begin
      result := TYAltitude.FirstAltitude();
    end;

  procedure _AltitudeCleanup();
    begin
    end;

//--- (end of YAltitude functions)

initialization
  //--- (YAltitude initialization)
  //--- (end of YAltitude initialization)

finalization
  //--- (YAltitude cleanup)
  _AltitudeCleanup();
  //--- (end of YAltitude cleanup)
end.
