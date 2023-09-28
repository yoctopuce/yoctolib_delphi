{*********************************************************************
 *
 *  $Id: yocto_magnetometer.pas 56084 2023-08-15 16:13:01Z mvuilleu $
 *
 *  Implements yFindMagnetometer(), the high-level API for Magnetometer functions
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


unit yocto_magnetometer;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YMagnetometer definitions)

const Y_BANDWIDTH_INVALID             = YAPI_INVALID_UINT;
const Y_XVALUE_INVALID                = YAPI_INVALID_DOUBLE;
const Y_YVALUE_INVALID                = YAPI_INVALID_DOUBLE;
const Y_ZVALUE_INVALID                = YAPI_INVALID_DOUBLE;

//--- (end of YMagnetometer definitions)

//--- (YMagnetometer yapiwrapper declaration)
//--- (end of YMagnetometer yapiwrapper declaration)

type

  TYMagnetometer = class;
  //--- (YMagnetometer class start)
  TYMagnetometerValueCallback = procedure(func: TYMagnetometer; value:string);
  TYMagnetometerTimedReportCallback = procedure(func: TYMagnetometer; value:TYMeasure);

  ////
  /// <summary>
  ///   TYMagnetometer Class: magnetometer control interface, available for instance in the Yocto-3D-V2
  /// <para>
  ///   The <c>YSensor</c> class is the parent class for all Yoctopuce sensor types. It can be
  ///   used to read the current value and unit of any sensor, read the min/max
  ///   value, configure autonomous recording frequency and access recorded data.
  ///   It also provide a function to register a callback invoked each time the
  ///   observed value changes, or at a predefined interval. Using this class rather
  ///   than a specific subclass makes it possible to create generic applications
  ///   that work with any Yoctopuce sensor, even those that do not yet exist.
  ///   Note: The <c>YAnButton</c> class is the only analog input which does not inherit
  ///   from <c>YSensor</c>.
  /// </para>
  /// </summary>
  ///-
  TYMagnetometer=class(TYSensor)
  //--- (end of YMagnetometer class start)
  protected
  //--- (YMagnetometer declaration)
    // Attributes (function value cache)
    _bandwidth                : LongInt;
    _xValue                   : double;
    _yValue                   : double;
    _zValue                   : double;
    _valueCallbackMagnetometer : TYMagnetometerValueCallback;
    _timedReportCallbackMagnetometer : TYMagnetometerTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YMagnetometer declaration)

  public
    //--- (YMagnetometer accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the measure update frequency, measured in Hz.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the measure update frequency, measured in Hz
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMagnetometer.BANDWIDTH_INVALID</c>.
    /// </para>
    ///-
    function get_bandwidth():LongInt;

    ////
    /// <summary>
    ///   Changes the measure update frequency, measured in Hz.
    /// <para>
    ///   When the
    ///   frequency is lower, the device performs averaging.
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the measure update frequency, measured in Hz
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
    function set_bandwidth(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the X component of the magnetic field, as a floating point number.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the X component of the magnetic field, as a floating point number
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMagnetometer.XVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_xValue():double;

    ////
    /// <summary>
    ///   Returns the Y component of the magnetic field, as a floating point number.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the Y component of the magnetic field, as a floating point number
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMagnetometer.YVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_yValue():double;

    ////
    /// <summary>
    ///   Returns the Z component of the magnetic field, as a floating point number.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the Z component of the magnetic field, as a floating point number
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMagnetometer.ZVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_zValue():double;

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
    ///   Use the method <c>YMagnetometer.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YMagnetometer</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindMagnetometer(func: string):TYMagnetometer;

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
    function registerValueCallback(callback: TYMagnetometerValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYMagnetometerTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of magnetometers started using <c>yFirstMagnetometer()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned magnetometers order.
    ///   If you want to find a specific a magnetometer, use <c>Magnetometer.findMagnetometer()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YMagnetometer</c> object, corresponding to
    ///   a magnetometer currently online, or a <c>NIL</c> pointer
    ///   if there are no more magnetometers to enumerate.
    /// </returns>
    ///-
    function nextMagnetometer():TYMagnetometer;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstMagnetometer():TYMagnetometer;
  //--- (end of YMagnetometer accessors declaration)
  end;

//--- (YMagnetometer functions declaration)
  ////
  /// <summary>
  ///   Retrieves a magnetometer for a given identifier.
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
  ///   This function does not require that the magnetometer is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YMagnetometer.isOnline()</c> to test if the magnetometer is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a magnetometer by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the magnetometer, for instance
  ///   <c>Y3DMK002.magnetometer</c>.
  /// </param>
  /// <returns>
  ///   a <c>YMagnetometer</c> object allowing you to drive the magnetometer.
  /// </returns>
  ///-
  function yFindMagnetometer(func:string):TYMagnetometer;
  ////
  /// <summary>
  ///   Starts the enumeration of magnetometers currently accessible.
  /// <para>
  ///   Use the method <c>YMagnetometer.nextMagnetometer()</c> to iterate on
  ///   next magnetometers.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YMagnetometer</c> object, corresponding to
  ///   the first magnetometer currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstMagnetometer():TYMagnetometer;

//--- (end of YMagnetometer functions declaration)

implementation

//--- (YMagnetometer dlldef)
//--- (end of YMagnetometer dlldef)

  constructor TYMagnetometer.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Magnetometer';
      //--- (YMagnetometer accessors initialization)
      _bandwidth := Y_BANDWIDTH_INVALID;
      _xValue := Y_XVALUE_INVALID;
      _yValue := Y_YVALUE_INVALID;
      _zValue := Y_ZVALUE_INVALID;
      _valueCallbackMagnetometer := nil;
      _timedReportCallbackMagnetometer := nil;
      //--- (end of YMagnetometer accessors initialization)
    end;

//--- (YMagnetometer yapiwrapper)
//--- (end of YMagnetometer yapiwrapper)

//--- (YMagnetometer implementation)
{$HINTS OFF}
  function TYMagnetometer._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'bandwidth') then
        begin
          _bandwidth := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'xValue') then
        begin
          _xValue := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'yValue') then
        begin
          _yValue := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'zValue') then
        begin
          _zValue := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYMagnetometer.get_bandwidth():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BANDWIDTH_INVALID;
              exit;
            end;
        end;
      res := self._bandwidth;
      result := res;
      exit;
    end;


  function TYMagnetometer.set_bandwidth(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('bandwidth',rest_val);
    end;

  function TYMagnetometer.get_xValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_XVALUE_INVALID;
              exit;
            end;
        end;
      res := self._xValue;
      result := res;
      exit;
    end;


  function TYMagnetometer.get_yValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_YVALUE_INVALID;
              exit;
            end;
        end;
      res := self._yValue;
      result := res;
      exit;
    end;


  function TYMagnetometer.get_zValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ZVALUE_INVALID;
              exit;
            end;
        end;
      res := self._zValue;
      result := res;
      exit;
    end;


  class function TYMagnetometer.FindMagnetometer(func: string):TYMagnetometer;
    var
      obj : TYMagnetometer;
    begin
      obj := TYMagnetometer(TYFunction._FindFromCache('Magnetometer', func));
      if obj = nil then
        begin
          obj :=  TYMagnetometer.create(func);
          TYFunction._AddToCache('Magnetometer',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYMagnetometer.registerValueCallback(callback: TYMagnetometerValueCallback):LongInt;
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
      self._valueCallbackMagnetometer := callback;
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


  function TYMagnetometer._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackMagnetometer) <> nil) then
        begin
          self._valueCallbackMagnetometer(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYMagnetometer.registerTimedReportCallback(callback: TYMagnetometerTimedReportCallback):LongInt;
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
      self._timedReportCallbackMagnetometer := callback;
      result := 0;
      exit;
    end;


  function TYMagnetometer._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackMagnetometer) <> nil) then
        begin
          self._timedReportCallbackMagnetometer(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYMagnetometer.nextMagnetometer(): TYMagnetometer;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextMagnetometer := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextMagnetometer := nil;
          exit;
        end;
      nextMagnetometer := TYMagnetometer.FindMagnetometer(hwid);
    end;

  class function TYMagnetometer.FirstMagnetometer(): TYMagnetometer;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Magnetometer', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYMagnetometer.FindMagnetometer(serial+'.'+funcId);
    end;

//--- (end of YMagnetometer implementation)

//--- (YMagnetometer functions)

  function yFindMagnetometer(func:string): TYMagnetometer;
    begin
      result := TYMagnetometer.FindMagnetometer(func);
    end;

  function yFirstMagnetometer(): TYMagnetometer;
    begin
      result := TYMagnetometer.FirstMagnetometer();
    end;

  procedure _MagnetometerCleanup();
    begin
    end;

//--- (end of YMagnetometer functions)

initialization
  //--- (YMagnetometer initialization)
  //--- (end of YMagnetometer initialization)

finalization
  //--- (YMagnetometer cleanup)
  _MagnetometerCleanup();
  //--- (end of YMagnetometer cleanup)

end.
