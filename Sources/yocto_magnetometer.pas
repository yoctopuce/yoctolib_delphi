{*********************************************************************
 *
 * $Id: yocto_magnetometer.pas 21551 2015-09-17 16:50:38Z seb $
 *
 * Implements yFindMagnetometer(), the high-level API for Magnetometer functions
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


unit yocto_magnetometer;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YMagnetometer definitions)

const Y_XVALUE_INVALID                = YAPI_INVALID_DOUBLE;
const Y_YVALUE_INVALID                = YAPI_INVALID_DOUBLE;
const Y_ZVALUE_INVALID                = YAPI_INVALID_DOUBLE;


//--- (end of YMagnetometer definitions)

type
  TYMagnetometer = class;
  //--- (YMagnetometer class start)
  TYMagnetometerValueCallback = procedure(func: TYMagnetometer; value:string);
  TYMagnetometerTimedReportCallback = procedure(func: TYMagnetometer; value:TYMeasure);

  ////
  /// <summary>
  ///   TYMagnetometer Class: Magnetometer function interface
  /// <para>
  ///   The YSensor class is the parent class for all Yoctopuce sensors. It can be
  ///   used to read the current value and unit of any sensor, read the min/max
  ///   value, configure autonomous recording frequency and access recorded data.
  ///   It also provide a function to register a callback invoked each time the
  ///   observed value changes, or at a predefined interval. Using this class rather
  ///   than a specific subclass makes it possible to create generic applications
  ///   that work with any Yoctopuce sensor, even those that do not yet exist.
  ///   Note: The YAnButton class is the only analog input which does not inherit
  ///   from YSensor.
  /// </para>
  /// </summary>
  ///-
  TYMagnetometer=class(TYSensor)
  //--- (end of YMagnetometer class start)
  protected
  //--- (YMagnetometer declaration)
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
    ///   On failure, throws an exception or returns <c>Y_XVALUE_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_YVALUE_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_ZVALUE_INVALID</c>.
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
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$
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
    function registerValueCallback(callback: TYMagnetometerValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYMagnetometerTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of magnetometers started using <c>yFirstMagnetometer()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YMagnetometer</c> object, corresponding to
    ///   a magnetometer currently online, or a <c>null</c> pointer
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

//--- (Magnetometer functions declaration)
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
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the magnetometer
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
  ///   the first magnetometer currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstMagnetometer():TYMagnetometer;

//--- (end of Magnetometer functions declaration)

implementation
//--- (YMagnetometer dlldef)
//--- (end of YMagnetometer dlldef)

  constructor TYMagnetometer.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Magnetometer';
      //--- (YMagnetometer accessors initialization)
      _xValue := Y_XVALUE_INVALID;
      _yValue := Y_YVALUE_INVALID;
      _zValue := Y_ZVALUE_INVALID;
      _valueCallbackMagnetometer := nil;
      _timedReportCallbackMagnetometer := nil;
      //--- (end of YMagnetometer accessors initialization)
    end;


//--- (YMagnetometer implementation)
{$HINTS OFF}
  function TYMagnetometer._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'xValue') then
        begin
          _xValue := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'yValue') then
        begin
          _yValue := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'zValue') then
        begin
          _zValue := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

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
  ///   On failure, throws an exception or returns Y_XVALUE_INVALID.
  /// </para>
  ///-
  function TYMagnetometer.get_xValue():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_XVALUE_INVALID;
              exit;
            end;
        end;
      result := self._xValue;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_YVALUE_INVALID.
  /// </para>
  ///-
  function TYMagnetometer.get_yValue():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_YVALUE_INVALID;
              exit;
            end;
        end;
      result := self._yValue;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_ZVALUE_INVALID.
  /// </para>
  ///-
  function TYMagnetometer.get_zValue():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ZVALUE_INVALID;
              exit;
            end;
        end;
      result := self._zValue;
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
  ///   Use the method <c>YMagnetometer.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YMagnetometer</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
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
  function TYMagnetometer.registerTimedReportCallback(callback: TYMagnetometerTimedReportCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(self, true);
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(self, false);
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

//--- (Magnetometer functions)

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

//--- (end of Magnetometer functions)

initialization
  //--- (Magnetometer initialization)
  //--- (end of Magnetometer initialization)

finalization
  //--- (Magnetometer cleanup)
  _MagnetometerCleanup();
  //--- (end of Magnetometer cleanup)
end.
