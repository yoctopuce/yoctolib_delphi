{*********************************************************************
 *
 * $Id: yocto_compass.pas 22695 2016-01-12 23:13:53Z seb $
 *
 * Implements yFindCompass(), the high-level API for Compass functions
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


unit yocto_compass;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YCompass definitions)

const Y_AXIS_X = 0;
const Y_AXIS_Y = 1;
const Y_AXIS_Z = 2;
const Y_AXIS_INVALID = -1;
const Y_MAGNETICHEADING_INVALID       = YAPI_INVALID_DOUBLE;


//--- (end of YCompass definitions)

type
  TYCompass = class;
  //--- (YCompass class start)
  TYCompassValueCallback = procedure(func: TYCompass; value:string);
  TYCompassTimedReportCallback = procedure(func: TYCompass; value:TYMeasure);

  ////
  /// <summary>
  ///   TYCompass Class: Compass function interface
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
  TYCompass=class(TYSensor)
  //--- (end of YCompass class start)
  protected
  //--- (YCompass declaration)
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
    _axis                     : Integer;
    _magneticHeading          : double;
    _valueCallbackCompass     : TYCompassValueCallback;
    _timedReportCallbackCompass : TYCompassTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YCompass declaration)

  public
    //--- (YCompass accessors declaration)
    constructor Create(func:string);

    function get_axis():Integer;

    ////
    /// <summary>
    ///   Returns the magnetic heading, regardless of the configured bearing.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the magnetic heading, regardless of the configured bearing
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MAGNETICHEADING_INVALID</c>.
    /// </para>
    ///-
    function get_magneticHeading():double;

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
    ///   Use the method <c>YCompass.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YCompass</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindCompass(func: string):TYCompass;

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
    function registerValueCallback(callback: TYCompassValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYCompassTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of compasses started using <c>yFirstCompass()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YCompass</c> object, corresponding to
    ///   a compass currently online, or a <c>null</c> pointer
    ///   if there are no more compasses to enumerate.
    /// </returns>
    ///-
    function nextCompass():TYCompass;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstCompass():TYCompass;
  //--- (end of YCompass accessors declaration)
  end;

//--- (Compass functions declaration)
  ////
  /// <summary>
  ///   Retrieves a compass for a given identifier.
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
  ///   This function does not require that the compass is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YCompass.isOnline()</c> to test if the compass is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a compass by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the compass
  /// </param>
  /// <returns>
  ///   a <c>YCompass</c> object allowing you to drive the compass.
  /// </returns>
  ///-
  function yFindCompass(func:string):TYCompass;
  ////
  /// <summary>
  ///   Starts the enumeration of compasses currently accessible.
  /// <para>
  ///   Use the method <c>YCompass.nextCompass()</c> to iterate on
  ///   next compasses.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YCompass</c> object, corresponding to
  ///   the first compass currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstCompass():TYCompass;

//--- (end of Compass functions declaration)

implementation
//--- (YCompass dlldef)
//--- (end of YCompass dlldef)

  constructor TYCompass.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Compass';
      //--- (YCompass accessors initialization)
      _axis := Y_AXIS_INVALID;
      _magneticHeading := Y_MAGNETICHEADING_INVALID;
      _valueCallbackCompass := nil;
      _timedReportCallbackCompass := nil;
      //--- (end of YCompass accessors initialization)
    end;


//--- (YCompass implementation)
{$HINTS OFF}
  function TYCompass._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'axis') then
        begin
          _axis := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'magneticHeading') then
        begin
          _magneticHeading := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYCompass.get_axis():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_AXIS_INVALID;
              exit;
            end;
        end;
      result := self._axis;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the magnetic heading, regardless of the configured bearing.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the magnetic heading, regardless of the configured bearing
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_MAGNETICHEADING_INVALID.
  /// </para>
  ///-
  function TYCompass.get_magneticHeading():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MAGNETICHEADING_INVALID;
              exit;
            end;
        end;
      result := self._magneticHeading;
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
  ///   Use the method <c>YCompass.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YCompass</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYCompass.FindCompass(func: string):TYCompass;
    var
      obj : TYCompass;
    begin
      obj := TYCompass(TYFunction._FindFromCache('Compass', func));
      if obj = nil then
        begin
          obj :=  TYCompass.create(func);
          TYFunction._AddToCache('Compass',  func, obj);
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
  function TYCompass.registerValueCallback(callback: TYCompassValueCallback):LongInt;
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
      self._valueCallbackCompass := callback;
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


  function TYCompass._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackCompass) <> nil) then
        begin
          self._valueCallbackCompass(self, value);
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
  function TYCompass.registerTimedReportCallback(callback: TYCompassTimedReportCallback):LongInt;
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
      self._timedReportCallbackCompass := callback;
      result := 0;
      exit;
    end;


  function TYCompass._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackCompass) <> nil) then
        begin
          self._timedReportCallbackCompass(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYCompass.nextCompass(): TYCompass;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextCompass := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextCompass := nil;
          exit;
        end;
      nextCompass := TYCompass.FindCompass(hwid);
    end;

  class function TYCompass.FirstCompass(): TYCompass;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Compass', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYCompass.FindCompass(serial+'.'+funcId);
    end;

//--- (end of YCompass implementation)

//--- (Compass functions)

  function yFindCompass(func:string): TYCompass;
    begin
      result := TYCompass.FindCompass(func);
    end;

  function yFirstCompass(): TYCompass;
    begin
      result := TYCompass.FirstCompass();
    end;

  procedure _CompassCleanup();
    begin
    end;

//--- (end of Compass functions)

initialization
  //--- (Compass initialization)
  //--- (end of Compass initialization)

finalization
  //--- (Compass cleanup)
  _CompassCleanup();
  //--- (end of Compass cleanup)
end.
