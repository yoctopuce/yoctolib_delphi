{*********************************************************************
 *
 *  $Id: yocto_compass.pas 37619 2019-10-11 11:52:42Z mvuilleu $
 *
 *  Implements yFindCompass(), the high-level API for Compass functions
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


unit yocto_compass;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YCompass definitions)

const Y_BANDWIDTH_INVALID             = YAPI_INVALID_INT;
const Y_AXIS_X = 0;
const Y_AXIS_Y = 1;
const Y_AXIS_Z = 2;
const Y_AXIS_INVALID = -1;
const Y_MAGNETICHEADING_INVALID       = YAPI_INVALID_DOUBLE;


//--- (end of YCompass definitions)
//--- (YCompass yapiwrapper declaration)
//--- (end of YCompass yapiwrapper declaration)

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
    _bandwidth                : LongInt;
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

    ////
    /// <summary>
    ///   Returns the measure update frequency, measured in Hz (Yocto-3D-V2 only).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the measure update frequency, measured in Hz (Yocto-3D-V2 only)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BANDWIDTH_INVALID</c>.
    /// </para>
    ///-
    function get_bandwidth():LongInt;

    ////
    /// <summary>
    ///   Changes the measure update frequency, measured in Hz (Yocto-3D-V2 only).
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
    ///   an integer corresponding to the measure update frequency, measured in Hz (Yocto-3D-V2 only)
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
    function set_bandwidth(newval:LongInt):integer;

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
    function registerValueCallback(callback: TYCompassValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYCompassTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of compasses started using <c>yFirstCompass()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned compasses order.
    ///   If you want to find a specific a compass, use <c>Compass.findCompass()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YCompass</c> object, corresponding to
    ///   a compass currently online, or a <c>NIL</c> pointer
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

//--- (YCompass functions declaration)
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
  /// <para>
  ///   If a call to this object's is_online() method returns FALSE although
  ///   you are certain that the matching device is plugged, make sure that you did
  ///   call registerHub() at application initialization time.
  /// </para>
  /// <para>
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
  ///   the first compass currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstCompass():TYCompass;

//--- (end of YCompass functions declaration)

implementation
//--- (YCompass dlldef)
//--- (end of YCompass dlldef)

  constructor TYCompass.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Compass';
      //--- (YCompass accessors initialization)
      _bandwidth := Y_BANDWIDTH_INVALID;
      _axis := Y_AXIS_INVALID;
      _magneticHeading := Y_MAGNETICHEADING_INVALID;
      _valueCallbackCompass := nil;
      _timedReportCallbackCompass := nil;
      //--- (end of YCompass accessors initialization)
    end;

//--- (YCompass yapiwrapper)
//--- (end of YCompass yapiwrapper)

//--- (YCompass implementation)
{$HINTS OFF}
  function TYCompass._parseAttr(member:PJSONRECORD):integer;
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

  function TYCompass.get_bandwidth():LongInt;
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


  function TYCompass.set_bandwidth(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('bandwidth',rest_val);
    end;

  function TYCompass.get_axis():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_AXIS_INVALID;
              exit;
            end;
        end;
      res := self._axis;
      result := res;
      exit;
    end;


  function TYCompass.get_magneticHeading():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MAGNETICHEADING_INVALID;
              exit;
            end;
        end;
      res := self._magneticHeading;
      result := res;
      exit;
    end;


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

//--- (YCompass functions)

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

//--- (end of YCompass functions)

initialization
  //--- (YCompass initialization)
  //--- (end of YCompass initialization)

finalization
  //--- (YCompass cleanup)
  _CompassCleanup();
  //--- (end of YCompass cleanup)
end.
