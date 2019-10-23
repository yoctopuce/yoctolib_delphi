{*********************************************************************
 *
 *  $Id: yocto_tilt.pas 37619 2019-10-11 11:52:42Z mvuilleu $
 *
 *  Implements yFindTilt(), the high-level API for Tilt functions
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


unit yocto_tilt;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YTilt definitions)

const Y_BANDWIDTH_INVALID             = YAPI_INVALID_INT;
const Y_AXIS_X = 0;
const Y_AXIS_Y = 1;
const Y_AXIS_Z = 2;
const Y_AXIS_INVALID = -1;


//--- (end of YTilt definitions)
//--- (YTilt yapiwrapper declaration)
//--- (end of YTilt yapiwrapper declaration)

type
  TYTilt = class;
  //--- (YTilt class start)
  TYTiltValueCallback = procedure(func: TYTilt; value:string);
  TYTiltTimedReportCallback = procedure(func: TYTilt; value:TYMeasure);

  ////
  /// <summary>
  ///   TYTilt Class: Tilt function interface
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
  TYTilt=class(TYSensor)
  //--- (end of YTilt class start)
  protected
  //--- (YTilt declaration)
    // Attributes (function value cache)
    _bandwidth                : LongInt;
    _axis                     : Integer;
    _valueCallbackTilt        : TYTiltValueCallback;
    _timedReportCallbackTilt  : TYTiltTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YTilt declaration)

  public
    //--- (YTilt accessors declaration)
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
    ///   Use the method <c>YTilt.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YTilt</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindTilt(func: string):TYTilt;

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
    function registerValueCallback(callback: TYTiltValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYTiltTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of tilt sensors started using <c>yFirstTilt()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned tilt sensors order.
    ///   If you want to find a specific a tilt sensor, use <c>Tilt.findTilt()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YTilt</c> object, corresponding to
    ///   a tilt sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more tilt sensors to enumerate.
    /// </returns>
    ///-
    function nextTilt():TYTilt;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstTilt():TYTilt;
  //--- (end of YTilt accessors declaration)
  end;

//--- (YTilt functions declaration)
  ////
  /// <summary>
  ///   Retrieves a tilt sensor for a given identifier.
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
  ///   This function does not require that the tilt sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YTilt.isOnline()</c> to test if the tilt sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a tilt sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the tilt sensor
  /// </param>
  /// <returns>
  ///   a <c>YTilt</c> object allowing you to drive the tilt sensor.
  /// </returns>
  ///-
  function yFindTilt(func:string):TYTilt;
  ////
  /// <summary>
  ///   Starts the enumeration of tilt sensors currently accessible.
  /// <para>
  ///   Use the method <c>YTilt.nextTilt()</c> to iterate on
  ///   next tilt sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YTilt</c> object, corresponding to
  ///   the first tilt sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstTilt():TYTilt;

//--- (end of YTilt functions declaration)

implementation
//--- (YTilt dlldef)
//--- (end of YTilt dlldef)

  constructor TYTilt.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Tilt';
      //--- (YTilt accessors initialization)
      _bandwidth := Y_BANDWIDTH_INVALID;
      _axis := Y_AXIS_INVALID;
      _valueCallbackTilt := nil;
      _timedReportCallbackTilt := nil;
      //--- (end of YTilt accessors initialization)
    end;

//--- (YTilt yapiwrapper)
//--- (end of YTilt yapiwrapper)

//--- (YTilt implementation)
{$HINTS OFF}
  function TYTilt._parseAttr(member:PJSONRECORD):integer;
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
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYTilt.get_bandwidth():LongInt;
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


  function TYTilt.set_bandwidth(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('bandwidth',rest_val);
    end;

  function TYTilt.get_axis():Integer;
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


  class function TYTilt.FindTilt(func: string):TYTilt;
    var
      obj : TYTilt;
    begin
      obj := TYTilt(TYFunction._FindFromCache('Tilt', func));
      if obj = nil then
        begin
          obj :=  TYTilt.create(func);
          TYFunction._AddToCache('Tilt',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYTilt.registerValueCallback(callback: TYTiltValueCallback):LongInt;
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
      self._valueCallbackTilt := callback;
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


  function TYTilt._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackTilt) <> nil) then
        begin
          self._valueCallbackTilt(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYTilt.registerTimedReportCallback(callback: TYTiltTimedReportCallback):LongInt;
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
      self._timedReportCallbackTilt := callback;
      result := 0;
      exit;
    end;


  function TYTilt._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackTilt) <> nil) then
        begin
          self._timedReportCallbackTilt(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYTilt.nextTilt(): TYTilt;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextTilt := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextTilt := nil;
          exit;
        end;
      nextTilt := TYTilt.FindTilt(hwid);
    end;

  class function TYTilt.FirstTilt(): TYTilt;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Tilt', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYTilt.FindTilt(serial+'.'+funcId);
    end;

//--- (end of YTilt implementation)

//--- (YTilt functions)

  function yFindTilt(func:string): TYTilt;
    begin
      result := TYTilt.FindTilt(func);
    end;

  function yFirstTilt(): TYTilt;
    begin
      result := TYTilt.FirstTilt();
    end;

  procedure _TiltCleanup();
    begin
    end;

//--- (end of YTilt functions)

initialization
  //--- (YTilt initialization)
  //--- (end of YTilt initialization)

finalization
  //--- (YTilt cleanup)
  _TiltCleanup();
  //--- (end of YTilt cleanup)
end.
