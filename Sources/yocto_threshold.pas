{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindThreshold(), the high-level API for Threshold functions
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


unit yocto_threshold;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YThreshold definitions)

const Y_THRESHOLDSTATE_SAFE = 0;
const Y_THRESHOLDSTATE_ALERT = 1;
const Y_THRESHOLDSTATE_INVALID = -1;
const Y_TARGETSENSOR_INVALID          = YAPI_INVALID_STRING;
const Y_ALERTLEVEL_INVALID            = YAPI_INVALID_DOUBLE;
const Y_SAFELEVEL_INVALID             = YAPI_INVALID_DOUBLE;

//--- (end of YThreshold definitions)

//--- (YThreshold yapiwrapper declaration)
//--- (end of YThreshold yapiwrapper declaration)

type

  TYThreshold = class;
  //--- (YThreshold class start)
  TYThresholdValueCallback = procedure(func: TYThreshold; value:string);
  TYThresholdTimedReportCallback = procedure(func: TYThreshold; value:TYMeasure);

  ////
  /// <summary>
  ///   TYThreshold Class: Control interface to define a threshold
  /// <para>
  ///   The <c>Threshold</c> class allows you define a threshold on a Yoctopuce sensor
  ///   to trigger a predefined action, on specific devices where this is implemented.
  /// </para>
  /// </summary>
  ///-
  TYThreshold=class(TYFunction)
  //--- (end of YThreshold class start)
  protected
  //--- (YThreshold declaration)
    // Attributes (function value cache)
    _thresholdState           : Integer;
    _targetSensor             : string;
    _alertLevel               : double;
    _safeLevel                : double;
    _valueCallbackThreshold   : TYThresholdValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YThreshold declaration)

  public
    //--- (YThreshold accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns current state of the threshold function.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YThreshold.THRESHOLDSTATE_SAFE</c> or <c>YThreshold.THRESHOLDSTATE_ALERT</c>, according
    ///   to current state of the threshold function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YThreshold.THRESHOLDSTATE_INVALID</c>.
    /// </para>
    ///-
    function get_thresholdState():Integer;

    ////
    /// <summary>
    ///   Returns the name of the sensor monitored by the threshold function.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the name of the sensor monitored by the threshold function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YThreshold.TARGETSENSOR_INVALID</c>.
    /// </para>
    ///-
    function get_targetSensor():string;

    ////
    /// <summary>
    ///   Changes the sensor alert level triggering the threshold function.
    /// <para>
    ///   Remember to call the matching module <c>saveToFlash()</c>
    ///   method if you want to preserve the setting after reboot.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the sensor alert level triggering the threshold function
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
    function set_alertLevel(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the sensor alert level, triggering the threshold function.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the sensor alert level, triggering the threshold function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YThreshold.ALERTLEVEL_INVALID</c>.
    /// </para>
    ///-
    function get_alertLevel():double;

    ////
    /// <summary>
    ///   Changes the sensor acceptable level for disabling the threshold function.
    /// <para>
    ///   Remember to call the matching module <c>saveToFlash()</c>
    ///   method if you want to preserve the setting after reboot.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the sensor acceptable level for disabling the threshold function
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
    function set_safeLevel(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the sensor acceptable level for disabling the threshold function.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the sensor acceptable level for disabling the threshold function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YThreshold.SAFELEVEL_INVALID</c>.
    /// </para>
    ///-
    function get_safeLevel():double;

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
    ///   Use the method <c>YThreshold.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YThreshold</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindThreshold(func: string):TYThreshold;

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
    function registerValueCallback(callback: TYThresholdValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of threshold functions started using <c>yFirstThreshold()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned threshold functions order.
    ///   If you want to find a specific a threshold function, use <c>Threshold.findThreshold()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YThreshold</c> object, corresponding to
    ///   a threshold function currently online, or a <c>NIL</c> pointer
    ///   if there are no more threshold functions to enumerate.
    /// </returns>
    ///-
    function nextThreshold():TYThreshold;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstThreshold():TYThreshold;
  //--- (end of YThreshold accessors declaration)
  end;

//--- (YThreshold functions declaration)
  ////
  /// <summary>
  ///   Retrieves a threshold function for a given identifier.
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
  ///   This function does not require that the threshold function is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YThreshold.isOnline()</c> to test if the threshold function is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a threshold function by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the threshold function, for instance
  ///   <c>MyDevice.threshold1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YThreshold</c> object allowing you to drive the threshold function.
  /// </returns>
  ///-
  function yFindThreshold(func:string):TYThreshold;
  ////
  /// <summary>
  ///   Starts the enumeration of threshold functions currently accessible.
  /// <para>
  ///   Use the method <c>YThreshold.nextThreshold()</c> to iterate on
  ///   next threshold functions.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YThreshold</c> object, corresponding to
  ///   the first threshold function currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstThreshold():TYThreshold;

//--- (end of YThreshold functions declaration)

implementation

//--- (YThreshold dlldef)
//--- (end of YThreshold dlldef)

  constructor TYThreshold.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Threshold';
      //--- (YThreshold accessors initialization)
      _thresholdState := Y_THRESHOLDSTATE_INVALID;
      _targetSensor := Y_TARGETSENSOR_INVALID;
      _alertLevel := Y_ALERTLEVEL_INVALID;
      _safeLevel := Y_SAFELEVEL_INVALID;
      _valueCallbackThreshold := nil;
      //--- (end of YThreshold accessors initialization)
    end;

//--- (YThreshold yapiwrapper)
//--- (end of YThreshold yapiwrapper)

//--- (YThreshold implementation)
{$HINTS OFF}
  function TYThreshold._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'thresholdState') then
        begin
          _thresholdState := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'targetSensor') then
        begin
          _targetSensor := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'alertLevel') then
        begin
          _alertLevel := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'safeLevel') then
        begin
          _safeLevel := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYThreshold.get_thresholdState():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_THRESHOLDSTATE_INVALID;
              exit;
            end;
        end;
      res := self._thresholdState;
      result := res;
      exit;
    end;


  function TYThreshold.get_targetSensor():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_TARGETSENSOR_INVALID;
              exit;
            end;
        end;
      res := self._targetSensor;
      result := res;
      exit;
    end;


  function TYThreshold.set_alertLevel(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('alertLevel',rest_val);
    end;

  function TYThreshold.get_alertLevel():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ALERTLEVEL_INVALID;
              exit;
            end;
        end;
      res := self._alertLevel;
      result := res;
      exit;
    end;


  function TYThreshold.set_safeLevel(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('safeLevel',rest_val);
    end;

  function TYThreshold.get_safeLevel():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SAFELEVEL_INVALID;
              exit;
            end;
        end;
      res := self._safeLevel;
      result := res;
      exit;
    end;


  class function TYThreshold.FindThreshold(func: string):TYThreshold;
    var
      obj : TYThreshold;
    begin
      obj := TYThreshold(TYFunction._FindFromCache('Threshold', func));
      if obj = nil then
        begin
          obj :=  TYThreshold.create(func);
          TYFunction._AddToCache('Threshold', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYThreshold.registerValueCallback(callback: TYThresholdValueCallback):LongInt;
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
      self._valueCallbackThreshold := callback;
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


  function TYThreshold._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackThreshold) <> nil) then
        begin
          self._valueCallbackThreshold(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYThreshold.nextThreshold(): TYThreshold;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextThreshold := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextThreshold := nil;
          exit;
        end;
      nextThreshold := TYThreshold.FindThreshold(hwid);
    end;

  class function TYThreshold.FirstThreshold(): TYThreshold;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Threshold', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYThreshold.FindThreshold(serial+'.'+funcId);
    end;

//--- (end of YThreshold implementation)

//--- (YThreshold functions)

  function yFindThreshold(func:string): TYThreshold;
    begin
      result := TYThreshold.FindThreshold(func);
    end;

  function yFirstThreshold(): TYThreshold;
    begin
      result := TYThreshold.FirstThreshold();
    end;

  procedure _ThresholdCleanup();
    begin
    end;

//--- (end of YThreshold functions)

initialization
  //--- (YThreshold initialization)
  //--- (end of YThreshold initialization)

finalization
  //--- (YThreshold cleanup)
  _ThresholdCleanup();
  //--- (end of YThreshold cleanup)

end.
