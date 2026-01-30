{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindSoundLevel(), the high-level API for SoundLevel functions
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


unit yocto_soundlevel;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YSoundLevel definitions)

const Y_LABEL_INVALID                 = YAPI_INVALID_STRING;
const Y_INTEGRATIONTIME_INVALID       = YAPI_INVALID_UINT;

//--- (end of YSoundLevel definitions)

//--- (YSoundLevel yapiwrapper declaration)
//--- (end of YSoundLevel yapiwrapper declaration)

type

  TYSoundLevel = class;
  //--- (YSoundLevel class start)
  TYSoundLevelValueCallback = procedure(func: TYSoundLevel; value:string);
  TYSoundLevelTimedReportCallback = procedure(func: TYSoundLevel; value:TYMeasure);

  ////
  /// <summary>
  ///   TYSoundLevel Class: sound pressure level meter control interface
  /// <para>
  ///   The <c>YSoundLevel</c> class allows you to read and configure Yoctopuce sound pressure level meters.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYSoundLevel=class(TYSensor)
  //--- (end of YSoundLevel class start)
  protected
  //--- (YSoundLevel declaration)
    // Attributes (function value cache)
    _label                    : string;
    _integrationTime          : LongInt;
    _valueCallbackSoundLevel  : TYSoundLevelValueCallback;
    _timedReportCallbackSoundLevel : TYSoundLevelTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YSoundLevel declaration)

  public
    //--- (YSoundLevel accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the measuring unit for the sound pressure level (dBA, dBC or dBZ).
    /// <para>
    ///   That unit will directly determine frequency weighting to be used to compute
    ///   the measured value. Remember to call the <c>saveToFlash()</c> method of the
    ///   module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the measuring unit for the sound pressure level (dBA, dBC or dBZ)
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
    ///   Returns the label for the sound pressure level measurement, as per
    ///   IEC standard 61672-1:2013.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the label for the sound pressure level measurement, as per
    ///   IEC standard 61672-1:2013
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSoundLevel.LABEL_INVALID</c>.
    /// </para>
    ///-
    function get_label():string;

    ////
    /// <summary>
    ///   Returns the integration time in milliseconds for measuring the sound pressure level.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the integration time in milliseconds for measuring the sound pressure level
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSoundLevel.INTEGRATIONTIME_INVALID</c>.
    /// </para>
    ///-
    function get_integrationTime():LongInt;

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
    ///   Use the method <c>YSoundLevel.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YSoundLevel</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindSoundLevel(func: string):TYSoundLevel;

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
    function registerValueCallback(callback: TYSoundLevelValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYSoundLevelTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of sound pressure level meters started using <c>yFirstSoundLevel()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned sound pressure level meters order.
    ///   If you want to find a specific a sound pressure level meter, use <c>SoundLevel.findSoundLevel()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YSoundLevel</c> object, corresponding to
    ///   a sound pressure level meter currently online, or a <c>NIL</c> pointer
    ///   if there are no more sound pressure level meters to enumerate.
    /// </returns>
    ///-
    function nextSoundLevel():TYSoundLevel;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstSoundLevel():TYSoundLevel;
  //--- (end of YSoundLevel accessors declaration)
  end;

//--- (YSoundLevel functions declaration)
  ////
  /// <summary>
  ///   Retrieves a sound pressure level meter for a given identifier.
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
  ///   This function does not require that the sound pressure level meter is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YSoundLevel.isOnline()</c> to test if the sound pressure level meter is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a sound pressure level meter by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the sound pressure level meter, for instance
  ///   <c>MyDevice.soundLevel1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YSoundLevel</c> object allowing you to drive the sound pressure level meter.
  /// </returns>
  ///-
  function yFindSoundLevel(func:string):TYSoundLevel;
  ////
  /// <summary>
  ///   Starts the enumeration of sound pressure level meters currently accessible.
  /// <para>
  ///   Use the method <c>YSoundLevel.nextSoundLevel()</c> to iterate on
  ///   next sound pressure level meters.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YSoundLevel</c> object, corresponding to
  ///   the first sound pressure level meter currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstSoundLevel():TYSoundLevel;

//--- (end of YSoundLevel functions declaration)

implementation

//--- (YSoundLevel dlldef)
//--- (end of YSoundLevel dlldef)

  constructor TYSoundLevel.Create(func:string);
    begin
      inherited Create(func);
      _className := 'SoundLevel';
      //--- (YSoundLevel accessors initialization)
      _label := Y_LABEL_INVALID;
      _integrationTime := Y_INTEGRATIONTIME_INVALID;
      _valueCallbackSoundLevel := nil;
      _timedReportCallbackSoundLevel := nil;
      //--- (end of YSoundLevel accessors initialization)
    end;

//--- (YSoundLevel yapiwrapper)
//--- (end of YSoundLevel yapiwrapper)

//--- (YSoundLevel implementation)
{$HINTS OFF}
  function TYSoundLevel._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'label') then
        begin
          _label := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'integrationTime') then
        begin
          _integrationTime := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYSoundLevel.set_unit(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('unit',rest_val);
    end;

  function TYSoundLevel.get_label():string;
    var
      res : string;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LABEL_INVALID;
              exit;
            end;
        end;
      res := self._label;
      result := res;
      exit;
    end;


  function TYSoundLevel.get_integrationTime():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_INTEGRATIONTIME_INVALID;
              exit;
            end;
        end;
      res := self._integrationTime;
      result := res;
      exit;
    end;


  class function TYSoundLevel.FindSoundLevel(func: string):TYSoundLevel;
    var
      obj : TYSoundLevel;
    begin
      obj := TYSoundLevel(TYFunction._FindFromCache('SoundLevel', func));
      if (obj = nil) then
        begin
          obj :=  TYSoundLevel.create(func);
          TYFunction._AddToCache('SoundLevel', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYSoundLevel.registerValueCallback(callback: TYSoundLevelValueCallback):LongInt;
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
      self._valueCallbackSoundLevel := callback;
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


  function TYSoundLevel._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackSoundLevel) <> nil) then
        begin
          self._valueCallbackSoundLevel(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYSoundLevel.registerTimedReportCallback(callback: TYSoundLevelTimedReportCallback):LongInt;
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
      self._timedReportCallbackSoundLevel := callback;
      result := 0;
      exit;
    end;


  function TYSoundLevel._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackSoundLevel) <> nil) then
        begin
          self._timedReportCallbackSoundLevel(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYSoundLevel.nextSoundLevel(): TYSoundLevel;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextSoundLevel := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextSoundLevel := nil;
          exit;
        end;
      nextSoundLevel := TYSoundLevel.FindSoundLevel(hwid);
    end;

  class function TYSoundLevel.FirstSoundLevel(): TYSoundLevel;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('SoundLevel', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYSoundLevel.FindSoundLevel(serial+'.'+funcId);
    end;

//--- (end of YSoundLevel implementation)

//--- (YSoundLevel functions)

  function yFindSoundLevel(func:string): TYSoundLevel;
    begin
      result := TYSoundLevel.FindSoundLevel(func);
    end;

  function yFirstSoundLevel(): TYSoundLevel;
    begin
      result := TYSoundLevel.FirstSoundLevel();
    end;

  procedure _SoundLevelCleanup();
    begin
    end;

//--- (end of YSoundLevel functions)

initialization
  //--- (YSoundLevel initialization)
  //--- (end of YSoundLevel initialization)

finalization
  //--- (YSoundLevel cleanup)
  _SoundLevelCleanup();
  //--- (end of YSoundLevel cleanup)

end.
