{*********************************************************************
 *
 *  $Id: yocto_audioin.pas 56084 2023-08-15 16:13:01Z mvuilleu $
 *
 *  Implements yFindAudioIn(), the high-level API for AudioIn functions
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


unit yocto_audioin;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YAudioIn definitions)

const Y_VOLUME_INVALID                = YAPI_INVALID_UINT;
const Y_MUTE_FALSE = 0;
const Y_MUTE_TRUE = 1;
const Y_MUTE_INVALID = -1;
const Y_VOLUMERANGE_INVALID           = YAPI_INVALID_STRING;
const Y_SIGNAL_INVALID                = YAPI_INVALID_INT;
const Y_NOSIGNALFOR_INVALID           = YAPI_INVALID_INT;

//--- (end of YAudioIn definitions)

//--- (YAudioIn yapiwrapper declaration)
//--- (end of YAudioIn yapiwrapper declaration)

type

  TYAudioIn = class;
  //--- (YAudioIn class start)
  TYAudioInValueCallback = procedure(func: TYAudioIn; value:string);
  TYAudioInTimedReportCallback = procedure(func: TYAudioIn; value:TYMeasure);

  ////
  /// <summary>
  ///   TYAudioIn Class: audio input control interface
  /// <para>
  ///   The <c>YAudioIn</c> class allows you to configure the volume of an audio input.
  /// </para>
  /// </summary>
  ///-
  TYAudioIn=class(TYFunction)
  //--- (end of YAudioIn class start)
  protected
  //--- (YAudioIn declaration)
    // Attributes (function value cache)
    _volume                   : LongInt;
    _mute                     : Integer;
    _volumeRange              : string;
    _signal                   : LongInt;
    _noSignalFor              : LongInt;
    _valueCallbackAudioIn     : TYAudioInValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YAudioIn declaration)

  public
    //--- (YAudioIn accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns audio input gain, in per cents.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to audio input gain, in per cents
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAudioIn.VOLUME_INVALID</c>.
    /// </para>
    ///-
    function get_volume():LongInt;

    ////
    /// <summary>
    ///   Changes audio input gain, in per cents.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to audio input gain, in per cents
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
    function set_volume(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the state of the mute function.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YAudioIn.MUTE_FALSE</c> or <c>YAudioIn.MUTE_TRUE</c>, according to the state of the mute function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAudioIn.MUTE_INVALID</c>.
    /// </para>
    ///-
    function get_mute():Integer;

    ////
    /// <summary>
    ///   Changes the state of the mute function.
    /// <para>
    ///   Remember to call the matching module
    ///   <c>saveToFlash()</c> method to save the setting permanently.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YAudioIn.MUTE_FALSE</c> or <c>YAudioIn.MUTE_TRUE</c>, according to the state of the mute function
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
    function set_mute(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the supported volume range.
    /// <para>
    ///   The low value of the
    ///   range corresponds to the minimal audible value. To
    ///   completely mute the sound, use <c>set_mute()</c>
    ///   instead of the <c>set_volume()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the supported volume range
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAudioIn.VOLUMERANGE_INVALID</c>.
    /// </para>
    ///-
    function get_volumeRange():string;

    ////
    /// <summary>
    ///   Returns the detected input signal level.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the detected input signal level
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAudioIn.SIGNAL_INVALID</c>.
    /// </para>
    ///-
    function get_signal():LongInt;

    ////
    /// <summary>
    ///   Returns the number of seconds elapsed without detecting a signal.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of seconds elapsed without detecting a signal
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAudioIn.NOSIGNALFOR_INVALID</c>.
    /// </para>
    ///-
    function get_noSignalFor():LongInt;

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
    ///   Use the method <c>YAudioIn.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YAudioIn</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindAudioIn(func: string):TYAudioIn;

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
    function registerValueCallback(callback: TYAudioInValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of audio inputs started using <c>yFirstAudioIn()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned audio inputs order.
    ///   If you want to find a specific an audio input, use <c>AudioIn.findAudioIn()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YAudioIn</c> object, corresponding to
    ///   an audio input currently online, or a <c>NIL</c> pointer
    ///   if there are no more audio inputs to enumerate.
    /// </returns>
    ///-
    function nextAudioIn():TYAudioIn;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstAudioIn():TYAudioIn;
  //--- (end of YAudioIn accessors declaration)
  end;

//--- (YAudioIn functions declaration)
  ////
  /// <summary>
  ///   Retrieves an audio input for a given identifier.
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
  ///   This function does not require that the audio input is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YAudioIn.isOnline()</c> to test if the audio input is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   an audio input by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the audio input, for instance
  ///   <c>MyDevice.audioIn1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YAudioIn</c> object allowing you to drive the audio input.
  /// </returns>
  ///-
  function yFindAudioIn(func:string):TYAudioIn;
  ////
  /// <summary>
  ///   Starts the enumeration of audio inputs currently accessible.
  /// <para>
  ///   Use the method <c>YAudioIn.nextAudioIn()</c> to iterate on
  ///   next audio inputs.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YAudioIn</c> object, corresponding to
  ///   the first audio input currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstAudioIn():TYAudioIn;

//--- (end of YAudioIn functions declaration)

implementation

//--- (YAudioIn dlldef)
//--- (end of YAudioIn dlldef)

  constructor TYAudioIn.Create(func:string);
    begin
      inherited Create(func);
      _className := 'AudioIn';
      //--- (YAudioIn accessors initialization)
      _volume := Y_VOLUME_INVALID;
      _mute := Y_MUTE_INVALID;
      _volumeRange := Y_VOLUMERANGE_INVALID;
      _signal := Y_SIGNAL_INVALID;
      _noSignalFor := Y_NOSIGNALFOR_INVALID;
      _valueCallbackAudioIn := nil;
      //--- (end of YAudioIn accessors initialization)
    end;

//--- (YAudioIn yapiwrapper)
//--- (end of YAudioIn yapiwrapper)

//--- (YAudioIn implementation)
{$HINTS OFF}
  function TYAudioIn._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'volume') then
        begin
          _volume := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'mute') then
        begin
          _mute := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'volumeRange') then
        begin
          _volumeRange := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'signal') then
        begin
          _signal := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'noSignalFor') then
        begin
          _noSignalFor := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYAudioIn.get_volume():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_VOLUME_INVALID;
              exit;
            end;
        end;
      res := self._volume;
      result := res;
      exit;
    end;


  function TYAudioIn.set_volume(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('volume',rest_val);
    end;

  function TYAudioIn.get_mute():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MUTE_INVALID;
              exit;
            end;
        end;
      res := self._mute;
      result := res;
      exit;
    end;


  function TYAudioIn.set_mute(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('mute',rest_val);
    end;

  function TYAudioIn.get_volumeRange():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_VOLUMERANGE_INVALID;
              exit;
            end;
        end;
      res := self._volumeRange;
      result := res;
      exit;
    end;


  function TYAudioIn.get_signal():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNAL_INVALID;
              exit;
            end;
        end;
      res := self._signal;
      result := res;
      exit;
    end;


  function TYAudioIn.get_noSignalFor():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NOSIGNALFOR_INVALID;
              exit;
            end;
        end;
      res := self._noSignalFor;
      result := res;
      exit;
    end;


  class function TYAudioIn.FindAudioIn(func: string):TYAudioIn;
    var
      obj : TYAudioIn;
    begin
      obj := TYAudioIn(TYFunction._FindFromCache('AudioIn', func));
      if obj = nil then
        begin
          obj :=  TYAudioIn.create(func);
          TYFunction._AddToCache('AudioIn',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYAudioIn.registerValueCallback(callback: TYAudioInValueCallback):LongInt;
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
      self._valueCallbackAudioIn := callback;
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


  function TYAudioIn._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackAudioIn) <> nil) then
        begin
          self._valueCallbackAudioIn(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYAudioIn.nextAudioIn(): TYAudioIn;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextAudioIn := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextAudioIn := nil;
          exit;
        end;
      nextAudioIn := TYAudioIn.FindAudioIn(hwid);
    end;

  class function TYAudioIn.FirstAudioIn(): TYAudioIn;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('AudioIn', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYAudioIn.FindAudioIn(serial+'.'+funcId);
    end;

//--- (end of YAudioIn implementation)

//--- (YAudioIn functions)

  function yFindAudioIn(func:string): TYAudioIn;
    begin
      result := TYAudioIn.FindAudioIn(func);
    end;

  function yFirstAudioIn(): TYAudioIn;
    begin
      result := TYAudioIn.FirstAudioIn();
    end;

  procedure _AudioInCleanup();
    begin
    end;

//--- (end of YAudioIn functions)

initialization
  //--- (YAudioIn initialization)
  //--- (end of YAudioIn initialization)

finalization
  //--- (YAudioIn cleanup)
  _AudioInCleanup();
  //--- (end of YAudioIn cleanup)

end.
