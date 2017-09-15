{*********************************************************************
 *
 * $Id: yocto_audioout.pas 28561 2017-09-15 15:09:45Z seb $
 *
 * Implements yFindAudioOut(), the high-level API for AudioOut functions
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


unit yocto_audioout;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YAudioOut definitions)

const Y_VOLUME_INVALID                = YAPI_INVALID_UINT;
const Y_MUTE_FALSE = 0;
const Y_MUTE_TRUE = 1;
const Y_MUTE_INVALID = -1;
const Y_VOLUMERANGE_INVALID           = YAPI_INVALID_STRING;
const Y_SIGNAL_INVALID                = YAPI_INVALID_INT;
const Y_NOSIGNALFOR_INVALID           = YAPI_INVALID_INT;


//--- (end of YAudioOut definitions)

type
  TYAudioOut = class;
  //--- (YAudioOut class start)
  TYAudioOutValueCallback = procedure(func: TYAudioOut; value:string);
  TYAudioOutTimedReportCallback = procedure(func: TYAudioOut; value:TYMeasure);

  ////
  /// <summary>
  ///   TYAudioOut Class: AudioOut function interface
  /// <para>
  ///   The Yoctopuce application programming interface allows you to configure the volume of the outout.
  /// </para>
  /// </summary>
  ///-
  TYAudioOut=class(TYFunction)
  //--- (end of YAudioOut class start)
  protected
  //--- (YAudioOut declaration)
    // Attributes (function value cache)
    _volume                   : LongInt;
    _mute                     : Integer;
    _volumeRange              : string;
    _signal                   : LongInt;
    _noSignalFor              : LongInt;
    _valueCallbackAudioOut    : TYAudioOutValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YAudioOut declaration)

  public
    //--- (YAudioOut accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns audio output volume, in per cents.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to audio output volume, in per cents
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_VOLUME_INVALID</c>.
    /// </para>
    ///-
    function get_volume():LongInt;

    ////
    /// <summary>
    ///   Changes audio output volume, in per cents.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to audio output volume, in per cents
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
    ///   either <c>Y_MUTE_FALSE</c> or <c>Y_MUTE_TRUE</c>, according to the state of the mute function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MUTE_INVALID</c>.
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
    ///   either <c>Y_MUTE_FALSE</c> or <c>Y_MUTE_TRUE</c>, according to the state of the mute function
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
    ///   On failure, throws an exception or returns <c>Y_VOLUMERANGE_INVALID</c>.
    /// </para>
    ///-
    function get_volumeRange():string;

    ////
    /// <summary>
    ///   Returns the detected output current level.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the detected output current level
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SIGNAL_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_NOSIGNALFOR_INVALID</c>.
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
    ///   Use the method <c>YAudioOut.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YAudioOut</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindAudioOut(func: string):TYAudioOut;

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
    function registerValueCallback(callback: TYAudioOutValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of audio outputs started using <c>yFirstAudioOut()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YAudioOut</c> object, corresponding to
    ///   an audio output currently online, or a <c>NIL</c> pointer
    ///   if there are no more audio outputs to enumerate.
    /// </returns>
    ///-
    function nextAudioOut():TYAudioOut;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstAudioOut():TYAudioOut;
  //--- (end of YAudioOut accessors declaration)
  end;

//--- (AudioOut functions declaration)
  ////
  /// <summary>
  ///   Retrieves an audio output for a given identifier.
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
  ///   This function does not require that the audio output is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YAudioOut.isOnline()</c> to test if the audio output is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   an audio output by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the audio output
  /// </param>
  /// <returns>
  ///   a <c>YAudioOut</c> object allowing you to drive the audio output.
  /// </returns>
  ///-
  function yFindAudioOut(func:string):TYAudioOut;
  ////
  /// <summary>
  ///   Starts the enumeration of audio outputs currently accessible.
  /// <para>
  ///   Use the method <c>YAudioOut.nextAudioOut()</c> to iterate on
  ///   next audio outputs.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YAudioOut</c> object, corresponding to
  ///   the first audio output currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstAudioOut():TYAudioOut;

//--- (end of AudioOut functions declaration)

implementation
//--- (YAudioOut dlldef)
//--- (end of YAudioOut dlldef)

  constructor TYAudioOut.Create(func:string);
    begin
      inherited Create(func);
      _className := 'AudioOut';
      //--- (YAudioOut accessors initialization)
      _volume := Y_VOLUME_INVALID;
      _mute := Y_MUTE_INVALID;
      _volumeRange := Y_VOLUMERANGE_INVALID;
      _signal := Y_SIGNAL_INVALID;
      _noSignalFor := Y_NOSIGNALFOR_INVALID;
      _valueCallbackAudioOut := nil;
      //--- (end of YAudioOut accessors initialization)
    end;


//--- (YAudioOut implementation)
{$HINTS OFF}
  function TYAudioOut._parseAttr(member:PJSONRECORD):integer;
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

  ////
  /// <summary>
  ///   Returns audio output volume, in per cents.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to audio output volume, in per cents
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_VOLUME_INVALID.
  /// </para>
  ///-
  function TYAudioOut.get_volume():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_VOLUME_INVALID;
              exit;
            end;
        end;
      res := self._volume;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes audio output volume, in per cents.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to audio output volume, in per cents
  /// </param>
  /// <para>
  /// </para>
  /// <returns>
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYAudioOut.set_volume(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('volume',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the state of the mute function.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_MUTE_FALSE or Y_MUTE_TRUE, according to the state of the mute function
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_MUTE_INVALID.
  /// </para>
  ///-
  function TYAudioOut.get_mute():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MUTE_INVALID;
              exit;
            end;
        end;
      res := self._mute;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the state of the mute function.
  /// <para>
  ///   Remember to call the matching module
  ///   saveToFlash() method to save the setting permanently.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   either Y_MUTE_FALSE or Y_MUTE_TRUE, according to the state of the mute function
  /// </param>
  /// <para>
  /// </para>
  /// <returns>
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYAudioOut.set_mute(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('mute',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the supported volume range.
  /// <para>
  ///   The low value of the
  ///   range corresponds to the minimal audible value. To
  ///   completely mute the sound, use set_mute()
  ///   instead of the set_volume().
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the supported volume range
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_VOLUMERANGE_INVALID.
  /// </para>
  ///-
  function TYAudioOut.get_volumeRange():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_VOLUMERANGE_INVALID;
              exit;
            end;
        end;
      res := self._volumeRange;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the detected output current level.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the detected output current level
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_SIGNAL_INVALID.
  /// </para>
  ///-
  function TYAudioOut.get_signal():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SIGNAL_INVALID;
              exit;
            end;
        end;
      res := self._signal;
      result := res;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_NOSIGNALFOR_INVALID.
  /// </para>
  ///-
  function TYAudioOut.get_noSignalFor():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_NOSIGNALFOR_INVALID;
              exit;
            end;
        end;
      res := self._noSignalFor;
      result := res;
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
  ///   Use the method <c>YAudioOut.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YAudioOut</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYAudioOut.FindAudioOut(func: string):TYAudioOut;
    var
      obj : TYAudioOut;
    begin
      obj := TYAudioOut(TYFunction._FindFromCache('AudioOut', func));
      if obj = nil then
        begin
          obj :=  TYAudioOut.create(func);
          TYFunction._AddToCache('AudioOut',  func, obj);
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
  function TYAudioOut.registerValueCallback(callback: TYAudioOutValueCallback):LongInt;
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
      self._valueCallbackAudioOut := callback;
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


  function TYAudioOut._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackAudioOut) <> nil) then
        begin
          self._valueCallbackAudioOut(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYAudioOut.nextAudioOut(): TYAudioOut;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextAudioOut := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextAudioOut := nil;
          exit;
        end;
      nextAudioOut := TYAudioOut.FindAudioOut(hwid);
    end;

  class function TYAudioOut.FirstAudioOut(): TYAudioOut;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('AudioOut', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYAudioOut.FindAudioOut(serial+'.'+funcId);
    end;

//--- (end of YAudioOut implementation)

//--- (AudioOut functions)

  function yFindAudioOut(func:string): TYAudioOut;
    begin
      result := TYAudioOut.FindAudioOut(func);
    end;

  function yFirstAudioOut(): TYAudioOut;
    begin
      result := TYAudioOut.FirstAudioOut();
    end;

  procedure _AudioOutCleanup();
    begin
    end;

//--- (end of AudioOut functions)

initialization
  //--- (AudioOut initialization)
  //--- (end of AudioOut initialization)

finalization
  //--- (AudioOut cleanup)
  _AudioOutCleanup();
  //--- (end of AudioOut cleanup)
end.
