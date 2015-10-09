{*********************************************************************
 *
 * $Id: yocto_buzzer.pas 21551 2015-09-17 16:50:38Z seb $
 *
 * Implements yFindBuzzer(), the high-level API for Buzzer functions
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


unit yocto_buzzer;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YBuzzer definitions)

const Y_FREQUENCY_INVALID             = YAPI_INVALID_DOUBLE;
const Y_VOLUME_INVALID                = YAPI_INVALID_UINT;
const Y_PLAYSEQSIZE_INVALID           = YAPI_INVALID_UINT;
const Y_PLAYSEQMAXSIZE_INVALID        = YAPI_INVALID_UINT;
const Y_PLAYSEQSIGNATURE_INVALID      = YAPI_INVALID_UINT;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YBuzzer definitions)

type
  TYBuzzer = class;
  //--- (YBuzzer class start)
  TYBuzzerValueCallback = procedure(func: TYBuzzer; value:string);
  TYBuzzerTimedReportCallback = procedure(func: TYBuzzer; value:TYMeasure);

  ////
  /// <summary>
  ///   TYBuzzer Class: Buzzer function interface
  /// <para>
  ///   The Yoctopuce application programming interface allows you to
  ///   choose the frequency and volume at which the buzzer must sound.
  ///   You can also pre-program a play sequence.
  /// </para>
  /// </summary>
  ///-
  TYBuzzer=class(TYFunction)
  //--- (end of YBuzzer class start)
  protected
  //--- (YBuzzer declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _frequency                : double;
    _volume                   : LongInt;
    _playSeqSize              : LongInt;
    _playSeqMaxSize           : LongInt;
    _playSeqSignature         : LongInt;
    _command                  : string;
    _valueCallbackBuzzer      : TYBuzzerValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YBuzzer declaration)

  public
    //--- (YBuzzer accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the frequency of the signal sent to the buzzer.
    /// <para>
    ///   A zero value stops the buzzer.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the frequency of the signal sent to the buzzer
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
    function set_frequency(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the  frequency of the signal sent to the buzzer/speaker.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the  frequency of the signal sent to the buzzer/speaker
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_FREQUENCY_INVALID</c>.
    /// </para>
    ///-
    function get_frequency():double;

    ////
    /// <summary>
    ///   Returns the volume of the signal sent to the buzzer/speaker.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the volume of the signal sent to the buzzer/speaker
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_VOLUME_INVALID</c>.
    /// </para>
    ///-
    function get_volume():LongInt;

    ////
    /// <summary>
    ///   Changes the volume of the signal sent to the buzzer/speaker.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the volume of the signal sent to the buzzer/speaker
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
    ///   Returns the current length of the playing sequence
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current length of the playing sequence
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PLAYSEQSIZE_INVALID</c>.
    /// </para>
    ///-
    function get_playSeqSize():LongInt;

    ////
    /// <summary>
    ///   Returns the maximum length of the playing sequence
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximum length of the playing sequence
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PLAYSEQMAXSIZE_INVALID</c>.
    /// </para>
    ///-
    function get_playSeqMaxSize():LongInt;

    ////
    /// <summary>
    ///   Returns the playing sequence signature.
    /// <para>
    ///   As playing
    ///   sequences cannot be read from the device, this can be used
    ///   to detect if a specific playing sequence is already
    ///   programmed.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the playing sequence signature
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PLAYSEQSIGNATURE_INVALID</c>.
    /// </para>
    ///-
    function get_playSeqSignature():LongInt;

    function get_command():string;

    function set_command(newval:string):integer;

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
    ///   Use the method <c>YBuzzer.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YBuzzer</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindBuzzer(func: string):TYBuzzer;

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
    function registerValueCallback(callback: TYBuzzerValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function sendCommand(command: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Adds a new frequency transition to the playing sequence.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="freq">
    ///   desired frequency when the transition is completed, in Hz
    /// </param>
    /// <param name="msDelay">
    ///   duration of the frequency transition, in milliseconds.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function addFreqMoveToPlaySeq(freq: LongInt; msDelay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Adds a pulse to the playing sequence.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="freq">
    ///   pulse frequency, in Hz
    /// </param>
    /// <param name="msDuration">
    ///   pulse duration, in milliseconds.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function addPulseToPlaySeq(freq: LongInt; msDuration: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Adds a new volume transition to the playing sequence.
    /// <para>
    ///   Frequency stays untouched:
    ///   if frequency is at zero, the transition has no effect.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="volume">
    ///   desired volume when the transition is completed, as a percentage.
    /// </param>
    /// <param name="msDuration">
    ///   duration of the volume transition, in milliseconds.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function addVolMoveToPlaySeq(volume: LongInt; msDuration: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts the preprogrammed playing sequence.
    /// <para>
    ///   The sequence
    ///   runs in loop until it is stopped by stopPlaySeq or an explicit
    ///   change.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function startPlaySeq():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops the preprogrammed playing sequence and sets the frequency to zero.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function stopPlaySeq():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Resets the preprogrammed playing sequence and sets the frequency to zero.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function resetPlaySeq():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Activates the buzzer for a short duration.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="frequency">
    ///   pulse frequency, in hertz
    /// </param>
    /// <param name="duration">
    ///   pulse duration in millseconds
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function pulse(frequency: LongInt; duration: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Makes the buzzer frequency change over a period of time.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="frequency">
    ///   frequency to reach, in hertz. A frequency under 25Hz stops the buzzer.
    /// </param>
    /// <param name="duration">
    ///   pulse duration in millseconds
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function freqMove(frequency: LongInt; duration: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Makes the buzzer volume change over a period of time, frequency  stays untouched.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="volume">
    ///   volume to reach in %
    /// </param>
    /// <param name="duration">
    ///   change duration in millseconds
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function volumeMove(volume: LongInt; duration: LongInt):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of buzzers started using <c>yFirstBuzzer()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YBuzzer</c> object, corresponding to
    ///   a buzzer currently online, or a <c>null</c> pointer
    ///   if there are no more buzzers to enumerate.
    /// </returns>
    ///-
    function nextBuzzer():TYBuzzer;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstBuzzer():TYBuzzer;
  //--- (end of YBuzzer accessors declaration)
  end;

//--- (Buzzer functions declaration)
  ////
  /// <summary>
  ///   Retrieves a buzzer for a given identifier.
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
  ///   This function does not require that the buzzer is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YBuzzer.isOnline()</c> to test if the buzzer is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a buzzer by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the buzzer
  /// </param>
  /// <returns>
  ///   a <c>YBuzzer</c> object allowing you to drive the buzzer.
  /// </returns>
  ///-
  function yFindBuzzer(func:string):TYBuzzer;
  ////
  /// <summary>
  ///   Starts the enumeration of buzzers currently accessible.
  /// <para>
  ///   Use the method <c>YBuzzer.nextBuzzer()</c> to iterate on
  ///   next buzzers.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YBuzzer</c> object, corresponding to
  ///   the first buzzer currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstBuzzer():TYBuzzer;

//--- (end of Buzzer functions declaration)

implementation
//--- (YBuzzer dlldef)
//--- (end of YBuzzer dlldef)

  constructor TYBuzzer.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Buzzer';
      //--- (YBuzzer accessors initialization)
      _frequency := Y_FREQUENCY_INVALID;
      _volume := Y_VOLUME_INVALID;
      _playSeqSize := Y_PLAYSEQSIZE_INVALID;
      _playSeqMaxSize := Y_PLAYSEQMAXSIZE_INVALID;
      _playSeqSignature := Y_PLAYSEQSIGNATURE_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackBuzzer := nil;
      //--- (end of YBuzzer accessors initialization)
    end;


//--- (YBuzzer implementation)
{$HINTS OFF}
  function TYBuzzer._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'frequency') then
        begin
          _frequency := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'volume') then
        begin
          _volume := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'playSeqSize') then
        begin
          _playSeqSize := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'playSeqMaxSize') then
        begin
          _playSeqMaxSize := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'playSeqSignature') then
        begin
          _playSeqSignature := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'command') then
        begin
          _command := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Changes the frequency of the signal sent to the buzzer.
  /// <para>
  ///   A zero value stops the buzzer.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a floating point number corresponding to the frequency of the signal sent to the buzzer
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
  function TYBuzzer.set_frequency(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('frequency',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the  frequency of the signal sent to the buzzer/speaker.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the  frequency of the signal sent to the buzzer/speaker
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_FREQUENCY_INVALID.
  /// </para>
  ///-
  function TYBuzzer.get_frequency():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_FREQUENCY_INVALID;
              exit;
            end;
        end;
      result := self._frequency;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the volume of the signal sent to the buzzer/speaker.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the volume of the signal sent to the buzzer/speaker
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_VOLUME_INVALID.
  /// </para>
  ///-
  function TYBuzzer.get_volume():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_VOLUME_INVALID;
              exit;
            end;
        end;
      result := self._volume;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the volume of the signal sent to the buzzer/speaker.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the volume of the signal sent to the buzzer/speaker
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
  function TYBuzzer.set_volume(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('volume',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the current length of the playing sequence
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the current length of the playing sequence
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PLAYSEQSIZE_INVALID.
  /// </para>
  ///-
  function TYBuzzer.get_playSeqSize():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PLAYSEQSIZE_INVALID;
              exit;
            end;
        end;
      result := self._playSeqSize;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the maximum length of the playing sequence
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the maximum length of the playing sequence
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PLAYSEQMAXSIZE_INVALID.
  /// </para>
  ///-
  function TYBuzzer.get_playSeqMaxSize():LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PLAYSEQMAXSIZE_INVALID;
              exit;
            end;
        end;
      result := self._playSeqMaxSize;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the playing sequence signature.
  /// <para>
  ///   As playing
  ///   sequences cannot be read from the device, this can be used
  ///   to detect if a specific playing sequence is already
  ///   programmed.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the playing sequence signature
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PLAYSEQSIGNATURE_INVALID.
  /// </para>
  ///-
  function TYBuzzer.get_playSeqSignature():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PLAYSEQSIGNATURE_INVALID;
              exit;
            end;
        end;
      result := self._playSeqSignature;
      exit;
    end;


  function TYBuzzer.get_command():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_COMMAND_INVALID;
              exit;
            end;
        end;
      result := self._command;
      exit;
    end;


  function TYBuzzer.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
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
  ///   Use the method <c>YBuzzer.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YBuzzer</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYBuzzer.FindBuzzer(func: string):TYBuzzer;
    var
      obj : TYBuzzer;
    begin
      obj := TYBuzzer(TYFunction._FindFromCache('Buzzer', func));
      if obj = nil then
        begin
          obj :=  TYBuzzer.create(func);
          TYFunction._AddToCache('Buzzer',  func, obj);
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
  function TYBuzzer.registerValueCallback(callback: TYBuzzerValueCallback):LongInt;
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
      self._valueCallbackBuzzer := callback;
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


  function TYBuzzer._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackBuzzer) <> nil) then
        begin
          self._valueCallbackBuzzer(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYBuzzer.sendCommand(command: string):LongInt;
    begin
      result := self.set_command(command);
      exit;
    end;


  ////
  /// <summary>
  ///   Adds a new frequency transition to the playing sequence.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="freq">
  ///   desired frequency when the transition is completed, in Hz
  /// </param>
  /// <param name="msDelay">
  ///   duration of the frequency transition, in milliseconds.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYBuzzer.addFreqMoveToPlaySeq(freq: LongInt; msDelay: LongInt):LongInt;
    begin
      result := self.sendCommand('A'+inttostr(freq)+','+inttostr(msDelay));
      exit;
    end;


  ////
  /// <summary>
  ///   Adds a pulse to the playing sequence.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="freq">
  ///   pulse frequency, in Hz
  /// </param>
  /// <param name="msDuration">
  ///   pulse duration, in milliseconds.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYBuzzer.addPulseToPlaySeq(freq: LongInt; msDuration: LongInt):LongInt;
    begin
      result := self.sendCommand('B'+inttostr(freq)+','+inttostr(msDuration));
      exit;
    end;


  ////
  /// <summary>
  ///   Adds a new volume transition to the playing sequence.
  /// <para>
  ///   Frequency stays untouched:
  ///   if frequency is at zero, the transition has no effect.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="volume">
  ///   desired volume when the transition is completed, as a percentage.
  /// </param>
  /// <param name="msDuration">
  ///   duration of the volume transition, in milliseconds.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYBuzzer.addVolMoveToPlaySeq(volume: LongInt; msDuration: LongInt):LongInt;
    begin
      result := self.sendCommand('C'+inttostr(volume)+','+inttostr(msDuration));
      exit;
    end;


  ////
  /// <summary>
  ///   Starts the preprogrammed playing sequence.
  /// <para>
  ///   The sequence
  ///   runs in loop until it is stopped by stopPlaySeq or an explicit
  ///   change.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYBuzzer.startPlaySeq():LongInt;
    begin
      result := self.sendCommand('S');
      exit;
    end;


  ////
  /// <summary>
  ///   Stops the preprogrammed playing sequence and sets the frequency to zero.
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYBuzzer.stopPlaySeq():LongInt;
    begin
      result := self.sendCommand('X');
      exit;
    end;


  ////
  /// <summary>
  ///   Resets the preprogrammed playing sequence and sets the frequency to zero.
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function TYBuzzer.resetPlaySeq():LongInt;
    begin
      result := self.sendCommand('Z');
      exit;
    end;


  ////
  /// <summary>
  ///   Activates the buzzer for a short duration.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="frequency">
  ///   pulse frequency, in hertz
  /// </param>
  /// <param name="duration">
  ///   pulse duration in millseconds
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYBuzzer.pulse(frequency: LongInt; duration: LongInt):LongInt;
    begin
      result := self.set_command('P'+inttostr(frequency)+','+inttostr(duration));
      exit;
    end;


  ////
  /// <summary>
  ///   Makes the buzzer frequency change over a period of time.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="frequency">
  ///   frequency to reach, in hertz. A frequency under 25Hz stops the buzzer.
  /// </param>
  /// <param name="duration">
  ///   pulse duration in millseconds
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYBuzzer.freqMove(frequency: LongInt; duration: LongInt):LongInt;
    begin
      result := self.set_command('F'+inttostr(frequency)+','+inttostr(duration));
      exit;
    end;


  ////
  /// <summary>
  ///   Makes the buzzer volume change over a period of time, frequency  stays untouched.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="volume">
  ///   volume to reach in %
  /// </param>
  /// <param name="duration">
  ///   change duration in millseconds
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYBuzzer.volumeMove(volume: LongInt; duration: LongInt):LongInt;
    begin
      result := self.set_command('V'+inttostr(volume)+','+inttostr(duration));
      exit;
    end;


  function TYBuzzer.nextBuzzer(): TYBuzzer;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextBuzzer := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextBuzzer := nil;
          exit;
        end;
      nextBuzzer := TYBuzzer.FindBuzzer(hwid);
    end;

  class function TYBuzzer.FirstBuzzer(): TYBuzzer;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Buzzer', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYBuzzer.FindBuzzer(serial+'.'+funcId);
    end;

//--- (end of YBuzzer implementation)

//--- (Buzzer functions)

  function yFindBuzzer(func:string): TYBuzzer;
    begin
      result := TYBuzzer.FindBuzzer(func);
    end;

  function yFirstBuzzer(): TYBuzzer;
    begin
      result := TYBuzzer.FirstBuzzer();
    end;

  procedure _BuzzerCleanup();
    begin
    end;

//--- (end of Buzzer functions)

initialization
  //--- (Buzzer initialization)
  //--- (end of Buzzer initialization)

finalization
  //--- (Buzzer cleanup)
  _BuzzerCleanup();
  //--- (end of Buzzer cleanup)
end.
