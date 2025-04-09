{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindRelay(), the high-level API for Relay functions
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


unit yocto_relay;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YRelay definitions)
type  TYRelayDelayedPulse = class(TObject)
  public
      target      : LongInt;
      ms          : LongInt;
      moving      : LongInt;
      constructor Create();
    end;

const Y_STATE_A = 0;
const Y_STATE_B = 1;
const Y_STATE_INVALID = -1;
const Y_STATEATPOWERON_UNCHANGED = 0;
const Y_STATEATPOWERON_A = 1;
const Y_STATEATPOWERON_B = 2;
const Y_STATEATPOWERON_INVALID = -1;
const Y_MAXTIMEONSTATEA_INVALID       = YAPI_INVALID_LONG;
const Y_MAXTIMEONSTATEB_INVALID       = YAPI_INVALID_LONG;
const Y_OUTPUT_OFF = 0;
const Y_OUTPUT_ON = 1;
const Y_OUTPUT_INVALID = -1;
const Y_PULSETIMER_INVALID            = YAPI_INVALID_LONG;
const Y_COUNTDOWN_INVALID             = YAPI_INVALID_LONG;

var Y_DELAYEDPULSETIMER_INVALID : TYRelayDelayedPulse;
//--- (end of YRelay definitions)

//--- (YRelay yapiwrapper declaration)
//--- (end of YRelay yapiwrapper declaration)

type

  TYRelay = class;
  //--- (YRelay class start)
  TYRelayValueCallback = procedure(func: TYRelay; value:string);
  TYRelayTimedReportCallback = procedure(func: TYRelay; value:TYMeasure);

  ////
  /// <summary>
  ///   TYRelay Class: relay control interface, available for instance in the Yocto-LatchedRelay, the
  ///   Yocto-MaxiPowerRelay, the Yocto-PowerRelay-V3 or the Yocto-Relay
  /// <para>
  ///   The <c>YRelay</c> class allows you to drive a Yoctopuce relay or optocoupled output.
  ///   It can be used to simply switch the output on or off, but also to automatically generate short
  ///   pulses of determined duration.
  ///   On devices with two output for each relay (double throw), the two outputs are named A and B,
  ///   with output A corresponding to the idle position (normally closed) and the output B corresponding to the
  ///   active state (normally open).
  /// </para>
  /// </summary>
  ///-
  TYRelay=class(TYFunction)
  //--- (end of YRelay class start)
  protected
  //--- (YRelay declaration)
    // Attributes (function value cache)
    _state                    : Integer;
    _stateAtPowerOn           : Integer;
    _maxTimeOnStateA          : int64;
    _maxTimeOnStateB          : int64;
    _output                   : Integer;
    _pulseTimer               : int64;
    _delayedPulseTimer        : TYRelayDelayedPulse;
    _countdown                : int64;
    _valueCallbackRelay       : TYRelayValueCallback;
    _firm                     : LongInt;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YRelay declaration)

  public
    //--- (YRelay accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the state of the relays (A for the idle position, B for the active position).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YRelay.STATE_A</c> or <c>YRelay.STATE_B</c>, according to the state of the relays (A for
    ///   the idle position, B for the active position)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRelay.STATE_INVALID</c>.
    /// </para>
    ///-
    function get_state():Integer;

    ////
    /// <summary>
    ///   Changes the state of the relays (A for the idle position, B for the active position).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YRelay.STATE_A</c> or <c>YRelay.STATE_B</c>, according to the state of the relays (A for
    ///   the idle position, B for the active position)
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
    function set_state(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the state of the relays at device startup (A for the idle position,
    ///   B for the active position, UNCHANGED to leave the relay state as is).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YRelay.STATEATPOWERON_UNCHANGED</c>, <c>YRelay.STATEATPOWERON_A</c> and
    ///   <c>YRelay.STATEATPOWERON_B</c> corresponding to the state of the relays at device startup (A for
    ///   the idle position,
    ///   B for the active position, UNCHANGED to leave the relay state as is)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRelay.STATEATPOWERON_INVALID</c>.
    /// </para>
    ///-
    function get_stateAtPowerOn():Integer;

    ////
    /// <summary>
    ///   Changes the state of the relays at device startup (A for the idle position,
    ///   B for the active position, UNCHANGED to leave the relay state as is).
    /// <para>
    ///   Remember to call the matching module <c>saveToFlash()</c>
    ///   method, otherwise this call will have no effect.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YRelay.STATEATPOWERON_UNCHANGED</c>, <c>YRelay.STATEATPOWERON_A</c> and
    ///   <c>YRelay.STATEATPOWERON_B</c> corresponding to the state of the relays at device startup (A for
    ///   the idle position,
    ///   B for the active position, UNCHANGED to leave the relay state as is)
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
    function set_stateAtPowerOn(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the maximum time (ms) allowed for the relay to stay in state
    ///   A before automatically switching back in to B state.
    /// <para>
    ///   Zero means no time limit.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximum time (ms) allowed for the relay to stay in state
    ///   A before automatically switching back in to B state
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRelay.MAXTIMEONSTATEA_INVALID</c>.
    /// </para>
    ///-
    function get_maxTimeOnStateA():int64;

    ////
    /// <summary>
    ///   Changes the maximum time (ms) allowed for the relay to stay in state A
    ///   before automatically switching back in to B state.
    /// <para>
    ///   Use zero for no time limit.
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the maximum time (ms) allowed for the relay to stay in state A
    ///   before automatically switching back in to B state
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
    function set_maxTimeOnStateA(newval:int64):integer;

    ////
    /// <summary>
    ///   Retourne the maximum time (ms) allowed for the relay to stay in state B
    ///   before automatically switching back in to A state.
    /// <para>
    ///   Zero means no time limit.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRelay.MAXTIMEONSTATEB_INVALID</c>.
    /// </para>
    ///-
    function get_maxTimeOnStateB():int64;

    ////
    /// <summary>
    ///   Changes the maximum time (ms) allowed for the relay to stay in state B before
    ///   automatically switching back in to A state.
    /// <para>
    ///   Use zero for no time limit.
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the maximum time (ms) allowed for the relay to stay in state B before
    ///   automatically switching back in to A state
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
    function set_maxTimeOnStateB(newval:int64):integer;

    ////
    /// <summary>
    ///   Returns the output state of the relays, when used as a simple switch (single throw).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YRelay.OUTPUT_OFF</c> or <c>YRelay.OUTPUT_ON</c>, according to the output state of the
    ///   relays, when used as a simple switch (single throw)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRelay.OUTPUT_INVALID</c>.
    /// </para>
    ///-
    function get_output():Integer;

    ////
    /// <summary>
    ///   Changes the output state of the relays, when used as a simple switch (single throw).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YRelay.OUTPUT_OFF</c> or <c>YRelay.OUTPUT_ON</c>, according to the output state of the
    ///   relays, when used as a simple switch (single throw)
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
    function set_output(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the number of milliseconds remaining before the relays is returned to idle position
    ///   (state A), during a measured pulse generation.
    /// <para>
    ///   When there is no ongoing pulse, returns zero.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of milliseconds remaining before the relays is returned to idle position
    ///   (state A), during a measured pulse generation
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRelay.PULSETIMER_INVALID</c>.
    /// </para>
    ///-
    function get_pulseTimer():int64;

    function set_pulseTimer(newval:int64):integer;

    ////
    /// <summary>
    ///   Sets the relay to output B (active) for a specified duration, then brings it
    ///   automatically back to output A (idle state).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="ms_duration">
    ///   pulse duration, in milliseconds
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
    function pulse(ms_duration: LongInt):integer;

    function get_delayedPulseTimer():TYRelayDelayedPulse;

    function set_delayedPulseTimer(newval:TYRelayDelayedPulse):integer;

    ////
    /// <summary>
    ///   Schedules a pulse.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="ms_delay">
    ///   waiting time before the pulse, in milliseconds
    /// </param>
    /// <param name="ms_duration">
    ///   pulse duration, in milliseconds
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
    function delayedPulse(ms_delay: LongInt; ms_duration: LongInt):integer;

    ////
    /// <summary>
    ///   Returns the number of milliseconds remaining before a pulse (delayedPulse() call)
    ///   When there is no scheduled pulse, returns zero.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of milliseconds remaining before a pulse (delayedPulse() call)
    ///   When there is no scheduled pulse, returns zero
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRelay.COUNTDOWN_INVALID</c>.
    /// </para>
    ///-
    function get_countdown():int64;

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
    ///   Use the method <c>YRelay.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YRelay</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindRelay(func: string):TYRelay;

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
    function registerValueCallback(callback: TYRelayValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Switch the relay to the opposite state.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function toggle():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of relays started using <c>yFirstRelay()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned relays order.
    ///   If you want to find a specific a relay, use <c>Relay.findRelay()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YRelay</c> object, corresponding to
    ///   a relay currently online, or a <c>NIL</c> pointer
    ///   if there are no more relays to enumerate.
    /// </returns>
    ///-
    function nextRelay():TYRelay;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstRelay():TYRelay;
  //--- (end of YRelay accessors declaration)
  end;

//--- (YRelay functions declaration)
  ////
  /// <summary>
  ///   Retrieves a relay for a given identifier.
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
  ///   This function does not require that the relay is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YRelay.isOnline()</c> to test if the relay is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a relay by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the relay, for instance
  ///   <c>YLTCHRL1.relay1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YRelay</c> object allowing you to drive the relay.
  /// </returns>
  ///-
  function yFindRelay(func:string):TYRelay;
  ////
  /// <summary>
  ///   Starts the enumeration of relays currently accessible.
  /// <para>
  ///   Use the method <c>YRelay.nextRelay()</c> to iterate on
  ///   next relays.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YRelay</c> object, corresponding to
  ///   the first relay currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstRelay():TYRelay;

//--- (end of YRelay functions declaration)

implementation

//--- (YRelay dlldef)
//--- (end of YRelay dlldef)

    constructor TYRelayDelayedPulse.Create();
    begin
      target := YAPI_INVALID_INT;
      ms := YAPI_INVALID_INT;
      moving := YAPI_INVALID_UINT;
  end;

  constructor TYRelay.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Relay';
      //--- (YRelay accessors initialization)
      _state := Y_STATE_INVALID;
      _stateAtPowerOn := Y_STATEATPOWERON_INVALID;
      _maxTimeOnStateA := Y_MAXTIMEONSTATEA_INVALID;
      _maxTimeOnStateB := Y_MAXTIMEONSTATEB_INVALID;
      _output := Y_OUTPUT_INVALID;
      _pulseTimer := Y_PULSETIMER_INVALID;
      _delayedPulseTimer := Y_DELAYEDPULSETIMER_INVALID;
      _countdown := Y_COUNTDOWN_INVALID;
      _valueCallbackRelay := nil;
      _firm := 0;
      //--- (end of YRelay accessors initialization)
    end;

//--- (YRelay yapiwrapper)
//--- (end of YRelay yapiwrapper)

//--- (YRelay implementation)
{$HINTS OFF}
  function TYRelay._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'state') then
        begin
          _state := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'stateAtPowerOn') then
        begin
          _stateAtPowerOn := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'maxTimeOnStateA') then
        begin
          _maxTimeOnStateA := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'maxTimeOnStateB') then
        begin
          _maxTimeOnStateB := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'output') then
        begin
          _output := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'pulseTimer') then
        begin
          _pulseTimer := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'delayedPulseTimer') then
        begin
          if member^.recordtype = JSON_STRUCT then
            begin
              for l:=0 to member^.membercount-1 do
               begin
                 sub := member^.members[l];
                 if (sub^.name = 'moving') then
                    _delayedPulseTimer.moving := sub^.ivalue else
                 if (sub^.name = 'target') then
                    _delayedPulseTimer.target := sub^.ivalue else
                 if (sub^.name = 'ms') then
                    _delayedPulseTimer.ms := sub^.ivalue;
               end;
            end;
         result := 1;
         exit;
         end;
      if (member^.name = 'countdown') then
        begin
          _countdown := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYRelay.get_state():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_STATE_INVALID;
              exit;
            end;
        end;
      res := self._state;
      result := res;
      exit;
    end;


  function TYRelay.set_state(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('state',rest_val);
    end;

  function TYRelay.get_stateAtPowerOn():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_STATEATPOWERON_INVALID;
              exit;
            end;
        end;
      res := self._stateAtPowerOn;
      result := res;
      exit;
    end;


  function TYRelay.set_stateAtPowerOn(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('stateAtPowerOn',rest_val);
    end;

  function TYRelay.get_maxTimeOnStateA():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MAXTIMEONSTATEA_INVALID;
              exit;
            end;
        end;
      res := self._maxTimeOnStateA;
      result := res;
      exit;
    end;


  function TYRelay.set_maxTimeOnStateA(newval:int64):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('maxTimeOnStateA',rest_val);
    end;

  function TYRelay.get_maxTimeOnStateB():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MAXTIMEONSTATEB_INVALID;
              exit;
            end;
        end;
      res := self._maxTimeOnStateB;
      result := res;
      exit;
    end;


  function TYRelay.set_maxTimeOnStateB(newval:int64):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('maxTimeOnStateB',rest_val);
    end;

  function TYRelay.get_output():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_OUTPUT_INVALID;
              exit;
            end;
        end;
      res := self._output;
      result := res;
      exit;
    end;


  function TYRelay.set_output(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('output',rest_val);
    end;

  function TYRelay.get_pulseTimer():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PULSETIMER_INVALID;
              exit;
            end;
        end;
      res := self._pulseTimer;
      result := res;
      exit;
    end;


  function TYRelay.set_pulseTimer(newval:int64):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('pulseTimer',rest_val);
    end;

  ////
  /// <summary>
  ///   Sets the relay to output B (active) for a specified duration, then brings it
  ///   automatically back to output A (idle state).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="ms_duration">
  ///   pulse duration, in milliseconds
  /// </param>
  /// <para>
  /// </para>
  /// <returns>
  ///   YAPI.SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYRelay.pulse(ms_duration: LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(ms_duration);
      result := _setAttr('pulseTimer', rest_val);
    end;

  function TYRelay.get_delayedPulseTimer():TYRelayDelayedPulse;
    var
      res : TYRelayDelayedPulse;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DELAYEDPULSETIMER_INVALID;
              exit;
            end;
        end;
      res := self._delayedPulseTimer;
      result := res;
      exit;
    end;


  function TYRelay.set_delayedPulseTimer(newval:TYRelayDelayedPulse):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval.target)+':'+inttostr(newval.ms);
      result := _setAttr('delayedPulseTimer',rest_val);
    end;

  ////
  /// <summary>
  ///   Schedules a pulse.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="ms_delay">
  ///   waiting time before the pulse, in milliseconds
  /// </param>
  /// <param name="ms_duration">
  ///   pulse duration, in milliseconds
  /// </param>
  /// <para>
  /// </para>
  /// <returns>
  ///   YAPI.SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYRelay.delayedPulse(ms_delay: LongInt; ms_duration: LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(ms_delay)+':'+inttostr(ms_duration);
      result := _setAttr('delayedPulseTimer', rest_val);
    end;

  function TYRelay.get_countdown():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_COUNTDOWN_INVALID;
              exit;
            end;
        end;
      res := self._countdown;
      result := res;
      exit;
    end;


  class function TYRelay.FindRelay(func: string):TYRelay;
    var
      obj : TYRelay;
    begin
      obj := TYRelay(TYFunction._FindFromCache('Relay', func));
      if obj = nil then
        begin
          obj :=  TYRelay.create(func);
          TYFunction._AddToCache('Relay', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYRelay.registerValueCallback(callback: TYRelayValueCallback):LongInt;
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
      self._valueCallbackRelay := callback;
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


  function TYRelay._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackRelay) <> nil) then
        begin
          self._valueCallbackRelay(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYRelay.toggle():LongInt;
    var
      sta : LongInt;
      fw : string;
      mo : TYModule;
    begin
      if self._firm = 0 then
        begin
          mo := self.get_module;
          fw := mo.get_firmwareRelease;
          if (fw = Y_FIRMWARERELEASE_INVALID) then
            begin
              result := Y_STATE_INVALID;
              exit;
            end;
          self._firm := _atoi(fw);
        end;
      if self._firm < 34921 then
        begin
          sta := self.get_state;
          if sta = Y_STATE_INVALID then
            begin
              result := Y_STATE_INVALID;
              exit;
            end;
          if sta = Y_STATE_B then
            begin
              self.set_state(Y_STATE_A);
            end
          else
            begin
              self.set_state(Y_STATE_B);
            end;
          result := YAPI_SUCCESS;
          exit;
        end
      else
        begin
          result := self._setAttr('state', 'X');
          exit;
        end;
    end;


  function TYRelay.nextRelay(): TYRelay;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextRelay := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextRelay := nil;
          exit;
        end;
      nextRelay := TYRelay.FindRelay(hwid);
    end;

  class function TYRelay.FirstRelay(): TYRelay;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Relay', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYRelay.FindRelay(serial+'.'+funcId);
    end;

//--- (end of YRelay implementation)

//--- (YRelay functions)

  function yFindRelay(func:string): TYRelay;
    begin
      result := TYRelay.FindRelay(func);
    end;

  function yFirstRelay(): TYRelay;
    begin
      result := TYRelay.FirstRelay();
    end;

  procedure _RelayCleanup();
    begin
    end;

//--- (end of YRelay functions)

initialization
  //--- (YRelay initialization)
    Y_DELAYEDPULSETIMER_INVALID := TYRelayDelayedPulse.Create();
  //--- (end of YRelay initialization)

finalization
  //--- (YRelay cleanup)
  _RelayCleanup();
  //--- (end of YRelay cleanup)

end.
