{*********************************************************************
 *
 * $Id: yocto_relay.pas 10384 2013-03-16 16:57:45Z martinm $
 *
 * Implements yFindRelay(), the high-level API for Relay functions
 *
 * - - - - - - - - - License information: - - - - - - - - - 
 *
 * Copyright (C) 2011 and beyond by Yoctopuce Sarl, Switzerland.
 *
 * 1) If you have obtained this file from www.yoctopuce.com,
 *    Yoctopuce Sarl licenses to you (hereafter Licensee) the
 *    right to use, modify, copy, and integrate this source file
 *    into your own solution for the sole purpose of interfacing
 *    a Yoctopuce product with Licensee's solution.
 *
 *    The use of this file and all relationship between Yoctopuce 
 *    and Licensee are governed by Yoctopuce General Terms and 
 *    Conditions.
 *
 *    THE SOFTWARE AND DOCUMENTATION ARE PROVIDED 'AS IS' WITHOUT
 *    WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING 
 *    WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS 
 *    FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO
 *    EVENT SHALL LICENSOR BE LIABLE FOR ANY INCIDENTAL, SPECIAL,
 *    INDIRECT OR CONSEQUENTIAL DAMAGES, LOST PROFITS OR LOST DATA, 
 *    COST OF PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR 
 *    SERVICES, ANY CLAIMS BY THIRD PARTIES (INCLUDING BUT NOT 
 *    LIMITED TO ANY DEFENSE THEREOF), ANY CLAIMS FOR INDEMNITY OR
 *    CONTRIBUTION, OR OTHER SIMILAR COSTS, WHETHER ASSERTED ON THE
 *    BASIS OF CONTRACT, TORT (INCLUDING NEGLIGENCE), BREACH OF
 *    WARRANTY, OR OTHERWISE.
 *
 * 2) If your intent is not to interface with Yoctopuce products,
 *    you are not entitled to use, read or create any derived
 *    material from this source file.
 *
 *********************************************************************}


unit yocto_relay;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YRelay definitions)
type TYRelayDelayedPulse = class(TObject)
public
   target      : longint;
   ms          : longint;
   moving      : longint;
   constructor Create();
end;

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_STATE_A = 0;
   Y_STATE_B = 1;
   Y_STATE_INVALID = -1;

   Y_OUTPUT_OFF = 0;
   Y_OUTPUT_ON = 1;
   Y_OUTPUT_INVALID = -1;

   Y_PULSETIMER_INVALID            = YAPI_INVALID_LONGWORD;
   Y_COUNTDOWN_INVALID             = YAPI_INVALID_LONGWORD;

var Y_DELAYEDPULSETIMER_INVALID : TYRelayDelayedPulse;

//--- (end of YRelay definitions)

type
//--- (YRelay declaration)
 TYRelay = class;
 TUpdateCallback  = procedure(func: TYRelay; value:string);
////
/// <summary>
///   TYRelay Class: Relay function interface
/// <para>
///   The Yoctopuce application programming interface allows you to switch the relay state.
///   This change is not persistent: the relay will automatically return to its idle position
///   whenever power is lost or if the module is restarted.
///   The library can also generate automatically short pulses of determined duration.
///   On devices with two output for each relay (double throw), the two outputs are named A and B,
///   with output A corresponding to the idle position (at power off) and the output B corresponding to the
///   active state. If you prefer the alternate default state, simply switch your cables on the board.
/// </para>
/// </summary>
///-
TYRelay=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _state                    : Integer;
   _output                   : Integer;
   _pulseTimer               : LongWord;
   _delayedPulseTimer        : TYRelayDelayedPulse;
   _countdown                : LongWord;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YRelay declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of relays started using <c>yFirstRelay()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YRelay</c> object, corresponding to
   ///   a relay currently online, or a <c>null</c> pointer
   ///   if there are no more relays to enumerate.
   /// </returns>
   ///-
   function nextRelay():TYRelay;

   //--- (YRelay accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the relay.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the relay
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the relay.
   /// <para>
   ///   You can use <c>yCheckLogicalName()</c>
   ///   prior to this call to make sure that your parameter is valid.
   ///   Remember to call the <c>saveToFlash()</c> method of the module if the
   ///   modification must be kept.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   a string corresponding to the logical name of the relay
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
   function set_logicalName(newval:string):integer;

   ////
   /// <summary>
   ///   Returns the current value of the relay (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the relay (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the state of the relays (A for the idle position, B for the active position).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_STATE_A</c> or <c>Y_STATE_B</c>, according to the state of the relays (A for the idle
   ///   position, B for the active position)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_STATE_INVALID</c>.
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
   ///   either <c>Y_STATE_A</c> or <c>Y_STATE_B</c>, according to the state of the relays (A for the idle
   ///   position, B for the active position)
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
   function set_state(newval:Integer):integer;

   ////
   /// <summary>
   ///   Returns the output state of the relays, when used as a simple switch (single throw).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_OUTPUT_OFF</c> or <c>Y_OUTPUT_ON</c>, according to the output state of the relays, when
   ///   used as a simple switch (single throw)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_OUTPUT_INVALID</c>.
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
   ///   either <c>Y_OUTPUT_OFF</c> or <c>Y_OUTPUT_ON</c>, according to the output state of the relays, when
   ///   used as a simple switch (single throw)
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
   ///   On failure, throws an exception or returns <c>Y_PULSETIMER_INVALID</c>.
   /// </para>
   ///-
   function get_pulseTimer():LongWord;

   function set_pulseTimer(newval:LongWord):integer;

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
   ///   pulse duration, in millisecondes
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
   function pulse(ms_duration:integer):integer;

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
   ///   waiting time before the pulse, in millisecondes
   /// </param>
   /// <param name="ms_duration">
   ///   pulse duration, in millisecondes
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
   function delayedPulse(ms_delay:integer;ms_duration:integer):integer;

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
   ///   On failure, throws an exception or returns <c>Y_COUNTDOWN_INVALID</c>.
   /// </para>
   ///-
   function get_countdown():LongWord;

   //--- (end of YRelay accessors declaration)
end;

//--- (Relay functions declaration)

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
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the relay
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
///   the first relay currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstRelay():TYRelay;

//--- (end of Relay functions declaration)

implementation

//--- (YRelay implementation)

var
   _RelayCache : TStringList;

constructor TYRelayDelayedPulse.Create();
 begin
   target := YAPI_INVALID_LONGINT;
   ms := YAPI_INVALID_LONGINT;
   moving := YAPI_INVALID_LONGINT;
 end;

constructor TYRelay.Create(func:string);
 begin
   inherited Create('Relay', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _state := Y_STATE_INVALID;
   _output := Y_OUTPUT_INVALID;
   _pulseTimer := Y_PULSETIMER_INVALID;
   _delayedPulseTimer := TYRelayDelayedPulse.Create();
   _countdown := Y_COUNTDOWN_INVALID;
 end;

{$HINTS OFF}
function TYRelay._parse(j:PJSONRECORD):integer;
 var
   member,sub : PJSONRECORD;
   i,l        : integer;
 begin
   if (j^.recordtype <> JSON_STRUCT) then begin _parse:= -1; exit; end;
   for i:=0 to j^.membercount-1 do
    begin
      member := j^.members[i];
      if (member^.name = 'logicalName') then
       begin
         _logicalName := string(member^.svalue);
       end else
      if (member^.name = 'advertisedValue') then
       begin
         _advertisedValue := string(member^.svalue);
       end else
      if (member^.name = 'state') then
       begin
         _state := member^.ivalue;
       end else
      if (member^.name = 'output') then
       begin
         _output := member^.ivalue;
       end else
      if (member^.name = 'pulseTimer') then
       begin
         _pulseTimer := member^.ivalue;
       end else
      if (member^.name = 'delayedPulseTimer') then
       begin
         if (member^.recordtype <> JSON_STRUCT) then begin _parse:= -1; exit; end;
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
         
       end else
      if (member^.name = 'countdown') then
       begin
         _countdown := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the relay.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the relay
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYRelay.get_logicalName():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LOGICALNAME_INVALID;
         exit;
       end;
   result := _logicalName;
 end;

////
/// <summary>
///   Changes the logical name of the relay.
/// <para>
///   You can use yCheckLogicalName()
///   prior to this call to make sure that your parameter is valid.
///   Remember to call the saveToFlash() method of the module if the
///   modification must be kept.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   a string corresponding to the logical name of the relay
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
function TYRelay.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the relay (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the relay (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYRelay.get_advertisedValue():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_ADVERTISEDVALUE_INVALID;
         exit;
       end;
   result := _advertisedValue;
 end;

////
/// <summary>
///   Returns the state of the relays (A for the idle position, B for the active position).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_STATE_A or Y_STATE_B, according to the state of the relays (A for the idle position, B for
///   the active position)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_STATE_INVALID.
/// </para>
///-
function TYRelay.get_state():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_STATE_INVALID;
         exit;
       end;
   result := _state;
 end;

////
/// <summary>
///   Changes the state of the relays (A for the idle position, B for the active position).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   either Y_STATE_A or Y_STATE_B, according to the state of the relays (A for the idle position, B for
///   the active position)
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
function TYRelay.set_state(newval:Integer):integer;
 var
   rest_val: string;
 begin
   if(newval>0) then rest_val := '1' else rest_val := '0';
   result := _setAttr('state',rest_val);
 end;

////
/// <summary>
///   Returns the output state of the relays, when used as a simple switch (single throw).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_OUTPUT_OFF or Y_OUTPUT_ON, according to the output state of the relays, when used as a
///   simple switch (single throw)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_OUTPUT_INVALID.
/// </para>
///-
function TYRelay.get_output():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_OUTPUT_INVALID;
         exit;
       end;
   result := _output;
 end;

////
/// <summary>
///   Changes the output state of the relays, when used as a simple switch (single throw).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   either Y_OUTPUT_OFF or Y_OUTPUT_ON, according to the output state of the relays, when used as a
///   simple switch (single throw)
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
function TYRelay.set_output(newval:Integer):integer;
 var
   rest_val: string;
 begin
   if(newval>0) then rest_val := '1' else rest_val := '0';
   result := _setAttr('output',rest_val);
 end;

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
///   On failure, throws an exception or returns Y_PULSETIMER_INVALID.
/// </para>
///-
function TYRelay.get_pulseTimer():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PULSETIMER_INVALID;
         exit;
       end;
   result := _pulseTimer;
 end;

function TYRelay.set_pulseTimer(newval:LongWord):integer;
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
///   pulse duration, in millisecondes
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
function TYRelay.pulse(ms_duration:integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(ms_duration);
   result := _setAttr('pulseTimer', rest_val);
 end;

function TYRelay.get_delayedPulseTimer():TYRelayDelayedPulse;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_DELAYEDPULSETIMER_INVALID;
         exit;
       end;
   result := _delayedPulseTimer;
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
///   waiting time before the pulse, in millisecondes
/// </param>
/// <param name="ms_duration">
///   pulse duration, in millisecondes
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
function TYRelay.delayedPulse(ms_delay:integer;ms_duration:integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(ms_delay)+':'+inttostr(ms_duration);
   result := _setAttr('delayedPulseTimer', rest_val);
 end;

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
///   On failure, throws an exception or returns Y_COUNTDOWN_INVALID.
/// </para>
///-
function TYRelay.get_countdown():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_COUNTDOWN_INVALID;
         exit;
       end;
   result := _countdown;
 end;

function TYRelay.nextRelay(): TYRelay;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextRelay := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextRelay := nil;
      exit;
    end;
    nextRelay := yFindRelay(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYRelay.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYRelay.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYRelay.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYRelay.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YRelay implementation)

//--- (Relay functions)

function yFindRelay(func:string): TYRelay;
 var
   index: integer;
   res  : TYRelay;
 begin
    if (_RelayCache.Find(func, index)) then
     begin
       yFindRelay := TYRelay(_RelayCache.objects[index]);
       exit;
     end;
   res := TYRelay.Create(func);
   _RelayCache.addObject(func, res);
   yFindRelay := res;
 end;

function yFirstRelay(): TYRelay;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('Relay', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstRelay := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstRelay := nil;
       exit;
    end;
   yFirstRelay := yFindRelay(serial+'.'+funcId);
 end;

procedure _RelayCleanup();
  var i:integer;
begin
  for i:=0 to _RelayCache.count-1 do 
    begin
     _RelayCache.objects[i].free();
     _RelayCache.objects[i]:=nil;
    end;
   _RelayCache.free();
   _RelayCache:=nil;
end;

//--- (end of Relay functions)

initialization
   //--- (Relay initialization)
   _RelayCache        := TstringList.create();
   _RelayCache.sorted := true;
   Y_DELAYEDPULSETIMER_INVALID := TYRelayDelayedPulse.Create();
   //--- (end of Relay initialization)

finalization
   //--- (Relay cleanup)
   _RelayCleanup();
   //--- (end of Relay cleanup)
end.
