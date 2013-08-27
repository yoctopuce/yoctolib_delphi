{*********************************************************************
 *
 * $Id: yocto_watchdog.pas 12324 2013-08-13 15:10:31Z mvuilleu $
 *
 * Implements yFindWatchdog(), the high-level API for Watchdog functions
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


unit yocto_watchdog;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YWatchdog definitions)
type TYWatchdogDelayedPulse = class(TObject)
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
   Y_AUTOSTART_OFF = 0;
   Y_AUTOSTART_ON = 1;
   Y_AUTOSTART_INVALID = -1;

   Y_RUNNING_OFF = 0;
   Y_RUNNING_ON = 1;
   Y_RUNNING_INVALID = -1;

   Y_TRIGGERDELAY_INVALID          = YAPI_INVALID_LONGWORD;
   Y_TRIGGERDURATION_INVALID       = YAPI_INVALID_LONGWORD;

var Y_DELAYEDPULSETIMER_INVALID : TYWatchdogDelayedPulse;

//--- (end of YWatchdog definitions)

type
//--- (YWatchdog declaration)
 TYWatchdog = class;
 TUpdateCallback  = procedure(func: TYWatchdog; value:string);
////
/// <summary>
///   TYWatchdog Class: Watchdog function interface
/// <para>
///   The watchog function works like a relay and can cause a brief power cut
///   to an appliance after a preset delay to force this appliance to
///   reset. The Watchdog must be called from time to time to reset the
///   timer and prevent the appliance reset.
///   The watchdog can be driven direcly with <i>pulse</i> and <i>delayedpulse</i> methods to switch
///   off an appliance for a given duration.
/// </para>
/// </summary>
///-
TYWatchdog=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _state                    : Integer;
   _output                   : Integer;
   _pulseTimer               : LongWord;
   _delayedPulseTimer        : TYWatchdogDelayedPulse;
   _countdown                : LongWord;
   _autoStart                : Integer;
   _running                  : Integer;
   _triggerDelay             : LongWord;
   _triggerDuration          : LongWord;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YWatchdog declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of watchdog started using <c>yFirstWatchdog()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YWatchdog</c> object, corresponding to
   ///   a watchdog currently online, or a <c>null</c> pointer
   ///   if there are no more watchdog to enumerate.
   /// </returns>
   ///-
   function nextWatchdog():TYWatchdog;

   //--- (YWatchdog accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the watchdog.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the watchdog
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the watchdog.
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
   ///   a string corresponding to the logical name of the watchdog
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
   ///   Returns the current value of the watchdog (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the watchdog (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the state of the watchdog (A for the idle position, B for the active position).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_STATE_A</c> or <c>Y_STATE_B</c>, according to the state of the watchdog (A for the idle
   ///   position, B for the active position)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_STATE_INVALID</c>.
   /// </para>
   ///-
   function get_state():Integer;

   ////
   /// <summary>
   ///   Changes the state of the watchdog (A for the idle position, B for the active position).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   either <c>Y_STATE_A</c> or <c>Y_STATE_B</c>, according to the state of the watchdog (A for the idle
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
   ///   Returns the output state of the watchdog, when used as a simple switch (single throw).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_OUTPUT_OFF</c> or <c>Y_OUTPUT_ON</c>, according to the output state of the watchdog,
   ///   when used as a simple switch (single throw)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_OUTPUT_INVALID</c>.
   /// </para>
   ///-
   function get_output():Integer;

   ////
   /// <summary>
   ///   Changes the output state of the watchdog, when used as a simple switch (single throw).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   either <c>Y_OUTPUT_OFF</c> or <c>Y_OUTPUT_ON</c>, according to the output state of the watchdog,
   ///   when used as a simple switch (single throw)
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
   ///   Returns the number of milliseconds remaining before the watchdog is returned to idle position
   ///   (state A), during a measured pulse generation.
   /// <para>
   ///   When there is no ongoing pulse, returns zero.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the number of milliseconds remaining before the watchdog is returned to
   ///   idle position
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

   function get_delayedPulseTimer():TYWatchdogDelayedPulse;

   function set_delayedPulseTimer(newval:TYWatchdogDelayedPulse):integer;

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

   ////
   /// <summary>
   ///   Returns the watchdog runing state at module power up.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_AUTOSTART_OFF</c> or <c>Y_AUTOSTART_ON</c>, according to the watchdog runing state at
   ///   module power up
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_AUTOSTART_INVALID</c>.
   /// </para>
   ///-
   function get_autoStart():Integer;

   ////
   /// <summary>
   ///   Changes the watchdog runningsttae at module power up.
   /// <para>
   ///   Remember to call the
   ///   <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   either <c>Y_AUTOSTART_OFF</c> or <c>Y_AUTOSTART_ON</c>, according to the watchdog runningsttae at
   ///   module power up
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
   function set_autoStart(newval:Integer):integer;

   ////
   /// <summary>
   ///   Returns the watchdog running state.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_RUNNING_OFF</c> or <c>Y_RUNNING_ON</c>, according to the watchdog running state
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_RUNNING_INVALID</c>.
   /// </para>
   ///-
   function get_running():Integer;

   ////
   /// <summary>
   ///   Changes the running state of the watchdog.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   either <c>Y_RUNNING_OFF</c> or <c>Y_RUNNING_ON</c>, according to the running state of the watchdog
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
   function set_running(newval:Integer):integer;

   ////
   /// <summary>
   ///   Resets the watchdog.
   /// <para>
   ///   When the watchdog is running, this function
   ///   must be called on a regular basis to prevent the watchog to
   ///   trigger
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function resetWatchdog():integer;

   ////
   /// <summary>
   ///   Returns  the waiting duration before a reset is automatically triggered by the watchdog, in milliseconds.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to  the waiting duration before a reset is automatically triggered by the
   ///   watchdog, in milliseconds
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_TRIGGERDELAY_INVALID</c>.
   /// </para>
   ///-
   function get_triggerDelay():LongWord;

   ////
   /// <summary>
   ///   Changes the waiting delay before a reset is triggered by the watchdog, in milliseconds.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the waiting delay before a reset is triggered by the watchdog, in milliseconds
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
   function set_triggerDelay(newval:LongWord):integer;

   ////
   /// <summary>
   ///   Returns the duration of resets caused by the watchdog, in milliseconds.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the duration of resets caused by the watchdog, in milliseconds
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_TRIGGERDURATION_INVALID</c>.
   /// </para>
   ///-
   function get_triggerDuration():LongWord;

   ////
   /// <summary>
   ///   Changes the duration of resets caused by the watchdog, in milliseconds.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the duration of resets caused by the watchdog, in milliseconds
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
   function set_triggerDuration(newval:LongWord):integer;

   //--- (end of YWatchdog accessors declaration)
end;

//--- (Watchdog functions declaration)

////
/// <summary>
///   Retrieves a watchdog for a given identifier.
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
///   This function does not require that the watchdog is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YWatchdog.isOnline()</c> to test if the watchdog is
///   indeed online at a given time. In case of ambiguity when looking for
///   a watchdog by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the watchdog
/// </param>
/// <returns>
///   a <c>YWatchdog</c> object allowing you to drive the watchdog.
/// </returns>
///-
function yFindWatchdog(func:string):TYWatchdog;
////
/// <summary>
///   Starts the enumeration of watchdog currently accessible.
/// <para>
///   Use the method <c>YWatchdog.nextWatchdog()</c> to iterate on
///   next watchdog.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YWatchdog</c> object, corresponding to
///   the first watchdog currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstWatchdog():TYWatchdog;

//--- (end of Watchdog functions declaration)

implementation

//--- (YWatchdog implementation)

var
   _WatchdogCache : TStringList;

constructor TYWatchdogDelayedPulse.Create();
 begin
   target := YAPI_INVALID_LONGINT;
   ms := YAPI_INVALID_LONGINT;
   moving := YAPI_INVALID_LONGINT;
 end;

constructor TYWatchdog.Create(func:string);
 begin
   inherited Create('Watchdog', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _state := Y_STATE_INVALID;
   _output := Y_OUTPUT_INVALID;
   _pulseTimer := Y_PULSETIMER_INVALID;
   _delayedPulseTimer := TYWatchdogDelayedPulse.Create();
   _countdown := Y_COUNTDOWN_INVALID;
   _autoStart := Y_AUTOSTART_INVALID;
   _running := Y_RUNNING_INVALID;
   _triggerDelay := Y_TRIGGERDELAY_INVALID;
   _triggerDuration := Y_TRIGGERDURATION_INVALID;
 end;

{$HINTS OFF}
function TYWatchdog._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'autoStart') then
       begin
         _autoStart := member^.ivalue;
       end else
      if (member^.name = 'running') then
       begin
         _running := member^.ivalue;
       end else
      if (member^.name = 'triggerDelay') then
       begin
         _triggerDelay := member^.ivalue;
       end else
      if (member^.name = 'triggerDuration') then
       begin
         _triggerDuration := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the watchdog.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the watchdog
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYWatchdog.get_logicalName():string;
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
///   Changes the logical name of the watchdog.
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
///   a string corresponding to the logical name of the watchdog
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
function TYWatchdog.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the watchdog (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the watchdog (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYWatchdog.get_advertisedValue():string;
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
///   Returns the state of the watchdog (A for the idle position, B for the active position).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_STATE_A or Y_STATE_B, according to the state of the watchdog (A for the idle position, B
///   for the active position)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_STATE_INVALID.
/// </para>
///-
function TYWatchdog.get_state():Integer;
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
///   Changes the state of the watchdog (A for the idle position, B for the active position).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   either Y_STATE_A or Y_STATE_B, according to the state of the watchdog (A for the idle position, B
///   for the active position)
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
function TYWatchdog.set_state(newval:Integer):integer;
 var
   rest_val: string;
 begin
   if(newval>0) then rest_val := '1' else rest_val := '0';
   result := _setAttr('state',rest_val);
 end;

////
/// <summary>
///   Returns the output state of the watchdog, when used as a simple switch (single throw).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_OUTPUT_OFF or Y_OUTPUT_ON, according to the output state of the watchdog, when used as a
///   simple switch (single throw)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_OUTPUT_INVALID.
/// </para>
///-
function TYWatchdog.get_output():Integer;
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
///   Changes the output state of the watchdog, when used as a simple switch (single throw).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   either Y_OUTPUT_OFF or Y_OUTPUT_ON, according to the output state of the watchdog, when used as a
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
function TYWatchdog.set_output(newval:Integer):integer;
 var
   rest_val: string;
 begin
   if(newval>0) then rest_val := '1' else rest_val := '0';
   result := _setAttr('output',rest_val);
 end;

////
/// <summary>
///   Returns the number of milliseconds remaining before the watchdog is returned to idle position
///   (state A), during a measured pulse generation.
/// <para>
///   When there is no ongoing pulse, returns zero.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the number of milliseconds remaining before the watchdog is returned to
///   idle position
///   (state A), during a measured pulse generation
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_PULSETIMER_INVALID.
/// </para>
///-
function TYWatchdog.get_pulseTimer():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PULSETIMER_INVALID;
         exit;
       end;
   result := _pulseTimer;
 end;

function TYWatchdog.set_pulseTimer(newval:LongWord):integer;
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
function TYWatchdog.pulse(ms_duration:integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(ms_duration);
   result := _setAttr('pulseTimer', rest_val);
 end;

function TYWatchdog.get_delayedPulseTimer():TYWatchdogDelayedPulse;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_DELAYEDPULSETIMER_INVALID;
         exit;
       end;
   result := _delayedPulseTimer;
 end;

function TYWatchdog.set_delayedPulseTimer(newval:TYWatchdogDelayedPulse):integer;
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
function TYWatchdog.delayedPulse(ms_delay:integer;ms_duration:integer):integer;
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
function TYWatchdog.get_countdown():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_COUNTDOWN_INVALID;
         exit;
       end;
   result := _countdown;
 end;

////
/// <summary>
///   Returns the watchdog runing state at module power up.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_AUTOSTART_OFF or Y_AUTOSTART_ON, according to the watchdog runing state at module power up
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_AUTOSTART_INVALID.
/// </para>
///-
function TYWatchdog.get_autoStart():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_AUTOSTART_INVALID;
         exit;
       end;
   result := _autoStart;
 end;

////
/// <summary>
///   Changes the watchdog runningsttae at module power up.
/// <para>
///   Remember to call the
///   saveToFlash() method and then to reboot the module to apply this setting.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   either Y_AUTOSTART_OFF or Y_AUTOSTART_ON, according to the watchdog runningsttae at module power up
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
function TYWatchdog.set_autoStart(newval:Integer):integer;
 var
   rest_val: string;
 begin
   if(newval>0) then rest_val := '1' else rest_val := '0';
   result := _setAttr('autoStart',rest_val);
 end;

////
/// <summary>
///   Returns the watchdog running state.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_RUNNING_OFF or Y_RUNNING_ON, according to the watchdog running state
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_RUNNING_INVALID.
/// </para>
///-
function TYWatchdog.get_running():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_RUNNING_INVALID;
         exit;
       end;
   result := _running;
 end;

////
/// <summary>
///   Changes the running state of the watchdog.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   either Y_RUNNING_OFF or Y_RUNNING_ON, according to the running state of the watchdog
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
function TYWatchdog.set_running(newval:Integer):integer;
 var
   rest_val: string;
 begin
   if(newval>0) then rest_val := '1' else rest_val := '0';
   result := _setAttr('running',rest_val);
 end;

////
/// <summary>
///   Resets the watchdog.
/// <para>
///   When the watchdog is running, this function
///   must be called on a regular basis to prevent the watchog to
///   trigger
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYWatchdog.resetWatchdog():integer;
 var
   rest_val: string;
 begin
   rest_val := '1';
   result := _setAttr('running', rest_val);
 end;

////
/// <summary>
///   Returns  the waiting duration before a reset is automatically triggered by the watchdog, in milliseconds.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to  the waiting duration before a reset is automatically triggered by the
///   watchdog, in milliseconds
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_TRIGGERDELAY_INVALID.
/// </para>
///-
function TYWatchdog.get_triggerDelay():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_TRIGGERDELAY_INVALID;
         exit;
       end;
   result := _triggerDelay;
 end;

////
/// <summary>
///   Changes the waiting delay before a reset is triggered by the watchdog, in milliseconds.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the waiting delay before a reset is triggered by the watchdog, in milliseconds
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
function TYWatchdog.set_triggerDelay(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('triggerDelay',rest_val);
 end;

////
/// <summary>
///   Returns the duration of resets caused by the watchdog, in milliseconds.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the duration of resets caused by the watchdog, in milliseconds
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_TRIGGERDURATION_INVALID.
/// </para>
///-
function TYWatchdog.get_triggerDuration():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_TRIGGERDURATION_INVALID;
         exit;
       end;
   result := _triggerDuration;
 end;

////
/// <summary>
///   Changes the duration of resets caused by the watchdog, in milliseconds.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the duration of resets caused by the watchdog, in milliseconds
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
function TYWatchdog.set_triggerDuration(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('triggerDuration',rest_val);
 end;

function TYWatchdog.nextWatchdog(): TYWatchdog;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextWatchdog := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextWatchdog := nil;
      exit;
    end;
    nextWatchdog := yFindWatchdog(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYWatchdog.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYWatchdog.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYWatchdog.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYWatchdog.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YWatchdog implementation)

//--- (Watchdog functions)

function yFindWatchdog(func:string): TYWatchdog;
 var
   index: integer;
   res  : TYWatchdog;
 begin
    if (_WatchdogCache.Find(func, index)) then
     begin
       yFindWatchdog := TYWatchdog(_WatchdogCache.objects[index]);
       exit;
     end;
   res := TYWatchdog.Create(func);
   _WatchdogCache.addObject(func, res);
   yFindWatchdog := res;
 end;

function yFirstWatchdog(): TYWatchdog;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('Watchdog', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstWatchdog := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstWatchdog := nil;
       exit;
    end;
   yFirstWatchdog := yFindWatchdog(serial+'.'+funcId);
 end;

procedure _WatchdogCleanup();
  var i:integer;
begin
  for i:=0 to _WatchdogCache.count-1 do 
    begin
     _WatchdogCache.objects[i].free();
     _WatchdogCache.objects[i]:=nil;
    end;
   _WatchdogCache.free();
   _WatchdogCache:=nil;
end;

//--- (end of Watchdog functions)

initialization
   //--- (Watchdog initialization)
   _WatchdogCache        := TstringList.create();
   _WatchdogCache.sorted := true;
   Y_DELAYEDPULSETIMER_INVALID := TYWatchdogDelayedPulse.Create();
   //--- (end of Watchdog initialization)

finalization
   //--- (Watchdog cleanup)
   _WatchdogCleanup();
   //--- (end of Watchdog cleanup)
end.
