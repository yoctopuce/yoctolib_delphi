{*********************************************************************
 *
 * $Id: yocto_vsource.pas 10411 2013-03-17 21:54:18Z seb $
 *
 * Implements yFindVSource(), the high-level API for VSource functions
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


unit yocto_vsource;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YVSource definitions)
type TYVSourceMove = class(TObject)
public
   target      : longint;
   ms          : longint;
   moving      : longint;
   constructor Create();
end;
type TYVSourcePulse = class(TObject)
public
   target      : longint;
   ms          : longint;
   moving      : longint;
   constructor Create();
end;

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_UNIT_INVALID                  = YAPI_INVALID_STRING;
   Y_VOLTAGE_INVALID               = YAPI_INVALID_LONGINT;
   Y_FAILURE_FALSE = 0;
   Y_FAILURE_TRUE = 1;
   Y_FAILURE_INVALID = -1;

   Y_OVERHEAT_FALSE = 0;
   Y_OVERHEAT_TRUE = 1;
   Y_OVERHEAT_INVALID = -1;

   Y_OVERCURRENT_FALSE = 0;
   Y_OVERCURRENT_TRUE = 1;
   Y_OVERCURRENT_INVALID = -1;

   Y_OVERLOAD_FALSE = 0;
   Y_OVERLOAD_TRUE = 1;
   Y_OVERLOAD_INVALID = -1;

   Y_REGULATIONFAILURE_FALSE = 0;
   Y_REGULATIONFAILURE_TRUE = 1;
   Y_REGULATIONFAILURE_INVALID = -1;

   Y_EXTPOWERFAILURE_FALSE = 0;
   Y_EXTPOWERFAILURE_TRUE = 1;
   Y_EXTPOWERFAILURE_INVALID = -1;


var Y_MOVE_INVALID : TYVSourceMove;
var Y_PULSETIMER_INVALID : TYVSourcePulse;

//--- (end of YVSource definitions)

type
//--- (YVSource declaration)
 TYVSource = class;
 TUpdateCallback  = procedure(func: TYVSource; value:string);
////
/// <summary>
///   TYVSource Class: Voltage source function interface
/// <para>
///   Yoctopuce application programming interface allows you to control
///   the module voltage output. You affect absolute output values or make
///   transitions
/// </para>
/// </summary>
///-
TYVSource=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _unit                     : string;
   _voltage                  : LongInt;
   _failure                  : Integer;
   _overHeat                 : Integer;
   _overCurrent              : Integer;
   _overLoad                 : Integer;
   _regulationFailure        : Integer;
   _extPowerFailure          : Integer;
   _move                     : TYVSourceMove;
   _pulseTimer               : TYVSourcePulse;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YVSource declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of voltage sources started using <c>yFirstVSource()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YVSource</c> object, corresponding to
   ///   a voltage source currently online, or a <c>null</c> pointer
   ///   if there are no more voltage sources to enumerate.
   /// </returns>
   ///-
   function nextVSource():TYVSource;

   //--- (YVSource accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the voltage source.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the voltage source
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the voltage source.
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
   ///   a string corresponding to the logical name of the voltage source
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
   ///   Returns the current value of the voltage source (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the voltage source (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the measuring unit for the voltage.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the measuring unit for the voltage
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_UNIT_INVALID</c>.
   /// </para>
   ///-
   function get_unit():string;

   ////
   /// <summary>
   ///   Returns the voltage output command (mV)
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the voltage output command (mV)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_VOLTAGE_INVALID</c>.
   /// </para>
   ///-
   function get_voltage():LongInt;

   ////
   /// <summary>
   ///   Tunes the device output voltage (milliVolts).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer
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
   function set_voltage(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns true if the  module is in failure mode.
   /// <para>
   ///   More information can be obtained by testing
   ///   get_overheat, get_overcurrent etc... When a error condition is met, the output voltage is
   ///   set to zéro and cannot be changed until the reset() function is called.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_FAILURE_FALSE</c> or <c>Y_FAILURE_TRUE</c>, according to true if the  module is in failure mode
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_FAILURE_INVALID</c>.
   /// </para>
   ///-
   function get_failure():Integer;

   function set_failure(newval:Integer):integer;

   ////
   /// <summary>
   ///   Returns TRUE if the  module is overheating.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_OVERHEAT_FALSE</c> or <c>Y_OVERHEAT_TRUE</c>, according to TRUE if the  module is overheating
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_OVERHEAT_INVALID</c>.
   /// </para>
   ///-
   function get_overHeat():Integer;

   ////
   /// <summary>
   ///   Returns true if the appliance connected to the device is too greedy .
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_OVERCURRENT_FALSE</c> or <c>Y_OVERCURRENT_TRUE</c>, according to true if the appliance
   ///   connected to the device is too greedy
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_OVERCURRENT_INVALID</c>.
   /// </para>
   ///-
   function get_overCurrent():Integer;

   ////
   /// <summary>
   ///   Returns true if the device is not able to maintaint the requested voltage output  .
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_OVERLOAD_FALSE</c> or <c>Y_OVERLOAD_TRUE</c>, according to true if the device is not
   ///   able to maintaint the requested voltage output
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_OVERLOAD_INVALID</c>.
   /// </para>
   ///-
   function get_overLoad():Integer;

   ////
   /// <summary>
   ///   Returns true if the voltage output is too high regarding the requested voltage  .
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_REGULATIONFAILURE_FALSE</c> or <c>Y_REGULATIONFAILURE_TRUE</c>, according to true if
   ///   the voltage output is too high regarding the requested voltage
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_REGULATIONFAILURE_INVALID</c>.
   /// </para>
   ///-
   function get_regulationFailure():Integer;

   ////
   /// <summary>
   ///   Returns true if external power supply voltage is too low.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_EXTPOWERFAILURE_FALSE</c> or <c>Y_EXTPOWERFAILURE_TRUE</c>, according to true if
   ///   external power supply voltage is too low
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_EXTPOWERFAILURE_INVALID</c>.
   /// </para>
   ///-
   function get_extPowerFailure():Integer;

   function get_move():TYVSourceMove;

   function set_move(newval:TYVSourceMove):integer;

   ////
   /// <summary>
   ///   Performs a smooth move at constant speed toward a given value.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="target">
   ///   new output value at end of transition, in milliVolts.
   /// </param>
   /// <param name="ms_duration">
   ///   transition duration, in milliseconds
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
   function voltageMove(target:integer;ms_duration:integer):integer;

   function get_pulseTimer():TYVSourcePulse;

   function set_pulseTimer(newval:TYVSourcePulse):integer;

   ////
   /// <summary>
   ///   Sets device output to a specific volatage, for a specified duration, then brings it
   ///   automatically to 0V.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="voltage">
   ///   pulse voltage, in millivolts
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
   function pulse(voltage:integer;ms_duration:integer):integer;

   //--- (end of YVSource accessors declaration)
end;

//--- (VSource functions declaration)

////
/// <summary>
///   Retrieves a voltage source for a given identifier.
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
///   This function does not require that the voltage source is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YVSource.isOnline()</c> to test if the voltage source is
///   indeed online at a given time. In case of ambiguity when looking for
///   a voltage source by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the voltage source
/// </param>
/// <returns>
///   a <c>YVSource</c> object allowing you to drive the voltage source.
/// </returns>
///-
function yFindVSource(func:string):TYVSource;
////
/// <summary>
///   Starts the enumeration of voltage sources currently accessible.
/// <para>
///   Use the method <c>YVSource.nextVSource()</c> to iterate on
///   next voltage sources.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YVSource</c> object, corresponding to
///   the first voltage source currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstVSource():TYVSource;

//--- (end of VSource functions declaration)

implementation

//--- (YVSource implementation)

var
   _VSourceCache : TStringList;

constructor TYVSourceMove.Create();
 begin
   target := YAPI_INVALID_LONGINT;
   ms := YAPI_INVALID_LONGINT;
   moving := YAPI_INVALID_LONGINT;
 end;

constructor TYVSourcePulse.Create();
 begin
   target := YAPI_INVALID_LONGINT;
   ms := YAPI_INVALID_LONGINT;
   moving := YAPI_INVALID_LONGINT;
 end;

constructor TYVSource.Create(func:string);
 begin
   inherited Create('VSource', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _unit := Y_UNIT_INVALID;
   _voltage := Y_VOLTAGE_INVALID;
   _failure := Y_FAILURE_INVALID;
   _overHeat := Y_OVERHEAT_INVALID;
   _overCurrent := Y_OVERCURRENT_INVALID;
   _overLoad := Y_OVERLOAD_INVALID;
   _regulationFailure := Y_REGULATIONFAILURE_INVALID;
   _extPowerFailure := Y_EXTPOWERFAILURE_INVALID;
   _move := TYVSourceMove.Create();
   _pulseTimer := TYVSourcePulse.Create();
 end;

{$HINTS OFF}
function TYVSource._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'unit') then
       begin
         _unit := string(member^.svalue);
       end else
      if (member^.name = 'voltage') then
       begin
         _voltage := member^.ivalue;
       end else
      if (member^.name = 'failure') then
       begin
         _failure := member^.ivalue;
       end else
      if (member^.name = 'overHeat') then
       begin
         _overHeat := member^.ivalue;
       end else
      if (member^.name = 'overCurrent') then
       begin
         _overCurrent := member^.ivalue;
       end else
      if (member^.name = 'overLoad') then
       begin
         _overLoad := member^.ivalue;
       end else
      if (member^.name = 'regulationFailure') then
       begin
         _regulationFailure := member^.ivalue;
       end else
      if (member^.name = 'extPowerFailure') then
       begin
         _extPowerFailure := member^.ivalue;
       end else
      if (member^.name = 'move') then
       begin
         if (member^.recordtype <> JSON_STRUCT) then begin _parse:= -1; exit; end;
         for l:=0 to member^.membercount-1 do
          begin
            sub := member^.members[l];
            if (sub^.name = 'moving') then
               _move.moving := sub^.ivalue else
            if (sub^.name = 'target') then
               _move.target := sub^.ivalue else
            if (sub^.name = 'ms') then
               _move.ms := sub^.ivalue;
          end; 
         
       end else
      if (member^.name = 'pulseTimer') then
       begin
         if (member^.recordtype <> JSON_STRUCT) then begin _parse:= -1; exit; end;
         for l:=0 to member^.membercount-1 do
          begin
            sub := member^.members[l];
            if (sub^.name = 'moving') then
               _pulseTimer.moving := sub^.ivalue else
            if (sub^.name = 'target') then
               _pulseTimer.target := sub^.ivalue else
            if (sub^.name = 'ms') then
               _pulseTimer.ms := sub^.ivalue;
          end; 
         
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the voltage source.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the voltage source
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYVSource.get_logicalName():string;
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
///   Changes the logical name of the voltage source.
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
///   a string corresponding to the logical name of the voltage source
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
function TYVSource.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the voltage source (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the voltage source (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYVSource.get_advertisedValue():string;
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
///   Returns the measuring unit for the voltage.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the measuring unit for the voltage
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_UNIT_INVALID.
/// </para>
///-
function TYVSource.get_unit():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_UNIT_INVALID;
         exit;
       end;
   result := _unit;
 end;

////
/// <summary>
///   Returns the voltage output command (mV)
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the voltage output command (mV)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_VOLTAGE_INVALID.
/// </para>
///-
function TYVSource.get_voltage():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_VOLTAGE_INVALID;
         exit;
       end;
   result := _voltage;
 end;

////
/// <summary>
///   Tunes the device output voltage (milliVolts).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer
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
function TYVSource.set_voltage(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('voltage',rest_val);
 end;

////
/// <summary>
///   Returns true if the  module is in failure mode.
/// <para>
///   More information can be obtained by testing
///   get_overheat, get_overcurrent etc... When a error condition is met, the output voltage is
///   set to zéro and cannot be changed until the reset() function is called.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_FAILURE_FALSE or Y_FAILURE_TRUE, according to true if the  module is in failure mode
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_FAILURE_INVALID.
/// </para>
///-
function TYVSource.get_failure():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_FAILURE_INVALID;
         exit;
       end;
   result := _failure;
 end;

function TYVSource.set_failure(newval:Integer):integer;
 var
   rest_val: string;
 begin
   if(newval>0) then rest_val := '1' else rest_val := '0';
   result := _setAttr('failure',rest_val);
 end;

////
/// <summary>
///   Returns TRUE if the  module is overheating.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_OVERHEAT_FALSE or Y_OVERHEAT_TRUE, according to TRUE if the  module is overheating
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_OVERHEAT_INVALID.
/// </para>
///-
function TYVSource.get_overHeat():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_OVERHEAT_INVALID;
         exit;
       end;
   result := _overHeat;
 end;

////
/// <summary>
///   Returns true if the appliance connected to the device is too greedy .
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_OVERCURRENT_FALSE or Y_OVERCURRENT_TRUE, according to true if the appliance connected to
///   the device is too greedy
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_OVERCURRENT_INVALID.
/// </para>
///-
function TYVSource.get_overCurrent():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_OVERCURRENT_INVALID;
         exit;
       end;
   result := _overCurrent;
 end;

////
/// <summary>
///   Returns true if the device is not able to maintaint the requested voltage output  .
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_OVERLOAD_FALSE or Y_OVERLOAD_TRUE, according to true if the device is not able to
///   maintaint the requested voltage output
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_OVERLOAD_INVALID.
/// </para>
///-
function TYVSource.get_overLoad():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_OVERLOAD_INVALID;
         exit;
       end;
   result := _overLoad;
 end;

////
/// <summary>
///   Returns true if the voltage output is too high regarding the requested voltage  .
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_REGULATIONFAILURE_FALSE or Y_REGULATIONFAILURE_TRUE, according to true if the voltage
///   output is too high regarding the requested voltage
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_REGULATIONFAILURE_INVALID.
/// </para>
///-
function TYVSource.get_regulationFailure():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_REGULATIONFAILURE_INVALID;
         exit;
       end;
   result := _regulationFailure;
 end;

////
/// <summary>
///   Returns true if external power supply voltage is too low.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_EXTPOWERFAILURE_FALSE or Y_EXTPOWERFAILURE_TRUE, according to true if external power
///   supply voltage is too low
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_EXTPOWERFAILURE_INVALID.
/// </para>
///-
function TYVSource.get_extPowerFailure():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_EXTPOWERFAILURE_INVALID;
         exit;
       end;
   result := _extPowerFailure;
 end;

function TYVSource.get_move():TYVSourceMove;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_MOVE_INVALID;
         exit;
       end;
   result := _move;
 end;

function TYVSource.set_move(newval:TYVSourceMove):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval.target)+':'+inttostr(newval.ms);
   result := _setAttr('move',rest_val);
 end;

////
/// <summary>
///   Performs a smooth move at constant speed toward a given value.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="target">
///   new output value at end of transition, in milliVolts.
/// </param>
/// <param name="ms_duration">
///   transition duration, in milliseconds
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
function TYVSource.voltageMove(target:integer;ms_duration:integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(target)+':'+inttostr(ms_duration);
   result := _setAttr('move', rest_val);
 end;

function TYVSource.get_pulseTimer():TYVSourcePulse;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PULSETIMER_INVALID;
         exit;
       end;
   result := _pulseTimer;
 end;

function TYVSource.set_pulseTimer(newval:TYVSourcePulse):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval.target)+':'+inttostr(newval.ms);
   result := _setAttr('pulseTimer',rest_val);
 end;

////
/// <summary>
///   Sets device output to a specific volatage, for a specified duration, then brings it
///   automatically to 0V.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="voltage">
///   pulse voltage, in millivolts
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
function TYVSource.pulse(voltage:integer;ms_duration:integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(voltage)+':'+inttostr(ms_duration);
   result := _setAttr('pulseTimer', rest_val);
 end;

function TYVSource.nextVSource(): TYVSource;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextVSource := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextVSource := nil;
      exit;
    end;
    nextVSource := yFindVSource(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYVSource.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYVSource.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYVSource.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYVSource.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YVSource implementation)

//--- (VSource functions)

function yFindVSource(func:string): TYVSource;
 var
   index: integer;
   res  : TYVSource;
 begin
    if (_VSourceCache.Find(func, index)) then
     begin
       yFindVSource := TYVSource(_VSourceCache.objects[index]);
       exit;
     end;
   res := TYVSource.Create(func);
   _VSourceCache.addObject(func, res);
   yFindVSource := res;
 end;

function yFirstVSource(): TYVSource;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('VSource', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstVSource := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstVSource := nil;
       exit;
    end;
   yFirstVSource := yFindVSource(serial+'.'+funcId);
 end;

procedure _VSourceCleanup();
  var i:integer;
begin
  for i:=0 to _VSourceCache.count-1 do 
    begin
     _VSourceCache.objects[i].free();
     _VSourceCache.objects[i]:=nil;
    end;
   _VSourceCache.free();
   _VSourceCache:=nil;
end;

//--- (end of VSource functions)

initialization
   //--- (VSource initialization)
   _VSourceCache        := TstringList.create();
   _VSourceCache.sorted := true;
   Y_MOVE_INVALID := TYVSourceMove.Create();
   Y_PULSETIMER_INVALID := TYVSourcePulse.Create();
   //--- (end of VSource initialization)

finalization
   //--- (VSource cleanup)
   _VSourceCleanup();
   //--- (end of VSource cleanup)
end.
