{*********************************************************************
 *
 * $Id: pic24config.php 12323 2013-08-13 15:09:18Z mvuilleu $
 *
 * Implements yFindDigitalIO(), the high-level API for DigitalIO functions
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


unit yocto_digitalio;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YDigitalIO definitions)

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_PORTSTATE_INVALID             = -1;
   Y_PORTDIRECTION_INVALID         = -1;
   Y_PORTOPENDRAIN_INVALID         = -1;
   Y_PORTSIZE_INVALID              = YAPI_INVALID_LONGWORD;
   Y_OUTPUTVOLTAGE_USB_5V = 0;
   Y_OUTPUTVOLTAGE_USB_3V3 = 1;
   Y_OUTPUTVOLTAGE_EXT_V = 2;
   Y_OUTPUTVOLTAGE_INVALID = -1;

   Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YDigitalIO definitions)

type
//--- (YDigitalIO declaration)
 TYDigitalIO = class;
 TUpdateCallback  = procedure(func: TYDigitalIO; value:string);
////
/// <summary>
///   TYDigitalIO Class: Digital IO function interface
/// <para>
///   ....
/// </para>
/// </summary>
///-
TYDigitalIO=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _portState                : LongInt;
   _portDirection            : LongInt;
   _portOpenDrain            : LongInt;
   _portSize                 : LongWord;
   _outputVoltage            : Integer;
   _command                  : string;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YDigitalIO declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of digital IO port started using <c>yFirstDigitalIO()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YDigitalIO</c> object, corresponding to
   ///   a digital IO port currently online, or a <c>null</c> pointer
   ///   if there are no more digital IO port to enumerate.
   /// </returns>
   ///-
   function nextDigitalIO():TYDigitalIO;

   //--- (YDigitalIO accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the digital IO port.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the digital IO port
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the digital IO port.
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
   ///   a string corresponding to the logical name of the digital IO port
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
   ///   Returns the current value of the digital IO port (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the digital IO port (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the digital IO port state: bit 0 represents input 0, and so on.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the digital IO port state: bit 0 represents input 0, and so on
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_PORTSTATE_INVALID</c>.
   /// </para>
   ///-
   function get_portState():LongInt;

   ////
   /// <summary>
   ///   Changes the digital IO port state: bit 0 represents input 0, and so on.
   /// <para>
   ///   This function has no effect
   ///   on bits configured as input in <c>portDirection</c>.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the digital IO port state: bit 0 represents input 0, and so on
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
   function set_portState(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns the IO direction of all bits of the port: 0 makes a bit an input, 1 makes it an output.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the IO direction of all bits of the port: 0 makes a bit an input, 1
   ///   makes it an output
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_PORTDIRECTION_INVALID</c>.
   /// </para>
   ///-
   function get_portDirection():LongInt;

   ////
   /// <summary>
   ///   Changes the IO direction of all bits of the port: 0 makes a bit an input, 1 makes it an output.
   /// <para>
   ///   Remember to call the <c>saveToFlash()</c> method  to make sure the setting will be kept after a reboot.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the IO direction of all bits of the port: 0 makes a bit an input, 1
   ///   makes it an output
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
   function set_portDirection(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns the electrical interface for each bit of the port.
   /// <para>
   ///   0 makes a bit a regular input/output, 1 makes
   ///   it an open-drain (open-collector) input/output.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the electrical interface for each bit of the port
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_PORTOPENDRAIN_INVALID</c>.
   /// </para>
   ///-
   function get_portOpenDrain():LongInt;

   ////
   /// <summary>
   ///   Changes the electrical interface for each bit of the port.
   /// <para>
   ///   0 makes a bit a regular input/output, 1 makes
   ///   it an open-drain (open-collector) input/output. Remember to call the
   ///   <c>saveToFlash()</c> method  to make sure the setting will be kept after a reboot.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the electrical interface for each bit of the port
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
   function set_portOpenDrain(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns the number of bits implemented in the I/O port.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the number of bits implemented in the I/O port
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_PORTSIZE_INVALID</c>.
   /// </para>
   ///-
   function get_portSize():LongWord;

   ////
   /// <summary>
   ///   Returns the voltage source used to drive output bits.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a value among <c>Y_OUTPUTVOLTAGE_USB_5V</c>, <c>Y_OUTPUTVOLTAGE_USB_3V3</c> and
   ///   <c>Y_OUTPUTVOLTAGE_EXT_V</c> corresponding to the voltage source used to drive output bits
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_OUTPUTVOLTAGE_INVALID</c>.
   /// </para>
   ///-
   function get_outputVoltage():Integer;

   ////
   /// <summary>
   ///   Changes the voltage source used to drive output bits.
   /// <para>
   ///   Remember to call the <c>saveToFlash()</c> method  to make sure the setting will be kept after a reboot.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   a value among <c>Y_OUTPUTVOLTAGE_USB_5V</c>, <c>Y_OUTPUTVOLTAGE_USB_3V3</c> and
   ///   <c>Y_OUTPUTVOLTAGE_EXT_V</c> corresponding to the voltage source used to drive output bits
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
   function set_outputVoltage(newval:Integer):integer;

   function get_command():string;

   function set_command(newval:string):integer;

   ////
   /// <summary>
   ///   Set a single bit of the I/O port.
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="bitno">
   ///   the bit number; lowest bit is index 0
   /// </param>
   /// <param name="bitval">
   ///   the value of the bit (1 or 0)
   /// </param>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function set_bitState(bitno:integer; bitval:integer):integer;

   ////
   /// <summary>
   ///   Returns the value of a single bit of the I/O port.
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="bitno">
   ///   the bit number; lowest bit is index 0
   /// </param>
   /// <returns>
   ///   the bit value (0 or 1)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function get_bitState(bitno:integer):integer;

   ////
   /// <summary>
   ///   Revert a single bit of the I/O port.
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="bitno">
   ///   the bit number; lowest bit is index 0
   /// </param>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function toggle_bitState(bitno:integer):integer;

   ////
   /// <summary>
   ///   Change  the direction of a single bit from the I/O port.
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="bitno">
   ///   the bit number; lowest bit is index 0
   /// </param>
   /// <param name="bitdirection">
   ///   direction to set, 0 makes the bit an input, 1 makes it an output.
   ///   Remember to call the   <c>saveToFlash()</c> method to make sure the setting will be kept after a reboot.
   /// </param>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function set_bitDirection(bitno:integer; bitdirection:integer):integer;

   ////
   /// <summary>
   ///   Change  the direction of a single bit from the I/O port (0 means the bit is an input, 1  an output).
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="bitno">
   ///   the bit number; lowest bit is index 0
   /// </param>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function get_bitDirection(bitno:integer):integer;

   ////
   /// <summary>
   ///   Change  the electrical interface of a single bit from the I/O port.
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="bitno">
   ///   the bit number; lowest bit is index 0
   /// </param>
   /// <param name="opendrain">
   ///   value to set, 0 makes a bit a regular input/output, 1 makes
   ///   it an open-drain (open-collector) input/output. Remember to call the
   ///   <c>saveToFlash()</c> method to make sure the setting will be kept after a reboot.
   /// </param>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function set_bitOpenDrain(bitno:integer; opendrain:integer):integer;

   ////
   /// <summary>
   ///   Returns the type of electrical interface of a single bit from the I/O port.
   /// <para>
   ///   (0 means the bit is an input, 1  an output).
   /// </para>
   /// </summary>
   /// <param name="bitno">
   ///   the bit number; lowest bit is index 0
   /// </param>
   /// <returns>
   ///   0 means the a bit is a regular input/output, 1means the b it an open-drain (open-collector) input/output.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function get_bitOpenDrain(bitno:integer):integer;

   //--- (end of YDigitalIO accessors declaration)
end;

//--- (DigitalIO functions declaration)

////
/// <summary>
///   Retrieves a digital IO port for a given identifier.
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
///   This function does not require that the digital IO port is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YDigitalIO.isOnline()</c> to test if the digital IO port is
///   indeed online at a given time. In case of ambiguity when looking for
///   a digital IO port by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the digital IO port
/// </param>
/// <returns>
///   a <c>YDigitalIO</c> object allowing you to drive the digital IO port.
/// </returns>
///-
function yFindDigitalIO(func:string):TYDigitalIO;
////
/// <summary>
///   Starts the enumeration of digital IO port currently accessible.
/// <para>
///   Use the method <c>YDigitalIO.nextDigitalIO()</c> to iterate on
///   next digital IO port.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YDigitalIO</c> object, corresponding to
///   the first digital IO port currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstDigitalIO():TYDigitalIO;

//--- (end of DigitalIO functions declaration)

implementation

//--- (YDigitalIO implementation)

var
   _DigitalIOCache : TStringList;

constructor TYDigitalIO.Create(func:string);
 begin
   inherited Create('DigitalIO', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _portState := Y_PORTSTATE_INVALID;
   _portDirection := Y_PORTDIRECTION_INVALID;
   _portOpenDrain := Y_PORTOPENDRAIN_INVALID;
   _portSize := Y_PORTSIZE_INVALID;
   _outputVoltage := Y_OUTPUTVOLTAGE_INVALID;
   _command := Y_COMMAND_INVALID;
 end;

{$HINTS OFF}
function TYDigitalIO._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'portState') then
       begin
         _portState := member^.ivalue;
       end else
      if (member^.name = 'portDirection') then
       begin
         _portDirection := member^.ivalue;
       end else
      if (member^.name = 'portOpenDrain') then
       begin
         _portOpenDrain := member^.ivalue;
       end else
      if (member^.name = 'portSize') then
       begin
         _portSize := member^.ivalue;
       end else
      if (member^.name = 'outputVoltage') then
       begin
         _outputVoltage := member^.ivalue;
       end else
      if (member^.name = 'command') then
       begin
         _command := string(member^.svalue);
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the digital IO port.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the digital IO port
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYDigitalIO.get_logicalName():string;
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
///   Changes the logical name of the digital IO port.
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
///   a string corresponding to the logical name of the digital IO port
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
function TYDigitalIO.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the digital IO port (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the digital IO port (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYDigitalIO.get_advertisedValue():string;
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
///   Returns the digital IO port state: bit 0 represents input 0, and so on.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the digital IO port state: bit 0 represents input 0, and so on
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_PORTSTATE_INVALID.
/// </para>
///-
function TYDigitalIO.get_portState():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PORTSTATE_INVALID;
         exit;
       end;
   result := _portState;
 end;

////
/// <summary>
///   Changes the digital IO port state: bit 0 represents input 0, and so on.
/// <para>
///   This function has no effect
///   on bits configured as input in portDirection.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the digital IO port state: bit 0 represents input 0, and so on
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
function TYDigitalIO.set_portState(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('portState',rest_val);
 end;

////
/// <summary>
///   Returns the IO direction of all bits of the port: 0 makes a bit an input, 1 makes it an output.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the IO direction of all bits of the port: 0 makes a bit an input, 1
///   makes it an output
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_PORTDIRECTION_INVALID.
/// </para>
///-
function TYDigitalIO.get_portDirection():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PORTDIRECTION_INVALID;
         exit;
       end;
   result := _portDirection;
 end;

////
/// <summary>
///   Changes the IO direction of all bits of the port: 0 makes a bit an input, 1 makes it an output.
/// <para>
///   Remember to call the saveToFlash() method  to make sure the setting will be kept after a reboot.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the IO direction of all bits of the port: 0 makes a bit an input, 1
///   makes it an output
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
function TYDigitalIO.set_portDirection(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('portDirection',rest_val);
 end;

////
/// <summary>
///   Returns the electrical interface for each bit of the port.
/// <para>
///   0 makes a bit a regular input/output, 1 makes
///   it an open-drain (open-collector) input/output.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the electrical interface for each bit of the port
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_PORTOPENDRAIN_INVALID.
/// </para>
///-
function TYDigitalIO.get_portOpenDrain():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PORTOPENDRAIN_INVALID;
         exit;
       end;
   result := _portOpenDrain;
 end;

////
/// <summary>
///   Changes the electrical interface for each bit of the port.
/// <para>
///   0 makes a bit a regular input/output, 1 makes
///   it an open-drain (open-collector) input/output. Remember to call the
///   saveToFlash() method  to make sure the setting will be kept after a reboot.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the electrical interface for each bit of the port
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
function TYDigitalIO.set_portOpenDrain(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('portOpenDrain',rest_val);
 end;

////
/// <summary>
///   Returns the number of bits implemented in the I/O port.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the number of bits implemented in the I/O port
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_PORTSIZE_INVALID.
/// </para>
///-
function TYDigitalIO.get_portSize():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PORTSIZE_INVALID;
         exit;
       end;
   result := _portSize;
 end;

////
/// <summary>
///   Returns the voltage source used to drive output bits.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a value among Y_OUTPUTVOLTAGE_USB_5V, Y_OUTPUTVOLTAGE_USB_3V3 and Y_OUTPUTVOLTAGE_EXT_V
///   corresponding to the voltage source used to drive output bits
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_OUTPUTVOLTAGE_INVALID.
/// </para>
///-
function TYDigitalIO.get_outputVoltage():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_OUTPUTVOLTAGE_INVALID;
         exit;
       end;
   result := _outputVoltage;
 end;

////
/// <summary>
///   Changes the voltage source used to drive output bits.
/// <para>
///   Remember to call the saveToFlash() method  to make sure the setting will be kept after a reboot.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   a value among Y_OUTPUTVOLTAGE_USB_5V, Y_OUTPUTVOLTAGE_USB_3V3 and Y_OUTPUTVOLTAGE_EXT_V
///   corresponding to the voltage source used to drive output bits
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
function TYDigitalIO.set_outputVoltage(newval:Integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('outputVoltage',rest_val);
 end;

function TYDigitalIO.get_command():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_COMMAND_INVALID;
         exit;
       end;
   result := _command;
 end;

function TYDigitalIO.set_command(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('command',rest_val);
 end;

////
/// <summary>
///   Set a single bit of the I/O port.
/// <para>
/// </para>
/// </summary>
/// <param name="bitno">
///   the bit number; lowest bit is index 0
/// </param>
/// <param name="bitval">
///   the value of the bit (1 or 0)
/// </param>
/// <returns>
///   <c>YAPI_SUCCESS</c> if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYDigitalIO.set_bitState(bitno:integer; bitval:integer):integer;
     begin
        if not(bitval >= 0) then begin self._throw( YAPI_INVALID_ARGUMENT, 'invalid bitval'); result:= YAPI_INVALID_ARGUMENT; exit; end;;
        if not(bitval <= 1) then begin self._throw( YAPI_INVALID_ARGUMENT, 'invalid bitval'); result:= YAPI_INVALID_ARGUMENT; exit; end;;
        result:= self.set_command(''+chr(82+bitval)+''+inttostr( bitno)); 
            
     end;


////
/// <summary>
///   Returns the value of a single bit of the I/O port.
/// <para>
/// </para>
/// </summary>
/// <param name="bitno">
///   the bit number; lowest bit is index 0
/// </param>
/// <returns>
///   the bit value (0 or 1)
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYDigitalIO.get_bitState(bitno:integer):integer;
     var
        portVal : integer;
     begin
        portVal := self.get_portState();
        result:= ((((portVal) shr (bitno))) and (1));
            
     end;


////
/// <summary>
///   Revert a single bit of the I/O port.
/// <para>
/// </para>
/// </summary>
/// <param name="bitno">
///   the bit number; lowest bit is index 0
/// </param>
/// <returns>
///   <c>YAPI_SUCCESS</c> if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYDigitalIO.toggle_bitState(bitno:integer):integer;
     begin
        result:= self.set_command('T'+inttostr( bitno)); 
            
     end;


////
/// <summary>
///   Change  the direction of a single bit from the I/O port.
/// <para>
/// </para>
/// </summary>
/// <param name="bitno">
///   the bit number; lowest bit is index 0
/// </param>
/// <param name="bitdirection">
///   direction to set, 0 makes the bit an input, 1 makes it an output.
///   Remember to call the   <c>saveToFlash()</c> method to make sure the setting will be kept after a reboot.
/// </param>
/// <returns>
///   <c>YAPI_SUCCESS</c> if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYDigitalIO.set_bitDirection(bitno:integer; bitdirection:integer):integer;
     begin
        if not(bitdirection >= 0) then begin self._throw( YAPI_INVALID_ARGUMENT, 'invalid direction'); result:= YAPI_INVALID_ARGUMENT; exit; end;;
        if not(bitdirection <= 1) then begin self._throw( YAPI_INVALID_ARGUMENT, 'invalid direction'); result:= YAPI_INVALID_ARGUMENT; exit; end;;
        result:= self.set_command(''+chr(73+6*bitdirection)+''+inttostr( bitno)); 
            
     end;


////
/// <summary>
///   Change  the direction of a single bit from the I/O port (0 means the bit is an input, 1  an output).
/// <para>
/// </para>
/// </summary>
/// <param name="bitno">
///   the bit number; lowest bit is index 0
/// </param>
/// <returns>
///   <c>YAPI_SUCCESS</c> if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYDigitalIO.get_bitDirection(bitno:integer):integer;
     var
        portDir : integer;
     begin
        portDir := self.get_portDirection();
        result:= ((((portDir) shr (bitno))) and (1));
            
     end;


////
/// <summary>
///   Change  the electrical interface of a single bit from the I/O port.
/// <para>
/// </para>
/// </summary>
/// <param name="bitno">
///   the bit number; lowest bit is index 0
/// </param>
/// <param name="opendrain">
///   value to set, 0 makes a bit a regular input/output, 1 makes
///   it an open-drain (open-collector) input/output. Remember to call the
///   <c>saveToFlash()</c> method to make sure the setting will be kept after a reboot.
/// </param>
/// <returns>
///   <c>YAPI_SUCCESS</c> if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYDigitalIO.set_bitOpenDrain(bitno:integer; opendrain:integer):integer;
     begin
        if not(opendrain >= 0) then begin self._throw( YAPI_INVALID_ARGUMENT, 'invalid state'); result:= YAPI_INVALID_ARGUMENT; exit; end;;
        if not(opendrain <= 1) then begin self._throw( YAPI_INVALID_ARGUMENT, 'invalid state'); result:= YAPI_INVALID_ARGUMENT; exit; end;;
        result:= self.set_command(''+chr(100-32*opendrain)+''+inttostr( bitno)); 
            
     end;


////
/// <summary>
///   Returns the type of electrical interface of a single bit from the I/O port.
/// <para>
///   (0 means the bit is an input, 1  an output).
/// </para>
/// </summary>
/// <param name="bitno">
///   the bit number; lowest bit is index 0
/// </param>
/// <returns>
///   0 means the a bit is a regular input/output, 1means the b it an open-drain (open-collector) input/output.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYDigitalIO.get_bitOpenDrain(bitno:integer):integer;
     var
        portOpenDrain : integer;
     begin
        portOpenDrain := self.get_portOpenDrain();
        result:= ((((portOpenDrain) shr (bitno))) and (1));
            
     end;


function TYDigitalIO.nextDigitalIO(): TYDigitalIO;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextDigitalIO := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextDigitalIO := nil;
      exit;
    end;
    nextDigitalIO := yFindDigitalIO(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYDigitalIO.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYDigitalIO.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYDigitalIO.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYDigitalIO.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YDigitalIO implementation)

//--- (DigitalIO functions)

function yFindDigitalIO(func:string): TYDigitalIO;
 var
   index: integer;
   res  : TYDigitalIO;
 begin
    if (_DigitalIOCache.Find(func, index)) then
     begin
       yFindDigitalIO := TYDigitalIO(_DigitalIOCache.objects[index]);
       exit;
     end;
   res := TYDigitalIO.Create(func);
   _DigitalIOCache.addObject(func, res);
   yFindDigitalIO := res;
 end;

function yFirstDigitalIO(): TYDigitalIO;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('DigitalIO', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstDigitalIO := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstDigitalIO := nil;
       exit;
    end;
   yFirstDigitalIO := yFindDigitalIO(serial+'.'+funcId);
 end;

procedure _DigitalIOCleanup();
  var i:integer;
begin
  for i:=0 to _DigitalIOCache.count-1 do 
    begin
     _DigitalIOCache.objects[i].free();
     _DigitalIOCache.objects[i]:=nil;
    end;
   _DigitalIOCache.free();
   _DigitalIOCache:=nil;
end;

//--- (end of DigitalIO functions)

initialization
   //--- (DigitalIO initialization)
   _DigitalIOCache        := TstringList.create();
   _DigitalIOCache.sorted := true;
   //--- (end of DigitalIO initialization)

finalization
   //--- (DigitalIO cleanup)
   _DigitalIOCleanup();
   //--- (end of DigitalIO cleanup)
end.
