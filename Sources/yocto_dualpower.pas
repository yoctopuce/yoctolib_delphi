{*********************************************************************
 *
 * $Id: yocto_dualpower.pas 12324 2013-08-13 15:10:31Z mvuilleu $
 *
 * Implements yFindDualPower(), the high-level API for DualPower functions
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


unit yocto_dualpower;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YDualPower definitions)

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_POWERSTATE_OFF = 0;
   Y_POWERSTATE_FROM_USB = 1;
   Y_POWERSTATE_FROM_EXT = 2;
   Y_POWERSTATE_INVALID = -1;

   Y_POWERCONTROL_AUTO = 0;
   Y_POWERCONTROL_FROM_USB = 1;
   Y_POWERCONTROL_FROM_EXT = 2;
   Y_POWERCONTROL_OFF = 3;
   Y_POWERCONTROL_INVALID = -1;

   Y_EXTVOLTAGE_INVALID            = YAPI_INVALID_LONGWORD;


//--- (end of YDualPower definitions)

type
//--- (YDualPower declaration)
 TYDualPower = class;
 TUpdateCallback  = procedure(func: TYDualPower; value:string);
////
/// <summary>
///   TYDualPower Class: External power supply control interface
/// <para>
///   Yoctopuce application programming interface allows you to control
///   the power source to use for module functions that require high current.
///   The module can also automatically disconnect the external power
///   when a voltage drop is observed on the external power source
///   (external battery running out of power).
/// </para>
/// </summary>
///-
TYDualPower=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _powerState               : Integer;
   _powerControl             : Integer;
   _extVoltage               : LongWord;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YDualPower declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of dual power controls started using <c>yFirstDualPower()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YDualPower</c> object, corresponding to
   ///   a dual power control currently online, or a <c>null</c> pointer
   ///   if there are no more dual power controls to enumerate.
   /// </returns>
   ///-
   function nextDualPower():TYDualPower;

   //--- (YDualPower accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the power control.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the power control
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the power control.
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
   ///   a string corresponding to the logical name of the power control
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
   ///   Returns the current value of the power control (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the power control (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the current power source for module functions that require lots of current.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a value among <c>Y_POWERSTATE_OFF</c>, <c>Y_POWERSTATE_FROM_USB</c> and
   ///   <c>Y_POWERSTATE_FROM_EXT</c> corresponding to the current power source for module functions that
   ///   require lots of current
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_POWERSTATE_INVALID</c>.
   /// </para>
   ///-
   function get_powerState():Integer;

   ////
   /// <summary>
   ///   Returns the selected power source for module functions that require lots of current.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a value among <c>Y_POWERCONTROL_AUTO</c>, <c>Y_POWERCONTROL_FROM_USB</c>,
   ///   <c>Y_POWERCONTROL_FROM_EXT</c> and <c>Y_POWERCONTROL_OFF</c> corresponding to the selected power
   ///   source for module functions that require lots of current
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_POWERCONTROL_INVALID</c>.
   /// </para>
   ///-
   function get_powerControl():Integer;

   ////
   /// <summary>
   ///   Changes the selected power source for module functions that require lots of current.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   a value among <c>Y_POWERCONTROL_AUTO</c>, <c>Y_POWERCONTROL_FROM_USB</c>,
   ///   <c>Y_POWERCONTROL_FROM_EXT</c> and <c>Y_POWERCONTROL_OFF</c> corresponding to the selected power
   ///   source for module functions that require lots of current
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
   function set_powerControl(newval:Integer):integer;

   ////
   /// <summary>
   ///   Returns the measured voltage on the external power source, in millivolts.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the measured voltage on the external power source, in millivolts
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_EXTVOLTAGE_INVALID</c>.
   /// </para>
   ///-
   function get_extVoltage():LongWord;

   //--- (end of YDualPower accessors declaration)
end;

//--- (DualPower functions declaration)

////
/// <summary>
///   Retrieves a dual power control for a given identifier.
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
///   This function does not require that the power control is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YDualPower.isOnline()</c> to test if the power control is
///   indeed online at a given time. In case of ambiguity when looking for
///   a dual power control by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the power control
/// </param>
/// <returns>
///   a <c>YDualPower</c> object allowing you to drive the power control.
/// </returns>
///-
function yFindDualPower(func:string):TYDualPower;
////
/// <summary>
///   Starts the enumeration of dual power controls currently accessible.
/// <para>
///   Use the method <c>YDualPower.nextDualPower()</c> to iterate on
///   next dual power controls.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YDualPower</c> object, corresponding to
///   the first dual power control currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstDualPower():TYDualPower;

//--- (end of DualPower functions declaration)

implementation

//--- (YDualPower implementation)

var
   _DualPowerCache : TStringList;

constructor TYDualPower.Create(func:string);
 begin
   inherited Create('DualPower', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _powerState := Y_POWERSTATE_INVALID;
   _powerControl := Y_POWERCONTROL_INVALID;
   _extVoltage := Y_EXTVOLTAGE_INVALID;
 end;

{$HINTS OFF}
function TYDualPower._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'powerState') then
       begin
         _powerState := member^.ivalue;
       end else
      if (member^.name = 'powerControl') then
       begin
         _powerControl := member^.ivalue;
       end else
      if (member^.name = 'extVoltage') then
       begin
         _extVoltage := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the power control.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the power control
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYDualPower.get_logicalName():string;
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
///   Changes the logical name of the power control.
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
///   a string corresponding to the logical name of the power control
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
function TYDualPower.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the power control (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the power control (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYDualPower.get_advertisedValue():string;
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
///   Returns the current power source for module functions that require lots of current.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a value among Y_POWERSTATE_OFF, Y_POWERSTATE_FROM_USB and Y_POWERSTATE_FROM_EXT corresponding to
///   the current power source for module functions that require lots of current
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_POWERSTATE_INVALID.
/// </para>
///-
function TYDualPower.get_powerState():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_POWERSTATE_INVALID;
         exit;
       end;
   result := _powerState;
 end;

////
/// <summary>
///   Returns the selected power source for module functions that require lots of current.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a value among Y_POWERCONTROL_AUTO, Y_POWERCONTROL_FROM_USB, Y_POWERCONTROL_FROM_EXT and
///   Y_POWERCONTROL_OFF corresponding to the selected power source for module functions that require lots of current
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_POWERCONTROL_INVALID.
/// </para>
///-
function TYDualPower.get_powerControl():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_POWERCONTROL_INVALID;
         exit;
       end;
   result := _powerControl;
 end;

////
/// <summary>
///   Changes the selected power source for module functions that require lots of current.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   a value among Y_POWERCONTROL_AUTO, Y_POWERCONTROL_FROM_USB, Y_POWERCONTROL_FROM_EXT and
///   Y_POWERCONTROL_OFF corresponding to the selected power source for module functions that require lots of current
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
function TYDualPower.set_powerControl(newval:Integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('powerControl',rest_val);
 end;

////
/// <summary>
///   Returns the measured voltage on the external power source, in millivolts.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the measured voltage on the external power source, in millivolts
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_EXTVOLTAGE_INVALID.
/// </para>
///-
function TYDualPower.get_extVoltage():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_EXTVOLTAGE_INVALID;
         exit;
       end;
   result := _extVoltage;
 end;

function TYDualPower.nextDualPower(): TYDualPower;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextDualPower := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextDualPower := nil;
      exit;
    end;
    nextDualPower := yFindDualPower(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYDualPower.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYDualPower.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYDualPower.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYDualPower.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YDualPower implementation)

//--- (DualPower functions)

function yFindDualPower(func:string): TYDualPower;
 var
   index: integer;
   res  : TYDualPower;
 begin
    if (_DualPowerCache.Find(func, index)) then
     begin
       yFindDualPower := TYDualPower(_DualPowerCache.objects[index]);
       exit;
     end;
   res := TYDualPower.Create(func);
   _DualPowerCache.addObject(func, res);
   yFindDualPower := res;
 end;

function yFirstDualPower(): TYDualPower;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('DualPower', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstDualPower := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstDualPower := nil;
       exit;
    end;
   yFirstDualPower := yFindDualPower(serial+'.'+funcId);
 end;

procedure _DualPowerCleanup();
  var i:integer;
begin
  for i:=0 to _DualPowerCache.count-1 do 
    begin
     _DualPowerCache.objects[i].free();
     _DualPowerCache.objects[i]:=nil;
    end;
   _DualPowerCache.free();
   _DualPowerCache:=nil;
end;

//--- (end of DualPower functions)

initialization
   //--- (DualPower initialization)
   _DualPowerCache        := TstringList.create();
   _DualPowerCache.sorted := true;
   //--- (end of DualPower initialization)

finalization
   //--- (DualPower cleanup)
   _DualPowerCleanup();
   //--- (end of DualPower cleanup)
end.
