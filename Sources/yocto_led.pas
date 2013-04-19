{*********************************************************************
 *
 * $Id: yocto_led.pas 10411 2013-03-17 21:54:18Z seb $
 *
 * Implements yFindLed(), the high-level API for Led functions
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


unit yocto_led;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YLed definitions)

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_POWER_OFF = 0;
   Y_POWER_ON = 1;
   Y_POWER_INVALID = -1;

   Y_LUMINOSITY_INVALID            = -1;
   Y_BLINKING_STILL = 0;
   Y_BLINKING_RELAX = 1;
   Y_BLINKING_AWARE = 2;
   Y_BLINKING_RUN = 3;
   Y_BLINKING_CALL = 4;
   Y_BLINKING_PANIC = 5;
   Y_BLINKING_INVALID = -1;



//--- (end of YLed definitions)

type
//--- (YLed declaration)
 TYLed = class;
 TUpdateCallback  = procedure(func: TYLed; value:string);
////
/// <summary>
///   TYLed Class: Led function interface
/// <para>
///   Yoctopuce application programming interface
///   allows you not only to drive the intensity of the led, but also to
///   have it blink at various preset frequencies.
/// </para>
/// </summary>
///-
TYLed=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _power                    : Integer;
   _luminosity               : LongInt;
   _blinking                 : Integer;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YLed declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of leds started using <c>yFirstLed()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YLed</c> object, corresponding to
   ///   a led currently online, or a <c>null</c> pointer
   ///   if there are no more leds to enumerate.
   /// </returns>
   ///-
   function nextLed():TYLed;

   //--- (YLed accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the led.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the led
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the led.
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
   ///   a string corresponding to the logical name of the led
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
   ///   Returns the current value of the led (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the led (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the current led state.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_POWER_OFF</c> or <c>Y_POWER_ON</c>, according to the current led state
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_POWER_INVALID</c>.
   /// </para>
   ///-
   function get_power():Integer;

   ////
   /// <summary>
   ///   Changes the state of the led.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   either <c>Y_POWER_OFF</c> or <c>Y_POWER_ON</c>, according to the state of the led
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
   function set_power(newval:Integer):integer;

   ////
   /// <summary>
   ///   Returns the current led intensity (in per cent).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the current led intensity (in per cent)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LUMINOSITY_INVALID</c>.
   /// </para>
   ///-
   function get_luminosity():LongInt;

   ////
   /// <summary>
   ///   Changes the current led intensity (in per cent).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the current led intensity (in per cent)
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
   function set_luminosity(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns the current led signaling mode.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a value among <c>Y_BLINKING_STILL</c>, <c>Y_BLINKING_RELAX</c>, <c>Y_BLINKING_AWARE</c>,
   ///   <c>Y_BLINKING_RUN</c>, <c>Y_BLINKING_CALL</c> and <c>Y_BLINKING_PANIC</c> corresponding to the
   ///   current led signaling mode
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_BLINKING_INVALID</c>.
   /// </para>
   ///-
   function get_blinking():Integer;

   ////
   /// <summary>
   ///   Changes the current led signaling mode.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   a value among <c>Y_BLINKING_STILL</c>, <c>Y_BLINKING_RELAX</c>, <c>Y_BLINKING_AWARE</c>,
   ///   <c>Y_BLINKING_RUN</c>, <c>Y_BLINKING_CALL</c> and <c>Y_BLINKING_PANIC</c> corresponding to the
   ///   current led signaling mode
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
   function set_blinking(newval:Integer):integer;

   //--- (end of YLed accessors declaration)
end;

//--- (Led functions declaration)

////
/// <summary>
///   Retrieves a led for a given identifier.
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
///   This function does not require that the led is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YLed.isOnline()</c> to test if the led is
///   indeed online at a given time. In case of ambiguity when looking for
///   a led by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the led
/// </param>
/// <returns>
///   a <c>YLed</c> object allowing you to drive the led.
/// </returns>
///-
function yFindLed(func:string):TYLed;
////
/// <summary>
///   Starts the enumeration of leds currently accessible.
/// <para>
///   Use the method <c>YLed.nextLed()</c> to iterate on
///   next leds.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YLed</c> object, corresponding to
///   the first led currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstLed():TYLed;

//--- (end of Led functions declaration)

implementation

//--- (YLed implementation)

var
   _LedCache : TStringList;

constructor TYLed.Create(func:string);
 begin
   inherited Create('Led', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _power := Y_POWER_INVALID;
   _luminosity := Y_LUMINOSITY_INVALID;
   _blinking := Y_BLINKING_INVALID;
 end;

{$HINTS OFF}
function TYLed._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'power') then
       begin
         _power := member^.ivalue;
       end else
      if (member^.name = 'luminosity') then
       begin
         _luminosity := member^.ivalue;
       end else
      if (member^.name = 'blinking') then
       begin
         _blinking := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the led.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the led
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYLed.get_logicalName():string;
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
///   Changes the logical name of the led.
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
///   a string corresponding to the logical name of the led
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
function TYLed.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the led (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the led (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYLed.get_advertisedValue():string;
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
///   Returns the current led state.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_POWER_OFF or Y_POWER_ON, according to the current led state
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_POWER_INVALID.
/// </para>
///-
function TYLed.get_power():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_POWER_INVALID;
         exit;
       end;
   result := _power;
 end;

////
/// <summary>
///   Changes the state of the led.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   either Y_POWER_OFF or Y_POWER_ON, according to the state of the led
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
function TYLed.set_power(newval:Integer):integer;
 var
   rest_val: string;
 begin
   if(newval>0) then rest_val := '1' else rest_val := '0';
   result := _setAttr('power',rest_val);
 end;

////
/// <summary>
///   Returns the current led intensity (in per cent).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the current led intensity (in per cent)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LUMINOSITY_INVALID.
/// </para>
///-
function TYLed.get_luminosity():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LUMINOSITY_INVALID;
         exit;
       end;
   result := _luminosity;
 end;

////
/// <summary>
///   Changes the current led intensity (in per cent).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the current led intensity (in per cent)
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
function TYLed.set_luminosity(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('luminosity',rest_val);
 end;

////
/// <summary>
///   Returns the current led signaling mode.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a value among Y_BLINKING_STILL, Y_BLINKING_RELAX, Y_BLINKING_AWARE, Y_BLINKING_RUN, Y_BLINKING_CALL
///   and Y_BLINKING_PANIC corresponding to the current led signaling mode
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_BLINKING_INVALID.
/// </para>
///-
function TYLed.get_blinking():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_BLINKING_INVALID;
         exit;
       end;
   result := _blinking;
 end;

////
/// <summary>
///   Changes the current led signaling mode.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   a value among Y_BLINKING_STILL, Y_BLINKING_RELAX, Y_BLINKING_AWARE, Y_BLINKING_RUN, Y_BLINKING_CALL
///   and Y_BLINKING_PANIC corresponding to the current led signaling mode
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
function TYLed.set_blinking(newval:Integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('blinking',rest_val);
 end;

function TYLed.nextLed(): TYLed;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextLed := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextLed := nil;
      exit;
    end;
    nextLed := yFindLed(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYLed.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYLed.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYLed.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYLed.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YLed implementation)

//--- (Led functions)

function yFindLed(func:string): TYLed;
 var
   index: integer;
   res  : TYLed;
 begin
    if (_LedCache.Find(func, index)) then
     begin
       yFindLed := TYLed(_LedCache.objects[index]);
       exit;
     end;
   res := TYLed.Create(func);
   _LedCache.addObject(func, res);
   yFindLed := res;
 end;

function yFirstLed(): TYLed;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('Led', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstLed := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstLed := nil;
       exit;
    end;
   yFirstLed := yFindLed(serial+'.'+funcId);
 end;

procedure _LedCleanup();
  var i:integer;
begin
  for i:=0 to _LedCache.count-1 do 
    begin
     _LedCache.objects[i].free();
     _LedCache.objects[i]:=nil;
    end;
   _LedCache.free();
   _LedCache:=nil;
end;

//--- (end of Led functions)

initialization
   //--- (Led initialization)
   _LedCache        := TstringList.create();
   _LedCache.sorted := true;
   //--- (end of Led initialization)

finalization
   //--- (Led cleanup)
   _LedCleanup();
   //--- (end of Led cleanup)
end.
