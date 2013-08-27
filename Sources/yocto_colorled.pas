{*********************************************************************
 *
 * $Id: yocto_colorled.pas 12324 2013-08-13 15:10:31Z mvuilleu $
 *
 * Implements yFindColorLed(), the high-level API for ColorLed functions
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


unit yocto_colorled;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YColorLed definitions)
type TYColorLedMove = class(TObject)
public
   target      : longint;
   ms          : longint;
   moving      : longint;
   constructor Create();
end;

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_RGBCOLOR_INVALID              = YAPI_INVALID_LONGWORD;
   Y_HSLCOLOR_INVALID              = YAPI_INVALID_LONGWORD;
   Y_RGBCOLORATPOWERON_INVALID     = YAPI_INVALID_LONGWORD;

var Y_RGBMOVE_INVALID : TYColorLedMove;
var Y_HSLMOVE_INVALID : TYColorLedMove;

//--- (end of YColorLed definitions)

type
//--- (YColorLed declaration)
 TYColorLed = class;
 TUpdateCallback  = procedure(func: TYColorLed; value:string);
////
/// <summary>
///   TYColorLed Class: ColorLed function interface
/// <para>
///   Yoctopuce application programming interface
///   allows you to drive a color led using RGB coordinates as well as HSL coordinates.
///   The module performs all conversions form RGB to HSL automatically. It is then
///   self-evident to turn on a led with a given hue and to progressively vary its
///   saturation or lightness. If needed, you can find more information on the
///   difference between RGB and HSL in the section following this one.
/// </para>
/// </summary>
///-
TYColorLed=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _rgbColor                 : LongWord;
   _hslColor                 : LongWord;
   _rgbMove                  : TYColorLedMove;
   _hslMove                  : TYColorLedMove;
   _rgbColorAtPowerOn        : LongWord;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YColorLed declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of RGB leds started using <c>yFirstColorLed()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YColorLed</c> object, corresponding to
   ///   an RGB led currently online, or a <c>null</c> pointer
   ///   if there are no more RGB leds to enumerate.
   /// </returns>
   ///-
   function nextColorLed():TYColorLed;

   //--- (YColorLed accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the RGB led.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the RGB led
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the RGB led.
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
   ///   a string corresponding to the logical name of the RGB led
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
   ///   Returns the current value of the RGB led (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the RGB led (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the current RGB color of the led.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the current RGB color of the led
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_RGBCOLOR_INVALID</c>.
   /// </para>
   ///-
   function get_rgbColor():LongWord;

   ////
   /// <summary>
   ///   Changes the current color of the led, using a RGB color.
   /// <para>
   ///   Encoding is done as follows: 0xRRGGBB.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the current color of the led, using a RGB color
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
   function set_rgbColor(newval:LongWord):integer;

   ////
   /// <summary>
   ///   Returns the current HSL color of the led.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the current HSL color of the led
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_HSLCOLOR_INVALID</c>.
   /// </para>
   ///-
   function get_hslColor():LongWord;

   ////
   /// <summary>
   ///   Changes the current color of the led, using a color HSL.
   /// <para>
   ///   Encoding is done as follows: 0xHHSSLL.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the current color of the led, using a color HSL
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
   function set_hslColor(newval:LongWord):integer;

   function get_rgbMove():TYColorLedMove;

   function set_rgbMove(newval:TYColorLedMove):integer;

   ////
   /// <summary>
   ///   Performs a smooth transition in the RGB color space between the current color and a target color.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="rgb_target">
   ///   desired RGB color at the end of the transition
   /// </param>
   /// <param name="ms_duration">
   ///   duration of the transition, in millisecond
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
   function rgbMove(rgb_target:integer;ms_duration:integer):integer;

   function get_hslMove():TYColorLedMove;

   function set_hslMove(newval:TYColorLedMove):integer;

   ////
   /// <summary>
   ///   Performs a smooth transition in the HSL color space between the current color and a target color.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="hsl_target">
   ///   desired HSL color at the end of the transition
   /// </param>
   /// <param name="ms_duration">
   ///   duration of the transition, in millisecond
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
   function hslMove(hsl_target:integer;ms_duration:integer):integer;

   ////
   /// <summary>
   ///   Returns the configured color to be displayed when the module is turned on.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the configured color to be displayed when the module is turned on
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_RGBCOLORATPOWERON_INVALID</c>.
   /// </para>
   ///-
   function get_rgbColorAtPowerOn():LongWord;

   ////
   /// <summary>
   ///   Changes the color that the led will display by default when the module is turned on.
   /// <para>
   ///   This color will be displayed as soon as the module is powered on.
   ///   Remember to call the <c>saveToFlash()</c> method of the module if the
   ///   change should be kept.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the color that the led will display by default when the module is turned on
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
   function set_rgbColorAtPowerOn(newval:LongWord):integer;

   //--- (end of YColorLed accessors declaration)
end;

//--- (ColorLed functions declaration)

////
/// <summary>
///   Retrieves an RGB led for a given identifier.
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
///   This function does not require that the RGB led is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YColorLed.isOnline()</c> to test if the RGB led is
///   indeed online at a given time. In case of ambiguity when looking for
///   an RGB led by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the RGB led
/// </param>
/// <returns>
///   a <c>YColorLed</c> object allowing you to drive the RGB led.
/// </returns>
///-
function yFindColorLed(func:string):TYColorLed;
////
/// <summary>
///   Starts the enumeration of RGB leds currently accessible.
/// <para>
///   Use the method <c>YColorLed.nextColorLed()</c> to iterate on
///   next RGB leds.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YColorLed</c> object, corresponding to
///   the first RGB led currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstColorLed():TYColorLed;

//--- (end of ColorLed functions declaration)

implementation

//--- (YColorLed implementation)

var
   _ColorLedCache : TStringList;

constructor TYColorLedMove.Create();
 begin
   target := YAPI_INVALID_LONGINT;
   ms := YAPI_INVALID_LONGINT;
   moving := YAPI_INVALID_LONGINT;
 end;

constructor TYColorLed.Create(func:string);
 begin
   inherited Create('ColorLed', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _rgbColor := Y_RGBCOLOR_INVALID;
   _hslColor := Y_HSLCOLOR_INVALID;
   _rgbMove := TYColorLedMove.Create();
   _hslMove := TYColorLedMove.Create();
   _rgbColorAtPowerOn := Y_RGBCOLORATPOWERON_INVALID;
 end;

{$HINTS OFF}
function TYColorLed._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'rgbColor') then
       begin
         _rgbColor := member^.ivalue;
       end else
      if (member^.name = 'hslColor') then
       begin
         _hslColor := member^.ivalue;
       end else
      if (member^.name = 'rgbMove') then
       begin
         if (member^.recordtype <> JSON_STRUCT) then begin _parse:= -1; exit; end;
         for l:=0 to member^.membercount-1 do
          begin
            sub := member^.members[l];
            if (sub^.name = 'moving') then
               _rgbMove.moving := sub^.ivalue else
            if (sub^.name = 'target') then
               _rgbMove.target := sub^.ivalue else
            if (sub^.name = 'ms') then
               _rgbMove.ms := sub^.ivalue;
          end; 
         
       end else
      if (member^.name = 'hslMove') then
       begin
         if (member^.recordtype <> JSON_STRUCT) then begin _parse:= -1; exit; end;
         for l:=0 to member^.membercount-1 do
          begin
            sub := member^.members[l];
            if (sub^.name = 'moving') then
               _hslMove.moving := sub^.ivalue else
            if (sub^.name = 'target') then
               _hslMove.target := sub^.ivalue else
            if (sub^.name = 'ms') then
               _hslMove.ms := sub^.ivalue;
          end; 
         
       end else
      if (member^.name = 'rgbColorAtPowerOn') then
       begin
         _rgbColorAtPowerOn := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the RGB led.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the RGB led
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYColorLed.get_logicalName():string;
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
///   Changes the logical name of the RGB led.
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
///   a string corresponding to the logical name of the RGB led
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
function TYColorLed.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the RGB led (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the RGB led (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYColorLed.get_advertisedValue():string;
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
///   Returns the current RGB color of the led.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the current RGB color of the led
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_RGBCOLOR_INVALID.
/// </para>
///-
function TYColorLed.get_rgbColor():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_RGBCOLOR_INVALID;
         exit;
       end;
   result := _rgbColor;
 end;

////
/// <summary>
///   Changes the current color of the led, using a RGB color.
/// <para>
///   Encoding is done as follows: 0xRRGGBB.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the current color of the led, using a RGB color
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
function TYColorLed.set_rgbColor(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := '0x'+inttohex(newval,6);
   result := _setAttr('rgbColor',rest_val);
 end;

////
/// <summary>
///   Returns the current HSL color of the led.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the current HSL color of the led
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_HSLCOLOR_INVALID.
/// </para>
///-
function TYColorLed.get_hslColor():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_HSLCOLOR_INVALID;
         exit;
       end;
   result := _hslColor;
 end;

////
/// <summary>
///   Changes the current color of the led, using a color HSL.
/// <para>
///   Encoding is done as follows: 0xHHSSLL.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the current color of the led, using a color HSL
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
function TYColorLed.set_hslColor(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := '0x'+inttohex(newval,6);
   result := _setAttr('hslColor',rest_val);
 end;

function TYColorLed.get_rgbMove():TYColorLedMove;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_RGBMOVE_INVALID;
         exit;
       end;
   result := _rgbMove;
 end;

function TYColorLed.set_rgbMove(newval:TYColorLedMove):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval.target)+':'+inttostr(newval.ms);
   result := _setAttr('rgbMove',rest_val);
 end;

////
/// <summary>
///   Performs a smooth transition in the RGB color space between the current color and a target color.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="rgb_target">
///   desired RGB color at the end of the transition
/// </param>
/// <param name="ms_duration">
///   duration of the transition, in millisecond
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
function TYColorLed.rgbMove(rgb_target:integer;ms_duration:integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(rgb_target)+':'+inttostr(ms_duration);
   result := _setAttr('rgbMove', rest_val);
 end;

function TYColorLed.get_hslMove():TYColorLedMove;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_HSLMOVE_INVALID;
         exit;
       end;
   result := _hslMove;
 end;

function TYColorLed.set_hslMove(newval:TYColorLedMove):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval.target)+':'+inttostr(newval.ms);
   result := _setAttr('hslMove',rest_val);
 end;

////
/// <summary>
///   Performs a smooth transition in the HSL color space between the current color and a target color.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="hsl_target">
///   desired HSL color at the end of the transition
/// </param>
/// <param name="ms_duration">
///   duration of the transition, in millisecond
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
function TYColorLed.hslMove(hsl_target:integer;ms_duration:integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(hsl_target)+':'+inttostr(ms_duration);
   result := _setAttr('hslMove', rest_val);
 end;

////
/// <summary>
///   Returns the configured color to be displayed when the module is turned on.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the configured color to be displayed when the module is turned on
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_RGBCOLORATPOWERON_INVALID.
/// </para>
///-
function TYColorLed.get_rgbColorAtPowerOn():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_RGBCOLORATPOWERON_INVALID;
         exit;
       end;
   result := _rgbColorAtPowerOn;
 end;

////
/// <summary>
///   Changes the color that the led will display by default when the module is turned on.
/// <para>
///   This color will be displayed as soon as the module is powered on.
///   Remember to call the saveToFlash() method of the module if the
///   change should be kept.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the color that the led will display by default when the module is turned on
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
function TYColorLed.set_rgbColorAtPowerOn(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := '0x'+inttohex(newval,6);
   result := _setAttr('rgbColorAtPowerOn',rest_val);
 end;

function TYColorLed.nextColorLed(): TYColorLed;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextColorLed := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextColorLed := nil;
      exit;
    end;
    nextColorLed := yFindColorLed(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYColorLed.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYColorLed.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYColorLed.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYColorLed.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YColorLed implementation)

//--- (ColorLed functions)

function yFindColorLed(func:string): TYColorLed;
 var
   index: integer;
   res  : TYColorLed;
 begin
    if (_ColorLedCache.Find(func, index)) then
     begin
       yFindColorLed := TYColorLed(_ColorLedCache.objects[index]);
       exit;
     end;
   res := TYColorLed.Create(func);
   _ColorLedCache.addObject(func, res);
   yFindColorLed := res;
 end;

function yFirstColorLed(): TYColorLed;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('ColorLed', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstColorLed := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstColorLed := nil;
       exit;
    end;
   yFirstColorLed := yFindColorLed(serial+'.'+funcId);
 end;

procedure _ColorLedCleanup();
  var i:integer;
begin
  for i:=0 to _ColorLedCache.count-1 do 
    begin
     _ColorLedCache.objects[i].free();
     _ColorLedCache.objects[i]:=nil;
    end;
   _ColorLedCache.free();
   _ColorLedCache:=nil;
end;

//--- (end of ColorLed functions)

initialization
   //--- (ColorLed initialization)
   _ColorLedCache        := TstringList.create();
   _ColorLedCache.sorted := true;
   Y_RGBMOVE_INVALID := TYColorLedMove.Create();
   Y_HSLMOVE_INVALID := TYColorLedMove.Create();
   //--- (end of ColorLed initialization)

finalization
   //--- (ColorLed cleanup)
   _ColorLedCleanup();
   //--- (end of ColorLed cleanup)
end.
