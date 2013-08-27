{*********************************************************************
 *
 * $Id: yocto_hubport.pas 12337 2013-08-14 15:22:22Z mvuilleu $
 *
 * Implements yFindHubPort(), the high-level API for HubPort functions
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


unit yocto_hubport;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YHubPort definitions)

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_ENABLED_FALSE = 0;
   Y_ENABLED_TRUE = 1;
   Y_ENABLED_INVALID = -1;

   Y_PORTSTATE_OFF = 0;
   Y_PORTSTATE_OVRLD = 1;
   Y_PORTSTATE_ON = 2;
   Y_PORTSTATE_RUN = 3;
   Y_PORTSTATE_PROG = 4;
   Y_PORTSTATE_INVALID = -1;

   Y_BAUDRATE_INVALID              = -1;


//--- (end of YHubPort definitions)

type
//--- (YHubPort declaration)
 TYHubPort = class;
 TUpdateCallback  = procedure(func: TYHubPort; value:string);
////
/// <summary>
///   TYHubPort Class: Yocto-hub port interface
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
///-
TYHubPort=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _enabled                  : Integer;
   _portState                : Integer;
   _baudRate                 : LongInt;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YHubPort declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of Yocto-hub ports started using <c>yFirstHubPort()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YHubPort</c> object, corresponding to
   ///   a Yocto-hub port currently online, or a <c>null</c> pointer
   ///   if there are no more Yocto-hub ports to enumerate.
   /// </returns>
   ///-
   function nextHubPort():TYHubPort;

   //--- (YHubPort accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the Yocto-hub port, which is always the serial number of the
   ///   connected module.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the Yocto-hub port, which is always the serial number of the
   ///   connected module
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   It is not possible to configure the logical name of a Yocto-hub port.
   /// <para>
   ///   The logical
   ///   name is automatically set to the serial number of the connected module.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   a string
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
   ///   Returns the current value of the Yocto-hub port (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the Yocto-hub port (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns true if the Yocto-hub port is powered, false otherwise.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_ENABLED_FALSE</c> or <c>Y_ENABLED_TRUE</c>, according to true if the Yocto-hub port is
   ///   powered, false otherwise
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ENABLED_INVALID</c>.
   /// </para>
   ///-
   function get_enabled():Integer;

   ////
   /// <summary>
   ///   Changes the activation of the Yocto-hub port.
   /// <para>
   ///   If the port is enabled, the
   ///   *      connected module is powered. Otherwise, port power is shut down.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   either <c>Y_ENABLED_FALSE</c> or <c>Y_ENABLED_TRUE</c>, according to the activation of the Yocto-hub port
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
   function set_enabled(newval:Integer):integer;

   ////
   /// <summary>
   ///   Returns the current state of the Yocto-hub port.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a value among <c>Y_PORTSTATE_OFF</c>, <c>Y_PORTSTATE_OVRLD</c>, <c>Y_PORTSTATE_ON</c>,
   ///   <c>Y_PORTSTATE_RUN</c> and <c>Y_PORTSTATE_PROG</c> corresponding to the current state of the Yocto-hub port
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_PORTSTATE_INVALID</c>.
   /// </para>
   ///-
   function get_portState():Integer;

   ////
   /// <summary>
   ///   Returns the current baud rate used by this Yocto-hub port, in kbps.
   /// <para>
   ///   The default value is 1000 kbps, but a slower rate may be used if communication
   ///   problems are encountered.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the current baud rate used by this Yocto-hub port, in kbps
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_BAUDRATE_INVALID</c>.
   /// </para>
   ///-
   function get_baudRate():LongInt;

   //--- (end of YHubPort accessors declaration)
end;

//--- (HubPort functions declaration)

////
/// <summary>
///   Retrieves a Yocto-hub port for a given identifier.
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
///   This function does not require that the Yocto-hub port is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YHubPort.isOnline()</c> to test if the Yocto-hub port is
///   indeed online at a given time. In case of ambiguity when looking for
///   a Yocto-hub port by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the Yocto-hub port
/// </param>
/// <returns>
///   a <c>YHubPort</c> object allowing you to drive the Yocto-hub port.
/// </returns>
///-
function yFindHubPort(func:string):TYHubPort;
////
/// <summary>
///   Starts the enumeration of Yocto-hub ports currently accessible.
/// <para>
///   Use the method <c>YHubPort.nextHubPort()</c> to iterate on
///   next Yocto-hub ports.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YHubPort</c> object, corresponding to
///   the first Yocto-hub port currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstHubPort():TYHubPort;

//--- (end of HubPort functions declaration)

implementation

//--- (YHubPort implementation)

var
   _HubPortCache : TStringList;

constructor TYHubPort.Create(func:string);
 begin
   inherited Create('HubPort', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _enabled := Y_ENABLED_INVALID;
   _portState := Y_PORTSTATE_INVALID;
   _baudRate := Y_BAUDRATE_INVALID;
 end;

{$HINTS OFF}
function TYHubPort._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'enabled') then
       begin
         _enabled := member^.ivalue;
       end else
      if (member^.name = 'portState') then
       begin
         _portState := member^.ivalue;
       end else
      if (member^.name = 'baudRate') then
       begin
         _baudRate := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the Yocto-hub port, which is always the serial number of the
///   connected module.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the Yocto-hub port, which is always the serial number of the
///   connected module
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYHubPort.get_logicalName():string;
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
///   It is not possible to configure the logical name of a Yocto-hub port.
/// <para>
///   The logical
///   name is automatically set to the serial number of the connected module.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   a string
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
function TYHubPort.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the Yocto-hub port (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the Yocto-hub port (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYHubPort.get_advertisedValue():string;
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
///   Returns true if the Yocto-hub port is powered, false otherwise.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_ENABLED_FALSE or Y_ENABLED_TRUE, according to true if the Yocto-hub port is powered, false otherwise
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ENABLED_INVALID.
/// </para>
///-
function TYHubPort.get_enabled():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_ENABLED_INVALID;
         exit;
       end;
   result := _enabled;
 end;

////
/// <summary>
///   Changes the activation of the Yocto-hub port.
/// <para>
///   If the port is enabled, the
///   *      connected module is powered. Otherwise, port power is shut down.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   either Y_ENABLED_FALSE or Y_ENABLED_TRUE, according to the activation of the Yocto-hub port
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
function TYHubPort.set_enabled(newval:Integer):integer;
 var
   rest_val: string;
 begin
   if(newval>0) then rest_val := '1' else rest_val := '0';
   result := _setAttr('enabled',rest_val);
 end;

////
/// <summary>
///   Returns the current state of the Yocto-hub port.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a value among Y_PORTSTATE_OFF, Y_PORTSTATE_OVRLD, Y_PORTSTATE_ON, Y_PORTSTATE_RUN and
///   Y_PORTSTATE_PROG corresponding to the current state of the Yocto-hub port
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_PORTSTATE_INVALID.
/// </para>
///-
function TYHubPort.get_portState():Integer;
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
///   Returns the current baud rate used by this Yocto-hub port, in kbps.
/// <para>
///   The default value is 1000 kbps, but a slower rate may be used if communication
///   problems are encountered.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the current baud rate used by this Yocto-hub port, in kbps
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_BAUDRATE_INVALID.
/// </para>
///-
function TYHubPort.get_baudRate():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_BAUDRATE_INVALID;
         exit;
       end;
   result := _baudRate;
 end;

function TYHubPort.nextHubPort(): TYHubPort;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextHubPort := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextHubPort := nil;
      exit;
    end;
    nextHubPort := yFindHubPort(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYHubPort.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYHubPort.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYHubPort.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYHubPort.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YHubPort implementation)

//--- (HubPort functions)

function yFindHubPort(func:string): TYHubPort;
 var
   index: integer;
   res  : TYHubPort;
 begin
    if (_HubPortCache.Find(func, index)) then
     begin
       yFindHubPort := TYHubPort(_HubPortCache.objects[index]);
       exit;
     end;
   res := TYHubPort.Create(func);
   _HubPortCache.addObject(func, res);
   yFindHubPort := res;
 end;

function yFirstHubPort(): TYHubPort;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('HubPort', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstHubPort := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstHubPort := nil;
       exit;
    end;
   yFirstHubPort := yFindHubPort(serial+'.'+funcId);
 end;

procedure _HubPortCleanup();
  var i:integer;
begin
  for i:=0 to _HubPortCache.count-1 do 
    begin
     _HubPortCache.objects[i].free();
     _HubPortCache.objects[i]:=nil;
    end;
   _HubPortCache.free();
   _HubPortCache:=nil;
end;

//--- (end of HubPort functions)

initialization
   //--- (HubPort initialization)
   _HubPortCache        := TstringList.create();
   _HubPortCache.sorted := true;
   //--- (end of HubPort initialization)

finalization
   //--- (HubPort cleanup)
   _HubPortCleanup();
   //--- (end of HubPort cleanup)
end.
