{*********************************************************************
 *
 * $Id: yocto_oscontrol.pas 12337 2013-08-14 15:22:22Z mvuilleu $
 *
 * Implements yFindOsControl(), the high-level API for OsControl functions
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


unit yocto_oscontrol;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YOsControl definitions)

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_SHUTDOWNCOUNTDOWN_INVALID     = YAPI_INVALID_LONGWORD;


//--- (end of YOsControl definitions)

type
//--- (YOsControl declaration)
 TYOsControl = class;
 TUpdateCallback  = procedure(func: TYOsControl; value:string);
////
/// <summary>
///   TYOsControl Class: OS control
/// <para>
///   The OScontrol object allows some control over the operating system running a VirtualHub.
///   OsControl is available on the VirtualHub software only. This feature must be activated at the VirtualHub
///   start up with -o option.
/// </para>
/// </summary>
///-
TYOsControl=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _shutdownCountdown        : LongWord;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YOsControl declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of OS control started using <c>yFirstOsControl()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YOsControl</c> object, corresponding to
   ///   OS control currently online, or a <c>null</c> pointer
   ///   if there are no more OS control to enumerate.
   /// </returns>
   ///-
   function nextOsControl():TYOsControl;

   //--- (YOsControl accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the OS control, corresponding to the network name of the module.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the OS control, corresponding to the network name of the module
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the OS control.
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
   ///   a string corresponding to the logical name of the OS control
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
   ///   Returns the current value of the OS control (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the OS control (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the remaining number of seconds before the OS shutdown, or zero when no
   ///   shutdown has been scheduled.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the remaining number of seconds before the OS shutdown, or zero when no
   ///   shutdown has been scheduled
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_SHUTDOWNCOUNTDOWN_INVALID</c>.
   /// </para>
   ///-
   function get_shutdownCountdown():LongWord;

   function set_shutdownCountdown(newval:LongWord):integer;

   ////
   /// <summary>
   ///   Schedules an OS shutdown after a given number of seconds.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="secBeforeShutDown">
   ///   number of seconds before shutdown
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
   function shutdown(secBeforeShutDown:integer):integer;

   //--- (end of YOsControl accessors declaration)
end;

//--- (OsControl functions declaration)

////
/// <summary>
///   Retrieves OS control for a given identifier.
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
///   This function does not require that the OS control is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YOsControl.isOnline()</c> to test if the OS control is
///   indeed online at a given time. In case of ambiguity when looking for
///   OS control by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the OS control
/// </param>
/// <returns>
///   a <c>YOsControl</c> object allowing you to drive the OS control.
/// </returns>
///-
function yFindOsControl(func:string):TYOsControl;
////
/// <summary>
///   Starts the enumeration of OS control currently accessible.
/// <para>
///   Use the method <c>YOsControl.nextOsControl()</c> to iterate on
///   next OS control.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YOsControl</c> object, corresponding to
///   the first OS control currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstOsControl():TYOsControl;

//--- (end of OsControl functions declaration)

implementation

//--- (YOsControl implementation)

var
   _OsControlCache : TStringList;

constructor TYOsControl.Create(func:string);
 begin
   inherited Create('OsControl', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _shutdownCountdown := Y_SHUTDOWNCOUNTDOWN_INVALID;
 end;

{$HINTS OFF}
function TYOsControl._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'shutdownCountdown') then
       begin
         _shutdownCountdown := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the OS control, corresponding to the network name of the module.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the OS control, corresponding to the network name of the module
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYOsControl.get_logicalName():string;
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
///   Changes the logical name of the OS control.
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
///   a string corresponding to the logical name of the OS control
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
function TYOsControl.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the OS control (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the OS control (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYOsControl.get_advertisedValue():string;
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
///   Returns the remaining number of seconds before the OS shutdown, or zero when no
///   shutdown has been scheduled.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the remaining number of seconds before the OS shutdown, or zero when no
///   shutdown has been scheduled
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_SHUTDOWNCOUNTDOWN_INVALID.
/// </para>
///-
function TYOsControl.get_shutdownCountdown():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_SHUTDOWNCOUNTDOWN_INVALID;
         exit;
       end;
   result := _shutdownCountdown;
 end;

function TYOsControl.set_shutdownCountdown(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('shutdownCountdown',rest_val);
 end;

////
/// <summary>
///   Schedules an OS shutdown after a given number of seconds.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="secBeforeShutDown">
///   number of seconds before shutdown
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
function TYOsControl.shutdown(secBeforeShutDown:integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(secBeforeShutDown);
   result := _setAttr('shutdownCountdown', rest_val);
 end;

function TYOsControl.nextOsControl(): TYOsControl;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextOsControl := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextOsControl := nil;
      exit;
    end;
    nextOsControl := yFindOsControl(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYOsControl.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYOsControl.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYOsControl.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYOsControl.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YOsControl implementation)

//--- (OsControl functions)

function yFindOsControl(func:string): TYOsControl;
 var
   index: integer;
   res  : TYOsControl;
 begin
    if (_OsControlCache.Find(func, index)) then
     begin
       yFindOsControl := TYOsControl(_OsControlCache.objects[index]);
       exit;
     end;
   res := TYOsControl.Create(func);
   _OsControlCache.addObject(func, res);
   yFindOsControl := res;
 end;

function yFirstOsControl(): TYOsControl;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('OsControl', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstOsControl := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstOsControl := nil;
       exit;
    end;
   yFirstOsControl := yFindOsControl(serial+'.'+funcId);
 end;

procedure _OsControlCleanup();
  var i:integer;
begin
  for i:=0 to _OsControlCache.count-1 do 
    begin
     _OsControlCache.objects[i].free();
     _OsControlCache.objects[i]:=nil;
    end;
   _OsControlCache.free();
   _OsControlCache:=nil;
end;

//--- (end of OsControl functions)

initialization
   //--- (OsControl initialization)
   _OsControlCache        := TstringList.create();
   _OsControlCache.sorted := true;
   //--- (end of OsControl initialization)

finalization
   //--- (OsControl cleanup)
   _OsControlCleanup();
   //--- (end of OsControl cleanup)
end.
