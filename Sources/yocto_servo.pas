{*********************************************************************
 *
 * $Id: yocto_servo.pas 12324 2013-08-13 15:10:31Z mvuilleu $
 *
 * Implements yFindServo(), the high-level API for Servo functions
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


unit yocto_servo;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YServo definitions)
type TYServoMove = class(TObject)
public
   target      : longint;
   ms          : longint;
   moving      : longint;
   constructor Create();
end;

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_POSITION_INVALID              = YAPI_INVALID_LONGINT;
   Y_RANGE_INVALID                 = -1;
   Y_NEUTRAL_INVALID               = -1;

var Y_MOVE_INVALID : TYServoMove;

//--- (end of YServo definitions)

type
//--- (YServo declaration)
 TYServo = class;
 TUpdateCallback  = procedure(func: TYServo; value:string);
////
/// <summary>
///   TYServo Class: Servo function interface
/// <para>
///   Yoctopuce application programming interface allows you not only to move
///   a servo to a given position, but also to specify the time interval
///   in which the move should be performed. This makes it possible to
///   synchronize two servos involved in a same move.
/// </para>
/// </summary>
///-
TYServo=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _position                 : LongInt;
   _range                    : LongInt;
   _neutral                  : LongInt;
   _move                     : TYServoMove;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YServo declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of servos started using <c>yFirstServo()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YServo</c> object, corresponding to
   ///   a servo currently online, or a <c>null</c> pointer
   ///   if there are no more servos to enumerate.
   /// </returns>
   ///-
   function nextServo():TYServo;

   //--- (YServo accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the servo.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the servo
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the servo.
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
   ///   a string corresponding to the logical name of the servo
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
   ///   Returns the current value of the servo (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the servo (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the current servo position.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the current servo position
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_POSITION_INVALID</c>.
   /// </para>
   ///-
   function get_position():LongInt;

   ////
   /// <summary>
   ///   Changes immediately the servo driving position.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to immediately the servo driving position
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
   function set_position(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns the current range of use of the servo.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the current range of use of the servo
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_RANGE_INVALID</c>.
   /// </para>
   ///-
   function get_range():LongInt;

   ////
   /// <summary>
   ///   Changes the range of use of the servo, specified in per cents.
   /// <para>
   ///   A range of 100% corresponds to a standard control signal, that varies
   ///   from 1 [ms] to 2 [ms], When using a servo that supports a double range,
   ///   from 0.5 [ms] to 2.5 [ms], you can select a range of 200%.
   ///   Be aware that using a range higher than what is supported by the servo
   ///   is likely to damage the servo.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the range of use of the servo, specified in per cents
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
   function set_range(newval:LongInt):integer;

   ////
   /// <summary>
   ///   Returns the duration in microseconds of a neutral pulse for the servo.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the duration in microseconds of a neutral pulse for the servo
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_NEUTRAL_INVALID</c>.
   /// </para>
   ///-
   function get_neutral():LongInt;

   ////
   /// <summary>
   ///   Changes the duration of the pulse corresponding to the neutral position of the servo.
   /// <para>
   ///   The duration is specified in microseconds, and the standard value is 1500 [us].
   ///   This setting makes it possible to shift the range of use of the servo.
   ///   Be aware that using a range higher than what is supported by the servo is
   ///   likely to damage the servo.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   an integer corresponding to the duration of the pulse corresponding to the neutral position of the servo
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
   function set_neutral(newval:LongInt):integer;

   function get_move():TYServoMove;

   function set_move(newval:TYServoMove):integer;

   ////
   /// <summary>
   ///   Performs a smooth move at constant speed toward a given position.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="target">
   ///   new position at the end of the move
   /// </param>
   /// <param name="ms_duration">
   ///   total duration of the move, in milliseconds
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
   function move(target:integer;ms_duration:integer):integer;

   //--- (end of YServo accessors declaration)
end;

//--- (Servo functions declaration)

////
/// <summary>
///   Retrieves a servo for a given identifier.
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
///   This function does not require that the servo is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YServo.isOnline()</c> to test if the servo is
///   indeed online at a given time. In case of ambiguity when looking for
///   a servo by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the servo
/// </param>
/// <returns>
///   a <c>YServo</c> object allowing you to drive the servo.
/// </returns>
///-
function yFindServo(func:string):TYServo;
////
/// <summary>
///   Starts the enumeration of servos currently accessible.
/// <para>
///   Use the method <c>YServo.nextServo()</c> to iterate on
///   next servos.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YServo</c> object, corresponding to
///   the first servo currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstServo():TYServo;

//--- (end of Servo functions declaration)

implementation

//--- (YServo implementation)

var
   _ServoCache : TStringList;

constructor TYServoMove.Create();
 begin
   target := YAPI_INVALID_LONGINT;
   ms := YAPI_INVALID_LONGINT;
   moving := YAPI_INVALID_LONGINT;
 end;

constructor TYServo.Create(func:string);
 begin
   inherited Create('Servo', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _position := Y_POSITION_INVALID;
   _range := Y_RANGE_INVALID;
   _neutral := Y_NEUTRAL_INVALID;
   _move := TYServoMove.Create();
 end;

{$HINTS OFF}
function TYServo._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'position') then
       begin
         _position := member^.ivalue;
       end else
      if (member^.name = 'range') then
       begin
         _range := member^.ivalue;
       end else
      if (member^.name = 'neutral') then
       begin
         _neutral := member^.ivalue;
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
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the servo.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the servo
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYServo.get_logicalName():string;
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
///   Changes the logical name of the servo.
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
///   a string corresponding to the logical name of the servo
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
function TYServo.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the servo (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the servo (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYServo.get_advertisedValue():string;
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
///   Returns the current servo position.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the current servo position
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_POSITION_INVALID.
/// </para>
///-
function TYServo.get_position():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_POSITION_INVALID;
         exit;
       end;
   result := _position;
 end;

////
/// <summary>
///   Changes immediately the servo driving position.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to immediately the servo driving position
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
function TYServo.set_position(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('position',rest_val);
 end;

////
/// <summary>
///   Returns the current range of use of the servo.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the current range of use of the servo
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_RANGE_INVALID.
/// </para>
///-
function TYServo.get_range():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_RANGE_INVALID;
         exit;
       end;
   result := _range;
 end;

////
/// <summary>
///   Changes the range of use of the servo, specified in per cents.
/// <para>
///   A range of 100% corresponds to a standard control signal, that varies
///   from 1 [ms] to 2 [ms], When using a servo that supports a double range,
///   from 0.5 [ms] to 2.5 [ms], you can select a range of 200%.
///   Be aware that using a range higher than what is supported by the servo
///   is likely to damage the servo.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the range of use of the servo, specified in per cents
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
function TYServo.set_range(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('range',rest_val);
 end;

////
/// <summary>
///   Returns the duration in microseconds of a neutral pulse for the servo.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the duration in microseconds of a neutral pulse for the servo
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_NEUTRAL_INVALID.
/// </para>
///-
function TYServo.get_neutral():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_NEUTRAL_INVALID;
         exit;
       end;
   result := _neutral;
 end;

////
/// <summary>
///   Changes the duration of the pulse corresponding to the neutral position of the servo.
/// <para>
///   The duration is specified in microseconds, and the standard value is 1500 [us].
///   This setting makes it possible to shift the range of use of the servo.
///   Be aware that using a range higher than what is supported by the servo is
///   likely to damage the servo.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   an integer corresponding to the duration of the pulse corresponding to the neutral position of the servo
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
function TYServo.set_neutral(newval:LongInt):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('neutral',rest_val);
 end;

function TYServo.get_move():TYServoMove;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_MOVE_INVALID;
         exit;
       end;
   result := _move;
 end;

function TYServo.set_move(newval:TYServoMove):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval.target)+':'+inttostr(newval.ms);
   result := _setAttr('move',rest_val);
 end;

////
/// <summary>
///   Performs a smooth move at constant speed toward a given position.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="target">
///   new position at the end of the move
/// </param>
/// <param name="ms_duration">
///   total duration of the move, in milliseconds
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
function TYServo.move(target:integer;ms_duration:integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(target)+':'+inttostr(ms_duration);
   result := _setAttr('move', rest_val);
 end;

function TYServo.nextServo(): TYServo;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextServo := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextServo := nil;
      exit;
    end;
    nextServo := yFindServo(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYServo.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYServo.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYServo.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYServo.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YServo implementation)

//--- (Servo functions)

function yFindServo(func:string): TYServo;
 var
   index: integer;
   res  : TYServo;
 begin
    if (_ServoCache.Find(func, index)) then
     begin
       yFindServo := TYServo(_ServoCache.objects[index]);
       exit;
     end;
   res := TYServo.Create(func);
   _ServoCache.addObject(func, res);
   yFindServo := res;
 end;

function yFirstServo(): TYServo;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('Servo', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstServo := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstServo := nil;
       exit;
    end;
   yFirstServo := yFindServo(serial+'.'+funcId);
 end;

procedure _ServoCleanup();
  var i:integer;
begin
  for i:=0 to _ServoCache.count-1 do 
    begin
     _ServoCache.objects[i].free();
     _ServoCache.objects[i]:=nil;
    end;
   _ServoCache.free();
   _ServoCache:=nil;
end;

//--- (end of Servo functions)

initialization
   //--- (Servo initialization)
   _ServoCache        := TstringList.create();
   _ServoCache.sorted := true;
   Y_MOVE_INVALID := TYServoMove.Create();
   //--- (end of Servo initialization)

finalization
   //--- (Servo cleanup)
   _ServoCleanup();
   //--- (end of Servo cleanup)
end.
