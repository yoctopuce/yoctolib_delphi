{*********************************************************************
 *
 *  $Id: yocto_servo.pas 32903 2018-11-02 10:14:32Z seb $
 *
 *  Implements yFindServo(), the high-level API for Servo functions
 *
 *  - - - - - - - - - License information: - - - - - - - - -
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
type  TYServoMove = class(TObject)
  public
      target      : LongInt;
      ms          : LongInt;
      moving      : LongInt;
      constructor Create();
    end;

const Y_POSITION_INVALID              = YAPI_INVALID_INT;
const Y_ENABLED_FALSE = 0;
const Y_ENABLED_TRUE = 1;
const Y_ENABLED_INVALID = -1;
const Y_RANGE_INVALID                 = YAPI_INVALID_UINT;
const Y_NEUTRAL_INVALID               = YAPI_INVALID_UINT;
const Y_POSITIONATPOWERON_INVALID     = YAPI_INVALID_INT;
const Y_ENABLEDATPOWERON_FALSE = 0;
const Y_ENABLEDATPOWERON_TRUE = 1;
const Y_ENABLEDATPOWERON_INVALID = -1;

var Y_MOVE_INVALID : TYServoMove;

//--- (end of YServo definitions)
//--- (YServo yapiwrapper declaration)
//--- (end of YServo yapiwrapper declaration)

type
  TYServo = class;
  //--- (YServo class start)
  TYServoValueCallback = procedure(func: TYServo; value:string);
  TYServoTimedReportCallback = procedure(func: TYServo; value:TYMeasure);

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
  //--- (end of YServo class start)
  protected
  //--- (YServo declaration)
    // Attributes (function value cache)
    _position                 : LongInt;
    _enabled                  : Integer;
    _range                    : LongInt;
    _neutral                  : LongInt;
    _move                     : TYServoMove;
    _positionAtPowerOn        : LongInt;
    _enabledAtPowerOn         : Integer;
    _valueCallbackServo       : TYServoValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YServo declaration)

  public
    //--- (YServo accessors declaration)
    constructor Create(func:string);

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
    ///   Returns the state of the servos.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_ENABLED_FALSE</c> or <c>Y_ENABLED_TRUE</c>, according to the state of the servos
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ENABLED_INVALID</c>.
    /// </para>
    ///-
    function get_enabled():Integer;

    ////
    /// <summary>
    ///   Stops or starts the servo.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>Y_ENABLED_FALSE</c> or <c>Y_ENABLED_TRUE</c>
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
    ///   is likely to damage the servo. Remember to call the matching module
    ///   <c>saveToFlash()</c> method, otherwise this call will have no effect.
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
    ///   likely to damage the servo. Remember to call the matching module
    ///   <c>saveToFlash()</c> method, otherwise this call will have no effect.
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
    function move(target: LongInt; ms_duration: LongInt):integer;

    ////
    /// <summary>
    ///   Returns the servo position at device power up.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the servo position at device power up
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_POSITIONATPOWERON_INVALID</c>.
    /// </para>
    ///-
    function get_positionAtPowerOn():LongInt;

    ////
    /// <summary>
    ///   Configure the servo position at device power up.
    /// <para>
    ///   Remember to call the matching
    ///   module <c>saveToFlash()</c> method, otherwise this call will have no effect.
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
    function set_positionAtPowerOn(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the servo signal generator state at power up.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_ENABLEDATPOWERON_FALSE</c> or <c>Y_ENABLEDATPOWERON_TRUE</c>, according to the servo
    ///   signal generator state at power up
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ENABLEDATPOWERON_INVALID</c>.
    /// </para>
    ///-
    function get_enabledAtPowerOn():Integer;

    ////
    /// <summary>
    ///   Configure the servo signal generator state at power up.
    /// <para>
    ///   Remember to call the matching module <c>saveToFlash()</c>
    ///   method, otherwise this call will have no effect.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>Y_ENABLEDATPOWERON_FALSE</c> or <c>Y_ENABLEDATPOWERON_TRUE</c>
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
    function set_enabledAtPowerOn(newval:Integer):integer;

    ////
    /// <summary>
    ///   Retrieves $AFUNCTION$ for a given identifier.
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
    ///   This function does not require that $THEFUNCTION$ is online at the time
    ///   it is invoked. The returned object is nevertheless valid.
    ///   Use the method <c>YServo.isOnline()</c> to test if $THEFUNCTION$ is
    ///   indeed online at a given time. In case of ambiguity when looking for
    ///   $AFUNCTION$ by logical name, no error is notified: the first instance
    ///   found is returned. The search is performed first by hardware name,
    ///   then by logical name.
    /// </para>
    /// <para>
    ///   If a call to this object's is_online() method returns FALSE although
    ///   you are certain that the matching device is plugged, make sure that you did
    ///   call registerHub() at application initialization time.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$
    /// </param>
    /// <returns>
    ///   a <c>YServo</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindServo(func: string):TYServo;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every change of advertised value.
    /// <para>
    ///   The callback is invoked only during the execution of <c>ySleep</c> or <c>yHandleEvents</c>.
    ///   This provides control over the time when the callback is triggered. For good responsiveness, remember to call
    ///   one of these two functions periodically. To unregister a callback, pass a NIL pointer as argument.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="callback">
    ///   the callback function to call, or a NIL pointer. The callback function should take two
    ///   arguments: the function object of which the value has changed, and the character string describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerValueCallback(callback: TYServoValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of servos started using <c>yFirstServo()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned servos order.
    ///   If you want to find a specific a servo, use <c>Servo.findServo()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YServo</c> object, corresponding to
    ///   a servo currently online, or a <c>NIL</c> pointer
    ///   if there are no more servos to enumerate.
    /// </returns>
    ///-
    function nextServo():TYServo;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstServo():TYServo;
  //--- (end of YServo accessors declaration)
  end;

//--- (YServo functions declaration)
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
  /// <para>
  ///   If a call to this object's is_online() method returns FALSE although
  ///   you are certain that the matching device is plugged, make sure that you did
  ///   call registerHub() at application initialization time.
  /// </para>
  /// <para>
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
  ///   the first servo currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstServo():TYServo;

//--- (end of YServo functions declaration)

implementation
//--- (YServo dlldef)
//--- (end of YServo dlldef)

    constructor TYServoMove.Create();
    begin
      target := YAPI_INVALID_INT;
      ms := YAPI_INVALID_INT;
      moving := YAPI_INVALID_UINT;
  end;

  constructor TYServo.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Servo';
      //--- (YServo accessors initialization)
      _position := Y_POSITION_INVALID;
      _enabled := Y_ENABLED_INVALID;
      _range := Y_RANGE_INVALID;
      _neutral := Y_NEUTRAL_INVALID;
      _move := Y_MOVE_INVALID;
      _positionAtPowerOn := Y_POSITIONATPOWERON_INVALID;
      _enabledAtPowerOn := Y_ENABLEDATPOWERON_INVALID;
      _valueCallbackServo := nil;
      //--- (end of YServo accessors initialization)
    end;

//--- (YServo yapiwrapper)
//--- (end of YServo yapiwrapper)

//--- (YServo implementation)
{$HINTS OFF}
  function TYServo._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'position') then
        begin
          _position := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'enabled') then
        begin
          _enabled := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'range') then
        begin
          _range := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'neutral') then
        begin
          _neutral := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'move') then
        begin
          if member^.recordtype = JSON_STRUCT then
            begin
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
            end;
         result := 1;
         exit;
         end;
      if (member^.name = 'positionAtPowerOn') then
        begin
          _positionAtPowerOn := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'enabledAtPowerOn') then
        begin
          _enabledAtPowerOn := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYServo.get_position():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_POSITION_INVALID;
              exit;
            end;
        end;
      res := self._position;
      result := res;
      exit;
    end;


  function TYServo.set_position(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('position',rest_val);
    end;

  function TYServo.get_enabled():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ENABLED_INVALID;
              exit;
            end;
        end;
      res := self._enabled;
      result := res;
      exit;
    end;


  function TYServo.set_enabled(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('enabled',rest_val);
    end;

  function TYServo.get_range():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RANGE_INVALID;
              exit;
            end;
        end;
      res := self._range;
      result := res;
      exit;
    end;


  function TYServo.set_range(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('range',rest_val);
    end;

  function TYServo.get_neutral():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NEUTRAL_INVALID;
              exit;
            end;
        end;
      res := self._neutral;
      result := res;
      exit;
    end;


  function TYServo.set_neutral(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('neutral',rest_val);
    end;

  function TYServo.get_move():TYServoMove;
    var
      res : TYServoMove;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MOVE_INVALID;
              exit;
            end;
        end;
      res := self._move;
      result := res;
      exit;
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
  function TYServo.move(target: LongInt; ms_duration: LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(target)+':'+inttostr(ms_duration);
      result := _setAttr('move', rest_val);
    end;

  function TYServo.get_positionAtPowerOn():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_POSITIONATPOWERON_INVALID;
              exit;
            end;
        end;
      res := self._positionAtPowerOn;
      result := res;
      exit;
    end;


  function TYServo.set_positionAtPowerOn(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('positionAtPowerOn',rest_val);
    end;

  function TYServo.get_enabledAtPowerOn():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ENABLEDATPOWERON_INVALID;
              exit;
            end;
        end;
      res := self._enabledAtPowerOn;
      result := res;
      exit;
    end;


  function TYServo.set_enabledAtPowerOn(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('enabledAtPowerOn',rest_val);
    end;

  class function TYServo.FindServo(func: string):TYServo;
    var
      obj : TYServo;
    begin
      obj := TYServo(TYFunction._FindFromCache('Servo', func));
      if obj = nil then
        begin
          obj :=  TYServo.create(func);
          TYFunction._AddToCache('Servo',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYServo.registerValueCallback(callback: TYServoValueCallback):LongInt;
    var
      val : string;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateValueCallbackList(self, true);
        end
      else
        begin
          TYFunction._UpdateValueCallbackList(self, false);
        end;
      self._valueCallbackServo := callback;
      // Immediately invoke value callback with current value
      if (addr(callback) <> nil) and self.isOnline then
        begin
          val := self._advertisedValue;
          if not((val = '')) then
            begin
              self._invokeValueCallback(val);
            end;
        end;
      result := 0;
      exit;
    end;


  function TYServo._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackServo) <> nil) then
        begin
          self._valueCallbackServo(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYServo.nextServo(): TYServo;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextServo := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextServo := nil;
          exit;
        end;
      nextServo := TYServo.FindServo(hwid);
    end;

  class function TYServo.FirstServo(): TYServo;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Servo', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
      if (YISERR(err) or (neededsize = 0)) then
        begin
          result := nil;
          exit;
        end;
      if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
        begin
          result := nil;
          exit;
        end;
     result := TYServo.FindServo(serial+'.'+funcId);
    end;

//--- (end of YServo implementation)

//--- (YServo functions)

  function yFindServo(func:string): TYServo;
    begin
      result := TYServo.FindServo(func);
    end;

  function yFirstServo(): TYServo;
    begin
      result := TYServo.FirstServo();
    end;

  procedure _ServoCleanup();
    begin
    end;

//--- (end of YServo functions)

initialization
  //--- (YServo initialization)
    Y_MOVE_INVALID := TYServoMove.Create();
  //--- (end of YServo initialization)

finalization
  //--- (YServo cleanup)
  _ServoCleanup();
  //--- (end of YServo cleanup)
end.
