{*********************************************************************
 *
 * $Id: yocto_motor.pas 26668 2017-02-28 13:36:03Z seb $
 *
 * Implements yFindMotor(), the high-level API for Motor functions
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


unit yocto_motor;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YMotor definitions)

const Y_MOTORSTATUS_IDLE = 0;
const Y_MOTORSTATUS_BRAKE = 1;
const Y_MOTORSTATUS_FORWD = 2;
const Y_MOTORSTATUS_BACKWD = 3;
const Y_MOTORSTATUS_LOVOLT = 4;
const Y_MOTORSTATUS_HICURR = 5;
const Y_MOTORSTATUS_HIHEAT = 6;
const Y_MOTORSTATUS_FAILSF = 7;
const Y_MOTORSTATUS_INVALID = -1;
const Y_DRIVINGFORCE_INVALID          = YAPI_INVALID_DOUBLE;
const Y_BRAKINGFORCE_INVALID          = YAPI_INVALID_DOUBLE;
const Y_CUTOFFVOLTAGE_INVALID         = YAPI_INVALID_DOUBLE;
const Y_OVERCURRENTLIMIT_INVALID      = YAPI_INVALID_INT;
const Y_FREQUENCY_INVALID             = YAPI_INVALID_DOUBLE;
const Y_STARTERTIME_INVALID           = YAPI_INVALID_INT;
const Y_FAILSAFETIMEOUT_INVALID       = YAPI_INVALID_UINT;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YMotor definitions)

type
  TYMotor = class;
  //--- (YMotor class start)
  TYMotorValueCallback = procedure(func: TYMotor; value:string);
  TYMotorTimedReportCallback = procedure(func: TYMotor; value:TYMeasure);

  ////
  /// <summary>
  ///   TYMotor Class: Motor function interface
  /// <para>
  ///   Yoctopuce application programming interface allows you to drive the
  ///   power sent to the motor to make it turn both ways, but also to drive accelerations
  ///   and decelerations. The motor will then accelerate automatically: you will not
  ///   have to monitor it. The API also allows to slow down the motor by shortening
  ///   its terminals: the motor will then act as an electromagnetic brake.
  /// </para>
  /// </summary>
  ///-
  TYMotor=class(TYFunction)
  //--- (end of YMotor class start)
  protected
  //--- (YMotor declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _motorStatus              : Integer;
    _drivingForce             : double;
    _brakingForce             : double;
    _cutOffVoltage            : double;
    _overCurrentLimit         : LongInt;
    _frequency                : double;
    _starterTime              : LongInt;
    _failSafeTimeout          : LongInt;
    _command                  : string;
    _valueCallbackMotor       : TYMotorValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YMotor declaration)

  public
    //--- (YMotor accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Return the controller state.
    /// <para>
    ///   Possible states are:
    ///   IDLE   when the motor is stopped/in free wheel, ready to start;
    ///   FORWD  when the controller is driving the motor forward;
    ///   BACKWD when the controller is driving the motor backward;
    ///   BRAKE  when the controller is braking;
    ///   LOVOLT when the controller has detected a low voltage condition;
    ///   HICURR when the controller has detected an overcurrent condition;
    ///   HIHEAT when the controller has detected an overheat condition;
    ///   FAILSF when the controller switched on the failsafe security.
    /// </para>
    /// <para>
    ///   When an error condition occurred (LOVOLT, HICURR, HIHEAT, FAILSF), the controller
    ///   status must be explicitly reset using the <c>resetStatus</c> function.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_MOTORSTATUS_IDLE</c>, <c>Y_MOTORSTATUS_BRAKE</c>, <c>Y_MOTORSTATUS_FORWD</c>,
    ///   <c>Y_MOTORSTATUS_BACKWD</c>, <c>Y_MOTORSTATUS_LOVOLT</c>, <c>Y_MOTORSTATUS_HICURR</c>,
    ///   <c>Y_MOTORSTATUS_HIHEAT</c> and <c>Y_MOTORSTATUS_FAILSF</c>
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MOTORSTATUS_INVALID</c>.
    /// </para>
    ///-
    function get_motorStatus():Integer;

    function set_motorStatus(newval:Integer):integer;

    ////
    /// <summary>
    ///   Changes immediately the power sent to the motor.
    /// <para>
    ///   The value is a percentage between -100%
    ///   to 100%. If you want go easy on your mechanics and avoid excessive current consumption,
    ///   try to avoid brutal power changes. For example, immediate transition from forward full power
    ///   to reverse full power is a very bad idea. Each time the driving power is modified, the
    ///   braking power is set to zero.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to immediately the power sent to the motor
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
    function set_drivingForce(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the power sent to the motor, as a percentage between -100% and +100%.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the power sent to the motor, as a percentage between -100% and +100%
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DRIVINGFORCE_INVALID</c>.
    /// </para>
    ///-
    function get_drivingForce():double;

    ////
    /// <summary>
    ///   Changes immediately the braking force applied to the motor (in percents).
    /// <para>
    ///   The value 0 corresponds to no braking (free wheel). When the braking force
    ///   is changed, the driving power is set to zero. The value is a percentage.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to immediately the braking force applied to the motor (in percents)
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
    function set_brakingForce(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the braking force applied to the motor, as a percentage.
    /// <para>
    ///   The value 0 corresponds to no braking (free wheel).
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the braking force applied to the motor, as a percentage
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BRAKINGFORCE_INVALID</c>.
    /// </para>
    ///-
    function get_brakingForce():double;

    ////
    /// <summary>
    ///   Changes the threshold voltage under which the controller automatically switches to error state
    ///   and prevents further current draw.
    /// <para>
    ///   This setting prevent damage to a battery that can
    ///   occur when drawing current from an "empty" battery.
    ///   Note that whatever the cutoff threshold, the controller switches to undervoltage
    ///   error state if the power supply goes under 3V, even for a very brief time.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the threshold voltage under which the controller
    ///   automatically switches to error state
    ///   and prevents further current draw
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
    function set_cutOffVoltage(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the threshold voltage under which the controller automatically switches to error state
    ///   and prevents further current draw.
    /// <para>
    ///   This setting prevents damage to a battery that can
    ///   occur when drawing current from an "empty" battery.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the threshold voltage under which the controller
    ///   automatically switches to error state
    ///   and prevents further current draw
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CUTOFFVOLTAGE_INVALID</c>.
    /// </para>
    ///-
    function get_cutOffVoltage():double;

    ////
    /// <summary>
    ///   Returns the current threshold (in mA) above which the controller automatically
    ///   switches to error state.
    /// <para>
    ///   A zero value means that there is no limit.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current threshold (in mA) above which the controller automatically
    ///   switches to error state
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_OVERCURRENTLIMIT_INVALID</c>.
    /// </para>
    ///-
    function get_overCurrentLimit():LongInt;

    ////
    /// <summary>
    ///   Changes the current threshold (in mA) above which the controller automatically
    ///   switches to error state.
    /// <para>
    ///   A zero value means that there is no limit. Note that whatever the
    ///   current limit is, the controller switches to OVERCURRENT status if the current
    ///   goes above 32A, even for a very brief time.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the current threshold (in mA) above which the controller automatically
    ///   switches to error state
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
    function set_overCurrentLimit(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Changes the PWM frequency used to control the motor.
    /// <para>
    ///   Low frequency is usually
    ///   more efficient and may help the motor to start, but an audible noise might be
    ///   generated. A higher frequency reduces the noise, but more energy is converted
    ///   into heat.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the PWM frequency used to control the motor
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
    function set_frequency(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the PWM frequency used to control the motor.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the PWM frequency used to control the motor
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_FREQUENCY_INVALID</c>.
    /// </para>
    ///-
    function get_frequency():double;

    ////
    /// <summary>
    ///   Returns the duration (in ms) during which the motor is driven at low frequency to help
    ///   it start up.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the duration (in ms) during which the motor is driven at low frequency to help
    ///   it start up
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_STARTERTIME_INVALID</c>.
    /// </para>
    ///-
    function get_starterTime():LongInt;

    ////
    /// <summary>
    ///   Changes the duration (in ms) during which the motor is driven at low frequency to help
    ///   it start up.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the duration (in ms) during which the motor is driven at low frequency to help
    ///   it start up
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
    function set_starterTime(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the delay in milliseconds allowed for the controller to run autonomously without
    ///   receiving any instruction from the control process.
    /// <para>
    ///   When this delay has elapsed,
    ///   the controller automatically stops the motor and switches to FAILSAFE error.
    ///   Failsafe security is disabled when the value is zero.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the delay in milliseconds allowed for the controller to run autonomously without
    ///   receiving any instruction from the control process
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_FAILSAFETIMEOUT_INVALID</c>.
    /// </para>
    ///-
    function get_failSafeTimeout():LongInt;

    ////
    /// <summary>
    ///   Changes the delay in milliseconds allowed for the controller to run autonomously without
    ///   receiving any instruction from the control process.
    /// <para>
    ///   When this delay has elapsed,
    ///   the controller automatically stops the motor and switches to FAILSAFE error.
    ///   Failsafe security is disabled when the value is zero.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the delay in milliseconds allowed for the controller to run autonomously without
    ///   receiving any instruction from the control process
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
    function set_failSafeTimeout(newval:LongInt):integer;

    function get_command():string;

    function set_command(newval:string):integer;

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
    ///   Use the method <c>YMotor.isOnline()</c> to test if $THEFUNCTION$ is
    ///   indeed online at a given time. In case of ambiguity when looking for
    ///   $AFUNCTION$ by logical name, no error is notified: the first instance
    ///   found is returned. The search is performed first by hardware name,
    ///   then by logical name.
    /// </para>
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$
    /// </param>
    /// <returns>
    ///   a <c>YMotor</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindMotor(func: string):TYMotor;

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
    function registerValueCallback(callback: TYMotorValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Rearms the controller failsafe timer.
    /// <para>
    ///   When the motor is running and the failsafe feature
    ///   is active, this function should be called periodically to prove that the control process
    ///   is running properly. Otherwise, the motor is automatically stopped after the specified
    ///   timeout. Calling a motor <i>set</i> function implicitely rearms the failsafe timer.
    /// </para>
    /// </summary>
    ///-
    function keepALive():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reset the controller state to IDLE.
    /// <para>
    ///   This function must be invoked explicitely
    ///   after any error condition is signaled.
    /// </para>
    /// </summary>
    ///-
    function resetStatus():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes progressively the power sent to the moteur for a specific duration.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="targetPower">
    ///   desired motor power, in percents (between -100% and +100%)
    /// </param>
    /// <param name="delay">
    ///   duration (in ms) of the transition
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function drivingForceMove(targetPower: double; delay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes progressively the braking force applied to the motor for a specific duration.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="targetPower">
    ///   desired braking force, in percents
    /// </param>
    /// <param name="delay">
    ///   duration (in ms) of the transition
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function brakingForceMove(targetPower: double; delay: LongInt):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of motors started using <c>yFirstMotor()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YMotor</c> object, corresponding to
    ///   a motor currently online, or a <c>NIL</c> pointer
    ///   if there are no more motors to enumerate.
    /// </returns>
    ///-
    function nextMotor():TYMotor;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstMotor():TYMotor;
  //--- (end of YMotor accessors declaration)
  end;

//--- (Motor functions declaration)
  ////
  /// <summary>
  ///   Retrieves a motor for a given identifier.
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
  ///   This function does not require that the motor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YMotor.isOnline()</c> to test if the motor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a motor by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the motor
  /// </param>
  /// <returns>
  ///   a <c>YMotor</c> object allowing you to drive the motor.
  /// </returns>
  ///-
  function yFindMotor(func:string):TYMotor;
  ////
  /// <summary>
  ///   Starts the enumeration of motors currently accessible.
  /// <para>
  ///   Use the method <c>YMotor.nextMotor()</c> to iterate on
  ///   next motors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YMotor</c> object, corresponding to
  ///   the first motor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstMotor():TYMotor;

//--- (end of Motor functions declaration)

implementation
//--- (YMotor dlldef)
//--- (end of YMotor dlldef)

  constructor TYMotor.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Motor';
      //--- (YMotor accessors initialization)
      _motorStatus := Y_MOTORSTATUS_INVALID;
      _drivingForce := Y_DRIVINGFORCE_INVALID;
      _brakingForce := Y_BRAKINGFORCE_INVALID;
      _cutOffVoltage := Y_CUTOFFVOLTAGE_INVALID;
      _overCurrentLimit := Y_OVERCURRENTLIMIT_INVALID;
      _frequency := Y_FREQUENCY_INVALID;
      _starterTime := Y_STARTERTIME_INVALID;
      _failSafeTimeout := Y_FAILSAFETIMEOUT_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackMotor := nil;
      //--- (end of YMotor accessors initialization)
    end;


//--- (YMotor implementation)
{$HINTS OFF}
  function TYMotor._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'motorStatus') then
        begin
          _motorStatus := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'drivingForce') then
        begin
          _drivingForce := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'brakingForce') then
        begin
          _brakingForce := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'cutOffVoltage') then
        begin
          _cutOffVoltage := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'overCurrentLimit') then
        begin
          _overCurrentLimit := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'frequency') then
        begin
          _frequency := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'starterTime') then
        begin
          _starterTime := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'failSafeTimeout') then
        begin
          _failSafeTimeout := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'command') then
        begin
          _command := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Return the controller state.
  /// <para>
  ///   Possible states are:
  ///   IDLE   when the motor is stopped/in free wheel, ready to start;
  ///   FORWD  when the controller is driving the motor forward;
  ///   BACKWD when the controller is driving the motor backward;
  ///   BRAKE  when the controller is braking;
  ///   LOVOLT when the controller has detected a low voltage condition;
  ///   HICURR when the controller has detected an overcurrent condition;
  ///   HIHEAT when the controller has detected an overheat condition;
  ///   FAILSF when the controller switched on the failsafe security.
  /// </para>
  /// <para>
  ///   When an error condition occurred (LOVOLT, HICURR, HIHEAT, FAILSF), the controller
  ///   status must be explicitly reset using the resetStatus function.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_MOTORSTATUS_IDLE, Y_MOTORSTATUS_BRAKE, Y_MOTORSTATUS_FORWD, Y_MOTORSTATUS_BACKWD,
  ///   Y_MOTORSTATUS_LOVOLT, Y_MOTORSTATUS_HICURR, Y_MOTORSTATUS_HIHEAT and Y_MOTORSTATUS_FAILSF
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_MOTORSTATUS_INVALID.
  /// </para>
  ///-
  function TYMotor.get_motorStatus():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MOTORSTATUS_INVALID;
              exit;
            end;
        end;
      res := self._motorStatus;
      result := res;
      exit;
    end;


  function TYMotor.set_motorStatus(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('motorStatus',rest_val);
    end;

  ////
  /// <summary>
  ///   Changes immediately the power sent to the motor.
  /// <para>
  ///   The value is a percentage between -100%
  ///   to 100%. If you want go easy on your mechanics and avoid excessive current consumption,
  ///   try to avoid brutal power changes. For example, immediate transition from forward full power
  ///   to reverse full power is a very bad idea. Each time the driving power is modified, the
  ///   braking power is set to zero.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a floating point number corresponding to immediately the power sent to the motor
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
  function TYMotor.set_drivingForce(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('drivingForce',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the power sent to the motor, as a percentage between -100% and +100%.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the power sent to the motor, as a percentage between -100% and +100%
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DRIVINGFORCE_INVALID.
  /// </para>
  ///-
  function TYMotor.get_drivingForce():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_DRIVINGFORCE_INVALID;
              exit;
            end;
        end;
      res := self._drivingForce;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes immediately the braking force applied to the motor (in percents).
  /// <para>
  ///   The value 0 corresponds to no braking (free wheel). When the braking force
  ///   is changed, the driving power is set to zero. The value is a percentage.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a floating point number corresponding to immediately the braking force applied to the motor (in percents)
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
  function TYMotor.set_brakingForce(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('brakingForce',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the braking force applied to the motor, as a percentage.
  /// <para>
  ///   The value 0 corresponds to no braking (free wheel).
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the braking force applied to the motor, as a percentage
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_BRAKINGFORCE_INVALID.
  /// </para>
  ///-
  function TYMotor.get_brakingForce():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_BRAKINGFORCE_INVALID;
              exit;
            end;
        end;
      res := self._brakingForce;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the threshold voltage under which the controller automatically switches to error state
  ///   and prevents further current draw.
  /// <para>
  ///   This setting prevent damage to a battery that can
  ///   occur when drawing current from an "empty" battery.
  ///   Note that whatever the cutoff threshold, the controller switches to undervoltage
  ///   error state if the power supply goes under 3V, even for a very brief time.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a floating point number corresponding to the threshold voltage under which the controller
  ///   automatically switches to error state
  ///   and prevents further current draw
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
  function TYMotor.set_cutOffVoltage(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('cutOffVoltage',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the threshold voltage under which the controller automatically switches to error state
  ///   and prevents further current draw.
  /// <para>
  ///   This setting prevents damage to a battery that can
  ///   occur when drawing current from an "empty" battery.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the threshold voltage under which the controller
  ///   automatically switches to error state
  ///   and prevents further current draw
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_CUTOFFVOLTAGE_INVALID.
  /// </para>
  ///-
  function TYMotor.get_cutOffVoltage():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CUTOFFVOLTAGE_INVALID;
              exit;
            end;
        end;
      res := self._cutOffVoltage;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the current threshold (in mA) above which the controller automatically
  ///   switches to error state.
  /// <para>
  ///   A zero value means that there is no limit.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the current threshold (in mA) above which the controller automatically
  ///   switches to error state
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_OVERCURRENTLIMIT_INVALID.
  /// </para>
  ///-
  function TYMotor.get_overCurrentLimit():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_OVERCURRENTLIMIT_INVALID;
              exit;
            end;
        end;
      res := self._overCurrentLimit;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the current threshold (in mA) above which the controller automatically
  ///   switches to error state.
  /// <para>
  ///   A zero value means that there is no limit. Note that whatever the
  ///   current limit is, the controller switches to OVERCURRENT status if the current
  ///   goes above 32A, even for a very brief time.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the current threshold (in mA) above which the controller automatically
  ///   switches to error state
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
  function TYMotor.set_overCurrentLimit(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('overCurrentLimit',rest_val);
    end;

  ////
  /// <summary>
  ///   Changes the PWM frequency used to control the motor.
  /// <para>
  ///   Low frequency is usually
  ///   more efficient and may help the motor to start, but an audible noise might be
  ///   generated. A higher frequency reduces the noise, but more energy is converted
  ///   into heat.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a floating point number corresponding to the PWM frequency used to control the motor
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
  function TYMotor.set_frequency(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('frequency',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the PWM frequency used to control the motor.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the PWM frequency used to control the motor
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_FREQUENCY_INVALID.
  /// </para>
  ///-
  function TYMotor.get_frequency():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_FREQUENCY_INVALID;
              exit;
            end;
        end;
      res := self._frequency;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the duration (in ms) during which the motor is driven at low frequency to help
  ///   it start up.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the duration (in ms) during which the motor is driven at low frequency to help
  ///   it start up
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_STARTERTIME_INVALID.
  /// </para>
  ///-
  function TYMotor.get_starterTime():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_STARTERTIME_INVALID;
              exit;
            end;
        end;
      res := self._starterTime;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the duration (in ms) during which the motor is driven at low frequency to help
  ///   it start up.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the duration (in ms) during which the motor is driven at low frequency to help
  ///   it start up
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
  function TYMotor.set_starterTime(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('starterTime',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the delay in milliseconds allowed for the controller to run autonomously without
  ///   receiving any instruction from the control process.
  /// <para>
  ///   When this delay has elapsed,
  ///   the controller automatically stops the motor and switches to FAILSAFE error.
  ///   Failsafe security is disabled when the value is zero.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the delay in milliseconds allowed for the controller to run autonomously without
  ///   receiving any instruction from the control process
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_FAILSAFETIMEOUT_INVALID.
  /// </para>
  ///-
  function TYMotor.get_failSafeTimeout():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_FAILSAFETIMEOUT_INVALID;
              exit;
            end;
        end;
      res := self._failSafeTimeout;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the delay in milliseconds allowed for the controller to run autonomously without
  ///   receiving any instruction from the control process.
  /// <para>
  ///   When this delay has elapsed,
  ///   the controller automatically stops the motor and switches to FAILSAFE error.
  ///   Failsafe security is disabled when the value is zero.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the delay in milliseconds allowed for the controller to run autonomously without
  ///   receiving any instruction from the control process
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
  function TYMotor.set_failSafeTimeout(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('failSafeTimeout',rest_val);
    end;

  function TYMotor.get_command():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_COMMAND_INVALID;
              exit;
            end;
        end;
      res := self._command;
      result := res;
      exit;
    end;


  function TYMotor.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

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
  ///   Use the method <c>YMotor.isOnline()</c> to test if $THEFUNCTION$ is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   $AFUNCTION$ by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes $THEFUNCTION$
  /// </param>
  /// <returns>
  ///   a <c>YMotor</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYMotor.FindMotor(func: string):TYMotor;
    var
      obj : TYMotor;
    begin
      obj := TYMotor(TYFunction._FindFromCache('Motor', func));
      if obj = nil then
        begin
          obj :=  TYMotor.create(func);
          TYFunction._AddToCache('Motor',  func, obj);
        end;
      result := obj;
      exit;
    end;


  ////
  /// <summary>
  ///   Registers the callback function that is invoked on every change of advertised value.
  /// <para>
  ///   The callback is invoked only during the execution of <c>ySleep</c> or <c>yHandleEvents</c>.
  ///   This provides control over the time when the callback is triggered. For good responsiveness, remember to call
  ///   one of these two functions periodically. To unregister a callback, pass a null pointer as argument.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="callback">
  ///   the callback function to call, or a null pointer. The callback function should take two
  ///   arguments: the function object of which the value has changed, and the character string describing
  ///   the new advertised value.
  /// @noreturn
  /// </param>
  ///-
  function TYMotor.registerValueCallback(callback: TYMotorValueCallback):LongInt;
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
      self._valueCallbackMotor := callback;
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


  function TYMotor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackMotor) <> nil) then
        begin
          self._valueCallbackMotor(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Rearms the controller failsafe timer.
  /// <para>
  ///   When the motor is running and the failsafe feature
  ///   is active, this function should be called periodically to prove that the control process
  ///   is running properly. Otherwise, the motor is automatically stopped after the specified
  ///   timeout. Calling a motor <i>set</i> function implicitely rearms the failsafe timer.
  /// </para>
  /// </summary>
  ///-
  function TYMotor.keepALive():LongInt;
    begin
      result := self.set_command('K');
      exit;
    end;


  ////
  /// <summary>
  ///   Reset the controller state to IDLE.
  /// <para>
  ///   This function must be invoked explicitely
  ///   after any error condition is signaled.
  /// </para>
  /// </summary>
  ///-
  function TYMotor.resetStatus():LongInt;
    begin
      result := self.set_motorStatus(Y_MOTORSTATUS_IDLE);
      exit;
    end;


  ////
  /// <summary>
  ///   Changes progressively the power sent to the moteur for a specific duration.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="targetPower">
  ///   desired motor power, in percents (between -100% and +100%)
  /// </param>
  /// <param name="delay">
  ///   duration (in ms) of the transition
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYMotor.drivingForceMove(targetPower: double; delay: LongInt):LongInt;
    begin
      result := self.set_command('P'+inttostr(round(targetPower*10))+','+inttostr(delay));
      exit;
    end;


  ////
  /// <summary>
  ///   Changes progressively the braking force applied to the motor for a specific duration.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="targetPower">
  ///   desired braking force, in percents
  /// </param>
  /// <param name="delay">
  ///   duration (in ms) of the transition
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYMotor.brakingForceMove(targetPower: double; delay: LongInt):LongInt;
    begin
      result := self.set_command('B'+inttostr(round(targetPower*10))+','+inttostr(delay));
      exit;
    end;


  function TYMotor.nextMotor(): TYMotor;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextMotor := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextMotor := nil;
          exit;
        end;
      nextMotor := TYMotor.FindMotor(hwid);
    end;

  class function TYMotor.FirstMotor(): TYMotor;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Motor', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYMotor.FindMotor(serial+'.'+funcId);
    end;

//--- (end of YMotor implementation)

//--- (Motor functions)

  function yFindMotor(func:string): TYMotor;
    begin
      result := TYMotor.FindMotor(func);
    end;

  function yFirstMotor(): TYMotor;
    begin
      result := TYMotor.FirstMotor();
    end;

  procedure _MotorCleanup();
    begin
    end;

//--- (end of Motor functions)

initialization
  //--- (Motor initialization)
  //--- (end of Motor initialization)

finalization
  //--- (Motor cleanup)
  _MotorCleanup();
  //--- (end of Motor cleanup)
end.
