{*********************************************************************
 *
 *  $Id: yocto_steppermotor.pas 37827 2019-10-25 13:07:48Z mvuilleu $
 *
 *  Implements yFindStepperMotor(), the high-level API for StepperMotor functions
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


unit yocto_steppermotor;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YStepperMotor definitions)

const Y_MOTORSTATE_ABSENT = 0;
const Y_MOTORSTATE_ALERT = 1;
const Y_MOTORSTATE_HI_Z = 2;
const Y_MOTORSTATE_STOP = 3;
const Y_MOTORSTATE_RUN = 4;
const Y_MOTORSTATE_BATCH = 5;
const Y_MOTORSTATE_INVALID = -1;
const Y_DIAGS_INVALID                 = YAPI_INVALID_UINT;
const Y_STEPPOS_INVALID               = YAPI_INVALID_DOUBLE;
const Y_SPEED_INVALID                 = YAPI_INVALID_DOUBLE;
const Y_PULLINSPEED_INVALID           = YAPI_INVALID_DOUBLE;
const Y_MAXACCEL_INVALID              = YAPI_INVALID_DOUBLE;
const Y_MAXSPEED_INVALID              = YAPI_INVALID_DOUBLE;
const Y_STEPPING_MICROSTEP16 = 0;
const Y_STEPPING_MICROSTEP8 = 1;
const Y_STEPPING_MICROSTEP4 = 2;
const Y_STEPPING_HALFSTEP = 3;
const Y_STEPPING_FULLSTEP = 4;
const Y_STEPPING_INVALID = -1;
const Y_OVERCURRENT_INVALID           = YAPI_INVALID_UINT;
const Y_TCURRSTOP_INVALID             = YAPI_INVALID_UINT;
const Y_TCURRRUN_INVALID              = YAPI_INVALID_UINT;
const Y_ALERTMODE_INVALID             = YAPI_INVALID_STRING;
const Y_AUXMODE_INVALID               = YAPI_INVALID_STRING;
const Y_AUXSIGNAL_INVALID             = YAPI_INVALID_INT;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YStepperMotor definitions)
//--- (YStepperMotor yapiwrapper declaration)
//--- (end of YStepperMotor yapiwrapper declaration)

type
  TYStepperMotor = class;
  //--- (YStepperMotor class start)
  TYStepperMotorValueCallback = procedure(func: TYStepperMotor; value:string);
  TYStepperMotorTimedReportCallback = procedure(func: TYStepperMotor; value:TYMeasure);

  ////
  /// <summary>
  ///   TYStepperMotor Class: StepperMotor function interface
  /// <para>
  ///   The YStepperMotor class allows you to drive a stepper motor.
  /// </para>
  /// </summary>
  ///-
  TYStepperMotor=class(TYFunction)
  //--- (end of YStepperMotor class start)
  protected
  //--- (YStepperMotor declaration)
    // Attributes (function value cache)
    _motorState               : Integer;
    _diags                    : LongInt;
    _stepPos                  : double;
    _speed                    : double;
    _pullinSpeed              : double;
    _maxAccel                 : double;
    _maxSpeed                 : double;
    _stepping                 : Integer;
    _overcurrent              : LongInt;
    _tCurrStop                : LongInt;
    _tCurrRun                 : LongInt;
    _alertMode                : string;
    _auxMode                  : string;
    _auxSignal                : LongInt;
    _command                  : string;
    _valueCallbackStepperMotor : TYStepperMotorValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YStepperMotor declaration)

  public
    //--- (YStepperMotor accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the motor working state.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_MOTORSTATE_ABSENT</c>, <c>Y_MOTORSTATE_ALERT</c>, <c>Y_MOTORSTATE_HI_Z</c>,
    ///   <c>Y_MOTORSTATE_STOP</c>, <c>Y_MOTORSTATE_RUN</c> and <c>Y_MOTORSTATE_BATCH</c> corresponding to
    ///   the motor working state
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MOTORSTATE_INVALID</c>.
    /// </para>
    ///-
    function get_motorState():Integer;

    ////
    /// <summary>
    ///   Returns the stepper motor controller diagnostics, as a bitmap.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the stepper motor controller diagnostics, as a bitmap
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DIAGS_INVALID</c>.
    /// </para>
    ///-
    function get_diags():LongInt;

    ////
    /// <summary>
    ///   Changes the current logical motor position, measured in steps.
    /// <para>
    ///   This command does not cause any motor move, as its purpose is only to setup
    ///   the origin of the position counter. The fractional part of the position,
    ///   that corresponds to the physical position of the rotor, is not changed.
    ///   To trigger a motor move, use methods <c>moveTo()</c> or <c>moveRel()</c>
    ///   instead.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the current logical motor position, measured in steps
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
    function set_stepPos(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the current logical motor position, measured in steps.
    /// <para>
    ///   The value may include a fractional part when micro-stepping is in use.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current logical motor position, measured in steps
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_STEPPOS_INVALID</c>.
    /// </para>
    ///-
    function get_stepPos():double;

    ////
    /// <summary>
    ///   Returns current motor speed, measured in steps per second.
    /// <para>
    ///   To change speed, use method <c>changeSpeed()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to current motor speed, measured in steps per second
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SPEED_INVALID</c>.
    /// </para>
    ///-
    function get_speed():double;

    ////
    /// <summary>
    ///   Changes the motor speed immediately reachable from stop state, measured in steps per second.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the motor speed immediately reachable from stop state,
    ///   measured in steps per second
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
    function set_pullinSpeed(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the motor speed immediately reachable from stop state, measured in steps per second.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the motor speed immediately reachable from stop state,
    ///   measured in steps per second
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PULLINSPEED_INVALID</c>.
    /// </para>
    ///-
    function get_pullinSpeed():double;

    ////
    /// <summary>
    ///   Changes the maximal motor acceleration, measured in steps per second^2.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the maximal motor acceleration, measured in steps per second^2
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
    function set_maxAccel(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the maximal motor acceleration, measured in steps per second^2.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the maximal motor acceleration, measured in steps per second^2
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MAXACCEL_INVALID</c>.
    /// </para>
    ///-
    function get_maxAccel():double;

    ////
    /// <summary>
    ///   Changes the maximal motor speed, measured in steps per second.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the maximal motor speed, measured in steps per second
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
    function set_maxSpeed(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the maximal motor speed, measured in steps per second.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the maximal motor speed, measured in steps per second
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MAXSPEED_INVALID</c>.
    /// </para>
    ///-
    function get_maxSpeed():double;

    ////
    /// <summary>
    ///   Returns the stepping mode used to drive the motor.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_STEPPING_MICROSTEP16</c>, <c>Y_STEPPING_MICROSTEP8</c>,
    ///   <c>Y_STEPPING_MICROSTEP4</c>, <c>Y_STEPPING_HALFSTEP</c> and <c>Y_STEPPING_FULLSTEP</c>
    ///   corresponding to the stepping mode used to drive the motor
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_STEPPING_INVALID</c>.
    /// </para>
    ///-
    function get_stepping():Integer;

    ////
    /// <summary>
    ///   Changes the stepping mode used to drive the motor.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_STEPPING_MICROSTEP16</c>, <c>Y_STEPPING_MICROSTEP8</c>,
    ///   <c>Y_STEPPING_MICROSTEP4</c>, <c>Y_STEPPING_HALFSTEP</c> and <c>Y_STEPPING_FULLSTEP</c>
    ///   corresponding to the stepping mode used to drive the motor
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
    function set_stepping(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the overcurrent alert and emergency stop threshold, measured in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the overcurrent alert and emergency stop threshold, measured in mA
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_OVERCURRENT_INVALID</c>.
    /// </para>
    ///-
    function get_overcurrent():LongInt;

    ////
    /// <summary>
    ///   Changes the overcurrent alert and emergency stop threshold, measured in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the overcurrent alert and emergency stop threshold, measured in mA
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
    function set_overcurrent(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the torque regulation current when the motor is stopped, measured in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the torque regulation current when the motor is stopped, measured in mA
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_TCURRSTOP_INVALID</c>.
    /// </para>
    ///-
    function get_tCurrStop():LongInt;

    ////
    /// <summary>
    ///   Changes the torque regulation current when the motor is stopped, measured in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the torque regulation current when the motor is stopped, measured in mA
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
    function set_tCurrStop(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the torque regulation current when the motor is running, measured in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the torque regulation current when the motor is running, measured in mA
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_TCURRRUN_INVALID</c>.
    /// </para>
    ///-
    function get_tCurrRun():LongInt;

    ////
    /// <summary>
    ///   Changes the torque regulation current when the motor is running, measured in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the torque regulation current when the motor is running, measured in mA
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
    function set_tCurrRun(newval:LongInt):integer;

    function get_alertMode():string;

    function set_alertMode(newval:string):integer;

    function get_auxMode():string;

    function set_auxMode(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the current value of the signal generated on the auxiliary output.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current value of the signal generated on the auxiliary output
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_AUXSIGNAL_INVALID</c>.
    /// </para>
    ///-
    function get_auxSignal():LongInt;

    ////
    /// <summary>
    ///   Changes the value of the signal generated on the auxiliary output.
    /// <para>
    ///   Acceptable values depend on the auxiliary output signal type configured.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the value of the signal generated on the auxiliary output
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
    function set_auxSignal(newval:LongInt):integer;

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
    ///   Use the method <c>YStepperMotor.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a string that uniquely characterizes $THEFUNCTION$, for instance
    ///   <c>$FULLHARDWAREID$</c>.
    /// </param>
    /// <returns>
    ///   a <c>YStepperMotor</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindStepperMotor(func: string):TYStepperMotor;

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
    function registerValueCallback(callback: TYStepperMotorValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function sendCommand(command: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reinitialize the controller and clear all alert flags.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function reset():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts the motor backward at the specified speed, to search for the motor home position.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="speed">
    ///   desired speed, in steps per second.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function findHomePosition(speed: double):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts the motor at a given speed.
    /// <para>
    ///   The time needed to reach the requested speed
    ///   will depend on the acceleration parameters configured for the motor.
    /// </para>
    /// </summary>
    /// <param name="speed">
    ///   desired speed, in steps per second. The minimal non-zero speed
    ///   is 0.001 pulse per second.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function changeSpeed(speed: double):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts the motor to reach a given absolute position.
    /// <para>
    ///   The time needed to reach the requested
    ///   position will depend on the acceleration and max speed parameters configured for
    ///   the motor.
    /// </para>
    /// </summary>
    /// <param name="absPos">
    ///   absolute position, measured in steps from the origin.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function moveTo(absPos: double):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts the motor to reach a given relative position.
    /// <para>
    ///   The time needed to reach the requested
    ///   position will depend on the acceleration and max speed parameters configured for
    ///   the motor.
    /// </para>
    /// </summary>
    /// <param name="relPos">
    ///   relative position, measured in steps from the current position.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function moveRel(relPos: double):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts the motor to reach a given relative position, keeping the speed under the
    ///   specified limit.
    /// <para>
    ///   The time needed to reach the requested position will depend on
    ///   the acceleration parameters configured for the motor.
    /// </para>
    /// </summary>
    /// <param name="relPos">
    ///   relative position, measured in steps from the current position.
    /// </param>
    /// <param name="maxSpeed">
    ///   limit speed, in steps per second.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function moveRelSlow(relPos: double; maxSpeed: double):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Keep the motor in the same state for the specified amount of time, before processing next command.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="waitMs">
    ///   wait time, specified in milliseconds.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function pause(waitMs: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops the motor with an emergency alert, without taking any additional precaution.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function emergencyStop():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Move one step in the direction opposite the direction set when the most recent alert was raised.
    /// <para>
    ///   The move occurs even if the system is still in alert mode (end switch depressed). Caution.
    ///   use this function with great care as it may cause mechanical damages !
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function alertStepOut():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Move one single step in the selected direction without regards to end switches.
    /// <para>
    ///   The move occurs even if the system is still in alert mode (end switch depressed). Caution.
    ///   use this function with great care as it may cause mechanical damages !
    /// </para>
    /// </summary>
    /// <param name="dir">
    ///   Value +1 or -1, according to the desired direction of the move
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function alertStepDir(dir: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops the motor smoothly as soon as possible, without waiting for ongoing move completion.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function abortAndBrake():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Turn the controller into Hi-Z mode immediately, without waiting for ongoing move completion.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function abortAndHiZ():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of stepper motors started using <c>yFirstStepperMotor()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned stepper motors order.
    ///   If you want to find a specific a stepper motor, use <c>StepperMotor.findStepperMotor()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YStepperMotor</c> object, corresponding to
    ///   a stepper motor currently online, or a <c>NIL</c> pointer
    ///   if there are no more stepper motors to enumerate.
    /// </returns>
    ///-
    function nextStepperMotor():TYStepperMotor;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstStepperMotor():TYStepperMotor;
  //--- (end of YStepperMotor accessors declaration)
  end;

//--- (YStepperMotor functions declaration)
  ////
  /// <summary>
  ///   Retrieves a stepper motor for a given identifier.
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
  ///   This function does not require that the stepper motor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YStepperMotor.isOnline()</c> to test if the stepper motor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a stepper motor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the stepper motor, for instance
  ///   <c>MyDevice.stepperMotor1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YStepperMotor</c> object allowing you to drive the stepper motor.
  /// </returns>
  ///-
  function yFindStepperMotor(func:string):TYStepperMotor;
  ////
  /// <summary>
  ///   Starts the enumeration of stepper motors currently accessible.
  /// <para>
  ///   Use the method <c>YStepperMotor.nextStepperMotor()</c> to iterate on
  ///   next stepper motors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YStepperMotor</c> object, corresponding to
  ///   the first stepper motor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstStepperMotor():TYStepperMotor;

//--- (end of YStepperMotor functions declaration)

implementation
//--- (YStepperMotor dlldef)
//--- (end of YStepperMotor dlldef)

  constructor TYStepperMotor.Create(func:string);
    begin
      inherited Create(func);
      _className := 'StepperMotor';
      //--- (YStepperMotor accessors initialization)
      _motorState := Y_MOTORSTATE_INVALID;
      _diags := Y_DIAGS_INVALID;
      _stepPos := Y_STEPPOS_INVALID;
      _speed := Y_SPEED_INVALID;
      _pullinSpeed := Y_PULLINSPEED_INVALID;
      _maxAccel := Y_MAXACCEL_INVALID;
      _maxSpeed := Y_MAXSPEED_INVALID;
      _stepping := Y_STEPPING_INVALID;
      _overcurrent := Y_OVERCURRENT_INVALID;
      _tCurrStop := Y_TCURRSTOP_INVALID;
      _tCurrRun := Y_TCURRRUN_INVALID;
      _alertMode := Y_ALERTMODE_INVALID;
      _auxMode := Y_AUXMODE_INVALID;
      _auxSignal := Y_AUXSIGNAL_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackStepperMotor := nil;
      //--- (end of YStepperMotor accessors initialization)
    end;

//--- (YStepperMotor yapiwrapper)
//--- (end of YStepperMotor yapiwrapper)

//--- (YStepperMotor implementation)
{$HINTS OFF}
  function TYStepperMotor._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'motorState') then
        begin
          _motorState := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'diags') then
        begin
          _diags := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'stepPos') then
        begin
          _stepPos := member^.ivalue / 16.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'speed') then
        begin
          _speed := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'pullinSpeed') then
        begin
          _pullinSpeed := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'maxAccel') then
        begin
          _maxAccel := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'maxSpeed') then
        begin
          _maxSpeed := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'stepping') then
        begin
          _stepping := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'overcurrent') then
        begin
          _overcurrent := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'tCurrStop') then
        begin
          _tCurrStop := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'tCurrRun') then
        begin
          _tCurrRun := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'alertMode') then
        begin
          _alertMode := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'auxMode') then
        begin
          _auxMode := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'auxSignal') then
        begin
          _auxSignal := integer(member^.ivalue);
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

  function TYStepperMotor.get_motorState():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MOTORSTATE_INVALID;
              exit;
            end;
        end;
      res := self._motorState;
      result := res;
      exit;
    end;


  function TYStepperMotor.get_diags():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DIAGS_INVALID;
              exit;
            end;
        end;
      res := self._diags;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_stepPos(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := floattostrf(round(newval * 100.0)/100.0, ffGeneral, 16, 2);
      result := _setAttr('stepPos',rest_val);
    end;

  function TYStepperMotor.get_stepPos():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_STEPPOS_INVALID;
              exit;
            end;
        end;
      res := self._stepPos;
      result := res;
      exit;
    end;


  function TYStepperMotor.get_speed():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SPEED_INVALID;
              exit;
            end;
        end;
      res := self._speed;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_pullinSpeed(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('pullinSpeed',rest_val);
    end;

  function TYStepperMotor.get_pullinSpeed():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PULLINSPEED_INVALID;
              exit;
            end;
        end;
      res := self._pullinSpeed;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_maxAccel(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('maxAccel',rest_val);
    end;

  function TYStepperMotor.get_maxAccel():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MAXACCEL_INVALID;
              exit;
            end;
        end;
      res := self._maxAccel;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_maxSpeed(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('maxSpeed',rest_val);
    end;

  function TYStepperMotor.get_maxSpeed():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MAXSPEED_INVALID;
              exit;
            end;
        end;
      res := self._maxSpeed;
      result := res;
      exit;
    end;


  function TYStepperMotor.get_stepping():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_STEPPING_INVALID;
              exit;
            end;
        end;
      res := self._stepping;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_stepping(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('stepping',rest_val);
    end;

  function TYStepperMotor.get_overcurrent():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_OVERCURRENT_INVALID;
              exit;
            end;
        end;
      res := self._overcurrent;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_overcurrent(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('overcurrent',rest_val);
    end;

  function TYStepperMotor.get_tCurrStop():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_TCURRSTOP_INVALID;
              exit;
            end;
        end;
      res := self._tCurrStop;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_tCurrStop(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('tCurrStop',rest_val);
    end;

  function TYStepperMotor.get_tCurrRun():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_TCURRRUN_INVALID;
              exit;
            end;
        end;
      res := self._tCurrRun;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_tCurrRun(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('tCurrRun',rest_val);
    end;

  function TYStepperMotor.get_alertMode():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ALERTMODE_INVALID;
              exit;
            end;
        end;
      res := self._alertMode;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_alertMode(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('alertMode',rest_val);
    end;

  function TYStepperMotor.get_auxMode():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_AUXMODE_INVALID;
              exit;
            end;
        end;
      res := self._auxMode;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_auxMode(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('auxMode',rest_val);
    end;

  function TYStepperMotor.get_auxSignal():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_AUXSIGNAL_INVALID;
              exit;
            end;
        end;
      res := self._auxSignal;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_auxSignal(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('auxSignal',rest_val);
    end;

  function TYStepperMotor.get_command():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_COMMAND_INVALID;
              exit;
            end;
        end;
      res := self._command;
      result := res;
      exit;
    end;


  function TYStepperMotor.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYStepperMotor.FindStepperMotor(func: string):TYStepperMotor;
    var
      obj : TYStepperMotor;
    begin
      obj := TYStepperMotor(TYFunction._FindFromCache('StepperMotor', func));
      if obj = nil then
        begin
          obj :=  TYStepperMotor.create(func);
          TYFunction._AddToCache('StepperMotor',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYStepperMotor.registerValueCallback(callback: TYStepperMotorValueCallback):LongInt;
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
      self._valueCallbackStepperMotor := callback;
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


  function TYStepperMotor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackStepperMotor) <> nil) then
        begin
          self._valueCallbackStepperMotor(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYStepperMotor.sendCommand(command: string):LongInt;
    var
      id : string;
      url : string;
      retBin : TByteArray;
      res : LongInt;
    begin
      id := self.get_functionId;
      id := Copy(id,  12 + 1, 1);
      url := 'cmd.txt?'+ id+'='+command;
      //may throw an exception
      retBin := self._download(url);
      res := retBin[0];
      if res = 49 then
        begin
          if not(res = 48) then
            begin
              self._throw( YAPI_DEVICE_BUSY, 'Motor command pipeline is full, try again later');
              result:=YAPI_DEVICE_BUSY;
              exit;
            end;
        end
      else
        begin
          if not(res = 48) then
            begin
              self._throw( YAPI_IO_ERROR, 'Motor command failed permanently');
              result:=YAPI_IO_ERROR;
              exit;
            end;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYStepperMotor.reset():LongInt;
    begin
      result := self.set_command('Z');
      exit;
    end;


  function TYStepperMotor.findHomePosition(speed: double):LongInt;
    begin
      result := self.sendCommand('H'+inttostr(round(1000*speed)));
      exit;
    end;


  function TYStepperMotor.changeSpeed(speed: double):LongInt;
    begin
      result := self.sendCommand('R'+inttostr(round(1000*speed)));
      exit;
    end;


  function TYStepperMotor.moveTo(absPos: double):LongInt;
    begin
      result := self.sendCommand('M'+inttostr(round(16*absPos)));
      exit;
    end;


  function TYStepperMotor.moveRel(relPos: double):LongInt;
    begin
      result := self.sendCommand('m'+inttostr(round(16*relPos)));
      exit;
    end;


  function TYStepperMotor.moveRelSlow(relPos: double; maxSpeed: double):LongInt;
    begin
      result := self.sendCommand('m'+inttostr(round(16*relPos))+'@'+inttostr(round(1000*maxSpeed)));
      exit;
    end;


  function TYStepperMotor.pause(waitMs: LongInt):LongInt;
    begin
      result := self.sendCommand('_'+inttostr(waitMs));
      exit;
    end;


  function TYStepperMotor.emergencyStop():LongInt;
    begin
      result := self.set_command('!');
      exit;
    end;


  function TYStepperMotor.alertStepOut():LongInt;
    begin
      result := self.set_command('.');
      exit;
    end;


  function TYStepperMotor.alertStepDir(dir: LongInt):LongInt;
    begin
      if not(dir <> 0) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'direction must be +1 or -1');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if dir > 0 then
        begin
          result := self.set_command('.+');
          exit;
        end;
      result := self.set_command('.-');
      exit;
    end;


  function TYStepperMotor.abortAndBrake():LongInt;
    begin
      result := self.set_command('B');
      exit;
    end;


  function TYStepperMotor.abortAndHiZ():LongInt;
    begin
      result := self.set_command('z');
      exit;
    end;


  function TYStepperMotor.nextStepperMotor(): TYStepperMotor;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextStepperMotor := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextStepperMotor := nil;
          exit;
        end;
      nextStepperMotor := TYStepperMotor.FindStepperMotor(hwid);
    end;

  class function TYStepperMotor.FirstStepperMotor(): TYStepperMotor;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('StepperMotor', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYStepperMotor.FindStepperMotor(serial+'.'+funcId);
    end;

//--- (end of YStepperMotor implementation)

//--- (YStepperMotor functions)

  function yFindStepperMotor(func:string): TYStepperMotor;
    begin
      result := TYStepperMotor.FindStepperMotor(func);
    end;

  function yFirstStepperMotor(): TYStepperMotor;
    begin
      result := TYStepperMotor.FirstStepperMotor();
    end;

  procedure _StepperMotorCleanup();
    begin
    end;

//--- (end of YStepperMotor functions)

initialization
  //--- (YStepperMotor initialization)
  //--- (end of YStepperMotor initialization)

finalization
  //--- (YStepperMotor cleanup)
  _StepperMotorCleanup();
  //--- (end of YStepperMotor cleanup)
end.
