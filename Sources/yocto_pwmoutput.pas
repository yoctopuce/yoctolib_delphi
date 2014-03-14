{*********************************************************************
 *
 * $Id: pic24config.php 15411 2014-03-13 12:08:37Z mvuilleu $
 *
 * Implements yFindPwmOutput(), the high-level API for PwmOutput functions
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


unit yocto_pwmoutput;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YPwmOutput definitions)

const Y_ENABLED_FALSE = 0;
const Y_ENABLED_TRUE = 1;
const Y_ENABLED_INVALID = -1;

const Y_DUTYCYCLE_INVALID             = YAPI_INVALID_DOUBLE;
const Y_PULSEDURATION_INVALID         = YAPI_INVALID_DOUBLE;
const Y_FREQUENCY_INVALID             = YAPI_INVALID_UINT;
const Y_PERIOD_INVALID                = YAPI_INVALID_DOUBLE;
const Y_PWMTRANSITION_INVALID         = YAPI_INVALID_STRING;
const Y_ENABLEDATPOWERON_FALSE = 0;
const Y_ENABLEDATPOWERON_TRUE = 1;
const Y_ENABLEDATPOWERON_INVALID = -1;

const Y_DUTYCYCLEATPOWERON_INVALID    = YAPI_INVALID_DOUBLE;


//--- (end of YPwmOutput definitions)

type
  TYPwmOutput = class;
  //--- (YPwmOutput class start)
  TYPwmOutputValueCallback = procedure(func: TYPwmOutput; value:string);
  TYPwmOutputTimedReportCallback = procedure(func: TYPwmOutput; value:TYMeasure);

  ////
  /// <summary>
  ///   TYPwmOutput Class: Pwm function interface
  /// <para>
  ///   The Yoctopuce application programming interface allows you to configure, start, and stop the PWM.
  /// </para>
  /// </summary>
  ///-
  TYPwmOutput=class(TYFunction)
  //--- (end of YPwmOutput class start)
  protected
  //--- (YPwmOutput declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _enabled                  : Integer;
    _dutyCycle                : double;
    _pulseDuration            : double;
    _frequency                : LongInt;
    _period                   : double;
    _pwmTransition            : string;
    _enabledAtPowerOn         : Integer;
    _dutyCycleAtPowerOn       : double;
    _valueCallbackPwmOutput   : TYPwmOutputValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YPwmOutput declaration)

  public
    //--- (YPwmOutput accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the state of the PWMs.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_ENABLED_FALSE</c> or <c>Y_ENABLED_TRUE</c>, according to the state of the PWMs
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ENABLED_INVALID</c>.
    /// </para>
    ///-
    function get_enabled():Integer;

    ////
    /// <summary>
    ///   Stops or starts the PWM.
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
    ///   Configures the PWMs duty cyle.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number
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
    function set_dutyCycle(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the PWMs dutty cyle as a floating point number between 0 an 1.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the PWMs dutty cyle as a floating point number between 0 an 1
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DUTYCYCLE_INVALID</c>.
    /// </para>
    ///-
    function get_dutyCycle():double;

    ////
    /// <summary>
    ///   Configures the PWM pluses length.
    /// <para>
    ///   A pulse length cannot be longer than period, otherwise it is truncated.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number
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
    function set_pulseDuration(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the PWM pulse length in milliseconds.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the PWM pulse length in milliseconds
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PULSEDURATION_INVALID</c>.
    /// </para>
    ///-
    function get_pulseDuration():double;

    ////
    /// <summary>
    ///   Returns the PWM frequency in Hz.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the PWM frequency in Hz
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_FREQUENCY_INVALID</c>.
    /// </para>
    ///-
    function get_frequency():LongInt;

    ////
    /// <summary>
    ///   Configures the PWM frequency.
    /// <para>
    ///   The duty cycle is kept unchanged thanks to an
    ///   automatic pulse width change.
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
    function set_frequency(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Configures the PWM period.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number
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
    function set_period(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the PWM period in nonaseconde.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the PWM period in nonaseconde
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PERIOD_INVALID</c>.
    /// </para>
    ///-
    function get_period():double;

    function get_pwmTransition():string;

    function set_pwmTransition(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the state of the PWMs at device power on.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_ENABLEDATPOWERON_FALSE</c> or <c>Y_ENABLEDATPOWERON_TRUE</c>, according to the state of
    ///   the PWMs at device power on
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ENABLEDATPOWERON_INVALID</c>.
    /// </para>
    ///-
    function get_enabledAtPowerOn():Integer;

    ////
    /// <summary>
    ///   Configures the state of PWM at device power on.
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
    ///   Configures the PWMs duty cycle at device power on.
    /// <para>
    ///   Remember to call the matching
    ///   module <c>saveToFlash()</c> method, otherwise this call will have no effect.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number
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
    function set_dutyCycleAtPowerOn(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the PWMs duty cycle at device power on as a floating point number between 0.0 and 100.
    /// <para>
    ///   0%
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the PWMs duty cycle at device power on as a floating point
    ///   number between 0.0 and 100
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DUTYCYCLEATPOWERON_INVALID</c>.
    /// </para>
    ///-
    function get_dutyCycleAtPowerOn():double;

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
    ///   Use the method <c>YPwmOutput.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YPwmOutput</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindPwmOutput(func: string):TYPwmOutput;

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
    function registerValueCallback(callback: TYPwmOutputValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Performs a smooth change of the pulse duration toward a given value.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="ms_target">
    ///   new pulse duration at the end of the transition
    ///   (floating-point number, representing the pulse duration in milliseconds)
    /// </param>
    /// <param name="ms_duration">
    ///   total duration of the transition, in milliseconds
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function pulseDurationMove(ms_target: double; ms_duration: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Performs a smooth change of the pulse duration toward a given value.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="target">
    ///   new duty cycle at the end of the transition
    ///   (floating-point number, between 0 and 1)
    /// </param>
    /// <param name="ms_duration">
    ///   total duration of the transition, in milliseconds
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function dutyCycleMove(target: double; ms_duration: LongInt):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of PWMs started using <c>yFirstPwmOutput()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YPwmOutput</c> object, corresponding to
    ///   a PWM currently online, or a <c>null</c> pointer
    ///   if there are no more PWMs to enumerate.
    /// </returns>
    ///-
    function nextPwmOutput():TYPwmOutput;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstPwmOutput():TYPwmOutput;
  //--- (end of YPwmOutput accessors declaration)
  end;

//--- (PwmOutput functions declaration)

  ////
  /// <summary>
  ///   Retrieves a PWM for a given identifier.
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
  ///   This function does not require that the PWM is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YPwmOutput.isOnline()</c> to test if the PWM is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a PWM by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the PWM
  /// </param>
  /// <returns>
  ///   a <c>YPwmOutput</c> object allowing you to drive the PWM.
  /// </returns>
  ///-
  function yFindPwmOutput(func:string):TYPwmOutput;
  ////
  /// <summary>
  ///   Starts the enumeration of PWMs currently accessible.
  /// <para>
  ///   Use the method <c>YPwmOutput.nextPwmOutput()</c> to iterate on
  ///   next PWMs.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YPwmOutput</c> object, corresponding to
  ///   the first PWM currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstPwmOutput():TYPwmOutput;

//--- (end of PwmOutput functions declaration)

implementation

  constructor TYPwmOutput.Create(func:string);
    begin
      inherited Create(func);
      _className := 'PwmOutput';
      //--- (YPwmOutput accessors initialization)
      _enabled := Y_ENABLED_INVALID;
      _dutyCycle := Y_DUTYCYCLE_INVALID;
      _pulseDuration := Y_PULSEDURATION_INVALID;
      _frequency := Y_FREQUENCY_INVALID;
      _period := Y_PERIOD_INVALID;
      _pwmTransition := Y_PWMTRANSITION_INVALID;
      _enabledAtPowerOn := Y_ENABLEDATPOWERON_INVALID;
      _dutyCycleAtPowerOn := Y_DUTYCYCLEATPOWERON_INVALID;
      _valueCallbackPwmOutput := nil;
      //--- (end of YPwmOutput accessors initialization)
    end;


//--- (YPwmOutput implementation)
{$HINTS OFF}
  function TYPwmOutput._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'enabled') then
        begin
          _enabled := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'dutyCycle') then
        begin
          _dutyCycle := member^.ivalue/65536.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'pulseDuration') then
        begin
          _pulseDuration := member^.ivalue/65536.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'frequency') then
        begin
          _frequency := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'period') then
        begin
          _period := member^.ivalue/65536.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'pwmTransition') then
        begin
          _pwmTransition := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'enabledAtPowerOn') then
        begin
          _enabledAtPowerOn := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'dutyCycleAtPowerOn') then
        begin
          _dutyCycleAtPowerOn := member^.ivalue/65536.0;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Returns the state of the PWMs.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_ENABLED_FALSE or Y_ENABLED_TRUE, according to the state of the PWMs
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ENABLED_INVALID.
  /// </para>
  ///-
  function TYPwmOutput.get_enabled():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ENABLED_INVALID;
              exit
            end;
        end;
      result := self._enabled;
      exit;
    end;


  ////
  /// <summary>
  ///   Stops or starts the PWM.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   either Y_ENABLED_FALSE or Y_ENABLED_TRUE
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
  function TYPwmOutput.set_enabled(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('enabled',rest_val);
    end;

  ////
  /// <summary>
  ///   Configures the PWMs duty cyle.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a floating point number
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
  function TYPwmOutput.set_dutyCycle(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval*65536.0));
      result := _setAttr('dutyCycle',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the PWMs dutty cyle as a floating point number between 0 an 1.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the PWMs dutty cyle as a floating point number between 0 an 1
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DUTYCYCLE_INVALID.
  /// </para>
  ///-
  function TYPwmOutput.get_dutyCycle():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_DUTYCYCLE_INVALID;
              exit
            end;
        end;
      result := self._dutyCycle;
      exit;
    end;


  ////
  /// <summary>
  ///   Configures the PWM pluses length.
  /// <para>
  ///   A pulse length cannot be longer than period, otherwise it is truncated.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a floating point number
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
  function TYPwmOutput.set_pulseDuration(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval*65536.0));
      result := _setAttr('pulseDuration',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the PWM pulse length in milliseconds.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the PWM pulse length in milliseconds
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PULSEDURATION_INVALID.
  /// </para>
  ///-
  function TYPwmOutput.get_pulseDuration():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PULSEDURATION_INVALID;
              exit
            end;
        end;
      result := self._pulseDuration;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the PWM frequency in Hz.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the PWM frequency in Hz
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_FREQUENCY_INVALID.
  /// </para>
  ///-
  function TYPwmOutput.get_frequency():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_FREQUENCY_INVALID;
              exit
            end;
        end;
      result := self._frequency;
      exit;
    end;


  ////
  /// <summary>
  ///   Configures the PWM frequency.
  /// <para>
  ///   The duty cycle is kept unchanged thanks to an
  ///   automatic pulse width change.
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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYPwmOutput.set_frequency(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('frequency',rest_val);
    end;

  ////
  /// <summary>
  ///   Configures the PWM period.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a floating point number
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
  function TYPwmOutput.set_period(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval*65536.0));
      result := _setAttr('period',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the PWM period in nonaseconde.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the PWM period in nonaseconde
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PERIOD_INVALID.
  /// </para>
  ///-
  function TYPwmOutput.get_period():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PERIOD_INVALID;
              exit
            end;
        end;
      result := self._period;
      exit;
    end;


  function TYPwmOutput.get_pwmTransition():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PWMTRANSITION_INVALID;
              exit
            end;
        end;
      result := self._pwmTransition;
      exit;
    end;


  function TYPwmOutput.set_pwmTransition(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('pwmTransition',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the state of the PWMs at device power on.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_ENABLEDATPOWERON_FALSE or Y_ENABLEDATPOWERON_TRUE, according to the state of the PWMs at
  ///   device power on
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ENABLEDATPOWERON_INVALID.
  /// </para>
  ///-
  function TYPwmOutput.get_enabledAtPowerOn():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ENABLEDATPOWERON_INVALID;
              exit
            end;
        end;
      result := self._enabledAtPowerOn;
      exit;
    end;


  ////
  /// <summary>
  ///   Configures the state of PWM at device power on.
  /// <para>
  ///   Remember to call the matching module saveToFlash()
  ///   method, otherwise this call will have no effect.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   either Y_ENABLEDATPOWERON_FALSE or Y_ENABLEDATPOWERON_TRUE
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
  function TYPwmOutput.set_enabledAtPowerOn(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('enabledAtPowerOn',rest_val);
    end;

  ////
  /// <summary>
  ///   Configures the PWMs duty cycle at device power on.
  /// <para>
  ///   Remember to call the matching
  ///   module saveToFlash() method, otherwise this call will have no effect.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a floating point number
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
  function TYPwmOutput.set_dutyCycleAtPowerOn(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval*65536.0));
      result := _setAttr('dutyCycleAtPowerOn',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the PWMs duty cycle at device power on as a floating point number between 0.0 and 100.
  /// <para>
  ///   0%
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the PWMs duty cycle at device power on as a floating point
  ///   number between 0.0 and 100
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DUTYCYCLEATPOWERON_INVALID.
  /// </para>
  ///-
  function TYPwmOutput.get_dutyCycleAtPowerOn():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_DUTYCYCLEATPOWERON_INVALID;
              exit
            end;
        end;
      result := self._dutyCycleAtPowerOn;
      exit;
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
  ///   Use the method <c>YPwmOutput.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YPwmOutput</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYPwmOutput.FindPwmOutput(func: string):TYPwmOutput;
    var
      obj : TYPwmOutput;
    begin
      obj := TYPwmOutput(TYFunction._FindFromCache('PwmOutput', func));
      if obj = nil then
        begin
          obj :=  TYPwmOutput.create(func);
          TYFunction._AddToCache('PwmOutput',  func, obj)
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
  function TYPwmOutput.registerValueCallback(callback: TYPwmOutputValueCallback):LongInt;
    var
      val : string;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateValueCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateValueCallbackList(self, false)
        end;
      self._valueCallbackPwmOutput := callback;
      // Immediately invoke value callback with current value
      if (addr(callback) <> nil) and self.isOnline then
        begin
          val := self._advertisedValue;
          if not((val = '')) then
            begin
              self._invokeValueCallback(val)
            end;
        end;
      result := 0;
      exit;
    end;


  function TYPwmOutput._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackPwmOutput) <> nil) then
        begin
          self._valueCallbackPwmOutput(self, value)
        end
      else
        begin
          inherited _invokeValueCallback(value)
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Performs a smooth change of the pulse duration toward a given value.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="ms_target">
  ///   new pulse duration at the end of the transition
  ///   (floating-point number, representing the pulse duration in milliseconds)
  /// </param>
  /// <param name="ms_duration">
  ///   total duration of the transition, in milliseconds
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYPwmOutput.pulseDurationMove(ms_target: double; ms_duration: LongInt):LongInt;
    var
      newval : string;
    begin
      if ms_target < 0.0 then
        begin
          ms_target := 0.0
        end;
      newval := ''+inttostr( round(ms_target*65536))+'ms:'+inttostr(ms_duration);
      result := self.set_pwmTransition(newval);
      exit;
    end;


  ////
  /// <summary>
  ///   Performs a smooth change of the pulse duration toward a given value.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="target">
  ///   new duty cycle at the end of the transition
  ///   (floating-point number, between 0 and 1)
  /// </param>
  /// <param name="ms_duration">
  ///   total duration of the transition, in milliseconds
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYPwmOutput.dutyCycleMove(target: double; ms_duration: LongInt):LongInt;
    var
      newval : string;
    begin
      if target < 0.0 then
        begin
          target := 0.0
        end;
      if target > 100.0 then
        begin
          target := 100.0
        end;
      newval := ''+inttostr( round(target*65536))+':'+inttostr(ms_duration);
      result := self.set_pwmTransition(newval);
      exit;
    end;


  function TYPwmOutput.nextPwmOutput(): TYPwmOutput;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextPwmOutput := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextPwmOutput := nil;
          exit;
        end;
      nextPwmOutput := TYPwmOutput.FindPwmOutput(hwid);
    end;

  class function TYPwmOutput.FirstPwmOutput(): TYPwmOutput;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('PwmOutput', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYPwmOutput.FindPwmOutput(serial+'.'+funcId);
    end;

//--- (end of YPwmOutput implementation)

//--- (PwmOutput functions)

  function yFindPwmOutput(func:string): TYPwmOutput;
    begin
      result := TYPwmOutput.FindPwmOutput(func);
    end;

  function yFirstPwmOutput(): TYPwmOutput;
    begin
      result := TYPwmOutput.FirstPwmOutput();
    end;

  procedure _PwmOutputCleanup();
    begin
    end;

//--- (end of PwmOutput functions)

initialization
  //--- (PwmOutput initialization)
  //--- (end of PwmOutput initialization)

finalization
  //--- (PwmOutput cleanup)
  _PwmOutputCleanup();
  //--- (end of PwmOutput cleanup)
end.
