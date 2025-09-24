{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindPwmInput(), the high-level API for PwmInput functions
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


unit yocto_pwminput;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YPwmInput definitions)

const Y_DUTYCYCLE_INVALID             = YAPI_INVALID_DOUBLE;
const Y_PULSEDURATION_INVALID         = YAPI_INVALID_DOUBLE;
const Y_FREQUENCY_INVALID             = YAPI_INVALID_DOUBLE;
const Y_PERIOD_INVALID                = YAPI_INVALID_DOUBLE;
const Y_PULSECOUNTER_INVALID          = YAPI_INVALID_LONG;
const Y_PULSETIMER_INVALID            = YAPI_INVALID_LONG;
const Y_PWMREPORTMODE_PWM_DUTYCYCLE = 0;
const Y_PWMREPORTMODE_PWM_FREQUENCY = 1;
const Y_PWMREPORTMODE_PWM_PULSEDURATION = 2;
const Y_PWMREPORTMODE_PWM_EDGECOUNT = 3;
const Y_PWMREPORTMODE_PWM_PULSECOUNT = 4;
const Y_PWMREPORTMODE_PWM_CPS = 5;
const Y_PWMREPORTMODE_PWM_CPM = 6;
const Y_PWMREPORTMODE_PWM_STATE = 7;
const Y_PWMREPORTMODE_PWM_FREQ_CPS = 8;
const Y_PWMREPORTMODE_PWM_FREQ_CPM = 9;
const Y_PWMREPORTMODE_PWM_PERIODCOUNT = 10;
const Y_PWMREPORTMODE_INVALID = -1;
const Y_DEBOUNCEPERIOD_INVALID        = YAPI_INVALID_UINT;
const Y_MINFREQUENCY_INVALID          = YAPI_INVALID_DOUBLE;
const Y_BANDWIDTH_INVALID             = YAPI_INVALID_UINT;
const Y_EDGESPERPERIOD_INVALID        = YAPI_INVALID_UINT;

//--- (end of YPwmInput definitions)

//--- (YPwmInput yapiwrapper declaration)
//--- (end of YPwmInput yapiwrapper declaration)

type

  TYPwmInput = class;
  //--- (YPwmInput class start)
  TYPwmInputValueCallback = procedure(func: TYPwmInput; value:string);
  TYPwmInputTimedReportCallback = procedure(func: TYPwmInput; value:TYMeasure);

  ////
  /// <summary>
  ///   TYPwmInput Class: PWM input control interface, available for instance in the Yocto-PWM-Rx
  /// <para>
  ///   The <c>YPwmInput</c> class allows you to read and configure Yoctopuce PWM inputs.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  ///   This class adds the ability to configure the signal parameter used to transmit
  ///   information: the duty cycle, the frequency or the pulse width.
  /// </para>
  /// </summary>
  ///-
  TYPwmInput=class(TYSensor)
  //--- (end of YPwmInput class start)
  protected
  //--- (YPwmInput declaration)
    // Attributes (function value cache)
    _dutyCycle                : double;
    _pulseDuration            : double;
    _frequency                : double;
    _period                   : double;
    _pulseCounter             : int64;
    _pulseTimer               : int64;
    _pwmReportMode            : Integer;
    _debouncePeriod           : LongInt;
    _minFrequency             : double;
    _bandwidth                : LongInt;
    _edgesPerPeriod           : LongInt;
    _valueCallbackPwmInput    : TYPwmInputValueCallback;
    _timedReportCallbackPwmInput : TYPwmInputTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YPwmInput declaration)

  public
    //--- (YPwmInput accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the measuring unit for the measured quantity.
    /// <para>
    ///   That unit
    ///   is just a string which is automatically initialized each time
    ///   the measurement mode is changed. But is can be set to an
    ///   arbitrary value.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the measuring unit for the measured quantity
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_unit(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the PWM duty cycle, in per cents.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the PWM duty cycle, in per cents
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmInput.DUTYCYCLE_INVALID</c>.
    /// </para>
    ///-
    function get_dutyCycle():double;

    ////
    /// <summary>
    ///   Returns the PWM pulse length in milliseconds, as a floating point number.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the PWM pulse length in milliseconds, as a floating point number
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmInput.PULSEDURATION_INVALID</c>.
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
    ///   a floating point number corresponding to the PWM frequency in Hz
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmInput.FREQUENCY_INVALID</c>.
    /// </para>
    ///-
    function get_frequency():double;

    ////
    /// <summary>
    ///   Returns the PWM period in milliseconds.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the PWM period in milliseconds
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmInput.PERIOD_INVALID</c>.
    /// </para>
    ///-
    function get_period():double;

    ////
    /// <summary>
    ///   Returns the pulse counter value.
    /// <para>
    ///   Actually that
    ///   counter is incremented twice per period. That counter is
    ///   limited  to 1 billion.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the pulse counter value
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmInput.PULSECOUNTER_INVALID</c>.
    /// </para>
    ///-
    function get_pulseCounter():int64;

    function set_pulseCounter(newval:int64):integer;

    ////
    /// <summary>
    ///   Returns the timer of the pulses counter (ms).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the timer of the pulses counter (ms)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmInput.PULSETIMER_INVALID</c>.
    /// </para>
    ///-
    function get_pulseTimer():int64;

    ////
    /// <summary>
    ///   Returns the parameter (frequency/duty cycle, pulse width, edges count) returned by the get_currentValue function and callbacks.
    /// <para>
    ///   Attention
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YPwmInput.PWMREPORTMODE_PWM_DUTYCYCLE</c>, <c>YPwmInput.PWMREPORTMODE_PWM_FREQUENCY</c>,
    ///   <c>YPwmInput.PWMREPORTMODE_PWM_PULSEDURATION</c>, <c>YPwmInput.PWMREPORTMODE_PWM_EDGECOUNT</c>,
    ///   <c>YPwmInput.PWMREPORTMODE_PWM_PULSECOUNT</c>, <c>YPwmInput.PWMREPORTMODE_PWM_CPS</c>,
    ///   <c>YPwmInput.PWMREPORTMODE_PWM_CPM</c>, <c>YPwmInput.PWMREPORTMODE_PWM_STATE</c>,
    ///   <c>YPwmInput.PWMREPORTMODE_PWM_FREQ_CPS</c>, <c>YPwmInput.PWMREPORTMODE_PWM_FREQ_CPM</c> and
    ///   <c>YPwmInput.PWMREPORTMODE_PWM_PERIODCOUNT</c> corresponding to the parameter (frequency/duty
    ///   cycle, pulse width, edges count) returned by the get_currentValue function and callbacks
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmInput.PWMREPORTMODE_INVALID</c>.
    /// </para>
    ///-
    function get_pwmReportMode():Integer;

    ////
    /// <summary>
    ///   Changes the  parameter  type (frequency/duty cycle, pulse width, or edge count) returned by the get_currentValue function and callbacks.
    /// <para>
    ///   The edge count value is limited to the 6 lowest digits. For values greater than one million, use
    ///   get_pulseCounter().
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YPwmInput.PWMREPORTMODE_PWM_DUTYCYCLE</c>, <c>YPwmInput.PWMREPORTMODE_PWM_FREQUENCY</c>,
    ///   <c>YPwmInput.PWMREPORTMODE_PWM_PULSEDURATION</c>, <c>YPwmInput.PWMREPORTMODE_PWM_EDGECOUNT</c>,
    ///   <c>YPwmInput.PWMREPORTMODE_PWM_PULSECOUNT</c>, <c>YPwmInput.PWMREPORTMODE_PWM_CPS</c>,
    ///   <c>YPwmInput.PWMREPORTMODE_PWM_CPM</c>, <c>YPwmInput.PWMREPORTMODE_PWM_STATE</c>,
    ///   <c>YPwmInput.PWMREPORTMODE_PWM_FREQ_CPS</c>, <c>YPwmInput.PWMREPORTMODE_PWM_FREQ_CPM</c> and
    ///   <c>YPwmInput.PWMREPORTMODE_PWM_PERIODCOUNT</c> corresponding to the  parameter  type
    ///   (frequency/duty cycle, pulse width, or edge count) returned by the get_currentValue function and callbacks
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_pwmReportMode(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the shortest expected pulse duration, in ms.
    /// <para>
    ///   Any shorter pulse will be automatically ignored (debounce).
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the shortest expected pulse duration, in ms
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmInput.DEBOUNCEPERIOD_INVALID</c>.
    /// </para>
    ///-
    function get_debouncePeriod():LongInt;

    ////
    /// <summary>
    ///   Changes the shortest expected pulse duration, in ms.
    /// <para>
    ///   Any shorter pulse will be automatically ignored (debounce).
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the shortest expected pulse duration, in ms
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_debouncePeriod(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Changes the minimum detected frequency, in Hz.
    /// <para>
    ///   Slower signals will be consider as zero frequency.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the minimum detected frequency, in Hz
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_minFrequency(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the minimum detected frequency, in Hz.
    /// <para>
    ///   Slower signals will be consider as zero frequency.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the minimum detected frequency, in Hz
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmInput.MINFREQUENCY_INVALID</c>.
    /// </para>
    ///-
    function get_minFrequency():double;

    ////
    /// <summary>
    ///   Returns the input signal sampling rate, in kHz.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the input signal sampling rate, in kHz
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmInput.BANDWIDTH_INVALID</c>.
    /// </para>
    ///-
    function get_bandwidth():LongInt;

    ////
    /// <summary>
    ///   Changes the input signal sampling rate, measured in kHz.
    /// <para>
    ///   A lower sampling frequency can be used to hide hide-frequency bounce effects,
    ///   for instance on electromechanical contacts, but limits the measure resolution.
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the input signal sampling rate, measured in kHz
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_bandwidth(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the number of edges detected per preiod.
    /// <para>
    ///   For a clean PWM signal, this should be exactly two,
    ///   but in cas the signal is created by a mechanical contact with bounces, it can get higher.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of edges detected per preiod
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmInput.EDGESPERPERIOD_INVALID</c>.
    /// </para>
    ///-
    function get_edgesPerPeriod():LongInt;

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
    ///   Use the method <c>YPwmInput.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YPwmInput</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindPwmInput(func: string):TYPwmInput;

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
    function registerValueCallback(callback: TYPwmInputValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every periodic timed notification.
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
    ///   arguments: the function object of which the value has changed, and an <c>YMeasure</c> object describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerTimedReportCallback(callback: TYPwmInputTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;

    ////
    /// <summary>
    ///   Resets the periodicity detection algorithm.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function resetPeriodDetection():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Resets the pulse counter value as well as its timer.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function resetCounter():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of PWM inputs started using <c>yFirstPwmInput()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned PWM inputs order.
    ///   If you want to find a specific a PWM input, use <c>PwmInput.findPwmInput()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YPwmInput</c> object, corresponding to
    ///   a PWM input currently online, or a <c>NIL</c> pointer
    ///   if there are no more PWM inputs to enumerate.
    /// </returns>
    ///-
    function nextPwmInput():TYPwmInput;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstPwmInput():TYPwmInput;
  //--- (end of YPwmInput accessors declaration)
  end;

//--- (YPwmInput functions declaration)
  ////
  /// <summary>
  ///   Retrieves a PWM input for a given identifier.
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
  ///   This function does not require that the PWM input is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YPwmInput.isOnline()</c> to test if the PWM input is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a PWM input by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the PWM input, for instance
  ///   <c>YPWMRX01.pwmInput1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YPwmInput</c> object allowing you to drive the PWM input.
  /// </returns>
  ///-
  function yFindPwmInput(func:string):TYPwmInput;
  ////
  /// <summary>
  ///   Starts the enumeration of PWM inputs currently accessible.
  /// <para>
  ///   Use the method <c>YPwmInput.nextPwmInput()</c> to iterate on
  ///   next PWM inputs.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YPwmInput</c> object, corresponding to
  ///   the first PWM input currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstPwmInput():TYPwmInput;

//--- (end of YPwmInput functions declaration)

implementation

//--- (YPwmInput dlldef)
//--- (end of YPwmInput dlldef)

  constructor TYPwmInput.Create(func:string);
    begin
      inherited Create(func);
      _className := 'PwmInput';
      //--- (YPwmInput accessors initialization)
      _dutyCycle := Y_DUTYCYCLE_INVALID;
      _pulseDuration := Y_PULSEDURATION_INVALID;
      _frequency := Y_FREQUENCY_INVALID;
      _period := Y_PERIOD_INVALID;
      _pulseCounter := Y_PULSECOUNTER_INVALID;
      _pulseTimer := Y_PULSETIMER_INVALID;
      _pwmReportMode := Y_PWMREPORTMODE_INVALID;
      _debouncePeriod := Y_DEBOUNCEPERIOD_INVALID;
      _minFrequency := Y_MINFREQUENCY_INVALID;
      _bandwidth := Y_BANDWIDTH_INVALID;
      _edgesPerPeriod := Y_EDGESPERPERIOD_INVALID;
      _valueCallbackPwmInput := nil;
      _timedReportCallbackPwmInput := nil;
      //--- (end of YPwmInput accessors initialization)
    end;

//--- (YPwmInput yapiwrapper)
//--- (end of YPwmInput yapiwrapper)

//--- (YPwmInput implementation)
{$HINTS OFF}
  function TYPwmInput._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'dutyCycle') then
        begin
          _dutyCycle := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'pulseDuration') then
        begin
          _pulseDuration := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'frequency') then
        begin
          _frequency := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'period') then
        begin
          _period := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'pulseCounter') then
        begin
          _pulseCounter := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'pulseTimer') then
        begin
          _pulseTimer := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'pwmReportMode') then
        begin
          _pwmReportMode := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'debouncePeriod') then
        begin
          _debouncePeriod := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'minFrequency') then
        begin
          _minFrequency := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'bandwidth') then
        begin
          _bandwidth := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'edgesPerPeriod') then
        begin
          _edgesPerPeriod := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYPwmInput.set_unit(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('unit',rest_val);
    end;

  function TYPwmInput.get_dutyCycle():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DUTYCYCLE_INVALID;
              exit;
            end;
        end;
      res := self._dutyCycle;
      result := res;
      exit;
    end;


  function TYPwmInput.get_pulseDuration():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PULSEDURATION_INVALID;
              exit;
            end;
        end;
      res := self._pulseDuration;
      result := res;
      exit;
    end;


  function TYPwmInput.get_frequency():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_FREQUENCY_INVALID;
              exit;
            end;
        end;
      res := self._frequency;
      result := res;
      exit;
    end;


  function TYPwmInput.get_period():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PERIOD_INVALID;
              exit;
            end;
        end;
      res := self._period;
      result := res;
      exit;
    end;


  function TYPwmInput.get_pulseCounter():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PULSECOUNTER_INVALID;
              exit;
            end;
        end;
      res := self._pulseCounter;
      result := res;
      exit;
    end;


  function TYPwmInput.set_pulseCounter(newval:int64):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('pulseCounter',rest_val);
    end;

  function TYPwmInput.get_pulseTimer():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PULSETIMER_INVALID;
              exit;
            end;
        end;
      res := self._pulseTimer;
      result := res;
      exit;
    end;


  function TYPwmInput.get_pwmReportMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PWMREPORTMODE_INVALID;
              exit;
            end;
        end;
      res := self._pwmReportMode;
      result := res;
      exit;
    end;


  function TYPwmInput.set_pwmReportMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('pwmReportMode',rest_val);
    end;

  function TYPwmInput.get_debouncePeriod():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DEBOUNCEPERIOD_INVALID;
              exit;
            end;
        end;
      res := self._debouncePeriod;
      result := res;
      exit;
    end;


  function TYPwmInput.set_debouncePeriod(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('debouncePeriod',rest_val);
    end;

  function TYPwmInput.set_minFrequency(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('minFrequency',rest_val);
    end;

  function TYPwmInput.get_minFrequency():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MINFREQUENCY_INVALID;
              exit;
            end;
        end;
      res := self._minFrequency;
      result := res;
      exit;
    end;


  function TYPwmInput.get_bandwidth():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BANDWIDTH_INVALID;
              exit;
            end;
        end;
      res := self._bandwidth;
      result := res;
      exit;
    end;


  function TYPwmInput.set_bandwidth(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('bandwidth',rest_val);
    end;

  function TYPwmInput.get_edgesPerPeriod():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_EDGESPERPERIOD_INVALID;
              exit;
            end;
        end;
      res := self._edgesPerPeriod;
      result := res;
      exit;
    end;


  class function TYPwmInput.FindPwmInput(func: string):TYPwmInput;
    var
      obj : TYPwmInput;
    begin
      obj := TYPwmInput(TYFunction._FindFromCache('PwmInput', func));
      if (obj = nil) then
        begin
          obj :=  TYPwmInput.create(func);
          TYFunction._AddToCache('PwmInput', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYPwmInput.registerValueCallback(callback: TYPwmInputValueCallback):LongInt;
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
      self._valueCallbackPwmInput := callback;
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


  function TYPwmInput._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackPwmInput) <> nil) then
        begin
          self._valueCallbackPwmInput(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYPwmInput.registerTimedReportCallback(callback: TYPwmInputTimedReportCallback):LongInt;
    var
      sensor : TYSensor;
    begin
      sensor := self;
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(sensor, true);
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(sensor, false);
        end;
      self._timedReportCallbackPwmInput := callback;
      result := 0;
      exit;
    end;


  function TYPwmInput._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackPwmInput) <> nil) then
        begin
          self._timedReportCallbackPwmInput(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYPwmInput.resetPeriodDetection():LongInt;
    begin
      result := self.set_bandwidth(self.get_bandwidth);
      exit;
    end;


  function TYPwmInput.resetCounter():LongInt;
    begin
      result := self.set_pulseCounter(0);
      exit;
    end;


  function TYPwmInput.nextPwmInput(): TYPwmInput;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextPwmInput := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextPwmInput := nil;
          exit;
        end;
      nextPwmInput := TYPwmInput.FindPwmInput(hwid);
    end;

  class function TYPwmInput.FirstPwmInput(): TYPwmInput;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('PwmInput', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYPwmInput.FindPwmInput(serial+'.'+funcId);
    end;

//--- (end of YPwmInput implementation)

//--- (YPwmInput functions)

  function yFindPwmInput(func:string): TYPwmInput;
    begin
      result := TYPwmInput.FindPwmInput(func);
    end;

  function yFirstPwmInput(): TYPwmInput;
    begin
      result := TYPwmInput.FirstPwmInput();
    end;

  procedure _PwmInputCleanup();
    begin
    end;

//--- (end of YPwmInput functions)

initialization
  //--- (YPwmInput initialization)
  //--- (end of YPwmInput initialization)

finalization
  //--- (YPwmInput cleanup)
  _PwmInputCleanup();
  //--- (end of YPwmInput cleanup)

end.
