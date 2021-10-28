{*********************************************************************
 *
 *  $Id: yocto_anbutton.pas 46894 2021-10-25 15:07:44Z seb $
 *
 *  Implements yFindAnButton(), the high-level API for AnButton functions
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


unit yocto_anbutton;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YAnButton definitions)

const Y_CALIBRATEDVALUE_INVALID       = YAPI_INVALID_UINT;
const Y_RAWVALUE_INVALID              = YAPI_INVALID_UINT;
const Y_ANALOGCALIBRATION_OFF = 0;
const Y_ANALOGCALIBRATION_ON = 1;
const Y_ANALOGCALIBRATION_INVALID = -1;
const Y_CALIBRATIONMAX_INVALID        = YAPI_INVALID_UINT;
const Y_CALIBRATIONMIN_INVALID        = YAPI_INVALID_UINT;
const Y_SENSITIVITY_INVALID           = YAPI_INVALID_UINT;
const Y_ISPRESSED_FALSE = 0;
const Y_ISPRESSED_TRUE = 1;
const Y_ISPRESSED_INVALID = -1;
const Y_LASTTIMEPRESSED_INVALID       = YAPI_INVALID_LONG;
const Y_LASTTIMERELEASED_INVALID      = YAPI_INVALID_LONG;
const Y_PULSECOUNTER_INVALID          = YAPI_INVALID_LONG;
const Y_PULSETIMER_INVALID            = YAPI_INVALID_LONG;
const Y_INPUTTYPE_ANALOG_FAST = 0;
const Y_INPUTTYPE_DIGITAL4 = 1;
const Y_INPUTTYPE_ANALOG_SMOOTH = 2;
const Y_INPUTTYPE_INVALID = -1;


//--- (end of YAnButton definitions)
//--- (YAnButton yapiwrapper declaration)
//--- (end of YAnButton yapiwrapper declaration)

type
  TYAnButton = class;
  //--- (YAnButton class start)
  TYAnButtonValueCallback = procedure(func: TYAnButton; value:string);
  TYAnButtonTimedReportCallback = procedure(func: TYAnButton; value:TYMeasure);

  ////
  /// <summary>
  ///   TYAnButton Class: analog input control interface, available for instance in the Yocto-Buzzer, the
  ///   Yocto-Knob, the Yocto-MaxiBuzzer or the Yocto-MaxiDisplay
  /// <para>
  ///   The <c>YAnButton</c> class provide access to basic resistive inputs.
  ///   Such inputs can be used to measure the state
  ///   of a simple button as well as to read an analog potentiometer (variable resistance).
  ///   This can be use for instance with a continuous rotating knob, a throttle grip
  ///   or a joystick. The module is capable to calibrate itself on min and max values,
  ///   in order to compute a calibrated value that varies proportionally with the
  ///   potentiometer position, regardless of its total resistance.
  /// </para>
  /// </summary>
  ///-
  TYAnButton=class(TYFunction)
  //--- (end of YAnButton class start)
  protected
  //--- (YAnButton declaration)
    // Attributes (function value cache)
    _calibratedValue          : LongInt;
    _rawValue                 : LongInt;
    _analogCalibration        : Integer;
    _calibrationMax           : LongInt;
    _calibrationMin           : LongInt;
    _sensitivity              : LongInt;
    _isPressed                : Integer;
    _lastTimePressed          : int64;
    _lastTimeReleased         : int64;
    _pulseCounter             : int64;
    _pulseTimer               : int64;
    _inputType                : Integer;
    _valueCallbackAnButton    : TYAnButtonValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YAnButton declaration)

  public
    //--- (YAnButton accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the current calibrated input value (between 0 and 1000, included).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current calibrated input value (between 0 and 1000, included)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAnButton.CALIBRATEDVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_calibratedValue():LongInt;

    ////
    /// <summary>
    ///   Returns the current measured input value as-is (between 0 and 4095, included).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current measured input value as-is (between 0 and 4095, included)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAnButton.RAWVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_rawValue():LongInt;

    ////
    /// <summary>
    ///   Tells if a calibration process is currently ongoing.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YAnButton.ANALOGCALIBRATION_OFF</c> or <c>YAnButton.ANALOGCALIBRATION_ON</c>
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAnButton.ANALOGCALIBRATION_INVALID</c>.
    /// </para>
    ///-
    function get_analogCalibration():Integer;

    ////
    /// <summary>
    ///   Starts or stops the calibration process.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module at the end of the calibration if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YAnButton.ANALOGCALIBRATION_OFF</c> or <c>YAnButton.ANALOGCALIBRATION_ON</c>
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
    function set_analogCalibration(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the maximal value measured during the calibration (between 0 and 4095, included).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximal value measured during the calibration (between 0 and 4095, included)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAnButton.CALIBRATIONMAX_INVALID</c>.
    /// </para>
    ///-
    function get_calibrationMax():LongInt;

    ////
    /// <summary>
    ///   Changes the maximal calibration value for the input (between 0 and 4095, included), without actually
    ///   starting the automated calibration.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the maximal calibration value for the input (between 0 and 4095,
    ///   included), without actually
    ///   starting the automated calibration
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
    function set_calibrationMax(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the minimal value measured during the calibration (between 0 and 4095, included).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the minimal value measured during the calibration (between 0 and 4095, included)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAnButton.CALIBRATIONMIN_INVALID</c>.
    /// </para>
    ///-
    function get_calibrationMin():LongInt;

    ////
    /// <summary>
    ///   Changes the minimal calibration value for the input (between 0 and 4095, included), without actually
    ///   starting the automated calibration.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the minimal calibration value for the input (between 0 and 4095,
    ///   included), without actually
    ///   starting the automated calibration
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
    function set_calibrationMin(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the sensibility for the input (between 1 and 1000) for triggering user callbacks.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the sensibility for the input (between 1 and 1000) for triggering user callbacks
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAnButton.SENSITIVITY_INVALID</c>.
    /// </para>
    ///-
    function get_sensitivity():LongInt;

    ////
    /// <summary>
    ///   Changes the sensibility for the input (between 1 and 1000) for triggering user callbacks.
    /// <para>
    ///   The sensibility is used to filter variations around a fixed value, but does not preclude the
    ///   transmission of events when the input value evolves constantly in the same direction.
    ///   Special case: when the value 1000 is used, the callback will only be thrown when the logical state
    ///   of the input switches from pressed to released and back.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the sensibility for the input (between 1 and 1000) for triggering user callbacks
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
    function set_sensitivity(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns true if the input (considered as binary) is active (closed contact), and false otherwise.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YAnButton.ISPRESSED_FALSE</c> or <c>YAnButton.ISPRESSED_TRUE</c>, according to true if
    ///   the input (considered as binary) is active (closed contact), and false otherwise
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAnButton.ISPRESSED_INVALID</c>.
    /// </para>
    ///-
    function get_isPressed():Integer;

    ////
    /// <summary>
    ///   Returns the number of elapsed milliseconds between the module power on and the last time
    ///   the input button was pressed (the input contact transitioned from open to closed).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of elapsed milliseconds between the module power on and the last time
    ///   the input button was pressed (the input contact transitioned from open to closed)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAnButton.LASTTIMEPRESSED_INVALID</c>.
    /// </para>
    ///-
    function get_lastTimePressed():int64;

    ////
    /// <summary>
    ///   Returns the number of elapsed milliseconds between the module power on and the last time
    ///   the input button was released (the input contact transitioned from closed to open).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of elapsed milliseconds between the module power on and the last time
    ///   the input button was released (the input contact transitioned from closed to open)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAnButton.LASTTIMERELEASED_INVALID</c>.
    /// </para>
    ///-
    function get_lastTimeReleased():int64;

    ////
    /// <summary>
    ///   Returns the pulse counter value.
    /// <para>
    ///   The value is a 32 bit integer. In case
    ///   of overflow (>=2^32), the counter will wrap. To reset the counter, just
    ///   call the resetCounter() method.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the pulse counter value
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAnButton.PULSECOUNTER_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YAnButton.PULSETIMER_INVALID</c>.
    /// </para>
    ///-
    function get_pulseTimer():int64;

    ////
    /// <summary>
    ///   Returns the decoding method applied to the input (analog or multiplexed binary switches).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YAnButton.INPUTTYPE_ANALOG_FAST</c>, <c>YAnButton.INPUTTYPE_DIGITAL4</c> and
    ///   <c>YAnButton.INPUTTYPE_ANALOG_SMOOTH</c> corresponding to the decoding method applied to the input
    ///   (analog or multiplexed binary switches)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YAnButton.INPUTTYPE_INVALID</c>.
    /// </para>
    ///-
    function get_inputType():Integer;

    ////
    /// <summary>
    ///   Changes the decoding method applied to the input (analog or multiplexed binary switches).
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YAnButton.INPUTTYPE_ANALOG_FAST</c>, <c>YAnButton.INPUTTYPE_DIGITAL4</c> and
    ///   <c>YAnButton.INPUTTYPE_ANALOG_SMOOTH</c> corresponding to the decoding method applied to the input
    ///   (analog or multiplexed binary switches)
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
    function set_inputType(newval:Integer):integer;

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
    ///   Use the method <c>YAnButton.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YAnButton</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindAnButton(func: string):TYAnButton;

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
    function registerValueCallback(callback: TYAnButtonValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Returns the pulse counter value as well as its timer.
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
    ///   Continues the enumeration of analog inputs started using <c>yFirstAnButton()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned analog inputs order.
    ///   If you want to find a specific an analog input, use <c>AnButton.findAnButton()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YAnButton</c> object, corresponding to
    ///   an analog input currently online, or a <c>NIL</c> pointer
    ///   if there are no more analog inputs to enumerate.
    /// </returns>
    ///-
    function nextAnButton():TYAnButton;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstAnButton():TYAnButton;
  //--- (end of YAnButton accessors declaration)
  end;

//--- (YAnButton functions declaration)
  ////
  /// <summary>
  ///   Retrieves an analog input for a given identifier.
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
  ///   This function does not require that the analog input is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YAnButton.isOnline()</c> to test if the analog input is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   an analog input by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the analog input, for instance
  ///   <c>YBUZZER2.anButton1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YAnButton</c> object allowing you to drive the analog input.
  /// </returns>
  ///-
  function yFindAnButton(func:string):TYAnButton;
  ////
  /// <summary>
  ///   Starts the enumeration of analog inputs currently accessible.
  /// <para>
  ///   Use the method <c>YAnButton.nextAnButton()</c> to iterate on
  ///   next analog inputs.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YAnButton</c> object, corresponding to
  ///   the first analog input currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstAnButton():TYAnButton;

//--- (end of YAnButton functions declaration)

implementation
//--- (YAnButton dlldef)
//--- (end of YAnButton dlldef)

  constructor TYAnButton.Create(func:string);
    begin
      inherited Create(func);
      _className := 'AnButton';
      //--- (YAnButton accessors initialization)
      _calibratedValue := Y_CALIBRATEDVALUE_INVALID;
      _rawValue := Y_RAWVALUE_INVALID;
      _analogCalibration := Y_ANALOGCALIBRATION_INVALID;
      _calibrationMax := Y_CALIBRATIONMAX_INVALID;
      _calibrationMin := Y_CALIBRATIONMIN_INVALID;
      _sensitivity := Y_SENSITIVITY_INVALID;
      _isPressed := Y_ISPRESSED_INVALID;
      _lastTimePressed := Y_LASTTIMEPRESSED_INVALID;
      _lastTimeReleased := Y_LASTTIMERELEASED_INVALID;
      _pulseCounter := Y_PULSECOUNTER_INVALID;
      _pulseTimer := Y_PULSETIMER_INVALID;
      _inputType := Y_INPUTTYPE_INVALID;
      _valueCallbackAnButton := nil;
      //--- (end of YAnButton accessors initialization)
    end;

//--- (YAnButton yapiwrapper)
//--- (end of YAnButton yapiwrapper)

//--- (YAnButton implementation)
{$HINTS OFF}
  function TYAnButton._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'calibratedValue') then
        begin
          _calibratedValue := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'rawValue') then
        begin
          _rawValue := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'analogCalibration') then
        begin
          _analogCalibration := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'calibrationMax') then
        begin
          _calibrationMax := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'calibrationMin') then
        begin
          _calibrationMin := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'sensitivity') then
        begin
          _sensitivity := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'isPressed') then
        begin
          _isPressed := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'lastTimePressed') then
        begin
          _lastTimePressed := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'lastTimeReleased') then
        begin
          _lastTimeReleased := member^.ivalue;
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
      if (member^.name = 'inputType') then
        begin
          _inputType := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYAnButton.get_calibratedValue():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CALIBRATEDVALUE_INVALID;
              exit;
            end;
        end;
      res := self._calibratedValue;
      result := res;
      exit;
    end;


  function TYAnButton.get_rawValue():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RAWVALUE_INVALID;
              exit;
            end;
        end;
      res := self._rawValue;
      result := res;
      exit;
    end;


  function TYAnButton.get_analogCalibration():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ANALOGCALIBRATION_INVALID;
              exit;
            end;
        end;
      res := self._analogCalibration;
      result := res;
      exit;
    end;


  function TYAnButton.set_analogCalibration(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('analogCalibration',rest_val);
    end;

  function TYAnButton.get_calibrationMax():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CALIBRATIONMAX_INVALID;
              exit;
            end;
        end;
      res := self._calibrationMax;
      result := res;
      exit;
    end;


  function TYAnButton.set_calibrationMax(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('calibrationMax',rest_val);
    end;

  function TYAnButton.get_calibrationMin():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CALIBRATIONMIN_INVALID;
              exit;
            end;
        end;
      res := self._calibrationMin;
      result := res;
      exit;
    end;


  function TYAnButton.set_calibrationMin(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('calibrationMin',rest_val);
    end;

  function TYAnButton.get_sensitivity():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SENSITIVITY_INVALID;
              exit;
            end;
        end;
      res := self._sensitivity;
      result := res;
      exit;
    end;


  function TYAnButton.set_sensitivity(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('sensitivity',rest_val);
    end;

  function TYAnButton.get_isPressed():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ISPRESSED_INVALID;
              exit;
            end;
        end;
      res := self._isPressed;
      result := res;
      exit;
    end;


  function TYAnButton.get_lastTimePressed():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LASTTIMEPRESSED_INVALID;
              exit;
            end;
        end;
      res := self._lastTimePressed;
      result := res;
      exit;
    end;


  function TYAnButton.get_lastTimeReleased():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LASTTIMERELEASED_INVALID;
              exit;
            end;
        end;
      res := self._lastTimeReleased;
      result := res;
      exit;
    end;


  function TYAnButton.get_pulseCounter():int64;
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


  function TYAnButton.set_pulseCounter(newval:int64):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('pulseCounter',rest_val);
    end;

  function TYAnButton.get_pulseTimer():int64;
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


  function TYAnButton.get_inputType():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_INPUTTYPE_INVALID;
              exit;
            end;
        end;
      res := self._inputType;
      result := res;
      exit;
    end;


  function TYAnButton.set_inputType(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('inputType',rest_val);
    end;

  class function TYAnButton.FindAnButton(func: string):TYAnButton;
    var
      obj : TYAnButton;
    begin
      obj := TYAnButton(TYFunction._FindFromCache('AnButton', func));
      if obj = nil then
        begin
          obj :=  TYAnButton.create(func);
          TYFunction._AddToCache('AnButton',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYAnButton.registerValueCallback(callback: TYAnButtonValueCallback):LongInt;
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
      self._valueCallbackAnButton := callback;
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


  function TYAnButton._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackAnButton) <> nil) then
        begin
          self._valueCallbackAnButton(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYAnButton.resetCounter():LongInt;
    begin
      result := self.set_pulseCounter(0);
      exit;
    end;


  function TYAnButton.nextAnButton(): TYAnButton;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextAnButton := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextAnButton := nil;
          exit;
        end;
      nextAnButton := TYAnButton.FindAnButton(hwid);
    end;

  class function TYAnButton.FirstAnButton(): TYAnButton;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('AnButton', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYAnButton.FindAnButton(serial+'.'+funcId);
    end;

//--- (end of YAnButton implementation)

//--- (YAnButton functions)

  function yFindAnButton(func:string): TYAnButton;
    begin
      result := TYAnButton.FindAnButton(func);
    end;

  function yFirstAnButton(): TYAnButton;
    begin
      result := TYAnButton.FirstAnButton();
    end;

  procedure _AnButtonCleanup();
    begin
    end;

//--- (end of YAnButton functions)

initialization
  //--- (YAnButton initialization)
  //--- (end of YAnButton initialization)

finalization
  //--- (YAnButton cleanup)
  _AnButtonCleanup();
  //--- (end of YAnButton cleanup)
end.
