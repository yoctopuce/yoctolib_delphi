{*********************************************************************
 *
 * $Id: yocto_anbutton.pas 15254 2014-03-06 10:16:24Z seb $
 *
 * Implements yFindAnButton(), the high-level API for AnButton functions
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


unit yocto_anbutton;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

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


//--- (end of YAnButton definitions)

type
  TYAnButton = class;
  //--- (YAnButton class start)
  TYAnButtonValueCallback = procedure(func: TYAnButton; value:string);
  TYAnButtonTimedReportCallback = procedure(func: TYAnButton; value:TYMeasure);

  ////
  /// <summary>
  ///   TYAnButton Class: AnButton function interface
  /// <para>
  ///   Yoctopuce application programming interface allows you to measure the state
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
    _logicalName              : string;
    _advertisedValue          : string;
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
    ///   On failure, throws an exception or returns <c>Y_CALIBRATEDVALUE_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_RAWVALUE_INVALID</c>.
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
    ///   either <c>Y_ANALOGCALIBRATION_OFF</c> or <c>Y_ANALOGCALIBRATION_ON</c>
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ANALOGCALIBRATION_INVALID</c>.
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
    ///   either <c>Y_ANALOGCALIBRATION_OFF</c> or <c>Y_ANALOGCALIBRATION_ON</c>
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
    ///   On failure, throws an exception or returns <c>Y_CALIBRATIONMAX_INVALID</c>.
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    ///   On failure, throws an exception or returns <c>Y_CALIBRATIONMIN_INVALID</c>.
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    ///   On failure, throws an exception or returns <c>Y_SENSITIVITY_INVALID</c>.
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    ///   either <c>Y_ISPRESSED_FALSE</c> or <c>Y_ISPRESSED_TRUE</c>, according to true if the input
    ///   (considered as binary) is active (closed contact), and false otherwise
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ISPRESSED_INVALID</c>.
    /// </para>
    ///-
    function get_isPressed():Integer;

    ////
    /// <summary>
    ///   Returns the number of elapsed milliseconds between the module power on and the last time
    ///   the input button was pressed (the input contact transitionned from open to closed).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of elapsed milliseconds between the module power on and the last time
    ///   the input button was pressed (the input contact transitionned from open to closed)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LASTTIMEPRESSED_INVALID</c>.
    /// </para>
    ///-
    function get_lastTimePressed():int64;

    ////
    /// <summary>
    ///   Returns the number of elapsed milliseconds between the module power on and the last time
    ///   the input button was released (the input contact transitionned from closed to open).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of elapsed milliseconds between the module power on and the last time
    ///   the input button was released (the input contact transitionned from closed to open)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LASTTIMERELEASED_INVALID</c>.
    /// </para>
    ///-
    function get_lastTimeReleased():int64;

    ////
    /// <summary>
    ///   Returns the pulse counter value
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the pulse counter value
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PULSECOUNTER_INVALID</c>.
    /// </para>
    ///-
    function get_pulseCounter():int64;

    function set_pulseCounter(newval:int64):integer;

    ////
    /// <summary>
    ///   Returns the timer of the pulses counter (ms)
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the timer of the pulses counter (ms)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PULSETIMER_INVALID</c>.
    /// </para>
    ///-
    function get_pulseTimer():int64;

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
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$
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
    function registerValueCallback(callback: TYAnButtonValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Returns the pulse counter value as well as his timer
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YAnButton</c> object, corresponding to
    ///   an analog input currently online, or a <c>null</c> pointer
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

//--- (AnButton functions declaration)

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
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the analog input
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
  ///   the first analog input currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstAnButton():TYAnButton;

//--- (end of AnButton functions declaration)

implementation

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
      _valueCallbackAnButton := nil;
      //--- (end of YAnButton accessors initialization)
    end;


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
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

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
  ///   On failure, throws an exception or returns Y_CALIBRATEDVALUE_INVALID.
  /// </para>
  ///-
  function TYAnButton.get_calibratedValue():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CALIBRATEDVALUE_INVALID;
              exit
            end;
        end;
      result := self._calibratedValue;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_RAWVALUE_INVALID.
  /// </para>
  ///-
  function TYAnButton.get_rawValue():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_RAWVALUE_INVALID;
              exit
            end;
        end;
      result := self._rawValue;
      exit;
    end;


  ////
  /// <summary>
  ///   Tells if a calibration process is currently ongoing.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_ANALOGCALIBRATION_OFF or Y_ANALOGCALIBRATION_ON
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ANALOGCALIBRATION_INVALID.
  /// </para>
  ///-
  function TYAnButton.get_analogCalibration():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ANALOGCALIBRATION_INVALID;
              exit
            end;
        end;
      result := self._analogCalibration;
      exit;
    end;


  ////
  /// <summary>
  ///   Starts or stops the calibration process.
  /// <para>
  ///   Remember to call the saveToFlash()
  ///   method of the module at the end of the calibration if the modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   either Y_ANALOGCALIBRATION_OFF or Y_ANALOGCALIBRATION_ON
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
  function TYAnButton.set_analogCalibration(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('analogCalibration',rest_val);
    end;

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
  ///   On failure, throws an exception or returns Y_CALIBRATIONMAX_INVALID.
  /// </para>
  ///-
  function TYAnButton.get_calibrationMax():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CALIBRATIONMAX_INVALID;
              exit
            end;
        end;
      result := self._calibrationMax;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the maximal calibration value for the input (between 0 and 4095, included), without actually
  ///   starting the automated calibration.
  /// <para>
  ///   Remember to call the saveToFlash()
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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYAnButton.set_calibrationMax(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('calibrationMax',rest_val);
    end;

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
  ///   On failure, throws an exception or returns Y_CALIBRATIONMIN_INVALID.
  /// </para>
  ///-
  function TYAnButton.get_calibrationMin():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CALIBRATIONMIN_INVALID;
              exit
            end;
        end;
      result := self._calibrationMin;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the minimal calibration value for the input (between 0 and 4095, included), without actually
  ///   starting the automated calibration.
  /// <para>
  ///   Remember to call the saveToFlash()
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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYAnButton.set_calibrationMin(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('calibrationMin',rest_val);
    end;

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
  ///   On failure, throws an exception or returns Y_SENSITIVITY_INVALID.
  /// </para>
  ///-
  function TYAnButton.get_sensitivity():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SENSITIVITY_INVALID;
              exit
            end;
        end;
      result := self._sensitivity;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the sensibility for the input (between 1 and 1000) for triggering user callbacks.
  /// <para>
  ///   The sensibility is used to filter variations around a fixed value, but does not preclude the
  ///   transmission of events when the input value evolves constantly in the same direction.
  ///   Special case: when the value 1000 is used, the callback will only be thrown when the logical state
  ///   of the input switches from pressed to released and back.
  ///   Remember to call the saveToFlash() method of the module if the modification must be kept.
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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYAnButton.set_sensitivity(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('sensitivity',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns true if the input (considered as binary) is active (closed contact), and false otherwise.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_ISPRESSED_FALSE or Y_ISPRESSED_TRUE, according to true if the input (considered as binary)
  ///   is active (closed contact), and false otherwise
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ISPRESSED_INVALID.
  /// </para>
  ///-
  function TYAnButton.get_isPressed():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ISPRESSED_INVALID;
              exit
            end;
        end;
      result := self._isPressed;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the number of elapsed milliseconds between the module power on and the last time
  ///   the input button was pressed (the input contact transitionned from open to closed).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the number of elapsed milliseconds between the module power on and the last time
  ///   the input button was pressed (the input contact transitionned from open to closed)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LASTTIMEPRESSED_INVALID.
  /// </para>
  ///-
  function TYAnButton.get_lastTimePressed():int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LASTTIMEPRESSED_INVALID;
              exit
            end;
        end;
      result := self._lastTimePressed;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the number of elapsed milliseconds between the module power on and the last time
  ///   the input button was released (the input contact transitionned from closed to open).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the number of elapsed milliseconds between the module power on and the last time
  ///   the input button was released (the input contact transitionned from closed to open)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LASTTIMERELEASED_INVALID.
  /// </para>
  ///-
  function TYAnButton.get_lastTimeReleased():int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LASTTIMERELEASED_INVALID;
              exit
            end;
        end;
      result := self._lastTimeReleased;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the pulse counter value
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the pulse counter value
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PULSECOUNTER_INVALID.
  /// </para>
  ///-
  function TYAnButton.get_pulseCounter():int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PULSECOUNTER_INVALID;
              exit
            end;
        end;
      result := self._pulseCounter;
      exit;
    end;


  function TYAnButton.set_pulseCounter(newval:int64):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('pulseCounter',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the timer of the pulses counter (ms)
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the timer of the pulses counter (ms)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PULSETIMER_INVALID.
  /// </para>
  ///-
  function TYAnButton.get_pulseTimer():int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PULSETIMER_INVALID;
              exit
            end;
        end;
      result := self._pulseTimer;
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
  ///   Use the method <c>YAnButton.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YAnButton</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYAnButton.FindAnButton(func: string):TYAnButton;
    var
      obj : TYAnButton;
    begin
      obj := TYAnButton(TYFunction._FindFromCache('AnButton', func));
      if obj = nil then
        begin
          obj :=  TYAnButton.create(func);
          TYFunction._AddToCache('AnButton',  func, obj)
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
  function TYAnButton.registerValueCallback(callback: TYAnButtonValueCallback):LongInt;
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
      self._valueCallbackAnButton := callback;
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


  function TYAnButton._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackAnButton) <> nil) then
        begin
          self._valueCallbackAnButton(self, value)
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
  ///   Returns the pulse counter value as well as his timer
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
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

//--- (AnButton functions)

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

//--- (end of AnButton functions)

initialization
  //--- (AnButton initialization)
  //--- (end of AnButton initialization)

finalization
  //--- (AnButton cleanup)
  _AnButtonCleanup();
  //--- (end of AnButton cleanup)
end.
