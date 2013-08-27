{*********************************************************************
 *
 * $Id: yocto_anbutton.pas 12324 2013-08-13 15:10:31Z mvuilleu $
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

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_CALIBRATEDVALUE_INVALID       = YAPI_INVALID_LONGWORD;
   Y_RAWVALUE_INVALID              = YAPI_INVALID_LONGWORD;
   Y_ANALOGCALIBRATION_OFF = 0;
   Y_ANALOGCALIBRATION_ON = 1;
   Y_ANALOGCALIBRATION_INVALID = -1;

   Y_CALIBRATIONMAX_INVALID        = YAPI_INVALID_LONGWORD;
   Y_CALIBRATIONMIN_INVALID        = YAPI_INVALID_LONGWORD;
   Y_SENSITIVITY_INVALID           = YAPI_INVALID_LONGWORD;
   Y_ISPRESSED_FALSE = 0;
   Y_ISPRESSED_TRUE = 1;
   Y_ISPRESSED_INVALID = -1;

   Y_LASTTIMEPRESSED_INVALID       = YAPI_INVALID_LONGWORD;
   Y_LASTTIMERELEASED_INVALID      = YAPI_INVALID_LONGWORD;
   Y_PULSECOUNTER_INVALID          = YAPI_INVALID_LONGWORD;
   Y_PULSETIMER_INVALID            = YAPI_INVALID_LONGWORD;


//--- (end of YAnButton definitions)

type
//--- (YAnButton declaration)
 TYAnButton = class;
 TUpdateCallback  = procedure(func: TYAnButton; value:string);
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
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _calibratedValue          : LongWord;
   _rawValue                 : LongWord;
   _analogCalibration        : Integer;
   _calibrationMax           : LongWord;
   _calibrationMin           : LongWord;
   _sensitivity              : LongWord;
   _isPressed                : Integer;
   _lastTimePressed          : LongWord;
   _lastTimeReleased         : LongWord;
   _pulseCounter             : LongWord;
   _pulseTimer               : LongWord;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YAnButton declaration)

public
   constructor Create(func:string);

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

   //--- (YAnButton accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the analog input.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the analog input
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the analog input.
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
   ///   a string corresponding to the logical name of the analog input
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
   ///   Returns the current value of the analog input (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the analog input (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

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
   function get_calibratedValue():LongWord;

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
   function get_rawValue():LongWord;

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
   function get_calibrationMax():LongWord;

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
   function set_calibrationMax(newval:LongWord):integer;

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
   function get_calibrationMin():LongWord;

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
   function set_calibrationMin(newval:LongWord):integer;

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
   function get_sensitivity():LongWord;

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
   function set_sensitivity(newval:LongWord):integer;

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
   function get_lastTimePressed():LongWord;

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
   function get_lastTimeReleased():LongWord;

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
   function get_pulseCounter():LongWord;

   function set_pulseCounter(newval:LongWord):integer;

   ////
   /// <summary>
   ///   Returns the pulse counter value as well as his timer
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function resetCounter():integer;

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
   function get_pulseTimer():LongWord;

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

//--- (YAnButton implementation)

var
   _AnButtonCache : TStringList;

constructor TYAnButton.Create(func:string);
 begin
   inherited Create('AnButton', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
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
 end;

{$HINTS OFF}
function TYAnButton._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'calibratedValue') then
       begin
         _calibratedValue := member^.ivalue;
       end else
      if (member^.name = 'rawValue') then
       begin
         _rawValue := member^.ivalue;
       end else
      if (member^.name = 'analogCalibration') then
       begin
         _analogCalibration := member^.ivalue;
       end else
      if (member^.name = 'calibrationMax') then
       begin
         _calibrationMax := member^.ivalue;
       end else
      if (member^.name = 'calibrationMin') then
       begin
         _calibrationMin := member^.ivalue;
       end else
      if (member^.name = 'sensitivity') then
       begin
         _sensitivity := member^.ivalue;
       end else
      if (member^.name = 'isPressed') then
       begin
         _isPressed := member^.ivalue;
       end else
      if (member^.name = 'lastTimePressed') then
       begin
         _lastTimePressed := member^.ivalue;
       end else
      if (member^.name = 'lastTimeReleased') then
       begin
         _lastTimeReleased := member^.ivalue;
       end else
      if (member^.name = 'pulseCounter') then
       begin
         _pulseCounter := member^.ivalue;
       end else
      if (member^.name = 'pulseTimer') then
       begin
         _pulseTimer := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the analog input.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the analog input
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYAnButton.get_logicalName():string;
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
///   Changes the logical name of the analog input.
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
///   a string corresponding to the logical name of the analog input
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
function TYAnButton.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the analog input (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the analog input (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYAnButton.get_advertisedValue():string;
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
function TYAnButton.get_calibratedValue():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CALIBRATEDVALUE_INVALID;
         exit;
       end;
   result := _calibratedValue;
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
function TYAnButton.get_rawValue():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_RAWVALUE_INVALID;
         exit;
       end;
   result := _rawValue;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_ANALOGCALIBRATION_INVALID;
         exit;
       end;
   result := _analogCalibration;
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
function TYAnButton.get_calibrationMax():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CALIBRATIONMAX_INVALID;
         exit;
       end;
   result := _calibrationMax;
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
function TYAnButton.set_calibrationMax(newval:LongWord):integer;
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
function TYAnButton.get_calibrationMin():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CALIBRATIONMIN_INVALID;
         exit;
       end;
   result := _calibrationMin;
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
function TYAnButton.set_calibrationMin(newval:LongWord):integer;
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
function TYAnButton.get_sensitivity():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_SENSITIVITY_INVALID;
         exit;
       end;
   result := _sensitivity;
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
function TYAnButton.set_sensitivity(newval:LongWord):integer;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_ISPRESSED_INVALID;
         exit;
       end;
   result := _isPressed;
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
function TYAnButton.get_lastTimePressed():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LASTTIMEPRESSED_INVALID;
         exit;
       end;
   result := _lastTimePressed;
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
function TYAnButton.get_lastTimeReleased():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LASTTIMERELEASED_INVALID;
         exit;
       end;
   result := _lastTimeReleased;
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
function TYAnButton.get_pulseCounter():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PULSECOUNTER_INVALID;
         exit;
       end;
   result := _pulseCounter;
 end;

function TYAnButton.set_pulseCounter(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('pulseCounter',rest_val);
 end;

////
/// <summary>
///   Returns the pulse counter value as well as his timer
/// <para>
/// </para>
/// </summary>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYAnButton.resetCounter():integer;
 var
   rest_val: string;
 begin
   rest_val := '0';
   result := _setAttr('pulseCounter', rest_val);
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
function TYAnButton.get_pulseTimer():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PULSETIMER_INVALID;
         exit;
       end;
   result := _pulseTimer;
 end;

function TYAnButton.nextAnButton(): TYAnButton;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextAnButton := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextAnButton := nil;
      exit;
    end;
    nextAnButton := yFindAnButton(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYAnButton.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYAnButton.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYAnButton.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYAnButton.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YAnButton implementation)

//--- (AnButton functions)

function yFindAnButton(func:string): TYAnButton;
 var
   index: integer;
   res  : TYAnButton;
 begin
    if (_AnButtonCache.Find(func, index)) then
     begin
       yFindAnButton := TYAnButton(_AnButtonCache.objects[index]);
       exit;
     end;
   res := TYAnButton.Create(func);
   _AnButtonCache.addObject(func, res);
   yFindAnButton := res;
 end;

function yFirstAnButton(): TYAnButton;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('AnButton', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstAnButton := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstAnButton := nil;
       exit;
    end;
   yFirstAnButton := yFindAnButton(serial+'.'+funcId);
 end;

procedure _AnButtonCleanup();
  var i:integer;
begin
  for i:=0 to _AnButtonCache.count-1 do 
    begin
     _AnButtonCache.objects[i].free();
     _AnButtonCache.objects[i]:=nil;
    end;
   _AnButtonCache.free();
   _AnButtonCache:=nil;
end;

//--- (end of AnButton functions)

initialization
   //--- (AnButton initialization)
   _AnButtonCache        := TstringList.create();
   _AnButtonCache.sorted := true;
   //--- (end of AnButton initialization)

finalization
   //--- (AnButton cleanup)
   _AnButtonCleanup();
   //--- (end of AnButton cleanup)
end.
