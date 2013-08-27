{*********************************************************************
 *
 * $Id: yocto_current.pas 12324 2013-08-13 15:10:31Z mvuilleu $
 *
 * Implements yFindCurrent(), the high-level API for Current functions
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


unit yocto_current;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (YCurrent definitions)

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_UNIT_INVALID                  = YAPI_INVALID_STRING;
   Y_CURRENTVALUE_INVALID          : double = YAPI_INVALID_DOUBLE;
   Y_LOWESTVALUE_INVALID           : double = YAPI_INVALID_DOUBLE;
   Y_HIGHESTVALUE_INVALID          : double = YAPI_INVALID_DOUBLE;
   Y_CURRENTRAWVALUE_INVALID       : double = YAPI_INVALID_DOUBLE;
   Y_CALIBRATIONPARAM_INVALID      = YAPI_INVALID_STRING;
   Y_RESOLUTION_INVALID            : double = YAPI_INVALID_DOUBLE;


//--- (end of YCurrent definitions)

type
//--- (YCurrent declaration)
 TYCurrent = class;
 TUpdateCallback  = procedure(func: TYCurrent; value:string);
////
/// <summary>
///   TYCurrent Class: Current function interface
/// <para>
///   The Yoctopuce application programming interface allows you to read an instant
///   measure of the sensor, as well as the minimal and maximal values observed.
/// </para>
/// </summary>
///-
TYCurrent=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _unit                     : string;
   _currentValue             : double;
   _lowestValue              : double;
   _highestValue             : double;
   _currentRawValue          : double;
   _calibrationParam         : string;
   _resolution               : double;
   _calibrationOffset        : LongInt;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YCurrent declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of current sensors started using <c>yFirstCurrent()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YCurrent</c> object, corresponding to
   ///   a current sensor currently online, or a <c>null</c> pointer
   ///   if there are no more current sensors to enumerate.
   /// </returns>
   ///-
   function nextCurrent():TYCurrent;

   //--- (YCurrent accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the current sensor.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the current sensor
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the current sensor.
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
   ///   a string corresponding to the logical name of the current sensor
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
   ///   Returns the current value of the current sensor (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the current sensor (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the measuring unit for the measured value.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the measuring unit for the measured value
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_UNIT_INVALID</c>.
   /// </para>
   ///-
   function get_unit():string;

   ////
   /// <summary>
   ///   Returns the current measured value.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a floating point number corresponding to the current measured value
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_CURRENTVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_currentValue():double;

   ////
   /// <summary>
   ///   Changes the recorded minimal value observed.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   a floating point number corresponding to the recorded minimal value observed
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
   function set_lowestValue(newval:double):integer;

   ////
   /// <summary>
   ///   Returns the minimal value observed.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a floating point number corresponding to the minimal value observed
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOWESTVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_lowestValue():double;

   ////
   /// <summary>
   ///   Changes the recorded maximal value observed.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   a floating point number corresponding to the recorded maximal value observed
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
   function set_highestValue(newval:double):integer;

   ////
   /// <summary>
   ///   Returns the maximal value observed.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a floating point number corresponding to the maximal value observed
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_HIGHESTVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_highestValue():double;

   ////
   /// <summary>
   ///   Returns the uncalibrated, unrounded raw value returned by the sensor.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a floating point number corresponding to the uncalibrated, unrounded raw value returned by the sensor
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_CURRENTRAWVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_currentRawValue():double;

   function get_calibrationParam():string;

   function set_calibrationParam(newval:string):integer;

   ////
   /// <summary>
   ///   Configures error correction data points, in particular to compensate for
   ///   a possible perturbation of the measure caused by an enclosure.
   /// <para>
   ///   It is possible
   ///   to configure up to five correction points. Correction points must be provided
   ///   in ascending order, and be in the range of the sensor. The device will automatically
   ///   perform a linear interpolation of the error correction between specified
   ///   points. Remember to call the <c>saveToFlash()</c> method of the module if the
   ///   modification must be kept.
   /// </para>
   /// <para>
   ///   For more information on advanced capabilities to refine the calibration of
   ///   sensors, please contact support@yoctopuce.com.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="rawValues">
   ///   array of floating point numbers, corresponding to the raw
   ///   values returned by the sensor for the correction points.
   /// </param>
   /// <param name="refValues">
   ///   array of floating point numbers, corresponding to the corrected
   ///   values for the correction points.
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
   function calibrateFromPoints(rawValues:floatArr;refValues:floatArr):integer;

   function loadCalibrationPoints(var rawValues:floatArr;var refValues:floatArr):integer;

   ////
   /// <summary>
   ///   Changes the resolution of the measured values.
   /// <para>
   ///   The resolution corresponds to the numerical precision
   ///   when displaying value. It does not change the precision of the measure itself.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   a floating point number corresponding to the resolution of the measured values
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
   function set_resolution(newval:double):integer;

   ////
   /// <summary>
   ///   Returns the resolution of the measured values.
   /// <para>
   ///   The resolution corresponds to the numerical precision
   ///   when displaying value, which is not always the same as the actual precision of the sensor.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a floating point number corresponding to the resolution of the measured values
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_RESOLUTION_INVALID</c>.
   /// </para>
   ///-
   function get_resolution():double;

   //--- (end of YCurrent accessors declaration)
end;

//--- (Current functions declaration)

////
/// <summary>
///   Retrieves a current sensor for a given identifier.
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
///   This function does not require that the current sensor is online at the time
///   it is invoked. The returned object is nevertheless valid.
///   Use the method <c>YCurrent.isOnline()</c> to test if the current sensor is
///   indeed online at a given time. In case of ambiguity when looking for
///   a current sensor by logical name, no error is notified: the first instance
///   found is returned. The search is performed first by hardware name,
///   then by logical name.
/// </para>
/// </summary>
/// <param name="func">
///   a string that uniquely characterizes the current sensor
/// </param>
/// <returns>
///   a <c>YCurrent</c> object allowing you to drive the current sensor.
/// </returns>
///-
function yFindCurrent(func:string):TYCurrent;
////
/// <summary>
///   Starts the enumeration of current sensors currently accessible.
/// <para>
///   Use the method <c>YCurrent.nextCurrent()</c> to iterate on
///   next current sensors.
/// </para>
/// </summary>
/// <returns>
///   a pointer to a <c>YCurrent</c> object, corresponding to
///   the first current sensor currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstCurrent():TYCurrent;

//--- (end of Current functions declaration)

implementation

//--- (YCurrent implementation)

var
   _CurrentCache : TStringList;

constructor TYCurrent.Create(func:string);
 begin
   inherited Create('Current', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _unit := Y_UNIT_INVALID;
   _currentValue := Y_CURRENTVALUE_INVALID;
   _lowestValue := Y_LOWESTVALUE_INVALID;
   _highestValue := Y_HIGHESTVALUE_INVALID;
   _currentRawValue := Y_CURRENTRAWVALUE_INVALID;
   _calibrationParam := Y_CALIBRATIONPARAM_INVALID;
   _resolution := Y_RESOLUTION_INVALID;
   _calibrationOffset := -32767;
 end;

{$HINTS OFF}
function TYCurrent._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'unit') then
       begin
         _unit := string(member^.svalue);
       end else
      if (member^.name = 'currentValue') then
       begin
         _currentValue := round(member^.ivalue/65536.0);
       end else
      if (member^.name = 'lowestValue') then
       begin
         _lowestValue := round(member^.ivalue/65536.0);
       end else
      if (member^.name = 'highestValue') then
       begin
         _highestValue := round(member^.ivalue/65536.0);
       end else
      if (member^.name = 'currentRawValue') then
       begin
         _currentRawValue := member^.ivalue/65536.0;
       end else
      if (member^.name = 'calibrationParam') then
       begin
         _calibrationParam := string(member^.svalue);
       end else
      if (member^.name = 'resolution') then
       begin
         if (member^.ivalue > 100) then _resolution := 1.0 / round(65536.0/member^.ivalue) else _resolution := 0.001 / round(67.0/member^.ivalue);
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the current sensor.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the current sensor
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYCurrent.get_logicalName():string;
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
///   Changes the logical name of the current sensor.
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
///   a string corresponding to the logical name of the current sensor
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
function TYCurrent.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the current sensor (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the current sensor (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYCurrent.get_advertisedValue():string;
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
///   Returns the measuring unit for the measured value.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the measuring unit for the measured value
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_UNIT_INVALID.
/// </para>
///-
function TYCurrent.get_unit():string;
 begin
   if (_unit = Y_UNIT_INVALID) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_UNIT_INVALID;
         exit;
       end;
   result := _unit;
 end;

////
/// <summary>
///   Returns the current measured value.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a floating point number corresponding to the current measured value
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_CURRENTVALUE_INVALID.
/// </para>
///-
function TYCurrent.get_currentValue():double;
 var res : double;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CURRENTVALUE_INVALID;
         exit;
       end;
    res := _applyCalibration(_currentRawValue, _calibrationParam, _calibrationOffset, _resolution);
    if(res <> Y_CURRENTVALUE_INVALID) then
       begin
         result := res;
         exit;
       end;
   result := _currentValue;
 end;

////
/// <summary>
///   Changes the recorded minimal value observed.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   a floating point number corresponding to the recorded minimal value observed
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
function TYCurrent.set_lowestValue(newval:double):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(round(newval*65536.0));
   result := _setAttr('lowestValue',rest_val);
 end;

////
/// <summary>
///   Returns the minimal value observed.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a floating point number corresponding to the minimal value observed
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOWESTVALUE_INVALID.
/// </para>
///-
function TYCurrent.get_lowestValue():double;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LOWESTVALUE_INVALID;
         exit;
       end;
   result := _lowestValue;
 end;

////
/// <summary>
///   Changes the recorded maximal value observed.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   a floating point number corresponding to the recorded maximal value observed
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
function TYCurrent.set_highestValue(newval:double):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(round(newval*65536.0));
   result := _setAttr('highestValue',rest_val);
 end;

////
/// <summary>
///   Returns the maximal value observed.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a floating point number corresponding to the maximal value observed
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_HIGHESTVALUE_INVALID.
/// </para>
///-
function TYCurrent.get_highestValue():double;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_HIGHESTVALUE_INVALID;
         exit;
       end;
   result := _highestValue;
 end;

////
/// <summary>
///   Returns the uncalibrated, unrounded raw value returned by the sensor.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a floating point number corresponding to the uncalibrated, unrounded raw value returned by the sensor
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_CURRENTRAWVALUE_INVALID.
/// </para>
///-
function TYCurrent.get_currentRawValue():double;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CURRENTRAWVALUE_INVALID;
         exit;
       end;
   result := _currentRawValue;
 end;

function TYCurrent.get_calibrationParam():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CALIBRATIONPARAM_INVALID;
         exit;
       end;
   result := _calibrationParam;
 end;

function TYCurrent.set_calibrationParam(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('calibrationParam',rest_val);
 end;

////
/// <summary>
///   Configures error correction data points, in particular to compensate for
///   a possible perturbation of the measure caused by an enclosure.
/// <para>
///   It is possible
///   to configure up to five correction points. Correction points must be provided
///   in ascending order, and be in the range of the sensor. The device will automatically
///   perform a linear interpolation of the error correction between specified
///   points. Remember to call the saveToFlash() method of the module if the
///   modification must be kept.
/// </para>
/// <para>
///   For more information on advanced capabilities to refine the calibration of
///   sensors, please contact support@yoctopuce.com.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="rawValues">
///   array of floating point numbers, corresponding to the raw
///   values returned by the sensor for the correction points.
/// </param>
/// <param name="refValues">
///   array of floating point numbers, corresponding to the corrected
///   values for the correction points.
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
function TYCurrent.calibrateFromPoints(rawValues:floatArr;refValues:floatArr):integer;
 var
   rest_val: string;
 begin
   rest_val := _encodeCalibrationPoints(rawValues,refValues,_resolution,_calibrationOffset,_calibrationParam);
   result := _setAttr('calibrationParam', rest_val);
 end;

function TYCurrent.loadCalibrationPoints(var rawValues:floatArr;var refValues:floatArr):integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := _lastErrorType;
         exit;
       end;
   result := _decodeCalibrationPoints(_calibrationParam,nil,rawValues,refValues,_resolution,_calibrationOffset);
 end;

////
/// <summary>
///   Changes the resolution of the measured values.
/// <para>
///   The resolution corresponds to the numerical precision
///   when displaying value. It does not change the precision of the measure itself.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   a floating point number corresponding to the resolution of the measured values
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
function TYCurrent.set_resolution(newval:double):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(round(newval*65536.0));
   result := _setAttr('resolution',rest_val);
 end;

////
/// <summary>
///   Returns the resolution of the measured values.
/// <para>
///   The resolution corresponds to the numerical precision
///   when displaying value, which is not always the same as the actual precision of the sensor.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a floating point number corresponding to the resolution of the measured values
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_RESOLUTION_INVALID.
/// </para>
///-
function TYCurrent.get_resolution():double;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_RESOLUTION_INVALID;
         exit;
       end;
   result := _resolution;
 end;

function TYCurrent.nextCurrent(): TYCurrent;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextCurrent := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextCurrent := nil;
      exit;
    end;
    nextCurrent := yFindCurrent(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYCurrent.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYCurrent.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYCurrent.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYCurrent.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YCurrent implementation)

//--- (Current functions)

function yFindCurrent(func:string): TYCurrent;
 var
   index: integer;
   res  : TYCurrent;
 begin
    if (_CurrentCache.Find(func, index)) then
     begin
       yFindCurrent := TYCurrent(_CurrentCache.objects[index]);
       exit;
     end;
   res := TYCurrent.Create(func);
   _CurrentCache.addObject(func, res);
   yFindCurrent := res;
 end;

function yFirstCurrent(): TYCurrent;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('Current', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstCurrent := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstCurrent := nil;
       exit;
    end;
   yFirstCurrent := yFindCurrent(serial+'.'+funcId);
 end;

procedure _CurrentCleanup();
  var i:integer;
begin
  for i:=0 to _CurrentCache.count-1 do 
    begin
     _CurrentCache.objects[i].free();
     _CurrentCache.objects[i]:=nil;
    end;
   _CurrentCache.free();
   _CurrentCache:=nil;
end;

//--- (end of Current functions)

initialization
   //--- (Current initialization)
   _CurrentCache        := TstringList.create();
   _CurrentCache.sorted := true;
   //--- (end of Current initialization)

finalization
   //--- (Current cleanup)
   _CurrentCleanup();
   //--- (end of Current cleanup)
end.
