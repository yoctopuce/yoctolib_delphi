{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindSpectralSensor(), the high-level API for SpectralSensor functions
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


unit yocto_spectralsensor;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YSpectralSensor definitions)

const Y_LEDCURRENT_INVALID            = YAPI_INVALID_INT;
const Y_RESOLUTION_INVALID            = YAPI_INVALID_DOUBLE;
const Y_INTEGRATIONTIME_INVALID       = YAPI_INVALID_INT;
const Y_GAIN_INVALID                  = YAPI_INVALID_INT;
const Y_SATURATION_INVALID            = YAPI_INVALID_UINT;
const Y_ESTIMATEDRGB_INVALID          = YAPI_INVALID_UINT;
const Y_ESTIMATEDHSL_INVALID          = YAPI_INVALID_UINT;
const Y_ESTIMATEDXYZ_INVALID          = YAPI_INVALID_STRING;
const Y_ESTIMATEDOKLAB_INVALID        = YAPI_INVALID_STRING;
const Y_ESTIMATEDRAL_INVALID          = YAPI_INVALID_STRING;
const Y_LEDCURRENTATPOWERON_INVALID   = YAPI_INVALID_INT;
const Y_INTEGRATIONTIMEATPOWERON_INVALID = YAPI_INVALID_INT;
const Y_GAINATPOWERON_INVALID         = YAPI_INVALID_INT;

//--- (end of YSpectralSensor definitions)

//--- (YSpectralSensor yapiwrapper declaration)
//--- (end of YSpectralSensor yapiwrapper declaration)

type

  TYSpectralSensor = class;
  //--- (YSpectralSensor class start)
  TYSpectralSensorValueCallback = procedure(func: TYSpectralSensor; value:string);
  TYSpectralSensorTimedReportCallback = procedure(func: TYSpectralSensor; value:TYMeasure);

  ////
  /// <summary>
  ///   TYSpectralSensor Class: spectral sensor control interface
  /// <para>
  ///   The <c>YSpectralSensor</c> class allows you to read and configure Yoctopuce spectral sensors.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYSpectralSensor=class(TYFunction)
  //--- (end of YSpectralSensor class start)
  protected
  //--- (YSpectralSensor declaration)
    // Attributes (function value cache)
    _ledCurrent               : LongInt;
    _resolution               : double;
    _integrationTime          : LongInt;
    _gain                     : LongInt;
    _saturation               : LongInt;
    _estimatedRGB             : LongInt;
    _estimatedHSL             : LongInt;
    _estimatedXYZ             : string;
    _estimatedOkLab           : string;
    _estimatedRAL             : string;
    _ledCurrentAtPowerOn      : LongInt;
    _integrationTimeAtPowerOn : LongInt;
    _gainAtPowerOn            : LongInt;
    _valueCallbackSpectralSensor : TYSpectralSensorValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YSpectralSensor declaration)

  public
    //--- (YSpectralSensor accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the current value of the LED.
    /// <para>
    ///   This method retrieves the current flowing through the LED
    ///   and returns it as an integer or an object.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current value of the LED
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralSensor.LEDCURRENT_INVALID</c>.
    /// </para>
    ///-
    function get_ledCurrent():LongInt;

    ////
    /// <summary>
    ///   Changes the luminosity of the module leds.
    /// <para>
    ///   The parameter is a
    ///   value between 0 and 100.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the luminosity of the module leds
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
    function set_ledCurrent(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Changes the resolution of the measured physical values.
    /// <para>
    ///   The resolution corresponds to the numerical precision
    ///   when displaying value. It does not change the precision of the measure itself.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the resolution of the measured physical values
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
    function set_resolution(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the resolution of the measured values.
    /// <para>
    ///   The resolution corresponds to the numerical precision
    ///   of the measures, which is not always the same as the actual precision of the sensor.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the resolution of the measured values
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralSensor.RESOLUTION_INVALID</c>.
    /// </para>
    ///-
    function get_resolution():double;

    ////
    /// <summary>
    ///   Returns the current integration time.
    /// <para>
    ///   This method retrieves the integration time value
    ///   used for data processing and returns it as an integer or an object.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current integration time
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralSensor.INTEGRATIONTIME_INVALID</c>.
    /// </para>
    ///-
    function get_integrationTime():LongInt;

    ////
    /// <summary>
    ///   Sets the integration time for data processing.
    /// <para>
    ///   This method takes a parameter `val` and assigns it
    ///   as the new integration time. This affects the duration
    ///   for which data is integrated before being processed.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_integrationTime(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Retrieves the current gain.
    /// <para>
    ///   This method updates the gain value.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralSensor.GAIN_INVALID</c>.
    /// </para>
    ///-
    function get_gain():LongInt;

    ////
    /// <summary>
    ///   Sets the gain for signal processing.
    /// <para>
    ///   This method takes a parameter `val` and assigns it
    ///   as the new gain. This affects the sensitivity and
    ///   intensity of the processed signal.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_gain(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the current saturation of the sensor.
    /// <para>
    ///   This function updates the sensor's saturation value.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current saturation of the sensor
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralSensor.SATURATION_INVALID</c>.
    /// </para>
    ///-
    function get_saturation():LongInt;

    ////
    /// <summary>
    ///   Returns the estimated color in RGB format.
    /// <para>
    ///   This method retrieves the estimated color values
    ///   and returns them as an RGB object or structure.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the estimated color in RGB format
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralSensor.ESTIMATEDRGB_INVALID</c>.
    /// </para>
    ///-
    function get_estimatedRGB():LongInt;

    ////
    /// <summary>
    ///   Returns the estimated color in HSL format.
    /// <para>
    ///   This method retrieves the estimated color values
    ///   and returns them as an HSL object or structure.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the estimated color in HSL format
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralSensor.ESTIMATEDHSL_INVALID</c>.
    /// </para>
    ///-
    function get_estimatedHSL():LongInt;

    ////
    /// <summary>
    ///   Returns the estimated color in XYZ format.
    /// <para>
    ///   This method retrieves the estimated color values
    ///   and returns them as an XYZ object or structure.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the estimated color in XYZ format
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralSensor.ESTIMATEDXYZ_INVALID</c>.
    /// </para>
    ///-
    function get_estimatedXYZ():string;

    ////
    /// <summary>
    ///   Returns the estimated color in OkLab format.
    /// <para>
    ///   This method retrieves the estimated color values
    ///   and returns them as an OkLab object or structure.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the estimated color in OkLab format
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralSensor.ESTIMATEDOKLAB_INVALID</c>.
    /// </para>
    ///-
    function get_estimatedOkLab():string;

    function get_estimatedRAL():string;

    function get_ledCurrentAtPowerOn():LongInt;

    ////
    /// <summary>
    ///   Sets the LED current at power-on.
    /// <para>
    ///   This method takes a parameter `val` and assigns it to startupLumin, representing the LED current defined
    ///   at startup.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_ledCurrentAtPowerOn(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Retrieves the integration time at power-on.
    /// <para>
    ///   This method updates the power-on integration time value.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralSensor.INTEGRATIONTIMEATPOWERON_INVALID</c>.
    /// </para>
    ///-
    function get_integrationTimeAtPowerOn():LongInt;

    ////
    /// <summary>
    ///   Sets the integration time at power-on.
    /// <para>
    ///   This method takes a parameter `val` and assigns it to startupIntegTime, representing the integration time
    ///   defined at startup.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_integrationTimeAtPowerOn(newval:LongInt):integer;

    function get_gainAtPowerOn():LongInt;

    ////
    /// <summary>
    ///   Sets the gain at power-on.
    /// <para>
    ///   This method takes a parameter `val` and assigns it to startupGain, representing the gain defined at startup.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_gainAtPowerOn(newval:LongInt):integer;

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
    ///   Use the method <c>YSpectralSensor.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YSpectralSensor</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindSpectralSensor(func: string):TYSpectralSensor;

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
    function registerValueCallback(callback: TYSpectralSensorValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of spectral sensors started using <c>yFirstSpectralSensor()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned spectral sensors order.
    ///   If you want to find a specific a spectral sensor, use <c>SpectralSensor.findSpectralSensor()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YSpectralSensor</c> object, corresponding to
    ///   a spectral sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more spectral sensors to enumerate.
    /// </returns>
    ///-
    function nextSpectralSensor():TYSpectralSensor;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstSpectralSensor():TYSpectralSensor;
  //--- (end of YSpectralSensor accessors declaration)
  end;

//--- (YSpectralSensor functions declaration)
  ////
  /// <summary>
  ///   Retrieves a spectral sensor for a given identifier.
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
  ///   This function does not require that the spectral sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YSpectralSensor.isOnline()</c> to test if the spectral sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a spectral sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the spectral sensor, for instance
  ///   <c>MyDevice.spectralSensor</c>.
  /// </param>
  /// <returns>
  ///   a <c>YSpectralSensor</c> object allowing you to drive the spectral sensor.
  /// </returns>
  ///-
  function yFindSpectralSensor(func:string):TYSpectralSensor;
  ////
  /// <summary>
  ///   Starts the enumeration of spectral sensors currently accessible.
  /// <para>
  ///   Use the method <c>YSpectralSensor.nextSpectralSensor()</c> to iterate on
  ///   next spectral sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YSpectralSensor</c> object, corresponding to
  ///   the first spectral sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstSpectralSensor():TYSpectralSensor;

//--- (end of YSpectralSensor functions declaration)

implementation

//--- (YSpectralSensor dlldef)
//--- (end of YSpectralSensor dlldef)

  constructor TYSpectralSensor.Create(func:string);
    begin
      inherited Create(func);
      _className := 'SpectralSensor';
      //--- (YSpectralSensor accessors initialization)
      _ledCurrent := Y_LEDCURRENT_INVALID;
      _resolution := Y_RESOLUTION_INVALID;
      _integrationTime := Y_INTEGRATIONTIME_INVALID;
      _gain := Y_GAIN_INVALID;
      _saturation := Y_SATURATION_INVALID;
      _estimatedRGB := Y_ESTIMATEDRGB_INVALID;
      _estimatedHSL := Y_ESTIMATEDHSL_INVALID;
      _estimatedXYZ := Y_ESTIMATEDXYZ_INVALID;
      _estimatedOkLab := Y_ESTIMATEDOKLAB_INVALID;
      _estimatedRAL := Y_ESTIMATEDRAL_INVALID;
      _ledCurrentAtPowerOn := Y_LEDCURRENTATPOWERON_INVALID;
      _integrationTimeAtPowerOn := Y_INTEGRATIONTIMEATPOWERON_INVALID;
      _gainAtPowerOn := Y_GAINATPOWERON_INVALID;
      _valueCallbackSpectralSensor := nil;
      //--- (end of YSpectralSensor accessors initialization)
    end;

//--- (YSpectralSensor yapiwrapper)
//--- (end of YSpectralSensor yapiwrapper)

//--- (YSpectralSensor implementation)
{$HINTS OFF}
  function TYSpectralSensor._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'ledCurrent') then
        begin
          _ledCurrent := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'resolution') then
        begin
          _resolution := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'integrationTime') then
        begin
          _integrationTime := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'gain') then
        begin
          _gain := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'saturation') then
        begin
          _saturation := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'estimatedRGB') then
        begin
          _estimatedRGB := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'estimatedHSL') then
        begin
          _estimatedHSL := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'estimatedXYZ') then
        begin
          _estimatedXYZ := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'estimatedOkLab') then
        begin
          _estimatedOkLab := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'estimatedRAL') then
        begin
          _estimatedRAL := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'ledCurrentAtPowerOn') then
        begin
          _ledCurrentAtPowerOn := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'integrationTimeAtPowerOn') then
        begin
          _integrationTimeAtPowerOn := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'gainAtPowerOn') then
        begin
          _gainAtPowerOn := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYSpectralSensor.get_ledCurrent():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LEDCURRENT_INVALID;
              exit;
            end;
        end;
      res := self._ledCurrent;
      result := res;
      exit;
    end;


  function TYSpectralSensor.set_ledCurrent(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('ledCurrent',rest_val);
    end;

  function TYSpectralSensor.set_resolution(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('resolution',rest_val);
    end;

  function TYSpectralSensor.get_resolution():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RESOLUTION_INVALID;
              exit;
            end;
        end;
      res := self._resolution;
      result := res;
      exit;
    end;


  function TYSpectralSensor.get_integrationTime():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_INTEGRATIONTIME_INVALID;
              exit;
            end;
        end;
      res := self._integrationTime;
      result := res;
      exit;
    end;


  function TYSpectralSensor.set_integrationTime(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('integrationTime',rest_val);
    end;

  function TYSpectralSensor.get_gain():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_GAIN_INVALID;
              exit;
            end;
        end;
      res := self._gain;
      result := res;
      exit;
    end;


  function TYSpectralSensor.set_gain(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('gain',rest_val);
    end;

  function TYSpectralSensor.get_saturation():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SATURATION_INVALID;
              exit;
            end;
        end;
      res := self._saturation;
      result := res;
      exit;
    end;


  function TYSpectralSensor.get_estimatedRGB():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ESTIMATEDRGB_INVALID;
              exit;
            end;
        end;
      res := self._estimatedRGB;
      result := res;
      exit;
    end;


  function TYSpectralSensor.get_estimatedHSL():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ESTIMATEDHSL_INVALID;
              exit;
            end;
        end;
      res := self._estimatedHSL;
      result := res;
      exit;
    end;


  function TYSpectralSensor.get_estimatedXYZ():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ESTIMATEDXYZ_INVALID;
              exit;
            end;
        end;
      res := self._estimatedXYZ;
      result := res;
      exit;
    end;


  function TYSpectralSensor.get_estimatedOkLab():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ESTIMATEDOKLAB_INVALID;
              exit;
            end;
        end;
      res := self._estimatedOkLab;
      result := res;
      exit;
    end;


  function TYSpectralSensor.get_estimatedRAL():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ESTIMATEDRAL_INVALID;
              exit;
            end;
        end;
      res := self._estimatedRAL;
      result := res;
      exit;
    end;


  function TYSpectralSensor.get_ledCurrentAtPowerOn():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LEDCURRENTATPOWERON_INVALID;
              exit;
            end;
        end;
      res := self._ledCurrentAtPowerOn;
      result := res;
      exit;
    end;


  function TYSpectralSensor.set_ledCurrentAtPowerOn(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('ledCurrentAtPowerOn',rest_val);
    end;

  function TYSpectralSensor.get_integrationTimeAtPowerOn():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_INTEGRATIONTIMEATPOWERON_INVALID;
              exit;
            end;
        end;
      res := self._integrationTimeAtPowerOn;
      result := res;
      exit;
    end;


  function TYSpectralSensor.set_integrationTimeAtPowerOn(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('integrationTimeAtPowerOn',rest_val);
    end;

  function TYSpectralSensor.get_gainAtPowerOn():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_GAINATPOWERON_INVALID;
              exit;
            end;
        end;
      res := self._gainAtPowerOn;
      result := res;
      exit;
    end;


  function TYSpectralSensor.set_gainAtPowerOn(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('gainAtPowerOn',rest_val);
    end;

  class function TYSpectralSensor.FindSpectralSensor(func: string):TYSpectralSensor;
    var
      obj : TYSpectralSensor;
    begin
      obj := TYSpectralSensor(TYFunction._FindFromCache('SpectralSensor', func));
      if obj = nil then
        begin
          obj :=  TYSpectralSensor.create(func);
          TYFunction._AddToCache('SpectralSensor', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYSpectralSensor.registerValueCallback(callback: TYSpectralSensorValueCallback):LongInt;
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
      self._valueCallbackSpectralSensor := callback;
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


  function TYSpectralSensor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackSpectralSensor) <> nil) then
        begin
          self._valueCallbackSpectralSensor(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYSpectralSensor.nextSpectralSensor(): TYSpectralSensor;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextSpectralSensor := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextSpectralSensor := nil;
          exit;
        end;
      nextSpectralSensor := TYSpectralSensor.FindSpectralSensor(hwid);
    end;

  class function TYSpectralSensor.FirstSpectralSensor(): TYSpectralSensor;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('SpectralSensor', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYSpectralSensor.FindSpectralSensor(serial+'.'+funcId);
    end;

//--- (end of YSpectralSensor implementation)

//--- (YSpectralSensor functions)

  function yFindSpectralSensor(func:string): TYSpectralSensor;
    begin
      result := TYSpectralSensor.FindSpectralSensor(func);
    end;

  function yFirstSpectralSensor(): TYSpectralSensor;
    begin
      result := TYSpectralSensor.FirstSpectralSensor();
    end;

  procedure _SpectralSensorCleanup();
    begin
    end;

//--- (end of YSpectralSensor functions)

initialization
  //--- (YSpectralSensor initialization)
  //--- (end of YSpectralSensor initialization)

finalization
  //--- (YSpectralSensor cleanup)
  _SpectralSensorCleanup();
  //--- (end of YSpectralSensor cleanup)

end.
