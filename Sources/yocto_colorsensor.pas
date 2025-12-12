{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindColorSensor(), the high-level API for ColorSensor functions
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


unit yocto_colorsensor;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YColorSensor definitions)

const Y_ESTIMATIONMODEL_REFLECTION = 0;
const Y_ESTIMATIONMODEL_EMISSION = 1;
const Y_ESTIMATIONMODEL_INVALID = -1;
const Y_WORKINGMODE_AUTO = 0;
const Y_WORKINGMODE_EXPERT = 1;
const Y_WORKINGMODE_AUTOGAIN = 2;
const Y_WORKINGMODE_INVALID = -1;
const Y_LEDCURRENT_INVALID            = YAPI_INVALID_UINT;
const Y_LEDCALIBRATION_INVALID        = YAPI_INVALID_UINT;
const Y_INTEGRATIONTIME_INVALID       = YAPI_INVALID_UINT;
const Y_GAIN_INVALID                  = YAPI_INVALID_UINT;
const Y_AUTOGAIN_INVALID              = YAPI_INVALID_STRING;
const Y_SATURATION_INVALID            = YAPI_INVALID_UINT;
const Y_ESTIMATEDRGB_INVALID          = YAPI_INVALID_UINT;
const Y_ESTIMATEDHSL_INVALID          = YAPI_INVALID_UINT;
const Y_ESTIMATEDXYZ_INVALID          = YAPI_INVALID_STRING;
const Y_ESTIMATEDOKLAB_INVALID        = YAPI_INVALID_STRING;
const Y_NEARRAL1_INVALID              = YAPI_INVALID_STRING;
const Y_NEARRAL2_INVALID              = YAPI_INVALID_STRING;
const Y_NEARRAL3_INVALID              = YAPI_INVALID_STRING;
const Y_NEARHTMLCOLOR_INVALID         = YAPI_INVALID_STRING;
const Y_NEARSIMPLECOLORINDEX_BROWN = 0;
const Y_NEARSIMPLECOLORINDEX_RED = 1;
const Y_NEARSIMPLECOLORINDEX_ORANGE = 2;
const Y_NEARSIMPLECOLORINDEX_YELLOW = 3;
const Y_NEARSIMPLECOLORINDEX_WHITE = 4;
const Y_NEARSIMPLECOLORINDEX_GRAY = 5;
const Y_NEARSIMPLECOLORINDEX_BLACK = 6;
const Y_NEARSIMPLECOLORINDEX_GREEN = 7;
const Y_NEARSIMPLECOLORINDEX_BLUE = 8;
const Y_NEARSIMPLECOLORINDEX_PURPLE = 9;
const Y_NEARSIMPLECOLORINDEX_PINK = 10;
const Y_NEARSIMPLECOLORINDEX_INVALID = -1;
const Y_NEARSIMPLECOLOR_INVALID       = YAPI_INVALID_STRING;

//--- (end of YColorSensor definitions)

//--- (YColorSensor yapiwrapper declaration)
//--- (end of YColorSensor yapiwrapper declaration)

type

  TYColorSensor = class;
  //--- (YColorSensor class start)
  TYColorSensorValueCallback = procedure(func: TYColorSensor; value:string);
  TYColorSensorTimedReportCallback = procedure(func: TYColorSensor; value:TYMeasure);

  ////
  /// <summary>
  ///   TYColorSensor Class: color sensor control interface
  /// <para>
  ///   The <c>YColorSensor</c> class allows you to read and configure Yoctopuce color sensors.
  /// </para>
  /// </summary>
  ///-
  TYColorSensor=class(TYFunction)
  //--- (end of YColorSensor class start)
  protected
  //--- (YColorSensor declaration)
    // Attributes (function value cache)
    _estimationModel          : Integer;
    _workingMode              : Integer;
    _ledCurrent               : LongInt;
    _ledCalibration           : LongInt;
    _integrationTime          : LongInt;
    _gain                     : LongInt;
    _autoGain                 : string;
    _saturation               : LongInt;
    _estimatedRGB             : LongInt;
    _estimatedHSL             : LongInt;
    _estimatedXYZ             : string;
    _estimatedOkLab           : string;
    _nearRAL1                 : string;
    _nearRAL2                 : string;
    _nearRAL3                 : string;
    _nearHTMLColor            : string;
    _nearSimpleColorIndex     : Integer;
    _nearSimpleColor          : string;
    _valueCallbackColorSensor : TYColorSensorValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YColorSensor declaration)

  public
    //--- (YColorSensor accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the predictive model used for color estimation (reflective or emissive).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YColorSensor.ESTIMATIONMODEL_REFLECTION</c> or <c>YColorSensor.ESTIMATIONMODEL_EMISSION</c>,
    ///   according to the predictive model used for color estimation (reflective or emissive)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.ESTIMATIONMODEL_INVALID</c>.
    /// </para>
    ///-
    function get_estimationModel():Integer;

    ////
    /// <summary>
    ///   Changes the predictive model to be used for color estimation (reflective or emissive).
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YColorSensor.ESTIMATIONMODEL_REFLECTION</c> or <c>YColorSensor.ESTIMATIONMODEL_EMISSION</c>,
    ///   according to the predictive model to be used for color estimation (reflective or emissive)
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
    function set_estimationModel(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the sensor working mode.
    /// <para>
    ///   In Auto mode, sensor parameters are automatically set based on the selected estimation model.
    ///   In Expert mode, sensor parameters such as gain and integration time are configured manually.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YColorSensor.WORKINGMODE_AUTO</c>, <c>YColorSensor.WORKINGMODE_EXPERT</c> and
    ///   <c>YColorSensor.WORKINGMODE_AUTOGAIN</c> corresponding to the sensor working mode
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.WORKINGMODE_INVALID</c>.
    /// </para>
    ///-
    function get_workingMode():Integer;

    ////
    /// <summary>
    ///   Changes the sensor working mode.
    /// <para>
    ///   In Auto mode, sensor parameters are automatically set based on the selected estimation model.
    ///   In Expert mode, sensor parameters such as gain and integration time are configured manually.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YColorSensor.WORKINGMODE_AUTO</c>, <c>YColorSensor.WORKINGMODE_EXPERT</c> and
    ///   <c>YColorSensor.WORKINGMODE_AUTOGAIN</c> corresponding to the sensor working mode
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
    function set_workingMode(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the amount of current sent to the illumination LEDs, for reflection measures.
    /// <para>
    ///   The value is an integer ranging from 0 (LEDs off) to 254 (LEDs at maximum intensity).
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the amount of current sent to the illumination LEDs, for reflection measures
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.LEDCURRENT_INVALID</c>.
    /// </para>
    ///-
    function get_ledCurrent():LongInt;

    ////
    /// <summary>
    ///   Changes the amount of current sent to the illumination LEDs, for reflection measures.
    /// <para>
    ///   The value is an integer ranging from 0 (LEDs off) to 254 (LEDs at maximum intensity).
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the amount of current sent to the illumination LEDs, for reflection measures
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
    ///   Returns the current sent to the illumination LEDs during the latest calibration.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current sent to the illumination LEDs during the latest calibration
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.LEDCALIBRATION_INVALID</c>.
    /// </para>
    ///-
    function get_ledCalibration():LongInt;

    ////
    /// <summary>
    ///   Remember the LED current sent to the illumination LEDs during a calibration.
    /// <para>
    ///   Thanks to this, the device is able to use the same current when taking measures.
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
    function set_ledCalibration(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the current integration time for spectral measure, in milliseconds.
    /// <para>
    ///   A longer integration time increase the sensitivity for low light conditions,
    ///   but reduces the measure taking rate and may lead to saturation for lighter colors.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current integration time for spectral measure, in milliseconds
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.INTEGRATIONTIME_INVALID</c>.
    /// </para>
    ///-
    function get_integrationTime():LongInt;

    ////
    /// <summary>
    ///   Changes the integration time for spectral measure, in milliseconds.
    /// <para>
    ///   A longer integration time increase the sensitivity for low light conditions,
    ///   but reduces the measure taking rate and may lead to saturation for lighter colors.
    ///   This method can only be used when the sensor is configured in expert mode;
    ///   when running in auto mode, the change is ignored.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the integration time for spectral measure, in milliseconds
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
    ///   Returns the current spectral channel detector gain exponent.
    /// <para>
    ///   For a value <c>n</c> ranging from 0 to 12, the applied gain is 2^(n-1).
    ///   0 corresponds to a gain of 0.5, and 12 corresponds to a gain of 2048.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current spectral channel detector gain exponent
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.GAIN_INVALID</c>.
    /// </para>
    ///-
    function get_gain():LongInt;

    ////
    /// <summary>
    ///   Changes the spectral channel detector gain exponent.
    /// <para>
    ///   For a value <c>n</c> ranging from 0 to 12, the applied gain is 2^(n-1).
    ///   0 corresponds to a gain of 0.5, and 12 corresponds to a gain of 2048.
    ///   This method can only be used when the sensor is configured in expert mode;
    ///   when running in auto mode, the change is ignored.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the spectral channel detector gain exponent
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
    ///   Returns the current autogain parameters of the sensor as a character string.
    /// <para>
    ///   The returned parameter format is: "Min &lt; Channel &lt; Max:Saturation".
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the current autogain parameters of the sensor as a character string
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.AUTOGAIN_INVALID</c>.
    /// </para>
    ///-
    function get_autoGain():string;

    ////
    /// <summary>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string
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
    function set_autoGain(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the current saturation state of the sensor, as an integer.
    /// <para>
    ///   Bit 0 indicates saturation of the analog sensor, which can only
    ///   be corrected by reducing the gain parameters or the luminosity.
    ///   Bit 1 indicates saturation of the digital interface, which can
    ///   be corrected by reducing the integration time or the gain.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current saturation state of the sensor, as an integer
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.SATURATION_INVALID</c>.
    /// </para>
    ///-
    function get_saturation():LongInt;

    ////
    /// <summary>
    ///   Returns the estimated color in RGB color model (0xRRGGBB).
    /// <para>
    ///   The RGB color model describes each color using a combination of 3 components:
    /// </para>
    /// <para>
    ///   - Red (R): the intensity of red, in the 0...255 range
    /// </para>
    /// <para>
    ///   - Green (G): the intensity of green, in the 0...255 range
    /// </para>
    /// <para>
    ///   - Blue (B): the intensity of blue, in the 0...255 range
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the estimated color in RGB color model (0xRRGGBB)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.ESTIMATEDRGB_INVALID</c>.
    /// </para>
    ///-
    function get_estimatedRGB():LongInt;

    ////
    /// <summary>
    ///   Returns the estimated color in HSL color model (0xHHSSLL).
    /// <para>
    ///   The HSL color model describes each color using a combination of 3 components:
    /// </para>
    /// <para>
    ///   - Hue (H): the angle on the color wheel (0-360 degrees), mapped to 0...255
    /// </para>
    /// <para>
    ///   - Saturation (S): the intensity of the color (0-100%), mapped to 0...255
    /// </para>
    /// <para>
    ///   - Lightness (L): the brightness of the color (0-100%), mapped to 0...255
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the estimated color in HSL color model (0xHHSSLL)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.ESTIMATEDHSL_INVALID</c>.
    /// </para>
    ///-
    function get_estimatedHSL():LongInt;

    ////
    /// <summary>
    ///   Returns the estimated color according to the CIE XYZ color model.
    /// <para>
    ///   This color model is based on human vision and light perception, with three components
    ///   represented by real numbers between 0 and 1:
    /// </para>
    /// <para>
    ///   - X: corresponds to a component mixing sensitivity to red and green
    /// </para>
    /// <para>
    ///   - Y: represents luminance (perceived brightness)
    /// </para>
    /// <para>
    ///   - Z: corresponds to sensitivity to blue
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the estimated color according to the CIE XYZ color model
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.ESTIMATEDXYZ_INVALID</c>.
    /// </para>
    ///-
    function get_estimatedXYZ():string;

    ////
    /// <summary>
    ///   Returns the estimated color according to the OkLab color model.
    /// <para>
    ///   OkLab is a perceptual color model that aims to align human color perception with numerical
    ///   values, so that colors that are visually near are also numerically near. Colors are represented
    ///   using three components:
    /// </para>
    /// <para>
    ///   - L: lightness, a real number between 0 and 1
    /// </para>
    /// <para>
    ///   - a: color variations between green and red, between -0.5 and 0.5
    /// </para>
    /// <para>
    ///   - b: color variations between blue and yellow, between -0.5 and 0.5.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the estimated color according to the OkLab color model
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.ESTIMATEDOKLAB_INVALID</c>.
    /// </para>
    ///-
    function get_estimatedOkLab():string;

    ////
    /// <summary>
    ///   Returns the RAL Classic color closest to the estimated color, with a similarity ratio.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the RAL Classic color closest to the estimated color, with a similarity ratio
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.NEARRAL1_INVALID</c>.
    /// </para>
    ///-
    function get_nearRAL1():string;

    ////
    /// <summary>
    ///   Returns the second closest RAL Classic color to the estimated color, with a similarity ratio.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the second closest RAL Classic color to the estimated color, with a similarity ratio
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.NEARRAL2_INVALID</c>.
    /// </para>
    ///-
    function get_nearRAL2():string;

    ////
    /// <summary>
    ///   Returns the third closest RAL Classic color to the estimated color, with a similarity ratio.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the third closest RAL Classic color to the estimated color, with a similarity ratio
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.NEARRAL3_INVALID</c>.
    /// </para>
    ///-
    function get_nearRAL3():string;

    ////
    /// <summary>
    ///   Returns the name of the HTML color closest to the estimated color.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the name of the HTML color closest to the estimated color
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.NEARHTMLCOLOR_INVALID</c>.
    /// </para>
    ///-
    function get_nearHTMLColor():string;

    ////
    /// <summary>
    ///   Returns the index of the basic color typically used to refer to the estimated color (enumerated value).
    /// <para>
    ///   The list of basic colors recognized is:
    /// </para>
    /// <para>
    ///   - 0 - Brown
    /// </para>
    /// <para>
    ///   - 1 - Red
    /// </para>
    /// <para>
    ///   - 2 - Orange
    /// </para>
    /// <para>
    ///   - 3 - Yellow
    /// </para>
    /// <para>
    ///   - 4 - White
    /// </para>
    /// <para>
    ///   - 5 - Gray
    /// </para>
    /// <para>
    ///   - 6 - Black
    /// </para>
    /// <para>
    ///   - 7 - Green
    /// </para>
    /// <para>
    ///   - 8 - Blue
    /// </para>
    /// <para>
    ///   - 9 - Purple
    /// </para>
    /// <para>
    ///   - 10 - Pink
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YColorSensor.NEARSIMPLECOLORINDEX_BROWN</c>,
    ///   <c>YColorSensor.NEARSIMPLECOLORINDEX_RED</c>, <c>YColorSensor.NEARSIMPLECOLORINDEX_ORANGE</c>,
    ///   <c>YColorSensor.NEARSIMPLECOLORINDEX_YELLOW</c>, <c>YColorSensor.NEARSIMPLECOLORINDEX_WHITE</c>,
    ///   <c>YColorSensor.NEARSIMPLECOLORINDEX_GRAY</c>, <c>YColorSensor.NEARSIMPLECOLORINDEX_BLACK</c>,
    ///   <c>YColorSensor.NEARSIMPLECOLORINDEX_GREEN</c>, <c>YColorSensor.NEARSIMPLECOLORINDEX_BLUE</c>,
    ///   <c>YColorSensor.NEARSIMPLECOLORINDEX_PURPLE</c> and <c>YColorSensor.NEARSIMPLECOLORINDEX_PINK</c>
    ///   corresponding to the index of the basic color typically used to refer to the estimated color (enumerated value)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.NEARSIMPLECOLORINDEX_INVALID</c>.
    /// </para>
    ///-
    function get_nearSimpleColorIndex():Integer;

    ////
    /// <summary>
    ///   Returns the name of the basic color typically used to refer to the estimated color.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the name of the basic color typically used to refer to the estimated color
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YColorSensor.NEARSIMPLECOLOR_INVALID</c>.
    /// </para>
    ///-
    function get_nearSimpleColor():string;

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
    ///   Use the method <c>YColorSensor.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YColorSensor</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindColorSensor(func: string):TYColorSensor;

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
    function registerValueCallback(callback: TYColorSensorValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Changes the sensor automatic gain control settings.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// </summary>
    /// <param name="channel">
    ///   reference channel to use for automated gain control.
    /// </param>
    /// <param name="minRaw">
    ///   lower threshold for the measured raw value, below which the gain is
    ///   automatically increased as long as possible.
    /// </param>
    /// <param name="maxRaw">
    ///   high threshold for the measured raw value, over which the gain is
    ///   automatically decreased as long as possible.
    /// </param>
    /// <param name="noSatur">
    ///   enables gain reduction in case of sensor saturation.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the operation completes successfully.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function configureAutoGain(channel: string; minRaw: LongInt; maxRaw: LongInt; noSatur: boolean):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Turns on the built-in illumination LEDs using the same current as used during the latest calibration.
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    /// </summary>
    ///-
    function turnLedOn():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Turns off the built-in illumination LEDs.
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    /// </summary>
    ///-
    function turnLedOff():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of color sensors started using <c>yFirstColorSensor()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned color sensors order.
    ///   If you want to find a specific a color sensor, use <c>ColorSensor.findColorSensor()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YColorSensor</c> object, corresponding to
    ///   a color sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more color sensors to enumerate.
    /// </returns>
    ///-
    function nextColorSensor():TYColorSensor;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstColorSensor():TYColorSensor;
  //--- (end of YColorSensor accessors declaration)
  end;

//--- (YColorSensor functions declaration)
  ////
  /// <summary>
  ///   Retrieves a color sensor for a given identifier.
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
  ///   This function does not require that the color sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YColorSensor.isOnline()</c> to test if the color sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a color sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the color sensor, for instance
  ///   <c>MyDevice.colorSensor</c>.
  /// </param>
  /// <returns>
  ///   a <c>YColorSensor</c> object allowing you to drive the color sensor.
  /// </returns>
  ///-
  function yFindColorSensor(func:string):TYColorSensor;
  ////
  /// <summary>
  ///   Starts the enumeration of color sensors currently accessible.
  /// <para>
  ///   Use the method <c>YColorSensor.nextColorSensor()</c> to iterate on
  ///   next color sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YColorSensor</c> object, corresponding to
  ///   the first color sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstColorSensor():TYColorSensor;

//--- (end of YColorSensor functions declaration)

implementation

//--- (YColorSensor dlldef)
//--- (end of YColorSensor dlldef)

  constructor TYColorSensor.Create(func:string);
    begin
      inherited Create(func);
      _className := 'ColorSensor';
      //--- (YColorSensor accessors initialization)
      _estimationModel := Y_ESTIMATIONMODEL_INVALID;
      _workingMode := Y_WORKINGMODE_INVALID;
      _ledCurrent := Y_LEDCURRENT_INVALID;
      _ledCalibration := Y_LEDCALIBRATION_INVALID;
      _integrationTime := Y_INTEGRATIONTIME_INVALID;
      _gain := Y_GAIN_INVALID;
      _autoGain := Y_AUTOGAIN_INVALID;
      _saturation := Y_SATURATION_INVALID;
      _estimatedRGB := Y_ESTIMATEDRGB_INVALID;
      _estimatedHSL := Y_ESTIMATEDHSL_INVALID;
      _estimatedXYZ := Y_ESTIMATEDXYZ_INVALID;
      _estimatedOkLab := Y_ESTIMATEDOKLAB_INVALID;
      _nearRAL1 := Y_NEARRAL1_INVALID;
      _nearRAL2 := Y_NEARRAL2_INVALID;
      _nearRAL3 := Y_NEARRAL3_INVALID;
      _nearHTMLColor := Y_NEARHTMLCOLOR_INVALID;
      _nearSimpleColorIndex := Y_NEARSIMPLECOLORINDEX_INVALID;
      _nearSimpleColor := Y_NEARSIMPLECOLOR_INVALID;
      _valueCallbackColorSensor := nil;
      //--- (end of YColorSensor accessors initialization)
    end;

//--- (YColorSensor yapiwrapper)
//--- (end of YColorSensor yapiwrapper)

//--- (YColorSensor implementation)
{$HINTS OFF}
  function TYColorSensor._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'estimationModel') then
        begin
          _estimationModel := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'workingMode') then
        begin
          _workingMode := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'ledCurrent') then
        begin
          _ledCurrent := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'ledCalibration') then
        begin
          _ledCalibration := integer(member^.ivalue);
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
      if (member^.name = 'autoGain') then
        begin
          _autoGain := string(member^.svalue);
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
      if (member^.name = 'nearRAL1') then
        begin
          _nearRAL1 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'nearRAL2') then
        begin
          _nearRAL2 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'nearRAL3') then
        begin
          _nearRAL3 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'nearHTMLColor') then
        begin
          _nearHTMLColor := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'nearSimpleColorIndex') then
        begin
          _nearSimpleColorIndex := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'nearSimpleColor') then
        begin
          _nearSimpleColor := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYColorSensor.get_estimationModel():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ESTIMATIONMODEL_INVALID;
              exit;
            end;
        end;
      res := self._estimationModel;
      result := res;
      exit;
    end;


  function TYColorSensor.set_estimationModel(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('estimationModel',rest_val);
    end;

  function TYColorSensor.get_workingMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_WORKINGMODE_INVALID;
              exit;
            end;
        end;
      res := self._workingMode;
      result := res;
      exit;
    end;


  function TYColorSensor.set_workingMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('workingMode',rest_val);
    end;

  function TYColorSensor.get_ledCurrent():LongInt;
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


  function TYColorSensor.set_ledCurrent(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('ledCurrent',rest_val);
    end;

  function TYColorSensor.get_ledCalibration():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LEDCALIBRATION_INVALID;
              exit;
            end;
        end;
      res := self._ledCalibration;
      result := res;
      exit;
    end;


  function TYColorSensor.set_ledCalibration(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('ledCalibration',rest_val);
    end;

  function TYColorSensor.get_integrationTime():LongInt;
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


  function TYColorSensor.set_integrationTime(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('integrationTime',rest_val);
    end;

  function TYColorSensor.get_gain():LongInt;
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


  function TYColorSensor.set_gain(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('gain',rest_val);
    end;

  function TYColorSensor.get_autoGain():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_AUTOGAIN_INVALID;
              exit;
            end;
        end;
      res := self._autoGain;
      result := res;
      exit;
    end;


  function TYColorSensor.set_autoGain(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('autoGain',rest_val);
    end;

  function TYColorSensor.get_saturation():LongInt;
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


  function TYColorSensor.get_estimatedRGB():LongInt;
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


  function TYColorSensor.get_estimatedHSL():LongInt;
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


  function TYColorSensor.get_estimatedXYZ():string;
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


  function TYColorSensor.get_estimatedOkLab():string;
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


  function TYColorSensor.get_nearRAL1():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NEARRAL1_INVALID;
              exit;
            end;
        end;
      res := self._nearRAL1;
      result := res;
      exit;
    end;


  function TYColorSensor.get_nearRAL2():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NEARRAL2_INVALID;
              exit;
            end;
        end;
      res := self._nearRAL2;
      result := res;
      exit;
    end;


  function TYColorSensor.get_nearRAL3():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NEARRAL3_INVALID;
              exit;
            end;
        end;
      res := self._nearRAL3;
      result := res;
      exit;
    end;


  function TYColorSensor.get_nearHTMLColor():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NEARHTMLCOLOR_INVALID;
              exit;
            end;
        end;
      res := self._nearHTMLColor;
      result := res;
      exit;
    end;


  function TYColorSensor.get_nearSimpleColorIndex():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NEARSIMPLECOLORINDEX_INVALID;
              exit;
            end;
        end;
      res := self._nearSimpleColorIndex;
      result := res;
      exit;
    end;


  function TYColorSensor.get_nearSimpleColor():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NEARSIMPLECOLOR_INVALID;
              exit;
            end;
        end;
      res := self._nearSimpleColor;
      result := res;
      exit;
    end;


  class function TYColorSensor.FindColorSensor(func: string):TYColorSensor;
    var
      obj : TYColorSensor;
    begin
      obj := TYColorSensor(TYFunction._FindFromCache('ColorSensor', func));
      if (obj = nil) then
        begin
          obj :=  TYColorSensor.create(func);
          TYFunction._AddToCache('ColorSensor', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYColorSensor.registerValueCallback(callback: TYColorSensorValueCallback):LongInt;
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
      self._valueCallbackColorSensor := callback;
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


  function TYColorSensor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackColorSensor) <> nil) then
        begin
          self._valueCallbackColorSensor(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYColorSensor.configureAutoGain(channel: string; minRaw: LongInt; maxRaw: LongInt; noSatur: boolean):LongInt;
    var
      opt : string;
    begin
      if noSatur then
        begin
          opt := 'nosat';
        end
      else
        begin
          opt := '';
        end;

      result := self.set_autoGain(''+inttostr(minRaw)+' < '+channel+' < '+inttostr(maxRaw)+':'+opt);
      exit;
    end;


  function TYColorSensor.turnLedOn():LongInt;
    begin
      result := self.set_ledCurrent(self.get_ledCalibration);
      exit;
    end;


  function TYColorSensor.turnLedOff():LongInt;
    begin
      result := self.set_ledCurrent(0);
      exit;
    end;


  function TYColorSensor.nextColorSensor(): TYColorSensor;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextColorSensor := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextColorSensor := nil;
          exit;
        end;
      nextColorSensor := TYColorSensor.FindColorSensor(hwid);
    end;

  class function TYColorSensor.FirstColorSensor(): TYColorSensor;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('ColorSensor', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYColorSensor.FindColorSensor(serial+'.'+funcId);
    end;

//--- (end of YColorSensor implementation)

//--- (YColorSensor functions)

  function yFindColorSensor(func:string): TYColorSensor;
    begin
      result := TYColorSensor.FindColorSensor(func);
    end;

  function yFirstColorSensor(): TYColorSensor;
    begin
      result := TYColorSensor.FirstColorSensor();
    end;

  procedure _ColorSensorCleanup();
    begin
    end;

//--- (end of YColorSensor functions)

initialization
  //--- (YColorSensor initialization)
  //--- (end of YColorSensor initialization)

finalization
  //--- (YColorSensor cleanup)
  _ColorSensorCleanup();
  //--- (end of YColorSensor cleanup)

end.
