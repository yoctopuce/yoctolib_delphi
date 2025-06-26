{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindRefFrame(), the high-level API for RefFrame functions
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


unit yocto_refframe;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YRefFrame definitions)
type  TYMOUNTPOSITION = (Y_MOUNTPOSITION_BOTTOM,Y_MOUNTPOSITION_TOP,Y_MOUNTPOSITION_FRONT,Y_MOUNTPOSITION_REAR,Y_MOUNTPOSITION_RIGHT,Y_MOUNTPOSITION_LEFT,Y_MOUNTPOSITION_INVALID);
type  TYMOUNTORIENTATION = (Y_MOUNTORIENTATION_TWELVE,Y_MOUNTORIENTATION_THREE,Y_MOUNTORIENTATION_SIX,Y_MOUNTORIENTATION_NINE,Y_MOUNTORIENTATION_INVALID);

const Y_MOUNTPOS_INVALID              = YAPI_INVALID_UINT;
const Y_BEARING_INVALID               = YAPI_INVALID_DOUBLE;
const Y_CALIBRATIONPARAM_INVALID      = YAPI_INVALID_STRING;
const Y_FUSIONMODE_NDOF = 0;
const Y_FUSIONMODE_NDOF_FMC_OFF = 1;
const Y_FUSIONMODE_M4G = 2;
const Y_FUSIONMODE_COMPASS = 3;
const Y_FUSIONMODE_IMU = 4;
const Y_FUSIONMODE_INCLIN_90DEG_1G8 = 5;
const Y_FUSIONMODE_INCLIN_90DEG_3G6 = 6;
const Y_FUSIONMODE_INCLIN_10DEG = 7;
const Y_FUSIONMODE_INVALID = -1;

//--- (end of YRefFrame definitions)

//--- (YRefFrame yapiwrapper declaration)
//--- (end of YRefFrame yapiwrapper declaration)

type

  TYRefFrame = class;
  //--- (YRefFrame class start)
  TYRefFrameValueCallback = procedure(func: TYRefFrame; value:string);
  TYRefFrameTimedReportCallback = procedure(func: TYRefFrame; value:TYMeasure);

  ////
  /// <summary>
  ///   TYRefFrame Class: 3D reference frame configuration interface, available for instance in the
  ///   Yocto-3D-V2 or the Yocto-Inclinometer
  /// <para>
  ///   The <c>YRefFrame</c> class is used to set up the base orientation of the Yoctopuce inertial
  ///   sensors. Thanks to this, orientation functions relative to the earth surface plane
  ///   can use the proper reference frame. For some devices, the class also implements a
  ///   tridimensional sensor calibration process, which can compensate for local variations
  ///   of standard gravity and improve the precision of the tilt sensors.
  /// </para>
  /// </summary>
  ///-
  TYRefFrame=class(TYFunction)
  //--- (end of YRefFrame class start)
  protected
  //--- (YRefFrame declaration)
    // Attributes (function value cache)
    _mountPos                 : LongInt;
    _bearing                  : double;
    _calibrationParam         : string;
    _fusionMode               : Integer;
    _valueCallbackRefFrame    : TYRefFrameValueCallback;
    _calibV2                  : boolean;
    _calibStage               : LongInt;
    _calibStageHint           : string;
    _calibStageProgress       : LongInt;
    _calibProgress            : LongInt;
    _calibLogMsg              : string;
    _calibSavedParams         : string;
    _calibCount               : LongInt;
    _calibInternalPos         : LongInt;
    _calibPrevTick            : LongInt;
    _calibOrient              : TLongIntArray;
    _calibDataAccX            : TDoubleArray;
    _calibDataAccY            : TDoubleArray;
    _calibDataAccZ            : TDoubleArray;
    _calibDataAcc             : TDoubleArray;
    _calibAccXOfs             : double;
    _calibAccYOfs             : double;
    _calibAccZOfs             : double;
    _calibAccXScale           : double;
    _calibAccYScale           : double;
    _calibAccZScale           : double;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YRefFrame declaration)

  public
    //--- (YRefFrame accessors declaration)
    constructor Create(func:string);

    function get_mountPos():LongInt;

    function set_mountPos(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Changes the reference bearing used by the compass.
    /// <para>
    ///   The relative bearing
    ///   indicated by the compass is the difference between the measured magnetic
    ///   heading and the reference bearing indicated here.
    /// </para>
    /// <para>
    ///   For instance, if you set up as reference bearing the value of the earth
    ///   magnetic declination, the compass will provide the orientation relative
    ///   to the geographic North.
    /// </para>
    /// <para>
    ///   Similarly, when the sensor is not mounted along the standard directions
    ///   because it has an additional yaw angle, you can set this angle in the reference
    ///   bearing so that the compass provides the expected natural direction.
    /// </para>
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the reference bearing used by the compass
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
    function set_bearing(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the reference bearing used by the compass.
    /// <para>
    ///   The relative bearing
    ///   indicated by the compass is the difference between the measured magnetic
    ///   heading and the reference bearing indicated here.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the reference bearing used by the compass
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRefFrame.BEARING_INVALID</c>.
    /// </para>
    ///-
    function get_bearing():double;

    function get_calibrationParam():string;

    function set_calibrationParam(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the sensor fusion mode.
    /// <para>
    ///   Note that available sensor fusion modes depend on the sensor type.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YRefFrame.FUSIONMODE_NDOF</c>, <c>YRefFrame.FUSIONMODE_NDOF_FMC_OFF</c>,
    ///   <c>YRefFrame.FUSIONMODE_M4G</c>, <c>YRefFrame.FUSIONMODE_COMPASS</c>,
    ///   <c>YRefFrame.FUSIONMODE_IMU</c>, <c>YRefFrame.FUSIONMODE_INCLIN_90DEG_1G8</c>,
    ///   <c>YRefFrame.FUSIONMODE_INCLIN_90DEG_3G6</c> and <c>YRefFrame.FUSIONMODE_INCLIN_10DEG</c>
    ///   corresponding to the sensor fusion mode
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRefFrame.FUSIONMODE_INVALID</c>.
    /// </para>
    ///-
    function get_fusionMode():Integer;

    ////
    /// <summary>
    ///   Change the sensor fusion mode.
    /// <para>
    ///   Note that available sensor fusion modes depend on the sensor type.
    ///   Remember to call the matching module <c>saveToFlash()</c> method to save the setting permanently.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YRefFrame.FUSIONMODE_NDOF</c>, <c>YRefFrame.FUSIONMODE_NDOF_FMC_OFF</c>,
    ///   <c>YRefFrame.FUSIONMODE_M4G</c>, <c>YRefFrame.FUSIONMODE_COMPASS</c>,
    ///   <c>YRefFrame.FUSIONMODE_IMU</c>, <c>YRefFrame.FUSIONMODE_INCLIN_90DEG_1G8</c>,
    ///   <c>YRefFrame.FUSIONMODE_INCLIN_90DEG_3G6</c> and <c>YRefFrame.FUSIONMODE_INCLIN_10DEG</c>
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
    function set_fusionMode(newval:Integer):integer;

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
    ///   Use the method <c>YRefFrame.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YRefFrame</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindRefFrame(func: string):TYRefFrame;

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
    function registerValueCallback(callback: TYRefFrameValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Returns the installation position of the device, as configured
    ///   in order to define the reference frame for the compass and the
    ///   pitch/roll tilt sensors.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among the <c>YRefFrame.MOUNTPOSITION</c> enumeration
    ///   (<c>YRefFrame.MOUNTPOSITION_BOTTOM</c>,  <c>YRefFrame.MOUNTPOSITION_TOP</c>,
    ///   <c>YRefFrame.MOUNTPOSITION_FRONT</c>,    <c>YRefFrame.MOUNTPOSITION_RIGHT</c>,
    ///   <c>YRefFrame.MOUNTPOSITION_REAR</c>,     <c>YRefFrame.MOUNTPOSITION_LEFT</c>),
    ///   corresponding to the installation in a box, on one of the six faces.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns Y_MOUNTPOSITION_INVALID.
    /// </para>
    ///-
    function get_mountPosition():TYMOUNTPOSITION; overload; virtual;

    ////
    /// <summary>
    ///   Returns the installation orientation of the device, as configured
    ///   in order to define the reference frame for the compass and the
    ///   pitch/roll tilt sensors.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among the enumeration <c>YRefFrame.MOUNTORIENTATION</c>
    ///   (<c>YRefFrame.MOUNTORIENTATION_TWELVE</c>, <c>YRefFrame.MOUNTORIENTATION_THREE</c>,
    ///   <c>YRefFrame.MOUNTORIENTATION_SIX</c>,     <c>YRefFrame.MOUNTORIENTATION_NINE</c>)
    ///   corresponding to the orientation of the "X" arrow on the device,
    ///   as on a clock dial seen from an observer in the center of the box.
    ///   On the bottom face, the 12H orientation points to the front, while
    ///   on the top face, the 12H orientation points to the rear.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns Y_MOUNTORIENTATION_INVALID.
    /// </para>
    ///-
    function get_mountOrientation():TYMOUNTORIENTATION; overload; virtual;

    ////
    /// <summary>
    ///   Changes the compass and tilt sensor frame of reference.
    /// <para>
    ///   The magnetic compass
    ///   and the tilt sensors (pitch and roll) naturally work in the plane
    ///   parallel to the earth surface. In case the device is not installed upright
    ///   and horizontally, you must select its reference orientation (parallel to
    ///   the earth surface) so that the measures are made relative to this position.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="position">
    ///   a value among the <c>YRefFrame.MOUNTPOSITION</c> enumeration
    ///   (<c>YRefFrame.MOUNTPOSITION_BOTTOM</c>,  <c>YRefFrame.MOUNTPOSITION_TOP</c>,
    ///   <c>YRefFrame.MOUNTPOSITION_FRONT</c>,    <c>YRefFrame.MOUNTPOSITION_RIGHT</c>,
    ///   <c>YRefFrame.MOUNTPOSITION_REAR</c>,     <c>YRefFrame.MOUNTPOSITION_LEFT</c>),
    ///   corresponding to the installation in a box, on one of the six faces.
    /// </param>
    /// <param name="orientation">
    ///   a value among the enumeration <c>YRefFrame.MOUNTORIENTATION</c>
    ///   (<c>YRefFrame.MOUNTORIENTATION_TWELVE</c>, <c>YRefFrame.MOUNTORIENTATION_THREE</c>,
    ///   <c>YRefFrame.MOUNTORIENTATION_SIX</c>,     <c>YRefFrame.MOUNTORIENTATION_NINE</c>)
    ///   corresponding to the orientation of the "X" arrow on the device,
    ///   as on a clock dial seen from an observer in the center of the box.
    ///   On the bottom face, the 12H orientation points to the front, while
    ///   on the top face, the 12H orientation points to the rear.
    /// </param>
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_mountPosition(position: TYMOUNTPOSITION; orientation: TYMOUNTORIENTATION):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the 3D sensor calibration state (Yocto-3D-V2 only).
    /// <para>
    ///   This function returns
    ///   an integer representing the calibration state of the 3 inertial sensors of
    ///   the BNO055 chip, found in the Yocto-3D-V2. Hundredths show the calibration state
    ///   of the accelerometer, tenths show the calibration state of the magnetometer while
    ///   units show the calibration state of the gyroscope. For each sensor, the value 0
    ///   means no calibration and the value 3 means full calibration.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer representing the calibration state of Yocto-3D-V2:
    ///   333 when fully calibrated, 0 when not calibrated at all.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    ///   For the Yocto-3D (V1), this function always return -3 (unsupported function).
    /// </para>
    ///-
    function get_calibrationState():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns estimated quality of the orientation (Yocto-3D-V2 only).
    /// <para>
    ///   This function returns
    ///   an integer between 0 and 3 representing the degree of confidence of the position
    ///   estimate. When the value is 3, the estimation is reliable. Below 3, one should
    ///   expect sudden corrections, in particular for heading (<c>compass</c> function).
    ///   The most frequent causes for values below 3 are magnetic interferences, and
    ///   accelerations or rotations beyond the sensor range.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer between 0 and 3 (3 when the measure is reliable)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    ///   For the Yocto-3D (V1), this function always return -3 (unsupported function).
    /// </para>
    ///-
    function get_measureQuality():LongInt; overload; virtual;

    function _calibSort(start: LongInt; stopidx: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Initiates the sensors tridimensional calibration process.
    /// <para>
    ///   This calibration is used at low level for inertial position estimation
    ///   and to enhance the precision of the tilt sensors.
    /// </para>
    /// <para>
    ///   After calling this method, the device should be moved according to the
    ///   instructions provided by method <c>get_3DCalibrationHint</c>,
    ///   and <c>more3DCalibration</c> should be invoked about 5 times per second.
    ///   The calibration procedure is completed when the method
    ///   <c>get_3DCalibrationProgress</c> returns 100. At this point,
    ///   the computed calibration parameters can be applied using method
    ///   <c>save3DCalibration</c>. The calibration process can be cancelled
    ///   at any time using method <c>cancel3DCalibration</c>.
    /// </para>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    /// </summary>
    ///-
    function start3DCalibration():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Continues the sensors tridimensional calibration process previously
    ///   initiated using method <c>start3DCalibration</c>.
    /// <para>
    ///   This method should be called approximately 5 times per second, while
    ///   positioning the device according to the instructions provided by method
    ///   <c>get_3DCalibrationHint</c>. Note that the instructions change during
    ///   the calibration process.
    /// </para>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    /// </summary>
    ///-
    function more3DCalibration():LongInt; overload; virtual;

    function more3DCalibrationV1():LongInt; overload; virtual;

    function more3DCalibrationV2():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns instructions to proceed to the tridimensional calibration initiated with
    ///   method <c>start3DCalibration</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a character string.
    /// </returns>
    ///-
    function get_3DCalibrationHint():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the global process indicator for the tridimensional calibration
    ///   initiated with method <c>start3DCalibration</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer between 0 (not started) and 100 (stage completed).
    /// </returns>
    ///-
    function get_3DCalibrationProgress():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns index of the current stage of the calibration
    ///   initiated with method <c>start3DCalibration</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer, growing each time a calibration stage is completed.
    /// </returns>
    ///-
    function get_3DCalibrationStage():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the process indicator for the current stage of the calibration
    ///   initiated with method <c>start3DCalibration</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer between 0 (not started) and 100 (stage completed).
    /// </returns>
    ///-
    function get_3DCalibrationStageProgress():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the latest log message from the calibration process.
    /// <para>
    ///   When no new message is available, returns an empty string.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a character string.
    /// </returns>
    ///-
    function get_3DCalibrationLogMsg():string; overload; virtual;

    ////
    /// <summary>
    ///   Applies the sensors tridimensional calibration parameters that have just been computed.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>  method of the module if the changes
    ///   must be kept when the device is restarted.
    /// </para>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    /// </summary>
    ///-
    function save3DCalibration():LongInt; overload; virtual;

    function save3DCalibrationV1():LongInt; overload; virtual;

    function save3DCalibrationV2():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Aborts the sensors tridimensional calibration process et restores normal settings.
    /// <para>
    /// </para>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    /// </summary>
    ///-
    function cancel3DCalibration():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of reference frames started using <c>yFirstRefFrame()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned reference frames order.
    ///   If you want to find a specific a reference frame, use <c>RefFrame.findRefFrame()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YRefFrame</c> object, corresponding to
    ///   a reference frame currently online, or a <c>NIL</c> pointer
    ///   if there are no more reference frames to enumerate.
    /// </returns>
    ///-
    function nextRefFrame():TYRefFrame;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstRefFrame():TYRefFrame;
  //--- (end of YRefFrame accessors declaration)
  end;

//--- (YRefFrame functions declaration)
  ////
  /// <summary>
  ///   Retrieves a reference frame for a given identifier.
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
  ///   This function does not require that the reference frame is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YRefFrame.isOnline()</c> to test if the reference frame is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a reference frame by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the reference frame, for instance
  ///   <c>Y3DMK002.refFrame</c>.
  /// </param>
  /// <returns>
  ///   a <c>YRefFrame</c> object allowing you to drive the reference frame.
  /// </returns>
  ///-
  function yFindRefFrame(func:string):TYRefFrame;
  ////
  /// <summary>
  ///   Starts the enumeration of reference frames currently accessible.
  /// <para>
  ///   Use the method <c>YRefFrame.nextRefFrame()</c> to iterate on
  ///   next reference frames.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YRefFrame</c> object, corresponding to
  ///   the first reference frame currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstRefFrame():TYRefFrame;

//--- (end of YRefFrame functions declaration)

implementation

//--- (YRefFrame dlldef)
//--- (end of YRefFrame dlldef)

  constructor TYRefFrame.Create(func:string);
    begin
      inherited Create(func);
      _className := 'RefFrame';
      //--- (YRefFrame accessors initialization)
      _mountPos := Y_MOUNTPOS_INVALID;
      _bearing := Y_BEARING_INVALID;
      _calibrationParam := Y_CALIBRATIONPARAM_INVALID;
      _fusionMode := Y_FUSIONMODE_INVALID;
      _valueCallbackRefFrame := nil;
      _calibStage := 0;
      _calibStageProgress := 0;
      _calibProgress := 0;
      _calibCount := 0;
      _calibInternalPos := 0;
      _calibPrevTick := 0;
      _calibAccXOfs := 0;
      _calibAccYOfs := 0;
      _calibAccZOfs := 0;
      _calibAccXScale := 0;
      _calibAccYScale := 0;
      _calibAccZScale := 0;
      //--- (end of YRefFrame accessors initialization)
    end;

//--- (YRefFrame yapiwrapper)
//--- (end of YRefFrame yapiwrapper)

//--- (YRefFrame implementation)
{$HINTS OFF}
  function TYRefFrame._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'mountPos') then
        begin
          _mountPos := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bearing') then
        begin
          _bearing := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'calibrationParam') then
        begin
          _calibrationParam := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'fusionMode') then
        begin
          _fusionMode := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYRefFrame.get_mountPos():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MOUNTPOS_INVALID;
              exit;
            end;
        end;
      res := self._mountPos;
      result := res;
      exit;
    end;


  function TYRefFrame.set_mountPos(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('mountPos',rest_val);
    end;

  function TYRefFrame.set_bearing(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('bearing',rest_val);
    end;

  function TYRefFrame.get_bearing():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BEARING_INVALID;
              exit;
            end;
        end;
      res := self._bearing;
      result := res;
      exit;
    end;


  function TYRefFrame.get_calibrationParam():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CALIBRATIONPARAM_INVALID;
              exit;
            end;
        end;
      res := self._calibrationParam;
      result := res;
      exit;
    end;


  function TYRefFrame.set_calibrationParam(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('calibrationParam',rest_val);
    end;

  function TYRefFrame.get_fusionMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_FUSIONMODE_INVALID;
              exit;
            end;
        end;
      res := self._fusionMode;
      result := res;
      exit;
    end;


  function TYRefFrame.set_fusionMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('fusionMode',rest_val);
    end;

  class function TYRefFrame.FindRefFrame(func: string):TYRefFrame;
    var
      obj : TYRefFrame;
    begin
      obj := TYRefFrame(TYFunction._FindFromCache('RefFrame', func));
      if obj = nil then
        begin
          obj :=  TYRefFrame.create(func);
          TYFunction._AddToCache('RefFrame', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYRefFrame.registerValueCallback(callback: TYRefFrameValueCallback):LongInt;
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
      self._valueCallbackRefFrame := callback;
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


  function TYRefFrame._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackRefFrame) <> nil) then
        begin
          self._valueCallbackRefFrame(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYRefFrame.get_mountPosition():TYMOUNTPOSITION;
    var
      position : LongInt;
    begin
      position := self.get_mountPos;
      if position < 0 then
        begin
          result := Y_MOUNTPOSITION_INVALID;
          exit;
        end;
      return TYMOUNTPOSITION(((position) shr 2));
    end;


  function TYRefFrame.get_mountOrientation():TYMOUNTORIENTATION;
    var
      position : LongInt;
    begin
      position := self.get_mountPos;
      if position < 0 then
        begin
          result := Y_MOUNTORIENTATION_INVALID;
          exit;
        end;
      return TYMOUNTORIENTATION(((position) and 3));
    end;


  function TYRefFrame.set_mountPosition(position: TYMOUNTPOSITION; orientation: TYMOUNTORIENTATION):LongInt;
    var
      mixedPos : LongInt;
    begin
      mixedPos := ((position) shl 2) + orientation;
      return self.set_mountPos(mixedPos);
    end;


  function TYRefFrame.get_calibrationState():LongInt;
    var
      calibParam : string;
      iCalib : TLongIntArray;
      caltyp : LongInt;
      res : LongInt;
    begin
      calibParam := self.get_calibrationParam;
      iCalib := _decodeFloats(calibParam);
      caltyp := (iCalib[0] div 1000);
      if caltyp <> 33 then
        begin
          result := YAPI_NOT_SUPPORTED;
          exit;
        end;
      res := (iCalib[1] div 1000);
      result := res;
      exit;
    end;


  function TYRefFrame.get_measureQuality():LongInt;
    var
      calibParam : string;
      iCalib : TLongIntArray;
      caltyp : LongInt;
      res : LongInt;
    begin
      calibParam := self.get_calibrationParam;
      iCalib := _decodeFloats(calibParam);
      caltyp := (iCalib[0] div 1000);
      if caltyp <> 33 then
        begin
          result := YAPI_NOT_SUPPORTED;
          exit;
        end;
      res := (iCalib[2] div 1000);
      result := res;
      exit;
    end;


  function TYRefFrame._calibSort(start: LongInt; stopidx: LongInt):LongInt;
    var
      idx : LongInt;
      changed : LongInt;
      a : double;
      b : double;
      xa : double;
      xb : double;
    begin
      changed := 1;
      while changed > 0 do
        begin
          changed := 0;
          a := self._calibDataAcc[start];
          idx := start + 1;
          while idx < stopidx do
            begin
              b := self._calibDataAcc[idx];
              if a > b then
                begin
                  self._calibDataAcc[idx-1] := b;
                  self._calibDataAcc[idx] := a;
                  xa := self._calibDataAccX[idx-1];
                  xb := self._calibDataAccX[idx];
                  self._calibDataAccX[idx-1] := xb;
                  self._calibDataAccX[idx] := xa;
                  xa := self._calibDataAccY[idx-1];
                  xb := self._calibDataAccY[idx];
                  self._calibDataAccY[idx-1] := xb;
                  self._calibDataAccY[idx] := xa;
                  xa := self._calibDataAccZ[idx-1];
                  xb := self._calibDataAccZ[idx];
                  self._calibDataAccZ[idx-1] := xb;
                  self._calibDataAccZ[idx] := xa;
                  changed := changed + 1;
                end
              else
                begin
                  a := b;
                end;
              idx := idx + 1;
            end;
        end;
      result := 0;
      exit;
    end;


  function TYRefFrame.start3DCalibration():LongInt;
    var
      _calibOrient_pos : LongInt;
      _calibDataAccX_pos : LongInt;
      _calibDataAccY_pos : LongInt;
      _calibDataAccZ_pos : LongInt;
      _calibDataAcc_pos : LongInt;
    begin
      if not(self.isOnline) then
        begin
          result := YAPI_DEVICE_NOT_FOUND;
          exit;
        end;
      if self._calibStage <> 0 then
        begin
          self.cancel3DCalibration;
        end;
      self._calibSavedParams := self.get_calibrationParam;
      self._calibV2 := (_atoi(self._calibSavedParams) = 33);
      self.set_calibrationParam('0');
      self._calibCount := 50;
      self._calibStage := 1;
      self._calibStageHint := 'Set down the device on a steady horizontal surface';
      self._calibStageProgress := 0;
      self._calibProgress := 1;
      self._calibInternalPos := 0;
      self._calibPrevTick := ((yGetTickCount) and ($07FFFFFFF));
      _calibOrient_pos := 0;
      SetLength(self._calibOrient, 6);;
      _calibDataAccX_pos := 0;
      SetLength(self._calibDataAccX, 6 * self._calibCount);;
      _calibDataAccY_pos := 0;
      SetLength(self._calibDataAccY, 6 * self._calibCount);;
      _calibDataAccZ_pos := 0;
      SetLength(self._calibDataAccZ, 6 * self._calibCount);;
      _calibDataAcc_pos := 0;
      SetLength(self._calibDataAcc, 6 * self._calibCount);;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYRefFrame.more3DCalibration():LongInt;
    begin
      if self._calibV2 then
        begin
          result := self.more3DCalibrationV2;
          exit;
        end;
      result := self.more3DCalibrationV1;
      exit;
    end;


  function TYRefFrame.more3DCalibrationV1():LongInt;
    var
      currTick : LongInt;
      jsonData : TByteArray;
      xVal : double;
      yVal : double;
      zVal : double;
      xSq : double;
      ySq : double;
      zSq : double;
      norm : double;
      orient : LongInt;
      idx : LongInt;
      intpos : LongInt;
      err : LongInt;
    begin
      if self._calibStage = 0 then
        begin
          result := YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if self._calibProgress = 100 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      // make sure we leave at least 160 ms between samples
      currTick :=  ((yGetTickCount) and ($07FFFFFFF));
      if ((currTick - self._calibPrevTick) and ($07FFFFFFF)) < 160 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      // load current accelerometer values, make sure we are on a straight angle
      // (default timeout to 0,5 sec without reading measure when out of range)
      self._calibStageHint := 'Set down the device on a steady horizontal surface';
      self._calibPrevTick := ((currTick + 500) and ($07FFFFFFF));
      jsonData := self._download('api/accelerometer.json');
      xVal := _atoi(self._json_get_key(jsonData, 'xValue')) / 65536.0;
      yVal := _atoi(self._json_get_key(jsonData, 'yValue')) / 65536.0;
      zVal := _atoi(self._json_get_key(jsonData, 'zValue')) / 65536.0;
      xSq := xVal * xVal;
      if xSq >= 0.04 and xSq < 0.64 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      if xSq >= 1.44 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      ySq := yVal * yVal;
      if ySq >= 0.04 and ySq < 0.64 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      if ySq >= 1.44 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      zSq := zVal * zVal;
      if zSq >= 0.04 and zSq < 0.64 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      if zSq >= 1.44 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      norm := Sqrt(xSq + ySq + zSq);
      if norm < 0.8 or norm > 1.2 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      self._calibPrevTick := currTick;
      // Determine the device orientation index
      orient := 0;
      if zSq > 0.5 then
        begin
          if zVal > 0 then
            begin
              orient := 0;
            end
          else
            begin
              orient := 1;
            end;
        end;
      if xSq > 0.5 then
        begin
          if xVal > 0 then
            begin
              orient := 2;
            end
          else
            begin
              orient := 3;
            end;
        end;
      if ySq > 0.5 then
        begin
          if yVal > 0 then
            begin
              orient := 4;
            end
          else
            begin
              orient := 5;
            end;
        end;
      // Discard measures that are not in the proper orientation
      if self._calibStageProgress = 0 then
        begin
          // New stage, check that this orientation is not yet done
          idx := 0;
          err := 0;
          while idx + 1 < self._calibStage do
            begin
              if self._calibOrient[idx] = orient then
                begin
                  err := 1;
                end;
              idx := idx + 1;
            end;
          if err <> 0 then
            begin
              self._calibStageHint := 'Turn the device on another face';
              result := YAPI_SUCCESS;
              exit;
            end;
          self._calibOrient[_calibOrient_pos] := orient;
          inc(_calibOrient_pos);
        end
      else
        begin
          // Make sure device is not turned before stage is completed
          if orient <> self._calibOrient[self._calibStage-1] then
            begin
              self._calibStageHint := 'Not yet done, please move back to the previous face';
              result := YAPI_SUCCESS;
              exit;
            end;
        end;
      // Save measure
      self._calibStageHint := 'calibrating..';
      self._calibDataAccX[_calibDataAccX_pos] := xVal;
      inc(_calibDataAccX_pos);
      self._calibDataAccY[_calibDataAccY_pos] := yVal;
      inc(_calibDataAccY_pos);
      self._calibDataAccZ[_calibDataAccZ_pos] := zVal;
      inc(_calibDataAccZ_pos);
      self._calibDataAcc[_calibDataAcc_pos] := norm;
      inc(_calibDataAcc_pos);
      self._calibInternalPos := self._calibInternalPos + 1;
      self._calibProgress := 1 + 16 * (self._calibStage - 1) + ((16 * self._calibInternalPos) div self._calibCount);
      if self._calibInternalPos < self._calibCount then
        begin
          self._calibStageProgress := 1 + ((99 * self._calibInternalPos) div self._calibCount);
          result := YAPI_SUCCESS;
          exit;
        end;
      // Stage done, compute preliminary result
      intpos := (self._calibStage - 1) * self._calibCount;
      self._calibSort(intpos, intpos + self._calibCount);
      intpos := intpos + (self._calibCount div 2);
      self._calibLogMsg := 'Stage '+inttostr(self._calibStage)+': median is '+inttostr(round(1000*self._calibDataAccX[intpos]))+','+inttostr(round(1000*self._calibDataAccY[intpos]))+','+inttostr(round(1000*self._calibDataAccZ[intpos]));
      // move to next stage
      self._calibStage := self._calibStage + 1;
      if self._calibStage < 7 then
        begin
          self._calibStageHint := 'Turn the device on another face';
          self._calibPrevTick := ((currTick + 500) and ($07FFFFFFF));
          self._calibStageProgress := 0;
          self._calibInternalPos := 0;
          result := YAPI_SUCCESS;
          exit;
        end;
      // Data collection completed, compute accelerometer shift
      xVal := 0;
      yVal := 0;
      zVal := 0;
      idx := 0;
      while idx < 6 do
        begin
          intpos := idx * self._calibCount + (self._calibCount div 2);
          orient := self._calibOrient[idx];
          if orient = 0 or orient = 1 then
            begin
              zVal := zVal + self._calibDataAccZ[intpos];
            end;
          if orient = 2 or orient = 3 then
            begin
              xVal := xVal + self._calibDataAccX[intpos];
            end;
          if orient = 4 or orient = 5 then
            begin
              yVal := yVal + self._calibDataAccY[intpos];
            end;
          idx := idx + 1;
        end;
      self._calibAccXOfs := xVal / 2.0;
      self._calibAccYOfs := yVal / 2.0;
      self._calibAccZOfs := zVal / 2.0;
      // Recompute all norms, taking into account the computed shift, and re-sort
      intpos := 0;
      while intpos < length(self._calibDataAcc) do
        begin
          xVal := self._calibDataAccX[intpos] - self._calibAccXOfs;
          yVal := self._calibDataAccY[intpos] - self._calibAccYOfs;
          zVal := self._calibDataAccZ[intpos] - self._calibAccZOfs;
          norm := Sqrt(xVal * xVal + yVal * yVal + zVal * zVal);
          self._calibDataAcc[intpos] := norm;
          intpos := intpos + 1;
        end;
      idx := 0;
      while idx < 6 do
        begin
          intpos := idx * self._calibCount;
          self._calibSort(intpos, intpos + self._calibCount);
          idx := idx + 1;
        end;
      // Compute the scaling factor for each axis
      xVal := 0;
      yVal := 0;
      zVal := 0;
      idx := 0;
      while idx < 6 do
        begin
          intpos := idx * self._calibCount + (self._calibCount div 2);
          orient := self._calibOrient[idx];
          if orient = 0 or orient = 1 then
            begin
              zVal := zVal + self._calibDataAcc[intpos];
            end;
          if orient = 2 or orient = 3 then
            begin
              xVal := xVal + self._calibDataAcc[intpos];
            end;
          if orient = 4 or orient = 5 then
            begin
              yVal := yVal + self._calibDataAcc[intpos];
            end;
          idx := idx + 1;
        end;
      self._calibAccXScale := xVal / 2.0;
      self._calibAccYScale := yVal / 2.0;
      self._calibAccZScale := zVal / 2.0;
      // Report completion
      self._calibProgress := 100;
      self._calibStageHint := 'Calibration data ready for saving';
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYRefFrame.more3DCalibrationV2():LongInt;
    var
      currTick : LongInt;
      calibParam : TByteArray;
      iCalib : TLongIntArray;
      cal3 : LongInt;
      calAcc : LongInt;
      calMag : LongInt;
      calGyr : LongInt;
    begin
      if self._calibStage = 0 then
        begin
          result := YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if self._calibProgress = 100 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      // make sure we don't start before previous calibration is cleared
      if self._calibStage = 1 then
        begin
          currTick := ((yGetTickCount) and ($07FFFFFFF));
          currTick := ((currTick - self._calibPrevTick) and ($07FFFFFFF));
          if currTick < 1600 then
            begin
              self._calibStageHint := 'Set down the device on a steady horizontal surface';
              self._calibStageProgress = (currTick div 40);
              self._calibProgress = 1;
              result := YAPI_SUCCESS;
              exit;
            end;
        end;

      calibParam := self._download('api/refFrame/calibrationParam.txt');
      iCalib := _decodeFloats(_ByteToString(calibParam));
      cal3 := (iCalib[1] div 1000);
      calAcc = (cal3 div 100);
      calMag = (cal3 div 10) - 10*calAcc;
      calGyr = ((cal3) Mod (10));
      if calGyr < 3 then
        begin
          self._calibStageHint := 'Set down the device on a steady horizontal surface';
          self._calibStageProgress = 40 + calGyr*20;
          self._calibProgress = 4 + calGyr*2;
        end
      else
        begin
          self._calibStage = 2;
          if calMag < 3 then
            begin
              self._calibStageHint := 'Slowly draw ''8'' shapes along the 3 axis';
              self._calibStageProgress = 1 + calMag*33;
              self._calibProgress = 10 + calMag*5;
            end
          else
            begin
              self._calibStage = 3;
              if calAcc < 3 then
                begin
                  self._calibStageHint := 'Slowly turn the device, stopping at each 90 degrees';
                  self._calibStageProgress = 1 + calAcc*33;
                  self._calibProgress = 25 + calAcc*25;
                end
              else
                begin
                  self._calibStageProgress = 99;
                  self._calibProgress = 100;
                end;
            end;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYRefFrame.get_3DCalibrationHint():string;
    begin
      result := self._calibStageHint;
      exit;
    end;


  function TYRefFrame.get_3DCalibrationProgress():LongInt;
    begin
      result := self._calibProgress;
      exit;
    end;


  function TYRefFrame.get_3DCalibrationStage():LongInt;
    begin
      result := self._calibStage;
      exit;
    end;


  function TYRefFrame.get_3DCalibrationStageProgress():LongInt;
    begin
      result := self._calibStageProgress;
      exit;
    end;


  function TYRefFrame.get_3DCalibrationLogMsg():string;
    var
      msg : string;
    begin
      msg := self._calibLogMsg;
      self._calibLogMsg := '';
      result := msg;
      exit;
    end;


  function TYRefFrame.save3DCalibration():LongInt;
    begin
      if self._calibV2 then
        begin
          result := self.save3DCalibrationV2;
          exit;
        end;
      result := self.save3DCalibrationV1;
      exit;
    end;


  function TYRefFrame.save3DCalibrationV1():LongInt;
    var
      shiftX : LongInt;
      shiftY : LongInt;
      shiftZ : LongInt;
      scaleExp : LongInt;
      scaleX : LongInt;
      scaleY : LongInt;
      scaleZ : LongInt;
      scaleLo : LongInt;
      scaleHi : LongInt;
      newcalib : string;
    begin
      if self._calibProgress <> 100 then
        begin
          result := YAPI_INVALID_ARGUMENT;
          exit;
        end;
      // Compute integer values (correction unit is 732ug/count)
      shiftX := -round(self._calibAccXOfs / 0.000732);
      if shiftX < 0 then
        begin
          shiftX := shiftX + 65536;
        end;
      shiftY := -round(self._calibAccYOfs / 0.000732);
      if shiftY < 0 then
        begin
          shiftY := shiftY + 65536;
        end;
      shiftZ := -round(self._calibAccZOfs / 0.000732);
      if shiftZ < 0 then
        begin
          shiftZ := shiftZ + 65536;
        end;
      scaleX := round(2048.0 / self._calibAccXScale) - 2048;
      scaleY := round(2048.0 / self._calibAccYScale) - 2048;
      scaleZ := round(2048.0 / self._calibAccZScale) - 2048;
      if scaleX < -2048 or scaleX >= 2048 or scaleY < -2048 or scaleY >= 2048 or scaleZ < -2048 or scaleZ >= 2048 then
        begin
          scaleExp := 3;
        end
      else
        begin
          if scaleX < -1024 or scaleX >= 1024 or scaleY < -1024 or scaleY >= 1024 or scaleZ < -1024 or scaleZ >= 1024 then
            begin
              scaleExp := 2;
            end
          else
            begin
              if scaleX < -512 or scaleX >= 512 or scaleY < -512 or scaleY >= 512 or scaleZ < -512 or scaleZ >= 512 then
                begin
                  scaleExp := 1;
                end
              else
                begin
                  scaleExp := 0;
                end;
            end;
        end;
      if scaleExp > 0 then
        begin
          scaleX := ((scaleX) shr (scaleExp));
          scaleY := ((scaleY) shr (scaleExp));
          scaleZ := ((scaleZ) shr (scaleExp));
        end;
      if scaleX < 0 then
        begin
          scaleX := scaleX + 1024;
        end;
      if scaleY < 0 then
        begin
          scaleY := scaleY + 1024;
        end;
      if scaleZ < 0 then
        begin
          scaleZ := scaleZ + 1024;
        end;
      scaleLo := ((((scaleY) and 15)) shl 12) + ((scaleX) shl 2) + scaleExp;
      scaleHi := ((scaleZ) shl 6) + ((scaleY) shr 4);
      // Save calibration parameters
      newcalib := '5,'+inttostr(shiftX)+','+inttostr(shiftY)+','+inttostr(shiftZ)+','+inttostr(scaleLo)+','+inttostr(scaleHi);
      self._calibStage := 0;
      result := self.set_calibrationParam(newcalib);
      exit;
    end;


  function TYRefFrame.save3DCalibrationV2():LongInt;
    begin
      result := self.set_calibrationParam('5,5,5,5,5,5');
      exit;
    end;


  function TYRefFrame.cancel3DCalibration():LongInt;
    begin
      if self._calibStage = 0 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;

      self._calibStage := 0;
      result := self.set_calibrationParam(self._calibSavedParams);
      exit;
    end;


  function TYRefFrame.nextRefFrame(): TYRefFrame;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextRefFrame := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextRefFrame := nil;
          exit;
        end;
      nextRefFrame := TYRefFrame.FindRefFrame(hwid);
    end;

  class function TYRefFrame.FirstRefFrame(): TYRefFrame;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('RefFrame', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYRefFrame.FindRefFrame(serial+'.'+funcId);
    end;

//--- (end of YRefFrame implementation)

//--- (YRefFrame functions)

  function yFindRefFrame(func:string): TYRefFrame;
    begin
      result := TYRefFrame.FindRefFrame(func);
    end;

  function yFirstRefFrame(): TYRefFrame;
    begin
      result := TYRefFrame.FirstRefFrame();
    end;

  procedure _RefFrameCleanup();
    begin
    end;

//--- (end of YRefFrame functions)

initialization
  //--- (YRefFrame initialization)
  //--- (end of YRefFrame initialization)

finalization
  //--- (YRefFrame cleanup)
  _RefFrameCleanup();
  //--- (end of YRefFrame cleanup)

end.
