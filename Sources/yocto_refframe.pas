{*********************************************************************
 *
 * $Id: yocto_refframe.pas 17481 2014-09-03 09:38:35Z mvuilleu $
 *
 * Implements yFindRefFrame(), the high-level API for RefFrame functions
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


unit yocto_refframe;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YRefFrame definitions)
type  TYMOUNTPOSITION = (Y_MOUNTPOSITION_BOTTOM,Y_MOUNTPOSITION_TOP,Y_MOUNTPOSITION_FRONT,Y_MOUNTPOSITION_RIGHT,Y_MOUNTPOSITION_REAR,Y_MOUNTPOSITION_LEFT);

type  TYMOUNTORIENTATION = (Y_MOUNTORIENTATION_TWELVE,Y_MOUNTORIENTATION_THREE,Y_MOUNTORIENTATION_SIX,Y_MOUNTORIENTATION_NINE);


const Y_MOUNTPOS_INVALID              = YAPI_INVALID_UINT;
const Y_BEARING_INVALID               = YAPI_INVALID_DOUBLE;
const Y_CALIBRATIONPARAM_INVALID      = YAPI_INVALID_STRING;


//--- (end of YRefFrame definitions)

type
  TYRefFrame = class;
  //--- (YRefFrame class start)
  TYRefFrameValueCallback = procedure(func: TYRefFrame; value:string);
  TYRefFrameTimedReportCallback = procedure(func: TYRefFrame; value:TYMeasure);

  ////
  /// <summary>
  ///   TYRefFrame Class: Reference frame configuration
  /// <para>
  ///   This class is used to setup the base orientation of the Yocto-3D, so that
  ///   the orientation functions, relative to the earth surface plane, use
  ///   the proper reference frame. The class also implements a tridimensional
  ///   sensor calibration process, which can compensate for local variations
  ///   of standard gravity and improve the precision of the tilt sensors.
  /// </para>
  /// </summary>
  ///-
  TYRefFrame=class(TYFunction)
  //--- (end of YRefFrame class start)
  protected
  //--- (YRefFrame declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _mountPos                 : LongInt;
    _bearing                  : double;
    _calibrationParam         : string;
    _valueCallbackRefFrame    : TYRefFrameValueCallback;
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
    ///   For instance, if you setup as reference bearing the value of the earth
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    ///   On failure, throws an exception or returns <c>Y_BEARING_INVALID</c>.
    /// </para>
    ///-
    function get_bearing():double;

    function get_calibrationParam():string;

    function set_calibrationParam(newval:string):integer;

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
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$
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
    ///   a value among the <c>Y_MOUNTPOSITION</c> enumeration
    ///   (<c>Y_MOUNTPOSITION_BOTTOM</c>,   <c>Y_MOUNTPOSITION_TOP</c>,
    ///   <c>Y_MOUNTPOSITION_FRONT</c>,    <c>Y_MOUNTPOSITION_RIGHT</c>,
    ///   <c>Y_MOUNTPOSITION_REAR</c>,     <c>Y_MOUNTPOSITION_LEFT</c>),
    ///   corresponding to the installation in a box, on one of the six faces.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
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
    ///   a value among the enumeration <c>Y_MOUNTORIENTATION</c>
    ///   (<c>Y_MOUNTORIENTATION_TWELVE</c>, <c>Y_MOUNTORIENTATION_THREE</c>,
    ///   <c>Y_MOUNTORIENTATION_SIX</c>,     <c>Y_MOUNTORIENTATION_NINE</c>)
    ///   corresponding to the orientation of the "X" arrow on the device,
    ///   as on a clock dial seen from an observer in the center of the box.
    ///   On the bottom face, the 12H orientation points to the front, while
    ///   on the top face, the 12H orientation points to the rear.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
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
    ///   a value among the <c>Y_MOUNTPOSITION</c> enumeration
    ///   (<c>Y_MOUNTPOSITION_BOTTOM</c>,   <c>Y_MOUNTPOSITION_TOP</c>,
    ///   <c>Y_MOUNTPOSITION_FRONT</c>,    <c>Y_MOUNTPOSITION_RIGHT</c>,
    ///   <c>Y_MOUNTPOSITION_REAR</c>,     <c>Y_MOUNTPOSITION_LEFT</c>),
    ///   corresponding to the installation in a box, on one of the six faces.
    /// </param>
    /// <param name="orientation">
    ///   a value among the enumeration <c>Y_MOUNTORIENTATION</c>
    ///   (<c>Y_MOUNTORIENTATION_TWELVE</c>, <c>Y_MOUNTORIENTATION_THREE</c>,
    ///   <c>Y_MOUNTORIENTATION_SIX</c>,     <c>Y_MOUNTORIENTATION_NINE</c>)
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
    ///   <c>save3DCalibration</c>. The calibration process can be canceled
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
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YRefFrame</c> object, corresponding to
    ///   a reference frame currently online, or a <c>null</c> pointer
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

//--- (RefFrame functions declaration)
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
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the reference frame
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
  ///   the first reference frame currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstRefFrame():TYRefFrame;

//--- (end of RefFrame functions declaration)

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
          _bearing := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'calibrationParam') then
        begin
          _calibrationParam := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYRefFrame.get_mountPos():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MOUNTPOS_INVALID;
              exit
            end;
        end;
      result := self._mountPos;
      exit;
    end;


  function TYRefFrame.set_mountPos(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('mountPos',rest_val);
    end;

  ////
  /// <summary>
  ///   Changes the reference bearing used by the compass.
  /// <para>
  ///   The relative bearing
  ///   indicated by the compass is the difference between the measured magnetic
  ///   heading and the reference bearing indicated here.
  /// </para>
  /// <para>
  ///   For instance, if you setup as reference bearing the value of the earth
  ///   magnetic declination, the compass will provide the orientation relative
  ///   to the geographic North.
  /// </para>
  /// <para>
  ///   Similarly, when the sensor is not mounted along the standard directions
  ///   because it has an additional yaw angle, you can set this angle in the reference
  ///   bearing so that the compass provides the expected natural direction.
  /// </para>
  /// <para>
  ///   Remember to call the saveToFlash()
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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYRefFrame.set_bearing(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('bearing',rest_val);
    end;

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
  ///   On failure, throws an exception or returns Y_BEARING_INVALID.
  /// </para>
  ///-
  function TYRefFrame.get_bearing():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_BEARING_INVALID;
              exit
            end;
        end;
      result := self._bearing;
      exit;
    end;


  function TYRefFrame.get_calibrationParam():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CALIBRATIONPARAM_INVALID;
              exit
            end;
        end;
      result := self._calibrationParam;
      exit;
    end;


  function TYRefFrame.set_calibrationParam(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('calibrationParam',rest_val);
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
  ///   Use the method <c>YRefFrame.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YRefFrame</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYRefFrame.FindRefFrame(func: string):TYRefFrame;
    var
      obj : TYRefFrame;
    begin
      obj := TYRefFrame(TYFunction._FindFromCache('RefFrame', func));
      if obj = nil then
        begin
          obj :=  TYRefFrame.create(func);
          TYFunction._AddToCache('RefFrame',  func, obj)
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
  function TYRefFrame.registerValueCallback(callback: TYRefFrameValueCallback):LongInt;
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
      self._valueCallbackRefFrame := callback;
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


  function TYRefFrame._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackRefFrame) <> nil) then
        begin
          self._valueCallbackRefFrame(self, value)
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
  ///   Returns the installation position of the device, as configured
  ///   in order to define the reference frame for the compass and the
  ///   pitch/roll tilt sensors.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among the <c>Y_MOUNTPOSITION</c> enumeration
  ///   (<c>Y_MOUNTPOSITION_BOTTOM</c>,   <c>Y_MOUNTPOSITION_TOP</c>,
  ///   <c>Y_MOUNTPOSITION_FRONT</c>,    <c>Y_MOUNTPOSITION_RIGHT</c>,
  ///   <c>Y_MOUNTPOSITION_REAR</c>,     <c>Y_MOUNTPOSITION_LEFT</c>),
  ///   corresponding to the installation in a box, on one of the six faces.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYRefFrame.get_mountPosition():TYMOUNTPOSITION;
    var
      position : LongInt;
    begin
      position := self.get_mountPos;
      return TYMOUNTPOSITION(((position) shr 2));
    end;


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
  ///   a value among the enumeration <c>Y_MOUNTORIENTATION</c>
  ///   (<c>Y_MOUNTORIENTATION_TWELVE</c>, <c>Y_MOUNTORIENTATION_THREE</c>,
  ///   <c>Y_MOUNTORIENTATION_SIX</c>,     <c>Y_MOUNTORIENTATION_NINE</c>)
  ///   corresponding to the orientation of the "X" arrow on the device,
  ///   as on a clock dial seen from an observer in the center of the box.
  ///   On the bottom face, the 12H orientation points to the front, while
  ///   on the top face, the 12H orientation points to the rear.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYRefFrame.get_mountOrientation():TYMOUNTORIENTATION;
    var
      position : LongInt;
    begin
      position := self.get_mountPos;
      return TYMOUNTORIENTATION(((position) and 3));
    end;


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
  ///   a value among the <c>Y_MOUNTPOSITION</c> enumeration
  ///   (<c>Y_MOUNTPOSITION_BOTTOM</c>,   <c>Y_MOUNTPOSITION_TOP</c>,
  ///   <c>Y_MOUNTPOSITION_FRONT</c>,    <c>Y_MOUNTPOSITION_RIGHT</c>,
  ///   <c>Y_MOUNTPOSITION_REAR</c>,     <c>Y_MOUNTPOSITION_LEFT</c>),
  ///   corresponding to the installation in a box, on one of the six faces.
  /// </param>
  /// <param name="orientation">
  ///   a value among the enumeration <c>Y_MOUNTORIENTATION</c>
  ///   (<c>Y_MOUNTORIENTATION_TWELVE</c>, <c>Y_MOUNTORIENTATION_THREE</c>,
  ///   <c>Y_MOUNTORIENTATION_SIX</c>,     <c>Y_MOUNTORIENTATION_NINE</c>)
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
  function TYRefFrame.set_mountPosition(position: TYMOUNTPOSITION; orientation: TYMOUNTORIENTATION):LongInt;
    var
      mixedPos : LongInt;
    begin
      mixedPos := ((position) shl 2) + orientation;
      return self.set_mountPos(mixedPos);
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
                  self._calibDataAcc[ idx-1] := b;
                  self._calibDataAcc[ idx] := a;
                  xa := self._calibDataAccX[idx-1];
                  xb := self._calibDataAccX[idx];
                  self._calibDataAccX[ idx-1] := xb;
                  self._calibDataAccX[ idx] := xa;
                  xa := self._calibDataAccY[idx-1];
                  xb := self._calibDataAccY[idx];
                  self._calibDataAccY[ idx-1] := xb;
                  self._calibDataAccY[ idx] := xa;
                  xa := self._calibDataAccZ[idx-1];
                  xb := self._calibDataAccZ[idx];
                  self._calibDataAccZ[ idx-1] := xb;
                  self._calibDataAccZ[ idx] := xa;
                  changed := changed + 1
                end
              else
                begin
                  a := b
                end;
              idx := idx + 1
            end;
        end;
      result := 0;
      exit;
    end;


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
  ///   <c>save3DCalibration</c>. The calibration process can be canceled
  ///   at any time using method <c>cancel3DCalibration</c>.
  /// </para>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  /// </summary>
  ///-
  function TYRefFrame.start3DCalibration():LongInt;
    var
      calibOrient_pos : LongInt;
      calibDataAccX_pos : LongInt;
      calibDataAccY_pos : LongInt;
      calibDataAccZ_pos : LongInt;
      calibDataAcc_pos : LongInt;
    begin
      if not(self.isOnline) then
        begin
          result := YAPI_DEVICE_NOT_FOUND;
          exit
        end;
      if self._calibStage <> 0 then
        begin
          self.cancel3DCalibration
        end;
      self._calibSavedParams := self.get_calibrationParam;
      self.set_calibrationParam('0');
      self._calibCount := 50;
      self._calibStage := 1;
      self._calibStageHint := 'Set down the device on a steady horizontal surface';
      self._calibStageProgress := 0;
      self._calibProgress := 1;
      self._calibInternalPos := 0;
      self._calibPrevTick := ((yGetTickCount) and ($07FFFFFFF));
      calibOrient_pos := 0;
      SetLength(self._calibOrient, 6);;
      calibDataAccX_pos := 0;
      SetLength(self._calibDataAccX, 6 * self._calibCount);;
      calibDataAccY_pos := 0;
      SetLength(self._calibDataAccY, 6 * self._calibCount);;
      calibDataAccZ_pos := 0;
      SetLength(self._calibDataAccZ, 6 * self._calibCount);;
      calibDataAcc_pos := 0;
      SetLength(self._calibDataAcc, 6 * self._calibCount);;
      result := YAPI_SUCCESS;
      exit;
    end;


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
  function TYRefFrame.more3DCalibration():LongInt;
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
          exit
        end;
      if self._calibProgress = 100 then
        begin
          result := YAPI_SUCCESS;
          exit
        end;
      
      // make sure we leave at least 160ms between samples
      currTick :=  ((yGetTickCount) and ($07FFFFFFF));
      if ((currTick - self._calibPrevTick) and ($07FFFFFFF)) < 160 then
        begin
          result := YAPI_SUCCESS;
          exit
        end;
      // load current accelerometer values, make sure we are on a straight angle
      // (default timeout to 0,5 sec without reading measure when out of range)
      self._calibStageHint := 'Set down the device on a steady horizontal surface';
      self._calibPrevTick := ((currTick + 500) and ($07FFFFFFF));
      jsonData := self._download('api/accelerometer.json');
      xVal := StrToInt(self._json_get_key(jsonData, 'xValue')) / 65536.0;
      yVal := StrToInt(self._json_get_key(jsonData, 'yValue')) / 65536.0;
      zVal := StrToInt(self._json_get_key(jsonData, 'zValue')) / 65536.0;
      xSq := xVal * xVal;
      if xSq >= 0.04 and xSq < 0.64 then
        begin
          result := YAPI_SUCCESS;
          exit
        end;
      if xSq >= 1.44 then
        begin
          result := YAPI_SUCCESS;
          exit
        end;
      ySq := yVal * yVal;
      if ySq >= 0.04 and ySq < 0.64 then
        begin
          result := YAPI_SUCCESS;
          exit
        end;
      if ySq >= 1.44 then
        begin
          result := YAPI_SUCCESS;
          exit
        end;
      zSq := zVal * zVal;
      if zSq >= 0.04 and zSq < 0.64 then
        begin
          result := YAPI_SUCCESS;
          exit
        end;
      if zSq >= 1.44 then
        begin
          result := YAPI_SUCCESS;
          exit
        end;
      norm := Sqrt(xSq + ySq + zSq);
      if norm < 0.8 or norm > 1.2 then
        begin
          result := YAPI_SUCCESS;
          exit
        end;
      self._calibPrevTick := currTick;
      
      // Determine the device orientation index
      orient := 0;
      if zSq > 0.5 then
        begin
          if zVal > 0 then
            begin
              orient := 0
            end
          else
            begin
              orient := 1
            end;
        end;
      if xSq > 0.5 then
        begin
          if xVal > 0 then
            begin
              orient := 2
            end
          else
            begin
              orient := 3
            end;
        end;
      if ySq > 0.5 then
        begin
          if yVal > 0 then
            begin
              orient := 4
            end
          else
            begin
              orient := 5
            end;
        end;
      
      // Discard measures that are not in the proper orientation
      if self._calibStageProgress = 0 then
        begin
          idx := 0;
          err := 0;
          while idx + 1 < self._calibStage do
            begin
              if self._calibOrient[idx] = orient then
                begin
                  err := 1
                end;
              idx := idx + 1
            end;
          if err <> 0 then
            begin
              self._calibStageHint := 'Turn the device on another face';
              result := YAPI_SUCCESS;
              exit
            end;
          self._calibOrient[calibOrient_pos] := orient;
          inc(calibOrient_pos)
        end
      else
        begin
          if orient <> self._calibOrient[self._calibStage-1] then
            begin
              self._calibStageHint := 'Not yet done, please move back to the previous face';
              result := YAPI_SUCCESS;
              exit
            end;
        end;
      
      // Save measure
      self._calibStageHint := 'calibrating..';
      self._calibDataAccX[calibDataAccX_pos] := xVal;
      inc(calibDataAccX_pos);
      self._calibDataAccY[calibDataAccY_pos] := yVal;
      inc(calibDataAccY_pos);
      self._calibDataAccZ[calibDataAccZ_pos] := zVal;
      inc(calibDataAccZ_pos);
      self._calibDataAcc[calibDataAcc_pos] := norm;
      inc(calibDataAcc_pos);
      self._calibInternalPos := self._calibInternalPos + 1;
      self._calibProgress := 1 + 16 * (self._calibStage - 1) + (16 * self._calibInternalPos div self._calibCount);
      if self._calibInternalPos < self._calibCount then
        begin
          self._calibStageProgress := 1 + (99 * self._calibInternalPos div self._calibCount);
          result := YAPI_SUCCESS;
          exit
        end;
      
      // Stage done, compute preliminary result
      intpos := (self._calibStage - 1) * self._calibCount;
      self._calibSort(intpos, intpos + self._calibCount);
      intpos := intpos + (self._calibCount div 2);
      self._calibLogMsg := 'Stage '+inttostr( self._calibStage)+': median is '+inttostr(
      round(1000*self._calibDataAccX[intpos]))+','+inttostr(
      round(1000*self._calibDataAccY[intpos]))+','+inttostr(round(1000*self._calibDataAccZ[intpos]));
      
      // move to next stage
      self._calibStage := self._calibStage + 1;
      if self._calibStage < 7 then
        begin
          self._calibStageHint := 'Turn the device on another face';
          self._calibPrevTick := ((currTick + 500) and ($07FFFFFFF));
          self._calibStageProgress := 0;
          self._calibInternalPos := 0;
          result := YAPI_SUCCESS;
          exit
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
              zVal := zVal + self._calibDataAccZ[intpos]
            end;
          if orient = 2 or orient = 3 then
            begin
              xVal := xVal + self._calibDataAccX[intpos]
            end;
          if orient = 4 or orient = 5 then
            begin
              yVal := yVal + self._calibDataAccY[intpos]
            end;
          idx := idx + 1
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
          self._calibDataAcc[ intpos] := norm;
          intpos := intpos + 1
        end;
      idx := 0;
      while idx < 6 do
        begin
          intpos := idx * self._calibCount;
          self._calibSort(intpos, intpos + self._calibCount);
          idx := idx + 1
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
              zVal := zVal + self._calibDataAcc[intpos]
            end;
          if orient = 2 or orient = 3 then
            begin
              xVal := xVal + self._calibDataAcc[intpos]
            end;
          if orient = 4 or orient = 5 then
            begin
              yVal := yVal + self._calibDataAcc[intpos]
            end;
          idx := idx + 1
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
  function TYRefFrame.get_3DCalibrationHint():string;
    begin
      result := self._calibStageHint;
      exit;
    end;


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
  function TYRefFrame.get_3DCalibrationProgress():LongInt;
    begin
      result := self._calibProgress;
      exit;
    end;


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
  function TYRefFrame.get_3DCalibrationStage():LongInt;
    begin
      result := self._calibStage;
      exit;
    end;


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
  function TYRefFrame.get_3DCalibrationStageProgress():LongInt;
    begin
      result := self._calibStageProgress;
      exit;
    end;


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
  function TYRefFrame.get_3DCalibrationLogMsg():string;
    var
      msg : string;
    begin
      msg := self._calibLogMsg;
      self._calibLogMsg := '';
      result := msg;
      exit;
    end;


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
  function TYRefFrame.save3DCalibration():LongInt;
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
          exit
        end;
      
      // Compute integer values (correction unit is 732ug/count)
      shiftX := -round(self._calibAccXOfs / 0.000732);
      if shiftX < 0 then
        begin
          shiftX := shiftX + 65536
        end;
      shiftY := -round(self._calibAccYOfs / 0.000732);
      if shiftY < 0 then
        begin
          shiftY := shiftY + 65536
        end;
      shiftZ := -round(self._calibAccZOfs / 0.000732);
      if shiftZ < 0 then
        begin
          shiftZ := shiftZ + 65536
        end;
      scaleX := round(2048.0 / self._calibAccXScale) - 2048;
      scaleY := round(2048.0 / self._calibAccYScale) - 2048;
      scaleZ := round(2048.0 / self._calibAccZScale) - 2048;
      if scaleX < -2048 or scaleX >= 2048 or scaleY < -2048 or scaleY >= 2048 or scaleZ < -2048 or scaleZ >= 2048 then
        begin
          scaleExp := 3
        end
      else
        begin
          if scaleX < -1024 or scaleX >= 1024 or scaleY < -1024 or scaleY >= 1024 or scaleZ < -1024 or scaleZ >= 1024 then
            begin
              scaleExp := 2
            end
          else
            begin
              if scaleX < -512 or scaleX >= 512 or scaleY < -512 or scaleY >= 512 or scaleZ < -512 or scaleZ >= 512 then
                begin
                  scaleExp := 1
                end
              else
                begin
                  scaleExp := 0
                end;
            end;
        end;
      if scaleExp > 0 then
        begin
          scaleX := ((scaleX) shr (scaleExp));
          scaleY := ((scaleY) shr (scaleExp));
          scaleZ := ((scaleZ) shr (scaleExp))
        end;
      if scaleX < 0 then
        begin
          scaleX := scaleX + 1024
        end;
      if scaleY < 0 then
        begin
          scaleY := scaleY + 1024
        end;
      if scaleZ < 0 then
        begin
          scaleZ := scaleZ + 1024
        end;
      scaleLo := ((((scaleY) and 15)) shl 12) + ((scaleX) shl 2) + scaleExp;
      scaleHi := ((scaleZ) shl 6) + ((scaleY) shr 4);
      
      // Save calibration parameters
      newcalib := '5,'+inttostr( shiftX)+','+inttostr( shiftY)+','+inttostr( shiftZ)+','+inttostr( scaleLo)+','+inttostr(scaleHi);
      self._calibStage := 0;
      result := self.set_calibrationParam(newcalib);
      exit;
    end;


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
  function TYRefFrame.cancel3DCalibration():LongInt;
    begin
      if self._calibStage = 0 then
        begin
          result := YAPI_SUCCESS;
          exit
        end;
      // may throw an exception        
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

//--- (RefFrame functions)

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

//--- (end of RefFrame functions)

initialization
  //--- (RefFrame initialization)
  //--- (end of RefFrame initialization)

finalization
  //--- (RefFrame cleanup)
  _RefFrameCleanup();
  //--- (end of RefFrame cleanup)
end.