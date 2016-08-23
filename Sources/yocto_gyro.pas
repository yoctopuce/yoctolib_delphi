{*********************************************************************
 *
 * $Id: yocto_gyro.pas 24948 2016-07-01 20:57:28Z mvuilleu $
 *
 * Implements yFindGyro(), the high-level API for Gyro functions
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


unit yocto_gyro;

interface

uses
  sysutils, classes, windows, yocto_api, yjson, Math;

//--- (generated code: YQt definitions)



//--- (end of generated code: YQt definitions)

//--- (generated code: YGyro definitions)

const Y_BANDWIDTH_INVALID             = YAPI_INVALID_INT;
const Y_XVALUE_INVALID                = YAPI_INVALID_DOUBLE;
const Y_YVALUE_INVALID                = YAPI_INVALID_DOUBLE;
const Y_ZVALUE_INVALID                = YAPI_INVALID_DOUBLE;


//--- (end of generated code: YGyro definitions)

type
  TYQt = class;
  //--- (generated code: YQt class start)
  TYQtValueCallback = procedure(func: TYQt; value:string);
  TYQtTimedReportCallback = procedure(func: TYQt; value:TYMeasure);

  ////
  /// <summary>
  ///   TYQt Class: Quaternion interface
  /// <para>
  ///   The Yoctopuce API YQt class provides direct access to the Yocto3D attitude estimation
  ///   using a quaternion. It is usually not needed to use the YQt class directly, as the
  ///   YGyro class provides a more convenient higher-level interface.
  /// </para>
  /// </summary>
  ///-
  TYQt=class(TYSensor)
  //--- (end of generated code: YQt class start)
  protected
  //--- (generated code: YQt declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _unit                     : string;
    _currentValue             : double;
    _lowestValue              : double;
    _highestValue             : double;
    _currentRawValue          : double;
    _logFrequency             : string;
    _reportFrequency          : string;
    _calibrationParam         : string;
    _resolution               : double;
    _sensorState              : LongInt;
    _valueCallbackQt          : TYQtValueCallback;
    _timedReportCallbackQt    : TYQtTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YQt declaration)

  public
    //--- (generated code: YQt accessors declaration)
    constructor Create(func:string);

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
    ///   Use the method <c>YQt.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YQt</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindQt(func: string):TYQt;

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
    function registerValueCallback(callback: TYQtValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every periodic timed notification.
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
    ///   arguments: the function object of which the value has changed, and an YMeasure object describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerTimedReportCallback(callback: TYQtTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of quaternion components started using <c>yFirstQt()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YQt</c> object, corresponding to
    ///   a quaternion component currently online, or a <c>null</c> pointer
    ///   if there are no more quaternion components to enumerate.
    /// </returns>
    ///-
    function nextQt():TYQt;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstQt():TYQt;
  //--- (end of generated code: YQt accessors declaration)
  end;
  //--- (generated code: Qt functions declaration)
  ////
  /// <summary>
  ///   Retrieves a quaternion component for a given identifier.
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
  ///   This function does not require that the quaternion component is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YQt.isOnline()</c> to test if the quaternion component is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a quaternion component by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the quaternion component
  /// </param>
  /// <returns>
  ///   a <c>YQt</c> object allowing you to drive the quaternion component.
  /// </returns>
  ///-
  function yFindQt(func:string):TYQt;
  ////
  /// <summary>
  ///   Starts the enumeration of quaternion components currently accessible.
  /// <para>
  ///   Use the method <c>YQt.nextQt()</c> to iterate on
  ///   next quaternion components.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YQt</c> object, corresponding to
  ///   the first quaternion component currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstQt():TYQt;

//--- (end of generated code: Qt functions declaration)

  procedure yInternalGyroCallback(obj:TYQt; value:string);

type
  TYGyro = class;
  TYQuatCallback = procedure(func: TYGyro; w,x,y,z:double);
  TYAnglesCallback = procedure(func: TYGyro; roll,pitch,head:double);
  //--- (generated code: YGyro class start)
  TYGyroValueCallback = procedure(func: TYGyro; value:string);
  TYGyroTimedReportCallback = procedure(func: TYGyro; value:TYMeasure);

  ////
  /// <summary>
  ///   TYGyro Class: Gyroscope function interface
  /// <para>
  ///   The YSensor class is the parent class for all Yoctopuce sensors. It can be
  ///   used to read the current value and unit of any sensor, read the min/max
  ///   value, configure autonomous recording frequency and access recorded data.
  ///   It also provide a function to register a callback invoked each time the
  ///   observed value changes, or at a predefined interval. Using this class rather
  ///   than a specific subclass makes it possible to create generic applications
  ///   that work with any Yoctopuce sensor, even those that do not yet exist.
  ///   Note: The YAnButton class is the only analog input which does not inherit
  ///   from YSensor.
  /// </para>
  /// </summary>
  ///-
  TYGyro=class(TYSensor)
  //--- (end of generated code: YGyro class start)
  protected
  //--- (generated code: YGyro declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _unit                     : string;
    _currentValue             : double;
    _lowestValue              : double;
    _highestValue             : double;
    _currentRawValue          : double;
    _logFrequency             : string;
    _reportFrequency          : string;
    _calibrationParam         : string;
    _resolution               : double;
    _sensorState              : LongInt;
    _bandwidth                : LongInt;
    _xValue                   : double;
    _yValue                   : double;
    _zValue                   : double;
    _valueCallbackGyro        : TYGyroValueCallback;
    _timedReportCallbackGyro  : TYGyroTimedReportCallback;
    _qt_stamp                 : LongInt;
    _qt_w                     : TYQt;
    _qt_x                     : TYQt;
    _qt_y                     : TYQt;
    _qt_z                     : TYQt;
    _w                        : double;
    _x                        : double;
    _y                        : double;
    _z                        : double;
    _angles_stamp             : LongInt;
    _head                     : double;
    _pitch                    : double;
    _roll                     : double;
    _quatCallback             : TYQuatCallback;
    _anglesCallback           : TYAnglesCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YGyro declaration)

  public
    //--- (generated code: YGyro accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the measure update frequency, measured in Hz (Yocto-3D-V2 only).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the measure update frequency, measured in Hz (Yocto-3D-V2 only)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BANDWIDTH_INVALID</c>.
    /// </para>
    ///-
    function get_bandwidth():LongInt;

    ////
    /// <summary>
    ///   Changes the measure update frequency, measured in Hz (Yocto-3D-V2 only).
    /// <para>
    ///   When the
    ///   frequency is lower, the device performs averaging.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the measure update frequency, measured in Hz (Yocto-3D-V2 only)
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
    function set_bandwidth(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the angular velocity around the X axis of the device, as a floating point number.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the angular velocity around the X axis of the device, as a
    ///   floating point number
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_XVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_xValue():double;

    ////
    /// <summary>
    ///   Returns the angular velocity around the Y axis of the device, as a floating point number.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the angular velocity around the Y axis of the device, as a
    ///   floating point number
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_YVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_yValue():double;

    ////
    /// <summary>
    ///   Returns the angular velocity around the Z axis of the device, as a floating point number.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the angular velocity around the Z axis of the device, as a
    ///   floating point number
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ZVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_zValue():double;

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
    ///   Use the method <c>YGyro.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YGyro</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindGyro(func: string):TYGyro;

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
    function registerValueCallback(callback: TYGyroValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every periodic timed notification.
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
    ///   arguments: the function object of which the value has changed, and an YMeasure object describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerTimedReportCallback(callback: TYGyroTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;

    function _loadQuaternion():LongInt; overload; virtual;

    function _loadAngles():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the estimated roll angle, based on the integration of
    ///   gyroscopic measures combined with acceleration and
    ///   magnetic field measurements.
    /// <para>
    ///   The axis corresponding to the roll angle can be mapped to any
    ///   of the device X, Y or Z physical directions using methods of
    ///   the class <c>YRefFrame</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to roll angle
    ///   in degrees, between -180 and +180.
    /// </returns>
    ///-
    function get_roll():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the estimated pitch angle, based on the integration of
    ///   gyroscopic measures combined with acceleration and
    ///   magnetic field measurements.
    /// <para>
    ///   The axis corresponding to the pitch angle can be mapped to any
    ///   of the device X, Y or Z physical directions using methods of
    ///   the class <c>YRefFrame</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to pitch angle
    ///   in degrees, between -90 and +90.
    /// </returns>
    ///-
    function get_pitch():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the estimated heading angle, based on the integration of
    ///   gyroscopic measures combined with acceleration and
    ///   magnetic field measurements.
    /// <para>
    ///   The axis corresponding to the heading can be mapped to any
    ///   of the device X, Y or Z physical directions using methods of
    ///   the class <c>YRefFrame</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to heading
    ///   in degrees, between 0 and 360.
    /// </returns>
    ///-
    function get_heading():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the <c>w</c> component (real part) of the quaternion
    ///   describing the device estimated orientation, based on the
    ///   integration of gyroscopic measures combined with acceleration and
    ///   magnetic field measurements.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to the <c>w</c>
    ///   component of the quaternion.
    /// </returns>
    ///-
    function get_quaternionW():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the <c>x</c> component of the quaternion
    ///   describing the device estimated orientation, based on the
    ///   integration of gyroscopic measures combined with acceleration and
    ///   magnetic field measurements.
    /// <para>
    ///   The <c>x</c> component is
    ///   mostly correlated with rotations on the roll axis.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to the <c>x</c>
    ///   component of the quaternion.
    /// </returns>
    ///-
    function get_quaternionX():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the <c>y</c> component of the quaternion
    ///   describing the device estimated orientation, based on the
    ///   integration of gyroscopic measures combined with acceleration and
    ///   magnetic field measurements.
    /// <para>
    ///   The <c>y</c> component is
    ///   mostly correlated with rotations on the pitch axis.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to the <c>y</c>
    ///   component of the quaternion.
    /// </returns>
    ///-
    function get_quaternionY():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the <c>x</c> component of the quaternion
    ///   describing the device estimated orientation, based on the
    ///   integration of gyroscopic measures combined with acceleration and
    ///   magnetic field measurements.
    /// <para>
    ///   The <c>x</c> component is
    ///   mostly correlated with changes of heading.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to the <c>z</c>
    ///   component of the quaternion.
    /// </returns>
    ///-
    function get_quaternionZ():double; overload; virtual;

    ////
    /// <summary>
    ///   Registers a callback function that will be invoked each time that the estimated
    ///   device orientation has changed.
    /// <para>
    ///   The call frequency is typically around 95Hz during a move.
    ///   The callback is invoked only during the execution of <c>ySleep</c> or <c>yHandleEvents</c>.
    ///   This provides control over the time when the callback is triggered.
    ///   For good responsiveness, remember to call one of these two functions periodically.
    ///   To unregister a callback, pass a null pointer as argument.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="callback">
    ///   the callback function to invoke, or a null pointer.
    ///   The callback function should take five arguments:
    ///   the YGyro object of the turning device, and the floating
    ///   point values of the four components w, x, y and z
    ///   (as floating-point numbers).
    /// @noreturn
    /// </param>
    ///-
    function registerQuaternionCallback(callback: TYQuatCallback):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Registers a callback function that will be invoked each time that the estimated
    ///   device orientation has changed.
    /// <para>
    ///   The call frequency is typically around 95Hz during a move.
    ///   The callback is invoked only during the execution of <c>ySleep</c> or <c>yHandleEvents</c>.
    ///   This provides control over the time when the callback is triggered.
    ///   For good responsiveness, remember to call one of these two functions periodically.
    ///   To unregister a callback, pass a null pointer as argument.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="callback">
    ///   the callback function to invoke, or a null pointer.
    ///   The callback function should take four arguments:
    ///   the YGyro object of the turning device, and the floating
    ///   point values of the three angles roll, pitch and heading
    ///   in degrees (as floating-point numbers).
    /// @noreturn
    /// </param>
    ///-
    function registerAnglesCallback(callback: TYAnglesCallback):LongInt; overload; virtual;

    function _invokeGyroCallbacks(qtIndex: LongInt; qtValue: double):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of gyroscopes started using <c>yFirstGyro()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YGyro</c> object, corresponding to
    ///   a gyroscope currently online, or a <c>null</c> pointer
    ///   if there are no more gyroscopes to enumerate.
    /// </returns>
    ///-
    function nextGyro():TYGyro;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstGyro():TYGyro;
  //--- (end of generated code: YGyro accessors declaration)
  end;

//--- (generated code: Gyro functions declaration)
  ////
  /// <summary>
  ///   Retrieves a gyroscope for a given identifier.
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
  ///   This function does not require that the gyroscope is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YGyro.isOnline()</c> to test if the gyroscope is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a gyroscope by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the gyroscope
  /// </param>
  /// <returns>
  ///   a <c>YGyro</c> object allowing you to drive the gyroscope.
  /// </returns>
  ///-
  function yFindGyro(func:string):TYGyro;
  ////
  /// <summary>
  ///   Starts the enumeration of gyroscopes currently accessible.
  /// <para>
  ///   Use the method <c>YGyro.nextGyro()</c> to iterate on
  ///   next gyroscopes.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YGyro</c> object, corresponding to
  ///   the first gyro currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstGyro():TYGyro;

//--- (end of generated code: Gyro functions declaration)

implementation


constructor TYQt.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Qt';
      //--- (generated code: YQt accessors initialization)
      _valueCallbackQt := nil;
      _timedReportCallbackQt := nil;
      //--- (end of generated code: YQt accessors initialization)
    end;


//--- (generated code: YQt implementation)
{$HINTS OFF}
  function TYQt._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

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
  ///   Use the method <c>YQt.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YQt</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYQt.FindQt(func: string):TYQt;
    var
      obj : TYQt;
    begin
      obj := TYQt(TYFunction._FindFromCache('Qt', func));
      if obj = nil then
        begin
          obj :=  TYQt.create(func);
          TYFunction._AddToCache('Qt',  func, obj);
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
  function TYQt.registerValueCallback(callback: TYQtValueCallback):LongInt;
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
      self._valueCallbackQt := callback;
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


  function TYQt._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackQt) <> nil) then
        begin
          self._valueCallbackQt(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Registers the callback function that is invoked on every periodic timed notification.
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
  ///   arguments: the function object of which the value has changed, and an YMeasure object describing
  ///   the new advertised value.
  /// @noreturn
  /// </param>
  ///-
  function TYQt.registerTimedReportCallback(callback: TYQtTimedReportCallback):LongInt;
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
      self._timedReportCallbackQt := callback;
      result := 0;
      exit;
    end;


  function TYQt._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackQt) <> nil) then
        begin
          self._timedReportCallbackQt(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYQt.nextQt(): TYQt;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextQt := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextQt := nil;
          exit;
        end;
      nextQt := TYQt.FindQt(hwid);
    end;

  class function TYQt.FirstQt(): TYQt;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Qt', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYQt.FindQt(serial+'.'+funcId);
    end;

//--- (end of generated code: YQt implementation)

//--- (generated code: Qt functions)

  function yFindQt(func:string): TYQt;
    begin
      result := TYQt.FindQt(func);
    end;

  function yFirstQt(): TYQt;
    begin
      result := TYQt.FirstQt();
    end;

  procedure _QtCleanup();
    begin
    end;

//--- (end of generated code: Qt functions)
  procedure yInternalGyroCallback(obj:TYQt; value:string);
    var
      gyro :  TYGyro;
      tmp :  string;
      idx,dummy : integer;
      dbl_value : double;
    begin
      gyro := TYGyro(obj.get_userData());
      if gyro = nil then exit;
      tmp := obj.get_functionId;
      tmp := copy(tmp, 3, 1);
      val(tmp, idx, dummy);
      val(tmp, dbl_value, dummy);
      gyro._invokeGyroCallbacks(idx,dbl_value);
    end;


  constructor TYGyro.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Gyro';
      //--- (generated code: YGyro accessors initialization)
      _bandwidth := Y_BANDWIDTH_INVALID;
      _xValue := Y_XVALUE_INVALID;
      _yValue := Y_YVALUE_INVALID;
      _zValue := Y_ZVALUE_INVALID;
      _valueCallbackGyro := nil;
      _timedReportCallbackGyro := nil;
      _qt_stamp := 0;
      _w := 0;
      _x := 0;
      _y := 0;
      _z := 0;
      _angles_stamp := 0;
      _head := 0;
      _pitch := 0;
      _roll := 0;
      //--- (end of generated code: YGyro accessors initialization)
    end;


//--- (generated code: YGyro implementation)
{$HINTS OFF}
  function TYGyro._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'bandwidth') then
        begin
          _bandwidth := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'xValue') then
        begin
          _xValue := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'yValue') then
        begin
          _yValue := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'zValue') then
        begin
          _zValue := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Returns the measure update frequency, measured in Hz (Yocto-3D-V2 only).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the measure update frequency, measured in Hz (Yocto-3D-V2 only)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_BANDWIDTH_INVALID.
  /// </para>
  ///-
  function TYGyro.get_bandwidth():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_BANDWIDTH_INVALID;
              exit;
            end;
        end;
      result := self._bandwidth;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the measure update frequency, measured in Hz (Yocto-3D-V2 only).
  /// <para>
  ///   When the
  ///   frequency is lower, the device performs averaging.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the measure update frequency, measured in Hz (Yocto-3D-V2 only)
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
  function TYGyro.set_bandwidth(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('bandwidth',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the angular velocity around the X axis of the device, as a floating point number.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the angular velocity around the X axis of the device, as a
  ///   floating point number
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_XVALUE_INVALID.
  /// </para>
  ///-
  function TYGyro.get_xValue():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_XVALUE_INVALID;
              exit;
            end;
        end;
      result := self._xValue;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the angular velocity around the Y axis of the device, as a floating point number.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the angular velocity around the Y axis of the device, as a
  ///   floating point number
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_YVALUE_INVALID.
  /// </para>
  ///-
  function TYGyro.get_yValue():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_YVALUE_INVALID;
              exit;
            end;
        end;
      result := self._yValue;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the angular velocity around the Z axis of the device, as a floating point number.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the angular velocity around the Z axis of the device, as a
  ///   floating point number
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ZVALUE_INVALID.
  /// </para>
  ///-
  function TYGyro.get_zValue():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ZVALUE_INVALID;
              exit;
            end;
        end;
      result := self._zValue;
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
  ///   Use the method <c>YGyro.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YGyro</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYGyro.FindGyro(func: string):TYGyro;
    var
      obj : TYGyro;
    begin
      obj := TYGyro(TYFunction._FindFromCache('Gyro', func));
      if obj = nil then
        begin
          obj :=  TYGyro.create(func);
          TYFunction._AddToCache('Gyro',  func, obj);
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
  function TYGyro.registerValueCallback(callback: TYGyroValueCallback):LongInt;
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
      self._valueCallbackGyro := callback;
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


  function TYGyro._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackGyro) <> nil) then
        begin
          self._valueCallbackGyro(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Registers the callback function that is invoked on every periodic timed notification.
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
  ///   arguments: the function object of which the value has changed, and an YMeasure object describing
  ///   the new advertised value.
  /// @noreturn
  /// </param>
  ///-
  function TYGyro.registerTimedReportCallback(callback: TYGyroTimedReportCallback):LongInt;
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
      self._timedReportCallbackGyro := callback;
      result := 0;
      exit;
    end;


  function TYGyro._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackGyro) <> nil) then
        begin
          self._timedReportCallbackGyro(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYGyro._loadQuaternion():LongInt;
    var
      now_stamp : LongInt;
      age_ms : LongInt;
    begin
      now_stamp := ((yGetTickCount) and ($07FFFFFFF));
      age_ms := (((now_stamp - self._qt_stamp)) and ($07FFFFFFF));
      if (age_ms >= 10) or(self._qt_stamp = 0) then
        begin
          if self.load(10) <> YAPI_SUCCESS then
            begin
              result := YAPI_DEVICE_NOT_FOUND;
              exit;
            end;
          if self._qt_stamp = 0 then
            begin
              self._qt_w := TYQt.FindQt(''+self._serial+'.qt1');
              self._qt_x := TYQt.FindQt(''+self._serial+'.qt2');
              self._qt_y := TYQt.FindQt(''+self._serial+'.qt3');
              self._qt_z := TYQt.FindQt(''+self._serial+'.qt4');
            end;
          if self._qt_w.load(9) <> YAPI_SUCCESS then
            begin
              result := YAPI_DEVICE_NOT_FOUND;
              exit;
            end;
          if self._qt_x.load(9) <> YAPI_SUCCESS then
            begin
              result := YAPI_DEVICE_NOT_FOUND;
              exit;
            end;
          if self._qt_y.load(9) <> YAPI_SUCCESS then
            begin
              result := YAPI_DEVICE_NOT_FOUND;
              exit;
            end;
          if self._qt_z.load(9) <> YAPI_SUCCESS then
            begin
              result := YAPI_DEVICE_NOT_FOUND;
              exit;
            end;
          self._w := self._qt_w.get_currentValue();
          self._x := self._qt_x.get_currentValue();
          self._y := self._qt_y.get_currentValue();
          self._z := self._qt_z.get_currentValue();
          self._qt_stamp := now_stamp;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYGyro._loadAngles():LongInt;
    var
      sqw : double;
      sqx : double;
      sqy : double;
      sqz : double;
      norm : double;
      delta : double;
    begin
      if self._loadQuaternion <> YAPI_SUCCESS then
        begin
          result := YAPI_DEVICE_NOT_FOUND;
          exit;
        end;
      if self._angles_stamp <> self._qt_stamp then
        begin
          sqw := self._w * self._w;
          sqx := self._x * self._x;
          sqy := self._y * self._y;
          sqz := self._z * self._z;
          norm := sqx + sqy + sqz + sqw;
          delta := self._y * self._w - self._x * self._z;
          if delta > 0.499 * norm then
            begin
              self._pitch := 90.0;
              self._head  := round(2.0 * 1800.0/PI * ArcTan2(self._x,-self._w)) / 10.0;
            end
          else
            begin
              if delta < -0.499 * norm then
                begin
                  self._pitch := -90.0;
                  self._head  := round(-2.0 * 1800.0/PI * ArcTan2(self._x,-self._w)) / 10.0;
                end
              else
                begin
                  self._roll  := round(1800.0/PI * ArcTan2(2.0 * (self._w * self._x + self._y * self._z),sqw - sqx - sqy + sqz)) / 10.0;
                  self._pitch := round(1800.0/PI * ArcSin(2.0 * delta / norm)) / 10.0;
                  self._head  := round(1800.0/PI * ArcTan2(2.0 * (self._x * self._y + self._z * self._w),sqw + sqx - sqy - sqz)) / 10.0;
                end;
            end;
          self._angles_stamp := self._qt_stamp;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the estimated roll angle, based on the integration of
  ///   gyroscopic measures combined with acceleration and
  ///   magnetic field measurements.
  /// <para>
  ///   The axis corresponding to the roll angle can be mapped to any
  ///   of the device X, Y or Z physical directions using methods of
  ///   the class <c>YRefFrame</c>.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating-point number corresponding to roll angle
  ///   in degrees, between -180 and +180.
  /// </returns>
  ///-
  function TYGyro.get_roll():double;
    begin
      self._loadAngles;
      result := self._roll;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the estimated pitch angle, based on the integration of
  ///   gyroscopic measures combined with acceleration and
  ///   magnetic field measurements.
  /// <para>
  ///   The axis corresponding to the pitch angle can be mapped to any
  ///   of the device X, Y or Z physical directions using methods of
  ///   the class <c>YRefFrame</c>.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating-point number corresponding to pitch angle
  ///   in degrees, between -90 and +90.
  /// </returns>
  ///-
  function TYGyro.get_pitch():double;
    begin
      self._loadAngles;
      result := self._pitch;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the estimated heading angle, based on the integration of
  ///   gyroscopic measures combined with acceleration and
  ///   magnetic field measurements.
  /// <para>
  ///   The axis corresponding to the heading can be mapped to any
  ///   of the device X, Y or Z physical directions using methods of
  ///   the class <c>YRefFrame</c>.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating-point number corresponding to heading
  ///   in degrees, between 0 and 360.
  /// </returns>
  ///-
  function TYGyro.get_heading():double;
    begin
      self._loadAngles;
      result := self._head;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the <c>w</c> component (real part) of the quaternion
  ///   describing the device estimated orientation, based on the
  ///   integration of gyroscopic measures combined with acceleration and
  ///   magnetic field measurements.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating-point number corresponding to the <c>w</c>
  ///   component of the quaternion.
  /// </returns>
  ///-
  function TYGyro.get_quaternionW():double;
    begin
      self._loadQuaternion;
      result := self._w;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the <c>x</c> component of the quaternion
  ///   describing the device estimated orientation, based on the
  ///   integration of gyroscopic measures combined with acceleration and
  ///   magnetic field measurements.
  /// <para>
  ///   The <c>x</c> component is
  ///   mostly correlated with rotations on the roll axis.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating-point number corresponding to the <c>x</c>
  ///   component of the quaternion.
  /// </returns>
  ///-
  function TYGyro.get_quaternionX():double;
    begin
      self._loadQuaternion;
      result := self._x;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the <c>y</c> component of the quaternion
  ///   describing the device estimated orientation, based on the
  ///   integration of gyroscopic measures combined with acceleration and
  ///   magnetic field measurements.
  /// <para>
  ///   The <c>y</c> component is
  ///   mostly correlated with rotations on the pitch axis.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating-point number corresponding to the <c>y</c>
  ///   component of the quaternion.
  /// </returns>
  ///-
  function TYGyro.get_quaternionY():double;
    begin
      self._loadQuaternion;
      result := self._y;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the <c>x</c> component of the quaternion
  ///   describing the device estimated orientation, based on the
  ///   integration of gyroscopic measures combined with acceleration and
  ///   magnetic field measurements.
  /// <para>
  ///   The <c>x</c> component is
  ///   mostly correlated with changes of heading.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating-point number corresponding to the <c>z</c>
  ///   component of the quaternion.
  /// </returns>
  ///-
  function TYGyro.get_quaternionZ():double;
    begin
      self._loadQuaternion;
      result := self._z;
      exit;
    end;


  ////
  /// <summary>
  ///   Registers a callback function that will be invoked each time that the estimated
  ///   device orientation has changed.
  /// <para>
  ///   The call frequency is typically around 95Hz during a move.
  ///   The callback is invoked only during the execution of <c>ySleep</c> or <c>yHandleEvents</c>.
  ///   This provides control over the time when the callback is triggered.
  ///   For good responsiveness, remember to call one of these two functions periodically.
  ///   To unregister a callback, pass a null pointer as argument.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="callback">
  ///   the callback function to invoke, or a null pointer.
  ///   The callback function should take five arguments:
  ///   the YGyro object of the turning device, and the floating
  ///   point values of the four components w, x, y and z
  ///   (as floating-point numbers).
  /// @noreturn
  /// </param>
  ///-
  function TYGyro.registerQuaternionCallback(callback: TYQuatCallback):LongInt;
    begin
      self._quatCallback := callback;
      if (addr(callback) <> nil) then
        begin
          if self._loadQuaternion <> YAPI_SUCCESS then
            begin
              result := YAPI_DEVICE_NOT_FOUND;
              exit;
            end;
          self._qt_w.set_userData(self);
          self._qt_x.set_userData(self);
          self._qt_y.set_userData(self);
          self._qt_z.set_userData(self);
          self._qt_w.registerValueCallback(yInternalGyroCallback);
          self._qt_x.registerValueCallback(yInternalGyroCallback);
          self._qt_y.registerValueCallback(yInternalGyroCallback);
          self._qt_z.registerValueCallback(yInternalGyroCallback);
        end
      else
        begin
          if not((addr(self._anglesCallback) <> nil)) then
            begin
              self._qt_w.registerValueCallback(TYQtValueCallback(nil));
              self._qt_x.registerValueCallback(TYQtValueCallback(nil));
              self._qt_y.registerValueCallback(TYQtValueCallback(nil));
              self._qt_z.registerValueCallback(TYQtValueCallback(nil));
            end;
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Registers a callback function that will be invoked each time that the estimated
  ///   device orientation has changed.
  /// <para>
  ///   The call frequency is typically around 95Hz during a move.
  ///   The callback is invoked only during the execution of <c>ySleep</c> or <c>yHandleEvents</c>.
  ///   This provides control over the time when the callback is triggered.
  ///   For good responsiveness, remember to call one of these two functions periodically.
  ///   To unregister a callback, pass a null pointer as argument.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="callback">
  ///   the callback function to invoke, or a null pointer.
  ///   The callback function should take four arguments:
  ///   the YGyro object of the turning device, and the floating
  ///   point values of the three angles roll, pitch and heading
  ///   in degrees (as floating-point numbers).
  /// @noreturn
  /// </param>
  ///-
  function TYGyro.registerAnglesCallback(callback: TYAnglesCallback):LongInt;
    begin
      self._anglesCallback := callback;
      if (addr(callback) <> nil) then
        begin
          if self._loadQuaternion <> YAPI_SUCCESS then
            begin
              result := YAPI_DEVICE_NOT_FOUND;
              exit;
            end;
          self._qt_w.set_userData(self);
          self._qt_x.set_userData(self);
          self._qt_y.set_userData(self);
          self._qt_z.set_userData(self);
          self._qt_w.registerValueCallback(yInternalGyroCallback);
          self._qt_x.registerValueCallback(yInternalGyroCallback);
          self._qt_y.registerValueCallback(yInternalGyroCallback);
          self._qt_z.registerValueCallback(yInternalGyroCallback);
        end
      else
        begin
          if not((addr(self._quatCallback) <> nil)) then
            begin
              self._qt_w.registerValueCallback(TYQtValueCallback(nil));
              self._qt_x.registerValueCallback(TYQtValueCallback(nil));
              self._qt_y.registerValueCallback(TYQtValueCallback(nil));
              self._qt_z.registerValueCallback(TYQtValueCallback(nil));
            end;
        end;
      result := 0;
      exit;
    end;


  function TYGyro._invokeGyroCallbacks(qtIndex: LongInt; qtValue: double):LongInt;
    begin
      case qtIndex - 1 of
        0 :
          begin
            self._w := qtValue;
          end;
        1 :
          begin
            self._x := qtValue;
          end;
        2 :
          begin
            self._y := qtValue;
          end;
        3 :
          begin
            self._z := qtValue;
          end;
      end;;
      if qtIndex < 4 then
        begin
          result := 0;
          exit;
        end;
      self._qt_stamp := ((yGetTickCount) and ($07FFFFFFF));
      if (addr(self._quatCallback) <> nil) then
        begin
          self._quatCallback(self, self._w, self._x, self._y, self._z);
        end;
      if (addr(self._anglesCallback) <> nil) then
        begin
          self._loadAngles;
          self._anglesCallback(self, self._roll, self._pitch, self._head);
        end;
      result := 0;
      exit;
    end;


  function TYGyro.nextGyro(): TYGyro;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextGyro := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextGyro := nil;
          exit;
        end;
      nextGyro := TYGyro.FindGyro(hwid);
    end;

  class function TYGyro.FirstGyro(): TYGyro;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Gyro', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYGyro.FindGyro(serial+'.'+funcId);
    end;

//--- (end of generated code: YGyro implementation)

//--- (generated code: Gyro functions)

  function yFindGyro(func:string): TYGyro;
    begin
      result := TYGyro.FindGyro(func);
    end;

  function yFirstGyro(): TYGyro;
    begin
      result := TYGyro.FirstGyro();
    end;

  procedure _GyroCleanup();
    begin
    end;

//--- (end of generated code: Gyro functions)

initialization
  //--- (generated code: Qt initialization)
  //--- (end of generated code: Qt initialization)
  //--- (generated code: Gyro initialization)
  //--- (end of generated code: Gyro initialization)

finalization
  //--- (generated code: Qt cleanup)
  _QtCleanup();
  //--- (end of generated code: Qt cleanup)
  //--- (generated code: Gyro cleanup)
  _GyroCleanup();
  //--- (end of generated code: Gyro cleanup)
end.
