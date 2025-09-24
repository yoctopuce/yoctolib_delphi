{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindSdi12Port(), the high-level API for Sdi12Port functions
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


unit yocto_sdi12port;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (generated code: YSdi12Port yapiwrapper declaration)
//--- (end of generated code: YSdi12Port yapiwrapper declaration)

//--- (generated code: YSdi12SnoopingRecord definitions)

//--- (end of generated code: YSdi12SnoopingRecord definitions)

//--- (generated code: YSdi12SensorInfo definitions)

//--- (end of generated code: YSdi12SensorInfo definitions)

//--- (generated code: YSdi12Port definitions)

const Y_RXCOUNT_INVALID               = YAPI_INVALID_UINT;
const Y_TXCOUNT_INVALID               = YAPI_INVALID_UINT;
const Y_ERRCOUNT_INVALID              = YAPI_INVALID_UINT;
const Y_RXMSGCOUNT_INVALID            = YAPI_INVALID_UINT;
const Y_TXMSGCOUNT_INVALID            = YAPI_INVALID_UINT;
const Y_LASTMSG_INVALID               = YAPI_INVALID_STRING;
const Y_CURRENTJOB_INVALID            = YAPI_INVALID_STRING;
const Y_STARTUPJOB_INVALID            = YAPI_INVALID_STRING;
const Y_JOBMAXTASK_INVALID            = YAPI_INVALID_UINT;
const Y_JOBMAXSIZE_INVALID            = YAPI_INVALID_UINT;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;
const Y_PROTOCOL_INVALID              = YAPI_INVALID_STRING;
const Y_VOLTAGELEVEL_OFF = 0;
const Y_VOLTAGELEVEL_TTL3V = 1;
const Y_VOLTAGELEVEL_TTL3VR = 2;
const Y_VOLTAGELEVEL_TTL5V = 3;
const Y_VOLTAGELEVEL_TTL5VR = 4;
const Y_VOLTAGELEVEL_RS232 = 5;
const Y_VOLTAGELEVEL_RS485 = 6;
const Y_VOLTAGELEVEL_TTL1V8 = 7;
const Y_VOLTAGELEVEL_SDI12 = 8;
const Y_VOLTAGELEVEL_INVALID = -1;
const Y_SERIALMODE_INVALID            = YAPI_INVALID_STRING;

//--- (end of generated code: YSdi12Port definitions)


type

  TYSdi12Port = class;

  TYSdi12SnoopingRecord = class;
  TYSdi12SnoopingRecordArray = array of TYSdi12SnoopingRecord;

  TYSdi12SensorInfo = class;
  TYSdi12SensorInfoArray = array of TYSdi12SensorInfo;

  TStringArrayArray = array of TStringArray;

  //--- (generated code: YSdi12SnoopingRecord class start)
  ////
  /// <summary>
  ///   T
  /// <para>
  ///   YSdi12SnoopingRecord Class: Intercepted SDI12 message description, returned by
  ///   <c>sdi12Port.snoopMessages</c> method
  /// </para>
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  ///-
  TYSdi12SnoopingRecord=class(TObject)
  //--- (end of generated code: YSdi12SnoopingRecord class start)
  protected
  //--- (generated code: YSdi12SnoopingRecord declaration)
    // Attributes (function value cache)
    _tim                      : LongInt;
    _pos                      : LongInt;
    _dir                      : LongInt;
    _msg                      : string;
    //--- (end of generated code: YSdi12SnoopingRecord declaration)

  public
    constructor create(data:string);
    //--- (generated code: YSdi12SnoopingRecord accessors declaration)

    ////
    /// <summary>
    ///   Returns the elapsed time, in ms, since the beginning of the preceding message.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the elapsed time, in ms, since the beginning of the preceding message.
    /// </returns>
    ///-
    function get_time():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the absolute position of the message end.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the absolute position of the message end.
    /// </returns>
    ///-
    function get_pos():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the message direction (RX=0, TX=1).
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the message direction (RX=0, TX=1).
    /// </returns>
    ///-
    function get_direction():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the message content.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the message content.
    /// </returns>
    ///-
    function get_message():string; overload; virtual;


  //--- (end of generated code: YSdi12SnoopingRecord accessors declaration)
  end;


  //--- (generated code: YSdi12SensorInfo class start)
  ////
  /// <summary>
  ///   TYSdi12SensorInfo Class: Description of a discovered SDI12 sensor, returned by <c>sdi12Port.discoverSingleSensor</c> and <c>sdi12Port.
  /// <para>
  ///   discoverAllSensors</c> methods
  /// </para>
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  ///-
  TYSdi12SensorInfo=class(TObject)
  //--- (end of generated code: YSdi12SensorInfo class start)
  protected
  //--- (generated code: YSdi12SensorInfo declaration)
    // Attributes (function value cache)
    _sdi12Port                : TYSdi12Port;
    _isValid                  : boolean;
    _addr                     : string;
    _proto                    : string;
    _mfg                      : string;
    _model                    : string;
    _ver                      : string;
    _sn                       : string;
    _valuesDesc               : TStringArrayArray;
    //--- (end of generated code: YSdi12SensorInfo declaration)

  public
    constructor Create(sdi12Port: TYSdi12Port; infoStr: string);

    procedure _throw(errcode: LongInt; msg: string); overload; virtual;

    //--- (generated code: YSdi12SensorInfo accessors declaration)

    ////
    /// <summary>
    ///   Returns the sensor state.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the sensor state.
    /// </returns>
    ///-
    function isValid():boolean; overload; virtual;

    ////
    /// <summary>
    ///   Returns the sensor address.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the sensor address.
    /// </returns>
    ///-
    function get_sensorAddress():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the compatible SDI-12 version of the sensor.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the compatible SDI-12 version of the sensor.
    /// </returns>
    ///-
    function get_sensorProtocol():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the sensor vendor identification.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the sensor vendor identification.
    /// </returns>
    ///-
    function get_sensorVendor():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the sensor model number.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the sensor model number.
    /// </returns>
    ///-
    function get_sensorModel():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the sensor version.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the sensor version.
    /// </returns>
    ///-
    function get_sensorVersion():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the sensor serial number.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the sensor serial number.
    /// </returns>
    ///-
    function get_sensorSerial():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the number of sensor measurements.
    /// <para>
    ///   This function only works if the sensor is in version 1.4 SDI-12
    ///   and supports metadata commands.
    /// </para>
    /// </summary>
    /// <returns>
    ///   the number of sensor measurements.
    /// </returns>
    ///-
    function get_measureCount():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the sensor measurement command.
    /// <para>
    ///   This function only works if the sensor is in version 1.4 SDI-12
    ///   and supports metadata commands.
    /// </para>
    /// </summary>
    /// <param name="measureIndex">
    ///   measurement index
    /// </param>
    /// <returns>
    ///   the sensor measurement command.
    ///   On failure, throws an exception or returns an empty string.
    /// </returns>
    ///-
    function get_measureCommand(measureIndex: LongInt):string; overload; virtual;

    ////
    /// <summary>
    ///   Returns sensor measurement position.
    /// <para>
    ///   This function only works if the sensor is in version 1.4 SDI-12
    ///   and supports metadata commands.
    /// </para>
    /// </summary>
    /// <param name="measureIndex">
    ///   measurement index
    /// </param>
    /// <returns>
    ///   the sensor measurement command.
    ///   On failure, throws an exception or returns 0.
    /// </returns>
    ///-
    function get_measurePosition(measureIndex: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the measured value symbol.
    /// <para>
    ///   This function only works if the sensor is in version 1.4 SDI-12
    ///   and supports metadata commands.
    /// </para>
    /// </summary>
    /// <param name="measureIndex">
    ///   measurement index
    /// </param>
    /// <returns>
    ///   the sensor measurement command.
    ///   On failure, throws an exception or returns an empty string.
    /// </returns>
    ///-
    function get_measureSymbol(measureIndex: LongInt):string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the unit of the measured value.
    /// <para>
    ///   This function only works if the sensor is in version 1.4 SDI-12
    ///   and supports metadata commands.
    /// </para>
    /// </summary>
    /// <param name="measureIndex">
    ///   measurement index
    /// </param>
    /// <returns>
    ///   the sensor measurement command.
    ///   On failure, throws an exception or returns an empty string.
    /// </returns>
    ///-
    function get_measureUnit(measureIndex: LongInt):string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the description of the measured value.
    /// <para>
    ///   This function only works if the sensor is in version 1.4 SDI-12
    ///   and supports metadata commands.
    /// </para>
    /// </summary>
    /// <param name="measureIndex">
    ///   measurement index
    /// </param>
    /// <returns>
    ///   the sensor measurement command.
    ///   On failure, throws an exception or returns an empty string.
    /// </returns>
    ///-
    function get_measureDescription(measureIndex: LongInt):string; overload; virtual;

    function get_typeMeasure():TStringArrayArray; overload; virtual;

    procedure _parseInfoStr(infoStr: string); overload; virtual;

    procedure _queryValueInfo(); overload; virtual;


  //--- (end of generated code: YSdi12SensorInfo accessors declaration)
  end;

  //--- (generated code: YSdi12Port class start)
  TYSdi12PortValueCallback = procedure(func: TYSdi12Port; value:string);
  TYSdi12PortTimedReportCallback = procedure(func: TYSdi12Port; value:TYMeasure);

  ////
  /// <summary>
  ///   TYSdi12Port Class: SDI12 port control interface
  /// <para>
  ///   The <c>YSdi12Port</c> class allows you to fully drive a Yoctopuce SDI12 port.
  ///   It can be used to send and receive data, and to configure communication
  ///   parameters (baud rate, bit count, parity, flow control and protocol).
  ///   Note that Yoctopuce SDI12 ports are not exposed as virtual COM ports.
  ///   They are meant to be used in the same way as all Yoctopuce devices.
  /// </para>
  /// </summary>
  ///-
  TYSdi12Port=class(TYFunction)
  //--- (end of generated code: YSdi12Port class start)
  protected
  //--- (generated code: YSdi12Port declaration)
    // Attributes (function value cache)
    _rxCount                  : LongInt;
    _txCount                  : LongInt;
    _errCount                 : LongInt;
    _rxMsgCount               : LongInt;
    _txMsgCount               : LongInt;
    _lastMsg                  : string;
    _currentJob               : string;
    _startupJob               : string;
    _jobMaxTask               : LongInt;
    _jobMaxSize               : LongInt;
    _command                  : string;
    _protocol                 : string;
    _voltageLevel             : Integer;
    _serialMode               : string;
    _valueCallbackSdi12Port   : TYSdi12PortValueCallback;
    _rxptr                    : LongInt;
    _rxbuff                   : TByteArray;
    _rxbuffptr                : LongInt;
    _eventPos                 : LongInt;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of generated code: YSdi12Port declaration)

  public
    //--- (generated code: YSdi12Port accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the total number of bytes received since last reset.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the total number of bytes received since last reset
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.RXCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_rxCount():LongInt;

    ////
    /// <summary>
    ///   Returns the total number of bytes transmitted since last reset.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the total number of bytes transmitted since last reset
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.TXCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_txCount():LongInt;

    ////
    /// <summary>
    ///   Returns the total number of communication errors detected since last reset.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the total number of communication errors detected since last reset
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.ERRCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_errCount():LongInt;

    ////
    /// <summary>
    ///   Returns the total number of messages received since last reset.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the total number of messages received since last reset
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.RXMSGCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_rxMsgCount():LongInt;

    ////
    /// <summary>
    ///   Returns the total number of messages send since last reset.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the total number of messages send since last reset
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.TXMSGCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_txMsgCount():LongInt;

    ////
    /// <summary>
    ///   Returns the latest message fully received.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the latest message fully received
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.LASTMSG_INVALID</c>.
    /// </para>
    ///-
    function get_lastMsg():string;

    ////
    /// <summary>
    ///   Returns the name of the job file currently in use.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the name of the job file currently in use
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.CURRENTJOB_INVALID</c>.
    /// </para>
    ///-
    function get_currentJob():string;

    ////
    /// <summary>
    ///   Selects a job file to run immediately.
    /// <para>
    ///   If an empty string is
    ///   given as argument, stops running current job file.
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
    function set_currentJob(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the job file to use when the device is powered on.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the job file to use when the device is powered on
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.STARTUPJOB_INVALID</c>.
    /// </para>
    ///-
    function get_startupJob():string;

    ////
    /// <summary>
    ///   Changes the job to use when the device is powered on.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the job to use when the device is powered on
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
    function set_startupJob(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the maximum number of tasks in a job that the device can handle.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximum number of tasks in a job that the device can handle
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.JOBMAXTASK_INVALID</c>.
    /// </para>
    ///-
    function get_jobMaxTask():LongInt;

    ////
    /// <summary>
    ///   Returns maximum size allowed for job files.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to maximum size allowed for job files
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.JOBMAXSIZE_INVALID</c>.
    /// </para>
    ///-
    function get_jobMaxSize():LongInt;

    function get_command():string;

    function set_command(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the type of protocol used over the serial line, as a string.
    /// <para>
    ///   Possible values are "Line" for ASCII messages separated by CR and/or LF,
    ///   "Frame:[timeout]ms" for binary messages separated by a delay time,
    ///   "Char" for a continuous ASCII stream or
    ///   "Byte" for a continuous binary stream.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the type of protocol used over the serial line, as a string
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.PROTOCOL_INVALID</c>.
    /// </para>
    ///-
    function get_protocol():string;

    ////
    /// <summary>
    ///   Changes the type of protocol used over the serial line.
    /// <para>
    ///   Possible values are "Line" for ASCII messages separated by CR and/or LF,
    ///   "Frame:[timeout]ms" for binary messages separated by a delay time,
    ///   "Char" for a continuous ASCII stream or
    ///   "Byte" for a continuous binary stream.
    ///   The suffix "/[wait]ms" can be added to reduce the transmit rate so that there
    ///   is always at lest the specified number of milliseconds between each bytes sent.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the type of protocol used over the serial line
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
    function set_protocol(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the voltage level used on the serial line.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YSdi12Port.VOLTAGELEVEL_OFF</c>, <c>YSdi12Port.VOLTAGELEVEL_TTL3V</c>,
    ///   <c>YSdi12Port.VOLTAGELEVEL_TTL3VR</c>, <c>YSdi12Port.VOLTAGELEVEL_TTL5V</c>,
    ///   <c>YSdi12Port.VOLTAGELEVEL_TTL5VR</c>, <c>YSdi12Port.VOLTAGELEVEL_RS232</c>,
    ///   <c>YSdi12Port.VOLTAGELEVEL_RS485</c>, <c>YSdi12Port.VOLTAGELEVEL_TTL1V8</c> and
    ///   <c>YSdi12Port.VOLTAGELEVEL_SDI12</c> corresponding to the voltage level used on the serial line
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.VOLTAGELEVEL_INVALID</c>.
    /// </para>
    ///-
    function get_voltageLevel():Integer;

    ////
    /// <summary>
    ///   Changes the voltage type used on the serial line.
    /// <para>
    ///   Valid
    ///   values  will depend on the Yoctopuce device model featuring
    ///   the serial port feature.  Check your device documentation
    ///   to find out which values are valid for that specific model.
    ///   Trying to set an invalid value will have no effect.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YSdi12Port.VOLTAGELEVEL_OFF</c>, <c>YSdi12Port.VOLTAGELEVEL_TTL3V</c>,
    ///   <c>YSdi12Port.VOLTAGELEVEL_TTL3VR</c>, <c>YSdi12Port.VOLTAGELEVEL_TTL5V</c>,
    ///   <c>YSdi12Port.VOLTAGELEVEL_TTL5VR</c>, <c>YSdi12Port.VOLTAGELEVEL_RS232</c>,
    ///   <c>YSdi12Port.VOLTAGELEVEL_RS485</c>, <c>YSdi12Port.VOLTAGELEVEL_TTL1V8</c> and
    ///   <c>YSdi12Port.VOLTAGELEVEL_SDI12</c> corresponding to the voltage type used on the serial line
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
    function set_voltageLevel(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the serial port communication parameters, as a string such as
    ///   "1200,7E1,Simplex".
    /// <para>
    ///   The string includes the baud rate, the number of data bits,
    ///   the parity, and the number of stop bits. The suffix "Simplex" denotes
    ///   the fact that transmission in both directions is multiplexed on the
    ///   same transmission line.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the serial port communication parameters, as a string such as
    ///   "1200,7E1,Simplex"
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSdi12Port.SERIALMODE_INVALID</c>.
    /// </para>
    ///-
    function get_serialMode():string;

    ////
    /// <summary>
    ///   Changes the serial port communication parameters, with a string such as
    ///   "1200,7E1,Simplex".
    /// <para>
    ///   The string includes the baud rate, the number of data bits,
    ///   the parity, and the number of stop bits. The suffix "Simplex" denotes
    ///   the fact that transmission in both directions is multiplexed on the
    ///   same transmission line.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the serial port communication parameters, with a string such as
    ///   "1200,7E1,Simplex"
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
    function set_serialMode(newval:string):integer;

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
    ///   Use the method <c>YSdi12Port.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YSdi12Port</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindSdi12Port(func: string):TYSdi12Port;

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
    function registerValueCallback(callback: TYSdi12PortValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function sendCommand(text: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reads a single line (or message) from the receive buffer, starting at current stream position.
    /// <para>
    ///   This function is intended to be used when the serial port is configured for a message protocol,
    ///   such as 'Line' mode or frame protocols.
    /// </para>
    /// <para>
    ///   If data at current stream position is not available anymore in the receive buffer,
    ///   the function returns the oldest available line and moves the stream position just after.
    ///   If no new full line is received, the function returns an empty line.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with a single line of text
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function readLine():string; overload; virtual;

    ////
    /// <summary>
    ///   Searches for incoming messages in the serial port receive buffer matching a given pattern,
    ///   starting at current position.
    /// <para>
    ///   This function will only compare and return printable characters
    ///   in the message strings. Binary protocols are handled as hexadecimal strings.
    /// </para>
    /// <para>
    ///   The search returns all messages matching the expression provided as argument in the buffer.
    ///   If no matching message is found, the search waits for one up to the specified maximum timeout
    ///   (in milliseconds).
    /// </para>
    /// </summary>
    /// <param name="pattern">
    ///   a limited regular expression describing the expected message format,
    ///   or an empty string if all messages should be returned (no filtering).
    ///   When using binary protocols, the format applies to the hexadecimal
    ///   representation of the message.
    /// </param>
    /// <param name="maxWait">
    ///   the maximum number of milliseconds to wait for a message if none is found
    ///   in the receive buffer.
    /// </param>
    /// <returns>
    ///   an array of strings containing the messages found, if any.
    ///   Binary messages are converted to hexadecimal representation.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function readMessages(pattern: string; maxWait: LongInt):TStringArray; overload; virtual;

    ////
    /// <summary>
    ///   Changes the current internal stream position to the specified value.
    /// <para>
    ///   This function
    ///   does not affect the device, it only changes the value stored in the API object
    ///   for the next read operations.
    /// </para>
    /// </summary>
    /// <param name="absPos">
    ///   the absolute position index for next read operations.
    /// </param>
    /// <returns>
    ///   nothing.
    /// </returns>
    ///-
    function read_seek(absPos: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the current absolute stream position pointer of the API object.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the absolute position index for next read operations.
    /// </returns>
    ///-
    function read_tell():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the number of bytes available to read in the input buffer starting from the
    ///   current absolute stream position pointer of the API object.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the number of bytes available to read
    /// </returns>
    ///-
    function read_avail():LongInt; overload; virtual;

    function end_tell():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a text line query to the serial port, and reads the reply, if any.
    /// <para>
    ///   This function is intended to be used when the serial port is configured for 'Line' protocol.
    /// </para>
    /// </summary>
    /// <param name="query">
    ///   the line query to send (without CR/LF)
    /// </param>
    /// <param name="maxWait">
    ///   the maximum number of milliseconds to wait for a reply.
    /// </param>
    /// <returns>
    ///   the next text line received after sending the text query, as a string.
    ///   Additional lines can be obtained by calling readLine or readMessages.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function queryLine(query: string; maxWait: LongInt):string; overload; virtual;

    ////
    /// <summary>
    ///   Sends a binary message to the serial port, and reads the reply, if any.
    /// <para>
    ///   This function is intended to be used when the serial port is configured for
    ///   Frame-based protocol.
    /// </para>
    /// </summary>
    /// <param name="hexString">
    ///   the message to send, coded in hexadecimal
    /// </param>
    /// <param name="maxWait">
    ///   the maximum number of milliseconds to wait for a reply.
    /// </param>
    /// <returns>
    ///   the next frame received after sending the message, as a hex string.
    ///   Additional frames can be obtained by calling readHex or readMessages.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function queryHex(hexString: string; maxWait: LongInt):string; overload; virtual;

    ////
    /// <summary>
    ///   Saves the job definition string (JSON data) into a job file.
    /// <para>
    ///   The job file can be later enabled using <c>selectJob()</c>.
    /// </para>
    /// </summary>
    /// <param name="jobfile">
    ///   name of the job file to save on the device filesystem
    /// </param>
    /// <param name="jsonDef">
    ///   a string containing a JSON definition of the job
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function uploadJob(jobfile: string; jsonDef: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Load and start processing the specified job file.
    /// <para>
    ///   The file must have
    ///   been previously created using the user interface or uploaded on the
    ///   device filesystem using the <c>uploadJob()</c> function.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="jobfile">
    ///   name of the job file (on the device filesystem)
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function selectJob(jobfile: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Clears the serial port buffer and resets counters to zero.
    /// <para>
    /// </para>
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
    function reset():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a single byte to the serial port.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="code">
    ///   the byte to send
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function writeByte(code: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends an ASCII string to the serial port, as is.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="text">
    ///   the text string to send
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function writeStr(text: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a binary buffer to the serial port, as is.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="buff">
    ///   the binary buffer to send
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function writeBin(buff: TByteArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a byte sequence (provided as a list of bytes) to the serial port.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="byteList">
    ///   a list of byte codes
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function writeArray(byteList: TLongIntArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a byte sequence (provided as a hexadecimal string) to the serial port.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="hexString">
    ///   a string of hexadecimal byte codes
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function writeHex(hexString: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends an ASCII string to the serial port, followed by a line break (CR LF).
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="text">
    ///   the text string to send
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function writeLine(text: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reads one byte from the receive buffer, starting at current stream position.
    /// <para>
    ///   If data at current stream position is not available anymore in the receive buffer,
    ///   or if there is no data available yet, the function returns YAPI_NO_MORE_DATA.
    /// </para>
    /// </summary>
    /// <returns>
    ///   the next byte
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function readByte():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reads data from the receive buffer as a string, starting at current stream position.
    /// <para>
    ///   If data at current stream position is not available anymore in the receive buffer, the
    ///   function performs a short read.
    /// </para>
    /// </summary>
    /// <param name="nChars">
    ///   the maximum number of characters to read
    /// </param>
    /// <returns>
    ///   a string with receive buffer contents
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function readStr(nChars: LongInt):string; overload; virtual;

    ////
    /// <summary>
    ///   Reads data from the receive buffer as a binary buffer, starting at current stream position.
    /// <para>
    ///   If data at current stream position is not available anymore in the receive buffer, the
    ///   function performs a short read.
    /// </para>
    /// </summary>
    /// <param name="nChars">
    ///   the maximum number of bytes to read
    /// </param>
    /// <returns>
    ///   a binary object with receive buffer contents
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function readBin(nChars: LongInt):TByteArray; overload; virtual;

    ////
    /// <summary>
    ///   Reads data from the receive buffer as a list of bytes, starting at current stream position.
    /// <para>
    ///   If data at current stream position is not available anymore in the receive buffer, the
    ///   function performs a short read.
    /// </para>
    /// </summary>
    /// <param name="nChars">
    ///   the maximum number of bytes to read
    /// </param>
    /// <returns>
    ///   a sequence of bytes with receive buffer contents
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function readArray(nChars: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Reads data from the receive buffer as a hexadecimal string, starting at current stream position.
    /// <para>
    ///   If data at current stream position is not available anymore in the receive buffer, the
    ///   function performs a short read.
    /// </para>
    /// </summary>
    /// <param name="nBytes">
    ///   the maximum number of bytes to read
    /// </param>
    /// <returns>
    ///   a string with receive buffer contents, encoded in hexadecimal
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function readHex(nBytes: LongInt):string; overload; virtual;

    ////
    /// <summary>
    ///   Sends a SDI-12 query to the bus, and reads the sensor immediate reply.
    /// <para>
    ///   This function is intended to be used when the serial port is configured for 'SDI-12' protocol.
    /// </para>
    /// </summary>
    /// <param name="sensorAddr">
    ///   the sensor address, as a string
    /// </param>
    /// <param name="cmd">
    ///   the SDI12 query to send (without address and exclamation point)
    /// </param>
    /// <param name="maxWait">
    ///   the maximum timeout to wait for a reply from sensor, in millisecond
    /// </param>
    /// <returns>
    ///   the reply returned by the sensor, without newline, as a string.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function querySdi12(sensorAddr: string; cmd: string; maxWait: LongInt):string; overload; virtual;

    ////
    /// <summary>
    ///   Sends a discovery command to the bus, and reads the sensor information reply.
    /// <para>
    ///   This function is intended to be used when the serial port is configured for 'SDI-12' protocol.
    ///   This function work when only one sensor is connected.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the reply returned by the sensor, as a YSdi12SensorInfo object.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function discoverSingleSensor():TYSdi12SensorInfo; overload; virtual;

    ////
    /// <summary>
    ///   Sends a discovery command to the bus, and reads all sensors information reply.
    /// <para>
    ///   This function is intended to be used when the serial port is configured for 'SDI-12' protocol.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   all the information from every connected sensor, as an array of YSdi12SensorInfo object.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function discoverAllSensors():TYSdi12SensorInfoArray; overload; virtual;

    ////
    /// <summary>
    ///   Sends a mesurement command to the SDI-12 bus, and reads the sensor immediate reply.
    /// <para>
    ///   The supported commands are:
    ///   M: Measurement start control
    ///   M1...M9: Additional measurement start command
    ///   D: Measurement reading control
    ///   This function is intended to be used when the serial port is configured for 'SDI-12' protocol.
    /// </para>
    /// </summary>
    /// <param name="sensorAddr">
    ///   the sensor address, as a string
    /// </param>
    /// <param name="measCmd">
    ///   the SDI12 query to send (without address and exclamation point)
    /// </param>
    /// <param name="maxWait">
    ///   the maximum timeout to wait for a reply from sensor, in millisecond
    /// </param>
    /// <returns>
    ///   the reply returned by the sensor, without newline, as a list of float.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function readSensor(sensorAddr: string; measCmd: string; maxWait: LongInt):TDoubleArray; overload; virtual;

    ////
    /// <summary>
    ///   Changes the address of the selected sensor, and returns the sensor information with the new address.
    /// <para>
    ///   This function is intended to be used when the serial port is configured for 'SDI-12' protocol.
    /// </para>
    /// </summary>
    /// <param name="oldAddress">
    ///   Actual sensor address, as a string
    /// </param>
    /// <param name="newAddress">
    ///   New sensor address, as a string
    /// </param>
    /// <returns>
    ///   the sensor address and information , as a YSdi12SensorInfo object.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function changeAddress(oldAddress: string; newAddress: string):TYSdi12SensorInfo; overload; virtual;

    ////
    /// <summary>
    ///   Sends a information command to the bus, and reads sensors information selected.
    /// <para>
    ///   This function is intended to be used when the serial port is configured for 'SDI-12' protocol.
    /// </para>
    /// </summary>
    /// <param name="sensorAddr">
    ///   Sensor address, as a string
    /// </param>
    /// <returns>
    ///   the reply returned by the sensor, as a YSdi12Port object.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function getSensorInformation(sensorAddr: string):TYSdi12SensorInfo; overload; virtual;

    ////
    /// <summary>
    ///   Sends a information command to the bus, and reads sensors information selected.
    /// <para>
    ///   This function is intended to be used when the serial port is configured for 'SDI-12' protocol.
    /// </para>
    /// </summary>
    /// <param name="sensorAddr">
    ///   Sensor address, as a string
    /// </param>
    /// <returns>
    ///   the reply returned by the sensor, as a YSdi12Port object.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function readConcurrentMeasurements(sensorAddr: string):TDoubleArray; overload; virtual;

    ////
    /// <summary>
    ///   Sends a information command to the bus, and reads sensors information selected.
    /// <para>
    ///   This function is intended to be used when the serial port is configured for 'SDI-12' protocol.
    /// </para>
    /// </summary>
    /// <param name="sensorAddr">
    ///   Sensor address, as a string
    /// </param>
    /// <returns>
    ///   the reply returned by the sensor, as a YSdi12Port object.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function requestConcurrentMeasurements(sensorAddr: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Retrieves messages (both direction) in the SDI12 port buffer, starting at current position.
    /// <para>
    /// </para>
    /// <para>
    ///   If no message is found, the search waits for one up to the specified maximum timeout
    ///   (in milliseconds).
    /// </para>
    /// </summary>
    /// <param name="maxWait">
    ///   the maximum number of milliseconds to wait for a message if none is found
    ///   in the receive buffer.
    /// </param>
    /// <param name="maxMsg">
    ///   the maximum number of messages to be returned by the function; up to 254.
    /// </param>
    /// <returns>
    ///   an array of <c>YSdi12SnoopingRecord</c> objects containing the messages found, if any.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function snoopMessagesEx(maxWait: LongInt; maxMsg: LongInt):TYSdi12SnoopingRecordArray; overload; virtual;

    ////
    /// <summary>
    ///   Retrieves messages (both direction) in the SDI12 port buffer, starting at current position.
    /// <para>
    /// </para>
    /// <para>
    ///   If no message is found, the search waits for one up to the specified maximum timeout
    ///   (in milliseconds).
    /// </para>
    /// </summary>
    /// <param name="maxWait">
    ///   the maximum number of milliseconds to wait for a message if none is found
    ///   in the receive buffer.
    /// </param>
    /// <returns>
    ///   an array of <c>YSdi12SnoopingRecord</c> objects containing the messages found, if any.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function snoopMessages(maxWait: LongInt):TYSdi12SnoopingRecordArray; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of SDI12 ports started using <c>yFirstSdi12Port()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned SDI12 ports order.
    ///   If you want to find a specific an SDI12 port, use <c>Sdi12Port.findSdi12Port()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YSdi12Port</c> object, corresponding to
    ///   an SDI12 port currently online, or a <c>NIL</c> pointer
    ///   if there are no more SDI12 ports to enumerate.
    /// </returns>
    ///-
    function nextSdi12Port():TYSdi12Port;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstSdi12Port():TYSdi12Port;
  //--- (end of generated code: YSdi12Port accessors declaration)
  end;

//--- (generated code: YSdi12Port functions declaration)
  ////
  /// <summary>
  ///   Retrieves an SDI12 port for a given identifier.
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
  ///   This function does not require that the SDI12 port is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YSdi12Port.isOnline()</c> to test if the SDI12 port is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   an SDI12 port by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the SDI12 port, for instance
  ///   <c>MyDevice.sdi12Port</c>.
  /// </param>
  /// <returns>
  ///   a <c>YSdi12Port</c> object allowing you to drive the SDI12 port.
  /// </returns>
  ///-
  function yFindSdi12Port(func:string):TYSdi12Port;
  ////
  /// <summary>
  ///   Starts the enumeration of SDI12 ports currently accessible.
  /// <para>
  ///   Use the method <c>YSdi12Port.nextSdi12Port()</c> to iterate on
  ///   next SDI12 ports.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YSdi12Port</c> object, corresponding to
  ///   the first SDI12 port currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstSdi12Port():TYSdi12Port;

//--- (end of generated code: YSdi12Port functions declaration)

implementation

//--- (generated code: YSdi12Port dlldef)
//--- (end of generated code: YSdi12Port dlldef)

  constructor TYSdi12SnoopingRecord.create(data:string);
    var
      p : TJSONparser;
      node : PJSONRECORD;
      tmp : string;
      c: char;
    begin
      p := TJsonParser.create(data,false);
      node:= p.GetChildNode(nil,'t');
      self._tim:=node^.ivalue;
      node:= p.GetChildNode(nil,'p');
      self._pos:=node^.ivalue;
      node:= p.GetChildNode(nil,'m');
      tmp := string(node^.svalue);
      c := tmp[1];
      if (c = '<') then
        self._dir := 1
      else
        self._dir := 0;
      self._msg:=Copy(tmp, 2, Length(tmp)-1);
      p.free;
    end;

//--- (generated code: YSdi12SnoopingRecord implementation)

  function TYSdi12SnoopingRecord.get_time():LongInt;
    begin
      result := self._tim;
      exit;
    end;


  function TYSdi12SnoopingRecord.get_pos():LongInt;
    begin
      result := self._pos;
      exit;
    end;


  function TYSdi12SnoopingRecord.get_direction():LongInt;
    begin
      result := self._dir;
      exit;
    end;


  function TYSdi12SnoopingRecord.get_message():string;
    begin
      result := self._msg;
      exit;
    end;


//--- (end of generated code: YSdi12SnoopingRecord implementation)


  constructor TYSdi12SensorInfo.Create(sdi12Port: TYSdi12Port; infoStr: string);
    begin
      //--- (generated code: YSdi12SensorInfo accessors initialization)
      //--- (end of generated code: YSdi12SensorInfo accessors initialization)
      self._sdi12Port := sdi12Port;
      self._parseInfoStr(infoStr);
    end;


  procedure TYSdi12SensorInfo._throw(errcode: LongInt; msg: string);
    begin
      self._sdi12Port._throw(errcode, msg);
    end;


//--- (generated code: YSdi12SensorInfo implementation)

  function TYSdi12SensorInfo.isValid():boolean;
    begin
      result := self._isValid;
      exit;
    end;


  function TYSdi12SensorInfo.get_sensorAddress():string;
    begin
      result := self._addr;
      exit;
    end;


  function TYSdi12SensorInfo.get_sensorProtocol():string;
    begin
      result := self._proto;
      exit;
    end;


  function TYSdi12SensorInfo.get_sensorVendor():string;
    begin
      result := self._mfg;
      exit;
    end;


  function TYSdi12SensorInfo.get_sensorModel():string;
    begin
      result := self._model;
      exit;
    end;


  function TYSdi12SensorInfo.get_sensorVersion():string;
    begin
      result := self._ver;
      exit;
    end;


  function TYSdi12SensorInfo.get_sensorSerial():string;
    begin
      result := self._sn;
      exit;
    end;


  function TYSdi12SensorInfo.get_measureCount():LongInt;
    begin
      result := length(self._valuesDesc);
      exit;
    end;


  function TYSdi12SensorInfo.get_measureCommand(measureIndex: LongInt):string;
    begin
      if not(measureIndex < length(self._valuesDesc)) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'Invalid measure index');
          result:='';
          exit;
        end;
      result := self._valuesDesc[measureIndex][0];
      exit;
    end;


  function TYSdi12SensorInfo.get_measurePosition(measureIndex: LongInt):LongInt;
    begin
      if not(measureIndex < length(self._valuesDesc)) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'Invalid measure index');
          result:=0;
          exit;
        end;
      result := _atoi(self._valuesDesc[measureIndex][2]);
      exit;
    end;


  function TYSdi12SensorInfo.get_measureSymbol(measureIndex: LongInt):string;
    begin
      if not(measureIndex < length(self._valuesDesc)) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'Invalid measure index');
          result:='';
          exit;
        end;
      result := self._valuesDesc[measureIndex][3];
      exit;
    end;


  function TYSdi12SensorInfo.get_measureUnit(measureIndex: LongInt):string;
    begin
      if not(measureIndex < length(self._valuesDesc)) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'Invalid measure index');
          result:='';
          exit;
        end;
      result := self._valuesDesc[measureIndex][4];
      exit;
    end;


  function TYSdi12SensorInfo.get_measureDescription(measureIndex: LongInt):string;
    begin
      if not(measureIndex < length(self._valuesDesc)) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'Invalid measure index');
          result:='';
          exit;
        end;
      result := self._valuesDesc[measureIndex][5];
      exit;
    end;


  function TYSdi12SensorInfo.get_typeMeasure():TStringArrayArray;
    begin
      result := self._valuesDesc;
      exit;
    end;


  procedure TYSdi12SensorInfo._parseInfoStr(infoStr: string);
    var
      errmsg : string;
    begin
      if Length(infoStr) > 1 then
        begin
          if (Copy(infoStr, 0 + 1, 2) = 'ER') then
            begin
              errmsg := Copy(infoStr, 2 + 1, Length(infoStr)-2);
              self._addr := errmsg;
              self._proto := errmsg;
              self._mfg := errmsg;
              self._model := errmsg;
              self._ver := errmsg;
              self._sn := errmsg;
              self._isValid := false;
            end
          else
            begin
              self._addr := Copy(infoStr, 0 + 1, 1);
              self._proto := Copy(infoStr, 1 + 1, 2);
              self._mfg := Copy(infoStr, 3 + 1, 8);
              self._model := Copy(infoStr, 11 + 1, 6);
              self._ver := Copy(infoStr, 17 + 1, 3);
              self._sn := Copy(infoStr, 20 + 1, Length(infoStr)-20);
              self._isValid := true;
            end;
        end;
    end;


  procedure TYSdi12SensorInfo._queryValueInfo();
    var
      val : TStringArrayArray;
      data : TStringArray;
      infoNbVal : string;
      cmd : string;
      infoVal : string;
      value : string;
      nbVal : LongInt;
      k : LongInt;
      i : LongInt;
      j : LongInt;
      listVal : TStringArray;
      size : LongInt;
      val_pos : LongInt;
      listVal_pos : LongInt;
      data_pos : LongInt;
    begin
      SetLength(data, 0);
      SetLength(listVal, 0);

      k := 0;
      size := 4;
      while k < 10 do
        begin
          infoNbVal := self._sdi12Port.querySdi12(self._addr, 'IM'+inttostr(k), 5000);
          if Length(infoNbVal) > 1 then
            begin
              value := Copy(infoNbVal, 4 + 1, Length(infoNbVal)-4);
              nbVal := _atoi(value);
              if nbVal <> 0 then
                begin
                  val_pos := 0;
                  SetLength(val, nbVal);
                  i := 0;
                  while i < nbVal do
                    begin
                      cmd := 'IM'+inttostr(k)+'_00'+inttostr(i+1);
                      infoVal := self._sdi12Port.querySdi12(self._addr, cmd, 5000);
                      data := _stringSplit(infoVal, ';');
                      data := _stringSplit(data[0], ',');
                      listVal_pos := 0;
                      SetLength(listVal, length(data)+2);
                      data_pos := length(data);
                      SetLength(data, data_pos+size);
                      listVal[listVal_pos] := 'M'+inttostr(k);
                      inc(listVal_pos);
                      listVal[listVal_pos] := IntToStr(i+1);
                      inc(listVal_pos);
                      j := 0;
                      while length(data) < size do
                        begin
                          data[data_pos] := '';
                          inc(data_pos);
                        end;
                      while j < length(data) do
                        begin
                          listVal[listVal_pos] := data[j];
                          inc(listVal_pos);
                          j := j + 1;
                        end;
                      SetLength(listVal, listVal_pos);
                      val[val_pos] := listVal;
                      inc(val_pos);
                      i := i + 1;
                    end;
                  SetLength(val, val_pos);
                end;
            end;
          k := k + 1;
        end;
      self._valuesDesc := val;
    end;


//--- (end of generated code: YSdi12SensorInfo implementation)


  constructor TYSdi12Port.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Sdi12Port';
      //--- (generated code: YSdi12Port accessors initialization)
      _rxCount := Y_RXCOUNT_INVALID;
      _txCount := Y_TXCOUNT_INVALID;
      _errCount := Y_ERRCOUNT_INVALID;
      _rxMsgCount := Y_RXMSGCOUNT_INVALID;
      _txMsgCount := Y_TXMSGCOUNT_INVALID;
      _lastMsg := Y_LASTMSG_INVALID;
      _currentJob := Y_CURRENTJOB_INVALID;
      _startupJob := Y_STARTUPJOB_INVALID;
      _jobMaxTask := Y_JOBMAXTASK_INVALID;
      _jobMaxSize := Y_JOBMAXSIZE_INVALID;
      _command := Y_COMMAND_INVALID;
      _protocol := Y_PROTOCOL_INVALID;
      _voltageLevel := Y_VOLTAGELEVEL_INVALID;
      _serialMode := Y_SERIALMODE_INVALID;
      _valueCallbackSdi12Port := nil;
      _rxptr := 0;
      _rxbuffptr := 0;
      _eventPos := 0;
      //--- (end of generated code: YSdi12Port accessors initialization)
    end;

//--- (generated code: YSdi12Port yapiwrapper)
//--- (end of generated code: YSdi12Port yapiwrapper)

//--- (generated code: YSdi12Port implementation)
{$HINTS OFF}
  function TYSdi12Port._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'rxCount') then
        begin
          _rxCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'txCount') then
        begin
          _txCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'errCount') then
        begin
          _errCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'rxMsgCount') then
        begin
          _rxMsgCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'txMsgCount') then
        begin
          _txMsgCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'lastMsg') then
        begin
          _lastMsg := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'currentJob') then
        begin
          _currentJob := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'startupJob') then
        begin
          _startupJob := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'jobMaxTask') then
        begin
          _jobMaxTask := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'jobMaxSize') then
        begin
          _jobMaxSize := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'command') then
        begin
          _command := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'protocol') then
        begin
          _protocol := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'voltageLevel') then
        begin
          _voltageLevel := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'serialMode') then
        begin
          _serialMode := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYSdi12Port.get_rxCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RXCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._rxCount;
      result := res;
      exit;
    end;


  function TYSdi12Port.get_txCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_TXCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._txCount;
      result := res;
      exit;
    end;


  function TYSdi12Port.get_errCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ERRCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._errCount;
      result := res;
      exit;
    end;


  function TYSdi12Port.get_rxMsgCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RXMSGCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._rxMsgCount;
      result := res;
      exit;
    end;


  function TYSdi12Port.get_txMsgCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_TXMSGCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._txMsgCount;
      result := res;
      exit;
    end;


  function TYSdi12Port.get_lastMsg():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LASTMSG_INVALID;
              exit;
            end;
        end;
      res := self._lastMsg;
      result := res;
      exit;
    end;


  function TYSdi12Port.get_currentJob():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTJOB_INVALID;
              exit;
            end;
        end;
      res := self._currentJob;
      result := res;
      exit;
    end;


  function TYSdi12Port.set_currentJob(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('currentJob',rest_val);
    end;

  function TYSdi12Port.get_startupJob():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_STARTUPJOB_INVALID;
              exit;
            end;
        end;
      res := self._startupJob;
      result := res;
      exit;
    end;


  function TYSdi12Port.set_startupJob(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('startupJob',rest_val);
    end;

  function TYSdi12Port.get_jobMaxTask():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_JOBMAXTASK_INVALID;
              exit;
            end;
        end;
      res := self._jobMaxTask;
      result := res;
      exit;
    end;


  function TYSdi12Port.get_jobMaxSize():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_JOBMAXSIZE_INVALID;
              exit;
            end;
        end;
      res := self._jobMaxSize;
      result := res;
      exit;
    end;


  function TYSdi12Port.get_command():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_COMMAND_INVALID;
              exit;
            end;
        end;
      res := self._command;
      result := res;
      exit;
    end;


  function TYSdi12Port.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  function TYSdi12Port.get_protocol():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PROTOCOL_INVALID;
              exit;
            end;
        end;
      res := self._protocol;
      result := res;
      exit;
    end;


  function TYSdi12Port.set_protocol(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('protocol',rest_val);
    end;

  function TYSdi12Port.get_voltageLevel():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_VOLTAGELEVEL_INVALID;
              exit;
            end;
        end;
      res := self._voltageLevel;
      result := res;
      exit;
    end;


  function TYSdi12Port.set_voltageLevel(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('voltageLevel',rest_val);
    end;

  function TYSdi12Port.get_serialMode():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SERIALMODE_INVALID;
              exit;
            end;
        end;
      res := self._serialMode;
      result := res;
      exit;
    end;


  function TYSdi12Port.set_serialMode(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('serialMode',rest_val);
    end;

  class function TYSdi12Port.FindSdi12Port(func: string):TYSdi12Port;
    var
      obj : TYSdi12Port;
    begin
      obj := TYSdi12Port(TYFunction._FindFromCache('Sdi12Port', func));
      if (obj = nil) then
        begin
          obj :=  TYSdi12Port.create(func);
          TYFunction._AddToCache('Sdi12Port', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYSdi12Port.registerValueCallback(callback: TYSdi12PortValueCallback):LongInt;
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
      self._valueCallbackSdi12Port := callback;
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


  function TYSdi12Port._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackSdi12Port) <> nil) then
        begin
          self._valueCallbackSdi12Port(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYSdi12Port.sendCommand(text: string):LongInt;
    begin
      result := self.set_command(text);
      exit;
    end;


  function TYSdi12Port.readLine():string;
    var
      url : string;
      msgbin : TByteArray;
      msgarr : TTByteArrayArray;
      msglen : LongInt;
      res : string;
    begin
      url := 'rxmsg.json?pos='+inttostr(self._rxptr)+'&len=1&maxw=1';
      msgbin := self._download(url);
      msgarr := self._json_get_array(msgbin);
      msglen := length(msgarr);
      if msglen = 0 then
        begin
          result := '';
          exit;
        end;
      // last element of array is the new position
      msglen := msglen - 1;
      self._rxptr := self._decode_json_int(msgarr[msglen]);
      if msglen = 0 then
        begin
          result := '';
          exit;
        end;
      res := self._json_get_string(msgarr[0]);
      result := res;
      exit;
    end;


  function TYSdi12Port.readMessages(pattern: string; maxWait: LongInt):TStringArray;
    var
      url : string;
      msgbin : TByteArray;
      msgarr : TTByteArrayArray;
      msglen : LongInt;
      res : TStringArray;
      idx : LongInt;
      res_pos : LongInt;
    begin
      SetLength(res, 0);

      url := 'rxmsg.json?pos='+inttostr(self._rxptr)+'&maxw='+inttostr(maxWait)+'&pat='+pattern;
      msgbin := self._download(url);
      msgarr := self._json_get_array(msgbin);
      msglen := length(msgarr);
      if msglen = 0 then
        begin
          result := res;
          exit;
        end;
      // last element of array is the new position
      msglen := msglen - 1;
      self._rxptr := self._decode_json_int(msgarr[msglen]);
      idx := 0;
      res_pos := length(res);
      SetLength(res, res_pos+msglen);;
      while idx < msglen do
        begin
          res[res_pos] := self._json_get_string(msgarr[idx]);
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSdi12Port.read_seek(absPos: LongInt):LongInt;
    begin
      self._rxptr := absPos;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSdi12Port.read_tell():LongInt;
    begin
      result := self._rxptr;
      exit;
    end;


  function TYSdi12Port.read_avail():LongInt;
    var
      availPosStr : string;
      atPos : LongInt;
      res : LongInt;
      databin : TByteArray;
    begin
      databin := self._download('rxcnt.bin?pos='+inttostr(self._rxptr));
      availPosStr := _ByteToString(databin);
      atPos := (pos('@', availPosStr) - 1);
      res := _atoi(Copy(availPosStr, 0 + 1, atPos));
      result := res;
      exit;
    end;


  function TYSdi12Port.end_tell():LongInt;
    var
      availPosStr : string;
      atPos : LongInt;
      res : LongInt;
      databin : TByteArray;
    begin
      databin := self._download('rxcnt.bin?pos='+inttostr(self._rxptr));
      availPosStr := _ByteToString(databin);
      atPos := (pos('@', availPosStr) - 1);
      res := _atoi(Copy(availPosStr, atPos+1 + 1, Length(availPosStr)-atPos-1));
      result := res;
      exit;
    end;


  function TYSdi12Port.queryLine(query: string; maxWait: LongInt):string;
    var
      prevpos : LongInt;
      url : string;
      msgbin : TByteArray;
      msgarr : TTByteArrayArray;
      msglen : LongInt;
      res : string;
    begin
      if Length(query) <= 80 then
        begin
          // fast query
          url := 'rxmsg.json?len=1&maxw='+inttostr(maxWait)+'&cmd=!'+self._escapeAttr(query);
        end
      else
        begin
          // long query
          prevpos := self.end_tell;
          self._upload('txdata', _StrToByte(query + ''#13''#10''));
          url := 'rxmsg.json?len=1&maxw='+inttostr(maxWait)+'&pos='+inttostr(prevpos);
        end;

      msgbin := self._download(url);
      msgarr := self._json_get_array(msgbin);
      msglen := length(msgarr);
      if msglen = 0 then
        begin
          result := '';
          exit;
        end;
      // last element of array is the new position
      msglen := msglen - 1;
      self._rxptr := self._decode_json_int(msgarr[msglen]);
      if msglen = 0 then
        begin
          result := '';
          exit;
        end;
      res := self._json_get_string(msgarr[0]);
      result := res;
      exit;
    end;


  function TYSdi12Port.queryHex(hexString: string; maxWait: LongInt):string;
    var
      prevpos : LongInt;
      url : string;
      msgbin : TByteArray;
      msgarr : TTByteArrayArray;
      msglen : LongInt;
      res : string;
    begin
      if Length(hexString) <= 80 then
        begin
          // fast query
          url := 'rxmsg.json?len=1&maxw='+inttostr(maxWait)+'&cmd=$'+hexString;
        end
      else
        begin
          // long query
          prevpos := self.end_tell;
          self._upload('txdata', _hexStrToBin(hexString));
          url := 'rxmsg.json?len=1&maxw='+inttostr(maxWait)+'&pos='+inttostr(prevpos);
        end;

      msgbin := self._download(url);
      msgarr := self._json_get_array(msgbin);
      msglen := length(msgarr);
      if msglen = 0 then
        begin
          result := '';
          exit;
        end;
      // last element of array is the new position
      msglen := msglen - 1;
      self._rxptr := self._decode_json_int(msgarr[msglen]);
      if msglen = 0 then
        begin
          result := '';
          exit;
        end;
      res := self._json_get_string(msgarr[0]);
      result := res;
      exit;
    end;


  function TYSdi12Port.uploadJob(jobfile: string; jsonDef: string):LongInt;
    begin
      self._upload(jobfile, _StrToByte(jsonDef));
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSdi12Port.selectJob(jobfile: string):LongInt;
    begin
      result := self.set_currentJob(jobfile);
      exit;
    end;


  function TYSdi12Port.reset():LongInt;
    begin
      self._eventPos := 0;
      self._rxptr := 0;
      self._rxbuffptr := 0;
      setlength(self._rxbuff,0);

      result := self.sendCommand('Z');
      exit;
    end;


  function TYSdi12Port.writeByte(code: LongInt):LongInt;
    begin
      result := self.sendCommand('$'+AnsiUpperCase(inttohex(code,02)));
      exit;
    end;


  function TYSdi12Port.writeStr(text: string):LongInt;
    var
      buff : TByteArray;
      bufflen : LongInt;
      idx : LongInt;
      ch : LongInt;
    begin
      buff := _StrToByte(text);
      bufflen := length(buff);
      if bufflen < 100 then
        begin
          // if string is pure text, we can send it as a simple command (faster)
          ch := $020;
          idx := 0;
          while (idx < bufflen) and(ch <> 0) do
            begin
              ch := buff[idx];
              if (ch >= $020) and(ch < $07f) then
                begin
                  idx := idx + 1;
                end
              else
                begin
                  ch := 0;
                end;
            end;
          if idx >= bufflen then
            begin
              result := self.sendCommand('+'+text);
              exit;
            end;
        end;
      // send string using file upload
      result := self._upload('txdata', buff);
      exit;
    end;


  function TYSdi12Port.writeBin(buff: TByteArray):LongInt;
    begin
      result := self._upload('txdata', buff);
      exit;
    end;


  function TYSdi12Port.writeArray(byteList: TLongIntArray):LongInt;
    var
      buff : TByteArray;
      bufflen : LongInt;
      idx : LongInt;
      hexb : LongInt;
      res : LongInt;
    begin
      bufflen := length(byteList);
      setlength(buff,bufflen);
      idx := 0;
      while idx < bufflen do
        begin
          hexb := byteList[idx];
          buff[idx] := hexb;
          idx := idx + 1;
        end;

      res := self._upload('txdata', buff);
      result := res;
      exit;
    end;


  function TYSdi12Port.writeHex(hexString: string):LongInt;
    var
      buff : TByteArray;
      bufflen : LongInt;
      idx : LongInt;
      hexb : LongInt;
      res : LongInt;
    begin
      bufflen := Length(hexString);
      if bufflen < 100 then
        begin
          result := self.sendCommand('$'+hexString);
          exit;
        end;
      bufflen := ((bufflen) shr 1);
      setlength(buff,bufflen);
      idx := 0;
      while idx < bufflen do
        begin
          hexb := StrToInt('$0' + Copy(hexString, 2 * idx + 1, 2));
          buff[idx] := hexb;
          idx := idx + 1;
        end;

      res := self._upload('txdata', buff);
      result := res;
      exit;
    end;


  function TYSdi12Port.writeLine(text: string):LongInt;
    var
      buff : TByteArray;
      bufflen : LongInt;
      idx : LongInt;
      ch : LongInt;
    begin
      buff := _StrToByte(''+text+''#13''#10'');
      bufflen := length(buff)-2;
      if bufflen < 100 then
        begin
          // if string is pure text, we can send it as a simple command (faster)
          ch := $020;
          idx := 0;
          while (idx < bufflen) and(ch <> 0) do
            begin
              ch := buff[idx];
              if (ch >= $020) and(ch < $07f) then
                begin
                  idx := idx + 1;
                end
              else
                begin
                  ch := 0;
                end;
            end;
          if idx >= bufflen then
            begin
              result := self.sendCommand('!'+text);
              exit;
            end;
        end;
      // send string using file upload
      result := self._upload('txdata', buff);
      exit;
    end;


  function TYSdi12Port.readByte():LongInt;
    var
      currpos : LongInt;
      reqlen : LongInt;
      buff : TByteArray;
      bufflen : LongInt;
      mult : LongInt;
      endpos : LongInt;
      res : LongInt;
    begin
      bufflen := length(self._rxbuff);
      if (self._rxptr >= self._rxbuffptr) and(self._rxptr < self._rxbuffptr+bufflen) then
        begin
          res := self._rxbuff[self._rxptr-self._rxbuffptr];
          self._rxptr := self._rxptr + 1;
          result := res;
          exit;
        end;
      // try to preload more than one byte to speed-up byte-per-byte access
      currpos := self._rxptr;
      reqlen := 1024;
      buff := self.readBin(reqlen);
      bufflen := length(buff);
      if self._rxptr = currpos+bufflen then
        begin
          res := buff[0];
          self._rxptr := currpos+1;
          self._rxbuffptr := currpos;
          self._rxbuff := buff;
          result := res;
          exit;
        end;
      // mixed bidirectional data, retry with a smaller block
      self._rxptr := currpos;
      reqlen := 16;
      buff := self.readBin(reqlen);
      bufflen := length(buff);
      if self._rxptr = currpos+bufflen then
        begin
          res := buff[0];
          self._rxptr := currpos+1;
          self._rxbuffptr := currpos;
          self._rxbuff := buff;
          result := res;
          exit;
        end;
      // still mixed, need to process character by character
      self._rxptr := currpos;

      buff := self._download('rxdata.bin?pos='+inttostr(self._rxptr)+'&len=1');
      bufflen := length(buff) - 1;
      endpos := 0;
      mult := 1;
      while (bufflen > 0) and(buff[bufflen] <> 64) do
        begin
          endpos := endpos + mult * (buff[bufflen] - 48);
          mult := mult * 10;
          bufflen := bufflen - 1;
        end;
      self._rxptr := endpos;
      if bufflen = 0 then
        begin
          result := YAPI_NO_MORE_DATA;
          exit;
        end;
      res := buff[0];
      result := res;
      exit;
    end;


  function TYSdi12Port.readStr(nChars: LongInt):string;
    var
      buff : TByteArray;
      bufflen : LongInt;
      mult : LongInt;
      endpos : LongInt;
      res : string;
    begin
      if nChars > 65535 then
        begin
          nChars := 65535;
        end;

      buff := self._download('rxdata.bin?pos='+inttostr(self._rxptr)+'&len='+inttostr(nChars));
      bufflen := length(buff) - 1;
      endpos := 0;
      mult := 1;
      while (bufflen > 0) and(buff[bufflen] <> 64) do
        begin
          endpos := endpos + mult * (buff[bufflen] - 48);
          mult := mult * 10;
          bufflen := bufflen - 1;
        end;
      self._rxptr := endpos;
      res := Copy(_ByteToString(buff), 0 + 1, bufflen);
      result := res;
      exit;
    end;


  function TYSdi12Port.readBin(nChars: LongInt):TByteArray;
    var
      buff : TByteArray;
      bufflen : LongInt;
      mult : LongInt;
      endpos : LongInt;
      idx : LongInt;
      res : TByteArray;
    begin
      if nChars > 65535 then
        begin
          nChars := 65535;
        end;

      buff := self._download('rxdata.bin?pos='+inttostr(self._rxptr)+'&len='+inttostr(nChars));
      bufflen := length(buff) - 1;
      endpos := 0;
      mult := 1;
      while (bufflen > 0) and(buff[bufflen] <> 64) do
        begin
          endpos := endpos + mult * (buff[bufflen] - 48);
          mult := mult * 10;
          bufflen := bufflen - 1;
        end;
      self._rxptr := endpos;
      setlength(res,bufflen);
      idx := 0;
      while idx < bufflen do
        begin
          res[idx] := buff[idx];
          idx := idx + 1;
        end;
      result := res;
      exit;
    end;


  function TYSdi12Port.readArray(nChars: LongInt):TLongIntArray;
    var
      buff : TByteArray;
      bufflen : LongInt;
      mult : LongInt;
      endpos : LongInt;
      idx : LongInt;
      b : LongInt;
      res : TLongIntArray;
      res_pos : LongInt;
    begin
      if nChars > 65535 then
        begin
          nChars := 65535;
        end;

      buff := self._download('rxdata.bin?pos='+inttostr(self._rxptr)+'&len='+inttostr(nChars));
      bufflen := length(buff) - 1;
      endpos := 0;
      mult := 1;
      while (bufflen > 0) and(buff[bufflen] <> 64) do
        begin
          endpos := endpos + mult * (buff[bufflen] - 48);
          mult := mult * 10;
          bufflen := bufflen - 1;
        end;
      self._rxptr := endpos;
      res_pos := 0;
      SetLength(res, bufflen);;
      idx := 0;
      while idx < bufflen do
        begin
          b := buff[idx];
          res[res_pos] := b;
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSdi12Port.readHex(nBytes: LongInt):string;
    var
      buff : TByteArray;
      bufflen : LongInt;
      mult : LongInt;
      endpos : LongInt;
      ofs : LongInt;
      res : string;
    begin
      if nBytes > 65535 then
        begin
          nBytes := 65535;
        end;

      buff := self._download('rxdata.bin?pos='+inttostr(self._rxptr)+'&len='+inttostr(nBytes));
      bufflen := length(buff) - 1;
      endpos := 0;
      mult := 1;
      while (bufflen > 0) and(buff[bufflen] <> 64) do
        begin
          endpos := endpos + mult * (buff[bufflen] - 48);
          mult := mult * 10;
          bufflen := bufflen - 1;
        end;
      self._rxptr := endpos;
      res := '';
      ofs := 0;
      while ofs + 3 < bufflen do
        begin
          res := ''+res+''+AnsiUpperCase(inttohex(buff[ofs],02))+''+AnsiUpperCase(inttohex(buff[ofs + 1],02))+''+AnsiUpperCase(inttohex(buff[ofs + 2],02))+''+AnsiUpperCase(inttohex(buff[ofs + 3],02));
          ofs := ofs + 4;
        end;
      while ofs < bufflen do
        begin
          res := ''+res+''+AnsiUpperCase(inttohex(buff[ofs],02));
          ofs := ofs + 1;
        end;
      result := res;
      exit;
    end;


  function TYSdi12Port.querySdi12(sensorAddr: string; cmd: string; maxWait: LongInt):string;
    var
      fullCmd : string;
      cmdChar : string;
      pattern : string;
      url : string;
      msgbin : TByteArray;
      msgarr : TTByteArrayArray;
      msglen : LongInt;
      res : string;
    begin
      cmdChar  := '';

      pattern := sensorAddr;
      if Length(cmd) > 0 then
        begin
          cmdChar := Copy(cmd, 0 + 1, 1);
        end;
      if (sensorAddr = '?') then
        begin
          pattern := '.*';
        end
      else
        begin
          if (cmdChar = 'M') or (cmdChar = 'D') then
            begin
              pattern := ''+sensorAddr+':.*';
            end
          else
            begin
              pattern := ''+sensorAddr+'.*';
            end;
        end;
      pattern := self._escapeAttr(pattern);
      fullCmd := self._escapeAttr('+'+sensorAddr+''+cmd+'!');
      url := 'rxmsg.json?len=1&maxw='+inttostr(maxWait)+'&cmd='+fullCmd+'&pat='+pattern;

      msgbin := self._download(url);
      if length(msgbin)<2 then
        begin
          result := '';
          exit;
        end;
      msgarr := self._json_get_array(msgbin);
      msglen := length(msgarr);
      if msglen = 0 then
        begin
          result := '';
          exit;
        end;
      // last element of array is the new position
      msglen := msglen - 1;
      self._rxptr := self._decode_json_int(msgarr[msglen]);
      if msglen = 0 then
        begin
          result := '';
          exit;
        end;
      res := self._json_get_string(msgarr[0]);
      result := res;
      exit;
    end;


  function TYSdi12Port.discoverSingleSensor():TYSdi12SensorInfo;
    var
      resStr : string;
    begin
      resStr := self.querySdi12('?', '', 5000);
      if (resStr = '') then
        begin
          result := TYSdi12SensorInfo.create(self, 'ERSensor Not Found');
          exit;
        end;

      result := self.getSensorInformation(resStr);
      exit;
    end;


  function TYSdi12Port.discoverAllSensors():TYSdi12SensorInfoArray;
    var
      sensors : TYSdi12SensorInfoArray;
      idSens : TStringArray;
      res : string;
      i : LongInt;
      lettreMin : string;
      lettreMaj : string;
      idSens_pos : LongInt;
      sensors_pos : LongInt;
    begin
      SetLength(idSens, 0);

      // 1. Search for sensors present
      idSens_pos := 0;
      SetLength(idSens, 62);;
      i := 0 ;
      while i < 10 do
        begin
          res := self.querySdi12(IntToStr(i), '!', 500);
          if Length(res) >= 1 then
            begin
              idSens[idSens_pos] := res;
              inc(idSens_pos);
            end;
          i := i+1;
        end;
      lettreMin := 'abcdefghijklmnopqrstuvwxyz';
      lettreMaj := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
      i := 0;
      while i<26 do
        begin
          res := self.querySdi12(Copy(lettreMin, i + 1, 1), '!', 500);
          if Length(res) >= 1 then
            begin
              idSens[idSens_pos] := res;
              inc(idSens_pos);
            end;
          i := i +1;
        end;
      while i<26 do
        begin
          res := self.querySdi12(Copy(lettreMaj, i + 1, 1), '!', 500);
          if Length(res) >= 1 then
            begin
              idSens[idSens_pos] := res;
              inc(idSens_pos);
            end;
          i := i +1;
        end;
      SetLength(idSens, idSens_pos);;
      // 2. Query existing sensors information
      i := 0;
      sensors_pos := 0;
      SetLength(sensors, length(idSens));;
      while i < length(idSens) do
        begin
          sensors[sensors_pos] := self.getSensorInformation(idSens[i]);
          inc(sensors_pos);
          i := i + 1;
        end;
      SetLength(sensors, sensors_pos);;
      result := sensors;
      exit;
    end;


  function TYSdi12Port.readSensor(sensorAddr: string; measCmd: string; maxWait: LongInt):TDoubleArray;
    var
      resStr : string;
      res : TDoubleArray;
      tab : TStringArray;
      split : TStringArray;
      i : LongInt;
      valdouble : double;
      res_pos : LongInt;
    begin
      SetLength(tab, 0);
      SetLength(split, 0);

      resStr := self.querySdi12(sensorAddr, measCmd, maxWait);
      tab := _stringSplit(resStr, ',');
      split := _stringSplit(tab[0], ':');
      if length(split) < 2 then
        begin
          result := res;
          exit;
        end;
      res_pos := length(res);
      SetLength(res, res_pos+length(tab));;
      valdouble := _yapiStrToFloat(split[1]);
      res[res_pos] := valdouble;
      inc(res_pos);
      i := 1;
      while i < length(tab) do
        begin
          valdouble := _yapiStrToFloat(tab[i]);
          res[res_pos] := valdouble;
          inc(res_pos);
          i := i + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSdi12Port.changeAddress(oldAddress: string; newAddress: string):TYSdi12SensorInfo;
    var
      addr : TYSdi12SensorInfo;
    begin
      self.querySdi12(oldAddress, 'A' + newAddress, 1000);
      addr := self.getSensorInformation(newAddress);
      result := addr;
      exit;
    end;


  function TYSdi12Port.getSensorInformation(sensorAddr: string):TYSdi12SensorInfo;
    var
      res : string;
      sensor : TYSdi12SensorInfo;
    begin
      res := self.querySdi12(sensorAddr, 'I', 1000);
      if (res = '') then
        begin
          result := TYSdi12SensorInfo.create(self, 'ERSensor Not Found');
          exit;
        end;
      sensor :=  TYSdi12SensorInfo.create(self, res);
      sensor._queryValueInfo;
      result := sensor;
      exit;
    end;


  function TYSdi12Port.readConcurrentMeasurements(sensorAddr: string):TDoubleArray;
    var
      res : TDoubleArray;
    begin
      res:= self.readSensor(sensorAddr, 'D', 1000);
      result := res;
      exit;
    end;


  function TYSdi12Port.requestConcurrentMeasurements(sensorAddr: string):LongInt;
    var
      timewait : LongInt;
      wait : string;
    begin
      wait := self.querySdi12(sensorAddr, 'C', 1000);
      wait := Copy(wait, 1 + 1, 3);
      timewait := _atoi(wait) * 1000;
      result := timewait;
      exit;
    end;


  function TYSdi12Port.snoopMessagesEx(maxWait: LongInt; maxMsg: LongInt):TYSdi12SnoopingRecordArray;
    var
      url : string;
      msgbin : TByteArray;
      msgarr : TTByteArrayArray;
      msglen : LongInt;
      res : TYSdi12SnoopingRecordArray;
      idx : LongInt;
      res_pos : LongInt;
    begin
      url := 'rxmsg.json?pos='+inttostr(self._rxptr)+'&maxw='+inttostr(maxWait)+'&t=0&len='+inttostr(maxMsg);
      msgbin := self._download(url);
      msgarr := self._json_get_array(msgbin);
      msglen := length(msgarr);
      if msglen = 0 then
        begin
          result := res;
          exit;
        end;
      // last element of array is the new position
      msglen := msglen - 1;
      self._rxptr := self._decode_json_int(msgarr[msglen]);
      idx := 0;
      res_pos := length(res);
      SetLength(res, res_pos+msglen);;
      while idx < msglen do
        begin
          res[res_pos] := TYSdi12SnoopingRecord.create(_ByteToString(msgarr[idx]));
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSdi12Port.snoopMessages(maxWait: LongInt):TYSdi12SnoopingRecordArray;
    begin
      result := self.snoopMessagesEx(maxWait, 255);
      exit;
    end;


  function TYSdi12Port.nextSdi12Port(): TYSdi12Port;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextSdi12Port := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextSdi12Port := nil;
          exit;
        end;
      nextSdi12Port := TYSdi12Port.FindSdi12Port(hwid);
    end;

  class function TYSdi12Port.FirstSdi12Port(): TYSdi12Port;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Sdi12Port', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYSdi12Port.FindSdi12Port(serial+'.'+funcId);
    end;

//--- (end of generated code: YSdi12Port implementation)

//--- (generated code: YSdi12Port functions)

  function yFindSdi12Port(func:string): TYSdi12Port;
    begin
      result := TYSdi12Port.FindSdi12Port(func);
    end;

  function yFirstSdi12Port(): TYSdi12Port;
    begin
      result := TYSdi12Port.FirstSdi12Port();
    end;

  procedure _Sdi12PortCleanup();
    begin
    end;

//--- (end of generated code: YSdi12Port functions)

initialization
  //--- (generated code: YSdi12Port initialization)
  //--- (end of generated code: YSdi12Port initialization)

finalization
  //--- (generated code: YSdi12Port cleanup)
  _Sdi12PortCleanup();
  //--- (end of generated code: YSdi12Port cleanup)

end.
