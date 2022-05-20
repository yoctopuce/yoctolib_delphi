{*********************************************************************
 *
 *  $Id: yocto_spiport.pas 49750 2022-05-13 07:10:42Z seb $
 *
 *  Implements yFindSpiPort(), the high-level API for SpiPort functions
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


unit yocto_spiport;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,{$IFNDEF UNIX}windows,
{$ENDIF}
 yocto_api, yjson;

//--- (generated code: YSpiPort definitions)

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
const Y_SPIMODE_INVALID               = YAPI_INVALID_STRING;
const Y_SSPOLARITY_ACTIVE_LOW = 0;
const Y_SSPOLARITY_ACTIVE_HIGH = 1;
const Y_SSPOLARITY_INVALID = -1;
const Y_SHIFTSAMPLING_OFF = 0;
const Y_SHIFTSAMPLING_ON = 1;
const Y_SHIFTSAMPLING_INVALID = -1;


//--- (end of generated code: YSpiPort definitions)
//--- (generated code: YSpiPort yapiwrapper declaration)
//--- (end of generated code: YSpiPort yapiwrapper declaration)

type
  TYSpiPort = class;
  TYSpiSnoopingRecord = class;

  //--- (generated code: YSpiSnoopingRecord class start)
  ////
  /// <summary>
  ///   T
  /// <para>
  ///   YSpiSnoopingRecord Class: Intercepted SPI message description, returned by <c>spiPort.snoopMessages</c> method
  /// </para>
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  ///-
  TYSpiSnoopingRecord=class(TObject)
  //--- (end of generated code: YSpiSnoopingRecord class start)
  protected

    //--- (generated code: YSpiSnoopingRecord declaration)
    // Attributes (function value cache)
    _tim                      : LongInt;
    _dir                      : LongInt;
    _msg                      : string;

    //--- (end of generated code: YSpiSnoopingRecord declaration)
public
   constructor create(data:string);


   //--- (generated code: YSpiSnoopingRecord accessors declaration)
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


  //--- (end of generated code: YSpiSnoopingRecord accessors declaration)
end;


TYSpiSnoopingRecordARRAY = array of TYSpiSnoopingRecord;


  //--- (generated code: YSpiPort class start)
  TYSpiPortValueCallback = procedure(func: TYSpiPort; value:string);
  TYSpiPortTimedReportCallback = procedure(func: TYSpiPort; value:TYMeasure);

  ////
  /// <summary>
  ///   TYSpiPort Class: SPI port control interface, available for instance in the Yocto-SPI
  /// <para>
  ///   The <c>YSpiPort</c> class allows you to fully drive a Yoctopuce SPI port.
  ///   It can be used to send and receive data, and to configure communication
  ///   parameters (baud rate, bit count, parity, flow control and protocol).
  ///   Note that Yoctopuce SPI ports are not exposed as virtual COM ports.
  ///   They are meant to be used in the same way as all Yoctopuce devices.
  /// </para>
  /// </summary>
  ///-
  TYSpiPort=class(TYFunction)
  //--- (end of generated code: YSpiPort class start)
  protected
  //--- (generated code: YSpiPort declaration)
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
    _spiMode                  : string;
    _ssPolarity               : Integer;
    _shiftSampling            : Integer;
    _valueCallbackSpiPort     : TYSpiPortValueCallback;
    _rxptr                    : LongInt;
    _rxbuff                   : TByteArray;
    _rxbuffptr                : LongInt;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YSpiPort declaration)

  public
    //--- (generated code: YSpiPort accessors declaration)
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
    ///   On failure, throws an exception or returns <c>YSpiPort.RXCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSpiPort.TXCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSpiPort.ERRCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSpiPort.RXMSGCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSpiPort.TXMSGCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_txMsgCount():LongInt;

    ////
    /// <summary>
    ///   Returns the latest message fully received (for Line and Frame protocols).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the latest message fully received (for Line and Frame protocols)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpiPort.LASTMSG_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSpiPort.CURRENTJOB_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSpiPort.STARTUPJOB_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSpiPort.JOBMAXTASK_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSpiPort.JOBMAXSIZE_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSpiPort.PROTOCOL_INVALID</c>.
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
    ///   a value among <c>YSpiPort.VOLTAGELEVEL_OFF</c>, <c>YSpiPort.VOLTAGELEVEL_TTL3V</c>,
    ///   <c>YSpiPort.VOLTAGELEVEL_TTL3VR</c>, <c>YSpiPort.VOLTAGELEVEL_TTL5V</c>,
    ///   <c>YSpiPort.VOLTAGELEVEL_TTL5VR</c>, <c>YSpiPort.VOLTAGELEVEL_RS232</c>,
    ///   <c>YSpiPort.VOLTAGELEVEL_RS485</c>, <c>YSpiPort.VOLTAGELEVEL_TTL1V8</c> and
    ///   <c>YSpiPort.VOLTAGELEVEL_SDI12</c> corresponding to the voltage level used on the serial line
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpiPort.VOLTAGELEVEL_INVALID</c>.
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
    ///   a value among <c>YSpiPort.VOLTAGELEVEL_OFF</c>, <c>YSpiPort.VOLTAGELEVEL_TTL3V</c>,
    ///   <c>YSpiPort.VOLTAGELEVEL_TTL3VR</c>, <c>YSpiPort.VOLTAGELEVEL_TTL5V</c>,
    ///   <c>YSpiPort.VOLTAGELEVEL_TTL5VR</c>, <c>YSpiPort.VOLTAGELEVEL_RS232</c>,
    ///   <c>YSpiPort.VOLTAGELEVEL_RS485</c>, <c>YSpiPort.VOLTAGELEVEL_TTL1V8</c> and
    ///   <c>YSpiPort.VOLTAGELEVEL_SDI12</c> corresponding to the voltage type used on the serial line
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
    ///   Returns the SPI port communication parameters, as a string such as
    ///   "125000,0,msb".
    /// <para>
    ///   The string includes the baud rate, the SPI mode (between
    ///   0 and 3) and the bit order.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the SPI port communication parameters, as a string such as
    ///   "125000,0,msb"
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpiPort.SPIMODE_INVALID</c>.
    /// </para>
    ///-
    function get_spiMode():string;

    ////
    /// <summary>
    ///   Changes the SPI port communication parameters, with a string such as
    ///   "125000,0,msb".
    /// <para>
    ///   The string includes the baud rate, the SPI mode (between
    ///   0 and 3) and the bit order.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the SPI port communication parameters, with a string such as
    ///   "125000,0,msb"
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
    function set_spiMode(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the SS line polarity.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YSpiPort.SSPOLARITY_ACTIVE_LOW</c> or <c>YSpiPort.SSPOLARITY_ACTIVE_HIGH</c>, according
    ///   to the SS line polarity
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpiPort.SSPOLARITY_INVALID</c>.
    /// </para>
    ///-
    function get_ssPolarity():Integer;

    ////
    /// <summary>
    ///   Changes the SS line polarity.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YSpiPort.SSPOLARITY_ACTIVE_LOW</c> or <c>YSpiPort.SSPOLARITY_ACTIVE_HIGH</c>, according
    ///   to the SS line polarity
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
    function set_ssPolarity(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns true when the SDI line phase is shifted with regards to the SDO line.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YSpiPort.SHIFTSAMPLING_OFF</c> or <c>YSpiPort.SHIFTSAMPLING_ON</c>, according to true
    ///   when the SDI line phase is shifted with regards to the SDO line
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpiPort.SHIFTSAMPLING_INVALID</c>.
    /// </para>
    ///-
    function get_shiftSampling():Integer;

    ////
    /// <summary>
    ///   Changes the SDI line sampling shift.
    /// <para>
    ///   When disabled, SDI line is
    ///   sampled in the middle of data output time. When enabled, SDI line is
    ///   samples at the end of data output time.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YSpiPort.SHIFTSAMPLING_OFF</c> or <c>YSpiPort.SHIFTSAMPLING_ON</c>, according to the SDI
    ///   line sampling shift
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
    function set_shiftSampling(newval:Integer):integer;

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
    ///   Use the method <c>YSpiPort.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YSpiPort</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindSpiPort(func: string):TYSpiPort;

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
    function registerValueCallback(callback: TYSpiPortValueCallback):LongInt; overload;

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
    ///   Manually sets the state of the SS line.
    /// <para>
    ///   This function has no effect when
    ///   the SS line is handled automatically.
    /// </para>
    /// </summary>
    /// <param name="val">
    ///   1 to turn SS active, 0 to release SS.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_SS(val: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Retrieves messages (both direction) in the SPI port buffer, starting at current position.
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
    ///   an array of <c>YSpiSnoopingRecord</c> objects containing the messages found, if any.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function snoopMessages(maxWait: LongInt):TYSpiSnoopingRecordArray; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of SPI ports started using <c>yFirstSpiPort()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned SPI ports order.
    ///   If you want to find a specific a SPI port, use <c>SpiPort.findSpiPort()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YSpiPort</c> object, corresponding to
    ///   a SPI port currently online, or a <c>NIL</c> pointer
    ///   if there are no more SPI ports to enumerate.
    /// </returns>
    ///-
    function nextSpiPort():TYSpiPort;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstSpiPort():TYSpiPort;
  //--- (end of generated code: YSpiPort accessors declaration)
  end;

//--- (generated code: YSpiPort functions declaration)
  ////
  /// <summary>
  ///   Retrieves a SPI port for a given identifier.
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
  ///   This function does not require that the SPI port is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YSpiPort.isOnline()</c> to test if the SPI port is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a SPI port by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the SPI port, for instance
  ///   <c>YSPIMK01.spiPort</c>.
  /// </param>
  /// <returns>
  ///   a <c>YSpiPort</c> object allowing you to drive the SPI port.
  /// </returns>
  ///-
  function yFindSpiPort(func:string):TYSpiPort;
  ////
  /// <summary>
  ///   Starts the enumeration of SPI ports currently accessible.
  /// <para>
  ///   Use the method <c>YSpiPort.nextSpiPort()</c> to iterate on
  ///   next SPI ports.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YSpiPort</c> object, corresponding to
  ///   the first SPI port currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstSpiPort():TYSpiPort;

//--- (end of generated code: YSpiPort functions declaration)

implementation
//--- (generated code: YSpiPort dlldef)
//--- (end of generated code: YSpiPort dlldef)

  constructor TYSpiPort.Create(func:string);
    begin
      inherited Create(func);
      _className := 'SpiPort';
      //--- (generated code: YSpiPort accessors initialization)
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
      _spiMode := Y_SPIMODE_INVALID;
      _ssPolarity := Y_SSPOLARITY_INVALID;
      _shiftSampling := Y_SHIFTSAMPLING_INVALID;
      _valueCallbackSpiPort := nil;
      _rxptr := 0;
      _rxbuffptr := 0;
      //--- (end of generated code: YSpiPort accessors initialization)
    end;

//--- (generated code: YSpiPort yapiwrapper)
//--- (end of generated code: YSpiPort yapiwrapper)

//--- (generated code: YSpiPort implementation)
{$HINTS OFF}
  function TYSpiPort._parseAttr(member:PJSONRECORD):integer;
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
      if (member^.name = 'spiMode') then
        begin
          _spiMode := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'ssPolarity') then
        begin
          _ssPolarity := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'shiftSampling') then
        begin
          _shiftSampling := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYSpiPort.get_rxCount():LongInt;
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


  function TYSpiPort.get_txCount():LongInt;
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


  function TYSpiPort.get_errCount():LongInt;
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


  function TYSpiPort.get_rxMsgCount():LongInt;
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


  function TYSpiPort.get_txMsgCount():LongInt;
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


  function TYSpiPort.get_lastMsg():string;
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


  function TYSpiPort.get_currentJob():string;
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


  function TYSpiPort.set_currentJob(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('currentJob',rest_val);
    end;

  function TYSpiPort.get_startupJob():string;
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


  function TYSpiPort.set_startupJob(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('startupJob',rest_val);
    end;

  function TYSpiPort.get_jobMaxTask():LongInt;
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


  function TYSpiPort.get_jobMaxSize():LongInt;
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


  function TYSpiPort.get_command():string;
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


  function TYSpiPort.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  function TYSpiPort.get_protocol():string;
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


  function TYSpiPort.set_protocol(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('protocol',rest_val);
    end;

  function TYSpiPort.get_voltageLevel():Integer;
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


  function TYSpiPort.set_voltageLevel(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('voltageLevel',rest_val);
    end;

  function TYSpiPort.get_spiMode():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SPIMODE_INVALID;
              exit;
            end;
        end;
      res := self._spiMode;
      result := res;
      exit;
    end;


  function TYSpiPort.set_spiMode(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('spiMode',rest_val);
    end;

  function TYSpiPort.get_ssPolarity():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SSPOLARITY_INVALID;
              exit;
            end;
        end;
      res := self._ssPolarity;
      result := res;
      exit;
    end;


  function TYSpiPort.set_ssPolarity(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('ssPolarity',rest_val);
    end;

  function TYSpiPort.get_shiftSampling():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SHIFTSAMPLING_INVALID;
              exit;
            end;
        end;
      res := self._shiftSampling;
      result := res;
      exit;
    end;


  function TYSpiPort.set_shiftSampling(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('shiftSampling',rest_val);
    end;

  class function TYSpiPort.FindSpiPort(func: string):TYSpiPort;
    var
      obj : TYSpiPort;
    begin
      obj := TYSpiPort(TYFunction._FindFromCache('SpiPort', func));
      if obj = nil then
        begin
          obj :=  TYSpiPort.create(func);
          TYFunction._AddToCache('SpiPort',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYSpiPort.registerValueCallback(callback: TYSpiPortValueCallback):LongInt;
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
      self._valueCallbackSpiPort := callback;
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


  function TYSpiPort._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackSpiPort) <> nil) then
        begin
          self._valueCallbackSpiPort(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYSpiPort.sendCommand(text: string):LongInt;
    begin
      result := self.set_command(text);
      exit;
    end;


  function TYSpiPort.readLine():string;
    var
      url : string;
      msgbin : TByteArray;
      msgarr : TStringArray;
      msglen : LongInt;
      res : string;
    begin
      SetLength(msgarr, 0);

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
      self._rxptr := _atoi(msgarr[msglen]);
      if msglen = 0 then
        begin
          result := '';
          exit;
        end;
      res := self._json_get_string(_StrToByte(msgarr[0]));
      result := res;
      exit;
    end;


  function TYSpiPort.readMessages(pattern: string; maxWait: LongInt):TStringArray;
    var
      url : string;
      msgbin : TByteArray;
      msgarr : TStringArray;
      msglen : LongInt;
      res : TStringArray;
      idx : LongInt;
      res_pos : LongInt;
    begin
      SetLength(msgarr, 0);
      SetLength(res, 0);

      url := 'rxmsg.json?pos='+inttostr( self._rxptr)+'&maxw='+inttostr( maxWait)+'&pat='+pattern;
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
      self._rxptr := _atoi(msgarr[msglen]);
      idx := 0;
      res_pos := length(res);
      SetLength(res, res_pos+msglen);;
      while idx < msglen do
        begin
          res[res_pos] := self._json_get_string(_StrToByte(msgarr[idx]));
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSpiPort.read_seek(absPos: LongInt):LongInt;
    begin
      self._rxptr := absPos;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSpiPort.read_tell():LongInt;
    begin
      result := self._rxptr;
      exit;
    end;


  function TYSpiPort.read_avail():LongInt;
    var
      buff : TByteArray;
      bufflen : LongInt;
      res : LongInt;
    begin
      buff := self._download('rxcnt.bin?pos='+inttostr(self._rxptr));
      bufflen := length(buff) - 1;
      while (bufflen > 0) and(buff[bufflen] <> 64) do
        begin
          bufflen := bufflen - 1;
        end;
      res := _atoi(Copy(_ByteToString(buff),  0 + 1, bufflen));
      result := res;
      exit;
    end;


  function TYSpiPort.queryLine(query: string; maxWait: LongInt):string;
    var
      url : string;
      msgbin : TByteArray;
      msgarr : TStringArray;
      msglen : LongInt;
      res : string;
    begin
      SetLength(msgarr, 0);

      url := 'rxmsg.json?len=1&maxw='+inttostr( maxWait)+'&cmd=!'+self._escapeAttr(query);
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
      self._rxptr := _atoi(msgarr[msglen]);
      if msglen = 0 then
        begin
          result := '';
          exit;
        end;
      res := self._json_get_string(_StrToByte(msgarr[0]));
      result := res;
      exit;
    end;


  function TYSpiPort.queryHex(hexString: string; maxWait: LongInt):string;
    var
      url : string;
      msgbin : TByteArray;
      msgarr : TStringArray;
      msglen : LongInt;
      res : string;
    begin
      SetLength(msgarr, 0);

      url := 'rxmsg.json?len=1&maxw='+inttostr( maxWait)+'&cmd=$'+hexString;
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
      self._rxptr := _atoi(msgarr[msglen]);
      if msglen = 0 then
        begin
          result := '';
          exit;
        end;
      res := self._json_get_string(_StrToByte(msgarr[0]));
      result := res;
      exit;
    end;


  function TYSpiPort.uploadJob(jobfile: string; jsonDef: string):LongInt;
    begin
      self._upload(jobfile, _StrToByte(jsonDef));
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSpiPort.selectJob(jobfile: string):LongInt;
    begin
      result := self.set_currentJob(jobfile);
      exit;
    end;


  function TYSpiPort.reset():LongInt;
    begin
      self._rxptr := 0;
      self._rxbuffptr := 0;
      setlength(self._rxbuff,0);

      result := self.sendCommand('Z');
      exit;
    end;


  function TYSpiPort.writeByte(code: LongInt):LongInt;
    begin
      result := self.sendCommand('$'+AnsiUpperCase(inttohex(code,02)));
      exit;
    end;


  function TYSpiPort.writeStr(text: string):LongInt;
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


  function TYSpiPort.writeBin(buff: TByteArray):LongInt;
    begin
      result := self._upload('txdata', buff);
      exit;
    end;


  function TYSpiPort.writeArray(byteList: TLongIntArray):LongInt;
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


  function TYSpiPort.writeHex(hexString: string):LongInt;
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
          hexb := StrToInt('$0' + Copy(hexString,  2 * idx + 1, 2));
          buff[idx] := hexb;
          idx := idx + 1;
        end;

      res := self._upload('txdata', buff);
      result := res;
      exit;
    end;


  function TYSpiPort.writeLine(text: string):LongInt;
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


  function TYSpiPort.readByte():LongInt;
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


  function TYSpiPort.readStr(nChars: LongInt):string;
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

      buff := self._download('rxdata.bin?pos='+inttostr( self._rxptr)+'&len='+inttostr(nChars));
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
      res := Copy(_ByteToString(buff),  0 + 1, bufflen);
      result := res;
      exit;
    end;


  function TYSpiPort.readBin(nChars: LongInt):TByteArray;
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

      buff := self._download('rxdata.bin?pos='+inttostr( self._rxptr)+'&len='+inttostr(nChars));
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


  function TYSpiPort.readArray(nChars: LongInt):TLongIntArray;
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

      buff := self._download('rxdata.bin?pos='+inttostr( self._rxptr)+'&len='+inttostr(nChars));
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


  function TYSpiPort.readHex(nBytes: LongInt):string;
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

      buff := self._download('rxdata.bin?pos='+inttostr( self._rxptr)+'&len='+inttostr(nBytes));
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
          res := ''+ res+''+AnsiUpperCase(inttohex( buff[ofs],02))+''+AnsiUpperCase(inttohex( buff[ofs + 1],02))+''+AnsiUpperCase(inttohex( buff[ofs + 2],02))+''+AnsiUpperCase(inttohex(buff[ofs + 3],02));
          ofs := ofs + 4;
        end;
      while ofs < bufflen do
        begin
          res := ''+ res+''+AnsiUpperCase(inttohex(buff[ofs],02));
          ofs := ofs + 1;
        end;
      result := res;
      exit;
    end;


  function TYSpiPort.set_SS(val: LongInt):LongInt;
    begin
      result := self.sendCommand('S'+inttostr(val));
      exit;
    end;


  function TYSpiPort.snoopMessages(maxWait: LongInt):TYSpiSnoopingRecordArray;
    var
      url : string;
      msgbin : TByteArray;
      msgarr : TStringArray;
      msglen : LongInt;
      res : TYSpiSnoopingRecordArray;
      idx : LongInt;
      res_pos : LongInt;
    begin
      SetLength(msgarr, 0);

      url := 'rxmsg.json?pos='+inttostr( self._rxptr)+'&maxw='+inttostr(maxWait)+'&t=0';
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
      self._rxptr := _atoi(msgarr[msglen]);
      idx := 0;
      res_pos := length(res);
      SetLength(res, res_pos+msglen);;
      while idx < msglen do
        begin
          res[res_pos] := TYSpiSnoopingRecord.create(msgarr[idx]);
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSpiPort.nextSpiPort(): TYSpiPort;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextSpiPort := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextSpiPort := nil;
          exit;
        end;
      nextSpiPort := TYSpiPort.FindSpiPort(hwid);
    end;

  class function TYSpiPort.FirstSpiPort(): TYSpiPort;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('SpiPort', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYSpiPort.FindSpiPort(serial+'.'+funcId);
    end;

//--- (end of generated code: YSpiPort implementation)

//--- (generated code: YSpiPort functions)

  function yFindSpiPort(func:string): TYSpiPort;
    begin
      result := TYSpiPort.FindSpiPort(func);
    end;

  function yFirstSpiPort(): TYSpiPort;
    begin
      result := TYSpiPort.FirstSpiPort();
    end;

  procedure _SpiPortCleanup();
    begin
    end;

//--- (end of generated code: YSpiPort functions)



//--- (generated code: YSpiSnoopingRecord implementation)

  function TYSpiSnoopingRecord.get_time():LongInt;
    begin
      result := self._tim;
      exit;
    end;


  function TYSpiSnoopingRecord.get_direction():LongInt;
    begin
      result := self._dir;
      exit;
    end;


  function TYSpiSnoopingRecord.get_message():string;
    begin
      result := self._msg;
      exit;
    end;


//--- (end of generated code: YSpiSnoopingRecord implementation)


constructor TYSpiSnoopingRecord.create(data:string);
 var
   p : TJSONparser;
   node : PJSONRECORD;
   tmp : string;
   c: char;
 begin
   p := TJsonParser.create(data,false);
   node:= p.GetChildNode(nil,'t');
   self._tim:=node^.ivalue;
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

//--- (generated code: YSpiSnoopingRecord functions)

  procedure _SpiSnoopingRecordCleanup();
    begin
    end;

//--- (end of generated code: YSpiSnoopingRecord functions)

procedure freeSpiSnoopingRecordArray(var list:TYSpiSnoopingRecordARRAY);
 var i:integer;
 begin
  for i:=0 to length(list)-1 do
   list[i].free();
  setLength(list,0);
 end;




initialization
  //--- (generated code: YSpiPort initialization)
  //--- (end of generated code: YSpiPort initialization)
  //--- (generated code: YSpiSnoopingRecord initialization)
  //--- (end of generated code: YSpiSnoopingRecord initialization)

finalization
  //--- (generated code: YSpiPort cleanup)
  _SpiPortCleanup();
  //--- (end of generated code: YSpiPort cleanup)
  //--- (generated code: YSpiSnoopingRecord cleanup)
  //--- (end of generated code: YSpiSnoopingRecord cleanup)

end.
