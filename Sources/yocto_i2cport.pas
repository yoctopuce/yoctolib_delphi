{*********************************************************************
 *
 *  $Id: yocto_i2cport.pas 43619 2021-01-29 09:14:45Z mvuilleu $
 *
 *  Implements yFindI2cPort(), the high-level API for I2cPort functions
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


unit yocto_i2cport;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (generated code: YI2cPort definitions)

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
const Y_I2CVOLTAGELEVEL_OFF = 0;
const Y_I2CVOLTAGELEVEL_3V3 = 1;
const Y_I2CVOLTAGELEVEL_1V8 = 2;
const Y_I2CVOLTAGELEVEL_INVALID = -1;
const Y_I2CMODE_INVALID               = YAPI_INVALID_STRING;


//--- (end of generated code: YI2cPort definitions)
//--- (generated code: YI2cPort yapiwrapper declaration)
//--- (end of generated code: YI2cPort yapiwrapper declaration)

type
  TYI2cPort = class;
  TYI2cSnoopingRecord = class;

  //--- (generated code: YI2cSnoopingRecord class start)
  ////
  /// <summary>
  ///   T
  /// <para>
  ///   YI2cSnoopingRecord Class: Intercepted I2C message description, returned by <c>i2cPort.snoopMessages</c> method
  /// </para>
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  ///-
  TYI2cSnoopingRecord=class(TObject)
  //--- (end of generated code: YI2cSnoopingRecord class start)
  protected

    //--- (generated code: YI2cSnoopingRecord declaration)
    // Attributes (function value cache)
    _tim                      : LongInt;
    _dir                      : LongInt;
    _msg                      : string;

    //--- (end of generated code: YI2cSnoopingRecord declaration)
public
   constructor create(data:string);


   //--- (generated code: YI2cSnoopingRecord accessors declaration)
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


  //--- (end of generated code: YI2cSnoopingRecord accessors declaration)
end;


TYI2cSnoopingRecordARRAY = array of TYI2cSnoopingRecord;


  //--- (generated code: YI2cPort class start)
  TYI2cPortValueCallback = procedure(func: TYI2cPort; value:string);
  TYI2cPortTimedReportCallback = procedure(func: TYI2cPort; value:TYMeasure);

  ////
  /// <summary>
  ///   TYI2cPort Class: I2C port control interface, available for instance in the Yocto-I2C
  /// <para>
  ///   The <c>YI2cPort</c> classe allows you to fully drive a Yoctopuce I2C port.
  ///   It can be used to send and receive data, and to configure communication
  ///   parameters (baud rate, etc).
  ///   Note that Yoctopuce I2C ports are not exposed as virtual COM ports.
  ///   They are meant to be used in the same way as all Yoctopuce devices.
  /// </para>
  /// </summary>
  ///-
  TYI2cPort=class(TYFunction)
  //--- (end of generated code: YI2cPort class start)
  protected
  //--- (generated code: YI2cPort declaration)
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
    _i2cVoltageLevel          : Integer;
    _i2cMode                  : string;
    _valueCallbackI2cPort     : TYI2cPortValueCallback;
    _rxptr                    : LongInt;
    _rxbuff                   : TByteArray;
    _rxbuffptr                : LongInt;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YI2cPort declaration)

  public
    //--- (generated code: YI2cPort accessors declaration)
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
    ///   On failure, throws an exception or returns <c>YI2cPort.RXCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YI2cPort.TXCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YI2cPort.ERRCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YI2cPort.RXMSGCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YI2cPort.TXMSGCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YI2cPort.LASTMSG_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YI2cPort.CURRENTJOB_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YI2cPort.STARTUPJOB_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YI2cPort.JOBMAXTASK_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YI2cPort.JOBMAXSIZE_INVALID</c>.
    /// </para>
    ///-
    function get_jobMaxSize():LongInt;

    function get_command():string;

    function set_command(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the type of protocol used to send I2C messages, as a string.
    /// <para>
    ///   Possible values are
    ///   "Line" for messages separated by LF or
    ///   "Char" for continuous stream of codes.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the type of protocol used to send I2C messages, as a string
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YI2cPort.PROTOCOL_INVALID</c>.
    /// </para>
    ///-
    function get_protocol():string;

    ////
    /// <summary>
    ///   Changes the type of protocol used to send I2C messages.
    /// <para>
    ///   Possible values are
    ///   "Line" for messages separated by LF or
    ///   "Char" for continuous stream of codes.
    ///   The suffix "/[wait]ms" can be added to reduce the transmit rate so that there
    ///   is always at lest the specified number of milliseconds between each message sent.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the type of protocol used to send I2C messages
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
    ///   Returns the voltage level used on the I2C bus.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YI2cPort.I2CVOLTAGELEVEL_OFF</c>, <c>YI2cPort.I2CVOLTAGELEVEL_3V3</c> and
    ///   <c>YI2cPort.I2CVOLTAGELEVEL_1V8</c> corresponding to the voltage level used on the I2C bus
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YI2cPort.I2CVOLTAGELEVEL_INVALID</c>.
    /// </para>
    ///-
    function get_i2cVoltageLevel():Integer;

    ////
    /// <summary>
    ///   Changes the voltage level used on the I2C bus.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YI2cPort.I2CVOLTAGELEVEL_OFF</c>, <c>YI2cPort.I2CVOLTAGELEVEL_3V3</c> and
    ///   <c>YI2cPort.I2CVOLTAGELEVEL_1V8</c> corresponding to the voltage level used on the I2C bus
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
    function set_i2cVoltageLevel(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the I2C port communication parameters, as a string such as
    ///   "400kbps,2000ms,NoRestart".
    /// <para>
    ///   The string includes the baud rate, the
    ///   recovery delay after communications errors, and if needed the option
    ///   <c>NoRestart</c> to use a Stop/Start sequence instead of the
    ///   Restart state when performing read on the I2C bus.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the I2C port communication parameters, as a string such as
    ///   "400kbps,2000ms,NoRestart"
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YI2cPort.I2CMODE_INVALID</c>.
    /// </para>
    ///-
    function get_i2cMode():string;

    ////
    /// <summary>
    ///   Changes the I2C port communication parameters, with a string such as
    ///   "400kbps,2000ms".
    /// <para>
    ///   The string includes the baud rate, the
    ///   recovery delay after communications errors, and if needed the option
    ///   <c>NoRestart</c> to use a Stop/Start sequence instead of the
    ///   Restart state when performing read on the I2C bus.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the I2C port communication parameters, with a string such as
    ///   "400kbps,2000ms"
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
    function set_i2cMode(newval:string):integer;

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
    ///   Use the method <c>YI2cPort.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YI2cPort</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindI2cPort(func: string):TYI2cPort;

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
    function registerValueCallback(callback: TYI2cPortValueCallback):LongInt; overload;

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
    ///   Sends a one-way message (provided as a a binary buffer) to a device on the I2C bus.
    /// <para>
    ///   This function checks and reports communication errors on the I2C bus.
    /// </para>
    /// </summary>
    /// <param name="slaveAddr">
    ///   the 7-bit address of the slave device (without the direction bit)
    /// </param>
    /// <param name="buff">
    ///   the binary buffer to be sent
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function i2cSendBin(slaveAddr: LongInt; buff: TByteArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a one-way message (provided as a list of integer) to a device on the I2C bus.
    /// <para>
    ///   This function checks and reports communication errors on the I2C bus.
    /// </para>
    /// </summary>
    /// <param name="slaveAddr">
    ///   the 7-bit address of the slave device (without the direction bit)
    /// </param>
    /// <param name="values">
    ///   a list of data bytes to be sent
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function i2cSendArray(slaveAddr: LongInt; values: TLongIntArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a one-way message (provided as a a binary buffer) to a device on the I2C bus,
    ///   then read back the specified number of bytes from device.
    /// <para>
    ///   This function checks and reports communication errors on the I2C bus.
    /// </para>
    /// </summary>
    /// <param name="slaveAddr">
    ///   the 7-bit address of the slave device (without the direction bit)
    /// </param>
    /// <param name="buff">
    ///   the binary buffer to be sent
    /// </param>
    /// <param name="rcvCount">
    ///   the number of bytes to receive once the data bytes are sent
    /// </param>
    /// <returns>
    ///   a list of bytes with the data received from slave device.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty binary buffer.
    /// </para>
    ///-
    function i2cSendAndReceiveBin(slaveAddr: LongInt; buff: TByteArray; rcvCount: LongInt):TByteArray; overload; virtual;

    ////
    /// <summary>
    ///   Sends a one-way message (provided as a list of integer) to a device on the I2C bus,
    ///   then read back the specified number of bytes from device.
    /// <para>
    ///   This function checks and reports communication errors on the I2C bus.
    /// </para>
    /// </summary>
    /// <param name="slaveAddr">
    ///   the 7-bit address of the slave device (without the direction bit)
    /// </param>
    /// <param name="values">
    ///   a list of data bytes to be sent
    /// </param>
    /// <param name="rcvCount">
    ///   the number of bytes to receive once the data bytes are sent
    /// </param>
    /// <returns>
    ///   a list of bytes with the data received from slave device.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function i2cSendAndReceiveArray(slaveAddr: LongInt; values: TLongIntArray; rcvCount: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Sends a text-encoded I2C code stream to the I2C bus, as is.
    /// <para>
    ///   An I2C code stream is a string made of hexadecimal data bytes,
    ///   but that may also include the I2C state transitions code:
    ///   "{S}" to emit a start condition,
    ///   "{R}" for a repeated start condition,
    ///   "{P}" for a stop condition,
    ///   "xx" for receiving a data byte,
    ///   "{A}" to ack a data byte received and
    ///   "{N}" to nack a data byte received.
    ///   If a newline ("\n") is included in the stream, the message
    ///   will be terminated and a newline will also be added to the
    ///   receive stream.
    /// </para>
    /// </summary>
    /// <param name="codes">
    ///   the code stream to send
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function writeStr(codes: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a text-encoded I2C code stream to the I2C bus, and terminate
    ///   the message en relâchant le bus.
    /// <para>
    ///   An I2C code stream is a string made of hexadecimal data bytes,
    ///   but that may also include the I2C state transitions code:
    ///   "{S}" to emit a start condition,
    ///   "{R}" for a repeated start condition,
    ///   "{P}" for a stop condition,
    ///   "xx" for receiving a data byte,
    ///   "{A}" to ack a data byte received and
    ///   "{N}" to nack a data byte received.
    ///   At the end of the stream, a stop condition is added if missing
    ///   and a newline is added to the receive buffer as well.
    /// </para>
    /// </summary>
    /// <param name="codes">
    ///   the code stream to send
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function writeLine(codes: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a single byte to the I2C bus.
    /// <para>
    ///   Depending on the I2C bus state, the byte
    ///   will be interpreted as an address byte or a data byte.
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
    ///   Sends a byte sequence (provided as a hexadecimal string) to the I2C bus.
    /// <para>
    ///   Depending on the I2C bus state, the first byte will be interpreted as an
    ///   address byte or a data byte.
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
    ///   Sends a binary buffer to the I2C bus, as is.
    /// <para>
    ///   Depending on the I2C bus state, the first byte will be interpreted
    ///   as an address byte or a data byte.
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
    ///   Sends a byte sequence (provided as a list of bytes) to the I2C bus.
    /// <para>
    ///   Depending on the I2C bus state, the first byte will be interpreted as an
    ///   address byte or a data byte.
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
    ///   Retrieves messages (both direction) in the I2C port buffer, starting at current position.
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
    ///   an array of <c>YI2cSnoopingRecord</c> objects containing the messages found, if any.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function snoopMessages(maxWait: LongInt):TYI2cSnoopingRecordArray; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of I2C ports started using <c>yFirstI2cPort()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned I2C ports order.
    ///   If you want to find a specific an I2C port, use <c>I2cPort.findI2cPort()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YI2cPort</c> object, corresponding to
    ///   an I2C port currently online, or a <c>NIL</c> pointer
    ///   if there are no more I2C ports to enumerate.
    /// </returns>
    ///-
    function nextI2cPort():TYI2cPort;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstI2cPort():TYI2cPort;
  //--- (end of generated code: YI2cPort accessors declaration)
  end;

//--- (generated code: YI2cPort functions declaration)
  ////
  /// <summary>
  ///   Retrieves an I2C port for a given identifier.
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
  ///   This function does not require that the I2C port is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YI2cPort.isOnline()</c> to test if the I2C port is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   an I2C port by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the I2C port, for instance
  ///   <c>YI2CMK01.i2cPort</c>.
  /// </param>
  /// <returns>
  ///   a <c>YI2cPort</c> object allowing you to drive the I2C port.
  /// </returns>
  ///-
  function yFindI2cPort(func:string):TYI2cPort;
  ////
  /// <summary>
  ///   Starts the enumeration of I2C ports currently accessible.
  /// <para>
  ///   Use the method <c>YI2cPort.nextI2cPort()</c> to iterate on
  ///   next I2C ports.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YI2cPort</c> object, corresponding to
  ///   the first I2C port currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstI2cPort():TYI2cPort;

//--- (end of generated code: YI2cPort functions declaration)

implementation
//--- (generated code: YI2cPort dlldef)
//--- (end of generated code: YI2cPort dlldef)

  constructor TYI2cPort.Create(func:string);
    begin
      inherited Create(func);
      _className := 'I2cPort';
      //--- (generated code: YI2cPort accessors initialization)
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
      _i2cVoltageLevel := Y_I2CVOLTAGELEVEL_INVALID;
      _i2cMode := Y_I2CMODE_INVALID;
      _valueCallbackI2cPort := nil;
      _rxptr := 0;
      _rxbuffptr := 0;
      //--- (end of generated code: YI2cPort accessors initialization)
    end;

//--- (generated code: YI2cPort yapiwrapper)
//--- (end of generated code: YI2cPort yapiwrapper)

//--- (generated code: YI2cPort implementation)
{$HINTS OFF}
  function TYI2cPort._parseAttr(member:PJSONRECORD):integer;
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
      if (member^.name = 'i2cVoltageLevel') then
        begin
          _i2cVoltageLevel := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'i2cMode') then
        begin
          _i2cMode := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYI2cPort.get_rxCount():LongInt;
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


  function TYI2cPort.get_txCount():LongInt;
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


  function TYI2cPort.get_errCount():LongInt;
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


  function TYI2cPort.get_rxMsgCount():LongInt;
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


  function TYI2cPort.get_txMsgCount():LongInt;
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


  function TYI2cPort.get_lastMsg():string;
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


  function TYI2cPort.get_currentJob():string;
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


  function TYI2cPort.set_currentJob(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('currentJob',rest_val);
    end;

  function TYI2cPort.get_startupJob():string;
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


  function TYI2cPort.set_startupJob(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('startupJob',rest_val);
    end;

  function TYI2cPort.get_jobMaxTask():LongInt;
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


  function TYI2cPort.get_jobMaxSize():LongInt;
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


  function TYI2cPort.get_command():string;
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


  function TYI2cPort.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  function TYI2cPort.get_protocol():string;
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


  function TYI2cPort.set_protocol(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('protocol',rest_val);
    end;

  function TYI2cPort.get_i2cVoltageLevel():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_I2CVOLTAGELEVEL_INVALID;
              exit;
            end;
        end;
      res := self._i2cVoltageLevel;
      result := res;
      exit;
    end;


  function TYI2cPort.set_i2cVoltageLevel(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('i2cVoltageLevel',rest_val);
    end;

  function TYI2cPort.get_i2cMode():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_I2CMODE_INVALID;
              exit;
            end;
        end;
      res := self._i2cMode;
      result := res;
      exit;
    end;


  function TYI2cPort.set_i2cMode(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('i2cMode',rest_val);
    end;

  class function TYI2cPort.FindI2cPort(func: string):TYI2cPort;
    var
      obj : TYI2cPort;
    begin
      obj := TYI2cPort(TYFunction._FindFromCache('I2cPort', func));
      if obj = nil then
        begin
          obj :=  TYI2cPort.create(func);
          TYFunction._AddToCache('I2cPort',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYI2cPort.registerValueCallback(callback: TYI2cPortValueCallback):LongInt;
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
      self._valueCallbackI2cPort := callback;
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


  function TYI2cPort._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackI2cPort) <> nil) then
        begin
          self._valueCallbackI2cPort(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYI2cPort.sendCommand(text: string):LongInt;
    begin
      result := self.set_command(text);
      exit;
    end;


  function TYI2cPort.readLine():string;
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


  function TYI2cPort.readMessages(pattern: string; maxWait: LongInt):TStringArray;
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


  function TYI2cPort.read_seek(absPos: LongInt):LongInt;
    begin
      self._rxptr := absPos;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYI2cPort.read_tell():LongInt;
    begin
      result := self._rxptr;
      exit;
    end;


  function TYI2cPort.read_avail():LongInt;
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


  function TYI2cPort.queryLine(query: string; maxWait: LongInt):string;
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


  function TYI2cPort.queryHex(hexString: string; maxWait: LongInt):string;
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


  function TYI2cPort.uploadJob(jobfile: string; jsonDef: string):LongInt;
    begin
      self._upload(jobfile, _StrToByte(jsonDef));
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYI2cPort.selectJob(jobfile: string):LongInt;
    begin
      result := self.set_currentJob(jobfile);
      exit;
    end;


  function TYI2cPort.reset():LongInt;
    begin
      self._rxptr := 0;
      self._rxbuffptr := 0;
      setlength(self._rxbuff,0);

      result := self.sendCommand('Z');
      exit;
    end;


  function TYI2cPort.i2cSendBin(slaveAddr: LongInt; buff: TByteArray):LongInt;
    var
      nBytes : LongInt;
      idx : LongInt;
      val : LongInt;
      msg : string;
      reply : string;
    begin
      msg := '@'+AnsiLowerCase(inttohex(slaveAddr,02))+':';
      nBytes := length(buff);
      idx := 0;
      while idx < nBytes do
        begin
          val := buff[idx];
          msg := ''+ msg+''+AnsiLowerCase(inttohex(val,02));
          idx := idx + 1;
        end;

      reply := self.queryLine(msg, 1000);
      if not(Length(reply) > 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'No response from I2C device');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      idx := (pos('[N]!', reply) - 1);
      if not(idx < 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'No I2C ACK received');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      idx := (pos('!', reply) - 1);
      if not(idx < 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'I2C protocol error');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYI2cPort.i2cSendArray(slaveAddr: LongInt; values: TLongIntArray):LongInt;
    var
      nBytes : LongInt;
      idx : LongInt;
      val : LongInt;
      msg : string;
      reply : string;
    begin
      msg := '@'+AnsiLowerCase(inttohex(slaveAddr,02))+':';
      nBytes := length(values);
      idx := 0;
      while idx < nBytes do
        begin
          val := values[idx];
          msg := ''+ msg+''+AnsiLowerCase(inttohex(val,02));
          idx := idx + 1;
        end;

      reply := self.queryLine(msg, 1000);
      if not(Length(reply) > 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'No response from I2C device');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      idx := (pos('[N]!', reply) - 1);
      if not(idx < 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'No I2C ACK received');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      idx := (pos('!', reply) - 1);
      if not(idx < 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'I2C protocol error');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYI2cPort.i2cSendAndReceiveBin(slaveAddr: LongInt; buff: TByteArray; rcvCount: LongInt):TByteArray;
    var
      nBytes : LongInt;
      idx : LongInt;
      val : LongInt;
      msg : string;
      reply : string;
      rcvbytes : TByteArray;
    begin
      msg := '@'+AnsiLowerCase(inttohex(slaveAddr,02))+':';
      nBytes := length(buff);
      idx := 0;
      while idx < nBytes do
        begin
          val := buff[idx];
          msg := ''+ msg+''+AnsiLowerCase(inttohex(val,02));
          idx := idx + 1;
        end;
      idx := 0;
      while idx < rcvCount do
        begin
          msg := ''+msg+'xx';
          idx := idx + 1;
        end;

      reply := self.queryLine(msg, 1000);
      setlength(rcvbytes,0);
      if not(Length(reply) > 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'No response from I2C device');
          result:=rcvbytes;
          exit;
        end;
      idx := (pos('[N]!', reply) - 1);
      if not(idx < 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'No I2C ACK received');
          result:=rcvbytes;
          exit;
        end;
      idx := (pos('!', reply) - 1);
      if not(idx < 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'I2C protocol error');
          result:=rcvbytes;
          exit;
        end;
      reply := Copy(reply,  Length(reply)-2*rcvCount + 1, 2*rcvCount);
      rcvbytes := _hexStrToBin(reply);
      result := rcvbytes;
      exit;
    end;


  function TYI2cPort.i2cSendAndReceiveArray(slaveAddr: LongInt; values: TLongIntArray; rcvCount: LongInt):TLongIntArray;
    var
      nBytes : LongInt;
      idx : LongInt;
      val : LongInt;
      msg : string;
      reply : string;
      rcvbytes : TByteArray;
      res : TLongIntArray;
      res_pos : LongInt;
    begin
      msg := '@'+AnsiLowerCase(inttohex(slaveAddr,02))+':';
      nBytes := length(values);
      idx := 0;
      while idx < nBytes do
        begin
          val := values[idx];
          msg := ''+ msg+''+AnsiLowerCase(inttohex(val,02));
          idx := idx + 1;
        end;
      idx := 0;
      while idx < rcvCount do
        begin
          msg := ''+msg+'xx';
          idx := idx + 1;
        end;

      reply := self.queryLine(msg, 1000);
      if not(Length(reply) > 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'No response from I2C device');
          result:=res;
          exit;
        end;
      idx := (pos('[N]!', reply) - 1);
      if not(idx < 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'No I2C ACK received');
          result:=res;
          exit;
        end;
      idx := (pos('!', reply) - 1);
      if not(idx < 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'I2C protocol error');
          result:=res;
          exit;
        end;
      reply := Copy(reply,  Length(reply)-2*rcvCount + 1, 2*rcvCount);
      rcvbytes := _hexStrToBin(reply);
      res_pos := 0;
      SetLength(res, rcvCount);;
      idx := 0;
      while idx < rcvCount do
        begin
          val := rcvbytes[idx];
          res[res_pos] := val;
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYI2cPort.writeStr(codes: string):LongInt;
    var
      bufflen : LongInt;
      buff : TByteArray;
      idx : LongInt;
      ch : LongInt;
    begin
      buff := _StrToByte(codes);
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
              result := self.sendCommand('+'+codes);
              exit;
            end;
        end;
      // send string using file upload
      result := self._upload('txdata', buff);
      exit;
    end;


  function TYI2cPort.writeLine(codes: string):LongInt;
    var
      bufflen : LongInt;
      buff : TByteArray;
    begin
      bufflen := Length(codes);
      if bufflen < 100 then
        begin
          result := self.sendCommand('!'+codes);
          exit;
        end;
      // send string using file upload
      buff := _StrToByte(''+codes+''#10'');
      result := self._upload('txdata', buff);
      exit;
    end;


  function TYI2cPort.writeByte(code: LongInt):LongInt;
    begin
      result := self.sendCommand('+'+AnsiUpperCase(inttohex(code,02)));
      exit;
    end;


  function TYI2cPort.writeHex(hexString: string):LongInt;
    var
      bufflen : LongInt;
      buff : TByteArray;
    begin
      bufflen := Length(hexString);
      if bufflen < 100 then
        begin
          result := self.sendCommand('+'+hexString);
          exit;
        end;
      buff := _StrToByte(hexString);

      result := self._upload('txdata', buff);
      exit;
    end;


  function TYI2cPort.writeBin(buff: TByteArray):LongInt;
    var
      nBytes : LongInt;
      idx : LongInt;
      val : LongInt;
      msg : string;
    begin
      msg := '';
      nBytes := length(buff);
      idx := 0;
      while idx < nBytes do
        begin
          val := buff[idx];
          msg := ''+ msg+''+AnsiLowerCase(inttohex(val,02));
          idx := idx + 1;
        end;

      result := self.writeHex(msg);
      exit;
    end;


  function TYI2cPort.writeArray(byteList: TLongIntArray):LongInt;
    var
      nBytes : LongInt;
      idx : LongInt;
      val : LongInt;
      msg : string;
    begin
      msg := '';
      nBytes := length(byteList);
      idx := 0;
      while idx < nBytes do
        begin
          val := byteList[idx];
          msg := ''+ msg+''+AnsiLowerCase(inttohex(val,02));
          idx := idx + 1;
        end;

      result := self.writeHex(msg);
      exit;
    end;


  function TYI2cPort.snoopMessages(maxWait: LongInt):TYI2cSnoopingRecordArray;
    var
      url : string;
      msgbin : TByteArray;
      msgarr : TStringArray;
      msglen : LongInt;
      res : TYI2cSnoopingRecordArray;
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
          res[res_pos] := TYI2cSnoopingRecord.create(msgarr[idx]);
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYI2cPort.nextI2cPort(): TYI2cPort;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextI2cPort := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextI2cPort := nil;
          exit;
        end;
      nextI2cPort := TYI2cPort.FindI2cPort(hwid);
    end;

  class function TYI2cPort.FirstI2cPort(): TYI2cPort;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('I2cPort', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYI2cPort.FindI2cPort(serial+'.'+funcId);
    end;

//--- (end of generated code: YI2cPort implementation)

//--- (generated code: YI2cPort functions)

  function yFindI2cPort(func:string): TYI2cPort;
    begin
      result := TYI2cPort.FindI2cPort(func);
    end;

  function yFirstI2cPort(): TYI2cPort;
    begin
      result := TYI2cPort.FirstI2cPort();
    end;

  procedure _I2cPortCleanup();
    begin
    end;

//--- (end of generated code: YI2cPort functions)




//--- (generated code: YI2cSnoopingRecord implementation)

  function TYI2cSnoopingRecord.get_time():LongInt;
    begin
      result := self._tim;
      exit;
    end;


  function TYI2cSnoopingRecord.get_direction():LongInt;
    begin
      result := self._dir;
      exit;
    end;


  function TYI2cSnoopingRecord.get_message():string;
    begin
      result := self._msg;
      exit;
    end;


//--- (end of generated code: YI2cSnoopingRecord implementation)


constructor TYI2cSnoopingRecord.create(data:string);
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

//--- (generated code: YI2cSnoopingRecord functions)

  procedure _I2cSnoopingRecordCleanup();
    begin
    end;

//--- (end of generated code: YI2cSnoopingRecord functions)

procedure freeI2cSnoopingRecordArray(var list:TYI2cSnoopingRecordARRAY);
 var i:integer;
 begin
  for i:=0 to length(list)-1 do
   list[i].free();
  setLength(list,0);
 end;



initialization
  //--- (generated code: YI2cPort initialization)
  //--- (end of generated code: YI2cPort initialization)
  //--- (generated code: YI2cSnoopingRecord initialization)
  //--- (end of generated code: YI2cSnoopingRecord initialization)

finalization
  //--- (generated code: YI2cPort cleanup)
  _I2cPortCleanup();
  //--- (end of generated code: YI2cPort cleanup)
  //--- (generated code: YI2cSnoopingRecord cleanup)
  //--- (end of generated code: YI2cSnoopingRecord cleanup)

end.
