{*********************************************************************
 *
 * $Id: yocto_serialport.pas 66665 2025-05-14 07:32:24Z seb $
 *
 * Implements yFindSerialPort(), the high-level API for SerialPort functions
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


unit yocto_serialport;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,{$IFNDEF UNIX}windows,
{$ENDIF}
 yocto_api, yjson;

//--- (generated code: YSerialPort definitions)

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

//--- (end of generated code: YSerialPort definitions)

type
  TYSerialPort = class;
  TYSnoopingRecord = class;

  //--- (generated code: YSnoopingRecord class start)
  ////
  /// <summary>
  ///   T
  /// <para>
  ///   YSnoopingRecord Class: Intercepted serial message description, returned by
  ///   <c>serialPort.snoopMessages</c> method
  /// </para>
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  ///-
  TYSnoopingRecord=class(TObject)
  //--- (end of generated code: YSnoopingRecord class start)
  protected

    //--- (generated code: YSnoopingRecord declaration)
    // Attributes (function value cache)
    _tim                      : LongInt;
    _pos                      : LongInt;
    _dir                      : LongInt;
    _msg                      : string;
    //--- (end of generated code: YSnoopingRecord declaration)
public
   constructor create(data:string);


   //--- (generated code: YSnoopingRecord accessors declaration)

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


  //--- (end of generated code: YSnoopingRecord accessors declaration)
end;


TYSNOOPINGRECORDARRAY = array of TYSnoopingRecord;


  //--- (generated code: YSerialPort class start)
  TYSerialPortValueCallback = procedure(func: TYSerialPort; value:string);
  TYSerialPortTimedReportCallback = procedure(func: TYSerialPort; value:TYMeasure);
  TYSnoopingCallback = procedure(func: TYSerialPort; rec: TYSnoopingRecord);

  ////
  /// <summary>
  ///   TYSerialPort Class: serial port control interface, available for instance in the Yocto-RS232, the
  ///   Yocto-RS485-V2 or the Yocto-Serial
  /// <para>
  ///   The <c>YSerialPort</c> class allows you to fully drive a Yoctopuce serial port.
  ///   It can be used to send and receive data, and to configure communication
  ///   parameters (baud rate, bit count, parity, flow control and protocol).
  ///   Note that Yoctopuce serial ports are not exposed as virtual COM ports.
  ///   They are meant to be used in the same way as all Yoctopuce devices.
  /// </para>
  /// </summary>
  ///-
  TYSerialPort=class(TYFunction)
  //--- (end of generated code: YSerialPort class start)
  protected
  //--- (generated code: YSerialPort declaration)
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
    _valueCallbackSerialPort  : TYSerialPortValueCallback;
    _rxptr                    : LongInt;
    _rxbuff                   : TByteArray;
    _rxbuffptr                : LongInt;
    _eventPos                 : LongInt;
    _eventCallback            : TYSnoopingCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of generated code: YSerialPort declaration)

  public
    //--- (generated code: YSerialPort accessors declaration)
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
    ///   On failure, throws an exception or returns <c>YSerialPort.RXCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSerialPort.TXCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSerialPort.ERRCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSerialPort.RXMSGCOUNT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSerialPort.TXMSGCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_txMsgCount():LongInt;

    ////
    /// <summary>
    ///   Returns the latest message fully received (for Line, Frame and Modbus protocols).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the latest message fully received (for Line, Frame and Modbus protocols)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSerialPort.LASTMSG_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSerialPort.CURRENTJOB_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSerialPort.STARTUPJOB_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSerialPort.JOBMAXTASK_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YSerialPort.JOBMAXSIZE_INVALID</c>.
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
    ///   "StxEtx" for ASCII messages delimited by STX/ETX codes,
    ///   "Frame:[timeout]ms" for binary messages separated by a delay time,
    ///   "Modbus-ASCII" for MODBUS messages in ASCII mode,
    ///   "Modbus-RTU" for MODBUS messages in RTU mode,
    ///   "Wiegand-ASCII" for Wiegand messages in ASCII mode,
    ///   "Wiegand-26","Wiegand-34", etc for Wiegand messages in byte mode,
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
    ///   On failure, throws an exception or returns <c>YSerialPort.PROTOCOL_INVALID</c>.
    /// </para>
    ///-
    function get_protocol():string;

    ////
    /// <summary>
    ///   Changes the type of protocol used over the serial line.
    /// <para>
    ///   Possible values are "Line" for ASCII messages separated by CR and/or LF,
    ///   "StxEtx" for ASCII messages delimited by STX/ETX codes,
    ///   "Frame:[timeout]ms" for binary messages separated by a delay time,
    ///   "Modbus-ASCII" for MODBUS messages in ASCII mode,
    ///   "Modbus-RTU" for MODBUS messages in RTU mode,
    ///   "Wiegand-ASCII" for Wiegand messages in ASCII mode,
    ///   "Wiegand-26","Wiegand-34", etc for Wiegand messages in byte mode,
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
    ///   a value among <c>YSerialPort.VOLTAGELEVEL_OFF</c>, <c>YSerialPort.VOLTAGELEVEL_TTL3V</c>,
    ///   <c>YSerialPort.VOLTAGELEVEL_TTL3VR</c>, <c>YSerialPort.VOLTAGELEVEL_TTL5V</c>,
    ///   <c>YSerialPort.VOLTAGELEVEL_TTL5VR</c>, <c>YSerialPort.VOLTAGELEVEL_RS232</c>,
    ///   <c>YSerialPort.VOLTAGELEVEL_RS485</c>, <c>YSerialPort.VOLTAGELEVEL_TTL1V8</c> and
    ///   <c>YSerialPort.VOLTAGELEVEL_SDI12</c> corresponding to the voltage level used on the serial line
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSerialPort.VOLTAGELEVEL_INVALID</c>.
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
    ///   a value among <c>YSerialPort.VOLTAGELEVEL_OFF</c>, <c>YSerialPort.VOLTAGELEVEL_TTL3V</c>,
    ///   <c>YSerialPort.VOLTAGELEVEL_TTL3VR</c>, <c>YSerialPort.VOLTAGELEVEL_TTL5V</c>,
    ///   <c>YSerialPort.VOLTAGELEVEL_TTL5VR</c>, <c>YSerialPort.VOLTAGELEVEL_RS232</c>,
    ///   <c>YSerialPort.VOLTAGELEVEL_RS485</c>, <c>YSerialPort.VOLTAGELEVEL_TTL1V8</c> and
    ///   <c>YSerialPort.VOLTAGELEVEL_SDI12</c> corresponding to the voltage type used on the serial line
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
    ///   "9600,8N1".
    /// <para>
    ///   The string includes the baud rate, the number of data bits,
    ///   the parity, and the number of stop bits. An optional suffix is included
    ///   if flow control is active: "CtsRts" for hardware handshake, "XOnXOff"
    ///   for logical flow control and "Simplex" for acquiring a shared bus using
    ///   the RTS line (as used by some RS485 adapters for instance).
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the serial port communication parameters, as a string such as
    ///   "9600,8N1"
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSerialPort.SERIALMODE_INVALID</c>.
    /// </para>
    ///-
    function get_serialMode():string;

    ////
    /// <summary>
    ///   Changes the serial port communication parameters, with a string such as
    ///   "9600,8N1".
    /// <para>
    ///   The string includes the baud rate, the number of data bits,
    ///   the parity, and the number of stop bits. An optional suffix can be added
    ///   to enable flow control: "CtsRts" for hardware handshake, "XOnXOff"
    ///   for logical flow control and "Simplex" for acquiring a shared bus using
    ///   the RTS line (as used by some RS485 adapters for instance).
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the serial port communication parameters, with a string such as
    ///   "9600,8N1"
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
    ///   Use the method <c>YSerialPort.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YSerialPort</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindSerialPort(func: string):TYSerialPort;

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
    function registerValueCallback(callback: TYSerialPortValueCallback):LongInt; overload;

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
    ///   Emits a BREAK condition on the serial interface.
    /// <para>
    ///   When the specified
    ///   duration is 0, the BREAK signal will be exactly one character wide.
    ///   When the duration is between 1 and 100, the BREAK condition will
    ///   be hold for the specified number of milliseconds.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="duration">
    ///   0 for a standard BREAK, or duration between 1 and 100 ms
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function sendBreak(duration: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Manually sets the state of the RTS line.
    /// <para>
    ///   This function has no effect when
    ///   hardware handshake is enabled, as the RTS line is driven automatically.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="val">
    ///   1 to turn RTS on, 0 to turn RTS off
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_RTS(val: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reads the level of the CTS line.
    /// <para>
    ///   The CTS line is usually driven by
    ///   the RTS signal of the connected serial device.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   1 if the CTS line is high, 0 if the CTS line is low.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function get_CTS():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Retrieves messages (both direction) in the serial port buffer, starting at current position.
    /// <para>
    ///   This function will only compare and return printable characters in the message strings.
    ///   Binary protocols are handled as hexadecimal strings.
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
    ///   an array of <c>YSnoopingRecord</c> objects containing the messages found, if any.
    ///   Binary messages are converted to hexadecimal representation.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function snoopMessagesEx(maxWait: LongInt; maxMsg: LongInt):TYSnoopingRecordArray; overload; virtual;

    ////
    /// <summary>
    ///   Retrieves messages (both direction) in the serial port buffer, starting at current position.
    /// <para>
    ///   This function will only compare and return printable characters in the message strings.
    ///   Binary protocols are handled as hexadecimal strings.
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
    ///   an array of <c>YSnoopingRecord</c> objects containing the messages found, if any.
    ///   Binary messages are converted to hexadecimal representation.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function snoopMessages(maxWait: LongInt):TYSnoopingRecordArray; overload; virtual;

    ////
    /// <summary>
    ///   Registers a callback function to be called each time that a message is sent or
    ///   received by the serial port.
    /// <para>
    ///   The callback is invoked only during the execution of
    ///   <c>ySleep</c> or <c>yHandleEvents</c>. This provides control over the time when
    ///   the callback is triggered. For good responsiveness, remember to call one of these
    ///   two functions periodically. To unregister a callback, pass a NIL pointer as argument.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="callback">
    ///   the callback function to call, or a NIL pointer.
    ///   The callback function should take four arguments:
    ///   the <c>YSerialPort</c> object that emitted the event, and
    ///   the <c>YSnoopingRecord</c> object that describes the message
    ///   sent or received.
    ///   On failure, throws an exception or returns a negative error code.
    /// </param>
    ///-
    function registerSnoopingCallback(callback: TYSnoopingCallback):LongInt; overload; virtual;

    function _internalEventHandler(advstr: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends an ASCII string to the serial port, preceeded with an STX code and
    ///   followed by an ETX code.
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
    function writeStxEtx(text: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a MODBUS message (provided as a hexadecimal string) to the serial port.
    /// <para>
    ///   The message must start with the slave address. The MODBUS CRC/LRC is
    ///   automatically added by the function. This function does not wait for a reply.
    /// </para>
    /// </summary>
    /// <param name="hexString">
    ///   a hexadecimal message string, including device address but no CRC/LRC
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function writeMODBUS(hexString: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a message to a specified MODBUS slave connected to the serial port, and reads the
    ///   reply, if any.
    /// <para>
    ///   The message is the PDU, provided as a vector of bytes.
    /// </para>
    /// </summary>
    /// <param name="slaveNo">
    ///   the address of the slave MODBUS device to query
    /// </param>
    /// <param name="pduBytes">
    ///   the message to send (PDU), as a vector of bytes. The first byte of the
    ///   PDU is the MODBUS function code.
    /// </param>
    /// <returns>
    ///   the received reply, as a vector of bytes.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array (or a MODBUS error reply).
    /// </para>
    ///-
    function queryMODBUS(slaveNo: LongInt; pduBytes: TLongIntArray):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Reads one or more contiguous internal bits (or coil status) from a MODBUS serial device.
    /// <para>
    ///   This method uses the MODBUS function code 0x01 (Read Coils).
    /// </para>
    /// </summary>
    /// <param name="slaveNo">
    ///   the address of the slave MODBUS device to query
    /// </param>
    /// <param name="pduAddr">
    ///   the relative address of the first bit/coil to read (zero-based)
    /// </param>
    /// <param name="nBits">
    ///   the number of bits/coils to read
    /// </param>
    /// <returns>
    ///   a vector of integers, each corresponding to one bit.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function modbusReadBits(slaveNo: LongInt; pduAddr: LongInt; nBits: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Reads one or more contiguous input bits (or discrete inputs) from a MODBUS serial device.
    /// <para>
    ///   This method uses the MODBUS function code 0x02 (Read Discrete Inputs).
    /// </para>
    /// </summary>
    /// <param name="slaveNo">
    ///   the address of the slave MODBUS device to query
    /// </param>
    /// <param name="pduAddr">
    ///   the relative address of the first bit/input to read (zero-based)
    /// </param>
    /// <param name="nBits">
    ///   the number of bits/inputs to read
    /// </param>
    /// <returns>
    ///   a vector of integers, each corresponding to one bit.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function modbusReadInputBits(slaveNo: LongInt; pduAddr: LongInt; nBits: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Reads one or more contiguous internal registers (holding registers) from a MODBUS serial device.
    /// <para>
    ///   This method uses the MODBUS function code 0x03 (Read Holding Registers).
    /// </para>
    /// </summary>
    /// <param name="slaveNo">
    ///   the address of the slave MODBUS device to query
    /// </param>
    /// <param name="pduAddr">
    ///   the relative address of the first holding register to read (zero-based)
    /// </param>
    /// <param name="nWords">
    ///   the number of holding registers to read
    /// </param>
    /// <returns>
    ///   a vector of integers, each corresponding to one 16-bit register value.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function modbusReadRegisters(slaveNo: LongInt; pduAddr: LongInt; nWords: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Reads one or more contiguous input registers (read-only registers) from a MODBUS serial device.
    /// <para>
    ///   This method uses the MODBUS function code 0x04 (Read Input Registers).
    /// </para>
    /// </summary>
    /// <param name="slaveNo">
    ///   the address of the slave MODBUS device to query
    /// </param>
    /// <param name="pduAddr">
    ///   the relative address of the first input register to read (zero-based)
    /// </param>
    /// <param name="nWords">
    ///   the number of input registers to read
    /// </param>
    /// <returns>
    ///   a vector of integers, each corresponding to one 16-bit input value.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function modbusReadInputRegisters(slaveNo: LongInt; pduAddr: LongInt; nWords: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Sets a single internal bit (or coil) on a MODBUS serial device.
    /// <para>
    ///   This method uses the MODBUS function code 0x05 (Write Single Coil).
    /// </para>
    /// </summary>
    /// <param name="slaveNo">
    ///   the address of the slave MODBUS device to drive
    /// </param>
    /// <param name="pduAddr">
    ///   the relative address of the bit/coil to set (zero-based)
    /// </param>
    /// <param name="value">
    ///   the value to set (0 for OFF state, non-zero for ON state)
    /// </param>
    /// <returns>
    ///   the number of bits/coils affected on the device (1)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns zero.
    /// </para>
    ///-
    function modbusWriteBit(slaveNo: LongInt; pduAddr: LongInt; value: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets several contiguous internal bits (or coils) on a MODBUS serial device.
    /// <para>
    ///   This method uses the MODBUS function code 0x0f (Write Multiple Coils).
    /// </para>
    /// </summary>
    /// <param name="slaveNo">
    ///   the address of the slave MODBUS device to drive
    /// </param>
    /// <param name="pduAddr">
    ///   the relative address of the first bit/coil to set (zero-based)
    /// </param>
    /// <param name="bits">
    ///   the vector of bits to be set (one integer per bit)
    /// </param>
    /// <returns>
    ///   the number of bits/coils affected on the device
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns zero.
    /// </para>
    ///-
    function modbusWriteBits(slaveNo: LongInt; pduAddr: LongInt; bits: TLongIntArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets a single internal register (or holding register) on a MODBUS serial device.
    /// <para>
    ///   This method uses the MODBUS function code 0x06 (Write Single Register).
    /// </para>
    /// </summary>
    /// <param name="slaveNo">
    ///   the address of the slave MODBUS device to drive
    /// </param>
    /// <param name="pduAddr">
    ///   the relative address of the register to set (zero-based)
    /// </param>
    /// <param name="value">
    ///   the 16 bit value to set
    /// </param>
    /// <returns>
    ///   the number of registers affected on the device (1)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns zero.
    /// </para>
    ///-
    function modbusWriteRegister(slaveNo: LongInt; pduAddr: LongInt; value: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets several contiguous internal registers (or holding registers) on a MODBUS serial device.
    /// <para>
    ///   This method uses the MODBUS function code 0x10 (Write Multiple Registers).
    /// </para>
    /// </summary>
    /// <param name="slaveNo">
    ///   the address of the slave MODBUS device to drive
    /// </param>
    /// <param name="pduAddr">
    ///   the relative address of the first internal register to set (zero-based)
    /// </param>
    /// <param name="values">
    ///   the vector of 16 bit values to set
    /// </param>
    /// <returns>
    ///   the number of registers affected on the device
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns zero.
    /// </para>
    ///-
    function modbusWriteRegisters(slaveNo: LongInt; pduAddr: LongInt; values: TLongIntArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets several contiguous internal registers (holding registers) on a MODBUS serial device,
    ///   then performs a contiguous read of a set of (possibly different) internal registers.
    /// <para>
    ///   This method uses the MODBUS function code 0x17 (Read/Write Multiple Registers).
    /// </para>
    /// </summary>
    /// <param name="slaveNo">
    ///   the address of the slave MODBUS device to drive
    /// </param>
    /// <param name="pduWriteAddr">
    ///   the relative address of the first internal register to set (zero-based)
    /// </param>
    /// <param name="values">
    ///   the vector of 16 bit values to set
    /// </param>
    /// <param name="pduReadAddr">
    ///   the relative address of the first internal register to read (zero-based)
    /// </param>
    /// <param name="nReadWords">
    ///   the number of 16 bit values to read
    /// </param>
    /// <returns>
    ///   a vector of integers, each corresponding to one 16-bit register value read.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function modbusWriteAndReadRegisters(slaveNo: LongInt; pduWriteAddr: LongInt; values: TLongIntArray; pduReadAddr: LongInt; nReadWords: LongInt):TLongIntArray; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of serial ports started using <c>yFirstSerialPort()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned serial ports order.
    ///   If you want to find a specific a serial port, use <c>SerialPort.findSerialPort()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YSerialPort</c> object, corresponding to
    ///   a serial port currently online, or a <c>NIL</c> pointer
    ///   if there are no more serial ports to enumerate.
    /// </returns>
    ///-
    function nextSerialPort():TYSerialPort;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstSerialPort():TYSerialPort;
  //--- (end of generated code: YSerialPort accessors declaration)
  end;

//--- (generated code: YSerialPort functions declaration)
  ////
  /// <summary>
  ///   Retrieves a serial port for a given identifier.
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
  ///   This function does not require that the serial port is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YSerialPort.isOnline()</c> to test if the serial port is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a serial port by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the serial port, for instance
  ///   <c>RS232MK1.serialPort</c>.
  /// </param>
  /// <returns>
  ///   a <c>YSerialPort</c> object allowing you to drive the serial port.
  /// </returns>
  ///-
  function yFindSerialPort(func:string):TYSerialPort;
  ////
  /// <summary>
  ///   Starts the enumeration of serial ports currently accessible.
  /// <para>
  ///   Use the method <c>YSerialPort.nextSerialPort()</c> to iterate on
  ///   next serial ports.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YSerialPort</c> object, corresponding to
  ///   the first serial port currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstSerialPort():TYSerialPort;

Procedure yInternalEventCallback(obj:TYSerialPort; value:string);

//--- (end of generated code: YSerialPort functions declaration)

implementation
//--- (generated code: YSerialPort dlldef)
//--- (end of generated code: YSerialPort dlldef)

  constructor TYSerialPort.Create(func:string);
    begin
      inherited Create(func);
      _className := 'SerialPort';
      //--- (generated code: YSerialPort accessors initialization)
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
      _valueCallbackSerialPort := nil;
      _rxptr := 0;
      _rxbuffptr := 0;
      _eventPos := 0;
      //--- (end of generated code: YSerialPort accessors initialization)
    end;


//--- (generated code: YSerialPort implementation)
{$HINTS OFF}
  function TYSerialPort._parseAttr(member:PJSONRECORD):integer;
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

  function TYSerialPort.get_rxCount():LongInt;
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


  function TYSerialPort.get_txCount():LongInt;
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


  function TYSerialPort.get_errCount():LongInt;
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


  function TYSerialPort.get_rxMsgCount():LongInt;
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


  function TYSerialPort.get_txMsgCount():LongInt;
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


  function TYSerialPort.get_lastMsg():string;
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


  function TYSerialPort.get_currentJob():string;
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


  function TYSerialPort.set_currentJob(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('currentJob',rest_val);
    end;

  function TYSerialPort.get_startupJob():string;
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


  function TYSerialPort.set_startupJob(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('startupJob',rest_val);
    end;

  function TYSerialPort.get_jobMaxTask():LongInt;
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


  function TYSerialPort.get_jobMaxSize():LongInt;
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


  function TYSerialPort.get_command():string;
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


  function TYSerialPort.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  function TYSerialPort.get_protocol():string;
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


  function TYSerialPort.set_protocol(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('protocol',rest_val);
    end;

  function TYSerialPort.get_voltageLevel():Integer;
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


  function TYSerialPort.set_voltageLevel(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('voltageLevel',rest_val);
    end;

  function TYSerialPort.get_serialMode():string;
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


  function TYSerialPort.set_serialMode(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('serialMode',rest_val);
    end;

  class function TYSerialPort.FindSerialPort(func: string):TYSerialPort;
    var
      obj : TYSerialPort;
    begin
      obj := TYSerialPort(TYFunction._FindFromCache('SerialPort', func));
      if obj = nil then
        begin
          obj :=  TYSerialPort.create(func);
          TYFunction._AddToCache('SerialPort', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYSerialPort.registerValueCallback(callback: TYSerialPortValueCallback):LongInt;
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
      self._valueCallbackSerialPort := callback;
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


  function TYSerialPort._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackSerialPort) <> nil) then
        begin
          self._valueCallbackSerialPort(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYSerialPort.sendCommand(text: string):LongInt;
    begin
      result := self.set_command(text);
      exit;
    end;


  function TYSerialPort.readLine():string;
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


  function TYSerialPort.readMessages(pattern: string; maxWait: LongInt):TStringArray;
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


  function TYSerialPort.read_seek(absPos: LongInt):LongInt;
    begin
      self._rxptr := absPos;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSerialPort.read_tell():LongInt;
    begin
      result := self._rxptr;
      exit;
    end;


  function TYSerialPort.read_avail():LongInt;
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


  function TYSerialPort.end_tell():LongInt;
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


  function TYSerialPort.queryLine(query: string; maxWait: LongInt):string;
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


  function TYSerialPort.queryHex(hexString: string; maxWait: LongInt):string;
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


  function TYSerialPort.uploadJob(jobfile: string; jsonDef: string):LongInt;
    begin
      self._upload(jobfile, _StrToByte(jsonDef));
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSerialPort.selectJob(jobfile: string):LongInt;
    begin
      result := self.set_currentJob(jobfile);
      exit;
    end;


  function TYSerialPort.reset():LongInt;
    begin
      self._eventPos := 0;
      self._rxptr := 0;
      self._rxbuffptr := 0;
      setlength(self._rxbuff,0);

      result := self.sendCommand('Z');
      exit;
    end;


  function TYSerialPort.writeByte(code: LongInt):LongInt;
    begin
      result := self.sendCommand('$'+AnsiUpperCase(inttohex(code,02)));
      exit;
    end;


  function TYSerialPort.writeStr(text: string):LongInt;
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


  function TYSerialPort.writeBin(buff: TByteArray):LongInt;
    begin
      result := self._upload('txdata', buff);
      exit;
    end;


  function TYSerialPort.writeArray(byteList: TLongIntArray):LongInt;
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


  function TYSerialPort.writeHex(hexString: string):LongInt;
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


  function TYSerialPort.writeLine(text: string):LongInt;
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


  function TYSerialPort.readByte():LongInt;
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


  function TYSerialPort.readStr(nChars: LongInt):string;
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


  function TYSerialPort.readBin(nChars: LongInt):TByteArray;
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


  function TYSerialPort.readArray(nChars: LongInt):TLongIntArray;
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


  function TYSerialPort.readHex(nBytes: LongInt):string;
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


  function TYSerialPort.sendBreak(duration: LongInt):LongInt;
    begin
      result := self.sendCommand('B'+inttostr(duration));
      exit;
    end;


  function TYSerialPort.set_RTS(val: LongInt):LongInt;
    begin
      result := self.sendCommand('R'+inttostr(val));
      exit;
    end;


  function TYSerialPort.get_CTS():LongInt;
    var
      buff : TByteArray;
      res : LongInt;
    begin
      buff := self._download('cts.txt');
      if not(length(buff) = 1) then
        begin
          self._throw(YAPI_IO_ERROR,'invalid CTS reply');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      res := buff[0] - 48;
      result := res;
      exit;
    end;


  function TYSerialPort.snoopMessagesEx(maxWait: LongInt; maxMsg: LongInt):TYSnoopingRecordArray;
    var
      url : string;
      msgbin : TByteArray;
      msgarr : TTByteArrayArray;
      msglen : LongInt;
      res : TYSnoopingRecordArray;
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
          res[res_pos] := TYSnoopingRecord.create(_ByteToString(msgarr[idx]));
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSerialPort.snoopMessages(maxWait: LongInt):TYSnoopingRecordArray;
    begin
      result := self.snoopMessagesEx(maxWait, 255);
      exit;
    end;


  function TYSerialPort.registerSnoopingCallback(callback: TYSnoopingCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          self.registerValueCallback(yInternalEventCallback);
        end
      else
        begin
          self.registerValueCallback(TYSerialPortValueCallback(nil));
        end;
      // register user callback AFTER the internal pseudo-event,
      // to make sure we start with future events only
      self._eventCallback := callback;
      result := 0;
      exit;
    end;


  function TYSerialPort._internalEventHandler(advstr: string):LongInt;
    var
      url : string;
      msgbin : TByteArray;
      msgarr : TTByteArrayArray;
      msglen : LongInt;
      idx : LongInt;
    begin
      if not((addr(self._eventCallback) <> nil)) then
        begin
          // first simulated event, use it only to initialize reference values
          self._eventPos := 0;
        end;

      url := 'rxmsg.json?pos='+inttostr(self._eventPos)+'&maxw=0&t=0';
      msgbin := self._download(url);
      msgarr := self._json_get_array(msgbin);
      msglen := length(msgarr);
      if msglen = 0 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      // last element of array is the new position
      msglen := msglen - 1;
      if not((addr(self._eventCallback) <> nil)) then
        begin
          // first simulated event, use it only to initialize reference values
          self._eventPos := self._decode_json_int(msgarr[msglen]);
          result := YAPI_SUCCESS;
          exit;
        end;
      self._eventPos := self._decode_json_int(msgarr[msglen]);
      idx := 0;
      while idx < msglen do
        begin
          self._eventCallback(self, TYSnoopingRecord.create(_ByteToString(msgarr[idx])));
          idx := idx + 1;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSerialPort.writeStxEtx(text: string):LongInt;
    var
      buff : TByteArray;
    begin
      buff := _StrToByte(''+chr(2)+''+text+''+chr(3));
      // send string using file upload
      result := self._upload('txdata', buff);
      exit;
    end;


  function TYSerialPort.writeMODBUS(hexString: string):LongInt;
    begin
      result := self.sendCommand(':'+hexString);
      exit;
    end;


  function TYSerialPort.queryMODBUS(slaveNo: LongInt; pduBytes: TLongIntArray):TLongIntArray;
    var
      funCode : LongInt;
      nib : LongInt;
      i : LongInt;
      cmd : string;
      prevpos : LongInt;
      url : string;
      pat : string;
      msgs : TByteArray;
      reps : TTByteArrayArray;
      rep : string;
      res : TLongIntArray;
      replen : LongInt;
      hexb : LongInt;
      res_pos : LongInt;
    begin
      funCode := pduBytes[0];
      nib := ((funCode) shr 4);
      pat := ''+AnsiUpperCase(inttohex(slaveNo,02))+'['+AnsiUpperCase(inttohex(nib,1))+''+AnsiUpperCase(inttohex((nib+8),1))+']'+AnsiUpperCase(inttohex(((funCode) and 15),1))+'.*';
      cmd := ''+AnsiUpperCase(inttohex(slaveNo,02))+''+AnsiUpperCase(inttohex(funCode,02));
      i := 1;
      while i < length(pduBytes) do
        begin
          cmd := ''+cmd+''+AnsiUpperCase(inttohex(((pduBytes[i]) and ($0ff)),02));
          i := i + 1;
        end;
      if Length(cmd) <= 80 then
        begin
          // fast query
          url := 'rxmsg.json?cmd=:'+cmd+'&pat=:'+pat;
        end
      else
        begin
          // long query
          prevpos := self.end_tell;
          self._upload('txdata:', _hexStrToBin(cmd));
          url := 'rxmsg.json?pos='+inttostr(prevpos)+'&maxw=2000&pat=:'+pat;
        end;

      msgs := self._download(url);
      reps := self._json_get_array(msgs);
      if not(length(reps) > 1) then
        begin
          self._throw(YAPI_IO_ERROR,'no reply from MODBUS slave');
          result:=res;
          exit;
        end;
      if length(reps) > 1 then
        begin
          rep := self._json_get_string(reps[0]);
          replen := ((Length(rep) - 3) shr 1);
          res_pos := length(res);
          SetLength(res, res_pos+replen);
          i := 0;
          while i < replen do
            begin
              hexb := StrToInt('$0' + Copy(rep, 2 * i + 3 + 1, 2));
              res[res_pos] := hexb;
              inc(res_pos);
              i := i + 1;
            end;
          SetLength(res, res_pos);
          if res[0] <> funCode then
            begin
              i := res[1];
              if not(i > 1) then
                begin
                  self._throw(YAPI_NOT_SUPPORTED,'MODBUS error: unsupported function code');
                  result:=res;
                  exit;
                end;
              if not(i > 2) then
                begin
                  self._throw(YAPI_INVALID_ARGUMENT,'MODBUS error: illegal data address');
                  result:=res;
                  exit;
                end;
              if not(i > 3) then
                begin
                  self._throw(YAPI_INVALID_ARGUMENT,'MODBUS error: illegal data value');
                  result:=res;
                  exit;
                end;
              if not(i > 4) then
                begin
                  self._throw(YAPI_INVALID_ARGUMENT,'MODBUS error: failed to execute function');
                  result:=res;
                  exit;
                end;
            end;
        end;
      result := res;
      exit;
    end;


  function TYSerialPort.modbusReadBits(slaveNo: LongInt; pduAddr: LongInt; nBits: LongInt):TLongIntArray;
    var
      pdu : TLongIntArray;
      reply : TLongIntArray;
      res : TLongIntArray;
      bitpos : LongInt;
      idx : LongInt;
      val : LongInt;
      mask : LongInt;
      pdu_pos : LongInt;
      res_pos : LongInt;
    begin
      pdu_pos := length(pdu);
      SetLength(pdu, pdu_pos+5);;
      pdu[pdu_pos] := $001;
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := ((nBits) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((nBits) and ($0ff));
      inc(pdu_pos);
      SetLength(pdu, pdu_pos);;

      reply := self.queryMODBUS(slaveNo, pdu);
      if length(reply) = 0 then
        begin
          result := res;
          exit;
        end;
      if reply[0] <> pdu[0] then
        begin
          result := res;
          exit;
        end;
      res_pos := length(res);
      SetLength(res, res_pos+nBits);;
      bitpos := 0;
      idx := 2;
      val := reply[idx];
      mask := 1;
      while bitpos < nBits do
        begin
          if ((val) and (mask)) = 0 then
            begin
              res[res_pos] := 0;
              inc(res_pos);
            end
          else
            begin
              res[res_pos] := 1;
              inc(res_pos);
            end;
          bitpos := bitpos + 1;
          if mask = $080 then
            begin
              idx := idx + 1;
              val := reply[idx];
              mask := 1;
            end
          else
            begin
              mask := ((mask) shl 1);
            end;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSerialPort.modbusReadInputBits(slaveNo: LongInt; pduAddr: LongInt; nBits: LongInt):TLongIntArray;
    var
      pdu : TLongIntArray;
      reply : TLongIntArray;
      res : TLongIntArray;
      bitpos : LongInt;
      idx : LongInt;
      val : LongInt;
      mask : LongInt;
      pdu_pos : LongInt;
      res_pos : LongInt;
    begin
      pdu_pos := length(pdu);
      SetLength(pdu, pdu_pos+5);;
      pdu[pdu_pos] := $002;
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := ((nBits) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((nBits) and ($0ff));
      inc(pdu_pos);
      SetLength(pdu, pdu_pos);;

      reply := self.queryMODBUS(slaveNo, pdu);
      if length(reply) = 0 then
        begin
          result := res;
          exit;
        end;
      if reply[0] <> pdu[0] then
        begin
          result := res;
          exit;
        end;
      res_pos := length(res);
      SetLength(res, res_pos+nBits);;
      bitpos := 0;
      idx := 2;
      val := reply[idx];
      mask := 1;
      while bitpos < nBits do
        begin
          if ((val) and (mask)) = 0 then
            begin
              res[res_pos] := 0;
              inc(res_pos);
            end
          else
            begin
              res[res_pos] := 1;
              inc(res_pos);
            end;
          bitpos := bitpos + 1;
          if mask = $080 then
            begin
              idx := idx + 1;
              val := reply[idx];
              mask := 1;
            end
          else
            begin
              mask := ((mask) shl 1);
            end;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSerialPort.modbusReadRegisters(slaveNo: LongInt; pduAddr: LongInt; nWords: LongInt):TLongIntArray;
    var
      pdu : TLongIntArray;
      reply : TLongIntArray;
      res : TLongIntArray;
      regpos : LongInt;
      idx : LongInt;
      val : LongInt;
      pdu_pos : LongInt;
      res_pos : LongInt;
    begin
      if not(nWords<=256) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'Cannot read more than 256 words');
          result:=res;
          exit;
        end;
      pdu_pos := length(pdu);
      SetLength(pdu, pdu_pos+5);;
      pdu[pdu_pos] := $003;
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := ((nWords) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((nWords) and ($0ff));
      inc(pdu_pos);
      SetLength(pdu, pdu_pos);;

      reply := self.queryMODBUS(slaveNo, pdu);
      if length(reply) = 0 then
        begin
          result := res;
          exit;
        end;
      if reply[0] <> pdu[0] then
        begin
          result := res;
          exit;
        end;
      res_pos := length(res);
      SetLength(res, res_pos+nWords);;
      regpos := 0;
      idx := 2;
      while regpos < nWords do
        begin
          val := ((reply[idx]) shl 8);
          idx := idx + 1;
          val := val + reply[idx];
          idx := idx + 1;
          res[res_pos] := val;
          inc(res_pos);
          regpos := regpos + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSerialPort.modbusReadInputRegisters(slaveNo: LongInt; pduAddr: LongInt; nWords: LongInt):TLongIntArray;
    var
      pdu : TLongIntArray;
      reply : TLongIntArray;
      res : TLongIntArray;
      regpos : LongInt;
      idx : LongInt;
      val : LongInt;
      pdu_pos : LongInt;
      res_pos : LongInt;
    begin
      pdu_pos := length(pdu);
      SetLength(pdu, pdu_pos+5);;
      pdu[pdu_pos] := $004;
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := ((nWords) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((nWords) and ($0ff));
      inc(pdu_pos);
      SetLength(pdu, pdu_pos);;

      reply := self.queryMODBUS(slaveNo, pdu);
      if length(reply) = 0 then
        begin
          result := res;
          exit;
        end;
      if reply[0] <> pdu[0] then
        begin
          result := res;
          exit;
        end;
      res_pos := length(res);
      SetLength(res, res_pos+nWords);;
      regpos := 0;
      idx := 2;
      while regpos < nWords do
        begin
          val := ((reply[idx]) shl 8);
          idx := idx + 1;
          val := val + reply[idx];
          idx := idx + 1;
          res[res_pos] := val;
          inc(res_pos);
          regpos := regpos + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSerialPort.modbusWriteBit(slaveNo: LongInt; pduAddr: LongInt; value: LongInt):LongInt;
    var
      pdu : TLongIntArray;
      reply : TLongIntArray;
      res : LongInt;
      pdu_pos : LongInt;
    begin
      res := 0;
      if value <> 0 then
        begin
          value := $0ff;
        end;
      pdu_pos := length(pdu);
      SetLength(pdu, pdu_pos+5);;
      pdu[pdu_pos] := $005;
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := value;
      inc(pdu_pos);
      pdu[pdu_pos] := $000;
      inc(pdu_pos);
      SetLength(pdu, pdu_pos);;

      reply := self.queryMODBUS(slaveNo, pdu);
      if length(reply) = 0 then
        begin
          result := res;
          exit;
        end;
      if reply[0] <> pdu[0] then
        begin
          result := res;
          exit;
        end;
      res := 1;
      result := res;
      exit;
    end;


  function TYSerialPort.modbusWriteBits(slaveNo: LongInt; pduAddr: LongInt; bits: TLongIntArray):LongInt;
    var
      nBits : LongInt;
      nBytes : LongInt;
      bitpos : LongInt;
      val : LongInt;
      mask : LongInt;
      pdu : TLongIntArray;
      reply : TLongIntArray;
      res : LongInt;
      pdu_pos : LongInt;
    begin
      res := 0;
      nBits := length(bits);
      nBytes := (((nBits + 7)) shr 3);
      pdu_pos := length(pdu);
      SetLength(pdu, pdu_pos+6 + nBytes);;
      pdu[pdu_pos] := $00f;
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := ((nBits) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((nBits) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := nBytes;
      inc(pdu_pos);
      bitpos := 0;
      val := 0;
      mask := 1;
      while bitpos < nBits do
        begin
          if bits[bitpos] <> 0 then
            begin
              val := ((val) or (mask));
            end;
          bitpos := bitpos + 1;
          if mask = $080 then
            begin
              pdu[pdu_pos] := val;
              inc(pdu_pos);
              val := 0;
              mask := 1;
            end
          else
            begin
              mask := ((mask) shl 1);
            end;
        end;
      if mask <> 1 then
        begin
          pdu[pdu_pos] := val;
          inc(pdu_pos);
        end;
      SetLength(pdu, pdu_pos);;

      reply := self.queryMODBUS(slaveNo, pdu);
      if length(reply) = 0 then
        begin
          result := res;
          exit;
        end;
      if reply[0] <> pdu[0] then
        begin
          result := res;
          exit;
        end;
      res := ((reply[3]) shl 8);
      res := res + reply[4];
      result := res;
      exit;
    end;


  function TYSerialPort.modbusWriteRegister(slaveNo: LongInt; pduAddr: LongInt; value: LongInt):LongInt;
    var
      pdu : TLongIntArray;
      reply : TLongIntArray;
      res : LongInt;
      pdu_pos : LongInt;
    begin
      res := 0;
      pdu_pos := length(pdu);
      SetLength(pdu, pdu_pos+5);;
      pdu[pdu_pos] := $006;
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := ((value) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((value) and ($0ff));
      inc(pdu_pos);
      SetLength(pdu, pdu_pos);;

      reply := self.queryMODBUS(slaveNo, pdu);
      if length(reply) = 0 then
        begin
          result := res;
          exit;
        end;
      if reply[0] <> pdu[0] then
        begin
          result := res;
          exit;
        end;
      res := 1;
      result := res;
      exit;
    end;


  function TYSerialPort.modbusWriteRegisters(slaveNo: LongInt; pduAddr: LongInt; values: TLongIntArray):LongInt;
    var
      nWords : LongInt;
      nBytes : LongInt;
      regpos : LongInt;
      val : LongInt;
      pdu : TLongIntArray;
      reply : TLongIntArray;
      res : LongInt;
      pdu_pos : LongInt;
    begin
      res := 0;
      nWords := length(values);
      nBytes := 2 * nWords;
      pdu_pos := length(pdu);
      SetLength(pdu, pdu_pos+6 + nBytes);;
      pdu[pdu_pos] := $010;
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduAddr) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := ((nWords) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((nWords) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := nBytes;
      inc(pdu_pos);
      regpos := 0;
      while regpos < nWords do
        begin
          val := values[regpos];
          pdu[pdu_pos] := ((val) shr 8);
          inc(pdu_pos);
          pdu[pdu_pos] := ((val) and ($0ff));
          inc(pdu_pos);
          regpos := regpos + 1;
        end;
      SetLength(pdu, pdu_pos);;

      reply := self.queryMODBUS(slaveNo, pdu);
      if length(reply) = 0 then
        begin
          result := res;
          exit;
        end;
      if reply[0] <> pdu[0] then
        begin
          result := res;
          exit;
        end;
      res := ((reply[3]) shl 8);
      res := res + reply[4];
      result := res;
      exit;
    end;


  function TYSerialPort.modbusWriteAndReadRegisters(slaveNo: LongInt; pduWriteAddr: LongInt; values: TLongIntArray; pduReadAddr: LongInt; nReadWords: LongInt):TLongIntArray;
    var
      nWriteWords : LongInt;
      nBytes : LongInt;
      regpos : LongInt;
      val : LongInt;
      idx : LongInt;
      pdu : TLongIntArray;
      reply : TLongIntArray;
      res : TLongIntArray;
      pdu_pos : LongInt;
      res_pos : LongInt;
    begin
      nWriteWords := length(values);
      nBytes := 2 * nWriteWords;
      pdu_pos := length(pdu);
      SetLength(pdu, pdu_pos+10 + nBytes);;
      pdu[pdu_pos] := $017;
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduReadAddr) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduReadAddr) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := ((nReadWords) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((nReadWords) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduWriteAddr) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((pduWriteAddr) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := ((nWriteWords) shr 8);
      inc(pdu_pos);
      pdu[pdu_pos] := ((nWriteWords) and ($0ff));
      inc(pdu_pos);
      pdu[pdu_pos] := nBytes;
      inc(pdu_pos);
      regpos := 0;
      while regpos < nWriteWords do
        begin
          val := values[regpos];
          pdu[pdu_pos] := ((val) shr 8);
          inc(pdu_pos);
          pdu[pdu_pos] := ((val) and ($0ff));
          inc(pdu_pos);
          regpos := regpos + 1;
        end;
      SetLength(pdu, pdu_pos);;

      reply := self.queryMODBUS(slaveNo, pdu);
      if length(reply) = 0 then
        begin
          result := res;
          exit;
        end;
      if reply[0] <> pdu[0] then
        begin
          result := res;
          exit;
        end;
      res_pos := length(res);
      SetLength(res, res_pos+nReadWords);;
      regpos := 0;
      idx := 2;
      while regpos < nReadWords do
        begin
          val := ((reply[idx]) shl 8);
          idx := idx + 1;
          val := val + reply[idx];
          idx := idx + 1;
          res[res_pos] := val;
          inc(res_pos);
          regpos := regpos + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSerialPort.nextSerialPort(): TYSerialPort;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextSerialPort := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextSerialPort := nil;
          exit;
        end;
      nextSerialPort := TYSerialPort.FindSerialPort(hwid);
    end;

  class function TYSerialPort.FirstSerialPort(): TYSerialPort;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('SerialPort', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYSerialPort.FindSerialPort(serial+'.'+funcId);
    end;

Procedure yInternalEventCallback(obj:TYSerialPort; value:string);
begin
    obj._internalEventHandler(value);
end;

//--- (end of generated code: YSerialPort implementation)

//--- (generated code: YSerialPort functions)

  function yFindSerialPort(func:string): TYSerialPort;
    begin
      result := TYSerialPort.FindSerialPort(func);
    end;

  function yFirstSerialPort(): TYSerialPort;
    begin
      result := TYSerialPort.FirstSerialPort();
    end;

  procedure _SerialPortCleanup();
    begin
    end;

//--- (end of generated code: YSerialPort functions)





//--- (generated code: YSnoopingRecord implementation)

  function TYSnoopingRecord.get_time():LongInt;
    begin
      result := self._tim;
      exit;
    end;


  function TYSnoopingRecord.get_pos():LongInt;
    begin
      result := self._pos;
      exit;
    end;


  function TYSnoopingRecord.get_direction():LongInt;
    begin
      result := self._dir;
      exit;
    end;


  function TYSnoopingRecord.get_message():string;
    begin
      result := self._msg;
      exit;
    end;


//--- (end of generated code: YSnoopingRecord implementation)


constructor TYSnoopingRecord.create(data:string);
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

//--- (generated code: YSnoopingRecord functions)

  procedure _SnoopingRecordCleanup();
    begin
    end;

//--- (end of generated code: YSnoopingRecord functions)

procedure freeSnoopingRecordArray(var list:TYSnoopingRecordARRAY);
 var i:integer;
 begin
  for i:=0 to length(list)-1 do
   list[i].free();
  setLength(list,0);
 end;

initialization
  //--- (generated code: YSerialPort initialization)
  //--- (end of generated code: YSerialPort initialization)
  //--- (generated code: YSnoopingRecord initialization)
  //--- (end of generated code: YSnoopingRecord initialization)

finalization
  //--- (generated code: YSerialPort cleanup)
  _SerialPortCleanup();
  //--- (end of generated code: YSerialPort cleanup)
  //--- (generated code: YSnoopingRecord cleanup)
  //--- (end of generated code: YSnoopingRecord cleanup)
end.
