{*********************************************************************
 *
 * $Id: yocto_digitalio.pas 25275 2016-08-24 13:42:24Z mvuilleu $
 *
 * Implements yFindDigitalIO(), the high-level API for DigitalIO functions
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


unit yocto_digitalio;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YDigitalIO definitions)

const Y_PORTSTATE_INVALID             = YAPI_INVALID_UINT;
const Y_PORTDIRECTION_INVALID         = YAPI_INVALID_UINT;
const Y_PORTOPENDRAIN_INVALID         = YAPI_INVALID_UINT;
const Y_PORTPOLARITY_INVALID          = YAPI_INVALID_UINT;
const Y_PORTSIZE_INVALID              = YAPI_INVALID_UINT;
const Y_OUTPUTVOLTAGE_USB_5V = 0;
const Y_OUTPUTVOLTAGE_USB_3V = 1;
const Y_OUTPUTVOLTAGE_EXT_V = 2;
const Y_OUTPUTVOLTAGE_INVALID = -1;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YDigitalIO definitions)

type
  TYDigitalIO = class;
  //--- (YDigitalIO class start)
  TYDigitalIOValueCallback = procedure(func: TYDigitalIO; value:string);
  TYDigitalIOTimedReportCallback = procedure(func: TYDigitalIO; value:TYMeasure);

  ////
  /// <summary>
  ///   TYDigitalIO Class: Digital IO function interface
  /// <para>
  ///   The Yoctopuce application programming interface allows you to switch the state of each
  ///   bit of the I/O port. You can switch all bits at once, or one by one. The library
  ///   can also automatically generate short pulses of a determined duration. Electrical behavior
  ///   of each I/O can be modified (open drain and reverse polarity).
  /// </para>
  /// </summary>
  ///-
  TYDigitalIO=class(TYFunction)
  //--- (end of YDigitalIO class start)
  protected
  //--- (YDigitalIO declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _portState                : LongInt;
    _portDirection            : LongInt;
    _portOpenDrain            : LongInt;
    _portPolarity             : LongInt;
    _portSize                 : LongInt;
    _outputVoltage            : Integer;
    _command                  : string;
    _valueCallbackDigitalIO   : TYDigitalIOValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YDigitalIO declaration)

  public
    //--- (YDigitalIO accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the digital IO port state: bit 0 represents input 0, and so on.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the digital IO port state: bit 0 represents input 0, and so on
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PORTSTATE_INVALID</c>.
    /// </para>
    ///-
    function get_portState():LongInt;

    ////
    /// <summary>
    ///   Changes the digital IO port state: bit 0 represents input 0, and so on.
    /// <para>
    ///   This function has no effect
    ///   on bits configured as input in <c>portDirection</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the digital IO port state: bit 0 represents input 0, and so on
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
    function set_portState(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the IO direction of all bits of the port: 0 makes a bit an input, 1 makes it an output.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the IO direction of all bits of the port: 0 makes a bit an input, 1
    ///   makes it an output
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PORTDIRECTION_INVALID</c>.
    /// </para>
    ///-
    function get_portDirection():LongInt;

    ////
    /// <summary>
    ///   Changes the IO direction of all bits of the port: 0 makes a bit an input, 1 makes it an output.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method  to make sure the setting is kept after a reboot.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the IO direction of all bits of the port: 0 makes a bit an input, 1
    ///   makes it an output
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
    function set_portDirection(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the electrical interface for each bit of the port.
    /// <para>
    ///   For each bit set to 0  the matching I/O works in the regular,
    ///   intuitive way, for each bit set to 1, the I/O works in reverse mode.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the electrical interface for each bit of the port
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PORTOPENDRAIN_INVALID</c>.
    /// </para>
    ///-
    function get_portOpenDrain():LongInt;

    ////
    /// <summary>
    ///   Changes the electrical interface for each bit of the port.
    /// <para>
    ///   0 makes a bit a regular input/output, 1 makes
    ///   it an open-drain (open-collector) input/output. Remember to call the
    ///   <c>saveToFlash()</c> method  to make sure the setting is kept after a reboot.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the electrical interface for each bit of the port
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
    function set_portOpenDrain(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the polarity of all the bits of the port.
    /// <para>
    ///   For each bit set to 0, the matching I/O works the regular,
    ///   intuitive way; for each bit set to 1, the I/O works in reverse mode.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the polarity of all the bits of the port
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PORTPOLARITY_INVALID</c>.
    /// </para>
    ///-
    function get_portPolarity():LongInt;

    ////
    /// <summary>
    ///   Changes the polarity of all the bits of the port: 0 makes a bit an input, 1 makes it an output.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method  to make sure the setting will be kept after a reboot.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the polarity of all the bits of the port: 0 makes a bit an input, 1
    ///   makes it an output
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
    function set_portPolarity(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the number of bits implemented in the I/O port.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of bits implemented in the I/O port
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PORTSIZE_INVALID</c>.
    /// </para>
    ///-
    function get_portSize():LongInt;

    ////
    /// <summary>
    ///   Returns the voltage source used to drive output bits.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_OUTPUTVOLTAGE_USB_5V</c>, <c>Y_OUTPUTVOLTAGE_USB_3V</c> and
    ///   <c>Y_OUTPUTVOLTAGE_EXT_V</c> corresponding to the voltage source used to drive output bits
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_OUTPUTVOLTAGE_INVALID</c>.
    /// </para>
    ///-
    function get_outputVoltage():Integer;

    ////
    /// <summary>
    ///   Changes the voltage source used to drive output bits.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method  to make sure the setting is kept after a reboot.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_OUTPUTVOLTAGE_USB_5V</c>, <c>Y_OUTPUTVOLTAGE_USB_3V</c> and
    ///   <c>Y_OUTPUTVOLTAGE_EXT_V</c> corresponding to the voltage source used to drive output bits
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
    function set_outputVoltage(newval:Integer):integer;

    function get_command():string;

    function set_command(newval:string):integer;

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
    ///   Use the method <c>YDigitalIO.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YDigitalIO</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindDigitalIO(func: string):TYDigitalIO;

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
    function registerValueCallback(callback: TYDigitalIOValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Sets a single bit of the I/O port.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="bitno">
    ///   the bit number; lowest bit has index 0
    /// </param>
    /// <param name="bitstate">
    ///   the state of the bit (1 or 0)
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_bitState(bitno: LongInt; bitstate: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the state of a single bit of the I/O port.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="bitno">
    ///   the bit number; lowest bit has index 0
    /// </param>
    /// <returns>
    ///   the bit state (0 or 1)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function get_bitState(bitno: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reverts a single bit of the I/O port.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="bitno">
    ///   the bit number; lowest bit has index 0
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function toggle_bitState(bitno: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes  the direction of a single bit from the I/O port.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="bitno">
    ///   the bit number; lowest bit has index 0
    /// </param>
    /// <param name="bitdirection">
    ///   direction to set, 0 makes the bit an input, 1 makes it an output.
    ///   Remember to call the   <c>saveToFlash()</c> method to make sure the setting is kept after a reboot.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_bitDirection(bitno: LongInt; bitdirection: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the direction of a single bit from the I/O port (0 means the bit is an input, 1  an output).
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="bitno">
    ///   the bit number; lowest bit has index 0
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function get_bitDirection(bitno: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes the polarity of a single bit from the I/O port.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="bitno">
    ///   the bit number; lowest bit has index 0.
    /// </param>
    /// <param name="bitpolarity">
    ///   polarity to set, 0 makes the I/O work in regular mode, 1 makes the I/O  works in reverse mode.
    ///   Remember to call the   <c>saveToFlash()</c> method to make sure the setting is kept after a reboot.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_bitPolarity(bitno: LongInt; bitpolarity: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the polarity of a single bit from the I/O port (0 means the I/O works in regular mode, 1 means the I/O  works in reverse mode).
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="bitno">
    ///   the bit number; lowest bit has index 0
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function get_bitPolarity(bitno: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes  the electrical interface of a single bit from the I/O port.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="bitno">
    ///   the bit number; lowest bit has index 0
    /// </param>
    /// <param name="opendrain">
    ///   0 makes a bit a regular input/output, 1 makes
    ///   it an open-drain (open-collector) input/output. Remember to call the
    ///   <c>saveToFlash()</c> method to make sure the setting is kept after a reboot.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_bitOpenDrain(bitno: LongInt; opendrain: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the type of electrical interface of a single bit from the I/O port.
    /// <para>
    ///   (0 means the bit is an input, 1  an output).
    /// </para>
    /// </summary>
    /// <param name="bitno">
    ///   the bit number; lowest bit has index 0
    /// </param>
    /// <returns>
    ///   0 means the a bit is a regular input/output, 1 means the bit is an open-drain
    ///   (open-collector) input/output.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function get_bitOpenDrain(bitno: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Triggers a pulse on a single bit for a specified duration.
    /// <para>
    ///   The specified bit
    ///   will be turned to 1, and then back to 0 after the given duration.
    /// </para>
    /// </summary>
    /// <param name="bitno">
    ///   the bit number; lowest bit has index 0
    /// </param>
    /// <param name="ms_duration">
    ///   desired pulse duration in milliseconds. Be aware that the device time
    ///   resolution is not guaranteed up to the millisecond.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function pulse(bitno: LongInt; ms_duration: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Schedules a pulse on a single bit for a specified duration.
    /// <para>
    ///   The specified bit
    ///   will be turned to 1, and then back to 0 after the given duration.
    /// </para>
    /// </summary>
    /// <param name="bitno">
    ///   the bit number; lowest bit has index 0
    /// </param>
    /// <param name="ms_delay">
    ///   waiting time before the pulse, in milliseconds
    /// </param>
    /// <param name="ms_duration">
    ///   desired pulse duration in milliseconds. Be aware that the device time
    ///   resolution is not guaranteed up to the millisecond.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function delayedPulse(bitno: LongInt; ms_delay: LongInt; ms_duration: LongInt):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of digital IO ports started using <c>yFirstDigitalIO()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YDigitalIO</c> object, corresponding to
    ///   a digital IO port currently online, or a <c>NIL</c> pointer
    ///   if there are no more digital IO ports to enumerate.
    /// </returns>
    ///-
    function nextDigitalIO():TYDigitalIO;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstDigitalIO():TYDigitalIO;
  //--- (end of YDigitalIO accessors declaration)
  end;

//--- (DigitalIO functions declaration)
  ////
  /// <summary>
  ///   Retrieves a digital IO port for a given identifier.
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
  ///   This function does not require that the digital IO port is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YDigitalIO.isOnline()</c> to test if the digital IO port is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a digital IO port by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the digital IO port
  /// </param>
  /// <returns>
  ///   a <c>YDigitalIO</c> object allowing you to drive the digital IO port.
  /// </returns>
  ///-
  function yFindDigitalIO(func:string):TYDigitalIO;
  ////
  /// <summary>
  ///   Starts the enumeration of digital IO ports currently accessible.
  /// <para>
  ///   Use the method <c>YDigitalIO.nextDigitalIO()</c> to iterate on
  ///   next digital IO ports.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YDigitalIO</c> object, corresponding to
  ///   the first digital IO port currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstDigitalIO():TYDigitalIO;

//--- (end of DigitalIO functions declaration)

implementation
//--- (YDigitalIO dlldef)
//--- (end of YDigitalIO dlldef)

  constructor TYDigitalIO.Create(func:string);
    begin
      inherited Create(func);
      _className := 'DigitalIO';
      //--- (YDigitalIO accessors initialization)
      _portState := Y_PORTSTATE_INVALID;
      _portDirection := Y_PORTDIRECTION_INVALID;
      _portOpenDrain := Y_PORTOPENDRAIN_INVALID;
      _portPolarity := Y_PORTPOLARITY_INVALID;
      _portSize := Y_PORTSIZE_INVALID;
      _outputVoltage := Y_OUTPUTVOLTAGE_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackDigitalIO := nil;
      //--- (end of YDigitalIO accessors initialization)
    end;


//--- (YDigitalIO implementation)
{$HINTS OFF}
  function TYDigitalIO._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'portState') then
        begin
          _portState := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'portDirection') then
        begin
          _portDirection := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'portOpenDrain') then
        begin
          _portOpenDrain := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'portPolarity') then
        begin
          _portPolarity := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'portSize') then
        begin
          _portSize := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'outputVoltage') then
        begin
          _outputVoltage := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'command') then
        begin
          _command := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Returns the digital IO port state: bit 0 represents input 0, and so on.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the digital IO port state: bit 0 represents input 0, and so on
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PORTSTATE_INVALID.
  /// </para>
  ///-
  function TYDigitalIO.get_portState():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PORTSTATE_INVALID;
              exit;
            end;
        end;
      result := self._portState;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the digital IO port state: bit 0 represents input 0, and so on.
  /// <para>
  ///   This function has no effect
  ///   on bits configured as input in portDirection.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the digital IO port state: bit 0 represents input 0, and so on
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
  function TYDigitalIO.set_portState(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('portState',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the IO direction of all bits of the port: 0 makes a bit an input, 1 makes it an output.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the IO direction of all bits of the port: 0 makes a bit an input, 1
  ///   makes it an output
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PORTDIRECTION_INVALID.
  /// </para>
  ///-
  function TYDigitalIO.get_portDirection():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PORTDIRECTION_INVALID;
              exit;
            end;
        end;
      result := self._portDirection;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the IO direction of all bits of the port: 0 makes a bit an input, 1 makes it an output.
  /// <para>
  ///   Remember to call the saveToFlash() method  to make sure the setting is kept after a reboot.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the IO direction of all bits of the port: 0 makes a bit an input, 1
  ///   makes it an output
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
  function TYDigitalIO.set_portDirection(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('portDirection',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the electrical interface for each bit of the port.
  /// <para>
  ///   For each bit set to 0  the matching I/O works in the regular,
  ///   intuitive way, for each bit set to 1, the I/O works in reverse mode.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the electrical interface for each bit of the port
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PORTOPENDRAIN_INVALID.
  /// </para>
  ///-
  function TYDigitalIO.get_portOpenDrain():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PORTOPENDRAIN_INVALID;
              exit;
            end;
        end;
      result := self._portOpenDrain;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the electrical interface for each bit of the port.
  /// <para>
  ///   0 makes a bit a regular input/output, 1 makes
  ///   it an open-drain (open-collector) input/output. Remember to call the
  ///   saveToFlash() method  to make sure the setting is kept after a reboot.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the electrical interface for each bit of the port
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
  function TYDigitalIO.set_portOpenDrain(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('portOpenDrain',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the polarity of all the bits of the port.
  /// <para>
  ///   For each bit set to 0, the matching I/O works the regular,
  ///   intuitive way; for each bit set to 1, the I/O works in reverse mode.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the polarity of all the bits of the port
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PORTPOLARITY_INVALID.
  /// </para>
  ///-
  function TYDigitalIO.get_portPolarity():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PORTPOLARITY_INVALID;
              exit;
            end;
        end;
      result := self._portPolarity;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the polarity of all the bits of the port: 0 makes a bit an input, 1 makes it an output.
  /// <para>
  ///   Remember to call the saveToFlash() method  to make sure the setting will be kept after a reboot.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the polarity of all the bits of the port: 0 makes a bit an input, 1
  ///   makes it an output
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
  function TYDigitalIO.set_portPolarity(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('portPolarity',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the number of bits implemented in the I/O port.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the number of bits implemented in the I/O port
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PORTSIZE_INVALID.
  /// </para>
  ///-
  function TYDigitalIO.get_portSize():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PORTSIZE_INVALID;
              exit;
            end;
        end;
      result := self._portSize;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the voltage source used to drive output bits.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_OUTPUTVOLTAGE_USB_5V, Y_OUTPUTVOLTAGE_USB_3V and Y_OUTPUTVOLTAGE_EXT_V
  ///   corresponding to the voltage source used to drive output bits
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_OUTPUTVOLTAGE_INVALID.
  /// </para>
  ///-
  function TYDigitalIO.get_outputVoltage():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_OUTPUTVOLTAGE_INVALID;
              exit;
            end;
        end;
      result := self._outputVoltage;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the voltage source used to drive output bits.
  /// <para>
  ///   Remember to call the saveToFlash() method  to make sure the setting is kept after a reboot.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_OUTPUTVOLTAGE_USB_5V, Y_OUTPUTVOLTAGE_USB_3V and Y_OUTPUTVOLTAGE_EXT_V
  ///   corresponding to the voltage source used to drive output bits
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
  function TYDigitalIO.set_outputVoltage(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('outputVoltage',rest_val);
    end;

  function TYDigitalIO.get_command():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_COMMAND_INVALID;
              exit;
            end;
        end;
      result := self._command;
      exit;
    end;


  function TYDigitalIO.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
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
  ///   Use the method <c>YDigitalIO.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YDigitalIO</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYDigitalIO.FindDigitalIO(func: string):TYDigitalIO;
    var
      obj : TYDigitalIO;
    begin
      obj := TYDigitalIO(TYFunction._FindFromCache('DigitalIO', func));
      if obj = nil then
        begin
          obj :=  TYDigitalIO.create(func);
          TYFunction._AddToCache('DigitalIO',  func, obj);
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
  function TYDigitalIO.registerValueCallback(callback: TYDigitalIOValueCallback):LongInt;
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
      self._valueCallbackDigitalIO := callback;
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


  function TYDigitalIO._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackDigitalIO) <> nil) then
        begin
          self._valueCallbackDigitalIO(self, value);
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
  ///   Sets a single bit of the I/O port.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="bitno">
  ///   the bit number; lowest bit has index 0
  /// </param>
  /// <param name="bitstate">
  ///   the state of the bit (1 or 0)
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDigitalIO.set_bitState(bitno: LongInt; bitstate: LongInt):LongInt;
    begin
      if not(bitstate >= 0) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'invalid bitstate');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if not(bitstate <= 1) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'invalid bitstate');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      result := self.set_command(''+chr(82+bitstate)+''+inttostr(bitno));
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the state of a single bit of the I/O port.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="bitno">
  ///   the bit number; lowest bit has index 0
  /// </param>
  /// <returns>
  ///   the bit state (0 or 1)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDigitalIO.get_bitState(bitno: LongInt):LongInt;
    var
      portVal : LongInt;
    begin
      portVal := self.get_portState;
      result := ((((portVal) shr (bitno))) and 1);
      exit;
    end;


  ////
  /// <summary>
  ///   Reverts a single bit of the I/O port.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="bitno">
  ///   the bit number; lowest bit has index 0
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDigitalIO.toggle_bitState(bitno: LongInt):LongInt;
    begin
      result := self.set_command('T'+inttostr(bitno));
      exit;
    end;


  ////
  /// <summary>
  ///   Changes  the direction of a single bit from the I/O port.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="bitno">
  ///   the bit number; lowest bit has index 0
  /// </param>
  /// <param name="bitdirection">
  ///   direction to set, 0 makes the bit an input, 1 makes it an output.
  ///   Remember to call the   <c>saveToFlash()</c> method to make sure the setting is kept after a reboot.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDigitalIO.set_bitDirection(bitno: LongInt; bitdirection: LongInt):LongInt;
    begin
      if not(bitdirection >= 0) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'invalid direction');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if not(bitdirection <= 1) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'invalid direction');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      result := self.set_command(''+chr(73+6*bitdirection)+''+inttostr(bitno));
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the direction of a single bit from the I/O port (0 means the bit is an input, 1  an output).
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="bitno">
  ///   the bit number; lowest bit has index 0
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDigitalIO.get_bitDirection(bitno: LongInt):LongInt;
    var
      portDir : LongInt;
    begin
      portDir := self.get_portDirection;
      result := ((((portDir) shr (bitno))) and 1);
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the polarity of a single bit from the I/O port.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="bitno">
  ///   the bit number; lowest bit has index 0.
  /// </param>
  /// <param name="bitpolarity">
  ///   polarity to set, 0 makes the I/O work in regular mode, 1 makes the I/O  works in reverse mode.
  ///   Remember to call the   <c>saveToFlash()</c> method to make sure the setting is kept after a reboot.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDigitalIO.set_bitPolarity(bitno: LongInt; bitpolarity: LongInt):LongInt;
    begin
      if not(bitpolarity >= 0) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'invalid bitpolarity');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if not(bitpolarity <= 1) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'invalid bitpolarity');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      result := self.set_command(''+chr(110+4*bitpolarity)+''+inttostr(bitno));
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the polarity of a single bit from the I/O port (0 means the I/O works in regular mode, 1 means the I/O  works in reverse mode).
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="bitno">
  ///   the bit number; lowest bit has index 0
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDigitalIO.get_bitPolarity(bitno: LongInt):LongInt;
    var
      portPol : LongInt;
    begin
      portPol := self.get_portPolarity;
      result := ((((portPol) shr (bitno))) and 1);
      exit;
    end;


  ////
  /// <summary>
  ///   Changes  the electrical interface of a single bit from the I/O port.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="bitno">
  ///   the bit number; lowest bit has index 0
  /// </param>
  /// <param name="opendrain">
  ///   0 makes a bit a regular input/output, 1 makes
  ///   it an open-drain (open-collector) input/output. Remember to call the
  ///   <c>saveToFlash()</c> method to make sure the setting is kept after a reboot.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDigitalIO.set_bitOpenDrain(bitno: LongInt; opendrain: LongInt):LongInt;
    begin
      if not(opendrain >= 0) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'invalid state');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if not(opendrain <= 1) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'invalid state');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      result := self.set_command(''+chr(100-32*opendrain)+''+inttostr(bitno));
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the type of electrical interface of a single bit from the I/O port.
  /// <para>
  ///   (0 means the bit is an input, 1  an output).
  /// </para>
  /// </summary>
  /// <param name="bitno">
  ///   the bit number; lowest bit has index 0
  /// </param>
  /// <returns>
  ///   0 means the a bit is a regular input/output, 1 means the bit is an open-drain
  ///   (open-collector) input/output.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDigitalIO.get_bitOpenDrain(bitno: LongInt):LongInt;
    var
      portOpenDrain : LongInt;
    begin
      portOpenDrain := self.get_portOpenDrain;
      result := ((((portOpenDrain) shr (bitno))) and 1);
      exit;
    end;


  ////
  /// <summary>
  ///   Triggers a pulse on a single bit for a specified duration.
  /// <para>
  ///   The specified bit
  ///   will be turned to 1, and then back to 0 after the given duration.
  /// </para>
  /// </summary>
  /// <param name="bitno">
  ///   the bit number; lowest bit has index 0
  /// </param>
  /// <param name="ms_duration">
  ///   desired pulse duration in milliseconds. Be aware that the device time
  ///   resolution is not guaranteed up to the millisecond.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDigitalIO.pulse(bitno: LongInt; ms_duration: LongInt):LongInt;
    begin
      result := self.set_command('Z'+inttostr( bitno)+',0,'+inttostr(ms_duration));
      exit;
    end;


  ////
  /// <summary>
  ///   Schedules a pulse on a single bit for a specified duration.
  /// <para>
  ///   The specified bit
  ///   will be turned to 1, and then back to 0 after the given duration.
  /// </para>
  /// </summary>
  /// <param name="bitno">
  ///   the bit number; lowest bit has index 0
  /// </param>
  /// <param name="ms_delay">
  ///   waiting time before the pulse, in milliseconds
  /// </param>
  /// <param name="ms_duration">
  ///   desired pulse duration in milliseconds. Be aware that the device time
  ///   resolution is not guaranteed up to the millisecond.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDigitalIO.delayedPulse(bitno: LongInt; ms_delay: LongInt; ms_duration: LongInt):LongInt;
    begin
      result := self.set_command('Z'+inttostr(bitno)+','+inttostr(ms_delay)+','+inttostr(ms_duration));
      exit;
    end;


  function TYDigitalIO.nextDigitalIO(): TYDigitalIO;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextDigitalIO := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextDigitalIO := nil;
          exit;
        end;
      nextDigitalIO := TYDigitalIO.FindDigitalIO(hwid);
    end;

  class function TYDigitalIO.FirstDigitalIO(): TYDigitalIO;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('DigitalIO', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYDigitalIO.FindDigitalIO(serial+'.'+funcId);
    end;

//--- (end of YDigitalIO implementation)

//--- (DigitalIO functions)

  function yFindDigitalIO(func:string): TYDigitalIO;
    begin
      result := TYDigitalIO.FindDigitalIO(func);
    end;

  function yFirstDigitalIO(): TYDigitalIO;
    begin
      result := TYDigitalIO.FirstDigitalIO();
    end;

  procedure _DigitalIOCleanup();
    begin
    end;

//--- (end of DigitalIO functions)

initialization
  //--- (DigitalIO initialization)
  //--- (end of DigitalIO initialization)

finalization
  //--- (DigitalIO cleanup)
  _DigitalIOCleanup();
  //--- (end of DigitalIO cleanup)
end.
