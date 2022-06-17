{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindInputChain(), the high-level API for InputChain functions
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


unit yocto_inputchain;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YInputChain definitions)

const Y_EXPECTEDNODES_INVALID         = YAPI_INVALID_UINT;
const Y_DETECTEDNODES_INVALID         = YAPI_INVALID_UINT;
const Y_LOOPBACKTEST_OFF = 0;
const Y_LOOPBACKTEST_ON = 1;
const Y_LOOPBACKTEST_INVALID = -1;
const Y_REFRESHRATE_INVALID           = YAPI_INVALID_UINT;
const Y_BITCHAIN1_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN2_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN3_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN4_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN5_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN6_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN7_INVALID             = YAPI_INVALID_STRING;
const Y_WATCHDOGPERIOD_INVALID        = YAPI_INVALID_UINT;
const Y_CHAINDIAGS_INVALID            = YAPI_INVALID_UINT;


//--- (end of YInputChain definitions)
//--- (YInputChain yapiwrapper declaration)
//--- (end of YInputChain yapiwrapper declaration)

type
  TYInputChain = class;
  //--- (YInputChain class start)
  TYInputChainValueCallback = procedure(func: TYInputChain; value:string);
  TYInputChainTimedReportCallback = procedure(func: TYInputChain; value:TYMeasure);
  TYEventCallback = procedure(func: TYInputChain; stamp:integer; evtType:string; evtData:string; evtChange:string);

  ////
  /// <summary>
  ///   TYInputChain Class: InputChain function interface
  /// <para>
  ///   The <c>YInputChain</c> class provides access to separate
  ///   digital inputs connected in a chain.
  /// </para>
  /// </summary>
  ///-
  TYInputChain=class(TYFunction)
  //--- (end of YInputChain class start)
  protected
  //--- (YInputChain declaration)
    // Attributes (function value cache)
    _expectedNodes            : LongInt;
    _detectedNodes            : LongInt;
    _loopbackTest             : Integer;
    _refreshRate              : LongInt;
    _bitChain1                : string;
    _bitChain2                : string;
    _bitChain3                : string;
    _bitChain4                : string;
    _bitChain5                : string;
    _bitChain6                : string;
    _bitChain7                : string;
    _watchdogPeriod           : LongInt;
    _chainDiags               : LongInt;
    _valueCallbackInputChain  : TYInputChainValueCallback;
    _eventCallback            : TYEventCallback;
    _prevPos                  : LongInt;
    _eventPos                 : LongInt;
    _eventStamp               : LongInt;
    _eventChains              : TStringArray;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YInputChain declaration)

  public
    //--- (YInputChain accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the number of nodes expected in the chain.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of nodes expected in the chain
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.EXPECTEDNODES_INVALID</c>.
    /// </para>
    ///-
    function get_expectedNodes():LongInt;

    ////
    /// <summary>
    ///   Changes the number of nodes expected in the chain.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the number of nodes expected in the chain
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
    function set_expectedNodes(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the number of nodes detected in the chain.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of nodes detected in the chain
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.DETECTEDNODES_INVALID</c>.
    /// </para>
    ///-
    function get_detectedNodes():LongInt;

    ////
    /// <summary>
    ///   Returns the activation state of the exhaustive chain connectivity test.
    /// <para>
    ///   The connectivity test requires a cable connecting the end of the chain
    ///   to the loopback test connector.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YInputChain.LOOPBACKTEST_OFF</c> or <c>YInputChain.LOOPBACKTEST_ON</c>, according to the
    ///   activation state of the exhaustive chain connectivity test
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.LOOPBACKTEST_INVALID</c>.
    /// </para>
    ///-
    function get_loopbackTest():Integer;

    ////
    /// <summary>
    ///   Changes the activation state of the exhaustive chain connectivity test.
    /// <para>
    ///   The connectivity test requires a cable connecting the end of the chain
    ///   to the loopback test connector.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YInputChain.LOOPBACKTEST_OFF</c> or <c>YInputChain.LOOPBACKTEST_ON</c>, according to the
    ///   activation state of the exhaustive chain connectivity test
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
    function set_loopbackTest(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the desired refresh rate, measured in Hz.
    /// <para>
    ///   The higher the refresh rate is set, the higher the
    ///   communication speed on the chain will be.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the desired refresh rate, measured in Hz
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.REFRESHRATE_INVALID</c>.
    /// </para>
    ///-
    function get_refreshRate():LongInt;

    ////
    /// <summary>
    ///   Changes the desired refresh rate, measured in Hz.
    /// <para>
    ///   The higher the refresh rate is set, the higher the
    ///   communication speed on the chain will be.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the desired refresh rate, measured in Hz
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
    function set_refreshRate(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the state of input 1 for all nodes of the input chain,
    ///   as a hexadecimal string.
    /// <para>
    ///   The node nearest to the controller
    ///   is the lowest bit of the result.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the state of input 1 for all nodes of the input chain,
    ///   as a hexadecimal string
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.BITCHAIN1_INVALID</c>.
    /// </para>
    ///-
    function get_bitChain1():string;

    ////
    /// <summary>
    ///   Returns the state of input 2 for all nodes of the input chain,
    ///   as a hexadecimal string.
    /// <para>
    ///   The node nearest to the controller
    ///   is the lowest bit of the result.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the state of input 2 for all nodes of the input chain,
    ///   as a hexadecimal string
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.BITCHAIN2_INVALID</c>.
    /// </para>
    ///-
    function get_bitChain2():string;

    ////
    /// <summary>
    ///   Returns the state of input 3 for all nodes of the input chain,
    ///   as a hexadecimal string.
    /// <para>
    ///   The node nearest to the controller
    ///   is the lowest bit of the result.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the state of input 3 for all nodes of the input chain,
    ///   as a hexadecimal string
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.BITCHAIN3_INVALID</c>.
    /// </para>
    ///-
    function get_bitChain3():string;

    ////
    /// <summary>
    ///   Returns the state of input 4 for all nodes of the input chain,
    ///   as a hexadecimal string.
    /// <para>
    ///   The node nearest to the controller
    ///   is the lowest bit of the result.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the state of input 4 for all nodes of the input chain,
    ///   as a hexadecimal string
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.BITCHAIN4_INVALID</c>.
    /// </para>
    ///-
    function get_bitChain4():string;

    ////
    /// <summary>
    ///   Returns the state of input 5 for all nodes of the input chain,
    ///   as a hexadecimal string.
    /// <para>
    ///   The node nearest to the controller
    ///   is the lowest bit of the result.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the state of input 5 for all nodes of the input chain,
    ///   as a hexadecimal string
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.BITCHAIN5_INVALID</c>.
    /// </para>
    ///-
    function get_bitChain5():string;

    ////
    /// <summary>
    ///   Returns the state of input 6 for all nodes of the input chain,
    ///   as a hexadecimal string.
    /// <para>
    ///   The node nearest to the controller
    ///   is the lowest bit of the result.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the state of input 6 for all nodes of the input chain,
    ///   as a hexadecimal string
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.BITCHAIN6_INVALID</c>.
    /// </para>
    ///-
    function get_bitChain6():string;

    ////
    /// <summary>
    ///   Returns the state of input 7 for all nodes of the input chain,
    ///   as a hexadecimal string.
    /// <para>
    ///   The node nearest to the controller
    ///   is the lowest bit of the result.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the state of input 7 for all nodes of the input chain,
    ///   as a hexadecimal string
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.BITCHAIN7_INVALID</c>.
    /// </para>
    ///-
    function get_bitChain7():string;

    ////
    /// <summary>
    ///   Returns the wait time in seconds before triggering an inactivity
    ///   timeout error.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the wait time in seconds before triggering an inactivity
    ///   timeout error
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.WATCHDOGPERIOD_INVALID</c>.
    /// </para>
    ///-
    function get_watchdogPeriod():LongInt;

    ////
    /// <summary>
    ///   Changes the wait time in seconds before triggering an inactivity
    ///   timeout error.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method
    ///   of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the wait time in seconds before triggering an inactivity
    ///   timeout error
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
    function set_watchdogPeriod(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the controller state diagnostics.
    /// <para>
    ///   Bit 0 indicates a chain length
    ///   error, bit 1 indicates an inactivity timeout and bit 2 indicates
    ///   a loopback test failure.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the controller state diagnostics
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.CHAINDIAGS_INVALID</c>.
    /// </para>
    ///-
    function get_chainDiags():LongInt;

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
    ///   Use the method <c>YInputChain.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YInputChain</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindInputChain(func: string):TYInputChain;

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
    function registerValueCallback(callback: TYInputChainValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Resets the application watchdog countdown.
    /// <para>
    ///   If you have setup a non-zero <c>watchdogPeriod</c>, you should
    ///   call this function on a regular basis to prevent the application
    ///   inactivity error to be triggered.
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
    function resetWatchdog():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a string with last events observed on the digital input chain.
    /// <para>
    ///   This method return only events that are still buffered in the device memory.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with last events observed (one per line).
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns  <c>YAPI_INVALID_STRING</c>.
    /// </para>
    ///-
    function get_lastEvents():string; overload; virtual;

    ////
    /// <summary>
    ///   Registers a callback function to be called each time that an event is detected on the
    ///   i
    /// <para>
    ///   nput chain.The callback is invoked only during the execution of
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
    ///   the <c>YInputChain</c> object that emitted the event, the
    ///   UTC timestamp of the event, a character string describing
    ///   the type of event and a character string with the event data.
    ///   On failure, throws an exception or returns a negative error code.
    /// </param>
    ///-
    function registerEventCallback(callback: TYEventCallback):LongInt; overload; virtual;

    function _internalEventHandler(cbpos: string):LongInt; overload; virtual;

    function _strXor(a: string; b: string):string; overload; virtual;

    function hex2array(hexstr: string):TLongIntArray; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of digital input chains started using <c>yFirstInputChain()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned digital input chains order.
    ///   If you want to find a specific a digital input chain, use <c>InputChain.findInputChain()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YInputChain</c> object, corresponding to
    ///   a digital input chain currently online, or a <c>NIL</c> pointer
    ///   if there are no more digital input chains to enumerate.
    /// </returns>
    ///-
    function nextInputChain():TYInputChain;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstInputChain():TYInputChain;
  //--- (end of YInputChain accessors declaration)
  end;

//--- (YInputChain functions declaration)
  ////
  /// <summary>
  ///   Retrieves a digital input chain for a given identifier.
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
  ///   This function does not require that the digital input chain is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YInputChain.isOnline()</c> to test if the digital input chain is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a digital input chain by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the digital input chain, for instance
  ///   <c>MyDevice.inputChain</c>.
  /// </param>
  /// <returns>
  ///   a <c>YInputChain</c> object allowing you to drive the digital input chain.
  /// </returns>
  ///-
  function yFindInputChain(func:string):TYInputChain;
  ////
  /// <summary>
  ///   Starts the enumeration of digital input chains currently accessible.
  /// <para>
  ///   Use the method <c>YInputChain.nextInputChain()</c> to iterate on
  ///   next digital input chains.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YInputChain</c> object, corresponding to
  ///   the first digital input chain currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstInputChain():TYInputChain;

Procedure yInternalEventCallback(obj:TYInputChain; value:string);

//--- (end of YInputChain functions declaration)

implementation
//--- (YInputChain dlldef)
//--- (end of YInputChain dlldef)

  constructor TYInputChain.Create(func:string);
    begin
      inherited Create(func);
      _className := 'InputChain';
      //--- (YInputChain accessors initialization)
      _expectedNodes := Y_EXPECTEDNODES_INVALID;
      _detectedNodes := Y_DETECTEDNODES_INVALID;
      _loopbackTest := Y_LOOPBACKTEST_INVALID;
      _refreshRate := Y_REFRESHRATE_INVALID;
      _bitChain1 := Y_BITCHAIN1_INVALID;
      _bitChain2 := Y_BITCHAIN2_INVALID;
      _bitChain3 := Y_BITCHAIN3_INVALID;
      _bitChain4 := Y_BITCHAIN4_INVALID;
      _bitChain5 := Y_BITCHAIN5_INVALID;
      _bitChain6 := Y_BITCHAIN6_INVALID;
      _bitChain7 := Y_BITCHAIN7_INVALID;
      _watchdogPeriod := Y_WATCHDOGPERIOD_INVALID;
      _chainDiags := Y_CHAINDIAGS_INVALID;
      _valueCallbackInputChain := nil;
      _prevPos := 0;
      _eventPos := 0;
      _eventStamp := 0;
      //--- (end of YInputChain accessors initialization)
    end;

//--- (YInputChain yapiwrapper)
//--- (end of YInputChain yapiwrapper)

//--- (YInputChain implementation)
{$HINTS OFF}
  function TYInputChain._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'expectedNodes') then
        begin
          _expectedNodes := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'detectedNodes') then
        begin
          _detectedNodes := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'loopbackTest') then
        begin
          _loopbackTest := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'refreshRate') then
        begin
          _refreshRate := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain1') then
        begin
          _bitChain1 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain2') then
        begin
          _bitChain2 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain3') then
        begin
          _bitChain3 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain4') then
        begin
          _bitChain4 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain5') then
        begin
          _bitChain5 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain6') then
        begin
          _bitChain6 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain7') then
        begin
          _bitChain7 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'watchdogPeriod') then
        begin
          _watchdogPeriod := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'chainDiags') then
        begin
          _chainDiags := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYInputChain.get_expectedNodes():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_EXPECTEDNODES_INVALID;
              exit;
            end;
        end;
      res := self._expectedNodes;
      result := res;
      exit;
    end;


  function TYInputChain.set_expectedNodes(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('expectedNodes',rest_val);
    end;

  function TYInputChain.get_detectedNodes():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DETECTEDNODES_INVALID;
              exit;
            end;
        end;
      res := self._detectedNodes;
      result := res;
      exit;
    end;


  function TYInputChain.get_loopbackTest():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LOOPBACKTEST_INVALID;
              exit;
            end;
        end;
      res := self._loopbackTest;
      result := res;
      exit;
    end;


  function TYInputChain.set_loopbackTest(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('loopbackTest',rest_val);
    end;

  function TYInputChain.get_refreshRate():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_REFRESHRATE_INVALID;
              exit;
            end;
        end;
      res := self._refreshRate;
      result := res;
      exit;
    end;


  function TYInputChain.set_refreshRate(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('refreshRate',rest_val);
    end;

  function TYInputChain.get_bitChain1():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN1_INVALID;
              exit;
            end;
        end;
      res := self._bitChain1;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain2():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN2_INVALID;
              exit;
            end;
        end;
      res := self._bitChain2;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain3():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN3_INVALID;
              exit;
            end;
        end;
      res := self._bitChain3;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain4():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN4_INVALID;
              exit;
            end;
        end;
      res := self._bitChain4;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain5():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN5_INVALID;
              exit;
            end;
        end;
      res := self._bitChain5;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain6():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN6_INVALID;
              exit;
            end;
        end;
      res := self._bitChain6;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain7():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN7_INVALID;
              exit;
            end;
        end;
      res := self._bitChain7;
      result := res;
      exit;
    end;


  function TYInputChain.get_watchdogPeriod():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_WATCHDOGPERIOD_INVALID;
              exit;
            end;
        end;
      res := self._watchdogPeriod;
      result := res;
      exit;
    end;


  function TYInputChain.set_watchdogPeriod(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('watchdogPeriod',rest_val);
    end;

  function TYInputChain.get_chainDiags():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CHAINDIAGS_INVALID;
              exit;
            end;
        end;
      res := self._chainDiags;
      result := res;
      exit;
    end;


  class function TYInputChain.FindInputChain(func: string):TYInputChain;
    var
      obj : TYInputChain;
    begin
      obj := TYInputChain(TYFunction._FindFromCache('InputChain', func));
      if obj = nil then
        begin
          obj :=  TYInputChain.create(func);
          TYFunction._AddToCache('InputChain',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYInputChain.registerValueCallback(callback: TYInputChainValueCallback):LongInt;
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
      self._valueCallbackInputChain := callback;
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


  function TYInputChain._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackInputChain) <> nil) then
        begin
          self._valueCallbackInputChain(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYInputChain.resetWatchdog():LongInt;
    begin
      result := self.set_watchdogPeriod(-1);
      exit;
    end;


  function TYInputChain.get_lastEvents():string;
    var
      content : TByteArray;
    begin
      content := self._download('events.txt');
      result := _ByteToString(content);
      exit;
    end;


  function TYInputChain.registerEventCallback(callback: TYEventCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          self.registerValueCallback(yInternalEventCallback);
        end
      else
        begin
          self.registerValueCallback(TYInputChainValueCallback(nil));
        end;
      // register user callback AFTER the internal pseudo-event,
      // to make sure we start with future events only
      self._eventCallback := callback;
      result := 0;
      exit;
    end;


  function TYInputChain._internalEventHandler(cbpos: string):LongInt;
    var
      newPos : LongInt;
      url : string;
      content : TByteArray;
      contentStr : string;
      eventArr : TStringArray;
      arrLen : LongInt;
      lenStr : string;
      arrPos : LongInt;
      eventStr : string;
      eventLen : LongInt;
      hexStamp : string;
      typePos : LongInt;
      dataPos : LongInt;
      evtStamp : LongInt;
      evtType : string;
      evtData : string;
      evtChange : string;
      chainIdx : LongInt;
      eventChains_pos : LongInt;
    begin
      SetLength(eventArr, 0);
      newPos := _atoi(cbpos);
      if newPos < self._prevPos then
        begin
          self._eventPos := 0;
        end;
      self._prevPos := newPos;
      if newPos < self._eventPos then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      if not((addr(self._eventCallback) <> nil)) then
        begin
          // first simulated event, use it to initialize reference values
          self._eventPos := newPos;
          SetLength(self._eventChains, 0);
          eventChains_pos := length(self._eventChains);
          SetLength(self._eventChains, eventChains_pos+7);
          self._eventChains[eventChains_pos] := self.get_bitChain1;
          inc(eventChains_pos);
          self._eventChains[eventChains_pos] := self.get_bitChain2;
          inc(eventChains_pos);
          self._eventChains[eventChains_pos] := self.get_bitChain3;
          inc(eventChains_pos);
          self._eventChains[eventChains_pos] := self.get_bitChain4;
          inc(eventChains_pos);
          self._eventChains[eventChains_pos] := self.get_bitChain5;
          inc(eventChains_pos);
          self._eventChains[eventChains_pos] := self.get_bitChain6;
          inc(eventChains_pos);
          self._eventChains[eventChains_pos] := self.get_bitChain7;
          inc(eventChains_pos);
          SetLength(self._eventChains, eventChains_pos);
          result := YAPI_SUCCESS;
          exit;
        end;
      url := 'events.txt?pos='+inttostr(self._eventPos);

      content := self._download(url);
      contentStr := _ByteToString(content);
      eventArr := _stringSplit(contentStr, #10);
      arrLen := length(eventArr);
      if not(arrLen > 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'fail to download events');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      // last element of array is the new position preceeded by '@'
      arrLen := arrLen - 1;
      lenStr := eventArr[arrLen];
      lenStr := Copy(lenStr,  1 + 1, Length(lenStr)-1);
      // update processed event position pointer
      self._eventPos := _atoi(lenStr);
      // now generate callbacks for each event received
      arrPos := 0;
      while arrPos < arrLen do
        begin
          eventStr := eventArr[arrPos];
          eventLen := Length(eventStr);
          if eventLen >= 1 then
            begin
              hexStamp := Copy(eventStr,  0 + 1, 8);
              evtStamp := StrToInt('$0' + hexStamp);
              typePos := (pos(':', eventStr) - 1)+1;
              if (evtStamp >= self._eventStamp) and(typePos > 8) then
                begin
                  self._eventStamp := evtStamp;
                  dataPos := (pos('=', eventStr) - 1)+1;
                  evtType := Copy(eventStr,  typePos + 1, 1);
                  evtData := '';
                  evtChange := '';
                  if dataPos > 10 then
                    begin
                      evtData := Copy(eventStr,  dataPos + 1, Length(eventStr)-dataPos);
                      if (pos(evtType, '1234567') - 1) >= 0 then
                        begin
                          chainIdx := _atoi(evtType) - 1;
                          evtChange := self._strXor(evtData, self._eventChains[chainIdx]);
                          self._eventChains[ chainIdx] := evtData;
                        end;
                    end;
                  self._eventCallback(self, evtStamp, evtType, evtData, evtChange);
                end;
            end;
          arrPos := arrPos + 1;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYInputChain._strXor(a: string; b: string):string;
    var
      lenA : LongInt;
      lenB : LongInt;
      res : string;
      idx : LongInt;
      digitA : LongInt;
      digitB : LongInt;
    begin
      lenA := Length(a);
      lenB := Length(b);
      if lenA > lenB then
        begin
          res := Copy(a,  0 + 1, lenA-lenB);
          a := Copy(a,  lenA-lenB + 1, lenB);
          lenA := lenB;
        end
      else
        begin
          res := '';
          b := Copy(b,  lenA-lenB + 1, lenA);
        end;
      // scan strings and compare digit by digit
      idx := 0;
      while idx < lenA do
        begin
          digitA := StrToInt('$0' + Copy(a,  idx + 1, 1));
          digitB := StrToInt('$0' + Copy(b,  idx + 1, 1));
          res := ''+ res+''+AnsiLowerCase(inttohex(((digitA) xor (digitB)),1));
          idx := idx + 1;
        end;
      result := res;
      exit;
    end;


  function TYInputChain.hex2array(hexstr: string):TLongIntArray;
    var
      hexlen : LongInt;
      res : TLongIntArray;
      idx : LongInt;
      digit : LongInt;
      res_pos : LongInt;
    begin
      hexlen := Length(hexstr);
      SetLength(res, 0);
      res_pos := length(res);
      SetLength(res, res_pos+4*hexlen);;
      idx := hexlen;
      while idx > 0 do
        begin
          idx := idx - 1;
          digit := StrToInt('$0' + Copy(hexstr,  idx + 1, 1));
          res[res_pos] := ((digit) and 1);
          inc(res_pos);
          res[res_pos] := ((((digit) shr 1)) and 1);
          inc(res_pos);
          res[res_pos] := ((((digit) shr 2)) and 1);
          inc(res_pos);
          res[res_pos] := ((((digit) shr 3)) and 1);
          inc(res_pos);
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYInputChain.nextInputChain(): TYInputChain;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextInputChain := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextInputChain := nil;
          exit;
        end;
      nextInputChain := TYInputChain.FindInputChain(hwid);
    end;

  class function TYInputChain.FirstInputChain(): TYInputChain;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('InputChain', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYInputChain.FindInputChain(serial+'.'+funcId);
    end;

Procedure yInternalEventCallback(obj:TYInputChain; value:string);
begin
    obj._internalEventHandler(value);
end;

//--- (end of YInputChain implementation)

//--- (YInputChain functions)

  function yFindInputChain(func:string): TYInputChain;
    begin
      result := TYInputChain.FindInputChain(func);
    end;

  function yFirstInputChain(): TYInputChain;
    begin
      result := TYInputChain.FirstInputChain();
    end;

  procedure _InputChainCleanup();
    begin
    end;

//--- (end of YInputChain functions)

initialization
  //--- (YInputChain initialization)
  //--- (end of YInputChain initialization)

finalization
  //--- (YInputChain cleanup)
  _InputChainCleanup();
  //--- (end of YInputChain cleanup)
end.
