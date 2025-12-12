{*********************************************************************
 *
 *  $Id: yocto_micropython.pas 69442 2025-10-16 08:53:14Z mvuilleu $
 *
 *  Implements yFindMicroPython(), the high-level API for MicroPython functions
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


unit yocto_micropython;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (generated code: YMicroPython definitions)

const Y_LASTMSG_INVALID               = YAPI_INVALID_STRING;
const Y_HEAPUSAGE_INVALID             = YAPI_INVALID_UINT;
const Y_HEAPFRAG_INVALID              = YAPI_INVALID_UINT;
const Y_XHEAPUSAGE_INVALID            = YAPI_INVALID_UINT;
const Y_STACKUSAGE_INVALID            = YAPI_INVALID_UINT;
const Y_CURRENTSCRIPT_INVALID         = YAPI_INVALID_STRING;
const Y_STARTUPSCRIPT_INVALID         = YAPI_INVALID_STRING;
const Y_STARTUPDELAY_INVALID          = YAPI_INVALID_DOUBLE;
const Y_DEBUGMODE_OFF = 0;
const Y_DEBUGMODE_ON = 1;
const Y_DEBUGMODE_INVALID = -1;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;

//--- (end of generated code: YMicroPython definitions)

//--- (generated code: YMicroPython yapiwrapper declaration)
//--- (end of generated code: YMicroPython yapiwrapper declaration)

type

  TYMicroPython = class;
  //--- (generated code: YMicroPython class start)
  TYMicroPythonValueCallback = procedure(func: TYMicroPython; value:string);
  TYMicroPythonTimedReportCallback = procedure(func: TYMicroPython; value:TYMeasure);
  TYMicroPythonLogCallback = procedure(func: TYMicroPython; logline: string);

  ////
  /// <summary>
  ///   TYMicroPython Class: MicroPython interpreter control interface
  /// <para>
  ///   The <c>YMicroPython</c> class provides control of the MicroPython interpreter
  ///   that can be found on some Yoctopuce devices.
  /// </para>
  /// </summary>
  ///-
  TYMicroPython=class(TYFunction)
  //--- (end of generated code: YMicroPython class start)
  protected
  //--- (generated code: YMicroPython declaration)
    // Attributes (function value cache)
    _lastMsg                  : string;
    _heapUsage                : LongInt;
    _heapFrag                 : LongInt;
    _xheapUsage               : LongInt;
    _stackUsage               : LongInt;
    _currentScript            : string;
    _startupScript            : string;
    _startupDelay             : double;
    _debugMode                : Integer;
    _command                  : string;
    _valueCallbackMicroPython : TYMicroPythonValueCallback;
    _logCallback              : TYMicroPythonLogCallback;
    _isFirstCb                : boolean;
    _prevCbPos                : LongInt;
    _logPos                   : LongInt;
    _prevPartialLog           : string;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of generated code: YMicroPython declaration)

  public
    //--- (generated code: YMicroPython accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the last message produced by a python script.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the last message produced by a python script
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMicroPython.LASTMSG_INVALID</c>.
    /// </para>
    ///-
    function get_lastMsg():string;

    ////
    /// <summary>
    ///   Returns the percentage of MicroPython main memory in use,
    ///   as observed at the end of the last garbage collection.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the percentage of MicroPython main memory in use,
    ///   as observed at the end of the last garbage collection
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMicroPython.HEAPUSAGE_INVALID</c>.
    /// </para>
    ///-
    function get_heapUsage():LongInt;

    ////
    /// <summary>
    ///   Returns the fragmentation ratio of MicroPython main memory,
    ///   as observed at the end of the last garbage collection.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the fragmentation ratio of MicroPython main memory,
    ///   as observed at the end of the last garbage collection
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMicroPython.HEAPFRAG_INVALID</c>.
    /// </para>
    ///-
    function get_heapFrag():LongInt;

    ////
    /// <summary>
    ///   Returns the percentage of MicroPython external memory in use,
    ///   as observed at the end of the last garbage collection.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the percentage of MicroPython external memory in use,
    ///   as observed at the end of the last garbage collection
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMicroPython.XHEAPUSAGE_INVALID</c>.
    /// </para>
    ///-
    function get_xheapUsage():LongInt;

    ////
    /// <summary>
    ///   Returns the maximum percentage of MicroPython call stack in use,
    ///   as observed at the end of the last garbage collection.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximum percentage of MicroPython call stack in use,
    ///   as observed at the end of the last garbage collection
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMicroPython.STACKUSAGE_INVALID</c>.
    /// </para>
    ///-
    function get_stackUsage():LongInt;

    ////
    /// <summary>
    ///   Returns the name of currently active script, if any.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the name of currently active script, if any
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMicroPython.CURRENTSCRIPT_INVALID</c>.
    /// </para>
    ///-
    function get_currentScript():string;

    ////
    /// <summary>
    ///   Stops current running script, and/or selects a script to run immediately in a
    ///   fresh new environment.
    /// <para>
    ///   If the MicroPython interpreter is busy running a script,
    ///   this function will abort it immediately and reset the execution environment.
    ///   If a non-empty string is given as argument, the new script will be started.
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
    function set_currentScript(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the name of the script to run when the device is powered on.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the name of the script to run when the device is powered on
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMicroPython.STARTUPSCRIPT_INVALID</c>.
    /// </para>
    ///-
    function get_startupScript():string;

    ////
    /// <summary>
    ///   Changes the script to run when the device is powered on.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the script to run when the device is powered on
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
    function set_startupScript(newval:string):integer;

    ////
    /// <summary>
    ///   Changes the wait time before running the startup script on power on, between 0.
    /// <para>
    ///   1
    ///   second and 25 seconds. Remember to call the <c>saveToFlash()</c> method of the
    ///   module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the wait time before running the startup script on power
    ///   on, between 0.1
    ///   second and 25 seconds
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
    function set_startupDelay(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the wait time before running the startup script on power on,
    ///   measured in seconds.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the wait time before running the startup script on power on,
    ///   measured in seconds
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMicroPython.STARTUPDELAY_INVALID</c>.
    /// </para>
    ///-
    function get_startupDelay():double;

    ////
    /// <summary>
    ///   Returns the activation state of MicroPython debugging interface.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YMicroPython.DEBUGMODE_OFF</c> or <c>YMicroPython.DEBUGMODE_ON</c>, according to the
    ///   activation state of MicroPython debugging interface
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMicroPython.DEBUGMODE_INVALID</c>.
    /// </para>
    ///-
    function get_debugMode():Integer;

    ////
    /// <summary>
    ///   Changes the activation state of MicroPython debugging interface.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YMicroPython.DEBUGMODE_OFF</c> or <c>YMicroPython.DEBUGMODE_ON</c>, according to the
    ///   activation state of MicroPython debugging interface
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
    function set_debugMode(newval:Integer):integer;

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
    ///   Use the method <c>YMicroPython.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YMicroPython</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindMicroPython(func: string):TYMicroPython;

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
    function registerValueCallback(callback: TYMicroPythonValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Submit MicroPython code for execution in the interpreter.
    /// <para>
    ///   If the MicroPython interpreter is busy, this function will
    ///   block until it becomes available. The code is then uploaded,
    ///   compiled and executed on the fly, without beeing stored on the device filesystem.
    /// </para>
    /// <para>
    ///   There is no implicit reset of the MicroPython interpreter with
    ///   this function. Use method <c>reset()</c> if you need to start
    ///   from a fresh environment to run your code.
    /// </para>
    /// <para>
    ///   Note that although MicroPython is mostly compatible with recent Python 3.x
    ///   interpreters, the limited ressources on the device impose some restrictions,
    ///   in particular regarding the libraries that can be used. Please refer to
    ///   the documentation for more details.
    /// </para>
    /// </summary>
    /// <param name="codeName">
    ///   name of the code file (used for error reporting only)
    /// </param>
    /// <param name="mpyCode">
    ///   MicroPython code to compile and execute
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function eval(codeName: string; mpyCode: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops current execution, and reset the MicroPython interpreter to initial state.
    /// <para>
    ///   All global variables are cleared, and all imports are forgotten.
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
    ///   Clears MicroPython interpreter console log buffer.
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
    function clearLogs():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a string with last logs of the MicroPython interpreter.
    /// <para>
    ///   This method return only logs that are still in the module.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with last MicroPython logs.
    ///   On failure, throws an exception or returns  <c>YAPI_INVALID_STRING</c>.
    /// </returns>
    ///-
    function get_lastLogs():string; overload; virtual;

    ////
    /// <summary>
    ///   Registers a device log callback function.
    /// <para>
    ///   This callback will be called each time
    ///   microPython sends a new log message.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="callback">
    ///   the callback function to invoke, or a NIL pointer.
    ///   The callback function should take two arguments:
    ///   the module object that emitted the log message,
    ///   and the character string containing the log.
    ///   On failure, throws an exception or returns a negative error code.
    /// </param>
    ///-
    function registerLogCallback(callback: TYMicroPythonLogCallback):LongInt; overload; virtual;

    function get_logCallback():TYMicroPythonLogCallback; overload; virtual;

    function _internalEventHandler(cbVal: string):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of MicroPython interpreters started using <c>yFirstMicroPython()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned MicroPython interpreters order.
    ///   If you want to find a specific a MicroPython interpreter, use <c>MicroPython.findMicroPython()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YMicroPython</c> object, corresponding to
    ///   a MicroPython interpreter currently online, or a <c>NIL</c> pointer
    ///   if there are no more MicroPython interpreters to enumerate.
    /// </returns>
    ///-
    function nextMicroPython():TYMicroPython;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstMicroPython():TYMicroPython;
  //--- (end of generated code: YMicroPython accessors declaration)
  end;

//--- (generated code: YMicroPython functions declaration)
  ////
  /// <summary>
  ///   Retrieves a MicroPython interpreter for a given identifier.
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
  ///   This function does not require that the MicroPython interpreter is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YMicroPython.isOnline()</c> to test if the MicroPython interpreter is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a MicroPython interpreter by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the MicroPython interpreter, for instance
  ///   <c>MyDevice.microPython</c>.
  /// </param>
  /// <returns>
  ///   a <c>YMicroPython</c> object allowing you to drive the MicroPython interpreter.
  /// </returns>
  ///-
  function yFindMicroPython(func:string):TYMicroPython;
  ////
  /// <summary>
  ///   Starts the enumeration of MicroPython interpreters currently accessible.
  /// <para>
  ///   Use the method <c>YMicroPython.nextMicroPython()</c> to iterate on
  ///   next MicroPython interpreters.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YMicroPython</c> object, corresponding to
  ///   the first MicroPython interpreter currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstMicroPython():TYMicroPython;

Procedure yInternalEventCallback(obj:TYMicroPython; value:string);

//--- (end of generated code: YMicroPython functions declaration)

implementation

//--- (generated code: YMicroPython dlldef)
//--- (end of generated code: YMicroPython dlldef)

  constructor TYMicroPython.Create(func:string);
    begin
      inherited Create(func);
      _className := 'MicroPython';
      //--- (generated code: YMicroPython accessors initialization)
      _lastMsg := Y_LASTMSG_INVALID;
      _heapUsage := Y_HEAPUSAGE_INVALID;
      _heapFrag := Y_HEAPFRAG_INVALID;
      _xheapUsage := Y_XHEAPUSAGE_INVALID;
      _stackUsage := Y_STACKUSAGE_INVALID;
      _currentScript := Y_CURRENTSCRIPT_INVALID;
      _startupScript := Y_STARTUPSCRIPT_INVALID;
      _startupDelay := Y_STARTUPDELAY_INVALID;
      _debugMode := Y_DEBUGMODE_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackMicroPython := nil;
      _prevCbPos := 0;
      _logPos := 0;
      //--- (end of generated code: YMicroPython accessors initialization)
    end;

//--- (generated code: YMicroPython yapiwrapper)
//--- (end of generated code: YMicroPython yapiwrapper)

//--- (generated code: YMicroPython implementation)
{$HINTS OFF}
  function TYMicroPython._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'lastMsg') then
        begin
          _lastMsg := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'heapUsage') then
        begin
          _heapUsage := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'heapFrag') then
        begin
          _heapFrag := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'xheapUsage') then
        begin
          _xheapUsage := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'stackUsage') then
        begin
          _stackUsage := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'currentScript') then
        begin
          _currentScript := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'startupScript') then
        begin
          _startupScript := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'startupDelay') then
        begin
          _startupDelay := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'debugMode') then
        begin
          _debugMode := member^.ivalue;
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

  function TYMicroPython.get_lastMsg():string;
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


  function TYMicroPython.get_heapUsage():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_HEAPUSAGE_INVALID;
              exit;
            end;
        end;
      res := self._heapUsage;
      result := res;
      exit;
    end;


  function TYMicroPython.get_heapFrag():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_HEAPFRAG_INVALID;
              exit;
            end;
        end;
      res := self._heapFrag;
      result := res;
      exit;
    end;


  function TYMicroPython.get_xheapUsage():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_XHEAPUSAGE_INVALID;
              exit;
            end;
        end;
      res := self._xheapUsage;
      result := res;
      exit;
    end;


  function TYMicroPython.get_stackUsage():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_STACKUSAGE_INVALID;
              exit;
            end;
        end;
      res := self._stackUsage;
      result := res;
      exit;
    end;


  function TYMicroPython.get_currentScript():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTSCRIPT_INVALID;
              exit;
            end;
        end;
      res := self._currentScript;
      result := res;
      exit;
    end;


  function TYMicroPython.set_currentScript(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('currentScript',rest_val);
    end;

  function TYMicroPython.get_startupScript():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_STARTUPSCRIPT_INVALID;
              exit;
            end;
        end;
      res := self._startupScript;
      result := res;
      exit;
    end;


  function TYMicroPython.set_startupScript(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('startupScript',rest_val);
    end;

  function TYMicroPython.set_startupDelay(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('startupDelay',rest_val);
    end;

  function TYMicroPython.get_startupDelay():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_STARTUPDELAY_INVALID;
              exit;
            end;
        end;
      res := self._startupDelay;
      result := res;
      exit;
    end;


  function TYMicroPython.get_debugMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DEBUGMODE_INVALID;
              exit;
            end;
        end;
      res := self._debugMode;
      result := res;
      exit;
    end;


  function TYMicroPython.set_debugMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('debugMode',rest_val);
    end;

  function TYMicroPython.get_command():string;
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


  function TYMicroPython.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYMicroPython.FindMicroPython(func: string):TYMicroPython;
    var
      obj : TYMicroPython;
    begin
      obj := TYMicroPython(TYFunction._FindFromCache('MicroPython', func));
      if (obj = nil) then
        begin
          obj :=  TYMicroPython.create(func);
          TYFunction._AddToCache('MicroPython', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYMicroPython.registerValueCallback(callback: TYMicroPythonValueCallback):LongInt;
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
      self._valueCallbackMicroPython := callback;
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


  function TYMicroPython._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackMicroPython) <> nil) then
        begin
          self._valueCallbackMicroPython(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYMicroPython.eval(codeName: string; mpyCode: string):LongInt;
    var
      fullname : string;
      res : LongInt;
    begin
      fullname := 'mpy:'+codeName;
      res := self._upload(fullname, _StrToByte(mpyCode));
      result := res;
      exit;
    end;


  function TYMicroPython.reset():LongInt;
    var
      res : LongInt;
      state : string;
      ignoreErrMsg : string;
    begin
      res := self.set_command('Z');
      if not(res = YAPI_SUCCESS) then
        begin
          self._throw(YAPI_IO_ERROR,'unable to trigger MicroPython reset');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      // Wait until the reset is effective
      state := Copy(self.get_advertisedValue, 0 + 1, 1);
      while not((state = 'z')) do
        begin
          ySleep(50, ignoreErrMsg);
          state := Copy(self.get_advertisedValue, 0 + 1, 1);
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYMicroPython.clearLogs():LongInt;
    var
      res : LongInt;
    begin
      res := self.set_command('z');
      result := res;
      exit;
    end;


  function TYMicroPython.get_lastLogs():string;
    var
      buff : TByteArray;
      bufflen : LongInt;
      res : string;
    begin
      buff := self._download('mpy.txt');
      bufflen := length(buff) - 1;
      while (bufflen > 0) and(buff[bufflen] <> 64) do
        begin
          bufflen := bufflen - 1;
        end;
      res := Copy(_ByteToString(buff), 0 + 1, bufflen);
      result := res;
      exit;
    end;


  function TYMicroPython.registerLogCallback(callback: TYMicroPythonLogCallback):LongInt;
    var
      serial : string;
    begin
      serial := self.get_serialNumber;
      if (serial = YAPI_INVALID_STRING) then
        begin
          result := YAPI_DEVICE_NOT_FOUND;
          exit;
        end;
      self._logCallback := callback;
      self._isFirstCb := true;
      if (addr(callback) <> nil) then
        begin
          self.registerValueCallback(yInternalEventCallback);
        end
      else
        begin
          self.registerValueCallback(TYMicroPythonValueCallback(nil));
        end;
      result := 0;
      exit;
    end;


  function TYMicroPython.get_logCallback():TYMicroPythonLogCallback;
    begin
      result := self._logCallback;
      exit;
    end;


  function TYMicroPython._internalEventHandler(cbVal: string):LongInt;
    var
      cbPos : LongInt;
      cbDPos : LongInt;
      url : string;
      content : TByteArray;
      endPos : LongInt;
      contentStr : string;
      msgArr : TStringArray;
      arrLen : LongInt;
      lenStr : string;
      arrPos : LongInt;
      logMsg : string;
    begin
      SetLength(msgArr, 0);
      // detect possible power cycle of the reader to clear event pointer
      cbPos := StrToInt('$0' + Copy(cbVal, 1 + 1, Length(cbVal)-1));
      cbDPos := ((cbPos - self._prevCbPos) and ($0fffff));
      self._prevCbPos := cbPos;
      if cbDPos > 65536 then
        begin
          self._logPos := 0;
        end;
      if not((addr(self._logCallback) <> nil)) then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      if self._isFirstCb then
        begin
          // use first emulated value callback caused by registerValueCallback:
          // to retrieve current logs position
          self._logPos := 0;
          self._prevPartialLog = '';
          url := 'mpy.txt';
        end
      else
        begin
          // load all messages since previous call
          url := 'mpy.txt?pos='+inttostr(self._logPos);
        end;

      content := self._download(url);
      contentStr := _ByteToString(content);
      // look for new position indicator at end of logs
      endPos := length(content) - 1;
      while (endPos >= 0) and(content[endPos] <> 64) do
        begin
          endPos := endPos - 1;
        end;
      if not(endPos > 0) then
        begin
          self._throw(YAPI_IO_ERROR,'fail to download micropython logs');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      lenStr := Copy(contentStr, endPos+1 + 1, Length(contentStr)-(endPos+1));
      // update processed event position pointer
      self._logPos := _atoi(lenStr);
      if self._isFirstCb then
        begin
          // don't generate callbacks log messages before call to registerLogCallback
          self._isFirstCb := false;
          result := YAPI_SUCCESS;
          exit;
        end;
      // now generate callbacks for each complete log line
      endPos := endPos - 1;
      if not(content[endPos] = 10) then
        begin
          self._throw(YAPI_IO_ERROR,'fail to download micropython logs');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      contentStr := Copy(contentStr, 0 + 1, endPos);
      msgArr := _stringSplit(contentStr, #10);
      arrLen := length(msgArr) - 1;
      if arrLen > 0 then
        begin
          logMsg := ''+self._prevPartialLog+''+msgArr[0];
          if (addr(self._logCallback) <> nil) then
            begin
              self._logCallback(self, logMsg);
            end;
          self._prevPartialLog := '';
          arrPos := 1;
          while arrPos < arrLen do
            begin
              logMsg := msgArr[arrPos];
              if (addr(self._logCallback) <> nil) then
                begin
                  self._logCallback(self, logMsg);
                end;
              arrPos := arrPos + 1;
            end;
        end;
      self._prevPartialLog = ''+self._prevPartialLog+''+msgArr[arrLen];
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYMicroPython.nextMicroPython(): TYMicroPython;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextMicroPython := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextMicroPython := nil;
          exit;
        end;
      nextMicroPython := TYMicroPython.FindMicroPython(hwid);
    end;

  class function TYMicroPython.FirstMicroPython(): TYMicroPython;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('MicroPython', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYMicroPython.FindMicroPython(serial+'.'+funcId);
    end;

Procedure yInternalEventCallback(obj:TYMicroPython; value:string);
begin
    obj._internalEventHandler(value);
end;

//--- (end of generated code: YMicroPython implementation)

//--- (generated code: YMicroPython functions)

  function yFindMicroPython(func:string): TYMicroPython;
    begin
      result := TYMicroPython.FindMicroPython(func);
    end;

  function yFirstMicroPython(): TYMicroPython;
    begin
      result := TYMicroPython.FirstMicroPython();
    end;

  procedure _MicroPythonCleanup();
    begin
    end;

//--- (end of generated code: YMicroPython functions)

initialization
  //--- (generated code: YMicroPython initialization)
  //--- (end of generated code: YMicroPython initialization)

finalization
  //--- (generated code: YMicroPython cleanup)
  _MicroPythonCleanup();
  //--- (end of generated code: YMicroPython cleanup)

end.
