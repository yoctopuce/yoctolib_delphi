{*********************************************************************
 *
 *  $Id: yocto_hubport.pas 35285 2019-05-07 07:37:56Z seb $
 *
 *  Implements yFindHubPort(), the high-level API for HubPort functions
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


unit yocto_hubport;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YHubPort definitions)

const Y_ENABLED_FALSE = 0;
const Y_ENABLED_TRUE = 1;
const Y_ENABLED_INVALID = -1;
const Y_PORTSTATE_OFF = 0;
const Y_PORTSTATE_OVRLD = 1;
const Y_PORTSTATE_ON = 2;
const Y_PORTSTATE_RUN = 3;
const Y_PORTSTATE_PROG = 4;
const Y_PORTSTATE_INVALID = -1;
const Y_BAUDRATE_INVALID              = YAPI_INVALID_UINT;


//--- (end of YHubPort definitions)
//--- (YHubPort yapiwrapper declaration)
//--- (end of YHubPort yapiwrapper declaration)

type
  TYHubPort = class;
  //--- (YHubPort class start)
  TYHubPortValueCallback = procedure(func: TYHubPort; value:string);
  TYHubPortTimedReportCallback = procedure(func: TYHubPort; value:TYMeasure);

  ////
  /// <summary>
  ///   TYHubPort Class: Yocto-hub port interface
  /// <para>
  ///   YHubPort objects provide control over the power supply for every
  ///   YoctoHub port and provide information about the device connected to it.
  ///   The logical name of a YHubPort is always automatically set to the
  ///   unique serial number of the Yoctopuce device connected to it.
  /// </para>
  /// </summary>
  ///-
  TYHubPort=class(TYFunction)
  //--- (end of YHubPort class start)
  protected
  //--- (YHubPort declaration)
    // Attributes (function value cache)
    _enabled                  : Integer;
    _portState                : Integer;
    _baudRate                 : LongInt;
    _valueCallbackHubPort     : TYHubPortValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YHubPort declaration)

  public
    //--- (YHubPort accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns true if the Yocto-hub port is powered, false otherwise.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_ENABLED_FALSE</c> or <c>Y_ENABLED_TRUE</c>, according to true if the Yocto-hub port is
    ///   powered, false otherwise
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ENABLED_INVALID</c>.
    /// </para>
    ///-
    function get_enabled():Integer;

    ////
    /// <summary>
    ///   Changes the activation of the Yocto-hub port.
    /// <para>
    ///   If the port is enabled, the
    ///   connected module is powered. Otherwise, port power is shut down.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>Y_ENABLED_FALSE</c> or <c>Y_ENABLED_TRUE</c>, according to the activation of the Yocto-hub port
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
    function set_enabled(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the current state of the Yocto-hub port.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_PORTSTATE_OFF</c>, <c>Y_PORTSTATE_OVRLD</c>, <c>Y_PORTSTATE_ON</c>,
    ///   <c>Y_PORTSTATE_RUN</c> and <c>Y_PORTSTATE_PROG</c> corresponding to the current state of the Yocto-hub port
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PORTSTATE_INVALID</c>.
    /// </para>
    ///-
    function get_portState():Integer;

    ////
    /// <summary>
    ///   Returns the current baud rate used by this Yocto-hub port, in kbps.
    /// <para>
    ///   The default value is 1000 kbps, but a slower rate may be used if communication
    ///   problems are encountered.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current baud rate used by this Yocto-hub port, in kbps
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BAUDRATE_INVALID</c>.
    /// </para>
    ///-
    function get_baudRate():LongInt;

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
    ///   Use the method <c>YHubPort.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a string that uniquely characterizes $THEFUNCTION$
    /// </param>
    /// <returns>
    ///   a <c>YHubPort</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindHubPort(func: string):TYHubPort;

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
    function registerValueCallback(callback: TYHubPortValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of Yocto-hub ports started using <c>yFirstHubPort()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned Yocto-hub ports order.
    ///   If you want to find a specific a Yocto-hub port, use <c>HubPort.findHubPort()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YHubPort</c> object, corresponding to
    ///   a Yocto-hub port currently online, or a <c>NIL</c> pointer
    ///   if there are no more Yocto-hub ports to enumerate.
    /// </returns>
    ///-
    function nextHubPort():TYHubPort;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstHubPort():TYHubPort;
  //--- (end of YHubPort accessors declaration)
  end;

//--- (YHubPort functions declaration)
  ////
  /// <summary>
  ///   Retrieves a Yocto-hub port for a given identifier.
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
  ///   This function does not require that the Yocto-hub port is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YHubPort.isOnline()</c> to test if the Yocto-hub port is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a Yocto-hub port by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the Yocto-hub port
  /// </param>
  /// <returns>
  ///   a <c>YHubPort</c> object allowing you to drive the Yocto-hub port.
  /// </returns>
  ///-
  function yFindHubPort(func:string):TYHubPort;
  ////
  /// <summary>
  ///   Starts the enumeration of Yocto-hub ports currently accessible.
  /// <para>
  ///   Use the method <c>YHubPort.nextHubPort()</c> to iterate on
  ///   next Yocto-hub ports.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YHubPort</c> object, corresponding to
  ///   the first Yocto-hub port currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstHubPort():TYHubPort;

//--- (end of YHubPort functions declaration)

implementation
//--- (YHubPort dlldef)
//--- (end of YHubPort dlldef)

  constructor TYHubPort.Create(func:string);
    begin
      inherited Create(func);
      _className := 'HubPort';
      //--- (YHubPort accessors initialization)
      _enabled := Y_ENABLED_INVALID;
      _portState := Y_PORTSTATE_INVALID;
      _baudRate := Y_BAUDRATE_INVALID;
      _valueCallbackHubPort := nil;
      //--- (end of YHubPort accessors initialization)
    end;

//--- (YHubPort yapiwrapper)
//--- (end of YHubPort yapiwrapper)

//--- (YHubPort implementation)
{$HINTS OFF}
  function TYHubPort._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'enabled') then
        begin
          _enabled := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'portState') then
        begin
          _portState := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'baudRate') then
        begin
          _baudRate := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYHubPort.get_enabled():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ENABLED_INVALID;
              exit;
            end;
        end;
      res := self._enabled;
      result := res;
      exit;
    end;


  function TYHubPort.set_enabled(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('enabled',rest_val);
    end;

  function TYHubPort.get_portState():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PORTSTATE_INVALID;
              exit;
            end;
        end;
      res := self._portState;
      result := res;
      exit;
    end;


  function TYHubPort.get_baudRate():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BAUDRATE_INVALID;
              exit;
            end;
        end;
      res := self._baudRate;
      result := res;
      exit;
    end;


  class function TYHubPort.FindHubPort(func: string):TYHubPort;
    var
      obj : TYHubPort;
    begin
      obj := TYHubPort(TYFunction._FindFromCache('HubPort', func));
      if obj = nil then
        begin
          obj :=  TYHubPort.create(func);
          TYFunction._AddToCache('HubPort',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYHubPort.registerValueCallback(callback: TYHubPortValueCallback):LongInt;
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
      self._valueCallbackHubPort := callback;
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


  function TYHubPort._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackHubPort) <> nil) then
        begin
          self._valueCallbackHubPort(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYHubPort.nextHubPort(): TYHubPort;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextHubPort := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextHubPort := nil;
          exit;
        end;
      nextHubPort := TYHubPort.FindHubPort(hwid);
    end;

  class function TYHubPort.FirstHubPort(): TYHubPort;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('HubPort', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYHubPort.FindHubPort(serial+'.'+funcId);
    end;

//--- (end of YHubPort implementation)

//--- (YHubPort functions)

  function yFindHubPort(func:string): TYHubPort;
    begin
      result := TYHubPort.FindHubPort(func);
    end;

  function yFirstHubPort(): TYHubPort;
    begin
      result := TYHubPort.FirstHubPort();
    end;

  procedure _HubPortCleanup();
    begin
    end;

//--- (end of YHubPort functions)

initialization
  //--- (YHubPort initialization)
  //--- (end of YHubPort initialization)

finalization
  //--- (YHubPort cleanup)
  _HubPortCleanup();
  //--- (end of YHubPort cleanup)
end.
