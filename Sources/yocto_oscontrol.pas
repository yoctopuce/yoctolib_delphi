{*********************************************************************
 *
 * $Id: yocto_oscontrol.pas 32348 2018-09-25 13:28:40Z seb $
 *
 * Implements yFindOsControl(), the high-level API for OsControl functions
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


unit yocto_oscontrol;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YOsControl definitions)

const Y_SHUTDOWNCOUNTDOWN_INVALID     = YAPI_INVALID_UINT;


//--- (end of YOsControl definitions)
//--- (YOsControl yapiwrapper declaration)
//--- (end of YOsControl yapiwrapper declaration)

type
  TYOsControl = class;
  //--- (YOsControl class start)
  TYOsControlValueCallback = procedure(func: TYOsControl; value:string);
  TYOsControlTimedReportCallback = procedure(func: TYOsControl; value:TYMeasure);

  ////
  /// <summary>
  ///   TYOsControl Class: OS control
  /// <para>
  ///   The OScontrol object allows some control over the operating system running a VirtualHub.
  ///   OsControl is available on the VirtualHub software only. This feature must be activated at the VirtualHub
  ///   start up with -o option.
  /// </para>
  /// </summary>
  ///-
  TYOsControl=class(TYFunction)
  //--- (end of YOsControl class start)
  protected
  //--- (YOsControl declaration)
    // Attributes (function value cache)
    _shutdownCountdown        : LongInt;
    _valueCallbackOsControl   : TYOsControlValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YOsControl declaration)

  public
    //--- (YOsControl accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the remaining number of seconds before the OS shutdown, or zero when no
    ///   shutdown has been scheduled.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the remaining number of seconds before the OS shutdown, or zero when no
    ///   shutdown has been scheduled
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SHUTDOWNCOUNTDOWN_INVALID</c>.
    /// </para>
    ///-
    function get_shutdownCountdown():LongInt;

    function set_shutdownCountdown(newval:LongInt):integer;

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
    ///   Use the method <c>YOsControl.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YOsControl</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindOsControl(func: string):TYOsControl;

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
    function registerValueCallback(callback: TYOsControlValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Schedules an OS shutdown after a given number of seconds.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="secBeforeShutDown">
    ///   number of seconds before shutdown
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function shutdown(secBeforeShutDown: LongInt):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of OS control started using <c>yFirstOsControl()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YOsControl</c> object, corresponding to
    ///   OS control currently online, or a <c>NIL</c> pointer
    ///   if there are no more OS control to enumerate.
    /// </returns>
    ///-
    function nextOsControl():TYOsControl;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstOsControl():TYOsControl;
  //--- (end of YOsControl accessors declaration)
  end;

//--- (YOsControl functions declaration)
  ////
  /// <summary>
  ///   Retrieves OS control for a given identifier.
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
  ///   This function does not require that the OS control is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YOsControl.isOnline()</c> to test if the OS control is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   OS control by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the OS control
  /// </param>
  /// <returns>
  ///   a <c>YOsControl</c> object allowing you to drive the OS control.
  /// </returns>
  ///-
  function yFindOsControl(func:string):TYOsControl;
  ////
  /// <summary>
  ///   Starts the enumeration of OS control currently accessible.
  /// <para>
  ///   Use the method <c>YOsControl.nextOsControl()</c> to iterate on
  ///   next OS control.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YOsControl</c> object, corresponding to
  ///   the first OS control currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstOsControl():TYOsControl;

//--- (end of YOsControl functions declaration)

implementation
//--- (YOsControl dlldef)
//--- (end of YOsControl dlldef)

  constructor TYOsControl.Create(func:string);
    begin
      inherited Create(func);
      _className := 'OsControl';
      //--- (YOsControl accessors initialization)
      _shutdownCountdown := Y_SHUTDOWNCOUNTDOWN_INVALID;
      _valueCallbackOsControl := nil;
      //--- (end of YOsControl accessors initialization)
    end;

//--- (YOsControl yapiwrapper)
//--- (end of YOsControl yapiwrapper)

//--- (YOsControl implementation)
{$HINTS OFF}
  function TYOsControl._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'shutdownCountdown') then
        begin
          _shutdownCountdown := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYOsControl.get_shutdownCountdown():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SHUTDOWNCOUNTDOWN_INVALID;
              exit;
            end;
        end;
      res := self._shutdownCountdown;
      result := res;
      exit;
    end;


  function TYOsControl.set_shutdownCountdown(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('shutdownCountdown',rest_val);
    end;

  class function TYOsControl.FindOsControl(func: string):TYOsControl;
    var
      obj : TYOsControl;
    begin
      obj := TYOsControl(TYFunction._FindFromCache('OsControl', func));
      if obj = nil then
        begin
          obj :=  TYOsControl.create(func);
          TYFunction._AddToCache('OsControl',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYOsControl.registerValueCallback(callback: TYOsControlValueCallback):LongInt;
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
      self._valueCallbackOsControl := callback;
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


  function TYOsControl._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackOsControl) <> nil) then
        begin
          self._valueCallbackOsControl(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYOsControl.shutdown(secBeforeShutDown: LongInt):LongInt;
    begin
      result := self.set_shutdownCountdown(secBeforeShutDown);
      exit;
    end;


  function TYOsControl.nextOsControl(): TYOsControl;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextOsControl := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextOsControl := nil;
          exit;
        end;
      nextOsControl := TYOsControl.FindOsControl(hwid);
    end;

  class function TYOsControl.FirstOsControl(): TYOsControl;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('OsControl', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYOsControl.FindOsControl(serial+'.'+funcId);
    end;

//--- (end of YOsControl implementation)

//--- (YOsControl functions)

  function yFindOsControl(func:string): TYOsControl;
    begin
      result := TYOsControl.FindOsControl(func);
    end;

  function yFirstOsControl(): TYOsControl;
    begin
      result := TYOsControl.FirstOsControl();
    end;

  procedure _OsControlCleanup();
    begin
    end;

//--- (end of YOsControl functions)

initialization
  //--- (YOsControl initialization)
  //--- (end of YOsControl initialization)

finalization
  //--- (YOsControl cleanup)
  _OsControlCleanup();
  //--- (end of YOsControl cleanup)
end.
