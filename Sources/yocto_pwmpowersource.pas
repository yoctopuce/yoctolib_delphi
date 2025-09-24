{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindPwmPowerSource(), the high-level API for PwmPowerSource functions
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


unit yocto_pwmpowersource;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YPwmPowerSource definitions)

const Y_POWERMODE_USB_5V = 0;
const Y_POWERMODE_USB_3V = 1;
const Y_POWERMODE_EXT_V = 2;
const Y_POWERMODE_OPNDRN = 3;
const Y_POWERMODE_INVALID = -1;

//--- (end of YPwmPowerSource definitions)

//--- (YPwmPowerSource yapiwrapper declaration)
//--- (end of YPwmPowerSource yapiwrapper declaration)

type

  TYPwmPowerSource = class;
  //--- (YPwmPowerSource class start)
  TYPwmPowerSourceValueCallback = procedure(func: TYPwmPowerSource; value:string);
  TYPwmPowerSourceTimedReportCallback = procedure(func: TYPwmPowerSource; value:TYMeasure);

  ////
  /// <summary>
  ///   TYPwmPowerSource Class: PWM generator power source control interface, available for instance in the Yocto-PWM-Tx
  /// <para>
  ///   The <c>YPwmPowerSource</c> class allows you to configure
  ///   the voltage source used by all PWM outputs on the same device.
  /// </para>
  /// </summary>
  ///-
  TYPwmPowerSource=class(TYFunction)
  //--- (end of YPwmPowerSource class start)
  protected
  //--- (YPwmPowerSource declaration)
    // Attributes (function value cache)
    _powerMode                : Integer;
    _valueCallbackPwmPowerSource : TYPwmPowerSourceValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YPwmPowerSource declaration)

  public
    //--- (YPwmPowerSource accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the selected power source for the PWM on the same device.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YPwmPowerSource.POWERMODE_USB_5V</c>, <c>YPwmPowerSource.POWERMODE_USB_3V</c>,
    ///   <c>YPwmPowerSource.POWERMODE_EXT_V</c> and <c>YPwmPowerSource.POWERMODE_OPNDRN</c> corresponding to
    ///   the selected power source for the PWM on the same device
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPwmPowerSource.POWERMODE_INVALID</c>.
    /// </para>
    ///-
    function get_powerMode():Integer;

    ////
    /// <summary>
    ///   Changes  the PWM power source.
    /// <para>
    ///   PWM can use isolated 5V from USB, isolated 3V from USB or
    ///   voltage from an external power source. The PWM can also work in open drain  mode. In that
    ///   mode, the PWM actively pulls the line down.
    ///   Warning: this setting is common to all PWM on the same device. If you change that parameter,
    ///   all PWM located on the same device are  affected.
    ///   If you want the change to be kept after a device reboot, make sure  to call the matching
    ///   module <c>saveToFlash()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YPwmPowerSource.POWERMODE_USB_5V</c>, <c>YPwmPowerSource.POWERMODE_USB_3V</c>,
    ///   <c>YPwmPowerSource.POWERMODE_EXT_V</c> and <c>YPwmPowerSource.POWERMODE_OPNDRN</c> corresponding to
    ///    the PWM power source
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
    function set_powerMode(newval:Integer):integer;

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
    ///   Use the method <c>YPwmPowerSource.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YPwmPowerSource</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindPwmPowerSource(func: string):TYPwmPowerSource;

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
    function registerValueCallback(callback: TYPwmPowerSourceValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of PWM generator power sources started using <c>yFirstPwmPowerSource()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned PWM generator power sources order.
    ///   If you want to find a specific a PWM generator power source, use <c>PwmPowerSource.findPwmPowerSource()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YPwmPowerSource</c> object, corresponding to
    ///   a PWM generator power source currently online, or a <c>NIL</c> pointer
    ///   if there are no more PWM generator power sources to enumerate.
    /// </returns>
    ///-
    function nextPwmPowerSource():TYPwmPowerSource;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstPwmPowerSource():TYPwmPowerSource;
  //--- (end of YPwmPowerSource accessors declaration)
  end;

//--- (YPwmPowerSource functions declaration)
  ////
  /// <summary>
  ///   Retrieves a PWM generator power source for a given identifier.
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
  ///   This function does not require that the PWM generator power source is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YPwmPowerSource.isOnline()</c> to test if the PWM generator power source is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a PWM generator power source by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the PWM generator power source, for instance
  ///   <c>YPWMTX01.pwmPowerSource</c>.
  /// </param>
  /// <returns>
  ///   a <c>YPwmPowerSource</c> object allowing you to drive the PWM generator power source.
  /// </returns>
  ///-
  function yFindPwmPowerSource(func:string):TYPwmPowerSource;
  ////
  /// <summary>
  ///   Starts the enumeration of PWM generator power sources currently accessible.
  /// <para>
  ///   Use the method <c>YPwmPowerSource.nextPwmPowerSource()</c> to iterate on
  ///   next PWM generator power sources.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YPwmPowerSource</c> object, corresponding to
  ///   the first PWM generator power source currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstPwmPowerSource():TYPwmPowerSource;

//--- (end of YPwmPowerSource functions declaration)

implementation

//--- (YPwmPowerSource dlldef)
//--- (end of YPwmPowerSource dlldef)

  constructor TYPwmPowerSource.Create(func:string);
    begin
      inherited Create(func);
      _className := 'PwmPowerSource';
      //--- (YPwmPowerSource accessors initialization)
      _powerMode := Y_POWERMODE_INVALID;
      _valueCallbackPwmPowerSource := nil;
      //--- (end of YPwmPowerSource accessors initialization)
    end;

//--- (YPwmPowerSource yapiwrapper)
//--- (end of YPwmPowerSource yapiwrapper)

//--- (YPwmPowerSource implementation)
{$HINTS OFF}
  function TYPwmPowerSource._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'powerMode') then
        begin
          _powerMode := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYPwmPowerSource.get_powerMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_POWERMODE_INVALID;
              exit;
            end;
        end;
      res := self._powerMode;
      result := res;
      exit;
    end;


  function TYPwmPowerSource.set_powerMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('powerMode',rest_val);
    end;

  class function TYPwmPowerSource.FindPwmPowerSource(func: string):TYPwmPowerSource;
    var
      obj : TYPwmPowerSource;
    begin
      obj := TYPwmPowerSource(TYFunction._FindFromCache('PwmPowerSource', func));
      if (obj = nil) then
        begin
          obj :=  TYPwmPowerSource.create(func);
          TYFunction._AddToCache('PwmPowerSource', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYPwmPowerSource.registerValueCallback(callback: TYPwmPowerSourceValueCallback):LongInt;
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
      self._valueCallbackPwmPowerSource := callback;
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


  function TYPwmPowerSource._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackPwmPowerSource) <> nil) then
        begin
          self._valueCallbackPwmPowerSource(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYPwmPowerSource.nextPwmPowerSource(): TYPwmPowerSource;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextPwmPowerSource := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextPwmPowerSource := nil;
          exit;
        end;
      nextPwmPowerSource := TYPwmPowerSource.FindPwmPowerSource(hwid);
    end;

  class function TYPwmPowerSource.FirstPwmPowerSource(): TYPwmPowerSource;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('PwmPowerSource', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYPwmPowerSource.FindPwmPowerSource(serial+'.'+funcId);
    end;

//--- (end of YPwmPowerSource implementation)

//--- (YPwmPowerSource functions)

  function yFindPwmPowerSource(func:string): TYPwmPowerSource;
    begin
      result := TYPwmPowerSource.FindPwmPowerSource(func);
    end;

  function yFirstPwmPowerSource(): TYPwmPowerSource;
    begin
      result := TYPwmPowerSource.FirstPwmPowerSource();
    end;

  procedure _PwmPowerSourceCleanup();
    begin
    end;

//--- (end of YPwmPowerSource functions)

initialization
  //--- (YPwmPowerSource initialization)
  //--- (end of YPwmPowerSource initialization)

finalization
  //--- (YPwmPowerSource cleanup)
  _PwmPowerSourceCleanup();
  //--- (end of YPwmPowerSource cleanup)

end.
