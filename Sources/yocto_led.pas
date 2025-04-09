{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindLed(), the high-level API for Led functions
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


unit yocto_led;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YLed definitions)

const Y_POWER_OFF = 0;
const Y_POWER_ON = 1;
const Y_POWER_INVALID = -1;
const Y_LUMINOSITY_INVALID            = YAPI_INVALID_UINT;
const Y_BLINKING_STILL = 0;
const Y_BLINKING_RELAX = 1;
const Y_BLINKING_AWARE = 2;
const Y_BLINKING_RUN = 3;
const Y_BLINKING_CALL = 4;
const Y_BLINKING_PANIC = 5;
const Y_BLINKING_INVALID = -1;

//--- (end of YLed definitions)

//--- (YLed yapiwrapper declaration)
//--- (end of YLed yapiwrapper declaration)

type

  TYLed = class;
  //--- (YLed class start)
  TYLedValueCallback = procedure(func: TYLed; value:string);
  TYLedTimedReportCallback = procedure(func: TYLed; value:TYMeasure);

  ////
  /// <summary>
  ///   TYLed Class: monochrome LED control interface, available for instance in the Yocto-Buzzer, the
  ///   YoctoBox-Short-Thin-Black-Prox, the YoctoBox-Short-Thin-Transp or the YoctoBox-Short-Thin-Transp-Prox
  /// <para>
  ///   The <c>YLed</c> class allows you to drive a monocolor LED.
  ///   You can not only to drive the intensity of the LED, but also to
  ///   have it blink at various preset frequencies.
  /// </para>
  /// </summary>
  ///-
  TYLed=class(TYFunction)
  //--- (end of YLed class start)
  protected
  //--- (YLed declaration)
    // Attributes (function value cache)
    _power                    : Integer;
    _luminosity               : LongInt;
    _blinking                 : Integer;
    _valueCallbackLed         : TYLedValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YLed declaration)

  public
    //--- (YLed accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the current LED state.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YLed.POWER_OFF</c> or <c>YLed.POWER_ON</c>, according to the current LED state
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YLed.POWER_INVALID</c>.
    /// </para>
    ///-
    function get_power():Integer;

    ////
    /// <summary>
    ///   Changes the state of the LED.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YLed.POWER_OFF</c> or <c>YLed.POWER_ON</c>, according to the state of the LED
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
    function set_power(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the current LED intensity (in per cent).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current LED intensity (in per cent)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YLed.LUMINOSITY_INVALID</c>.
    /// </para>
    ///-
    function get_luminosity():LongInt;

    ////
    /// <summary>
    ///   Changes the current LED intensity (in per cent).
    /// <para>
    ///   Remember to call the
    ///   <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the current LED intensity (in per cent)
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
    function set_luminosity(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the current LED signaling mode.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YLed.BLINKING_STILL</c>, <c>YLed.BLINKING_RELAX</c>, <c>YLed.BLINKING_AWARE</c>,
    ///   <c>YLed.BLINKING_RUN</c>, <c>YLed.BLINKING_CALL</c> and <c>YLed.BLINKING_PANIC</c> corresponding to
    ///   the current LED signaling mode
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YLed.BLINKING_INVALID</c>.
    /// </para>
    ///-
    function get_blinking():Integer;

    ////
    /// <summary>
    ///   Changes the current LED signaling mode.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YLed.BLINKING_STILL</c>, <c>YLed.BLINKING_RELAX</c>, <c>YLed.BLINKING_AWARE</c>,
    ///   <c>YLed.BLINKING_RUN</c>, <c>YLed.BLINKING_CALL</c> and <c>YLed.BLINKING_PANIC</c> corresponding to
    ///   the current LED signaling mode
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
    function set_blinking(newval:Integer):integer;

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
    ///   Use the method <c>YLed.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YLed</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindLed(func: string):TYLed;

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
    function registerValueCallback(callback: TYLedValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of monochrome LEDs started using <c>yFirstLed()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned monochrome LEDs order.
    ///   If you want to find a specific a monochrome LED, use <c>Led.findLed()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YLed</c> object, corresponding to
    ///   a monochrome LED currently online, or a <c>NIL</c> pointer
    ///   if there are no more monochrome LEDs to enumerate.
    /// </returns>
    ///-
    function nextLed():TYLed;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstLed():TYLed;
  //--- (end of YLed accessors declaration)
  end;

//--- (YLed functions declaration)
  ////
  /// <summary>
  ///   Retrieves a monochrome LED for a given identifier.
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
  ///   This function does not require that the monochrome LED is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YLed.isOnline()</c> to test if the monochrome LED is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a monochrome LED by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the monochrome LED, for instance
  ///   <c>YBUZZER2.led1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YLed</c> object allowing you to drive the monochrome LED.
  /// </returns>
  ///-
  function yFindLed(func:string):TYLed;
  ////
  /// <summary>
  ///   Starts the enumeration of monochrome LEDs currently accessible.
  /// <para>
  ///   Use the method <c>YLed.nextLed()</c> to iterate on
  ///   next monochrome LEDs.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YLed</c> object, corresponding to
  ///   the first monochrome LED currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstLed():TYLed;

//--- (end of YLed functions declaration)

implementation

//--- (YLed dlldef)
//--- (end of YLed dlldef)

  constructor TYLed.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Led';
      //--- (YLed accessors initialization)
      _power := Y_POWER_INVALID;
      _luminosity := Y_LUMINOSITY_INVALID;
      _blinking := Y_BLINKING_INVALID;
      _valueCallbackLed := nil;
      //--- (end of YLed accessors initialization)
    end;

//--- (YLed yapiwrapper)
//--- (end of YLed yapiwrapper)

//--- (YLed implementation)
{$HINTS OFF}
  function TYLed._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'power') then
        begin
          _power := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'luminosity') then
        begin
          _luminosity := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'blinking') then
        begin
          _blinking := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYLed.get_power():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_POWER_INVALID;
              exit;
            end;
        end;
      res := self._power;
      result := res;
      exit;
    end;


  function TYLed.set_power(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('power',rest_val);
    end;

  function TYLed.get_luminosity():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LUMINOSITY_INVALID;
              exit;
            end;
        end;
      res := self._luminosity;
      result := res;
      exit;
    end;


  function TYLed.set_luminosity(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('luminosity',rest_val);
    end;

  function TYLed.get_blinking():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BLINKING_INVALID;
              exit;
            end;
        end;
      res := self._blinking;
      result := res;
      exit;
    end;


  function TYLed.set_blinking(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('blinking',rest_val);
    end;

  class function TYLed.FindLed(func: string):TYLed;
    var
      obj : TYLed;
    begin
      obj := TYLed(TYFunction._FindFromCache('Led', func));
      if obj = nil then
        begin
          obj :=  TYLed.create(func);
          TYFunction._AddToCache('Led', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYLed.registerValueCallback(callback: TYLedValueCallback):LongInt;
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
      self._valueCallbackLed := callback;
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


  function TYLed._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackLed) <> nil) then
        begin
          self._valueCallbackLed(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYLed.nextLed(): TYLed;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextLed := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextLed := nil;
          exit;
        end;
      nextLed := TYLed.FindLed(hwid);
    end;

  class function TYLed.FirstLed(): TYLed;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Led', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYLed.FindLed(serial+'.'+funcId);
    end;

//--- (end of YLed implementation)

//--- (YLed functions)

  function yFindLed(func:string): TYLed;
    begin
      result := TYLed.FindLed(func);
    end;

  function yFirstLed(): TYLed;
    begin
      result := TYLed.FirstLed();
    end;

  procedure _LedCleanup();
    begin
    end;

//--- (end of YLed functions)

initialization
  //--- (YLed initialization)
  //--- (end of YLed initialization)

finalization
  //--- (YLed cleanup)
  _LedCleanup();
  //--- (end of YLed cleanup)

end.
