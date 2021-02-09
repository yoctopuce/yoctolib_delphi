{*********************************************************************
 *
 *  $Id: yocto_dualpower.pas 43580 2021-01-26 17:46:01Z mvuilleu $
 *
 *  Implements yFindDualPower(), the high-level API for DualPower functions
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


unit yocto_dualpower;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YDualPower definitions)

const Y_POWERSTATE_OFF = 0;
const Y_POWERSTATE_FROM_USB = 1;
const Y_POWERSTATE_FROM_EXT = 2;
const Y_POWERSTATE_INVALID = -1;
const Y_POWERCONTROL_AUTO = 0;
const Y_POWERCONTROL_FROM_USB = 1;
const Y_POWERCONTROL_FROM_EXT = 2;
const Y_POWERCONTROL_OFF = 3;
const Y_POWERCONTROL_INVALID = -1;
const Y_EXTVOLTAGE_INVALID            = YAPI_INVALID_UINT;


//--- (end of YDualPower definitions)
//--- (YDualPower yapiwrapper declaration)
//--- (end of YDualPower yapiwrapper declaration)

type
  TYDualPower = class;
  //--- (YDualPower class start)
  TYDualPowerValueCallback = procedure(func: TYDualPower; value:string);
  TYDualPowerTimedReportCallback = procedure(func: TYDualPower; value:TYMeasure);

  ////
  /// <summary>
  ///   TYDualPower Class: dual power switch control interface, available for instance in the Yocto-Servo
  /// <para>
  ///   The <c>YDualPower</c> class allows you to control
  ///   the power source to use for module functions that require high current.
  ///   The module can also automatically disconnect the external power
  ///   when a voltage drop is observed on the external power source
  ///   (external battery running out of power).
  /// </para>
  /// </summary>
  ///-
  TYDualPower=class(TYFunction)
  //--- (end of YDualPower class start)
  protected
  //--- (YDualPower declaration)
    // Attributes (function value cache)
    _powerState               : Integer;
    _powerControl             : Integer;
    _extVoltage               : LongInt;
    _valueCallbackDualPower   : TYDualPowerValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YDualPower declaration)

  public
    //--- (YDualPower accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the current power source for module functions that require lots of current.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YDualPower.POWERSTATE_OFF</c>, <c>YDualPower.POWERSTATE_FROM_USB</c> and
    ///   <c>YDualPower.POWERSTATE_FROM_EXT</c> corresponding to the current power source for module
    ///   functions that require lots of current
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDualPower.POWERSTATE_INVALID</c>.
    /// </para>
    ///-
    function get_powerState():Integer;

    ////
    /// <summary>
    ///   Returns the selected power source for module functions that require lots of current.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YDualPower.POWERCONTROL_AUTO</c>, <c>YDualPower.POWERCONTROL_FROM_USB</c>,
    ///   <c>YDualPower.POWERCONTROL_FROM_EXT</c> and <c>YDualPower.POWERCONTROL_OFF</c> corresponding to the
    ///   selected power source for module functions that require lots of current
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDualPower.POWERCONTROL_INVALID</c>.
    /// </para>
    ///-
    function get_powerControl():Integer;

    ////
    /// <summary>
    ///   Changes the selected power source for module functions that require lots of current.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YDualPower.POWERCONTROL_AUTO</c>, <c>YDualPower.POWERCONTROL_FROM_USB</c>,
    ///   <c>YDualPower.POWERCONTROL_FROM_EXT</c> and <c>YDualPower.POWERCONTROL_OFF</c> corresponding to the
    ///   selected power source for module functions that require lots of current
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
    function set_powerControl(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the measured voltage on the external power source, in millivolts.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the measured voltage on the external power source, in millivolts
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDualPower.EXTVOLTAGE_INVALID</c>.
    /// </para>
    ///-
    function get_extVoltage():LongInt;

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
    ///   Use the method <c>YDualPower.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YDualPower</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindDualPower(func: string):TYDualPower;

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
    function registerValueCallback(callback: TYDualPowerValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of dual power switches started using <c>yFirstDualPower()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned dual power switches order.
    ///   If you want to find a specific a dual power switch, use <c>DualPower.findDualPower()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YDualPower</c> object, corresponding to
    ///   a dual power switch currently online, or a <c>NIL</c> pointer
    ///   if there are no more dual power switches to enumerate.
    /// </returns>
    ///-
    function nextDualPower():TYDualPower;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstDualPower():TYDualPower;
  //--- (end of YDualPower accessors declaration)
  end;

//--- (YDualPower functions declaration)
  ////
  /// <summary>
  ///   Retrieves a dual power switch for a given identifier.
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
  ///   This function does not require that the dual power switch is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YDualPower.isOnline()</c> to test if the dual power switch is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a dual power switch by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the dual power switch, for instance
  ///   <c>SERVORC1.dualPower</c>.
  /// </param>
  /// <returns>
  ///   a <c>YDualPower</c> object allowing you to drive the dual power switch.
  /// </returns>
  ///-
  function yFindDualPower(func:string):TYDualPower;
  ////
  /// <summary>
  ///   Starts the enumeration of dual power switches currently accessible.
  /// <para>
  ///   Use the method <c>YDualPower.nextDualPower()</c> to iterate on
  ///   next dual power switches.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YDualPower</c> object, corresponding to
  ///   the first dual power switch currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstDualPower():TYDualPower;

//--- (end of YDualPower functions declaration)

implementation
//--- (YDualPower dlldef)
//--- (end of YDualPower dlldef)

  constructor TYDualPower.Create(func:string);
    begin
      inherited Create(func);
      _className := 'DualPower';
      //--- (YDualPower accessors initialization)
      _powerState := Y_POWERSTATE_INVALID;
      _powerControl := Y_POWERCONTROL_INVALID;
      _extVoltage := Y_EXTVOLTAGE_INVALID;
      _valueCallbackDualPower := nil;
      //--- (end of YDualPower accessors initialization)
    end;

//--- (YDualPower yapiwrapper)
//--- (end of YDualPower yapiwrapper)

//--- (YDualPower implementation)
{$HINTS OFF}
  function TYDualPower._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'powerState') then
        begin
          _powerState := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'powerControl') then
        begin
          _powerControl := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'extVoltage') then
        begin
          _extVoltage := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYDualPower.get_powerState():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_POWERSTATE_INVALID;
              exit;
            end;
        end;
      res := self._powerState;
      result := res;
      exit;
    end;


  function TYDualPower.get_powerControl():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_POWERCONTROL_INVALID;
              exit;
            end;
        end;
      res := self._powerControl;
      result := res;
      exit;
    end;


  function TYDualPower.set_powerControl(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('powerControl',rest_val);
    end;

  function TYDualPower.get_extVoltage():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_EXTVOLTAGE_INVALID;
              exit;
            end;
        end;
      res := self._extVoltage;
      result := res;
      exit;
    end;


  class function TYDualPower.FindDualPower(func: string):TYDualPower;
    var
      obj : TYDualPower;
    begin
      obj := TYDualPower(TYFunction._FindFromCache('DualPower', func));
      if obj = nil then
        begin
          obj :=  TYDualPower.create(func);
          TYFunction._AddToCache('DualPower',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYDualPower.registerValueCallback(callback: TYDualPowerValueCallback):LongInt;
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
      self._valueCallbackDualPower := callback;
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


  function TYDualPower._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackDualPower) <> nil) then
        begin
          self._valueCallbackDualPower(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYDualPower.nextDualPower(): TYDualPower;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextDualPower := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextDualPower := nil;
          exit;
        end;
      nextDualPower := TYDualPower.FindDualPower(hwid);
    end;

  class function TYDualPower.FirstDualPower(): TYDualPower;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('DualPower', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYDualPower.FindDualPower(serial+'.'+funcId);
    end;

//--- (end of YDualPower implementation)

//--- (YDualPower functions)

  function yFindDualPower(func:string): TYDualPower;
    begin
      result := TYDualPower.FindDualPower(func);
    end;

  function yFirstDualPower(): TYDualPower;
    begin
      result := TYDualPower.FirstDualPower();
    end;

  procedure _DualPowerCleanup();
    begin
    end;

//--- (end of YDualPower functions)

initialization
  //--- (YDualPower initialization)
  //--- (end of YDualPower initialization)

finalization
  //--- (YDualPower cleanup)
  _DualPowerCleanup();
  //--- (end of YDualPower cleanup)
end.
