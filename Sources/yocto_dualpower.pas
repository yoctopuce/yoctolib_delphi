{*********************************************************************
 *
 * $Id: yocto_dualpower.pas 17350 2014-08-29 08:54:26Z seb $
 *
 * Implements yFindDualPower(), the high-level API for DualPower functions
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


unit yocto_dualpower;

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

type
  TYDualPower = class;
  //--- (YDualPower class start)
  TYDualPowerValueCallback = procedure(func: TYDualPower; value:string);
  TYDualPowerTimedReportCallback = procedure(func: TYDualPower; value:TYMeasure);

  ////
  /// <summary>
  ///   TYDualPower Class: External power supply control interface
  /// <para>
  ///   Yoctopuce application programming interface allows you to control
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
    _logicalName              : string;
    _advertisedValue          : string;
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
    ///   a value among <c>Y_POWERSTATE_OFF</c>, <c>Y_POWERSTATE_FROM_USB</c> and
    ///   <c>Y_POWERSTATE_FROM_EXT</c> corresponding to the current power source for module functions that
    ///   require lots of current
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_POWERSTATE_INVALID</c>.
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
    ///   a value among <c>Y_POWERCONTROL_AUTO</c>, <c>Y_POWERCONTROL_FROM_USB</c>,
    ///   <c>Y_POWERCONTROL_FROM_EXT</c> and <c>Y_POWERCONTROL_OFF</c> corresponding to the selected power
    ///   source for module functions that require lots of current
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_POWERCONTROL_INVALID</c>.
    /// </para>
    ///-
    function get_powerControl():Integer;

    ////
    /// <summary>
    ///   Changes the selected power source for module functions that require lots of current.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_POWERCONTROL_AUTO</c>, <c>Y_POWERCONTROL_FROM_USB</c>,
    ///   <c>Y_POWERCONTROL_FROM_EXT</c> and <c>Y_POWERCONTROL_OFF</c> corresponding to the selected power
    ///   source for module functions that require lots of current
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
    ///   On failure, throws an exception or returns <c>Y_EXTVOLTAGE_INVALID</c>.
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
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$
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
    function registerValueCallback(callback: TYDualPowerValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of dual power controls started using <c>yFirstDualPower()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YDualPower</c> object, corresponding to
    ///   a dual power control currently online, or a <c>null</c> pointer
    ///   if there are no more dual power controls to enumerate.
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

//--- (DualPower functions declaration)
  ////
  /// <summary>
  ///   Retrieves a dual power control for a given identifier.
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
  ///   This function does not require that the power control is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YDualPower.isOnline()</c> to test if the power control is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a dual power control by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the power control
  /// </param>
  /// <returns>
  ///   a <c>YDualPower</c> object allowing you to drive the power control.
  /// </returns>
  ///-
  function yFindDualPower(func:string):TYDualPower;
  ////
  /// <summary>
  ///   Starts the enumeration of dual power controls currently accessible.
  /// <para>
  ///   Use the method <c>YDualPower.nextDualPower()</c> to iterate on
  ///   next dual power controls.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YDualPower</c> object, corresponding to
  ///   the first dual power control currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstDualPower():TYDualPower;

//--- (end of DualPower functions declaration)

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

  ////
  /// <summary>
  ///   Returns the current power source for module functions that require lots of current.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_POWERSTATE_OFF, Y_POWERSTATE_FROM_USB and Y_POWERSTATE_FROM_EXT corresponding to
  ///   the current power source for module functions that require lots of current
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_POWERSTATE_INVALID.
  /// </para>
  ///-
  function TYDualPower.get_powerState():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_POWERSTATE_INVALID;
              exit
            end;
        end;
      result := self._powerState;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the selected power source for module functions that require lots of current.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_POWERCONTROL_AUTO, Y_POWERCONTROL_FROM_USB, Y_POWERCONTROL_FROM_EXT and
  ///   Y_POWERCONTROL_OFF corresponding to the selected power source for module functions that require lots of current
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_POWERCONTROL_INVALID.
  /// </para>
  ///-
  function TYDualPower.get_powerControl():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_POWERCONTROL_INVALID;
              exit
            end;
        end;
      result := self._powerControl;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the selected power source for module functions that require lots of current.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_POWERCONTROL_AUTO, Y_POWERCONTROL_FROM_USB, Y_POWERCONTROL_FROM_EXT and
  ///   Y_POWERCONTROL_OFF corresponding to the selected power source for module functions that require lots of current
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
  function TYDualPower.set_powerControl(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('powerControl',rest_val);
    end;

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
  ///   On failure, throws an exception or returns Y_EXTVOLTAGE_INVALID.
  /// </para>
  ///-
  function TYDualPower.get_extVoltage():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_EXTVOLTAGE_INVALID;
              exit
            end;
        end;
      result := self._extVoltage;
      exit;
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
  ///   Use the method <c>YDualPower.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YDualPower</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYDualPower.FindDualPower(func: string):TYDualPower;
    var
      obj : TYDualPower;
    begin
      obj := TYDualPower(TYFunction._FindFromCache('DualPower', func));
      if obj = nil then
        begin
          obj :=  TYDualPower.create(func);
          TYFunction._AddToCache('DualPower',  func, obj)
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
  function TYDualPower.registerValueCallback(callback: TYDualPowerValueCallback):LongInt;
    var
      val : string;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateValueCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateValueCallbackList(self, false)
        end;
      self._valueCallbackDualPower := callback;
      // Immediately invoke value callback with current value
      if (addr(callback) <> nil) and self.isOnline then
        begin
          val := self._advertisedValue;
          if not((val = '')) then
            begin
              self._invokeValueCallback(val)
            end;
        end;
      result := 0;
      exit;
    end;


  function TYDualPower._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackDualPower) <> nil) then
        begin
          self._valueCallbackDualPower(self, value)
        end
      else
        begin
          inherited _invokeValueCallback(value)
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

//--- (DualPower functions)

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

//--- (end of DualPower functions)

initialization
  //--- (DualPower initialization)
  //--- (end of DualPower initialization)

finalization
  //--- (DualPower cleanup)
  _DualPowerCleanup();
  //--- (end of DualPower cleanup)
end.