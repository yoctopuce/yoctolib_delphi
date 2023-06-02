{*********************************************************************
 *
 *  $Id: yocto_powersupply.pas 54768 2023-05-26 06:46:41Z seb $
 *
 *  Implements yFindPowerSupply(), the high-level API for PowerSupply functions
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


unit yocto_powersupply;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YPowerSupply definitions)

const Y_VOLTAGESETPOINT_INVALID       = YAPI_INVALID_DOUBLE;
const Y_CURRENTLIMIT_INVALID          = YAPI_INVALID_DOUBLE;
const Y_POWEROUTPUT_OFF = 0;
const Y_POWEROUTPUT_ON = 1;
const Y_POWEROUTPUT_INVALID = -1;
const Y_MEASUREDVOLTAGE_INVALID       = YAPI_INVALID_DOUBLE;
const Y_MEASUREDCURRENT_INVALID       = YAPI_INVALID_DOUBLE;
const Y_INPUTVOLTAGE_INVALID          = YAPI_INVALID_DOUBLE;
const Y_VOLTAGETRANSITION_INVALID     = YAPI_INVALID_STRING;
const Y_VOLTAGEATSTARTUP_INVALID      = YAPI_INVALID_DOUBLE;
const Y_CURRENTATSTARTUP_INVALID      = YAPI_INVALID_DOUBLE;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YPowerSupply definitions)
//--- (YPowerSupply yapiwrapper declaration)
//--- (end of YPowerSupply yapiwrapper declaration)

type
  TYPowerSupply = class;
  //--- (YPowerSupply class start)
  TYPowerSupplyValueCallback = procedure(func: TYPowerSupply; value:string);
  TYPowerSupplyTimedReportCallback = procedure(func: TYPowerSupply; value:TYMeasure);

  ////
  /// <summary>
  ///   TYPowerSupply Class: regulated power supply control interface
  /// <para>
  ///   The <c>YPowerSupply</c> class allows you to drive a Yoctopuce power supply.
  ///   It can be use to change the voltage set point,
  ///   the current limit and the enable/disable the output.
  /// </para>
  /// </summary>
  ///-
  TYPowerSupply=class(TYFunction)
  //--- (end of YPowerSupply class start)
  protected
  //--- (YPowerSupply declaration)
    // Attributes (function value cache)
    _voltageSetPoint          : double;
    _currentLimit             : double;
    _powerOutput              : Integer;
    _measuredVoltage          : double;
    _measuredCurrent          : double;
    _inputVoltage             : double;
    _voltageTransition        : string;
    _voltageAtStartUp         : double;
    _currentAtStartUp         : double;
    _command                  : string;
    _valueCallbackPowerSupply : TYPowerSupplyValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YPowerSupply declaration)

  public
    //--- (YPowerSupply accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the voltage set point, in V.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the voltage set point, in V
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
    function set_voltageSetPoint(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the voltage set point, in V.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the voltage set point, in V
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPowerSupply.VOLTAGESETPOINT_INVALID</c>.
    /// </para>
    ///-
    function get_voltageSetPoint():double;

    ////
    /// <summary>
    ///   Changes the current limit, in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the current limit, in mA
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
    function set_currentLimit(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the current limit, in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current limit, in mA
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPowerSupply.CURRENTLIMIT_INVALID</c>.
    /// </para>
    ///-
    function get_currentLimit():double;

    ////
    /// <summary>
    ///   Returns the power supply output switch state.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YPowerSupply.POWEROUTPUT_OFF</c> or <c>YPowerSupply.POWEROUTPUT_ON</c>, according to the
    ///   power supply output switch state
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPowerSupply.POWEROUTPUT_INVALID</c>.
    /// </para>
    ///-
    function get_powerOutput():Integer;

    ////
    /// <summary>
    ///   Changes the power supply output switch state.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YPowerSupply.POWEROUTPUT_OFF</c> or <c>YPowerSupply.POWEROUTPUT_ON</c>, according to the
    ///   power supply output switch state
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
    function set_powerOutput(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the measured output voltage, in V.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the measured output voltage, in V
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPowerSupply.MEASUREDVOLTAGE_INVALID</c>.
    /// </para>
    ///-
    function get_measuredVoltage():double;

    ////
    /// <summary>
    ///   Returns the measured output current, in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the measured output current, in mA
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPowerSupply.MEASUREDCURRENT_INVALID</c>.
    /// </para>
    ///-
    function get_measuredCurrent():double;

    ////
    /// <summary>
    ///   Returns the measured input voltage, in V.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the measured input voltage, in V
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPowerSupply.INPUTVOLTAGE_INVALID</c>.
    /// </para>
    ///-
    function get_inputVoltage():double;

    function get_voltageTransition():string;

    function set_voltageTransition(newval:string):integer;

    ////
    /// <summary>
    ///   Changes the voltage set point at device start up.
    /// <para>
    ///   Remember to call the matching
    ///   module <c>saveToFlash()</c> method, otherwise this call has no effect.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the voltage set point at device start up
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
    function set_voltageAtStartUp(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the selected voltage set point at device startup, in V.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the selected voltage set point at device startup, in V
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPowerSupply.VOLTAGEATSTARTUP_INVALID</c>.
    /// </para>
    ///-
    function get_voltageAtStartUp():double;

    ////
    /// <summary>
    ///   Changes the current limit at device start up.
    /// <para>
    ///   Remember to call the matching
    ///   module <c>saveToFlash()</c> method, otherwise this call has no effect.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the current limit at device start up
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
    function set_currentAtStartUp(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the selected current limit at device startup, in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the selected current limit at device startup, in mA
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPowerSupply.CURRENTATSTARTUP_INVALID</c>.
    /// </para>
    ///-
    function get_currentAtStartUp():double;

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
    ///   Use the method <c>YPowerSupply.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YPowerSupply</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindPowerSupply(func: string):TYPowerSupply;

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
    function registerValueCallback(callback: TYPowerSupplyValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Performs a smooth transition of output voltage.
    /// <para>
    ///   Any explicit voltage
    ///   change cancels any ongoing transition process.
    /// </para>
    /// </summary>
    /// <param name="V_target">
    ///   new output voltage value at the end of the transition
    ///   (floating-point number, representing the end voltage in V)
    /// </param>
    /// <param name="ms_duration">
    ///   total duration of the transition, in milliseconds
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    ///-
    function voltageMove(V_target: double; ms_duration: LongInt):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of regulated power supplies started using <c>yFirstPowerSupply()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned regulated power supplies order.
    ///   If you want to find a specific a regulated power supply, use <c>PowerSupply.findPowerSupply()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YPowerSupply</c> object, corresponding to
    ///   a regulated power supply currently online, or a <c>NIL</c> pointer
    ///   if there are no more regulated power supplies to enumerate.
    /// </returns>
    ///-
    function nextPowerSupply():TYPowerSupply;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstPowerSupply():TYPowerSupply;
  //--- (end of YPowerSupply accessors declaration)
  end;

//--- (YPowerSupply functions declaration)
  ////
  /// <summary>
  ///   Retrieves a regulated power supply for a given identifier.
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
  ///   This function does not require that the regulated power supply is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YPowerSupply.isOnline()</c> to test if the regulated power supply is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a regulated power supply by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the regulated power supply, for instance
  ///   <c>MyDevice.powerSupply</c>.
  /// </param>
  /// <returns>
  ///   a <c>YPowerSupply</c> object allowing you to drive the regulated power supply.
  /// </returns>
  ///-
  function yFindPowerSupply(func:string):TYPowerSupply;
  ////
  /// <summary>
  ///   Starts the enumeration of regulated power supplies currently accessible.
  /// <para>
  ///   Use the method <c>YPowerSupply.nextPowerSupply()</c> to iterate on
  ///   next regulated power supplies.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YPowerSupply</c> object, corresponding to
  ///   the first regulated power supply currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstPowerSupply():TYPowerSupply;

//--- (end of YPowerSupply functions declaration)

implementation
//--- (YPowerSupply dlldef)
//--- (end of YPowerSupply dlldef)

  constructor TYPowerSupply.Create(func:string);
    begin
      inherited Create(func);
      _className := 'PowerSupply';
      //--- (YPowerSupply accessors initialization)
      _voltageSetPoint := Y_VOLTAGESETPOINT_INVALID;
      _currentLimit := Y_CURRENTLIMIT_INVALID;
      _powerOutput := Y_POWEROUTPUT_INVALID;
      _measuredVoltage := Y_MEASUREDVOLTAGE_INVALID;
      _measuredCurrent := Y_MEASUREDCURRENT_INVALID;
      _inputVoltage := Y_INPUTVOLTAGE_INVALID;
      _voltageTransition := Y_VOLTAGETRANSITION_INVALID;
      _voltageAtStartUp := Y_VOLTAGEATSTARTUP_INVALID;
      _currentAtStartUp := Y_CURRENTATSTARTUP_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackPowerSupply := nil;
      //--- (end of YPowerSupply accessors initialization)
    end;

//--- (YPowerSupply yapiwrapper)
//--- (end of YPowerSupply yapiwrapper)

//--- (YPowerSupply implementation)
{$HINTS OFF}
  function TYPowerSupply._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'voltageSetPoint') then
        begin
          _voltageSetPoint := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'currentLimit') then
        begin
          _currentLimit := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'powerOutput') then
        begin
          _powerOutput := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'measuredVoltage') then
        begin
          _measuredVoltage := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'measuredCurrent') then
        begin
          _measuredCurrent := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'inputVoltage') then
        begin
          _inputVoltage := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'voltageTransition') then
        begin
          _voltageTransition := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'voltageAtStartUp') then
        begin
          _voltageAtStartUp := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'currentAtStartUp') then
        begin
          _currentAtStartUp := round(member^.ivalue / 65.536) / 1000.0;
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

  function TYPowerSupply.set_voltageSetPoint(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('voltageSetPoint',rest_val);
    end;

  function TYPowerSupply.get_voltageSetPoint():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_VOLTAGESETPOINT_INVALID;
              exit;
            end;
        end;
      res := self._voltageSetPoint;
      result := res;
      exit;
    end;


  function TYPowerSupply.set_currentLimit(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('currentLimit',rest_val);
    end;

  function TYPowerSupply.get_currentLimit():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTLIMIT_INVALID;
              exit;
            end;
        end;
      res := self._currentLimit;
      result := res;
      exit;
    end;


  function TYPowerSupply.get_powerOutput():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_POWEROUTPUT_INVALID;
              exit;
            end;
        end;
      res := self._powerOutput;
      result := res;
      exit;
    end;


  function TYPowerSupply.set_powerOutput(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('powerOutput',rest_val);
    end;

  function TYPowerSupply.get_measuredVoltage():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MEASUREDVOLTAGE_INVALID;
              exit;
            end;
        end;
      res := self._measuredVoltage;
      result := res;
      exit;
    end;


  function TYPowerSupply.get_measuredCurrent():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MEASUREDCURRENT_INVALID;
              exit;
            end;
        end;
      res := self._measuredCurrent;
      result := res;
      exit;
    end;


  function TYPowerSupply.get_inputVoltage():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_INPUTVOLTAGE_INVALID;
              exit;
            end;
        end;
      res := self._inputVoltage;
      result := res;
      exit;
    end;


  function TYPowerSupply.get_voltageTransition():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_VOLTAGETRANSITION_INVALID;
              exit;
            end;
        end;
      res := self._voltageTransition;
      result := res;
      exit;
    end;


  function TYPowerSupply.set_voltageTransition(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('voltageTransition',rest_val);
    end;

  function TYPowerSupply.set_voltageAtStartUp(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('voltageAtStartUp',rest_val);
    end;

  function TYPowerSupply.get_voltageAtStartUp():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_VOLTAGEATSTARTUP_INVALID;
              exit;
            end;
        end;
      res := self._voltageAtStartUp;
      result := res;
      exit;
    end;


  function TYPowerSupply.set_currentAtStartUp(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('currentAtStartUp',rest_val);
    end;

  function TYPowerSupply.get_currentAtStartUp():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTATSTARTUP_INVALID;
              exit;
            end;
        end;
      res := self._currentAtStartUp;
      result := res;
      exit;
    end;


  function TYPowerSupply.get_command():string;
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


  function TYPowerSupply.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYPowerSupply.FindPowerSupply(func: string):TYPowerSupply;
    var
      obj : TYPowerSupply;
    begin
      obj := TYPowerSupply(TYFunction._FindFromCache('PowerSupply', func));
      if obj = nil then
        begin
          obj :=  TYPowerSupply.create(func);
          TYFunction._AddToCache('PowerSupply',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYPowerSupply.registerValueCallback(callback: TYPowerSupplyValueCallback):LongInt;
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
      self._valueCallbackPowerSupply := callback;
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


  function TYPowerSupply._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackPowerSupply) <> nil) then
        begin
          self._valueCallbackPowerSupply(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYPowerSupply.voltageMove(V_target: double; ms_duration: LongInt):LongInt;
    var
      newval : string;
    begin
      if V_target < 0.0 then
        begin
          V_target  := 0.0;
        end;
      newval := ''+inttostr( round(V_target*65536))+':'+inttostr(ms_duration);

      result := self.set_voltageTransition(newval);
      exit;
    end;


  function TYPowerSupply.nextPowerSupply(): TYPowerSupply;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextPowerSupply := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextPowerSupply := nil;
          exit;
        end;
      nextPowerSupply := TYPowerSupply.FindPowerSupply(hwid);
    end;

  class function TYPowerSupply.FirstPowerSupply(): TYPowerSupply;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('PowerSupply', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYPowerSupply.FindPowerSupply(serial+'.'+funcId);
    end;

//--- (end of YPowerSupply implementation)

//--- (YPowerSupply functions)

  function yFindPowerSupply(func:string): TYPowerSupply;
    begin
      result := TYPowerSupply.FindPowerSupply(func);
    end;

  function yFirstPowerSupply(): TYPowerSupply;
    begin
      result := TYPowerSupply.FirstPowerSupply();
    end;

  procedure _PowerSupplyCleanup();
    begin
    end;

//--- (end of YPowerSupply functions)

initialization
  //--- (YPowerSupply initialization)
  //--- (end of YPowerSupply initialization)

finalization
  //--- (YPowerSupply cleanup)
  _PowerSupplyCleanup();
  //--- (end of YPowerSupply cleanup)
end.
