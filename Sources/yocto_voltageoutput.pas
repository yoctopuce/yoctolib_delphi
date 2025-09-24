{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindVoltageOutput(), the high-level API for VoltageOutput functions
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


unit yocto_voltageoutput;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YVoltageOutput definitions)

const Y_CURRENTVOLTAGE_INVALID        = YAPI_INVALID_DOUBLE;
const Y_VOLTAGETRANSITION_INVALID     = YAPI_INVALID_STRING;
const Y_VOLTAGEATSTARTUP_INVALID      = YAPI_INVALID_DOUBLE;

//--- (end of YVoltageOutput definitions)

//--- (YVoltageOutput yapiwrapper declaration)
//--- (end of YVoltageOutput yapiwrapper declaration)

type

  TYVoltageOutput = class;
  //--- (YVoltageOutput class start)
  TYVoltageOutputValueCallback = procedure(func: TYVoltageOutput; value:string);
  TYVoltageOutputTimedReportCallback = procedure(func: TYVoltageOutput; value:TYMeasure);

  ////
  /// <summary>
  ///   TYVoltageOutput Class: voltage output control interface, available for instance in the Yocto-0-10V-Tx
  /// <para>
  ///   The <c>YVoltageOutput</c> class allows you to drive a voltage output.
  /// </para>
  /// </summary>
  ///-
  TYVoltageOutput=class(TYFunction)
  //--- (end of YVoltageOutput class start)
  protected
  //--- (YVoltageOutput declaration)
    // Attributes (function value cache)
    _currentVoltage           : double;
    _voltageTransition        : string;
    _voltageAtStartUp         : double;
    _valueCallbackVoltageOutput : TYVoltageOutputValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YVoltageOutput declaration)

  public
    //--- (YVoltageOutput accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the output voltage, in V.
    /// <para>
    ///   Valid range is from 0 to 10V.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the output voltage, in V
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
    function set_currentVoltage(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the output voltage set point, in V.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the output voltage set point, in V
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YVoltageOutput.CURRENTVOLTAGE_INVALID</c>.
    /// </para>
    ///-
    function get_currentVoltage():double;

    function get_voltageTransition():string;

    function set_voltageTransition(newval:string):integer;

    ////
    /// <summary>
    ///   Changes the output voltage at device start up.
    /// <para>
    ///   Remember to call the matching
    ///   module <c>saveToFlash()</c> method, otherwise this call has no effect.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the output voltage at device start up
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
    ///   Returns the selected voltage output at device startup, in V.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the selected voltage output at device startup, in V
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YVoltageOutput.VOLTAGEATSTARTUP_INVALID</c>.
    /// </para>
    ///-
    function get_voltageAtStartUp():double;

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
    ///   Use the method <c>YVoltageOutput.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YVoltageOutput</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindVoltageOutput(func: string):TYVoltageOutput;

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
    function registerValueCallback(callback: TYVoltageOutputValueCallback):LongInt; overload;

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
    ///   Continues the enumeration of voltage outputs started using <c>yFirstVoltageOutput()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned voltage outputs order.
    ///   If you want to find a specific a voltage output, use <c>VoltageOutput.findVoltageOutput()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YVoltageOutput</c> object, corresponding to
    ///   a voltage output currently online, or a <c>NIL</c> pointer
    ///   if there are no more voltage outputs to enumerate.
    /// </returns>
    ///-
    function nextVoltageOutput():TYVoltageOutput;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstVoltageOutput():TYVoltageOutput;
  //--- (end of YVoltageOutput accessors declaration)
  end;

//--- (YVoltageOutput functions declaration)
  ////
  /// <summary>
  ///   Retrieves a voltage output for a given identifier.
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
  ///   This function does not require that the voltage output is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YVoltageOutput.isOnline()</c> to test if the voltage output is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a voltage output by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the voltage output, for instance
  ///   <c>TX010V01.voltageOutput1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YVoltageOutput</c> object allowing you to drive the voltage output.
  /// </returns>
  ///-
  function yFindVoltageOutput(func:string):TYVoltageOutput;
  ////
  /// <summary>
  ///   Starts the enumeration of voltage outputs currently accessible.
  /// <para>
  ///   Use the method <c>YVoltageOutput.nextVoltageOutput()</c> to iterate on
  ///   next voltage outputs.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YVoltageOutput</c> object, corresponding to
  ///   the first voltage output currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstVoltageOutput():TYVoltageOutput;

//--- (end of YVoltageOutput functions declaration)

implementation

//--- (YVoltageOutput dlldef)
//--- (end of YVoltageOutput dlldef)

  constructor TYVoltageOutput.Create(func:string);
    begin
      inherited Create(func);
      _className := 'VoltageOutput';
      //--- (YVoltageOutput accessors initialization)
      _currentVoltage := Y_CURRENTVOLTAGE_INVALID;
      _voltageTransition := Y_VOLTAGETRANSITION_INVALID;
      _voltageAtStartUp := Y_VOLTAGEATSTARTUP_INVALID;
      _valueCallbackVoltageOutput := nil;
      //--- (end of YVoltageOutput accessors initialization)
    end;

//--- (YVoltageOutput yapiwrapper)
//--- (end of YVoltageOutput yapiwrapper)

//--- (YVoltageOutput implementation)
{$HINTS OFF}
  function TYVoltageOutput._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'currentVoltage') then
        begin
          _currentVoltage := round(member^.ivalue / 65.536) / 1000.0;
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
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYVoltageOutput.set_currentVoltage(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('currentVoltage',rest_val);
    end;

  function TYVoltageOutput.get_currentVoltage():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTVOLTAGE_INVALID;
              exit;
            end;
        end;
      res := self._currentVoltage;
      result := res;
      exit;
    end;


  function TYVoltageOutput.get_voltageTransition():string;
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


  function TYVoltageOutput.set_voltageTransition(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('voltageTransition',rest_val);
    end;

  function TYVoltageOutput.set_voltageAtStartUp(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('voltageAtStartUp',rest_val);
    end;

  function TYVoltageOutput.get_voltageAtStartUp():double;
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


  class function TYVoltageOutput.FindVoltageOutput(func: string):TYVoltageOutput;
    var
      obj : TYVoltageOutput;
    begin
      obj := TYVoltageOutput(TYFunction._FindFromCache('VoltageOutput', func));
      if (obj = nil) then
        begin
          obj :=  TYVoltageOutput.create(func);
          TYFunction._AddToCache('VoltageOutput', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYVoltageOutput.registerValueCallback(callback: TYVoltageOutputValueCallback):LongInt;
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
      self._valueCallbackVoltageOutput := callback;
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


  function TYVoltageOutput._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackVoltageOutput) <> nil) then
        begin
          self._valueCallbackVoltageOutput(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYVoltageOutput.voltageMove(V_target: double; ms_duration: LongInt):LongInt;
    var
      newval : string;
    begin
      if V_target < 0.0 then
        begin
          V_target  := 0.0;
        end;
      if V_target > 10.0 then
        begin
          V_target := 10.0;
        end;
      newval := ''+inttostr(LongInt(round(V_target*65536)))+':'+inttostr(ms_duration);

      result := self.set_voltageTransition(newval);
      exit;
    end;


  function TYVoltageOutput.nextVoltageOutput(): TYVoltageOutput;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextVoltageOutput := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextVoltageOutput := nil;
          exit;
        end;
      nextVoltageOutput := TYVoltageOutput.FindVoltageOutput(hwid);
    end;

  class function TYVoltageOutput.FirstVoltageOutput(): TYVoltageOutput;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('VoltageOutput', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYVoltageOutput.FindVoltageOutput(serial+'.'+funcId);
    end;

//--- (end of YVoltageOutput implementation)

//--- (YVoltageOutput functions)

  function yFindVoltageOutput(func:string): TYVoltageOutput;
    begin
      result := TYVoltageOutput.FindVoltageOutput(func);
    end;

  function yFirstVoltageOutput(): TYVoltageOutput;
    begin
      result := TYVoltageOutput.FirstVoltageOutput();
    end;

  procedure _VoltageOutputCleanup();
    begin
    end;

//--- (end of YVoltageOutput functions)

initialization
  //--- (YVoltageOutput initialization)
  //--- (end of YVoltageOutput initialization)

finalization
  //--- (YVoltageOutput cleanup)
  _VoltageOutputCleanup();
  //--- (end of YVoltageOutput cleanup)

end.
