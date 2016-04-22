{*********************************************************************
 *
 * $Id: yocto_poweroutput.pas 23240 2016-02-23 14:10:10Z seb $
 *
 * Implements yFindPowerOutput(), the high-level API for PowerOutput functions
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


unit yocto_poweroutput;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YPowerOutput definitions)

const Y_VOLTAGE_OFF = 0;
const Y_VOLTAGE_OUT3V3 = 1;
const Y_VOLTAGE_OUT5V = 2;
const Y_VOLTAGE_INVALID = -1;


//--- (end of YPowerOutput definitions)

type
  TYPowerOutput = class;
  //--- (YPowerOutput class start)
  TYPowerOutputValueCallback = procedure(func: TYPowerOutput; value:string);
  TYPowerOutputTimedReportCallback = procedure(func: TYPowerOutput; value:TYMeasure);

  ////
  /// <summary>
  ///   TYPowerOutput Class: External power supply control interface
  /// <para>
  ///   Yoctopuce application programming interface allows you to control
  ///   the power ouput featured on some devices such as the Yocto-Serial.
  /// </para>
  /// </summary>
  ///-
  TYPowerOutput=class(TYFunction)
  //--- (end of YPowerOutput class start)
  protected
  //--- (YPowerOutput declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _voltage                  : Integer;
    _valueCallbackPowerOutput : TYPowerOutputValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YPowerOutput declaration)

  public
    //--- (YPowerOutput accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the voltage on the power ouput featured by
    ///   the module.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_VOLTAGE_OFF</c>, <c>Y_VOLTAGE_OUT3V3</c> and <c>Y_VOLTAGE_OUT5V</c>
    ///   corresponding to the voltage on the power ouput featured by
    ///   the module
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_VOLTAGE_INVALID</c>.
    /// </para>
    ///-
    function get_voltage():Integer;

    ////
    /// <summary>
    ///   Changes the voltage on the power output provided by the
    ///   module.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_VOLTAGE_OFF</c>, <c>Y_VOLTAGE_OUT3V3</c> and <c>Y_VOLTAGE_OUT5V</c>
    ///   corresponding to the voltage on the power output provided by the
    ///   module
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
    function set_voltage(newval:Integer):integer;

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
    ///   Use the method <c>YPowerOutput.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YPowerOutput</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindPowerOutput(func: string):TYPowerOutput;

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
    function registerValueCallback(callback: TYPowerOutputValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of dual power ouput controls started using <c>yFirstPowerOutput()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YPowerOutput</c> object, corresponding to
    ///   a dual power  ouput control currently online, or a <c>null</c> pointer
    ///   if there are no more dual power ouput controls to enumerate.
    /// </returns>
    ///-
    function nextPowerOutput():TYPowerOutput;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstPowerOutput():TYPowerOutput;
  //--- (end of YPowerOutput accessors declaration)
  end;

//--- (PowerOutput functions declaration)
  ////
  /// <summary>
  ///   Retrieves a dual power  ouput control for a given identifier.
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
  ///   This function does not require that the power ouput control is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YPowerOutput.isOnline()</c> to test if the power ouput control is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a dual power  ouput control by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the power ouput control
  /// </param>
  /// <returns>
  ///   a <c>YPowerOutput</c> object allowing you to drive the power ouput control.
  /// </returns>
  ///-
  function yFindPowerOutput(func:string):TYPowerOutput;
  ////
  /// <summary>
  ///   Starts the enumeration of dual power ouput controls currently accessible.
  /// <para>
  ///   Use the method <c>YPowerOutput.nextPowerOutput()</c> to iterate on
  ///   next dual power ouput controls.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YPowerOutput</c> object, corresponding to
  ///   the first dual power ouput control currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstPowerOutput():TYPowerOutput;

//--- (end of PowerOutput functions declaration)

implementation
//--- (YPowerOutput dlldef)
//--- (end of YPowerOutput dlldef)

  constructor TYPowerOutput.Create(func:string);
    begin
      inherited Create(func);
      _className := 'PowerOutput';
      //--- (YPowerOutput accessors initialization)
      _voltage := Y_VOLTAGE_INVALID;
      _valueCallbackPowerOutput := nil;
      //--- (end of YPowerOutput accessors initialization)
    end;


//--- (YPowerOutput implementation)
{$HINTS OFF}
  function TYPowerOutput._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'voltage') then
        begin
          _voltage := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Returns the voltage on the power ouput featured by
  ///   the module.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_VOLTAGE_OFF, Y_VOLTAGE_OUT3V3 and Y_VOLTAGE_OUT5V corresponding to the voltage on
  ///   the power ouput featured by
  ///   the module
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_VOLTAGE_INVALID.
  /// </para>
  ///-
  function TYPowerOutput.get_voltage():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_VOLTAGE_INVALID;
              exit;
            end;
        end;
      result := self._voltage;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the voltage on the power output provided by the
  ///   module.
  /// <para>
  ///   Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_VOLTAGE_OFF, Y_VOLTAGE_OUT3V3 and Y_VOLTAGE_OUT5V corresponding to the voltage on
  ///   the power output provided by the
  ///   module
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
  function TYPowerOutput.set_voltage(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('voltage',rest_val);
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
  ///   Use the method <c>YPowerOutput.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YPowerOutput</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYPowerOutput.FindPowerOutput(func: string):TYPowerOutput;
    var
      obj : TYPowerOutput;
    begin
      obj := TYPowerOutput(TYFunction._FindFromCache('PowerOutput', func));
      if obj = nil then
        begin
          obj :=  TYPowerOutput.create(func);
          TYFunction._AddToCache('PowerOutput',  func, obj);
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
  function TYPowerOutput.registerValueCallback(callback: TYPowerOutputValueCallback):LongInt;
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
      self._valueCallbackPowerOutput := callback;
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


  function TYPowerOutput._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackPowerOutput) <> nil) then
        begin
          self._valueCallbackPowerOutput(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYPowerOutput.nextPowerOutput(): TYPowerOutput;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextPowerOutput := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextPowerOutput := nil;
          exit;
        end;
      nextPowerOutput := TYPowerOutput.FindPowerOutput(hwid);
    end;

  class function TYPowerOutput.FirstPowerOutput(): TYPowerOutput;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('PowerOutput', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYPowerOutput.FindPowerOutput(serial+'.'+funcId);
    end;

//--- (end of YPowerOutput implementation)

//--- (PowerOutput functions)

  function yFindPowerOutput(func:string): TYPowerOutput;
    begin
      result := TYPowerOutput.FindPowerOutput(func);
    end;

  function yFirstPowerOutput(): TYPowerOutput;
    begin
      result := TYPowerOutput.FirstPowerOutput();
    end;

  procedure _PowerOutputCleanup();
    begin
    end;

//--- (end of PowerOutput functions)

initialization
  //--- (PowerOutput initialization)
  //--- (end of PowerOutput initialization)

finalization
  //--- (PowerOutput cleanup)
  _PowerOutputCleanup();
  //--- (end of PowerOutput cleanup)
end.
