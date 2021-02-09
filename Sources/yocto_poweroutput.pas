{*********************************************************************
 *
 *  $Id: yocto_poweroutput.pas 43580 2021-01-26 17:46:01Z mvuilleu $
 *
 *  Implements yFindPowerOutput(), the high-level API for PowerOutput functions
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


unit yocto_poweroutput;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YPowerOutput definitions)

const Y_VOLTAGE_OFF = 0;
const Y_VOLTAGE_OUT3V3 = 1;
const Y_VOLTAGE_OUT5V = 2;
const Y_VOLTAGE_OUT4V7 = 3;
const Y_VOLTAGE_OUT1V8 = 4;
const Y_VOLTAGE_INVALID = -1;


//--- (end of YPowerOutput definitions)
//--- (YPowerOutput yapiwrapper declaration)
//--- (end of YPowerOutput yapiwrapper declaration)

type
  TYPowerOutput = class;
  //--- (YPowerOutput class start)
  TYPowerOutputValueCallback = procedure(func: TYPowerOutput; value:string);
  TYPowerOutputTimedReportCallback = procedure(func: TYPowerOutput; value:TYMeasure);

  ////
  /// <summary>
  ///   TYPowerOutput Class: power output control interface, available for instance in the Yocto-I2C, the
  ///   Yocto-MaxiMicroVolt-Rx, the Yocto-SPI or the Yocto-Serial
  /// <para>
  ///   The <c>YPowerOutput</c> class allows you to control
  ///   the power output featured on some Yoctopuce devices.
  /// </para>
  /// </summary>
  ///-
  TYPowerOutput=class(TYFunction)
  //--- (end of YPowerOutput class start)
  protected
  //--- (YPowerOutput declaration)
    // Attributes (function value cache)
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
    ///   Returns the voltage on the power output featured by the module.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YPowerOutput.VOLTAGE_OFF</c>, <c>YPowerOutput.VOLTAGE_OUT3V3</c>,
    ///   <c>YPowerOutput.VOLTAGE_OUT5V</c>, <c>YPowerOutput.VOLTAGE_OUT4V7</c> and
    ///   <c>YPowerOutput.VOLTAGE_OUT1V8</c> corresponding to the voltage on the power output featured by the module
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YPowerOutput.VOLTAGE_INVALID</c>.
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
    ///   a value among <c>YPowerOutput.VOLTAGE_OFF</c>, <c>YPowerOutput.VOLTAGE_OUT3V3</c>,
    ///   <c>YPowerOutput.VOLTAGE_OUT5V</c>, <c>YPowerOutput.VOLTAGE_OUT4V7</c> and
    ///   <c>YPowerOutput.VOLTAGE_OUT1V8</c> corresponding to the voltage on the power output provided by the
    ///   module
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
    function registerValueCallback(callback: TYPowerOutputValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of power output started using <c>yFirstPowerOutput()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned power output order.
    ///   If you want to find a specific a power output, use <c>PowerOutput.findPowerOutput()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YPowerOutput</c> object, corresponding to
    ///   a power output currently online, or a <c>NIL</c> pointer
    ///   if there are no more power output to enumerate.
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

//--- (YPowerOutput functions declaration)
  ////
  /// <summary>
  ///   Retrieves a power output for a given identifier.
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
  ///   This function does not require that the power output is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YPowerOutput.isOnline()</c> to test if the power output is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a power output by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the power output, for instance
  ///   <c>YI2CMK01.powerOutput</c>.
  /// </param>
  /// <returns>
  ///   a <c>YPowerOutput</c> object allowing you to drive the power output.
  /// </returns>
  ///-
  function yFindPowerOutput(func:string):TYPowerOutput;
  ////
  /// <summary>
  ///   Starts the enumeration of power output currently accessible.
  /// <para>
  ///   Use the method <c>YPowerOutput.nextPowerOutput()</c> to iterate on
  ///   next power output.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YPowerOutput</c> object, corresponding to
  ///   the first power output currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstPowerOutput():TYPowerOutput;

//--- (end of YPowerOutput functions declaration)

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

//--- (YPowerOutput yapiwrapper)
//--- (end of YPowerOutput yapiwrapper)

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

  function TYPowerOutput.get_voltage():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_VOLTAGE_INVALID;
              exit;
            end;
        end;
      res := self._voltage;
      result := res;
      exit;
    end;


  function TYPowerOutput.set_voltage(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('voltage',rest_val);
    end;

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

//--- (YPowerOutput functions)

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

//--- (end of YPowerOutput functions)

initialization
  //--- (YPowerOutput initialization)
  //--- (end of YPowerOutput initialization)

finalization
  //--- (YPowerOutput cleanup)
  _PowerOutputCleanup();
  //--- (end of YPowerOutput cleanup)
end.
