{*********************************************************************
 *
 *  $Id: yocto_currentloopoutput.pas 46894 2021-10-25 15:07:44Z seb $
 *
 *  Implements yFindCurrentLoopOutput(), the high-level API for CurrentLoopOutput functions
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


unit yocto_currentloopoutput;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YCurrentLoopOutput definitions)

const Y_CURRENT_INVALID               = YAPI_INVALID_DOUBLE;
const Y_CURRENTTRANSITION_INVALID     = YAPI_INVALID_STRING;
const Y_CURRENTATSTARTUP_INVALID      = YAPI_INVALID_DOUBLE;
const Y_LOOPPOWER_NOPWR = 0;
const Y_LOOPPOWER_LOWPWR = 1;
const Y_LOOPPOWER_POWEROK = 2;
const Y_LOOPPOWER_INVALID = -1;


//--- (end of YCurrentLoopOutput definitions)
//--- (YCurrentLoopOutput yapiwrapper declaration)
//--- (end of YCurrentLoopOutput yapiwrapper declaration)

type
  TYCurrentLoopOutput = class;
  //--- (YCurrentLoopOutput class start)
  TYCurrentLoopOutputValueCallback = procedure(func: TYCurrentLoopOutput; value:string);
  TYCurrentLoopOutputTimedReportCallback = procedure(func: TYCurrentLoopOutput; value:TYMeasure);

  ////
  /// <summary>
  ///   TYCurrentLoopOutput Class: 4-20mA output control interface, available for instance in the Yocto-4-20mA-Tx
  /// <para>
  ///   The <c>YCurrentLoopOutput</c> class allows you to drive a 4-20mA output
  ///   by regulating the current flowing through the current loop.
  ///   It can also provide information about the power state of the current loop.
  /// </para>
  /// </summary>
  ///-
  TYCurrentLoopOutput=class(TYFunction)
  //--- (end of YCurrentLoopOutput class start)
  protected
  //--- (YCurrentLoopOutput declaration)
    // Attributes (function value cache)
    _current                  : double;
    _currentTransition        : string;
    _currentAtStartUp         : double;
    _loopPower                : Integer;
    _valueCallbackCurrentLoopOutput : TYCurrentLoopOutputValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YCurrentLoopOutput declaration)

  public
    //--- (YCurrentLoopOutput accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the current loop, the valid range is from 3 to 21mA.
    /// <para>
    ///   If the loop is
    ///   not properly powered, the  target current is not reached and
    ///   loopPower is set to LOWPWR.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the current loop, the valid range is from 3 to 21mA
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
    function set_current(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the loop current set point in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the loop current set point in mA
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YCurrentLoopOutput.CURRENT_INVALID</c>.
    /// </para>
    ///-
    function get_current():double;

    function get_currentTransition():string;

    function set_currentTransition(newval:string):integer;

    ////
    /// <summary>
    ///   Changes the loop current at device start up.
    /// <para>
    ///   Remember to call the matching
    ///   module <c>saveToFlash()</c> method, otherwise this call has no effect.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the loop current at device start up
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
    ///   Returns the current in the loop at device startup, in mA.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current in the loop at device startup, in mA
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YCurrentLoopOutput.CURRENTATSTARTUP_INVALID</c>.
    /// </para>
    ///-
    function get_currentAtStartUp():double;

    ////
    /// <summary>
    ///   Returns the loop powerstate.
    /// <para>
    ///   POWEROK: the loop
    ///   is powered. NOPWR: the loop in not powered. LOWPWR: the loop is not
    ///   powered enough to maintain the current required (insufficient voltage).
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YCurrentLoopOutput.LOOPPOWER_NOPWR</c>, <c>YCurrentLoopOutput.LOOPPOWER_LOWPWR</c>
    ///   and <c>YCurrentLoopOutput.LOOPPOWER_POWEROK</c> corresponding to the loop powerstate
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YCurrentLoopOutput.LOOPPOWER_INVALID</c>.
    /// </para>
    ///-
    function get_loopPower():Integer;

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
    ///   Use the method <c>YCurrentLoopOutput.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YCurrentLoopOutput</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindCurrentLoopOutput(func: string):TYCurrentLoopOutput;

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
    function registerValueCallback(callback: TYCurrentLoopOutputValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Performs a smooth transition of current flowing in the loop.
    /// <para>
    ///   Any current explicit
    ///   change cancels any ongoing transition process.
    /// </para>
    /// </summary>
    /// <param name="mA_target">
    ///   new current value at the end of the transition
    ///   (floating-point number, representing the end current in mA)
    /// </param>
    /// <param name="ms_duration">
    ///   total duration of the transition, in milliseconds
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    ///-
    function currentMove(mA_target: double; ms_duration: LongInt):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of 4-20mA outputs started using <c>yFirstCurrentLoopOutput()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned 4-20mA outputs order.
    ///   If you want to find a specific a 4-20mA output, use <c>CurrentLoopOutput.findCurrentLoopOutput()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YCurrentLoopOutput</c> object, corresponding to
    ///   a 4-20mA output currently online, or a <c>NIL</c> pointer
    ///   if there are no more 4-20mA outputs to enumerate.
    /// </returns>
    ///-
    function nextCurrentLoopOutput():TYCurrentLoopOutput;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstCurrentLoopOutput():TYCurrentLoopOutput;
  //--- (end of YCurrentLoopOutput accessors declaration)
  end;

//--- (YCurrentLoopOutput functions declaration)
  ////
  /// <summary>
  ///   Retrieves a 4-20mA output for a given identifier.
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
  ///   This function does not require that the 4-20mA output is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YCurrentLoopOutput.isOnline()</c> to test if the 4-20mA output is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a 4-20mA output by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the 4-20mA output, for instance
  ///   <c>TX420MA1.currentLoopOutput</c>.
  /// </param>
  /// <returns>
  ///   a <c>YCurrentLoopOutput</c> object allowing you to drive the 4-20mA output.
  /// </returns>
  ///-
  function yFindCurrentLoopOutput(func:string):TYCurrentLoopOutput;
  ////
  /// <summary>
  ///   Starts the enumeration of 4-20mA outputs currently accessible.
  /// <para>
  ///   Use the method <c>YCurrentLoopOutput.nextCurrentLoopOutput()</c> to iterate on
  ///   next 4-20mA outputs.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YCurrentLoopOutput</c> object, corresponding to
  ///   the first 4-20mA output currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstCurrentLoopOutput():TYCurrentLoopOutput;

//--- (end of YCurrentLoopOutput functions declaration)

implementation
//--- (YCurrentLoopOutput dlldef)
//--- (end of YCurrentLoopOutput dlldef)

  constructor TYCurrentLoopOutput.Create(func:string);
    begin
      inherited Create(func);
      _className := 'CurrentLoopOutput';
      //--- (YCurrentLoopOutput accessors initialization)
      _current := Y_CURRENT_INVALID;
      _currentTransition := Y_CURRENTTRANSITION_INVALID;
      _currentAtStartUp := Y_CURRENTATSTARTUP_INVALID;
      _loopPower := Y_LOOPPOWER_INVALID;
      _valueCallbackCurrentLoopOutput := nil;
      //--- (end of YCurrentLoopOutput accessors initialization)
    end;

//--- (YCurrentLoopOutput yapiwrapper)
//--- (end of YCurrentLoopOutput yapiwrapper)

//--- (YCurrentLoopOutput implementation)
{$HINTS OFF}
  function TYCurrentLoopOutput._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'current') then
        begin
          _current := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'currentTransition') then
        begin
          _currentTransition := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'currentAtStartUp') then
        begin
          _currentAtStartUp := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'loopPower') then
        begin
          _loopPower := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYCurrentLoopOutput.set_current(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('current',rest_val);
    end;

  function TYCurrentLoopOutput.get_current():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENT_INVALID;
              exit;
            end;
        end;
      res := self._current;
      result := res;
      exit;
    end;


  function TYCurrentLoopOutput.get_currentTransition():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTTRANSITION_INVALID;
              exit;
            end;
        end;
      res := self._currentTransition;
      result := res;
      exit;
    end;


  function TYCurrentLoopOutput.set_currentTransition(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('currentTransition',rest_val);
    end;

  function TYCurrentLoopOutput.set_currentAtStartUp(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('currentAtStartUp',rest_val);
    end;

  function TYCurrentLoopOutput.get_currentAtStartUp():double;
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


  function TYCurrentLoopOutput.get_loopPower():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LOOPPOWER_INVALID;
              exit;
            end;
        end;
      res := self._loopPower;
      result := res;
      exit;
    end;


  class function TYCurrentLoopOutput.FindCurrentLoopOutput(func: string):TYCurrentLoopOutput;
    var
      obj : TYCurrentLoopOutput;
    begin
      obj := TYCurrentLoopOutput(TYFunction._FindFromCache('CurrentLoopOutput', func));
      if obj = nil then
        begin
          obj :=  TYCurrentLoopOutput.create(func);
          TYFunction._AddToCache('CurrentLoopOutput',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYCurrentLoopOutput.registerValueCallback(callback: TYCurrentLoopOutputValueCallback):LongInt;
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
      self._valueCallbackCurrentLoopOutput := callback;
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


  function TYCurrentLoopOutput._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackCurrentLoopOutput) <> nil) then
        begin
          self._valueCallbackCurrentLoopOutput(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYCurrentLoopOutput.currentMove(mA_target: double; ms_duration: LongInt):LongInt;
    var
      newval : string;
    begin
      if mA_target < 3.0 then
        begin
          mA_target  := 3.0;
        end;
      if mA_target > 21.0 then
        begin
          mA_target := 21.0;
        end;
      newval := ''+inttostr( round(mA_target*65536))+':'+inttostr(ms_duration);

      result := self.set_currentTransition(newval);
      exit;
    end;


  function TYCurrentLoopOutput.nextCurrentLoopOutput(): TYCurrentLoopOutput;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextCurrentLoopOutput := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextCurrentLoopOutput := nil;
          exit;
        end;
      nextCurrentLoopOutput := TYCurrentLoopOutput.FindCurrentLoopOutput(hwid);
    end;

  class function TYCurrentLoopOutput.FirstCurrentLoopOutput(): TYCurrentLoopOutput;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('CurrentLoopOutput', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYCurrentLoopOutput.FindCurrentLoopOutput(serial+'.'+funcId);
    end;

//--- (end of YCurrentLoopOutput implementation)

//--- (YCurrentLoopOutput functions)

  function yFindCurrentLoopOutput(func:string): TYCurrentLoopOutput;
    begin
      result := TYCurrentLoopOutput.FindCurrentLoopOutput(func);
    end;

  function yFirstCurrentLoopOutput(): TYCurrentLoopOutput;
    begin
      result := TYCurrentLoopOutput.FirstCurrentLoopOutput();
    end;

  procedure _CurrentLoopOutputCleanup();
    begin
    end;

//--- (end of YCurrentLoopOutput functions)

initialization
  //--- (YCurrentLoopOutput initialization)
  //--- (end of YCurrentLoopOutput initialization)

finalization
  //--- (YCurrentLoopOutput cleanup)
  _CurrentLoopOutputCleanup();
  //--- (end of YCurrentLoopOutput cleanup)
end.
