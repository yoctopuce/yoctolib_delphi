{*********************************************************************
 *
 * $Id: yocto_bridgecontrol.pas 27017 2017-03-31 14:47:59Z seb $
 *
 * Implements yFindBridgeControl(), the high-level API for BridgeControl functions
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


unit yocto_bridgecontrol;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YBridgeControl definitions)

const Y_EXCITATIONMODE_INTERNAL_AC = 0;
const Y_EXCITATIONMODE_INTERNAL_DC = 1;
const Y_EXCITATIONMODE_EXTERNAL_DC = 2;
const Y_EXCITATIONMODE_INVALID = -1;
const Y_BRIDGELATENCY_INVALID         = YAPI_INVALID_UINT;
const Y_ADVALUE_INVALID               = YAPI_INVALID_INT;
const Y_ADGAIN_INVALID                = YAPI_INVALID_UINT;


//--- (end of YBridgeControl definitions)

type
  TYBridgeControl = class;
  //--- (YBridgeControl class start)
  TYBridgeControlValueCallback = procedure(func: TYBridgeControl; value:string);
  TYBridgeControlTimedReportCallback = procedure(func: TYBridgeControl; value:TYMeasure);

  ////
  /// <summary>
  ///   TYBridgeControl Class: BridgeControl function interface
  /// <para>
  ///   The Yoctopuce class YBridgeControl allows you to control bridge excitation parameters
  ///   and measure parameters for a Wheatstone bridge sensor. To read the measurements, it
  ///   is best to use the GenericSensor calss, which will compute the measured value
  ///   in the optimal way.
  /// </para>
  /// </summary>
  ///-
  TYBridgeControl=class(TYFunction)
  //--- (end of YBridgeControl class start)
  protected
  //--- (YBridgeControl declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _excitationMode           : Integer;
    _bridgeLatency            : LongInt;
    _adValue                  : LongInt;
    _adGain                   : LongInt;
    _valueCallbackBridgeControl : TYBridgeControlValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YBridgeControl declaration)

  public
    //--- (YBridgeControl accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the current Wheatstone bridge excitation method.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_EXCITATIONMODE_INTERNAL_AC</c>, <c>Y_EXCITATIONMODE_INTERNAL_DC</c> and
    ///   <c>Y_EXCITATIONMODE_EXTERNAL_DC</c> corresponding to the current Wheatstone bridge excitation method
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_EXCITATIONMODE_INVALID</c>.
    /// </para>
    ///-
    function get_excitationMode():Integer;

    ////
    /// <summary>
    ///   Changes the current Wheatstone bridge excitation method.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_EXCITATIONMODE_INTERNAL_AC</c>, <c>Y_EXCITATIONMODE_INTERNAL_DC</c> and
    ///   <c>Y_EXCITATIONMODE_EXTERNAL_DC</c> corresponding to the current Wheatstone bridge excitation method
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
    function set_excitationMode(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the current Wheatstone bridge excitation method.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current Wheatstone bridge excitation method
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BRIDGELATENCY_INVALID</c>.
    /// </para>
    ///-
    function get_bridgeLatency():LongInt;

    ////
    /// <summary>
    ///   Changes the current Wheatstone bridge excitation method.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the current Wheatstone bridge excitation method
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
    function set_bridgeLatency(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the raw value returned by the ratiometric A/D converter
    ///   during last read.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the raw value returned by the ratiometric A/D converter
    ///   during last read
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ADVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_adValue():LongInt;

    ////
    /// <summary>
    ///   Returns the current ratiometric A/D converter gain.
    /// <para>
    ///   The gain is automatically
    ///   configured according to the signalRange set in the corresponding genericSensor.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current ratiometric A/D converter gain
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ADGAIN_INVALID</c>.
    /// </para>
    ///-
    function get_adGain():LongInt;

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
    ///   Use the method <c>YBridgeControl.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YBridgeControl</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindBridgeControl(func: string):TYBridgeControl;

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
    function registerValueCallback(callback: TYBridgeControlValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of Wheatstone bridge controllers started using <c>yFirstBridgeControl()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YBridgeControl</c> object, corresponding to
    ///   a Wheatstone bridge controller currently online, or a <c>NIL</c> pointer
    ///   if there are no more Wheatstone bridge controllers to enumerate.
    /// </returns>
    ///-
    function nextBridgeControl():TYBridgeControl;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstBridgeControl():TYBridgeControl;
  //--- (end of YBridgeControl accessors declaration)
  end;

//--- (BridgeControl functions declaration)
  ////
  /// <summary>
  ///   Retrieves a Wheatstone bridge controller for a given identifier.
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
  ///   This function does not require that the Wheatstone bridge controller is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YBridgeControl.isOnline()</c> to test if the Wheatstone bridge controller is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a Wheatstone bridge controller by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the Wheatstone bridge controller
  /// </param>
  /// <returns>
  ///   a <c>YBridgeControl</c> object allowing you to drive the Wheatstone bridge controller.
  /// </returns>
  ///-
  function yFindBridgeControl(func:string):TYBridgeControl;
  ////
  /// <summary>
  ///   Starts the enumeration of Wheatstone bridge controllers currently accessible.
  /// <para>
  ///   Use the method <c>YBridgeControl.nextBridgeControl()</c> to iterate on
  ///   next Wheatstone bridge controllers.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YBridgeControl</c> object, corresponding to
  ///   the first Wheatstone bridge controller currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstBridgeControl():TYBridgeControl;

//--- (end of BridgeControl functions declaration)

implementation
//--- (YBridgeControl dlldef)
//--- (end of YBridgeControl dlldef)

  constructor TYBridgeControl.Create(func:string);
    begin
      inherited Create(func);
      _className := 'BridgeControl';
      //--- (YBridgeControl accessors initialization)
      _excitationMode := Y_EXCITATIONMODE_INVALID;
      _bridgeLatency := Y_BRIDGELATENCY_INVALID;
      _adValue := Y_ADVALUE_INVALID;
      _adGain := Y_ADGAIN_INVALID;
      _valueCallbackBridgeControl := nil;
      //--- (end of YBridgeControl accessors initialization)
    end;


//--- (YBridgeControl implementation)
{$HINTS OFF}
  function TYBridgeControl._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'excitationMode') then
        begin
          _excitationMode := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bridgeLatency') then
        begin
          _bridgeLatency := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'adValue') then
        begin
          _adValue := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'adGain') then
        begin
          _adGain := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Returns the current Wheatstone bridge excitation method.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_EXCITATIONMODE_INTERNAL_AC, Y_EXCITATIONMODE_INTERNAL_DC and
  ///   Y_EXCITATIONMODE_EXTERNAL_DC corresponding to the current Wheatstone bridge excitation method
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_EXCITATIONMODE_INVALID.
  /// </para>
  ///-
  function TYBridgeControl.get_excitationMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_EXCITATIONMODE_INVALID;
              exit;
            end;
        end;
      res := self._excitationMode;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the current Wheatstone bridge excitation method.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_EXCITATIONMODE_INTERNAL_AC, Y_EXCITATIONMODE_INTERNAL_DC and
  ///   Y_EXCITATIONMODE_EXTERNAL_DC corresponding to the current Wheatstone bridge excitation method
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
  function TYBridgeControl.set_excitationMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('excitationMode',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the current Wheatstone bridge excitation method.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the current Wheatstone bridge excitation method
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_BRIDGELATENCY_INVALID.
  /// </para>
  ///-
  function TYBridgeControl.get_bridgeLatency():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_BRIDGELATENCY_INVALID;
              exit;
            end;
        end;
      res := self._bridgeLatency;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the current Wheatstone bridge excitation method.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the current Wheatstone bridge excitation method
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
  function TYBridgeControl.set_bridgeLatency(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('bridgeLatency',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the raw value returned by the ratiometric A/D converter
  ///   during last read.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the raw value returned by the ratiometric A/D converter
  ///   during last read
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ADVALUE_INVALID.
  /// </para>
  ///-
  function TYBridgeControl.get_adValue():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ADVALUE_INVALID;
              exit;
            end;
        end;
      res := self._adValue;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the current ratiometric A/D converter gain.
  /// <para>
  ///   The gain is automatically
  ///   configured according to the signalRange set in the corresponding genericSensor.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the current ratiometric A/D converter gain
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ADGAIN_INVALID.
  /// </para>
  ///-
  function TYBridgeControl.get_adGain():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ADGAIN_INVALID;
              exit;
            end;
        end;
      res := self._adGain;
      result := res;
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
  ///   Use the method <c>YBridgeControl.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YBridgeControl</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYBridgeControl.FindBridgeControl(func: string):TYBridgeControl;
    var
      obj : TYBridgeControl;
    begin
      obj := TYBridgeControl(TYFunction._FindFromCache('BridgeControl', func));
      if obj = nil then
        begin
          obj :=  TYBridgeControl.create(func);
          TYFunction._AddToCache('BridgeControl',  func, obj);
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
  function TYBridgeControl.registerValueCallback(callback: TYBridgeControlValueCallback):LongInt;
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
      self._valueCallbackBridgeControl := callback;
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


  function TYBridgeControl._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackBridgeControl) <> nil) then
        begin
          self._valueCallbackBridgeControl(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYBridgeControl.nextBridgeControl(): TYBridgeControl;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextBridgeControl := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextBridgeControl := nil;
          exit;
        end;
      nextBridgeControl := TYBridgeControl.FindBridgeControl(hwid);
    end;

  class function TYBridgeControl.FirstBridgeControl(): TYBridgeControl;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('BridgeControl', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYBridgeControl.FindBridgeControl(serial+'.'+funcId);
    end;

//--- (end of YBridgeControl implementation)

//--- (BridgeControl functions)

  function yFindBridgeControl(func:string): TYBridgeControl;
    begin
      result := TYBridgeControl.FindBridgeControl(func);
    end;

  function yFirstBridgeControl(): TYBridgeControl;
    begin
      result := TYBridgeControl.FirstBridgeControl();
    end;

  procedure _BridgeControlCleanup();
    begin
    end;

//--- (end of BridgeControl functions)

initialization
  //--- (BridgeControl initialization)
  //--- (end of BridgeControl initialization)

finalization
  //--- (BridgeControl cleanup)
  _BridgeControlCleanup();
  //--- (end of BridgeControl cleanup)
end.
