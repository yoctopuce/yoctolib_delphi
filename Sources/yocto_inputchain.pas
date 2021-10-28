{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindInputChain(), the high-level API for InputChain functions
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


unit yocto_inputchain;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YInputChain definitions)

const Y_EXPECTEDNODES_INVALID         = YAPI_INVALID_UINT;
const Y_DETECTEDNODES_INVALID         = YAPI_INVALID_UINT;
const Y_REFRESHRATE_INVALID           = YAPI_INVALID_UINT;
const Y_BITCHAIN1_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN2_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN3_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN4_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN5_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN6_INVALID             = YAPI_INVALID_STRING;
const Y_BITCHAIN7_INVALID             = YAPI_INVALID_STRING;


//--- (end of YInputChain definitions)
//--- (YInputChain yapiwrapper declaration)
//--- (end of YInputChain yapiwrapper declaration)

type
  TYInputChain = class;
  //--- (YInputChain class start)
  TYInputChainValueCallback = procedure(func: TYInputChain; value:string);
  TYInputChainTimedReportCallback = procedure(func: TYInputChain; value:TYMeasure);

  ////
  /// <summary>
  ///   TYInputChain Class: InputChain function interface
  /// <para>
  ///   The <c>YInputChain</c> class provides access to separate
  ///   digital inputs connected in a chain.
  /// </para>
  /// </summary>
  ///-
  TYInputChain=class(TYFunction)
  //--- (end of YInputChain class start)
  protected
  //--- (YInputChain declaration)
    // Attributes (function value cache)
    _expectedNodes            : LongInt;
    _detectedNodes            : LongInt;
    _refreshRate              : LongInt;
    _bitChain1                : string;
    _bitChain2                : string;
    _bitChain3                : string;
    _bitChain4                : string;
    _bitChain5                : string;
    _bitChain6                : string;
    _bitChain7                : string;
    _valueCallbackInputChain  : TYInputChainValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YInputChain declaration)

  public
    //--- (YInputChain accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the number of nodes expected in the chain.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of nodes expected in the chain
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.EXPECTEDNODES_INVALID</c>.
    /// </para>
    ///-
    function get_expectedNodes():LongInt;

    ////
    /// <summary>
    ///   Changes the number of nodes expected in the chain.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the number of nodes expected in the chain
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
    function set_expectedNodes(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the number of nodes detected in the chain.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of nodes detected in the chain
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.DETECTEDNODES_INVALID</c>.
    /// </para>
    ///-
    function get_detectedNodes():LongInt;

    ////
    /// <summary>
    ///   Returns the desired refresh rate, measured in Hz.
    /// <para>
    ///   The higher the refresh rate is set, the higher the
    ///   communication speed on the chain will be.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the desired refresh rate, measured in Hz
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputChain.REFRESHRATE_INVALID</c>.
    /// </para>
    ///-
    function get_refreshRate():LongInt;

    ////
    /// <summary>
    ///   Changes the desired refresh rate, measured in Hz.
    /// <para>
    ///   The higher the refresh rate is set, the higher the
    ///   communication speed on the chain will be.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the desired refresh rate, measured in Hz
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
    function set_refreshRate(newval:LongInt):integer;

    function get_bitChain1():string;

    function get_bitChain2():string;

    function get_bitChain3():string;

    function get_bitChain4():string;

    function get_bitChain5():string;

    function get_bitChain6():string;

    function get_bitChain7():string;

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
    ///   Use the method <c>YInputChain.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YInputChain</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindInputChain(func: string):TYInputChain;

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
    function registerValueCallback(callback: TYInputChainValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of digital input chains started using <c>yFirstInputChain()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned digital input chains order.
    ///   If you want to find a specific a digital input chain, use <c>InputChain.findInputChain()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YInputChain</c> object, corresponding to
    ///   a digital input chain currently online, or a <c>NIL</c> pointer
    ///   if there are no more digital input chains to enumerate.
    /// </returns>
    ///-
    function nextInputChain():TYInputChain;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstInputChain():TYInputChain;
  //--- (end of YInputChain accessors declaration)
  end;

//--- (YInputChain functions declaration)
  ////
  /// <summary>
  ///   Retrieves a digital input chain for a given identifier.
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
  ///   This function does not require that the digital input chain is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YInputChain.isOnline()</c> to test if the digital input chain is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a digital input chain by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the digital input chain, for instance
  ///   <c>MyDevice.inputChain</c>.
  /// </param>
  /// <returns>
  ///   a <c>YInputChain</c> object allowing you to drive the digital input chain.
  /// </returns>
  ///-
  function yFindInputChain(func:string):TYInputChain;
  ////
  /// <summary>
  ///   Starts the enumeration of digital input chains currently accessible.
  /// <para>
  ///   Use the method <c>YInputChain.nextInputChain()</c> to iterate on
  ///   next digital input chains.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YInputChain</c> object, corresponding to
  ///   the first digital input chain currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstInputChain():TYInputChain;

//--- (end of YInputChain functions declaration)

implementation
//--- (YInputChain dlldef)
//--- (end of YInputChain dlldef)

  constructor TYInputChain.Create(func:string);
    begin
      inherited Create(func);
      _className := 'InputChain';
      //--- (YInputChain accessors initialization)
      _expectedNodes := Y_EXPECTEDNODES_INVALID;
      _detectedNodes := Y_DETECTEDNODES_INVALID;
      _refreshRate := Y_REFRESHRATE_INVALID;
      _bitChain1 := Y_BITCHAIN1_INVALID;
      _bitChain2 := Y_BITCHAIN2_INVALID;
      _bitChain3 := Y_BITCHAIN3_INVALID;
      _bitChain4 := Y_BITCHAIN4_INVALID;
      _bitChain5 := Y_BITCHAIN5_INVALID;
      _bitChain6 := Y_BITCHAIN6_INVALID;
      _bitChain7 := Y_BITCHAIN7_INVALID;
      _valueCallbackInputChain := nil;
      //--- (end of YInputChain accessors initialization)
    end;

//--- (YInputChain yapiwrapper)
//--- (end of YInputChain yapiwrapper)

//--- (YInputChain implementation)
{$HINTS OFF}
  function TYInputChain._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'expectedNodes') then
        begin
          _expectedNodes := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'detectedNodes') then
        begin
          _detectedNodes := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'refreshRate') then
        begin
          _refreshRate := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain1') then
        begin
          _bitChain1 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain2') then
        begin
          _bitChain2 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain3') then
        begin
          _bitChain3 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain4') then
        begin
          _bitChain4 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain5') then
        begin
          _bitChain5 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain6') then
        begin
          _bitChain6 := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'bitChain7') then
        begin
          _bitChain7 := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYInputChain.get_expectedNodes():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_EXPECTEDNODES_INVALID;
              exit;
            end;
        end;
      res := self._expectedNodes;
      result := res;
      exit;
    end;


  function TYInputChain.set_expectedNodes(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('expectedNodes',rest_val);
    end;

  function TYInputChain.get_detectedNodes():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DETECTEDNODES_INVALID;
              exit;
            end;
        end;
      res := self._detectedNodes;
      result := res;
      exit;
    end;


  function TYInputChain.get_refreshRate():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_REFRESHRATE_INVALID;
              exit;
            end;
        end;
      res := self._refreshRate;
      result := res;
      exit;
    end;


  function TYInputChain.set_refreshRate(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('refreshRate',rest_val);
    end;

  function TYInputChain.get_bitChain1():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN1_INVALID;
              exit;
            end;
        end;
      res := self._bitChain1;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain2():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN2_INVALID;
              exit;
            end;
        end;
      res := self._bitChain2;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain3():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN3_INVALID;
              exit;
            end;
        end;
      res := self._bitChain3;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain4():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN4_INVALID;
              exit;
            end;
        end;
      res := self._bitChain4;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain5():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN5_INVALID;
              exit;
            end;
        end;
      res := self._bitChain5;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain6():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN6_INVALID;
              exit;
            end;
        end;
      res := self._bitChain6;
      result := res;
      exit;
    end;


  function TYInputChain.get_bitChain7():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BITCHAIN7_INVALID;
              exit;
            end;
        end;
      res := self._bitChain7;
      result := res;
      exit;
    end;


  class function TYInputChain.FindInputChain(func: string):TYInputChain;
    var
      obj : TYInputChain;
    begin
      obj := TYInputChain(TYFunction._FindFromCache('InputChain', func));
      if obj = nil then
        begin
          obj :=  TYInputChain.create(func);
          TYFunction._AddToCache('InputChain',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYInputChain.registerValueCallback(callback: TYInputChainValueCallback):LongInt;
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
      self._valueCallbackInputChain := callback;
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


  function TYInputChain._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackInputChain) <> nil) then
        begin
          self._valueCallbackInputChain(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYInputChain.nextInputChain(): TYInputChain;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextInputChain := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextInputChain := nil;
          exit;
        end;
      nextInputChain := TYInputChain.FindInputChain(hwid);
    end;

  class function TYInputChain.FirstInputChain(): TYInputChain;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('InputChain', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYInputChain.FindInputChain(serial+'.'+funcId);
    end;

//--- (end of YInputChain implementation)

//--- (YInputChain functions)

  function yFindInputChain(func:string): TYInputChain;
    begin
      result := TYInputChain.FindInputChain(func);
    end;

  function yFirstInputChain(): TYInputChain;
    begin
      result := TYInputChain.FirstInputChain();
    end;

  procedure _InputChainCleanup();
    begin
    end;

//--- (end of YInputChain functions)

initialization
  //--- (YInputChain initialization)
  //--- (end of YInputChain initialization)

finalization
  //--- (YInputChain cleanup)
  _InputChainCleanup();
  //--- (end of YInputChain cleanup)
end.
