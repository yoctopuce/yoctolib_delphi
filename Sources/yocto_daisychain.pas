{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindDaisyChain(), the high-level API for DaisyChain functions
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


unit yocto_daisychain;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YDaisyChain definitions)

const Y_DAISYSTATE_READY = 0;
const Y_DAISYSTATE_IS_CHILD = 1;
const Y_DAISYSTATE_FIRMWARE_MISMATCH = 2;
const Y_DAISYSTATE_CHILD_MISSING = 3;
const Y_DAISYSTATE_CHILD_LOST = 4;
const Y_DAISYSTATE_INVALID = -1;
const Y_CHILDCOUNT_INVALID            = YAPI_INVALID_UINT;
const Y_REQUIREDCHILDCOUNT_INVALID    = YAPI_INVALID_UINT;

//--- (end of YDaisyChain definitions)

//--- (YDaisyChain yapiwrapper declaration)
//--- (end of YDaisyChain yapiwrapper declaration)

type

  TYDaisyChain = class;
  //--- (YDaisyChain class start)
  TYDaisyChainValueCallback = procedure(func: TYDaisyChain; value:string);
  TYDaisyChainTimedReportCallback = procedure(func: TYDaisyChain; value:TYMeasure);

  ////
  /// <summary>
  ///   TYDaisyChain Class: Module chain configuration interface
  /// <para>
  ///   The <c>YDaisyChain</c> class can be used to verify that devices that
  ///   are daisy-chained directly from device to device, without a hub,
  ///   are detected properly.
  /// </para>
  /// </summary>
  ///-
  TYDaisyChain=class(TYFunction)
  //--- (end of YDaisyChain class start)
  protected
  //--- (YDaisyChain declaration)
    // Attributes (function value cache)
    _daisyState               : Integer;
    _childCount               : LongInt;
    _requiredChildCount       : LongInt;
    _valueCallbackDaisyChain  : TYDaisyChainValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YDaisyChain declaration)

  public
    //--- (YDaisyChain accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the state of the daisy-link between modules.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YDaisyChain.DAISYSTATE_READY</c>, <c>YDaisyChain.DAISYSTATE_IS_CHILD</c>,
    ///   <c>YDaisyChain.DAISYSTATE_FIRMWARE_MISMATCH</c>, <c>YDaisyChain.DAISYSTATE_CHILD_MISSING</c> and
    ///   <c>YDaisyChain.DAISYSTATE_CHILD_LOST</c> corresponding to the state of the daisy-link between modules
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDaisyChain.DAISYSTATE_INVALID</c>.
    /// </para>
    ///-
    function get_daisyState():Integer;

    ////
    /// <summary>
    ///   Returns the number of child nodes currently detected.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of child nodes currently detected
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDaisyChain.CHILDCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_childCount():LongInt;

    ////
    /// <summary>
    ///   Returns the number of child nodes expected in normal conditions.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of child nodes expected in normal conditions
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDaisyChain.REQUIREDCHILDCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_requiredChildCount():LongInt;

    ////
    /// <summary>
    ///   Changes the number of child nodes expected in normal conditions.
    /// <para>
    ///   If the value is zero, no check is performed. If it is non-zero, the number
    ///   child nodes is checked on startup and the status will change to error if
    ///   the count does not match. Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the number of child nodes expected in normal conditions
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
    function set_requiredChildCount(newval:LongInt):integer;

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
    ///   Use the method <c>YDaisyChain.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YDaisyChain</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindDaisyChain(func: string):TYDaisyChain;

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
    function registerValueCallback(callback: TYDaisyChainValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of module chains started using <c>yFirstDaisyChain()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned module chains order.
    ///   If you want to find a specific a module chain, use <c>DaisyChain.findDaisyChain()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YDaisyChain</c> object, corresponding to
    ///   a module chain currently online, or a <c>NIL</c> pointer
    ///   if there are no more module chains to enumerate.
    /// </returns>
    ///-
    function nextDaisyChain():TYDaisyChain;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstDaisyChain():TYDaisyChain;
  //--- (end of YDaisyChain accessors declaration)
  end;

//--- (YDaisyChain functions declaration)
  ////
  /// <summary>
  ///   Retrieves a module chain for a given identifier.
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
  ///   This function does not require that the module chain is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YDaisyChain.isOnline()</c> to test if the module chain is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a module chain by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the module chain, for instance
  ///   <c>MyDevice.daisyChain</c>.
  /// </param>
  /// <returns>
  ///   a <c>YDaisyChain</c> object allowing you to drive the module chain.
  /// </returns>
  ///-
  function yFindDaisyChain(func:string):TYDaisyChain;
  ////
  /// <summary>
  ///   Starts the enumeration of module chains currently accessible.
  /// <para>
  ///   Use the method <c>YDaisyChain.nextDaisyChain()</c> to iterate on
  ///   next module chains.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YDaisyChain</c> object, corresponding to
  ///   the first module chain currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstDaisyChain():TYDaisyChain;

//--- (end of YDaisyChain functions declaration)

implementation

//--- (YDaisyChain dlldef)
//--- (end of YDaisyChain dlldef)

  constructor TYDaisyChain.Create(func:string);
    begin
      inherited Create(func);
      _className := 'DaisyChain';
      //--- (YDaisyChain accessors initialization)
      _daisyState := Y_DAISYSTATE_INVALID;
      _childCount := Y_CHILDCOUNT_INVALID;
      _requiredChildCount := Y_REQUIREDCHILDCOUNT_INVALID;
      _valueCallbackDaisyChain := nil;
      //--- (end of YDaisyChain accessors initialization)
    end;

//--- (YDaisyChain yapiwrapper)
//--- (end of YDaisyChain yapiwrapper)

//--- (YDaisyChain implementation)
{$HINTS OFF}
  function TYDaisyChain._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'daisyState') then
        begin
          _daisyState := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'childCount') then
        begin
          _childCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'requiredChildCount') then
        begin
          _requiredChildCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYDaisyChain.get_daisyState():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DAISYSTATE_INVALID;
              exit;
            end;
        end;
      res := self._daisyState;
      result := res;
      exit;
    end;


  function TYDaisyChain.get_childCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CHILDCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._childCount;
      result := res;
      exit;
    end;


  function TYDaisyChain.get_requiredChildCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_REQUIREDCHILDCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._requiredChildCount;
      result := res;
      exit;
    end;


  function TYDaisyChain.set_requiredChildCount(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('requiredChildCount',rest_val);
    end;

  class function TYDaisyChain.FindDaisyChain(func: string):TYDaisyChain;
    var
      obj : TYDaisyChain;
    begin
      obj := TYDaisyChain(TYFunction._FindFromCache('DaisyChain', func));
      if obj = nil then
        begin
          obj :=  TYDaisyChain.create(func);
          TYFunction._AddToCache('DaisyChain', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYDaisyChain.registerValueCallback(callback: TYDaisyChainValueCallback):LongInt;
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
      self._valueCallbackDaisyChain := callback;
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


  function TYDaisyChain._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackDaisyChain) <> nil) then
        begin
          self._valueCallbackDaisyChain(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYDaisyChain.nextDaisyChain(): TYDaisyChain;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextDaisyChain := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextDaisyChain := nil;
          exit;
        end;
      nextDaisyChain := TYDaisyChain.FindDaisyChain(hwid);
    end;

  class function TYDaisyChain.FirstDaisyChain(): TYDaisyChain;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('DaisyChain', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYDaisyChain.FindDaisyChain(serial+'.'+funcId);
    end;

//--- (end of YDaisyChain implementation)

//--- (YDaisyChain functions)

  function yFindDaisyChain(func:string): TYDaisyChain;
    begin
      result := TYDaisyChain.FindDaisyChain(func);
    end;

  function yFirstDaisyChain(): TYDaisyChain;
    begin
      result := TYDaisyChain.FirstDaisyChain();
    end;

  procedure _DaisyChainCleanup();
    begin
    end;

//--- (end of YDaisyChain functions)

initialization
  //--- (YDaisyChain initialization)
  //--- (end of YDaisyChain initialization)

finalization
  //--- (YDaisyChain cleanup)
  _DaisyChainCleanup();
  //--- (end of YDaisyChain cleanup)

end.
