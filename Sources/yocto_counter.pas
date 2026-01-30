{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindCounter(), the high-level API for Counter functions
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


unit yocto_counter;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YCounter definitions)


//--- (end of YCounter definitions)

//--- (YCounter yapiwrapper declaration)
//--- (end of YCounter yapiwrapper declaration)

type

  TYCounter = class;
  //--- (YCounter class start)
  TYCounterValueCallback = procedure(func: TYCounter; value:string);
  TYCounterTimedReportCallback = procedure(func: TYCounter; value:TYMeasure);

  ////
  /// <summary>
  ///   TYCounter Class: counter control interface
  /// <para>
  ///   The <c>YCounter</c> class allows you to read and configure Yoctopuce gcounters.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYCounter=class(TYSensor)
  //--- (end of YCounter class start)
  protected
  //--- (YCounter declaration)
    // Attributes (function value cache)
    _valueCallbackCounter     : TYCounterValueCallback;
    _timedReportCallbackCounter : TYCounterTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YCounter declaration)

  public
    //--- (YCounter accessors declaration)
    constructor Create(func:string);

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
    ///   Use the method <c>YCounter.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YCounter</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindCounter(func: string):TYCounter;

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
    function registerValueCallback(callback: TYCounterValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every periodic timed notification.
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
    ///   arguments: the function object of which the value has changed, and an <c>YMeasure</c> object describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerTimedReportCallback(callback: TYCounterTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of gcounters started using <c>yFirstCounter()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned gcounters order.
    ///   If you want to find a specific a counter, use <c>Counter.findCounter()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YCounter</c> object, corresponding to
    ///   a counter currently online, or a <c>NIL</c> pointer
    ///   if there are no more gcounters to enumerate.
    /// </returns>
    ///-
    function nextCounter():TYCounter;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstCounter():TYCounter;
  //--- (end of YCounter accessors declaration)
  end;

//--- (YCounter functions declaration)
  ////
  /// <summary>
  ///   Retrieves a counter for a given identifier.
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
  ///   This function does not require that the counter is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YCounter.isOnline()</c> to test if the counter is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a counter by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the counter, for instance
  ///   <c>MyDevice.counter</c>.
  /// </param>
  /// <returns>
  ///   a <c>YCounter</c> object allowing you to drive the counter.
  /// </returns>
  ///-
  function yFindCounter(func:string):TYCounter;
  ////
  /// <summary>
  ///   Starts the enumeration of gcounters currently accessible.
  /// <para>
  ///   Use the method <c>YCounter.nextCounter()</c> to iterate on
  ///   next gcounters.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YCounter</c> object, corresponding to
  ///   the first counter currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstCounter():TYCounter;

//--- (end of YCounter functions declaration)

implementation

//--- (YCounter dlldef)
//--- (end of YCounter dlldef)

  constructor TYCounter.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Counter';
      //--- (YCounter accessors initialization)
      _valueCallbackCounter := nil;
      _timedReportCallbackCounter := nil;
      //--- (end of YCounter accessors initialization)
    end;

//--- (YCounter yapiwrapper)
//--- (end of YCounter yapiwrapper)

//--- (YCounter implementation)
{$HINTS OFF}
  function TYCounter._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  class function TYCounter.FindCounter(func: string):TYCounter;
    var
      obj : TYCounter;
    begin
      obj := TYCounter(TYFunction._FindFromCache('Counter', func));
      if (obj = nil) then
        begin
          obj :=  TYCounter.create(func);
          TYFunction._AddToCache('Counter', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYCounter.registerValueCallback(callback: TYCounterValueCallback):LongInt;
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
      self._valueCallbackCounter := callback;
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


  function TYCounter._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackCounter) <> nil) then
        begin
          self._valueCallbackCounter(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYCounter.registerTimedReportCallback(callback: TYCounterTimedReportCallback):LongInt;
    var
      sensor : TYSensor;
    begin
      sensor := self;
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(sensor, true);
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(sensor, false);
        end;
      self._timedReportCallbackCounter := callback;
      result := 0;
      exit;
    end;


  function TYCounter._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackCounter) <> nil) then
        begin
          self._timedReportCallbackCounter(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYCounter.nextCounter(): TYCounter;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextCounter := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextCounter := nil;
          exit;
        end;
      nextCounter := TYCounter.FindCounter(hwid);
    end;

  class function TYCounter.FirstCounter(): TYCounter;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Counter', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYCounter.FindCounter(serial+'.'+funcId);
    end;

//--- (end of YCounter implementation)

//--- (YCounter functions)

  function yFindCounter(func:string): TYCounter;
    begin
      result := TYCounter.FindCounter(func);
    end;

  function yFirstCounter(): TYCounter;
    begin
      result := TYCounter.FirstCounter();
    end;

  procedure _CounterCleanup();
    begin
    end;

//--- (end of YCounter functions)

initialization
  //--- (YCounter initialization)
  //--- (end of YCounter initialization)

finalization
  //--- (YCounter cleanup)
  _CounterCleanup();
  //--- (end of YCounter cleanup)

end.
