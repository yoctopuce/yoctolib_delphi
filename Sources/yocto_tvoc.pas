{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindTvoc(), the high-level API for Tvoc functions
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


unit yocto_tvoc;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YTvoc definitions)


//--- (end of YTvoc definitions)

//--- (YTvoc yapiwrapper declaration)
//--- (end of YTvoc yapiwrapper declaration)

type

  TYTvoc = class;
  //--- (YTvoc class start)
  TYTvocValueCallback = procedure(func: TYTvoc; value:string);
  TYTvocTimedReportCallback = procedure(func: TYTvoc; value:TYMeasure);

  ////
  /// <summary>
  ///   TYTvoc Class: Total Volatile Organic Compound sensor control interface, available for instance in
  ///   the Yocto-VOC-V3
  /// <para>
  ///   The <c>YTvoc</c> class allows you to read and configure Yoctopuce Total Volatile Organic Compound sensors.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYTvoc=class(TYSensor)
  //--- (end of YTvoc class start)
  protected
  //--- (YTvoc declaration)
    // Attributes (function value cache)
    _valueCallbackTvoc        : TYTvocValueCallback;
    _timedReportCallbackTvoc  : TYTvocTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YTvoc declaration)

  public
    //--- (YTvoc accessors declaration)
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
    ///   Use the method <c>YTvoc.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YTvoc</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindTvoc(func: string):TYTvoc;

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
    function registerValueCallback(callback: TYTvocValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYTvocTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of Total Volatile Organic Compound sensors started using <c>yFirstTvoc()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned Total Volatile Organic Compound sensors order.
    ///   If you want to find a specific a Total  Volatile Organic Compound sensor, use <c>Tvoc.findTvoc()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YTvoc</c> object, corresponding to
    ///   a Total  Volatile Organic Compound sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more Total Volatile Organic Compound sensors to enumerate.
    /// </returns>
    ///-
    function nextTvoc():TYTvoc;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstTvoc():TYTvoc;
  //--- (end of YTvoc accessors declaration)
  end;

//--- (YTvoc functions declaration)
  ////
  /// <summary>
  ///   Retrieves a Total  Volatile Organic Compound sensor for a given identifier.
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
  ///   This function does not require that the Total  Volatile Organic Compound sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YTvoc.isOnline()</c> to test if the Total  Volatile Organic Compound sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a Total  Volatile Organic Compound sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the Total  Volatile Organic Compound sensor, for instance
  ///   <c>YVOCMK03.tvoc</c>.
  /// </param>
  /// <returns>
  ///   a <c>YTvoc</c> object allowing you to drive the Total  Volatile Organic Compound sensor.
  /// </returns>
  ///-
  function yFindTvoc(func:string):TYTvoc;
  ////
  /// <summary>
  ///   Starts the enumeration of Total Volatile Organic Compound sensors currently accessible.
  /// <para>
  ///   Use the method <c>YTvoc.nextTvoc()</c> to iterate on
  ///   next Total Volatile Organic Compound sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YTvoc</c> object, corresponding to
  ///   the first Total Volatile Organic Compound sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstTvoc():TYTvoc;

//--- (end of YTvoc functions declaration)

implementation

//--- (YTvoc dlldef)
//--- (end of YTvoc dlldef)

  constructor TYTvoc.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Tvoc';
      //--- (YTvoc accessors initialization)
      _valueCallbackTvoc := nil;
      _timedReportCallbackTvoc := nil;
      //--- (end of YTvoc accessors initialization)
    end;

//--- (YTvoc yapiwrapper)
//--- (end of YTvoc yapiwrapper)

//--- (YTvoc implementation)
{$HINTS OFF}
  function TYTvoc._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  class function TYTvoc.FindTvoc(func: string):TYTvoc;
    var
      obj : TYTvoc;
    begin
      obj := TYTvoc(TYFunction._FindFromCache('Tvoc', func));
      if obj = nil then
        begin
          obj :=  TYTvoc.create(func);
          TYFunction._AddToCache('Tvoc', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYTvoc.registerValueCallback(callback: TYTvocValueCallback):LongInt;
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
      self._valueCallbackTvoc := callback;
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


  function TYTvoc._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackTvoc) <> nil) then
        begin
          self._valueCallbackTvoc(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYTvoc.registerTimedReportCallback(callback: TYTvocTimedReportCallback):LongInt;
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
      self._timedReportCallbackTvoc := callback;
      result := 0;
      exit;
    end;


  function TYTvoc._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackTvoc) <> nil) then
        begin
          self._timedReportCallbackTvoc(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYTvoc.nextTvoc(): TYTvoc;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextTvoc := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextTvoc := nil;
          exit;
        end;
      nextTvoc := TYTvoc.FindTvoc(hwid);
    end;

  class function TYTvoc.FirstTvoc(): TYTvoc;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Tvoc', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYTvoc.FindTvoc(serial+'.'+funcId);
    end;

//--- (end of YTvoc implementation)

//--- (YTvoc functions)

  function yFindTvoc(func:string): TYTvoc;
    begin
      result := TYTvoc.FindTvoc(func);
    end;

  function yFirstTvoc(): TYTvoc;
    begin
      result := TYTvoc.FirstTvoc();
    end;

  procedure _TvocCleanup();
    begin
    end;

//--- (end of YTvoc functions)

initialization
  //--- (YTvoc initialization)
  //--- (end of YTvoc initialization)

finalization
  //--- (YTvoc cleanup)
  _TvocCleanup();
  //--- (end of YTvoc cleanup)

end.
