{*********************************************************************
 *
 *  $Id: yocto_voc.pas 63506 2024-11-28 10:42:13Z seb $
 *
 *  Implements yFindVoc(), the high-level API for Voc functions
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


unit yocto_voc;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YVoc definitions)


//--- (end of YVoc definitions)

//--- (YVoc yapiwrapper declaration)
//--- (end of YVoc yapiwrapper declaration)

type

  TYVoc = class;
  //--- (YVoc class start)
  TYVocValueCallback = procedure(func: TYVoc; value:string);
  TYVocTimedReportCallback = procedure(func: TYVoc; value:TYMeasure);

  ////
  /// <summary>
  ///   TYVoc Class: Volatile Organic Compound sensor control interface, available for instance in the Yocto-VOC-V3
  /// <para>
  ///   The <c>YVoc</c> class allows you to read and configure Yoctopuce Volatile Organic Compound sensors.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYVoc=class(TYSensor)
  //--- (end of YVoc class start)
  protected
  //--- (YVoc declaration)
    // Attributes (function value cache)
    _valueCallbackVoc         : TYVocValueCallback;
    _timedReportCallbackVoc   : TYVocTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YVoc declaration)

  public
    //--- (YVoc accessors declaration)
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
    ///   Use the method <c>YVoc.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YVoc</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindVoc(func: string):TYVoc;

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
    function registerValueCallback(callback: TYVocValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYVocTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of Volatile Organic Compound sensors started using <c>yFirstVoc()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned Volatile Organic Compound sensors order.
    ///   If you want to find a specific a Volatile Organic Compound sensor, use <c>Voc.findVoc()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YVoc</c> object, corresponding to
    ///   a Volatile Organic Compound sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more Volatile Organic Compound sensors to enumerate.
    /// </returns>
    ///-
    function nextVoc():TYVoc;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstVoc():TYVoc;
  //--- (end of YVoc accessors declaration)
  end;

//--- (YVoc functions declaration)
  ////
  /// <summary>
  ///   Retrieves a Volatile Organic Compound sensor for a given identifier.
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
  ///   This function does not require that the Volatile Organic Compound sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YVoc.isOnline()</c> to test if the Volatile Organic Compound sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a Volatile Organic Compound sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the Volatile Organic Compound sensor, for instance
  ///   <c>YVOCMK03.voc</c>.
  /// </param>
  /// <returns>
  ///   a <c>YVoc</c> object allowing you to drive the Volatile Organic Compound sensor.
  /// </returns>
  ///-
  function yFindVoc(func:string):TYVoc;
  ////
  /// <summary>
  ///   Starts the enumeration of Volatile Organic Compound sensors currently accessible.
  /// <para>
  ///   Use the method <c>YVoc.nextVoc()</c> to iterate on
  ///   next Volatile Organic Compound sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YVoc</c> object, corresponding to
  ///   the first Volatile Organic Compound sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstVoc():TYVoc;

//--- (end of YVoc functions declaration)

implementation

//--- (YVoc dlldef)
//--- (end of YVoc dlldef)

  constructor TYVoc.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Voc';
      //--- (YVoc accessors initialization)
      _valueCallbackVoc := nil;
      _timedReportCallbackVoc := nil;
      //--- (end of YVoc accessors initialization)
    end;

//--- (YVoc yapiwrapper)
//--- (end of YVoc yapiwrapper)

//--- (YVoc implementation)
{$HINTS OFF}
  function TYVoc._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  class function TYVoc.FindVoc(func: string):TYVoc;
    var
      obj : TYVoc;
    begin
      obj := TYVoc(TYFunction._FindFromCache('Voc', func));
      if obj = nil then
        begin
          obj :=  TYVoc.create(func);
          TYFunction._AddToCache('Voc', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYVoc.registerValueCallback(callback: TYVocValueCallback):LongInt;
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
      self._valueCallbackVoc := callback;
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


  function TYVoc._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackVoc) <> nil) then
        begin
          self._valueCallbackVoc(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYVoc.registerTimedReportCallback(callback: TYVocTimedReportCallback):LongInt;
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
      self._timedReportCallbackVoc := callback;
      result := 0;
      exit;
    end;


  function TYVoc._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackVoc) <> nil) then
        begin
          self._timedReportCallbackVoc(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYVoc.nextVoc(): TYVoc;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextVoc := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextVoc := nil;
          exit;
        end;
      nextVoc := TYVoc.FindVoc(hwid);
    end;

  class function TYVoc.FirstVoc(): TYVoc;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Voc', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYVoc.FindVoc(serial+'.'+funcId);
    end;

//--- (end of YVoc implementation)

//--- (YVoc functions)

  function yFindVoc(func:string): TYVoc;
    begin
      result := TYVoc.FindVoc(func);
    end;

  function yFirstVoc(): TYVoc;
    begin
      result := TYVoc.FirstVoc();
    end;

  procedure _VocCleanup();
    begin
    end;

//--- (end of YVoc functions)

initialization
  //--- (YVoc initialization)
  //--- (end of YVoc initialization)

finalization
  //--- (YVoc cleanup)
  _VocCleanup();
  //--- (end of YVoc cleanup)

end.
