{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindOrientation(), the high-level API for Orientation functions
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


unit yocto_orientation;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YOrientation definitions)


//--- (end of YOrientation definitions)

//--- (YOrientation yapiwrapper declaration)
//--- (end of YOrientation yapiwrapper declaration)

type

  TYOrientation = class;
  //--- (YOrientation class start)
  TYOrientationValueCallback = procedure(func: TYOrientation; value:string);
  TYOrientationTimedReportCallback = procedure(func: TYOrientation; value:TYMeasure);

  ////
  /// <summary>
  ///   TYOrientation Class: orientation sensor control interface
  /// <para>
  ///   The <c>YOrientation</c> class allows you to read and configure Yoctopuce orientation sensors.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYOrientation=class(TYSensor)
  //--- (end of YOrientation class start)
  protected
  //--- (YOrientation declaration)
    // Attributes (function value cache)
    _valueCallbackOrientation : TYOrientationValueCallback;
    _timedReportCallbackOrientation : TYOrientationTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YOrientation declaration)

  public
    //--- (YOrientation accessors declaration)
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
    ///   Use the method <c>YOrientation.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YOrientation</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindOrientation(func: string):TYOrientation;

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
    function registerValueCallback(callback: TYOrientationValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYOrientationTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of orientation sensors started using <c>yFirstOrientation()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned orientation sensors order.
    ///   If you want to find a specific an orientation sensor, use <c>Orientation.findOrientation()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YOrientation</c> object, corresponding to
    ///   an orientation sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more orientation sensors to enumerate.
    /// </returns>
    ///-
    function nextOrientation():TYOrientation;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstOrientation():TYOrientation;
  //--- (end of YOrientation accessors declaration)
  end;

//--- (YOrientation functions declaration)
  ////
  /// <summary>
  ///   Retrieves an orientation sensor for a given identifier.
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
  ///   This function does not require that the orientation sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YOrientation.isOnline()</c> to test if the orientation sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   an orientation sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the orientation sensor, for instance
  ///   <c>MyDevice.orientation</c>.
  /// </param>
  /// <returns>
  ///   a <c>YOrientation</c> object allowing you to drive the orientation sensor.
  /// </returns>
  ///-
  function yFindOrientation(func:string):TYOrientation;
  ////
  /// <summary>
  ///   Starts the enumeration of orientation sensors currently accessible.
  /// <para>
  ///   Use the method <c>YOrientation.nextOrientation()</c> to iterate on
  ///   next orientation sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YOrientation</c> object, corresponding to
  ///   the first orientation sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstOrientation():TYOrientation;

//--- (end of YOrientation functions declaration)

implementation

//--- (YOrientation dlldef)
//--- (end of YOrientation dlldef)

  constructor TYOrientation.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Orientation';
      //--- (YOrientation accessors initialization)
      _valueCallbackOrientation := nil;
      _timedReportCallbackOrientation := nil;
      //--- (end of YOrientation accessors initialization)
    end;

//--- (YOrientation yapiwrapper)
//--- (end of YOrientation yapiwrapper)

//--- (YOrientation implementation)
{$HINTS OFF}
  function TYOrientation._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  class function TYOrientation.FindOrientation(func: string):TYOrientation;
    var
      obj : TYOrientation;
    begin
      obj := TYOrientation(TYFunction._FindFromCache('Orientation', func));
      if (obj = nil) then
        begin
          obj :=  TYOrientation.create(func);
          TYFunction._AddToCache('Orientation', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYOrientation.registerValueCallback(callback: TYOrientationValueCallback):LongInt;
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
      self._valueCallbackOrientation := callback;
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


  function TYOrientation._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackOrientation) <> nil) then
        begin
          self._valueCallbackOrientation(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYOrientation.registerTimedReportCallback(callback: TYOrientationTimedReportCallback):LongInt;
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
      self._timedReportCallbackOrientation := callback;
      result := 0;
      exit;
    end;


  function TYOrientation._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackOrientation) <> nil) then
        begin
          self._timedReportCallbackOrientation(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYOrientation.nextOrientation(): TYOrientation;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextOrientation := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextOrientation := nil;
          exit;
        end;
      nextOrientation := TYOrientation.FindOrientation(hwid);
    end;

  class function TYOrientation.FirstOrientation(): TYOrientation;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Orientation', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYOrientation.FindOrientation(serial+'.'+funcId);
    end;

//--- (end of YOrientation implementation)

//--- (YOrientation functions)

  function yFindOrientation(func:string): TYOrientation;
    begin
      result := TYOrientation.FindOrientation(func);
    end;

  function yFirstOrientation(): TYOrientation;
    begin
      result := TYOrientation.FirstOrientation();
    end;

  procedure _OrientationCleanup();
    begin
    end;

//--- (end of YOrientation functions)

initialization
  //--- (YOrientation initialization)
  //--- (end of YOrientation initialization)

finalization
  //--- (YOrientation cleanup)
  _OrientationCleanup();
  //--- (end of YOrientation cleanup)

end.
