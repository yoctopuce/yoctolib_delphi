{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindAngularSpeed(), the high-level API for AngularSpeed functions
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


unit yocto_angularspeed;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YAngularSpeed definitions)


//--- (end of YAngularSpeed definitions)

//--- (YAngularSpeed yapiwrapper declaration)
//--- (end of YAngularSpeed yapiwrapper declaration)

type

  TYAngularSpeed = class;
  //--- (YAngularSpeed class start)
  TYAngularSpeedValueCallback = procedure(func: TYAngularSpeed; value:string);
  TYAngularSpeedTimedReportCallback = procedure(func: TYAngularSpeed; value:TYMeasure);

  ////
  /// <summary>
  ///   TYAngularSpeed Class: tachometer control interface
  /// <para>
  ///   The <c>YAngularSpeed</c> class allows you to read and configure Yoctopuce tachometers.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYAngularSpeed=class(TYSensor)
  //--- (end of YAngularSpeed class start)
  protected
  //--- (YAngularSpeed declaration)
    // Attributes (function value cache)
    _valueCallbackAngularSpeed : TYAngularSpeedValueCallback;
    _timedReportCallbackAngularSpeed : TYAngularSpeedTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YAngularSpeed declaration)

  public
    //--- (YAngularSpeed accessors declaration)
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
    ///   Use the method <c>YAngularSpeed.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YAngularSpeed</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindAngularSpeed(func: string):TYAngularSpeed;

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
    function registerValueCallback(callback: TYAngularSpeedValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYAngularSpeedTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of tachometers started using <c>yFirstAngularSpeed()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned tachometers order.
    ///   If you want to find a specific a tachometer, use <c>AngularSpeed.findAngularSpeed()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YAngularSpeed</c> object, corresponding to
    ///   a tachometer currently online, or a <c>NIL</c> pointer
    ///   if there are no more tachometers to enumerate.
    /// </returns>
    ///-
    function nextAngularSpeed():TYAngularSpeed;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstAngularSpeed():TYAngularSpeed;
  //--- (end of YAngularSpeed accessors declaration)
  end;

//--- (YAngularSpeed functions declaration)
  ////
  /// <summary>
  ///   Retrieves a tachometer for a given identifier.
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
  ///   This function does not require that the rtachometer is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YAngularSpeed.isOnline()</c> to test if the rtachometer is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a tachometer by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the rtachometer, for instance
  ///   <c>MyDevice.angularSpeed</c>.
  /// </param>
  /// <returns>
  ///   a <c>YAngularSpeed</c> object allowing you to drive the rtachometer.
  /// </returns>
  ///-
  function yFindAngularSpeed(func:string):TYAngularSpeed;
  ////
  /// <summary>
  ///   Starts the enumeration of tachometers currently accessible.
  /// <para>
  ///   Use the method <c>YAngularSpeed.nextAngularSpeed()</c> to iterate on
  ///   next tachometers.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YAngularSpeed</c> object, corresponding to
  ///   the first tachometer currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstAngularSpeed():TYAngularSpeed;

//--- (end of YAngularSpeed functions declaration)

implementation

//--- (YAngularSpeed dlldef)
//--- (end of YAngularSpeed dlldef)

  constructor TYAngularSpeed.Create(func:string);
    begin
      inherited Create(func);
      _className := 'AngularSpeed';
      //--- (YAngularSpeed accessors initialization)
      _valueCallbackAngularSpeed := nil;
      _timedReportCallbackAngularSpeed := nil;
      //--- (end of YAngularSpeed accessors initialization)
    end;

//--- (YAngularSpeed yapiwrapper)
//--- (end of YAngularSpeed yapiwrapper)

//--- (YAngularSpeed implementation)
{$HINTS OFF}
  function TYAngularSpeed._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  class function TYAngularSpeed.FindAngularSpeed(func: string):TYAngularSpeed;
    var
      obj : TYAngularSpeed;
    begin
      obj := TYAngularSpeed(TYFunction._FindFromCache('AngularSpeed', func));
      if (obj = nil) then
        begin
          obj :=  TYAngularSpeed.create(func);
          TYFunction._AddToCache('AngularSpeed', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYAngularSpeed.registerValueCallback(callback: TYAngularSpeedValueCallback):LongInt;
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
      self._valueCallbackAngularSpeed := callback;
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


  function TYAngularSpeed._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackAngularSpeed) <> nil) then
        begin
          self._valueCallbackAngularSpeed(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYAngularSpeed.registerTimedReportCallback(callback: TYAngularSpeedTimedReportCallback):LongInt;
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
      self._timedReportCallbackAngularSpeed := callback;
      result := 0;
      exit;
    end;


  function TYAngularSpeed._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackAngularSpeed) <> nil) then
        begin
          self._timedReportCallbackAngularSpeed(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYAngularSpeed.nextAngularSpeed(): TYAngularSpeed;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextAngularSpeed := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextAngularSpeed := nil;
          exit;
        end;
      nextAngularSpeed := TYAngularSpeed.FindAngularSpeed(hwid);
    end;

  class function TYAngularSpeed.FirstAngularSpeed(): TYAngularSpeed;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('AngularSpeed', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYAngularSpeed.FindAngularSpeed(serial+'.'+funcId);
    end;

//--- (end of YAngularSpeed implementation)

//--- (YAngularSpeed functions)

  function yFindAngularSpeed(func:string): TYAngularSpeed;
    begin
      result := TYAngularSpeed.FindAngularSpeed(func);
    end;

  function yFirstAngularSpeed(): TYAngularSpeed;
    begin
      result := TYAngularSpeed.FirstAngularSpeed();
    end;

  procedure _AngularSpeedCleanup();
    begin
    end;

//--- (end of YAngularSpeed functions)

initialization
  //--- (YAngularSpeed initialization)
  //--- (end of YAngularSpeed initialization)

finalization
  //--- (YAngularSpeed cleanup)
  _AngularSpeedCleanup();
  //--- (end of YAngularSpeed cleanup)

end.
