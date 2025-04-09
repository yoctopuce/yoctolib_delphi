{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindLongitude(), the high-level API for Longitude functions
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


unit yocto_longitude;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YLongitude definitions)


//--- (end of YLongitude definitions)

//--- (YLongitude yapiwrapper declaration)
//--- (end of YLongitude yapiwrapper declaration)

type

  TYLongitude = class;
  //--- (YLongitude class start)
  TYLongitudeValueCallback = procedure(func: TYLongitude; value:string);
  TYLongitudeTimedReportCallback = procedure(func: TYLongitude; value:TYMeasure);

  ////
  /// <summary>
  ///   TYLongitude Class: longitude sensor control interface, available for instance in the Yocto-GPS-V2
  /// <para>
  ///   The <c>YLongitude</c> class allows you to read and configure Yoctopuce longitude sensors.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYLongitude=class(TYSensor)
  //--- (end of YLongitude class start)
  protected
  //--- (YLongitude declaration)
    // Attributes (function value cache)
    _valueCallbackLongitude   : TYLongitudeValueCallback;
    _timedReportCallbackLongitude : TYLongitudeTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YLongitude declaration)

  public
    //--- (YLongitude accessors declaration)
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
    ///   Use the method <c>YLongitude.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YLongitude</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindLongitude(func: string):TYLongitude;

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
    function registerValueCallback(callback: TYLongitudeValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYLongitudeTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of longitude sensors started using <c>yFirstLongitude()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned longitude sensors order.
    ///   If you want to find a specific a longitude sensor, use <c>Longitude.findLongitude()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YLongitude</c> object, corresponding to
    ///   a longitude sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more longitude sensors to enumerate.
    /// </returns>
    ///-
    function nextLongitude():TYLongitude;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstLongitude():TYLongitude;
  //--- (end of YLongitude accessors declaration)
  end;

//--- (YLongitude functions declaration)
  ////
  /// <summary>
  ///   Retrieves a longitude sensor for a given identifier.
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
  ///   This function does not require that the longitude sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YLongitude.isOnline()</c> to test if the longitude sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a longitude sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the longitude sensor, for instance
  ///   <c>YGNSSMK2.longitude</c>.
  /// </param>
  /// <returns>
  ///   a <c>YLongitude</c> object allowing you to drive the longitude sensor.
  /// </returns>
  ///-
  function yFindLongitude(func:string):TYLongitude;
  ////
  /// <summary>
  ///   Starts the enumeration of longitude sensors currently accessible.
  /// <para>
  ///   Use the method <c>YLongitude.nextLongitude()</c> to iterate on
  ///   next longitude sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YLongitude</c> object, corresponding to
  ///   the first longitude sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstLongitude():TYLongitude;

//--- (end of YLongitude functions declaration)

implementation

//--- (YLongitude dlldef)
//--- (end of YLongitude dlldef)

  constructor TYLongitude.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Longitude';
      //--- (YLongitude accessors initialization)
      _valueCallbackLongitude := nil;
      _timedReportCallbackLongitude := nil;
      //--- (end of YLongitude accessors initialization)
    end;

//--- (YLongitude yapiwrapper)
//--- (end of YLongitude yapiwrapper)

//--- (YLongitude implementation)
{$HINTS OFF}
  function TYLongitude._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  class function TYLongitude.FindLongitude(func: string):TYLongitude;
    var
      obj : TYLongitude;
    begin
      obj := TYLongitude(TYFunction._FindFromCache('Longitude', func));
      if obj = nil then
        begin
          obj :=  TYLongitude.create(func);
          TYFunction._AddToCache('Longitude', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYLongitude.registerValueCallback(callback: TYLongitudeValueCallback):LongInt;
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
      self._valueCallbackLongitude := callback;
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


  function TYLongitude._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackLongitude) <> nil) then
        begin
          self._valueCallbackLongitude(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYLongitude.registerTimedReportCallback(callback: TYLongitudeTimedReportCallback):LongInt;
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
      self._timedReportCallbackLongitude := callback;
      result := 0;
      exit;
    end;


  function TYLongitude._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackLongitude) <> nil) then
        begin
          self._timedReportCallbackLongitude(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYLongitude.nextLongitude(): TYLongitude;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextLongitude := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextLongitude := nil;
          exit;
        end;
      nextLongitude := TYLongitude.FindLongitude(hwid);
    end;

  class function TYLongitude.FirstLongitude(): TYLongitude;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Longitude', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYLongitude.FindLongitude(serial+'.'+funcId);
    end;

//--- (end of YLongitude implementation)

//--- (YLongitude functions)

  function yFindLongitude(func:string): TYLongitude;
    begin
      result := TYLongitude.FindLongitude(func);
    end;

  function yFirstLongitude(): TYLongitude;
    begin
      result := TYLongitude.FirstLongitude();
    end;

  procedure _LongitudeCleanup();
    begin
    end;

//--- (end of YLongitude functions)

initialization
  //--- (YLongitude initialization)
  //--- (end of YLongitude initialization)

finalization
  //--- (YLongitude cleanup)
  _LongitudeCleanup();
  //--- (end of YLongitude cleanup)

end.
