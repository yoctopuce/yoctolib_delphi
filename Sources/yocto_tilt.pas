{*********************************************************************
 *
 * $Id: yocto_tilt.pas 15254 2014-03-06 10:16:24Z seb $
 *
 * Implements yFindTilt(), the high-level API for Tilt functions
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


unit yocto_tilt;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YTilt definitions)

const Y_AXIS_X = 0;
const Y_AXIS_Y = 1;
const Y_AXIS_Z = 2;
const Y_AXIS_INVALID = -1;



//--- (end of YTilt definitions)

type
  TYTilt = class;
  //--- (YTilt class start)
  TYTiltValueCallback = procedure(func: TYTilt; value:string);
  TYTiltTimedReportCallback = procedure(func: TYTilt; value:TYMeasure);

  ////
  /// <summary>
  ///   TYTilt Class: Tilt function interface
  /// <para>
  ///   The Yoctopuce application programming interface allows you to read an instant
  ///   measure of the sensor, as well as the minimal and maximal values observed.
  /// </para>
  /// </summary>
  ///-
  TYTilt=class(TYSensor)
  //--- (end of YTilt class start)
  protected
  //--- (YTilt declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _unit                     : string;
    _currentValue             : double;
    _lowestValue              : double;
    _highestValue             : double;
    _currentRawValue          : double;
    _logFrequency             : string;
    _reportFrequency          : string;
    _calibrationParam         : string;
    _resolution               : double;
    _axis                     : Integer;
    _valueCallbackTilt        : TYTiltValueCallback;
    _timedReportCallbackTilt  : TYTiltTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YTilt declaration)

  public
    //--- (YTilt accessors declaration)
    constructor Create(func:string);

    function get_axis():Integer;

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
    ///   Use the method <c>YTilt.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YTilt</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindTilt(func: string):TYTilt;

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
    function registerValueCallback(callback: TYTiltValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every periodic timed notification.
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
    ///   arguments: the function object of which the value has changed, and an YMeasure object describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerTimedReportCallback(callback: TYTiltTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of tilt sensors started using <c>yFirstTilt()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YTilt</c> object, corresponding to
    ///   a tilt sensor currently online, or a <c>null</c> pointer
    ///   if there are no more tilt sensors to enumerate.
    /// </returns>
    ///-
    function nextTilt():TYTilt;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstTilt():TYTilt;
  //--- (end of YTilt accessors declaration)
  end;

//--- (Tilt functions declaration)

  ////
  /// <summary>
  ///   Retrieves a tilt sensor for a given identifier.
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
  ///   This function does not require that the tilt sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YTilt.isOnline()</c> to test if the tilt sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a tilt sensor by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the tilt sensor
  /// </param>
  /// <returns>
  ///   a <c>YTilt</c> object allowing you to drive the tilt sensor.
  /// </returns>
  ///-
  function yFindTilt(func:string):TYTilt;
  ////
  /// <summary>
  ///   Starts the enumeration of tilt sensors currently accessible.
  /// <para>
  ///   Use the method <c>YTilt.nextTilt()</c> to iterate on
  ///   next tilt sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YTilt</c> object, corresponding to
  ///   the first tilt sensor currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstTilt():TYTilt;

//--- (end of Tilt functions declaration)

implementation

  constructor TYTilt.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Tilt';
      //--- (YTilt accessors initialization)
      _axis := Y_AXIS_INVALID;
      _valueCallbackTilt := nil;
      _timedReportCallbackTilt := nil;
      //--- (end of YTilt accessors initialization)
    end;


//--- (YTilt implementation)
{$HINTS OFF}
  function TYTilt._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'axis') then
        begin
          _axis := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYTilt.get_axis():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_AXIS_INVALID;
              exit
            end;
        end;
      result := self._axis;
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
  ///   Use the method <c>YTilt.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YTilt</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYTilt.FindTilt(func: string):TYTilt;
    var
      obj : TYTilt;
    begin
      obj := TYTilt(TYFunction._FindFromCache('Tilt', func));
      if obj = nil then
        begin
          obj :=  TYTilt.create(func);
          TYFunction._AddToCache('Tilt',  func, obj)
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
  function TYTilt.registerValueCallback(callback: TYTiltValueCallback):LongInt;
    var
      val : string;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateValueCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateValueCallbackList(self, false)
        end;
      self._valueCallbackTilt := callback;
      // Immediately invoke value callback with current value
      if (addr(callback) <> nil) and self.isOnline then
        begin
          val := self._advertisedValue;
          if not((val = '')) then
            begin
              self._invokeValueCallback(val)
            end;
        end;
      result := 0;
      exit;
    end;


  function TYTilt._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackTilt) <> nil) then
        begin
          self._valueCallbackTilt(self, value)
        end
      else
        begin
          inherited _invokeValueCallback(value)
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Registers the callback function that is invoked on every periodic timed notification.
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
  ///   arguments: the function object of which the value has changed, and an YMeasure object describing
  ///   the new advertised value.
  /// @noreturn
  /// </param>
  ///-
  function TYTilt.registerTimedReportCallback(callback: TYTiltTimedReportCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(self, false)
        end;
      self._timedReportCallbackTilt := callback;
      result := 0;
      exit;
    end;


  function TYTilt._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackTilt) <> nil) then
        begin
          self._timedReportCallbackTilt(self, value)
        end
      else
        begin
          inherited _invokeTimedReportCallback(value)
        end;
      result := 0;
      exit;
    end;


  function TYTilt.nextTilt(): TYTilt;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextTilt := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextTilt := nil;
          exit;
        end;
      nextTilt := TYTilt.FindTilt(hwid);
    end;

  class function TYTilt.FirstTilt(): TYTilt;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Tilt', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYTilt.FindTilt(serial+'.'+funcId);
    end;

//--- (end of YTilt implementation)

//--- (Tilt functions)

  function yFindTilt(func:string): TYTilt;
    begin
      result := TYTilt.FindTilt(func);
    end;

  function yFirstTilt(): TYTilt;
    begin
      result := TYTilt.FirstTilt();
    end;

  procedure _TiltCleanup();
    begin
    end;

//--- (end of Tilt functions)

initialization
  //--- (Tilt initialization)
  //--- (end of Tilt initialization)

finalization
  //--- (Tilt cleanup)
  _TiltCleanup();
  //--- (end of Tilt cleanup)
end.
