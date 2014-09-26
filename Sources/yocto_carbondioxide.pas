{*********************************************************************
 *
 * $Id: yocto_carbondioxide.pas 17350 2014-08-29 08:54:26Z seb $
 *
 * Implements yFindCarbonDioxide(), the high-level API for CarbonDioxide functions
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


unit yocto_carbondioxide;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YCarbonDioxide definitions)



//--- (end of YCarbonDioxide definitions)

type
  TYCarbonDioxide = class;
  //--- (YCarbonDioxide class start)
  TYCarbonDioxideValueCallback = procedure(func: TYCarbonDioxide; value:string);
  TYCarbonDioxideTimedReportCallback = procedure(func: TYCarbonDioxide; value:TYMeasure);

  ////
  /// <summary>
  ///   TYCarbonDioxide Class: CarbonDioxide function interface
  /// <para>
  ///   The Yoctopuce application programming interface allows you to read an instant
  ///   measure of the sensor, as well as the minimal and maximal values observed.
  /// </para>
  /// </summary>
  ///-
  TYCarbonDioxide=class(TYSensor)
  //--- (end of YCarbonDioxide class start)
  protected
  //--- (YCarbonDioxide declaration)
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
    _valueCallbackCarbonDioxide : TYCarbonDioxideValueCallback;
    _timedReportCallbackCarbonDioxide : TYCarbonDioxideTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YCarbonDioxide declaration)

  public
    //--- (YCarbonDioxide accessors declaration)
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
    ///   Use the method <c>YCarbonDioxide.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YCarbonDioxide</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindCarbonDioxide(func: string):TYCarbonDioxide;

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
    function registerValueCallback(callback: TYCarbonDioxideValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYCarbonDioxideTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of CO2 sensors started using <c>yFirstCarbonDioxide()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YCarbonDioxide</c> object, corresponding to
    ///   a CO2 sensor currently online, or a <c>null</c> pointer
    ///   if there are no more CO2 sensors to enumerate.
    /// </returns>
    ///-
    function nextCarbonDioxide():TYCarbonDioxide;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstCarbonDioxide():TYCarbonDioxide;
  //--- (end of YCarbonDioxide accessors declaration)
  end;

//--- (CarbonDioxide functions declaration)
  ////
  /// <summary>
  ///   Retrieves a CO2 sensor for a given identifier.
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
  ///   This function does not require that the CO2 sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YCarbonDioxide.isOnline()</c> to test if the CO2 sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a CO2 sensor by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the CO2 sensor
  /// </param>
  /// <returns>
  ///   a <c>YCarbonDioxide</c> object allowing you to drive the CO2 sensor.
  /// </returns>
  ///-
  function yFindCarbonDioxide(func:string):TYCarbonDioxide;
  ////
  /// <summary>
  ///   Starts the enumeration of CO2 sensors currently accessible.
  /// <para>
  ///   Use the method <c>YCarbonDioxide.nextCarbonDioxide()</c> to iterate on
  ///   next CO2 sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YCarbonDioxide</c> object, corresponding to
  ///   the first CO2 sensor currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstCarbonDioxide():TYCarbonDioxide;

//--- (end of CarbonDioxide functions declaration)

implementation
//--- (YCarbonDioxide dlldef)
//--- (end of YCarbonDioxide dlldef)

  constructor TYCarbonDioxide.Create(func:string);
    begin
      inherited Create(func);
      _className := 'CarbonDioxide';
      //--- (YCarbonDioxide accessors initialization)
      _valueCallbackCarbonDioxide := nil;
      _timedReportCallbackCarbonDioxide := nil;
      //--- (end of YCarbonDioxide accessors initialization)
    end;


//--- (YCarbonDioxide implementation)
{$HINTS OFF}
  function TYCarbonDioxide._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

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
  ///   Use the method <c>YCarbonDioxide.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YCarbonDioxide</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYCarbonDioxide.FindCarbonDioxide(func: string):TYCarbonDioxide;
    var
      obj : TYCarbonDioxide;
    begin
      obj := TYCarbonDioxide(TYFunction._FindFromCache('CarbonDioxide', func));
      if obj = nil then
        begin
          obj :=  TYCarbonDioxide.create(func);
          TYFunction._AddToCache('CarbonDioxide',  func, obj)
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
  function TYCarbonDioxide.registerValueCallback(callback: TYCarbonDioxideValueCallback):LongInt;
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
      self._valueCallbackCarbonDioxide := callback;
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


  function TYCarbonDioxide._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackCarbonDioxide) <> nil) then
        begin
          self._valueCallbackCarbonDioxide(self, value)
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
  function TYCarbonDioxide.registerTimedReportCallback(callback: TYCarbonDioxideTimedReportCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(self, false)
        end;
      self._timedReportCallbackCarbonDioxide := callback;
      result := 0;
      exit;
    end;


  function TYCarbonDioxide._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackCarbonDioxide) <> nil) then
        begin
          self._timedReportCallbackCarbonDioxide(self, value)
        end
      else
        begin
          inherited _invokeTimedReportCallback(value)
        end;
      result := 0;
      exit;
    end;


  function TYCarbonDioxide.nextCarbonDioxide(): TYCarbonDioxide;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextCarbonDioxide := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextCarbonDioxide := nil;
          exit;
        end;
      nextCarbonDioxide := TYCarbonDioxide.FindCarbonDioxide(hwid);
    end;

  class function TYCarbonDioxide.FirstCarbonDioxide(): TYCarbonDioxide;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('CarbonDioxide', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYCarbonDioxide.FindCarbonDioxide(serial+'.'+funcId);
    end;

//--- (end of YCarbonDioxide implementation)

//--- (CarbonDioxide functions)

  function yFindCarbonDioxide(func:string): TYCarbonDioxide;
    begin
      result := TYCarbonDioxide.FindCarbonDioxide(func);
    end;

  function yFirstCarbonDioxide(): TYCarbonDioxide;
    begin
      result := TYCarbonDioxide.FirstCarbonDioxide();
    end;

  procedure _CarbonDioxideCleanup();
    begin
    end;

//--- (end of CarbonDioxide functions)

initialization
  //--- (CarbonDioxide initialization)
  //--- (end of CarbonDioxide initialization)

finalization
  //--- (CarbonDioxide cleanup)
  _CarbonDioxideCleanup();
  //--- (end of CarbonDioxide cleanup)
end.
