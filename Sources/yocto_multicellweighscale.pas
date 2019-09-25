{*********************************************************************
 *
 *  $Id: yocto_multicellweighscale.pas 37165 2019-09-13 16:57:27Z mvuilleu $
 *
 *  Implements yFindMultiCellWeighScale(), the high-level API for MultiCellWeighScale functions
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


unit yocto_multicellweighscale;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YMultiCellWeighScale definitions)

const Y_CELLCOUNT_INVALID             = YAPI_INVALID_UINT;
const Y_EXCITATION_OFF = 0;
const Y_EXCITATION_DC = 1;
const Y_EXCITATION_AC = 2;
const Y_EXCITATION_INVALID = -1;
const Y_TEMPAVGADAPTRATIO_INVALID     = YAPI_INVALID_DOUBLE;
const Y_TEMPCHGADAPTRATIO_INVALID     = YAPI_INVALID_DOUBLE;
const Y_COMPTEMPAVG_INVALID           = YAPI_INVALID_DOUBLE;
const Y_COMPTEMPCHG_INVALID           = YAPI_INVALID_DOUBLE;
const Y_COMPENSATION_INVALID          = YAPI_INVALID_DOUBLE;
const Y_ZEROTRACKING_INVALID          = YAPI_INVALID_DOUBLE;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YMultiCellWeighScale definitions)
//--- (YMultiCellWeighScale yapiwrapper declaration)
//--- (end of YMultiCellWeighScale yapiwrapper declaration)

type
  TYMultiCellWeighScale = class;
  //--- (YMultiCellWeighScale class start)
  TYMultiCellWeighScaleValueCallback = procedure(func: TYMultiCellWeighScale; value:string);
  TYMultiCellWeighScaleTimedReportCallback = procedure(func: TYMultiCellWeighScale; value:TYMeasure);

  ////
  /// <summary>
  ///   TYMultiCellWeighScale Class: MultiCellWeighScale function interface
  /// <para>
  ///   The YMultiCellWeighScale class provides a weight measurement from a set of ratiometric load cells
  ///   sensor. It can be used to control the bridge excitation parameters, in order to avoid
  ///   measure shifts caused by temperature variation in the electronics, and can also
  ///   automatically apply an additional correction factor based on temperature to
  ///   compensate for offsets in the load cells themselves.
  /// </para>
  /// </summary>
  ///-
  TYMultiCellWeighScale=class(TYSensor)
  //--- (end of YMultiCellWeighScale class start)
  protected
  //--- (YMultiCellWeighScale declaration)
    // Attributes (function value cache)
    _cellCount                : LongInt;
    _excitation               : Integer;
    _tempAvgAdaptRatio        : double;
    _tempChgAdaptRatio        : double;
    _compTempAvg              : double;
    _compTempChg              : double;
    _compensation             : double;
    _zeroTracking             : double;
    _command                  : string;
    _valueCallbackMultiCellWeighScale : TYMultiCellWeighScaleValueCallback;
    _timedReportCallbackMultiCellWeighScale : TYMultiCellWeighScaleTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YMultiCellWeighScale declaration)

  public
    //--- (YMultiCellWeighScale accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the measuring unit for the weight.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the measuring unit for the weight
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_unit(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the number of load cells in use.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of load cells in use
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CELLCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_cellCount():LongInt;

    ////
    /// <summary>
    ///   Changes the number of load cells in use.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the number of load cells in use
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_cellCount(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the current load cell bridge excitation method.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_EXCITATION_OFF</c>, <c>Y_EXCITATION_DC</c> and <c>Y_EXCITATION_AC</c>
    ///   corresponding to the current load cell bridge excitation method
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_EXCITATION_INVALID</c>.
    /// </para>
    ///-
    function get_excitation():Integer;

    ////
    /// <summary>
    ///   Changes the current load cell bridge excitation method.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_EXCITATION_OFF</c>, <c>Y_EXCITATION_DC</c> and <c>Y_EXCITATION_AC</c>
    ///   corresponding to the current load cell bridge excitation method
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_excitation(newval:Integer):integer;

    ////
    /// <summary>
    ///   Changes the averaged temperature update rate, in per mille.
    /// <para>
    ///   The purpose of this adaptation ratio is to model the thermal inertia of the load cell.
    ///   The averaged temperature is updated every 10 seconds, by applying this adaptation rate
    ///   to the difference between the measures ambient temperature and the current compensation
    ///   temperature. The standard rate is 0.2 per mille, and the maximal rate is 65 per mille.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the averaged temperature update rate, in per mille
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_tempAvgAdaptRatio(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the averaged temperature update rate, in per mille.
    /// <para>
    ///   The purpose of this adaptation ratio is to model the thermal inertia of the load cell.
    ///   The averaged temperature is updated every 10 seconds, by applying this adaptation rate
    ///   to the difference between the measures ambient temperature and the current compensation
    ///   temperature. The standard rate is 0.2 per mille, and the maximal rate is 65 per mille.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the averaged temperature update rate, in per mille
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_TEMPAVGADAPTRATIO_INVALID</c>.
    /// </para>
    ///-
    function get_tempAvgAdaptRatio():double;

    ////
    /// <summary>
    ///   Changes the temperature change update rate, in per mille.
    /// <para>
    ///   The temperature change is updated every 10 seconds, by applying this adaptation rate
    ///   to the difference between the measures ambient temperature and the current temperature used for
    ///   change compensation. The standard rate is 0.6 per mille, and the maximal rate is 65 per mille.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the temperature change update rate, in per mille
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_tempChgAdaptRatio(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the temperature change update rate, in per mille.
    /// <para>
    ///   The temperature change is updated every 10 seconds, by applying this adaptation rate
    ///   to the difference between the measures ambient temperature and the current temperature used for
    ///   change compensation. The standard rate is 0.6 per mille, and the maximal rate is 65 per mille.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the temperature change update rate, in per mille
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_TEMPCHGADAPTRATIO_INVALID</c>.
    /// </para>
    ///-
    function get_tempChgAdaptRatio():double;

    ////
    /// <summary>
    ///   Returns the current averaged temperature, used for thermal compensation.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current averaged temperature, used for thermal compensation
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_COMPTEMPAVG_INVALID</c>.
    /// </para>
    ///-
    function get_compTempAvg():double;

    ////
    /// <summary>
    ///   Returns the current temperature variation, used for thermal compensation.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current temperature variation, used for thermal compensation
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_COMPTEMPCHG_INVALID</c>.
    /// </para>
    ///-
    function get_compTempChg():double;

    ////
    /// <summary>
    ///   Returns the current current thermal compensation value.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current current thermal compensation value
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_COMPENSATION_INVALID</c>.
    /// </para>
    ///-
    function get_compensation():double;

    ////
    /// <summary>
    ///   Changes the zero tracking threshold value.
    /// <para>
    ///   When this threshold is larger than
    ///   zero, any measure under the threshold will automatically be ignored and the
    ///   zero compensation will be updated.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the zero tracking threshold value
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_zeroTracking(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the zero tracking threshold value.
    /// <para>
    ///   When this threshold is larger than
    ///   zero, any measure under the threshold will automatically be ignored and the
    ///   zero compensation will be updated.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the zero tracking threshold value
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ZEROTRACKING_INVALID</c>.
    /// </para>
    ///-
    function get_zeroTracking():double;

    function get_command():string;

    function set_command(newval:string):integer;

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
    ///   Use the method <c>YMultiCellWeighScale.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a string that uniquely characterizes $THEFUNCTION$
    /// </param>
    /// <returns>
    ///   a <c>YMultiCellWeighScale</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindMultiCellWeighScale(func: string):TYMultiCellWeighScale;

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
    function registerValueCallback(callback: TYMultiCellWeighScaleValueCallback):LongInt; overload;

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
    ///   arguments: the function object of which the value has changed, and an YMeasure object describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerTimedReportCallback(callback: TYMultiCellWeighScaleTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;

    ////
    /// <summary>
    ///   Adapts the load cell signal bias (stored in the corresponding genericSensor)
    ///   so that the current signal corresponds to a zero weight.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function tare():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Configures the load cells span parameters (stored in the corresponding genericSensors)
    ///   so that the current signal corresponds to the specified reference weight.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="currWeight">
    ///   reference weight presently on the load cell.
    /// </param>
    /// <param name="maxWeight">
    ///   maximum weight to be expected on the load cell.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function setupSpan(currWeight: double; maxWeight: double):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of multi-cell weighing scale sensors started using <c>yFirstMultiCellWeighScale()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned multi-cell weighing scale sensors order.
    ///   If you want to find a specific a multi-cell weighing scale sensor, use
    ///   <c>MultiCellWeighScale.findMultiCellWeighScale()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YMultiCellWeighScale</c> object, corresponding to
    ///   a multi-cell weighing scale sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more multi-cell weighing scale sensors to enumerate.
    /// </returns>
    ///-
    function nextMultiCellWeighScale():TYMultiCellWeighScale;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstMultiCellWeighScale():TYMultiCellWeighScale;
  //--- (end of YMultiCellWeighScale accessors declaration)
  end;

//--- (YMultiCellWeighScale functions declaration)
  ////
  /// <summary>
  ///   Retrieves a multi-cell weighing scale sensor for a given identifier.
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
  ///   This function does not require that the multi-cell weighing scale sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YMultiCellWeighScale.isOnline()</c> to test if the multi-cell weighing scale sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a multi-cell weighing scale sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the multi-cell weighing scale sensor
  /// </param>
  /// <returns>
  ///   a <c>YMultiCellWeighScale</c> object allowing you to drive the multi-cell weighing scale sensor.
  /// </returns>
  ///-
  function yFindMultiCellWeighScale(func:string):TYMultiCellWeighScale;
  ////
  /// <summary>
  ///   Starts the enumeration of multi-cell weighing scale sensors currently accessible.
  /// <para>
  ///   Use the method <c>YMultiCellWeighScale.nextMultiCellWeighScale()</c> to iterate on
  ///   next multi-cell weighing scale sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YMultiCellWeighScale</c> object, corresponding to
  ///   the first multi-cell weighing scale sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstMultiCellWeighScale():TYMultiCellWeighScale;

//--- (end of YMultiCellWeighScale functions declaration)

implementation
//--- (YMultiCellWeighScale dlldef)
//--- (end of YMultiCellWeighScale dlldef)

  constructor TYMultiCellWeighScale.Create(func:string);
    begin
      inherited Create(func);
      _className := 'MultiCellWeighScale';
      //--- (YMultiCellWeighScale accessors initialization)
      _cellCount := Y_CELLCOUNT_INVALID;
      _excitation := Y_EXCITATION_INVALID;
      _tempAvgAdaptRatio := Y_TEMPAVGADAPTRATIO_INVALID;
      _tempChgAdaptRatio := Y_TEMPCHGADAPTRATIO_INVALID;
      _compTempAvg := Y_COMPTEMPAVG_INVALID;
      _compTempChg := Y_COMPTEMPCHG_INVALID;
      _compensation := Y_COMPENSATION_INVALID;
      _zeroTracking := Y_ZEROTRACKING_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackMultiCellWeighScale := nil;
      _timedReportCallbackMultiCellWeighScale := nil;
      //--- (end of YMultiCellWeighScale accessors initialization)
    end;

//--- (YMultiCellWeighScale yapiwrapper)
//--- (end of YMultiCellWeighScale yapiwrapper)

//--- (YMultiCellWeighScale implementation)
{$HINTS OFF}
  function TYMultiCellWeighScale._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'cellCount') then
        begin
          _cellCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'excitation') then
        begin
          _excitation := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'tempAvgAdaptRatio') then
        begin
          _tempAvgAdaptRatio := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'tempChgAdaptRatio') then
        begin
          _tempChgAdaptRatio := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'compTempAvg') then
        begin
          _compTempAvg := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'compTempChg') then
        begin
          _compTempChg := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'compensation') then
        begin
          _compensation := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'zeroTracking') then
        begin
          _zeroTracking := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'command') then
        begin
          _command := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYMultiCellWeighScale.set_unit(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('unit',rest_val);
    end;

  function TYMultiCellWeighScale.get_cellCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CELLCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._cellCount;
      result := res;
      exit;
    end;


  function TYMultiCellWeighScale.set_cellCount(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('cellCount',rest_val);
    end;

  function TYMultiCellWeighScale.get_excitation():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_EXCITATION_INVALID;
              exit;
            end;
        end;
      res := self._excitation;
      result := res;
      exit;
    end;


  function TYMultiCellWeighScale.set_excitation(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('excitation',rest_val);
    end;

  function TYMultiCellWeighScale.set_tempAvgAdaptRatio(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('tempAvgAdaptRatio',rest_val);
    end;

  function TYMultiCellWeighScale.get_tempAvgAdaptRatio():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_TEMPAVGADAPTRATIO_INVALID;
              exit;
            end;
        end;
      res := self._tempAvgAdaptRatio;
      result := res;
      exit;
    end;


  function TYMultiCellWeighScale.set_tempChgAdaptRatio(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('tempChgAdaptRatio',rest_val);
    end;

  function TYMultiCellWeighScale.get_tempChgAdaptRatio():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_TEMPCHGADAPTRATIO_INVALID;
              exit;
            end;
        end;
      res := self._tempChgAdaptRatio;
      result := res;
      exit;
    end;


  function TYMultiCellWeighScale.get_compTempAvg():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_COMPTEMPAVG_INVALID;
              exit;
            end;
        end;
      res := self._compTempAvg;
      result := res;
      exit;
    end;


  function TYMultiCellWeighScale.get_compTempChg():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_COMPTEMPCHG_INVALID;
              exit;
            end;
        end;
      res := self._compTempChg;
      result := res;
      exit;
    end;


  function TYMultiCellWeighScale.get_compensation():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_COMPENSATION_INVALID;
              exit;
            end;
        end;
      res := self._compensation;
      result := res;
      exit;
    end;


  function TYMultiCellWeighScale.set_zeroTracking(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('zeroTracking',rest_val);
    end;

  function TYMultiCellWeighScale.get_zeroTracking():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ZEROTRACKING_INVALID;
              exit;
            end;
        end;
      res := self._zeroTracking;
      result := res;
      exit;
    end;


  function TYMultiCellWeighScale.get_command():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_COMMAND_INVALID;
              exit;
            end;
        end;
      res := self._command;
      result := res;
      exit;
    end;


  function TYMultiCellWeighScale.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYMultiCellWeighScale.FindMultiCellWeighScale(func: string):TYMultiCellWeighScale;
    var
      obj : TYMultiCellWeighScale;
    begin
      obj := TYMultiCellWeighScale(TYFunction._FindFromCache('MultiCellWeighScale', func));
      if obj = nil then
        begin
          obj :=  TYMultiCellWeighScale.create(func);
          TYFunction._AddToCache('MultiCellWeighScale',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYMultiCellWeighScale.registerValueCallback(callback: TYMultiCellWeighScaleValueCallback):LongInt;
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
      self._valueCallbackMultiCellWeighScale := callback;
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


  function TYMultiCellWeighScale._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackMultiCellWeighScale) <> nil) then
        begin
          self._valueCallbackMultiCellWeighScale(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYMultiCellWeighScale.registerTimedReportCallback(callback: TYMultiCellWeighScaleTimedReportCallback):LongInt;
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
      self._timedReportCallbackMultiCellWeighScale := callback;
      result := 0;
      exit;
    end;


  function TYMultiCellWeighScale._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackMultiCellWeighScale) <> nil) then
        begin
          self._timedReportCallbackMultiCellWeighScale(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYMultiCellWeighScale.tare():LongInt;
    begin
      result := self.set_command('T');
      exit;
    end;


  function TYMultiCellWeighScale.setupSpan(currWeight: double; maxWeight: double):LongInt;
    begin
      result := self.set_command('S'+inttostr( round(1000*currWeight))+':'+inttostr(round(1000*maxWeight)));
      exit;
    end;


  function TYMultiCellWeighScale.nextMultiCellWeighScale(): TYMultiCellWeighScale;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextMultiCellWeighScale := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextMultiCellWeighScale := nil;
          exit;
        end;
      nextMultiCellWeighScale := TYMultiCellWeighScale.FindMultiCellWeighScale(hwid);
    end;

  class function TYMultiCellWeighScale.FirstMultiCellWeighScale(): TYMultiCellWeighScale;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('MultiCellWeighScale', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYMultiCellWeighScale.FindMultiCellWeighScale(serial+'.'+funcId);
    end;

//--- (end of YMultiCellWeighScale implementation)

//--- (YMultiCellWeighScale functions)

  function yFindMultiCellWeighScale(func:string): TYMultiCellWeighScale;
    begin
      result := TYMultiCellWeighScale.FindMultiCellWeighScale(func);
    end;

  function yFirstMultiCellWeighScale(): TYMultiCellWeighScale;
    begin
      result := TYMultiCellWeighScale.FirstMultiCellWeighScale();
    end;

  procedure _MultiCellWeighScaleCleanup();
    begin
    end;

//--- (end of YMultiCellWeighScale functions)

initialization
  //--- (YMultiCellWeighScale initialization)
  //--- (end of YMultiCellWeighScale initialization)

finalization
  //--- (YMultiCellWeighScale cleanup)
  _MultiCellWeighScaleCleanup();
  //--- (end of YMultiCellWeighScale cleanup)
end.
