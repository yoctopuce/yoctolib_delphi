{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindInputCaptureData(), the high-level API for InputCaptureData functions
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


unit yocto_inputcapture;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (generated code: YInputCaptureData definitions)

//--- (end of generated code: YInputCaptureData definitions)
//--- (generated code: YInputCaptureData yapiwrapper declaration)
//--- (end of generated code: YInputCaptureData yapiwrapper declaration)

type
  TYInputCaptureData = class;
  //--- (generated code: YInputCaptureData class start)
  ////
  /// <summary>
  ///   TYInputCaptureData Class: Sampled data from a Yoctopuce electrical sensor
  /// <para>
  ///   <c>InputCaptureData</c> objects represent raw data
  ///   sampled by the analog/digital converter present in
  ///   a Yoctopuce electrical sensor. When several inputs
  ///   are samples simultaneously, their data are provided
  ///   as distinct series.
  /// </para>
  /// </summary>
  ///-
  TYInputCaptureData=class(TObject)
  //--- (end of generated code: YInputCaptureData class start)
  protected
  //--- (generated code: YInputCaptureData declaration)
    // Attributes (function value cache)
    _fmt                      : LongInt;
    _var1size                 : LongInt;
    _var2size                 : LongInt;
    _var3size                 : LongInt;
    _nVars                    : LongInt;
    _recOfs                   : LongInt;
    _nRecs                    : LongInt;
    _samplesPerSec            : LongInt;
    _trigType                 : LongInt;
    _trigVal                  : double;
    _trigPos                  : LongInt;
    _trigUTC                  : double;
    _var1unit                 : string;
    _var2unit                 : string;
    _var3unit                 : string;
    _var1samples              : TDoubleArray;
    _var2samples              : TDoubleArray;
    _var3samples              : TDoubleArray;
    //--- (end of generated code: YInputCaptureData declaration)

  public
    constructor Create(parent:TYFunction; sdata: TByteArray);

    // Method used to throw exceptions or save error type/message
    Procedure _throw(errType:YRETCODE;  errMsg:string );

    //--- (generated code: YInputCaptureData accessors declaration)

    function _decodeU16(sdata: TByteArray; ofs: LongInt):LongInt; overload; virtual;

    function _decodeU32(sdata: TByteArray; ofs: LongInt):double; overload; virtual;

    function _decodeVal(sdata: TByteArray; ofs: LongInt; len: LongInt):double; overload; virtual;

    function _decodeSnapBin(sdata: TByteArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the number of series available in the capture.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of
    ///   simultaneous data series available.
    /// </returns>
    ///-
    function get_serieCount():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the number of records captured (in a serie).
    /// <para>
    ///   In the exceptional case where it was not possible
    ///   to transfer all data in time, the number of records
    ///   actually present in the series might be lower than
    ///   the number of records captured
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of
    ///   records expected in each serie.
    /// </returns>
    ///-
    function get_recordCount():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the effective sampling rate of the device.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of
    ///   samples taken each second.
    /// </returns>
    ///-
    function get_samplingRate():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the type of automatic conditional capture
    ///   that triggered the capture of this data sequence.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the type of conditional capture.
    /// </returns>
    ///-
    function get_captureType():Integer; overload; virtual;

    ////
    /// <summary>
    ///   Returns the threshold value that triggered
    ///   this automatic conditional capture, if it was
    ///   not an instant captured triggered manually.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the conditional threshold value
    ///   at the time of capture.
    /// </returns>
    ///-
    function get_triggerValue():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the index in the series of the sample
    ///   corresponding to the exact time when the capture
    ///   was triggered.
    /// <para>
    ///   In case of trigger based on average
    ///   or RMS value, the trigger index corresponds to
    ///   the end of the averaging period.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to a position
    ///   in the data serie.
    /// </returns>
    ///-
    function get_triggerPosition():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the absolute time when the capture was
    ///   triggered, as a Unix timestamp.
    /// <para>
    ///   Milliseconds are
    ///   included in this timestamp (floating-point number).
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to
    ///   the number of seconds between the Jan 1,
    ///   1970 and the moment where the capture
    ///   was triggered.
    /// </returns>
    ///-
    function get_triggerRealTimeUTC():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the unit of measurement for data points in
    ///   the first serie.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string containing to a physical unit of
    ///   measurement.
    /// </returns>
    ///-
    function get_serie1Unit():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the unit of measurement for data points in
    ///   the second serie.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string containing to a physical unit of
    ///   measurement.
    /// </returns>
    ///-
    function get_serie2Unit():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the unit of measurement for data points in
    ///   the third serie.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string containing to a physical unit of
    ///   measurement.
    /// </returns>
    ///-
    function get_serie3Unit():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the sampled data corresponding to the first serie.
    /// <para>
    ///   The corresponding physical unit can be obtained
    ///   using the method <c>get_serie1Unit()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list of real numbers corresponding to all
    ///   samples received for serie 1.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_serie1Values():TDoubleArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns the sampled data corresponding to the second serie.
    /// <para>
    ///   The corresponding physical unit can be obtained
    ///   using the method <c>get_serie2Unit()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list of real numbers corresponding to all
    ///   samples received for serie 2.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_serie2Values():TDoubleArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns the sampled data corresponding to the third serie.
    /// <para>
    ///   The corresponding physical unit can be obtained
    ///   using the method <c>get_serie3Unit()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list of real numbers corresponding to all
    ///   samples received for serie 3.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_serie3Values():TDoubleArray; overload; virtual;


  //--- (end of generated code: YInputCaptureData accessors declaration)
  end;

//--- (generated code: YInputCaptureData functions declaration)
//--- (end of generated code: YInputCaptureData functions declaration)

//--- (generated code: YInputCapture definitions)

const Y_LASTCAPTURETIME_INVALID       = YAPI_INVALID_LONG;
const Y_NSAMPLES_INVALID              = YAPI_INVALID_UINT;
const Y_SAMPLINGRATE_INVALID          = YAPI_INVALID_UINT;
const Y_CAPTURETYPE_NONE = 0;
const Y_CAPTURETYPE_TIMED = 1;
const Y_CAPTURETYPE_V_MAX = 2;
const Y_CAPTURETYPE_V_MIN = 3;
const Y_CAPTURETYPE_I_MAX = 4;
const Y_CAPTURETYPE_I_MIN = 5;
const Y_CAPTURETYPE_P_MAX = 6;
const Y_CAPTURETYPE_P_MIN = 7;
const Y_CAPTURETYPE_V_AVG_MAX = 8;
const Y_CAPTURETYPE_V_AVG_MIN = 9;
const Y_CAPTURETYPE_V_RMS_MAX = 10;
const Y_CAPTURETYPE_V_RMS_MIN = 11;
const Y_CAPTURETYPE_I_AVG_MAX = 12;
const Y_CAPTURETYPE_I_AVG_MIN = 13;
const Y_CAPTURETYPE_I_RMS_MAX = 14;
const Y_CAPTURETYPE_I_RMS_MIN = 15;
const Y_CAPTURETYPE_P_AVG_MAX = 16;
const Y_CAPTURETYPE_P_AVG_MIN = 17;
const Y_CAPTURETYPE_PF_MIN = 18;
const Y_CAPTURETYPE_DPF_MIN = 19;
const Y_CAPTURETYPE_INVALID = -1;
const Y_CONDVALUE_INVALID             = YAPI_INVALID_DOUBLE;
const Y_CONDALIGN_INVALID             = YAPI_INVALID_UINT;
const Y_CAPTURETYPEATSTARTUP_NONE = 0;
const Y_CAPTURETYPEATSTARTUP_TIMED = 1;
const Y_CAPTURETYPEATSTARTUP_V_MAX = 2;
const Y_CAPTURETYPEATSTARTUP_V_MIN = 3;
const Y_CAPTURETYPEATSTARTUP_I_MAX = 4;
const Y_CAPTURETYPEATSTARTUP_I_MIN = 5;
const Y_CAPTURETYPEATSTARTUP_P_MAX = 6;
const Y_CAPTURETYPEATSTARTUP_P_MIN = 7;
const Y_CAPTURETYPEATSTARTUP_V_AVG_MAX = 8;
const Y_CAPTURETYPEATSTARTUP_V_AVG_MIN = 9;
const Y_CAPTURETYPEATSTARTUP_V_RMS_MAX = 10;
const Y_CAPTURETYPEATSTARTUP_V_RMS_MIN = 11;
const Y_CAPTURETYPEATSTARTUP_I_AVG_MAX = 12;
const Y_CAPTURETYPEATSTARTUP_I_AVG_MIN = 13;
const Y_CAPTURETYPEATSTARTUP_I_RMS_MAX = 14;
const Y_CAPTURETYPEATSTARTUP_I_RMS_MIN = 15;
const Y_CAPTURETYPEATSTARTUP_P_AVG_MAX = 16;
const Y_CAPTURETYPEATSTARTUP_P_AVG_MIN = 17;
const Y_CAPTURETYPEATSTARTUP_PF_MIN = 18;
const Y_CAPTURETYPEATSTARTUP_DPF_MIN = 19;
const Y_CAPTURETYPEATSTARTUP_INVALID = -1;
const Y_CONDVALUEATSTARTUP_INVALID    = YAPI_INVALID_DOUBLE;

//--- (end of generated code: YInputCapture definitions)
//--- (generated code: YInputCapture yapiwrapper declaration)
//--- (end of generated code: YInputCapture yapiwrapper declaration)

type
  TYInputCapture = class;
  //--- (generated code: YInputCapture class start)
  TYInputCaptureValueCallback = procedure(func: TYInputCapture; value:string);
  TYInputCaptureTimedReportCallback = procedure(func: TYInputCapture; value:TYMeasure);

  ////
  /// <summary>
  ///   TYInputCapture Class: instant snapshot trigger control interface
  /// <para>
  ///   The <c>YInputCapture</c> class allows you to access data samples
  ///   measured by a Yoctopuce electrical sensor. The data capture can be
  ///   triggered manually, or be configured to detect specific events.
  /// </para>
  /// </summary>
  ///-
  TYInputCapture=class(TYFunction)
  //--- (end of generated code: YInputCapture class start)
  protected
  //--- (generated code: YInputCapture declaration)
    // Attributes (function value cache)
    _lastCaptureTime          : int64;
    _nSamples                 : LongInt;
    _samplingRate             : LongInt;
    _captureType              : Integer;
    _condValue                : double;
    _condAlign                : LongInt;
    _captureTypeAtStartup     : Integer;
    _condValueAtStartup       : double;
    _valueCallbackInputCapture : TYInputCaptureValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of generated code: YInputCapture declaration)

  public
    //--- (generated code: YInputCapture accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the number of elapsed milliseconds between the module power on
    ///   and the last capture (time of trigger), or zero if no capture has been done.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of elapsed milliseconds between the module power on
    ///   and the last capture (time of trigger), or zero if no capture has been done
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputCapture.LASTCAPTURETIME_INVALID</c>.
    /// </para>
    ///-
    function get_lastCaptureTime():int64;

    ////
    /// <summary>
    ///   Returns the number of samples that will be captured.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of samples that will be captured
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputCapture.NSAMPLES_INVALID</c>.
    /// </para>
    ///-
    function get_nSamples():LongInt;

    ////
    /// <summary>
    ///   Changes the type of automatic conditional capture.
    /// <para>
    ///   The maximum number of samples depends on the device memory.
    /// </para>
    /// <para>
    ///   If you want the change to be kept after a device reboot,
    ///   make sure  to call the matching module <c>saveToFlash()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the type of automatic conditional capture
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
    function set_nSamples(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the sampling frequency, in Hz.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the sampling frequency, in Hz
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputCapture.SAMPLINGRATE_INVALID</c>.
    /// </para>
    ///-
    function get_samplingRate():LongInt;

    ////
    /// <summary>
    ///   Returns the type of automatic conditional capture.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YInputCapture.CAPTURETYPE_NONE</c>, <c>YInputCapture.CAPTURETYPE_TIMED</c>,
    ///   <c>YInputCapture.CAPTURETYPE_V_MAX</c>, <c>YInputCapture.CAPTURETYPE_V_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_I_MAX</c>, <c>YInputCapture.CAPTURETYPE_I_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_P_MAX</c>, <c>YInputCapture.CAPTURETYPE_P_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_V_AVG_MAX</c>, <c>YInputCapture.CAPTURETYPE_V_AVG_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_V_RMS_MAX</c>, <c>YInputCapture.CAPTURETYPE_V_RMS_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_I_AVG_MAX</c>, <c>YInputCapture.CAPTURETYPE_I_AVG_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_I_RMS_MAX</c>, <c>YInputCapture.CAPTURETYPE_I_RMS_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_P_AVG_MAX</c>, <c>YInputCapture.CAPTURETYPE_P_AVG_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_PF_MIN</c> and <c>YInputCapture.CAPTURETYPE_DPF_MIN</c> corresponding
    ///   to the type of automatic conditional capture
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputCapture.CAPTURETYPE_INVALID</c>.
    /// </para>
    ///-
    function get_captureType():Integer;

    ////
    /// <summary>
    ///   Changes the type of automatic conditional capture.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YInputCapture.CAPTURETYPE_NONE</c>, <c>YInputCapture.CAPTURETYPE_TIMED</c>,
    ///   <c>YInputCapture.CAPTURETYPE_V_MAX</c>, <c>YInputCapture.CAPTURETYPE_V_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_I_MAX</c>, <c>YInputCapture.CAPTURETYPE_I_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_P_MAX</c>, <c>YInputCapture.CAPTURETYPE_P_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_V_AVG_MAX</c>, <c>YInputCapture.CAPTURETYPE_V_AVG_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_V_RMS_MAX</c>, <c>YInputCapture.CAPTURETYPE_V_RMS_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_I_AVG_MAX</c>, <c>YInputCapture.CAPTURETYPE_I_AVG_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_I_RMS_MAX</c>, <c>YInputCapture.CAPTURETYPE_I_RMS_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_P_AVG_MAX</c>, <c>YInputCapture.CAPTURETYPE_P_AVG_MIN</c>,
    ///   <c>YInputCapture.CAPTURETYPE_PF_MIN</c> and <c>YInputCapture.CAPTURETYPE_DPF_MIN</c> corresponding
    ///   to the type of automatic conditional capture
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
    function set_captureType(newval:Integer):integer;

    ////
    /// <summary>
    ///   Changes current threshold value for automatic conditional capture.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to current threshold value for automatic conditional capture
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
    function set_condValue(newval:double):integer;

    ////
    /// <summary>
    ///   Returns current threshold value for automatic conditional capture.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to current threshold value for automatic conditional capture
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputCapture.CONDVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_condValue():double;

    ////
    /// <summary>
    ///   Returns the relative position of the trigger event within the capture window.
    /// <para>
    ///   When the value is 50%, the capture is centered on the event.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the relative position of the trigger event within the capture window
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputCapture.CONDALIGN_INVALID</c>.
    /// </para>
    ///-
    function get_condAlign():LongInt;

    ////
    /// <summary>
    ///   Changes the relative position of the trigger event within the capture window.
    /// <para>
    ///   The new value must be between 10% (on the left) and 90% (on the right).
    ///   When the value is 50%, the capture is centered on the event.
    /// </para>
    /// <para>
    ///   If you want the change to be kept after a device reboot,
    ///   make sure  to call the matching module <c>saveToFlash()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the relative position of the trigger event within the capture window
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
    function set_condAlign(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the type of automatic conditional capture
    ///   applied at device power on.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YInputCapture.CAPTURETYPEATSTARTUP_NONE</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_TIMED</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_V_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_V_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_I_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_I_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_P_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_P_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_V_AVG_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_V_AVG_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_V_RMS_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_V_RMS_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_I_AVG_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_I_AVG_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_I_RMS_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_I_RMS_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_P_AVG_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_P_AVG_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_PF_MIN</c>
    ///   and <c>YInputCapture.CAPTURETYPEATSTARTUP_DPF_MIN</c> corresponding to the type of automatic conditional capture
    ///   applied at device power on
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputCapture.CAPTURETYPEATSTARTUP_INVALID</c>.
    /// </para>
    ///-
    function get_captureTypeAtStartup():Integer;

    ////
    /// <summary>
    ///   Changes the type of automatic conditional capture
    ///   applied at device power on.
    /// <para>
    /// </para>
    /// <para>
    ///   If you want the change to be kept after a device reboot,
    ///   make sure  to call the matching module <c>saveToFlash()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YInputCapture.CAPTURETYPEATSTARTUP_NONE</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_TIMED</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_V_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_V_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_I_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_I_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_P_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_P_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_V_AVG_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_V_AVG_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_V_RMS_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_V_RMS_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_I_AVG_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_I_AVG_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_I_RMS_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_I_RMS_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_P_AVG_MAX</c>,
    ///   <c>YInputCapture.CAPTURETYPEATSTARTUP_P_AVG_MIN</c>, <c>YInputCapture.CAPTURETYPEATSTARTUP_PF_MIN</c>
    ///   and <c>YInputCapture.CAPTURETYPEATSTARTUP_DPF_MIN</c> corresponding to the type of automatic conditional capture
    ///   applied at device power on
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
    function set_captureTypeAtStartup(newval:Integer):integer;

    ////
    /// <summary>
    ///   Changes current threshold value for automatic conditional
    ///   capture applied at device power on.
    /// <para>
    /// </para>
    /// <para>
    ///   If you want the change to be kept after a device reboot,
    ///   make sure  to call the matching module <c>saveToFlash()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to current threshold value for automatic conditional
    ///   capture applied at device power on
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
    function set_condValueAtStartup(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the threshold value for automatic conditional
    ///   capture applied at device power on.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the threshold value for automatic conditional
    ///   capture applied at device power on
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YInputCapture.CONDVALUEATSTARTUP_INVALID</c>.
    /// </para>
    ///-
    function get_condValueAtStartup():double;

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
    ///   Use the method <c>YInputCapture.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YInputCapture</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindInputCapture(func: string):TYInputCapture;

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
    function registerValueCallback(callback: TYInputCaptureValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Returns all details about the last automatic input capture.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an <c>YInputCaptureData</c> object including
    ///   data series and all related meta-information.
    ///   On failure, throws an exception or returns an capture object.
    /// </returns>
    ///-
    function get_lastCapture():TYInputCaptureData; overload; virtual;

    ////
    /// <summary>
    ///   Returns a new immediate capture of the device inputs.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="msDuration">
    ///   duration of the capture window,
    ///   in milliseconds (eg. between 20 and 1000).
    /// </param>
    /// <returns>
    ///   an <c>YInputCaptureData</c> object including
    ///   data series for the specified duration.
    ///   On failure, throws an exception or returns an capture object.
    /// </returns>
    ///-
    function get_immediateCapture(msDuration: LongInt):TYInputCaptureData; overload; virtual;


    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    function nextInputCapture():TYInputCapture;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstInputCapture():TYInputCapture;
  //--- (end of generated code: YInputCapture accessors declaration)
  end;

//--- (generated code: YInputCapture functions declaration)
  ////
  /// <summary>
  ///   Retrieves an instant snapshot trigger for a given identifier.
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
  ///   This function does not require that the instant snapshot trigger is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YInputCapture.isOnline()</c> to test if the instant snapshot trigger is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   an instant snapshot trigger by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the instant snapshot trigger, for instance
  ///   <c>MyDevice.inputCapture</c>.
  /// </param>
  /// <returns>
  ///   a <c>YInputCapture</c> object allowing you to drive the instant snapshot trigger.
  /// </returns>
  ///-
  function yFindInputCapture(func:string):TYInputCapture;
  ////
  /// <summary>
  ///   c
  /// <para>
  ///   omment from .yc definition
  /// </para>
  /// </summary>
  ///-
  function yFirstInputCapture():TYInputCapture;

//--- (end of generated code: YInputCapture functions declaration)

implementation

//--- (generated code: YInputCaptureData dlldef)
//--- (end of generated code: YInputCaptureData dlldef)
  constructor TYInputCaptureData.Create(parent:TYFunction; sdata: TByteArray);
    begin
      //--- (generated code: YInputCaptureData accessors initialization)
      _fmt := 0;
      _var1size := 0;
      _var2size := 0;
      _var3size := 0;
      _nVars := 0;
      _recOfs := 0;
      _nRecs := 0;
      _samplesPerSec := 0;
      _trigType := 0;
      _trigVal := 0;
      _trigPos := 0;
      _trigUTC := 0;
      //--- (end of generated code: YInputCaptureData accessors initialization)
      self._decodeSnapBin(sdata);
    end;

//--- (generated code: YInputCaptureData yapiwrapper)
//--- (end of generated code: YInputCaptureData yapiwrapper)

  Procedure TYInputCaptureData._throw(errType:YRETCODE; errMsg:string );
    begin
      if not(YAPI_exceptionsDisabled) then
        Raise YAPI_Exception.Create(errType,'YoctoApi error : '+errMsg);
    end;

//--- (generated code: YInputCaptureData implementation)

  function TYInputCaptureData._decodeU16(sdata: TByteArray; ofs: LongInt):LongInt;
    var
      v : LongInt;
    begin
      v := sdata[ofs];
      v := v + 256 * sdata[ofs+1];
      result := v;
      exit;
    end;


  function TYInputCaptureData._decodeU32(sdata: TByteArray; ofs: LongInt):double;
    var
      v : double;
    begin
      v := self._decodeU16(sdata, ofs);
      v := v + 65536.0 * self._decodeU16(sdata, ofs+2);
      result := v;
      exit;
    end;


  function TYInputCaptureData._decodeVal(sdata: TByteArray; ofs: LongInt; len: LongInt):double;
    var
      v : double;
      b : double;
    begin
      v := self._decodeU16(sdata, ofs);
      b := 65536.0;
      ofs := ofs + 2;
      len := len - 2;
      while len > 0 do
        begin
          v := v + b * sdata[ofs];
          b := b * 256;
          ofs := ofs + 1;
          len := len - 1;
        end;
      if v > (b/2) then
        begin
          // negative number
          v := v - b;
        end;
      result := v;
      exit;
    end;


  function TYInputCaptureData._decodeSnapBin(sdata: TByteArray):LongInt;
    var
      buffSize : LongInt;
      recOfs : LongInt;
      ms : LongInt;
      recSize : LongInt;
      count : LongInt;
      mult1 : LongInt;
      mult2 : LongInt;
      mult3 : LongInt;
      v : double;
      var1samples_pos : LongInt;
      var2samples_pos : LongInt;
      var3samples_pos : LongInt;
    begin
      buffSize := length(sdata);
      if not(buffSize >= 24) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'Invalid snapshot data (too short)');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      self._fmt := sdata[0];
      self._var1size := sdata[1] - 48;
      self._var2size := sdata[2] - 48;
      self._var3size := sdata[3] - 48;
      if not(self._fmt = 83) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'Unsupported snapshot format');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if not((self._var1size >= 2) and(self._var1size <= 4)) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'Invalid sample size');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if not((self._var2size >= 0) and(self._var1size <= 4)) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'Invalid sample size');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if not((self._var3size >= 0) and(self._var1size <= 4)) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'Invalid sample size');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if self._var2size = 0 then
        begin
          self._nVars := 1;
        end
      else
        begin
          if self._var3size = 0 then
            begin
              self._nVars := 2;
            end
          else
            begin
              self._nVars := 3;
            end;
        end;
      recSize := self._var1size + self._var2size + self._var3size;
      self._recOfs := self._decodeU16(sdata, 4);
      self._nRecs := self._decodeU16(sdata, 6);
      self._samplesPerSec := self._decodeU16(sdata, 8);
      self._trigType := self._decodeU16(sdata, 10);
      self._trigVal := self._decodeVal(sdata, 12, 4) / 1000;
      self._trigPos := self._decodeU16(sdata, 16);
      ms := self._decodeU16(sdata, 18);
      self._trigUTC := self._decodeVal(sdata, 20, 4);
      self._trigUTC := self._trigUTC + (ms / 1000.0);
      recOfs := 24;
      while sdata[recOfs] >= 32 do
        begin
          self._var1unit := ''+self._var1unit+''+chr(sdata[recOfs]);
          recOfs := recOfs + 1;
        end;
      if self._var2size > 0 then
        begin
          recOfs := recOfs + 1;
          while sdata[recOfs] >= 32 do
            begin
              self._var2unit := ''+self._var2unit+''+chr(sdata[recOfs]);
              recOfs := recOfs + 1;
            end;
        end;
      if self._var3size > 0 then
        begin
          recOfs := recOfs + 1;
          while sdata[recOfs] >= 32 do
            begin
              self._var3unit := ''+self._var3unit+''+chr(sdata[recOfs]);
              recOfs := recOfs + 1;
            end;
        end;
      if ((recOfs) and 1) = 1 then
        begin
          // align to next word
          recOfs := recOfs + 1;
        end;
      mult1 := 1;
      mult2 := 1;
      mult3 := 1;
      if recOfs < self._recOfs then
        begin
          // load optional value multiplier
          mult1 := self._decodeU16(sdata, recOfs);
          recOfs := recOfs + 2;
          if self._var2size > 0 then
            begin
              mult2 := self._decodeU16(sdata, recOfs);
              recOfs := recOfs + 2;
            end;
          if self._var3size > 0 then
            begin
              mult3 := self._decodeU16(sdata, recOfs);
              recOfs := recOfs + 2;
            end;
        end;
      var1samples_pos := length(self._var1samples);
      SetLength(self._var1samples, var1samples_pos+self._nRecs);;
      recOfs := self._recOfs;
      count := self._nRecs;
      while (count > 0) and(recOfs + self._var1size <= buffSize) do
        begin
          v := self._decodeVal(sdata, recOfs, self._var1size) / 1000.0;
          self._var1samples[var1samples_pos] := v*mult1;
          inc(var1samples_pos);
          recOfs := recOfs + recSize;
        end;
      SetLength(self._var1samples, var1samples_pos);;
      if self._var2size > 0 then
        begin
          var2samples_pos := length(self._var2samples);
          SetLength(self._var2samples, var2samples_pos+self._nRecs);
          recOfs := self._recOfs + self._var1size;
          count := self._nRecs;
          while (count > 0) and(recOfs + self._var2size <= buffSize) do
            begin
              v := self._decodeVal(sdata, recOfs, self._var2size) / 1000.0;
              self._var2samples[var2samples_pos] := v*mult2;
              inc(var2samples_pos);
              recOfs := recOfs + recSize;
            end;
          SetLength(self._var2samples, var2samples_pos);
        end;
      if self._var3size > 0 then
        begin
          var3samples_pos := length(self._var3samples);
          SetLength(self._var3samples, var3samples_pos+self._nRecs);
          recOfs := self._recOfs + self._var1size + self._var2size;
          count := self._nRecs;
          while (count > 0) and(recOfs + self._var3size <= buffSize) do
            begin
              v := self._decodeVal(sdata, recOfs, self._var3size) / 1000.0;
              self._var3samples[var3samples_pos] := v*mult3;
              inc(var3samples_pos);
              recOfs := recOfs + recSize;
            end;
          SetLength(self._var3samples, var3samples_pos);
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYInputCaptureData.get_serieCount():LongInt;
    begin
      result := self._nVars;
      exit;
    end;


  function TYInputCaptureData.get_recordCount():LongInt;
    begin
      result := self._nRecs;
      exit;
    end;


  function TYInputCaptureData.get_samplingRate():LongInt;
    begin
      result := self._samplesPerSec;
      exit;
    end;


  function TYInputCaptureData.get_captureType():Integer;
    begin
      result := self._trigType;
      exit;
    end;


  function TYInputCaptureData.get_triggerValue():double;
    begin
      result := self._trigVal;
      exit;
    end;


  function TYInputCaptureData.get_triggerPosition():LongInt;
    begin
      result := self._trigPos;
      exit;
    end;


  function TYInputCaptureData.get_triggerRealTimeUTC():double;
    begin
      result := self._trigUTC;
      exit;
    end;


  function TYInputCaptureData.get_serie1Unit():string;
    begin
      result := self._var1unit;
      exit;
    end;


  function TYInputCaptureData.get_serie2Unit():string;
    begin
      if not(self._nVars >= 2) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'There is no serie 2 in this capture data');
          result:='';
          exit;
        end;
      result := self._var2unit;
      exit;
    end;


  function TYInputCaptureData.get_serie3Unit():string;
    begin
      if not(self._nVars >= 3) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'There is no serie 3 in this capture data');
          result:='';
          exit;
        end;
      result := self._var3unit;
      exit;
    end;


  function TYInputCaptureData.get_serie1Values():TDoubleArray;
    begin
      result := self._var1samples;
      exit;
    end;


  function TYInputCaptureData.get_serie2Values():TDoubleArray;
    begin
      if not(self._nVars >= 2) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'There is no serie 2 in this capture data');
          result:=self._var2samples;
          exit;
        end;
      result := self._var2samples;
      exit;
    end;


  function TYInputCaptureData.get_serie3Values():TDoubleArray;
    begin
      if not(self._nVars >= 3) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT,'There is no serie 3 in this capture data');
          result:=self._var3samples;
          exit;
        end;
      result := self._var3samples;
      exit;
    end;


//--- (end of generated code: YInputCaptureData implementation)

//--- (generated code: YInputCaptureData functions)

  procedure _InputCaptureDataCleanup();
    begin
    end;

//--- (end of generated code: YInputCaptureData functions)

//--- (generated code: YInputCapture dlldef)
//--- (end of generated code: YInputCapture dlldef)

  constructor TYInputCapture.Create(func:string);
    begin
      inherited Create(func);
      _className := 'InputCapture';
      //--- (generated code: YInputCapture accessors initialization)
      _lastCaptureTime := Y_LASTCAPTURETIME_INVALID;
      _nSamples := Y_NSAMPLES_INVALID;
      _samplingRate := Y_SAMPLINGRATE_INVALID;
      _captureType := Y_CAPTURETYPE_INVALID;
      _condValue := Y_CONDVALUE_INVALID;
      _condAlign := Y_CONDALIGN_INVALID;
      _captureTypeAtStartup := Y_CAPTURETYPEATSTARTUP_INVALID;
      _condValueAtStartup := Y_CONDVALUEATSTARTUP_INVALID;
      _valueCallbackInputCapture := nil;
      //--- (end of generated code: YInputCapture accessors initialization)
    end;

//--- (generated code: YInputCapture yapiwrapper)
//--- (end of generated code: YInputCapture yapiwrapper)

//--- (generated code: YInputCapture implementation)
{$HINTS OFF}
  function TYInputCapture._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'lastCaptureTime') then
        begin
          _lastCaptureTime := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'nSamples') then
        begin
          _nSamples := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'samplingRate') then
        begin
          _samplingRate := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'captureType') then
        begin
          _captureType := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'condValue') then
        begin
          _condValue := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'condAlign') then
        begin
          _condAlign := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'captureTypeAtStartup') then
        begin
          _captureTypeAtStartup := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'condValueAtStartup') then
        begin
          _condValueAtStartup := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYInputCapture.get_lastCaptureTime():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LASTCAPTURETIME_INVALID;
              exit;
            end;
        end;
      res := self._lastCaptureTime;
      result := res;
      exit;
    end;


  function TYInputCapture.get_nSamples():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NSAMPLES_INVALID;
              exit;
            end;
        end;
      res := self._nSamples;
      result := res;
      exit;
    end;


  function TYInputCapture.set_nSamples(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('nSamples',rest_val);
    end;

  function TYInputCapture.get_samplingRate():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SAMPLINGRATE_INVALID;
              exit;
            end;
        end;
      res := self._samplingRate;
      result := res;
      exit;
    end;


  function TYInputCapture.get_captureType():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CAPTURETYPE_INVALID;
              exit;
            end;
        end;
      res := self._captureType;
      result := res;
      exit;
    end;


  function TYInputCapture.set_captureType(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('captureType',rest_val);
    end;

  function TYInputCapture.set_condValue(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('condValue',rest_val);
    end;

  function TYInputCapture.get_condValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CONDVALUE_INVALID;
              exit;
            end;
        end;
      res := self._condValue;
      result := res;
      exit;
    end;


  function TYInputCapture.get_condAlign():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CONDALIGN_INVALID;
              exit;
            end;
        end;
      res := self._condAlign;
      result := res;
      exit;
    end;


  function TYInputCapture.set_condAlign(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('condAlign',rest_val);
    end;

  function TYInputCapture.get_captureTypeAtStartup():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CAPTURETYPEATSTARTUP_INVALID;
              exit;
            end;
        end;
      res := self._captureTypeAtStartup;
      result := res;
      exit;
    end;


  function TYInputCapture.set_captureTypeAtStartup(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('captureTypeAtStartup',rest_val);
    end;

  function TYInputCapture.set_condValueAtStartup(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('condValueAtStartup',rest_val);
    end;

  function TYInputCapture.get_condValueAtStartup():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CONDVALUEATSTARTUP_INVALID;
              exit;
            end;
        end;
      res := self._condValueAtStartup;
      result := res;
      exit;
    end;


  class function TYInputCapture.FindInputCapture(func: string):TYInputCapture;
    var
      obj : TYInputCapture;
    begin
      obj := TYInputCapture(TYFunction._FindFromCache('InputCapture', func));
      if obj = nil then
        begin
          obj :=  TYInputCapture.create(func);
          TYFunction._AddToCache('InputCapture', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYInputCapture.registerValueCallback(callback: TYInputCaptureValueCallback):LongInt;
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
      self._valueCallbackInputCapture := callback;
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


  function TYInputCapture._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackInputCapture) <> nil) then
        begin
          self._valueCallbackInputCapture(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYInputCapture.get_lastCapture():TYInputCaptureData;
    var
      snapData : TByteArray;
    begin
      snapData := self._download('snap.bin');
      result := TYInputCaptureData.create(self, snapData);
      exit;
    end;


  function TYInputCapture.get_immediateCapture(msDuration: LongInt):TYInputCaptureData;
    var
      snapUrl : string;
      snapData : TByteArray;
      snapStart : LongInt;
    begin
      if msDuration < 1 then
        begin
          msDuration := 20;
        end;
      if msDuration > 1000 then
        begin
          msDuration := 1000;
        end;
      snapStart := (-msDuration div 2);
      snapUrl := 'snap.bin?t='+inttostr(snapStart)+'&d='+inttostr(msDuration);

      snapData := self._download(snapUrl);
      result := TYInputCaptureData.create(self, snapData);
      exit;
    end;


  function TYInputCapture.nextInputCapture(): TYInputCapture;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextInputCapture := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextInputCapture := nil;
          exit;
        end;
      nextInputCapture := TYInputCapture.FindInputCapture(hwid);
    end;

  class function TYInputCapture.FirstInputCapture(): TYInputCapture;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('InputCapture', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYInputCapture.FindInputCapture(serial+'.'+funcId);
    end;

//--- (end of generated code: YInputCapture implementation)

//--- (generated code: YInputCapture functions)

  function yFindInputCapture(func:string): TYInputCapture;
    begin
      result := TYInputCapture.FindInputCapture(func);
    end;

  function yFirstInputCapture(): TYInputCapture;
    begin
      result := TYInputCapture.FirstInputCapture();
    end;

  procedure _InputCaptureCleanup();
    begin
    end;

//--- (end of generated code: YInputCapture functions)

initialization
  //--- (generated code: YInputCaptureData initialization)
  //--- (end of generated code: YInputCaptureData initialization)

  //--- (generated code: YInputCapture initialization)
  //--- (end of generated code: YInputCapture initialization)

finalization
  //--- (generated code: YInputCaptureData cleanup)
  //--- (end of generated code: YInputCaptureData cleanup)

  //--- (generated code: YInputCapture cleanup)
  _InputCaptureCleanup();
  //--- (end of generated code: YInputCapture cleanup)
end.
