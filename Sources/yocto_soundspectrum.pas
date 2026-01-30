{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindSoundSpectrum(), the high-level API for SoundSpectrum functions
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


unit yocto_soundspectrum;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YSoundSpectrum definitions)

const Y_INTEGRATIONTIME_INVALID       = YAPI_INVALID_UINT;
const Y_SPECTRUMDATA_INVALID          = YAPI_INVALID_STRING;

//--- (end of YSoundSpectrum definitions)

//--- (YSoundSpectrum yapiwrapper declaration)
//--- (end of YSoundSpectrum yapiwrapper declaration)

type

  TYSoundSpectrum = class;
  //--- (YSoundSpectrum class start)
  TYSoundSpectrumValueCallback = procedure(func: TYSoundSpectrum; value:string);
  TYSoundSpectrumTimedReportCallback = procedure(func: TYSoundSpectrum; value:TYMeasure);

  ////
  /// <summary>
  ///   TYSoundSpectrum Class: sound spectrum analyzer control interface
  /// <para>
  ///   The <c>YSoundSpectrum</c> class allows you to read and configure Yoctopuce sound spectrum analyzers.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYSoundSpectrum=class(TYFunction)
  //--- (end of YSoundSpectrum class start)
  protected
  //--- (YSoundSpectrum declaration)
    // Attributes (function value cache)
    _integrationTime          : LongInt;
    _spectrumData             : string;
    _valueCallbackSoundSpectrum : TYSoundSpectrumValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YSoundSpectrum declaration)

  public
    //--- (YSoundSpectrum accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the integration time in milliseconds for calculating time
    ///   weighted spectrum data.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the integration time in milliseconds for calculating time
    ///   weighted spectrum data
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSoundSpectrum.INTEGRATIONTIME_INVALID</c>.
    /// </para>
    ///-
    function get_integrationTime():LongInt;

    ////
    /// <summary>
    ///   Changes the integration time in milliseconds for computing time weighted
    ///   spectrum data.
    /// <para>
    ///   Be aware that on some devices, changing the integration
    ///   time for time-weighted spectrum data may also affect the integration
    ///   period for one or more sound pressure level measurements.
    ///   Remember to call the <c>saveToFlash()</c> method of the
    ///   module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the integration time in milliseconds for computing time weighted
    ///   spectrum data
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
    function set_integrationTime(newval:LongInt):integer;

    function get_spectrumData():string;

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
    ///   Use the method <c>YSoundSpectrum.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YSoundSpectrum</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindSoundSpectrum(func: string):TYSoundSpectrum;

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
    function registerValueCallback(callback: TYSoundSpectrumValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    function nextSoundSpectrum():TYSoundSpectrum;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstSoundSpectrum():TYSoundSpectrum;
  //--- (end of YSoundSpectrum accessors declaration)
  end;

//--- (YSoundSpectrum functions declaration)
  ////
  /// <summary>
  ///   Retrieves a sound spectrum analyzer for a given identifier.
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
  ///   This function does not require that the sound spectrum analyzer is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YSoundSpectrum.isOnline()</c> to test if the sound spectrum analyzer is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a sound spectrum analyzer by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the sound spectrum analyzer, for instance
  ///   <c>MyDevice.soundSpectrum</c>.
  /// </param>
  /// <returns>
  ///   a <c>YSoundSpectrum</c> object allowing you to drive the sound spectrum analyzer.
  /// </returns>
  ///-
  function yFindSoundSpectrum(func:string):TYSoundSpectrum;
  ////
  /// <summary>
  ///   c
  /// <para>
  ///   omment from .yc definition
  /// </para>
  /// </summary>
  ///-
  function yFirstSoundSpectrum():TYSoundSpectrum;

//--- (end of YSoundSpectrum functions declaration)

implementation

//--- (YSoundSpectrum dlldef)
//--- (end of YSoundSpectrum dlldef)

  constructor TYSoundSpectrum.Create(func:string);
    begin
      inherited Create(func);
      _className := 'SoundSpectrum';
      //--- (YSoundSpectrum accessors initialization)
      _integrationTime := Y_INTEGRATIONTIME_INVALID;
      _spectrumData := Y_SPECTRUMDATA_INVALID;
      _valueCallbackSoundSpectrum := nil;
      //--- (end of YSoundSpectrum accessors initialization)
    end;

//--- (YSoundSpectrum yapiwrapper)
//--- (end of YSoundSpectrum yapiwrapper)

//--- (YSoundSpectrum implementation)
{$HINTS OFF}
  function TYSoundSpectrum._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'integrationTime') then
        begin
          _integrationTime := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'spectrumData') then
        begin
          _spectrumData := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYSoundSpectrum.get_integrationTime():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_INTEGRATIONTIME_INVALID;
              exit;
            end;
        end;
      res := self._integrationTime;
      result := res;
      exit;
    end;


  function TYSoundSpectrum.set_integrationTime(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('integrationTime',rest_val);
    end;

  function TYSoundSpectrum.get_spectrumData():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SPECTRUMDATA_INVALID;
              exit;
            end;
        end;
      res := self._spectrumData;
      result := res;
      exit;
    end;


  class function TYSoundSpectrum.FindSoundSpectrum(func: string):TYSoundSpectrum;
    var
      obj : TYSoundSpectrum;
    begin
      obj := TYSoundSpectrum(TYFunction._FindFromCache('SoundSpectrum', func));
      if (obj = nil) then
        begin
          obj :=  TYSoundSpectrum.create(func);
          TYFunction._AddToCache('SoundSpectrum', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYSoundSpectrum.registerValueCallback(callback: TYSoundSpectrumValueCallback):LongInt;
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
      self._valueCallbackSoundSpectrum := callback;
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


  function TYSoundSpectrum._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackSoundSpectrum) <> nil) then
        begin
          self._valueCallbackSoundSpectrum(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYSoundSpectrum.nextSoundSpectrum(): TYSoundSpectrum;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextSoundSpectrum := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextSoundSpectrum := nil;
          exit;
        end;
      nextSoundSpectrum := TYSoundSpectrum.FindSoundSpectrum(hwid);
    end;

  class function TYSoundSpectrum.FirstSoundSpectrum(): TYSoundSpectrum;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('SoundSpectrum', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYSoundSpectrum.FindSoundSpectrum(serial+'.'+funcId);
    end;

//--- (end of YSoundSpectrum implementation)

//--- (YSoundSpectrum functions)

  function yFindSoundSpectrum(func:string): TYSoundSpectrum;
    begin
      result := TYSoundSpectrum.FindSoundSpectrum(func);
    end;

  function yFirstSoundSpectrum(): TYSoundSpectrum;
    begin
      result := TYSoundSpectrum.FirstSoundSpectrum();
    end;

  procedure _SoundSpectrumCleanup();
    begin
    end;

//--- (end of YSoundSpectrum functions)

initialization
  //--- (YSoundSpectrum initialization)
  //--- (end of YSoundSpectrum initialization)

finalization
  //--- (YSoundSpectrum cleanup)
  _SoundSpectrumCleanup();
  //--- (end of YSoundSpectrum cleanup)

end.
