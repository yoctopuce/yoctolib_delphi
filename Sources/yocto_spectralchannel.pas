{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindSpectralChannel(), the high-level API for SpectralChannel functions
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


unit yocto_spectralchannel;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YSpectralChannel definitions)

const Y_RAWCOUNT_INVALID              = YAPI_INVALID_INT;
const Y_CHANNELNAME_INVALID           = YAPI_INVALID_STRING;
const Y_PEAKWAVELENGTH_INVALID        = YAPI_INVALID_INT;

//--- (end of YSpectralChannel definitions)

//--- (YSpectralChannel yapiwrapper declaration)
//--- (end of YSpectralChannel yapiwrapper declaration)

type

  TYSpectralChannel = class;
  //--- (YSpectralChannel class start)
  TYSpectralChannelValueCallback = procedure(func: TYSpectralChannel; value:string);
  TYSpectralChannelTimedReportCallback = procedure(func: TYSpectralChannel; value:TYMeasure);

  ////
  /// <summary>
  ///   TYSpectralChannel Class: spectral analysis channel control interface
  /// <para>
  ///   The <c>YSpectralChannel</c> class allows you to read and configure Yoctopuce spectral analysis channels.
  ///   It inherits from <c>YSensor</c> class the core functions to read measurements,
  ///   to register callback functions, and to access the autonomous datalogger.
  /// </para>
  /// </summary>
  ///-
  TYSpectralChannel=class(TYSensor)
  //--- (end of YSpectralChannel class start)
  protected
  //--- (YSpectralChannel declaration)
    // Attributes (function value cache)
    _rawCount                 : LongInt;
    _channelName              : string;
    _peakWavelength           : LongInt;
    _valueCallbackSpectralChannel : TYSpectralChannelValueCallback;
    _timedReportCallbackSpectralChannel : TYSpectralChannelTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YSpectralChannel declaration)

  public
    //--- (YSpectralChannel accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Retrieves the raw spectral intensity value as measured by the sensor, without any scaling or calibration.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralChannel.RAWCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_rawCount():LongInt;

    ////
    /// <summary>
    ///   Returns the target spectral band name.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the target spectral band name
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralChannel.CHANNELNAME_INVALID</c>.
    /// </para>
    ///-
    function get_channelName():string;

    ////
    /// <summary>
    ///   Returns the target spectral band peak wavelength, in nm.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the target spectral band peak wavelength, in nm
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSpectralChannel.PEAKWAVELENGTH_INVALID</c>.
    /// </para>
    ///-
    function get_peakWavelength():LongInt;

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
    ///   Use the method <c>YSpectralChannel.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YSpectralChannel</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindSpectralChannel(func: string):TYSpectralChannel;

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
    function registerValueCallback(callback: TYSpectralChannelValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYSpectralChannelTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of spectral analysis channels started using <c>yFirstSpectralChannel()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned spectral analysis channels order.
    ///   If you want to find a specific a spectral analysis channel, use <c>SpectralChannel.findSpectralChannel()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YSpectralChannel</c> object, corresponding to
    ///   a spectral analysis channel currently online, or a <c>NIL</c> pointer
    ///   if there are no more spectral analysis channels to enumerate.
    /// </returns>
    ///-
    function nextSpectralChannel():TYSpectralChannel;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstSpectralChannel():TYSpectralChannel;
  //--- (end of YSpectralChannel accessors declaration)
  end;

//--- (YSpectralChannel functions declaration)
  ////
  /// <summary>
  ///   Retrieves a spectral analysis channel for a given identifier.
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
  ///   This function does not require that the spectral analysis channel is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YSpectralChannel.isOnline()</c> to test if the spectral analysis channel is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a spectral analysis channel by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the spectral analysis channel, for instance
  ///   <c>MyDevice.spectralChannel1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YSpectralChannel</c> object allowing you to drive the spectral analysis channel.
  /// </returns>
  ///-
  function yFindSpectralChannel(func:string):TYSpectralChannel;
  ////
  /// <summary>
  ///   Starts the enumeration of spectral analysis channels currently accessible.
  /// <para>
  ///   Use the method <c>YSpectralChannel.nextSpectralChannel()</c> to iterate on
  ///   next spectral analysis channels.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YSpectralChannel</c> object, corresponding to
  ///   the first spectral analysis channel currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstSpectralChannel():TYSpectralChannel;

//--- (end of YSpectralChannel functions declaration)

implementation

//--- (YSpectralChannel dlldef)
//--- (end of YSpectralChannel dlldef)

  constructor TYSpectralChannel.Create(func:string);
    begin
      inherited Create(func);
      _className := 'SpectralChannel';
      //--- (YSpectralChannel accessors initialization)
      _rawCount := Y_RAWCOUNT_INVALID;
      _channelName := Y_CHANNELNAME_INVALID;
      _peakWavelength := Y_PEAKWAVELENGTH_INVALID;
      _valueCallbackSpectralChannel := nil;
      _timedReportCallbackSpectralChannel := nil;
      //--- (end of YSpectralChannel accessors initialization)
    end;

//--- (YSpectralChannel yapiwrapper)
//--- (end of YSpectralChannel yapiwrapper)

//--- (YSpectralChannel implementation)
{$HINTS OFF}
  function TYSpectralChannel._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'rawCount') then
        begin
          _rawCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'channelName') then
        begin
          _channelName := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'peakWavelength') then
        begin
          _peakWavelength := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYSpectralChannel.get_rawCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RAWCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._rawCount;
      result := res;
      exit;
    end;


  function TYSpectralChannel.get_channelName():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CHANNELNAME_INVALID;
              exit;
            end;
        end;
      res := self._channelName;
      result := res;
      exit;
    end;


  function TYSpectralChannel.get_peakWavelength():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PEAKWAVELENGTH_INVALID;
              exit;
            end;
        end;
      res := self._peakWavelength;
      result := res;
      exit;
    end;


  class function TYSpectralChannel.FindSpectralChannel(func: string):TYSpectralChannel;
    var
      obj : TYSpectralChannel;
    begin
      obj := TYSpectralChannel(TYFunction._FindFromCache('SpectralChannel', func));
      if obj = nil then
        begin
          obj :=  TYSpectralChannel.create(func);
          TYFunction._AddToCache('SpectralChannel', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYSpectralChannel.registerValueCallback(callback: TYSpectralChannelValueCallback):LongInt;
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
      self._valueCallbackSpectralChannel := callback;
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


  function TYSpectralChannel._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackSpectralChannel) <> nil) then
        begin
          self._valueCallbackSpectralChannel(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYSpectralChannel.registerTimedReportCallback(callback: TYSpectralChannelTimedReportCallback):LongInt;
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
      self._timedReportCallbackSpectralChannel := callback;
      result := 0;
      exit;
    end;


  function TYSpectralChannel._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackSpectralChannel) <> nil) then
        begin
          self._timedReportCallbackSpectralChannel(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYSpectralChannel.nextSpectralChannel(): TYSpectralChannel;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextSpectralChannel := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextSpectralChannel := nil;
          exit;
        end;
      nextSpectralChannel := TYSpectralChannel.FindSpectralChannel(hwid);
    end;

  class function TYSpectralChannel.FirstSpectralChannel(): TYSpectralChannel;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('SpectralChannel', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYSpectralChannel.FindSpectralChannel(serial+'.'+funcId);
    end;

//--- (end of YSpectralChannel implementation)

//--- (YSpectralChannel functions)

  function yFindSpectralChannel(func:string): TYSpectralChannel;
    begin
      result := TYSpectralChannel.FindSpectralChannel(func);
    end;

  function yFirstSpectralChannel(): TYSpectralChannel;
    begin
      result := TYSpectralChannel.FirstSpectralChannel();
    end;

  procedure _SpectralChannelCleanup();
    begin
    end;

//--- (end of YSpectralChannel functions)

initialization
  //--- (YSpectralChannel initialization)
  //--- (end of YSpectralChannel initialization)

finalization
  //--- (YSpectralChannel cleanup)
  _SpectralChannelCleanup();
  //--- (end of YSpectralChannel cleanup)

end.
