{*********************************************************************
 *
 *  $Id: yocto_gps.pas 33711 2018-12-14 14:19:13Z seb $
 *
 *  Implements yFindGps(), the high-level API for Gps functions
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


unit yocto_gps;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YGps definitions)

const Y_ISFIXED_FALSE = 0;
const Y_ISFIXED_TRUE = 1;
const Y_ISFIXED_INVALID = -1;
const Y_SATCOUNT_INVALID              = YAPI_INVALID_LONG;
const Y_COORDSYSTEM_GPS_DMS = 0;
const Y_COORDSYSTEM_GPS_DM = 1;
const Y_COORDSYSTEM_GPS_D = 2;
const Y_COORDSYSTEM_INVALID = -1;
const Y_LATITUDE_INVALID              = YAPI_INVALID_STRING;
const Y_LONGITUDE_INVALID             = YAPI_INVALID_STRING;
const Y_DILUTION_INVALID              = YAPI_INVALID_DOUBLE;
const Y_ALTITUDE_INVALID              = YAPI_INVALID_DOUBLE;
const Y_GROUNDSPEED_INVALID           = YAPI_INVALID_DOUBLE;
const Y_DIRECTION_INVALID             = YAPI_INVALID_DOUBLE;
const Y_UNIXTIME_INVALID              = YAPI_INVALID_LONG;
const Y_DATETIME_INVALID              = YAPI_INVALID_STRING;
const Y_UTCOFFSET_INVALID             = YAPI_INVALID_INT;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YGps definitions)
//--- (YGps yapiwrapper declaration)
//--- (end of YGps yapiwrapper declaration)

type
  TYGps = class;
  //--- (YGps class start)
  TYGpsValueCallback = procedure(func: TYGps; value:string);
  TYGpsTimedReportCallback = procedure(func: TYGps; value:TYMeasure);

  ////
  /// <summary>
  ///   TYGps Class: GPS function interface
  /// <para>
  ///   The GPS function allows you to extract positioning
  ///   data from the GPS device. This class can provides
  ///   complete positioning information: However, if you
  ///   wish to define callbacks on position changes, you
  ///   should use the YLatitude et YLongitude classes.
  /// </para>
  /// </summary>
  ///-
  TYGps=class(TYFunction)
  //--- (end of YGps class start)
  protected
  //--- (YGps declaration)
    // Attributes (function value cache)
    _isFixed                  : Integer;
    _satCount                 : int64;
    _coordSystem              : Integer;
    _latitude                 : string;
    _longitude                : string;
    _dilution                 : double;
    _altitude                 : double;
    _groundSpeed              : double;
    _direction                : double;
    _unixTime                 : int64;
    _dateTime                 : string;
    _utcOffset                : LongInt;
    _command                  : string;
    _valueCallbackGps         : TYGpsValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YGps declaration)

  public
    //--- (YGps accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns TRUE if the receiver has found enough satellites to work.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_ISFIXED_FALSE</c> or <c>Y_ISFIXED_TRUE</c>, according to TRUE if the receiver has found
    ///   enough satellites to work
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ISFIXED_INVALID</c>.
    /// </para>
    ///-
    function get_isFixed():Integer;

    ////
    /// <summary>
    ///   Returns the count of visible satellites.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the count of visible satellites
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SATCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_satCount():int64;

    ////
    /// <summary>
    ///   Returns the representation system used for positioning data.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_COORDSYSTEM_GPS_DMS</c>, <c>Y_COORDSYSTEM_GPS_DM</c> and
    ///   <c>Y_COORDSYSTEM_GPS_D</c> corresponding to the representation system used for positioning data
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_COORDSYSTEM_INVALID</c>.
    /// </para>
    ///-
    function get_coordSystem():Integer;

    ////
    /// <summary>
    ///   Changes the representation system used for positioning data.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_COORDSYSTEM_GPS_DMS</c>, <c>Y_COORDSYSTEM_GPS_DM</c> and
    ///   <c>Y_COORDSYSTEM_GPS_D</c> corresponding to the representation system used for positioning data
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
    function set_coordSystem(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the current latitude.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the current latitude
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LATITUDE_INVALID</c>.
    /// </para>
    ///-
    function get_latitude():string;

    ////
    /// <summary>
    ///   Returns the current longitude.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the current longitude
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LONGITUDE_INVALID</c>.
    /// </para>
    ///-
    function get_longitude():string;

    ////
    /// <summary>
    ///   Returns the current horizontal dilution of precision,
    ///   the smaller that number is, the better .
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current horizontal dilution of precision,
    ///   the smaller that number is, the better
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DILUTION_INVALID</c>.
    /// </para>
    ///-
    function get_dilution():double;

    ////
    /// <summary>
    ///   Returns the current altitude.
    /// <para>
    ///   Beware:  GPS technology
    ///   is very inaccurate regarding altitude.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current altitude
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ALTITUDE_INVALID</c>.
    /// </para>
    ///-
    function get_altitude():double;

    ////
    /// <summary>
    ///   Returns the current ground speed in Km/h.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current ground speed in Km/h
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_GROUNDSPEED_INVALID</c>.
    /// </para>
    ///-
    function get_groundSpeed():double;

    ////
    /// <summary>
    ///   Returns the current move bearing in degrees, zero
    ///   is the true (geographic) north.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current move bearing in degrees, zero
    ///   is the true (geographic) north
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DIRECTION_INVALID</c>.
    /// </para>
    ///-
    function get_direction():double;

    ////
    /// <summary>
    ///   Returns the current time in Unix format (number of
    ///   seconds elapsed since Jan 1st, 1970).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current time in Unix format (number of
    ///   seconds elapsed since Jan 1st, 1970)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_UNIXTIME_INVALID</c>.
    /// </para>
    ///-
    function get_unixTime():int64;

    ////
    /// <summary>
    ///   Returns the current time in the form "YYYY/MM/DD hh:mm:ss".
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the current time in the form "YYYY/MM/DD hh:mm:ss"
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DATETIME_INVALID</c>.
    /// </para>
    ///-
    function get_dateTime():string;

    ////
    /// <summary>
    ///   Returns the number of seconds between current time and UTC time (time zone).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of seconds between current time and UTC time (time zone)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_UTCOFFSET_INVALID</c>.
    /// </para>
    ///-
    function get_utcOffset():LongInt;

    ////
    /// <summary>
    ///   Changes the number of seconds between current time and UTC time (time zone).
    /// <para>
    ///   The timezone is automatically rounded to the nearest multiple of 15 minutes.
    ///   If current UTC time is known, the current time is automatically be updated according to the selected time zone.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the number of seconds between current time and UTC time (time zone)
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
    function set_utcOffset(newval:LongInt):integer;

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
    ///   Use the method <c>YGps.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YGps</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindGps(func: string):TYGps;

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
    function registerValueCallback(callback: TYGpsValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of GPS started using <c>yFirstGps()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned GPS order.
    ///   If you want to find a specific a GPS, use <c>Gps.findGps()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YGps</c> object, corresponding to
    ///   a GPS currently online, or a <c>NIL</c> pointer
    ///   if there are no more GPS to enumerate.
    /// </returns>
    ///-
    function nextGps():TYGps;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstGps():TYGps;
  //--- (end of YGps accessors declaration)
  end;

//--- (YGps functions declaration)
  ////
  /// <summary>
  ///   Retrieves a GPS for a given identifier.
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
  ///   This function does not require that the GPS is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YGps.isOnline()</c> to test if the GPS is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a GPS by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the GPS
  /// </param>
  /// <returns>
  ///   a <c>YGps</c> object allowing you to drive the GPS.
  /// </returns>
  ///-
  function yFindGps(func:string):TYGps;
  ////
  /// <summary>
  ///   Starts the enumeration of GPS currently accessible.
  /// <para>
  ///   Use the method <c>YGps.nextGps()</c> to iterate on
  ///   next GPS.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YGps</c> object, corresponding to
  ///   the first GPS currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstGps():TYGps;

//--- (end of YGps functions declaration)

implementation
//--- (YGps dlldef)
//--- (end of YGps dlldef)

  constructor TYGps.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Gps';
      //--- (YGps accessors initialization)
      _isFixed := Y_ISFIXED_INVALID;
      _satCount := Y_SATCOUNT_INVALID;
      _coordSystem := Y_COORDSYSTEM_INVALID;
      _latitude := Y_LATITUDE_INVALID;
      _longitude := Y_LONGITUDE_INVALID;
      _dilution := Y_DILUTION_INVALID;
      _altitude := Y_ALTITUDE_INVALID;
      _groundSpeed := Y_GROUNDSPEED_INVALID;
      _direction := Y_DIRECTION_INVALID;
      _unixTime := Y_UNIXTIME_INVALID;
      _dateTime := Y_DATETIME_INVALID;
      _utcOffset := Y_UTCOFFSET_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackGps := nil;
      //--- (end of YGps accessors initialization)
    end;

//--- (YGps yapiwrapper)
//--- (end of YGps yapiwrapper)

//--- (YGps implementation)
{$HINTS OFF}
  function TYGps._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'isFixed') then
        begin
          _isFixed := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'satCount') then
        begin
          _satCount := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'coordSystem') then
        begin
          _coordSystem := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'latitude') then
        begin
          _latitude := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'longitude') then
        begin
          _longitude := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'dilution') then
        begin
          _dilution := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'altitude') then
        begin
          _altitude := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'groundSpeed') then
        begin
          _groundSpeed := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'direction') then
        begin
          _direction := round(member^.ivalue * 1000.0 / 65536.0) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'unixTime') then
        begin
          _unixTime := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'dateTime') then
        begin
          _dateTime := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'utcOffset') then
        begin
          _utcOffset := integer(member^.ivalue);
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

  function TYGps.get_isFixed():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ISFIXED_INVALID;
              exit;
            end;
        end;
      res := self._isFixed;
      result := res;
      exit;
    end;


  function TYGps.get_satCount():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SATCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._satCount;
      result := res;
      exit;
    end;


  function TYGps.get_coordSystem():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_COORDSYSTEM_INVALID;
              exit;
            end;
        end;
      res := self._coordSystem;
      result := res;
      exit;
    end;


  function TYGps.set_coordSystem(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('coordSystem',rest_val);
    end;

  function TYGps.get_latitude():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LATITUDE_INVALID;
              exit;
            end;
        end;
      res := self._latitude;
      result := res;
      exit;
    end;


  function TYGps.get_longitude():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LONGITUDE_INVALID;
              exit;
            end;
        end;
      res := self._longitude;
      result := res;
      exit;
    end;


  function TYGps.get_dilution():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DILUTION_INVALID;
              exit;
            end;
        end;
      res := self._dilution;
      result := res;
      exit;
    end;


  function TYGps.get_altitude():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ALTITUDE_INVALID;
              exit;
            end;
        end;
      res := self._altitude;
      result := res;
      exit;
    end;


  function TYGps.get_groundSpeed():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_GROUNDSPEED_INVALID;
              exit;
            end;
        end;
      res := self._groundSpeed;
      result := res;
      exit;
    end;


  function TYGps.get_direction():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DIRECTION_INVALID;
              exit;
            end;
        end;
      res := self._direction;
      result := res;
      exit;
    end;


  function TYGps.get_unixTime():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_UNIXTIME_INVALID;
              exit;
            end;
        end;
      res := self._unixTime;
      result := res;
      exit;
    end;


  function TYGps.get_dateTime():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DATETIME_INVALID;
              exit;
            end;
        end;
      res := self._dateTime;
      result := res;
      exit;
    end;


  function TYGps.get_utcOffset():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_UTCOFFSET_INVALID;
              exit;
            end;
        end;
      res := self._utcOffset;
      result := res;
      exit;
    end;


  function TYGps.set_utcOffset(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('utcOffset',rest_val);
    end;

  function TYGps.get_command():string;
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


  function TYGps.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYGps.FindGps(func: string):TYGps;
    var
      obj : TYGps;
    begin
      obj := TYGps(TYFunction._FindFromCache('Gps', func));
      if obj = nil then
        begin
          obj :=  TYGps.create(func);
          TYFunction._AddToCache('Gps',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYGps.registerValueCallback(callback: TYGpsValueCallback):LongInt;
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
      self._valueCallbackGps := callback;
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


  function TYGps._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackGps) <> nil) then
        begin
          self._valueCallbackGps(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYGps.nextGps(): TYGps;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextGps := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextGps := nil;
          exit;
        end;
      nextGps := TYGps.FindGps(hwid);
    end;

  class function TYGps.FirstGps(): TYGps;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Gps', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYGps.FindGps(serial+'.'+funcId);
    end;

//--- (end of YGps implementation)

//--- (YGps functions)

  function yFindGps(func:string): TYGps;
    begin
      result := TYGps.FindGps(func);
    end;

  function yFirstGps(): TYGps;
    begin
      result := TYGps.FirstGps();
    end;

  procedure _GpsCleanup();
    begin
    end;

//--- (end of YGps functions)

initialization
  //--- (YGps initialization)
  //--- (end of YGps initialization)

finalization
  //--- (YGps cleanup)
  _GpsCleanup();
  //--- (end of YGps cleanup)
end.
