{*********************************************************************
 *
 * $Id: yocto_wakeupschedule.pas 26668 2017-02-28 13:36:03Z seb $
 *
 * Implements yFindWakeUpSchedule(), the high-level API for WakeUpSchedule functions
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


unit yocto_wakeupschedule;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YWakeUpSchedule definitions)

const Y_MINUTESA_INVALID              = YAPI_INVALID_UINT;
const Y_MINUTESB_INVALID              = YAPI_INVALID_UINT;
const Y_HOURS_INVALID                 = YAPI_INVALID_UINT;
const Y_WEEKDAYS_INVALID              = YAPI_INVALID_UINT;
const Y_MONTHDAYS_INVALID             = YAPI_INVALID_UINT;
const Y_MONTHS_INVALID                = YAPI_INVALID_UINT;
const Y_NEXTOCCURENCE_INVALID         = YAPI_INVALID_LONG;


//--- (end of YWakeUpSchedule definitions)

type
  TYWakeUpSchedule = class;
  //--- (YWakeUpSchedule class start)
  TYWakeUpScheduleValueCallback = procedure(func: TYWakeUpSchedule; value:string);
  TYWakeUpScheduleTimedReportCallback = procedure(func: TYWakeUpSchedule; value:TYMeasure);

  ////
  /// <summary>
  ///   TYWakeUpSchedule Class: WakeUpSchedule function interface
  /// <para>
  ///   The WakeUpSchedule function implements a wake up condition. The wake up time is
  ///   specified as a set of months and/or days and/or hours and/or minutes when the
  ///   wake up should happen.
  /// </para>
  /// </summary>
  ///-
  TYWakeUpSchedule=class(TYFunction)
  //--- (end of YWakeUpSchedule class start)
  protected
  //--- (YWakeUpSchedule declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _minutesA                 : LongInt;
    _minutesB                 : LongInt;
    _hours                    : LongInt;
    _weekDays                 : LongInt;
    _monthDays                : LongInt;
    _months                   : LongInt;
    _nextOccurence            : int64;
    _valueCallbackWakeUpSchedule : TYWakeUpScheduleValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YWakeUpSchedule declaration)

  public
    //--- (YWakeUpSchedule accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the minutes in the 00-29 interval of each hour scheduled for wake up.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the minutes in the 00-29 interval of each hour scheduled for wake up
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MINUTESA_INVALID</c>.
    /// </para>
    ///-
    function get_minutesA():LongInt;

    ////
    /// <summary>
    ///   Changes the minutes in the 00-29 interval when a wake up must take place.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the minutes in the 00-29 interval when a wake up must take place
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
    function set_minutesA(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the minutes in the 30-59 intervalof each hour scheduled for wake up.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the minutes in the 30-59 intervalof each hour scheduled for wake up
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MINUTESB_INVALID</c>.
    /// </para>
    ///-
    function get_minutesB():LongInt;

    ////
    /// <summary>
    ///   Changes the minutes in the 30-59 interval when a wake up must take place.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the minutes in the 30-59 interval when a wake up must take place
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
    function set_minutesB(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the hours scheduled for wake up.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the hours scheduled for wake up
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_HOURS_INVALID</c>.
    /// </para>
    ///-
    function get_hours():LongInt;

    ////
    /// <summary>
    ///   Changes the hours when a wake up must take place.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the hours when a wake up must take place
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
    function set_hours(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the days of the week scheduled for wake up.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the days of the week scheduled for wake up
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_WEEKDAYS_INVALID</c>.
    /// </para>
    ///-
    function get_weekDays():LongInt;

    ////
    /// <summary>
    ///   Changes the days of the week when a wake up must take place.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the days of the week when a wake up must take place
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
    function set_weekDays(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the days of the month scheduled for wake up.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the days of the month scheduled for wake up
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MONTHDAYS_INVALID</c>.
    /// </para>
    ///-
    function get_monthDays():LongInt;

    ////
    /// <summary>
    ///   Changes the days of the month when a wake up must take place.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the days of the month when a wake up must take place
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
    function set_monthDays(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the months scheduled for wake up.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the months scheduled for wake up
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MONTHS_INVALID</c>.
    /// </para>
    ///-
    function get_months():LongInt;

    ////
    /// <summary>
    ///   Changes the months when a wake up must take place.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the months when a wake up must take place
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
    function set_months(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the date/time (seconds) of the next wake up occurence.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the date/time (seconds) of the next wake up occurence
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_NEXTOCCURENCE_INVALID</c>.
    /// </para>
    ///-
    function get_nextOccurence():int64;

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
    ///   Use the method <c>YWakeUpSchedule.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YWakeUpSchedule</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindWakeUpSchedule(func: string):TYWakeUpSchedule;

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
    function registerValueCallback(callback: TYWakeUpScheduleValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Returns all the minutes of each hour that are scheduled for wake up.
    /// <para>
    /// </para>
    /// </summary>
    ///-
    function get_minutes():int64; overload; virtual;

    ////
    /// <summary>
    ///   Changes all the minutes where a wake up must take place.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="bitmap">
    ///   Minutes 00-59 of each hour scheduled for wake up.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_minutes(bitmap: int64):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of wake up schedules started using <c>yFirstWakeUpSchedule()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YWakeUpSchedule</c> object, corresponding to
    ///   a wake up schedule currently online, or a <c>NIL</c> pointer
    ///   if there are no more wake up schedules to enumerate.
    /// </returns>
    ///-
    function nextWakeUpSchedule():TYWakeUpSchedule;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstWakeUpSchedule():TYWakeUpSchedule;
  //--- (end of YWakeUpSchedule accessors declaration)
  end;

//--- (WakeUpSchedule functions declaration)
  ////
  /// <summary>
  ///   Retrieves a wake up schedule for a given identifier.
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
  ///   This function does not require that the wake up schedule is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YWakeUpSchedule.isOnline()</c> to test if the wake up schedule is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a wake up schedule by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the wake up schedule
  /// </param>
  /// <returns>
  ///   a <c>YWakeUpSchedule</c> object allowing you to drive the wake up schedule.
  /// </returns>
  ///-
  function yFindWakeUpSchedule(func:string):TYWakeUpSchedule;
  ////
  /// <summary>
  ///   Starts the enumeration of wake up schedules currently accessible.
  /// <para>
  ///   Use the method <c>YWakeUpSchedule.nextWakeUpSchedule()</c> to iterate on
  ///   next wake up schedules.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YWakeUpSchedule</c> object, corresponding to
  ///   the first wake up schedule currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstWakeUpSchedule():TYWakeUpSchedule;

//--- (end of WakeUpSchedule functions declaration)

implementation
//--- (YWakeUpSchedule dlldef)
//--- (end of YWakeUpSchedule dlldef)

  constructor TYWakeUpSchedule.Create(func:string);
    begin
      inherited Create(func);
      _className := 'WakeUpSchedule';
      //--- (YWakeUpSchedule accessors initialization)
      _minutesA := Y_MINUTESA_INVALID;
      _minutesB := Y_MINUTESB_INVALID;
      _hours := Y_HOURS_INVALID;
      _weekDays := Y_WEEKDAYS_INVALID;
      _monthDays := Y_MONTHDAYS_INVALID;
      _months := Y_MONTHS_INVALID;
      _nextOccurence := Y_NEXTOCCURENCE_INVALID;
      _valueCallbackWakeUpSchedule := nil;
      //--- (end of YWakeUpSchedule accessors initialization)
    end;


//--- (YWakeUpSchedule implementation)
{$HINTS OFF}
  function TYWakeUpSchedule._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'minutesA') then
        begin
          _minutesA := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'minutesB') then
        begin
          _minutesB := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'hours') then
        begin
          _hours := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'weekDays') then
        begin
          _weekDays := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'monthDays') then
        begin
          _monthDays := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'months') then
        begin
          _months := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'nextOccurence') then
        begin
          _nextOccurence := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Returns the minutes in the 00-29 interval of each hour scheduled for wake up.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the minutes in the 00-29 interval of each hour scheduled for wake up
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_MINUTESA_INVALID.
  /// </para>
  ///-
  function TYWakeUpSchedule.get_minutesA():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MINUTESA_INVALID;
              exit;
            end;
        end;
      res := self._minutesA;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the minutes in the 00-29 interval when a wake up must take place.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the minutes in the 00-29 interval when a wake up must take place
  /// </param>
  /// <para>
  /// </para>
  /// <returns>
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYWakeUpSchedule.set_minutesA(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('minutesA',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the minutes in the 30-59 intervalof each hour scheduled for wake up.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the minutes in the 30-59 intervalof each hour scheduled for wake up
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_MINUTESB_INVALID.
  /// </para>
  ///-
  function TYWakeUpSchedule.get_minutesB():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MINUTESB_INVALID;
              exit;
            end;
        end;
      res := self._minutesB;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the minutes in the 30-59 interval when a wake up must take place.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the minutes in the 30-59 interval when a wake up must take place
  /// </param>
  /// <para>
  /// </para>
  /// <returns>
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYWakeUpSchedule.set_minutesB(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('minutesB',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the hours scheduled for wake up.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the hours scheduled for wake up
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_HOURS_INVALID.
  /// </para>
  ///-
  function TYWakeUpSchedule.get_hours():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_HOURS_INVALID;
              exit;
            end;
        end;
      res := self._hours;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the hours when a wake up must take place.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the hours when a wake up must take place
  /// </param>
  /// <para>
  /// </para>
  /// <returns>
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYWakeUpSchedule.set_hours(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('hours',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the days of the week scheduled for wake up.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the days of the week scheduled for wake up
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_WEEKDAYS_INVALID.
  /// </para>
  ///-
  function TYWakeUpSchedule.get_weekDays():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_WEEKDAYS_INVALID;
              exit;
            end;
        end;
      res := self._weekDays;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the days of the week when a wake up must take place.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the days of the week when a wake up must take place
  /// </param>
  /// <para>
  /// </para>
  /// <returns>
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYWakeUpSchedule.set_weekDays(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('weekDays',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the days of the month scheduled for wake up.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the days of the month scheduled for wake up
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_MONTHDAYS_INVALID.
  /// </para>
  ///-
  function TYWakeUpSchedule.get_monthDays():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MONTHDAYS_INVALID;
              exit;
            end;
        end;
      res := self._monthDays;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the days of the month when a wake up must take place.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the days of the month when a wake up must take place
  /// </param>
  /// <para>
  /// </para>
  /// <returns>
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYWakeUpSchedule.set_monthDays(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('monthDays',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the months scheduled for wake up.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the months scheduled for wake up
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_MONTHS_INVALID.
  /// </para>
  ///-
  function TYWakeUpSchedule.get_months():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MONTHS_INVALID;
              exit;
            end;
        end;
      res := self._months;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the months when a wake up must take place.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the months when a wake up must take place
  /// </param>
  /// <para>
  /// </para>
  /// <returns>
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYWakeUpSchedule.set_months(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('months',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the date/time (seconds) of the next wake up occurence.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the date/time (seconds) of the next wake up occurence
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_NEXTOCCURENCE_INVALID.
  /// </para>
  ///-
  function TYWakeUpSchedule.get_nextOccurence():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_NEXTOCCURENCE_INVALID;
              exit;
            end;
        end;
      res := self._nextOccurence;
      result := res;
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
  ///   Use the method <c>YWakeUpSchedule.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YWakeUpSchedule</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYWakeUpSchedule.FindWakeUpSchedule(func: string):TYWakeUpSchedule;
    var
      obj : TYWakeUpSchedule;
    begin
      obj := TYWakeUpSchedule(TYFunction._FindFromCache('WakeUpSchedule', func));
      if obj = nil then
        begin
          obj :=  TYWakeUpSchedule.create(func);
          TYFunction._AddToCache('WakeUpSchedule',  func, obj);
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
  function TYWakeUpSchedule.registerValueCallback(callback: TYWakeUpScheduleValueCallback):LongInt;
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
      self._valueCallbackWakeUpSchedule := callback;
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


  function TYWakeUpSchedule._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackWakeUpSchedule) <> nil) then
        begin
          self._valueCallbackWakeUpSchedule(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns all the minutes of each hour that are scheduled for wake up.
  /// <para>
  /// </para>
  /// </summary>
  ///-
  function TYWakeUpSchedule.get_minutes():int64;
    var
      res : int64;
    begin
      res := self.get_minutesB;
      res := ((res) shl 30);
      res := res + self.get_minutesA;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes all the minutes where a wake up must take place.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="bitmap">
  ///   Minutes 00-59 of each hour scheduled for wake up.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYWakeUpSchedule.set_minutes(bitmap: int64):LongInt;
    begin
      self.set_minutesA(integer(((bitmap) and ($03fffffff))));
      bitmap := ((bitmap) shr 30);
      result := self.set_minutesB(integer(((bitmap) and ($03fffffff))));
      exit;
    end;


  function TYWakeUpSchedule.nextWakeUpSchedule(): TYWakeUpSchedule;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextWakeUpSchedule := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextWakeUpSchedule := nil;
          exit;
        end;
      nextWakeUpSchedule := TYWakeUpSchedule.FindWakeUpSchedule(hwid);
    end;

  class function TYWakeUpSchedule.FirstWakeUpSchedule(): TYWakeUpSchedule;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('WakeUpSchedule', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYWakeUpSchedule.FindWakeUpSchedule(serial+'.'+funcId);
    end;

//--- (end of YWakeUpSchedule implementation)

//--- (WakeUpSchedule functions)

  function yFindWakeUpSchedule(func:string): TYWakeUpSchedule;
    begin
      result := TYWakeUpSchedule.FindWakeUpSchedule(func);
    end;

  function yFirstWakeUpSchedule(): TYWakeUpSchedule;
    begin
      result := TYWakeUpSchedule.FirstWakeUpSchedule();
    end;

  procedure _WakeUpScheduleCleanup();
    begin
    end;

//--- (end of WakeUpSchedule functions)

initialization
  //--- (WakeUpSchedule initialization)
  //--- (end of WakeUpSchedule initialization)

finalization
  //--- (WakeUpSchedule cleanup)
  _WakeUpScheduleCleanup();
  //--- (end of WakeUpSchedule cleanup)
end.
