{*********************************************************************
 *
 *  $Id: yocto_segmenteddisplay.pas 35285 2019-05-07 07:37:56Z seb $
 *
 *  Implements yFindSegmentedDisplay(), the high-level API for SegmentedDisplay functions
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


unit yocto_segmenteddisplay;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YSegmentedDisplay definitions)

const Y_DISPLAYEDTEXT_INVALID         = YAPI_INVALID_STRING;
const Y_DISPLAYMODE_DISCONNECTED = 0;
const Y_DISPLAYMODE_MANUAL = 1;
const Y_DISPLAYMODE_AUTO1 = 2;
const Y_DISPLAYMODE_AUTO60 = 3;
const Y_DISPLAYMODE_INVALID = -1;


//--- (end of YSegmentedDisplay definitions)
//--- (YSegmentedDisplay yapiwrapper declaration)
//--- (end of YSegmentedDisplay yapiwrapper declaration)

type
  TYSegmentedDisplay = class;
  //--- (YSegmentedDisplay class start)
  TYSegmentedDisplayValueCallback = procedure(func: TYSegmentedDisplay; value:string);
  TYSegmentedDisplayTimedReportCallback = procedure(func: TYSegmentedDisplay; value:TYMeasure);

  ////
  /// <summary>
  ///   TYSegmentedDisplay Class: SegmentedDisplay function interface
  /// <para>
  ///   The SegmentedDisplay class allows you to drive segmented displays.
  /// </para>
  /// </summary>
  ///-
  TYSegmentedDisplay=class(TYFunction)
  //--- (end of YSegmentedDisplay class start)
  protected
  //--- (YSegmentedDisplay declaration)
    // Attributes (function value cache)
    _displayedText            : string;
    _displayMode              : Integer;
    _valueCallbackSegmentedDisplay : TYSegmentedDisplayValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YSegmentedDisplay declaration)

  public
    //--- (YSegmentedDisplay accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the text currently displayed on the screen.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the text currently displayed on the screen
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DISPLAYEDTEXT_INVALID</c>.
    /// </para>
    ///-
    function get_displayedText():string;

    ////
    /// <summary>
    ///   Changes the text currently displayed on the screen.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the text currently displayed on the screen
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
    function set_displayedText(newval:string):integer;

    function get_displayMode():Integer;

    function set_displayMode(newval:Integer):integer;

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
    ///   Use the method <c>YSegmentedDisplay.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YSegmentedDisplay</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindSegmentedDisplay(func: string):TYSegmentedDisplay;

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
    function registerValueCallback(callback: TYSegmentedDisplayValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;


    ////
    /// <summary>
    ///   Continues the enumeration of segmented displays started using <c>yFirstSegmentedDisplay()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned segmented displays order.
    ///   If you want to find a specific a segmented display, use <c>SegmentedDisplay.findSegmentedDisplay()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YSegmentedDisplay</c> object, corresponding to
    ///   a segmented display currently online, or a <c>NIL</c> pointer
    ///   if there are no more segmented displays to enumerate.
    /// </returns>
    ///-
    function nextSegmentedDisplay():TYSegmentedDisplay;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstSegmentedDisplay():TYSegmentedDisplay;
  //--- (end of YSegmentedDisplay accessors declaration)
  end;

//--- (YSegmentedDisplay functions declaration)
  ////
  /// <summary>
  ///   Retrieves a segmented display for a given identifier.
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
  ///   This function does not require that the segmented displays is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YSegmentedDisplay.isOnline()</c> to test if the segmented displays is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a segmented display by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the segmented displays
  /// </param>
  /// <returns>
  ///   a <c>YSegmentedDisplay</c> object allowing you to drive the segmented displays.
  /// </returns>
  ///-
  function yFindSegmentedDisplay(func:string):TYSegmentedDisplay;
  ////
  /// <summary>
  ///   Starts the enumeration of segmented displays currently accessible.
  /// <para>
  ///   Use the method <c>YSegmentedDisplay.nextSegmentedDisplay()</c> to iterate on
  ///   next segmented displays.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YSegmentedDisplay</c> object, corresponding to
  ///   the first segmented displays currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstSegmentedDisplay():TYSegmentedDisplay;

//--- (end of YSegmentedDisplay functions declaration)

implementation
//--- (YSegmentedDisplay dlldef)
//--- (end of YSegmentedDisplay dlldef)

  constructor TYSegmentedDisplay.Create(func:string);
    begin
      inherited Create(func);
      _className := 'SegmentedDisplay';
      //--- (YSegmentedDisplay accessors initialization)
      _displayedText := Y_DISPLAYEDTEXT_INVALID;
      _displayMode := Y_DISPLAYMODE_INVALID;
      _valueCallbackSegmentedDisplay := nil;
      //--- (end of YSegmentedDisplay accessors initialization)
    end;

//--- (YSegmentedDisplay yapiwrapper)
//--- (end of YSegmentedDisplay yapiwrapper)

//--- (YSegmentedDisplay implementation)
{$HINTS OFF}
  function TYSegmentedDisplay._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'displayedText') then
        begin
          _displayedText := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'displayMode') then
        begin
          _displayMode := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYSegmentedDisplay.get_displayedText():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DISPLAYEDTEXT_INVALID;
              exit;
            end;
        end;
      res := self._displayedText;
      result := res;
      exit;
    end;


  function TYSegmentedDisplay.set_displayedText(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('displayedText',rest_val);
    end;

  function TYSegmentedDisplay.get_displayMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DISPLAYMODE_INVALID;
              exit;
            end;
        end;
      res := self._displayMode;
      result := res;
      exit;
    end;


  function TYSegmentedDisplay.set_displayMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('displayMode',rest_val);
    end;

  class function TYSegmentedDisplay.FindSegmentedDisplay(func: string):TYSegmentedDisplay;
    var
      obj : TYSegmentedDisplay;
    begin
      obj := TYSegmentedDisplay(TYFunction._FindFromCache('SegmentedDisplay', func));
      if obj = nil then
        begin
          obj :=  TYSegmentedDisplay.create(func);
          TYFunction._AddToCache('SegmentedDisplay',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYSegmentedDisplay.registerValueCallback(callback: TYSegmentedDisplayValueCallback):LongInt;
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
      self._valueCallbackSegmentedDisplay := callback;
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


  function TYSegmentedDisplay._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackSegmentedDisplay) <> nil) then
        begin
          self._valueCallbackSegmentedDisplay(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYSegmentedDisplay.nextSegmentedDisplay(): TYSegmentedDisplay;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextSegmentedDisplay := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextSegmentedDisplay := nil;
          exit;
        end;
      nextSegmentedDisplay := TYSegmentedDisplay.FindSegmentedDisplay(hwid);
    end;

  class function TYSegmentedDisplay.FirstSegmentedDisplay(): TYSegmentedDisplay;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('SegmentedDisplay', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYSegmentedDisplay.FindSegmentedDisplay(serial+'.'+funcId);
    end;

//--- (end of YSegmentedDisplay implementation)

//--- (YSegmentedDisplay functions)

  function yFindSegmentedDisplay(func:string): TYSegmentedDisplay;
    begin
      result := TYSegmentedDisplay.FindSegmentedDisplay(func);
    end;

  function yFirstSegmentedDisplay(): TYSegmentedDisplay;
    begin
      result := TYSegmentedDisplay.FirstSegmentedDisplay();
    end;

  procedure _SegmentedDisplayCleanup();
    begin
    end;

//--- (end of YSegmentedDisplay functions)

initialization
  //--- (YSegmentedDisplay initialization)
  //--- (end of YSegmentedDisplay initialization)

finalization
  //--- (YSegmentedDisplay cleanup)
  _SegmentedDisplayCleanup();
  //--- (end of YSegmentedDisplay cleanup)
end.
