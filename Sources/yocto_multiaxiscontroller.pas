{*********************************************************************
 *
 *  $Id: yocto_multiaxiscontroller.pas 63506 2024-11-28 10:42:13Z seb $
 *
 *  Implements yFindMultiAxisController(), the high-level API for MultiAxisController functions
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


unit yocto_multiaxiscontroller;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YMultiAxisController definitions)

const Y_NAXIS_INVALID                 = YAPI_INVALID_UINT;
const Y_GLOBALSTATE_ABSENT = 0;
const Y_GLOBALSTATE_ALERT = 1;
const Y_GLOBALSTATE_HI_Z = 2;
const Y_GLOBALSTATE_STOP = 3;
const Y_GLOBALSTATE_RUN = 4;
const Y_GLOBALSTATE_BATCH = 5;
const Y_GLOBALSTATE_INVALID = -1;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;

//--- (end of YMultiAxisController definitions)

//--- (YMultiAxisController yapiwrapper declaration)
//--- (end of YMultiAxisController yapiwrapper declaration)

type

  TYMultiAxisController = class;
  //--- (YMultiAxisController class start)
  TYMultiAxisControllerValueCallback = procedure(func: TYMultiAxisController; value:string);
  TYMultiAxisControllerTimedReportCallback = procedure(func: TYMultiAxisController; value:TYMeasure);

  ////
  /// <summary>
  ///   TYMultiAxisController Class: MultiAxisController function interface
  /// <para>
  ///   The <c>YMultiAxisController</c> class allows you to drive multiple stepper motors
  ///   synchronously.
  /// </para>
  /// </summary>
  ///-
  TYMultiAxisController=class(TYFunction)
  //--- (end of YMultiAxisController class start)
  protected
  //--- (YMultiAxisController declaration)
    // Attributes (function value cache)
    _nAxis                    : LongInt;
    _globalState              : Integer;
    _command                  : string;
    _valueCallbackMultiAxisController : TYMultiAxisControllerValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YMultiAxisController declaration)

  public
    //--- (YMultiAxisController accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the number of synchronized controllers.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of synchronized controllers
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMultiAxisController.NAXIS_INVALID</c>.
    /// </para>
    ///-
    function get_nAxis():LongInt;

    ////
    /// <summary>
    ///   Changes the number of synchronized controllers.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the number of synchronized controllers
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
    function set_nAxis(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the stepper motor set overall state.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YMultiAxisController.GLOBALSTATE_ABSENT</c>,
    ///   <c>YMultiAxisController.GLOBALSTATE_ALERT</c>, <c>YMultiAxisController.GLOBALSTATE_HI_Z</c>,
    ///   <c>YMultiAxisController.GLOBALSTATE_STOP</c>, <c>YMultiAxisController.GLOBALSTATE_RUN</c> and
    ///   <c>YMultiAxisController.GLOBALSTATE_BATCH</c> corresponding to the stepper motor set overall state
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMultiAxisController.GLOBALSTATE_INVALID</c>.
    /// </para>
    ///-
    function get_globalState():Integer;

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
    ///   Use the method <c>YMultiAxisController.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YMultiAxisController</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindMultiAxisController(func: string):TYMultiAxisController;

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
    function registerValueCallback(callback: TYMultiAxisControllerValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function sendCommand(command: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reinitialize all controllers and clear all alert flags.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function reset():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts all motors backward at the specified speeds, to search for the motor home position.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="speed">
    ///   desired speed for all axis, in steps per second.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function findHomePosition(speed: TDoubleArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts all motors synchronously to reach a given absolute position.
    /// <para>
    ///   The time needed to reach the requested position will depend on the lowest
    ///   acceleration and max speed parameters configured for all motors.
    ///   The final position will be reached on all axis at the same time.
    /// </para>
    /// </summary>
    /// <param name="absPos">
    ///   absolute position, measured in steps from each origin.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function moveTo(absPos: TDoubleArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts all motors synchronously to reach a given relative position.
    /// <para>
    ///   The time needed to reach the requested position will depend on the lowest
    ///   acceleration and max speed parameters configured for all motors.
    ///   The final position will be reached on all axis at the same time.
    /// </para>
    /// </summary>
    /// <param name="relPos">
    ///   relative position, measured in steps from the current position.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function moveRel(relPos: TDoubleArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Keep the motor in the same state for the specified amount of time, before processing next command.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="waitMs">
    ///   wait time, specified in milliseconds.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function pause(waitMs: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops the motor with an emergency alert, without taking any additional precaution.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function emergencyStop():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops the motor smoothly as soon as possible, without waiting for ongoing move completion.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function abortAndBrake():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Turn the controller into Hi-Z mode immediately, without waiting for ongoing move completion.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function abortAndHiZ():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of multi-axis controllers started using <c>yFirstMultiAxisController()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned multi-axis controllers order.
    ///   If you want to find a specific a multi-axis controller, use <c>MultiAxisController.findMultiAxisController()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YMultiAxisController</c> object, corresponding to
    ///   a multi-axis controller currently online, or a <c>NIL</c> pointer
    ///   if there are no more multi-axis controllers to enumerate.
    /// </returns>
    ///-
    function nextMultiAxisController():TYMultiAxisController;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstMultiAxisController():TYMultiAxisController;
  //--- (end of YMultiAxisController accessors declaration)
  end;

//--- (YMultiAxisController functions declaration)
  ////
  /// <summary>
  ///   Retrieves a multi-axis controller for a given identifier.
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
  ///   This function does not require that the multi-axis controller is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YMultiAxisController.isOnline()</c> to test if the multi-axis controller is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a multi-axis controller by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the multi-axis controller, for instance
  ///   <c>MyDevice.multiAxisController</c>.
  /// </param>
  /// <returns>
  ///   a <c>YMultiAxisController</c> object allowing you to drive the multi-axis controller.
  /// </returns>
  ///-
  function yFindMultiAxisController(func:string):TYMultiAxisController;
  ////
  /// <summary>
  ///   Starts the enumeration of multi-axis controllers currently accessible.
  /// <para>
  ///   Use the method <c>YMultiAxisController.nextMultiAxisController()</c> to iterate on
  ///   next multi-axis controllers.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YMultiAxisController</c> object, corresponding to
  ///   the first multi-axis controller currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstMultiAxisController():TYMultiAxisController;

//--- (end of YMultiAxisController functions declaration)

implementation

//--- (YMultiAxisController dlldef)
//--- (end of YMultiAxisController dlldef)

  constructor TYMultiAxisController.Create(func:string);
    begin
      inherited Create(func);
      _className := 'MultiAxisController';
      //--- (YMultiAxisController accessors initialization)
      _nAxis := Y_NAXIS_INVALID;
      _globalState := Y_GLOBALSTATE_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackMultiAxisController := nil;
      //--- (end of YMultiAxisController accessors initialization)
    end;

//--- (YMultiAxisController yapiwrapper)
//--- (end of YMultiAxisController yapiwrapper)

//--- (YMultiAxisController implementation)
{$HINTS OFF}
  function TYMultiAxisController._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'nAxis') then
        begin
          _nAxis := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'globalState') then
        begin
          _globalState := integer(member^.ivalue);
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

  function TYMultiAxisController.get_nAxis():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NAXIS_INVALID;
              exit;
            end;
        end;
      res := self._nAxis;
      result := res;
      exit;
    end;


  function TYMultiAxisController.set_nAxis(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('nAxis',rest_val);
    end;

  function TYMultiAxisController.get_globalState():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_GLOBALSTATE_INVALID;
              exit;
            end;
        end;
      res := self._globalState;
      result := res;
      exit;
    end;


  function TYMultiAxisController.get_command():string;
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


  function TYMultiAxisController.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYMultiAxisController.FindMultiAxisController(func: string):TYMultiAxisController;
    var
      obj : TYMultiAxisController;
    begin
      obj := TYMultiAxisController(TYFunction._FindFromCache('MultiAxisController', func));
      if obj = nil then
        begin
          obj :=  TYMultiAxisController.create(func);
          TYFunction._AddToCache('MultiAxisController', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYMultiAxisController.registerValueCallback(callback: TYMultiAxisControllerValueCallback):LongInt;
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
      self._valueCallbackMultiAxisController := callback;
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


  function TYMultiAxisController._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackMultiAxisController) <> nil) then
        begin
          self._valueCallbackMultiAxisController(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYMultiAxisController.sendCommand(command: string):LongInt;
    var
      url : string;
      retBin : TByteArray;
      res : LongInt;
    begin
      url := 'cmd.txt?X='+command;
      //may throw an exception
      retBin := self._download(url);
      res := retBin[0];
      if res < 58 then
        begin
          if not(res = 48) then
            begin
              self._throw(YAPI_DEVICE_BUSY,'Motor command pipeline is full, try again later');
              result:=YAPI_DEVICE_BUSY;
              exit;
            end;
        end
      else
        begin
          if not(res = 48) then
            begin
              self._throw(YAPI_IO_ERROR,'Motor command failed permanently');
              result:=YAPI_IO_ERROR;
              exit;
            end;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYMultiAxisController.reset():LongInt;
    begin
      result := self.set_command('Z');
      exit;
    end;


  function TYMultiAxisController.findHomePosition(speed: TDoubleArray):LongInt;
    var
      cmd : string;
      i : LongInt;
      ndim : LongInt;
    begin
      ndim := length(speed);
      cmd := 'H'+inttostr(round(1000*speed[0]));
      i := 1;
      while i < ndim do
        begin
          cmd := ''+cmd+','+inttostr(round(1000*speed[i]));
          i := i + 1;
        end;
      result := self.sendCommand(cmd);
      exit;
    end;


  function TYMultiAxisController.moveTo(absPos: TDoubleArray):LongInt;
    var
      cmd : string;
      i : LongInt;
      ndim : LongInt;
    begin
      ndim := length(absPos);
      cmd := 'M'+inttostr(round(16*absPos[0]));
      i := 1;
      while i < ndim do
        begin
          cmd := ''+cmd+','+inttostr(round(16*absPos[i]));
          i := i + 1;
        end;
      result := self.sendCommand(cmd);
      exit;
    end;


  function TYMultiAxisController.moveRel(relPos: TDoubleArray):LongInt;
    var
      cmd : string;
      i : LongInt;
      ndim : LongInt;
    begin
      ndim := length(relPos);
      cmd := 'm'+inttostr(round(16*relPos[0]));
      i := 1;
      while i < ndim do
        begin
          cmd := ''+cmd+','+inttostr(round(16*relPos[i]));
          i := i + 1;
        end;
      result := self.sendCommand(cmd);
      exit;
    end;


  function TYMultiAxisController.pause(waitMs: LongInt):LongInt;
    begin
      result := self.sendCommand('_'+inttostr(waitMs));
      exit;
    end;


  function TYMultiAxisController.emergencyStop():LongInt;
    begin
      result := self.set_command('!');
      exit;
    end;


  function TYMultiAxisController.abortAndBrake():LongInt;
    begin
      result := self.set_command('B');
      exit;
    end;


  function TYMultiAxisController.abortAndHiZ():LongInt;
    begin
      result := self.set_command('z');
      exit;
    end;


  function TYMultiAxisController.nextMultiAxisController(): TYMultiAxisController;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextMultiAxisController := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextMultiAxisController := nil;
          exit;
        end;
      nextMultiAxisController := TYMultiAxisController.FindMultiAxisController(hwid);
    end;

  class function TYMultiAxisController.FirstMultiAxisController(): TYMultiAxisController;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('MultiAxisController', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYMultiAxisController.FindMultiAxisController(serial+'.'+funcId);
    end;

//--- (end of YMultiAxisController implementation)

//--- (YMultiAxisController functions)

  function yFindMultiAxisController(func:string): TYMultiAxisController;
    begin
      result := TYMultiAxisController.FindMultiAxisController(func);
    end;

  function yFirstMultiAxisController(): TYMultiAxisController;
    begin
      result := TYMultiAxisController.FirstMultiAxisController();
    end;

  procedure _MultiAxisControllerCleanup();
    begin
    end;

//--- (end of YMultiAxisController functions)

initialization
  //--- (YMultiAxisController initialization)
  //--- (end of YMultiAxisController initialization)

finalization
  //--- (YMultiAxisController cleanup)
  _MultiAxisControllerCleanup();
  //--- (end of YMultiAxisController cleanup)

end.
