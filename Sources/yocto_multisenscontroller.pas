{*********************************************************************
 *
 *  $Id: yocto_multisenscontroller.pas 33270 2018-11-22 08:41:15Z seb $
 *
 *  Implements yFindMultiSensController(), the high-level API for MultiSensController functions
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


unit yocto_multisenscontroller;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YMultiSensController definitions)

const Y_NSENSORS_INVALID              = YAPI_INVALID_UINT;
const Y_MAXSENSORS_INVALID            = YAPI_INVALID_UINT;
const Y_MAINTENANCEMODE_FALSE = 0;
const Y_MAINTENANCEMODE_TRUE = 1;
const Y_MAINTENANCEMODE_INVALID = -1;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YMultiSensController definitions)
//--- (YMultiSensController yapiwrapper declaration)
//--- (end of YMultiSensController yapiwrapper declaration)

type
  TYMultiSensController = class;
  //--- (YMultiSensController class start)
  TYMultiSensControllerValueCallback = procedure(func: TYMultiSensController; value:string);
  TYMultiSensControllerTimedReportCallback = procedure(func: TYMultiSensController; value:TYMeasure);

  ////
  /// <summary>
  ///   TYMultiSensController Class: MultiSensController function interface
  /// <para>
  ///   The Yoctopuce application programming interface allows you to drive a stepper motor.
  /// </para>
  /// </summary>
  ///-
  TYMultiSensController=class(TYFunction)
  //--- (end of YMultiSensController class start)
  protected
  //--- (YMultiSensController declaration)
    // Attributes (function value cache)
    _nSensors                 : LongInt;
    _maxSensors               : LongInt;
    _maintenanceMode          : Integer;
    _command                  : string;
    _valueCallbackMultiSensController : TYMultiSensControllerValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YMultiSensController declaration)

  public
    //--- (YMultiSensController accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the number of sensors to poll.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of sensors to poll
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_NSENSORS_INVALID</c>.
    /// </para>
    ///-
    function get_nSensors():LongInt;

    ////
    /// <summary>
    ///   Changes the number of sensors to poll.
    /// <para>
    ///   Remember to call the
    ///   <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept. Il is recommended to restart the
    ///   device with  <c>module->reboot()</c> after modifing
    ///   (and saving) this settings
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the number of sensors to poll
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
    function set_nSensors(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the maximum configurable sensor count allowed on this device.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximum configurable sensor count allowed on this device
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MAXSENSORS_INVALID</c>.
    /// </para>
    ///-
    function get_maxSensors():LongInt;

    ////
    /// <summary>
    ///   Returns true when the device is in maintenance mode.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_MAINTENANCEMODE_FALSE</c> or <c>Y_MAINTENANCEMODE_TRUE</c>, according to true when the
    ///   device is in maintenance mode
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MAINTENANCEMODE_INVALID</c>.
    /// </para>
    ///-
    function get_maintenanceMode():Integer;

    ////
    /// <summary>
    ///   Changes the device mode to enable maintenance and stop sensors polling.
    /// <para>
    ///   This way, the device will not restart automatically in case it cannot
    ///   communicate with one of the sensors.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>Y_MAINTENANCEMODE_FALSE</c> or <c>Y_MAINTENANCEMODE_TRUE</c>, according to the device
    ///   mode to enable maintenance and stop sensors polling
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
    function set_maintenanceMode(newval:Integer):integer;

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
    ///   Use the method <c>YMultiSensController.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YMultiSensController</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindMultiSensController(func: string):TYMultiSensController;

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
    function registerValueCallback(callback: TYMultiSensControllerValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Configure the I2C address of the only sensor connected to the device.
    /// <para>
    ///   It is recommanded to put the the device in maintenance mode before
    ///   changing Sensors addresses.  This method is only intended to work with a single
    ///   sensor connected to the device, if several sensors are connected, result
    ///   is unpredictible.
    ///   Note that the device is probably expecting to find a string of sensors with specific
    ///   addresses. Check the device documentation to find out which addresses should be used.
    /// </para>
    /// </summary>
    /// <param name="addr">
    ///   new address of the connected sensor
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function setupAddress(addr: LongInt):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of multi-sensor controllers started using <c>yFirstMultiSensController()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned multi-sensor controllers order.
    ///   If you want to find a specific a multi-sensor controller, use
    ///   <c>MultiSensController.findMultiSensController()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YMultiSensController</c> object, corresponding to
    ///   a multi-sensor controller currently online, or a <c>NIL</c> pointer
    ///   if there are no more multi-sensor controllers to enumerate.
    /// </returns>
    ///-
    function nextMultiSensController():TYMultiSensController;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstMultiSensController():TYMultiSensController;
  //--- (end of YMultiSensController accessors declaration)
  end;

//--- (YMultiSensController functions declaration)
  ////
  /// <summary>
  ///   Retrieves a multi-sensor controller for a given identifier.
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
  ///   This function does not require that the multi-sensor controller is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YMultiSensController.isOnline()</c> to test if the multi-sensor controller is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a multi-sensor controller by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the multi-sensor controller
  /// </param>
  /// <returns>
  ///   a <c>YMultiSensController</c> object allowing you to drive the multi-sensor controller.
  /// </returns>
  ///-
  function yFindMultiSensController(func:string):TYMultiSensController;
  ////
  /// <summary>
  ///   Starts the enumeration of multi-sensor controllers currently accessible.
  /// <para>
  ///   Use the method <c>YMultiSensController.nextMultiSensController()</c> to iterate on
  ///   next multi-sensor controllers.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YMultiSensController</c> object, corresponding to
  ///   the first multi-sensor controller currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstMultiSensController():TYMultiSensController;

//--- (end of YMultiSensController functions declaration)

implementation
//--- (YMultiSensController dlldef)
//--- (end of YMultiSensController dlldef)

  constructor TYMultiSensController.Create(func:string);
    begin
      inherited Create(func);
      _className := 'MultiSensController';
      //--- (YMultiSensController accessors initialization)
      _nSensors := Y_NSENSORS_INVALID;
      _maxSensors := Y_MAXSENSORS_INVALID;
      _maintenanceMode := Y_MAINTENANCEMODE_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackMultiSensController := nil;
      //--- (end of YMultiSensController accessors initialization)
    end;

//--- (YMultiSensController yapiwrapper)
//--- (end of YMultiSensController yapiwrapper)

//--- (YMultiSensController implementation)
{$HINTS OFF}
  function TYMultiSensController._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'nSensors') then
        begin
          _nSensors := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'maxSensors') then
        begin
          _maxSensors := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'maintenanceMode') then
        begin
          _maintenanceMode := member^.ivalue;
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

  function TYMultiSensController.get_nSensors():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NSENSORS_INVALID;
              exit;
            end;
        end;
      res := self._nSensors;
      result := res;
      exit;
    end;


  function TYMultiSensController.set_nSensors(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('nSensors',rest_val);
    end;

  function TYMultiSensController.get_maxSensors():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MAXSENSORS_INVALID;
              exit;
            end;
        end;
      res := self._maxSensors;
      result := res;
      exit;
    end;


  function TYMultiSensController.get_maintenanceMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MAINTENANCEMODE_INVALID;
              exit;
            end;
        end;
      res := self._maintenanceMode;
      result := res;
      exit;
    end;


  function TYMultiSensController.set_maintenanceMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('maintenanceMode',rest_val);
    end;

  function TYMultiSensController.get_command():string;
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


  function TYMultiSensController.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYMultiSensController.FindMultiSensController(func: string):TYMultiSensController;
    var
      obj : TYMultiSensController;
    begin
      obj := TYMultiSensController(TYFunction._FindFromCache('MultiSensController', func));
      if obj = nil then
        begin
          obj :=  TYMultiSensController.create(func);
          TYFunction._AddToCache('MultiSensController',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYMultiSensController.registerValueCallback(callback: TYMultiSensControllerValueCallback):LongInt;
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
      self._valueCallbackMultiSensController := callback;
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


  function TYMultiSensController._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackMultiSensController) <> nil) then
        begin
          self._valueCallbackMultiSensController(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYMultiSensController.setupAddress(addr: LongInt):LongInt;
    var
      cmd : string;
    begin
      cmd := 'A'+inttostr(addr);
      result := self.set_command(cmd);
      exit;
    end;


  function TYMultiSensController.nextMultiSensController(): TYMultiSensController;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextMultiSensController := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextMultiSensController := nil;
          exit;
        end;
      nextMultiSensController := TYMultiSensController.FindMultiSensController(hwid);
    end;

  class function TYMultiSensController.FirstMultiSensController(): TYMultiSensController;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('MultiSensController', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYMultiSensController.FindMultiSensController(serial+'.'+funcId);
    end;

//--- (end of YMultiSensController implementation)

//--- (YMultiSensController functions)

  function yFindMultiSensController(func:string): TYMultiSensController;
    begin
      result := TYMultiSensController.FindMultiSensController(func);
    end;

  function yFirstMultiSensController(): TYMultiSensController;
    begin
      result := TYMultiSensController.FirstMultiSensController();
    end;

  procedure _MultiSensControllerCleanup();
    begin
    end;

//--- (end of YMultiSensController functions)

initialization
  //--- (YMultiSensController initialization)
  //--- (end of YMultiSensController initialization)

finalization
  //--- (YMultiSensController cleanup)
  _MultiSensControllerCleanup();
  //--- (end of YMultiSensController cleanup)
end.
