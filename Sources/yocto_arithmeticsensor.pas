{*********************************************************************
 *
 *  $Id: yocto_arithmeticsensor.pas 35698 2019-06-05 17:25:12Z mvuilleu $
 *
 *  Implements yFindArithmeticSensor(), the high-level API for ArithmeticSensor functions
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


unit yocto_arithmeticsensor;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YArithmeticSensor definitions)

const Y_DESCRIPTION_INVALID           = YAPI_INVALID_STRING;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YArithmeticSensor definitions)
//--- (YArithmeticSensor yapiwrapper declaration)
//--- (end of YArithmeticSensor yapiwrapper declaration)

type
  TYArithmeticSensor = class;
  //--- (YArithmeticSensor class start)
  TYArithmeticSensorValueCallback = procedure(func: TYArithmeticSensor; value:string);
  TYArithmeticSensorTimedReportCallback = procedure(func: TYArithmeticSensor; value:TYMeasure);

  ////
  /// <summary>
  ///   TYArithmeticSensor Class: ArithmeticSensor function interface
  /// <para>
  ///   The YArithmeticSensor class can produce measurements computed using an arithmetic
  ///   formula based on one or more measured signals and temperature measurements.
  /// </para>
  /// </summary>
  ///-
  TYArithmeticSensor=class(TYSensor)
  //--- (end of YArithmeticSensor class start)
  protected
  //--- (YArithmeticSensor declaration)
    // Attributes (function value cache)
    _description              : string;
    _command                  : string;
    _valueCallbackArithmeticSensor : TYArithmeticSensorValueCallback;
    _timedReportCallbackArithmeticSensor : TYArithmeticSensorTimedReportCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YArithmeticSensor declaration)

  public
    //--- (YArithmeticSensor accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Changes the measuring unit for the arithmetic sensor.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the measuring unit for the arithmetic sensor
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
    ///   Returns a short informative description of the formula.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to a short informative description of the formula
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DESCRIPTION_INVALID</c>.
    /// </para>
    ///-
    function get_description():string;

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
    ///   Use the method <c>YArithmeticSensor.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YArithmeticSensor</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindArithmeticSensor(func: string):TYArithmeticSensor;

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
    function registerValueCallback(callback: TYArithmeticSensorValueCallback):LongInt; overload;

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
    function registerTimedReportCallback(callback: TYArithmeticSensorTimedReportCallback):LongInt; overload;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; override;

    ////
    /// <summary>
    ///   Defines the arithmetic function by means of an algebraic expression.
    /// <para>
    ///   The expression
    ///   may include references to device sensors, by their physical or logical name, to
    ///   usual math functions and to auxiliary functions defined separately.
    /// </para>
    /// </summary>
    /// <param name="expr">
    ///   the algebraic expression defining the function.
    /// </param>
    /// <param name="descr">
    ///   short informative description of the expression.
    /// </param>
    /// <returns>
    ///   the current expression value if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns YAPI_INVALID_DOUBLE.
    /// </para>
    ///-
    function defineExpression(expr: string; descr: string):double; overload; virtual;

    ////
    /// <summary>
    ///   Retrieves the algebraic expression defining the arithmetic function, as previously
    ///   configured using the <c>defineExpression</c> function.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string containing the mathematical expression.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function loadExpression():string; overload; virtual;

    ////
    /// <summary>
    ///   Defines a auxiliary function by means of a table of reference points.
    /// <para>
    ///   Intermediate values
    ///   will be interpolated between specified reference points. The reference points are given
    ///   as pairs of floating point numbers.
    ///   The auxiliary function will be available for use by all ArithmeticSensor objects of the
    ///   device. Up to nine auxiliary function can be defined in a device, each containing up to
    ///   96 reference points.
    /// </para>
    /// </summary>
    /// <param name="name">
    ///   auxiliary function name, up to 16 characters.
    /// </param>
    /// <param name="inputValues">
    ///   array of floating point numbers, corresponding to the function input value.
    /// </param>
    /// <param name="outputValues">
    ///   array of floating point numbers, corresponding to the output value
    ///   desired for each of the input value, index by index.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function defineAuxiliaryFunction(name: string; inputValues: TDoubleArray; outputValues: TDoubleArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Retrieves the reference points table defining an auxiliary function previously
    ///   configured using the <c>defineAuxiliaryFunction</c> function.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="name">
    ///   auxiliary function name, up to 16 characters.
    /// </param>
    /// <param name="inputValues">
    ///   array of floating point numbers, that is filled by the function
    ///   with all the function reference input value.
    /// </param>
    /// <param name="outputValues">
    ///   array of floating point numbers, that is filled by the function
    ///   output value for each of the input value, index by index.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function loadAuxiliaryFunction(name: string; var inputValues: TDoubleArray; var outputValues: TDoubleArray):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of arithmetic sensors started using <c>yFirstArithmeticSensor()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned arithmetic sensors order.
    ///   If you want to find a specific an arithmetic sensor, use <c>ArithmeticSensor.findArithmeticSensor()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YArithmeticSensor</c> object, corresponding to
    ///   an arithmetic sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more arithmetic sensors to enumerate.
    /// </returns>
    ///-
    function nextArithmeticSensor():TYArithmeticSensor;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstArithmeticSensor():TYArithmeticSensor;
  //--- (end of YArithmeticSensor accessors declaration)
  end;

//--- (YArithmeticSensor functions declaration)
  ////
  /// <summary>
  ///   Retrieves an arithmetic sensor for a given identifier.
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
  ///   This function does not require that the arithmetic sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YArithmeticSensor.isOnline()</c> to test if the arithmetic sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   an arithmetic sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the arithmetic sensor
  /// </param>
  /// <returns>
  ///   a <c>YArithmeticSensor</c> object allowing you to drive the arithmetic sensor.
  /// </returns>
  ///-
  function yFindArithmeticSensor(func:string):TYArithmeticSensor;
  ////
  /// <summary>
  ///   Starts the enumeration of arithmetic sensors currently accessible.
  /// <para>
  ///   Use the method <c>YArithmeticSensor.nextArithmeticSensor()</c> to iterate on
  ///   next arithmetic sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YArithmeticSensor</c> object, corresponding to
  ///   the first arithmetic sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstArithmeticSensor():TYArithmeticSensor;

//--- (end of YArithmeticSensor functions declaration)

implementation
//--- (YArithmeticSensor dlldef)
//--- (end of YArithmeticSensor dlldef)

  constructor TYArithmeticSensor.Create(func:string);
    begin
      inherited Create(func);
      _className := 'ArithmeticSensor';
      //--- (YArithmeticSensor accessors initialization)
      _description := Y_DESCRIPTION_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackArithmeticSensor := nil;
      _timedReportCallbackArithmeticSensor := nil;
      //--- (end of YArithmeticSensor accessors initialization)
    end;

//--- (YArithmeticSensor yapiwrapper)
//--- (end of YArithmeticSensor yapiwrapper)

//--- (YArithmeticSensor implementation)
{$HINTS OFF}
  function TYArithmeticSensor._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'description') then
        begin
          _description := string(member^.svalue);
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

  function TYArithmeticSensor.set_unit(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('unit',rest_val);
    end;

  function TYArithmeticSensor.get_description():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DESCRIPTION_INVALID;
              exit;
            end;
        end;
      res := self._description;
      result := res;
      exit;
    end;


  function TYArithmeticSensor.get_command():string;
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


  function TYArithmeticSensor.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYArithmeticSensor.FindArithmeticSensor(func: string):TYArithmeticSensor;
    var
      obj : TYArithmeticSensor;
    begin
      obj := TYArithmeticSensor(TYFunction._FindFromCache('ArithmeticSensor', func));
      if obj = nil then
        begin
          obj :=  TYArithmeticSensor.create(func);
          TYFunction._AddToCache('ArithmeticSensor',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYArithmeticSensor.registerValueCallback(callback: TYArithmeticSensorValueCallback):LongInt;
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
      self._valueCallbackArithmeticSensor := callback;
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


  function TYArithmeticSensor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackArithmeticSensor) <> nil) then
        begin
          self._valueCallbackArithmeticSensor(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYArithmeticSensor.registerTimedReportCallback(callback: TYArithmeticSensorTimedReportCallback):LongInt;
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
      self._timedReportCallbackArithmeticSensor := callback;
      result := 0;
      exit;
    end;


  function TYArithmeticSensor._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackArithmeticSensor) <> nil) then
        begin
          self._timedReportCallbackArithmeticSensor(self, value);
        end
      else
        begin
          inherited _invokeTimedReportCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYArithmeticSensor.defineExpression(expr: string; descr: string):double;
    var
      id : string;
      fname : string;
      content : string;
      data : TByteArray;
      diags : string;
      resval : double;
    begin
      id := self.get_functionId;
      id := Copy(id,  16 + 1, Length(id) - 16);
      fname := 'arithmExpr'+id+'.txt';

      content := '// '+ descr+''#10''+expr;
      data := self._uploadEx(fname, _StrToByte(content));
      diags := _ByteToString(data);
      if not((Copy(diags, 0 + 1, 8) = 'Result: ')) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, diags);
          result:=YAPI_INVALID_DOUBLE;
          exit;
        end;
      resval := StrToFloat(Copy(diags,  8 + 1, Length(diags)-8));
      result := resval;
      exit;
    end;


  function TYArithmeticSensor.loadExpression():string;
    var
      id : string;
      fname : string;
      content : string;
      idx : LongInt;
    begin
      id := self.get_functionId;
      id := Copy(id,  16 + 1, Length(id) - 16);
      fname := 'arithmExpr'+id+'.txt';

      content := _ByteToString(self._download(fname));
      idx := (pos(''#10'', content) - 1);
      if idx > 0 then
        begin
          content := Copy(content,  idx+1 + 1, Length(content)-(idx+1));
        end;
      result := content;
      exit;
    end;


  function TYArithmeticSensor.defineAuxiliaryFunction(name: string; inputValues: TDoubleArray; outputValues: TDoubleArray):LongInt;
    var
      siz : LongInt;
      defstr : string;
      idx : LongInt;
      inputVal : double;
      outputVal : double;
      fname : string;
    begin
      siz := length(inputValues);
      if not(siz > 1) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'auxiliary function must be defined by at least two points');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if not(siz = length(outputValues)) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'table sizes mismatch');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      defstr := '';
      idx := 0;
      while idx < siz do
        begin
          inputVal := inputValues[idx];
          outputVal := outputValues[idx];
          defstr := ''+ defstr+''+_yapiFloatToStr( inputVal)+':'+_yapiFloatToStr(outputVal)+''#10'';
          idx := idx + 1;
        end;
      fname := 'userMap'+name+'.txt';

      result := self._upload(fname, _StrToByte(defstr));
      exit;
    end;


  function TYArithmeticSensor.loadAuxiliaryFunction(name: string; var inputValues: TDoubleArray; var outputValues: TDoubleArray):LongInt;
    var
      fname : string;
      defbin : TByteArray;
      siz : LongInt;
      inputValues_pos : LongInt;
      outputValues_pos : LongInt;
    begin
      fname := 'userMap'+name+'.txt';
      defbin := self._download(fname);
      siz := length(defbin);
      if not(siz > 0) then
        begin
          self._throw( YAPI_INVALID_ARGUMENT, 'auxiliary function does not exist');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      inputValues_pos := 0;
      SetLength(inputValues, siz);;
      outputValues_pos := 0;
      SetLength(outputValues, siz);;
      // FIXME: decode line by line
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYArithmeticSensor.nextArithmeticSensor(): TYArithmeticSensor;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextArithmeticSensor := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextArithmeticSensor := nil;
          exit;
        end;
      nextArithmeticSensor := TYArithmeticSensor.FindArithmeticSensor(hwid);
    end;

  class function TYArithmeticSensor.FirstArithmeticSensor(): TYArithmeticSensor;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('ArithmeticSensor', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYArithmeticSensor.FindArithmeticSensor(serial+'.'+funcId);
    end;

//--- (end of YArithmeticSensor implementation)

//--- (YArithmeticSensor functions)

  function yFindArithmeticSensor(func:string): TYArithmeticSensor;
    begin
      result := TYArithmeticSensor.FindArithmeticSensor(func);
    end;

  function yFirstArithmeticSensor(): TYArithmeticSensor;
    begin
      result := TYArithmeticSensor.FirstArithmeticSensor();
    end;

  procedure _ArithmeticSensorCleanup();
    begin
    end;

//--- (end of YArithmeticSensor functions)

initialization
  //--- (YArithmeticSensor initialization)
  //--- (end of YArithmeticSensor initialization)

finalization
  //--- (YArithmeticSensor cleanup)
  _ArithmeticSensorCleanup();
  //--- (end of YArithmeticSensor cleanup)
end.
