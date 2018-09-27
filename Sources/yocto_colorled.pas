{*********************************************************************
 *
 * $Id: yocto_colorled.pas 32348 2018-09-25 13:28:40Z seb $
 *
 * Implements yFindColorLed(), the high-level API for ColorLed functions
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


unit yocto_colorled;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YColorLed definitions)
type  TYColorLedMove = class(TObject)
  public
      target      : LongInt;
      ms          : LongInt;
      moving      : LongInt;
      constructor Create();
    end;

const Y_RGBCOLOR_INVALID              = YAPI_INVALID_UINT;
const Y_HSLCOLOR_INVALID              = YAPI_INVALID_UINT;
const Y_RGBCOLORATPOWERON_INVALID     = YAPI_INVALID_UINT;
const Y_BLINKSEQSIZE_INVALID          = YAPI_INVALID_UINT;
const Y_BLINKSEQMAXSIZE_INVALID       = YAPI_INVALID_UINT;
const Y_BLINKSEQSIGNATURE_INVALID     = YAPI_INVALID_UINT;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;

var Y_RGBMOVE_INVALID : TYColorLedMove;
var Y_HSLMOVE_INVALID : TYColorLedMove;

//--- (end of YColorLed definitions)
//--- (YColorLed yapiwrapper declaration)
//--- (end of YColorLed yapiwrapper declaration)

type
  TYColorLed = class;
  //--- (YColorLed class start)
  TYColorLedValueCallback = procedure(func: TYColorLed; value:string);
  TYColorLedTimedReportCallback = procedure(func: TYColorLed; value:TYMeasure);

  ////
  /// <summary>
  ///   TYColorLed Class: ColorLed function interface
  /// <para>
  ///   The Yoctopuce application programming interface
  ///   allows you to drive a color LED using RGB coordinates as well as HSL coordinates.
  ///   The module performs all conversions form RGB to HSL automatically. It is then
  ///   self-evident to turn on a LED with a given hue and to progressively vary its
  ///   saturation or lightness. If needed, you can find more information on the
  ///   difference between RGB and HSL in the section following this one.
  /// </para>
  /// </summary>
  ///-
  TYColorLed=class(TYFunction)
  //--- (end of YColorLed class start)
  protected
  //--- (YColorLed declaration)
    // Attributes (function value cache)
    _rgbColor                 : LongInt;
    _hslColor                 : LongInt;
    _rgbMove                  : TYColorLedMove;
    _hslMove                  : TYColorLedMove;
    _rgbColorAtPowerOn        : LongInt;
    _blinkSeqSize             : LongInt;
    _blinkSeqMaxSize          : LongInt;
    _blinkSeqSignature        : LongInt;
    _command                  : string;
    _valueCallbackColorLed    : TYColorLedValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YColorLed declaration)

  public
    //--- (YColorLed accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the current RGB color of the LED.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current RGB color of the LED
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_RGBCOLOR_INVALID</c>.
    /// </para>
    ///-
    function get_rgbColor():LongInt;

    ////
    /// <summary>
    ///   Changes the current color of the LED, using an RGB color.
    /// <para>
    ///   Encoding is done as follows: 0xRRGGBB.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the current color of the LED, using an RGB color
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
    function set_rgbColor(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the current HSL color of the LED.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current HSL color of the LED
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_HSLCOLOR_INVALID</c>.
    /// </para>
    ///-
    function get_hslColor():LongInt;

    ////
    /// <summary>
    ///   Changes the current color of the LED, using a color HSL.
    /// <para>
    ///   Encoding is done as follows: 0xHHSSLL.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the current color of the LED, using a color HSL
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
    function set_hslColor(newval:LongInt):integer;

    function get_rgbMove():TYColorLedMove;

    function set_rgbMove(newval:TYColorLedMove):integer;

    ////
    /// <summary>
    ///   Performs a smooth transition in the RGB color space between the current color and a target color.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="rgb_target">
    ///   desired RGB color at the end of the transition
    /// </param>
    /// <param name="ms_duration">
    ///   duration of the transition, in millisecond
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
    function rgbMove(rgb_target: LongInt; ms_duration: LongInt):integer;

    function get_hslMove():TYColorLedMove;

    function set_hslMove(newval:TYColorLedMove):integer;

    ////
    /// <summary>
    ///   Performs a smooth transition in the HSL color space between the current color and a target color.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="hsl_target">
    ///   desired HSL color at the end of the transition
    /// </param>
    /// <param name="ms_duration">
    ///   duration of the transition, in millisecond
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
    function hslMove(hsl_target: LongInt; ms_duration: LongInt):integer;

    ////
    /// <summary>
    ///   Returns the configured color to be displayed when the module is turned on.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the configured color to be displayed when the module is turned on
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_RGBCOLORATPOWERON_INVALID</c>.
    /// </para>
    ///-
    function get_rgbColorAtPowerOn():LongInt;

    ////
    /// <summary>
    ///   Changes the color that the LED will display by default when the module is turned on.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the color that the LED will display by default when the module is turned on
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
    function set_rgbColorAtPowerOn(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the current length of the blinking sequence.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current length of the blinking sequence
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BLINKSEQSIZE_INVALID</c>.
    /// </para>
    ///-
    function get_blinkSeqSize():LongInt;

    ////
    /// <summary>
    ///   Returns the maximum length of the blinking sequence.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximum length of the blinking sequence
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BLINKSEQMAXSIZE_INVALID</c>.
    /// </para>
    ///-
    function get_blinkSeqMaxSize():LongInt;

    ////
    /// <summary>
    ///   Return the blinking sequence signature.
    /// <para>
    ///   Since blinking
    ///   sequences cannot be read from the device, this can be used
    ///   to detect if a specific blinking sequence is already
    ///   programmed.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BLINKSEQSIGNATURE_INVALID</c>.
    /// </para>
    ///-
    function get_blinkSeqSignature():LongInt;

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
    ///   Use the method <c>YColorLed.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YColorLed</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindColorLed(func: string):TYColorLed;

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
    function registerValueCallback(callback: TYColorLedValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function sendCommand(command: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Add a new transition to the blinking sequence, the move will
    ///   be performed in the HSL space.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="HSLcolor">
    ///   desired HSL color when the traisntion is completed
    /// </param>
    /// <param name="msDelay">
    ///   duration of the color transition, in milliseconds.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function addHslMoveToBlinkSeq(HSLcolor: LongInt; msDelay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Adds a new transition to the blinking sequence, the move is
    ///   performed in the RGB space.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="RGBcolor">
    ///   desired RGB color when the transition is completed
    /// </param>
    /// <param name="msDelay">
    ///   duration of the color transition, in milliseconds.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function addRgbMoveToBlinkSeq(RGBcolor: LongInt; msDelay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts the preprogrammed blinking sequence.
    /// <para>
    ///   The sequence is
    ///   run in a loop until it is stopped by stopBlinkSeq or an explicit
    ///   change.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function startBlinkSeq():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops the preprogrammed blinking sequence.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function stopBlinkSeq():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Resets the preprogrammed blinking sequence.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    ///   On failure, throws an exception or returns a negative error code.
    /// </returns>
    ///-
    function resetBlinkSeq():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of RGB LEDs started using <c>yFirstColorLed()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YColorLed</c> object, corresponding to
    ///   an RGB LED currently online, or a <c>NIL</c> pointer
    ///   if there are no more RGB LEDs to enumerate.
    /// </returns>
    ///-
    function nextColorLed():TYColorLed;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstColorLed():TYColorLed;
  //--- (end of YColorLed accessors declaration)
  end;

//--- (YColorLed functions declaration)
  ////
  /// <summary>
  ///   Retrieves an RGB LED for a given identifier.
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
  ///   This function does not require that the RGB LED is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YColorLed.isOnline()</c> to test if the RGB LED is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   an RGB LED by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the RGB LED
  /// </param>
  /// <returns>
  ///   a <c>YColorLed</c> object allowing you to drive the RGB LED.
  /// </returns>
  ///-
  function yFindColorLed(func:string):TYColorLed;
  ////
  /// <summary>
  ///   Starts the enumeration of RGB LEDs currently accessible.
  /// <para>
  ///   Use the method <c>YColorLed.nextColorLed()</c> to iterate on
  ///   next RGB LEDs.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YColorLed</c> object, corresponding to
  ///   the first RGB LED currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstColorLed():TYColorLed;

//--- (end of YColorLed functions declaration)

implementation
//--- (YColorLed dlldef)
//--- (end of YColorLed dlldef)

    constructor TYColorLedMove.Create();
    begin
      target := YAPI_INVALID_INT;
      ms := YAPI_INVALID_INT;
      moving := YAPI_INVALID_UINT;
  end;

  constructor TYColorLed.Create(func:string);
    begin
      inherited Create(func);
      _className := 'ColorLed';
      //--- (YColorLed accessors initialization)
      _rgbColor := Y_RGBCOLOR_INVALID;
      _hslColor := Y_HSLCOLOR_INVALID;
      _rgbMove := Y_RGBMOVE_INVALID;
      _hslMove := Y_HSLMOVE_INVALID;
      _rgbColorAtPowerOn := Y_RGBCOLORATPOWERON_INVALID;
      _blinkSeqSize := Y_BLINKSEQSIZE_INVALID;
      _blinkSeqMaxSize := Y_BLINKSEQMAXSIZE_INVALID;
      _blinkSeqSignature := Y_BLINKSEQSIGNATURE_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackColorLed := nil;
      //--- (end of YColorLed accessors initialization)
    end;

//--- (YColorLed yapiwrapper)
//--- (end of YColorLed yapiwrapper)

//--- (YColorLed implementation)
{$HINTS OFF}
  function TYColorLed._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'rgbColor') then
        begin
          _rgbColor := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'hslColor') then
        begin
          _hslColor := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'rgbMove') then
        begin
          if member^.recordtype = JSON_STRUCT then
            begin
              for l:=0 to member^.membercount-1 do
               begin
                 sub := member^.members[l];
                 if (sub^.name = 'moving') then
                    _rgbMove.moving := sub^.ivalue else
                 if (sub^.name = 'target') then
                    _rgbMove.target := sub^.ivalue else
                 if (sub^.name = 'ms') then
                    _rgbMove.ms := sub^.ivalue;
               end;
            end;
         result := 1;
         exit;
         end;
      if (member^.name = 'hslMove') then
        begin
          if member^.recordtype = JSON_STRUCT then
            begin
              for l:=0 to member^.membercount-1 do
               begin
                 sub := member^.members[l];
                 if (sub^.name = 'moving') then
                    _hslMove.moving := sub^.ivalue else
                 if (sub^.name = 'target') then
                    _hslMove.target := sub^.ivalue else
                 if (sub^.name = 'ms') then
                    _hslMove.ms := sub^.ivalue;
               end;
            end;
         result := 1;
         exit;
         end;
      if (member^.name = 'rgbColorAtPowerOn') then
        begin
          _rgbColorAtPowerOn := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'blinkSeqSize') then
        begin
          _blinkSeqSize := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'blinkSeqMaxSize') then
        begin
          _blinkSeqMaxSize := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'blinkSeqSignature') then
        begin
          _blinkSeqSignature := integer(member^.ivalue);
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

  function TYColorLed.get_rgbColor():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RGBCOLOR_INVALID;
              exit;
            end;
        end;
      res := self._rgbColor;
      result := res;
      exit;
    end;


  function TYColorLed.set_rgbColor(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := '0x'+inttohex(newval,6);
      result := _setAttr('rgbColor',rest_val);
    end;

  function TYColorLed.get_hslColor():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_HSLCOLOR_INVALID;
              exit;
            end;
        end;
      res := self._hslColor;
      result := res;
      exit;
    end;


  function TYColorLed.set_hslColor(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := '0x'+inttohex(newval,6);
      result := _setAttr('hslColor',rest_val);
    end;

  function TYColorLed.get_rgbMove():TYColorLedMove;
    var
      res : TYColorLedMove;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RGBMOVE_INVALID;
              exit;
            end;
        end;
      res := self._rgbMove;
      result := res;
      exit;
    end;


  function TYColorLed.set_rgbMove(newval:TYColorLedMove):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval.target)+':'+inttostr(newval.ms);
      result := _setAttr('rgbMove',rest_val);
    end;

  ////
  /// <summary>
  ///   Performs a smooth transition in the RGB color space between the current color and a target color.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="rgb_target">
  ///   desired RGB color at the end of the transition
  /// </param>
  /// <param name="ms_duration">
  ///   duration of the transition, in millisecond
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
  function TYColorLed.rgbMove(rgb_target: LongInt; ms_duration: LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(rgb_target)+':'+inttostr(ms_duration);
      result := _setAttr('rgbMove', rest_val);
    end;

  function TYColorLed.get_hslMove():TYColorLedMove;
    var
      res : TYColorLedMove;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_HSLMOVE_INVALID;
              exit;
            end;
        end;
      res := self._hslMove;
      result := res;
      exit;
    end;


  function TYColorLed.set_hslMove(newval:TYColorLedMove):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval.target)+':'+inttostr(newval.ms);
      result := _setAttr('hslMove',rest_val);
    end;

  ////
  /// <summary>
  ///   Performs a smooth transition in the HSL color space between the current color and a target color.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="hsl_target">
  ///   desired HSL color at the end of the transition
  /// </param>
  /// <param name="ms_duration">
  ///   duration of the transition, in millisecond
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
  function TYColorLed.hslMove(hsl_target: LongInt; ms_duration: LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(hsl_target)+':'+inttostr(ms_duration);
      result := _setAttr('hslMove', rest_val);
    end;

  function TYColorLed.get_rgbColorAtPowerOn():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RGBCOLORATPOWERON_INVALID;
              exit;
            end;
        end;
      res := self._rgbColorAtPowerOn;
      result := res;
      exit;
    end;


  function TYColorLed.set_rgbColorAtPowerOn(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := '0x'+inttohex(newval,6);
      result := _setAttr('rgbColorAtPowerOn',rest_val);
    end;

  function TYColorLed.get_blinkSeqSize():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BLINKSEQSIZE_INVALID;
              exit;
            end;
        end;
      res := self._blinkSeqSize;
      result := res;
      exit;
    end;


  function TYColorLed.get_blinkSeqMaxSize():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BLINKSEQMAXSIZE_INVALID;
              exit;
            end;
        end;
      res := self._blinkSeqMaxSize;
      result := res;
      exit;
    end;


  function TYColorLed.get_blinkSeqSignature():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BLINKSEQSIGNATURE_INVALID;
              exit;
            end;
        end;
      res := self._blinkSeqSignature;
      result := res;
      exit;
    end;


  function TYColorLed.get_command():string;
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


  function TYColorLed.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYColorLed.FindColorLed(func: string):TYColorLed;
    var
      obj : TYColorLed;
    begin
      obj := TYColorLed(TYFunction._FindFromCache('ColorLed', func));
      if obj = nil then
        begin
          obj :=  TYColorLed.create(func);
          TYFunction._AddToCache('ColorLed',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYColorLed.registerValueCallback(callback: TYColorLedValueCallback):LongInt;
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
      self._valueCallbackColorLed := callback;
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


  function TYColorLed._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackColorLed) <> nil) then
        begin
          self._valueCallbackColorLed(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYColorLed.sendCommand(command: string):LongInt;
    begin
      result := self.set_command(command);
      exit;
    end;


  function TYColorLed.addHslMoveToBlinkSeq(HSLcolor: LongInt; msDelay: LongInt):LongInt;
    begin
      result := self.sendCommand('H'+inttostr(HSLcolor)+','+inttostr(msDelay));
      exit;
    end;


  function TYColorLed.addRgbMoveToBlinkSeq(RGBcolor: LongInt; msDelay: LongInt):LongInt;
    begin
      result := self.sendCommand('R'+inttostr(RGBcolor)+','+inttostr(msDelay));
      exit;
    end;


  function TYColorLed.startBlinkSeq():LongInt;
    begin
      result := self.sendCommand('S');
      exit;
    end;


  function TYColorLed.stopBlinkSeq():LongInt;
    begin
      result := self.sendCommand('X');
      exit;
    end;


  function TYColorLed.resetBlinkSeq():LongInt;
    begin
      result := self.sendCommand('Z');
      exit;
    end;


  function TYColorLed.nextColorLed(): TYColorLed;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextColorLed := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextColorLed := nil;
          exit;
        end;
      nextColorLed := TYColorLed.FindColorLed(hwid);
    end;

  class function TYColorLed.FirstColorLed(): TYColorLed;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('ColorLed', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYColorLed.FindColorLed(serial+'.'+funcId);
    end;

//--- (end of YColorLed implementation)

//--- (YColorLed functions)

  function yFindColorLed(func:string): TYColorLed;
    begin
      result := TYColorLed.FindColorLed(func);
    end;

  function yFirstColorLed(): TYColorLed;
    begin
      result := TYColorLed.FirstColorLed();
    end;

  procedure _ColorLedCleanup();
    begin
    end;

//--- (end of YColorLed functions)

initialization
  //--- (YColorLed initialization)
    Y_RGBMOVE_INVALID := TYColorLedMove.Create();
    Y_HSLMOVE_INVALID := TYColorLedMove.Create();
  //--- (end of YColorLed initialization)

finalization
  //--- (YColorLed cleanup)
  _ColorLedCleanup();
  //--- (end of YColorLed cleanup)
end.
