{*********************************************************************
 *
 * $Id: yocto_display.pas 19338 2015-02-17 17:44:58Z seb $
 *
 * Implements yFindDisplay(), the high-level API for Display functions
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
 *  THE SOFTWARE AND DOCUMENTATION ARE PROVIDED "AS IS" WITHOUT
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


unit yocto_display;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (generated code: YDisplay definitions)

const Y_ENABLED_FALSE = 0;
const Y_ENABLED_TRUE = 1;
const Y_ENABLED_INVALID = -1;
const Y_STARTUPSEQ_INVALID            = YAPI_INVALID_STRING;
const Y_BRIGHTNESS_INVALID            = YAPI_INVALID_UINT;
const Y_ORIENTATION_LEFT = 0;
const Y_ORIENTATION_UP = 1;
const Y_ORIENTATION_RIGHT = 2;
const Y_ORIENTATION_DOWN = 3;
const Y_ORIENTATION_INVALID = -1;
const Y_DISPLAYWIDTH_INVALID          = YAPI_INVALID_UINT;
const Y_DISPLAYHEIGHT_INVALID         = YAPI_INVALID_UINT;
const Y_DISPLAYTYPE_MONO = 0;
const Y_DISPLAYTYPE_GRAY = 1;
const Y_DISPLAYTYPE_RGB = 2;
const Y_DISPLAYTYPE_INVALID = -1;
const Y_LAYERWIDTH_INVALID            = YAPI_INVALID_UINT;
const Y_LAYERHEIGHT_INVALID           = YAPI_INVALID_UINT;
const Y_LAYERCOUNT_INVALID            = YAPI_INVALID_UINT;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of generated code: YDisplay definitions)

//--- (generated code: YDisplayLayer definitions)
type  TYALIGN = (Y_ALIGN_TOP_LEFT,Y_ALIGN_CENTER_LEFT,Y_ALIGN_BASELINE_LEFT,Y_ALIGN_BOTTOM_LEFT,Y_ALIGN_TOP_CENTER,Y_ALIGN_CENTER,Y_ALIGN_BASELINE_CENTER,Y_ALIGN_BOTTOM_CENTER,Y_ALIGN_TOP_DECIMAL,Y_ALIGN_CENTER_DECIMAL,Y_ALIGN_BASELINE_DECIMAL,Y_ALIGN_BOTTOM_DECIMAL,Y_ALIGN_TOP_RIGHT,Y_ALIGN_CENTER_RIGHT,Y_ALIGN_BASELINE_RIGHT,Y_ALIGN_BOTTOM_RIGHT);


//--- (end of generated code: YDisplayLayer definitions)

type
 TYDisplayLayer = class;
 TYDisplay = class;

  //--- (generated code: YDisplay class start)
  TYDisplayValueCallback = procedure(func: TYDisplay; value:string);
  TYDisplayTimedReportCallback = procedure(func: TYDisplay; value:TYMeasure);

  ////
  /// <summary>
  ///   TYDisplay Class: Display function interface
  /// <para>
  ///   Yoctopuce display interface has been designed to easily
  ///   show information and images. The device provides built-in
  ///   multi-layer rendering. Layers can be drawn offline, individually,
  ///   and freely moved on the display. It can also replay recorded
  ///   sequences (animations).
  /// </para>
  /// </summary>
  ///-
  TYDisplay=class(TYFunction)
  //--- (end of generated code: YDisplay class start)
  protected
  //--- (generated code: YDisplay declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _enabled                  : Integer;
    _startupSeq               : string;
    _brightness               : LongInt;
    _orientation              : Integer;
    _displayWidth             : LongInt;
    _displayHeight            : LongInt;
    _displayType              : Integer;
    _layerWidth               : LongInt;
    _layerHeight              : LongInt;
    _layerCount               : LongInt;
    _command                  : string;
    _valueCallbackDisplay     : TYDisplayValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YDisplay declaration)

private
   _allDisplayLayers :  array of TYDisplayLayer;
   _recording : boolean;
   _sequence : string;

public
   destructor Destroy(); override;
   function sendCommand(cmd:string):integer;
   function  flushLayers() :integer;
   procedure resetHiddenLayerFlags();

   //--- (generated code: YDisplay accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns true if the screen is powered, false otherwise.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_ENABLED_FALSE</c> or <c>Y_ENABLED_TRUE</c>, according to true if the screen is powered,
    ///   false otherwise
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ENABLED_INVALID</c>.
    /// </para>
    ///-
    function get_enabled():Integer;

    ////
    /// <summary>
    ///   Changes the power state of the display.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>Y_ENABLED_FALSE</c> or <c>Y_ENABLED_TRUE</c>, according to the power state of the display
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
    function set_enabled(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the name of the sequence to play when the displayed is powered on.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the name of the sequence to play when the displayed is powered on
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_STARTUPSEQ_INVALID</c>.
    /// </para>
    ///-
    function get_startupSeq():string;

    ////
    /// <summary>
    ///   Changes the name of the sequence to play when the displayed is powered on.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the name of the sequence to play when the displayed is powered on
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
    function set_startupSeq(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the luminosity of the  module informative leds (from 0 to 100).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the luminosity of the  module informative leds (from 0 to 100)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BRIGHTNESS_INVALID</c>.
    /// </para>
    ///-
    function get_brightness():LongInt;

    ////
    /// <summary>
    ///   Changes the brightness of the display.
    /// <para>
    ///   The parameter is a value between 0 and
    ///   100. Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the brightness of the display
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
    function set_brightness(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the currently selected display orientation.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_ORIENTATION_LEFT</c>, <c>Y_ORIENTATION_UP</c>, <c>Y_ORIENTATION_RIGHT</c> and
    ///   <c>Y_ORIENTATION_DOWN</c> corresponding to the currently selected display orientation
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ORIENTATION_INVALID</c>.
    /// </para>
    ///-
    function get_orientation():Integer;

    ////
    /// <summary>
    ///   Changes the display orientation.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_ORIENTATION_LEFT</c>, <c>Y_ORIENTATION_UP</c>, <c>Y_ORIENTATION_RIGHT</c> and
    ///   <c>Y_ORIENTATION_DOWN</c> corresponding to the display orientation
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
    function set_orientation(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the display width, in pixels.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the display width, in pixels
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DISPLAYWIDTH_INVALID</c>.
    /// </para>
    ///-
    function get_displayWidth():LongInt;

    ////
    /// <summary>
    ///   Returns the display height, in pixels.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the display height, in pixels
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DISPLAYHEIGHT_INVALID</c>.
    /// </para>
    ///-
    function get_displayHeight():LongInt;

    ////
    /// <summary>
    ///   Returns the display type: monochrome, gray levels or full color.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_DISPLAYTYPE_MONO</c>, <c>Y_DISPLAYTYPE_GRAY</c> and <c>Y_DISPLAYTYPE_RGB</c>
    ///   corresponding to the display type: monochrome, gray levels or full color
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DISPLAYTYPE_INVALID</c>.
    /// </para>
    ///-
    function get_displayType():Integer;

    ////
    /// <summary>
    ///   Returns the width of the layers to draw on, in pixels.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the width of the layers to draw on, in pixels
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LAYERWIDTH_INVALID</c>.
    /// </para>
    ///-
    function get_layerWidth():LongInt;

    ////
    /// <summary>
    ///   Returns the height of the layers to draw on, in pixels.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the height of the layers to draw on, in pixels
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LAYERHEIGHT_INVALID</c>.
    /// </para>
    ///-
    function get_layerHeight():LongInt;

    ////
    /// <summary>
    ///   Returns the number of available layers to draw on.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of available layers to draw on
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LAYERCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_layerCount():LongInt;

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
    ///   Use the method <c>YDisplay.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YDisplay</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindDisplay(func: string):TYDisplay;

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
    function registerValueCallback(callback: TYDisplayValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Clears the display screen and resets all display layers to their default state.
    /// <para>
    ///   Using this function in a sequence will kill the sequence play-back. Don't use that
    ///   function to reset the display at sequence start-up.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function resetAll():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Smoothly changes the brightness of the screen to produce a fade-in or fade-out
    ///   effect.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="brightness">
    ///   the new screen brightness
    /// </param>
    /// <param name="duration">
    ///   duration of the brightness transition, in milliseconds.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function fade(brightness: LongInt; duration: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts to record all display commands into a sequence, for later replay.
    /// <para>
    ///   The name used to store the sequence is specified when calling
    ///   <c>saveSequence()</c>, once the recording is complete.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function newSequence():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops recording display commands and saves the sequence into the specified
    ///   file on the display internal memory.
    /// <para>
    ///   The sequence can be later replayed
    ///   using <c>playSequence()</c>.
    /// </para>
    /// </summary>
    /// <param name="sequenceName">
    ///   the name of the newly created sequence
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function saveSequence(sequenceName: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Replays a display sequence previously recorded using
    ///   <c>newSequence()</c> and <c>saveSequence()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="sequenceName">
    ///   the name of the newly created sequence
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function playSequence(sequenceName: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Waits for a specified delay (in milliseconds) before playing next
    ///   commands in current sequence.
    /// <para>
    ///   This method can be used while
    ///   recording a display sequence, to insert a timed wait in the sequence
    ///   (without any immediate effect). It can also be used dynamically while
    ///   playing a pre-recorded sequence, to suspend or resume the execution of
    ///   the sequence. To cancel a delay, call the same method with a zero delay.
    /// </para>
    /// </summary>
    /// <param name="delay_ms">
    ///   the duration to wait, in milliseconds
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function pauseSequence(delay_ms: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops immediately any ongoing sequence replay.
    /// <para>
    ///   The display is left as is.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function stopSequence():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Uploads an arbitrary file (for instance a GIF file) to the display, to the
    ///   specified full path name.
    /// <para>
    ///   If a file already exists with the same path name,
    ///   its content is overwritten.
    /// </para>
    /// </summary>
    /// <param name="pathname">
    ///   path and name of the new file to create
    /// </param>
    /// <param name="content">
    ///   binary buffer with the content to set
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function upload(pathname: string; content: TByteArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Copies the whole content of a layer to another layer.
    /// <para>
    ///   The color and transparency
    ///   of all the pixels from the destination layer are set to match the source pixels.
    ///   This method only affects the displayed content, but does not change any
    ///   property of the layer object.
    ///   Note that layer 0 has no transparency support (it is always completely opaque).
    /// </para>
    /// </summary>
    /// <param name="srcLayerId">
    ///   the identifier of the source layer (a number in range 0..layerCount-1)
    /// </param>
    /// <param name="dstLayerId">
    ///   the identifier of the destination layer (a number in range 0..layerCount-1)
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function copyLayerContent(srcLayerId: LongInt; dstLayerId: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Swaps the whole content of two layers.
    /// <para>
    ///   The color and transparency of all the pixels from
    ///   the two layers are swapped. This method only affects the displayed content, but does
    ///   not change any property of the layer objects. In particular, the visibility of each
    ///   layer stays unchanged. When used between onae hidden layer and a visible layer,
    ///   this method makes it possible to easily implement double-buffering.
    ///   Note that layer 0 has no transparency support (it is always completely opaque).
    /// </para>
    /// </summary>
    /// <param name="layerIdA">
    ///   the first layer (a number in range 0..layerCount-1)
    /// </param>
    /// <param name="layerIdB">
    ///   the second layer (a number in range 0..layerCount-1)
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function swapLayerContent(layerIdA: LongInt; layerIdB: LongInt):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of displays started using <c>yFirstDisplay()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YDisplay</c> object, corresponding to
    ///   a display currently online, or a <c>null</c> pointer
    ///   if there are no more displays to enumerate.
    /// </returns>
    ///-
    function nextDisplay():TYDisplay;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstDisplay():TYDisplay;
  //--- (end of generated code: YDisplay accessors declaration)

    ////
    /// <summary>
    ///   Returns a YDisplayLayer object that can be used to draw on the specified
    ///   layer.
    /// <para>
    ///   The content is displayed only when the layer is active on the
    ///   screen (and not masked by other overlapping layers).
    /// </para>
    /// </summary>
    /// <param name="layerId">
    ///   the identifier of the layer (a number in range 0..layerCount-1)
    /// </param>
    /// <returns>
    ///   an <c>YDisplayLayer</c> object
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>null</c>.
    /// </para>
    ///-
    function get_displayLayer(layerId:integer):TYDisplayLayer;

end;

 // from YDisplayLayer


//--- (generated code: YDisplayLayer class start)
  ////
  /// <summary>
  ///   TYDisplayLayer Class: DisplayLayer object interface
  /// <para>
  ///   A DisplayLayer is an image layer containing objects to display
  ///   (bitmaps, text, etc.). The content is displayed only when
  ///   the layer is active on the screen (and not masked by other
  ///   overlapping layers).
  /// </para>
  /// </summary>
  ///-
  TYDisplayLayer=class(TObject)
  //--- (end of generated code: YDisplayLayer class start)


  //--- (generated code: YDisplayLayer declaration)
    // Attributes (function value cache)

    //--- (end of generated code: YDisplayLayer declaration)
private
   _display : TYdisplay;
   _cmdbuff : string;
   _id:integer;
   _hidden:boolean;

public
   constructor Create(parent: TYdisplay; id :string);
 
   function command_push(cmd:string):integer;
   function command_flush(cmd:string):integer;
   function flush_now():integer;


   //--- (generated code: YDisplayLayer accessors declaration)
    ////
    /// <summary>
    ///   Reverts the layer to its initial state (fully transparent, default settings).
    /// <para>
    ///   Reinitializes the drawing pointer to the upper left position,
    ///   and selects the most visible pen color. If you only want to erase the layer
    ///   content, use the method <c>clear()</c> instead.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function reset():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Erases the whole content of the layer (makes it fully transparent).
    /// <para>
    ///   This method does not change any other attribute of the layer.
    ///   To reinitialize the layer attributes to defaults settings, use the method
    ///   <c>reset()</c> instead.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function clear():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Selects the pen color for all subsequent drawing functions,
    ///   including text drawing.
    /// <para>
    ///   The pen color is provided as an RGB value.
    ///   For grayscale or monochrome displays, the value is
    ///   automatically converted to the proper range.
    /// </para>
    /// </summary>
    /// <param name="color">
    ///   the desired pen color, as a 24-bit RGB value
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function selectColorPen(color: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Selects the pen gray level for all subsequent drawing functions,
    ///   including text drawing.
    /// <para>
    ///   The gray level is provided as a number between
    ///   0 (black) and 255 (white, or whichever the lighest color is).
    ///   For monochrome displays (without gray levels), any value
    ///   lower than 128 is rendered as black, and any value equal
    ///   or above to 128 is non-black.
    /// </para>
    /// </summary>
    /// <param name="graylevel">
    ///   the desired gray level, from 0 to 255
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function selectGrayPen(graylevel: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Selects an eraser instead of a pen for all subsequent drawing functions,
    ///   except for bitmap copy functions.
    /// <para>
    ///   Any point drawn using the eraser
    ///   becomes transparent (as when the layer is empty), showing the other
    ///   layers beneath it.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function selectEraser():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Enables or disables anti-aliasing for drawing oblique lines and circles.
    /// <para>
    ///   Anti-aliasing provides a smoother aspect when looked from far enough,
    ///   but it can add fuzzyness when the display is looked from very close.
    ///   At the end of the day, it is your personal choice.
    ///   Anti-aliasing is enabled by default on grayscale and color displays,
    ///   but you can disable it if you prefer. This setting has no effect
    ///   on monochrome displays.
    /// </para>
    /// </summary>
    /// <param name="mode">
    ///   <t>true</t> to enable antialiasing, <t>false</t> to
    ///   disable it.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function setAntialiasingMode(mode: boolean):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Draws a single pixel at the specified position.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="x">
    ///   the distance from left of layer, in pixels
    /// </param>
    /// <param name="y">
    ///   the distance from top of layer, in pixels
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function drawPixel(x: LongInt; y: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Draws an empty rectangle at a specified position.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="x1">
    ///   the distance from left of layer to the left border of the rectangle, in pixels
    /// </param>
    /// <param name="y1">
    ///   the distance from top of layer to the top border of the rectangle, in pixels
    /// </param>
    /// <param name="x2">
    ///   the distance from left of layer to the right border of the rectangle, in pixels
    /// </param>
    /// <param name="y2">
    ///   the distance from top of layer to the bottom border of the rectangle, in pixels
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function drawRect(x1: LongInt; y1: LongInt; x2: LongInt; y2: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Draws a filled rectangular bar at a specified position.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="x1">
    ///   the distance from left of layer to the left border of the rectangle, in pixels
    /// </param>
    /// <param name="y1">
    ///   the distance from top of layer to the top border of the rectangle, in pixels
    /// </param>
    /// <param name="x2">
    ///   the distance from left of layer to the right border of the rectangle, in pixels
    /// </param>
    /// <param name="y2">
    ///   the distance from top of layer to the bottom border of the rectangle, in pixels
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function drawBar(x1: LongInt; y1: LongInt; x2: LongInt; y2: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Draws an empty circle at a specified position.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="x">
    ///   the distance from left of layer to the center of the circle, in pixels
    /// </param>
    /// <param name="y">
    ///   the distance from top of layer to the center of the circle, in pixels
    /// </param>
    /// <param name="r">
    ///   the radius of the circle, in pixels
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function drawCircle(x: LongInt; y: LongInt; r: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Draws a filled disc at a given position.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="x">
    ///   the distance from left of layer to the center of the disc, in pixels
    /// </param>
    /// <param name="y">
    ///   the distance from top of layer to the center of the disc, in pixels
    /// </param>
    /// <param name="r">
    ///   the radius of the disc, in pixels
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function drawDisc(x: LongInt; y: LongInt; r: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Selects a font to use for the next text drawing functions, by providing the name of the
    ///   font file.
    /// <para>
    ///   You can use a built-in font as well as a font file that you have previously
    ///   uploaded to the device built-in memory. If you experience problems selecting a font
    ///   file, check the device logs for any error message such as missing font file or bad font
    ///   file format.
    /// </para>
    /// </summary>
    /// <param name="fontname">
    ///   the font file name
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function selectFont(fontname: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Draws a text string at the specified position.
    /// <para>
    ///   The point of the text that is aligned
    ///   to the specified pixel position is called the anchor point, and can be chosen among
    ///   several options. Text is rendered from left to right, without implicit wrapping.
    /// </para>
    /// </summary>
    /// <param name="x">
    ///   the distance from left of layer to the text anchor point, in pixels
    /// </param>
    /// <param name="y">
    ///   the distance from top of layer to the text anchor point, in pixels
    /// </param>
    /// <param name="anchor">
    ///   the text anchor point, chosen among the <c>Y_ALIGN</c> enumeration:
    ///   <c>Y_ALIGN_TOP_LEFT</c>,    <c>Y_ALIGN_CENTER_LEFT</c>,    <c>Y_ALIGN_BASELINE_LEFT</c>,   
    ///   <c>Y_ALIGN_BOTTOM_LEFT</c>,
    ///   <c>Y_ALIGN_TOP_CENTER</c>,  <c>Y_ALIGN_CENTER</c>,         <c>Y_ALIGN_BASELINE_CENTER</c>, 
    ///   <c>Y_ALIGN_BOTTOM_CENTER</c>,
    ///   <c>Y_ALIGN_TOP_DECIMAL</c>, <c>Y_ALIGN_CENTER_DECIMAL</c>, <c>Y_ALIGN_BASELINE_DECIMAL</c>,
    ///   <c>Y_ALIGN_BOTTOM_DECIMAL</c>,
    ///   <c>Y_ALIGN_TOP_RIGHT</c>,   <c>Y_ALIGN_CENTER_RIGHT</c>,   <c>Y_ALIGN_BASELINE_RIGHT</c>,  
    ///   <c>Y_ALIGN_BOTTOM_RIGHT</c>.
    /// </param>
    /// <param name="text">
    ///   the text string to draw
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function drawText(x: LongInt; y: LongInt; anchor: TYALIGN; text: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Draws a GIF image at the specified position.
    /// <para>
    ///   The GIF image must have been previously
    ///   uploaded to the device built-in memory. If you experience problems using an image
    ///   file, check the device logs for any error message such as missing image file or bad
    ///   image file format.
    /// </para>
    /// </summary>
    /// <param name="x">
    ///   the distance from left of layer to the left of the image, in pixels
    /// </param>
    /// <param name="y">
    ///   the distance from top of layer to the top of the image, in pixels
    /// </param>
    /// <param name="imagename">
    ///   the GIF file name
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function drawImage(x: LongInt; y: LongInt; imagename: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Draws a bitmap at the specified position.
    /// <para>
    ///   The bitmap is provided as a binary object,
    ///   where each pixel maps to a bit, from left to right and from top to bottom.
    ///   The most significant bit of each byte maps to the leftmost pixel, and the least
    ///   significant bit maps to the rightmost pixel. Bits set to 1 are drawn using the
    ///   layer selected pen color. Bits set to 0 are drawn using the specified background
    ///   gray level, unless -1 is specified, in which case they are not drawn at all
    ///   (as if transparent).
    /// </para>
    /// </summary>
    /// <param name="x">
    ///   the distance from left of layer to the left of the bitmap, in pixels
    /// </param>
    /// <param name="y">
    ///   the distance from top of layer to the top of the bitmap, in pixels
    /// </param>
    /// <param name="w">
    ///   the width of the bitmap, in pixels
    /// </param>
    /// <param name="bitmap">
    ///   a binary object
    /// </param>
    /// <param name="bgcol">
    ///   the background gray level to use for zero bits (0 = black,
    ///   255 = white), or -1 to leave the pixels unchanged
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function drawBitmap(x: LongInt; y: LongInt; w: LongInt; bitmap: TByteArray; bgcol: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Moves the drawing pointer of this layer to the specified position.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="x">
    ///   the distance from left of layer, in pixels
    /// </param>
    /// <param name="y">
    ///   the distance from top of layer, in pixels
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function moveTo(x: LongInt; y: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Draws a line from current drawing pointer position to the specified position.
    /// <para>
    ///   The specified destination pixel is included in the line. The pointer position
    ///   is then moved to the end point of the line.
    /// </para>
    /// </summary>
    /// <param name="x">
    ///   the distance from left of layer to the end point of the line, in pixels
    /// </param>
    /// <param name="y">
    ///   the distance from top of layer to the end point of the line, in pixels
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function lineTo(x: LongInt; y: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Outputs a message in the console area, and advances the console pointer accordingly.
    /// <para>
    ///   The console pointer position is automatically moved to the beginning
    ///   of the next line when a newline character is met, or when the right margin
    ///   is hit. When the new text to display extends below the lower margin, the
    ///   console area is automatically scrolled up.
    /// </para>
    /// </summary>
    /// <param name="text">
    ///   the message to display
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function consoleOut(text: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets up display margins for the <c>consoleOut</c> function.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="x1">
    ///   the distance from left of layer to the left margin, in pixels
    /// </param>
    /// <param name="y1">
    ///   the distance from top of layer to the top margin, in pixels
    /// </param>
    /// <param name="x2">
    ///   the distance from left of layer to the right margin, in pixels
    /// </param>
    /// <param name="y2">
    ///   the distance from top of layer to the bottom margin, in pixels
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function setConsoleMargins(x1: LongInt; y1: LongInt; x2: LongInt; y2: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets up the background color used by the <c>clearConsole</c> function and by
    ///   the console scrolling feature.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="bgcol">
    ///   the background gray level to use when scrolling (0 = black,
    ///   255 = white), or -1 for transparent
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function setConsoleBackground(bgcol: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets up the wrapping behaviour used by the <c>consoleOut</c> function.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="wordwrap">
    ///   <c>true</c> to wrap only between words,
    ///   <c>false</c> to wrap on the last column anyway.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function setConsoleWordWrap(wordwrap: boolean):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Blanks the console area within console margins, and resets the console pointer
    ///   to the upper left corner of the console.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function clearConsole():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets the position of the layer relative to the display upper left corner.
    /// <para>
    ///   When smooth scrolling is used, the display offset of the layer is
    ///   automatically updated during the next milliseconds to animate the move of the layer.
    /// </para>
    /// </summary>
    /// <param name="x">
    ///   the distance from left of display to the upper left corner of the layer
    /// </param>
    /// <param name="y">
    ///   the distance from top of display to the upper left corner of the layer
    /// </param>
    /// <param name="scrollTime">
    ///   number of milliseconds to use for smooth scrolling, or
    ///   0 if the scrolling should be immediate.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function setLayerPosition(x: LongInt; y: LongInt; scrollTime: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Hides the layer.
    /// <para>
    ///   The state of the layer is perserved but the layer is not displayed
    ///   on the screen until the next call to <c>unhide()</c>. Hiding the layer can positively
    ///   affect the drawing speed, since it postpones the rendering until all operations are
    ///   completed (double-buffering).
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function hide():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Shows the layer.
    /// <para>
    ///   Shows the layer again after a hide command.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function unhide():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Gets parent YDisplay.
    /// <para>
    ///   Returns the parent YDisplay object of the current YDisplayLayer.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an <c>YDisplay</c> object
    /// </returns>
    ///-
    function get_display():TYDisplay; overload; virtual;

    ////
    /// <summary>
    ///   Returns the display width, in pixels.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the display width, in pixels
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns Y_DISPLAYWIDTH_INVALID.
    /// </para>
    ///-
    function get_displayWidth():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the display height, in pixels.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the display height, in pixels
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns Y_DISPLAYHEIGHT_INVALID.
    /// </para>
    ///-
    function get_displayHeight():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the width of the layers to draw on, in pixels.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the width of the layers to draw on, in pixels
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns Y_LAYERWIDTH_INVALID.
    /// </para>
    ///-
    function get_layerWidth():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the height of the layers to draw on, in pixels.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the height of the layers to draw on, in pixels
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns Y_LAYERHEIGHT_INVALID.
    /// </para>
    ///-
    function get_layerHeight():LongInt; overload; virtual;

    function resetHiddenFlag():LongInt; overload; virtual;


  //--- (end of generated code: YDisplayLayer accessors declaration)
end;

//--- (generated code: Display functions declaration)
  ////
  /// <summary>
  ///   Retrieves a display for a given identifier.
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
  ///   This function does not require that the display is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YDisplay.isOnline()</c> to test if the display is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a display by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the display
  /// </param>
  /// <returns>
  ///   a <c>YDisplay</c> object allowing you to drive the display.
  /// </returns>
  ///-
  function yFindDisplay(func:string):TYDisplay;
  ////
  /// <summary>
  ///   Starts the enumeration of displays currently accessible.
  /// <para>
  ///   Use the method <c>YDisplay.nextDisplay()</c> to iterate on
  ///   next displays.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YDisplay</c> object, corresponding to
  ///   the first display currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstDisplay():TYDisplay;

//--- (end of generated code: Display functions declaration)

implementation

destructor TYDisplay.destroy();
 var
  i:integer;
 begin
  if _allDisplayLayers<>nil then
   begin
    for i:=0 to  length(_allDisplayLayers)-1 do
      begin
        _allDisplayLayers[i].free();
        _allDisplayLayers[i]:=nil;
      end;
    setlength(_allDisplayLayers,0);

  end;
  inherited destroy();
 end;


 constructor TYDisplay.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Display';
	  //--- (generated code: YDisplay accessors initialization)
      _enabled := Y_ENABLED_INVALID;
      _startupSeq := Y_STARTUPSEQ_INVALID;
      _brightness := Y_BRIGHTNESS_INVALID;
      _orientation := Y_ORIENTATION_INVALID;
      _displayWidth := Y_DISPLAYWIDTH_INVALID;
      _displayHeight := Y_DISPLAYHEIGHT_INVALID;
      _displayType := Y_DISPLAYTYPE_INVALID;
      _layerWidth := Y_LAYERWIDTH_INVALID;
      _layerHeight := Y_LAYERHEIGHT_INVALID;
      _layerCount := Y_LAYERCOUNT_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackDisplay := nil;
      //--- (end of generated code: YDisplay accessors initialization)
    end;


//--- (generated code: YDisplay implementation)
{$HINTS OFF}
  function TYDisplay._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'enabled') then
        begin
          _enabled := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'startupSeq') then
        begin
          _startupSeq := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'brightness') then
        begin
          _brightness := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'orientation') then
        begin
          _orientation := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'displayWidth') then
        begin
          _displayWidth := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'displayHeight') then
        begin
          _displayHeight := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'displayType') then
        begin
          _displayType := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'layerWidth') then
        begin
          _layerWidth := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'layerHeight') then
        begin
          _layerHeight := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'layerCount') then
        begin
          _layerCount := integer(member^.ivalue);
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

  ////
  /// <summary>
  ///   Returns true if the screen is powered, false otherwise.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_ENABLED_FALSE or Y_ENABLED_TRUE, according to true if the screen is powered, false otherwise
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ENABLED_INVALID.
  /// </para>
  ///-
  function TYDisplay.get_enabled():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ENABLED_INVALID;
              exit
            end;
        end;
      result := self._enabled;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the power state of the display.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   either Y_ENABLED_FALSE or Y_ENABLED_TRUE, according to the power state of the display
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
  function TYDisplay.set_enabled(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('enabled',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the name of the sequence to play when the displayed is powered on.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the name of the sequence to play when the displayed is powered on
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_STARTUPSEQ_INVALID.
  /// </para>
  ///-
  function TYDisplay.get_startupSeq():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_STARTUPSEQ_INVALID;
              exit
            end;
        end;
      result := self._startupSeq;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the name of the sequence to play when the displayed is powered on.
  /// <para>
  ///   Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the name of the sequence to play when the displayed is powered on
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
  function TYDisplay.set_startupSeq(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('startupSeq',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the luminosity of the  module informative leds (from 0 to 100).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the luminosity of the  module informative leds (from 0 to 100)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_BRIGHTNESS_INVALID.
  /// </para>
  ///-
  function TYDisplay.get_brightness():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_BRIGHTNESS_INVALID;
              exit
            end;
        end;
      result := self._brightness;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the brightness of the display.
  /// <para>
  ///   The parameter is a value between 0 and
  ///   100. Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the brightness of the display
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
  function TYDisplay.set_brightness(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('brightness',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the currently selected display orientation.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_ORIENTATION_LEFT, Y_ORIENTATION_UP, Y_ORIENTATION_RIGHT and Y_ORIENTATION_DOWN
  ///   corresponding to the currently selected display orientation
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ORIENTATION_INVALID.
  /// </para>
  ///-
  function TYDisplay.get_orientation():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ORIENTATION_INVALID;
              exit
            end;
        end;
      result := self._orientation;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the display orientation.
  /// <para>
  ///   Remember to call the saveToFlash()
  ///   method of the module if the modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_ORIENTATION_LEFT, Y_ORIENTATION_UP, Y_ORIENTATION_RIGHT and Y_ORIENTATION_DOWN
  ///   corresponding to the display orientation
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
  function TYDisplay.set_orientation(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('orientation',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the display width, in pixels.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the display width, in pixels
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DISPLAYWIDTH_INVALID.
  /// </para>
  ///-
  function TYDisplay.get_displayWidth():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_DISPLAYWIDTH_INVALID;
              exit
            end;
        end;
      result := self._displayWidth;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the display height, in pixels.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the display height, in pixels
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DISPLAYHEIGHT_INVALID.
  /// </para>
  ///-
  function TYDisplay.get_displayHeight():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_DISPLAYHEIGHT_INVALID;
              exit
            end;
        end;
      result := self._displayHeight;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the display type: monochrome, gray levels or full color.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_DISPLAYTYPE_MONO, Y_DISPLAYTYPE_GRAY and Y_DISPLAYTYPE_RGB corresponding to the
  ///   display type: monochrome, gray levels or full color
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DISPLAYTYPE_INVALID.
  /// </para>
  ///-
  function TYDisplay.get_displayType():Integer;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_DISPLAYTYPE_INVALID;
              exit
            end;
        end;
      result := self._displayType;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the width of the layers to draw on, in pixels.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the width of the layers to draw on, in pixels
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LAYERWIDTH_INVALID.
  /// </para>
  ///-
  function TYDisplay.get_layerWidth():LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LAYERWIDTH_INVALID;
              exit
            end;
        end;
      result := self._layerWidth;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the height of the layers to draw on, in pixels.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the height of the layers to draw on, in pixels
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LAYERHEIGHT_INVALID.
  /// </para>
  ///-
  function TYDisplay.get_layerHeight():LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LAYERHEIGHT_INVALID;
              exit
            end;
        end;
      result := self._layerHeight;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the number of available layers to draw on.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the number of available layers to draw on
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LAYERCOUNT_INVALID.
  /// </para>
  ///-
  function TYDisplay.get_layerCount():LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LAYERCOUNT_INVALID;
              exit
            end;
        end;
      result := self._layerCount;
      exit;
    end;


  function TYDisplay.get_command():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_COMMAND_INVALID;
              exit
            end;
        end;
      result := self._command;
      exit;
    end;


  function TYDisplay.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
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
  ///   Use the method <c>YDisplay.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YDisplay</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYDisplay.FindDisplay(func: string):TYDisplay;
    var
      obj : TYDisplay;
    begin
      obj := TYDisplay(TYFunction._FindFromCache('Display', func));
      if obj = nil then
        begin
          obj :=  TYDisplay.create(func);
          TYFunction._AddToCache('Display',  func, obj)
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
  function TYDisplay.registerValueCallback(callback: TYDisplayValueCallback):LongInt;
    var
      val : string;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateValueCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateValueCallbackList(self, false)
        end;
      self._valueCallbackDisplay := callback;
      // Immediately invoke value callback with current value
      if (addr(callback) <> nil) and self.isOnline then
        begin
          val := self._advertisedValue;
          if not((val = '')) then
            begin
              self._invokeValueCallback(val)
            end;
        end;
      result := 0;
      exit;
    end;


  function TYDisplay._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackDisplay) <> nil) then
        begin
          self._valueCallbackDisplay(self, value)
        end
      else
        begin
          inherited _invokeValueCallback(value)
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Clears the display screen and resets all display layers to their default state.
  /// <para>
  ///   Using this function in a sequence will kill the sequence play-back. Don't use that
  ///   function to reset the display at sequence start-up.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplay.resetAll():LongInt;
    begin
      self.flushLayers;
      self.resetHiddenLayerFlags;
      result := self.sendCommand('Z');
      exit;
    end;


  ////
  /// <summary>
  ///   Smoothly changes the brightness of the screen to produce a fade-in or fade-out
  ///   effect.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="brightness">
  ///   the new screen brightness
  /// </param>
  /// <param name="duration">
  ///   duration of the brightness transition, in milliseconds.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplay.fade(brightness: LongInt; duration: LongInt):LongInt;
    begin
      self.flushLayers;
      result := self.sendCommand('+'+inttostr(brightness)+','+inttostr(duration));
      exit;
    end;


  ////
  /// <summary>
  ///   Starts to record all display commands into a sequence, for later replay.
  /// <para>
  ///   The name used to store the sequence is specified when calling
  ///   <c>saveSequence()</c>, once the recording is complete.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplay.newSequence():LongInt;
    begin
      self.flushLayers;
      self._sequence := '';
      self._recording := true;
      result := YAPI_SUCCESS;
      exit;
    end;


  ////
  /// <summary>
  ///   Stops recording display commands and saves the sequence into the specified
  ///   file on the display internal memory.
  /// <para>
  ///   The sequence can be later replayed
  ///   using <c>playSequence()</c>.
  /// </para>
  /// </summary>
  /// <param name="sequenceName">
  ///   the name of the newly created sequence
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplay.saveSequence(sequenceName: string):LongInt;
    begin
      self.flushLayers;
      self._recording := false;
      self._upload(sequenceName, _StrToByte(self._sequence));
      //We need to use YPRINTF("") for Objective-C 
      self._sequence := '';
      result := YAPI_SUCCESS;
      exit;
    end;


  ////
  /// <summary>
  ///   Replays a display sequence previously recorded using
  ///   <c>newSequence()</c> and <c>saveSequence()</c>.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="sequenceName">
  ///   the name of the newly created sequence
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplay.playSequence(sequenceName: string):LongInt;
    begin
      self.flushLayers;
      result := self.sendCommand('S'+sequenceName);
      exit;
    end;


  ////
  /// <summary>
  ///   Waits for a specified delay (in milliseconds) before playing next
  ///   commands in current sequence.
  /// <para>
  ///   This method can be used while
  ///   recording a display sequence, to insert a timed wait in the sequence
  ///   (without any immediate effect). It can also be used dynamically while
  ///   playing a pre-recorded sequence, to suspend or resume the execution of
  ///   the sequence. To cancel a delay, call the same method with a zero delay.
  /// </para>
  /// </summary>
  /// <param name="delay_ms">
  ///   the duration to wait, in milliseconds
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplay.pauseSequence(delay_ms: LongInt):LongInt;
    begin
      self.flushLayers;
      result := self.sendCommand('W'+inttostr(delay_ms));
      exit;
    end;


  ////
  /// <summary>
  ///   Stops immediately any ongoing sequence replay.
  /// <para>
  ///   The display is left as is.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplay.stopSequence():LongInt;
    begin
      self.flushLayers;
      result := self.sendCommand('S');
      exit;
    end;


  ////
  /// <summary>
  ///   Uploads an arbitrary file (for instance a GIF file) to the display, to the
  ///   specified full path name.
  /// <para>
  ///   If a file already exists with the same path name,
  ///   its content is overwritten.
  /// </para>
  /// </summary>
  /// <param name="pathname">
  ///   path and name of the new file to create
  /// </param>
  /// <param name="content">
  ///   binary buffer with the content to set
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplay.upload(pathname: string; content: TByteArray):LongInt;
    begin
      result := self._upload(pathname, content);
      exit;
    end;


  ////
  /// <summary>
  ///   Copies the whole content of a layer to another layer.
  /// <para>
  ///   The color and transparency
  ///   of all the pixels from the destination layer are set to match the source pixels.
  ///   This method only affects the displayed content, but does not change any
  ///   property of the layer object.
  ///   Note that layer 0 has no transparency support (it is always completely opaque).
  /// </para>
  /// </summary>
  /// <param name="srcLayerId">
  ///   the identifier of the source layer (a number in range 0..layerCount-1)
  /// </param>
  /// <param name="dstLayerId">
  ///   the identifier of the destination layer (a number in range 0..layerCount-1)
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplay.copyLayerContent(srcLayerId: LongInt; dstLayerId: LongInt):LongInt;
    begin
      self.flushLayers;
      result := self.sendCommand('o'+inttostr(srcLayerId)+','+inttostr(dstLayerId));
      exit;
    end;


  ////
  /// <summary>
  ///   Swaps the whole content of two layers.
  /// <para>
  ///   The color and transparency of all the pixels from
  ///   the two layers are swapped. This method only affects the displayed content, but does
  ///   not change any property of the layer objects. In particular, the visibility of each
  ///   layer stays unchanged. When used between onae hidden layer and a visible layer,
  ///   this method makes it possible to easily implement double-buffering.
  ///   Note that layer 0 has no transparency support (it is always completely opaque).
  /// </para>
  /// </summary>
  /// <param name="layerIdA">
  ///   the first layer (a number in range 0..layerCount-1)
  /// </param>
  /// <param name="layerIdB">
  ///   the second layer (a number in range 0..layerCount-1)
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplay.swapLayerContent(layerIdA: LongInt; layerIdB: LongInt):LongInt;
    begin
      self.flushLayers;
      result := self.sendCommand('E'+inttostr(layerIdA)+','+inttostr(layerIdB));
      exit;
    end;


  function TYDisplay.nextDisplay(): TYDisplay;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextDisplay := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextDisplay := nil;
          exit;
        end;
      nextDisplay := TYDisplay.FindDisplay(hwid);
    end;

  class function TYDisplay.FirstDisplay(): TYDisplay;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Display', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYDisplay.FindDisplay(serial+'.'+funcId);
    end;

//--- (end of generated code: YDisplay implementation)


////
///
///-
function  TYDisplay.flushLayers() :integer;
  var i:integer;
    begin
      if  (_allDisplayLayers<>nil) then
        begin
           for i:=0 to length(_allDisplayLayers)-1 do
             begin
                TYdisplayLayer(_allDisplayLayers[i]).flush_now();
             end;
        end;

       flushLayers := YAPI_SUCCESS;
  end ;

function  TYDisplay.sendCommand(cmd :string ):integer ;
  begin
    if not(_recording) then
      begin
        sendCommand :=self.set_command(cmd);
      end else
      begin
        _sequence := _sequence +  cmd+#13;
        sendCommand := YAPI_SUCCESS;
      end;
  end;

procedure TYDisplay.resetHiddenLayerFlags();
  var i:integer;
    begin
      if  (_allDisplayLayers<>nil) then
        begin
           for i:=0 to length(_allDisplayLayers)-1 do
             begin
                TYdisplayLayer(_allDisplayLayers[i]).resetHiddenFlag();
             end;
        end;


  end ;


//--- (generated code: Display functions)

  function yFindDisplay(func:string): TYDisplay;
    begin
      result := TYDisplay.FindDisplay(func);
    end;

  function yFirstDisplay(): TYDisplay;
    begin
      result := TYDisplay.FirstDisplay();
    end;

  procedure _DisplayCleanup();
    begin
    end;

//--- (end of generated code: Display functions)

////
/// <summary>
///   Returns a YDisplayLayer object that can be used to draw on the specified
///   layer. The content will only be displayed when the layer is active on the
///   screen (and not masked by other overlapping layers).
/// <para>
/// </para>
/// <param name="layerId">
///   the identifier of the layer (a string containing one character)
/// </param>
/// </summary>
/// <returns>
///   an YDisplayLayer object
/// </returns>
/// <para>
///    On failure, throws an exception or returns nil.
/// </para>
///-
function TYDisplay.get_displayLayer(layerId:integer):TYDisplayLayer;
  var
  layercount : integer;
  i:integer;
   begin
      if (layerId<0) then
         begin
            _throw(YAPI_INVALID_ARGUMENT,'Negative layer number are not allowed');
            get_displayLayer:=nil;
            exit;
         end;

      layercount :=  get_layerCount();
      if (layerId>=layercount) then
        begin
          _throw(YAPI_INVALID_ARGUMENT,'This display only have '+inttostr(layercount)+' layers');
          get_displayLayer:=nil;
          exit;
        end;

       if  (length(_allDisplayLayers)<=0) then
        begin
          setlength(_allDisplayLayers,layercount);
           for i:=0 to layercount-1 do
             _allDisplayLayers[i] :=TYDisplayLayer.create(self, inttostr(i));
        end;


       get_displayLayer := _allDisplayLayers[layerId];
   end;

 constructor TYDisplayLayer.Create(parent: TYdisplay; id :string);
 begin
    inherited create();
    self._display := parent;
    self._id := strtoint(id);
    self._cmdbuff:='';
 end;



function  TYDisplayLayer.flush_now():integer;
  var res:integer;
 begin
  res:=YAPI_SUCCESS;
  if (self._cmdbuff<>'') then
   begin
    res := self._display.sendCommand(self._cmdbuff);
    self._cmdbuff := '';
   end;
 flush_now :=res;
 end;

function TYDisplayLayer.command_push(cmd:string):integer;
var res:integer;
 begin
    res:=YAPI_SUCCESS;
    if (length(self._cmdbuff) + length(cmd) >=100) then res:=flush_now();
    if (self._cmdbuff='') then self._cmdbuff := inttostr(self._id);
    self._cmdbuff:=self._cmdbuff+cmd;
    command_push := res ;
 end;

function TYDisplayLayer.command_flush(cmd:string):integer;
 var
   res : integer;
 begin
  res:=command_push(cmd);
  if not(_hidden) then res:= flush_now();
  command_flush := res;
 end;


//--- (generated code: YDisplayLayer implementation)

  ////
  /// <summary>
  ///   Reverts the layer to its initial state (fully transparent, default settings).
  /// <para>
  ///   Reinitializes the drawing pointer to the upper left position,
  ///   and selects the most visible pen color. If you only want to erase the layer
  ///   content, use the method <c>clear()</c> instead.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.reset():LongInt;
    begin
      self._hidden := false;
      result := self.command_flush('X');
      exit;
    end;


  ////
  /// <summary>
  ///   Erases the whole content of the layer (makes it fully transparent).
  /// <para>
  ///   This method does not change any other attribute of the layer.
  ///   To reinitialize the layer attributes to defaults settings, use the method
  ///   <c>reset()</c> instead.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.clear():LongInt;
    begin
      result := self.command_flush('x');
      exit;
    end;


  ////
  /// <summary>
  ///   Selects the pen color for all subsequent drawing functions,
  ///   including text drawing.
  /// <para>
  ///   The pen color is provided as an RGB value.
  ///   For grayscale or monochrome displays, the value is
  ///   automatically converted to the proper range.
  /// </para>
  /// </summary>
  /// <param name="color">
  ///   the desired pen color, as a 24-bit RGB value
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.selectColorPen(color: LongInt):LongInt;
    begin
      result := self.command_push('c'+inttohex(color,06));
      exit;
    end;


  ////
  /// <summary>
  ///   Selects the pen gray level for all subsequent drawing functions,
  ///   including text drawing.
  /// <para>
  ///   The gray level is provided as a number between
  ///   0 (black) and 255 (white, or whichever the lighest color is).
  ///   For monochrome displays (without gray levels), any value
  ///   lower than 128 is rendered as black, and any value equal
  ///   or above to 128 is non-black.
  /// </para>
  /// </summary>
  /// <param name="graylevel">
  ///   the desired gray level, from 0 to 255
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.selectGrayPen(graylevel: LongInt):LongInt;
    begin
      result := self.command_push('g'+inttostr(graylevel));
      exit;
    end;


  ////
  /// <summary>
  ///   Selects an eraser instead of a pen for all subsequent drawing functions,
  ///   except for bitmap copy functions.
  /// <para>
  ///   Any point drawn using the eraser
  ///   becomes transparent (as when the layer is empty), showing the other
  ///   layers beneath it.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.selectEraser():LongInt;
    begin
      result := self.command_push('e');
      exit;
    end;


  ////
  /// <summary>
  ///   Enables or disables anti-aliasing for drawing oblique lines and circles.
  /// <para>
  ///   Anti-aliasing provides a smoother aspect when looked from far enough,
  ///   but it can add fuzzyness when the display is looked from very close.
  ///   At the end of the day, it is your personal choice.
  ///   Anti-aliasing is enabled by default on grayscale and color displays,
  ///   but you can disable it if you prefer. This setting has no effect
  ///   on monochrome displays.
  /// </para>
  /// </summary>
  /// <param name="mode">
  ///   <t>true</t> to enable antialiasing, <t>false</t> to
  ///   disable it.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.setAntialiasingMode(mode: boolean):LongInt;
    begin
      result := self.command_push('a'+_yapiBoolToStr(mode));
      exit;
    end;


  ////
  /// <summary>
  ///   Draws a single pixel at the specified position.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="x">
  ///   the distance from left of layer, in pixels
  /// </param>
  /// <param name="y">
  ///   the distance from top of layer, in pixels
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.drawPixel(x: LongInt; y: LongInt):LongInt;
    begin
      result := self.command_flush('P'+inttostr(x)+','+inttostr(y));
      exit;
    end;


  ////
  /// <summary>
  ///   Draws an empty rectangle at a specified position.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="x1">
  ///   the distance from left of layer to the left border of the rectangle, in pixels
  /// </param>
  /// <param name="y1">
  ///   the distance from top of layer to the top border of the rectangle, in pixels
  /// </param>
  /// <param name="x2">
  ///   the distance from left of layer to the right border of the rectangle, in pixels
  /// </param>
  /// <param name="y2">
  ///   the distance from top of layer to the bottom border of the rectangle, in pixels
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.drawRect(x1: LongInt; y1: LongInt; x2: LongInt; y2: LongInt):LongInt;
    begin
      result := self.command_flush('R'+inttostr(x1)+','+inttostr(y1)+','+inttostr(x2)+','+inttostr(y2));
      exit;
    end;


  ////
  /// <summary>
  ///   Draws a filled rectangular bar at a specified position.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="x1">
  ///   the distance from left of layer to the left border of the rectangle, in pixels
  /// </param>
  /// <param name="y1">
  ///   the distance from top of layer to the top border of the rectangle, in pixels
  /// </param>
  /// <param name="x2">
  ///   the distance from left of layer to the right border of the rectangle, in pixels
  /// </param>
  /// <param name="y2">
  ///   the distance from top of layer to the bottom border of the rectangle, in pixels
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.drawBar(x1: LongInt; y1: LongInt; x2: LongInt; y2: LongInt):LongInt;
    begin
      result := self.command_flush('B'+inttostr(x1)+','+inttostr(y1)+','+inttostr(x2)+','+inttostr(y2));
      exit;
    end;


  ////
  /// <summary>
  ///   Draws an empty circle at a specified position.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="x">
  ///   the distance from left of layer to the center of the circle, in pixels
  /// </param>
  /// <param name="y">
  ///   the distance from top of layer to the center of the circle, in pixels
  /// </param>
  /// <param name="r">
  ///   the radius of the circle, in pixels
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.drawCircle(x: LongInt; y: LongInt; r: LongInt):LongInt;
    begin
      result := self.command_flush('C'+inttostr(x)+','+inttostr(y)+','+inttostr(r));
      exit;
    end;


  ////
  /// <summary>
  ///   Draws a filled disc at a given position.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="x">
  ///   the distance from left of layer to the center of the disc, in pixels
  /// </param>
  /// <param name="y">
  ///   the distance from top of layer to the center of the disc, in pixels
  /// </param>
  /// <param name="r">
  ///   the radius of the disc, in pixels
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.drawDisc(x: LongInt; y: LongInt; r: LongInt):LongInt;
    begin
      result := self.command_flush('D'+inttostr(x)+','+inttostr(y)+','+inttostr(r));
      exit;
    end;


  ////
  /// <summary>
  ///   Selects a font to use for the next text drawing functions, by providing the name of the
  ///   font file.
  /// <para>
  ///   You can use a built-in font as well as a font file that you have previously
  ///   uploaded to the device built-in memory. If you experience problems selecting a font
  ///   file, check the device logs for any error message such as missing font file or bad font
  ///   file format.
  /// </para>
  /// </summary>
  /// <param name="fontname">
  ///   the font file name
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.selectFont(fontname: string):LongInt;
    begin
      result := self.command_push('&'+fontname+''+chr(27));
      exit;
    end;


  ////
  /// <summary>
  ///   Draws a text string at the specified position.
  /// <para>
  ///   The point of the text that is aligned
  ///   to the specified pixel position is called the anchor point, and can be chosen among
  ///   several options. Text is rendered from left to right, without implicit wrapping.
  /// </para>
  /// </summary>
  /// <param name="x">
  ///   the distance from left of layer to the text anchor point, in pixels
  /// </param>
  /// <param name="y">
  ///   the distance from top of layer to the text anchor point, in pixels
  /// </param>
  /// <param name="anchor">
  ///   the text anchor point, chosen among the <c>Y_ALIGN</c> enumeration:
  ///   <c>Y_ALIGN_TOP_LEFT</c>,    <c>Y_ALIGN_CENTER_LEFT</c>,    <c>Y_ALIGN_BASELINE_LEFT</c>,   
  ///   <c>Y_ALIGN_BOTTOM_LEFT</c>,
  ///   <c>Y_ALIGN_TOP_CENTER</c>,  <c>Y_ALIGN_CENTER</c>,         <c>Y_ALIGN_BASELINE_CENTER</c>, 
  ///   <c>Y_ALIGN_BOTTOM_CENTER</c>,
  ///   <c>Y_ALIGN_TOP_DECIMAL</c>, <c>Y_ALIGN_CENTER_DECIMAL</c>, <c>Y_ALIGN_BASELINE_DECIMAL</c>,
  ///   <c>Y_ALIGN_BOTTOM_DECIMAL</c>,
  ///   <c>Y_ALIGN_TOP_RIGHT</c>,   <c>Y_ALIGN_CENTER_RIGHT</c>,   <c>Y_ALIGN_BASELINE_RIGHT</c>,  
  ///   <c>Y_ALIGN_BOTTOM_RIGHT</c>.
  /// </param>
  /// <param name="text">
  ///   the text string to draw
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.drawText(x: LongInt; y: LongInt; anchor: TYALIGN; text: string):LongInt;
    begin
      result := self.command_flush('T'+inttostr(x)+','+inttostr(y)+','+inttostr(ord(anchor))+','+text+''+chr(27));
      exit;
    end;


  ////
  /// <summary>
  ///   Draws a GIF image at the specified position.
  /// <para>
  ///   The GIF image must have been previously
  ///   uploaded to the device built-in memory. If you experience problems using an image
  ///   file, check the device logs for any error message such as missing image file or bad
  ///   image file format.
  /// </para>
  /// </summary>
  /// <param name="x">
  ///   the distance from left of layer to the left of the image, in pixels
  /// </param>
  /// <param name="y">
  ///   the distance from top of layer to the top of the image, in pixels
  /// </param>
  /// <param name="imagename">
  ///   the GIF file name
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.drawImage(x: LongInt; y: LongInt; imagename: string):LongInt;
    begin
      result := self.command_flush('*'+inttostr(x)+','+inttostr(y)+','+imagename+''+chr(27));
      exit;
    end;


  ////
  /// <summary>
  ///   Draws a bitmap at the specified position.
  /// <para>
  ///   The bitmap is provided as a binary object,
  ///   where each pixel maps to a bit, from left to right and from top to bottom.
  ///   The most significant bit of each byte maps to the leftmost pixel, and the least
  ///   significant bit maps to the rightmost pixel. Bits set to 1 are drawn using the
  ///   layer selected pen color. Bits set to 0 are drawn using the specified background
  ///   gray level, unless -1 is specified, in which case they are not drawn at all
  ///   (as if transparent).
  /// </para>
  /// </summary>
  /// <param name="x">
  ///   the distance from left of layer to the left of the bitmap, in pixels
  /// </param>
  /// <param name="y">
  ///   the distance from top of layer to the top of the bitmap, in pixels
  /// </param>
  /// <param name="w">
  ///   the width of the bitmap, in pixels
  /// </param>
  /// <param name="bitmap">
  ///   a binary object
  /// </param>
  /// <param name="bgcol">
  ///   the background gray level to use for zero bits (0 = black,
  ///   255 = white), or -1 to leave the pixels unchanged
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.drawBitmap(x: LongInt; y: LongInt; w: LongInt; bitmap: TByteArray; bgcol: LongInt):LongInt;
    var
      destname : string;
    begin
      destname := 'layer'+inttostr(self._id)+':'+inttostr(w)+','+inttostr(bgcol)+'@'+inttostr(x)+','+inttostr(y);
      result := self._display.upload(destname, bitmap);
      exit;
    end;


  ////
  /// <summary>
  ///   Moves the drawing pointer of this layer to the specified position.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="x">
  ///   the distance from left of layer, in pixels
  /// </param>
  /// <param name="y">
  ///   the distance from top of layer, in pixels
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.moveTo(x: LongInt; y: LongInt):LongInt;
    begin
      result := self.command_push('@'+inttostr(x)+','+inttostr(y));
      exit;
    end;


  ////
  /// <summary>
  ///   Draws a line from current drawing pointer position to the specified position.
  /// <para>
  ///   The specified destination pixel is included in the line. The pointer position
  ///   is then moved to the end point of the line.
  /// </para>
  /// </summary>
  /// <param name="x">
  ///   the distance from left of layer to the end point of the line, in pixels
  /// </param>
  /// <param name="y">
  ///   the distance from top of layer to the end point of the line, in pixels
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.lineTo(x: LongInt; y: LongInt):LongInt;
    begin
      result := self.command_flush('-'+inttostr(x)+','+inttostr(y));
      exit;
    end;


  ////
  /// <summary>
  ///   Outputs a message in the console area, and advances the console pointer accordingly.
  /// <para>
  ///   The console pointer position is automatically moved to the beginning
  ///   of the next line when a newline character is met, or when the right margin
  ///   is hit. When the new text to display extends below the lower margin, the
  ///   console area is automatically scrolled up.
  /// </para>
  /// </summary>
  /// <param name="text">
  ///   the message to display
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.consoleOut(text: string):LongInt;
    begin
      result := self.command_flush('!'+text+''+chr(27));
      exit;
    end;


  ////
  /// <summary>
  ///   Sets up display margins for the <c>consoleOut</c> function.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="x1">
  ///   the distance from left of layer to the left margin, in pixels
  /// </param>
  /// <param name="y1">
  ///   the distance from top of layer to the top margin, in pixels
  /// </param>
  /// <param name="x2">
  ///   the distance from left of layer to the right margin, in pixels
  /// </param>
  /// <param name="y2">
  ///   the distance from top of layer to the bottom margin, in pixels
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.setConsoleMargins(x1: LongInt; y1: LongInt; x2: LongInt; y2: LongInt):LongInt;
    begin
      result := self.command_push('m'+inttostr(x1)+','+inttostr(y1)+','+inttostr(x2)+','+inttostr(y2));
      exit;
    end;


  ////
  /// <summary>
  ///   Sets up the background color used by the <c>clearConsole</c> function and by
  ///   the console scrolling feature.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="bgcol">
  ///   the background gray level to use when scrolling (0 = black,
  ///   255 = white), or -1 for transparent
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.setConsoleBackground(bgcol: LongInt):LongInt;
    begin
      result := self.command_push('b'+inttostr(bgcol));
      exit;
    end;


  ////
  /// <summary>
  ///   Sets up the wrapping behaviour used by the <c>consoleOut</c> function.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="wordwrap">
  ///   <c>true</c> to wrap only between words,
  ///   <c>false</c> to wrap on the last column anyway.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.setConsoleWordWrap(wordwrap: boolean):LongInt;
    begin
      result := self.command_push('w'+_yapiBoolToStr(wordwrap));
      exit;
    end;


  ////
  /// <summary>
  ///   Blanks the console area within console margins, and resets the console pointer
  ///   to the upper left corner of the console.
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.clearConsole():LongInt;
    begin
      result := self.command_flush('^');
      exit;
    end;


  ////
  /// <summary>
  ///   Sets the position of the layer relative to the display upper left corner.
  /// <para>
  ///   When smooth scrolling is used, the display offset of the layer is
  ///   automatically updated during the next milliseconds to animate the move of the layer.
  /// </para>
  /// </summary>
  /// <param name="x">
  ///   the distance from left of display to the upper left corner of the layer
  /// </param>
  /// <param name="y">
  ///   the distance from top of display to the upper left corner of the layer
  /// </param>
  /// <param name="scrollTime">
  ///   number of milliseconds to use for smooth scrolling, or
  ///   0 if the scrolling should be immediate.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.setLayerPosition(x: LongInt; y: LongInt; scrollTime: LongInt):LongInt;
    begin
      result := self.command_flush('#'+inttostr(x)+','+inttostr(y)+','+inttostr(scrollTime));
      exit;
    end;


  ////
  /// <summary>
  ///   Hides the layer.
  /// <para>
  ///   The state of the layer is perserved but the layer is not displayed
  ///   on the screen until the next call to <c>unhide()</c>. Hiding the layer can positively
  ///   affect the drawing speed, since it postpones the rendering until all operations are
  ///   completed (double-buffering).
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.hide():LongInt;
    begin
      self.command_push('h');
      self._hidden := true;
      result := self.flush_now;
      exit;
    end;


  ////
  /// <summary>
  ///   Shows the layer.
  /// <para>
  ///   Shows the layer again after a hide command.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDisplayLayer.unhide():LongInt;
    begin
      self._hidden := false;
      result := self.command_flush('s');
      exit;
    end;


  ////
  /// <summary>
  ///   Gets parent YDisplay.
  /// <para>
  ///   Returns the parent YDisplay object of the current YDisplayLayer.
  /// </para>
  /// </summary>
  /// <returns>
  ///   an <c>YDisplay</c> object
  /// </returns>
  ///-
  function TYDisplayLayer.get_display():TYDisplay;
    begin
      result := self._display;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the display width, in pixels.
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the display width, in pixels
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DISPLAYWIDTH_INVALID.
  /// </para>
  ///-
  function TYDisplayLayer.get_displayWidth():LongInt;
    begin
      result := self._display.get_displayWidth();
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the display height, in pixels.
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the display height, in pixels
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DISPLAYHEIGHT_INVALID.
  /// </para>
  ///-
  function TYDisplayLayer.get_displayHeight():LongInt;
    begin
      result := self._display.get_displayHeight();
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the width of the layers to draw on, in pixels.
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the width of the layers to draw on, in pixels
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LAYERWIDTH_INVALID.
  /// </para>
  ///-
  function TYDisplayLayer.get_layerWidth():LongInt;
    begin
      result := self._display.get_layerWidth();
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the height of the layers to draw on, in pixels.
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the height of the layers to draw on, in pixels
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LAYERHEIGHT_INVALID.
  /// </para>
  ///-
  function TYDisplayLayer.get_layerHeight():LongInt;
    begin
      result := self._display.get_layerHeight();
      exit;
    end;


  function TYDisplayLayer.resetHiddenFlag():LongInt;
    begin
      self._hidden := false;
      result := YAPI_SUCCESS;
      exit;
    end;


//--- (end of generated code: YDisplayLayer implementation)

 //--- (generated code: DisplayLayer functions)

  procedure _DisplayLayerCleanup();
    begin
    end;

//--- (end of generated code: DisplayLayer functions)



initialization
   //--- (generated code: Display initialization)
  //--- (end of generated code: Display initialization)

finalization
   //--- (generated code: Display cleanup)
  _DisplayCleanup();
  //--- (end of generated code: Display cleanup)
end.
