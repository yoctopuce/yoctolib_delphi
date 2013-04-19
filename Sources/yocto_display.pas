{*********************************************************************
 *
 * $Id: pic24config.php 8610 2012-11-07 08:54:50Z mvuilleu $
 *
 * Implements yFindDisplay(), the high-level API for Display functions
 *
 * - - - - - - - - - License information: - - - - - - - - -
 *
 * Copyright (C) 2011 and beyond by Yoctopuce Sarl, Switzerland.
 *
 * 1) If you have obtained this file from www.yoctopuce.com,
 *    Yoctopuce Sarl licenses to you (hereafter Licensee) the
 *    right to use, modify, copy, and integrate this source file
 *    into your own solution for the sole purpose of interfacing
 *    a Yoctopuce product with Licensee's solution.
 *
 *    The use of this file and all relationship between Yoctopuce 
 *    and Licensee are governed by Yoctopuce General Terms and
 *    Conditions.
 *
 *    THE SOFTWARE AND DOCUMENTATION ARE PROVIDED 'AS IS' WITHOUT
 *    WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING 
 *    WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS 
 *    FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO
 *    EVENT SHALL LICENSOR BE LIABLE FOR ANY INCIDENTAL, SPECIAL,
 *    INDIRECT OR CONSEQUENTIAL DAMAGES, LOST PROFITS OR LOST DATA, 
 *    COST OF PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR 
 *    SERVICES, ANY CLAIMS BY THIRD PARTIES (INCLUDING BUT NOT 
 *    LIMITED TO ANY DEFENSE THEREOF), ANY CLAIMS FOR INDEMNITY OR
 *    CONTRIBUTION, OR OTHER SIMILAR COSTS, WHETHER ASSERTED ON THE
 *    BASIS OF CONTRACT, TORT (INCLUDING NEGLIGENCE), BREACH OF
 *    WARRANTY, OR OTHERWISE.
 *
 * 2) If your intent is not to interface with Yoctopuce products,
 *    you are not entitled to use, read or create any derived
 *    material from this source file.
 *
 *********************************************************************}


unit yocto_display;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (generated code: YDisplay definitions)

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_POWERSTATE_OFF = 0;
   Y_POWERSTATE_ON = 1;
   Y_POWERSTATE_INVALID = -1;

   Y_STARTUPSEQ_INVALID            = YAPI_INVALID_STRING;
   Y_BRIGHTNESS_INVALID            = -1;
   Y_ORIENTATION_LEFT = 0;
   Y_ORIENTATION_UP = 1;
   Y_ORIENTATION_RIGHT = 2;
   Y_ORIENTATION_DOWN = 3;
   Y_ORIENTATION_INVALID = -1;

   Y_DISPLAYWIDTH_INVALID          = YAPI_INVALID_LONGWORD;
   Y_DISPLAYHEIGHT_INVALID         = YAPI_INVALID_LONGWORD;
   Y_DISPLAYTYPE_MONO = 0;
   Y_DISPLAYTYPE_GRAY = 1;
   Y_DISPLAYTYPE_RGB = 2;
   Y_DISPLAYTYPE_INVALID = -1;

   Y_LAYERWIDTH_INVALID            = YAPI_INVALID_LONGWORD;
   Y_LAYERHEIGHT_INVALID           = YAPI_INVALID_LONGWORD;
   Y_LAYERCOUNT_INVALID            = YAPI_INVALID_LONGWORD;
   Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of generated code: YDisplay definitions)

//--- (generated code: YDisplayLayer definitions)
 type   TYALIGN = (Y_ALIGN_TOP_LEFT,Y_ALIGN_CENTER_LEFT,Y_ALIGN_BASELINE_LEFT,Y_ALIGN_BOTTOM_LEFT,Y_ALIGN_TOP_CENTER,Y_ALIGN_CENTER,Y_ALIGN_BASELINE_CENTER,Y_ALIGN_BOTTOM_CENTER,Y_ALIGN_TOP_DECIMAL,Y_ALIGN_CENTER_DECIMAL,Y_ALIGN_BASELINE_DECIMAL,Y_ALIGN_BOTTOM_DECIMAL,Y_ALIGN_TOP_RIGHT,Y_ALIGN_CENTER_RIGHT,Y_ALIGN_BASELINE_RIGHT,Y_ALIGN_BOTTOM_RIGHT);



//--- (end of generated code: YDisplayLayer definitions)

type
 TYDisplayLayer = class;

//--- (generated code: YDisplay declaration)
 TYDisplay = class;
 TUpdateCallback  = procedure(func: TYDisplay; value:string);
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
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _powerState               : Integer;
   _startupSeq               : string;
   _brightness               : LongInt;
   _orientation              : Integer;
   _displayWidth             : LongWord;
   _displayHeight            : LongWord;
   _displayType              : Integer;
   _layerWidth               : LongWord;
   _layerHeight              : LongWord;
   _layerCount               : LongWord;
   _command                  : string;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of generated code: YDisplay declaration)

private
   _allDisplayLayers :  array of TYDisplayLayer;
   _recording : boolean;
   _sequence : string;

public
   constructor Create(func:string);
   destructor Destroy(); override;
   function get_displayLayer(layerId:integer):TYDisplayLayer;
   function sendCommand(cmd:string):integer;

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

  function  flushLayers() :integer;
  procedure resetHiddenLayerFlags();

   //--- (generated code: YDisplay accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the display.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the display
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the display.
   /// <para>
   ///   You can use <c>yCheckLogicalName()</c>
   ///   prior to this call to make sure that your parameter is valid.
   ///   Remember to call the <c>saveToFlash()</c> method of the module if the
   ///   modification must be kept.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   a string corresponding to the logical name of the display
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
   function set_logicalName(newval:string):integer;

   ////
   /// <summary>
   ///   Returns the current value of the display (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the display (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the power state of the display.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_POWERSTATE_OFF</c> or <c>Y_POWERSTATE_ON</c>, according to the power state of the display
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_POWERSTATE_INVALID</c>.
   /// </para>
   ///-
   function get_powerState():Integer;

   ////
   /// <summary>
   ///   Changes the power state of the display.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   either <c>Y_POWERSTATE_OFF</c> or <c>Y_POWERSTATE_ON</c>, according to the power state of the display
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
   function set_powerState(newval:Integer):integer;

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
   function get_displayWidth():LongWord;

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
   function get_displayHeight():LongWord;

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
   function get_layerWidth():LongWord;

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
   function get_layerHeight():LongWord;

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
   function get_layerCount():LongWord;

   function get_command():string;

   function set_command(newval:string):integer;

   ////
   /// <summary>
   ///   Clears the display screen and resets all display layers to their default state.
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
   function resetAll():integer;

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
   function fade(brightness:integer; duration:integer):integer;

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
   function newSequence():integer;

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
   function saveSequence(sequenceName:string):integer;

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
   function playSequence(sequenceName:string):integer;

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
   function pauseSequence(delay_ms:integer):integer;

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
   function stopSequence():integer;

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
   function upload(pathname:string; content:TBYTEARRAY):integer;

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
   function copyLayerContent(srcLayerId:integer; dstLayerId:integer):integer;

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
   function swapLayerContent(layerIdA:integer; layerIdB:integer):integer;

   //--- (end of generated code: YDisplay accessors declaration)
end;

 // from YDisplayLayer
//--- ( generated code: YDisplayLayer declaration)


////
/// <summary>
///   TYDisplayLayer Class: Image layer containing data to display
/// <para>
///   A DisplayLayer is an image layer containing objects to display
///   (bitmaps, text, etc.). The content will only be displayed when
///   the layer is active on the screen (and not masked by other
///   overlapping layers).
/// </para>
/// </summary>
///-
TYDisplayLayer=class(TOBJECT)
protected
   // Attributes (function value cache)
   // Function-specific method for reading JSON output and caching result

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
   function reset():integer;

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
   function clear():integer;

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
   function selectColorPen(color:integer):integer;

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
   function selectGrayPen(graylevel:integer):integer;

   ////
   /// <summary>
   ///   Selects an eraser instead of a pen for all subsequent drawing functions,
   ///   except for text drawing and bitmap copy functions.
   /// <para>
   ///   Any point drawn
   ///   using the eraser becomes transparent (as when the layer is empty),
   ///   showing the other layers beneath it.
   /// </para>
   /// </summary>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function selectEraser():integer;

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
   function setAntialiasingMode(mode:boolean):integer;

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
   function drawPixel(x:integer; y:integer):integer;

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
   function drawRect(x1:integer; y1:integer; x2:integer; y2:integer):integer;

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
   function drawBar(x1:integer; y1:integer; x2:integer; y2:integer):integer;

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
   function drawCircle(x:integer; y:integer; r:integer):integer;

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
   function drawDisc(x:integer; y:integer; r:integer):integer;

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
   function selectFont(fontname:string):integer;

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
   ///   the distance from left of layer to the text ancor point, in pixels
   /// </param>
   /// <param name="y">
   ///   the distance from top of layer to the text ancor point, in pixels
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
   function drawText(x:integer; y:integer; anchor:TYALIGN; text:string):integer;

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
   function drawImage(x:integer; y:integer; imagename:string):integer;

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
   function drawBitmap(x:integer; y:integer; w:integer; bitmap:TBYTEARRAY; bgcol:integer):integer;

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
   function moveTo(x:integer; y:integer):integer;

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
   function lineTo(x:integer; y:integer):integer;

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
   function consoleOut(text:string):integer;

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
   function setConsoleMargins(x1:integer; y1:integer; x2:integer; y2:integer):integer;

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
   function setConsoleBackground(bgcol:integer):integer;

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
   function setConsoleWordWrap(wordwrap:boolean):integer;

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
   function clearConsole():integer;

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
   function setLayerPosition(x:integer; y:integer; scrollTime:integer):integer;

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
   function hide():integer;

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
   function unhide():integer;

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
   function get_display():TYDisplay;

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
   function get_displayWidth():integer;

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
   function get_displayHeight():integer;

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
   function get_layerWidth():integer;

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
   function get_layerHeight():integer;

   function resetHiddenFlag():integer;

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



//--- (generated code: YDisplay implementation)

var
   _DisplayCache : TStringList;

constructor TYDisplay.Create(func:string);
 begin
   inherited Create('Display', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _powerState := Y_POWERSTATE_INVALID;
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
 end;

{$HINTS OFF}
function TYDisplay._parse(j:PJSONRECORD):integer;
 var
   member,sub : PJSONRECORD;
   i,l        : integer;
 begin
   if (j^.recordtype <> JSON_STRUCT) then begin _parse:= -1; exit; end;
   for i:=0 to j^.membercount-1 do
    begin
      member := j^.members[i];
      if (member^.name = 'logicalName') then
       begin
         _logicalName := string(member^.svalue);
       end else
      if (member^.name = 'advertisedValue') then
       begin
         _advertisedValue := string(member^.svalue);
       end else
      if (member^.name = 'powerState') then
       begin
         _powerState := member^.ivalue;
       end else
      if (member^.name = 'startupSeq') then
       begin
         _startupSeq := string(member^.svalue);
       end else
      if (member^.name = 'brightness') then
       begin
         _brightness := member^.ivalue;
       end else
      if (member^.name = 'orientation') then
       begin
         _orientation := member^.ivalue;
       end else
      if (member^.name = 'displayWidth') then
       begin
         _displayWidth := member^.ivalue;
       end else
      if (member^.name = 'displayHeight') then
       begin
         _displayHeight := member^.ivalue;
       end else
      if (member^.name = 'displayType') then
       begin
         _displayType := member^.ivalue;
       end else
      if (member^.name = 'layerWidth') then
       begin
         _layerWidth := member^.ivalue;
       end else
      if (member^.name = 'layerHeight') then
       begin
         _layerHeight := member^.ivalue;
       end else
      if (member^.name = 'layerCount') then
       begin
         _layerCount := member^.ivalue;
       end else
      if (member^.name = 'command') then
       begin
         _command := string(member^.svalue);
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the display.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the display
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYDisplay.get_logicalName():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LOGICALNAME_INVALID;
         exit;
       end;
   result := _logicalName;
 end;

////
/// <summary>
///   Changes the logical name of the display.
/// <para>
///   You can use yCheckLogicalName()
///   prior to this call to make sure that your parameter is valid.
///   Remember to call the saveToFlash() method of the module if the
///   modification must be kept.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   a string corresponding to the logical name of the display
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
function TYDisplay.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the display (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the display (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYDisplay.get_advertisedValue():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_ADVERTISEDVALUE_INVALID;
         exit;
       end;
   result := _advertisedValue;
 end;

////
/// <summary>
///   Returns the power state of the display.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   either Y_POWERSTATE_OFF or Y_POWERSTATE_ON, according to the power state of the display
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_POWERSTATE_INVALID.
/// </para>
///-
function TYDisplay.get_powerState():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_POWERSTATE_INVALID;
         exit;
       end;
   result := _powerState;
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
///   either Y_POWERSTATE_OFF or Y_POWERSTATE_ON, according to the power state of the display
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
function TYDisplay.set_powerState(newval:Integer):integer;
 var
   rest_val: string;
 begin
   if(newval>0) then rest_val := '1' else rest_val := '0';
   result := _setAttr('powerState',rest_val);
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_STARTUPSEQ_INVALID;
         exit;
       end;
   result := _startupSeq;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_BRIGHTNESS_INVALID;
         exit;
       end;
   result := _brightness;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_ORIENTATION_INVALID;
         exit;
       end;
   result := _orientation;
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
function TYDisplay.get_displayWidth():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_DISPLAYWIDTH_INVALID;
         exit;
       end;
   result := _displayWidth;
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
function TYDisplay.get_displayHeight():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_DISPLAYHEIGHT_INVALID;
         exit;
       end;
   result := _displayHeight;
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
   if (_displayType = Y_DISPLAYTYPE_INVALID) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_DISPLAYTYPE_INVALID;
         exit;
       end;
   result := _displayType;
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
function TYDisplay.get_layerWidth():LongWord;
 begin
   if (_layerWidth = Y_LAYERWIDTH_INVALID) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LAYERWIDTH_INVALID;
         exit;
       end;
   result := _layerWidth;
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
function TYDisplay.get_layerHeight():LongWord;
 begin
   if (_layerHeight = Y_LAYERHEIGHT_INVALID) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LAYERHEIGHT_INVALID;
         exit;
       end;
   result := _layerHeight;
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
function TYDisplay.get_layerCount():LongWord;
 begin
   if (_layerCount = Y_LAYERCOUNT_INVALID) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LAYERCOUNT_INVALID;
         exit;
       end;
   result := _layerCount;
 end;

function TYDisplay.get_command():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_COMMAND_INVALID;
         exit;
       end;
   result := _command;
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
///   Clears the display screen and resets all display layers to their default state.
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
function TYDisplay.resetAll():integer;
     begin
        self.flushLayers(); 
        self.resetHiddenLayerFlags();
        result:= self.sendCommand('Z'); 
            
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
function TYDisplay.fade(brightness:integer; duration:integer):integer;
     begin
        self.flushLayers(); 
        result:= self.sendCommand('+'+inttostr(brightness)+','+inttostr(duration)); 
            
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
function TYDisplay.newSequence():integer;
     begin
        self.flushLayers();
        self._sequence := ''; 
        self._recording := TRUE; 
        result:= YAPI_SUCCESS; 
            
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
function TYDisplay.saveSequence(sequenceName:string):integer;
     begin
        self.flushLayers();
        self._recording := FALSE; 
        self._upload(sequenceName, ansistring(self._sequence));
        //We need to use YPRINTF("") for Objective-C 
        self._sequence := ''; 
        result:= YAPI_SUCCESS; 
            
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
function TYDisplay.playSequence(sequenceName:string):integer;
     begin
        self.flushLayers();
        result:= self.sendCommand('S'+sequenceName); 
            
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
function TYDisplay.pauseSequence(delay_ms:integer):integer;
     begin
        self.flushLayers(); 
        result:= self.sendCommand('W'+inttostr(delay_ms)); 
            
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
function TYDisplay.stopSequence():integer;
     begin
        self.flushLayers();
        result:= self.sendCommand('S'); 
            
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
function TYDisplay.upload(pathname:string; content:TBYTEARRAY):integer;
     begin
        result:= self._upload(pathname,content);
            
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
function TYDisplay.copyLayerContent(srcLayerId:integer; dstLayerId:integer):integer;
     begin
        self.flushLayers(); 
        result:= self.sendCommand('o'+inttostr(srcLayerId)+','+inttostr(dstLayerId)); 
            
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
function TYDisplay.swapLayerContent(layerIdA:integer; layerIdB:integer):integer;
     begin
        self.flushLayers(); 
        result:= self.sendCommand('E'+inttostr(layerIdA)+','+inttostr(layerIdB)); 
            
     end;


function TYDisplay.nextDisplay(): TYDisplay;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextDisplay := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextDisplay := nil;
      exit;
    end;
    nextDisplay := yFindDisplay(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYDisplay.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYDisplay.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYDisplay.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYDisplay.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

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
 var
   index: integer;
   res  : TYDisplay;
 begin
    if (_DisplayCache.Find(func, index)) then
     begin
       yFindDisplay := TYDisplay(_DisplayCache.objects[index]);
       exit;
     end;
   res := TYDisplay.Create(func);
   _DisplayCache.addObject(func, res);
   yFindDisplay := res;
 end;

function yFirstDisplay(): TYDisplay;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('Display', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstDisplay := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstDisplay := nil;
       exit;
    end;
   yFirstDisplay := yFindDisplay(serial+'.'+funcId);
 end;

procedure _DisplayCleanup();
  var i:integer;
begin
  for i:=0 to _DisplayCache.count-1 do 
    begin
     _DisplayCache.objects[i].free();
     _DisplayCache.objects[i]:=nil;
    end;
   _DisplayCache.free();
   _DisplayCache:=nil;
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


 function yapibooltostr(value:boolean):string ;
  begin
    if (value) then yapibooltostr:='1' else yapibooltostr:='0';
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
function TYDisplayLayer.reset():integer;
     begin
        self._hidden := FALSE; 
        result:= self.command_flush('X'); 
            
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
function TYDisplayLayer.clear():integer;
     begin
        result:= self.command_flush('x'); 
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
function TYDisplayLayer.selectColorPen(color:integer):integer;
     begin
        result:= self.command_push('c'+inttohex(color,06)); 
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
function TYDisplayLayer.selectGrayPen(graylevel:integer):integer;
     begin
        result:= self.command_push('g'+inttostr(graylevel)); 
     end;


////
/// <summary>
///   Selects an eraser instead of a pen for all subsequent drawing functions,
///   except for text drawing and bitmap copy functions.
/// <para>
///   Any point drawn
///   using the eraser becomes transparent (as when the layer is empty),
///   showing the other layers beneath it.
/// </para>
/// </summary>
/// <returns>
///   <c>YAPI_SUCCESS</c> if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYDisplayLayer.selectEraser():integer;
     begin
        result:= self.command_push('e'); 
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
function TYDisplayLayer.setAntialiasingMode(mode:boolean):integer;
     begin
        result:= self.command_push('a'+yapibooltostr(mode)); 
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
function TYDisplayLayer.drawPixel(x:integer; y:integer):integer;
     begin
        result:= self.command_flush('P'+inttostr(x)+','+inttostr(y)); 
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
function TYDisplayLayer.drawRect(x1:integer; y1:integer; x2:integer; y2:integer):integer;
     begin
        result:= self.command_flush('R'+inttostr(x1)+','+inttostr(y1)+','+inttostr(x2)+','+inttostr(y2)); 
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
function TYDisplayLayer.drawBar(x1:integer; y1:integer; x2:integer; y2:integer):integer;
     begin
        result:= self.command_flush('B'+inttostr(x1)+','+inttostr(y1)+','+inttostr(x2)+','+inttostr(y2)); 
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
function TYDisplayLayer.drawCircle(x:integer; y:integer; r:integer):integer;
     begin
        result:= self.command_flush('C'+inttostr(x)+','+inttostr(y)+','+inttostr(r)); 
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
function TYDisplayLayer.drawDisc(x:integer; y:integer; r:integer):integer;
     begin
        result:= self.command_flush('D'+inttostr(x)+','+inttostr(y)+','+inttostr(r)); 
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
function TYDisplayLayer.selectFont(fontname:string):integer;
     begin
        result:= self.command_push('&'+fontname+''#27); 
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
///   the distance from left of layer to the text ancor point, in pixels
/// </param>
/// <param name="y">
///   the distance from top of layer to the text ancor point, in pixels
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
function TYDisplayLayer.drawText(x:integer; y:integer; anchor:TYALIGN; text:string):integer;
     begin
        result:= self.command_flush('T'+inttostr(x)+','+inttostr(y)+','+inttostr(ord(anchor))+','+text+''#27); 
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
function TYDisplayLayer.drawImage(x:integer; y:integer; imagename:string):integer;
     begin
        result:= self.command_flush('*'+inttostr(x)+','+inttostr(y)+','+imagename+''#27);
            
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
function TYDisplayLayer.drawBitmap(x:integer; y:integer; w:integer; bitmap:TBYTEARRAY; bgcol:integer):integer;
     var
        destname : string;
     begin
        destname := 'layer'+inttostr(self._id)+':'+inttostr(w)+','+inttostr(bgcol)+'@'+inttostr(x)+','+inttostr(y);
        result:= self._display.upload(destname,bitmap);
            
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
function TYDisplayLayer.moveTo(x:integer; y:integer):integer;
     begin
        result:= self.command_push('@'+inttostr(x)+','+inttostr(y)); 
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
function TYDisplayLayer.lineTo(x:integer; y:integer):integer;
     begin
        result:= self.command_flush('-'+inttostr(x)+','+inttostr(y)); 
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
function TYDisplayLayer.consoleOut(text:string):integer;
     begin
        result:= self.command_flush('!'+text+''#27); 
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
function TYDisplayLayer.setConsoleMargins(x1:integer; y1:integer; x2:integer; y2:integer):integer;
     begin
        result:= self.command_push('m'+inttostr(x1)+','+inttostr(y1)+','+inttostr(x2)+','+inttostr(y2)); 
            
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
function TYDisplayLayer.setConsoleBackground(bgcol:integer):integer;
     begin
        result:= self.command_push('b'+inttostr(bgcol)); 
            
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
function TYDisplayLayer.setConsoleWordWrap(wordwrap:boolean):integer;
     begin
        result:= self.command_push('w'+yapibooltostr(wordwrap)); 
            
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
function TYDisplayLayer.clearConsole():integer;
     begin
        result:= self.command_flush('^'); 
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
function TYDisplayLayer.setLayerPosition(x:integer; y:integer; scrollTime:integer):integer;
     begin
        result:= self.command_flush('#'+inttostr(x)+','+inttostr(y)+','+inttostr(scrollTime)); 
            
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
function TYDisplayLayer.hide():integer;
     begin
        self.command_push('h'); 
        self._hidden := TRUE; 
        result:= self.flush_now(); 
            
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
function TYDisplayLayer.unhide():integer;
     begin
        self._hidden := FALSE; 
        result:= self.command_flush('s'); 
            
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
        result:= self._display; 
            
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
function TYDisplayLayer.get_displayWidth():integer;
     begin
        result:= self._display.get_displayWidth();
            
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
function TYDisplayLayer.get_displayHeight():integer;
     begin
        result:= self._display.get_displayHeight();
            
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
function TYDisplayLayer.get_layerWidth():integer;
     begin
        result:= self._display.get_layerWidth();
            
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
function TYDisplayLayer.get_layerHeight():integer;
     begin
        result:= self._display.get_layerHeight();
            
     end;


function TYDisplayLayer.resetHiddenFlag():integer;
     begin
        self._hidden := FALSE; 
        result:= YAPI_SUCCESS;
            
     end;



    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
//--- (end of generated code: YDisplayLayer implementation)

 //--- (generated code: YDisplayLayer functions)

 //--- (end of generated code: YDisplayLayer functions)



initialization
   //--- (generated code: Display initialization)
   _DisplayCache        := TstringList.create();
   _DisplayCache.sorted := true;
   //--- (end of generated code: Display initialization)

finalization
   //--- (generated code: Display cleanup)
   _DisplayCleanup();
   //--- (end of generated code: Display cleanup)
end.
