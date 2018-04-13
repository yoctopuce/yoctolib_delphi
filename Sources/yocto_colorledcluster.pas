{*********************************************************************
 *
 * $Id: yocto_colorledcluster.pas 30500 2018-04-04 07:53:46Z mvuilleu $
 *
 * Implements yFindColorLedCluster(), the high-level API for ColorLedCluster functions
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


unit yocto_colorledcluster;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YColorLedCluster definitions)

const Y_ACTIVELEDCOUNT_INVALID        = YAPI_INVALID_UINT;
const Y_LEDTYPE_RGB = 0;
const Y_LEDTYPE_RGBW = 1;
const Y_LEDTYPE_INVALID = -1;
const Y_MAXLEDCOUNT_INVALID           = YAPI_INVALID_UINT;
const Y_BLINKSEQMAXCOUNT_INVALID      = YAPI_INVALID_UINT;
const Y_BLINKSEQMAXSIZE_INVALID       = YAPI_INVALID_UINT;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of YColorLedCluster definitions)

type
  TYColorLedCluster = class;
  //--- (YColorLedCluster class start)
  TYColorLedClusterValueCallback = procedure(func: TYColorLedCluster; value:string);
  TYColorLedClusterTimedReportCallback = procedure(func: TYColorLedCluster; value:TYMeasure);

  ////
  /// <summary>
  ///   TYColorLedCluster Class: ColorLedCluster function interface
  /// <para>
  ///   The Yoctopuce application programming interface
  ///   allows you to drive a color LED cluster. Unlike the ColorLed class, the ColorLedCluster
  ///   allows to handle several LEDs at one. Color changes can be done   using RGB coordinates as well as
  ///   HSL coordinates.
  ///   The module performs all conversions form RGB to HSL automatically. It is then
  ///   self-evident to turn on a LED with a given hue and to progressively vary its
  ///   saturation or lightness. If needed, you can find more information on the
  ///   difference between RGB and HSL in the section following this one.
  /// </para>
  /// </summary>
  ///-
  TYColorLedCluster=class(TYFunction)
  //--- (end of YColorLedCluster class start)
  protected
  //--- (YColorLedCluster declaration)
    // Attributes (function value cache)
    _activeLedCount           : LongInt;
    _ledType                  : Integer;
    _maxLedCount              : LongInt;
    _blinkSeqMaxCount         : LongInt;
    _blinkSeqMaxSize          : LongInt;
    _command                  : string;
    _valueCallbackColorLedCluster : TYColorLedClusterValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YColorLedCluster declaration)

  public
    //--- (YColorLedCluster accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the number of LEDs currently handled by the device.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of LEDs currently handled by the device
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ACTIVELEDCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_activeLedCount():LongInt;

    ////
    /// <summary>
    ///   Changes the number of LEDs currently handled by the device.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the number of LEDs currently handled by the device
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
    function set_activeLedCount(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the RGB LED type currently handled by the device.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_LEDTYPE_RGB</c> or <c>Y_LEDTYPE_RGBW</c>, according to the RGB LED type currently
    ///   handled by the device
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LEDTYPE_INVALID</c>.
    /// </para>
    ///-
    function get_ledType():Integer;

    ////
    /// <summary>
    ///   Changes the RGB LED type currently handled by the device.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>Y_LEDTYPE_RGB</c> or <c>Y_LEDTYPE_RGBW</c>, according to the RGB LED type currently
    ///   handled by the device
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
    function set_ledType(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the maximum number of LEDs that the device can handle.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximum number of LEDs that the device can handle
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MAXLEDCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_maxLedCount():LongInt;

    ////
    /// <summary>
    ///   Returns the maximum number of sequences that the device can handle.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximum number of sequences that the device can handle
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BLINKSEQMAXCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_blinkSeqMaxCount():LongInt;

    ////
    /// <summary>
    ///   Returns the maximum length of sequences.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximum length of sequences
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BLINKSEQMAXSIZE_INVALID</c>.
    /// </para>
    ///-
    function get_blinkSeqMaxSize():LongInt;

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
    ///   Use the method <c>YColorLedCluster.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YColorLedCluster</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindColorLedCluster(func: string):TYColorLedCluster;

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
    function registerValueCallback(callback: TYColorLedClusterValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function sendCommand(command: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes the current color of consecutve LEDs in the cluster, using a RGB color.
    /// <para>
    ///   Encoding is done as follows: 0xRRGGBB.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first affected LED.
    /// </param>
    /// <param name="count">
    ///   affected LED count.
    /// </param>
    /// <param name="rgbValue">
    ///   new color.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_rgbColor(ledIndex: LongInt; count: LongInt; rgbValue: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes the  color at device startup of consecutve LEDs in the cluster, using a RGB color.
    /// <para>
    ///   Encoding is done as follows: 0xRRGGBB.
    ///   Don't forget to call <c>saveLedsConfigAtPowerOn()</c> to make sure the modification is saved in the
    ///   device flash memory.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first affected LED.
    /// </param>
    /// <param name="count">
    ///   affected LED count.
    /// </param>
    /// <param name="rgbValue">
    ///   new color.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_rgbColorAtPowerOn(ledIndex: LongInt; count: LongInt; rgbValue: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes the current color of consecutive LEDs in the cluster, using a HSL color.
    /// <para>
    ///   Encoding is done as follows: 0xHHSSLL.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first affected LED.
    /// </param>
    /// <param name="count">
    ///   affected LED count.
    /// </param>
    /// <param name="hslValue">
    ///   new color.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_hslColor(ledIndex: LongInt; count: LongInt; hslValue: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Allows you to modify the current color of a group of adjacent LEDs to another color, in a seamless and
    ///   autonomous manner.
    /// <para>
    ///   The transition is performed in the RGB space.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first affected LED.
    /// </param>
    /// <param name="count">
    ///   affected LED count.
    /// </param>
    /// <param name="rgbValue">
    ///   new color (0xRRGGBB).
    /// </param>
    /// <param name="delay">
    ///   transition duration in ms
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function rgb_move(ledIndex: LongInt; count: LongInt; rgbValue: LongInt; delay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Allows you to modify the current color of a group of adjacent LEDs  to another color, in a seamless and
    ///   autonomous manner.
    /// <para>
    ///   The transition is performed in the HSL space. In HSL, hue is a circular
    ///   value (0..360°). There are always two paths to perform the transition: by increasing
    ///   or by decreasing the hue. The module selects the shortest transition.
    ///   If the difference is exactly 180°, the module selects the transition which increases
    ///   the hue.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the fisrt affected LED.
    /// </param>
    /// <param name="count">
    ///   affected LED count.
    /// </param>
    /// <param name="hslValue">
    ///   new color (0xHHSSLL).
    /// </param>
    /// <param name="delay">
    ///   transition duration in ms
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function hsl_move(ledIndex: LongInt; count: LongInt; hslValue: LongInt; delay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Adds an RGB transition to a sequence.
    /// <para>
    ///   A sequence is a transition list, which can
    ///   be executed in loop by a group of LEDs.  Sequences are persistent and are saved
    ///   in the device flash memory as soon as the <c>saveBlinkSeq()</c> method is called.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   sequence index.
    /// </param>
    /// <param name="rgbValue">
    ///   target color (0xRRGGBB)
    /// </param>
    /// <param name="delay">
    ///   transition duration in ms
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function addRgbMoveToBlinkSeq(seqIndex: LongInt; rgbValue: LongInt; delay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Adds an HSL transition to a sequence.
    /// <para>
    ///   A sequence is a transition list, which can
    ///   be executed in loop by an group of LEDs.  Sequences are persistant and are saved
    ///   in the device flash memory as soon as the <c>saveBlinkSeq()</c> method is called.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   sequence index.
    /// </param>
    /// <param name="hslValue">
    ///   target color (0xHHSSLL)
    /// </param>
    /// <param name="delay">
    ///   transition duration in ms
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function addHslMoveToBlinkSeq(seqIndex: LongInt; hslValue: LongInt; delay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Adds a mirror ending to a sequence.
    /// <para>
    ///   When the sequence will reach the end of the last
    ///   transition, its running speed will automatically be reversed so that the sequence plays
    ///   in the reverse direction, like in a mirror. After the first transition of the sequence
    ///   is played at the end of the reverse execution, the sequence starts again in
    ///   the initial direction.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   sequence index.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function addMirrorToBlinkSeq(seqIndex: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Adds to a sequence a jump to another sequence.
    /// <para>
    ///   When a pixel will reach this jump,
    ///   it will be automatically relinked to the new sequence, and will run it starting
    ///   from the beginning.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   sequence index.
    /// </param>
    /// <param name="linkSeqIndex">
    ///   index of the sequence to chain.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function addJumpToBlinkSeq(seqIndex: LongInt; linkSeqIndex: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Adds a to a sequence a hard stop code.
    /// <para>
    ///   When a pixel will reach this stop code,
    ///   instead of restarting the sequence in a loop it will automatically be unlinked
    ///   from the sequence.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   sequence index.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function addUnlinkToBlinkSeq(seqIndex: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Links adjacent LEDs to a specific sequence.
    /// <para>
    ///   These LEDs start to execute
    ///   the sequence as soon as  startBlinkSeq is called. It is possible to add an offset
    ///   in the execution: that way we  can have several groups of LED executing the same
    ///   sequence, with a  temporal offset. A LED cannot be linked to more than one sequence.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first affected LED.
    /// </param>
    /// <param name="count">
    ///   affected LED count.
    /// </param>
    /// <param name="seqIndex">
    ///   sequence index.
    /// </param>
    /// <param name="offset">
    ///   execution offset in ms.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function linkLedToBlinkSeq(ledIndex: LongInt; count: LongInt; seqIndex: LongInt; offset: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Links adjacent LEDs to a specific sequence at device poweron.
    /// <para>
    ///   Don't forget to configure
    ///   the sequence auto start flag as well and call <c>saveLedsConfigAtPowerOn()</c>. It is possible to add an offset
    ///   in the execution: that way we  can have several groups of LEDs executing the same
    ///   sequence, with a  temporal offset. A LED cannot be linked to more than one sequence.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first affected LED.
    /// </param>
    /// <param name="count">
    ///   affected LED count.
    /// </param>
    /// <param name="seqIndex">
    ///   sequence index.
    /// </param>
    /// <param name="offset">
    ///   execution offset in ms.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function linkLedToBlinkSeqAtPowerOn(ledIndex: LongInt; count: LongInt; seqIndex: LongInt; offset: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Links adjacent LEDs to a specific sequence.
    /// <para>
    ///   These LED start to execute
    ///   the sequence as soon as  startBlinkSeq is called. This function automatically
    ///   introduces a shift between LEDs so that the specified number of sequence periods
    ///   appears on the group of LEDs (wave effect).
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first affected LED.
    /// </param>
    /// <param name="count">
    ///   affected LED count.
    /// </param>
    /// <param name="seqIndex">
    ///   sequence index.
    /// </param>
    /// <param name="periods">
    ///   number of periods to show on LEDs.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function linkLedToPeriodicBlinkSeq(ledIndex: LongInt; count: LongInt; seqIndex: LongInt; periods: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Unlinks adjacent LEDs from a  sequence.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first affected LED.
    /// </param>
    /// <param name="count">
    ///   affected LED count.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function unlinkLedFromBlinkSeq(ledIndex: LongInt; count: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Starts a sequence execution: every LED linked to that sequence starts to
    ///   run it in a loop.
    /// <para>
    ///   Note that a sequence with a zero duration can't be started.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   index of the sequence to start.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function startBlinkSeq(seqIndex: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops a sequence execution.
    /// <para>
    ///   If started again, the execution
    ///   restarts from the beginning.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   index of the sequence to stop.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function stopBlinkSeq(seqIndex: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops a sequence execution and resets its contents.
    /// <para>
    ///   Leds linked to this
    ///   sequence are not automatically updated anymore.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   index of the sequence to reset
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function resetBlinkSeq(seqIndex: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Configures a sequence to make it start automatically at device
    ///   startup.
    /// <para>
    ///   Note that a sequence with a zero duration can't be started.
    ///   Don't forget to call <c>saveBlinkSeq()</c> to make sure the
    ///   modification is saved in the device flash memory.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   index of the sequence to reset.
    /// </param>
    /// <param name="autostart">
    ///   0 to keep the sequence turned off and 1 to start it automatically.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_blinkSeqStateAtPowerOn(seqIndex: LongInt; autostart: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes the execution speed of a sequence.
    /// <para>
    ///   The natural execution speed is 1000 per
    ///   thousand. If you configure a slower speed, you can play the sequence in slow-motion.
    ///   If you set a negative speed, you can play the sequence in reverse direction.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   index of the sequence to start.
    /// </param>
    /// <param name="speed">
    ///   sequence running speed (-1000...1000).
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_blinkSeqSpeed(seqIndex: LongInt; speed: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Saves the LEDs power-on configuration.
    /// <para>
    ///   This includes the start-up color or
    ///   sequence binding for all LEDs. Warning: if some LEDs are linked to a sequence, the
    ///   method <c>saveBlinkSeq()</c> must also be called to save the sequence definition.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function saveLedsConfigAtPowerOn():LongInt; overload; virtual;

    function saveLedsState():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Saves the definition of a sequence.
    /// <para>
    ///   Warning: only sequence steps and flags are saved.
    ///   to save the LEDs startup bindings, the method <c>saveLedsConfigAtPowerOn()</c>
    ///   must be called.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   index of the sequence to start.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function saveBlinkSeq(seqIndex: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a binary buffer to the LED RGB buffer, as is.
    /// <para>
    ///   First three bytes are RGB components for LED specified as parameter, the
    ///   next three bytes for the next LED, etc.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first LED which should be updated
    /// </param>
    /// <param name="buff">
    ///   the binary buffer to send
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_rgbColorBuffer(ledIndex: LongInt; buff: TByteArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends 24bit RGB colors (provided as a list of integers) to the LED RGB buffer, as is.
    /// <para>
    ///   The first number represents the RGB value of the LED specified as parameter, the second
    ///   number represents the RGB value of the next LED, etc.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first LED which should be updated
    /// </param>
    /// <param name="rgbList">
    ///   a list of 24bit RGB codes, in the form 0xRRGGBB
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_rgbColorArray(ledIndex: LongInt; rgbList: TLongIntArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets up a smooth RGB color transition to the specified pixel-by-pixel list of RGB
    ///   color codes.
    /// <para>
    ///   The first color code represents the target RGB value of the first LED,
    ///   the next color code represents the target value of the next LED, etc.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first LED which should be updated
    /// </param>
    /// <param name="rgbList">
    ///   a list of target 24bit RGB codes, in the form 0xRRGGBB
    /// </param>
    /// <param name="delay">
    ///   transition duration in ms
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function rgbArrayOfs_move(ledIndex: LongInt; rgbList: TLongIntArray; delay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets up a smooth RGB color transition to the specified pixel-by-pixel list of RGB
    ///   color codes.
    /// <para>
    ///   The first color code represents the target RGB value of the first LED,
    ///   the next color code represents the target value of the next LED, etc.
    /// </para>
    /// </summary>
    /// <param name="rgbList">
    ///   a list of target 24bit RGB codes, in the form 0xRRGGBB
    /// </param>
    /// <param name="delay">
    ///   transition duration in ms
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function rgbArray_move(rgbList: TLongIntArray; delay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a binary buffer to the LED HSL buffer, as is.
    /// <para>
    ///   First three bytes are HSL components for the LED specified as parameter, the
    ///   next three bytes for the second LED, etc.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first LED which should be updated
    /// </param>
    /// <param name="buff">
    ///   the binary buffer to send
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_hslColorBuffer(ledIndex: LongInt; buff: TByteArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends 24bit HSL colors (provided as a list of integers) to the LED HSL buffer, as is.
    /// <para>
    ///   The first number represents the HSL value of the LED specified as parameter, the second number represents
    ///   the HSL value of the second LED, etc.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first LED which should be updated
    /// </param>
    /// <param name="hslList">
    ///   a list of 24bit HSL codes, in the form 0xHHSSLL
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_hslColorArray(ledIndex: LongInt; hslList: TLongIntArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets up a smooth HSL color transition to the specified pixel-by-pixel list of HSL
    ///   color codes.
    /// <para>
    ///   The first color code represents the target HSL value of the first LED,
    ///   the second color code represents the target value of the second LED, etc.
    /// </para>
    /// </summary>
    /// <param name="hslList">
    ///   a list of target 24bit HSL codes, in the form 0xHHSSLL
    /// </param>
    /// <param name="delay">
    ///   transition duration in ms
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function hslArray_move(hslList: TLongIntArray; delay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sets up a smooth HSL color transition to the specified pixel-by-pixel list of HSL
    ///   color codes.
    /// <para>
    ///   The first color code represents the target HSL value of the first LED,
    ///   the second color code represents the target value of the second LED, etc.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first LED which should be updated
    /// </param>
    /// <param name="hslList">
    ///   a list of target 24bit HSL codes, in the form 0xHHSSLL
    /// </param>
    /// <param name="delay">
    ///   transition duration in ms
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function hslArrayOfs_move(ledIndex: LongInt; hslList: TLongIntArray; delay: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a binary buffer with content from the LED RGB buffer, as is.
    /// <para>
    ///   First three bytes are RGB components for the first LED in the interval,
    ///   the next three bytes for the second LED in the interval, etc.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first LED which should be returned
    /// </param>
    /// <param name="count">
    ///   number of LEDs which should be returned
    /// </param>
    /// <returns>
    ///   a binary buffer with RGB components of selected LEDs.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty binary buffer.
    /// </para>
    ///-
    function get_rgbColorBuffer(ledIndex: LongInt; count: LongInt):TByteArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list on 24bit RGB color values with the current colors displayed on
    ///   the RGB leds.
    /// <para>
    ///   The first number represents the RGB value of the first LED,
    ///   the second number represents the RGB value of the second LED, etc.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first LED which should be returned
    /// </param>
    /// <param name="count">
    ///   number of LEDs which should be returned
    /// </param>
    /// <returns>
    ///   a list of 24bit color codes with RGB components of selected LEDs, as 0xRRGGBB.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_rgbColorArray(ledIndex: LongInt; count: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list on 24bit RGB color values with the RGB LEDs startup colors.
    /// <para>
    ///   The first number represents the startup RGB value of the first LED,
    ///   the second number represents the RGB value of the second LED, etc.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first LED  which should be returned
    /// </param>
    /// <param name="count">
    ///   number of LEDs which should be returned
    /// </param>
    /// <returns>
    ///   a list of 24bit color codes with RGB components of selected LEDs, as 0xRRGGBB.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_rgbColorArrayAtPowerOn(ledIndex: LongInt; count: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list on sequence index for each RGB LED.
    /// <para>
    ///   The first number represents the
    ///   sequence index for the the first LED, the second number represents the sequence
    ///   index for the second LED, etc.
    /// </para>
    /// </summary>
    /// <param name="ledIndex">
    ///   index of the first LED which should be returned
    /// </param>
    /// <param name="count">
    ///   number of LEDs which should be returned
    /// </param>
    /// <returns>
    ///   a list of integers with sequence index
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_linkedSeqArray(ledIndex: LongInt; count: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list on 32 bit signatures for specified blinking sequences.
    /// <para>
    ///   Since blinking sequences cannot be read from the device, this can be used
    ///   to detect if a specific blinking sequence is already programmed.
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   index of the first blinking sequence which should be returned
    /// </param>
    /// <param name="count">
    ///   number of blinking sequences which should be returned
    /// </param>
    /// <returns>
    ///   a list of 32 bit integer signatures
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_blinkSeqSignatures(seqIndex: LongInt; count: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list of integers with the current speed for specified blinking sequences.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   index of the first sequence speed which should be returned
    /// </param>
    /// <param name="count">
    ///   number of sequence speeds which should be returned
    /// </param>
    /// <returns>
    ///   a list of integers, 0 for sequences turned off and 1 for sequences running
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_blinkSeqStateSpeed(seqIndex: LongInt; count: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list of integers with the "auto-start at power on" flag state for specified blinking sequences.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   index of the first blinking sequence which should be returned
    /// </param>
    /// <param name="count">
    ///   number of blinking sequences which should be returned
    /// </param>
    /// <returns>
    ///   a list of integers, 0 for sequences turned off and 1 for sequences running
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_blinkSeqStateAtPowerOn(seqIndex: LongInt; count: LongInt):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list of integers with the started state for specified blinking sequences.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="seqIndex">
    ///   index of the first blinking sequence which should be returned
    /// </param>
    /// <param name="count">
    ///   number of blinking sequences which should be returned
    /// </param>
    /// <returns>
    ///   a list of integers, 0 for sequences turned off and 1 for sequences running
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_blinkSeqState(seqIndex: LongInt; count: LongInt):TLongIntArray; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of RGB LED clusters started using <c>yFirstColorLedCluster()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YColorLedCluster</c> object, corresponding to
    ///   a RGB LED cluster currently online, or a <c>NIL</c> pointer
    ///   if there are no more RGB LED clusters to enumerate.
    /// </returns>
    ///-
    function nextColorLedCluster():TYColorLedCluster;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstColorLedCluster():TYColorLedCluster;
  //--- (end of YColorLedCluster accessors declaration)
  end;

//--- (YColorLedCluster functions declaration)
  ////
  /// <summary>
  ///   Retrieves a RGB LED cluster for a given identifier.
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
  ///   This function does not require that the RGB LED cluster is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YColorLedCluster.isOnline()</c> to test if the RGB LED cluster is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a RGB LED cluster by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the RGB LED cluster
  /// </param>
  /// <returns>
  ///   a <c>YColorLedCluster</c> object allowing you to drive the RGB LED cluster.
  /// </returns>
  ///-
  function yFindColorLedCluster(func:string):TYColorLedCluster;
  ////
  /// <summary>
  ///   Starts the enumeration of RGB LED clusters currently accessible.
  /// <para>
  ///   Use the method <c>YColorLedCluster.nextColorLedCluster()</c> to iterate on
  ///   next RGB LED clusters.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YColorLedCluster</c> object, corresponding to
  ///   the first RGB LED cluster currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstColorLedCluster():TYColorLedCluster;

//--- (end of YColorLedCluster functions declaration)

implementation
//--- (YColorLedCluster dlldef)
//--- (end of YColorLedCluster dlldef)

  constructor TYColorLedCluster.Create(func:string);
    begin
      inherited Create(func);
      _className := 'ColorLedCluster';
      //--- (YColorLedCluster accessors initialization)
      _activeLedCount := Y_ACTIVELEDCOUNT_INVALID;
      _ledType := Y_LEDTYPE_INVALID;
      _maxLedCount := Y_MAXLEDCOUNT_INVALID;
      _blinkSeqMaxCount := Y_BLINKSEQMAXCOUNT_INVALID;
      _blinkSeqMaxSize := Y_BLINKSEQMAXSIZE_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackColorLedCluster := nil;
      //--- (end of YColorLedCluster accessors initialization)
    end;


//--- (YColorLedCluster implementation)
{$HINTS OFF}
  function TYColorLedCluster._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'activeLedCount') then
        begin
          _activeLedCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'ledType') then
        begin
          _ledType := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'maxLedCount') then
        begin
          _maxLedCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'blinkSeqMaxCount') then
        begin
          _blinkSeqMaxCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'blinkSeqMaxSize') then
        begin
          _blinkSeqMaxSize := integer(member^.ivalue);
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
  ///   Returns the number of LEDs currently handled by the device.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the number of LEDs currently handled by the device
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ACTIVELEDCOUNT_INVALID.
  /// </para>
  ///-
  function TYColorLedCluster.get_activeLedCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ACTIVELEDCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._activeLedCount;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the number of LEDs currently handled by the device.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the number of LEDs currently handled by the device
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
  function TYColorLedCluster.set_activeLedCount(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('activeLedCount',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the RGB LED type currently handled by the device.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_LEDTYPE_RGB or Y_LEDTYPE_RGBW, according to the RGB LED type currently handled by the device
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LEDTYPE_INVALID.
  /// </para>
  ///-
  function TYColorLedCluster.get_ledType():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LEDTYPE_INVALID;
              exit;
            end;
        end;
      res := self._ledType;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the RGB LED type currently handled by the device.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   either Y_LEDTYPE_RGB or Y_LEDTYPE_RGBW, according to the RGB LED type currently handled by the device
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
  function TYColorLedCluster.set_ledType(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('ledType',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the maximum number of LEDs that the device can handle.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the maximum number of LEDs that the device can handle
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_MAXLEDCOUNT_INVALID.
  /// </para>
  ///-
  function TYColorLedCluster.get_maxLedCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MAXLEDCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._maxLedCount;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the maximum number of sequences that the device can handle.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the maximum number of sequences that the device can handle
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_BLINKSEQMAXCOUNT_INVALID.
  /// </para>
  ///-
  function TYColorLedCluster.get_blinkSeqMaxCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_BLINKSEQMAXCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._blinkSeqMaxCount;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the maximum length of sequences.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the maximum length of sequences
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_BLINKSEQMAXSIZE_INVALID.
  /// </para>
  ///-
  function TYColorLedCluster.get_blinkSeqMaxSize():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_BLINKSEQMAXSIZE_INVALID;
              exit;
            end;
        end;
      res := self._blinkSeqMaxSize;
      result := res;
      exit;
    end;


  function TYColorLedCluster.get_command():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_COMMAND_INVALID;
              exit;
            end;
        end;
      res := self._command;
      result := res;
      exit;
    end;


  function TYColorLedCluster.set_command(newval:string):integer;
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
  ///   Use the method <c>YColorLedCluster.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YColorLedCluster</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYColorLedCluster.FindColorLedCluster(func: string):TYColorLedCluster;
    var
      obj : TYColorLedCluster;
    begin
      obj := TYColorLedCluster(TYFunction._FindFromCache('ColorLedCluster', func));
      if obj = nil then
        begin
          obj :=  TYColorLedCluster.create(func);
          TYFunction._AddToCache('ColorLedCluster',  func, obj);
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
  function TYColorLedCluster.registerValueCallback(callback: TYColorLedClusterValueCallback):LongInt;
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
      self._valueCallbackColorLedCluster := callback;
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


  function TYColorLedCluster._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackColorLedCluster) <> nil) then
        begin
          self._valueCallbackColorLedCluster(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYColorLedCluster.sendCommand(command: string):LongInt;
    begin
      result := self.set_command(command);
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the current color of consecutve LEDs in the cluster, using a RGB color.
  /// <para>
  ///   Encoding is done as follows: 0xRRGGBB.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first affected LED.
  /// </param>
  /// <param name="count">
  ///   affected LED count.
  /// </param>
  /// <param name="rgbValue">
  ///   new color.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.set_rgbColor(ledIndex: LongInt; count: LongInt; rgbValue: LongInt):LongInt;
    begin
      result := self.sendCommand('SR'+inttostr(ledIndex)+','+inttostr(count)+','+inttohex(rgbValue,1));
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the  color at device startup of consecutve LEDs in the cluster, using a RGB color.
  /// <para>
  ///   Encoding is done as follows: 0xRRGGBB.
  ///   Don't forget to call <c>saveLedsConfigAtPowerOn()</c> to make sure the modification is saved in the
  ///   device flash memory.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first affected LED.
  /// </param>
  /// <param name="count">
  ///   affected LED count.
  /// </param>
  /// <param name="rgbValue">
  ///   new color.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.set_rgbColorAtPowerOn(ledIndex: LongInt; count: LongInt; rgbValue: LongInt):LongInt;
    begin
      result := self.sendCommand('SC'+inttostr(ledIndex)+','+inttostr(count)+','+inttohex(rgbValue,1));
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the current color of consecutive LEDs in the cluster, using a HSL color.
  /// <para>
  ///   Encoding is done as follows: 0xHHSSLL.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first affected LED.
  /// </param>
  /// <param name="count">
  ///   affected LED count.
  /// </param>
  /// <param name="hslValue">
  ///   new color.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.set_hslColor(ledIndex: LongInt; count: LongInt; hslValue: LongInt):LongInt;
    begin
      result := self.sendCommand('SH'+inttostr(ledIndex)+','+inttostr(count)+','+inttohex(hslValue,1));
      exit;
    end;


  ////
  /// <summary>
  ///   Allows you to modify the current color of a group of adjacent LEDs to another color, in a seamless and
  ///   autonomous manner.
  /// <para>
  ///   The transition is performed in the RGB space.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first affected LED.
  /// </param>
  /// <param name="count">
  ///   affected LED count.
  /// </param>
  /// <param name="rgbValue">
  ///   new color (0xRRGGBB).
  /// </param>
  /// <param name="delay">
  ///   transition duration in ms
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.rgb_move(ledIndex: LongInt; count: LongInt; rgbValue: LongInt; delay: LongInt):LongInt;
    begin
      result := self.sendCommand('MR'+inttostr(ledIndex)+','+inttostr(count)+','+inttohex(rgbValue,1)+','+inttostr(delay));
      exit;
    end;


  ////
  /// <summary>
  ///   Allows you to modify the current color of a group of adjacent LEDs  to another color, in a seamless and
  ///   autonomous manner.
  /// <para>
  ///   The transition is performed in the HSL space. In HSL, hue is a circular
  ///   value (0..360°). There are always two paths to perform the transition: by increasing
  ///   or by decreasing the hue. The module selects the shortest transition.
  ///   If the difference is exactly 180°, the module selects the transition which increases
  ///   the hue.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the fisrt affected LED.
  /// </param>
  /// <param name="count">
  ///   affected LED count.
  /// </param>
  /// <param name="hslValue">
  ///   new color (0xHHSSLL).
  /// </param>
  /// <param name="delay">
  ///   transition duration in ms
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.hsl_move(ledIndex: LongInt; count: LongInt; hslValue: LongInt; delay: LongInt):LongInt;
    begin
      result := self.sendCommand('MH'+inttostr(ledIndex)+','+inttostr(count)+','+inttohex(hslValue,1)+','+inttostr(delay));
      exit;
    end;


  ////
  /// <summary>
  ///   Adds an RGB transition to a sequence.
  /// <para>
  ///   A sequence is a transition list, which can
  ///   be executed in loop by a group of LEDs.  Sequences are persistent and are saved
  ///   in the device flash memory as soon as the <c>saveBlinkSeq()</c> method is called.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   sequence index.
  /// </param>
  /// <param name="rgbValue">
  ///   target color (0xRRGGBB)
  /// </param>
  /// <param name="delay">
  ///   transition duration in ms
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.addRgbMoveToBlinkSeq(seqIndex: LongInt; rgbValue: LongInt; delay: LongInt):LongInt;
    begin
      result := self.sendCommand('AR'+inttostr(seqIndex)+','+inttohex(rgbValue,1)+','+inttostr(delay));
      exit;
    end;


  ////
  /// <summary>
  ///   Adds an HSL transition to a sequence.
  /// <para>
  ///   A sequence is a transition list, which can
  ///   be executed in loop by an group of LEDs.  Sequences are persistant and are saved
  ///   in the device flash memory as soon as the <c>saveBlinkSeq()</c> method is called.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   sequence index.
  /// </param>
  /// <param name="hslValue">
  ///   target color (0xHHSSLL)
  /// </param>
  /// <param name="delay">
  ///   transition duration in ms
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.addHslMoveToBlinkSeq(seqIndex: LongInt; hslValue: LongInt; delay: LongInt):LongInt;
    begin
      result := self.sendCommand('AH'+inttostr(seqIndex)+','+inttohex(hslValue,1)+','+inttostr(delay));
      exit;
    end;


  ////
  /// <summary>
  ///   Adds a mirror ending to a sequence.
  /// <para>
  ///   When the sequence will reach the end of the last
  ///   transition, its running speed will automatically be reversed so that the sequence plays
  ///   in the reverse direction, like in a mirror. After the first transition of the sequence
  ///   is played at the end of the reverse execution, the sequence starts again in
  ///   the initial direction.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   sequence index.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.addMirrorToBlinkSeq(seqIndex: LongInt):LongInt;
    begin
      result := self.sendCommand('AC'+inttostr(seqIndex)+',0,0');
      exit;
    end;


  ////
  /// <summary>
  ///   Adds to a sequence a jump to another sequence.
  /// <para>
  ///   When a pixel will reach this jump,
  ///   it will be automatically relinked to the new sequence, and will run it starting
  ///   from the beginning.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   sequence index.
  /// </param>
  /// <param name="linkSeqIndex">
  ///   index of the sequence to chain.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.addJumpToBlinkSeq(seqIndex: LongInt; linkSeqIndex: LongInt):LongInt;
    begin
      result := self.sendCommand('AC'+inttostr(seqIndex)+',100,'+inttostr(linkSeqIndex)+',1000');
      exit;
    end;


  ////
  /// <summary>
  ///   Adds a to a sequence a hard stop code.
  /// <para>
  ///   When a pixel will reach this stop code,
  ///   instead of restarting the sequence in a loop it will automatically be unlinked
  ///   from the sequence.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   sequence index.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.addUnlinkToBlinkSeq(seqIndex: LongInt):LongInt;
    begin
      result := self.sendCommand('AC'+inttostr(seqIndex)+',100,-1,1000');
      exit;
    end;


  ////
  /// <summary>
  ///   Links adjacent LEDs to a specific sequence.
  /// <para>
  ///   These LEDs start to execute
  ///   the sequence as soon as  startBlinkSeq is called. It is possible to add an offset
  ///   in the execution: that way we  can have several groups of LED executing the same
  ///   sequence, with a  temporal offset. A LED cannot be linked to more than one sequence.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first affected LED.
  /// </param>
  /// <param name="count">
  ///   affected LED count.
  /// </param>
  /// <param name="seqIndex">
  ///   sequence index.
  /// </param>
  /// <param name="offset">
  ///   execution offset in ms.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.linkLedToBlinkSeq(ledIndex: LongInt; count: LongInt; seqIndex: LongInt; offset: LongInt):LongInt;
    begin
      result := self.sendCommand('LS'+inttostr(ledIndex)+','+inttostr(count)+','+inttostr(seqIndex)+','+inttostr(offset));
      exit;
    end;


  ////
  /// <summary>
  ///   Links adjacent LEDs to a specific sequence at device poweron.
  /// <para>
  ///   Don't forget to configure
  ///   the sequence auto start flag as well and call <c>saveLedsConfigAtPowerOn()</c>. It is possible to add an offset
  ///   in the execution: that way we  can have several groups of LEDs executing the same
  ///   sequence, with a  temporal offset. A LED cannot be linked to more than one sequence.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first affected LED.
  /// </param>
  /// <param name="count">
  ///   affected LED count.
  /// </param>
  /// <param name="seqIndex">
  ///   sequence index.
  /// </param>
  /// <param name="offset">
  ///   execution offset in ms.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.linkLedToBlinkSeqAtPowerOn(ledIndex: LongInt; count: LongInt; seqIndex: LongInt; offset: LongInt):LongInt;
    begin
      result := self.sendCommand('LO'+inttostr(ledIndex)+','+inttostr(count)+','+inttostr(seqIndex)+','+inttostr(offset));
      exit;
    end;


  ////
  /// <summary>
  ///   Links adjacent LEDs to a specific sequence.
  /// <para>
  ///   These LED start to execute
  ///   the sequence as soon as  startBlinkSeq is called. This function automatically
  ///   introduces a shift between LEDs so that the specified number of sequence periods
  ///   appears on the group of LEDs (wave effect).
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first affected LED.
  /// </param>
  /// <param name="count">
  ///   affected LED count.
  /// </param>
  /// <param name="seqIndex">
  ///   sequence index.
  /// </param>
  /// <param name="periods">
  ///   number of periods to show on LEDs.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.linkLedToPeriodicBlinkSeq(ledIndex: LongInt; count: LongInt; seqIndex: LongInt; periods: LongInt):LongInt;
    begin
      result := self.sendCommand('LP'+inttostr(ledIndex)+','+inttostr(count)+','+inttostr(seqIndex)+','+inttostr(periods));
      exit;
    end;


  ////
  /// <summary>
  ///   Unlinks adjacent LEDs from a  sequence.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first affected LED.
  /// </param>
  /// <param name="count">
  ///   affected LED count.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.unlinkLedFromBlinkSeq(ledIndex: LongInt; count: LongInt):LongInt;
    begin
      result := self.sendCommand('US'+inttostr(ledIndex)+','+inttostr(count));
      exit;
    end;


  ////
  /// <summary>
  ///   Starts a sequence execution: every LED linked to that sequence starts to
  ///   run it in a loop.
  /// <para>
  ///   Note that a sequence with a zero duration can't be started.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   index of the sequence to start.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.startBlinkSeq(seqIndex: LongInt):LongInt;
    begin
      result := self.sendCommand('SS'+inttostr(seqIndex));
      exit;
    end;


  ////
  /// <summary>
  ///   Stops a sequence execution.
  /// <para>
  ///   If started again, the execution
  ///   restarts from the beginning.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   index of the sequence to stop.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.stopBlinkSeq(seqIndex: LongInt):LongInt;
    begin
      result := self.sendCommand('XS'+inttostr(seqIndex));
      exit;
    end;


  ////
  /// <summary>
  ///   Stops a sequence execution and resets its contents.
  /// <para>
  ///   Leds linked to this
  ///   sequence are not automatically updated anymore.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   index of the sequence to reset
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.resetBlinkSeq(seqIndex: LongInt):LongInt;
    begin
      result := self.sendCommand('ZS'+inttostr(seqIndex));
      exit;
    end;


  ////
  /// <summary>
  ///   Configures a sequence to make it start automatically at device
  ///   startup.
  /// <para>
  ///   Note that a sequence with a zero duration can't be started.
  ///   Don't forget to call <c>saveBlinkSeq()</c> to make sure the
  ///   modification is saved in the device flash memory.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   index of the sequence to reset.
  /// </param>
  /// <param name="autostart">
  ///   0 to keep the sequence turned off and 1 to start it automatically.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.set_blinkSeqStateAtPowerOn(seqIndex: LongInt; autostart: LongInt):LongInt;
    begin
      result := self.sendCommand('AS'+inttostr(seqIndex)+','+inttostr(autostart));
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the execution speed of a sequence.
  /// <para>
  ///   The natural execution speed is 1000 per
  ///   thousand. If you configure a slower speed, you can play the sequence in slow-motion.
  ///   If you set a negative speed, you can play the sequence in reverse direction.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   index of the sequence to start.
  /// </param>
  /// <param name="speed">
  ///   sequence running speed (-1000...1000).
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.set_blinkSeqSpeed(seqIndex: LongInt; speed: LongInt):LongInt;
    begin
      result := self.sendCommand('CS'+inttostr(seqIndex)+','+inttostr(speed));
      exit;
    end;


  ////
  /// <summary>
  ///   Saves the LEDs power-on configuration.
  /// <para>
  ///   This includes the start-up color or
  ///   sequence binding for all LEDs. Warning: if some LEDs are linked to a sequence, the
  ///   method <c>saveBlinkSeq()</c> must also be called to save the sequence definition.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.saveLedsConfigAtPowerOn():LongInt;
    begin
      result := self.sendCommand('WL');
      exit;
    end;


  function TYColorLedCluster.saveLedsState():LongInt;
    begin
      result := self.sendCommand('WL');
      exit;
    end;


  ////
  /// <summary>
  ///   Saves the definition of a sequence.
  /// <para>
  ///   Warning: only sequence steps and flags are saved.
  ///   to save the LEDs startup bindings, the method <c>saveLedsConfigAtPowerOn()</c>
  ///   must be called.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   index of the sequence to start.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.saveBlinkSeq(seqIndex: LongInt):LongInt;
    begin
      result := self.sendCommand('WS'+inttostr(seqIndex));
      exit;
    end;


  ////
  /// <summary>
  ///   Sends a binary buffer to the LED RGB buffer, as is.
  /// <para>
  ///   First three bytes are RGB components for LED specified as parameter, the
  ///   next three bytes for the next LED, etc.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first LED which should be updated
  /// </param>
  /// <param name="buff">
  ///   the binary buffer to send
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.set_rgbColorBuffer(ledIndex: LongInt; buff: TByteArray):LongInt;
    begin
      result := self._upload('rgb:0:'+inttostr(ledIndex), buff);
      exit;
    end;


  ////
  /// <summary>
  ///   Sends 24bit RGB colors (provided as a list of integers) to the LED RGB buffer, as is.
  /// <para>
  ///   The first number represents the RGB value of the LED specified as parameter, the second
  ///   number represents the RGB value of the next LED, etc.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first LED which should be updated
  /// </param>
  /// <param name="rgbList">
  ///   a list of 24bit RGB codes, in the form 0xRRGGBB
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.set_rgbColorArray(ledIndex: LongInt; rgbList: TLongIntArray):LongInt;
    var
      listlen : LongInt;
      buff : TByteArray;
      idx : LongInt;
      rgb : LongInt;
      res : LongInt;
    begin
      listlen := length(rgbList);
      setlength(buff,3*listlen);
      idx := 0;
      while idx < listlen do
        begin
          rgb := rgbList[idx];
          buff[3*idx] := ((((rgb) shr 16)) and 255);
          buff[3*idx+1] := ((((rgb) shr 8)) and 255);
          buff[3*idx+2] := ((rgb) and 255);
          idx := idx + 1;
        end;

      res := self._upload('rgb:0:'+inttostr(ledIndex), buff);
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Sets up a smooth RGB color transition to the specified pixel-by-pixel list of RGB
  ///   color codes.
  /// <para>
  ///   The first color code represents the target RGB value of the first LED,
  ///   the next color code represents the target value of the next LED, etc.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first LED which should be updated
  /// </param>
  /// <param name="rgbList">
  ///   a list of target 24bit RGB codes, in the form 0xRRGGBB
  /// </param>
  /// <param name="delay">
  ///   transition duration in ms
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.rgbArrayOfs_move(ledIndex: LongInt; rgbList: TLongIntArray; delay: LongInt):LongInt;
    var
      listlen : LongInt;
      buff : TByteArray;
      idx : LongInt;
      rgb : LongInt;
      res : LongInt;
    begin
      listlen := length(rgbList);
      setlength(buff,3*listlen);
      idx := 0;
      while idx < listlen do
        begin
          rgb := rgbList[idx];
          buff[3*idx] := ((((rgb) shr 16)) and 255);
          buff[3*idx+1] := ((((rgb) shr 8)) and 255);
          buff[3*idx+2] := ((rgb) and 255);
          idx := idx + 1;
        end;

      res := self._upload('rgb:'+inttostr(delay)+':'+inttostr(ledIndex), buff);
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Sets up a smooth RGB color transition to the specified pixel-by-pixel list of RGB
  ///   color codes.
  /// <para>
  ///   The first color code represents the target RGB value of the first LED,
  ///   the next color code represents the target value of the next LED, etc.
  /// </para>
  /// </summary>
  /// <param name="rgbList">
  ///   a list of target 24bit RGB codes, in the form 0xRRGGBB
  /// </param>
  /// <param name="delay">
  ///   transition duration in ms
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.rgbArray_move(rgbList: TLongIntArray; delay: LongInt):LongInt;
    var
      res : LongInt;
    begin
      res := self.rgbArrayOfs_move(0, rgbList, delay);
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Sends a binary buffer to the LED HSL buffer, as is.
  /// <para>
  ///   First three bytes are HSL components for the LED specified as parameter, the
  ///   next three bytes for the second LED, etc.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first LED which should be updated
  /// </param>
  /// <param name="buff">
  ///   the binary buffer to send
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.set_hslColorBuffer(ledIndex: LongInt; buff: TByteArray):LongInt;
    begin
      result := self._upload('hsl:0:'+inttostr(ledIndex), buff);
      exit;
    end;


  ////
  /// <summary>
  ///   Sends 24bit HSL colors (provided as a list of integers) to the LED HSL buffer, as is.
  /// <para>
  ///   The first number represents the HSL value of the LED specified as parameter, the second number represents
  ///   the HSL value of the second LED, etc.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first LED which should be updated
  /// </param>
  /// <param name="hslList">
  ///   a list of 24bit HSL codes, in the form 0xHHSSLL
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.set_hslColorArray(ledIndex: LongInt; hslList: TLongIntArray):LongInt;
    var
      listlen : LongInt;
      buff : TByteArray;
      idx : LongInt;
      hsl : LongInt;
      res : LongInt;
    begin
      listlen := length(hslList);
      setlength(buff,3*listlen);
      idx := 0;
      while idx < listlen do
        begin
          hsl := hslList[idx];
          buff[3*idx] := ((((hsl) shr 16)) and 255);
          buff[3*idx+1] := ((((hsl) shr 8)) and 255);
          buff[3*idx+2] := ((hsl) and 255);
          idx := idx + 1;
        end;

      res := self._upload('hsl:0:'+inttostr(ledIndex), buff);
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Sets up a smooth HSL color transition to the specified pixel-by-pixel list of HSL
  ///   color codes.
  /// <para>
  ///   The first color code represents the target HSL value of the first LED,
  ///   the second color code represents the target value of the second LED, etc.
  /// </para>
  /// </summary>
  /// <param name="hslList">
  ///   a list of target 24bit HSL codes, in the form 0xHHSSLL
  /// </param>
  /// <param name="delay">
  ///   transition duration in ms
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.hslArray_move(hslList: TLongIntArray; delay: LongInt):LongInt;
    var
      res : LongInt;
    begin
      res := self.hslArrayOfs_move(0, hslList, delay);
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Sets up a smooth HSL color transition to the specified pixel-by-pixel list of HSL
  ///   color codes.
  /// <para>
  ///   The first color code represents the target HSL value of the first LED,
  ///   the second color code represents the target value of the second LED, etc.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first LED which should be updated
  /// </param>
  /// <param name="hslList">
  ///   a list of target 24bit HSL codes, in the form 0xHHSSLL
  /// </param>
  /// <param name="delay">
  ///   transition duration in ms
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYColorLedCluster.hslArrayOfs_move(ledIndex: LongInt; hslList: TLongIntArray; delay: LongInt):LongInt;
    var
      listlen : LongInt;
      buff : TByteArray;
      idx : LongInt;
      hsl : LongInt;
      res : LongInt;
    begin
      listlen := length(hslList);
      setlength(buff,3*listlen);
      idx := 0;
      while idx < listlen do
        begin
          hsl := hslList[idx];
          buff[3*idx] := ((((hsl) shr 16)) and 255);
          buff[3*idx+1] := ((((hsl) shr 8)) and 255);
          buff[3*idx+2] := ((hsl) and 255);
          idx := idx + 1;
        end;

      res := self._upload('hsl:'+inttostr(delay)+':'+inttostr(ledIndex), buff);
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a binary buffer with content from the LED RGB buffer, as is.
  /// <para>
  ///   First three bytes are RGB components for the first LED in the interval,
  ///   the next three bytes for the second LED in the interval, etc.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first LED which should be returned
  /// </param>
  /// <param name="count">
  ///   number of LEDs which should be returned
  /// </param>
  /// <returns>
  ///   a binary buffer with RGB components of selected LEDs.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty binary buffer.
  /// </para>
  ///-
  function TYColorLedCluster.get_rgbColorBuffer(ledIndex: LongInt; count: LongInt):TByteArray;
    begin
      result := self._download('rgb.bin?typ=0&pos='+inttostr(3*ledIndex)+'&len='+inttostr(3*count));
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a list on 24bit RGB color values with the current colors displayed on
  ///   the RGB leds.
  /// <para>
  ///   The first number represents the RGB value of the first LED,
  ///   the second number represents the RGB value of the second LED, etc.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first LED which should be returned
  /// </param>
  /// <param name="count">
  ///   number of LEDs which should be returned
  /// </param>
  /// <returns>
  ///   a list of 24bit color codes with RGB components of selected LEDs, as 0xRRGGBB.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty array.
  /// </para>
  ///-
  function TYColorLedCluster.get_rgbColorArray(ledIndex: LongInt; count: LongInt):TLongIntArray;
    var
      buff : TByteArray;
      res : TLongIntArray;
      idx : LongInt;
      r : LongInt;
      g : LongInt;
      b : LongInt;
      res_pos : LongInt;
    begin
      buff := self._download('rgb.bin?typ=0&pos='+inttostr(3*ledIndex)+'&len='+inttostr(3*count));
      SetLength(res, 0);
      res_pos := length(res);
      SetLength(res, res_pos+count);;
      idx := 0;
      while idx < count do
        begin
          r := buff[3*idx];
          g := buff[3*idx+1];
          b := buff[3*idx+2];
          res[res_pos] := r*65536+g*256+b;
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a list on 24bit RGB color values with the RGB LEDs startup colors.
  /// <para>
  ///   The first number represents the startup RGB value of the first LED,
  ///   the second number represents the RGB value of the second LED, etc.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first LED  which should be returned
  /// </param>
  /// <param name="count">
  ///   number of LEDs which should be returned
  /// </param>
  /// <returns>
  ///   a list of 24bit color codes with RGB components of selected LEDs, as 0xRRGGBB.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty array.
  /// </para>
  ///-
  function TYColorLedCluster.get_rgbColorArrayAtPowerOn(ledIndex: LongInt; count: LongInt):TLongIntArray;
    var
      buff : TByteArray;
      res : TLongIntArray;
      idx : LongInt;
      r : LongInt;
      g : LongInt;
      b : LongInt;
      res_pos : LongInt;
    begin
      buff := self._download('rgb.bin?typ=4&pos='+inttostr(3*ledIndex)+'&len='+inttostr(3*count));
      SetLength(res, 0);
      res_pos := length(res);
      SetLength(res, res_pos+count);;
      idx := 0;
      while idx < count do
        begin
          r := buff[3*idx];
          g := buff[3*idx+1];
          b := buff[3*idx+2];
          res[res_pos] := r*65536+g*256+b;
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a list on sequence index for each RGB LED.
  /// <para>
  ///   The first number represents the
  ///   sequence index for the the first LED, the second number represents the sequence
  ///   index for the second LED, etc.
  /// </para>
  /// </summary>
  /// <param name="ledIndex">
  ///   index of the first LED which should be returned
  /// </param>
  /// <param name="count">
  ///   number of LEDs which should be returned
  /// </param>
  /// <returns>
  ///   a list of integers with sequence index
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty array.
  /// </para>
  ///-
  function TYColorLedCluster.get_linkedSeqArray(ledIndex: LongInt; count: LongInt):TLongIntArray;
    var
      buff : TByteArray;
      res : TLongIntArray;
      idx : LongInt;
      seq : LongInt;
      res_pos : LongInt;
    begin
      buff := self._download('rgb.bin?typ=1&pos='+inttostr(ledIndex)+'&len='+inttostr(count));
      SetLength(res, 0);
      res_pos := length(res);
      SetLength(res, res_pos+count);;
      idx := 0;
      while idx < count do
        begin
          seq := buff[idx];
          res[res_pos] := seq;
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a list on 32 bit signatures for specified blinking sequences.
  /// <para>
  ///   Since blinking sequences cannot be read from the device, this can be used
  ///   to detect if a specific blinking sequence is already programmed.
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   index of the first blinking sequence which should be returned
  /// </param>
  /// <param name="count">
  ///   number of blinking sequences which should be returned
  /// </param>
  /// <returns>
  ///   a list of 32 bit integer signatures
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty array.
  /// </para>
  ///-
  function TYColorLedCluster.get_blinkSeqSignatures(seqIndex: LongInt; count: LongInt):TLongIntArray;
    var
      buff : TByteArray;
      res : TLongIntArray;
      idx : LongInt;
      hh : LongInt;
      hl : LongInt;
      lh : LongInt;
      ll : LongInt;
      res_pos : LongInt;
    begin
      buff := self._download('rgb.bin?typ=2&pos='+inttostr(4*seqIndex)+'&len='+inttostr(4*count));
      SetLength(res, 0);
      res_pos := length(res);
      SetLength(res, res_pos+count);;
      idx := 0;
      while idx < count do
        begin
          hh := buff[4*idx];
          hl := buff[4*idx+1];
          lh := buff[4*idx+2];
          ll := buff[4*idx+3];
          res[res_pos] := ((hh) shl 24)+((hl) shl 16)+((lh) shl 8)+ll;
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a list of integers with the current speed for specified blinking sequences.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   index of the first sequence speed which should be returned
  /// </param>
  /// <param name="count">
  ///   number of sequence speeds which should be returned
  /// </param>
  /// <returns>
  ///   a list of integers, 0 for sequences turned off and 1 for sequences running
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty array.
  /// </para>
  ///-
  function TYColorLedCluster.get_blinkSeqStateSpeed(seqIndex: LongInt; count: LongInt):TLongIntArray;
    var
      buff : TByteArray;
      res : TLongIntArray;
      idx : LongInt;
      lh : LongInt;
      ll : LongInt;
      res_pos : LongInt;
    begin
      buff := self._download('rgb.bin?typ=6&pos='+inttostr(seqIndex)+'&len='+inttostr(count));
      SetLength(res, 0);
      res_pos := length(res);
      SetLength(res, res_pos+count);;
      idx := 0;
      while idx < count do
        begin
          lh := buff[2*idx];
          ll := buff[2*idx+1];
          res[res_pos] := ((lh) shl 8)+ll;
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a list of integers with the "auto-start at power on" flag state for specified blinking sequences.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   index of the first blinking sequence which should be returned
  /// </param>
  /// <param name="count">
  ///   number of blinking sequences which should be returned
  /// </param>
  /// <returns>
  ///   a list of integers, 0 for sequences turned off and 1 for sequences running
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty array.
  /// </para>
  ///-
  function TYColorLedCluster.get_blinkSeqStateAtPowerOn(seqIndex: LongInt; count: LongInt):TLongIntArray;
    var
      buff : TByteArray;
      res : TLongIntArray;
      idx : LongInt;
      started : LongInt;
      res_pos : LongInt;
    begin
      buff := self._download('rgb.bin?typ=5&pos='+inttostr(seqIndex)+'&len='+inttostr(count));
      SetLength(res, 0);
      res_pos := length(res);
      SetLength(res, res_pos+count);;
      idx := 0;
      while idx < count do
        begin
          started := buff[idx];
          res[res_pos] := started;
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a list of integers with the started state for specified blinking sequences.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="seqIndex">
  ///   index of the first blinking sequence which should be returned
  /// </param>
  /// <param name="count">
  ///   number of blinking sequences which should be returned
  /// </param>
  /// <returns>
  ///   a list of integers, 0 for sequences turned off and 1 for sequences running
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty array.
  /// </para>
  ///-
  function TYColorLedCluster.get_blinkSeqState(seqIndex: LongInt; count: LongInt):TLongIntArray;
    var
      buff : TByteArray;
      res : TLongIntArray;
      idx : LongInt;
      started : LongInt;
      res_pos : LongInt;
    begin
      buff := self._download('rgb.bin?typ=3&pos='+inttostr(seqIndex)+'&len='+inttostr(count));
      SetLength(res, 0);
      res_pos := length(res);
      SetLength(res, res_pos+count);;
      idx := 0;
      while idx < count do
        begin
          started := buff[idx];
          res[res_pos] := started;
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYColorLedCluster.nextColorLedCluster(): TYColorLedCluster;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextColorLedCluster := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextColorLedCluster := nil;
          exit;
        end;
      nextColorLedCluster := TYColorLedCluster.FindColorLedCluster(hwid);
    end;

  class function TYColorLedCluster.FirstColorLedCluster(): TYColorLedCluster;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('ColorLedCluster', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYColorLedCluster.FindColorLedCluster(serial+'.'+funcId);
    end;

//--- (end of YColorLedCluster implementation)

//--- (YColorLedCluster functions)

  function yFindColorLedCluster(func:string): TYColorLedCluster;
    begin
      result := TYColorLedCluster.FindColorLedCluster(func);
    end;

  function yFirstColorLedCluster(): TYColorLedCluster;
    begin
      result := TYColorLedCluster.FirstColorLedCluster();
    end;

  procedure _ColorLedClusterCleanup();
    begin
    end;

//--- (end of YColorLedCluster functions)

initialization
  //--- (YColorLedCluster initialization)
  //--- (end of YColorLedCluster initialization)

finalization
  //--- (YColorLedCluster cleanup)
  _ColorLedClusterCleanup();
  //--- (end of YColorLedCluster cleanup)
end.
