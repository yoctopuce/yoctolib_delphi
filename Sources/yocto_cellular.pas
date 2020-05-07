{*********************************************************************
 *
 * $Id: yocto_cellular.pas 40298 2020-05-05 08:37:49Z seb $
 *
 * Implements yFindCellular(), the high-level API for Cellular functions
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


unit yocto_cellular;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (generated code: YCellular definitions)

const Y_LINKQUALITY_INVALID           = YAPI_INVALID_UINT;
const Y_CELLOPERATOR_INVALID          = YAPI_INVALID_STRING;
const Y_CELLIDENTIFIER_INVALID        = YAPI_INVALID_STRING;
const Y_CELLTYPE_GPRS = 0;
const Y_CELLTYPE_EGPRS = 1;
const Y_CELLTYPE_WCDMA = 2;
const Y_CELLTYPE_HSDPA = 3;
const Y_CELLTYPE_NONE = 4;
const Y_CELLTYPE_CDMA = 5;
const Y_CELLTYPE_LTE_M = 6;
const Y_CELLTYPE_NB_IOT = 7;
const Y_CELLTYPE_EC_GSM_IOT = 8;
const Y_CELLTYPE_INVALID = -1;
const Y_IMSI_INVALID                  = YAPI_INVALID_STRING;
const Y_MESSAGE_INVALID               = YAPI_INVALID_STRING;
const Y_PIN_INVALID                   = YAPI_INVALID_STRING;
const Y_RADIOCONFIG_INVALID           = YAPI_INVALID_STRING;
const Y_LOCKEDOPERATOR_INVALID        = YAPI_INVALID_STRING;
const Y_AIRPLANEMODE_OFF = 0;
const Y_AIRPLANEMODE_ON = 1;
const Y_AIRPLANEMODE_INVALID = -1;
const Y_ENABLEDATA_HOMENETWORK = 0;
const Y_ENABLEDATA_ROAMING = 1;
const Y_ENABLEDATA_NEVER = 2;
const Y_ENABLEDATA_NEUTRALITY = 3;
const Y_ENABLEDATA_INVALID = -1;
const Y_APN_INVALID                   = YAPI_INVALID_STRING;
const Y_APNSECRET_INVALID             = YAPI_INVALID_STRING;
const Y_PINGINTERVAL_INVALID          = YAPI_INVALID_UINT;
const Y_DATASENT_INVALID              = YAPI_INVALID_UINT;
const Y_DATARECEIVED_INVALID          = YAPI_INVALID_UINT;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of generated code: YCellular definitions)

//--- (generated code: YCellRecord definitions)


//--- (end of generated code: YCellRecord definitions)


type
  TYCellular = class;
  TYCellRecord = class;
  //--- (generated code: YCellular class start)
  TYCellularValueCallback = procedure(func: TYCellular; value:string);
  TYCellularTimedReportCallback = procedure(func: TYCellular; value:TYMeasure);

  ////
  /// <summary>
  ///   TYCellular Class: cellular interface control interface, available for instance in the
  ///   YoctoHub-GSM-2G, the YoctoHub-GSM-3G-EU or the YoctoHub-GSM-3G-NA
  /// <para>
  ///   The <c>YCellular</c> class provides control over cellular network parameters
  ///   and status for devices that are GSM-enabled.
  ///   Note that TCP/IP parameters are configured separately, using class <c>YNetwork</c>.
  /// </para>
  /// </summary>
  ///-
  TYCellular=class(TYFunction)
  //--- (end of generated code: YCellular class start)
  protected
  //--- (generated code: YCellular declaration)
    // Attributes (function value cache)
    _linkQuality              : LongInt;
    _cellOperator             : string;
    _cellIdentifier           : string;
    _cellType                 : Integer;
    _imsi                     : string;
    _message                  : string;
    _pin                      : string;
    _radioConfig              : string;
    _lockedOperator           : string;
    _airplaneMode             : Integer;
    _enableData               : Integer;
    _apn                      : string;
    _apnSecret                : string;
    _pingInterval             : LongInt;
    _dataSent                 : LongInt;
    _dataReceived             : LongInt;
    _command                  : string;
    _valueCallbackCellular    : TYCellularValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YCellular declaration)

  public
    //--- (generated code: YCellular accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the link quality, expressed in percent.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the link quality, expressed in percent
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LINKQUALITY_INVALID</c>.
    /// </para>
    ///-
    function get_linkQuality():LongInt;

    ////
    /// <summary>
    ///   Returns the name of the cell operator currently in use.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the name of the cell operator currently in use
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CELLOPERATOR_INVALID</c>.
    /// </para>
    ///-
    function get_cellOperator():string;

    ////
    /// <summary>
    ///   Returns the unique identifier of the cellular antenna in use: MCC, MNC, LAC and Cell ID.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the unique identifier of the cellular antenna in use: MCC, MNC, LAC and Cell ID
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CELLIDENTIFIER_INVALID</c>.
    /// </para>
    ///-
    function get_cellIdentifier():string;

    ////
    /// <summary>
    ///   Active cellular connection type.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_CELLTYPE_GPRS</c>, <c>Y_CELLTYPE_EGPRS</c>, <c>Y_CELLTYPE_WCDMA</c>,
    ///   <c>Y_CELLTYPE_HSDPA</c>, <c>Y_CELLTYPE_NONE</c>, <c>Y_CELLTYPE_CDMA</c>, <c>Y_CELLTYPE_LTE_M</c>,
    ///   <c>Y_CELLTYPE_NB_IOT</c> and <c>Y_CELLTYPE_EC_GSM_IOT</c>
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CELLTYPE_INVALID</c>.
    /// </para>
    ///-
    function get_cellType():Integer;

    ////
    /// <summary>
    ///   Returns the International Mobile Subscriber Identity (MSI) that uniquely identifies
    ///   the SIM card.
    /// <para>
    ///   The first 3 digits represent the mobile country code (MCC), which
    ///   is followed by the mobile network code (MNC), either 2-digit (European standard)
    ///   or 3-digit (North American standard)
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the International Mobile Subscriber Identity (MSI) that uniquely identifies
    ///   the SIM card
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_IMSI_INVALID</c>.
    /// </para>
    ///-
    function get_imsi():string;

    ////
    /// <summary>
    ///   Returns the latest status message from the wireless interface.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the latest status message from the wireless interface
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MESSAGE_INVALID</c>.
    /// </para>
    ///-
    function get_message():string;

    ////
    /// <summary>
    ///   Returns an opaque string if a PIN code has been configured in the device to access
    ///   the SIM card, or an empty string if none has been configured or if the code provided
    ///   was rejected by the SIM card.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to an opaque string if a PIN code has been configured in the device to access
    ///   the SIM card, or an empty string if none has been configured or if the code provided
    ///   was rejected by the SIM card
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PIN_INVALID</c>.
    /// </para>
    ///-
    function get_pin():string;

    ////
    /// <summary>
    ///   Changes the PIN code used by the module to access the SIM card.
    /// <para>
    ///   This function does not change the code on the SIM card itself, but only changes
    ///   the parameter used by the device to try to get access to it. If the SIM code
    ///   does not work immediately on first try, it will be automatically forgotten
    ///   and the message will be set to "Enter SIM PIN". The method should then be
    ///   invoked again with right correct PIN code. After three failed attempts in a row,
    ///   the message is changed to "Enter SIM PUK" and the SIM card PUK code must be
    ///   provided using method <c>sendPUK</c>.
    /// </para>
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module to save the
    ///   new value in the device flash.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the PIN code used by the module to access the SIM card
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
    function set_pin(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the type of protocol used over the serial line, as a string.
    /// <para>
    ///   Possible values are "Line" for ASCII messages separated by CR and/or LF,
    ///   "Frame:[timeout]ms" for binary messages separated by a delay time,
    ///   "Char" for a continuous ASCII stream or
    ///   "Byte" for a continuous binary stream.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the type of protocol used over the serial line, as a string
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_RADIOCONFIG_INVALID</c>.
    /// </para>
    ///-
    function get_radioConfig():string;

    ////
    /// <summary>
    ///   Changes the type of protocol used over the serial line.
    /// <para>
    ///   Possible values are "Line" for ASCII messages separated by CR and/or LF,
    ///   "Frame:[timeout]ms" for binary messages separated by a delay time,
    ///   "Char" for a continuous ASCII stream or
    ///   "Byte" for a continuous binary stream.
    ///   The suffix "/[wait]ms" can be added to reduce the transmit rate so that there
    ///   is always at lest the specified number of milliseconds between each bytes sent.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the type of protocol used over the serial line
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
    function set_radioConfig(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the name of the only cell operator to use if automatic choice is disabled,
    ///   or an empty string if the SIM card will automatically choose among available
    ///   cell operators.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the name of the only cell operator to use if automatic choice is disabled,
    ///   or an empty string if the SIM card will automatically choose among available
    ///   cell operators
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LOCKEDOPERATOR_INVALID</c>.
    /// </para>
    ///-
    function get_lockedOperator():string;

    ////
    /// <summary>
    ///   Changes the name of the cell operator to be used.
    /// <para>
    ///   If the name is an empty
    ///   string, the choice will be made automatically based on the SIM card. Otherwise,
    ///   the selected operator is the only one that will be used.
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the name of the cell operator to be used
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
    function set_lockedOperator(newval:string):integer;

    ////
    /// <summary>
    ///   Returns true if the airplane mode is active (radio turned off).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_AIRPLANEMODE_OFF</c> or <c>Y_AIRPLANEMODE_ON</c>, according to true if the airplane
    ///   mode is active (radio turned off)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_AIRPLANEMODE_INVALID</c>.
    /// </para>
    ///-
    function get_airplaneMode():Integer;

    ////
    /// <summary>
    ///   Changes the activation state of airplane mode (radio turned off).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>Y_AIRPLANEMODE_OFF</c> or <c>Y_AIRPLANEMODE_ON</c>, according to the activation state of
    ///   airplane mode (radio turned off)
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
    function set_airplaneMode(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the condition for enabling IP data services (GPRS).
    /// <para>
    ///   When data services are disabled, SMS are the only mean of communication.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_ENABLEDATA_HOMENETWORK</c>, <c>Y_ENABLEDATA_ROAMING</c>,
    ///   <c>Y_ENABLEDATA_NEVER</c> and <c>Y_ENABLEDATA_NEUTRALITY</c> corresponding to the condition for
    ///   enabling IP data services (GPRS)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ENABLEDATA_INVALID</c>.
    /// </para>
    ///-
    function get_enableData():Integer;

    ////
    /// <summary>
    ///   Changes the condition for enabling IP data services (GPRS).
    /// <para>
    ///   The service can be either fully deactivated, or limited to the SIM home network,
    ///   or enabled for all partner networks (roaming). Caution: enabling data services
    ///   on roaming networks may cause prohibitive communication costs !
    /// </para>
    /// <para>
    ///   When data services are disabled, SMS are the only mean of communication.
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_ENABLEDATA_HOMENETWORK</c>, <c>Y_ENABLEDATA_ROAMING</c>,
    ///   <c>Y_ENABLEDATA_NEVER</c> and <c>Y_ENABLEDATA_NEUTRALITY</c> corresponding to the condition for
    ///   enabling IP data services (GPRS)
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
    function set_enableData(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the Access Point Name (APN) to be used, if needed.
    /// <para>
    ///   When left blank, the APN suggested by the cell operator will be used.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the Access Point Name (APN) to be used, if needed
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_APN_INVALID</c>.
    /// </para>
    ///-
    function get_apn():string;

    ////
    /// <summary>
    ///   Returns the Access Point Name (APN) to be used, if needed.
    /// <para>
    ///   When left blank, the APN suggested by the cell operator will be used.
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string
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
    function set_apn(newval:string):integer;

    ////
    /// <summary>
    ///   Returns an opaque string if APN authentication parameters have been configured
    ///   in the device, or an empty string otherwise.
    /// <para>
    ///   To configure these parameters, use <c>set_apnAuth()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to an opaque string if APN authentication parameters have been configured
    ///   in the device, or an empty string otherwise
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_APNSECRET_INVALID</c>.
    /// </para>
    ///-
    function get_apnSecret():string;

    function set_apnSecret(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the automated connectivity check interval, in seconds.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the automated connectivity check interval, in seconds
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PINGINTERVAL_INVALID</c>.
    /// </para>
    ///-
    function get_pingInterval():LongInt;

    ////
    /// <summary>
    ///   Changes the automated connectivity check interval, in seconds.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the automated connectivity check interval, in seconds
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
    function set_pingInterval(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the number of bytes sent so far.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of bytes sent so far
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DATASENT_INVALID</c>.
    /// </para>
    ///-
    function get_dataSent():LongInt;

    ////
    /// <summary>
    ///   Changes the value of the outgoing data counter.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the value of the outgoing data counter
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
    function set_dataSent(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the number of bytes received so far.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of bytes received so far
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DATARECEIVED_INVALID</c>.
    /// </para>
    ///-
    function get_dataReceived():LongInt;

    ////
    /// <summary>
    ///   Changes the value of the incoming data counter.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the value of the incoming data counter
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
    function set_dataReceived(newval:LongInt):integer;

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
    ///   Use the method <c>YCellular.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YCellular</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindCellular(func: string):TYCellular;

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
    function registerValueCallback(callback: TYCellularValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Sends a PUK code to unlock the SIM card after three failed PIN code attempts, and
    ///   setup a new PIN into the SIM card.
    /// <para>
    ///   Only ten consecutive tentatives are permitted:
    ///   after that, the SIM card will be blocked permanently without any mean of recovery
    ///   to use it again. Note that after calling this method, you have usually to invoke
    ///   method <c>set_pin()</c> to tell the YoctoHub which PIN to use in the future.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="puk">
    ///   the SIM PUK code
    /// </param>
    /// <param name="newPin">
    ///   new PIN code to configure into the SIM card
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function sendPUK(puk: string; newPin: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Configure authentication parameters to connect to the APN.
    /// <para>
    ///   Both
    ///   PAP and CHAP authentication are supported.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="username">
    ///   APN username
    /// </param>
    /// <param name="password">
    ///   APN password
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_apnAuth(username: string; password: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Clear the transmitted data counters.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function clearDataCounters():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends an AT command to the GSM module and returns the command output.
    /// <para>
    ///   The command will only execute when the GSM module is in standard
    ///   command state, and should leave it in the exact same state.
    ///   Use this function with great care !
    /// </para>
    /// </summary>
    /// <param name="cmd">
    ///   the AT command to execute, like for instance: "+CCLK?".
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   a string with the result of the commands. Empty lines are
    ///   automatically removed from the output.
    /// </returns>
    ///-
    function _AT(cmd: string):string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the list detected cell operators in the neighborhood.
    /// <para>
    ///   This function will typically take between 30 seconds to 1 minute to
    ///   return. Note that any SIM card can usually only connect to specific
    ///   operators. All networks returned by this function might therefore
    ///   not be available for connection.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list of string (cell operator names).
    /// </returns>
    ///-
    function get_availableOperators():TStringArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list of nearby cellular antennas, as required for quick
    ///   geolocation of the device.
    /// <para>
    ///   The first cell listed is the serving
    ///   cell, and the next ones are the neighbor cells reported by the
    ///   serving cell.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list of <c>YCellRecords</c>.
    /// </returns>
    ///-
    function quickCellSurvey():TYCellRecordArray; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of cellular interfaces started using <c>yFirstCellular()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned cellular interfaces order.
    ///   If you want to find a specific a cellular interface, use <c>Cellular.findCellular()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YCellular</c> object, corresponding to
    ///   a cellular interface currently online, or a <c>NIL</c> pointer
    ///   if there are no more cellular interfaces to enumerate.
    /// </returns>
    ///-
    function nextCellular():TYCellular;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstCellular():TYCellular;
  //--- (end of generated code: YCellular accessors declaration)
  end;



  //--- (generated code: YCellRecord class start)
  ////
  /// <summary>
  ///   T
  /// <para>
  ///   YCellRecord Class: Cellular antenna description, returned by <c>cellular.quickCellSurvey</c> method
  /// </para>
  /// <para>
  ///   <c>YCellRecord</c> objects are used to describe a wireless network.
  ///   These objects are used in particular in conjunction with the
  ///   <c>YCellular</c> class.
  /// </para>
  /// </summary>
  ///-
  TYCellRecord=class(TObject)
  //--- (end of generated code: YCellRecord class start)
  protected
  //--- (generated code: YCellRecord declaration)
    // Attributes (function value cache)
    _oper                     : string;
    _mcc                      : LongInt;
    _mnc                      : LongInt;
    _lac                      : LongInt;
    _cid                      : LongInt;
    _dbm                      : LongInt;
    _tad                      : LongInt;

    //--- (end of generated code: YCellRecord declaration)

  public
    //--- (generated code: YCellRecord accessors declaration)
    ////
    /// <summary>
    ///   Returns the name of the the cell operator, as received from the network.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with the name of the the cell operator.
    /// </returns>
    ///-
    function get_cellOperator():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the Mobile Country Code (MCC).
    /// <para>
    ///   The MCC is a unique identifier for each country.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the Mobile Country Code (MCC).
    /// </returns>
    ///-
    function get_mobileCountryCode():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the Mobile Network Code (MNC).
    /// <para>
    ///   The MNC is a unique identifier for each phone
    ///   operator within a country.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the Mobile Network Code (MNC).
    /// </returns>
    ///-
    function get_mobileNetworkCode():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the Location Area Code (LAC).
    /// <para>
    ///   The LAC is a unique identifier for each
    ///   place within a country.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the Location Area Code (LAC).
    /// </returns>
    ///-
    function get_locationAreaCode():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the Cell ID.
    /// <para>
    ///   The Cell ID is a unique identifier for each
    ///   base transmission station within a LAC.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the Cell Id.
    /// </returns>
    ///-
    function get_cellId():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the signal strength, measured in dBm.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the signal strength.
    /// </returns>
    ///-
    function get_signalStrength():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the Timing Advance (TA).
    /// <para>
    ///   The TA corresponds to the time necessary
    ///   for the signal to reach the base station from the device.
    ///   Each increment corresponds about to 550m of distance.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the Timing Advance (TA).
    /// </returns>
    ///-
    function get_timingAdvance():LongInt; overload; virtual;


  //--- (end of generated code: YCellRecord accessors declaration)
  end;


//--- (generated code: YCellular functions declaration)
  ////
  /// <summary>
  ///   Retrieves a cellular interface for a given identifier.
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
  ///   This function does not require that the cellular interface is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YCellular.isOnline()</c> to test if the cellular interface is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a cellular interface by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the cellular interface, for instance
  ///   <c>YHUBGSM1.cellular</c>.
  /// </param>
  /// <returns>
  ///   a <c>YCellular</c> object allowing you to drive the cellular interface.
  /// </returns>
  ///-
  function yFindCellular(func:string):TYCellular;
  ////
  /// <summary>
  ///   Starts the enumeration of cellular interfaces currently accessible.
  /// <para>
  ///   Use the method <c>YCellular.nextCellular()</c> to iterate on
  ///   next cellular interfaces.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YCellular</c> object, corresponding to
  ///   the first cellular interface currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstCellular():TYCellular;

//--- (end of generated code: YCellular functions declaration)
//--- (generated code: YCellRecord functions declaration)
//--- (end of generated code: YCellRecord functions declaration)

implementation
//--- (generated code: YCellular dlldef)
//--- (end of generated code: YCellular dlldef)
//--- (generated code: YCellRecord dlldef)
//--- (end of generated code: YCellRecord dlldef)

  constructor TYCellular.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Cellular';
      //--- (generated code: YCellular accessors initialization)
      _linkQuality := Y_LINKQUALITY_INVALID;
      _cellOperator := Y_CELLOPERATOR_INVALID;
      _cellIdentifier := Y_CELLIDENTIFIER_INVALID;
      _cellType := Y_CELLTYPE_INVALID;
      _imsi := Y_IMSI_INVALID;
      _message := Y_MESSAGE_INVALID;
      _pin := Y_PIN_INVALID;
      _radioConfig := Y_RADIOCONFIG_INVALID;
      _lockedOperator := Y_LOCKEDOPERATOR_INVALID;
      _airplaneMode := Y_AIRPLANEMODE_INVALID;
      _enableData := Y_ENABLEDATA_INVALID;
      _apn := Y_APN_INVALID;
      _apnSecret := Y_APNSECRET_INVALID;
      _pingInterval := Y_PINGINTERVAL_INVALID;
      _dataSent := Y_DATASENT_INVALID;
      _dataReceived := Y_DATARECEIVED_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackCellular := nil;
      //--- (end of generated code: YCellular accessors initialization)
    end;


//--- (generated code: YCellular implementation)
{$HINTS OFF}
  function TYCellular._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'linkQuality') then
        begin
          _linkQuality := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'cellOperator') then
        begin
          _cellOperator := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'cellIdentifier') then
        begin
          _cellIdentifier := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'cellType') then
        begin
          _cellType := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'imsi') then
        begin
          _imsi := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'message') then
        begin
          _message := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'pin') then
        begin
          _pin := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'radioConfig') then
        begin
          _radioConfig := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'lockedOperator') then
        begin
          _lockedOperator := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'airplaneMode') then
        begin
          _airplaneMode := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'enableData') then
        begin
          _enableData := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'apn') then
        begin
          _apn := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'apnSecret') then
        begin
          _apnSecret := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'pingInterval') then
        begin
          _pingInterval := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'dataSent') then
        begin
          _dataSent := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'dataReceived') then
        begin
          _dataReceived := integer(member^.ivalue);
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

  function TYCellular.get_linkQuality():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LINKQUALITY_INVALID;
              exit;
            end;
        end;
      res := self._linkQuality;
      result := res;
      exit;
    end;


  function TYCellular.get_cellOperator():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CELLOPERATOR_INVALID;
              exit;
            end;
        end;
      res := self._cellOperator;
      result := res;
      exit;
    end;


  function TYCellular.get_cellIdentifier():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CELLIDENTIFIER_INVALID;
              exit;
            end;
        end;
      res := self._cellIdentifier;
      result := res;
      exit;
    end;


  function TYCellular.get_cellType():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CELLTYPE_INVALID;
              exit;
            end;
        end;
      res := self._cellType;
      result := res;
      exit;
    end;


  function TYCellular.get_imsi():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_IMSI_INVALID;
              exit;
            end;
        end;
      res := self._imsi;
      result := res;
      exit;
    end;


  function TYCellular.get_message():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MESSAGE_INVALID;
              exit;
            end;
        end;
      res := self._message;
      result := res;
      exit;
    end;


  function TYCellular.get_pin():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PIN_INVALID;
              exit;
            end;
        end;
      res := self._pin;
      result := res;
      exit;
    end;


  function TYCellular.set_pin(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('pin',rest_val);
    end;

  function TYCellular.get_radioConfig():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RADIOCONFIG_INVALID;
              exit;
            end;
        end;
      res := self._radioConfig;
      result := res;
      exit;
    end;


  function TYCellular.set_radioConfig(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('radioConfig',rest_val);
    end;

  function TYCellular.get_lockedOperator():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LOCKEDOPERATOR_INVALID;
              exit;
            end;
        end;
      res := self._lockedOperator;
      result := res;
      exit;
    end;


  function TYCellular.set_lockedOperator(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('lockedOperator',rest_val);
    end;

  function TYCellular.get_airplaneMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_AIRPLANEMODE_INVALID;
              exit;
            end;
        end;
      res := self._airplaneMode;
      result := res;
      exit;
    end;


  function TYCellular.set_airplaneMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('airplaneMode',rest_val);
    end;

  function TYCellular.get_enableData():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ENABLEDATA_INVALID;
              exit;
            end;
        end;
      res := self._enableData;
      result := res;
      exit;
    end;


  function TYCellular.set_enableData(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('enableData',rest_val);
    end;

  function TYCellular.get_apn():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_APN_INVALID;
              exit;
            end;
        end;
      res := self._apn;
      result := res;
      exit;
    end;


  function TYCellular.set_apn(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('apn',rest_val);
    end;

  function TYCellular.get_apnSecret():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_APNSECRET_INVALID;
              exit;
            end;
        end;
      res := self._apnSecret;
      result := res;
      exit;
    end;


  function TYCellular.set_apnSecret(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('apnSecret',rest_val);
    end;

  function TYCellular.get_pingInterval():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PINGINTERVAL_INVALID;
              exit;
            end;
        end;
      res := self._pingInterval;
      result := res;
      exit;
    end;


  function TYCellular.set_pingInterval(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('pingInterval',rest_val);
    end;

  function TYCellular.get_dataSent():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DATASENT_INVALID;
              exit;
            end;
        end;
      res := self._dataSent;
      result := res;
      exit;
    end;


  function TYCellular.set_dataSent(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('dataSent',rest_val);
    end;

  function TYCellular.get_dataReceived():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_DATARECEIVED_INVALID;
              exit;
            end;
        end;
      res := self._dataReceived;
      result := res;
      exit;
    end;


  function TYCellular.set_dataReceived(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('dataReceived',rest_val);
    end;

  function TYCellular.get_command():string;
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


  function TYCellular.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYCellular.FindCellular(func: string):TYCellular;
    var
      obj : TYCellular;
    begin
      obj := TYCellular(TYFunction._FindFromCache('Cellular', func));
      if obj = nil then
        begin
          obj :=  TYCellular.create(func);
          TYFunction._AddToCache('Cellular',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYCellular.registerValueCallback(callback: TYCellularValueCallback):LongInt;
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
      self._valueCallbackCellular := callback;
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


  function TYCellular._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackCellular) <> nil) then
        begin
          self._valueCallbackCellular(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYCellular.sendPUK(puk: string; newPin: string):LongInt;
    var
      gsmMsg : string;
    begin
      gsmMsg := self.get_message;
      if not((Copy(gsmMsg, 0 + 1, 13) = 'Enter SIM PUK')) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT, 'PUK not expected at this time');
          result:=YAPI_INVALID_ARGUMENT;
          exit;
        end;
      if (newPin = '') then
        begin
          result := self.set_command('AT+CPIN='+puk+',0000;
          +CLCK=SC,0,0000');
          exit;
        end;
      result := self.set_command('AT+CPIN='+puk+','+newPin);
      exit;
    end;


  function TYCellular.set_apnAuth(username: string; password: string):LongInt;
    begin
      result := self.set_apnSecret(''+username+','+password);
      exit;
    end;


  function TYCellular.clearDataCounters():LongInt;
    var
      retcode : LongInt;
    begin
      retcode := self.set_dataReceived(0);
      if retcode <> YAPI_SUCCESS then
        begin
          result := retcode;
          exit;
        end;
      retcode := self.set_dataSent(0);
      result := retcode;
      exit;
    end;


  function TYCellular._AT(cmd: string):string;
    var
      chrPos : LongInt;
      cmdLen : LongInt;
      waitMore : LongInt;
      res : string;
      buff : TByteArray;
      bufflen : LongInt;
      buffstr : string;
      buffstrlen : LongInt;
      idx : LongInt;
      suffixlen : LongInt;
    begin
      cmdLen := Length(cmd);
      chrPos := (pos('#', cmd) - 1);
      while chrPos >= 0 do
        begin
          cmd := ''+ Copy(cmd,  0 + 1, chrPos)+''+chr( 37)+'23'+Copy(cmd,  chrPos+1 + 1, cmdLen-chrPos-1);
          cmdLen := cmdLen + 2;
          chrPos := (pos('#', cmd) - 1);
        end;
      chrPos := (pos('+', cmd) - 1);
      while chrPos >= 0 do
        begin
          cmd := ''+ Copy(cmd,  0 + 1, chrPos)+''+chr( 37)+'2B'+Copy(cmd,  chrPos+1 + 1, cmdLen-chrPos-1);
          cmdLen := cmdLen + 2;
          chrPos := (pos('+', cmd) - 1);
        end;
      chrPos := (pos('=', cmd) - 1);
      while chrPos >= 0 do
        begin
          cmd := ''+ Copy(cmd,  0 + 1, chrPos)+''+chr( 37)+'3D'+Copy(cmd,  chrPos+1 + 1, cmdLen-chrPos-1);
          cmdLen := cmdLen + 2;
          chrPos := (pos('=', cmd) - 1);
        end;
      cmd := 'at.txt?cmd='+cmd;
      res := '';
      // max 2 minutes (each iteration may take up to 5 seconds if waiting)
      waitMore := 24;
      while waitMore > 0 do
        begin
          buff := self._download(cmd);
          bufflen := length(buff);
          buffstr := _ByteToString(buff);
          buffstrlen := Length(buffstr);
          idx := bufflen - 1;
          while (idx > 0) and(buff[idx] <> 64) and(buff[idx] <> 10) and(buff[idx] <> 13) do
            begin
              idx := idx - 1;
            end;
          if buff[idx] = 64 then
            begin
              // continuation detected
              suffixlen := bufflen - idx;
              cmd := 'at.txt?cmd='+Copy(buffstr,  buffstrlen - suffixlen + 1, suffixlen);
              buffstr := Copy(buffstr,  0 + 1, buffstrlen - suffixlen);
              waitMore := waitMore - 1;
            end
          else
            begin
              // request complete
              waitMore := 0;
            end;
          res := ''+ res+''+buffstr;
        end;
      result := res;
      exit;
    end;


  function TYCellular.get_availableOperators():TStringArray;
    var
      cops : string;
      idx : LongInt;
      slen : LongInt;
      res : TStringArray;
      res_pos : LongInt;
    begin
      SetLength(res, 0);

      cops := self._AT('+COPS=?');
      slen := Length(cops);
      res_pos := 0;
      SetLength(res, 10);;
      idx := (pos('(', cops) - 1);
      while idx >= 0 do
        begin
          slen := slen - (idx+1);
          cops := Copy(cops,  idx+1 + 1, slen);
          idx := (pos('"', cops) - 1);
          if idx > 0 then
            begin
              slen := slen - (idx+1);
              cops := Copy(cops,  idx+1 + 1, slen);
              idx := (pos('"', cops) - 1);
              if idx > 0 then
                begin
                  res[res_pos] := Copy(cops,  0 + 1, idx);
                  inc(res_pos);
                end;
            end;
          idx := (pos('(', cops) - 1);
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYCellular.quickCellSurvey():TYCellRecordArray;
    var
      moni : string;
      recs : TStringArray;
      llen : LongInt;
      mccs : string;
      mcc : LongInt;
      mncs : string;
      mnc : LongInt;
      lac : LongInt;
      cellId : LongInt;
      dbms : string;
      dbm : LongInt;
      tads : string;
      tad : LongInt;
      oper : string;
      res : TYCellRecordArray;
      res_pos : LongInt;
      i_i : LongInt;
    begin
      SetLength(recs, 0);

      moni := self._AT('+CCED=0;#MONI=7;#MONI');
      mccs := Copy(moni, 7 + 1, 3);
      if (Copy(mccs, 0 + 1, 1) = '0') then
        begin
          mccs := Copy(mccs, 1 + 1, 2);
        end;
      if (Copy(mccs, 0 + 1, 1) = '0') then
        begin
          mccs := Copy(mccs, 1 + 1, 1);
        end;
      mcc := _atoi(mccs);
      mncs := Copy(moni, 11 + 1, 3);
      if (Copy(mncs, 2 + 1, 1) = ',') then
        begin
          mncs := Copy(mncs, 0 + 1, 2);
        end;
      if (Copy(mncs, 0 + 1, 1) = '0') then
        begin
          mncs := Copy(mncs, 1 + 1, Length(mncs)-1);
        end;
      mnc := _atoi(mncs);
      recs := _stringSplit(moni, '#');
      // process each line in turn
      res_pos := 0;
      SetLength(res, length(celllist));;
      for i_i:=0 to length(recs)-1 do
        begin
          llen := Length(recs[i_i]) - 2;
          if llen >= 44 then
            begin
              if (Copy(recs[i_i], 41 + 1, 3) = 'dbm') then
                begin
                  lac := StrToInt('$0' + Copy(recs[i_i], 16 + 1, 4));
                  cellId := StrToInt('$0' + Copy(recs[i_i], 23 + 1, 4));
                  dbms := Copy(recs[i_i], 37 + 1, 4);
                  if (Copy(dbms, 0 + 1, 1) = ' ') then
                    begin
                      dbms := Copy(dbms, 1 + 1, 3);
                    end;
                  dbm := _atoi(dbms);
                  if llen > 66 then
                    begin
                      tads := Copy(recs[i_i], 54 + 1, 2);
                      if (Copy(tads, 0 + 1, 1) = ' ') then
                        begin
                          tads := Copy(tads, 1 + 1, 3);
                        end;
                      tad := _atoi(tads);
                      oper := Copy(recs[i_i], 66 + 1, llen-66);
                    end
                  else
                    begin
                      tad := -1;
                      oper := '';
                    end;
                  if lac < 65535 then
                    begin
                      res[res_pos] := TYCellRecord.create(mcc, mnc, lac, cellId, dbm, tad, oper);
                      inc(res_pos);
                    end;
                end;
            end;
        end;
      result := res;
      exit;
    end;


  function TYCellular.nextCellular(): TYCellular;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextCellular := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextCellular := nil;
          exit;
        end;
      nextCellular := TYCellular.FindCellular(hwid);
    end;

  class function TYCellular.FirstCellular(): TYCellular;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Cellular', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYCellular.FindCellular(serial+'.'+funcId);
    end;

//--- (end of generated code: YCellular implementation)

//--- (generated code: YCellular functions)

  function yFindCellular(func:string): TYCellular;
    begin
      result := TYCellular.FindCellular(func);
    end;

  function yFirstCellular(): TYCellular;
    begin
      result := TYCellular.FirstCellular();
    end;

  procedure _CellularCleanup();
    begin
    end;

//--- (end of generated code: YCellular functions)

  constructor TYCellRecord.Create(mcc, mnc, lac, cellId, dbm, tad, cnsot: integer; oper :string);
    begin
      //--- (generated code: YCellRecord accessors initialization)
      _mcc := 0;
      _mnc := 0;
      _lac := 0;
      _cid := 0;
      _dbm := 0;
      _tad := 0;
      //--- (end of generated code: YCellRecord accessors initialization)
      _oper := oper;
      _mcc := mcc;
      _mnc := mnc;
      _lac := lac;
      _cid := cellId;
      _dbm := dbm;
      _tad := tad;
    end;


//--- (generated code: YCellRecord implementation)

  function TYCellRecord.get_cellOperator():string;
    begin
      result := self._oper;
      exit;
    end;


  function TYCellRecord.get_mobileCountryCode():LongInt;
    begin
      result := self._mcc;
      exit;
    end;


  function TYCellRecord.get_mobileNetworkCode():LongInt;
    begin
      result := self._mnc;
      exit;
    end;


  function TYCellRecord.get_locationAreaCode():LongInt;
    begin
      result := self._lac;
      exit;
    end;


  function TYCellRecord.get_cellId():LongInt;
    begin
      result := self._cid;
      exit;
    end;


  function TYCellRecord.get_signalStrength():LongInt;
    begin
      result := self._dbm;
      exit;
    end;


  function TYCellRecord.get_timingAdvance():LongInt;
    begin
      result := self._tad;
      exit;
    end;


//--- (end of generated code: YCellRecord implementation)

//--- (generated code: YCellRecord functions)

  procedure _CellRecordCleanup();
    begin
    end;

//--- (end of generated code: YCellRecord functions)


initialization
  //--- (generated code: YCellular initialization)
  //--- (end of generated code: YCellular initialization)
  //--- (generated code: YCellRecord initialization)
  //--- (end of generated code: YCellRecord initialization)

finalization
  //--- (generated code: YCellular cleanup)
  _CellularCleanup();
  //--- (end of generated code: YCellular cleanup)
  //--- (generated code: YCellRecord cleanup)
  //--- (end of generated code: YCellRecord cleanup)
end.
