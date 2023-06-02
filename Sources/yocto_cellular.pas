{*********************************************************************
 *
 * $Id: yocto_cellular.pas 54155 2023-04-20 10:23:39Z seb $
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
  sysutils, classes,{$IFNDEF UNIX}windows,
{$ENDIF}
 yocto_api, yjson;

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
  ///   YoctoHub-GSM-2G, the YoctoHub-GSM-3G-EU, the YoctoHub-GSM-3G-NA or the YoctoHub-GSM-4G
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
    ///   On failure, throws an exception or returns <c>YCellular.LINKQUALITY_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YCellular.CELLOPERATOR_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YCellular.CELLIDENTIFIER_INVALID</c>.
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
    ///   a value among <c>YCellular.CELLTYPE_GPRS</c>, <c>YCellular.CELLTYPE_EGPRS</c>,
    ///   <c>YCellular.CELLTYPE_WCDMA</c>, <c>YCellular.CELLTYPE_HSDPA</c>, <c>YCellular.CELLTYPE_NONE</c>,
    ///   <c>YCellular.CELLTYPE_CDMA</c>, <c>YCellular.CELLTYPE_LTE_M</c>, <c>YCellular.CELLTYPE_NB_IOT</c>
    ///   and <c>YCellular.CELLTYPE_EC_GSM_IOT</c>
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YCellular.CELLTYPE_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YCellular.IMSI_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YCellular.MESSAGE_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YCellular.PIN_INVALID</c>.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
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
    ///   On failure, throws an exception or returns <c>YCellular.RADIOCONFIG_INVALID</c>.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
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
    ///   On failure, throws an exception or returns <c>YCellular.LOCKEDOPERATOR_INVALID</c>.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
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
    ///   either <c>YCellular.AIRPLANEMODE_OFF</c> or <c>YCellular.AIRPLANEMODE_ON</c>, according to true if
    ///   the airplane mode is active (radio turned off)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YCellular.AIRPLANEMODE_INVALID</c>.
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
    ///   either <c>YCellular.AIRPLANEMODE_OFF</c> or <c>YCellular.AIRPLANEMODE_ON</c>, according to the
    ///   activation state of airplane mode (radio turned off)
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
    ///   a value among <c>YCellular.ENABLEDATA_HOMENETWORK</c>, <c>YCellular.ENABLEDATA_ROAMING</c>,
    ///   <c>YCellular.ENABLEDATA_NEVER</c> and <c>YCellular.ENABLEDATA_NEUTRALITY</c> corresponding to the
    ///   condition for enabling IP data services (GPRS)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YCellular.ENABLEDATA_INVALID</c>.
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
    ///   a value among <c>YCellular.ENABLEDATA_HOMENETWORK</c>, <c>YCellular.ENABLEDATA_ROAMING</c>,
    ///   <c>YCellular.ENABLEDATA_NEVER</c> and <c>YCellular.ENABLEDATA_NEUTRALITY</c> corresponding to the
    ///   condition for enabling IP data services (GPRS)
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
    ///   On failure, throws an exception or returns <c>YCellular.APN_INVALID</c>.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
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
    ///   On failure, throws an exception or returns <c>YCellular.APNSECRET_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YCellular.PINGINTERVAL_INVALID</c>.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
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
    ///   On failure, throws an exception or returns <c>YCellular.DATASENT_INVALID</c>.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
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
    ///   On failure, throws an exception or returns <c>YCellular.DATARECEIVED_INVALID</c>.
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
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
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
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
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
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
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
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
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

    function imm_decodePLMN(mccmnc: string):string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the cell operator brand for a given MCC/MNC pair.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="mccmnc">
    ///   a string starting with a MCC code followed by a MNC code,
    /// </param>
    /// <returns>
    ///   a string containing the corresponding cell operator brand name.
    /// </returns>
    ///-
    function decodePLMN(mccmnc: string):string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the list available radio communication profiles, as a string array
    ///   (YoctoHub-GSM-4G only).
    /// <para>
    ///   Each string is a made of a numerical ID, followed by a colon,
    ///   followed by the profile description.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list of string describing available radio communication profiles.
    /// </returns>
    ///-
    function get_communicationProfiles():TStringArray; overload; virtual;


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
      ii_0 : LongInt;
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
      for ii_0:=0 to length(recs)-1 do
        begin
          llen := Length(recs[ii_0]) - 2;
          if llen >= 44 then
            begin
              if (Copy(recs[ii_0], 41 + 1, 3) = 'dbm') then
                begin
                  lac := StrToInt('$0' + Copy(recs[ii_0], 16 + 1, 4));
                  cellId := StrToInt('$0' + Copy(recs[ii_0], 23 + 1, 4));
                  dbms := Copy(recs[ii_0], 37 + 1, 4);
                  if (Copy(dbms, 0 + 1, 1) = ' ') then
                    begin
                      dbms := Copy(dbms, 1 + 1, 3);
                    end;
                  dbm := _atoi(dbms);
                  if llen > 66 then
                    begin
                      tads := Copy(recs[ii_0], 54 + 1, 2);
                      if (Copy(tads, 0 + 1, 1) = ' ') then
                        begin
                          tads := Copy(tads, 1 + 1, 3);
                        end;
                      tad := _atoi(tads);
                      oper := Copy(recs[ii_0], 66 + 1, llen-66);
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


  function TYCellular.imm_decodePLMN(mccmnc: string):string;
    var
      inputlen : LongInt;
      mcc : LongInt;
      npos : LongInt;
      nval : LongInt;
      ch : string;
      plmnid : LongInt;
    begin
      inputlen := Length(mccmnc);
      if inputlen < 5 then
        begin
          result := mccmnc;
          exit;
        end;
      mcc := _atoi(Copy(mccmnc, 0 + 1, 3));
      if mcc < 200 then
        begin
          result := mccmnc;
          exit;
        end;
      if (Copy(mccmnc, 3 + 1, 1) = ' ') then
        begin
          npos := 4;
        end
      else
        begin
          npos := 3;
        end;
      plmnid := mcc;
      while plmnid < 100000 and npos < inputlen do
        begin
          ch := Copy(mccmnc, npos + 1, 1);
          nval := _atoi(ch);
          if (ch = IntToStr(nval)) then
            begin
              plmnid := plmnid * 10 + nval;
              npos := npos + 1;
            end
          else
            begin
              npos := inputlen;
            end;
        end;
      // Search for PLMN operator brand, if known
      if plmnid < 20201 then
        begin
          result := mccmnc;
          exit;
        end;
      if plmnid < 50503 then
        begin
          if plmnid < 40407 then
            begin
              if plmnid < 25008 then
                begin
                  if plmnid < 23102 then
                    begin
                      if plmnid < 21601 then
                        begin
                          if plmnid < 20809 then
                            begin
                              if plmnid < 20408 then
                                begin
                                  if plmnid < 20210 then
                                    begin
                                      if plmnid = 20201 then
                                        begin
                                          result := 'Cosmote';
                                          exit;
                                        end;
                                      if plmnid = 20202 then
                                        begin
                                          result := 'Cosmote';
                                          exit;
                                        end;
                                      if plmnid = 20205 then
                                        begin
                                          result := 'Vodafone GR';
                                          exit;
                                        end;
                                      if plmnid = 20209 then
                                        begin
                                          result := 'Wind GR';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 20210 then
                                        begin
                                          result := 'Wind GR';
                                          exit;
                                        end;
                                      if plmnid = 20402 then
                                        begin
                                          result := 'Tele2 NL';
                                          exit;
                                        end;
                                      if plmnid = 20403 then
                                        begin
                                          result := 'Voiceworks';
                                          exit;
                                        end;
                                      if plmnid = 20404 then
                                        begin
                                          result := 'Vodafone NL';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 20601 then
                                    begin
                                      if plmnid = 20408 then
                                        begin
                                          result := 'KPN';
                                          exit;
                                        end;
                                      if plmnid = 20410 then
                                        begin
                                          result := 'KPN';
                                          exit;
                                        end;
                                      if plmnid = 20416 then
                                        begin
                                          result := 'T-Mobile (BEN)';
                                          exit;
                                        end;
                                      if plmnid = 20420 then
                                        begin
                                          result := 'T-Mobile NL';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 20620 then
                                        begin
                                          if plmnid = 20601 then
                                            begin
                                              result := 'Proximus';
                                              exit;
                                            end;
                                          if plmnid = 20610 then
                                            begin
                                              result := 'Orange Belgium';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 20620 then
                                            begin
                                              result := 'Base';
                                              exit;
                                            end;
                                          if plmnid = 20801 then
                                            begin
                                              result := 'Orange FR';
                                              exit;
                                            end;
                                          if plmnid = 20802 then
                                            begin
                                              result := 'Orange FR';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 20836 then
                                begin
                                  if plmnid < 20815 then
                                    begin
                                      if plmnid = 20809 then
                                        begin
                                          result := 'SFR';
                                          exit;
                                        end;
                                      if plmnid = 20810 then
                                        begin
                                          result := 'SFR';
                                          exit;
                                        end;
                                      if plmnid = 20813 then
                                        begin
                                          result := 'SFR';
                                          exit;
                                        end;
                                      if plmnid = 20814 then
                                        begin
                                          result := 'SNCF Réseau';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 20815 then
                                        begin
                                          result := 'Free FR';
                                          exit;
                                        end;
                                      if plmnid = 20816 then
                                        begin
                                          result := 'Free FR';
                                          exit;
                                        end;
                                      if plmnid = 20820 then
                                        begin
                                          result := 'Bouygues';
                                          exit;
                                        end;
                                      if plmnid = 20835 then
                                        begin
                                          result := 'Free FR';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 21401 then
                                    begin
                                      if plmnid = 20836 then
                                        begin
                                          result := 'Free FR';
                                          exit;
                                        end;
                                      if plmnid = 20888 then
                                        begin
                                          result := 'Bouygues';
                                          exit;
                                        end;
                                      if plmnid = 21210 then
                                        begin
                                          result := 'Office des Telephones';
                                          exit;
                                        end;
                                      if plmnid = 21303 then
                                        begin
                                          result := 'Som, Mobiland';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 21404 then
                                        begin
                                          if plmnid = 21401 then
                                            begin
                                              result := 'Vodafone ES';
                                              exit;
                                            end;
                                          if plmnid = 21403 then
                                            begin
                                              result := 'Orange ES';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 21404 then
                                            begin
                                              result := 'Yoigo';
                                              exit;
                                            end;
                                          if plmnid = 21407 then
                                            begin
                                              result := 'Movistar ES';
                                              exit;
                                            end;
                                          if plmnid = 21451 then
                                            begin
                                              result := 'ADIF';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 22210 then
                            begin
                              if plmnid < 21901 then
                                begin
                                  if plmnid < 21699 then
                                    begin
                                      if plmnid = 21601 then
                                        begin
                                          result := 'Telenor Hungary';
                                          exit;
                                        end;
                                      if plmnid = 21603 then
                                        begin
                                          result := 'DIGI';
                                          exit;
                                        end;
                                      if plmnid = 21630 then
                                        begin
                                          result := 'Telekom HU';
                                          exit;
                                        end;
                                      if plmnid = 21670 then
                                        begin
                                          result := 'Vodafone HU';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 21699 then
                                        begin
                                          result := 'MAV GSM-R';
                                          exit;
                                        end;
                                      if plmnid = 21803 then
                                        begin
                                          result := 'HT-ERONET';
                                          exit;
                                        end;
                                      if plmnid = 21805 then
                                        begin
                                          result := 'm:tel BiH';
                                          exit;
                                        end;
                                      if plmnid = 21890 then
                                        begin
                                          result := 'BH Mobile';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 22003 then
                                    begin
                                      if plmnid = 21901 then
                                        begin
                                          result := 'T-Mobile HR';
                                          exit;
                                        end;
                                      if plmnid = 21902 then
                                        begin
                                          result := 'Tele2 HR';
                                          exit;
                                        end;
                                      if plmnid = 21910 then
                                        begin
                                          result := 'A1 HR';
                                          exit;
                                        end;
                                      if plmnid = 22001 then
                                        begin
                                          result := 'Telenor RS';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 22101 then
                                        begin
                                          if plmnid = 22003 then
                                            begin
                                              result := 'mts';
                                              exit;
                                            end;
                                          if plmnid = 22005 then
                                            begin
                                              result := 'VIP';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 22101 then
                                            begin
                                              result := 'Vala';
                                              exit;
                                            end;
                                          if plmnid = 22102 then
                                            begin
                                              result := 'IPKO';
                                              exit;
                                            end;
                                          if plmnid = 22201 then
                                            begin
                                              result := 'TIM IT';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 22801 then
                                begin
                                  if plmnid < 22299 then
                                    begin
                                      if plmnid = 22210 then
                                        begin
                                          result := 'Vodafone IT';
                                          exit;
                                        end;
                                      if plmnid = 22230 then
                                        begin
                                          result := 'RFI';
                                          exit;
                                        end;
                                      if plmnid = 22250 then
                                        begin
                                          result := 'Iliad';
                                          exit;
                                        end;
                                      if plmnid = 22288 then
                                        begin
                                          result := 'Wind IT';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 22603 then
                                        begin
                                          if plmnid = 22299 then
                                            begin
                                              result := '3 Italia';
                                              exit;
                                            end;
                                          if plmnid = 22601 then
                                            begin
                                              result := 'Vodafone RO';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 22603 then
                                            begin
                                              result := 'Telekom RO';
                                              exit;
                                            end;
                                          if plmnid = 22605 then
                                            begin
                                              result := 'Digi.Mobil';
                                              exit;
                                            end;
                                          if plmnid = 22610 then
                                            begin
                                              result := 'Orange RO';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 22808 then
                                    begin
                                      if plmnid = 22801 then
                                        begin
                                          result := 'Swisscom CH';
                                          exit;
                                        end;
                                      if plmnid = 22802 then
                                        begin
                                          result := 'Sunrise';
                                          exit;
                                        end;
                                      if plmnid = 22803 then
                                        begin
                                          result := 'Salt';
                                          exit;
                                        end;
                                      if plmnid = 22806 then
                                        begin
                                          result := 'SBB-CFF-FFS';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 23002 then
                                        begin
                                          if plmnid = 22808 then
                                            begin
                                              result := 'Tele4u';
                                              exit;
                                            end;
                                          if plmnid = 23001 then
                                            begin
                                              result := 'T-Mobile CZ';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 23002 then
                                            begin
                                              result := 'O2 CZ';
                                              exit;
                                            end;
                                          if plmnid = 23003 then
                                            begin
                                              result := 'Vodafone CZ';
                                              exit;
                                            end;
                                          if plmnid = 23101 then
                                            begin
                                              result := 'Orange SK';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end
                  else
                    begin
                      if plmnid < 23458 then
                        begin
                          if plmnid < 23411 then
                            begin
                              if plmnid < 23205 then
                                begin
                                  if plmnid < 23106 then
                                    begin
                                      if plmnid = 23102 then
                                        begin
                                          result := 'Telekom SK';
                                          exit;
                                        end;
                                      if plmnid = 23103 then
                                        begin
                                          result := '4ka';
                                          exit;
                                        end;
                                      if plmnid = 23104 then
                                        begin
                                          result := 'Telekom SK';
                                          exit;
                                        end;
                                      if plmnid = 23105 then
                                        begin
                                          result := 'Orange SK';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 23106 then
                                        begin
                                          result := 'O2 SK';
                                          exit;
                                        end;
                                      if plmnid = 23199 then
                                        begin
                                          result := '?SR';
                                          exit;
                                        end;
                                      if plmnid = 23201 then
                                        begin
                                          result := 'A1.net';
                                          exit;
                                        end;
                                      if plmnid = 23203 then
                                        begin
                                          result := 'Magenta';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 23402 then
                                    begin
                                      if plmnid = 23205 then
                                        begin
                                          result := '3 AT';
                                          exit;
                                        end;
                                      if plmnid = 23210 then
                                        begin
                                          result := '3 AT';
                                          exit;
                                        end;
                                      if plmnid = 23291 then
                                        begin
                                          result := 'GSM-R A';
                                          exit;
                                        end;
                                      if plmnid = 23400 then
                                        begin
                                          result := 'BT';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 23403 then
                                        begin
                                          if plmnid = 23402 then
                                            begin
                                              result := 'O2 (UK)';
                                              exit;
                                            end;
                                          if plmnid = 23403 then
                                            begin
                                              result := 'Airtel-Vodafone GG';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 23403 then
                                            begin
                                              result := 'Airtel-Vodafone JE';
                                              exit;
                                            end;
                                          if plmnid = 23403 then
                                            begin
                                              result := 'Airtel-Vodafone GB';
                                              exit;
                                            end;
                                          if plmnid = 23410 then
                                            begin
                                              result := 'O2 (UK)';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 23436 then
                                begin
                                  if plmnid < 23419 then
                                    begin
                                      if plmnid = 23411 then
                                        begin
                                          result := 'O2 (UK)';
                                          exit;
                                        end;
                                      if plmnid = 23412 then
                                        begin
                                          result := 'Railtrack';
                                          exit;
                                        end;
                                      if plmnid = 23413 then
                                        begin
                                          result := 'Railtrack';
                                          exit;
                                        end;
                                      if plmnid = 23415 then
                                        begin
                                          result := 'Vodafone UK';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 23430 then
                                        begin
                                          if plmnid = 23419 then
                                            begin
                                              result := 'Private Mobile Networks PMN';
                                              exit;
                                            end;
                                          if plmnid = 23420 then
                                            begin
                                              result := '3 GB';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 23430 then
                                            begin
                                              result := 'T-Mobile UK';
                                              exit;
                                            end;
                                          if plmnid = 23433 then
                                            begin
                                              result := 'Orange GB';
                                              exit;
                                            end;
                                          if plmnid = 23434 then
                                            begin
                                              result := 'Orange GB';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 23450 then
                                    begin
                                      if plmnid = 23436 then
                                        begin
                                          result := 'Sure Mobile IM';
                                          exit;
                                        end;
                                      if plmnid = 23436 then
                                        begin
                                          result := 'Sure Mobile GB';
                                          exit;
                                        end;
                                      if plmnid = 23450 then
                                        begin
                                          result := 'JT GG';
                                          exit;
                                        end;
                                      if plmnid = 23450 then
                                        begin
                                          result := 'JT JE';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 23455 then
                                        begin
                                          if plmnid = 23450 then
                                            begin
                                              result := 'JT GB';
                                              exit;
                                            end;
                                          if plmnid = 23455 then
                                            begin
                                              result := 'Sure Mobile GG';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 23455 then
                                            begin
                                              result := 'Sure Mobile JE';
                                              exit;
                                            end;
                                          if plmnid = 23455 then
                                            begin
                                              result := 'Sure Mobile GB';
                                              exit;
                                            end;
                                          if plmnid = 23458 then
                                            begin
                                              result := 'Pronto GSM IM';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 24405 then
                            begin
                              if plmnid < 24001 then
                                begin
                                  if plmnid < 23806 then
                                    begin
                                      if plmnid = 23458 then
                                        begin
                                          result := 'Pronto GSM GB';
                                          exit;
                                        end;
                                      if plmnid = 23476 then
                                        begin
                                          result := 'BT';
                                          exit;
                                        end;
                                      if plmnid = 23801 then
                                        begin
                                          result := 'TDC';
                                          exit;
                                        end;
                                      if plmnid = 23802 then
                                        begin
                                          result := 'Telenor DK';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 23806 then
                                        begin
                                          result := '3 DK';
                                          exit;
                                        end;
                                      if plmnid = 23820 then
                                        begin
                                          result := 'Telia DK';
                                          exit;
                                        end;
                                      if plmnid = 23823 then
                                        begin
                                          result := 'GSM-R DK';
                                          exit;
                                        end;
                                      if plmnid = 23877 then
                                        begin
                                          result := 'Telenor DK';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 24024 then
                                    begin
                                      if plmnid = 24001 then
                                        begin
                                          result := 'Telia SE';
                                          exit;
                                        end;
                                      if plmnid = 24002 then
                                        begin
                                          result := '3 SE';
                                          exit;
                                        end;
                                      if plmnid = 24007 then
                                        begin
                                          result := 'Tele2 SE';
                                          exit;
                                        end;
                                      if plmnid = 24021 then
                                        begin
                                          result := 'MobiSir';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 24202 then
                                        begin
                                          if plmnid = 24024 then
                                            begin
                                              result := 'Sweden 2G';
                                              exit;
                                            end;
                                          if plmnid = 24201 then
                                            begin
                                              result := 'Telenor NO';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 24202 then
                                            begin
                                              result := 'Telia NO';
                                              exit;
                                            end;
                                          if plmnid = 24214 then
                                            begin
                                              result := 'ice';
                                              exit;
                                            end;
                                          if plmnid = 24403 then
                                            begin
                                              result := 'DNA';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 24605 then
                                begin
                                  if plmnid < 24436 then
                                    begin
                                      if plmnid = 24405 then
                                        begin
                                          result := 'Elisa FI';
                                          exit;
                                        end;
                                      if plmnid = 24407 then
                                        begin
                                          result := 'Nokia';
                                          exit;
                                        end;
                                      if plmnid = 24412 then
                                        begin
                                          result := 'DNA';
                                          exit;
                                        end;
                                      if plmnid = 24414 then
                                        begin
                                          result := 'Ålcom';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 24601 then
                                        begin
                                          if plmnid = 24436 then
                                            begin
                                              result := 'Telia / DNA';
                                              exit;
                                            end;
                                          if plmnid = 24491 then
                                            begin
                                              result := 'Telia FI';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 24601 then
                                            begin
                                              result := 'Telia LT';
                                              exit;
                                            end;
                                          if plmnid = 24602 then
                                            begin
                                              result := 'BIT?';
                                              exit;
                                            end;
                                          if plmnid = 24603 then
                                            begin
                                              result := 'Tele2 LT';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 24801 then
                                    begin
                                      if plmnid = 24605 then
                                        begin
                                          result := 'LitRail';
                                          exit;
                                        end;
                                      if plmnid = 24701 then
                                        begin
                                          result := 'LMT';
                                          exit;
                                        end;
                                      if plmnid = 24702 then
                                        begin
                                          result := 'Tele2 LV';
                                          exit;
                                        end;
                                      if plmnid = 24705 then
                                        begin
                                          result := 'Bite';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 24803 then
                                        begin
                                          if plmnid = 24801 then
                                            begin
                                              result := 'Telia EE';
                                              exit;
                                            end;
                                          if plmnid = 24802 then
                                            begin
                                              result := 'Elisa EE';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 24803 then
                                            begin
                                              result := 'Tele2 EE';
                                              exit;
                                            end;
                                          if plmnid = 25001 then
                                            begin
                                              result := 'MTS RU';
                                              exit;
                                            end;
                                          if plmnid = 25002 then
                                            begin
                                              result := 'MegaFon RU';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end
              else
                begin
                  if plmnid < 28405 then
                    begin
                      if plmnid < 26801 then
                        begin
                          if plmnid < 25706 then
                            begin
                              if plmnid < 25099 then
                                begin
                                  if plmnid < 25033 then
                                    begin
                                      if plmnid = 25008 then
                                        begin
                                          result := 'Vainah Telecom';
                                          exit;
                                        end;
                                      if plmnid = 25020 then
                                        begin
                                          result := 'Tele2 RU';
                                          exit;
                                        end;
                                      if plmnid = 25027 then
                                        begin
                                          result := 'Letai';
                                          exit;
                                        end;
                                      if plmnid = 25032 then
                                        begin
                                          result := 'Win Mobile';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 25033 then
                                        begin
                                          result := 'Sevmobile';
                                          exit;
                                        end;
                                      if plmnid = 25034 then
                                        begin
                                          result := 'Krymtelekom';
                                          exit;
                                        end;
                                      if plmnid = 25035 then
                                        begin
                                          result := 'MOTIV';
                                          exit;
                                        end;
                                      if plmnid = 25060 then
                                        begin
                                          result := 'Volna mobile';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 25506 then
                                    begin
                                      if plmnid = 25099 then
                                        begin
                                          result := 'Beeline RU';
                                          exit;
                                        end;
                                      if plmnid = 25501 then
                                        begin
                                          result := 'Vodafone UA';
                                          exit;
                                        end;
                                      if plmnid = 25502 then
                                        begin
                                          result := 'Kyivstar';
                                          exit;
                                        end;
                                      if plmnid = 25503 then
                                        begin
                                          result := 'Kyivstar';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 25701 then
                                        begin
                                          if plmnid = 25506 then
                                            begin
                                              result := 'lifecell';
                                              exit;
                                            end;
                                          if plmnid = 25599 then
                                            begin
                                              result := 'Phoenix UA';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 25701 then
                                            begin
                                              result := 'A1 BY';
                                              exit;
                                            end;
                                          if plmnid = 25702 then
                                            begin
                                              result := 'MTS BY';
                                              exit;
                                            end;
                                          if plmnid = 25704 then
                                            begin
                                              result := 'life:)';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 26015 then
                                begin
                                  if plmnid < 25915 then
                                    begin
                                      if plmnid = 25706 then
                                        begin
                                          result := 'beCloud';
                                          exit;
                                        end;
                                      if plmnid = 25901 then
                                        begin
                                          result := 'Orange MD';
                                          exit;
                                        end;
                                      if plmnid = 25902 then
                                        begin
                                          result := 'Moldcell';
                                          exit;
                                        end;
                                      if plmnid = 25905 then
                                        begin
                                          result := 'Unité';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 26002 then
                                        begin
                                          if plmnid = 25915 then
                                            begin
                                              result := 'IDC';
                                              exit;
                                            end;
                                          if plmnid = 26001 then
                                            begin
                                              result := 'Plus';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 26002 then
                                            begin
                                              result := 'T-Mobile PL';
                                              exit;
                                            end;
                                          if plmnid = 26003 then
                                            begin
                                              result := 'Orange PL';
                                              exit;
                                            end;
                                          if plmnid = 26006 then
                                            begin
                                              result := 'Play';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 26202 then
                                    begin
                                      if plmnid = 26015 then
                                        begin
                                          result := 'Aero2';
                                          exit;
                                        end;
                                      if plmnid = 26016 then
                                        begin
                                          result := 'Aero2';
                                          exit;
                                        end;
                                      if plmnid = 26034 then
                                        begin
                                          result := 'NetWorkS!';
                                          exit;
                                        end;
                                      if plmnid = 26201 then
                                        begin
                                          result := 'Telekom DE';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 26209 then
                                        begin
                                          if plmnid = 26202 then
                                            begin
                                              result := 'Vodafone DE';
                                              exit;
                                            end;
                                          if plmnid = 26203 then
                                            begin
                                              result := 'O2 DE';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 26209 then
                                            begin
                                              result := 'Vodafone DE';
                                              exit;
                                            end;
                                          if plmnid = 26601 then
                                            begin
                                              result := 'GibTel';
                                              exit;
                                            end;
                                          if plmnid = 26609 then
                                            begin
                                              result := 'Shine';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 27601 then
                            begin
                              if plmnid < 27202 then
                                begin
                                  if plmnid < 27071 then
                                    begin
                                      if plmnid = 26801 then
                                        begin
                                          result := 'Vodafone PT';
                                          exit;
                                        end;
                                      if plmnid = 26803 then
                                        begin
                                          result := 'NOS';
                                          exit;
                                        end;
                                      if plmnid = 26806 then
                                        begin
                                          result := 'MEO';
                                          exit;
                                        end;
                                      if plmnid = 27001 then
                                        begin
                                          result := 'POST';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 27071 then
                                        begin
                                          result := 'CFL';
                                          exit;
                                        end;
                                      if plmnid = 27077 then
                                        begin
                                          result := 'Tango';
                                          exit;
                                        end;
                                      if plmnid = 27099 then
                                        begin
                                          result := 'Orange LU';
                                          exit;
                                        end;
                                      if plmnid = 27201 then
                                        begin
                                          result := 'Vodafone IE';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 27401 then
                                    begin
                                      if plmnid = 27202 then
                                        begin
                                          result := '3 IE';
                                          exit;
                                        end;
                                      if plmnid = 27203 then
                                        begin
                                          result := 'Eir';
                                          exit;
                                        end;
                                      if plmnid = 27205 then
                                        begin
                                          result := '3 IE';
                                          exit;
                                        end;
                                      if plmnid = 27207 then
                                        begin
                                          result := 'Eir';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 27404 then
                                        begin
                                          if plmnid = 27401 then
                                            begin
                                              result := 'Síminn';
                                              exit;
                                            end;
                                          if plmnid = 27402 then
                                            begin
                                              result := 'Vodafone IS';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 27404 then
                                            begin
                                              result := 'Viking';
                                              exit;
                                            end;
                                          if plmnid = 27408 then
                                            begin
                                              result := 'On-waves';
                                              exit;
                                            end;
                                          if plmnid = 27411 then
                                            begin
                                              result := 'Nova';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 28201 then
                                begin
                                  if plmnid < 27821 then
                                    begin
                                      if plmnid = 27601 then
                                        begin
                                          result := 'Telekom.al';
                                          exit;
                                        end;
                                      if plmnid = 27602 then
                                        begin
                                          result := 'Vodafone AL';
                                          exit;
                                        end;
                                      if plmnid = 27603 then
                                        begin
                                          result := 'Eagle Mobile';
                                          exit;
                                        end;
                                      if plmnid = 27801 then
                                        begin
                                          result := 'Vodafone MT';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 28001 then
                                        begin
                                          if plmnid = 27821 then
                                            begin
                                              result := 'GO';
                                              exit;
                                            end;
                                          if plmnid = 27877 then
                                            begin
                                              result := 'Melita';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 28001 then
                                            begin
                                              result := 'Cytamobile-Vodafone';
                                              exit;
                                            end;
                                          if plmnid = 28010 then
                                            begin
                                              result := 'Epic';
                                              exit;
                                            end;
                                          if plmnid = 28020 then
                                            begin
                                              result := 'PrimeTel';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 28304 then
                                    begin
                                      if plmnid = 28201 then
                                        begin
                                          result := 'Geocell';
                                          exit;
                                        end;
                                      if plmnid = 28202 then
                                        begin
                                          result := 'Magti';
                                          exit;
                                        end;
                                      if plmnid = 28204 then
                                        begin
                                          result := 'Beeline GE';
                                          exit;
                                        end;
                                      if plmnid = 28301 then
                                        begin
                                          result := 'Beeline AM';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 28310 then
                                        begin
                                          if plmnid = 28304 then
                                            begin
                                              result := 'Karabakh Telecom';
                                              exit;
                                            end;
                                          if plmnid = 28305 then
                                            begin
                                              result := 'VivaCell-MTS';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 28310 then
                                            begin
                                              result := 'Ucom';
                                              exit;
                                            end;
                                          if plmnid = 28401 then
                                            begin
                                              result := 'A1 BG';
                                              exit;
                                            end;
                                          if plmnid = 28403 then
                                            begin
                                              result := 'Vivacom';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end
                  else
                    begin
                      if plmnid < 36251 then
                        begin
                          if plmnid < 29501 then
                            begin
                              if plmnid < 28967 then
                                begin
                                  if plmnid < 28602 then
                                    begin
                                      if plmnid = 28405 then
                                        begin
                                          result := 'Telenor BG';
                                          exit;
                                        end;
                                      if plmnid = 28407 then
                                        begin
                                          result := '????';
                                          exit;
                                        end;
                                      if plmnid = 28413 then
                                        begin
                                          result := '??.???';
                                          exit;
                                        end;
                                      if plmnid = 28601 then
                                        begin
                                          result := 'Turkcell';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 28602 then
                                        begin
                                          result := 'Vodafone TR';
                                          exit;
                                        end;
                                      if plmnid = 28603 then
                                        begin
                                          result := 'Türk Telekom';
                                          exit;
                                        end;
                                      if plmnid = 28801 then
                                        begin
                                          result := 'Føroya Tele';
                                          exit;
                                        end;
                                      if plmnid = 28802 then
                                        begin
                                          result := 'Hey';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 29341 then
                                    begin
                                      if plmnid = 28967 then
                                        begin
                                          result := 'Aquafon';
                                          exit;
                                        end;
                                      if plmnid = 28988 then
                                        begin
                                          result := 'A-Mobile';
                                          exit;
                                        end;
                                      if plmnid = 29201 then
                                        begin
                                          result := 'PRIMA';
                                          exit;
                                        end;
                                      if plmnid = 29340 then
                                        begin
                                          result := 'A1 SI';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 29401 then
                                        begin
                                          if plmnid = 29341 then
                                            begin
                                              result := 'Mobitel SI';
                                              exit;
                                            end;
                                          if plmnid = 29370 then
                                            begin
                                              result := 'Telemach';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 29401 then
                                            begin
                                              result := 'Telekom.mk';
                                              exit;
                                            end;
                                          if plmnid = 29402 then
                                            begin
                                              result := 'vip';
                                              exit;
                                            end;
                                          if plmnid = 29403 then
                                            begin
                                              result := 'vip';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 34001 then
                                begin
                                  if plmnid < 29702 then
                                    begin
                                      if plmnid = 29501 then
                                        begin
                                          result := 'Swisscom LI';
                                          exit;
                                        end;
                                      if plmnid = 29502 then
                                        begin
                                          result := '7acht';
                                          exit;
                                        end;
                                      if plmnid = 29505 then
                                        begin
                                          result := 'FL1';
                                          exit;
                                        end;
                                      if plmnid = 29701 then
                                        begin
                                          result := 'Telenor ME';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 30801 then
                                        begin
                                          if plmnid = 29702 then
                                            begin
                                              result := 'T-Mobile ME';
                                              exit;
                                            end;
                                          if plmnid = 29703 then
                                            begin
                                              result := 'm:tel CG';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 30801 then
                                            begin
                                              result := 'Ameris';
                                              exit;
                                            end;
                                          if plmnid = 30802 then
                                            begin
                                              result := 'GLOBALTEL';
                                              exit;
                                            end;
                                          if plmnid = 34001 then
                                            begin
                                              result := 'Orange BL/GF/GP/MF/MQ';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 34008 then
                                    begin
                                      if plmnid = 34001 then
                                        begin
                                          result := 'Orange GF';
                                          exit;
                                        end;
                                      if plmnid = 34002 then
                                        begin
                                          result := 'SFR Caraïbe BL/GF/GP/MF/MQ';
                                          exit;
                                        end;
                                      if plmnid = 34002 then
                                        begin
                                          result := 'SFR Caraïbe GF';
                                          exit;
                                        end;
                                      if plmnid = 34003 then
                                        begin
                                          result := 'Chippie BL/GF/GP/MF/MQ';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 34020 then
                                        begin
                                          if plmnid = 34008 then
                                            begin
                                              result := 'Dauphin';
                                              exit;
                                            end;
                                          if plmnid = 34020 then
                                            begin
                                              result := 'Digicel BL/GF/GP/MF/MQ';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 34020 then
                                            begin
                                              result := 'Digicel GF';
                                              exit;
                                            end;
                                          if plmnid = 35000 then
                                            begin
                                              result := 'One';
                                              exit;
                                            end;
                                          if plmnid = 35002 then
                                            begin
                                              result := 'Mobility';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 37202 then
                            begin
                              if plmnid < 36291 then
                                begin
                                  if plmnid < 36268 then
                                    begin
                                      if plmnid = 36251 then
                                        begin
                                          result := 'Telcell';
                                          exit;
                                        end;
                                      if plmnid = 36254 then
                                        begin
                                          result := 'ECC';
                                          exit;
                                        end;
                                      if plmnid = 36259 then
                                        begin
                                          result := 'Chippie BQ/CW/SX';
                                          exit;
                                        end;
                                      if plmnid = 36260 then
                                        begin
                                          result := 'Chippie BQ/CW/SX';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 36268 then
                                        begin
                                          result := 'Digicel BQ/CW/SX';
                                          exit;
                                        end;
                                      if plmnid = 36269 then
                                        begin
                                          result := 'Digicel BQ/CW/SX';
                                          exit;
                                        end;
                                      if plmnid = 36276 then
                                        begin
                                          result := 'Digicel BQ/CW/SX';
                                          exit;
                                        end;
                                      if plmnid = 36278 then
                                        begin
                                          result := 'Telbo';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 36449 then
                                    begin
                                      if plmnid = 36291 then
                                        begin
                                          result := 'Chippie BQ/CW/SX';
                                          exit;
                                        end;
                                      if plmnid = 36301 then
                                        begin
                                          result := 'SETAR';
                                          exit;
                                        end;
                                      if plmnid = 36302 then
                                        begin
                                          result := 'Digicel AW';
                                          exit;
                                        end;
                                      if plmnid = 36439 then
                                        begin
                                          result := 'BTC';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 37001 then
                                        begin
                                          if plmnid = 36449 then
                                            begin
                                              result := 'Aliv';
                                              exit;
                                            end;
                                          if plmnid = 36801 then
                                            begin
                                              result := 'CUBACEL';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 37001 then
                                            begin
                                              result := 'Altice';
                                              exit;
                                            end;
                                          if plmnid = 37002 then
                                            begin
                                              result := 'Claro DO';
                                              exit;
                                            end;
                                          if plmnid = 37004 then
                                            begin
                                              result := 'Viva DO';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 40107 then
                                begin
                                  if plmnid < 40002 then
                                    begin
                                      if plmnid = 37202 then
                                        begin
                                          result := 'Digicel HT';
                                          exit;
                                        end;
                                      if plmnid = 37203 then
                                        begin
                                          result := 'Natcom';
                                          exit;
                                        end;
                                      if plmnid = 37412 then
                                        begin
                                          result := 'bmobile TT';
                                          exit;
                                        end;
                                      if plmnid = 40001 then
                                        begin
                                          result := 'Azercell';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 40006 then
                                        begin
                                          if plmnid = 40002 then
                                            begin
                                              result := 'Bakcell';
                                              exit;
                                            end;
                                          if plmnid = 40004 then
                                            begin
                                              result := 'Nar Mobile';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 40006 then
                                            begin
                                              result := 'Naxtel';
                                              exit;
                                            end;
                                          if plmnid = 40101 then
                                            begin
                                              result := 'Beeline KZ';
                                              exit;
                                            end;
                                          if plmnid = 40102 then
                                            begin
                                              result := 'Kcell';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 40401 then
                                    begin
                                      if plmnid = 40107 then
                                        begin
                                          result := 'Altel';
                                          exit;
                                        end;
                                      if plmnid = 40177 then
                                        begin
                                          result := 'Tele2.kz';
                                          exit;
                                        end;
                                      if plmnid = 40211 then
                                        begin
                                          result := 'B-Mobile';
                                          exit;
                                        end;
                                      if plmnid = 40277 then
                                        begin
                                          result := 'TashiCell';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 40403 then
                                        begin
                                          if plmnid = 40401 then
                                            begin
                                              result := 'Vodafone India';
                                              exit;
                                            end;
                                          if plmnid = 40402 then
                                            begin
                                              result := 'AirTel';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 40403 then
                                            begin
                                              result := 'AirTel';
                                              exit;
                                            end;
                                          if plmnid = 40404 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                          if plmnid = 40405 then
                                            begin
                                              result := 'Vodafone India';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            end
          else
            begin
              if plmnid < 42001 then
                begin
                  if plmnid < 40493 then
                    begin
                      if plmnid < 40450 then
                        begin
                          if plmnid < 40427 then
                            begin
                              if plmnid < 40416 then
                                begin
                                  if plmnid < 40412 then
                                    begin
                                      if plmnid = 40407 then
                                        begin
                                          result := 'IDEA';
                                          exit;
                                        end;
                                      if plmnid = 40409 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40410 then
                                        begin
                                          result := 'AirTel';
                                          exit;
                                        end;
                                      if plmnid = 40411 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 40412 then
                                        begin
                                          result := 'IDEA';
                                          exit;
                                        end;
                                      if plmnid = 40413 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                      if plmnid = 40414 then
                                        begin
                                          result := 'IDEA';
                                          exit;
                                        end;
                                      if plmnid = 40415 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 40420 then
                                    begin
                                      if plmnid = 40416 then
                                        begin
                                          result := 'Airtel IN';
                                          exit;
                                        end;
                                      if plmnid = 40417 then
                                        begin
                                          result := 'AIRCEL';
                                          exit;
                                        end;
                                      if plmnid = 40418 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40419 then
                                        begin
                                          result := 'IDEA';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 40422 then
                                        begin
                                          if plmnid = 40420 then
                                            begin
                                              result := 'Vodafone India';
                                              exit;
                                            end;
                                          if plmnid = 40421 then
                                            begin
                                              result := 'Loop Mobile';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 40422 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                          if plmnid = 40424 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                          if plmnid = 40425 then
                                            begin
                                              result := 'AIRCEL';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 40438 then
                                begin
                                  if plmnid < 40431 then
                                    begin
                                      if plmnid = 40427 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                      if plmnid = 40428 then
                                        begin
                                          result := 'AIRCEL';
                                          exit;
                                        end;
                                      if plmnid = 40429 then
                                        begin
                                          result := 'AIRCEL';
                                          exit;
                                        end;
                                      if plmnid = 40430 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 40435 then
                                        begin
                                          if plmnid = 40431 then
                                            begin
                                              result := 'AirTel';
                                              exit;
                                            end;
                                          if plmnid = 40434 then
                                            begin
                                              result := 'cellone';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 40435 then
                                            begin
                                              result := 'Aircel';
                                              exit;
                                            end;
                                          if plmnid = 40436 then
                                            begin
                                              result := 'Reliance';
                                              exit;
                                            end;
                                          if plmnid = 40437 then
                                            begin
                                              result := 'Aircel';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 40444 then
                                    begin
                                      if plmnid = 40438 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                      if plmnid = 40441 then
                                        begin
                                          result := 'Aircel';
                                          exit;
                                        end;
                                      if plmnid = 40442 then
                                        begin
                                          result := 'Aircel';
                                          exit;
                                        end;
                                      if plmnid = 40443 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 40446 then
                                        begin
                                          if plmnid = 40444 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                          if plmnid = 40445 then
                                            begin
                                              result := 'Airtel IN';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 40446 then
                                            begin
                                              result := 'Vodafone India';
                                              exit;
                                            end;
                                          if plmnid = 40448 then
                                            begin
                                              result := 'Dishnet Wireless';
                                              exit;
                                            end;
                                          if plmnid = 40449 then
                                            begin
                                              result := 'Airtel IN';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 40471 then
                            begin
                              if plmnid < 40458 then
                                begin
                                  if plmnid < 40454 then
                                    begin
                                      if plmnid = 40450 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40451 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                      if plmnid = 40452 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40453 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 40454 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                      if plmnid = 40455 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                      if plmnid = 40456 then
                                        begin
                                          result := 'IDEA';
                                          exit;
                                        end;
                                      if plmnid = 40457 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 40464 then
                                    begin
                                      if plmnid = 40458 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                      if plmnid = 40459 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                      if plmnid = 40460 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                      if plmnid = 40462 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 40467 then
                                        begin
                                          if plmnid = 40464 then
                                            begin
                                              result := 'cellone';
                                              exit;
                                            end;
                                          if plmnid = 40466 then
                                            begin
                                              result := 'cellone';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 40467 then
                                            begin
                                              result := 'Reliance';
                                              exit;
                                            end;
                                          if plmnid = 40468 then
                                            begin
                                              result := 'DOLPHIN';
                                              exit;
                                            end;
                                          if plmnid = 40469 then
                                            begin
                                              result := 'DOLPHIN';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 40480 then
                                begin
                                  if plmnid < 40475 then
                                    begin
                                      if plmnid = 40471 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                      if plmnid = 40472 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                      if plmnid = 40473 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                      if plmnid = 40474 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 40477 then
                                        begin
                                          if plmnid = 40475 then
                                            begin
                                              result := 'cellone';
                                              exit;
                                            end;
                                          if plmnid = 40476 then
                                            begin
                                              result := 'cellone';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 40477 then
                                            begin
                                              result := 'cellone';
                                              exit;
                                            end;
                                          if plmnid = 40478 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                          if plmnid = 40479 then
                                            begin
                                              result := 'cellone';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 40485 then
                                    begin
                                      if plmnid = 40480 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                      if plmnid = 40481 then
                                        begin
                                          result := 'cellone';
                                          exit;
                                        end;
                                      if plmnid = 40483 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40484 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 40490 then
                                        begin
                                          if plmnid = 40485 then
                                            begin
                                              result := 'Reliance';
                                              exit;
                                            end;
                                          if plmnid = 40486 then
                                            begin
                                              result := 'Vodafone India';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 40490 then
                                            begin
                                              result := 'AirTel';
                                              exit;
                                            end;
                                          if plmnid = 40491 then
                                            begin
                                              result := 'AIRCEL';
                                              exit;
                                            end;
                                          if plmnid = 40492 then
                                            begin
                                              result := 'AirTel';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end
                  else
                    begin
                      if plmnid < 41004 then
                        begin
                          if plmnid < 40517 then
                            begin
                              if plmnid < 40507 then
                                begin
                                  if plmnid < 40503 then
                                    begin
                                      if plmnid = 40493 then
                                        begin
                                          result := 'AirTel';
                                          exit;
                                        end;
                                      if plmnid = 40495 then
                                        begin
                                          result := 'AirTel';
                                          exit;
                                        end;
                                      if plmnid = 40496 then
                                        begin
                                          result := 'AirTel';
                                          exit;
                                        end;
                                      if plmnid = 40501 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 40503 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40504 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40505 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40506 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 40511 then
                                    begin
                                      if plmnid = 40507 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40508 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40509 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40510 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 40513 then
                                        begin
                                          if plmnid = 40511 then
                                            begin
                                              result := 'Reliance';
                                              exit;
                                            end;
                                          if plmnid = 40512 then
                                            begin
                                              result := 'Reliance';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 40513 then
                                            begin
                                              result := 'Reliance';
                                              exit;
                                            end;
                                          if plmnid = 40514 then
                                            begin
                                              result := 'Reliance';
                                              exit;
                                            end;
                                          if plmnid = 40515 then
                                            begin
                                              result := 'Reliance';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 40553 then
                                begin
                                  if plmnid < 40521 then
                                    begin
                                      if plmnid = 40517 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40518 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40519 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                      if plmnid = 40520 then
                                        begin
                                          result := 'Reliance';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 40523 then
                                        begin
                                          if plmnid = 40521 then
                                            begin
                                              result := 'Reliance';
                                              exit;
                                            end;
                                          if plmnid = 40522 then
                                            begin
                                              result := 'Reliance';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 40523 then
                                            begin
                                              result := 'Reliance';
                                              exit;
                                            end;
                                          if plmnid = 40551 then
                                            begin
                                              result := 'AirTel';
                                              exit;
                                            end;
                                          if plmnid = 40552 then
                                            begin
                                              result := 'AirTel';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 40566 then
                                    begin
                                      if plmnid = 40553 then
                                        begin
                                          result := 'AirTel';
                                          exit;
                                        end;
                                      if plmnid = 40554 then
                                        begin
                                          result := 'AirTel';
                                          exit;
                                        end;
                                      if plmnid = 40555 then
                                        begin
                                          result := 'Airtel IN';
                                          exit;
                                        end;
                                      if plmnid = 40556 then
                                        begin
                                          result := 'AirTel';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 41001 then
                                        begin
                                          if plmnid = 40566 then
                                            begin
                                              result := 'Vodafone India';
                                              exit;
                                            end;
                                          if plmnid = 40570 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 41001 then
                                            begin
                                              result := 'Jazz';
                                              exit;
                                            end;
                                          if plmnid = 41002 then
                                            begin
                                              result := '3G EVO / CharJi 4G';
                                              exit;
                                            end;
                                          if plmnid = 41003 then
                                            begin
                                              result := 'Ufone';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 41406 then
                            begin
                              if plmnid < 41250 then
                                begin
                                  if plmnid < 41008 then
                                    begin
                                      if plmnid = 41004 then
                                        begin
                                          result := 'Zong';
                                          exit;
                                        end;
                                      if plmnid = 41005 then
                                        begin
                                          result := 'SCO Mobile';
                                          exit;
                                        end;
                                      if plmnid = 41006 then
                                        begin
                                          result := 'Telenor PK';
                                          exit;
                                        end;
                                      if plmnid = 41007 then
                                        begin
                                          result := 'Jazz';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 41008 then
                                        begin
                                          result := 'SCO Mobile';
                                          exit;
                                        end;
                                      if plmnid = 41201 then
                                        begin
                                          result := 'AWCC';
                                          exit;
                                        end;
                                      if plmnid = 41220 then
                                        begin
                                          result := 'Roshan';
                                          exit;
                                        end;
                                      if plmnid = 41240 then
                                        begin
                                          result := 'MTN AF';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 41302 then
                                    begin
                                      if plmnid = 41250 then
                                        begin
                                          result := 'Etisalat AF';
                                          exit;
                                        end;
                                      if plmnid = 41280 then
                                        begin
                                          result := 'Salaam';
                                          exit;
                                        end;
                                      if plmnid = 41288 then
                                        begin
                                          result := 'Salaam';
                                          exit;
                                        end;
                                      if plmnid = 41301 then
                                        begin
                                          result := 'Mobitel LK';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 41309 then
                                        begin
                                          if plmnid = 41302 then
                                            begin
                                              result := 'Dialog';
                                              exit;
                                            end;
                                          if plmnid = 41305 then
                                            begin
                                              result := 'Airtel LK';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 41309 then
                                            begin
                                              result := 'Hutch';
                                              exit;
                                            end;
                                          if plmnid = 41401 then
                                            begin
                                              result := 'MPT';
                                              exit;
                                            end;
                                          if plmnid = 41405 then
                                            begin
                                              result := 'Ooredoo MM';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 41800 then
                                begin
                                  if plmnid < 41601 then
                                    begin
                                      if plmnid = 41406 then
                                        begin
                                          result := 'Telenor MM';
                                          exit;
                                        end;
                                      if plmnid = 41409 then
                                        begin
                                          result := 'Mytel';
                                          exit;
                                        end;
                                      if plmnid = 41501 then
                                        begin
                                          result := 'Alfa';
                                          exit;
                                        end;
                                      if plmnid = 41503 then
                                        begin
                                          result := 'Touch';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 41677 then
                                        begin
                                          if plmnid = 41601 then
                                            begin
                                              result := 'zain JO';
                                              exit;
                                            end;
                                          if plmnid = 41603 then
                                            begin
                                              result := 'Umniah';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 41677 then
                                            begin
                                              result := 'Orange JO';
                                              exit;
                                            end;
                                          if plmnid = 41701 then
                                            begin
                                              result := 'Syriatel';
                                              exit;
                                            end;
                                          if plmnid = 41702 then
                                            begin
                                              result := 'MTN SY';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 41830 then
                                    begin
                                      if plmnid = 41800 then
                                        begin
                                          result := 'Asia Cell';
                                          exit;
                                        end;
                                      if plmnid = 41805 then
                                        begin
                                          result := 'Asia Cell';
                                          exit;
                                        end;
                                      if plmnid = 41808 then
                                        begin
                                          result := 'SanaTel';
                                          exit;
                                        end;
                                      if plmnid = 41820 then
                                        begin
                                          result := 'Zain IQ';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 41902 then
                                        begin
                                          if plmnid = 41830 then
                                            begin
                                              result := 'Zain IQ';
                                              exit;
                                            end;
                                          if plmnid = 41840 then
                                            begin
                                              result := 'Korek';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 41902 then
                                            begin
                                              result := 'zain KW';
                                              exit;
                                            end;
                                          if plmnid = 41903 then
                                            begin
                                              result := 'K.S.C Ooredoo';
                                              exit;
                                            end;
                                          if plmnid = 41904 then
                                            begin
                                              result := 'STC KW';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end
              else
                begin
                  if plmnid < 44054 then
                    begin
                      if plmnid < 43211 then
                        begin
                          if plmnid < 42506 then
                            begin
                              if plmnid < 42203 then
                                begin
                                  if plmnid < 42101 then
                                    begin
                                      if plmnid = 42001 then
                                        begin
                                          result := 'Al Jawal (STC )';
                                          exit;
                                        end;
                                      if plmnid = 42003 then
                                        begin
                                          result := 'Mobily';
                                          exit;
                                        end;
                                      if plmnid = 42004 then
                                        begin
                                          result := 'Zain SA';
                                          exit;
                                        end;
                                      if plmnid = 42021 then
                                        begin
                                          result := 'RGSM';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 42101 then
                                        begin
                                          result := 'SabaFon';
                                          exit;
                                        end;
                                      if plmnid = 42102 then
                                        begin
                                          result := 'MTN YE';
                                          exit;
                                        end;
                                      if plmnid = 42104 then
                                        begin
                                          result := 'Y';
                                          exit;
                                        end;
                                      if plmnid = 42202 then
                                        begin
                                          result := 'Omantel';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 42502 then
                                    begin
                                      if plmnid = 42203 then
                                        begin
                                          result := 'ooredoo OM';
                                          exit;
                                        end;
                                      if plmnid = 42402 then
                                        begin
                                          result := 'Etisalat AE';
                                          exit;
                                        end;
                                      if plmnid = 42403 then
                                        begin
                                          result := 'du';
                                          exit;
                                        end;
                                      if plmnid = 42501 then
                                        begin
                                          result := 'Partner';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 42505 then
                                        begin
                                          if plmnid = 42502 then
                                            begin
                                              result := 'Cellcom IL';
                                              exit;
                                            end;
                                          if plmnid = 42503 then
                                            begin
                                              result := 'Pelephone';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 42505 then
                                            begin
                                              result := 'Jawwal IL';
                                              exit;
                                            end;
                                          if plmnid = 42505 then
                                            begin
                                              result := 'Jawwal PS';
                                              exit;
                                            end;
                                          if plmnid = 42506 then
                                            begin
                                              result := 'Wataniya Mobile';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 42701 then
                                begin
                                  if plmnid < 42510 then
                                    begin
                                      if plmnid = 42506 then
                                        begin
                                          result := 'Wataniya';
                                          exit;
                                        end;
                                      if plmnid = 42507 then
                                        begin
                                          result := 'Hot Mobile';
                                          exit;
                                        end;
                                      if plmnid = 42508 then
                                        begin
                                          result := 'Golan Telecom';
                                          exit;
                                        end;
                                      if plmnid = 42509 then
                                        begin
                                          result := 'We4G';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 42602 then
                                        begin
                                          if plmnid = 42510 then
                                            begin
                                              result := 'Partner';
                                              exit;
                                            end;
                                          if plmnid = 42601 then
                                            begin
                                              result := 'Batelco';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 42602 then
                                            begin
                                              result := 'zain BH';
                                              exit;
                                            end;
                                          if plmnid = 42604 then
                                            begin
                                              result := 'STC BH';
                                              exit;
                                            end;
                                          if plmnid = 42605 then
                                            begin
                                              result := 'Batelco';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 42898 then
                                    begin
                                      if plmnid = 42701 then
                                        begin
                                          result := 'ooredoo QA';
                                          exit;
                                        end;
                                      if plmnid = 42702 then
                                        begin
                                          result := 'Vodafone QA';
                                          exit;
                                        end;
                                      if plmnid = 42888 then
                                        begin
                                          result := 'Unitel MN';
                                          exit;
                                        end;
                                      if plmnid = 42891 then
                                        begin
                                          result := 'Skytel';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 42901 then
                                        begin
                                          if plmnid = 42898 then
                                            begin
                                              result := 'G-Mobile';
                                              exit;
                                            end;
                                          if plmnid = 42899 then
                                            begin
                                              result := 'Mobicom';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 42901 then
                                            begin
                                              result := 'Namaste / NT Mobile / Sky Phone';
                                              exit;
                                            end;
                                          if plmnid = 42902 then
                                            begin
                                              result := 'Ncell';
                                              exit;
                                            end;
                                          if plmnid = 42904 then
                                            begin
                                              result := 'SmartCell';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 43408 then
                            begin
                              if plmnid < 43270 then
                                begin
                                  if plmnid < 43220 then
                                    begin
                                      if plmnid = 43211 then
                                        begin
                                          result := 'IR-TCI (Hamrah-e-Avval)';
                                          exit;
                                        end;
                                      if plmnid = 43212 then
                                        begin
                                          result := 'Avacell(HiWEB)';
                                          exit;
                                        end;
                                      if plmnid = 43214 then
                                        begin
                                          result := 'TKC/KFZO';
                                          exit;
                                        end;
                                      if plmnid = 43219 then
                                        begin
                                          result := 'Espadan (JV-PJS)';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 43220 then
                                        begin
                                          result := 'RighTel';
                                          exit;
                                        end;
                                      if plmnid = 43221 then
                                        begin
                                          result := 'RighTel';
                                          exit;
                                        end;
                                      if plmnid = 43232 then
                                        begin
                                          result := 'Taliya';
                                          exit;
                                        end;
                                      if plmnid = 43235 then
                                        begin
                                          result := 'MTN Irancell';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 43293 then
                                    begin
                                      if plmnid = 43270 then
                                        begin
                                          result := 'MTCE';
                                          exit;
                                        end;
                                      if plmnid = 43271 then
                                        begin
                                          result := 'KOOHE NOOR';
                                          exit;
                                        end;
                                      if plmnid = 43290 then
                                        begin
                                          result := 'Iraphone';
                                          exit;
                                        end;
                                      if plmnid = 43293 then
                                        begin
                                          result := 'Iraphone';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 43404 then
                                        begin
                                          if plmnid = 43293 then
                                            begin
                                              result := 'Farzanegan Pars';
                                              exit;
                                            end;
                                          if plmnid = 43299 then
                                            begin
                                              result := 'TCI (GSM WLL)';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 43404 then
                                            begin
                                              result := 'Beeline UZ';
                                              exit;
                                            end;
                                          if plmnid = 43405 then
                                            begin
                                              result := 'Ucell';
                                              exit;
                                            end;
                                          if plmnid = 43407 then
                                            begin
                                              result := 'Mobiuz';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 43802 then
                                begin
                                  if plmnid < 43604 then
                                    begin
                                      if plmnid = 43408 then
                                        begin
                                          result := 'UzMobile';
                                          exit;
                                        end;
                                      if plmnid = 43601 then
                                        begin
                                          result := 'Tcell';
                                          exit;
                                        end;
                                      if plmnid = 43602 then
                                        begin
                                          result := 'Tcell';
                                          exit;
                                        end;
                                      if plmnid = 43603 then
                                        begin
                                          result := 'MegaFon TJ';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 43701 then
                                        begin
                                          if plmnid = 43604 then
                                            begin
                                              result := 'Babilon-M';
                                              exit;
                                            end;
                                          if plmnid = 43605 then
                                            begin
                                              result := 'ZET-Mobile';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 43701 then
                                            begin
                                              result := 'Beeline KG';
                                              exit;
                                            end;
                                          if plmnid = 43705 then
                                            begin
                                              result := 'MegaCom';
                                              exit;
                                            end;
                                          if plmnid = 43709 then
                                            begin
                                              result := 'O!';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 44021 then
                                    begin
                                      if plmnid = 43802 then
                                        begin
                                          result := 'TM-Cell';
                                          exit;
                                        end;
                                      if plmnid = 44010 then
                                        begin
                                          result := 'NTT docomo';
                                          exit;
                                        end;
                                      if plmnid = 44011 then
                                        begin
                                          result := 'Rakuten Mobile';
                                          exit;
                                        end;
                                      if plmnid = 44020 then
                                        begin
                                          result := 'SoftBank';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 44051 then
                                        begin
                                          if plmnid = 44021 then
                                            begin
                                              result := 'SoftBank';
                                              exit;
                                            end;
                                          if plmnid = 44050 then
                                            begin
                                              result := 'au';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 44051 then
                                            begin
                                              result := 'au';
                                              exit;
                                            end;
                                          if plmnid = 44052 then
                                            begin
                                              result := 'au';
                                              exit;
                                            end;
                                          if plmnid = 44053 then
                                            begin
                                              result := 'au';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end
                  else
                    begin
                      if plmnid < 45606 then
                        begin
                          if plmnid < 45205 then
                            begin
                              if plmnid < 44101 then
                                begin
                                  if plmnid < 44073 then
                                    begin
                                      if plmnid = 44054 then
                                        begin
                                          result := 'au';
                                          exit;
                                        end;
                                      if plmnid = 44070 then
                                        begin
                                          result := 'au';
                                          exit;
                                        end;
                                      if plmnid = 44071 then
                                        begin
                                          result := 'au';
                                          exit;
                                        end;
                                      if plmnid = 44072 then
                                        begin
                                          result := 'au';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 44073 then
                                        begin
                                          result := 'au';
                                          exit;
                                        end;
                                      if plmnid = 44074 then
                                        begin
                                          result := 'au';
                                          exit;
                                        end;
                                      if plmnid = 44075 then
                                        begin
                                          result := 'au';
                                          exit;
                                        end;
                                      if plmnid = 44076 then
                                        begin
                                          result := 'au';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 45008 then
                                    begin
                                      if plmnid = 44101 then
                                        begin
                                          result := 'SoftBank';
                                          exit;
                                        end;
                                      if plmnid = 45004 then
                                        begin
                                          result := 'KT';
                                          exit;
                                        end;
                                      if plmnid = 45005 then
                                        begin
                                          result := 'SKTelecom';
                                          exit;
                                        end;
                                      if plmnid = 45006 then
                                        begin
                                          result := 'LG U+';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 45201 then
                                        begin
                                          if plmnid = 45008 then
                                            begin
                                              result := 'olleh';
                                              exit;
                                            end;
                                          if plmnid = 45012 then
                                            begin
                                              result := 'SKTelecom';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 45201 then
                                            begin
                                              result := 'MobiFone';
                                              exit;
                                            end;
                                          if plmnid = 45202 then
                                            begin
                                              result := 'Vinaphone';
                                              exit;
                                            end;
                                          if plmnid = 45204 then
                                            begin
                                              result := 'Viettel Mobile';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 45500 then
                                begin
                                  if plmnid < 45404 then
                                    begin
                                      if plmnid = 45205 then
                                        begin
                                          result := 'Vietnamobile';
                                          exit;
                                        end;
                                      if plmnid = 45207 then
                                        begin
                                          result := 'Gmobile';
                                          exit;
                                        end;
                                      if plmnid = 45400 then
                                        begin
                                          result := '1O1O / One2Free / New World Mobility / SUNMobile';
                                          exit;
                                        end;
                                      if plmnid = 45403 then
                                        begin
                                          result := '3 HK';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 45412 then
                                        begin
                                          if plmnid = 45404 then
                                            begin
                                              result := '3 (2G)';
                                              exit;
                                            end;
                                          if plmnid = 45406 then
                                            begin
                                              result := 'SmarTone HK';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 45412 then
                                            begin
                                              result := 'CMCC HK';
                                              exit;
                                            end;
                                          if plmnid = 45416 then
                                            begin
                                              result := 'PCCW Mobile (2G)';
                                              exit;
                                            end;
                                          if plmnid = 45420 then
                                            begin
                                              result := 'PCCW Mobile (4G)';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 45601 then
                                    begin
                                      if plmnid = 45500 then
                                        begin
                                          result := 'SmarTone MO';
                                          exit;
                                        end;
                                      if plmnid = 45501 then
                                        begin
                                          result := 'CTM';
                                          exit;
                                        end;
                                      if plmnid = 45505 then
                                        begin
                                          result := '3 MO';
                                          exit;
                                        end;
                                      if plmnid = 45507 then
                                        begin
                                          result := 'China Telecom MO';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 45603 then
                                        begin
                                          if plmnid = 45601 then
                                            begin
                                              result := 'Cellcard';
                                              exit;
                                            end;
                                          if plmnid = 45602 then
                                            begin
                                              result := 'Smart KH';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 45603 then
                                            begin
                                              result := 'qb';
                                              exit;
                                            end;
                                          if plmnid = 45604 then
                                            begin
                                              result := 'qb';
                                              exit;
                                            end;
                                          if plmnid = 45605 then
                                            begin
                                              result := 'Smart KH';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 46697 then
                            begin
                              if plmnid < 45708 then
                                begin
                                  if plmnid < 45618 then
                                    begin
                                      if plmnid = 45606 then
                                        begin
                                          result := 'Smart KH';
                                          exit;
                                        end;
                                      if plmnid = 45608 then
                                        begin
                                          result := 'Metfone';
                                          exit;
                                        end;
                                      if plmnid = 45609 then
                                        begin
                                          result := 'Metfone';
                                          exit;
                                        end;
                                      if plmnid = 45611 then
                                        begin
                                          result := 'SEATEL';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 45618 then
                                        begin
                                          result := 'Cellcard';
                                          exit;
                                        end;
                                      if plmnid = 45701 then
                                        begin
                                          result := 'LaoTel';
                                          exit;
                                        end;
                                      if plmnid = 45702 then
                                        begin
                                          result := 'ETL';
                                          exit;
                                        end;
                                      if plmnid = 45703 then
                                        begin
                                          result := 'Unitel LA';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 46020 then
                                    begin
                                      if plmnid = 45708 then
                                        begin
                                          result := 'Beeline LA';
                                          exit;
                                        end;
                                      if plmnid = 46000 then
                                        begin
                                          result := 'China Mobile';
                                          exit;
                                        end;
                                      if plmnid = 46001 then
                                        begin
                                          result := 'China Unicom';
                                          exit;
                                        end;
                                      if plmnid = 46003 then
                                        begin
                                          result := 'China Telecom CN';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 46605 then
                                        begin
                                          if plmnid = 46020 then
                                            begin
                                              result := 'China Tietong';
                                              exit;
                                            end;
                                          if plmnid = 46601 then
                                            begin
                                              result := 'FarEasTone';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 46605 then
                                            begin
                                              result := 'APTG';
                                              exit;
                                            end;
                                          if plmnid = 46689 then
                                            begin
                                              result := 'T Star';
                                              exit;
                                            end;
                                          if plmnid = 46692 then
                                            begin
                                              result := 'Chunghwa';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 50211 then
                                begin
                                  if plmnid < 47004 then
                                    begin
                                      if plmnid = 46697 then
                                        begin
                                          result := 'Taiwan Mobile';
                                          exit;
                                        end;
                                      if plmnid = 47001 then
                                        begin
                                          result := 'Grameenphone';
                                          exit;
                                        end;
                                      if plmnid = 47002 then
                                        begin
                                          result := 'Robi';
                                          exit;
                                        end;
                                      if plmnid = 47003 then
                                        begin
                                          result := 'Banglalink';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 47009 then
                                        begin
                                          if plmnid = 47004 then
                                            begin
                                              result := 'TeleTalk';
                                              exit;
                                            end;
                                          if plmnid = 47007 then
                                            begin
                                              result := 'Airtel BD';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 47009 then
                                            begin
                                              result := 'ollo';
                                              exit;
                                            end;
                                          if plmnid = 47201 then
                                            begin
                                              result := 'Dhiraagu';
                                              exit;
                                            end;
                                          if plmnid = 47202 then
                                            begin
                                              result := 'Ooredoo MV';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 50217 then
                                    begin
                                      if plmnid = 50211 then
                                        begin
                                          result := 'TM Homeline';
                                          exit;
                                        end;
                                      if plmnid = 50212 then
                                        begin
                                          result := 'Maxis';
                                          exit;
                                        end;
                                      if plmnid = 50213 then
                                        begin
                                          result := 'Celcom';
                                          exit;
                                        end;
                                      if plmnid = 50216 then
                                        begin
                                          result := 'DiGi';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 50219 then
                                        begin
                                          if plmnid = 50217 then
                                            begin
                                              result := 'Maxis';
                                              exit;
                                            end;
                                          if plmnid = 50218 then
                                            begin
                                              result := 'U Mobile';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 50219 then
                                            begin
                                              result := 'Celcom';
                                              exit;
                                            end;
                                          if plmnid = 50501 then
                                            begin
                                              result := 'Telstra';
                                              exit;
                                            end;
                                          if plmnid = 50502 then
                                            begin
                                              result := 'Optus';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            end;
        end
      else
        begin
          if plmnid < 72402 then
            begin
              if plmnid < 62303 then
                begin
                  if plmnid < 60501 then
                    begin
                      if plmnid < 53901 then
                        begin
                          if plmnid < 52001 then
                            begin
                              if plmnid < 51011 then
                                begin
                                  if plmnid < 50516 then
                                    begin
                                      if plmnid = 50503 then
                                        begin
                                          result := 'Vodafone AU';
                                          exit;
                                        end;
                                      if plmnid = 50510 then
                                        begin
                                          result := 'Norfolk Is.';
                                          exit;
                                        end;
                                      if plmnid = 50510 then
                                        begin
                                          result := 'Norfolk Telecom';
                                          exit;
                                        end;
                                      if plmnid = 50513 then
                                        begin
                                          result := 'RailCorp';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 50516 then
                                        begin
                                          result := 'VicTrack';
                                          exit;
                                        end;
                                      if plmnid = 51001 then
                                        begin
                                          result := 'Indosat Ooredoo';
                                          exit;
                                        end;
                                      if plmnid = 51009 then
                                        begin
                                          result := 'Smartfren';
                                          exit;
                                        end;
                                      if plmnid = 51010 then
                                        begin
                                          result := 'Telkomsel';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 51402 then
                                    begin
                                      if plmnid = 51011 then
                                        begin
                                          result := 'XL';
                                          exit;
                                        end;
                                      if plmnid = 51028 then
                                        begin
                                          result := 'Fren/Hepi';
                                          exit;
                                        end;
                                      if plmnid = 51089 then
                                        begin
                                          result := '3 ID';
                                          exit;
                                        end;
                                      if plmnid = 51401 then
                                        begin
                                          result := 'Telkomcel';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 51502 then
                                        begin
                                          if plmnid = 51402 then
                                            begin
                                              result := 'TT';
                                              exit;
                                            end;
                                          if plmnid = 51403 then
                                            begin
                                              result := 'Telemor';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 51502 then
                                            begin
                                              result := 'Globe';
                                              exit;
                                            end;
                                          if plmnid = 51503 then
                                            begin
                                              result := 'SMART PH';
                                              exit;
                                            end;
                                          if plmnid = 51505 then
                                            begin
                                              result := 'Sun Cellular';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 52505 then
                                begin
                                  if plmnid < 52018 then
                                    begin
                                      if plmnid = 52001 then
                                        begin
                                          result := 'AIS';
                                          exit;
                                        end;
                                      if plmnid = 52003 then
                                        begin
                                          result := 'AIS';
                                          exit;
                                        end;
                                      if plmnid = 52004 then
                                        begin
                                          result := 'TrueMove H';
                                          exit;
                                        end;
                                      if plmnid = 52005 then
                                        begin
                                          result := 'dtac';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 52018 then
                                        begin
                                          result := 'dtac';
                                          exit;
                                        end;
                                      if plmnid = 52099 then
                                        begin
                                          result := 'TrueMove';
                                          exit;
                                        end;
                                      if plmnid = 52501 then
                                        begin
                                          result := 'SingTel';
                                          exit;
                                        end;
                                      if plmnid = 52503 then
                                        begin
                                          result := 'M1';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 53024 then
                                    begin
                                      if plmnid = 52505 then
                                        begin
                                          result := 'StarHub';
                                          exit;
                                        end;
                                      if plmnid = 52811 then
                                        begin
                                          result := 'DST';
                                          exit;
                                        end;
                                      if plmnid = 53001 then
                                        begin
                                          result := 'Vodafone NZ';
                                          exit;
                                        end;
                                      if plmnid = 53005 then
                                        begin
                                          result := 'Spark';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 53701 then
                                        begin
                                          if plmnid = 53024 then
                                            begin
                                              result := '2degrees';
                                              exit;
                                            end;
                                          if plmnid = 53602 then
                                            begin
                                              result := 'Digicel NR';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 53701 then
                                            begin
                                              result := 'bmobile PG';
                                              exit;
                                            end;
                                          if plmnid = 53702 then
                                            begin
                                              result := 'citifon';
                                              exit;
                                            end;
                                          if plmnid = 53703 then
                                            begin
                                              result := 'Digicel PG';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 54801 then
                            begin
                              if plmnid < 54202 then
                                begin
                                  if plmnid < 54100 then
                                    begin
                                      if plmnid = 53901 then
                                        begin
                                          result := 'U-Call';
                                          exit;
                                        end;
                                      if plmnid = 53988 then
                                        begin
                                          result := 'Digicel TO';
                                          exit;
                                        end;
                                      if plmnid = 54001 then
                                        begin
                                          result := 'BREEZE';
                                          exit;
                                        end;
                                      if plmnid = 54002 then
                                        begin
                                          result := 'BeMobile';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 54100 then
                                        begin
                                          result := 'AIL';
                                          exit;
                                        end;
                                      if plmnid = 54101 then
                                        begin
                                          result := 'SMILE';
                                          exit;
                                        end;
                                      if plmnid = 54105 then
                                        begin
                                          result := 'Digicel VU';
                                          exit;
                                        end;
                                      if plmnid = 54201 then
                                        begin
                                          result := 'Vodafone FJ';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 54509 then
                                    begin
                                      if plmnid = 54202 then
                                        begin
                                          result := 'Digicel FJ';
                                          exit;
                                        end;
                                      if plmnid = 54203 then
                                        begin
                                          result := 'TFL';
                                          exit;
                                        end;
                                      if plmnid = 54411 then
                                        begin
                                          result := 'Bluesky AS';
                                          exit;
                                        end;
                                      if plmnid = 54501 then
                                        begin
                                          result := 'Kiribati - ATH';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 54705 then
                                        begin
                                          if plmnid = 54509 then
                                            begin
                                              result := 'Kiribati - Frigate Net';
                                              exit;
                                            end;
                                          if plmnid = 54601 then
                                            begin
                                              result := 'Mobilis NC';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 54705 then
                                            begin
                                              result := 'Ora';
                                              exit;
                                            end;
                                          if plmnid = 54715 then
                                            begin
                                              result := 'Vodafone PF';
                                              exit;
                                            end;
                                          if plmnid = 54720 then
                                            begin
                                              result := 'Vini';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 60203 then
                                begin
                                  if plmnid < 55202 then
                                    begin
                                      if plmnid = 54801 then
                                        begin
                                          result := 'Bluesky CK';
                                          exit;
                                        end;
                                      if plmnid = 54901 then
                                        begin
                                          result := 'Digicel WS';
                                          exit;
                                        end;
                                      if plmnid = 54927 then
                                        begin
                                          result := 'Bluesky WS';
                                          exit;
                                        end;
                                      if plmnid = 55201 then
                                        begin
                                          result := 'PNCC';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 55501 then
                                        begin
                                          if plmnid = 55202 then
                                            begin
                                              result := 'PT Waves';
                                              exit;
                                            end;
                                          if plmnid = 55301 then
                                            begin
                                              result := 'TTC';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 55501 then
                                            begin
                                              result := 'Telecom Niue';
                                              exit;
                                            end;
                                          if plmnid = 60201 then
                                            begin
                                              result := 'Orange EG';
                                              exit;
                                            end;
                                          if plmnid = 60202 then
                                            begin
                                              result := 'Vodafone EG';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 60303 then
                                    begin
                                      if plmnid = 60203 then
                                        begin
                                          result := 'Etisalat EG';
                                          exit;
                                        end;
                                      if plmnid = 60204 then
                                        begin
                                          result := 'WE';
                                          exit;
                                        end;
                                      if plmnid = 60301 then
                                        begin
                                          result := 'Mobilis DZ';
                                          exit;
                                        end;
                                      if plmnid = 60302 then
                                        begin
                                          result := 'Djezzy';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 60401 then
                                        begin
                                          if plmnid = 60303 then
                                            begin
                                              result := 'Ooredoo DZ';
                                              exit;
                                            end;
                                          if plmnid = 60400 then
                                            begin
                                              result := 'Orange Morocco';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 60401 then
                                            begin
                                              result := 'IAM';
                                              exit;
                                            end;
                                          if plmnid = 60402 then
                                            begin
                                              result := 'INWI';
                                              exit;
                                            end;
                                          if plmnid = 60405 then
                                            begin
                                              result := 'INWI';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end
                  else
                    begin
                      if plmnid < 61403 then
                        begin
                          if plmnid < 61002 then
                            begin
                              if plmnid < 60703 then
                                begin
                                  if plmnid < 60601 then
                                    begin
                                      if plmnid = 60501 then
                                        begin
                                          result := 'Orange TN';
                                          exit;
                                        end;
                                      if plmnid = 60502 then
                                        begin
                                          result := 'Tunicell';
                                          exit;
                                        end;
                                      if plmnid = 60503 then
                                        begin
                                          result := 'OOREDOO TN';
                                          exit;
                                        end;
                                      if plmnid = 60600 then
                                        begin
                                          result := 'Libyana';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 60601 then
                                        begin
                                          result := 'Madar';
                                          exit;
                                        end;
                                      if plmnid = 60603 then
                                        begin
                                          result := 'Libya Phone';
                                          exit;
                                        end;
                                      if plmnid = 60701 then
                                        begin
                                          result := 'Gamcel';
                                          exit;
                                        end;
                                      if plmnid = 60702 then
                                        begin
                                          result := 'Africell GM';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 60803 then
                                    begin
                                      if plmnid = 60703 then
                                        begin
                                          result := 'Comium';
                                          exit;
                                        end;
                                      if plmnid = 60704 then
                                        begin
                                          result := 'QCell';
                                          exit;
                                        end;
                                      if plmnid = 60801 then
                                        begin
                                          result := 'Orange SN';
                                          exit;
                                        end;
                                      if plmnid = 60802 then
                                        begin
                                          result := 'Free SN';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 60902 then
                                        begin
                                          if plmnid = 60803 then
                                            begin
                                              result := 'Expresso';
                                              exit;
                                            end;
                                          if plmnid = 60901 then
                                            begin
                                              result := 'Mattel';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 60902 then
                                            begin
                                              result := 'Chinguitel';
                                              exit;
                                            end;
                                          if plmnid = 60910 then
                                            begin
                                              result := 'Mauritel';
                                              exit;
                                            end;
                                          if plmnid = 61001 then
                                            begin
                                              result := 'Malitel';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 61204 then
                                begin
                                  if plmnid < 61103 then
                                    begin
                                      if plmnid = 61002 then
                                        begin
                                          result := 'Orange ML';
                                          exit;
                                        end;
                                      if plmnid = 61003 then
                                        begin
                                          result := 'Telecel ML';
                                          exit;
                                        end;
                                      if plmnid = 61101 then
                                        begin
                                          result := 'Orange GN';
                                          exit;
                                        end;
                                      if plmnid = 61102 then
                                        begin
                                          result := 'Sotelgui';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 61105 then
                                        begin
                                          if plmnid = 61103 then
                                            begin
                                              result := 'Telecel Guinee';
                                              exit;
                                            end;
                                          if plmnid = 61104 then
                                            begin
                                              result := 'MTN GN';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 61105 then
                                            begin
                                              result := 'Cellcom GN';
                                              exit;
                                            end;
                                          if plmnid = 61202 then
                                            begin
                                              result := 'Moov CI';
                                              exit;
                                            end;
                                          if plmnid = 61203 then
                                            begin
                                              result := 'Orange CI';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 61301 then
                                    begin
                                      if plmnid = 61204 then
                                        begin
                                          result := 'KoZ';
                                          exit;
                                        end;
                                      if plmnid = 61205 then
                                        begin
                                          result := 'MTN CI';
                                          exit;
                                        end;
                                      if plmnid = 61206 then
                                        begin
                                          result := 'GreenN';
                                          exit;
                                        end;
                                      if plmnid = 61207 then
                                        begin
                                          result := 'café';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 61303 then
                                        begin
                                          if plmnid = 61301 then
                                            begin
                                              result := 'Telmob';
                                              exit;
                                            end;
                                          if plmnid = 61302 then
                                            begin
                                              result := 'Orange BF';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 61303 then
                                            begin
                                              result := 'Telecel Faso';
                                              exit;
                                            end;
                                          if plmnid = 61401 then
                                            begin
                                              result := 'SahelCom';
                                              exit;
                                            end;
                                          if plmnid = 61402 then
                                            begin
                                              result := 'Airtel NE';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 61909 then
                            begin
                              if plmnid < 61701 then
                                begin
                                  if plmnid < 61601 then
                                    begin
                                      if plmnid = 61403 then
                                        begin
                                          result := 'Moov NE';
                                          exit;
                                        end;
                                      if plmnid = 61404 then
                                        begin
                                          result := 'Orange NE';
                                          exit;
                                        end;
                                      if plmnid = 61501 then
                                        begin
                                          result := 'Togo Cell';
                                          exit;
                                        end;
                                      if plmnid = 61503 then
                                        begin
                                          result := 'Moov TG';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 61601 then
                                        begin
                                          result := 'Libercom';
                                          exit;
                                        end;
                                      if plmnid = 61602 then
                                        begin
                                          result := 'Moov BJ';
                                          exit;
                                        end;
                                      if plmnid = 61603 then
                                        begin
                                          result := 'MTN BJ';
                                          exit;
                                        end;
                                      if plmnid = 61604 then
                                        begin
                                          result := 'BBCOM';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 61804 then
                                    begin
                                      if plmnid = 61701 then
                                        begin
                                          result := 'my.t';
                                          exit;
                                        end;
                                      if plmnid = 61703 then
                                        begin
                                          result := 'CHILI';
                                          exit;
                                        end;
                                      if plmnid = 61710 then
                                        begin
                                          result := 'Emtel';
                                          exit;
                                        end;
                                      if plmnid = 61801 then
                                        begin
                                          result := 'Lonestar Cell MTN';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 61901 then
                                        begin
                                          if plmnid = 61804 then
                                            begin
                                              result := 'Novafone';
                                              exit;
                                            end;
                                          if plmnid = 61807 then
                                            begin
                                              result := 'Orange LBR';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 61901 then
                                            begin
                                              result := 'Orange SL';
                                              exit;
                                            end;
                                          if plmnid = 61903 then
                                            begin
                                              result := 'Africell SL';
                                              exit;
                                            end;
                                          if plmnid = 61905 then
                                            begin
                                              result := 'Africell SL';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 62130 then
                                begin
                                  if plmnid < 62006 then
                                    begin
                                      if plmnid = 61909 then
                                        begin
                                          result := 'Smart Mobile SL';
                                          exit;
                                        end;
                                      if plmnid = 62001 then
                                        begin
                                          result := 'MTN GH';
                                          exit;
                                        end;
                                      if plmnid = 62002 then
                                        begin
                                          result := 'Vodafone GH';
                                          exit;
                                        end;
                                      if plmnid = 62003 then
                                        begin
                                          result := 'AirtelTigo';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 62120 then
                                        begin
                                          if plmnid = 62006 then
                                            begin
                                              result := 'AirtelTigo';
                                              exit;
                                            end;
                                          if plmnid = 62007 then
                                            begin
                                              result := 'Globacom';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 62120 then
                                            begin
                                              result := 'Airtel NG';
                                              exit;
                                            end;
                                          if plmnid = 62122 then
                                            begin
                                              result := 'InterC';
                                              exit;
                                            end;
                                          if plmnid = 62127 then
                                            begin
                                              result := 'Smile NG';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 62201 then
                                    begin
                                      if plmnid = 62130 then
                                        begin
                                          result := 'MTN NG';
                                          exit;
                                        end;
                                      if plmnid = 62140 then
                                        begin
                                          result := 'Ntel';
                                          exit;
                                        end;
                                      if plmnid = 62150 then
                                        begin
                                          result := 'Glo';
                                          exit;
                                        end;
                                      if plmnid = 62160 then
                                        begin
                                          result := '9mobile';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 62207 then
                                        begin
                                          if plmnid = 62201 then
                                            begin
                                              result := 'Airtel TD';
                                              exit;
                                            end;
                                          if plmnid = 62203 then
                                            begin
                                              result := 'Tigo TD';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 62207 then
                                            begin
                                              result := 'Salam';
                                              exit;
                                            end;
                                          if plmnid = 62301 then
                                            begin
                                              result := 'Moov CF';
                                              exit;
                                            end;
                                          if plmnid = 62302 then
                                            begin
                                              result := 'TC';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end
              else
                begin
                  if plmnid < 64201 then
                    begin
                      if plmnid < 63409 then
                        begin
                          if plmnid < 62910 then
                            begin
                              if plmnid < 62601 then
                                begin
                                  if plmnid < 62403 then
                                    begin
                                      if plmnid = 62303 then
                                        begin
                                          result := 'Orange CF';
                                          exit;
                                        end;
                                      if plmnid = 62304 then
                                        begin
                                          result := 'Azur';
                                          exit;
                                        end;
                                      if plmnid = 62401 then
                                        begin
                                          result := 'MTN Cameroon';
                                          exit;
                                        end;
                                      if plmnid = 62402 then
                                        begin
                                          result := 'Orange CM';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 62403 then
                                        begin
                                          result := 'Camtel';
                                          exit;
                                        end;
                                      if plmnid = 62404 then
                                        begin
                                          result := 'Nexttel';
                                          exit;
                                        end;
                                      if plmnid = 62501 then
                                        begin
                                          result := 'CVMOVEL';
                                          exit;
                                        end;
                                      if plmnid = 62502 then
                                        begin
                                          result := 'T+';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 62801 then
                                    begin
                                      if plmnid = 62601 then
                                        begin
                                          result := 'CSTmovel';
                                          exit;
                                        end;
                                      if plmnid = 62602 then
                                        begin
                                          result := 'Unitel STP';
                                          exit;
                                        end;
                                      if plmnid = 62701 then
                                        begin
                                          result := 'Orange GQ';
                                          exit;
                                        end;
                                      if plmnid = 62703 then
                                        begin
                                          result := 'Muni';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 62803 then
                                        begin
                                          if plmnid = 62801 then
                                            begin
                                              result := 'Libertis';
                                              exit;
                                            end;
                                          if plmnid = 62802 then
                                            begin
                                              result := 'Moov GA';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 62803 then
                                            begin
                                              result := 'Airtel GA';
                                              exit;
                                            end;
                                          if plmnid = 62901 then
                                            begin
                                              result := 'Airtel CG';
                                              exit;
                                            end;
                                          if plmnid = 62907 then
                                            begin
                                              result := 'Airtel CG';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 63201 then
                                begin
                                  if plmnid < 63086 then
                                    begin
                                      if plmnid = 62910 then
                                        begin
                                          result := 'Libertis Telecom';
                                          exit;
                                        end;
                                      if plmnid = 63001 then
                                        begin
                                          result := 'Vodacom CD';
                                          exit;
                                        end;
                                      if plmnid = 63002 then
                                        begin
                                          result := 'Airtel CD';
                                          exit;
                                        end;
                                      if plmnid = 63005 then
                                        begin
                                          result := 'Supercell';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 63090 then
                                        begin
                                          if plmnid = 63086 then
                                            begin
                                              result := 'Orange RDC';
                                              exit;
                                            end;
                                          if plmnid = 63089 then
                                            begin
                                              result := 'Orange RDC';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 63090 then
                                            begin
                                              result := 'Africell CD';
                                              exit;
                                            end;
                                          if plmnid = 63102 then
                                            begin
                                              result := 'UNITEL AO';
                                              exit;
                                            end;
                                          if plmnid = 63104 then
                                            begin
                                              result := 'MOVICEL';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 63301 then
                                    begin
                                      if plmnid = 63201 then
                                        begin
                                          result := 'Guinetel';
                                          exit;
                                        end;
                                      if plmnid = 63202 then
                                        begin
                                          result := 'MTN Areeba';
                                          exit;
                                        end;
                                      if plmnid = 63203 then
                                        begin
                                          result := 'Orange GW';
                                          exit;
                                        end;
                                      if plmnid = 63207 then
                                        begin
                                          result := 'Guinetel';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 63401 then
                                        begin
                                          if plmnid = 63301 then
                                            begin
                                              result := 'Cable & Wireless SC';
                                              exit;
                                            end;
                                          if plmnid = 63310 then
                                            begin
                                              result := 'Airtel SC';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 63401 then
                                            begin
                                              result := 'Zain SD';
                                              exit;
                                            end;
                                          if plmnid = 63402 then
                                            begin
                                              result := 'MTN SD';
                                              exit;
                                            end;
                                          if plmnid = 63407 then
                                            begin
                                              result := 'Sudani One';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 63902 then
                            begin
                              if plmnid < 63720 then
                                begin
                                  if plmnid < 63601 then
                                    begin
                                      if plmnid = 63409 then
                                        begin
                                          result := 'khartoum INC';
                                          exit;
                                        end;
                                      if plmnid = 63510 then
                                        begin
                                          result := 'MTN RW';
                                          exit;
                                        end;
                                      if plmnid = 63513 then
                                        begin
                                          result := 'Airtel RW';
                                          exit;
                                        end;
                                      if plmnid = 63517 then
                                        begin
                                          result := 'Olleh';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 63601 then
                                        begin
                                          result := 'MTN ET';
                                          exit;
                                        end;
                                      if plmnid = 63701 then
                                        begin
                                          result := 'Telesom';
                                          exit;
                                        end;
                                      if plmnid = 63704 then
                                        begin
                                          result := 'Somafone';
                                          exit;
                                        end;
                                      if plmnid = 63710 then
                                        begin
                                          result := 'Nationlink';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 63760 then
                                    begin
                                      if plmnid = 63720 then
                                        begin
                                          result := 'SOMNET';
                                          exit;
                                        end;
                                      if plmnid = 63730 then
                                        begin
                                          result := 'Golis';
                                          exit;
                                        end;
                                      if plmnid = 63750 then
                                        begin
                                          result := 'Hormuud';
                                          exit;
                                        end;
                                      if plmnid = 63757 then
                                        begin
                                          result := 'UNITEL SO';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 63771 then
                                        begin
                                          if plmnid = 63760 then
                                            begin
                                              result := 'Nationlink';
                                              exit;
                                            end;
                                          if plmnid = 63767 then
                                            begin
                                              result := 'Horntel Group';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 63771 then
                                            begin
                                              result := 'Somtel';
                                              exit;
                                            end;
                                          if plmnid = 63782 then
                                            begin
                                              result := 'Telcom';
                                              exit;
                                            end;
                                          if plmnid = 63801 then
                                            begin
                                              result := 'Evatis';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 64009 then
                                begin
                                  if plmnid < 64002 then
                                    begin
                                      if plmnid = 63902 then
                                        begin
                                          result := 'Safaricom';
                                          exit;
                                        end;
                                      if plmnid = 63903 then
                                        begin
                                          result := 'Airtel KE';
                                          exit;
                                        end;
                                      if plmnid = 63907 then
                                        begin
                                          result := 'Telkom KE';
                                          exit;
                                        end;
                                      if plmnid = 63910 then
                                        begin
                                          result := 'Faiba 4G';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 64004 then
                                        begin
                                          if plmnid = 64002 then
                                            begin
                                              result := 'tiGO';
                                              exit;
                                            end;
                                          if plmnid = 64003 then
                                            begin
                                              result := 'Zantel';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 64004 then
                                            begin
                                              result := 'Vodacom TZ';
                                              exit;
                                            end;
                                          if plmnid = 64005 then
                                            begin
                                              result := 'Airtel TZ';
                                              exit;
                                            end;
                                          if plmnid = 64007 then
                                            begin
                                              result := 'TTCL Mobile';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 64111 then
                                    begin
                                      if plmnid = 64009 then
                                        begin
                                          result := 'Halotel';
                                          exit;
                                        end;
                                      if plmnid = 64011 then
                                        begin
                                          result := 'SmileCom';
                                          exit;
                                        end;
                                      if plmnid = 64101 then
                                        begin
                                          result := 'Airtel UG';
                                          exit;
                                        end;
                                      if plmnid = 64110 then
                                        begin
                                          result := 'MTN UG';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 64118 then
                                        begin
                                          if plmnid = 64111 then
                                            begin
                                              result := 'Uganda Telecom';
                                              exit;
                                            end;
                                          if plmnid = 64114 then
                                            begin
                                              result := 'Africell UG';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 64118 then
                                            begin
                                              result := 'Smart UG';
                                              exit;
                                            end;
                                          if plmnid = 64122 then
                                            begin
                                              result := 'Airtel UG';
                                              exit;
                                            end;
                                          if plmnid = 64133 then
                                            begin
                                              result := 'Smile UG';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end
                  else
                    begin
                      if plmnid < 65501 then
                        begin
                          if plmnid < 64703 then
                            begin
                              if plmnid < 64501 then
                                begin
                                  if plmnid < 64282 then
                                    begin
                                      if plmnid = 64201 then
                                        begin
                                          result := 'econet Leo';
                                          exit;
                                        end;
                                      if plmnid = 64203 then
                                        begin
                                          result := 'Onatel';
                                          exit;
                                        end;
                                      if plmnid = 64207 then
                                        begin
                                          result := 'Smart Mobile BI';
                                          exit;
                                        end;
                                      if plmnid = 64208 then
                                        begin
                                          result := 'Lumitel';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 64282 then
                                        begin
                                          result := 'econet Leo';
                                          exit;
                                        end;
                                      if plmnid = 64301 then
                                        begin
                                          result := 'mCel';
                                          exit;
                                        end;
                                      if plmnid = 64303 then
                                        begin
                                          result := 'Movitel';
                                          exit;
                                        end;
                                      if plmnid = 64304 then
                                        begin
                                          result := 'Vodacom MZ';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 64602 then
                                    begin
                                      if plmnid = 64501 then
                                        begin
                                          result := 'Airtel ZM';
                                          exit;
                                        end;
                                      if plmnid = 64502 then
                                        begin
                                          result := 'MTN ZM';
                                          exit;
                                        end;
                                      if plmnid = 64503 then
                                        begin
                                          result := 'ZAMTEL';
                                          exit;
                                        end;
                                      if plmnid = 64601 then
                                        begin
                                          result := 'Airtel MG';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 64700 then
                                        begin
                                          if plmnid = 64602 then
                                            begin
                                              result := 'Orange MG';
                                              exit;
                                            end;
                                          if plmnid = 64604 then
                                            begin
                                              result := 'Telma';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 64700 then
                                            begin
                                              result := 'Orange YT/RE';
                                              exit;
                                            end;
                                          if plmnid = 64701 then
                                            begin
                                              result := 'Maoré Mobile';
                                              exit;
                                            end;
                                          if plmnid = 64702 then
                                            begin
                                              result := 'Only';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 65010 then
                                begin
                                  if plmnid < 64804 then
                                    begin
                                      if plmnid = 64703 then
                                        begin
                                          result := 'Free YT/RE';
                                          exit;
                                        end;
                                      if plmnid = 64710 then
                                        begin
                                          result := 'SFR Réunion';
                                          exit;
                                        end;
                                      if plmnid = 64801 then
                                        begin
                                          result := 'Net*One';
                                          exit;
                                        end;
                                      if plmnid = 64803 then
                                        begin
                                          result := 'Telecel ZW';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 64903 then
                                        begin
                                          if plmnid = 64804 then
                                            begin
                                              result := 'Econet';
                                              exit;
                                            end;
                                          if plmnid = 64901 then
                                            begin
                                              result := 'MTC';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 64903 then
                                            begin
                                              result := 'TN Mobile';
                                              exit;
                                            end;
                                          if plmnid = 65001 then
                                            begin
                                              result := 'TNM';
                                              exit;
                                            end;
                                          if plmnid = 65002 then
                                            begin
                                              result := 'Access';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 65202 then
                                    begin
                                      if plmnid = 65010 then
                                        begin
                                          result := 'Airtel MW';
                                          exit;
                                        end;
                                      if plmnid = 65101 then
                                        begin
                                          result := 'Vodacom LS';
                                          exit;
                                        end;
                                      if plmnid = 65102 then
                                        begin
                                          result := 'Econet Telecom';
                                          exit;
                                        end;
                                      if plmnid = 65201 then
                                        begin
                                          result := 'Mascom';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 65310 then
                                        begin
                                          if plmnid = 65202 then
                                            begin
                                              result := 'Orange BW';
                                              exit;
                                            end;
                                          if plmnid = 65204 then
                                            begin
                                              result := 'beMobile';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 65310 then
                                            begin
                                              result := 'Swazi MTN';
                                              exit;
                                            end;
                                          if plmnid = 65401 then
                                            begin
                                              result := 'HURI';
                                              exit;
                                            end;
                                          if plmnid = 65402 then
                                            begin
                                              result := 'TELCO SA';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 70602 then
                            begin
                              if plmnid < 65902 then
                                begin
                                  if plmnid < 65514 then
                                    begin
                                      if plmnid = 65501 then
                                        begin
                                          result := 'Vodacom ZA';
                                          exit;
                                        end;
                                      if plmnid = 65502 then
                                        begin
                                          result := 'Telkom ZA';
                                          exit;
                                        end;
                                      if plmnid = 65507 then
                                        begin
                                          result := 'Cell C';
                                          exit;
                                        end;
                                      if plmnid = 65510 then
                                        begin
                                          result := 'MTN ZA';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 65514 then
                                        begin
                                          result := 'Neotel';
                                          exit;
                                        end;
                                      if plmnid = 65519 then
                                        begin
                                          result := 'Rain';
                                          exit;
                                        end;
                                      if plmnid = 65701 then
                                        begin
                                          result := 'Eritel';
                                          exit;
                                        end;
                                      if plmnid = 65801 then
                                        begin
                                          result := 'Sure SH';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 70269 then
                                    begin
                                      if plmnid = 65902 then
                                        begin
                                          result := 'MTN SS';
                                          exit;
                                        end;
                                      if plmnid = 65903 then
                                        begin
                                          result := 'Gemtel';
                                          exit;
                                        end;
                                      if plmnid = 65906 then
                                        begin
                                          result := 'Zain SS';
                                          exit;
                                        end;
                                      if plmnid = 70267 then
                                        begin
                                          result := 'DigiCell';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 70402 then
                                        begin
                                          if plmnid = 70269 then
                                            begin
                                              result := 'SMART BZ';
                                              exit;
                                            end;
                                          if plmnid = 70401 then
                                            begin
                                              result := 'Claro GT';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 70402 then
                                            begin
                                              result := 'Tigo GT';
                                              exit;
                                            end;
                                          if plmnid = 70403 then
                                            begin
                                              result := 'movistar GT';
                                              exit;
                                            end;
                                          if plmnid = 70601 then
                                            begin
                                              result := 'Claro SV';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 71204 then
                                begin
                                  if plmnid < 71030 then
                                    begin
                                      if plmnid = 70602 then
                                        begin
                                          result := 'Digicel SV';
                                          exit;
                                        end;
                                      if plmnid = 70603 then
                                        begin
                                          result := 'Tigo SV';
                                          exit;
                                        end;
                                      if plmnid = 70604 then
                                        begin
                                          result := 'movistar SV';
                                          exit;
                                        end;
                                      if plmnid = 71021 then
                                        begin
                                          result := 'Claro NI';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 71201 then
                                        begin
                                          if plmnid = 71030 then
                                            begin
                                              result := 'movistar NI';
                                              exit;
                                            end;
                                          if plmnid = 71073 then
                                            begin
                                              result := 'Claro NI';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 71201 then
                                            begin
                                              result := 'Kölbi ICE';
                                              exit;
                                            end;
                                          if plmnid = 71202 then
                                            begin
                                              result := 'Kölbi ICE';
                                              exit;
                                            end;
                                          if plmnid = 71203 then
                                            begin
                                              result := 'Claro CR';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 71404 then
                                    begin
                                      if plmnid = 71204 then
                                        begin
                                          result := 'movistar CR';
                                          exit;
                                        end;
                                      if plmnid = 71401 then
                                        begin
                                          result := 'Cable & Wireless PA';
                                          exit;
                                        end;
                                      if plmnid = 71402 then
                                        begin
                                          result := 'movistar PA';
                                          exit;
                                        end;
                                      if plmnid = 71403 then
                                        begin
                                          result := 'Claro PA';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 71610 then
                                        begin
                                          if plmnid = 71404 then
                                            begin
                                              result := 'Digicel PA';
                                              exit;
                                            end;
                                          if plmnid = 71606 then
                                            begin
                                              result := 'Movistar PE';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 71610 then
                                            begin
                                              result := 'Claro PE';
                                              exit;
                                            end;
                                          if plmnid = 71615 then
                                            begin
                                              result := 'Bitel';
                                              exit;
                                            end;
                                          if plmnid = 71617 then
                                            begin
                                              result := 'Entel PE';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            end
          else
            begin
              if plmnid < 312330 then
                begin
                  if plmnid < 310080 then
                    begin
                      if plmnid < 74602 then
                        begin
                          if plmnid < 73009 then
                            begin
                              if plmnid < 72423 then
                                begin
                                  if plmnid < 72406 then
                                    begin
                                      if plmnid = 72402 then
                                        begin
                                          result := 'TIM BR';
                                          exit;
                                        end;
                                      if plmnid = 72403 then
                                        begin
                                          result := 'TIM BR';
                                          exit;
                                        end;
                                      if plmnid = 72404 then
                                        begin
                                          result := 'TIM BR';
                                          exit;
                                        end;
                                      if plmnid = 72405 then
                                        begin
                                          result := 'Claro BR';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 72406 then
                                        begin
                                          result := 'Vivo';
                                          exit;
                                        end;
                                      if plmnid = 72410 then
                                        begin
                                          result := 'Vivo';
                                          exit;
                                        end;
                                      if plmnid = 72411 then
                                        begin
                                          result := 'Vivo';
                                          exit;
                                        end;
                                      if plmnid = 72415 then
                                        begin
                                          result := 'Sercomtel';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 72434 then
                                    begin
                                      if plmnid = 72423 then
                                        begin
                                          result := 'Vivo';
                                          exit;
                                        end;
                                      if plmnid = 72431 then
                                        begin
                                          result := 'Oi';
                                          exit;
                                        end;
                                      if plmnid = 72432 then
                                        begin
                                          result := 'Algar Telecom';
                                          exit;
                                        end;
                                      if plmnid = 72433 then
                                        begin
                                          result := 'Algar Telecom';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 73001 then
                                        begin
                                          if plmnid = 72434 then
                                            begin
                                              result := 'Algar Telecom';
                                              exit;
                                            end;
                                          if plmnid = 72439 then
                                            begin
                                              result := 'Nextel';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 73001 then
                                            begin
                                              result := 'entel';
                                              exit;
                                            end;
                                          if plmnid = 73002 then
                                            begin
                                              result := 'movistar CL';
                                              exit;
                                            end;
                                          if plmnid = 73003 then
                                            begin
                                              result := 'CLARO CL';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 73801 then
                                begin
                                  if plmnid < 73404 then
                                    begin
                                      if plmnid = 73009 then
                                        begin
                                          result := 'WOM';
                                          exit;
                                        end;
                                      if plmnid = 73010 then
                                        begin
                                          result := 'entel';
                                          exit;
                                        end;
                                      if plmnid = 73099 then
                                        begin
                                          result := 'Will';
                                          exit;
                                        end;
                                      if plmnid = 73402 then
                                        begin
                                          result := 'Digitel GSM';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 73601 then
                                        begin
                                          if plmnid = 73404 then
                                            begin
                                              result := 'movistar VE';
                                              exit;
                                            end;
                                          if plmnid = 73406 then
                                            begin
                                              result := 'Movilnet';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 73601 then
                                            begin
                                              result := 'Viva BO';
                                              exit;
                                            end;
                                          if plmnid = 73602 then
                                            begin
                                              result := 'Entel BO';
                                              exit;
                                            end;
                                          if plmnid = 73603 then
                                            begin
                                              result := 'Tigo BO';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 74401 then
                                    begin
                                      if plmnid = 73801 then
                                        begin
                                          result := 'Digicel GY';
                                          exit;
                                        end;
                                      if plmnid = 74000 then
                                        begin
                                          result := 'Movistar EC';
                                          exit;
                                        end;
                                      if plmnid = 74001 then
                                        begin
                                          result := 'Claro EC';
                                          exit;
                                        end;
                                      if plmnid = 74002 then
                                        begin
                                          result := 'CNT Mobile';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 74404 then
                                        begin
                                          if plmnid = 74401 then
                                            begin
                                              result := 'VOX';
                                              exit;
                                            end;
                                          if plmnid = 74402 then
                                            begin
                                              result := 'Claro PY';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 74404 then
                                            begin
                                              result := 'Tigo PY';
                                              exit;
                                            end;
                                          if plmnid = 74405 then
                                            begin
                                              result := 'Personal PY';
                                              exit;
                                            end;
                                          if plmnid = 74406 then
                                            begin
                                              result := 'Copaco';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 302270 then
                            begin
                              if plmnid < 90115 then
                                begin
                                  if plmnid < 74810 then
                                    begin
                                      if plmnid = 74602 then
                                        begin
                                          result := 'Telesur';
                                          exit;
                                        end;
                                      if plmnid = 74603 then
                                        begin
                                          result := 'Digicel SR';
                                          exit;
                                        end;
                                      if plmnid = 74801 then
                                        begin
                                          result := 'Antel';
                                          exit;
                                        end;
                                      if plmnid = 74807 then
                                        begin
                                          result := 'Movistar UY';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 74810 then
                                        begin
                                          result := 'Claro UY';
                                          exit;
                                        end;
                                      if plmnid = 90112 then
                                        begin
                                          result := 'Telenor INTL';
                                          exit;
                                        end;
                                      if plmnid = 90113 then
                                        begin
                                          result := 'GSM.AQ';
                                          exit;
                                        end;
                                      if plmnid = 90114 then
                                        begin
                                          result := 'AeroMobile';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 90127 then
                                    begin
                                      if plmnid = 90115 then
                                        begin
                                          result := 'OnAir';
                                          exit;
                                        end;
                                      if plmnid = 90118 then
                                        begin
                                          result := 'Cellular @Sea';
                                          exit;
                                        end;
                                      if plmnid = 90119 then
                                        begin
                                          result := 'Vodafone Malta Maritime';
                                          exit;
                                        end;
                                      if plmnid = 90126 then
                                        begin
                                          result := 'TIM@sea';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 90132 then
                                        begin
                                          if plmnid = 90127 then
                                            begin
                                              result := 'OnMarine';
                                              exit;
                                            end;
                                          if plmnid = 90131 then
                                            begin
                                              result := 'Orange INTL';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 90132 then
                                            begin
                                              result := 'Sky High';
                                              exit;
                                            end;
                                          if plmnid = 99501 then
                                            begin
                                              result := 'FonePlus';
                                              exit;
                                            end;
                                          if plmnid = 302220 then
                                            begin
                                              result := 'Telus Mobility, Koodo Mobile, Public Mobile';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 302720 then
                                begin
                                  if plmnid < 302510 then
                                    begin
                                      if plmnid = 302270 then
                                        begin
                                          result := 'EastLink';
                                          exit;
                                        end;
                                      if plmnid = 302480 then
                                        begin
                                          result := 'Qiniq';
                                          exit;
                                        end;
                                      if plmnid = 302490 then
                                        begin
                                          result := 'Freedom Mobile';
                                          exit;
                                        end;
                                      if plmnid = 302500 then
                                        begin
                                          result := 'Videotron';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 302610 then
                                        begin
                                          if plmnid = 302510 then
                                            begin
                                              result := 'Videotron';
                                              exit;
                                            end;
                                          if plmnid = 302530 then
                                            begin
                                              result := 'Keewaytinook Mobile';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 302610 then
                                            begin
                                              result := 'Bell Mobility';
                                              exit;
                                            end;
                                          if plmnid = 302620 then
                                            begin
                                              result := 'ICE Wireless';
                                              exit;
                                            end;
                                          if plmnid = 302660 then
                                            begin
                                              result := 'MTS CA';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 310030 then
                                    begin
                                      if plmnid = 302720 then
                                        begin
                                          result := 'Rogers Wireless';
                                          exit;
                                        end;
                                      if plmnid = 302780 then
                                        begin
                                          result := 'SaskTel';
                                          exit;
                                        end;
                                      if plmnid = 310012 then
                                        begin
                                          result := 'Verizon';
                                          exit;
                                        end;
                                      if plmnid = 310020 then
                                        begin
                                          result := 'Union Wireless';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 310032 then
                                        begin
                                          if plmnid = 310030 then
                                            begin
                                              result := 'AT&T US';
                                              exit;
                                            end;
                                          if plmnid = 310032 then
                                            begin
                                              result := 'IT&E Wireless GU';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 310032 then
                                            begin
                                              result := 'IT&E Wireless US';
                                              exit;
                                            end;
                                          if plmnid = 310066 then
                                            begin
                                              result := 'U.S. Cellular';
                                              exit;
                                            end;
                                          if plmnid = 310070 then
                                            begin
                                              result := 'AT&T US';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end
                  else
                    begin
                      if plmnid < 311080 then
                        begin
                          if plmnid < 310370 then
                            begin
                              if plmnid < 310150 then
                                begin
                                  if plmnid < 310110 then
                                    begin
                                      if plmnid = 310080 then
                                        begin
                                          result := 'AT&T US';
                                          exit;
                                        end;
                                      if plmnid = 310090 then
                                        begin
                                          result := 'AT&T US';
                                          exit;
                                        end;
                                      if plmnid = 310100 then
                                        begin
                                          result := 'Plateau Wireless';
                                          exit;
                                        end;
                                      if plmnid = 310110 then
                                        begin
                                          result := 'IT&E Wireless MP';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 310110 then
                                        begin
                                          result := 'IT&E Wireless US';
                                          exit;
                                        end;
                                      if plmnid = 310120 then
                                        begin
                                          result := 'Sprint';
                                          exit;
                                        end;
                                      if plmnid = 310140 then
                                        begin
                                          result := 'GTA Wireless GU';
                                          exit;
                                        end;
                                      if plmnid = 310140 then
                                        begin
                                          result := 'GTA Wireless US';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 310190 then
                                    begin
                                      if plmnid = 310150 then
                                        begin
                                          result := 'AT&T US';
                                          exit;
                                        end;
                                      if plmnid = 310160 then
                                        begin
                                          result := 'T-Mobile US';
                                          exit;
                                        end;
                                      if plmnid = 310170 then
                                        begin
                                          result := 'AT&T US';
                                          exit;
                                        end;
                                      if plmnid = 310180 then
                                        begin
                                          result := 'West Central';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 310320 then
                                        begin
                                          if plmnid = 310190 then
                                            begin
                                              result := 'GCI';
                                              exit;
                                            end;
                                          if plmnid = 310260 then
                                            begin
                                              result := 'T-Mobile US';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 310320 then
                                            begin
                                              result := 'Cellular One';
                                              exit;
                                            end;
                                          if plmnid = 310370 then
                                            begin
                                              result := 'Docomo GU';
                                              exit;
                                            end;
                                          if plmnid = 310370 then
                                            begin
                                              result := 'Docomo MP';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 310740 then
                                begin
                                  if plmnid < 310410 then
                                    begin
                                      if plmnid = 310370 then
                                        begin
                                          result := 'Docomo US';
                                          exit;
                                        end;
                                      if plmnid = 310390 then
                                        begin
                                          result := 'Cellular One of East Texas';
                                          exit;
                                        end;
                                      if plmnid = 310400 then
                                        begin
                                          result := 'iConnect GU';
                                          exit;
                                        end;
                                      if plmnid = 310400 then
                                        begin
                                          result := 'iConnect US';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 310450 then
                                        begin
                                          if plmnid = 310410 then
                                            begin
                                              result := 'AT&T US';
                                              exit;
                                            end;
                                          if plmnid = 310430 then
                                            begin
                                              result := 'GCI';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 310450 then
                                            begin
                                              result := 'Viaero';
                                              exit;
                                            end;
                                          if plmnid = 310540 then
                                            begin
                                              result := 'Phoenix US';
                                              exit;
                                            end;
                                          if plmnid = 310680 then
                                            begin
                                              result := 'AT&T US';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 310990 then
                                    begin
                                      if plmnid = 310740 then
                                        begin
                                          result := 'Viaero';
                                          exit;
                                        end;
                                      if plmnid = 310770 then
                                        begin
                                          result := 'iWireless';
                                          exit;
                                        end;
                                      if plmnid = 310790 then
                                        begin
                                          result := 'BLAZE';
                                          exit;
                                        end;
                                      if plmnid = 310950 then
                                        begin
                                          result := 'AT&T US';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 311030 then
                                        begin
                                          if plmnid = 310990 then
                                            begin
                                              result := 'Evolve Broadband';
                                              exit;
                                            end;
                                          if plmnid = 311020 then
                                            begin
                                              result := 'Chariton Valley';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 311030 then
                                            begin
                                              result := 'Indigo Wireless';
                                              exit;
                                            end;
                                          if plmnid = 311040 then
                                            begin
                                              result := 'Choice Wireless';
                                              exit;
                                            end;
                                          if plmnid = 311070 then
                                            begin
                                              result := 'AT&T US';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 311670 then
                            begin
                              if plmnid < 311470 then
                                begin
                                  if plmnid < 311320 then
                                    begin
                                      if plmnid = 311080 then
                                        begin
                                          result := 'Pine Cellular';
                                          exit;
                                        end;
                                      if plmnid = 311090 then
                                        begin
                                          result := 'AT&T US';
                                          exit;
                                        end;
                                      if plmnid = 311230 then
                                        begin
                                          result := 'C Spire Wireless';
                                          exit;
                                        end;
                                      if plmnid = 311290 then
                                        begin
                                          result := 'BLAZE';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 311320 then
                                        begin
                                          result := 'Choice Wireless';
                                          exit;
                                        end;
                                      if plmnid = 311330 then
                                        begin
                                          result := 'Bug Tussel Wireless';
                                          exit;
                                        end;
                                      if plmnid = 311370 then
                                        begin
                                          result := 'GCI Wireless';
                                          exit;
                                        end;
                                      if plmnid = 311450 then
                                        begin
                                          result := 'PTCI';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 311550 then
                                    begin
                                      if plmnid = 311470 then
                                        begin
                                          result := 'Viya';
                                          exit;
                                        end;
                                      if plmnid = 311480 then
                                        begin
                                          result := 'Verizon';
                                          exit;
                                        end;
                                      if plmnid = 311490 then
                                        begin
                                          result := 'Sprint';
                                          exit;
                                        end;
                                      if plmnid = 311530 then
                                        begin
                                          result := 'NewCore';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 311580 then
                                        begin
                                          if plmnid = 311550 then
                                            begin
                                              result := 'Choice Wireless';
                                              exit;
                                            end;
                                          if plmnid = 311560 then
                                            begin
                                              result := 'OTZ Cellular';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 311580 then
                                            begin
                                              result := 'U.S. Cellular';
                                              exit;
                                            end;
                                          if plmnid = 311640 then
                                            begin
                                              result := 'Rock Wireless';
                                              exit;
                                            end;
                                          if plmnid = 311650 then
                                            begin
                                              result := 'United Wireless';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 312120 then
                                begin
                                  if plmnid < 311850 then
                                    begin
                                      if plmnid = 311670 then
                                        begin
                                          result := 'Pine Belt Wireless';
                                          exit;
                                        end;
                                      if plmnid = 311780 then
                                        begin
                                          result := 'ASTCA US';
                                          exit;
                                        end;
                                      if plmnid = 311780 then
                                        begin
                                          result := 'ASTCA AS';
                                          exit;
                                        end;
                                      if plmnid = 311840 then
                                        begin
                                          result := 'Cellcom US';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 311950 then
                                        begin
                                          if plmnid = 311850 then
                                            begin
                                              result := 'Cellcom US';
                                              exit;
                                            end;
                                          if plmnid = 311860 then
                                            begin
                                              result := 'STRATA';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 311950 then
                                            begin
                                              result := 'ETC';
                                              exit;
                                            end;
                                          if plmnid = 311970 then
                                            begin
                                              result := 'Big River Broadband';
                                              exit;
                                            end;
                                          if plmnid = 312030 then
                                            begin
                                              result := 'Bravado Wireless';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 312170 then
                                    begin
                                      if plmnid = 312120 then
                                        begin
                                          result := 'Appalachian Wireless';
                                          exit;
                                        end;
                                      if plmnid = 312130 then
                                        begin
                                          result := 'Appalachian Wireless';
                                          exit;
                                        end;
                                      if plmnid = 312150 then
                                        begin
                                          result := 'NorthwestCell';
                                          exit;
                                        end;
                                      if plmnid = 312160 then
                                        begin
                                          result := 'Chat Mobility';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 312260 then
                                        begin
                                          if plmnid = 312170 then
                                            begin
                                              result := 'Chat Mobility';
                                              exit;
                                            end;
                                          if plmnid = 312220 then
                                            begin
                                              result := 'Chariton Valley';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 312260 then
                                            begin
                                              result := 'NewCore';
                                              exit;
                                            end;
                                          if plmnid = 312270 then
                                            begin
                                              result := 'Pioneer Cellular';
                                              exit;
                                            end;
                                          if plmnid = 312280 then
                                            begin
                                              result := 'Pioneer Cellular';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end
              else
                begin
                  if plmnid < 405803 then
                    begin
                      if plmnid < 360110 then
                        begin
                          if plmnid < 338180 then
                            begin
                              if plmnid < 330120 then
                                begin
                                  if plmnid < 312860 then
                                    begin
                                      if plmnid = 312330 then
                                        begin
                                          result := 'Nemont';
                                          exit;
                                        end;
                                      if plmnid = 312400 then
                                        begin
                                          result := 'Mid-Rivers Wireless';
                                          exit;
                                        end;
                                      if plmnid = 312470 then
                                        begin
                                          result := 'Carolina West Wireless';
                                          exit;
                                        end;
                                      if plmnid = 312720 then
                                        begin
                                          result := 'Southern LINC';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 312860 then
                                        begin
                                          result := 'ClearTalk';
                                          exit;
                                        end;
                                      if plmnid = 312900 then
                                        begin
                                          result := 'ClearTalk';
                                          exit;
                                        end;
                                      if plmnid = 313100 then
                                        begin
                                          result := 'FirstNet';
                                          exit;
                                        end;
                                      if plmnid = 330110 then
                                        begin
                                          result := 'Claro Puerto Rico';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 334140 then
                                    begin
                                      if plmnid = 330120 then
                                        begin
                                          result := 'Open Mobile';
                                          exit;
                                        end;
                                      if plmnid = 334020 then
                                        begin
                                          result := 'Telcel';
                                          exit;
                                        end;
                                      if plmnid = 334050 then
                                        begin
                                          result := 'AT&T / Unefon';
                                          exit;
                                        end;
                                      if plmnid = 334090 then
                                        begin
                                          result := 'AT&T MX';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 338050 then
                                        begin
                                          if plmnid = 334140 then
                                            begin
                                              result := 'Altan Redes';
                                              exit;
                                            end;
                                          if plmnid = 338050 then
                                            begin
                                              result := 'Digicel JM';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 338050 then
                                            begin
                                              result := 'Digicel LC';
                                              exit;
                                            end;
                                          if plmnid = 338050 then
                                            begin
                                              result := 'Digicel TC';
                                              exit;
                                            end;
                                          if plmnid = 338050 then
                                            begin
                                              result := 'Digicel Bermuda';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 348570 then
                                begin
                                  if plmnid < 344050 then
                                    begin
                                      if plmnid = 338180 then
                                        begin
                                          result := 'FLOW JM';
                                          exit;
                                        end;
                                      if plmnid = 342600 then
                                        begin
                                          result := 'FLOW BB';
                                          exit;
                                        end;
                                      if plmnid = 342750 then
                                        begin
                                          result := 'Digicel BB';
                                          exit;
                                        end;
                                      if plmnid = 344030 then
                                        begin
                                          result := 'APUA';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 346050 then
                                        begin
                                          if plmnid = 344050 then
                                            begin
                                              result := 'Digicel AG';
                                              exit;
                                            end;
                                          if plmnid = 344920 then
                                            begin
                                              result := 'FLOW AG';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 346050 then
                                            begin
                                              result := 'Digicel KY';
                                              exit;
                                            end;
                                          if plmnid = 346140 then
                                            begin
                                              result := 'FLOW KY';
                                              exit;
                                            end;
                                          if plmnid = 348170 then
                                            begin
                                              result := 'FLOW VG';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 354860 then
                                    begin
                                      if plmnid = 348570 then
                                        begin
                                          result := 'CCT Boatphone';
                                          exit;
                                        end;
                                      if plmnid = 348770 then
                                        begin
                                          result := 'Digicel VG';
                                          exit;
                                        end;
                                      if plmnid = 352030 then
                                        begin
                                          result := 'Digicel GD';
                                          exit;
                                        end;
                                      if plmnid = 352110 then
                                        begin
                                          result := 'FLOW GD';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 356110 then
                                        begin
                                          if plmnid = 354860 then
                                            begin
                                              result := 'FLOW MS';
                                              exit;
                                            end;
                                          if plmnid = 356050 then
                                            begin
                                              result := 'Digicel KN';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 356110 then
                                            begin
                                              result := 'FLOW KN';
                                              exit;
                                            end;
                                          if plmnid = 358110 then
                                            begin
                                              result := 'FLOW LC';
                                              exit;
                                            end;
                                          if plmnid = 360050 then
                                            begin
                                              result := 'Digicel VC';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 405039 then
                            begin
                              if plmnid < 405028 then
                                begin
                                  if plmnid < 374130 then
                                    begin
                                      if plmnid = 360110 then
                                        begin
                                          result := 'FLOW VC';
                                          exit;
                                        end;
                                      if plmnid = 365840 then
                                        begin
                                          result := 'FLOW AI';
                                          exit;
                                        end;
                                      if plmnid = 366020 then
                                        begin
                                          result := 'Digicel DM';
                                          exit;
                                        end;
                                      if plmnid = 366110 then
                                        begin
                                          result := 'FLOW DM';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 374130 then
                                        begin
                                          result := 'Digicel TT';
                                          exit;
                                        end;
                                      if plmnid = 376350 then
                                        begin
                                          result := 'FLOW TC';
                                          exit;
                                        end;
                                      if plmnid = 405025 then
                                        begin
                                          result := 'TATA DOCOMO';
                                          exit;
                                        end;
                                      if plmnid = 405027 then
                                        begin
                                          result := 'TATA DOCOMO';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 405034 then
                                    begin
                                      if plmnid = 405028 then
                                        begin
                                          result := 'TATA DOCOMO';
                                          exit;
                                        end;
                                      if plmnid = 405030 then
                                        begin
                                          result := 'TATA DOCOMO';
                                          exit;
                                        end;
                                      if plmnid = 405031 then
                                        begin
                                          result := 'TATA DOCOMO';
                                          exit;
                                        end;
                                      if plmnid = 405032 then
                                        begin
                                          result := 'TATA DOCOMO';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 405036 then
                                        begin
                                          if plmnid = 405034 then
                                            begin
                                              result := 'TATA DOCOMO';
                                              exit;
                                            end;
                                          if plmnid = 405035 then
                                            begin
                                              result := 'TATA DOCOMO';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 405036 then
                                            begin
                                              result := 'TATA DOCOMO';
                                              exit;
                                            end;
                                          if plmnid = 405037 then
                                            begin
                                              result := 'TATA DOCOMO';
                                              exit;
                                            end;
                                          if plmnid = 405038 then
                                            begin
                                              result := 'TATA DOCOMO';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 405751 then
                                begin
                                  if plmnid < 405044 then
                                    begin
                                      if plmnid = 405039 then
                                        begin
                                          result := 'TATA DOCOMO';
                                          exit;
                                        end;
                                      if plmnid = 405041 then
                                        begin
                                          result := 'TATA DOCOMO';
                                          exit;
                                        end;
                                      if plmnid = 405042 then
                                        begin
                                          result := 'TATA DOCOMO';
                                          exit;
                                        end;
                                      if plmnid = 405043 then
                                        begin
                                          result := 'TATA DOCOMO';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 405046 then
                                        begin
                                          if plmnid = 405044 then
                                            begin
                                              result := 'TATA DOCOMO';
                                              exit;
                                            end;
                                          if plmnid = 405045 then
                                            begin
                                              result := 'TATA DOCOMO';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 405046 then
                                            begin
                                              result := 'TATA DOCOMO';
                                              exit;
                                            end;
                                          if plmnid = 405047 then
                                            begin
                                              result := 'TATA DOCOMO';
                                              exit;
                                            end;
                                          if plmnid = 405750 then
                                            begin
                                              result := 'Vodafone India';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 405755 then
                                    begin
                                      if plmnid = 405751 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                      if plmnid = 405752 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                      if plmnid = 405753 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                      if plmnid = 405754 then
                                        begin
                                          result := 'Vodafone India';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 405799 then
                                        begin
                                          if plmnid = 405755 then
                                            begin
                                              result := 'Vodafone India';
                                              exit;
                                            end;
                                          if plmnid = 405756 then
                                            begin
                                              result := 'Vodafone India';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 405799 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                          if plmnid = 405800 then
                                            begin
                                              result := 'AIRCEL';
                                              exit;
                                            end;
                                          if plmnid = 405801 then
                                            begin
                                              result := 'AIRCEL';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end
                  else
                    begin
                      if plmnid < 405866 then
                        begin
                          if plmnid < 405848 then
                            begin
                              if plmnid < 405819 then
                                begin
                                  if plmnid < 405809 then
                                    begin
                                      if plmnid = 405803 then
                                        begin
                                          result := 'AIRCEL';
                                          exit;
                                        end;
                                      if plmnid = 405804 then
                                        begin
                                          result := 'AIRCEL';
                                          exit;
                                        end;
                                      if plmnid = 405805 then
                                        begin
                                          result := 'AIRCEL';
                                          exit;
                                        end;
                                      if plmnid = 405806 then
                                        begin
                                          result := 'AIRCEL';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 405809 then
                                        begin
                                          result := 'AIRCEL';
                                          exit;
                                        end;
                                      if plmnid = 405810 then
                                        begin
                                          result := 'AIRCEL';
                                          exit;
                                        end;
                                      if plmnid = 405811 then
                                        begin
                                          result := 'AIRCEL';
                                          exit;
                                        end;
                                      if plmnid = 405818 then
                                        begin
                                          result := 'Uninor';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 405827 then
                                    begin
                                      if plmnid = 405819 then
                                        begin
                                          result := 'Uninor';
                                          exit;
                                        end;
                                      if plmnid = 405820 then
                                        begin
                                          result := 'Uninor';
                                          exit;
                                        end;
                                      if plmnid = 405821 then
                                        begin
                                          result := 'Uninor';
                                          exit;
                                        end;
                                      if plmnid = 405822 then
                                        begin
                                          result := 'Uninor';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 405845 then
                                        begin
                                          if plmnid = 405827 then
                                            begin
                                              result := 'Videocon Datacom';
                                              exit;
                                            end;
                                          if plmnid = 405840 then
                                            begin
                                              result := 'Jio';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 405845 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                          if plmnid = 405846 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                          if plmnid = 405847 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 405857 then
                                begin
                                  if plmnid < 405852 then
                                    begin
                                      if plmnid = 405848 then
                                        begin
                                          result := 'IDEA';
                                          exit;
                                        end;
                                      if plmnid = 405849 then
                                        begin
                                          result := 'IDEA';
                                          exit;
                                        end;
                                      if plmnid = 405850 then
                                        begin
                                          result := 'IDEA';
                                          exit;
                                        end;
                                      if plmnid = 405851 then
                                        begin
                                          result := 'IDEA';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 405854 then
                                        begin
                                          if plmnid = 405852 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                          if plmnid = 405853 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 405854 then
                                            begin
                                              result := 'Jio';
                                              exit;
                                            end;
                                          if plmnid = 405855 then
                                            begin
                                              result := 'Jio';
                                              exit;
                                            end;
                                          if plmnid = 405856 then
                                            begin
                                              result := 'Jio';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 405861 then
                                    begin
                                      if plmnid = 405857 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                      if plmnid = 405858 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                      if plmnid = 405859 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                      if plmnid = 405860 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 405863 then
                                        begin
                                          if plmnid = 405861 then
                                            begin
                                              result := 'Jio';
                                              exit;
                                            end;
                                          if plmnid = 405862 then
                                            begin
                                              result := 'Jio';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 405863 then
                                            begin
                                              result := 'Jio';
                                              exit;
                                            end;
                                          if plmnid = 405864 then
                                            begin
                                              result := 'Jio';
                                              exit;
                                            end;
                                          if plmnid = 405865 then
                                            begin
                                              result := 'Jio';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end
                      else
                        begin
                          if plmnid < 708002 then
                            begin
                              if plmnid < 405874 then
                                begin
                                  if plmnid < 405870 then
                                    begin
                                      if plmnid = 405866 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                      if plmnid = 405867 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                      if plmnid = 405868 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                      if plmnid = 405869 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid = 405870 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                      if plmnid = 405871 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                      if plmnid = 405872 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                      if plmnid = 405873 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 405910 then
                                    begin
                                      if plmnid = 405874 then
                                        begin
                                          result := 'Jio';
                                          exit;
                                        end;
                                      if plmnid = 405880 then
                                        begin
                                          result := 'Uninor';
                                          exit;
                                        end;
                                      if plmnid = 405908 then
                                        begin
                                          result := 'IDEA';
                                          exit;
                                        end;
                                      if plmnid = 405909 then
                                        begin
                                          result := 'IDEA';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 405929 then
                                        begin
                                          if plmnid = 405910 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                          if plmnid = 405911 then
                                            begin
                                              result := 'IDEA';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 405929 then
                                            begin
                                              result := 'Uninor';
                                              exit;
                                            end;
                                          if plmnid = 502153 then
                                            begin
                                              result := 'unifi';
                                              exit;
                                            end;
                                          if plmnid = 708001 then
                                            begin
                                              result := 'Claro HN';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end
                          else
                            begin
                              if plmnid < 732099 then
                                begin
                                  if plmnid < 722070 then
                                    begin
                                      if plmnid = 708002 then
                                        begin
                                          result := 'Tigo HN';
                                          exit;
                                        end;
                                      if plmnid = 708030 then
                                        begin
                                          result := 'Hondutel';
                                          exit;
                                        end;
                                      if plmnid = 714020 then
                                        begin
                                          result := 'movistar PA';
                                          exit;
                                        end;
                                      if plmnid = 722010 then
                                        begin
                                          result := 'Movistar AR';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 722320 then
                                        begin
                                          if plmnid = 722070 then
                                            begin
                                              result := 'Movistar AR';
                                              exit;
                                            end;
                                          if plmnid = 722310 then
                                            begin
                                              result := 'Claro AR';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 722320 then
                                            begin
                                              result := 'Claro AR';
                                              exit;
                                            end;
                                          if plmnid = 722330 then
                                            begin
                                              result := 'Claro AR';
                                              exit;
                                            end;
                                          if plmnid = 722341 then
                                            begin
                                              result := 'Personal AR';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end
                              else
                                begin
                                  if plmnid < 732123 then
                                    begin
                                      if plmnid = 732099 then
                                        begin
                                          result := 'EMCALI';
                                          exit;
                                        end;
                                      if plmnid = 732101 then
                                        begin
                                          result := 'Claro CO';
                                          exit;
                                        end;
                                      if plmnid = 732103 then
                                        begin
                                          result := 'Tigo CO';
                                          exit;
                                        end;
                                      if plmnid = 732111 then
                                        begin
                                          result := 'Tigo CO';
                                          exit;
                                        end;
                                    end
                                  else
                                    begin
                                      if plmnid < 732187 then
                                        begin
                                          if plmnid = 732123 then
                                            begin
                                              result := 'movistar CO';
                                              exit;
                                            end;
                                          if plmnid = 732130 then
                                            begin
                                              result := 'AVANTEL';
                                              exit;
                                            end;
                                        end
                                      else
                                        begin
                                          if plmnid = 732187 then
                                            begin
                                              result := 'eTb';
                                              exit;
                                            end;
                                          if plmnid = 738002 then
                                            begin
                                              result := 'GT&T Cellink Plus';
                                              exit;
                                            end;
                                          if plmnid = 750001 then
                                            begin
                                              result := 'Sure FK';
                                              exit;
                                            end;
                                        end;
                                    end;
                                end;
                            end;
                        end;
                    end;
                end;
            end;
        end;
      result := mccmnc;
      exit;
    end;


  function TYCellular.decodePLMN(mccmnc: string):string;
    begin
      result := self.imm_decodePLMN(mccmnc);
      exit;
    end;


  function TYCellular.get_communicationProfiles():TStringArray;
    var
      profiles : string;
      lines : TStringArray;
      nlines : LongInt;
      idx : LongInt;
      line : string;
      cpos : LongInt;
      profno : LongInt;
      res : TStringArray;
      res_pos : LongInt;
    begin
      SetLength(lines, 0);
      SetLength(res, 0);

      profiles := self._AT('+UMNOPROF=?');
      lines := _stringSplit(profiles, #10);
      nlines := length(lines);
      if not(nlines > 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'fail to retrieve profile list');
          result:=res;
          exit;
        end;
      res_pos := 0;
      SetLength(res, nlines);;
      idx := 0;
      while idx < nlines do
        begin
          line := lines[idx];
          cpos := (pos(':', line) - 1);
          if cpos > 0 then
            begin
              profno := _atoi(Copy(line,  0 + 1, cpos));
              if profno > 1 then
                begin
                  res[res_pos] := line;
                  inc(res_pos);
                end;
            end;
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
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
