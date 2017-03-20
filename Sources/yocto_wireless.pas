{*********************************************************************
 *
 * $Id: yocto_wireless.pas 26668 2017-02-28 13:36:03Z seb $
 *
 * Implements yFindWireless(), the high-level API for Wireless functions
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


unit yocto_wireless;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (generated code: YWireless definitions)

const Y_LINKQUALITY_INVALID           = YAPI_INVALID_UINT;
const Y_SSID_INVALID                  = YAPI_INVALID_STRING;
const Y_CHANNEL_INVALID               = YAPI_INVALID_UINT;
const Y_SECURITY_UNKNOWN = 0;
const Y_SECURITY_OPEN = 1;
const Y_SECURITY_WEP = 2;
const Y_SECURITY_WPA = 3;
const Y_SECURITY_WPA2 = 4;
const Y_SECURITY_INVALID = -1;
const Y_MESSAGE_INVALID               = YAPI_INVALID_STRING;
const Y_WLANCONFIG_INVALID            = YAPI_INVALID_STRING;


//--- (end of generated code: YWireless definitions)

type
TYWireless = class;
TYWlanRecord = class;
TYWlanRecordArr = array of TYWlanRecord;

  //--- (generated code: YWlanRecord class start)
  ////
  /// <summary>
  ///   TYWlanRecord Class: Description of a wireless network
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  ///-
  TYWlanRecord=class(TObject)
  //--- (end of generated code: YWlanRecord class start)
  protected

    //--- (generated code: YWlanRecord declaration)
    // Attributes (function value cache)
    _ssid                     : string;
    _channel                  : LongInt;
    _sec                      : string;
    _rssi                     : LongInt;

    //--- (end of generated code: YWlanRecord declaration)
public
   constructor create(data:string);
   

   //--- (generated code: YWlanRecord accessors declaration)
    function get_ssid():string; overload; virtual;

    function get_channel():LongInt; overload; virtual;

    function get_security():string; overload; virtual;

    function get_linkQuality():LongInt; overload; virtual;


  //--- (end of generated code: YWlanRecord accessors declaration)
end;


TYWLANRECORDARRAY = array of TYWlanRecord;

//--- (generated code: YWireless class start)
  TYWirelessValueCallback = procedure(func: TYWireless; value:string);
  TYWirelessTimedReportCallback = procedure(func: TYWireless; value:TYMeasure);

  ////
  /// <summary>
  ///   TYWireless Class: Wireless function interface
  /// <para>
  ///   YWireless functions provides control over wireless network parameters
  ///   and status for devices that are wireless-enabled.
  /// </para>
  /// </summary>
  ///-
  TYWireless=class(TYFunction)
  //--- (end of generated code: YWireless class start)

//--- (generated code: YWireless declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _linkQuality              : LongInt;
    _ssid                     : string;
    _channel                  : LongInt;
    _security                 : Integer;
    _message                  : string;
    _wlanConfig               : string;
    _valueCallbackWireless    : TYWirelessValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YWireless declaration)

public
   //--- (generated code: YWireless accessors declaration)
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
    ///   Returns the wireless network name (SSID).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the wireless network name (SSID)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SSID_INVALID</c>.
    /// </para>
    ///-
    function get_ssid():string;

    ////
    /// <summary>
    ///   Returns the 802.11 channel currently used, or 0 when the selected network has not been found.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the 802.11 channel currently used, or 0 when the selected network has not been found
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CHANNEL_INVALID</c>.
    /// </para>
    ///-
    function get_channel():LongInt;

    ////
    /// <summary>
    ///   Returns the security algorithm used by the selected wireless network.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_SECURITY_UNKNOWN</c>, <c>Y_SECURITY_OPEN</c>, <c>Y_SECURITY_WEP</c>,
    ///   <c>Y_SECURITY_WPA</c> and <c>Y_SECURITY_WPA2</c> corresponding to the security algorithm used by
    ///   the selected wireless network
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SECURITY_INVALID</c>.
    /// </para>
    ///-
    function get_security():Integer;

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

    function get_wlanConfig():string;

    function set_wlanConfig(newval:string):integer;

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
    ///   Use the method <c>YWireless.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YWireless</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindWireless(func: string):TYWireless;

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
    function registerValueCallback(callback: TYWirelessValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Changes the configuration of the wireless lan interface to connect to an existing
    ///   access point (infrastructure mode).
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
    /// </para>
    /// </summary>
    /// <param name="ssid">
    ///   the name of the network to connect to
    /// </param>
    /// <param name="securityKey">
    ///   the network key, as a character string
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function joinNetwork(ssid: string; securityKey: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes the configuration of the wireless lan interface to create an ad-hoc
    ///   wireless network, without using an access point.
    /// <para>
    ///   On the YoctoHub-Wireless-g,
    ///   it is best to use softAPNetworkInstead(), which emulates an access point
    ///   (Soft AP) which is more efficient and more widely supported than ad-hoc networks.
    /// </para>
    /// <para>
    ///   When a security key is specified for an ad-hoc network, the network is protected
    ///   by a WEP40 key (5 characters or 10 hexadecimal digits) or WEP128 key (13 characters
    ///   or 26 hexadecimal digits). It is recommended to use a well-randomized WEP128 key
    ///   using 26 hexadecimal digits to maximize security.
    ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module
    ///   to apply this setting.
    /// </para>
    /// </summary>
    /// <param name="ssid">
    ///   the name of the network to connect to
    /// </param>
    /// <param name="securityKey">
    ///   the network key, as a character string
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function adhocNetwork(ssid: string; securityKey: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes the configuration of the wireless lan interface to create a new wireless
    ///   network by emulating a WiFi access point (Soft AP).
    /// <para>
    ///   This function can only be
    ///   used with the YoctoHub-Wireless-g.
    /// </para>
    /// <para>
    ///   When a security key is specified for a SoftAP network, the network is protected
    ///   by a WEP40 key (5 characters or 10 hexadecimal digits) or WEP128 key (13 characters
    ///   or 26 hexadecimal digits). It is recommended to use a well-randomized WEP128 key
    ///   using 26 hexadecimal digits to maximize security.
    ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
    /// </para>
    /// </summary>
    /// <param name="ssid">
    ///   the name of the network to connect to
    /// </param>
    /// <param name="securityKey">
    ///   the network key, as a character string
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function softAPNetwork(ssid: string; securityKey: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list of YWlanRecord objects that describe detected Wireless networks.
    /// <para>
    ///   This list is not updated when the module is already connected to an acces point (infrastructure mode).
    ///   To force an update of this list, <c>adhocNetwork()</c> must be called to disconnect
    ///   the module from the current network. The returned list must be unallocated by the caller.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list of <c>YWlanRecord</c> objects, containing the SSID, channel,
    ///   link quality and the type of security of the wireless network.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty list.
    /// </para>
    ///-
    function get_detectedWlans():TYWlanRecordArray; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of wireless lan interfaces started using <c>yFirstWireless()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YWireless</c> object, corresponding to
    ///   a wireless lan interface currently online, or a <c>NIL</c> pointer
    ///   if there are no more wireless lan interfaces to enumerate.
    /// </returns>
    ///-
    function nextWireless():TYWireless;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstWireless():TYWireless;
  //--- (end of generated code: YWireless accessors declaration)
end;

procedure freeWlanRecordArray(var list:TYWLANRECORDARRAY);

//--- (generated code: Wireless functions declaration)
  ////
  /// <summary>
  ///   Retrieves a wireless lan interface for a given identifier.
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
  ///   This function does not require that the wireless lan interface is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YWireless.isOnline()</c> to test if the wireless lan interface is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a wireless lan interface by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the wireless lan interface
  /// </param>
  /// <returns>
  ///   a <c>YWireless</c> object allowing you to drive the wireless lan interface.
  /// </returns>
  ///-
  function yFindWireless(func:string):TYWireless;
  ////
  /// <summary>
  ///   Starts the enumeration of wireless lan interfaces currently accessible.
  /// <para>
  ///   Use the method <c>YWireless.nextWireless()</c> to iterate on
  ///   next wireless lan interfaces.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YWireless</c> object, corresponding to
  ///   the first wireless lan interface currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstWireless():TYWireless;

//--- (end of generated code: Wireless functions declaration)

implementation

  constructor TYWireless.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Wireless';
      //--- (generated code: YWireless accessors initialization)
      _linkQuality := Y_LINKQUALITY_INVALID;
      _ssid := Y_SSID_INVALID;
      _channel := Y_CHANNEL_INVALID;
      _security := Y_SECURITY_INVALID;
      _message := Y_MESSAGE_INVALID;
      _wlanConfig := Y_WLANCONFIG_INVALID;
      _valueCallbackWireless := nil;
      //--- (end of generated code: YWireless accessors initialization)
    end;

//--- (generated code: YWireless implementation)
{$HINTS OFF}
  function TYWireless._parseAttr(member:PJSONRECORD):integer;
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
      if (member^.name = 'ssid') then
        begin
          _ssid := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'channel') then
        begin
          _channel := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'security') then
        begin
          _security := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'message') then
        begin
          _message := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'wlanConfig') then
        begin
          _wlanConfig := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

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
  ///   On failure, throws an exception or returns Y_LINKQUALITY_INVALID.
  /// </para>
  ///-
  function TYWireless.get_linkQuality():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LINKQUALITY_INVALID;
              exit;
            end;
        end;
      res := self._linkQuality;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the wireless network name (SSID).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the wireless network name (SSID)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_SSID_INVALID.
  /// </para>
  ///-
  function TYWireless.get_ssid():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SSID_INVALID;
              exit;
            end;
        end;
      res := self._ssid;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the 802.11 channel currently used, or 0 when the selected network has not been found.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the 802.11 channel currently used, or 0 when the selected network has not been found
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_CHANNEL_INVALID.
  /// </para>
  ///-
  function TYWireless.get_channel():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CHANNEL_INVALID;
              exit;
            end;
        end;
      res := self._channel;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the security algorithm used by the selected wireless network.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_SECURITY_UNKNOWN, Y_SECURITY_OPEN, Y_SECURITY_WEP, Y_SECURITY_WPA and
  ///   Y_SECURITY_WPA2 corresponding to the security algorithm used by the selected wireless network
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_SECURITY_INVALID.
  /// </para>
  ///-
  function TYWireless.get_security():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SECURITY_INVALID;
              exit;
            end;
        end;
      res := self._security;
      result := res;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_MESSAGE_INVALID.
  /// </para>
  ///-
  function TYWireless.get_message():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MESSAGE_INVALID;
              exit;
            end;
        end;
      res := self._message;
      result := res;
      exit;
    end;


  function TYWireless.get_wlanConfig():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_WLANCONFIG_INVALID;
              exit;
            end;
        end;
      res := self._wlanConfig;
      result := res;
      exit;
    end;


  function TYWireless.set_wlanConfig(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('wlanConfig',rest_val);
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
  ///   Use the method <c>YWireless.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YWireless</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYWireless.FindWireless(func: string):TYWireless;
    var
      obj : TYWireless;
    begin
      obj := TYWireless(TYFunction._FindFromCache('Wireless', func));
      if obj = nil then
        begin
          obj :=  TYWireless.create(func);
          TYFunction._AddToCache('Wireless',  func, obj);
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
  function TYWireless.registerValueCallback(callback: TYWirelessValueCallback):LongInt;
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
      self._valueCallbackWireless := callback;
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


  function TYWireless._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackWireless) <> nil) then
        begin
          self._valueCallbackWireless(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the configuration of the wireless lan interface to connect to an existing
  ///   access point (infrastructure mode).
  /// <para>
  ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
  /// </para>
  /// </summary>
  /// <param name="ssid">
  ///   the name of the network to connect to
  /// </param>
  /// <param name="securityKey">
  ///   the network key, as a character string
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYWireless.joinNetwork(ssid: string; securityKey: string):LongInt;
    begin
      result := self.set_wlanConfig('INFRA:'+ ssid+'\'+securityKey);
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the configuration of the wireless lan interface to create an ad-hoc
  ///   wireless network, without using an access point.
  /// <para>
  ///   On the YoctoHub-Wireless-g,
  ///   it is best to use softAPNetworkInstead(), which emulates an access point
  ///   (Soft AP) which is more efficient and more widely supported than ad-hoc networks.
  /// </para>
  /// <para>
  ///   When a security key is specified for an ad-hoc network, the network is protected
  ///   by a WEP40 key (5 characters or 10 hexadecimal digits) or WEP128 key (13 characters
  ///   or 26 hexadecimal digits). It is recommended to use a well-randomized WEP128 key
  ///   using 26 hexadecimal digits to maximize security.
  ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module
  ///   to apply this setting.
  /// </para>
  /// </summary>
  /// <param name="ssid">
  ///   the name of the network to connect to
  /// </param>
  /// <param name="securityKey">
  ///   the network key, as a character string
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYWireless.adhocNetwork(ssid: string; securityKey: string):LongInt;
    begin
      result := self.set_wlanConfig('ADHOC:'+ ssid+'\'+securityKey);
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the configuration of the wireless lan interface to create a new wireless
  ///   network by emulating a WiFi access point (Soft AP).
  /// <para>
  ///   This function can only be
  ///   used with the YoctoHub-Wireless-g.
  /// </para>
  /// <para>
  ///   When a security key is specified for a SoftAP network, the network is protected
  ///   by a WEP40 key (5 characters or 10 hexadecimal digits) or WEP128 key (13 characters
  ///   or 26 hexadecimal digits). It is recommended to use a well-randomized WEP128 key
  ///   using 26 hexadecimal digits to maximize security.
  ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
  /// </para>
  /// </summary>
  /// <param name="ssid">
  ///   the name of the network to connect to
  /// </param>
  /// <param name="securityKey">
  ///   the network key, as a character string
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYWireless.softAPNetwork(ssid: string; securityKey: string):LongInt;
    begin
      result := self.set_wlanConfig('SOFTAP:'+ ssid+'\'+securityKey);
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a list of YWlanRecord objects that describe detected Wireless networks.
  /// <para>
  ///   This list is not updated when the module is already connected to an acces point (infrastructure mode).
  ///   To force an update of this list, <c>adhocNetwork()</c> must be called to disconnect
  ///   the module from the current network. The returned list must be unallocated by the caller.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a list of <c>YWlanRecord</c> objects, containing the SSID, channel,
  ///   link quality and the type of security of the wireless network.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty list.
  /// </para>
  ///-
  function TYWireless.get_detectedWlans():TYWlanRecordArray;
    var
      json : TByteArray;
      wlanlist : TStringArray;
      res : TYWlanRecordArray;
      res_pos : LongInt;
      i_i : LongInt;
    begin
      SetLength(wlanlist, 0);
      // may throw an exception
      json := self._download('wlan.json?by=name');
      wlanlist := self._json_get_array(json);
      res_pos := 0;
      SetLength(res, length(wlanlist));;
      for i_i:=0 to length(wlanlist)-1 do
        begin
          res[res_pos] := TYWlanRecord.create(wlanlist[i_i]);
          inc(res_pos);
        end;
      result := res;
      exit;
    end;


  function TYWireless.nextWireless(): TYWireless;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextWireless := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextWireless := nil;
          exit;
        end;
      nextWireless := TYWireless.FindWireless(hwid);
    end;

  class function TYWireless.FirstWireless(): TYWireless;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Wireless', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYWireless.FindWireless(serial+'.'+funcId);
    end;

//--- (end of generated code: YWireless implementation)

//--- (generated code: Wireless functions)

  function yFindWireless(func:string): TYWireless;
    begin
      result := TYWireless.FindWireless(func);
    end;

  function yFirstWireless(): TYWireless;
    begin
      result := TYWireless.FirstWireless();
    end;

  procedure _WirelessCleanup();
    begin
    end;

//--- (end of generated code: Wireless functions)



//--- (generated code: YWlanRecord implementation)

  function TYWlanRecord.get_ssid():string;
    begin
      result := self._ssid;
      exit;
    end;


  function TYWlanRecord.get_channel():LongInt;
    begin
      result := self._channel;
      exit;
    end;


  function TYWlanRecord.get_security():string;
    begin
      result := self._sec;
      exit;
    end;


  function TYWlanRecord.get_linkQuality():LongInt;
    begin
      result := self._rssi;
      exit;
    end;


//--- (end of generated code: YWlanRecord implementation)


constructor TYWlanRecord.create(data:string);
 var
   p : TJSONparser;
   node : PJSONRECORD;
 begin
   p := TJsonParser.create(data,false);
   node:= p.GetChildNode(nil,'ssid');
   self._ssid:=string(node^.svalue);
   node:= p.GetChildNode(nil,'channel');
   self._channel:=node^.ivalue;
   node:= p.GetChildNode(nil,'sec');
   self._sec:=string(node^.svalue);
   node:= p.GetChildNode(nil,'rssi');
   self._rssi:=node^.ivalue;
   p.free;
 end;

//--- (generated code: WlanRecord functions)

  procedure _WlanRecordCleanup();
    begin
    end;

//--- (end of generated code: WlanRecord functions)

procedure freeWlanRecordArray(var list:TYWLANRECORDARRAY);
 var i:integer;
 begin
  for i:=0 to length(list)-1 do
   list[i].free();
  setLength(list,0);
 end;


initialization
   //--- (generated code: Wireless initialization)
  //--- (end of generated code: Wireless initialization)

finalization
   //--- (generated code: Wireless cleanup)
  _WirelessCleanup();
  //--- (end of generated code: Wireless cleanup)
end.
