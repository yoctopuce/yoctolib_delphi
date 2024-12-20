{*********************************************************************
 *
 * $Id: yocto_wireless.pas 63506 2024-11-28 10:42:13Z seb $
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
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
   sysutils, classes,{$IFNDEF UNIX}windows,
{$ENDIF}
 yocto_api, yjson;

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
const Y_WLANSTATE_DOWN = 0;
const Y_WLANSTATE_SCANNING = 1;
const Y_WLANSTATE_CONNECTED = 2;
const Y_WLANSTATE_REJECTED = 3;
const Y_WLANSTATE_INVALID = -1;

//--- (end of generated code: YWireless definitions)

type
TYWireless = class;
TYWlanRecord = class;
TYWlanRecordArr = array of TYWlanRecord;

  //--- (generated code: YWlanRecord class start)
  ////
  /// <summary>
  ///   T
  /// <para>
  ///   YWlanRecord Class: Wireless network description, returned by <c>wireless.get_detectedWlans</c> method
  /// </para>
  /// <para>
  ///   <c>YWlanRecord</c> objects are used to describe a wireless network.
  ///   These objects are  used in particular in conjunction with the
  ///   <c>YWireless</c> class.
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

    ////
    /// <summary>
    ///   Returns the name of the wireless network (SSID).
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with the name of the wireless network (SSID).
    /// </returns>
    ///-
    function get_ssid():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the 802.11 b/g/n channel number used by this network.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the channel.
    /// </returns>
    ///-
    function get_channel():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the security algorithm used by the wireless network.
    /// <para>
    ///   If the network implements to security, the value is <c>"OPEN"</c>.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with the security algorithm.
    /// </returns>
    ///-
    function get_security():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the quality of the wireless network link, in per cents.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer between 0 and 100 corresponding to the signal quality.
    /// </returns>
    ///-
    function get_linkQuality():LongInt; overload; virtual;


  //--- (end of generated code: YWlanRecord accessors declaration)
end;


TYWLANRECORDARRAY = array of TYWlanRecord;

//--- (generated code: YWireless class start)
  TYWirelessValueCallback = procedure(func: TYWireless; value:string);
  TYWirelessTimedReportCallback = procedure(func: TYWireless; value:TYMeasure);

  ////
  /// <summary>
  ///   TYWireless Class: wireless LAN interface control interface, available for instance in the
  ///   YoctoHub-Wireless, the YoctoHub-Wireless-SR, the YoctoHub-Wireless-g or the YoctoHub-Wireless-n
  /// <para>
  ///   The YWireless class provides control over wireless network parameters
  ///   and status for devices that are wireless-enabled.
  ///   Note that TCP/IP parameters are configured separately, using class <c>YNetwork</c>.
  /// </para>
  /// </summary>
  ///-
  TYWireless=class(TYFunction)
  //--- (end of generated code: YWireless class start)

//--- (generated code: YWireless declaration)
    // Attributes (function value cache)
    _linkQuality              : LongInt;
    _ssid                     : string;
    _channel                  : LongInt;
    _security                 : Integer;
    _message                  : string;
    _wlanConfig               : string;
    _wlanState                : Integer;
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
    ///   On failure, throws an exception or returns <c>YWireless.LINKQUALITY_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YWireless.SSID_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YWireless.CHANNEL_INVALID</c>.
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
    ///   a value among <c>YWireless.SECURITY_UNKNOWN</c>, <c>YWireless.SECURITY_OPEN</c>,
    ///   <c>YWireless.SECURITY_WEP</c>, <c>YWireless.SECURITY_WPA</c> and <c>YWireless.SECURITY_WPA2</c>
    ///   corresponding to the security algorithm used by the selected wireless network
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YWireless.SECURITY_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>YWireless.MESSAGE_INVALID</c>.
    /// </para>
    ///-
    function get_message():string;

    function get_wlanConfig():string;

    function set_wlanConfig(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the current state of the wireless interface.
    /// <para>
    ///   The state <c>YWireless.WLANSTATE_DOWN</c> means that the network interface is
    ///   not connected to a network. The state <c>YWireless.WLANSTATE_SCANNING</c> means that the network
    ///   interface is scanning available
    ///   frequencies. During this stage, the device is not reachable, and the network settings are not yet
    ///   applied. The state
    ///   <c>YWireless.WLANSTATE_CONNECTED</c> means that the network settings have been successfully applied
    ///   ant that the device is reachable
    ///   from the wireless network. If the device is configured to use ad-hoc or Soft AP mode, it means that
    ///   the wireless network
    ///   is up and that other devices can join the network. The state <c>YWireless.WLANSTATE_REJECTED</c>
    ///   means that the network interface has
    ///   not been able to join the requested network. The description of the error can be obtain with the
    ///   <c>get_message()</c> method.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YWireless.WLANSTATE_DOWN</c>, <c>YWireless.WLANSTATE_SCANNING</c>,
    ///   <c>YWireless.WLANSTATE_CONNECTED</c> and <c>YWireless.WLANSTATE_REJECTED</c> corresponding to the
    ///   current state of the wireless interface
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YWireless.WLANSTATE_INVALID</c>.
    /// </para>
    ///-
    function get_wlanState():Integer;

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
    ///   Triggers a scan of the wireless frequency and builds the list of available networks.
    /// <para>
    ///   The scan forces a disconnection from the current network. At then end of the process, the
    ///   the network interface attempts to reconnect to the previous network. During the scan, the <c>wlanState</c>
    ///   switches to <c>YWireless.WLANSTATE_DOWN</c>, then to <c>YWireless.WLANSTATE_SCANNING</c>. When the
    ///   scan is completed,
    ///   <c>get_wlanState()</c> returns either <c>YWireless.WLANSTATE_DOWN</c> or
    ///   <c>YWireless.WLANSTATE_SCANNING</c>. At this
    ///   point, the list of detected network can be retrieved with the <c>get_detectedWlans()</c> method.
    /// </para>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    /// </summary>
    ///-
    function startWlanScan():LongInt; overload; virtual;

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
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
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
    ///   On the YoctoHub-Wireless-g
    ///   and YoctoHub-Wireless-n,
    ///   you should use <c>softAPNetwork()</c> instead, which emulates an access point
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
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
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
    ///   used with the YoctoHub-Wireless-g and the YoctoHub-Wireless-n.
    /// </para>
    /// <para>
    ///   On the YoctoHub-Wireless-g, when a security key is specified for a SoftAP network,
    ///   the network is protected by a WEP40 key (5 characters or 10 hexadecimal digits) or
    ///   WEP128 key (13 characters or 26 hexadecimal digits). It is recommended to use a
    ///   well-randomized WEP128 key using 26 hexadecimal digits to maximize security.
    /// </para>
    /// <para>
    ///   On the YoctoHub-Wireless-n, when a security key is specified for a SoftAP network,
    ///   the network will be protected by WPA2.
    /// </para>
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
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function softAPNetwork(ssid: string; securityKey: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list of <c>YWlanRecord</c> objects that describe detected Wireless networks.
    /// <para>
    ///   This list is not updated when the module is already connected to an access point (infrastructure mode).
    ///   To force an update of this list, <c>startWlanScan()</c> must be called.
    ///   Note that an languages without garbage collections, the returned list must be freed by the caller.
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
    ///   Continues the enumeration of wireless LAN interfaces started using <c>yFirstWireless()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned wireless LAN interfaces order.
    ///   If you want to find a specific a wireless LAN interface, use <c>Wireless.findWireless()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YWireless</c> object, corresponding to
    ///   a wireless LAN interface currently online, or a <c>NIL</c> pointer
    ///   if there are no more wireless LAN interfaces to enumerate.
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

//--- (generated code: YWireless functions declaration)
  ////
  /// <summary>
  ///   Retrieves a wireless LAN interface for a given identifier.
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
  ///   This function does not require that the wireless LAN interface is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YWireless.isOnline()</c> to test if the wireless LAN interface is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a wireless LAN interface by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the wireless LAN interface, for instance
  ///   <c>YHUBWLN1.wireless</c>.
  /// </param>
  /// <returns>
  ///   a <c>YWireless</c> object allowing you to drive the wireless LAN interface.
  /// </returns>
  ///-
  function yFindWireless(func:string):TYWireless;
  ////
  /// <summary>
  ///   Starts the enumeration of wireless LAN interfaces currently accessible.
  /// <para>
  ///   Use the method <c>YWireless.nextWireless()</c> to iterate on
  ///   next wireless LAN interfaces.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YWireless</c> object, corresponding to
  ///   the first wireless LAN interface currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstWireless():TYWireless;

//--- (end of generated code: YWireless functions declaration)

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
      _wlanState := Y_WLANSTATE_INVALID;
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
      if (member^.name = 'wlanState') then
        begin
          _wlanState := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYWireless.get_linkQuality():LongInt;
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


  function TYWireless.get_ssid():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SSID_INVALID;
              exit;
            end;
        end;
      res := self._ssid;
      result := res;
      exit;
    end;


  function TYWireless.get_channel():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CHANNEL_INVALID;
              exit;
            end;
        end;
      res := self._channel;
      result := res;
      exit;
    end;


  function TYWireless.get_security():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SECURITY_INVALID;
              exit;
            end;
        end;
      res := self._security;
      result := res;
      exit;
    end;


  function TYWireless.get_message():string;
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


  function TYWireless.get_wlanConfig():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
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

  function TYWireless.get_wlanState():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_WLANSTATE_INVALID;
              exit;
            end;
        end;
      res := self._wlanState;
      result := res;
      exit;
    end;


  class function TYWireless.FindWireless(func: string):TYWireless;
    var
      obj : TYWireless;
    begin
      obj := TYWireless(TYFunction._FindFromCache('Wireless', func));
      if obj = nil then
        begin
          obj :=  TYWireless.create(func);
          TYFunction._AddToCache('Wireless', func, obj);
        end;
      result := obj;
      exit;
    end;


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


  function TYWireless.startWlanScan():LongInt;
    var
      config : string;
    begin
      config := self.get_wlanConfig;
      // a full scan is triggered when a config is applied
      result := self.set_wlanConfig(config);
      exit;
    end;


  function TYWireless.joinNetwork(ssid: string; securityKey: string):LongInt;
    begin
      result := self.set_wlanConfig('INFRA:'+ssid+'\'+securityKey);
      exit;
    end;


  function TYWireless.adhocNetwork(ssid: string; securityKey: string):LongInt;
    begin
      result := self.set_wlanConfig('ADHOC:'+ssid+'\'+securityKey);
      exit;
    end;


  function TYWireless.softAPNetwork(ssid: string; securityKey: string):LongInt;
    begin
      result := self.set_wlanConfig('SOFTAP:'+ssid+'\'+securityKey);
      exit;
    end;


  function TYWireless.get_detectedWlans():TYWlanRecordArray;
    var
      json : TByteArray;
      wlanlist : TStringArray;
      res : TYWlanRecordArray;
      res_pos : LongInt;
      ii_0 : LongInt;
    begin
      SetLength(wlanlist, 0);

      json := self._download('wlan.json?by=name');
      wlanlist := self._json_get_array(json);
      res_pos := 0;
      SetLength(res, length(wlanlist));;
      for ii_0:=0 to length(wlanlist)-1 do
        begin
          res[res_pos] := TYWlanRecord.create(wlanlist[ii_0]);
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

//--- (generated code: YWireless functions)

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

//--- (end of generated code: YWireless functions)



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

//--- (generated code: YWlanRecord functions)

  procedure _WlanRecordCleanup();
    begin
    end;

//--- (end of generated code: YWlanRecord functions)

procedure freeWlanRecordArray(var list:TYWLANRECORDARRAY);
 var i:integer;
 begin
  for i:=0 to length(list)-1 do
   list[i].free();
  setLength(list,0);
 end;


initialization
   //--- (generated code: YWireless initialization)
  //--- (end of generated code: YWireless initialization)

finalization
   //--- (generated code: YWireless cleanup)
  _WirelessCleanup();
  //--- (end of generated code: YWireless cleanup)
end.
