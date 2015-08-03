{*********************************************************************
 *
 * $Id: yocto_network.pas 20815 2015-07-09 17:23:30Z mvuilleu $
 *
 * Implements yFindNetwork(), the high-level API for Network functions
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


unit yocto_network;

interface

uses
  sysutils, classes, windows, yocto_api, yjson;

//--- (YNetwork definitions)

const Y_READINESS_DOWN = 0;
const Y_READINESS_EXISTS = 1;
const Y_READINESS_LINKED = 2;
const Y_READINESS_LAN_OK = 3;
const Y_READINESS_WWW_OK = 4;
const Y_READINESS_INVALID = -1;
const Y_MACADDRESS_INVALID            = YAPI_INVALID_STRING;
const Y_IPADDRESS_INVALID             = YAPI_INVALID_STRING;
const Y_SUBNETMASK_INVALID            = YAPI_INVALID_STRING;
const Y_ROUTER_INVALID                = YAPI_INVALID_STRING;
const Y_IPCONFIG_INVALID              = YAPI_INVALID_STRING;
const Y_PRIMARYDNS_INVALID            = YAPI_INVALID_STRING;
const Y_SECONDARYDNS_INVALID          = YAPI_INVALID_STRING;
const Y_NTPSERVER_INVALID             = YAPI_INVALID_STRING;
const Y_USERPASSWORD_INVALID          = YAPI_INVALID_STRING;
const Y_ADMINPASSWORD_INVALID         = YAPI_INVALID_STRING;
const Y_HTTPPORT_INVALID              = YAPI_INVALID_UINT;
const Y_DEFAULTPAGE_INVALID           = YAPI_INVALID_STRING;
const Y_DISCOVERABLE_FALSE = 0;
const Y_DISCOVERABLE_TRUE = 1;
const Y_DISCOVERABLE_INVALID = -1;
const Y_WWWWATCHDOGDELAY_INVALID      = YAPI_INVALID_UINT;
const Y_CALLBACKURL_INVALID           = YAPI_INVALID_STRING;
const Y_CALLBACKMETHOD_POST = 0;
const Y_CALLBACKMETHOD_GET = 1;
const Y_CALLBACKMETHOD_PUT = 2;
const Y_CALLBACKMETHOD_INVALID = -1;
const Y_CALLBACKENCODING_FORM = 0;
const Y_CALLBACKENCODING_JSON = 1;
const Y_CALLBACKENCODING_JSON_ARRAY = 2;
const Y_CALLBACKENCODING_CSV = 3;
const Y_CALLBACKENCODING_YOCTO_API = 4;
const Y_CALLBACKENCODING_JSON_NUM = 5;
const Y_CALLBACKENCODING_EMONCMS = 6;
const Y_CALLBACKENCODING_AZURE = 7;
const Y_CALLBACKENCODING_INVALID = -1;
const Y_CALLBACKCREDENTIALS_INVALID   = YAPI_INVALID_STRING;
const Y_CALLBACKMINDELAY_INVALID      = YAPI_INVALID_UINT;
const Y_CALLBACKMAXDELAY_INVALID      = YAPI_INVALID_UINT;
const Y_POECURRENT_INVALID            = YAPI_INVALID_UINT;


//--- (end of YNetwork definitions)

type
  TYNetwork = class;
  //--- (YNetwork class start)
  TYNetworkValueCallback = procedure(func: TYNetwork; value:string);
  TYNetworkTimedReportCallback = procedure(func: TYNetwork; value:TYMeasure);

  ////
  /// <summary>
  ///   TYNetwork Class: Network function interface
  /// <para>
  ///   YNetwork objects provide access to TCP/IP parameters of Yoctopuce
  ///   modules that include a built-in network interface.
  /// </para>
  /// </summary>
  ///-
  TYNetwork=class(TYFunction)
  //--- (end of YNetwork class start)
  protected
  //--- (YNetwork declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _readiness                : Integer;
    _macAddress               : string;
    _ipAddress                : string;
    _subnetMask               : string;
    _router                   : string;
    _ipConfig                 : string;
    _primaryDNS               : string;
    _secondaryDNS             : string;
    _ntpServer                : string;
    _userPassword             : string;
    _adminPassword            : string;
    _httpPort                 : LongInt;
    _defaultPage              : string;
    _discoverable             : Integer;
    _wwwWatchdogDelay         : LongInt;
    _callbackUrl              : string;
    _callbackMethod           : Integer;
    _callbackEncoding         : Integer;
    _callbackCredentials      : string;
    _callbackMinDelay         : LongInt;
    _callbackMaxDelay         : LongInt;
    _poeCurrent               : LongInt;
    _valueCallbackNetwork     : TYNetworkValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of YNetwork declaration)

  public
    //--- (YNetwork accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the current established working mode of the network interface.
    /// <para>
    ///   Level zero (DOWN_0) means that no hardware link has been detected. Either there is no signal
    ///   on the network cable, or the selected wireless access point cannot be detected.
    ///   Level 1 (LIVE_1) is reached when the network is detected, but is not yet connected.
    ///   For a wireless network, this shows that the requested SSID is present.
    ///   Level 2 (LINK_2) is reached when the hardware connection is established.
    ///   For a wired network connection, level 2 means that the cable is attached at both ends.
    ///   For a connection to a wireless access point, it shows that the security parameters
    ///   are properly configured. For an ad-hoc wireless connection, it means that there is
    ///   at least one other device connected on the ad-hoc network.
    ///   Level 3 (DHCP_3) is reached when an IP address has been obtained using DHCP.
    ///   Level 4 (DNS_4) is reached when the DNS server is reachable on the network.
    ///   Level 5 (WWW_5) is reached when global connectivity is demonstrated by properly loading the
    ///   current time from an NTP server.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_READINESS_DOWN</c>, <c>Y_READINESS_EXISTS</c>, <c>Y_READINESS_LINKED</c>,
    ///   <c>Y_READINESS_LAN_OK</c> and <c>Y_READINESS_WWW_OK</c> corresponding to the current established
    ///   working mode of the network interface
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_READINESS_INVALID</c>.
    /// </para>
    ///-
    function get_readiness():Integer;

    ////
    /// <summary>
    ///   Returns the MAC address of the network interface.
    /// <para>
    ///   The MAC address is also available on a sticker
    ///   on the module, in both numeric and barcode forms.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the MAC address of the network interface
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_MACADDRESS_INVALID</c>.
    /// </para>
    ///-
    function get_macAddress():string;

    ////
    /// <summary>
    ///   Returns the IP address currently in use by the device.
    /// <para>
    ///   The address may have been configured
    ///   statically, or provided by a DHCP server.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the IP address currently in use by the device
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_IPADDRESS_INVALID</c>.
    /// </para>
    ///-
    function get_ipAddress():string;

    ////
    /// <summary>
    ///   Returns the subnet mask currently used by the device.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the subnet mask currently used by the device
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SUBNETMASK_INVALID</c>.
    /// </para>
    ///-
    function get_subnetMask():string;

    ////
    /// <summary>
    ///   Returns the IP address of the router on the device subnet (default gateway).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the IP address of the router on the device subnet (default gateway)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ROUTER_INVALID</c>.
    /// </para>
    ///-
    function get_router():string;

    function get_ipConfig():string;

    function set_ipConfig(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the IP address of the primary name server to be used by the module.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the IP address of the primary name server to be used by the module
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PRIMARYDNS_INVALID</c>.
    /// </para>
    ///-
    function get_primaryDNS():string;

    ////
    /// <summary>
    ///   Changes the IP address of the primary name server to be used by the module.
    /// <para>
    ///   When using DHCP, if a value is specified, it overrides the value received from the DHCP server.
    ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the IP address of the primary name server to be used by the module
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
    function set_primaryDNS(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the IP address of the secondary name server to be used by the module.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the IP address of the secondary name server to be used by the module
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_SECONDARYDNS_INVALID</c>.
    /// </para>
    ///-
    function get_secondaryDNS():string;

    ////
    /// <summary>
    ///   Changes the IP address of the secondary name server to be used by the module.
    /// <para>
    ///   When using DHCP, if a value is specified, it overrides the value received from the DHCP server.
    ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the IP address of the secondary name server to be used by the module
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
    function set_secondaryDNS(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the IP address of the NTP server to be used by the device.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the IP address of the NTP server to be used by the device
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_NTPSERVER_INVALID</c>.
    /// </para>
    ///-
    function get_ntpServer():string;

    ////
    /// <summary>
    ///   Changes the IP address of the NTP server to be used by the module.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the IP address of the NTP server to be used by the module
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
    function set_ntpServer(newval:string):integer;

    ////
    /// <summary>
    ///   Returns a hash string if a password has been set for "user" user,
    ///   or an empty string otherwise.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to a hash string if a password has been set for "user" user,
    ///   or an empty string otherwise
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_USERPASSWORD_INVALID</c>.
    /// </para>
    ///-
    function get_userPassword():string;

    ////
    /// <summary>
    ///   Changes the password for the "user" user.
    /// <para>
    ///   This password becomes instantly required
    ///   to perform any use of the module. If the specified value is an
    ///   empty string, a password is not required anymore.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the password for the "user" user
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
    function set_userPassword(newval:string):integer;

    ////
    /// <summary>
    ///   Returns a hash string if a password has been set for user "admin",
    ///   or an empty string otherwise.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to a hash string if a password has been set for user "admin",
    ///   or an empty string otherwise
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ADMINPASSWORD_INVALID</c>.
    /// </para>
    ///-
    function get_adminPassword():string;

    ////
    /// <summary>
    ///   Changes the password for the "admin" user.
    /// <para>
    ///   This password becomes instantly required
    ///   to perform any change of the module state. If the specified value is an
    ///   empty string, a password is not required anymore.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the password for the "admin" user
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
    function set_adminPassword(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the HTML page to serve for the URL "/"" of the hub.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the HTML page to serve for the URL "/"" of the hub
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_HTTPPORT_INVALID</c>.
    /// </para>
    ///-
    function get_httpPort():LongInt;

    ////
    /// <summary>
    ///   Changes the default HTML page returned by the hub.
    /// <para>
    ///   If not value are set the hub return
    ///   "index.html" which is the web interface of the hub. It is possible de change this page
    ///   for file that has been uploaded on the hub.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the default HTML page returned by the hub
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
    function set_httpPort(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the HTML page to serve for the URL "/"" of the hub.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the HTML page to serve for the URL "/"" of the hub
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DEFAULTPAGE_INVALID</c>.
    /// </para>
    ///-
    function get_defaultPage():string;

    ////
    /// <summary>
    ///   Changes the default HTML page returned by the hub.
    /// <para>
    ///   If not value are set the hub return
    ///   "index.html" which is the web interface of the hub. It is possible de change this page
    ///   for file that has been uploaded on the hub.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the default HTML page returned by the hub
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
    function set_defaultPage(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the activation state of the multicast announce protocols to allow easy
    ///   discovery of the module in the network neighborhood (uPnP/Bonjour protocol).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_DISCOVERABLE_FALSE</c> or <c>Y_DISCOVERABLE_TRUE</c>, according to the activation state
    ///   of the multicast announce protocols to allow easy
    ///   discovery of the module in the network neighborhood (uPnP/Bonjour protocol)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_DISCOVERABLE_INVALID</c>.
    /// </para>
    ///-
    function get_discoverable():Integer;

    ////
    /// <summary>
    ///   Changes the activation state of the multicast announce protocols to allow easy
    ///   discovery of the module in the network neighborhood (uPnP/Bonjour protocol).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>Y_DISCOVERABLE_FALSE</c> or <c>Y_DISCOVERABLE_TRUE</c>, according to the activation state
    ///   of the multicast announce protocols to allow easy
    ///   discovery of the module in the network neighborhood (uPnP/Bonjour protocol)
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
    function set_discoverable(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the allowed downtime of the WWW link (in seconds) before triggering an automated
    ///   reboot to try to recover Internet connectivity.
    /// <para>
    ///   A zero value disables automated reboot
    ///   in case of Internet connectivity loss.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the allowed downtime of the WWW link (in seconds) before triggering an automated
    ///   reboot to try to recover Internet connectivity
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_WWWWATCHDOGDELAY_INVALID</c>.
    /// </para>
    ///-
    function get_wwwWatchdogDelay():LongInt;

    ////
    /// <summary>
    ///   Changes the allowed downtime of the WWW link (in seconds) before triggering an automated
    ///   reboot to try to recover Internet connectivity.
    /// <para>
    ///   A zero value disables automated reboot
    ///   in case of Internet connectivity loss. The smallest valid non-zero timeout is
    ///   90 seconds.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the allowed downtime of the WWW link (in seconds) before triggering an automated
    ///   reboot to try to recover Internet connectivity
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
    function set_wwwWatchdogDelay(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the callback URL to notify of significant state changes.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the callback URL to notify of significant state changes
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CALLBACKURL_INVALID</c>.
    /// </para>
    ///-
    function get_callbackUrl():string;

    ////
    /// <summary>
    ///   Changes the callback URL to notify significant state changes.
    /// <para>
    ///   Remember to call the
    ///   <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the callback URL to notify significant state changes
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
    function set_callbackUrl(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the HTTP method used to notify callbacks for significant state changes.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_CALLBACKMETHOD_POST</c>, <c>Y_CALLBACKMETHOD_GET</c> and
    ///   <c>Y_CALLBACKMETHOD_PUT</c> corresponding to the HTTP method used to notify callbacks for
    ///   significant state changes
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CALLBACKMETHOD_INVALID</c>.
    /// </para>
    ///-
    function get_callbackMethod():Integer;

    ////
    /// <summary>
    ///   Changes the HTTP method used to notify callbacks for significant state changes.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_CALLBACKMETHOD_POST</c>, <c>Y_CALLBACKMETHOD_GET</c> and
    ///   <c>Y_CALLBACKMETHOD_PUT</c> corresponding to the HTTP method used to notify callbacks for
    ///   significant state changes
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
    function set_callbackMethod(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the encoding standard to use for representing notification values.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>Y_CALLBACKENCODING_FORM</c>, <c>Y_CALLBACKENCODING_JSON</c>,
    ///   <c>Y_CALLBACKENCODING_JSON_ARRAY</c>, <c>Y_CALLBACKENCODING_CSV</c>,
    ///   <c>Y_CALLBACKENCODING_YOCTO_API</c>, <c>Y_CALLBACKENCODING_JSON_NUM</c>,
    ///   <c>Y_CALLBACKENCODING_EMONCMS</c> and <c>Y_CALLBACKENCODING_AZURE</c> corresponding to the encoding
    ///   standard to use for representing notification values
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CALLBACKENCODING_INVALID</c>.
    /// </para>
    ///-
    function get_callbackEncoding():Integer;

    ////
    /// <summary>
    ///   Changes the encoding standard to use for representing notification values.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>Y_CALLBACKENCODING_FORM</c>, <c>Y_CALLBACKENCODING_JSON</c>,
    ///   <c>Y_CALLBACKENCODING_JSON_ARRAY</c>, <c>Y_CALLBACKENCODING_CSV</c>,
    ///   <c>Y_CALLBACKENCODING_YOCTO_API</c>, <c>Y_CALLBACKENCODING_JSON_NUM</c>,
    ///   <c>Y_CALLBACKENCODING_EMONCMS</c> and <c>Y_CALLBACKENCODING_AZURE</c> corresponding to the encoding
    ///   standard to use for representing notification values
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
    function set_callbackEncoding(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns a hashed version of the notification callback credentials if set,
    ///   or an empty string otherwise.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to a hashed version of the notification callback credentials if set,
    ///   or an empty string otherwise
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CALLBACKCREDENTIALS_INVALID</c>.
    /// </para>
    ///-
    function get_callbackCredentials():string;

    ////
    /// <summary>
    ///   Changes the credentials required to connect to the callback address.
    /// <para>
    ///   The credentials
    ///   must be provided as returned by function <c>get_callbackCredentials</c>,
    ///   in the form <c>username:hash</c>. The method used to compute the hash varies according
    ///   to the the authentication scheme implemented by the callback, For Basic authentication,
    ///   the hash is the MD5 of the string <c>username:password</c>. For Digest authentication,
    ///   the hash is the MD5 of the string <c>username:realm:password</c>. For a simpler
    ///   way to configure callback credentials, use function <c>callbackLogin</c> instead.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the credentials required to connect to the callback address
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
    function set_callbackCredentials(newval:string):integer;

    ////
    /// <summary>
    ///   Connects to the notification callback and saves the credentials required to
    ///   log into it.
    /// <para>
    ///   The password is not stored into the module, only a hashed
    ///   copy of the credentials are saved. Remember to call the
    ///   <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="username">
    ///   username required to log to the callback
    /// </param>
    /// <param name="password">
    ///   password required to log to the callback
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
    function callbackLogin(username: string; password: string):integer;

    ////
    /// <summary>
    ///   Returns the minimum waiting time between two callback notifications, in seconds.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the minimum waiting time between two callback notifications, in seconds
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CALLBACKMINDELAY_INVALID</c>.
    /// </para>
    ///-
    function get_callbackMinDelay():LongInt;

    ////
    /// <summary>
    ///   Changes the minimum waiting time between two callback notifications, in seconds.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the minimum waiting time between two callback notifications, in seconds
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
    function set_callbackMinDelay(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the maximum waiting time between two callback notifications, in seconds.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the maximum waiting time between two callback notifications, in seconds
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CALLBACKMAXDELAY_INVALID</c>.
    /// </para>
    ///-
    function get_callbackMaxDelay():LongInt;

    ////
    /// <summary>
    ///   Changes the maximum waiting time between two callback notifications, in seconds.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the maximum waiting time between two callback notifications, in seconds
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
    function set_callbackMaxDelay(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the current consumed by the module from Power-over-Ethernet (PoE), in milli-amps.
    /// <para>
    ///   The current consumption is measured after converting PoE source to 5 Volt, and should
    ///   never exceed 1800 mA.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current consumed by the module from Power-over-Ethernet (PoE), in milli-amps
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_POECURRENT_INVALID</c>.
    /// </para>
    ///-
    function get_poeCurrent():LongInt;

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
    ///   Use the method <c>YNetwork.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YNetwork</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindNetwork(func: string):TYNetwork;

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
    function registerValueCallback(callback: TYNetworkValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Changes the configuration of the network interface to enable the use of an
    ///   IP address received from a DHCP server.
    /// <para>
    ///   Until an address is received from a DHCP
    ///   server, the module uses the IP parameters specified to this function.
    ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
    /// </para>
    /// </summary>
    /// <param name="fallbackIpAddr">
    ///   fallback IP address, to be used when no DHCP reply is received
    /// </param>
    /// <param name="fallbackSubnetMaskLen">
    ///   fallback subnet mask length when no DHCP reply is received, as an
    ///   integer (eg. 24 means 255.255.255.0)
    /// </param>
    /// <param name="fallbackRouter">
    ///   fallback router IP address, to be used when no DHCP reply is received
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function useDHCP(fallbackIpAddr: string; fallbackSubnetMaskLen: LongInt; fallbackRouter: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes the configuration of the network interface to use a static IP address.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
    /// </para>
    /// </summary>
    /// <param name="ipAddress">
    ///   device IP address
    /// </param>
    /// <param name="subnetMaskLen">
    ///   subnet mask length, as an integer (eg. 24 means 255.255.255.0)
    /// </param>
    /// <param name="router">
    ///   router IP address (default gateway)
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function useStaticIP(ipAddress: string; subnetMaskLen: LongInt; router: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Pings str_host to test the network connectivity.
    /// <para>
    ///   Sends four ICMP ECHO_REQUEST requests from the
    ///   module to the target str_host. This method returns a string with the result of the
    ///   4 ICMP ECHO_REQUEST requests.
    /// </para>
    /// </summary>
    /// <param name="host">
    ///   the hostname or the IP address of the target
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   a string with the result of the ping.
    /// </returns>
    ///-
    function ping(host: string):string; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of network interfaces started using <c>yFirstNetwork()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YNetwork</c> object, corresponding to
    ///   a network interface currently online, or a <c>null</c> pointer
    ///   if there are no more network interfaces to enumerate.
    /// </returns>
    ///-
    function nextNetwork():TYNetwork;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstNetwork():TYNetwork;
  //--- (end of YNetwork accessors declaration)
  end;

//--- (Network functions declaration)
  ////
  /// <summary>
  ///   Retrieves a network interface for a given identifier.
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
  ///   This function does not require that the network interface is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YNetwork.isOnline()</c> to test if the network interface is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a network interface by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the network interface
  /// </param>
  /// <returns>
  ///   a <c>YNetwork</c> object allowing you to drive the network interface.
  /// </returns>
  ///-
  function yFindNetwork(func:string):TYNetwork;
  ////
  /// <summary>
  ///   Starts the enumeration of network interfaces currently accessible.
  /// <para>
  ///   Use the method <c>YNetwork.nextNetwork()</c> to iterate on
  ///   next network interfaces.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YNetwork</c> object, corresponding to
  ///   the first network interface currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstNetwork():TYNetwork;

//--- (end of Network functions declaration)

implementation
//--- (YNetwork dlldef)
//--- (end of YNetwork dlldef)

  constructor TYNetwork.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Network';
      //--- (YNetwork accessors initialization)
      _readiness := Y_READINESS_INVALID;
      _macAddress := Y_MACADDRESS_INVALID;
      _ipAddress := Y_IPADDRESS_INVALID;
      _subnetMask := Y_SUBNETMASK_INVALID;
      _router := Y_ROUTER_INVALID;
      _ipConfig := Y_IPCONFIG_INVALID;
      _primaryDNS := Y_PRIMARYDNS_INVALID;
      _secondaryDNS := Y_SECONDARYDNS_INVALID;
      _ntpServer := Y_NTPSERVER_INVALID;
      _userPassword := Y_USERPASSWORD_INVALID;
      _adminPassword := Y_ADMINPASSWORD_INVALID;
      _httpPort := Y_HTTPPORT_INVALID;
      _defaultPage := Y_DEFAULTPAGE_INVALID;
      _discoverable := Y_DISCOVERABLE_INVALID;
      _wwwWatchdogDelay := Y_WWWWATCHDOGDELAY_INVALID;
      _callbackUrl := Y_CALLBACKURL_INVALID;
      _callbackMethod := Y_CALLBACKMETHOD_INVALID;
      _callbackEncoding := Y_CALLBACKENCODING_INVALID;
      _callbackCredentials := Y_CALLBACKCREDENTIALS_INVALID;
      _callbackMinDelay := Y_CALLBACKMINDELAY_INVALID;
      _callbackMaxDelay := Y_CALLBACKMAXDELAY_INVALID;
      _poeCurrent := Y_POECURRENT_INVALID;
      _valueCallbackNetwork := nil;
      //--- (end of YNetwork accessors initialization)
    end;


//--- (YNetwork implementation)
{$HINTS OFF}
  function TYNetwork._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'readiness') then
        begin
          _readiness := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'macAddress') then
        begin
          _macAddress := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'ipAddress') then
        begin
          _ipAddress := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'subnetMask') then
        begin
          _subnetMask := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'router') then
        begin
          _router := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'ipConfig') then
        begin
          _ipConfig := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'primaryDNS') then
        begin
          _primaryDNS := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'secondaryDNS') then
        begin
          _secondaryDNS := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'ntpServer') then
        begin
          _ntpServer := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'userPassword') then
        begin
          _userPassword := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'adminPassword') then
        begin
          _adminPassword := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'httpPort') then
        begin
          _httpPort := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'defaultPage') then
        begin
          _defaultPage := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'discoverable') then
        begin
          _discoverable := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'wwwWatchdogDelay') then
        begin
          _wwwWatchdogDelay := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'callbackUrl') then
        begin
          _callbackUrl := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'callbackMethod') then
        begin
          _callbackMethod := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'callbackEncoding') then
        begin
          _callbackEncoding := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'callbackCredentials') then
        begin
          _callbackCredentials := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'callbackMinDelay') then
        begin
          _callbackMinDelay := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'callbackMaxDelay') then
        begin
          _callbackMaxDelay := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'poeCurrent') then
        begin
          _poeCurrent := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Returns the current established working mode of the network interface.
  /// <para>
  ///   Level zero (DOWN_0) means that no hardware link has been detected. Either there is no signal
  ///   on the network cable, or the selected wireless access point cannot be detected.
  ///   Level 1 (LIVE_1) is reached when the network is detected, but is not yet connected.
  ///   For a wireless network, this shows that the requested SSID is present.
  ///   Level 2 (LINK_2) is reached when the hardware connection is established.
  ///   For a wired network connection, level 2 means that the cable is attached at both ends.
  ///   For a connection to a wireless access point, it shows that the security parameters
  ///   are properly configured. For an ad-hoc wireless connection, it means that there is
  ///   at least one other device connected on the ad-hoc network.
  ///   Level 3 (DHCP_3) is reached when an IP address has been obtained using DHCP.
  ///   Level 4 (DNS_4) is reached when the DNS server is reachable on the network.
  ///   Level 5 (WWW_5) is reached when global connectivity is demonstrated by properly loading the
  ///   current time from an NTP server.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_READINESS_DOWN, Y_READINESS_EXISTS, Y_READINESS_LINKED, Y_READINESS_LAN_OK and
  ///   Y_READINESS_WWW_OK corresponding to the current established working mode of the network interface
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_READINESS_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_readiness():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_READINESS_INVALID;
              exit
            end;
        end;
      result := self._readiness;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the MAC address of the network interface.
  /// <para>
  ///   The MAC address is also available on a sticker
  ///   on the module, in both numeric and barcode forms.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the MAC address of the network interface
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_MACADDRESS_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_macAddress():string;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_MACADDRESS_INVALID;
              exit
            end;
        end;
      result := self._macAddress;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the IP address currently in use by the device.
  /// <para>
  ///   The address may have been configured
  ///   statically, or provided by a DHCP server.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the IP address currently in use by the device
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_IPADDRESS_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_ipAddress():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_IPADDRESS_INVALID;
              exit
            end;
        end;
      result := self._ipAddress;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the subnet mask currently used by the device.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the subnet mask currently used by the device
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_SUBNETMASK_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_subnetMask():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SUBNETMASK_INVALID;
              exit
            end;
        end;
      result := self._subnetMask;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the IP address of the router on the device subnet (default gateway).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the IP address of the router on the device subnet (default gateway)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ROUTER_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_router():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ROUTER_INVALID;
              exit
            end;
        end;
      result := self._router;
      exit;
    end;


  function TYNetwork.get_ipConfig():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_IPCONFIG_INVALID;
              exit
            end;
        end;
      result := self._ipConfig;
      exit;
    end;


  function TYNetwork.set_ipConfig(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('ipConfig',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the IP address of the primary name server to be used by the module.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the IP address of the primary name server to be used by the module
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PRIMARYDNS_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_primaryDNS():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PRIMARYDNS_INVALID;
              exit
            end;
        end;
      result := self._primaryDNS;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the IP address of the primary name server to be used by the module.
  /// <para>
  ///   When using DHCP, if a value is specified, it overrides the value received from the DHCP server.
  ///   Remember to call the saveToFlash() method and then to reboot the module to apply this setting.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the IP address of the primary name server to be used by the module
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
  function TYNetwork.set_primaryDNS(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('primaryDNS',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the IP address of the secondary name server to be used by the module.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the IP address of the secondary name server to be used by the module
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_SECONDARYDNS_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_secondaryDNS():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SECONDARYDNS_INVALID;
              exit
            end;
        end;
      result := self._secondaryDNS;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the IP address of the secondary name server to be used by the module.
  /// <para>
  ///   When using DHCP, if a value is specified, it overrides the value received from the DHCP server.
  ///   Remember to call the saveToFlash() method and then to reboot the module to apply this setting.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the IP address of the secondary name server to be used by the module
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
  function TYNetwork.set_secondaryDNS(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('secondaryDNS',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the IP address of the NTP server to be used by the device.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the IP address of the NTP server to be used by the device
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_NTPSERVER_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_ntpServer():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_NTPSERVER_INVALID;
              exit
            end;
        end;
      result := self._ntpServer;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the IP address of the NTP server to be used by the module.
  /// <para>
  ///   Remember to call the saveToFlash() method and then to reboot the module to apply this setting.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the IP address of the NTP server to be used by the module
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
  function TYNetwork.set_ntpServer(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('ntpServer',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns a hash string if a password has been set for "user" user,
  ///   or an empty string otherwise.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to a hash string if a password has been set for "user" user,
  ///   or an empty string otherwise
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_USERPASSWORD_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_userPassword():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_USERPASSWORD_INVALID;
              exit
            end;
        end;
      result := self._userPassword;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the password for the "user" user.
  /// <para>
  ///   This password becomes instantly required
  ///   to perform any use of the module. If the specified value is an
  ///   empty string, a password is not required anymore.
  ///   Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the password for the "user" user
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
  function TYNetwork.set_userPassword(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('userPassword',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns a hash string if a password has been set for user "admin",
  ///   or an empty string otherwise.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to a hash string if a password has been set for user "admin",
  ///   or an empty string otherwise
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ADMINPASSWORD_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_adminPassword():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ADMINPASSWORD_INVALID;
              exit
            end;
        end;
      result := self._adminPassword;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the password for the "admin" user.
  /// <para>
  ///   This password becomes instantly required
  ///   to perform any change of the module state. If the specified value is an
  ///   empty string, a password is not required anymore.
  ///   Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the password for the "admin" user
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
  function TYNetwork.set_adminPassword(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('adminPassword',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the HTML page to serve for the URL "/"" of the hub.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the HTML page to serve for the URL "/"" of the hub
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_HTTPPORT_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_httpPort():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_HTTPPORT_INVALID;
              exit
            end;
        end;
      result := self._httpPort;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the default HTML page returned by the hub.
  /// <para>
  ///   If not value are set the hub return
  ///   "index.html" which is the web interface of the hub. It is possible de change this page
  ///   for file that has been uploaded on the hub.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the default HTML page returned by the hub
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
  function TYNetwork.set_httpPort(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('httpPort',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the HTML page to serve for the URL "/"" of the hub.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the HTML page to serve for the URL "/"" of the hub
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DEFAULTPAGE_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_defaultPage():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_DEFAULTPAGE_INVALID;
              exit
            end;
        end;
      result := self._defaultPage;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the default HTML page returned by the hub.
  /// <para>
  ///   If not value are set the hub return
  ///   "index.html" which is the web interface of the hub. It is possible de change this page
  ///   for file that has been uploaded on the hub.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the default HTML page returned by the hub
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
  function TYNetwork.set_defaultPage(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('defaultPage',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the activation state of the multicast announce protocols to allow easy
  ///   discovery of the module in the network neighborhood (uPnP/Bonjour protocol).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_DISCOVERABLE_FALSE or Y_DISCOVERABLE_TRUE, according to the activation state of the
  ///   multicast announce protocols to allow easy
  ///   discovery of the module in the network neighborhood (uPnP/Bonjour protocol)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DISCOVERABLE_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_discoverable():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_DISCOVERABLE_INVALID;
              exit
            end;
        end;
      result := self._discoverable;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the activation state of the multicast announce protocols to allow easy
  ///   discovery of the module in the network neighborhood (uPnP/Bonjour protocol).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   either Y_DISCOVERABLE_FALSE or Y_DISCOVERABLE_TRUE, according to the activation state of the
  ///   multicast announce protocols to allow easy
  ///   discovery of the module in the network neighborhood (uPnP/Bonjour protocol)
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
  function TYNetwork.set_discoverable(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('discoverable',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the allowed downtime of the WWW link (in seconds) before triggering an automated
  ///   reboot to try to recover Internet connectivity.
  /// <para>
  ///   A zero value disables automated reboot
  ///   in case of Internet connectivity loss.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the allowed downtime of the WWW link (in seconds) before triggering an automated
  ///   reboot to try to recover Internet connectivity
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_WWWWATCHDOGDELAY_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_wwwWatchdogDelay():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_WWWWATCHDOGDELAY_INVALID;
              exit
            end;
        end;
      result := self._wwwWatchdogDelay;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the allowed downtime of the WWW link (in seconds) before triggering an automated
  ///   reboot to try to recover Internet connectivity.
  /// <para>
  ///   A zero value disables automated reboot
  ///   in case of Internet connectivity loss. The smallest valid non-zero timeout is
  ///   90 seconds.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the allowed downtime of the WWW link (in seconds) before triggering an automated
  ///   reboot to try to recover Internet connectivity
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
  function TYNetwork.set_wwwWatchdogDelay(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('wwwWatchdogDelay',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the callback URL to notify of significant state changes.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the callback URL to notify of significant state changes
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_CALLBACKURL_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_callbackUrl():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CALLBACKURL_INVALID;
              exit
            end;
        end;
      result := self._callbackUrl;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the callback URL to notify significant state changes.
  /// <para>
  ///   Remember to call the
  ///   saveToFlash() method of the module if the modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the callback URL to notify significant state changes
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
  function TYNetwork.set_callbackUrl(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('callbackUrl',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the HTTP method used to notify callbacks for significant state changes.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_CALLBACKMETHOD_POST, Y_CALLBACKMETHOD_GET and Y_CALLBACKMETHOD_PUT corresponding to
  ///   the HTTP method used to notify callbacks for significant state changes
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_CALLBACKMETHOD_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_callbackMethod():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CALLBACKMETHOD_INVALID;
              exit
            end;
        end;
      result := self._callbackMethod;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the HTTP method used to notify callbacks for significant state changes.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_CALLBACKMETHOD_POST, Y_CALLBACKMETHOD_GET and Y_CALLBACKMETHOD_PUT corresponding to
  ///   the HTTP method used to notify callbacks for significant state changes
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
  function TYNetwork.set_callbackMethod(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('callbackMethod',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the encoding standard to use for representing notification values.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_CALLBACKENCODING_FORM, Y_CALLBACKENCODING_JSON, Y_CALLBACKENCODING_JSON_ARRAY,
  ///   Y_CALLBACKENCODING_CSV, Y_CALLBACKENCODING_YOCTO_API, Y_CALLBACKENCODING_JSON_NUM,
  ///   Y_CALLBACKENCODING_EMONCMS and Y_CALLBACKENCODING_AZURE corresponding to the encoding standard to
  ///   use for representing notification values
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_CALLBACKENCODING_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_callbackEncoding():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CALLBACKENCODING_INVALID;
              exit
            end;
        end;
      result := self._callbackEncoding;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the encoding standard to use for representing notification values.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_CALLBACKENCODING_FORM, Y_CALLBACKENCODING_JSON, Y_CALLBACKENCODING_JSON_ARRAY,
  ///   Y_CALLBACKENCODING_CSV, Y_CALLBACKENCODING_YOCTO_API, Y_CALLBACKENCODING_JSON_NUM,
  ///   Y_CALLBACKENCODING_EMONCMS and Y_CALLBACKENCODING_AZURE corresponding to the encoding standard to
  ///   use for representing notification values
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
  function TYNetwork.set_callbackEncoding(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('callbackEncoding',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns a hashed version of the notification callback credentials if set,
  ///   or an empty string otherwise.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to a hashed version of the notification callback credentials if set,
  ///   or an empty string otherwise
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_CALLBACKCREDENTIALS_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_callbackCredentials():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CALLBACKCREDENTIALS_INVALID;
              exit
            end;
        end;
      result := self._callbackCredentials;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the credentials required to connect to the callback address.
  /// <para>
  ///   The credentials
  ///   must be provided as returned by function get_callbackCredentials,
  ///   in the form username:hash. The method used to compute the hash varies according
  ///   to the the authentication scheme implemented by the callback, For Basic authentication,
  ///   the hash is the MD5 of the string username:password. For Digest authentication,
  ///   the hash is the MD5 of the string username:realm:password. For a simpler
  ///   way to configure callback credentials, use function callbackLogin instead.
  ///   Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a string corresponding to the credentials required to connect to the callback address
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
  function TYNetwork.set_callbackCredentials(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('callbackCredentials',rest_val);
    end;

  ////
  /// <summary>
  ///   Connects to the notification callback and saves the credentials required to
  ///   log into it.
  /// <para>
  ///   The password is not stored into the module, only a hashed
  ///   copy of the credentials are saved. Remember to call the
  ///   saveToFlash() method of the module if the modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="username">
  ///   username required to log to the callback
  /// </param>
  /// <param name="password">
  ///   password required to log to the callback
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
  function TYNetwork.callbackLogin(username: string; password: string):integer;
    var
      rest_val: string;
    begin
      rest_val := username+':'+password;
      result := _setAttr('callbackCredentials', rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the minimum waiting time between two callback notifications, in seconds.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the minimum waiting time between two callback notifications, in seconds
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_CALLBACKMINDELAY_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_callbackMinDelay():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CALLBACKMINDELAY_INVALID;
              exit
            end;
        end;
      result := self._callbackMinDelay;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the minimum waiting time between two callback notifications, in seconds.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the minimum waiting time between two callback notifications, in seconds
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
  function TYNetwork.set_callbackMinDelay(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('callbackMinDelay',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the maximum waiting time between two callback notifications, in seconds.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the maximum waiting time between two callback notifications, in seconds
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_CALLBACKMAXDELAY_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_callbackMaxDelay():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CALLBACKMAXDELAY_INVALID;
              exit
            end;
        end;
      result := self._callbackMaxDelay;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the maximum waiting time between two callback notifications, in seconds.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   an integer corresponding to the maximum waiting time between two callback notifications, in seconds
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
  function TYNetwork.set_callbackMaxDelay(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('callbackMaxDelay',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the current consumed by the module from Power-over-Ethernet (PoE), in milli-amps.
  /// <para>
  ///   The current consumption is measured after converting PoE source to 5 Volt, and should
  ///   never exceed 1800 mA.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the current consumed by the module from Power-over-Ethernet (PoE), in milli-amps
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_POECURRENT_INVALID.
  /// </para>
  ///-
  function TYNetwork.get_poeCurrent():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_POECURRENT_INVALID;
              exit
            end;
        end;
      result := self._poeCurrent;
      exit;
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
  ///   Use the method <c>YNetwork.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YNetwork</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYNetwork.FindNetwork(func: string):TYNetwork;
    var
      obj : TYNetwork;
    begin
      obj := TYNetwork(TYFunction._FindFromCache('Network', func));
      if obj = nil then
        begin
          obj :=  TYNetwork.create(func);
          TYFunction._AddToCache('Network',  func, obj)
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
  function TYNetwork.registerValueCallback(callback: TYNetworkValueCallback):LongInt;
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
      self._valueCallbackNetwork := callback;
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


  function TYNetwork._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackNetwork) <> nil) then
        begin
          self._valueCallbackNetwork(self, value)
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
  ///   Changes the configuration of the network interface to enable the use of an
  ///   IP address received from a DHCP server.
  /// <para>
  ///   Until an address is received from a DHCP
  ///   server, the module uses the IP parameters specified to this function.
  ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
  /// </para>
  /// </summary>
  /// <param name="fallbackIpAddr">
  ///   fallback IP address, to be used when no DHCP reply is received
  /// </param>
  /// <param name="fallbackSubnetMaskLen">
  ///   fallback subnet mask length when no DHCP reply is received, as an
  ///   integer (eg. 24 means 255.255.255.0)
  /// </param>
  /// <param name="fallbackRouter">
  ///   fallback router IP address, to be used when no DHCP reply is received
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYNetwork.useDHCP(fallbackIpAddr: string; fallbackSubnetMaskLen: LongInt; fallbackRouter: string):LongInt;
    begin
      result := self.set_ipConfig('DHCP:'+ fallbackIpAddr+'/'+inttostr( fallbackSubnetMaskLen)+'/'+fallbackRouter);
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the configuration of the network interface to use a static IP address.
  /// <para>
  ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
  /// </para>
  /// </summary>
  /// <param name="ipAddress">
  ///   device IP address
  /// </param>
  /// <param name="subnetMaskLen">
  ///   subnet mask length, as an integer (eg. 24 means 255.255.255.0)
  /// </param>
  /// <param name="router">
  ///   router IP address (default gateway)
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYNetwork.useStaticIP(ipAddress: string; subnetMaskLen: LongInt; router: string):LongInt;
    begin
      result := self.set_ipConfig('STATIC:'+ ipAddress+'/'+inttostr( subnetMaskLen)+'/'+router);
      exit;
    end;


  ////
  /// <summary>
  ///   Pings str_host to test the network connectivity.
  /// <para>
  ///   Sends four ICMP ECHO_REQUEST requests from the
  ///   module to the target str_host. This method returns a string with the result of the
  ///   4 ICMP ECHO_REQUEST requests.
  /// </para>
  /// </summary>
  /// <param name="host">
  ///   the hostname or the IP address of the target
  /// </param>
  /// <para>
  /// </para>
  /// <returns>
  ///   a string with the result of the ping.
  /// </returns>
  ///-
  function TYNetwork.ping(host: string):string;
    var
      content : TByteArray;
    begin
      content := self._download('ping.txt?host='+host);
      result := _ByteToString(content);
      exit;
    end;


  function TYNetwork.nextNetwork(): TYNetwork;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextNetwork := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextNetwork := nil;
          exit;
        end;
      nextNetwork := TYNetwork.FindNetwork(hwid);
    end;

  class function TYNetwork.FirstNetwork(): TYNetwork;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Network', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYNetwork.FindNetwork(serial+'.'+funcId);
    end;

//--- (end of YNetwork implementation)

//--- (Network functions)

  function yFindNetwork(func:string): TYNetwork;
    begin
      result := TYNetwork.FindNetwork(func);
    end;

  function yFirstNetwork(): TYNetwork;
    begin
      result := TYNetwork.FirstNetwork();
    end;

  procedure _NetworkCleanup();
    begin
    end;

//--- (end of Network functions)

initialization
  //--- (Network initialization)
  //--- (end of Network initialization)

finalization
  //--- (Network cleanup)
  _NetworkCleanup();
  //--- (end of Network cleanup)
end.
