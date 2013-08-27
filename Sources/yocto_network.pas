{*********************************************************************
 *
 * $Id: yocto_network.pas 12337 2013-08-14 15:22:22Z mvuilleu $
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

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_READINESS_DOWN = 0;
   Y_READINESS_EXISTS = 1;
   Y_READINESS_LINKED = 2;
   Y_READINESS_LAN_OK = 3;
   Y_READINESS_WWW_OK = 4;
   Y_READINESS_INVALID = -1;

   Y_MACADDRESS_INVALID            = YAPI_INVALID_STRING;
   Y_IPADDRESS_INVALID             = YAPI_INVALID_STRING;
   Y_SUBNETMASK_INVALID            = YAPI_INVALID_STRING;
   Y_ROUTER_INVALID                = YAPI_INVALID_STRING;
   Y_IPCONFIG_INVALID              = YAPI_INVALID_STRING;
   Y_PRIMARYDNS_INVALID            = YAPI_INVALID_STRING;
   Y_SECONDARYDNS_INVALID          = YAPI_INVALID_STRING;
   Y_USERPASSWORD_INVALID          = YAPI_INVALID_STRING;
   Y_ADMINPASSWORD_INVALID         = YAPI_INVALID_STRING;
   Y_DISCOVERABLE_FALSE = 0;
   Y_DISCOVERABLE_TRUE = 1;
   Y_DISCOVERABLE_INVALID = -1;

   Y_WWWWATCHDOGDELAY_INVALID      = YAPI_INVALID_LONGWORD;
   Y_CALLBACKURL_INVALID           = YAPI_INVALID_STRING;
   Y_CALLBACKMETHOD_POST = 0;
   Y_CALLBACKMETHOD_GET = 1;
   Y_CALLBACKMETHOD_PUT = 2;
   Y_CALLBACKMETHOD_INVALID = -1;

   Y_CALLBACKENCODING_FORM = 0;
   Y_CALLBACKENCODING_JSON = 1;
   Y_CALLBACKENCODING_JSON_ARRAY = 2;
   Y_CALLBACKENCODING_CSV = 3;
   Y_CALLBACKENCODING_YOCTO_API = 4;
   Y_CALLBACKENCODING_INVALID = -1;

   Y_CALLBACKCREDENTIALS_INVALID   = YAPI_INVALID_STRING;
   Y_CALLBACKMINDELAY_INVALID      = YAPI_INVALID_LONGWORD;
   Y_CALLBACKMAXDELAY_INVALID      = YAPI_INVALID_LONGWORD;
   Y_POECURRENT_INVALID            = YAPI_INVALID_LONGWORD;


//--- (end of YNetwork definitions)

type
//--- (YNetwork declaration)
 TYNetwork = class;
 TUpdateCallback  = procedure(func: TYNetwork; value:string);
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
protected
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
   _userPassword             : string;
   _adminPassword            : string;
   _discoverable             : Integer;
   _wwwWatchdogDelay         : LongWord;
   _callbackUrl              : string;
   _callbackMethod           : Integer;
   _callbackEncoding         : Integer;
   _callbackCredentials      : string;
   _callbackMinDelay         : LongWord;
   _callbackMaxDelay         : LongWord;
   _poeCurrent               : LongWord;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of YNetwork declaration)

public
   constructor Create(func:string);

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

   //--- (YNetwork accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the network interface, corresponding to the network name of the module.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the network interface, corresponding to the network
   ///   name of the module
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the network interface, corresponding to the network name of the module.
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
   ///   a string corresponding to the logical name of the network interface, corresponding to the network
   ///   name of the module
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
   ///   Returns the current value of the network interface (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the network interface (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the current established working mode of the network interface.
   /// <para>
   ///   Level zero (DOWN_0) means that no hardware link has been detected. Either there is no signal
   ///   on the network cable, or the selected wireless access point cannot be detected.
   ///   Level 1 (LIVE_1) is reached when the network is detected, but is not yet connected,
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
   ///   The adress may have been configured
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
   ///   Changes the configuration of the network interface to enable the use of an
   ///   IP address received from a DHCP server.
   /// <para>
   ///   Until an address is received from a DHCP
   ///   server, the module uses the IP parameters specified to this function.
   ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
   /// </para>
   /// <para>
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
   /// <para>
   /// </para>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function useDHCP(fallbackIpAddr:string;fallbackSubnetMaskLen:integer;fallbackRouter:string):integer;

   ////
   /// <summary>
   ///   Changes the configuration of the network interface to use a static IP address.
   /// <para>
   ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
   /// </para>
   /// <para>
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
   /// <para>
   /// </para>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function useStaticIP(ipAddress:string;subnetMaskLen:integer;router:string):integer;

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
   ///   Changes the IP address of the secondarz name server to be used by the module.
   /// <para>
   ///   When using DHCP, if a value is specified, it overrides the value received from the DHCP server.
   ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="newval">
   ///   a string corresponding to the IP address of the secondarz name server to be used by the module
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
   function get_wwwWatchdogDelay():LongWord;

   ////
   /// <summary>
   ///   Changes the allowed downtime of the WWW link (in seconds) before triggering an automated
   ///   reboot to try to recover Internet connectivity.
   /// <para>
   ///   A zero value disable automated reboot
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
   function set_wwwWatchdogDelay(newval:LongWord):integer;

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
   ///   <c>Y_CALLBACKENCODING_JSON_ARRAY</c>, <c>Y_CALLBACKENCODING_CSV</c> and
   ///   <c>Y_CALLBACKENCODING_YOCTO_API</c> corresponding to the encoding standard to use for representing
   ///   notification values
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
   ///   <c>Y_CALLBACKENCODING_JSON_ARRAY</c>, <c>Y_CALLBACKENCODING_CSV</c> and
   ///   <c>Y_CALLBACKENCODING_YOCTO_API</c> corresponding to the encoding standard to use for representing
   ///   notification values
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
   function callbackLogin(username:string;password:string):integer;

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
   function get_callbackMinDelay():LongWord;

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
   function set_callbackMinDelay(newval:LongWord):integer;

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
   function get_callbackMaxDelay():LongWord;

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
   function set_callbackMaxDelay(newval:LongWord):integer;

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
   function get_poeCurrent():LongWord;

   ////
   /// <summary>
   ///   Pings str_host to test the network connectivity.
   /// <para>
   ///   Sends four requests ICMP ECHO_REQUEST from the
   ///   module to the target str_host. This method returns a string with the result of the
   ///   4 ICMP ECHO_REQUEST result.
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
   function ping(host:string):string;

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

//--- (YNetwork implementation)

var
   _NetworkCache : TStringList;

constructor TYNetwork.Create(func:string);
 begin
   inherited Create('Network', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _readiness := Y_READINESS_INVALID;
   _macAddress := Y_MACADDRESS_INVALID;
   _ipAddress := Y_IPADDRESS_INVALID;
   _subnetMask := Y_SUBNETMASK_INVALID;
   _router := Y_ROUTER_INVALID;
   _ipConfig := Y_IPCONFIG_INVALID;
   _primaryDNS := Y_PRIMARYDNS_INVALID;
   _secondaryDNS := Y_SECONDARYDNS_INVALID;
   _userPassword := Y_USERPASSWORD_INVALID;
   _adminPassword := Y_ADMINPASSWORD_INVALID;
   _discoverable := Y_DISCOVERABLE_INVALID;
   _wwwWatchdogDelay := Y_WWWWATCHDOGDELAY_INVALID;
   _callbackUrl := Y_CALLBACKURL_INVALID;
   _callbackMethod := Y_CALLBACKMETHOD_INVALID;
   _callbackEncoding := Y_CALLBACKENCODING_INVALID;
   _callbackCredentials := Y_CALLBACKCREDENTIALS_INVALID;
   _callbackMinDelay := Y_CALLBACKMINDELAY_INVALID;
   _callbackMaxDelay := Y_CALLBACKMAXDELAY_INVALID;
   _poeCurrent := Y_POECURRENT_INVALID;
 end;

{$HINTS OFF}
function TYNetwork._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'readiness') then
       begin
         _readiness := member^.ivalue;
       end else
      if (member^.name = 'macAddress') then
       begin
         _macAddress := string(member^.svalue);
       end else
      if (member^.name = 'ipAddress') then
       begin
         _ipAddress := string(member^.svalue);
       end else
      if (member^.name = 'subnetMask') then
       begin
         _subnetMask := string(member^.svalue);
       end else
      if (member^.name = 'router') then
       begin
         _router := string(member^.svalue);
       end else
      if (member^.name = 'ipConfig') then
       begin
         _ipConfig := string(member^.svalue);
       end else
      if (member^.name = 'primaryDNS') then
       begin
         _primaryDNS := string(member^.svalue);
       end else
      if (member^.name = 'secondaryDNS') then
       begin
         _secondaryDNS := string(member^.svalue);
       end else
      if (member^.name = 'userPassword') then
       begin
         _userPassword := string(member^.svalue);
       end else
      if (member^.name = 'adminPassword') then
       begin
         _adminPassword := string(member^.svalue);
       end else
      if (member^.name = 'discoverable') then
       begin
         _discoverable := member^.ivalue;
       end else
      if (member^.name = 'wwwWatchdogDelay') then
       begin
         _wwwWatchdogDelay := member^.ivalue;
       end else
      if (member^.name = 'callbackUrl') then
       begin
         _callbackUrl := string(member^.svalue);
       end else
      if (member^.name = 'callbackMethod') then
       begin
         _callbackMethod := member^.ivalue;
       end else
      if (member^.name = 'callbackEncoding') then
       begin
         _callbackEncoding := member^.ivalue;
       end else
      if (member^.name = 'callbackCredentials') then
       begin
         _callbackCredentials := string(member^.svalue);
       end else
      if (member^.name = 'callbackMinDelay') then
       begin
         _callbackMinDelay := member^.ivalue;
       end else
      if (member^.name = 'callbackMaxDelay') then
       begin
         _callbackMaxDelay := member^.ivalue;
       end else
      if (member^.name = 'poeCurrent') then
       begin
         _poeCurrent := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the network interface, corresponding to the network name of the module.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the network interface, corresponding to the network
///   name of the module
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYNetwork.get_logicalName():string;
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
///   Changes the logical name of the network interface, corresponding to the network name of the module.
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
///   a string corresponding to the logical name of the network interface, corresponding to the network
///   name of the module
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
function TYNetwork.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the network interface (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the network interface (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYNetwork.get_advertisedValue():string;
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
///   Returns the current established working mode of the network interface.
/// <para>
///   Level zero (DOWN_0) means that no hardware link has been detected. Either there is no signal
///   on the network cable, or the selected wireless access point cannot be detected.
///   Level 1 (LIVE_1) is reached when the network is detected, but is not yet connected,
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_READINESS_INVALID;
         exit;
       end;
   result := _readiness;
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
   if (_macAddress = Y_MACADDRESS_INVALID) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_MACADDRESS_INVALID;
         exit;
       end;
   result := _macAddress;
 end;

////
/// <summary>
///   Returns the IP address currently in use by the device.
/// <para>
///   The adress may have been configured
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_IPADDRESS_INVALID;
         exit;
       end;
   result := _ipAddress;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_SUBNETMASK_INVALID;
         exit;
       end;
   result := _subnetMask;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_ROUTER_INVALID;
         exit;
       end;
   result := _router;
 end;

function TYNetwork.get_ipConfig():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_IPCONFIG_INVALID;
         exit;
       end;
   result := _ipConfig;
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
///   Changes the configuration of the network interface to enable the use of an
///   IP address received from a DHCP server.
/// <para>
///   Until an address is received from a DHCP
///   server, the module uses the IP parameters specified to this function.
///   Remember to call the saveToFlash() method and then to reboot the module to apply this setting.
/// </para>
/// <para>
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
/// <para>
/// </para>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYNetwork.useDHCP(fallbackIpAddr:string;fallbackSubnetMaskLen:integer;fallbackRouter:string):integer;
 var
   rest_val: string;
 begin
   rest_val := 'DHCP:'+fallbackIpAddr+'/'+str(fallbackSubnetMaskLen)+'/'+fallbackRouter;
   result := _setAttr('ipConfig', rest_val);
 end;

////
/// <summary>
///   Changes the configuration of the network interface to use a static IP address.
/// <para>
///   Remember to call the saveToFlash() method and then to reboot the module to apply this setting.
/// </para>
/// <para>
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
/// <para>
/// </para>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYNetwork.useStaticIP(ipAddress:string;subnetMaskLen:integer;router:string):integer;
 var
   rest_val: string;
 begin
   rest_val := 'STATIC:'+ipAddress+'/'+str(subnetMaskLen)+'/'+router;
   result := _setAttr('ipConfig', rest_val);
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PRIMARYDNS_INVALID;
         exit;
       end;
   result := _primaryDNS;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_SECONDARYDNS_INVALID;
         exit;
       end;
   result := _secondaryDNS;
 end;

////
/// <summary>
///   Changes the IP address of the secondarz name server to be used by the module.
/// <para>
///   When using DHCP, if a value is specified, it overrides the value received from the DHCP server.
///   Remember to call the saveToFlash() method and then to reboot the module to apply this setting.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="newval">
///   a string corresponding to the IP address of the secondarz name server to be used by the module
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_USERPASSWORD_INVALID;
         exit;
       end;
   result := _userPassword;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_ADMINPASSWORD_INVALID;
         exit;
       end;
   result := _adminPassword;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_DISCOVERABLE_INVALID;
         exit;
       end;
   result := _discoverable;
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
function TYNetwork.get_wwwWatchdogDelay():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_WWWWATCHDOGDELAY_INVALID;
         exit;
       end;
   result := _wwwWatchdogDelay;
 end;

////
/// <summary>
///   Changes the allowed downtime of the WWW link (in seconds) before triggering an automated
///   reboot to try to recover Internet connectivity.
/// <para>
///   A zero value disable automated reboot
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
function TYNetwork.set_wwwWatchdogDelay(newval:LongWord):integer;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CALLBACKURL_INVALID;
         exit;
       end;
   result := _callbackUrl;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CALLBACKMETHOD_INVALID;
         exit;
       end;
   result := _callbackMethod;
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
///   Y_CALLBACKENCODING_CSV and Y_CALLBACKENCODING_YOCTO_API corresponding to the encoding standard to
///   use for representing notification values
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_CALLBACKENCODING_INVALID.
/// </para>
///-
function TYNetwork.get_callbackEncoding():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CALLBACKENCODING_INVALID;
         exit;
       end;
   result := _callbackEncoding;
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
///   Y_CALLBACKENCODING_CSV and Y_CALLBACKENCODING_YOCTO_API corresponding to the encoding standard to
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CALLBACKCREDENTIALS_INVALID;
         exit;
       end;
   result := _callbackCredentials;
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
function TYNetwork.callbackLogin(username:string;password:string):integer;
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
function TYNetwork.get_callbackMinDelay():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CALLBACKMINDELAY_INVALID;
         exit;
       end;
   result := _callbackMinDelay;
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
function TYNetwork.set_callbackMinDelay(newval:LongWord):integer;
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
function TYNetwork.get_callbackMaxDelay():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CALLBACKMAXDELAY_INVALID;
         exit;
       end;
   result := _callbackMaxDelay;
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
function TYNetwork.set_callbackMaxDelay(newval:LongWord):integer;
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
function TYNetwork.get_poeCurrent():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_POECURRENT_INVALID;
         exit;
       end;
   result := _poeCurrent;
 end;

////
/// <summary>
///   Pings str_host to test the network connectivity.
/// <para>
///   Sends four requests ICMP ECHO_REQUEST from the
///   module to the target str_host. This method returns a string with the result of the
///   4 ICMP ECHO_REQUEST result.
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
function TYNetwork.ping(host:string):string;
     var
        content : TBYTEARRAY;
     begin
        content := self._download('ping.txt?host='+host);
        result:= string(content);
            
     end;


function TYNetwork.nextNetwork(): TYNetwork;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextNetwork := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextNetwork := nil;
      exit;
    end;
    nextNetwork := yFindNetwork(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYNetwork.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYNetwork.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYNetwork.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYNetwork.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of YNetwork implementation)

//--- (Network functions)

function yFindNetwork(func:string): TYNetwork;
 var
   index: integer;
   res  : TYNetwork;
 begin
    if (_NetworkCache.Find(func, index)) then
     begin
       yFindNetwork := TYNetwork(_NetworkCache.objects[index]);
       exit;
     end;
   res := TYNetwork.Create(func);
   _NetworkCache.addObject(func, res);
   yFindNetwork := res;
 end;

function yFirstNetwork(): TYNetwork;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('Network', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstNetwork := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstNetwork := nil;
       exit;
    end;
   yFirstNetwork := yFindNetwork(serial+'.'+funcId);
 end;

procedure _NetworkCleanup();
  var i:integer;
begin
  for i:=0 to _NetworkCache.count-1 do 
    begin
     _NetworkCache.objects[i].free();
     _NetworkCache.objects[i]:=nil;
    end;
   _NetworkCache.free();
   _NetworkCache:=nil;
end;

//--- (end of Network functions)

initialization
   //--- (Network initialization)
   _NetworkCache        := TstringList.create();
   _NetworkCache.sorted := true;
   //--- (end of Network initialization)

finalization
   //--- (Network cleanup)
   _NetworkCleanup();
   //--- (end of Network cleanup)
end.
