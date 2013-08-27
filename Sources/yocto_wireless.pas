{*********************************************************************
 *
 * $Id: yocto_wireless.pas 12337 2013-08-14 15:22:22Z mvuilleu $
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

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_LINKQUALITY_INVALID           = -1;
   Y_SSID_INVALID                  = YAPI_INVALID_STRING;
   Y_CHANNEL_INVALID               = YAPI_INVALID_LONGWORD;
   Y_SECURITY_UNKNOWN = 0;
   Y_SECURITY_OPEN = 1;
   Y_SECURITY_WEP = 2;
   Y_SECURITY_WPA = 3;
   Y_SECURITY_WPA2 = 4;
   Y_SECURITY_INVALID = -1;

   Y_MESSAGE_INVALID               = YAPI_INVALID_STRING;
   Y_WLANCONFIG_INVALID            = YAPI_INVALID_STRING;


//--- (end of generated code: YWireless definitions)

type


//--- (generated code: YWlanRecord declaration)
 TYWlanRecord = class;
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
protected
   // Attributes (function value cache)
   // Function-specific method for reading JSON output and caching result

   //--- (end of generated code: YWlanRecord declaration)
   _ssid,_sec:string;
   _channel,_rssi :integer;
public
   constructor create(data:string);
   

   //--- (generated code: YWlanRecord accessors declaration)
   function get_ssid():string;

   function get_channel():integer;

   function get_security():string;

   function get_linkQuality():integer;

   //--- (end of generated code: YWlanRecord accessors declaration)
end;



TYWLANRECORDARRAY = array of TYWlanRecord;


//--- (YWireless declaration)
 TYWireless = class;
 TUpdateCallback  = procedure(func: TYWireless; value:string);
////
/// <summary>
///   TYWireless Class: Wireless function interface
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
///-
TYWireless=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _linkQuality              : LongInt;
   _ssid                     : string;
   _channel                  : LongWord;
   _security                 : Integer;
   _message                  : string;
   _wlanConfig               : string;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of generated code: YWireless declaration)

public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of wireless lan interfaces started using <c>yFirstWireless()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YWireless</c> object, corresponding to
   ///   a wireless lan interface currently online, or a <c>null</c> pointer
   ///   if there are no more wireless lan interfaces to enumerate.
   /// </returns>
   ///-
   function nextWireless():TYWireless;

   //--- (generated code: YWireless accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the wireless lan interface.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the wireless lan interface
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the wireless lan interface.
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
   ///   a string corresponding to the logical name of the wireless lan interface
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
   ///   Returns the current value of the wireless lan interface (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the wireless lan interface (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the link quality, expressed in per cents.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the link quality, expressed in per cents
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
   ///   Returns the 802.
   /// <para>
   ///   11 channel currently used, or 0 when the selected network has not been found.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the 802
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_CHANNEL_INVALID</c>.
   /// </para>
   ///-
   function get_channel():LongWord;

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
   ///   Returns the last status message from the wireless interface.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the last status message from the wireless interface
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
   ///   Changes the configuration of the wireless lan interface to connect to an existing
   ///   access point (infrastructure mode).
   /// <para>
   ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="ssid">
   ///   the name of the network to connect to
   /// </param>
   /// <param name="securityKey">
   ///   the network key, as a character string
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
   function joinNetwork(ssid:string;securityKey:string):integer;

   ////
   /// <summary>
   ///   Changes the configuration of the wireless lan interface to create an ad-hoc
   ///   wireless network, without using an access point.
   /// <para>
   ///   If a security key is specified,
   ///   the network is protected by WEP128, since WPA is not standardized for
   ///   ad-hoc networks.
   ///   Remember to call the <c>saveToFlash()</c> method and then to reboot the module to apply this setting.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="ssid">
   ///   the name of the network to connect to
   /// </param>
   /// <param name="securityKey">
   ///   the network key, as a character string
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
   function adhocNetwork(ssid:string;securityKey:string):integer;

   ////
   /// <summary>
   ///   Returns a list of YWlanRecord objects which describe detected Wireless networks.
   /// <para>
   ///   This list is not updated when the module is already connected to an acces point (infrastructure mode).
   ///   To force an update of this list, <c>adhocNetwork()</c> must be called to disconnect
   ///   the module from the current network. The returned list must be unallocated by caller,
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
   function get_detectedWlans():TYWLANRECORDARRAY;

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
///   the first wireless lan interface currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstWireless():TYWireless;

//--- (end of generated code: Wireless functions declaration)

implementation

//--- (generated code: YWireless implementation)

var
   _WirelessCache : TStringList;

constructor TYWireless.Create(func:string);
 begin
   inherited Create('Wireless', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _linkQuality := Y_LINKQUALITY_INVALID;
   _ssid := Y_SSID_INVALID;
   _channel := Y_CHANNEL_INVALID;
   _security := Y_SECURITY_INVALID;
   _message := Y_MESSAGE_INVALID;
   _wlanConfig := Y_WLANCONFIG_INVALID;
 end;

{$HINTS OFF}
function TYWireless._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'linkQuality') then
       begin
         _linkQuality := member^.ivalue;
       end else
      if (member^.name = 'ssid') then
       begin
         _ssid := string(member^.svalue);
       end else
      if (member^.name = 'channel') then
       begin
         _channel := member^.ivalue;
       end else
      if (member^.name = 'security') then
       begin
         _security := member^.ivalue;
       end else
      if (member^.name = 'message') then
       begin
         _message := string(member^.svalue);
       end else
      if (member^.name = 'wlanConfig') then
       begin
         _wlanConfig := string(member^.svalue);
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the wireless lan interface.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the wireless lan interface
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYWireless.get_logicalName():string;
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
///   Changes the logical name of the wireless lan interface.
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
///   a string corresponding to the logical name of the wireless lan interface
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
function TYWireless.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the wireless lan interface (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the wireless lan interface (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYWireless.get_advertisedValue():string;
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
///   Returns the link quality, expressed in per cents.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the link quality, expressed in per cents
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LINKQUALITY_INVALID.
/// </para>
///-
function TYWireless.get_linkQuality():LongInt;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LINKQUALITY_INVALID;
         exit;
       end;
   result := _linkQuality;
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
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_SSID_INVALID;
         exit;
       end;
   result := _ssid;
 end;

////
/// <summary>
///   Returns the 802.
/// <para>
///   11 channel currently used, or 0 when the selected network has not been found.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the 802
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_CHANNEL_INVALID.
/// </para>
///-
function TYWireless.get_channel():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CHANNEL_INVALID;
         exit;
       end;
   result := _channel;
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
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_SECURITY_INVALID;
         exit;
       end;
   result := _security;
 end;

////
/// <summary>
///   Returns the last status message from the wireless interface.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the last status message from the wireless interface
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_MESSAGE_INVALID.
/// </para>
///-
function TYWireless.get_message():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_MESSAGE_INVALID;
         exit;
       end;
   result := _message;
 end;

function TYWireless.get_wlanConfig():string;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_WLANCONFIG_INVALID;
         exit;
       end;
   result := _wlanConfig;
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
///   Changes the configuration of the wireless lan interface to connect to an existing
///   access point (infrastructure mode).
/// <para>
///   Remember to call the saveToFlash() method and then to reboot the module to apply this setting.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="ssid">
///   the name of the network to connect to
/// </param>
/// <param name="securityKey">
///   the network key, as a character string
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
function TYWireless.joinNetwork(ssid:string;securityKey:string):integer;
 var
   rest_val: string;
 begin
   rest_val := 'INFRA:'+ssid+'\\'+securityKey;
   result := _setAttr('wlanConfig', rest_val);
 end;

////
/// <summary>
///   Changes the configuration of the wireless lan interface to create an ad-hoc
///   wireless network, without using an access point.
/// <para>
///   If a security key is specified,
///   the network is protected by WEP128, since WPA is not standardized for
///   ad-hoc networks.
///   Remember to call the saveToFlash() method and then to reboot the module to apply this setting.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="ssid">
///   the name of the network to connect to
/// </param>
/// <param name="securityKey">
///   the network key, as a character string
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
function TYWireless.adhocNetwork(ssid:string;securityKey:string):integer;
 var
   rest_val: string;
 begin
   rest_val := 'ADHOC:'+ssid+'\\'+securityKey;
   result := _setAttr('wlanConfig', rest_val);
 end;

////
/// <summary>
///   Returns a list of YWlanRecord objects which describe detected Wireless networks.
/// <para>
///   This list is not updated when the module is already connected to an acces point (infrastructure mode).
///   To force an update of this list, <c>adhocNetwork()</c> must be called to disconnect
///   the module from the current network. The returned list must be unallocated by caller,
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
function TYWireless.get_detectedWlans():TYWLANRECORDARRAY;
     var
        json : TBYTEARRAY;
         list : TSTRINGARRAY;
         res : TYWLANRECORDARRAY;
        i_i : integer;
     begin
        json := self._download('wlan.json?by=name');
        list := self._json_get_array(json);
        for i_i:=0 to length(list)-1 do begin SetLength(res, length(res)+1); res[length(res)-1]:= TYWlanRecord.create(list[i_i]);end;
        result:= res;
            
     end;


function TYWireless.nextWireless(): TYWireless;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextWireless := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextWireless := nil;
      exit;
    end;
    nextWireless := yFindWireless(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYWireless.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYWireless.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYWireless.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYWireless.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of generated code: YWireless implementation)

//--- (generated code: Wireless functions)

function yFindWireless(func:string): TYWireless;
 var
   index: integer;
   res  : TYWireless;
 begin
    if (_WirelessCache.Find(func, index)) then
     begin
       yFindWireless := TYWireless(_WirelessCache.objects[index]);
       exit;
     end;
   res := TYWireless.Create(func);
   _WirelessCache.addObject(func, res);
   yFindWireless := res;
 end;

function yFirstWireless(): TYWireless;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('Wireless', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstWireless := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstWireless := nil;
       exit;
    end;
   yFirstWireless := yFindWireless(serial+'.'+funcId);
 end;

procedure _WirelessCleanup();
  var i:integer;
begin
  for i:=0 to _WirelessCache.count-1 do 
    begin
     _WirelessCache.objects[i].free();
     _WirelessCache.objects[i]:=nil;
    end;
   _WirelessCache.free();
   _WirelessCache:=nil;
end;

//--- (end of generated code: Wireless functions)



//--- (generated code: YWlanRecord implementation)


function TYWlanRecord.get_ssid():string;
     begin
        result:= self._ssid; 
     end;


function TYWlanRecord.get_channel():integer;
     begin
        result:= self._channel; 
     end;


function TYWlanRecord.get_security():string;
     begin
        result:= self._sec; 
     end;


function TYWlanRecord.get_linkQuality():integer;
     begin
        result:= self._rssi; 
     end;



    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
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
//--- (end of generated code: YWlanRecord functions)

procedure freeWlanRecordArray(var list:TYWLANRECORDARRAY);
 var i:integer;
 begin
  for i:=0 to length(list)-1 do
   list[i].free();
  setLength(list,0);
 end;


initialization
   //--- (generated code: Wireless initialization)
   _WirelessCache        := TstringList.create();
   _WirelessCache.sorted := true;
   //--- (end of generated code: Wireless initialization)

finalization
   //--- (generated code: Wireless cleanup)
   _WirelessCleanup();
   //--- (end of generated code: Wireless cleanup)
end.
