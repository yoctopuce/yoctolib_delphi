{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindBluetoothLink(), the high-level API for BluetoothLink functions
 *
 *  - - - - - - - - - License information: - - - - - - - - -
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


unit yocto_bluetoothlink;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (YBluetoothLink definitions)

const Y_OWNADDRESS_INVALID            = YAPI_INVALID_STRING;
const Y_PAIRINGPIN_INVALID            = YAPI_INVALID_STRING;
const Y_REMOTEADDRESS_INVALID         = YAPI_INVALID_STRING;
const Y_REMOTENAME_INVALID            = YAPI_INVALID_STRING;
const Y_MUTE_FALSE = 0;
const Y_MUTE_TRUE = 1;
const Y_MUTE_INVALID = -1;
const Y_PREAMPLIFIER_INVALID          = YAPI_INVALID_UINT;
const Y_VOLUME_INVALID                = YAPI_INVALID_UINT;
const Y_LINKSTATE_DOWN = 0;
const Y_LINKSTATE_FREE = 1;
const Y_LINKSTATE_SEARCH = 2;
const Y_LINKSTATE_EXISTS = 3;
const Y_LINKSTATE_LINKED = 4;
const Y_LINKSTATE_PLAY = 5;
const Y_LINKSTATE_INVALID = -1;
const Y_LINKQUALITY_INVALID           = YAPI_INVALID_UINT;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;

//--- (end of YBluetoothLink definitions)

//--- (YBluetoothLink yapiwrapper declaration)
//--- (end of YBluetoothLink yapiwrapper declaration)

type

  TYBluetoothLink = class;
  //--- (YBluetoothLink class start)
  TYBluetoothLinkValueCallback = procedure(func: TYBluetoothLink; value:string);
  TYBluetoothLinkTimedReportCallback = procedure(func: TYBluetoothLink; value:TYMeasure);

  ////
  /// <summary>
  ///   TYBluetoothLink Class: Bluetooth sound controller control interface
  /// <para>
  ///   BluetoothLink function provides control over Bluetooth link
  ///   and status for devices that are Bluetooth-enabled.
  /// </para>
  /// </summary>
  ///-
  TYBluetoothLink=class(TYFunction)
  //--- (end of YBluetoothLink class start)
  protected
  //--- (YBluetoothLink declaration)
    // Attributes (function value cache)
    _ownAddress               : string;
    _pairingPin               : string;
    _remoteAddress            : string;
    _remoteName               : string;
    _mute                     : Integer;
    _preAmplifier             : LongInt;
    _volume                   : LongInt;
    _linkState                : Integer;
    _linkQuality              : LongInt;
    _command                  : string;
    _valueCallbackBluetoothLink : TYBluetoothLinkValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of YBluetoothLink declaration)

  public
    //--- (YBluetoothLink accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the MAC-48 address of the bluetooth interface, which is unique on the bluetooth network.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the MAC-48 address of the bluetooth interface, which is unique on the
    ///   bluetooth network
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YBluetoothLink.OWNADDRESS_INVALID</c>.
    /// </para>
    ///-
    function get_ownAddress():string;

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
    ///   On failure, throws an exception or returns <c>YBluetoothLink.PAIRINGPIN_INVALID</c>.
    /// </para>
    ///-
    function get_pairingPin():string;

    ////
    /// <summary>
    ///   Changes the PIN code used by the module for bluetooth pairing.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module to save the
    ///   new value in the device flash.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the PIN code used by the module for bluetooth pairing
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
    function set_pairingPin(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the MAC-48 address of the remote device to connect to.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the MAC-48 address of the remote device to connect to
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YBluetoothLink.REMOTEADDRESS_INVALID</c>.
    /// </para>
    ///-
    function get_remoteAddress():string;

    ////
    /// <summary>
    ///   Changes the MAC-48 address defining which remote device to connect to.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the MAC-48 address defining which remote device to connect to
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
    function set_remoteAddress(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the bluetooth name the remote device, if found on the bluetooth network.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the bluetooth name the remote device, if found on the bluetooth network
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YBluetoothLink.REMOTENAME_INVALID</c>.
    /// </para>
    ///-
    function get_remoteName():string;

    ////
    /// <summary>
    ///   Returns the state of the mute function.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YBluetoothLink.MUTE_FALSE</c> or <c>YBluetoothLink.MUTE_TRUE</c>, according to the state
    ///   of the mute function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YBluetoothLink.MUTE_INVALID</c>.
    /// </para>
    ///-
    function get_mute():Integer;

    ////
    /// <summary>
    ///   Changes the state of the mute function.
    /// <para>
    ///   Remember to call the matching module
    ///   <c>saveToFlash()</c> method to save the setting permanently.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YBluetoothLink.MUTE_FALSE</c> or <c>YBluetoothLink.MUTE_TRUE</c>, according to the state
    ///   of the mute function
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
    function set_mute(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the audio pre-amplifier volume, in per cents.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the audio pre-amplifier volume, in per cents
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YBluetoothLink.PREAMPLIFIER_INVALID</c>.
    /// </para>
    ///-
    function get_preAmplifier():LongInt;

    ////
    /// <summary>
    ///   Changes the audio pre-amplifier volume, in per cents.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c>
    ///   method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the audio pre-amplifier volume, in per cents
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
    function set_preAmplifier(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the connected headset volume, in per cents.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the connected headset volume, in per cents
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YBluetoothLink.VOLUME_INVALID</c>.
    /// </para>
    ///-
    function get_volume():LongInt;

    ////
    /// <summary>
    ///   Changes the connected headset volume, in per cents.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the connected headset volume, in per cents
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
    function set_volume(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the bluetooth link state.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YBluetoothLink.LINKSTATE_DOWN</c>, <c>YBluetoothLink.LINKSTATE_FREE</c>,
    ///   <c>YBluetoothLink.LINKSTATE_SEARCH</c>, <c>YBluetoothLink.LINKSTATE_EXISTS</c>,
    ///   <c>YBluetoothLink.LINKSTATE_LINKED</c> and <c>YBluetoothLink.LINKSTATE_PLAY</c> corresponding to
    ///   the bluetooth link state
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YBluetoothLink.LINKSTATE_INVALID</c>.
    /// </para>
    ///-
    function get_linkState():Integer;

    ////
    /// <summary>
    ///   Returns the bluetooth receiver signal strength, in pourcents, or 0 if no connection is established.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the bluetooth receiver signal strength, in pourcents, or 0 if no
    ///   connection is established
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YBluetoothLink.LINKQUALITY_INVALID</c>.
    /// </para>
    ///-
    function get_linkQuality():LongInt;

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
    ///   Use the method <c>YBluetoothLink.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YBluetoothLink</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindBluetoothLink(func: string):TYBluetoothLink;

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
    function registerValueCallback(callback: TYBluetoothLinkValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Attempt to connect to the previously selected remote device.
    /// <para>
    /// </para>
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
    function connect():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Disconnect from the previously selected remote device.
    /// <para>
    /// </para>
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
    function disconnect():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of Bluetooth sound controllers started using <c>yFirstBluetoothLink()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned Bluetooth sound controllers order.
    ///   If you want to find a specific a Bluetooth sound controller, use <c>BluetoothLink.findBluetoothLink()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YBluetoothLink</c> object, corresponding to
    ///   a Bluetooth sound controller currently online, or a <c>NIL</c> pointer
    ///   if there are no more Bluetooth sound controllers to enumerate.
    /// </returns>
    ///-
    function nextBluetoothLink():TYBluetoothLink;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstBluetoothLink():TYBluetoothLink;
  //--- (end of YBluetoothLink accessors declaration)
  end;

//--- (YBluetoothLink functions declaration)
  ////
  /// <summary>
  ///   Retrieves a Bluetooth sound controller for a given identifier.
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
  ///   This function does not require that the Bluetooth sound controller is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YBluetoothLink.isOnline()</c> to test if the Bluetooth sound controller is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a Bluetooth sound controller by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the Bluetooth sound controller, for instance
  ///   <c>MyDevice.bluetoothLink1</c>.
  /// </param>
  /// <returns>
  ///   a <c>YBluetoothLink</c> object allowing you to drive the Bluetooth sound controller.
  /// </returns>
  ///-
  function yFindBluetoothLink(func:string):TYBluetoothLink;
  ////
  /// <summary>
  ///   Starts the enumeration of Bluetooth sound controllers currently accessible.
  /// <para>
  ///   Use the method <c>YBluetoothLink.nextBluetoothLink()</c> to iterate on
  ///   next Bluetooth sound controllers.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YBluetoothLink</c> object, corresponding to
  ///   the first Bluetooth sound controller currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstBluetoothLink():TYBluetoothLink;

//--- (end of YBluetoothLink functions declaration)

implementation

//--- (YBluetoothLink dlldef)
//--- (end of YBluetoothLink dlldef)

  constructor TYBluetoothLink.Create(func:string);
    begin
      inherited Create(func);
      _className := 'BluetoothLink';
      //--- (YBluetoothLink accessors initialization)
      _ownAddress := Y_OWNADDRESS_INVALID;
      _pairingPin := Y_PAIRINGPIN_INVALID;
      _remoteAddress := Y_REMOTEADDRESS_INVALID;
      _remoteName := Y_REMOTENAME_INVALID;
      _mute := Y_MUTE_INVALID;
      _preAmplifier := Y_PREAMPLIFIER_INVALID;
      _volume := Y_VOLUME_INVALID;
      _linkState := Y_LINKSTATE_INVALID;
      _linkQuality := Y_LINKQUALITY_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackBluetoothLink := nil;
      //--- (end of YBluetoothLink accessors initialization)
    end;

//--- (YBluetoothLink yapiwrapper)
//--- (end of YBluetoothLink yapiwrapper)

//--- (YBluetoothLink implementation)
{$HINTS OFF}
  function TYBluetoothLink._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'ownAddress') then
        begin
          _ownAddress := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'pairingPin') then
        begin
          _pairingPin := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'remoteAddress') then
        begin
          _remoteAddress := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'remoteName') then
        begin
          _remoteName := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'mute') then
        begin
          _mute := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'preAmplifier') then
        begin
          _preAmplifier := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'volume') then
        begin
          _volume := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'linkState') then
        begin
          _linkState := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'linkQuality') then
        begin
          _linkQuality := integer(member^.ivalue);
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

  function TYBluetoothLink.get_ownAddress():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_OWNADDRESS_INVALID;
              exit;
            end;
        end;
      res := self._ownAddress;
      result := res;
      exit;
    end;


  function TYBluetoothLink.get_pairingPin():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PAIRINGPIN_INVALID;
              exit;
            end;
        end;
      res := self._pairingPin;
      result := res;
      exit;
    end;


  function TYBluetoothLink.set_pairingPin(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('pairingPin',rest_val);
    end;

  function TYBluetoothLink.get_remoteAddress():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_REMOTEADDRESS_INVALID;
              exit;
            end;
        end;
      res := self._remoteAddress;
      result := res;
      exit;
    end;


  function TYBluetoothLink.set_remoteAddress(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('remoteAddress',rest_val);
    end;

  function TYBluetoothLink.get_remoteName():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_REMOTENAME_INVALID;
              exit;
            end;
        end;
      res := self._remoteName;
      result := res;
      exit;
    end;


  function TYBluetoothLink.get_mute():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_MUTE_INVALID;
              exit;
            end;
        end;
      res := self._mute;
      result := res;
      exit;
    end;


  function TYBluetoothLink.set_mute(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('mute',rest_val);
    end;

  function TYBluetoothLink.get_preAmplifier():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PREAMPLIFIER_INVALID;
              exit;
            end;
        end;
      res := self._preAmplifier;
      result := res;
      exit;
    end;


  function TYBluetoothLink.set_preAmplifier(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('preAmplifier',rest_val);
    end;

  function TYBluetoothLink.get_volume():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_VOLUME_INVALID;
              exit;
            end;
        end;
      res := self._volume;
      result := res;
      exit;
    end;


  function TYBluetoothLink.set_volume(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('volume',rest_val);
    end;

  function TYBluetoothLink.get_linkState():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LINKSTATE_INVALID;
              exit;
            end;
        end;
      res := self._linkState;
      result := res;
      exit;
    end;


  function TYBluetoothLink.get_linkQuality():LongInt;
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


  function TYBluetoothLink.get_command():string;
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


  function TYBluetoothLink.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYBluetoothLink.FindBluetoothLink(func: string):TYBluetoothLink;
    var
      obj : TYBluetoothLink;
    begin
      obj := TYBluetoothLink(TYFunction._FindFromCache('BluetoothLink', func));
      if (obj = nil) then
        begin
          obj :=  TYBluetoothLink.create(func);
          TYFunction._AddToCache('BluetoothLink', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYBluetoothLink.registerValueCallback(callback: TYBluetoothLinkValueCallback):LongInt;
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
      self._valueCallbackBluetoothLink := callback;
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


  function TYBluetoothLink._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackBluetoothLink) <> nil) then
        begin
          self._valueCallbackBluetoothLink(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYBluetoothLink.connect():LongInt;
    begin
      result := self.set_command('C');
      exit;
    end;


  function TYBluetoothLink.disconnect():LongInt;
    begin
      result := self.set_command('D');
      exit;
    end;


  function TYBluetoothLink.nextBluetoothLink(): TYBluetoothLink;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextBluetoothLink := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextBluetoothLink := nil;
          exit;
        end;
      nextBluetoothLink := TYBluetoothLink.FindBluetoothLink(hwid);
    end;

  class function TYBluetoothLink.FirstBluetoothLink(): TYBluetoothLink;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('BluetoothLink', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYBluetoothLink.FindBluetoothLink(serial+'.'+funcId);
    end;

//--- (end of YBluetoothLink implementation)

//--- (YBluetoothLink functions)

  function yFindBluetoothLink(func:string): TYBluetoothLink;
    begin
      result := TYBluetoothLink.FindBluetoothLink(func);
    end;

  function yFirstBluetoothLink(): TYBluetoothLink;
    begin
      result := TYBluetoothLink.FirstBluetoothLink();
    end;

  procedure _BluetoothLinkCleanup();
    begin
    end;

//--- (end of YBluetoothLink functions)

initialization
  //--- (YBluetoothLink initialization)
  //--- (end of YBluetoothLink initialization)

finalization
  //--- (YBluetoothLink cleanup)
  _BluetoothLinkCleanup();
  //--- (end of YBluetoothLink cleanup)

end.
