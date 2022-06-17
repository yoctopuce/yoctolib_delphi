{*********************************************************************
 *
 * $Id: yocto_messagebox.pas 50144 2022-06-17 06:59:52Z seb $
 *
 * Implements yFindMessageBox(), the high-level API for Cellular functions
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


unit yocto_messagebox;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}
{$WARN USE_BEFORE_DEF OFF}
interface

uses
  sysutils, classes,{$IFNDEF UNIX}windows,
{$ENDIF}
 yocto_api, yjson;

//--- (generated code: YMessageBox definitions)

const Y_SLOTSINUSE_INVALID            = YAPI_INVALID_UINT;
const Y_SLOTSCOUNT_INVALID            = YAPI_INVALID_UINT;
const Y_SLOTSBITMAP_INVALID           = YAPI_INVALID_STRING;
const Y_PDUSENT_INVALID               = YAPI_INVALID_UINT;
const Y_PDURECEIVED_INVALID           = YAPI_INVALID_UINT;
const Y_COMMAND_INVALID               = YAPI_INVALID_STRING;


//--- (end of generated code: YMessageBox definitions)

//--- (generated code: YSms definitions)


//--- (end of generated code: YSms definitions)


type
  TYMessageBox = class;
  TYSms = class;

  TYSmsArray = array  of TYSms;

  //--- (generated code: YMessageBox class start)
  TYMessageBoxValueCallback = procedure(func: TYMessageBox; value:string);
  TYMessageBoxTimedReportCallback = procedure(func: TYMessageBox; value:TYMeasure);

  ////
  /// <summary>
  ///   TYMessageBox Class: SMS message box interface control interface, available for instance in the
  ///   YoctoHub-GSM-2G, the YoctoHub-GSM-3G-EU, the YoctoHub-GSM-3G-NA or the YoctoHub-GSM-4G
  /// <para>
  ///   The <c>YMessageBox</c> class provides SMS sending and receiving capability for
  ///   GSM-enabled Yoctopuce devices.
  /// </para>
  /// </summary>
  ///-
  TYMessageBox=class(TYFunction)
  //--- (end of generated code: YMessageBox class start)
  protected
  //--- (generated code: YMessageBox declaration)
    // Attributes (function value cache)
    _slotsInUse               : LongInt;
    _slotsCount               : LongInt;
    _slotsBitmap              : string;
    _pduSent                  : LongInt;
    _pduReceived              : LongInt;
    _command                  : string;
    _valueCallbackMessageBox  : TYMessageBoxValueCallback;
    _nextMsgRef               : LongInt;
    _prevBitmapStr            : string;
    _pdus                     : TYSmsArray;
    _messages                 : TYSmsArray;
    _gsm2unicodeReady         : boolean;
    _gsm2unicode              : TLongIntArray;
    _iso2gsm                  : TByteArray;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YMessageBox declaration)

  public
    //--- (generated code: YMessageBox accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the number of message storage slots currently in use.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of message storage slots currently in use
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMessageBox.SLOTSINUSE_INVALID</c>.
    /// </para>
    ///-
    function get_slotsInUse():LongInt;

    ////
    /// <summary>
    ///   Returns the total number of message storage slots on the SIM card.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the total number of message storage slots on the SIM card
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMessageBox.SLOTSCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_slotsCount():LongInt;

    function get_slotsBitmap():string;

    ////
    /// <summary>
    ///   Returns the number of SMS units sent so far.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of SMS units sent so far
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMessageBox.PDUSENT_INVALID</c>.
    /// </para>
    ///-
    function get_pduSent():LongInt;

    ////
    /// <summary>
    ///   Changes the value of the outgoing SMS units counter.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the value of the outgoing SMS units counter
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
    function set_pduSent(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the number of SMS units received so far.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of SMS units received so far
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YMessageBox.PDURECEIVED_INVALID</c>.
    /// </para>
    ///-
    function get_pduReceived():LongInt;

    ////
    /// <summary>
    ///   Changes the value of the incoming SMS units counter.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the value of the incoming SMS units counter
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
    function set_pduReceived(newval:LongInt):integer;

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
    ///   Use the method <c>YMessageBox.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YMessageBox</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindMessageBox(func: string):TYMessageBox;

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
    function registerValueCallback(callback: TYMessageBoxValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function nextMsgRef():LongInt; overload; virtual;

    function clearSIMSlot(slot: LongInt):LongInt; overload; virtual;

    function _AT(cmd: string):string; overload; virtual;

    function fetchPdu(slot: LongInt):TYSms; overload; virtual;

    function initGsm2Unicode():LongInt; overload; virtual;

    function gsm2unicode(gsm: TByteArray):TLongIntArray; overload; virtual;

    function gsm2str(gsm: TByteArray):string; overload; virtual;

    function str2gsm(msg: string):TByteArray; overload; virtual;

    function checkNewMessages():LongInt; overload; virtual;

    function get_pdus():TYSmsArray; overload; virtual;

    ////
    /// <summary>
    ///   Clear the SMS units counters.
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
    function clearPduCounters():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a regular text SMS, with standard parameters.
    /// <para>
    ///   This function can send messages
    ///   of more than 160 characters, using SMS concatenation. ISO-latin accented characters
    ///   are supported. For sending messages with special unicode characters such as asian
    ///   characters and emoticons, use <c>newMessage</c> to create a new message and define
    ///   the content of using methods <c>addText</c> and <c>addUnicodeData</c>.
    /// </para>
    /// </summary>
    /// <param name="recipient">
    ///   a text string with the recipient phone number, either as a
    ///   national number, or in international format starting with a plus sign
    /// </param>
    /// <param name="message">
    ///   the text to be sent in the message
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function sendTextMessage(recipient: string; message: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends a Flash SMS (class 0 message).
    /// <para>
    ///   Flash messages are displayed on the handset
    ///   immediately and are usually not saved on the SIM card. This function can send messages
    ///   of more than 160 characters, using SMS concatenation. ISO-latin accented characters
    ///   are supported. For sending messages with special unicode characters such as asian
    ///   characters and emoticons, use <c>newMessage</c> to create a new message and define
    ///   the content of using methods <c>addText</c> et <c>addUnicodeData</c>.
    /// </para>
    /// </summary>
    /// <param name="recipient">
    ///   a text string with the recipient phone number, either as a
    ///   national number, or in international format starting with a plus sign
    /// </param>
    /// <param name="message">
    ///   the text to be sent in the message
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function sendFlashMessage(recipient: string; message: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Creates a new empty SMS message, to be configured and sent later on.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="recipient">
    ///   a text string with the recipient phone number, either as a
    ///   national number, or in international format starting with a plus sign
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function newMessage(recipient: string):TYSms; overload; virtual;

    ////
    /// <summary>
    ///   Returns the list of messages received and not deleted.
    /// <para>
    ///   This function
    ///   will automatically decode concatenated SMS.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an YSms object list.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty list.
    /// </para>
    ///-
    function get_messages():TYSmsArray; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of SMS message box interfaces started using <c>yFirstMessageBox()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned SMS message box interfaces order.
    ///   If you want to find a specific a SMS message box interface, use <c>MessageBox.findMessageBox()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YMessageBox</c> object, corresponding to
    ///   a SMS message box interface currently online, or a <c>NIL</c> pointer
    ///   if there are no more SMS message box interfaces to enumerate.
    /// </returns>
    ///-
    function nextMessageBox():TYMessageBox;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstMessageBox():TYMessageBox;
  //--- (end of generated code: YMessageBox accessors declaration)
  end;



  //--- (generated code: YSms class start)
  ////
  /// <summary>
  ///   TYSms Class: SMS message sent or received, returned by <c>messageBox.get_messages</c> or <c>messageBox.
  /// <para>
  ///   newMessage</c>
  /// </para>
  /// <para>
  ///   <c>YSms</c> objects are used to describe an SMS message, received or to be sent.
  ///   These objects are used in particular in conjunction with the <c>YMessageBox</c> class.
  /// </para>
  /// </summary>
  ///-
  TYSms=class(TObject)
  //--- (end of generated code: YSms class start)
  protected
  //--- (generated code: YSms declaration)
    // Attributes (function value cache)
    _mbox                     : TYMessageBox;
    _slot                     : LongInt;
    _deliv                    : boolean;
    _smsc                     : string;
    _mref                     : LongInt;
    _orig                     : string;
    _dest                     : string;
    _pid                      : LongInt;
    _alphab                   : LongInt;
    _mclass                   : LongInt;
    _stamp                    : string;
    _udh                      : TByteArray;
    _udata                    : TByteArray;
    _npdu                     : LongInt;
    _pdu                      : TByteArray;
    _parts                    : TYSmsArray;
    _aggSig                   : string;
    _aggIdx                   : LongInt;
    _aggCnt                   : LongInt;

    //--- (end of generated code: YSms declaration)

  public
    constructor create(mbox :TYMessageBox);

    //--- (generated code: YSms accessors declaration)
    function get_slot():LongInt; overload; virtual;

    function get_smsc():string; overload; virtual;

    function get_msgRef():LongInt; overload; virtual;

    function get_sender():string; overload; virtual;

    function get_recipient():string; overload; virtual;

    function get_protocolId():LongInt; overload; virtual;

    function isReceived():boolean; overload; virtual;

    function get_alphabet():LongInt; overload; virtual;

    function get_msgClass():LongInt; overload; virtual;

    function get_dcs():LongInt; overload; virtual;

    function get_timestamp():string; overload; virtual;

    function get_userDataHeader():TByteArray; overload; virtual;

    function get_userData():TByteArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns the content of the message.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with the content of the message.
    /// </returns>
    ///-
    function get_textData():string; overload; virtual;

    function get_unicodeData():TLongIntArray; overload; virtual;

    function get_partCount():LongInt; overload; virtual;

    function get_pdu():TByteArray; overload; virtual;

    function get_parts():TYSmsArray; overload; virtual;

    function get_concatSignature():string; overload; virtual;

    function get_concatIndex():LongInt; overload; virtual;

    function get_concatCount():LongInt; overload; virtual;

    function set_slot(val: LongInt):LongInt; overload; virtual;

    function set_received(val: boolean):LongInt; overload; virtual;

    function set_smsc(val: string):LongInt; overload; virtual;

    function set_msgRef(val: LongInt):LongInt; overload; virtual;

    function set_sender(val: string):LongInt; overload; virtual;

    function set_recipient(val: string):LongInt; overload; virtual;

    function set_protocolId(val: LongInt):LongInt; overload; virtual;

    function set_alphabet(val: LongInt):LongInt; overload; virtual;

    function set_msgClass(val: LongInt):LongInt; overload; virtual;

    function set_dcs(val: LongInt):LongInt; overload; virtual;

    function set_timestamp(val: string):LongInt; overload; virtual;

    function set_userDataHeader(val: TByteArray):LongInt; overload; virtual;

    function set_userData(val: TByteArray):LongInt; overload; virtual;

    function convertToUnicode():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Add a regular text to the SMS.
    /// <para>
    ///   This function support messages
    ///   of more than 160 characters. ISO-latin accented characters
    ///   are supported. For messages with special unicode characters such as asian
    ///   characters and emoticons, use the  <c>addUnicodeData</c> method.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="val">
    ///   the text to be sent in the message
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    ///-
    function addText(val: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Add a unicode text to the SMS.
    /// <para>
    ///   This function support messages
    ///   of more than 160 characters, using SMS concatenation.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="val">
    ///   an array of special unicode characters
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    ///-
    function addUnicodeData(val: TLongIntArray):LongInt; overload; virtual;

    function set_pdu(pdu: TByteArray):LongInt; overload; virtual;

    function set_parts(parts: TYSmsArray):LongInt; overload; virtual;

    function encodeAddress(addr: string):TByteArray; overload; virtual;

    function decodeAddress(addr: TByteArray; ofs: LongInt; siz: LongInt):string; overload; virtual;

    function encodeTimeStamp(exp: string):TByteArray; overload; virtual;

    function decodeTimeStamp(exp: TByteArray; ofs: LongInt; siz: LongInt):string; overload; virtual;

    function udataSize():LongInt; overload; virtual;

    function encodeUserData():TByteArray; overload; virtual;

    function generateParts():LongInt; overload; virtual;

    function generatePdu():LongInt; overload; virtual;

    function parseUserDataHeader():LongInt; overload; virtual;

    function parsePdu(pdu: TByteArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Sends the SMS to the recipient.
    /// <para>
    ///   Messages of more than 160 characters are supported
    ///   using SMS concatenation.
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
    function send():LongInt; overload; virtual;

    function deleteFromSIM():LongInt; overload; virtual;


  //--- (end of generated code: YSms accessors declaration)
  end;


//--- (generated code: YMessageBox functions declaration)
  ////
  /// <summary>
  ///   Retrieves a SMS message box interface for a given identifier.
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
  ///   This function does not require that the SMS message box interface is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YMessageBox.isOnline()</c> to test if the SMS message box interface is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a SMS message box interface by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the SMS message box interface, for instance
  ///   <c>YHUBGSM1.messageBox</c>.
  /// </param>
  /// <returns>
  ///   a <c>YMessageBox</c> object allowing you to drive the SMS message box interface.
  /// </returns>
  ///-
  function yFindMessageBox(func:string):TYMessageBox;
  ////
  /// <summary>
  ///   Starts the enumeration of SMS message box interfaces currently accessible.
  /// <para>
  ///   Use the method <c>YMessageBox.nextMessageBox()</c> to iterate on
  ///   next SMS message box interfaces.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YMessageBox</c> object, corresponding to
  ///   the first SMS message box interface currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstMessageBox():TYMessageBox;

//--- (end of generated code: YMessageBox functions declaration)
//--- (generated code: YSms functions declaration)
//--- (end of generated code: YSms functions declaration)

implementation
//--- (generated code: YMessageBox dlldef)
//--- (end of generated code: YMessageBox dlldef)
//--- (generated code: YSms dlldef)
//--- (end of generated code: YSms dlldef)

  constructor TYMessageBox.Create(func:string);
    begin
      inherited Create(func);
      _className := 'MessageBox';
      //--- (generated code: YMessageBox accessors initialization)
      _slotsInUse := Y_SLOTSINUSE_INVALID;
      _slotsCount := Y_SLOTSCOUNT_INVALID;
      _slotsBitmap := Y_SLOTSBITMAP_INVALID;
      _pduSent := Y_PDUSENT_INVALID;
      _pduReceived := Y_PDURECEIVED_INVALID;
      _command := Y_COMMAND_INVALID;
      _valueCallbackMessageBox := nil;
      _nextMsgRef := 0;
      _prevBitmapStr := '';
      //--- (end of generated code: YMessageBox accessors initialization)
    end;


//--- (generated code: YMessageBox implementation)
{$HINTS OFF}
  function TYMessageBox._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'slotsInUse') then
        begin
          _slotsInUse := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'slotsCount') then
        begin
          _slotsCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'slotsBitmap') then
        begin
          _slotsBitmap := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'pduSent') then
        begin
          _pduSent := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'pduReceived') then
        begin
          _pduReceived := integer(member^.ivalue);
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

  function TYMessageBox.get_slotsInUse():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SLOTSINUSE_INVALID;
              exit;
            end;
        end;
      res := self._slotsInUse;
      result := res;
      exit;
    end;


  function TYMessageBox.get_slotsCount():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SLOTSCOUNT_INVALID;
              exit;
            end;
        end;
      res := self._slotsCount;
      result := res;
      exit;
    end;


  function TYMessageBox.get_slotsBitmap():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SLOTSBITMAP_INVALID;
              exit;
            end;
        end;
      res := self._slotsBitmap;
      result := res;
      exit;
    end;


  function TYMessageBox.get_pduSent():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PDUSENT_INVALID;
              exit;
            end;
        end;
      res := self._pduSent;
      result := res;
      exit;
    end;


  function TYMessageBox.set_pduSent(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('pduSent',rest_val);
    end;

  function TYMessageBox.get_pduReceived():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PDURECEIVED_INVALID;
              exit;
            end;
        end;
      res := self._pduReceived;
      result := res;
      exit;
    end;


  function TYMessageBox.set_pduReceived(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('pduReceived',rest_val);
    end;

  function TYMessageBox.get_command():string;
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


  function TYMessageBox.set_command(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('command',rest_val);
    end;

  class function TYMessageBox.FindMessageBox(func: string):TYMessageBox;
    var
      obj : TYMessageBox;
    begin
      obj := TYMessageBox(TYFunction._FindFromCache('MessageBox', func));
      if obj = nil then
        begin
          obj :=  TYMessageBox.create(func);
          TYFunction._AddToCache('MessageBox',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYMessageBox.registerValueCallback(callback: TYMessageBoxValueCallback):LongInt;
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
      self._valueCallbackMessageBox := callback;
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


  function TYMessageBox._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackMessageBox) <> nil) then
        begin
          self._valueCallbackMessageBox(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYMessageBox.nextMsgRef():LongInt;
    begin
      self._nextMsgRef := self._nextMsgRef + 1;
      result := self._nextMsgRef;
      exit;
    end;


  function TYMessageBox.clearSIMSlot(slot: LongInt):LongInt;
    var
      retry : LongInt;
      idx : LongInt;
      res : string;
      bitmapStr : string;
      int_res : LongInt;
      newBitmap : TByteArray;
      bitVal : LongInt;
    begin
      retry := 5;
      while retry > 0 do
        begin
          self.clearCache;
          bitmapStr := self.get_slotsBitmap;
          newBitmap := _hexStrToBin(bitmapStr);
          idx := ((slot) shr 3);
          if idx < length(newBitmap) then
            begin
              bitVal := ((1) shl ((((slot) and 7))));
              if (((newBitmap[idx]) and (bitVal))) <> 0 then
                begin
                  self._prevBitmapStr := '';
                  int_res := self.set_command('DS'+inttostr(slot));
                  if int_res < 0 then
                    begin
                      result := int_res;
                      exit;
                    end;
                end
              else
                begin
                  result := YAPI_SUCCESS;
                  exit;
                end;
            end
          else
            begin
              result := YAPI_INVALID_ARGUMENT;
              exit;
            end;
          res := self._AT('');
          retry := retry - 1;
        end;
      result := YAPI_IO_ERROR;
      exit;
    end;


  function TYMessageBox._AT(cmd: string):string;
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


  function TYMessageBox.fetchPdu(slot: LongInt):TYSms;
    var
      binPdu : TByteArray;
      arrPdu : TStringArray;
      hexPdu : string;
      sms : TYSms;
    begin
      SetLength(arrPdu, 0);

      binPdu := self._download('sms.json?pos='+inttostr(slot)+'&len=1');
      arrPdu := self._json_get_array(binPdu);
      hexPdu := self._decode_json_string(arrPdu[0]);
      sms :=  TYSms.create(self);
      sms.set_slot(slot);
      sms.parsePdu(_hexStrToBin(hexPdu));
      result := sms;
      exit;
    end;


  function TYMessageBox.initGsm2Unicode():LongInt;
    var
      i : LongInt;
      uni : LongInt;
      gsm2unicode_pos : LongInt;
    begin
      gsm2unicode_pos := 0;
      SetLength(self._gsm2unicode, 256);;
      // 00-07
      self._gsm2unicode[gsm2unicode_pos] := 64;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 163;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 36;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 165;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 232;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 233;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 249;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 236;
      inc(gsm2unicode_pos);
      // 08-0F
      self._gsm2unicode[gsm2unicode_pos] := 242;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 199;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 10;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 216;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 248;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 13;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 197;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 229;
      inc(gsm2unicode_pos);
      // 10-17
      self._gsm2unicode[gsm2unicode_pos] := 916;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 95;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 934;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 915;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 923;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 937;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 928;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 936;
      inc(gsm2unicode_pos);
      // 18-1F
      self._gsm2unicode[gsm2unicode_pos] := 931;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 920;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 926;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 27;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 198;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 230;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 223;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 201;
      inc(gsm2unicode_pos);
      // 20-7A
      i := 32;
      while i <= 122 do
        begin
          self._gsm2unicode[gsm2unicode_pos] := i;
          inc(gsm2unicode_pos);
          i := i + 1;
        end;
      // exceptions in range 20-7A
      self._gsm2unicode[ 36] := 164;
      self._gsm2unicode[ 64] := 161;
      self._gsm2unicode[ 91] := 196;
      self._gsm2unicode[ 92] := 214;
      self._gsm2unicode[ 93] := 209;
      self._gsm2unicode[ 94] := 220;
      self._gsm2unicode[ 95] := 167;
      self._gsm2unicode[ 96] := 191;
      // 7B-7F
      self._gsm2unicode[gsm2unicode_pos] := 228;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 246;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 241;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 252;
      inc(gsm2unicode_pos);
      self._gsm2unicode[gsm2unicode_pos] := 224;
      inc(gsm2unicode_pos);
      SetLength(self._gsm2unicode, gsm2unicode_pos);;
      // Invert table as well wherever possible
      setlength(self._iso2gsm,256);
      i := 0;
      while i <= 127 do
        begin
          uni := self._gsm2unicode[i];
          if uni <= 255 then
            begin
              self._iso2gsm[uni] := i;
            end;
          i := i + 1;
        end;
      i := 0;
      while i < 4 do
        begin
          // mark escape sequences
          self._iso2gsm[91+i] := 27;
          self._iso2gsm[123+i] := 27;
          i := i + 1;
        end;
      // Done
      self._gsm2unicodeReady := true;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYMessageBox.gsm2unicode(gsm: TByteArray):TLongIntArray;
    var
      i : LongInt;
      gsmlen : LongInt;
      reslen : LongInt;
      res : TLongIntArray;
      uni : LongInt;
      res_pos : LongInt;
    begin
      if not(self._gsm2unicodeReady) then
        begin
          self.initGsm2Unicode;
        end;
      gsmlen := length(gsm);
      reslen := gsmlen;
      i := 0;
      while i < gsmlen do
        begin
          if gsm[i] = 27 then
            begin
              reslen := reslen - 1;
            end;
          i := i + 1;
        end;
      res_pos := 0;
      SetLength(res, reslen);;
      i := 0;
      while i < gsmlen do
        begin
          uni := self._gsm2unicode[gsm[i]];
          if (uni = 27) and(i+1 < gsmlen) then
            begin
              i := i + 1;
              uni := gsm[i];
              if uni < 60 then
                begin
                  if uni < 41 then
                    begin
                      if uni=20 then
                        begin
                          uni:=94;
                        end
                      else
                        begin
                          if uni=40 then
                            begin
                              uni:=123;
                            end
                          else
                            begin
                              uni:=0;
                            end;
                        end;
                    end
                  else
                    begin
                      if uni=41 then
                        begin
                          uni:=125;
                        end
                      else
                        begin
                          if uni=47 then
                            begin
                              uni:=92;
                            end
                          else
                            begin
                              uni:=0;
                            end;
                        end;
                    end;
                end
              else
                begin
                  if uni < 62 then
                    begin
                      if uni=60 then
                        begin
                          uni:=91;
                        end
                      else
                        begin
                          if uni=61 then
                            begin
                              uni:=126;
                            end
                          else
                            begin
                              uni:=0;
                            end;
                        end;
                    end
                  else
                    begin
                      if uni=62 then
                        begin
                          uni:=93;
                        end
                      else
                        begin
                          if uni=64 then
                            begin
                              uni:=124;
                            end
                          else
                            begin
                              if uni=101 then
                                begin
                                  uni:=164;
                                end
                              else
                                begin
                                  uni:=0;
                                end;
                            end;
                        end;
                    end;
                end;
            end;
          if uni > 0 then
            begin
              res[res_pos] := uni;
              inc(res_pos);
            end;
          i := i + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYMessageBox.gsm2str(gsm: TByteArray):string;
    var
      i : LongInt;
      gsmlen : LongInt;
      reslen : LongInt;
      resbin : TByteArray;
      resstr : string;
      uni : LongInt;
    begin
      if not(self._gsm2unicodeReady) then
        begin
          self.initGsm2Unicode;
        end;
      gsmlen := length(gsm);
      reslen := gsmlen;
      i := 0;
      while i < gsmlen do
        begin
          if gsm[i] = 27 then
            begin
              reslen := reslen - 1;
            end;
          i := i + 1;
        end;
      setlength(resbin,reslen);
      i := 0;
      reslen := 0;
      while i < gsmlen do
        begin
          uni := self._gsm2unicode[gsm[i]];
          if (uni = 27) and(i+1 < gsmlen) then
            begin
              i := i + 1;
              uni := gsm[i];
              if uni < 60 then
                begin
                  if uni < 41 then
                    begin
                      if uni=20 then
                        begin
                          uni:=94;
                        end
                      else
                        begin
                          if uni=40 then
                            begin
                              uni:=123;
                            end
                          else
                            begin
                              uni:=0;
                            end;
                        end;
                    end
                  else
                    begin
                      if uni=41 then
                        begin
                          uni:=125;
                        end
                      else
                        begin
                          if uni=47 then
                            begin
                              uni:=92;
                            end
                          else
                            begin
                              uni:=0;
                            end;
                        end;
                    end;
                end
              else
                begin
                  if uni < 62 then
                    begin
                      if uni=60 then
                        begin
                          uni:=91;
                        end
                      else
                        begin
                          if uni=61 then
                            begin
                              uni:=126;
                            end
                          else
                            begin
                              uni:=0;
                            end;
                        end;
                    end
                  else
                    begin
                      if uni=62 then
                        begin
                          uni:=93;
                        end
                      else
                        begin
                          if uni=64 then
                            begin
                              uni:=124;
                            end
                          else
                            begin
                              if uni=101 then
                                begin
                                  uni:=164;
                                end
                              else
                                begin
                                  uni:=0;
                                end;
                            end;
                        end;
                    end;
                end;
            end;
          if (uni > 0) and(uni < 256) then
            begin
              resbin[reslen] := uni;
              reslen := reslen + 1;
            end;
          i := i + 1;
        end;
      resstr := _ByteToString(resbin);
      if Length(resstr) > reslen then
        begin
          resstr := Copy(resstr, 0 + 1, reslen);
        end;
      result := resstr;
      exit;
    end;


  function TYMessageBox.str2gsm(msg: string):TByteArray;
    var
      asc : TByteArray;
      asclen : LongInt;
      i : LongInt;
      ch : LongInt;
      gsm7 : LongInt;
      extra : LongInt;
      res : TByteArray;
      wpos : LongInt;
    begin
      if not(self._gsm2unicodeReady) then
        begin
          self.initGsm2Unicode;
        end;
      asc := _StrToByte(msg);
      asclen := length(asc);
      extra := 0;
      i := 0;
      while i < asclen do
        begin
          ch := asc[i];
          gsm7 := self._iso2gsm[ch];
          if gsm7 = 27 then
            begin
              extra := extra + 1;
            end;
          if gsm7 = 0 then
            begin
              // cannot use standard GSM encoding
              setlength(res,0);
              result := res;
              exit;
            end;
          i := i + 1;
        end;
      setlength(res,asclen+extra);
      wpos := 0;
      i := 0;
      while i < asclen do
        begin
          ch := asc[i];
          gsm7 := self._iso2gsm[ch];
          res[wpos] := gsm7;
          wpos := wpos + 1;
          if gsm7 = 27 then
            begin
              if ch < 100 then
                begin
                  if ch<93 then
                    begin
                      if ch<92 then
                        begin
                          gsm7:=60;
                        end
                      else
                        begin
                          gsm7:=47;
                        end;
                    end
                  else
                    begin
                      if ch<94 then
                        begin
                          gsm7:=62;
                        end
                      else
                        begin
                          gsm7:=20;
                        end;
                    end;
                end
              else
                begin
                  if ch<125 then
                    begin
                      if ch<124 then
                        begin
                          gsm7:=40;
                        end
                      else
                        begin
                          gsm7:=64;
                        end;
                    end
                  else
                    begin
                      if ch<126 then
                        begin
                          gsm7:=41;
                        end
                      else
                        begin
                          gsm7:=61;
                        end;
                    end;
                end;
              res[wpos] := gsm7;
              wpos := wpos + 1;
            end;
          i := i + 1;
        end;
      result := res;
      exit;
    end;


  function TYMessageBox.checkNewMessages():LongInt;
    var
      bitmapStr : string;
      prevBitmap : TByteArray;
      newBitmap : TByteArray;
      slot : LongInt;
      nslots : LongInt;
      pduIdx : LongInt;
      idx : LongInt;
      bitVal : LongInt;
      prevBit : LongInt;
      i : LongInt;
      nsig : LongInt;
      cnt : LongInt;
      sig : string;
      newArr : TYSmsArray;
      newMsg : TYSmsArray;
      newAgg : TYSmsArray;
      signatures : TStringArray;
      sms : TYSms;
      newArr_pos : LongInt;
      newMsg_pos : LongInt;
      signatures_pos : LongInt;
      newAgg_pos : LongInt;
    begin
      SetLength(signatures, 0);

      bitmapStr := self.get_slotsBitmap;
      if (bitmapStr = self._prevBitmapStr) then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      prevBitmap := _hexStrToBin(self._prevBitmapStr);
      newBitmap := _hexStrToBin(bitmapStr);
      self._prevBitmapStr := bitmapStr;
      nslots := 8*length(newBitmap);
      newArr_pos := 0;
      SetLength(newArr, nslots);;
      newMsg_pos := 0;
      SetLength(newMsg, nslots);;
      signatures_pos := 0;
      SetLength(signatures, nslots);;
      nsig := 0;
      // copy known messages
      pduIdx := 0;
      while pduIdx < length(self._pdus) do
        begin
          sms := self._pdus[pduIdx];
          slot := sms.get_slot;
          idx := ((slot) shr 3);
          if idx < length(newBitmap) then
            begin
              bitVal := ((1) shl ((((slot) and 7))));
              if (((newBitmap[idx]) and (bitVal))) <> 0 then
                begin
                  newArr[newArr_pos] := sms;
                  inc(newArr_pos);
                  if sms.get_concatCount = 0 then
                    begin
                      newMsg[newMsg_pos] := sms;
                      inc(newMsg_pos);
                    end
                  else
                    begin
                      sig := sms.get_concatSignature;
                      i := 0;
                      while (i < nsig) and(Length(sig) > 0) do
                        begin
                          if (signatures[i] = sig) then
                            begin
                              sig := '';
                            end;
                          i := i + 1;
                        end;
                      if Length(sig) > 0 then
                        begin
                          signatures[signatures_pos] := sig;
                          inc(signatures_pos);
                          nsig := nsig + 1;
                        end;
                    end;
                end;
            end;
          pduIdx := pduIdx + 1;
        end;
      // receive new messages
      slot := 0;
      while slot < nslots do
        begin
          idx := ((slot) shr 3);
          bitVal := ((1) shl ((((slot) and 7))));
          prevBit := 0;
          if idx < length(prevBitmap) then
            begin
              prevBit := ((prevBitmap[idx]) and (bitVal));
            end;
          if (((newBitmap[idx]) and (bitVal))) <> 0 then
            begin
              if prevBit = 0 then
                begin
                  sms := self.fetchPdu(slot);
                  newArr[newArr_pos] := sms;
                  inc(newArr_pos);
                  if sms.get_concatCount = 0 then
                    begin
                      newMsg[newMsg_pos] := sms;
                      inc(newMsg_pos);
                    end
                  else
                    begin
                      sig := sms.get_concatSignature;
                      i := 0;
                      while (i < nsig) and(Length(sig) > 0) do
                        begin
                          if (signatures[i] = sig) then
                            begin
                              sig := '';
                            end;
                          i := i + 1;
                        end;
                      if Length(sig) > 0 then
                        begin
                          signatures[signatures_pos] := sig;
                          inc(signatures_pos);
                          nsig := nsig + 1;
                        end;
                    end;
                end;
            end;
          slot := slot + 1;
        end;
      SetLength(newArr, newArr_pos);;
      self._pdus := newArr;
      // append complete concatenated messages
      i := 0;
      while i < nsig do
        begin
          sig := signatures[i];
          cnt := 0;
          pduIdx := 0;
          while pduIdx < length(self._pdus) do
            begin
              sms := self._pdus[pduIdx];
              if sms.get_concatCount > 0 then
                begin
                  if (sms.get_concatSignature = sig) then
                    begin
                      if cnt = 0 then
                        begin
                          cnt := sms.get_concatCount;
                          newAgg_pos := 0;
                          SetLength(newAgg, cnt);
                        end;
                      newAgg[newAgg_pos] := sms;
                      inc(newAgg_pos);
                    end;
                end;
              pduIdx := pduIdx + 1;
            end;
          SetLength(newAgg, newAgg_pos);
          if (cnt > 0) and(length(newAgg) = cnt) then
            begin
              sms :=  TYSms.create(self);
              sms.set_parts(newAgg);
              newMsg[newMsg_pos] := sms;
              inc(newMsg_pos);
            end;
          i := i + 1;
        end;
      SetLength(newMsg, newMsg_pos);;
      self._messages := newMsg;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYMessageBox.get_pdus():TYSmsArray;
    begin
      self.checkNewMessages;
      result := self._pdus;
      exit;
    end;


  function TYMessageBox.clearPduCounters():LongInt;
    var
      retcode : LongInt;
    begin
      retcode := self.set_pduReceived(0);
      if retcode <> YAPI_SUCCESS then
        begin
          result := retcode;
          exit;
        end;
      retcode := self.set_pduSent(0);
      result := retcode;
      exit;
    end;


  function TYMessageBox.sendTextMessage(recipient: string; message: string):LongInt;
    var
      sms : TYSms;
    begin
      sms :=  TYSms.create(self);
      sms.set_recipient(recipient);
      sms.addText(message);
      result := sms.send;
      exit;
    end;


  function TYMessageBox.sendFlashMessage(recipient: string; message: string):LongInt;
    var
      sms : TYSms;
    begin
      sms :=  TYSms.create(self);
      sms.set_recipient(recipient);
      sms.set_msgClass(0);
      sms.addText(message);
      result := sms.send;
      exit;
    end;


  function TYMessageBox.newMessage(recipient: string):TYSms;
    var
      sms : TYSms;
    begin
      sms :=  TYSms.create(self);
      sms.set_recipient(recipient);
      result := sms;
      exit;
    end;


  function TYMessageBox.get_messages():TYSmsArray;
    begin
      self.checkNewMessages;
      result := self._messages;
      exit;
    end;


  function TYMessageBox.nextMessageBox(): TYMessageBox;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextMessageBox := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextMessageBox := nil;
          exit;
        end;
      nextMessageBox := TYMessageBox.FindMessageBox(hwid);
    end;

  class function TYMessageBox.FirstMessageBox(): TYMessageBox;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('MessageBox', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYMessageBox.FindMessageBox(serial+'.'+funcId);
    end;

//--- (end of generated code: YMessageBox implementation)

//--- (generated code: YMessageBox functions)

  function yFindMessageBox(func:string): TYMessageBox;
    begin
      result := TYMessageBox.FindMessageBox(func);
    end;

  function yFirstMessageBox(): TYMessageBox;
    begin
      result := TYMessageBox.FirstMessageBox();
    end;

  procedure _MessageBoxCleanup();
    begin
    end;

//--- (end of generated code: YMessageBox functions)

  constructor TYSms.Create(mbox :TYMessageBox);
    begin
      //--- (generated code: YSms accessors initialization)
      _slot := 0;
      _smsc := '';
      _mref := 0;
      _orig := '';
      _dest := '';
      _pid := 0;
      _alphab := 0;
      _mclass := 0;
      _stamp := '';
      _npdu := 0;
      _aggSig := '';
      _aggIdx := 0;
      _aggCnt := 0;
      //--- (end of generated code: YSms accessors initialization)
      _mbox := mbox;
    end;

{$HINTS OFF}

//--- (generated code: YSms implementation)

  function TYSms.get_slot():LongInt;
    begin
      result := self._slot;
      exit;
    end;


  function TYSms.get_smsc():string;
    begin
      result := self._smsc;
      exit;
    end;


  function TYSms.get_msgRef():LongInt;
    begin
      result := self._mref;
      exit;
    end;


  function TYSms.get_sender():string;
    begin
      result := self._orig;
      exit;
    end;


  function TYSms.get_recipient():string;
    begin
      result := self._dest;
      exit;
    end;


  function TYSms.get_protocolId():LongInt;
    begin
      result := self._pid;
      exit;
    end;


  function TYSms.isReceived():boolean;
    begin
      result := self._deliv;
      exit;
    end;


  function TYSms.get_alphabet():LongInt;
    begin
      result := self._alphab;
      exit;
    end;


  function TYSms.get_msgClass():LongInt;
    begin
      if ((self._mclass) and 16) = 0 then
        begin
          result := -1;
          exit;
        end;
      result := ((self._mclass) and 3);
      exit;
    end;


  function TYSms.get_dcs():LongInt;
    begin
      result := ((self._mclass) or ((((self._alphab) shl 2))));
      exit;
    end;


  function TYSms.get_timestamp():string;
    begin
      result := self._stamp;
      exit;
    end;


  function TYSms.get_userDataHeader():TByteArray;
    begin
      result := self._udh;
      exit;
    end;


  function TYSms.get_userData():TByteArray;
    begin
      result := self._udata;
      exit;
    end;


  function TYSms.get_textData():string;
    var
      isolatin : TByteArray;
      isosize : LongInt;
      i : LongInt;
    begin
      if self._alphab = 0 then
        begin
          // using GSM standard 7-bit alphabet
          result := self._mbox.gsm2str(self._udata);
          exit;
        end;
      if self._alphab = 2 then
        begin
          // using UCS-2 alphabet
          isosize := ((length(self._udata)) shr 1);
          setlength(isolatin,isosize);
          i := 0;
          while i < isosize do
            begin
              isolatin[i] := self._udata[2*i+1];
              i := i + 1;
            end;
          result := _ByteToString(isolatin);
          exit;
        end;
      // default: convert 8 bit to string as-is
      result := _ByteToString(self._udata);
      exit;
    end;


  function TYSms.get_unicodeData():TLongIntArray;
    var
      res : TLongIntArray;
      unisize : LongInt;
      unival : LongInt;
      i : LongInt;
      res_pos : LongInt;
    begin
      if self._alphab = 0 then
        begin
          // using GSM standard 7-bit alphabet
          result := self._mbox.gsm2unicode(self._udata);
          exit;
        end;
      if self._alphab = 2 then
        begin
          // using UCS-2 alphabet
          unisize := ((length(self._udata)) shr 1);
          res_pos := 0;
          SetLength(res, unisize);
          i := 0;
          while i < unisize do
            begin
              unival := 256*self._udata[2*i]+self._udata[2*i+1];
              res[res_pos] := unival;
              inc(res_pos);
              i := i + 1;
            end;
        end
      else
        begin
          // return straight 8-bit values
          unisize := length(self._udata);
          res_pos := 0;
          SetLength(res, unisize);
          i := 0;
          while i < unisize do
            begin
              res[res_pos] := self._udata[i]+0;
              inc(res_pos);
              i := i + 1;
            end;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYSms.get_partCount():LongInt;
    begin
      if self._npdu = 0 then
        begin
          self.generatePdu;
        end;
      result := self._npdu;
      exit;
    end;


  function TYSms.get_pdu():TByteArray;
    begin
      if self._npdu = 0 then
        begin
          self.generatePdu;
        end;
      result := self._pdu;
      exit;
    end;


  function TYSms.get_parts():TYSmsArray;
    begin
      if self._npdu = 0 then
        begin
          self.generatePdu;
        end;
      result := self._parts;
      exit;
    end;


  function TYSms.get_concatSignature():string;
    begin
      if self._npdu = 0 then
        begin
          self.generatePdu;
        end;
      result := self._aggSig;
      exit;
    end;


  function TYSms.get_concatIndex():LongInt;
    begin
      if self._npdu = 0 then
        begin
          self.generatePdu;
        end;
      result := self._aggIdx;
      exit;
    end;


  function TYSms.get_concatCount():LongInt;
    begin
      if self._npdu = 0 then
        begin
          self.generatePdu;
        end;
      result := self._aggCnt;
      exit;
    end;


  function TYSms.set_slot(val: LongInt):LongInt;
    begin
      self._slot := val;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_received(val: boolean):LongInt;
    begin
      self._deliv := val;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_smsc(val: string):LongInt;
    begin
      self._smsc := val;
      self._npdu := 0;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_msgRef(val: LongInt):LongInt;
    begin
      self._mref := val;
      self._npdu := 0;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_sender(val: string):LongInt;
    begin
      self._orig := val;
      self._npdu := 0;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_recipient(val: string):LongInt;
    begin
      self._dest := val;
      self._npdu := 0;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_protocolId(val: LongInt):LongInt;
    begin
      self._pid := val;
      self._npdu := 0;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_alphabet(val: LongInt):LongInt;
    begin
      self._alphab := val;
      self._npdu := 0;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_msgClass(val: LongInt):LongInt;
    begin
      if val = -1 then
        begin
          self._mclass := 0;
        end
      else
        begin
          self._mclass := 16+val;
        end;
      self._npdu := 0;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_dcs(val: LongInt):LongInt;
    begin
      self._alphab := (((((val) shr 2))) and 3);
      self._mclass := ((val) and (16+3));
      self._npdu := 0;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_timestamp(val: string):LongInt;
    begin
      self._stamp := val;
      self._npdu := 0;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_userDataHeader(val: TByteArray):LongInt;
    begin
      self._udh := val;
      self._npdu := 0;
      self.parseUserDataHeader;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.set_userData(val: TByteArray):LongInt;
    begin
      self._udata := val;
      self._npdu := 0;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.convertToUnicode():LongInt;
    var
      ucs2 : TLongIntArray;
      udatalen : LongInt;
      i : LongInt;
      uni : LongInt;
      ucs2_pos : LongInt;
    begin
      if self._alphab = 2 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      if self._alphab = 0 then
        begin
          ucs2 := self._mbox.gsm2unicode(self._udata);
        end
      else
        begin
          udatalen := length(self._udata);
          ucs2_pos := 0;
          SetLength(ucs2, udatalen);
          i := 0;
          while i < udatalen do
            begin
              uni := self._udata[i];
              ucs2[ucs2_pos] := uni;
              inc(ucs2_pos);
              i := i + 1;
            end;
          SetLength(ucs2, ucs2_pos);
        end;
      self._alphab := 2;
      setlength(self._udata,0);
      self.addUnicodeData(ucs2);
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.addText(val: string):LongInt;
    var
      udata : TByteArray;
      udatalen : LongInt;
      newdata : TByteArray;
      newdatalen : LongInt;
      i : LongInt;
    begin
      if Length(val) = 0 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      if self._alphab = 0 then
        begin
          // Try to append using GSM 7-bit alphabet
          newdata := self._mbox.str2gsm(val);
          newdatalen := length(newdata);
          if newdatalen = 0 then
            begin
              // 7-bit not possible, switch to unicode
              self.convertToUnicode;
              newdata := _StrToByte(val);
              newdatalen := length(newdata);
            end;
        end
      else
        begin
          newdata := _StrToByte(val);
          newdatalen := length(newdata);
        end;
      udatalen := length(self._udata);
      if self._alphab = 2 then
        begin
          // Append in unicode directly
          setlength(udata,udatalen + 2*newdatalen);
          i := 0;
          while i < udatalen do
            begin
              udata[i] := self._udata[i];
              i := i + 1;
            end;
          i := 0;
          while i < newdatalen do
            begin
              udata[udatalen+1] := newdata[i];
              udatalen := udatalen + 2;
              i := i + 1;
            end;
        end
      else
        begin
          // Append binary buffers
          setlength(udata,udatalen+newdatalen);
          i := 0;
          while i < udatalen do
            begin
              udata[i] := self._udata[i];
              i := i + 1;
            end;
          i := 0;
          while i < newdatalen do
            begin
              udata[udatalen] := newdata[i];
              udatalen := udatalen + 1;
              i := i + 1;
            end;
        end;
      result := self.set_userData(udata);
      exit;
    end;


  function TYSms.addUnicodeData(val: TLongIntArray):LongInt;
    var
      arrlen : LongInt;
      newdatalen : LongInt;
      i : LongInt;
      uni : LongInt;
      udata : TByteArray;
      udatalen : LongInt;
      surrogate : LongInt;
    begin
      if self._alphab <> 2 then
        begin
          self.convertToUnicode;
        end;
      // compute number of 16-bit code units
      arrlen := length(val);
      newdatalen := arrlen;
      i := 0;
      while i < arrlen do
        begin
          uni := val[i];
          if uni > 65535 then
            begin
              newdatalen := newdatalen + 1;
            end;
          i := i + 1;
        end;
      // now build utf-16 buffer
      udatalen := length(self._udata);
      setlength(udata,udatalen+2*newdatalen);
      i := 0;
      while i < udatalen do
        begin
          udata[i] := self._udata[i];
          i := i + 1;
        end;
      i := 0;
      while i < arrlen do
        begin
          uni := val[i];
          if uni >= 65536 then
            begin
              surrogate := uni - 65536;
              uni := (((((surrogate) shr 10)) and 1023)) + 55296;
              udata[udatalen] := ((uni) shr 8);
              udata[udatalen+1] := ((uni) and 255);
              udatalen := udatalen + 2;
              uni := (((surrogate) and 1023)) + 56320;
            end;
          udata[udatalen] := ((uni) shr 8);
          udata[udatalen+1] := ((uni) and 255);
          udatalen := udatalen + 2;
          i := i + 1;
        end;
      result := self.set_userData(udata);
      exit;
    end;


  function TYSms.set_pdu(pdu: TByteArray):LongInt;
    begin
      self._pdu := pdu;
      self._npdu := 1;
      result := self.parsePdu(pdu);
      exit;
    end;


  function TYSms.set_parts(parts: TYSmsArray):LongInt;
    var
      sorted : TYSmsArray;
      partno : LongInt;
      initpartno : LongInt;
      i : LongInt;
      retcode : LongInt;
      totsize : LongInt;
      subsms : TYSms;
      subdata : TByteArray;
      res : TByteArray;
      sorted_pos : LongInt;
    begin
      self._npdu := length(parts);
      if self._npdu = 0 then
        begin
          result := YAPI_INVALID_ARGUMENT;
          exit;
        end;
      sorted_pos := 0;
      SetLength(sorted, self._npdu);;
      partno := 0;
      while partno < self._npdu do
        begin
          initpartno := partno;
          i := 0;
          while i < self._npdu do
            begin
              subsms := parts[i];
              if subsms.get_concatIndex = partno then
                begin
                  sorted[sorted_pos] := subsms;
                  inc(sorted_pos);
                  partno := partno + 1;
                end;
              i := i + 1;
            end;
          if initpartno = partno then
            begin
              partno := partno + 1;
            end;
        end;
      SetLength(sorted, sorted_pos);;
      self._parts := sorted;
      // inherit header fields from first part
      subsms := self._parts[0];
      retcode := self.parsePdu(subsms.get_pdu);
      if retcode <> YAPI_SUCCESS then
        begin
          result := retcode;
          exit;
        end;
      self._npdu := length(sorted);
      // concatenate user data from all parts
      totsize := 0;
      partno := 0;
      while partno < length(self._parts) do
        begin
          subsms := self._parts[partno];
          subdata := subsms.get_userData;
          totsize := totsize + length(subdata);
          partno := partno + 1;
        end;
      setlength(res,totsize);
      totsize := 0;
      partno := 0;
      while partno < length(self._parts) do
        begin
          subsms := self._parts[partno];
          subdata := subsms.get_userData;
          i := 0;
          while i < length(subdata) do
            begin
              res[totsize] := subdata[i];
              totsize := totsize + 1;
              i := i + 1;
            end;
          partno := partno + 1;
        end;
      self._udata := res;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.encodeAddress(addr: string):TByteArray;
    var
      bytes : TByteArray;
      srclen : LongInt;
      numlen : LongInt;
      i : LongInt;
      val : LongInt;
      digit : LongInt;
      res : TByteArray;
    begin
      bytes := _StrToByte(addr);
      srclen := length(bytes);
      numlen := 0;
      i := 0;
      while i < srclen do
        begin
          val := bytes[i];
          if (val >= 48) and(val < 58) then
            begin
              numlen := numlen + 1;
            end;
          i := i + 1;
        end;
      if numlen = 0 then
        begin
          setlength(res,1);
          res[0] := 0;
          result := res;
          exit;
        end;
      setlength(res,2+((numlen+1) shr 1));
      res[0] := numlen;
      if bytes[0] = 43 then
        begin
          res[1] := 145;
        end
      else
        begin
          res[1] := 129;
        end;
      numlen := 4;
      digit := 0;
      i := 0;
      while i < srclen do
        begin
          val := bytes[i];
          if (val >= 48) and(val < 58) then
            begin
              if ((numlen) and 1) = 0 then
                begin
                  digit := val - 48;
                end
              else
                begin
                  res[((numlen) shr 1)] := digit + 16*(val-48);
                end;
              numlen := numlen + 1;
            end;
          i := i + 1;
        end;
      // pad with F if needed
      if ((numlen) and 1) <> 0 then
        begin
          res[((numlen) shr 1)] := digit + 240;
        end;
      result := res;
      exit;
    end;


  function TYSms.decodeAddress(addr: TByteArray; ofs: LongInt; siz: LongInt):string;
    var
      addrType : LongInt;
      gsm7 : TByteArray;
      res : string;
      i : LongInt;
      rpos : LongInt;
      carry : LongInt;
      nbits : LongInt;
      byt : LongInt;
    begin
      if siz = 0 then
        begin
          result := '';
          exit;
        end;
      res := '';
      addrType := ((addr[ofs]) and 112);
      if addrType = 80 then
        begin
          // alphanumeric number
          siz := (4*siz div 7);
          setlength(gsm7,siz);
          rpos := 1;
          carry := 0;
          nbits := 0;
          i := 0;
          while i < siz do
            begin
              if nbits = 7 then
                begin
                  gsm7[i] := carry;
                  carry := 0;
                  nbits := 0;
                end
              else
                begin
                  byt := addr[ofs+rpos];
                  rpos := rpos + 1;
                  gsm7[i] := ((carry) or ((((((byt) shl (nbits)))) and 127)));
                  carry := ((byt) shr ((7 - nbits)));
                  nbits := nbits + 1;
                end;
              i := i + 1;
            end;
          result := self._mbox.gsm2str(gsm7);
          exit;
        end
      else
        begin
          // standard phone number
          if addrType = 16 then
            begin
              res := '+';
            end;
          siz := (((siz+1)) shr 1);
          i := 0;
          while i < siz do
            begin
              byt := addr[ofs+i+1];
              res := ''+ res+''+AnsiLowerCase(inttohex( ((byt) and 15),1))+''+AnsiLowerCase(inttohex(((byt) shr 4),1));
              i := i + 1;
            end;
          // remove padding digit if needed
          if ((addr[ofs+siz]) shr 4) = 15 then
            begin
              res := Copy(res,  0 + 1, Length(res)-1);
            end;
          result := res;
          exit;
        end;
    end;


  function TYSms.encodeTimeStamp(exp: string):TByteArray;
    var
      explen : LongInt;
      i : LongInt;
      res : TByteArray;
      n : LongInt;
      expasc : TByteArray;
      v1 : LongInt;
      v2 : LongInt;
    begin
      explen := Length(exp);
      if explen = 0 then
        begin
          setlength(res,0);
          result := res;
          exit;
        end;
      if (Copy(exp, 0 + 1, 1) = '+') then
        begin
          n := _atoi(Copy(exp, 1 + 1, explen-1));
          setlength(res,1);
          if n > 30*86400 then
            begin
              n := 192+((n+6*86400) div (7*86400));
            end
          else
            begin
              if n > 86400 then
                begin
                  n := 166+((n+86399) div 86400);
                end
              else
                begin
                  if n > 43200 then
                    begin
                      n := 143+((n-43200+1799) div 1800);
                    end
                  else
                    begin
                      n := -1+((n+299) div 300);
                    end;
                end;
            end;
          if n < 0 then
            begin
              n := 0;
            end;
          res[0] := n;
          result := res;
          exit;
        end;
      if (Copy(exp, 4 + 1, 1) = '-') or (Copy(exp, 4 + 1, 1) = '/') then
        begin
          // ignore century
          exp := Copy(exp,  2 + 1, explen-2);
          explen := Length(exp);
        end;
      expasc := _StrToByte(exp);
      setlength(res,7);
      n := 0;
      i := 0;
      while (i+1 < explen) and(n < 7) do
        begin
          v1 := expasc[i];
          if (v1 >= 48) and(v1 < 58) then
            begin
              v2 := expasc[i+1];
              if (v2 >= 48) and(v2 < 58) then
                begin
                  v1 := v1 - 48;
                  v2 := v2 - 48;
                  res[n] := (((v2) shl 4)) + v1;
                  n := n + 1;
                  i := i + 1;
                end;
            end;
          i := i + 1;
        end;
      while n < 7 do
        begin
          res[n] := 0;
          n := n + 1;
        end;
      if i+2 < explen then
        begin
          // convert for timezone in cleartext ISO format +/-nn:nn
          v1 := expasc[i-3];
          v2 := expasc[i];
          if ((v1 = 43) or(v1 = 45)) and(v2 = 58) then
            begin
              v1 := expasc[i+1];
              v2 := expasc[i+2];
              if (v1 >= 48) and(v1 < 58) and(v1 >= 48) and(v1 < 58) then
                begin
                  v1 := ((10*(v1 - 48)+(v2 - 48)) div 15);
                  n := n - 1;
                  v2 := 4 * res[n] + v1;
                  if expasc[i-3] = 45 then
                    begin
                      v2 := v2 + 128;
                    end;
                  res[n] := v2;
                end;
            end;
        end;
      result := res;
      exit;
    end;


  function TYSms.decodeTimeStamp(exp: TByteArray; ofs: LongInt; siz: LongInt):string;
    var
      n : LongInt;
      res : string;
      i : LongInt;
      byt : LongInt;
      sign : string;
      hh : string;
      ss : string;
    begin
      if siz < 1 then
        begin
          result := '';
          exit;
        end;
      if siz = 1 then
        begin
          n := exp[ofs];
          if n < 144 then
            begin
              n := n * 300;
            end
          else
            begin
              if n < 168 then
                begin
                  n := (n-143) * 1800;
                end
              else
                begin
                  if n < 197 then
                    begin
                      n := (n-166) * 86400;
                    end
                  else
                    begin
                      n := (n-192) * 7 * 86400;
                    end;
                end;
            end;
          result := '+'+inttostr(n);
          exit;
        end;
      res := '20';
      i := 0;
      while (i < siz) and(i < 6) do
        begin
          byt := exp[ofs+i];
          res := ''+ res+''+AnsiLowerCase(inttohex( ((byt) and 15),1))+''+AnsiLowerCase(inttohex(((byt) shr 4),1));
          if i < 3 then
            begin
              if i < 2 then
                begin
                  res := ''+res+'-';
                end
              else
                begin
                  res := ''+res+' ';
                end;
            end
          else
            begin
              if i < 5 then
                begin
                  res := ''+res+':';
                end;
            end;
          i := i + 1;
        end;
      if siz = 7 then
        begin
          byt := exp[ofs+i];
          sign := '+';
          if ((byt) and 8) <> 0 then
            begin
              byt := byt - 8;
              sign := '-';
            end;
          byt := (10*(((byt) and 15))) + (((byt) shr 4));
          hh := ''+inttostr(((byt) shr 2));
          ss := ''+inttostr(15*(((byt) and 3)));
          if Length(hh)<2 then
            begin
              hh := '0'+hh;
            end;
          if Length(ss)<2 then
            begin
              ss := '0'+ss;
            end;
          res := ''+ res+''+ sign+''+ hh+':'+ss;
        end;
      result := res;
      exit;
    end;


  function TYSms.udataSize():LongInt;
    var
      res : LongInt;
      udhsize : LongInt;
    begin
      udhsize := length(self._udh);
      res := length(self._udata);
      if self._alphab = 0 then
        begin
          if udhsize > 0 then
            begin
              res := res + ((8 + 8*udhsize + 6) div 7);
            end;
          res := ((res * 7 + 7) div 8);
        end
      else
        begin
          if udhsize > 0 then
            begin
              res := res + 1 + udhsize;
            end;
        end;
      result := res;
      exit;
    end;


  function TYSms.encodeUserData():TByteArray;
    var
      udsize : LongInt;
      udlen : LongInt;
      udhsize : LongInt;
      udhlen : LongInt;
      res : TByteArray;
      i : LongInt;
      wpos : LongInt;
      carry : LongInt;
      nbits : LongInt;
      thi_b : LongInt;
    begin
      udsize := self.udataSize;
      udhsize := length(self._udh);
      udlen := length(self._udata);
      setlength(res,1+udsize);
      udhlen := 0;
      nbits := 0;
      carry := 0;
      // 1. Encode UDL
      if self._alphab = 0 then
        begin
          // 7-bit encoding
          if udhsize > 0 then
            begin
              udhlen := ((8 + 8*udhsize + 6) div 7);
              nbits := 7*udhlen - 8 - 8*udhsize;
            end;
          res[0] := udhlen+udlen;
        end
      else
        begin
          // 8-bit encoding
          res[0] := udsize;
        end;
      // 2. Encode UDHL and UDL
      wpos := 1;
      if udhsize > 0 then
        begin
          res[wpos] := udhsize;
          wpos := wpos + 1;
          i := 0;
          while i < udhsize do
            begin
              res[wpos] := self._udh[i];
              wpos := wpos + 1;
              i := i + 1;
            end;
        end;
      // 3. Encode UD
      if self._alphab = 0 then
        begin
          // 7-bit encoding
          i := 0;
          while i < udlen do
            begin
              if nbits = 0 then
                begin
                  carry := self._udata[i];
                  nbits := 7;
                end
              else
                begin
                  thi_b := self._udata[i];
                  res[wpos] := ((carry) or ((((((thi_b) shl (nbits)))) and 255)));
                  wpos := wpos + 1;
                  nbits := nbits - 1;
                  carry := ((thi_b) shr ((7 - nbits)));
                end;
              i := i + 1;
            end;
          if nbits > 0 then
            begin
              res[wpos] := carry;
            end;
        end
      else
        begin
          // 8-bit encoding
          i := 0;
          while i < udlen do
            begin
              res[wpos] := self._udata[i];
              wpos := wpos + 1;
              i := i + 1;
            end;
        end;
      result := res;
      exit;
    end;


  function TYSms.generateParts():LongInt;
    var
      udhsize : LongInt;
      udlen : LongInt;
      mss : LongInt;
      partno : LongInt;
      partlen : LongInt;
      newud : TByteArray;
      newudh : TByteArray;
      newpdu : TYSms;
      i : LongInt;
      wpos : LongInt;
      parts_pos : LongInt;
    begin
      udhsize := length(self._udh);
      udlen := length(self._udata);
      mss := 140 - 1 - 5 - udhsize;
      if self._alphab = 0 then
        begin
          mss := ((mss * 8 - 6) div 7);
        end;
      self._npdu := ((udlen+mss-1) div mss);
      parts_pos := 0;
      SetLength(self._parts, self._npdu);;
      partno := 0;
      wpos := 0;
      while wpos < udlen do
        begin
          partno := partno + 1;
          setlength(newudh,5+udhsize);
          newudh[0] := 0;
          // IEI: concatenated message
          newudh[1] := 3;
          // IEDL: 3 bytes
          newudh[2] := self._mref;
          newudh[3] := self._npdu;
          newudh[4] := partno;
          i := 0;
          while i < udhsize do
            begin
              newudh[5+i] := self._udh[i];
              i := i + 1;
            end;
          if wpos+mss < udlen then
            begin
              partlen := mss;
            end
          else
            begin
              partlen := udlen-wpos;
            end;
          setlength(newud,partlen);
          i := 0;
          while i < partlen do
            begin
              newud[i] := self._udata[wpos];
              wpos := wpos + 1;
              i := i + 1;
            end;
          newpdu :=  TYSms.create(self._mbox);
          newpdu.set_received(self.isReceived);
          newpdu.set_smsc(self.get_smsc);
          newpdu.set_msgRef(self.get_msgRef);
          newpdu.set_sender(self.get_sender);
          newpdu.set_recipient(self.get_recipient);
          newpdu.set_protocolId(self.get_protocolId);
          newpdu.set_dcs(self.get_dcs);
          newpdu.set_timestamp(self.get_timestamp);
          newpdu.set_userDataHeader(newudh);
          newpdu.set_userData(newud);
          self._parts[parts_pos] := newpdu;
          inc(parts_pos);
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.generatePdu():LongInt;
    var
      sca : TByteArray;
      hdr : TByteArray;
      addr : TByteArray;
      stamp : TByteArray;
      udata : TByteArray;
      pdutyp : LongInt;
      pdulen : LongInt;
      i : LongInt;
    begin
      SetLength(self._parts, 0);
      if self.udataSize > 140 then
        begin
          // multiple PDU are needed
          setlength(self._pdu,0);
          result := self.generateParts;
          exit;
        end;
      sca := self.encodeAddress(self._smsc);
      if length(sca) > 0 then
        begin
          sca[0] := length(sca)-1;
        end;
      stamp := self.encodeTimeStamp(self._stamp);
      udata := self.encodeUserData;
      if self._deliv then
        begin
          addr := self.encodeAddress(self._orig);
          setlength(hdr,1);
          pdutyp := 0;
        end
      else
        begin
          addr := self.encodeAddress(self._dest);
          self._mref := self._mbox.nextMsgRef();
          setlength(hdr,2);
          hdr[1] := self._mref;
          pdutyp := 1;
          if length(stamp) > 0 then
            begin
              pdutyp := pdutyp + 16;
            end;
          if length(stamp) = 7 then
            begin
              pdutyp := pdutyp + 8;
            end;
        end;
      if length(self._udh) > 0 then
        begin
          pdutyp := pdutyp + 64;
        end;
      hdr[0] := pdutyp;
      pdulen := length(sca)+length(hdr)+length(addr)+2+length(stamp)+length(udata);
      setlength(self._pdu,pdulen);
      pdulen := 0;
      i := 0;
      while i < length(sca) do
        begin
          self._pdu[pdulen] := sca[i];
          pdulen := pdulen + 1;
          i := i + 1;
        end;
      i := 0;
      while i < length(hdr) do
        begin
          self._pdu[pdulen] := hdr[i];
          pdulen := pdulen + 1;
          i := i + 1;
        end;
      i := 0;
      while i < length(addr) do
        begin
          self._pdu[pdulen] := addr[i];
          pdulen := pdulen + 1;
          i := i + 1;
        end;
      self._pdu[pdulen] := self._pid;
      pdulen := pdulen + 1;
      self._pdu[pdulen] := self.get_dcs;
      pdulen := pdulen + 1;
      i := 0;
      while i < length(stamp) do
        begin
          self._pdu[pdulen] := stamp[i];
          pdulen := pdulen + 1;
          i := i + 1;
        end;
      i := 0;
      while i < length(udata) do
        begin
          self._pdu[pdulen] := udata[i];
          pdulen := pdulen + 1;
          i := i + 1;
        end;
      self._npdu := 1;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.parseUserDataHeader():LongInt;
    var
      udhlen : LongInt;
      i : LongInt;
      iei : LongInt;
      ielen : LongInt;
      sig : string;
    begin
      self._aggSig := '';
      self._aggIdx := 0;
      self._aggCnt := 0;
      udhlen := length(self._udh);
      i := 0;
      while i+1 < udhlen do
        begin
          iei := self._udh[i];
          ielen := self._udh[i+1];
          i := i + 2;
          if i + ielen <= udhlen then
            begin
              if (iei = 0) and(ielen = 3) then
                begin
                  // concatenated SMS, 8-bit ref
                  sig := ''+ self._orig+'-'+ self._dest+'-'+AnsiLowerCase(inttohex(
                  self._mref,02))+'-'+AnsiLowerCase(inttohex(self._udh[i],02));
                  self._aggSig := sig;
                  self._aggCnt := self._udh[i+1];
                  self._aggIdx := self._udh[i+2];
                end;
              if (iei = 8) and(ielen = 4) then
                begin
                  // concatenated SMS, 16-bit ref
                  sig := ''+ self._orig+'-'+ self._dest+'-'+AnsiLowerCase(inttohex(
                  self._mref,02))+'-'+AnsiLowerCase(inttohex( self._udh[i],02))+''+AnsiLowerCase(inttohex(self._udh[i+1],02));
                  self._aggSig := sig;
                  self._aggCnt := self._udh[i+2];
                  self._aggIdx := self._udh[i+3];
                end;
            end;
          i := i + ielen;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.parsePdu(pdu: TByteArray):LongInt;
    var
      rpos : LongInt;
      addrlen : LongInt;
      pdutyp : LongInt;
      tslen : LongInt;
      dcs : LongInt;
      udlen : LongInt;
      udhsize : LongInt;
      udhlen : LongInt;
      i : LongInt;
      carry : LongInt;
      nbits : LongInt;
      thi_b : LongInt;
    begin
      self._pdu := pdu;
      self._npdu := 1;
      // parse meta-data
      self._smsc := self.decodeAddress(pdu,  1, 2*(pdu[0]-1));
      rpos := 1+pdu[0];
      pdutyp := pdu[rpos];
      rpos := rpos + 1;
      self._deliv := (((pdutyp) and 3) = 0);
      if self._deliv then
        begin
          addrlen := pdu[rpos];
          rpos := rpos + 1;
          self._orig := self.decodeAddress(pdu,  rpos, addrlen);
          self._dest := '';
          tslen := 7;
        end
      else
        begin
          self._mref := pdu[rpos];
          rpos := rpos + 1;
          addrlen := pdu[rpos];
          rpos := rpos + 1;
          self._dest := self.decodeAddress(pdu,  rpos, addrlen);
          self._orig := '';
          if (((pdutyp) and 16)) <> 0 then
            begin
              if (((pdutyp) and 8)) <> 0 then
                begin
                  tslen := 7;
                end
              else
                begin
                  tslen:= 1;
                end;
            end
          else
            begin
              tslen := 0;
            end;
        end;
      rpos := rpos + ((((addrlen+3)) shr 1));
      self._pid := pdu[rpos];
      rpos := rpos + 1;
      dcs := pdu[rpos];
      rpos := rpos + 1;
      self._alphab := (((((dcs) shr 2))) and 3);
      self._mclass := ((dcs) and (16+3));
      self._stamp := self.decodeTimeStamp(pdu,  rpos, tslen);
      rpos := rpos + tslen;
      // parse user data (including udh)
      nbits := 0;
      carry := 0;
      udlen := pdu[rpos];
      rpos := rpos + 1;
      if ((pdutyp) and 64) <> 0 then
        begin
          udhsize := pdu[rpos];
          rpos := rpos + 1;
          setlength(self._udh,udhsize);
          i := 0;
          while i < udhsize do
            begin
              self._udh[i] := pdu[rpos];
              rpos := rpos + 1;
              i := i + 1;
            end;
          if self._alphab = 0 then
            begin
              // 7-bit encoding
              udhlen := ((8 + 8*udhsize + 6) div 7);
              nbits := 7*udhlen - 8 - 8*udhsize;
              if nbits > 0 then
                begin
                  thi_b := pdu[rpos];
                  rpos := rpos + 1;
                  carry := ((thi_b) shr (nbits));
                  nbits := 8 - nbits;
                end;
            end
          else
            begin
              // byte encoding
              udhlen := 1+udhsize;
            end;
          udlen := udlen - udhlen;
        end
      else
        begin
          udhsize := 0;
          setlength(self._udh,0);
        end;
      setlength(self._udata,udlen);
      if self._alphab = 0 then
        begin
          // 7-bit encoding
          i := 0;
          while i < udlen do
            begin
              if nbits = 7 then
                begin
                  self._udata[i] := carry;
                  carry := 0;
                  nbits := 0;
                end
              else
                begin
                  thi_b := pdu[rpos];
                  rpos := rpos + 1;
                  self._udata[i] := ((carry) or ((((((thi_b) shl (nbits)))) and 127)));
                  carry := ((thi_b) shr ((7 - nbits)));
                  nbits := nbits + 1;
                end;
              i := i + 1;
            end;
        end
      else
        begin
          // 8-bit encoding
          i := 0;
          while i < udlen do
            begin
              self._udata[i] := pdu[rpos];
              rpos := rpos + 1;
              i := i + 1;
            end;
        end;
      self.parseUserDataHeader;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSms.send():LongInt;
    var
      i : LongInt;
      retcode : LongInt;
      pdu : TYSms;
    begin
      if self._npdu = 0 then
        begin
          self.generatePdu;
        end;
      if self._npdu = 1 then
        begin
          result := self._mbox._upload('sendSMS', self._pdu);
          exit;
        end;
      retcode := YAPI_SUCCESS;
      i := 0;
      while (i < self._npdu) and(retcode = YAPI_SUCCESS) do
        begin
          pdu := self._parts[i];
          retcode:= pdu.send;
          i := i + 1;
        end;
      result := retcode;
      exit;
    end;


  function TYSms.deleteFromSIM():LongInt;
    var
      i : LongInt;
      retcode : LongInt;
      pdu : TYSms;
    begin
      if self._npdu < 2 then
        begin
          result := self._mbox.clearSIMSlot(self._slot);
          exit;
        end;
      retcode := YAPI_SUCCESS;
      i := 0;
      while (i < self._npdu) and(retcode = YAPI_SUCCESS) do
        begin
          pdu := self._parts[i];
          retcode:= pdu.deleteFromSIM;
          i := i + 1;
        end;
      result := retcode;
      exit;
    end;


//--- (end of generated code: YSms implementation)
{$HINTS ON}

//--- (generated code: YSms functions)

  procedure _SmsCleanup();
    begin
    end;

//--- (end of generated code: YSms functions)


initialization
  //--- (generated code: YMessageBox initialization)
  //--- (end of generated code: YMessageBox initialization)
  //--- (generated code: YSms initialization)
  //--- (end of generated code: YSms initialization)

finalization
  //--- (generated code: YMessageBox cleanup)
  _MessageBoxCleanup();
  //--- (end of generated code: YMessageBox cleanup)
  //--- (generated code: YSms cleanup)
  //--- (end of generated code: YSms cleanup)
end.
