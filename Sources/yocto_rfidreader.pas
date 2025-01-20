{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  Implements yFindRfidReader(), the high-level API for RfidReader functions
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


unit yocto_rfidreader;
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils, classes,
{$IFNDEF UNIX}
  windows,
{$ENDIF}
  yocto_api, yjson;

//--- (generated code: YRfidTagInfo definitions)

const Y_IEC_15693                     = 1;
const Y_IEC_14443                     = 2;
const Y_IEC_14443_MIFARE_ULTRALIGHT   = 3;
const Y_IEC_14443_MIFARE_CLASSIC1K    = 4;
const Y_IEC_14443_MIFARE_CLASSIC4K    = 5;
const Y_IEC_14443_MIFARE_DESFIRE      = 6;
const Y_IEC_14443_NTAG_213            = 7;
const Y_IEC_14443_NTAG_215            = 8;
const Y_IEC_14443_NTAG_216            = 9;
const Y_IEC_14443_NTAG_424_DNA        = 10;
//--- (end of generated code: YRfidTagInfo definitions)

type

  TYRfidTagInfo = class;
  //--- (generated code: YRfidTagInfo class start)
  ////
  /// <summary>
  ///   TYRfidTagInfo Class: RFID tag description, used by class <c>YRfidReader</c>
  /// <para>
  ///   <c>YRfidTagInfo</c> objects are used to describe RFID tag attributes,
  ///   such as the tag type and its storage size. These objects are returned by
  ///   method <c>get_tagInfo()</c> of class <c>YRfidReader</c>.
  /// </para>
  /// </summary>
  ///-
  TYRfidTagInfo=class(TObject)
  //--- (end of generated code: YRfidTagInfo class start)
  protected
    //--- (generated code: YRfidTagInfo declaration)
    // Attributes (function value cache)
    _tagId                    : string;
    _tagType                  : LongInt;
    _typeStr                  : string;
    _size                     : LongInt;
    _usable                   : LongInt;
    _blksize                  : LongInt;
    _fblk                     : LongInt;
    _lblk                     : LongInt;
    //--- (end of generated code: YRfidTagInfo declaration)

  public
    constructor Create();
    //--- (generated code: YRfidTagInfo accessors declaration)

    ////
    /// <summary>
    ///   Returns the RFID tag identifier.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with the RFID tag identifier.
    /// </returns>
    ///-
    function get_tagId():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the type of the RFID tag, as a numeric constant.
    /// <para>
    ///   (<c>IEC_14443_MIFARE_CLASSIC1K</c>, ...).
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the RFID tag type
    /// </returns>
    ///-
    function get_tagType():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the type of the RFID tag, as a string.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the RFID tag type
    /// </returns>
    ///-
    function get_tagTypeStr():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the total memory size of the RFID tag, in bytes.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the total memory size of the RFID tag
    /// </returns>
    ///-
    function get_tagMemorySize():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the usable storage size of the RFID tag, in bytes.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the usable storage size of the RFID tag
    /// </returns>
    ///-
    function get_tagUsableSize():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the block size of the RFID tag, in bytes.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the block size of the RFID tag
    /// </returns>
    ///-
    function get_tagBlockSize():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the index of the block available for data storage on the RFID tag.
    /// <para>
    ///   Some tags have special block used to configure the tag behavior, these
    ///   blocks must be handled with precaution. However, the  block return by
    ///   <c>get_tagFirstBlock()</c> can be locked, use <c>get_tagLockState()</c>
    ///   to find out  which block are locked.
    /// </para>
    /// </summary>
    /// <returns>
    ///   the index of the first usable storage block on the RFID tag
    /// </returns>
    ///-
    function get_tagFirstBlock():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the index of the last last black available for data storage on the RFID tag,
    ///   However, this block can be locked, use <c>get_tagLockState()</c> to find out
    ///   which block are locked.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the index of the last usable storage block on the RFID tag
    /// </returns>
    ///-
    function get_tagLastBlock():LongInt; overload; virtual;

    procedure imm_init(tagId: string; tagType: LongInt; size: LongInt; usable: LongInt; blksize: LongInt; fblk: LongInt; lblk: LongInt); overload; virtual;


  //--- (end of generated code: YRfidTagInfo accessors declaration)
  end;


//--- (generated code: YRfidOptions definitions)

const Y_NO_RFID_KEY                   = 0;
const Y_MIFARE_KEY_A                  = 1;
const Y_MIFARE_KEY_B                  = 2;
//--- (end of generated code: YRfidOptions definitions)

type

  TYRfidOptions = class;
  //--- (generated code: YRfidOptions class start)
  ////
  /// <summary>
  ///   TYRfidOptions Class: Additional parameters for operations on RFID tags.
  /// <para>
  /// </para>
  /// <para>
  ///   The <c>YRfidOptions</c> objects are used to specify additional
  ///   optional parameters to RFID commands that interact with tags,
  ///   including security keys. When instantiated,the parameters of
  ///   this object are pre-initialized to a value  which corresponds
  ///   to the most common usage.
  /// </para>
  /// </summary>
  ///-
  TYRfidOptions=class(TObject)
  //--- (end of generated code: YRfidOptions class start)
  public
    //--- (generated code: YRfidOptions declaration)
    // Attributes
    ////
    /// <summary>
    ///   Type of security key to be used to access the RFID tag.
    /// <para>
    ///   For MIFARE Classic tags, allowed values are
    ///   <c>Y_MIFARE_KEY_A</c> or <c>Y_MIFARE_KEY_B</c>.
    ///   The default value is <c>Y_NO_RFID_KEY</c>, in that case
    ///   the reader will use the most common default key for the
    ///   tag type.
    ///   When a security key is required, it must be provided
    ///   using property <c>HexKey</c>.
    /// </para>
    /// </summary>
    ///-
    KeyType                  : LongInt;
    ////
    /// <summary>
    ///   Security key to be used to access the RFID tag, as an
    ///   hexadecimal string.
    /// <para>
    ///   The key will only be used if you
    ///   also specify which type of key it is, using property
    ///   <c>KeyType</c>.
    /// </para>
    /// </summary>
    ///-
    HexKey                   : string;
    ////
    /// <summary>
    ///   Forces the use of single-block commands to access RFID tag memory blocks.
    /// <para>
    ///   By default, the Yoctopuce library uses the most efficient access strategy
    ///   generally available for each tag type, but you can force the use of
    ///   single-block commands if the RFID tags you are using do not support
    ///   multi-block commands. If operation speed is not a priority, choose
    ///   single-block mode as it will work with any mode.
    /// </para>
    /// </summary>
    ///-
    ForceSingleBlockAccess   : boolean;
    ////
    /// <summary>
    ///   Forces the use of multi-block commands to access RFID tag memory blocks.
    /// <para>
    ///   By default, the Yoctopuce library uses the most efficient access strategy
    ///   generally available for each tag type, but you can force the use of
    ///   multi-block commands if you know for sure that the RFID tags you are using
    ///   do support multi-block commands. Be  aware that even if a tag allows multi-block
    ///   operations, the maximum number of blocks that can be written or read at the same
    ///   time can be (very) limited. If the tag does not support multi-block mode
    ///   for the wanted operation, the option will be ignored.
    /// </para>
    /// </summary>
    ///-
    ForceMultiBlockAccess    : boolean;
    ////
    /// <summary>
    ///   Enables direct access to RFID tag control blocks.
    /// <para>
    ///   By default, Yoctopuce library read and write functions only work
    ///   on data blocks and automatically skip special blocks, as specific functions are provided
    ///   to configure security parameters found in control blocks.
    ///   If you need to access control blocks in your own way using
    ///   read/write functions, enable this option.  Use this option wisely,
    ///   as overwriting a special block migth very well irreversibly alter your
    ///   tag behavior.
    /// </para>
    /// </summary>
    ///-
    EnableRawAccess          : boolean;
    ////
    /// <summary>
    ///   Disables the tag memory overflow test.
    /// <para>
    ///   By default, the Yoctopuce
    ///   library's read/write functions detect overruns and do not run
    ///   commands that are likely to fail. If you nevertheless wish to
    ///   try to access more memory than the tag announces, you can try to use
    ///   this option.
    /// </para>
    /// </summary>
    ///-
    DisableBoundaryChecks    : boolean;
    ////
    /// <summary>
    ///   Enables simulation mode to check the affected block range as well
    ///   as access rights.
    /// <para>
    ///   When this option is active, the operation is
    ///   not fully applied to the RFID tag but the affected block range
    ///   is determined and the optional access key is tested on these blocks.
    ///   The access key rights are not tested though. This option applies to
    ///   write / configure operations only, it is ignored for read operations.
    /// </para>
    /// </summary>
    ///-
    EnableDryRun             : boolean;
    //--- (end of generated code: YRfidOptions declaration)

    constructor Create();
    //--- (generated code: YRfidOptions accessors declaration)

    function imm_getParams():string; overload; virtual;


  //--- (end of generated code: YRfidOptions accessors declaration)
  end;


//--- (generated code: YRfidStatus definitions)

const Y_SUCCESS                       = 0;
const Y_COMMAND_NOT_SUPPORTED         = 1;
const Y_COMMAND_NOT_RECOGNIZED        = 2;
const Y_COMMAND_OPTION_NOT_RECOGNIZED = 3;
const Y_COMMAND_CANNOT_BE_PROCESSED_IN_TIME = 4;
const Y_UNDOCUMENTED_ERROR            = 15;
const Y_BLOCK_NOT_AVAILABLE           = 16;
const Y_BLOCK_ALREADY_LOCKED          = 17;
const Y_BLOCK_LOCKED                  = 18;
const Y_BLOCK_NOT_SUCESSFULLY_PROGRAMMED = 19;
const Y_BLOCK_NOT_SUCESSFULLY_LOCKED  = 20;
const Y_BLOCK_IS_PROTECTED            = 21;
const Y_CRYPTOGRAPHIC_ERROR           = 64;
const Y_READER_BUSY                   = 1000;
const Y_TAG_NOTFOUND                  = 1001;
const Y_TAG_LEFT                      = 1002;
const Y_TAG_JUSTLEFT                  = 1003;
const Y_TAG_COMMUNICATION_ERROR       = 1004;
const Y_TAG_NOT_RESPONDING            = 1005;
const Y_TIMEOUT_ERROR                 = 1006;
const Y_COLLISION_DETECTED            = 1007;
const Y_INVALID_CMD_ARGUMENTS         = -66;
const Y_UNKNOWN_CAPABILITIES          = -67;
const Y_MEMORY_NOT_SUPPORTED          = -68;
const Y_INVALID_BLOCK_INDEX           = -69;
const Y_MEM_SPACE_UNVERRUN_ATTEMPT    = -70;
const Y_BROWNOUT_DETECTED             = -71     ;
const Y_BUFFER_OVERFLOW               = -72;
const Y_CRC_ERROR                     = -73;
const Y_COMMAND_RECEIVE_TIMEOUT       = -75;
const Y_DID_NOT_SLEEP                 = -76;
const Y_ERROR_DECIMAL_EXPECTED        = -77;
const Y_HARDWARE_FAILURE              = -78;
const Y_ERROR_HEX_EXPECTED            = -79;
const Y_FIFO_LENGTH_ERROR             = -80;
const Y_FRAMING_ERROR                 = -81;
const Y_NOT_IN_CNR_MODE               = -82;
const Y_NUMBER_OU_OF_RANGE            = -83;
const Y_NOT_SUPPORTED                 = -84;
const Y_NO_RF_FIELD_ACTIVE            = -85;
const Y_READ_DATA_LENGTH_ERROR        = -86;
const Y_WATCHDOG_RESET                = -87;
const Y_UNKNOW_COMMAND                = -91;
const Y_UNKNOW_ERROR                  = -92;
const Y_UNKNOW_PARAMETER              = -93;
const Y_UART_RECEIVE_ERROR            = -94;
const Y_WRONG_DATA_LENGTH             = -95;
const Y_WRONG_MODE                    = -96;
const Y_UNKNOWN_DWARFxx_ERROR_CODE    = -97;
const Y_RESPONSE_SHORT                = -98;
const Y_UNEXPECTED_TAG_ID_IN_RESPONSE = -99;
const Y_UNEXPECTED_TAG_INDEX          = -100;
const Y_READ_EOF                      = -101;
const Y_READ_OK_SOFAR                 = -102;
const Y_WRITE_DATA_MISSING            = -103;
const Y_WRITE_TOO_MUCH_DATA           = -104;
const Y_TRANSFER_CLOSED               = -105;
const Y_COULD_NOT_BUILD_REQUEST       = -106;
const Y_INVALID_OPTIONS               = -107;
const Y_UNEXPECTED_RESPONSE           = -108;
const Y_AFI_NOT_AVAILABLE             = -109;
const Y_DSFID_NOT_AVAILABLE           = -110;
const Y_TAG_RESPONSE_TOO_SHORT        = -111;
const Y_DEC_EXPECTED                  = -112 ;
const Y_HEX_EXPECTED                  = -113;
const Y_NOT_SAME_SECOR                = -114;
const Y_MIFARE_AUTHENTICATED          = -115;
const Y_NO_DATABLOCK                  = -116;
const Y_KEYB_IS_READABLE              = -117;
const Y_OPERATION_NOT_EXECUTED        = -118;
const Y_BLOK_MODE_ERROR               = -119;
const Y_BLOCK_NOT_WRITABLE            = -120;
const Y_BLOCK_ACCESS_ERROR            = -121;
const Y_BLOCK_NOT_AUTHENTICATED       = -122;
const Y_ACCESS_KEY_BIT_NOT_WRITABLE   = -123;
const Y_USE_KEYA_FOR_AUTH             = -124;
const Y_USE_KEYB_FOR_AUTH             = -125;
const Y_KEY_NOT_CHANGEABLE            = -126;
const Y_BLOCK_TOO_HIGH                = -127;
const Y_AUTH_ERR                      = -128;
const Y_NOKEY_SELECT                  = -129;
const Y_CARD_NOT_SELECTED             = -130;
const Y_BLOCK_TO_READ_NONE            = -131;
const Y_NO_TAG                        = -132;
const Y_TOO_MUCH_DATA                 = -133;
const Y_CON_NOT_SATISFIED             = -134;
const Y_BLOCK_IS_SPECIAL              = -135;
const Y_READ_BEYOND_ANNOUNCED_SIZE    = -136;
const Y_BLOCK_ZERO_IS_RESERVED        = -137;
const Y_VALUE_BLOCK_BAD_FORMAT        = -138;
const Y_ISO15693_ONLY_FEATURE         = -139;
const Y_ISO14443_ONLY_FEATURE         = -140;
const Y_MIFARE_CLASSIC_ONLY_FEATURE   = -141;
const Y_BLOCK_MIGHT_BE_PROTECTED      = -142;
const Y_NO_SUCH_BLOCK                 = -143;
const Y_COUNT_TOO_BIG                 = -144;
const Y_UNKNOWN_MEM_SIZE              = -145;
const Y_MORE_THAN_2BLOCKS_MIGHT_NOT_WORK = -146;
const Y_READWRITE_NOT_SUPPORTED       = -147;
const Y_UNEXPECTED_VICC_ID_IN_RESPONSE = -148;
const Y_LOCKBLOCK_NOT_SUPPORTED       = -150;
const Y_INTERNAL_ERROR_SHOULD_NEVER_HAPPEN = -151;
const Y_INVLD_BLOCK_MODE_COMBINATION  = -152;
const Y_INVLD_ACCESS_MODE_COMBINATION = -153;
const Y_INVALID_SIZE                  = -154;
const Y_BAD_PASSWORD_FORMAT           = -155;
const Y_RADIO_IS_OFF                  = -156;
//--- (end of generated code: YRfidStatus definitions)

type

  TYRfidStatus = class;
  //--- (generated code: YRfidStatus class start)
  ////
  /// <summary>
  ///   TYRfidStatus Class: Detailled information about the result of RFID tag operations, allowing to find out what happened exactly after a tag operation failure.
  /// <para>
  /// </para>
  /// <para>
  ///   <c>YRfidStatus</c> objects provide additional information about
  ///   operations on RFID tags, including the range of blocks affected
  ///   by read/write operations and possible errors when communicating
  ///   with RFID tags.
  ///   This makes it possible, for example, to distinguish communication
  ///   errors that can be recovered by an additional attempt, from
  ///   security or other errors on the tag.
  ///   Combined with the <c>EnableDryRun</c> option in <c>RfidOptions</c>,
  ///   this structure can be used to predict which blocks will be affected
  ///   by a write operation.
  /// </para>
  /// </summary>
  ///-
  TYRfidStatus=class(TObject)
  //--- (end of generated code: YRfidStatus class start)
  protected
    //--- (generated code: YRfidStatus declaration)
    // Attributes (function value cache)
    _tagId                    : string;
    _errCode                  : LongInt;
    _errBlk                   : LongInt;
    _errMsg                   : string;
    _yapierr                  : LongInt;
    _fab                      : LongInt;
    _lab                      : LongInt;
    //--- (end of generated code: YRfidStatus declaration)

  public
    constructor Create();
    //--- (generated code: YRfidStatus accessors declaration)

    ////
    /// <summary>
    ///   Returns RFID tag identifier related to the status.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with the RFID tag identifier.
    /// </returns>
    ///-
    function get_tagId():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the detailled error code, or 0 if no error happened.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a numeric error code
    /// </returns>
    ///-
    function get_errorCode():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the RFID tag memory block number where the error was encountered, or -1 if the
    ///   error is not specific to a memory block.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an RFID tag block number
    /// </returns>
    ///-
    function get_errorBlock():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a string describing precisely the RFID commande result.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an error message string
    /// </returns>
    ///-
    function get_errorMessage():string; overload; virtual;

    function get_yapiError():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the block number of the first RFID tag memory block affected
    ///   by the operation.
    /// <para>
    ///   Depending on the type of operation and on the tag
    ///   memory granularity, this number may be smaller than the requested
    ///   memory block index.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an RFID tag block number
    /// </returns>
    ///-
    function get_firstAffectedBlock():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the block number of the last RFID tag memory block affected
    ///   by the operation.
    /// <para>
    ///   Depending on the type of operation and on the tag
    ///   memory granularity, this number may be bigger than the requested
    ///   memory block index.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an RFID tag block number
    /// </returns>
    ///-
    function get_lastAffectedBlock():LongInt; overload; virtual;

    procedure imm_init(tagId: string; errCode: LongInt; errBlk: LongInt; fab: LongInt; lab: LongInt); overload; virtual;


  //--- (end of generated code: YRfidStatus accessors declaration)
  end;


//--- (generated code: YRfidReader definitions)

const Y_NTAGS_INVALID                 = YAPI_INVALID_UINT;
const Y_REFRESHRATE_INVALID           = YAPI_INVALID_UINT;

//--- (end of generated code: YRfidReader definitions)

//--- (generated code: YRfidReader yapiwrapper declaration)
//--- (end of generated code: YRfidReader yapiwrapper declaration)

type

  TYRfidReader = class;
  //--- (generated code: YRfidReader class start)
  TYRfidReaderValueCallback = procedure(func: TYRfidReader; value:string);
  TYRfidReaderTimedReportCallback = procedure(func: TYRfidReader; value:TYMeasure);
  TYEventCallback = procedure(func: TYRfidReader; stamp:double; evtType:string; evtData:string);

  ////
  /// <summary>
  ///   TYRfidReader Class: RfidReader function interface
  /// <para>
  ///   The <c>YRfidReader</c> class allows you to detect RFID tags, as well as
  ///   read and write on these tags if the security settings allow it.
  /// </para>
  /// <para>
  ///   Short reminder:
  /// </para>
  /// <para>
  /// </para>
  /// <para>
  ///   - A tag's memory is generally organized in fixed-size blocks.
  /// </para>
  /// <para>
  ///   - At tag level, each block must be read and written in its entirety.
  /// </para>
  /// <para>
  ///   - Some blocks are special configuration blocks, and may alter the tag's behavior
  ///   if they are rewritten with arbitrary data.
  /// </para>
  /// <para>
  ///   - Data blocks can be set to read-only mode, but on many tags, this operation is irreversible.
  /// </para>
  /// <para>
  /// </para>
  /// <para>
  ///   By default, the RfidReader class automatically manages these blocks so that
  ///   arbitrary size data  can be manipulated of  without risk and without knowledge of
  ///   tag architecture.
  /// </para>
  /// </summary>
  ///-
  TYRfidReader=class(TYFunction)
  //--- (end of generated code: YRfidReader class start)
  protected
    //--- (generated code: YRfidReader declaration)
    // Attributes (function value cache)
    _nTags                    : LongInt;
    _refreshRate              : LongInt;
    _valueCallbackRfidReader  : TYRfidReaderValueCallback;
    _eventCallback            : TYEventCallback;
    _isFirstCb                : boolean;
    _prevCbPos                : LongInt;
    _eventPos                 : LongInt;
    _eventStamp               : LongInt;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;
    //--- (end of generated code: YRfidReader declaration)

  public
    //--- (generated code: YRfidReader accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the number of RFID tags currently detected.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of RFID tags currently detected
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRfidReader.NTAGS_INVALID</c>.
    /// </para>
    ///-
    function get_nTags():LongInt;

    ////
    /// <summary>
    ///   Returns the tag list refresh rate, measured in Hz.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the tag list refresh rate, measured in Hz
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YRfidReader.REFRESHRATE_INVALID</c>.
    /// </para>
    ///-
    function get_refreshRate():LongInt;

    ////
    /// <summary>
    ///   Changes the present tag list refresh rate, measured in Hz.
    /// <para>
    ///   The reader will do
    ///   its best to respect it. Note that the reader cannot detect tag arrival or removal
    ///   while it is  communicating with a tag.  Maximum frequency is limited to 100Hz,
    ///   but in real life it will be difficult to do better than 50Hz.  A zero value
    ///   will power off the device radio.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the present tag list refresh rate, measured in Hz
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
    function set_refreshRate(newval:LongInt):integer;

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
    ///   Use the method <c>YRfidReader.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YRfidReader</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindRfidReader(func: string):TYRfidReader;

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
    function registerValueCallback(callback: TYRfidReaderValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function _chkerror(tagId: string; json: TByteArray; var status: TYRfidStatus):LongInt; overload; virtual;

    function reset():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the list of RFID tags currently detected by the reader.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list of strings, corresponding to each tag identifier (UID).
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty list.
    /// </para>
    ///-
    function get_tagIdList():TStringArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns a description of the properties of an existing RFID tag.
    /// <para>
    ///   This function can cause communications with the tag.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to check
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   a <c>YRfidTagInfo</c> object.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty <c>YRfidTagInfo</c> objact.
    ///   When it happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function get_tagInfo(tagId: string; var status: TYRfidStatus):TYRfidTagInfo; overload; virtual;

    ////
    /// <summary>
    ///   Changes an RFID tag configuration to prevents any further write to
    ///   the selected blocks.
    /// <para>
    ///   This operation is definitive and irreversible.
    ///   Depending on the tag type and block index, adjascent blocks may become
    ///   read-only as well, based on the locking granularity.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="firstBlock">
    ///   first block to lock
    /// </param>
    /// <param name="nBlocks">
    ///   number of blocks to lock
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagLockBlocks(tagId: string; firstBlock: LongInt; nBlocks: LongInt; options: TYRfidOptions; var status: TYRfidStatus):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reads the locked state for RFID tag memory data blocks.
    /// <para>
    ///   FirstBlock cannot be a special block, and any special
    ///   block encountered in the middle of the read operation will be
    ///   skipped automatically.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="firstBlock">
    ///   number of the first block to check
    /// </param>
    /// <param name="nBlocks">
    ///   number of blocks to check
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   a list of booleans with the lock state of selected blocks
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty list. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function get_tagLockState(tagId: string; firstBlock: LongInt; nBlocks: LongInt; options: TYRfidOptions; var status: TYRfidStatus):TBooleanArray; overload; virtual;

    ////
    /// <summary>
    ///   Tells which block of a RFID tag memory are special and cannot be used
    ///   to store user data.
    /// <para>
    ///   Mistakely writing a special block can lead to
    ///   an irreversible alteration of the tag.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="firstBlock">
    ///   number of the first block to check
    /// </param>
    /// <param name="nBlocks">
    ///   number of blocks to check
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   a list of booleans with the lock state of selected blocks
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty list. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function get_tagSpecialBlocks(tagId: string; firstBlock: LongInt; nBlocks: LongInt; options: TYRfidOptions; var status: TYRfidStatus):TBooleanArray; overload; virtual;

    ////
    /// <summary>
    ///   Reads data from an RFID tag memory, as an hexadecimal string.
    /// <para>
    ///   The read operation may span accross multiple blocks if the requested
    ///   number of bytes is larger than the RFID tag block size. By default
    ///   firstBlock cannot be a special block, and any special block encountered
    ///   in the middle of the read operation will be skipped automatically.
    ///   If you rather want to read special blocks, use the <c>EnableRawAccess</c>
    ///   field from the <c>options</c> parameter.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="firstBlock">
    ///   block number where read should start
    /// </param>
    /// <param name="nBytes">
    ///   total number of bytes to read
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   an hexadecimal string if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty binary buffer. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagReadHex(tagId: string; firstBlock: LongInt; nBytes: LongInt; options: TYRfidOptions; var status: TYRfidStatus):string; overload; virtual;

    ////
    /// <summary>
    ///   Reads data from an RFID tag memory, as a binary buffer.
    /// <para>
    ///   The read operation
    ///   may span accross multiple blocks if the requested number of bytes
    ///   is larger than the RFID tag block size.  By default
    ///   firstBlock cannot be a special block, and any special block encountered
    ///   in the middle of the read operation will be skipped automatically.
    ///   If you rather want to read special blocks, use the <c>EnableRawAccess</c>
    ///   field frrm the <c>options</c> parameter.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="firstBlock">
    ///   block number where read should start
    /// </param>
    /// <param name="nBytes">
    ///   total number of bytes to read
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   a binary object with the data read if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty binary buffer. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagReadBin(tagId: string; firstBlock: LongInt; nBytes: LongInt; options: TYRfidOptions; var status: TYRfidStatus):TByteArray; overload; virtual;

    ////
    /// <summary>
    ///   Reads data from an RFID tag memory, as a byte list.
    /// <para>
    ///   The read operation
    ///   may span accross multiple blocks if the requested number of bytes
    ///   is larger than the RFID tag block size.  By default
    ///   firstBlock cannot be a special block, and any special block encountered
    ///   in the middle of the read operation will be skipped automatically.
    ///   If you rather want to read special blocks, use the <c>EnableRawAccess</c>
    ///   field from the <c>options</c> parameter.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="firstBlock">
    ///   block number where read should start
    /// </param>
    /// <param name="nBytes">
    ///   total number of bytes to read
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   a byte list with the data read if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty list. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagReadArray(tagId: string; firstBlock: LongInt; nBytes: LongInt; options: TYRfidOptions; var status: TYRfidStatus):TLongIntArray; overload; virtual;

    ////
    /// <summary>
    ///   Reads data from an RFID tag memory, as a text string.
    /// <para>
    ///   The read operation
    ///   may span accross multiple blocks if the requested number of bytes
    ///   is larger than the RFID tag block size.  By default
    ///   firstBlock cannot be a special block, and any special block encountered
    ///   in the middle of the read operation will be skipped automatically.
    ///   If you rather want to read special blocks, use the <c>EnableRawAccess</c>
    ///   field from the <c>options</c> parameter.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="firstBlock">
    ///   block number where read should start
    /// </param>
    /// <param name="nChars">
    ///   total number of characters to read
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   a text string with the data read if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagReadStr(tagId: string; firstBlock: LongInt; nChars: LongInt; options: TYRfidOptions; var status: TYRfidStatus):string; overload; virtual;

    ////
    /// <summary>
    ///   Writes data provided as a binary buffer to an RFID tag memory.
    /// <para>
    ///   The write operation may span accross multiple blocks if the
    ///   number of bytes to write is larger than the RFID tag block size.
    ///   By default firstBlock cannot be a special block, and any special block
    ///   encountered in the middle of the write operation will be skipped
    ///   automatically. The last data block affected by the operation will
    ///   be automatically padded with zeros if neccessary.  If you rather want
    ///   to rewrite special blocks as well,
    ///   use the <c>EnableRawAccess</c> field from the <c>options</c> parameter.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="firstBlock">
    ///   block number where write should start
    /// </param>
    /// <param name="buff">
    ///   the binary buffer to write
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagWriteBin(tagId: string; firstBlock: LongInt; buff: TByteArray; options: TYRfidOptions; var status: TYRfidStatus):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Writes data provided as a list of bytes to an RFID tag memory.
    /// <para>
    ///   The write operation may span accross multiple blocks if the
    ///   number of bytes to write is larger than the RFID tag block size.
    ///   By default firstBlock cannot be a special block, and any special block
    ///   encountered in the middle of the write operation will be skipped
    ///   automatically. The last data block affected by the operation will
    ///   be automatically padded with zeros if neccessary.
    ///   If you rather want to rewrite special blocks as well,
    ///   use the <c>EnableRawAccess</c> field from the <c>options</c> parameter.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="firstBlock">
    ///   block number where write should start
    /// </param>
    /// <param name="byteList">
    ///   a list of byte to write
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagWriteArray(tagId: string; firstBlock: LongInt; byteList: TLongIntArray; options: TYRfidOptions; var status: TYRfidStatus):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Writes data provided as an hexadecimal string to an RFID tag memory.
    /// <para>
    ///   The write operation may span accross multiple blocks if the
    ///   number of bytes to write is larger than the RFID tag block size.
    ///   By default firstBlock cannot be a special block, and any special block
    ///   encountered in the middle of the write operation will be skipped
    ///   automatically. The last data block affected by the operation will
    ///   be automatically padded with zeros if neccessary.
    ///   If you rather want to rewrite special blocks as well,
    ///   use the <c>EnableRawAccess</c> field from the <c>options</c> parameter.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="firstBlock">
    ///   block number where write should start
    /// </param>
    /// <param name="hexString">
    ///   a string of hexadecimal byte codes to write
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagWriteHex(tagId: string; firstBlock: LongInt; hexString: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Writes data provided as an ASCII string to an RFID tag memory.
    /// <para>
    ///   The write operation may span accross multiple blocks if the
    ///   number of bytes to write is larger than the RFID tag block size.
    ///   Note that only the characters present in the provided string
    ///   will be written, there is no notion of string length. If your
    ///   string data have variable length, you'll have to encode the
    ///   string length yourself, with a terminal zero for instannce.
    /// </para>
    /// <para>
    ///   This function only works with ISO-latin characters, if you wish to
    ///   write strings encoded with alternate character sets, you'll have to
    ///   use tagWriteBin() function.
    /// </para>
    /// <para>
    ///   By default firstBlock cannot be a special block, and any special block
    ///   encountered in the middle of the write operation will be skipped
    ///   automatically. The last data block affected by the operation will
    ///   be automatically padded with zeros if neccessary.
    ///   If you rather want to rewrite special blocks as well,
    ///   use the <c>EnableRawAccess</c> field from the <c>options</c> parameter
    ///   (definitely not recommanded).
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="firstBlock">
    ///   block number where write should start
    /// </param>
    /// <param name="text">
    ///   the text string to write
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagWriteStr(tagId: string; firstBlock: LongInt; text: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reads an RFID tag AFI byte (ISO 15693 only).
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   the AFI value (0...255)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagGetAFI(tagId: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes an RFID tag AFI byte (ISO 15693 only).
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="afi">
    ///   the AFI value to write (0...255)
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagSetAFI(tagId: string; afi: LongInt; options: TYRfidOptions; var status: TYRfidStatus):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Locks the RFID tag AFI byte (ISO 15693 only).
    /// <para>
    ///   This operation is definitive and irreversible.
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagLockAFI(tagId: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reads an RFID tag DSFID byte (ISO 15693 only).
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   the DSFID value (0...255)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagGetDSFID(tagId: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Changes an RFID tag DSFID byte (ISO 15693 only).
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="dsfid">
    ///   the DSFID value to write (0...255)
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagSetDSFID(tagId: string; dsfid: LongInt; options: TYRfidOptions; var status: TYRfidStatus):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Locks the RFID tag DSFID byte (ISO 15693 only).
    /// <para>
    ///   This operation is definitive and irreversible.
    /// </para>
    /// </summary>
    /// <param name="tagId">
    ///   identifier of the tag to use
    /// </param>
    /// <param name="options">
    ///   an <c>YRfidOptions</c> object with the optional
    ///   command execution parameters, such as security key
    ///   if required
    /// </param>
    /// <param name="status">
    ///   an <c>RfidStatus</c> object that will contain
    ///   the detailled status of the operation
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code. When it
    ///   happens, you can get more information from the <c>status</c> object.
    /// </para>
    ///-
    function tagLockDSFID(tagId: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a string with last tag arrival/removal events observed.
    /// <para>
    ///   This method return only events that are still buffered in the device memory.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with last events observed (one per line).
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns  <c>YAPI_INVALID_STRING</c>.
    /// </para>
    ///-
    function get_lastEvents():string; overload; virtual;

    ////
    /// <summary>
    ///   Registers a callback function to be called each time that an RFID tag appears or
    ///   disappears.
    /// <para>
    ///   The callback is invoked only during the execution of
    ///   <c>ySleep</c> or <c>yHandleEvents</c>. This provides control over the time when
    ///   the callback is triggered. For good responsiveness, remember to call one of these
    ///   two functions periodically. To unregister a callback, pass a NIL pointer as argument.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="callback">
    ///   the callback function to call, or a NIL pointer.
    ///   The callback function should take four arguments:
    ///   the <c>YRfidReader</c> object that emitted the event, the
    ///   UTC timestamp of the event, a character string describing
    ///   the type of event ("+" or "-") and a character string with the
    ///   RFID tag identifier.
    ///   On failure, throws an exception or returns a negative error code.
    /// </param>
    ///-
    function registerEventCallback(callback: TYEventCallback):LongInt; overload; virtual;

    function _internalEventHandler(cbVal: string):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of RFID readers started using <c>yFirstRfidReader()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned RFID readers order.
    ///   If you want to find a specific a RFID reader, use <c>RfidReader.findRfidReader()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YRfidReader</c> object, corresponding to
    ///   a RFID reader currently online, or a <c>NIL</c> pointer
    ///   if there are no more RFID readers to enumerate.
    /// </returns>
    ///-
    function nextRfidReader():TYRfidReader;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstRfidReader():TYRfidReader;
  //--- (end of generated code: YRfidReader accessors declaration)
  end;

//--- (generated code: YRfidReader functions declaration)
  ////
  /// <summary>
  ///   Retrieves a RFID reader for a given identifier.
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
  ///   This function does not require that the RFID reader is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YRfidReader.isOnline()</c> to test if the RFID reader is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a RFID reader by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the RFID reader, for instance
  ///   <c>MyDevice.rfidReader</c>.
  /// </param>
  /// <returns>
  ///   a <c>YRfidReader</c> object allowing you to drive the RFID reader.
  /// </returns>
  ///-
  function yFindRfidReader(func:string):TYRfidReader;
  ////
  /// <summary>
  ///   Starts the enumeration of RFID readers currently accessible.
  /// <para>
  ///   Use the method <c>YRfidReader.nextRfidReader()</c> to iterate on
  ///   next RFID readers.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YRfidReader</c> object, corresponding to
  ///   the first RFID reader currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstRfidReader():TYRfidReader;

Procedure yInternalEventCallback(obj:TYRfidReader; value:string);

//--- (end of generated code: YRfidReader functions declaration)

implementation

//--- (generated code: YRfidReader dlldef)
//--- (end of generated code: YRfidReader dlldef)

  constructor TYRfidTagInfo.Create();
    begin
      //--- (generated code: YRfidTagInfo accessors initialization)
      _tagType := 0;
      _size := 0;
      _usable := 0;
      _blksize := 0;
      _fblk := 0;
      _lblk := 0;
      //--- (end of generated code: YRfidTagInfo accessors initialization)
    end;

//--- (generated code: YRfidTagInfo implementation)

  function TYRfidTagInfo.get_tagId():string;
    begin
      result := self._tagId;
      exit;
    end;


  function TYRfidTagInfo.get_tagType():LongInt;
    begin
      result := self._tagType;
      exit;
    end;


  function TYRfidTagInfo.get_tagTypeStr():string;
    begin
      result := self._typeStr;
      exit;
    end;


  function TYRfidTagInfo.get_tagMemorySize():LongInt;
    begin
      result := self._size;
      exit;
    end;


  function TYRfidTagInfo.get_tagUsableSize():LongInt;
    begin
      result := self._usable;
      exit;
    end;


  function TYRfidTagInfo.get_tagBlockSize():LongInt;
    begin
      result := self._blksize;
      exit;
    end;


  function TYRfidTagInfo.get_tagFirstBlock():LongInt;
    begin
      result := self._fblk;
      exit;
    end;


  function TYRfidTagInfo.get_tagLastBlock():LongInt;
    begin
      result := self._lblk;
      exit;
    end;


  procedure TYRfidTagInfo.imm_init(tagId: string; tagType: LongInt; size: LongInt; usable: LongInt; blksize: LongInt; fblk: LongInt; lblk: LongInt);
    var
      typeStr : string;
    begin
      typeStr := 'unknown';
      if tagType = Y_IEC_15693 then
        begin
          typeStr := 'IEC 15693';
        end;
      if tagType = Y_IEC_14443 then
        begin
          typeStr := 'IEC 14443';
        end;
      if tagType = Y_IEC_14443_MIFARE_ULTRALIGHT then
        begin
          typeStr := 'MIFARE Ultralight';
        end;
      if tagType = Y_IEC_14443_MIFARE_CLASSIC1K then
        begin
          typeStr := 'MIFARE Classic 1K';
        end;
      if tagType = Y_IEC_14443_MIFARE_CLASSIC4K then
        begin
          typeStr := 'MIFARE Classic 4K';
        end;
      if tagType = Y_IEC_14443_MIFARE_DESFIRE then
        begin
          typeStr := 'MIFARE DESFire';
        end;
      if tagType = Y_IEC_14443_NTAG_213 then
        begin
          typeStr := 'NTAG 213';
        end;
      if tagType = Y_IEC_14443_NTAG_215 then
        begin
          typeStr := 'NTAG 215';
        end;
      if tagType = Y_IEC_14443_NTAG_216 then
        begin
          typeStr := 'NTAG 216';
        end;
      if tagType = Y_IEC_14443_NTAG_424_DNA then
        begin
          typeStr := 'NTAG 424 DNA';
        end;
      self._tagId := tagId;
      self._tagType := tagType;
      self._typeStr := typeStr;
      self._size := size;
      self._usable := usable;
      self._blksize := blksize;
      self._fblk := fblk;
      self._lblk := lblk;
    end;


//--- (end of generated code: YRfidTagInfo implementation)


  constructor TYRfidOptions.Create();
    begin
      //--- (generated code: YRfidOptions accessors initialization)
      KeyType := 0;
      //--- (end of generated code: YRfidOptions accessors initialization)
    end;

//--- (generated code: YRfidOptions implementation)

  function TYRfidOptions.imm_getParams():string;
    var
      opt : LongInt;
      res : string;
    begin
      if self.ForceSingleBlockAccess then
        begin
          opt := 1;
        end
      else
        begin
          opt := 0;
        end;
      if self.ForceMultiBlockAccess then
        begin
          opt := ((opt) or 2);
        end;
      if self.EnableRawAccess then
        begin
          opt := ((opt) or 4);
        end;
      if self.DisableBoundaryChecks then
        begin
          opt := ((opt) or 8);
        end;
      if self.EnableDryRun then
        begin
          opt := ((opt) or 16);
        end;
      res := '&o='+inttostr(opt);
      if self.KeyType <> 0 then
        begin
          res := ''+res+'&k='+AnsiLowerCase(inttohex(self.KeyType,02))+':'+self.HexKey;
        end;
      result := res;
      exit;
    end;


//--- (end of generated code: YRfidOptions implementation)


  constructor TYRfidStatus.Create();
    begin
      //--- (generated code: YRfidStatus accessors initialization)
      _errCode := 0;
      _errBlk := 0;
      _yapierr := 0;
      _fab := 0;
      _lab := 0;
      //--- (end of generated code: YRfidStatus accessors initialization)
    end;

//--- (generated code: YRfidStatus implementation)

  function TYRfidStatus.get_tagId():string;
    begin
      result := self._tagId;
      exit;
    end;


  function TYRfidStatus.get_errorCode():LongInt;
    begin
      result := self._errCode;
      exit;
    end;


  function TYRfidStatus.get_errorBlock():LongInt;
    begin
      result := self._errBlk;
      exit;
    end;


  function TYRfidStatus.get_errorMessage():string;
    begin
      result := self._errMsg;
      exit;
    end;


  function TYRfidStatus.get_yapiError():LongInt;
    begin
      result := self._yapierr;
      exit;
    end;


  function TYRfidStatus.get_firstAffectedBlock():LongInt;
    begin
      result := self._fab;
      exit;
    end;


  function TYRfidStatus.get_lastAffectedBlock():LongInt;
    begin
      result := self._lab;
      exit;
    end;


  procedure TYRfidStatus.imm_init(tagId: string; errCode: LongInt; errBlk: LongInt; fab: LongInt; lab: LongInt);
    var
      errMsg : string;
    begin
      if errCode = 0 then
        begin
          self._yapierr := YAPI_SUCCESS;
          errMsg := 'Success (no error)';
        end
      else
        begin
          if errCode < 0 then
            begin
              if errCode > -50 then
                begin
                  self._yapierr := errCode;
                  errMsg := 'YoctoLib error '+inttostr(errCode);
                end
              else
                begin
                  self._yapierr := YAPI_RFID_HARD_ERROR;
                  errMsg := 'Non-recoverable RFID error '+inttostr(errCode);
                end;
            end
          else
            begin
              if errCode > 1000 then
                begin
                  self._yapierr := YAPI_RFID_SOFT_ERROR;
                  errMsg := 'Recoverable RFID error '+inttostr(errCode);
                end
              else
                begin
                  self._yapierr := YAPI_RFID_HARD_ERROR;
                  errMsg := 'Non-recoverable RFID error '+inttostr(errCode);
                end;
            end;
          if errCode = Y_TAG_NOTFOUND then
            begin
              errMsg := 'Tag not found';
            end;
          if errCode = Y_TAG_JUSTLEFT then
            begin
              errMsg := 'Tag left during operation';
            end;
          if errCode = Y_TAG_LEFT then
            begin
              errMsg := 'Tag not here anymore';
            end;
          if errCode = Y_READER_BUSY then
            begin
              errMsg := 'Reader is busy';
            end;
          if errCode = Y_INVALID_CMD_ARGUMENTS then
            begin
              errMsg := 'Invalid command arguments';
            end;
          if errCode = Y_UNKNOWN_CAPABILITIES then
            begin
              errMsg := 'Unknown capabilities';
            end;
          if errCode = Y_MEMORY_NOT_SUPPORTED then
            begin
              errMsg := 'Memory no present';
            end;
          if errCode = Y_INVALID_BLOCK_INDEX then
            begin
              errMsg := 'Invalid block index';
            end;
          if errCode = Y_MEM_SPACE_UNVERRUN_ATTEMPT then
            begin
              errMsg := 'Tag memory space overrun attempt';
            end;
          if errCode = Y_COMMAND_NOT_SUPPORTED then
            begin
              errMsg := 'The command is not supported';
            end;
          if errCode = Y_COMMAND_NOT_RECOGNIZED then
            begin
              errMsg := 'The command is not recognized';
            end;
          if errCode = Y_COMMAND_OPTION_NOT_RECOGNIZED then
            begin
              errMsg := 'The command option is not supported.';
            end;
          if errCode = Y_COMMAND_CANNOT_BE_PROCESSED_IN_TIME then
            begin
              errMsg := 'The command cannot be processed in time';
            end;
          if errCode = Y_UNDOCUMENTED_ERROR then
            begin
              errMsg := 'Error with no information given';
            end;
          if errCode = Y_BLOCK_NOT_AVAILABLE then
            begin
              errMsg := 'Block is not available';
            end;
          if errCode = Y_BLOCK_ALREADY_LOCKED then
            begin
              errMsg := 'Block / byte is already locked and thus cannot be locked again.'
              + '';
            end;
          if errCode = Y_BLOCK_LOCKED then
            begin
              errMsg := 'Block / byte is locked and its content cannot be changed';
            end;
          if errCode = Y_BLOCK_NOT_SUCESSFULLY_PROGRAMMED then
            begin
              errMsg := 'Block was not successfully programmed';
            end;
          if errCode = Y_BLOCK_NOT_SUCESSFULLY_LOCKED then
            begin
              errMsg := 'Block was not successfully locked';
            end;
          if errCode = Y_BLOCK_IS_PROTECTED then
            begin
              errMsg := 'Block is protected';
            end;
          if errCode = Y_CRYPTOGRAPHIC_ERROR then
            begin
              errMsg := 'Generic cryptographic error';
            end;
          if errCode = Y_BROWNOUT_DETECTED then
            begin
              errMsg := 'BrownOut detected (BOD)';
            end;
          if errCode = Y_BUFFER_OVERFLOW then
            begin
              errMsg := 'Buffer Overflow (BOF)';
            end;
          if errCode = Y_CRC_ERROR then
            begin
              errMsg := 'Communication CRC Error (CCE)';
            end;
          if errCode = Y_COLLISION_DETECTED then
            begin
              errMsg := 'Collision Detected (CLD/CDT)';
            end;
          if errCode = Y_COMMAND_RECEIVE_TIMEOUT then
            begin
              errMsg := 'Command Receive Timeout (CRT)';
            end;
          if errCode = Y_DID_NOT_SLEEP then
            begin
              errMsg := 'Did Not Sleep (DNS)';
            end;
          if errCode = Y_ERROR_DECIMAL_EXPECTED then
            begin
              errMsg := 'Error Decimal Expected (EDX)';
            end;
          if errCode = Y_HARDWARE_FAILURE then
            begin
              errMsg := 'Error Hardware Failure (EHF)';
            end;
          if errCode = Y_ERROR_HEX_EXPECTED then
            begin
              errMsg := 'Error Hex Expected (EHX)';
            end;
          if errCode = Y_FIFO_LENGTH_ERROR then
            begin
              errMsg := 'FIFO length error (FLE)';
            end;
          if errCode = Y_FRAMING_ERROR then
            begin
              errMsg := 'Framing error (FER)';
            end;
          if errCode = Y_NOT_IN_CNR_MODE then
            begin
              errMsg := 'Not in CNR Mode (NCM)';
            end;
          if errCode = Y_NUMBER_OU_OF_RANGE then
            begin
              errMsg := 'Number Out of Range (NOR)';
            end;
          if errCode = Y_NOT_SUPPORTED then
            begin
              errMsg := 'Not Supported (NOS)';
            end;
          if errCode = Y_NO_RF_FIELD_ACTIVE then
            begin
              errMsg := 'No RF field active (NRF)';
            end;
          if errCode = Y_READ_DATA_LENGTH_ERROR then
            begin
              errMsg := 'Read data length error (RDL)';
            end;
          if errCode = Y_WATCHDOG_RESET then
            begin
              errMsg := 'Watchdog reset (SRT)';
            end;
          if errCode = Y_TAG_COMMUNICATION_ERROR then
            begin
              errMsg := 'Tag Communication Error (TCE)';
            end;
          if errCode = Y_TAG_NOT_RESPONDING then
            begin
              errMsg := 'Tag Not Responding (TNR)';
            end;
          if errCode = Y_TIMEOUT_ERROR then
            begin
              errMsg := 'TimeOut Error (TOE)';
            end;
          if errCode = Y_UNKNOW_COMMAND then
            begin
              errMsg := 'Unknown Command (UCO)';
            end;
          if errCode = Y_UNKNOW_ERROR then
            begin
              errMsg := 'Unknown error (UER)';
            end;
          if errCode = Y_UNKNOW_PARAMETER then
            begin
              errMsg := 'Unknown Parameter (UPA)';
            end;
          if errCode = Y_UART_RECEIVE_ERROR then
            begin
              errMsg := 'UART Receive Error (URE)';
            end;
          if errCode = Y_WRONG_DATA_LENGTH then
            begin
              errMsg := 'Wrong Data Length (WDL)';
            end;
          if errCode = Y_WRONG_MODE then
            begin
              errMsg := 'Wrong Mode (WMO)';
            end;
          if errCode = Y_UNKNOWN_DWARFxx_ERROR_CODE then
            begin
              errMsg := 'Unknown DWARF15 error code';
            end;
          if errCode = Y_UNEXPECTED_TAG_ID_IN_RESPONSE then
            begin
              errMsg := 'Unexpected Tag id in response';
            end;
          if errCode = Y_UNEXPECTED_TAG_INDEX then
            begin
              errMsg := 'internal error : unexpected TAG index';
            end;
          if errCode = Y_TRANSFER_CLOSED then
            begin
              errMsg := 'transfer closed';
            end;
          if errCode = Y_WRITE_DATA_MISSING then
            begin
              errMsg := 'Missing write data';
            end;
          if errCode = Y_WRITE_TOO_MUCH_DATA then
            begin
              errMsg := 'Attempt to write too much data';
            end;
          if errCode = Y_COULD_NOT_BUILD_REQUEST then
            begin
              errMsg := 'Could not not request';
            end;
          if errCode = Y_INVALID_OPTIONS then
            begin
              errMsg := 'Invalid transfer options';
            end;
          if errCode = Y_UNEXPECTED_RESPONSE then
            begin
              errMsg := 'Unexpected Tag response';
            end;
          if errCode = Y_AFI_NOT_AVAILABLE then
            begin
              errMsg := 'AFI not available';
            end;
          if errCode = Y_DSFID_NOT_AVAILABLE then
            begin
              errMsg := 'DSFID not available';
            end;
          if errCode = Y_TAG_RESPONSE_TOO_SHORT then
            begin
              errMsg := 'Tag''s response too short';
            end;
          if errCode = Y_DEC_EXPECTED then
            begin
              errMsg := 'Error Decimal value Expected, or is missing';
            end;
          if errCode = Y_HEX_EXPECTED then
            begin
              errMsg := 'Error Hexadecimal value Expected, or is missing';
            end;
          if errCode = Y_NOT_SAME_SECOR then
            begin
              errMsg := 'Input and Output block are not in the same Sector';
            end;
          if errCode = Y_MIFARE_AUTHENTICATED then
            begin
              errMsg := 'No chip with MIFARE Classic technology Authenticated';
            end;
          if errCode = Y_NO_DATABLOCK then
            begin
              errMsg := 'No Data Block';
            end;
          if errCode = Y_KEYB_IS_READABLE then
            begin
              errMsg := 'Key B is Readable';
            end;
          if errCode = Y_OPERATION_NOT_EXECUTED then
            begin
              errMsg := 'Operation Not Executed, would have caused an overflow';
            end;
          if errCode = Y_BLOK_MODE_ERROR then
            begin
              errMsg := 'Block has not been initialized as a ''value block''';
            end;
          if errCode = Y_BLOCK_NOT_WRITABLE then
            begin
              errMsg := 'Block Not Writable';
            end;
          if errCode = Y_BLOCK_ACCESS_ERROR then
            begin
              errMsg := 'Block Access Error';
            end;
          if errCode = Y_BLOCK_NOT_AUTHENTICATED then
            begin
              errMsg := 'Block Not Authenticated';
            end;
          if errCode = Y_ACCESS_KEY_BIT_NOT_WRITABLE then
            begin
              errMsg := 'Access bits or Keys not Writable';
            end;
          if errCode = Y_USE_KEYA_FOR_AUTH then
            begin
              errMsg := 'Use Key B for authentication';
            end;
          if errCode = Y_USE_KEYB_FOR_AUTH then
            begin
              errMsg := 'Use Key A for authentication';
            end;
          if errCode = Y_KEY_NOT_CHANGEABLE then
            begin
              errMsg := 'Key(s) not changeable';
            end;
          if errCode = Y_BLOCK_TOO_HIGH then
            begin
              errMsg := 'Block index is too high';
            end;
          if errCode = Y_AUTH_ERR then
            begin
              errMsg := 'Authentication Error (i.e. wrong key)';
            end;
          if errCode = Y_NOKEY_SELECT then
            begin
              errMsg := 'No Key Select, select a temporary or a static key';
            end;
          if errCode = Y_CARD_NOT_SELECTED then
            begin
              errMsg := ' Card is Not Selected';
            end;
          if errCode = Y_BLOCK_TO_READ_NONE then
            begin
              errMsg := 'Number of Blocks to Read is 0';
            end;
          if errCode = Y_NO_TAG then
            begin
              errMsg := 'No Tag detected';
            end;
          if errCode = Y_TOO_MUCH_DATA then
            begin
              errMsg := 'Too Much Data (i.e. Uart input buffer overflow)';
            end;
          if errCode = Y_CON_NOT_SATISFIED then
            begin
              errMsg := 'Conditions Not Satisfied';
            end;
          if errCode = Y_BLOCK_IS_SPECIAL then
            begin
              errMsg := 'Bad parameter: block is a special block';
            end;
          if errCode = Y_READ_BEYOND_ANNOUNCED_SIZE then
            begin
              errMsg := 'Attempt to read more than announced size.';
            end;
          if errCode = Y_BLOCK_ZERO_IS_RESERVED then
            begin
              errMsg := 'Block 0 is reserved and cannot be used';
            end;
          if errCode = Y_VALUE_BLOCK_BAD_FORMAT then
            begin
              errMsg := 'One value block is not properly initialized';
            end;
          if errCode = Y_ISO15693_ONLY_FEATURE then
            begin
              errMsg := 'Feature available on ISO 15693 only';
            end;
          if errCode = Y_ISO14443_ONLY_FEATURE then
            begin
              errMsg := 'Feature available on ISO 14443 only';
            end;
          if errCode = Y_MIFARE_CLASSIC_ONLY_FEATURE then
            begin
              errMsg := 'Feature available on ISO 14443 MIFARE Classic only';
            end;
          if errCode = Y_BLOCK_MIGHT_BE_PROTECTED then
            begin
              errMsg := 'Block might be protected';
            end;
          if errCode = Y_NO_SUCH_BLOCK then
            begin
              errMsg := 'No such block';
            end;
          if errCode = Y_COUNT_TOO_BIG then
            begin
              errMsg := 'Count parameter is too large';
            end;
          if errCode = Y_UNKNOWN_MEM_SIZE then
            begin
              errMsg := 'Tag memory size is unknown';
            end;
          if errCode = Y_MORE_THAN_2BLOCKS_MIGHT_NOT_WORK then
            begin
              errMsg := 'Writing more than two blocks at once might not be supported by '
              + 'this tag';
            end;
          if errCode = Y_READWRITE_NOT_SUPPORTED then
            begin
              errMsg := 'Read/write operation not supported for this tag';
            end;
          if errCode = Y_UNEXPECTED_VICC_ID_IN_RESPONSE then
            begin
              errMsg := 'Unexpected VICC ID in response';
            end;
          if errCode = Y_LOCKBLOCK_NOT_SUPPORTED then
            begin
              errMsg := 'This tag does not support the Lock block function';
            end;
          if errCode = Y_INTERNAL_ERROR_SHOULD_NEVER_HAPPEN then
            begin
              errMsg := 'Yoctopuce RFID code ran into an unexpected state, please contac'
              + 't support';
            end;
          if errCode = Y_INVLD_BLOCK_MODE_COMBINATION then
            begin
              errMsg := 'Invalid combination of block mode options';
            end;
          if errCode = Y_INVLD_ACCESS_MODE_COMBINATION then
            begin
              errMsg := 'Invalid combination of access mode options';
            end;
          if errCode = Y_INVALID_SIZE then
            begin
              errMsg := 'Invalid data size parameter';
            end;
          if errCode = Y_BAD_PASSWORD_FORMAT then
            begin
              errMsg := 'Bad password format or type';
            end;
          if errCode = Y_RADIO_IS_OFF then
            begin
              errMsg := 'Radio is OFF (refreshRate=0).';
            end;
          if errBlk >= 0 then
            begin
              errMsg := ''+errMsg+' (block '+inttostr(errBlk)+')';
            end;
        end;
      self._tagId := tagId;
      self._errCode := errCode;
      self._errBlk := errBlk;
      self._errMsg := errMsg;
      self._fab := fab;
      self._lab := lab;
    end;


//--- (end of generated code: YRfidStatus implementation)


  constructor TYRfidReader.Create(func:string);
    begin
      inherited Create(func);
      _className := 'RfidReader';
      //--- (generated code: YRfidReader accessors initialization)
      _nTags := Y_NTAGS_INVALID;
      _refreshRate := Y_REFRESHRATE_INVALID;
      _valueCallbackRfidReader := nil;
      _prevCbPos := 0;
      _eventPos := 0;
      _eventStamp := 0;
      //--- (end of generated code: YRfidReader accessors initialization)
    end;

//--- (generated code: YRfidReader yapiwrapper)
//--- (end of generated code: YRfidReader yapiwrapper)

//--- (generated code: YRfidReader implementation)
{$HINTS OFF}
  function TYRfidReader._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'nTags') then
        begin
          _nTags := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'refreshRate') then
        begin
          _refreshRate := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYRfidReader.get_nTags():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_NTAGS_INVALID;
              exit;
            end;
        end;
      res := self._nTags;
      result := res;
      exit;
    end;


  function TYRfidReader.get_refreshRate():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_REFRESHRATE_INVALID;
              exit;
            end;
        end;
      res := self._refreshRate;
      result := res;
      exit;
    end;


  function TYRfidReader.set_refreshRate(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('refreshRate',rest_val);
    end;

  class function TYRfidReader.FindRfidReader(func: string):TYRfidReader;
    var
      obj : TYRfidReader;
    begin
      obj := TYRfidReader(TYFunction._FindFromCache('RfidReader', func));
      if obj = nil then
        begin
          obj :=  TYRfidReader.create(func);
          TYFunction._AddToCache('RfidReader', func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYRfidReader.registerValueCallback(callback: TYRfidReaderValueCallback):LongInt;
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
      self._valueCallbackRfidReader := callback;
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


  function TYRfidReader._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackRfidReader) <> nil) then
        begin
          self._valueCallbackRfidReader(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYRfidReader._chkerror(tagId: string; json: TByteArray; var status: TYRfidStatus):LongInt;
    var
      jsonStr : string;
      errCode : LongInt;
      errBlk : LongInt;
      fab : LongInt;
      lab : LongInt;
      retcode : LongInt;
    begin
      if length(json) = 0 then
        begin
          errCode := self.get_errorType;
          errBlk := -1;
          fab := -1;
          lab := -1;
        end
      else
        begin
          jsonStr := _ByteToString(json);
          errCode := _atoi(self._json_get_key(json, 'err'));
          errBlk := _atoi(self._json_get_key(json, 'errBlk'))-1;
          if (pos('"fab":', jsonStr) - 1) >= 0 then
            begin
              fab := _atoi(self._json_get_key(json, 'fab'))-1;
              lab := _atoi(self._json_get_key(json, 'lab'))-1;
            end
          else
            begin
              fab := -1;
              lab := -1;
            end;
        end;
      status.imm_init(tagId, errCode, errBlk, fab, lab);
      retcode := status.get_yapiError;
      if not(retcode = YAPI_SUCCESS) then
        begin
          self._throw(retcode,status.get_errorMessage);
          result:=retcode;
          exit;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYRfidReader.reset():LongInt;
    var
      json : TByteArray;
      status : TYRfidStatus;
    begin
      status :=  TYRfidStatus.create();

      json := self._download('rfid.json?a=reset');
      result := self._chkerror('', json, status);
      exit;
    end;


  function TYRfidReader.get_tagIdList():TStringArray;
    var
      json : TByteArray;
      jsonList : TStringArray;
      taglist : TStringArray;
      taglist_pos : LongInt;
      ii_0 : LongInt;
    begin
      SetLength(jsonList, 0);
      SetLength(taglist, 0);

      json := self._download('rfid.json?a=list');
      SetLength(taglist, 0);
      if length(json) > 3 then
        begin
          jsonList := self._json_get_array(json);
          taglist_pos := length(taglist);
          SetLength(taglist, taglist_pos+length(jsonList));
          for ii_0:=0 to length(jsonList)-1 do
            begin
              taglist[taglist_pos] := self._json_get_string(_StrToByte(jsonList[ii_0]));
              inc(taglist_pos);
            end;
          SetLength(taglist, taglist_pos);
        end;
      result := taglist;
      exit;
    end;


  function TYRfidReader.get_tagInfo(tagId: string; var status: TYRfidStatus):TYRfidTagInfo;
    var
      url : string;
      json : TByteArray;
      tagType : LongInt;
      size : LongInt;
      usable : LongInt;
      blksize : LongInt;
      fblk : LongInt;
      lblk : LongInt;
      res : TYRfidTagInfo;
    begin
      url := 'rfid.json?a=info&t='+tagId;

      json := self._download(url);
      self._chkerror(tagId, json, status);
      tagType := _atoi(self._json_get_key(json, 'type'));
      size := _atoi(self._json_get_key(json, 'size'));
      usable := _atoi(self._json_get_key(json, 'usable'));
      blksize := _atoi(self._json_get_key(json, 'blksize'));
      fblk := _atoi(self._json_get_key(json, 'fblk'));
      lblk := _atoi(self._json_get_key(json, 'lblk'));
      res :=  TYRfidTagInfo.create();
      res.imm_init(tagId, tagType, size, usable, blksize, fblk, lblk);
      result := res;
      exit;
    end;


  function TYRfidReader.tagLockBlocks(tagId: string; firstBlock: LongInt; nBlocks: LongInt; options: TYRfidOptions; var status: TYRfidStatus):LongInt;
    var
      optstr : string;
      url : string;
      json : TByteArray;
    begin
      optstr := options.imm_getParams;
      url := 'rfid.json?a=lock&t='+tagId+'&b='+inttostr(firstBlock)+'&n='+inttostr(nBlocks)+''+optstr;

      json := self._download(url);
      result := self._chkerror(tagId, json, status);
      exit;
    end;


  function TYRfidReader.get_tagLockState(tagId: string; firstBlock: LongInt; nBlocks: LongInt; options: TYRfidOptions; var status: TYRfidStatus):TBooleanArray;
    var
      optstr : string;
      url : string;
      json : TByteArray;
      binRes : TByteArray;
      res : TBooleanArray;
      idx : LongInt;
      val : LongInt;
      isLocked : boolean;
      res_pos : LongInt;
    begin
      optstr := options.imm_getParams;
      url := 'rfid.json?a=chkl&t='+tagId+'&b='+inttostr(firstBlock)+'&n='+inttostr(nBlocks)+''+optstr;

      json := self._download(url);
      self._chkerror(tagId, json, status);
      if status.get_yapiError <> YAPI_SUCCESS then
        begin
          result := res;
          exit;
        end;
      res_pos := length(res);
      SetLength(res, res_pos+nBlocks);;
      binRes := _hexStrToBin(self._json_get_key(json, 'bitmap'));
      idx := 0;
      while idx < nBlocks do
        begin
          val := binRes[((idx) shr 3)];
          isLocked := (((val) and (((1) shl (((idx) and 7))))) <> 0);
          res[res_pos] := isLocked;
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYRfidReader.get_tagSpecialBlocks(tagId: string; firstBlock: LongInt; nBlocks: LongInt; options: TYRfidOptions; var status: TYRfidStatus):TBooleanArray;
    var
      optstr : string;
      url : string;
      json : TByteArray;
      binRes : TByteArray;
      res : TBooleanArray;
      idx : LongInt;
      val : LongInt;
      isLocked : boolean;
      res_pos : LongInt;
    begin
      optstr := options.imm_getParams;
      url := 'rfid.json?a=chks&t='+tagId+'&b='+inttostr(firstBlock)+'&n='+inttostr(nBlocks)+''+optstr;

      json := self._download(url);
      self._chkerror(tagId, json, status);
      if status.get_yapiError <> YAPI_SUCCESS then
        begin
          result := res;
          exit;
        end;
      res_pos := length(res);
      SetLength(res, res_pos+nBlocks);;
      binRes := _hexStrToBin(self._json_get_key(json, 'bitmap'));
      idx := 0;
      while idx < nBlocks do
        begin
          val := binRes[((idx) shr 3)];
          isLocked := (((val) and (((1) shl (((idx) and 7))))) <> 0);
          res[res_pos] := isLocked;
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYRfidReader.tagReadHex(tagId: string; firstBlock: LongInt; nBytes: LongInt; options: TYRfidOptions; var status: TYRfidStatus):string;
    var
      optstr : string;
      url : string;
      json : TByteArray;
      hexbuf : string;
    begin
      optstr := options.imm_getParams;
      url := 'rfid.json?a=read&t='+tagId+'&b='+inttostr(firstBlock)+'&n='+inttostr(nBytes)+''+optstr;

      json := self._download(url);
      self._chkerror(tagId, json, status);
      if status.get_yapiError = YAPI_SUCCESS then
        begin
          hexbuf := self._json_get_key(json, 'res');
        end
      else
        begin
          hexbuf := '';
        end;
      result := hexbuf;
      exit;
    end;


  function TYRfidReader.tagReadBin(tagId: string; firstBlock: LongInt; nBytes: LongInt; options: TYRfidOptions; var status: TYRfidStatus):TByteArray;
    begin
      result := _hexStrToBin(self.tagReadHex(tagId, firstBlock, nBytes, options, status));
      exit;
    end;


  function TYRfidReader.tagReadArray(tagId: string; firstBlock: LongInt; nBytes: LongInt; options: TYRfidOptions; var status: TYRfidStatus):TLongIntArray;
    var
      blk : TByteArray;
      idx : LongInt;
      endidx : LongInt;
      res : TLongIntArray;
      res_pos : LongInt;
    begin
      blk := self.tagReadBin(tagId, firstBlock, nBytes, options, status);
      endidx := length(blk);
      res_pos := length(res);
      SetLength(res, res_pos+endidx);;
      idx := 0;
      while idx < endidx do
        begin
          res[res_pos] := blk[idx];
          inc(res_pos);
          idx := idx + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYRfidReader.tagReadStr(tagId: string; firstBlock: LongInt; nChars: LongInt; options: TYRfidOptions; var status: TYRfidStatus):string;
    begin
      result := _ByteToString(self.tagReadBin(tagId, firstBlock, nChars, options, status));
      exit;
    end;


  function TYRfidReader.tagWriteBin(tagId: string; firstBlock: LongInt; buff: TByteArray; options: TYRfidOptions; var status: TYRfidStatus):LongInt;
    var
      optstr : string;
      hexstr : string;
      buflen : LongInt;
      fname : string;
      json : TByteArray;
    begin
      buflen := length(buff);
      if buflen <= 16 then
        begin
          // short data, use an URL-based command
          hexstr := _bytesToHexStr(buff, 0, length(buff));
          result := self.tagWriteHex(tagId, firstBlock, hexstr, options, status);
          exit;
        end
      else
        begin
          // long data, use an upload command
          optstr := options.imm_getParams;
          fname := 'Rfid:t='+tagId+'&b='+inttostr(firstBlock)+'&n='+inttostr(buflen)+''+optstr;
          json := self._uploadEx(fname, buff);
          result := self._chkerror(tagId, json, status);
          exit;
        end;
    end;


  function TYRfidReader.tagWriteArray(tagId: string; firstBlock: LongInt; byteList: TLongIntArray; options: TYRfidOptions; var status: TYRfidStatus):LongInt;
    var
      bufflen : LongInt;
      buff : TByteArray;
      idx : LongInt;
      hexb : LongInt;
    begin
      bufflen := length(byteList);
      setlength(buff,bufflen);
      idx := 0;
      while idx < bufflen do
        begin
          hexb := byteList[idx];
          buff[idx] := hexb;
          idx := idx + 1;
        end;

      result := self.tagWriteBin(tagId, firstBlock, buff, options, status);
      exit;
    end;


  function TYRfidReader.tagWriteHex(tagId: string; firstBlock: LongInt; hexString: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt;
    var
      bufflen : LongInt;
      optstr : string;
      url : string;
      json : TByteArray;
      buff : TByteArray;
      idx : LongInt;
      hexb : LongInt;
    begin
      bufflen := Length(hexString);
      bufflen := ((bufflen) shr 1);
      if bufflen <= 16 then
        begin
          // short data, use an URL-based command
          optstr := options.imm_getParams;
          url := 'rfid.json?a=writ&t='+tagId+'&b='+inttostr(firstBlock)+'&w='+hexString+''+optstr;
          json := self._download(url);
          result := self._chkerror(tagId, json, status);
          exit;
        end
      else
        begin
          // long data, use an upload command
          setlength(buff,bufflen);
          idx := 0;
          while idx < bufflen do
            begin
              hexb := StrToInt('$0' + Copy(hexString, 2 * idx + 1, 2));
              buff[idx] := hexb;
              idx := idx + 1;
            end;
          result := self.tagWriteBin(tagId, firstBlock, buff, options, status);
          exit;
        end;
    end;


  function TYRfidReader.tagWriteStr(tagId: string; firstBlock: LongInt; text: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt;
    var
      buff : TByteArray;
    begin
      buff := _StrToByte(text);

      result := self.tagWriteBin(tagId, firstBlock, buff, options, status);
      exit;
    end;


  function TYRfidReader.tagGetAFI(tagId: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt;
    var
      optstr : string;
      url : string;
      json : TByteArray;
      res : LongInt;
    begin
      optstr := options.imm_getParams;
      url := 'rfid.json?a=rdsf&t='+tagId+'&b=0'+optstr;

      json := self._download(url);
      self._chkerror(tagId, json, status);
      if status.get_yapiError = YAPI_SUCCESS then
        begin
          res := _atoi(self._json_get_key(json, 'res'));
        end
      else
        begin
          res := status.get_yapiError;
        end;
      result := res;
      exit;
    end;


  function TYRfidReader.tagSetAFI(tagId: string; afi: LongInt; options: TYRfidOptions; var status: TYRfidStatus):LongInt;
    var
      optstr : string;
      url : string;
      json : TByteArray;
    begin
      optstr := options.imm_getParams;
      url := 'rfid.json?a=wrsf&t='+tagId+'&b=0&v='+inttostr(afi)+''+optstr;

      json := self._download(url);
      result := self._chkerror(tagId, json, status);
      exit;
    end;


  function TYRfidReader.tagLockAFI(tagId: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt;
    var
      optstr : string;
      url : string;
      json : TByteArray;
    begin
      optstr := options.imm_getParams;
      url := 'rfid.json?a=lksf&t='+tagId+'&b=0'+optstr;

      json := self._download(url);
      result := self._chkerror(tagId, json, status);
      exit;
    end;


  function TYRfidReader.tagGetDSFID(tagId: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt;
    var
      optstr : string;
      url : string;
      json : TByteArray;
      res : LongInt;
    begin
      optstr := options.imm_getParams;
      url := 'rfid.json?a=rdsf&t='+tagId+'&b=1'+optstr;

      json := self._download(url);
      self._chkerror(tagId, json, status);
      if status.get_yapiError = YAPI_SUCCESS then
        begin
          res := _atoi(self._json_get_key(json, 'res'));
        end
      else
        begin
          res := status.get_yapiError;
        end;
      result := res;
      exit;
    end;


  function TYRfidReader.tagSetDSFID(tagId: string; dsfid: LongInt; options: TYRfidOptions; var status: TYRfidStatus):LongInt;
    var
      optstr : string;
      url : string;
      json : TByteArray;
    begin
      optstr := options.imm_getParams;
      url := 'rfid.json?a=wrsf&t='+tagId+'&b=1&v='+inttostr(dsfid)+''+optstr;

      json := self._download(url);
      result := self._chkerror(tagId, json, status);
      exit;
    end;


  function TYRfidReader.tagLockDSFID(tagId: string; options: TYRfidOptions; var status: TYRfidStatus):LongInt;
    var
      optstr : string;
      url : string;
      json : TByteArray;
    begin
      optstr := options.imm_getParams;
      url := 'rfid.json?a=lksf&t='+tagId+'&b=1'+optstr;

      json := self._download(url);
      result := self._chkerror(tagId, json, status);
      exit;
    end;


  function TYRfidReader.get_lastEvents():string;
    var
      content : TByteArray;
    begin
      content := self._download('events.txt?pos=0');
      result := _ByteToString(content);
      exit;
    end;


  function TYRfidReader.registerEventCallback(callback: TYEventCallback):LongInt;
    begin
      self._eventCallback := callback;
      self._isFirstCb := true;
      if (addr(callback) <> nil) then
        begin
          self.registerValueCallback(yInternalEventCallback);
        end
      else
        begin
          self.registerValueCallback(TYRfidReaderValueCallback(nil));
        end;
      result := 0;
      exit;
    end;


  function TYRfidReader._internalEventHandler(cbVal: string):LongInt;
    var
      cbPos : LongInt;
      cbDPos : LongInt;
      url : string;
      content : TByteArray;
      contentStr : string;
      eventArr : TStringArray;
      arrLen : LongInt;
      lenStr : string;
      arrPos : LongInt;
      eventStr : string;
      eventLen : LongInt;
      hexStamp : string;
      typePos : LongInt;
      dataPos : LongInt;
      intStamp : LongInt;
      binMStamp : TByteArray;
      msStamp : LongInt;
      evtStamp : double;
      evtType : string;
      evtData : string;
    begin
      SetLength(eventArr, 0);
      // detect possible power cycle of the reader to clear event pointer
      cbPos := _atoi(cbVal);
      cbPos := (cbPos div 1000);
      cbDPos := ((cbPos - self._prevCbPos) and ($07ffff));
      self._prevCbPos := cbPos;
      if cbDPos > 16384 then
        begin
          self._eventPos := 0;
        end;
      if not((addr(self._eventCallback) <> nil)) then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      if self._isFirstCb then
        begin
          // first emulated value callback caused by registerValueCallback:
          // retrieve arrivals of all tags currently present to emulate arrival
          self._isFirstCb := false;
          self._eventStamp := 0;
          content := self._download('events.txt');
          contentStr := _ByteToString(content);
          eventArr := _stringSplit(contentStr, #10);
          arrLen := length(eventArr);
          if not(arrLen > 0) then
            begin
              self._throw(YAPI_IO_ERROR,'fail to download events');
              result:=YAPI_IO_ERROR;
              exit;
            end;
          // first element of array is the new position preceeded by '@'
          arrPos := 1;
          lenStr := eventArr[0];
          lenStr := Copy(lenStr, 1 + 1, Length(lenStr)-1);
          // update processed event position pointer
          self._eventPos := _atoi(lenStr);
        end
      else
        begin
          // load all events since previous call
          url := 'events.txt?pos='+inttostr(self._eventPos);
          content := self._download(url);
          contentStr := _ByteToString(content);
          eventArr := _stringSplit(contentStr, #10);
          arrLen := length(eventArr);
          if not(arrLen > 0) then
            begin
              self._throw(YAPI_IO_ERROR,'fail to download events');
              result:=YAPI_IO_ERROR;
              exit;
            end;
          // last element of array is the new position preceeded by '@'
          arrPos := 0;
          arrLen := arrLen - 1;
          lenStr := eventArr[arrLen];
          lenStr := Copy(lenStr, 1 + 1, Length(lenStr)-1);
          // update processed event position pointer
          self._eventPos := _atoi(lenStr);
        end;
      // now generate callbacks for each real event
      while arrPos < arrLen do
        begin
          eventStr := eventArr[arrPos];
          eventLen := Length(eventStr);
          typePos := (pos(':', eventStr) - 1)+1;
          if (eventLen >= 14) and(typePos > 10) then
            begin
              hexStamp := Copy(eventStr, 0 + 1, 8);
              intStamp := StrToInt('$0' + hexStamp);
              if intStamp >= self._eventStamp then
                begin
                  self._eventStamp := intStamp;
                  binMStamp := _StrToByte(Copy(eventStr, 8 + 1, 2));
                  msStamp := (binMStamp[0]-64) * 32 + binMStamp[1];
                  evtStamp := intStamp + (0.001 * msStamp);
                  dataPos := (pos('=', eventStr) - 1)+1;
                  evtType := Copy(eventStr, typePos + 1, 1);
                  evtData := '';
                  if dataPos > 10 then
                    begin
                      evtData := Copy(eventStr, dataPos + 1, eventLen-dataPos);
                    end;
                  if (addr(self._eventCallback) <> nil) then
                    begin
                      self._eventCallback(self, evtStamp, evtType, evtData);
                    end;
                end;
            end;
          arrPos := arrPos + 1;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYRfidReader.nextRfidReader(): TYRfidReader;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextRfidReader := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextRfidReader := nil;
          exit;
        end;
      nextRfidReader := TYRfidReader.FindRfidReader(hwid);
    end;

  class function TYRfidReader.FirstRfidReader(): TYRfidReader;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('RfidReader', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYRfidReader.FindRfidReader(serial+'.'+funcId);
    end;

Procedure yInternalEventCallback(obj:TYRfidReader; value:string);
begin
    obj._internalEventHandler(value);
end;

//--- (end of generated code: YRfidReader implementation)

//--- (generated code: YRfidReader functions)

  function yFindRfidReader(func:string): TYRfidReader;
    begin
      result := TYRfidReader.FindRfidReader(func);
    end;

  function yFirstRfidReader(): TYRfidReader;
    begin
      result := TYRfidReader.FirstRfidReader();
    end;

  procedure _RfidReaderCleanup();
    begin
    end;

//--- (end of generated code: YRfidReader functions)

initialization
  //--- (generated code: YRfidReader initialization)
  //--- (end of generated code: YRfidReader initialization)

finalization
  //--- (generated code: YRfidReader cleanup)
  _RfidReaderCleanup();
  //--- (end of generated code: YRfidReader cleanup)

end.
