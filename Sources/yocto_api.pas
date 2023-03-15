{*********************************************************************
 *
 * $Id: yocto_api.pas 53258 2023-02-16 11:16:45Z seb $
 *
 * High-level programming interface, common to all modules
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
unit yocto_api;

{$WRITEABLECONST OFF}
{$IFDEF FPC}{$MODE DELPHI}{$ENDIF}

interface

uses
  sysutils,classes,Math,

   {$IFNDEF UNIX}
    windows,  winsock,
   {$ENDIF}

  {$IFDEF CONDITIONALEXPRESSIONS}

     {$IF RTLVersion >= 14.0}
          DateUtils,
     {$IFEND}
  {$ENDIF}
  yjson;

type
  YHANDLE    = integer;
  YRETCODE   = integer;
  s8         = shortint;
  s16        = smallint;
  s32        = integer;
  s64        = int64;
  u8         = byte;
  u16        = word;
  u32        = longword;
  u64        = int64;

  YDEV_DESCR = s32;           // yStrRef of serial number
  YFUN_DESCR = s32;           // yStrRef of serial + (ystrRef of funcId << 16)
  PYDEVICE   = ^YDEV_DESCR;
  PYFUNCTION = ^YFUN_DESCR;
  yTime      = u32;           // measured in milliseconds
  yHash      = s16;
  yBlkHdl    = u16;           // (yHash << 1) + [0,1]
  yStrRef    = yHash;
  yUrlRef    = yHash;

  TStringArray    = array of string;
  TByteArray      = array of byte;
  TDoubleArray    = array of double;
  floatArr        = TDoubleArray;  // for backward compatibility
  TLongIntArray   = array of LongInt;
  intArr          = TLongIntArray;    // for backward compatibility
  pTLongIntArray  = ^TLongIntArray;
  TYDataLoggerRawData = array of array of double;

  yCalibrationHandler = function (rawValue:double; calibType: integer; params : TLongIntArray; rawValues,refValues:TDoubleArray ):double;

var
  // Switch to turn off exceptions and use return codes instead, for source-code compatibility
  // with languages without exception support like C
  YAPI_ExceptionsDisabled : Boolean ;
  YAPI_apiInitialized  : Boolean;
  _decExp: array[0..15] of Double =
    (1.0e-6, 1.0e-5, 1.0e-4, 1.0e-3, 1.0e-2, 1.0e-1, 1.0,
      1.0e1, 1.0e2, 1.0e3, 1.0e4, 1.0e5, 1.0e6, 1.0e7, 1.0e8, 1.0e9 );

const

  Y_FUNCTIONDESCRIPTOR_INVALID = -1;


  YAPI_INVALID_STRING     = '!INVALID!';
  YAPI_INVALID_DOUBLE     = -65536.0*65536.0*65536.0*65536.0*65536.0*65536.0;  // float-friendly constant
  YAPI_INVALID_INT        = longint($07FFFFFFF);
  YAPI_INVALID_UINT       = longint(-1);
  YAPI_INVALID_LONG       = longword($07FFFFFFFFFFFFFFF);
  InfinityAndBeyond       =  1.0 / 0.0;
  YAPI_MAX_DOUBLE         = +InfinityAndBeyond;
  YAPI_MIN_DOUBLE         = -InfinityAndBeyond;


  Y_HARDWAREID_INVALID   =  YAPI_INVALID_STRING;
  Y_FUNCTIONID_INVALID   =  YAPI_INVALID_STRING;
  Y_FRIENDLYNAME_INVALID =  YAPI_INVALID_STRING;

  // fyInitAPI argument
  Y_DETECT_NONE        = 0;
  Y_DETECT_USB         = 1;
  Y_DETECT_NET         = 2;
  Y_RESEND_MISSING_PKT = 4;
  Y_DETECT_ALL : integer = (Y_DETECT_USB or Y_DETECT_NET);

  YOCTO_API_VERSION_STR     = '1.10';
  YOCTO_API_VERSION_BCD     = $0110;
  YOCTO_API_BUILD_NO        = '53532';
  YOCTO_DEFAULT_PORT        = 4444;
  YOCTO_VENDORID            = $24e0;
  YOCTO_DEVID_FACTORYBOOT   = 1;
  YOCTO_DEVID_BOOTLOADER    = 2;

  YOCTO_ERRMSG_LEN          = 256;
  YOCTO_MANUFACTURER_LEN    = 20;
  YOCTO_SERIAL_LEN          = 20;
  YOCTO_BASE_SERIAL_LEN     = 8;
  YOCTO_PRODUCTNAME_LEN     = 28;
  YOCTO_FIRMWARE_LEN        = 22;
  YOCTO_LOGICAL_LEN         = 20;
  YOCTO_FUNCTION_LEN        = 20;
  YOCTO_PUBVAL_SIZE         =  6; // Size of the data (can be non null terminated)
  YOCTO_PUBVAL_LEN          = 16; // Temporary storage, > YOCTO_PUBVAL_SIZE
  YOCTO_PASS_LEN            = 20;
  YOCTO_REALM_LEN           = 20;
  YIOHDL_SIZE               = 8;

  YOCTO_CALIB_TYPE_OFS      = 30;

  INVALID_YHANDLE   =   0;

  //const definition for YDataStream
  Y_DATA_INVALID = YAPI_INVALID_DOUBLE;
  Y_DURATION_INVALID = -1;

  yUnknowSize = 1024;

type

  yDeviceSt = packed record
    vendorid        : u16;
    deviceid        : u16;
    devrelease      : u16;
    nbinbterfaces   : u16;
    manufacturer    : array [0..YOCTO_MANUFACTURER_LEN-1] of ansichar;
    productname     : array [0..YOCTO_PRODUCTNAME_LEN-1] of ansichar;
    serial          : array [0..YOCTO_SERIAL_LEN-1] of ansichar;
    logicalname     : array [0..YOCTO_LOGICAL_LEN-1] of ansichar;
    firmware        : array [0..YOCTO_FIRMWARE_LEN-1] of ansichar;
    beacon          : u8;
    pad             : u8;
  end;

  YIOHDL = packed record
    raw : array[0..YIOHDL_SIZE-1] of u8;
  end;

  PYIOHDL = ^YIOHDL;



  yDEVICE_PROP = ( PROP_VENDORID, PROP_DEVICEID,PROP_DEVRELEASE,
    PROP_FIRMWARELEVEL,PROP_MANUFACTURER,PROP_PRODUCTNAME,
    PROP_SERIAL,PROP_LOGICALNAME,PROP_URL );

  yFACE_STATUS = (YFACE_EMPTY,YFACE_RUNNING, YFACE_ERROR);



  ySerialList   = Array[0..yUnknowSize-1] of string[YOCTO_SERIAL_LEN];
  yHandleArray  = Array[0..yUnknowSize-1] of yHandle;
  PyHandleArray = ^yHandleArray;

  TyFlashCallback = function (step,totalStep:u32; context:pointer):integer; cdecl;

  PyFlashArg = ^TyFlashArg;

  TyFlashArg = packed record
    OSDeviceName  : pansichar;      //device windows name on os (used to acces device)
    serial2assign : pansichar;      //serial number of the device
    firmwarePtr   : pointer;        //pointer to the content of the Hex file
    firmwareLen   : u32;            //len of the Hexfile
    progress      : TyFlashCallback;
    context       : pointer;
  end;

//--- (generated code: YFunction definitions)

  // Yoctopuce error codes, also used by default as function return value
const YAPI_SUCCESS                   = 0;       // everything worked all right
const YAPI_NOT_INITIALIZED           = -1;      // call yInitAPI() first !
const YAPI_INVALID_ARGUMENT          = -2;      // one of the arguments passed to the function is invalid
const YAPI_NOT_SUPPORTED             = -3;      // the operation attempted is (currently) not supported
const YAPI_DEVICE_NOT_FOUND          = -4;      // the requested device is not reachable
const YAPI_VERSION_MISMATCH          = -5;      // the device firmware is incompatible with this API version
const YAPI_DEVICE_BUSY               = -6;      // the device is busy with another task and cannot answer
const YAPI_TIMEOUT                   = -7;      // the device took too long to provide an answer
const YAPI_IO_ERROR                  = -8;      // there was an I/O problem while talking to the device
const YAPI_NO_MORE_DATA              = -9;      // there is no more data to read from
const YAPI_EXHAUSTED                 = -10;     // you have run out of a limited resource, check the documentation
const YAPI_DOUBLE_ACCES              = -11;     // you have two process that try to access to the same device
const YAPI_UNAUTHORIZED              = -12;     // unauthorized access to password-protected device
const YAPI_RTC_NOT_READY             = -13;     // real-time clock has not been initialized (or time was lost)
const YAPI_FILE_NOT_FOUND            = -14;     // the file is not found
const YAPI_SSL_ERROR                 = -15;     // Error reported by mbedSSL

const Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
const Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;


//--- (end of generated code: YFunction definitions)
const YAPI_HASH_BUF_SIZE            = 28;

//--- (generated code: YModule definitions)

const Y_PRODUCTNAME_INVALID           = YAPI_INVALID_STRING;
const Y_SERIALNUMBER_INVALID          = YAPI_INVALID_STRING;
const Y_PRODUCTID_INVALID             = YAPI_INVALID_UINT;
const Y_PRODUCTRELEASE_INVALID        = YAPI_INVALID_UINT;
const Y_FIRMWARERELEASE_INVALID       = YAPI_INVALID_STRING;
const Y_PERSISTENTSETTINGS_LOADED = 0;
const Y_PERSISTENTSETTINGS_SAVED = 1;
const Y_PERSISTENTSETTINGS_MODIFIED = 2;
const Y_PERSISTENTSETTINGS_INVALID = -1;
const Y_LUMINOSITY_INVALID            = YAPI_INVALID_UINT;
const Y_BEACON_OFF = 0;
const Y_BEACON_ON = 1;
const Y_BEACON_INVALID = -1;
const Y_UPTIME_INVALID                = YAPI_INVALID_LONG;
const Y_USBCURRENT_INVALID            = YAPI_INVALID_UINT;
const Y_REBOOTCOUNTDOWN_INVALID       = YAPI_INVALID_INT;
const Y_USERVAR_INVALID               = YAPI_INVALID_INT;


//--- (end of generated code: YModule definitions)
//--- (generated code: YSensor definitions)

const Y_UNIT_INVALID                  = YAPI_INVALID_STRING;
const Y_CURRENTVALUE_INVALID          = YAPI_INVALID_DOUBLE;
const Y_LOWESTVALUE_INVALID           = YAPI_INVALID_DOUBLE;
const Y_HIGHESTVALUE_INVALID          = YAPI_INVALID_DOUBLE;
const Y_CURRENTRAWVALUE_INVALID       = YAPI_INVALID_DOUBLE;
const Y_LOGFREQUENCY_INVALID          = YAPI_INVALID_STRING;
const Y_REPORTFREQUENCY_INVALID       = YAPI_INVALID_STRING;
const Y_ADVMODE_IMMEDIATE = 0;
const Y_ADVMODE_PERIOD_AVG = 1;
const Y_ADVMODE_PERIOD_MIN = 2;
const Y_ADVMODE_PERIOD_MAX = 3;
const Y_ADVMODE_INVALID = -1;
const Y_CALIBRATIONPARAM_INVALID      = YAPI_INVALID_STRING;
const Y_RESOLUTION_INVALID            = YAPI_INVALID_DOUBLE;
const Y_SENSORSTATE_INVALID           = YAPI_INVALID_INT;


//--- (end of generated code: YSensor definitions)

//--- (generated code: YAPIContext definitions)


//--- (end of generated code: YAPIContext definitions)


//--- (generated code: YFirmwareUpdate definitions)


//--- (end of generated code: YFirmwareUpdate definitions)
//--- (generated code: YDataStream definitions)


//--- (end of generated code: YDataStream definitions)
//--- (generated code: YMeasure definitions)


//--- (end of generated code: YMeasure definitions)
//--- (generated code: YDataSet definitions)


//--- (end of generated code: YDataSet definitions)
//--- (generated code: YConsolidatedDataSet definitions)


//--- (end of generated code: YConsolidatedDataSet definitions)

//--- (generated code: YDataLogger definitions)

const Y_CURRENTRUNINDEX_INVALID       = YAPI_INVALID_UINT;
const Y_TIMEUTC_INVALID               = YAPI_INVALID_LONG;
const Y_RECORDING_OFF = 0;
const Y_RECORDING_ON = 1;
const Y_RECORDING_PENDING = 2;
const Y_RECORDING_INVALID = -1;
const Y_AUTOSTART_OFF = 0;
const Y_AUTOSTART_ON = 1;
const Y_AUTOSTART_INVALID = -1;
const Y_BEACONDRIVEN_OFF = 0;
const Y_BEACONDRIVEN_ON = 1;
const Y_BEACONDRIVEN_INVALID = -1;
const Y_USAGE_INVALID                 = YAPI_INVALID_UINT;
const Y_CLEARHISTORY_FALSE = 0;
const Y_CLEARHISTORY_TRUE = 1;
const Y_CLEARHISTORY_INVALID = -1;


//--- (end of generated code: YDataLogger definitions)

 Function IsNAN_D5 (const sgl: single)  : boolean; overload;
 Function IsNAN_D5 (const dbl: double)  : boolean; overload;


type
  TYDevice = class;
  TYFunction = class;
  TYModule = class;
  TYSensor = class;
  TYFirmwareUpdate = class;
  TYDataLogger = class;
  TYDataStream = class;
  TYMeasure = class;
  TYDataSet = class;
  TYConsolidatedDataSet = class;

  TDoubleArrayArray = array of TDoubleArray;
  TYDataStreamArray = array of TYDataStream;
  TYMeasureArray = array of TYMeasure;
  TYDataSetArray  = array of TYDataSet;
  TYSensorArray   = array of TYSensor;

  TYAPIContext = class;


  YAPI_Exception  = class (exception)
  public
    errorType   :  YRETCODE;
    constructor  Create(errType:YRETCODE;  errMsg:string ); overload;
  end;

  THTTPRequestCallback = procedure(device:TYDevice;context:pointer;returnval:YRETCODE; result,errmsg:string );

  TYDevice = class(Tobject)
  private
    _devdescr   : YDEV_DESCR;
    _cacheStamp : u64;
    _cacheJson  : TJsonParser;
    _functions  : Tlist;
    _http_result: string;
    _rootdevice : string;
    _subpath    : string;
    _subpathinit:boolean;
  public
    constructor Create(devdesc:YDEV_DESCR);
    class function getDevice(devdescr:YDEV_DESCR ):TYDevice;
    class procedure PlugDevice(devdescr:YDEV_DESCR );
    function  HTTPRequestPrepare(request: TByteArray; var fullrequest: TByteArray; var errmsg:string):YRETCODE;
    function  HTTPRequestAsync(request: string; var errmsg: string):YRETCODE; overload;
    function  HTTPRequestAsync(request: TByteArray; var errmsg: string):YRETCODE; overload;
    function  HTTPRequest(request :string ; var buffer : string; var   errmsg:string) : YRETCODE; overload;
    function  HTTPRequest(request :string ; var buffer : TByteArray; var   errmsg:string) : YRETCODE; overload;
    function  HTTPRequest(request :TByteArray ; var buffer : TByteArray; var   errmsg:string) : YRETCODE; overload;
    function  requestAPI(var apires:TJsonParser;var errmsg:string):YRETCODE;
    procedure clearCache();
    function  getFunctions(var functions:tlist; var errmsg:string):YRETCODE;
    destructor Destroy();override;
  end;


  //--- (generated code: YFunction class start)
  TYFunctionValueCallback = procedure(func: TYFunction; value:string);
  TYFunctionTimedReportCallback = procedure(func: TYFunction; value:TYMeasure);

  ////
  /// <summary>
  ///   TYFunction Class: Common function interface
  /// <para>
  ///   This is the parent class for all public objects representing device functions documented in
  ///   the high-level programming API. This abstract class does all the real job, but without
  ///   knowledge of the specific function attributes.
  /// </para>
  /// <para>
  ///   Instantiating a child class of YFunction does not cause any communication.
  ///   The instance simply keeps track of its function identifier, and will dynamically bind
  ///   to a matching device at the time it is really being used to read or set an attribute.
  ///   In order to allow true hot-plug replacement of one device by another, the binding stay
  ///   dynamic through the life of the object.
  /// </para>
  /// <para>
  ///   The YFunction class implements a generic high-level cache for the attribute values of
  ///   the specified function, pre-parsed from the REST API string.
  /// </para>
  /// </summary>
  ///-
  TYFunction=class(TObject)
  //--- (end of generated code: YFunction class start)
  protected
    _className       : string;
    _func            : string;
    _lastErrorType   : YRETCODE;
    _lastErrorMsg    : string;
    _fundescr        : YFUN_DESCR;
    _userdata        : Tobject;
    _dataStreams     : TStringList;

    //--- (generated code: YFunction declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _valueCallbackFunction    : TYFunctionValueCallback;
    _cacheExpiration          : u64;
    _serial                   : string;
    _funId                    : string;
    _hwId                     : string;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; virtual;

    //--- (end of generated code: YFunction declaration)

    function _parse(j:PJSONRECORD):integer;

    // Constructor is protected. Use the device-specific factory function to instantiate
    Constructor Create(func:string); overload;

    // Method used to throw exceptions or save error type/message
    Procedure _throw(errType:YRETCODE;  errMsg:string );

    // Method used to retrieve our unique function descriptor (may trigger a hub scan)
    function  _getDescriptor(var fundescr:YFUN_DESCR ; var errMsg:string):YRETCODE;

    // Method used to retrieve our device object (may trigger a hub scan)
    function  _getDevice(var dev:TYDevice; var errMsg:string):YRETCODE;

    // Return the next known function of current class listed in the yellow pages
    function  _nextFunction(var hwid:string):YRETCODE;

    // Set an attribute in the function, and parse the resulting new function state
    function _setAttr( attrname:string; newvalue:string):YRETCODE;

    function  _strip_http_header(buffer:TByteArray) :TByteArray;
    function  _upload(path:string; strcontent:string):integer; overload;
    function  _upload(path:string; content:TByteArray):integer; overload;
    function  _uploadEx(path:string; content:TByteArray):TByteArray; overload;

    function  _download(path:string) :TByteArray;
    function  _request(request: string) :TByteArray; overload;
    function  _request(request: TByteArray) :TByteArray; overload;
    function  _json_get_array(data: TByteArray) :TStringArray;
    function  _get_json_path(json, path: string):string;
    function  _decode_json_string(json: string):string;
    function  _json_get_key(data: TByteArray; key: string):string;
    function  _json_get_string(data: TByteArray):string;

    function  _escapeAttr(changeval:string):string;
    function  _buildSetRequest(changeattr : string ; changeval:string ; var request:string; var errmsg:string):YRETCODE;

  public
    class function _FindFromCache(classname: string; func: string): TYFunction;
    class procedure _AddToCache(classname: string; func: string; obj: TYFunction);
    class procedure _ClearCache();
    class procedure _UpdateValueCallbackList(func : TYFunction; add : boolean);
    class procedure _UpdateTimedReportCallbackList(func : TYFunction; add : boolean);

    Destructor Destroy(); override;

    function  _findDataStream(dataset: TYDataSet; def :string) : TYDataStream;
    procedure  _clearDataStreamCache();

    ////
    /// <summary>
    ///   Returns a short text that describes unambiguously the instance of the function in the form <c>TYPE(NAME)=SERIAL&#46;FUNCTIONID</c>.
    /// <para>
    ///   More precisely,
    ///   <c>TYPE</c>       is the type of the function,
    ///   <c>NAME</c>       it the name used for the first access to the function,
    ///   <c>SERIAL</c>     is the serial number of the module if the module is connected or <c>"unresolved"</c>, and
    ///   <c>FUNCTIONID</c> is  the hardware identifier of the function if the module is connected.
    ///   For example, this method returns <c>Relay(MyCustomName.relay1)=RELAYLO1-123456.relay1</c> if the
    ///   module is already connected or <c>Relay(BadCustomeName.relay1)=unresolved</c> if the module has
    ///   not yet been connected. This method does not trigger any USB or TCP transaction and can therefore be used in
    ///   a debugger.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string that describes the function
    ///   (ex: <c>Relay(MyCustomName.relay1)=RELAYLO1-123456.relay1</c>)
    /// </returns>
    ///-
    function describe():string;

    ///
    /// <summary>
    /// Returns the unique hardware identifier of the function.
    /// <para>
    /// The unique hardware identifier is made of the device serial
    /// number and of the hardware identifier of the function.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   return a string that uniquely identifies the function
    /// </returns>
    function get_hardwareId():string; virtual;

    ///
    /// <summary>
    /// Returns the unique hardware identifier of the function.
    /// <para>
    /// The unique hardware identifier is made of the device serial
    /// number and of the hardware identifier of the function.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   return a string that uniquely identifies the function
    /// </returns>
    function get_functionId():string;

    ///
    /// <summary>
    /// Returns the unique hardware identifier of the function.
    /// <para>
    /// The unique hardware identifier is made of the device serial
    /// number and of the hardware identifier of the function.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   return a string that uniquely identifies the function
    /// </returns>
    function get_friendlyName():string;


    ////
    /// <summary>
    ///   Returns the numerical error code of the latest error with the function.
    /// <para>
    ///   This method is mostly useful when using the Yoctopuce library with
    ///   exceptions disabled.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a number corresponding to the code of the latest error that occurred while
    ///   using the function object
    /// </returns>
    ///-
    function get_errorType():YRETCODE;
    function errorType():YRETCODE;
    function errType():YRETCODE;

    ////
    /// <summary>
    ///   Returns the error message of the latest error with the function.
    /// <para>
    ///   This method is mostly useful when using the Yoctopuce library with
    ///   exceptions disabled.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the latest error message that occured while
    ///   using the function object
    /// </returns>
    ///-
    function get_errorMessage():string;
    function errorMessage():string;
    function errMessage():string;

    ////
    /// <summary>
    ///   Checks if the function is currently reachable, without raising any error.
    /// <para>
    ///   If there is a cached value for the function in cache, that has not yet
    ///   expired, the device is considered reachable.
    ///   No exception is raised if there is an error while trying to contact the
    ///   device hosting the function.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>true</c> if the function can be reached, and <c>false</c> otherwise
    /// </returns>
    ///-
    function isOnline():boolean;

    ////
    /// <summary>
    ///   Preloads the function cache with a specified validity duration.
    /// <para>
    ///   By default, whenever accessing a device, all function attributes
    ///   are kept in cache for the standard duration (5 ms). This method can be
    ///   used to temporarily mark the cache as valid for a longer period, in order
    ///   to reduce network traffic for instance.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="msValidity">
    ///   an integer corresponding to the validity attributed to the
    ///   loaded function parameters, in milliseconds
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function load(msValidity:u64):YRETCODE;

    ////
    /// <summary>
    ///   Invalidates the cache.
    /// <para>
    ///   Invalidates the cache of the function attributes. Forces the
    ///   next call to get_xxx() or loadxxx() to use values that come from the device.
    /// </para>
    /// <para>
    /// @noreturn
    /// </para>
    /// </summary>
    ///-
    procedure clearCache();


    ////
    /// <summary>
    ///   Gets the <c>YModule</c> object for the device on which the function is located.
    /// <para>
    ///   If the function cannot be located on any module, the returned instance of
    ///   <c>YModule</c> is not shown as on-line.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an instance of <c>YModule</c>
    /// </returns>
    ///-
    function get_module():TYModule;

    ////
    /// <summary>
    ///   Returns a unique identifier of type <c>YFUN_DESCR</c> corresponding to the function.
    /// <para>
    ///   This identifier can be used to test if two instances of <c>YFunction</c> reference the same
    ///   physical function on the same physical device.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an identifier of type <c>YFUN_DESCR</c>.
    /// </returns>
    /// <para>
    ///   If the function has never been contacted, the returned value is <c>Y$CLASSNAME$.FUNCTIONDESCRIPTOR_INVALID</c>.
    /// </para>
    ///-
    function get_functionDescriptor():YFUN_DESCR;

    ////
    /// <summary>
    ///   Returns the value of the userData attribute, as previously stored using method
    ///   <c>set_userData</c>.
    /// <para>
    ///   This attribute is never touched directly by the API, and is at disposal of the caller to
    ///   store a context.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the object stored previously by the caller.
    /// </returns>
    ///-
    function get_userData():Tobject;

    ////
    /// <summary>
    ///   Stores a user context provided as argument in the userData attribute of the function.
    /// <para>
    ///   This attribute is never touched by the API, and is at disposal of the caller to store a context.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="data">
    ///   any kind of object to be stored
    /// @noreturn
    /// </param>
    ///-
    procedure set_userData(data : Tobject);

    ////
    ///
    ///-
    {$Warnings OFF}
     // disable the override warning: we cannot use the override directive
     // because Tobject.ToString does not exists in all delphi versions.
     function ToString():string;

    //--- (generated code: YFunction accessors declaration)
    ////
    /// <summary>
    ///   Returns the logical name of the function.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the logical name of the function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YFunction.LOGICALNAME_INVALID</c>.
    /// </para>
    ///-
    function get_logicalName():string;

    ////
    /// <summary>
    ///   Changes the logical name of the function.
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
    ///   a string corresponding to the logical name of the function
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
    function set_logicalName(newval:string):integer;

    ////
    /// <summary>
    ///   Returns a short string representing the current state of the function.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to a short string representing the current state of the function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YFunction.ADVERTISEDVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_advertisedValue():string;

    function set_advertisedValue(newval:string):integer;

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
    ///   Use the method <c>YFunction.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YFunction</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindFunction(func: string):TYFunction;

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
    function registerValueCallback(callback: TYFunctionValueCallback):LongInt; overload; virtual;

    function _invokeValueCallback(value: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Disables the propagation of every new advertised value to the parent hub.
    /// <para>
    ///   You can use this function to save bandwidth and CPU on computers with limited
    ///   resources, or to prevent unwanted invocations of the HTTP callback.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function muteValueCallbacks():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Re-enables the propagation of every new advertised value to the parent hub.
    /// <para>
    ///   This function reverts the effect of a previous call to <c>muteValueCallbacks()</c>.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function unmuteValueCallbacks():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the current value of a single function attribute, as a text string, as quickly as
    ///   possible but without using the cached value.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="attrName">
    ///   the name of the requested attribute
    /// </param>
    /// <returns>
    ///   a string with the value of the the attribute
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function loadAttribute(attrName: string):string; overload; virtual;

    ////
    /// <summary>
    ///   Test if the function is readOnly.
    /// <para>
    ///   Return <c>true</c> if the function is write protected
    ///   or that the function is not available.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>true</c> if the function is readOnly or not online.
    /// </returns>
    ///-
    function isReadOnly():boolean; overload; virtual;

    ////
    /// <summary>
    ///   Returns the serial number of the module, as set by the factory.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the serial number of the module, as set by the factory.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns YFunction.SERIALNUMBER_INVALID.
    /// </para>
    ///-
    function get_serialNumber():string; overload; virtual;

    function _parserHelper():LongInt; overload; virtual;


    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    function nextFunction():TYFunction;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstFunction():TYFunction;
  //--- (end of generated code: YFunction accessors declaration)
  end;

  {$Warnings ON}


  //--- (generated code: YModule class start)
  TYModuleLogCallback = procedure(func: TYModule; logline:string);
  TYModuleConfigChangeCallback = procedure(func: TYModule);
  TYModuleBeaconCallback = procedure(func: TYModule; beacon:Integer);
  TYModuleValueCallback = procedure(func: TYModule; value:string);
  TYModuleTimedReportCallback = procedure(func: TYModule; value:TYMeasure);

  ////
  /// <summary>
  ///   TYModule Class: Global parameters control interface for all Yoctopuce devices
  /// <para>
  ///   The <c>YModule</c> class can be used with all Yoctopuce USB devices.
  ///   It can be used to control the module global parameters, and
  ///   to enumerate the functions provided by each module.
  /// </para>
  /// </summary>
  ///-
  TYModule=class(TYFunction)
  //--- (end of generated code: YModule class start)
  protected
  //--- (generated code: YModule declaration)
    // Attributes (function value cache)
    _productName              : string;
    _serialNumber             : string;
    _productId                : LongInt;
    _productRelease           : LongInt;
    _firmwareRelease          : string;
    _persistentSettings       : Integer;
    _luminosity               : LongInt;
    _beacon                   : Integer;
    _upTime                   : int64;
    _usbCurrent               : LongInt;
    _rebootCountdown          : LongInt;
    _userVar                  : LongInt;
    _valueCallbackModule      : TYModuleValueCallback;
    _logCallback              : TYModuleLogCallback;
    _confChangeCallback       : TYModuleConfigChangeCallback;
    _beaconCallback           : TYModuleBeaconCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YModule declaration)

    // Return the properties of the nth function of our device
    function _getFunction(idx:integer; var serial,funcId,baseType,funcName,funcVal,errMsg:string):YRETCODE;

  public

    function get_logicalName_internal():string;


    ////
    /// <summary>
    ///   Returns the number of functions (beside the "module" interface) available on the module.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the number of functions on the module
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function functionCount():integer;

    ////
    /// <summary>
    ///   Retrieves the hardware identifier of the <i>n</i>th function on the module.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="functionIndex">
    ///   the index of the function for which the information is desired, starting at 0 for the first function.
    /// </param>
    /// <returns>
    ///   a string corresponding to the unambiguous hardware identifier of the requested module function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function functionId(functionIndex:integer):string;

    ////
    /// <summary>
    ///   Retrieves the type of the <i>n</i>th function on the module.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="functionIndex">
    ///   the index of the function for which the information is desired, starting at 0 for the first function.
    /// </param>
    /// <returns>
    ///   a string corresponding to the type of the function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function functionType(functionIndex:integer):string;

    ////
    /// <summary>
    ///   Retrieves the base type of the <i>n</i>th function on the module.
    /// <para>
    ///   For instance, the base type of all measuring functions is "Sensor".
    /// </para>
    /// </summary>
    /// <param name="functionIndex">
    ///   the index of the function for which the information is desired, starting at 0 for the first function.
    /// </param>
    /// <returns>
    ///   a string corresponding to the base type of the function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function functionBaseType(functionIndex:integer):string;

    ////
    /// <summary>
    ///   Retrieves the logical name of the <i>n</i>th function on the module.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="functionIndex">
    ///   the index of the function for which the information is desired, starting at 0 for the first function.
    /// </param>
    /// <returns>
    ///   a string corresponding to the logical name of the requested module function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function functionName(functionIndex:integer):string;

    ////
    /// <summary>
    ///   Retrieves the advertised value of the <i>n</i>th function on the module.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="functionIndex">
    ///   the index of the function for which the information is desired, starting at 0 for the first function.
    /// </param>
    /// <returns>
    ///   a short string (up to 6 characters) corresponding to the advertised value of the requested module function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty string.
    /// </para>
    ///-
    function functionValue(functionIndex:integer):string;


    ///
    /// <summary>
    /// Returns the unique hardware identifier of the function.
    /// <para>
    /// The unique hardware identifier is made of the device serial
    /// number and of the hardware identifier of the function.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   return a string that uniquely identifies the function
    /// </returns>
    function     get_friendlyName():string;

    procedure setImmutableAttributes(var infos : yDeviceSt);

    class procedure _updateModuleCallbackList(modul : TYModule; add : boolean);

    //--- (generated code: YModule accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the commercial name of the module, as set by the factory.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the commercial name of the module, as set by the factory
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.PRODUCTNAME_INVALID</c>.
    /// </para>
    ///-
    function get_productName():string;

    ////
    /// <summary>
    ///   Returns the serial number of the module, as set by the factory.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the serial number of the module, as set by the factory
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.SERIALNUMBER_INVALID</c>.
    /// </para>
    ///-
    function get_serialNumber():string; override;

    ////
    /// <summary>
    ///   Returns the USB device identifier of the module.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the USB device identifier of the module
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.PRODUCTID_INVALID</c>.
    /// </para>
    ///-
    function get_productId():LongInt;

    ////
    /// <summary>
    ///   Returns the release number of the module hardware, preprogrammed at the factory.
    /// <para>
    ///   The original hardware release returns value 1, revision B returns value 2, etc.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the release number of the module hardware, preprogrammed at the factory
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.PRODUCTRELEASE_INVALID</c>.
    /// </para>
    ///-
    function get_productRelease():LongInt;

    ////
    /// <summary>
    ///   Returns the version of the firmware embedded in the module.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the version of the firmware embedded in the module
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.FIRMWARERELEASE_INVALID</c>.
    /// </para>
    ///-
    function get_firmwareRelease():string;

    ////
    /// <summary>
    ///   Returns the current state of persistent module settings.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YModule.PERSISTENTSETTINGS_LOADED</c>, <c>YModule.PERSISTENTSETTINGS_SAVED</c> and
    ///   <c>YModule.PERSISTENTSETTINGS_MODIFIED</c> corresponding to the current state of persistent module settings
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.PERSISTENTSETTINGS_INVALID</c>.
    /// </para>
    ///-
    function get_persistentSettings():Integer;

    function set_persistentSettings(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the luminosity of the  module informative LEDs (from 0 to 100).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the luminosity of the  module informative LEDs (from 0 to 100)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.LUMINOSITY_INVALID</c>.
    /// </para>
    ///-
    function get_luminosity():LongInt;

    ////
    /// <summary>
    ///   Changes the luminosity of the module informative leds.
    /// <para>
    ///   The parameter is a
    ///   value between 0 and 100.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the luminosity of the module informative leds
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
    function set_luminosity(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the state of the localization beacon.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YModule.BEACON_OFF</c> or <c>YModule.BEACON_ON</c>, according to the state of the localization beacon
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.BEACON_INVALID</c>.
    /// </para>
    ///-
    function get_beacon():Integer;

    ////
    /// <summary>
    ///   Turns on or off the module localization beacon.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YModule.BEACON_OFF</c> or <c>YModule.BEACON_ON</c>
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
    function set_beacon(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the number of milliseconds spent since the module was powered on.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of milliseconds spent since the module was powered on
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.UPTIME_INVALID</c>.
    /// </para>
    ///-
    function get_upTime():int64;

    ////
    /// <summary>
    ///   Returns the current consumed by the module on the USB bus, in milli-amps.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current consumed by the module on the USB bus, in milli-amps
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.USBCURRENT_INVALID</c>.
    /// </para>
    ///-
    function get_usbCurrent():LongInt;

    ////
    /// <summary>
    ///   Returns the remaining number of seconds before the module restarts, or zero when no
    ///   reboot has been scheduled.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the remaining number of seconds before the module restarts, or zero when no
    ///   reboot has been scheduled
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.REBOOTCOUNTDOWN_INVALID</c>.
    /// </para>
    ///-
    function get_rebootCountdown():LongInt;

    function set_rebootCountdown(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the value previously stored in this attribute.
    /// <para>
    ///   On startup and after a device reboot, the value is always reset to zero.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the value previously stored in this attribute
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YModule.USERVAR_INVALID</c>.
    /// </para>
    ///-
    function get_userVar():LongInt;

    ////
    /// <summary>
    ///   Stores a 32 bit value in the device RAM.
    /// <para>
    ///   This attribute is at programmer disposal,
    ///   should he need to store a state variable.
    ///   On startup and after a device reboot, the value is always reset to zero.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer
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
    function set_userVar(newval:LongInt):integer;

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
    ///   Use the method <c>YModule.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YModule</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindModule(func: string):TYModule;

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
    function registerValueCallback(callback: TYModuleValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function get_productNameAndRevision():string; overload; virtual;

    ////
    /// <summary>
    ///   Saves current settings in the nonvolatile memory of the module.
    /// <para>
    ///   Warning: the number of allowed save operations during a module life is
    ///   limited (about 100000 cycles). Do not call this function within a loop.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function saveToFlash():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Reloads the settings stored in the nonvolatile memory, as
    ///   when the module is powered on.
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
    function revertFromFlash():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Schedules a simple module reboot after the given number of seconds.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="secBeforeReboot">
    ///   number of seconds before rebooting
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function reboot(secBeforeReboot: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Schedules a module reboot into special firmware update mode.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="secBeforeReboot">
    ///   number of seconds before rebooting
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function triggerFirmwareUpdate(secBeforeReboot: LongInt):LongInt; overload; virtual;

    procedure _startStopDevLog(serial: string; start: boolean); overload; virtual;

    ////
    /// <summary>
    ///   Registers a device log callback function.
    /// <para>
    ///   This callback will be called each time
    ///   that a module sends a new log message. Mostly useful to debug a Yoctopuce module.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="callback">
    ///   the callback function to call, or a NIL pointer. The callback function should take two
    ///   arguments: the module object that emitted the log message, and the character string containing the log.
    ///   On failure, throws an exception or returns a negative error code.
    /// </param>
    ///-
    function registerLogCallback(callback: TYModuleLogCallback):LongInt; overload; virtual;

    function get_logCallback():TYModuleLogCallback; overload; virtual;

    ////
    /// <summary>
    ///   Register a callback function, to be called when a persistent settings in
    ///   a device configuration has been changed (e.g.
    /// <para>
    ///   change of unit, etc).
    /// </para>
    /// </summary>
    /// <param name="callback">
    ///   a procedure taking a YModule parameter, or <c>NIL</c>
    ///   to unregister a previously registered  callback.
    /// </param>
    ///-
    function registerConfigChangeCallback(callback: TYModuleConfigChangeCallback):LongInt; overload; virtual;

    function _invokeConfigChangeCallback():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Register a callback function, to be called when the localization beacon of the module
    ///   has been changed.
    /// <para>
    ///   The callback function should take two arguments: the YModule object of
    ///   which the beacon has changed, and an integer describing the new beacon state.
    /// </para>
    /// </summary>
    /// <param name="callback">
    ///   The callback function to call, or <c>NIL</c> to unregister a
    ///   previously registered callback.
    /// </param>
    ///-
    function registerBeaconCallback(callback: TYModuleBeaconCallback):LongInt; overload; virtual;

    function _invokeBeaconCallback(beaconState: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Triggers a configuration change callback, to check if they are supported or not.
    /// <para>
    /// </para>
    /// </summary>
    ///-
    function triggerConfigChangeCallback():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Tests whether the byn file is valid for this module.
    /// <para>
    ///   This method is useful to test if the module needs to be updated.
    ///   It is possible to pass a directory as argument instead of a file. In this case, this method returns
    ///   the path of the most recent
    ///   appropriate <c>.byn</c> file. If the parameter <c>onlynew</c> is true, the function discards
    ///   firmwares that are older or
    ///   equal to the installed firmware.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="path">
    ///   the path of a byn file or a directory that contains byn files
    /// </param>
    /// <param name="onlynew">
    ///   returns only files that are strictly newer
    /// </param>
    /// <para>
    /// </para>
    /// <returns>
    ///   the path of the byn file to use or a empty string if no byn files matches the requirement
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a string that start with "error:".
    /// </para>
    ///-
    function checkFirmware(path: string; onlynew: boolean):string; overload; virtual;

    ////
    /// <summary>
    ///   Prepares a firmware update of the module.
    /// <para>
    ///   This method returns a <c>YFirmwareUpdate</c> object which
    ///   handles the firmware update process.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="path">
    ///   the path of the <c>.byn</c> file to use.
    /// </param>
    /// <param name="force">
    ///   true to force the firmware update even if some prerequisites appear not to be met
    /// </param>
    /// <returns>
    ///   a <c>YFirmwareUpdate</c> object or NIL on error.
    /// </returns>
    ///-
    function updateFirmwareEx(path: string; force: boolean):TYFirmwareUpdate; overload; virtual;

    ////
    /// <summary>
    ///   Prepares a firmware update of the module.
    /// <para>
    ///   This method returns a <c>YFirmwareUpdate</c> object which
    ///   handles the firmware update process.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="path">
    ///   the path of the <c>.byn</c> file to use.
    /// </param>
    /// <returns>
    ///   a <c>YFirmwareUpdate</c> object or NIL on error.
    /// </returns>
    ///-
    function updateFirmware(path: string):TYFirmwareUpdate; overload; virtual;

    ////
    /// <summary>
    ///   Returns all the settings and uploaded files of the module.
    /// <para>
    ///   Useful to backup all the
    ///   logical names, calibrations parameters, and uploaded files of a device.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a binary buffer with all the settings.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an binary object of size 0.
    /// </para>
    ///-
    function get_allSettings():TByteArray; overload; virtual;

    function loadThermistorExtra(funcId: string; jsonExtra: string):LongInt; overload; virtual;

    function set_extraSettings(jsonExtra: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Restores all the settings and uploaded files to the module.
    /// <para>
    ///   This method is useful to restore all the logical names and calibrations parameters,
    ///   uploaded files etc. of a device from a backup.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modifications must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="settings">
    ///   a binary buffer with all the settings.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_allSettingsAndFiles(settings: TByteArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Tests if the device includes a specific function.
    /// <para>
    ///   This method takes a function identifier
    ///   and returns a boolean.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="funcId">
    ///   the requested function identifier
    /// </param>
    /// <returns>
    ///   true if the device has the function identifier
    /// </returns>
    ///-
    function hasFunction(funcId: string):boolean; overload; virtual;

    ////
    /// <summary>
    ///   Retrieve all hardware identifier that match the type passed in argument.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="funType">
    ///   The type of function (Relay, LightSensor, Voltage,...)
    /// </param>
    /// <returns>
    ///   an array of strings.
    /// </returns>
    ///-
    function get_functionIds(funType: string):TStringArray; overload; virtual;

    function _flattenJsonStruct(jsoncomplex: TByteArray):TByteArray; overload; virtual;

    function calibVersion(cparams: string):LongInt; overload; virtual;

    function calibScale(unit_name: string; sensorType: string):LongInt; overload; virtual;

    function calibOffset(unit_name: string):LongInt; overload; virtual;

    function calibConvert(param: string; currentFuncValue: string; unit_name: string; sensorType: string):string; overload; virtual;

    function _tryExec(url: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Restores all the settings of the device.
    /// <para>
    ///   Useful to restore all the logical names and calibrations parameters
    ///   of a module from a backup.Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modifications must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="settings">
    ///   a binary buffer with all the settings.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_allSettings(settings: TByteArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Adds a file to the uploaded data at the next HTTP callback.
    /// <para>
    ///   This function only affects the next HTTP callback and only works in
    ///   HTTP callback mode.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="filename">
    ///   the name of the file to upload at the next HTTP callback
    /// </param>
    /// <returns>
    ///   nothing.
    /// </returns>
    ///-
    function addFileToHTTPCallback(filename: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the unique hardware identifier of the module.
    /// <para>
    ///   The unique hardware identifier is made of the device serial
    ///   number followed by string ".module".
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string that uniquely identifies the module
    /// </returns>
    ///-
    function get_hardwareId():string; override;

    ////
    /// <summary>
    ///   Downloads the specified built-in file and returns a binary buffer with its content.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="pathname">
    ///   name of the new file to load
    /// </param>
    /// <returns>
    ///   a binary buffer with the file content
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns  <c>YAPI_INVALID_STRING</c>.
    /// </para>
    ///-
    function download(pathname: string):TByteArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns the icon of the module.
    /// <para>
    ///   The icon is a PNG image and does not
    ///   exceeds 1536 bytes.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a binary buffer with module icon, in png format.
    ///   On failure, throws an exception or returns  <c>YAPI_INVALID_STRING</c>.
    /// </returns>
    ///-
    function get_icon2d():TByteArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns a string with last logs of the module.
    /// <para>
    ///   This method return only
    ///   logs that are still in the module.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with last logs of the module.
    ///   On failure, throws an exception or returns  <c>YAPI_INVALID_STRING</c>.
    /// </returns>
    ///-
    function get_lastLogs():string; overload; virtual;

    ////
    /// <summary>
    ///   Adds a text message to the device logs.
    /// <para>
    ///   This function is useful in
    ///   particular to trace the execution of HTTP callbacks. If a newline
    ///   is desired after the message, it must be included in the string.
    /// </para>
    /// </summary>
    /// <param name="text">
    ///   the string to append to the logs.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function log(text: string):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list of all the modules that are plugged into the current module.
    /// <para>
    ///   This method only makes sense when called for a YoctoHub/VirtualHub.
    ///   Otherwise, an empty array will be returned.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an array of strings containing the sub modules.
    /// </returns>
    ///-
    function get_subDevices():TStringArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns the serial number of the YoctoHub on which this module is connected.
    /// <para>
    ///   If the module is connected by USB, or if the module is the root YoctoHub, an
    ///   empty string is returned.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with the serial number of the YoctoHub or an empty string
    /// </returns>
    ///-
    function get_parentHub():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the URL used to access the module.
    /// <para>
    ///   If the module is connected by USB, the
    ///   string 'usb' is returned.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string with the URL of the module.
    /// </returns>
    ///-
    function get_url():string; overload; virtual;


    ////
    /// <summary>
    ///   Continues the module enumeration started using <c>yFirstModule()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned modules order.
    ///   If you want to find a specific module, use <c>Module.findModule()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YModule</c> object, corresponding to
    ///   the next module found, or a <c>NIL</c> pointer
    ///   if there are no more modules to enumerate.
    /// </returns>
    ///-
    function nextModule():TYModule;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstModule():TYModule;
  //--- (end of generated code: YModule accessors declaration)
end;

  //--- (generated code: YSensor class start)
  TYSensorValueCallback = procedure(func: TYSensor; value:string);
  TYSensorTimedReportCallback = procedure(func: TYSensor; value:TYMeasure);

  ////
  /// <summary>
  ///   TYSensor Class: Sensor function interface.
  /// <para>
  /// </para>
  /// <para>
  ///   The <c>YSensor</c> class is the parent class for all Yoctopuce sensor types. It can be
  ///   used to read the current value and unit of any sensor, read the min/max
  ///   value, configure autonomous recording frequency and access recorded data.
  ///   It also provide a function to register a callback invoked each time the
  ///   observed value changes, or at a predefined interval. Using this class rather
  ///   than a specific subclass makes it possible to create generic applications
  ///   that work with any Yoctopuce sensor, even those that do not yet exist.
  ///   Note: The <c>YAnButton</c> class is the only analog input which does not inherit
  ///   from <c>YSensor</c>.
  /// </para>
  /// </summary>
  ///-
  TYSensor=class(TYFunction)
  //--- (end of generated code: YSensor class start)
  protected
  //--- (generated code: YSensor declaration)
    // Attributes (function value cache)
    _unit                     : string;
    _currentValue             : double;
    _lowestValue              : double;
    _highestValue             : double;
    _currentRawValue          : double;
    _logFrequency             : string;
    _reportFrequency          : string;
    _advMode                  : Integer;
    _calibrationParam         : string;
    _resolution               : double;
    _sensorState              : LongInt;
    _valueCallbackSensor      : TYSensorValueCallback;
    _timedReportCallbackSensor : TYSensorTimedReportCallback;
    _prevTimedReport          : double;
    _iresol                   : double;
    _offset                   : double;
    _scale                    : double;
    _decexp                   : double;
    _caltyp                   : LongInt;
    _calpar                   : TLongIntArray;
    _calraw                   : TDoubleArray;
    _calref                   : TDoubleArray;
    _calhdl                   : yCalibrationHandler;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YSensor declaration)


  public

    constructor Create(func:string); overload;
    //--- (generated code: YSensor accessors declaration)
    ////
    /// <summary>
    ///   Returns the measuring unit for the measure.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the measuring unit for the measure
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSensor.UNIT_INVALID</c>.
    /// </para>
    ///-
    function get_unit():string;

    ////
    /// <summary>
    ///   Returns the current value of the measure, in the specified unit, as a floating point number.
    /// <para>
    ///   Note that a get_currentValue() call will *not* start a measure in the device, it
    ///   will just return the last measure that occurred in the device. Indeed, internally, each Yoctopuce
    ///   devices is continuously making measurements at a hardware specific frequency.
    /// </para>
    /// <para>
    ///   If continuously calling  get_currentValue() leads you to performances issues, then
    ///   you might consider to switch to callback programming model. Check the "advanced
    ///   programming" chapter in in your device user manual for more information.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current value of the measure, in the specified unit,
    ///   as a floating point number
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSensor.CURRENTVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_currentValue():double;

    ////
    /// <summary>
    ///   Changes the recorded minimal value observed.
    /// <para>
    ///   Can be used to reset the value returned
    ///   by get_lowestValue().
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the recorded minimal value observed
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
    function set_lowestValue(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the minimal value observed for the measure since the device was started.
    /// <para>
    ///   Can be reset to an arbitrary value thanks to set_lowestValue().
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the minimal value observed for the measure since the device was started
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSensor.LOWESTVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_lowestValue():double;

    ////
    /// <summary>
    ///   Changes the recorded maximal value observed.
    /// <para>
    ///   Can be used to reset the value returned
    ///   by get_lowestValue().
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the recorded maximal value observed
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
    function set_highestValue(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the maximal value observed for the measure since the device was started.
    /// <para>
    ///   Can be reset to an arbitrary value thanks to set_highestValue().
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the maximal value observed for the measure since the device was started
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSensor.HIGHESTVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_highestValue():double;

    ////
    /// <summary>
    ///   Returns the uncalibrated, unrounded raw value returned by the
    ///   sensor, in the specified unit, as a floating point number.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the uncalibrated, unrounded raw value returned by the
    ///   sensor, in the specified unit, as a floating point number
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSensor.CURRENTRAWVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_currentRawValue():double;

    ////
    /// <summary>
    ///   Returns the datalogger recording frequency for this function, or "OFF"
    ///   when measures are not stored in the data logger flash memory.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the datalogger recording frequency for this function, or "OFF"
    ///   when measures are not stored in the data logger flash memory
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSensor.LOGFREQUENCY_INVALID</c>.
    /// </para>
    ///-
    function get_logFrequency():string;

    ////
    /// <summary>
    ///   Changes the datalogger recording frequency for this function.
    /// <para>
    ///   The frequency can be specified as samples per second,
    ///   as sample per minute (for instance "15/m") or in samples per
    ///   hour (eg. "4/h"). To disable recording for this function, use
    ///   the value "OFF". Note that setting the  datalogger recording frequency
    ///   to a greater value than the sensor native sampling frequency is useless,
    ///   and even counterproductive: those two frequencies are not related.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the datalogger recording frequency for this function
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
    function set_logFrequency(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the timed value notification frequency, or "OFF" if timed
    ///   value notifications are disabled for this function.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the timed value notification frequency, or "OFF" if timed
    ///   value notifications are disabled for this function
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSensor.REPORTFREQUENCY_INVALID</c>.
    /// </para>
    ///-
    function get_reportFrequency():string;

    ////
    /// <summary>
    ///   Changes the timed value notification frequency for this function.
    /// <para>
    ///   The frequency can be specified as samples per second,
    ///   as sample per minute (for instance "15/m") or in samples per
    ///   hour (e.g. "4/h"). To disable timed value notifications for this
    ///   function, use the value "OFF". Note that setting the  timed value
    ///   notification frequency to a greater value than the sensor native
    ///   sampling frequency is unless, and even counterproductive: those two
    ///   frequencies are not related.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a string corresponding to the timed value notification frequency for this function
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
    function set_reportFrequency(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the measuring mode used for the advertised value pushed to the parent hub.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YSensor.ADVMODE_IMMEDIATE</c>, <c>YSensor.ADVMODE_PERIOD_AVG</c>,
    ///   <c>YSensor.ADVMODE_PERIOD_MIN</c> and <c>YSensor.ADVMODE_PERIOD_MAX</c> corresponding to the
    ///   measuring mode used for the advertised value pushed to the parent hub
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSensor.ADVMODE_INVALID</c>.
    /// </para>
    ///-
    function get_advMode():Integer;

    ////
    /// <summary>
    ///   Changes the measuring mode used for the advertised value pushed to the parent hub.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YSensor.ADVMODE_IMMEDIATE</c>, <c>YSensor.ADVMODE_PERIOD_AVG</c>,
    ///   <c>YSensor.ADVMODE_PERIOD_MIN</c> and <c>YSensor.ADVMODE_PERIOD_MAX</c> corresponding to the
    ///   measuring mode used for the advertised value pushed to the parent hub
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
    function set_advMode(newval:Integer):integer;

    function get_calibrationParam():string;

    function set_calibrationParam(newval:string):integer;

    ////
    /// <summary>
    ///   Changes the resolution of the measured physical values.
    /// <para>
    ///   The resolution corresponds to the numerical precision
    ///   when displaying value. It does not change the precision of the measure itself.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a floating point number corresponding to the resolution of the measured physical values
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
    function set_resolution(newval:double):integer;

    ////
    /// <summary>
    ///   Returns the resolution of the measured values.
    /// <para>
    ///   The resolution corresponds to the numerical precision
    ///   of the measures, which is not always the same as the actual precision of the sensor.
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the resolution of the measured values
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSensor.RESOLUTION_INVALID</c>.
    /// </para>
    ///-
    function get_resolution():double;

    ////
    /// <summary>
    ///   Returns the sensor health state code, which is zero when there is an up-to-date measure
    ///   available or a positive code if the sensor is not able to provide a measure right now.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the sensor health state code, which is zero when there is an up-to-date measure
    ///   available or a positive code if the sensor is not able to provide a measure right now
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YSensor.SENSORSTATE_INVALID</c>.
    /// </para>
    ///-
    function get_sensorState():LongInt;

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
    ///   Use the method <c>YSensor.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YSensor</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindSensor(func: string):TYSensor;

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
    function registerValueCallback(callback: TYSensorValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function _parserHelper():LongInt; override;

    ////
    /// <summary>
    ///   Checks if the sensor is currently able to provide an up-to-date measure.
    /// <para>
    ///   Returns false if the device is unreachable, or if the sensor does not have
    ///   a current measure to transmit. No exception is raised if there is an error
    ///   while trying to contact the device hosting $THEFUNCTION$.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>true</c> if the sensor can provide an up-to-date measure, and <c>false</c> otherwise
    /// </returns>
    ///-
    function isSensorReady():boolean; overload; virtual;

    ////
    /// <summary>
    ///   Returns the <c>YDatalogger</c> object of the device hosting the sensor.
    /// <para>
    ///   This method returns an object
    ///   that can control global parameters of the data logger. The returned object
    ///   should not be freed.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an <c>YDatalogger</c> object, or NIL on error.
    /// </returns>
    ///-
    function get_dataLogger():TYDataLogger; overload; virtual;

    ////
    /// <summary>
    ///   Starts the data logger on the device.
    /// <para>
    ///   Note that the data logger
    ///   will only save the measures on this sensor if the logFrequency
    ///   is not set to "OFF".
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    ///-
    function startDataLogger():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Stops the datalogger on the device.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    ///-
    function stopDataLogger():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Retrieves a <c>YDataSet</c> object holding historical data for this
    ///   sensor, for a specified time interval.
    /// <para>
    ///   The measures will be
    ///   retrieved from the data logger, which must have been turned
    ///   on at the desired time. See the documentation of the <c>YDataSet</c>
    ///   class for information on how to get an overview of the
    ///   recorded data, and how to load progressively a large set
    ///   of measures from the data logger.
    /// </para>
    /// <para>
    ///   This function only works if the device uses a recent firmware,
    ///   as <c>YDataSet</c> objects are not supported by firmwares older than
    ///   version 13000.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="startTime">
    ///   the start of the desired measure time interval,
    ///   as a Unix timestamp, i.e. the number of seconds since
    ///   January 1, 1970 UTC. The special value 0 can be used
    ///   to include any measure, without initial limit.
    /// </param>
    /// <param name="endTime">
    ///   the end of the desired measure time interval,
    ///   as a Unix timestamp, i.e. the number of seconds since
    ///   January 1, 1970 UTC. The special value 0 can be used
    ///   to include any measure, without ending limit.
    /// </param>
    /// <returns>
    ///   an instance of <c>YDataSet</c>, providing access to historical
    ///   data. Past measures can be loaded progressively
    ///   using methods from the <c>YDataSet</c> object.
    /// </returns>
    ///-
    function get_recordedData(startTime: double; endTime: double):TYDataSet; overload; virtual;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every periodic timed notification.
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
    ///   arguments: the function object of which the value has changed, and an <c>YMeasure</c> object describing
    ///   the new advertised value.
    /// @noreturn
    /// </param>
    ///-
    function registerTimedReportCallback(callback: TYSensorTimedReportCallback):LongInt; overload; virtual;

    function _invokeTimedReportCallback(value: TYMeasure):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Configures error correction data points, in particular to compensate for
    ///   a possible perturbation of the measure caused by an enclosure.
    /// <para>
    ///   It is possible
    ///   to configure up to five correction points. Correction points must be provided
    ///   in ascending order, and be in the range of the sensor. The device will automatically
    ///   perform a linear interpolation of the error correction between specified
    ///   points. Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    ///   For more information on advanced capabilities to refine the calibration of
    ///   sensors, please contact support@yoctopuce.com.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="rawValues">
    ///   array of floating point numbers, corresponding to the raw
    ///   values returned by the sensor for the correction points.
    /// </param>
    /// <param name="refValues">
    ///   array of floating point numbers, corresponding to the corrected
    ///   values for the correction points.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function calibrateFromPoints(rawValues: TDoubleArray; refValues: TDoubleArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Retrieves error correction data points previously entered using the method
    ///   <c>calibrateFromPoints</c>.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="rawValues">
    ///   array of floating point numbers, that will be filled by the
    ///   function with the raw sensor values for the correction points.
    /// </param>
    /// <param name="refValues">
    ///   array of floating point numbers, that will be filled by the
    ///   function with the desired values for the correction points.
    /// </param>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function loadCalibrationPoints(var rawValues: TDoubleArray; var refValues: TDoubleArray):LongInt; overload; virtual;

    function _encodeCalibrationPoints(rawValues: TDoubleArray; refValues: TDoubleArray):string; overload; virtual;

    function _applyCalibration(rawValue: double):double; overload; virtual;

    function _decodeTimedReport(timestamp: double; duration: double; report: TLongIntArray):TYMeasure; overload; virtual;

    function _decodeVal(w: LongInt):double; overload; virtual;

    function _decodeAvg(dw: LongInt):double; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of sensors started using <c>yFirstSensor()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned sensors order.
    ///   If you want to find a specific a sensor, use <c>Sensor.findSensor()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YSensor</c> object, corresponding to
    ///   a sensor currently online, or a <c>NIL</c> pointer
    ///   if there are no more sensors to enumerate.
    /// </returns>
    ///-
    function nextSensor():TYSensor;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstSensor():TYSensor;
  //--- (end of generated code: YSensor accessors declaration)
  end;

//--- (generated code: YAPIContext class start)
  ////
  /// <summary>
  ///   TYAPIContext Class: Yoctopuce I/O context configuration.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  ///-
  TYAPIContext=class(TObject)
  //--- (end of generated code: YAPIContext class start)
  protected
  //--- (generated code: YAPIContext declaration)
    // Attributes (function value cache)
    _defaultCacheValidity     : u64;

    //--- (end of generated code: YAPIContext declaration)
  public
   constructor Create();
    //--- (generated code: YAPIContext accessors declaration)
    ////
    /// <summary>
    ///   Modifies the delay between each forced enumeration of the used YoctoHubs.
    /// <para>
    ///   By default, the library performs a full enumeration every 10 seconds.
    ///   To reduce network traffic, you can increase this delay.
    ///   It's particularly useful when a YoctoHub is connected to the GSM network
    ///   where traffic is billed. This parameter doesn't impact modules connected by USB,
    ///   nor the working of module arrival/removal callbacks.
    ///   Note: you must call this function after <c>yInitAPI</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="deviceListValidity">
    ///   nubmer of seconds between each enumeration.
    /// @noreturn
    /// </param>
    ///-
    procedure SetDeviceListValidity(deviceListValidity: LongInt); overload; virtual;

    ////
    /// <summary>
    ///   Returns the delay between each forced enumeration of the used YoctoHubs.
    /// <para>
    ///   Note: you must call this function after <c>yInitAPI</c>.
    /// </para>
    /// </summary>
    /// <returns>
    ///   the number of seconds between each enumeration.
    /// </returns>
    ///-
    function GetDeviceListValidity():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Adds a UDEV rule which authorizes all users to access Yoctopuce modules
    ///   connected to the USB ports.
    /// <para>
    ///   This function works only under Linux. The process that
    ///   calls this method must have root privileges because this method changes the Linux configuration.
    /// </para>
    /// </summary>
    /// <param name="force">
    ///   if true, overwrites any existing rule.
    /// </param>
    /// <returns>
    ///   an empty string if the rule has been added.
    /// </returns>
    /// <para>
    ///   On failure, returns a string that starts with "error:".
    /// </para>
    ///-
    function AddUdevRule(force: boolean):string; overload; virtual;

    ////
    /// <summary>
    ///   Modifies the network connection delay for <c>yRegisterHub()</c> and <c>yUpdateDeviceList()</c>.
    /// <para>
    ///   This delay impacts only the YoctoHubs and VirtualHub
    ///   which are accessible through the network. By default, this delay is of 20000 milliseconds,
    ///   but depending or you network you may want to change this delay,
    ///   gor example if your network infrastructure is based on a GSM connection.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="networkMsTimeout">
    ///   the network connection delay in milliseconds.
    /// @noreturn
    /// </param>
    ///-
    procedure SetNetworkTimeout(networkMsTimeout: LongInt); overload; virtual;

    ////
    /// <summary>
    ///   Returns the network connection delay for <c>yRegisterHub()</c> and <c>yUpdateDeviceList()</c>.
    /// <para>
    ///   This delay impacts only the YoctoHubs and VirtualHub
    ///   which are accessible through the network. By default, this delay is of 20000 milliseconds,
    ///   but depending or you network you may want to change this delay,
    ///   for example if your network infrastructure is based on a GSM connection.
    /// </para>
    /// </summary>
    /// <returns>
    ///   the network connection delay in milliseconds.
    /// </returns>
    ///-
    function GetNetworkTimeout():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Change the validity period of the data loaded by the library.
    /// <para>
    ///   By default, when accessing a module, all the attributes of the
    ///   module functions are automatically kept in cache for the standard
    ///   duration (5 ms). This method can be used to change this standard duration,
    ///   for example in order to reduce network or USB traffic. This parameter
    ///   does not affect value change callbacks
    ///   Note: This function must be called after <c>yInitAPI</c>.
    /// </para>
    /// </summary>
    /// <param name="cacheValidityMs">
    ///   an integer corresponding to the validity attributed to the
    ///   loaded function parameters, in milliseconds.
    /// @noreturn
    /// </param>
    ///-
    procedure SetCacheValidity(cacheValidityMs: u64); overload; virtual;

    ////
    /// <summary>
    ///   Returns the validity period of the data loaded by the library.
    /// <para>
    ///   This method returns the cache validity of all attributes
    ///   module functions.
    ///   Note: This function must be called after <c>yInitAPI </c>.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the validity attributed to the
    ///   loaded function parameters, in milliseconds
    /// </returns>
    ///-
    function GetCacheValidity():u64; overload; virtual;


  //--- (end of generated code: YAPIContext accessors declaration)
  end;

  //--- (generated code: YFirmwareUpdate class start)
  ////
  /// <summary>
  ///   TYFirmwareUpdate Class: Firmware update process control interface, returned by <c>module.updateFirmware</c> method.
  /// <para>
  /// </para>
  /// <para>
  ///   The <c>YFirmwareUpdate</c> class let you control the firmware update of a Yoctopuce
  ///   module. This class should not be instantiate directly, but instances should be retrieved
  ///   using the <c>YModule</c> method <c>module.updateFirmware</c>.
  /// </para>
  /// </summary>
  ///-
  TYFirmwareUpdate=class(TObject)
  //--- (end of generated code: YFirmwareUpdate class start)
  protected
  //--- (generated code: YFirmwareUpdate declaration)
    // Attributes (function value cache)
    _serial                   : string;
    _settings                 : TByteArray;
    _firmwarepath             : string;
    _progress_msg             : string;
    _progress_c               : LongInt;
    _progress                 : LongInt;
    _restore_step             : LongInt;
    _force                    : boolean;

    //--- (end of generated code: YFirmwareUpdate declaration)

  public
    constructor Create(serial: String; path :string; settings :TByteArray; force :boolean);

  //--- (generated code: YFirmwareUpdate accessors declaration)
    function _processMore(newupdate: LongInt):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list of all the modules in "firmware update" mode.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an array of strings containing the serial numbers of devices in "firmware update" mode.
    /// </returns>
    ///-
    class function GetAllBootLoaders():TStringArray;

    ////
    /// <summary>
    ///   Test if the byn file is valid for this module.
    /// <para>
    ///   It is possible to pass a directory instead of a file.
    ///   In that case, this method returns the path of the most recent appropriate byn file. This method will
    ///   ignore any firmware older than minrelease.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="serial">
    ///   the serial number of the module to update
    /// </param>
    /// <param name="path">
    ///   the path of a byn file or a directory that contains byn files
    /// </param>
    /// <param name="minrelease">
    ///   a positive integer
    /// </param>
    /// <returns>
    ///   : the path of the byn file to use, or an empty string if no byn files matches the requirement
    /// </returns>
    /// <para>
    ///   On failure, returns a string that starts with "error:".
    /// </para>
    ///-
    class function CheckFirmware(serial: string; path: string; minrelease: LongInt):string;

    ////
    /// <summary>
    ///   Returns the progress of the firmware update, on a scale from 0 to 100.
    /// <para>
    ///   When the object is
    ///   instantiated, the progress is zero. The value is updated during the firmware update process until
    ///   the value of 100 is reached. The 100 value means that the firmware update was completed
    ///   successfully. If an error occurs during the firmware update, a negative value is returned, and the
    ///   error message can be retrieved with <c>get_progressMessage</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer in the range 0 to 100 (percentage of completion)
    ///   or a negative error code in case of failure.
    /// </returns>
    ///-
    function get_progress():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the last progress message of the firmware update process.
    /// <para>
    ///   If an error occurs during the
    ///   firmware update process, the error message is returned
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string  with the latest progress message, or the error message.
    /// </returns>
    ///-
    function get_progressMessage():string; overload; virtual;

    ////
    /// <summary>
    ///   Starts the firmware update process.
    /// <para>
    ///   This method starts the firmware update process in background. This method
    ///   returns immediately. You can monitor the progress of the firmware update with the <c>get_progress()</c>
    ///   and <c>get_progressMessage()</c> methods.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer in the range 0 to 100 (percentage of completion),
    ///   or a negative error code in case of failure.
    /// </returns>
    /// <para>
    ///   On failure returns a negative error code.
    /// </para>
    ///-
    function startUpdate():LongInt; overload; virtual;


  //--- (end of generated code: YFirmwareUpdate accessors declaration)
  end;


//--- (generated code: YDataStream class start)
  ////
  /// <summary>
  ///   TYDataStream Class: Unformatted data sequence
  /// <para>
  ///   <c>DataStream</c> objects represent bare recorded measure sequences,
  ///   exactly as found within the data logger present on Yoctopuce
  ///   sensors.
  /// </para>
  /// <para>
  ///   In most cases, it is not necessary to use <c>DataStream</c> objects
  ///   directly, as the <c>DataSet</c> objects (returned by the
  ///   <c>get_recordedData()</c> method from sensors and the
  ///   <c>get_dataSets()</c> method from the data logger) provide
  ///   a more convenient interface.
  /// </para>
  /// </summary>
  ///-
  TYDataStream=class(TObject)
  //--- (end of generated code: YDataStream class start)
  protected
  //--- (generated code: YDataStream declaration)
    // Attributes (function value cache)
    _parent                   : TYFunction;
    _runNo                    : LongInt;
    _utcStamp                 : int64;
    _nCols                    : LongInt;
    _nRows                    : LongInt;
    _startTime                : double;
    _duration                 : double;
    _dataSamplesInterval      : double;
    _firstMeasureDuration     : double;
    _columnNames              : TStringArray;
    _functionId               : string;
    _isClosed                 : boolean;
    _isAvg                    : boolean;
    _minVal                   : double;
    _avgVal                   : double;
    _maxVal                   : double;
    _caltyp                   : LongInt;
    _calpar                   : TLongIntArray;
    _calraw                   : TDoubleArray;
    _calref                   : TDoubleArray;
    _values                   : TDoubleArrayArray;
    _isLoaded                 : boolean;

    //--- (end of generated code: YDataStream declaration)
    _calhdl                  : yCalibrationHandler;
  public
    constructor Create(parent:TYFunction); Overload;
    constructor Create(parent:TYFunction; dataset:TYDataSet; encoded:TLongIntArray); Overload;


  //--- (generated code: YDataStream accessors declaration)
    function _initFromDataSet(dataset: TYDataSet; encoded: TLongIntArray):LongInt; overload; virtual;

    function _parseStream(sdata: TByteArray):LongInt; overload; virtual;

    function _wasLoaded():boolean; overload; virtual;

    function _get_url():string; overload; virtual;

    function _get_baseurl():string; overload; virtual;

    function _get_urlsuffix():string; overload; virtual;

    function loadStream():LongInt; overload; virtual;

    function _decodeVal(w: LongInt):double; overload; virtual;

    function _decodeAvg(dw: LongInt; count: LongInt):double; overload; virtual;

    function isClosed():boolean; overload; virtual;

    ////
    /// <summary>
    ///   Returns the run index of the data stream.
    /// <para>
    ///   A run can be made of
    ///   multiple datastreams, for different time intervals.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an unsigned number corresponding to the run index.
    /// </returns>
    ///-
    function get_runIndex():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the relative start time of the data stream, measured in seconds.
    /// <para>
    ///   For recent firmwares, the value is relative to the present time,
    ///   which means the value is always negative.
    ///   If the device uses a firmware older than version 13000, value is
    ///   relative to the start of the time the device was powered on, and
    ///   is always positive.
    ///   If you need an absolute UTC timestamp, use <c>get_realStartTimeUTC()</c>.
    /// </para>
    /// <para>
    ///   <b>DEPRECATED</b>: This method has been replaced by <c>get_realStartTimeUTC()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an unsigned number corresponding to the number of seconds
    ///   between the start of the run and the beginning of this data
    ///   stream.
    /// </returns>
    ///-
    function get_startTime():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the start time of the data stream, relative to the Jan 1, 1970.
    /// <para>
    ///   If the UTC time was not set in the datalogger at the time of the recording
    ///   of this data stream, this method returns 0.
    /// </para>
    /// <para>
    ///   <b>DEPRECATED</b>: This method has been replaced by <c>get_realStartTimeUTC()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an unsigned number corresponding to the number of seconds
    ///   between the Jan 1, 1970 and the beginning of this data
    ///   stream (i.e. Unix time representation of the absolute time).
    /// </returns>
    ///-
    function get_startTimeUTC():int64; overload; virtual;

    ////
    /// <summary>
    ///   Returns the start time of the data stream, relative to the Jan 1, 1970.
    /// <para>
    ///   If the UTC time was not set in the datalogger at the time of the recording
    ///   of this data stream, this method returns 0.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number  corresponding to the number of seconds
    ///   between the Jan 1, 1970 and the beginning of this data
    ///   stream (i.e. Unix time representation of the absolute time).
    /// </returns>
    ///-
    function get_realStartTimeUTC():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the number of milliseconds between two consecutive
    ///   rows of this data stream.
    /// <para>
    ///   By default, the data logger records one row
    ///   per second, but the recording frequency can be changed for
    ///   each device function
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an unsigned number corresponding to a number of milliseconds.
    /// </returns>
    ///-
    function get_dataSamplesIntervalMs():LongInt; overload; virtual;

    function get_dataSamplesInterval():double; overload; virtual;

    function get_firstDataSamplesInterval():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the number of data rows present in this stream.
    /// <para>
    /// </para>
    /// <para>
    ///   If the device uses a firmware older than version 13000,
    ///   this method fetches the whole data stream from the device
    ///   if not yet done, which can cause a little delay.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an unsigned number corresponding to the number of rows.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns zero.
    /// </para>
    ///-
    function get_rowCount():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the number of data columns present in this stream.
    /// <para>
    ///   The meaning of the values present in each column can be obtained
    ///   using the method <c>get_columnNames()</c>.
    /// </para>
    /// <para>
    ///   If the device uses a firmware older than version 13000,
    ///   this method fetches the whole data stream from the device
    ///   if not yet done, which can cause a little delay.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an unsigned number corresponding to the number of columns.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns zero.
    /// </para>
    ///-
    function get_columnCount():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns the title (or meaning) of each data column present in this stream.
    /// <para>
    ///   In most case, the title of the data column is the hardware identifier
    ///   of the sensor that produced the data. For streams recorded at a lower
    ///   recording rate, the dataLogger stores the min, average and max value
    ///   during each measure interval into three columns with suffixes _min,
    ///   _avg and _max respectively.
    /// </para>
    /// <para>
    ///   If the device uses a firmware older than version 13000,
    ///   this method fetches the whole data stream from the device
    ///   if not yet done, which can cause a little delay.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list containing as many strings as there are columns in the
    ///   data stream.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_columnNames():TStringArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns the smallest measure observed within this stream.
    /// <para>
    ///   If the device uses a firmware older than version 13000,
    ///   this method will always return Y_DATA_INVALID.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to the smallest value,
    ///   or Y_DATA_INVALID if the stream is not yet complete (still recording).
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns Y_DATA_INVALID.
    /// </para>
    ///-
    function get_minValue():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the average of all measures observed within this stream.
    /// <para>
    ///   If the device uses a firmware older than version 13000,
    ///   this method will always return Y_DATA_INVALID.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to the average value,
    ///   or Y_DATA_INVALID if the stream is not yet complete (still recording).
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns Y_DATA_INVALID.
    /// </para>
    ///-
    function get_averageValue():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the largest measure observed within this stream.
    /// <para>
    ///   If the device uses a firmware older than version 13000,
    ///   this method will always return Y_DATA_INVALID.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to the largest value,
    ///   or Y_DATA_INVALID if the stream is not yet complete (still recording).
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns Y_DATA_INVALID.
    /// </para>
    ///-
    function get_maxValue():double; overload; virtual;

    function get_realDuration():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the whole data set contained in the stream, as a bidimensional
    ///   table of numbers.
    /// <para>
    ///   The meaning of the values present in each column can be obtained
    ///   using the method <c>get_columnNames()</c>.
    /// </para>
    /// <para>
    ///   This method fetches the whole data stream from the device,
    ///   if not yet done.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list containing as many elements as there are rows in the
    ///   data stream. Each row itself is a list of floating-point
    ///   numbers.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_dataRows():TDoubleArrayArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns a single measure from the data stream, specified by its
    ///   row and column index.
    /// <para>
    ///   The meaning of the values present in each column can be obtained
    ///   using the method get_columnNames().
    /// </para>
    /// <para>
    ///   This method fetches the whole data stream from the device,
    ///   if not yet done.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="row">
    ///   row index
    /// </param>
    /// <param name="col">
    ///   column index
    /// </param>
    /// <returns>
    ///   a floating-point number
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns Y_DATA_INVALID.
    /// </para>
    ///-
    function get_data(row: LongInt; col: LongInt):double; overload; virtual;


  //--- (end of generated code: YDataStream accessors declaration)
  end;

  //--- (generated code: YMeasure class start)
  ////
  /// <summary>
  ///   TYMeasure Class: Measured value, returned in particular by the methods of the <c>YDataSet</c> class.
  /// <para>
  /// </para>
  /// <para>
  ///   <c>YMeasure</c> objects are used within the API to represent
  ///   a value measured at a specified time. These objects are
  ///   used in particular in conjunction with the <c>YDataSet</c> class,
  ///   but also for sensors periodic timed reports
  ///   (see <c>sensor.registerTimedReportCallback</c>).
  /// </para>
  /// </summary>
  ///-
  TYMeasure=class(TObject)
  //--- (end of generated code: YMeasure class start)
  protected
  //--- (generated code: YMeasure declaration)
    // Attributes (function value cache)
    _start                    : double;
    _end                      : double;
    _minVal                   : double;
    _avgVal                   : double;
    _maxVal                   : double;

    //--- (end of generated code: YMeasure declaration)
    _startDateTime           : TDateTime;
    _endDateTime             : TDateTime;



  public
    constructor Create(start, endt, minVal, avgVal, maxVal :double);

    function get_startTimeUTC_asTDateTime():TDateTime;
    function get_endTimeUTC_asTDateTime():TDateTime;

  //--- (generated code: YMeasure accessors declaration)
    ////
    /// <summary>
    ///   Returns the start time of the measure, relative to the Jan 1, 1970 UTC
    ///   (Unix timestamp).
    /// <para>
    ///   When the recording rate is higher then 1 sample
    ///   per second, the timestamp may have a fractional part.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the number of seconds
    ///   between the Jan 1, 1970 UTC and the beginning of this measure.
    /// </returns>
    ///-
    function get_startTimeUTC():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the end time of the measure, relative to the Jan 1, 1970 UTC
    ///   (Unix timestamp).
    /// <para>
    ///   When the recording rate is higher than 1 sample
    ///   per second, the timestamp may have a fractional part.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the number of seconds
    ///   between the Jan 1, 1970 UTC and the end of this measure.
    /// </returns>
    ///-
    function get_endTimeUTC():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the smallest value observed during the time interval
    ///   covered by this measure.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to the smallest value observed.
    /// </returns>
    ///-
    function get_minValue():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the average value observed during the time interval
    ///   covered by this measure.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to the average value observed.
    /// </returns>
    ///-
    function get_averageValue():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the largest value observed during the time interval
    ///   covered by this measure.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating-point number corresponding to the largest value observed.
    /// </returns>
    ///-
    function get_maxValue():double; overload; virtual;


  //--- (end of generated code: YMeasure accessors declaration)
end;

  //--- (generated code: YDataSet class start)
  ////
  /// <summary>
  ///   T
  /// <para>
  ///   YDataSet Class: Recorded data sequence, as returned by <c>sensor.get_recordedData()</c>
  /// </para>
  /// <para>
  ///   <c>YDataSet</c> objects make it possible to retrieve a set of recorded measures
  ///   for a given sensor and a specified time interval. They can be used
  ///   to load data points with a progress report. When the <c>YDataSet</c> object is
  ///   instantiated by the <c>sensor.get_recordedData()</c>  function, no data is
  ///   yet loaded from the module. It is only when the <c>loadMore()</c>
  ///   method is called over and over than data will be effectively loaded
  ///   from the dataLogger.
  /// </para>
  /// <para>
  ///   A preview of available measures is available using the function
  ///   <c>get_preview()</c> as soon as <c>loadMore()</c> has been called
  ///   once. Measures themselves are available using function <c>get_measures()</c>
  ///   when loaded by subsequent calls to <c>loadMore()</c>.
  /// </para>
  /// <para>
  ///   This class can only be used on devices that use a relatively recent firmware,
  ///   as <c>YDataSet</c> objects are not supported by firmwares older than version 13000.
  /// </para>
  /// </summary>
  ///-
  TYDataSet=class(TObject)
  //--- (end of generated code: YDataSet class start)
  protected
  //--- (generated code: YDataSet declaration)
    // Attributes (function value cache)
    _parent                   : TYFunction;
    _hardwareId               : string;
    _functionId               : string;
    _unit                     : string;
    _bulkLoad                 : LongInt;
    _startTimeMs              : double;
    _endTimeMs                : double;
    _progress                 : LongInt;
    _calib                    : TLongIntArray;
    _streams                  : TYDataStreamArray;
    _summary                  : TYMeasure;
    _preview                  : TYMeasureArray;
    _measures                 : TYMeasureArray;
    _summaryMinVal            : double;
    _summaryMaxVal            : double;
    _summaryTotalAvg          : double;
    _summaryTotalTime         : double;

    //--- (end of generated code: YDataSet declaration)


  public

    constructor Create(parent:TYFunction; functionId,func_unit:string; startTime,endTime: double); Overload;

    constructor Create(parent:TYFunction); Overload;

    destructor Destroy();override;

    function _parse(data:string):integer;

  //--- (generated code: YDataSet accessors declaration)
    function _get_calibration():TLongIntArray; overload; virtual;

    function loadSummary(data: TByteArray):LongInt; overload; virtual;

    function processMore(progress: LongInt; data: TByteArray):LongInt; overload; virtual;

    function get_privateDataStreams():TYDataStreamArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns the unique hardware identifier of the function who performed the measures,
    ///   in the form <c>SERIAL.FUNCTIONID</c>.
    /// <para>
    ///   The unique hardware identifier is composed of the
    ///   device serial number and of the hardware identifier of the function
    ///   (for example <c>THRMCPL1-123456.temperature1</c>)
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string that uniquely identifies the function (ex: <c>THRMCPL1-123456.temperature1</c>)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns  <c>YDataSet.HARDWAREID_INVALID</c>.
    /// </para>
    ///-
    function get_hardwareId():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the hardware identifier of the function that performed the measure,
    ///   without reference to the module.
    /// <para>
    ///   For example <c>temperature1</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string that identifies the function (ex: <c>temperature1</c>)
    /// </returns>
    ///-
    function get_functionId():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the measuring unit for the measured value.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string that represents a physical unit.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns  <c>YDataSet.UNIT_INVALID</c>.
    /// </para>
    ///-
    function get_unit():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the start time of the dataset, relative to the Jan 1, 1970.
    /// <para>
    ///   When the <c>YDataSet</c> object is created, the start time is the value passed
    ///   in parameter to the <c>get_dataSet()</c> function. After the
    ///   very first call to <c>loadMore()</c>, the start time is updated
    ///   to reflect the timestamp of the first measure actually found in the
    ///   dataLogger within the specified range.
    /// </para>
    /// <para>
    ///   <b>DEPRECATED</b>: This method has been replaced by <c>get_summary()</c>
    ///   which contain more precise informations.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an unsigned number corresponding to the number of seconds
    ///   between the Jan 1, 1970 and the beginning of this data
    ///   set (i.e. Unix time representation of the absolute time).
    /// </returns>
    ///-
    function get_startTimeUTC():int64; overload; virtual;

    function imm_get_startTimeUTC():int64; overload; virtual;

    ////
    /// <summary>
    ///   Returns the end time of the dataset, relative to the Jan 1, 1970.
    /// <para>
    ///   When the <c>YDataSet</c> object is created, the end time is the value passed
    ///   in parameter to the <c>get_dataSet()</c> function. After the
    ///   very first call to <c>loadMore()</c>, the end time is updated
    ///   to reflect the timestamp of the last measure actually found in the
    ///   dataLogger within the specified range.
    /// </para>
    /// <para>
    ///   <b>DEPRECATED</b>: This method has been replaced by <c>get_summary()</c>
    ///   which contain more precise informations.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an unsigned number corresponding to the number of seconds
    ///   between the Jan 1, 1970 and the end of this data
    ///   set (i.e. Unix time representation of the absolute time).
    /// </returns>
    ///-
    function get_endTimeUTC():int64; overload; virtual;

    function imm_get_endTimeUTC():int64; overload; virtual;

    ////
    /// <summary>
    ///   Returns the progress of the downloads of the measures from the data logger,
    ///   on a scale from 0 to 100.
    /// <para>
    ///   When the object is instantiated by <c>get_dataSet</c>,
    ///   the progress is zero. Each time <c>loadMore()</c> is invoked, the progress
    ///   is updated, to reach the value 100 only once all measures have been loaded.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer in the range 0 to 100 (percentage of completion).
    /// </returns>
    ///-
    function get_progress():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Loads the next block of measures from the dataLogger, and updates
    ///   the progress indicator.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer in the range 0 to 100 (percentage of completion),
    ///   or a negative error code in case of failure.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function loadMore():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns an <c>YMeasure</c> object which summarizes the whole
    ///   <c>YDataSet</c>.
    /// <para>
    ///   In includes the following information:
    ///   - the start of a time interval
    ///   - the end of a time interval
    ///   - the minimal value observed during the time interval
    ///   - the average value observed during the time interval
    ///   - the maximal value observed during the time interval
    /// </para>
    /// <para>
    ///   This summary is available as soon as <c>loadMore()</c> has
    ///   been called for the first time.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an <c>YMeasure</c> object
    /// </returns>
    ///-
    function get_summary():TYMeasure; overload; virtual;

    ////
    /// <summary>
    ///   Returns a condensed version of the measures that can
    ///   retrieved in this <c>YDataSet</c>, as a list of <c>YMeasure</c>
    ///   objects.
    /// <para>
    ///   Each item includes:
    ///   - the start of a time interval
    ///   - the end of a time interval
    ///   - the minimal value observed during the time interval
    ///   - the average value observed during the time interval
    ///   - the maximal value observed during the time interval
    /// </para>
    /// <para>
    ///   This preview is available as soon as <c>loadMore()</c> has
    ///   been called for the first time.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a table of records, where each record depicts the
    ///   measured values during a time interval
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_preview():TYMeasureArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns the detailed set of measures for the time interval corresponding
    ///   to a given condensed measures previously returned by <c>get_preview()</c>.
    /// <para>
    ///   The result is provided as a list of <c>YMeasure</c> objects.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="measure">
    ///   condensed measure from the list previously returned by
    ///   <c>get_preview()</c>.
    /// </param>
    /// <returns>
    ///   a table of records, where each record depicts the
    ///   measured values during a time interval
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_measuresAt(measure: TYMeasure):TYMeasureArray; overload; virtual;

    ////
    /// <summary>
    ///   Returns all measured values currently available for this DataSet,
    ///   as a list of <c>YMeasure</c> objects.
    /// <para>
    ///   Each item includes:
    ///   - the start of the measure time interval
    ///   - the end of the measure time interval
    ///   - the minimal value observed during the time interval
    ///   - the average value observed during the time interval
    ///   - the maximal value observed during the time interval
    /// </para>
    /// <para>
    ///   Before calling this method, you should call <c>loadMore()</c>
    ///   to load data from the device. You may have to call loadMore()
    ///   several time until all rows are loaded, but you can start
    ///   looking at available data rows before the load is complete.
    /// </para>
    /// <para>
    ///   The oldest measures are always loaded first, and the most
    ///   recent measures will be loaded last. As a result, timestamps
    ///   are normally sorted in ascending order within the measure table,
    ///   unless there was an unexpected adjustment of the datalogger UTC
    ///   clock.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a table of records, where each record depicts the
    ///   measured value for a given time interval
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty array.
    /// </para>
    ///-
    function get_measures():TYMeasureArray; overload; virtual;


  //--- (end of generated code: YDataSet accessors declaration)
end;

  //--- (generated code: YConsolidatedDataSet class start)
  ////
  /// <summary>
  ///   TYConsolidatedDataSet Class: Cross-sensor consolidated data sequence.
  /// <para>
  /// </para>
  /// <para>
  ///   <c>YConsolidatedDataSet</c> objects make it possible to retrieve a set of
  ///   recorded measures from multiple sensors, for a specified time interval.
  ///   They can be used to load data points progressively, and to receive
  ///   data records by timestamp, one by one..
  /// </para>
  /// </summary>
  ///-
  TYConsolidatedDataSet=class(TObject)
  //--- (end of generated code: YConsolidatedDataSet class start)
  protected
  //--- (generated code: YConsolidatedDataSet declaration)
    // Attributes (function value cache)
    _start                    : double;
    _end                      : double;
    _nsensors                 : LongInt;
    _sensors                  : TYSensorArray;
    _datasets                 : TYDataSetArray;
    _progresss                : TLongIntArray;
    _nextidx                  : TLongIntArray;
    _nexttim                  : TDoubleArray;

    //--- (end of generated code: YConsolidatedDataSet declaration)


  public

    constructor Create(startTime,endTime: double; sensorList: TYSensorArray); Overload;

    destructor Destroy();override;

  //--- (generated code: YConsolidatedDataSet accessors declaration)
    function imm_init(startt: double; endt: double; sensorList: TYSensorArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns an object holding historical data for multiple
    ///   sensors, for a specified time interval.
    /// <para>
    ///   The measures will be retrieved from the data logger, which must have been turned
    ///   on at the desired time. The resulting object makes it possible to load progressively
    ///   a large set of measures from multiple sensors, consolidating data on the fly
    ///   to align records based on measurement timestamps.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="sensorNames">
    ///   array of logical names or hardware identifiers of the sensors
    ///   for which data must be loaded from their data logger.
    /// </param>
    /// <param name="startTime">
    ///   the start of the desired measure time interval,
    ///   as a Unix timestamp, i.e. the number of seconds since
    ///   January 1, 1970 UTC. The special value 0 can be used
    ///   to include any measure, without initial limit.
    /// </param>
    /// <param name="endTime">
    ///   the end of the desired measure time interval,
    ///   as a Unix timestamp, i.e. the number of seconds since
    ///   January 1, 1970 UTC. The special value 0 can be used
    ///   to include any measure, without ending limit.
    /// </param>
    /// <returns>
    ///   an instance of <c>YConsolidatedDataSet</c>, providing access to
    ///   consolidated historical data. Records can be loaded progressively
    ///   using the <c>YConsolidatedDataSet.nextRecord()</c> method.
    /// </returns>
    ///-
    class function Init(sensorNames: TStringArray; startTime: double; endTime: double):TYConsolidatedDataSet;

    ////
    /// <summary>
    ///   Extracts the next data record from the data logger of all sensors linked to this
    ///   object.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="datarec">
    ///   array of floating point numbers, that will be filled by the
    ///   function with the timestamp of the measure in first position,
    ///   followed by the measured value in next positions.
    /// </param>
    /// <returns>
    ///   an integer in the range 0 to 100 (percentage of completion),
    ///   or a negative error code in case of failure.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function nextRecord(var datarec: TDoubleArray):LongInt; overload; virtual;


  //--- (end of generated code: YConsolidatedDataSet accessors declaration)
end;


  TYOldDataStream=class(TYDataStream)
  protected
    _dataLogger   : TYDataLogger;
    _timeStamp, _interval :longword;
  public
    constructor create(parent: TYDataLogger; run, stamp, utc, itv: longword);
    destructor  Destroy();  override;

    function loadStream():integer; override;



    ////
    /// <summary>
    ///   Returns the relative start time of the data stream, measured in seconds.
    /// <para>
    ///   For recent firmwares, the value is relative to the present time,
    ///   which means the value is always negative.
    ///   If the device uses a firmware older than version 13000, value is
    ///   relative to the start of the time the device was powered on, and
    ///   is always positive.
    ///   If you need an absolute UTC timestamp, use <c>get_startTimeUTC()</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an unsigned number corresponding to the number of seconds
    ///   between the start of the run and the beginning of this data
    ///   stream.
    /// </returns>
    ///-
    function   get_startTime():LongInt; override;

    ////
    /// <summary>
    ///   Returns the number of seconds elapsed between  two consecutive
    ///   rows of this data stream.
    /// <para>
    ///   By default, the data logger records one row
    ///   per second, but there might be alternative streams at lower resolution
    ///   created by summarizing the original stream for archiving purposes.
    /// </para>
    /// <para>
    ///   This method does not cause any access to the device, as the value
    ///   is preloaded in the object at instantiation time.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an unsigned number corresponding to a number of seconds.
    /// </returns>
    ///-
    function     get_dataSamplesInterval():double; override;

  end;

  //--- (generated code: YDataLogger class start)
  TYDataLoggerValueCallback = procedure(func: TYDataLogger; value:string);
  TYDataLoggerTimedReportCallback = procedure(func: TYDataLogger; value:TYMeasure);

  ////
  /// <summary>
  ///   TYDataLogger Class: DataLogger control interface, available on most Yoctopuce sensors.
  /// <para>
  /// </para>
  /// <para>
  ///   A non-volatile memory for storing ongoing measured data is available on most Yoctopuce
  ///   sensors. Recording can happen automatically, without requiring a permanent
  ///   connection to a computer.
  ///   The <c>YDataLogger</c> class controls the global parameters of the internal data
  ///   logger. Recording control (start/stop) as well as data retreival is done at
  ///   sensor objects level.
  /// </para>
  /// </summary>
  ///-
  TYDataLogger=class(TYFunction)
  //--- (end of generated code: YDataLogger class start)
  protected
  //--- (generated code: YDataLogger declaration)
    // Attributes (function value cache)
    _currentRunIndex          : LongInt;
    _timeUTC                  : int64;
    _recording                : Integer;
    _autoStart                : Integer;
    _beaconDriven             : Integer;
    _usage                    : LongInt;
    _clearHistory             : Integer;
    _valueCallbackDataLogger  : TYDataLoggerValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YDataLogger declaration)
  protected
    _dataLoggerURL :string;

    function getData(runIdx: longword  ; timeIdx: longword; var jsondata:TJsonParser):integer;
  public

    function  get_dataStreams(v:Tlist):integer;

   //--- (generated code: YDataLogger accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the current run number, corresponding to the number of times the module was
    ///   powered on with the dataLogger enabled at some point.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the current run number, corresponding to the number of times the module was
    ///   powered on with the dataLogger enabled at some point
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDataLogger.CURRENTRUNINDEX_INVALID</c>.
    /// </para>
    ///-
    function get_currentRunIndex():LongInt;

    ////
    /// <summary>
    ///   Returns the Unix timestamp for current UTC time, if known.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the Unix timestamp for current UTC time, if known
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDataLogger.TIMEUTC_INVALID</c>.
    /// </para>
    ///-
    function get_timeUTC():int64;

    ////
    /// <summary>
    ///   Changes the current UTC time reference used for recorded data.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   an integer corresponding to the current UTC time reference used for recorded data
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
    function set_timeUTC(newval:int64):integer;

    ////
    /// <summary>
    ///   Returns the current activation state of the data logger.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a value among <c>YDataLogger.RECORDING_OFF</c>, <c>YDataLogger.RECORDING_ON</c> and
    ///   <c>YDataLogger.RECORDING_PENDING</c> corresponding to the current activation state of the data logger
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDataLogger.RECORDING_INVALID</c>.
    /// </para>
    ///-
    function get_recording():Integer;

    ////
    /// <summary>
    ///   Changes the activation state of the data logger to start/stop recording data.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   a value among <c>YDataLogger.RECORDING_OFF</c>, <c>YDataLogger.RECORDING_ON</c> and
    ///   <c>YDataLogger.RECORDING_PENDING</c> corresponding to the activation state of the data logger to
    ///   start/stop recording data
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
    function set_recording(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the default activation state of the data logger on power up.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YDataLogger.AUTOSTART_OFF</c> or <c>YDataLogger.AUTOSTART_ON</c>, according to the
    ///   default activation state of the data logger on power up
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDataLogger.AUTOSTART_INVALID</c>.
    /// </para>
    ///-
    function get_autoStart():Integer;

    ////
    /// <summary>
    ///   Changes the default activation state of the data logger on power up.
    /// <para>
    ///   Do not forget to call the <c>saveToFlash()</c> method of the module to save the
    ///   configuration change.  Note: if the device doesn't have any time source at his disposal when
    ///   starting up, it will wait for ~8 seconds before automatically starting to record  with
    ///   an arbitrary timestamp
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YDataLogger.AUTOSTART_OFF</c> or <c>YDataLogger.AUTOSTART_ON</c>, according to the
    ///   default activation state of the data logger on power up
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
    function set_autoStart(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns true if the data logger is synchronised with the localization beacon.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>YDataLogger.BEACONDRIVEN_OFF</c> or <c>YDataLogger.BEACONDRIVEN_ON</c>, according to true
    ///   if the data logger is synchronised with the localization beacon
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDataLogger.BEACONDRIVEN_INVALID</c>.
    /// </para>
    ///-
    function get_beaconDriven():Integer;

    ////
    /// <summary>
    ///   Changes the type of synchronisation of the data logger.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>YDataLogger.BEACONDRIVEN_OFF</c> or <c>YDataLogger.BEACONDRIVEN_ON</c>, according to the
    ///   type of synchronisation of the data logger
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
    function set_beaconDriven(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the percentage of datalogger memory in use.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the percentage of datalogger memory in use
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>YDataLogger.USAGE_INVALID</c>.
    /// </para>
    ///-
    function get_usage():LongInt;

    function get_clearHistory():Integer;

    function set_clearHistory(newval:Integer):integer;

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
    ///   Use the method <c>YDataLogger.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YDataLogger</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindDataLogger(func: string):TYDataLogger;

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
    function registerValueCallback(callback: TYDataLoggerValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Clears the data logger memory and discards all recorded data streams.
    /// <para>
    ///   This method also resets the current run index to zero.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI.SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function forgetAllDataStreams():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list of <c>YDataSet</c> objects that can be used to retrieve
    ///   all measures stored by the data logger.
    /// <para>
    /// </para>
    /// <para>
    ///   This function only works if the device uses a recent firmware,
    ///   as <c>YDataSet</c> objects are not supported by firmwares older than
    ///   version 13000.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list of <c>YDataSet</c> object.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty list.
    /// </para>
    ///-
    function get_dataSets():TYDataSetArray; overload; virtual;

    function parse_dataSets(json: TByteArray):TYDataSetArray; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of data loggers started using <c>yFirstDataLogger()</c>.
    /// <para>
    ///   Caution: You can't make any assumption about the returned data loggers order.
    ///   If you want to find a specific a data logger, use <c>DataLogger.findDataLogger()</c>
    ///   and a hardwareID or a logical name.
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YDataLogger</c> object, corresponding to
    ///   a data logger currently online, or a <c>NIL</c> pointer
    ///   if there are no more data loggers to enumerate.
    /// </returns>
    ///-
    function nextDataLogger():TYDataLogger;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstDataLogger():TYDataLogger;
  //--- (end of generated code: YDataLogger accessors declaration)
end;

//--- (generated code: YDataLogger functions declaration)
  ////
  /// <summary>
  ///   Retrieves a data logger for a given identifier.
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
  ///   This function does not require that the data logger is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YDataLogger.isOnline()</c> to test if the data logger is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a data logger by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the data logger, for instance
  ///   <c>LIGHTMK4.dataLogger</c>.
  /// </param>
  /// <returns>
  ///   a <c>YDataLogger</c> object allowing you to drive the data logger.
  /// </returns>
  ///-
  function yFindDataLogger(func:string):TYDataLogger;
  ////
  /// <summary>
  ///   Starts the enumeration of data loggers currently accessible.
  /// <para>
  ///   Use the method <c>YDataLogger.nextDataLogger()</c> to iterate on
  ///   next data loggers.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YDataLogger</c> object, corresponding to
  ///   the first data logger currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstDataLogger():TYDataLogger;

//--- (end of generated code: YDataLogger functions declaration)

//--- (generated code: YAPIContext yapiwrapper declaration)
    ////
    /// <summary>
    ///   Modifies the delay between each forced enumeration of the used YoctoHubs.
    /// <para>
    ///   By default, the library performs a full enumeration every 10 seconds.
    ///   To reduce network traffic, you can increase this delay.
    ///   It's particularly useful when a YoctoHub is connected to the GSM network
    ///   where traffic is billed. This parameter doesn't impact modules connected by USB,
    ///   nor the working of module arrival/removal callbacks.
    ///   Note: you must call this function after <c>yInitAPI</c>.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="deviceListValidity">
    ///   nubmer of seconds between each enumeration.
    /// @noreturn
    /// </param>
    ///-
    procedure ySetDeviceListValidity(deviceListValidity: LongInt);

    ////
    /// <summary>
    ///   Returns the delay between each forced enumeration of the used YoctoHubs.
    /// <para>
    ///   Note: you must call this function after <c>yInitAPI</c>.
    /// </para>
    /// </summary>
    /// <returns>
    ///   the number of seconds between each enumeration.
    /// </returns>
    ///-
    function yGetDeviceListValidity():LongInt;

    ////
    /// <summary>
    ///   Adds a UDEV rule which authorizes all users to access Yoctopuce modules
    ///   connected to the USB ports.
    /// <para>
    ///   This function works only under Linux. The process that
    ///   calls this method must have root privileges because this method changes the Linux configuration.
    /// </para>
    /// </summary>
    /// <param name="force">
    ///   if true, overwrites any existing rule.
    /// </param>
    /// <returns>
    ///   an empty string if the rule has been added.
    /// </returns>
    /// <para>
    ///   On failure, returns a string that starts with "error:".
    /// </para>
    ///-
    function yAddUdevRule(force: boolean):string;

    ////
    /// <summary>
    ///   Modifies the network connection delay for <c>yRegisterHub()</c> and <c>yUpdateDeviceList()</c>.
    /// <para>
    ///   This delay impacts only the YoctoHubs and VirtualHub
    ///   which are accessible through the network. By default, this delay is of 20000 milliseconds,
    ///   but depending or you network you may want to change this delay,
    ///   gor example if your network infrastructure is based on a GSM connection.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="networkMsTimeout">
    ///   the network connection delay in milliseconds.
    /// @noreturn
    /// </param>
    ///-
    procedure ySetNetworkTimeout(networkMsTimeout: LongInt);

    ////
    /// <summary>
    ///   Returns the network connection delay for <c>yRegisterHub()</c> and <c>yUpdateDeviceList()</c>.
    /// <para>
    ///   This delay impacts only the YoctoHubs and VirtualHub
    ///   which are accessible through the network. By default, this delay is of 20000 milliseconds,
    ///   but depending or you network you may want to change this delay,
    ///   for example if your network infrastructure is based on a GSM connection.
    /// </para>
    /// </summary>
    /// <returns>
    ///   the network connection delay in milliseconds.
    /// </returns>
    ///-
    function yGetNetworkTimeout():LongInt;

    ////
    /// <summary>
    ///   Change the validity period of the data loaded by the library.
    /// <para>
    ///   By default, when accessing a module, all the attributes of the
    ///   module functions are automatically kept in cache for the standard
    ///   duration (5 ms). This method can be used to change this standard duration,
    ///   for example in order to reduce network or USB traffic. This parameter
    ///   does not affect value change callbacks
    ///   Note: This function must be called after <c>yInitAPI</c>.
    /// </para>
    /// </summary>
    /// <param name="cacheValidityMs">
    ///   an integer corresponding to the validity attributed to the
    ///   loaded function parameters, in milliseconds.
    /// @noreturn
    /// </param>
    ///-
    procedure ySetCacheValidity(cacheValidityMs: u64);

    ////
    /// <summary>
    ///   Returns the validity period of the data loaded by the library.
    /// <para>
    ///   This method returns the cache validity of all attributes
    ///   module functions.
    ///   Note: This function must be called after <c>yInitAPI </c>.
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the validity attributed to the
    ///   loaded function parameters, in milliseconds
    /// </returns>
    ///-
    function yGetCacheValidity():u64;

//--- (end of generated code: YAPIContext yapiwrapper declaration)

  ////
  /// <summary>
  ///   Returns the version identifier for the Yoctopuce library in use.
  /// <para>
  ///   The version is a string in the form <c>"Major.Minor.Build"</c>,
  ///   for instance <c>"1.01.5535"</c>. For languages using an external
  ///   DLL (for instance C#, VisualBasic or Delphi), the character string
  ///   includes as well the DLL version, for instance
  ///   <c>"1.01.5535 (1.01.5439)"</c>.
  /// </para>
  /// <para>
  ///   If you want to verify in your code that the library version is
  ///   compatible with the version that you have used during development,
  ///   verify that the major number is strictly equal and that the minor
  ///   number is greater or equal. The build number is not relevant
  ///   with respect to the library compatibility.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a character string describing the library version.
  /// </returns>
  ///-
  function yGetAPIVersion():string;

  ////
  /// <summary>
  ///   Initializes the Yoctopuce programming library explicitly.
  /// <para>
  ///   It is not strictly needed to call <c>yInitAPI()</c>, as the library is
  ///   automatically  initialized when calling <c>yRegisterHub()</c> for the
  ///   first time.
  /// </para>
  /// <para>
  ///   When <c>YAPI.DETECT_NONE</c> is used as detection <c>mode</c>,
  ///   you must explicitly use <c>yRegisterHub()</c> to point the API to the
  ///   VirtualHub on which your devices are connected before trying to access them.
  /// </para>
  /// </summary>
  /// <param name="mode">
  ///   an integer corresponding to the type of automatic
  ///   device detection to use. Possible values are
  ///   <c>YAPI.DETECT_NONE</c>, <c>YAPI.DETECT_USB</c>, <c>YAPI.DETECT_NET</c>,
  ///   and <c>YAPI.DETECT_ALL</c>.
  /// </param>
  /// <param name="errmsg">
  ///   a string passed by reference to receive any error message.
  /// </param>
  /// <returns>
  ///   <c>YAPI.SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function yInitAPI(mode: integer; var errmsg:string ):integer;

  ////
  /// <summary>
  ///   Waits for all pending communications with Yoctopuce devices to be
  ///   completed then frees dynamically allocated resources used by
  ///   the Yoctopuce library.
  /// <para>
  /// </para>
  /// <para>
  ///   From an operating system standpoint, it is generally not required to call
  ///   this function since the OS will automatically free allocated resources
  ///   once your program is completed. However there are two situations when
  ///   you may really want to use that function:
  /// </para>
  /// <para>
  ///   - Free all dynamically allocated memory blocks in order to
  ///   track a memory leak.
  /// </para>
  /// <para>
  ///   - Send commands to devices right before the end
  ///   of the program. Since commands are sent in an asynchronous way
  ///   the program could exit before all commands are effectively sent.
  /// </para>
  /// <para>
  ///   You should not call any other library function after calling
  ///   <c>yFreeAPI()</c>, or your program will crash.
  /// </para>
  /// </summary>
  ///-
  procedure yFreeAPI();

  ////
  /// <summary>
  ///   Disables the use of exceptions to report runtime errors.
  /// <para>
  ///   When exceptions are disabled, every function returns a specific
  ///   error value which depends on its type and which is documented in
  ///   this reference manual.
  /// </para>
  /// </summary>
  ///-
  procedure yDisableExceptions();

  ////
  /// <summary>
  ///   Re-enables the use of exceptions for runtime error handling.
  /// <para>
  ///   Be aware than when exceptions are enabled, every function that fails
  ///   triggers an exception. If the exception is not caught by the user code,
  ///   it  either fires the debugger or aborts (i.e. crash) the program.
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  /// </summary>
  ///-
  procedure yEnableExceptions();


  function  yapiLockDeviceCallBack(var errmsg:string):integer;
  function  yapiUnlockDeviceCallBack(var errmsg:string):integer;


type
  yLogFunc            = procedure (log:string);
  yDeviceUpdateFunc   = procedure (module:TYModule);
  YHubDiscoveryCallback = procedure(serial, url: string);

  ////
  /// <summary>
  ///   Registers a log callback function.
  /// <para>
  ///   This callback will be called each time
  ///   the API have something to say. Quite useful to debug the API.
  /// </para>
  /// </summary>
  /// <param name="logfun">
  ///   a procedure taking a string parameter, or <c>NIL</c>
  ///   to unregister a previously registered  callback.
  /// </param>
  ///-
  procedure yRegisterLogFunction(logfun: yLogFunc);

  ////
  /// <summary>
  ///   Register a callback function, to be called each time
  ///   a device is plugged.
  /// <para>
  ///   This callback will be invoked while <c>yUpdateDeviceList</c>
  ///   is running. You will have to call this function on a regular basis.
  /// </para>
  /// </summary>
  /// <param name="arrivalCallback">
  ///   a procedure taking a <c>YModule</c> parameter, or <c>NIL</c>
  ///   to unregister a previously registered  callback.
  /// </param>
  ///-
  procedure yRegisterDeviceArrivalCallback(arrivalCallback: yDeviceUpdateFunc);

  ////
  /// <summary>
  ///   Register a callback function, to be called each time
  ///   a device is unplugged.
  /// <para>
  ///   This callback will be invoked while <c>yUpdateDeviceList</c>
  ///   is running. You will have to call this function on a regular basis.
  /// </para>
  /// </summary>
  /// <param name="removalCallback">
  ///   a procedure taking a <c>YModule</c> parameter, or <c>NIL</c>
  ///   to unregister a previously registered  callback.
  /// </param>
  ///-
  procedure yRegisterDeviceRemovalCallback(removalCallback: yDeviceUpdateFunc);


  procedure yRegisterDeviceChangeCallback(callback: yDeviceUpdateFunc);

  // register a new value calibration handler for a given calibration type
  procedure yRegisterCalibrationHandler(calibType:integer;callback:yCalibrationHandler);

  //  standard value calibration handler (n-points linear error correction).
  function  yLinearCalibrationHandler(rawValue:double; calibType: integer; params : TLongIntArray; rawValues,refValues:TDoubleArray):double;

  ////
  /// <summary>
  ///   Setup the Yoctopuce library to use modules connected on a given machine.
  /// <para>
  ///   Idealy this
  ///   call will be made once at the begining of your application.  The
  ///   parameter will determine how the API will work. Use the following values:
  /// </para>
  /// <para>
  ///   <b>usb</b>: When the <c>usb</c> keyword is used, the API will work with
  ///   devices connected directly to the USB bus. Some programming languages such a JavaScript,
  ///   PHP, and Java don't provide direct access to USB hardware, so <c>usb</c> will
  ///   not work with these. In this case, use a VirtualHub or a networked YoctoHub (see below).
  /// </para>
  /// <para>
  ///   <b><i>x.x.x.x</i></b> or <b><i>hostname</i></b>: The API will use the devices connected to the
  ///   host with the given IP address or hostname. That host can be a regular computer
  ///   running a <i>native VirtualHub</i>, a <i>VirtualHub for web</i> hosted on a server,
  ///   or a networked YoctoHub such as YoctoHub-Ethernet or
  ///   YoctoHub-Wireless. If you want to use the VirtualHub running on you local
  ///   computer, use the IP address 127.0.0.1. If the given IP is unresponsive, <c>yRegisterHub</c>
  ///   will not return until a time-out defined by <c>ySetNetworkTimeout</c> has elapsed.
  ///   However, it is possible to preventively test a connection  with <c>yTestHub</c>.
  ///   If you cannot afford a network time-out, you can use the non blocking <c>yPregisterHub</c>
  ///   function that will establish the connection as soon as it is available.
  /// </para>
  /// <para>
  /// </para>
  /// <para>
  ///   <b>callback</b>: that keyword make the API run in "<i>HTTP Callback</i>" mode.
  ///   This a special mode allowing to take control of Yoctopuce devices
  ///   through a NAT filter when using a VirtualHub or a networked YoctoHub. You only
  ///   need to configure your hub to call your server script on a regular basis.
  ///   This mode is currently available for PHP and Node.JS only.
  /// </para>
  /// <para>
  ///   Be aware that only one application can use direct USB access at a
  ///   given time on a machine. Multiple access would cause conflicts
  ///   while trying to access the USB modules. In particular, this means
  ///   that you must stop the VirtualHub software before starting
  ///   an application that uses direct USB access. The workaround
  ///   for this limitation is to setup the library to use the VirtualHub
  ///   rather than direct USB access.
  /// </para>
  /// <para>
  ///   If access control has been activated on the hub, virtual or not, you want to
  ///   reach, the URL parameter should look like:
  /// </para>
  /// <para>
  ///   <c>http://username:password@address:port</c>
  /// </para>
  /// <para>
  ///   You can call <i>RegisterHub</i> several times to connect to several machines. On
  ///   the other hand, it is useless and even counterproductive to call <i>RegisterHub</i>
  ///   with to same address multiple times during the life of the application.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="url">
  ///   a string containing either <c>"usb"</c>,<c>"callback"</c> or the
  ///   root URL of the hub to monitor
  /// </param>
  /// <param name="errmsg">
  ///   a string passed by reference to receive any error message.
  /// </param>
  /// <returns>
  ///   <c>YAPI.SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function yRegisterHub(url:string;var errmsg:string):integer;

  ////
  /// <summary>
  ///   Fault-tolerant alternative to <c>yRegisterHub()</c>.
  /// <para>
  ///   This function has the same
  ///   purpose and same arguments as <c>yRegisterHub()</c>, but does not trigger
  ///   an error when the selected hub is not available at the time of the function call.
  ///   If the connexion cannot be established immediately, a background task will automatically
  ///   perform periodic retries. This makes it possible to register a network hub independently of the current
  ///   connectivity, and to try to contact it only when a device is actively needed.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="url">
  ///   a string containing either <c>"usb"</c>,<c>"callback"</c> or the
  ///   root URL of the hub to monitor
  /// </param>
  /// <param name="errmsg">
  ///   a string passed by reference to receive any error message.
  /// </param>
  /// <returns>
  ///   <c>YAPI.SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function yPreregisterHub(url:string;var errmsg:string):integer;

  ////
  /// <summary>
  ///   Setup the Yoctopuce library to no more use modules connected on a previously
  ///   registered machine with RegisterHub.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="url">
  ///   a string containing either <c>"usb"</c> or the
  ///   root URL of the hub to monitor
  /// </param>
  ///-
  procedure yUnregisterHub(url:string);

  ////
  /// <summary>
  ///   Test if the hub is reachable.
  /// <para>
  ///   This method do not register the hub, it only test if the
  ///   hub is usable. The url parameter follow the same convention as the <c>yRegisterHub</c>
  ///   method. This method is useful to verify the authentication parameters for a hub. It
  ///   is possible to force this method to return after mstimeout milliseconds.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="url">
  ///   a string containing either <c>"usb"</c>,<c>"callback"</c> or the
  ///   root URL of the hub to monitor
  /// </param>
  /// <param name="mstimeout">
  ///   the number of millisecond available to test the connection.
  /// </param>
  /// <param name="errmsg">
  ///   a string passed by reference to receive any error message.
  /// </param>
  /// <returns>
  ///   <c>YAPI.SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure returns a negative error code.
  /// </para>
  ///-
  function yTestHub(url:string; mstimeout:integer; var errmsg:string):integer;


  ////
  /// <summary>
  ///   Triggers a (re)detection of connected Yoctopuce modules.
  /// <para>
  ///   The library searches the machines or USB ports previously registered using
  ///   <c>yRegisterHub()</c>, and invokes any user-defined callback function
  ///   in case a change in the list of connected devices is detected.
  /// </para>
  /// <para>
  ///   This function can be called as frequently as desired to refresh the device list
  ///   and to make the application aware of hot-plug events. However, since device
  ///   detection is quite a heavy process, UpdateDeviceList shouldn't be called more
  ///   than once every two seconds.
  /// </para>
  /// </summary>
  /// <param name="errmsg">
  ///   a string passed by reference to receive any error message.
  /// </param>
  /// <returns>
  ///   <c>YAPI.SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function yUpdateDeviceList(var errmsg:string):integer;

  ////
  /// <summary>
  ///   Maintains the device-to-library communication channel.
  /// <para>
  ///   If your program includes significant loops, you may want to include
  ///   a call to this function to make sure that the library takes care of
  ///   the information pushed by the modules on the communication channels.
  ///   This is not strictly necessary, but it may improve the reactivity
  ///   of the library for the following commands.
  /// </para>
  /// <para>
  ///   This function may signal an error in case there is a communication problem
  ///   while contacting a module.
  /// </para>
  /// </summary>
  /// <param name="errmsg">
  ///   a string passed by reference to receive any error message.
  /// </param>
  /// <returns>
  ///   <c>YAPI.SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function  yHandleEvents(var errmsg:string):integer;

  ////
  /// <summary>
  ///   Pauses the execution flow for a specified duration.
  /// <para>
  ///   This function implements a passive waiting loop, meaning that it does not
  ///   consume CPU cycles significantly. The processor is left available for
  ///   other threads and processes. During the pause, the library nevertheless
  ///   reads from time to time information from the Yoctopuce modules by
  ///   calling <c>yHandleEvents()</c>, in order to stay up-to-date.
  /// </para>
  /// <para>
  ///   This function may signal an error in case there is a communication problem
  ///   while contacting a module.
  /// </para>
  /// </summary>
  /// <param name="ms_duration">
  ///   an integer corresponding to the duration of the pause,
  ///   in milliseconds.
  /// </param>
  /// <param name="errmsg">
  ///   a string passed by reference to receive any error message.
  /// </param>
  /// <returns>
  ///   <c>YAPI.SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function ySleep(ms_duration:integer; var errmsg:string):integer;

  ////
  /// <summary>
  ///   Returns the current value of a monotone millisecond-based time counter.
  /// <para>
  ///   This counter can be used to compute delays in relation with
  ///   Yoctopuce devices, which also uses the millisecond as timebase.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a long integer corresponding to the millisecond counter.
  /// </returns>
  ///-
  function yGetTickCount():u64;

  ////
  /// <summary>
  ///   Checks if a given string is valid as logical name for a module or a function.
  /// <para>
  ///   A valid logical name has a maximum of 19 characters, all among
  ///   <c>A..Z</c>, <c>a..z</c>, <c>0..9</c>, <c>_</c>, and <c>-</c>.
  ///   If you try to configure a logical name with an incorrect string,
  ///   the invalid characters are ignored.
  /// </para>
  /// </summary>
  /// <param name="name">
  ///   a string containing the name to check.
  /// </param>
  /// <returns>
  ///   <c>true</c> if the name is valid, <c>false</c> otherwise.
  /// </returns>
  ///-
  function yCheckLogicalName(name:string):boolean;

  ////
  /// <summary>
  ///   Register a callback function, to be called each time an Network Hub send
  ///   an SSDP message.
  /// <para>
  ///   The callback has two string parameter, the first one
  ///   contain the serial number of the hub and the second contain the URL of the
  ///   network hub (this URL can be passed to RegisterHub). This callback will be invoked
  ///   while yUpdateDeviceList is running. You will have to call this function on a regular basis.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="hubDiscoveryCallback">
  ///   a procedure taking two string parameter, the serial
  ///   number and the hub URL. Use <c>NIL</c> to unregister a previously registered  callback.
  /// </param>
  ///-
  procedure yRegisterHubDiscoveryCallback(hubDiscoveryCallback: YHubDiscoveryCallback);

  ////
  /// <summary>
  ///   Force a hub discovery, if a callback as been registered with <c>yRegisterHubDiscoveryCallback</c> it
  ///   will be called for each net work hub that will respond to the discovery.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="errmsg">
  ///   a string passed by reference to receive any error message.
  /// </param>
  /// <returns>
  ///   <c>YAPI.SUCCESS</c> when the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function yTriggerHubDiscovery(var errmsg:string):integer;

  //--- (generated code: YFunction functions declaration)
  ////
  /// <summary>
  ///   Retrieves a function for a given identifier.
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
  ///   This function does not require that the function is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YFunction.isOnline()</c> to test if the function is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a function by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the function, for instance
  ///   <c>MyDevice.</c>.
  /// </param>
  /// <returns>
  ///   a <c>YFunction</c> object allowing you to drive the function.
  /// </returns>
  ///-
  function yFindFunction(func:string):TYFunction;
  ////
  /// <summary>
  ///   c
  /// <para>
  ///   omment from .yc definition
  /// </para>
  /// </summary>
  ///-
  function yFirstFunction():TYFunction;

//--- (end of generated code: YFunction functions declaration)

  //--- (generated code: YModule functions declaration)
  ////
  /// <summary>
  ///   Allows you to find a module from its serial number or from its logical name.
  /// <para>
  /// </para>
  /// <para>
  ///   This function does not require that the module is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YModule.isOnline()</c> to test if the module is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a module by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// <para>
  /// </para>
  /// <para>
  ///   If a call to this object's is_online() method returns FALSE although
  ///   you are certain that the device is plugged, make sure that you did
  ///   call registerHub() at application initialization time.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string containing either the serial number or
  ///   the logical name of the desired module
  /// </param>
  /// <returns>
  ///   a <c>YModule</c> object allowing you to drive the module
  ///   or get additional information on the module.
  /// </returns>
  ///-
  function yFindModule(func:string):TYModule;
  ////
  /// <summary>
  ///   Starts the enumeration of modules currently accessible.
  /// <para>
  ///   Use the method <c>YModule.nextModule()</c> to iterate on the
  ///   next modules.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YModule</c> object, corresponding to
  ///   the first module currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstModule():TYModule;

//--- (end of generated code: YModule functions declaration)

//--- (generated code: YSensor functions declaration)
  ////
  /// <summary>
  ///   Retrieves a sensor for a given identifier.
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
  ///   This function does not require that the sensor is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YSensor.isOnline()</c> to test if the sensor is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a sensor by logical name, no error is notified: the first instance
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
  ///   a string that uniquely characterizes the sensor, for instance
  ///   <c>MyDevice.</c>.
  /// </param>
  /// <returns>
  ///   a <c>YSensor</c> object allowing you to drive the sensor.
  /// </returns>
  ///-
  function yFindSensor(func:string):TYSensor;
  ////
  /// <summary>
  ///   Starts the enumeration of sensors currently accessible.
  /// <para>
  ///   Use the method <c>YSensor.nextSensor()</c> to iterate on
  ///   next sensors.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YSensor</c> object, corresponding to
  ///   the first sensor currently online, or a <c>NIL</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstSensor():TYSensor;

//--- (end of generated code: YSensor functions declaration)
//--- (generated code: YFirmwareUpdate functions declaration)
//--- (end of generated code: YFirmwareUpdate functions declaration)
//--- (generated code: YDataStream functions declaration)
//--- (end of generated code: YDataStream functions declaration)
//--- (generated code: YMeasure functions declaration)
//--- (end of generated code: YMeasure functions declaration)
//--- (generated code: YDataSet functions declaration)
//--- (end of generated code: YDataSet functions declaration)
//--- (generated code: YConsolidatedDataSet functions declaration)
//--- (end of generated code: YConsolidatedDataSet functions declaration)


  {*****************************************************************************
  Function:
  function yGetAPIVersion(var version,subversion:string):u16;

  Description:
  Return API Version

  Parameters:
  version: if not NULL this pointer will be updated with the version of the api
  subversion: if not NULL this pointer will be updated with the subversion of the api

  Returns:
  Return the BCD encoded version number of the API

  Remarks:
  ***************************************************************************}
  function yapiGetAPIVersion(var version,build_date:string):u16;


  procedure yapiSetTraceFile(filename:string);
  function yapiGetDevice(device_str:string;var errmsg:string):YDEV_DESCR;
  function yapiGetDeviceInfo(d:YDEV_DESCR;var infos:yDeviceSt;var errmsg:string):integer;
  function yapiGetFunction(class_str,function_str:string;var errmsg:string):YFUN_DESCR;
  function yapiGetFunctionsByClass( class_str:string; precFuncDesc:YFUN_DESCR;dbuffer:PyHandleArray;maxsize:integer;var neededsize:integer; var errmsg:string):integer;
  function yapiGetFunctionsByDevice( devdesc:YDEV_DESCR; precFuncDesc:YFUN_DESCR;dbuffer:PyHandleArray;maxsize:integer;var neededsize:integer; var errmsg:string):integer;
  function yapiGetFunctionInfo(fundesc:YFUN_DESCR;var devdesc:YDEV_DESCR;var serial,funcId,funcName,funcVal,errmsg : string):integer;
  function yapiGetDeviceByFunction(fundesc:YFUN_DESCR;var errmsg : string):integer;
  function yapiHTTPRequest(device:string; request:string; var answer:string; var errmsg:string):integer;
  function yapiGetDevicePath(devdesc: integer; var rootdevice :string; var path : string ; var errmsg:String ) :integer;

  function  YISERR(retcode:YRETCODE):boolean;
{$ifdef ENABLEPROGRAMMING}
  function  yapiFlashDevice(args:TyFlashArg; var errmsg : string):integer;
  function  yapiVerifyDevice(args:TyFlashArg;var errmsg : string):integer;
{$endif}
  function _getCalibrationHandler(calType:integer):yCalibrationHandler;
  function _StrToByte(value: string): TByteArray;
  function _ByteToString(value: TByteArray): string;
  function _decimalToDouble(val:u64):double;
  function _doubleToDecimal(val:double):u64;
  function _decodeWords(sdat:string):TLongIntArray;
  function _decodeFloats(sdat:string):TLongIntArray;
  function _yapiBoolToStr(value:boolean):string;
  function _yapiFloatToStr(value:double):string;
  function _stringSplit(str :String; delimiter :Char):TStringArray;
  function _atoi(val:string):integer;
  function _bytesToHexStr(bytes: TByteArray; ofs:integer; len:integer):string;
  function _hexStrToBin(hex_str:string):TByteArray;
  function _bytesMerge(array_a:TByteArray; array_b:TByteArray):TByteArray;

var
  _yapiContext       : TYAPIContext;


implementation

var
  _cache             : TStringList;
  _FunctionCallbacks : Tlist;
  _TimedReportCallbackList : Tlist;
  _CalibHandlers     : TStringList;
  _moduleCallbackList: TStringList;

  constructor  YAPI_Exception.Create(errType:YRETCODE;  errMsg:string );
    begin
      inherited create(errMsg);
      errorType := errType;
    end;

type
  _yapiLogFunc            = procedure (log:pansichar; loglen:u32);cdecl;
  _yapiDeviceUpdateFunc   = procedure (dev:YDEV_DESCR);cdecl;
  _yapiFunctionUpdateFunc = procedure (func:YFUN_DESCR; value:pansichar);cdecl;
  _yapiBeaconFunc         = procedure (dev:YDEV_DESCR; beacon:integer);cdecl;
  _yapiTimedReportFunc    = procedure (func:YFUN_DESCR; timestamp:double; bytes:pansichar; len:integer; duration:double);cdecl;
  _yapiHubDiscoveryFunc = procedure (serial:pansichar; url:pansichar);cdecl;
  _yapiDeviceLogFunc    = procedure (dev:YDEV_DESCR; line:pansichar);cdecl;
  _yapiRequestAsyncFunc = procedure (context:pointer; result:pansichar; resultlen:integer; retcode: integer; errmsg:pansichar);cdecl;
  _yapiRequestProgressFunc = procedure (context:pointer; acked:integer; totalbytes:integer);


var
  ylog:     yLogFunc;
  yArrival: yDeviceUpdateFunc;
  yRemoval: yDeviceUpdateFunc;
  yChange:  yDeviceUpdateFunc;
  _HubDiscoveryCallback: YHubDiscoveryCallback;


type
  PDEV_BROADCAST_DEVICEINTERFACE  = ^DEV_BROADCAST_DEVICEINTERFACE;

  ULONG_PTR = DWORD;

  DEV_BROADCAST_DEVICEINTERFACE = record
    dbcc_size       : DWORD;
    dbcc_devicetype : DWORD;
    dbcc_reserved   : DWORD;
    dbcc_classguid  : TGUID;
    dbcc_name       : char;
  end;

  PDEV_BROADCAST_HDR = ^DEV_BROADCAST_HDR;
  DEV_BROADCAST_HDR =record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
  end;

  HDEVINFO = Pointer;

  PSP_DEVICE_INTERFACE_DETAIL_DATA_A = ^SP_DEVICE_INTERFACE_DETAIL_DATA_A;
  PSP_DEVICE_INTERFACE_DETAIL_DATA_W = ^SP_DEVICE_INTERFACE_DETAIL_DATA_W;
  PSP_DEVICE_INTERFACE_DETAIL_DATA = ^SP_DEVICE_INTERFACE_DETAIL_DATA_A;
  SP_DEVICE_INTERFACE_DETAIL_DATA_A = packed record
    cbSize: DWORD;
    DevicePath: array [0..1] of AnsiChar;
  end;

  SP_DEVICE_INTERFACE_DETAIL_DATA_W = packed record
    cbSize: DWORD;
    DevicePath: array [0..1] of WideChar;
  end;

  PSP_DEVINFO_DATA = ^SP_DEVINFO_DATA;
  SP_DEVINFO_DATA = packed record
    cbSize: DWORD;
    ClassGuid: TGUID;
    DevInst: DWORD;
    Reserved: ULONG_PTR;
  end;

  PSP_DEVICE_INTERFACE_DATA = ^SP_DEVICE_INTERFACE_DATA;
  SP_DEVICE_INTERFACE_DATA = packed record
    cbSize: DWORD;
    InterfaceClassGuid: TGUID;
    Flags: DWORD;
    Reserved: ULONG_PTR;
  end;


  {$IFDEF ENABLEPROGRAMMING}
const
  InterfaceClassGuid : TGUID = '{4D1E55B2-F16F-11CF-88CB-001111000030}';
  DBT_DEVICEARRIVAL          = $8000;          // system detected a new device
  DBT_DEVICEREMOVECOMPLETE   = $8004;          // device is gone
  DBT_DEVTYP_DEVICEINTERFACE = $00000005;      // device interface class


  DIGCF_DEFAULT         = $00000001;
  DIGCF_PRESENT         = $00000002;
  DIGCF_ALLCLASSES      = $00000004;
  DIGCF_PROFILE         = $00000008;
  DIGCF_DEVICEINTERFACE = $00000010;

  SPDRP_DEVICEDESC                  = $00000000; // DeviceDesc (R/W)
  SPDRP_HARDWAREID                  = $00000001; // HardwareID (R/W)
  SPDRP_COMPATIBLEIDS               = $00000002; // CompatibleIDs (R/W)
  SPDRP_UNUSED0                     = $00000003; // unused
  SPDRP_SERVICE                     = $00000004; // Service (R/W)
  SPDRP_UNUSED1                     = $00000005; // unused
  SPDRP_UNUSED2                     = $00000006; // unused
  SPDRP_CLASS                       = $00000007; // Class (R--tied to ClassGUID)
  SPDRP_CLASSGUID                   = $00000008; // ClassGUID (R/W)
  SPDRP_DRIVER                      = $00000009; // Driver (R/W)
  SPDRP_CONFIGFLAGS                 = $0000000A; // ConfigFlags (R/W)
  SPDRP_MFG                         = $0000000B; // Mfg (R/W)
  SPDRP_FRIENDLYNAME                = $0000000C; // FriendlyName (R/W)
  SPDRP_LOCATION_INFORMATION        = $0000000D; // LocationInformation (R/W)
  SPDRP_PHYSICAL_DEVICE_OBJECT_NAME = $0000000E; // PhysicalDeviceObjectName (R)
  SPDRP_CAPABILITIES                = $0000000F; // Capabilities (R)
  SPDRP_UI_NUMBER                   = $00000010; // UiNumber (R)
  SPDRP_UPPERFILTERS                = $00000011; // UpperFilters (R/W)
  SPDRP_LOWERFILTERS                = $00000012; // LowerFilters (R/W)
  SPDRP_BUSTYPEGUID                 = $00000013; // BusTypeGUID (R)
  SPDRP_LEGACYBUSTYPE               = $00000014; // LegacyBusType (R)
  SPDRP_BUSNUMBER                   = $00000015; // BusNumber (R)
  SPDRP_ENUMERATOR_NAME             = $00000016; // Enumerator Name (R)
  SPDRP_SECURITY                    = $00000017; // Security (R/W, binary form)
  SPDRP_SECURITY_SDS                = $00000018; // Security (W, SDS form)
  SPDRP_DEVTYPE                     = $00000019; // Device Type (R/W)
  SPDRP_EXCLUSIVE                   = $0000001A; // Device is exclusive-access (R/W)
  SPDRP_CHARACTERISTICS             = $0000001B; // Device Characteristics (R/W)
  SPDRP_ADDRESS                     = $0000001C; // Device Address (R)
  SPDRP_UI_NUMBER_DESC_FORMAT       = $0000001E; // UiNumberDescFormat (R/W)
  SPDRP_MAXIMUM_PROPERTY            = $0000001F; // Upper bound on ordinals
  {$ENDIF}

{IsNaN does not exist in delphi 5.
 Implementation by John Herbste
}

type  TExtPackedRec = packed record Man: Int64; Exp: word end;
const SglExpBits: LongInt = $7F800000;          { 8 bits}
      DblExpBits: Int64   = $7FF0000000000000;  {11 bits}
      ExtExpBits: word    = $7FFF;              {15 bits}

Function IsNAN_D5 (const sgl: single): boolean;
var InputX: LongInt absolute sgl;
begin
  Result := (InputX <> 0) and ((InputX and SglExpBits)=SglExpBits);
end;

Function IsNAN_D5 (const dbl: double): boolean;
var InputX: Int64 absolute dbl;
begin
  Result := (InputX <> 0) and ((InputX and DblExpBits)=DblExpBits);
end;


  {$IFNDEF UNIX}

  function SetupDiGetClassDevsA(ClassGuid: PGUID; const Enumerator: PChar;
    hwndParent: HWND; Flags: DWORD): HDEVINFO; stdcall;   external   'setupapi.dll' name 'SetupDiGetClassDevsA';

  function SetupDiEnumDeviceInterfaces(DeviceInfoSet: HDEVINFO;
    DeviceInfoData: PSP_DEVINFO_DATA; const InterfaceClassGuid: TGUID;
    MemberIndex: DWORD; var DeviceInterfaceData: SP_DEVICE_INTERFACE_DATA): LongBool; stdcall;    external   'setupapi.dll' name 'SetupDiEnumDeviceInterfaces';

  function SetupDiEnumDeviceInfo(DeviceInfoSet: HDEVINFO;
    MemberIndex: DWORD; var DeviceInfoData: SP_DEVINFO_DATA): LongBool; stdcall; external   'setupapi.dll' name 'SetupDiEnumDeviceInfo';

  function SetupDiGetDeviceRegistryPropertyA(DeviceInfoSet: HDEVINFO;
    const DeviceInfoData: SP_DEVINFO_DATA; Property_: DWORD;
    var PropertyRegDataType: DWORD; PropertyBuffer: PBYTE; PropertyBufferSize: DWORD;
    var RequiredSize: DWORD): LongBool; stdcall;  external   'setupapi.dll' name 'SetupDiGetDeviceRegistryPropertyA';


  function SetupDiGetDeviceInterfaceDetailA(DeviceInfoSet: HDEVINFO;
    DeviceInterfaceData: PSP_DEVICE_INTERFACE_DATA;
    DeviceInterfaceDetailData: PSP_DEVICE_INTERFACE_DETAIL_DATA_A;
    DeviceInterfaceDetailDataSize: DWORD; var RequiredSize: DWORD;
    Device: PSP_DEVINFO_DATA): LongBool; stdcall;   external   'setupapi.dll' name 'SetupDiGetDeviceInterfaceDetailA';


  function SetupDiDestroyDeviceInfoList(DeviceInfoSet: HDEVINFO): LongBool; stdcall;  external   'setupapi.dll' name 'SetupDiDestroyDeviceInfoList';
   {$ENDIF}
const
{$IFDEF ENABLEPROGRAMMING}
  dllfile = 'yprogrammer.dll';
{$ELSE}
  {$IFDEF LINUX}
    {$IFDEF CPUARM}
      {$IFDEF CPU64}
          dllfile = 'libyapi-aarch64.so';
      {$ELSE}
          dllfile = 'libyapi-armhf.so';
      {$ENDIF}
    {$ELSE}
      {$IFDEF CPU64}
        dllfile = 'libyapi-amd64.so';
      {$ELSE}
        dllfile = 'libyapi-i386.so';
      {$endif}
    {$ENDIF}
  {$ELSE}
    {$IFDEF WIN32}
       dllfile = 'yapi.dll';
    {$ELSE}
      {$IFDEF WIN64}
         dllfile = 'yapi64.dll';
      {$ELSE}
       dllfile = 'yapi.dll';
      {$ENDIF}
  {$ENDIF}
 {$ENDIF}
{$ENDIF}

  {$ifdef ENABLEPROGRAMMING}
  function  _yapiFlashDevice(args:PyFlashArg;errmsg : pansichar):integer;cdecl; external dllfile name 'yapiFlashDevice';
  function  _yapiVerifyDevice(args:PyFlashArg;errmsg : pansichar):integer;cdecl; external dllfile name 'yapiVerifyDevice';
  {$endif}
  function  _yapiGetMem(size:integer):pointer; cdecl; external dllfile name 'yapiGetMem';
  function  _yapiGetErrorString(errorcode:integer;buffer:pansichar; maxsize:integer;errmsg : pansichar):integer;  cdecl; external dllfile name 'yapiGetErrorString';
  //--- (generated code: YFunction dlldef)
  function _yapiInitAPI(mode:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiInitAPI';
  procedure _yapiFreeAPI(); cdecl; external dllfile name 'yapiFreeAPI';
  procedure _yapiSetTraceFile(tracefile:pansichar); cdecl; external dllfile name 'yapiSetTraceFile';
  procedure _yapiRegisterLogFunction(fct:_yapiLogFunc); cdecl; external dllfile name 'yapiRegisterLogFunction';
  procedure _yapiRegisterDeviceArrivalCallback(fct:_yapiDeviceUpdateFunc); cdecl; external dllfile name 'yapiRegisterDeviceArrivalCallback';
  procedure _yapiRegisterDeviceRemovalCallback(fct:_yapiDeviceUpdateFunc); cdecl; external dllfile name 'yapiRegisterDeviceRemovalCallback';
  procedure _yapiRegisterDeviceChangeCallback(fct:_yapiDeviceUpdateFunc); cdecl; external dllfile name 'yapiRegisterDeviceChangeCallback';
  procedure _yapiRegisterDeviceConfigChangeCallback(fct:_yapiDeviceUpdateFunc); cdecl; external dllfile name 'yapiRegisterDeviceConfigChangeCallback';
  procedure _yapiRegisterFunctionUpdateCallback(fct:_yapiFunctionUpdateFunc); cdecl; external dllfile name 'yapiRegisterFunctionUpdateCallback';
  procedure _yapiRegisterTimedReportCallback(fct:_yapiTimedReportFunc); cdecl; external dllfile name 'yapiRegisterTimedReportCallback';
  function _yapiLockDeviceCallBack(errmsg:pansichar):integer; cdecl; external dllfile name 'yapiLockDeviceCallBack';
  function _yapiUnlockDeviceCallBack(errmsg:pansichar):integer; cdecl; external dllfile name 'yapiUnlockDeviceCallBack';
  function _yapiLockFunctionCallBack(errmsg:pansichar):integer; cdecl; external dllfile name 'yapiLockFunctionCallBack';
  function _yapiUnlockFunctionCallBack(errmsg:pansichar):integer; cdecl; external dllfile name 'yapiUnlockFunctionCallBack';
  function _yapiRegisterHub(rootUrl:pansichar; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiRegisterHub';
  function _yapiPreregisterHub(rootUrl:pansichar; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiPreregisterHub';
  procedure _yapiUnregisterHub(rootUrl:pansichar); cdecl; external dllfile name 'yapiUnregisterHub';
  function _yapiUpdateDeviceList(force:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiUpdateDeviceList';
  function _yapiHandleEvents(errmsg:pansichar):integer; cdecl; external dllfile name 'yapiHandleEvents';
  function _yapiGetTickCount():u64; cdecl; external dllfile name 'yapiGetTickCount';
  function _yapiCheckLogicalName(name:pansichar):integer; cdecl; external dllfile name 'yapiCheckLogicalName';
  function _yapiGetAPIVersion(var version:pansichar; var dat_:pansichar):u16; cdecl; external dllfile name 'yapiGetAPIVersion';
  function _yapiGetDevice(device_str:pansichar; errmsg:pansichar):YDEV_DESCR; cdecl; external dllfile name 'yapiGetDevice';
  function _yapiGetDeviceInfo(d:YDEV_DESCR; var infos:yDeviceSt; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiGetDeviceInfo';
  function _yapiGetFunction(class_str:pansichar; function_str:pansichar; errmsg:pansichar):YFUN_DESCR; cdecl; external dllfile name 'yapiGetFunction';
  function _yapiGetFunctionsByClass(class_str:pansichar; precFuncDesc:YFUN_DESCR; buffer:PyHandleArray; maxsize:integer; var neededsize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiGetFunctionsByClass';
  function _yapiGetFunctionsByDevice(device:YDEV_DESCR; precFuncDesc:YFUN_DESCR; buffer:PyHandleArray; maxsize:integer; var neededsize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiGetFunctionsByDevice';
  function _yapiGetFunctionInfoEx(fundesc:YFUN_DESCR; var devdesc:YDEV_DESCR; serial:pansichar; funcId:pansichar; baseType:pansichar; funcName:pansichar; funcVal:pansichar; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiGetFunctionInfoEx';
  function _yapiHTTPRequestSyncStart(iohdl:PYIOHDL; device:pansichar; request:pansichar; var reply:pansichar; var replysize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiHTTPRequestSyncStart';
  function _yapiHTTPRequestSyncStartEx(iohdl:PYIOHDL; device:pansichar; request:pansichar; requestlen:integer; var reply:pansichar; var replysize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiHTTPRequestSyncStartEx';
  function _yapiHTTPRequestSyncDone(iohdl:PYIOHDL; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiHTTPRequestSyncDone';
  function _yapiHTTPRequestAsync(device:pansichar; request:pansichar; callback:_yapiRequestAsyncFunc; context:pointer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiHTTPRequestAsync';
  function _yapiHTTPRequestAsyncEx(device:pansichar; request:pansichar; requestlen:integer; callback:_yapiRequestAsyncFunc; context:pointer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiHTTPRequestAsyncEx';
  function _yapiHTTPRequest(device:pansichar; url:pansichar; buffer:pansichar; buffsize:integer; var fullsize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiHTTPRequest';
  function _yapiGetDevicePath(devdesc:integer; rootdevice:pansichar; path:pansichar; pathsize:integer; var neededsize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiGetDevicePath';
  function _yapiSleep(duration_ms:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiSleep';
  procedure _yapiRegisterHubDiscoveryCallback(fct:_yapiHubDiscoveryFunc); cdecl; external dllfile name 'yapiRegisterHubDiscoveryCallback';
  function _yapiTriggerHubDiscovery(errmsg:pansichar):integer; cdecl; external dllfile name 'yapiTriggerHubDiscovery';
  procedure _yapiRegisterDeviceLogCallback(fct:_yapiDeviceLogFunc); cdecl; external dllfile name 'yapiRegisterDeviceLogCallback';
  function _yapiGetAllJsonKeys(jsonbuffer:pansichar; out_buffer:pansichar; out_buffersize:integer; var fullsize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiGetAllJsonKeys';
  function _yapiCheckFirmware(serial:pansichar; rev:pansichar; path:pansichar; buffer:pansichar; buffersize:integer; var fullsize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiCheckFirmware';
  function _yapiGetBootloaders(buffer:pansichar; buffersize:integer; var totalSize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiGetBootloaders';
  function _yapiUpdateFirmwareEx(serial:pansichar; firmwarePath:pansichar; settings:pansichar; force:integer; startUpdate:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiUpdateFirmwareEx';
  function _yapiHTTPRequestSyncStartOutOfBand(iohdl:PYIOHDL; channel:integer; device:pansichar; request:pansichar; requestsize:integer; var reply:pansichar; var replysize:integer; progress_cb:_yapiRequestProgressFunc; progress_ctx:pointer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiHTTPRequestSyncStartOutOfBand';
  function _yapiHTTPRequestAsyncOutOfBand(channel:integer; device:pansichar; request:pansichar; requestsize:integer; callback:_yapiRequestAsyncFunc; context:pointer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiHTTPRequestAsyncOutOfBand';
  function _yapiTestHub(url:pansichar; mstimeout:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiTestHub';
  function _yapiJsonGetPath(path:pansichar; json_data:pansichar; json_len:integer; var result:pansichar; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiJsonGetPath';
  function _yapiJsonDecodeString(json_data:pansichar; output:pansichar):integer; cdecl; external dllfile name 'yapiJsonDecodeString';
  function _yapiGetSubdevices(serial:pansichar; buffer:pansichar; buffersize:integer; var totalSize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiGetSubdevices';
  procedure _yapiFreeMem(buffer:pointer); cdecl; external dllfile name 'yapiFreeMem';
  function _yapiGetDevicePathEx(serial:pansichar; rootdevice:pansichar; path:pansichar; pathsize:integer; var neededsize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiGetDevicePathEx';
  procedure _yapiSetNetDevListValidity(sValidity:integer); cdecl; external dllfile name 'yapiSetNetDevListValidity';
  function _yapiGetNetDevListValidity():integer; cdecl; external dllfile name 'yapiGetNetDevListValidity';
  procedure _yapiRegisterBeaconCallback(beaconCallback:_yapiBeaconFunc); cdecl; external dllfile name 'yapiRegisterBeaconCallback';
  procedure _yapiStartStopDeviceLogCallback(serial:pansichar; start:integer); cdecl; external dllfile name 'yapiStartStopDeviceLogCallback';
  function _yapiIsModuleWritable(serial:pansichar; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiIsModuleWritable';
  function _yapiGetDLLPath(path:pansichar; pathsize:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiGetDLLPath';
  procedure _yapiSetNetworkTimeout(sValidity:integer); cdecl; external dllfile name 'yapiSetNetworkTimeout';
  function _yapiGetNetworkTimeout():integer; cdecl; external dllfile name 'yapiGetNetworkTimeout';
  function _yapiAddUdevRulesForYocto(force:integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiAddUdevRulesForYocto';
//--- (end of generated code: YFunction dlldef)


  function YISERR(retcode:YRETCODE):boolean;
    begin
      YISERR:= (retcode<0);
    end;

  procedure yDisableExceptions();
    begin
      YAPI_ExceptionsDisabled :=true;
    end;

  procedure yEnableExceptions();
    begin
      YAPI_ExceptionsDisabled :=false;
    end;

  procedure native_yLogFunction (log:pansichar;loglen:u32);cdecl;
    begin
      if assigned(ylog) then ylog(string(log));
    end;

  procedure native_yDeviceLogCallback (devdescr:YDEV_DESCR; line:pansichar);cdecl;
    var
        infos: yDeviceSt;
        modul: TYModule;
        errmsg: string;
        callback: TYModuleLogCallback;
    begin
        errmsg:='';
        if (yapiGetDeviceInfo(devdescr, infos, errmsg) <> YAPI_SUCCESS)then exit;
        modul := yFindModule(infos.serial + '.module');
        callback := modul.get_logCallback();
        if (addr(callback) <> nil) then
          begin
            callback(modul, string(line));
          end;
    end;



  procedure yRegisterLogFunction(logfun: yLogFunc);
    begin
      ylog := logfun;
    end;

type

  TyapiEventType = (YAPI_DEV_ARRIVAL,YAPI_DEV_REMOVAL,YAPI_DEV_CHANGE,
                    YAPI_FUN_UPDATE,YAPI_FUN_VALUE,YAPI_FUN_TIMEDREPORT,
                    YAPI_HUB_DISCOVERY,YAPI_DEV_CONFIGCHANGE,
                    YAPI_BEACON_CHANGE, YAPI_FUN_REFRESH);

  TyapiEvent = record
    eventtype: TyapiEventType;
    module   : TYmodule;
    fun   : TYFunction;
    fun_descr: YFUN_DESCR;
    value    : string[YOCTO_PUBVAL_LEN];
    timestamp: double;
    duration : double;
    data     : array[0..18] of byte;
    data_len : integer;
    serial   : string[YOCTO_SERIAL_LEN];
    url      : string[64];
    beacon   : integer;
  end;
  PyapiEvent = ^TyapiEvent;

var
  _PlugEvents : Tlist;
  _DataEvents : Tlist;


  procedure native_yDeviceArrivalCallback(d:YDEV_DESCR);cdecl;
    var
      infos  : yDeviceSt;
      event  : PyapiEvent;
      devent  : PyapiEvent;
      errmsg : string;
      i : integer;
      descriptor : YDEV_DESCR;
    begin
      errmsg:='';

      For i := 0 To _FunctionCallbacks.Count - 1 do
        begin
          descriptor := TYfunction(_FunctionCallbacks.items[i]).get_functionDescriptor();
          if descriptor = Y_FUNCTIONDESCRIPTOR_INVALID then
            begin
              devent := _yapiGetMem(sizeof(TyapiEvent));
              devent^.eventtype := YAPI_FUN_REFRESH;
              devent^.fun := TYfunction(_FunctionCallbacks.items[i]);
              _DataEvents.add(devent)
            end;
        end;
      TYDevice.PlugDevice(d);
      event := _yapiGetMem(sizeof(TyapiEvent));
      event^.eventtype    := YAPI_DEV_ARRIVAL;
      if(yapiGetDeviceInfo(d, infos, errmsg) <> YAPI_SUCCESS) then exit;
      event^.module := yFindModule(string(infos.serial)+'.module');
      event^.module.setImmutableAttributes(infos);
      if(assigned(yArrival)) then _PlugEvents.add(event)
      else _yapiFreeMem(event);
    end;


  procedure yRegisterDeviceArrivalCallback(arrivalCallback: yDeviceUpdateFunc);
    var
      m:tymodule;
      errmsg:string;
    begin
      errmsg:='';
      yArrival :=arrivalCallback;
      if (assigned(arrivalCallback)) then
        begin
          m:= yFirstModule();
          while (m<>nil) do
            begin
              if (m.isOnline()) then
                begin
                  yapiLockDeviceCallBack(errmsg);
                  native_yDeviceArrivalCallback(m.get_functionDescriptor());
                  yapiUnlockDeviceCallBack(errmsg);
                end;
              m:=m.nextModule();
            end;
        end;
    end;


  procedure native_yDeviceRemovalCallback (d:YDEV_DESCR);cdecl;
    var
      event  : PyapiEvent;
      infos  : yDeviceSt;
      errmsg : string;
    begin
      errmsg:='';
      if(not assigned(yRemoval)) then exit;
      event := _yapiGetMem(sizeof(TyapiEvent));
      event^.eventtype    := YAPI_DEV_REMOVAL;
      if(yapiGetDeviceInfo(d, infos, errmsg) <> YAPI_SUCCESS) then exit;
      event^.module := yFindModule(string(infos.serial)+'.module');
      if(assigned(yRemoval)) then _PlugEvents.add(event)
      else _yapiFreeMem(event);
    end;


  procedure yRegisterDeviceRemovalCallback(removalCallback: yDeviceUpdateFunc);
    begin
      yRemoval :=removalCallback;
    end;


  procedure native_yDeviceChangeCallback (d:YDEV_DESCR);cdecl;
    var
      infos  : yDeviceSt;
      errmsg : string;
      event  : PyapiEvent;
    begin
      errmsg:='';
      if(not assigned(yChange)) then exit;
      event := _yapiGetMem(sizeof(TyapiEvent));
      event^.eventtype    := YAPI_DEV_CHANGE;
      if(yapiGetDeviceInfo(d, infos, errmsg) <> YAPI_SUCCESS) then exit;
      event^.module := yFindModule(string(infos.serial)+'.module');
      if(assigned(yChange)) then   _PlugEvents.add(event)
      else _yapiFreeMem(event);
    end;


  procedure native_yDeviceConfigChangeCallback (d:YDEV_DESCR);cdecl;
    var
      infos  : yDeviceSt;
      errmsg : string;
      event  : PyapiEvent;
      hwid   : string;
      index  : integer;
      modul  : TYModule;
    begin
      errmsg:='';
      if(yapiGetDeviceInfo(d, infos, errmsg) <> YAPI_SUCCESS) then exit;
      hwid := string(infos.serial)+'.module';
      index := _moduleCallbackList.indexof(hwid);
      if (index > 0) and (Integer(_moduleCallbackList.Objects[index]) > 0) then
      begin
        modul := yFindModule(hwid);
        event := _yapiGetMem(sizeof(TyapiEvent));
        event^.eventtype    := YAPI_DEV_CONFIGCHANGE;
        event^.module := modul;
        _DataEvents.add(event);
      end;
   end;

  procedure native_yBeaconChangeCallback (d:YDEV_DESCR; beacon:integer);cdecl;
    var
      infos  : yDeviceSt;
      errmsg : string;
      event  : PyapiEvent;
      hwid   : string;
      index  : integer;
      modul  : TYModule;
    begin
      errmsg:='';
      if(yapiGetDeviceInfo(d, infos, errmsg) <> YAPI_SUCCESS) then exit;
      hwid := string(infos.serial)+'.module';
      index := _moduleCallbackList.indexof(hwid);
      if (index >= 0) and (Integer(_moduleCallbackList.Objects[index]) > 0) then
      begin
        modul := yFindModule(hwid);
        event := _yapiGetMem(sizeof(TyapiEvent));
        event^.eventtype    := YAPI_BEACON_CHANGE;
        event^.module := modul;
        event^.beacon := beacon;
        _DataEvents.add(event);
      end;
   end;


  procedure yRegisterDeviceChangeCallback(callback: yDeviceUpdateFunc);
    begin
      yChange :=callback;
      if assigned(yChange) then
        _yapiRegisterDeviceChangeCallback(native_yDeviceChangeCallback)
      else
        _yapiRegisterDeviceChangeCallback(nil);
    end;


  Procedure queuesCleanUp();
    var i:integer;
    begin
      for i:=0 to _PlugEvents.count-1 do _yapiFreeMem(_PlugEvents[i]);
      _PlugEvents.free();
      _PlugEvents:=nil;
      for i:=0 to _DataEvents.count-1 do _yapiFreeMem(_DataEvents[i]);
      _DataEvents.free();
      _DataEvents:=nil;
    end;



  procedure native_yFunctionUpdateCallback (f:YFUN_DESCR; data:pansichar);cdecl;
    var
      event  : PyapiEvent;
    begin
      event := _yapiGetMem(sizeof(TyapiEvent));
      event^.fun_descr:=f;
      if (data=nil) then
        event^.eventtype :=  YAPI_FUN_UPDATE
      else
        begin
          event^.eventtype :=  YAPI_FUN_VALUE;
          event^.value     := shortstring(data);
        end;
      _DataEvents.add(event)
    end;


  procedure native_yTimedReportCallback (f:YFUN_DESCR; timestamp:double; bytes:pansichar; len:integer; duration:double);cdecl;
    var
      event  : PyapiEvent;
    begin
      event := _yapiGetMem(sizeof(TyapiEvent));
      event^.fun_descr:=f;
      event^.eventtype := YAPI_FUN_TIMEDREPORT;
      event^.timestamp := timestamp;
      event^.duration := duration;
      move(bytes^,event^.data[0],len);
      event^.data_len := len;
      _DataEvents.add(event)
    end;


  procedure native_yapiHubDiscoveryCallbackFwd(serial:pansichar; url:pansichar);cdecl;
    var
      event : PyapiEvent;
    begin
      event := _yapiGetMem(sizeof(TyapiEvent));
      event^.eventtype := YAPI_HUB_DISCOVERY;
      event^.serial := shortstring(serial);
      event^.url := shortstring(url);
      _PlugEvents.add(event)
    end;


  procedure yRegisterHubDiscoveryCallback(hubDiscoveryCallback: YHubDiscoveryCallback);
    var
      error : string;
    begin
      _HubDiscoveryCallback := hubDiscoveryCallback;
      if assigned(_HubDiscoveryCallback) then
        yTriggerHubDiscovery(error);
    end;


  procedure yRegisterCalibrationHandler(calibType:integer;callback:yCalibrationHandler);
    var
      key   : string;
    begin
      key := intToStr(calibType);
      _calibhandlers.addObject(key, addr(callback));
    end;

  //  standard value calibration handler (n-points linear error correction).
  function  yLinearCalibrationHandler(rawValue:double; calibType: integer; params : TLongIntArray; rawValues,refValues:TDoubleArray):double;
    var
      npt:integer;
      x,adj:double;
      x2,adj2:double;
      i:integer;
    begin
      if calibType < YOCTO_CALIB_TYPE_OFS then
        begin
          npt:= calibType mod 10;
          if npt>high(rawValues) then npt:=high(rawValues)+1;
          if npt>high(refValues) then npt:=high(refValues)+1;
        end
      else
        begin
          npt:=high(refValues)+1;
        end;
      x:= rawValues[0];
      adj := refValues[0]-x;
      i:=0;
      while (rawValue>rawValues[i]) and (i+1<npt) do
        begin
          inc(i);
          x2:=x;
          adj2:=adj;
          x:=rawValues[i];
          adj:=refValues[i]-x;
          if  ((rawValue<x) and (x>x2)) then
            begin
              adj:=adj2+(adj-adj2)*(rawValue-x2)/(x-x2);
            end;
        end;
      yLinearCalibrationHandler:=rawValue + adj;
    end;

  function  yapiLockDeviceCallBack(var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror : pansichar;
    begin
      buffer[0]:=#0;perror:=@buffer;
      yapiLockDeviceCallBack := _yapiLockDeviceCallBack(perror);
      errmsg:=string(perror);
    end;

  function  yapiUnlockDeviceCallBack(var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror : pansichar;
    begin
      buffer[0]:=#0;perror:=@buffer;
      yapiUnlockDeviceCallBack := _yapiUnlockDeviceCallBack(perror);
      errmsg:=string(perror);
    end;

  function  yapiLockFunctionCallBack(var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror : pansichar;
    begin
      buffer[0]:=#0;perror:=@buffer;
      yapiLockFunctionCallBack := _yapiLockFunctionCallBack(perror);
      errmsg:=string(perror);
    end;

  function  yapiUnlockFunctionCallBack(var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror : pansichar;
    begin
      buffer[0]:=#0;perror:=@buffer;
      yapiUnlockFunctionCallBack := _yapiUnlockFunctionCallBack(perror);
      errmsg:=string(perror);
    end;

  function _getCalibrationHandler(calType:integer):yCalibrationHandler;
    var key:string;
      index :integer;
    begin
      key:=intToStr(calType);
      index:=_calibhandlers.indexof(key);
      if (index>=0) then
      begin
        _getCalibrationHandler:= yCalibrationHandler(_calibhandlers.objects[index]);
      end else _getCalibrationHandler:=nil;
    end;

  function _StrToByte(value: String): TByteArray;
    var
      i: integer;
      res: TByteArray;
    begin
      SetLength(res, length(value));
      for i := 0 to Length(value) - 1 do
        res[i] := ord(value[i + 1]);
      result := res;
    end;

  function _ByteToString(value: TByteArray): String;
    var
      i : integer;
      s : String;
      c : char;
    begin
      s := '';
      for i := Length(value)-1 Downto 0 do
        begin
          c := Chr(value[i]);
          s := c + s;
        end;
      result := s;
    end;

  function _decimalToDouble(val:u64):double;
    var
      exp,negate:integer;
      res:double;
      mantis:integer;
    begin
      negate:=0;
      mantis:= val and 2047;
      if (mantis=0) then
        begin
          _decimalToDouble := 0.0;
          exit;
        end;
      if(val>32767) then
        begin
          negate := 1;
          val := 65536-val;
        end
      else if (val <0  ) then
        begin
          negate :=1;
          val := val;
        end;
      exp := val shr 11;
      res := (mantis) * _decExp[exp];
      if(negate<>0) then
        _decimalToDouble := -res
      else
        _decimalToDouble := res;
    end;


  function _doubleToDecimal(val:double):u64;
    var
      decpow,negate:integer;
      compo,mant:double;
      res:u64;
    begin
      negate:=0;
      if ( val =0.0) then
        begin
          _doubleToDecimal :=0;
          exit;
        end;
      if(val <0) then
        begin
          negate := 1;
          val := -val;
        end;
      compo := val/1999.0;
      decpow := 0;
      while (compo > _decExp[decpow]) and (decpow <15 ) do
        decpow := decpow+1;
      mant := val /_decExp[decpow];
      if (decpow=15) and (mant > 2047.0) then
        res := (15 shl 11) + 2047 {overflow}
      else
        res := (decpow shl 11) + round(mant);
      if(negate<>0) then
        _doubleToDecimal := -res
      else
        _doubleToDecimal := res;
    end;


  function _decodeWords(sdat:string):TLongIntArray;
    var
      udat    : TLongIntArray;
      p       : integer;
      val,c   : integer;
      srcpos  : integer;
    begin
      p := 0;
      while p < length(sdat) do
        begin
          c := ord(sdat[p + 1]);
          p := p + 1;
          if c = 42 then // 42 == '*'
            val := 0
          else if c = 88 then // 88 == 'X'
            val :=  $0ffff
          else if c = 89 then // 99 == 'Y'
            val :=  $07fff
          else if (c >= 97) then // 97 == 'a'
            begin
              srcpos := length(udat) - 1 - (c - 97);
              if (srcpos < 0) then
                val := 0
              else
                val := udat[srcpos];
            end
          else
            begin
              if (p + 2 > length(sdat)) then
                begin
                  _decodeWords := udat;
                  exit;
                end;
              val := c - 48; //48 == '0'
              c := ord(sdat[p + 1]);
              p := p + 1;
              val := val + ((c - 48) shl 5); // 48 == '0'
              c := ord(sdat[p + 1]);
              p := p + 1;
              if ( c = 122) then // 122 == 'z'
                c := 92; // 92 == '\'
              val := val + ((c - 48) shl 10); // 48 == '0'
            end;
          SetLength(udat, length(udat)+1);
          udat[length(udat)-1] := val;
        end;
      _decodeWords := udat;
    end;

  function _decodeFloats(sdat:string):TLongIntArray;
    var
      idat    : TLongIntArray;
      p       : integer;
      val     : LongInt;
      sign    : integer;
      dec     : integer;
      decInc  : integer;
      c       : integer;
    begin
      p := 0;
      while p < length(sdat) do
        begin
          val := 0;
          sign := 1;
          dec := 0;
          decInc := 0;
          c := ord(sdat[p + 1]);
          p := p + 1;
          while (c <> 45) and ((c < 48) or (c > 57)) do // 45='-', 48='0', 57='9'
            begin
              if p >= length(sdat) then
                begin
                  _decodeFloats := idat;
                  exit;
                end;
              c := ord(sdat[p + 1]);
              p := p + 1;
            end;
          if c = 45 then // 45 == '-'
            begin
              if p >= length(sdat) then
                begin
                  _decodeFloats := idat;
                  exit;
                end;
              sign := -sign;
              c := ord(sdat[p + 1]);
              p := p + 1;
            end;
          while ((c >= 48) and (c <= 57)) or (c = 46) do // 48='0', 57='9', 46='.'
            begin
              if c = 46 then
                begin
                  decInc := 1;
                end
              else if dec < 3 then
                begin
                  val := val * 10 + (c - 48); // 48='0'
                  dec := dec + decInc;
                end;
              if p <length(sdat) then
                begin
                  c := ord(sdat[p + 1]);
                  p := p + 1;
                end
              else c := 0;
            end;
          if dec < 3 then
            begin
              if dec = 0 then val := val * 1000
              else if dec = 1 then val := val * 100
              else val := val * 10;
            end;
          SetLength(idat, length(idat)+1);
          idat[length(idat)-1] := sign*val;
        end;
      _decodeFloats := idat;
    end;

    function _atoi(val:string):integer;
    var
      p       : integer;
      start   : integer;
    begin
      p := 0;
      while ((p < length(val)) and (val[p + 1] =' ')) do
        begin
          p := p + 1;
        end;
      start := p;
      if ((p < length(val)) and ((val[p + 1] = '-') or (val[p + 1] = '+'))) then
        begin
          p := p + 1;
        end;
      while ((p < length(val)) and (val[p + 1] >= '0') and (val[p + 1] <= '9')) do
        begin
          p := p + 1;
        end;
      if (start < p) then
        begin
          val := copy(val, start + 1, p-start);
          _atoi := StrtoInt(val);
        end
      else
        begin
          _atoi := 0;
        end;
    end;


  function _bytesToHexStr(bytes: TByteArray; ofs:integer; len:integer):string;
    const
      hexArray : string = '0123456789ABCDEF';
    var
      hexChars : array of char;
      j : integer;
      v : integer;
    begin
      SetLength(hexChars, len * 2);
      j := 0;
      while (j < len) do
        begin
          v := bytes[j + ofs] And $0FF;
          hexChars[j * 2] := hexArray[v shr 4 + 1];
          hexChars[j * 2 + 1] := hexArray[v and $0F + 1];
          inc(j);
        end;
      if Length(hexChars)>0 then
        SetString(Result, PChar(@hexChars[0]), Length(hexChars))
      else
        Result := '';
    end;

  function _hexStrToBin(hex_str:string):TByteArray;
    var
      len : integer;
      res :TByteArray;
      i : integer;
      val, n, c : integer;
    begin
      len := Length(hex_str) div 2;
      SetLength(res, len);
      i := 0;
      while (i < len) do
        begin
          val := 0;
          for n := 0 to  1 do
            begin
              c := ord(hex_str[i * 2 + n + 1]);
              val := val shl 4;
              //57='9', 70='F', 102='f'
              if (c <= 57) Then
                val := val + c - 48
              else if (c <= 70) then
                val := val + c - 65 + 10
              else
                val := val + c - 97 + 10;
            end;
          res[i] := val And $FF;
          inc(i);
        end;
      _hexStrToBin := res;
    end;


  function _bytesMerge(array_a:TByteArray; array_b:TByteArray):TByteArray;
    var
      res : TByteArray;
    begin
      SetLength(res, length(array_a) + length(array_b));
      move(array_a[0], res[0],  length(array_a));
      move(array_b[0], res[length(array_a)], length(array_b));
      _bytesMerge := res;
    end;

  function _yapiBoolToStr(value:boolean):string;
    begin
      if (value) then _yapiBoolToStr:='1' else _yapiBoolToStr:='0';
    end;

  function _yapiFloatToStr(value:double):string;
    var
      res : string;
      rounded : LongInt;
      decim : integer;
    begin
      res := '';
      rounded := round(value * 1000);
      if(rounded < 0) then
        begin
          res := '-';
          rounded := -rounded;
        end;
      res := res + IntToStr(rounded div 1000);
      decim := rounded mod 1000;
      if decim > 0 then
        begin
          res := res + '.';
          if decim < 100 then res := res + '0';
          if decim < 10 then res := res + '0';
          if (decim mod 10) = 0 then decim := decim div 10;
          if (decim mod 10) = 0 then decim := decim div 10;
          res := res + IntToStr(decim);
        end;
      _yapiFloatToStr := res;
    end;

  function _stringSplit(str :String; delimiter :Char):TStringArray;
    var
      pos, lastpos ,nbchar, count : Integer;
      part : string;
    begin
      SetLength(Result, 0);
      nbchar := Length(str);
      if nbchar = 0 then
        Exit;
      pos := 1;
      count := 1;
      while pos <= nbchar do
        begin
        if char(str[pos]) = delimiter then
          begin
             inc(count);
          end;
        inc(pos)
        end;

      SetLength(Result, count);
      pos := 1;
      lastpos:=1;
      count :=0;
      while pos <= nbchar do
        begin
        if str[pos] = delimiter then
          begin
            part := Copy(str,lastpos, pos-lastpos);
            result[count] := part;
            inc(count);
            lastpos := pos + 1;
          end;
        inc(pos)
        end;
      part := Copy(str,lastpos, pos-lastpos);
      result[count] := part;
    end;


  function yGetAPIVersion():string;
    var
      version : string;
      apidate : string;
    begin
      yapiGetAPIVersion(version, apidate);
      yGetAPIVersion:= YOCTO_API_VERSION_STR + '.' + YOCTO_API_BUILD_NO + ' (' + version + ')';
    end;


  function yInitAPI(mode: integer; var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror : pansichar;
      res:   YRETCODE;
      i:integer;
    begin
      if (YAPI_apiInitialized) then
        begin
          yInitAPI := YAPI_SUCCESS;
          exit;
        end;
      buffer[0]:=#0;perror:=@buffer;
      res  := _yapiInitAPI(mode,perror);
      errmsg:=string(perror);
      if (YISERR(res)) then
        begin
          yInitAPI := res;
          exit;
        end;
      _yapiRegisterLogFunction(native_yLogFunction);
      _yapiRegisterDeviceLogCallback(native_yDeviceLogCallback);
      _yapiRegisterDeviceArrivalCallback(native_yDeviceArrivalCallback);
      _yapiRegisterDeviceRemovalCallback(native_yDeviceRemovalCallback);
      _yapiRegisterDeviceChangeCallback(native_yDeviceChangeCallback);
      _yapiRegisterDeviceConfigChangeCallback(native_yDeviceConfigChangeCallback);
      _yapiRegisterBeaconCallback(native_yBeaconChangeCallback);
      _yapiRegisterFunctionUpdateCallback(native_yFunctionUpdateCallback);
      _yapiRegisterTimedReportCallback(native_yTimedReportCallback);
      _yapiRegisterLogFunction(native_yLogFunction);
      _yapiRegisterHubDiscoveryCallback(native_yapiHubDiscoveryCallbackFwd);
      for i:=1 to 20  do
        begin
          yRegisterCalibrationHandler(i,yLinearCalibrationHandler);
        end;
      yRegisterCalibrationHandler(YOCTO_CALIB_TYPE_OFS,yLinearCalibrationHandler);
      YAPI_apiInitialized := true;
      yInitAPI         := res;
    end;

  procedure yFreeAPI();
    begin
      _yapiFreeAPI();
      TYFunction._ClearCache();
      YAPI_apiInitialized := false;
    end;

  function yRegisterHub(url:string;var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror : pansichar;
      res    : yretcode;
    begin
      res:= yInitAPI(0,errmsg);
      if yiserr(res) then
        begin
          yRegisterHub:=res;
          exit;
        end;
      buffer[0]:=#0;perror:=@buffer;
      yRegisterHub := _yapiRegisterHub(pansichar(ansistring(url)),perror);
      errmsg:=string(perror);
    end;

  function yPreregisterHub(url:string;var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror : pansichar;
      res    : yretcode;
    begin
      res:= yInitAPI(0,errmsg);
      if yiserr(res) then
        begin
          yPreregisterHub:=res;
          exit;
        end;
      buffer[0]:=#0;perror:=@buffer;
      yPreregisterHub := _yapiPreregisterHub(pansichar(ansistring(url)),perror);
      errmsg:=string(perror);
    end;

  procedure yUnregisterHub(url:string);
    begin
      _yapiUnregisterHub(pansichar(ansistring(url)));
    end;

  function yTestHub(url:string; mstimeout:integer; var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror : pansichar;
    begin
      buffer[0]:=#0;perror:=@buffer;
      yTestHub := _yapiTestHub(pansichar(ansistring(url)), mstimeout, perror);
      errmsg:=string(perror);
    end;


  function yUpdateDeviceList(var errmsg:string):Yretcode;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror : pansichar;
      res    : Yretcode;
      p      : PyapiEvent;
    begin
      buffer[0]:=#0;perror:=@buffer;
      res := _yapiUpdateDeviceList(0,perror);
      if (YISERR(res)) then
        begin
          errmsg:=string(perror);
        end
      else
        begin
          res := _yapiHandleEvents(perror);
          if (YISERR(res)) then
            begin
              errmsg:=string(perror);
            end;
        end;
      while (_plugEvents.count>0) do
        begin
          yapiLockDeviceCallBack(errmsg);
          p:=_plugEvents[0];
          _plugEvents.delete(0);
          yapiUnlockDeviceCallBack(errmsg);
          case (p^.eventType) of
            YAPI_DEV_ARRIVAL :
              if assigned(yArrival) then yArrival(p^.module);
            YAPI_DEV_REMOVAL:
              if assigned(yRemoval) then yRemoval(p^.module);
            YAPI_DEV_CHANGE:
              if assigned(yChange) then yChange(p^.module);
            YAPI_HUB_DISCOVERY:
              if assigned(_HubDiscoveryCallback) then
                begin
                  _HubDiscoveryCallback(string(p^.serial), string(p^.url));
                end;
          end;
          _yapiFreeMem(p);
        end;
      yUpdateDeviceList:=res;
    end;


  function yHandleEvents(var errmsg:string):yretcode;
    var
      errBuffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      pError    : pansichar;
      report    : TLongIntArray;
      sensor    : TYSensor;
      measure   : TYMeasure;
      res       : yretcode;
      p         : PyapiEvent;
      i,j       : integer;
    begin
      errBuffer[0]:=#0;pError:=@errBuffer;
      res := _yapiHandleEvents(perror);
      if (YISERR(res)) then
        begin
          errmsg := string(pError);
          yHandleEvents :=res;
          exit;
        end;
      while (_dataEvents.count>0) do
        begin
          yapiLockFunctionCallBack(errmsg);
          if  (_dataEvents.count=0) then    // not sure this if is really ussefull
            yapiUnlockFunctionCallBack(errmsg)
          else
            begin
              p:=_dataEvents[0];
              _dataEvents.delete(0);
              yapiUnlockFunctionCallBack(errmsg);
              If (p^.eventtype = YAPI_FUN_VALUE) Then
                begin
                  For i := 0 To _FunctionCallbacks.Count - 1 do
                    If (TYfunction(_FunctionCallbacks.items[i]).get_functionDescriptor() = p^.fun_descr) Then
                      TYfunction(_FunctionCallbacks.items[i])._invokeValueCallback(string(p^.value));
                end
              else If (p^.eventtype = YAPI_FUN_TIMEDREPORT) Then
                begin
                  For i := 0 To _TimedReportCallbackList.Count - 1 do
                    If (TYfunction(_TimedReportCallbackList.items[i]).get_functionDescriptor() = p^.fun_descr) Then
                    begin
                      if p^.data[0] <= 2 then
                        begin
                          SetLength(report, p^.data_len);
                          for j := 0 To p^.data_len-1 do report[j] := p^.data[j];
                          sensor := TYSensor(_TimedReportCallbackList.items[i]);
                          measure := sensor._decodeTimedReport(p^.timestamp, p^.duration, report);
                          sensor._invokeTimedReportCallback(measure);
                          measure.free();
                        end;
                    end;
                end
              else If (p^.eventtype = YAPI_DEV_CONFIGCHANGE) Then
                begin
                  p^.module._invokeConfigChangeCallback();
                end
              else If (p^.eventtype = YAPI_BEACON_CHANGE) Then
                begin
                  p^.module._invokeBeaconCallback(p^.beacon);
                end
              else If (p^.eventtype = YAPI_FUN_REFRESH) Then
                begin
                  p^.fun.isOnline();
                end;
              _yapiFreeMem(p);
            end;
        end;
      yHandleEvents:=YAPI_SUCCESS;
    end;


  function ySleep(ms_duration: integer; var errmsg:string):integer;
    var
      errBuffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      pError    : pansichar;
      timeout   : u64;
      res       : integer;
    begin
      timeout := yGetTickCount() + ms_duration;
      errBuffer[0]:=#0;pError:=@errBuffer;
      repeat
        res := yHandleEvents(errmsg);
        if YISERR(res) then
          begin
            ySleep :=res;
            exit;
          end;
        if (yGetTickCount() < timeout) then
          begin
            res := _yapiSleep(2,pError);
            if YISERR(res) then
              begin
                ySleep :=res;
                errmsg:=string(pError);
                exit;
              end;
          end;
      until yGetTickCount() >= timeout;
      errmsg := string(pError);
      ySleep := res;
    end;

  function yTriggerHubDiscovery(var errmsg: string):integer;
    var
      res :integer;
      errbuf : array[0..YOCTO_ERRMSG_LEN] of ansichar;
    begin
      res:= yInitAPI(0,errmsg);
      if yiserr(res) then
        begin
          result := res;
          exit;
        end;
      res := _yapiTriggerHubDiscovery(errbuf);
      if yiserr(res) then
        begin
          result := res;
          exit;
        end;
      result := YAPI_SUCCESS;
    end;

  function yGetTickCount():u64;
    begin
      yGetTickCount := _yapiGetTickCount();
    end;


  function yCheckLogicalName(name:string):boolean;
    begin
      if  (_yapiCheckLogicalName(pansichar(ansistring(name)))=0)  then
        yCheckLogicalName:=false
      else
        yCheckLogicalName:=true;
    end;


  (**
   *
   *)

  function yapiGetAPIVersion(var version,build_date:string):u16;
    var
      pversion,pbuild_date :pansichar;
      res:u16;
    begin
      res            :=  _yapiGetAPIVersion(pversion,pbuild_date);
      version        :=  string(pversion);
      build_date     :=  string(pbuild_date);
      yapiGetAPIVersion :=res;
    end;

  procedure yapiSetTraceFile(filename:string);
    begin
      _yapiSetTraceFile(pansichar(ansistring(filename)));
    end;

  function yapiGetDevice(device_str:string;var errmsg:string):YDEV_DESCR;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror :pansichar;
    begin
      buffer[0]:=#0;perror:=@buffer;
      yapiGetDevice := _yapiGetDevice(pansichar(ansistring(device_str)),perror);
      errmsg:=string(perror);
    end;

  function yapiGetDeviceInfo(d:YDEV_DESCR;var infos:yDeviceSt;var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror : pansichar;
    begin
      buffer[0]:=#0;perror:=@buffer;
      yapiGetDeviceInfo :=_yapiGetDeviceInfo(d,infos,perror);
      errmsg:=string(perror);
    end;

  function yapiGetFunction(class_str,function_str:string;var errmsg:string):YFUN_DESCR;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror :pansichar;
    begin
      buffer[0]:=#0;perror:=@buffer;
      yapiGetFunction := _yapiGetFunction(pansichar(ansistring(class_str)),pansichar(ansistring(function_str)),perror);
      errmsg:=string(perror);
    end;

  function yapiGetFunctionsByClass( class_str:string; precFuncDesc:YFUN_DESCR;dbuffer:PyHandleArray;maxsize:integer;var neededsize:integer; var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror :pansichar;
      tmp :integer;
    begin
      buffer[0]:=#0;perror:=@buffer;
      tmp:=_yapiGetFunctionsByClass(PansiChar(ansistring(class_str)),precFuncDesc, dbuffer,maxsize,neededsize,perror);
      yapiGetFunctionsByClass:=tmp;
      errmsg := string(perror);
    end;


  function yapiGetFunctionsByDevice( devdesc:YDEV_DESCR; precFuncDesc:YFUN_DESCR;dbuffer:PyHandleArray;maxsize:integer;var neededsize:integer; var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror :pansichar;
    begin
      buffer[0]:=#0;perror:=@buffer;
      yapiGetFunctionsByDevice:=_yapiGetFunctionsByDevice(devdesc,precFuncDesc, dbuffer,maxsize,neededsize,perror);
      errmsg := string(perror);
    end;




  function yapiGetFunctionInfoEx(fundesc:YFUN_DESCR;var devdesc:YDEV_DESCR;var serial,funcId,baseType,funcName,funcVal,errmsg : string):integer;
    var
      serialBuffer   : array[0..YOCTO_SERIAL_LEN] of ansichar;
      funcIdBuffer   : array[0..YOCTO_FUNCTION_LEN] of ansichar;
      baseTypeBuffer : array[0..YOCTO_FUNCTION_LEN] of ansichar;
      funcNameBuffer : array[0..YOCTO_LOGICAL_LEN] of ansichar;
      funcValBuffer  : array[0..YOCTO_PUBVAL_LEN] of ansichar;
      errBuffer      : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      pError,pSerial,pFuncId,pBaseType,pFuncname,pFuncVal : pansichar;
    begin
      serialBuffer[0]:=#0;  pSerial    := @serialBuffer;
      funcIdBuffer[0]:=#0;  pFuncId    := @funcIdBuffer;
      baseTypeBuffer[0]:=#0;pBaseType  := @baseTypeBuffer;
      funcNameBuffer[0]:=#0;pFuncname  := @funcNameBuffer;
      funcValBuffer[0]:=#0; pFuncVal   := @funcValBuffer;
      errBuffer[0]:=#0;     pError     := @errBuffer;
      yapiGetFunctionInfoEx :=_yapiGetFunctionInfoEx(fundesc,devdesc,pSerial,pFuncId,pBaseType,pFuncname,pFuncVal,pError);
      serial    := string(pSerial);
      funcId    := string(pFuncId);
      baseType  := string(pBaseType);
      funcName  := string(pFuncname);
      funcVal   := string(pFuncVal);
      errmsg    := string(perror);
    end;

    function yapiGetFunctionInfo(fundesc:YFUN_DESCR;var devdesc:YDEV_DESCR;var serial,funcId,funcName,funcVal,errmsg : string):integer;
    var
      serialBuffer   : array[0..YOCTO_SERIAL_LEN] of ansichar;
      funcIdBuffer   : array[0..YOCTO_FUNCTION_LEN] of ansichar;
      funcNameBuffer : array[0..YOCTO_LOGICAL_LEN] of ansichar;
      funcValBuffer  : array[0..YOCTO_PUBVAL_LEN] of ansichar;
      errBuffer      : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      pError,pSerial,pFuncId,pFuncname,pFuncVal : pansichar;
    begin
      serialBuffer[0]:=#0;  pSerial    := @serialBuffer;
      funcIdBuffer[0]:=#0;  pFuncId    := @funcIdBuffer;
      funcNameBuffer[0]:=#0;pFuncname  := @funcNameBuffer;
      funcValBuffer[0]:=#0; pFuncVal   := @funcValBuffer;
      errBuffer[0]:=#0;     pError     := @errBuffer;
      yapiGetFunctionInfo :=_yapiGetFunctionInfoEx(fundesc,devdesc,pSerial,pFuncId,nil,pFuncname,pFuncVal,pError);
      serial    := string(pSerial);
      funcId    := string(pFuncId);
      funcName  := string(pFuncname);
      funcVal   := string(pFuncVal);
      errmsg    := string(perror);
    end;


  function yapiGetDeviceByFunction(fundesc:YFUN_DESCR;var errmsg : string):integer;
    var
      errBuffer      : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      pError         : pansichar;
      devdesc        : YDEV_DESCR;
      res            : integer;
    begin
      errBuffer[0]:=#0;     pError     := @errBuffer;
      res    := _yapiGetFunctionInfoEx(fundesc,devdesc,nil,nil,nil,nil,nil,pError);
      errmsg := string(perror);
      if(res<0) then yapiGetDeviceByFunction := res
      else yapiGetDeviceByFunction := devdesc;
    end;





  function yapiHTTPRequestSync(device:string; request:TByteArray; var reply:TByteArray; var errmsg:string):YRETCODE;
    var
      iohdl  : YIOHDL;
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror,preply: pansichar;
      replysize : integer;
      res    : YRETCODE;
    begin
      buffer[0]:=#0;
      perror:=@buffer;
      replysize:=-1;
      res := _yapiHTTPRequestSyncStartEx( addr(iohdl),
                                          pansichar(ansistring(device)),
                                          pansichar(request),
                                          length(request),
                                          preply,replysize,perror);
      if(res<0) then
        begin
          errmsg:=string(perror);
          yapiHTTPRequestSync := res;
          exit;
        end;
      setlength( reply,replysize);
      if (preply <> NIL) then
        move(preply^,reply[0],replysize);
      res := _yapiHTTPRequestSyncDone(addr(iohdl),perror);
      errmsg := string(perror);
      yapiHTTPRequestSync := res;
    end;

  function yapiHTTPRequestAsync(device:string; request:TByteArray; var errmsg:string):YRETCODE;
    var
      buffer    : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror    : pansichar;
      res       : YRETCODE;
    begin
      buffer[0]:=#0;perror:=@buffer;
      res := _yapiHTTPRequestAsync(pansichar(ansistring(device)),pansichar(request),nil,nil,perror);
      errmsg := string(perror);
      yapiHTTPRequestAsync := res;
    end;

  function yapiGetDevicePath(devdesc: integer; var rootdevice :string ; var path : string ; var errmsg:String ) :integer;
    var
      buffer         : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      rbuffer        : array[0..YOCTO_SERIAL_LEN] of ansichar;
      perror         : pansichar;
      prootdevice    : pansichar;
      pathsize       : integer;
      neededsize,res : integer;
      ppath           : pansichar;
    begin
      pathsize := 1024;
      getmem(ppath,pathsize);
      perror := addr(buffer[0]);
      prootdevice :=   addr(rbuffer[0]);
      res:=_yapiGetDevicePath(devdesc,prootdevice, ppath, pathsize, neededsize,perror);
      if  (neededsize>pathsize)  then
        begin
          freemem(ppath);
          pathsize := neededsize;
          getmem(ppath,pathsize);
          res:=_yapiGetDevicePath(devdesc, prootdevice, ppath, pathsize, neededsize,perror)
        end;
      path:=string(ppath);
      freemem(ppath);
      rootdevice := string(prootdevice);
      errmsg:=string(perror);
      yapiGetDevicePath:=res;
    end;



  function yapiHTTPRequest(device:string; request:string; var answer:string; var errmsg:string):integer;
    var
      errbuffer    : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      initsize,res : integer;
      ptr          : Pansichar;
      devdesc      : integer;
      errbuf       : pansichar;
      neededsize   : integer;
      subpath      : string;
      slashpos     : integer;
      newrequest   : string;
      rootdevice   : string;
    begin
      initsize     := 2048;
      errbuffer[0] := #0;
      errbuf       := addr(errbuffer[0]);
      errmsg       := '';
      devdesc := yapiGetDevice(device,errmsg);
      if YISERR(devdesc) then
        begin
          yapiHTTPRequest := devdesc;
          exit;
        end;
      res := yapiGetDevicePath(devdesc,rootdevice, subpath, errmsg);
      if YISERR(res) then
        begin
          yapiHTTPRequest := res;
          exit;
        end;
      slashpos   :=  pos('/',request);
      newrequest :=  copy(request,1,slashpos-1 ) + subpath+  copy(request,slashpos+1,length(request)-slashpos);
      getmem(ptr,initsize);
      res := _yapiHTTPRequest(pansichar(ansistring(device)), pansichar(ansistring(newrequest)), ptr, initsize, neededsize, errbuf);
      if YISERR(res) then
        begin
          freemem(ptr);
          errmsg          := string(errbuf);
          yapiHTTPRequest := res;
          exit;
        end;
      if(neededsize > initsize) then
        begin
          initsize:=  neededsize;
          getmem(ptr,initsize);
          res := _yapiHTTPRequest(pansichar(ansistring(device)), pansichar(ansistring(newrequest)), ptr, initsize, neededsize, errbuf);
          if YISERR(res) then errmsg   := string(errbuf);
        end;
      answer          := string(ptr);
      freemem(ptr);
      yapiHTTPRequest := res;
    end;


 {$ifdef ENABLEPROGRAMMING}
  function  yapiFlashDevice(args:TyFlashArg; var errmsg : string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror :pansichar;
    begin
      buffer[0]:=#0; perror:=@buffer;
      yapiFlashDevice:=_yapiFlashDevice(@args,perror);
      errmsg:=string(perror);
    end;


  function  yapiVerifyDevice(args:TyFlashArg;var errmsg : string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror :pansichar;
    begin
      buffer[0]:=#0; perror:=@buffer;
      yapiVerifyDevice:=_yapiVerifyDevice(@args,perror);
      errmsg:=string(perror);
    end;
{$endif}


 {$Ifdef BUILTIN_YAPI}

 {$LINK 'yapi.obj'}
 {$LINK 'ymemory.obj'}
 {$LINK 'yprog.obj'}
 {$LINK 'ytcp.obj'}
 {$LINK 'ypkt_win.obj'}
 {$LINK 'yfifo.obj'}
 {$LINK 'yhash.obj'}
 {$LINK 'yjson.obj'}


 {$LINK 'fprintf.obj'}
 {$LINK 'fopen.obj'}
 {$LINK 'fwrite.obj'}
 {$LINK 'fclose.obj'}
 {$LINK 'xfclose.obj'}
 {$LINK 'fflush.obj'}
 {$LINK 'fputn.obj'}
 {$LINK 'unlink.obj'}
 {$LINK 'flushout.obj'}
 {$LINK 'fmode.obj'}
 {$LINK 'lputc.obj'}
 {$LINK '_read.obj'}
 {$LINK '_write.obj'}
 {$LINK '__open.obj'}
 {$LINK '__read.obj'}
 {$LINK '__write.obj'}
 {$LINK '__close.obj'}
 {$LINK '_read.obj'}
 {$LINK '_write.obj'}
 {$LINK '__lseek.obj'}
 {$LINK '_umask.obj'}
 {$LINK '_unlink.obj'}
 {$LINK '_fputc.obj'}
 {$LINK 'fmodeptr.obj'}
 {$LINK 'isatty.obj'}
 {$LINK '__isatty.obj'}
 {$LINK 'ioerror.obj'}
 {$LINK 'setvbuf.obj'}
 {$LINK 'mkname.obj'}
 {$LINK 'fmode.obj'}
 {$LINK 'perror.obj'}
 {$LINK 'fputs.obj'}
 {$LINK 'fseek.obj'}
 {$LINK 'allocbuf.obj'}

 {$LINK 'xfflush.obj'}
 {$LINK 'streams.obj'}
 {$LINK 'cfinfo.obj'}
 {$LINK 'files2.obj'}
 {$LINK 'files.obj'}
 {$LINK 'handles.obj'}

 {$LINK 'sprintf.obj'}
 {$LINK 'strcpy.obj'}
 {$LINK 'strcat.obj'}
 {$LINK 'strdup.obj'}
 {$LINK 'memset.obj'}
 {$LINK 'memmove.obj'}
 {$LINK 'memchr.obj'}
 {$LINK 'memcmp.obj'}
 {$LINK 'strnicmp.obj'}
 {$LINK 'strcmp.obj'}
 {$LINK 'atol.obj'}
 {$LINK 'ltoa1.obj'}
 {$LINK 'longtoa.obj'}
 {$LINK 'longtow.obj'}
 {$LINK 'strchr.obj'}
 {$LINK 'strncpy.obj'}
 {$LINK 'strlen.obj'}
 {$LINK 'strncmp.obj'}
 {$LINK 'memcpy.obj'}
 {$LINK 'stricmp.obj'}
 {$LINK 'strncat.obj'}
 {$LINK 'ltolower.obj'}
 {$LINK 'ltoupper.obj'}
 {$LINK 'clocale.obj'}
 {$LINK 'clower.obj'}
 {$LINK 'cupper.obj'}
 {$LINK 'tzset.obj'}

 {$LINK 'vprinter.obj'}
 {$LINK 'stpcpy.obj'}
 {$LINK '_stpcpy.obj'}
 {$LINK 'mbctype.obj'}
 {$LINK 'mbisalp.obj'}
 {$LINK 'mbisdgt.obj'}
 {$LINK 'mbsnbcpy.obj'}
 {$LINK 'mbsnbicm.obj'}
 {$LINK 'mbisspc.obj'}
 {$LINK 'cvtentry.obj'}
 {$LINK 'cvtfak.obj'}

 {$LINK '_tzset.obj'}
 {$LINK 'tzdata.obj'}
 {$LINK 'tzset.obj'}
 {$LINK 'isctype.obj'}
 {$LINK 'getenv.obj'}
 {$LINK 'setenvp.obj'}
 {$LINK 'calloc.obj'}
 {$LINK 'hrdir_s.obj'}
 {$LINK 'hrdir_mf.obj'}
 {$LINK 'hrdir_b.obj'}
 {$LINK 'realloc.obj'}
 {$LINK 'heap.obj'}
 {$LINK 'virtmem.obj'}
 {$LINK '_ll.obj'}
 {$LINK 'int64toa.obj'}
 {$LINK '_ll.obj'}
 {$LINK 'time.obj'}
 {$LINK 'timefunc.obj'}
 {$LINK 'timedata.obj'}
 {$LINK 'errormsg.obj'}
 {$LINK 'ermsghlp.obj'}
 {$LINK 'hrdir_s.obj'}
 {$LINK 'platform.obj'}
 {$LINK 'exit.obj'}
 {$LINK 'initexit.obj'}
 {$LINK 'patexit.obj'}
 {$LINK 'streams.obj'}

 {$LINK 'is.obj'}
 {$LINK 'isctype.obj'}
 {$LINK 'bigctype.obj'}
 {$LINK 'mbsnbicm.obj'}
 {$LINK 'mbsrchr.obj'}
 {$LINK 'mbyte1.obj'}
 {$LINK 'wcslen.obj'}
 {$LINK '_cfinfo.obj'}

 {$LINK 'access.obj'}
 {$LINK '__access.obj'}
 {$LINK 'globals.obj'}
 {$LINK 'errno.obj'}

 {$endif}


  // internal time convertions function (delphi 5 does not have dateutils unit)
  function _UNIXTimeToDateTime(UnixTime: LongWord): TDateTime;
    begin
      result := (UnixTime / 86400) + 25569;
    end;

  function _DateTimeToUNIXTime(DelphiTime : TDateTime): LongWord;
    begin
      result := Round((DelphiTime - 25569) * 86400);
    end;

  class function TYFunction._FindFromCache(classname: string; func: string): TYFunction;
    var
      index: integer;
    begin
      if _cache.Find(classname + '_' + func , index) then
        begin
          result := TYFunction(_cache.objects[index]);
          exit;
        end;
      result := nil;
    end;

  class procedure TYFunction._AddToCache(classname: string; func: string; obj: TYFunction);
    begin
      _cache.addObject(classname + '_' + func, obj);
    end;

  class procedure TYFunction._ClearCache();
    var
      i: integer;
    begin
      for i:=0 to _cache.count-1 do
        begin
          _cache.objects[i].free();
          _cache.objects[i]:=nil;
        end;
      _cache.free();
    end;


  constructor TYFunction.Create(func:string);
    begin
      _className       := 'Function';
      _func            := func;
      _lastErrorType   := YAPI_SUCCESS;
      _lastErrorMsg    := '';
      _cacheExpiration := 0;
      _fundescr        := Y_FUNCTIONDESCRIPTOR_INVALID;
      _userData        := nil;
      _dataStreams     := TStringList.create();
      _dataStreams.sorted := true;
      //--- (generated code: YFunction accessors initialization)
      _logicalName := Y_LOGICALNAME_INVALID;
      _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
      _valueCallbackFunction := nil;
      _cacheExpiration := 0;
      //--- (end of generated code: YFunction accessors initialization)
    end;

  destructor TYFunction.Destroy();
    var i: integer;
    begin
      for i := _dataStreams.count-1 downto 0 do
        begin
          _dataStreams.objects[i].free();
          _dataStreams.objects[i]:=nil;
        end;
      _dataStreams.free();
      inherited Destroy();
    end;


  Procedure TYFunction._throw(errType:YRETCODE; errMsg:string );
    begin
      _lastErrorType := errType;
      _lastErrorMsg  := errMsg;
      if not(YAPI_exceptionsDisabled) then
        Raise YAPI_Exception.Create(errType,'YoctoApi error : '+errMsg);
    end;


  // Method used to resolve our name to our unique function descriptor (may trigger a hub scan)
  function TYFunction._getDescriptor(var fundescr:YFUN_DESCR ; var errMsg:string):YRETCODE;
    var
      res:integer;
      tmp_fundescr :YFUN_DESCR;
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror : pansichar;
    begin
      tmp_fundescr:= yapiGetFunction(_className, _func, errmsg);
      if(YISERR(tmp_fundescr)) then
        begin
          buffer[0]:=#0;perror:=@buffer;
          res := _yapiUpdateDeviceList(1,perror);
          if (YISERR(res)) then
            begin
              errmsg:=string(perror);
              result:=res;
              exit;
            end;
          tmp_fundescr:= yapiGetFunction(_className, _func, errmsg);
          if(YISERR(tmp_fundescr)) then
            begin
              result:= tmp_fundescr; exit;
            end;
        end;
      fundescr := tmp_fundescr;
      _fundescr := tmp_fundescr;
      result:= YAPI_SUCCESS;
    end;

  // Return a pointer to our device caching object (may trigger a hub scan)
  function  TYFunction._getDevice(var dev:TYDevice; var errMsg:string):YRETCODE;
    var
      fundescr : YFUN_DESCR;
      devdescr : YDEV_DESCR;
      res      : YRETCODE;
    begin
      // Resolve function name
      res := _getDescriptor(fundescr, errmsg);
      if(YISERR(res)) then begin result:= res; exit;end;
      // Get device descriptor
      devdescr := yapiGetDeviceByFunction(fundescr, errmsg);
      if(YISERR(devdescr)) then begin result:= res; exit;end;
      // Get device object
      dev := TYDevice.getDevice(devdescr);
      result:= YAPI_SUCCESS;
    end;


  // Return the next known function of current class listed in the yellow pages
  function  TYFunction._nextFunction(var hwid:string):YRETCODE;
    var
      fundescr   : YFUN_DESCR;
      devdescr   : YDEV_DESCR;
      serial,funcId, funcName, funcVal, errmsg:string;
      res,count        : integer;
      neededsize,maxsize:integer;
      p:PyHandleArray;
    const
      n_element = 1;
    begin
      errmsg:='';
      res := _getDescriptor(fundescr, errmsg);
      if(YISERR(res)) then  begin   _throw(res, errmsg); result:=res; exit; end;
      maxsize :=n_element *sizeof(yHandle);
      getmem(p,maxsize);
      res:=yapiGetFunctionsByClass( _className,fundescr,p, maxsize,neededsize, errmsg);
      if (YISERR(res)) then begin   freemem(p);_throw(res,errmsg);result:=res; exit;end;
      count :=  neededsize div sizeof(yHandle) ;
      if count=0 then
        begin
          freemem(p);
          hwid:='';
          result:= YAPI_SUCCESS;
          exit;
        end;
      res := yapiGetFunctionInfo(p^[0], devdescr, serial, funcId, funcName, funcVal, errmsg);
      freemem(p);
      if (YISERR(res)) then begin   _throw(res,errmsg);result:=res; exit;end;
      hwid   := serial+'.'+funcId;
      result := YAPI_SUCCESS;
    end;

    function  TYFunction._escapeAttr(changeval:string):string;
      var
        i,c_ord,c_ord_next: integer;
        uchangeval : string;
        c          : char;
      begin
        i := 1;
        while i <= length(changeval) do
          begin
            c := changeval[i];
            if (c<' ') or ((c>#122) and (c<>'~')) or (c='"') or (c='%') or (c='&') or
            (c='+') or (c='<') or (c='=') or (c='>') or (c='\') or (c='^') or (c = '`')
            then
              begin
                c_ord := ord(c);
                if (((c_ord = $c2) or (c_ord = $c3)) and (i + 1 <= length(changeval)))then
                  begin
                    c_ord_next := ord(changeval[i + 1]) and $c0;
                    if (c_ord_next = $80) then
                      begin
                        { UTF8-encoded ISO-8859-1 character: translate to plain ISO-8859-1}
                        c_ord := (c_ord and 1) * $40;
                        i := i + 1;
                        c_ord := c_ord + ord(changeval[i]);
                      end;
                  end;
                uchangeval := uchangeval + '%' + IntToHex(c_ord, 2);
              end
            else
              uchangeval := uchangeval + c;
            i := i + 1;
          end;
        _escapeAttr := uchangeval
      end;

    function  TYFunction._buildSetRequest( changeattr : string ; changeval:string ; var request:string; var errmsg:string):YRETCODE;
      var
        res      : integer;
        fundesc  : YFUN_DESCR ;
        funcid   : array[0..YOCTO_FUNCTION_LEN] of ansichar;
        pfuncid  : pansichar;
        errbuff  : array[0..YOCTO_ERRMSG_LEN] of ansichar;
        perrbuff   : pansichar;
        uchangeval : string;
        devdesc:YDEV_DESCR;
      begin
        pfuncid  := addr(funcid[0]);
        perrbuff := addr(errbuff[0]);
        // Resolve the function name
        res := _getDescriptor(fundesc, errmsg);
        if(YISERR(res))  then
          begin
            _buildSetRequest :=res;
            exit;
          end;
        res:=_yapiGetFunctionInfoEx(fundesc, devdesc, NIL, pfuncid, NIL, NIL, NIL,perrbuff);
        if  YISERR(res) then
          begin
            errmsg := string(errbuff);
            _throw(res, errmsg);
            _buildSetRequest :=res;
            exit;
          end;
        request := 'GET /api/'+string(funcid)+'/';
        uchangeval :='';
        if (changeattr<>'')  then
          begin
            request := request+changeattr+'?'+changeattr+'='+_escapeAttr(changeval);
          end;
         request := request+ uchangeval+'&. '#13#10#13#10;     // no HTTP/1.1 to get light headers
        _buildSetRequest:= YAPI_SUCCESS;
      end;




    function TYFunction._parse(j:PJSONRECORD):integer;
      var
        member : PJSONRECORD;
        i      : integer;
      begin
        if (j^.recordtype <> JSON_STRUCT) then
          begin
            _parse:= -1;
            exit;
          end;
        for i:=0 to j^.membercount-1 do
          begin
            member := j^.members[i];
            _parseAttr(member);
          end;
        _parserHelper();
        _parse := 0;
      end;



  // Set an attribute in the function, and parse the resulting new function state
  function TYFunction._setAttr(attrname:string; newvalue:string):YRETCODE;
    var
      errmsg, request : string;
      res    : integer;
      dev    : tydevice;
      errBuffer       : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror          : pansichar;
    begin
      errmsg:='';
      // Execute http request
      res := _buildSetRequest(attrname, newvalue, request, errmsg);
      if(YISERR(res)) then
        begin
          _throw(res, errmsg);
          _setAttr:=res;
          exit;
        end;
      // Get device Object
      res := _getDevice(dev,  errmsg);
      if(YISERR(res)) then
        begin
          _throw(res, errmsg);
          _setAttr:=res;
          exit;
        end;
      res := dev.HTTPRequestAsync(request,errmsg);
      if(YISERR(res)) then
        begin
          // make sure a device scan does not solve the issue
          errBuffer[0]:=#0;perror:=@errBuffer;
          res := _yapiUpdateDeviceList(1,perror);
          if (YISERR(res)) then
            begin
              errmsg:=string(perror);
              result:=res;
              exit;
            end;
          res := dev.HTTPRequestAsync(request,errmsg);
          if(YISERR(res)) then
            begin
              _throw(res, errmsg);
              _setAttr:=res;
              exit;
            end;
        end;
      if (_cacheExpiration <> 0) then
        begin
          _cacheExpiration := yGetTickCount;
        end;
      _setAttr := YAPI_SUCCESS;
    end;

  // Method used to send http request to the device (not the function)
  function  TYFunction._request(request: string): TByteArray;
    begin
      result := self._request(_StrToByte(request));
    end;


  // Method used to send http request to the device (not the function)
  function  TYFunction._request(request: TByteArray): TByteArray;
    var
      dev           : TYDevice;
      errmsg        : string;
      buffer,check  : TByteArray;
      res           : integer;
      errBuffer     : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror        : pansichar;

    begin
      errmsg:='';
      // Resolve our reference to our device, load REST API
      res := _getDevice(dev, errmsg);
      if(YISERR(res)) then
        begin
          _throw(res, errmsg);
          setlength(result,0);
          exit;
        end;
      res := dev.HTTPRequest(request, buffer, errmsg);
      if(YISERR(res))  then
        begin
          // Check if an update of the device list does notb solve the issue
          errBuffer[0]:=#0;perror:=@errBuffer;
          res := _yapiUpdateDeviceList(1,perror);
          if(YISERR(res)) then
            begin
              errmsg:=string(perror);
              _throw(res,errmsg);
              setlength(result,0);
              exit;
            end;
          res := dev.HTTPRequest(request, buffer, errmsg);
          if(YISERR(res)) then
            begin
              _throw(res,errmsg);
            setlength(result,0);
            exit;
            end;
        end;
      if length(buffer) >= 4 then
        begin
          setLength(check, 4);
          move(buffer[0],check[0],4);
          if _ByteToString(check) = 'OK'#13#10 then
            begin
              result := buffer;
              exit;
            end;
          if length(buffer) >= 17 then
            begin
              setLength(check, 17);
              move(buffer[0],check[0],17);
              if _ByteToString(check) = 'HTTP/1.1 200 OK'#13#10 then
                begin
                  result := buffer;
                  exit;
                end;
            end;
        end;
      _throw(YAPI_IO_ERROR,'http request failed');
      setlength(result,0);
    end;

  function TYFunction._strip_http_header(buffer:TByteArray):TByteArray;
   var
      found,i,j :integer;
      body      :integer;
      res       :TByteArray;
      isfound   :boolean;
      tmp :string;
    begin
      if length(buffer) =0 then
        begin
          result := buffer;
          exit;
        end;
      tmp := _ByteToString(buffer);
      isfound := false;
      found := 0;
      while not(isfound) and (found < length(buffer) - 4) do
        begin
          if (buffer[found] = 13) and (buffer[found + 1] = 10) and (buffer[found + 2] = 13) and (buffer[found + 3] = 10) then
            isfound := true
          else
            inc(found);
        end;
      if(found > length(buffer) - 4) then
        begin
          _throw(YAPI_IO_ERROR,'http request failed');
          setlength(result,0);
          exit;
        end;
      body := found +4;
      setLength(res,length(buffer)-body);
      j:=0;
      for i:=body to length(buffer)-1 do
        begin
          res[j] := ord(buffer[i]);
          inc(j);
        end;
      result := res;
    end;


  // Method used to send http request to the device (not the function)
  function TYFunction._download(path:string):TByteArray;
    var
      request   :string;
      buffer    :TByteArray;
    begin
      request := 'GET /'+path+' HTTP/1.1'#13#10#13#10;
      buffer := self._request(request);
      result := _strip_http_header(buffer);
    end;

  function  TYFunction._upload(path: string; strcontent: string) :integer;
    begin
      result :=  _upload(path,  _StrToByte(strcontent));
    end;

  // Method used to upload a file to the device
  function  TYFunction._upload(path: string; content: TByteArray) :integer;
    var
      buffer :TByteArray;
    begin

      buffer  := _uploadEx(path, content);
      if length(buffer) = 0 then
        begin
          result := YAPI_IO_ERROR;
          exit;
        end;
      result := YAPI_SUCCESS;
    end;

 // Method used to upload a file to the device
  function  TYFunction._uploadEx(path: string; content: TByteArray) :TByteArray;
    var
      buffer,bb,header,footer :TByteArray;
      body,fullrequest        :TByteArray;
      boundary,bodystr  :string;
      i,pos       :integer;
      randomnum         :integer;
    begin
      bodystr := 'Content-Disposition: form-data; name="'+
        path + '"; filename="api"'#13#10 +
        'Content-Type: application/octet-stream'#13#10+
        'Content-Transfer-Encoding: binary'#13#10#13#10;

      setLength(body,length(bodystr) + length(content));
      Move(_StrToByte(bodystr)[0], body[0], Length(bodystr));
      Move(content[0], body[Length(bodystr)], length(content));
      repeat
        randomnum := trunc(random(899999)+100000);
        boundary := 'Zz' + inttostr(randomnum) + 'zZ';
        bb := _StrToByte(boundary);
        pos := 0 ;
        while pos <= (length(body) - length(bb)) do
          begin
            if body[pos] = 90 then
              begin
                i := 1;
                while (i < length(bb)) and (body[pos + i] = bb[i]) do
                  inc(i);
                if i >= length(bb) then
                  break;
              end;
            pos := pos + 1;
          end;
      until not(pos <= (length(body) - length(bb))) ;
      header := _StrToByte('POST /upload.html HTTP/1.1'#13#10 +
                          'Content-Type: multipart/form-data; boundary='+boundary+#13#10 +
                          #13#10'--' + boundary + #13#10);

      footer := _StrToByte(#13#10'--' + boundary + '--'#13#10);
      setLength(fullrequest, length(header) + length(body) + length(footer));
      Move(header[0], fullrequest[0], Length(header));
      Move(body[0],   fullrequest[Length(header)], length(body));
      Move(footer[0], fullrequest[Length(header) + length(body)], length(footer));
      buffer  := _request(fullrequest);
      result := _strip_http_header(buffer);
    end;


  function  TYFunction._findDataStream(dataset: TYDataSet; def :string) : TYDataStream;
    var
      index : integer;
      key : string;
      newDataStream : TYDataStream;
      words : TLongIntArray;
    begin
      key := dataset.get_functionId() + ':' + def;
      if _dataStreams.Find(key, index) then
        begin
          result := TYDataStream(_dataStreams.objects[index]);
          exit;
        end;
      words := _decodeWords(def);
      if length(words) < 14 then
        begin
          self._throw(YAPI_VERSION_MISMATCH, 'device firmware is too old');
          result := NIL;
          exit;
        end;
      newDataStream := TYDataStream.Create(self, dataset, words);
      _dataStreams.addObject(key, newDataStream);
      result :=newDataStream;
  end;

  procedure  TYFunction._clearDataStreamCache();
    begin
      _dataStreams.Free;
    end;


//--- (generated code: YFunction implementation)
{$HINTS OFF}
  function TYFunction._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'logicalName') then
        begin
          _logicalName := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'advertisedValue') then
        begin
          _advertisedValue := string(member^.svalue);
         result := 1;
         exit;
         end;
      result := 0;
    end;
{$HINTS ON}

  function TYFunction.get_logicalName():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LOGICALNAME_INVALID;
              exit;
            end;
        end;
      res := self._logicalName;
      result := res;
      exit;
    end;


  function TYFunction.set_logicalName(newval:string):integer;
    var
      rest_val: string;
    begin
      if (Not(yCheckLogicalName(newval))) then
        begin
          _throw(YAPI_INVALID_ARGUMENT,'Invalid name :' + newval);
          result := YAPI_INVALID_ARGUMENT;
          exit;
        end;
      rest_val := newval;
      result := _setAttr('logicalName',rest_val);
    end;

  function TYFunction.get_advertisedValue():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ADVERTISEDVALUE_INVALID;
              exit;
            end;
        end;
      res := self._advertisedValue;
      result := res;
      exit;
    end;


  function TYFunction.set_advertisedValue(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('advertisedValue',rest_val);
    end;

  class function TYFunction.FindFunction(func: string):TYFunction;
    var
      obj : TYFunction;
    begin
      obj := TYFunction(TYFunction._FindFromCache('Function', func));
      if obj = nil then
        begin
          obj :=  TYFunction.create(func);
          TYFunction._AddToCache('Function',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYFunction.registerValueCallback(callback: TYFunctionValueCallback):LongInt;
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
      self._valueCallbackFunction := callback;
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


  function TYFunction._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackFunction) <> nil) then
        begin
          self._valueCallbackFunction(self, value);
        end
      else
        begin
        end;
      result := 0;
      exit;
    end;


  function TYFunction.muteValueCallbacks():LongInt;
    begin
      result := self.set_advertisedValue('SILENT');
      exit;
    end;


  function TYFunction.unmuteValueCallbacks():LongInt;
    begin
      result := self.set_advertisedValue('');
      exit;
    end;


  function TYFunction.loadAttribute(attrName: string):string;
    var
      url : string;
      attrVal : TByteArray;
    begin
      url := 'api/'+ self.get_functionId+'/'+attrName;
      attrVal := self._download(url);
      result := _ByteToString(attrVal);
      exit;
    end;


  function TYFunction.isReadOnly():boolean;
    var
      serial : string;
      errmsg_buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      errmsg : pansichar;
      res : LongInt;
    begin
      errmsg_buffer[0]:=#0;errmsg:=@errmsg_buffer;
      Try
        serial := self.get_serialNumber;
      Except
        result := true;
        exit;
      End;
      res := _yapiIsModuleWritable(pansichar(ansistring(serial)), errmsg);
      if res > 0 then
        begin
          result := false;
          exit;
        end;
      result := true;
      exit;
    end;


  function TYFunction.get_serialNumber():string;
    var
      m : TYModule;
    begin
      m := self.get_module;
      result := m.get_serialNumber();
      exit;
    end;


  function TYFunction._parserHelper():LongInt;
    begin
      result := 0;
      exit;
    end;


  function TYFunction.nextFunction(): TYFunction;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextFunction := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextFunction := nil;
          exit;
        end;
      nextFunction := TYFunction.FindFunction(hwid);
    end;

  class function TYFunction.FirstFunction(): TYFunction;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Function', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYFunction.FindFunction(serial+'.'+funcId);
    end;

//--- (end of generated code: YFunction implementation)


  // Return a unique hardware identifier for the device
  function TYFunction.get_hardwareId():string;
    var
      retcode: YRETCODE    ;
      fundesc : YFUN_DESCR  ;
      devdesc:YDEV_DESCR ;
      funcName,funcVal:string ;
      errmsg : string      ;
      snum:string;
      funcid:string;
      errbuff:string;
    begin
      errmsg:='';
      // Resolve the function name
      retcode := _getDescriptor(fundesc, errmsg);
      if(YISERR(retcode)) then
        begin
          _throw(retcode, errmsg);
          get_hardwareId:=  Y_HARDWAREID_INVALID;
          exit;
        end;
      retcode:=yapiGetFunctionInfo(fundesc, devdesc, snum, funcid, funcName,funcVal,errbuff) ;
      if(YISERR(retcode)) then
        begin
          errmsg := errbuff;
          _throw(retcode, errmsg);
          get_hardwareId := Y_HARDWAREID_INVALID;
          exit;
        end;
      get_hardwareId := snum+'.'+funcid;
    end;


  // Return a unique hardware identifier for the device
  function TYFunction.get_functionId():string;
    var
      retcode: YRETCODE    ;
      fundesc : YFUN_DESCR  ;
      devdesc:YDEV_DESCR ;
      funcName,funcVal:string ;
      errmsg : string      ;
      snum:string;
      funcid:string;
      errbuff:string;
    begin
      errmsg:='';
      // Resolve the function name
      retcode := _getDescriptor(fundesc, errmsg);
      if(YISERR(retcode)) then
        begin
          _throw(retcode, errmsg);
          get_functionId:=  Y_FUNCTIONID_INVALID;
          exit;
        end;
      retcode:=yapiGetFunctionInfo(fundesc, devdesc, snum, funcid, funcName,funcVal,errbuff) ;
      if(YISERR(retcode)) then
        begin
          errmsg := errbuff;
          _throw(retcode, errmsg);
          get_functionId := Y_FUNCTIONID_INVALID;
          exit;
        end;
      get_functionId := funcid;
    end;


  // Return a unique hardware identifier for the device
  function TYFunction.get_friendlyName():string;
    var
      retcode: YRETCODE;
      fundesc : YFUN_DESCR;
      moddesc : YFUN_DESCR;
      devdesc:YDEV_DESCR ;
      funcName,funcVal:string ;
      errmsg : string      ;
      snum:string;
      funcid:string;
      errbuff:string;
      friendMod:string;
      friendFun:string;
    begin
      errmsg:='';
      // Resolve the function name
      retcode := _getDescriptor(fundesc, errmsg);
      if(YISERR(retcode)) then
        begin
          _throw(retcode, errmsg);
          get_friendlyName:=  Y_FRIENDLYNAME_INVALID;
          exit;
        end;
      retcode:=yapiGetFunctionInfo(fundesc, devdesc, snum, funcid, funcName,funcVal,errbuff) ;
      if(YISERR(retcode)) then
        begin
          errmsg := errbuff;
          _throw(retcode, errmsg);
          get_friendlyName := Y_FRIENDLYNAME_INVALID;
          exit;
        end;
      if (funcName <> '') then
        friendFun :=funcName
      else
        friendFun := funcid;
      moddesc:= yapiGetFunction('Module', snum, errmsg);
      if(YISERR(moddesc)) then
        begin
          _throw(retcode, errmsg);
          get_friendlyName:=  Y_FRIENDLYNAME_INVALID;
          exit;
        end;
      retcode:=yapiGetFunctionInfo(moddesc, devdesc, snum, funcid, funcName,funcVal,errbuff) ;
      if(YISERR(retcode)) then
        begin
          errmsg := errbuff;
          _throw(retcode, errmsg);
          get_friendlyName := Y_FRIENDLYNAME_INVALID;
          exit;
        end;
      if (funcName <> '') then
        friendMod :=funcName
      else
        friendMod := snum;
      get_friendlyName := friendMod+'.'+friendFun;
    end;



  // Return a string that describes the function (class and logical name or hardware id)
  function TYFunction.describe():string;
    var
      fundescr:YFUN_DESCR;
      devdescr:YDEV_DESCR;
      errmsg, serial, funcId, funcName, funcValue:string;
    begin
      errmsg:='';
      fundescr := yapiGetFunction(_className, _func, errmsg);
      if(not(YISERR(fundescr)))  then
      if(not(YISERR(yapiGetFunctionInfo(fundescr, devdescr, serial, funcId, funcName, funcValue, errmsg)))) then
      begin
        describe := _className + '(' + _func+')='+serial+'.'+funcid;
        exit;
      end;
      describe := _className + '(' + _func+')=unresolved';
    end;


  function TYFunction.ToString() :string ;
  begin
    ToString:=describe();
  end;


  // Return the numerical error type of the last error with this function
  function TYFunction.get_errorType():YRETCODE;
    begin
      result:= _lastErrorType;
    end;

  function TYFunction.errorType():YRETCODE;
    begin
      result:= _lastErrorType;
    end;

  function TYFunction.errType():YRETCODE;
    begin
      result:= _lastErrorType;
    end;

  // Return the human-readable explanation about the last error with this function
  function TYFunction.get_errorMessage():string;
    begin
      result:= _lastErrorMsg;
    end;

  function TYFunction.errorMessage():string;
    begin
      result:= _lastErrorMsg;
    end;

  function TYFunction.errMessage():string;
    begin
      result:= _lastErrorMsg;
    end;

  // Return true if the function can be reached, and false otherwise. No exception will be raised.
  // If there is a valid value in cache, the device is considered reachable.
  function TYFunction.isOnline():boolean;
    var
      dev:TYDevice ;
      errmsg :string;
      apires :TJsonParser;
    begin
      errmsg:='';
      // A valid value in cache means that the device is online
      if(_cacheExpiration > yGetTickCount()) then
      begin result:= true; exit;end;
      // Check that the function is available, without throwing exceptions
      if(YISERR(_getDevice(dev, errmsg))) then
      begin result:=false;exit;end;
      // Try to execute a function request to be positively sure that the device is ready
      if(YISERR(dev.requestAPI(apires, errmsg)))  then
      begin result:=false;exit;end;
      self.load(_yapicontext.GetCacheValidity());
      result :=true;
    end;

  function   TYFunction._json_get_key(data: TByteArray; key: string): string;
    var
      node  : PJSONRECORD;
      st    : string;
      p     : TJSONparser;
      size,i : integer;
    begin
      size:=length(data);
      setlength(st,size);
      for i:=0 to size-1 do
        st[i+1]:=chr(data[i]);

      if not(YAPI_ExceptionsDisabled) then  p := TJsonParser.create(st, false)
      else
      try
        p := TJsonParser.create(st, false)
      except
        on E: Exception do
          begin
            result:='';
            exit;
          end;
      end;

      node := p.GetChildNode(nil,key);
      _json_get_key := string(node^.svalue);
      p.free();
    end;

  function  TYFunction._json_get_array(data: TByteArray):TStringArray;
    var
      st  : string;
      p   : TJSONparser;
      res : TStringArray;
      size,i:integer;
    begin
      size:=length(data);
      setlength(st,size);
      for i:=0 to size-1 do
        st[i+1]:=chr(data[i]);


      if not(YAPI_ExceptionsDisabled) then  p := TJsonParser.create(st, false)
      else
      try
        p := TJsonParser.create(st, false)
      except
        on E: Exception do
          begin
            result:=nil;
            exit;
          end;
      end;


      res:=TStringArray(p.GetAllChilds(nil));
      p.free();
      _json_get_array := res;
    end;

  function  TYFunction._get_json_path(json, path: string):string;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror, p: pansichar;
      dllres    : YRETCODE;
      res : string;
    begin
      buffer[0]:=#0;
      perror:=@buffer;
      dllres := _yapiJsonGetPath( pansichar(ansistring(path)),
                                          pansichar(ansistring(json)),
                                          length(json),
                                          p, perror);
      if(dllres<=0) then
        begin
          _get_json_path := '';
          exit;
        end;
      res := string(p);
      SetLength(res, dllres);
      _get_json_path := res;
    end;


  function  TYFunction._decode_json_string(json: string):string;
    var
      bigbuff : pansichar;
      size : LongInt;
      res : LongInt;
    begin
      size := length(json);
      if size = 0 then
         _decode_json_string := ''
      else
        begin
          getmem(bigbuff, size);
          res := _yapiJsonDecodeString(pansichar(ansistring(json)), bigbuff);
          if (res>0) then
            _decode_json_string := string(bigbuff)
          else
            _decode_json_string := '';
          freemem(bigbuff);
        end;
    end;

  function   TYFunction._json_get_string(data: TByteArray):string;
    var
      node  : PJSONRECORD;
      st    : string;
      p     : TJSONparser;
    begin
      st := '[' + _ByteToString(data)+ ']';
      p := TJSONparser.create(st,false);
      node := p.GetRootNode();
      _json_get_string := string(node^.items[0].svalue);
      p.free();
    end;


  // Preload the function cache with a specified validity duration.
  // Note: the function cache is a typed (parsed) cache, contrarily to the agnostic device cache
  function TYFunction.load(msValidity:u64):YRETCODE;
    var
      dev            : TYDevice;
      errmsg         : string;
      apires         : TJsonParser;
      fundescr       : YFUN_DESCR;
      res            : integer;
      errbuf         : string;
      funcId         : string;
      devdesc        : YDEV_DESCR;
      serial         : string;
      funcName,funcVal:string;
      node: PJSONRECORD;
    begin
      errmsg:='';
      // Resolve our reference to our device, load REST API
      res := _getDevice(dev, errmsg);
      if(YISERR(res))  then begin _throw(res, errmsg); result:= res;exit;end;
      res := dev.requestAPI(apires, errmsg);
      if(YISERR(res))  then begin _throw(res, errmsg); result:= res;exit;end;
      // Get our function Id
      fundescr := yapiGetFunction(_className, _func, errmsg);
      if(YISERR(fundescr))  then begin _throw(res, errmsg); result:= res;exit;end;
      devdesc:=0;
      res := yapiGetFunctionInfo(fundescr, devdesc, serial, funcId, funcName, funcVal, errbuf);
      if(YISERR(res))  then begin _throw(res, errmsg); result:= res;exit;end;
      _cacheExpiration := yGetTickCount() + msValidity;
      _serial := serial;
      _funId := funcId;
      _hwId := _serial + '.' + _funId;
      node:=apires.getChildNode(nil,funcId);
      if (node=nil) then
        begin
          _throw(YAPI_IO_ERROR,'unexpected JSON structure: missing function '+funcId);
          result:=YAPI_IO_ERROR;
          exit;
        end;
      _parse(node);
      result := YAPI_SUCCESS;
    end;

  procedure TYFunction.clearCache();
    var
      dev            : TYDevice;
      errmsg         : string;
      res            : integer;
    begin
      errmsg:='';
      // Resolve our reference to our device, load REST API
      res := _getDevice(dev, errmsg);
      if(YISERR(res))  then
        exit;
      dev.clearCache();
      if (_cacheExpiration <> 0) then
        _cacheExpiration := yGetTickCount();
    end;

  // Return the YModule object for the device on which the function is located
  function  TYFunction.get_module():TYModule;
  var
    fundescr   : YFUN_DESCR;
    devdescr   : YDEV_DESCR;
    errmsg, serial, funcId, funcName, funcValue : string;
  begin
    errmsg:='';
    if not(_serial = '') then
        begin
          result :=  yFindModule(_serial + '.module');
          exit;
        end;
    fundescr := yapiGetFunction(_className, _func, errmsg);
    if (not(YISERR(fundescr)))  then
    if(not(YISERR(yapiGetFunctionInfo(fundescr, devdescr, serial, funcId, funcName, funcValue, errmsg)))) then
      begin
        result :=  yFindModule(serial+'.module');
        exit;
      end;
    // return a true YModule object even if it is not a module valid for communicating
    result:= yFindModule('module_of_'+_className+'_'+_func);
  end;

  (**
  *
  *
  *
  *)
  function TYFunction.get_functionDescriptor():YFUN_DESCR;
    begin
      get_functionDescriptor:=_fundescr;
    end;

  (**
  *
  *
  *
  *)
  function TYFunction.get_userData():Tobject;
    begin
      get_userdata := _userData;
    end;

  (**
   *
   *
   *
   *)
  procedure TYFunction.set_userData(data : Tobject);
    begin
      _userdata:=data;
    end;

  class procedure TYFunction._UpdateValueCallbackList(func : TYFunction; add : boolean);
    begin
      if add then
        begin
          func.isOnline();
          If  _FunctionCallbacks.IndexOf(func)<0 Then
            _FunctionCallbacks.Add(func)
        end
      else
        begin
          _FunctionCallbacks.Remove(func);
        end;
    end;

  class procedure TYFunction._UpdateTimedReportCallbackList(func : TYFunction; add : boolean);
    begin
      if add then
        begin
          func.isOnline();
          If  _TimedReportCallbackList.IndexOf(func)<0 Then
            _TimedReportCallbackList.Add(func)
        end
      else
        begin
          _TimedReportCallbackList.Remove(func);
        end;
    end;

  function TYModule.get_logicalName_internal():string;
    begin
      get_logicalName_internal:=_logicalName;
    end;

  procedure TYModule.setImmutableAttributes(var infos : yDeviceSt);
    begin
      _serialNumber := string(pansichar(addr(infos.serial)));
      _productName  := string(pansichar(addr(infos.productname)));
      _productId    := infos.deviceid;
      self._cacheExpiration := yGetTickCount;
    end;

  // Return the properties of the nth function of our device
  function TYModule._getFunction(idx:integer; var serial,funcId,baseType,funcName,funcVal,errmsg:string):YRETCODE;
    var
      functions  : tlist;
      dev        : TYDevice;
      res        : integer;
      fundescr   : YFUN_DESCR;
      devdescr   : YDEV_DESCR;
    begin
      // retrieve device object
      res := _getDevice(dev, errmsg);
      if(YISERR(res)) then
        begin
          _throw(res, errmsg);
          result:=res;
          exit;
        end;
      // get reference to all functions from the device
      res := dev.getFunctions(functions, errmsg);
      if(YISERR(res))  then begin result:=res;exit;end;
      // get latest function info from yellow pages
      fundescr := YFUN_DESCR(functions.items[idx]);
      res := yapiGetFunctionInfoEx(fundescr, devdescr, serial, funcId, baseType, funcName, funcVal, errmsg);
      if(YISERR(res))then begin   result:=res;exit;end;
      result := YAPI_SUCCESS;
    end;

  // Retrieve the number of functions (beside "module") in the device
  function TYModule.functionCount():integer;
    var
      functions : tlist;
      dev       : TYDevice;
      errmsg    : string;
      res       : integer;
    begin
      errmsg:='';
      res := _getDevice(dev, errmsg);
      if(YISERR(res)) then begin _throw(res, errmsg); result:= res;exit;end;
      res := dev.getFunctions(functions, errmsg);
      if(YISERR(res)) then begin functions.free(); _throw(res, errmsg); result:= res;exit;end;
      result:= functions.count;
    end;

  // Retrieve the Id of the nth function (beside "module") in the device
  function TYModule.functionId(functionIndex:integer):string;
    var
      serial, funcId, baseType, funcName, funcVal, errmsg:string;
      res:integer;
    begin
      errmsg:='';
      res := _getFunction(functionIndex, serial, funcId, baseType, funcName, funcVal, errmsg);
      if(YISERR(res)) then
        begin
          _throw(res, errmsg);
          result:=YAPI_INVALID_STRING;
          exit;
        end;
      result:= funcId;
    end;

  // Retrieve the type of the nth function (beside "module") in the device
  function TYModule.functionType(functionIndex:integer):string;
    var
      serial, funcId, baseType, funcName, funcVal, errmsg:string;
      res, i: integer;
      first, c : char;
    begin
      errmsg:='';
      res := _getFunction(functionIndex, serial, funcId, baseType, funcName, funcVal, errmsg);
      if(YISERR(res)) then
        begin
          _throw(res, errmsg);
          result:=YAPI_INVALID_STRING;
          exit;
        end;
      first := funcId[1];
      for i := Length(funcId) DownTo 2 do
        begin
          c := funcId[i];
          if (c > '9') Then
            break;
        end;
      result := UpCase(first) + Copy(funcId, 2, i - 1);
    end;

 // Retrieve the type of the nth function (beside "module") in the device
  function TYModule.functionBaseType(functionIndex:integer):string;
    var
      serial, funcId, funcName, baseType, funcVal, errmsg:string;
      res: integer;
    begin
      errmsg:='';
      res := _getFunction(functionIndex, serial, funcId, baseType, funcName, funcVal, errmsg);
      if(YISERR(res)) then
        begin
          _throw(res, errmsg);
          result:=YAPI_INVALID_STRING;
          exit;
        end;
      result := baseType;
    end;

  // Retrieve the logical name of the nth function (beside "module") in the device
  function TYModule.functionName(functionIndex:integer):string;
    var
      serial, funcId, baseType, funcName, funcVal, errmsg :  string;
      res:integer;
    begin
      errmsg:='';
      res := _getFunction(functionIndex, serial, funcId, baseType, funcName, funcVal, errmsg);
      if(YISERR(res)) then
        begin
          _throw(res, errmsg);
          result:=YAPI_INVALID_STRING;
          exit;
        end;
      result:=funcName;
    end;

  // Retrieve the advertised value of the nth function (beside "module") in the device
  function TYModule.functionValue(functionIndex:integer):string;
    var
      serial, funcId, baseType, funcName, funcVal, errmsg:string;
      res: integer;
    begin
      errmsg:='';
      res := _getFunction(functionIndex, serial, funcId, baseType, funcName, funcVal, errmsg);
      if(YISERR(res)) then
        begin
          _throw(res, errmsg);
          result:=YAPI_INVALID_STRING;
          exit;
        end;
      result:=funcVal;
    end;


var
  YDevice_devCache : Tlist;

  constructor  TYDevice.create(devdesc:YDEV_DESCR);
    begin
      _functions   := tlist.create();
      _devdescr    := devdesc;
      _cacheStamp  := 0;
      _cacheJson   := nil;
      _subpathinit := false;
    end;

  destructor TYDevice.Destroy();
    begin
      _functions.free();
      _functions:=nil;
      if assigned(_cacheJson) then _cacheJson.free();
      _cacheJson:=nil;
      inherited Destroy();
    end;

  class function TYDevice.getDevice(devdescr:YDEV_DESCR ):TYDevice;
    var
      idx : integer;
      dev : TYDevice;
    begin
      for idx:=0 to YDevice_devCache.count-1 do
        if  TYDevice(YDevice_devCache.items[idx])._devdescr = devdescr  then
          begin
            result :=  YDevice_devCache.items[idx];
            exit;
          end;
      // Not found, add new entry
      dev := TYDevice.create(devdescr);
      YDevice_devCache.add(dev);
      result:= dev;
    end;

  class procedure TYDevice.PlugDevice(devdescr:YDEV_DESCR );
    var
      idx : integer;
    begin
      for idx:=0 to YDevice_devCache.count-1 do
        if  TYDevice(YDevice_devCache.items[idx])._devdescr = devdescr  then
          begin
            TYDevice(YDevice_devCache.items[idx])._subpathinit := false;
            TYDevice(YDevice_devCache.items[idx])._cacheStamp := 0;
            exit;
          end;
    end;

  function  TYDevice.HTTPRequestPrepare(request:TByteArray; var fullrequest:TByteArray; var errmsg:string):YRETCODE;
    var
      res     : YRETCODE;
      errbuff : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perrbuf : pansichar;
      b       : pansichar;
      neededsize : integer;
      p          : integer;
      root   : array[0..YOCTO_SERIAL_LEN] of ansichar;
      proot  : pansichar;
      tmp : integer;
    begin
      _http_result   := '';
      _cacheStamp    := ygetTickCount(); //invalidate cache
      errbuff[0]     := #0;
      perrbuf        := addr(errbuff[0]);
      proot          := addr(root[0]);

      if not(_subpathinit) then
        begin
          res := _yapiGetDevicePath(_devdescr,proot, NIL, 0, neededsize, perrbuf);
          if YISERR(res) then
            begin
              errmsg := string(perrbuf);
              result := res;
              exit;
            end;
          getmem(b,neededsize);
          res := _yapiGetDevicePath(_devdescr,proot, b, neededsize, tmp, perrbuf);
          if(YISERR(res))  then
            begin
              freemem(b);
              errmsg := string(perrbuf);
              result := res;
              exit;
            end;
          _rootdevice:= string(proot);
          _subpath:=string(b);
          freemem(b);
          _subpathinit:=true;
        end;
      p := 0;
      while (p < length(request)) and (request[p] <> 47) do
        p := p + 1;
      setLength(fullrequest, length(request) -1 + length(_subpath));
      Move(request[0], fullrequest[0], p);
      Move(_StrToByte(_subpath)[0], fullrequest[p], length(_subpath));
      Move(request[p + 1],fullrequest[p + length(_subpath)], length(request)-p-1);
      result := YAPI_SUCCESS;
    end;

  function  TYDevice.HTTPRequestAsync(request :string; var errmsg:string):YRETCODE;

    begin
      result := self.HTTPRequestAsync(_StrToByte(request), errmsg);
    end;

  function  TYDevice.HTTPRequestAsync(request :TByteArray; var errmsg:string):YRETCODE;
    var
      res         : YRETCODE;
      fullrequest : TByteArray;
    begin
      res := HTTPRequestPrepare(request, fullrequest, errmsg);
      if(YISERR(res) ) then
        begin
          HTTPRequestAsync := res;
          exit;
        end;
      HTTPRequestAsync := yapiHTTPRequestAsync(_rootdevice,fullrequest,errmsg);
    end;

  function TYDevice.HTTPRequest(request :string ; var buffer :string; var errmsg:string):YRETCODE;
    var
      res         : YRETCODE;
      binreply    : TByteArray;
    begin
      setLength(binreply, 0);
      res := HTTPRequest(_StrToByte(request), binreply, errmsg);
      if YISERR(res) then
        begin
          result := res;
          exit;
        end;
      buffer := _ByteToString(binreply);
      result := res;
    end;

  function TYDevice.HTTPRequest(request :string ; var buffer :TByteArray; var errmsg:string):YRETCODE;
    begin
      result := self.HTTPRequest(_StrToByte(request), buffer,errmsg);
    end;

  function TYDevice.HTTPRequest(request :TByteArray ; var buffer :TByteArray; var errmsg:string):YRETCODE;
    var
      res         : YRETCODE;
      fullrequest : TByteArray;
    begin
      res := HTTPRequestPrepare(request, fullrequest, errmsg);
      if(YISERR(res) ) then
        begin
          result := res;
          exit;
        end;
      result := yapiHTTPRequestSync(_rootdevice,fullrequest,buffer,errmsg);
    end;



  function TYDevice.requestAPI(var  apires:TJsonParser;var errmsg:string) :YRETCODE;
    var
      j               : TJsonParser;
      buffer          : TByteArray;
      res             : integer;
      errBuffer       : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror          : pansichar;
    begin
      // Check if we have a valid cache value
      if(_cacheStamp > yGetTickCount()) then
        begin
          apires := _cacheJson;
          result :=  YAPI_SUCCESS;
          exit;
        end;
      res := HTTPRequest( 'GET /api.json '#13#10#13#10, buffer, errmsg);   // no HTTP/1.1 to get light headers
      if(YISERR(res)) then
        begin
          // make sure a device scan does not solve the issue
          errBuffer[0]:=#0;perror:=@errBuffer;
          res := _yapiUpdateDeviceList(1,perror);
          if (YISERR(res)) then
            begin
              errmsg:=string(perror);
              result:=res;
              exit;
            end;
          res := HTTPRequest( 'GET /api.json '#13#10#13#10, buffer, errmsg);   // no HTTP/1.1 to get light headers
          if(YISERR(res)) then
            begin
              result := res; exit;
            end;
        end;
      try
        j:= TJsonParser.create(_ByteToString(buffer));
      except
        on E: Exception do
          begin
            errmsg:='unexpected JSON structure: '+e.Message;
            result:=YAPI_IO_ERROR;
            exit;
          end;
      end;
      if j.httpcode <> 200 then
        begin
          errmsg := 'Unexpected HTTP return code:' + IntToStr(j.httpcode);
          result:=YAPI_IO_ERROR;
          exit;
        end;

      // store result in cache
      if assigned(_cacheJson) then _cacheJson.free();
      _cacheJson:= j;
      apires    := j;
      _cacheStamp := yGetTickCount() + _yapicontext.GetCacheValidity();
      result:= YAPI_SUCCESS;
    end;

    procedure TYDevice.clearCache();
      begin
        if assigned(_cacheJson) then _cacheJson.free();
        _cacheJson := nil;
        _cacheStamp := 0;
      end;

    function TYDevice.getFunctions(var  functions:tlist; var errmsg:string):YRETCODE;
      var
        res,neededsize,i,count:integer;
        p:PyHandleArray;
      begin
        if(_functions.count = 0) then
          begin
            res := yapiGetFunctionsByDevice(_devdescr, 0,nil, 64,neededsize, errmsg);
            if (YISERR(res)) then begin result:=res; exit;end;
            getmem(p,neededsize);
            res := yapiGetFunctionsByDevice(_devdescr, 0,p, 64,neededsize, errmsg);
            if (YISERR(res)) then begin   freemem(p);result:=res; exit;end;
            count :=  neededsize div sizeof(yHandle) ;
            for i:=0 to count-1  do _functions.add(pointer(p^[i]));
              freemem(p);
          end;
        functions := _functions;
        result    := YAPI_SUCCESS;
      end;


    procedure devicesCleanUp();
      var i:integer;
      begin
        for i:=0 to YDevice_devCache.count-1 do
          begin
            Tydevice(YDevice_devCache[i]).free();
            YDevice_devCache[i]:=nil;
          end;
        YDevice_devCache.free();
        YDevice_devCache:=nil;
      end;

  constructor TYModule.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Module';
      //--- (generated code: YModule accessors initialization)
      _productName := Y_PRODUCTNAME_INVALID;
      _serialNumber := Y_SERIALNUMBER_INVALID;
      _productId := Y_PRODUCTID_INVALID;
      _productRelease := Y_PRODUCTRELEASE_INVALID;
      _firmwareRelease := Y_FIRMWARERELEASE_INVALID;
      _persistentSettings := Y_PERSISTENTSETTINGS_INVALID;
      _luminosity := Y_LUMINOSITY_INVALID;
      _beacon := Y_BEACON_INVALID;
      _upTime := Y_UPTIME_INVALID;
      _usbCurrent := Y_USBCURRENT_INVALID;
      _rebootCountdown := Y_REBOOTCOUNTDOWN_INVALID;
      _userVar := Y_USERVAR_INVALID;
      _valueCallbackModule := nil;
      _logCallback := nil;
      _confChangeCallback := nil;
      //--- (end of generated code: YModule accessors initialization)
    end;


  class procedure TYModule._updateModuleCallbackList(modul: TYModule; add:boolean);
    var
      key : string;
      index : integer;
    begin
      key := modul.get_hardwareId();
      index := _moduleCallbackList.indexof(key);
      if add then
        begin
          if index < 0 then
            _moduleCallbackList.AddObject(key, TObject(1))
          else
            _moduleCallbackList.Objects[index] :=  TObject(Integer(_moduleCallbackList.Objects[index]) + 1);
        end
      else
        begin
          if (index >= 0) and (Integer(_moduleCallbackList.Objects[index]) > 1) then
             _moduleCallbackList.Objects[index] := TObject(Integer(_moduleCallbackList.Objects[index]) - 1);
        end;
    end;

//--- (generated code: YModule implementation)
{$HINTS OFF}
  function TYModule._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'productName') then
        begin
          _productName := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'serialNumber') then
        begin
          _serialNumber := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'productId') then
        begin
          _productId := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'productRelease') then
        begin
          _productRelease := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'firmwareRelease') then
        begin
          _firmwareRelease := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'persistentSettings') then
        begin
          _persistentSettings := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'luminosity') then
        begin
          _luminosity := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'beacon') then
        begin
          _beacon := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'upTime') then
        begin
          _upTime := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'usbCurrent') then
        begin
          _usbCurrent := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'rebootCountdown') then
        begin
          _rebootCountdown := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'userVar') then
        begin
          _userVar := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYModule.get_productName():string;
    var
      res : string;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PRODUCTNAME_INVALID;
              exit;
            end;
        end;
      res := self._productName;
      result := res;
      exit;
    end;


  function TYModule.get_serialNumber():string;
    var
      res : string;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SERIALNUMBER_INVALID;
              exit;
            end;
        end;
      res := self._serialNumber;
      result := res;
      exit;
    end;


  function TYModule.get_productId():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PRODUCTID_INVALID;
              exit;
            end;
        end;
      res := self._productId;
      result := res;
      exit;
    end;


  function TYModule.get_productRelease():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PRODUCTRELEASE_INVALID;
              exit;
            end;
        end;
      res := self._productRelease;
      result := res;
      exit;
    end;


  function TYModule.get_firmwareRelease():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_FIRMWARERELEASE_INVALID;
              exit;
            end;
        end;
      res := self._firmwareRelease;
      result := res;
      exit;
    end;


  function TYModule.get_persistentSettings():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_PERSISTENTSETTINGS_INVALID;
              exit;
            end;
        end;
      res := self._persistentSettings;
      result := res;
      exit;
    end;


  function TYModule.set_persistentSettings(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('persistentSettings',rest_val);
    end;

  function TYModule.get_luminosity():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LUMINOSITY_INVALID;
              exit;
            end;
        end;
      res := self._luminosity;
      result := res;
      exit;
    end;


  function TYModule.set_luminosity(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('luminosity',rest_val);
    end;

  function TYModule.get_beacon():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BEACON_INVALID;
              exit;
            end;
        end;
      res := self._beacon;
      result := res;
      exit;
    end;


  function TYModule.set_beacon(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('beacon',rest_val);
    end;

  function TYModule.get_upTime():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_UPTIME_INVALID;
              exit;
            end;
        end;
      res := self._upTime;
      result := res;
      exit;
    end;


  function TYModule.get_usbCurrent():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_USBCURRENT_INVALID;
              exit;
            end;
        end;
      res := self._usbCurrent;
      result := res;
      exit;
    end;


  function TYModule.get_rebootCountdown():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_REBOOTCOUNTDOWN_INVALID;
              exit;
            end;
        end;
      res := self._rebootCountdown;
      result := res;
      exit;
    end;


  function TYModule.set_rebootCountdown(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('rebootCountdown',rest_val);
    end;

  function TYModule.get_userVar():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_USERVAR_INVALID;
              exit;
            end;
        end;
      res := self._userVar;
      result := res;
      exit;
    end;


  function TYModule.set_userVar(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('userVar',rest_val);
    end;

  class function TYModule.FindModule(func: string):TYModule;
    var
      obj : TYModule;
      cleanHwId : string;
      modpos : LongInt;
    begin
      cleanHwId := func;
      modpos := (pos('.module', func) - 1);
      if modpos <> (Length(func) - 7) then
        begin
          cleanHwId := func + '.module';
        end;
      obj := TYModule(TYFunction._FindFromCache('Module', cleanHwId));
      if obj = nil then
        begin
          obj :=  TYModule.create(cleanHwId);
          TYFunction._AddToCache('Module',  cleanHwId, obj);
        end;
      result := obj;
      exit;
    end;


  function TYModule.registerValueCallback(callback: TYModuleValueCallback):LongInt;
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
      self._valueCallbackModule := callback;
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


  function TYModule._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackModule) <> nil) then
        begin
          self._valueCallbackModule(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYModule.get_productNameAndRevision():string;
    var
      prodname : string;
      prodrel : LongInt;
      fullname : string;
    begin
      prodname := self.get_productName;
      prodrel := self.get_productRelease;
      if prodrel > 1 then
        begin
          fullname := ''+ prodname+' rev. '+chr(64+prodrel);
        end
      else
        begin
          fullname := prodname;
        end;
      result := fullname;
      exit;
    end;


  function TYModule.saveToFlash():LongInt;
    begin
      result := self.set_persistentSettings(Y_PERSISTENTSETTINGS_SAVED);
      exit;
    end;


  function TYModule.revertFromFlash():LongInt;
    begin
      result := self.set_persistentSettings(Y_PERSISTENTSETTINGS_LOADED);
      exit;
    end;


  function TYModule.reboot(secBeforeReboot: LongInt):LongInt;
    begin
      result := self.set_rebootCountdown(secBeforeReboot);
      exit;
    end;


  function TYModule.triggerFirmwareUpdate(secBeforeReboot: LongInt):LongInt;
    begin
      result := self.set_rebootCountdown(-secBeforeReboot);
      exit;
    end;


  procedure TYModule._startStopDevLog(serial: string; start: boolean);
    var
      i_start : LongInt;
    begin
      if start then
        begin
          i_start := 1;
        end
      else
        begin
          i_start := 0;
        end;

      _yapiStartStopDeviceLogCallback(pansichar(ansistring(serial)), i_start);
    end;


  function TYModule.registerLogCallback(callback: TYModuleLogCallback):LongInt;
    var
      serial : string;
    begin
      serial := self.get_serialNumber;
      if (serial = YAPI_INVALID_STRING) then
        begin
          result := YAPI_DEVICE_NOT_FOUND;
          exit;
        end;
      self._logCallback := callback;
      self._startStopDevLog(serial, (addr(callback) <> nil));
      result := 0;
      exit;
    end;


  function TYModule.get_logCallback():TYModuleLogCallback;
    begin
      result := self._logCallback;
      exit;
    end;


  function TYModule.registerConfigChangeCallback(callback: TYModuleConfigChangeCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          TYModule._updateModuleCallbackList(self, true);
        end
      else
        begin
          TYModule._updateModuleCallbackList(self, false);
        end;
      self._confChangeCallback := callback;
      result := 0;
      exit;
    end;


  function TYModule._invokeConfigChangeCallback():LongInt;
    begin
      if (addr(self._confChangeCallback) <> nil) then
        begin
          self._confChangeCallback(self);
        end;
      result := 0;
      exit;
    end;


  function TYModule.registerBeaconCallback(callback: TYModuleBeaconCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          TYModule._updateModuleCallbackList(self, true);
        end
      else
        begin
          TYModule._updateModuleCallbackList(self, false);
        end;
      self._beaconCallback := callback;
      result := 0;
      exit;
    end;


  function TYModule._invokeBeaconCallback(beaconState: LongInt):LongInt;
    begin
      if (addr(self._beaconCallback) <> nil) then
        begin
          self._beaconCallback(self, beaconState);
        end;
      result := 0;
      exit;
    end;


  function TYModule.triggerConfigChangeCallback():LongInt;
    begin
      self._setAttr('persistentSettings', '2');
      result := 0;
      exit;
    end;


  function TYModule.checkFirmware(path: string; onlynew: boolean):string;
    var
      serial : string;
      release : LongInt;
      tmp_res : string;
    begin
      if onlynew then
        begin
          release := _atoi(self.get_firmwareRelease);
        end
      else
        begin
          release := 0;
        end;
      //may throw an exception
      serial := self.get_serialNumber;
      tmp_res := TYFirmwareUpdate.CheckFirmware(serial,  path, release);
      if (pos('error:', tmp_res) - 1) = 0 then
        begin
          self._throw(YAPI_INVALID_ARGUMENT, tmp_res);
        end;
      result := tmp_res;
      exit;
    end;


  function TYModule.updateFirmwareEx(path: string; force: boolean):TYFirmwareUpdate;
    var
      serial : string;
      settings : TByteArray;
    begin
      serial := self.get_serialNumber;
      settings := self.get_allSettings;
      if length(settings) = 0 then
        begin
          self._throw(YAPI_IO_ERROR, 'Unable to get device settings');
          settings := _StrToByte('error:Unable to get device settings');
        end;
      result := TYFirmwareUpdate.create(serial, path, settings, force);
      exit;
    end;


  function TYModule.updateFirmware(path: string):TYFirmwareUpdate;
    begin
      result := self.updateFirmwareEx(path, false);
      exit;
    end;


  function TYModule.get_allSettings():TByteArray;
    var
      settings : TByteArray;
      json : TByteArray;
      res : TByteArray;
      sep : string;
      name : string;
      item : string;
      t_type : string;
      id : string;
      url : string;
      file_data : string;
      file_data_bin : TByteArray;
      temp_data_bin : TByteArray;
      ext_settings : string;
      filelist : TStringArray;
      templist : TStringArray;
      i_i : LongInt;
    begin
      SetLength(filelist, 0);
      SetLength(templist, 0);

      settings := self._download('api.json');
      if length(settings) = 0 then
        begin
          result := settings;
          exit;
        end;
      ext_settings := ', "extras":[';
      templist := self.get_functionIds('Temperature');
      sep := '';
      for i_i:=0 to length( templist)-1 do
        begin
          if _atoi(self.get_firmwareRelease) > 9000 then
            begin
              url := 'api/'+ templist[i_i]+'/sensorType';
              t_type := _ByteToString(self._download(url));
              if (t_type = 'RES_NTC') or (t_type = 'RES_LINEAR') then
                begin
                  id := Copy( templist[i_i],  11 + 1, Length( templist[i_i]) - 11);
                  if (id = '') then
                    begin
                      id := '1';
                    end;
                  temp_data_bin := self._download('extra.json?page='+id);
                  if length(temp_data_bin) > 0 then
                    begin
                      item := ''+ sep+'{"fid":"'+  templist[i_i]+'", "json":'+_ByteToString(temp_data_bin)+'}'#10'';
                      ext_settings := ext_settings + item;
                      sep := ',';
                    end;
                end;
            end;
        end;
      ext_settings := ext_settings + '],'#10'"files":[';
      if self.hasFunction('files') then
        begin
          json := self._download('files.json?a=dir&f=');
          if length(json) = 0 then
            begin
              result := json;
              exit;
            end;
          filelist := self._json_get_array(json);
          sep := '';
          for i_i:=0 to length( filelist)-1 do
            begin
              name := self._json_get_key(_StrToByte( filelist[i_i]), 'name');
              if (Length(name) > 0) and not((name = 'startupConf.json')) then
                begin
                  file_data_bin := self._download(self._escapeAttr(name));
                  file_data := _bytesToHexStr(file_data_bin, 0, length(file_data_bin));
                  item := ''+ sep+'{"name":"'+ name+'", "data":"'+file_data+'"}'#10'';
                  ext_settings := ext_settings + item;
                  sep := ',';
                end;
            end;
        end;
      res := _StrToByte('{ "api":' + _ByteToString(settings) + ext_settings + ']}');
      result := res;
      exit;
    end;


  function TYModule.loadThermistorExtra(funcId: string; jsonExtra: string):LongInt;
    var
      values : TStringArray;
      url : string;
      curr : string;
      currTemp : string;
      ofs : LongInt;
      size : LongInt;
    begin
      SetLength(values, 0);
      url := 'api/' + funcId + '.json?command=Z';

      self._download(url);
      // add records in growing resistance value
      values := self._json_get_array(_StrToByte(jsonExtra));
      ofs := 0;
      size := length(values);
      while ofs + 1 < size do
        begin
          curr := values[ofs];
          currTemp := values[ofs + 1];
          url := 'api/'+ funcId+'.json?command=m'+ curr+':'+currTemp;
          self._download(url);
          ofs := ofs + 2;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYModule.set_extraSettings(jsonExtra: string):LongInt;
    var
      extras : TStringArray;
      functionId : string;
      data : string;
      i_i : LongInt;
    begin
      SetLength(extras, 0);
      extras := self._json_get_array(_StrToByte(jsonExtra));
      for i_i:=0 to length( extras)-1 do
        begin
          functionId := self._get_json_path( extras[i_i], 'fid');
          functionId := self._decode_json_string(functionId);
          data := self._get_json_path( extras[i_i], 'json');
          if self.hasFunction(functionId) then
            begin
              self.loadThermistorExtra(functionId, data);
            end;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYModule.set_allSettingsAndFiles(settings: TByteArray):LongInt;
    var
      down : TByteArray;
      json : string;
      json_api : string;
      json_files : string;
      json_extra : string;
      fuperror : LongInt;
      globalres : LongInt;
        files : TStringArray;
        res : string;
        name : string;
        data : string;
      i_i : LongInt;
    begin
      SetLength(files, 0);
      fuperror := 0;
      json := _ByteToString(settings);
      json_api := self._get_json_path(json, 'api');
      if (json_api = '') then
        begin
          result := self.set_allSettings(settings);
          exit;
        end;
      json_extra := self._get_json_path(json, 'extras');
      if not((json_extra = '')) then
        begin
          self.set_extraSettings(json_extra);
        end;
      self.set_allSettings(_StrToByte(json_api));
      if self.hasFunction('files') then
        begin
          down := self._download('files.json?a=format');
          res := self._get_json_path(_ByteToString(down), 'res');
          res := self._decode_json_string(res);
          if not((res = 'ok')) then
            begin
              self._throw( YAPI_IO_ERROR, 'format failed');
              result:=YAPI_IO_ERROR;
              exit;
            end;
          json_files := self._get_json_path(json, 'files');
          files := self._json_get_array(_StrToByte(json_files));
          for i_i:=0 to length( files)-1 do
            begin
              name := self._get_json_path( files[i_i], 'name');
              name := self._decode_json_string(name);
              data := self._get_json_path( files[i_i], 'data');
              data := self._decode_json_string(data);
              if (name = '') then
                begin
                  fuperror := fuperror + 1;
                end
              else
                begin
                  self._upload(name, _hexStrToBin(data));
                end;
            end;
        end;
      // Apply settings a second time for file-dependent settings and dynamic sensor nodes
      globalres := self.set_allSettings(_StrToByte(json_api));
      if not(fuperror = 0) then
        begin
          self._throw( YAPI_IO_ERROR, 'Error during file upload');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      result := globalres;
      exit;
    end;


  function TYModule.hasFunction(funcId: string):boolean;
    var
      count : LongInt;
      i : LongInt;
      fid : string;
    begin
      count := self.functionCount;
      i := 0;
      while i < count do
        begin
          fid := self.functionId(i);
          if (fid = funcId) then
            begin
              result := true;
              exit;
            end;
          i := i + 1;
        end;
      result := false;
      exit;
    end;


  function TYModule.get_functionIds(funType: string):TStringArray;
    var
      count : LongInt;
      i : LongInt;
      ftype : string;
      res : TStringArray;
      res_pos : LongInt;
    begin
      SetLength(res, 0);

      count := self.functionCount;
      i := 0;
      res_pos := length(res);
      SetLength(res, res_pos+count);;
      while i < count do
        begin
          ftype := self.functionType(i);
          if (ftype = funType) then
            begin
              res[res_pos] := self.functionId(i);
              inc(res_pos);
            end
          else
            begin
              ftype := self.functionBaseType(i);
              if (ftype = funType) then
                begin
                  res[res_pos] := self.functionId(i);
                  inc(res_pos);
                end;
            end;
          i := i + 1;
        end;
      SetLength(res, res_pos);;
      result := res;
      exit;
    end;


  function TYModule._flattenJsonStruct(jsoncomplex: TByteArray):TByteArray;
    var
      errmsg_buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      errmsg : pansichar;
      smallbuff_buffer : array[0..1024] of ansichar;
      smallbuff : pansichar;
      bigbuff : pansichar;
      buffsize : LongInt;
      fullsize : LongInt;
      res : LongInt;
      jsonflat : string;
      jsoncomplexstr : string;
    begin
      errmsg_buffer[0]:=#0;errmsg:=@errmsg_buffer;
      smallbuff_buffer[0]:=#0;smallbuff:=@smallbuff_buffer;
              fullsize := 0;
              jsoncomplexstr := _ByteToString(jsoncomplex);
              res := _yapiGetAllJsonKeys(pansichar(ansistring(jsoncomplexstr)), smallbuff, 1024, fullsize, errmsg);
      if res < 0 then
        begin
          self._throw(YAPI_INVALID_ARGUMENT, string(errmsg));
          jsonflat := 'error:' + string(errmsg);
          result := _StrToByte(jsonflat);
          exit;
        end;
      if fullsize <= 1024 then
        begin
          jsonflat := string(smallbuff);
        end
      else
        begin
          fullsize := fullsize * 2;
          buffsize := fullsize;
          getmem(bigbuff, buffsize);
          res := _yapiGetAllJsonKeys(pansichar(ansistring(jsoncomplexstr)), bigbuff, buffsize, fullsize, errmsg);
          if res < 0 then
            begin
              self._throw(YAPI_INVALID_ARGUMENT, string(errmsg));
              jsonflat := 'error:' + string(errmsg);
            end
          else
            begin
              jsonflat := string(bigbuff);
            end;
          freemem(bigbuff);
        end;
      result := _StrToByte(jsonflat);
      exit;
    end;


  function TYModule.calibVersion(cparams: string):LongInt;
    begin
      if (cparams = '0,') then
        begin
          result := 3;
          exit;
        end;
      if (pos(',', cparams) - 1) >= 0 then
        begin
          if (pos(' ', cparams) - 1) > 0 then
            begin
              result := 3;
              exit;
            end
          else
            begin
              result := 1;
              exit;
            end;
        end;
      if (cparams = '') or (cparams = '0') then
        begin
          result := 1;
          exit;
        end;
      if (Length(cparams) < 2) or((pos('.', cparams) - 1) >= 0) then
        begin
          result := 0;
          exit;
        end
      else
        begin
          result := 2;
          exit;
        end;
    end;


  function TYModule.calibScale(unit_name: string; sensorType: string):LongInt;
    begin
      if (unit_name = 'g') or (unit_name = 'gauss') or (unit_name = 'W') then
        begin
          result := 1000;
          exit;
        end;
      if (unit_name = 'C') then
        begin
          if (sensorType = '') then
            begin
              result := 16;
              exit;
            end;
          if _atoi(sensorType) < 8 then
            begin
              result := 16;
              exit;
            end
          else
            begin
              result := 100;
              exit;
            end;
        end;
      if (unit_name = 'm') or (unit_name = 'deg') then
        begin
          result := 10;
          exit;
        end;
      result := 1;
      exit;
    end;


  function TYModule.calibOffset(unit_name: string):LongInt;
    begin
      if (unit_name = '% RH') or (unit_name = 'mbar') or (unit_name = 'lx') then
        begin
          result := 0;
          exit;
        end;
      result := 32767;
      exit;
    end;


  function TYModule.calibConvert(param: string; currentFuncValue: string; unit_name: string; sensorType: string):string;
    var
      paramVer : LongInt;
      funVer : LongInt;
      funScale : LongInt;
      funOffset : LongInt;
      paramScale : LongInt;
      paramOffset : LongInt;
      words : TLongIntArray;
      words_str : TStringArray;
      calibData : TDoubleArray;
      iCalib : TLongIntArray;
      calibType : LongInt;
      i : LongInt;
      maxSize : LongInt;
      ratio : double;
      nPoints : LongInt;
      wordVal : double;
      calibData_pos : LongInt;
      words_pos : LongInt;
      i_i : LongInt;
    begin
      SetLength(words_str, 0);
      // Initial guess for parameter encoding
      paramVer := self.calibVersion(param);
      funVer := self.calibVersion(currentFuncValue);
      funScale := self.calibScale(unit_name, sensorType);
      funOffset := self.calibOffset(unit_name);
      paramScale := funScale;
      paramOffset := funOffset;
      if funVer < 3 then
        begin
          // Read the effective device scale if available
          if funVer = 2 then
            begin
              words := _decodeWords(currentFuncValue);
              if (words[0] = 1366) and(words[1] = 12500) then
                begin
                  // Yocto-3D RefFrame used a special encoding
                  funScale := 1;
                  funOffset := 0;
                end
              else
                begin
                  funScale := words[1];
                  funOffset := words[0];
                end;
            end
          else
            begin
              if funVer = 1 then
                begin
                  if (currentFuncValue = '') or(_atoi(currentFuncValue) > 10) then
                    begin
                      funScale := 0;
                    end;
                end;
            end;
        end;
      SetLength(calibData, 0);
      calibType := 0;
      if paramVer < 3 then
        begin
          // Handle old 16 bit parameters formats
          if paramVer = 2 then
            begin
              words := _decodeWords(param);
              if (words[0] = 1366) and(words[1] = 12500) then
                begin
                  // Yocto-3D RefFrame used a special encoding
                  paramScale := 1;
                  paramOffset := 0;
                end
              else
                begin
                  paramScale := words[1];
                  paramOffset := words[0];
                end;
              if (length(words) >= 3) and(words[2] > 0) then
                begin
                  maxSize := 3 + 2 * ((words[2]) Mod (10));
                  if maxSize > length(words) then
                    begin
                      maxSize := length(words);
                    end;
                  calibData_pos := length(calibData);
                  SetLength(calibData, calibData_pos+maxSize);
                  i := 3;
                  while i < maxSize do
                    begin
                      calibData[calibData_pos] := words[i];
                      inc(calibData_pos);
                      i := i + 1;
                    end;
                  SetLength(calibData, calibData_pos);
                end;
            end
          else
            begin
              if paramVer = 1 then
                begin
                  words_str := _stringSplit(param, ',');
                  words_pos := length(words);
                  SetLength(words, words_pos+length(words_str));
                  for i_i:=0 to length(words_str)-1 do
                    begin
                      words[words_pos] := _atoi(words_str[i_i]);
                      inc(words_pos);
                    end;
                  SetLength(words, words_pos);
                  if (param = '') or(words[0] > 10) then
                    begin
                      paramScale := 0;
                    end;
                  if (length(words) > 0) and(words[0] > 0) then
                    begin
                      maxSize := 1 + 2 * ((words[0]) Mod (10));
                      if maxSize > length(words) then
                        begin
                          maxSize := length(words);
                        end;
                      i := 1;
                      calibData_pos := length(calibData);
                      SetLength(calibData, calibData_pos+maxSize);
                      while i < maxSize do
                        begin
                          calibData[calibData_pos] := words[i];
                          inc(calibData_pos);
                          i := i + 1;
                        end;
                      SetLength(calibData, calibData_pos);
                    end;
                end
              else
                begin
                  if paramVer = 0 then
                    begin
                      ratio := StrToFloat(param);
                      calibData_pos := length(calibData);
                      SetLength(calibData, calibData_pos+4);
                      if ratio > 0 then
                        begin
                          calibData[calibData_pos] := 0.0;
                          inc(calibData_pos);
                          calibData[calibData_pos] := 0.0;
                          inc(calibData_pos);
                          calibData[calibData_pos] := round(65535 / ratio);
                          inc(calibData_pos);
                          calibData[calibData_pos] := 65535.0;
                          inc(calibData_pos);
                        end;
                      SetLength(calibData, calibData_pos);
                    end;
                end;
            end;
          i := 0;
          while i < length(calibData) do
            begin
              if paramScale > 0 then
                begin
                  // scalar decoding
                  calibData[ i] := (calibData[i] - paramOffset) / paramScale;
                end
              else
                begin
                  // floating-point decoding
                  calibData[ i] := _decimalToDouble(round(calibData[i]));
                end;
              i := i + 1;
            end;
        end
      else
        begin
          // Handle latest 32bit parameter format
          iCalib := _decodeFloats(param);
          calibType := round(iCalib[0] / 1000.0);
          if calibType >= 30 then
            begin
              calibType := calibType - 30;
            end;
          i := 1;
          calibData_pos := length(calibData);
          SetLength(calibData, calibData_pos+length(iCalib)-1);
          while i < length(iCalib) do
            begin
              calibData[calibData_pos] := iCalib[i] / 1000.0;
              inc(calibData_pos);
              i := i + 1;
            end;
          SetLength(calibData, calibData_pos);
        end;
      if funVer >= 3 then
        begin
          // Encode parameters in new format
          if length(calibData) = 0 then
            begin
              param := '0,';
            end
          else
            begin
              param := IntToStr(30 + calibType);
              i := 0;
              while i < length(calibData) do
                begin
                  if ((i) and 1) > 0 then
                    begin
                      param := param + ':';
                    end
                  else
                    begin
                      param := param + ' ';
                    end;
                  param := param + IntToStr(round(calibData[i] * 1000.0 / 1000.0));
                  i := i + 1;
                end;
              param := param + ',';
            end;
        end
      else
        begin
          if funVer >= 1 then
            begin
              // Encode parameters for older devices
              nPoints := (length(calibData) div 2);
              param := IntToStr(nPoints);
              i := 0;
              while i < 2 * nPoints do
                begin
                  if funScale = 0 then
                    begin
                      wordVal := _doubleToDecimal(round(calibData[i]));
                    end
                  else
                    begin
                      wordVal := calibData[i] * funScale + funOffset;
                    end;
                  param := param + ',' + _yapiFloatToStr(round(wordVal));
                  i := i + 1;
                end;
            end
          else
            begin
              // Initial V0 encoding used for old Yocto-Light
              if length(calibData) = 4 then
                begin
                  param := _yapiFloatToStr(round(1000 * (calibData[3] - calibData[1]) / calibData[2] - calibData[0]));
                end;
            end;
        end;
      result := param;
      exit;
    end;


  function TYModule._tryExec(url: string):LongInt;
    var
      res : LongInt;
      done : LongInt;
      ignoreErrMsg : string;
    begin
      res := YAPI_SUCCESS;
      done := 1;
      Try
        self._download(url);
      Except
        done := 0;
      End;
      if done = 0 then
        begin
          // retry silently after a short wait
          Try
            ySleep(500, ignoreErrMsg);
            self._download(url);
          Except
            // second failure, return error code
            res := self.get_errorType;
          End;
        end;
      result := res;
      exit;
    end;


  function TYModule.set_allSettings(settings: TByteArray):LongInt;
    var
      restoreLast : TStringArray;
      old_json_flat : TByteArray;
      old_dslist : TStringArray;
      old_jpath : TStringArray;
      old_jpath_len : TLongIntArray;
      old_val_arr : TStringArray;
      actualSettings : TByteArray;
      new_dslist : TStringArray;
      new_jpath : TStringArray;
      new_jpath_len : TLongIntArray;
      new_val_arr : TStringArray;
      cpos : LongInt;
      eqpos : LongInt;
      leng : LongInt;
      i : LongInt;
      j : LongInt;
      subres : LongInt;
      res : LongInt;
      njpath : string;
      jpath : string;
      fun : string;
      attr : string;
      value : string;
      url : string;
      tmp : string;
      new_calib : string;
      sensorType : string;
      unit_name : string;
      newval : string;
      oldval : string;
      old_calib : string;
      each_str : string;
      do_update : boolean;
      found : boolean;
      jpath_pos : LongInt;
      len_pos : LongInt;
      arr_pos : LongInt;
      i_i : LongInt;
      ignoreErrMsg : string;
      restoreLast_pos : LongInt;
    begin
      SetLength(restoreLast, 0);
      SetLength(old_dslist, 0);
      SetLength(old_jpath, 0);
      SetLength(old_val_arr, 0);
      SetLength(new_dslist, 0);
      SetLength(new_jpath, 0);
      SetLength(new_val_arr, 0);
      res := YAPI_SUCCESS;
      tmp := _ByteToString(settings);
      tmp := self._get_json_path(tmp, 'api');
      if not((tmp = '')) then
        begin
          settings := _StrToByte(tmp);
        end;
      oldval := '';
      newval := '';
      old_json_flat := self._flattenJsonStruct(settings);
      old_dslist := self._json_get_array(old_json_flat);
      jpath_pos := length(old_jpath);
      SetLength(old_jpath, jpath_pos+length(old_dslist));;
      len_pos := length(old_jpath_len);
      SetLength(old_jpath_len, len_pos+length(old_dslist));;
      arr_pos := length(old_val_arr);
      SetLength(old_val_arr, arr_pos+length(old_dslist));;
      for i_i:=0 to length(old_dslist)-1 do
        begin
          each_str := self._json_get_string(_StrToByte(old_dslist[i_i]));
          // split json path and attr
          leng := Length(each_str);
          eqpos := (pos('=', each_str) - 1);
          if (eqpos < 0) or(leng = 0) then
            begin
              self._throw(YAPI_INVALID_ARGUMENT, 'Invalid settings');
              result := YAPI_INVALID_ARGUMENT;
              exit;
            end;
          jpath := Copy(each_str,  0 + 1, eqpos);
          eqpos := eqpos + 1;
          value := Copy(each_str,  eqpos + 1, leng - eqpos);
          old_jpath[jpath_pos] := jpath;
          inc(jpath_pos);
          old_jpath_len[len_pos] := Length(jpath);
          inc(len_pos);
          old_val_arr[arr_pos] := value;
          inc(arr_pos);
        end;
      SetLength(old_jpath, jpath_pos);;
      SetLength(old_jpath_len, len_pos);;
      SetLength(old_val_arr, arr_pos);;

      Try
        actualSettings := self._download('api.json');
      Except
        // retry silently after a short wait
        ySleep(500, ignoreErrMsg);
        actualSettings := self._download('api.json');
      End;
      actualSettings := self._flattenJsonStruct(actualSettings);
      new_dslist := self._json_get_array(actualSettings);
      jpath_pos := length(new_jpath);
      SetLength(new_jpath, jpath_pos+length(new_dslist));;
      len_pos := length(new_jpath_len);
      SetLength(new_jpath_len, len_pos+length(new_dslist));;
      arr_pos := length(new_val_arr);
      SetLength(new_val_arr, arr_pos+length(new_dslist));;
      for i_i:=0 to length(new_dslist)-1 do
        begin
          // remove quotes
          each_str := self._json_get_string(_StrToByte(new_dslist[i_i]));
          // split json path and attr
          leng := Length(each_str);
          eqpos := (pos('=', each_str) - 1);
          if (eqpos < 0) or(leng = 0) then
            begin
              self._throw(YAPI_INVALID_ARGUMENT, 'Invalid settings');
              result := YAPI_INVALID_ARGUMENT;
              exit;
            end;
          jpath := Copy(each_str,  0 + 1, eqpos);
          eqpos := eqpos + 1;
          value := Copy(each_str,  eqpos + 1, leng - eqpos);
          new_jpath[jpath_pos] := jpath;
          inc(jpath_pos);
          new_jpath_len[len_pos] := Length(jpath);
          inc(len_pos);
          new_val_arr[arr_pos] := value;
          inc(arr_pos);
        end;
      SetLength(new_jpath, jpath_pos);;
      SetLength(new_jpath_len, len_pos);;
      SetLength(new_val_arr, arr_pos);;
      restoreLast_pos := length(restoreLast);
      SetLength(restoreLast, restoreLast_pos+5);;
      i := 0;
      while i < length(new_jpath) do
        begin
          njpath := new_jpath[i];
          leng := Length(njpath);
          cpos := (pos('/', njpath) - 1);
          if (cpos < 0) or(leng = 0) then
            begin
              continue;
            end;
          fun := Copy(njpath,  0 + 1, cpos);
          cpos := cpos + 1;
          attr := Copy(njpath,  cpos + 1, leng - cpos);
          do_update := true;
          if (fun = 'services') then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'firmwareRelease')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'usbCurrent')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'upTime')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'persistentSettings')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'adminPassword')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'userPassword')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'rebootCountdown')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'advertisedValue')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'poeCurrent')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'readiness')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'ipAddress')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'subnetMask')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'router')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'linkQuality')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'ssid')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'channel')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'security')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'message')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'signalValue')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'currentValue')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'currentRawValue')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'currentRunIndex')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'pulseTimer')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'lastTimePressed')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'lastTimeReleased')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'filesCount')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'freeSpace')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'timeUTC')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'rtcTime')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'unixTime')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'dateTime')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'rawValue')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'lastMsg')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'delayedPulseTimer')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'rxCount')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'txCount')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'msgCount')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'rxMsgCount')) then
            begin
              do_update := false;
            end;
          if (do_update) and((attr = 'txMsgCount')) then
            begin
              do_update := false;
            end;
          if do_update then
            begin
              do_update := false;
              newval := new_val_arr[i];
              j := 0;
              found := false;
              while  (j < length(old_jpath)) and not(found) do
                begin
                  if (new_jpath_len[i] = old_jpath_len[j]) and((new_jpath[i] = old_jpath[j])) then
                    begin
                      found := true;
                      oldval := old_val_arr[j];
                      if not((newval = oldval)) then
                        begin
                          do_update := true;
                        end;
                    end;
                  j := j + 1;
                end;
            end;
          if do_update then
            begin
              if (attr = 'calibrationParam') then
                begin
                  old_calib := '';
                  unit_name := '';
                  sensorType := '';
                  new_calib := newval;
                  j := 0;
                  found := false;
                  while  (j < length(old_jpath)) and not(found) do
                    begin
                      if (new_jpath_len[i] = old_jpath_len[j]) and((new_jpath[i] = old_jpath[j])) then
                        begin
                          found := true;
                          old_calib := old_val_arr[j];
                        end;
                      j := j + 1;
                    end;
                  tmp := fun + '/unit';
                  j := 0;
                  found := false;
                  while  (j < length(new_jpath)) and not(found) do
                    begin
                      if (tmp = new_jpath[j]) then
                        begin
                          found := true;
                          unit_name := new_val_arr[j];
                        end;
                      j := j + 1;
                    end;
                  tmp := fun + '/sensorType';
                  j := 0;
                  found := false;
                  while  (j < length(new_jpath)) and not(found) do
                    begin
                      if (tmp = new_jpath[j]) then
                        begin
                          found := true;
                          sensorType := new_val_arr[j];
                        end;
                      j := j + 1;
                    end;
                  newval := self.calibConvert(old_calib,  new_val_arr[i],  unit_name, sensorType);
                  url := 'api/' + fun + '.json?' + attr + '=' + self._escapeAttr(newval);
                  subres := self._tryExec(url);
                  if (res = YAPI_SUCCESS) and(subres <> YAPI_SUCCESS) then
                    begin
                      res := subres;
                    end;
                end
              else
                begin
                  url := 'api/' + fun + '.json?' + attr + '=' + self._escapeAttr(oldval);
                  if (attr = 'resolution') then
                    begin
                      restoreLast[restoreLast_pos] := url;
                      inc(restoreLast_pos);
                    end
                  else
                    begin
                      subres := self._tryExec(url);
                      if (res = YAPI_SUCCESS) and(subres <> YAPI_SUCCESS) then
                        begin
                          res := subres;
                        end;
                    end;
                end;
            end;
          i := i + 1;
        end;
      SetLength(restoreLast, restoreLast_pos);;
      for i_i:=0 to length(restoreLast)-1 do
        begin
          subres := self._tryExec(restoreLast[i_i]);
          if (res = YAPI_SUCCESS) and(subres <> YAPI_SUCCESS) then
            begin
              res := subres;
            end;
        end;
      self.clearCache;
      result := res;
      exit;
    end;


  function TYModule.addFileToHTTPCallback(filename: string):LongInt;
    var
      content : TByteArray;
    begin
      content := self._download('@YCB+' + filename);
      if length(content) = 0 then
        begin
          result := YAPI_NOT_SUPPORTED;
          exit;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYModule.get_hardwareId():string;
    var
      serial : string;
    begin
      serial := self.get_serialNumber;
      result := serial + '.module';
      exit;
    end;


  function TYModule.download(pathname: string):TByteArray;
    begin
      result := self._download(pathname);
      exit;
    end;


  function TYModule.get_icon2d():TByteArray;
    begin
      result := self._download('icon2d.png');
      exit;
    end;


  function TYModule.get_lastLogs():string;
    var
      content : TByteArray;
    begin
      content := self._download('logs.txt');
      result := _ByteToString(content);
      exit;
    end;


  function TYModule.log(text: string):LongInt;
    begin
      result := self._upload('logs.txt', _StrToByte(text));
      exit;
    end;


  function TYModule.get_subDevices():TStringArray;
    var
      errmsg_buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      errmsg : pansichar;
      smallbuff_buffer : array[0..1024] of ansichar;
      smallbuff : pansichar;
      bigbuff : pansichar;
      buffsize : LongInt;
      fullsize : LongInt;
      yapi_res : LongInt;
      subdevice_list : string;
      subdevices : TStringArray;
      serial : string;
    begin
      errmsg_buffer[0]:=#0;errmsg:=@errmsg_buffer;
      smallbuff_buffer[0]:=#0;smallbuff:=@smallbuff_buffer;
      SetLength(subdevices, 0);

      serial := self.get_serialNumber;
      fullsize := 0;
      yapi_res := _yapiGetSubdevices(pansichar(ansistring(serial)), smallbuff, 1024, fullsize, errmsg);
      if yapi_res < 0 then
        begin
          result := subdevices;
          exit;
        end;
      if fullsize <= 1024 then
        begin
          subdevice_list := string(smallbuff);
        end
      else
        begin
          buffsize := fullsize;
          getmem(bigbuff, buffsize);
          yapi_res := _yapiGetSubdevices(pansichar(ansistring(serial)), bigbuff, buffsize, fullsize, errmsg);
          if yapi_res < 0 then
            begin
              freemem(bigbuff);
              result := subdevices;
              exit;
            end
          else
            begin
              subdevice_list := string(bigbuff);
            end;
          freemem(bigbuff);
        end;
      if not((subdevice_list = '')) then
        begin
          subdevices := _stringSplit(subdevice_list, ',');
        end;
      result := subdevices;
      exit;
    end;


  function TYModule.get_parentHub():string;
    var
      errmsg_buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      errmsg : pansichar;
      hubserial_buffer : array[0..YOCTO_SERIAL_LEN] of ansichar;
      hubserial : pansichar;
      pathsize : LongInt;
      yapi_res : LongInt;
      serial : string;
    begin
      errmsg_buffer[0]:=#0;errmsg:=@errmsg_buffer;
      hubserial_buffer[0]:=#0;hubserial:=@hubserial_buffer;

      serial := self.get_serialNumber;
      // retrieve device object
      pathsize := 0;
      yapi_res := _yapiGetDevicePathEx(pansichar(ansistring(serial)), hubserial, nil, 0, pathsize, errmsg);
      if yapi_res < 0 then
        begin
          result := '';
          exit;
        end;
      result := string(hubserial);
      exit;
    end;


  function TYModule.get_url():string;
    var
      errmsg_buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      errmsg : pansichar;
      path_buffer : array[0..1024] of ansichar;
      path : pansichar;
      pathsize : LongInt;
      yapi_res : LongInt;
      serial : string;
    begin
      errmsg_buffer[0]:=#0;errmsg:=@errmsg_buffer;
      path_buffer[0]:=#0;path:=@path_buffer;

      serial := self.get_serialNumber;
      // retrieve device object
      pathsize := 0;
      yapi_res := _yapiGetDevicePathEx(pansichar(ansistring(serial)), nil, path, 1024, pathsize, errmsg);
      if yapi_res < 0 then
        begin
          result := '';
          exit;
        end;
      result := string(path);
      exit;
    end;


  function TYModule.nextModule(): TYModule;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextModule := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextModule := nil;
          exit;
        end;
      nextModule := TYModule.FindModule(hwid);
    end;

  class function TYModule.FirstModule(): TYModule;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Module', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYModule.FindModule(serial+'.'+funcId);
    end;

//--- (end of generated code: YModule implementation)

 // Return a unique hardware identifier for the device
  function TYModule.get_friendlyName():string;
    var
      retcode: YRETCODE;
      fundesc : YFUN_DESCR;
      devdesc:YDEV_DESCR ;
      funcName,funcVal:string ;
      errmsg : string;
      snum:string;
      funcid:string;
      errbuff:string;
    begin
     errmsg:='';
      // Resolve the function name
      retcode := _getDescriptor(fundesc, errmsg);
      if(YISERR(retcode)) then
        begin
          _throw(retcode, errmsg);
          get_friendlyName:=  Y_FRIENDLYNAME_INVALID;
          exit;
        end;
      retcode:=yapiGetFunctionInfo(fundesc, devdesc, snum, funcid, funcName,funcVal,errbuff) ;
      if(YISERR(retcode)) then
        begin
          errmsg := errbuff;
          _throw(retcode, errmsg);
          get_friendlyName := Y_FRIENDLYNAME_INVALID;
          exit;
        end;
      if (funcName <> '') then
        get_friendlyName := funcName
      else
        get_friendlyName := snum;
    end;


  constructor TYSensor.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Sensor';
//--- (generated code: YSensor accessors initialization)
      _unit := Y_UNIT_INVALID;
      _currentValue := Y_CURRENTVALUE_INVALID;
      _lowestValue := Y_LOWESTVALUE_INVALID;
      _highestValue := Y_HIGHESTVALUE_INVALID;
      _currentRawValue := Y_CURRENTRAWVALUE_INVALID;
      _logFrequency := Y_LOGFREQUENCY_INVALID;
      _reportFrequency := Y_REPORTFREQUENCY_INVALID;
      _advMode := Y_ADVMODE_INVALID;
      _calibrationParam := Y_CALIBRATIONPARAM_INVALID;
      _resolution := Y_RESOLUTION_INVALID;
      _sensorState := Y_SENSORSTATE_INVALID;
      _valueCallbackSensor := nil;
      _timedReportCallbackSensor := nil;
      _prevTimedReport := 0;
      _iresol := 0;
      _offset := 0;
      _scale := 0;
      _decexp := 0;
      _caltyp := 0;
      //--- (end of generated code: YSensor accessors initialization)
    end;


//--- (generated code: YSensor implementation)
{$HINTS OFF}
  function TYSensor._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'unit') then
        begin
          _unit := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'currentValue') then
        begin
          _currentValue := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'lowestValue') then
        begin
          _lowestValue := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'highestValue') then
        begin
          _highestValue := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'currentRawValue') then
        begin
          _currentRawValue := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'logFrequency') then
        begin
          _logFrequency := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'reportFrequency') then
        begin
          _reportFrequency := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'advMode') then
        begin
          _advMode := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'calibrationParam') then
        begin
          _calibrationParam := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'resolution') then
        begin
          _resolution := round(member^.ivalue / 65.536) / 1000.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'sensorState') then
        begin
          _sensorState := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYSensor.get_unit():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_UNIT_INVALID;
              exit;
            end;
        end;
      res := self._unit;
      result := res;
      exit;
    end;


  function TYSensor.get_currentValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTVALUE_INVALID;
              exit;
            end;
        end;
      res := self._applyCalibration(self._currentRawValue);
      if res = Y_CURRENTVALUE_INVALID then
        begin
          res := self._currentValue;
        end;
      res := res * self._iresol;
      res := round(res) / self._iresol;
      result := res;
      exit;
    end;


  function TYSensor.set_lowestValue(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('lowestValue',rest_val);
    end;

  function TYSensor.get_lowestValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LOWESTVALUE_INVALID;
              exit;
            end;
        end;
      res := self._lowestValue * self._iresol;
      res := round(res) / self._iresol;
      result := res;
      exit;
    end;


  function TYSensor.set_highestValue(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('highestValue',rest_val);
    end;

  function TYSensor.get_highestValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_HIGHESTVALUE_INVALID;
              exit;
            end;
        end;
      res := self._highestValue * self._iresol;
      res := round(res) / self._iresol;
      result := res;
      exit;
    end;


  function TYSensor.get_currentRawValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTRAWVALUE_INVALID;
              exit;
            end;
        end;
      res := self._currentRawValue;
      result := res;
      exit;
    end;


  function TYSensor.get_logFrequency():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_LOGFREQUENCY_INVALID;
              exit;
            end;
        end;
      res := self._logFrequency;
      result := res;
      exit;
    end;


  function TYSensor.set_logFrequency(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('logFrequency',rest_val);
    end;

  function TYSensor.get_reportFrequency():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_REPORTFREQUENCY_INVALID;
              exit;
            end;
        end;
      res := self._reportFrequency;
      result := res;
      exit;
    end;


  function TYSensor.set_reportFrequency(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('reportFrequency',rest_val);
    end;

  function TYSensor.get_advMode():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_ADVMODE_INVALID;
              exit;
            end;
        end;
      res := self._advMode;
      result := res;
      exit;
    end;


  function TYSensor.set_advMode(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('advMode',rest_val);
    end;

  function TYSensor.get_calibrationParam():string;
    var
      res : string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CALIBRATIONPARAM_INVALID;
              exit;
            end;
        end;
      res := self._calibrationParam;
      result := res;
      exit;
    end;


  function TYSensor.set_calibrationParam(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('calibrationParam',rest_val);
    end;

  function TYSensor.set_resolution(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval * 65536.0));
      result := _setAttr('resolution',rest_val);
    end;

  function TYSensor.get_resolution():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RESOLUTION_INVALID;
              exit;
            end;
        end;
      res := self._resolution;
      result := res;
      exit;
    end;


  function TYSensor.get_sensorState():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_SENSORSTATE_INVALID;
              exit;
            end;
        end;
      res := self._sensorState;
      result := res;
      exit;
    end;


  class function TYSensor.FindSensor(func: string):TYSensor;
    var
      obj : TYSensor;
    begin
      obj := TYSensor(TYFunction._FindFromCache('Sensor', func));
      if obj = nil then
        begin
          obj :=  TYSensor.create(func);
          TYFunction._AddToCache('Sensor',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYSensor.registerValueCallback(callback: TYSensorValueCallback):LongInt;
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
      self._valueCallbackSensor := callback;
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


  function TYSensor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackSensor) <> nil) then
        begin
          self._valueCallbackSensor(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYSensor._parserHelper():LongInt;
    var
      position : LongInt;
      maxpos : LongInt;
      iCalib : TLongIntArray;
      iRaw : LongInt;
      iRef : LongInt;
      fRaw : double;
      fRef : double;
      calpar_pos : LongInt;
      calraw_pos : LongInt;
      calref_pos : LongInt;
    begin
      self._caltyp := -1;
      self._scale := -1;
      SetLength(self._calpar, 0);
      SetLength(self._calraw, 0);
      SetLength(self._calref, 0);
      // Store inverted resolution, to provide better rounding
      if self._resolution > 0 then
        begin
          self._iresol := round(1.0 / self._resolution);
        end
      else
        begin
          self._iresol := 10000;
          self._resolution := 0.0001;
        end;
      // Old format: supported when there is no calibration
      if (self._calibrationParam = '') or (self._calibrationParam = '0') then
        begin
          self._caltyp := 0;
          result := 0;
          exit;
        end;
      if (pos(',', self._calibrationParam) - 1) >= 0 then
        begin
          // Plain text format
          iCalib := _decodeFloats(self._calibrationParam);
          self._caltyp := (iCalib[0] div 1000);
          if self._caltyp > 0 then
            begin
              if self._caltyp < YOCTO_CALIB_TYPE_OFS then
                begin
                  // Unknown calibration type: calibrated value will be provided by the device
                  self._caltyp := -1;
                  result := 0;
                  exit;
                end;
              self._calhdl := _getCalibrationHandler(self._caltyp);
              if not((addr(self._calhdl) <> nil)) then
                begin
                  // Unknown calibration type: calibrated value will be provided by the device
                  self._caltyp := -1;
                  result := 0;
                  exit;
                end;
            end;
          // New 32 bits text format
          self._offset := 0;
          self._scale := 1000;
          maxpos := length(iCalib);
          calpar_pos := 0;
          SetLength(self._calpar, maxpos);
          position := 1;
          while position < maxpos do
            begin
              self._calpar[calpar_pos] := iCalib[position];
              inc(calpar_pos);
              position := position + 1;
            end;
          SetLength(self._calpar, calpar_pos);
          calraw_pos := 0;
          SetLength(self._calraw, maxpos);
          calref_pos := 0;
          SetLength(self._calref, maxpos);
          position := 1;
          while position + 1 < maxpos do
            begin
              fRaw := iCalib[position];
              fRaw := fRaw / 1000.0;
              fRef := iCalib[position + 1];
              fRef := fRef / 1000.0;
              self._calraw[calraw_pos] := fRaw;
              inc(calraw_pos);
              self._calref[calref_pos] := fRef;
              inc(calref_pos);
              position := position + 2;
            end;
          SetLength(self._calraw, calraw_pos);
          SetLength(self._calref, calref_pos);
        end
      else
        begin
          // Recorder-encoded format, including encoding
          iCalib := _decodeWords(self._calibrationParam);
          // In case of unknown format, calibrated value will be provided by the device
          if length(iCalib) < 2 then
            begin
              self._caltyp := -1;
              result := 0;
              exit;
            end;
          // Save variable format (scale for scalar, or decimal exponent)
          self._offset := 0;
          self._scale := 1;
          self._decexp := 1.0;
          position := iCalib[0];
          while position > 0 do
            begin
              self._decexp := self._decexp * 10;
              position := position - 1;
            end;
          // Shortcut when there is no calibration parameter
          if length(iCalib) = 2 then
            begin
              self._caltyp := 0;
              result := 0;
              exit;
            end;
          self._caltyp := iCalib[2];
          self._calhdl := _getCalibrationHandler(self._caltyp);
          // parse calibration points
          if self._caltyp <= 10 then
            begin
              maxpos := self._caltyp;
            end
          else
            begin
              if self._caltyp <= 20 then
                begin
                  maxpos := self._caltyp - 10;
                end
              else
                begin
                  maxpos := 5;
                end;
            end;
          maxpos := 3 + 2 * maxpos;
          if maxpos > length(iCalib) then
            begin
              maxpos := length(iCalib);
            end;
          calpar_pos := 0;
          SetLength(self._calpar, maxpos);
          calraw_pos := 0;
          SetLength(self._calraw, maxpos);
          calref_pos := 0;
          SetLength(self._calref, maxpos);
          position := 3;
          while position + 1 < maxpos do
            begin
              iRaw := iCalib[position];
              iRef := iCalib[position + 1];
              self._calpar[calpar_pos] := iRaw;
              inc(calpar_pos);
              self._calpar[calpar_pos] := iRef;
              inc(calpar_pos);
              self._calraw[calraw_pos] := _decimalToDouble(iRaw);
              inc(calraw_pos);
              self._calref[calref_pos] := _decimalToDouble(iRef);
              inc(calref_pos);
              position := position + 2;
            end;
          SetLength(self._calpar, calpar_pos);
          SetLength(self._calraw, calraw_pos);
          SetLength(self._calref, calref_pos);
        end;
      result := 0;
      exit;
    end;


  function TYSensor.isSensorReady():boolean;
    begin
      if not(self.isOnline) then
        begin
          result := false;
          exit;
        end;
      if not(self._sensorState = 0) then
        begin
          result := false;
          exit;
        end;
      result := true;
      exit;
    end;


  function TYSensor.get_dataLogger():TYDataLogger;
    var
      logger : TYDataLogger;
      modu : TYModule;
      serial : string;
      hwid : string;
    begin
      modu := self.get_module;
      serial := modu.get_serialNumber();
      if (serial = YAPI_INVALID_STRING) then
        begin
          result := nil;
          exit;
        end;
      hwid := serial + '.dataLogger';
      logger := TYDataLogger.FindDataLogger(hwid);
      result := logger;
      exit;
    end;


  function TYSensor.startDataLogger():LongInt;
    var
      res : TByteArray;
    begin
      res := self._download('api/dataLogger/recording?recording=1');
      if not(length(res)>0) then
        begin
          self._throw( YAPI_IO_ERROR, 'unable to start datalogger');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSensor.stopDataLogger():LongInt;
    var
      res : TByteArray;
    begin
      res := self._download('api/dataLogger/recording?recording=0');
      if not(length(res)>0) then
        begin
          self._throw( YAPI_IO_ERROR, 'unable to stop datalogger');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSensor.get_recordedData(startTime: double; endTime: double):TYDataSet;
    var
      funcid : string;
      funit : string;
    begin
      funcid := self.get_functionId;
      funit := self.get_unit;
      result := TYDataSet.create(self, funcid, funit, startTime, endTime);
      exit;
    end;


  function TYSensor.registerTimedReportCallback(callback: TYSensorTimedReportCallback):LongInt;
    var
      sensor : TYSensor;
    begin
      sensor := self;
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(sensor, true);
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(sensor, false);
        end;
      self._timedReportCallbackSensor := callback;
      result := 0;
      exit;
    end;


  function TYSensor._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackSensor) <> nil) then
        begin
          self._timedReportCallbackSensor(self, value);
        end
      else
        begin
        end;
      result := 0;
      exit;
    end;


  function TYSensor.calibrateFromPoints(rawValues: TDoubleArray; refValues: TDoubleArray):LongInt;
    var
      rest_val : string;
      res : LongInt;
    begin
      rest_val := self._encodeCalibrationPoints(rawValues, refValues);
      res := self._setAttr('calibrationParam', rest_val);
      result := res;
      exit;
    end;


  function TYSensor.loadCalibrationPoints(var rawValues: TDoubleArray; var refValues: TDoubleArray):LongInt;
    var
      rawValues_pos : LongInt;
      refValues_pos : LongInt;
      i_i : LongInt;
    begin
      SetLength(rawValues, 0);
      SetLength(refValues, 0);
      // Load function parameters if not yet loaded
      if self._scale = 0 then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := YAPI_DEVICE_NOT_FOUND;
              exit;
            end;
        end;
      if self._caltyp < 0 then
        begin
          self._throw(YAPI_NOT_SUPPORTED, 'Calibration parameters format mismatch. Please upgrade your lib'
          + 'rary or firmware.');
          result := YAPI_NOT_SUPPORTED;
          exit;
        end;
      rawValues_pos := 0;
      SetLength(rawValues, length(self._calraw));
      refValues_pos := 0;
      SetLength(refValues, length(self._calref));
      for i_i:=0 to length(self._calraw)-1 do
        begin
          rawValues[rawValues_pos] := self._calraw[i_i];
          inc(rawValues_pos);
        end;
      for i_i:=0 to length(self._calref)-1 do
        begin
          refValues[refValues_pos] := self._calref[i_i];
          inc(refValues_pos);
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSensor._encodeCalibrationPoints(rawValues: TDoubleArray; refValues: TDoubleArray):string;
    var
      res : string;
      npt : LongInt;
      idx : LongInt;
    begin
      npt := length(rawValues);
      if npt <> length(refValues) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT, 'Invalid calibration parameters (size mismatch)');
          result := YAPI_INVALID_STRING;
          exit;
        end;
      // Shortcut when building empty calibration parameters
      if npt = 0 then
        begin
          result := '0';
          exit;
        end;
      // Load function parameters if not yet loaded
      if self._scale = 0 then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := YAPI_INVALID_STRING;
              exit;
            end;
        end;
      // Detect old firmware
      if (self._caltyp < 0) or(self._scale < 0) then
        begin
          self._throw(YAPI_NOT_SUPPORTED, 'Calibration parameters format mismatch. Please upgrade your lib'
          + 'rary or firmware.');
          result := '0';
          exit;
        end;
      // 32-bit fixed-point encoding
      res := ''+inttostr(YOCTO_CALIB_TYPE_OFS);
      idx := 0;
      while idx < npt do
        begin
          res := ''+ res+','+_yapiFloatToStr( rawValues[idx])+','+_yapiFloatToStr(refValues[idx]);
          idx := idx + 1;
        end;
      result := res;
      exit;
    end;


  function TYSensor._applyCalibration(rawValue: double):double;
    begin
      if rawValue = Y_CURRENTVALUE_INVALID then
        begin
          result := Y_CURRENTVALUE_INVALID;
          exit;
        end;
      if self._caltyp = 0 then
        begin
          result := rawValue;
          exit;
        end;
      if self._caltyp < 0 then
        begin
          result := Y_CURRENTVALUE_INVALID;
          exit;
        end;
      if not((addr(self._calhdl) <> nil)) then
        begin
          result := Y_CURRENTVALUE_INVALID;
          exit;
        end;
      result := self._calhdl(rawValue, self._caltyp, self._calpar, self._calraw, self._calref);
      exit;
    end;


  function TYSensor._decodeTimedReport(timestamp: double; duration: double; report: TLongIntArray):TYMeasure;
    var
      i : LongInt;
      byteVal : LongInt;
      poww : double;
      minRaw : double;
      avgRaw : double;
      maxRaw : double;
      sublen : LongInt;
      difRaw : double;
      startTime : double;
      endTime : double;
      minVal : double;
      avgVal : double;
      maxVal : double;
    begin
      if duration > 0 then
        begin
          startTime := timestamp - duration;
        end
      else
        begin
          startTime := self._prevTimedReport;
        end;
      endTime := timestamp;
      self._prevTimedReport := endTime;
      if startTime = 0 then
        begin
          startTime := endTime;
        end;
      // 32 bits timed report format
      if length(report) <= 5 then
        begin
          // sub-second report, 1-4 bytes
          poww := 1;
          avgRaw := 0;
          byteVal := 0;
          i := 1;
          while i < length(report) do
            begin
              byteVal := report[i];
              avgRaw := avgRaw + poww * byteVal;
              poww := poww * $0100;
              i := i + 1;
            end;
          if ((byteVal) and ($080)) <> 0 then
            begin
              avgRaw := avgRaw - poww;
            end;
          avgVal := avgRaw / 1000.0;
          if self._caltyp <> 0 then
            begin
              if (addr(self._calhdl) <> nil) then
                begin
                  avgVal := self._calhdl(avgVal, self._caltyp, self._calpar, self._calraw, self._calref);
                end;
            end;
          minVal := avgVal;
          maxVal := avgVal;
        end
      else
        begin
          // averaged report: avg,avg-min,max-avg
          sublen := 1 + ((report[1]) and 3);
          poww := 1;
          avgRaw := 0;
          byteVal := 0;
          i := 2;
          while (sublen > 0) and(i < length(report)) do
            begin
              byteVal := report[i];
              avgRaw := avgRaw + poww * byteVal;
              poww := poww * $0100;
              i := i + 1;
              sublen := sublen - 1;
            end;
          if ((byteVal) and ($080)) <> 0 then
            begin
              avgRaw := avgRaw - poww;
            end;
          sublen := 1 + ((((report[1]) shr 2)) and 3);
          poww := 1;
          difRaw := 0;
          while (sublen > 0) and(i < length(report)) do
            begin
              byteVal := report[i];
              difRaw := difRaw + poww * byteVal;
              poww := poww * $0100;
              i := i + 1;
              sublen := sublen - 1;
            end;
          minRaw := avgRaw - difRaw;
          sublen := 1 + ((((report[1]) shr 4)) and 3);
          poww := 1;
          difRaw := 0;
          while (sublen > 0) and(i < length(report)) do
            begin
              byteVal := report[i];
              difRaw := difRaw + poww * byteVal;
              poww := poww * $0100;
              i := i + 1;
              sublen := sublen - 1;
            end;
          maxRaw := avgRaw + difRaw;
          avgVal := avgRaw / 1000.0;
          minVal := minRaw / 1000.0;
          maxVal := maxRaw / 1000.0;
          if self._caltyp <> 0 then
            begin
              if (addr(self._calhdl) <> nil) then
                begin
                  avgVal := self._calhdl(avgVal, self._caltyp, self._calpar, self._calraw, self._calref);
                  minVal := self._calhdl(minVal, self._caltyp, self._calpar, self._calraw, self._calref);
                  maxVal := self._calhdl(maxVal, self._caltyp, self._calpar, self._calraw, self._calref);
                end;
            end;
        end;
      result := TYMeasure.create(startTime, endTime, minVal, avgVal, maxVal);
      exit;
    end;


  function TYSensor._decodeVal(w: LongInt):double;
    var
      val : double;
    begin
      val := w;
      if self._caltyp <> 0 then
        begin
          if (addr(self._calhdl) <> nil) then
            begin
              val := self._calhdl(val, self._caltyp, self._calpar, self._calraw, self._calref);
            end;
        end;
      result := val;
      exit;
    end;


  function TYSensor._decodeAvg(dw: LongInt):double;
    var
      val : double;
    begin
      val := dw;
      if self._caltyp <> 0 then
        begin
          if (addr(self._calhdl) <> nil) then
            begin
              val := self._calhdl(val, self._caltyp, self._calpar, self._calraw, self._calref);
            end;
        end;
      result := val;
      exit;
    end;


  function TYSensor.nextSensor(): TYSensor;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextSensor := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextSensor := nil;
          exit;
        end;
      nextSensor := TYSensor.FindSensor(hwid);
    end;

  class function TYSensor.FirstSensor(): TYSensor;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Sensor', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYSensor.FindSensor(serial+'.'+funcId);
    end;

//--- (end of generated code: YSensor implementation)

//--- (generated code: YSensor functions)

  function yFindSensor(func:string): TYSensor;
    begin
      result := TYSensor.FindSensor(func);
    end;

  function yFirstSensor(): TYSensor;
    begin
      result := TYSensor.FirstSensor();
    end;

  procedure _SensorCleanup();
    begin
    end;

//--- (end of generated code: YSensor functions)

//--- (generated code: YAPIContext dlldef)
//--- (end of generated code: YAPIContext dlldef)

  constructor TYAPIContext.Create();
    begin
      //--- (generated code: YAPIContext accessors initialization)
      _defaultCacheValidity := 5;
      //--- (end of generated code: YAPIContext accessors initialization)
    end;


//--- (generated code: YAPIContext implementation)

  procedure TYAPIContext.SetDeviceListValidity(deviceListValidity: LongInt);
    begin
      _yapiSetNetDevListValidity(deviceListValidity);
    end;


  function TYAPIContext.GetDeviceListValidity():LongInt;
    var
      res : LongInt;
    begin
      res := _yapiGetNetDevListValidity();
      result := res;
      exit;
    end;


  function TYAPIContext.AddUdevRule(force: boolean):string;
    var
      msg : string;
      res : LongInt;
      c_force : LongInt;
      errmsg_buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      errmsg : pansichar;
    begin
      errmsg_buffer[0]:=#0;errmsg:=@errmsg_buffer;
      if force then
        begin
          c_force := 1;
        end
      else
        begin
          c_force := 0;
        end;
      res := _yapiAddUdevRulesForYocto(c_force, errmsg);
      if res < 0 then
        begin
          msg := 'error: ' + string(errmsg);
        end
      else
        begin
          msg := '';
        end;
      result := msg;
      exit;
    end;


  procedure TYAPIContext.SetNetworkTimeout(networkMsTimeout: LongInt);
    begin
      _yapiSetNetworkTimeout(networkMsTimeout);
    end;


  function TYAPIContext.GetNetworkTimeout():LongInt;
    var
      res : LongInt;
    begin
      res := _yapiGetNetworkTimeout();
      result := res;
      exit;
    end;


  procedure TYAPIContext.SetCacheValidity(cacheValidityMs: u64);
    begin
      self._defaultCacheValidity := cacheValidityMs;
    end;


  function TYAPIContext.GetCacheValidity():u64;
    begin
      result := self._defaultCacheValidity;
      exit;
    end;


//--- (end of generated code: YAPIContext implementation)

//--- (generated code: YAPIContext functions)

  procedure _APIContextCleanup();
    begin
    end;

//--- (end of generated code: YAPIContext functions)


//--- (generated code: YAPIContext yapiwrapper)

  procedure ySetDeviceListValidity(deviceListValidity: LongInt);
    begin
        _yapiContext.SetDeviceListValidity(deviceListValidity);
    end;


  function yGetDeviceListValidity():LongInt;
    begin
        result := _yapiContext.GetDeviceListValidity();
    end;


  function yAddUdevRule(force: boolean):string;
    begin
        result := _yapiContext.AddUdevRule(force);
    end;


  procedure ySetNetworkTimeout(networkMsTimeout: LongInt);
    begin
        _yapiContext.SetNetworkTimeout(networkMsTimeout);
    end;


  function yGetNetworkTimeout():LongInt;
    begin
        result := _yapiContext.GetNetworkTimeout();
    end;


  procedure ySetCacheValidity(cacheValidityMs: u64);
    begin
        _yapiContext.SetCacheValidity(cacheValidityMs);
    end;


  function yGetCacheValidity():u64;
    begin
        result := _yapiContext.GetCacheValidity();
    end;

//--- (end of generated code: YAPIContext yapiwrapper)

  constructor TYDataStream.Create(parent : TYFunction);
    begin
      self._parent := parent;
    end;

  constructor TYDataStream.Create(parent : TYFunction; dataset : TYDataSet; encoded : TLongIntArray);
    begin
      self._parent := parent;
      // decode sequence header to extract data
      _initFromDataSet(dataset, encoded);
    end;

  constructor TYFirmwareUpdate.Create(serial: String; path :string; settings :TByteArray; force: boolean);
    begin
      _serial := serial;
      _firmwarepath := path;
      _settings := settings;
      _force := force;
    end;


//--- (generated code: YFirmwareUpdate implementation)

  function TYFirmwareUpdate._processMore(newupdate: LongInt):LongInt;
    var
      errmsg_buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      errmsg : pansichar;
      m : TYModule;
      res : LongInt;
      serial : string;
      firmwarepath : string;
      settings : string;
      prod_prefix : string;
      force : LongInt;
      ignoreErrMsg : string;
    begin
      errmsg_buffer[0]:=#0;errmsg:=@errmsg_buffer;
      if (self._progress_c < 100) and(self._progress_c <> YAPI_VERSION_MISMATCH) then
        begin
          serial := self._serial;
          firmwarepath := self._firmwarepath;
          settings := _ByteToString(self._settings);
          if self._force then
            begin
              force := 1;
            end
          else
            begin
              force := 0;
            end;
          res := _yapiUpdateFirmwareEx(pansichar(ansistring(serial)), pansichar(ansistring(firmwarepath)), pansichar(ansistring(settings)), force, newupdate, errmsg);
          if (res = YAPI_VERSION_MISMATCH) and(length(self._settings) <> 0) then
            begin
              self._progress_c := res;
              self._progress_msg := string(errmsg);
              result := self._progress;
              exit;
            end;
          if res < 0 then
            begin
              self._progress := res;
              self._progress_msg := string(errmsg);
              result := res;
              exit;
            end;
          self._progress_c := res;
          self._progress := (self._progress_c * 9 div 10);
          self._progress_msg := string(errmsg);
        end
      else
        begin
          if (length(self._settings) <> 0) then
            begin
              self._progress_msg := 'restoring settings';
              m := TYModule.FindModule(self._serial + '.module');
              if not(m.isOnline()) then
                begin
                  result := self._progress;
                  exit;
                end;
              if self._progress < 95 then
                begin
                  prod_prefix := Copy(m.get_productName(),  0 + 1, 8);
                  if (prod_prefix = 'YoctoHub') then
                    begin
                      ySleep(1000, ignoreErrMsg);
                      self._progress := self._progress + 1;
                      result := self._progress;
                      exit;
                    end
                  else
                    begin
                      self._progress := 95;
                    end;
                end;
              if self._progress < 100 then
                begin
                  m.set_allSettingsAndFiles(self._settings);
                  m.saveToFlash();
                  setlength(self._settings,0);
                  if self._progress_c = YAPI_VERSION_MISMATCH then
                    begin
                      self._progress := YAPI_IO_ERROR;
                      self._progress_msg := 'Unable to update firmware';
                    end
                  else
                    begin
                      self._progress := 100;
                      self._progress_msg := 'success';
                    end;
                end;
            end
          else
            begin
              self._progress := 100;
              self._progress_msg := 'success';
            end;
        end;
      result := self._progress;
      exit;
    end;


  class function TYFirmwareUpdate.GetAllBootLoaders():TStringArray;
    var
      errmsg_buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      errmsg : pansichar;
      smallbuff_buffer : array[0..1024] of ansichar;
      smallbuff : pansichar;
      bigbuff : pansichar;
      buffsize : LongInt;
      fullsize : LongInt;
      yapi_res : LongInt;
      bootloader_list : string;
      bootladers : TStringArray;
    begin
      errmsg_buffer[0]:=#0;errmsg:=@errmsg_buffer;
      smallbuff_buffer[0]:=#0;smallbuff:=@smallbuff_buffer;
      SetLength(bootladers, 0);
      fullsize := 0;
      yapi_res := _yapiGetBootloaders(smallbuff, 1024, fullsize, errmsg);
      if yapi_res < 0 then
        begin
          result := bootladers;
          exit;
        end;
      if fullsize <= 1024 then
        begin
          bootloader_list := string(smallbuff);
        end
      else
        begin
          buffsize := fullsize;
          getmem(bigbuff, buffsize);
          yapi_res := _yapiGetBootloaders(bigbuff, buffsize, fullsize, errmsg);
          if yapi_res < 0 then
            begin
              freemem(bigbuff);
              result := bootladers;
              exit;
            end
          else
            begin
              bootloader_list := string(bigbuff);
            end;
          freemem(bigbuff);
        end;
      if not((bootloader_list = '')) then
        begin
          bootladers := _stringSplit(bootloader_list, ',');
        end;
      result := bootladers;
      exit;
    end;


  class function TYFirmwareUpdate.CheckFirmware(serial: string; path: string; minrelease: LongInt):string;
    var
      errmsg_buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      errmsg : pansichar;
      smallbuff_buffer : array[0..1024] of ansichar;
      smallbuff : pansichar;
      bigbuff : pansichar;
      buffsize : LongInt;
      fullsize : LongInt;
      res : LongInt;
      firmware_path : string;
      release : string;
    begin
      errmsg_buffer[0]:=#0;errmsg:=@errmsg_buffer;
      smallbuff_buffer[0]:=#0;smallbuff:=@smallbuff_buffer;
      fullsize := 0;
      release := IntToStr(minrelease);
      res := _yapiCheckFirmware(pansichar(ansistring(serial)), pansichar(ansistring(release)), pansichar(ansistring(path)), smallbuff, 1024, fullsize, errmsg);
      if res < 0 then
        begin
          firmware_path := 'error:' + string(errmsg);
          result := 'error:' + string(errmsg);
          exit;
        end;
      if fullsize <= 1024 then
        begin
          firmware_path := string(smallbuff);
        end
      else
        begin
          buffsize := fullsize;
          getmem(bigbuff, buffsize);
          res := _yapiCheckFirmware(pansichar(ansistring(serial)), pansichar(ansistring(release)), pansichar(ansistring(path)), bigbuff, buffsize, fullsize, errmsg);
          if res < 0 then
            begin
              firmware_path := 'error:' + string(errmsg);
            end
          else
            begin
              firmware_path := string(bigbuff);
            end;
          freemem(bigbuff);
        end;
      result := firmware_path;
      exit;
    end;


  function TYFirmwareUpdate.get_progress():LongInt;
    begin
      if self._progress >= 0 then
        begin
          self._processMore(0);
        end;
      result := self._progress;
      exit;
    end;


  function TYFirmwareUpdate.get_progressMessage():string;
    begin
      result := self._progress_msg;
      exit;
    end;


  function TYFirmwareUpdate.startUpdate():LongInt;
    var
      err : string;
      leng : LongInt;
    begin
      err := _ByteToString(self._settings);
      leng := Length(err);
      if (leng >= 6) and(('error:' = Copy(err, 0 + 1, 6))) then
        begin
          self._progress := -1;
          self._progress_msg := Copy(err,  6 + 1, leng - 6);
        end
      else
        begin
          self._progress := 0;
          self._progress_c := 0;
          self._processMore(1);
        end;
      result := self._progress;
      exit;
    end;


//--- (end of generated code: YFirmwareUpdate implementation)


//--- (generated code: YDataStream implementation)

  function TYDataStream._initFromDataSet(dataset: TYDataSet; encoded: TLongIntArray):LongInt;
    var
      val : LongInt;
      i : LongInt;
      maxpos : LongInt;
      ms_offset : LongInt;
      samplesPerHour : LongInt;
      fRaw : double;
      fRef : double;
      iCalib : TLongIntArray;
      calpar_pos : LongInt;
      calraw_pos : LongInt;
      calref_pos : LongInt;
      columnNames_pos : LongInt;
    begin
      self._runNo := encoded[0] + (((encoded[1]) shl 16));
      self._utcStamp := encoded[2] + (((encoded[3]) shl 16));
      val := encoded[4];
      self._isAvg := (((val) and ($0100)) = 0);
      samplesPerHour := ((val) and ($0ff));
      if ((val) and ($0100)) <> 0 then
        begin
          samplesPerHour := samplesPerHour * 3600;
        end
      else
        begin
          if ((val) and ($0200)) <> 0 then
            begin
              samplesPerHour := samplesPerHour * 60;
            end;
        end;
      self._dataSamplesInterval := 3600.0 / samplesPerHour;
      ms_offset := encoded[6];
      if ms_offset < 1000 then
        begin
          // new encoding -> add the ms to the UTC timestamp
          self._startTime := self._utcStamp + (ms_offset / 1000.0);
        end
      else
        begin
          // legacy encoding subtract the measure interval form the UTC timestamp
          self._startTime := self._utcStamp -  self._dataSamplesInterval;
        end;
      self._firstMeasureDuration := encoded[5];
      if not(self._isAvg) then
        begin
          self._firstMeasureDuration := self._firstMeasureDuration / 1000.0;
        end;
      val := encoded[7];
      self._isClosed := (val <> $0ffff);
      if val = $0ffff then
        begin
          val := 0;
        end;
      self._nRows := val;
      if self._nRows > 0 then
        begin
          if self._firstMeasureDuration > 0 then
            begin
              self._duration := self._firstMeasureDuration + (self._nRows - 1) * self._dataSamplesInterval;
            end
          else
            begin
              self._duration := self._nRows * self._dataSamplesInterval;
            end;
        end
      else
        begin
          self._duration := 0;
        end;
      // precompute decoding parameters
      iCalib := dataset._get_calibration();
      self._caltyp := iCalib[0];
      if self._caltyp <> 0 then
        begin
          self._calhdl := _getCalibrationHandler(self._caltyp);
          maxpos := length(iCalib);
          calpar_pos := 0;
          SetLength(self._calpar, length(iCalib));
          calraw_pos := 0;
          SetLength(self._calraw, length(iCalib));
          calref_pos := 0;
          SetLength(self._calref, length(iCalib));
          i := 1;
          while i < maxpos do
            begin
              self._calpar[calpar_pos] := iCalib[i];
              inc(calpar_pos);
              i := i + 1;
            end;
          i := 1;
          while i + 1 < maxpos do
            begin
              fRaw := iCalib[i];
              fRaw := fRaw / 1000.0;
              fRef := iCalib[i + 1];
              fRef := fRef / 1000.0;
              self._calraw[calraw_pos] := fRaw;
              inc(calraw_pos);
              self._calref[calref_pos] := fRef;
              inc(calref_pos);
              i := i + 2;
            end;
          SetLength(self._calpar, calpar_pos);
          SetLength(self._calraw, calraw_pos);
          SetLength(self._calref, calref_pos);
        end;
      // preload column names for backward-compatibility
      self._functionId := dataset.get_functionId();
      if self._isAvg then
        begin
          columnNames_pos := 0;
          SetLength(self._columnNames, 3);
          self._columnNames[columnNames_pos] := ''+self._functionId+'_min';
          inc(columnNames_pos);
          self._columnNames[columnNames_pos] := ''+self._functionId+'_avg';
          inc(columnNames_pos);
          self._columnNames[columnNames_pos] := ''+self._functionId+'_max';
          inc(columnNames_pos);
          SetLength(self._columnNames, columnNames_pos);
          self._nCols := 3;
        end
      else
        begin
          columnNames_pos := 0;
          SetLength(self._columnNames, 1);
          self._columnNames[columnNames_pos] := self._functionId;
          inc(columnNames_pos);
          SetLength(self._columnNames, columnNames_pos);
          self._nCols := 1;
        end;
      // decode min/avg/max values for the sequence
      if self._nRows > 0 then
        begin
          self._avgVal := self._decodeAvg(encoded[8] + (((((encoded[9]) xor ($08000))) shl 16)), 1);
          self._minVal := self._decodeVal(encoded[10] + (((encoded[11]) shl 16)));
          self._maxVal := self._decodeVal(encoded[12] + (((encoded[13]) shl 16)));
        end;
      result := 0;
      exit;
    end;


  function TYDataStream._parseStream(sdata: TByteArray):LongInt;
    var
      idx : LongInt;
      udat : TLongIntArray;
      dat : TDoubleArray;
      values_pos : LongInt;
      dat_pos : LongInt;
    begin
      if self._isLoaded and not(self._isClosed) then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      if length(sdata) = 0 then
        begin
          self._nRows := 0;
          result := YAPI_SUCCESS;
          exit;
        end;

      udat := _decodeWords(self._parent._json_get_string(sdata));
      values_pos := 0;
      SetLength(self._values, length(udat));;
      idx := 0;
      if self._isAvg then
        begin
          while idx + 3 < length(udat) do
            begin
              dat_pos := 0;
              SetLength(dat, 3);
              if (udat[idx] = 65535) and(udat[idx + 1] = 65535) then
                begin
                  dat[dat_pos] := (0/0);
                  inc(dat_pos);
                  dat[dat_pos] := (0/0);
                  inc(dat_pos);
                  dat[dat_pos] := (0/0);
                  inc(dat_pos);
                end
              else
                begin
                  dat[dat_pos] := self._decodeVal(udat[idx + 2] + (((udat[idx + 3]) shl 16)));
                  inc(dat_pos);
                  dat[dat_pos] := self._decodeAvg(udat[idx] + (((((udat[idx + 1]) xor ($08000))) shl 16)), 1);
                  inc(dat_pos);
                  dat[dat_pos] := self._decodeVal(udat[idx + 4] + (((udat[idx + 5]) shl 16)));
                  inc(dat_pos);
                end;
              idx := idx + 6;
              SetLength(dat, dat_pos);
              self._values[values_pos] := dat;
              inc(values_pos);
            end;
        end
      else
        begin
          while idx + 1 < length(udat) do
            begin
              dat_pos := 0;
              SetLength(dat, 1);
              if (udat[idx] = 65535) and(udat[idx + 1] = 65535) then
                begin
                  dat[dat_pos] := (0/0);
                  inc(dat_pos);
                end
              else
                begin
                  dat[dat_pos] := self._decodeAvg(udat[idx] + (((((udat[idx + 1]) xor ($08000))) shl 16)), 1);
                  inc(dat_pos);
                end;
              SetLength(dat, dat_pos);
              self._values[values_pos] := dat;
              inc(values_pos);
              idx := idx + 2;
            end;
        end;
      SetLength(self._values, values_pos);
      self._nRows := length(self._values);
      self._isLoaded := true;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYDataStream._wasLoaded():boolean;
    begin
      result := self._isLoaded;
      exit;
    end;


  function TYDataStream._get_url():string;
    var
      url : string;
    begin
      url := 'logger.json?id='+
      self._functionId+'&run='+inttostr(self._runNo)+'&utc='+inttostr(self._utcStamp);
      result := url;
      exit;
    end;


  function TYDataStream._get_baseurl():string;
    var
      url : string;
    begin
      url := 'logger.json?id='+
      self._functionId+'&run='+inttostr(self._runNo)+'&utc=';
      result := url;
      exit;
    end;


  function TYDataStream._get_urlsuffix():string;
    var
      url : string;
    begin
      url := ''+inttostr(self._utcStamp);
      result := url;
      exit;
    end;


  function TYDataStream.loadStream():LongInt;
    begin
      result := self._parseStream(self._parent._download(self._get_url));
      exit;
    end;


  function TYDataStream._decodeVal(w: LongInt):double;
    var
      val : double;
    begin
      val := w;
      val := val / 1000.0;
      if self._caltyp <> 0 then
        begin
          if (addr(self._calhdl) <> nil) then
            begin
              val := self._calhdl(val, self._caltyp, self._calpar, self._calraw, self._calref);
            end;
        end;
      result := val;
      exit;
    end;


  function TYDataStream._decodeAvg(dw: LongInt; count: LongInt):double;
    var
      val : double;
    begin
      val := dw;
      val := val / 1000.0;
      if self._caltyp <> 0 then
        begin
          if (addr(self._calhdl) <> nil) then
            begin
              val := self._calhdl(val, self._caltyp, self._calpar, self._calraw, self._calref);
            end;
        end;
      result := val;
      exit;
    end;


  function TYDataStream.isClosed():boolean;
    begin
      result := self._isClosed;
      exit;
    end;


  function TYDataStream.get_runIndex():LongInt;
    begin
      result := self._runNo;
      exit;
    end;


  function TYDataStream.get_startTime():LongInt;
    begin
      result := integer(self._utcStamp - Round((Now()-25569)*86400));
      exit;
    end;


  function TYDataStream.get_startTimeUTC():int64;
    begin
      result := round(self._startTime);
      exit;
    end;


  function TYDataStream.get_realStartTimeUTC():double;
    begin
      result := self._startTime;
      exit;
    end;


  function TYDataStream.get_dataSamplesIntervalMs():LongInt;
    begin
      result := round(self._dataSamplesInterval*1000);
      exit;
    end;


  function TYDataStream.get_dataSamplesInterval():double;
    begin
      result := self._dataSamplesInterval;
      exit;
    end;


  function TYDataStream.get_firstDataSamplesInterval():double;
    begin
      result := self._firstMeasureDuration;
      exit;
    end;


  function TYDataStream.get_rowCount():LongInt;
    begin
      if (self._nRows <> 0) and self._isClosed then
        begin
          result := self._nRows;
          exit;
        end;
      self.loadStream;
      result := self._nRows;
      exit;
    end;


  function TYDataStream.get_columnCount():LongInt;
    begin
      if self._nCols <> 0 then
        begin
          result := self._nCols;
          exit;
        end;
      self.loadStream;
      result := self._nCols;
      exit;
    end;


  function TYDataStream.get_columnNames():TStringArray;
    begin
      if length(self._columnNames) <> 0 then
        begin
          result := self._columnNames;
          exit;
        end;
      self.loadStream;
      result := self._columnNames;
      exit;
    end;


  function TYDataStream.get_minValue():double;
    begin
      result := self._minVal;
      exit;
    end;


  function TYDataStream.get_averageValue():double;
    begin
      result := self._avgVal;
      exit;
    end;


  function TYDataStream.get_maxValue():double;
    begin
      result := self._maxVal;
      exit;
    end;


  function TYDataStream.get_realDuration():double;
    begin
      if self._isClosed then
        begin
          result := self._duration;
          exit;
        end;
      result := Round((Now()-25569)*86400) - self._utcStamp;
      exit;
    end;


  function TYDataStream.get_dataRows():TDoubleArrayArray;
    begin
      if (length(self._values) = 0) or not(self._isClosed) then
        begin
          self.loadStream;
        end;
      result := self._values;
      exit;
    end;


  function TYDataStream.get_data(row: LongInt; col: LongInt):double;
    begin
      if (length(self._values) = 0) or not(self._isClosed) then
        begin
          self.loadStream;
        end;
      if row >= length(self._values) then
        begin
          result := Y_DATA_INVALID;
          exit;
        end;
      if col >= length(self._values[row]) then
        begin
          result := Y_DATA_INVALID;
          exit;
        end;
      result := self._values[row][col];
      exit;
    end;


//--- (end of generated code: YDataStream implementation)

  constructor TYMeasure.Create(start, endt, minVal, avgVal, maxVal :double);
    var
      epoch : TDateTime;
    begin
      self._start := start;
      self._end := endt;
      self._minVal := minVal;
      self._avgVal := avgVal;
      self._maxVal := maxVal;

      epoch := 25569.0;
      self._startDateTime :=(start / 86400) + epoch;
      self._endDateTime :=(endt / 86400) + epoch;
    end;


  function TYMeasure.get_startTimeUTC_asTDateTime():TDateTime;
    begin
      result := self._startDateTime;
    end;

  function TYMeasure.get_endTimeUTC_asTDateTime():TDateTime;
    begin
      result := self._endDateTime;
    end;


//--- (generated code: YMeasure implementation)

  function TYMeasure.get_startTimeUTC():double;
    begin
      result := self._start;
      exit;
    end;


  function TYMeasure.get_endTimeUTC():double;
    begin
      result := self._end;
      exit;
    end;


  function TYMeasure.get_minValue():double;
    begin
      result := self._minVal;
      exit;
    end;


  function TYMeasure.get_averageValue():double;
    begin
      result := self._avgVal;
      exit;
    end;


  function TYMeasure.get_maxValue():double;
    begin
      result := self._maxVal;
      exit;
    end;


//--- (end of generated code: YMeasure implementation)





  constructor TYDataSet.Create(parent:TYFunction; functionId,func_unit:string; startTime,endTime: double);
    begin
      self._parent     := parent;
      self._functionId := functionId;
      self._unit       := func_unit;
      self._startTimeMs:= startTime *1000;
      self._endTimeMS  := endTime * 1000;
      self._summary    := TYMeasure.create(0, 0, 0, 0, 0);
      self._progress   := -1;
    end;

  constructor TYDataSet.Create(parent:TYFunction);
    begin
      self._parent := parent;
      self._startTimeMs := 0;
      self._endTimeMs := 0;
      self._summary := TYMeasure.create(0, 0, 0, 0, 0);
    end;

  destructor TYDataSet.Destroy();
    var i: integer;
    begin
      self._summary.free();
      for i := length(self._preview)-1 downto 0 do
        self._preview[i].Free();
      setLength(self._preview,0);
      for i := length(self._measures)-1 downto 0 do
        self._measures[i].Free();
      setLength(self._measures,0);
      inherited Destroy();
    end;

  function TYDataSet._parse(data:string):integer;
    var
      p : TJsonParser;
      node, arr : PJSONRECORD;
      stream : TYDataStream;
      i : integer;
      streamEndTime, streamStartTime: double;
    begin
      if not(YAPI_ExceptionsDisabled) then  p := TJsonParser.create(data, false)
      else
      try
        p := TJsonParser.create(data, false)
      except
        on E: Exception do
          begin
            result:=YAPI_NOT_SUPPORTED;
            exit;
          end;
      end;
      node := p.GetChildNode(nil, 'id');
      self._functionId := string(node.svalue);
      node := p.GetChildNode(nil, 'unit');
      self._unit := string(node.svalue);
      node := p.GetChildNode(nil, 'bulk');
      if(node <> nil) then
        begin
          self._bulkLoad := _atoi(string(node.svalue));
        end;
      node := p.GetChildNode(nil, 'calib');
      if(node <> nil) then
        begin
          self._calib := _decodeFloats(string(node.svalue));
          self._calib[0] := self._calib[0] div 1000;
        end
      else
        begin
          node := p.GetChildNode(nil, 'cal');
          self._calib := _decodeWords(string(node.svalue));
        end;
      arr := p.GetChildNode(nil, 'streams');
      SetLength(self._streams, 0);
      SetLength(self._preview, 0);
      SetLength(self._measures, 0);
      for i := 0 to arr.itemcount-1 do
        begin
          stream := _parent._findDataStream(self, string(arr.items[i].svalue));
          streamStartTime := round(stream.get_realStartTimeUTC() * 1000);
          streamEndTime := streamStartTime + round(stream.get_realDuration() * 1000);
          if (self._startTimeMs > 0) and (streamEndTime <= self._startTimeMs) then
            begin
              // self stream is too early, drop it
            end
          else
          if (self._endTimeMs > 0) and (streamStartTime >= self._endTimeMs) then
            begin
              // self stream is too late, drop it
            end
          else
            begin
              SetLength(self._streams, length(self._streams) + 1);
              self._streams[length(self._streams)-1] := stream;
            end;
        end;
      self._progress := 0;
      p.free();
      result := self.get_progress();
    end;



//--- (generated code: YDataSet implementation)

  function TYDataSet._get_calibration():TLongIntArray;
    begin
      result := self._calib;
      exit;
    end;


  function TYDataSet.loadSummary(data: TByteArray):LongInt;
    var
      dataRows : TDoubleArrayArray;
      tim : double;
      mitv : double;
      itv : double;
      fitv : double;
      end_ : double;
      nCols : LongInt;
      minCol : LongInt;
      avgCol : LongInt;
      maxCol : LongInt;
      res : LongInt;
      m_pos : LongInt;
      previewTotalTime : double;
      previewTotalAvg : double;
      previewMinVal : double;
      previewMaxVal : double;
      previewAvgVal : double;
      previewStartMs : double;
      previewStopMs : double;
      previewDuration : double;
      streamStartTimeMs : double;
      streamDuration : double;
      streamEndTimeMs : double;
      minVal : double;
      avgVal : double;
      maxVal : double;
      summaryStartMs : double;
      summaryStopMs : double;
      summaryTotalTime : double;
      summaryTotalAvg : double;
      summaryMinVal : double;
      summaryMaxVal : double;
      url : string;
      strdata : string;
      measure_data : TDoubleArray;
      preview_pos : LongInt;
      i_i : LongInt;
    begin
      if self._progress < 0 then
        begin
          strdata := _ByteToString(data);
          if (strdata = '{}') then
            begin
              self._parent._throw(YAPI_VERSION_MISMATCH, 'device firmware is too old');
              result := YAPI_VERSION_MISMATCH;
              exit;
            end;
          res := self._parse(strdata);
          if res < 0 then
            begin
              result := res;
              exit;
            end;
        end;
      summaryTotalTime := 0;
      summaryTotalAvg := 0;
      summaryMinVal := YAPI_MAX_DOUBLE;
      summaryMaxVal := YAPI_MIN_DOUBLE;
      summaryStartMs := YAPI_MAX_DOUBLE;
      summaryStopMs := YAPI_MIN_DOUBLE;
      preview_pos := length(self._preview);
      SetLength(self._preview, preview_pos+length(self._streams));
      // Parse complete streams
      for i_i:=0 to length( self._streams)-1 do
        begin
          streamStartTimeMs := round( self._streams[i_i].get_realStartTimeUTC() *1000);
          streamDuration :=  self._streams[i_i].get_realDuration() ;
          streamEndTimeMs := streamStartTimeMs + round(streamDuration * 1000);
          if (streamStartTimeMs >= self._startTimeMs) and((self._endTimeMs = 0) or(streamEndTimeMs <= self._endTimeMs)) then
            begin
              // stream that are completely inside the dataset
              previewMinVal :=  self._streams[i_i].get_minValue();
              previewAvgVal :=  self._streams[i_i].get_averageValue();
              previewMaxVal :=  self._streams[i_i].get_maxValue();
              previewStartMs := streamStartTimeMs;
              previewStopMs := streamEndTimeMs;
              previewDuration := streamDuration;
            end
          else
            begin
              // stream that are partially in the dataset
              // we need to parse data to filter value outside the dataset
              if not( self._streams[i_i]._wasLoaded()) then
                begin
                  url :=  self._streams[i_i]._get_url();
                  data := self._parent._download(url);
                  self._streams[i_i]._parseStream(data);
                end;
              dataRows :=  self._streams[i_i].get_dataRows();
              if length(dataRows) = 0 then
                begin
                  result := self.get_progress;
                  exit;
                end;
              tim := streamStartTimeMs;
              fitv := round( self._streams[i_i].get_firstDataSamplesInterval() * 1000);
              itv := round( self._streams[i_i].get_dataSamplesInterval() * 1000);
              nCols := length(dataRows[0]);
              minCol := 0;
              if nCols > 2 then
                begin
                  avgCol := 1;
                end
              else
                begin
                  avgCol := 0;
                end;
              if nCols > 2 then
                begin
                  maxCol := 2;
                end
              else
                begin
                  maxCol := 0;
                end;
              previewTotalTime := 0;
              previewTotalAvg := 0;
              previewStartMs := streamEndTimeMs;
              previewStopMs := streamStartTimeMs;
              previewMinVal := YAPI_MAX_DOUBLE;
              previewMaxVal := YAPI_MIN_DOUBLE;
              m_pos := 0;
              while m_pos < length(dataRows) do
                begin
                  measure_data  := dataRows[m_pos];
                  if m_pos = 0 then
                    begin
                      mitv := fitv;
                    end
                  else
                    begin
                      mitv := itv;
                    end;
                  end_ := tim + mitv;
                  if (end_ > self._startTimeMs) and((self._endTimeMs = 0) or(tim < self._endTimeMs)) then
                    begin
                      minVal := measure_data[minCol];
                      avgVal := measure_data[avgCol];
                      maxVal := measure_data[maxCol];
                      if previewStartMs > tim then
                        begin
                          previewStartMs := tim;
                        end;
                      if previewStopMs < end_ then
                        begin
                          previewStopMs := end_;
                        end;
                      if previewMinVal > minVal then
                        begin
                          previewMinVal := minVal;
                        end;
                      if previewMaxVal < maxVal then
                        begin
                          previewMaxVal := maxVal;
                        end;
                      if not(isNaN_D5(avgVal)) then
                        begin
                          previewTotalAvg := previewTotalAvg + (avgVal * mitv);
                          previewTotalTime := previewTotalTime + mitv;
                        end;
                    end;
                  tim := end_;
                  m_pos := m_pos + 1;
                end;
              if previewTotalTime > 0 then
                begin
                  previewAvgVal := previewTotalAvg / previewTotalTime;
                  previewDuration := (previewStopMs - previewStartMs) / 1000.0;
                end
              else
                begin
                  previewAvgVal := 0.0;
                  previewDuration := 0.0;
                end;
            end;
          self._preview[preview_pos] := TYMeasure.create(previewStartMs / 1000.0, previewStopMs / 1000.0, previewMinVal, previewAvgVal, previewMaxVal);
          inc(preview_pos);
          if summaryMinVal > previewMinVal then
            begin
              summaryMinVal := previewMinVal;
            end;
          if summaryMaxVal < previewMaxVal then
            begin
              summaryMaxVal := previewMaxVal;
            end;
          if summaryStartMs > previewStartMs then
            begin
              summaryStartMs := previewStartMs;
            end;
          if summaryStopMs < previewStopMs then
            begin
              summaryStopMs := previewStopMs;
            end;
          summaryTotalAvg := summaryTotalAvg + (previewAvgVal * previewDuration);
          summaryTotalTime := summaryTotalTime + previewDuration;
        end;
      if (self._startTimeMs = 0) or(self._startTimeMs > summaryStartMs) then
        begin
          self._startTimeMs := summaryStartMs;
        end;
      if (self._endTimeMs = 0) or(self._endTimeMs < summaryStopMs) then
        begin
          self._endTimeMs := summaryStopMs;
        end;
      if summaryTotalTime > 0 then
        begin
          self._summary :=  TYMeasure.create(summaryStartMs / 1000.0, summaryStopMs / 1000.0, summaryMinVal, summaryTotalAvg / summaryTotalTime, summaryMaxVal);
        end
      else
        begin
          self._summary :=  TYMeasure.create(0.0, 0.0, YAPI_INVALID_DOUBLE, YAPI_INVALID_DOUBLE, YAPI_INVALID_DOUBLE);
        end;
      result := self.get_progress;
      exit;
    end;


  function TYDataSet.processMore(progress: LongInt; data: TByteArray):LongInt;
    var
      stream : TYDataStream;
      dataRows : TDoubleArrayArray;
      tim : double;
      itv : double;
      fitv : double;
      avgv : double;
      end_ : double;
      nCols : LongInt;
      minCol : LongInt;
      avgCol : LongInt;
      maxCol : LongInt;
      firstMeasure : boolean;
      baseurl : string;
      url : string;
      suffix : string;
      suffixes : TStringArray;
      idx : LongInt;
      bulkFile : TByteArray;
      streamStr : TStringArray;
      urlIdx : LongInt;
      streamBin : TByteArray;
      measures_pos : LongInt;
      i_i : LongInt;
      suffixes_pos : LongInt;
    begin
      SetLength(suffixes, 0);
      SetLength(streamStr, 0);

      if progress <> self._progress then
        begin
          result := self._progress;
          exit;
        end;
      if self._progress < 0 then
        begin
          result := self.loadSummary(data);
          exit;
        end;
      stream := self._streams[self._progress];
      if not(stream._wasLoaded()) then
        begin
          stream._parseStream(data);
        end;
      dataRows := stream.get_dataRows();
      self._progress := self._progress + 1;
      if length(dataRows) = 0 then
        begin
          result := self.get_progress;
          exit;
        end;
      tim := round(stream.get_realStartTimeUTC() * 1000);
      fitv := round(stream.get_firstDataSamplesInterval() * 1000);
      itv := round(stream.get_dataSamplesInterval() * 1000);
      if fitv = 0 then
        begin
          fitv := itv;
        end;
      if tim < itv then
        begin
          tim := itv;
        end;
      nCols := length(dataRows[0]);
      minCol := 0;
      if nCols > 2 then
        begin
          avgCol := 1;
        end
      else
        begin
          avgCol := 0;
        end;
      if nCols > 2 then
        begin
          maxCol := 2;
        end
      else
        begin
          maxCol := 0;
        end;
      measures_pos := length(self._measures);
      SetLength(self._measures, measures_pos+length(dataRows));
      firstMeasure := true;
      for i_i:=0 to length(dataRows)-1 do
        begin
          if firstMeasure then
            begin
              end_ := tim + fitv;
              firstMeasure := false;
            end
          else
            begin
              end_ := tim + itv;
            end;
          avgv := dataRows[i_i][avgCol];
          if (end_ > self._startTimeMs) and((self._endTimeMs = 0) or(tim < self._endTimeMs)) and not(isNaN_D5(avgv)) then
            begin
              self._measures[measures_pos] := TYMeasure.create(tim / 1000, end_ / 1000, dataRows[i_i][minCol], avgv, dataRows[i_i][maxCol]);
              inc(measures_pos);
            end;
          tim := end_;
        end;
      SetLength(self._measures, measures_pos);;
      // Perform bulk preload to speed-up network transfer
      if (self._bulkLoad > 0) and(self._progress < length(self._streams)) then
        begin
          stream := self._streams[self._progress];
          if stream._wasLoaded() then
            begin
              result := self.get_progress;
              exit;
            end;
          baseurl := stream._get_baseurl();
          url := stream._get_url();
          suffix := stream._get_urlsuffix();
          suffixes_pos := length(suffixes);
          SetLength(suffixes, suffixes_pos+self._bulkLoad);
          suffixes[suffixes_pos] := suffix;
          inc(suffixes_pos);
          idx := self._progress+1;
          while (idx < length(self._streams)) and(length(suffixes) < self._bulkLoad) do
            begin
              stream := self._streams[idx];
              if not(stream._wasLoaded()) and((stream._get_baseurl() = baseurl)) then
                begin
                  suffix := stream._get_urlsuffix();
                  suffixes[suffixes_pos] := suffix;
                  inc(suffixes_pos);
                  url := url + ',' + suffix;
                end;
              idx := idx + 1;
            end;
          bulkFile := self._parent._download(url);
          streamStr := self._parent._json_get_array(bulkFile);
          urlIdx := 0;
          idx := self._progress;
          while (idx < length(self._streams)) and(urlIdx < length(suffixes)) and(urlIdx < length(streamStr)) do
            begin
              stream := self._streams[idx];
              if ((stream._get_baseurl() = baseurl)) and((stream._get_urlsuffix() = suffixes[urlIdx])) then
                begin
                  streamBin := _StrToByte(streamStr[urlIdx]);
                  stream._parseStream(streamBin);
                  urlIdx := urlIdx + 1;
                end;
              idx := idx + 1;
            end;
        end;
      result := self.get_progress;
      exit;
    end;


  function TYDataSet.get_privateDataStreams():TYDataStreamArray;
    begin
      result := self._streams;
      exit;
    end;


  function TYDataSet.get_hardwareId():string;
    var
      mo : TYModule;
    begin
      if not((self._hardwareId = '')) then
        begin
          result := self._hardwareId;
          exit;
        end;
      mo := self._parent.get_module();
      self._hardwareId := ''+ mo.get_serialNumber()+'.'+self.get_functionId;
      result := self._hardwareId;
      exit;
    end;


  function TYDataSet.get_functionId():string;
    begin
      result := self._functionId;
      exit;
    end;


  function TYDataSet.get_unit():string;
    begin
      result := self._unit;
      exit;
    end;


  function TYDataSet.get_startTimeUTC():int64;
    begin
      result := self.imm_get_startTimeUTC;
      exit;
    end;


  function TYDataSet.imm_get_startTimeUTC():int64;
    begin
      result := floor((self._startTimeMs / 1000.0));
      exit;
    end;


  function TYDataSet.get_endTimeUTC():int64;
    begin
      result := self.imm_get_endTimeUTC;
      exit;
    end;


  function TYDataSet.imm_get_endTimeUTC():int64;
    begin
      result := floor(round(self._endTimeMs / 1000.0));
      exit;
    end;


  function TYDataSet.get_progress():LongInt;
    begin
      if self._progress < 0 then
        begin
          result := 0;
          exit;
        end;
      // index not yet loaded
      if self._progress >= length(self._streams) then
        begin
          result := 100;
          exit;
        end;
      result := (1 + (1 + self._progress) * 98  div (1 + length(self._streams)));
      exit;
    end;


  function TYDataSet.loadMore():LongInt;
    var
      url : string;
      stream : TYDataStream;
    begin
      if self._progress < 0 then
        begin
          url := 'logger.json?id='+self._functionId;
          if self._startTimeMs <> 0 then
            begin
              url := ''+url+'&from='+inttostr(self.imm_get_startTimeUTC);
            end;
          if self._endTimeMs <> 0 then
            begin
              url := ''+url+'&to='+inttostr(self.imm_get_endTimeUTC+1);
            end;
        end
      else
        begin
          if self._progress >= length(self._streams) then
            begin
              result := 100;
              exit;
            end
          else
            begin
              stream := self._streams[self._progress];
              if stream._wasLoaded() then
                begin
                  // Do not reload stream if it was already loaded
                  result := self.processMore(self._progress, _StrToByte(''));
                  exit;
                end;
              url := stream._get_url();
            end;
        end;
      Try
        result := self.processMore(self._progress, self._parent._download(url));
        exit;
      Except
        result := self.processMore(self._progress, self._parent._download(url));
        exit;
      End;
    end;


  function TYDataSet.get_summary():TYMeasure;
    begin
      result := self._summary;
      exit;
    end;


  function TYDataSet.get_preview():TYMeasureArray;
    begin
      result := self._preview;
      exit;
    end;


  function TYDataSet.get_measuresAt(measure: TYMeasure):TYMeasureArray;
    var
      startUtcMs : double;
      stream : TYDataStream;
      dataRows : TDoubleArrayArray;
      measures : TYMeasureArray;
      tim : double;
      itv : double;
      end_ : double;
      nCols : LongInt;
      minCol : LongInt;
      avgCol : LongInt;
      maxCol : LongInt;
      i_i : LongInt;
      measures_pos : LongInt;
    begin
      startUtcMs := measure.get_startTimeUTC * 1000;
      stream := nil;
      for i_i:=0 to length(self._streams)-1 do
        begin
          if round(self._streams[i_i].get_realStartTimeUTC() *1000) = startUtcMs then
            begin
              stream := self._streams[i_i];
            end;
        end;
      if stream = nil then
        begin
          result := measures;
          exit;
        end;
      dataRows := stream.get_dataRows();
      if length(dataRows) = 0 then
        begin
          result := measures;
          exit;
        end;
      tim := round(stream.get_realStartTimeUTC() * 1000);
      itv := round(stream.get_dataSamplesInterval() * 1000);
      if tim < itv then
        begin
          tim := itv;
        end;
      nCols := length(dataRows[0]);
      minCol := 0;
      if nCols > 2 then
        begin
          avgCol := 1;
        end
      else
        begin
          avgCol := 0;
        end;
      if nCols > 2 then
        begin
          maxCol := 2;
        end
      else
        begin
          maxCol := 0;
        end;
      measures_pos := length(measures);
      SetLength(measures, measures_pos+length(dataRows));
      for i_i:=0 to length(dataRows)-1 do
        begin
          end_ := tim + itv;
          if (end_ > self._startTimeMs) and((self._endTimeMs = 0) or(tim < self._endTimeMs)) then
            begin
              measures[measures_pos] := TYMeasure.create(tim / 1000.0, end_ / 1000.0, dataRows[i_i][minCol], dataRows[i_i][avgCol], dataRows[i_i][maxCol]);
              inc(measures_pos);
            end;
          tim := end_;
        end;
      SetLength(measures, measures_pos);;
      result := measures;
      exit;
    end;


  function TYDataSet.get_measures():TYMeasureArray;
    begin
      result := self._measures;
      exit;
    end;


//--- (end of generated code: YDataSet implementation)


  constructor TYConsolidatedDataSet.Create(startTime,endTime: double; sensorList: TYSensorArray);
    begin
      self.imm_init(startTime, endTime, sensorList);
    end;

  destructor TYConsolidatedDataSet.Destroy();
    var
      i : LongInt;
    begin
      for i := length(self._datasets)-1 downto 0 do
        self._datasets[i].Free();
      setLength(self._datasets,0);
      setLength(self._progresss,0);
      setLength(self._nextidx,0);
      setLength(self._nexttim,0);
      inherited Destroy();
    end;


//--- (generated code: YConsolidatedDataSet implementation)

  function TYConsolidatedDataSet.imm_init(startt: double; endt: double; sensorList: TYSensorArray):LongInt;
    begin
      self._start := startt;
      self._end := endt;
      self._sensors := sensorList;
      self._nsensors := -1;
      result := YAPI_SUCCESS;
      exit;
    end;


  class function TYConsolidatedDataSet.Init(sensorNames: TStringArray; startTime: double; endTime: double):TYConsolidatedDataSet;
    var
      nSensors : LongInt;
      sensorList : TYSensorArray;
      idx : LongInt;
      sensorName : string;
      s : TYSensor;
      obj : TYConsolidatedDataSet;
      sensorList_pos : LongInt;
    begin
      nSensors := length(sensorNames);
      sensorList_pos := 0;
      SetLength(sensorList, nSensors);;
      idx := 0;
      while idx < nSensors do
        begin
          sensorName := sensorNames[idx];
          s := TYSensor.FindSensor(sensorName);
          sensorList[sensorList_pos] := s;
          inc(sensorList_pos);
          idx := idx + 1;
        end;
      SetLength(sensorList, sensorList_pos);;
      obj :=  TYConsolidatedDataSet.create(startTime, endTime, sensorList);
      result := obj;
      exit;
    end;


  function TYConsolidatedDataSet.nextRecord(var datarec: TDoubleArray):LongInt;
    var
      s : LongInt;
      idx : LongInt;
      sensor : TYSensor;
      newdataset : TYDataSet;
      globprogress : LongInt;
      currprogress : LongInt;
      currnexttim : double;
      newvalue : double;
      measures : TYMeasureArray;
      nexttime : double;
      datasets_pos : LongInt;
      progresss_pos : LongInt;
      nextidx_pos : LongInt;
      nexttim_pos : LongInt;
      datarec_pos : LongInt;
    begin
      if self._nsensors = -1 then
        begin
          self._nsensors := length(self._sensors);
          datasets_pos := 0;
          SetLength(self._datasets, self._nsensors);
          progresss_pos := 0;
          SetLength(self._progresss, self._nsensors);
          nextidx_pos := 0;
          SetLength(self._nextidx, self._nsensors);
          nexttim_pos := 0;
          SetLength(self._nexttim, self._nsensors);
          s := 0;
          while s < self._nsensors do
            begin
              sensor := self._sensors[s];
              newdataset := sensor.get_recordedData(self._start, self._end);
              self._datasets[datasets_pos] := newdataset;
              inc(datasets_pos);
              self._progresss[progresss_pos] := 0;
              inc(progresss_pos);
              self._nextidx[nextidx_pos] := 0;
              inc(nextidx_pos);
              self._nexttim[nexttim_pos] := 0.0;
              inc(nexttim_pos);
              s := s + 1;
            end;
          SetLength(self._datasets, datasets_pos);
          SetLength(self._progresss, progresss_pos);
          SetLength(self._nextidx, nextidx_pos);
          SetLength(self._nexttim, nexttim_pos);
        end;
      SetLength(datarec, 0);
      //
      // Find next timestamp to process
      //
      nexttime := 0;
      s := 0;
      while s < self._nsensors do
        begin
          currnexttim := self._nexttim[s];
          if currnexttim = 0 then
            begin
              idx := self._nextidx[s];
              measures := self._datasets[s].get_measures();
              currprogress := self._progresss[s];
              while (idx >= length(measures)) and(currprogress < 100) do
                begin
                  currprogress := self._datasets[s].loadMore();
                  if currprogress < 0 then
                    begin
                      currprogress := 100;
                    end;
                  self._progresss[ s] := currprogress;
                  measures := self._datasets[s].get_measures();
                end;
              if idx < length(measures) then
                begin
                  currnexttim := measures[idx].get_endTimeUTC();
                  self._nexttim[ s] := currnexttim;
                end;
            end;
          if currnexttim > 0 then
            begin
              if (nexttime = 0) or(nexttime > currnexttim) then
                begin
                  nexttime := currnexttim;
                end;
            end;
          s := s + 1;
        end;
      if nexttime = 0 then
        begin
          result := 100;
          exit;
        end;
      //
      // Extract data for this timestamp
      //
      datarec_pos := 0;
      SetLength(datarec, 1+self._nsensors);;
      datarec[datarec_pos] := nexttime;
      inc(datarec_pos);
      globprogress := 0;
      s := 0;
      while s < self._nsensors do
        begin
          if self._nexttim[s] = nexttime then
            begin
              idx := self._nextidx[s];
              measures := self._datasets[s].get_measures();
              newvalue := measures[idx].get_averageValue();
              datarec[datarec_pos] := newvalue;
              inc(datarec_pos);
              self._nexttim[ s] := 0.0;
              self._nextidx[ s] := idx+1;
            end
          else
            begin
              datarec[datarec_pos] := (0/0);
              inc(datarec_pos);
            end;
          currprogress := self._progresss[s];
          globprogress := globprogress + currprogress;
          s := s + 1;
        end;
      if globprogress > 0 then
        begin
          globprogress := (globprogress div self._nsensors);
          if globprogress > 99 then
            begin
              globprogress := 99;
            end;
        end;
      SetLength(datarec, datarec_pos);;
      result := globprogress;
      exit;
    end;


//--- (end of generated code: YConsolidatedDataSet implementation)


const
  OLDDATAURL ='/dataLogger.json';
  NEWDATAURL ='/logger.json';

   constructor TYDataLogger.Create(func:string);
    begin
      inherited Create(func);
      _className := 'DataLogger';
      //--- (generated code: YDataLogger accessors initialization)
      _currentRunIndex := Y_CURRENTRUNINDEX_INVALID;
      _timeUTC := Y_TIMEUTC_INVALID;
      _recording := Y_RECORDING_INVALID;
      _autoStart := Y_AUTOSTART_INVALID;
      _beaconDriven := Y_BEACONDRIVEN_INVALID;
      _usage := Y_USAGE_INVALID;
      _clearHistory := Y_CLEARHISTORY_INVALID;
      _valueCallbackDataLogger := nil;
      //--- (end of generated code: YDataLogger accessors initialization)
    end;


  function TYDataLogger.getData(runIdx: longword; timeIdx: longword; var jsondata: TJsonParser) :integer;
    var
      dev      : TYdevice;
      errmsg   : string;
      query    : string;
      buffer   : TByteArray;
      res      : integer;
    begin
      // Resolve our reference to our device, load REST API
      res := _getDevice(dev, errmsg);
      if(YISERR(res)) then
        begin
          _throw(res, errmsg);
          getData :=  res;
          exit;
        end;
      if  (_dataLoggerURL='') then
        _dataLoggerURL:=NEWDATAURL;
      if (timeIdx>0) then
        query :=    'GET '+_dataLoggerURL+'?run='+inttostr(runIdx)+'&time='+inttostr(timeIdx)+' HTTP/1.1'#13#10#13#10
      else
        query := 'GET '+_dataLoggerURL+' HTTP/1.1'#13#10#13#10;
      res := dev.HTTPRequest(query, buffer, errmsg);
      if(YISERR(res)) then
        begin
          res := yUpdateDeviceList(errmsg);
          if(YISERR(res)) then
            begin
              _throw(res, errmsg);
              getData :=  res;
              exit;
            end;
          res := dev.HTTPRequest(query, buffer, errmsg);
          if(YISERR(res)) then
            begin
              _throw(res, errmsg);
              getData :=  res;
              exit;
            end;
        end;
      try
        jsondata:= TJsonParser.create(string(buffer));
      except
        on E: Exception do
        begin
          errmsg:='unexpected JSON structure: '+e.Message;
          _throw(YAPI_IO_ERROR, errmsg);
          getData:=YAPI_IO_ERROR;
          exit;
        end;
      end;
      if ((jsondata.getHTTcode() = 404) And (_dataLoggerURL <> OLDDATAURL)) Then
        begin
          // retry using backward-compatible datalogger URL
          _dataLoggerURL := OLDDATAURL;
          getData := getData(runIdx, timeIdx, jsondata);
          exit;
        end;
      getData :=YAPI_SUCCESS;
    end;


  function  TYDataLogger.get_dataStreams(v: Tlist):integer;
    var
      j       : TJsonParser;
      sj,si   :integer;
      i,res   : integer;
      root,el : PJSONRECORD;
      json_buffer : string;
      sets        : TYDataSetArray;
      ds          : TYDataStreamArray;
    begin
      sets:=nil;
      ds:=nil;
      v.clear();
      res := getData(0, 0,  j);
      if (res <> YAPI_SUCCESS) then
        begin
          result := res;
          exit;
        end;
      root := j.GetRootNode();
      if root.itemcount = 0 then
        begin
          result := YAPI_SUCCESS;
          exit;
        end;
      if root.items[0].recordtype = JSON_ARRAY then
        begin
          for i:=0 to root.itemcount-1 do
            begin
              el := root.items[i] ;
              v.add(TYOldDataStream.create(self,el.items[0].ivalue,el.items[1].ivalue,el.items[2].ivalue,el.items[3].ivalue))
            end;
        end
      else
        begin
          json_buffer := j.convertToString(root,false);
          sets := self.parse_dataSets(_StrToByte(json_buffer));
          for sj := 0 to root.itemcount-1 do
            begin
              ds := sets[sj].get_privateDataStreams();
              for si := 0 to length(ds) do
                begin
                  v.Add(ds[si]);
                end;
            end;
        end;
      j.free();
      result:= YAPI_SUCCESS;
    end;


//--- (generated code: YDataLogger implementation)
{$HINTS OFF}
  function TYDataLogger._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'currentRunIndex') then
        begin
          _currentRunIndex := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'timeUTC') then
        begin
          _timeUTC := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'recording') then
        begin
          _recording := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'autoStart') then
        begin
          _autoStart := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'beaconDriven') then
        begin
          _beaconDriven := member^.ivalue;
         result := 1;
         exit;
         end;
      if (member^.name = 'usage') then
        begin
          _usage := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'clearHistory') then
        begin
          _clearHistory := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  function TYDataLogger.get_currentRunIndex():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTRUNINDEX_INVALID;
              exit;
            end;
        end;
      res := self._currentRunIndex;
      result := res;
      exit;
    end;


  function TYDataLogger.get_timeUTC():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_TIMEUTC_INVALID;
              exit;
            end;
        end;
      res := self._timeUTC;
      result := res;
      exit;
    end;


  function TYDataLogger.set_timeUTC(newval:int64):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('timeUTC',rest_val);
    end;

  function TYDataLogger.get_recording():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_RECORDING_INVALID;
              exit;
            end;
        end;
      res := self._recording;
      result := res;
      exit;
    end;


  function TYDataLogger.set_recording(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('recording',rest_val);
    end;

  function TYDataLogger.get_autoStart():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_AUTOSTART_INVALID;
              exit;
            end;
        end;
      res := self._autoStart;
      result := res;
      exit;
    end;


  function TYDataLogger.set_autoStart(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('autoStart',rest_val);
    end;

  function TYDataLogger.get_beaconDriven():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_BEACONDRIVEN_INVALID;
              exit;
            end;
        end;
      res := self._beaconDriven;
      result := res;
      exit;
    end;


  function TYDataLogger.set_beaconDriven(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('beaconDriven',rest_val);
    end;

  function TYDataLogger.get_usage():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_USAGE_INVALID;
              exit;
            end;
        end;
      res := self._usage;
      result := res;
      exit;
    end;


  function TYDataLogger.get_clearHistory():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(_yapicontext.GetCacheValidity()) <> YAPI_SUCCESS then
            begin
              result := Y_CLEARHISTORY_INVALID;
              exit;
            end;
        end;
      res := self._clearHistory;
      result := res;
      exit;
    end;


  function TYDataLogger.set_clearHistory(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('clearHistory',rest_val);
    end;

  class function TYDataLogger.FindDataLogger(func: string):TYDataLogger;
    var
      obj : TYDataLogger;
    begin
      obj := TYDataLogger(TYFunction._FindFromCache('DataLogger', func));
      if obj = nil then
        begin
          obj :=  TYDataLogger.create(func);
          TYFunction._AddToCache('DataLogger',  func, obj);
        end;
      result := obj;
      exit;
    end;


  function TYDataLogger.registerValueCallback(callback: TYDataLoggerValueCallback):LongInt;
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
      self._valueCallbackDataLogger := callback;
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


  function TYDataLogger._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackDataLogger) <> nil) then
        begin
          self._valueCallbackDataLogger(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYDataLogger.forgetAllDataStreams():LongInt;
    begin
      result := self.set_clearHistory(Y_CLEARHISTORY_TRUE);
      exit;
    end;


  function TYDataLogger.get_dataSets():TYDataSetArray;
    begin
      result := self.parse_dataSets(self._download('logger.json'));
      exit;
    end;


  function TYDataLogger.parse_dataSets(json: TByteArray):TYDataSetArray;
    var
      dslist : TStringArray;
      dataset : TYDataSet;
      res : TYDataSetArray;
      res_pos : LongInt;
      i_i : LongInt;
    begin
      SetLength(dslist, 0);

      dslist := self._json_get_array(json);
      res_pos := 0;
      SetLength(res, length(dslist));;
      for i_i:=0 to length(dslist)-1 do
        begin
          dataset :=  TYDataSet.create(self);
          dataset._parse(dslist[i_i]);
          res[res_pos] := dataset;
          inc(res_pos);
        end;
      result := res;
      exit;
    end;


  function TYDataLogger.nextDataLogger(): TYDataLogger;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextDataLogger := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextDataLogger := nil;
          exit;
        end;
      nextDataLogger := TYDataLogger.FindDataLogger(hwid);
    end;

  class function TYDataLogger.FirstDataLogger(): TYDataLogger;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('DataLogger', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYDataLogger.FindDataLogger(serial+'.'+funcId);
    end;

//--- (end of generated code: YDataLogger implementation)

//--- (generated code: YDataLogger functions)

  function yFindDataLogger(func:string): TYDataLogger;
    begin
      result := TYDataLogger.FindDataLogger(func);
    end;

  function yFirstDataLogger(): TYDataLogger;
    begin
      result := TYDataLogger.FirstDataLogger();
    end;

  procedure _DataLoggerCleanup();
    begin
    end;

//--- (end of generated code: YDataLogger functions)



  function TYOldDataStream.loadStream():integer;
    type
      longWordArray = array[0..100] of  longWord;
      PlongWordArray = ^longWordArray;
    var
      json            : TJsonParser;
      res             : integer;
      index,offset    : integer;
      count           : integer;
      root,el         : PJSONRECORD;
      name            : string;
      coldiv,coltype  : PlongWordArray;
      data            : array of array of double;
      x,y,i,j,k       : integer;
      c               : ansichar;
      value           : double;
      tmpdata         : array of word;
      colscl          : array of double;
      colofs          : array of integer;
    begin
      data := nil;
      res := _datalogger.getData(_runNo, _timeStamp,  json);
      if (res<>YAPI_SUCCESS) then
        begin
          loadStream:=res;
          exit;
        end;
      _nrows := 0;
      _ncols := 0;
      SetLength(_columnNames,0);
      _values:=nil;
      coldiv := nil;
      coltype :=nil;
      root := json.GetRootNode();
      for i:=0 to root.membercount-1 do
        begin
          el    :=  root.members[i];
          name  := string(el.name);
          if (name='time') then
            _timeStamp := el.ivalue
          else if (name='UTC') then
            _utcStamp  := el.ivalue
          else if (name='interval') then
            _interval  := el.ivalue
          else if (name='nRows') then
            _nRows     := el.ivalue
          else if (name='keys') then
            begin
              _ncols :=  el.itemcount;
              SetLength(_columnNames,_nCols);
              for j:=0 to  _ncols-1 do
                begin
                  _columnNames[j] := string(el.items[j].svalue);
                end;
            end
          else if (name='div') then
            begin
              _ncols :=  el.itemcount;
              getmem( coldiv,sizeof(longWord)*_ncols);
              for j:=0 to  _ncols-1 do
                coldiv^[j]:= el.items[j].ivalue;
            end
          else if (name='type') then
            begin
              _ncols := el.itemcount;
              getmem( coltype,sizeof(longWord)*_ncols);
              for j:=0 to _ncols-1 do
                coltype^[j]:= el.items[j].ivalue;
            end
          else if (name='scal') then
            begin
              _ncols := el.itemcount;
              setLength(colscl,_ncols);
              setLength(colofs,_ncols);
              for j:=0 to _ncols-1 do
                begin
                  colscl[j]:= el.items[j].ivalue / 65536.0;
                  if coltype^[j] <> 0 then
                    colofs[j]:=-32767
                  else
                    colofs[j]:=0;
                end;
            end
          else if (name='data') then
            begin
              if length(colscl) =0 then
                begin
                  setLength(colscl,_ncols);
                  setLength(colofs,_ncols);
                  for j:=0 to _ncols-1 do
                    begin
                      colscl[j]:= 1.0 / coldiv^[j];
                      if coltype^[j] <> 0 then
                        colofs[j]:=-32767
                      else
                        colofs[j]:=0;
                    end;
                end;
              count :=  _nrows *_ncols ;
              setLength(tmpdata,count );
              if (el.recordtype=JSON_STRING) then
                begin
                  k:=0; index:=0;
                  while k<length(el.svalue) do
                    begin
                      if (index>=count)  then
                        begin
                          loadStream:=YAPI_IO_ERROR;
                          exit;
                        end;
                      if (el.svalue[k]>='a') then
                        begin
                          offset := ord(el.svalue[k])-97;
                          if (offset>index-1) then
                            begin
                              loadStream:=YAPI_IO_ERROR;
                              exit;
                            end;
                          tmpdata[index]:=tmpdata[index-offset-1];
                          inc(index);
                          inc(k);
                        end
                      else
                        begin
                          if k+2>=length(el.svalue) then
                            begin
                              loadStream:=YAPI_IO_ERROR;
                              exit;
                            end;
                          c := el.svalue[k+2];
                          if(c = 'z') then c:= '\';
                            tmpdata[index] := (ord(el.svalue[k])-48) + ((ord(el.svalue[k+1])-48)shl 5)  + ((ord(c)-48) shl 10);
                          inc(index);
                          inc(k,3);
                        end;
                    end;
                end
              else
                begin
                  count :=  el.itemcount;
                  if count <> (_nrows * _ncols)  then
                    begin
                      loadStream:=YAPI_IO_ERROR;
                      exit;
                    end;
                  for j:=0 to count-1 do
                    tmpdata[j]:=  el.items[j].ivalue;
                end;
              setLength(_values, _nrows, _ncols);
              x:=0;y:=0;
              for j:=0 to count-1 do
                begin
                  if(coltype[x]<2) then
                    value := (tmpdata[j] + colofs[x]) * colscl[x]
                  else
                    value := _decimalToDouble(tmpdata[j]-32767);
                  _values[y][x] := value;
                  inc(x);
                  if (x>=integer(_ncols)) then
                    begin
                      x:=0;
                      inc(y);
                    end;
                end;
              setLength(tmpdata,0);
            end;
        end;
      json.free();
      freemem(coldiv);
      freemem(coltype);
      loadStream := YAPI_SUCCESS;
    end;

  constructor TYOldDataStream.create(parent:TYDataLogger;run,stamp,utc,itv:longword );
    begin
      _datalogger  := parent;
      _runNo       := run;
      _timestamp   := stamp;
      _utcStamp    := utc;
      _interval    := itv;
      _nrows       := 0;
      _ncols       := 0;
      SetLength(_columnNames, 0);
      _values      := nil;
    end;

  destructor  TYOldDataStream.destroy();
    begin
      _values:=nil;
      inherited destroy();
    end;



  ////
  /// <summary>
  /// Returns the start time of the data stream, relative to the beginning
  /// of the run.
  /// <para>
  /// If you want need an absolute time, use get_startTimeUTC().
  /// </para>
  /// <para>
  /// This method does not cause any access to the device, as the value
  /// is preloaded in the object at instantiation time.
  /// </para>
  /// </summary>
  /// <returns>
  ///   an unsigned number corresponding to the number of seconds
  ///   between the start of the run and the beginning of this data
  ///   stream.
  /// </returns>
  ///-
  function   TYOldDataStream.get_startTime():longInt;
    begin
      get_startTime := _timeStamp;
    end;


  ////
  /// <summary>
  /// Returns the number of seconds elapsed between the every two consecutive
  /// rows of this data stream.
  /// <para>
  /// By default, the data logger records one row
  /// per second, but there might be alternate streams at lower resolution
  /// created for archiving purpose by summarizing the original stream.
  /// </para>
  /// <para>
  /// This method does not cause any access to the device, as the value
  /// is preloaded in the object at instantiation time.
  /// </para>
  /// </summary>
  /// <returns>
  ///  an unsigned number corresponding to a number of seconds.
  /// </returns>
  ///-
  function     TYOldDataStream.get_dataSamplesInterval():double;
    begin
      get_dataSamplesInterval := _interval;
    end;



//--- (generated code: YFunction functions)

  function yFindFunction(func:string): TYFunction;
    begin
      result := TYFunction.FindFunction(func);
    end;

  function yFirstFunction(): TYFunction;
    begin
      result := TYFunction.FirstFunction();
    end;

  procedure _FunctionCleanup();
    begin
    end;

//--- (end of generated code: YFunction functions)



//--- (generated code: YModule functions)

  function yFindModule(func:string): TYModule;
    begin
      result := TYModule.FindModule(func);
    end;

  function yFirstModule(): TYModule;
    begin
      result := TYModule.FirstModule();
    end;

  procedure _ModuleCleanup();
    begin
    end;

//--- (end of generated code: YModule functions)

//--- (generated code: YDataStream functions)

  procedure _DataStreamCleanup();
    begin
    end;

//--- (end of generated code: YDataStream functions)
//--- (generated code: YMeasure functions)

  procedure _MeasureCleanup();
    begin
    end;

//--- (end of generated code: YMeasure functions)
//--- (generated code: YDataSet functions)

  procedure _DataSetCleanup();
    begin
    end;

//--- (end of generated code: YDataSet functions)

  procedure handlersCleanUp();
    begin
      _CalibHandlers.free();
      _CalibHandlers:=nil;
    end;


initialization
  randomize;
  YAPI_apiInitialized     := false;
  YAPI_ExceptionsDisabled := false;


  YDevice_devCache        := Tlist.create();
  //--- (generated code: YModule initialization)
  //--- (end of generated code: YModule initialization)
  //--- (generated code: YFirmwareUpdate initialization)
  //--- (end of generated code: YFirmwareUpdate initialization)
  //--- (generated code: YDataStream initialization)
  //--- (end of generated code: YDataStream initialization)
  //--- (generated code: YDataLogger initialization)
  //--- (end of generated code: YDataLogger initialization)
  //--- (generated code: YMeasure initialization)
  //--- (end of generated code: YMeasure initialization)
  //--- (generated code: YDataSet initialization)
  //--- (end of generated code: YDataSet initialization)
  //--- (generated code: YAPIContext initialization)
  //--- (end of generated code: YAPIContext initialization)

  _cache                := TStringList.create();
  _cache.sorted         := true;
  _FunctionCallbacks    := TList.create();
  _yapiContext          := TYAPIContext.create();
  _TimedReportCallbackList := TList.create();
  _PlugEvents           := TList.create;
  _DataEvents           := TList.create;
  _CalibHandlers        := TStringList.create();
  _CalibHandlers.sorted := true;
  _moduleCallbackList   := TStringList.create();

finalization
  //--- (generated code: YModule cleanup)
  _ModuleCleanup();
  //--- (end of generated code: YModule cleanup)
  //--- (generated code: YFirmwareUpdate cleanup)
  //--- (end of generated code: YFirmwareUpdate cleanup)
  //--- (generated code: YDataStream cleanup)
  //--- (end of generated code: YDataStream cleanup)
  //--- (generated code: YMeasure cleanup)
  //--- (end of generated code: YMeasure cleanup)
  //--- (generated code: YDataSet cleanup)
  //--- (end of generated code: YDataSet cleanup)
  //--- (generated code: YDataLogger cleanup)
  _DataLoggerCleanup();
  //--- (end of generated code: YDataLogger cleanup)
  //--- (generated code: YSensor cleanup)
  _SensorCleanup();
  //--- (end of generated code: YSensor cleanup)
  //--- (generated code: YFunction cleanup)
  _FunctionCleanup();
  //--- (end of generated code: YFunction cleanup)
    //--- (generated code: YAPIContext cleanup)
  //--- (end of generated code: YAPIContext cleanup)

  _FunctionCallbacks.free();
  _TimedReportCallbackList.free();
  devicesCleanUp();
  queuesCleanUp();
  handlersCleanUp();
  _yapiContext.free();

end.

