{*********************************************************************
 *
 * $Id: yocto_api.pas 15331 2014-03-07 15:57:19Z mvuilleu $
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

interface

uses
  sysutils,classes,windows,winsock,Math,
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

  YDEV_DESCR    = s32;           // yStrRef of serial number
  YFUN_DESCR  = s32;           // yStrRef of serial + (ystrRef of funcId << 16)
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
  // Default cache validity (in [ms]) before reloading data from device. This saves a lots of trafic.
  // Note that a value undger 2 ms makes little sense since a USB bus itself has a 2ms roundtrip period
  YAPI_DefaultCacheValidity = 5;

  Y_FUNCTIONDESCRIPTOR_INVALID = -1;


  YAPI_INVALID_STRING     = '!INVALID!';
  YAPI_INVALID_DOUBLE     = -1.79769313486232e308;
  YAPI_INVALID_INT        = longint($07FFFFFFF);
  YAPI_INVALID_UINT       = longint(-1);
  YAPI_INVALID_LONG       = longword($07FFFFFFFFFFFFFFF);

  Y_HARDWAREID_INVALID   =  YAPI_INVALID_STRING;
  Y_FUNCTIONID_INVALID   =  YAPI_INVALID_STRING;
  Y_FRIENDLYNAME_INVALID =  YAPI_INVALID_STRING;

  // fyInitAPI argument
  Y_DETECT_NONE   = 0;
  Y_DETECT_USB    = 1;
  Y_DETECT_NET    = 2;
  Y_DETECT_ALL : integer = (Y_DETECT_USB or Y_DETECT_NET);

  YOCTO_API_VERSION_STR     = '1.10';
  YOCTO_API_VERSION_BCD     = $0110;
  YOCTO_API_BUILD_NO        = '15466';
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
const YAPI_SUCCESS                   = 0;       // everything worked allright
const YAPI_NOT_INITIALIZED           = -1;      // call yInitAPI() first !
const YAPI_INVALID_ARGUMENT          = -2;      // one of the arguments passed to the function is invalid
const YAPI_NOT_SUPPORTED             = -3;      // the operation attempted is (currently) not supported
const YAPI_DEVICE_NOT_FOUND          = -4;      // the requested device is not reachable
const YAPI_VERSION_MISMATCH          = -5;      // the device firmware is incompatible with this API version
const YAPI_DEVICE_BUSY               = -6;      // the device is busy with another task and cannot answer
const YAPI_TIMEOUT                   = -7;      // the device took too long to provide an answer
const YAPI_IO_ERROR                  = -8;      // there was an I/O problem while talking to the device
const YAPI_NO_MORE_DATA              = -9;      // there is no more data to read from
const YAPI_EXHAUSTED                 = -10;     // you have run out of a limited ressource, check the documentation
const YAPI_DOUBLE_ACCES              = -11;     // you have two process that try to acces to the same device
const YAPI_UNAUTHORIZED              = -12;     // unauthorized access to password-protected device
const YAPI_RTC_NOT_READY             = -13;     // real-time clock has not been initialized (or time was lost)

const Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
const Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;


//--- (end of generated code: YFunction definitions)

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
const Y_USBBANDWIDTH_SIMPLE = 0;
const Y_USBBANDWIDTH_DOUBLE = 1;
const Y_USBBANDWIDTH_INVALID = -1;



//--- (end of generated code: YModule definitions)
//--- (generated code: YSensor definitions)

const Y_UNIT_INVALID                  = YAPI_INVALID_STRING;
const Y_CURRENTVALUE_INVALID          = YAPI_INVALID_DOUBLE;
const Y_LOWESTVALUE_INVALID           = YAPI_INVALID_DOUBLE;
const Y_HIGHESTVALUE_INVALID          = YAPI_INVALID_DOUBLE;
const Y_CURRENTRAWVALUE_INVALID       = YAPI_INVALID_DOUBLE;
const Y_LOGFREQUENCY_INVALID          = YAPI_INVALID_STRING;
const Y_REPORTFREQUENCY_INVALID       = YAPI_INVALID_STRING;
const Y_CALIBRATIONPARAM_INVALID      = YAPI_INVALID_STRING;
const Y_RESOLUTION_INVALID            = YAPI_INVALID_DOUBLE;


//--- (end of generated code: YSensor definitions)

//--- (generated code: YDataStream definitions)


//--- (end of generated code: YDataStream definitions)
//--- (generated code: YMeasure definitions)


//--- (end of generated code: YMeasure definitions)
//--- (generated code: YDataSet definitions)


//--- (end of generated code: YDataSet definitions)

type
  TYDevice = class;
  TYFunction = class;
  TYModule = class;
  TYSensor = class;
  TYDataStream = class;
  TYMeasure = class;
  TYDataSet = class;

  TDoubleArrayArray = array of TDoubleArray;
  TYDataStreamArray = array of TYDataStream;
  TYMeasureArray = array of TYMeasure;


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
    function  HTTPRequestPrepare(request: TByteArray; var fullrequest: TByteArray; var errmsg:string):YRETCODE;
    function  HTTPRequestAsync(request: string; var errmsg: string):YRETCODE; overload;
    function  HTTPRequestAsync(request: TByteArray; var errmsg: string):YRETCODE; overload;
    function  HTTPRequest(request :string ; var buffer : string; var   errmsg:string) : YRETCODE; overload;
    function  HTTPRequest(request :string ; var buffer : TByteArray; var   errmsg:string) : YRETCODE; overload;
    function  HTTPRequest(request :TByteArray ; var buffer : TByteArray; var   errmsg:string) : YRETCODE; overload;
    function  requestAPI(var apires:TJsonParser;var errmsg:string):YRETCODE;
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

    function  _upload(path:string; strcontent:string):integer; overload;
    function  _upload(path:string; content:TByteArray):integer; overload;

    function  _download(path:string) :TByteArray;
    function  _request(request: string) :TByteArray; overload;
    function  _request(request: TByteArray) :TByteArray; overload;
    function  _json_get_array(data: TByteArray) :TStringArray;
    function  _json_get_key(data: TByteArray; key: string):string;
    function  _json_get_string(data: TByteArray):string;

    function  _buildSetRequest( changeattr : string ; changeval:string ; var request:string; var errmsg:string):YRETCODE;

  public
    class function _FindFromCache(classname: string; func: string): TYFunction;
    class procedure _AddToCache(classname: string; func: string; obj: TYFunction);
    class procedure _ClearCache();
    class procedure _UpdateValueCallbackList(func : TYFunction; add : boolean);
    class procedure _UpdateTimedReportCallbackList(func : TYFunction; add : boolean);

    Destructor Destroy(); override;

    function  _findDataStream(dataset: TYDataSet; def :string) : TYDataStream;

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
    function get_hardwareId():string;

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
    ///   a number corresponding to the code of the latest error that occured while
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
    ///   to reduce network trafic for instance.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="msValidity">
    ///   an integer corresponding to the validity attributed to the
    ///   loaded function parameters, in milliseconds
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function load(msValidity:integer):YRETCODE;

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
    ///   If the function has never been contacted, the returned value is <c>Y_FUNCTIONDESCRIPTOR_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_logicalName(newval:string):integer;

    ////
    /// <summary>
    ///   Returns the current value of the function (no more than 6 characters).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a string corresponding to the current value of the function (no more than 6 characters)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_advertisedValue():string;

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
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$
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
    function registerValueCallback(callback: TYFunctionValueCallback):LongInt; overload; virtual;

    function _invokeValueCallback(value: string):LongInt; overload; virtual;

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
  TYModuleValueCallback = procedure(func: TYModule; value:string);
  TYModuleTimedReportCallback = procedure(func: TYModule; value:TYMeasure);

  ////
  /// <summary>
  ///   TYModule Class: Module control interface
  /// <para>
  ///   This interface is identical for all Yoctopuce USB modules.
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
    _logicalName              : string;
    _productId                : LongInt;
    _productRelease           : LongInt;
    _firmwareRelease          : string;
    _persistentSettings       : Integer;
    _luminosity               : LongInt;
    _beacon                   : Integer;
    _upTime                   : int64;
    _usbCurrent               : LongInt;
    _rebootCountdown          : LongInt;
    _usbBandwidth             : Integer;
    _valueCallbackModule      : TYModuleValueCallback;
    _logCallback              : TYModuleLogCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YModule declaration)

     // Return the properties of the nth function of our device
     function _getFunction(idx:integer; var serial,funcId,funcName,funcVal,errMsg:string):YRETCODE;

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


    ///
    ///
    ///
    function registerLogCallback(callback : TYModuleLogCallback): integer;

    function get_logCallback(): TYModuleLogCallback;


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
    ///   On failure, throws an exception or returns <c>Y_PRODUCTNAME_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_SERIALNUMBER_INVALID</c>.
    /// </para>
    ///-
    function get_serialNumber():string;

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
    ///   On failure, throws an exception or returns <c>Y_PRODUCTID_INVALID</c>.
    /// </para>
    ///-
    function get_productId():LongInt;

    ////
    /// <summary>
    ///   Returns the hardware release version of the module.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the hardware release version of the module
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PRODUCTRELEASE_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_FIRMWARERELEASE_INVALID</c>.
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
    ///   a value among <c>Y_PERSISTENTSETTINGS_LOADED</c>, <c>Y_PERSISTENTSETTINGS_SAVED</c> and
    ///   <c>Y_PERSISTENTSETTINGS_MODIFIED</c> corresponding to the current state of persistent module settings
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_PERSISTENTSETTINGS_INVALID</c>.
    /// </para>
    ///-
    function get_persistentSettings():Integer;

    function set_persistentSettings(newval:Integer):integer;

    ////
    /// <summary>
    ///   Returns the luminosity of the  module informative leds (from 0 to 100).
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the luminosity of the  module informative leds (from 0 to 100)
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LUMINOSITY_INVALID</c>.
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    ///   either <c>Y_BEACON_OFF</c> or <c>Y_BEACON_ON</c>, according to the state of the localization beacon
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BEACON_INVALID</c>.
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
    ///   either <c>Y_BEACON_OFF</c> or <c>Y_BEACON_ON</c>
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
    ///   On failure, throws an exception or returns <c>Y_UPTIME_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_USBCURRENT_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_REBOOTCOUNTDOWN_INVALID</c>.
    /// </para>
    ///-
    function get_rebootCountdown():LongInt;

    function set_rebootCountdown(newval:LongInt):integer;

    ////
    /// <summary>
    ///   Returns the number of USB interfaces used by the module.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   either <c>Y_USBBANDWIDTH_SIMPLE</c> or <c>Y_USBBANDWIDTH_DOUBLE</c>, according to the number of USB
    ///   interfaces used by the module
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_USBBANDWIDTH_INVALID</c>.
    /// </para>
    ///-
    function get_usbBandwidth():Integer;

    ////
    /// <summary>
    ///   Changes the number of USB interfaces used by the module.
    /// <para>
    ///   You must reboot the module
    ///   after changing this setting.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>Y_USBBANDWIDTH_SIMPLE</c> or <c>Y_USBBANDWIDTH_DOUBLE</c>, according to the number of USB
    ///   interfaces used by the module
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
    function set_usbBandwidth(newval:Integer):integer;

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
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$
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
    function registerValueCallback(callback: TYModuleValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    ////
    /// <summary>
    ///   Saves current settings in the nonvolatile memory of the module.
    /// <para>
    ///   Warning: the number of allowed save operations during a module life is
    ///   limited (about 100000 cycles). Do not call this function within a loop.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
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
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
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
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
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
    ///   <c>YAPI_SUCCESS</c> when the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function triggerFirmwareUpdate(secBeforeReboot: LongInt):LongInt; overload; virtual;

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
    ///   On failure, throws an exception or returns an empty content.
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
    /// </returns>
    ///-
    function get_lastLogs():string; overload; virtual;


    ////
    /// <summary>
    ///   Continues the module enumeration started using <c>yFirstModule()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YModule</c> object, corresponding to
    ///   the next module found, or a <c>null</c> pointer
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
  ///   TYSensor Class: Sensor function interface
  /// <para>
  ///   The Yoctopuce application programming interface allows you to read an instant
  ///   measure of the sensor, as well as the minimal and maximal values observed.
  /// </para>
  /// </summary>
  ///-
  TYSensor=class(TYFunction)
  //--- (end of generated code: YSensor class start)  
  protected
  //--- (generated code: YSensor declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _unit                     : string;
    _currentValue             : double;
    _lowestValue              : double;
    _highestValue             : double;
    _currentRawValue          : double;
    _logFrequency             : string;
    _reportFrequency          : string;
    _calibrationParam         : string;
    _resolution               : double;
    _valueCallbackSensor      : TYSensorValueCallback;
    _timedReportCallbackSensor : TYSensorTimedReportCallback;
    _prevTimedReport          : double;
    _iresol                   : double;
    _offset                   : double;
    _scale                    : double;
    _decexp                   : double;
    _isScal                   : boolean;
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
    ///   On failure, throws an exception or returns <c>Y_UNIT_INVALID</c>.
    /// </para>
    ///-
    function get_unit():string;

    ////
    /// <summary>
    ///   Returns the current value of the measure.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the current value of the measure
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CURRENTVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_currentValue():double;

    ////
    /// <summary>
    ///   Changes the recorded minimal value observed.
    /// <para>
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the minimal value observed for the measure since the device was started
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_LOWESTVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_lowestValue():double;

    ////
    /// <summary>
    ///   Changes the recorded maximal value observed.
    /// <para>
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the maximal value observed for the measure since the device was started
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_HIGHESTVALUE_INVALID</c>.
    /// </para>
    ///-
    function get_highestValue():double;

    ////
    /// <summary>
    ///   Returns the uncalibrated, unrounded raw value returned by the sensor.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the uncalibrated, unrounded raw value returned by the sensor
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_CURRENTRAWVALUE_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_LOGFREQUENCY_INVALID</c>.
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
    ///   the value "OFF".
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    ///   On failure, throws an exception or returns <c>Y_REPORTFREQUENCY_INVALID</c>.
    /// </para>
    ///-
    function get_reportFrequency():string;

    ////
    /// <summary>
    ///   Changes the timed value notification frequency for this function.
    /// <para>
    ///   The frequency can be specified as samples per second,
    ///   as sample per minute (for instance "15/m") or in samples per
    ///   hour (eg. "4/h"). To disable timed value notifications for this
    ///   function, use the value "OFF".
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function set_reportFrequency(newval:string):integer;

    function get_calibrationParam():string;

    function set_calibrationParam(newval:string):integer;

    ////
    /// <summary>
    ///   Changes the resolution of the measured physical values.
    /// <para>
    ///   The resolution corresponds to the numerical precision
    ///   when displaying value. It does not change the precision of the measure itself.
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a floating point number corresponding to the resolution of the measured values
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_RESOLUTION_INVALID</c>.
    /// </para>
    ///-
    function get_resolution():double;

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
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$
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
    function registerValueCallback(callback: TYSensorValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function _parserHelper():LongInt; override;

    ////
    /// <summary>
    ///   Retrieves a DataSet object holding historical data for this
    ///   sensor, for a specified time interval.
    /// <para>
    ///   The measures will be
    ///   retrieved from the data logger, which must have been turned
    ///   on at the desired time. See the documentation of the DataSet
    ///   class for information on how to get an overview of the
    ///   recorded data, and how to load progressively a large set
    ///   of measures from the data logger.
    /// </para>
    /// <para>
    ///   This function only works if the device uses a recent firmware,
    ///   as DataSet objects are not supported by firmwares older than
    ///   version 13000.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="startTime">
    ///   the start of the desired measure time interval,
    ///   as a Unix timestamp, i.e. the number of seconds since
    ///   January 1, 1970 UTC. The special value 0 can be used
    ///   to include any meaasure, without initial limit.
    /// </param>
    /// <param name="endTime">
    ///   the end of the desired measure time interval,
    ///   as a Unix timestamp, i.e. the number of seconds since
    ///   January 1, 1970 UTC. The special value 0 can be used
    ///   to include any meaasure, without ending limit.
    /// </param>
    /// <returns>
    ///   an instance of YDataSet, providing access to historical
    ///   data. Past measures can be loaded progressively
    ///   using methods from the YDataSet object.
    /// </returns>
    ///-
    function get_recordedData(startTime: int64; endTime: int64):TYDataSet; overload; virtual;

    ////
    /// <summary>
    ///   Registers the callback function that is invoked on every periodic timed notification.
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
    ///   arguments: the function object of which the value has changed, and an YMeasure object describing
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function loadCalibrationPoints(var rawValues: TDoubleArray; var refValues: TDoubleArray):LongInt; overload; virtual;

    function _encodeCalibrationPoints(rawValues: TDoubleArray; refValues: TDoubleArray):string; overload; virtual;

    function _applyCalibration(rawValue: double):double; overload; virtual;

    function _decodeTimedReport(timestamp: double; report: TLongIntArray):TYMeasure; overload; virtual;

    function _decodeVal(w: LongInt):double; overload; virtual;

    function _decodeAvg(dw: LongInt):double; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of sensors started using <c>yFirstSensor()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YSensor</c> object, corresponding to
    ///   a sensor currently online, or a <c>null</c> pointer
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


  //--- (generated code: YDataStream class start)
  ////
  /// <summary>
  ///   TYDataStream Class: Unformatted data sequence
  /// <para>
  ///   YDataStream objects represent bare recorded measure sequences,
  ///   exactly as found within the data logger present on Yoctopuce
  ///   sensors.
  /// </para>
  /// <para>
  ///   In most cases, it is not necessary to use YDataStream objects
  ///   directly, as the YDataSet objects (returned by the
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
    _duration                 : LongInt;
    _columnNames              : TStringArray;
    _functionId               : string;
    _isClosed                 : boolean;
    _isAvg                    : boolean;
    _isScal                   : boolean;
    _decimals                 : LongInt;
    _offset                   : double;
    _scale                    : double;
    _samplesPerHour           : LongInt;
    _minVal                   : double;
    _avgVal                   : double;
    _maxVal                   : double;
    _decexp                   : double;
    _caltyp                   : LongInt;
    _calpar                   : TLongIntArray;
    _calraw                   : TDoubleArray;
    _calref                   : TDoubleArray;
    _values                   : TDoubleArrayArray;

    //--- (end of generated code: YDataStream declaration)
    _calhdl                  : yCalibrationHandler;
  public
    constructor Create(parent:TYFunction); Overload;
    constructor Create(parent:TYFunction; dataset:TYDataSet; encoded:TLongIntArray); Overload;

  
  //--- (generated code: YDataStream accessors declaration)
    function _initFromDataSet(dataset: TYDataSet; encoded: TLongIntArray):LongInt; overload; virtual;

    function parse(sdata: TByteArray):LongInt; overload; virtual;

    function get_url():string; overload; virtual;

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
    function get_startTime():LongInt; overload; virtual;

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
    ///   an unsigned number corresponding to the number of seconds
    ///   between the Jan 1, 1970 and the beginning of this data
    ///   stream (i.e. Unix time representation of the absolute time).
    /// </returns>
    ///-
    function get_startTimeUTC():int64; overload; virtual;

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

    ////
    /// <summary>
    ///   Returns the approximate duration of this stream, in seconds.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   the number of seconds covered by this stream.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns Y_DURATION_INVALID.
    /// </para>
    ///-
    function get_duration():LongInt; overload; virtual;

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
  ///   TYMeasure Class: Measured value
  /// <para>
  ///   YMeasure objects are used within the API to represent
  ///   a value measured at a specified time. These objects are
  ///   used in particular in conjunction with the YDataSet class.
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
    ///   an floating point number corresponding to the number of seconds
    ///   between the Jan 1, 1970 UTC and the beginning of this measure.
    /// </returns>
    ///-
    function get_startTimeUTC():double; overload; virtual;

    ////
    /// <summary>
    ///   Returns the end time of the measure, relative to the Jan 1, 1970 UTC
    ///   (Unix timestamp).
    /// <para>
    ///   When the recording rate is higher then 1 sample
    ///   per second, the timestamp may have a fractional part.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an floating point number corresponding to the number of seconds
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
  ///   TYDataSet Class: Recorded data sequence
  /// <para>
  ///   YDataSet objects make it possible to retrieve a set of recorded measures
  ///   for a given sensor and a specified time interval. They can be used
  ///   to load data points with a progress report. When the YDataSet object is
  ///   instanciated by the <c>get_recordedData()</c>  function, no data is
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
  ///   This class can only be used on devices that use a recent firmware,
  ///   as YDataSet objects are not supported by firmwares older than version 13000.
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
    _startTime                : int64;
    _endTime                  : int64;
    _progress                 : LongInt;
    _calib                    : TLongIntArray;
    _streams                  : TYDataStreamArray;
    _summary                  : TYMeasure;
    _preview                  : TYMeasureArray;
    _measures                 : TYMeasureArray;

    //--- (end of generated code: YDataSet declaration)

    function _parse(data:string):integer;

  public

    constructor Create(parent:TYFunction; functionId,func_unit:string; startTime,endTime: LongWord); Overload;

    constructor Create(parent:TYFunction; data:string); Overload;

    destructor Destroy();override;

  //--- (generated code: YDataSet accessors declaration)
    function get_calibration():TLongIntArray; overload; virtual;

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
    ///   On failure, throws an exception or returns  <c>Y_HARDWAREID_INVALID</c>.
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
    ///   On failure, throws an exception or returns  <c>Y_UNIT_INVALID</c>.
    /// </para>
    ///-
    function get_unit():string; overload; virtual;

    ////
    /// <summary>
    ///   Returns the start time of the dataset, relative to the Jan 1, 1970.
    /// <para>
    ///   When the YDataSet is created, the start time is the value passed
    ///   in parameter to the <c>get_dataSet()</c> function. After the
    ///   very first call to <c>loadMore()</c>, the start time is updated
    ///   to reflect the timestamp of the first measure actually found in the
    ///   dataLogger within the specified range.
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

    ////
    /// <summary>
    ///   Returns the end time of the dataset, relative to the Jan 1, 1970.
    /// <para>
    ///   When the YDataSet is created, the end time is the value passed
    ///   in parameter to the <c>get_dataSet()</c> function. After the
    ///   very first call to <c>loadMore()</c>, the end time is updated
    ///   to reflect the timestamp of the last measure actually found in the
    ///   dataLogger within the specified range.
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

    ////
    /// <summary>
    ///   Returns the progress of the downloads of the measures from the data logger,
    ///   on a scale from 0 to 100.
    /// <para>
    ///   When the object is instanciated by <c>get_dataSet</c>,
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
    ///   Loads the the next block of measures from the dataLogger, and updates
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
    ///   Returns an YMeasure object which summarizes the whole
    ///   DataSet.
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
    ///   an YMeasure object
    /// </returns>
    ///-
    function get_summary():TYMeasure; overload; virtual;

    ////
    /// <summary>
    ///   Returns a condensed version of the measures that can
    ///   retrieved in this YDataSet, as a list of YMeasure
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
    ///   Returns all measured values currently available for this DataSet,
    ///   as a list of YMeasure objects.
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
  ///   When <c>Y_DETECT_NONE</c> is used as detection <c>mode</c>,
  ///   you must explicitly use <c>yRegisterHub()</c> to point the API to the
  ///   VirtualHub on which your devices are connected before trying to access them.
  /// </para>
  /// </summary>
  /// <param name="mode">
  ///   an integer corresponding to the type of automatic
  ///   device detection to use. Possible values are
  ///   <c>Y_DETECT_NONE</c>, <c>Y_DETECT_USB</c>, <c>Y_DETECT_NET</c>,
  ///   and <c>Y_DETECT_ALL</c>.
  /// </param>
  /// <param name="errmsg">
  ///   a string passed by reference to receive any error message.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function yInitAPI(mode: integer; var errmsg:string ):integer;

  ////
  /// <summary>
  ///   Frees dynamically allocated memory blocks used by the Yoctopuce library.
  /// <para>
  ///   It is generally not required to call this function, unless you
  ///   want to free all dynamically allocated memory blocks in order to
  ///   track a memory leak for instance.
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
  ///   a procedure taking a string parameter, or <c>null</c>
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
  ///   a procedure taking a <c>YModule</c> parameter, or <c>null</c>
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
  ///   a procedure taking a <c>YModule</c> parameter, or <c>null</c>
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
  ///   The
  ///   parameter will determine how the API will work. Use the following values:
  /// </para>
  /// <para>
  ///   <b>usb</b>: When the <c>usb</c> keyword is used, the API will work with
  ///   devices connected directly to the USB bus. Some programming languages such a Javascript,
  ///   PHP, and Java don't provide direct access to USB hardware, so <c>usb</c> will
  ///   not work with these. In this case, use a VirtualHub or a networked YoctoHub (see below).
  /// </para>
  /// <para>
  ///   <b><i>x.x.x.x</i></b> or <b><i>hostname</i></b>: The API will use the devices connected to the
  ///   host with the given IP address or hostname. That host can be a regular computer
  ///   running a VirtualHub, or a networked YoctoHub such as YoctoHub-Ethernet or
  ///   YoctoHub-Wireless. If you want to use the VirtualHub running on you local
  ///   computer, use the IP address 127.0.0.1.
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
  ///   <c>http://username:password@adresse:port</c>
  /// </para>
  /// <para>
  ///   You can call <i>RegisterHub</i> several times to connect to several machines.
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
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function yRegisterHub(url:string;var errmsg:string):integer;

  ////
  /// <summary>
  ///   Fault-tolerant alternative to RegisterHub().
  /// <para>
  ///   This function has the same
  ///   purpose and same arguments as <c>RegisterHub()</c>, but does not trigger
  ///   an error when the selected hub is not available at the time of the function call.
  ///   This makes it possible to register a network hub independently of the current
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
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
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
  ///   Triggers a (re)detection of connected Yoctopuce modules.
  /// <para>
  ///   The library searches the machines or USB ports previously registered using
  ///   <c>yRegisterHub()</c>, and invokes any user-defined callback function
  ///   in case a change in the list of connected devices is detected.
  /// </para>
  /// <para>
  ///   This function can be called as frequently as desired to refresh the device list
  ///   and to make the application aware of hot-plug events.
  /// </para>
  /// </summary>
  /// <param name="errmsg">
  ///   a string passed by reference to receive any error message.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
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
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
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
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
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
  ///   a procedure taking two string parameter, or null
  ///   to unregister a previously registered  callback.
  /// </param>
  ///-
  procedure yRegisterHubDiscoveryCallback(hubDiscoveryCallback: YHubDiscoveryCallback);

  ////
  /// <summary>
  ///   Force a hub discovery, if a callback as been registered with <c>yRegisterDeviceRemovalCallback</c> it
  ///   will be called for each net work hub that will respond to the discovery.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="errmsg">
  ///   a string passed by reference to receive any error message.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  ///   On failure, throws an exception or returns a negative error code.
  /// </returns>
  ///-
  function yTriggerHubDiscovery(var errmsg:string):integer;

  //--- (generated code: Function functions declaration)

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
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the function
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

//--- (end of generated code: Function functions declaration)

  //--- (generated code: Module functions declaration)

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
  ///   the first module currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstModule():TYModule;

//--- (end of generated code: Module functions declaration)

//--- (generated code: Sensor functions declaration)

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
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the sensor
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
  ///   the first sensor currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstSensor():TYSensor;

//--- (end of generated code: Sensor functions declaration)
//--- (generated code: DataStream functions declaration)


//--- (end of generated code: DataStream functions declaration)
//--- (generated code: Measure functions declaration)


//--- (end of generated code: Measure functions declaration)
//--- (generated code: DataSet functions declaration)


//--- (end of generated code: DataSet functions declaration)


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
  function yapiGetAllDevices(dbuffer:PyHandleArray;maxsize:integer;var neededsize:integer; var errmsg:string):integer;
  function yapiGetDeviceInfo(d:YDEV_DESCR;var infos:yDeviceSt;var errmsg:string):integer;
  function yapiGetFunction(class_str,function_str:string;var errmsg:string):YFUN_DESCR;
  function yapiGetFunctionsByClass( class_str:string; precFuncDesc:YFUN_DESCR;dbuffer:PyHandleArray;maxsize:integer;var neededsize:integer; var errmsg:string):integer;
  function yapiGetFunctionsByDevice( devdesc:YDEV_DESCR; precFuncDesc:YFUN_DESCR;dbuffer:PyHandleArray;maxsize:integer;var neededsize:integer; var errmsg:string):integer;
  function yapiGetFunctionInfo(fundesc:YFUN_DESCR;var devdesc:YDEV_DESCR;var serial,funcId,funcName,funcVal,errmsg : string):integer;
  function yapiGetDeviceByFunction(fundesc:YFUN_DESCR;var errmsg : string):integer;
  function yapiHTTPRequest(device:string; request:string; var answer:string; var errmsg:string):integer;
  function yapiGetDevicePath(devdesc: integer; var rootdevice :string; var path : string ; var errmsg:String ) :integer;

  function  YISERR(retcode:YRETCODE):boolean;
  function  yapiFlashDevice(args:TyFlashArg; var errmsg : string):integer;
  function  yapiVerifyDevice(args:TyFlashArg;var errmsg : string):integer;

  function _getCalibrationHandler(calType:integer):yCalibrationHandler;
  function _StrToByte(value: string): TByteArray;
  function _ByteToString(value: TByteArray): string;
  function _decimalToDouble(val:u64):double;
  function _doubleToDecimal(val:double):u64;
  function _decodeWords(sdat:string):TLongIntArray;
implementation

var
  _cache             : TStringList;
  _FunctionCallbacks : Tlist;
  _TimedReportCallbackList : Tlist;
  _CalibHandlers     : TStringList;

  constructor  YAPI_Exception.Create(errType:YRETCODE;  errMsg:string );
    begin
      inherited create(errMsg);
    end;

type
  _yapiLogFunc            = procedure (log:pansichar; loglen:u32);cdecl;
  _yapiDeviceUpdateFunc   = procedure (dev:YDEV_DESCR);cdecl;
  _yapiFunctionUpdateFunc = procedure (func:YFUN_DESCR; value:pansichar);cdecl;
  _yapiTimedReportFunc    = procedure (func:YFUN_DESCR; timestamp:double; bytes:pansichar; len:integer);cdecl;
  _yapiHubDiscoveryCallback = procedure (serial:pansichar; url:pansichar);cdecl;
  _yapiDeviceLogCallback  = procedure (dev:YDEV_DESCR; line:pansichar);cdecl;

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


  {$Ifdef BUILTIN_YAPI}

  function  _yapiInitAPI(mode:integer; errmsg : pansichar):integer;cdecl; external;
  procedure _yapiFreeAPI();cdecl;  external ;
  procedure _yapiRegisterLogFunction(fct:_yLogFunc); cdecl; external ;
  procedure _yapiRegisterDeviceLogCallback(fct:_yapiDeviceLogCallback); cdecl; external ;
  procedure _yapiRegisterDeviceArrivalCallback(fct:_yDeviceUpdateFunc); cdecl; external ;
  procedure _yapiRegisterDeviceRemovalCallback(fct:_yDeviceUpdateFunc); cdecl; external;
  procedure _yapiRegisterDeviceChangeCallback(fct:_yDeviceUpdateFunc); cdecl; external;
  function  _yapiRegisterHub(rootUrl:pansichar; errmsg:pansichar):integer;cdecl;external;
  function  _yapiPreregisterHub(rootUrl:pansichar; errmsg:pansichar):integer;cdecl;external;
  procedure _yapiUnregisterHub(rootUrl:pansichar);cdecl;external;
  function  _yapiUpdateDeviceList(force:integer; errmsg : pansichar):integer;cdecl;  external;
  function  _yapiHandleEvents(mode:integer; errmsg : pansichar):integer;cdecl; external;
  function  _yapiGetTickCount():u64; external;
  function  _yapiCheckLogicalName(name:pansichar):integer;cdecl;external;
  function  _yapiGetAPIVersion(var version:pansichar;var build_date:pansichar):u16;cdecl; external;
  procedure _yapiSetTraceFile(tracefile:pansichar);cdecl;external;
  function  _yapiGetDevice(device_str:pansichar;errmsg:pansichar):YDEV_DESCR; cdecl; external ;
  function  _yapiGetAllDevices( buffer:PyHandleArray;maxsize:integer;var neededsize:integer;errmsg : pansichar):integer; cdecl; external ;
  function  _yapiGetDeviceInfo(d:YDEV_DESCR;var infos:yDeviceSt;errmsg : pansichar):integer;  cdecl; external ;
  function  _yapiGetFunction(class_str,function_str:pansichar;errmsg : pansichar ):YFUN_DESCR; cdecl; external ;
  function  _yapiGetFunctionsByClass( class_str:pansichar; precFuncDesc:YFUN_DESCR; buffer:PyHandleArray;maxsize:integer;var neededsize:integer;errmsg : pansichar):integer; cdecl;  external ;
  function  _yapiGetFunctionsByDevice( device:YDEV_DESCR; precFuncDesc:YFUN_DESCR; buffer:PyHandleArray;maxsize:integer;var neededsize:integer;errmsg : pansichar):integer; cdecl;  external;
  function  _yapiGetFunctionInfo(fundesc:YFUN_DESCR;var devdesc:YDEV_DESCR;serial,funcId,funcName,funcVal,errmsg : pansichar):integer;  cdecl; external;
  function  _yapiHTTPRequestSyncStart(iohdl:PYIOHDL;device:pansichar;url:pansichar; var reply:pansichar; var replysize:integer; errmsg : pansichar):integer;cdecl;external ;
  function  _yapiHTTPRequestSyncStartEx(iohdl:PYIOHDL;device:pansichar;url:pansichar;urllen:integer; var reply:pansichar; var replysize:integer; errmsg : pansichar):integer;cdecl;external ;

  function  _yapiHTTPRequestSyncDone(iohdl:PYIOHDL;errmsg : pansichar):integer;cdecl;external ;
  function  _yapiHTTPRequestAsync(device:pansichar;url:pansichar; callback:pointer; context:pointer; errmsg : pansichar):integer;cdecl;external ;
  function  _yapiHTTPRequest(device:pansichar;url:pansichar; buffer:pansichar;buffsize:integer;var fullsize:integer;errmsg : pansichar):integer;cdecl;external ;
  function  _yapiGetBootloadersDevs(serials:pansichar; maxNbSerial:u32; var totalBootladers:u32; errmsg:pansichar) :integer;cdecl; external ;
  function  _yapiFlashDevice(args:PyFlashArg;errmsg : pansichar):integer;cdecl; external ;
  function  _yapiVerifyDevice(args:PyFlashArg;errmsg : pansichar):integer;cdecl; external ;
  function  _yapiGetDevicePath(devdesc :integer ; rootdevice :pansichar; path:pansichar; pathsize:integer; var neededsize:integer; errmsg:pansichar);cdecl;external ;
  function  _yapiSleep(duration_ms: integer; errmsg:pansichar):integer; cdecl; external;

  function  getaddrinfo(pNodeName,pServiceName :  pansichar; pHints, ppResult :pointer):u16;cdecl; external  'ws2_32.dll' name 'getaddrinfo';
  procedure _freeaddrinfo( ai:pointer);  cdecl; external 'ws2_32.dll' name 'freeaddrinfo';
  function  _wsprintfA( lpOut,lpFmt:pointer):integer;  cdecl; external 'user32.dll' name 'wsprintfA';

  {$else}

const
  {$ifdef ENABLEPROGRAMMING}
  dllfile = 'yprogrammer.dll';
  {$else}
  dllfile = 'yapi.dll';
  {$endif}


  function  _yapiInitAPI(mode:integer; errmsg : pansichar):integer;cdecl; external dllfile name 'yapiInitAPI';
  procedure _yapiFreeAPI();cdecl;  external dllfile name 'yapiFreeAPI';
  procedure _yapiRegisterLogFunction(fct:_yapiLogFunc); cdecl; external dllfile name 'yapiRegisterLogFunction';
  procedure _yapiRegisterDeviceLogCallback(fct:_yapiDeviceLogCallback); cdecl; external dllfile name 'yapiRegisterDeviceLogCallback';
  procedure _yapiStartStopDeviceLogCallback(serial:pansichar; startstop:integer); cdecl; external dllfile name 'yapiStartStopDeviceLogCallback';
  procedure _yapiRegisterDeviceArrivalCallback(fct:_yapiDeviceUpdateFunc); cdecl; external dllfile name 'yapiRegisterDeviceArrivalCallback';
  procedure _yapiRegisterDeviceRemovalCallback(fct:_yapiDeviceUpdateFunc); cdecl; external dllfile name 'yapiRegisterDeviceRemovalCallback';
  procedure _yapiRegisterDeviceChangeCallback(fct:_yapiDeviceUpdateFunc); cdecl; external dllfile name 'yapiRegisterDeviceChangeCallback';
  procedure _yapiRegisterFunctionUpdateCallback(fct:_yapiFunctionUpdateFunc); cdecl; external dllfile name 'yapiRegisterFunctionUpdateCallback';
  procedure _yapiRegisterTimedReportCallback(fct:_yapiTimedReportFunc); cdecl; external dllfile name 'yapiRegisterTimedReportCallback';
  function  _yapiLockDeviceCallBack(errmsg:pansichar):integer;  cdecl; external dllfile name 'yapiLockDeviceCallBack';
  function  _yapiUnlockDeviceCallBack(errmsg:pansichar):integer;  cdecl; external dllfile name 'yapiUnlockDeviceCallBack';
  function  _yapiLockFunctionCallBack(errmsg:pansichar):integer;  cdecl; external dllfile name 'yapiLockFunctionCallBack';
  function  _yapiUnlockFunctionCallBack(errmsg:pansichar):integer;  cdecl; external dllfile name 'yapiUnlockFunctionCallBack';
  function  _yapiRegisterHub(rootUrl:pansichar;errmsg:pansichar):integer;cdecl;external dllfile name 'yapiRegisterHub';
  function  _yapiPreregisterHub(rootUrl:pansichar;errmsg:pansichar):integer;cdecl;external dllfile name 'yapiPreregisterHub';
  procedure _yapiUnregisterHub(rootUrl:pansichar);cdecl;external dllfile name 'yapiUnregisterHub';
  function  _yapiUpdateDeviceList(force:integer;errmsg : pansichar):integer;cdecl;  external dllfile name 'yapiUpdateDeviceList';
  function  _yapiHandleEvents(errmsg : pansichar):integer;cdecl; external dllfile name 'yapiHandleEvents';
  function  _yapiGetTickCount():u64; external dllfile name 'yapiGetTickCount';
  function  _yapiCheckLogicalName(name:pansichar):integer;cdecl;external dllfile name 'yapiCheckLogicalName';
  function  _yapiGetAPIVersion(var version:pansichar;var build_date:pansichar):u16;cdecl; external   dllfile  name 'yapiGetAPIVersion';
  procedure _yapiSetTraceFile(tracefile:pansichar);cdecl; external dllfile name 'yapiSetTraceFile';
  function  _yapiGetDevice(device_str:pansichar;errmsg : pansichar ):YDEV_DESCR; cdecl; external dllfile name 'yapiGetDevice';
  function  _yapiGetAllDevices( buffer:PyHandleArray;maxsize:integer;var neededsize:integer;errmsg : pansichar):integer; cdecl;  external dllfile name 'yapiGetAllDevices';
  function  _yapiGetDeviceInfo(d:YDEV_DESCR;var infos:yDeviceSt;errmsg : pansichar):integer;  cdecl; external dllfile name 'yapiGetDeviceInfo';
  function  _yapiGetFunction(class_str,function_str:pansichar;errmsg : pansichar ):YFUN_DESCR; cdecl; external dllfile name 'yapiGetFunction';
  function  _yapiGetFunctionsByClass( class_str:pansichar; precFuncDesc:YFUN_DESCR; buffer:PyHandleArray;maxsize:integer;var neededsize:integer;errmsg : pansichar):integer; cdecl;  external dllfile name 'yapiGetFunctionsByClass';
  function  _yapiGetFunctionsByDevice( device:YDEV_DESCR; precFuncDesc:YFUN_DESCR; buffer:PyHandleArray;maxsize:integer;var neededsize:integer;errmsg : pansichar):integer; cdecl;  external dllfile name 'yapiGetFunctionsByDevice';
  function  _yapiGetFunctionInfo(fundesc:YFUN_DESCR;var devdesc:YDEV_DESCR;serial,funcId,funcName,funcVal,errmsg : pansichar):integer;  cdecl; external dllfile name 'yapiGetFunctionInfo';
  function  _yapiGetErrorString(errorcode:integer;buffer:pansichar; maxsize:integer;errmsg : pansichar):integer;  cdecl; external dllfile name 'yapiGetErrorString';
  function  _yapiHTTPRequestSyncStart(iohdl:PYIOHDL;device:pansichar;url:pansichar; var reply:pansichar; var replysize:integer; errmsg : pansichar):integer;cdecl;external dllfile name 'yapiHTTPRequestSyncStart';
  function  _yapiHTTPRequestSyncStartEx(iohdl:PYIOHDL;device:pansichar;url:pansichar;urllen:integer; var reply:pansichar; var replysize:integer; errmsg : pansichar):integer;cdecl;external dllfile name 'yapiHTTPRequestSyncStartEx';
  function  _yapiHTTPRequestSyncDone(iohdl:PYIOHDL;errmsg : pansichar):integer;cdecl;external dllfile name 'yapiHTTPRequestSyncDone';
  function  _yapiHTTPRequestAsync(device:pansichar;url:pansichar; callback:pointer; context:pointer; errmsg : pansichar):integer;cdecl;external dllfile name 'yapiHTTPRequestAsync';
  function  _yapiHTTPRequest(device:pansichar;url:pansichar; buffer:pansichar;buffsize:integer;var fullsize:integer;errmsg : pansichar):integer;cdecl;external dllfile name 'yapiHTTPRequest';
  function  _yapiGetBootloadersDevs(serials:pansichar; maxNbSerial:u32; var totalBootladers:u32; errmsg:pansichar) :integer;cdecl; external dllfile name 'yapiGetBootloadersDevs';
  function  _yapiFlashDevice(args:PyFlashArg;errmsg : pansichar):integer;cdecl; external dllfile name 'yapiFlashDevice';
  function  _yapiVerifyDevice(args:PyFlashArg;errmsg : pansichar):integer;cdecl; external dllfile name 'yapiVerifyDevice';
  function  _yapiGetDevicePath(devdesc :integer ; rootdevice :pansichar; path:pansichar; pathsize:integer; var neededsize:integer; errmsg:pansichar):integer;cdecl; external dllfile name 'yapiGetDevicePath';
  function  _yapiSleep(duration_ms: integer; errmsg:pansichar):integer; cdecl; external dllfile name 'yapiSleep';
  procedure _yapiRegisterHubDiscoveryCallback(fct:_yapiHubDiscoveryCallback); cdecl; external dllfile name 'yapiRegisterHubDiscoveryCallback';
  function  _yapiTriggerHubDiscovery(errmsg:pansichar):integer; cdecl; external dllfile name 'yapiTriggerHubDiscovery';

  {$endif}


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
                    YAPI_FUN_UPDATE,YAPI_FUN_VALUE,YAPI_FUN_TIMEDREPORT,YAPI_HUB_DISCOVERY);

  TyapiEvent = record
    eventtype: TyapiEventType;
    module   : TYmodule;
    fun_descr: YFUN_DESCR;
    value    : string[YOCTO_PUBVAL_LEN];
    timestamp: double;
    data     : array[0..15] of byte;
    data_len : integer;
    serial   : string[YOCTO_SERIAL_LEN];
    url      : string[64];
  end;
  PyapiEvent = ^TyapiEvent;

var
  _PlugEvents : Tlist;
  _DataEvents : Tlist;


  procedure native_yDeviceArrivalCallback(d:YDEV_DESCR);cdecl;
    var
      infos  :  yDeviceSt ;
      event  : PyapiEvent;
      errmsg : string;
    begin
      getmem(event,sizeof(TyapiEvent));
      event^.eventtype    := YAPI_DEV_ARRIVAL;
      if(yapiGetDeviceInfo(d, infos, errmsg) <> YAPI_SUCCESS) then exit;
      event^.module := yFindModule(string(infos.serial)+'.module');
      event^.module.setImmutableAttributes(infos);
      if(assigned(yArrival)) then _PlugEvents.add(event)
      else freemem(event);
    end;


  procedure yRegisterDeviceArrivalCallback(arrivalCallback: yDeviceUpdateFunc);
    var
      m:tymodule;
      errmsg:string;
    begin
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
      if(not assigned(yRemoval)) then exit;
      getmem(event,sizeof(TyapiEvent));
      event^.eventtype    := YAPI_DEV_REMOVAL;
      if(yapiGetDeviceInfo(d, infos, errmsg) <> YAPI_SUCCESS) then exit;
      event^.module := yFindModule(string(infos.serial)+'.module');
      if(assigned(yRemoval)) then _PlugEvents.add(event)
      else freemem(event);
    end;


  procedure yRegisterDeviceRemovalCallback(removalCallback: yDeviceUpdateFunc);
    begin
      yRemoval :=removalCallback;
      if assigned(yRemoval) then
        _yapiRegisterDeviceRemovalCallback(native_yDeviceRemovalCallback)
      else
        _yapiRegisterDeviceRemovalCallback(nil);
    end;


  procedure native_yDeviceChangeCallback (d:YDEV_DESCR);cdecl;
    var
      infos  : yDeviceSt;
      errmsg : string;
      event  : PyapiEvent;
    begin
      if(not assigned(yChange)) then exit;
      getmem(event,sizeof(TyapiEvent));
      event^.eventtype    := YAPI_DEV_CHANGE;
      if(yapiGetDeviceInfo(d, infos, errmsg) <> YAPI_SUCCESS) then exit;
      event^.module := yFindModule(string(infos.serial)+'.module');
      if(assigned(yChange)) then   _PlugEvents.add(event)
      else freemem(event);
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
      for i:=0 to _PlugEvents.count-1 do freemem(_PlugEvents[i]);
      _PlugEvents.free();
      _PlugEvents:=nil;
      for i:=0 to _DataEvents.count-1 do freemem(_DataEvents[i]);
      _DataEvents.free();
      _DataEvents:=nil;
    end;



  procedure native_yFunctionUpdateCallback (f:YFUN_DESCR; data:pansichar);cdecl;
    var
      event  : PyapiEvent;
    begin
      getmem(event,sizeof(TyapiEvent));
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


  procedure native_yTimedReportCallback (f:YFUN_DESCR; timestamp:double; bytes:pansichar; len:integer);cdecl;
    var
      event  : PyapiEvent;
    begin
      getmem(event,sizeof(TyapiEvent));
      event^.fun_descr:=f;
      event^.eventtype := YAPI_FUN_TIMEDREPORT;
      event^.timestamp := timestamp;
      move(bytes^,event^.data[0],len);
      event^.data_len := len;
      _DataEvents.add(event)
    end;


  procedure native_yapiHubDiscoveryCallbackFwd(serial:pansichar; url:pansichar);cdecl;
    var
      event : PyapiEvent;
    begin
      getmem(event,sizeof(TyapiEvent));
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
      npt:= calibType mod 10;
      x:= rawValues[0];
      adj := refValues[0]-x;
      i:=0;
      if npt>high(rawValues)+1 then  npt:=high(rawValues)+1;
      if npt>high(refValues)+1 then  npt:=high(refValues)+1;
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
    begin
      negate:=0;
      if (val=0) then
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
      res := (val and 2047) * _decExp[exp];
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
      _yapiRegisterFunctionUpdateCallback(native_yFunctionUpdateCallback);
      _yapiRegisterTimedReportCallback(native_yTimedReportCallback);
      _yapiRegisterLogFunction(native_yLogFunction);
      _yapiRegisterHubDiscoveryCallback(native_yapiHubDiscoveryCallbackFwd);
      for i:=1 to 20  do
        begin
          yRegisterCalibrationHandler(i,yLinearCalibrationHandler);
        end;
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
          yUpdateDeviceList:=res;
          exit;
        end;
      res := _yapiHandleEvents(perror);
      if (YISERR(res)) then
        begin
          errmsg:=string(perror);
          yUpdateDeviceList:=res;
          exit;
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
          freemem(p);
        end;
      yUpdateDeviceList:=YAPI_SUCCESS;
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
                      SetLength(report, p^.data_len);
                      for j := 0 To p^.data_len-1 do report[j] := p^.data[j];
                      sensor := TYSensor(_TimedReportCallbackList.items[i]);
                      measure := sensor._decodeTimedReport(p^.timestamp, report);
                      sensor._invokeTimedReportCallback(measure);
                      measure.free();
                    end;
                end;
              freemem(p);
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
            res := _yapiSleep(1,pError);
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

  function yapiGetAllDevices( dbuffer:PyHandleArray;maxsize:integer;var neededsize:integer; var errmsg:string):integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror :pansichar;
    begin
      buffer[0]:=#0;perror:=@buffer;
      yapiGetAllDevices:=_yapiGetAllDevices( dbuffer,maxsize,neededsize,perror);
      errmsg := string(perror);
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
      yapiGetFunctionInfo :=_yapiGetFunctionInfo(fundesc,devdesc,pSerial,pFuncId,pFuncname,pFuncVal,pError);
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
      res    := _yapiGetFunctionInfo(fundesc,devdesc,nil,nil,nil,nil,pError);
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




  function  yapiGetBootloadersDevs(var serials:string; maxNbSerial:u32; var totalBootladers:u32; var errmsg:string) :integer;
    var
      buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
      perror,p :pansichar;
    begin
      buffer[0]:=#0; perror:=@buffer;
      getmem(p, maxNbSerial * YOCTO_SERIAL_LEN);
      yapiGetBootloadersDevs := _yapiGetBootloadersDevs(p,maxNbSerial,totalBootladers,perror);
      serials := string(p);
      errmsg  := string(perror);
    end;


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


const
  InfinityAndBeyond =  1.0 / 0.0;

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


    function  TYFunction._buildSetRequest( changeattr : string ; changeval:string ; var request:string; var errmsg:string):YRETCODE;
      var
        res,i    : integer;
        fundesc  : YFUN_DESCR ;
        funcid   : array[0..YOCTO_FUNCTION_LEN] of ansichar;
        pfuncid  : pansichar;
        errbuff  : array[0..YOCTO_ERRMSG_LEN] of ansichar;
        perrbuff   : pansichar;
        uchangeval : string;
        c          : char;
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
        res:=_yapiGetFunctionInfo(fundesc, devdesc, NIL, pfuncid, NIL, NIL,perrbuff);
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
            request    := request+changeattr+'?'+changeattr+'=';
            for i:=1 to length(changeval)  do
              begin
                c:=  changeval[i];
                if  (c<' ') or ((c>#122) and (c<>'~')) or (c='"') or (c='%') or (c='&') or
                (c='+') or (c='<') or (c='=') or (c='>') or (c='\') or (c='^') or (c = '`')
                then
                  begin
                    uchangeval:=uchangeval+'%'+IntToHex(ord(c),2);
                  end 
                else  
                  uchangeval:=uchangeval+c;
              end;
          end;
        request := request+ uchangeval+' '#13#10#13#10;     // no HTTP/1.1 to get light headers
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
      _cacheExpiration := 0;
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


  // Method used to send http request to the device (not the function)
  function TYFunction._download(path:string):TByteArray;
    var
      request   :string;
      buffer    :TByteArray;
      found,i,j :integer;
      body      :integer;
      res       :TByteArray;
      isfound   :boolean;
      tmp :string;
    begin
      request := 'GET /'+path+' HTTP/1.1'#13#10#13#10;
      buffer := self._request(request);
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
      if(found >= length(buffer) - 4) then
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

  function  TYFunction._upload(path: string; strcontent: string) :integer;
    begin
      result :=  _upload(path,  _StrToByte(strcontent));
    end;

  // Method used to upload a file to the device
  function  TYFunction._upload(path: string; content: TByteArray) :integer;
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
      if length(buffer) = 0 then
        begin
          _throw(YAPI_IO_ERROR,'http request failed');
          result := YAPI_IO_ERROR;
          exit;
        end;
      result := YAPI_SUCCESS;
    end;


  function  TYFunction._findDataStream(dataset: TYDataSet; def :string) : TYDataStream;
    var 
      index : integer;
      key : string;
      newDataStream : TYDataStream;
    begin
      key := dataset.get_functionId() + ':' + def;
      if _dataStreams.Find(key, index) then
        begin
          result := TYDataStream(_dataStreams.objects[index]);
          exit;
        end;
      newDataStream := TYDataStream.Create(self, dataset, _decodeWords(def));
      _dataStreams.addObject(key, newDataStream);
      result :=newDataStream;
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
  ///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
  /// </para>
  ///-
  function TYFunction.get_logicalName():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LOGICALNAME_INVALID;
              exit
            end;
        end;
      result := self._logicalName;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the logical name of the function.
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
  ///   a string corresponding to the logical name of the function
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
  function TYFunction.set_logicalName(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('logicalName',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the current value of the function (no more than 6 characters).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the current value of the function (no more than 6 characters)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
  /// </para>
  ///-
  function TYFunction.get_advertisedValue():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_ADVERTISEDVALUE_INVALID;
              exit
            end;
        end;
      result := self._advertisedValue;
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
  ///   Use the method <c>YFunction.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YFunction</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYFunction.FindFunction(func: string):TYFunction;
    var
      obj : TYFunction;
    begin
      obj := TYFunction(TYFunction._FindFromCache('Function', func));
      if obj = nil then
        begin
          obj :=  TYFunction.create(func);
          TYFunction._AddToCache('Function',  func, obj)
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
  function TYFunction.registerValueCallback(callback: TYFunctionValueCallback):LongInt;
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
      self._valueCallbackFunction := callback;
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


  function TYFunction._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackFunction) <> nil) then
        begin
          self._valueCallbackFunction(self, value)
        end
      else
        begin
        end;
      result := 0;
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
      // A valid value in cache means that the device is online
      if(_cacheExpiration > yGetTickCount()) then
      begin result:= true; exit;end;
      // Check that the function is available, without throwing exceptions
      if(YISERR(_getDevice(dev, errmsg))) then
      begin result:=false;exit;end;
      // Try to execute a function request to be positively sure that the device is ready
      if(YISERR(dev.requestAPI(apires, errmsg)))  then
      begin result:=false;exit;end;
      self.load(YAPI_DEFAULTCACHEVALIDITY);
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
      p.free();
      _json_get_key := string(node^.svalue);
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
  function TYFunction.load(msValidity:integer):YRETCODE;
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

  // Return the YModule object for the device on which the function is located
  function  TYFunction.get_module():TYModule;
  var
    fundescr   : YFUN_DESCR;
    devdescr   : YDEV_DESCR;
    errmsg, serial, funcId, funcName, funcValue : string;
  begin
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
    end;

    function TYModule.registerLogCallback(callback : TYModuleLogCallback): integer;
      begin
        _logCallback := callback;
        if (addr(_logCallback) = nil) then
          begin
            _yapiStartStopDeviceLogCallback(pansiChar(ansistring(_serial)), 0)
          end
        else
          begin
            _yapiStartStopDeviceLogCallback(pansiChar(ansistring(_serial)), 1)
          end;
        result := YAPI_SUCCESS;
      end;

    function TYModule.get_logCallback(): TYModuleLogCallback;
      begin
        result := _logCallback;
      end;


  // Return the properties of the nth function of our device
  function TYModule._getFunction(idx:integer; var serial,funcId,funcName,funcVal,errmsg:string):YRETCODE;
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
      res := yapiGetFunctionInfo(fundescr, devdescr, serial, funcId, funcName, funcVal, errmsg);
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
      res := _getDevice(dev, errmsg);
      if(YISERR(res)) then begin _throw(res, errmsg); result:= res;exit;end;
      res := dev.getFunctions(functions, errmsg);
      if(YISERR(res)) then begin functions.free(); _throw(res, errmsg); result:= res;exit;end;
      result:= functions.count;
    end;

  // Retrieve the Id of the nth function (beside "module") in the device
  function TYModule.functionId(functionIndex:integer):string;
    var
      serial, funcId, funcName, funcVal, errmsg:string;
      res:integer;
    begin
      res := _getFunction(functionIndex, serial, funcId, funcName, funcVal, errmsg);
      if(YISERR(res)) then
        begin
          _throw(res, errmsg);
          result:=YAPI_INVALID_STRING;
          exit;
        end;
      result:= funcId;
    end;

  // Retrieve the logical name of the nth function (beside "module") in the device
  function TYModule.functionName(functionIndex:integer):string;
    var
      serial, funcId, funcName, funcVal, errmsg :  string;
      res:integer;
    begin
      res := _getFunction(functionIndex, serial, funcId, funcName, funcVal, errmsg);
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
      serial, funcId, funcName, funcVal, errmsg:string;
      res: integer;
    begin
      res := _getFunction(functionIndex, serial, funcId, funcName, funcVal, errmsg);
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
      // store result in cache
      if assigned(_cacheJson) then _cacheJson.free();
      _cacheJson:= j;
      apires    := j;
      _cacheStamp := yGetTickCount() + YAPI_defaultCacheValidity;
      result:= YAPI_SUCCESS;
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
      _usbBandwidth := Y_USBBANDWIDTH_INVALID;
      _valueCallbackModule := nil;
      _logCallback := nil;
      //--- (end of generated code: YModule accessors initialization)
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
      if (member^.name = 'usbBandwidth') then
        begin
          _usbBandwidth := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

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
  ///   On failure, throws an exception or returns Y_PRODUCTNAME_INVALID.
  /// </para>
  ///-
  function TYModule.get_productName():string;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PRODUCTNAME_INVALID;
              exit
            end;
        end;
      result := self._productName;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_SERIALNUMBER_INVALID.
  /// </para>
  ///-
  function TYModule.get_serialNumber():string;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_SERIALNUMBER_INVALID;
              exit
            end;
        end;
      result := self._serialNumber;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_PRODUCTID_INVALID.
  /// </para>
  ///-
  function TYModule.get_productId():LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PRODUCTID_INVALID;
              exit
            end;
        end;
      result := self._productId;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the hardware release version of the module.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the hardware release version of the module
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PRODUCTRELEASE_INVALID.
  /// </para>
  ///-
  function TYModule.get_productRelease():LongInt;
    begin
      if self._cacheExpiration = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PRODUCTRELEASE_INVALID;
              exit
            end;
        end;
      result := self._productRelease;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_FIRMWARERELEASE_INVALID.
  /// </para>
  ///-
  function TYModule.get_firmwareRelease():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_FIRMWARERELEASE_INVALID;
              exit
            end;
        end;
      result := self._firmwareRelease;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the current state of persistent module settings.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_PERSISTENTSETTINGS_LOADED, Y_PERSISTENTSETTINGS_SAVED and
  ///   Y_PERSISTENTSETTINGS_MODIFIED corresponding to the current state of persistent module settings
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_PERSISTENTSETTINGS_INVALID.
  /// </para>
  ///-
  function TYModule.get_persistentSettings():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_PERSISTENTSETTINGS_INVALID;
              exit
            end;
        end;
      result := self._persistentSettings;
      exit;
    end;


  function TYModule.set_persistentSettings(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('persistentSettings',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the luminosity of the  module informative leds (from 0 to 100).
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the luminosity of the  module informative leds (from 0 to 100)
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LUMINOSITY_INVALID.
  /// </para>
  ///-
  function TYModule.get_luminosity():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LUMINOSITY_INVALID;
              exit
            end;
        end;
      result := self._luminosity;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the luminosity of the module informative leds.
  /// <para>
  ///   The parameter is a
  ///   value between 0 and 100.
  ///   Remember to call the saveToFlash() method of the module if the
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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYModule.set_luminosity(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('luminosity',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the state of the localization beacon.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_BEACON_OFF or Y_BEACON_ON, according to the state of the localization beacon
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_BEACON_INVALID.
  /// </para>
  ///-
  function TYModule.get_beacon():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_BEACON_INVALID;
              exit
            end;
        end;
      result := self._beacon;
      exit;
    end;


  ////
  /// <summary>
  ///   Turns on or off the module localization beacon.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   either Y_BEACON_OFF or Y_BEACON_ON
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
  function TYModule.set_beacon(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('beacon',rest_val);
    end;

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
  ///   On failure, throws an exception or returns Y_UPTIME_INVALID.
  /// </para>
  ///-
  function TYModule.get_upTime():int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_UPTIME_INVALID;
              exit
            end;
        end;
      result := self._upTime;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_USBCURRENT_INVALID.
  /// </para>
  ///-
  function TYModule.get_usbCurrent():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_USBCURRENT_INVALID;
              exit
            end;
        end;
      result := self._usbCurrent;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_REBOOTCOUNTDOWN_INVALID.
  /// </para>
  ///-
  function TYModule.get_rebootCountdown():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_REBOOTCOUNTDOWN_INVALID;
              exit
            end;
        end;
      result := self._rebootCountdown;
      exit;
    end;


  function TYModule.set_rebootCountdown(newval:LongInt):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('rebootCountdown',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the number of USB interfaces used by the module.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_USBBANDWIDTH_SIMPLE or Y_USBBANDWIDTH_DOUBLE, according to the number of USB interfaces
  ///   used by the module
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_USBBANDWIDTH_INVALID.
  /// </para>
  ///-
  function TYModule.get_usbBandwidth():Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_USBBANDWIDTH_INVALID;
              exit
            end;
        end;
      result := self._usbBandwidth;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the number of USB interfaces used by the module.
  /// <para>
  ///   You must reboot the module
  ///   after changing this setting.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   either Y_USBBANDWIDTH_SIMPLE or Y_USBBANDWIDTH_DOUBLE, according to the number of USB interfaces
  ///   used by the module
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
  function TYModule.set_usbBandwidth(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('usbBandwidth',rest_val);
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
  ///   Use the method <c>YModule.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YModule</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYModule.FindModule(func: string):TYModule;
    var
      obj : TYModule;
    begin
      obj := TYModule(TYFunction._FindFromCache('Module', func));
      if obj = nil then
        begin
          obj :=  TYModule.create(func);
          TYFunction._AddToCache('Module',  func, obj)
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
  function TYModule.registerValueCallback(callback: TYModuleValueCallback):LongInt;
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
      self._valueCallbackModule := callback;
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


  function TYModule._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackModule) <> nil) then
        begin
          self._valueCallbackModule(self, value)
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
  ///   Saves current settings in the nonvolatile memory of the module.
  /// <para>
  ///   Warning: the number of allowed save operations during a module life is
  ///   limited (about 100000 cycles). Do not call this function within a loop.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYModule.saveToFlash():LongInt;
    begin
      result := self.set_persistentSettings(Y_PERSISTENTSETTINGS_SAVED);
      exit;
    end;


  ////
  /// <summary>
  ///   Reloads the settings stored in the nonvolatile memory, as
  ///   when the module is powered on.
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYModule.revertFromFlash():LongInt;
    begin
      result := self.set_persistentSettings(Y_PERSISTENTSETTINGS_LOADED);
      exit;
    end;


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
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYModule.reboot(secBeforeReboot: LongInt):LongInt;
    begin
      result := self.set_rebootCountdown(secBeforeReboot);
      exit;
    end;


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
  ///   <c>YAPI_SUCCESS</c> when the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYModule.triggerFirmwareUpdate(secBeforeReboot: LongInt):LongInt;
    begin
      result := self.set_rebootCountdown(-secBeforeReboot);
      exit;
    end;


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
  ///   On failure, throws an exception or returns an empty content.
  /// </para>
  ///-
  function TYModule.download(pathname: string):TByteArray;
    begin
      result := self._download(pathname);
      exit;
    end;


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
  /// </returns>
  ///-
  function TYModule.get_icon2d():TByteArray;
    begin
      result := self._download('icon2d.png');
      exit;
    end;


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
  /// </returns>
  ///-
  function TYModule.get_lastLogs():string;
    var
      content : TByteArray;
    begin
      content := self._download('logs.txt');
      result := _ByteToString(content);
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
      _calibrationParam := Y_CALIBRATIONPARAM_INVALID;
      _resolution := Y_RESOLUTION_INVALID;
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
          _currentValue := member^.ivalue/65536.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'lowestValue') then
        begin
          _lowestValue := member^.ivalue/65536.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'highestValue') then
        begin
          _highestValue := member^.ivalue/65536.0;
         result := 1;
         exit;
         end;
      if (member^.name = 'currentRawValue') then
        begin
          _currentRawValue := member^.ivalue/65536.0;
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
      if (member^.name = 'calibrationParam') then
        begin
          _calibrationParam := string(member^.svalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'resolution') then
        begin
          if (member^.ivalue > 100) then _resolution := 1.0 / round(65536.0/member^.ivalue) else _resolution := 0.001 / round(67.0/member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

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
  ///   On failure, throws an exception or returns Y_UNIT_INVALID.
  /// </para>
  ///-
  function TYSensor.get_unit():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_UNIT_INVALID;
              exit
            end;
        end;
      result := self._unit;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the current value of the measure.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the current value of the measure
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_CURRENTVALUE_INVALID.
  /// </para>
  ///-
  function TYSensor.get_currentValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTVALUE_INVALID;
              exit
            end;
        end;
      res := self._applyCalibration(self._currentRawValue);
      if res = Y_CURRENTVALUE_INVALID then
        begin
          res := self._currentValue
        end;
      res := res * self._iresol;
      result := round(res) / self._iresol;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the recorded minimal value observed.
  /// <para>
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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYSensor.set_lowestValue(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval*65536.0));
      result := _setAttr('lowestValue',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the minimal value observed for the measure since the device was started.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the minimal value observed for the measure since the device was started
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_LOWESTVALUE_INVALID.
  /// </para>
  ///-
  function TYSensor.get_lowestValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LOWESTVALUE_INVALID;
              exit
            end;
        end;
      res := self._lowestValue * self._iresol;
      result := round(res) / self._iresol;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the recorded maximal value observed.
  /// <para>
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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYSensor.set_highestValue(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval*65536.0));
      result := _setAttr('highestValue',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the maximal value observed for the measure since the device was started.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the maximal value observed for the measure since the device was started
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_HIGHESTVALUE_INVALID.
  /// </para>
  ///-
  function TYSensor.get_highestValue():double;
    var
      res : double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_HIGHESTVALUE_INVALID;
              exit
            end;
        end;
      res := self._highestValue * self._iresol;
      result := round(res) / self._iresol;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the uncalibrated, unrounded raw value returned by the sensor.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the uncalibrated, unrounded raw value returned by the sensor
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_CURRENTRAWVALUE_INVALID.
  /// </para>
  ///-
  function TYSensor.get_currentRawValue():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTRAWVALUE_INVALID;
              exit
            end;
        end;
      result := self._currentRawValue;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_LOGFREQUENCY_INVALID.
  /// </para>
  ///-
  function TYSensor.get_logFrequency():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_LOGFREQUENCY_INVALID;
              exit
            end;
        end;
      result := self._logFrequency;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the datalogger recording frequency for this function.
  /// <para>
  ///   The frequency can be specified as samples per second,
  ///   as sample per minute (for instance "15/m") or in samples per
  ///   hour (eg. "4/h"). To disable recording for this function, use
  ///   the value "OFF".
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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYSensor.set_logFrequency(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('logFrequency',rest_val);
    end;

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
  ///   On failure, throws an exception or returns Y_REPORTFREQUENCY_INVALID.
  /// </para>
  ///-
  function TYSensor.get_reportFrequency():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_REPORTFREQUENCY_INVALID;
              exit
            end;
        end;
      result := self._reportFrequency;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the timed value notification frequency for this function.
  /// <para>
  ///   The frequency can be specified as samples per second,
  ///   as sample per minute (for instance "15/m") or in samples per
  ///   hour (eg. "4/h"). To disable timed value notifications for this
  ///   function, use the value "OFF".
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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYSensor.set_reportFrequency(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('reportFrequency',rest_val);
    end;

  function TYSensor.get_calibrationParam():string;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CALIBRATIONPARAM_INVALID;
              exit
            end;
        end;
      result := self._calibrationParam;
      exit;
    end;


  function TYSensor.set_calibrationParam(newval:string):integer;
    var
      rest_val: string;
    begin
      rest_val := newval;
      result := _setAttr('calibrationParam',rest_val);
    end;

  ////
  /// <summary>
  ///   Changes the resolution of the measured physical values.
  /// <para>
  ///   The resolution corresponds to the numerical precision
  ///   when displaying value. It does not change the precision of the measure itself.
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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYSensor.set_resolution(newval:double):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(round(newval*65536.0));
      result := _setAttr('resolution',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the resolution of the measured values.
  /// <para>
  ///   The resolution corresponds to the numerical precision
  ///   of the measures, which is not always the same as the actual precision of the sensor.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a floating point number corresponding to the resolution of the measured values
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_RESOLUTION_INVALID.
  /// </para>
  ///-
  function TYSensor.get_resolution():double;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_RESOLUTION_INVALID;
              exit
            end;
        end;
      result := self._resolution;
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
  ///   Use the method <c>YSensor.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YSensor</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYSensor.FindSensor(func: string):TYSensor;
    var
      obj : TYSensor;
    begin
      obj := TYSensor(TYFunction._FindFromCache('Sensor', func));
      if obj = nil then
        begin
          obj :=  TYSensor.create(func);
          TYFunction._AddToCache('Sensor',  func, obj)
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
  function TYSensor.registerValueCallback(callback: TYSensorValueCallback):LongInt;
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
      self._valueCallbackSensor := callback;
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


  function TYSensor._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackSensor) <> nil) then
        begin
          self._valueCallbackSensor(self, value)
        end
      else
        begin
          inherited _invokeValueCallback(value)
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
      if self._resolution > 0 then
        begin
          self._iresol := round(1.0 / self._resolution)
        end
      else
        begin
          result := 0;
          exit
        end;
      
      self._scale := -1;
      SetLength(self._calpar, 0);
      SetLength(self._calraw, 0);
      SetLength(self._calref, 0);
      
      // Old format: supported when there is no calibration
      if (self._calibrationParam = '') or (self._calibrationParam = '0') then
        begin
          self._caltyp := 0;
          result := 0;
          exit
        end;
      // Old format: calibrated value will be provided by the device
      if (pos(',', self._calibrationParam) - 1) >= 0 then
        begin
          self._caltyp := -1;
          result := 0;
          exit
        end;
      // New format, decode parameters
      iCalib := _decodeWords(self._calibrationParam);
      // In case of unknown format, calibrated value will be provided by the device
      if length(iCalib) < 2 then
        begin
          self._caltyp := -1;
          result := 0;
          exit
        end;
      
      // Save variable format (scale for scalar, or decimal exponent)          
      self._isScal := (iCalib[1] > 0);
      if self._isScal then
        begin
          self._offset := iCalib[0];
          if self._offset > 32767 then
            begin
              self._offset := self._offset - 65536
            end;
          self._scale := iCalib[1];
          self._decexp := 0
        end
      else
        begin
          self._offset := 0;
          self._scale := 1;
          self._decexp := 1.0;
          position := iCalib[0];
          while position > 0 do
            begin
              self._decexp := self._decexp * 10;
              position := position - 1
            end;
        end;
      
      // Shortcut when there is no calibration parameter
      if length(iCalib) = 2 then
        begin
          self._caltyp := 0;
          result := 0;
          exit
        end;
      
      self._caltyp := iCalib[2];
      self._calhdl := _getCalibrationHandler(self._caltyp);
      // parse calibration points
      position := 3;
      if self._caltyp <= 10 then
        begin
          maxpos := self._caltyp
        end
      else
        begin
          if self._caltyp <= 20 then
            begin
              maxpos := self._caltyp - 10
            end
          else
            begin
              maxpos := 5
            end;
        end;
      maxpos := 3 + 2 * maxpos;
      if maxpos > length(iCalib) then
        begin
          maxpos := length(iCalib)
        end;
      calpar_pos := 0;
      SetLength(self._calpar, maxpos);;
      calraw_pos := 0;
      SetLength(self._calraw, maxpos);;
      calref_pos := 0;
      SetLength(self._calref, maxpos);;
      while position + 1 < maxpos do
        begin
          iRaw := iCalib[position];
          iRef := iCalib[position + 1];
          self._calpar[calpar_pos] := iRaw;
          inc(calpar_pos);
          self._calpar[calpar_pos] := iRef;
          inc(calpar_pos);
          if self._isScal then
            begin
              fRaw := iRaw;
              fRaw := (fRaw - self._offset) / self._scale;
              fRef := iRef;
              fRef := (fRef - self._offset) / self._scale;
              self._calraw[calraw_pos] := fRaw;
              inc(calraw_pos);
              self._calref[calref_pos] := fRef;
              inc(calref_pos)
            end
          else
            begin
              self._calraw[calraw_pos] := _decimalToDouble(iRaw);
              inc(calraw_pos);
              self._calref[calref_pos] := _decimalToDouble(iRef);
              inc(calref_pos)
            end;
          position := position + 2
        end;
      SetLength(self._calpar, calpar_pos);
      SetLength(self._calraw, calraw_pos);
      SetLength(self._calref, calref_pos);
      result := 0;
      exit;
    end;


  ////
  /// <summary>
  ///   Retrieves a DataSet object holding historical data for this
  ///   sensor, for a specified time interval.
  /// <para>
  ///   The measures will be
  ///   retrieved from the data logger, which must have been turned
  ///   on at the desired time. See the documentation of the DataSet
  ///   class for information on how to get an overview of the
  ///   recorded data, and how to load progressively a large set
  ///   of measures from the data logger.
  /// </para>
  /// <para>
  ///   This function only works if the device uses a recent firmware,
  ///   as DataSet objects are not supported by firmwares older than
  ///   version 13000.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="startTime">
  ///   the start of the desired measure time interval,
  ///   as a Unix timestamp, i.e. the number of seconds since
  ///   January 1, 1970 UTC. The special value 0 can be used
  ///   to include any meaasure, without initial limit.
  /// </param>
  /// <param name="endTime">
  ///   the end of the desired measure time interval,
  ///   as a Unix timestamp, i.e. the number of seconds since
  ///   January 1, 1970 UTC. The special value 0 can be used
  ///   to include any meaasure, without ending limit.
  /// </param>
  /// <returns>
  ///   an instance of YDataSet, providing access to historical
  ///   data. Past measures can be loaded progressively
  ///   using methods from the YDataSet object.
  /// </returns>
  ///-
  function TYSensor.get_recordedData(startTime: int64; endTime: int64):TYDataSet;
    var
      funcid : string;
      funit : string;
    begin
      funcid := self.get_functionId;
      funit := self.get_unit;
      result := TYDataSet.create(self, funcid, funit, startTime, endTime);
      exit;
    end;


  ////
  /// <summary>
  ///   Registers the callback function that is invoked on every periodic timed notification.
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
  ///   arguments: the function object of which the value has changed, and an YMeasure object describing
  ///   the new advertised value.
  /// @noreturn
  /// </param>
  ///-
  function TYSensor.registerTimedReportCallback(callback: TYSensorTimedReportCallback):LongInt;
    begin
      if (addr(callback) <> nil) then
        begin
          TYFunction._UpdateTimedReportCallbackList(self, true)
        end
      else
        begin
          TYFunction._UpdateTimedReportCallbackList(self, false)
        end;
      self._timedReportCallbackSensor := callback;
      result := 0;
      exit;
    end;


  function TYSensor._invokeTimedReportCallback(value: TYMeasure):LongInt;
    begin
      if (addr(self._timedReportCallbackSensor) <> nil) then
        begin
          self._timedReportCallbackSensor(self, value)
        end
      else
        begin
        end;
      result := 0;
      exit;
    end;


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
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYSensor.calibrateFromPoints(rawValues: TDoubleArray; refValues: TDoubleArray):LongInt;
    var
      rest_val : string;
    begin
      rest_val := self._encodeCalibrationPoints(rawValues, refValues);
      result := self._setAttr('calibrationParam', rest_val);
      exit;
    end;


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
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
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
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := YAPI_DEVICE_NOT_FOUND;
              exit
            end;
        end;
      
      if self._caltyp < 0 then
        begin
          self._throw(YAPI_NOT_SUPPORTED, 'Device does not support new calibration parameters. Please upgrade your firmware');
          result := YAPI_NOT_SUPPORTED;
          exit
        end;
      rawValues_pos := 0;
      SetLength(rawValues, length(self._calraw));;
      refValues_pos := 0;
      SetLength(refValues, length(self._calref));;
      for i_i:=0 to length(self._calraw)-1 do
        begin
          rawValues[rawValues_pos] := self._calraw[i_i];
          inc(rawValues_pos)
        end;
      for i_i:=0 to length(self._calref)-1 do
        begin
          refValues[refValues_pos] := self._calref[i_i];
          inc(refValues_pos)
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYSensor._encodeCalibrationPoints(rawValues: TDoubleArray; refValues: TDoubleArray):string;
    var
      res : string;
      npt : LongInt;
      idx : LongInt;
      iRaw : LongInt;
      iRef : LongInt;
    begin
      npt := length(rawValues);
      if npt <> length(refValues) then
        begin
          self._throw(YAPI_INVALID_ARGUMENT, 'Invalid calibration parameters (size mismatch)');
          result := YAPI_INVALID_STRING;
          exit
        end;
      
      // Shortcut when building empty calibration parameters       
      if npt = 0 then
        begin
          result := '0';
          exit
        end;
      
      // Load function parameters if not yet loaded
      if self._scale = 0 then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := YAPI_INVALID_STRING;
              exit
            end;
        end;
      
      // Detect old firmware
      if (self._caltyp < 0) or(self._scale < 0) then
        begin
          self._throw(YAPI_NOT_SUPPORTED, 'Device does not support new calibration parameters. Please upgrade your firmware');
          result := '0';
          exit
        end;
      if self._isScal then
        begin
          res := ''+inttostr(npt);
          idx := 0;
          while idx < npt do
            begin
              iRaw := round(rawValues[idx] * self._scale - self._offset);
              iRef := round(refValues[idx] * self._scale - self._offset);
              res := ''+ res+','+inttostr( iRaw)+','+inttostr(iRef);
              idx := idx + 1
            end;
        end
      else
        begin
          res := ''+inttostr(10 + npt);
          idx := 0;
          while idx < npt do
            begin
              iRaw := _doubleToDecimal(rawValues[idx]);
              iRef := _doubleToDecimal(refValues[idx]);
              res := ''+ res+','+inttostr( iRaw)+','+inttostr(iRef);
              idx := idx + 1
            end;
        end;
      result := res;
      exit;
    end;


  function TYSensor._applyCalibration(rawValue: double):double;
    begin
      if rawValue = Y_CURRENTVALUE_INVALID then
        begin
          result := Y_CURRENTVALUE_INVALID;
          exit
        end;
      if self._caltyp = 0 then
        begin
          result := rawValue;
          exit
        end;
      if self._caltyp < 0 then
        begin
          result := Y_CURRENTVALUE_INVALID;
          exit
        end;
      if not((addr(self._calhdl) <> nil)) then
        begin
          result := Y_CURRENTVALUE_INVALID;
          exit
        end;
      result := self._calhdl(rawValue, self._caltyp, self._calpar, self._calraw, self._calref);
      exit;
    end;


  function TYSensor._decodeTimedReport(timestamp: double; report: TLongIntArray):TYMeasure;
    var
      i : LongInt;
      byteVal : LongInt;
      poww : LongInt;
      minRaw : LongInt;
      avgRaw : LongInt;
      maxRaw : LongInt;
      startTime : double;
      endTime : double;
      minVal : double;
      avgVal : double;
      maxVal : double;
    begin
      startTime := self._prevTimedReport;
      endTime := timestamp;
      self._prevTimedReport := endTime;
      if startTime = 0 then
        begin
          startTime := endTime
        end;
      if report[0] > 0 then
        begin
          minRaw := report[1] + $0100 * report[2];
          maxRaw := report[3] + $0100 * report[4];
          avgRaw := report[5] + $0100 * report[6] + $010000 * report[7];
          byteVal := report[8];
          if ((byteVal) and ($080)) = 0 then
            begin
              avgRaw := avgRaw + $01000000 * byteVal
            end
          else
            begin
              avgRaw := avgRaw - $01000000 * ($0100 - byteVal)
            end;
          minVal := self._decodeVal(minRaw);
          avgVal := self._decodeAvg(avgRaw);
          maxVal := self._decodeVal(maxRaw)
        end
      else
        begin
          poww := 1;
          avgRaw := 0;
          byteVal := 0;
          i := 1;
          while i < length(report) do
            begin
              byteVal := report[i];
              avgRaw := avgRaw + poww * byteVal;
              poww := poww * $0100;
              i := i + 1
            end;
          if self._isScal then
            begin
              avgVal := self._decodeVal(avgRaw)
            end
          else
            begin
              if ((byteVal) and ($080)) <> 0 then
                begin
                  avgRaw := avgRaw - poww
                end;
              avgVal := self._decodeAvg(avgRaw)
            end;
          minVal := avgVal;
          maxVal := avgVal
        end;
      
      result := TYMeasure.create(startTime, endTime, minVal, avgVal, maxVal);
      exit;
    end;


  function TYSensor._decodeVal(w: LongInt):double;
    var
      val : double;
    begin
      val := w;
      if self._isScal then
        begin
          val := (val - self._offset) / self._scale
        end
      else
        begin
          val := _decimalToDouble(w)
        end;
      if self._caltyp <> 0 then
        begin
          val := self._calhdl(val, self._caltyp, self._calpar, self._calraw, self._calref)
        end;
      result := val;
      exit;
    end;


  function TYSensor._decodeAvg(dw: LongInt):double;
    var
      val : double;
    begin
      val := dw;
      if self._isScal then
        begin
          val := (val / 100 - self._offset) / self._scale
        end
      else
        begin
          val := val / self._decexp
        end;
      if self._caltyp <> 0 then
        begin
          val := self._calhdl(val, self._caltyp, self._calpar, self._calraw, self._calref)
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

//--- (generated code: Sensor functions)

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

//--- (end of generated code: Sensor functions)


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


//--- (generated code: YDataStream implementation)

  function TYDataStream._initFromDataSet(dataset: TYDataSet; encoded: TLongIntArray):LongInt;
    var
      val : LongInt;
      i : LongInt;
      iRaw : LongInt;
      iRef : LongInt;
      fRaw : double;
      fRef : double;
      duration_float : double;
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
      self._samplesPerHour := ((val) and ($0ff));
      if ((val) and ($0100)) <> 0 then
        begin
          self._samplesPerHour := self._samplesPerHour * 3600
        end
      else
        begin
          if ((val) and ($0200)) <> 0 then
            begin
              self._samplesPerHour := self._samplesPerHour * 60
            end;
        end;
      
      val := encoded[5];
      if val > 32767 then
        begin
          val := val - 65536
        end;
      self._decimals := val;
      self._offset := val;
      self._scale := encoded[6];
      self._isScal := (self._scale <> 0);
      
      val := encoded[7];
      self._isClosed := (val <> $0ffff);
      if val = $0ffff then
        begin
          val := 0
        end;
      self._nRows := val;
      duration_float := self._nRows * 3600 / self._samplesPerHour;
      self._duration := round(duration_float);
      // precompute decoding parameters
      self._decexp := 1.0;
      if self._scale = 0 then
        begin
          i := 0;
          while i < self._decimals do
            begin
              self._decexp := self._decexp * 10.0;
              i := i + 1
            end;
        end;
      iCalib := dataset.get_calibration();
      self._caltyp := iCalib[0];
      if self._caltyp <> 0 then
        begin
          self._calhdl := _getCalibrationHandler(self._caltyp);
          calpar_pos := 0;
          SetLength(self._calpar, length(iCalib));
          calraw_pos := 0;
          SetLength(self._calraw, length(iCalib));
          calref_pos := 0;
          SetLength(self._calref, length(iCalib));
          i := 1;
          while i + 1 < length(iCalib) do
            begin
              iRaw := iCalib[i];
              iRef := iCalib[i + 1];
              self._calpar[calpar_pos] := iRaw;
              inc(calpar_pos);
              self._calpar[calpar_pos] := iRef;
              inc(calpar_pos);
              if self._isScal then
                begin
                  fRaw := iRaw;
                  fRaw := (fRaw - self._offset) / self._scale;
                  fRef := iRef;
                  fRef := (fRef - self._offset) / self._scale;
                  self._calraw[calraw_pos] := fRaw;
                  inc(calraw_pos);
                  self._calref[calref_pos] := fRef;
                  inc(calref_pos)
                end
              else
                begin
                  self._calraw[calraw_pos] := _decimalToDouble(iRaw);
                  inc(calraw_pos);
                  self._calref[calref_pos] := _decimalToDouble(iRef);
                  inc(calref_pos)
                end;
              i := i + 2
            end;
          SetLength(self._calpar, calpar_pos);
          SetLength(self._calraw, calraw_pos);
          SetLength(self._calref, calref_pos)
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
          self._nCols := 3
        end
      else
        begin
          columnNames_pos := 0;
          SetLength(self._columnNames, 1);
          self._columnNames[columnNames_pos] := self._functionId;
          inc(columnNames_pos);
          SetLength(self._columnNames, columnNames_pos);
          self._nCols := 1
        end;
      // decode min/avg/max values for the sequence
      if self._nRows > 0 then
        begin
          self._minVal := self._decodeVal(encoded[8]);
          self._maxVal := self._decodeVal(encoded[9]);
          self._avgVal := self._decodeAvg(encoded[10] + (((encoded[11]) shl 16)), self._nRows)
        end;
      result := 0;
      exit;
    end;


  function TYDataStream.parse(sdata: TByteArray):LongInt;
    var
      idx : LongInt;
      udat : TLongIntArray;
      dat : TDoubleArray;
      values_pos : LongInt;
      dat_pos : LongInt;
    begin
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
              dat[dat_pos] := self._decodeVal(udat[idx]);
              inc(dat_pos);
              dat[dat_pos] := self._decodeAvg(udat[idx + 2] + (((udat[idx + 3]) shl 16)), 1);
              inc(dat_pos);
              dat[dat_pos] := self._decodeVal(udat[idx + 1]);
              inc(dat_pos);
              SetLength(dat, dat_pos);
              self._values[values_pos] := dat;
              inc(values_pos);
              idx := idx + 4
            end;
        end
      else
        begin
          if self._isScal then
            begin
              while idx < length(udat) do
                begin
                  dat_pos := 0;
                  SetLength(dat, 1);
                  dat[dat_pos] := self._decodeVal(udat[idx]);
                  inc(dat_pos);
                  SetLength(dat, dat_pos);
                  self._values[values_pos] := dat;
                  inc(values_pos);
                  idx := idx + 1
                end;
            end
          else
            begin
              while idx + 1 < length(udat) do
                begin
                  dat_pos := 0;
                  SetLength(dat, 1);
                  dat[dat_pos] := self._decodeAvg(udat[idx] + (((udat[idx + 1]) shl 16)), 1);
                  inc(dat_pos);
                  SetLength(dat, dat_pos);
                  self._values[values_pos] := dat;
                  inc(values_pos);
                  idx := idx + 2
                end;
            end;
        end;
      SetLength(self._values, values_pos);
      self._nRows := length(self._values);
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYDataStream.get_url():string;
    var
      url : string;
    begin
      url := 'logger.json?id='+
      self._functionId+'&run='+inttostr(self._runNo)+'&utc='+inttostr(self._utcStamp);
      result := url;
      exit;
    end;


  function TYDataStream.loadStream():LongInt;
    begin
      result := self.parse(self._parent._download(self.get_url));
      exit;
    end;


  function TYDataStream._decodeVal(w: LongInt):double;
    var
      val : double;
    begin
      val := w;
      if self._isScal then
        begin
          val := (val - self._offset) / self._scale
        end
      else
        begin
          val := _decimalToDouble(w)
        end;
      if self._caltyp <> 0 then
        begin
          val := self._calhdl(val, self._caltyp, self._calpar, self._calraw, self._calref)
        end;
      result := val;
      exit;
    end;


  function TYDataStream._decodeAvg(dw: LongInt; count: LongInt):double;
    var
      val : double;
    begin
      val := dw;
      if self._isScal then
        begin
          val := (val / (100 * count) - self._offset) / self._scale
        end
      else
        begin
          val := val / (count * self._decexp)
        end;
      if self._caltyp <> 0 then
        begin
          val := self._calhdl(val, self._caltyp, self._calpar, self._calraw, self._calref)
        end;
      result := val;
      exit;
    end;


  function TYDataStream.isClosed():boolean;
    begin
      result := self._isClosed;
      exit;
    end;


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
  function TYDataStream.get_runIndex():LongInt;
    begin
      result := self._runNo;
      exit;
    end;


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
  function TYDataStream.get_startTime():LongInt;
    begin
      result := integer(self._utcStamp - Round((Now()-25569)*86400));
      exit;
    end;


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
  ///   an unsigned number corresponding to the number of seconds
  ///   between the Jan 1, 1970 and the beginning of this data
  ///   stream (i.e. Unix time representation of the absolute time).
  /// </returns>
  ///-
  function TYDataStream.get_startTimeUTC():int64;
    begin
      result := self._utcStamp;
      exit;
    end;


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
  function TYDataStream.get_dataSamplesIntervalMs():LongInt;
    begin
      result := (3600000 div self._samplesPerHour);
      exit;
    end;


  function TYDataStream.get_dataSamplesInterval():double;
    begin
      result := 3600.0 / self._samplesPerHour;
      exit;
    end;


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
  function TYDataStream.get_rowCount():LongInt;
    begin
      if (self._nRows <> 0) and self._isClosed then
        begin
          result := self._nRows;
          exit
        end;
      self.loadStream;
      result := self._nRows;
      exit;
    end;


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
  function TYDataStream.get_columnCount():LongInt;
    begin
      if self._nCols <> 0 then
        begin
          result := self._nCols;
          exit
        end;
      self.loadStream;
      result := self._nCols;
      exit;
    end;


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
  function TYDataStream.get_columnNames():TStringArray;
    begin
      if length(self._columnNames) <> 0 then
        begin
          result := self._columnNames;
          exit
        end;
      self.loadStream;
      result := self._columnNames;
      exit;
    end;


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
  function TYDataStream.get_minValue():double;
    begin
      result := self._minVal;
      exit;
    end;


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
  function TYDataStream.get_averageValue():double;
    begin
      result := self._avgVal;
      exit;
    end;


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
  function TYDataStream.get_maxValue():double;
    begin
      result := self._maxVal;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the approximate duration of this stream, in seconds.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   the number of seconds covered by this stream.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_DURATION_INVALID.
  /// </para>
  ///-
  function TYDataStream.get_duration():LongInt;
    begin
      if self._isClosed then
        begin
          result := self._duration;
          exit
        end;
      result := integer(Round((Now()-25569)*86400) - self._utcStamp);
      exit;
    end;


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
  function TYDataStream.get_dataRows():TDoubleArrayArray;
    begin
      if (length(self._values) = 0) or not(self._isClosed) then
        begin
          self.loadStream
        end;
      result := self._values;
      exit;
    end;


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
  function TYDataStream.get_data(row: LongInt; col: LongInt):double;
    begin
      if (length(self._values) = 0) or not(self._isClosed) then
        begin
          self.loadStream
        end;
      if row >= length(self._values) then
        begin
          result := Y_DATA_INVALID;
          exit
        end;
      if col >= length(self._values[row]) then
        begin
          result := Y_DATA_INVALID;
          exit
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
  ///   an floating point number corresponding to the number of seconds
  ///   between the Jan 1, 1970 UTC and the beginning of this measure.
  /// </returns>
  ///-
  function TYMeasure.get_startTimeUTC():double;
    begin
      result := self._start;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the end time of the measure, relative to the Jan 1, 1970 UTC
  ///   (Unix timestamp).
  /// <para>
  ///   When the recording rate is higher then 1 sample
  ///   per second, the timestamp may have a fractional part.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an floating point number corresponding to the number of seconds
  ///   between the Jan 1, 1970 UTC and the end of this measure.
  /// </returns>
  ///-
  function TYMeasure.get_endTimeUTC():double;
    begin
      result := self._end;
      exit;
    end;


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
  function TYMeasure.get_minValue():double;
    begin
      result := self._minVal;
      exit;
    end;


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
  function TYMeasure.get_averageValue():double;
    begin
      result := self._avgVal;
      exit;
    end;


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
  function TYMeasure.get_maxValue():double;
    begin
      result := self._maxVal;
      exit;
    end;


//--- (end of generated code: YMeasure implementation)





  constructor TYDataSet.Create(parent:TYFunction; functionId,func_unit:string; startTime,endTime: LongWord);
    begin
      self._parent     := parent;
      self._functionId := functionId;
      self._unit       := func_unit;
      self._startTime  := startTime;
      self._endTime    := endTime;
      self._summary    := TYMeasure.create(0, 0, 0, 0, 0);
      self._progress   := -1;
    end;

  constructor TYDataSet.Create(parent:TYFunction; data:string);
    begin
      self._parent := parent;
      self._startTime := 0;
      self._endTime := 0;
      self._summary := TYMeasure.create(0, 0, 0, 0, 0);
      self._parse(data);
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
      summaryMinVal, summaryMaxVal, summaryTotalTime, summaryTotalAvg: double;
      i : integer;
      endtime, startTime: LongWord;
      rec :  TYMeasure;
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


      summaryMinVal := +InfinityAndBeyond;
      summaryMaxVal := -InfinityAndBeyond;
      summaryTotalTime := 0;
      summaryTotalAvg := 0;
      node := p.GetChildNode(nil, 'id');
      self._functionId := string(node.svalue);
      node := p.GetChildNode(nil, 'unit');
      self._unit := string(node.svalue);
      node := p.GetChildNode(nil, 'cal');
      self._calib := _decodeWords(string(node.svalue));
      arr := p.GetChildNode(nil, 'streams');
      SetLength(self._streams, 0);
      SetLength(self._preview, 0);
      SetLength(self._measures, 0);
      for i := 0 to arr.itemcount-1 do
        begin
          stream := _parent._findDataStream(self, string(arr.items[i].svalue));
          if (self._startTime > 0) and (stream.get_startTimeUTC() + LongWord(stream.get_duration()) <= LongWord(self._startTime)) then
            begin
              // self stream is too early, drop it
            end
          else
          if (self._endTime > 0) and (stream.get_startTimeUTC() > self._endTime) then
            begin
              // self stream is too late, drop it
            end
          else
            begin
              SetLength(self._streams, length(self._streams) + 1);
              self._streams[length(self._streams)-1] := stream;
              if stream.isClosed() and (stream.get_startTimeUTC() >= self._startTime) and
                 ( (self._endTime = 0) or (stream.get_startTimeUTC() + LongWord(stream.get_duration()) <= LongWord(self._endTime))) then
                begin
                  if summaryMinVal > stream.get_minValue() then
                    begin
                      summaryMinVal := stream.get_minValue();
                    end;                 
                  if summaryMaxVal < stream.get_maxValue() then
                    begin
                      summaryMaxVal := stream.get_maxValue();
                    end;
                  summaryTotalAvg := summaryTotalAvg + (stream.get_averageValue() * stream.get_duration());
                  summaryTotalTime := summaryTotalTime + stream.get_duration();
                  rec := TYMeasure.create(stream.get_startTimeUTC(),
                                          stream.get_startTimeUTC() + LongWord(stream.get_duration()),
                                          stream.get_minValue(),
                                          stream.get_averageValue(),
                                          stream.get_maxValue());
                  SetLength(self._preview, length(self._preview) + 1); 
                  self._preview[length(self._preview)-1] := rec;
                end;
            end;
        end;
      if (length(self._streams)>0)  and (summaryTotalTime>0) then
        begin      
          // update time boundaries with actual data
          stream := self._streams[length(self._streams) - 1];
          endtime := stream.get_startTimeUTC() + LongWord(stream.get_duration());
          startTime := self._streams[0].get_startTimeUTC() - LongWord(stream.get_dataSamplesIntervalMs() div 1000);
          if self._startTime < startTime then
            begin
              self._startTime := startTime;
            end;
          if (self._endTime = 0) or (self._endTime > endtime) then
            begin
              self._endTime := endtime;
            end;
          self._summary.free();
          self._summary := TYMeasure.create(_startTime,
                                            _endTime,
                                            summaryMinVal,
                                            summaryTotalAvg / summaryTotalTime,
                                            summaryMaxVal);
        end;
      self._progress := 0;
      p.free();
      result := self.get_progress();
    end;



//--- (generated code: YDataSet implementation)

  function TYDataSet.get_calibration():TLongIntArray;
    begin
      result := self._calib;
      exit;
    end;


  function TYDataSet.processMore(progress: LongInt; data: TByteArray):LongInt;
    var
      stream : TYDataStream;
      dataRows : TDoubleArrayArray;
      strdata : string;
      tim : double;
      itv : double;
      nCols : LongInt;
      minCol : LongInt;
      avgCol : LongInt;
      maxCol : LongInt;
      measures_pos : LongInt;
      i_i : LongInt;
    begin
      if progress <> self._progress then
        begin
          result := self._progress;
          exit
        end;
      if self._progress < 0 then
        begin
          strdata := _ByteToString(data);
          if (strdata = '{}') then
            begin
              self._parent._throw(YAPI_VERSION_MISMATCH, 'device firmware is too old');
              result := YAPI_VERSION_MISMATCH;
              exit
            end;
          result := self._parse(strdata);
          exit
        end;
      stream := self._streams[self._progress];
      stream.parse(data);
      dataRows := stream.get_dataRows();
      self._progress := self._progress + 1;
      if length(dataRows) = 0 then
        begin
          result := self.get_progress;
          exit
        end;
      tim := stream.get_startTimeUTC();
      itv := stream.get_dataSamplesInterval();
      nCols := length(dataRows[0]);
      minCol := 0;
      if nCols > 2 then
        begin
          avgCol := 1
        end
      else
        begin
          avgCol := 0
        end;
      if nCols > 2 then
        begin
          maxCol := 2
        end
      else
        begin
          maxCol := 0
        end;
      measures_pos := length(self._measures);
      SetLength(self._measures, measures_pos+length(dataRows));
      for i_i:=0 to length(dataRows)-1 do
        begin
          if (tim >= self._startTime) and((self._endTime = 0) or(tim <= self._endTime)) then
            begin
              self._measures[measures_pos] := TYMeasure.create(tim - itv, tim, dataRows[i_i][minCol], dataRows[i_i][avgCol], dataRows[i_i][maxCol]);
              inc(measures_pos);
              tim := tim + itv
            end;
        end;
      SetLength(self._measures, measures_pos);
      result := self.get_progress;
      exit;
    end;


  function TYDataSet.get_privateDataStreams():TYDataStreamArray;
    begin
      result := self._streams;
      exit;
    end;


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
  ///   On failure, throws an exception or returns  <c>Y_HARDWAREID_INVALID</c>.
  /// </para>
  ///-
  function TYDataSet.get_hardwareId():string;
    var
      mo : TYModule;
    begin
      if not((self._hardwareId = '')) then
        begin
          result := self._hardwareId;
          exit
        end;
      mo := self._parent.get_module();
      self._hardwareId := ''+ mo.get_serialNumber()+'.'+self.get_functionId;
      result := self._hardwareId;
      exit;
    end;


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
  function TYDataSet.get_functionId():string;
    begin
      result := self._functionId;
      exit;
    end;


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
  ///   On failure, throws an exception or returns  <c>Y_UNIT_INVALID</c>.
  /// </para>
  ///-
  function TYDataSet.get_unit():string;
    begin
      result := self._unit;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the start time of the dataset, relative to the Jan 1, 1970.
  /// <para>
  ///   When the YDataSet is created, the start time is the value passed
  ///   in parameter to the <c>get_dataSet()</c> function. After the
  ///   very first call to <c>loadMore()</c>, the start time is updated
  ///   to reflect the timestamp of the first measure actually found in the
  ///   dataLogger within the specified range.
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
  function TYDataSet.get_startTimeUTC():int64;
    begin
      result := self._startTime;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the end time of the dataset, relative to the Jan 1, 1970.
  /// <para>
  ///   When the YDataSet is created, the end time is the value passed
  ///   in parameter to the <c>get_dataSet()</c> function. After the
  ///   very first call to <c>loadMore()</c>, the end time is updated
  ///   to reflect the timestamp of the last measure actually found in the
  ///   dataLogger within the specified range.
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
  function TYDataSet.get_endTimeUTC():int64;
    begin
      result := self._endTime;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the progress of the downloads of the measures from the data logger,
  ///   on a scale from 0 to 100.
  /// <para>
  ///   When the object is instanciated by <c>get_dataSet</c>,
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
  function TYDataSet.get_progress():LongInt;
    begin
      if self._progress < 0 then
        begin
          result := 0;
          exit
        end;
      // index not yet loaded
      if self._progress >= length(self._streams) then
        begin
          result := 100;
          exit
        end;
      result := (1 + (1 + self._progress) * 98  div (1 + length(self._streams)));
      exit;
    end;


  ////
  /// <summary>
  ///   Loads the the next block of measures from the dataLogger, and updates
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
  function TYDataSet.loadMore():LongInt;
    var
      url : string;
      stream : TYDataStream;
    begin
      if self._progress < 0 then
        begin
          url := 'logger.json?id='+self._functionId
        end
      else
        begin
          if self._progress >= length(self._streams) then
            begin
              result := 100;
              exit
            end
          else
            begin
              stream := self._streams[self._progress];
              url := stream.get_url()
            end;
        end;
      result := self.processMore(self._progress, self._parent._download(url));
      exit;
    end;


  ////
  /// <summary>
  ///   Returns an YMeasure object which summarizes the whole
  ///   DataSet.
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
  ///   an YMeasure object
  /// </returns>
  ///-
  function TYDataSet.get_summary():TYMeasure;
    begin
      result := self._summary;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a condensed version of the measures that can
  ///   retrieved in this YDataSet, as a list of YMeasure
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
  function TYDataSet.get_preview():TYMeasureArray;
    begin
      result := self._preview;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns all measured values currently available for this DataSet,
  ///   as a list of YMeasure objects.
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
  function TYDataSet.get_measures():TYMeasureArray;
    begin
      result := self._measures;
      exit;
    end;


//--- (end of generated code: YDataSet implementation)







//--- (generated code: Function functions)

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

//--- (end of generated code: Function functions)



//--- (generated code: Module functions)

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

//--- (end of generated code: Module functions)

//--- (generated code: DataStream functions)

  procedure _DataStreamCleanup();
    begin
    end;

//--- (end of generated code: DataStream functions)
//--- (generated code: Measure functions)

  procedure _MeasureCleanup();
    begin
    end;

//--- (end of generated code: Measure functions)
//--- (generated code: DataSet functions)

  procedure _DataSetCleanup();
    begin
    end;

//--- (end of generated code: DataSet functions)

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
  //--- (generated code: Module initialization)
  //--- (end of generated code: Module initialization)
  //--- (generated code: DataStream initialization)
  //--- (end of generated code: DataStream initialization)
  //--- (generated code: Measure initialization)
  //--- (end of generated code: Measure initialization)
  //--- (generated code: DataSet initialization)
  //--- (end of generated code: DataSet initialization)

  _cache                := TStringList.create();
  _cache.sorted         := true;
  _FunctionCallbacks    := TList.create();
  _TimedReportCallbackList := TList.create();
  _PlugEvents           := TList.create;
  _DataEvents           := TList.create;
  _CalibHandlers        := TStringList.create();
  _CalibHandlers.sorted := true;

finalization
  //--- (generated code: Module cleanup)
  _ModuleCleanup();
  //--- (end of generated code: Module cleanup)
  //--- (generated code: DataStream cleanup)
  //--- (end of generated code: DataStream cleanup)
  //--- (generated code: Measure cleanup)
  //--- (end of generated code: Measure cleanup)
  //--- (generated code: DataSet cleanup)
  //--- (end of generated code: DataSet cleanup)
  //--- (generated code: Sensor cleanup)
  _SensorCleanup();
  //--- (end of generated code: Sensor cleanup)
  //--- (generated code: Function cleanup)
  _FunctionCleanup();
  //--- (end of generated code: Function cleanup)
  _FunctionCallbacks.free();
  _TimedReportCallbackList.free();
  devicesCleanUp();
  queuesCleanUp();
  handlersCleanUp();

end.

