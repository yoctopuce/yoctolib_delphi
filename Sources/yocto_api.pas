(*********************************************************************
 *
 * $Id: yocto_api.pas 10744 2013-03-27 17:17:42Z martinm $
 *
 * High-level programming interface, common to all modules
 *
 * - - - - - - - - - License information: - - - - - - - - -
 *
 * Copyright (C) 2011 and beyond by Yoctopuce Sarl, Switzerland.
 *
 * 1) If you have obtained this file from www.yoctopuce.com,
 *    Yoctopuce Sarl licenses to you (hereafter Licensee) the
 *    right to use, modify, copy, and integrate this source file
 *    into your own solution for the sole purpose of interfacing
 *    a Yoctopuce product with Licensee's solution.
 *
 *    The use of this file and all relationship between Yoctopuce
 *    and Licensee are governed by Yoctopuce General Terms and
 *    Conditions.
 *
 *    THE SOFTWARE AND DOCUMENTATION ARE PROVIDED "AS IS" WITHOUT
 *    WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING
 *    WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY, FITNESS
 *    FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO
 *    EVENT SHALL LICENSOR BE LIABLE FOR ANY INCIDENTAL, SPECIAL,
 *    INDIRECT OR CONSEQUENTIAL DAMAGES, LOST PROFITS OR LOST DATA,
 *    COST OF PROCUREMENT OF SUBSTITUTE GOODS, TECHNOLOGY OR
 *    SERVICES, ANY CLAIMS BY THIRD PARTIES (INCLUDING BUT NOT
 *    LIMITED TO ANY DEFENSE THEREOF), ANY CLAIMS FOR INDEMNITY OR
 *    CONTRIBUTION, OR OTHER SIMILAR COSTS, WHETHER ASSERTED ON THE
 *    BASIS OF CONTRACT, TORT (INCLUDING NEGLIGENCE), BREACH OF
 *    WARRANTY, OR OTHERWISE.
 *
 * 2) If your intent is not to interface with Yoctopuce products,
 *    you are not entitled to use, read or create any derived
 *    material from this source file.
 *
 *********************************************************************)
unit yocto_api;

interface

uses
   sysutils,classes,windows,winsock,Math,yjson;

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

   TBYTEARRAY = array of byte;


   floatArr = array  of double;
   intArr   = array  of integer;
   pIntArr  = ^intArr;

   yCalibrationHandler = function (rawValue:double; calibType: integer; params : intArr; rawValues,refValues:floatArr ):double;

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
   YAPI_INVALID_LONGINT    = longint($80000000);
   YAPI_INVALID_LONGWORD   = longword($ffffffff);

   Y_HARDWAREID_INVALID   =  YAPI_INVALID_STRING;
   Y_FUNCTIONID_INVALID   =  YAPI_INVALID_STRING;
   Y_FRIENDLYNAME_INVALID =  YAPI_INVALID_STRING;

   // yInitAPI argument
   Y_DETECT_NONE   = 0;
   Y_DETECT_USB    = 1;
   Y_DETECT_NET    = 2;
   Y_DETECT_ALL : integer = (Y_DETECT_USB or Y_DETECT_NET);

   YOCTO_API_VERSION_STR     = '1.01';
   YOCTO_API_VERSION_BCD     = $0101;
   YOCTO_API_BUILD_NO        = '11167';
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

   INVALID_YHANDLE   =   0;

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
   raw : array[0..7] of u8;
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

//--- (generated code: YModule definitions)

const
// Yoctopuce error codes, also used by default as function return value
   YAPI_SUCCESS                   = 0;       // everything worked allright
   YAPI_NOT_INITIALIZED           = -1;      // call yInitAPI() first !
   YAPI_INVALID_ARGUMENT          = -2;      // one of the arguments passed to the function is invalid
   YAPI_NOT_SUPPORTED             = -3;      // the operation attempted is (currently) not supported
   YAPI_DEVICE_NOT_FOUND          = -4;      // the requested device is not reachable
   YAPI_VERSION_MISMATCH          = -5;      // the device firmware is incompatible with this API version
   YAPI_DEVICE_BUSY               = -6;      // the device is busy with another task and cannot answer
   YAPI_TIMEOUT                   = -7;      // the device took too long to provide an answer
   YAPI_IO_ERROR                  = -8;      // there was an I/O problem while talking to the device
   YAPI_NO_MORE_DATA              = -9;      // there is no more data to read from
   YAPI_EXHAUSTED                 = -10;     // you have run out of a limited ressource, check the documentation
   YAPI_DOUBLE_ACCES              = -11;     // you have two process that try to acces to the same device
   YAPI_UNAUTHORIZED              = -12;     // unauthorized access to password-protected device

   Y_PRODUCTNAME_INVALID           = YAPI_INVALID_STRING;
   Y_SERIALNUMBER_INVALID          = YAPI_INVALID_STRING;
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_PRODUCTID_INVALID             = -1;
   Y_PRODUCTRELEASE_INVALID        = -1;
   Y_FIRMWARERELEASE_INVALID       = YAPI_INVALID_STRING;
   Y_PERSISTENTSETTINGS_LOADED = 0;
   Y_PERSISTENTSETTINGS_SAVED = 1;
   Y_PERSISTENTSETTINGS_MODIFIED = 2;
   Y_PERSISTENTSETTINGS_INVALID = -1;

   Y_LUMINOSITY_INVALID            = -1;
   Y_BEACON_OFF = 0;
   Y_BEACON_ON = 1;
   Y_BEACON_INVALID = -1;

   Y_UPTIME_INVALID                = YAPI_INVALID_LONGWORD;
   Y_USBCURRENT_INVALID            = YAPI_INVALID_LONGWORD;
   Y_REBOOTCOUNTDOWN_INVALID       = YAPI_INVALID_LONGINT;
   Y_USBBANDWIDTH_SIMPLE = 0;
   Y_USBBANDWIDTH_DOUBLE = 1;
   Y_USBBANDWIDTH_INVALID = -1;



//--- (end of generated code: YModule definitions)


type
TYModule = class;
TYDevice = class;
TYFunction = class;


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
  function  HTTPRequestPrepare(request:string; var fullrequest:string; var errmsg:string):YRETCODE;
  function  HTTPRequestAsync(request :string; var errmsg:string):YRETCODE;
  function  HTTPRequest(request :string ; var buffer : ansistring; var   errmsg:string) : YRETCODE;
  function  requestAPI(var apires:TJsonParser;var errmsg:string):YRETCODE;
  function getFunctions(var functions:tlist; var errmsg:string):YRETCODE;
  destructor  Destroy();override;




end;





  TGenericUpdateCallback  = procedure(func: TYfunction; value:string);




(**
 * TYFunction Class (virtual class, used internally)
 *
 * This is the parent class for all public objects representing device functions documented in
 * the high-level programming API. This abstract class does all the real job, but without
 * knowledge of the specific function attributes.
 *
 * Instantiating a child class of YFunction does not cause any communication.
 * The instance simply keeps track of its function identifier, and will dynamically bind
 * to a matching device at the time it is really beeing used to read or set an attribute.
 * In order to allow true hot-plug replacement of one device by another, the binding stay
 * dynamic through the life of the object.
 *
 * The YFunction class implements a generic high-level cache for the attribute values of
 * the specified function, pre-parsed from the REST API string.
 *)
TYFunction  = class(Tobject)
protected
  _className       : string;
  _func            : string;
  _lastErrorType   : YRETCODE ;
  _lastErrorMsg    : string ;
  _cacheExpiration : u64;
  _fundescr        : YFUN_DESCR;
  _userdata        : Tobject;
  // Action<T,T>   _callback;
  _genCallback     : TGenericUpdateCallback;

  procedure registerFuncCallback(func : TYFunction);
  procedure unregisterFuncCallback(func : TYFunction);


  // Constructor is protected. Use the device-specific factory function to instantiate
  Constructor Create(classname:string;func:string); overload;



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

  function  _upload(path:string;  content:ansistring):integer;   overload;
  function  _upload(path:string;  content:array of byte):integer;  overload;

  function  _download(path:string) :TBYTEARRAY;
  function  _request(request:ansistring) :ansistring;
  function  _json_get_array(data: array of byte):TSTRINGARRAY;
  function  _json_get_key(data: array of byte;key:string):string;

  function _parse(parser:PJSONRECORD):integer;virtual;  abstract;

  function  _buildSetRequest( changeattr : string ; changeval:string ; var request:string; var errmsg:string):YRETCODE;

public
  ////
  /// <summary>
  ///   Returns a short text that describes the function in the form <c>TYPE(NAME)=SERIAL&#46;FUNCTIONID</c>.
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
  function     get_hardwareId():string;

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
  function     get_functionId():string;

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


  ////
  /// <summary>
  ///   Returns the numerical error code of the latest error with this function.
  /// <para>
  ///   This method is mostly useful when using the Yoctopuce library with
  ///   exceptions disabled.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a number corresponding to the code of the latest error that occured while
  ///   using this function object
  /// </returns>
  ///-
  function get_errorType():YRETCODE;
  function errorType():YRETCODE;
  function errType():YRETCODE;

  ////
  /// <summary>
  ///   Returns the error message of the latest error with this function.
  /// <para>
  ///   This method is mostly useful when using the Yoctopuce library with
  ///   exceptions disabled.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a string corresponding to the latest error message that occured while
  ///   using this function object
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
  ///   device hosting the requested function.
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
 procedure registerValueCallback(callback : TGenericUpdateCallback);

  ////
  ///
  ///-
 procedure  advertiseValue(value:string);virtual;


  ////
  ///
  ///-
{$Warnings OFF}
   // disable the override warning: we cannot use the override directive
   // because Tobject.ToString does not exists in all delphi versions.
   function ToString():string;
end;
{$Warnings ON}

//--- (generated code: YModule declaration)
 TUpdateCallback  = procedure(func: TYModule; value:string);
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
protected
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
   _upTime                   : LongWord;
   _usbCurrent               : LongWord;
   _rebootCountdown          : LongInt;
   _usbBandwidth             : Integer;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of generated code: YModule declaration)

   // Return the properties of the nth function of our device
   function _getFunction(idx:integer; var serial,funcId,funcName,funcVal,errMsg:string):YRETCODE;





public

   constructor Create(func:string);
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

   //--- (generated code: YModule accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
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
   ///   Returns the logical name of the module.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the module
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the module.
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
   ///   a string corresponding to the logical name of the module
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
   ///   Saves current settings in the nonvolatile memory of the module.
   /// <para>
   ///   Warning: the number of allowed save operations during a module life is
   ///   limited (about 100000 cycles). Do not call this function within a loop.
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function saveToFlash():integer;

   ////
   /// <summary>
   ///   Reloads the settings stored in the nonvolatile memory, as
   ///   when the module is powered on.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   <c>YAPI_SUCCESS</c> if the call succeeds.
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns a negative error code.
   /// </para>
   ///-
   function revertFromFlash():integer;

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
   function get_upTime():LongWord;

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
   function get_usbCurrent():LongWord;

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
   ///   Schedules a simple module reboot after the given number of seconds.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="secBeforeReboot">
   ///   number of seconds before rebooting
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
   function reboot(secBeforeReboot:integer):integer;

   ////
   /// <summary>
   ///   Schedules a module reboot into special firmware update mode.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="secBeforeReboot">
   ///   number of seconds before rebooting
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
   function triggerFirmwareUpdate(secBeforeReboot:integer):integer;

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
   function download(pathname:string):TBYTEARRAY;

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
   function get_icon2d():TBYTEARRAY;

   //--- (end of generated code: YModule accessors declaration)

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
  yFunctionUpdateFunc = procedure (module:TYModule;functionId,functionName,functionValue:string);


 ////
 /// <summary>
 ///   Registers a log callback function.
 /// <para>
 ///   This callback will be called each time
 ///   the API have something to say. Quite usefull to debug the API.
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
 ///   a device is pluged.
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
 ///   a device is unpluged.
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
 function  yLinearCalibrationHandler(rawValue:double; calibType: integer; params : intArr; rawValues,refValues:floatArr):double;

////
/// <summary>
///   Setup the Yoctopuce library to use modules connected on a given machine.
/// <para>
///   When using Yoctopuce modules through the VirtualHub gateway,
///   you should provide as parameter the address of the machine on which the
///   VirtualHub software is running (typically <c>"http://127.0.0.1:4444"</c>,
///   which represents the local machine).
///   When you use a language which has direct access to the USB hardware,
///   you can use the pseudo-URL <c>"usb"</c> instead.
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
///   If acces control has been activated on the VirtualHub you want to
///   reach, the URL parameter should look like:
/// </para>
/// <para>
///   <c>http://username:password@adresse:port</c>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="url">
///   a string containing either <c>"usb"</c> or the
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
/// 
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
///   consume CPU cycles significatively. The processor is left available for
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
///   Yoctopuce devices, which also uses the milisecond as timebase.
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
function _decimalToDouble(val:u64):double;
function _doubleToDecimal(val:double):u64;
function _encodeCalibrationPoints(rawValues,refValues:floatArr;resolution:double; calibrationOffset:integer; actualCparams:string):string;
function _decodeCalibrationPoints(calibParams:string; intPt: pIntArr; var rawPt:floatArr; var calPt:floatArr;resolution:double;calibOffset:integer):integer;
function _applyCalibration(rawValue:double; calibParams:string; calibOffset:integer; resolution:double  ):double;

implementation

var
   _FunctionCache : TList;
   _FunctionCallbacks : Tlist;
   _CalibHandlers : TstringList;

constructor  YAPI_Exception.Create(errType:YRETCODE;  errMsg:string );
 begin
  inherited create(errMsg);
 end;

type
  _yapiLogFunc            = procedure (log:pansichar;loglen:u32);cdecl;
  _yapiDeviceUpdateFunc   = procedure (dev:YDEV_DESCR);cdecl;
  _yapiFunctionUpdateFunc = procedure (func:YFUN_DESCR; value: pansichar);cdecl;

var
  ylog:     yLogFunc;
  yArrival: yDeviceUpdateFunc;
  yRemoval: yDeviceUpdateFunc;
  yChange:  yDeviceUpdateFunc;


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
procedure _yapiRegisterDeviceArrivalCallback(fct:_yDeviceUpdateFunc); cdecl; external ;
procedure _yapiRegisterDeviceRemovalCallback(fct:_yDeviceUpdateFunc); cdecl; external;
procedure _yapiRegisterDeviceChangeCallback(fct:_yDeviceUpdateFunc); cdecl; external;
procedure _yapiRegisterFunctionUpdateCallback(fct:_yFunctionUpdateFunc); cdecl; external ;
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
procedure _yapiRegisterDeviceArrivalCallback(fct:_yapiDeviceUpdateFunc); cdecl; external dllfile name 'yapiRegisterDeviceArrivalCallback';
procedure _yapiRegisterDeviceRemovalCallback(fct:_yapiDeviceUpdateFunc); cdecl; external dllfile name 'yapiRegisterDeviceRemovalCallback';
procedure _yapiRegisterDeviceChangeCallback(fct:_yapiDeviceUpdateFunc); cdecl; external dllfile name 'yapiRegisterDeviceChangeCallback';
procedure _yapiRegisterFunctionUpdateCallback(fct:_yapiFunctionUpdateFunc); cdecl; external dllfile name 'yapiRegisterFunctionUpdateCallback';
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


procedure yRegisterLogFunction(logfun: yLogFunc);
  begin
   ylog := logfun;
  end;

type

  TyapiEventType = (YAPI_DEV_ARRIVAL,YAPI_DEV_REMOVAL,YAPI_DEV_CHANGE,
    YAPI_FUN_UPDATE,YAPI_FUN_VALUE) ;

  TyapiEvent = record
    eventtype: TyapiEventType;
    module   : TYmodule;
    fun_descr: YFUN_DESCR;
    value    : string[YOCTO_PUBVAL_LEN];
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



procedure native_yFunctionUpdateCallback (f:YFUN_DESCR; data :pansichar);cdecl;
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


procedure yRegisterCalibrationHandler(calibType:integer;callback:yCalibrationHandler);
  var
   key   : string;
  begin
   key := intToStr(calibType);
   _calibhandlers.addObject(key, addr(callback));
 end;


 //  standard value calibration handler (n-points linear error correction).
function  yLinearCalibrationHandler(rawValue:double; calibType: integer; params : intArr; rawValues,refValues:floatArr):double;
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



function _encodeCalibrationPoints(rawValues,refValues:floatArr;resolution:double; calibrationOffset:integer; actualCparams:string):string;
  var
  calibType,i,nPt,rawVal,refVal,minRaw: integer;
  res: string;
  begin
    minRaw:=0;
    res:='';
    npt:= min(high(rawValues),high(refValues))+1;
    if(npt=0) then
      begin
        _encodeCalibrationPoints:='';
        exit;
      end;
    if(actualCparams='') then
      calibType := 10+nPt
    else
      begin
        i := pos(',',actualCparams);
        if (i<=0)  then calibType := 0 else  calibType := strToInt(copy(actualCparams,1,i-1));
        if (calibType <= 10) then
          calibType := npt
        else
          calibType := 10+npt;
      end;
    res := intToStr(calibType) ;
    if(calibType <=10) then
      begin
        for i:=0 to npt-1 do
          begin
            rawval := round(rawValues[i] /  resolution - calibrationOffset);
            if    ((rawval>=minRaw) and (rawval<65536)) then
              begin
                refval := round(refValues[i] /  resolution - calibrationOffset);
                if ((refVal>=0) and (refVal<65536)) then
                  begin
                    res:=res+','+intToStr(rawVal)+','+intToStr(refVal);
                    minRaw := rawVal+1;
                  end
              end;
          end;
            
      end
    else
      begin
        for i:=0 to npt-1 do
          begin
            rawVal := _doubleToDecimal(rawValues[i]);
            refval := _doubleToDecimal(refValues[i]);
            res:=res+','+intToStr(rawVal)+','+intToStr(refVal);
          end;
      end;
    _encodeCalibrationPoints := res;
  end;



function _decodeCalibrationPoints(calibParams:string; intPt: pIntArr; var rawPt:floatArr; var calPt:floatArr;resolution:double;calibOffset:integer):integer;
  var
    caltype:integer;
    p,last_p,cnt,intval:integer;
    nval:integer;
    dval:double;
  begin
    if(calibParams='')then
      begin
        _decodeCalibrationPoints:=0;
        exit;
      end;

    p:=pos(',',calibParams);
    if (p<=0) then
      begin
        _decodeCalibrationPoints:=0;
        exit;
      end;
    calType := strToInt(copy(calibParams,1,p-1));
    nval:=99;
    if(calType<20) then
      nval := 2*(calType mod 10);
    if(intPt<>nil) then
      setLength(intPt^,nval);
    setLength(rawPt,nval shr 1);
    setLength(calPt,nval shr 1);
    inc(p);
    last_p:=p;
    cnt:=0;
    calibParams:=calibParams+',';
    while( (p<=length(calibParams)) and (calibParams[p]<>',')) do
        inc(p);
    while ((p<=length(calibParams)) and (cnt < nval)) do
      begin
        intval:= strToInt(copy(calibParams,last_p,p-last_p));
        if (calType <= 10) then
          dval :=  (intval + calibOffset) * resolution
        else
          dval := _decimalToDouble(intval);
        if(intPt<>nil) then
          intPt^[cnt]:=intval;
        if (cnt mod 2)= 0 then
          rawPt[cnt div 2 ] :=  dval
        else
          calPt[cnt div 2 ] :=  dval;
        inc(cnt);
        // look for next mumber
        inc(p);
        last_p:=p;
        while( (p<=length(calibParams)) and (calibParams[p]<>',')) do
          inc(p);
      end;
    _decodeCalibrationPoints:= calType;
  end;

function _applyCalibration(rawValue:double; calibParams:string; calibOffset:integer; resolution:double ):double;
  var
    calType:integer;
    iparams:intArr;
    rawPt,calPt:floatArr;
    calhdl : yCalibrationHandler;

  begin
    if  (rawValue = YAPI_INVALID_DOUBLE) or (resolution = YAPI_INVALID_DOUBLE)  then
      begin
        _applyCalibration:=YAPI_INVALID_DOUBLE;
        exit;
      end;
    calType := _decodeCalibrationPoints(calibParams,addr(iparams),rawPt,calPt,resolution,calibOffset);
    if(calType=0) then
      begin
        _applyCalibration := rawValue;
        exit;
      end;
    calhdl := _getCalibrationHandler(calType);
    if (addr(calhdl)=nil) then
      begin
        _applyCalibration:=YAPI_INVALID_DOUBLE;
        exit;
      end;
    _applyCalibration:=calhdl(rawValue,caltype,iparams,rawPt,calPt);
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
   _yapiRegisterDeviceArrivalCallback( native_yDeviceArrivalCallback);
   _yapiRegisterDeviceRemovalCallback( native_yDeviceRemovalCallback);
   _yapiRegisterDeviceChangeCallback( native_yDeviceChangeCallback);
   _yapiRegisterFunctionUpdateCallback( native_yFunctionUpdateCallback);
   _yapiRegisterLogFunction(native_yLogFunction);
 
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

        end;
       freemem(p);
     end;

    yUpdateDeviceList:=YAPI_SUCCESS;
  end;


 function yHandleEvents(var errmsg:string):yretcode;
  var
     errBuffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
     pError :pansichar;
     res:yretcode;
     p : PyapiEvent;
     i :integer;

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
           For i := 0 To _FunctionCallbacks.Count - 1 do
             If (TYfunction(_FunctionCallbacks.items[i]).get_functionDescriptor() = p^.fun_descr) Then
               TYfunction(_FunctionCallbacks.items[i]).advertiseValue(string(p^.value));
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


 function yGetTickCount():u64;
  begin
    yGetTickCount := _yapiGetTickCount();
  end;

  
 function yCheckLogicalName(name:string):boolean;
  begin
    if  (_yapiCheckLogicalName(pansichar(ansistring(name)))=0)  then yCheckLogicalName:=false
    else yCheckLogicalName:=true;
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





 function yapiHTTPRequestSync(device:string; request:string; var reply:ansistring; var errmsg:string):YRETCODE;
   var
    iohdl  : YIOHDL;
    buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
    perror,preply: pansichar;

    replysize : integer;
    res    : YRETCODE;

  begin
    buffer[0]:=#0;perror:=@buffer;
    res := _yapiHTTPRequestSyncStartEx(addr(iohdl),pansichar(ansistring(device)),
                                       pansichar(ansistring(request)),
                                       length(request),
                                       preply,replysize,perror);
    if(res<0) then
    begin
       errmsg:=string(perror);
       yapiHTTPRequestSync := res;
       exit;
    end;

    setlength( reply,replysize);
    move(preply^,reply[1],replysize);

    res := _yapiHTTPRequestSyncDone(addr(iohdl),perror);
    errmsg := string(perror);
    yapiHTTPRequestSync := res;
 end;

 function yapiHTTPRequestAsync(device:string; request:string; var errmsg:string):YRETCODE;
   var
    buffer : array[0..YOCTO_ERRMSG_LEN] of ansichar;
    perror : pansichar;
    res    : YRETCODE;

  begin
    buffer[0]:=#0;perror:=@buffer;
    res := _yapiHTTPRequestAsync(pansichar(ansistring(device)),pansichar(ansistring(request)),nil,nil,perror);
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




constructor TYFunction.Create(classname:string;func:string);
 begin
    _className       :=  classname;
    _func            :=  func;
    _lastErrorType   :=  YAPI_SUCCESS;
    _lastErrorMsg    :=  '';
    _cacheExpiration :=  0;
    _fundescr        := Y_FUNCTIONDESCRIPTOR_INVALID;
    _userData        := nil;
    _genCallback     := nil;
    _FunctionCache.Add(self);

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
             end else  uchangeval:=uchangeval+c;
           end;
      end;

     request := request+ uchangeval+' '#13#10#13#10;     // no HTTP/1.1 to get light headers

     _buildSetRequest:= YAPI_SUCCESS;
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
function  TYFunction._request(request:ansistring):ansistring;
  var
    dev: TYDevice;
    errmsg: string;
     buffer :ansistring;
    res :integer;
     errBuffer       : array[0..YOCTO_ERRMSG_LEN] of ansichar;
    perror          : pansichar;
  begin
   // Resolve our reference to our device, load REST API
    res := _getDevice(dev, errmsg);
    if(YISERR(res)) then
      begin
        _throw(res, errmsg);
        result := YAPI_INVALID_STRING;
        exit;
      end;
    res := dev.HTTPRequest(string(request), buffer, errmsg);
    if(YISERR(res))  then
       begin
        // Check if an update of the device list does notb solve the issue
        errBuffer[0]:=#0;perror:=@errBuffer;
        res := _yapiUpdateDeviceList(1,perror);

        if(YISERR(res)) then
           begin
            errmsg:=string(perror);
            _throw(res,errmsg);
            result := YAPI_INVALID_STRING;
            exit;
           end;
        res := dev.HTTPRequest(string(request), buffer, errmsg);
        if(YISERR(res)) then
           begin
            _throw(res,errmsg);
            result :=  YAPI_INVALID_STRING;
          end;
       end;
    if(pos(ansistring('OK'#13#10),buffer)<=0) then
        if( pos(ansistring('HTTP/1.1 200 OK'#13#10),buffer)<=0) then
          begin
            _throw(YAPI_IO_ERROR,'http request failed');
            result := YAPI_INVALID_STRING;
            exit;
          end;

   result :=buffer;
   end;


// Method used to send http request to the device (not the function)
function TYFunction._download(path:string):TBYTEARRAY;
 var
    request,buffer:ansistring;
    found,i,j:integer;
    res:TBYTEARRAY;
 begin
    request := 'GET /'+ansistring(path)+' HTTP/1.1'#13#10#13#10;
    buffer := _request(request);
    found := pos(ansistring(#13#10#13#10),buffer);
    if(found<=0) then
      begin
        _throw(YAPI_IO_ERROR,'http request failed');
        result :=nil;
        exit;
      end;
    inc(found,4);
    setLength(res,length(buffer)-found+1);
    j:=0;
    for i:=found to length(buffer) do
     begin
       res[j] := ord(buffer[i]);
       inc(j);
     end;
    result := res;
 end;


// Method used to upload a file to the device
function  TYFunction._upload(path:string;  content:ansistring):integer;
  var
     request,buffer:ansistring;
     boundary,body:ansistring;
     found :integer;
  begin

    request := ansistring('POST /upload.html HTTP/1.1'#13#10);
    body :=   ansistring('Content-Disposition: form-data; name="')+
              ansistring(path)+ansistring('"; filename="api"'#13#10)+
             ansistring('Content-Type: application/octet-stream'#13#10)+
             ansistring('Content-Transfer-Encoding: binary'#13#10#13#10)+content;
    repeat
        boundary := 'Zz'+ansistring(inttostr(trunc(random(899999))+100000))+'zZ';
    until pos(boundary ,body)<=0;

    request := request+ 'Content-Type: multipart/form-data; boundary='+boundary+#13#10;
    request := request+  #13#10'--'+boundary+#13#10+body+#13#10'--'+boundary+'--'#13#10;
    buffer  := _request(request);
    found := pos(#13#10,string(buffer));
    if (found<=0) then
     begin
        _throw(YAPI_IO_ERROR,'http request failed');
        result := YAPI_IO_ERROR;
        exit;
     end;
    result := YAPI_SUCCESS;
end;

function  TYFunction._upload(path:string;  content:array of byte):integer;
 var strcontent : ansistring;
   i:integer;
 begin
  strcontent:='';
  setlength( strcontent, length(content));
  for i:=0  to  length(content)-1 do strcontent[i+1]:=ansichar(chr(content[i]));
  result :=  _upload(path,  strcontent);
 end;





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

    result :=true;
 end;

function   TYFunction._json_get_key(data: array of byte;key:string):string;
   var
    node  : PJSONRECORD;
    st    : string;
    p     : TJSONparser;
    size,i : integer;

  begin
    size:=length(data);
    setlength(st,size);
    for I:=0 to size-1 do
      st[i+1]:=chr(data[i]);
    p := TJSONparser.create(st,false);

    node := p.GetChildNode(nil,key);
     p.free();
    _json_get_key := string(node^.svalue);

  end;

function  TYFunction._json_get_array(data: array of byte):TSTRINGARRAY;
  var
    st  : string;
    p   : TJSONparser;
    res : TSTRINGARRAY;
    size,i:integer;
  begin
    size:=length(data);
    setlength(st,size);
    for I:=0 to size-1 do
      st[i+1]:=chr(data[i]);
    p := TJSONparser.create(st,false);
    res:=p.GetAllChilds(nil);
    p.free();
    _json_get_array := res;
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

    node:=apires.getChildNode(nil,funcId);
    if (node=nil) then
     begin
        _throw(YAPI_IO_ERROR,'unexpected JSON structure: missing function '+funcId);
         result:=YAPI_IO_ERROR;
         exit;
     end;

    _parse(node);
    _cacheExpiration := yGetTickCount() + msValidity;

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

  procedure TYFunction.registerFuncCallback(func : TYFunction);
   begin
     isOnline();
     If  _FunctionCallbacks.IndexOf(self)<0 Then
       _FunctionCallbacks.Add(self)

   end;

  procedure TYFunction.unregisterFuncCallback(func : TYFunction);
    begin
      _FunctionCallbacks.Remove(self);
    end;

 (**
   *
   *
   *
   *)
  procedure TYFunction.registerValueCallback(callback : TGenericUpdateCallback);
   begin
    if assigned(callback) then
        registerFuncCallback(self)
      Else
        unregisterFuncCallback(self);
    _genCallback := callback;

   end;

  (**
   *
   *
   *
   *)
  procedure  TYFunction.advertiseValue(value:string);
  begin
    if assigned(_genCallback) then _genCallback(self,value);
  end;


procedure _FunctionCleanup();
  var i:integer;
begin
  for i:=0 to _Functioncache.count-1 do
    _Functioncache.items[i]:=nil;   // freeed at module cleanup
  _Functioncache.free();
  _Functioncache:=nil;

  for i:=0 to _FunctionCallbacks.count-1 do
     _FunctionCallbacks.items[i]:=nil;   // those are callbacks
  _FunctionCallbacks.free();
  _FunctionCallbacks:=nil;

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

destructor  TYDevice.destroy();
 begin

  _functions.free();
  _functions:=nil;
  if assigned(_cacheJson) then _cacheJson.free();
  _cacheJson:=nil;
  inherited destroy();
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

function  TYDevice.HTTPRequestPrepare(request:string; var fullrequest:string; var errmsg:string):YRETCODE;
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

  if(not(_subpathinit)) then
   begin
     res := _yapiGetDevicePath(_devdescr,proot, NIL, 0, neededsize, perrbuf);
     if(YISERR(res))  then
       begin
         errmsg := string(perrbuf);
         HTTPRequestPrepare := res;
         exit;
       end;

      getmem(b,neededsize);
      res := _yapiGetDevicePath(_devdescr,proot, b, neededsize, tmp, perrbuf);
      if(YISERR(res))  then
       begin
         freemem(b);
         errmsg := string(perrbuf);
         HTTPRequestPrepare := res;
         exit;
       end;

      _rootdevice:= string(proot);
      _subpath:=string(b);
      freemem(b);
      _subpathinit:=true;
   end;

   p := pos('/',request);
   fullrequest:=copy(request,1,p-1)+_subpath+copy(request,p+1,length(request)-p);
   HTTPRequestPrepare := YAPI_SUCCESS;
end;

function  TYDevice.HTTPRequestAsync(request :string; var errmsg:string):YRETCODE;
 var
   res         : YRETCODE;
   fullrequest : string;

begin
   res := HTTPRequestPrepare(request, fullrequest, errmsg);
    if(YISERR(res) ) then
      begin
        HTTPRequestAsync := res;
        exit;
      end;
   HTTPRequestAsync := yapiHTTPRequestAsync(_rootdevice,fullrequest,errmsg);
end;


function TYDevice.HTTPRequest(request :string ; var buffer :ansistring; var errmsg:string):YRETCODE;
 var
   res         : YRETCODE;
   fullrequest : string;

 begin
   res := HTTPRequestPrepare(request, fullrequest, errmsg);
    if(YISERR(res) ) then
      begin
        HTTPRequest := res;
        exit;
      end;
   HTTPRequest := yapiHTTPRequestSync(_rootdevice,fullrequest,buffer,errmsg);
 end;



function TYDevice.requestAPI(var  apires:TJsonParser;var errmsg:string) :YRETCODE;
 var
    j               : TJsonParser;
    buffer          : ansistring;
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
    j:= TJsonParser.create(string(buffer));
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

//--- (generated code: YModule implementation)

var
   _ModuleCache : TStringList;

constructor TYModule.Create(func:string);
 begin
   inherited Create('Module', func);
   _productName := Y_PRODUCTNAME_INVALID;
   _serialNumber := Y_SERIALNUMBER_INVALID;
   _logicalName := Y_LOGICALNAME_INVALID;
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
 end;

{$HINTS OFF}
function TYModule._parse(j:PJSONRECORD):integer;
 var
   member,sub : PJSONRECORD;
   i,l        : integer;
 begin
   if (j^.recordtype <> JSON_STRUCT) then begin _parse:= -1; exit; end;
   for i:=0 to j^.membercount-1 do
    begin
      member := j^.members[i];
      if (member^.name = 'productName') then
       begin
         _productName := string(member^.svalue);
       end else
      if (member^.name = 'serialNumber') then
       begin
         _serialNumber := string(member^.svalue);
       end else
      if (member^.name = 'logicalName') then
       begin
         _logicalName := string(member^.svalue);
       end else
      if (member^.name = 'productId') then
       begin
         _productId := member^.ivalue;
       end else
      if (member^.name = 'productRelease') then
       begin
         _productRelease := member^.ivalue;
       end else
      if (member^.name = 'firmwareRelease') then
       begin
         _firmwareRelease := string(member^.svalue);
       end else
      if (member^.name = 'persistentSettings') then
       begin
         _persistentSettings := member^.ivalue;
       end else
      if (member^.name = 'luminosity') then
       begin
         _luminosity := member^.ivalue;
       end else
      if (member^.name = 'beacon') then
       begin
         _beacon := member^.ivalue;
       end else
      if (member^.name = 'upTime') then
       begin
         _upTime := member^.ivalue;
       end else
      if (member^.name = 'usbCurrent') then
       begin
         _usbCurrent := member^.ivalue;
       end else
      if (member^.name = 'rebootCountdown') then
       begin
         _rebootCountdown := member^.ivalue;
       end else
      if (member^.name = 'usbBandwidth') then
       begin
         _usbBandwidth := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
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
   if (_productName = Y_PRODUCTNAME_INVALID) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PRODUCTNAME_INVALID;
         exit;
       end;
   result := _productName;
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
   if (_serialNumber = Y_SERIALNUMBER_INVALID) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_SERIALNUMBER_INVALID;
         exit;
       end;
   result := _serialNumber;
 end;

////
/// <summary>
///   Returns the logical name of the module.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the module
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYModule.get_logicalName():string;
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
///   Changes the logical name of the module.
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
///   a string corresponding to the logical name of the module
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
function TYModule.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
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
   if (_productId = Y_PRODUCTID_INVALID) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PRODUCTID_INVALID;
         exit;
       end;
   result := _productId;
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
   if (_productRelease = Y_PRODUCTRELEASE_INVALID) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PRODUCTRELEASE_INVALID;
         exit;
       end;
   result := _productRelease;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_FIRMWARERELEASE_INVALID;
         exit;
       end;
   result := _firmwareRelease;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_PERSISTENTSETTINGS_INVALID;
         exit;
       end;
   result := _persistentSettings;
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
///   Saves current settings in the nonvolatile memory of the module.
/// <para>
///   Warning: the number of allowed save operations during a module life is
///   limited (about 100000 cycles). Do not call this function within a loop.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYModule.saveToFlash():integer;
 var
   rest_val: string;
 begin
   rest_val := '1';
   result := _setAttr('persistentSettings', rest_val);
 end;

////
/// <summary>
///   Reloads the settings stored in the nonvolatile memory, as
///   when the module is powered on.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   YAPI_SUCCESS if the call succeeds.
/// </returns>
/// <para>
///   On failure, throws an exception or returns a negative error code.
/// </para>
///-
function TYModule.revertFromFlash():integer;
 var
   rest_val: string;
 begin
   rest_val := '0';
   result := _setAttr('persistentSettings', rest_val);
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_LUMINOSITY_INVALID;
         exit;
       end;
   result := _luminosity;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_BEACON_INVALID;
         exit;
       end;
   result := _beacon;
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
function TYModule.get_upTime():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_UPTIME_INVALID;
         exit;
       end;
   result := _upTime;
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
function TYModule.get_usbCurrent():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_USBCURRENT_INVALID;
         exit;
       end;
   result := _usbCurrent;
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_REBOOTCOUNTDOWN_INVALID;
         exit;
       end;
   result := _rebootCountdown;
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
///   Schedules a simple module reboot after the given number of seconds.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="secBeforeReboot">
///   number of seconds before rebooting
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
function TYModule.reboot(secBeforeReboot:integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(secBeforeReboot);
   result := _setAttr('rebootCountdown', rest_val);
 end;

////
/// <summary>
///   Schedules a module reboot into special firmware update mode.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <param name="secBeforeReboot">
///   number of seconds before rebooting
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
function TYModule.triggerFirmwareUpdate(secBeforeReboot:integer):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(-secBeforeReboot);
   result := _setAttr('rebootCountdown', rest_val);
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
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_USBBANDWIDTH_INVALID;
         exit;
       end;
   result := _usbBandwidth;
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
function TYModule.download(pathname:string):TBYTEARRAY;
     begin
        result:= self._download(pathname);
            
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
function TYModule.get_icon2d():TBYTEARRAY;
     begin
        result:= self._download('icon2d.png');
            
     end;


function TYModule.nextModule(): TYModule;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextModule := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextModule := nil;
      exit;
    end;
    nextModule := yFindModule(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYModule.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYModule.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYModule.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYModule.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

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



//--- (generated code: Module functions)

function yFindModule(func:string): TYModule;
 var
   index: integer;
   res  : TYModule;
 begin
    if (_ModuleCache.Find(func, index)) then
     begin
       yFindModule := TYModule(_ModuleCache.objects[index]);
       exit;
     end;
   res := TYModule.Create(func);
   _ModuleCache.addObject(func, res);
   yFindModule := res;
 end;

function yFirstModule(): TYModule;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('Module', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstModule := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstModule := nil;
       exit;
    end;
   yFirstModule := yFindModule(serial+'.'+funcId);
 end;

procedure _ModuleCleanup();
  var i:integer;
begin
  for i:=0 to _ModuleCache.count-1 do 
    begin
     _ModuleCache.objects[i].free();
     _ModuleCache.objects[i]:=nil;
    end;
   _ModuleCache.free();
   _ModuleCache:=nil;
end;

//--- (end of generated code: Module functions)

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
   _ModuleCache        := TstringList.create();
   _ModuleCache.sorted := true;
   //--- (end of generated code: Module initialization)
   _functionCache      := TList.create();
   _FunctionCallbacks  := TList.create();
   _PlugEvents         := Tlist.create;
   _DataEvents         := Tlist.create;
   _CalibHandlers        := TstringList.create();
   _CalibHandlers.sorted := true;


finalization
   //--- (generated code: Module cleanup)
   _ModuleCleanup();
   //--- (end of generated code: Module cleanup)
   _FunctionCleanUp();
   devicesCleanUp();
   queuesCleanUp();
   handlersCleanUp();

end.

