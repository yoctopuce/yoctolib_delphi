{*********************************************************************
 *
 * $Id: yocto_datalogger.pas 27276 2017-04-25 15:40:55Z seb $
 *
 * Implements yFindDataLogger(), the high-level API for DataLogger functions
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


unit yocto_datalogger;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

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
const Y_CLEARHISTORY_FALSE = 0;
const Y_CLEARHISTORY_TRUE = 1;
const Y_CLEARHISTORY_INVALID = -1;


//--- (end of generated code: YDataLogger definitions)

   Y_DATA_INVALID = YAPI_INVALID_DOUBLE;

type

  TYDataLoggerRawData = array of array of double;
  TYDataLogger = class;
  TYDataSetArray = array of TYDataSet;


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
  ///   TYDataLogger Class: DataLogger function interface
  /// <para>
  ///   Yoctopuce sensors include a non-volatile memory capable of storing ongoing measured
  ///   data automatically, without requiring a permanent connection to a computer.
  ///   The DataLogger function controls the global parameters of the internal data
  ///   logger.
  /// </para>
  /// </summary>
  ///-
  TYDataLogger=class(TYFunction)
  //--- (end of generated code: YDataLogger class start)
  protected
  //--- (generated code: YDataLogger declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _currentRunIndex          : LongInt;
    _timeUTC                  : int64;
    _recording                : Integer;
    _autoStart                : Integer;
    _beaconDriven             : Integer;
    _clearHistory             : Integer;
    _valueCallbackDataLogger  : TYDataLoggerValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YDataLogger declaration)
  protected
    _dataLoggerURL :string;

    function getData(runIdx: longword  ; timeIdx: longword; var jsondata:TJsonParser):integer;
  public

    ////
    /// <summary>
    ///   Builds a list of all data streams hold by the data logger (legacy method).
    /// <para>
    ///   The caller must pass by reference an empty array to hold YDataStream
    ///   objects, and the function fills it with objects describing available
    ///   data sequences.
    /// </para>
    /// <para>
    ///   This is the old way to retrieve data from the DataLogger.
    ///   For new applications, you should rather use <c>get_dataSets()</c>
    ///   method, or call directly <c>get_recordedData()</c> on the
    ///   sensor object.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="v">
    ///   an array of YDataStream objects to be filled in
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
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
    ///   On failure, throws an exception or returns <c>Y_CURRENTRUNINDEX_INVALID</c>.
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
    ///   On failure, throws an exception or returns <c>Y_TIMEUTC_INVALID</c>.
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
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
    ///   a value among <c>Y_RECORDING_OFF</c>, <c>Y_RECORDING_ON</c> and <c>Y_RECORDING_PENDING</c>
    ///   corresponding to the current activation state of the data logger
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_RECORDING_INVALID</c>.
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
    ///   a value among <c>Y_RECORDING_OFF</c>, <c>Y_RECORDING_ON</c> and <c>Y_RECORDING_PENDING</c>
    ///   corresponding to the activation state of the data logger to start/stop recording data
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
    ///   either <c>Y_AUTOSTART_OFF</c> or <c>Y_AUTOSTART_ON</c>, according to the default activation state
    ///   of the data logger on power up
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_AUTOSTART_INVALID</c>.
    /// </para>
    ///-
    function get_autoStart():Integer;

    ////
    /// <summary>
    ///   Changes the default activation state of the data logger on power up.
    /// <para>
    ///   Remember to call the <c>saveToFlash()</c> method of the module if the
    ///   modification must be kept.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="newval">
    ///   either <c>Y_AUTOSTART_OFF</c> or <c>Y_AUTOSTART_ON</c>, according to the default activation state
    ///   of the data logger on power up
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
    ///   either <c>Y_BEACONDRIVEN_OFF</c> or <c>Y_BEACONDRIVEN_ON</c>, according to true if the data logger
    ///   is synchronised with the localization beacon
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_BEACONDRIVEN_INVALID</c>.
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
    ///   either <c>Y_BEACONDRIVEN_OFF</c> or <c>Y_BEACONDRIVEN_ON</c>, according to the type of
    ///   synchronisation of the data logger
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
    function set_beaconDriven(newval:Integer):integer;

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
    /// </summary>
    /// <param name="func">
    ///   a string that uniquely characterizes $THEFUNCTION$
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
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function forgetAllDataStreams():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list of YDataSet objects that can be used to retrieve
    ///   all measures stored by the data logger.
    /// <para>
    /// </para>
    /// <para>
    ///   This function only works if the device uses a recent firmware,
    ///   as YDataSet objects are not supported by firmwares older than
    ///   version 13000.
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a list of YDataSet object.
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

//--- (generated code: DataLogger functions declaration)
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
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the data logger
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

//--- (end of generated code: DataLogger functions declaration)

implementation

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
      if (member^.name = 'clearHistory') then
        begin
          _clearHistory := member^.ivalue;
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

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
  ///   On failure, throws an exception or returns Y_CURRENTRUNINDEX_INVALID.
  /// </para>
  ///-
  function TYDataLogger.get_currentRunIndex():LongInt;
    var
      res : LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_CURRENTRUNINDEX_INVALID;
              exit;
            end;
        end;
      res := self._currentRunIndex;
      result := res;
      exit;
    end;


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
  ///   On failure, throws an exception or returns Y_TIMEUTC_INVALID.
  /// </para>
  ///-
  function TYDataLogger.get_timeUTC():int64;
    var
      res : int64;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_TIMEUTC_INVALID;
              exit;
            end;
        end;
      res := self._timeUTC;
      result := res;
      exit;
    end;


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
  ///   YAPI_SUCCESS if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDataLogger.set_timeUTC(newval:int64):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('timeUTC',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the current activation state of the data logger.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a value among Y_RECORDING_OFF, Y_RECORDING_ON and Y_RECORDING_PENDING corresponding to the current
  ///   activation state of the data logger
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_RECORDING_INVALID.
  /// </para>
  ///-
  function TYDataLogger.get_recording():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_RECORDING_INVALID;
              exit;
            end;
        end;
      res := self._recording;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the activation state of the data logger to start/stop recording data.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   a value among Y_RECORDING_OFF, Y_RECORDING_ON and Y_RECORDING_PENDING corresponding to the
  ///   activation state of the data logger to start/stop recording data
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
  function TYDataLogger.set_recording(newval:Integer):integer;
    var
      rest_val: string;
    begin
      rest_val := inttostr(newval);
      result := _setAttr('recording',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns the default activation state of the data logger on power up.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_AUTOSTART_OFF or Y_AUTOSTART_ON, according to the default activation state of the data
  ///   logger on power up
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_AUTOSTART_INVALID.
  /// </para>
  ///-
  function TYDataLogger.get_autoStart():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_AUTOSTART_INVALID;
              exit;
            end;
        end;
      res := self._autoStart;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the default activation state of the data logger on power up.
  /// <para>
  ///   Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   either Y_AUTOSTART_OFF or Y_AUTOSTART_ON, according to the default activation state of the data
  ///   logger on power up
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
  function TYDataLogger.set_autoStart(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('autoStart',rest_val);
    end;

  ////
  /// <summary>
  ///   Returns true if the data logger is synchronised with the localization beacon.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   either Y_BEACONDRIVEN_OFF or Y_BEACONDRIVEN_ON, according to true if the data logger is
  ///   synchronised with the localization beacon
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_BEACONDRIVEN_INVALID.
  /// </para>
  ///-
  function TYDataLogger.get_beaconDriven():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_BEACONDRIVEN_INVALID;
              exit;
            end;
        end;
      res := self._beaconDriven;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Changes the type of synchronisation of the data logger.
  /// <para>
  ///   Remember to call the saveToFlash() method of the module if the
  ///   modification must be kept.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="newval">
  ///   either Y_BEACONDRIVEN_OFF or Y_BEACONDRIVEN_ON, according to the type of synchronisation of the data logger
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
  function TYDataLogger.set_beaconDriven(newval:Integer):integer;
    var
      rest_val: string;
    begin
      if(newval>0) then rest_val := '1' else rest_val := '0';
      result := _setAttr('beaconDriven',rest_val);
    end;

  function TYDataLogger.get_clearHistory():Integer;
    var
      res : Integer;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
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
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes $THEFUNCTION$
  /// </param>
  /// <returns>
  ///   a <c>YDataLogger</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
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


  ////
  /// <summary>
  ///   Clears the data logger memory and discards all recorded data streams.
  /// <para>
  ///   This method also resets the current run index to zero.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYDataLogger.forgetAllDataStreams():LongInt;
    begin
      result := self.set_clearHistory(Y_CLEARHISTORY_TRUE);
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a list of YDataSet objects that can be used to retrieve
  ///   all measures stored by the data logger.
  /// <para>
  /// </para>
  /// <para>
  ///   This function only works if the device uses a recent firmware,
  ///   as YDataSet objects are not supported by firmwares older than
  ///   version 13000.
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   a list of YDataSet object.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty list.
  /// </para>
  ///-
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

//--- (generated code: DataLogger functions)

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

//--- (end of generated code: DataLogger functions)



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


initialization
   //--- (generated code: DataLogger initialization)
  //--- (end of generated code: DataLogger initialization)

finalization
   //--- (generated code: DataLogger cleanup)
  _DataLoggerCleanup();
  //--- (end of generated code: DataLogger cleanup)
end.
