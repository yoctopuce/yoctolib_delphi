{*********************************************************************
 *
 * $Id: yocto_datalogger.pas 10762 2013-03-28 10:12:04Z mvuilleu $
 *
 * Implements yFindDataLogger(), the high-level API for DataLogger functions
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
 *    THE SOFTWARE AND DOCUMENTATION ARE PROVIDED 'AS IS' WITHOUT
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
 *********************************************************************}


unit yocto_datalogger;

interface

uses
   sysutils, classes, windows, yocto_api, yjson;

//--- (generated code: YDataLogger definitions)

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_OLDESTRUNINDEX_INVALID        = YAPI_INVALID_LONGWORD;
   Y_CURRENTRUNINDEX_INVALID       = YAPI_INVALID_LONGWORD;
   Y_SAMPLINGINTERVAL_INVALID      = YAPI_INVALID_LONGWORD;
   Y_TIMEUTC_INVALID               = YAPI_INVALID_LONGWORD;
   Y_RECORDING_OFF = 0;
   Y_RECORDING_ON = 1;
   Y_RECORDING_INVALID = -1;

   Y_AUTOSTART_OFF = 0;
   Y_AUTOSTART_ON = 1;
   Y_AUTOSTART_INVALID = -1;

   Y_CLEARHISTORY_FALSE = 0;
   Y_CLEARHISTORY_TRUE = 1;
   Y_CLEARHISTORY_INVALID = -1;



//--- (end of generated code: YDataLogger definitions)

   Y_DATA_INVALID = YAPI_INVALID_DOUBLE;

type

TYDataLoggerRawData = Array of Array of  Double;

TYDataLogger =class;

TYDataStream=class(Tobject)
 protected
   dataLogger   : TYDataLogger  ;
   runNo, timeStamp, interval :longword;
   utcStamp     : longword;

   nRows, nCols : longword;
   columnNames  : Tstringlist;
   values       : TYDataLoggerRawData;


 public
   constructor create(parent:TYDataLogger;run,stamp,utc,itv:longword );
   destructor  Destroy();  override;

   function loadStream():integer;

////
/// <summary>
///   Returns the run index of the data stream.
/// <para>
///   A run can be made of
///   multiple datastreams, for different time intervals.
/// </para>
/// <para>
///   This method does not cause any access to the device, as the value
///   is preloaded in the object at instantiation time.
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an unsigned number corresponding to the run index.
/// </returns>
///-
    function  get_runIndex():longword;

////
/// <summary>
///   Returns the start time of the data stream, relative to the beginning
///   of the run.
/// <para>
///   If you need an absolute time, use <c>get_startTimeUTC()</c>.
/// </para>
/// <para>
///   This method does not cause any access to the device, as the value
///   is preloaded in the object at instantiation time.
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
    function   get_startTime():longword;

////
/// <summary>
///   Returns the start time of the data stream, relative to the Jan 1, 1970.
/// <para>
///   If the UTC time was not set in the datalogger at the time of the recording
///   of this data stream, this method returns 0.
/// </para>
/// <para>
///   This method does not cause any access to the device, as the value
///   is preloaded in the object at instantiation time.
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
    function  get_startTimeUTC():longword;

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
    function     get_dataSamplesInterval():longword;

////
/// <summary>
///   Returns the number of data rows present in this stream.
/// <para>
/// </para>
/// <para>
///   This method fetches the whole data stream from the device,
///   if not yet done.
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
    function     get_rowCount():longword;


////
/// <summary>
///   Returns the number of data columns present in this stream.
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
///   an unsigned number corresponding to the number of rows.
/// </returns>
/// <para>
///   On failure, throws an exception or returns zero.
/// </para>
///-
    function    get_columnCount():longword;

////
/// <summary>
///   Returns the title (or meaning) of each data column present in this stream.
/// <para>
///   In most case, the title of the data column is the hardware identifier
///   of the sensor that produced the data. For archived streams created by
///   summarizing a high-resolution data stream, there can be a suffix appended
///   to the sensor identifier, such as _min for the minimum value, _avg for the
///   average value and _max for the maximal value.
/// </para>
/// <para>
///   This method fetches the whole data stream from the device,
///   if not yet done.
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
    function get_columnNames():tstringlist;

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
    function get_dataRows():TYDataLoggerRawData;

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
    function get_data( row, col :integer):double;
 end;




//--- (generated code: YDataLogger declaration)
 TUpdateCallback  = procedure(func: TYDataLogger; value:string);
////
/// <summary>
///   TYDataLogger Class: DataLogger function interface
/// <para>
///   Yoctopuce sensors include a non-volatile memory capable of storing ongoing measured
///   data automatically, without requiring a permanent connection to a computer.
///   The Yoctopuce application programming interface includes functions to control
///   how this internal data logger works.
///   Beacause the sensors do not include a battery, they do not have an absolute time
///   reference. Therefore, measures are simply indexed by the absolute run number
///   and time relative to the start of the run. Every new power up starts a new run.
///   It is however possible to setup an absolute UTC time by software at a given time,
///   so that the data logger keeps track of it until it is next powered off.
/// </para>
/// </summary>
///-
TYDataLogger=class(TYFunction)
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _oldestRunIndex           : LongWord;
   _currentRunIndex          : LongWord;
   _samplingInterval         : LongWord;
   _timeUTC                  : LongWord;
   _recording                : Integer;
   _autoStart                : Integer;
   _clearHistory             : Integer;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of generated code: YDataLogger declaration)
protected
  _dataLoggerURL :string;

  function getData(runIdx: longword  ; timeIdx: longword; var jsondata:TJsonParser):integer;
public
   constructor Create(func:string);

   ////
   /// <summary>
   ///   Continues the enumeration of data loggers started using <c>yFirstDataLogger()</c>.
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a pointer to a <c>YDataLogger</c> object, corresponding to
   ///   a data logger currently online, or a <c>null</c> pointer
   ///   if there are no more data loggers to enumerate.
   /// </returns>
   ///-
   function nextDataLogger():TYDataLogger;

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
   function  forgetAllDataStreams():integer;

   ////
   /// <summary>
   ///   Builds a list of all data streams hold by the data logger.
   /// <para>
   ///   The caller must pass by reference an empty array to hold YDataStream
   ///   objects, and the function fills it with objects describing available
   ///   data sequences.
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
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the data logger.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the data logger
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the data logger.
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
   ///   a string corresponding to the logical name of the data logger
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
   ///   Returns the current value of the data logger (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the data logger (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

   ////
   /// <summary>
   ///   Returns the index of the oldest run for which the non-volatile memory still holds recorded data.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   an integer corresponding to the index of the oldest run for which the non-volatile memory still
   ///   holds recorded data
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_OLDESTRUNINDEX_INVALID</c>.
   /// </para>
   ///-
   function get_oldestRunIndex():LongWord;

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
   function get_currentRunIndex():LongWord;

   function get_samplingInterval():LongWord;

   function set_samplingInterval(newval:LongWord):integer;

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
   function get_timeUTC():LongWord;

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
   function set_timeUTC(newval:LongWord):integer;

   ////
   /// <summary>
   ///   Returns the current activation state of the data logger.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   either <c>Y_RECORDING_OFF</c> or <c>Y_RECORDING_ON</c>, according to the current activation state
   ///   of the data logger
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
   ///   either <c>Y_RECORDING_OFF</c> or <c>Y_RECORDING_ON</c>, according to the activation state of the
   ///   data logger to start/stop recording data
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

   function get_clearHistory():Integer;

   function set_clearHistory(newval:Integer):integer;

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
///   the first data logger currently online, or a <c>null</c> pointer
///   if there are none.
/// </returns>
///-
function yFirstDataLogger():TYDataLogger;

//--- (end of generated code: DataLogger functions declaration)

implementation

  const
  OLDDATAURL ='/dataLogger.json';
  NEWDATAURL ='/logger.json';


 function TYDataLogger.getData(runIdx: longword  ; timeIdx: longword; var jsondata:TJsonParser):integer;
   var
    dev      : TYdevice;
    errmsg   : string;
    query    : string;
    buffer   : ansistring;
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

    if  (_dataLoggerURL='') then _dataLoggerURL:=NEWDATAURL;

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

     If ((jsondata.getHTTcode() = 404) And (_dataLoggerURL <> OLDDATAURL)) Then
      begin
        // retry using backward-compatible datalogger URL
        _dataLoggerURL := OLDDATAURL;
        getData := getData(runIdx, timeIdx, jsondata);
        exit;
      end;


    getData :=YAPI_SUCCESS;
  end;

 function  TYDataLogger.forgetAllDataStreams():integer;
  begin
    forgetAllDataStreams:=set_clearHistory(Y_CLEARHISTORY_TRUE);
  end;

 function  TYDataLogger.get_dataStreams(v:Tlist):integer;
  var
    j       : TJsonParser;
    i,res   : integer;
    root,el : PJSONRECORD;
  begin
    v.clear();
    res := getData(0, 0,  j);
    if (res <> YAPI_SUCCESS) then
     begin
       get_dataStreams :=res;
       exit;
     end;

    root:=j.GetRootNode();
    for i:=0 to root.itemcount-1 do
     begin
       el := root.items[i] ;
       v.add(TYDataStream.create(self,el.items[0].ivalue,el.items[1].ivalue,el.items[2].ivalue,el.items[3].ivalue))
     end;

    j.free();
    get_dataStreams:= YAPI_SUCCESS;
  end;


//--- (generated code: YDataLogger implementation)

var
   _DataLoggerCache : TStringList;

constructor TYDataLogger.Create(func:string);
 begin
   inherited Create('DataLogger', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _oldestRunIndex := Y_OLDESTRUNINDEX_INVALID;
   _currentRunIndex := Y_CURRENTRUNINDEX_INVALID;
   _samplingInterval := Y_SAMPLINGINTERVAL_INVALID;
   _timeUTC := Y_TIMEUTC_INVALID;
   _recording := Y_RECORDING_INVALID;
   _autoStart := Y_AUTOSTART_INVALID;
   _clearHistory := Y_CLEARHISTORY_INVALID;
 end;

{$HINTS OFF}
function TYDataLogger._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'oldestRunIndex') then
       begin
         _oldestRunIndex := member^.ivalue;
       end else
      if (member^.name = 'currentRunIndex') then
       begin
         _currentRunIndex := member^.ivalue;
       end else
      if (member^.name = 'samplingInterval') then
       begin
         _samplingInterval := member^.ivalue;
       end else
      if (member^.name = 'timeUTC') then
       begin
         _timeUTC := member^.ivalue;
       end else
      if (member^.name = 'recording') then
       begin
         _recording := member^.ivalue;
       end else
      if (member^.name = 'autoStart') then
       begin
         _autoStart := member^.ivalue;
       end else
      if (member^.name = 'clearHistory') then
       begin
         _clearHistory := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the data logger.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the data logger
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYDataLogger.get_logicalName():string;
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
///   Changes the logical name of the data logger.
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
///   a string corresponding to the logical name of the data logger
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
function TYDataLogger.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the data logger (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the data logger (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYDataLogger.get_advertisedValue():string;
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
///   Returns the index of the oldest run for which the non-volatile memory still holds recorded data.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   an integer corresponding to the index of the oldest run for which the non-volatile memory still
///   holds recorded data
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_OLDESTRUNINDEX_INVALID.
/// </para>
///-
function TYDataLogger.get_oldestRunIndex():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_OLDESTRUNINDEX_INVALID;
         exit;
       end;
   result := _oldestRunIndex;
 end;

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
function TYDataLogger.get_currentRunIndex():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CURRENTRUNINDEX_INVALID;
         exit;
       end;
   result := _currentRunIndex;
 end;

function TYDataLogger.get_samplingInterval():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_SAMPLINGINTERVAL_INVALID;
         exit;
       end;
   result := _samplingInterval;
 end;

function TYDataLogger.set_samplingInterval(newval:LongWord):integer;
 var
   rest_val: string;
 begin
   rest_val := inttostr(newval);
   result := _setAttr('samplingInterval',rest_val);
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
function TYDataLogger.get_timeUTC():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_TIMEUTC_INVALID;
         exit;
       end;
   result := _timeUTC;
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
function TYDataLogger.set_timeUTC(newval:LongWord):integer;
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
///   either Y_RECORDING_OFF or Y_RECORDING_ON, according to the current activation state of the data logger
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_RECORDING_INVALID.
/// </para>
///-
function TYDataLogger.get_recording():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_RECORDING_INVALID;
         exit;
       end;
   result := _recording;
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
///   either Y_RECORDING_OFF or Y_RECORDING_ON, according to the activation state of the data logger to
///   start/stop recording data
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
   if(newval>0) then rest_val := '1' else rest_val := '0';
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
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_AUTOSTART_INVALID;
         exit;
       end;
   result := _autoStart;
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

function TYDataLogger.get_clearHistory():Integer;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_CLEARHISTORY_INVALID;
         exit;
       end;
   result := _clearHistory;
 end;

function TYDataLogger.set_clearHistory(newval:Integer):integer;
 var
   rest_val: string;
 begin
   if(newval>0) then rest_val := '1' else rest_val := '0';
   result := _setAttr('clearHistory',rest_val);
 end;

function TYDataLogger.nextDataLogger(): TYDataLogger;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextDataLogger := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextDataLogger := nil;
      exit;
    end;
    nextDataLogger := yFindDataLogger(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYDataLogger.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYDataLogger.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYDataLogger.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYDataLogger.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of generated code: YDataLogger implementation)

//--- (generated code: DataLogger functions)

function yFindDataLogger(func:string): TYDataLogger;
 var
   index: integer;
   res  : TYDataLogger;
 begin
    if (_DataLoggerCache.Find(func, index)) then
     begin
       yFindDataLogger := TYDataLogger(_DataLoggerCache.objects[index]);
       exit;
     end;
   res := TYDataLogger.Create(func);
   _DataLoggerCache.addObject(func, res);
   yFindDataLogger := res;
 end;

function yFirstDataLogger(): TYDataLogger;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('DataLogger', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstDataLogger := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstDataLogger := nil;
       exit;
    end;
   yFirstDataLogger := yFindDataLogger(serial+'.'+funcId);
 end;

procedure _DataLoggerCleanup();
  var i:integer;
begin
  for i:=0 to _DataLoggerCache.count-1 do 
    begin
     _DataLoggerCache.objects[i].free();
     _DataLoggerCache.objects[i]:=nil;
    end;
   _DataLoggerCache.free();
   _DataLoggerCache:=nil;
end;

//--- (end of generated code: DataLogger functions)



function TYDataStream.loadStream():integer;
  type
   longWordArray = array[0..100] of  longWord;
   PlongWordArray = ^longWordArray;
  var

   json            : TJsonParser;
   res             : integer;
   count,index,offset: longword;
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
   calhdl          : array of yCalibrationHandler;
   caltyp          : array of integer;
   calpar          : array of intArr;
   calraw          : array of floatArr;
   calref          : array of floatArr;

  begin
    data := nil;
    res := datalogger.getData(runNo, timeStamp,  json);
    if (res<>YAPI_SUCCESS) then
     begin
      loadStream:=res;
      exit;
     end;

    nrows := 0;
    ncols := 0;
    columnNames.clear();
    values:=nil;
    coldiv := nil;
    coltype :=nil;

    root := json.GetRootNode();
    for i:=0 to root.membercount-1 do
     begin
      el    :=  root.members[i];
      name  := string(el.name);
      if (name='time')          then timeStamp := el.ivalue
      else if (name='UTC')      then utcStamp  := el.ivalue
      else if (name='interval') then interval  := el.ivalue
      else if (name='nRows')    then nRows     := el.ivalue
      else if (name='keys')     then
       begin
         ncols :=  el.itemcount;
         for j:=0 to  ncols-1 do
           columnNames.add(string(el.items[j].svalue))
       end
      else if (name='div') then
       begin
         ncols :=  el.itemcount;
         getmem( coldiv,sizeof(longWord)*ncols);
         for j:=0 to  ncols-1 do
           coldiv^[j]:= el.items[j].ivalue;

       end
      else  if (name='type') then
        begin
         ncols := el.itemcount;
         getmem( coltype,sizeof(longWord)*ncols);
         for j:=0 to ncols-1 do
           coltype^[j]:= el.items[j].ivalue;
       end
      else  if (name='scal') then
        begin
         ncols := el.itemcount;
         setLength(colscl,ncols);
         setLength(colofs,ncols);
         for j:=0 to ncols-1 do
           begin
             colscl[j]:= el.items[j].ivalue / 65536.0;
             if coltype^[j] <> 0 then colofs[j]:=-32767 else colofs[j]:=0;
           end;
       end
       else  if (name='cal') then
        begin
         setlength(calhdl,ncols);
         setlength(caltyp,ncols);
         setlength(calpar,ncols);
         setlength(calraw,ncols);
         setlength(calref,ncols);
         for j:=0 to ncols-1 do
            begin
               caltyp[j] := _decodeCalibrationPoints(string(el.items[j].svalue), addr(calPar[j]), calRaw[j],calref[j],colscl[j],colofs[j]);
               calhdl[j] := _getCalibrationHandler(caltyp[j]);
            end;
       end

      else if  (name='data') then
       begin

        if length(colscl) =0 then
         begin
          setLength(colscl,ncols);
          setLength(colofs,ncols);
          for j:=0 to ncols-1 do
            begin
              colscl[j]:= 1.0 / coldiv^[j];
              if coltype^[j] <> 0 then colofs[j]:=-32767 else colofs[j]:=0;
            end;
         end;

        count :=  nrows *ncols ;
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
          if (count<> nrows *ncols)  then
            begin
               loadStream:=YAPI_IO_ERROR;
               exit;
            end;
           for j:=0 to count-1 do tmpdata[j]:=  el.items[j].ivalue;
         end;

         setLength(values,nrows,  ncols);
         x:=0;y:=0;
         for j:=0 to count-1 do
          begin
            if(coltype[x]<2) then
              value := (tmpdata[j] + colofs[x]) * colscl[x]
            else
              value := _decimalToDouble(tmpdata[j]-32767);
            if (high(calhdl)>=x) and (addr(calhdl[x])<>nil)  then
              begin
                if( caltyp[x] <= 10) then
                  value := calhdl[x]((value + colofs[x])/coldiv^[x],caltyp[x],calpar[x],calRaw[x],calref[x])
                else if (caltyp[x] >20) then
                  value := calhdl[x](value,caltyp[x],calpar[x],calRaw[x],calref[x])
              end;
            values[y][x] := value;
            inc(x);
            if (x>=integer(ncols)) then
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

constructor TYDataStream.create(parent:TYDataLogger;run,stamp,utc,itv:longword );
  begin
    datalogger  := parent;
    runNo       := run;
    timestamp   := stamp;
    utcStamp    := utc;
    interval    := itv;
    nrows       := 0;
    ncols       := 0;
    columnNames := Tstringlist.create;
    values      := nil;
   end;

destructor  TYDataStream.destroy();
 begin
    columnNames.free;
    values:=nil;
    inherited destroy();
 end;


////
/// <summary>
/// Returns the run index of the data stream.
/// <para>
/// A run can be made of
/// multiple datastreams, for different time intervals.
/// This method does not cause any access to the device, as the value
/// is preloaded in the object at instantiation time.
/// </para>
/// </summary>
/// <returns>
/// an unsigned number corresponding to the run index.
/// </returns>
///-
function  TYDataStream.get_runIndex():longword;
  begin
   get_runIndex:=runNo;
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
function   TYDataStream.get_startTime():longword;
  begin
     get_startTime := timeStamp;
  end;

////
/// <summary>
/// Returns the start time of the data stream, relative to the Jan 1, 1970.
/// <para>
/// If the UTC time was not set in the datalogger at the time of the recording
/// of this data stream, this method will return 0.
/// </para>
/// <para>
/// This method does not cause any access to the device, as the value
/// is preloaded in the object at instantiation time.
/// </para>
/// </summary>
/// <returns>
///         an unsigned number corresponding to the number of seconds
///         between the Jan 1, 1970 and the beginning of this data
///         stream (i.e. Unix time representation of the absolute time).
/// </returns>
///-
function  TYDataStream.get_startTimeUTC():longword;
  begin
     get_startTimeUTC:=    utcStamp;
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
function     TYDataStream.get_dataSamplesInterval():longword;
  begin
    get_dataSamplesInterval := interval;

  end;

////
/// <summary>
/// Returns the number of data rows present in this stream.
/// <para>
/// This method will cause fetching the whole data stream from the device,
/// if not yet done.
/// </para>
/// </summary>
/// <returns>
///  an unsigned number corresponding to the number of rows.
/// </returns>
/// <para>
/// On failure, throws an exception or returns zero.
/// </para>
///-
function     TYDataStream.get_rowCount():longword;
  begin
    if(nRows = 0)  then loadStream();
    get_rowCount := nRows;
  end;

////
/// <summary>
/// Returns the number of data columns present in this stream.
/// <para>
/// The meaning of the values present in each column can be obtained
/// using the method get_columnNames().
/// </para>
/// <para>
/// This method will cause fetching the whole data stream from the device,
/// if not yet done.
/// </para>
/// </summary>
/// <returns>
/// an unsigned number corresponding to the number of rows.
/// </returns>
/// <para>
/// On failure, throws an exception or returns zero.
/// </para>
///-
function    TYDataStream.get_columnCount():longword;
  begin
     if(nCols = 0) then  loadStream();
     get_columnCount := nCols;
  end;

////
/// <summary>
/// Returns title (or meaning) of each data columns present in this stream.
/// <para>
/// In most case, the title of the data column is the hardware identifier
/// of the sensor that produced the data. For archive streams created by
/// summarizing a high-resolution data stream, there can be a suffix appended
/// to the sensor identifier, such as _min for the minimum value, _avg for the
/// average value and _max for the maximal value found in the interval.
/// </para>
/// <para>
/// This method will cause fetching the whole data stream from the device,
/// if not yet done.
/// </para>
/// </summary>
/// <returns> a vector containing as many strings as there are columns in the
///         data stream.
/// </returns>
/// <para>
/// On failure, throws an exception or returns an empty array.
/// </para>
///-
function TYDataStream.get_columnNames():tstringlist;
  begin
    if(columnNames.count = 0) then loadStream();
    get_columnNames := columnNames;
  end;

////
/// <summary>
/// Returns the whole data set contained in the stream, as a bidimensional
/// vector of numbers.
/// <para>
/// The meaning of the values present in each column can be obtained
/// using the method get_columnNames().
/// </para>
/// <para>
/// This method will cause fetching the whole data stream from the device,
/// if not yet done.
/// </para>
/// </summary>
/// <returns> a vector containing as many elements as there are rows in the
///         data stream. Each row itself is a vector of floating-point
///         numbers.
/// </returns>
/// <para>
/// On failure, throws an exception or returns an empty array.
/// </para>
///-

function TYDataStream.get_dataRows():TYDataLoggerRawData;
  begin
     if (values=nil) then loadStream();
     get_dataRows :=  values;
  end;

////
/// <summary>
/// Returns a single measure from the data stream, specified by its
/// row and column index.
/// <para>
/// The meaning of the values present in each column can be obtained
/// using the method get_columnNames().
/// </para>
/// <para>
/// This method will cause fetching the whole data stream from the device,
/// if not yet done.
/// </para>
/// </summary>
/// <param name="row">  row index</param>
/// <param name="col">  column index </param>
/// <returns> a floating-point number </returns>
/// <para>
/// On failure, throws an exception or returns Y_DATA_INVALID.
/// </para>
///-

function TYDataStream.get_data( row, col :integer):double;
  begin

   if(values=nil) then loadStream();
   if(row >= integer(nrows)) or (row<0) then
      begin
        get_data := Y_DATA_INVALID;
        exit;
      end;


   if(col >= integer(ncols)) or (col<0) then
    begin
      get_data := Y_DATA_INVALID;
      exit;
    end;

    get_data := values[row][col];

  end;



initialization
   //--- (generated code: DataLogger initialization)
   _DataLoggerCache        := TstringList.create();
   _DataLoggerCache.sorted := true;
   //--- (end of generated code: DataLogger initialization)

finalization
   //--- (generated code: DataLogger cleanup)
   _DataLoggerCleanup();
   //--- (end of generated code: DataLogger cleanup)
end.
