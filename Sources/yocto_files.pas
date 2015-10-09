{*********************************************************************
 *
 * $Id: yocto_files.pas 21551 2015-09-17 16:50:38Z seb $
 *
 * Implements yFindFiles(), the high-level API for Files functions
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


unit yocto_files;

interface

uses
   sysutils, classes,contnrs, windows, yocto_api, yjson;

//--- (generated code: YFiles definitions)

const Y_FILESCOUNT_INVALID            = YAPI_INVALID_UINT;
const Y_FREESPACE_INVALID             = YAPI_INVALID_UINT;


//--- (end of generated code: YFiles definitions)

type
  TYFiles = class;
  TYFileRecord = class;
  TYFileRecordArr = array  of TYFileRecord;

  //--- (generated code: YFileRecord class start)
  ////
  /// <summary>
  ///   TYFileRecord Class: Description of a file on the device filesystem
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  ///-
  TYFileRecord=class(TObject)
  //--- (end of generated code: YFileRecord class start)
  protected

    //--- (generated code: YFileRecord declaration)
    // Attributes (function value cache)
    _name                     : string;
    _size                     : LongInt;
    _crc                      : LongInt;

    //--- (end of generated code: YFileRecord declaration)
public
   constructor create(data:string);
   

   //--- (generated code: YFileRecord accessors declaration)
    function get_name():string; overload; virtual;

    function get_size():LongInt; overload; virtual;

    function get_crc():LongInt; overload; virtual;


  //--- (end of generated code: YFileRecord accessors declaration)
end;


TYFILERECORDARRAY = array of TYFileRecord;

  //--- (generated code: YFiles class start)
  TYFilesValueCallback = procedure(func: TYFiles; value:string);
  TYFilesTimedReportCallback = procedure(func: TYFiles; value:TYMeasure);

  ////
  /// <summary>
  ///   TYFiles Class: Files function interface
  /// <para>
  ///   The filesystem interface makes it possible to store files
  ///   on some devices, for instance to design a custom web UI
  ///   (for networked devices) or to add fonts (on display
  ///   devices).
  /// </para>
  /// </summary>
  ///-
  TYFiles=class(TYFunction)
  //--- (end of generated code: YFiles class start)
  protected
    //--- (generated code: YFiles declaration)
    // Attributes (function value cache)
    _logicalName              : string;
    _advertisedValue          : string;
    _filesCount               : LongInt;
    _freeSpace                : LongInt;
    _valueCallbackFiles       : TYFilesValueCallback;
    // Function-specific method for reading JSON output and caching result
    function _parseAttr(member:PJSONRECORD):integer; override;

    //--- (end of generated code: YFiles declaration)

public
//--- (generated code: YFiles accessors declaration)
    constructor Create(func:string);

    ////
    /// <summary>
    ///   Returns the number of files currently loaded in the filesystem.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the number of files currently loaded in the filesystem
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_FILESCOUNT_INVALID</c>.
    /// </para>
    ///-
    function get_filesCount():LongInt;

    ////
    /// <summary>
    ///   Returns the free space for uploading new files to the filesystem, in bytes.
    /// <para>
    /// </para>
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   an integer corresponding to the free space for uploading new files to the filesystem, in bytes
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns <c>Y_FREESPACE_INVALID</c>.
    /// </para>
    ///-
    function get_freeSpace():LongInt;

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
    ///   Use the method <c>YFiles.isOnline()</c> to test if $THEFUNCTION$ is
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
    ///   a <c>YFiles</c> object allowing you to drive $THEFUNCTION$.
    /// </returns>
    ///-
    class function FindFiles(func: string):TYFiles;

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
    function registerValueCallback(callback: TYFilesValueCallback):LongInt; overload;

    function _invokeValueCallback(value: string):LongInt; override;

    function sendCommand(command: string):TByteArray; overload; virtual;

    ////
    /// <summary>
    ///   Reinitialize the filesystem to its clean, unfragmented, empty state.
    /// <para>
    ///   All files previously uploaded are permanently lost.
    /// </para>
    /// </summary>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function format_fs():LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Returns a list of YFileRecord objects that describe files currently loaded
    ///   in the filesystem.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="pattern">
    ///   an optional filter pattern, using star and question marks
    ///   as wildcards. When an empty pattern is provided, all file records
    ///   are returned.
    /// </param>
    /// <returns>
    ///   a list of <c>YFileRecord</c> objects, containing the file path
    ///   and name, byte size and 32-bit CRC of the file content.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns an empty list.
    /// </para>
    ///-
    function get_list(pattern: string):TYFileRecordArray; overload; virtual;

    ////
    /// <summary>
    ///   Downloads the requested file and returns a binary buffer with its content.
    /// <para>
    /// </para>
    /// </summary>
    /// <param name="pathname">
    ///   path and name of the file to download
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
    ///   Uploads a file to the filesystem, to the specified full path name.
    /// <para>
    ///   If a file already exists with the same path name, its content is overwritten.
    /// </para>
    /// </summary>
    /// <param name="pathname">
    ///   path and name of the new file to create
    /// </param>
    /// <param name="content">
    ///   binary buffer with the content to set
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function upload(pathname: string; content: TByteArray):LongInt; overload; virtual;

    ////
    /// <summary>
    ///   Deletes a file, given by its full path name, from the filesystem.
    /// <para>
    ///   Because of filesystem fragmentation, deleting a file may not always
    ///   free up the whole space used by the file. However, rewriting a file
    ///   with the same path name will always reuse any space not freed previously.
    ///   If you need to ensure that no space is taken by previously deleted files,
    ///   you can use <c>format_fs</c> to fully reinitialize the filesystem.
    /// </para>
    /// </summary>
    /// <param name="pathname">
    ///   path and name of the file to remove.
    /// </param>
    /// <returns>
    ///   <c>YAPI_SUCCESS</c> if the call succeeds.
    /// </returns>
    /// <para>
    ///   On failure, throws an exception or returns a negative error code.
    /// </para>
    ///-
    function remove(pathname: string):LongInt; overload; virtual;


    ////
    /// <summary>
    ///   Continues the enumeration of filesystems started using <c>yFirstFiles()</c>.
    /// <para>
    /// </para>
    /// </summary>
    /// <returns>
    ///   a pointer to a <c>YFiles</c> object, corresponding to
    ///   a filesystem currently online, or a <c>null</c> pointer
    ///   if there are no more filesystems to enumerate.
    /// </returns>
    ///-
    function nextFiles():TYFiles;
    ////
    /// <summary>
    ///   c
    /// <para>
    ///   omment from .yc definition
    /// </para>
    /// </summary>
    ///-
    class function FirstFiles():TYFiles;
  //--- (end of generated code: YFiles accessors declaration)
end;

procedure freeFileRecordArray(var list:TYFILERECORDARRAY);


//--- (generated code: Files functions declaration)
  ////
  /// <summary>
  ///   Retrieves a filesystem for a given identifier.
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
  ///   This function does not require that the filesystem is online at the time
  ///   it is invoked. The returned object is nevertheless valid.
  ///   Use the method <c>YFiles.isOnline()</c> to test if the filesystem is
  ///   indeed online at a given time. In case of ambiguity when looking for
  ///   a filesystem by logical name, no error is notified: the first instance
  ///   found is returned. The search is performed first by hardware name,
  ///   then by logical name.
  /// </para>
  /// </summary>
  /// <param name="func">
  ///   a string that uniquely characterizes the filesystem
  /// </param>
  /// <returns>
  ///   a <c>YFiles</c> object allowing you to drive the filesystem.
  /// </returns>
  ///-
  function yFindFiles(func:string):TYFiles;
  ////
  /// <summary>
  ///   Starts the enumeration of filesystems currently accessible.
  /// <para>
  ///   Use the method <c>YFiles.nextFiles()</c> to iterate on
  ///   next filesystems.
  /// </para>
  /// </summary>
  /// <returns>
  ///   a pointer to a <c>YFiles</c> object, corresponding to
  ///   the first filesystem currently online, or a <c>null</c> pointer
  ///   if there are none.
  /// </returns>
  ///-
  function yFirstFiles():TYFiles;

//--- (end of generated code: Files functions declaration)

implementation

 constructor TYFiles.Create(func:string);
    begin
      inherited Create(func);
      _className := 'Files';
      //--- (generated code: YFiles accessors initialization)
      _filesCount := Y_FILESCOUNT_INVALID;
      _freeSpace := Y_FREESPACE_INVALID;
      _valueCallbackFiles := nil;
      //--- (end of generated code: YFiles accessors initialization)
    end;

//--- (generated code: YFiles implementation)
{$HINTS OFF}
  function TYFiles._parseAttr(member:PJSONRECORD):integer;
    var
      sub : PJSONRECORD;
      i,l        : integer;
    begin
      if (member^.name = 'filesCount') then
        begin
          _filesCount := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      if (member^.name = 'freeSpace') then
        begin
          _freeSpace := integer(member^.ivalue);
         result := 1;
         exit;
         end;
      result := inherited _parseAttr(member);
    end;
{$HINTS ON}

  ////
  /// <summary>
  ///   Returns the number of files currently loaded in the filesystem.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the number of files currently loaded in the filesystem
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_FILESCOUNT_INVALID.
  /// </para>
  ///-
  function TYFiles.get_filesCount():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_FILESCOUNT_INVALID;
              exit;
            end;
        end;
      result := self._filesCount;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns the free space for uploading new files to the filesystem, in bytes.
  /// <para>
  /// </para>
  /// <para>
  /// </para>
  /// </summary>
  /// <returns>
  ///   an integer corresponding to the free space for uploading new files to the filesystem, in bytes
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns Y_FREESPACE_INVALID.
  /// </para>
  ///-
  function TYFiles.get_freeSpace():LongInt;
    begin
      if self._cacheExpiration <= yGetTickCount then
        begin
          if self.load(YAPI_DEFAULTCACHEVALIDITY) <> YAPI_SUCCESS then
            begin
              result := Y_FREESPACE_INVALID;
              exit;
            end;
        end;
      result := self._freeSpace;
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
  ///   Use the method <c>YFiles.isOnline()</c> to test if $THEFUNCTION$ is
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
  ///   a <c>YFiles</c> object allowing you to drive $THEFUNCTION$.
  /// </returns>
  ///-
  class function TYFiles.FindFiles(func: string):TYFiles;
    var
      obj : TYFiles;
    begin
      obj := TYFiles(TYFunction._FindFromCache('Files', func));
      if obj = nil then
        begin
          obj :=  TYFiles.create(func);
          TYFunction._AddToCache('Files',  func, obj);
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
  function TYFiles.registerValueCallback(callback: TYFilesValueCallback):LongInt;
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
      self._valueCallbackFiles := callback;
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


  function TYFiles._invokeValueCallback(value: string):LongInt;
    begin
      if (addr(self._valueCallbackFiles) <> nil) then
        begin
          self._valueCallbackFiles(self, value);
        end
      else
        begin
          inherited _invokeValueCallback(value);
        end;
      result := 0;
      exit;
    end;


  function TYFiles.sendCommand(command: string):TByteArray;
    var
      url : string;
    begin
      url := 'files.json?a='+command;
      // may throw an exception
      result := self._download(url);
      exit;
    end;


  ////
  /// <summary>
  ///   Reinitialize the filesystem to its clean, unfragmented, empty state.
  /// <para>
  ///   All files previously uploaded are permanently lost.
  /// </para>
  /// </summary>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYFiles.format_fs():LongInt;
    var
      json : TByteArray;
      res : string;
    begin
      json := self.sendCommand('format');
      res := self._json_get_key(json, 'res');
      if not((res = 'ok')) then
        begin
          self._throw( YAPI_IO_ERROR, 'format failed');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  ////
  /// <summary>
  ///   Returns a list of YFileRecord objects that describe files currently loaded
  ///   in the filesystem.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="pattern">
  ///   an optional filter pattern, using star and question marks
  ///   as wildcards. When an empty pattern is provided, all file records
  ///   are returned.
  /// </param>
  /// <returns>
  ///   a list of <c>YFileRecord</c> objects, containing the file path
  ///   and name, byte size and 32-bit CRC of the file content.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty list.
  /// </para>
  ///-
  function TYFiles.get_list(pattern: string):TYFileRecordArray;
    var
      json : TByteArray;
      filelist : TStringArray;
      res : TYFileRecordArray;
      res_pos : LongInt;
      i_i : LongInt;
    begin
      SetLength(filelist, 0);
      json := self.sendCommand('dir&f='+pattern);
      filelist := self._json_get_array(json);
      res_pos := 0;
      SetLength(res, length(filelist));;
      for i_i:=0 to length(filelist)-1 do
        begin
          res[res_pos] := TYFileRecord.create(filelist[i_i]);
          inc(res_pos);
        end;
      result := res;
      exit;
    end;


  ////
  /// <summary>
  ///   Downloads the requested file and returns a binary buffer with its content.
  /// <para>
  /// </para>
  /// </summary>
  /// <param name="pathname">
  ///   path and name of the file to download
  /// </param>
  /// <returns>
  ///   a binary buffer with the file content
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns an empty content.
  /// </para>
  ///-
  function TYFiles.download(pathname: string):TByteArray;
    begin
      result := self._download(pathname);
      exit;
    end;


  ////
  /// <summary>
  ///   Uploads a file to the filesystem, to the specified full path name.
  /// <para>
  ///   If a file already exists with the same path name, its content is overwritten.
  /// </para>
  /// </summary>
  /// <param name="pathname">
  ///   path and name of the new file to create
  /// </param>
  /// <param name="content">
  ///   binary buffer with the content to set
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYFiles.upload(pathname: string; content: TByteArray):LongInt;
    begin
      result := self._upload(pathname, content);
      exit;
    end;


  ////
  /// <summary>
  ///   Deletes a file, given by its full path name, from the filesystem.
  /// <para>
  ///   Because of filesystem fragmentation, deleting a file may not always
  ///   free up the whole space used by the file. However, rewriting a file
  ///   with the same path name will always reuse any space not freed previously.
  ///   If you need to ensure that no space is taken by previously deleted files,
  ///   you can use <c>format_fs</c> to fully reinitialize the filesystem.
  /// </para>
  /// </summary>
  /// <param name="pathname">
  ///   path and name of the file to remove.
  /// </param>
  /// <returns>
  ///   <c>YAPI_SUCCESS</c> if the call succeeds.
  /// </returns>
  /// <para>
  ///   On failure, throws an exception or returns a negative error code.
  /// </para>
  ///-
  function TYFiles.remove(pathname: string):LongInt;
    var
      json : TByteArray;
      res : string;
    begin
      json := self.sendCommand('del&f='+pathname);
      res  := self._json_get_key(json, 'res');
      if not((res = 'ok')) then
        begin
          self._throw( YAPI_IO_ERROR, 'unable to remove file');
          result:=YAPI_IO_ERROR;
          exit;
        end;
      result := YAPI_SUCCESS;
      exit;
    end;


  function TYFiles.nextFiles(): TYFiles;
    var
      hwid: string;
    begin
      if YISERR(_nextFunction(hwid)) then
        begin
          nextFiles := nil;
          exit;
        end;
      if hwid = '' then
        begin
          nextFiles := nil;
          exit;
        end;
      nextFiles := TYFiles.FindFiles(hwid);
    end;

  class function TYFiles.FirstFiles(): TYFiles;
    var
      v_fundescr      : YFUN_DESCR;
      dev             : YDEV_DESCR;
      neededsize, err : integer;
      serial, funcId, funcName, funcVal, errmsg : string;
    begin
      err := yapiGetFunctionsByClass('Files', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
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
     result := TYFiles.FindFiles(serial+'.'+funcId);
    end;

//--- (end of generated code: YFiles implementation)

//--- (generated code: Files functions)

  function yFindFiles(func:string): TYFiles;
    begin
      result := TYFiles.FindFiles(func);
    end;

  function yFirstFiles(): TYFiles;
    begin
      result := TYFiles.FirstFiles();
    end;

  procedure _FilesCleanup();
    begin
    end;

//--- (end of generated code: Files functions)



//--- (generated code: YFileRecord implementation)

  function TYFileRecord.get_name():string;
    begin
      result := self._name;
      exit;
    end;


  function TYFileRecord.get_size():LongInt;
    begin
      result := self._size;
      exit;
    end;


  function TYFileRecord.get_crc():LongInt;
    begin
      result := self._crc;
      exit;
    end;


//--- (end of generated code: YFileRecord implementation)


constructor TYFileRecord.create(data:string);
 var
   p : TJSONparser;
   node : PJSONRECORD;
 begin
   p := TJsonParser.create(data,false);
   node:= p.GetChildNode(nil,'name');
   self._name:=string(node^.svalue);
   node:= p.GetChildNode(nil,'size');
   self._size:=node^.ivalue;
   node:= p.GetChildNode(nil,'crc');
   self._crc:=node^.ivalue;
   p.free;
 end;

//--- (generated code: FileRecord functions)

  procedure _FileRecordCleanup();
    begin
    end;

//--- (end of generated code: FileRecord functions)

procedure freeFileRecordArray(var list:TYFILERECORDARRAY);
 var i:integer;
 begin
  for i:=0 to length(list)-1 do
   list[i].free();
  setLength(list,0);
 end;


initialization
   //--- (generated code: Files initialization)
  //--- (end of generated code: Files initialization)

finalization
   //--- (Files cleanup)
   _FilesCleanup();
   //--- (end of generated code: Files cleanup)
end.
