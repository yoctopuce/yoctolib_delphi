{*********************************************************************
 *
 * $Id: yocto_files.pas 12326 2013-08-13 15:52:20Z mvuilleu $
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

const
   Y_LOGICALNAME_INVALID           = YAPI_INVALID_STRING;
   Y_ADVERTISEDVALUE_INVALID       = YAPI_INVALID_STRING;
   Y_FILESCOUNT_INVALID            = YAPI_INVALID_LONGWORD;
   Y_FREESPACE_INVALID             = YAPI_INVALID_LONGWORD;


//--- (end of generated code: YFiles definitions)

type


//--- (generated code: YFileRecord declaration)
 TYFileRecord = class;
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
protected
   // Attributes (function value cache)
   // Function-specific method for reading JSON output and caching result

   //--- (end of generated code: YFileRecord declaration)
   _name:string;
   _crc,_size :integer;
public
   constructor create(data:string);
   

   //--- (generated code: YFileRecord accessors declaration)
   function get_name():string;

   function get_size():integer;

   function get_crc():integer;

   function name():string;

   function size():integer;

   function crc():integer;

   //--- (end of generated code: YFileRecord accessors declaration)
end;



TYFILERECORDARRAY = array of TYFileRecord;

//--- (generated code: YFiles declaration)
 TYFiles = class;
 TUpdateCallback  = procedure(func: TYFiles; value:string);
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
protected
   // Attributes (function value cache)
   _logicalName              : string;
   _advertisedValue          : string;
   _filesCount               : LongWord;
   _freeSpace                : LongWord;
   // ValueCallback 
   _callback                 : TUpdateCallback;
   // Function-specific method for reading JSON output and caching result
   function _parse(j:PJSONRECORD):integer; override;

   //--- (end of generated code: YFiles declaration)

   constructor Create(func:string);


   ////
   ///
   ///-
   function nextfiles():TYFiles;

public
//--- (generated code: YFiles accessors declaration)
  Procedure registerValueCallback(callback : TUpdateCallback);
  procedure set_callback(callback : TUpdateCallback);
  procedure setCallback(callback : TUpdateCallback);
  procedure advertiseValue(value : String);override;
   ////
   /// <summary>
   ///   Returns the logical name of the filesystem.
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the logical name of the filesystem
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_LOGICALNAME_INVALID</c>.
   /// </para>
   ///-
   function get_logicalName():string;

   ////
   /// <summary>
   ///   Changes the logical name of the filesystem.
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
   ///   a string corresponding to the logical name of the filesystem
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
   ///   Returns the current value of the filesystem (no more than 6 characters).
   /// <para>
   /// </para>
   /// <para>
   /// </para>
   /// </summary>
   /// <returns>
   ///   a string corresponding to the current value of the filesystem (no more than 6 characters)
   /// </returns>
   /// <para>
   ///   On failure, throws an exception or returns <c>Y_ADVERTISEDVALUE_INVALID</c>.
   /// </para>
   ///-
   function get_advertisedValue():string;

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
   function get_filesCount():LongWord;

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
   function get_freeSpace():LongWord;

   function sendCommand(command:string):TBYTEARRAY;

   ////
   /// <summary>
   ///   Reinitializes the filesystem to its clean, unfragmented, empty state.
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
   function format_fs():integer;

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
   function get_list(pattern:string):TYFILERECORDARRAY;

   ////
   /// <summary>
   ///   Downloads the requested file and returns a binary buffer with its content.
   /// <para>
   /// </para>
   /// </summary>
   /// <param name="pathname">
   ///   path and name of the new file to load
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
   function upload(pathname:string; content:TBYTEARRAY):integer;

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
   function remove(pathname:string):integer;

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

//--- (generated code: YFiles implementation)

var
   _FilesCache : TStringList;

constructor TYFiles.Create(func:string);
 begin
   inherited Create('Files', func);
   _logicalName := Y_LOGICALNAME_INVALID;
   _advertisedValue := Y_ADVERTISEDVALUE_INVALID;
   _filesCount := Y_FILESCOUNT_INVALID;
   _freeSpace := Y_FREESPACE_INVALID;
 end;

{$HINTS OFF}
function TYFiles._parse(j:PJSONRECORD):integer;
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
      if (member^.name = 'filesCount') then
       begin
         _filesCount := member^.ivalue;
       end else
      if (member^.name = 'freeSpace') then
       begin
         _freeSpace := member^.ivalue;
       end else
       begin end;
    end;
   _parse := 0;
 end;
{$HINTS ON}

////
/// <summary>
///   Returns the logical name of the filesystem.
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the logical name of the filesystem
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_LOGICALNAME_INVALID.
/// </para>
///-
function TYFiles.get_logicalName():string;
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
///   Changes the logical name of the filesystem.
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
///   a string corresponding to the logical name of the filesystem
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
function TYFiles.set_logicalName(newval:string):integer;
 var
   rest_val: string;
 begin
   rest_val := newval;
   result := _setAttr('logicalName',rest_val);
 end;

////
/// <summary>
///   Returns the current value of the filesystem (no more than 6 characters).
/// <para>
/// </para>
/// <para>
/// </para>
/// </summary>
/// <returns>
///   a string corresponding to the current value of the filesystem (no more than 6 characters)
/// </returns>
/// <para>
///   On failure, throws an exception or returns Y_ADVERTISEDVALUE_INVALID.
/// </para>
///-
function TYFiles.get_advertisedValue():string;
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
function TYFiles.get_filesCount():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_FILESCOUNT_INVALID;
         exit;
       end;
   result := _filesCount;
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
function TYFiles.get_freeSpace():LongWord;
 begin
   if (_cacheExpiration <= yGetTickCount()) then
      if (YISERR(load(YAPI_defaultCacheValidity))) then
       begin
         result := Y_FREESPACE_INVALID;
         exit;
       end;
   result := _freeSpace;
 end;

function TYFiles.sendCommand(command:string):TBYTEARRAY;
     var
        url : string;
     begin
        url :=  'files.json?a='+command;
        result:= self._download(url);
            
     end;


////
/// <summary>
///   Reinitializes the filesystem to its clean, unfragmented, empty state.
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
function TYFiles.format_fs():integer;
     var
        json : TBYTEARRAY;
         res : string;
     begin
        json := self.sendCommand('format'); 
        res  := self._json_get_key(json, 'res');
        if not(res = 'ok') then begin self._throw( YAPI_IO_ERROR, 'format failed'); result:= YAPI_IO_ERROR; exit; end;;
        result:= YAPI_SUCCESS;
            
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
function TYFiles.get_list(pattern:string):TYFILERECORDARRAY;
     var
        json : TBYTEARRAY;
         list : TSTRINGARRAY;
         res : TYFILERECORDARRAY;
        i_i : integer;
     begin
        json := self.sendCommand('dir&f='+pattern);
        list := self._json_get_array(json);
        for i_i:=0 to length(list)-1 do begin SetLength(res, length(res)+1); res[length(res)-1]:= TYFileRecord.create(list[i_i]);end;
        result:= res;
            
     end;


////
/// <summary>
///   Downloads the requested file and returns a binary buffer with its content.
/// <para>
/// </para>
/// </summary>
/// <param name="pathname">
///   path and name of the new file to load
/// </param>
/// <returns>
///   a binary buffer with the file content
/// </returns>
/// <para>
///   On failure, throws an exception or returns an empty content.
/// </para>
///-
function TYFiles.download(pathname:string):TBYTEARRAY;
     begin
        result:= self._download(pathname);
            
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
function TYFiles.upload(pathname:string; content:TBYTEARRAY):integer;
     begin
        result:= self._upload(pathname,content);
            
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
function TYFiles.remove(pathname:string):integer;
     var
        json : TBYTEARRAY;
         res : string;
     begin
        json := self.sendCommand('del&f='+pathname); 
        res  := self._json_get_key(json, 'res');
        if not(res = 'ok') then begin self._throw( YAPI_IO_ERROR, 'unable to remove file'); result:= YAPI_IO_ERROR; exit; end;;
        result:= YAPI_SUCCESS;
            
     end;


function TYFiles.nextFiles(): TYFiles;
 var
   hwid: string;
 begin
   if (YISERR(_nextFunction(hwid))) then
    begin
      nextFiles := nil;
      exit;
    end;
   if (hwid='') then
    begin
      nextFiles := nil;
      exit;
    end;
    nextFiles := yFindFiles(hwid);
 end;


    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
  Procedure TYFiles.registerValueCallback(callback : TUpdateCallback);
  begin
   If assigned(callback) Then
     registerFuncCallback(self)
   else
     unregisterFuncCallback(self);
   _callback := callback;
  End;

  procedure TYFiles.set_callback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
  End;

  procedure  TYFiles.setCallback(callback : TUpdateCallback);
   Begin
    registerValueCallback(callback);
   End;

  procedure  TYFiles.advertiseValue(value : String);
  Begin
    If assigned(_callback)  Then _callback(self, value)
   End;

//--- (end of generated code: YFiles implementation)

//--- (generated code: Files functions)

function yFindFiles(func:string): TYFiles;
 var
   index: integer;
   res  : TYFiles;
 begin
    if (_FilesCache.Find(func, index)) then
     begin
       yFindFiles := TYFiles(_FilesCache.objects[index]);
       exit;
     end;
   res := TYFiles.Create(func);
   _FilesCache.addObject(func, res);
   yFindFiles := res;
 end;

function yFirstFiles(): TYFiles;
 var
   v_fundescr      : YFUN_DESCR;
   dev             : YDEV_DESCR;
   neededsize, err : integer;
   serial, funcId, funcName, funcVal, errmsg : string;
 begin
   err := yapiGetFunctionsByClass('Files', 0, PyHandleArray(@v_fundescr), sizeof(YFUN_DESCR), neededsize, errmsg);
   if (YISERR(err) or (neededsize = 0)) then
    begin
       yFirstFiles := nil;
       exit;
    end;
   if (YISERR(yapiGetFunctionInfo(v_fundescr, dev, serial, funcId, funcName, funcVal, errmsg))) then
    begin
       yFirstFiles := nil;
       exit;
    end;
   yFirstFiles := yFindFiles(serial+'.'+funcId);
 end;

procedure _FilesCleanup();
  var i:integer;
begin
  for i:=0 to _FilesCache.count-1 do 
    begin
     _FilesCache.objects[i].free();
     _FilesCache.objects[i]:=nil;
    end;
   _FilesCache.free();
   _FilesCache:=nil;
end;

//--- (end of generated code: Files functions)



//--- (generated code: YFileRecord implementation)


function TYFileRecord.get_name():string;
     begin
        result:= self._name; 
     end;


function TYFileRecord.get_size():integer;
     begin
        result:= self._size; 
     end;


function TYFileRecord.get_crc():integer;
     begin
        result:= self._crc; 
     end;


function TYFileRecord.name():string;
     begin
        result:= self._name; 
     end;


function TYFileRecord.size():integer;
     begin
        result:= self._size; 
     end;


function TYFileRecord.crc():integer;
     begin
        result:= self._crc; 
     end;



    ////
    /// <summary>
    ///   comment from .
    /// <para>
    ///   yc definition
    /// </para>
    /// </summary>
    ///-
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

//--- (generated code: YFileRecord functions)
//--- (end of generated code: YFileRecord functions)

procedure freeFileRecordArray(var list:TYFILERECORDARRAY);
 var i:integer;
 begin
  for i:=0 to length(list)-1 do
   list[i].free();
  setLength(list,0);
 end;


initialization
   //--- (generated code: Files initialization)
   _FilesCache        := TstringList.create();
   _FilesCache.sorted := true;
   //--- (end of generated code: Files initialization)

finalization
   //--- (Files cleanup)
   _FilesCleanup();
   //--- (end of generated code: Files cleanup)
end.
