program helloworld;
{$APPTYPE CONSOLE}
uses
  windows,
  SysUtils,
  yocto_api,
  yocto_display,
  yocto_files;

Procedure  Usage();
  var
   exe : string;
  begin
    exe:= ExtractFileName(paramstr(0));
    WriteLn(exe+' <serial_number>');
    WriteLn(exe+' <logical_name>');
    WriteLn(exe+' any');
    sleep(3000);
    halt;
  End;

var
  errmsg,filename   : String;
  files             : TYFiles;
  filelist          : TYFILERECORDARRAY;
  contents          : ansistring;
  binaryData        : TBYTEARRAY;
  i                 : integer;

begin

  if (paramcount<1) then usage();

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    exit;
  end;

  // first one of yfiles function available
  if paramstr(1)='any' then
    begin
      files := yFirstfiles();
      if files=nil then
         begin
           writeln('No module with files features (check USB cable)');
           halt;
         end
      end
   else
  files:= YFindFiles(paramstr(1)+'.files');

  writeln;
  writeln('Using '+files.get_FriendlyName());
  writeln;

  // make sure it is online
  if not(files.isOnline()) then
      begin
        writeln('No module with files connected (check USB cable)');
        halt;
      end;


  // create text files and upload them to the device
  for i:=1 to 5 do
   begin
     contents:='This is file '+ansistring(intToStr(i))+#13#10;
     // convert th string to binary data
     setlength(binaryData,length(contents));
     move(contents[1],binaryData[0],length(binaryData));

     // upload the file to the device
     files.upload('file'+intToStr(i)+'.txt',binaryData );

     setlength(binaryData,0);
   end;

  // list files found on the device
  Writeln('Files on device:');
  filelist:=files.get_list('*');
  for i:=0 to length(filelist)-1 do
    begin
      filename := filelist[i].get_name();
      write(filename);
      write(StringOfChar(' ',40-length(filename)));  // align
      write(IntToHex(filelist[i].get_crc(),8));
      write('    ');
      writeln(inttostr(filelist[i].get_size())+' bytes');
    end;

  // object returned by the YfindFiles is one of the few
  // we must explicitely free
  freeFileRecordArray(filelist);

  //download a file
  binaryData:=files.download('file1.txt');

  // convert to string
  setlength(contents,length(binaryData));
  move(binaryData[0],contents[1],length(contents));

  // and display
  Writeln('');
  Writeln('contents of file1.txt:');
  Writeln(contents);
end.

