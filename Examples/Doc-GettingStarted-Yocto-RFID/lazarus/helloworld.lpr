{*********************************************************************
 *
 *  $Id: helloworld.dpr 70974 2025-12-22 15:06:26Z seb $
 *
 *  An example that shows how to use a Yocto-RFID-xxxx
 *
 *  You can find more information on our web site:
 *   Yocto-RFID-15693 documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-rfid-15693/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}


program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  {$IFNDEF UNIX}
  windows,
  {$ENDIF UNIX}   
  yocto_api,
  yocto_rfidreader,
  yocto_buzzer,
  yocto_anbutton,
  yocto_colorledcluster;


Procedure  Usage();
  var
   exe : string;

  begin
    exe:= ExtractFileName(paramstr(0));
    WriteLn(exe+' <serial_number>');
    WriteLn(exe+' <logical_name>');
    WriteLn(exe+' any ');
    WriteLn('');
    WriteLn('Example:');
    WriteLn(exe+' any');
    sleep(3000);
    halt;
  End;



var
  m               : TYmodule;
  leds            : TYColorLedCluster;
  buzzer          : TYBuzzer;
  reader          : TYRfidREader;
  serial          : string;
  errmsg          : string;
  tagList         : TStringArray;
  tagId           : string;
  opStatus        : TYRfidStatus;
  options         : TYRfidOptions;
  tagInfo         : TYRfidTagInfo;
  blocksize       : integer;
  firstBlock      : integer;
  data            : string;

begin

  if (paramcount<1) then usage();

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    sleep(3000);
    exit;
  end;

  if paramstr(1)='any' then
    begin
      // try to first the first relay available
      reader := yFirstRfidReader();
      if reader=nil then
         begin
           writeln('No module connected (check USB cable)');
           sleep(3000);
           halt;
         end
      end
  else // or use the one specified the command line
    reader:= YFindRfidReader(paramstr(1)+'.rfidreader');

  // make sure it connected
  if not reader.isOnline() then
    begin
       writeln('No module connected (check USB cable)');
       sleep(3000);
       halt;
    end;

  m:= reader.get_module();
  serial := m.get_serialNumber();
  leds:= YFindColorLedCluster(serial+'.colorLedCluster');
  buzzer := YFindBuzzer(serial+'.buzzer');
  leds.set_rgbColor(0,1,$000000);
  buzzer.set_volume(75);

  writeln('Place a RFID tag near the antenna');

  repeat
    tagList := reader.get_tagIdList() ;
  until Length(tagList)>0;

  options := TYRfidOptions.create();
  opStatus        := TYRfidStatus.create();

  tagId      := tagList[0];
  taginfo    := reader.get_tagInfo(tagId,opStatus);
  blocksize  := taginfo.get_tagBlockSize();
  firstBlock := taginfo.get_tagFirstBlock();

  writeln('Tag ID          = '+taginfo.get_tagId());
  writeln('Tag Memory size = '+IntToStr(taginfo.get_tagMemorySize())+' bytes');
  writeln('Tag Block  size = '+IntToStr(taginfo.get_tagBlockSize())+' bytes');



  data := reader.tagReadHex(tagId, firstBlock, 3*blocksize, options, opStatus);
  if (opStatus.get_errorCode()=Y_SUCCESS)  then
  begin
    writeln ('First 3 blocks  = '+data);
    leds.set_rgbColor(0,1,$00FF00);
    buzzer.pulse(1000,100);
  end
  else
  begin
    writeln('Cannot read tag contents ('+opStatus.get_errorMessage()+')');
    leds.set_rgbColor(0, 1, $FF0000);
  end;
  leds.rgb_move(0, 1, $000000, 200);
  sleep(3000);
  yFreeAPI();
end.