{*********************************************************************
 *
 *  $Id: helloworld.dpr 32621 2018-10-10 13:10:25Z seb $
 *
 *  An example that show how to use a  Yocto-VOC
 *
 *  You can find more information on our web site:
 *   Yocto-VOC documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-voc/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
  yocto_api,
  yocto_voc;

Procedure  Usage();
  var
   exe : string;

  begin
    exe:= ExtractFileName(paramstr(0));
    WriteLn(exe+' <serial_number>');
    WriteLn(exe+' <logical_name>');
    WriteLn(exe+' any');
    halt;
  End;

var
  sensor : TYVoc;
  errmsg : string;
  done   : boolean;

begin

  if (paramcount<1) then usage();

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    halt;
  end;

  if paramstr(1)='any' then
    begin
      sensor := yFirstVoc();
      if sensor=nil then
         begin
           writeln('No module connected (check USB cable)');
           halt;
         end
       end
   else
    sensor:= YFindVoc(paramstr(1)+'.voc');

  done:= false;
  repeat
    if (sensor.isOnline()) then
     begin
       Write('VOC: '+FloatToStr(sensor.get_currentValue())+' ppm');
       Writeln('   (press Ctrl-C to exit)');
       Sleep(1000);
     end
    else
     begin
       Writeln('Module not connected (check identification and USB cable)');
       done := true;
     end;
  until done;
  yFreeAPI();

end.
