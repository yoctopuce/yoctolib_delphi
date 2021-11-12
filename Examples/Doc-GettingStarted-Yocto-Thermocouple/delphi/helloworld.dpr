{*********************************************************************
 *
 *  $Id: helloworld.dpr 47192 2021-11-08 18:02:19Z seb $
 *
 *  An example that show how to use a  Yocto-Thermocouple
 *
 *  You can find more information on our web site:
 *   Yocto-Thermocouple documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-thermocouple/doc.html
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
  yocto_temperature;

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
  sensor,ch1,ch2 : TYTemperature;
  module : TYModule;
  errmsg,serial : string;
  done   : boolean;

begin

  if (paramcount<1) then usage();

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    sleep(3000);
    halt;
  end;

  if paramstr(1)='any' then
    begin
      sensor := yFirstTemperature();
      if sensor=nil then
         begin
           writeln('No module connected (check USB cable)');
           sleep(3000);
           halt;
         end
      end
   else
    sensor:= YFindTemperature(paramstr(1)+'.temperature1');

  module:=sensor.get_module();
  serial:=module.get_serialNumber();
  ch1:=YFindTemperature(serial+'.temperature1');
  ch2:=YFindTemperature(serial+'.temperature2');

  done:= false;
  repeat
    if (sensor.isOnline()) then
     begin
       Write('Channel 1: '+FloatToStr(ch1.get_currentValue())+' C  ');
       Write('Channel 2: '+FloatToStr(ch2.get_currentValue())+' C  ');
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