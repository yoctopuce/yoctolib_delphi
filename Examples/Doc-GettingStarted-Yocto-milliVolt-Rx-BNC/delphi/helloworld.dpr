{*********************************************************************
 *
 *  $Id: helloworld.dpr 58172 2023-11-30 17:10:23Z martinm $
 *
 *  An example that shows how to use a  Yocto-milliVolt-Rx-BNC
 *
 *  You can find more information on our web site:
 *   Yocto-milliVolt-Rx-BNC documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-millivolt-rx-bnc/doc.html
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
  yocto_genericsensor;

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
  sensor,ch1: TYGenericSensor;
  module : TYModule;
  errmsg,serial : string;
  unitSensor1:string;
begin
  if (paramcount<1) then
    usage();
  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
    begin
      Write('RegisterHub error: '+errmsg);
      sleep(3000);
      halt;
    end;
  if paramstr(1)='any' then
    begin
      sensor := yFirstGenericSensor();
      if sensor=nil then
        begin
          writeln('No module connected (check USB cable)');
          sleep(3000);
          halt;
        end
    end
  else
    sensor:= YFindGenericSensor(paramstr(1)+'.genericSensor1');

  module:=sensor.get_module();
  serial:=module.get_serialNumber();
  ch1:=YFindGenericSensor(serial+'.genericSensor1');
  writeln('using '+serial);
  if ch1.isOnline() then unitSensor1:= ch1.get_unit();

  while  ch1.isOnline()  do
    begin
      Write('Voltage: '+FloatToStr(ch1.get_currentValue())+unitSensor1);
      Writeln('   (press Ctrl-C to exit)');
      Sleep(1000);
     end;
  yFreeAPI();
  Writeln('Module not connected (check identification and USB cable)');
end.