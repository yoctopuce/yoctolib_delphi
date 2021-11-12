{*********************************************************************
 *
 *  $Id: helloworld.dpr 47192 2021-11-08 18:02:19Z seb $
 *
 *  An example that show how to use a  Yocto-4-20mA-Rx
 *
 *  You can find more information on our web site:
 *   Yocto-4-20mA-Rx documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-4-20ma-rx/doc.html
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
  sensor,ch1,ch2 : TYGenericSensor;
  module : TYModule;
  errmsg,serial : string;
  unitSensor1,unitSensor2:string;

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
  ch2:=YFindGenericSensor(serial+'.genericSensor2');

  if ch1.isOnline() then unitSensor1:= ch1.get_unit();
  if ch2.isOnline() then unitSensor2:= ch2.get_unit();

  while  ch1.isOnline() and  ch2.isOnline() do
    begin
       Write('Channel 1: '+FloatToStr(ch1.get_currentValue())+unitSensor1);
       Write('  Channel 2: '+FloatToStr(ch2.get_currentValue())+unitSensor2);
       Writeln('   (press Ctrl-C to exit)');
       Sleep(1000);
     end;
  yFreeAPI();
  Writeln('Module not connected (check identification and USB cable)');
end.