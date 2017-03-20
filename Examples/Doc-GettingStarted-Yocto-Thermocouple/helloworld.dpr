program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
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
    halt;
  end;

  if paramstr(1)='any' then
    begin
      sensor := yFirstTemperature();
      if sensor=nil then
         begin
           writeln('No module connected (check USB cable)');
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