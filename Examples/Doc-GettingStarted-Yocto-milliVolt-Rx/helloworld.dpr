program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
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
      halt;
    end;
  if paramstr(1)='any' then
    begin
      sensor := yFirstGenericSensor();
      if sensor=nil then
        begin
          writeln('No module connected (check USB cable)');
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