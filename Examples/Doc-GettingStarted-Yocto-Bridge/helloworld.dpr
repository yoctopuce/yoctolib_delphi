{*********************************************************************
 *
 *  $Id: helloworld.dpr 46876 2021-10-21 08:43:08Z martinm $
 *
 *  An example that show how to use a  Yocto-Bridge
 *
 *  You can find more information on our web site:
 *   Yocto-Bridge documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-bridge/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api,
  yocto_weighscale;

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
  sensor : TYWeighScale;
  errmsg : string;
  sensorUnit : string;

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
      sensor := yFirstWeighScale();
      if sensor=nil then
         begin
           writeln('No module connected (check USB cable)');
           halt;
         end
      end
  else
    sensor:= yFindWeighScale(paramstr(1)+'.weighScale1');

  if sensor.isOnline() then 
    begin
      // On startup, enable excitation and tare weigh scale
      Writeln('Resetting tare weight...');
      sensor.set_excitation(Y_EXCITATION_AC);
      ySleep(3000,errmsg);
      sensor.tare();
      sensorUnit := sensor.get_unit();
    end;

  // Show measured weight continuously
  while sensor.isOnline() do
    begin
      Write('Weight : '+FloatToStr(sensor.get_currentValue())+sensorUnit);
      Writeln('   (press Ctrl-C to exit)');
      ySleep(1000,errmsg);
    end;
  yFreeAPI();
  Writeln('Module not connected (check identification and USB cable)');
end.