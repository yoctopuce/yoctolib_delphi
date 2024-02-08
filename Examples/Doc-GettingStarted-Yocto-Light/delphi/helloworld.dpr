{*********************************************************************
 *
 *  $Id: helloworld.dpr 58172 2023-11-30 17:10:23Z martinm $
 *
 *  An example that shows how to use a  Yocto-Light
 *
 *  You can find more information on our web site:
 *   Yocto-Light documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-light/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api,
  yocto_lightSensor;

Procedure  Usage();
  var
   exe : string;
  begin
    exe:= ExtractFileName(paramstr(0));
    WriteLn(exe+' <serial_number>');
    WriteLn(exe+' <logical_name>');
    WriteLn(exe+' any');
    sleep(2500);
    halt;
  End;

var
  sensor : TYLightSensor;
  errmsg : string;
  done   : boolean;

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
      // search for the first available light sensor
      sensor := yFirstLightSensor();
      if sensor=nil then
         begin
           writeln('No module connected (check USB cable)');
           sleep(3000);
           halt;
         end
       end
   else // or use the one specified on command line
   sensor:= YFindLightSensor(paramstr(1)+'.lightSensor');

   // lets poll the sensor
  done := false;
  repeat
    if (sensor.isOnline()) then
     begin
       Write('Current ambient light: '+FloatToStr(sensor.get_currentValue())+' lx');
       Writeln('   (press Ctrl-C to exit)');
       YSleep(1000,errmsg);
     end
    else
     begin
       Writeln('Module not connected (check identification and USB cable)');
       done := true;
     end;
  until done;
  yFreeAPI();

end.
