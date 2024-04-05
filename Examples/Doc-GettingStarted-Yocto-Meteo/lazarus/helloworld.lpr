{*********************************************************************
 *
 *  $Id: helloworld.lpr 59602 2024-03-04 09:18:09Z seb $
 *
 *  An example that shows how to use a  Yocto-Meteo
 *
 *  You can find more information on our web site:
 *   Yocto-Meteo documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-meteo/doc.html
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
  {$ENDIF}
  yocto_api,
  yocto_humidity,
  yocto_temperature,
  yocto_pressure;

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
  hsensor       : TYHumidity;
  tsensor       : TYTemperature;
  psensor       : TYPressure;
  m             : TYModule;
  errmsg,serial : string;
  done          : boolean;

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
      // lets try to find the first available humidity sensor
      hsensor := yFirstHumidity();
      if hsensor=nil then
         begin
           writeln('No module connected (check USB cable)');
           sleep(3000);
           halt;
         end
       end
   else
  // or the one specified on command line
  hsensor:= YFindHumidity(paramstr(1)+'.humidity');

  // make sure it is online
  if  not hsensor.isOnline() then
    begin
      writeln('No module connected (check USB cable)');
      sleep(3000);
      halt;
    end;

  // lets find the parent module so we can get the other sensors
  m :=  hsensor.get_module();
  serial :=  m.get_serialNumber();

  // retreive all sensor present on the yocto-meteo
  hsensor := yFindHumidity(serial+'.humidity');
  tsensor := yFindTemperature(serial+'.temperature');
  psensor := yFindPressure(serial+'.pressure');

  // let's poll
  done := false;
  repeat
    if (hsensor.isOnline()) then
     begin
       Writeln('Curr humidity:    '+FloatToStr(hsensor.get_currentValue())+' %RH');
       Writeln('Curr temperature: '+FloatToStr(tsensor.get_currentValue())+' deg C');
       Writeln('Curr pressure:    '+FloatToStr(psensor.get_currentValue())+' hPa');
       Writeln('   (press Ctrl-C to exit)');
       Writeln('');
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