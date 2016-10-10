program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
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
    sleep(2500);
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
    exit;
  end;

  if paramstr(1)='any' then
    begin
      // lets try to find the first available humidity sensor
      hsensor := yFirstHumidity();
      if hsensor=nil then
         begin
           writeln('No module connected (check USB cable)');
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