{*********************************************************************
 *
 *  $Id: helloworld.dpr 47192 2021-11-08 18:02:19Z seb $
 *
 *  An example that show how to use a  Yocto-Altimeter
 *
 *  You can find more information on our web site:
 *   Yocto-Altimeter documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-altimeter/doc.html
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
  yocto_altitude,
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
  End;

var
  asensor       : TYAltitude;
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
      // lets try to find the first available altitude sensor
      asensor := yFirstAltitude();
      if asensor=nil then
         begin
           writeln('No module connected (check USB cable)');
           sleep(3000);
           halt;
         end
       end
   else
  // or the one specified on command line
  asensor:= YFindAltitude(paramstr(1)+'.altitude');

  // make sure it is online
  if  not asensor.isOnline() then
    begin
      writeln('No module connected (check USB cable)');
      sleep(3000);
      halt;
    end;

  // lets find the parent module so we can get the other sensors
  m :=  asensor.get_module();
  serial :=  m.get_serialNumber();

  // retreive all sensor present on the yocto-meteo
  asensor := yFindAltitude(serial+'.altitude');
  tsensor := yFindTemperature(serial+'.temperature');
  psensor := yFindPressure(serial+'.pressure');

  // let's poll
  done := false;
  repeat
    if (asensor.isOnline()) then
     begin
       Writeln('Curr altitude:    '+FloatToStr(asensor.get_currentValue())+' m ' +
               '(QNH='+FloatToStr(asensor.get_qnh())+' hPa)');
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