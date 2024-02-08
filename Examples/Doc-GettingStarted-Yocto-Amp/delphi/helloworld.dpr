{*********************************************************************
 *
 *  $Id: helloworld.dpr 58172 2023-11-30 17:10:23Z martinm $
 *
 *  An example that shows how to use a  Yocto-Amp
 *
 *  You can find more information on our web site:
 *   Yocto-Amp documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-amp/doc.html
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
  yocto_current;

  Procedure usage();
   var
    execname :string;

  begin
    execname := ExtractFileName(paramstr(0));
    writeln(execname+' <serial_number>');
    writeln(execname+' <logical_name>');
    writeln(execname+' any  ');
    sleep(3000);
    halt;
  end;

  Procedure die(msg:string);
   begin
     writeln(msg + ' (check USB cable)');
     sleep(3000);
     halt;
  end;

var

  errmsg   : string;
  target   : string;
  sensor   : TYCurrent;
  sensorDC : TYCurrent;
  sensorAC : TYCurrent;
  m        : TyModule;

begin

  if (paramcount<1)  then usage();

  target:=paramstr(1);

  // Setup the API to use local USB devices
  If (yRegisterHub('usb', errmsg) <> YAPI_SUCCESS) Then
    begin
      WriteLn('RegisterHub error: ' + errmsg);
      halt;
    End;

  if (target='any') then
   begin
    // retreive any voltage sensor (can be AC or DC)
    sensor := yFirstCurrent();
    If sensor=nil Then Die('No module connected');
   end
   else
   sensor:= yFindCurrent(target + '.current1');

   m := nil;
   sensorDC := nil;
   sensorAC := nil;
   //  we need to retreive both DC and AC voltage from the device.
   If (sensor.isOnline()) Then
    begin
      m := sensor.get_module();
      sensorDC := yFindCurrent(m.get_serialNumber() + '.current1');
      sensorAC := yFindCurrent(m.get_serialNumber() + '.current2');
    end else Die('Module not connected');

   // let's poll
   repeat
      If Not(m.isOnline()) Then Die('Module not connected');
      Write('DC: ' + FloatToStr(sensorDC.get_currentValue()) + ' mA ');
      Write('AC: ' + FloatToStr(sensorAC.get_currentValue()) + ' mA ');
      Writeln('  (press Ctrl-C to exit)');
      ySleep(1000, errmsg);
   until (false);
  yFreeAPI();
end.