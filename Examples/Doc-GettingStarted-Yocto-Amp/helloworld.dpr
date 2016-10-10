program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
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
    sleep(2500);
    halt;
  end;

  Procedure die(msg:string);
   begin
     writeln(msg + ' (check USB cable)');
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