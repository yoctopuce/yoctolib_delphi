program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
  yocto_api,
  yocto_voltage;

  Procedure usage();
  begin
    writeln('demo <serial_number>');
    writeln('demo <logical_name>');
    writeln('demo any  ');
    sleep(2500);
    halt;
  end;

  Procedure die(msg:string);
   begin
     writeln(msg + '(check USB cable)');
     halt;
  end;

var

  errmsg : string;
  target : string;
  sensor : TYVoltage;
  sensorDC : TYVoltage;
  sensorAC : TYVoltage;
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
    sensor := yFirstVoltage();
    If sensor=nil Then Die('No module connected');
   end
   else
   sensor:= yFindVoltage(target + '.voltage1');
                                          
   m := nil;
   sensorDC := nil;
   sensorAC := nil;
   //  we need to retreive both DC and AC voltage from the device.
   If (sensor.isOnline()) Then
    begin
      m := sensor.get_module();
      sensorDC := yFindVoltage(m.get_serialNumber() + '.voltage1');
      sensorAC := yFindVoltage(m.get_serialNumber() + '.voltage2');
    end else Die('Module not connected');

   // let's poll
   repeat
      If Not(m.isOnline()) Then Die('Module not connected');
      Write('DC: ' + FloatToStr(sensorDC.get_currentValue()) + ' v ');
      Write('AC: ' + FloatToStr(sensorAC.get_currentValue()) + ' v ');
      Writeln('  (press Ctrl-C to exit)');
      ySleep(1000, errmsg);
   until (false);

end.