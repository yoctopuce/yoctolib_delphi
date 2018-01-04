program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
  yocto_api,
  yocto_humidity;

const
  serial = 'HUMSENS1-123456'; // use serial number or logical name

var
  sensor : TYHumidity;
  errmsg : string;
  done   : boolean;

begin

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    exit;
  end;

  sensor := yFindHumidity(serial+'.humidity');

  done := false;
  repeat
    if (sensor.isOnline()) then
     begin
       Write('Current humidity: '+FloatToStr(sensor.get_currentValue())+'%');
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