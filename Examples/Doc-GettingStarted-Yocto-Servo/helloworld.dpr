program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api,
  yocto_servo;

procedure usage();
  var
    execname:string;
  begin
    execname := ExtractFileName(paramstr(0));
    WriteLn('Usage:');
    WriteLn(execname + ' <serial_number> <channel> position');
    WriteLn(execname + ' <logical_name> <channel> position');
    WriteLn(execname + ' any <channel> position');
    WriteLn('Example:');
    WriteLn(execname + ' any 2 1000');
    sleep(2500);
    halt;
  end;

var
 errmsg,target,channel:string;
 position:integer;
 servo:TYservo;
 m : TYModule;

begin
  if (paramcount<3) then usage();

  target   :=  UpperCase(paramstr(1));
  channel  :=  paramstr(2);
  position :=  StrToInt(paramstr(3));

  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  if (target='ANY') then
    begin
      servo :=  YFirstServo();
      if (servo =nil) then
       begin
         writeln('No module connected (check USB cable)');
         halt;
       end;
      m :=  servo.get_module();
      target := m. get_serialNumber();
     end;

  Writeln('using ' + target);
  servo := YFindServo(target + '.servo'+channel);

  if (servo.isOnline()) then servo.move(position,1500)
  else writeln('Module not connected (check identification and USB cable)');

end.
