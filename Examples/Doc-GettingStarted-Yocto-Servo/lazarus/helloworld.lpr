{*********************************************************************
 *
 *  $Id: helloworld.lpr 47192 2021-11-08 18:02:19Z seb $
 *
 *  An example that show how to use a  Yocto-Servo
 *
 *  You can find more information on our web site:
 *   Yocto-Servo documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-servo/doc.html
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
    sleep(3000);
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
         sleep(3000);
         halt;
       end;
      m :=  servo.get_module();
      target := m. get_serialNumber();
     end;

  Writeln('using ' + target);
  servo := YFindServo(target + '.servo'+channel);

  if (servo.isOnline()) then servo.move(position,1500)
  else writeln('Module not connected (check identification and USB cable)');
  yFreeAPI();
end.
