program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
  yocto_api,
  yocto_led;

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

procedure setLedState(led:TYLed; state:boolean);
  begin
    if (led.isOnline()) then
     begin
      if state then led.set_power(Y_POWER_ON)
               else led.set_power(Y_POWER_OFF);
     end
    else Writeln('Module not connected (check identification and USB cable)');
  end;

var
  c         : char;
  led       : TYLed;
  errmsg    : string;

begin

  // Setup the API to use local USB devices
  if yRegisterHub('usb', errmsg)<>YAPI_SUCCESS then
  begin
    Write('RegisterHub error: '+errmsg);
    exit;
  end;

  if paramstr(1)='any' then
    begin
      // use the first available led
      led := yFirstLed();
      if led=nil then
         begin
           writeln('No module connected (check USB cable)');
           halt;
         end
       end
   else // or use the one specified on the command line
    led:= YFindLed(paramstr(1)+'.led');

  // make sure it is connected
  if not(led.isOnline()) then
     begin
        Writeln('Module not connected (check identification and USB cable)');
       halt;
     end;

  // minimalist UI
  Writeln('0: turn test led OFF');
  Writeln('1: turn test led ON');
  Writeln('x: exit');
  repeat
    read(c);
    case c  of
      '0' : setLedState(led,false);
      '1' : setLedState(led,true);
    end;
  until c='x';

end.