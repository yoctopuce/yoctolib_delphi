{*********************************************************************
 *
 *  $Id: helloworld.dpr 47192 2021-11-08 18:02:19Z seb $
 *
 *  An example that show how to use a  Yocto-Demo
 *
 *  You can find more information on our web site:
 *   Yocto-Demo documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-demo/doc.html
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
  yocto_led;

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
    sleep(3000);
    exit;
  end;

  if paramstr(1)='any' then
    begin
      // use the first available led
      led := yFirstLed();
      if led=nil then
         begin
           writeln('No module connected (check USB cable)');
           sleep(3000);
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
  yFreeAPI();

end.