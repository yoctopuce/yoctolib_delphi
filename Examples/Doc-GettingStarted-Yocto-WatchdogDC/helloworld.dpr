{*********************************************************************
 *
 *  $Id: helloworld.dpr 46876 2021-10-21 08:43:08Z martinm $
 *
 *  An example that show how to use a  Yocto-WatchdogDC
 *
 *  You can find more information on our web site:
 *   Yocto-WatchdogDC documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-watchdogdc/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api,
  yocto_watchdog;

Procedure  Usage();
  var
   exe : string;

  begin
    exe:= ExtractFileName(paramstr(0));
    WriteLn(exe+' <serial_number> [on|off]');
    WriteLn(exe+' <logical_name> [on|off]');
    WriteLn(exe+' any [on|off]');
    sleep(2500);
    halt;
  End;

var
  c         : char;
  watchdog  : TYWatchdog;
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
      watchdog := yFirstWatchdog();
      if watchdog=nil then
         begin
           writeln('No module connected (check USB cable)');
           halt;
         end
       end
   else
    watchdog:= YFindWatchdog(paramstr(1)+'.watchdog1');

  Writeln('1: Start the watchdog');
  Writeln('0: Stop the watchdog');
  Writeln('r: Reset the watchdog');
  Writeln('x: exit');
  repeat
    read(c);
    if (not watchdog.isOnline()) then
      Writeln('Module not connected (check identification and USB cable)');
    case c  of
      '0' : watchdog.set_running(Y_RUNNING_OFF);
      '1' : watchdog.set_running(Y_RUNNING_ON);
      'r' : watchdog.resetWatchdog();
    end;
  until c='x';
  yFreeAPI();

end.