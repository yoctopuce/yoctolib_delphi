{*********************************************************************
 *
 *  $Id: helloworld.lpr 47192 2021-11-08 18:02:19Z seb $
 *
 *  An example that show how to use a  Yocto-MaxiPowerRelay
 *
 *  You can find more information on our web site:
 *   Yocto-MaxiPowerRelay documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-maxipowerrelay/doc.html
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
  yocto_relay;

procedure usage();
  var
    execname:string;
  begin
    execname := ExtractFileName(paramstr(0));
    WriteLn('Usage:');
    WriteLn(execname + ' <serial_number> <channel> [ ON | OFF ]');
    WriteLn(execname + ' <logical_name> <channel>  [ ON | OFF ]');
    WriteLn(execname + ' any <channel> [ ON | OFF ]');
    WriteLn('Example:');
    WriteLn(execname + ' any 2 ON');
    sleep(3000);
    halt;
  end;

var
 errmsg,target,state,channel:string;
 relay:TYRelay;
 m : TYModule;

begin
  if (paramcount<3) then usage();

  target  :=  UpperCase(paramstr(1));
  channel := paramstr(2);
  state   :=  UpperCase(paramstr(3));
  writeln('state='+state);

  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  if (target='ANY') then
    begin
      relay :=  YFirstRelay();
      if (relay =nil) then
       begin
         writeln('No module connected (check USB cable)');
         sleep(3000);
         halt;
       end;
      m :=  relay.get_module();
      target := m. get_serialNumber();
     end;

  Writeln('using ' + target);
  relay := YFindRelay(target + '.relay'+channel);

  if (relay.isOnline()) then
    begin
      relay.set_output(Y_OUTPUT_ON)
    end
   else writeln('Module not connected (check identification and USB cable)');
  yFreeAPI();

end.