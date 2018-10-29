{*********************************************************************
 *
 *  $Id: helloworld.dpr 32621 2018-10-10 13:10:25Z seb $
 *
 *  An example that show how to use a  Yocto-Relay
 *
 *  You can find more information on our web site:
 *   Yocto-Relay documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-relay/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
  yocto_api,
  yocto_relay;

procedure usage();
  var
    execname:string;
  begin
    execname := ExtractFileName(paramstr(0));
    WriteLn('Usage:');
    WriteLn(execname + ' <serial_number> <channel> < A | B >');
    WriteLn(execname + ' <logical_name> <channel>  < A | B >');
    WriteLn(execname + ' any <channel> < A | B >');
    WriteLn('Example:');
    WriteLn(execname + ' any 2 B');
    sleep(2500);
    halt;
  end;

var
 errmsg,target,state,channel:string;
 relay:TYRelay;
 m : TYModule;

begin
  if (paramcount<3) then usage();

  // parse command line
  target  :=  UpperCase(paramstr(1));
  channel :=  paramstr(2);
  state   :=  UpperCase(paramstr(3));

  // Setup the API to use local USB devices
  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  if (target='ANY') then
    begin
      // try to find the first vavailable relay
      relay :=  YFirstRelay();
      if (relay =nil) then
       begin
         writeln('No module connected (check USB cable)');
         halt;
       end;
      // retreive the hosting device serial
      m :=  relay.get_module();
      target := m. get_serialNumber();
     end;

  Writeln('using ' + target);
  // retreive the right channel
  relay := YFindRelay(target + '.relay'+channel);

  // lets switch the relay
  if (relay.isOnline()) then
    begin
      if (state = 'B') then  relay.set_output(Y_OUTPUT_ON)
                        else relay.set_output(Y_OUTPUT_OFF);
    end
   else writeln('Module not connected (check identification and USB cable)');
  yFreeAPI();

end.
