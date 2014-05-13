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
    WriteLn(execname + ' <serial_number> <channel> [ ON | OFF ]');
    WriteLn(execname + ' <logical_name> <channel>  [ ON | OFF ]');
    WriteLn(execname + ' any <channel> [ ON | OFF ]');
    WriteLn('Example:');
    WriteLn(execname + ' any 2 ON');
    sleep(2500);
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
         halt;
       end;
      m :=  relay.get_module();
      target := m. get_serialNumber();
     end;

  Writeln('using ' + target);
  relay := YFindRelay(target + '.relay'+channel);

  if (relay.isOnline()) then
    begin
      if (state = 'ON') then  relay.set_output(Y_OUTPUT_ON)
                        else relay.set_output(Y_OUTPUT_OFF);
    end
   else writeln('Module not connected (check identification and USB cable)');

end.