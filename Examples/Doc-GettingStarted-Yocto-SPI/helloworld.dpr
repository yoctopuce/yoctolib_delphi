program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api,
  yocto_spiport;

procedure usage();
  var
    execname:string;
  begin
    execname := ExtractFileName(paramstr(0));
    WriteLn('Usage:');
    WriteLn(execname + '<serial_number> <value>');
    WriteLn(execname + '<logical_name>  <value>');
    WriteLn(execname + 'any  <value>   (use any discovered device)');
    //sleep(2500);
    halt;
  end;

var
 errmsg,target : string;
 value,i : integer;
 m : TYModule;
 spiPort : TYSpiPort;
 bytes : TLongIntArray;

begin
  if (paramcount<2) then usage();
  target := UpperCase(paramstr(1));
  value  := StrToInt(paramstr(2));

  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  if (target='ANY') then
    begin
      spiPort := YFirstSpiPort();
      if (spiPort = nil) then
       begin
         writeln('No module connected (check USB cable)');
         halt;
       end;
      m := spiPort.get_module();
      target := m.get_serialNumber();
     end;

  Writeln('using ' + target);
  spiPort := YFindSpiPort(target + '.spiPort');

  if (spiPort.isOnline()) then
    begin
      spiPort.set_spiMode('250000,2,msb');
      spiPort.set_ssPolarity(Y_SSPOLARITY_ACTIVE_LOW);
      spiPort.set_protocol('Frame:5ms');
      spiPort.reset();
      writeln('****************************');
      writeln('* make sure voltage levels *');
      writeln('* are properly configured  *');
      writeln('****************************');
      
      spiPort.writeHex('0c01'); // Exit from shutdown state
      spiPort.writeHex('09ff'); // Enable BCD for all digits
      spiPort.writeHex('0b07'); // Enable digits 0-7 (=8 in total)
      spiPort.writeHex('0a0a'); // Set medium brightness
      setLength(bytes,2);
      for i := 1 to 8 do
        begin
          bytes[0] := i;          // digit position
          bytes[1] := value mod 10; // digit value
          spiPort.writeArray(bytes);
          value := value div 10;
        end
    end
  else writeln('Module not connected (check identification and USB cable)');

  yFreeAPI();
end.
