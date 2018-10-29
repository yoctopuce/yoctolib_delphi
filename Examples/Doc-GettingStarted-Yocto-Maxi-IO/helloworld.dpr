{*********************************************************************
 *
 *  $Id: helloworld.dpr 32621 2018-10-10 13:10:25Z seb $
 *
 *  An example that show how to use a  Yocto-Maxi-IO
 *
 *  You can find more information on our web site:
 *   Yocto-Maxi-IO documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-maxi-io/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api,
  yocto_digitalIO;

procedure usage();
  var
    execname,errmsg:string;
  begin
    execname := ExtractFileName(paramstr(0));
    WriteLn('Usage:');
    WriteLn(execname + ' <serial_number>  ');
    WriteLn(execname + ' <logical_name>  ');
    WriteLn(execname + ' any  ');
    WriteLn('Example:');
    WriteLn(execname + ' any ');
    ysleep(2500,errmsg);
    halt;
  end;

var
 errmsg,target:string;
 io:TYDigitalIO;
 m : TYModule;
 outputdata,inputdata,i :integer;
 line:string;
begin
  if (paramcount<1) then usage();

  // parse command line
  target :=  UpperCase(paramstr(1));

  // Setup the API to use local USB devices
  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  if (target='ANY') then
    begin
      // try to find the first available digitial IO  feature
      io :=  YFirstDigitalIO();
      if (io =nil) then
       begin
         writeln('No module connected (check USB cable)');
         halt;
       end;
      // retreive the hosting device serial
      m :=  io.get_module();
      target := m. get_serialNumber();
     end;

  Writeln('using ' + target);

  // retreive the right DigitalIO function
  io := YFindDigitalIO(target + '.digitalIO');

  // make sure the device is here
  if not(io.isOnline()) then
    begin
     writeln('Module not connected (check identification and USB cable)');
     halt;
    end;

  // lets configure the channels direction
  // bits 0..3 as output
  // bits 4..7 as input
  io.set_portDirection($0F);
  io.set_portPolarity(0); // polarity set to regular
  io.set_portOpenDrain(0); // No open drain
  // We could have used set_bitXXX to configure channels one by one

  Writeln('Channels 0..3 are configured as inputs and channels 4..7');
  Writeln('are configred as ouputs, you can connect some inputs to');
  Writeln('ouputs and see what happens');

  outputdata := 0;
  while (io.isOnline()) do
    begin
     inputdata := io.get_portState(); // read port values
     line:='';  // display value as binary
     for i := 0 to 7 do
       if  (inputdata and (128 shr i))>0 then line:=line+'1' else line:=line+'0';
     Writeln('port value = ' + line);
     outputdata := (outputdata +1) mod 16; // cycle ouput 0..15
     io.set_portState(outputdata); // We could have used set_bitState as well
     ysleep(1000,errmsg);
    end;

  yFreeAPI();
  writeln('Device disconnected');
end.
