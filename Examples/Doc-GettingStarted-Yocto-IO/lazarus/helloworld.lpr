{*********************************************************************
 *
 *  $Id: helloworld.lpr 59602 2024-03-04 09:18:09Z seb $
 *
 *  An example that shows how to use a  Yocto-IO
 *
 *  You can find more information on our web site:
 *   Yocto-IO documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-io/doc.html
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
  yocto_digitalIO;

procedure usage();
  var
    execname:string;
  begin
    execname := ExtractFileName(paramstr(0));
    WriteLn('Usage:');
    WriteLn(execname + ' <serial_number>  ');
    WriteLn(execname + ' <logical_name>  ');
    WriteLn(execname + ' any  ');
    WriteLn('Example:');
    WriteLn(execname + ' any ');
    sleep(3000);
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
         sleep(3000);
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
  // bits 0..1 as output
  // bits 2..3 as input
  io.set_portDirection($03);
  io.set_portPolarity(0); // polarity set to regular
  io.set_portOpenDrain(0); // No open drain
  // We could have used set_bitXXX to configure channels one by one

  Writeln('Channels 0..1 are configured as inputs and channels 2..3');
  Writeln('are configred as ouputs, you can connect some inputs to');
  Writeln('ouputs and see what happens');

  outputdata := 0;
  while (io.isOnline()) do
    begin
     inputdata := io.get_portState(); // read port values
     line:='';  // display value as binary
     for i := 0 to 3 do
       if  (inputdata and (8 shr i))>0 then line:=line+'1' else line:=line+'0';
     Writeln('port value = ' + line);
     outputdata := (outputdata +1) mod 4; // cycle ouput 0..3
     io.set_portState(outputdata); // We could have used set_bitState as well
     ysleep(1000,errmsg);
    end;

  yFreeAPI();
  writeln('Device disconnected');
end.
