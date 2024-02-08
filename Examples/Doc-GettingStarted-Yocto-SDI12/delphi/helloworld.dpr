{*********************************************************************
 *
 *  $Id: svn_id $
 *
 *  An example that shows how to use a  Yocto-SDI12
 *
 *  You can find more information on our web site:
 *   Yocto-SDI12 documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-sdi12/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program demo;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  {$IFNDEF UNIX}
  windows,
  {$ENDIF UNIX} 
  
  yocto_api,
  yocto_sdi12port;

procedure usage();
  var
    execname:string;
  begin
    execname := ExtractFileName(paramstr(0));
    WriteLn('Usage:');
    WriteLn(execname + ' <serial_number>');
    WriteLn(execname + ' <logical_name> ');
    WriteLn(execname + ' any           (use any discovered device)');
    sleep(3000);
    halt;
  end;

var
 errmsg,target : string;
 m : TYModule;
 sdi12Port : TYSdi12Port;
 singleSensor : TYSdi12Sensor;
 valSensor : TDoubleArray;
 j :integer;

begin
  if (paramcount<1) then usage();
  target := UpperCase(paramstr(1));

  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  if (target='ANY') then
    begin
      sdi12Port := YFirstSdi12Port();
      if (sdi12Port = nil) then
       begin
         writeln('No module connected (check USB cable)');
         sleep(3000);
         halt;
       end;
      m := sdi12Port.get_module();
      target := m.get_serialNumber();
     end;

  Writeln(target);
  sdi12Port := YFindSdi12Port(target + '.sdi12Port');

  if (sdi12Port.isOnline()) then
    begin
        singleSensor := sdi12Port.discoverSingleSensor();
        writeln('Sensor address : ' + singleSensor.get_sensorAddress());
        writeln('Sensor SDI-12 compatibility : ' + singleSensor.get_sensorProtocol());
        writeln('Sensor company name : ' +  singleSensor.get_sensorVendor());
        writeln('Sensor model number : ' + singleSensor.get_sensorModel());
        writeln('Sensor version : ' + singleSensor.get_sensorVersion());
        writeln('Sensor serial number : ' + singleSensor.get_sensorSerial());
        valSensor := sdi12Port.readSensor(singleSensor.get_sensorAddress(),'M', 5000);

        for j := 0 to length(valSensor)-1 do
        begin
                writeln(Format('%s: %.2f %s %s',
            [singleSensor.get_measureSymbol(j), valSensor[j],
            singleSensor.get_measureUnit(j), singleSensor.get_measureDescription(j)]));
        end;
        sleep(5000);


    end
  else writeln('Module not connected (check identification and USB cable)');
  
  yFreeAPI();
end.
