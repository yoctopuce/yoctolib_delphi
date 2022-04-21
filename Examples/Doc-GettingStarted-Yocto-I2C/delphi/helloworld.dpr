{*********************************************************************
 *
 *  $Id: helloworld.dpr 48374 2022-01-28 15:44:48Z mvuilleu $
 *
 *  An example that show how to use a  Yocto-I2C
 *
 *  You can find more information on our web site:
 *   Yocto-I2C documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-i2c/doc.html
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
  yocto_i2cport;

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
 i2cPort : TYI2cPort;
 toSend : TLongIntArray;
 received : TLongIntArray;
 tempReg : integer;

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
      i2cPort := YFirstI2cPort();
      if (i2cPort = nil) then
       begin
         writeln('No module connected (check USB cable)');
         sleep(3000);
         halt;
       end;
      m := i2cPort.get_module();
      target := m.get_serialNumber();
     end;

  Writeln('using ' + target);
  i2cPort := YFindI2cPort(target + '.i2cPort');

  if (i2cPort.isOnline()) then
    begin
      // sample code reading MCP9804 temperature sensor
      i2cPort.set_i2cMode('100kbps');
      i2cPort.set_i2cVoltageLevel(Y_I2CVOLTAGELEVEL_3V3);
      i2cPort.reset();
      // do not forget to configure the powerOutput
      // of the Yocto-I2C as well if used
      writeln('****************************');
      writeln('* make sure voltage levels *');
      writeln('* are properly configured  *');
      writeln('****************************');
      setLength(toSend, 1);
      toSend[0] := $05;
      received := i2cPort.i2cSendAndReceiveArray($1f, toSend, 2);
      tempReg := (received[0] shl 8) + received[1];
      if (tempReg and $1000) <> 0 then
        tempReg := tempReg - $2000     // perform sign extension
      else
        tempReg := tempReg and $0fff;  // clear status bits
      WriteLn('Ambiant temperature: ', tempReg / 16.0);
    end
  else writeln('Module not connected (check identification and USB cable)');
  
  yFreeAPI();
end.
