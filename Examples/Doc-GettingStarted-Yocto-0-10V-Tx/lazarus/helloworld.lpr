{*********************************************************************
 *
 *  $Id: helloworld.lpr 47192 2021-11-08 18:02:19Z seb $
 *
 *  An example that show how to use a  Yocto-0-10V-Tx
 *
 *  You can find more information on our web site:
 *   Yocto-0-10V-Tx documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-0-10v-tx/doc.html
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
  yocto_voltageoutput;

procedure usage();
  var
    execname:string;
  begin
    execname := ExtractFileName(paramstr(0));
    WriteLn('Usage:');
    WriteLn(execname + '<serial_number> <voltage>');
    WriteLn(execname + '<logical_name>  <voltage>');
    WriteLn(execname + 'any  <voltage>    (use any discovered device)');
    WriteLn('    <voltage>: floating point number between 0.0 and 10.000');
    WriteLn('Example:');
    WriteLn(execname + ' any 7.5');
    sleep(3000);
    halt;
  end;

var
 errmsg,target,channel:string;
 voltage:double;
 vout:TYVoltageOutput;
 vout1,vout2:TYVoltageOutput;
 m : TYModule;

begin
  if (paramcount<2) then usage();

  target  := UpperCase(paramstr(1));
  voltage := StrToFloat(paramstr(2));

  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  if (target='ANY') then
    begin
      vout := yFirstVoltageOutput();
      if (vout =nil) then
       begin
         writeln('No module connected (check USB cable)');
         sleep(3000);
         halt;
       end;
      m :=  vout.get_module();
      target := m. get_serialNumber();
     end;

  Writeln('using ' + target);
  vout1 := yFindVoltageOutput(target + '.voltageOutput1');
  vout2 := yFindVoltageOutput(target + '.voltageOutput2');

  if (vout1.isOnline()) then
    begin
      // output 1 : immediate change
      vout1.set_currentVoltage(voltage);
      // output 2 : smooth change
      vout2.voltageMove(voltage,3000);
    end
  else writeln('Module not connected (check identification and USB cable)');
  yFreeAPI();

end.
