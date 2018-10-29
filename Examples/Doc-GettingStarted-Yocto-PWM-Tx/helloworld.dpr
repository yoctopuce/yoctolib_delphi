{*********************************************************************
 *
 *  $Id: helloworld.dpr 32621 2018-10-10 13:10:25Z seb $
 *
 *  An example that show how to use a  Yocto-PWM-Tx
 *
 *  You can find more information on our web site:
 *   Yocto-PWM-Tx documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-pwm-tx/doc.html
 *   Delphi API Reference:
 *      https://www.yoctopuce.com/EN/doc/reference/yoctolib-delphi-EN.html
 *
 *********************************************************************}

program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  yocto_api,
  yocto_pwmoutput;

procedure usage();
  var
    execname:string;
  begin
    execname := ExtractFileName(paramstr(0));
    WriteLn('Usage:');
    WriteLn(execname + '<serial_number>  <frequency> <dutyCycle>');
    WriteLn(execname + '<logical_name> <frequency> <dutyCycle>');
    WriteLn(execname + 'any  <frequency> <dutyCycle>   (use any discovered device)');
    WriteLn('    <frequency>: integer between 1Hz and 1000000Hz');
    WriteLn('    <dutyCycle>: floating point number between 0.0 and 100.0');
    WriteLn('Example:');
    WriteLn(execname + ' any 1000 22.5');
    sleep(2500);
    halt;
  end;

var
 errmsg,target,channel:string;
 frequency:integer;
 dutyCycle:double;
 pwmoutput:TYpwmoutput;
 pwmoutput1,pwmoutput2:TYpwmoutput;
 m : TYModule;

begin
  if (paramcount<3) then usage();

  target   :=  UpperCase(paramstr(1));
  frequency :=  StrToInt(paramstr(2));
  dutyCycle :=  StrToFloat(paramstr(3));

  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  if (target='ANY') then
    begin
      pwmoutput :=  YFirstPwmOutput();
      if (pwmoutput =nil) then
       begin
         writeln('No module connected (check USB cable)');
         halt;
       end;
      m :=  pwmoutput.get_module();
      target := m. get_serialNumber();
     end;

  Writeln('using ' + target);
  pwmoutput1 := YFindPwmOutput(target + '.pwmOutput1');
  pwmoutput2 := YFindPwmOutput(target + '.pwmOutput2');

  if (pwmoutput1.isOnline()) then
    begin
      // output 1 : immediate change
      pwmoutput1.set_frequency(frequency);
      pwmoutput1.set_enabled(Y_ENABLED_TRUE);
      pwmoutput1.set_dutyCycle(dutyCycle);
      // output 2 : smooth change
      pwmoutput2.set_frequency(frequency);
      pwmoutput2.set_enabled(Y_ENABLED_TRUE);
      pwmoutput2.dutyCycleMove(dutyCycle,3000);
    end
  else writeln('Module not connected (check identification and USB cable)');
  yFreeAPI();

end.
