program helloworld;
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
  yocto_api,
  yocto_pwminput;

  Procedure usage();
  begin
    writeln('demo <serial_number>');
    writeln('demo <logical_name>');
    writeln('demo any  ');
    sleep(2500);
    halt;
  end;

  Procedure die(msg:string);
   begin
     writeln(msg + '(check USB cable)');
     halt;
  end;

var

  errmsg : string;
  target : string;
  pwm    : TYPWMInput;
  pwm1   : TYPWMInput;
  pwm2   : TYPWMInput;
  m      : TyModule;


begin

  if (paramcount<1)  then usage();

  target:=paramstr(1);

  // Setup the API to use local USB devices
  If (yRegisterHub('usb', errmsg) <> YAPI_SUCCESS) Then
    begin
      WriteLn('RegisterHub error: ' + errmsg);
      halt;
    End;

  if (target='any') then
   begin
    // retreive any pwm input available
    pwm := yFirstPwmInput();
    If pwm=nil Then Die('No module connected');
   end
   else
   pwm:= yFindPwmInput(target + '.pwmInput1');

   m := nil;
   pwm1 := nil;
   pwm2 := nil;
   //  we need to retreive both DC and AC voltage from the device.
   If (pwm.isOnline()) Then
    begin
      m := pwm.get_module();
      pwm1 := yFindPwmInput(m.get_serialNumber() + '.pwmInput1');
      pwm2 := yFindPwmInput(m.get_serialNumber() + '.pwmInput2');
    end else Die('Module not connected');

   // let's poll
  while (m.isOnline()) do
     begin
      If Not(m.isOnline()) Then Die('Module not connected');
      Writeln('PWM1: ' + FloatToStr(pwm1.get_frequency())  + ' Hz '
                       + FloatToStr(pwm1.get_dutyCycle())  + ' % '
                       + IntToStr(pwm1.get_pulseCounter()) + ' pulse edges ');
      Writeln('PWM2: ' + FloatToStr(pwm2.get_frequency())  + ' Hz '
                       + FloatToStr(pwm2.get_dutyCycle())  + ' % '
                       + IntToStr(pwm2.get_pulseCounter()) + ' pulse edges ');
      Writeln('  (press Ctrl-C to exit)');
      ySleep(1000, errmsg);
   end;
   Die('Module not connected');
end.