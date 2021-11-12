{*********************************************************************
 *
 *  $Id: helloworld.dpr 47192 2021-11-08 18:02:19Z seb $
 *
 *  An example that show how to use a  Yocto-Motor-DC
 *
 *  You can find more information on our web site:
 *   Yocto-Motor-DC documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-motor-dc/doc.html
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
  yocto_motor,yocto_current,yocto_voltage,yocto_temperature;

procedure usage();
  var
    execname:string;
  begin
    execname := ExtractFileName(paramstr(0));
    WriteLn('Usage:');
    WriteLn(execname + ' <serial_number> power ');
    WriteLn(execname + ' <logical_name> power');
    WriteLn(execname + ' any <channel> power');
    WriteLn('power is a integer between -100 and 100%');
    WriteLn('Example:');
    WriteLn(execname + ' any 75');
    sleep(3000);
    halt;
  end;

var
 errmsg,target:string;
 power:integer;
 motor:TYMotor;
 temperature:TYTemperature;
 current:TYCurrent;
 voltage:TYVoltage;

 m : TYModule;

begin
  if (paramcount<2) then usage();

  // parse command line
  target  :=  UpperCase(paramstr(1));
  power   :=  strtoint(paramstr(2));
  writeln(power);

  // Setup the API to use local USB devices
  if (YRegisterHub('usb', errmsg) <> YAPI_SUCCESS)  then
    begin
      writeln('RegisterHub error: ' + errmsg);
      halt;
    end;

  if (target='ANY') then
    begin
      // find the serial# of the first available motor
      motor :=  YFirstMotor();
      if (motor =nil) then
       begin
         writeln('No module connected (check USB cable)');
         sleep(3000);
         halt;
       end;
      // retreive the hosting device serial
      m :=  motor.get_module();
      target := m.get_serialNumber();
     end;

  Writeln('using ' + target);

  // retreive motor, current, voltage and temperature features from the device
  motor       := YFindMotor(target + '.motor');
  current     := YFindCurrent(target + '.current');
  temperature := YFindTemperature(target + '.temperature');
  voltage     := YFindVoltage(target + '.voltage');

  // lets start the motor
  if (motor.isOnline()) then
    begin
       // if motor is in error state, reset it.
       if ( motor.get_motorStatus>=Y_MOTORSTATUS_LOVOLT) then motor.resetStatus();
       motor.drivingForceMove(power,2000);  // ramp up to power in 2 seconds
       while motor.isOnline() do
        begin
          // display motor status
          Write('Status=',motor.get_advertisedValue(),'  ');
          Write('Voltage=',FloatToStrF(voltage.get_currentValue(),ffFixed,3,1),'V  ' );
          Write('Current=',FloatToStrF(current.get_currentValue()/1000,ffFixed,3,1),'A  ');
          Writeln('Temp=',FloatToStrF(temperature.get_currentValue(),ffFixed,3,1),'deg C');
          Ysleep(1000,errmsg); // wait for one second
        end;
  end else writeln('Module not connected (check identification and USB cable)');
  yFreeAPI();

end.
