{*********************************************************************
 *
 *  $Id: helloworld.dpr 70974 2025-12-22 15:06:26Z seb $
 *
 *  An example that shows how to use a  Yocto-Spectral
 *
 *  You can find more information on our web site:
 *   Yocto-Spectral documentation:
 *      https://www.yoctopuce.com/EN/products/yocto-spectral/doc.html
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
  yocto_colorsensor;

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
 colorSensor : TYColorSensor;

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
      colorSensor := YFirstColorSensor();
      if (colorSensor = nil) then
       begin
         writeln('No module connected (check USB cable)');
         sleep(3000);
         halt;
       end;
      m := colorSensor.get_module();
      target := m.get_serialNumber();
     end;

  Writeln(target);
  colorSensor := YFindColorSensor(target + '.colorSensor');

  if (colorSensor.isOnline()) then
    begin
        colorSensor.set_workingMode(Y_WORKINGMODE_AUTO);
        colorSensor.set_estimationModel(Y_ESTIMATIONMODEL_REFLECTION);
        while colorSensor.isOnline() do
        begin
          writeln('Near color :' + colorSensor.get_nearSimpleColor());
          writeln(Format('RGB HEX : #%x' ,[colorSensor.get_estimatedRGB()]));
          Writeln('------------------------------------');
          sleep(5000);
        end;

    end
  else writeln('Module not connected (check identification and USB cable)');
  
  yFreeAPI();
end.
