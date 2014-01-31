program demo;
{$APPTYPE CONSOLE}
uses
  SysUtils,yocto_api,yocto_anButton;

  Procedure anButtonValueChangeCallBack(fct:TYAnButton; value:string);
   begin
    writeln(fct.get_hardwareId() + ' : ' + value + ' (new value)');
   end;

  Procedure sensorValueChangeCallBack(fct:TYSensor; value:string);
   begin
    writeln(fct.get_hardwareId() + ' : ' + value + ' ' + fct.get_unit() + ' (new value)');
   end;

  Procedure sensorTimedReportCallBack(fct:TYSensor; measure:TYMeasure);
   begin
    writeln(fct.get_hardwareId() + ' : ' + FloatToStr(measure.get_averageValue()) + ' ' + fct.get_unit() + ' (timed report)');
   end;

  Procedure deviceArrival(m:TYModule);
   var
     serial     : string;
     hardwareId : string;
     fctcount,i : integer;
     anButton   : TYAnButton;
     sensor     : TYSensor;

   begin
     serial := m.get_serialNumber();
     writeln('Device arrival : ' + serial);

     // First solution: look for a specific type of function (eg. anButton)
     fctcount := m.functionCount();
     for i := 0 to fctcount-1 do
       begin
         hardwareId := serial + '.' + m.functionId(i);
         if (pos('.anButton',hardwareId) > 0) then
         begin
           writeln('- ' + hardwareId);
           anButton := yFindAnButton(hardwareId);
           anButton.registerValueCallback(anButtonValueChangeCallBack);
         end;
       end;

     // Alternate solution: register any kind of sensor on the device
     sensor := yFirstSensor();
     while sensor<>nil do
       begin
         if(sensor.get_module().get_serialNumber() = serial) then
         begin
           hardwareId := sensor.get_hardwareId();
           writeln('- ' + hardwareId);
           sensor.registerValueCallback(sensorValueChangeCallBack);
           sensor.registerTimedReportCallback(sensorTimedReportCallBack);
         end;
         sensor := sensor.nextSensor();
       end
   end;

  Procedure deviceRemoval( m: TYmodule);
   begin
      Writeln('Device removal : ' + m.get_serialNumber());
   end;

var
   errmsg:string;

begin
  if (yRegisterHub('usb',  errmsg) <> YAPI_SUCCESS) then
   begin
     WriteLn('RegisterHub error : ' + errmsg);
     halt;
   end;

  yRegisterDeviceArrivalCallback(deviceArrival);
  yRegisterDeviceRemovalCallback(deviceRemoval);

  WriteLn('Hit Ctrl-C to Stop ');

  while(true) do
   begin
      yUpdateDeviceList( errmsg); // traps plug/unplug events
      ySleep(500, errmsg);   // traps others events
   end;

end.