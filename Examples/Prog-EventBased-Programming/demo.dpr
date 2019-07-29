program demo;
{$APPTYPE CONSOLE}
uses
  SysUtils,yocto_api,yocto_anButton;

  Procedure anButtonValueChangeCallBack(fct:TYAnButton; value:string);
   begin
    writeln(fct.get_hardwareId() + ' : ' + value + ' (new value)');
   end;

  Procedure sensorValueChangeCallBack(fct:TYSensor; value:string);
   var
    uni        : String;
   begin
    uni :=  String(fct.get_userData());
    writeln(fct.get_hardwareId() + ' : ' + value + ' ' + uni + ' (new value)');
   end;

  Procedure sensorTimedReportCallBack(fct:TYSensor; measure:TYMeasure);
   var
    uni        : String;
   begin
    uni :=  String(fct.get_userData());
    writeln(fct.get_hardwareId() + ' : ' + FloatToStr(measure.get_averageValue()) + ' ' + uni + ' (timed report)');
   end;

  Procedure configChangeCallBack(m:TYModule);
   begin
    writeln(m.get_serialNumber() + ' : config changed');
   end;

  Procedure beaconCallback(m:TYModule; beacon:Integer);
   begin
    writeln(m.get_serialNumber() + ' : beacon changed to '+ IntToStr(beacon));
   end;


  Procedure deviceArrival(m:TYModule);
   var
     serial     : string;
     hardwareId : string;
     fctcount,i : integer;
     anButton   : TYAnButton;
     sensor     : TYSensor;
     uni        : string;
   begin
     serial := m.get_serialNumber();
     writeln('Device arrival : ' + serial);
     m.registerConfigChangeCallback(@configChangeCallback);
     m.registerBeaconCallback(@beaconCallback);

     // First solution: look for a specific type of function (eg. anButton)
     fctcount := m.functionCount();
     for i := 0 to fctcount-1 do
       begin
         hardwareId := serial + '.' + m.functionId(i);
         if (pos('.anButton',hardwareId) > 0) then
         begin
           writeln('- ' + hardwareId);
           anButton := yFindAnButton(hardwareId);
           anButton.registerValueCallback(@anButtonValueChangeCallBack);
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
           uni := sensor.get_unit();
           sensor.set_userData(TObject(uni));
           sensor.registerValueCallback(@sensorValueChangeCallBack);
           sensor.registerTimedReportCallback(@sensorTimedReportCallBack);
         end;
         sensor := sensor.nextSensor();
       end
   end;

  Procedure deviceRemoval( m: TYmodule);
   begin
      Writeln('Device removal : ' + m.get_serialNumber());
   end;

  Procedure logfun(line: string);
   begin
      Write('LOG : ' + line);
   end;

var
   errmsg:string;

begin
  yRegisterLogFunction(@logfun);

  if (yRegisterHub('usb',  errmsg) <> YAPI_SUCCESS) then
   begin
     WriteLn('RegisterHub error : ' + errmsg);
     halt;
   end;
  yRegisterDeviceArrivalCallback(@deviceArrival);
  yRegisterDeviceRemovalCallback(@deviceRemoval);

  WriteLn('Hit Ctrl-C to Stop ');

  while(true) do
   begin
      yUpdateDeviceList( errmsg); // traps plug/unplug events
      ySleep(500, errmsg);   // traps others events
   end;

end.